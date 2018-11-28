with SXML.Parser;
with SXML.Serialize;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Directories;
with Ada.Exceptions;
with GNAT.Exception_Actions;

procedure Fuzzdriver
is
   function Read_File (Name : String) return access String
   is
      Block_Size : constant := 1024;
      use Ada.Text_IO.Text_Streams;
      File_Size : Natural := Natural (Ada.Directories.Size (Name));
      Result : access String := new String (1 .. File_Size);
      File : File_Type;
      Len  : Natural;
      Off  : Natural := 1;
   begin
      Open (File, In_File, Name);
      loop
         Len := (if File_Size - Off > Block_Size then Block_Size else File_Size - Off + 1);
         String'Read (Stream (File), Result.all (Off .. Off + Len - 1));
         Off := Off + Len;
         exit when Off >= File_Size;
      end loop;
      Close (File);
      return Result;
   end Read_File;

   procedure Parse_Document (File : String)
   is
      Input   : access String := Read_File (File);
      Output  : access String := new String (1 .. 3 * Input'Length);
      Document : access SXML.Document_Type := new SXML.Document_Type (1 .. Input'Length);
      use SXML;
      use SXML.Parser;
      Match    : Match_Type;
      Result   : Result_Type;
      Position : Natural;
   begin
      Document.all := (others => SXML.Null_Node);
      Parse (Data         => Input.all,
             Document     => Document.all,
             Parse_Result => Match,
             Position     => Position);
      if Match /= Match_OK
      then
         Ada.Text_IO.Put_Line (File & ":" & Position'Img(2..Position'Img'Last) & ": Invalid result: " & Match'Img);
      else
         SXML.Serialize.To_String (Document.all, Output.all, Position, Result);
         if Result /= Result_OK
         then
            Ada.Text_IO.Put_Line (File & ": Serialization error: " & Result'Img & " at " & Position'Img);
         end if;
         Ada.Text_IO.Put_Line (File & ": OK");
      end if;
   end Parse_Document;
begin
   Parse_Document (Argument (1));
exception
   when Occurence : others =>
      Put_Line ("exception occured [" & Ada.Exceptions.Exception_Name (Occurence)
                & "] [" & Ada.Exceptions.Exception_Message (Occurence)
                & "] [" & Ada.Exceptions.Exception_Information (Occurence) & "]");
      GNAT.Exception_Actions.Core_Dump (Occurence);
end Fuzzdriver;
