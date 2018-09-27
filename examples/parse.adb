with SXML.Parser;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Directories;

procedure Parse
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
      Context : access SXML.Subtree_Type := new SXML.Subtree_Type (1 .. Input'Length);
      use SXML.Parser;
      Result   : Match_Type;
      Position : Natural;
   begin
      Parse (Data         => Input.all,
             Context      => Context.all,
             Parse_Result => Result,
             Position     => Position);
      if Result /= Match_OK
      then
         Ada.Text_IO.Put_Line (File & ":" & Position'Img(2..Position'Img'Last) & ": Invalid result: " & Result'Img);
      else
         Ada.Text_IO.Put_Line (File & ": OK");
      end if;
   end Parse_Document;
begin
   if Argument_Count < 1 then
      Put_Line ("Invalid arguments");
   else
      Parse_Document (Argument (1));
   end if;
end Parse;

