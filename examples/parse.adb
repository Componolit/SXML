with SXML.Parser;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Direct_IO;
with Ada.Directories;

procedure Parse
is
   function Read_File (File_Name : String) return String
   is
      File_Size : Natural := Natural (Ada.Directories.Size (File_Name));
      subtype File_String is String (1 .. File_Size);
      package File_String_IO is new Ada.Direct_IO (File_String);

      File     : File_String_IO.File_Type;
      Contents : File_String;
   begin
      File_String_IO.Open (File, File_String_IO.In_File, File_Name);
      File_String_IO.Read (File, Contents);
      File_String_IO.Close (File);
      return Contents;
   end Read_File;

   procedure Parse_Document (File : String)
   is
      Context : access SXML.Subtree_Type := new SXML.Subtree_Type (1 .. 1000000);
      Input : constant String := Read_File (File);
      package Parser is new SXML.Parser (Input, Context.all);
      use SXML;
      use Parser;
      Result   : Match_Type;
      Position : Natural;
   begin
      Parser.Parse (Match    => Result,
                    Position => Position);
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

