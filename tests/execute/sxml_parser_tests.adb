--
-- @summary Tests for XML generator
-- @author  Alexander Senier
-- @date    2018-08-12
--
-- Copyright (C) 2018 Componolit GmbH
--
-- This file is part of SXML, which is distributed under the terms of the
-- GNU Affero General Public License version 3.
--

with AUnit.Assertions; use AUnit.Assertions;
with SXML;
with SXML.Parser;
with Ada.Direct_IO;
with Ada.Directories;

package body SXML_Parser_Tests is

   Context : SXML.Subtree_Type (1 .. 1000000);

   ---------------
   -- Read_File --
   ---------------

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
      Input : constant String := Read_File (File);
      package Parser is new SXML.Parser (Input, Context);
      use SXML;
      use Parser;
      Result   : Match_Type;
      Position : Natural;
   begin
      Parser.Parse (Match    => Result,
                    Position => Position);
      Assert (Result = Match_OK,
              File & ":" & Position'Img(2..Position'Img'Last) & ": Invalid result");
   end Parse_Document;

   procedure Check_Document (Input    : String;
                             Output   : String := "INPUT")
   is
      package Parser is new SXML.Parser (Input, Context);
      use SXML;
      use Parser;
      Result   : Match_Type;
      Position : Natural;
   begin
      Context := (others => Null_Node);
      Parser.Parse (Match    => Result,
                    Position => Position);
      Assert (Result = Match_OK, "Invalid result: " & Result'Img & " (Pos:" & Position'Img  & ")");
      declare
         Doc : constant String := To_String (Parser.Document);
      begin
         --  FIXME: Remove whitespace
         Assert (Doc = (if Output = "INPUT" then Input else Output),
            "Invalid result at" & Position'Img &
            ": (" & Doc & "), expected: (" & (if Output = "INPUT" then Input else Output) & ")");
      end;
   end Check_Document;

   -----------------------------------------------------------------------------

   procedure Check_Invalid (Input : String)
   is
      package Parser is new SXML.Parser (Input, Context);
      use SXML;
      use Parser;
      Result   : Match_Type;
      Position : Natural;
   begin
      Parser.Parse (Match    => Result,
                    Position => Position);
      pragma Unreferenced (Position);
      Assert (Result /= Match_OK, "Error expected");
   end Check_Invalid;

   -----------------------------------------------------------------------------

   procedure Single_Node (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Check_Document ("<valid></valid>");
   end Single_Node;

   ---------------------------------------------------------------------------

   procedure Single_Short_Node (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Check_Document ("<valid/>", "<valid></valid>");
   end Single_Short_Node;

   ---------------------------------------------------------------------------

   procedure Multiple_Children (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Check_Document ("<parent><child1/><child2/></parent>",
                      "<parent><child1></child1><child2></child2></parent>");
   end Multiple_Children;

   -----------------------------------------------------------------------------

   procedure Invalid_Whitespace (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Check_Invalid ("<   valid></valid>");
   end Invalid_Whitespace;

   -----------------------------------------------------------------------------

   procedure Valid_Whitespace (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Check_Document ("<valid     ></valid>", "<valid></valid>");
      Check_Document ("<valid" & Character'Val(16#D#) & "></valid>", "<valid></valid>");
      Check_Document ("<valid></valid  " & Character'Val(16#9#) & ">", "<valid></valid>");
      Check_Document ("<valid></valid  " & Character'Val(16#A#) & ">", "<valid></valid>");
   end Valid_Whitespace;

   ---------------------------------------------------------------------------

   procedure Simple_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Check_Document ("<parent attr=""test""></parent>");
   end Simple_Attribute;

   ---------------------------------------------------------------------------

   procedure Whitespace_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Check_Document ("<parent attr = ""test""   ></parent>", "<parent attr=""test""></parent>");
   end Whitespace_Attribute;

   ---------------------------------------------------------------------------

   procedure Multiple_Attributes (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Check_Document ("<parent attr1=""data1"" attr2=""data2"" attr3=""data3""></parent>");
   end Multiple_Attributes;

   ---------------------------------------------------------------------------

   procedure Whitespace_Between_Nodes (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Check_Document ("<parent attr=""test"">     </parent>", "<parent attr=""test""></parent>");
   end Whitespace_Between_Nodes;

   ---------------------------------------------------------------------------

   procedure Complex_File_Without_Attributes (T : in out Test_Cases.Test_Case'Class)
   is
      Data     : constant String := Read_File ("tests/data/complex1.xml");
      Expected : constant String := Read_File ("tests/data/complex1_expect.xml");
   begin
      Check_Document (Data, Expected (Expected'First .. Expected'Last - 1));
   end Complex_File_Without_Attributes;

   ---------------------------------------------------------------------------

   procedure Complex_File (T : in out Test_Cases.Test_Case'Class)
   is
      Data     : constant String := Read_File ("tests/data/complex2.xml");
      Expected : constant String := Read_File ("tests/data/complex2_expect.xml");
   begin
      Check_Document (Data, Expected (Expected'First .. Expected'Last - 1));
   end Complex_File;

   ---------------------------------------------------------------------------

   procedure Ignore_Node_Content (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Check_Document ("<parent>Node content to be ignored</parent>",
                      "<parent></parent>");
   end Ignore_Node_Content;

   ---------------------------------------------------------------------------

   procedure Ignore_Comments (T : in out Test_Cases.Test_Case'Class)
   is
      Data : constant String := Read_File ("tests/data/comments.xml");
   begin
      Check_Document (Data, "<parent><child></child></parent>");
   end Ignore_Comments;

   ---------------------------------------------------------------------------

   procedure Ignore_Processing_Information (T : in out Test_Cases.Test_Case'Class)
   is
      Data : constant String := Read_File ("tests/data/pi.xml");
   begin
      Check_Document (Data, "<parent><child></child></parent>");
   end Ignore_Processing_Information;

   ---------------------------------------------------------------------------

   procedure CCDA_1 (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Parse_Document ("tests/data/Vitera_CCDA_SMART_Sample.xml");
   end CCDA_1;

   ---------------------------------------------------------------------------

   procedure MXML_1 (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Parse_Document ("tests/data/Ws-bach243b.xml");
   end MXML_1;

   ---------------------------------------------------------------------------

   procedure MXML_2 (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Parse_Document ("tests/data/Bach-himmel.xml");
   end MXML_2;

   ---------------------------------------------------------------------------

   procedure Register_Tests (T: in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Single_Node'Access, "Parse single node");
      Register_Routine (T, Single_Short_Node'Access, "Parse single short node");
      Register_Routine (T, Multiple_Children'Access, "Parse multiple children");
      Register_Routine (T, Invalid_Whitespace'Access, "Invalid whitespace");
      Register_Routine (T, Valid_Whitespace'Access, "Valid whitespace");
      Register_Routine (T, Simple_Attribute'Access, "Simple attribute");
      Register_Routine (T, Whitespace_Attribute'Access, "Whitespace attribute");
      Register_Routine (T, Multiple_Attributes'Access, "Multiple attributes");
      Register_Routine (T, Whitespace_Between_Nodes'Access, "Whitespace between nodes");
      Register_Routine (T, Ignore_Node_Content'Access, "Ignore node content");
      Register_Routine (T, Ignore_Comments'Access, "Ignore comments");
      Register_Routine (T, Ignore_Processing_Information'Access, "Ignore PI");
      Register_Routine (T, Complex_File_Without_Attributes'Access, "Complex file without attributes");
      Register_Routine (T, Complex_File'Access, "Complex file");
      Register_Routine (T, CCDA_1'Access, "CCDA sample file");
      Register_Routine (T, MXML_1'Access, "MusicXML sample file #1");
      Register_Routine (T, MXML_2'Access, "MusicXML sample file #2");
   end Register_Tests;

   ---------------------------------------------------------------------------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("SXML Parser Tests");
   end Name;

end SXML_Parser_Tests;
