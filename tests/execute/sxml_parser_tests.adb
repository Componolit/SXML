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
with SXML_Utils; use SXML_Utils;

package body SXML_Parser_Tests is

   procedure Single_Node (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Check_Document ("<valid></valid>");
   end Single_Node;

   -----------------------------------------------------------------------------

   procedure Long_Node (T : in out Test_Cases.Test_Case'Class)
   is
      Tag : constant String := "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUWXYZ0123456789";
   begin
      Check_Document ("<" & Tag & "></" & Tag & ">");
   end Long_Node;

   -----------------------------------------------------------------------------

   procedure Multiple_Of_Chunk_Tag (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      for I in 1 .. 100
      loop
         declare
            Tag : constant String (1 .. I) := (others => 'Y');
         begin
            Check_Document ("<" & Tag & "></" & Tag & ">");
         end;
      end loop;
   end Multiple_Of_Chunk_Tag;

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

   procedure Single_Quote_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Check_Document ("<parent attr='test'></parent>", "<parent attr=""test""></parent>");
   end Single_Quote_Attribute;

   ---------------------------------------------------------------------------

   procedure Invalid_Mixed_Quote_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Check_Invalid ("<parent attr='test""></parent>");
      Check_Invalid ("<parent attr=""test'></parent>");
   end Invalid_Mixed_Quote_Attribute;

   ---------------------------------------------------------------------------

   procedure Long_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
      Attr : constant String := "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUWXYZ0123456789";
   begin
      Check_Document ("<parent attr=""" & Attr & """></parent>");
   end Long_Attribute;

   ---------------------------------------------------------------------------

   procedure Multiple_Of_Chunk_Attribute_Name (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      --  We cannot determine data length programatically, as Data_Type is
      --  private in SXML.
      for I in 1 .. 100
      loop
         declare
            Name : constant String (1 .. I) := (others => 'X');
         begin
            Check_Document ("<parent " & Name & "=""value""></parent>");
         end;
      end loop;
   end Multiple_Of_Chunk_Attribute_Name;

   ---------------------------------------------------------------------------

   procedure Multiple_Of_Chunk_Attribute_Value (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      --  We cannot determine data length programatically, as Data_Type is
      --  private in SXML.
      for I in 1 .. 100
      loop
         declare
            Attr : constant String (1 .. I) := (others => 'X');
         begin
            Check_Document ("<parent attr=""" & Attr & """></parent>");
         end;
      end loop;
   end Multiple_Of_Chunk_Attribute_Value;

   ---------------------------------------------------------------------------

   procedure Slash_In_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Check_Document ("<parent attr=""/foo/bar/baz""></parent>");
   end Slash_In_Attribute;

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
      Register_Routine (T, Long_Node'Access, "Node with long tag name");
      Register_Routine (T, Multiple_Of_Chunk_Tag'Access, "Tag that is multiple of chunk length");
      Register_Routine (T, Single_Short_Node'Access, "Parse single short node");
      Register_Routine (T, Multiple_Children'Access, "Parse multiple children");
      Register_Routine (T, Invalid_Whitespace'Access, "Invalid whitespace");
      Register_Routine (T, Valid_Whitespace'Access, "Valid whitespace");
      Register_Routine (T, Simple_Attribute'Access, "Simple attribute");
      Register_Routine (T, Single_Quote_Attribute'Access, "Single quote attribute");
      Register_Routine (T, Invalid_Mixed_Quote_Attribute'Access, "Invalid mixed quote attribute");
      Register_Routine (T, Long_Attribute'Access, "Long attribute");
      Register_Routine (T, Multiple_Of_Chunk_Attribute_Name'Access, "Attribute name that is multple of chunk length");
      Register_Routine (T, Multiple_Of_Chunk_Attribute_Value'Access, "Attribute value that is multple of chunk length");
      Register_Routine (T, Slash_In_Attribute'Access, "Slash character in attribute");
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
