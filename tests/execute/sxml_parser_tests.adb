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
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body SXML_Parser_Tests is

   procedure Single_Node (T : in out Test_Cases.Test_Case'Class)
   is
      Data : String := "<valid></valid>";
   begin
      Check_Document (Data, "<valid/>");
   end Single_Node;

   -----------------------------------------------------------------------------

   procedure Long_Node (T : in out Test_Cases.Test_Case'Class)
   is
      Tag : constant String := "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUWXYZ0123456789";
      Data : String := "<" & Tag & "></" & Tag & ">";
   begin
      Check_Document (Data, "<" & Tag & "/>");
   end Long_Node;

   -----------------------------------------------------------------------------

   procedure Multiple_Of_Chunk_Tag (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      for I in 1 .. 100
      loop
         declare
            Tag : constant String (1 .. I) := (others => 'Y');
            Data : String := "<" & Tag & "></" & Tag & ">";
         begin
            Check_Document (Data, "<" & Tag & "/>");
         end;
      end loop;
   end Multiple_Of_Chunk_Tag;

   ---------------------------------------------------------------------------

   procedure Single_Short_Node (T : in out Test_Cases.Test_Case'Class)
   is
      Data : String := "<valid/>";
   begin
      Check_Document (Data);
   end Single_Short_Node;

   ---------------------------------------------------------------------------

   procedure Multiple_Children (T : in out Test_Cases.Test_Case'Class)
   is
      Data : String := "<parent><child1/><child2/></parent>";
   begin
      Check_Document (Data);
   end Multiple_Children;

   -----------------------------------------------------------------------------

   procedure Invalid_Whitespace (T : in out Test_Cases.Test_Case'Class)
   is
      Data : String := "<   valid></valid>";
   begin
      Check_Invalid (Data);
   end Invalid_Whitespace;

   -----------------------------------------------------------------------------

   procedure Valid_Whitespace (T : in out Test_Cases.Test_Case'Class)
   is
      Data1 : String := "<valid     ></valid>";
      Data2 : String := "<valid" & Character'Val(16#D#) & "></valid>";
      Data3 : String := "<valid></valid  " & Character'Val(16#9#) & ">";
      Data4 : String := "<valid></valid  " & Character'Val(16#A#) & ">";
   begin
      Check_Document (Data1, "<valid/>");
      Check_Document (Data2, "<valid/>");
      Check_Document (Data3, "<valid/>");
      Check_Document (Data4, "<valid/>");
   end Valid_Whitespace;

   ---------------------------------------------------------------------------

   procedure Simple_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
      Data : String := "<parent attr=""test""></parent>";
   begin
      Check_Document (Data, "<parent attr=""test""/>");
   end Simple_Attribute;

   ---------------------------------------------------------------------------

   procedure Empty_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
      Data : String := "<parent attr=""""></parent>";
   begin
      Check_Document (Data, "<parent attr=""""/>");
   end Empty_Attribute;

   ---------------------------------------------------------------------------

   procedure Single_Quote_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
      Data : String := "<parent attr='test'></parent>";
   begin
      Check_Document (Data, "<parent attr=""test""/>");
   end Single_Quote_Attribute;

   ---------------------------------------------------------------------------

   procedure Invalid_Mixed_Quote_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
      Data1 : String := "<parent attr='test""></parent>";
      Data2 : String := "<parent attr=""test'></parent>";
   begin
      Check_Invalid (Data1);
      Check_Invalid (Data2);
   end Invalid_Mixed_Quote_Attribute;

   ---------------------------------------------------------------------------

   procedure Long_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
      Attr : constant String := "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUWXYZ0123456789";
      Data : String := "<parent attr=""" & Attr & """></parent>";
   begin
      Check_Document (Data, "<parent attr=""" & Attr & """/>");
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
            Data : String := "<parent " & Name & "=""value""></parent>";
         begin
            Check_Document (Data, "<parent " & Name & "=""value""/>");
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
            Data : String := "<parent attr=""" & Attr & """></parent>";
         begin
            Check_Document (Data, "<parent attr=""" & Attr & """/>");
         end;
      end loop;
   end Multiple_Of_Chunk_Attribute_Value;

   ---------------------------------------------------------------------------

   procedure Slash_In_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
      Data : String := "<parent attr=""/foo/bar/baz""></parent>";
   begin
      Check_Document (Data, "<parent attr=""/foo/bar/baz""/>");
   end Slash_In_Attribute;

   ---------------------------------------------------------------------------

   procedure Whitespace_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
      Data : String := "<parent attr = ""test""   ></parent>";
   begin
      Check_Document (Data, "<parent attr=""test""/>");
   end Whitespace_Attribute;

   ---------------------------------------------------------------------------

   procedure Multiple_Attributes (T : in out Test_Cases.Test_Case'Class)
   is
      Data : String := "<parent attr1=""data1"" attr2=""data2"" attr3=""data3""></parent>";
   begin
      Check_Document (Data, "<parent attr1=""data1"" attr2=""data2"" attr3=""data3""/>");
   end Multiple_Attributes;

   ---------------------------------------------------------------------------

   procedure Whitespace_Between_Nodes (T : in out Test_Cases.Test_Case'Class)
   is
      Data : String := "<parent attr=""test"">     </parent>";
   begin
      Check_Document (Data);
   end Whitespace_Between_Nodes;

   ---------------------------------------------------------------------------

   procedure Complex_File_Without_Attributes (T : in out Test_Cases.Test_Case'Class)
   is
      Data     : access String := Read_File ("tests/data/complex1.xml");
   begin
      Check_Document (Data.all);
   end Complex_File_Without_Attributes;

   ---------------------------------------------------------------------------

   procedure Complex_File (T : in out Test_Cases.Test_Case'Class)
   is
      Data     : access String := Read_File ("tests/data/complex2.xml");
   begin
      Check_Document (Data.all);
   end Complex_File;

   ---------------------------------------------------------------------------

   procedure Ignore_Node_Content (T : in out Test_Cases.Test_Case'Class)
   is
      Data : String := "<parent>Node content to be ignored</parent>";
   begin
      Check_Document (Data);
   end Ignore_Node_Content;

   ---------------------------------------------------------------------------

   procedure Ignore_Comments (T : in out Test_Cases.Test_Case'Class)
   is
      Data : access String := Read_File ("tests/data/comments.xml");
   begin
      Check_Document (Data.all, "<parent>" & ASCII.LF & "   <child/>" & ASCII.LF & "</parent>");
   end Ignore_Comments;

   ---------------------------------------------------------------------------

   procedure PI_And_Comment (T : in out Test_Cases.Test_Case'Class)
   is
      Data : access String := Read_File ("tests/data/pi_and_comment.xml");
   begin
      Check_Document (Data.all, "<bar/>");
   end PI_And_Comment;

   ---------------------------------------------------------------------------

   procedure Ignore_Processing_Information (T : in out Test_Cases.Test_Case'Class)
   is
      Data : access String := Read_File ("tests/data/pi.xml");
   begin
      Check_Document (Data.all, "<parent>" & ASCII.LF & "   <child/>" & ASCII.LF & "</parent>");
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

   procedure Handle_CDATA (T : in out Test_Cases.Test_Case'Class)
   is
      Data : String := "<parent>Content <![CDATA[ The parser shouln'd break if x < 1 & y > 42! ;</ ]]> More content </parent>";
   begin
      Check_Document (Data, "<parent>Content  The parser shouln&apos;d break if x &lt; 1 &amp; y &gt; 42! ;&lt;/  More content </parent>");
   end Handle_CDATA;

   ---------------------------------------------------------------------------

   procedure Handle_Multiple_CDATA (T : in out Test_Cases.Test_Case'Class)
   is
      Data : String :=
         "<parent>Content <![CDATA[ The parser shouln'd break if x < 1 & y > 42! ;</ ]]> " &
         "More content <![CDATA[ Yet another <CDATA>! ]]> even more <![CDATA[content]]> !!! </parent>";
   begin
      Check_Document (Data,"<parent>Content  The parser shouln&apos;d break if x &lt; 1 &amp; y &gt; 42! ;&lt;/  " &
                           "More content  Yet another &lt;CDATA&gt;!  even more content !!! </parent>");
   end Handle_Multiple_CDATA;

   ---------------------------------------------------------------------------

   procedure Single_Quote_Content (T : in out Test_Cases.Test_Case'Class)
   is
      Data : String := "<test><test1>='</test1><test2>'</test2></test>";
   begin
      Check_Document
         (Data, "<test><test1>=&apos;</test1><test2>&apos;</test2></test>");
   end Single_Quote_Content;

   ---------------------------------------------------------------------------

   procedure Attribute_Value_With_Gt (T : in out Test_Cases.Test_Case'Class)
   is
      Data : String := "<test attr="">""></test>";
   begin
      Check_Document (Data, "<test attr="">""/>");
   end Attribute_Value_With_Gt;

   ---------------------------------------------------------------------------

   procedure File_With_BOM (T : in out Test_Cases.Test_Case'Class)
   is
      Data : access String := Read_File ("tests/data/bom.xml");
   begin
      Check_Document (Data.all, "<foo/>");
   end File_With_BOM;

   ---------------------------------------------------------------------------

   procedure Gone321 (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Parse_Document ("tests/data/321gone.xml");
   end Gone321;

   ---------------------------------------------------------------------------

   procedure Ebay (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Parse_Document ("tests/data/ebay.xml");
   end Ebay;

   ---------------------------------------------------------------------------

   procedure Reed (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Parse_Document ("tests/data/reed.xml");
   end Reed;

   ---------------------------------------------------------------------------

   procedure SigmodRecord (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Parse_Document ("tests/data/SigmodRecord.xml");
   end SigmodRecord;

   ---------------------------------------------------------------------------

   procedure Ubid (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Parse_Document ("tests/data/ubid.xml");
   end Ubid;

   ---------------------------------------------------------------------------

   procedure Yahoo (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Parse_Document ("tests/data/yahoo.xml");
   end Yahoo;

   ---------------------------------------------------------------------------

   procedure Comment_Single (T : in out Test_Cases.Test_Case'Class)
   is
      Data : String := "<elem><!--Comment--></elem>";
   begin
      Check_Document (Data, "<elem/>");
   end Comment_Single;

   ---------------------------------------------------------------------------

   procedure Doctype_With_Elements (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Parse_Document ("tests/data/doctype.xml");
   end Doctype_With_Elements;

   ---------------------------------------------------------------------------

   procedure Comments_Inside_Content (T : in out Test_Cases.Test_Case'Class)
   is
      Data : String := "<elem> Here is <!--Comment--> some content </elem>";
   begin
      Check_Document (Data, "<elem> Here is  some content </elem>");
   end Comments_Inside_Content;

   ---------------------------------------------------------------------------

   procedure Processing_Info_Inside_Content (T : in out Test_Cases.Test_Case'Class)
   is
      Data : String := "<elem> Here is <?some processing info?> some content </elem>";
   begin
      Check_Document (Data, "<elem> Here is  some content </elem>");
   end Processing_Info_Inside_Content;

   ---------------------------------------------------------------------------

   procedure Large_File (T : in out Test_Cases.Test_Case'Class)
   is
   begin
      Parse_Document ("tests/data/orders.xml");
   end Large_File;

   ---------------------------------------------------------------------------

   procedure Deep_File (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      type Subtree_Access is access Subtree_Type;
      procedure Free_Subtree is new Ada.Unchecked_Deallocation
         (Object => Subtree_Type, Name => Subtree_Access);
      File_Name : constant String := "obj/generated.xml";
      Context   : Subtree_Access := new Subtree_Type (1 .. 1000000);
   begin
      Generate_Deep_Nodes (File_Name, 1000000);
      declare
         Input : access String := Read_File (File_Name);
         package P is new Parser (Input.all, Context.all);
         use P;
         Result   : Match_Type;
         Position : Natural;
      begin
         P.Parse (Match    => Result,
                  Position => Position);
         Free_Subtree (Context);
         Assert (Result /= Match_OK,
                 File_Name & ":" & Position'Img(2..Position'Img'Last) & ": Expected error");
      end;
   end Deep_File;

   ---------------------------------------------------------------------------

   procedure Many_Nodes (T : in out Test_Cases.Test_Case'Class)
   is
      File_Name : constant String := "obj/many_nodes.xml";
   begin
      Generate_Many_Nodes (File_Name, 1000000);
      Parse_Document (File_Name);
   end Many_Nodes;

   ---------------------------------------------------------------------------

   procedure Large_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
      File_Name : constant String := "obj/large_attribute.xml";
   begin
      Generate_Large_Attribute (File_Name, 1000000);
      Parse_Document (File_Name);
   end Large_Attribute;

   ---------------------------------------------------------------------------

   procedure Many_Attributes (T : in out Test_Cases.Test_Case'Class)
   is
      File_Name : constant String := "obj/many_attributes.xml";
   begin
      Generate_Many_Attributes (File_Name, 1000000);
      Parse_Document (File_Name);
   end Many_Attributes;

   ---------------------------------------------------------------------------

   procedure Simple_Content (T : in out Test_Cases.Test_Case'Class)
   is
      Data : String := "<valid>Some content</valid>";
   begin
      Check_Document (Data);
   end Simple_Content;

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
      Register_Routine (T, Empty_Attribute'Access, "Empty attribute");
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
      Register_Routine (T, PI_And_Comment'Access, "PI and comment");
      Register_Routine (T, Ignore_Processing_Information'Access, "Ignore PI");
      Register_Routine (T, Complex_File_Without_Attributes'Access, "Complex file without attributes");
      Register_Routine (T, Complex_File'Access, "Complex file");
      Register_Routine (T, CCDA_1'Access, "CCDA sample file");
      Register_Routine (T, MXML_1'Access, "MusicXML sample file #1");
      Register_Routine (T, MXML_2'Access, "MusicXML sample file #2");
      Register_Routine (T, Handle_CDATA'Access, "Handle CDATA");
      Register_Routine (T, Handle_Multiple_CDATA'Access, "Handle multiple CDATA");
      Register_Routine (T, Single_Quote_Content'Access, "Single quote content");
      Register_Routine (T, Attribute_Value_With_Gt'Access, "Attribute value with > sign");
      Register_Routine (T, File_With_BOM'Access, "File with Unicode byteorder mark");
      Register_Routine (T, Yahoo'Access, "Yahoo test set");
      Register_Routine (T, Ubid'Access, "Ubid");
      Register_Routine (T, SigmodRecord'Access, "SigmodRecord");
      Register_Routine (T, Reed'Access, "Reed");
      Register_Routine (T, Ebay'Access, "Ebay");
      Register_Routine (T, Gone321'Access, "321 gone");
      Register_Routine (T, Comment_Single'Access, "Only comment between tags");
      Register_Routine (T, Doctype_With_Elements'Access, "Doctype with elements");
      Register_Routine (T, Comments_Inside_Content'Access, "Comments inside content");
      Register_Routine (T, Processing_Info_Inside_Content'Access, "Processing info inside content");
      Register_Routine (T, Large_File'Access, "Large file");
      Register_Routine (T, Deep_File'Access, "Deep file");
      Register_Routine (T, Many_Nodes'Access, "Many nodes");
      Register_Routine (T, Large_Attribute'Access, "Large attribute");
      Register_Routine (T, Many_Attributes'Access, "Many attributes");
      Register_Routine (T, Simple_Content'Access, "Simple content");
   end Register_Tests;

   ---------------------------------------------------------------------------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("SXML Parser Tests");
   end Name;

end SXML_Parser_Tests;
