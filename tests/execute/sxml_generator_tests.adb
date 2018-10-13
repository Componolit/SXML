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
with SXML.Generator; use SXML.Generator;
with SXML.Serialize; use SXML.Serialize;
with SXML.Debug; use SXML.Debug;
with SXML.Generator.Debug; use SXML.Generator.Debug;

package body SXML_Generator_Tests is

   procedure Expect (Doc      : SXML.Subtree_Type;
                     Expected : String)
   is
      use SXML;
      XML    : access String := new String (1 .. 2 * Expected'Length);
      Offset : Natural := 0;
      Last   : Natural;
      Result : Result_Type;
   begin
      To_String (Doc, XML.all, Offset, Result);
      Last := XML.all'First + Offset - 1;
      Assert (Result = Result_OK, "Error serializing: " & Result'Img);
      Assert (XML.all (1 .. Last) = Expected,
         "Unexpected document: (" & XML.all (1 .. Last) & ") len:" & Last'Img &
         ", expected (" & Expected & ") len:" & Expected'Length'Img);
   end Expect;

   ---------------------------------------------------------------------------

   procedure Test_Generate_Single_Node (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Doc : Subtree_Type := E ("config");
   begin
      Expect (Doc, "<config/>");
	end Test_Generate_Single_Node;

   ---------------------------------------------------------------------------

   procedure Test_Generate_Single_Node_Attrib (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Doc : Subtree_Type := E ("config", A ("attrib", "Foo"));
   begin
      Expect (Doc, "<config attrib=""Foo""/>");
	end Test_Generate_Single_Node_Attrib;

   ---------------------------------------------------------------------------

   procedure Test_Generate_Nodes (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Doc : Subtree_Type := E ("config", E ("child"));
   begin
      Expect (Doc, "<config><child/></config>");
	end Test_Generate_Nodes;

   ---------------------------------------------------------------------------

   procedure Test_String_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Doc : Subtree_Type := E ("config", E ("child", A ("attr", "value")));
   begin
      Expect (Doc, "<config><child attr=""value""/></config>");
	end Test_String_Attribute;

   ---------------------------------------------------------------------------

   procedure Test_Integer_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Doc : Subtree_Type := E ("config", E ("child", A ("attr", 42)));
   begin
      Expect (Doc, "<config><child attr=""42""/></config>");
	end Test_Integer_Attribute;

   ---------------------------------------------------------------------------

   procedure Test_Float_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Doc : Subtree_Type := E ("config", E ("child", A ("attr", 3.14)));
   begin
      Expect (Doc, "<config><child attr=""3.14000E+00""/></config>");
	end Test_Float_Attribute;

   ---------------------------------------------------------------------------

   procedure Long_Attribute_Value (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Val : String (1 .. 100) := (others => 'x');
      Doc : Subtree_Type := E ("config", A ("attr", Val));
   begin
      Expect (Doc, "<config attr=""" & Val & """/>");
	end Long_Attribute_Value;

   ---------------------------------------------------------------------------

   procedure Long_Attribute_Name (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Aname : String (1 .. 100) := (others => 'x');
      Doc : Subtree_Type := E ("config", A (Aname, "value"));
   begin
      Expect (Doc, "<config " & Aname & "=""value""/>");
	end Long_Attribute_Name;

   ---------------------------------------------------------------------------

   procedure Long_Tag_Name (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Name : String (1 .. 100) := (others => 'x');
      Doc  : Subtree_Type := E (Name, A ("attr", "value"));
   begin
      Expect (Doc, "<" & Name & " attr=""value""/>");
	end Long_Tag_Name;

   ---------------------------------------------------------------------------

   procedure Multiple_Attributes (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Long : String (1 .. 100) := (others => 'x');
      Doc  : Subtree_Type := E ("parent",
                                A ("attr1", "value") +
                                A ("attr2", Long) +
                                A ("attr3_" & Long, "value"));
   begin
      Expect (Doc, "<parent attr1=""value"" attr2=""" & Long & """ attr3_" & Long & "=""value""/>");
	end Multiple_Attributes;

   ---------------------------------------------------------------------------

   procedure Nested (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Doc  : Subtree_Type :=
          E ("parent", A ("attr1", "value1") +
                       A ("attr2", "value2") +
                       A ("attr3", "value3") +
                       A ("attr4", "value4"),
            E ("child1") +
            E ("child2",
               E ("subchild")));
   begin
      Expect (Doc, "<parent attr1=""value1"" attr2=""value2"" attr3=""value3"" attr4=""value4""><child1/><child2><subchild/></child2></parent>");
	end Nested;

   ---------------------------------------------------------------------------

   procedure Test_Generate_Simple_Content (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Doc : Subtree_Type := E ("config", A ("attrib", "Foo"), C ("Some content"));
   begin
      Expect (Doc, "<config attrib=""Foo"">Some content</config>");
	end Test_Generate_Simple_Content;

   ---------------------------------------------------------------------------

   procedure Test_Generate_Escape_Content (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Doc : Subtree_Type := E ("config", A ("attrib", "Foo"), C ("<XML>""Quoted"" & 'Quoted'<XML/>"));
   begin
      Expect (Doc, "<config attrib=""Foo"">&lt;XML&gt;&quot;Quoted&quot; &amp; &apos;Quoted&apos;&lt;XML/&gt;</config>");
	end Test_Generate_Escape_Content;

   ---------------------------------------------------------------------------

   procedure Test_Generate_Interleaved_Content (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Doc : Subtree_Type := E ("config", A ("attrib", "Foo"),
                               C ("Some") +
                               C (" content") +
                               E ("element2") +
                               C ("More content") +
                               E ("element3"));
   begin
      Expect (Doc, "<config attrib=""Foo"">Some content<element2/>More content<element3/></config>");
	end Test_Generate_Interleaved_Content;

   ---------------------------------------------------------------------------

   procedure Test_Context_Length (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;

      procedure Check_Length (N : String;
                              L : Offset_Type;
                              S : Subtree_Type)
      is
      begin
         if Num_Elements (S) /= L
         then
            Dump (S);
            Assert (False, N & ": Expected length" & L'Img & " got " & Num_Elements (S)'Img);
         end if;
      end Check_Length;

      procedure Check_Length (N : String;
                              L : Offset_Type;
                              A : Attributes_Type)
      is
      begin
         if Num_Elements (A) /= L
         then
            Dump (A);
            Assert (Num_Elements (A) = L, N & ": Expected length" & L'Img & " got" & Num_Elements (A)'Img);
         end if;
      end Check_Length;
   begin
      Check_Length ("C1", 1, E ("name"));
      Check_Length ("C2", 2, E ("name") + E ("name"));
      Check_Length ("C3", 2, E ("name", E ("child")));
      Check_Length ("C4", 2, A ("attr", "name"));
      Check_Length ("C5", 5, E ("parent", A ("attr1", "name1") + A ("attr2", "name2")));
      Check_Length ("C6", 4, E ("parent", A ("attr1", "name1"), C ("test")));
      Check_Length ("C7", 5, E ("parent", A ("attr1", "name1"), C ("longstringtest")));
      Check_Length ("C8", 7, E ("parent", A ("attr1", "name1"), E ("child", C ("longstringtest") + C ("short"))));
   end Test_Context_Length;

   ---------------------------------------------------------------------------

   procedure Register_Tests (T: in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Generate_Single_Node'Access, "Generate single node");
      Register_Routine (T, Test_Generate_Single_Node_Attrib'Access, "Generate single node with attribute");
      Register_Routine (T, Test_Generate_Nodes'Access, "Generate nodes");
      Register_Routine (T, Test_String_Attribute'Access, "String attribute");
      Register_Routine (T, Test_Integer_Attribute'Access, "Integer attribute");
      Register_Routine (T, Test_Float_Attribute'Access, "Float attribute");
      Register_Routine (T, Long_Tag_Name'Access, "Long tag name");
      Register_Routine (T, Long_Attribute_Name'Access, "Long attribute name");
      Register_Routine (T, Long_Attribute_Value'Access, "Long attribute value");
      Register_Routine (T, Multiple_Attributes'Access, "Multiple attributes");
      Register_Routine (T, Nested'Access, "Nested");
      Register_Routine (T, Test_Generate_Simple_Content'Access, "Simple content");
      Register_Routine (T, Test_Generate_Escape_Content'Access, "Escaped content");
      Register_Routine (T, Test_Generate_Interleaved_Content'Access, "Interleaved content");
      Register_Routine (T, Test_Context_Length'Access, "Context length");
   end Register_Tests;

   ---------------------------------------------------------------------------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("SXML Generator Tests");
   end Name;

end SXML_Generator_Tests;
