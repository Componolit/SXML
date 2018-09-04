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

package body SXML_Generator_Tests is

   procedure Expect (XML : String;
                    Expected : String)
   is
   begin
      Assert (XML = Expected,
         "Unexpected document: (" & XML & ") len:" & XML'Length'Img &
         ", expected (" & Expected & ") len:" & Expected'Length'Img);
   end Expect;

   ---------------------------------------------------------------------------

   procedure Test_Generate_Single_Node (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Doc : Subtree_Type := E ("config");
   begin
      Expect (To_String (Doc), "<config/>");
	end Test_Generate_Single_Node;

   ---------------------------------------------------------------------------

   procedure Test_Generate_Single_Node_Attrib (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Doc : Subtree_Type := E ("config", A ("attrib", "Foo"));
   begin
      Expect (To_String (Doc), "<config attrib=""Foo""/>");
	end Test_Generate_Single_Node_Attrib;

   ---------------------------------------------------------------------------

   procedure Test_Generate_Nodes (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Doc : Subtree_Type := E ("config", E ("child"));
   begin
      Expect (To_String (Doc), "<config><child/></config>");
	end Test_Generate_Nodes;

   ---------------------------------------------------------------------------

   procedure Test_String_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Doc : Subtree_Type := E ("config", E ("child", A ("attr", "value")));
   begin
      Expect (To_String (Doc), "<config><child attr=""value""/></config>");
	end Test_String_Attribute;

   ---------------------------------------------------------------------------

   procedure Test_Integer_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Doc : Subtree_Type := E ("config", E ("child", A ("attr", 42)));
   begin
      Expect (To_String (Doc), "<config><child attr=""42""/></config>");
	end Test_Integer_Attribute;

   ---------------------------------------------------------------------------

   procedure Test_Float_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Doc : Subtree_Type := E ("config", E ("child", A ("attr", 3.14)));
   begin
      Expect (To_String (Doc), "<config><child attr=""3.14000E+00""/></config>");
	end Test_Float_Attribute;

   ---------------------------------------------------------------------------

   procedure Long_Attribute_Value (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Val : String (1 .. 100) := (others => 'x');
      Doc : Subtree_Type := E ("config", A ("attr", Val));
   begin
      Expect (To_String (Doc), "<config attr=""" & Val & """/>");
	end Long_Attribute_Value;

   ---------------------------------------------------------------------------

   procedure Long_Attribute_Name (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Aname : String (1 .. 100) := (others => 'x');
      Doc : Subtree_Type := E ("config", A (Aname, "value"));
   begin
      Expect (To_String (Doc), "<config " & Aname & "=""value""/>");
	end Long_Attribute_Name;

   ---------------------------------------------------------------------------

   procedure Long_Tag_Name (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Name : String (1 .. 100) := (others => 'x');
      Doc  : Subtree_Type := E (Name, A ("attr", "value"));
   begin
      Expect (To_String (Doc), "<" & Name & " attr=""value""/>");
	end Long_Tag_Name;

   ---------------------------------------------------------------------------

   procedure Multiple_Attributes (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Long : String (1 .. 100) := (others => 'x');
      Doc  : Subtree_Type := E ("parent", A ("attr1", "value") & A ("attr2", Long) & A ("attr3_" & Long, "value"));
   begin
      Expect (To_String (Doc), "<parent attr1=""value"" attr2=""" & Long & """ attr3_" & Long & "=""value""/>");
	end Multiple_Attributes;

   ---------------------------------------------------------------------------

   procedure Nested (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Doc  : Subtree_Type :=
          E ("parent", A ("attr1", "value1") &
                       A ("attr2", "value2") &
                       A ("attr3", "value3"),
            E ("child1") &
            E ("child2",
               E ("subchild")));
   begin
      Expect (To_String (Doc), "<parent attr1=""value1"" attr2=""value2"" attr3=""value3""><child1/><child2><subchild/></child2></parent>");
	end Nested;

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
   end Register_Tests;

   ---------------------------------------------------------------------------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("SXML Generator Tests");
   end Name;

end SXML_Generator_Tests;
