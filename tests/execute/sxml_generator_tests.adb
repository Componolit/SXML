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

   procedure Test_Generate_Single_Node (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Doc : Subtree_Type := E ("config");
      XML : String := To_String (Doc);
   begin
      Assert (XML = "<config/>",
         "Unexpected document: (" & XML & ") len:" & XML'Length'Img);
	end Test_Generate_Single_Node;

   ---------------------------------------------------------------------------

   procedure Test_Generate_Single_Node_Attrib (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Doc : Subtree_Type := E ("config", A ("attrib", "Foo"));
      XML : String := To_String (Doc);
   begin
      Assert (XML = "<config attrib=""Foo""/>",
         "Unexpected document: (" & XML & ") len:" & XML'Length'Img);
	end Test_Generate_Single_Node_Attrib;

   ---------------------------------------------------------------------------

   procedure Test_Generate_Nodes (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Doc : Subtree_Type := E ("config", E ("child"));
      XML : String := To_String (Doc);
   begin
      Assert (XML = "<config><child/></config>",
         "Unexpected document: (" & XML & ") len:" & XML'Length'Img);
	end Test_Generate_Nodes;

   ---------------------------------------------------------------------------

   procedure Test_String_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Doc : Subtree_Type := E ("config", E ("child", A ("attr", "value")));
      XML : String := To_String (Doc);
   begin
      Assert (XML = "<config><child attr=""value""/></config>",
         "Unexpected document: (" & XML & ") len:" & XML'Length'Img);
	end Test_String_Attribute;

   ---------------------------------------------------------------------------

   procedure Test_Integer_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Doc : Subtree_Type := E ("config", E ("child", A ("attr", 42)));
      XML : String := To_String (Doc);
   begin
      Assert (XML = "<config><child attr=""42""/></config>",
         "Unexpected document: (" & XML & ") len:" & XML'Length'Img);
	end Test_Integer_Attribute;

   ---------------------------------------------------------------------------

   procedure Test_Float_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Doc : Subtree_Type := E ("config", E ("child", A ("attr", 3.14)));
      XML : String := To_String (Doc);
   begin
      Assert (XML = "<config><child attr=""3.14000E+00""/></config>",
         "Unexpected document: (" & XML & ") len:" & XML'Length'Img);
	end Test_Float_Attribute;

   ---------------------------------------------------------------------------

   procedure Long_Attribute_Value (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Val : String (1 .. 100) := (others => 'x');
      Doc : Subtree_Type := E ("config", A ("attr", Val));
      XML : String := To_String (Doc);
   begin
      Assert (XML = "<config attr=""" & Val & """ />",
         "Unexpected document: (" & XML & ") len:" & XML'Length'Img);
	end Long_Attribute_Value;

   ---------------------------------------------------------------------------

   procedure Long_Attribute_Name (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Aname : String (1 .. 100) := (others => 'x');
      Doc : Subtree_Type := E ("config", A (Aname, "value"));
      XML : String := To_String (Doc);
   begin
      Assert (XML = "<config " & Aname & "=""value"" />",
         "Unexpected document: (" & XML & ") len:" & XML'Length'Img);
	end Long_Attribute_Name;

   ---------------------------------------------------------------------------

   procedure Long_Tag_Name (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Name : String (1 .. 100) := (others => 'x');
      Doc  : Subtree_Type := E (Name, A ("attr", "value"));
      XML  : String := To_String (Doc);
   begin
      Assert (XML = "<" & Name & " attr=""value"" />",
         "Unexpected document: (" & XML & ") len:" & XML'Length'Img);
	end Long_Tag_Name;

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
   end Register_Tests;

   ---------------------------------------------------------------------------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("SXML Generator Tests");
   end Name;

end SXML_Generator_Tests;
