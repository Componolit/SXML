--
-- \brief  Tests for XML generator
-- \author Alexander Senier
-- \date   2018-08-12
--
-- Copyright (C) 2018 Componolit GmbH
--
-- This file is part of JWX, which is distributed under the terms of the
-- GNU Affero General Public License version 3.
--

with AUnit.Assertions; use AUnit.Assertions;
with SXML;

package body SXML_Generator_Tests is

   procedure Test_Generate_Single_Node (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Doc : Subtree_Type := E ("config");
      XML : String := To_String (Doc);
   begin
      Assert (XML = "<config></config>",
         "Unexpected document: " & XML & " len:" & XML'Length'Img);
	end Test_Generate_Single_Node;

   ---------------------------------------------------------------------------

   procedure Test_Generate_Single_Node_Attrib (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Doc : Subtree_Type := E ("config", A ("attrib", "Foo"));
      XML : String := To_String (Doc);
   begin
      Assert (XML = "<config attrib=""Foo""></config>",
         "Unexpected document: " & XML & " len:" & XML'Length'Img);
	end Test_Generate_Single_Node_Attrib;

   ---------------------------------------------------------------------------

   procedure Test_Generate_Nodes (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      Doc : Subtree_Type := E ("config", E ("child"));
      XML : String := To_String (Doc);
   begin
      Assert (XML = "<config><child></child></config>",
         "Unexpected document: " & XML & " len:" & XML'Length'Img);
	end Test_Generate_Nodes;

   ---------------------------------------------------------------------------

   procedure Register_Tests (T: in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Generate_Single_Node'Access, "Generate single node");
      Register_Routine (T, Test_Generate_Single_Node_Attrib'Access, "Generate single node with attribute");
      Register_Routine (T, Test_Generate_Nodes'Access, "Generate nodes");
   end Register_Tests;

   ---------------------------------------------------------------------------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("SXML Generator Tests");
   end Name;

end SXML_Generator_Tests;
