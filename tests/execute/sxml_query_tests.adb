--
-- @summary Tests for XML query interface
-- @author  Alexander Senier
-- @date    2018-09-03
--
-- Copyright (C) 2018 Componolit GmbH
--
-- This file is part of SXML, which is distributed under the terms of the
-- GNU Affero General Public License version 3.
--

with AUnit.Assertions; use AUnit.Assertions;
with SXML.Generator; use SXML.Generator;
with SXML.Query; use SXML.Query;

package body SXML_Query_Tests is

   procedure Test_Query_Node_Name (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      use SXML.Query;
      Doc   : Subtree_Type := E ("config");
      State : State_Type   := Init (Doc);
      Name  : String       := State.Name (Doc);
   begin
      Assert (Name = "config", "Unexpected name: " & Name & " (expected config)");
	end Test_Query_Node_Name;

   ---------------------------------------------------------------------------

   procedure Register_Tests (T: in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Query_Node_Name'Access, "Query node name");
   end Register_Tests;

   ---------------------------------------------------------------------------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("SXML Query Tests");
   end Name;

end SXML_Query_Tests;
