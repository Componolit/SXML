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

   procedure Test_Generate_Node (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
   begin
      Assert (False, "Not implemented");
	end Test_Generate_Node;

   ---------------------------------------------------------------------------

   procedure Register_Tests (T: in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Generate_Node'Access, "Generate node");
   end Register_Tests;

   ---------------------------------------------------------------------------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("SXML Generator Tests");
   end Name;

end SXML_Generator_Tests;
