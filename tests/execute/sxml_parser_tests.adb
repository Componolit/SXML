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

package body SXML_Parser_Tests is

   procedure Test_Parser_Single_Node (T : in out Test_Cases.Test_Case'Class)
   is
      package Parser is new SXML.Parser ("<valid></valid>");
      use SXML;
      use Parser;
      Result : Match_Type;
   begin
      Parser.Parse (Match => Result);
      Assert (Result = Match_OK, "Invalid result: " & Result'Img);
      declare
         Doc : constant String := To_String (Parser.Document);
      begin
         Assert (Doc = "<valid></valid>", "Invalid result: " & Doc);
      end;
	end Test_Parser_Single_Node;

   ---------------------------------------------------------------------------

   procedure Register_Tests (T: in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Parser_Single_Node'Access, "Parse single node");
   end Register_Tests;

   ---------------------------------------------------------------------------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("SXML Parser Tests");
   end Name;

end SXML_Parser_Tests;
