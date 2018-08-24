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

   procedure Check_Document (Input  : String;
                             Output : String := "INPUT")
   is
      package Parser is new SXML.Parser (Input);
      use SXML;
      use Parser;
      Result : Match_Type;
   begin
      Parser.Parse (Match => Result);
      Assert (Result = Match_OK, "Invalid result: " & Result'Img);
      declare
         Doc : constant String := To_String (Parser.Document);
      begin
         --  FIXME: Remove whitespace
         Assert (Doc = (if Output = "INPUT" then Input else Output),
            "Invalid result: " & Doc);
      end;
   end Check_Document;

   -----------------------------------------------------------------------------

   procedure Check_Invalid (Input : String)
   is
      package Parser is new SXML.Parser (Input);
      use SXML;
      use Parser;
      Result : Match_Type;
   begin
      Parser.Parse (Match => Result);
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
   end Register_Tests;

   ---------------------------------------------------------------------------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("SXML Parser Tests");
   end Name;

end SXML_Parser_Tests;
