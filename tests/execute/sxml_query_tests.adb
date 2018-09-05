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

   ---------------------------------------------------------------------------

   procedure Test_Query_Node_Name (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      use SXML.Query;
      Doc   : Subtree_Type := E ("config");
      State : State_Type   := Init (Doc);
      Name  : String       := State.Name (Doc);
   begin
      Assert (Name = "config", "Unexpected name: """ & Name & """ (expected ""config"")");
	end Test_Query_Node_Name;

   ---------------------------------------------------------------------------

   procedure Test_Query_Child (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      use SXML.Query;
      Doc    : Subtree_Type := E ("config", E ("child"));
      State  : State_Type   := Init (Doc);
      Result : Result_Type;
   begin
      State.Child (Doc, Result);
      Assert (Result = Result_OK, "Expected child");
      declare
         Child_Name : constant String := State.Name (Doc);
      begin
         Assert (Child_Name = "child", "Unexpected name: """ & Child_Name & """ (expected ""config"")");
      end;
	end Test_Query_Child;

   ---------------------------------------------------------------------------

   procedure Test_Query_No_Child (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      use SXML.Query;
      Doc    : Subtree_Type := E ("config");
      State  : State_Type   := Init (Doc);
      Result : Result_Type;
   begin
      State.Child (Doc, Result);
      Assert (Result = Result_Not_Found, "Expected no child");
	end Test_Query_No_Child;

   ---------------------------------------------------------------------------

   procedure Test_Query_All_Children (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      use SXML.Query;
      Doc    : Subtree_Type := E ("config", E ("child1") + E ("child2") + E ("child3"));
      State  : State_Type   := Init (Doc);
      Result : Result_Type;
   begin
      State.Child (Doc, Result);
      Assert (Result = Result_OK, "First child not found");
      Assert (State.Name (Doc) = "child1", "Invalid first child");
      State.Sibling (Doc, Result);
      Assert (Result = Result_OK, "Second child not found");
      Assert (State.Name (Doc) = "child2", "Invalid second child");
      State.Sibling (Doc, Result);
      Assert (Result = Result_OK, "Third child not found");
      Assert (State.Name (Doc) = "child3", "Invalid third child");
      State.Sibling (Doc, Result);
      Assert (Result = Result_Not_Found, "Expected no more children");
	end Test_Query_All_Children;

   ---------------------------------------------------------------------------

   procedure Test_Query_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      use SXML.Query;
      Doc    : Subtree_Type := E ("config", A ("attribute1", "value1"));
      State  : State_Type   := Init (Doc);
      Result : Result_Type;
   begin
      State.Attribute (Doc, Result);
      Assert (Result = Result_OK, "Invalid attribute");
      declare
         Name  : String := State.Name (Doc);
         Value : String := State.Value (Doc);
      begin
         Assert (Name = "attribute1", "Unexpected name: """ & Name & """ (expected ""attribute1"")");
         Assert (Value = "value1", "Unexpected value: """ & Value & """ (expected ""value1"")");
      end;
	end Test_Query_Attribute;

   ---------------------------------------------------------------------------

   procedure Test_Query_Multiple_Attributes (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      use SXML.Query;
      Doc    : Subtree_Type := E ("config",
                                  A ("attribute1", "value1") +
                                  A ("attribute2", "value2") +
                                  A ("attribute3", "value3") +
                                  A ("attribute4", "value4"));
      State  : State_Type   := Init (Doc);
      Result : Result_Type;
   begin
      State.Attribute (Doc, Result);
      Assert (Result = Result_OK, "Invalid attribute");
      Assert (State.Name (Doc) = "attribute1", "Unexpected name");
      Assert (State.Value (Doc) = "value1", "Unexpected name");
      State.Next_Attribute (Doc, Result);
      Assert (Result = Result_OK, "Invalid attribute");
      Assert (State.Name (Doc) = "attribute2", "Unexpected name");
      Assert (State.Value (Doc) = "value2", "Unexpected name");
      State.Next_Attribute (Doc, Result);
      Assert (Result = Result_OK, "Invalid attribute");
      Assert (State.Name (Doc) = "attribute3", "Unexpected name");
      Assert (State.Value (Doc) = "value3", "Unexpected name");
      State.Next_Attribute (Doc, Result);
      Assert (Result = Result_OK, "Invalid attribute");
      Assert (State.Name (Doc) = "attribute4", "Unexpected name");
      Assert (State.Value (Doc) = "value4", "Unexpected name");
      State.Next_Attribute (Doc, Result);
      Assert (Result = Result_Not_Found, "Expected end of attributes");
   end Test_Query_Multiple_Attributes;

   ---------------------------------------------------------------------------

   procedure Register_Tests (T: in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Query_Node_Name'Access, "Query node name");
      Register_Routine (T, Test_Query_Child'Access, "Query child");
      Register_Routine (T, Test_Query_No_Child'Access, "Query no child");
      Register_Routine (T, Test_Query_All_Children'Access, "Query all children");
      Register_Routine (T, Test_Query_Attribute'Access, "Query attribute name and value");
      Register_Routine (T, Test_Query_Multiple_Attributes'Access, "Query multiple attribute");
   end Register_Tests;

   ---------------------------------------------------------------------------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("SXML Query Tests");
   end Name;

end SXML_Query_Tests;