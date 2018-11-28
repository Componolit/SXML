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
with SXML.Parser;
with SXML_Utils; use SXML_Utils;
use SXML;

package body SXML_Query_Tests is

   Scratch : String (1 .. 1000);
   Stack_Buffer : access SXML.Parser.Stack_Type_Base := new SXML.Parser.Stack_Type_Base (1 .. 10000);

   function Name (State : State_Type;
                  Doc   : Document_Type) return String
   is
      Result : Result_Type;
      Last   : Natural;
   begin
      State.Name (Doc, Result, Scratch, Last);
      if Result = Result_OK
      then
         return Scratch (1 .. Last);
      else
         return "<<Invalid Name>>";
      end if;
   end Name;

   function Value (State : State_Type;
                   Doc   : Document_Type) return String
   is
      Result : Result_Type;
      Last   : Natural;
   begin
      State.Value (Doc, Result, Scratch, Last);
      if Result = Result_OK
      then
         return Scratch (1 .. Last);
      else
         return "<<Invalid Value>>";
      end if;
   end Value;

   procedure Path_Query (Context      : in out SXML.Document_Type;
                         Input_File   : String;
                         Query_String : String;
                         Result       : out SXML.Result_Type;
                         Position     : out Natural;
                         State        : out State_Type)
   is
      use SXML;
      use SXML.Query;
      Input : access String := Read_File (Input_File);
      use SXML.Parser;
      Match : Match_Type;
   begin
      State := Init (Context);
      Result := Result_Invalid;
      Parser.Parse (Input.all, Context, Stack_Buffer.all, Match, Position);
      if Match /= Match_OK
      then
         Result := Result_Invalid;
         return;
      end if;
      State.Path (Context, Result, Query_String);
   end Path_Query;

   ---------------------------------------------------------------------------

   procedure Test_Query_Node_Name (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      use SXML.Query;
      Doc   : Document_Type := E ("config");
      State : State_Type   := Init (Doc);
      N     : String       := Name (State, Doc);
   begin
      Assert (N = "config", "Unexpected name: """ & N & """ (expected ""config"")");
	end Test_Query_Node_Name;

   ---------------------------------------------------------------------------

   procedure Test_Query_Child (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      use SXML.Query;
      Doc    : Document_Type := E ("config", E ("child"));
      State  : State_Type   := Init (Doc);
      Result : Result_Type;
   begin
      State.Child (Doc, Result);
      Assert (Result = Result_OK, "Expected child");
      declare
         Child_Name : constant String := Name (State, Doc);
      begin
         Assert (Child_Name = "child", "Unexpected name: """ & Child_Name & """ (expected ""config"")");
      end;
	end Test_Query_Child;

   ---------------------------------------------------------------------------

   procedure Test_Query_No_Child (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      use SXML.Query;
      Doc    : Document_Type := E ("config");
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
      Doc    : Document_Type := E ("config", E ("child1") + E ("child2") + E ("child3"));
      State  : State_Type   := Init (Doc);
      Result : Result_Type;
   begin
      State.Child (Doc, Result);
      Assert (Result = Result_OK, "First child not found");
      Assert (Name (State, Doc) = "child1", "Invalid first child");
      State.Sibling (Doc, Result);
      Assert (Result = Result_OK, "Second child not found");
      Assert (Name (State, Doc) = "child2", "Invalid second child");
      State.Sibling (Doc, Result);
      Assert (Result = Result_OK, "Third child not found");
      Assert (Name (State, Doc) = "child3", "Invalid third child");
      State.Sibling (Doc, Result);
      Assert (Result = Result_Not_Found, "Expected no more children");
	end Test_Query_All_Children;

   ---------------------------------------------------------------------------

   procedure Test_Query_Find_Sibling (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      use SXML.Query;
      Doc    : Document_Type := E ("config", E ("child1") + E ("child2") + E ("child3"));
      State  : State_Type   := Init (Doc);
      Result : Result_Type;
   begin
      State.Child (Doc, Result);
      Assert (Result = Result_OK, "First child not found");
      State.Find_Sibling (Doc, "child2", Result);
      Assert (Name (State, Doc) = "child2", "Invalid child");
	end Test_Query_Find_Sibling;

   ---------------------------------------------------------------------------

   procedure Test_Query_Find_Sibling_With_Content (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      use SXML.Query;
      Doc    : Document_Type := E ("config", E ("child1")
                                          + C ("content")
                                          + E ("child2")
                                          + E ("child3"));
      State  : State_Type   := Init (Doc);
      Result : Result_Type;
   begin
      State.Child (Doc, Result);
      Assert (Result = Result_OK, "First child not found");
      State.Find_Sibling (Doc, "child2", Result);
      Assert (Result = Result_OK, "Second child not found: " & Result'Img);
      Assert (Name (State, Doc) = "child2", "Invalid child: " & Name (State, Doc));
	end Test_Query_Find_Sibling_With_Content;

   ---------------------------------------------------------------------------

   procedure Test_Query_Find_First_Sibling (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      use SXML.Query;
      Doc    : Document_Type := E ("config", E ("child1") + E ("child2") + E ("child3"));
      State  : State_Type   := Init (Doc);
      Result : Result_Type;
   begin
      State.Child (Doc, Result);
      Assert (Result = Result_OK, "First child not found");
      State.Find_Sibling (Doc, "child1", Result);
      Assert (Name (State, Doc) = "child1", "Invalid child");
	end Test_Query_Find_First_Sibling;

   ---------------------------------------------------------------------------

   procedure Test_Query_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      use SXML.Query;
      Doc    : Document_Type := E ("config", A ("attribute1", "value1"));
      State  : State_Type   := Init (Doc);
      Result : Result_Type;
   begin
      State.Attribute (Doc, Result);
      Assert (Result = Result_OK, "Invalid attribute");
      declare
         N : String := Name (State, Doc);
         V : String := Value (State, Doc);
      begin
         Assert (N = "attribute1", "Unexpected name: """ & N & """ (expected ""attribute1"")");
         Assert (V = "value1", "Unexpected value: """ & V & """ (expected ""value1"")");
      end;
	end Test_Query_Attribute;

   ---------------------------------------------------------------------------

   procedure Test_Query_Multiple_Attributes (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      use SXML.Query;
      Doc    : Document_Type := E ("config",
                                  A ("attribute1", "value1") +
                                  A ("attribute2", "value2") +
                                  A ("attribute3", "value3") +
                                  A ("attribute4", "value4"));
      State  : State_Type   := Init (Doc);
      Result : Result_Type;
   begin
      State.Attribute (Doc, Result);
      Assert (Result = Result_OK, "Invalid attribute");
      Assert (Name (State, Doc) = "attribute1", "Unexpected name");
      Assert (Value (State, Doc) = "value1", "Unexpected name");
      State.Next_Attribute (Doc, Result);
      Assert (Result = Result_OK, "Invalid attribute");
      Assert (Name (State, Doc) = "attribute2", "Unexpected name");
      Assert (Value (State, Doc) = "value2", "Unexpected name");
      State.Next_Attribute (Doc, Result);
      Assert (Result = Result_OK, "Invalid attribute");
      Assert (Name (State, Doc) = "attribute3", "Unexpected name");
      Assert (Value (State, Doc) = "value3", "Unexpected name");
      State.Next_Attribute (Doc, Result);
      Assert (Result = Result_OK, "Invalid attribute");
      Assert (Name (State, Doc) = "attribute4", "Unexpected name");
      Assert (Value (State, Doc) = "value4", "Unexpected name");
      State.Next_Attribute (Doc, Result);
      Assert (Result = Result_Not_Found, "Expected end of attributes");
   end Test_Query_Multiple_Attributes;

   ---------------------------------------------------------------------------

   procedure Test_Query_Find_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      use SXML.Query;
      Doc    : Document_Type := E ("config",
                                  A ("attribute1", "value1") +
                                  A ("attribute2", "value2") +
                                  A ("attribute3", "value3") +
                                  A ("attribute4", "value4"));
      State  : State_Type := Init (Doc);
      Result : Result_Type;
   begin
      State.Find_Attribute (Doc, "attribute2", Result);
      Assert (Result = Result_OK, "Attribute not found");
      Assert (Value (State, Doc) = "value2", "Unexpected value");
   end Test_Query_Find_Attribute;

   ---------------------------------------------------------------------------

   procedure Test_Query_Find_Attribute_Missing (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      use SXML.Query;
      Doc    : Document_Type := E ("config",
                                  A ("attribute1", "value1") +
                                  A ("attribute2", "value2") +
                                  A ("attribute3", "value3") +
                                  A ("attribute4", "value4"));
      State  : State_Type := Init (Doc);
      Result : Result_Type;
   begin
      State.Find_Attribute (Doc, "does_not_exist", Result);
      Assert (Result = Result_Not_Found, "Attribute must not be found: " & Result'Img);
   end Test_Query_Find_Attribute_Missing;

   ---------------------------------------------------------------------------

   procedure Test_Query_Find_Sub_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      use SXML.Query;
      Doc    : Document_Type := E ("config", E ("sub",
                                                A ("attribute1", "value1") +
                                                A ("attribute2", "value2") +
                                                A ("attribute3", "value3") +
                                                A ("attribute4", "value4")));
      State  : State_Type := Init (Doc);
      Result : Result_Type;
   begin
      State.Child (Doc, Result);
      Assert (Result = Result_OK, "Expected child");
      Assert (Name (State, Doc) = "sub", "Unexpected name");
      State.Find_Attribute (Doc, "attribute2", Result);
      Assert (Result = Result_OK, "Attribute not found");
      Assert (Value (State, Doc) = "value2", "Unexpected value");
   end Test_Query_Find_Sub_Attribute;

   ---------------------------------------------------------------------------

   procedure Test_Path_Query_Simple (T : in out Test_Cases.Test_Case'Class)
   is
      State    : State_Type;
      Result   : Result_Type;
      Context  : access SXML.Document_Type := new SXML.Document_Type (1 .. 1000000);
      Position : Natural;
   begin
      Path_Query (Context.all, "tests/data/Vitera_CCDA_SMART_Sample.xml", "/ClinicalDocument", Result, Position, State);
      Assert (Result = Result_OK, "Invalid result: " & Result'Img & " at" & Position'Img);
      Assert (Name (State, Context.all) = "ClinicalDocument", "Invalid name");
	end Test_Path_Query_Simple;

   ---------------------------------------------------------------------------

   procedure Test_Path_Query_Simple_Missing (T : in out Test_Cases.Test_Case'Class)
   is
      State    : State_Type;
      Result   : Result_Type;
      Context  : access SXML.Document_Type := new SXML.Document_Type (1 .. 1000000);
      Position : Natural;
   begin
      Path_Query (Context.all, "tests/data/Vitera_CCDA_SMART_Sample.xml", "/Does_Not_Exist", Result, Position, State);
      Assert (Result = Result_Not_Found, "Invalid result: " & Result'Img & " at" & Position'Img);
	end Test_Path_Query_Simple_Missing;

   ---------------------------------------------------------------------------

   procedure Test_Path_Query_Long (T : in out Test_Cases.Test_Case'Class)
   is
      State   : State_Type;
      Result  : Result_Type;
      Context : access SXML.Document_Type := new SXML.Document_Type (1 .. 1000000);
      Position : Natural;
   begin
      Path_Query (Context.all,
                  "tests/data/Vitera_CCDA_SMART_Sample.xml",
                  "/ClinicalDocument/recordTarget/patientRole/id",
                  Result, Position, State);
      Assert (Result = Result_OK, "Invalid result: " & Result'Img & " at" & Position'Img);
      Assert (Name (State, Context.all) = "id", "Invalid node name: " & Name (State, Context.all));
      State.Find_Attribute (Context.all, "root", Result);
      Assert (Result = Result_OK, "Invalid attribute: " & Result'Img);
      Assert (Value (State, Context.all) = "2.16.840.1.113883.3.140.1.0.6.4", "Invalid value");
	end Test_Path_Query_Long;

   ---------------------------------------------------------------------------

   procedure Register_Tests (T: in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Query_Node_Name'Access, "Query node name");
      Register_Routine (T, Test_Query_Child'Access, "Query child");
      Register_Routine (T, Test_Query_No_Child'Access, "Query no child");
      Register_Routine (T, Test_Query_All_Children'Access, "Query all children");
      Register_Routine (T, Test_Query_Find_Sibling'Access, "Find sibling by name");
      Register_Routine (T, Test_Query_Find_Sibling_With_Content'Access, "Find sibling with content");
      Register_Routine (T, Test_Query_Find_First_Sibling'Access, "Find first sibling by name");
      Register_Routine (T, Test_Query_Attribute'Access, "Query attribute name and value");
      Register_Routine (T, Test_Query_Multiple_Attributes'Access, "Query multiple attribute");
      Register_Routine (T, Test_Query_Find_Attribute'Access, "Find attribute");
      Register_Routine (T, Test_Query_Find_Attribute_Missing'Access, "Find missing attribute");
      Register_Routine (T, Test_Query_Find_Sub_Attribute'Access, "Find attribute in sub-tree");
      Register_Routine (T, Test_Path_Query_Simple'Access, "Simple path query");
      Register_Routine (T, Test_Path_Query_Simple_Missing'Access, "Simple path query for missing path");
      Register_Routine (T, Test_Path_Query_Long'Access, "Long path query");
   end Register_Tests;

   ---------------------------------------------------------------------------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("SXML Query Tests");
   end Name;

end SXML_Query_Tests;
