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

   function Name (State : State_Type;
                  Doc   : Document_Type) return String
   is
      Result : Result_Type;
      Last   : Natural;
   begin
      Name (State, Doc, Result, Scratch, Last);
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
      Value (State, Doc, Result, Scratch, Last);
      if Result = Result_OK
      then
         return Scratch (1 .. Last);
      else
         return "<<Invalid Value>>";
      end if;
   end Value;

   function Path_Query (Context      : in out SXML.Document_Type;
                        Input_File   : String;
                        Query_String : String;
                        Position     : out Natural) return State_Type
   is
      use SXML;
      use SXML.Query;
      Input : access String := Read_File (Input_File);
      use SXML.Parser;
      Match  : Match_Type;
      Result : State_Type := Init (Context);
   begin
      Parse (Input.all, Context, Position, Match);
      if Match /= Match_OK
      then
         return Invalid_State;
      end if;
      return Path (Result, Context, Query_String);
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
      Doc   : Document_Type := E ("config", E ("child"));
      State : State_Type   := Init (Doc);
   begin
      State := Child (State, Doc);
      Assert (State.Result = Result_OK, "Expected child");
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
      Doc   : Document_Type := E ("config");
      State : State_Type := Init (Doc);
   begin
      State := Child (State, Doc);
      Assert (State.Result = Result_Not_Found, "Expected no child");
   end Test_Query_No_Child;

   ---------------------------------------------------------------------------

   procedure Test_Query_All_Children (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      use SXML.Query;
      Doc    : Document_Type := E ("config", E ("child1") + E ("child2") + E ("child3"));
      State  : State_Type := Init (Doc);
   begin
      State := Child (State, Doc);
      Assert (State.Result = Result_OK, "First child not found");
      Assert (Name (State, Doc) = "child1", "Invalid first child");
      State := Sibling (State, Doc);
      Assert (State.Result = Result_OK, "Second child not found");
      Assert (Name (State, Doc) = "child2", "Invalid second child");
      State := Sibling (State, Doc);
      Assert (State.Result = Result_OK, "Third child not found");
      Assert (Name (State, Doc) = "child3", "Invalid third child");
      State := Sibling (State, Doc);
      Assert (State.Result = Result_Not_Found, "Expected no more children");
   end Test_Query_All_Children;

   ---------------------------------------------------------------------------

   procedure Test_Query_Find_Sibling (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      use SXML.Query;
      Doc   : Document_Type := E ("config", E ("child1") + E ("child2") + E ("child3"));
      State : State_Type := Init (Doc);
   begin
      State := Child (State, Doc);
      Assert (State.Result = Result_OK, "First child not found");
      State := Find_Sibling (State, Doc, "child2");
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
      State : State_Type := Init (Doc);
   begin
      State := Child (State, Doc);
      Assert (State.Result = Result_OK, "First child not found");
      Assert (Name (State, Doc) = "child1", "First child has wrong value");
      State := Find_Sibling (State, Doc, "child2");
      Assert (State.Result = Result_OK, "Second child not found: " & State.Result'Img);
      Assert (Name (State, Doc) = "child2", "Invalid child: " & Name (State, Doc));
   end Test_Query_Find_Sibling_With_Content;

   ---------------------------------------------------------------------------

   procedure Test_Query_Find_First_Sibling (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      use SXML.Query;
      Doc    : Document_Type := E ("config", E ("child1") + E ("child2") + E ("child3"));
      State  : State_Type := Init (Doc);
   begin
      State := Child (State, Doc);
      Assert (State.Result = Result_OK, "First child not found");
      State := Find_Sibling (State, Doc, "child1");
      Assert (Name (State, Doc) = "child1", "Invalid child");
   end Test_Query_Find_First_Sibling;

   ---------------------------------------------------------------------------

   procedure Test_Query_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
      use SXML;
      use SXML.Query;
      Doc   : Document_Type := E ("config", A ("attribute1", "value1"));
      State : State_Type := Init (Doc);
   begin
      State := Attribute (State, Doc);
      Assert (State.Result = Result_OK, "Invalid attribute");
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
      State : State_Type := Init (Doc);
   begin
      State := Attribute (State, Doc);
      Assert (State.Result = Result_OK, "Invalid attribute");
      Assert (Name (State, Doc) = "attribute1", "Unexpected name");
      Assert (Value (State, Doc) = "value1", "Unexpected name");
      State := Next_Attribute (State, Doc);
      Assert (State.Result = Result_OK, "Invalid attribute");
      Assert (Name (State, Doc) = "attribute2", "Unexpected name");
      Assert (Value (State, Doc) = "value2", "Unexpected name");
      State := Next_Attribute (State, Doc);
      Assert (State.Result = Result_OK, "Invalid attribute");
      Assert (Name (State, Doc) = "attribute3", "Unexpected name");
      Assert (Value (State, Doc) = "value3", "Unexpected name");
      State := Next_Attribute (State, Doc);
      Assert (State.Result = Result_OK, "Invalid attribute");
      Assert (Name (State, Doc) = "attribute4", "Unexpected name");
      Assert (Value (State, Doc) = "value4", "Unexpected name");
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
   begin
      State := Find_Attribute (State, Doc, "attribute2");
      Assert (State.Result = Result_OK, "Attribute not found");
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
      State : State_Type := Init (Doc);
   begin
      State := Find_Attribute (State, Doc, "does_not_exist");
      Assert (State.Result = Result_Not_Found, "Attribute must not be found: " & State.Result'Img);
   end Test_Query_Find_Attribute_Missing;

   ---------------------------------------------------------------------------

   procedure Test_Query_Attribute_Missing (T : in out Test_Cases.Test_Case'Class)
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
      Data   : Content_Type (1 .. 10);
      Last   : Natural;
   begin
      Attribute (State, Doc, "attribute2", Result, Data, Last);
      Assert (Result = Result_OK, "Attribute not found: " & Result'Img);
      Assert (Last = 6, "Attribute has invalid last: " & Last'Img);
      Assert (Data (Data'First .. Last) = "value2", "Attribute result invalid: " & Data (Data'First .. Last));
      Attribute (State, Doc, "does_not_exist", Result, Data, Last);
      Assert (Result = Result_Not_Found, "Attribute must not be found: " & Result'Img);
   end Test_Query_Attribute_Missing;

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
      State : State_Type := Init (Doc);
   begin
      State := Child (State, Doc);
      Assert (State.Result = Result_OK, "Expected child");
      Assert (Name (State, Doc) = "sub", "Unexpected name");
      State := Find_Attribute (State, Doc, "attribute2");
      Assert (State.Result = Result_OK, "Attribute not found");
      Assert (Value (State, Doc) = "value2", "Unexpected value");
   end Test_Query_Find_Sub_Attribute;

   ---------------------------------------------------------------------------

   procedure Test_Path_Query_Simple (T : in out Test_Cases.Test_Case'Class)
   is
      Context  : access SXML.Document_Type := new SXML.Document_Type (1 .. 1000000);
      Position : Natural;
      State    : State_Type :=
         Path_Query (Context.all,
                     "tests/data/Vitera_CCDA_SMART_Sample.xml", "/ClinicalDocument",
                     Position);
   begin
      Assert (State.Result = Result_OK, "Invalid result: " & State.Result'Img & " at" & Position'Img);
      Assert (Name (State, Context.all) = "ClinicalDocument", "Invalid name: " & Name (State, Context.all));
   end Test_Path_Query_Simple;

   ---------------------------------------------------------------------------

   procedure Test_Path_Query_Simple_Missing (T : in out Test_Cases.Test_Case'Class)
   is
      Context  : access SXML.Document_Type := new SXML.Document_Type (1 .. 1000000);
      Position : Natural;
      State    : State_Type :=
         Path_Query (Context.all,
                     "tests/data/Vitera_CCDA_SMART_Sample.xml", "/Does_Not_Exist",
                     Position);
   begin
      Assert (State.Result = Result_Not_Found, "Invalid result: " & State.Result'Img & " at" & Position'Img);
   end Test_Path_Query_Simple_Missing;

   ---------------------------------------------------------------------------

   procedure Test_Path_Query_Long (T : in out Test_Cases.Test_Case'Class)
   is
      Context : access SXML.Document_Type := new SXML.Document_Type (1 .. 1000000);
      Position : Natural;
      State : State_Type :=
         Path_Query (Context.all,
                     "tests/data/Vitera_CCDA_SMART_Sample.xml",
                     "/ClinicalDocument/recordTarget/patientRole/id",
                     Position);
   begin
      Assert (State.Result = Result_OK, "Invalid result: " & State.Result'Img & " at" & Position'Img);
      Assert (Name (State, Context.all) = "id", "Invalid node name: " & Name (State, Context.all));
      State := Find_Attribute (State, Context.all, "root");
      Assert (State.Result = Result_OK, "Invalid attribute: " & State.Result'Img);
      Assert (Value (State, Context.all) = "2.16.840.1.113883.3.140.1.0.6.4", "Invalid value");
   end Test_Path_Query_Long;

   ---------------------------------------------------------------------------

   procedure Test_Attribute_Value (T : in out Test_Cases.Test_Case'Class)
   is
      Context : access SXML.Document_Type := new SXML.Document_Type (1 .. 1000000);
      Position : Natural;
      Attr  : State_Type;
      State : State_Type :=
         Path_Query (Context.all,
                     "tests/data/attribute_value.xml",
                     "/root/child/elem",
                     Position);
   begin
      Assert (State.Result = Result_OK, "Invalid result: " & State.Result'Img & " at" & Position'Img);
      Assert (Name (State, Context.all) = "elem", "Invalid node name: " & Name (State, Context.all));

      Attr := Find_Attribute (State, Context.all, "id");
      Assert (Attr.Result = Result_OK, "Invalid attribute (1): " & Attr.Result'Img);
      Assert (Value (Attr, Context.all) = "1", "Invalid value (1)");

      Attr := Find_Attribute (State, Context.all, "id", "");
      Assert (Attr.Result = Result_OK, "Invalid attribute (1b): " & Attr.Result'Img);
      Assert (Value (Attr, Context.all) = "1", "Invalid value (1b)");

      Attr := Find_Attribute (State, Context.all, "id", "1");
      Assert (Attr.Result = Result_OK, "Invalid attribute (2): " & Attr.Result'Img);

      Attr := Find_Attribute (State, Context.all, "value", "a");
      Assert (Attr.Result = Result_OK, "Invalid value: " & Attr.Result'Img);

      Attr := Find_Attribute (State, Context.all, "id", "invalid");
      Assert (Attr.Result = Result_Not_Found, "Expected Result_Not_Found, got: " & Attr.Result'Img);

      Attr := Find_Attribute (State, Context.all);
      Assert (Attr.Result = Result_OK, "Invalid attribute (3): " & Attr.Result'Img);
      Assert (Name (Attr, Context.all) = "id", "Invalid attribute name: " & Name (Attr, Context.all));
      Assert (Value (Attr, Context.all) = "1", "Invalid attribute value: " & Value (Attr, Context.all));
   end Test_Attribute_Value;

   ---------------------------------------------------------------------------

   procedure Test_Find_Node_By_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
      Context : access SXML.Document_Type := new SXML.Document_Type (1 .. 1000000);
      Position : Natural;
      Elem, Attr : State_Type;
      State : State_Type :=
         Path_Query (Context.all,
                     "tests/data/attribute_value.xml",
                     "/root/child/elem",
                     Position);
   begin
      Assert (State.Result = Result_OK, "Invalid result: " & State.Result'Img & " at" & Position'Img);
      Assert (Name (State, Context.all) = "elem", "Invalid node name: " & Name (State, Context.all));

      Elem := Find_Sibling (State, Context.all, "elem", "id", "2");
      Assert (Elem.Result = Result_OK, "Element not found (1): " & Elem.Result'Img);
      Attr := Find_Attribute (Elem, Context.all, "value");
      Assert (Value (Attr, Context.all) = "b", "Invalid attribute (1): " & Value (Attr, Context.all));

      Elem := Find_Sibling (State, Context.all, "elem", "id", "4");
      Assert (Elem.Result = Result_OK, "Element not found (2): " & Elem.Result'Img);
      Attr := Find_Attribute (Elem, Context.all, "value");
      Assert (Value (Attr, Context.all) = "d", "Invalid attribute (2)" & Value (Attr, Context.all));

      Elem := Find_Sibling (State, Context.all, "elem", "id", "7");
      Assert (Elem.Result = Result_Not_Found, "Expected Result_Not_Found, got " & Elem.Result'Img);

      Elem := Find_Sibling (State, Context.all, "elem", "value", "e");
      Assert (Elem.Result = Result_OK, "Element not found (4): " & Elem.Result'Img);
      Attr := Find_Attribute (Elem, Context.all, "value");
      Assert (Value (Attr, Context.all) = "e", "Invalid attribute (4)" & Value (Attr, Context.all));

      Elem := Find_Sibling (State, Context.all, "elem", "id", "invalid");
      Assert (Elem.Result = Result_Not_Found, "Expected Result_Not_Found, got " & Elem.Result'Img);

   end Test_Find_Node_By_Attribute;

   ---------------------------------------------------------------------------

   procedure Test_Path_Query_With_Attribute (T : in out Test_Cases.Test_Case'Class)
   is
      Context  : access SXML.Document_Type := new SXML.Document_Type (1 .. 1000000);
      Position : Natural;
   begin
      declare
         Elem : State_Type := Path_Query (Context.all, "tests/data/attribute_value.xml", "/root/child2/elem[@id=3]", Position);
         Attr : State_Type;
      begin
         Assert (Elem.Result = Result_OK, "Element not found: " & Elem.Result'Img & " at" & Position'Img);
         Attr := Find_Attribute (Elem, Context.all, "value");
         Assert (Value (Attr, Context.all) = "c", "Invalid value, got: " & Value (Attr, Context.all));
      end;

      declare
         Elem : State_Type := Path_Query (Context.all, "tests/data/attribute_value.xml", "/root/child2/elem[@foo]", Position);
         Attr : State_Type;
      begin
         Assert (Elem.Result = Result_OK, "Element not found: " & Elem.Result'Img & " at" & Position'Img);
         Attr := Find_Attribute (Elem, Context.all, "value");
         Assert (Value (Attr, Context.all) = "x", "Invalid value");
      end;

      declare
         Elem : State_Type := Path_Query (Context.all, "tests/data/attribute_value.xml", "/root/child2/elem[", Position);
      begin
         Assert (Elem.Result = Result_Invalid, "Expected Result_Not_Found, got " & Elem.Result'Img);
      end;

      declare
         Elem : State_Type := Path_Query (Context.all, "tests/data/attribute_value.xml", "/root/child2/elem[x]", Position);
      begin
         Assert (Elem.Result = Result_Invalid, "Expected Result_Invalid, got " & Elem.Result'Img);
      end;

      declare
         Elem : State_Type := Path_Query (Context.all, "tests/data/attribute_value.xml", "/root/child2/elem[]", Position);
         Attr : State_Type;
      begin
         Assert (Elem.Result = Result_OK, "Element not found: " & Elem.Result'Img & " at" & Position'Img);
         Attr := Find_Attribute (Elem, Context.all, "value");
         Assert (Value (Attr, Context.all) = "a", "Invalid value");
      end;
   end Test_Path_Query_With_Attribute;

   ---------------------------------------------------------------------------

   procedure Test_Path_Query_With_Multiple_Attributes (T : in out Test_Cases.Test_Case'Class)
   is
      Context  : access SXML.Document_Type := new SXML.Document_Type (1 .. 1000000);
      Position : Natural;
   begin
      declare
         Elem : State_Type :=Path_Query (Context.all,
                                         "tests/data/attribute_value.xml", "/root/child3/elem[@value=d]/subelem[@id=3]",
                                         Position);
         Attr : State_Type;
      begin
         Assert (Elem.Result = Result_OK, "Element not found: " & Elem.Result'Img & " at" & Position'Img);
         Attr := Find_Attribute (Elem, Context.all, "value");
         Assert (Value (Attr, Context.all) = "43", "Invalid value, got: " & Value (Attr, Context.all));
      end;

      declare
         Elem : State_Type :=Path_Query (Context.all,
                                         "tests/data/attribute_value.xml", "/root/child3/elem[@value=e]/subelem[@id=7]",
                                         Position);
         Attr : State_Type;
      begin
         Assert (Elem.Result = Result_OK, "Element not found: " & Elem.Result'Img & " at" & Position'Img);
         Attr := Find_Attribute (Elem, Context.all, "value");
         Assert (Value (Attr, Context.all) = "47", "Invalid value, got: " & Value (Attr, Context.all));
      end;

   end Test_Path_Query_With_Multiple_Attributes;

   ---------------------------------------------------------------------------

   procedure Test_Relative_Path_Query (T : in out Test_Cases.Test_Case'Class)
   is
      Context  : access SXML.Document_Type := new SXML.Document_Type (1 .. 1000000);
      Position : Natural;
      Attr     : State_Type;
      State    : State_Type := Path_Query (Context.all, "tests/data/attribute_value.xml", "/root/child3", Position);
   begin
      Assert (State.Result = Result_OK, "Invalid result: " & State.Result'Img & " at" & Position'Img);
      State := Query.Path (State, Context.all, "/child3/elem[@value=e]/subelem/subsubelem");
      Assert (State.Result = Result_OK, "Invalid relative query: " & State.Result'Img);
      Attr := Find_Attribute (State, Context.all, "id");
      Assert (Attr.Result = Result_OK, "Invalid attribute (1): " & Attr.Result'Img);
      Assert (Value (Attr, Context.all) = "10", "Invalid value (1)");
   end Test_Relative_Path_Query;

   ---------------------------------------------------------------------------

   procedure Test_Simplified_Attribute_Value (T : in out Test_Cases.Test_Case'Class)
   is
      Context  : access SXML.Document_Type := new SXML.Document_Type (1 .. 1000000);
      Position : Natural;
      State : State_Type :=
         Path_Query (Context.all,
                     "tests/data/attribute_value.xml",
                     "/root/child/elem",
                     Position);
      Result : Result_Type;
      Data   : String (1..1000);
      Last   : Natural;
   begin
      Assert (State.Result = Result_OK, "Invalid result: " & State.Result'Img & " at" & Position'Img);
      Assert (Name (State, Context.all) = "elem", "Invalid node name: " & Name (State, Context.all));

      Assert (Has_Attribute (State, Context.all, "value"), "Attribute ""value"" not present");
      Attribute (State, Context.all, "value", Result, Data, Last);
      Assert (Result = Result_OK, "Error querying result: " & Result'Img);
      Assert (Data (Data'First .. Last) = "a", "Invalid attribute: " & Data (Data'First .. Last));
      Assert (not Has_Attribute (State, Context.all, "nonexistent"), "Unexpected attribute");

   end Test_Simplified_Attribute_Value;

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
      Register_Routine (T, Test_Query_Attribute_Missing'Access, "Query missing attribute");
      Register_Routine (T, Test_Query_Multiple_Attributes'Access, "Query multiple attribute");
      Register_Routine (T, Test_Query_Find_Attribute'Access, "Find attribute");
      Register_Routine (T, Test_Query_Find_Attribute_Missing'Access, "Find missing attribute");
      Register_Routine (T, Test_Query_Find_Sub_Attribute'Access, "Find attribute in sub-tree");
      Register_Routine (T, Test_Path_Query_Simple'Access, "Simple path query");
      Register_Routine (T, Test_Path_Query_Simple_Missing'Access, "Simple path query for missing path");
      Register_Routine (T, Test_Path_Query_Long'Access, "Long path query");
      Register_Routine (T, Test_Attribute_Value'Access, "Attribute value");
      Register_Routine (T, Test_Find_Node_By_Attribute'Access, "Find node by attribute value");
      Register_Routine (T, Test_Path_Query_With_Attribute'Access, "Path query with attribute");
      Register_Routine (T, Test_Path_Query_With_Multiple_Attributes'Access, "Path query with multiple attributes");
      Register_Routine (T, Test_Relative_Path_Query'Access, "Query with relative path");
      Register_Routine (T, Test_Simplified_Attribute_Value'Access, "Simplified attribute value");
   end Register_Tests;

   ---------------------------------------------------------------------------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("SXML Query Tests");
   end Name;

end SXML_Query_Tests;
