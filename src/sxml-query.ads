--
--  @summary XML query specification
--  @author  Alexander Senier
--  @date    2018-11-15
--
--  Copyright (C) 2018 Componolit GmbH
--
--  This file is part of SXML, which is distributed under the terms of the
--  GNU Affero General Public License version 3.
--

package SXML.Query
is
   pragma Annotate (GNATprove, Terminating, SXML.Query);

   type State_Type (Result : Result_Type := Result_Invalid) is private;
   --  @field  Result Result of the last operation

   Invalid_State : constant State_Type;
   Initial_State : constant State_Type;

   ------------
   -- Offset --
   ------------

   function Offset (State : State_Type) return Offset_Type with
     Pre => State.Result = Result_OK,
     Ghost;
   --  Return offset
   --
   --  @param State  Current query state

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (State    : State_Type;
                      Document : Document_Type) return Boolean with
     Ghost,
     Post => Is_Valid'Result = (Document'Length > 0 and (if State.Result = Result_OK
                                                         then Offset (State) < Document'Length));
   --  Current state is valid
   --
   --  @param State     Current query state
   --  @param Document  Document

   -------------
   -- Is_Open --
   -------------

   function Is_Open (State    : State_Type;
                     Document : Document_Type) return Boolean with
     Pre => State.Result = Result_OK and Is_Valid (State, Document);
   --  Current state points to open element in document
   --
   --  @param State     Current query state
   --  @param Document  Document

   ----------------
   -- Is_Content --
   ----------------

   function Is_Content (State    : State_Type;
                        Document : Document_Type) return Boolean with
     Ghost,
     Pre => State.Result = Result_OK and Is_Valid (State, Document);
   --  Current state points to content element in document
   --
   --  @param State     Current query state
   --  @param Document  Document

   ------------------
   -- Is_Attribute --
   ------------------

   function Is_Attribute (State    : State_Type;
                          Document : Document_Type) return Boolean with
     Ghost,
     Pre => State.Result = Result_OK and Is_Valid (State, Document);
   --  Current state points to attribute element in document
   --
   --  @param State     Current query state
   --  @param Document  Document

   ----------
   -- Init --
   ----------

   function Init (Document : Document_Type) return State_Type is (Initial_State) with
     Post => Offset (Init'Result) < Document'Length;
   --  Initialize state
   --
   --  @param Document  Document to initialize state for

   ----------
   -- Name --
   ----------

   procedure Name (State    :     State_Type;
                   Document :     Document_Type;
                   Result   : out Result_Type;
                   Data     : out Content_Type;
                   Last     : out Natural) with
     Pre  => Valid_Content (Data'First, Data'Last)
             and then State.Result = Result_OK
             and then Is_Valid (State, Document)
             and then (Is_Open (State, Document) or Is_Attribute (State, Document)),
     Post => (if Result = Result_OK then Last in Data'Range);
   --  Return name for current node
   --
   --  @param State     Current state
   --  @param Document  Document
   --  @param Result    Result of operation
   --  @param Data      Result data
   --  @param Last      Last valid element of result

   -----------
   -- Child --
   -----------

   function Child (State    : State_Type;
                   Document : Document_Type) return State_Type with
     Pre  => State.Result = Result_OK
             and then Is_Valid (State, Document),
     Post => (if Child'Result.Result = Result_OK
              then Offset (Child'Result) > Offset (State)
                   and then (Is_Valid (Child'Result, Document)
                             and then (Is_Open (Child'Result, Document)
                                       or Is_Content (Child'Result, Document))));
   --  Get child
   --
   --  @param State     Current state
   --  @param Document  Document
   --  @return          Result of operation

   -------------
   -- Sibling --
   -------------

   function Sibling (State    : State_Type;
                     Document : Document_Type) return State_Type with
     Pre => State.Result = Result_OK
            and then Is_Valid (State, Document)
            and then (Is_Open (State, Document)
                      or Is_Content (State, Document)),
     Post => (if Sibling'Result.Result = Result_OK
              then Offset (Sibling'Result) > Offset (State)
                   and then (Is_Valid (Sibling'Result, Document)
                             and then (Is_Open (Sibling'Result, Document)
                                       or Is_Content (Sibling'Result, Document))));
   --  Get next sibling
   --
   --  @param State     Current state
   --  @param Document  Document
   --  @return          Result of operation

   ------------------
   -- Find_Sibling --
   ------------------

   function Find_Sibling (State           : State_Type;
                          Document        : Document_Type;
                          Sibling_Name    : String := "*";
                          Attribute_Name  : String := "*";
                          Attribute_Value : String := "*") return State_Type with
     Pre  => State.Result = Result_OK
             and then Is_Valid (State, Document)
             and then (Is_Open (State, Document)
                       or Is_Content (State, Document)),
     Post => (if Find_Sibling'Result.Result = Result_OK
              then Offset (Find_Sibling'Result) >= Offset (State)
                   and then (Is_Valid (Find_Sibling'Result, Document)
                             and then Is_Open (Find_Sibling'Result, Document)));
   --
   --  Find sibling by name, beginning at State. The result has to match Sibling_Name, Attribute_Name and
   --  Attribute_Value. The special value "*" can be used to match any name or value. If State passed to the function
   --  already is a match, it is returned directly. If not, a matching sibling is searched. If no matching sibling
   --  is found, State.Result will have the value Result_Not_Found.
   --
   --  @param State           Current state
   --  @param Document        Document
   --  @param Sibling_Name    Name of sibling
   --  @param Attribute_Name  Attribute name to match for
   --  @param Attribute_Value Attribute valur to match for
   --  @return                Result of operation

   ---------------
   -- Attribute --
   ---------------

   function Attribute (State    : State_Type;
                       Document : Document_Type) return State_Type with
     Pre  => State.Result = Result_OK
             and then Is_Valid (State, Document)
             and then Is_Open (State, Document),
     Post => (if Attribute'Result.Result = Result_OK
              then (Is_Valid (Attribute'Result, Document)
                    and then Is_Attribute (Attribute'Result, Document)
                    and then Is_Valid_Value (Attribute'Result, Document)));
   --  Get first attribute of opening element
   --
   --  @param State     Current state
   --  @param Document  Document
   --  @return          Result of operation

   -------------------
   -- Has_Attribute --
   -------------------

   function Has_Attribute (State          : State_Type;
                           Document       : Document_Type;
                           Attribute_Name : String) return Boolean with
     Pre  => State.Result = Result_OK
             and then Is_Valid (State, Document)
             and then Is_Open (State, Document);
   --  Check whether node has an attribute
   --
   --  @param State           Current state
   --  @param Document        Document
   --  @param Attribute_Name  Name of attribute

   ---------------
   -- Attribute --
   ---------------

   procedure Attribute (State          :     State_Type;
                        Document       :     Document_Type;
                        Attribute_Name :     String;
                        Result         : out Result_Type;
                        Data           : out Content_Type;
                        Last           : out Natural) with
     Pre   => State.Result = Result_OK
              and then Is_Valid (State, Document)
              and then Is_Open (State, Document)
              and then Data'Length > 0
              and then Data'Last <= Natural'Last - Chunk_Length,
     Post => (if Result = Result_OK then Last in Data'Range);
   --  Get first attribute by name
   --
   --  @param State     Current state
   --  @param Document  Document
   --  @param Data      Name to match for
   --  @param Result    Result of operation
   --  @param Data      Result data
   --  @param Last      Last valid element of result

   --------------------
   -- Is_Valid_Value --
   --------------------

   function Is_Valid_Value (State    : State_Type;
                            Document : Document_Type) return Boolean with
     Pre => State.Result = Result_OK
            and then Is_Valid (State, Document)
            and then Is_Attribute (State, Document);
   --  Check if current attribute has valid value
   --
   --  @param State     Current state
   --  @param Document  Document

   -----------
   -- Value --
   -----------

   procedure Value (State    :     State_Type;
                    Document :     Document_Type;
                    Result   : out Result_Type;
                    Data     : out Content_Type;
                    Last     : out Natural) with
     Pre => Valid_Content (Data'First, Data'Last)
            and then State.Result = Result_OK
            and then Is_Valid (State, Document)
            and then Is_Attribute (State, Document)
            and then Is_Valid_Value (State, Document),
     Post => (if Result = Result_OK
              then Last in Data'Range
                   and Is_Valid_Value (State, Document));
   --  Return value for current attribute
   --
   --  @param State     Current state
   --  @param Document  Document
   --  @param Result    Result of operation
   --  @param Data      Result data
   --  @param Last      Last valid element of result

   --------------------
   -- Next_Attribute --
   --------------------

   function Next_Attribute (State    : State_Type;
                            Document : Document_Type) return State_Type with
     Pre => State.Result = Result_OK
            and then Is_Valid (State, Document)
            and then Is_Attribute (State, Document),
     Post => (if Next_Attribute'Result.Result = Result_OK
              then Offset (Next_Attribute'Result) > Offset (State)
                   and then (Is_Valid (Next_Attribute'Result, Document)
                             and then Is_Attribute (Next_Attribute'Result, Document)
                             and then Is_Valid_Value (Next_Attribute'Result, Document)));
   --  Get next attribute
   --
   --  @param State     Current state
   --  @param Document  Document
   --  @return          Result of operation

   --------------------
   -- Find_Attribute --
   --------------------

   function Find_Attribute (State           : State_Type;
                            Document        : Document_Type;
                            Attribute_Name  : String := "*";
                            Attribute_Value : String := "*") return State_Type with
     Pre => State.Result = Result_OK
            and then Is_Valid (State, Document)
            and then Is_Open (State, Document),
     Post => Is_Valid (Find_Attribute'Result, Document)
             and then (if Find_Attribute'Result.Result = Result_OK
                       then Is_Attribute (Find_Attribute'Result, Document)
                            and then Is_Valid_Value (Find_Attribute'Result, Document));
   --  Find attribute by name
   --
   --  @param State           Current state
   --  @param Document        Document
   --  @param Attribute_Name  Name to search for
   --  @param Attribute_Value Value to match for
   --  @return                Result of operation

   ----------
   -- Path --
   ----------

   function Path (State        : State_Type;
                  Document     : Document_Type;
                  Query_String : String) return State_Type with
     Pre  => Query_String'First > 0
             and then Query_String'Length > 0
             and then State.Result = Result_OK
             and then Is_Valid (State, Document),
     Post => (if State.Result = Result_OK then Is_Valid (Path'Result, Document));
   --  Query element by path beginning at State. Path queries may reference
   --  element names, wildcards and attributes
   --  e.g. /root/parent/*/grandchild[@attribute=value]
   --
   --  @param State         Current state
   --  @param Document      Document
   --  @param Query_String  Path to query
   --  @return              Result of operation

private

   type State_Type (Result : Result_Type := Result_Invalid) is
      record
         case Result is
            when Result_OK =>
               Offset : Offset_Type;
            when others =>
               null;
         end case;
      end record;

   Invalid_State : constant State_Type := (Result => Result_Invalid);
   Initial_State : constant State_Type := (Result => Result_OK, Offset => 0);

end SXML.Query;
