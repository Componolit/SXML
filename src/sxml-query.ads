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
   type State_Type (Result : Result_Type := Result_Invalid) is private;
   --  @field  Result Result of the last operation

   Invalid_State : constant State_Type;

   ------------
   -- Offset --
   ------------

   function Offset (State : State_Type) return Offset_Type with
     Pre => State.Result = Result_OK,
     Ghost;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Document : Document_Type;
                      State    : State_Type) return Boolean with
     Ghost;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (Document : Document_Type;
                     State    : State_Type) return Boolean with
     Pre => State.Result = Result_OK and Is_Valid (Document, State);
   --  Current state points to open element in document
   --
   --  @param Document  Document
   --  @param State     Current query state

   ----------------
   -- Is_Content --
   ----------------

   function Is_Content (Document : Document_Type;
                        State    : State_Type) return Boolean with
     Ghost,
     Pre => State.Result = Result_OK and Is_Valid (Document, State);
   --  Current state points to content element in document
   --
   --  @param Document  Document
   --  @param State     Current query state

   ------------------
   -- Is_Attribute --
   ------------------

   function Is_Attribute (Document : Document_Type;
                          State    : State_Type) return Boolean with
     Ghost,
     Pre => State.Result = Result_OK and Is_Valid (Document, State);
   --  Current state points to attribute element in document
   --
   --  @param Document  Document
   --  @param State     Current query state

   ----------
   -- Init --
   ----------

   function Init (Document : Document_Type) return State_Type with
     Post => Is_Valid (Document, Init'Result);
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
             and then (Is_Valid (Document, State)
             and then (Is_Open (Document, State) or Is_Attribute (Document, State))),
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
             and then Is_Valid (Document, State),
     Post => (if Child'Result.Result = Result_OK
              then Offset (Child'Result) > Offset (State)
                   and then (Is_Valid (Document, Child'Result)
                             and then (Is_Open (Document, Child'Result)
                                       or Is_Content (Document, Child'Result))));
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
            and then Is_Valid (Document, State)
            and then (Is_Open (Document, State)
                      or Is_Content (Document, State)),
     Post => (if Sibling'Result.Result = Result_OK
              then Offset (Sibling'Result) > Offset (State)
                   and then (Is_Valid (Document, Sibling'Result)
                             and then (Is_Open (Document, Sibling'Result)
                                       or Is_Content (Document, Sibling'Result))));
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
             and then Is_Valid (Document, State)
             and then (Is_Open (Document, State)
                       or Is_Content (Document, State)),
     Post => (if Find_Sibling'Result.Result = Result_OK
              then Offset (Find_Sibling'Result) >= Offset (State)
                   and then (Is_Valid (Document, Find_Sibling'Result)
                             and then Is_Open (Document, Find_Sibling'Result)));
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
             and then Is_Valid (Document, State)
             and then Is_Open (Document, State),
     Post => (if Attribute'Result.Result = Result_OK
              then (Is_Valid (Document, Attribute'Result)
                    and then Is_Attribute (Document, Attribute'Result)
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
             and then Is_Valid (Document, State)
             and then Is_Open (Document, State);
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
              and then Is_Valid (Document, State)
              and then Is_Open (Document, State)
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
            and then Is_Valid (Document, State)
            and then Is_Attribute (Document, State);
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
            and then Is_Valid (Document, State)
            and then Is_Attribute (Document, State)
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
            and then Is_Valid (Document, State)
            and then Is_Attribute (Document, State),
     Post => (if Next_Attribute'Result.Result = Result_OK
              then Offset (Next_Attribute'Result) > Offset (State)
                   and then (Is_Valid (Document, Next_Attribute'Result)
                             and then Is_Attribute (Document, Next_Attribute'Result)
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
            and then Is_Valid (Document, State)
            and then Is_Open (Document, State),
     Post => Is_Valid (Document, Find_Attribute'Result)
             and then (if Find_Attribute'Result.Result = Result_OK
                       then Is_Attribute (Document, Find_Attribute'Result)
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
             and then Is_Valid (Document, State),
     Post => (if State.Result = Result_OK then Is_Valid (Document, Path'Result));
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

end SXML.Query;
