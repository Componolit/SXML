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

   type State_Type is private;

   Invalid_State : constant State_Type;
   Initial_State : constant State_Type;

   ------------------
   -- State_Result --
   ------------------

   --  Return result of state
   --
   --  @param State  Current query state
   function State_Result (State : State_Type) return Result_Type;

   ------------
   -- Offset --
   ------------

   --  Return offset
   --
   --  @param State  Current query state
   function Offset (State : State_Type) return Offset_Type with
     Pre => State_Result (State) = Result_OK,
     Ghost;

   --------------
   -- Is_Valid --
   --------------

   --  Current state is valid
   --
   --  @param State     Current query state
   --  @param Document  Document
   function Is_Valid (State    : State_Type;
                      Document : Document_Type) return Boolean with
     Ghost,
     Post => Is_Valid'Result = (Document'Length > 0 and (if State_Result (State) = Result_OK
                                                         then Offset (State) < Document'Length));

   -------------
   -- Is_Open --
   -------------

   --  Current state points to open element in document
   --
   --  @param State     Current query state
   --  @param Document  Document
   function Is_Open (State    : State_Type;
                     Document : Document_Type) return Boolean with
     Pre => State_Result (State) = Result_OK and Is_Valid (State, Document);

   ----------------
   -- Is_Content --
   ----------------

   --  Current state points to content element in document
   --
   --  @param State     Current query state
   --  @param Document  Document
   function Is_Content (State    : State_Type;
                        Document : Document_Type) return Boolean with
     Ghost,
     Pre => State_Result (State) = Result_OK and Is_Valid (State, Document);

   ------------------
   -- Is_Attribute --
   ------------------

   --  Current state points to attribute element in document
   --
   --  @param State     Current query state
   --  @param Document  Document
   function Is_Attribute (State    : State_Type;
                          Document : Document_Type) return Boolean with
     Ghost,
     Pre => State_Result (State) = Result_OK and Is_Valid (State, Document);

   ----------
   -- Init --
   ----------

   --  Initialize state
   --
   --  @param Document  Document to initialize state for
   function Init (Document : Document_Type) return State_Type is (Initial_State) with
     Post => Offset (Init'Result) < Document'Length;

   ----------
   -- Name --
   ----------

   --  Return name for current node
   --
   --  @param State     Current state
   --  @param Document  Document
   --  @param Result    Result of operation
   --  @param Data      Result data
   --  @param Last      Last valid element of result
   procedure Name (State    :     State_Type;
                   Document :     Document_Type;
                   Result   : out Result_Type;
                   Data     : out Content_Type;
                   Last     : out Natural) with
     Pre  => Valid_Content (Data'First, Data'Last)
             and then State_Result (State) = Result_OK
             and then Is_Valid (State, Document)
             and then (Is_Open (State, Document) or Is_Attribute (State, Document)),
     Post => (if Result = Result_OK then Last in Data'Range);

   -----------
   -- Child --
   -----------

   --  Get child
   --
   --  @param State     Current state
   --  @param Document  Document
   --  @return          Result of operation
   function Child (State    : State_Type;
                   Document : Document_Type) return State_Type with
     Pre  => State_Result (State) = Result_OK
             and then Is_Valid (State, Document),
     Post => (if State_Result (Child'Result) = Result_OK
              then Offset (Child'Result) > Offset (State)
                   and then (Is_Valid (Child'Result, Document)
                             and then (Is_Open (Child'Result, Document)
                                       or Is_Content (Child'Result, Document))));

   -------------
   -- Sibling --
   -------------

   --  Get next sibling
   --
   --  @param State     Current state
   --  @param Document  Document
   --  @return          Result of operation
   function Sibling (State    : State_Type;
                     Document : Document_Type) return State_Type with
     Pre => State_Result (State) = Result_OK
            and then Is_Valid (State, Document)
            and then (Is_Open (State, Document)
                      or Is_Content (State, Document)),
     Post => (if State_Result (Sibling'Result) = Result_OK
              then Offset (Sibling'Result) > Offset (State)
                   and then (Is_Valid (Sibling'Result, Document)
                             and then (Is_Open (Sibling'Result, Document)
                                       or Is_Content (Sibling'Result, Document))));

   ------------------
   -- Find_Sibling --
   ------------------

   --  Find sibling by name, beginning at State. The result has to match Sibling_Name, Attribute_Name and
   --  Attribute_Value. The special value "*" can be used to match any name or value. If State passed to the function
   --  already is a match, it is returned directly. If not, a matching sibling is searched. If no matching sibling
   --  is found, State_Result (State) will have the value Result_Not_Found.
   --
   --  @param State            Current state
   --  @param Document         Document
   --  @param Sibling_Name     Name of sibling
   --  @param Attribute_Name   Attribute name to match for
   --  @param Attribute_Value  Attribute valur to match for
   --  @return                 Result of operation
   function Find_Sibling (State           : State_Type;
                          Document        : Document_Type;
                          Sibling_Name    : String := "*";
                          Attribute_Name  : String := "*";
                          Attribute_Value : String := "*") return State_Type with
     Pre  => State_Result (State) = Result_OK
             and then Is_Valid (State, Document)
             and then (Is_Open (State, Document)
                       or Is_Content (State, Document)),
     Post => (if State_Result (Find_Sibling'Result) = Result_OK
              then Offset (Find_Sibling'Result) >= Offset (State)
                   and then (Is_Valid (Find_Sibling'Result, Document)
                             and then Is_Open (Find_Sibling'Result, Document)));

   ---------------
   -- Attribute --
   ---------------

   --  Get first attribute of opening element
   --
   --  @param State     Current state
   --  @param Document  Document
   --  @return          Result of operation
   function Attribute (State    : State_Type;
                       Document : Document_Type) return State_Type with
     Pre  => State_Result (State) = Result_OK
             and then Is_Valid (State, Document)
             and then Is_Open (State, Document),
     Post => (if State_Result (Attribute'Result) = Result_OK
              then (Is_Valid (Attribute'Result, Document)
                    and then Is_Attribute (Attribute'Result, Document)
                    and then Is_Valid_Value (Attribute'Result, Document)));

   -------------------
   -- Has_Attribute --
   -------------------

   --  Check whether node has an attribute
   --
   --  @param State           Current state
   --  @param Document        Document
   --  @param Attribute_Name  Name of attribute
   function Has_Attribute (State          : State_Type;
                           Document       : Document_Type;
                           Attribute_Name : String) return Boolean with
     Pre  => State_Result (State) = Result_OK
             and then Is_Valid (State, Document)
             and then Is_Open (State, Document);

   ---------------
   -- Attribute --
   ---------------

   --  Get first attribute by name
   --
   --  @param State           Current state
   --  @param Document        Document
   --  @param Attribute_Name  Name to match for
   --  @param Result          Result of operation
   --  @param Data            Result data
   --  @param Last            Last valid element of result
   procedure Attribute (State          :     State_Type;
                        Document       :     Document_Type;
                        Attribute_Name :     String;
                        Result         : out Result_Type;
                        Data           : out Content_Type;
                        Last           : out Natural) with
     Pre   => State_Result (State) = Result_OK
              and then Is_Valid (State, Document)
              and then Is_Open (State, Document)
              and then Data'Length > 0
              and then Data'Last <= Natural'Last - Chunk_Length,
     Post => (if Result = Result_OK then Last in Data'Range);

   --------------------
   -- Is_Valid_Value --
   --------------------

   --  Check if current attribute has valid value
   --
   --  @param State     Current state
   --  @param Document  Document
   function Is_Valid_Value (State    : State_Type;
                            Document : Document_Type) return Boolean with
     Pre => State_Result (State) = Result_OK
            and then Is_Valid (State, Document)
            and then Is_Attribute (State, Document);

   -----------
   -- Value --
   -----------

   --  Return value for current attribute
   --
   --  @param State     Current state
   --  @param Document  Document
   --  @param Result    Result of operation
   --  @param Data      Result data
   --  @param Last      Last valid element of result
   procedure Value (State    :     State_Type;
                    Document :     Document_Type;
                    Result   : out Result_Type;
                    Data     : out Content_Type;
                    Last     : out Natural) with
     Pre => Valid_Content (Data'First, Data'Last)
            and then State_Result (State) = Result_OK
            and then Is_Valid (State, Document)
            and then Is_Attribute (State, Document)
            and then Is_Valid_Value (State, Document),
     Post => (if Result = Result_OK
              then Last in Data'Range
                   and Is_Valid_Value (State, Document));

   --------------------
   -- Next_Attribute --
   --------------------

   --  Get next attribute
   --
   --  @param State     Current state
   --  @param Document  Document
   --  @return          Result of operation
   function Next_Attribute (State    : State_Type;
                            Document : Document_Type) return State_Type with
     Pre => State_Result (State) = Result_OK
            and then Is_Valid (State, Document)
            and then Is_Attribute (State, Document),
     Post => (if State_Result (Next_Attribute'Result) = Result_OK
              then Offset (Next_Attribute'Result) > Offset (State)
                   and then (Is_Valid (Next_Attribute'Result, Document)
                             and then Is_Attribute (Next_Attribute'Result, Document)
                             and then Is_Valid_Value (Next_Attribute'Result, Document)));

   --------------------
   -- Find_Attribute --
   --------------------

   --  Find attribute by name
   --
   --  @param State            Current state
   --  @param Document         Document
   --  @param Attribute_Name   Name to search for
   --  @param Attribute_Value  Value to match for
   --  @return                 Result of operation
   function Find_Attribute (State           : State_Type;
                            Document        : Document_Type;
                            Attribute_Name  : String := "*";
                            Attribute_Value : String := "*") return State_Type with
     Pre => State_Result (State) = Result_OK
            and then Is_Valid (State, Document)
            and then Is_Open (State, Document),
     Post => Is_Valid (Find_Attribute'Result, Document)
             and then (if State_Result (Find_Attribute'Result) = Result_OK
                       then Is_Attribute (Find_Attribute'Result, Document)
                            and then Is_Valid_Value (Find_Attribute'Result, Document));

   ----------
   -- Path --
   ----------

   --  Query element by path beginning at State. Path queries may reference
   --  element names, wildcards and attributes
   --  e.g. /root/parent/*/grandchild[@attribute=value]
   --
   --  @param State         Current state
   --  @param Document      Document
   --  @param Query_String  Path to query
   --  @return              Result of operation
   function Path (State        : State_Type;
                  Document     : Document_Type;
                  Query_String : String) return State_Type with
     Pre  => Query_String'First > 0
             and then Query_String'Length > 0
             and then State_Result (State) = Result_OK
             and then Is_Valid (State, Document),
     Post => (if State_Result (State) = Result_OK then Is_Valid (Path'Result, Document))
             and then (if State_Result (Path'Result) = Result_OK
                       then Is_Open (Path'Result, Document) or else
                            Is_Content (Path'Result, Document));

private

   --  State originally was a descriminant record with Offset only present when
   --  Result is Result_OK. This made the code unprovable with Community 2019.
   --  Cf. Componolit/Workarounds#12
   type State_Type is
      record
         Result : Result_Type := Result_Invalid;
         Offset : Offset_Type := 0;
      end record;

   function State_Result (State : State_Type) return Result_Type is (State.Result);

   Invalid_State : constant State_Type := (Result => Result_Invalid, Offset => 0);
   Initial_State : constant State_Type := (Result => Result_OK, Offset => 0);

end SXML.Query;
