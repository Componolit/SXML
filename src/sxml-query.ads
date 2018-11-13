package SXML.Query
is
   type State_Type is tagged private;

   ------------
   -- Offset --
   ------------

   function Offset (State : State_Type) return Offset_Type
   with
      Ghost;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Document : Document_Type;
                      State    : State_Type) return Boolean
   with
      Ghost;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (Document : Document_Type;
                     State    : State_Type) return Boolean
   with
      Pre'Class => Is_Valid (Document, State);
   --  Current state points to open element in document
   --
   --  @param Document  Document
   --  @param State     Current query state

   ----------------
   -- Is_Content --
   ----------------

   function Is_Content (Document : Document_Type;
                        State    : State_Type) return Boolean
   with
      Ghost,
      Pre'Class => Is_Valid (Document, State);
   --  Current state points to content element in document
   --
   --  @param Document  Document
   --  @param State     Current query state

   ------------------
   -- Is_Attribute --
   ------------------

   function Is_Attribute (Document : Document_Type;
                          State    : State_Type) return Boolean
   with
      Ghost,
      Pre'Class => Is_Valid (Document, State);
   --  Current state points to attribute element in document
   --
   --  @param Document  Document
   --  @param State     Current query state

   ----------
   -- Init --
   ----------

   function Init (Document : Document_Type) return State_Type
   with
      Post => Is_Valid (Document, Init'Result);
   --  Initialize state
   --
   --  @param Document  Document to initialize state for

   ----------
   -- Name --
   ----------

   procedure Name (State    : State_Type;
                   Document : Document_Type;
                   Result   : out Result_Type;
                   Data     : in out Content_Type;
                   Last     : out Natural)
   with
      Pre'Class  => (Is_Valid (Document, State) and then
                    (Is_Open (Document, State) or Is_Attribute (Document, State)));
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

   procedure Child (State    : in out State_Type;
                    Document : Document_Type;
                    Result   : out Result_Type)
   with
      Pre'Class  => Is_Valid (Document, State),
      Post'Class => (if Result = Result_OK
                     then State.Offset > State'Old.Offset and
                          (Is_Valid (Document, State) and then
                           (Is_Open (Document, State) or
                            Is_Content (Document, State)))
                     else State = State'Old);
   --  Get child
   --
   --  @param State     Current state
   --  @param Document  Document
   --  @param Result    Result of operation

   -------------
   -- Sibling --
   -------------

   procedure Sibling (State    : in out State_Type;
                      Document : Document_Type;
                      Result   : out Result_Type)
   with
      Pre'Class  => Is_Valid (Document, State) and then
                    (Is_Open (Document, State) or
                     Is_Content (Document, State)),
      Post'Class => (if Result = Result_OK
                     then State.Offset > State'Old.Offset and
                          (Is_Valid (Document, State) and then
                             (Is_Open (Document, State) or
                                      Is_Content (Document, State)))
                     else State = State'Old);
   --  Get next sibling
   --
   --  @param State     Current state
   --  @param Document  Document
   --  @param Result    Result of operation

   ------------------
   -- Find_Sibling --
   ------------------

   procedure Find_Sibling (State        : in out State_Type;
                           Document     : Document_Type;
                           Sibling_Name : Content_Type;
                           Result       : out Result_Type)
   with
      Pre'Class  => Is_Valid (Document, State) and then
                     (Is_Open (Document, State) or
                      Is_Content (Document, State)),
      Post'Class => (if Result = Result_OK
                     then State.Offset >= State'Old.Offset and
                          (Is_Valid (Document, State) and then
                           Is_Open (Document, State))
                     else State = State'Old);
   --  Find sibling by name
   --
   --  @param State         Current state
   --  @param Document      Document
   --  @param Sibling_Name  Name of sibling
   --  @param Result        Result of operation

   ---------------
   -- Attribute --
   ---------------

   procedure Attribute (State    : in out State_Type;
                        Document : Document_Type;
                        Result   : out Result_Type)
   with
      Pre'Class  => Is_Valid (Document, State) and then
                    Is_Open (Document, State),
      Post'Class => (Result = Result_OK and
                     (Is_Valid (Document, State) and then
                      Is_Attribute (Document, State))) or
                    (Result /= Result_OK and State = State'Old);
   --  Get first attribute of opening element
   --
   --  @param State     Current state
   --  @param Document  Document
   --  @param Result    Result of operation

   --------------------
   -- Is_Valid_Value --
   --------------------

   function Is_Valid_Value (State    : State_Type;
                            Document : Document_Type) return Boolean
   with
      Pre'Class  => Is_Valid (Document, State) and then
                    Is_Attribute (Document, State);
   --  Check if current attribute has valid value
   --
   --  @param State     Current state
   --  @param Document  Document

   -----------
   -- Value --
   -----------

   procedure Value (State    : State_Type;
                    Document : Document_Type;
                    Result   : out Result_Type;
                    Data     : in out Content_Type;
                    Last     : out Natural)
   with
      Pre'Class => Is_Valid (Document, State) and then
                   Is_Attribute (Document, State) and then
                   Is_Valid_Value (State, Document);
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

   procedure Next_Attribute (State    : in out State_Type;
                             Document : Document_Type;
                             Result   : out Result_Type)
   with
      Pre'Class  => Is_Valid (Document, State) and then
                    Is_Attribute (Document, State),
      Post'Class => (if Result = Result_OK
                     then State.Offset > State'Old.Offset and
                          (Is_Valid (Document, State) and then
                           Is_Attribute (Document, State))
                     else State = State'Old);
   --  Get next attribute
   --
   --  @param State     Current state
   --  @param Document  Document
   --  @param Result    Result of operation

   --------------------
   -- Find_Attribute --
   --------------------

   procedure Find_Attribute (State          : in out State_Type;
                             Document       : Document_Type;
                             Attribute_Name : Content_Type;
                             Result         : out Result_Type)
   with
      Pre'Class  => Is_Valid (Document, State) and then
                    Is_Open (Document, State),
      Post'Class => Is_Valid (Document, State);
   --  Find attribute by name
   --
   --  @param State           Current state
   --  @param Document        Document
   --  @param Attribute_Name  Name to search for
   --  @param Result          Result of operation

   ----------
   -- Path --
   ----------

   procedure Path (State        : in out State_Type;
                   Document     : Document_Type;
                   Result       : out Result_Type;
                   Query_String : String)
   with
       Pre'Class => Query_String'First > 0 and
                    Query_String'First <= Query_String'Last and
                    Query_String'Last < Natural'Last and
                    Query_String'Length > 1 and
                    (Is_Valid (Document, State) and then
                     Is_Open (Document, State));
   --  Query element by path beging at root of document. Only
   --  simple path queries referencing element names are supported,
   --  e.g. /root/parent/child/grandchild.
   --  FIXME: Relative queries starting from state could be supported easily, add tests.
   --
   --  @param State         Current state
   --  @param Document      Document
   --  @param Result        Result of operation
   --  @param Query_String  Path to query

private

   type State_Type is tagged
   record
      Offset : Offset_Type;
   end record;

end SXML.Query;
