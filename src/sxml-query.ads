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

   ----------------
   -- Is_Content --
   ----------------

   function Is_Content (Document : Document_Type;
                        State    : State_Type) return Boolean
   with
      Ghost,
      Pre'Class => Is_Valid (Document, State);

   ------------------
   -- Is_Attribute --
   ------------------

   function Is_Attribute (Document : Document_Type;
                          State    : State_Type) return Boolean
   with
      Ghost,
      Pre'Class => Is_Valid (Document, State);

   ----------
   -- Init --
   ----------

   function Init (Document : Document_Type) return State_Type
   with
      Post => Is_Valid (Document, Init'Result);

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

   --------------------
   -- Is_Valid_Value --
   --------------------

   function Is_Valid_Value (State    : State_Type;
                            Document : Document_Type) return Boolean
   with
      Pre'Class  => Is_Valid (Document, State) and then
                    Is_Attribute (Document, State);

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

private

   type State_Type is tagged
   record
      Offset : Offset_Type;
   end record;

end SXML.Query;
