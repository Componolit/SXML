package SXML.Query is

   type Result_Type is (Result_Invalid,
                        Result_Not_Found,
                        Result_OK);

   type State_Type is tagged private;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Document : Subtree_Type;
                      State    : State_Type) return Boolean
   with
      Ghost;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (Document : Subtree_Type;
                     State    : State_Type) return Boolean
   with
      Ghost,
      Pre'Class => Is_Valid (Document, State);

   ----------------
   -- Is_Content --
   ----------------

   function Is_Content (Document : Subtree_Type;
                        State    : State_Type) return Boolean
   with
      Ghost,
      Pre'Class => Is_Valid (Document, State);

   ------------------
   -- Is_Attribute --
   ------------------

   function Is_Attribute (Document : Subtree_Type;
                          State    : State_Type) return Boolean
   with
      Ghost,
      Pre'Class => Is_Valid (Document, State);

   ----------
   -- Init --
   ----------

   function Init (Document : Subtree_Type) return State_Type
   with
      Pre'Class => Document'Length > 0,
      Post      => Is_Valid (Document, Init'Result) and then
                   Is_Open (Document, Init'Result);

   ----------
   -- Name --
   ----------

   function Name (State    : State_Type;
                  Document : Subtree_Type) return String
   with
      Pre'Class => Is_Valid (Document, State) and then
                   Is_Open (Document, State);

   -----------
   -- Child --
   -----------

   procedure Child (State    : in out State_Type;
                    Document : Subtree_Type;
                    Result   : out Result_Type)
   with
      Pre'Class  => Is_Valid (Document, State),
      Post'Class => (Result = Result_OK and
                     (Is_Valid (Document, State) and then
                      Is_Open (Document, State))) or
                    State = State'Old;

   -------------
   -- Sibling --
   -------------

   procedure Sibling (State    : in out State_Type;
                      Document : Subtree_Type;
                      Result   : out Result_Type)
   with
      Pre'Class  => Is_Valid (Document, State) and then
                    Is_Open (Document, State),
      Post'Class => (Result = Result_OK and
                     (Is_Valid (Document, State) and then
                        (Is_Open (Document, State) or
                         Is_Content (Document, State)))) or
                    State = State'Old;

   ------------------
   -- Find_Sibling --
   ------------------

   procedure Find_Sibling (State        : in out State_Type;
                           Document     : Subtree_Type;
                           Sibling_Name : String;
                           Result       : out Result_Type)
   with
      Pre'Class  => Is_Valid (Document, State) and then
                    Is_Open (Document, State),
      Post'Class => (Result = Result_OK and Is_Valid (Document, State)) or
                    State = State'Old;

   ---------------
   -- Attribute --
   ---------------

   procedure Attribute (State    : in out State_Type;
                        Document : Subtree_Type;
                        Result   : out Result_Type)
   with
      Pre'Class  => Is_Valid (Document, State) and then
                    Is_Open (Document, State),
      Post'Class => (Result = Result_OK and Is_Valid (Document, State)) or
                    State = State'Old;

   -----------
   -- Value --
   -----------

   function Value (State    : State_Type;
                   Document : Subtree_Type) return String
   with
      Pre'Class  => Is_Valid (Document, State) and then
                    Is_Attribute (Document, State);

   --------------------
   -- Next_Attribute --
   --------------------

   procedure Next_Attribute (State    : in out State_Type;
                             Document : Subtree_Type;
                             Result   : out Result_Type)
   with
      Pre'Class  => Is_Valid (Document, State) and then
                    Is_Attribute (Document, State),
      Post'Class => Is_Valid (Document, State);

   --------------------
   -- Find_Attribute --
   --------------------

   procedure Find_Attribute (State          : in out State_Type;
                             Document       : Subtree_Type;
                             Attribute_Name : String;
                             Result         : out Result_Type)
   with
      Pre'Class  => Is_Valid (Document, State) and then
                    Is_Open (Document, State),
      Post'Class => Is_Valid (Document, State);

   ----------
   -- Path --
   ----------

   procedure Path (State        : in out State_Type;
                   Document     : Subtree_Type;
                   Result       : out Result_Type;
                   Query_String : String);

private

   type State_Type is tagged
   record
      Offset : Offset_Type;
   end record;

end SXML.Query;
