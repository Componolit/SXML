package SXML.Query is

   type Result_Type is (Result_Invalid,
                        Result_Not_Found,
                        Result_OK);

   type State_Type is tagged private;

   function Bound (Document : Subtree_Type;
                   State    : State_Type) return Boolean
   with
      Ghost;

   ----------
   -- Init --
   ----------

   function Init (Document : Subtree_Type) return State_Type
   with
      Post => Bound (Document, Init'Result);

   ----------
   -- Name --
   ----------

   function Name (State    : State_Type;
                  Document : Subtree_Type) return String
   with
      Pre'Class => Bound (Document, State);

   -----------
   -- Child --
   -----------

   procedure Child (State    : in out State_Type;
                    Document : Subtree_Type;
                    Result   : out Result_Type)
   with
      Pre'Class  => Bound (Document, State),
      Post'Class => Bound (Document, State) and
                    (Result = Result_OK or State = State'Old);

   -------------
   -- Sibling --
   -------------

   procedure Sibling (State    : in out State_Type;
                      Document : Subtree_Type;
                      Result   : out Result_Type)
   with
      Pre'Class  => Bound (Document, State),
      Post'Class => Bound (Document, State) and
                    (Result = Result_OK or State = State'Old);

   ------------------
   -- Find_Sibling --
   ------------------

   procedure Find_Sibling (State        : in out State_Type;
                           Document     : Subtree_Type;
                           Sibling_Name : String;
                           Result       : out Result_Type)
   with
      Pre'Class  => Bound (Document, State),
      Post'Class => Bound (Document, State) and
                    (Result = Result_OK or State = State'Old);

   ---------------
   -- Attribute --
   ---------------

   procedure Attribute (State    : in out State_Type;
                        Document : Subtree_Type;
                        Result   : out Result_Type)
   with
      Pre'Class  => Bound (Document, State),
      Post'Class => Bound (Document, State) and
                    (Result = Result_OK or State = State'Old);

   -----------
   -- Value --
   -----------

   function Value (State    : State_Type;
                   Document : Subtree_Type) return String
   with
      Pre'Class  => Bound (Document, State);

   --------------------
   -- Next_Attribute --
   --------------------

   procedure Next_Attribute (State    : in out State_Type;
                             Document : Subtree_Type;
                             Result   : out Result_Type)
   with
      Pre'Class  => Bound (Document, State),
      Post'Class => Bound (Document, State);

   --------------------
   -- Find_Attribute --
   --------------------

   procedure Find_Attribute (State          : in out State_Type;
                             Document       : Subtree_Type;
                             Attribute_Name : String;
                             Result         : out Result_Type)
   with
      Pre'Class  => Bound (Document, State),
      Post'Class => Bound (Document, State);

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
