package SXML.Query is

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

private

   type State_Type is tagged
   record
      Offset : Offset_Type;
   end record;

end SXML.Query;
