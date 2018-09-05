package body SXML.Query is

   -----------
   -- Bound --
   -----------

   function Bound (Document : Subtree_Type;
                   State    : State_Type) return Boolean
   is
      pragma Unreferenced (Document, State);
   begin
      return True;
   end Bound;

   ----------
   -- Init --
   ----------

   function Init (Document : Subtree_Type) return State_Type
   is
      pragma Unreferenced (Document);
   begin
      return State_Type'(Offset => 0);
   end Init;

   ----------
   -- Name --
   ----------

   function Name (State    : State_Type;
                  Document : Subtree_Type) return String
   is
   begin
      return Get_String (Document, State.Offset);
   end Name;

end SXML.Query;
