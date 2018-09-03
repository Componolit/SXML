package body SXML.Query is

   ----------
   -- Init --
   ----------

   function Init (Document : Subtree_Type) return State_Type
   is
   begin
      return State_Type'(Position => Document'First);
   end Init;

   ----------
   -- Name --
   ----------

   function Name (State    : State_Type;
                  Document : Subtree_Type) return String
   is
      pragma Unreferenced (State, Document);
   begin
      return "Invalid";
   end Name;

end SXML.Query;
