package SXML.Query is

   type State_Type is tagged private;

   ----------
   -- Init --
   ----------

   function Init (Document : Subtree_Type) return State_Type;

   ----------
   -- Name --
   ----------

   function Name (State    : State_Type;
                  Document : Subtree_Type) return String;

private

   type State_Type is tagged
   record
      Position : Index_Type;
   end record;

end SXML.Query;
