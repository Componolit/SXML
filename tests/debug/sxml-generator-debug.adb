with SXML.Debug;

package body SXML.Generator.Debug
is
   procedure Dump (Context : Attributes_Type;
                   Short   : Boolean := False;
                   Message : String  := "Dumping attribute")
   is
   begin
      SXML.Debug.Dump (SXML.Subtree_Type (Context), Short, Message);
   end Dump;

end SXML.Generator.Debug;
