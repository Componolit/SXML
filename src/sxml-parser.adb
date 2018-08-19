package body SXML.Parser is

   Context       : Subtree_Type (1 .. Context_Size);
   Context_Valid : Boolean := False;

   function Document_Valid return Boolean is (Context_Valid);

   -----------
   -- Parse --
   -----------

   procedure Parse (Match : out Match_Type)
   is
   begin
      Context_Valid := False;
      Context := (others => Null_Node);
      Match := Match_Invalid;
   end Parse;

   --------------
   -- Document --
   --------------

   function Document return Subtree_Type is (Context);

end SXML.Parser;
