package body SXML.Stack
is

   function Is_Empty return Boolean
   is (Index = S'First);

   function Is_Full return Boolean
   is (Index >= S'Last);

   procedure Push (E : Element_Type)
   is
   begin
      S (Index) := E;
      Index := Index + 1;
   end Push;

   procedure Pop (E : out Element_Type)
   is
   begin
      Index := Index - 1;
      E := S (Index);
   end Pop;

   procedure Drop
   is
   begin
      Index := Index - 1;
   end Drop;

   procedure Reset
   is
   begin
      Index := S'First;
   end Reset;

   procedure Init
   is
   begin
      S := (others => Null_Element);
      Index := S'First;
   end Init;

end SXML.Stack;
