package body SXML_Stack is

   function Is_Empty return Boolean
   is (Index = Stack_Index_Type'First);

   function Is_Full return Boolean
   is (Index < S'Last);

   procedure Push (E : Element_Type)
   is
   begin
      Index := Index + 1;
      S (Index) := E;
   end Push;

   procedure Pop (E : out Element_Type)
   is
   begin
      E := S (Index);
      Index := Index - 1;
   end Pop;

   procedure Reset
   is
   begin
      Index := Stack_Index_Type'First;
   end Reset;

end SXML_Stack;
