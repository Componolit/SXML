package body SXML.Queue is

   function Is_Empty return Boolean
   is (Head = Tail);

   function Is_Full return Boolean
   is (Head mod Q'Length = (Tail - 1) mod Q'Length);

   procedure Enqueue (E : Element_Type)
   is
   begin
      Q (Head) := E;
      Head := (Head + 1) mod Q'Length;
   end Enqueue;

   procedure Dequeue (E : out Element_Type)
   is
   begin
      E := Q (Tail);
      Tail := (Tail + 1) mod Q'Length;
   end Dequeue;

   procedure Reset
   is
   begin
      Head := Q'First;
      Tail := Q'First;
   end Reset;

end SXML.Queue;
