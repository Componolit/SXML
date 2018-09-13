generic
   type Element_Type is private;
   Length : Natural;
package SXML.Queue is

   function Is_Empty return Boolean;
   --  Queue is empty

   function Is_Full return Boolean;
   --  Queue is full

   procedure Enqueue (E : Element_Type)
   with
      Pre => not Is_Full;
   --  Enqueue element into queue

   procedure Dequeue (E : out Element_Type)
   with
      Pre => not Is_Empty;
   --  Dequeue element from queue

   procedure Reset
   with
      Post => not Is_Full and Is_Empty;
   --  Reset queue to initial state


private

   type Queue_Index_Type is new Natural range 0 .. Length + 1;
   type Queue_Type is array (Queue_Index_Type range 0 .. Queue_Index_Type'Last - 1) of Element_Type;

   Q    : Queue_Type;
   Head : Queue_Index_Type := Q'First;
   Tail : Queue_Index_Type := Q'First;
end SXML.Queue;
