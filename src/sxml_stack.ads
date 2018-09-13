generic
   type Element_Type is private;
   Length : Natural;
package SXML_Stack is

   function Is_Empty return Boolean;
   --  Stack is empty

   function Is_Full return Boolean;
   --  Stack is full

   procedure Push (E : Element_Type)
   with
      Pre => not Is_Full;
   --  Push element onto stack

   procedure Pop (E : out Element_Type)
   with
      Pre => not Is_Empty;

   procedure Reset
   with
      Post => not Is_Full and Is_Empty;

private

   type Stack_Index_Type is new Natural range 0 .. Length;
   type Stack_Type is array (Stack_Index_Type range 1 .. Stack_Index_Type'Last) of Element_Type;

   Index : Stack_Index_Type := Stack_Index_Type'First;
   S     : Stack_Type;

end SXML_Stack;
