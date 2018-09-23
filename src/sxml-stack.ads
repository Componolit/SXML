generic
   type Element_Type is private;
   type Stack_Type is array (Natural range <>) of Element_Type;
   S            : in out Stack_Type;
   Null_Element : Element_Type;
package SXML.Stack is

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

   procedure Drop
   with
      Pre => not Is_Empty;

   procedure Reset
   with
      Post => not Is_Full and Is_Empty;

   procedure Init;

private

   Index : Natural := S'First;

end SXML.Stack;
