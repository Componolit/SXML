generic
   type Element_Type is private;
   type Stack_Type is array (SXML.Natural_Without_Last range <>) of Element_Type;
   S            : in out Stack_Type;
   Null_Element : Element_Type;
package SXML.Stack is

   function Level return Natural;

   function Is_Valid return Boolean
   with
      Ghost;

   function Is_Empty return Boolean;
   --  Stack is empty

   function Is_Full return Boolean;
   --  Stack is full

   procedure Push (E : Element_Type)
   with
      Pre  => Is_Valid and not Is_Full,
      Post => Is_Valid and not Is_Empty and Level = Level'Old + 1;
   --  Push element onto stack

   procedure Pop (E : out Element_Type)
   with
      Pre  => Is_Valid and not Is_Empty,
      Post => Is_Valid and not Is_Full and Level = Level'Old - 1;

   procedure Drop
   with
      Pre  => Is_Valid and not Is_Empty,
      Post => Is_Valid and not Is_Full and Level = Level'Old - 1;

   procedure Reset
   with
      Post => Is_Valid and Is_Empty and not Is_Full;

   procedure Init
   with
      Post => Is_Valid and Is_Empty and not Is_Full;

private

   Index : Natural := S'First;

   function Level return Natural
   is (Index);

   function Is_Valid return Boolean
   is (Index >= S'First and Index <= S'Last);

end SXML.Stack;
