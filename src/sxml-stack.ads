--
--  @summary Generic stack specification
--  @author  Alexander Senier
--  @date    2018-11-15
--
--  Copyright (C) 2018 Componolit GmbH
--
--  This file is part of SXML, which is distributed under the terms of the
--  GNU Affero General Public License version 3.
--

generic
   type Element_Type is private;
   Null_Element : Element_Type;
   Size         : Natural;
package SXML.Stack is

   function Level return Natural;
   --  Number of elements on stack

   function Is_Valid return Boolean with
     Ghost;
   --  Stack buffer is valid

   function Is_Empty return Boolean;
   --  Stack is empty

   function Is_Full return Boolean;
   --  Stack is full

   procedure Push (E : Element_Type) with
     Pre  => Is_Valid and not Is_Full,
     Post => Is_Valid and not Is_Empty and Level = Level'Old + 1;
   --  Push element onto stack
   --
   --  @param E  Element to push onto stack

   procedure Pop (E : out Element_Type) with
     Pre  => Is_Valid and not Is_Empty,
     Post => Is_Valid and not Is_Full and Level = Level'Old - 1;
   --  Pop an element off the stack
   --
   --  @param E  Result element

   procedure Drop with
     Pre  => Is_Valid and not Is_Empty,
     Post => Is_Valid and not Is_Full and Level = Level'Old - 1;
   --  Drop an element from stack

   procedure Reset with
     Post => Is_Valid and Is_Empty and not Is_Full;
   --  Reset stack without erasing data

   procedure Init with
     Post => Is_Valid and Is_Empty and not Is_Full;
   --  Initialize stack and clear stack buffer

end SXML.Stack;
