--
--  @summary Generic stack implementation
--  @author  Alexander Senier
--  @date    2018-11-15
--
--  Copyright (C) 2018 Componolit GmbH
--
--  This file is part of SXML, which is distributed under the terms of the
--  GNU Affero General Public License version 3.
--

package body SXML.Stack
is
   pragma Annotate (GNATprove, Terminating, SXML.Stack);

   -----------
   -- Level --
   -----------

   function Capacity (S : Stack_Type) return Positive is
     (S.List'Last);

   -----------
   -- Level --
   -----------

   function Level (S : Stack_Type) return Natural is
     (S.Index);

   ----------
   -- Push --
   ----------

   procedure Push (S : in out Stack_Type;
                   E :        Element_Type) is
   begin
      S.Index := S.Index + 1;
      S.List (S.Index) := E;
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (S : in out Stack_Type;
                  E :    out Element_Type) is
   begin
      E := S.List (S.Index);
      Drop (S);
   end Pop;

   ----------
   -- Drop --
   ----------

   procedure Drop (S : in out Stack_Type) is
   begin
      S.Index := S.Index - 1;
   end Drop;

   -----------
   -- Reset --
   -----------

   procedure Reset (S : in out Stack_Type) is
   begin
      S.Index := 0;
   end Reset;

   ----------
   -- Init --
   ----------

   procedure Init (S            : out Stack_Type;
                   Null_Element :     Element_Type)
   is
   begin
      S.Index := 0;
      --  This would be the correct way to initialize S.List:
      --     S.List  := (others => Null_Element);
      --  As this creates a (potentially large) object on the stack, we initialize in a loop. The resulting flow
      --  error is justified in the spec.
      for E of S.List loop
         pragma Loop_Invariant (S.Index = 0);
         E := Null_Element;
      end loop;
   end Init;

end SXML.Stack;
