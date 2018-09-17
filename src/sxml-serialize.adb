with SXML.Stack;

package body SXML.Serialize is

   type Mode_Type is (Mode_Open, Mode_Close);

   type Traversal_Type is
   record
      Index : Index_Type;
      Mode  : Mode_Type;
   end record;

   ---------
   -- Put --
   ---------

   procedure Put (Value    : String;
                  Data     : in out String;
                  Position : in out Integer);

   procedure Put (Value    : String;
                  Data     : in out String;
                  Position : in out Integer)
   is
   begin
      if Position < 0 or else
         Data'Last - Position < Value'Length
      then
         Position := -1;
         return;
      end if;

      Data (Data'First + Position .. Data'First + Position + Value'Length - 1) := Value;
      Position := Position + Value'Length;
   end Put;

   -----------------
   -- Put_Escaped --
   -----------------

   procedure Put_Escaped (Doc      : Subtree_Type;
                          Start    : Index_Type;
                          Data     : in out String;
                          Position : in out Integer);

   procedure Put_Escaped (Doc      : Subtree_Type;
                          Start    : Index_Type;
                          Data     : in out String;
                          Position : in out Integer)
   is
      procedure Put_Escaped_Char (Char : Character;
                                  D    : in out String;
                                  P    : in out Natural);

      procedure Put_Escaped_Char (Char : Character;
                                  D    : in out String;
                                  P    : in out Natural)
      is
      begin
         case Char is
            when '"' => Put ("&quot;", D, P);
            when ''' => Put ("&apos;", D, P);
            when '&' => Put ("&amp;", D, P);
            when '>' => Put ("&gt;", D, P);
            when '<' => Put ("&lt;", D, P);
            when others => Put ("" & Char, D, P);
         end case;
      end Put_Escaped_Char;

      N   : Node_Type;
      Pos : Index_Type := Start;
   begin

      loop
         N := Doc (Pos);
         for C of N.Data (1 .. Natural (N.Length))
         loop
            Put_Escaped_Char (C, Data, Position);
            if Position < 0
            then
               return;
            end if;
         end loop;
         exit when N.Next = Invalid_Relative_Index;
         Pos := Add (Pos, N.Next);
      end loop;
   end Put_Escaped;

   ---------------
   -- Serialize --
   ---------------

   procedure Serialize (Doc      : Subtree_Type;
                        Start    : Index_Type;
                        Data     : in out String;
                        Position : in out Integer);

   procedure Serialize (Doc      : Subtree_Type;
                        Start    : Index_Type;
                        Data     : in out String;
                        Position : in out Integer)
   is
      N   : Node_Type;
      Pos : Index_Type := Start;
   begin

      loop
         N := Doc (Pos);
         Put (N.Data (1 .. Natural (N.Length)), Data, Position);
         if Position < 0
         then
            return;
         end if;
         exit when N.Next = Invalid_Relative_Index;
         Pos := Add (Pos, N.Next);
      end loop;
   end Serialize;

   package S   is new SXML.Stack (Traversal_Type, 10000000);
   package Rev is new SXML.Stack (Index_Type, 20000000);

   ------------
   -- Handle --
   ------------

   procedure Handle (Doc      : Subtree_Type;
                     Current  : Index_Type;
                     Mode     : Mode_Type;
                     Data     : in out String;
                     Position : in out Natural);

   procedure Handle (Doc      : Subtree_Type;
                     Current  : Index_Type;
                     Mode     : Mode_Type;
                     Data     : in out String;
                     Position : in out Natural)
   is
      Attr  : Index_Type;
      Value : Index_Type;
      Pos   : Relative_Index_Type;
      N     : constant Node_Type := Doc (Current);
   begin

      if N.Kind = Kind_Content and
         Mode = Mode_Open
      then
         Put_Escaped (Doc, Current, Data, Position);
         return;
      end if;

      if Mode = Mode_Close and N.Children /= Invalid_Relative_Index
      then
         Put ("</", Data, Position);
         Serialize (Doc, Current, Data, Position);
         Put (">", Data, Position);
      end if;

      if Mode = Mode_Open
      then
         Put ("<", Data, Position);
         Serialize (Doc, Current, Data, Position);
         Pos  := N.Attributes;
         Attr := Current;
         while Pos /= Invalid_Relative_Index
         loop
            Attr  := Add (Attr, Pos);
            Value := Add (Attr, Doc (Attr).Value);
            Put (" ", Data, Position);
            Serialize (Doc, Attr, Data, Position);
            Put ("=""", Data, Position);
            Serialize (Doc, Value, Data, Position);
            Put ("""", Data, Position);
            Pos := Doc (Attr).Next_Attribute;
         end loop;
         if N.Children = Invalid_Relative_Index
         then
            Put ("/", Data, Position);
         end if;
         Put (">", Data, Position);
      end if;
   end Handle;

---------------
   -- To_String --
   ---------------

   procedure To_String (Doc  : Subtree_Type;
                        Data : out String;
                        Last : out Natural)
   is
      Child    : Relative_Index_Type;
      Element  : Index_Type;
      Current  : Traversal_Type;
      Position : Integer := 0;
   begin
      Last := 0;
      S.Reset;
      Rev.Reset;
      S.Push ((Doc'First, Mode_Open));

      while not S.Is_Empty
      loop
         S.Pop (Current);
         Handle (Doc, Current.Index, Current.Mode, Data, Position);
         if Position < 0
         then
            return;
         end if;

         if Current.Mode = Mode_Open
         then
            if S.Is_Full
            then
               return;
            end if;
            S.Push ((Current.Index, Mode_Close));
            Element := Current.Index;
            Child   := Doc (Element).Children;
            loop
               exit when Child = Invalid_Relative_Index;
               Element := Add (Element, Child);
               if Rev.Is_Full
               then
                  return;
               end if;
               Rev.Push (Element);
               Child := Doc (Element).Siblings;
            end loop;
            while not Rev.Is_Empty
            loop
               Rev.Pop (Element);
               if S.Is_Full
               then
                  return;
               end if;
               S.Push ((Element, Mode_Open));
            end loop;
         end if;

      end loop;

      Last := Position;
   end To_String;

end SXML.Serialize;
