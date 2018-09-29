with SXML.Stack;

package body SXML.Serialize
is

   ---------
   -- Put --
   ---------

   procedure Put (Value    : String;
                  Data     : in out String;
                  Position : in out Integer)
   with
      Post => (if Position'Old < 0 then Position < 0);

   procedure Put (Value    : String;
                  Data     : in out String;
                  Position : in out Integer)
   is
   begin
      if Position < 0 or else
         Value'Length > Data'Length - Position or else
         Data'First > Data'Last - Position - Value'Length
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
                          Position : in out Integer)
   with
      Pre => Start in Doc'Range;

   procedure Put_Escaped (Doc      : Subtree_Type;
                          Start    : Index_Type;
                          Data     : in out String;
                          Position : in out Integer)
   is
      procedure Put_Escaped_Char (Char : Character;
                                  D    : in out String;
                                  P    : in out Integer);

      procedure Put_Escaped_Char (Char : Character;
                                  D    : in out String;
                                  P    : in out Integer)
      is
      begin
         case Char is
            when '"' => Put ("&quot;", D, P);
            when ''' => Put ("&apos;", D, P);
            when '&' => Put ("&amp;", D, P);
            when '>' => Put ("&gt;", D, P);
            when '<' => Put ("&lt;", D, P);
            when others => Put ((1 => Char), D, P);
         end case;
      end Put_Escaped_Char;

      N   : Node_Type;
      Pos : Index_Type := Start;
   begin

      loop
         pragma Loop_Invariant (Pos in Doc'Range);
         N := Doc (Pos);
         for C of N.Data (1 .. Natural (N.Length))
         loop
            Put_Escaped_Char (C, Data, Position);
            if Position < 0
            then
               return;
            end if;
         end loop;

         exit when
           N.Next = Invalid_Relative_Index or
           Overflow (Pos, N.Next);

         Pos := Add (Pos, N.Next);
         exit when not (Pos in Doc'Range);
      end loop;
   end Put_Escaped;

   --------------------
   -- Serialize_Data --
   --------------------

   procedure Serialize_Data (Doc      : Subtree_Type;
                             Start    : Index_Type;
                             Data     : in out String;
                             Position : in out Integer)
   with
      Pre => Start in Doc'Range;

   procedure Serialize_Data (Doc      : Subtree_Type;
                             Start    : Index_Type;
                             Data     : in out String;
                             Position : in out Integer)
   is
      N   : Node_Type;
      Pos : Index_Type := Start;
   begin

      loop
         pragma Loop_Invariant (Pos in Doc'Range);
         N := Doc (Pos);
         Put (N.Data (1 .. Natural (N.Length)), Data, Position);
         if Position < 0
         then
            return;
         end if;
         exit when
           N.Next = Invalid_Relative_Index or
           Overflow (Pos, N.Next);

         Pos := Add (Pos, N.Next);
         exit when not (Pos in Doc'Range);
      end loop;
   end Serialize_Data;

   ------------
   -- Handle --
   ------------

   procedure Handle (Doc      : Subtree_Type;
                     Current  : Index_Type;
                     Mode     : Mode_Type;
                     Data     : in out String;
                     Position : in out Integer)
   with
      Pre => Current in Doc'Range;

   procedure Handle (Doc      : Subtree_Type;
                     Current  : Index_Type;
                     Mode     : Mode_Type;
                     Data     : in out String;
                     Position : in out Integer)
   is
      Attr  : Index_Type;
      Value : Index_Type;
      Pos   : Relative_Index_Type;
   begin

      if Doc (Current).Kind = Kind_Content and
         Mode = Mode_Open
      then
         Put_Escaped (Doc, Current, Data, Position);
         return;
      end if;

      if Doc (Current).Kind /= Kind_Element_Open and
         Doc (Current).Kind /= Kind_Content
      then
         Position := -1;
         return;
      end if;

      if Mode = Mode_Close and
         Doc (Current).Children /= Invalid_Relative_Index
      then
         Put ("</", Data, Position);
         Serialize_Data (Doc, Current, Data, Position);
         Put (">", Data, Position);
      end if;

      if Mode = Mode_Open
      then
         Put ("<", Data, Position);
         Serialize_Data (Doc, Current, Data, Position);
         Pos  := Doc (Current).Attributes;
         Attr := Current;
         while Pos /= Invalid_Relative_Index and
               not Overflow (Attr, Pos)
         loop
            Attr := Add (Attr, Pos);
            exit when not (Attr in Doc'Range) or else
                      Doc (Attr).Kind /= Kind_Attribute or else
                      Overflow (Attr, Doc (Attr).Value);

            Put (" ", Data, Position);
            Serialize_Data (Doc, Attr, Data, Position);

            Put ("=""", Data, Position);
            Value := Add (Attr, Doc (Attr).Value);
            exit when not (Value in Doc'Range);

            Serialize_Data (Doc, Value, Data, Position);
            Put ("""", Data, Position);
            Pos := Doc (Attr).Next_Attribute;
         end loop;
         if Doc (Current).Children = Invalid_Relative_Index
         then
            Put ("/", Data, Position);
         end if;
         Put (">", Data, Position);
      end if;
   end Handle;

   ---------------
   -- To_String --
   ---------------

   procedure To_String (Doc    : Subtree_Type;
                        Data   : out String;
                        Last   : out Natural;
                        Result : out Result_Type;
                        Buffer : in out Stack_Type)
   is
      Child    : Relative_Index_Type;
      Element  : Index_Type;
      Tmp      : Traversal_Type;
      Current  : Traversal_Type;
      Position : Integer := 0;

      package S   is new SXML.Stack (Traversal_Type,
                                     Stack_Type,
                                     Buffer (Buffer'First .. Buffer'First + Buffer'Length / 2 - 1),
                                     Null_Traversal);

      package Rev is new SXML.Stack (Traversal_Type,
                                     Stack_Type,
                                     Buffer (Buffer'First + Buffer'Length / 2 .. Buffer'Last),
                                     Null_Traversal);
   begin
      Result := Result_Invalid;
      Last := 0;
      S.Reset;
      Rev.Reset;
      S.Push ((Doc'First, Mode_Open));
      Data := (others => Character'Val (0));

      while not S.Is_Empty
      loop
         pragma Loop_Invariant (Position >= 0);
         pragma Loop_Invariant (S.Is_Valid);
         pragma Loop_Invariant (not S.Is_Empty);
         pragma Loop_Invariant (Rev.Is_Valid);

         S.Pop (Current);
         if not (Current.Index in Doc'Range)
         then
            Result := Result_Overflow;
            return;
         end if;

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
            if not (Element in Doc'Range) or else
              (Doc (Element).Kind /= Kind_Element_Open and
               Doc (Element).Kind /= Kind_Content)
            then
               return;
            end if;

            Child := Doc (Element).Children;
            loop
               exit when
                 Child = Invalid_Relative_Index or
                 Overflow (Element, Child);

               Element := Add (Element, Child);
               if (Rev.Is_Full or
                   not (Element in Doc'Range)) or else
                  (Doc (Element).Kind /= Kind_Element_Open and
                   Doc (Element).Kind /= Kind_Content)
               then
                  return;
               end if;

               pragma Loop_Invariant (Rev.Is_Valid);
               pragma Loop_Invariant (not Rev.Is_Full);
               pragma Loop_Invariant (Element in Doc'Range);
               pragma Loop_Invariant (Doc (Element).Kind = Kind_Element_Open or
                                      Doc (Element).Kind = Kind_Content);

               Rev.Push ((Element, Mode_Open));
               Child := Doc (Element).Siblings;
            end loop;
            while not Rev.Is_Empty
            loop
               pragma Loop_Invariant (S.Is_Valid);
               pragma Loop_Invariant (Rev.Is_Valid);
               pragma Loop_Invariant (not Rev.Is_Empty);
               Rev.Pop (Tmp);
               if S.Is_Full
               then
                  return;
               end if;
               S.Push ((Tmp.Index, Mode_Open));
            end loop;
         end if;

      end loop;

      Result := Result_OK;
      Last := Position;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   procedure To_String (Doc    : Subtree_Type;
                        Data   : out String;
                        Last   : out Natural;
                        Result : out Result_Type)
   is
      Stack_Buffer : Stack_Type (1 .. 1000) := (others => Null_Traversal);
   begin
      To_String (Doc, Data, Last, Result, Stack_Buffer);
      pragma Unreferenced (Stack_Buffer);
   end To_String;

end SXML.Serialize;
