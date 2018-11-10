with SXML.Stack;

package body SXML.Serialize
is
   pragma Annotate (GNATprove, Terminating, SXML.Serialize);

   ---------
   -- Put --
   ---------

   procedure Put (Value    : String;
                  Data     : in out String;
                  Position : in out Integer)
   with
      Pre  => Value'Length > 0,
      Post => (if Position'Old < 0 then Position < 0) and
              (if Position >= 0 then Position > Position'Old);

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

   procedure Put_Escaped (Doc      : Document_Type;
                          Start    : Index_Type;
                          Data     : in out String;
                          Position : in out Integer)
   with
      Pre  => Start in Doc'Range,
      Post => (if Position'Old < 0 then Position < 0) and
              (if Position >= 0 and Doc (Start).Length > 0
               then Position > Position'Old);

   procedure Put_Escaped (Doc      : Document_Type;
                          Start    : Index_Type;
                          Data     : in out String;
                          Position : in out Integer)
   is
      procedure Put_Escaped_Char (Char : Character;
                                  D    : in out String;
                                  P    : in out Integer)
      with
         Post => (if P'Old < 0 then P < 0) and
                 (if P >= 0 then P > P'Old);

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
         N := Doc (Pos);

         for C of N.Data (1 .. Natural (N.Length))
         loop
            Put_Escaped_Char (C, Data, Position);
            if Position < 0
            then
               return;
            end if;
            pragma Loop_Invariant (Position > Position'Loop_Entry);
         end loop;

         exit when
           N.Length = 0 or
           N.Next = Invalid_Relative_Index or
           Overflow (Pos, N.Next);

         Pos := Add (Pos, N.Next);
         exit when not (Pos in Doc'Range);

         pragma Loop_Invariant (Pos in Doc'Range);
         pragma Loop_Invariant (Position > Position'Loop_Entry);
         pragma Loop_Variant (Increases => Pos);
      end loop;
   end Put_Escaped;

   --------------------
   -- Serialize_Data --
   --------------------

   procedure Serialize_Data (Doc      : Document_Type;
                             Start    : Index_Type;
                             Data     : in out String;
                             Position : in out Integer)
   with
      Pre  => Start in Doc'Range,
      Post => (if Position'Old < 0 then Position < 0) and
              (if Position >= 0 then
                 (if Doc (Start).Length > 0
                  then Position > Position'Old
                  else Position = Position'Old));

   procedure Serialize_Data (Doc      : Document_Type;
                             Start    : Index_Type;
                             Data     : in out String;
                             Position : in out Integer)
   is
      N   : Node_Type;
      Pos : Index_Type := Start;
   begin

      loop
         N := Doc (Pos);
         if N.Length = 0
         then
            return;
         end if;

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

         pragma Loop_Variant (Increases => Pos);
         pragma Loop_Invariant (Position > Position'Loop_Entry);
         pragma Loop_Invariant (Pos in Doc'Range);
      end loop;
   end Serialize_Data;

   -------------------
   -- Valid_Element --
   -------------------

   function Valid_Element (Doc      : Document_Type;
                           Current  : Index_Type;
                           Mode     : Mode_Type) return Boolean
   is ((Mode = Mode_Open and Doc (Current).Kind = Kind_Content) or
       ((Doc (Current).Kind = Kind_Element_Open or Doc (Current).Kind = Kind_Content) and then
       ((Mode = Mode_Close and Doc (Current).Children /= Invalid_Relative_Index) or
       Mode = Mode_Open)))
   with
      Pre => Current in Doc'Range;

   -----------------------
   -- Serialize_Element --
   -----------------------

   procedure Serialize_Element (Doc      : Document_Type;
                                Current  : Index_Type;
                                Mode     : Mode_Type;
                                Data     : in out String;
                                Position : in out Integer)
   with
      Pre  => Data'Length > 0 and
              (Current in Doc'Range and then
               Valid_Element (Doc, Current, Mode)),
      Post => (if Position'Old < 0 then Position < 0) and
              (if Position >= 0  and Doc (Current).Length > 0
               then Position > Position'Old),
      Annotate => (GNATprove, Terminating);

   procedure Serialize_Element (Doc      : Document_Type;
                                Current  : Index_Type;
                                Mode     : Mode_Type;
                                Data     : in out String;
                                Position : in out Integer)
   is
      Attr  : Index_Type;
      Value : Index_Type;
      Pos   : Relative_Index_Type;
      Position_Old : constant Integer := Position with Ghost;
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
      elsif Mode = Mode_Open
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

            pragma Loop_Variant (Increases => Attr);
            pragma Loop_Invariant (if Position'Loop_Entry < 0 then Position < 0);
            pragma Loop_Invariant ((if Position >= 0 then
                                   (if Doc (Current).Length > 0
                                    then Position > Position_Old
                                    else Position >= Position_Old)));
         end loop;

         if Doc (Current).Children = Invalid_Relative_Index
         then
            Put ("/", Data, Position);
         end if;
         Put (">", Data, Position);
      end if;
   end Serialize_Element;

   ---------------
   -- To_String --
   ---------------

   procedure To_String (Doc    : Document_Type;
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
      Count    : Natural := 0;

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
         pragma Loop_Variant (Increases => Count);
         pragma Loop_Invariant (Position >= 0);
         pragma Loop_Invariant (S.Is_Valid);
         pragma Loop_Invariant (not S.Is_Empty);
         pragma Loop_Invariant (Rev.Is_Valid);

         S.Pop (Current);

         if Count = Natural'Last or
            not (Current.Index in Doc'Range)
         then
            Result := Result_Overflow;
            return;
         end if;
         Count := Count + 1;

         if Valid_Element (Doc, Current.Index, Current.Mode)
         then
            Serialize_Element (Doc, Current.Index, Current.Mode, Data, Position);
            if Position < 0
            then
               return;
            end if;
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

               pragma Loop_Variant (Increases => Element);
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
               pragma Loop_Variant (Decreases => Rev.Level);
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

   procedure To_String (Doc    : Document_Type;
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
