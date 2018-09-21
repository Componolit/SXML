with SXML.Stack;

package body SXML.Serialize is

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

   --------------------
   -- Serialize_Data --
   --------------------

   procedure Serialize_Data (Doc      : Subtree_Type;
                             Start    : Index_Type;
                             Data     : in out String;
                             Position : in out Integer);

   procedure Serialize_Data (Doc      : Subtree_Type;
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
   end Serialize_Data;

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
         Serialize_Data (Doc, Current, Data, Position);
         Put (">", Data, Position);
      end if;

      if Mode = Mode_Open
      then
         Put ("<", Data, Position);
         Serialize_Data (Doc, Current, Data, Position);
         Pos  := N.Attributes;
         Attr := Current;
         while Pos /= Invalid_Relative_Index
         loop
            Attr  := Add (Attr, Pos);
            Value := Add (Attr, Doc (Attr).Value);
            Put (" ", Data, Position);
            Serialize_Data (Doc, Attr, Data, Position);
            Put ("=""", Data, Position);
            Serialize_Data (Doc, Value, Data, Position);
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

   procedure To_String (Doc    : Subtree_Type;
                        Data   : out String;
                        Last   : out Natural;
                        Buffer : out Stack_Type)
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
               Rev.Push ((Element, Mode_Open));
               Child := Doc (Element).Siblings;
            end loop;
            while not Rev.Is_Empty
            loop
               Rev.Pop (Tmp);
               if S.Is_Full
               then
                  return;
               end if;
               S.Push ((Tmp.Index, Mode_Open));
            end loop;
         end if;

      end loop;

      Last := Position;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   procedure To_String (Doc    : Subtree_Type;
                        Data   : out String;
                        Last   : out Natural)
   is
      Stack_Buffer : Stack_Type (1 .. 1000);
   begin
      To_String (Doc, Data, Last, Stack_Buffer);
   end To_String;

end SXML.Serialize;
