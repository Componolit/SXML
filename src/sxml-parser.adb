package body SXML.Parser is

   Context       : Subtree_Type (1 .. Context_Size) := (others => Null_Node);
   Context_Index : Index_Type := Context'First;
   Context_Valid : Boolean    := False;
   Offset        : Natural    := 0;

   type Range_Type is
   record
      First : Natural;
      Last  : Natural;
   end record;
   Null_Range : constant Range_Type := (Natural'Last, 0);

   Whitespace : constant String :=
      Character'Val (16#20#) &
      Character'Val (16#9#)  &
      Character'Val (16#D#)  &
      Character'Val (16#A#);

   function Document_Valid return Boolean is (Context_Valid);

   ---------------
   -- Match_Set --
   ---------------

   function Match_Set (S : String) return Boolean
   with
      Pre    => Data'First >= 0 and
                Data'Last < Natural'Last,
      Post   => (if Match_Set'Result then
                    (for some E of S => E = Data (Data'First + Offset)));

   function Match_Set (S : String) return Boolean
   is
   begin
      for Value of S
      loop
         if Offset < Data'Length and then
            Data (Data'First + Offset) = Value
         then
            return True;
         end if;
      end loop;
      return False;
   end Match_Set;

   -------------------
   -- Data_Overflow --
   -------------------

   function Data_Overflow return Boolean is
      (Data'First > Integer'Last - Offset or
       Offset > Data'Length - 1);

   ------------------
   -- Match_String --
   ------------------

   procedure Match_String (Text  : String;
                           Match : out Match_Type);

   procedure Match_String (Text  : String;
                           Match : out Match_Type)
   is
      Old_Offset : constant Natural := Offset;
   begin
      for I in 1 .. Text'Length
      loop
         if Data_Overflow
         then
            Match  := Match_Invalid;
            Offset := Old_Offset;
            return;
         end if;

         if Data (Data'First + Offset) /= Text (I)
         then
            Match  := Match_None;
            Offset := Old_Offset;
            return;
         end if;

         Offset := Offset + 1;
      end loop;

      Match := Match_OK;
   end Match_String;

   ----------------------
   -- Context_Overflow --
   ----------------------

   function Context_Overflow return Boolean is
      (Context_Index >= Context'Last);

   -----------------
   -- Match_Until --
   -----------------

   procedure Match_Until (End_Set : String;
                          Text    : out Range_Type);

   procedure Match_Until (End_Set : String;
                          Text    : out Range_Type)
   is
      Old_Offset : constant Natural := Offset;
      First      : constant Natural := Data'First + Offset;
   begin
      Text := Null_Range;

      loop
         if Data_Overflow
         then
            Offset := Old_Offset;
            return;
         end if;

         exit when Match_Set (End_Set);
         Offset := Offset + 1;
      end loop;

      Text := (First, Data'First + Offset - 1);

   end Match_Until;

   ----------
   -- Skip --
   ----------

   procedure Skip (Skip_Set : String);

   procedure Skip (Skip_Set : String)
   is
   begin
      loop
         if Data_Overflow
         then
            return;
         end if;

         exit when not Match_Set (Skip_Set);
         Offset := Offset + 1;
      end loop;
   end Skip;

   -----------------------
   -- Parse_Opening_Tag --
   -----------------------

   procedure Parse_Opening_Tag (Match : out Match_Type;
                                Name  : out Range_Type;
                                Done  : out Boolean);

   procedure Parse_Opening_Tag (Match : out Match_Type;
                                Name  : out Range_Type;
                                Done  : out Boolean)
   is
      Old_Offset : constant Natural := Offset;
   begin
      Match := Match_Invalid;
      Done := False;

      if Context_Overflow
      then
         Match := Match_Out_Of_Memory;
         return;
      end if;

      --  Match opening '<'
      if not Match_Set ("<")
      then
         return;
      end if;

      Offset := Offset + 1;
      if Data_Overflow
      then
         Offset := Old_Offset;
         return;
      end if;

      --  Match tag name
      Match_Until (Whitespace & ">/", Name);
      if Name = Null_Range
      then
         Offset := Old_Offset;
         return;
      end if;

      Skip (Whitespace);

      if Data_Overflow
      then
         return;
      end if;

      --  Short form node?
      Match_String ("/>", Match);
      if Match = Match_OK
      then
         Done := True;
      else
         --  Match closing '>'
         if not Match_Set (">")
         then
            Offset := Old_Offset;
            return;
         end if;
         Offset := Offset + 1;
      end if;

      --  FIXME: Add range type for string to avoid copying
      Context (Context_Index) :=
        (Kind => Kind_Element_Open,
         Name => To_Name (Data (Name.First .. Name.Last)));
      Context_Index := Context_Index + 1;
      Match := Match_OK;
   end Parse_Opening_Tag;

   -----------------------
   -- Parse_Closing_Tag --
   -----------------------

   procedure Parse_Closing_Tag (Name  : String;
                                Match : out Match_Type);

   procedure Parse_Closing_Tag (Name  : String;
                                Match : out Match_Type)
   is
      Old_Offset   : constant Natural := Offset;
      Closing_Name : Range_Type;
   begin
      Match := Match_Invalid;

      if Context_Overflow
      then
         Match := Match_Out_Of_Memory;
         return;
      end if;

      Match_String ("</", Match);
      if Match /= Match_OK
      then
         return;
      end if;

      Match_Until (Whitespace & ">", Closing_Name);
      if Closing_Name = Null_Range or Data_Overflow
      then
         Offset := Old_Offset;
         return;
      end if;

      Skip (Whitespace);

      --  Match closing tag
      if not Match_Set (">")
      then
         Offset := Old_Offset;
         return;
      end if;
      Offset := Offset + 1;

      if Data (Closing_Name.First .. Closing_Name.Last) /= Name
      then
         Match := Match_None_Wellformed;
         return;
      end if;

      Context (Context_Index) :=
        (Kind => Kind_Element_Close,
         Name => To_Name (Name));
      Context_Index := Context_Index + 1;
      Match := Match_OK;

   end Parse_Closing_Tag;

   --------------------
   -- Parse_Internal --
   --------------------

   procedure Parse_Internal (Match : out Match_Type);

   procedure Parse_Internal (Match : out Match_Type)
   is
      Done       : Boolean;
      Old_Offset : constant Natural := Offset;
      Name       : Range_Type;
   begin

      Parse_Opening_Tag (Match, Name, Done);
      if Match /= Match_OK
      then
         Offset := Old_Offset;
         Match  := Match_None;
         return;
      end if;

      if Context_Overflow
      then
         Match := Match_Out_Of_Memory;
         return;
      end if;

      if Done
      then
         Context (Context_Index) :=
           (Kind => Kind_Element_Close,
            Name => To_Name (Data (Name.First .. Name.Last)));
         Context_Index := Context_Index + 1;
         Match := Match_OK;
         return;
      end if;

      loop
         Parse_Internal (Match);
         exit when Match = Match_None;
      end loop;

      --  FIXME: Match closing tag with opening tag
      Parse_Closing_Tag (Data (Name.First .. Name.Last), Match);

   end Parse_Internal;

   -----------
   -- Parse --
   -----------

   procedure Parse (Match : out Match_Type)
   is
   begin
      Context_Valid := False;
      Parse_Internal (Match);
      if Match = Match_OK
      then
         Context_Valid := True;
      end if;
   end Parse;

   --------------
   -- Document --
   --------------

   function Document return Subtree_Type is (Context);

end SXML.Parser;
