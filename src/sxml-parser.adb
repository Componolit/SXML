package body SXML.Parser is

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

   --------------
   -- In_Range --
   --------------

   function In_Range (R : Range_Type) return Boolean
   is (R.First >= Data'First and R.Last <= Data'Last and R.First <= R.Last);

   ------------
   -- Length --
   ------------

   function Length (R : Range_Type) return Natural
   is (if R.Last > R.First then R.Last - R.First - 1 else 0);

   -------------------
   -- Is_Valid_Name --
   -------------------

   function Is_Valid_Name (R : Range_Type) return Boolean
   is (Length (R) <= Name_Type'Length);

   -------------------
   -- Data_Overflow --
   -------------------

   function Data_Overflow return Boolean is
      (Data'First > Integer'Last - Offset or
       Offset > Data'Length - 1);

   ---------------
   -- Match_Set --
   ---------------

   procedure Match_Set (S      : String;
                        Result : out Match_Type)
   with
      Pre    => Data_Valid and
                Offset < Natural'Last,
       Post   => (if Result = Match_OK then
                    (for some E of S => E = Data (Data'First + Offset - 1)) and
                    Offset > Offset'Old);

   procedure Match_Set (S      : String;
                        Result : out Match_Type)
   is
   begin
      Result := Match_Invalid;

      for Value of S
      loop
         pragma Loop_Invariant (Result = Match_Invalid and Offset = Offset'Loop_Entry);
         if Data_Overflow
         then
            return;
         end if;

         if Data (Data'First + Offset) = Value
         then
            Offset := Offset + 1;
            Result := Match_OK;
            return;
         end if;
      end loop;
      Offset := Offset + 1;
   end Match_Set;

   ------------------
   -- Match_String --
   ------------------

   procedure Match_String (Text  : String;
                           Match : out Match_Type)
   with
      Post => (if Match /= Match_OK then Offset = Offset'Old);

   procedure Match_String (Text  : String;
                           Match : out Match_Type)
   is
      Old_Offset : constant Natural := Offset;
   begin
      for C of Text
      loop
         if Data_Overflow
         then
            Match  := Match_Invalid;
            Offset := Old_Offset;
            return;
         end if;

         if Data (Data'First + Offset) /= C
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

   ----------------------
   -- Match_Until_Text --
   ----------------------

   procedure Match_Until_String (End_String : String;
                                 Text       : out Range_Type);

   procedure Match_Until_String (End_String : String;
                                 Text       : out Range_Type)
   is
      Old_Offset : constant Natural := Offset;
      First      : constant Natural := Data'First + Offset;
      Result     : Match_Type;
      Len        : Natural := 0;
   begin
      Text := Null_Range;

      loop
         if Data_Overflow or
            Len >= Natural'Last
         then
            Offset := Old_Offset;
            return;
         end if;

         Match_String (End_String, Result);
         exit when Result = Match_OK;
         Offset := Offset + 1;
         Len := Len + 1;
      end loop;

      if Len = 0
      then
         Offset := Old_Offset;
         return;
      end if;

      Text := (First, Data'First + Offset - End_String'Length - 1);

   end Match_Until_String;

   ---------------------
   -- Match_Until_Set --
   ---------------------

   procedure Match_Until_Set (End_Set : String;
                              Text    : out Range_Type)
   with
      Pre  => Data_Valid and then
              Data'First <= Data'Last - Offset,
      Post => (if Text = Null_Range
               then Offset = Offset'Old
               else In_Range (Text));

   procedure Match_Until_Set (End_Set : String;
                              Text    : out Range_Type)
   is
      Old_Offset : constant Natural := Offset;
      First      : constant Natural := Data'First + Offset;
      Result     : Match_Type;
      Len        : Natural := 0;
   begin
      Text := Null_Range;

      loop
         if Data_Overflow or
            Len >= Natural'Last
         then
            Offset := Old_Offset;
            return;
         end if;

         Match_Set (End_Set, Result);
         exit when Result = Match_OK;
         Len := Len + 1;
      end loop;

      if Len = 0
      then
         Offset := Old_Offset;
         return;
      end if;

      Offset := Offset - 1;
      Text := (First, Data'First + Offset - 1);

   end Match_Until_Set;

   ----------
   -- Skip --
   ----------

   procedure Skip (Skip_Set : String)
   with
      Pre => Data_Valid;

   procedure Skip (Skip_Set : String)
   is
      Last_None_Whitespace : Natural;
      Result : Match_Type;
   begin
      loop
         if Data_Overflow
         then
            return;
         end if;

         Last_None_Whitespace := Offset;
         Match_Set (Skip_Set, Result);
         exit when Result /= Match_OK;
      end loop;
      Offset := Last_None_Whitespace;
   end Skip;

   ---------------------
   -- Parse_Attribute --
   ---------------------

   procedure Parse_Attribute (Match : out Match_Type)
   with
      Pre  => Data_Valid,
      Post => (if Match /= Match_OK then Offset = Offset'Old);

   procedure Parse_Attribute (Match : out Match_Type)
   is
      Old_Offset : constant Natural := Offset;
      Attr_Name  : Range_Type;
      Attr_Value : Range_Type;
      Match_Tmp  : Match_Type;
   begin

      if Context_Overflow
      then
         Match  := Match_Out_Of_Memory;
         return;
      end if;

      Match := Match_Invalid;

      Skip (Whitespace);
      Match_Until_Set (Whitespace & "=", Attr_Name);
      if Attr_Name = Null_Range or else
         not Is_Valid_Name (Attr_Name)
      then
         Offset := Old_Offset;
         return;
      end if;

      Skip (Whitespace);
      Match_Set ("=", Match_Tmp);
      if Match_Tmp /= Match_OK
      then
         Offset := Old_Offset;
         return;
      end if;

      Skip (Whitespace);
      Match_Set ("""", Match_Tmp);
      if Match_Tmp /= Match_OK
      then
         Offset := Old_Offset;
         return;
      end if;

      Match_Until_Set ("""", Attr_Value);
      if Attr_Value = Null_Range or else
         not Is_Valid_Name (Attr_Value)
      then
         Offset := Old_Offset;
         return;
      end if;
      Offset := Offset + 1;

      --  FIXME: Add range type for string to avoid copying
      Context (Context_Index) :=
        (Kind  => Kind_Attr,
         Name  => To_Name (Data (Attr_Name.First .. Attr_Name.Last)),
         Value => To_Name (Data (Attr_Value.First .. Attr_Value.Last)));
      Context_Index := Context_Index + 1;
      Match := Match_OK;

   end Parse_Attribute;

   -----------------------
   -- Parse_Opening_Tag --
   -----------------------

   procedure Parse_Opening_Tag (Match : out Match_Type;
                                Name  : out Range_Type;
                                Done  : out Boolean)
   with
      Pre  => Data_Valid,
      Post => (if Match /= Match_OK
               then Offset = Offset'Old
               else In_Range (Name));

   procedure Parse_Opening_Tag (Match : out Match_Type;
                                Name  : out Range_Type;
                                Done  : out Boolean)
   is
      Old_Index   : constant Index_Type := Context_Index;
      Old_Offset  : constant Natural := Offset;
      Match_Attr  : Match_Type;
      Match_Tmp   : Match_Type;
   begin
      Name  := Null_Range;
      Match := Match_Invalid;
      Done := False;

      if Context_Overflow
      then
         Match := Match_Out_Of_Memory;
         return;
      end if;

      Skip (Whitespace);

      --  Match opening '<'
      Match_Set ("<", Match_Tmp);
      if Match_Tmp /= Match_OK
      then
         Offset := Old_Offset;
         return;
      end if;

      --  Match tag name
      Match_Until_Set (Whitespace & ">/", Name);
      if Name = Null_Range
      then
         Offset := Old_Offset;
         return;
      end if;

      --  Parse_Attribute may add attributes to the context. Leave space for
      --  the opening tag we add below.
      Context_Index := Context_Index + 1;

      loop
         Parse_Attribute (Match_Attr);
         exit when Match_Attr /= Match_OK;
      end loop;

      --  Short form node?
      Skip (Whitespace);
      Match_String ("/>", Match_Tmp);
      if Match_Tmp = Match_OK
      then
         Done := True;
      else
         --  Match closing '>'
         Match_Set (">", Match_Tmp);
         if Match_Tmp /= Match_OK
         then
            Context_Index := Old_Index;
            Offset := Old_Offset;
            return;
         end if;
      end if;

      --  FIXME: Add range type for string to avoid copying
      Context (Old_Index) :=
        (Kind => Kind_Element_Open,
         Name => To_Name (Data (Name.First .. Name.Last)));
      Match := Match_OK;

   end Parse_Opening_Tag;

   -----------------------
   -- Parse_Closing_Tag --
   -----------------------

   procedure Parse_Closing_Tag (Name  : String;
                                Match : out Match_Type)
   with
      Post => (if Match /= Match_OK then Offset = Offset'Old);

   procedure Parse_Closing_Tag (Name  : String;
                                Match : out Match_Type)
   is
      Old_Offset   : constant Natural := Offset;
      Closing_Name : Range_Type;
      Match_Tmp    : Match_Type;
   begin
      if Context_Overflow
      then
         Offset := Old_Offset;
         Match := Match_Out_Of_Memory;
         return;
      end if;

      Skip (Whitespace);

      Match_String ("</", Match);
      if Match /= Match_OK
      then
         Offset := Old_Offset;
         return;
      end if;

      Match_Until_Set (Whitespace & ">", Closing_Name);
      if Closing_Name = Null_Range
      then
         Offset := Old_Offset;
         return;
      end if;

      --  Match closing tag
      Skip (Whitespace);
      Match_Set (">", Match_Tmp);
      if Match_Tmp /= Match_OK
      then
         Offset := Old_Offset;
         return;
      end if;

      if Data (Closing_Name.First .. Closing_Name.Last) /= Name
      then
         Offset := Old_Offset;
         Match := Match_None_Wellformed;
         return;
      end if;

      Context (Context_Index) :=
        (Kind => Kind_Element_Close,
         Name => To_Name (Name));
      Context_Index := Context_Index + 1;
      Match := Match_OK;

   end Parse_Closing_Tag;

   -------------------
   -- Parse_Content --
   -------------------

   procedure Parse_Content;

   procedure Parse_Content
   is
      Content : Range_Type;
   begin
      Match_Until_Set ("<", Content);
      pragma Unreferenced (Content);
   end Parse_Content;

   -------------------
   -- Parse_Comment --
   -------------------

   procedure Parse_Comment;

   procedure Parse_Comment
   is
      Old_Offset   : constant Natural := Offset;
      Result       : Match_Type;
      Comment_Text : Range_Type;
   begin
      Skip (Whitespace);
      Match_String ("<!--", Result);
      if Result = Match_OK
      then
         Match_Until_String ("-->", Comment_Text);
         if Comment_Text = Null_Range
         then
            Offset := Old_Offset;
            return;
         end if;
      end if;
   end Parse_Comment;

   ----------------------------------
   -- Parse_Processing_Information --
   ----------------------------------

   procedure Parse_Processing_Information;

   procedure Parse_Processing_Information
   is
      Old_Offset : Natural;
      Result     : Match_Type;
      PI_Text    : Range_Type;
   begin
      loop
         Skip (Whitespace);
         Old_Offset := Offset;
         Match_String ("<?", Result);
         if Result /= Match_OK
         then
            return;
         end if;

         Match_Until_String ("?>", PI_Text);
         if PI_Text = Null_Range
         then
            Offset := Old_Offset;
            return;
         end if;
      end loop;
   end Parse_Processing_Information;

   --------------------
   -- Parse_Internal --
   --------------------

   procedure Parse_Internal (Match : out Match_Type;
                             Level : Natural := 0)
   with
      Post => (if Match /= Match_OK then Offset = Offset'Old);

   procedure Parse_Internal (Match : out Match_Type;
                             Level : Natural := 0)
   is
      Old_Offset : constant Natural := Offset;
      Done       : Boolean;
      Name       : Range_Type;
      Sub_Match  : Match_Type;
   begin

      Parse_Comment;
      Parse_Processing_Information;

      Parse_Opening_Tag (Match, Name, Done);
      if Match /= Match_OK
      then
         Offset := Old_Offset;
         Match  := Match_None;
         return;
      end if;

      if Context_Overflow
      then
         Offset := Old_Offset;
         Match := Match_Out_Of_Memory;
         return;
      end if;

      if Done
      then
         Parse_Comment;
         Parse_Processing_Information;
         Context (Context_Index) :=
           (Kind => Kind_Element_Close,
            Name => To_Name (Data (Name.First .. Name.Last)));
         Context_Index := Context_Index + 1;
         Match := Match_OK;
         return;
      end if;

      loop
         Parse_Content;
         Parse_Internal (Sub_Match, Level + 1);
         exit when Sub_Match /= Match_OK;
      end loop;

      Parse_Closing_Tag (Data (Name.First .. Name.Last), Match);
      Parse_Comment;
      Parse_Processing_Information;

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
      Context (Context_Index) := Null_Node;
   end Parse;

   --------------
   -- Document --
   --------------

   function Document return Subtree_Type is (Context (Context'First .. Context_Index));

end SXML.Parser;
