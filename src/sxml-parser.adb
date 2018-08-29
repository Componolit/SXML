package body SXML.Parser is

   Context_Index : Index_Type := Context'First;
   Context_Valid : Boolean    := False;
   Offset        : Natural    := 0;
   Error_Index   : Natural    := 0;

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

   --------------------
   -- Restore_Offset --
   --------------------

   procedure Restore_Offset (Old_Offset : Natural)
   with
      Post => Offset = Old_Offset;

   procedure Restore_Offset (Old_Offset : Natural)
   is
   begin
      if Offset > Error_Index
      then
         Error_Index := Offset;
      end if;
      Offset := Old_Offset;
   end Restore_Offset;

   ---------------------
   -- Restore_Context --
   ---------------------

   procedure Restore_Context (Old_Context : Index_Type)
   with
      Post => Context_Index = Old_Context;

   procedure Restore_Context (Old_Context : Index_Type)
   is
   begin
      Context_Index := Old_Context;
   end Restore_Context;

   --------------
   -- In_Range --
   --------------

   function In_Range (R : Range_Type) return Boolean
   is (R.First >= Data'First and R.Last <= Data'Last and R.First <= R.Last);

   ------------
   -- Length --
   ------------

   --  FIXME: Remove
   --  function Length (R : Range_Type) return Natural
   --  is (if R.Last > R.First then R.Last - R.First - 1 else 0);

   -------------------
   -- Data_Overflow --
   -------------------

   function Data_Overflow return Boolean is
      (Data'First > Natural'Last - Offset or
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
         pragma Loop_Invariant (Result = Match_Invalid);
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
            Restore_Offset (Old_Offset);
            return;
         end if;

         if Data (Data'First + Offset) /= C
         then
            Match  := Match_None;
            Restore_Offset (Old_Offset);
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
   -- Context_Put --
   -----------------

   procedure Context_Put (Value  : Subtree_Type;
                          Result : out Boolean);

   procedure Context_Put (Value  : Subtree_Type;
                          Result : out Boolean)
   is
   begin
      Result := False;
      if Context_Index > Context'Last - Value'Length
      then
         return;
      end if;

      for V of Value
      loop
         Context (Context_Index) := V;
         Context_Index := Context_Index + 1;
      end loop;
      Result := True;
   end Context_Put;

   ----------------------
   -- Match_Until_Text --
   ----------------------

   procedure Match_Until_String (End_String : String;
                                 Text       : out Range_Type)
   with
      Pre => not Data_Overflow;

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
            Restore_Offset (Old_Offset);
            return;
         end if;

         pragma Loop_Invariant (Data'First <= Natural'Last - Offset);
         pragma Loop_Invariant (Len < Natural'Last);

         Match_String (End_String, Result);
         exit when Result = Match_OK;
         Offset := Offset + 1;
         Len := Len + 1;
      end loop;

      if Len = 0 or
        (Data'First > Natural'Last - Offset or else
         Data'First + Offset - End_String'Length <= 0)
      then
         Restore_Offset (Old_Offset);
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
            Restore_Offset (Old_Offset);
            return;
         end if;

         Match_Set (End_Set, Result);
         exit when Result = Match_OK;
         Len := Len + 1;
      end loop;

      if Len = 0 or
         (Data'First > Data'Last - Offset + 1 or else
          First > Data'First + Offset - 2)
      then
         Restore_Offset (Old_Offset);
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
      Old_Offset    : constant Natural := Offset;
      Attr_Name     : Range_Type;
      Attr_Value    : Range_Type;
      Match_Tmp     : Match_Type;
      Valid         : Boolean;
   begin

      if Context_Overflow
      then
         Match  := Match_Out_Of_Memory;
         return;
      end if;

      Match := Match_Invalid;

      Skip (Whitespace);
      if Data_Overflow
      then
         Restore_Offset (Old_Offset);
         return;
      end if;

      Match_Until_Set (Whitespace & "=", Attr_Name);
      if Attr_Name = Null_Range
      then
         Restore_Offset (Old_Offset);
         return;
      end if;

      Skip (Whitespace);
      if Data_Overflow
      then
         Restore_Offset (Old_Offset);
         return;
      end if;

      Match_Set ("=", Match_Tmp);
      if Match_Tmp /= Match_OK
      then
         Restore_Offset (Old_Offset);
         return;
      end if;

      Skip (Whitespace);
      if Data_Overflow
      then
         Restore_Offset (Old_Offset);
         return;
      end if;

      Match_Set ("""", Match_Tmp);
      if Match_Tmp /= Match_OK or
         Data_Overflow
      then
         Restore_Offset (Old_Offset);
         return;
      end if;

      Match_Until_Set ("""", Attr_Value);
      if Attr_Value = Null_Range or
         Data_Overflow
      then
         Restore_Offset (Old_Offset);
         return;
      end if;
      Offset := Offset + 1;

      Context_Put (Value  => A (Name  => Data (Attr_Name.First .. Attr_Name.Last),
                                Value =>  Data (Attr_Value.First .. Attr_Value.Last)),
                   Result => Valid);
      if not Valid
      then
         Restore_Offset (Old_Offset);
         return;
      end if;

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
      Valid       : Boolean;
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
      if Data_Overflow
      then
         Restore_Offset (Old_Offset);
         return;
      end if;

      --  Match opening '<'
      Match_Set ("<", Match_Tmp);
      if Match_Tmp /= Match_OK or
         Data_Overflow
      then
         Restore_Offset (Old_Offset);
         return;
      end if;

      --  Match tag name
      Match_Until_Set (Whitespace & ">/", Name);
      if Name = Null_Range
      then
         Restore_Offset (Old_Offset);
         return;
      end if;

      Context_Put (Value  => Open (Data (Name.First .. Name.Last)),
                   Result => Valid);
      if not Valid
      then
         Restore_Offset (Old_Offset);
         return;
      end if;

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
         if Data_Overflow
         then
            Restore_Offset (Old_Offset);
            Restore_Context (Old_Index);
            return;
         end if;

         --  Match closing '>'
         Match_Set (">", Match_Tmp);
         if Match_Tmp /= Match_OK
         then
            Restore_Offset (Old_Offset);
            Restore_Context (Old_Index);
            return;
         end if;
      end if;

      Match := Match_OK;

   end Parse_Opening_Tag;

   -----------------------
   -- Parse_Closing_Tag --
   -----------------------

   procedure Parse_Closing_Tag (Name  : String;
                                Match : out Match_Type)
   with
      Pre  => Data_Valid,
      Post => (if Match /= Match_OK then Offset = Offset'Old);

   procedure Parse_Closing_Tag (Name  : String;
                                Match : out Match_Type)
   is
      Old_Offset   : constant Natural := Offset;
      Closing_Name : Range_Type;
      Match_Tmp    : Match_Type;
      Valid        : Boolean;
   begin
      Match := Match_Invalid;

      if Context_Overflow
      then
         Restore_Offset (Old_Offset);
         Match := Match_Out_Of_Memory;
         return;
      end if;

      if Data_Overflow
      then
         Restore_Offset (Old_Offset);
         return;
      end if;

      Skip (Whitespace);

      Match_String ("</", Match);
      if Match /= Match_OK or
         Data_Overflow
      then
         Restore_Offset (Old_Offset);
         return;
      end if;

      Match_Until_Set (Whitespace & ">", Closing_Name);
      if Closing_Name = Null_Range or
         Data_Overflow
      then
         Restore_Offset (Old_Offset);
         return;
      end if;

      --  Match closing tag
      Skip (Whitespace);

      if Data_Overflow
      then
         Restore_Offset (Old_Offset);
         return;
      end if;

      Match_Set (">", Match_Tmp);
      if Match_Tmp /= Match_OK
      then
         Restore_Offset (Old_Offset);
         return;
      end if;

      if Data (Closing_Name.First .. Closing_Name.Last) /= Name
      then
         Restore_Offset (Old_Offset);
         Match := Match_None_Wellformed;
         return;
      end if;

      Context_Put (Value  => Close (Name => Name),
                   Result => Valid);
      if not Valid
      then
         Restore_Offset (Old_Offset);
         return;
      end if;

      Match := Match_OK;

   end Parse_Closing_Tag;

   -------------------
   -- Parse_Content --
   -------------------

   procedure Parse_Content
   with
      Pre => Data_Valid;

   procedure Parse_Content
   is
      Content : Range_Type;
   begin
      if Data_Overflow
      then
         return;
      end if;

      Match_Until_Set ("<", Content);
      pragma Unreferenced (Content);
   end Parse_Content;

   -------------------
   -- Parse_Comment --
   -------------------

   procedure Parse_Comment
   with
      Pre => Data_Valid;

   procedure Parse_Comment
   is
      Old_Offset   : Natural;
      Result       : Match_Type;
      Comment_Text : Range_Type;
   begin
      loop
         if Data_Overflow
         then
            return;
         end if;

         Skip (Whitespace);
         Old_Offset := Offset;
         Match_String ("<!--", Result);
         if Result /= Match_OK or
            Data_Overflow
         then
            return;
         end if;

         Match_Until_String ("-->", Comment_Text);
         if Comment_Text = Null_Range
         then
            Restore_Offset (Old_Offset);
            return;
         end if;
      end loop;
   end Parse_Comment;

   ----------------------------------
   -- Parse_Processing_Information --
   ----------------------------------

   procedure Parse_Processing_Information
   with
      Pre => Data_Valid;

   procedure Parse_Processing_Information
   is
      Old_Offset : Natural;
      Result     : Match_Type;
      PI_Text    : Range_Type;
   begin
      loop
         if Data_Overflow
         then
            return;
         end if;

         Skip (Whitespace);
         Old_Offset := Offset;
         Match_String ("<?", Result);
         if Result /= Match_OK or
            Data_Overflow
         then
            return;
         end if;

         Match_Until_String ("?>", PI_Text);
         if PI_Text = Null_Range
         then
            Restore_Offset (Old_Offset);
            return;
         end if;
      end loop;
   end Parse_Processing_Information;

   -------------------
   -- Parse_Doctype --
   -------------------

   procedure Parse_Doctype
   with
      Pre => Data_Valid;

   procedure Parse_Doctype
   is
      Old_Offset   : Natural;
      Result       : Match_Type;
      Doctype_Text : Range_Type;
   begin
      loop
         if Data_Overflow
         then
            return;
         end if;

         Skip (Whitespace);
         Old_Offset := Offset;
         Match_String ("<!DOCTYPE", Result);
         if Result /= Match_OK or
            Data_Overflow
         then
            return;
         end if;

         Match_Until_String (">", Doctype_Text);
         if Doctype_Text = Null_Range
         then
            Restore_Offset (Old_Offset);
            return;
         end if;
      end loop;
   end Parse_Doctype;

   --------------------
   -- Parse_Internal --
   --------------------

   procedure Parse_Internal (Match : out Match_Type;
                             Level : Natural := 0)
   with
      Pre  => Data_Valid,
      Post => (if Match /= Match_OK then Offset = Offset'Old);

   procedure Parse_Internal (Match : out Match_Type;
                             Level : Natural := 0)
   is
      Old_Offset : constant Natural := Offset;
      Done       : Boolean;
      Name       : Range_Type;
      Sub_Match  : Match_Type;
      Valid      : Boolean;
   begin

      Match := Match_Invalid;

      Parse_Comment;
      Parse_Processing_Information;
      Parse_Doctype;

      if Data_Overflow
      then
         Restore_Offset (Old_Offset);
         return;
      end if;

      Parse_Opening_Tag (Match, Name, Done);
      if Match /= Match_OK
      then
         Restore_Offset (Old_Offset);
         Match := Match_None;
         return;
      end if;

      if Context_Overflow
      then
         Restore_Offset (Old_Offset);
         Match := Match_Out_Of_Memory;
         return;
      end if;

      if Done
      then
         Parse_Comment;
         Parse_Processing_Information;
         Parse_Doctype;
         Context_Put (Value  => Close (Data (Name.First .. Name.Last)),
                      Result => Valid);
         if not Valid
         then
            Restore_Offset (Old_Offset);
            return;
         end if;

         Match := Match_OK;
         return;
      end if;

      loop
         Parse_Content;

         if Level > 100
         then
            return;
         end if;

         Parse_Internal (Sub_Match, Level + 1);
         exit when Sub_Match /= Match_OK;
      end loop;

      Parse_Closing_Tag (Data (Name.First .. Name.Last), Match);

      if Match /= Match_OK
      then
         Restore_Offset (Old_Offset);
         return;
      end if;

      Parse_Comment;
      Parse_Processing_Information;
      Parse_Doctype;

   end Parse_Internal;

   -----------
   -- Parse --
   -----------

   procedure Parse (Match    : out Match_Type;
                    Position : out Natural)
   is
   begin
      Context_Valid := False;
      Parse_Internal (Match);
      if Match = Match_OK
      then
         Context_Valid := True;
         Position      := 0;
      else
         Position := Error_Index;
      end if;

      if Context_Index in Context'Range
      then
         Context (Context_Index) := Null_Node;
      end if;
   end Parse;

   --------------
   -- Document --
   --------------

   function Document return Subtree_Type is (Context (Context'First .. Context_Index));

end SXML.Parser;
