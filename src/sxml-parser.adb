with SXML.Stack;

package body SXML.Parser is

   -----------
   -- Parse --
   -----------

   procedure Parse (Data         : String;
                    Context      : in out Subtree_Type;
                    Parse_Result : out Match_Type;
                    Position     : out Natural)
   is
      Unused        : Index_Type;
      Context_Index : Index_Type := Context'First;
      Offset        : Natural    := 0;
      Error_Index   : Natural    := 0;

      type Range_Type is
         record
            First : Natural;
            Last  : Natural;
         end record;
      Null_Range : constant Range_Type := (Natural'Last, 0);

      function Length (R : Range_Type) return Natural
      is (R.Last - R.First + 1)
        with
          Pre => R /= Null_Range or else R.Last >= R.First;

      type Set_Type is new String;
      Empty_Set : constant Set_Type := "";

      Whitespace : constant Set_Type :=
        Character'Val (16#20#) &
        Character'Val (16#9#)  &
        Character'Val (16#D#)  &
        Character'Val (16#A#);

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

      -------------------
      -- Data_Overflow --
      -------------------

      function Data_Overflow return Boolean is
        (Data'First > Natural'Last - Offset or
           Offset > Data'Length - 1);

      ---------------
      -- Match_Set --
      ---------------

      function Match_Set (Valid   : Set_Type;
                          Invalid : Set_Type;
                          Value   : Character) return Match_Type;

      function Match_Set (Valid   : Set_Type;
                          Invalid : Set_Type;
                          Value   : Character) return Match_Type
      is
      begin
         for I of Invalid
         loop
            if I = Value then
               return Match_Invalid;
            end if;
         end loop;

         for V of Valid
         loop
            if V = Value then
               return Match_OK;
            end if;
         end loop;
         return Match_None;
      end Match_Set;

      ---------------
      -- Match_Set --
      ---------------

      procedure Match_Set (Valid   : Set_Type;
                           Invalid : Set_Type;
                           Match   : out Match_Type;
                           Value   : out Character)
        with
          Pre    => Data_Valid (Data) and
          Offset < Natural'Last,
          Post   => (if Match = Match_OK then
                       (for some E of Valid =>
                              E = Data (Data'First + Offset - 1) and
                            E = Value) and
                           Offset > Offset'Old);

      procedure Match_Set (Valid   : Set_Type;
                           Invalid : Set_Type;
                           Match   : out Match_Type;
                           Value   : out Character)
      is
      begin
         Match := Match_Invalid;
         Value := Character'Val (0);

         if Data_Overflow
         then
            return;
         end if;

         Match  := Match_Set (Valid, Invalid, Data (Data'First + Offset));
         Value  := Data (Data'First + Offset);
         Offset := Offset + 1;
      end Match_Set;

      procedure Match_Set (Valid   : Set_Type;
                           Invalid : Set_Type;
                           Match   : out Match_Type)
        with
          Pre    => Data_Valid (Data) and
          Offset < Natural'Last,
          Post   => (if Match = Match_OK then
                       (for some E of Valid => E = Data (Data'First + Offset - 1)) and
                         Offset > Offset'Old);

      procedure Match_Set (Valid   : Set_Type;
                           Invalid : Set_Type;
                           Match   : out Match_Type)
      is
         Result_Unused : Character;
      begin
         Match_Set (Valid, Invalid, Match, Result_Unused);
         pragma Unreferenced (Result_Unused);
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
                             Start  : out Index_Type;
                             Result : out Boolean);

      procedure Context_Put (Value  : Subtree_Type;
                             Start  : out Index_Type;
                             Result : out Boolean)
      is
      begin
         Result := False;
         Start  := Invalid_Index;

         if Context_Index > Context'Last - Value'Length
         then
            return;
         end if;

         Start := Context_Index;

         for V of Value
         loop
            Context (Context_Index) := V;
            Context_Index := Context_Index + 1;
         end loop;
         Result := True;
      end Context_Put;

      ------------------------
      -- Context_Put_String --
      ------------------------

      procedure Context_Put_String (Value  : String;
                                    Start  : out Index_Type;
                                    Result : out Boolean);

      procedure Context_Put_String (Value  : String;
                                    Start  : out Index_Type;
                                    Result : out Boolean)
      is
         NE : constant Offset_Type := Num_Elements (Value);
      begin
         Result := False;
         Start  := Invalid_Index;

         if Context_Index > Sub (Context'Last, NE)
         then
            return;
         end if;

         Start := Context_Index;
         Put_Content (Context, Sub (Context_Index, Context'First), Value);
         Context_Index := Add (Context_Index, NE);
         Result := True;
      end Context_Put_String;

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
              Len = Natural'Last
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

      procedure Match_Until_Set (End_Set     : Set_Type;
                                 Invalid_Set : Set_Type;
                                 Match       : out Match_Type;
                                 Result      : out Range_Type)
        with
          Pre  => Data_Valid (Data) and then
          Data'First <= Data'Last - Offset,
          Post => (if Match = Match_OK
                     then Offset = Offset'Old
                       else In_Range (Result));

      procedure Match_Until_Set (End_Set     : Set_Type;
                                 Invalid_Set : Set_Type;
                                 Match       : out Match_Type;
                                 Result      : out Range_Type)
      is
         Old_Offset : constant Natural := Offset;
         First      : constant Natural := Data'First + Offset;
         Last       : Natural;
         Tmp_Match  : Match_Type;
         Len        : Natural := 0;
      begin
         Match  := Match_Invalid;
         Result := Null_Range;

         loop
            if Data_Overflow or
              Len = Natural'Last
            then
               Restore_Offset (Old_Offset);
               return;
            end if;

            Match_Set (End_Set, Invalid_Set, Tmp_Match);
            exit when Tmp_Match /= Match_None;
            Len := Len + 1;
         end loop;

         if Tmp_Match = Match_Invalid or
           Data'First > Data'Last - Offset + 1
         then
            Restore_Offset (Old_Offset);
            return;
         end if;

         Offset := Offset - 1;
         Last   := Data'First + Offset - 1;
         Result := (First, Last);
         Match  := (if Len > 0 then Match_OK else Match_None);

      end Match_Until_Set;

      ----------
      -- Skip --
      ----------

      procedure Skip (Skip_Set : Set_Type)
        with
          Pre => Data_Valid (Data);

      procedure Skip (Skip_Set : Set_Type)
      is
         Last_None_Whitespace : Natural;
         Result : Match_Type;
         Unused_Result : Character;
         pragma Unreferenced (Unused_Result);
      begin
         loop
            if Data_Overflow
            then
               return;
            end if;

            Last_None_Whitespace := Offset;
            Match_Set (Skip_Set, Empty_Set, Result, Unused_Result);
            exit when Result /= Match_OK;
         end loop;
         Offset := Last_None_Whitespace;
      end Skip;

      ---------------------
      -- Parse_Attribute --
      ---------------------

      procedure Parse_Attribute (Start : out Index_Type;
                                 Match : out Match_Type)
        with
          Pre  => Data_Valid (Data),
          Post => (if Match /= Match_OK then Offset = Offset'Old);

      procedure Parse_Attribute (Start : out Index_Type;
                                 Match : out Match_Type)
      is
         Old_Offset      : constant Natural := Offset;
         Attribute_Name  : Range_Type;
         Attribute_Value : Range_Type;
         Match_Tmp       : Match_Type;
         Separator       : Character;
         Off             : Offset_Type;
      begin

         Start := Invalid_Index;

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

         Match_Until_Set (Whitespace & "=", ">", Match_Tmp, Attribute_Name);
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

         Match_Set ("=", ">", Match_Tmp);
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

         Match_Set ("""'", ">", Match_Tmp, Separator);
         if Match_Tmp /= Match_OK or
           Data_Overflow
         then
            Restore_Offset (Old_Offset);
            return;
         end if;

         Match_Until_Set ("" & Separator, "<", Match_Tmp, Attribute_Value);
         if (Match_Tmp /= Match_OK and Match_Tmp /= Match_None) or
           Data_Overflow
         then
            Restore_Offset (Old_Offset);
            return;
         end if;
         Offset := Offset + 1;

         Start := Context_Index;
         Off := Sub (Context_Index, Context'First);
         SXML.Attribute
           (Name   => Data (Attribute_Name.First .. Attribute_Name.Last),
            Data   => Data (Attribute_Value.First .. Attribute_Value.Last),
            Offset => Off,
            Output => Context);
         Context_Index := Add (Context'First, Off);

         Match := Match_OK;

      end Parse_Attribute;

      -----------------------
      -- Parse_Opening_Tag --
      -----------------------

      procedure Parse_Opening_Tag (Match : out Match_Type;
                                   Name  : out Range_Type;
                                   Start : out Index_Type;
                                   Done  : out Boolean)
        with
          Pre  => Data_Valid (Data),
          Post => (if Match /= Match_OK
                     then Offset = Offset'Old
                       else In_Range (Name));

      procedure Parse_Opening_Tag (Match : out Match_Type;
                                   Name  : out Range_Type;
                                   Start : out Index_Type;
                                   Done  : out Boolean)
      is
         Old_Index          : constant Index_Type := Context_Index;
         Old_Offset         : constant Natural := Offset;
         Match_Attr         : Match_Type;
         Match_Tmp          : Match_Type;
         Valid              : Boolean;
         Attribute_Start    : Index_Type;
         Previous_Attribute : Index_Type := Invalid_Index;
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
         Match_Set ("<", Empty_Set, Match_Tmp);
         if Match_Tmp /= Match_OK or
           Data_Overflow
         then
            Restore_Offset (Old_Offset);
            return;
         end if;

         --  Match tag name
         Match_Until_Set (Whitespace & ">/", Empty_Set, Match_Tmp, Name);
         if Match_Tmp /= Match_OK
         then
            Restore_Offset (Old_Offset);
            return;
         end if;

         Context_Put (Value  => Open (Data (Name.First .. Name.Last)),
                      Start  => Start,
                      Result => Valid);
         if not Valid
         then
            Restore_Offset (Old_Offset);
            return;
         end if;

         loop
            Parse_Attribute (Attribute_Start, Match_Attr);
            exit when Match_Attr /= Match_OK;
            if Previous_Attribute = Invalid_Index
            then
               Context (Start).Attributes := Sub (Attribute_Start, Start);
            else
               Context (Previous_Attribute).Next_Attribute := Sub (Attribute_Start, Previous_Attribute);
            end if;
            Previous_Attribute := Attribute_Start;
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
            Match_Set (">", Empty_Set, Match_Tmp);
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
          Pre  => Data_Valid (Data),
          Post => (if Match /= Match_OK then Offset = Offset'Old);

      procedure Parse_Closing_Tag (Name  : String;
                                   Match : out Match_Type)
      is
         Old_Offset   : constant Natural := Offset;
         Closing_Name : Range_Type;
         Match_Tmp    : Match_Type;
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

         Match_Until_Set (Whitespace & ">", Empty_Set, Match_Tmp, Closing_Name);
         if Match_Tmp /= Match_OK or
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

         Match_Set (">", Empty_Set, Match_Tmp);
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

         Match := Match_OK;

      end Parse_Closing_Tag;

      -------------------
      -- Parse_Section --
      -------------------

      procedure Parse_Sections (Start_Tag : String;
                                End_Tag   : String;
                                Result    : out Range_Type)
        with
          Pre => Data_Valid (Data);

      procedure Parse_Sections (Start_Tag : String;
                                End_Tag   : String;
                                Result    : out Range_Type)
      is
         Old_Offset   : Natural;
         Tmp_Result   : Match_Type;
      begin
         Result := Null_Range;
         loop
            Old_Offset := Offset;
            if Data_Overflow
            then
               Restore_Offset (Old_Offset);
               return;
            end if;

            Skip (Whitespace);
            Match_String (Start_Tag, Tmp_Result);
            if Tmp_Result /= Match_OK or
              Data_Overflow
            then
               Restore_Offset (Old_Offset);
               return;
            end if;

            Match_Until_String (End_Tag, Result);
            if Result = Null_Range
            then
               Restore_Offset (Old_Offset);
               return;
            end if;
         end loop;
      end Parse_Sections;

      -------------------
      -- Parse_Comment --
      -------------------

      procedure Parse_Comment (Result : out Match_Type)
        with
          Pre => Data_Valid (Data);

      procedure Parse_Comment (Result : out Match_Type)
      is
         Tmp_Result : Range_Type;
      begin
         Parse_Sections ("<!--", "-->", Tmp_Result);
         Result := (if Tmp_Result = Null_Range then Match_None else Match_OK);
      end Parse_Comment;

      ----------------------------------
      -- Parse_Processing_Information --
      ----------------------------------

      procedure Parse_Processing_Information (Result : out Match_Type)
        with
          Pre => Data_Valid (Data);

      procedure Parse_Processing_Information (Result : out Match_Type)
      is
         Tmp_Result : Range_Type;
      begin
         Parse_Sections ("<?", "?>", Tmp_Result);
         Result := (if Tmp_Result = Null_Range then Match_None else Match_OK);
      end Parse_Processing_Information;

      -------------------
      -- Parse_Doctype --
      -------------------

      procedure Parse_Doctype (Result : out Match_Type)
        with
          Pre => Data_Valid (Data);

      procedure Parse_Doctype (Result : out Match_Type)
      is
         Old_Offset   : constant Natural := Offset;
         Tmp_Result   : Match_Type;
         Unused_Range, Text : Range_Type;
      begin
         Result := Match_Invalid;
         Skip (Whitespace);
         Match_String ("<!DOCTYPE", Tmp_Result);
         if Tmp_Result /= Match_OK or
           Data_Overflow
         then
            Restore_Offset (Old_Offset);
            return;
         end if;

         Match_Until_Set ("[", ">", Tmp_Result, Unused_Range);
         if Tmp_Result = Match_OK or Data_Overflow
         then
            Match_Until_Set ("]", Empty_Set, Tmp_Result, Unused_Range);
            if Tmp_Result /= Match_OK or Data_Overflow
            then
               Restore_Offset (Old_Offset);
               return;
            end if;
         end if;
         pragma Unreferenced (Unused_Range);

         Skip (Whitespace);
         Match_Until_String (">", Text);
         if Text = Null_Range
         then
            Restore_Offset (Old_Offset);
            return;
         end if;
         Result := Match_OK;
      end Parse_Doctype;

      -------------------
      -- Parse_Section --
      -------------------

      procedure Parse_Sections
        with
          Pre => Data_Valid (Data);

      procedure Parse_Sections
      is
         Match_Doctype, Match_Comment, Match_PI : Match_Type;
      begin
         loop
            Parse_Doctype (Match_Doctype);
            Parse_Comment (Match_Comment);
            Parse_Processing_Information (Match_PI);
            exit when Match_Doctype /= Match_OK and
              Match_Comment /= Match_OK and
              Match_PI /= Match_OK;
         end loop;

      end Parse_Sections;

      --------------------
      -- Parse_Internal --
      --------------------

      procedure Parse_Internal (Match  : out Match_Type;
                                Start  : out Index_Type)
        with
          Pre  => Data_Valid (Data),
          Post => (if Match /= Match_OK then Offset = Offset'Old),
          Annotate => (gnatcheck, Exempt_On, "Recursive_Subprograms", "foo");

      -----------------
      -- Parse_CDATA --
      -----------------

      procedure Parse_CDATA (Result : out Match_Type;
                             Start  : out Index_Type)
        with
          Pre => Data_Valid (Data);

      procedure Parse_CDATA (Result : out Match_Type;
                             Start  : out Index_Type)
      is
         Tmp_Result : Range_Type;
         Tmp_Start  : Index_Type;
         Valid      : Boolean;
      begin
         Result := Match_None;
         Start  := Invalid_Index;

         Parse_Sections ("<![CDATA[", "]]>", Tmp_Result);
         if Tmp_Result /= Null_Range and then Length (Tmp_Result) > 0
         then
            Context_Put_String (Value  => Data (Tmp_Result.First .. Tmp_Result.Last),
                                Start  => Tmp_Start,
                                Result => Valid);
            if Valid
            then
               Start  := Tmp_Start;
               Result := Match_OK;
            else
               Result := Match_Invalid;
            end if;
            return;
         end if;
      end Parse_CDATA;

      -------------------
      -- Parse_Content --
      -------------------

      procedure Parse_Content (Match : out Match_Type;
                               Start : out Index_Type)
        with
          Pre => Data_Valid (Data);

      procedure Parse_Content (Match : out Match_Type;
                               Start : out Index_Type)
      is
         Content_Range : Range_Type;
         Match_Content : Match_Type;
         Match_CDATA   : Match_Type;
         Match_Comment : Match_Type;
         Match_PI      : Match_Type;
         Valid         : Boolean;
      begin
         Start := Invalid_Index;
         Match := Match_Invalid;
         if Data_Overflow
         then
            return;
         end if;

         Match := Match_OK;

         Parse_CDATA (Match_CDATA, Start);
         if Match_CDATA = Match_OK
         then
            return;
         end if;

         Parse_Comment (Match_Comment);
         if Match_Comment = Match_OK
         then
            return;
         end if;

         Parse_Processing_Information (Match_PI);
         if Match_PI = Match_OK
         then
            return;
         end if;

         Match_Until_Set ("<", Empty_Set, Match_Content, Content_Range);
         if Content_Range /= Null_Range and then Length (Content_Range) > 0
         then
            Context_Put_String (Value  => Data (Content_Range.First .. Content_Range.Last),
                                Start  => Start,
                                Result => Valid);
            if not Valid
            then
               Match := Match_Invalid;
            end if;
            return;
         end if;

         Match := Match_None;
         return;

      end Parse_Content;

      -------------------------
      -- Parse_Internal_Left --
      -------------------------

      procedure Parse_Internal_Left (Match  : out Match_Type;
                                     Start  : out Index_Type;
                                     Parent : out Index_Type;
                                     Name   : out Range_Type;
                                     Done   : out Boolean);

      procedure Parse_Internal_Left (Match  : out Match_Type;
                                     Start  : out Index_Type;
                                     Parent : out Index_Type;
                                     Name   : out Range_Type;
                                     Done   : out Boolean)
      is
         Old_Offset : constant Natural := Offset;
      begin
         Match  := Match_Invalid;
         Start  := Invalid_Index;
         Parent := Invalid_Index;
         Done   := False;

         Parse_Sections;

         if Data_Overflow
         then
            Restore_Offset (Old_Offset);
            return;
         end if;

         Parse_Opening_Tag (Match, Name, Start, Done);
         if Match /= Match_OK
         then
            Restore_Offset (Old_Offset);
            Match := Match_None;
            return;
         end if;

         Parent := Start;

         if Context_Overflow
         then
            Restore_Offset (Old_Offset);
            Match := Match_Out_Of_Memory;
            return;
         end if;

         Parse_Sections;

         if Done
         then
            Match := Match_OK;
            return;
         end if;

      end Parse_Internal_Left;

      ----------------
      -- Link_Child --
      ----------------

      procedure Link_Child (Sub_Match      : Match_Type;
                            Child_Start    : Index_Type;
                            Parent         : Index_Type;
                            Previous_Child : in out Index_Type);

      procedure Link_Child (Sub_Match      : Match_Type;
                            Child_Start    : Index_Type;
                            Parent         : Index_Type;
                            Previous_Child : in out Index_Type)
      is
      begin
         if Sub_Match = Match_OK and Child_Start /= Invalid_Index
         then
            if Previous_Child = Invalid_Index
            then
               Context (Parent).Children := Sub (Child_Start, Parent);
            else
               Context (Previous_Child).Siblings := Sub (Child_Start, Previous_Child);
            end if;
            Previous_Child := Child_Start;
         end if;
      end Link_Child;

      --------------------
      -- Parse_Internal --
      --------------------

      procedure Parse_Internal (Match  : out Match_Type;
                                Start  : out Index_Type)
      is
         Sub_Match      : Match_Type;
         Child_Start    : Index_Type;
         Previous_Child : Index_Type := Invalid_Index;
         Parent         : Index_Type;
         Name           : Range_Type;
         Done           : Boolean;

         type Parse_Mode_Type is (Parse_Start, Parse_Cont);
         type Parse_Element_Type is
            record
               Index    : Index_Type;
               Position : Natural;
               Mode     : Parse_Mode_Type := Parse_Cont;
            end record;

         Tmp : Parse_Element_Type;
         type Parse_Stack_Type is array (Natural range <>) of Parse_Element_Type;
         Parse_Stack : Parse_Stack_Type (1 .. 100);
         package S is new SXML.Stack (Parse_Element_Type, Parse_Stack_Type, Parse_Stack);
      begin

         Match := Match_Invalid;
         Start := Invalid_Index;

         S.Push ((Context_Index, Position, Parse_Start));

         Outer :
         loop
            loop
               exit Outer when S.Is_Empty;

               S.Pop (Tmp);
               Position      := Tmp.Position;
               Context_Index := Tmp.Index;

               if Tmp.Mode = Parse_Start
               then
                  Parse_Internal_Left (Sub_Match, Start, Parent, Name, Done);
                  if Done
                  then
                     Match := Match_OK;
                     exit;
                  end if;

                  if Sub_Match /= Match_OK
                  then
                     S.Drop;
                     exit;
                  end if;

                  Parse_Content (Sub_Match, Child_Start);
                  Link_Child (Sub_Match, Child_Start, Parent, Previous_Child);

                  S.Push ((Context_Index, Position, Parse_Cont));
                  S.Push ((Context_Index, Position, Parse_Start));
                  exit;

               end if;

               if Tmp.Mode = Parse_Cont
               then

                  Parse_Closing_Tag (Data (Name.First .. Name.Last), Match);

                  if Match /= Match_OK
                  then
                     S.Drop;
                     exit;
                  end if;

                  Parse_Sections;
                  S.Drop;
               end if;

            end loop;
         end loop Outer;

      end Parse_Internal;

      --------------------------
      -- Skip_Byte_Order_Mark --
      --------------------------

      procedure Skip_Byte_Order_Mark;

      procedure Skip_Byte_Order_Mark
      is
      begin
         --  Too little space for BOM
         if Data'First > Data'Last - Offset - 3
         then
            return;
         end if;

         --  We only support UTF-8 BOM
         if Data (Data'First + Offset .. Data'First + Offset + 2) =
           Character'Val (16#ef#) &
           Character'Val (16#bb#) &
           Character'Val (16#bf#)
         then
            Offset := Offset + 3;
         end if;

      end Skip_Byte_Order_Mark;

   begin
      Skip_Byte_Order_Mark;
      Parse_Internal (Parse_Result, Unused);
      pragma Unreferenced (Unused);
      if Parse_Result = Match_OK
      then
         Position := 0;
      else
         Position := Error_Index;
      end if;

      if Context_Index in Context'Range
      then
         Context (Context_Index) := Null_Node;
      end if;
   end Parse;

end SXML.Parser;
