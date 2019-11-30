--
--  @summary XML parser implementation
--  @author  Alexander Senier
--  @date    2018-11-15
--
--  Copyright (C) 2018 Componolit GmbH
--
--  This file is part of SXML, which is distributed under the terms of the
--  GNU Affero General Public License version 3.
--

private with SXML.Stack;

package body SXML.Generic_Parser with
   Refined_State => (State => (Call_Stack.State, Result_Stack.State))
is
   pragma Annotate (GNATprove, Terminating, SXML.Generic_Parser);

   type Range_Type is
      record
         First : Natural;
         Last  : Natural;
      end record
    with
      Predicate => (Range_Type.Last < Natural'Last and Range_Type.First <= Range_Type.Last)
                   or (Range_Type.First = Natural'Last and Range_Type.Last = 0);
   Null_Range : constant Range_Type := (Natural'Last, 0);

   type State_Type is (Call_Start, Loop_Start, Recurse_End, Loop_End);

   type Local_Type is
      record
         Done           : Boolean;
         Do_Exit        : Boolean;
         Name           : Range_Type;
         Sub_Match      : Match_Type;
         Child_Start    : Index_Type;
         Previous_Child : Index_Type;
         Parent         : Index_Type;
         Old_Offset     : Natural;
         State          : State_Type;
      end record;
   Null_Local : constant Local_Type := (Done           => False,
                                        Do_Exit        => False,
                                        Name           => Null_Range,
                                        Sub_Match      => Match_Invalid,
                                        Child_Start    => Invalid_Index,
                                        Previous_Child => Invalid_Index,
                                        Parent         => Invalid_Index,
                                        Old_Offset     => 0,
                                        State          => Call_Start);

   type Frame_Type is
      record
         First : Boolean;
         Local : Local_Type;
      end record;

   package Call_Stack is new SXML.Stack (Frame_Type, (True, Null_Local), Depth);

   type Out_Type is
      record
         Match : Match_Type;
         Start : Index_Type;
      end record;

   package Result_Stack is new SXML.Stack (Out_Type, (Match_Invalid, Invalid_Index), Depth);

   -----------
   -- Parse --
   -----------

   procedure Parse (Data         :        Content_Type;
                    Document     : in out Document_Type;
                    Parse_Result :    out Match_Type;
                    Position     :    out Natural)
   is
      Unused : Index_Type;

      subtype Document_Index_Type is Index_Type with
        Predicate => Document_Index_Type in Document'Range;

      Document_Index : Document_Index_Type := Document'First;
      Offset         : Natural := 0;
      Error_Index    : Natural := 0;

      function Is_Valid (R : Range_Type) return Boolean is
        (R.Last < Natural'Last and R.Last >= R.First);

      function Length (R : Range_Type) return Natural is
        (R.Last - R.First + 1)
        with
          Pre => Is_Valid (R);

      type Set_Type is new String with
        Predicate => Set_Type'First >= 0
                     and then Set_Type'Last < Natural'Last
                     and then Set_Type'First <= Set_Type'Last;
      Empty_Set : constant Set_Type := (1 => Character'Val (0));

      Whitespace : constant Set_Type :=
        Character'Val (16#20#)
        & Character'Val (16#9#)
        & Character'Val (16#D#)
        & Character'Val (16#A#);

      --------------------
      -- Restore_Offset --
      --------------------

      procedure Restore_Offset (Old_Offset : Natural) with
        Post => Offset = Old_Offset;

      procedure Restore_Offset (Old_Offset : Natural) is
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

      procedure Restore_Context (Old_Index : Document_Index_Type) with
        Post => Document_Index = Old_Index;

      procedure Restore_Context (Old_Index : Document_Index_Type) is
      begin
         Document_Index := Old_Index;
      end Restore_Context;

      --------------
      -- In_Range --
      --------------

      function In_Range (R : Range_Type) return Boolean is
         (R.First >= Data'First
          and then R.Last <= Data'Last
          and then R.First <= R.Last
          and then Valid_Content (R.First, R.Last));

      -------------------
      -- Data_Overflow --
      -------------------

      function Data_Overflow return Boolean is
        (Data'First > Natural'Last - Offset
         or Offset > Data'Length - 1);

      ---------------
      -- Match_Set --
      ---------------

      function Match_Set (Valid   : Set_Type;
                          Invalid : Set_Type;
                          Value   : Character) return Match_Type with
        Pre  => Offset < Data'Length,
        Post => (case Match_Set'Result is
                 when Match_OK      => (for some E of Valid   => E = Value)
                                       and (for all E of Invalid  => E /= Value),
                 when Match_Invalid => (for some E of Invalid => E = Value),
                 when Match_None    => (for all E of Invalid  => E /= Value)
                                       and (for all E of Valid => E /= Value),
                 when others        => False);

      function Match_Set (Valid   : Set_Type;
                          Invalid : Set_Type;
                          Value   : Character) return Match_Type
      is
      begin
         for I in Invalid'Range
         loop
            if Invalid (I) = Value then
               return Match_Invalid;
            end if;
            pragma Loop_Invariant ((for all E of Invalid (Invalid'First .. I) => E /= Value));
         end loop;

         for V in Valid'Range
         loop
            if Valid (V) = Value then
               return Match_OK;
            end if;
            pragma Loop_Invariant ((for all E of Valid (Valid'First .. V) => E /= Value));
         end loop;
         return Match_None;
      end Match_Set;

      ---------------
      -- Match_Set --
      ---------------

      procedure Match_Set (Valid   :     Set_Type;
                           Invalid :     Set_Type;
                           Match   : out Match_Type;
                           Value   : out Character) with
        Pre  => Offset < Data'Length,
        Post => (case Match is
                 when Match_OK      => (for some E of Valid   => E = Data (Data'First + Offset'Old))
                                       and (for all E of Invalid  => E /= Data (Data'First + Offset'Old))
                                       and Offset > Offset'Old,
                 when Match_Invalid => (for some E of Invalid => E = Data (Data'First + Offset'Old))
                                       and Offset > Offset'Old,
                 when Match_None    => (for all E of Invalid  => E /= Data (Data'First + Offset'Old))
                                       and (for all E of Valid    => E /= Data (Data'First + Offset'Old))
                                       and Offset > Offset'Old,
                 when others        => False);

      procedure Match_Set (Valid   :     Set_Type;
                           Invalid :     Set_Type;
                           Match   : out Match_Type;
                           Value   : out Character)
      is
      begin
         Match := Match_Invalid;
         Value := Character'Val (0);

         if Data_Overflow then
            return;
         end if;

         Match := Match_Set (Valid, Invalid, Data (Data'First + Offset));

         if Match = Match_OK then
            Value  := Data (Data'First + Offset);
         end if;

         Offset := Offset + 1;

      end Match_Set;

      procedure Match_Set (Valid   :     Set_Type;
                           Invalid :     Set_Type;
                           Match   : out Match_Type)
      with
         Pre  => Offset < Data'Length,
         Post => (case Match is
                  when Match_OK      => (for some E of Valid   => E = Data (Data'First + Offset'Old))
                                        and (for all E of Invalid  => E /= Data (Data'First + Offset'Old))
                                        and Offset > Offset'Old,
                  when Match_Invalid => (for some E of Invalid => E = Data (Data'First + Offset'Old))
                                        and Offset > Offset'Old,
                  when Match_None    => (for all E of Invalid  => E /= Data (Data'First + Offset'Old))
                                        and (for all E of Valid    => E /= Data (Data'First + Offset'Old))
                                        and Offset > Offset'Old,
                  when others        => False);

      procedure Match_Set (Valid   :     Set_Type;
                           Invalid :     Set_Type;
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
                              Match : out Match_Type) with
        Pre  => Text'Length > 0,
        Post => (if Match /= Match_OK
                 then Offset = Offset'Old
                 else Offset > Offset'Old);

      procedure Match_String (Text  :     String;
                              Match : out Match_Type)
      is
         Old_Offset : constant Natural := Offset;
      begin
         for C of Text
         loop
            if Data_Overflow then
               Match  := Match_Invalid;
               Restore_Offset (Old_Offset);
               return;
            end if;

            if Data (Data'First + Offset) /= C then
               Match  := Match_None;
               Restore_Offset (Old_Offset);
               return;
            end if;

            Offset := Offset + 1;
            pragma Loop_Invariant (Offset > Offset'Loop_Entry);
         end loop;

         Match := Match_OK;
      end Match_String;

      ----------------------
      -- Context_Overflow --
      ----------------------

      function Context_Overflow return Boolean is
        (Document_Index >= Document'Last);

      ------------------------
      -- Context_Put_String --
      ------------------------

      procedure Context_Put_String (Value  :     Content_Type;
                                    Start  : out Index_Type;
                                    Result : out Boolean);

      procedure Context_Put_String (Value  :     Content_Type;
                                    Start  : out Index_Type;
                                    Result : out Boolean)
      is
         NE : constant Offset_Type := Length (Value);
      begin
         Result := False;
         Start  := Invalid_Index;

         if
            (Underflow (Document'Last, NE) or else Document_Index > Sub (Document'Last, NE))
            or Underflow (Document_Index, Document'First)
         then
            return;
         end if;

         Start := Document_Index;
         Put_Content (Document, Sub (Document_Index, Document'First), Value);
         Document_Index := Add (Document_Index, NE);
         Result := True;
      end Context_Put_String;

      ----------------------
      -- Match_Until_Text --
      ----------------------

      procedure Match_Until_String (End_String :     String;
                                    Text       : out Range_Type) with
        Pre  => End_String'Length > 0
                and not Data_Overflow,
        Post => (if Text /= Null_Range then Offset > Offset'Old else Offset = Offset'Old);

      procedure Match_Until_String (End_String :     String;
                                    Text       : out Range_Type)
      is
         Old_Offset : constant Natural := Offset;
         First      : constant Natural := Data'First + Offset;
         Result     : Match_Type;
      begin
         Text := Null_Range;

         loop
            if
               Data_Overflow
               or Offset = Natural'Last
            then
               Restore_Offset (Old_Offset);
               return;
            end if;

            Match_String (End_String, Result);
            exit when Result = Match_OK;

            Offset := Offset + 1;

            pragma Loop_Variant (Increases => Offset);
            pragma Loop_Invariant (Data'First <= Natural'Last - Offset);
            pragma Loop_Invariant (Offset < Natural'Last);
            pragma Loop_Invariant (Offset > Old_Offset);

         end loop;

         if
            Old_Offset > Offset - End_String'Length - 1
            or Data'First > Natural'Last - Offset
         then
            Restore_Offset (Old_Offset);
            return;
         end if;

         Text := (First, Data'First + Offset - End_String'Length - 1);

      end Match_Until_String;

      ---------------------
      -- Match_Until_Set --
      ---------------------

      procedure Match_Until_Set (End_Set     :     Set_Type;
                                 Invalid_Set :     Set_Type;
                                 Match       : out Match_Type;
                                 Result      : out Range_Type) with
        Pre  => Data'First <= Data'Last - Offset,
        Post => (case Match is
                 when Match_OK   => In_Range (Result)
                                    and Valid_Content (Result.First, Result.Last)
                                    and Offset > Offset'Old,
                 when Match_None => Result = Null_Range
                                    and Offset >= Offset'Old,
                 when others     => Offset = Offset'Old);

      procedure Match_Until_Set (End_Set     :     Set_Type;
                                 Invalid_Set :     Set_Type;
                                 Match       : out Match_Type;
                                 Result      : out Range_Type)
      is
         Old_Offset : constant Natural := Offset;
         First      : constant Natural := Data'First + Offset;
         Last       : Natural;
         Tmp_Match  : Match_Type;
      begin
         Match  := Match_Invalid;
         Result := Null_Range;

         loop
            if Data_Overflow then
               Restore_Offset (Old_Offset);
               return;
            end if;

            Match_Set (End_Set, Invalid_Set, Tmp_Match);
            exit when Tmp_Match /= Match_None;

            pragma Loop_Variant (Increases => Offset);
            pragma Loop_Invariant (Offset > Old_Offset);
         end loop;

         if
            Tmp_Match /= Match_OK
            or Data'First > Data'Last - Offset + 1
         then
            Restore_Offset (Old_Offset);
            return;
         end if;

         Offset := Offset - 1;
         Last   := Data'First + Offset - 1;

         if Old_Offset <= Offset - 1 then
            Result := (First, Last);
            Match  := Match_OK;
         else
            Result := Null_Range;
            Match  := Match_None;
         end if;

      end Match_Until_Set;

      ----------
      -- Skip --
      ----------

      procedure Skip (Skip_Set : Set_Type) with
        Post => Offset >= Offset'Old;

      procedure Skip (Skip_Set : Set_Type)
      is
         Last_None_Whitespace : Natural;
         Result : Match_Type;
         Unused_Result : Character;
         pragma Unreferenced (Unused_Result);
      begin
         loop
            pragma Loop_Variant (Increases => Offset);
            pragma Loop_Invariant (Offset >= Offset'Loop_Entry);
            if Data_Overflow then
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
                                 Match : out Match_Type) with
        Post => (if Match = Match_OK
                 then Offset > Offset'Old
                 else Offset = Offset'Old);

      procedure Parse_Attribute (Start : out Index_Type;
                                 Match : out Match_Type)
      is
         Old_Offset          : constant Natural := Offset;
         Attribute_Name      : Range_Type;
         Attribute_Value     : Range_Type;
         Match_Tmp           : Match_Type;
         Separator           : Character;
         Off                 : Offset_Type;
         Attr_Value_Elements : Offset_Type;
      begin

         Start := Invalid_Index;

         if Context_Overflow then
            Match := Match_Out_Of_Memory;
            return;
         end if;

         Match := Match_Invalid;

         Skip (Whitespace);
         if Data_Overflow then
            Restore_Offset (Old_Offset);
            return;
         end if;

         Match_Until_Set (Whitespace & "=", ">", Match_Tmp, Attribute_Name);
         if Match_Tmp /= Match_OK then
            Restore_Offset (Old_Offset);
            return;
         end if;

         Skip (Whitespace);
         if Data_Overflow then
            Restore_Offset (Old_Offset);
            return;
         end if;

         Match_Set ("=", ">", Match_Tmp);
         if Match_Tmp /= Match_OK then
            Restore_Offset (Old_Offset);
            return;
         end if;

         Skip (Whitespace);
         if Data_Overflow then
            Restore_Offset (Old_Offset);
            return;
         end if;

         Match_Set ("""'", ">", Match_Tmp, Separator);
         if
            Match_Tmp /= Match_OK
            or Data_Overflow
         then
            Restore_Offset (Old_Offset);
            return;
         end if;

         Match_Until_Set ((1 => Separator), "<", Match_Tmp, Attribute_Value);
         if
            (Match_Tmp /= Match_OK and Match_Tmp /= Match_None)
            or Data_Overflow
         then
            Restore_Offset (Old_Offset);
            return;
         end if;
         Offset := Offset + 1;

         Start := Document_Index;
         Off   := Sub (Document_Index, Document'First);

         pragma Assert (Valid_Content (Attribute_Name.First, Attribute_Name.Last));

         Attr_Value_Elements := (if Attribute_Value /= Null_Range
                                 then Length (Data (Attribute_Value.First .. Attribute_Value.Last))
                                 else 0);

         if
            Off > Document'Length
                  - Length (Data (Attribute_Name.First .. Attribute_Name.Last))
                  - Attr_Value_Elements
            or else Off + Length (Data (Attribute_Name.First .. Attribute_Name.Last)) + Attr_Value_Elements
                    >= Document'Length
         then
            Restore_Offset (Old_Offset);
            return;
         end if;

         SXML.Attribute
           (Name     => Data (Attribute_Name.First .. Attribute_Name.Last),
            Data     => Data (Attribute_Value.First .. Attribute_Value.Last),
            Offset   => Off,
            Document => Document);

         if
            Off > Offset_Type (Index_Type'Last - Document'First)
            or Off >= Document'Length
         then
            Restore_Offset (Old_Offset);
            return;
         end if;

         Document_Index := Add (Document'First, Off);
         Match := Match_OK;

      end Parse_Attribute;

      -----------------------------
      -- Is_Valid_Attribute_Link --
      -----------------------------

      function Is_Valid_Attribute_Link (Next     : Index_Type;
                                        Parent   : Index_Type;
                                        Previous : Index_Type) return Boolean;

      function Is_Valid_Attribute_Link (Next     : Index_Type;
                                        Parent   : Index_Type;
                                        Previous : Index_Type) return Boolean is
        (if
            Next /= Invalid_Index
            and then Previous = Invalid_Index
         then
            Parent in Document'Range
            and then Next > Parent
            and then (Document (Parent).Kind = Kind_Element_Open)
         else
            Previous in Document'Range
            and then Next > Previous
            and then (Document (Previous).Kind = Kind_Attribute));

      --------------------
      -- Link_Attribute --
      --------------------

      procedure Link_Attribute (Next     :        Index_Type;
                                Parent   :        Index_Type;
                                Previous : in out Index_Type) with
        Pre  => Is_Valid_Attribute_Link (Next, Parent, Previous),
        Post => (if Next /= Invalid_Index then Previous = Next);

      procedure Link_Attribute (Next     :        Index_Type;
                                Parent   :        Index_Type;
                                Previous : in out Index_Type)
      is
      begin
         if Next /= Invalid_Index then
            if Previous = Invalid_Index then
               Document (Parent).Attributes := Sub (Next, Parent);
            else
               Document (Previous).Next_Attribute := Sub (Next, Previous);
            end if;
            Previous := Next;
         end if;
      end Link_Attribute;

      -----------------------
      -- Parse_Opening_Tag --
      -----------------------

      procedure Parse_Opening_Tag (Match : out Match_Type;
                                   Name  : out Range_Type;
                                   Start : out Index_Type;
                                   Done  : out Boolean) with
        Post => (if Match = Match_OK
                 then In_Range (Name) and Offset > Offset'Old
                 else Offset = Offset'Old);

      procedure Parse_Opening_Tag (Match : out Match_Type;
                                   Name  : out Range_Type;
                                   Start : out Index_Type;
                                   Done  : out Boolean)
      is
         Old_Index          : constant Index_Type := Document_Index;
         Old_Offset         : constant Natural := Offset;
         Match_Attr         : Match_Type;
         Match_Tmp          : Match_Type;
         Attribute_Start    : Index_Type;
         Previous_Attribute : Index_Type := Invalid_Index;
      begin
         Name  := Null_Range;
         Match := Match_Invalid;
         Done  := False;
         Start := Invalid_Index;

         if Context_Overflow then
            Match := Match_Out_Of_Memory;
            return;
         end if;

         Skip (Whitespace);
         if Data_Overflow then
            Restore_Offset (Old_Offset);
            return;
         end if;

         --  Match opening '<'
         Match_Set ("<", Empty_Set, Match_Tmp);
         if
            Match_Tmp /= Match_OK
            or Data_Overflow
         then
            Restore_Offset (Old_Offset);
            return;
         end if;

         --  Match tag name
         Match_Until_Set (Whitespace & ">/", Empty_Set, Match_Tmp, Name);
         if
            Match_Tmp /= Match_OK
            or else not (Document_Index in Document'Range)
            or else not Has_Space (Document, Offset_Type (Document_Index) + 1, Data (Name.First .. Name.Last))
         then
            Restore_Offset (Old_Offset);
            return;
         end if;

         Open (Name     => Data (Name.First .. Name.Last),
               Document => Document,
               Position => Document_Index,
               Start    => Start);

         loop
            Parse_Attribute (Attribute_Start, Match_Attr);
            exit when Match_Attr /= Match_OK;

            if not Is_Valid_Attribute_Link (Attribute_Start, Start, Previous_Attribute) then
               Restore_Offset (Old_Offset);
               return;
            end if;

            Link_Attribute (Attribute_Start, Start, Previous_Attribute);

            pragma Loop_Variant (Increases => Offset);
            pragma Loop_Invariant (Offset > Old_Offset);

         end loop;

         --  Short form node?
         Skip (Whitespace);
         Match_String ("/>", Match_Tmp);
         if Match_Tmp = Match_OK then
            Done := True;
         else
            if Data_Overflow then
               Restore_Offset (Old_Offset);
               Restore_Context (Old_Index);
               return;
            end if;

            --  Match closing '>'
            Match_Set (">", Empty_Set, Match_Tmp);
            if Match_Tmp /= Match_OK then
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

      procedure Parse_Closing_Tag (Name  :     String;
                                   Match : out Match_Type) with
        Post => (if Match = Match_OK then Offset > Offset'Old else Offset = Offset'Old);

      procedure Parse_Closing_Tag (Name  :     String;
                                   Match : out Match_Type)
      is
         Old_Offset   : constant Natural := Offset;
         Closing_Name : Range_Type;
         Match_Tmp    : Match_Type;
      begin
         Match := Match_Invalid;

         if Context_Overflow then
            Restore_Offset (Old_Offset);
            Match := Match_Out_Of_Memory;
            return;
         end if;

         if Data_Overflow then
            Restore_Offset (Old_Offset);
            return;
         end if;

         Skip (Whitespace);

         Match_String ("</", Match_Tmp);
         if
            Match_Tmp /= Match_OK
            or Data_Overflow
         then
            Restore_Offset (Old_Offset);
            return;
         end if;

         Match_Until_Set (Whitespace & ">", Empty_Set, Match_Tmp, Closing_Name);
         if
            Match_Tmp /= Match_OK
            or Data_Overflow
         then
            Restore_Offset (Old_Offset);
            return;
         end if;

         --  Match closing tag
         Skip (Whitespace);

         if Data_Overflow then
            Restore_Offset (Old_Offset);
            return;
         end if;

         Match_Set (">", Empty_Set, Match_Tmp);
         if Match_Tmp /= Match_OK then
            Restore_Offset (Old_Offset);
            return;
         end if;

         if Data (Closing_Name.First .. Closing_Name.Last) /= Name then
            Restore_Offset (Old_Offset);
            Match := Match_None_Wellformed;
            return;
         end if;

         Match := Match_OK;

      end Parse_Closing_Tag;

      -------------------
      -- Parse_Section --
      -------------------

      procedure Parse_Sections (Start_Tag :     String;
                                End_Tag   :     String;
                                Result    : out Range_Type) with
        Pre  => Start_Tag'Length > 0 and
                End_Tag'Length > 0,
        Post => (if Result /= Null_Range then Offset > Offset'Old else Offset >= Offset'Old);

      procedure Parse_Sections (Start_Tag :     String;
                                End_Tag   :     String;
                                Result    : out Range_Type)
      is
         Old_Offset : Natural;
         Tmp_Result : Match_Type;
      begin
         Result := Null_Range;
         loop
            Old_Offset := Offset;
            if Data_Overflow then
               Restore_Offset (Old_Offset);
               return;
            end if;

            Skip (Whitespace);
            Match_String (Start_Tag, Tmp_Result);
            if
               Tmp_Result /= Match_OK
               or Data_Overflow
            then
               Restore_Offset (Old_Offset);
               return;
            end if;

            Match_Until_String (End_Tag, Result);
            if Result = Null_Range then
               Restore_Offset (Old_Offset);
               return;
            end if;

            pragma Loop_Variant (Increases => Offset);
            pragma Loop_Invariant (Offset > Offset'Loop_Entry);
         end loop;
      end Parse_Sections;

      -------------------
      -- Parse_Comment --
      -------------------

      procedure Parse_Comment (Result : out Match_Type) with
        Post => (if Result = Match_OK then Offset > Offset'Old else Offset >= Offset'Old);

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

      procedure Parse_Processing_Information (Result : out Match_Type) with
        Post => (if Result = Match_OK then Offset > Offset'Old else Offset >= Offset'Old);

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

      procedure Parse_Doctype (Result : out Match_Type) with
        Post => (if Result = Match_OK then Offset > Offset'Old else Offset = Offset'Old);

      procedure Parse_Doctype (Result : out Match_Type)
      is
         Old_Offset   : constant Natural := Offset;
         Tmp_Result   : Match_Type;
         Unused_Range : Range_Type;
         Text         : Range_Type;
      begin
         Result := Match_Invalid;
         Skip (Whitespace);
         Match_String ("<!DOCTYPE", Tmp_Result);
         if
            Tmp_Result /= Match_OK
            or Data_Overflow
         then
            Restore_Offset (Old_Offset);
            return;
         end if;

         Match_Until_Set ("[", ">", Tmp_Result, Unused_Range);
         if Tmp_Result = Match_OK then
            if Data_Overflow then
               Restore_Offset (Old_Offset);
               return;
            end if;
            Match_Until_Set ("]", Empty_Set, Tmp_Result, Unused_Range);
            if
               Tmp_Result /= Match_OK
               or Data_Overflow
            then
               Restore_Offset (Old_Offset);
               return;
            end if;
         end if;
         pragma Unreferenced (Unused_Range);

         Skip (Whitespace);
         if Data_Overflow then
            Restore_Offset (Old_Offset);
            return;
         end if;

         Match_Until_String (">", Text);
         if Text = Null_Range then
            Restore_Offset (Old_Offset);
            return;
         end if;
         Result := Match_OK;
      end Parse_Doctype;

      -------------------
      -- Parse_Section --
      -------------------

      procedure Parse_Sections with
        Post => Offset >= Offset'Old;

      procedure Parse_Sections
      is
         Match_Doctype : Match_Type;
         Match_Comment : Match_Type;
         Match_PI      : Match_Type;
         Old_Offset    : constant Natural := Offset with Ghost;
      begin
         loop
            Parse_Doctype (Match_Doctype);
            Parse_Comment (Match_Comment);
            Parse_Processing_Information (Match_PI);

            exit when Match_Doctype /= Match_OK and Match_Comment /= Match_OK and Match_PI /= Match_OK;

            pragma Loop_Variant (Increases => Offset);
            pragma Loop_Invariant (Offset >= Old_Offset);

         end loop;
      end Parse_Sections;

      --------------------
      -- Parse_Internal --
      --------------------

      procedure Parse_Internal (Match : out Match_Type;
                                Start : out Index_Type) with
        Post => (if Match = Match_OK then Offset > Offset'Old else Offset >= Offset'Old);

      -----------------
      -- Parse_CDATA --
      -----------------

      procedure Parse_CDATA (Result : out Match_Type;
                             Start  : out Index_Type)
      with
        Post => (if Result = Match_OK then Offset > Offset'Old else Offset >= Offset'Old);

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
         if
            Tmp_Result /= Null_Range
            and then Tmp_Result.First >= Data'First
            and then Tmp_Result.Last <= Data'Last
            and then Valid_Content (Tmp_Result.First, Tmp_Result.Last)
         then
            Context_Put_String (Value  => Data (Tmp_Result.First .. Tmp_Result.Last),
                                Start  => Tmp_Start,
                                Result => Valid);
            if Valid then
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
        Post => (if Match = Match_OK then Offset > Offset'Old else Offset >= Offset'Old);

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
         if Data_Overflow then
            return;
         end if;

         Match := Match_OK;

         Parse_CDATA (Match_CDATA, Start);
         if Match_CDATA = Match_OK then
            return;
         end if;

         Parse_Comment (Match_Comment);
         if Match_Comment = Match_OK then
            return;
         end if;

         Parse_Processing_Information (Match_PI);
         if Match_PI = Match_OK then
            return;
         end if;

         if Data_Overflow then
            return;
         end if;

         Match_Until_Set ("<", Empty_Set, Match_Content, Content_Range);
         if
            Match_Content = Match_OK
            and then Length (Content_Range) > 0
         then
            Context_Put_String (Value  => Data (Content_Range.First .. Content_Range.Last),
                                Start  => Start,
                                Result => Valid);
            if not Valid then
               Match := Match_Invalid;
            end if;
            return;
         end if;

         Match := Match_None;
         return;

      end Parse_Content;

      -------------------
      -- Is_Valid_Link --
      -------------------

      function Is_Valid_Link (Child    : Index_Type;
                              Parent   : Index_Type;
                              Previous : Index_Type) return Boolean;

      function Is_Valid_Link (Child    : Index_Type;
                              Parent   : Index_Type;
                              Previous : Index_Type) return Boolean is
         (if
            Child /= Invalid_Index
            and then Previous = Invalid_Index
          then
            Parent in Document'Range
            and then Child > Parent
            and then (Document (Parent).Kind = Kind_Element_Open
                      or Document (Parent).Kind = Kind_Content)
          else
            Previous in Document'Range
            and then Child > Previous
            and then (Document (Previous).Kind = Kind_Element_Open
                      or Document (Previous).Kind = Kind_Content));

      ----------------
      -- Link_Child --
      ----------------

      procedure Link_Child (Child    :        Index_Type;
                            Parent   :        Index_Type;
                            Previous : in out Index_Type) with
        Pre  => Is_Valid_Link (Child, Parent, Previous),
        Post => (if Child /= Invalid_Index then Previous = Child);

      procedure Link_Child (Child    :        Index_Type;
                            Parent   :        Index_Type;
                            Previous : in out Index_Type)
      is
      begin
         if Child /= Invalid_Index then
            if Previous = Invalid_Index then
               Document (Parent).Children := Sub (Child, Parent);
            else
               Document (Previous).Siblings := Sub (Child, Previous);
            end if;
            Previous := Child;
         end if;
      end Link_Child;

      --------------------
      -- Parse_Internal --
      --------------------

      procedure Parse_Internal (Match : out Match_Type;
                                Start : out Index_Type)
      is
         Old_Offset : constant Natural := Offset;

         Frame  : Frame_Type;
         Result : Out_Type;
         Limit  : Natural := Natural'Last;
      begin
         Call_Stack.Init;
         Result_Stack.Init;
         Call_Stack.Push ((True, Null_Local));

         Start := Invalid_Index;
         Match := Match_Invalid;
         loop
            Call_Return : loop

               if Limit < 2 or Call_Stack.Is_Empty then
                  if Match = Match_OK and Offset <= Old_Offset then
                     Match := Match_Invalid;
                  end if;
                  return;
               end if;
               Call_Stack.Pop (Frame);
               case Frame.Local.State is

                  when Call_Start =>
                     Frame.Local.Old_Offset := Offset;
                     Frame.Local.Previous_Child := Invalid_Index;
                     Match := Match_Invalid;

                     if not Frame.First then
                        Parse_Content (Frame.Local.Sub_Match, Start);
                        if Frame.Local.Sub_Match = Match_OK then
                           Match := Match_OK;
                           exit Call_Return;
                        end if;
                     end if;

                     Parse_Sections;

                     if Data_Overflow then
                        Restore_Offset (Frame.Local.Old_Offset);
                        exit Call_Return;
                     end if;

                     Parse_Opening_Tag (Frame.Local.Sub_Match, Frame.Local.Name, Start, Frame.Local.Done);
                     if Frame.Local.Sub_Match /= Match_OK then
                        Restore_Offset (Frame.Local.Old_Offset);
                        Match := Match_None;
                        exit Call_Return;
                     end if;

                     Frame.Local.Parent := Start;

                     if Context_Overflow then
                        Restore_Offset (Frame.Local.Old_Offset);
                        Match := Match_Out_Of_Memory;
                        exit Call_Return;
                     end if;

                     Parse_Sections;

                     if Frame.Local.Done then
                        Match := Match_OK;
                        exit Call_Return;
                     end if;

                     Frame.Local.State := Loop_Start;
                     Call_Stack.Push (Frame);

                  when Loop_Start =>

                     Frame.Local.State := Recurse_End;
                     Call_Stack.Push (Frame);

                     if Call_Stack.Is_Full or Result_Stack.Is_Full then
                        Match := Match_Depth_Limit;
                        return;
                     end if;

                     Call_Stack.Push ((False, Null_Local));
                     Result_Stack.Push ((Match, Start));

                  when Recurse_End =>

                     Frame.Local.Sub_Match := Match;
                     Frame.Local.Child_Start := Start;

                     if Result_Stack.Is_Empty then
                        Match := Match_Invalid;
                        return;
                     end if;

                     Result_Stack.Pop (Result);
                     Match := Result.Match;
                     Start := Result.Start;

                     if Frame.Local.Sub_Match /= Match_OK then
                        Frame.Local.State := Loop_End;
                        Call_Stack.Push (Frame);
                        exit Call_Return;
                     end if;

                     if
                        not Is_Valid_Link (Child    => Frame.Local.Child_Start,
                                           Parent   => Frame.Local.Parent,
                                           Previous => Frame.Local.Previous_Child)
                     then
                        exit Call_Return;
                     end if;

                     Link_Child (Frame.Local.Child_Start, Frame.Local.Parent, Frame.Local.Previous_Child);

                     Frame.Local.State := Loop_Start;
                     Call_Stack.Push (Frame);

                  when Loop_End =>

                     if Frame.Local.Name.First not in Data'Range or Frame.Local.Name.Last not in Data'Range then
                        Match := Match_Invalid;
                        return;
                     end if;

                     Parse_Closing_Tag (Name  => Data (Frame.Local.Name.First .. Frame.Local.Name.Last),
                                        Match => Frame.Local.Sub_Match);
                     if Frame.Local.Sub_Match /= Match_OK then
                        Restore_Offset (Frame.Local.Old_Offset);
                        --  Should not happen, but we cannot prove it unless we show that the value of
                        --  Frame.Local.Old_Offset remains greater or equal to to Old_Offset between stack pushes
                        --  and pops.
                        if Offset < Old_Offset then
                           Offset := Old_Offset;
                        end if;
                        exit Call_Return;
                     end if;

                     Parse_Sections;
                     Match := Match_OK;

               end case;

               Limit := Limit - 1;

               pragma Loop_Invariant (Call_Stack.Is_Valid);
               pragma Loop_Invariant (Result_Stack.Is_Valid);
               pragma Loop_Invariant (Offset >= Old_Offset);
               pragma Loop_Invariant (Limit < Limit'Loop_Entry);
               pragma Loop_Variant (Decreases => Limit);

            end loop Call_Return;

            Limit := Limit - 1;

            pragma Loop_Invariant (Call_Stack.Is_Valid);
            pragma Loop_Invariant (Result_Stack.Is_Valid);
            pragma Loop_Invariant (Offset >= Old_Offset);
            pragma Loop_Variant (Decreases => Limit);
         end loop;

      end Parse_Internal;

      --------------------------
      -- Skip_Byte_Order_Mark --
      --------------------------

      procedure Skip_Byte_Order_Mark;

      procedure Skip_Byte_Order_Mark
      is
      begin
         --  Too little space for BOM
         if
            Offset > Data'Length - 3
            or else Data'First > Data'Last - Offset - 3
         then
            return;
         end if;

         --  We only support UTF-8 BOM
         if
            Data (Data'First + Offset .. Data'First + Offset + 2)
            = Character'Val (16#ef#) & Character'Val (16#bb#) & Character'Val (16#bf#)
         then
            Offset := Offset + 3;
         end if;

      end Skip_Byte_Order_Mark;

   begin
      Skip_Byte_Order_Mark;
      Parse_Internal (Parse_Result, Unused);
      pragma Unreferenced (Unused);
      Skip (Whitespace);

      if Parse_Result = Match_OK then
         if Offset < Data'Length then
            Parse_Result := Match_Trailing_Data;
         end if;
         Position := Offset;
      else
         Position := Error_Index;
      end if;

      if Document_Index in Document'Range
      then
         Document (Document_Index) := Null_Node;
      end if;
   end Parse;

end SXML.Generic_Parser;