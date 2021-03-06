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

private with Basalt.Stack;

package body SXML.Generic_Parser with
   Refined_State => (State => (CS, RS))
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
   Null_Frame : constant Frame_Type := (False, Null_Local);

   type Out_Type is
      record
         Match : Match_Type;
         Start : Index_Type;
      end record;
   Null_Out : constant Out_Type := (Match_Invalid, Invalid_Index);

   package Call_Stack is new Basalt.Stack (Frame_Type, Null_Frame);
   CS : Call_Stack.Context (Depth);

   package Result_Stack is new Basalt.Stack (Out_Type, Null_Out);
   RS : Result_Stack.Context (Depth);

   -----------
   -- Parse --
   -----------

   procedure Parse (Data     :        Content_Type;
                    Document : in out Document_Type;
                    Offset   :    out Natural;
                    Result   :    out Match_Type)
   is
      Unused : Index_Type;

      subtype Document_Index_Type is Index_Type with
        Predicate => Document_Index_Type in Document'Range;

      Document_Index : Document_Index_Type := Document'First;
      Current_Offset : Natural := 0;
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

      --  Need to use & operator of base type (Componolit/Workarounds#11)
      Whitespace : constant Set_Type := Set_Type (String'(Character'Val (16#20#)
                                                          & Character'Val (16#9#)
                                                          & Character'Val (16#D#)
                                                          & Character'Val (16#A#)));

      --------------------
      -- Restore_Offset --
      --------------------

      procedure Restore_Offset (Old_Offset : Natural) with
        Post => Current_Offset = Old_Offset;

      procedure Restore_Offset (Old_Offset : Natural) is
      begin
         if Current_Offset > Error_Index
         then
            Error_Index := Current_Offset;
         end if;
         Current_Offset := Old_Offset;
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
        (Data'First > Natural'Last - Current_Offset
         or Current_Offset > Data'Length - 1);

      ---------------
      -- Match_Set --
      ---------------

      function Match_Set (Valid   : Set_Type;
                          Invalid : Set_Type;
                          Value   : Character) return Match_Type with
        Pre  => Current_Offset < Data'Length,
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
        Pre  => Current_Offset < Data'Length,
        Post => (case Match is
                 when Match_OK      => (for some E of Valid   => E = Data (Data'First + Current_Offset'Old))
                                       and (for all E of Invalid  => E /= Data (Data'First + Current_Offset'Old))
                                       and Current_Offset > Current_Offset'Old,
                 when Match_Invalid => (for some E of Invalid => E = Data (Data'First + Current_Offset'Old))
                                       and Current_Offset > Current_Offset'Old,
                 when Match_None    => (for all E of Invalid  => E /= Data (Data'First + Current_Offset'Old))
                                       and (for all E of Valid    => E /= Data (Data'First + Current_Offset'Old))
                                       and Current_Offset > Current_Offset'Old,
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

         Match := Match_Set (Valid, Invalid, Data (Data'First + Current_Offset));

         if Match = Match_OK then
            Value  := Data (Data'First + Current_Offset);
         end if;

         Current_Offset := Current_Offset + 1;

      end Match_Set;

      procedure Match_Set (Valid   :     Set_Type;
                           Invalid :     Set_Type;
                           Match   : out Match_Type)
      with
         Pre  => Current_Offset < Data'Length,
         Post => (case Match is
                  when Match_OK      => (for some E of Valid   => E = Data (Data'First + Current_Offset'Old))
                                        and (for all E of Invalid  => E /= Data (Data'First + Current_Offset'Old))
                                        and Current_Offset > Current_Offset'Old,
                  when Match_Invalid => (for some E of Invalid => E = Data (Data'First + Current_Offset'Old))
                                        and Current_Offset > Current_Offset'Old,
                  when Match_None    => (for all E of Invalid  => E /= Data (Data'First + Current_Offset'Old))
                                        and (for all E of Valid    => E /= Data (Data'First + Current_Offset'Old))
                                        and Current_Offset > Current_Offset'Old,
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
                 then Current_Offset = Current_Offset'Old
                 else Current_Offset > Current_Offset'Old);

      procedure Match_String (Text  :     String;
                              Match : out Match_Type)
      is
         Old_Offset : constant Natural := Current_Offset;
      begin
         for C of Text
         loop
            if Data_Overflow then
               Match  := Match_Invalid;
               Restore_Offset (Old_Offset);
               return;
            end if;

            if Data (Data'First + Current_Offset) /= C then
               Match  := Match_None;
               Restore_Offset (Old_Offset);
               return;
            end if;

            Current_Offset := Current_Offset + 1;
            pragma Loop_Invariant (Current_Offset > Current_Offset'Loop_Entry);
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
                                    Valid  : out Boolean);

      procedure Context_Put_String (Value  :     Content_Type;
                                    Start  : out Index_Type;
                                    Valid  : out Boolean)
      is
         NE : constant Offset_Type := Length (Value);
      begin
         Valid := False;
         Start := Invalid_Index;

         if
            (Underflow (Document'Last, NE) or else Document_Index > Sub (Document'Last, NE))
            or Underflow (Document_Index, Document'First)
         then
            return;
         end if;

         Start := Document_Index;
         Put_Content (Document, Sub (Document_Index, Document'First), Value);
         Document_Index := Add (Document_Index, NE);
         Valid := True;
      end Context_Put_String;

      ----------------------
      -- Match_Until_Text --
      ----------------------

      procedure Match_Until_String (End_String :     String;
                                    Text       : out Range_Type) with
        Pre  => End_String'Length > 0
                and not Data_Overflow,
        Post => (if Text /= Null_Range
                 then Current_Offset > Current_Offset'Old
                 else Current_Offset = Current_Offset'Old);

      procedure Match_Until_String (End_String :     String;
                                    Text       : out Range_Type)
      is
         Old_Offset   : constant Natural := Current_Offset;
         First        : constant Natural := Data'First + Current_Offset;
         Match_Result : Match_Type;
      begin
         Text := Null_Range;

         loop
            if
               Data_Overflow
               or Current_Offset = Natural'Last
            then
               Restore_Offset (Old_Offset);
               return;
            end if;

            Match_String (End_String, Match_Result);
            exit when Match_Result = Match_OK;

            Current_Offset := Current_Offset + 1;

            pragma Loop_Variant (Increases => Current_Offset);
            pragma Loop_Invariant (Data'First <= Natural'Last - Current_Offset);
            pragma Loop_Invariant (Current_Offset < Natural'Last);
            pragma Loop_Invariant (Current_Offset > Old_Offset);

         end loop;

         if
            Old_Offset > Current_Offset - End_String'Length - 1
            or Data'First > Natural'Last - Current_Offset
         then
            Restore_Offset (Old_Offset);
            return;
         end if;

         Text := (First, Data'First + Current_Offset - End_String'Length - 1);

      end Match_Until_String;

      ---------------------
      -- Match_Until_Set --
      ---------------------

      procedure Match_Until_Set (End_Set     :     Set_Type;
                                 Invalid_Set :     Set_Type;
                                 Match       : out Match_Type;
                                 Set_Range   : out Range_Type) with
        Pre  => Data'First <= Data'Last - Current_Offset,
        Post => (case Match is
                 when Match_OK   => In_Range (Set_Range)
                                    and Valid_Content (Set_Range.First, Set_Range.Last)
                                    and Current_Offset > Current_Offset'Old,
                 when Match_None => Set_Range = Null_Range
                                    and Current_Offset >= Current_Offset'Old,
                 when others     => Current_Offset = Current_Offset'Old);

      procedure Match_Until_Set (End_Set     :     Set_Type;
                                 Invalid_Set :     Set_Type;
                                 Match       : out Match_Type;
                                 Set_Range   : out Range_Type)
      is
         Old_Offset : constant Natural := Current_Offset;
         First      : constant Natural := Data'First + Current_Offset;
         Last       : Natural;
         Tmp_Match  : Match_Type;
      begin
         Match     := Match_Invalid;
         Set_Range := Null_Range;

         loop
            if Data_Overflow then
               Restore_Offset (Old_Offset);
               return;
            end if;

            Match_Set (End_Set, Invalid_Set, Tmp_Match);
            exit when Tmp_Match /= Match_None;

            pragma Loop_Variant (Increases => Current_Offset);
            pragma Loop_Invariant (Current_Offset > Old_Offset);
         end loop;

         if
            Tmp_Match /= Match_OK
            or Data'First > Data'Last - Current_Offset + 1
         then
            Restore_Offset (Old_Offset);
            return;
         end if;

         Current_Offset := Current_Offset - 1;
         Last   := Data'First + Current_Offset - 1;

         if Old_Offset <= Current_Offset - 1 then
            Set_Range := (First, Last);
            Match     := Match_OK;
         else
            Set_Range := Null_Range;
            Match     := Match_None;
         end if;

      end Match_Until_Set;

      ----------
      -- Skip --
      ----------

      procedure Skip (Skip_Set : Set_Type) with
        Post => Current_Offset >= Current_Offset'Old;

      procedure Skip (Skip_Set : Set_Type)
      is
         Last_None_Whitespace : Natural;
         Match : Match_Type;
         Unused_Result : Character;
         pragma Unreferenced (Unused_Result);
      begin
         loop
            pragma Loop_Variant (Increases => Current_Offset);
            pragma Loop_Invariant (Current_Offset >= Current_Offset'Loop_Entry);

            if Data_Overflow then
               return;
            end if;

            Last_None_Whitespace := Current_Offset;
            Match_Set (Skip_Set, Empty_Set, Match, Unused_Result);
            exit when Match /= Match_OK;
         end loop;
         Current_Offset := Last_None_Whitespace;
      end Skip;

      ---------------------
      -- Parse_Attribute --
      ---------------------

      procedure Parse_Attribute (Start : out Index_Type;
                                 Match : out Match_Type) with
        Post => (if Match = Match_OK
                 then Current_Offset > Current_Offset'Old
                 else Current_Offset = Current_Offset'Old);

      procedure Parse_Attribute (Start : out Index_Type;
                                 Match : out Match_Type)
      is
         Old_Offset          : constant Natural := Current_Offset;
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

         --  Need to use & operator of base type (Componolit/Workarounds#11)
         Match_Until_Set (Set_Type (String (Whitespace) & "="), ">", Match_Tmp, Attribute_Name);
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
         Current_Offset := Current_Offset + 1;

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
                 then In_Range (Name) and Current_Offset > Current_Offset'Old
                 else Current_Offset = Current_Offset'Old);

      procedure Parse_Opening_Tag (Match : out Match_Type;
                                   Name  : out Range_Type;
                                   Start : out Index_Type;
                                   Done  : out Boolean)
      is
         Old_Index          : constant Index_Type := Document_Index;
         Old_Offset         : constant Natural := Current_Offset;
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
         --
         --  Need to use & operator of base type (Componolit/Workarounds#11)
         Match_Until_Set (Set_Type (String (Whitespace) & ">/"), Empty_Set, Match_Tmp, Name);
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

            pragma Loop_Variant (Increases => Current_Offset);
            pragma Loop_Invariant (Current_Offset > Old_Offset);

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
        Post => (if Match = Match_OK then Current_Offset > Current_Offset'Old else Current_Offset = Current_Offset'Old);

      procedure Parse_Closing_Tag (Name  :     String;
                                   Match : out Match_Type)
      is
         Old_Offset   : constant Natural := Current_Offset;
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

         --  Need to use & operator of base type (Componolit/Workarounds#11)
         Match_Until_Set (Set_Type (String (Whitespace) & ">"), Empty_Set, Match_Tmp, Closing_Name);
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
                                Sec_Range : out Range_Type) with
        Pre  => Start_Tag'Length > 0 and
                End_Tag'Length > 0,
        Post => (if Sec_Range /= Null_Range
                 then Current_Offset > Current_Offset'Old
                 else Current_Offset >= Current_Offset'Old);

      procedure Parse_Sections (Start_Tag :     String;
                                End_Tag   :     String;
                                Sec_Range : out Range_Type)
      is
         Old_Offset : Natural;
         Tmp_Result : Match_Type;
      begin
         Sec_Range := Null_Range;
         loop
            Old_Offset := Current_Offset;
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

            Match_Until_String (End_Tag, Sec_Range);
            if Sec_Range = Null_Range then
               Restore_Offset (Old_Offset);
               return;
            end if;

            pragma Loop_Variant (Increases => Current_Offset);
            pragma Loop_Invariant (Current_Offset > Current_Offset'Loop_Entry);
         end loop;
      end Parse_Sections;

      -------------------
      -- Parse_Comment --
      -------------------

      procedure Parse_Comment (Match : out Match_Type) with
        Post => (if Match = Match_OK
                 then Current_Offset > Current_Offset'Old
                 else Current_Offset >= Current_Offset'Old);

      procedure Parse_Comment (Match : out Match_Type)
      is
         Tmp_Result : Range_Type;
      begin
         Parse_Sections ("<!--", "-->", Tmp_Result);
         Match := (if Tmp_Result = Null_Range then Match_None else Match_OK);
      end Parse_Comment;

      ----------------------------------
      -- Parse_Processing_Information --
      ----------------------------------

      procedure Parse_Processing_Information (Match : out Match_Type) with
        Post => (if Match = Match_OK
                 then Current_Offset > Current_Offset'Old
                 else Current_Offset >= Current_Offset'Old);

      procedure Parse_Processing_Information (Match : out Match_Type)
      is
         Tmp_Result : Range_Type;
      begin
         Parse_Sections ("<?", "?>", Tmp_Result);
         Match := (if Tmp_Result = Null_Range then Match_None else Match_OK);
      end Parse_Processing_Information;

      -------------------
      -- Parse_Doctype --
      -------------------

      procedure Parse_Doctype (Match : out Match_Type) with
        Post => (if Match = Match_OK
                 then Current_Offset > Current_Offset'Old
                 else Current_Offset = Current_Offset'Old);

      procedure Parse_Doctype (Match : out Match_Type)
      is
         Old_Offset   : constant Natural := Current_Offset;
         Tmp_Result   : Match_Type;
         Unused_Range : Range_Type;
         Text         : Range_Type;
      begin
         Match := Match_Invalid;
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
         Match := Match_OK;
      end Parse_Doctype;

      -------------------
      -- Parse_Section --
      -------------------

      procedure Parse_Sections with
        Post => Current_Offset >= Current_Offset'Old;

      procedure Parse_Sections
      is
         Match_Doctype : Match_Type;
         Match_Comment : Match_Type;
         Match_PI      : Match_Type;
         Old_Offset    : constant Natural := Current_Offset with Ghost;
      begin
         loop
            Parse_Doctype (Match_Doctype);
            Parse_Comment (Match_Comment);
            Parse_Processing_Information (Match_PI);

            exit when Match_Doctype /= Match_OK and Match_Comment /= Match_OK and Match_PI /= Match_OK;

            pragma Loop_Variant (Increases => Current_Offset);
            pragma Loop_Invariant (Current_Offset >= Old_Offset);

         end loop;
      end Parse_Sections;

      --------------------
      -- Parse_Internal --
      --------------------

      procedure Parse_Internal (Match : out Match_Type;
                                Start : out Index_Type) with
        Post => (if Match = Match_OK
                 then Current_Offset > Current_Offset'Old
                 else Current_Offset >= Current_Offset'Old);

      -----------------
      -- Parse_CDATA --
      -----------------

      procedure Parse_CDATA (Match : out Match_Type;
                             Start : out Index_Type)
      with
        Post => (if Match = Match_OK
                 then Current_Offset > Current_Offset'Old
                 else Current_Offset >= Current_Offset'Old);

      procedure Parse_CDATA (Match : out Match_Type;
                             Start : out Index_Type)
      is
         Tmp_Result : Range_Type;
         Tmp_Start  : Index_Type;
         Valid      : Boolean;
      begin
         Match := Match_None;
         Start := Invalid_Index;

         Parse_Sections ("<![CDATA[", "]]>", Tmp_Result);
         if
            Tmp_Result /= Null_Range
            and then Tmp_Result.First >= Data'First
            and then Tmp_Result.Last <= Data'Last
            and then Valid_Content (Tmp_Result.First, Tmp_Result.Last)
         then
            Context_Put_String (Value => Data (Tmp_Result.First .. Tmp_Result.Last),
                                Start => Tmp_Start,
                                Valid => Valid);
            if Valid then
               Start := Tmp_Start;
               Match := Match_OK;
            else
               Match := Match_Invalid;
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
        Post => (if Match = Match_OK
                 then Current_Offset > Current_Offset'Old
                 else Current_Offset >= Current_Offset'Old);

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
            Context_Put_String (Value => Data (Content_Range.First .. Content_Range.Last),
                                Start => Start,
                                Valid => Valid);
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
         Old_Offset : constant Natural := Current_Offset;

         Frame  : Frame_Type;
         Output : Out_Type;
         Limit  : Natural := Natural'Last;
      begin
         Call_Stack.Initialize (CS);
         Result_Stack.Initialize (RS);
         Call_Stack.Push (CS, (True, Null_Local));

         Start := Invalid_Index;
         Match := Match_Invalid;
         loop
            Call_Return : loop

               if Limit < 2 or Call_Stack.Is_Empty (CS) then
                  if Match = Match_OK and Current_Offset <= Old_Offset then
                     Match := Match_Invalid;
                  end if;
                  return;
               end if;
               Call_Stack.Pop (CS, Frame);
               case Frame.Local.State is

                  when Call_Start =>
                     Frame.Local.Old_Offset := Current_Offset;
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
                     Call_Stack.Push (CS, Frame);

                  when Loop_Start =>

                     Frame.Local.State := Recurse_End;
                     Call_Stack.Push (CS, Frame);

                     if Call_Stack.Is_Full (CS) or Result_Stack.Is_Full (RS) then
                        Match := Match_Depth_Limit;
                        return;
                     end if;

                     Call_Stack.Push (CS, (False, Null_Local));
                     Result_Stack.Push (RS, (Match, Start));

                  when Recurse_End =>

                     Frame.Local.Sub_Match := Match;
                     Frame.Local.Child_Start := Start;

                     if Result_Stack.Is_Empty (RS) or Call_Stack.Is_Full (CS) then
                        Match := Match_Invalid;
                        return;
                     end if;

                     Result_Stack.Pop (RS, Output);
                     Match := Output.Match;
                     Start := Output.Start;

                     if Frame.Local.Sub_Match /= Match_OK then
                        Frame.Local.State := Loop_End;
                        Call_Stack.Push (CS, Frame);
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
                     Call_Stack.Push (CS, Frame);

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
                        if Current_Offset < Old_Offset then
                           Current_Offset := Old_Offset;
                        end if;
                        exit Call_Return;
                     end if;

                     Parse_Sections;
                     Match := Match_OK;

               end case;

               Limit := Limit - 1;

               pragma Loop_Invariant (Current_Offset >= Old_Offset);
               pragma Loop_Invariant (Limit < Limit'Loop_Entry);
               pragma Loop_Variant (Decreases => Limit);

            end loop Call_Return;

            Limit := Limit - 1;

            pragma Loop_Invariant (Current_Offset >= Old_Offset);
            pragma Loop_Variant (Decreases => Limit);
         end loop;

      end Parse_Internal;

      --------------------------
      -- Skip_Byte_Order_Mark --
      --------------------------

      procedure Skip_Byte_Order_Mark with
         Post => Current_Offset >= Current_Offset'Old;

      procedure Skip_Byte_Order_Mark
      is
      begin
         --  Too little space for BOM
         if
            Current_Offset > Data'Length - 3
            or else Data'First > Data'Last - Current_Offset - 3
         then
            return;
         end if;

         --  We only support UTF-8 BOM
         if
            Data (Data'First + Current_Offset .. Data'First + Current_Offset + 2)
            = Character'Val (16#ef#) & Character'Val (16#bb#) & Character'Val (16#bf#)
         then
            Current_Offset := Current_Offset + 3;
         end if;

      end Skip_Byte_Order_Mark;

   begin
      Skip_Byte_Order_Mark;
      Parse_Internal (Result, Unused);
      pragma Unreferenced (Unused);
      Skip (Whitespace);

      if Result = Match_OK then
         if Current_Offset < Data'Length then
            Result := Match_Trailing_Data;
         end if;
         Offset := Current_Offset;
      else
         Offset := Error_Index;
      end if;

      if Offset >= Data'Length then
         Offset := Data'Length - 1;
      end if;

      if Document_Index in Document'Range
      then
         Document (Document_Index) := Null_Node;
      end if;
   end Parse;

begin -- SXML.Generic_Parser
   Call_Stack.Initialize (CS);
   Result_Stack.Initialize (RS);
end SXML.Generic_Parser;
