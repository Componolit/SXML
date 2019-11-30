--
--  @summary XML query implementation
--  @author  Alexander Senier
--  @date    2018-11-15
--
--  Copyright (C) 2018 Componolit GmbH
--
--  This file is part of SXML, which is distributed under the terms of the
--  GNU Affero General Public License version 3.
--

package body SXML.Query
is
   pragma Annotate (GNATprove, Terminating, SXML.Query);

   --  Scratch buffer for queries. This will be the largest attribute size you can search for.
   Scratch_Buffer_Length : constant := 1024;

   ------------
   -- Offset --
   ------------

   function Offset (State : State_Type) return Offset_Type is
     (State.Offset);

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Document : Document_Type;
                      State    : State_Type) return Boolean is
     (Document'Length > 0 and (if State.Result = Result_OK then State.Offset < Document'Length));

   -------------
   -- Is_Open --
   -------------

   function Is_Open (Document : Document_Type;
                     State    : State_Type) return Boolean is
     (Document (Add (Document'First, State.Offset)).Kind = Kind_Element_Open);

   ----------------
   -- Is_Content --
   ----------------

   function Is_Content (Document : Document_Type;
                        State    : State_Type) return Boolean is
     (Document (Add (Document'First, State.Offset)).Kind = Kind_Content);

   ------------------
   -- Is_Attribute --
   ------------------

   function Is_Attribute (Document : Document_Type;
                          State    : State_Type) return Boolean is
     (Document (Add (Document'First, State.Offset)).Kind = Kind_Attribute);

   ----------
   -- Init --
   ----------

   function Init (Document : Document_Type) return State_Type
   is
      pragma Unreferenced (Document);
   begin
      return State_Type'(Result => Result_OK, Offset => 0);
   end Init;

   ----------
   -- Name --
   ----------

   procedure Name (State    :     State_Type;
                   Document :     Document_Type;
                   Result   : out Result_Type;
                   Data     : out Content_Type;
                   Last     : out Natural) is
   begin
      Get_String (Document, State.Offset, Result, Data, Last);
   end Name;

   -----------
   -- Child --
   -----------

   function Child (State    : State_Type;
                   Document : Document_Type) return State_Type
   is
      Children_Offset : constant Index_Type := Add (Document'First, State.Offset);
      Children        : Relative_Index_Type;
      Tmp_Offset      : Offset_Type;
   begin
      if Document (Children_Offset).Kind /= Kind_Element_Open then
         return (Result => Result_Invalid);
      end if;
      Children := Document (Children_Offset).Children;
      if Offset_Type (Children) > Offset_Type'Last - State.Offset then
         return (Result => Result_Invalid);
      end if;
      if Children = Invalid_Relative_Index then
         return (Result => Result_Not_Found);
      end if;
      Tmp_Offset := Add (State.Offset, Children);
      if
         Tmp_Offset >= Document'Length
         or else (Document (Add (Document'First, Tmp_Offset)).Kind /= Kind_Element_Open
                  and Document (Add (Document'First, Tmp_Offset)).Kind /= Kind_Content)
      then
         return (Result => Result_Invalid);
      end if;

      return (Result => Result_OK, Offset => Tmp_Offset);
   end Child;

   -------------
   -- Sibling --
   -------------

   function Sibling (State    : State_Type;
                     Document : Document_Type) return State_Type
   is
      Current  : constant Index_Type := Add (Document'First, State.Offset);
      Siblings : constant Relative_Index_Type := Document (Current).Siblings;
      Tmp_Index : Index_Type;
   begin
      if Siblings = Invalid_Relative_Index then
         return (Result => Result_Not_Found);
      end if;

      if Overflow (Current, Siblings) then
         return (Result => Result_Invalid);
      end if;

      Tmp_Index := Add (Current, Siblings);
      if
         not (Tmp_Index in Document'Range)
         or else (Document (Tmp_Index).Kind /= Kind_Element_Open
                  and Document (Tmp_Index).Kind /= Kind_Content)
      then
         return (Result => Result_Invalid);
      end if;

      return (Result => Result_OK,
              Offset => Sub (Tmp_Index, Document'First));
   end Sibling;

   ---------------
   -- Attribute --
   ---------------

   function Attribute (State    : State_Type;
                       Document : Document_Type) return State_Type
   is
      Tmp_State  : Offset_Type;
      Attributes : constant Relative_Index_Type := Document (Add (Document'First, State.Offset)).Attributes;
   begin
      if Overflow (State.Offset, Attributes) then
         return (Result => Result_Invalid);
      end if;

      if Attributes = Invalid_Relative_Index then
         return (Result => Result_Not_Found);
      end if;

      Tmp_State := Add (State.Offset, Attributes);

      if
         Tmp_State >= Document'Length
         or else Document (Add (Document'First, Tmp_State)).Kind /= Kind_Attribute
         or else Overflow (Tmp_State, Document (Add (Document'First, Tmp_State)).Value)
         or else Add (Tmp_State, Document (Add (Document'First, Tmp_State)).Value) >= Document'Length
      then
         return (Result => Result_Invalid);
      end if;

      return (Result => Result_OK,
              Offset => Tmp_State);
   end Attribute;

   --------------------
   -- Next_Attribute --
   --------------------

   function Next_Attribute (State    : State_Type;
                            Document : Document_Type) return State_Type
   is
      Tmp_State : Offset_Type;
      Next      : constant Relative_Index_Type := Document (Add (Document'First, State.Offset)).Next_Attribute;
   begin
      if Overflow (State.Offset, Next) then
         return (Result => Result_Invalid);
      end if;

      if Next = Invalid_Relative_Index then
         return (Result => Result_Not_Found);
      end if;

      Tmp_State := Add (State.Offset, Next);
      if
         Tmp_State >= Document'Length
         or else Document (Add (Document'First, Tmp_State)).Kind /= Kind_Attribute
         or else Overflow (Tmp_State, Document (Add (Document'First, Tmp_State)).Value)
         or else Add (Tmp_State, Document (Add (Document'First, Tmp_State)).Value) >= Document'Length
      then
         return (Result => Result_Invalid);
      end if;

      return (Result => Result_OK,
              Offset => Tmp_State);
   end Next_Attribute;

   --------------------
   -- Is_Valid_Value --
   --------------------

   function Is_Valid_Value (State    : State_Type;
                            Document : Document_Type) return Boolean is
     (not Overflow (State.Offset, Document (Add (Document'First, State.Offset)).Value)
      and then Add (State.Offset, Document (Add (Document'First, State.Offset)).Value) < Document'Length);

   -----------
   -- Value --
   -----------

   procedure Value (State    : State_Type;
                    Document : Document_Type;
                    Result   : out Result_Type;
                    Data     : out Content_Type;
                    Last     : out Natural)
   is
      Val : constant Relative_Index_Type := Document (Add (Document'First, State.Offset)).Value;
   begin
      Get_String (Document, Add (State.Offset, Val), Result, Data, Last);
   end Value;

   -----------------
   -- Split_Query --
   -----------------

   type Position_Type (Valid : Boolean) is
   record
      case Valid is
         when True =>
            Name_First  : Natural;
            Name_Last   : Natural;
            Value_First : Natural;
            Value_Last  : Natural;
         when others =>
            null;
      end case;
   end record;

   function Split_Query (Query_String : String;
                         First        : Natural) return Position_Type with
     Pre => First >= Query_String'First
            and First <= Query_String'Last
            and Query_String'Last < Natural'Last
            and Query_String'Length > 0,
     Post => (if
                 Split_Query'Result.Valid
              then
                 Split_Query'Result.Name_First >= Query_String'First
                 and Split_Query'Result.Name_Last <= Query_String'Last
                 and Split_Query'Result.Value_First >= Query_String'First
                 and Split_Query'Result.Value_Last <= Query_String'Last);

   function Split_Query (Query_String :     String;
                         First        :     Natural) return Position_Type
   is
      Eq_Pos : Natural := Query_String'Last;
      Len    : constant Natural := Query_String'Last - First + 1;
   begin
      --  Missing opening or closing bracket
      if
         Len < 2
         or else Query_String (First) /= '['
         or else Query_String (Query_String'Last)  /= ']'
      then
         return (Valid => False);
      end if;

      --  Empty expression always matches
      if Len = 2 then
         return (Valid       => True,
                 Name_First  => Query_String'Last,
                 Name_Last   => Query_String'First,
                 Value_First => Query_String'Last,
                 Value_Last  => Query_String'First);
      end if;

      if Query_String (First + 1) /= '@' then
         return (Valid => False);
      end if;

      --  Find position and ensure single occurrence of '='
      for I in First .. Query_String'Last
      loop
         pragma Loop_Invariant (Eq_Pos in Query_String'Range);
         if
            Eq_Pos /= Query_String'Last
            and Query_String (I) = '='
         then
            return (Valid => False);
         end if;
         if Query_String (I) = '=' then
            Eq_Pos := I;
         end if;
      end loop;

      return
         (Valid       => True,
          Name_First  => First + 2,
          Name_Last   => Eq_Pos - 1,
          Value_First => Eq_Pos + 1,
          Value_Last  => Query_String'Last - 1);

   end Split_Query;

   ------------------
   -- Path_Segment --
   ------------------

   function Path_Segment (State    : State_Type;
                          Document : Document_Type;
                          Segment  : String) return State_Type with
     Pre => Valid_Content (Segment'First, Segment'Last)
            and then State.Result = Result_OK
            and then Is_Valid (Document, State)
            and then (Is_Open (Document, State)
                      or Is_Content (Document, State)),
     Post => (if
                Path_Segment'Result.Result = Result_OK
              then
                Path_Segment'Result.Offset >= State.Offset
                and Path_Segment'Result.Offset < Document'Length);

   function Path_Segment (State    : State_Type;
                          Document : Document_Type;
                          Segment  : String) return State_Type
   is
      Attr_Start : Natural := Segment'Last + 1;
   begin
      for I in Segment'Range
      loop
         pragma Loop_Invariant (Attr_Start >= Segment'First);
         if Segment (I) = '[' then
            Attr_Start := I;
         end if;
      end loop;

      if Attr_Start > Segment'Last then
         return Find_Sibling (State        => State,
                              Document     => Document,
                              Sibling_Name => Segment);
      end if;

      declare
         Pos : constant Position_Type := Split_Query (Query_String => Segment (Attr_Start .. Segment'Last),
                                                      First        => Attr_Start);
      begin
         if not Pos.Valid then
            return (Result => Result_Invalid);
         end if;

         return Find_Sibling (State           => State,
                              Document        => Document,
                              Sibling_Name    => Segment (Segment'First .. Attr_Start - 1),
                              Attribute_Name  => Segment (Pos.Name_First .. Pos.Name_Last),
                              Attribute_Value => Segment (Pos.Value_First .. Pos.Value_Last));
      end;

   end Path_Segment;

   ----------
   -- Path --
   ----------

   function Path (State        : State_Type;
                  Document     : Document_Type;
                  Query_String : String) return State_Type
   is
      First        : Natural;
      Last         : Natural    := Query_String'First - 1;
      Result_State : State_Type := State;
   begin

      if not Is_Open (Document, Result_State) then
         return (Result => Result_Invalid);
      end if;

      loop
         exit when Last >= Query_String'Last - 1;
         pragma Assert (Last < Query_String'Last - 1);
         First := Last + 2;
         Last  := First;

         pragma Loop_Variant (Increases => Result_State.Offset);
         pragma Loop_Invariant (Result_State.Result = Result_OK);
         pragma Loop_Invariant (Is_Valid (Document, Result_State));
         pragma Loop_Invariant (Is_Open (Document, Result_State) or
                                Is_Content (Document, Result_State));
         pragma Loop_Invariant (First >= Query_String'First);
         pragma Loop_Invariant (Last >= Query_String'First);
         pragma Loop_Invariant (Last <= Query_String'Last);

         loop
            pragma Loop_Variant (Increases => Last);
            pragma Loop_Invariant (Result_State.Offset = Result_State.Offset'Loop_Entry);
            pragma Loop_Invariant (First >= Query_String'First);
            pragma Loop_Invariant (Last >= Query_String'First);
            pragma Loop_Invariant (Last <= Query_String'Last);

            exit when Last >= Query_String'Last or else Query_String (Last + 1) = '/';
            Last := Last + 1;
         end loop;

         exit when not Valid_Content (First, Last);

         Result_State := Path_Segment (Result_State, Document, Query_String (First .. Last));
         if Result_State.Result /= Result_OK then
            return Result_State;
         end if;

         exit when Last >= Query_String'Last;

         pragma Assert (Result_State.Offset < Document'Length);

         Result_State := Child (Result_State, Document);
         if Result_State.Result /= Result_OK then
            return (Result => Result_Not_Found);
         end if;
      end loop;

      return Result_State;
   end Path;

   --------------------
   -- Find_Attribute --
   --------------------

   function Find_Attribute (State           : State_Type;
                            Document        : Document_Type;
                            Attribute_Name  : String := "*";
                            Attribute_Value : String := "*") return State_Type
   is
      Result          : Result_Type;
      Result_State    : State_Type := Attribute (State, Document);
      Last            : Natural;
      Scratch_Buffer  : String (1 .. Scratch_Buffer_Length) := (others => ASCII.NUL);
      Attribute_Found : Boolean;
      Value_Matches   : Boolean;
   begin
      if Attribute_Name'Length > Scratch_Buffer'Length then
         return (Result => Result_Overflow);
      end if;

      while Result_State.Result = Result_OK
      loop
         pragma Loop_Variant (Increases => Offset (Result_State));
         pragma Loop_Invariant (Is_Valid (Document, Result_State));
         pragma Loop_Invariant (Is_Attribute (Document, Result_State));
         pragma Loop_Invariant (Valid_Content (Scratch_Buffer'First, Scratch_Buffer'Last));
         pragma Loop_Invariant (Is_Valid_Value (Result_State, Document));

         if
            Attribute_Name = "*"
            or Attribute_Name = ""
         then
            Attribute_Found := True;
         else
            pragma Assert (Valid_Content (1, Attribute_Name'Length));
            Name (Result_State, Document, Result, Scratch_Buffer (1 .. Attribute_Name'Length), Last);
            Attribute_Found :=
               Result = Result_OK
               and then Last = Attribute_Name'Length
               and then Scratch_Buffer (1 .. Last) = Attribute_Name;
         end if;

         if Attribute_Found
         then
            if
               Attribute_Value = "*"
               or Attribute_Value = ""
            then
               Value_Matches := True;
            elsif
               not Is_Valid_Value (Result_State, Document)
            then
               Value_Matches := False;
            else
               declare
                  R : Result_Type;
                  L : Natural;
               begin
                  Value (State    => Result_State,
                         Document => Document,
                         Result   => R,
                         Data     => Scratch_Buffer,
                         Last     => L);
                  Value_Matches := R = Result_OK and then Scratch_Buffer (1 .. L) = Attribute_Value;
               end;
            end if;
            if Value_Matches then
               return Result_State;
            end if;
         end if;
         Result_State := Next_Attribute (Result_State, Document);
      end loop;

      return (Result => Result_Not_Found);
   end Find_Attribute;

   ------------------
   -- Find_Sibling --
   ------------------

   function Find_Sibling (State           : State_Type;
                          Document        : Document_Type;
                          Sibling_Name    : String := "*";
                          Attribute_Name  : String := "*";
                          Attribute_Value : String := "*") return State_Type
   is
      Result_State   : State_Type := State;
      Attr_State     : State_Type;
      Result         : Result_Type;
      Last           : Natural;
      Scratch_Buffer : String (1 .. Scratch_Buffer_Length) := (others => ASCII.NUL);
   begin
      if Sibling_Name'Length >= Scratch_Buffer'Length then
         return (Result => Result_Overflow);
      end if;

      while Result_State.Result = Result_OK
      loop
         if Is_Open (Document, Result_State) then
            if
               Sibling_Name = "*"
               or Sibling_Name = ""
            then
               return Result_State;
            end if;

            pragma Assert (Valid_Content (1, Sibling_Name'Length));

            Name (Result_State, Document, Result, Scratch_Buffer (1 .. Sibling_Name'Length), Last);
            if
               Result = Result_OK
               and then Last = Sibling_Name'Length
               and then Scratch_Buffer (1 .. Last) = Sibling_Name
            then
               Attr_State := Find_Attribute (Result_State, Document, Attribute_Name, Attribute_Value);
               if
                  ((Attribute_Name = "*" or Attribute_Name = "") and (Attribute_Value = "*"  or Attribute_Value = ""))
                  or Attr_State.Result = Result_OK
               then
                  return Result_State;
               end if;
            end if;
         end if;

         pragma Loop_Variant (Increases => Result_State.Offset);
         pragma Loop_Invariant (Is_Valid (Document, Result_State));
         pragma Loop_Invariant (Result_State.Result = Result_OK);
         pragma Loop_Invariant (Offset (Result_State) >= Offset (Result_State'Loop_Entry));
         pragma Loop_Invariant (Is_Open (Document, Result_State) or Is_Content (Document, Result_State));

         Result_State := Sibling (Result_State, Document);
      end loop;

      return (Result => Result_Not_Found);
   end Find_Sibling;

   -------------------
   -- Has_Attribute --
   -------------------

   function Has_Attribute (State          : State_Type;
                           Document       : Document_Type;
                           Attribute_Name : String) return Boolean is
     (Find_Attribute (State, Document, Attribute_Name).Result = Result_OK);

   ---------------
   -- Attribute --
   ---------------

   procedure Attribute (State          :     State_Type;
                        Document       :     Document_Type;
                        Attribute_Name :     String;
                        Result         : out Result_Type;
                        Data           : out Content_Type;
                        Last           : out Natural)
   is
      S : constant State_Type := Find_Attribute (State, Document, Attribute_Name);
   begin
      Last   := 0;
      Result := Result_Invalid;

      for I in Data'Range
      loop
         Data (I) := Character'Val (0);
      end loop;

      if S.Result /= Result_OK then
         return;
      end if;

      declare
         Val : constant Relative_Index_Type := Document (Add (Document'First, S.Offset)).Value;
         Len : constant Natural := String_Length (Document, Add (S.Offset, Val));
      begin
         if
           Len > Data'Length
           or else Data'First > Data'Last - Len
           or else Data'First > Natural'Last
         then
            return;
         end if;

         Value (S, Document, Result, Data (Data'First .. Data'First + Len), Last);
      end;

   end Attribute;

end SXML.Query;
