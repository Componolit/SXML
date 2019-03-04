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
   Scratch_Buffer : String (1 .. Scratch_Buffer_Length);

   ------------
   -- Offset --
   ------------

   function Offset (State : State_Type) return Offset_Type
   is (State.Offset);

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Document : Document_Type;
                      State    : State_Type) return Boolean
   is (Document'Length > 0 and
       State.Offset < Document'Length);

   -------------
   -- Is_Open --
   -------------

   function Is_Open (Document : Document_Type;
                     State    : State_Type) return Boolean
   is (Document (Add (Document'First, State.Offset)).Kind = Kind_Element_Open);

   ----------------
   -- Is_Content --
   ----------------

   function Is_Content (Document : Document_Type;
                        State    : State_Type) return Boolean
   is (Document (Add (Document'First, State.Offset)).Kind = Kind_Content);

   ------------------
   -- Is_Attribute --
   ------------------

   function Is_Attribute (Document : Document_Type;
                          State    : State_Type) return Boolean
   is (Document (Add (Document'First, State.Offset)).Kind = Kind_Attribute);

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

   procedure Name (State    : State_Type;
                   Document : Document_Type;
                   Result   : out Result_Type;
                   Data     : in out Content_Type;
                   Last     : out Natural)
   is
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
      if Document (Children_Offset).Kind /= Kind_Element_Open
      then
         return (Result => Result_Invalid);
      end if;
      Children := Document (Children_Offset).Children;
      if Offset_Type (Children) > Offset_Type'Last - State.Offset
      then
         return (Result => Result_Invalid);
      end if;
      if Children = Invalid_Relative_Index
      then
         return (Result => Result_Not_Found);
      end if;
      Tmp_Offset := Add (State.Offset, Children);
      if Tmp_Offset >= Document'Length or else
         (Document (Add (Document'First, Tmp_Offset)).Kind /= Kind_Element_Open and
          Document (Add (Document'First, Tmp_Offset)).Kind /= Kind_Content)
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
      if Siblings = Invalid_Relative_Index
      then
         return (Result => Result_Not_Found);
      end if;

      if Overflow (Current, Siblings)
      then
         return (Result => Result_Invalid);
      end if;

      Tmp_Index := Add (Current, Siblings);
      if not (Tmp_Index in Document'Range) or else
        (Document (Tmp_Index).Kind /= Kind_Element_Open and
         Document (Tmp_Index).Kind /= Kind_Content)
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
      Attributes : constant Relative_Index_Type :=
        Document (Add (Document'First, State.Offset)).Attributes;
   begin
      if Overflow (State.Offset, Attributes)
      then
         return (Result => Result_Invalid);
      end if;

      if Attributes = Invalid_Relative_Index
      then
         return (Result => Result_Not_Found);
      end if;

      Tmp_State := Add (State.Offset, Attributes);
      if Tmp_State >= Document'Length or else
         Document (Add (Document'First, Tmp_State)).Kind /= Kind_Attribute
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
      Next      : constant Relative_Index_Type :=
        Document (Add (Document'First, State.Offset)).Next_Attribute;
   begin
      if Overflow (State.Offset, Next)
      then
         return (Result => Result_Invalid);
      end if;

      if Next = Invalid_Relative_Index
      then
         return (Result => Result_Not_Found);
      end if;

      Tmp_State := Add (State.Offset, Next);
      if Tmp_State >= Document'Length or else
         Document (Add (Document'First, Tmp_State)).Kind /= Kind_Attribute
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
                            Document : Document_Type) return Boolean
   is (not Overflow (State.Offset, Document (Add (Document'First, State.Offset)).Value) and then
       Add (State.Offset, Document (Add (Document'First, State.Offset)).Value) < Document'Length);

   -----------
   -- Value --
   -----------

   procedure Value (State    : State_Type;
                    Document : Document_Type;
                    Result   : out Result_Type;
                    Data     : in out Content_Type;
                    Last     : out Natural)
   is
      Val : constant Relative_Index_Type :=
        Document (Add (Document'First, State.Offset)).Value;
   begin
      Get_String (Document, Add (State.Offset, Val), Result, Data, Last);
   end Value;

   ----------
   -- Path --
   ----------

   function Path (State        : State_Type;
                  Document     : Document_Type;
                  Query_String : String) return State_Type
   is
      First : Natural;
      Last  : Natural := Query_String'First - 1;
      Result_State : State_Type := State;
   begin

      if not Is_Open (Document, Result_State)
      then
         return (Result => Result_Not_Found);
      end if;

      loop
         exit when Last >= Query_String'Last - 1;
         First := Last + 2;
         Last  := First;

         pragma Loop_Variant (Increases => Result_State.Offset);
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

            exit when Last >= Query_String'Last or else
                      Query_String (Last + 1) = '/';
            Last := Last + 1;
         end loop;

         if First > Last or
            Last > Natural'Last - Chunk_Length
         then
            exit;
         end if;

         Result_State := Find_Sibling (Result_State, Document, Query_String (First .. Last));
         if Result_State.Result /= Result_OK
         then
            return (Result => Result_Not_Found);
         end if;

         --  Query string processed
         if Last = Query_String'Last
         then
            return Result_State;
         end if;

         Result_State := Child (Result_State, Document);
         if Result_State.Result /= Result_OK
         then
            return (Result => Result_Not_Found);
         end if;
      end loop;

      return Result_State;
   end Path;

   --------------------
   -- Find_Attribute --
   --------------------

   function Find_Attribute (State          : State_Type;
                            Document       : Document_Type;
                            Attribute_Name : Content_Type) return State_Type
   is
      Result       : Result_Type;
      Result_State : State_Type := Attribute (State, Document);
      Last         : Natural;
   begin
      if Attribute_Name'Length > Scratch_Buffer'Length
      then
         return (Result => Result_Overflow);
      end if;

      while Result_State.Result = Result_OK
      loop
         pragma Loop_Variant (Increases => Offset (Result_State));
         pragma Loop_Invariant (Is_Valid (Document, Result_State));
         pragma Loop_Invariant (Is_Attribute (Document, Result_State));

         Name (Result_State, Document, Result, Scratch_Buffer (1 .. Attribute_Name'Length), Last);
         if Result = Result_OK and then
            Last = Attribute_Name'Length and then
            Scratch_Buffer (1 .. Last) = Attribute_Name
         then
            return Result_State;
         end if;
         Result_State := Next_Attribute (Result_State, Document);
      end loop;

      return (Result => Result_Not_Found);
   end Find_Attribute;

   ------------------
   -- Find_Sibling --
   ------------------

   function Find_Sibling (State        : State_Type;
                          Document     : Document_Type;
                          Sibling_Name : Content_Type) return State_Type
   is
      Result_State : State_Type := State;
      Result       : Result_Type;
      Last         : Natural;
   begin
      if Sibling_Name'Length >= Scratch_Buffer'Length
      then
         return (Result => Result_Overflow);
      end if;

      loop
         if Is_Open (Document, Result_State)
         then
            Name (Result_State, Document, Result, Scratch_Buffer (1 .. Sibling_Name'Length), Last);
            if Result = Result_OK and then
               Last = Sibling_Name'Length and then
               Scratch_Buffer (1 .. Last) = Sibling_Name
            then
               return Result_State;
            end if;
         end if;
         Result_State := Sibling (Result_State, Document);
         exit when Result_State.Result /= Result_OK;

         pragma Loop_Variant (Increases => Result_State.Offset);
         pragma Loop_Invariant (Is_Valid (Document, Result_State) and then
                                  (Is_Open (Document, Result_State) or
                                   Is_Content (Document, Result_State)));
         pragma Loop_Invariant (Result_State.Offset > Result_State.Offset'Loop_Entry);
      end loop;

      return (Result => Result_Not_Found);
   end Find_Sibling;

end SXML.Query;
