package body SXML.Query
is
   pragma Annotate (GNATprove, Terminating, SXML.Query);

   --  Scratch buffer for queries. This will be the largest attribute size you can search for.
   Scratch_Buffer : String (1 .. SXML.Scratch_Buffer_Length);

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
      return State_Type'(Offset => 0);
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

   procedure Child (State    : in out State_Type;
                    Document : Document_Type;
                    Result   : out Result_Type)
   is
      Children_Offset : constant Index_Type := Add (Document'First, State.Offset);
      Children        : Relative_Index_Type;
      Tmp_Offset      : Offset_Type;
   begin
      Result := Result_Invalid;
      if Document (Children_Offset).Kind /= Kind_Element_Open
      then
         return;
      end if;
      Children := Document (Children_Offset).Children;
      if Offset_Type (Children) > Offset_Type'Last - State.Offset
      then
         return;
      end if;
      if Children = Invalid_Relative_Index
      then
         Result := Result_Not_Found;
         return;
      end if;
      Tmp_Offset := Add (State.Offset, Children);
      if Tmp_Offset >= Document'Length or else
         (Document (Add (Document'First, Tmp_Offset)).Kind /= Kind_Element_Open and
          Document (Add (Document'First, Tmp_Offset)).Kind /= Kind_Content)
      then
         return;
      end if;

      Result := Result_OK;
      State.Offset := Tmp_Offset;
   end Child;

   -------------
   -- Sibling --
   -------------

   procedure Sibling (State    : in out State_Type;
                      Document : Document_Type;
                      Result   : out Result_Type)
   is
      Current  : constant Index_Type := Add (Document'First, State.Offset);
      Siblings : constant Relative_Index_Type := Document (Current).Siblings;
      Tmp_Index : Index_Type;
   begin
      if Siblings = Invalid_Relative_Index
      then
         Result := Result_Not_Found;
         return;
      end if;

      Result := Result_Invalid;
      if Overflow (Current, Siblings)
      then
         return;
      end if;

      Tmp_Index := Add (Current, Siblings);
      if not (Tmp_Index in Document'Range) or else
        (Document (Tmp_Index).Kind /= Kind_Element_Open and
         Document (Tmp_Index).Kind /= Kind_Content)
      then
         return;
      end if;

      State.Offset := Sub (Tmp_Index, Document'First);
      Result := Result_OK;
   end Sibling;

   ---------------
   -- Attribute --
   ---------------

   procedure Attribute (State    : in out State_Type;
                        Document : Document_Type;
                        Result   : out Result_Type)
   is
      Tmp_State  : Offset_Type;
      Attributes : constant Relative_Index_Type :=
        Document (Add (Document'First, State.Offset)).Attributes;
   begin
      Result := Result_Invalid;
      if Overflow (State.Offset, Attributes)
      then
         return;
      end if;

      if Attributes = Invalid_Relative_Index
      then
         Result := Result_Not_Found;
         return;
      end if;

      Tmp_State := Add (State.Offset, Attributes);
      if Tmp_State >= Document'Length or else
         Document (Add (Document'First, Tmp_State)).Kind /= Kind_Attribute
      then
         return;
      end if;

      State.Offset := Tmp_State;
      Result := Result_OK;
   end Attribute;

   --------------------
   -- Next_Attribute --
   --------------------

   procedure Next_Attribute (State    : in out State_Type;
                             Document : Document_Type;
                             Result   : out Result_Type)
   is
      Tmp_State : Offset_Type;
      Next      : constant Relative_Index_Type :=
        Document (Add (Document'First, State.Offset)).Next_Attribute;
   begin
      Result := Result_Invalid;
      if Overflow (State.Offset, Next)
      then
         return;
      end if;

      if Next = Invalid_Relative_Index
      then
         Result := Result_Not_Found;
         return;
      end if;

      Tmp_State := Add (State.Offset, Next);
      if Tmp_State >= Document'Length or else
         Document (Add (Document'First, Tmp_State)).Kind /= Kind_Attribute
      then
         return;
      end if;

      Result := Result_OK;
      State.Offset := Tmp_State;
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

   procedure Path (State        : in out State_Type;
                   Document     : Document_Type;
                   Result       : out Result_Type;
                   Query_String : String)
   is
      First : Natural;
      Last  : Natural := Query_String'First - 1;
      Tmp_Result : Result_Type;
   begin
      Result := Result_Not_Found;
      State  := Init (Document);

      if not Is_Open (Document, State)
      then
         return;
      end if;

      loop
         exit when Last >= Query_String'Last - 1;
         First := Last + 2;
         Last  := First;

         pragma Loop_Variant (Increases => State.Offset);
         pragma Loop_Invariant (Is_Valid (Document, State));
         pragma Loop_Invariant (Is_Open (Document, State) or
                                Is_Content (Document, State));
         pragma Loop_Invariant (First >= Query_String'First);
         pragma Loop_Invariant (Last >= Query_String'First);
         pragma Loop_Invariant (Last <= Query_String'Last);

         loop
            pragma Loop_Variant (Increases => Last);
            pragma Loop_Invariant (State.Offset = State.Offset'Loop_Entry);
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

         State.Find_Sibling (Document, Query_String (First .. Last), Tmp_Result);
         if Tmp_Result /= Result_OK
         then
            return;
         end if;

         --  Query string processed
         if Last = Query_String'Last
         then
            Result := Result_OK;
            return;
         end if;

         State.Child (Document, Tmp_Result);
         if Tmp_Result /= Result_OK
         then
            return;
         end if;
      end loop;

      Result := Result_OK;
   end Path;

   --------------------
   -- Find_Attribute --
   --------------------

   procedure Find_Attribute (State          : in out State_Type;
                             Document       : Document_Type;
                             Attribute_Name : Content_Type;
                             Result         : out Result_Type)
   is
      Tmp_Result : Result_Type;
      Tmp_Last   : Natural;
   begin
      Result := Result_Not_Found;

      if Attribute_Name'Length > Scratch_Buffer'Length
      then
         Result := Result_Overflow;
         return;
      end if;

      State.Attribute (Document, Tmp_Result);

      while Tmp_Result = Result_OK
      loop
         pragma Loop_Variant (Increases => State.Offset);
         pragma Loop_Invariant (Is_Valid (Document, State));
         pragma Loop_Invariant (Is_Attribute (Document, State));

         State.Name (Document, Tmp_Result, Scratch_Buffer (1 .. Attribute_Name'Length), Tmp_Last);
         if Tmp_Result = Result_OK and then
            Tmp_Last = Attribute_Name'Length and then
            Scratch_Buffer (1 .. Tmp_Last) = Attribute_Name
         then
            Result := Result_OK;
            return;
         end if;
         State.Next_Attribute (Document, Tmp_Result);
      end loop;

   end Find_Attribute;

   ------------------
   -- Find_Sibling --
   ------------------

   procedure Find_Sibling (State        : in out State_Type;
                           Document     : Document_Type;
                           Sibling_Name : Content_Type;
                           Result       : out Result_Type)
   is
      Old_State : constant State_Type := State;
      Tmp_Last  : Natural;
   begin
      if Sibling_Name'Length >= Scratch_Buffer'Length
      then
         Result := Result_Overflow;
         return;
      end if;

      loop
         if Is_Open (Document, State)
         then
            State.Name (Document, Result, Scratch_Buffer (1 .. Sibling_Name'Length), Tmp_Last);
            if Result = Result_OK and then
               Tmp_Last = Sibling_Name'Length and then
               Scratch_Buffer (1 .. Tmp_Last) = Sibling_Name
            then
               return;
            end if;
         end if;
         State.Sibling (Document, Result);
         exit when Result /= Result_OK;

         pragma Loop_Variant (Increases => State.Offset);
         pragma Loop_Invariant (Is_Valid (Document, State) and then
                                  (Is_Open (Document, State) or
                                   Is_Content (Document, State)));
         pragma Loop_Invariant (State.Offset > State.Offset'Loop_Entry);
      end loop;
      State := Old_State;
   end Find_Sibling;

end SXML.Query;
