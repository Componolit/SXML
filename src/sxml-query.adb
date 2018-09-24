package body SXML.Query is

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Document : Subtree_Type;
                      State    : State_Type) return Boolean
   is (Document'Length > 0 and
       State.Offset < Document'Length);

   -------------
   -- Is_Open --
   -------------

   function Is_Open (Document : Subtree_Type;
                     State    : State_Type) return Boolean
   is (Document (Add (Document'First, State.Offset)).Kind = Kind_Element_Open);

   ----------------
   -- Is_Content --
   ----------------

   function Is_Content (Document : Subtree_Type;
                        State    : State_Type) return Boolean
   is (Document (Add (Document'First, State.Offset)).Kind = Kind_Content);

   ------------------
   -- Is_Attribute --
   ------------------

   function Is_Attribute (Document : Subtree_Type;
                          State    : State_Type) return Boolean
   is (Document (Add (Document'First, State.Offset)).Kind = Kind_Attribute);

   ----------
   -- Init --
   ----------

   function Init (Document : Subtree_Type) return State_Type
   is
      pragma Unreferenced (Document);
   begin
      return State_Type'(Offset => 0);
   end Init;

   ----------
   -- Name --
   ----------

   function Name (State    : State_Type;
                  Document : Subtree_Type) return String
   is (Get_String (Document, State.Offset));

   -----------
   -- Child --
   -----------

   procedure Child (State    : in out State_Type;
                    Document : Subtree_Type;
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
      if Tmp_Offset >= Document'Length
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
                      Document : Subtree_Type;
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
                        Document : Subtree_Type;
                        Result   : out Result_Type)
   is
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
      Result := Result_OK;
      State.Offset := Add (State.Offset, Attributes);
   end Attribute;

   --------------------
   -- Next_Attribute --
   --------------------

   procedure Next_Attribute (State    : in out State_Type;
                             Document : Subtree_Type;
                             Result   : out Result_Type)
   is
      Next : constant Relative_Index_Type :=
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
      Result := Result_OK;
      State.Offset := Add (State.Offset, Next);
   end Next_Attribute;

   -----------
   -- Value --
   -----------

   function Value (State    : State_Type;
                   Document : Subtree_Type) return String
   is
      Val : constant Relative_Index_Type :=
        Document (Add (Document'First, State.Offset)).Value;
      Tmp_Offset : Offset_Type;
   begin
      if Overflow (State.Offset, Val)
      then
         --  FIXME: Refactor interface to allow proper error reporting
         return "";
      end if;

      Tmp_Offset := Add (State.Offset, Val);
      if Tmp_Offset >= Document'Length
      then
         --  FIXME: Refactor interface to allow proper error reporting
         return "";
      end if;

      return Get_String (Document, Tmp_Offset);
   end Value;

   ----------
   -- Path --
   ----------

   procedure Path (State        : in out State_Type;
                   Document     : Subtree_Type;
                   Result       : out Result_Type;
                   Query_String : String)
   is
      First : Natural := Query_String'First;
      Last  : Natural := Query_String'First - 1;
      Tmp_Result : Result_Type;
   begin
      Result := Result_Not_Found;
      State := Init (Document);

      loop
         First := Last + 2;
         Last  := First;
         loop
            exit when Last >= Query_String'Last or else
                      Query_String (Last + 1) = '/';
            Last := Last + 1;
         end loop;

         if  First > Last
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
                             Document       : Subtree_Type;
                             Attribute_Name : String;
                             Result         : out Result_Type)
   is
      Tmp_Result : Result_Type;
   begin
      Result := Result_Not_Found;
      State.Attribute (Document, Tmp_Result);

      while Tmp_Result = Result_OK
      loop
         if State.Name (Document) = Attribute_Name
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
                           Document     : Subtree_Type;
                           Sibling_Name : String;
                           Result       : out Result_Type)
   is
   begin
      loop
         if State.Name (Document) = Sibling_Name
         then
            Result := Result_OK;
            return;
         end if;
         State.Sibling (Document, Result);
         exit when Result /= Result_OK;

      end loop;
   end Find_Sibling;

end SXML.Query;
