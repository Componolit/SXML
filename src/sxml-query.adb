package body SXML.Query is

   -----------
   -- Bound --
   -----------

   function Bound (Document : Subtree_Type;
                   State    : State_Type) return Boolean
   is
      pragma Unreferenced (Document, State);
   begin
      return True;
   end Bound;

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
   is
   begin
      return Get_String (Document, State.Offset);
   end Name;

   -----------
   -- Child --
   -----------

   procedure Child (State    : in out State_Type;
                    Document : Subtree_Type;
                    Result   : out Result_Type)
   is
      Children : constant Relative_Index_Type :=
        Document (Add (Document'First, State.Offset)).Children;
   begin
      if Children = Invalid_Relative_Index
      then
         Result := Result_Not_Found;
         return;
      end if;
      Result := Result_OK;
      State.Offset := Add (State.Offset, Children);
   end Child;

   -------------
   -- Sibling --
   -------------

   procedure Sibling (State    : in out State_Type;
                      Document : Subtree_Type;
                      Result   : out Result_Type)
   is
      Siblings : constant Relative_Index_Type :=
        Document (Add (Document'First, State.Offset)).Siblings;
   begin
      if Siblings = Invalid_Relative_Index
      then
         Result := Result_Not_Found;
         return;
      end if;
      Result := Result_OK;
      State.Offset := Add (State.Offset, Siblings);
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
   begin
      return Get_String (Document, Add (State.Offset, Val));
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
         exit when Result = Result_Not_Found;

      end loop;
   end Find_Sibling;

end SXML.Query;
