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
      Children : constant Offset_Type :=
        Document (Document'First + State.Offset).Children;
   begin
      if Children = Null_Offset
      then
         Result := Result_Not_Found;
         return;
      end if;
      Result := Result_OK;
      State.Offset := State.Offset + Children;
   end Child;

   -------------
   -- Sibling --
   -------------

   procedure Sibling (State    : in out State_Type;
                      Document : Subtree_Type;
                      Result   : out Result_Type)
   is
      Siblings : constant Offset_Type :=
        Document (Document'First + State.Offset).Siblings;
   begin
      if Siblings = Null_Offset
      then
         Result := Result_Not_Found;
         return;
      end if;
      Result := Result_OK;
      State.Offset := State.Offset + Siblings;
   end Sibling;

   ---------------
   -- Attribute --
   ---------------

   procedure Attribute (State    : in out State_Type;
                        Document : Subtree_Type;
                        Result   : out Result_Type)
   is
      Attributes : constant Offset_Type :=
        Document (Document'First + State.Offset).Attributes;
   begin
      if Attributes = Null_Offset
      then
         Result := Result_Not_Found;
         return;
      end if;
      Result := Result_OK;
      State.Offset := State.Offset + Attributes;
   end Attribute;

   -----------
   -- Value --
   -----------

   function Value (State    : State_Type;
                   Document : Subtree_Type) return String
   is
      Val : constant Offset_Type :=
        Document (Document'First + State.Offset).Value;
   begin
      return Get_String (Document, State.Offset + Val);
   end Value;

end SXML.Query;
