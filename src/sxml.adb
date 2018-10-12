package body SXML
is
   Null_Open_Element : constant Node_Type :=
     Node_Type'(Kind           => Kind_Element_Open,
                Length         => 0,
                Next           => Invalid_Relative_Index,
                Data           => Null_Data,
                Attributes     => Invalid_Relative_Index,
                Children       => Invalid_Relative_Index,
                Siblings       => Invalid_Relative_Index);

   Null_Data_Element : constant Node_Type :=
     Node_Type'(Kind           => Kind_Data,
                Length         => 0,
                Next           => Invalid_Relative_Index,
                Data           => Null_Data);

   Null_Attribute_Element : constant Node_Type :=
     Node_Type'(Kind           => Kind_Attribute,
                Length         => 0,
                Next           => Invalid_Relative_Index,
                Next_Attribute => Invalid_Relative_Index,
                Data           => Null_Data,
                Value          => Invalid_Relative_Index);

   Null_Content_Element : constant Node_Type :=
     Node_Type'(Kind           => Kind_Content,
                Length         => 0,
                Next           => Invalid_Relative_Index,
                Data           => Null_Data,
                Attributes     => Invalid_Relative_Index,
                Children       => Invalid_Relative_Index,
                Siblings       => Invalid_Relative_Index);

   ------------------
   -- Num_Elements --
   ------------------

   function Num_Elements (D : Content_Type) return Offset_Type
   is ((Offset_Type (D'Length + Data_Type'Length - 1)) / Offset_Type (Data_Type'Length));

   function Num_Attr_Elements (D : Attr_Data_Type) return Offset_Type
   is ((Offset_Type (D'Length + Data_Type'Length - 1)) / Offset_Type (Data_Type'Length));

   function Num_Elements (Subtree : Subtree_Type) return Offset_Type
   is (Offset_Type (Subtree'Length));

   ----------------
   -- Put_String --
   ----------------

   procedure Put_String (Subtree : in out Subtree_Type;
                         Offset  : Offset_Type;
                         Name    : Attr_Data_Type)
   is
      Position : Natural := 0;
      Len      : Natural;
      NE       : constant Offset_Type := Num_Attr_Elements (Name);
   begin
      if NE = 0
      then
         return;
      end if;

      for I in Index_Type range
        Add (Subtree'First, Offset) .. Add (Add (Subtree'First, Offset), NE) - 1
      loop
         pragma Loop_Invariant (Position <= Name'Length);
         if Name'Length - Position > Subtree (I).Data'Length
         then
            Len := Data_Type'Length;
            Subtree (I).Next := 1;
         else
            Len := Name'Length - Position;
         end if;
         Subtree (I).Data (1 .. Len) :=
           Name (Name'First + Position .. Name'First + Position + Len - 1);
         Subtree (I).Length := Length_Type (Len);
         Position := Position + Len;
      end loop;
   end Put_String;

   ----------
   -- Open --
   ----------

   procedure Open (Name     : Content_Type;
                   Output   : in out Subtree_Type;
                   Position : in out Index_Type;
                   Start    : out Index_Type)
   is
   begin
      Start          := Position;
      Output (Start) := Null_Open_Element;
      Put_String (Output, Sub (Start, Output'First), Name);
      Position := Add (Position, Num_Elements (Name));
   end Open;

   -----------------
   -- Put_Content --
   -----------------

   procedure Put_Content (Subtree : in out Subtree_Type;
                          Offset  : Offset_Type;
                          Value   : Content_Type)
   is
      Start : constant Index_Type := Add (Subtree'First, Offset);
   begin
      Subtree (Start) := Null_Content_Element;
      Subtree (Start + 1 .. Add (Start, Num_Elements (Value) - 1)) := (others => Null_Data_Element);
      Put_String (Subtree, Offset, Value);
   end Put_Content;

   ---------------
   -- Attribute --
   ---------------

   procedure Attribute (Name   : Content_Type;
                        Data   : Attr_Data_Type;
                        Offset : in out Offset_Type;
                        Output : in out Subtree_Type)
   is
      Name_Elements : constant Offset_Type := Num_Elements (Name);
      Data_Elements : constant Offset_Type := Num_Attr_Elements (Data);
      Start         : constant Index_Type  := Add (Output'First, Offset);
      Last_Element  : constant Index_Type  := Add (Add (Start, Name_Elements), Data_Elements) - 1;
   begin
      Output (Start)                     := Null_Attribute_Element;
      Output (Start + 1 .. Last_Element) := (others => Null_Data_Element);

      Put_String (Output, Offset, Name);
      Output (Start).Next  := (if Name_Elements > 1 then 1 else Invalid_Relative_Index);
      Output (Start).Value := Relative_Index_Type (Name_Elements);
      Put_String (Output, Offset + Name_Elements, Data);
      Offset := Offset + Name_Elements + Data_Elements;
   end Attribute;

   ----------------
   -- Get_String --
   ----------------

   procedure Get_String (Doc    : Subtree_Type;
                         Start  : Offset_Type;
                         Result : out Result_Type;
                         Data   : in out Content_Type;
                         Last   : out Natural)
   is
      function String_Length (Document : Subtree_Type;
                              Offset   : Offset_Type) return Natural
      with
         Pre      => Offset < Document'Length,
         Annotate => (Gnatprove, Terminating);

      function String_Length (Document : Subtree_Type;
                              Offset   : Offset_Type) return Natural
      is
         Pos : Index_Type := Add (Document'First, Offset);
         N   : Node_Type  := Document (Pos);
         Len : Natural := 0;
      begin
         loop
            pragma Loop_Variant (Decreases => Document'Last - Pos);
            if Len > Natural'Last - Natural (N.Length)
            then
               return 0;
            end if;
            Len := Len + Natural (N.Length);
            exit when N.Next = Invalid_Relative_Index or N.Next >= Sub (Index_Type'Last, Pos);
            Pos := Add (Pos, N.Next);
            exit when not (Pos in Document'Range);
            N := Document (Pos);
         end loop;
         return Len;
      end String_Length;

      Len    : constant Natural := String_Length (Doc, Start);
      Offset : Natural := 0;
      Pos    : Index_Type := Add (Doc'First, Start);
      N      : Node_Type  := Doc (Pos);

   begin
      for D of Data
      loop
         D := Character'Val (0);
      end loop;

      if Len > Data'Length
      then
         Last   := 0;
         Result := Result_Overflow;
         return;
      end if;

      loop
         pragma Loop_Variant (Decreases => Doc'Last - Pos);
         pragma Loop_Invariant (Data'First <= Natural (Index_Type'Last) - Offset);

         --  FIXME: We can assume this already by the way we caluculated Length.
         exit when Natural (N.Length) > Data'Length - Offset or
            Offset > Natural'Last - Data'First - Natural (N.Length);
         Data (Data'First + Offset .. Data'First + Offset + Natural (N.Length) - 1) :=
            N.Data (N.Data'First .. N.Data'First + Natural (N.Length) - 1);
         Offset := Offset + Natural (N.Length);
         exit when N.Next = Invalid_Relative_Index or N.Next >= Sub (Index_Type'Last, Pos);
         Pos := Add (Pos, N.Next);
         exit when not (Pos in Doc'Range);
         N := Doc (Pos);
      end loop;

      Last   := Data'First + Offset - 1;
      Result := Result_OK;

   end Get_String;

   -------
   -- & --
   -------

   overriding
   function "&" (Left, Right : Subtree_Type) return Subtree_Type
   is
      pragma Unreferenced (Left, Right);
   begin
      return Null_Tree;
   end "&";

end SXML;
