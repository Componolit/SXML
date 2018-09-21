package body SXML
   with SPARK_Mode
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

   function Num_Elements (D : String) return Offset_Type
   is ((Offset_Type (D'Length + Data_Type'Length - 1)) / Offset_Type (Data_Type'Length));

   ----------------
   -- Put_String --
   ----------------

   procedure Put_String (Subtree : in out Subtree_Type;
                         Offset  : Offset_Type;
                         Name    : String)
   is
      Position : Natural := 0;
      Len      : Natural;
      NE       : constant Offset_Type := Num_Elements (Name);
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

   function Open (Name : String) return Subtree_Type
   is
      Result : Subtree_Type (1 .. Add (1, Num_Elements (Name))) :=
        (1      => Null_Open_Element,
         others => Null_Data_Element);
   begin
      Put_String (Result, 0, Name);
      return Result;
   end Open;

   -----------------
   -- Put_Content --
   -----------------

   procedure Put_Content (Subtree : in out Subtree_Type;
                          Offset  : Offset_Type;
                          Value   : String)
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

   procedure Attribute (Name   : String;
                        Data   : String;
                        Offset : in out Offset_Type;
                        Output : in out Subtree_Type)
   is
      Name_Elements : constant Offset_Type := Num_Elements (Name);
      Data_Elements : constant Offset_Type := Num_Elements (Data);
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

   function Get_String (Doc   : Subtree_Type;
                        Start : Offset_Type) return String
   is
      function String_Length (Document : Subtree_Type;
                              Offset   : Offset_Type) return Natural
      with
         Pre      => Offset < Document'Length,
         Annotate => (Gnatprove, Terminating);

      function String_Length (Document : Subtree_Type;
                              Offset   : Offset_Type) return Natural
      is
         Pos    : Index_Type := Add (Document'First, Offset);
         N      : Node_Type  := Document (Pos);
         Length : Natural := 0;
      begin
         loop
            pragma Loop_Variant (Decreases => Document'Last - Pos);
            if Length > Natural'Last - Natural (N.Length)
            then
               return 0;
            end if;
            Length := Length + Natural (N.Length);
            exit when N.Next = Invalid_Relative_Index or N.Next >= Sub (Index_Type'Last, Pos);
            Pos := Add (Pos, N.Next);
            exit when not (Pos in Document'Range);
            N := Document (Pos);
         end loop;
         return Length;
      end String_Length;

      Length : constant Natural := String_Length (Doc, Start);
      Offset : Natural := 0;
      Pos    : Index_Type := Add (Doc'First, Start);
      N      : Node_Type  := Doc (Pos);

   begin

      return Result : String (1 .. Length) := (others => Character'Val (0))
      do
         loop
            pragma Loop_Variant (Decreases => Doc'Last - Pos);

            --  FIXME: We can assume this already by the way we caluculated Length.
            exit when Natural (N.Length) > Result'Length - Offset or
                      Offset > Natural'Last - Result'First - Natural (N.Length);
            Result (Result'First + Offset .. Result'First + Offset + Natural (N.Length) - 1) :=
              N.Data (N.Data'First .. N.Data'First + Natural (N.Length) - 1);
            Offset := Offset + Natural (N.Length);
            exit when N.Next = Invalid_Relative_Index or N.Next >= Sub (Index_Type'Last, Pos);
            Pos := Add (Pos, N.Next);
            exit when not (Pos in Doc'Range);
            N := Doc (Pos);
         end loop;
      end return;
   end Get_String;

   overriding
   function "&" (Left, Right : Subtree_Type) return Subtree_Type
   is
      pragma Unreferenced (Left, Right);
   begin
      return Null_Tree;
   end "&";

end SXML;
