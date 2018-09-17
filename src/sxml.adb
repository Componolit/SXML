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
         if Name'Length - Position > Subtree (I).Data'Length
         then
            Len := Subtree (I).Data'Length;
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
                        Output : out Subtree_Type)
   is
      Name_Elements : constant Offset_Type := Num_Elements (Name);
      Data_Elements : constant Offset_Type := Num_Elements (Data);
      Start         : constant Index_Type := Add (Output'First, Offset);
   begin
      Output (Start) := Null_Attribute_Element;
      Output (Start + 1 .. Add (Add (Start, Name_Elements), Data_Elements) - 1) :=
        (others => Null_Data_Element);

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
                        Start : Offset_Type;
                        Level : Natural := Get_String_Depth) return String
   is
      N : constant Node_Type := Doc (Add (Doc'First, Start));
   begin
      if Level = 0
      then
         return "OVERFLOW";
      end if;

      return N.Data (1 .. Natural (N.Length)) &
         (if N.Next /= Invalid_Relative_Index
          then Get_String (Doc, Add (Start, N.Next), Level - 1)
          else "");
   end Get_String;

   overriding
   function "&" (Left, Right : Subtree_Type) return Subtree_Type
   is
      pragma Unreferenced (Left, Right);
   begin
      return Null_Tree;
   end "&";

end SXML;
