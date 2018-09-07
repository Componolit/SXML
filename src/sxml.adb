package body SXML
   with SPARK_Mode
is
   Null_Open_Element : constant Node_Type :=
     Node_Type'(Kind           => Kind_Element_Open,
                Length         => 0,
                Next           => Null_Offset,
                Data           => Null_Data,
                Attributes     => Null_Offset,
                Children       => Null_Offset,
                Siblings       => Null_Offset);

   Null_Data_Element : constant Node_Type :=
     Node_Type'(Kind           => Kind_Data,
                Length         => 0,
                Next           => Null_Offset,
                Data           => Null_Data);

   Null_Attribute_Element : constant Node_Type :=
     Node_Type'(Kind           => Kind_Attribute,
                Length         => 0,
                Next           => Null_Offset,
                Next_Attribute => Null_Offset,
                Data           => Null_Data,
                Value          => Null_Offset);

   ------------------
   -- Num_Elements --
   ------------------

   function Num_Elements (D : String) return Offset_Type
   is ((D'Length + Data_Type'Length - 1) / Data_Type'Length);

   ----------------
   -- Put_String --
   ----------------

   procedure Put_String (Subtree : in out Subtree_Type;
                         Offset  : Offset_Type;
                         Name    : String)
   with
      Pre => Subtree'Length >= Num_Elements (Name);

   procedure Put_String (Subtree : in out Subtree_Type;
                         Offset  : Offset_Type;
                         Name    : String)
   is
      Position : Natural := 0;
      Len      : Natural;
   begin
      for I in Index_Type range
        Subtree'First + Offset .. Subtree'First + Offset + Index_Type (Num_Elements (Name)) - 1
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
      Result : Subtree_Type (1 .. Num_Elements (Name)) :=
        (1      => Null_Open_Element,
         others => Null_Data_Element);
   begin
      Put_String (Result, 0, Name);
      return Result;
   end Open;

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
      Start : constant Index_Type := Output'First + Offset;
   begin
      Output (Start) := Null_Attribute_Element;
      Output (Start + 1 .. Start + Name_Elements + Data_Elements - 1) :=
        (others => Null_Data_Element);

      Put_String (Output, Offset, Name);
      Output (Start).Next  := (if Name_Elements > 1 then 1 else 0);
      Output (Start).Value := Name_Elements;
      Put_String (Output, Offset + Name_Elements, Data);
      Offset := Offset + Name_Elements + Data_Elements;
   end Attribute;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (T : Subtree_Type;
                        I : Offset_Type) return String
   is
      N : constant Node_Type := T (T'First + I);
   begin
      return N.Data (1 .. Natural (N.Length)) &
         (if N.Next /= Null_Offset
          then Get_String (T, I + N.Next)
          else "");
   end Get_String;

   ----------------
   -- Attributes --
   ----------------

   function Attributes (T : Subtree_Type;
                        I : Offset_Type) return String;

   function Attributes (T : Subtree_Type;
                        I : Offset_Type) return String
   is
      N : constant Node_Type := T (T'First + I);
   begin
      if I = Null_Offset
      then
         return "";
      end if;

      pragma Assert (N.Kind = Kind_Attribute);
      return " "
           & Get_String (T, I)
           & "="""
           & Get_String (T, I + N.Value)
           & """"
           & (if N.Next_Attribute /= Null_Offset
              then Attributes (T, I + N.Next_Attribute)
              else "");
   end Attributes;

   ---------------
   -- To_String --
   ---------------

   function To_String (T : Subtree_Type) return String
   is
      N   : constant Node_Type := T (T'First);
      Tag : constant String    := Get_String (T, 0);
   begin
      return "<"
           & Tag
           & Attributes (T, N.Attributes)
           & (if N.Children = Null_Offset
              then "/>"
              else ">" & To_String (T (T'First + N.Children .. T'Last)) & "</" & Tag & ">")
           & (if N.Siblings = Null_Offset
              then ""
              else To_String (T (T'First + N.Siblings .. T'Last)));
   end To_String;

   overriding
   function "&" (Left, Right : Subtree_Type) return Subtree_Type
   is
      pragma Unreferenced (Left, Right);
   begin
      return Null_Tree;
   end "&";

end SXML;
