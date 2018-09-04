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
                         Name    : String)
   with
      Pre => Subtree'Length >= Num_Elements (Name);

   procedure Put_String (Subtree : in out Subtree_Type;
                         Name    : String)
   is
      Offset : Natural := 0;
      Len    : Natural;
   begin
      for I in Index_Type range 1 .. Index_Type (Num_Elements (Name))
      loop
         if Name'Length - Offset > Subtree (I).Data'Length
         then
            Len := Subtree (I).Data'Length;
            Subtree (I).Next := 1;
         else
            Len := Name'Length - Offset;
         end if;
         Subtree (I).Data (1 .. Len) :=
           Name (Name'First + Offset .. Name'First + Offset + Len - 1);
         Subtree (I).Length := Length_Type (Len);
         Offset := Offset + Len;
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
      Put_String (Result, Name);
      return Result;
   end Open;

   ---------------
   -- Attribute --
   ---------------

   function Attribute (Name : String;
                       Data : String) return Subtree_Type
   is
      Name_Tree : Subtree_Type (1 .. Num_Elements (Name)) :=
        (1      => Null_Attribute_Element,
         others => Null_Data_Element);
      Data_Tree : Subtree_Type (1 .. Num_Elements (Data)) :=
        (others => Null_Data_Element);
   begin
      Put_String (Name_Tree, Name);
      Name_Tree (1).Next  := (if Name_Tree'Length > 1 then 1 else 0);
      Name_Tree (1).Value := Name_Tree'Length;
      Put_String (Data_Tree, Data);
      return Name_Tree & Data_Tree;
   end Attribute;

   ----------
   -- Data --
   ----------

   function Data (T : Subtree_Type;
                  I : Offset_Type) return String;

   function Data (T : Subtree_Type;
                  I : Offset_Type) return String
   is
      N : constant Node_Type := T (T'First + I);
   begin
      return N.Data (1 .. Natural (N.Length)) &
         (if N.Next /= Null_Offset
          then Data (T, I + N.Next)
          else "");
   end Data;

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

      return
         " " &
         Data (T, I) &
         "=""" &
         Data (T, I + N.Value) &
         """" &
         (if N.Next_Attribute /= Null_Offset
          then Attributes (T, I + N.Next_Attribute)
          else "");
   end Attributes;

   ---------------
   -- To_String --
   ---------------

   function To_String (T : Subtree_Type) return String
   is
      N   : constant Node_Type := T (T'First);
      Tag : constant String    := Data (T, 0);
   begin
      return
        "<" & Tag &
        Attributes (T, N.Attributes) &
        (if N.Children = Null_Offset
         then "/>"
         else ">" & To_String (T (T'First + N.Children .. T'Last)) & "</" & Tag & ">") &
        (if N.Siblings = Null_Offset
         then ""
         else To_String (T (T'First + N.Siblings .. T'Last)));
   end To_String;

end SXML;
