package body SXML.Generator is

   ---------------
   -- To_String --
   ---------------

   function To_String (Value : Float) return String
   with
      SPARK_Mode => Off
   is
      Value_Img : constant String := Value'Img;
   begin
      if Value >= 0.0
      then
         --  Remove leading space
         return Value_Img (2 .. Value_Img'Last);
      else
         return Value_Img;
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Value : Integer) return String
   with
      SPARK_Mode => Off
   is
   begin
      if Value >= 0
      then
         return Value'Img (2 .. Value'Img'Last);
      else
         return Value'Img;
      end if;
   end To_String;

   -------
   -- E --
   -------

   function E (Name       : String;
               Attributes : Attributes_Type;
               Children   : Subtree_Type) return Subtree_Type
   is
      O : Subtree_Type := Open (Name);
   begin
      O (O'First).Attributes :=
        (if Attributes = Null_Attributes then Null_Offset else O'Length);
      O (O'First).Children :=
        (if Children = Null_Tree then Null_Offset else O'Length + Attributes'Length);
      return O * Subtree_Type (Attributes) * Children;
   end E;

   function E (Name       : String;
               Attributes : Attributes_Type) return Subtree_Type
   is (E (Name, Attributes, Null_Tree));

   function E (Name     : String;
               Children : Subtree_Type) return Subtree_Type
   is (E (Name, Null_Attributes, Children));

   function E (Name : String) return Subtree_Type
   is (E (Name, Null_Attributes, Null_Tree));

   -------
   -- A --
   -------

   function A (Name  : String;
               Value : Integer) return Attributes_Type
   is
   begin
      return A (Name, To_String (Value));
   end A;

   -------
   -- A --
   -------

   function A (Name  : String;
               Value : Float) return Attributes_Type
   is
   begin
      return A (Name, To_String (Value));
   end A;

   -------
   -- A --
   -------

   function A (Name  : String;
               Value : String) return Attributes_Type
   is
      Result : Subtree_Type (1 .. Num_Elements (Name) + Num_Elements (Value));
      Offset : Offset_Type := 0;
   begin
      SXML.Attribute (Name   => Name,
                      Data   => Value,
                      Offset => Offset,
                      Output => Result);
      return Attributes_Type (Result);
   end A;

   ---------
   -- "+"--
   ---------

   function "+" (Left, Right : Subtree_Type) return Subtree_Type
   is
      Result : Subtree_Type := Left * Right;
      I : Offset_Type := 0;
   begin
      if Right'Length = 0
      then
         return Left;
      end if;

      --  Find last element
      loop
         exit when Left (Left'First + I).Siblings = Null_Offset;
         I := I + Left (Left'First + I).Siblings;
      end loop;

      Result (Result'First + I).Siblings := Left'Length - I;
      return Result;
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Attributes_Type) return Attributes_Type
   is
      Result : Attributes_Type := Left * Right;
      I : Offset_Type := 0;
   begin
      --  Find last attibute
      loop
         exit when Left (Left'First + I).Next_Attribute = Null_Offset;
         I := I + Left (Left'First + I).Next_Attribute;
      end loop;

      Result (Result'First + I).Next_Attribute := Left'Length - I;
      return Result;
   end "+";

end SXML.Generator;
