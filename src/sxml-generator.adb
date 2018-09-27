package body SXML.Generator
is
   pragma Annotate (GNATprove, Terminating, SXML.Generator);

   ------------------
   -- Num_Elements --
   ------------------

   overriding
   function Num_Elements (Attributes : Attributes_Type) return Offset_Type
   is (Attributes'Length);

   ---------------
   -- To_String --
   ---------------

   function To_String (Value : Float) return Content_Type
   with
      SPARK_Mode => Off
   is
   begin
      if Value >= 0.0
      then
         --  Remove leading space
         return Value'Img (2 .. Value'Img'Last);
      else
         return Value'Img;
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Value : Integer) return Content_Type
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

   function E (Name       : Content_Type;
               Attributes : Attributes_Type;
               Children   : Subtree_Type) return Subtree_Type
   is
      Len : constant Index_Type :=
         Index_Type (Num_Elements (Name) + Num_Elements (Attributes) + Num_Elements (Children));

      Result   : Subtree_Type (1 .. Len) := (others => Null_Node);
      Position : Index_Type := Result'First;
      Start    : Index_Type;
   begin
      Open (Name, Result, Position, Start);
      Result (Start).Attributes :=
         (if Attributes = Null_Attributes
          then Invalid_Relative_Index
          else Relative_Index_Type (Num_Elements (Name)));
      Result (Start).Children :=
         (if Children = Null_Tree
          then Invalid_Relative_Index
          else Relative_Index_Type (Num_Elements (Name) + Num_Elements (Attributes)));

      if Attributes /= Null_Attributes
      then
         Result (Position .. Add (Position, Num_Elements (Attributes)) - 1) := Subtree_Type (Attributes);
      end if;

      if Children /= Null_Tree
      then
         pragma Assert (Children'Length = Num_Elements (Children));
         Result (Add (Position, Num_Elements (Attributes)) ..
                 Add (Add (Position, Num_Elements (Attributes)), Num_Elements (Children)) - 1) := Children;
      end if;

      return Result;
   end E;

   -------
   -- A --
   -------

   function A (Name  : Content_Type;
               Value : Integer) return Attributes_Type
   is
   begin
      return A (Name, To_String (Value));
   end A;

   -------
   -- A --
   -------

   function A (Name  : Content_Type;
               Value : Float) return Attributes_Type
   is
   begin
      return A (Name, To_String (Value));
   end A;

   -------
   -- A --
   -------

   function A (Name  : Content_Type;
               Value : Content_Type) return Attributes_Type
   is
      Result : Subtree_Type (1 .. Sub (Add (Add (1, Num_Elements (Name)), Num_Elements (Value)), 1));
      Offset : Offset_Type := 0;
   begin
      Result := (others => Null_Node);
      SXML.Attribute (Name   => Name,
                      Data   => Value,
                      Offset => Offset,
                      Output => Result);
      pragma Unreferenced (Offset);
      return Attributes_Type (Result);
   end A;

   -------
   -- C --
   -------

   function C (Value : Content_Type) return Subtree_Type
   is
      Result : Subtree_Type (1 .. Add (1, Num_Elements (Value) - 1)) :=
         (others => Null_Node);
   begin
      Put_Content (Result, 0, Value);
      return Result;
   end C;

   ---------
   -- "+"--
   ---------

   function "+" (Left, Right : Subtree_Type) return Subtree_Type
   is
      Result : Subtree_Type (1 .. Index_Type (Num_Elements (Left) + Num_Elements (Right))) := (others => Null_Node);
      I      : Relative_Index_Type := 0;
      N      : Index_Type;
   begin
      if Right'Length = 0
      then
         return Left;
      end if;

      Result (1 .. Left'Length) := Left;
      Result (Left'Length + 1 .. Left'Length + Right'Length) := Right;

      --  Find last element
      loop
         pragma Loop_Variant (Increases => I);

         if Overflow (Result'First, I)
         then
            return Result;
         end if;

         N :=  Add (Result'First, I);
         if (N - Result'First) > Left'Length or
            (not (N in Result'Range) or else
             (Result (N).Kind /= Kind_Content and
              Result (N).Kind /= Kind_Element_Open))
         then
            return Result;
         end if;

         exit when Result (N).Siblings = Invalid_Relative_Index;

         if Result (N).Siblings > Relative_Index_Type'Last - I
         then
            return Result;
         end if;

         I := I + Result (N).Siblings;
      end loop;

      Result (N).Siblings := Left'Length - I;
      return Result;
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Attributes_Type) return Attributes_Type
   is
      Result : Attributes_Type (1 .. Left'Length + Right'Length);
      I      : Relative_Index_Type := 0;
      N      : Index_Type;
   begin
      --  This yields a SPARK bug box (cf. RA06-002)
      --  Result := (Result'Range => Null_Node);

      Result (1 .. Left'Length) := Left;
      Result (Left'Length + 1 .. Left'Length + Right'Length) := Right;

      pragma Assert (Num_Elements (Result) = Num_Elements (Left) + Num_Elements (Right));

      --  Find last attibute
      loop
         if Overflow (Result'First, I)
         then
            return Result;
         end if;

         N :=  Add (Result'First, I);
         if I > Left'Length or
            (not (N in Result'Range) or else
             Result (N).Kind /= Kind_Attribute)
         then
            return Result;
         end if;

         pragma Loop_Variant (Increases => I);
         pragma Loop_Invariant (N in Result'Range);
         pragma Loop_Invariant (Result (N).Kind = Kind_Attribute);
         pragma Loop_Invariant (I <= Left'Length);

         exit when Result (N).Next_Attribute = Invalid_Relative_Index;

         if Result (N).Next_Attribute > Relative_Index_Type'Last - I
         then
            return Result;
         end if;

         I := I + Result (N).Next_Attribute;
      end loop;

      Result (N).Next_Attribute := Left'Length - I;
      return Result;
   end "+";

end SXML.Generator;
