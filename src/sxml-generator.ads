package SXML.Generator
is
   pragma Annotate (GNATprove, Terminating, SXML.Generator);

   type Attributes_Type (<>) is private;
   Null_Attributes : constant Attributes_Type;

   ------------------
   -- Num_Elements --
   ------------------

   function Num_Elements (Attributes : Attributes_Type) return Offset_Type
   with
      Annotate => (GNATprove, Terminating);

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Left  : Attributes_Type;
                      Right : Attributes_Type) return Boolean
   with Ghost;

   ---------------
   -- To_String --
   ---------------

   function To_String (Value : Float) return Content_Type
   with
      SPARK_Mode,
      Post     => To_String'Result'Length < 12,
      Annotate => (GNATprove, Terminating);

   function To_String (Value : Integer) return Content_Type
   with
      SPARK_Mode,
      Post     => To_String'Result'Length < 12,
      Annotate => (GNATprove, Terminating);

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Subtree_Type) return Subtree_Type
   with
      Pre  => Is_Valid (Left, Right),
      Post => Num_Elements ("+"'Result) = Num_Elements (Left) + Num_Elements (Right);

   function "+" (Left, Right : Attributes_Type) return Attributes_Type
   with
      Pre  => Is_Valid (Left, Right),
      Post => Num_Elements ("+"'Result) = Num_Elements (Left) + Num_Elements (Right);

   -------
   -- E --
   -------

   function E (Name       : Content_Type;
               Attributes : Attributes_Type;
               Children   : Subtree_Type) return Subtree_Type
   with
      Pre      => Num_Elements (Name) < Offset_Type'Last - Num_Elements (Attributes) - Num_Elements (Children),
      Post     => E'Result /= Null_Tree and
                  E'Result'Length = Num_Elements (Name) + Num_Elements (Attributes) + Num_Elements (Children),
      Annotate => (GNATprove, Terminating);

   function E (Name       : Content_Type;
               Children   : Subtree_Type) return Subtree_Type
   is (E (Name, Null_Attributes, Children))
   with
      Pre      => Num_Elements (Name) < Offset_Type'Last - Num_Elements (Null_Attributes) - Num_Elements (Children),
      Post     => E'Result /= Null_Tree and
                  Num_Elements (E'Result) =
                     Num_Elements (Name) + Num_Elements (Null_Attributes) + Num_Elements (Children),
      Annotate => (GNATprove, Terminating);

   function E (Name       : Content_Type;
               Attributes : Attributes_Type) return Subtree_Type
   is (E (Name, Attributes, Null_Tree))
   with
      Pre      => Num_Elements (Name) < Offset_Type'Last - Num_Elements (Attributes) - Num_Elements (Null_Tree),
      Post     => E'Result /= Null_Tree and
                  Num_Elements (E'Result) = Num_Elements (Name) + Num_Elements (Attributes) + Num_Elements (Null_Tree),
      Annotate => (GNATprove, Terminating);

   function E (Name : Content_Type) return Subtree_Type
   is (E (Name, Null_Attributes, Null_Tree))
   with
      Pre  => Num_Elements (Name) < Offset_Type'Last - Num_Elements (Null_Attributes) - Num_Elements (Null_Tree),
      Post => E'Result /= Null_Tree and
              Num_Elements (E'Result) = Num_Elements (Name) + Num_Elements (Null_Attributes) + Num_Elements (Null_Tree),
      Annotate => (GNATprove, Terminating);

   -------
   -- A --
   -------

   function A (Name  : Content_Type;
               Value : Integer) return Attributes_Type
   with
      Pre  => Num_Elements (Name) < Offset_Type'Last - 2,
      Post => A'Result /= Null_Attributes and
              Num_Elements (A'Result) = Num_Elements (Name) + Num_Elements (To_String (Value)),
      Annotate => (GNATprove, Terminating);

   -------
   -- A --
   -------

   function A (Name  : Content_Type;
               Value : Float) return Attributes_Type
   with
      Pre  => Num_Elements (Name) < Offset_Type'Last - 2,
      Post => A'Result /= Null_Attributes and
              Num_Elements (A'Result) = Num_Elements (Name) + Num_Elements (To_String (Value)),
      Annotate => (GNATprove, Terminating);

   -------
   -- A --
   -------

   function A (Name  : Content_Type;
               Value : Content_Type) return Attributes_Type
   with
      Pre  => Num_Elements (Name) < Offset_Type'Last and then
              Num_Elements (Value) <= Offset_Type (Index_Type'Last - Add (1, Num_Elements (Name))),
      Post => A'Result /= Null_Attributes and
              Num_Elements (A'Result) = Num_Elements (Name) + Num_Elements (Value),
      Annotate => (GNATproof, Terminating);

   -------
   -- C --
   -------

   function C (Value : Content_Type) return Subtree_Type
   with
      Post => C'Result /= Null_Tree and
              C'Result'Length = Num_Elements (Value),
      Annotate => (GNATprove, Terminating);

private

   type Attributes_Type is new Subtree_Type;
   Null_Attributes : constant Attributes_Type := Attributes_Type (Null_Tree);

   --------------
   -- Is_Valid --
   --------------

   overriding
   function Is_Valid (Left  : Attributes_Type;
                      Right : Attributes_Type) return Boolean
   is (Is_Valid (Subtree_Type (Left), Subtree_Type (Right)));

end SXML.Generator;
