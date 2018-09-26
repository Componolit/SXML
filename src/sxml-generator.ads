package SXML.Generator
with
   SPARK_Mode
is

   type Attributes_Type (<>) is private;
   Null_Attributes : constant Attributes_Type;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Left  : Attributes_Type;
                      Right : Attributes_Type) return Boolean
   with Ghost;

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Subtree_Type) return Subtree_Type
   with
      Pre => Is_Valid (Left, Right);

   function "+" (Left, Right : Attributes_Type) return Attributes_Type
   with
      Pre => Is_Valid (Left, Right);

   -------
   -- E --
   -------

   function E (Name       : Content_Type;
               Attributes : Attributes_Type;
               Children   : Subtree_Type) return Subtree_Type;

   function E (Name       : Content_Type;
               Children   : Subtree_Type) return Subtree_Type;

   function E (Name       : Content_Type;
               Attributes : Attributes_Type) return Subtree_Type;

   function E (Name : Content_Type) return Subtree_Type;

   -------
   -- A --
   -------

   function A (Name  : Content_Type;
               Value : Integer) return Attributes_Type
   with
      Pre => Num_Elements (Name) < Offset_Type'Last - 2;

   -------
   -- A --
   -------

   function A (Name  : Content_Type;
               Value : Float) return Attributes_Type
   with
      Pre => Num_Elements (Name) < Offset_Type'Last - 2;

   -------
   -- A --
   -------

   function A (Name  : Content_Type;
               Value : Content_Type) return Attributes_Type
   with
      Pre => Num_Elements (Name) < Offset_Type'Last and then
             Num_Elements (Value) <= Offset_Type (Index_Type'Last - Add (1, Num_Elements (Name)));

   -------
   -- C --
   -------

   function C (Value : Content_Type) return Subtree_Type;

private

   type Attributes_Type is new Subtree_Type;
   Null_Attributes : constant Attributes_Type := Attributes_Type (Null_Tree);

   function To_String (Value : Float) return Content_Type
   with
       Post     => To_String'Result'Length < 12,
       Annotate => (GNATprove, Terminating);

   function To_String (Value : Integer) return Content_Type
   with
      Post     => To_String'Result'Length < 12,
      Annotate => (GNATprove, Terminating);

   --------------
   -- Is_Valid --
   --------------

   overriding
   function Is_Valid (Left  : Attributes_Type;
                      Right : Attributes_Type) return Boolean
   is (Is_Valid (Subtree_Type (Left), Subtree_Type (Right)));

end SXML.Generator;
