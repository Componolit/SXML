package SXML.Generator
with
   SPARK_Mode
is

   type Attributes_Type (<>) is private;
   Null_Attributes : constant Attributes_Type;

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Subtree_Type) return Subtree_Type;

   function "+" (Left, Right : Attributes_Type) return Attributes_Type;

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
               Value : Integer) return Attributes_Type;

   -------
   -- A --
   -------

   function A (Name  : Content_Type;
               Value : Float) return Attributes_Type;

   -------
   -- A --
   -------

   function A (Name  : Content_Type;
               Value : Content_Type) return Attributes_Type;

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

end SXML.Generator;
