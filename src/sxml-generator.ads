package SXML.Generator
with
   SPARK_Mode
is
   type Attributes_Type is new SXML.Subtree_Type;
   Null_Attributes : constant Attributes_Type;

   -------
   -- E --
   -------

   function E (Name       : String;
               Attributes : Attributes_Type;
               Children   : Subtree_Type) return Subtree_Type;

   function E (Name       : String;
               Children   : Subtree_Type) return Subtree_Type;

   function E (Name       : String;
               Attributes : Attributes_Type) return Subtree_Type;

   function E (Name       : String) return Subtree_Type;

   -------
   -- A --
   -------

   function A (Name  : String;
               Value : Integer) return Attributes_Type;

   -------
   -- A --
   -------

   function A (Name  : String;
               Value : Float) return Attributes_Type;

   -------
   -- A --
   -------

   function A (Name  : String;
               Value : String) return Attributes_Type;

private

   Null_Attributes : constant Attributes_Type := Attributes_Type (Null_Tree);

   function To_String (Value : Float) return String
   with
       Post     => To_String'Result'Length < 12,
       Annotate => (GNATprove, Terminating);

   function To_String (Value : Integer) return String
   with
      Post     => To_String'Result'Length < 12,
      Annotate => (GNATprove, Terminating);

end SXML.Generator;
