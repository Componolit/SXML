package SXML.Generator
with
   SPARK_Mode
is

   -------
   -- E --
   -------

   function E (Name     : String;
               Children : Subtree_Type := Null_Tree) return Subtree_Type
   with
      Pre  => Children'Length < Index_Type'Last - 2;

   -------
   -- A --
   -------

   function A (Name  : String;
               Value : Integer) return Subtree_Type;

   -------
   -- A --
   -------

   function A (Name  : String;
               Value : Float) return Subtree_Type;

   -------
   -- A --
   -------

   function A (Name  : String;
               Value : String) return Subtree_Type;

   ---------------
   -- To_String --
   ---------------

   function To_String (Tree : Subtree_Type) return String;

private

   function To_String (Value : Float) return String
   with
       Post     => To_String'Result'Length < 12,
       Annotate => (GNATprove, Terminating);

   function To_String (Value : Integer) return String
   with
      Post     => To_String'Result'Length < 12,
      Annotate => (GNATprove, Terminating);

end SXML.Generator;
