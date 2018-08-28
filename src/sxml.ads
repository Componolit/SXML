package SXML
   with SPARK_Mode
is
   type Node_Type is private;
   Null_Node : constant Node_Type;

   type Index_Type is new Natural range 1 .. Natural'Last / 2;
   type Subtree_Type is array (Index_Type range <>) of Node_Type;

   function Is_Valid (Left, Right : Subtree_Type) return Boolean
   is
      (Left'First = 1 and Left'Length <= Index_Type'Last - Right'Length)
   with
      Ghost;

   function Concatenate (Left, Right : Subtree_Type) return Subtree_Type
   is (Left & Right)
   with
      Pre => Is_Valid (Left, Right);

   overriding
   function "&" (Left, Right : Subtree_Type) return Subtree_Type
   is (Concatenate (Left, Right))
   with
      Pre  => Is_Valid (Left, Right),
      Post => "&"'Result'Length = Left'Length + Right'Length;

   Null_Tree : constant Subtree_Type;

   ----------
   -- Open --
   ----------

   function Open (Name : String) return Subtree_Type;

   -----------
   -- Close --
   -----------

   function Close (Name : String) return Subtree_Type;

   -------
   -- E --
   -------

   function E (Name     : String;
               Children : Subtree_Type := Null_Tree) return Subtree_Type
   with
      Pre  => Children'Length < Index_Type'Last - 2,
      Post => E'Result'First = 1 and E'Result'Length = Children'Length + 2;

   -------
   -- A --
   -------

   function A (Name  : String;
               Value : Integer) return Subtree_Type
   with
      Post => A'Result'First = 1 and A'Result'Length = 1;

   -------
   -- A --
   -------

   function A (Name  : String;
               Value : Float) return Subtree_Type
   with
      Post => A'Result'First = 1 and A'Result'Length = 1;

   -------
   -- A --
   -------

   function A (Name  : String;
               Value : String) return Subtree_Type
   with
       Post => A'Result'First = 1 and A'Result'Length = 1;

   ---------------
   -- To_String --
   ---------------

   function To_String (Tree : Subtree_Type) return String;

private
   subtype Data_Type is String (1 .. 10);
   Null_Data : constant Data_Type := (others => Character'Val (0));

   type Data_Index_Type is new Natural range Data_Type'First - 1 .. Data_Type'Last - 1;

   type Kind_Type is (Kind_Invalid,
                      Kind_Element_Open,
                      Kind_Element_Close,
                      Kind_Attr_Name,
                      Kind_Attr_Data,
                      Kind_Data);

   type Node_Type (Kind : Kind_Type := Kind_Invalid) is
   record
      Length : Data_Index_Type;
      Data   : Data_Type;
   end record;

   Null_Node : constant Node_Type := (Kind   => Kind_Invalid,
                                      Length => Data_Index_Type'First,
                                      Data   => Null_Data);
   Null_Tree : constant Subtree_Type (1 .. 0) := (others => Null_Node);

   function To_String (Value : Float) return String
   with
       Post     => To_String'Result'Length < 12,
       Annotate => (GNATprove, Terminating);

   function To_String (Value : Integer) return String
   with
      Post     => To_String'Result'Length < 12,
      Annotate => (GNATprove, Terminating);

end SXML;
