package SXML
   with SPARK_Mode
is
   type Node_Type is private;
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

   Null_Node : constant Node_Type;
   Null_Tree : constant Subtree_Type;

   ----------
   -- Open --
   ----------

   function Open (Name : String) return Subtree_Type;

   -----------
   -- Close --
   -----------

   function Close (Name : String) return Subtree_Type;

   ---------------
   -- Attr_Name --
   ---------------

   function Attr_Name (Name : String) return Subtree_Type;

   ---------------
   -- Attr_Data --
   ---------------

   function Attr_Data (Data : String) return Subtree_Type;

private

   type Length_Type is range 0 .. 8 with Size => 8;
   subtype Data_Type is String (1 .. Natural (Length_Type'Last));
   Null_Data : constant Data_Type := (others => Character'Val (0));

   type Kind_Type is (Kind_Invalid,
                      Kind_Element_Open,
                      Kind_Element_Close,
                      Kind_Attr_Name,
                      Kind_Attr_Data,
                      Kind_Data);

   type Node_Type (Kind : Kind_Type := Kind_Invalid) is
   record
      Length : Length_Type;
      Data   : Data_Type;
   end record;

   Null_Node : constant Node_Type := (Kind   => Kind_Invalid,
                                      Length => 0,
                                      Data   => Null_Data);
   Null_Tree : constant Subtree_Type (1 .. 0) := (others => Null_Node);

end SXML;
