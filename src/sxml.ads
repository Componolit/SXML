package SXML
with
   SPARK_Mode
is
   type Offset_Type is new Natural range 0 .. Natural'Last / 2;
   subtype Index_Type is Offset_Type range 1 .. Offset_Type'Last;
   Invalid_Index : constant Index_Type;

   type Node_Type is private;
   Null_Node : constant Node_Type;

   type Subtree_Type is array (Index_Type range <>) of Node_Type;
   Null_Tree : constant Subtree_Type;

   function Is_Valid (Left, Right : Subtree_Type) return Boolean
   with
      Ghost;

   function "*" (Left, Right : Subtree_Type) return Subtree_Type
   is (Left & Right)
   with
     Pre => Is_Valid (Left, Right);

   overriding
   function "&" (Left, Right : Subtree_Type) return Subtree_Type
   with
     Pre => False;

   ----------
   -- Open --
   ----------

   function Open (Name : String) return Subtree_Type;

   ---------------
   -- Attribute --
   ---------------

   function Attribute (Name : String;
                       Data : String) return Subtree_Type;

   ---------------
   -- To_String --
   ---------------

   function To_String (T : Subtree_Type) return String;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (T : Subtree_Type;
                        I : Offset_Type) return String;

private

   type Length_Type is range 0 .. 8 with Size => 8;
   subtype Data_Type is String (1 .. Natural (Length_Type'Last));
   Null_Data : constant Data_Type := (others => Character'Val (0));

   type Kind_Type is (Kind_Invalid,
                      Kind_Element_Open,
                      Kind_Attribute,
                      Kind_Data);

   Null_Offset : constant Offset_Type := 0;

   type Node_Type (Kind : Kind_Type := Kind_Invalid) is
   record
      Length : Length_Type;
      Next   : Offset_Type;
      Data   : Data_Type;
      case Kind is
         when Kind_Element_Open =>
            Attributes     : Offset_Type;
            Children       : Offset_Type;
            Siblings       : Offset_Type;
         when Kind_Attribute =>
            Next_Attribute : Offset_Type;
            Value          : Offset_Type;
         when Kind_Data |
              Kind_Invalid =>
            null;
      end case;
   end record;

   Null_Node : constant Node_Type := (Kind   => Kind_Invalid,
                                      Next   => 0,
                                      Data   => Null_Data,
                                      Length => 0);
   Null_Tree : constant Subtree_Type (1 .. 0) := (others => Null_Node);

   function Is_Valid (Left, Right : Subtree_Type) return Boolean
   is
      (Left'First = 1 and Left'Length <= Index_Type'Last - Right'Length);

   Invalid_Index : constant Index_Type := Index_Type'Last;

end SXML;
