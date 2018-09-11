package SXML
with
   SPARK_Mode
is
   To_String_Depth  : constant := 100;
   Attributes_Depth : constant := 100;
   Get_String_Depth : constant := 100;

   type Index_Type is range 1 .. Natural'Last;
   Invalid_Index : constant Index_Type := Index_Type'Last;

   type Relative_Index_Type is range 0 .. Natural'Last;
   Invalid_Relative_Index : constant Relative_Index_Type := Relative_Index_Type'First;

   type Offset_Type is new Natural range 0 .. Natural'Last;
   Null_Offset : constant Offset_Type := Offset_Type'First;

   function Add (Left  : Offset_Type;
                 Right : Relative_Index_Type) return Offset_Type
   is (Left + Offset_Type (Right));

   function Add (Left  : Index_Type;
                 Right : Offset_Type) return Index_Type
   is (Index_Type (Natural (Left) + Natural (Right)));

   function Add (Left  : Index_Type;
                 Right : Relative_Index_Type) return Index_Type
   is (Index_Type (Natural (Left) + Natural (Right)));

   function Sub (Left  : Index_Type;
                 Right : Index_Type) return Relative_Index_Type
   is (Relative_Index_Type (Left - Right))
   with
      Pre => Left >= Right;

   function Sub (Left  : Index_Type;
                 Right : Index_Type) return Offset_Type
   is (Offset_Type (Left - Right))
   with
      Pre => Left >= Right;

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

   -------------
   -- Content --
   -------------

   function Content (Value : String) return Subtree_Type;

   ---------------
   -- Attribute --
   ---------------

   procedure Attribute (Name   : String;
                        Data   : String;
                        Offset : in out Offset_Type;
                        Output : out Subtree_Type)
   with
       Pre => Output'Length >= Offset + Num_Elements (Name) + Num_Elements (Data);

   ---------------
   -- To_String --
   ---------------

   function To_String (Doc   : Subtree_Type;
                       Level : Natural := To_String_Depth) return String;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (Doc   : Subtree_Type;
                        Start : Offset_Type;
                        Level : Natural := Get_String_Depth) return String;

   ------------------
   -- Num_Elements --
   ------------------

   function Num_Elements (D : String) return Offset_Type;

private

   type Length_Type is range 0 .. 8 with Size => 8;
   subtype Data_Type is String (1 .. Natural (Length_Type'Last));
   Null_Data : constant Data_Type := (others => Character'Val (0));

   type Kind_Type is (Kind_Invalid,
                      Kind_Element_Open,
                      Kind_Content,
                      Kind_Attribute,
                      Kind_Data);

   type Node_Type (Kind : Kind_Type := Kind_Invalid) is
   record
      Length : Length_Type;
      Next   : Relative_Index_Type;
      Data   : Data_Type;
      case Kind is
         when Kind_Element_Open
            | Kind_Content =>
            Attributes     : Relative_Index_Type;
            Children       : Relative_Index_Type;
            Siblings       : Relative_Index_Type;
         when Kind_Attribute =>
            Next_Attribute : Relative_Index_Type;
            Value          : Relative_Index_Type;
         when Kind_Data
            | Kind_Invalid =>
            null;
      end case;
   end record;

   Null_Node : constant Node_Type := (Kind   => Kind_Invalid,
                                      Next   => Invalid_Relative_Index,
                                      Data   => Null_Data,
                                      Length => 0);
   Null_Tree : constant Subtree_Type (1 .. 0) := (others => Null_Node);

   function Is_Valid (Left, Right : Subtree_Type) return Boolean
   is
      (Left'First = 1 and Left'Length <= Index_Type'Last - Right'Length);

end SXML;
