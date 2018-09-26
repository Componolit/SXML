package SXML
with
   SPARK_Mode
is
   Get_String_Depth : constant := 100;

   subtype Content_Type is String
   with
      Predicate => Content_Type'First <= Content_Type'Last and then
                   Content_Type'Length > 0;

   type Index_Type is range 1 .. Natural'Last;
   Invalid_Index : constant Index_Type := Index_Type'Last;

   type Relative_Index_Type is range 0 .. Natural'Last;
   Invalid_Relative_Index : constant Relative_Index_Type := Relative_Index_Type'First;

   type Offset_Type is new Natural range 0 .. Natural'Last;
   Null_Offset : constant Offset_Type := Offset_Type'First;

   subtype Natural_Without_Last is Natural range Natural'First .. Natural'Last - 1;
   --  Natural subtype that is useful for arrays, as we can use 'Length without
   --  having to show that 'Last is < Natural'Last.

   ---------
   -- Add --
   ---------

   function Add (Left  : Offset_Type;
                 Right : Relative_Index_Type) return Offset_Type
   is (Left + Offset_Type (Right))
   with
      Pre => Offset_Type (Right) <= Offset_Type'Last - Left;

   function Add (Left  : Index_Type;
                 Right : Offset_Type) return Index_Type
   is (Index_Type (Natural (Left) + Natural (Right)))
   with
      Pre => Right <= Offset_Type (Index_Type'Last - Left);

   function Add (Left  : Index_Type;
                 Right : Relative_Index_Type) return Index_Type
   is (Index_Type (Natural (Left) + Natural (Right)))
   with
      Pre => Right <= Relative_Index_Type (Index_Type'Last - Left);

   --------------
   -- Overflow --
   --------------

   function Overflow (Left  : Offset_Type;
                      Right : Relative_Index_Type) return Boolean
   is (Offset_Type (Right) > Offset_Type'Last - Left);

   function Overflow (Left  : Index_Type;
                      Right : Relative_Index_Type) return Boolean
   is (Right > Relative_Index_Type (Index_Type'Last - Left));

   function Overflow (Left  : Index_Type;
                      Right : Offset_Type) return Boolean
   is (Right > Offset_Type (Index_Type'Last - Left));

   ---------
   -- Sub --
   ---------

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

   function Sub (Left  : Index_Type;
                 Right : Offset_Type) return Index_Type
   is (Index_Type (Offset_Type (Left) - Right))
   with
      Pre => Offset_Type (Left) > Right;

   ---------------
   -- Underflow --
   ---------------

   function Underflow (Left  : Index_Type;
                       Right : Offset_Type) return Boolean
   is (Right >= Offset_Type (Left));

   function Underflow (Left  : Index_Type;
                       Right : Index_Type) return Boolean
   is (Right > Left);

   -----------------------------------------------------------------------------

   type Node_Type is private;
   Null_Node : constant Node_Type;

   type Subtree_Type is array (Index_Type range <>) of Node_Type
   with
      Predicate => Subtree_Type'First > 0 and Subtree_Type'Length > 0;
   Null_Tree : constant Subtree_Type;

   function Is_Valid (Left, Right : Subtree_Type) return Boolean
   with
      Ghost;

   function "*" (Left, Right : Subtree_Type) return Subtree_Type
   is (Left & Right)
   with
     Pre => Is_Valid (Left, Right);

   --  This operator must not be used, as subtrees have to be
   --  linked together. This is done by the * operator above.
   pragma Warnings (Off, "precondition is statically False");
   overriding function "&" (Left, Right : Subtree_Type) return Subtree_Type
   with Pre => False;
   pragma Warnings (On, "precondition is statically False");

   ----------
   -- Open --
   ----------

   function Open (Name : Content_Type) return Subtree_Type
   with
      Post => Open'Result'Length > 0;

   -----------------
   -- Put_Content --
   -----------------

   procedure Put_Content (Subtree : in out Subtree_Type;
                          Offset  : Offset_Type;
                          Value   : Content_Type)
   with
      Pre => Offset < Offset_Type (Index_Type'Last) and then
             Subtree'First <= Sub (Index_Type'Last, Offset) and then
             Natural (Num_Elements (Value)) <= Subtree'Length - Natural (Offset) and then
             Add (Subtree'First, Offset) <= Subtree'Last and then
             Num_Elements (Value) < Offset_Type (Sub (Index_Type'Last, Offset)) and then
             Subtree'First <= Sub (Sub (Index_Type'Last, Offset), Num_Elements (Value));

   ---------------
   -- Attribute --
   ---------------

   procedure Attribute (Name   : Content_Type;
                        Data   : Content_Type;
                        Offset : in out Offset_Type;
                        Output : in out Subtree_Type)
   with
      Pre => Output'Length <= Offset_Type'Last - Offset - Num_Elements (Name) - Num_Elements (Data) and then
             Offset + Num_Elements (Name) + Num_Elements (Data) < Output'Length and then
             Num_Elements (Data) <= Offset_Type (Index_Type'Last -
                                                 Add (Add (Output'First, Offset), Num_Elements (Name)));

   ----------------
   -- Get_String --
   ----------------

   function Get_String (Doc   : Subtree_Type;
                        Start : Offset_Type) return String
   with
      Pre      => Start < Doc'Length,
      Annotate => (Gnatprove, Terminating);

   ------------------
   -- Num_Elements --
   ------------------

   function Num_Elements (D : Content_Type) return Offset_Type
   with
      Post => Num_Elements'Result > 0;

   ---------------
   -- Same_Kind --
   ---------------

   function Same_Kind (Current : Subtree_Type;
                       Old     : Subtree_Type;
                       Offset  : Offset_Type) return Boolean
   with
      Pre => Current'First = Old'First and
             Current'Last = Old'Last and
             Offset < Current'Length,
      Ghost;

   ----------------
   -- Put_String --
   ----------------

   procedure Put_String (Subtree : in out Subtree_Type;
                         Offset  : Offset_Type;
                         Name    : Content_Type)
   with
      Pre  => Offset < Offset_Type (Index_Type'Last) and then
              Num_Elements (Name) < Offset_Type (Sub (Index_Type'Last, Offset)) and then
              Subtree'First <= Sub (Sub (Index_Type'Last, Offset), Num_Elements (Name)) and then
              Natural (Subtree'Length) - Natural (Offset) >= Natural (Num_Elements (Name)),
      Post => Same_Kind (Subtree, Subtree'Old, Offset);

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

   function Same_Kind (Current : Subtree_Type;
                       Old     : Subtree_Type;
                       Offset  : Offset_Type) return Boolean
   is
      (Current (Add (Current'First, Offset)).Kind = Old (Add (Old'First, Offset)).Kind);

end SXML;
