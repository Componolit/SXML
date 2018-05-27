package SXML
   with SPARK_Mode
is

   type Node_Type is private;
   type Subtree_Type is array (Natural range <>) of Node_Type;

   Null_Tree : constant Subtree_Type;

   function E (Name     : String;
               Children : Subtree_Type := Null_Tree) return Subtree_Type;

   function A (Name  : String;
               Value : Integer) return Subtree_Type;

   function A (Name  : String;
               Value : Float) return Subtree_Type;

   function A (Name  : String;
               Value : String) return Subtree_Type;

   procedure Print (Tree : Subtree_Type);

private
   type Kind_Type is (Kind_Invalid,
                      Kind_Element_Open,
                      Kind_Element_Close,
                      Kind_Attr_Integer,
                      Kind_Attr_Float,
                      Kind_Attr_String);

   type Name_Type is new String (1..100);
   Null_Name : constant Name_Type := (others => Character'Val (0));

   type Node_Type (Kind : Kind_Type := Kind_Invalid) is
   record
      Name : Name_Type;
      case Kind is
         when Kind_Invalid
            | Kind_Element_Open
            | Kind_Element_Close => null;
         when Kind_Attr_Integer =>
            Integer_Value : Integer;
         when Kind_Attr_Float =>
            Float_Value : Float;
         when Kind_Attr_String =>
            String_Value : Name_Type;
      end case;
   end record;

   Null_Node : constant Node_Type := (Kind => Kind_Invalid, Name => Null_Name);
   Null_Tree : constant Subtree_Type (1..0) := (others => Null_Node);
end SXML;
