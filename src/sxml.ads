package SXML
   with SPARK_Mode
is
   --  FIXME: The length should become a generic parameter
   type Name_Type is new String (1 .. 100);

   type Node_Type is private;
   Null_Node : constant Node_Type;

   type Index_Type is new Natural range 1 .. Natural'Last / 2;
   type Subtree_Type is array (Index_Type range <>) of Node_Type;

   function Is_Valid (Left, Right : Subtree_Type) return Boolean
   is
      (Left'First = 1 and Left'Length <= Index_Type'Last - Right'Length)
   with
      Ghost;

   function Is_Valid (Name : String) return Boolean
   is (Name'Last <= Name_Type'Last and
       Name'Length <= Name_Type'Length)
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

   -------
   -- E --
   -------

   function E (Name     : String;
               Children : Subtree_Type := Null_Tree) return Subtree_Type
   with
      Pre  => Children'Length < Index_Type'Last - 2 and
              Is_Valid (Name),
      Post => E'Result'First = 1 and E'Result'Length = Children'Length + 2;

   -------
   -- A --
   -------

   function A (Name  : String;
               Value : Integer) return Subtree_Type
   with
      Pre  => Is_Valid (Name),
      Post => A'Result'First = 1 and A'Result'Length = 1;

   -------
   -- A --
   -------

   function A (Name  : String;
               Value : Float) return Subtree_Type
   with
      Pre  => Is_Valid (Name),
      Post => A'Result'First = 1 and A'Result'Length = 1;

   -------
   -- A --
   -------

   function A (Name  : String;
               Value : String) return Subtree_Type
   with
      Pre  => Is_Valid (Name) and Is_Valid (Value),
      Post => A'Result'First = 1 and A'Result'Length = 1;

   -------------
   -- To_Name --
   -------------

   function To_Name (Name : String) return Name_Type
   with
      Pre => Is_Valid (Name);

   ---------------
   -- To_String --
   ---------------

   function To_String (Tree : Subtree_Type) return String;

private
   type Kind_Type is (Kind_Invalid,
                      Kind_Element_Open,
                      Kind_Element_Close,
                      Kind_Attr);

   Null_Name : constant Name_Type := (others => Character'Val (0));

   type Node_Type (Kind : Kind_Type := Kind_Invalid) is
   record
      Name : Name_Type;
      case Kind is
         when Kind_Invalid
            | Kind_Element_Open
            | Kind_Element_Close => null;
         when Kind_Attr =>
            Value : Name_Type;
      end case;
   end record;

   Null_Node : constant Node_Type := (Kind => Kind_Invalid, Name => Null_Name);
   Null_Tree : constant Subtree_Type (1 .. 0) := (others => Null_Node);

   function To_String (Value : Float) return String
   with
      Post     => To_String'Result'Length < 12 and
                  Is_Valid (To_String'Result),
      Annotate => (GNATprove, Terminating);

   function To_String (Value : Integer) return String
   with
      Post     => To_String'Result'Length < 12 and
                  Is_Valid (To_String'Result),
      Annotate => (GNATprove, Terminating);

end SXML;
