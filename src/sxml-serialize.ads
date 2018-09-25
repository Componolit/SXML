package SXML.Serialize is

   type Traversal_Type is private;
   Null_Traversal : constant Traversal_Type;

   type Stack_Type is array (Natural range <>) of Traversal_Type;

   ---------------
   -- To_String --
   ---------------

   procedure To_String (Doc    : Subtree_Type;
                        Data   : out String;
                        Last   : out Natural);

   ---------------
   -- To_String --
   ---------------

   procedure To_String (Doc    : Subtree_Type;
                        Data   : out String;
                        Last   : out Natural;
                        Buffer : in out Stack_Type)
   with
      Pre => Doc'Length > 0 and
             Buffer'Length > 0;

private

   type Mode_Type is (Mode_Invalid, Mode_Open, Mode_Close);

   type Traversal_Type is
   record
      Index : Index_Type;
      Mode  : Mode_Type;
   end record;

   Null_Traversal : constant Traversal_Type := (Invalid_Index, Mode_Invalid);

end SXML.Serialize;
