with Ada.Text_IO;
use Ada.Text_IO;

package body XML
   with SPARK_Mode
is

   type Kind_Type is (Kind_Invalid, Kind_Element_Open, Kind_Element_Close);

   type Name_Type is new String (1..100);

   type Node_Type is
   record
      Kind : Kind_Type;
      Name : Name_Type;
   end record;
   Null_Node : constant Node_Type := (Kind => Kind_Invalid, Name => (others => Character'Val (0)));

   type Subtree_Type is array (Natural range <>) of Node_Type;
   Null_Tree : constant Subtree_Type (1..0) := (others => Null_Node);

   function "&" (Left, Right : Subtree_Type) return Subtree_Type
   is
      Len : constant Natural := Left'Length + Right'Length;
      Result : Subtree_Type (1 .. Len);
   begin
      Result (1 .. Left'Length) := Left;
      Result (Left'Length + 1 .. Result'Last) := Right;
      return Result;
   end "&";

   function To_Name (Name : String) return Name_Type
   is
      Result : Name_Type := (others => Character'Val (0));
   begin
      for I in Name'Range
      loop
         Result (I) := Name (I);
      end loop;
      return Result;
   end To_Name;

   function E (Name     : String;
               Children : Subtree_Type := Null_Tree) return Subtree_Type
   is
      Index : Natural;
   begin
      return Result : Subtree_Type (1 .. Children'Length + 2)
      do
         Result (Result'First) := (Kind => Kind_Element_Open, Name => To_Name (Name));
         Index := 2;
         for Child of Children
         loop
            Result (Index) := Child;
            Index := Index + 1;
         end loop;
         Result (Result'Last) := (Kind => Kind_Element_Close, Name => To_Name (Name));
      end return;
   end E;

   procedure Print (Tree : Subtree_Type)
   is
   begin
      for I in Tree'Range
      loop
         case Tree (I).Kind
         is
            when Kind_Element_Open =>
               Put ("<" & String (Tree (I).Name) & ">");
            when Kind_Element_Close =>
               Put ("</" & String (Tree (I).Name) & ">");
            when Kind_Invalid =>
               Put ("INVALID");
         end case;
         New_Line;
      end loop;
   end Print;

   procedure Exec (Program   : String;
                   Arguments : Arguments_Type)
   is
      Tree : Subtree_Type :=
       E ("foo",
         E ("baz",
           E ("blub")
         ) &
         E ("bar")
       );
   begin
      Print (Tree);
   end Exec;

end XML;
