package SXML
is
   Get_String_Depth      : constant := 128;
   Chunk_Length          : constant := 8;
   Scratch_Buffer_Length : constant := 1024;

   type Result_Type is (Result_OK,
                        Result_Overflow,
                        Result_Invalid,
                        Result_Not_Found);

   --  FIXME: Rename
   subtype Content_Type is String
   with
      Predicate => Content_Type'First <= Content_Type'Last and then
                   Content_Type'Last <= Natural'Last - Chunk_Length and then
                   Content_Type'Length >= 0;

   --  FIXME: Rename
   subtype Attr_Data_Type is String
   with
      Predicate => Attr_Data_Type'Last <= Natural'Last - Chunk_Length;

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
      Pre      => Offset_Type (Right) <= Offset_Type'Last - Left,
      Annotate => (GNATprove, Terminating);

   function Add (Left  : Index_Type;
                 Right : Offset_Type) return Index_Type
   is (Index_Type (Natural (Left) + Natural (Right)))
   with
      Pre      => Right <= Offset_Type (Index_Type'Last - Left),
      Annotate => (GNATprove, Terminating);

   function Add (Left  : Index_Type;
                 Right : Relative_Index_Type) return Index_Type
   is (Index_Type (Natural (Left) + Natural (Right)))
   with
      Pre      => Right <= Relative_Index_Type (Index_Type'Last - Left),
      Annotate => (GNATprove, Terminating);

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
      Pre      => Left >= Right,
      Annotate => (GNATprove, Terminating);

   function Sub (Left  : Index_Type;
                 Right : Index_Type) return Offset_Type
   is (Offset_Type (Left - Right))
   with
      Pre      => Left >= Right,
      Annotate => (GNATprove, Terminating);

   function Sub (Left  : Index_Type;
                 Right : Offset_Type) return Index_Type
   is (Index_Type (Offset_Type (Left) - Right))
   with
      Pre      => Offset_Type (Left) > Right,
      Annotate => (GNATprove, Terminating);

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

   type Document_Type is array (Index_Type range <>) of Node_Type
   with
      Dynamic_Predicate => Document_Type'First > 0 and Document_Type'Length > 0;
   Null_Tree : constant Document_Type;

   function Is_Valid (Left, Right : Document_Type) return Boolean
   with
      Ghost;

   --  This operator must not be used, as subtrees have to be
   --  linked together. This is done by the * operator above.
   pragma Warnings (Off, "precondition is statically False");
   overriding function "&" (Left, Right : Document_Type) return Document_Type
   with Pre => False;
   pragma Warnings (On, "precondition is statically False");

   -------------
   -- Is_Open --
   -------------

   function Is_Open (Node : Node_Type) return Boolean;

   ----------------
   -- Is_Invalid --
   ----------------

   function Is_Invalid (Node : Node_Type) return Boolean;

   -----------------
   -- Put_Content --
   -----------------

   procedure Put_Content (Subtree : in out Document_Type;
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
                        Data   : Attr_Data_Type;
                        Offset : in out Offset_Type;
                        Output : in out Document_Type)
   with
      Pre => Offset <= Output'Length - Num_Elements (Name) - Num_Attr_Elements (Data) and then
             Offset + Num_Elements (Name) + Num_Attr_Elements (Data) < Output'Length and then
             Num_Attr_Elements (Data) <= Offset_Type (Index_Type'Last -
                                                      Add (Add (Output'First, Offset), Num_Elements (Name)));

   ----------------
   -- Get_String --
   ----------------

   procedure Get_String (Doc    : Document_Type;
                         Start  : Offset_Type;
                         Result : out Result_Type;
                         Data   : in out Content_Type;
                         Last   : out Natural)
   with
      Pre      => Start < Doc'Length,
      Annotate => (Gnatprove, Terminating);

   ------------------
   -- Num_Elements --
   ------------------

   function Num_Elements (D : Content_Type) return Offset_Type
   with
      Post     => Num_Elements'Result = (D'Length + (Chunk_Length - 1)) / Chunk_Length,
      Annotate => (GNATprove, Terminating);

   function Num_Attr_Elements (D : Attr_Data_Type) return Offset_Type
   with
      Post => Num_Attr_Elements'Result = (D'Length + (Chunk_Length - 1)) / Chunk_Length,
      Annotate => (GNATprove, Terminating);

   function Num_Elements (Subtree : Document_Type) return Offset_Type
   with
      Post     => Num_Elements'Result = (if Subtree = Null_Tree then 0 else Subtree'Length),
      Annotate => (GNATprove, Terminating);

   ---------------
   -- Has_Space --
   ---------------

   function Has_Space (Subtree : Document_Type;
                       Offset  : Offset_Type;
                       Name    : Attr_Data_Type) return Boolean
   is (Offset < Subtree'Length and then
       Offset < Offset_Type (Index_Type'Last) and then
       Num_Attr_Elements (Name) < Offset_Type (Sub (Index_Type'Last, Offset)) and then
       Subtree'First <= Sub (Sub (Index_Type'Last, Offset), Num_Attr_Elements (Name)) and then
       Natural (Subtree'Length) - Natural (Offset) >= Natural (Num_Attr_Elements (Name)));

   ----------
   -- Open --
   ----------

   procedure Open (Name     : Content_Type;
                   Output   : in out Document_Type;
                   Position : in out Index_Type;
                   Start    : out Index_Type)
   with
      Pre  => (Position in Output'Range and
               Position < Index_Type'Last) and then
              Has_Space (Output, Sub (Position, Output'First), Name),
      Post => Start in Output'Range and
              Is_Open (Output (Start)) and
              Position = Add (Position'Old, Num_Elements (Name));

private

   type Length_Type is range 0 .. Chunk_Length;
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
   Null_Tree : constant Document_Type := (1 .. 0 => Null_Node);

   function Is_Valid (Left, Right : Document_Type) return Boolean
   is
      ((Num_Elements (Left) > 0 or Num_Elements (Right) > 0) and
       Left'Length <= Index_Type'Last - Right'Length);

   ----------------
   -- Is_Invalid --
   ----------------

   function Is_Invalid (Node : Node_Type) return Boolean
   is (Node.Kind = Kind_Invalid);

   -------------
   -- Is_Open --
   -------------

   function Is_Open (Node : Node_Type) return Boolean
   is (Node.Kind = Kind_Element_Open);

end SXML;
