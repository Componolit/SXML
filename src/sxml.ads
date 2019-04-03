--
--  @summary Common data structures and operations
--  @author  Alexander Senier
--  @date    2018-11-15
--
--  Copyright (C) 2018 Componolit GmbH
--
--  This file is part of SXML, which is distributed under the terms of the
--  GNU Affero General Public License version 3.
--

package SXML
is
   Chunk_Length : constant := 8;

   type Result_Type is
   (
      Result_OK,        --  Result OK
      Result_Overflow,  --  Operation would cause overflow
      Result_Invalid,   --  Invalid input data
      Result_Not_Found  --  Queried data not found
   );

   function Valid_Content (First : Integer;
                           Last  : Integer) return Boolean is
      (First >= 0 and then
       First <= Last and then
       Last <= Natural'Last - Chunk_Length);
   --  Check validity of the range of a content type
   --
   --  @param First  First index
   --  @param Last   Last index

   subtype Content_Base_Type is String
   with
      Predicate => Content_Base_Type'First >= 0
                   and then Content_Base_Type'Last <= Natural'Last - Chunk_Length;
   Empty_Content : constant Content_Base_Type := "";

   subtype Content_Type is Content_Base_Type
   with
      Predicate => Content_Type'Length > 0
                   and then Content_Type'First <= Content_Type'Last;

   type Index_Type is range 1 .. Natural'Last - 1;
   Invalid_Index : constant Index_Type := Index_Type'Last;

   type Relative_Index_Type is range 0 .. Natural'Last;
   Invalid_Relative_Index : constant Relative_Index_Type := Relative_Index_Type'First;

   type Offset_Type is new Natural range 0 .. Natural'Last;
   Null_Offset : constant Offset_Type := Offset_Type'First;

   subtype Natural_Without_Last is Natural range Natural'First .. Natural'Last - 1;
   --  Natural subtype that is useful for arrays, as we can use 'Length without
   --  having to show that 'Last is < Natural'Last.

   type Kind_Type is (Kind_Invalid,
                      Kind_Element_Open,
                      Kind_Content,
                      Kind_Attribute,
                      Kind_Data);

   type Node_Type (Kind : Kind_Type := Kind_Invalid) is private;
   Null_Node : constant Node_Type (Kind => Kind_Invalid);

   type Document_Base_Type is array (Index_Type range <>) of Node_Type;
   Null_Document : constant Document_Base_Type;

   function Valid_Document (Document : Document_Base_Type) return Boolean is
      (Document'First > 0 and then Document'Length > 0) with
      Ghost;

   subtype Document_Type is Document_Base_Type with
     Dynamic_Predicate => Document_Type'First > 0
                          and Document_Type'Length > 0
                          and Document_Type'Last < Index_Type'Last;

   ---------
   -- Add --
   ---------

   function Add (Left  : Offset_Type;
                 Right : Relative_Index_Type) return Offset_Type
   is (Left + Offset_Type (Right))
   with
      Pre      => Offset_Type (Right) <= Offset_Type'Last - Left,
      Annotate => (GNATprove, Terminating),
      Annotate => (GNATprove, Inline_For_Proof);
   --  Add relative index to offset
   --
   --  @param Left   Offset
   --  @param Right  Relative offset

   function Add (Left  : Index_Type;
                 Right : Offset_Type) return Index_Type
   is (Index_Type (Natural (Left) + Natural (Right)))
   with
      Pre      => Right <= Offset_Type (Index_Type'Last - Left),
      Annotate => (GNATprove, Terminating),
      Annotate => (GNATprove, Inline_For_Proof);
   --  Add offset to index
   --
   --  @param Left   Index
   --  @param Right  Offset

   function Add (Left  : Index_Type;
                 Right : Relative_Index_Type) return Index_Type
   is (Index_Type (Natural (Left) + Natural (Right)))
   with
      Pre      => Right <= Relative_Index_Type (Index_Type'Last - Left),
      Annotate => (GNATprove, Terminating),
      Annotate => (GNATprove, Inline_For_Proof);
   --  Add relative index to index
   --
   --  @param Left   Index
   --  @param Right  Relative offset

   --------------
   -- Overflow --
   --------------

   function Overflow (Left  : Offset_Type;
                      Right : Relative_Index_Type) return Boolean
   is (Offset_Type (Right) > Offset_Type'Last - Left);
   --  Check whether adding a relative index would overflow an offset
   --
   --  @param Left   Offset
   --  @param Right  Relative index

   function Overflow (Left  : Index_Type;
                      Right : Relative_Index_Type) return Boolean
   is (Right > Relative_Index_Type (Index_Type'Last - Left));
   --  Check whether adding a relative index would overflow an index
   --
   --  @param Left   Index
   --  @param Right  Relative index

   function Overflow (Left  : Index_Type;
                      Right : Offset_Type) return Boolean
   is (Right > Offset_Type (Index_Type'Last - Left));
   --  Check whether adding an index would overflow an index
   --
   --  @param Left   Index
   --  @param Right  Relative index

   ---------
   -- Sub --
   ---------

   function Sub (Left  : Index_Type;
                 Right : Index_Type) return Relative_Index_Type
   is (Relative_Index_Type (Left - Right))
   with
      Pre      => Left >= Right,
      Annotate => (GNATprove, Terminating);
   --  Substract index from index yielding realtive index
   --
   --  @param Left   Index
   --  @param Right  Index

   function Sub (Left  : Index_Type;
                 Right : Index_Type) return Offset_Type
   is (Offset_Type (Left - Right))
   with
      Pre      => Left >= Right,
      Annotate => (GNATprove, Terminating);
   --  Substract index from index yielding offset
   --
   --  @param Left   Index
   --  @param Right  Index

   function Sub (Left  : Index_Type;
                 Right : Offset_Type) return Index_Type
   is (Index_Type (Offset_Type (Left) - Right))
   with
      Pre      => Offset_Type (Left) > Right,
      Annotate => (GNATprove, Terminating);
   --  Substract offset from index yielding index
   --
   --  @param Left   Index
   --  @param Right  Offset

   ---------------
   -- Underflow --
   ---------------

   function Underflow (Left  : Index_Type;
                       Right : Offset_Type) return Boolean
   is (Right >= Offset_Type (Left));
   --  Check whether subtracting an offset would underflow an index
   --
   --  @param Left   Index
   --  @param Right  Offset

   function Underflow (Left  : Index_Type;
                       Right : Index_Type) return Boolean
   is (Right > Left);
   --  Check whether subtracting an index would underflow an index
   --
   --  @param Left   Index
   --  @param Right  Index

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Left, Right : Document_Base_Type) return Boolean with
     Ghost,
     Annotate => (GNATprove, Inline_For_Proof);

   -------------
   -- Is_Open --
   -------------

   function Is_Open (Node : Node_Type) return Boolean
   with
      Post => (if Is_Open'Result then Node.Kind = Kind_Element_Open);
   --  Node is an opening element
   --
   --  @param Node  Document node

   ----------------
   -- Is_Invalid --
   ----------------

   function Is_Invalid (Node : Node_Type) return Boolean
   with
      Post => (if Is_Invalid'Result then Node.Kind = Kind_Invalid);
   --  Node is an invalid element
   --
   --  @param Node  Document node

   ------------
   -- Append --
   ------------

   procedure Append (Left   : in out Document_Type;
                     Offset :        Offset_Type;
                     Right  :        Document_Type)
   with
      Pre => Offset < Offset_Type (Index_Type'Last) and then
             Offset < Left'Length and then
             Left'Length - Offset <= Right'Length and then
             Right'Length <= Offset_Type'Last - Offset and then
             Right'Length <= Left'Length - Offset,
      Annotate => (GNATprove, Terminating);
   --  Append a document inplace
   --
   --  @param Left    Document to append to
   --  @param Offset  Offset to append at
   --  @param Right   Document to append

   -----------------
   -- Put_Content --
   -----------------

   procedure Put_Content (Document : in out Document_Type;
                          Offset   : Offset_Type;
                          Value    : Content_Type)
   with
      Pre => Has_Space (Document, Offset, Value);
   --  Write content to a document at the specified offset
   --
   --  @param Document Document to write content into
   --  @param Offset   Document offset to write data to
   --  @param Value    Value to write to document

   ---------------
   -- Attribute --
   ---------------

   procedure Attribute (Name     : Content_Type;
                        Data     : Content_Base_Type;
                        Offset   : in out Offset_Type;
                        Document : in out Document_Type) with
     Pre => Offset <= Offset_Type (Document'Length) - Length (Data) - Length (Name);
   --  Write attribute to document at the specified offset
   --
   --  @param Name     Attribute name
   --  @param Data     Attribute data
   --  @param Offset   Document offset to write attribute to
   --  @param Document Document to write attribute to

   -------------------
   -- String_Length --
   -------------------

   function String_Length (Document : Document_Type;
                           Offset   : Offset_Type) return Natural
   with
      Pre      => Offset < Document'Length,
      Post     => String_Length'Result <= Natural'Last - Chunk_Length,
      Annotate => (Gnatprove, Terminating);
   --  Return length of string at given offset
   --
   --  @param Document Source document
   --  @param Offset   Offset of string

   ----------------
   -- Get_String --
   ----------------

   procedure Get_String (Document : Document_Type;
                         Start    : Offset_Type;
                         Result   : out Result_Type;
                         Data     : out Content_Type;
                         Last     : out Natural)
   with
      Pre      => Start < Document'Length and
                  Valid_Content (Data'First, Data'Last),
      Post     => (if Result = Result_OK
                   then Last in Data'Range),
      Annotate => (Gnatprove, Terminating);
   --  Extract string at given position
   --
   --  @param Document Source document
   --  @param Start    Offset to extract string from
   --  @param Result   Result of operation
   --  @param Data     Variable to write result to
   --  @param Last     Last valid element in result data

   ------------------
   -- Num_Elements --
   ------------------

   function Length (D : Content_Base_Type) return Offset_Type
   with
      Post     => Length'Result = (D'Length + (Chunk_Length - 1)) / Chunk_Length,
      Annotate => (GNATprove, Terminating),
      Annotate => (GNATprove, Inline_For_Proof);
   --  Number of elements to store content
   --
   --  @param D  Element content

   function Num_Elements (Document : Document_Base_Type) return Offset_Type
   with
      Post     => Num_Elements'Result = (if Document = Null_Document then 0 else Document'Length),
      Annotate => (GNATprove, Terminating),
      Annotate => (GNATprove, Inline_For_Proof);
   --  Number of elements in document
   --
   --  @param Document  Document

   ---------------
   -- Has_Space --
   ---------------

   function Has_Space (Document : Document_Type;
                       Offset   : Offset_Type;
                       Name     : Content_Base_Type) return Boolean is
     (Offset <= Offset_Type (Document'Length) - Length (Name));

   --  Check whether document has sufficient space to store name at offset
   --
   --  @param Document  Document to calculate space for
   --  @param Offset    Offset to calculate space at
   --  @param Name      Name to calculate space for

   ----------
   -- Open --
   ----------

   procedure Open (Name     : Content_Type;
                   Document : in out Document_Type;
                   Position : in out Index_Type;
                   Start    : out Index_Type)
   with
      Pre  => Position in Document'Range and then
              Has_Space (Document, Sub (Position, Document'First), Name),
      Post => Start in Document'Range and
              Is_Open (Document (Start)) and
              Position = Add (Position'Old, Length (Name));
   --  Write open element to document
   --
   --  @param Name      Content to write to document
   --  @param Document  Document to write content to
   --  @param Position  Position to write content to, updated to next available element
   --  @param Start     Start offset of open element

   pragma Warnings (Off, "precondition is statically False");
   overriding function "&" (Left, Right : Document_Type) return Document_Type
   with Pre => False;
   --  This operator must not be used, as subtrees have to be
   --  linked together. This is done by the * operator above.
   --
   --  @param Left  Unused
   --  @param Right Unused
   pragma Warnings (On, "precondition is statically False");

private

   type Length_Type is range 0 .. Chunk_Length;
   subtype Data_Type is String (1 .. Natural (Length_Type'Last));
   Null_Data : constant Data_Type := (others => Character'Val (0));

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
   Null_Document : constant Document_Base_Type := (1 .. 0 => Null_Node);

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Left, Right : Document_Base_Type) return Boolean
   is
      ((Num_Elements (Left) > 0 or Num_Elements (Right) > 0) and
       Num_Elements (Left) < Offset_Type (Index_Type'Last) - Num_Elements (Right));

end SXML;
