package SXML.Serialize
is
   type Traversal_Type is private;
   Null_Traversal : constant Traversal_Type;

   type Stack_Type is array (SXML.Natural_Without_Last range <>) of Traversal_Type
   with
      Dynamic_Predicate => Stack_Type'First <= Stack_Type'Last and
                           Stack_Type'Length > 3;

   ---------------
   -- To_String --
   ---------------

   procedure To_String (Document : Document_Type;
                        Data     : out String;
                        Last     : out Natural;
                        Result   : out Result_Type)
   with
      Pre => Data'Length > 0;
   --  Serialize document to string using runtime stack
   --
   --  @param Document  Document to serialize
   --  @param Data      Output string
   --  @param Last      Last valid element of output string
   --  @param Result    Result of operation

   ---------------
   -- To_String --
   ---------------

   procedure To_String (Document : Document_Type;
                        Data     : out String;
                        Last     : out Natural;
                        Result   : out Result_Type;
                        Buffer   : in out Stack_Type)
   with
      Pre => Data'Length > 0 and
             Buffer'Length > 1;
   --  Serialize document to string using heap stack
   --
   --  @param Document  Document to serialize
   --  @param Data      Output string
   --  @param Last      Last valid element of output string
   --  @param Result    Result of operation
   --  @param Buffer    Stack buffer

private

   type Mode_Type is (Mode_Invalid, Mode_Open, Mode_Close);

   type Traversal_Type is
   record
      Index : Index_Type;
      Mode  : Mode_Type;
   end record;

   Null_Traversal : constant Traversal_Type := (Invalid_Index, Mode_Invalid);

end SXML.Serialize;
