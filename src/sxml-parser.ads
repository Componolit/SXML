generic
   Data    : in out String;
   Context : in out Subtree_Type;
package SXML.Parser is

   type Match_Type is (Match_OK,
                       Match_None,
                       Match_Invalid,
                       Match_Out_Of_Memory,
                       Match_None_Wellformed,
                       Match_Depth_Limit);
   --  Result of a parsing operation
   --
   --  @value Match_OK             XML document parsed successfully
   --  @value Match_None           No XML data found
   --  @value Match_Invalid        Malformed XML data found
   --  @value Match_Out_Of_Memory  Out of context buffer memory, increase
   --                              generic Context_Size parameter when
   --                              instanciating package
   --  @value Match_Depth_Limit    Recursion depth exceeded

   function Data_Valid return Boolean
   is
      (Data'First >= 0 and
       Data'Last < Natural'Last and
       Data'First <= Data'Last)
   with
      Ghost;

   function Document_Valid return Boolean
   with
      Ghost;

   -----------
   -- Parse --
   -----------

   procedure Parse (Match    : out Match_Type;
                    Position : out Natural)
   with
      Pre  => Data_Valid,
      Post => (if Match = Match_OK then Document_Valid);
   --  Parse an XML file
   --
   --  @param Match  Result of parsing operation

   --------------
   -- Document --
   --------------

   function Document return Subtree_Type;
   --  Return parsed document

end SXML.Parser;
