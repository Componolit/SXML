--
--  @summary XML parser specification
--  @author  Alexander Senier
--  @date    2018-11-15
--
--  Copyright (C) 2018 Componolit GmbH
--
--  This file is part of SXML, which is distributed under the terms of the
--  GNU Affero General Public License version 3.
--

package SXML.Parser
is
   type Match_Type is (Match_OK,
                       Match_None,
                       Match_Invalid,
                       Match_Out_Of_Memory,
                       Match_None_Wellformed,
                       Match_Depth_Limit,
                       Match_Trailing_Data);
   --  Result of a parsing operation
   --
   --  @value Match_OK              XML document parsed successfully
   --  @value Match_None            No XML data found
   --  @value Match_Invalid         Malformed XML data found
   --  @value Match_Out_Of_Memory   Out of context buffer memory, increase
   --                               generic Context_Size parameter when
   --                               instanciating package
   --  @value Match_None_Wellformed Document is not wellformed
   --  @value Match_Trailing_Data   Document successful parsed, but there is
   --                               trailing data after it
   --  @value Match_Depth_Limit     Recursion depth exceeded

   -----------
   -- Parse --
   -----------

   procedure Parse (Data         :        Content_Type;
                    Document     : in out Document_Type;
                    Parse_Result :    out Match_Type;
                    Position     :    out Natural);
   --  Parse an XML file
   --
   --  @param Data          Input data
   --  @param Document      Document
   --  @param Parse_Result  Result of operation
   --  @param Position      Input location after parsing or error

end SXML.Parser;
