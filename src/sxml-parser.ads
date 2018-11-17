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
   type State_Type is private;
   Null_Parser_State : constant State_Type;

   type Stack_Type is array (SXML.Natural_Without_Last range <>) of State_Type
   with
      Dynamic_Predicate => Stack_Type'First <= Stack_Type'Last and
                           Stack_Type'Length > 3;

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

   procedure Parse (Data         : Content_Type;
                    Document     : in out Document_Type;
                    Buffer       : in out Stack_Type;
                    Parse_Result : out Match_Type;
                    Position     : out Natural);
   --  Parse an XML file
   --
   --  @param Data          Input data
   --  @param Document      Document
   --  @param Buffer        Parser buffer
   --  @param Parse_Result  Result of operation
   --  @param Position      Input location after parsing or error

   procedure Parse (Data         : Content_Type;
                    Document     : in out Document_Type;
                    Parse_Result : out Match_Type;
                    Position     : out Natural);
   --  Parse an XML file
   --
   --  @param Data          Input data
   --  @param Document      Document
   --  @param Parse_Result  Result of operation
   --  @param Position      Input location after parsing or error

private

   type Range_Type is
      record
         First : Natural;
         Last  : Natural;
      end record
   with
      Predicate => (Range_Type.Last < Natural'Last and Range_Type.First <= Range_Type.Last) or
                   (Range_Type.First = Natural'Last and Range_Type.Last = 0);
   Null_Range : constant Range_Type := (Natural'Last, 0);

   type Block_Type is
      (Block_Invalid,
       Block_Content_Pre,
       Block_Open_Sections_Pre,
       Block_Open_Tag,
       Block_Open_Sections_Post,
       Block_Child,
       Block_Close);

   type State_Type is
   record
      Block   : Block_Type;
      Offset  : Integer;
      Name    : Range_Type;
      Current : Index_Type;
      Parent  : Index_Type;
      Sibling : Index_Type;
      Done    : Boolean;
   end record;

   Null_Parser_State : constant State_Type :=
      (Block   => Block_Invalid,
       Offset  => 0,
       Name    => Null_Range,
       Current => Invalid_Index,
       Parent  => Invalid_Index,
       Sibling => Invalid_Index,
       Done    => False);

end SXML.Parser;
