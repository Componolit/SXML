--
--  @summary XML serialization specification
--  @author  Alexander Senier
--  @date    2018-11-15
--
--  Copyright (C) 2018 Componolit GmbH
--
--  This file is part of SXML, which is distributed under the terms of the
--  GNU Affero General Public License version 3.
--

generic
   Depth : Positive;
package SXML.Generic_Serialize with
   Abstract_State => State,
   Initializes    => State
is
   ---------------
   -- To_String --
   ---------------

   --  Serialize document to string using runtime stack
   --
   --  @param Document  Document to serialize
   --  @param Data      Output string
   --  @param Offset    Offset of last valid character of output string
   --  @param Result    Result of operation
   procedure To_String (Document :     Document_Type;
                        Data     : out String;
                        Offset   : out Natural;
                        Result   : out Result_Type) with
     Global => (In_Out => State),
     Pre    => Data'Length > 0,
     Post   => (if Result = Result_OK then Offset < Data'Length);

end SXML.Generic_Serialize;
