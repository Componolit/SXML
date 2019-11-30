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

   procedure To_String (Document :     Document_Type;
                        Data     : out String;
                        Last     : out Natural;
                        Result   : out Result_Type) with
     Pre => Data'Length > 0;
   --  Serialize document to string using runtime stack
   --
   --  @param Document  Document to serialize
   --  @param Data      Output string
   --  @param Last      Last valid element of output string
   --  @param Result    Result of operation

end SXML.Generic_Serialize;
