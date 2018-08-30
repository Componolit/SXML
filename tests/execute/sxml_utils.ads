--
-- @summary Utils for SXML tests
-- @author  Alexander Senier
-- @date    2018-08-30
--
-- Copyright (C) 2018 Componolit GmbH
--
-- This file is part of SXML, which is distributed under the terms of the
-- GNU Affero General Public License version 3.
--

package SXML_Utils
is
   procedure Check_Invalid (Input : String);

   function Read_File (File_Name : String) return String;

   procedure Parse_Document (File : String);

   procedure Check_Document (Input    : String;
                             Output   : String := "INPUT");
end SXML_Utils;
