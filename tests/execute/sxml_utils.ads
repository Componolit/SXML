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
   procedure Check_Invalid (Input       : String;
                            Stack_Depth : Positive := 100);

   function Read_File (File_Name : String) return access String;

   procedure Parse_Document (File        : String;
                             Stack_Depth : Natural := 10000);

   procedure Check_Document (Input                : String;
                             Output               : String := "INPUT";
                             Ignore_Final_Newline : Boolean := False;
                             Stack_Depth          : Positive := 100);

   procedure Generate_Deep_Nodes (Name  : String;
                                  Level : Natural);

   procedure Generate_Many_Nodes (Name  : String;
                                  Level : Natural);

   procedure Generate_Large_Attribute (Name  : String;
                                       Level : Natural);

   procedure Generate_Many_Attributes (Name  : String;
                                       Level : Natural);

   procedure Generate_Large_CDATA (Name  : String;
                                   Level : Natural);
end SXML_Utils;
