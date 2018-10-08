--
-- @summary Bulk tests for SXML parser
-- @author  Alexander Senier
-- @date    2018-08-30
--
-- Copyright (C) 2018 Componolit GmbH
--
-- This file is part of SXML, which is distributed under the terms of the
-- GNU Affero General Public License version 3.
--

with Ada.Text_IO;
with AUnit.Assertions; use AUnit.Assertions;
with SXML.Parser;
with SXML_Utils; use SXML_Utils;
with GNAT.OS_Lib;
with Ada.Text_IO;

package body SXML_Bulk_Tests is

   procedure Test_URL (T : in out Test_Cases.Test_Case'Class)
   is
      URL : constant String := T.Routine_Name.all;
   begin
      Ada.Text_IO.Put_Line ("Testing " & URL);
      Parse_Document ("obj/document/" & URL (9..URL'Last), 15000000);
	end Test_URL;

   ---------------------------------------------------------------------------

   procedure Register_Tests (T: in out Test_Case) is
      use AUnit.Test_Cases.Registration;
      use Ada.Text_IO;
      File : File_Type;
      use GNAT.OS_Lib;
      Run_Bulk_Tests : access String := null;
      Run_Insane_Tests : access String := null;
   begin
      Run_Bulk_Tests := Getenv ("SXML_BULK_TESTS");
      Run_Insane_Tests := Getenv ("SXML_INSANE_TESTS");
      if (Run_Bulk_Tests = null or else
          Run_Bulk_Tests.all = "") and
         (Run_Insane_Tests = null or else
          Run_Insane_Tests.all = "")
      then
         return;
      end if;
      Open (File, In_File, (if Run_Insane_Tests /= null and then Run_Insane_Tests.all /= ""
                            then "tests/data/bulk_insane_urls.txt"
                            else "tests/data/bulk_urls.txt"));
      while not End_Of_File (File)
      loop
         Register_Routine (T, Test_URL'Access, Get_Line (File));
      end loop;
   end Register_Tests;

   ---------------------------------------------------------------------------

   function Name (T : Test_Case) return Test_String is
   begin
      return Format ("SXML Bulk Tests");
   end Name;

end SXML_Bulk_Tests;
