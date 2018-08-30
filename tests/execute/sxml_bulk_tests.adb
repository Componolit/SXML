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

package body SXML_Bulk_Tests is

   Context : SXML.Subtree_Type (1 .. 1000000);

   procedure Test_URL (T : in out Test_Cases.Test_Case'Class)
   is
      use GNAT.OS_Lib;
      URL : constant String := T.Routine_Name.all;
      Args : Argument_List := (1 => new String'("-r"),
                               2 => new String'("--quiet"),
                               3 => new String'("--directory-prefix=obj/document"),
                               4 => new String'(URL));
      Result : Integer;
   begin
      Result := Spawn (Locate_Exec_On_Path ("wget").all, Args);
      Assert (Result = 0, "Error downloading " & URL & ":" & Result'Img);
      Parse_Document ("obj/document/" & URL (9..URL'Last));

	end Test_URL;

   ---------------------------------------------------------------------------

   procedure Register_Tests (T: in out Test_Case) is
      use AUnit.Test_Cases.Registration;
      use Ada.Text_IO;
      File : File_Type;
   begin
      Open (File, In_File, "tests/data/bulk_urls.txt");
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
