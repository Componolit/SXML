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

with AUnit.Assertions; use AUnit.Assertions;
with Ada.Directories;
with Ada.Text_IO.Text_Streams;
with SXML.Generator; use SXML.Generator;
with SXML.Parser;

package body SXML_Utils
is
   Context : access SXML.Subtree_Type := new SXML.Subtree_Type (1 .. 10000000);

   -------------------
   -- Check_Invalid --
   -------------------
 
   procedure Check_Invalid (Input : in out String)
   is
      package Parser is new SXML.Parser (Input, Context.all);
      use SXML;
      use Parser;
      Result   : Match_Type;
      Position : Natural;
   begin
      Parser.Parse (Match    => Result,
                    Position => Position);
      pragma Unreferenced (Position);
      Assert (Result /= Match_OK, "Error expected");
   end Check_Invalid;

   ---------------
   -- Read_File --
   ---------------

   function Read_File (File_Name : String) return access String
   is
      Block_Size : constant := 1024;
      use Ada.Text_IO;
      use Ada.Text_IO.Text_Streams;
      File_Size : Natural := Natural (Ada.Directories.Size (File_Name));
      Result : access String := new String (1 .. File_Size);
      File : File_Type;
      Len  : Natural;
      Off  : Natural := 1;
   begin
      Open (File, In_File, File_Name);
      loop
         Len := (if File_Size - Off > Block_Size then Block_Size else File_Size - Off + 1);
         String'Read (Stream (File), Result.all (Off .. Off + Len - 1));
         Off := Off + Len;
         exit when Off >= File_Size;
      end loop;
      Close (File);
      return Result;
   end Read_File;

   --------------------
   -- Parse_Document --
   --------------------

   procedure Parse_Document (File : String)
   is
      Input : access String := Read_File (File);
      package Parser is new SXML.Parser (Input.all, Context.all);
      use SXML;
      use Parser;
      Result   : Match_Type;
      Position : Natural;
   begin
      Parser.Parse (Match    => Result,
                    Position => Position);
      Assert (Result = Match_OK,
              File & ":" & Position'Img(2..Position'Img'Last) & ": Invalid result: " & Result'Img);
   end Parse_Document;

   --------------------
   -- Check_Document --
   --------------------

   procedure Check_Document (Input    : in out String;
                             Output   : String := "INPUT")
   is
      package Parser is new SXML.Parser (Input, Context.all);
      use SXML;
      use Parser;
      Result   : Match_Type;
      Position : Natural;
   begin
      Context.all := (others => Null_Node);
      Parser.Parse (Match    => Result,
                    Position => Position);
      Assert (Result = Match_OK, "Invalid result: " & Result'Img & " (Pos:" & Position'Img  & ")");
      declare
         Doc : constant String := To_String (Parser.Document);
      begin
         --  FIXME: Remove whitespace
         Assert (Doc = (if Output = "INPUT" then Input else Output),
            "Invalid result at" & Position'Img &
            ": (" & Doc & "), expected: (" & (if Output = "INPUT" then Input else Output) & ")");
      end;
   end Check_Document;

end SXML_Utils;
