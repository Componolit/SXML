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
with Ada.Direct_IO;
with Ada.Directories;
with SXML;
with SXML.Parser;

package body SXML_Utils
is
   Context : SXML.Subtree_Type (1 .. 1000000);

   -------------------
   -- Check_Invalid --
   -------------------
 
   procedure Check_Invalid (Input : in out String)
   is
      package Parser is new SXML.Parser (Input, Context);
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

   function Read_File (File_Name : String) return String
   is
      File_Size : Natural := Natural (Ada.Directories.Size (File_Name));
      subtype File_String is String (1 .. File_Size);
      package File_String_IO is new Ada.Direct_IO (File_String);

      File     : File_String_IO.File_Type;
      Contents : File_String;
   begin
      File_String_IO.Open (File, File_String_IO.In_File, File_Name);
      File_String_IO.Read (File, Contents);
      File_String_IO.Close (File);
      return Contents;
   end Read_File;

   procedure Parse_Document (File : String)
   is
      Input : access String := Read_File (File);
      package Parser is new SXML.Parser (Input.all, Context);
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
      package Parser is new SXML.Parser (Input, Context);
      use SXML;
      use Parser;
      Result   : Match_Type;
      Position : Natural;
   begin
      Context := (others => Null_Node);
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
