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
with Text_IO;

package body SXML_Utils
is
   Context : access SXML.Subtree_Type := new SXML.Subtree_Type (1 .. 150000000);

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
      Data : access String := new String (1 .. 2 * Input'Length);
      Last : Natural := 1;
   begin
      Parser.Parse (Match    => Result,
                    Position => Position);
      Assert (Result = Match_OK,
              File & ":" & Position'Img(2..Position'Img'Last) & ": Invalid result: " & Result'Img);

      To_String (Context.all, Data.all, Last);
      Assert (Last > 0, File & ": Serialization error");
   end Parse_Document;

   --------------------
   -- Check_Document --
   --------------------

   procedure Check_Document (Input                : in out String;
                             Output               : String := "INPUT";
                             Ignore_Final_Newline : Boolean := False)
   is
      package Parser is new SXML.Parser (Input, Context.all);
      use SXML;
      use Parser;
      Result   : Match_Type;
      Position : Natural;
      XML      : access String := new String (1 .. 2 * Input'Length);
      Last     : Integer := 0;
   begin
      Context.all (1) := Null_Node;
      Parser.Parse (Match    => Result,
                    Position => Position);
      Assert (Result = Match_OK, "Invalid result: " & Result'Img & " (Pos:" & Position'Img  & ")");

      To_String (Context.all, XML.all, Last);
      Assert (Last >= 0, "Error serializing");
      Assert (XML.all (1 .. Last) = (if Output = "INPUT" then
                                        (if Ignore_Final_Newline then
                                            Input (Input'First .. Input'Last - 1)
                                         else Input)
                                     else Output),
         "Invalid result at" & Position'Img &
         ": (" & XML.all (1 .. Last) & "), expected: (" & (if Output = "INPUT" then Input else Output) &
         "), Last=" & Last'Img);
   end Check_Document;

   -------------------------
   -- Generate_Deep_Nodes --
   -------------------------

   procedure Generate_Deep_Nodes (Name  : String;
                                  Level : Natural)
   is
      use Text_IO;
      File : File_Type;
   begin
      Create (File, Out_File, Name);
      for I in 1 .. Level
      loop
         declare
            X : constant Natural := I mod 10;
         begin
            Put (File, "<" & X'Img (2 .. 2) & ">");
         end;
      end loop;
      for I in reverse 1 .. Level
      loop
         declare
            X : constant Natural := I mod 10;
         begin
            Put (File, "</" & X'Img (2 .. 2) & ">");
         end;
      end loop;
      Close (File);
   end Generate_Deep_Nodes;

   -------------------------
   -- Generate_Many_Nodes --
   -------------------------

   procedure Generate_Many_Nodes (Name  : String;
                                  Level : Natural)
   is
      use Text_IO;
      File : File_Type;
   begin
      Create (File, Out_File, Name);
      Put (File, "<root>");
      for I in 1 .. Level
      loop
         declare
            X : constant Natural := I mod 10;
         begin
            Put (File, "<" & X'Img (2 .. 2) & "/>");
         end;
      end loop;
      Put (File, "</root>");
      Close (File);
   end Generate_Many_Nodes;

   ------------------------------
   -- Generate_Large_Attribute --
   ------------------------------

   procedure Generate_Large_Attribute (Name  : String;
                                       Level : Natural)
   is
      use Text_IO;
      File : File_Type;
   begin
      Create (File, Out_File, Name);
      Put (File, "<document attr=""");
      for I in 1 .. Level
      loop
         Put (File, I'Img);
      end loop;
      Put (File, """></document>");
      Close (File);
   end Generate_Large_Attribute;

   ------------------------------
   -- Generate_Many_Attributes --
   ------------------------------

   procedure Generate_Many_Attributes (Name  : String;
                                       Level : Natural)
   is
      use Text_IO;
      File : File_Type;
   begin
      Create (File, Out_File, Name);
      Put (File, "<document");
      for I in 1 .. Level
      loop
         Put (File, " attr=""" & I'Img & """");
      end loop;
      Put (File, "></document>");
      Close (File);
   end Generate_Many_Attributes;

   --------------------------
   -- Generate_Large_CDATA --
   --------------------------

   procedure Generate_Large_CDATA (Name  : String;
                                   Level : Natural)
   is
      use Text_IO;
      File : File_Type;
   begin
      Create (File, Out_File, Name);
      Put (File, "<document><![CDATA[");
      for I in 1 .. Level
      loop
         Put (File, I'Img);
      end loop;
      Put (File, """]]></document>");
      Close (File);
   end Generate_Large_CDATA;

end SXML_Utils;
