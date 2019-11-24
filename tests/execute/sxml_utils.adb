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
with Ada.Unchecked_Deallocation;
with Ada.Text_IO.Text_Streams;
with SXML.Generator; use SXML.Generator;
with SXML.Serialize; use SXML.Serialize;
with SXML.Parser;
with Text_IO;

package body SXML_Utils
is
   Document : access SXML.Document_Type := new SXML.Document_Type (1 .. 150000000);

   type String_Access is access all String;
   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);

   type Stack_Access is access all Stack_Type;
   procedure Free is new Ada.Unchecked_Deallocation (Stack_Type, Stack_Access);

   -------------------
   -- Check_Invalid --
   -------------------

   procedure Check_Invalid (Input       : String;
                            Stack_Depth : Positive := 100)
   is
      use SXML;
      use SXML.Parser;
      Result   : Match_Type;
      Position : Natural;

      procedure Parse is new Parser.Parse (Stack_Depth);
   begin
      Parse (Data         => Input,
             Document     => Document.all,
             Parse_Result => Result,
             Position     => Position);
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

   procedure Parse_Document (File        : String;
                             Stack_Depth : Natural := 10000)
   is
      Input : access String := Read_File (File);
      use SXML;
      use SXML.Parser;
      Result   : Match_Type;
      Position : Natural;
      Data : access String := new String (1 .. (if Input'Length > Natural'Last / 4 then Natural'Last else 4 * Input'Length));
      Last : Natural := 1;
      Stack : access Stack_Type;
      Serialize_Result : Result_Type;

      procedure Parse is new Parser.Parse (Stack_Depth);
   begin
      Document.all (Document.all'First) := Null_Node;
      Parse (Data         => Input.all,
             Document     => Document.all,
             Parse_Result => Result,
             Position     => Position);
      Free (Input);
      Assert (Result = Match_OK,
              File & ":" & Position'Img(2..Position'Img'Last) & ": Invalid result: " & Result'Img);
      Stack := new Stack_Type (1 .. Stack_Depth);
      To_String (Document.all, Data.all, Last, Serialize_Result, Stack.All);
      Free (Data);
      Free (Stack);
      Assert (Serialize_Result = Result_OK, File & ": Serialization error: " & Serialize_Result'Img);
   end Parse_Document;

   --------------------
   -- Check_Document --
   --------------------

   procedure Check_Document (Input                : String;
                             Output               : String := "INPUT";
                             Ignore_Final_Newline : Boolean := False;
                             Stack_Depth          : Positive := 100)
   is
      use SXML;
      use SXML.Parser;
      Result   : Match_Type;
      Position : Natural;
      XML      : access String := new String (1 .. 2 * Input'Length);
      Last     : Integer := 0;
      Serialize_Result : Result_Type;

      procedure Parse is new Parser.Parse (Stack_Depth);
   begin
      Document.all (1) := Null_Node;
      Parse (Data         => Input,
             Document     => Document.all,
             Parse_Result => Result,
             Position     => Position);
      Assert (Result = Match_OK, "Invalid result: " & Result'Img & " (Pos:" & Position'Img  & ")");

      To_String (Document.all, XML.all, Last, Serialize_Result);
      Assert (Serialize_Result = Result_OK, "Error serializing: " & Serialize_Result'Img);
      Assert (XML.all (1 .. Last) = (if Output = "INPUT" then
                                        (if Ignore_Final_Newline then
                                            Input (Input'First .. Input'Last - 1)
                                         else Input)
                                     else Output),
         "Invalid result at" & Position'Img &
         ": (" & XML.all (1 .. Last) & "), expected: (" & (if Output = "INPUT" then Input else Output) &
         "), Last=" & Last'Img);
      Free (XML);
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
