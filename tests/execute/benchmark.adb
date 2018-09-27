--
-- \brief  SXML test runner
-- \author Alexander Senier
-- \date   2018-09-27
--
-- Copyright (C) 2018 Componolit GmbH
--
-- This file is part of SXML, which is distributed under the terms of the
-- GNU Affero General Public License version 3.
--

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with SXML_Utils; use SXML_Utils;

procedure Benchmark
is
begin
   Set_Exit_Status (1);

   if Argument_Count /= 3
   then
      Put_Line ("Insufficient arguments");
      return;
   end if;

   declare
      File_Name   : String := Argument (1);
      Num_Tests   : Natural := Natural'Value (Argument (2));
      Num_Seconds : Natural := Natural'Value (Argument (3));
   begin
      Put_Line ("Running benchmark using file """ & File_Name &
         """:" & Num_Tests'Img & " rounds in" & Num_Seconds'Img & " seconds");

      for Round in 1 .. Num_Tests
      loop
         Put (".");
         Parse_Document (File_Name);
      end loop;
   end;

end Benchmark;
