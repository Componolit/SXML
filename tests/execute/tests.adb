--
-- \brief  SXML test runner
-- \author Alexander Senier
-- \date   2018-08-12
--
-- Copyright (C) 2018 Componolit GmbH
--
-- This file is part of SXML, which is distributed under the terms of the
-- GNU Affero General Public License version 3.
--

with SXML_Suite;
with AUnit.Run;
with AUnit.Reporter.Text;
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

procedure Tests is
   function Run is new AUnit.Run.Test_Runner_With_Status (SXML_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   use AUnit;
   S : Status;
begin
   Put_Line ("Running SXML tests...");
   Reporter.Set_Use_ANSI_Colors (True);
   S := Run (Reporter);
   Ada.Command_Line.Set_Exit_Status ((if (S = Success) then 0 else 1));
end Tests;
