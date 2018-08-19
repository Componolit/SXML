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

procedure Tests is
   procedure Run is new AUnit.Run.Test_Runner (SXML_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (Reporter);
end Tests;
