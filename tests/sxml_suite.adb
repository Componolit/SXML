--
-- \brief  SXML test suite
-- \author Alexander Senier
-- \date   2018-08-12
--
-- Copyright (C) 2018 Componolit GmbH
--
-- This file is part of JWX, which is distributed under the terms of the
-- GNU Affero General Public License version 3.
--

-- Import tests and sub-suites to run
with SXML_Generator_Tests;

package body SXML_Suite is

   use AUnit.Test_Suites;

   -- Statically allocate test suite:
   Result : aliased Test_Suite;

   --  Statically allocate test cases:
   Generator_Tests : aliased SXML_Generator_Tests.Test_Case;

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Generator_Tests'Access);
      return Result'Access;
   end Suite;

end SXML_Suite;
