--
-- \brief  SXML test suite
-- \author Alexander Senier
-- \date   2018-08-12
--
-- Copyright (C) 2018 Componolit GmbH
--
-- This file is part of SXML, which is distributed under the terms of the
-- GNU Affero General Public License version 3.
--

-- Import tests and sub-suites to run
with SXML_Generator_Tests;
with SXML_Parser_Tests;
with SXML_Bulk_Tests;
with SXML_Query_Tests;
with SXML_Queue_Tests;

package body SXML_Suite is

   use AUnit.Test_Suites;

   -- Statically allocate test suite:
   Result : aliased Test_Suite;

   --  Statically allocate test cases:
   Generator_Tests : aliased SXML_Generator_Tests.Test_Case;
   Parser_Tests    : aliased SXML_Parser_Tests.Test_Case;
   Bulk_Tests      : aliased SXML_Bulk_Tests.Test_Case;
   Query_Tests     : aliased SXML_Query_Tests.Test_Case;
   Queue_Tests     : aliased SXML_Queue_Tests.Test_Case;

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Generator_Tests'Access);
      Add_Test (Result'Access, Parser_Tests'Access);
      Add_Test (Result'Access, Bulk_Tests'Access);
      Add_Test (Result'Access, Query_Tests'Access);
      Add_Test (Result'Access, Queue_Tests'Access);
      return Result'Access;
   end Suite;

end SXML_Suite;
