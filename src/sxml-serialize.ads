--
--  @summary XML serializer with fixed stack depth
--  @author  Alexander Senier
--  @date    2019-11-28
--
--  Copyright (C) 2019 Componolit GmbH
--
--  This file is part of SXML, which is distributed under the terms of the
--  GNU Affero General Public License version 3.
--

with SXML.Generic_Serialize;

package SXML.Serialize is new SXML.Generic_Serialize (Depth => 100);
