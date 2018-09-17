package SXML.Serialize is

   ---------------
   -- To_String --
   ---------------

   procedure To_String (Doc  : Subtree_Type;
                        Data : out String;
                        Last : out Natural);

end SXML.Serialize;
