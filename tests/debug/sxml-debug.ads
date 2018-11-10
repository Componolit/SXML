package SXML.Debug
with
   SPARK_Mode => Off
is
   procedure Dump (Context : Document_Type;
                   Short   : Boolean := False;
                   Message : String  := "Dumping context");

end SXML.Debug;
