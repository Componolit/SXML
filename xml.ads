package XML
   with SPARK_Mode
is

   type Argument_Type is new String (1..100);
   type Arguments_Type is array (Natural range <>) of Argument_Type;

   -- Print XML suitable for sub-init
   procedure Exec (Program   : String;
                   Arguments : Arguments_Type);

   function "+" (Value : String) return Argument_Type;
                   
end XML;
