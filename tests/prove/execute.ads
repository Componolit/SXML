with SXML;

package Execute
with
   SPARK_Mode,
   Abstract_State => (Output),
   Initializes => Output
is
   
   type Arg_Type is new String (1..100);
   type Args_Type is array (SXML.Index_Type range <>) of Arg_Type;
   
   function "+" (Value : String) return Arg_Type
   with
      Pre => Value'Length <= Arg_Type'Length;
      
   
   procedure Execute (Program   : String;
                      Arguments : Args_Type)
   with
      Global => (In_Out => Output);

end Execute;
