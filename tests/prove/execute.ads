with SXML.Serialize;

package Execute
with
   SPARK_Mode,
   Abstract_State => State,
   Initializes    => State
is

   type Arg_Type is new String (1..100);
   type Args_Type is array (SXML.Index_Type range <>) of Arg_Type;

   function "+" (Value : String) return Arg_Type
   with
      Pre => Value'Length <= Arg_Type'Length;


   procedure Execute (Program   : SXML.Content_Type;
                      Arguments : Args_Type)
   with
      Global => (In_Out => (State, SXML.Serialize.State)),
      Pre => Arguments'Length > 0 and Arguments'Length < 100 and Program'Length < 100 and Program'Length > 0;

end Execute;
