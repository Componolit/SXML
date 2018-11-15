with SXML.Parser;

procedure Parse
is
   use SXML.Parser;
   Input : String :=
     "<config>"
   & "   <report delay_ms=""500""/>"
   & "   <parent-provides>"
   & "      <service name=""CAP""/>"
   & "      <service name=""CPU"">Some content</service>"
   & "   </parent-provides>"
   & "</config>";
   Document : SXML.Document_Type (1 .. 100) := (others => SXML.Null_Node);
   Result   : Match_Type;
   Position : Natural;
begin
   Parse (Data         => Input,
          Document     => Document,
          Parse_Result => Result,                                                                                                           
          Position     => Position);
   if Result /= Match_OK
   then
      null;
   end if;
end Parse;
