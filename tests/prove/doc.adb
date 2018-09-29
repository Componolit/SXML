with SXML.Parser;

procedure Document
with SPARK_Mode
is
   use SXML;
   use SXML.Parser;
   Document : String := 
        "<?xml version=""1.0""?>"
      & "<?xml-stylesheet type='text/xsl' href='foo.xsl'?>"
      & "<parent>"
      & "   <?xml-stylesheet type='text/xsl' href='bar.xsl'?>"
      & "   <child/>"
      & "</parent>"
      & "<?xml-stylesheet type='text/xsl' href='baz.xsl'?>";

      Context  : SXML.Subtree_Type (1 .. 100) := (others => Null_Node);
      Result   : Match_Type;
      Position : Natural;
begin
   Parse (Data         => Document,
          Context      => Context,
          Parse_Result => Result,
          Position     => Position); 
end Document;
