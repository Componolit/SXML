with SXML.Parser;

procedure Doc
with SPARK_Mode
is
   Stack_Buffer : SXML.Parser.Stack_Type_Base (1 .. 1000) := (others => SXML.Parser.Null_Parser_State);

   use SXML;
   use SXML.Parser;
   Text : String := 
        "<?xml version=""1.0""?>"
      & "<?xml-stylesheet type='text/xsl' href='foo.xsl'?>"
      & "<parent>"
      & "   <?xml-stylesheet type='text/xsl' href='bar.xsl'?>"
      & "   <child/>"
      & "</parent>"
      & "<?xml-stylesheet type='text/xsl' href='baz.xsl'?>";

      Document : SXML.Document_Type (1 .. 100) := (others => Null_Node);
      Result   : Match_Type;
      Position : Natural;
begin
   Parse (Data         => Text,
          Document     => Document,
          Buffer       => Stack_Buffer,
          Parse_Result => Result,
          Position     => Position); 
end Doc;
