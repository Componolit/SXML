with SXML.Parser;

procedure Doc with
   Global => SXML.Parser.State
is
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
      Offset   : Natural;
begin
   pragma Assert (Valid_Content (Text'First, Text'Last));
   Parse (Data     => Text,
          Document => Document,
          Offset   => Offset,
          Result   => Result);
   pragma Unreferenced (Document, Result, Offset);
end Doc;
