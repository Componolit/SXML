with SXML.Parser;

package body Prove_Parser is

   Context : SXML.Subtree_Type (1 .. 100000);

   procedure Parse (XML    : String;
                    Result : out Boolean)
   is
      package Parser is new SXML.Parser (XML, Context);
      use Parser;
      Match : Match_Type;
   begin
      Parser.Parse (Match => Match);
      Result := Match = Match_OK;
   end Parse;

end Prove_Parser;
