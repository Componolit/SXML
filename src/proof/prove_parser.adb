with SXML.Parser;

package body Prove_Parser is

   Context : SXML.Subtree_Type (1 .. 100000);

   procedure Parse (XML    : in out String;
                    Result : out Boolean)
   is
      package P is new SXML.Parser (XML, Context);
      use P;
      Match : Match_Type;
      Position : Natural;
   begin
      P.Parse (Match    => Match,
               Position => Position);
      pragma Unreferenced (Position);
      Result := Match = Match_OK;
   end Parse;

end Prove_Parser;
