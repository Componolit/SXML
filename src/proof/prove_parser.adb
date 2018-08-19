with SXML.Parser;

package body Prove_Parser is

   function Parse (XML : String) return SXML.Subtree_Type
   is
      package Parser is new SXML.Parser (XML);
      use Parser;
      Result : Match_Type;
   begin
      Parser.Parse (Match => Result);
      if Result /= Match_OK
      then
         return SXML.Null_Tree;
      end if;
      return Parser.Document;
   end Parse;

end Prove_Parser;
