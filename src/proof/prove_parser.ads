with SXML;

package Prove_Parser is

   function Parse (XML : String) return SXML.Subtree_Type
   with
      Pre => XML'First >= 0 and
             XML'Last < Natural'Last and
             XML'First <= XML'Last;

end Prove_Parser;
