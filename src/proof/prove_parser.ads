package Prove_Parser is

   procedure Parse (XML    : String;
                    Result : out Boolean)
   with
      Pre => XML'First >= 0 and
             XML'Last < Natural'Last and
             XML'First <= XML'Last;

end Prove_Parser;
