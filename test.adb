with XML;
use XML;

procedure Test
is
   A : constant Arguments_Type (1..1) := (others => (others => ' '));
begin
   XML.Exec ("foo", A);
end Test;
