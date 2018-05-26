with XML;
use XML;

procedure Test
is
   A : constant Arguments_Type := (+"foo", +"bar", +"baz");
begin
   XML.Exec ("foo", A);
end Test;
