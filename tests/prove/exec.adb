with Execute;

procedure Exec is
   use Execute;
   A : constant Args_Type := (+"foo", +"bar", +"baz");
begin
   Execute.Execute ("foo", A);
end Exec;
