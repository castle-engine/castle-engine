var
  Tmp: TT;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;