{$define PRECALC_VALUE_RETURN:=
begin
 if not (PRECALC_VALUE_ENUM in Validities) then
 begin
  PRECALC_VALUE := PRECALC_VALUE_CALCULATE;
  Include(Validities, PRECALC_VALUE_ENUM);
 end;
 Result := PRECALC_VALUE;
end;}
