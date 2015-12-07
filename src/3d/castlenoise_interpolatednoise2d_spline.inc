var
  X0, Y0, X1, Y1, X2, Y2, X3, Y3: LongInt;
  XFrac, YFrac: Single;
begin
  X1 := Floor(X);
  Y1 := Floor(Y);
  X0 := X1 - 1;
  Y0 := Y1 - 1;
  X2 := X1 + 1;
  Y2 := Y1 + 1;
  X3 := X2 + 1;
  Y3 := Y2 + 1;

  XFrac := Frac(X); if XFrac < 0 then XFrac += 1;
  YFrac := Frac(Y); if YFrac < 0 then YFrac += 1;

  Result := CatmullRom(
    CatmullRom( IntegerNoise(X0, Y0, Seed), IntegerNoise(X1, Y0, Seed), IntegerNoise(X2, Y0, Seed), IntegerNoise(X3, Y0, Seed), XFrac),
    CatmullRom( IntegerNoise(X0, Y1, Seed), IntegerNoise(X1, Y1, Seed), IntegerNoise(X2, Y1, Seed), IntegerNoise(X3, Y1, Seed), XFrac),
    CatmullRom( IntegerNoise(X0, Y2, Seed), IntegerNoise(X1, Y2, Seed), IntegerNoise(X2, Y2, Seed), IntegerNoise(X3, Y2, Seed), XFrac),
    CatmullRom( IntegerNoise(X0, Y3, Seed), IntegerNoise(X1, Y3, Seed), IntegerNoise(X2, Y3, Seed), IntegerNoise(X3, Y3, Seed), XFrac),
    YFrac);
end;
