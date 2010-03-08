{ Common implementation for both
  InterpolatedNoise2D_Linear, InterpolatedNoise2D_Cosine }

var
  X1, Y1, X2, Y2: LongInt;
  X1f, Y1f, X2f, Y2f: Single;
begin
  X1 := Floor(X);
  Y1 := Floor(Y);
  X2 := X1 + 1;
  Y2 := Y1 + 1;

  X2f := Frac(X); if X2f < 0 then X2f += 1;
  Y2f := Frac(Y); if Y2f < 0 then Y2f += 1;
  {$ifdef InterpolatedNoise2D_Cosine}
  X2f := Cos(Pi * (1 - X2f)) / 2 + 0.5;
  Y2f := Cos(Pi * (1 - Y2f)) / 2 + 0.5;
  {$endif InterpolatedNoise2D_Cosine}
  X1f := 1 - X2f;
  Y1f := 1 - Y2f;

  Result :=
    IntegerNoise(X1, Y1, Seed) * X1f * Y1f +
    IntegerNoise(X2, Y1, Seed) * X2f * Y1f +
    IntegerNoise(X1, Y2, Seed) * X1f * Y2f +
    IntegerNoise(X2, Y2, Seed) * X2f * Y2f;
end;
