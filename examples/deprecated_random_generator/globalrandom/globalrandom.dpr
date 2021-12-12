program GlobalRandom;

uses CastleRandom;

var i: Integer;
begin
  for i := 0 to 10 do
    WriteLn(Rand.Random:5:3);
  for i := 0 to 10 do
    WriteLn(Rand.Random(100));
end.
