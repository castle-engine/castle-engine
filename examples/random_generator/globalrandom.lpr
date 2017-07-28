program GlobalRandom;

uses castle_base, CastleRandom;

var i: Integer;
begin
  for i := 0 to 10 do
    WriteLn(Rnd:5:3);
  for i := 0 to 10 do
    WriteLn(Rnd(100));
end.

