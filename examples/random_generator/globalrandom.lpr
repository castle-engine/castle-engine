program globalrandom;

uses castle_base, CastleRandom;

var i: integer;
begin
  for i := 0 to 10 do
    writeln(rnd:5:3);
  for i := 0 to 10 do
    writeln(rnd(100));

end.

