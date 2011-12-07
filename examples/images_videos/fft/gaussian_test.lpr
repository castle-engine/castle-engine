uses SysUtils, Math, Gaussian, CastleParameters;
var
  Radius, StdDev: Single;
  X, Y: Integer;
begin
  Parameters.CheckHigh(1);
  Radius := StrToFloat(Parameters[1]);
  StdDev := RadiusToStdDev(Radius);
  Writeln('Gaussian with standard deviation ', StdDev:0:4);
  for Y := -Floor(Radius) to Ceil(Radius) do
  begin
    for X := -Floor(Radius) to Ceil(Radius) do
      Write(Gaussian2D(X, Y, StdDev) :0:4, ' ');
    Writeln;
  end;
end.
