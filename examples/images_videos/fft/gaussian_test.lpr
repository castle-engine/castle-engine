{ Output Gaussian 1D and 2D values for given radius (with standard deviation
  derived from RadiusToStdDev).
  Outputs only the Gaussian side for positive arguments (rest is mirrored). }

uses SysUtils, Math, Gaussian, CastleParameters;
var
  Radius, StdDev: Single;
  X, Y: Integer;
begin
  Parameters.CheckHigh(1);
  Radius := StrToFloat(Parameters[1]);

  StdDev := RadiusToStdDev(Radius);
  Writeln('Gaussian with standard deviation ', StdDev:0:4);
  Writeln;

  Writeln('Gaussian 1D');
  for X := 0 to Ceil(Radius) do
      Write(Gaussian1D(X, StdDev), ' ');
  Writeln;
  Writeln;

  Writeln('Gaussian 2D');
  for Y := 0 to Ceil(Radius) do
  begin
    for X := 0 to Ceil(Radius) do
      Write(Gaussian2D(X, Y, StdDev), ' ');
    Writeln;
  end;
end.
