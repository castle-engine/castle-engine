{
  Copyright 2016-2016 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Output Gaussian 1D and 2D values for given radius. Radius is given in $1.
  Standard deviation is derived from RadiusToStdDev (using the same equation
  as GIMP), or may be given explicitly in $2.

  Outputs only the Gaussian side for positive arguments (rest is mirrored).
  Values output are separated by commas, can be easily pasted
  into source code to have hardcoded Gaussian weights. }

uses SysUtils, Math, Gaussian, CastleParameters;
var
  Radius, StdDev: Single;
  X, Y: Integer;
begin
  Parameters.CheckHighAtLeast(1);
  Parameters.CheckHighAtMost(2);
  Radius := StrToFloat(Parameters[1]);

  if Parameters.High = 2 then
    StdDev := StrToFloat(Parameters[2]) else
    StdDev := RadiusToStdDev(Radius);
  Writeln('Gaussian with standard deviation ', StdDev:0:4);
  Writeln;

  Writeln('Gaussian 1D');
  for X := 0 to Ceil(Radius) do
  begin
    Write(Format('%g', [Gaussian1D(X, StdDev)]));
    if X <> Ceil(Radius) then Write(', ');
  end;
  Writeln;
  Writeln;

  Writeln('Gaussian 2D');
  for Y := 0 to Ceil(Radius) do
  begin
    for X := 0 to Ceil(Radius) do
    begin
      Write(Format('%g', [Gaussian2D(X, Y, StdDev)]));
      if (X <> Ceil(Radius)) or (Y <> Ceil(Radius)) then Write(', ');
    end;
    Writeln;
  end;
end.
