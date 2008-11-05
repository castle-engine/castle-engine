{
  Copyright 2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ Spherical harmonic basis functions. }
unit SphericalHarmonics;

interface

uses VectorMath, KambiUtils, Math;

const
  { How many basis can SHBasis calculate. LM for SHBasis must be within
    0 .. SHBasesCount - 1. }
  SHBasesCount = 25;

{ Calculate spherical harmonic basis function for given arguments.

  LM indicates (L, M) that specify which SH basis to use.
  LM determines a pair (L, M) in the natural order: for each L,
  take for each M from -L to L.
  That is, LM = (0, 1, 2, 3, ...) indicate
@precalculated(
    (L, M) =
  ( (0, 0),
    (1, -1), (1, 0), (1, 1),
    (2, -2), (2, -1), (2, 0), (2, 1), (2, 2),
    ... )
) ) }
function SHBasis(const LM: Cardinal; const PhiTheta: TVector2Single): Float;

procedure LMDecode(const LM: Cardinal; out L: Cardinal; out M: Integer);

implementation

{ Taken from http://www.sjbrown.co.uk/2004/10/16/spherical-harmonic-basis/ }

function SHBasis(const LM: Cardinal; const PhiTheta: TVector2Single): Float;
var
  SinPhi, CosPhi, SinTheta, CosTheta: Float;

  function Sin2Theta: Float;
  begin
    Result := Sqr(SinTheta);
  end;

  function Sin3Theta: Float;
  begin
    Result := Sqr(SinTheta)*SinTheta;
  end;

  function Sin4Theta: Float;
  begin
    Result := Sqr(Sqr(SinTheta));
  end;

  function Cos2Theta: Float;
  begin
    Result := Sqr(CosTheta);
  end;

  function Cos3Theta: Float;
  begin
    Result := Sqr(CosTheta)*CosTheta;
  end;

  function Cos4Theta: Float;
  begin
    Result := Sqr(Sqr(CosTheta));
  end;

  function Sin2Phi: Float;
  begin
    Result := Sqr(SinPhi);
  end;

  function Sin3Phi: Float;
  begin
    Result := Sqr(SinPhi)*SinPhi;
  end;

  function Sin4Phi: Float;
  begin
    Result := Sqr(Sqr(SinPhi));
  end;

  function Cos2Phi: Float;
  begin
    Result := Sqr(CosPhi);
  end;

  function Cos3Phi: Float;
  begin
    Result := Sqr(CosPhi)*CosPhi;
  end;

  function Cos4Phi: Float;
  begin
    Result := Sqr(Sqr(CosPhi));
  end;

begin
  SinCos(PhiTheta[0], SinPhi, CosPhi);
  SinCos(PhiTheta[1], SinTheta, CosTheta);

  { Fear not, this case should be converted to lookup table by FPC. }

  case LM of
    0: Result := 1 / (2 * Sqrt(Pi));

    1: Result := Sqrt(3) / (2 * Sqrt(Pi)) * CosPhi * SinTheta;
    2: Result := Sqrt(3) / (2 * Sqrt(Pi)) * CosTheta;
    3: Result := Sqrt(3) / (2 * Sqrt(Pi)) * SinPhi * SinTheta;

    4: Result := Sqrt(15) / (2 * Sqrt(Pi)) * CosPhi * SinPhi * Sin2Theta;
    5: Result := Sqrt(15) / (2 * Sqrt(Pi)) * SinPhi * CosTheta * SinTheta;
    6: Result := Sqrt(15) / (4 * Sqrt(Pi)) * (3 * Cos2Theta - 1);
    7: Result := Sqrt(15) / (2 * Sqrt(Pi)) * CosPhi * CosTheta * SinTheta;
    8: Result := Sqrt(15) / (4 * Sqrt(Pi)) * (Cos2Phi - Sin2Phi) * Sin2Theta;

    9 : Result := Sqrt(35)  / (4 * Sqrt(2*Pi) ) * (3 * Cos2Phi - Sin2Phi) * SinPhi * Sin3Theta;
    10: Result := Sqrt(105) / (2 * Sqrt(Pi  ) ) * CosPhi * SinPhi * CosTheta * Sin2Theta;
    11: Result := Sqrt(21)  / (4 * Sqrt(2*Pi) ) * (5 * Cos2Theta - 1) * SinPhi * SinTheta;
    12: Result := Sqrt(7)   / (4 * Sqrt(Pi  ) ) * (5 * Cos3Theta - 3 * CosTheta);
    13: Result := Sqrt(21)  / (4 * Sqrt(2*Pi) ) * (5 * Cos2Theta - 1) * CosPhi * SinTheta;
    14: Result := Sqrt(105) / (4 * Sqrt(Pi  ) ) * (Cos2Phi -     Sin2Phi) * CosTheta * Sin2Theta;
    15: Result := Sqrt(35)  / (4 * Sqrt(2*Pi) ) * (Cos2Phi - 3 * Sin2Phi) * CosPhi * Sin3Theta;

    16: Result := 3 * Sqrt(35) / (4  * Sqrt(Pi  ) ) * (    Cos2Phi - Sin2Phi) * CosPhi * SinPhi * Sin4Theta;
    17: Result := 3 * Sqrt(35) / (4  * Sqrt(2*Pi) ) * (3 * Cos2Phi - Sin2Phi) * SinPhi * CosTheta * Sin3Theta;
    18: Result := 3 * Sqrt(5)  / (4  * Sqrt(Pi  ) ) * (7 * Cos2Theta - 1) * CosPhi * SinPhi * Sin2Theta;
    19: Result := 3 * Sqrt(5)  / (4  * Sqrt(2*Pi) ) * (7 * Cos2Theta - 3) * SinPhi * CosTheta * SinTheta;
    20: Result := 3            / (16 * Sqrt(Pi  ) ) * (3 - 30 * Cos2Theta + 35 * Cos4Theta);
    21: Result := 3 * Sqrt(5)  / (4  * Sqrt(2*Pi) ) * (7 * Cos2Theta - 3) * CosPhi * CosTheta * SinTheta;
    22: Result := 3 * Sqrt(5)  / (8  * Sqrt(Pi  ) ) * (7 * Cos2Theta - 1) * (Cos2Phi - Sin2Phi) * Sin2Theta;
    23: Result := 3 * Sqrt(35) / (4  * Sqrt(2*Pi) ) * (Cos2Phi - 3 * Sin2Phi) * CosPhi * CosTheta * Sin3Theta;
    24: Result := 3 * Sqrt(35) / (16 * Sqrt(Pi  ) ) * (Sin4Phi - 6 * Cos2Phi * Sin2Phi + Cos4Phi) * Sin4Theta;

    else raise EInternalError.Create('SHBasis LM out');
  end
end;

procedure LMDecode(const LM: Cardinal; out L: Cardinal; out M: Integer);
var
  ReachedLM: Integer;
begin
  L := 0;
  ReachedLM := -1;
  repeat
    if ReachedLM + Integer(2*L+1) >= Integer(LM) then
    begin
      M := LM - ReachedLM - L - 1;
      Break;
    end;
    ReachedLM += Integer(2*L+1);
    Inc(L);
  until false;
end;

end.
