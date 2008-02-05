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

{ Simple helper unit for setting up OpenGL full-screen anti-aliasing
  (that is, multisamling). }
unit GLAntiAliasing;

interface

const
  MaxAntiAliasing = 4;
  DefaultAntiAliasing = 0;

var
  { Anti-aliasing level.

    0 - none
    1 - 2 samples, dont_care
    2 - 2 samples, nicest (quincunx (5 taps) for NVidia)
    3 - 4 samples, dont_care
    4 and more - 4 samples, nicest (9 taps for NVidia)
  }
  AntiAliasing: Cardinal = DefaultAntiAliasing;

function AntiAliasingGlwMultiSampling: Cardinal;

procedure AntiAliasingGLInit;

procedure AntiAliasingEnable;
procedure AntiAliasingDisable;

implementation

uses KambiGLUtils, GL, GLExt;

function AntiAliasingGlwMultiSampling: Cardinal;
begin
  case AntiAliasing of
    0: Result := 1;
    1..2: Result := 2;
    else Result := 4;
  end;
end;

procedure AntiAliasingGLInit;
begin
  if ( (AntiAliasing = 2) or
       (AntiAliasing >= 4) ) and
     GL_NV_multisample_filter_hint then
    glHint(GL_MULTISAMPLE_FILTER_HINT_NV, GL_NICEST);
end;

procedure AntiAliasingEnable;
begin
  if (AntiAliasing > 0) and GL_ARB_multisample then
    glEnable(GL_MULTISAMPLE_ARB);
end;

procedure AntiAliasingDisable;
begin
  if (AntiAliasing > 0) and GL_ARB_multisample then
    glDisable(GL_MULTISAMPLE_ARB);
end;

end.
