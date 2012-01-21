{
  Copyright 2008-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Setup OpenGL full-screen anti-aliasing (multi-sampling). }
unit GLAntiAliasing;

interface

const
  MaxAntiAliasing = 4;
  DefaultAntiAliasing = 0;

type
  TAntiAliasing = Cardinal;
  TAntiAliasingRange = 0..MaxAntiAliasing;

var
  { Anti-aliasing level.

    @unorderedList(
      @item 0 - none
      @item 1 - 2 samples, dont_care
      @item 2 - 2 samples, nicest (quincunx (5 taps) for NVidia)
      @item 3 - 4 samples, dont_care
      @item 4 and more - 4 samples, nicest (9 taps for NVidia)
    )
  }
  AntiAliasing: TAntiAliasing = DefaultAntiAliasing;

function AntiAliasingGlwMultiSampling: Cardinal;

procedure AntiAliasingGLOpen;

procedure AntiAliasingEnable;
procedure AntiAliasingDisable;

function AntiAliasingToStr(Value: TAntiAliasing): string;

implementation

uses CastleGLUtils, GL, GLExt;

function AntiAliasingGlwMultiSampling: Cardinal;
begin
  case AntiAliasing of
    0: Result := 1;
    1..2: Result := 2;
    else Result := 4;
  end;
end;

procedure AntiAliasingGLOpen;
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

function AntiAliasingToStr(Value: TAntiAliasing): string;
begin
  case Value of
    0: Result := 'None';
    1: Result := '2 samples (faster)';
    2: Result := '2 samples (nicer)';
    3: Result := '4 samples (faster)';
    else Result := '4 samples (nicer)';
  end;
end;

end.
