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

type
  TAntiAliasing = (aaNone,
    aa2SamplesFaster, //< 2 samples, "don't care" hint.
    aa2SamplesNicer,  //< 2 samples, "nicest" hint (quincunx (5 taps) for NVidia).
    aa4SamplesFaster, //< 4 samples, "don't care" hint.
    aa4SamplesNicer   //< 4 samples, "nicest" hint (9 taps for NVidia).
  );

const
  DefaultAntiAliasing = aaNone;

  AntiAliasingNames: array [TAntiAliasing] of string =
  ( 'None',
    '2 samples (faster)',
    '2 samples (nicer)',
    '4 samples (faster)',
    '4 samples (nicer)'
  );

var
  AntiAliasing: TAntiAliasing = DefaultAntiAliasing;

function AntiAliasingGLMultiSampling: Cardinal;

procedure AntiAliasingGLOpen;

procedure AntiAliasingEnable;
procedure AntiAliasingDisable;

implementation

uses CastleGLUtils, GL, GLExt, CastleUtils;

function AntiAliasingGLMultiSampling: Cardinal;
begin
  { When extending possible results of this function, remember to also
    update ScreenEffectLibrary implementation to be able to handle them
    (and screen_effect_library.glsl to handle them in GLSL). }

  case AntiAliasing of
    aaNone: Result := 1;
    aa2SamplesFaster..aa2SamplesNicer: Result := 2;
    aa4SamplesFaster..aa4SamplesNicer: Result := 4;
    else raise EInternalError.Create('AntiAliasingGLMultiSampling:AntiAliasing?');
  end;
end;

procedure AntiAliasingGLOpen;
begin
  if ( (AntiAliasing = aa2SamplesNicer) or
       (AntiAliasing = aa4SamplesNicer) ) and
     GL_NV_multisample_filter_hint then
    glHint(GL_MULTISAMPLE_FILTER_HINT_NV, GL_NICEST);
end;

procedure AntiAliasingEnable;
begin
  if (AntiAliasing > aaNone) and GL_ARB_multisample then
    glEnable(GL_MULTISAMPLE_ARB);
end;

procedure AntiAliasingDisable;
begin
  if (AntiAliasing > aaNone) and GL_ARB_multisample then
    glDisable(GL_MULTISAMPLE_ARB);
end;

end.
