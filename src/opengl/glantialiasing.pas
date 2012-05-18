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

{ Control OpenGL full-screen anti-aliasing (multi-sampling).
  This unit provides a global AntiAliasing variable, TAntiAliasing type
  and some routines to control anti-aliasing.

  It's not really necessary to use this unit to have anti-aliasing
  (multi-sampling). All you really need to do is to set
  TCastleWindowBase.MultiSampling or TCastleControlBase.MultiSampling
  to values like 2 or 4 to get anti-aliasing. Then it will be enabled by default
  (http://www.opengl.org/registry/specs/ARB/multisample.txt spec says
  that MULTISAMPLE_ARB state is by default TRUE) and our GLCurrentMultiSampling
  will show the number of samples > 1, and this is enough to see anti-aliasing.

  This unit gives you some small functionality:
  @unorderedList(
    @itemSpacing compact
    @item(TAntiAliasing type means you don't have to wonder what MultiSampling
      values are supported by common GPUs.)
    @item(AntiAliasing global variable is useful to store in a config file or such.)
    @item(We use NV_multisample_filter_hint to instruct GPU to possibly
      use better sampling.)
  )
}
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
