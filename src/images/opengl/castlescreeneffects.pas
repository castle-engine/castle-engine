{
  Copyright 2010-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Utilities for creating custom screen effects. }
unit CastleScreenEffects;

interface

{ Standard GLSL vertex shader for screen effect. }
function ScreenEffectVertex: string;

(*Library of GLSL fragment shader functions useful for screen effects.
  This looks at current OpenGL context multi-sampling capabilities
  to return the correct shader code.

  Note that to work with OpenGLES, we have to glue all fragment shaders,
  and ScreenEffectFragment must be before the actual shader code.
  The string returned by this function is guaranteed to end with a newline,
  to make it easy.

  So you usually want to create screen effect shaders like this:

@longCode(#
  Shader := TGLSLProgram.Create;
  Shader.AttachVertexShader(ScreenEffectVertex);
  Shader.AttachFragmentShader(
    ScreenEffectFragment(false { or true if you use depth }) +
    '... my custom screen effect GLSL code ...');
  Shader.Link(true);
  { uaIgnore is a good idea here, in case some uniform variable
    from ScreenEffectFragment code may be left unused. }
  Shader.UniformNotFoundAction := uaIgnore;
#)

*)
function ScreenEffectFragment(const Depth: boolean): string;

implementation

uses SysUtils, CastleUtils, CastleGLUtils, CastleWarnings;

function ScreenEffectVertex: string;
begin
  Result := {$I screen_effect.vs.inc};
end;

function ScreenEffectFragment(const Depth: boolean): string;
begin
  Result := '';
  if Depth then
    Result += '#define DEPTH' +NL;
  if GLFeatures.FBOMultiSampling then
  begin
    if GLFeatures.CurrentMultiSampling > 1 then
      Result +=
        '#define MULTI_SAMPLING' +NL +
        '#define MULTI_SAMPLING_' + IntToStr(GLFeatures.CurrentMultiSampling) +NL;
    if not (GLFeatures.CurrentMultiSampling in [1, 2, 4, 8, 16]) then
      OnWarning(wtMajor, 'Screen Effects', Format('Our GLSL library for screen effects is not prepared for your number of samples (anti-aliasing): %d. This may indicate that your GPU is very new or very weird. Please submit this as a bug (see http://castle-engine.sourceforge.net/forum.php for links to forum, bug tracker and more), citing this message. For now, screen effects will not work.',
        [GLFeatures.CurrentMultiSampling]));
  end;
  Result += {$I screen_effect_library.glsl.inc} + NL;
end;

end.
