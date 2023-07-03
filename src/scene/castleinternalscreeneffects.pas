{
  Copyright 2010-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Internal screen effects stuff. }
unit CastleInternalScreenEffects;

interface

uses CastleGLShaders, CastleRenderOptions;

type
  { GLSL shader program specialized for rendering screen effects.
    See https://castle-engine.io/x3d_extensions_screen_effects.php
    about screen effects.

    Do not use the ancestor AttachVertexShader and AttachFragmentShader
    methods, instead set the @link(ScreenEffectShader).
    This way, the standard GLSL functionality of screen effects
    will be attached to the vertex and fragment shader code automatically.
    At link time, this looks at current OpenGL context multi-sampling capabilities,
    and the @link(NeedsDepth) value, to link the correct shader code. }
  TGLSLScreenEffect = class(TGLSLProgram)
  private
    FScreenEffectShader: string;
    FNeedsDepth: boolean;
  public
    constructor Create;

    property NeedsDepth: boolean read FNeedsDepth write FNeedsDepth default false;

    { In this class, UniformMissing is by default umIgnore, since it's
      normal that screen effect doesn't use some of it's uniform variables. }
    property UniformMissing default umIgnore;

    { Attach GLSL code for the screen effect (executed as part of fragment shader).
      See https://castle-engine.io/x3d_extensions_screen_effects.php . }
    property ScreenEffectShader: string read FScreenEffectShader write FScreenEffectShader;

    procedure Link; override;
  end;

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
    Shader.Link;
    { umIgnore is a good idea here, in case some uniform variable
      from ScreenEffectFragment code may be left unused. }
    Shader.UniformMissing := umIgnore;
  #)
*)
function ScreenEffectFragment(const Depth: boolean): string;

implementation

uses SysUtils,
  CastleGLUtils, CastleUtils, CastleLog;

{ routines ------------------------------------------------------------------- }

function ScreenEffectVertex: string;
begin
  Result := {$I screen_effect.vs.inc};
end;

function ScreenEffectFragment(const Depth: boolean): string;
begin
  Result := '';
  if Depth then
    Result := Result + ('#define DEPTH' +NL);
  if GLFeatures.FBOMultiSampling then
  begin
    if GLFeatures.CurrentMultiSampling > 1 then
      Result := Result +
        '#define MULTI_SAMPLING' +NL +
        '#define MULTI_SAMPLING_' + IntToStr(GLFeatures.CurrentMultiSampling) +NL;
    if not (GLFeatures.CurrentMultiSampling in [1, 2, 4, 8, 16]) then
      WritelnWarning('Screen Effects', Format('Our GLSL library for screen effects is not prepared for your number of samples (anti-aliasing): %d. ' + 'This may indicate that your GPU is very new or very weird. Please submit this as a bug (see https://castle-engine.io/forum.php for links to forum, bug tracker and more), citing this message. For now, screen effects will not work.',
        [GLFeatures.CurrentMultiSampling]));
  end;
  Result := Result + ({$I screen_effect_library.glsl.inc} + NL);
end;

{ TGLSLScreenEffect ---------------------------------------------------------- }

constructor TGLSLScreenEffect.Create;
begin
  inherited;
  UniformMissing := umIgnore;
  Name := 'TGLSLScreenEffect';
end;

procedure TGLSLScreenEffect.Link;
var
  VS, FS: String;
begin
  if FScreenEffectShader = '' then
    raise EGLSLError.Create('TGLSLScreenEffect shader not assigned by AttachScreenEffectShader method');
  VS := ScreenEffectVertex;
  FS := ScreenEffectFragment(NeedsDepth) + FScreenEffectShader;
  AttachVertexShader(VS);
  AttachFragmentShader(FS);
  inherited;
end;

end.
