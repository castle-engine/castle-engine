{
  Copyright 2002-2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Texture environment detection.
  @exclude Internal unit for GLRenderer and GLRendererShader. }
unit CastleRendererTextureEnv;

interface

uses GL, GLExt;

type
  { Source of color for texture mixing.
    Used to represent MultiTexture.source X3D field.
    And used to represent the source of alpha value,
    for MultiTexture.mode = BLEND* case. }
  TColorSource = (
    { Material (OpenGL calls this "primary color"). }
    csMaterial,
    { Current texture unit color. }
    csCurrentTexture,
    { Constant color. (MultiTexture.color/alpha fields;
      for TInterpolateAlphaSource, actually this indicates only MultiTexture.alpha.) }
    csConstant,
    { Previous texture unit color, or material (in case this is the first texture unit). }
    csPreviousTexture);

  { OpenGL texture environment argument: none, GL_SOURCE0 or GL_SOURCE1. }
  TTextureEnvArgument = (taNone, ta0, ta1);

  TChannel = (cRGB, cAlpha);
  TCombinePerChannel = array [TChannel] of TGLint;
  TArgPerChannel = array [TChannel] of TTextureEnvArgument;
  TScalePerChannel = array [TChannel] of TGLfloat;
  TSourcePerChannel = array [TChannel] of TColorSource;

  { How to mix the current texture color into the fragment color. }
  TTextureEnv = object
  public
    { How to calculate given fragment channel using this texture unit.
      Return OpenGL values for GL_COMBINE_RGB_EXT, GL_COMBINE_ALPHA_EXT.
      For some details about particular values,
      see OpenGL EXT_texture_env_combine extension
      (http://www.opengl.org/registry/specs/EXT/texture_env_combine.txt). }
    Combine: TCombinePerChannel;

    { Where should we load the current texture unit
      (this is "Arg1" in X3D spec wording; Arg1 is always current texture,
      indicated by X3D spec wording "The source field determines
      the colour source for the second argument.") }
    CurrentTextureArgument: TArgPerChannel;

    { Where should we load the @link(Source) color. This is "Arg2" in X3D spec. }
    SourceArgument: TArgPerChannel;

    { Scaling of given channel value.
      Fixed-function pipeline OpenGL allows only 1.0, 2.0 and 4.0 scales,
      and only such values will be set by TTextureEnv.Init. }
    Scale: TScalePerChannel;

    Disabled: boolean;
    NeedsConstantColor: boolean;

    { If, and only if, one of SourceArgument is not taNone,
      then SourceArgument should be loaded with this color. }
    Source: TSourcePerChannel;

    { If, and only if, one of Combine is GL_INTERPOLATE_EXT,
      then this specifies what is the OpenGL GL_SOURCE2.
      It should be filled (both RGB and alpha) with alpha from this source. }
    InterpolateAlphaSource: TColorSource;

    { Calculate values based on MultiTexture.mode, source values.
      This does not setup any OpenGL state, it only calculates fields
      of this object. }
    constructor Init(const Mode, SourceStr: string);

    { Calculate values based on simple OpenGL mode value. }
    constructor Init(const Mode: TGLint);

    function Hash: LongWord;
  end;

implementation

uses SysUtils, CastleStringUtils, CastleWarnings;

{ Simple type constructors, for ease of coding.
  Versions with only 1 argument set both channel (rgb and alpha) to the same. }
function CombinePerChannel(const RGB, Alpha: TGLint): TCombinePerChannel;
begin
  Result[cRGB] := RGB;
  Result[cAlpha] := Alpha;
end;

function CombinePerChannel(const Value: TGLint): TCombinePerChannel;
begin
  Result := CombinePerChannel(Value, Value);
end;

function ArgPerChannel(const Value: TTextureEnvArgument): TArgPerChannel;
begin
  Result[cRGB] := Value;
  Result[cAlpha] := Value;
end;

type
  TStringPerChannel = array [TChannel] of string;

{ If S contains two separate modes (one for RGB, one for Alpha)
  returns @true and sets PerChannel to these separate strings.
  Strings returned in PerChannel will not contain the separator
  (slash, comma), and will not contain whitespace. }
function SplitStringPerChannel(const S: string;
  out PerChannel: TStringPerChannel): boolean;
var
  P: Integer;
begin
  P := CharsPos(['/', ','], S);
  Result := P > 0;
  if Result then
  begin
    PerChannel[cRGB] := Trim(Copy(S, 1, P - 1));
    PerChannel[cAlpha] := Trim(SEnding(S, P + 1));
  end;
end;

{ Calculate values knowing MultiTexture.mode value. }
procedure ModeFromString(const S: string;
  out Combine: TCombinePerChannel;
  out CurrentTextureArgument: TArgPerChannel;
  out SourceArgument: TArgPerChannel;
  out Scale: TScalePerChannel;
  out Disabled: boolean;
  out NeedsConstantColor: boolean;
  out InterpolateAlphaSource: TColorSource);

  { Interpret simple mode name (this is for sure only one mode,
    without any "/" and whitespaces). This handles only the simplest
    modes, that behave the same and are allowed separately for
    both RGB and alpha channel.

    LS passed here must already be lowercase.

    Scale passed here must be initially 1.0. }
  procedure SimpleModeFromString(
    const LS: string;
    out Combine: TGLint;
    out CurrentTextureArgument, SourceArgument: TTextureEnvArgument;
    var Scale: TGLfloat;
    const Channels: string);
  begin
    if LS = 'modulate' then
    begin
      Combine := GL_MODULATE;
      CurrentTextureArgument := ta0;
      SourceArgument := ta1;
    end else
    if LS = 'modulate2x' then
    begin
      Combine := GL_MODULATE;
      CurrentTextureArgument := ta0;
      SourceArgument := ta1;
      Scale := 2;
    end else
    if LS = 'modulate4x' then
    begin
      Combine := GL_MODULATE;
      CurrentTextureArgument := ta0;
      SourceArgument := ta1;
      Scale := 4;
    end else
    if (LS = 'replace') or (LS = 'selectarg1') then
    begin
      { SELECTARG1 is exactly the same as REPLACE.

        Note: don't get confused by X3D spec saying in table 18.3 that
        "REPLACE" takes the Arg2, that's an error, it takes
        from Arg1 to be consistent with other spec words.
        I wrote some remarks about this on
        http://castle-engine.sourceforge.net/x3d_implementation_status.php }

      Combine := GL_REPLACE;
      CurrentTextureArgument := ta0;
      SourceArgument := taNone;
    end else
    if LS = 'selectarg2' then
    begin
      Combine := GL_REPLACE;
      CurrentTextureArgument := taNone;
      SourceArgument := ta0;
    end else
    if LS = 'add' then
    begin
      Combine := GL_ADD;
      CurrentTextureArgument := ta0;
      SourceArgument := ta1;
    end else
    if LS = 'addsigned' then
    begin
      Combine := GL_ADD_SIGNED_EXT;
      CurrentTextureArgument := ta0;
      SourceArgument := ta1;
    end else
    if LS = 'addsigned2x' then
    begin
      Combine := GL_ADD_SIGNED_EXT;
      CurrentTextureArgument := ta0;
      SourceArgument := ta1;
      Scale := 2;
    end else
    if LS = 'subtract' then
    begin
      Combine := GL_SUBTRACT;
      CurrentTextureArgument := ta0;
      SourceArgument := ta1;
    end else
    begin
      Combine := GL_MODULATE;
      CurrentTextureArgument := ta0;
      SourceArgument := ta1;
      OnWarning(wtMajor, 'VRML/X3D', Format('Not supported multi-texturing mode "%s" for channels "%s"', [LS, Channels]));
    end;
  end;

  procedure RGBModeFromString(
    const LS: string;
    out Combine: TGLint;
    out CurrentTextureArgument, SourceArgument: TTextureEnvArgument;
    var Scale: TGLfloat);
  begin
    if LS = 'dotproduct3' then
    begin
      { We use DOT3_RGB_ARB here.
        This means it will fill only RGB values.

        This is our extension (X3D spec allows only DOTPRODUCT3
        for both channels, and to fill them both, this case is handled
        in BothModesFromString). }
      Combine := GL_DOT3_RGB_ARB;
      CurrentTextureArgument := ta0;
      SourceArgument := ta1;
    end else
      SimpleModeFromString(LS, Combine, CurrentTextureArgument, SourceArgument, Scale, 'RGB');
  end;

  procedure AlphaModeFromString(
    const LS: string;
    out Combine: TGLint;
    out CurrentTextureArgument, SourceArgument: TTextureEnvArgument;
    var Scale: TGLfloat);
  begin
    SimpleModeFromString(LS, Combine, CurrentTextureArgument, SourceArgument, Scale, 'Alpha');
  end;

  procedure BothModesFromString(
    const LS: string;
    out Combine: TCombinePerChannel;
    out CurrentTextureArgument, SourceArgument: TArgPerChannel;
    var Scale: TScalePerChannel);
  begin
    if LS = '' then
    begin
      { LS = '' means that mode list was too short.
        X3D spec says explicitly that default mode is "MODULATE"
        in this case. (Accidentaly, this also will accept
        explict "" string as "MODULATE" --- not a worry, we don't
        have to produce error messages for all possible invalid VRMLs...). }

      Combine := CombinePerChannel(GL_MODULATE);
      CurrentTextureArgument := ArgPerChannel(ta0);
      SourceArgument := ArgPerChannel(ta1);
    end else
    if LS = 'off' then
    begin
      Disabled := true;
    end else
    if LS = 'dotproduct3' then
    begin
      { We use DOT3_RGBA_ARB, not DOT3_RGB_ARB.
        See [http://www.opengl.org/registry/specs/ARB/texture_env_dot3.txt].

        This means that the dot (done on only RGB channels) will
        be replicated to all four channels (RGBA). This is exactly what
        the X3D specification requires, so we're happy.
        Yes, this means that COMBINE_ALPHA_ARB will be ignored. }

      Combine := CombinePerChannel(GL_DOT3_RGBA_ARB,
        GL_REPLACE { <- whatever, alpha combine will be ignored });
      CurrentTextureArgument := ArgPerChannel(ta0);
      SourceArgument := ArgPerChannel(ta1);
    end else
    if LS = 'blenddiffusealpha' then
    begin
      Combine := CombinePerChannel(GL_INTERPOLATE_EXT);
      CurrentTextureArgument := ArgPerChannel(ta0);
      SourceArgument := ArgPerChannel(ta1);
      InterpolateAlphaSource := csMaterial;
    end else
    if LS = 'blendtexturealpha' then
    begin
      Combine := CombinePerChannel(GL_INTERPOLATE_EXT);
      CurrentTextureArgument := ArgPerChannel(ta0);
      SourceArgument := ArgPerChannel(ta1);
      InterpolateAlphaSource := csCurrentTexture;
    end else
    if LS = 'blendfactoralpha' then
    begin
      Combine := CombinePerChannel(GL_INTERPOLATE_EXT);
      CurrentTextureArgument := ArgPerChannel(ta0);
      SourceArgument := ArgPerChannel(ta1);
      InterpolateAlphaSource := csConstant;
      NeedsConstantColor := true;
    end else
    if LS = 'blendcurrentalpha' then
    begin
      Combine := CombinePerChannel(GL_INTERPOLATE_EXT);
      CurrentTextureArgument := ArgPerChannel(ta0);
      SourceArgument := ArgPerChannel(ta1);
      InterpolateAlphaSource := csPreviousTexture;
    end else
    begin
      SimpleModeFromString(LS,
        Combine[cRGB], CurrentTextureArgument[cRGB], SourceArgument[cRGB], Scale[cRGB], 'both RGB and Alpha');
      Combine               [cAlpha] := Combine               [cRGB];
      CurrentTextureArgument[cAlpha] := CurrentTextureArgument[cRGB];
      SourceArgument        [cAlpha] := SourceArgument        [cRGB];
      Scale                 [cAlpha] := Scale                 [cRGB];
    end;
  end;

var
  LS: string;
  StringPerChannel: TStringPerChannel;
begin
  { initialize some out parameters to default values }
  Disabled := false;
  Scale[cRGB] := 1;
  Scale[cAlpha] := 1;
  NeedsConstantColor := false;
  InterpolateAlphaSource := csMaterial; { unused, will define to be safe }

  LS := LowerCase(S);
  if SplitStringPerChannel(LS, StringPerChannel) then
  begin
    RGBModeFromString  (StringPerChannel[cRGB  ], Combine[cRGB  ], CurrentTextureArgument[cRGB  ], SourceArgument[cRGB  ], Scale[cRGB  ]);
    AlphaModeFromString(StringPerChannel[cAlpha], Combine[cAlpha], CurrentTextureArgument[cAlpha], SourceArgument[cAlpha], Scale[cAlpha]);
  end else
    BothModesFromString(LS, Combine, CurrentTextureArgument, SourceArgument, Scale);
end;

{ Calculate values knowing MultiTexture.source value. }
procedure SourceFromString(const S: string; out Source: TSourcePerChannel;
  var NeedsConstantColor: boolean);

  procedure SimpleSourceFromString(const LS: string;
    out Source: TColorSource;
    var NeedsConstantColor: boolean);
  begin
    if LS = '' then
      Source := csPreviousTexture else
    if (LS = 'diffuse') or (LS = 'specular') then
      Source := csMaterial else
    if LS = 'factor' then
    begin
      NeedsConstantColor := true;
      Source := csConstant;
    end else
    begin
      Source := csPreviousTexture;
      OnWarning(wtMajor, 'VRML/X3D', Format('Not supported multi-texturing source "%s"', [LS]))
    end;
  end;

var
  LS: string;
  SourcePerChannel: TStringPerChannel;
begin
  LS := LowerCase(S);
  if SplitStringPerChannel(LS, SourcePerChannel) then
  begin
    SimpleSourceFromString(SourcePerChannel[cRGB  ], Source[cRGB  ], NeedsConstantColor);
    SimpleSourceFromString(SourcePerChannel[cAlpha], Source[cAlpha], NeedsConstantColor);
  end else
  begin
    SimpleSourceFromString(LS, Source[cRGB], NeedsConstantColor);
    Source[cAlpha] := Source[cRGB];
  end;
end;

constructor TTextureEnv.Init(const Mode, SourceStr: string);
begin
  ModeFromString(Mode, Combine, CurrentTextureArgument, SourceArgument, Scale, Disabled,
    NeedsConstantColor, InterpolateAlphaSource);
  if (SourceArgument[cRGB] <> taNone) or
     (SourceArgument[cAlpha] <> taNone) then
    SourceFromString(SourceStr, Source, NeedsConstantColor);
end;

constructor TTextureEnv.Init(const Mode: TGLint);
begin
  Combine := CombinePerChannel(Mode);

  { constant default values for other fields }
  CurrentTextureArgument := ArgPerChannel(ta0);
  SourceArgument := ArgPerChannel(ta1);
  Scale[cRGB  ] := 1.0;
  Scale[cAlpha] := 1.0;
  Disabled := false;
  NeedsConstantColor := false;
  InterpolateAlphaSource := csMaterial;
  Source[cRGB  ] := csMaterial;
  Source[cAlpha] := csMaterial;
end;

function TTextureEnv.Hash: LongWord;
{$include norqcheckbegin.inc}
begin
  Result :=
    1693 * (1 + Ord(Combine[cRGB]                 )) +
    1697 * (1 + Ord(Combine[cAlpha]               )) +
    1699 * (1 + Ord(CurrentTextureArgument[cRGB]  )) +
    1709 * (1 + Ord(CurrentTextureArgument[cAlpha])) +
    1721 * (1 + Ord(SourceArgument[cRGB]          )) +
    1723 * (1 + Ord(SourceArgument[cAlpha]        )) +
    1733 * (1 + Round(Scale[cRGB] * 100           )) +
    1741 * (1 + Round(Scale[cAlpha] * 100         )) +
    1747 * (1 + Ord(Disabled                      )) +
    1753 * (1 + Ord(NeedsConstantColor            )) +
    1759 * (1 + Ord(Source[cRGB]                  )) +
    1777 * (1 + Ord(Source[cAlpha]                )) +
    1783 * (1 + Ord(InterpolateAlphaSource        ));
end;
{$include norqcheckend.inc}

end.
