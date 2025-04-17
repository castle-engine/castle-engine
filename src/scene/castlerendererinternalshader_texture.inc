{%MainUnit castlerendererinternalshader.pas}
{
  Copyright 2010-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Generating shader code for texture and texture coordinates. }

{$ifdef read_interface}

//type
  { Setup the necessary shader things to pass texture coordinates. }
  TTextureCoordinateShader = class
  private
    TextureUnit: Cardinal;
    HasMatrixTransform: boolean;

    { Name of texture coordinate varying vec4 vector. }
    class function CoordName(const TexUnit: Cardinal): String;
    { Name of texture matrix mat4 uniform. }
    class function MatrixName(const TexUnit: Cardinal): String;
  public
    { Update Hash for this texture shader. }
    procedure Prepare(var Hash: TShaderCodeHash); virtual;
    procedure Enable(const MainTextureMapping: Integer; const MultiTextureColor: TCastleColor;
      var TextureApply, TextureColorDeclare,
        TextureCoordInitialize, TextureCoordMatrix,
        TextureAttributeDeclare, TextureVaryingDeclareVertex, TextureVaryingDeclareFragment, TextureUniformsDeclare,
        GeometryVertexDeclare, GeometryVertexSet, GeometryVertexZero, GeometryVertexAdd: String); virtual;
  end;

  { Setup the necessary shader things to query a texture using texture coordinates. }
  TTextureShader = class(TTextureCoordinateShader)
  private
    TextureType: TTextureType;
    Node: TAbstractSingleTextureNode;
    Env: TTextureEnv;
    Shader: TShader;

    { Uniform to set for this texture. May be empty. }
    UniformName: String;
    UniformValue: Integer;

    class var TextureEnvWarningDone: Boolean;

    { Mix texture colors into fragment color, based on TTextureEnv specification. }
    class function TextureEnvMix(const AEnv: TTextureEnv; const MultiTextureColor: TCastleColor;
      const FragmentColor, CurrentTexture: String;
      const ATextureUnit: Cardinal): String;
  public
    { Update Hash for this texture shader. }
    procedure Prepare(var Hash: TShaderCodeHash); override;
    procedure Enable(const MainTextureMapping: Integer; const MultiTextureColor: TCastleColor;
      var TextureApply, TextureColorDeclare,
        TextureCoordInitialize, TextureCoordMatrix,
        TextureAttributeDeclare, TextureVaryingDeclareVertex, TextureVaryingDeclareFragment, TextureUniformsDeclare,
        GeometryVertexDeclare, GeometryVertexSet, GeometryVertexZero, GeometryVertexAdd: String); override;
  end;

  TTextureCoordinateShaderList = {$ifdef FPC}specialize{$endif} TObjectList<TTextureCoordinateShader>;

{$endif read_interface}

{$ifdef read_implementation}

{ TTextureCoordinateShader --------------------------------------------------- }

class function TTextureCoordinateShader.CoordName(const TexUnit: Cardinal): String;
begin
  Result := Format('castle_TexCoord%d', [TexUnit]);
end;

class function TTextureCoordinateShader.MatrixName(const TexUnit: Cardinal): String;
begin
  Result := Format('castle_TextureMatrix%d', [TexUnit]);
end;

procedure TTextureCoordinateShader.Prepare(var Hash: TShaderCodeHash);
var
  IntHash: UInt32;
begin
{$include norqcheckbegin.inc}
  IntHash :=
    1 +
    971 * Ord(HasMatrixTransform);
  Hash.AddInteger(977 * (TextureUnit + 1) * IntHash);
{$include norqcheckend.inc}
end;

procedure TTextureCoordinateShader.Enable(const MainTextureMapping: Integer; const MultiTextureColor: TCastleColor;
  var TextureApply, TextureColorDeclare,
    TextureCoordInitialize, TextureCoordMatrix,
    TextureAttributeDeclare, TextureVaryingDeclareVertex, TextureVaryingDeclareFragment, TextureUniformsDeclare,
    GeometryVertexDeclare, GeometryVertexSet, GeometryVertexZero, GeometryVertexAdd: String);
var
  TexCoordName, TexMatrixName: String;
begin
  TexCoordName := CoordName(TextureUnit);
  TexMatrixName := MatrixName(TextureUnit);

  TextureCoordInitialize := TextureCoordInitialize +
    Format('%s = castle_MultiTexCoord%d;' + NL,
    [TexCoordName, TextureUnit]);
  TextureAttributeDeclare := TextureAttributeDeclare +
    Format('attribute vec4 castle_MultiTexCoord%d;' + NL, [TextureUnit]);
  TextureVaryingDeclareVertex := TextureVaryingDeclareVertex + Format(
    'varying vec4 %s;' + NL, [TexCoordName]);
  TextureVaryingDeclareFragment := TextureVaryingDeclareFragment + Format(
    '#ifdef HAS_GEOMETRY_SHADER' + NL +
    '  #define %s %0:s_geoshader' + NL +
    '#endif' + NL +
    'varying vec4 %0:s;' + NL, [TexCoordName]);

  if HasMatrixTransform then
    TextureCoordMatrix := TextureCoordMatrix + Format('%s = %s * %0:s;' + NL,
      [TexCoordName, TexMatrixName]);

  GeometryVertexDeclare := GeometryVertexDeclare + Format(
    'in vec4 %s[CASTLE_GEOMETRY_INPUT_SIZE];' + NL +
    'out vec4 %0:s_geoshader;', [TexCoordName]);
  GeometryVertexSet  := GeometryVertexSet +
    Format('%s_geoshader  = %0:s[index];' + NL, [TexCoordName]);
  GeometryVertexZero := GeometryVertexZero + Format('%s_geoshader  = vec4(0.0);'
    + NL, [TexCoordName]);
  { NVidia will warn here "... might be used before being initialized".
    Which is of course true --- but we depend that author will always call
    geometryVertexZero() before geometryVertexAdd(). }
  GeometryVertexAdd  := GeometryVertexAdd +
    Format('%s_geoshader += %0:s[index] * scale;' + NL, [TexCoordName]);
end;

{ TTextureShader ------------------------------------------------------------- }

procedure TTextureShader.Prepare(var Hash: TShaderCodeHash);
var
  IntHash: UInt32;
begin
  inherited;

{$include norqcheckbegin.inc}
  IntHash :=
    1 +
    181 * Ord(TextureType) +
    Env.Hash;
  Hash.AddInteger(179 * (TextureUnit + 1) * IntHash);
  { Don't directly add Node to the Hash, it would prevent a lot of sharing.
    Node is only used to get effects. }
  Hash.AddEffects(Node.FdEffects);
{$include norqcheckend.inc}
end;

class function TTextureShader.TextureEnvMix(const AEnv: TTextureEnv; const MultiTextureColor: TCastleColor;
  const FragmentColor, CurrentTexture: String;
  const ATextureUnit: Cardinal): String;

  procedure Warn(const S: String; const Args: array of const);
  begin
    if not TextureEnvWarningDone then
    begin
      TextureEnvWarningDone := true;
      WritelnWarning('MultiTexture mixing', Format(S, Args));
    end;
  end;

  { GLSL code to express MultiTextureColor value. }
  function MultiTextureColorStr: String;
  begin
    Result := FormatDot('vec4(%f, %f, %f, %f)', [
      MultiTextureColor.X,
      MultiTextureColor.Y,
      MultiTextureColor.Z,
      MultiTextureColor.W
    ]);
  end;

type
  TChannels = (chAll, chRgb, chAlpha);

  { GLSL code to update FragmentColor based on given Combine operation. }
  function CombineCode(const Combine: TCombine;
    const Source: TColorSource;
    const SourceArgument: TTextureEnvArgument;
    const Channels: TChannels): String;

    { GLSL code to express given TColorSource value. }
    function SourceStr(const Source: TColorSource): String;
    begin
      case Source of
        csCurrentTexture : Result := CurrentTexture;
        csPreviousTexture: Result := FragmentColor;
        csMaterial       : Result := 'vec4(0.8, 0.8, 0.8, 1.0)'; // just Material.diffuseColor default
        csConstant       : Result := MultiTextureColorStr;
        {$ifndef COMPILER_CASE_ANALYSIS}
        else raise EInternalError.Create('SourceStr:TColorSource?');
        {$endif}
      end;
    end;

  var
    FragmentColorCh, CurrentTextureCh, Arg2Ch, ChannelsStr, ChannelsFromScalar: String;
    Arg2: String;
  begin
    { calculate Arg2, GLSL code that says "what is coming from MultiTexture.source" }
    Arg2 := SourceStr(Source);

    { calculate ChannelsStr, ChannelsFromScalar }
    case Channels of
      chAll  :
        begin
          ChannelsStr := ''; // set all RGBA of FragmentColor
          ChannelsFromScalar := 'vec4';
        end;
      chRgb  :
        begin
          ChannelsStr := '.rgb'; // set only RGB of FragmentColor
          ChannelsFromScalar := 'vec3';
        end;
      chAlpha:
        begin
          ChannelsStr := '.a'; // set only alpha of FragmentColor
          ChannelsFromScalar := '';
        end;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('Channels?');
      {$endif}
    end;

    FragmentColorCh := FragmentColor + ChannelsStr;
    CurrentTextureCh := CurrentTexture + ChannelsStr;
    Arg2Ch := Arg2 + ChannelsStr;

    { By default, set up simplest modulate operation.
      The "case" below may override this to something better. }
    Result := FragmentColorCh + ' *= ' + CurrentTextureCh + ';';

    case Combine of
      coModulate:
        if Arg2 = FragmentColor then
          // optimize GLSL code
          Result := FragmentColorCh + ' *= ' + CurrentTextureCh +  ';'
        else
          Result := FragmentColorCh + ' = ' + CurrentTextureCh + ' * ' + Arg2Ch + ';';
      coReplace:
        begin
          if SourceArgument = ta0 then
            { mode is SELECTARG2 }
            Result := FragmentColorCh + ' = ' + Arg2Ch + ';'
          else
            { assume CurrentTextureArgument = ta0, mode = REPLACE or SELECTARG1 }
            Result := FragmentColorCh + ' = ' + CurrentTextureCh + ';';
        end;
      coAddSigned:
        Result := FragmentColorCh + ' = ' + CurrentTextureCh + ' + ' + Arg2Ch +
          ' - ' + ChannelsFromScalar + '(0.5);';
      coAdd:
        if Arg2 = FragmentColor then
          // optimize GLSL code
          Result := FragmentColorCh + ' += ' + CurrentTextureCh + ';'
        else
          Result := FragmentColorCh + ' = ' + CurrentTextureCh + ' + ' + Arg2Ch + ';';
      coSubtract:
        Result := FragmentColorCh + ' = ' + CurrentTextureCh + ' - ' + Arg2Ch + ';';
      coBlend:
        Result := FragmentColorCh + ' = mix(' + FragmentColorCh + ', ' +
          CurrentTextureCh + ', ' + SourceStr(AEnv.BlendAlphaSource) + '.a);';
      coDot3Rgb,
      coDot3Rgba:
        Result := FragmentColorCh + ' = ' + ChannelsFromScalar + '(dot(' +
          '2 * (' + CurrentTexture + '.rgb - vec3(0.5)), ' +
          '2 * (' + Arg2 + '.rgb - vec3(0.5))));';
      else
        Warn('Not supported in GLSL pipeline: combine value %d', [Ord(Combine)]);
    end;
  end;

begin
  if AEnv.Disabled then Exit('');

  { Note that we ignore AEnv.CurrentTextureArgument.
    Which is OK -- it is always ta0 in practice, except for X3D "SELECTARG2" mode
    (which for us is a special case of coReplace, detected looking at AEnv.SourceArgument).

    We know that AEnv.SourceArgument is always ta1, except
    - it is taNone in case of X3D "REPLACE" mode (eqivalent "SELECTARG1")
    - it is ta0    in case of X3D "SELECTARG2" mode

    So, while we don't handle all *theoretically possible* cases of
    AEnv.CurrentTextureArgument and AEnv.SourceArgument, we don't need to.
    We detect and handle SELECTARG2 specially, in other cases
    we assume
    - AEnv.CurrentTextureArgument = ta0,
    - AEnv.SourceArgument = ta1,
    and this is enough to cover all *actually possible* (set by CastleRendererInternalTextureEnv)
    cases.
  }

  if (AEnv.Combine       [cRGB] = AEnv.Combine       [cAlpha]) and
     (AEnv.Source        [cRGB] = AEnv.Source        [cAlpha]) and
     (AEnv.SourceArgument[cRGB] = AEnv.SourceArgument[cAlpha]) then
  begin
    { Do the operation on whole RGBA. }
    Result := CombineCode(AEnv.Combine[cRGB], AEnv.Source[cRGB], AEnv.SourceArgument[cRGB], chAll) + NL;
  end else
  begin
    Result :=
      CombineCode(AEnv.Combine[cRGB]  , AEnv.Source[cRGB  ], AEnv.SourceArgument[cRGB]  , chRgb) + NL +
      CombineCode(AEnv.Combine[cAlpha], AEnv.Source[cAlpha], AEnv.SourceArgument[cAlpha], chAlpha) + NL;
  end;

  if AEnv.Scale[cRGB] = AEnv.Scale[cAlpha] then
  begin
    { Do the scaling on whole RGBA. }
    if AEnv.Scale[cRGB] <> 1 then
      Result := Result + FragmentColor + ' *= ' + FloatToStrDot(AEnv.Scale[cRGB]) + ';' + NL;
  end else
  begin
    if AEnv.Scale[cRGB] <> 1 then
      Result := Result + FragmentColor + '.rgb *= ' + FloatToStrDot(AEnv.Scale[cRGB]) + ';' + NL;
    if AEnv.Scale[cAlpha] <> 1 then
      Result := Result + FragmentColor + '.a *= ' + FloatToStrDot(AEnv.Scale[cAlpha]) + ';' + NL;
  end;

  case AEnv.TextureFunction of
    tfComplement    : Result := Result + FragmentColor + '.rgb = vec3(1.0) - ' + FragmentColor + '.rgb;' + NL;
    tfAlphaReplicate: Result := Result + FragmentColor + '.rgb = vec3(' + FragmentColor + '.a);' + NL;
    else ;
  end;
end;

procedure TTextureShader.Enable(const MainTextureMapping: Integer; const MultiTextureColor: TCastleColor;
  var TextureApply, TextureColorDeclare,
    TextureCoordInitialize, TextureCoordMatrix,
    TextureAttributeDeclare, TextureVaryingDeclareVertex, TextureVaryingDeclareFragment, TextureUniformsDeclare,
    GeometryVertexDeclare, GeometryVertexSet, GeometryVertexZero, GeometryVertexAdd: String);
const
  SamplerFromTextureType: array [TTextureType] of string =
  ('sampler2D', 'samplerCube', 'sampler3D', '');
var
  TextureSampleCall, TexCoordName: String;
  Code: TShaderSource;
  SamplerType: String;
begin
  inherited;

  if TextureType <> ttShader then
  begin
    UniformName := Format('castle_texture_%d', [TextureUnit]);
    UniformValue := TextureUnit;
  end else
    UniformName := '';

  if MainTextureMapping <> -1 then
    TexCoordName := CoordName(MainTextureMapping)
  else
    TexCoordName := CoordName(TextureUnit);

  SamplerType := SamplerFromTextureType[TextureType];

  if TextureColorDeclare = '' then
    TextureColorDeclare := 'vec4 texture_color;' + NL;
  case TextureType of
    tt2D:
      { texture2DProj reasoning:
        Most of the time, 'texture2D(%s, %s.st)' would be enough.
        But we may get 4D tex coords (that is, with last component <> 1)
        - through TextureCoordinate4D
        - through projected texture mapping, when using perspective light
          (spot light) or perspective viewpoint.

        TextureUnit = 0 check reasoning:
        Even when HAS_TEXTURE_COORD_SHIFT is defined (PLUG_texture_coord_shift
        was used), use it only for 0th texture unit. Parallax bump mapping
        calculates the shift, assuming that transformations to tangent space
        follow 0th texture coordinates. Also, for parallax bump mapping,
        we have to assume the 0th texture has simple 2D coords (not 4D). }
      if TextureUnit = 0 then
        TextureSampleCall := NL+
          '#ifdef HAS_TEXTURE_COORD_SHIFT' +NL+
          '  texture2D(%0:s, texture_coord_shifted(%1:s.st))' +NL+
          '#else' +NL+
          '  texture2DProj(%0:s, %1:s)' +NL+
          '#endif' + NL else
        TextureSampleCall := 'texture2DProj(%0:s, %1:s)';
    ttCubeMap : TextureSampleCall := 'textureCube(%s, %s.xyz)';
    { For 3D textures, remember we may get 4D tex coords
      through TextureCoordinate4D, so we have to use texture3DProj }
    tt3D      : TextureSampleCall := 'texture3DProj(%s, %s)';
    ttShader  : TextureSampleCall := 'vec4(1.0, 0.0, 1.0, 1.0)';
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TShader.EnableTexture:TextureType?');
    {$endif}
  end;

  Code := TShaderSource.Create;
  try
    if TextureType <> ttShader then
    begin
      { Optimization to not call castle_texture_color_to_linear when not needed.
        Although it should do nothing when ColorSpaceLinear=false,
        but to make sure it takes zero time we just not call it at all. }
      if Shader.ColorSpaceLinear then
        TextureSampleCall := 'castle_texture_color_to_linear(' + TextureSampleCall + ')';
      Code[stFragment].Add(Format(
        'texture_color = ' + TextureSampleCall + ';' +NL+
        '/* PLUG: texture_color (texture_color, %0:s, %1:s) */' +NL,
        [UniformName, TexCoordName]))
    end else
      Code[stFragment].Add(Format(
        'texture_color = ' + TextureSampleCall + ';' +NL+
        '/* PLUG: texture_color (texture_color, %0:s) */' +NL,
        [TexCoordName]));

    Shader.EnableEffects(Node.FdEffects, Code, true);

    { Add generated Code to Shader.Source. Code[stFragment][0] for texture
      is a little special, we add it to TextureApply that
      will be directly placed within the source. }
    TextureApply := TextureApply + Code[stFragment][0];
    Shader.Source.Append(Code, stFragment);
  finally FreeAndNil(Code) end;

  TextureApply := TextureApply + TextureEnvMix(Env, MultiTextureColor,
    'fragment_color', 'texture_color', TextureUnit) + NL;

  if TextureType <> ttShader then
    TextureUniformsDeclare := TextureUniformsDeclare + Format('uniform %s %s;' + NL,
      [SamplerType, UniformName]);
end;

{$endif read_implementation}
