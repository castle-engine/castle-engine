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

{ Generating shader code for lights. }

{$ifdef read_interface}

//type
  TLightingModel = (lmPhong, lmPhysical, lmUnlit);

  TLightUniforms = class
    Number: Cardinal;
    ShaderProgram: TGLSLProgram;

    { Current values provided to OpenGL(ES) for these uniforms.

      Note that they are initially filled with zero.

      This is correct, as OpenGL and OpenGLES docs say:
      """
      All active uniform variables defined in a program object
      are initialized to 0 when the program object is linked successfully.
      """

      https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glUniform.xhtml
      https://www.khronos.org/registry/OpenGL-Refpages/es3.0/html/glUniform.xhtml
    }
    Position: TVector3;
    Radius: Single;
    SpotCosCutoff: Single;
    SpotBeamWidth: Single;
    SpotDirection: TVector3;
    SpotExponent: Single;
    SpotCutoff: Single;
    Attenuation: TVector3;
    AmbientColor, Color: TVector3;

    procedure SetUniform(const NamePattern: String;
      var CurrentValue: Single; const NewValue: Single); overload;
    procedure SetUniform(const NamePattern: String;
      var CurrentValue: TVector3; const NewValue: TVector3); overload;
    procedure SetUniform(const NamePattern: String;
      var CurrentValue: TVector4; const NewValue: TVector4); overload;
  end;

  TLightUniformsList = {$ifdef FPC}specialize{$endif} TObjectList<TLightUniforms>;

  { Manage GLSL defines for TLightShader. }
  TLightDefine = (
    ldTypePositional,
    ldTypeSpot,
    ldTypeEnvironment,
    ldTypePunctual,
    ldHasAttenuation,
    ldHasRadius,
    ldHasAmbient,
    ldHasSpecular,
    ldHasBeamWidth,
    ldHasSpotExponent);

  { Generating shader code to handle a single light. }
  TLightShader = class
  private
    Number: Cardinal;
    Node: TAbstractLightNode;
    Light: PLightInstance;
    Shader: TShader;
    { Code calculated (on demand, when method called) using above vars. }
    FCode: TShaderSource;
    { Calculated by Prepare. Stored as TLightDefine array,
      since TLightShader.Prepare is executed very often and must be fast.
      Only TLightShader.Code actually changes it to a string. }
    Defines: array [0..9] of TLightDefine;
    DefinesCount: Cardinal;
  public
    RenderingCamera: TRenderingCamera; //< Set this after construction.
    LightingModel: TLightingModel;
    destructor Destroy; override;
    { Prepare some stuff for Code generation, update Hash for this light shader. }
    procedure Prepare(var Hash: TShaderCodeHash; const LightNumber: Cardinal);
    function Code: TShaderSource;
    procedure SetUniformsOnce(AProgram: TX3DShaderProgram);
    procedure SetDynamicUniforms(AProgram: TX3DShaderProgram);
  end;

  TLightShaders = class({$ifdef FPC}specialize{$endif} TObjectList<TLightShader>)
  private
    function Find(const Node: TAbstractLightNode; out Shader: TLightShader): boolean;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TLightUniforms ------------------------------------------------------- }

procedure TLightUniforms.SetUniform(const NamePattern: String;
  var CurrentValue: Single; const NewValue: Single);
begin
  if CurrentValue <> NewValue then
  begin
    ShaderProgram.SetUniform(Format(NamePattern, [Number]), NewValue);
    CurrentValue := NewValue;
  end;
end;

procedure TLightUniforms.SetUniform(const NamePattern: String;
  var CurrentValue: TVector3; const NewValue: TVector3);
begin
  if not TVector3.PerfectlyEquals(CurrentValue, NewValue) then
  begin
    ShaderProgram.SetUniform(Format(NamePattern, [Number]), NewValue);
    CurrentValue := NewValue;
  end;
end;

procedure TLightUniforms.SetUniform(const NamePattern: String;
  var CurrentValue: TVector4; const NewValue: TVector4);
begin
  if not TVector4.PerfectlyEquals(CurrentValue, NewValue) then
  begin
    ShaderProgram.SetUniform(Format(NamePattern, [Number]), NewValue);
    CurrentValue := NewValue;
  end;
end;

{ TLightShader --------------------------------------------------------------- }

destructor TLightShader.Destroy;
begin
  FreeAndNil(FCode);
  inherited;
end;

const
  LightDefines: array [TLightDefine] of record
    Name: String;
    Hash: UInt32;
  end =
  ( (Name: 'LIGHT%d_TYPE_POSITIONAL'  ; Hash: 107; ),
    (Name: 'LIGHT%d_TYPE_SPOT'        ; Hash: 109; ),
    (Name: 'LIGHT%d_TYPE_ENVIRONMENT' ; Hash: 421; ),
    (Name: 'LIGHT%d_TYPE_PUNCTUAL'    ; Hash: 443; ),
    (Name: 'LIGHT%d_HAS_ATTENUATION'  ; Hash: 113; ),
    (Name: 'LIGHT%d_HAS_RADIUS'       ; Hash: 127; ),
    (Name: 'LIGHT%d_HAS_AMBIENT'      ; Hash: 131; ),
    (Name: 'LIGHT%d_HAS_SPECULAR'     ; Hash: 137; ),
    (Name: 'LIGHT%d_HAS_BEAM_WIDTH'   ; Hash: 139; ),
    (Name: 'LIGHT%d_HAS_SPOT_EXPONENT'; Hash: 149; )
  );

procedure TLightShader.Prepare(var Hash: TShaderCodeHash; const LightNumber: Cardinal);

  procedure Define(const D: TLightDefine);
  begin
    Assert(DefinesCount <= High(Defines), 'Too many light #defines, increase High(TLightShader.Defines)');
    Defines[DefinesCount] := D;
    Inc(DefinesCount);
    Hash.AddInteger(LightDefines[D].Hash * (LightNumber + 1));
  end;

begin
  DefinesCount := 0;
  Hash.AddInteger(101);

  if Node is TAbstractPositionalLightNode then
  begin
    Define(ldTypePositional);
    if Node is TSpotLightNode_1 then
    begin
      Define(ldTypeSpot);
      Define(ldHasSpotExponent);
    end else
    if Node is TSpotLightNode then
    begin
      Define(ldTypeSpot);
      Define(ldHasBeamWidth);
    end;

    if TAbstractPositionalLightNode(Node).HasAttenuation then
      Define(ldHasAttenuation);

    if TAbstractPositionalLightNode(Node).HasRadius then
      Define(ldHasRadius);
  end;
  if Node.FdAmbientIntensity.Value <> 0 then
    Define(ldHasAmbient);
  if (Shader.Material is TPhongMaterialInfo) and
     (not TPhongMaterialInfo(Shader.Material).SpecularColor.IsPerfectlyZero) then
    Define(ldHasSpecular);
  if Node is TAbstractPunctualLightNode then
    Define(ldTypePunctual);
  if Node is TEnvironmentLightNode then
    Define(ldTypeEnvironment);
end;

function TLightShader.Code: TShaderSource;

  { Convert Defines list into a string of GLSL code. }
  function DefinesStr: String;
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to DefinesCount - 1 do
      Result := Result + '#define ' + Format(LightDefines[Defines[I]].Name, [Number]) + NL;
  end;

const
  TemplateLight: array [TLightingModel] of String = (
    {$I lighting_model_phong_add_light.glsl.inc},
    {$I lighting_model_physical_add_light.glsl.inc},
    {$I lighting_model_unlit_add_light.glsl.inc}
  );
var
  LightShader: String;
  LightingStage: TShaderType;
begin
  if FCode = nil then
  begin
    FCode := TShaderSource.Create;

    LightShader := TemplateLight[LightingModel];
    LightShader := StringReplace(LightShader,
      '<Light>', IntToStr(Number), [rfReplaceAll]);

    if Shader.PhongShading then
      LightingStage := stFragment
    else
      LightingStage := stVertex;
    FCode[LightingStage].Add(DefinesStr + LightShader);

    if Node <> nil then
      Shader.EnableEffects(Node.FdEffects, FCode);
  end;

  Result := FCode;
end;

procedure TLightShader.SetUniformsOnce(AProgram: TX3DShaderProgram);

  procedure SetEnvironmentLightUniforms(const EnvLight: TEnvironmentLightNode);
  begin
    if EnvLight.FdDiffuseTexture.Value <> nil then
      AProgram.UniformsTextures.Add(EnvLight.FdDiffuseTexture);
    if EnvLight.FdSpecularTexture.Value <> nil then
      AProgram.UniformsTextures.Add(EnvLight.FdSpecularTexture);
  end;

begin
  if Node is TEnvironmentLightNode then
    SetEnvironmentLightUniforms(TEnvironmentLightNode(Node));
end;

procedure TLightShader.SetDynamicUniforms(AProgram: TX3DShaderProgram);
var
  Uniforms: TLightUniforms;
  Color3, AmbientColor3: TVector3;
  Position: TVector4;
  LiPos: TAbstractPositionalLightNode;
  LiSpot1: TSpotLightNode_1;
  LiSpot: TSpotLightNode;
  LightToEyeSpace: PMatrix4;
begin
  while Number >= AProgram.FLightUniformsList.Count do
  begin
    Uniforms := TLightUniforms.Create;
    Uniforms.Number := AProgram.FLightUniformsList.Count;
    Uniforms.ShaderProgram := AProgram;
    AProgram.FLightUniformsList.Add(Uniforms);
  end;
  Uniforms := AProgram.FLightUniformsList[Number];

  { calculate Color3 = light color * light intensity }
  Color3 := Node.FdColor.Value * Node.FdIntensity.Value;

  { calculate AmbientColor3 = light color * light ambient intensity }
  AmbientColor3 := Node.FdColor.Value * Node.FdAmbientIntensity.Value;

  if Node is TAbstractPunctualLightNode then
  begin
    if Light^.WorldCoordinates then
      LightToEyeSpace := @RenderingCamera.Matrix
    else
      LightToEyeSpace := @Shader.SceneModelView;

    { This is incorrect, at least on Linux x86_64 and Darwin x86_64
      (works OK on Darwin i386), with FPC 3.0.2.
      Possibly TGenericMatrix4.Multiply has then equal addresses
      for Result and argument, although I didn't manage to "catch it red-handed"
      (it seems that merely adding a check to TGenericMatrix4.Multiply
      about it, disables this optimization, so everything is OK then). }
    // Position := Light^.Position;
    // Position := LightToEyeSpace^ * Position;
    Position := LightToEyeSpace^ * Light^.Position;

    { Note that we cut off last component of Node.Position,
      we don't need it. #defines tell the shader whether we deal with directional
      or positional light. }
    Uniforms.SetUniform('castle_LightSource%dPosition', Uniforms.Position,
      Position.XYZ);

    if Node is TAbstractPositionalLightNode then
    begin
      LiPos := TAbstractPositionalLightNode(Node);

      if LiPos.HasRadius then
      begin
        Uniforms.SetUniform('castle_LightSource%dRadius', Uniforms.Radius,
          Approximate3DScale(LightToEyeSpace^) * Light^.Radius);
      end;

      if LiPos is TSpotLightNode_1 then
      begin
        LiSpot1 := TSpotLightNode_1(Node);
        Uniforms.SetUniform('castle_LightSource%dSpotCosCutoff', Uniforms.SpotCosCutoff,
          LiSpot1.SpotCosCutoff);
        Uniforms.SetUniform('castle_LightSource%dSpotDirection', Uniforms.SpotDirection,
          LightToEyeSpace^.MultDirection(Light^.Direction));
        Uniforms.SetUniform('castle_LightSource%dSpotExponent', Uniforms.SpotExponent,
          LiSpot1.SpotExponent);
      end else
      if LiPos is TSpotLightNode then
      begin
        LiSpot := TSpotLightNode(Node);
        Uniforms.SetUniform('castle_LightSource%dSpotCosCutoff', Uniforms.SpotCosCutoff,
          LiSpot.SpotCosCutoff);
        Uniforms.SetUniform('castle_LightSource%dSpotDirection', Uniforms.SpotDirection,
          LightToEyeSpace^.MultDirection(Light^.Direction));
        Uniforms.SetUniform('castle_LightSource%dBeamWidth', Uniforms.SpotBeamWidth,
          LiSpot.FdBeamWidth.Value);
        Uniforms.SetUniform('castle_LightSource%dSpotCutoff', Uniforms.SpotCutoff,
          LiSpot.FdCutOffAngle.Value);
      end;

      if LiPos.HasAttenuation then
        Uniforms.SetUniform('castle_LightSource%dAttenuation', Uniforms.Attenuation,
          LiPos.FdAttenuation.Value);
    end;
  end;

  if Node.FdAmbientIntensity.Value <> 0 then
    Uniforms.SetUniform('castle_LightSource%dAmbientColor', Uniforms.AmbientColor, AmbientColor3);

  Uniforms.SetUniform('castle_LightSource%dColor', Uniforms.Color, Color3);
end;

{ TLightShaders -------------------------------------------------------------- }

function TLightShaders.Find(const Node: TAbstractLightNode; out Shader: TLightShader): boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Node = Node then
    begin
      Shader := Items[I];
      Exit(true);
    end;
  Shader := nil;
  Result := false;
end;

{$endif read_implementation}
