{
  Copyright 2010-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Internal screen effects stuff. }
unit CastleInternalScreenEffects;

interface

uses CastleGLShaders, CastleRenderOptions, CastleInternalRHI;

type
  TRHIScreenEffectUniformDecl = record
    Name: String;
    VarType: String;
    ArrayCount: Integer;
  end;

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
    FRHIOwnerDevice: TRHIDevice;
    FRHIVertexGLSL: String;
    FRHIFragmentGLSL: String;
    FRHIVertexShader: TRHIShaderModule;
    FRHIFragmentShader: TRHIShaderModule;
    FRHIDescriptorLayout: TRHIDescriptorSetLayout;
    FRHIPipeline: TRHIPipeline;
    FRHIUserEffectDecls: array of TRHIScreenEffectUniformDecl;
    FRHIUserEffectBuffer: TRHIBuffer;
    FRHIUserEffectDescriptorLayout: TRHIDescriptorSetLayout;
    FRHIUserEffectDescriptorSet: TRHIDescriptorSet;
    FRHIUserEffectUniformCount: Cardinal;
    FRHIUserEffectUploadCount: Cardinal;
    procedure FreeRHIResources;
    procedure ParseRHIUserEffectUniforms(const ASource: String);
  public
    constructor Create;
    destructor Destroy; override;

    property NeedsDepth: boolean read FNeedsDepth write FNeedsDepth default false;

    { In this class, UniformMissing is by default umIgnore, since it's
      normal that screen effect doesn't use some of it's uniform variables. }
    property UniformMissing default umIgnore;

    { Attach GLSL code for the screen effect (executed as part of fragment shader).
      See https://castle-engine.io/x3d_extensions_screen_effects.php . }
    property ScreenEffectShader: string read FScreenEffectShader write FScreenEffectShader;

    procedure Link; override;

    { Packs any custom uniform values set via @code(Uniform('...').SetValue(...))
      into the std140 @code(CastleUserEffectBlock) UBO (Set 2, Binding 1) and
      updates @link(RHIUserEffectDescriptorSet). Returns the number of declared
      user uniforms packed.
      @exclude }
    function UpdateRHIUniforms: Cardinal;

    { Lazily compiles or retrieves the RHI post-processing graphics pipeline for
      this screen effect on @code(ADevice), generating Vulkan GLSL 450 vertex and
      fragment shader sources with @code(screen_effect_library) helpers.
      @exclude }
    function GetOrCreateRHIPipeline(const ADevice: TRHIDevice): TRHIPipeline;
    property RHIVertexGLSL: String read FRHIVertexGLSL;
    property RHIFragmentGLSL: String read FRHIFragmentGLSL;
    property RHIVertexShader: TRHIShaderModule read FRHIVertexShader;
    property RHIFragmentShader: TRHIShaderModule read FRHIFragmentShader;
    property RHIDescriptorLayout: TRHIDescriptorSetLayout read FRHIDescriptorLayout;
    property RHIPipeline: TRHIPipeline read FRHIPipeline;
    property RHIUserEffectBuffer: TRHIBuffer read FRHIUserEffectBuffer;
    property RHIUserEffectDescriptorLayout: TRHIDescriptorSetLayout read FRHIUserEffectDescriptorLayout;
    property RHIUserEffectDescriptorSet: TRHIDescriptorSet read FRHIUserEffectDescriptorSet;
    property RHIUserEffectUniformCount: Cardinal read FRHIUserEffectUniformCount;
    property RHIUserEffectUploadCount: Cardinal read FRHIUserEffectUploadCount;
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

uses SysUtils, Classes,
  CastleGLUtils, CastleUtils, CastleStringUtils, CastleLog, CastleVectors, CastleInternalShaderRHI;

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
  if (GLFeatures <> nil) and GLFeatures.FBOMultiSampling then
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

destructor TGLSLScreenEffect.Destroy;
begin
  FreeRHIResources;
  inherited;
end;

procedure TGLSLScreenEffect.FreeRHIResources;
begin
  FreeAndNil(FRHIUserEffectDescriptorSet);
  FreeAndNil(FRHIUserEffectDescriptorLayout);
  FreeAndNil(FRHIUserEffectBuffer);
  SetLength(FRHIUserEffectDecls, 0);
  FRHIUserEffectUniformCount := 0;
  FRHIUserEffectUploadCount := 0;
  FreeAndNil(FRHIDescriptorLayout);
  FreeAndNil(FRHIFragmentShader);
  FreeAndNil(FRHIVertexShader);
  FRHIPipeline := nil; // Owned by RHIDevice pipeline cache
  FRHIOwnerDevice := nil;
  FRHIVertexGLSL := '';
  FRHIFragmentGLSL := '';
end;

procedure TGLSLScreenEffect.ParseRHIUserEffectUniforms(const ASource: String);
const
  PrecisionQualifiers: array[0..2] of String = ('lowp ', 'mediump ', 'highp ');
var
  Lines: TStringList;
  Line, Trimmed, DeclBody, VarType, VarName, BaseName, ArrStr, Qual: String;
  SpacePos, SemiPos, BracketPos, CloseBracketPos, ArrCount, Idx: Integer;
begin
  SetLength(FRHIUserEffectDecls, 0);
  if Trim(ASource) = '' then
    Exit;

  Lines := TStringList.Create;
  try
    Lines.Text := ASource;
    for Line in Lines do
    begin
      Trimmed := Trim(Line);
      if IsPrefix('uniform ', Trimmed, False) and (Pos('{', Trimmed) = 0) then
      begin
        DeclBody := Trim(SEnding(Trimmed, Length('uniform ') + 1));
        SemiPos := Pos(';', DeclBody);
        if SemiPos > 0 then
          DeclBody := Copy(DeclBody, 1, SemiPos - 1);

        for Qual in PrecisionQualifiers do
          if IsPrefix(Qual, DeclBody, False) then
          begin
            DeclBody := Trim(SEnding(DeclBody, Length(Qual) + 1));
            Break;
          end;

        SpacePos := Pos(' ', DeclBody);
        if SpacePos > 0 then
        begin
          VarType := Trim(Copy(DeclBody, 1, SpacePos - 1));
          VarName := Trim(Copy(DeclBody, SpacePos + 1, MaxInt));
          if not IsPrefix('sampler', VarType, False) then
          begin
            ArrCount := 0;
            BracketPos := Pos('[', VarName);
            if BracketPos > 0 then
            begin
              BaseName := Trim(Copy(VarName, 1, BracketPos - 1));
              CloseBracketPos := Pos(']', VarName);
              if CloseBracketPos > BracketPos + 1 then
              begin
                ArrStr := Trim(Copy(VarName, BracketPos + 1, CloseBracketPos - BracketPos - 1));
                ArrCount := StrToIntDef(ArrStr, 1);
              end;
            end else
              BaseName := VarName;

            Idx := Length(FRHIUserEffectDecls);
            SetLength(FRHIUserEffectDecls, Idx + 1);
            FRHIUserEffectDecls[Idx].Name := BaseName;
            FRHIUserEffectDecls[Idx].VarType := VarType;
            FRHIUserEffectDecls[Idx].ArrayCount := ArrCount;
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(Lines);
  end;
end;

function TGLSLScreenEffect.UpdateRHIUniforms: Cardinal;
var
  UBOData: array[0..1023] of Byte;
  UBOOffset: PtrUInt;

  procedure AlignOffset(const AAlignment: PtrUInt);
  begin
    UBOOffset := (UBOOffset + AAlignment - 1) and not (AAlignment - 1);
  end;

  procedure WriteBytes(const ASource; const ASize, AAlignment: PtrUInt);
  begin
    AlignOffset(AAlignment);
    if UBOOffset + ASize <= SizeOf(UBOData) then
    begin
      Move(ASource, UBOData[UBOOffset], ASize);
      Inc(UBOOffset, ASize);
    end;
  end;

var
  Dev: TRHIDevice;
  I, J: Integer;
  Decl: TRHIScreenEffectUniformDecl;
  Found: Boolean;
  FVal: Single;
  IVal: Int32;
  V2: TVector2;
  IV2: TVector2Integer;
  V3: TVector3;
  IV3: TVector3Integer;
  V4: TVector4;
  IV4: TVector4Integer;
  AlignedSize: PtrUInt;
  BufDesc: TRHIBufferDesc;
  Mapped: Pointer;
  Set2Bindings: array[0..4] of TRHIDescriptorBindingDesc;
  DebugPrefix: String;
begin
  Result := 0;
  Dev := FRHIOwnerDevice;
  if Dev = nil then
    Dev := RHIDevice;
  if Dev = nil then
    Exit;

  if (Length(FRHIUserEffectDecls) = 0) and (Trim(FScreenEffectShader) <> '') then
    ParseRHIUserEffectUniforms(FScreenEffectShader);
  if Length(FRHIUserEffectDecls) = 0 then
    Exit;

  UBOOffset := 0;
  FillChar(UBOData, SizeOf(UBOData), 0);

  for I := 0 to High(FRHIUserEffectDecls) do
  begin
    Decl := FRHIUserEffectDecls[I];
    Found := GetRHIUniformValue(Decl.Name, V4);
    if not Found then
      V4 := TVector4.Zero
    else
      Inc(Result);

    if Decl.VarType = 'float' then
    begin
      FVal := V4.X;
      WriteBytes(FVal, SizeOf(Single), 4);
    end else
    if (Decl.VarType = 'int') or (Decl.VarType = 'bool') then
    begin
      IVal := Round(V4.X);
      WriteBytes(IVal, SizeOf(Int32), 4);
    end else
    if Decl.VarType = 'vec2' then
    begin
      V2 := Vector2(V4.X, V4.Y);
      WriteBytes(V2, SizeOf(TVector2), 8);
    end else
    if Decl.VarType = 'ivec2' then
    begin
      IV2 := Vector2Integer(Round(V4.X), Round(V4.Y));
      WriteBytes(IV2, SizeOf(TVector2Integer), 8);
    end else
    if Decl.VarType = 'vec3' then
    begin
      V3 := Vector3(V4.X, V4.Y, V4.Z);
      WriteBytes(V3, SizeOf(TVector3), 16);
    end else
    if Decl.VarType = 'ivec3' then
    begin
      IV3 := Vector3Integer(Round(V4.X), Round(V4.Y), Round(V4.Z));
      WriteBytes(IV3, SizeOf(TVector3Integer), 16);
    end else
    if Decl.VarType = 'vec4' then
    begin
      WriteBytes(V4, SizeOf(TVector4), 16);
    end else
    if Decl.VarType = 'ivec4' then
    begin
      IV4 := Vector4Integer(Round(V4.X), Round(V4.Y), Round(V4.Z), Round(V4.W));
      WriteBytes(IV4, SizeOf(TVector4Integer), 16);
    end else
    if Decl.VarType = 'mat3' then
    begin
      for J := 0 to 2 do
      begin
        if not GetRHIUniformValue(Decl.Name + '[' + IntToStr(J) + ']', V4) then
          if (J <> 0) or (not GetRHIUniformValue(Decl.Name, V4)) then
            V4 := TVector4.Zero;
        WriteBytes(V4, SizeOf(TVector4), 16);
      end;
    end else
    if Decl.VarType = 'mat4' then
    begin
      for J := 0 to 3 do
      begin
        if not GetRHIUniformValue(Decl.Name + '[' + IntToStr(J) + ']', V4) then
          if (J <> 0) or (not GetRHIUniformValue(Decl.Name, V4)) then
            V4 := TVector4.Zero;
        WriteBytes(V4, SizeOf(TVector4), 16);
      end;
    end;
  end;

  FRHIUserEffectUniformCount := Length(FRHIUserEffectDecls);
  if UBOOffset = 0 then
    Exit;

  if Name <> '' then
    DebugPrefix := Name
  else
    DebugPrefix := 'TGLSLScreenEffect';

  AlignedSize := (UBOOffset + 15) and not PtrUInt(15);
  if AlignedSize = 0 then
    AlignedSize := 16;
  if (FRHIUserEffectBuffer = nil) or (FRHIUserEffectBuffer.Desc.Size < AlignedSize) then
  begin
    FreeAndNil(FRHIUserEffectBuffer);
    BufDesc := TRHIBufferDesc.Create(AlignedSize, [buUniform], muCpuToGpu,
      DebugPrefix + '.UserEffectUBO');
    FRHIUserEffectBuffer := Dev.CreateBuffer(BufDesc, nil);
  end;
  Mapped := FRHIUserEffectBuffer.Map;
  if Mapped <> nil then
  try
    FillChar(Mapped^, AlignedSize, 0);
    Move(UBOData[0], Mapped^, UBOOffset);
  finally
    FRHIUserEffectBuffer.Unmap;
  end;
  Inc(FRHIUserEffectUploadCount);

  if FRHIUserEffectDescriptorLayout = nil then
  begin
    FillChar(Set2Bindings, SizeOf(Set2Bindings), 0);
    Set2Bindings[0].Binding := 1;
    Set2Bindings[0].DescriptorType := dtUniformBuffer;
    Set2Bindings[0].Count := 1;
    Set2Bindings[0].StageFlags := [ssVertex, ssFragment];
    for I := 0 to 3 do
    begin
      Set2Bindings[I + 1].Binding := 2 + I;
      Set2Bindings[I + 1].DescriptorType := dtCombinedImageSampler;
      Set2Bindings[I + 1].Count := 1;
      Set2Bindings[I + 1].StageFlags := [ssVertex, ssFragment];
    end;
    FRHIUserEffectDescriptorLayout := Dev.CreateDescriptorSetLayout(Set2Bindings);
  end;

  if FRHIUserEffectDescriptorSet = nil then
    FRHIUserEffectDescriptorSet := Dev.CreateDescriptorSet(FRHIUserEffectDescriptorLayout);

  if FRHIUserEffectBuffer <> nil then
    FRHIUserEffectDescriptorSet.BindUniformBuffer(1, FRHIUserEffectBuffer, 0, FRHIUserEffectBuffer.Desc.Size);
  FRHIUserEffectDescriptorSet.Update;
end;

procedure TGLSLScreenEffect.Link;
var
  VS, FS: String;
begin
  if FScreenEffectShader = '' then
    raise EGLSLError.Create('TGLSLScreenEffect shader not assigned by AttachScreenEffectShader method');
  FreeRHIResources;
  VS := ScreenEffectVertex;
  FS := ScreenEffectFragment(NeedsDepth) + FScreenEffectShader;
  AttachVertexShader(VS);
  AttachFragmentShader(FS);
  inherited;
  if RHIDevice <> nil then
    GetOrCreateRHIPipeline(RHIDevice);
end;

function TGLSLScreenEffect.GetOrCreateRHIPipeline(const ADevice: TRHIDevice): TRHIPipeline;
var
  ShaderGen: TCastleShaderGeneratorRHI;
  DefaultMainVS, ScreenLibFrag: String;
  Bindings: array of TRHIDescriptorBindingDesc;
  PipeDesc: TRHIGraphicsPipelineDesc;
  DebugPrefix: String;
begin
  if (ADevice = nil) or (Trim(FScreenEffectShader) = '') then
    Exit(nil);
  if (FRHIPipeline <> nil) and (FRHIOwnerDevice = ADevice) then
    Exit(FRHIPipeline);

  FreeRHIResources;
  FRHIOwnerDevice := ADevice;

  if Name <> '' then
    DebugPrefix := Name
  else
    DebugPrefix := 'TGLSLScreenEffect';

  ShaderGen := TCastleShaderGeneratorRHI.Create(ADevice.Capabilities.Backend);
  try
    DefaultMainVS :=
      'void main(void)' + LineEnding +
      '{' + LineEnding +
      '  v_tex_coord = tex_coord;' + LineEnding +
      '  gl_Position = vec4(vertex, 0.0, 1.0);' + LineEnding +
      '}' + LineEnding;

    FRHIVertexGLSL :=
      ShaderGen.GenerateScreenEffectPreamble(ssVertex, NeedsDepth) + LineEnding +
      DefaultMainVS;

    ScreenLibFrag :=
      'vec2 castle_effective_screen_size() { return (castle_Screen.screen_size.x > 0.5 && castle_Screen.screen_size.y > 0.5) ? castle_Screen.screen_size : vec2(max(textureSize(screen, 0), ivec2(1, 1))); }' + LineEnding +
      'ivec2 screen_position() { return ivec2(tex_coord * castle_effective_screen_size()); }' + LineEnding +
      'int screen_x() { return int(tex_coord.x * castle_effective_screen_size().x); }' + LineEnding +
      'int screen_y() { return int(tex_coord.y * castle_effective_screen_size().y); }' + LineEnding +
      'vec2 screenf_position() { return tex_coord * castle_effective_screen_size(); }' + LineEnding +
      'float screenf_x() { return tex_coord.x * castle_effective_screen_size().x; }' + LineEnding +
      'float screenf_y() { return tex_coord.y * castle_effective_screen_size().y; }' + LineEnding +
      'vec4 screenf_get_color(vec2 position) { return texture(screen, position / castle_effective_screen_size()); }' + LineEnding +
      'vec4 screenf_get_original_color() { return texture(screen, screenf_01_position); }' + LineEnding +
      'vec4 screenf_01_get_color(vec2 position) { return texture(screen, position); }' + LineEnding +
      'vec4 screen_get_color(ivec2 position) { return screenf_get_color(vec2(position) + vec2(0.5)); }' + LineEnding;
    if NeedsDepth then
      ScreenLibFrag := ScreenLibFrag +
        'float screenf_01_get_depth(vec2 position) { return texture(screen_depth, position).r; }' + LineEnding +
        'float screenf_get_depth(vec2 position) { return screenf_01_get_depth(position / castle_effective_screen_size()); }' + LineEnding +
        'float screenf_get_original_depth() { return screenf_01_get_depth(screenf_01_position); }' + LineEnding +
        'float screen_get_depth(ivec2 position) { return screenf_get_depth(vec2(position) + vec2(0.5)); }' + LineEnding +
        'float screenf_get_depth_fast(vec2 position) { return screenf_get_depth(position); }' + LineEnding +
        'float screen_get_depth_fast(ivec2 position) { return screen_get_depth(position); }' + LineEnding;

    FRHIFragmentGLSL :=
      ShaderGen.GenerateScreenEffectPreamble(ssFragment, NeedsDepth) + LineEnding +
      ScreenLibFrag + LineEnding +
      ShaderGen.UpgradeUserEffectUniforms(FScreenEffectShader);
  finally
    FreeAndNil(ShaderGen);
  end;

  FRHIVertexShader := ADevice.CreateShaderModuleFromGLSL(
    ssVertex, FRHIVertexGLSL, DebugPrefix + '.VS');
  FRHIFragmentShader := ADevice.CreateShaderModuleFromGLSL(
    ssFragment, FRHIFragmentGLSL, DebugPrefix + '.FS');

  ParseRHIUserEffectUniforms(FScreenEffectShader);
  UpdateRHIUniforms;

  if NeedsDepth then
    SetLength(Bindings, 2)
  else
    SetLength(Bindings, 1);
  FillChar(Bindings[0], Length(Bindings) * SizeOf(TRHIDescriptorBindingDesc), 0);
  Bindings[0].Binding := 0;
  Bindings[0].DescriptorType := dtCombinedImageSampler;
  Bindings[0].Count := 1;
  Bindings[0].StageFlags := [ssFragment];
  if NeedsDepth then
  begin
    Bindings[1].Binding := 1;
    Bindings[1].DescriptorType := dtCombinedImageSampler;
    Bindings[1].Count := 1;
    Bindings[1].StageFlags := [ssFragment];
  end;
  FRHIDescriptorLayout := ADevice.CreateDescriptorSetLayout(Bindings);

  FillChar(PipeDesc, SizeOf(PipeDesc), 0);
  PipeDesc.VertexShader := FRHIVertexShader;
  PipeDesc.FragmentShader := FRHIFragmentShader;
  PipeDesc.Topology := ptTriangleList;
  PipeDesc.Rasterizer := TRHIRasterizerStateDesc.DefaultCullBack;
  PipeDesc.Rasterizer.CullMode := cmNone;
  PipeDesc.DepthStencil := TRHIDepthStencilStateDesc.DefaultDisabled;
  PipeDesc.BlendState := TRHIBlendStateDesc.DefaultOpaque;
  SetLength(PipeDesc.VertexInput.Bindings, 1);
  PipeDesc.VertexInput.Bindings[0].Binding := 0;
  PipeDesc.VertexInput.Bindings[0].Stride := SizeOf(TVector2) * 2;
  PipeDesc.VertexInput.Bindings[0].PerInstance := False;
  SetLength(PipeDesc.VertexInput.Attributes, 2);
  PipeDesc.VertexInput.Attributes[0].Location := 0;
  PipeDesc.VertexInput.Attributes[0].Binding := 0;
  PipeDesc.VertexInput.Attributes[0].Format := vafVector2;
  PipeDesc.VertexInput.Attributes[0].Offset := 0;
  PipeDesc.VertexInput.Attributes[1].Location := 1;
  PipeDesc.VertexInput.Attributes[1].Binding := 0;
  PipeDesc.VertexInput.Attributes[1].Format := vafVector2;
  PipeDesc.VertexInput.Attributes[1].Offset := SizeOf(TVector2);
  SetLength(PipeDesc.ColorAttachmentFormats, 1);
  PipeDesc.ColorAttachmentFormats[0] := pfRGBA8_UNorm;
  PipeDesc.SampleCount := 1;
  if FRHIUserEffectDescriptorLayout <> nil then
  begin
    SetLength(PipeDesc.DescriptorSetLayouts, 3);
    PipeDesc.DescriptorSetLayouts[0] := FRHIDescriptorLayout;
    PipeDesc.DescriptorSetLayouts[1] := nil;
    PipeDesc.DescriptorSetLayouts[2] := FRHIUserEffectDescriptorLayout;
  end else
  begin
    SetLength(PipeDesc.DescriptorSetLayouts, 1);
    PipeDesc.DescriptorSetLayouts[0] := FRHIDescriptorLayout;
  end;
  PipeDesc.PushConstantSize := SizeOf(TCastleScreenPushConstants);
  PipeDesc.PushConstantStages := [ssFragment];
  PipeDesc.DebugName := DebugPrefix + '.PSO';
  FRHIPipeline := ADevice.GetOrCreatePipeline(PipeDesc);
  Result := FRHIPipeline;
end;

end.
