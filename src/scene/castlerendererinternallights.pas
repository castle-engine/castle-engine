{
  Copyright 2003-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ X3D lights rendering. Internal for CastleInternalRenderer. @exclude }
unit CastleRendererInternalLights;

{$I castleconf.inc}

interface

uses CastleVectors, CastleGLUtils, X3DNodes, CastleRendererInternalShader,
  CastleTransform, CastleShapes, CastleInternalRHI, CastleInternalShaderRHI;

type
  { Callback type for @link(TLightsRenderer.LightRenderEvent).
    Modify whether the light is "on" right before it's rendered.
    By default, LightOn is the value of Light.LightNode.FdOn field.
    This is useful to filter lights. }
  TLightRenderEvent = procedure (const Shape: TShape;
    const Light: TLightInstance;
    const IsGlobalLight: Boolean; var LightOn: boolean) of object;

  { Render lights. }
  TLightsRenderer = class
  private
    FLightRenderEvent: TLightRenderEvent;
    LightsDone: array of PLightInstance;
    FMaxLightsPerShape: Cardinal;
    FRHILightsBuffer: TRHIBuffer;
    FRHILightsOwnerDevice: TRHIDevice;
    FLightsUniformBlock: TCastleLightsUniformBlock;
    FRHIActiveLightNodes: array[0..CASTLE_MAX_LIGHTS - 1] of TAbstractLightNode;
    FRHIActiveLightNodeCount: Cardinal;
    function InsideLightRadius(const Light: TLightInstance; const Shader: TShader): Boolean;
    { This avoids configuring the same light many times in case of fixed-function.

      Remembers what X3D light is set on which OpenGL light,
      and this way avoids needless reconfiguring of OpenGL lights.
      This may speed up rendering, avoiding changing OpenGL state when not
      necessary.
      Assumes that nothing changes OpenGL light properties during our
      lifetime. }
    function NeedRenderLight(Index: Integer; Light: PLightInstance): boolean;

    procedure SetMaxLightsPerShape(const Value: Cardinal);
  public
    { Statistics of how many OpenGL light setups were done
      (Statistics[true]) vs how many were avoided (Statistics[false]).
      The second number should usually be much higher, prooving
      that using a cache of LightsDone inside this class was useful. }
    Statistics: array [boolean] of Cardinal;

    RenderingCamera: TRenderingCamera;

    constructor Create;
    destructor Destroy; override;

    { Build std140 uniform block describing active punctual lights for Vulkan Set 0 Binding 1 }
    procedure BuildLightsUniformBlock(const Shape: TShape;
      const GlobalLights, SceneLights: TLightInstancesList;
      const Shader: TShader;
      out UniformBlock: TCastleLightsUniformBlock);

    { Uploads FLightsUniformBlock to FRHILightsBuffer }
    procedure UploadLightsToRHI;

    property RHILightsBuffer: TRHIBuffer read FRHILightsBuffer;
    property LightsUniformBlock: TCastleLightsUniformBlock read FLightsUniformBlock;
    property RHIActiveLightNodeCount: Cardinal read FRHIActiveLightNodeCount;
    function GetRHIActiveLightNode(const AIndex: Cardinal): TAbstractLightNode;

    { Maximum lights per shape.

      Only when ancient fixed-function rendering is used: value passed here is
      clamped by OpenGL implementation to GL_MAX_LIGHTS. }
    property MaxLightsPerShape: Cardinal read FMaxLightsPerShape
      write SetMaxLightsPerShape;

    { Set lights properties.

      Handles:

      - OpenGL fixed-function pipeline, if GLFeatures.EnableFixedFunction.
        Enables/disables and sets parameters for fixed-function lights.

      - Shader rendering using TShader.
        Lights are passed to TShader.EnableLight.

      Both GlobalLights and SceneLights may be @nil,
      which is equivalent to an empty list.

      GlobalLights, SceneLights lists are simply concatenated,
      this renderer considers lights on both lists the same.
      But TLightRenderEvent can apply different filtering on them.
    }
    procedure Render(const Shape: TShape;
      const GlobalLights, SceneLights: TLightInstancesList;
      const Shader: TShader);

    { Modify whether light is "on" right before rendering the light.

      This event, if assigned, must be deterministic,
      based purely on light properties (and other TLightRenderEvent params).
      For example, it's Ok to
      make LightRenderEvent that turns off lights that have shadowVolumes = TRUE.
      It is @italic(not Ok) to make LightRenderEvent that sets LightOn to
      a random boolean value. That because caching here assumes that
      for the same Light values, LightRenderEvent will set LightOn the same. }
    property LightRenderEvent: TLightRenderEvent
      read FLightRenderEvent write FLightRenderEvent;
  end;

implementation

uses SysUtils, Math,
  {$ifdef OpenGLES} CastleGLES, {$else} CastleGL, {$endif}
  CastleUtils, CastleBoxes, CastleInternalGLUtils, CastleApplicationProperties;

{ Set and enable OpenGL light properties based on X3D light.

  This does something only when ancient OpenGL fixed-function pipeline
  has to be used.
  For shaders, we pass light information to shaders in uniform variables,
  this is handled by CastleRendererInternalShader.

  Requires that current OpenGL matrix is modelview.
  Always preserves the matrix value (by using up to one modelview
  matrix stack slot).

  We do not examine Light.LightNode.FdOn.Value here.

  We make no assumptions about the previous state of this OpenGL light.
  We simply always set all the parameters to fully define the required
  light behavior. Some light parameters may not be set, but only because
  they are not used --- for example, if a light is not a spot light,
  then we set GL_SPOT_CUTOFF to 180 (indicates that light has no spot),
  but don't necessarily set GL_SPOT_DIRECTION or GL_SPOT_EXPONENT
  (as OpenGL will not use them anyway). }
procedure glLightFromX3DLight(
  glLightNum: Integer;
  const Light: TLightInstance;
  const RenderingCamera: TRenderingCamera);

{$ifndef OpenGLES}

  procedure SetupDirectionalLight(LightNode: TAbstractDirectionalLightNode);
  begin
    glLighti(glLightNum, GL_SPOT_CUTOFF, 180);
  end;

  procedure SetupPointLight(LightNode: TAbstractPointLightNode);
  begin
    glLighti(glLightNum, GL_SPOT_CUTOFF, 180);
  end;

  procedure SetupSpotLight_1(LightNode: TSpotLightNode_1);
  begin
    glLightv(glLightNum, GL_SPOT_DIRECTION, LightNode.FdDirection.Value);
    glLightf(glLightNum, GL_SPOT_EXPONENT, LightNode.SpotExponent);
    glLightf(glLightNum, GL_SPOT_CUTOFF, LightNode.SpotCutoffDeg);
  end;

  procedure SetupSpotLight(LightNode: TSpotLightNode);
  begin
    glLightv(glLightNum, GL_SPOT_DIRECTION, LightNode.FdDirection.Value);
    glLightf(glLightNum, GL_SPOT_EXPONENT, LightNode.SpotExponentApproximate);
    glLightf(glLightNum, GL_SPOT_CUTOFF, LightNode.SpotCutoffDeg);
  end;

var
  LightNode: TAbstractPunctualLightNode;
  SetNoAttenuation: boolean;
  Attenuat: TVector3;
  Color3, AmbientColor3: TVector3;
  Color4, AmbientColor4: TVector4;
begin
  if (RHIDevice <> nil) or (GLFeatures = nil) or (not GLFeatures.EnableFixedFunction) then
    Exit;

  if not (Light.Node is TAbstractPunctualLightNode) then
    Exit;
  LightNode := TAbstractPunctualLightNode(Light.Node);

  glLightNum := glLightNum + GL_LIGHT0;

  glPushMatrix;
  try
    if Light.WorldCoordinates then
      glLoadMatrix(RenderingCamera.Matrix);

    glMultMatrix(Light.Transform);

    glLightv(glLightNum, GL_POSITION, LightNode.PositionAndDirection);

    if LightNode is TAbstractDirectionalLightNode then
      SetupDirectionalLight(TAbstractDirectionalLightNode(LightNode)) else
    if LightNode is TAbstractPointLightNode then
      SetupPointLight(TAbstractPointLightNode(LightNode)) else
    if LightNode is TSpotLightNode_1 then
      SetupSpotLight_1(TSpotLightNode_1(LightNode)) else
    if LightNode is TSpotLightNode then
      SetupSpotLight(TSpotLightNode(LightNode)) else
      raise EInternalError.Create('Unknown light node class');

    { setup attenuation for OpenGL light }
    SetNoAttenuation := true;

    if (LightNode is TAbstractPositionalLightNode) then
    begin
      Attenuat := TAbstractPositionalLightNode(LightNode).FdAttenuation.Value;
      if not Attenuat.IsZero then
      begin
        SetNoAttenuation := false;
        glLightf(glLightNum, GL_CONSTANT_ATTENUATION, Attenuat[0]);
        glLightf(glLightNum, GL_LINEAR_ATTENUATION, Attenuat[1]);
        glLightf(glLightNum, GL_QUADRATIC_ATTENUATION, Attenuat[2]);
      end;
    end;

    if SetNoAttenuation then
    begin
      { lights with no Attenuation field or with Attenuation = (0, 0, 0)
         get default Attenuation = (1, 0, 0) }
      glLightf(glLightNum, GL_CONSTANT_ATTENUATION, 1);
      glLightf(glLightNum, GL_LINEAR_ATTENUATION, 0);
      glLightf(glLightNum, GL_QUADRATIC_ATTENUATION, 0);
    end;
  finally glPopMatrix end;

  { calculate Color4 = light color * light intensity }
  Color3 := LightNode.FdColor.Value * LightNode.FdIntensity.Value;
  Color4 := Vector4(Color3, 1);

  { calculate AmbientColor4 = light color * light ambient intensity }
  AmbientColor3 := LightNode.FdColor.Value * LightNode.FdAmbientIntensity.Value;
  AmbientColor4 := Vector4(AmbientColor3, 1);

  glLightv(glLightNum, GL_AMBIENT, AmbientColor4);
  glLightv(glLightNum, GL_DIFFUSE, Color4);
  glLightv(glLightNum, GL_SPECULAR, Color4);

  glEnable(glLightNum);
{$else}
begin
  { We pass light information to shaders in our own uniform variables,
    this is handled by CastleRendererInternalShader. }
{$endif}
end;

{ TLightsRenderer ----------------------------------------------- }

constructor TLightsRenderer.Create;
begin
  inherited Create;
  FMaxLightsPerShape := CASTLE_MAX_LIGHTS;
  SetLength(LightsDone, FMaxLightsPerShape);
end;

destructor TLightsRenderer.Destroy;
begin
  FreeAndNil(FRHILightsBuffer);
  FRHILightsOwnerDevice := nil;
  inherited Destroy;
end;

function TLightsRenderer.InsideLightRadius(const Light: TLightInstance;
  const Shader: TShader): Boolean;
var
  LightPosNode: TAbstractPositionalLightNode;
  ShapeBox: TBox3D;
  RadiusInWorld: Single;
  LocationInWorld: TVector3;
begin
  Result := true;

  if (Shader <> nil) and Shader.ShapeBoundingBoxInWorldDynamic then
    Exit;

  if Light.Node is TAbstractPositionalLightNode then
  begin
    LightPosNode := TAbstractPositionalLightNode(Light.Node);
    if LightPosNode.HasRadius then
    begin
      if Shader = nil then Exit;
      ShapeBox := Shader.ShapeBoundingBoxInWorld;
      if ShapeBox.IsEmpty then
        Exit(false);

      if Light.WorldCoordinates then
      begin
        RadiusInWorld := Light.Radius;
        LocationInWorld := Light.Location;
      end else
      begin
        RadiusInWorld := Approximate3DScale(Shader.SceneTransform) * Light.Radius;
        LocationInWorld := Shader.SceneTransform.MultPoint(Light.Location);
      end;

      if ShapeBox.PointDistanceSqr(LocationInWorld) > Sqr(RadiusInWorld) then
        Exit(false);
    end;
  end;
end;

procedure TLightsRenderer.BuildLightsUniformBlock(const Shape: TShape;
  const GlobalLights, SceneLights: TLightInstancesList;
  const Shader: TShader;
  out UniformBlock: TCastleLightsUniformBlock);

  var
    NextShadowSlotIdx: Integer;
    EffectiveMaxLights: Cardinal;
    AddedLights: array[0..CASTLE_MAX_LIGHTS - 1] of PLightInstance;
    AddedCount: Cardinal;

  function IsLightAlreadyAdded(const ALight: PLightInstance): Boolean;
  var
    J: Integer;
  begin
    Result := False;
    if ALight = nil then
      Exit;
    for J := 0 to Integer(AddedCount) - 1 do
      if (AddedLights[J] <> nil) and
         (AddedLights[J]^.Node = ALight^.Node) and
         TMatrix4.PerfectlyEquals(AddedLights[J]^.Transform, ALight^.Transform) then
        Exit(True);
  end;

  procedure AddList(const Lights: TLightInstancesList; const IsGlobalLight: Boolean);
  const
    ProjectorScalingMatrixVulkan: TMatrix4 = (Data:
    ( (0.5,  0.0, 0.0, 0.0),
      (0.0, -0.5, 0.0, 0.0),
      (0.0,  0.0, 0.5, 0.0),
      (0.5,  0.5, 0.5, 1.0) ));
  var
    I, Idx, ShadowSlotIdx: Integer;
    LightOn: boolean;
    Light: PLightInstance;
    Node: TAbstractLightNode;
    LightToEyeSpace: TMatrix4;
    Color3, AmbientColor3: TVector3;
    EyePos: TVector4;
    Dir, Att: TVector3;
    Rad: Single;
    LiPos: TAbstractPositionalLightNode;
    LiSpot1: TSpotLightNode_1;
    LiSpot: TSpotLightNode;
    ProjectorMatrix: TMatrix4;
  begin
    if (Lights = nil) or (UniformBlock.Count >= EffectiveMaxLights) then Exit;

    for I := 0 to Lights.Count - 1 do
    begin
      Light := PLightInstance(Lights.Ptr(I));
      if IsLightAlreadyAdded(Light) then
        Continue;

      LightOn := Light^.Node.FdOn.Value;
      if Assigned(LightRenderEvent) then
        LightRenderEvent(Shape, Light^, IsGlobalLight, LightOn);

      if LightOn and InsideLightRadius(Light^, Shader) then
      begin
        Node := Light^.Node;
        Color3 := Node.FdColor.Value * Node.FdIntensity.Value;
        AmbientColor3 := Node.FdColor.Value * Node.FdAmbientIntensity.Value;

        if Light^.WorldCoordinates then
          LightToEyeSpace := RenderingCamera.Matrix
        else
        if Shader <> nil then
          LightToEyeSpace := Shader.SceneModelView
        else
          LightToEyeSpace := RenderingCamera.Matrix;

        Idx := -1;
        if Node is TAbstractDirectionalLightNode then
        begin
          Dir := LightToEyeSpace.MultDirection(Light^.Direction);
          Idx := UniformBlock.AddDirectionalLight(Dir, Color3, AmbientColor3);
        end else
        if Node is TSpotLightNode_1 then
        begin
          LiSpot1 := TSpotLightNode_1(Node);
          EyePos := LightToEyeSpace * Light^.Position;
          Dir := LightToEyeSpace.MultDirection(Light^.Direction);
          Att := LiSpot1.FdAttenuation.Value;
          Rad := LiSpot1.Radius;
          if not Light^.WorldCoordinates then
            Rad := Approximate3DScale(LightToEyeSpace) * Rad;
          Idx := UniformBlock.AddSpotLight(EyePos.XYZ, Dir, Color3, AmbientColor3,
            Att, Rad, LiSpot1.SpotCosCutoff, LiSpot1.SpotCutoffDeg * Pi / 180.0,
            LiSpot1.SpotCutoffDeg * Pi / 180.0, LiSpot1.SpotExponent);
        end else
        if Node is TSpotLightNode then
        begin
          LiSpot := TSpotLightNode(Node);
          EyePos := LightToEyeSpace * Light^.Position;
          Dir := LightToEyeSpace.MultDirection(Light^.Direction);
          Att := LiSpot.FdAttenuation.Value;
          Rad := LiSpot.Radius;
          if not Light^.WorldCoordinates then
            Rad := Approximate3DScale(LightToEyeSpace) * Rad;
          Idx := UniformBlock.AddSpotLight(EyePos.XYZ, Dir, Color3, AmbientColor3,
            Att, Rad, LiSpot.SpotCosCutoff, LiSpot.FdCutOffAngle.Value,
            LiSpot.FdBeamWidth.Value, 0.0);
        end else
        if Node is TAbstractPointLightNode then
        begin
          LiPos := TAbstractPositionalLightNode(Node);
          EyePos := LightToEyeSpace * Light^.Position;
          Att := LiPos.FdAttenuation.Value;
          Rad := LiPos.Radius;
          if not Light^.WorldCoordinates then
            Rad := Approximate3DScale(LightToEyeSpace) * Rad;
          Idx := UniformBlock.AddPointLight(EyePos.XYZ, Color3, AmbientColor3, Att, Rad);
        end;

        if Idx >= 0 then
        begin
          if Idx < CASTLE_MAX_LIGHTS then
          begin
            FRHIActiveLightNodes[Idx] := Node;
            if Cardinal(Idx + 1) > FRHIActiveLightNodeCount then
              FRHIActiveLightNodeCount := Cardinal(Idx + 1);
          end;
          if AddedCount < CASTLE_MAX_LIGHTS then
          begin
            AddedLights[AddedCount] := Light;
            Inc(AddedCount);
          end;

          if (Node is TAbstractPunctualLightNode) and
             (TAbstractPunctualLightNode(Node).DefaultShadowMap <> nil) then
          begin
            RenderingCamera.InverseMatrixNeeded;
            ProjectorMatrix := (ProjectorScalingMatrixVulkan *
              TAbstractPunctualLightNode(Node).GetProjectorMatrix) *
              RenderingCamera.InverseMatrix;
            ShadowSlotIdx := -1;
            if (Shape <> nil) and (Shape.InternalShadowMaps <> nil) then
              ShadowSlotIdx := Shape.InternalShadowMaps.IndexOf(
                TAbstractPunctualLightNode(Node).DefaultShadowMap);
            if ShadowSlotIdx < 0 then
            begin
              ShadowSlotIdx := NextShadowSlotIdx;
              if NextShadowSlotIdx < 3 then
                Inc(NextShadowSlotIdx);
            end;
            UniformBlock.SetProjectorMatrix(Idx, ProjectorMatrix, EnsureRange(ShadowSlotIdx, 0, 3));
          end;
        end;

        if UniformBlock.Count >= EffectiveMaxLights then
          Exit;
      end;
    end;
  end;

begin
  UniformBlock.Clear;
  FRHIActiveLightNodeCount := 0;
  FillChar(FRHIActiveLightNodes, SizeOf(FRHIActiveLightNodes), 0);
  NextShadowSlotIdx := 0;
  AddedCount := 0;
  EffectiveMaxLights := Min(FMaxLightsPerShape, Cardinal(CASTLE_MAX_LIGHTS));
  if EffectiveMaxLights = 0 then
    Exit;
  AddList(GlobalLights, True);
  if UniformBlock.Count < EffectiveMaxLights then
    AddList(SceneLights, False);
end;

function TLightsRenderer.GetRHIActiveLightNode(const AIndex: Cardinal): TAbstractLightNode;
begin
  if AIndex < FRHIActiveLightNodeCount then
    Result := FRHIActiveLightNodes[AIndex]
  else
    Result := nil;
end;

procedure TLightsRenderer.UploadLightsToRHI;
var
  BufDesc: TRHIBufferDesc;
begin
  if RHIDevice = nil then
    Exit;

  if (FRHILightsBuffer <> nil) and (FRHILightsOwnerDevice <> RHIDevice) then
    FreeAndNil(FRHILightsBuffer);
  if FRHILightsBuffer = nil then
  begin
    BufDesc := TRHIBufferDesc.Create(SizeOf(TCastleLightsUniformBlock), [buUniform], muCpuToGpu, 'CastleLightsUBO');
    FRHILightsBuffer := RHIDevice.CreateBuffer(BufDesc, @FLightsUniformBlock);
    FRHILightsOwnerDevice := RHIDevice;
  end else
    FRHILightsBuffer.UploadData(FLightsUniformBlock, SizeOf(TCastleLightsUniformBlock));
end;

procedure TLightsRenderer.SetMaxLightsPerShape(const Value: Cardinal);
begin
  if FMaxLightsPerShape <> Value then
  begin
    FMaxLightsPerShape := Value;
    if (RHIDevice = nil) and (GLFeatures <> nil) and GLFeatures.EnableFixedFunction then
      MinVar(FMaxLightsPerShape, GLFeatures.MaxLights);
    SetLength(LightsDone, FMaxLightsPerShape);
  end;
end;

function TLightsRenderer.NeedRenderLight(Index: Integer; Light: PLightInstance): boolean;
begin
  Result := not (
    ( { Light Index is currently disabled, and we want it disabled: Ok. }
      ( (LightsDone[Index] = nil) and
        (Light = nil) )
      or
      { Light Index is currently enabled, and we want it enabled,
        with the same LightNode and Transform: Ok.
        (Other TLightInstance record properties are calculated from
        LightNode and Transform, so no need to compare them). }
      ( (LightsDone[Index] <> nil) and
        (Light <> nil) and
        (LightsDone[Index]^.Node = Light^.Node) and
        (TMatrix4.PerfectlyEquals(
          LightsDone[Index]^.Transform, Light^.Transform)) )
    ));
  if Result then
    LightsDone[Index] := Light;
  Inc(Statistics[Result]);
end;

procedure TLightsRenderer.Render(const Shape: TShape;
  const GlobalLights, SceneLights: TLightInstancesList;
  const Shader: TShader);
var
  LightsEnabled: Cardinal;

  procedure AddList(const Lights: TLightInstancesList; const IsGlobalLight: Boolean);
  var
    I: Integer;
    LightOn: boolean;
    Light: PLightInstance;
  begin
    for I := 0 to Lights.Count - 1 do
    begin
      Light := PLightInstance(Lights.Ptr(I));

      LightOn := Light^.Node.FdOn.Value;
      if Assigned(LightRenderEvent) then
        LightRenderEvent(Shape, Light^, IsGlobalLight, LightOn);

      if LightOn and InsideLightRadius(Light^, Shader) then
      begin
        if NeedRenderLight(LightsEnabled, Light) then
          glLightFromX3DLight(LightsEnabled, Light^, RenderingCamera);
        Shader.EnableLight(LightsEnabled, Light);
        Inc(LightsEnabled);
        if LightsEnabled >= FMaxLightsPerShape then Exit;
      end;
    end;
  end;

{$ifndef OpenGLES}
var
  I: Integer;
{$endif}
begin
  Assert(RenderingCamera <> nil);

  if (RHIDevice <> nil) or ((ApplicationProperties <> nil) and (ApplicationProperties.RenderBackend = backendVulkan)) then
  begin
    BuildLightsUniformBlock(Shape, GlobalLights, SceneLights, Shader, FLightsUniformBlock);
    UploadLightsToRHI;
  end;

  LightsEnabled := 0;
  if LightsEnabled >= FMaxLightsPerShape then Exit;

  if GlobalLights <> nil then
  begin
    AddList(GlobalLights, true);
    if LightsEnabled >= FMaxLightsPerShape then Exit;
  end;

  if SceneLights <> nil then
  begin
    AddList(SceneLights, false);
    if LightsEnabled >= FMaxLightsPerShape then Exit;
  end;

  if (RHIDevice = nil) and (GLFeatures <> nil) and GLFeatures.EnableFixedFunction then
  begin
    {$ifndef OpenGLES}
    { Disable remaining light for fixed-function pipeline }
    for I := LightsEnabled to FMaxLightsPerShape - 1 do
      if NeedRenderLight(I, nil) then
        glDisable(GL_LIGHT0 + I);
    {$endif}
  end;
end;

end.
