{
  Copyright 2010-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Screen effects (post-processing) using shaders (TCastleScreenEffects). }
unit CastleScreenEffects;

{$I castleconf.inc}

interface

uses SysUtils, Classes,
  {$ifdef OpenGLES} CastleGLES, {$else} CastleGL, {$endif}
  CastleVectors, CastleGLShaders, CastleUIControls, X3DNodes, CastleGLImages,
  CastleRectangles, CastleScene, CastleTransform, CastleCameras, CastleGLUtils,
  CastleRenderOptions, CastleInternalRenderer, CastleInternalRHI, CastleInternalShaderRHI;

type
  { Screen effects are shaders that post-processing the rendered screen.
    The effects may be defined using @url(https://castle-engine.io/x3d_extensions_screen_effects.php
    X3D node ScreenEffect).

    This control applies screen effects (post-processing)
    on the rendering done by children and (when this class is used as an ancestor)
    it's descendants.

    It can be used to apply screen effects over any UI control,
    like @link(TCastleViewport),
    @link(TCastleButton), @link(TCastleImageControl) and so on.
    Simply place the desired control as child of this control.

    To make it easier to apply effects on @link(TCastleViewport),
    it already descends from this class. So, while you can, you don't need to wrap
    @link(TCastleViewport) instance inside another @link(TCastleScreenEffects)
    instance. You can instead directly call @link(AddScreenEffect) on your
    @link(TCastleViewport) instance.

    Note that the UI controls rendered for the screen effects
    (our children and descendants) must always initialize and fill
    colors of the entire rectangle (@link(RenderRect)) of this control.
    Otherwise, the results are undefined, as an internal texture that is used
    for screen effects is initially undefined.
    You may use e.g. @link(TCastleRectangleControl) to fill the background with a solid color
    from @link(TCastleRectangleControl.Color).
    Or use @link(TCastleViewport) with @link(TCastleViewport.Transparent) = @false (default)
    which fills background with @link(TCastleViewport.BackgroundColor).
  }
  TCastleScreenEffects = class(TCastleUserInterface)
  strict private
    type
      TScreenPoint = packed record
        Position: TVector2;
        TexCoord: TVector2;
      end;
    var
      { Scene containing screen effects }
      ScreenEffectsScene: TCastleScene;
      ScreenEffectsRoot: TX3DRootNode;
      FScreenEffectsTimeScale: Single;
      { World to pass dummy camera position to ScreenEffectsScene. }
      World: TCastleRootTransform;
      FRenderer: TRenderer;
      FPrepareParams: TPrepareParams;

      { OpenGL(ES) resources for screen effects. }
      { If a texture for screen effects is ready, then
        ScreenEffectTextureDest/Src/Depth/Target are non-zero and
        ScreenEffectRTT is non-nil.
        Also, ScreenEffectTextureWidth/Height indicate size of the texture,
        as well as ScreenEffectRTT.Width/Height. }
      ScreenEffectTextureDest, ScreenEffectTextureSrc: TGLTexture;
      ScreenEffectTextureTarget: TGLenum;
      ScreenEffectTextureDepth: TGLTexture;
      ScreenEffectRTT: TGLRenderToTexture;
      ScreenEffectTextureWidth: Cardinal;
      ScreenEffectTextureHeight: Cardinal;
      { Saved ScreenEffectsCount/NeedDepth result, during rendering. }
      CurrentScreenEffectsCount: Integer;
      CurrentScreenEffectsNeedDepth: boolean;
      ScreenPointVbo: TGLBuffer;
      ScreenPointVao: TVertexArrayObject;
      ScreenPoint: packed array [0..3] of TScreenPoint;

      { RHI post-processing resources (Vulkan / explicit RHI when RHIDevice <> nil) }
      FRHIScreenTextureDest: TRHITexture;
      FRHIScreenTextureSrc: TRHITexture;
      FRHIScreenTextureDepth: TRHITexture;
      FRHIScreenSampler: TRHISampler;
      FRHIScreenQuadVBO: TRHIBuffer;
      FRHIScreenQuadIBO: TRHIBuffer;
      FRHIScreenLayoutColorOnly: TRHIDescriptorSetLayout;
      FRHIScreenLayoutColorAndDepth: TRHIDescriptorSetLayout;
      FRHIScreenDefaultVS: TRHIShaderModule;
      FRHIScreenDefaultFS: TRHIShaderModule;
      FRHIScreenDefaultPipelineColor: TRHIPipeline;
      FRHIScreenDefaultPipelineDepth: TRHIPipeline;
      FRHIScreenDescriptorSets: array of TRHIDescriptorSet;
      FRHIScreenPassCount: Cardinal;
      FRHIScreenDrawCount: Cardinal;

      FScreenEffectsEnable: Boolean;

    function GetScreenEffectsCount: Cardinal;
    function GetScreenEffectsNeedDepth: Boolean;
    function GetScreenEffect(const Index: Integer): TGLSLProgram;

    procedure GLContextCloseEvent(Sender: TObject);
    procedure FreeRHIScreenDescriptorSets;
  protected
    { Valid only between Render and RenderOverChildren calls.
      Tells whether we actually use screen effects, thus RenderWithoutScreenEffects
      renders to texture.
      Read-only in descendants. }
    RenderScreenEffects: Boolean;

    { Descendants with special rendering code should override this,
      and @italic(never) override @link(Render) or @link(RenderOverChildren)
      (as those two methods have special implementation in this class).

      This rendering method will be called regardless if we have
      or not some screen effects.
      When no screen effects are actually used (e.g. @link(AddScreenEffect)
      wasn't used, @code(InternalExtraScreenEffectsCount) is zero), then our @link(Render)
      trivially calls this method without doing anything else. }
    procedure RenderWithoutScreenEffects; virtual;

    { Render these additional screen effects, defined by explicit TGLSLProgram
      instead of by AddScreenEffect call.
      This is internal, only to be used by TCastleViewport.
      @exclude }
    function InternalExtraGetScreenEffects(
      const Index: Integer): TGLSLProgram; virtual;
    { @exclude }
    function InternalExtraGetScreenEffectRHIPipeline(
      const Index: Integer): TRHIPipeline; virtual;
    { @exclude }
    function InternalExtraGetScreenEffectRHIUserDescriptorSet(
      const Index: Integer): TRHIDescriptorSet; virtual;
    { @exclude }
    function InternalExtraScreenEffectsCount: Integer; virtual;
    { @exclude }
    function InternalExtraScreenEffectsNeedDepth: Boolean; virtual;
    { @exclude }
    function GetScreenEffectsNear: Single; virtual;
    { @exclude }
    function GetScreenEffectsFar: Single; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Add or remove a ScreenEffect node that defines new shader screen effect.
      See https://castle-engine.io/x3d_extensions_screen_effects.php .

      In the simple case, you can pass here an instance of TScreenEffectNode.
      In a general case, you can passs here any X3D node you want,
      just remember that only the TScreenEffectNode inside will be rendered.
      You can e.g. pass a TGroupNode with TScreenEffectNode and TTimeSensorNode
      as children,
      and use TTimeSensorNode to provide time to your shader uniform parameter.
      Or you can e.g. pass an X3D graph loaded from X3D file
      using @link(X3DLoad.LoadNode), this way you can define effects
      inside an external X3D file, e.g. like this:

      @longCode(#
      ScreenEffects.AddScreenEffect(
        LoadNode('castle-data:/screen_effects_scene.x3dv'));
      #)

      If you're looking for inspirations what to put in screen_effects_scene.x3dv,
      see https://github.com/castle-engine/demo-models/tree/master/screen_effects .
      See also CGE example in "examples/screen_effects_demo/" directory
      ( https://github.com/castle-engine/castle-engine/tree/master/examples/screen_effects_demo )
      that shows more screen effects code,
      and shows how to load or construct X3D node graph with ScreenEffect.

      The memory management of the node added here is automatic:
      the added screen effect becomes owned by the internal X3D Group node.
      It's reference-count is increased at AddScreenEffect, and decreased
      at RemoveScreenEffect, and it is automatically freed when reference-count
      drops to zero.

      This means that, if you create the provided Node by code,
      and don't add it anywhere else, then the node will be freed
      automatically at our destructor (if you call AddScreenEffect
      and do not call RemoveScreenEffect on it),
      or at RemoveScreenEffect call.
      If you don't want this, use @link(TX3DNode.WaitForRelease)
      to manage the node destruction yourself.

      Note that the given Node should not be used by other TCastleScene instances.
      In general, a node should not be present in more than one TCastleScene instance,
      and we already insert the node into an internal TCastleScene instance.
      Use TX3DNode.DeepCopy if necessary to duplicate node into multiple scenes.

      Note that you can enable/disable the effect using @link(TScreenEffectNode.Enabled),
      or enable/disable all using @link(ScreenEffectsEnable).
      You do not need to remove the node by @link(RemoveScreenEffect) if you only want
      to disable it temporarily.
    }
    procedure AddScreenEffect(const Node: TAbstractChildNode);
    procedure RemoveScreenEffect(const Node: TAbstractChildNode);

    procedure BeforeRender; override;
    procedure Render; override;
    procedure RenderOverChildren; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    procedure GLContextClose; override;

    { Scale time passing inside TimeSensor nodes you add as part of AddScreenEffect.
      May be 0 to stop time passing.
      This has deliberately long name, instead of simple TimeScale,
      to make it clear that it's completely independent from
      @link(TCastleAbstractRootTransform.TimeScale). }
    property ScreenEffectsTimeScale: Single
      read FScreenEffectsTimeScale write FScreenEffectsTimeScale {$ifdef FPC}default 1{$endif};

    { Enable or disable all screen effects added by AddScreenEffect.
      When this is @true, particular screen effects can still be enabled/disabled
      using @link(TScreenEffectNode.Enabled).
      When this is @false, all effects are disabled, regardless of
      @link(TScreenEffectNode.Enabled). }
    property ScreenEffectsEnable: Boolean
      read FScreenEffectsEnable write FScreenEffectsEnable default true;

    { Make the screen effects rendering resources ready (e.g. link shaders). }
    procedure PrepareResources;

    { RHI Post-Processing Screen Effects resources and ping-pong pass execution
      when @link(RHIDevice) <> @nil.
      @groupBegin }
    procedure EnsureRHIScreenResources(const AWidth, AHeight: Cardinal;
      const ANeedDepth: Boolean);
    function GetScreenEffectRHIPipeline(const Index: Integer): TRHIPipeline;
    function GetScreenEffectRHIUserDescriptorSet(const Index: Integer): TRHIDescriptorSet;
    function ExecuteRHIScreenEffectPasses(
      const APassPipelines: array of TRHIPipeline;
      const AWidth, AHeight: Cardinal;
      const ANeedDepth: Boolean;
      const AFinalTargetTexture: TRHITexture = nil;
      const ANearPlane: Single = 0.1;
      const AFarPlane: Single = 100.0): Cardinal;
    property RHIScreenTextureSrc: TRHITexture read FRHIScreenTextureSrc;
    property RHIScreenTextureDest: TRHITexture read FRHIScreenTextureDest;
    property RHIScreenTextureDepth: TRHITexture read FRHIScreenTextureDepth;
    property RHIScreenQuadVBO: TRHIBuffer read FRHIScreenQuadVBO;
    property RHIScreenQuadIBO: TRHIBuffer read FRHIScreenQuadIBO;
    property RHIScreenLayoutColorOnly: TRHIDescriptorSetLayout read FRHIScreenLayoutColorOnly;
    property RHIScreenLayoutColorAndDepth: TRHIDescriptorSetLayout read FRHIScreenLayoutColorAndDepth;
    property RHIScreenPassCount: Cardinal read FRHIScreenPassCount;
    property RHIScreenDrawCount: Cardinal read FRHIScreenDrawCount;
    { @groupEnd }
  end;

implementation

uses
  {$ifdef WASI} Job.Js, {$endif}
  Math, CastleUtils, CastleLog, CastleColors, CastleRenderContext, CastleInternalGLUtils,
  CastleApplicationProperties, CastleInternalScreenEffects;

procedure SwapValues(var A, B: TGLTexture);
var
  Tmp: TGLTexture;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;

procedure SwapRHIValues(var A, B: TRHITexture);
var
  Tmp: TRHITexture;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;

{ TCastleScreenEffects ------------------------------------------------------- }

constructor TCastleScreenEffects.Create(AOwner: TComponent);
begin
  inherited;
  FScreenEffectsTimeScale := 1;
  FScreenEffectsEnable := true;

  FRenderer := TRenderer.Create(nil);

  FPrepareParams := TPrepareParams.Create;
  FPrepareParams.GlobalLights := nil;
  FPrepareParams.GlobalFog := nil;
  FPrepareParams.RendererToPrepareShapes := FRenderer;

  ApplicationProperties.OnGLContextCloseObject.Add(
    {$ifdef FPC}@{$endif} GLContextCloseEvent);
end;

destructor TCastleScreenEffects.Destroy;
begin
  ApplicationProperties.OnGLContextCloseObject.Remove(
    {$ifdef FPC}@{$endif} GLContextCloseEvent);

  FreeAndNil(FPrepareParams);
  FreeAndNil(FRenderer);
  inherited;
end;

procedure TCastleScreenEffects.GLContextCloseEvent(Sender: TObject);
begin
  if ScreenEffectsScene <> nil then
    ScreenEffectsScene.GLContextClose;
end;

procedure TCastleScreenEffects.AddScreenEffect(const Node: TAbstractChildNode);
begin
  if ScreenEffectsScene = nil then
  begin
    ScreenEffectsRoot := TX3DRootNode.Create;

    { We create TCastleScene that is used only for rendering screen effects.
      This way we can make all "support" things for screen effects work,
      e.g. textures will be initialized using CastleInternalRenderer,
      TimeSensor will work and can be fed into shader uniform etc. }
    ScreenEffectsScene := TCastleScene.Create(Self);
    ScreenEffectsScene.Name := 'ScreenEffectsScene';
    ScreenEffectsScene.Load(ScreenEffectsRoot, true);
    ScreenEffectsScene.ProcessEvents := true;

    (* We setup World with camera knowledge to make some ProximitySensors working,
       like a dummy "ProximitySensors { size 10000 10000 10000 }". *)
    World := TCastleRootTransform.Create(Self);
    World.Add(ScreenEffectsScene);
    World.MainCamera := TCastleCamera.Create(Self);
    World.Add(World.MainCamera); // necessary to be able to query camera world coords
  end;

  { Note that AddChildren by default has AllowDuplicates=true,
    and enables multiple instances of this node on a list. That's good. }
  ScreenEffectsRoot.AddChildren(Node);

  { Send InternalCameraChanged now, to activate ProximitySensors inside new nodes. }
  ScreenEffectsScene.InternalCameraChanged;
end;

procedure TCastleScreenEffects.RemoveScreenEffect(const Node: TAbstractChildNode);
begin
  if ScreenEffectsRoot <> nil then
  begin
    ScreenEffectsRoot.FdChildren.Remove(Node);
    ScreenEffectsRoot.FdChildren.Changed;
  end;
end;

function TCastleScreenEffects.InternalExtraGetScreenEffects(const Index: Integer): TGLSLProgram;
begin
  raise EInternalError.Create('Override InternalExtraGetScreenEffects if you override InternalExtraScreenEffectsCount. In TCastleScreenEffects, InternalExtraScreenEffectsCount returns zero, so InternalExtraGetScreenEffects should never be called');
  Result := nil; // silence FPC warnings about undefined result
end;

function TCastleScreenEffects.InternalExtraGetScreenEffectRHIPipeline(const Index: Integer): TRHIPipeline;
begin
  Result := nil;
end;

function TCastleScreenEffects.InternalExtraGetScreenEffectRHIUserDescriptorSet(const Index: Integer): TRHIDescriptorSet;
begin
  Result := nil;
end;

function TCastleScreenEffects.InternalExtraScreenEffectsCount: Integer;
begin
  Result := 0;
end;

function TCastleScreenEffects.InternalExtraScreenEffectsNeedDepth: Boolean;
begin
  Result := false;
end;

function TCastleScreenEffects.GetScreenEffectsNear: Single;
begin
  Result := 0.1;
end;

function TCastleScreenEffects.GetScreenEffectsFar: Single;
begin
  Result := 100.0;
end;

function TCastleScreenEffects.GetScreenEffectsCount: Cardinal;
begin
  Result := 0;
  if ScreenEffectsEnable then
  begin
    if ScreenEffectsScene <> nil then
      { TCastleScene already implemented logic to only count screen effects
        that are enabled, and their GLSL code linked successfully.
        Cool, we depend on it. }
      Result := Result + ScreenEffectsScene.InternalScreenEffectsCount;
    Result := Result + InternalExtraScreenEffectsCount;
  end;
end;

function TCastleScreenEffects.GetScreenEffectsNeedDepth: Boolean;
begin
  Result := ScreenEffectsEnable and (
    ((ScreenEffectsScene <> nil) and ScreenEffectsScene.InternalScreenEffectsNeedDepth) or
    InternalExtraScreenEffectsNeedDepth
  );
end;

function TCastleScreenEffects.GetScreenEffect(const Index: Integer): TGLSLProgram;
var
  SceneEffects: Integer;
begin
  if ScreenEffectsScene <> nil then
    SceneEffects := ScreenEffectsScene.InternalScreenEffectsCount
  else
    SceneEffects := 0;

  if Index < SceneEffects then
    Result := ScreenEffectsScene.InternalScreenEffects(Index)
  else
    Result := InternalExtraGetScreenEffects(Index - SceneEffects);
end;

function TCastleScreenEffects.GetScreenEffectRHIPipeline(const Index: Integer): TRHIPipeline;
var
  SceneEffects: Integer;
begin
  Result := nil;
  if RHIDevice = nil then
    Exit;

  if ScreenEffectsScene <> nil then
    SceneEffects := ScreenEffectsScene.InternalScreenEffectsCount
  else
    SceneEffects := 0;

  if (Index >= 0) and (Index < SceneEffects) then
    Result := ScreenEffectsScene.InternalScreenEffectRHIPipeline(Index)
  else
  if Index >= SceneEffects then
    Result := InternalExtraGetScreenEffectRHIPipeline(Index - SceneEffects);

  if Result = nil then
  begin
    if CurrentScreenEffectsNeedDepth then
      Result := FRHIScreenDefaultPipelineDepth
    else
      Result := FRHIScreenDefaultPipelineColor;
  end;
end;

function TCastleScreenEffects.GetScreenEffectRHIUserDescriptorSet(const Index: Integer): TRHIDescriptorSet;
var
  SceneEffects: Integer;
begin
  Result := nil;
  if RHIDevice = nil then
    Exit;

  if ScreenEffectsScene <> nil then
    SceneEffects := ScreenEffectsScene.InternalScreenEffectsCount
  else
    SceneEffects := 0;

  if (Index >= 0) and (Index < SceneEffects) then
    Result := ScreenEffectsScene.InternalScreenEffectRHIUserDescriptorSet(Index)
  else
  if Index >= SceneEffects then
    Result := InternalExtraGetScreenEffectRHIUserDescriptorSet(Index - SceneEffects);
end;

procedure TCastleScreenEffects.FreeRHIScreenDescriptorSets;
var
  I: Integer;
begin
  for I := 0 to High(FRHIScreenDescriptorSets) do
    FreeAndNil(FRHIScreenDescriptorSets[I]);
  SetLength(FRHIScreenDescriptorSets, 0);
end;

procedure TCastleScreenEffects.EnsureRHIScreenResources(const AWidth, AHeight: Cardinal;
  const ANeedDepth: Boolean);
const
  QuadIndices: packed array[0..5] of UInt16 = (0, 1, 2, 0, 2, 3);
var
  TexDesc: TRHITextureDesc;
  SampDesc: TRHISamplerDesc;
  BufDesc: TRHIBufferDesc;
  BindingsColorOnly: array[0..0] of TRHIDescriptorBindingDesc;
  BindingsColorDepth: array[0..1] of TRHIDescriptorBindingDesc;
  PipeDesc: TRHIGraphicsPipelineDesc;
begin
  if (RHIDevice = nil) or (AWidth = 0) or (AHeight = 0) then
    Exit;

  if (FRHIScreenTextureDest = nil) or
     (FRHIScreenTextureSrc = nil) or
     (FRHIScreenTextureDepth = nil) or
     (ScreenEffectRTT = nil) or
     (ScreenEffectTextureWidth <> AWidth) or
     (ScreenEffectTextureHeight <> AHeight) then
  begin
    FreeAndNil(ScreenEffectRTT);
    FreeAndNil(FRHIScreenTextureDest);
    FreeAndNil(FRHIScreenTextureSrc);
    FreeAndNil(FRHIScreenTextureDepth);

    ScreenEffectTextureWidth := AWidth;
    ScreenEffectTextureHeight := AHeight;

    TexDesc := TRHITextureDesc.Create2D(AWidth, AHeight, pfRGBA8_UNorm,
      [tuColorAttachment, tuSampled, tuTransferSrc, tuTransferDst], 1, 1);
    TexDesc.DebugName := 'TCastleScreenEffects.Dest';
    FRHIScreenTextureDest := RHIDevice.CreateTexture(TexDesc);

    TexDesc.DebugName := 'TCastleScreenEffects.Src';
    FRHIScreenTextureSrc := RHIDevice.CreateTexture(TexDesc);

    { Always allocate a depth attachment for ScreenEffectRTT (matching OpenGL's
      TGLRenderToTexture depth renderbuffer) so 3D viewports rendered into
      FRHIScreenTextureDest retain hardware depth testing even when NeedsDepth = False. }
    TexDesc := TRHITextureDesc.Create2D(AWidth, AHeight, pfD32_Float,
      [tuDepthStencil, tuSampled, tuTransferSrc], 1, 1);
    TexDesc.DebugName := 'TCastleScreenEffects.Depth';
    FRHIScreenTextureDepth := RHIDevice.CreateTexture(TexDesc);

    ScreenEffectRTT := TGLRenderToTexture.Create(AWidth, AHeight);
    ScreenEffectRTT.SetRHITexture(FRHIScreenTextureDest, FRHIScreenTextureDepth);
    ScreenEffectRTT.Buffer := tbColorAndDepth;
    ScreenEffectRTT.Stencil := False;
    ScreenEffectRTT.GLContextOpen;
  end;

  if FRHIScreenSampler = nil then
  begin
    FillChar(SampDesc, SizeOf(SampDesc), 0);
    SampDesc.MinFilter := sfLinear;
    SampDesc.MagFilter := sfLinear;
    SampDesc.MipFilter := sfNearest;
    SampDesc.AddressModeU := amClampToEdge;
    SampDesc.AddressModeV := amClampToEdge;
    SampDesc.AddressModeW := amClampToEdge;
    SampDesc.MaxAnisotropy := 1.0;
    SampDesc.MaxLod := 1.0;
    FRHIScreenSampler := RHIDevice.CreateSampler(SampDesc);
  end;

  if FRHIScreenQuadVBO = nil then
  begin
    ScreenPoint[0].Position := Vector2(-1, -1);
    ScreenPoint[0].TexCoord := Vector2(0, 0);
    ScreenPoint[1].Position := Vector2( 1, -1);
    ScreenPoint[1].TexCoord := Vector2(1, 0);
    ScreenPoint[2].Position := Vector2( 1,  1);
    ScreenPoint[2].TexCoord := Vector2(1, 1);
    ScreenPoint[3].Position := Vector2(-1,  1);
    ScreenPoint[3].TexCoord := Vector2(0, 1);

    BufDesc := TRHIBufferDesc.Create(SizeOf(ScreenPoint),
      [buVertex, buTransferDst], muGpuOnly, 'TCastleScreenEffects.QuadVBO');
    FRHIScreenQuadVBO := RHIDevice.CreateBuffer(BufDesc, @ScreenPoint[0]);
  end;

  if FRHIScreenQuadIBO = nil then
  begin
    BufDesc := TRHIBufferDesc.Create(SizeOf(QuadIndices),
      [buIndex, buTransferDst], muGpuOnly, 'TCastleScreenEffects.QuadIBO');
    FRHIScreenQuadIBO := RHIDevice.CreateBuffer(BufDesc, @QuadIndices[0]);
  end;

  if FRHIScreenLayoutColorOnly = nil then
  begin
    FillChar(BindingsColorOnly, SizeOf(BindingsColorOnly), 0);
    BindingsColorOnly[0].Binding := 0;
    BindingsColorOnly[0].DescriptorType := dtCombinedImageSampler;
    BindingsColorOnly[0].Count := 1;
    BindingsColorOnly[0].StageFlags := [ssFragment];
    FRHIScreenLayoutColorOnly := RHIDevice.CreateDescriptorSetLayout(BindingsColorOnly);
  end;

  if FRHIScreenLayoutColorAndDepth = nil then
  begin
    FillChar(BindingsColorDepth, SizeOf(BindingsColorDepth), 0);
    BindingsColorDepth[0].Binding := 0;
    BindingsColorDepth[0].DescriptorType := dtCombinedImageSampler;
    BindingsColorDepth[0].Count := 1;
    BindingsColorDepth[0].StageFlags := [ssFragment];
    BindingsColorDepth[1].Binding := 1;
    BindingsColorDepth[1].DescriptorType := dtCombinedImageSampler;
    BindingsColorDepth[1].Count := 1;
    BindingsColorDepth[1].StageFlags := [ssFragment];
    FRHIScreenLayoutColorAndDepth := RHIDevice.CreateDescriptorSetLayout(BindingsColorDepth);
  end;

  if FRHIScreenDefaultVS = nil then
    FRHIScreenDefaultVS := RHIDevice.CreateShaderModuleFromGLSL(ssVertex,
      'void main() { v_tex_coord = tex_coord; gl_Position = vec4(vertex, 0.0, 1.0); }', 'ScreenDefaultVS');
  if FRHIScreenDefaultFS = nil then
    FRHIScreenDefaultFS := RHIDevice.CreateShaderModuleFromGLSL(ssFragment,
      'void main() { castle_FragColor = texture(screen, tex_coord); }', 'ScreenDefaultFS');

  if (FRHIScreenDefaultPipelineColor = nil) or (FRHIScreenDefaultPipelineDepth = nil) then
  begin
    FillChar(PipeDesc, SizeOf(PipeDesc), 0);
    PipeDesc.VertexShader := FRHIScreenDefaultVS;
    PipeDesc.FragmentShader := FRHIScreenDefaultFS;
    PipeDesc.Topology := ptTriangleList;
    PipeDesc.Rasterizer := TRHIRasterizerStateDesc.DefaultCullBack;
    PipeDesc.Rasterizer.CullMode := cmNone;
    PipeDesc.DepthStencil := TRHIDepthStencilStateDesc.DefaultDisabled;
    PipeDesc.BlendState := TRHIBlendStateDesc.DefaultOpaque;
    SetLength(PipeDesc.VertexInput.Bindings, 1);
    PipeDesc.VertexInput.Bindings[0].Binding := 0;
    PipeDesc.VertexInput.Bindings[0].Stride := SizeOf(TScreenPoint);
    PipeDesc.VertexInput.Bindings[0].PerInstance := False;
    SetLength(PipeDesc.VertexInput.Attributes, 2);
    PipeDesc.VertexInput.Attributes[0].Location := 0;
    PipeDesc.VertexInput.Attributes[0].Binding := 0;
    PipeDesc.VertexInput.Attributes[0].Format := vafVector2;
    PipeDesc.VertexInput.Attributes[0].Offset := OffsetUInt(ScreenPoint[0].Position, ScreenPoint[0]);
    PipeDesc.VertexInput.Attributes[1].Location := 1;
    PipeDesc.VertexInput.Attributes[1].Binding := 0;
    PipeDesc.VertexInput.Attributes[1].Format := vafVector2;
    PipeDesc.VertexInput.Attributes[1].Offset := OffsetUInt(ScreenPoint[0].TexCoord, ScreenPoint[0]);
    SetLength(PipeDesc.ColorAttachmentFormats, 1);
    PipeDesc.ColorAttachmentFormats[0] := pfRGBA8_UNorm;
    PipeDesc.SampleCount := 1;
    SetLength(PipeDesc.DescriptorSetLayouts, 1);
    PipeDesc.PushConstantSize := SizeOf(TCastleScreenPushConstants);
    PipeDesc.PushConstantStages := [ssFragment];

    if FRHIScreenDefaultPipelineColor = nil then
    begin
      PipeDesc.DescriptorSetLayouts[0] := FRHIScreenLayoutColorOnly;
      PipeDesc.DebugName := 'TCastleScreenEffects.DefaultColorPSO';
      FRHIScreenDefaultPipelineColor := RHIDevice.GetOrCreatePipeline(PipeDesc);
    end;

    if FRHIScreenDefaultPipelineDepth = nil then
    begin
      PipeDesc.DescriptorSetLayouts[0] := FRHIScreenLayoutColorAndDepth;
      PipeDesc.DebugName := 'TCastleScreenEffects.DefaultDepthPSO';
      FRHIScreenDefaultPipelineDepth := RHIDevice.GetOrCreatePipeline(PipeDesc);
    end;
  end;
end;

function TCastleScreenEffects.ExecuteRHIScreenEffectPasses(
  const APassPipelines: array of TRHIPipeline;
  const AWidth, AHeight: Cardinal;
  const ANeedDepth: Boolean;
  const AFinalTargetTexture: TRHITexture;
  const ANearPlane, AFarPlane: Single): Cardinal;
var
  Cmd: TRHICommandList;
  PassCount, I: Integer;
  TargetTex: TRHITexture;
  Layout: TRHIDescriptorSetLayout;
  DescSet, UserDescSet: TRHIDescriptorSet;
  PushConsts: TCastleScreenPushConstants;
  FinalPassInfo: TRHIRenderingInfo;
  RenderAreaRect, FinalAreaRect: TRectangle;
  OuterPassActive: Boolean;
  PassPipeline: TRHIPipeline;
  AdaptedDesc: TRHIGraphicsPipelineDesc;
  PassColorFormat, PassDepthFormat: TRHIPixelFormat;
  PassSampleCount: Cardinal;
begin
  Result := 0;
  Cmd := ActiveCommandList;
  if (RHIDevice = nil) or (Cmd = nil) or (AWidth = 0) or (AHeight = 0) then
    Exit;

  PassCount := Length(APassPipelines);
  if PassCount <= 0 then
    Exit;

  EnsureRHIScreenResources(AWidth, AHeight, ANeedDepth);
  FreeRHIScreenDescriptorSets;
  SetLength(FRHIScreenDescriptorSets, PassCount);

  if ANeedDepth then
    Layout := FRHIScreenLayoutColorAndDepth
  else
    Layout := FRHIScreenLayoutColorOnly;

  RenderAreaRect := Rectangle(0, 0, AWidth, AHeight);

  for I := 0 to PassCount - 1 do
  begin
    { Ensure source textures are in rsShaderRead state before entering a render pass }
    if (FRHIScreenTextureSrc <> nil) and (FRHIScreenTextureSrc.CurrentState <> rsShaderRead) then
      Cmd.TransitionTexture(FRHIScreenTextureSrc, rsShaderRead);
    if ANeedDepth and (FRHIScreenTextureDepth <> nil) and
       (FRHIScreenTextureDepth.CurrentState <> rsShaderRead) then
      Cmd.TransitionTexture(FRHIScreenTextureDepth, rsShaderRead);

    OuterPassActive := False;
    PassColorFormat := pfRGBA8_UNorm;
    PassDepthFormat := pfUndefined;
    PassSampleCount := 1;

    if I < PassCount - 1 then
    begin
      { Intermediate ping-pong pass: render from FRHIScreenTextureSrc into FRHIScreenTextureDest }
      ScreenEffectRTT.SetRHITexture(FRHIScreenTextureDest, nil);
      ScreenEffectRTT.Buffer := tbColor;
      ScreenEffectRTT.RenderBegin;
    end else
    begin
      OuterPassActive := (AFinalTargetTexture = nil) and Cmd.IsRendering;
      if OuterPassActive then
      begin
        { Render directly into the already-active outer pass (resumed by ScreenEffectRTT.RenderEnd)
          at the control's sub-rectangle without clobbering or ending the outer pass. }
        if (RenderContext <> nil) and
           (RenderContext.Viewport.Width = Integer(AWidth)) and
           (RenderContext.Viewport.Height = Integer(AHeight)) then
          FinalAreaRect := RenderContext.EffectiveViewportRect
        else
          FinalAreaRect := RenderAreaRect;

        Cmd.SetViewport(FloatRectangle(FinalAreaRect), 0.0, 1.0);
        Cmd.SetScissor(FinalAreaRect);

        if Cmd.ActiveRenderingHasColor then
          PassColorFormat := Cmd.ActiveRenderingColorFormat
        else
          PassColorFormat := pfBGRA8_UNorm;
        if Cmd.ActiveRenderingHasDepth then
          PassDepthFormat := Cmd.ActiveRenderingDepthFormat
        else
          PassDepthFormat := pfUndefined;
        PassSampleCount := Max(1, Cmd.ActiveRenderingSampleCount);
      end else
      begin
        { Final pass: render from FRHIScreenTextureSrc into AFinalTargetTexture (or swapchain / dest) }
        TargetTex := AFinalTargetTexture;
        if TargetTex = nil then
          TargetTex := RHIDevice.GetSwapchainColorTexture;
        if TargetTex = nil then
          TargetTex := FRHIScreenTextureDest;

        PassColorFormat := TargetTex.Desc.Format;
        PassDepthFormat := pfUndefined;
        PassSampleCount := Max(1, TargetTex.Desc.SampleCount);

        Cmd.TransitionTexture(TargetTex, rsColorAttachment);
        FinalPassInfo := TRHIRenderingInfo.CreateColorDepth(
          RenderAreaRect, TargetTex, loClear, Vector4(0, 0, 0, 1), nil, loDontCare, 1.0);
        Cmd.BeginRendering(FinalPassInfo);
        Cmd.SetViewport(FloatRectangle(0, 0, AWidth, AHeight), 0.0, 1.0);
        Cmd.SetScissor(RenderAreaRect);
      end;
    end;

    PassPipeline := APassPipelines[I];
    if PassPipeline = nil then
    begin
      if ANeedDepth then
        PassPipeline := FRHIScreenDefaultPipelineDepth
      else
        PassPipeline := FRHIScreenDefaultPipelineColor;
    end;
    if (PassPipeline <> nil) and
       ((Length(PassPipeline.Desc.ColorAttachmentFormats) <> 1) or
        (PassPipeline.Desc.ColorAttachmentFormats[0] <> PassColorFormat) or
        (PassPipeline.Desc.DepthAttachmentFormat <> PassDepthFormat) or
        (Max(1, PassPipeline.Desc.SampleCount) <> PassSampleCount)) then
    begin
      AdaptedDesc := PassPipeline.Desc;
      SetLength(AdaptedDesc.ColorAttachmentFormats, 1);
      AdaptedDesc.ColorAttachmentFormats[0] := PassColorFormat;
      AdaptedDesc.DepthAttachmentFormat := PassDepthFormat;
      AdaptedDesc.SampleCount := PassSampleCount;
      PassPipeline := RHIDevice.GetOrCreatePipeline(AdaptedDesc);
    end;

    { Allocate Set 0 descriptor set directly from PassPipeline's own Set 0 layout
      when present (critical for MoltenVK Metal Argument Buffers so MVKDescriptorSet
      and MVKGraphicsPipeline share the exact same MVKDescriptorSetLayout instance). }
    if (PassPipeline <> nil) and
       (Length(PassPipeline.Desc.DescriptorSetLayouts) > 0) and
       (PassPipeline.Desc.DescriptorSetLayouts[0] <> nil) then
      DescSet := RHIDevice.CreateDescriptorSet(PassPipeline.Desc.DescriptorSetLayouts[0])
    else
      DescSet := RHIDevice.CreateDescriptorSet(Layout);
    FRHIScreenDescriptorSets[I] := DescSet;
    DescSet.BindTexture(0, FRHIScreenTextureSrc, FRHIScreenSampler);
    if ANeedDepth and (FRHIScreenTextureDepth <> nil) then
      DescSet.BindTexture(1, FRHIScreenTextureDepth, FRHIScreenSampler);
    DescSet.Update;

    if PassPipeline <> nil then
      Cmd.BindPipeline(PassPipeline);

    { Push 32-byte TCastleScreenPushConstants immediately after BindPipeline
      (before BindDescriptorSet, matching CastleBatch2D so MoltenVK encoder state
      never clobbers Set 0 buffer bindings when switching pipeline layouts). }
    PushConsts.ScreenSize := Vector2(AWidth, AHeight);
    PushConsts.ScreenPlanes := Vector2(ANearPlane, AFarPlane);
    PushConsts.ScreenCustom := Vector4(I, PassCount, 0, 0);
    if (PassPipeline <> nil) and (PassPipeline.Desc.PushConstantStages <> []) then
      Cmd.PushConstants(PassPipeline.Desc.PushConstantStages, 0,
        SizeOf(TCastleScreenPushConstants), PushConsts)
    else
      Cmd.PushConstants([ssFragment], 0, SizeOf(TCastleScreenPushConstants), PushConsts);

    Cmd.BindDescriptorSet(0, DescSet, []);
    UserDescSet := GetScreenEffectRHIUserDescriptorSet(I);
    if UserDescSet <> nil then
      Cmd.BindDescriptorSet(2, UserDescSet, []);

    { Bind fullscreen quad VBO/IBO and draw 2 triangles (6 indices) }
    Cmd.BindVertexBuffers(0, [FRHIScreenQuadVBO], [0]);
    Cmd.BindIndexBuffer(FRHIScreenQuadIBO, 0, itUInt16);
    Cmd.DrawIndexed(6, 1, 0, 0, 0);

    if I < PassCount - 1 then
    begin
      ScreenEffectRTT.RenderEnd;
      SwapRHIValues(FRHIScreenTextureDest, FRHIScreenTextureSrc);
    end else
    if not OuterPassActive then
    begin
      Cmd.EndRendering;
    end;

    Inc(FRHIScreenPassCount);
    Inc(FRHIScreenDrawCount);
    Inc(Result);
  end;
end;

procedure TCastleScreenEffects.Render;
var
  SR: TRectangle;

  { Calculate
    RenderScreenEffects (whether we actually do anything about screen effects),
    CurrentScreenEffectsCount,
    CurrentScreenEffectsNeedDepth.
  }
  procedure CheckScreenEffects;
  begin
    { Check do we have any screen effects to show.

      Save GetScreenEffectsCount (and later GetScreenEffectsNeedDepth) result,
      to not recalculate it,
      and make the rest of code stable --- this way we know
      CurrentScreenEffectsCount and later CurrentScreenEffectsNeedDepth
      values are constant, even if user modifies FScreenEffects in the middle. }
    CurrentScreenEffectsCount := GetScreenEffectsCount;
    RenderScreenEffects := CurrentScreenEffectsCount <> 0;
    if not RenderScreenEffects then Exit; // no need to do anything else

    SR := RenderRect.Round;
    if RHIDevice <> nil then
    begin
      RenderScreenEffects := (SR.Width > 0) and (SR.Height > 0);
      if not RenderScreenEffects then Exit;
      CurrentScreenEffectsNeedDepth := GetScreenEffectsNeedDepth;
      Exit;
    end;

    { all the checks below should pass on a modern GPU }
    RenderScreenEffects :=
      (not GLFeatures.EnableFixedFunction) and
      GLFeatures.UseMultiTexturing and
      { check IsTextureSized, to gracefully work (without screen effects)
        on old desktop OpenGL that does not support NPOT textures. }
      IsTextureSized(SR.Width, SR.Height, tsAny);
    if not RenderScreenEffects then Exit; // no need to do anything else

    { check the need to render depth buffer to a texture. }
    CurrentScreenEffectsNeedDepth := GetScreenEffectsNeedDepth;
    if CurrentScreenEffectsNeedDepth and not GLFeatures.TextureDepth then
    begin
      { We support only screen effects that do not require depth.
        TODO: It would be cleaner to still enable screen effects not using
        depth (and only them), instead of just disabling all screen effects. }
      RenderScreenEffects := false;
      Exit;
    end;
  end;

  { Create and setup new OpenGL texture for screen effects.
    Depends on ScreenEffectTextureWidth, ScreenEffectTextureHeight being set. }
  function CreateScreenEffectTexture(const Depth: boolean): TGLTexture;

    { Create new OpenGL texture for screen effect.
      Calls glTexImage2D or glTexImage2DMultisample
      (depending on multi-sampling requirements).

      - image contents are always unallocated (pixels = nil for glTexImage2D).
        For screen effects, we never need to load initial image contents,
        and we also do not have to care about pixel packing.
      - level for mipmaps is always 0
      - border is always 0
      - image target is ScreenEffectTextureTarget
      - size is ScreenEffectTextureWidth/Height }
    procedure TexImage2D(const InternalFormat: TGLint;
      const Format, AType: TGLenum);
    begin
      {$ifndef OpenGLES}
      if (GLFeatures.CurrentMultiSampling > 1) and GLFeatures.FBOMultiSampling then
        glTexImage2DMultisample(ScreenEffectTextureTarget,
          GLFeatures.CurrentMultiSampling, InternalFormat,
          ScreenEffectTextureWidth,
          ScreenEffectTextureHeight,
          { fixedsamplelocations = TRUE are necessary in case we use
            this with cbColor mode, where FBO will also have renderbuffer
            for depth (and maybe stencil). In this case,
            https://www.opengl.org/registry/specs/ARB/texture_multisample.txt
            says that

              if the attached images are a mix of
              renderbuffers and textures, the value of
              TEXTURE_FIXED_SAMPLE_LOCATIONS must be TRUE for all attached
              textures.

            which implies that this parameter must be true.
            See https://sourceforge.net/p/castle-engine/tickets/22/ . }
          GL_TRUE) else
      {$endif}
        glTexImage2D(ScreenEffectTextureTarget, 0, InternalFormat,
          ScreenEffectTextureWidth,
          ScreenEffectTextureHeight, 0, Format, AType, nil);
    end;

  begin
    Result := glCreateTexture();
    glBindTexture(ScreenEffectTextureTarget, Result);
    {$ifndef OpenGLES}
    { for multisample texture, these cannot be configured (OpenGL makes
      "invalid enumerant" error) }
    if ScreenEffectTextureTarget <> GL_TEXTURE_2D_MULTISAMPLE then
    {$endif}
    begin
      { TODO: NEAREST or LINEAR?
        Allow to config this at each screen effect,
        and optionally create another version? }
      SetTextureFilter(ScreenEffectTextureTarget, TextureFilter(minNearest, magNearest));
      glTexParameteri(ScreenEffectTextureTarget, GL_TEXTURE_WRAP_S, GLFeatures.CLAMP_TO_EDGE);
      glTexParameteri(ScreenEffectTextureTarget, GL_TEXTURE_WRAP_T, GLFeatures.CLAMP_TO_EDGE);
    end;
    if Depth then
    begin
      if GLFeatures.ShadowVolumesPossible and GLFeatures.PackedDepthStencil then
        TexImage2D(
          {$ifdef OpenGLES} GL_DEPTH24_STENCIL8 {$else} GL_DEPTH24_STENCIL8_EXT {$endif},
          {$ifdef OpenGLES} GL_DEPTH_STENCIL {$else} GL_DEPTH_STENCIL_EXT {$endif},
          {$ifdef OpenGLES} GL_UNSIGNED_INT_24_8 {$else} GL_UNSIGNED_INT_24_8_EXT {$endif})
      else
        TexImage2D(GLFeatures.DepthTextureInternalFormat, GL_DEPTH_COMPONENT,
          GLFeatures.DepthTextureImageType);
      //glTexParameteri(ScreenEffectTextureTarget, GL_TEXTURE_COMPARE_MODE_ARB, GL_NONE);
      //glTexParameteri(ScreenEffectTextureTarget, GL_DEPTH_TEXTURE_MODE_ARB, GL_LUMINANCE);
    end else
      TexImage2D({$ifdef OpenGLES} GL_RGB {$else} GL_RGB8 {$endif},
        GL_RGB, GL_UNSIGNED_BYTE);

    TextureMemoryProfiler.Allocate(Result, 'screen-contents', '', { TODO } 0, false,
      ScreenEffectTextureWidth, ScreenEffectTextureHeight, 1);
  end;

  procedure BeginRenderingToTexture;
  begin
    if RHIDevice <> nil then
    begin
      EnsureRHIScreenResources(SR.Width, SR.Height, CurrentScreenEffectsNeedDepth);
      ScreenEffectRTT.Buffer := tbColorAndDepth;
      ScreenEffectRTT.SetRHITexture(FRHIScreenTextureDest, FRHIScreenTextureDepth);
      if RenderContext <> nil then
      begin
        RenderContext.Viewport := SR;
        RenderContext.ViewportDelta := -SR.LeftBottom;
      end;
      ScreenEffectRTT.RenderBegin;
      Exit;
    end;

    { We need a temporary texture, for screen effect. }
    if (ScreenEffectTextureDest = GLObjectNone) or
       (ScreenEffectTextureSrc = GLObjectNone) or
       (CurrentScreenEffectsNeedDepth <> (ScreenEffectTextureDepth <> GLObjectNone)) or
       (ScreenEffectRTT = nil) or
       (ScreenEffectTextureWidth  <> SR.Width ) or
       (ScreenEffectTextureHeight <> SR.Height) then
    begin
      FreeTexture(ScreenEffectTextureDest);
      FreeTexture(ScreenEffectTextureSrc);
      FreeTexture(ScreenEffectTextureDepth);
      FreeAndNil(ScreenEffectRTT);

      {$ifndef OpenGLES}
      if (GLFeatures.CurrentMultiSampling > 1) and GLFeatures.FBOMultiSampling then
        ScreenEffectTextureTarget := GL_TEXTURE_2D_MULTISAMPLE else
      {$endif}
        ScreenEffectTextureTarget := GL_TEXTURE_2D;

      ScreenEffectTextureWidth  := SR.Width;
      ScreenEffectTextureHeight := SR.Height;
      { We use two textures: ScreenEffectTextureDest is the destination
        of framebuffer, ScreenEffectTextureSrc is the source to render.

        Although for some effects one texture (both src and dest) is enough.
        But when you have > 1 effect and one of the effects has non-local
        operations (they read color values that can be modified by operations
        of the same shader, so it's undefined (depends on how shaders are
        executed in parallel) which one is first) then the artifacts are
        visible. For example, use castle-model-viewer "Edge Detect" effect +
        any other effect. }
      ScreenEffectTextureDest := CreateScreenEffectTexture(false);
      ScreenEffectTextureSrc := CreateScreenEffectTexture(false);
      if CurrentScreenEffectsNeedDepth then
        ScreenEffectTextureDepth := CreateScreenEffectTexture(true);

      { create new TGLRenderToTexture (usually, framebuffer object) }
      ScreenEffectRTT := TGLRenderToTexture.Create(
        ScreenEffectTextureWidth, ScreenEffectTextureHeight);
      ScreenEffectRTT.SetTexture(ScreenEffectTextureDest, ScreenEffectTextureTarget);
      ScreenEffectRTT.CompleteTextureTarget := ScreenEffectTextureTarget;
      { use the same multi-sampling strategy as container }
      ScreenEffectRTT.MultiSampling := GLFeatures.CurrentMultiSampling;
      if CurrentScreenEffectsNeedDepth then
      begin
        ScreenEffectRTT.Buffer := tbColorAndDepth;
        ScreenEffectRTT.DepthTexture := ScreenEffectTextureDepth;
        ScreenEffectRTT.DepthTextureTarget := ScreenEffectTextureTarget;
      end else
        ScreenEffectRTT.Buffer := tbColor;
      ScreenEffectRTT.Stencil := GLFeatures.ShadowVolumesPossible;
      ScreenEffectRTT.GLContextOpen;

      WritelnLog('Screen effects', Format('Created texture for screen effects, with size %d x %d, with depth texture: %s',
        [ ScreenEffectTextureWidth,
          ScreenEffectTextureHeight,
          BoolToStr(CurrentScreenEffectsNeedDepth, true) ]));
    end;

    RenderContext.Viewport := SR;
    { Any subquequent changes to RenderContext.Viewport should by shifted,
      such that point SR.LeftBottom is actually at (0,0). }
    RenderContext.ViewportDelta := -SR.LeftBottom;
    { Note: the effective glViewport is now Rectangle(0, 0, SR.Width, SR.Height). }

    ScreenEffectRTT.RenderBegin;
    ScreenEffectRTT.SetTexture(ScreenEffectTextureDest, ScreenEffectTextureTarget);
  end;

begin
  inherited;

  { Whether we have to render screen effects or not (regular TCastleViewport items),
    break TDrawableImage batching as we no longer draw only images. }
  TDrawableImage.BatchingFlush;

  CheckScreenEffects;
  if RenderScreenEffects then
    BeginRenderingToTexture;
  RenderWithoutScreenEffects;
end;

procedure TCastleScreenEffects.RenderWithoutScreenEffects;
begin
end;

procedure TCastleScreenEffects.RenderOverChildren;
var
  SR: TRectangle;

  procedure RenderWithScreenEffectsCore;

    procedure RenderOneEffect(Shader: TGLSLProgram);
    var
      BoundTextureUnits: Cardinal;
      AttribVertex, AttribTexCoord: TGLSLAttribute;
    begin
      if ScreenPointVbo = GLObjectNone then
      begin
        { generate and fill ScreenPointVbo. It's contents are constant. }
        ScreenPointVbo := glCreateBuffer();
        ScreenPoint[0].TexCoord := Vector2(0, 0);
        ScreenPoint[0].Position := Vector2(-1, -1);
        ScreenPoint[1].TexCoord := Vector2(1, 0);
        ScreenPoint[1].Position := Vector2( 1, -1);
        ScreenPoint[2].TexCoord := Vector2(1, 1);
        ScreenPoint[2].Position := Vector2( 1,  1);
        ScreenPoint[3].TexCoord := Vector2(0, 1);
        ScreenPoint[3].Position := Vector2(-1,  1);
        RenderContext.BindBuffer[btArray] := ScreenPointVbo;
        glBufferData(GL_ARRAY_BUFFER, SizeOf(ScreenPoint), @(ScreenPoint[0]), GL_STATIC_DRAW);
      end;

      if ScreenPointVao = nil then
        ScreenPointVao := TVertexArrayObject.Create(nil);

      RenderContext.CurrentProgram := Shader;
      RenderContext.CurrentVao := ScreenPointVao;

      RenderContext.BindBuffer[btArray] := ScreenPointVbo;

      glActiveTexture(GL_TEXTURE0); // GLFeatures.UseMultiTexturing is already checked
      glBindTexture(ScreenEffectTextureTarget, ScreenEffectTextureSrc);
      BoundTextureUnits := 1;

      if CurrentScreenEffectsNeedDepth then
      begin
        glActiveTexture(GL_TEXTURE1);
        glBindTexture(ScreenEffectTextureTarget, ScreenEffectTextureDepth);
        Inc(BoundTextureUnits);
      end;

      Shader.Uniform('screen').SetValue(0);
      if CurrentScreenEffectsNeedDepth then
        Shader.Uniform('screen_depth').SetValue(1);
      Shader.Uniform('screen_width').SetValue(TGLint(ScreenEffectTextureWidth));
      Shader.Uniform('screen_height').SetValue(TGLint(ScreenEffectTextureHeight));

      { Note that we ignore SetupUniforms result --- if some texture
        could not be bound, it will be undefined for shader.
        I don't see anything much better to do now. }
      Shader.SetupUniforms(BoundTextureUnits);

      { Note that there's no need to worry about Rect.Left or Rect.Bottom,
        here or inside RenderWithScreenEffectsCore, because we're already within
        RenderContext.Viewport that takes care of this. }

      AttribVertex := Shader.Attribute('vertex');
      AttribVertex.EnableArrayVector2(ScreenPointVao, SizeOf(TScreenPoint),
        OffsetUInt(ScreenPoint[0].Position, ScreenPoint[0]));
      AttribTexCoord := Shader.Attribute('tex_coord');
      AttribTexCoord.EnableArrayVector2(ScreenPointVao, SizeOf(TScreenPoint),
        OffsetUInt(ScreenPoint[0].TexCoord, ScreenPoint[0]));

      glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

      AttribVertex.DisableArray;
      AttribTexCoord.DisableArray;
      RenderContext.BindBuffer[btArray] := GLObjectNone;
    end;

  var
    I: Integer;
  begin
    { Render all except the last screen effects: from texture
      (ScreenEffectTextureDest/Src) and to texture (using ScreenEffectRTT) }

    { The effective glViewport should be now Rectangle(0, 0, SR.Width, SR.Height).
      Although our "Render" implementation already set it
      (somewhat indirectly, using ViewportDelta),
      but something since then could have changed the Viewport
      (e.g. ControlRenderBegin done right before C.RenderOverChildren in CastleGLContainer).
      So set it as we need, again.
      And this time we don't need to deal with ViewportDelta, just zero it. }
    RenderContext.ViewportDelta := TVector2Integer.Zero;
    RenderContext.Viewport := Rectangle(0, 0, SR.Width, SR.Height);

    for I := 0 to CurrentScreenEffectsCount - 2 do
    begin
      ScreenEffectRTT.RenderBegin;
      ScreenEffectRTT.SetTexture(ScreenEffectTextureDest, ScreenEffectTextureTarget);
      RenderOneEffect(GetScreenEffect(I));
      ScreenEffectRTT.RenderEnd;

      SwapValues(ScreenEffectTextureDest, ScreenEffectTextureSrc);
    end;

    { the last effect gets a texture, and renders straight into screen }
    RenderContext.ViewportDelta := TVector2Integer.Zero;
    RenderContext.Viewport := RenderRect.Round;
    RenderOneEffect(GetScreenEffect(CurrentScreenEffectsCount - 1));
  end;

  procedure EndRenderingToTexture;
  var
    PassPipelines: array of TRHIPipeline;
    I: Integer;
  begin
    SR := RenderRect.Round;

    TDrawableImage.BatchingFlush;
    ScreenEffectRTT.RenderEnd;

    if RHIDevice <> nil then
    begin
      if RenderContext <> nil then
      begin
        RenderContext.ViewportDelta := TVector2Integer.Zero;
        RenderContext.Viewport := SR;
      end;
      SwapRHIValues(FRHIScreenTextureDest, FRHIScreenTextureSrc);
      SetLength(PassPipelines, CurrentScreenEffectsCount);
      for I := 0 to CurrentScreenEffectsCount - 1 do
        PassPipelines[I] := GetScreenEffectRHIPipeline(I);
      ExecuteRHIScreenEffectPasses(PassPipelines, SR.Width, SR.Height,
        CurrentScreenEffectsNeedDepth, nil,
        GetScreenEffectsNear, GetScreenEffectsFar);
      Exit;
    end;

    SwapValues(ScreenEffectTextureDest, ScreenEffectTextureSrc);

    OrthoProjection(FloatRectangle(0, 0, SR.Width, SR.Height));
    RenderWithScreenEffectsCore;
  end;

begin
  inherited;
  if RenderScreenEffects then
    EndRenderingToTexture;
end;

procedure TCastleScreenEffects.Update(const SecondsPassed: Single; var HandleInput: boolean);
var
  RemoveItem: TRemoveType;
begin
  inherited;
  if (ScreenEffectsScene <> nil) and ScreenEffectsEnable then
  begin
    RemoveItem := rtNone;
    ScreenEffectsScene.Update(SecondsPassed * ScreenEffectsTimeScale, RemoveItem);
    { We ignore RemoveItem --- ScreenEffectsScene cannot be removed.
      Also, this is always TCastleScene that should not change RemoveItem ever. }
  end;
end;

procedure TCastleScreenEffects.PrepareResources;
begin
  if ScreenEffectsScene <> nil then
  begin
    {$warnings off} // using deprecated, this should be internal
    ScreenEffectsScene.PrepareResources([prScreenEffects], FPrepareParams);
    {$warnings on}
  end;
end;

procedure TCastleScreenEffects.BeforeRender;
begin
  inherited;
  PrepareResources;
end;

procedure TCastleScreenEffects.GLContextClose;
begin
  FreeRHIScreenDescriptorSets;
  FreeAndNil(ScreenEffectRTT);
  FreeAndNil(FRHIScreenTextureDest);
  FreeAndNil(FRHIScreenTextureSrc);
  FreeAndNil(FRHIScreenTextureDepth);
  FreeAndNil(FRHIScreenSampler);
  FreeAndNil(FRHIScreenQuadVBO);
  FreeAndNil(FRHIScreenQuadIBO);
  FreeAndNil(FRHIScreenLayoutColorOnly);
  FreeAndNil(FRHIScreenLayoutColorAndDepth);
  FreeAndNil(FRHIScreenDefaultVS);
  FreeAndNil(FRHIScreenDefaultFS);
  FRHIScreenDefaultPipelineColor := nil;
  FRHIScreenDefaultPipelineDepth := nil;

  FreeTexture(ScreenEffectTextureDest);
  FreeTexture(ScreenEffectTextureSrc);
  FreeTexture(ScreenEffectTextureDepth);
  ScreenEffectTextureTarget := 0; //< clear, for safety
  FreeBuffer(ScreenPointVbo);
  FreeAndNil(ScreenPointVao);
  inherited;
end;

end.
