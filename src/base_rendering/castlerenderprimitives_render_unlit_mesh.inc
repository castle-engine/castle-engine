{%MainUnit castlerenderprimitives.pas}
{
  Copyright 2023-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TCastleRenderUnlitMesh class. }

{$ifdef read_interface}

type
  { Render a set of vertexes, with optional indexes.
    Can be used multiple times, to render the same vertexes many times.

    This is a deliberately simple and @italic(thin) wrapper over creating
    and drawing a bunch of vertexes on modern GPUs.
    E.g. on OpenGL(ES) it creates 1 VBO (vertex buffer object) for position data
    and optional 1 VBO for indexes (if SetIndexes is ever called).

    Make sure all usage is within the same rendering context.
    This has to be created and destroyed while the OpenGL(ES) context is active.

    This is suitable to render only trivial unlit (or invisible) meshes.
    @italic(This is not a full-featured mesh renderer.)
    Some deliberate limitations / hardcoded assumptions to keep this class simple:

    @unorderedList(
      @item(It doesn't define any per-vertex attributes other than vertex positions.)

      @item(If it uses indexes, it always uses 16-bit indexes, not 32-bit.
        So it works even with OpenGLES 2.0 (not 3.0) or WebGL 1.0.)

      @item(Always passes vectors as 4D (in homogeneous coordinates).
        4D coordinates are useful e.g. for shadow volume quads.)

      @item(It follows various current @link(RenderContext) state,
        like @link(TRenderContext.DepthTest),
        @link(TRenderContext.LineWidth) -- it doesn't control this state.)
    )

    For a full-featured mesh rendering, just use TCastleScene, and construct your
    mesh as TIndexedTriangleSetNode or TIndexedFaceSetNode, see
    https://castle-engine.io/viewport_and_scenes_from_code#_building_a_mesh_using_code .

    The things you really have to set before rendering are:

    @unorderedList(
      @item(Vertexes, using @link(SetVertexes).)
      @item(Matrix (combined projection * camera * model transformation) @link(ModelViewProjection).)
      @item(That's it, the rest has sensible defaults. You can just call @link(Render).)
    )

    For shading this uses a simple shader:

    @unorderedList(
      @item(If @link(UseColor) (default @true) then the shader writes @link(Color)
        to the color buffer. Effectively this makes unlit rendering.

        Note: This class does not set up blending if @link(Color) alpha is less than 1.
        If you want blending, set it yourself using @link(TRenderContext.BlendingEnable)
        and disable using @link(TRenderContext.BlendingDisable).
      )

      @item(If @link(UseColor) is @false then the shader writes undefined
        value to the color buffer.

        But it still writes proper values to depth and stencil.

        This is esp. useful when you render things with color buffer disabled
        (using @code(RenderContext.ColorChannels := [])).
        The rendering may still be useful, e.g. to fill depth buffer (for shadow maps),
        stencil buffer, or define shape for GPU occlusion query.

        Note: This class does not configure @code(RenderContext.ColorChannels).
        It is up to you to decide how to hide from user the undefined effect
        this has on the color buffer.
      )
    )

    Example usage:

    @longCode(#
      SavedDepthTest := RenderContext.DepthTest;
      RenderContext.DepthTest := true;

      Mesh := TCastleRenderUnlitMesh.Create(true);
      try
        Mesh.Color := Yellow;
        Mesh.ModelViewProjection := RenderContext.ProjectionMatrix * RenderingCamera.CurrentMatrix;
        Mesh.SetVertexes([
          Vector4(0, 0, 0, 1),
          Vector4(10, 0, 0, 1),
          Vector4(0, 10, 0, 1)
        ], false);
        Mesh.Render(pmTriangles);
      finally FreeAndNil(Mesh) end;

      RenderContext.DepthTest := SavedDepthTest;
    #)
  }
  TCastleRenderUnlitMesh = class
  strict private
    type
      TProgram = record
        Shader: TGLSLProgram;
        UniformModelViewProjectionMatrix, UniformColor: TGLSLUniform;
        AttributeVertex: TGLSLAttribute;
      end;

    class var
      Programs: array [ { UseColor? } Boolean ] of TProgram;

    var
      FUseColor: Boolean;
      FColor: TCastleColor;
      VboVertex, VboIndex: TGLBuffer;
      VertexAllocatedBytes, IndexAllocatedBytes,
        VertexAllocatedItems, IndexAllocatedItems: Cardinal;
      FixedFunctionVertexes: TVector4List;
      FixedFunctionIndexes: TUInt16List;
      Vao: TVertexArrayObject;
      FRHIVertexBuffer: TRHIBuffer;
      FRHIIndexBuffer: TRHIBuffer;
      FRHIVertexWriteOffset: PtrUInt;
      FRHIVertexCurrentOffset: PtrUInt;
      FRHIVertexDrawnSinceUpload: Boolean;
      FRHILastFrameIndex: Cardinal;
      FRHILastFramesPresented: Cardinal;
      FRHILastCmdList: TRHICommandList;
      FRHIVertShader: TRHIShaderModule;
      FRHIFragShader: TRHIShaderModule;
      FRHIPipeline: TRHIPipeline;
      FRHIDrawCallCount: Cardinal;
      FRHILastVertexCount: Cardinal;
      FRHILastIndexCount: Cardinal;
      FRHILastTopology: TRHIPrimitiveTopology;

    class procedure PrepareShader(const AUseColor: Boolean);
    procedure PrepareRHIPipeline(const ATopology: TRHIPrimitiveTopology);
    procedure SetUseColor(const Value: Boolean);
  private
    class procedure StaticGLContextClose;
  public
    { By default all zeros, you must set this before any rendering. }
    ModelViewProjection: TMatrix4;
    CustomDepthStencilState: TRHIDepthStencilStateDesc;
    UseCustomDepthStencilState: Boolean;

    constructor Create(const AUseColor: Boolean = true);
    destructor Destroy; override;

    { Define indexes for all future rendering.
      Note: Passing indexes that do not exist in vertexes you define in SetVertexes
      may result in crashes, neither CGE nor rendering API necessarily check it. }
    procedure SetIndexes(const Indexes: array of UInt16); overload;
    procedure SetIndexes(const Indexes: PUInt16; const IndexesCount: Cardinal); overload;

    { Set vertexes (points) to render.

      UsageDynamic is an optimization hint, pass @true if you plan to change this VBO often,
      e.g. always or almost-always before each @link(Render) you will send new vertexes
      with @link(SetVertexes). Pass @false if this VBO stays constant for a long time,
      and will be drawn many times (so you call @link(SetVertexes) once,
      then many times @link(Render)). }
    procedure SetVertexes(const Vertexes: array of TVector4;
      const UsageDynamic: Boolean); overload;
    procedure SetVertexes(const Vertexes: TVector4List;
      const UsageDynamic: Boolean); overload;
    procedure SetVertexes(const Vertexes: PVector4; const VertexesCount: Cardinal;
      const UsageDynamic: Boolean); overload;

    { Should we render with @link(Color) (if @false, output color is undefined). }
    property UseColor: Boolean read FUseColor write SetUseColor;

    { Color used for unlit rendering, if only UseColor is @true. Opaque white by default. }
    property Color: TCastleColor read FColor write FColor;

    procedure Render(const Mode: TPrimitiveMode);

    { RHI resources and diagnostic counters when rendering with an active @link(RHIDevice).
      @exclude }
    property RHIVertexBuffer: TRHIBuffer read FRHIVertexBuffer;
    property RHIIndexBuffer: TRHIBuffer read FRHIIndexBuffer;
    property RHIVertShader: TRHIShaderModule read FRHIVertShader;
    property RHIFragShader: TRHIShaderModule read FRHIFragShader;
    property RHIPipeline: TRHIPipeline read FRHIPipeline;
    property RHIDrawCallCount: Cardinal read FRHIDrawCallCount;
    property RHILastVertexCount: Cardinal read FRHILastVertexCount;
    property RHILastIndexCount: Cardinal read FRHILastIndexCount;
    property RHILastTopology: TRHIPrimitiveTopology read FRHILastTopology;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TODO: Constants duplicated from castleglutils_draw_primitive_2d.inc,
  maybe one day we'll join DrawPrimitive2D and TCastleRenderUnlitMesh into one unit
  (CastleRenderPrimitives?) which would make it natural to define this in one place. }

const
  PrimitiveModeToGL: array [TPrimitiveMode] of TGLEnum = (
    GL_POINTS,
    GL_LINE_STRIP,
    GL_LINE_LOOP,
    GL_LINES,
    GL_TRIANGLE_STRIP,
    GL_TRIANGLE_FAN,
    GL_TRIANGLES
  );

constructor TCastleRenderUnlitMesh.Create(const AUseColor: Boolean);
begin
  inherited Create;
  FUseColor := AUseColor;
  FColor := White;
  if (RHIDevice = nil) and (GLFeatures <> nil) then
  begin
    PrepareShader(AUseColor);
    Vao := TVertexArrayObject.Create(nil);
  end;
end;

destructor TCastleRenderUnlitMesh.Destroy;
begin
  FreeAndNil(Vao);
  FreeBuffer(VboIndex);
  FreeBuffer(VboVertex);
  FreeAndNil(FixedFunctionVertexes);
  FreeAndNil(FixedFunctionIndexes);
  FreeAndNil(FRHIVertexBuffer);
  FreeAndNil(FRHIIndexBuffer);
  FreeAndNil(FRHIVertShader);
  FreeAndNil(FRHIFragShader);
  FRHIPipeline := nil; // owned by RHIDevice pipeline cache
  inherited;
end;

procedure TCastleRenderUnlitMesh.PrepareRHIPipeline(const ATopology: TRHIPrimitiveTopology);
const
  UnlitMeshVertGLSL =
    '#version 450' + LineEnding +
    'layout(location = 0) in vec4 castle_Vertex;' + LineEnding +
    'layout(push_constant) uniform PushConstants {' + LineEnding +
    '  mat4 u_MVP;' + LineEnding +
    '  vec4 u_Color;' + LineEnding +
    '} pc;' + LineEnding +
    'void main() {' + LineEnding +
    '  gl_Position = pc.u_MVP * castle_Vertex;' + LineEnding +
    '  gl_Position.z = (gl_Position.z + gl_Position.w) * 0.5;' + LineEnding +
    '}';
  UnlitMeshFragGLSL =
    '#version 450' + LineEnding +
    'layout(push_constant) uniform PushConstants {' + LineEnding +
    '  mat4 u_MVP;' + LineEnding +
    '  vec4 u_Color;' + LineEnding +
    '} pc;' + LineEnding +
    'layout(location = 0) out vec4 out_Color;' + LineEnding +
    'void main() {' + LineEnding +
    '  out_Color = pc.u_Color;' + LineEnding +
    '}';
var
  PipeDesc: TRHIGraphicsPipelineDesc;
  DepthTestActive, ColorWriteActive: Boolean;
begin
  if RHIDevice = nil then
    Exit;

  if FRHIVertShader = nil then
    FRHIVertShader := RHIDevice.CreateShaderModuleFromGLSL(
      ssVertex, UnlitMeshVertGLSL, 'UnlitMesh_VS');
  if FRHIFragShader = nil then
    FRHIFragShader := RHIDevice.CreateShaderModuleFromGLSL(
      ssFragment, UnlitMeshFragGLSL, 'UnlitMesh_FS');

  if RenderContext <> nil then
    DepthTestActive := RenderContext.DepthTest
  else
    DepthTestActive := True;
  ColorWriteActive := FUseColor and
    ((RenderContext = nil) or (RenderContext.ColorChannels <> []));

  FillChar(PipeDesc, SizeOf(PipeDesc), 0);
  PipeDesc.VertexShader := FRHIVertShader;
  PipeDesc.FragmentShader := FRHIFragShader;
  PipeDesc.Topology := ATopology;
  SetLength(PipeDesc.VertexInput.Bindings, 1);
  PipeDesc.VertexInput.Bindings[0].Binding := 0;
  PipeDesc.VertexInput.Bindings[0].Stride := SizeOf(TVector4);
  PipeDesc.VertexInput.Bindings[0].PerInstance := False;
  SetLength(PipeDesc.VertexInput.Attributes, 1);
  PipeDesc.VertexInput.Attributes[0].Location := 0;
  PipeDesc.VertexInput.Attributes[0].Binding := 0;
  PipeDesc.VertexInput.Attributes[0].Format := vafVector4;
  PipeDesc.VertexInput.Attributes[0].Offset := 0;
  if (RenderContext <> nil) and RenderContext.CullFace then
  begin
    PipeDesc.Rasterizer.CullMode := cmBack;
    if RenderContext.FrontFaceCcw then
      PipeDesc.Rasterizer.FrontFace := ffCounterClockwise
    else
      PipeDesc.Rasterizer.FrontFace := ffClockwise;
  end else
  begin
    PipeDesc.Rasterizer.CullMode := cmNone;
    PipeDesc.Rasterizer.FrontFace := ffCounterClockwise;
  end;
  PipeDesc.Rasterizer.PolygonMode := pmFill;
  if UseCustomDepthStencilState then
    PipeDesc.DepthStencil := CustomDepthStencilState
  else
  begin
    if DepthTestActive then
    begin
      PipeDesc.DepthStencil := TRHIDepthStencilStateDesc.DefaultDepthTestWrite;
      PipeDesc.DepthStencil.DepthWriteEnable := FUseColor and
        ((RenderContext = nil) or RenderContext.DepthBufferUpdate);
    end else
      PipeDesc.DepthStencil := TRHIDepthStencilStateDesc.DefaultDisabled;
    if RenderContext <> nil then
    begin
      case RenderContext.DepthFunc of
        dfNever:        PipeDesc.DepthStencil.DepthCompareOp := coNever;
        dfLess:         PipeDesc.DepthStencil.DepthCompareOp := coLess;
        dfEqual:        PipeDesc.DepthStencil.DepthCompareOp := coEqual;
        dfLessEqual:    PipeDesc.DepthStencil.DepthCompareOp := coLessOrEqual;
        dfGreater:      PipeDesc.DepthStencil.DepthCompareOp := coGreater;
        dfNotEqual:     PipeDesc.DepthStencil.DepthCompareOp := coNotEqual;
        dfGreaterEqual: PipeDesc.DepthStencil.DepthCompareOp := coGreaterOrEqual;
        dfAlways:       PipeDesc.DepthStencil.DepthCompareOp := coAlways;
      end;
    end;
  end;
  if ColorWriteActive then
  begin
    if (RenderContext <> nil) and RenderContext.BlendingEnabled then
    begin
      PipeDesc.BlendState := TRHIBlendStateDesc.DefaultAlphaBlend;
      PipeDesc.BlendState.SrcColorBlendFactor := TRenderContext.MapBlendSourceFactorToRHI(RenderContext.BlendingSourceFactor);
      PipeDesc.BlendState.DstColorBlendFactor := TRenderContext.MapBlendDestFactorToRHI(RenderContext.BlendingDestinationFactor);
      PipeDesc.BlendState.SrcAlphaBlendFactor := PipeDesc.BlendState.SrcColorBlendFactor;
      PipeDesc.BlendState.DstAlphaBlendFactor := PipeDesc.BlendState.DstColorBlendFactor;
    end else
      PipeDesc.BlendState := TRHIBlendStateDesc.DefaultOpaque;
  end else
    PipeDesc.BlendState := TRHIBlendStateDesc.DefaultColorMaskNone;
  PipeDesc.PushConstantSize := SizeOf(TMatrix4) + SizeOf(TVector4);
  PipeDesc.PushConstantStages := [ssVertex, ssFragment];
  if (ActiveCommandList <> nil) and ActiveCommandList.IsRendering then
  begin
    if ActiveCommandList.ActiveRenderingHasColor then
    begin
      SetLength(PipeDesc.ColorAttachmentFormats, 1);
      PipeDesc.ColorAttachmentFormats[0] := ActiveCommandList.ActiveRenderingColorFormat;
    end else
      SetLength(PipeDesc.ColorAttachmentFormats, 0);
    if ActiveCommandList.ActiveRenderingHasDepth then
      PipeDesc.DepthAttachmentFormat := ActiveCommandList.ActiveRenderingDepthFormat
    else
      PipeDesc.DepthAttachmentFormat := pfUndefined;
    if ActiveCommandList.ActiveRenderingSampleCount > 0 then
      PipeDesc.SampleCount := ActiveCommandList.ActiveRenderingSampleCount
    else
      PipeDesc.SampleCount := 1;
  end else
  begin
    SetLength(PipeDesc.ColorAttachmentFormats, 1);
    PipeDesc.ColorAttachmentFormats[0] := pfBGRA8_UNorm;
    PipeDesc.DepthAttachmentFormat := pfD24_UNorm_S8_UInt;
    PipeDesc.SampleCount := 1;
  end;
  FRHIPipeline := RHIDevice.GetOrCreatePipeline(PipeDesc);
end;

class procedure TCastleRenderUnlitMesh.PrepareShader(const AUseColor: Boolean);
const
  SimplestVS = {$I simplest.vs.inc};
  SimplestFS = {$I simplest.fs.inc};
  SimplestUnlitFS = {$I simplest_unlit.fs.inc};
var
  Shader: TGLSLProgram;
begin
  if (RHIDevice <> nil) or (GLFeatures = nil) or GLFeatures.EnableFixedFunction then
    Exit;
  Assert(GLFeatures.Shaders); // EnableFixedFunction and Shaders are now synchronized

  // already prepared
  if Programs[AUseColor].Shader <> nil then
    Exit;

  Shader := TGLSLProgram.Create;
  Shader.Name := 'TCastleRenderUnlitMesh:UseColor=' + BoolToStr(AUseColor, true);
  Shader.AttachShader(stVertex, SimplestVS);

  if AUseColor then
  begin
    Shader.AttachShader(stFragment, SimplestUnlitFS);
  end else
  begin
    { On desktop OpenGL, fragment shader doesn't need to exist now.
      https://www.khronos.org/opengl/wiki/Fragment_Shader#Optional
      It still has to exist with OpenGLES.
      Also macOS requires it always (in "core" profile), otherwise rendering shadow volumes
      makes GL error at glDrawArrays in this unit. }
    {$if defined(OpenGLES) or defined(DARWIN)}
    Shader.AttachShader(stFragment, SimplestFS);
    {$endif}
  end;

  Shader.Link;

  Programs[AUseColor].Shader := Shader;
  Programs[AUseColor].AttributeVertex := Shader.Attribute('castle_Vertex');
  Programs[AUseColor].UniformModelViewProjectionMatrix := Shader.Uniform('castle_ModelViewProjectionMatrix');
  if AUseColor then
    Programs[AUseColor].UniformColor := Shader.Uniform('color');
end;

class procedure TCastleRenderUnlitMesh.StaticGLContextClose;
begin
  FreeAndNil(Programs[false].Shader);
  FreeAndNil(Programs[true].Shader);
end;

procedure TCastleRenderUnlitMesh.Render(const Mode: TPrimitiveMode);
type
  TUnlitMeshPushConstants = packed record
    ModelViewProjection: TMatrix4;
    Color: TVector4;
  end;
const
  PrimitiveModeToRHI: array [TPrimitiveMode] of TRHIPrimitiveTopology = (
    ptPointList,
    ptLineStrip,
    ptLineStrip,
    ptLineList,
    ptTriangleStrip,
    ptTriangleList,
    ptTriangleList
  );

  { Call glDrawElements or glDrawArrays.
    IndexPtr is used only when using indexes, and should be
    relative to VBO memory (if VBO is used). }
  procedure RenderDrawCall(const IndexPtr: Pointer);
  begin
    if IndexAllocatedItems <> 0 then
    begin
      glDrawElements(PrimitiveModeToGL[Mode], IndexAllocatedItems, GL_UNSIGNED_SHORT, IndexPtr);
    end else
      glDrawArrays(PrimitiveModeToGL[Mode], 0, VertexAllocatedItems);
  end;

  { Render using modern pipeline. }
  procedure RenderShaders;
  begin
    if VboVertex = GLObjectNone then
      raise EInternalError.Create('TCastleRenderUnlitMesh has no vertexes defined to render');

    RenderContext.CurrentProgram := Programs[UseColor].Shader;
    RenderContext.CurrentVao := Vao;

    RenderContext.BindBuffer[btArray] := VboVertex;
    // do this even if VboIndex = 0, to set this to 0
    RenderContext.BindBuffer[btElementArray] := VboIndex;

    if FUseColor then
      Programs[UseColor].UniformColor.SetValue(Color);
    Programs[UseColor].UniformModelViewProjectionMatrix.SetValue(ModelViewProjection);
    Programs[UseColor].AttributeVertex.EnableArrayVector4(Vao, SizeOf(TVector4), 0);

    RenderDrawCall(nil);

    Programs[UseColor].AttributeVertex.DisableArray;

    RenderContext.BindBuffer[btArray] := GLObjectNone;
    RenderContext.BindBuffer[btElementArray] := GLObjectNone;
  end;

  {$ifndef OpenGLES}
  { Render using ancient fixed-function pipeline. }
  procedure RenderFixedFunction;
  var
    SavedFixedFunctionLighting: Boolean;
    IndexPtr: Pointer;
  begin
    if FUseColor then
      glColorv(Color);

    { We load as GL_MODELVIEW the complete matrix projection * modelview.
      This keeps our API simple (we just need 1 matrix, ModelViewProjection, already multiplied)
      and is good enough for rendering unlit stuff.
      So for GL_PROJECTION, we make sure to load identity. }

    glMatrixMode(GL_PROJECTION);
    glPushMatrix;
    glLoadIdentity;
    glMatrixMode(GL_MODELVIEW);
    glLoadMatrix(ModelViewProjection);

    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(4, GL_FLOAT, SizeOf(TVector4), FixedFunctionVertexes.L);

    SavedFixedFunctionLighting := RenderContext.FixedFunctionLighting;
    RenderContext.FixedFunctionLighting := false;

    if FixedFunctionIndexes = nil then
      IndexPtr := nil
    else
      IndexPtr := FixedFunctionIndexes.L;

    RenderDrawCall(IndexPtr);

    { Restore GL_PROJECTION matrix value, other drawing code may depend on it.
      Don't restore GL_MODELVIEW, no need, other drawing code has to be prepared
      to always initialize GL_MODELVIEW. }
    glMatrixMode(GL_PROJECTION);
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);

    glDisableClientState(GL_VERTEX_ARRAY);

    RenderContext.FixedFunctionLighting := SavedFixedFunctionLighting;
  end;
  {$endif}

var
  Topology: TRHIPrimitiveTopology;
  PushConsts: TUnlitMeshPushConstants;
begin
  if RHIDevice <> nil then
  begin
    Topology := PrimitiveModeToRHI[Mode];
    PrepareRHIPipeline(Topology);
    FRHILastTopology := Topology;
    FRHILastVertexCount := VertexAllocatedItems;
    FRHILastIndexCount := IndexAllocatedItems;
    Inc(FRHIDrawCallCount);

    if (ActiveCommandList <> nil) and (FRHIVertexBuffer <> nil) and (VertexAllocatedItems > 0) then
    begin
      ActiveCommandList.BindPipeline(FRHIPipeline);
      PushConsts.ModelViewProjection := ModelViewProjection;
      PushConsts.Color := FColor;
      ActiveCommandList.PushConstants([ssVertex, ssFragment], 0, SizeOf(PushConsts), PushConsts);
      ActiveCommandList.BindVertexBuffers(0, [FRHIVertexBuffer], [FRHIVertexCurrentOffset]);
      if (IndexAllocatedItems <> 0) and (FRHIIndexBuffer <> nil) then
      begin
        ActiveCommandList.BindIndexBuffer(FRHIIndexBuffer, 0, itUInt16);
        ActiveCommandList.DrawIndexed(IndexAllocatedItems, 1, 0, 0, 0);
      end else
        ActiveCommandList.Draw(VertexAllocatedItems, 1, 0, 0);
      FRHIVertexDrawnSinceUpload := True;
    end;
    Exit;
  end;

  {$ifndef OpenGLES}
  if GLFeatures.EnableFixedFunction then
    RenderFixedFunction
  else
  {$endif}
    RenderShaders;
end;

procedure TCastleRenderUnlitMesh.SetIndexes(const Indexes: array of UInt16);
begin
  { Note: This assumes that Indexes is actually a "packed array of UInt16",
    but it cannot be declared as such in Delphi.
    We verify it by test TTestCompiler.TestPackedOpenArray in CGE tests/ projects. }
  SetIndexes(@Indexes[0], High(Indexes) + 1);
end;

procedure TCastleRenderUnlitMesh.SetIndexes(const Indexes: PUInt16; const IndexesCount: Cardinal);
var
  NewVbo: Boolean;
  NewSize: Cardinal;
  BufDesc: TRHIBufferDesc;
begin
  if RHIDevice <> nil then
  begin
    NewSize := IndexesCount * SizeOf(UInt16);
    if NewSize > 0 then
    begin
      if (FRHIIndexBuffer = nil) or (FRHIIndexBuffer.Desc.Size < NewSize) then
      begin
        FreeAndNil(FRHIIndexBuffer);
        BufDesc := TRHIBufferDesc.Create(NewSize, [buIndex, buTransferDst], muGpuOnly);
        FRHIIndexBuffer := RHIDevice.CreateBuffer(BufDesc, Indexes);
      end else
      if Indexes <> nil then
        FRHIIndexBuffer.UploadData(Indexes^, NewSize, 0);
    end;
    IndexAllocatedItems := IndexesCount;
    IndexAllocatedBytes := NewSize;
    Exit;
  end;

  if not GLFeatures.EnableFixedFunction then
  begin
    NewVbo := VboIndex = GLObjectNone;
    if NewVbo then
      VboIndex := glCreateBuffer();

    RenderContext.BindBuffer[btElementArray] := VboIndex;

    NewSize := IndexesCount * SizeOf(UInt16);
    if NewVbo or (NewSize <> IndexAllocatedBytes) then
    begin
      // always GL_STATIC_DRAW for indexes, we assume we will not modify indexes very often
      glBufferData(GL_ELEMENT_ARRAY_BUFFER, NewSize, Indexes, GL_STATIC_DRAW);
      IndexAllocatedBytes := NewSize;
      IndexAllocatedItems := IndexesCount;
    end else
      glBufferSubData(GL_ELEMENT_ARRAY_BUFFER, 0, NewSize, Indexes);
  end else
  begin
    IndexAllocatedItems := IndexesCount;
    IndexAllocatedBytes := SizeOf(UInt16) * IndexesCount;
    if FixedFunctionIndexes = nil then
      FixedFunctionIndexes := TUInt16List.Create;
    FixedFunctionIndexes.Count := IndexesCount;
    Move(Indexes^, FixedFunctionIndexes.L^, IndexAllocatedBytes);
  end;
end;

procedure TCastleRenderUnlitMesh.SetVertexes(const Vertexes: PVector4; const VertexesCount: Cardinal;
  const UsageDynamic: Boolean);
var
  NewVbo: Boolean;
  NewSize: Cardinal;
  NewCapacity: PtrUInt;
  DataUsage: TGLEnum;
  BufDesc: TRHIBufferDesc;
begin
  if RHIDevice <> nil then
  begin
    NewSize := VertexesCount * SizeOf(TVector4);
    if NewSize > 0 then
    begin
      if (FRHILastFrameIndex <> RHIDevice.FrameIndex) or
         (FRHILastFramesPresented <> RHIDevice.TotalFramesStarted) or
         (FRHILastCmdList <> ActiveCommandList) then
      begin
        FRHIVertexWriteOffset := 0;
        FRHIVertexCurrentOffset := 0;
        FRHIVertexDrawnSinceUpload := False;
        FRHILastFrameIndex := RHIDevice.FrameIndex;
        FRHILastFramesPresented := RHIDevice.TotalFramesStarted;
        FRHILastCmdList := ActiveCommandList;
      end;

      if UsageDynamic and FRHIVertexDrawnSinceUpload then
        FRHIVertexCurrentOffset := FRHIVertexWriteOffset
      else
      if not UsageDynamic then
        FRHIVertexCurrentOffset := 0;

      if (FRHIVertexBuffer = nil) or
         (FRHIVertexBuffer.Desc.Size < FRHIVertexCurrentOffset + PtrUInt(NewSize)) then
      begin
        FreeAndNil(FRHIVertexBuffer);
        if FRHIVertexCurrentOffset > 0 then
          NewCapacity := 4096 + (FRHIVertexCurrentOffset + PtrUInt(NewSize)) * 2
        else
          NewCapacity := NewSize;
        if UsageDynamic then
          BufDesc := TRHIBufferDesc.Create(NewCapacity, [buVertex, buTransferDst], muCpuToGpu)
        else
          BufDesc := TRHIBufferDesc.Create(NewCapacity, [buVertex, buTransferDst], muGpuOnly);
        FRHIVertexCurrentOffset := 0;
        if NewCapacity = NewSize then
          FRHIVertexBuffer := RHIDevice.CreateBuffer(BufDesc, Vertexes)
        else
        begin
          FRHIVertexBuffer := RHIDevice.CreateBuffer(BufDesc, nil);
          if Vertexes <> nil then
            FRHIVertexBuffer.UploadData(Vertexes^, NewSize, 0);
        end;
      end else
      if Vertexes <> nil then
        FRHIVertexBuffer.UploadData(Vertexes^, NewSize, FRHIVertexCurrentOffset);

      FRHIVertexWriteOffset := (FRHIVertexCurrentOffset + PtrUInt(NewSize) + 15) and not PtrUInt(15);
      FRHIVertexDrawnSinceUpload := False;
    end;
    VertexAllocatedItems := VertexesCount;
    VertexAllocatedBytes := NewSize;
    Exit;
  end;

  if not GLFeatures.EnableFixedFunction then
  begin
    NewVbo := VboVertex = GLObjectNone;
    if NewVbo then
      VboVertex := glCreateBuffer();

    RenderContext.BindBuffer[btArray] := VboVertex;

    NewSize := VertexesCount * SizeOf(TVector4);
    if NewVbo or (NewSize <> VertexAllocatedBytes) then
    begin
      if UsageDynamic then
          { GL_STREAM_DRAW is most suitable if we will modify this every frame,
            according to
            https://www.khronos.org/opengl/wiki/Buffer_Object
            https://computergraphics.stackexchange.com/questions/5712/gl-static-draw-vs-gl-dynamic-draw-vs-gl-stream-draw-does-it-matter
          }
          DataUsage := GL_STREAM_DRAW
        else
          DataUsage := GL_STATIC_DRAW;
        glBufferData(GL_ARRAY_BUFFER, NewSize, Vertexes, DataUsage);
      VertexAllocatedBytes := NewSize;
      VertexAllocatedItems := VertexesCount;
    end else
      glBufferSubData(GL_ARRAY_BUFFER, 0, NewSize, Vertexes);
  end else
  begin
    VertexAllocatedItems := VertexesCount;
    VertexAllocatedBytes := SizeOf(TVector4) * VertexesCount;
    if FixedFunctionVertexes = nil then
      FixedFunctionVertexes := TVector4List.Create;
    FixedFunctionVertexes.Count := VertexesCount;
    Move(Vertexes^, FixedFunctionVertexes.L^, VertexAllocatedBytes);
  end;
end;

procedure TCastleRenderUnlitMesh.SetVertexes(const Vertexes: array of TVector4;
  const UsageDynamic: Boolean);
begin
  { Note: This assumes that Vertexes is actually a "packed array of TVector4",
    but it cannot be declared as such in Delphi.
    We verify it by test TTestCompiler.TestPackedOpenArray in CGE tests/ projects. }
  SetVertexes(@Vertexes[0], High(Vertexes) + 1, UsageDynamic);
end;

procedure TCastleRenderUnlitMesh.SetVertexes(const Vertexes: TVector4List;
  const UsageDynamic: Boolean);
begin
  SetVertexes(PVector4(Vertexes.L), Vertexes.Count, UsageDynamic);
end;

procedure TCastleRenderUnlitMesh.SetUseColor(const Value: Boolean);
begin
  if FUseColor <> Value then
  begin
    FUseColor := Value;
    if (RHIDevice = nil) and (GLFeatures <> nil) then
      PrepareShader(Value);
  end;
end;

{$endif read_implementation}
