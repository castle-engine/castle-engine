{%MainUnit castlerenderprimitives.pas}
{
  Copyright 2023-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
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
          Vector3(0, 0, 0, 1),
          Vector3(10, 0, 0, 1),
          Vector3(0, 10, 0, 1)
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
      VboVertex, VboIndex: TGLuint;
      VertexAllocatedBytes, IndexAllocatedBytes,
        VertexAllocatedItems, IndexAllocatedItems: Cardinal;
      FixedFunctionVertexes: TVector4List;
      FixedFunctionIndexes: TUInt16List;
      Vao: TVertexArrayObject;

    class procedure PrepareShader(const AUseColor: Boolean);
    procedure SetUseColor(const Value: Boolean);
  private
    class procedure StaticGLContextClose;
  public
    { By default all zeros, you must set this before any rendering. }
    ModelViewProjection: TMatrix4;

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
  PrepareShader(AUseColor);
  Vao := TVertexArrayObject.Create(nil);
end;

destructor TCastleRenderUnlitMesh.Destroy;
begin
  FreeAndNil(Vao);
  glFreeBuffer(VboIndex);
  glFreeBuffer(VboVertex);
  inherited;
end;

class procedure TCastleRenderUnlitMesh.PrepareShader(const AUseColor: Boolean);
const
  SimplestVS = {$I simplest.vs.inc};
  SimplestFS = {$I simplest.fs.inc};
  SimplestUnlitFS = {$I simplest_unlit.fs.inc};
var
  Shader: TGLSLProgram;
begin
  if GLFeatures.EnableFixedFunction then
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
    if VboVertex = 0 then
      raise EInternalError.Create('TCastleRenderUnlitMesh has no vertexes defined to render');

    RenderContext.CurrentProgram := Programs[UseColor].Shader;
    RenderContext.CurrentVao := Vao;

    glBindBuffer(GL_ARRAY_BUFFER, VboVertex);
    // do this even if VboIndex = 0, to set this to 0
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, VboIndex);

    if FUseColor then
      Programs[UseColor].UniformColor.SetValue(Color);
    Programs[UseColor].UniformModelViewProjectionMatrix.SetValue(ModelViewProjection);
    Programs[UseColor].AttributeVertex.EnableArrayVector4(Vao, SizeOf(TVector4), 0);

    RenderDrawCall(nil);

    Programs[UseColor].AttributeVertex.DisableArray;

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
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

begin
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
begin
  if not GLFeatures.EnableFixedFunction then
  begin
    NewVbo := VboIndex = 0;
    if NewVbo then
      glGenBuffers(1, @VboIndex);

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, VboIndex);

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
  DataUsage: TGLEnum;
begin
  if not GLFeatures.EnableFixedFunction then
  begin
    NewVbo := VboVertex = 0;
    if NewVbo then
      glGenBuffers(1, @VboVertex);

    glBindBuffer(GL_ARRAY_BUFFER, VboVertex);

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
    PrepareShader(Value);
  end;
end;

{$endif read_implementation}
