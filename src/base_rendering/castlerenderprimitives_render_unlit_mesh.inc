{%MainUnit castlerenderprimitives.pas}
{
  Copyright 2001-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Part of CastleInternalGLImages unit: drawing 2D images on screen (TDrawableImage class). }

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
    It doesn't define any per-vertex attributes other than vertex positions.
    It limits rendering to 16-bit indexes, and always passed vectors as 4D (in homogeneous
    coordinates).
    It follows some current @link(RenderContext) state,
    like @link(TCastleRenderContext.DepthTest),
    @link(TCastleRenderContext.LineWidth) -- it doesn't control this state.
    For a full-featured mesh rendering, just use TCastleScene, and construct your
    mesh as TIndexedTriangleSetNode or TIndexedFaceSetNode, see
    https://castle-engine.io/viewport_and_scenes_from_code#_building_a_mesh_using_code .

    For shading this uses a simple shader:

    @unorderedList(
      @link(If @link(UseColor) (default @true) then the shader writes @link(Color)
        to the color buffer. Effectively this makes unlit rendering.

        Note: This class does not set up blending if @link(Color) alpha is less than 1.
      )

      @link(If @link(UseColor) is @false then the shader writes undefined
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

      @link(
        TODO: Maybe in the future it will be extended to optionally use a provided Shader.
        Potential docs:

        Uses the provided @link(Shader) for shading, which must be linked when calling @link(Draw).
        If the OpenGL(ES) does not support shaders or @link(Shader) is @nil,
        this will use built-in shader based on @link(UseColor).
      )
    )
  }
  TCastleRenderUnlitMesh = class
  strict private
    Shader: TGLSLProgram;
    UniformModelViewProjectionMatrix, UniformColor: TGLSLUniform;
    AttributeVertex: TGLSLAttribute;
    FUseColor: Boolean;
    FColor: TCastleColor;
    VboVertex, VboIndex: TGLuint;
    VboVertexAllocatedBytes, VboIndexAllocatedBytes,
      VboVertexAllocatedItems, VboIndexAllocatedItems: Cardinal;
  public
    { By default all zeros, you must set this before any rendering. }
    ModelViewProjection: TMatrix4;

    constructor Create(const AUseColor: Boolean = true);
    destructor Destroy; override;

    { Define indexes for all future rendering.
      Note: Passing indexes that do not exist in vertexes you define in SetVertexes
      may result in crashes, neither CGE nor rendering API necessarily check it. }
    procedure SetIndexes(const Indexes: packed array of UInt16); overload;
    procedure SetIndexes(const Indexes: PUInt16; const IndexesCount: Cardinal); overload;

    procedure SetVertexes(const Vertexes: packed array of TVector4;
      const UsageDynamic: Boolean); overload;
    procedure SetVertexes(const Vertexes: PVector4; const VertexesCount: Cardinal;
      const UsageDynamic: Boolean); overload;

    property UseColor: Boolean read FUseColor;
    { Color used for unlit rendering, if only UseColor is @true. Opaque white by default. }
    property Color: TCastleColor read FColor write FColor;

    procedure Render(const Mode: TPrimitiveMode);
  end;

{$endif read_interface}

{$ifdef read_implementation}

// TODO: Duplicated from castleglutils_draw_primitive_2d.inc, that should joing CastleRenderPrimitives
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

// TODO: Prepare for EnableFixedFunction = false

constructor TCastleRenderUnlitMesh.Create(const AUseColor: Boolean);
const
  SimplestVS = {$I simplest.vs.inc};
  SimplestFS = {$I simplest.fs.inc};
  SimplestUnlitFS = {$I simplest_unlit.fs.inc};
begin
  inherited Create;
  FUseColor := AUseColor;
  FColor := White;

  if GLFeatures.Shaders <> gsNone then
  begin
    Shader := TGLSLProgram.Create;
    Shader.AttachShader(stVertex, SimplestVS);

    if AUseColor then
    begin
      Shader.AttachShader(stFragment, SimplestUnlitFS);
    end else
    begin
      { On desktop OpenGL, fragment shader doesn't need to exist now.
        https://www.khronos.org/opengl/wiki/Fragment_Shader#Optional }
      {$ifdef OpenGLES}
      Shader.AttachShader(stFragment, SimplestFS);
      {$endif}
    end;

    Shader.Link;

    AttributeVertex := Shader.Attribute('castle_Vertex');
    UniformModelViewProjectionMatrix := Shader.Uniform('castle_ModelViewProjectionMatrix');
    if AUseColor then
      UniformColor := Shader.Uniform('color');
  end;
end;

destructor TCastleRenderUnlitMesh.Destroy;
begin
  FreeAndNil(Shader);
  glFreeBuffer(VboIndex);
  glFreeBuffer(VboVertex);
  inherited;
end;

procedure TCastleRenderUnlitMesh.Render(const Mode: TPrimitiveMode);
begin
  if VboVertex = 0 then
    raise EInternalError.Create('TCastleRenderUnlitMesh has no vertexes defined to render');

  if GLFeatures.VertexBufferObject then
  begin
    glBindBuffer(GL_ARRAY_BUFFER, VboVertex);
    // do this even if VboIndex = 0, to set this to 0
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, VboIndex);
  end;

  RenderContext.CurrentProgram := Shader;

  if Shader <> nil then
  begin
    if FUseColor then
      UniformColor.SetValue(Color);

    UniformModelViewProjectionMatrix.SetValue(ModelViewProjection);

    AttributeVertex.EnableArrayVector4(SizeOf(TVector4), 0);
  end else
  begin
    {$ifndef OpenGLES}
    if FUseColor then
      {$warnings off}
      glColorv(Color); // consciously using deprecated stuff; this should be internal in this unit
      {$warnings on}

    // TODO: make sure GL_PROJECTION is identity here
    glLoadMatrix(ModelViewProjection);

    glEnableClientState(GL_VERTEX_ARRAY);
    if GLFeatures.VertexBufferObject then
      glVertexPointer(4, GL_FLOAT, SizeOf(TVector4), nil)
    else
      glVertexPointer(4, GL_FLOAT, SizeOf(TVector4), {Primitive2DRes.PointPtr}nil{TODO});
    {$endif}
  end;

  if VboIndex <> 0 then
    glDrawElements(PrimitiveModeToGL[Mode], VboIndexAllocatedItems, GL_UNSIGNED_SHORT, nil)
  else
    glDrawArrays(PrimitiveModeToGL[Mode], 0, VboVertexAllocatedItems);

  if Shader <> nil then
  begin
    AttributeVertex.DisableArray;
  end else
  begin
    {$ifndef OpenGLES}
    glDisableClientState(GL_VERTEX_ARRAY);
    {$endif}
  end;

  // TODO: Do not reset everything (CurrentProgram, both vbo) after every rendering
  RenderContext.CurrentProgram := nil;

  if GLFeatures.VertexBufferObject then
  begin
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  end;
end;

procedure TCastleRenderUnlitMesh.SetIndexes(const Indexes: array of UInt16);
begin
  SetIndexes(@Indexes[0], High(Indexes) + 1);
end;

procedure TCastleRenderUnlitMesh.SetIndexes(const Indexes: PUInt16; const IndexesCount: Cardinal);
var
  NewVbo: Boolean;
  NewSize: Cardinal;
begin
  NewVbo := VboIndex = 0;
  if NewVbo then
    glGenBuffers(1, @VboIndex);

  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, VboIndex);

  NewSize := IndexesCount * SizeOf(UInt16);
  if NewVbo or (NewSize <> VboIndexAllocatedBytes) then
  begin
    // always GL_STATIC_DRAW for indexes, we assume we will not modify indexes very often
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, NewSize, Indexes, GL_STATIC_DRAW);
    VboIndexAllocatedBytes := NewSize;
    VboIndexAllocatedItems := IndexesCount;
  end else
    glBufferSubData(GL_ELEMENT_ARRAY_BUFFER, 0, NewSize, Indexes);
end;

procedure TCastleRenderUnlitMesh.SetVertexes(const Vertexes: PVector4; const VertexesCount: Cardinal;
  const UsageDynamic: Boolean);
var
  NewVbo: Boolean;
  NewSize: Cardinal;
  DataUsage: TGLEnum;
begin
  NewVbo := VboVertex = 0;
  if NewVbo then
    glGenBuffers(1, @VboVertex);

  glBindBuffer(GL_ARRAY_BUFFER, VboVertex);

  NewSize := VertexesCount * SizeOf(TVector4);
  if NewVbo or (NewSize <> VboVertexAllocatedBytes) then
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
    VboVertexAllocatedBytes := NewSize;
    VboVertexAllocatedItems := VertexesCount;
  end else
    glBufferSubData(GL_ARRAY_BUFFER, 0, NewSize, Vertexes);
end;

procedure TCastleRenderUnlitMesh.SetVertexes(const Vertexes: array of TVector4;
  const UsageDynamic: Boolean);
begin
  SetVertexes(@Vertexes[0], High(Vertexes) + 1, UsageDynamic);
end;

{$endif read_implementation}
