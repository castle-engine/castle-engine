{$ifdef read_interface}

  { }
  TGLTextureNode = class;

  TGLTextureNodeClass = class of TGLTextureNode;

  { OpenGL handling for VRML/X3D texture node. }
  TGLTextureNode = class(TResourceRenderer)
  protected
    { Calculate things from TextureProperties node.
      If TextureProperties = @nil, they are taken from defaults
      (possibly in Attributes). }
    procedure HandleTextureProperties(
      TextureProperties: TTexturePropertiesNode;
      out Filter: TTextureFilter; out Anisotropy: TGLfloat; out GUITexture: boolean);

    { Decide if this class can handle given texture Node. }
    class function IsClassForTextureNode(
      ANode: TAbstractTextureNode): boolean; virtual; abstract;

    { Do the time-consuming preparations before rendering this texture.
      Called from TGLRenderer.Prepare, always through our
      Prepare method. Our Prepare method takes care to catch
      common exceptions from this (EFramebufferError, ETextureLoadError),
      convert them to WritelnWarning and make Unprepare to undo the preparations.
      So Prepare-Unprepare work like constructor-destructor, with Unprepare
      having to be prepared to finilizing incomplete instance. }
    procedure PrepareCore(State: TX3DGraphTraverseState); virtual; abstract;
  public
    { ANode must be TAbstractTextureNode }
    constructor Create(ARenderer: TGLRenderer; ANode: TX3DNode); override;

    { Reference to handled texture node.
      Never @nil.
      It's guaranteed to satisfy IsClassForTextureNode method of this class. }
    function TextureNode: TAbstractTextureNode;

    { Find suitable TGLTextureNode class that can best handle given Node.
      Returns @nil if not found.

      @italic(Descedants implementors): override IsClassForTextureNode
      to be correctly recognized by this. }
    class function ClassForTextureNode(
      ANode: TAbstractTextureNode): TGLTextureNodeClass;

    { Do the time-consuming preparations before rendering this texture.
      Calls PrepareCore. }
    procedure Prepare(State: TX3DGraphTraverseState);
  public
    { Bind texture for OpenGL (without enabling it).

      Just like Enable, returns @false when texture node was not successfully
      prepared for OpenGL. Returns @true when it was successfully bound
      (caller can be sure then that given texture unit is currently active). }
    function Bind(const TextureUnit: Cardinal): boolean; virtual; abstract;

    { Enables texture for OpenGL. This has to bind texture identifier
      and enable proper texture state (for example:
      2D, and not 3D, and not cube).

      When returns @false, it means that texture node was not successfully
      prepared for OpenGL, which means (we assume that you called Prepare
      before Enable) that texture failed to load, required not available
      OpenGL version / extension etc. Caller will then disable
      the texture unit, and you don't have to generate tex coords for it.

      When returns @true (success) caller can be sure that the specified
      TextureUnit is currently bound (if OpenGL multitexturing
      extensions are available at all). This is useful, if you want
      to later adjust texture unit parameters, like
      glTexEnvi(GL_TEXTURE_ENV, ...).

      It's also already enabled (by glEnable(GL_TEXTURE_2D /
      GL_TEXTURE_CUBE_MAP / GL_TEXTURE_3D) ). }
    function Enable(const TextureUnit: Cardinal;
      Shader: TShader; const Env: TTextureEnv): boolean; virtual; abstract;

    { Sets texture state for many texture units, based on this node.
      On every texture unit where something is enabled,
      proper texture identifier must be bound.
      Also, has to set complete glTexEnv on every enabled texture unit.

      TextureUnitsCount says how many texture units can be enabled/disabled.

      TextureUnitsCount does *not* take into account whether multitexturing
      OpenGL extensions are available at all.
      Look at GLFeatures.UseMultiTexturing for this.
      Think of GLFeatures.UseMultiTexturing as capping TextureUnitsCount to 1
      (still, remember to honour TextureUnitsCount = 0 case in your implementation,
      even when GLFeatures.UseMultiTexturing = @true).

      You have to update TextureSlotsUsed, this is the count of texture units
      where some texture coordinates should be generated.
      It's initial value may be > 0, in case some texture slots are already taken.
      This means that all texture units above TextureSlotsUsed
      (to TextureUnitsCount - 1) should be disabled by the caller (no need to
      do this in EnableAll),
      and there's no need to generated texture coords for them.

      (
      Yes, there is some small optimization missed in the definition
      of TextureSlotsUsed: if some textures in the middle of
      multitexture children list failed to load, but some following children
      succeded, we'll generate tex coords even for the useless texture units
      in the middle. We could avoid generating texture coords for them,
      by changing TextureSlotsUsed into bool array.
      This optimization is not considered worthy implementing for now.
      )

      You have to set texture state of all texture units < TextureSlotsUsed,
      and only on them. }
    procedure EnableAll(
      const TextureUnitsCount: Cardinal;
      var TextureSlotsUsed: Cardinal;
      Shader: TShader); virtual; abstract;
  end;

  TGLTextureNodes = class(TResourceRendererList)
  private
    function GetItems(const Index: Integer): TGLTextureNode;
  public
    { Looks for item with given ANode.
      Returns -1 if not found. }
    function TextureNodeIndex(ANode: TAbstractTextureNode): Integer;

    { Looks for item with given ANode.
      Returns @nil if not found. }
    function TextureNode(ANode: TAbstractTextureNode): TGLTextureNode;

    property Items[Index: Integer]: TGLTextureNode read GetItems; default;

    { Prepare texture node, adding it to the list, if not prepared already.
      Accepts multi texture or not-multi texture nodes, accepts (and ignores)
      also @nil as TextureNode.
      Ignore not handled node classes.

      Returns created (or already existing) TGLTextureNode,
      it may be useful in case of TGLMultiTextureNode.Prepare implementation.

      Returns @nil if Node not suitable for TGLTextureNode (not handled,
      or not really a texture node at all). }
    function Prepare(State: TX3DGraphTraverseState;
      ANode: TAbstractTextureNode;
      ARenderer: TGLRenderer): TGLTextureNode;

    { Only bind texture node. Calls TGLTextureNode.Enable method.
      If no texture renderer is prepared for this node,
      returns @false. }
    function Bind(ANode: TAbstractTextureNode;
      const TextureUnit: Cardinal): boolean;

    { Enable and bind texture node. Calls TGLTextureNode.Enable method.
      If no texture renderer is prepared for this node,
      returns @false. }
    function Enable(ANode: TAbstractTextureNode;
      const TextureUnit: Cardinal;
      Shader: TShader; const Env: TTextureEnv): boolean;
  end;

  { Common class for all single (not multi-texture) texture nodes.

    Implements EnableAll method, by calling @link(Enable) call.
    Override only @link(Enable) in descendants. }
  TGLSingleTextureNode = class(TGLTextureNode)
  protected
    { Determines should texture mode be suited for RGB and grayscale texture.
      Default false, you should set this in descendant
      (latest possibility is to set this is a successful Enable call,
      but usually you want to do it in a Prepare call).
      You can set it by SetTextureRGBFromImage. }
    TextureRGB: boolean;
    procedure SetTextureRGBFromImage(Image: TEncodedImage);
  public
    procedure EnableAll(const TextureUnitsCount: Cardinal;
      var TextureSlotsUsed: Cardinal; Shader: TShader); override;
  end;

  { Handler for TMultiTextureNode.

    This is somewhat special, in that it will use other TGLTextureNode
    handlers to handle single textures inside --- but this is completely
    hidden from the interface. }
  TGLMultiTextureNode = class(TGLTextureNode)
  private
    PreparedTextureChildren: TX3DNodeList;
  protected
    class function IsClassForTextureNode(ANode: TAbstractTextureNode): boolean; override;
    procedure PrepareCore(State: TX3DGraphTraverseState); override;
  public
    { ANode must be TMultiTextureNode }
    constructor Create(ARenderer: TGLRenderer; ANode: TX3DNode); override;
    destructor Destroy; override;

    function TextureNode: TMultiTextureNode;

    procedure Unprepare; override;
    function Bind(const TextureUnit: Cardinal): boolean; override;
    function Enable(const TextureUnit: Cardinal;
      Shader: TShader; const Env: TTextureEnv): boolean; override;
    procedure EnableAll(const TextureUnitsCount: Cardinal;
      var TextureSlotsUsed: Cardinal; Shader: TShader); override;
  end;

  { Handler for 2D textures that have a corresponding OpenGL resource. }
  TGL2DTextureNode = class(TGLSingleTextureNode)
  public
    { OpenGL texture identifier.
      It may be 0 if the texture was initialized successfully. }
    function GLName: TGLTextureId; virtual; abstract;
  end;

  { Handler for TAbstractTexture2DNode with image (not a video). }
  TGLImageTextureNode = class(TGL2DTextureNode)
  strict private
    FGLName: TGLuint;
  protected
    class function IsClassForTextureNode(ANode: TAbstractTextureNode): boolean; override;
    procedure PrepareCore(State: TX3DGraphTraverseState); override;
  public
    NormalMap, HeightMap: TGLuint;
    HeightMapScale: Single;

    function TextureNode: TAbstractTexture2DNode;

    procedure Unprepare; override;
    function Bind(const TextureUnit: Cardinal): boolean; override;
    function Enable(const TextureUnit: Cardinal;
      Shader: TShader; const Env: TTextureEnv): boolean; override;
    function GLName: TGLTextureId; override;
  end;

  TGLMovieTextureNode = class(TGL2DTextureNode)
  protected
    class function IsClassForTextureNode(ANode: TAbstractTextureNode): boolean; override;
    procedure PrepareCore(State: TX3DGraphTraverseState); override;
  public
    GLVideo: TGLVideo3D;

    function TextureNode: TMovieTextureNode;

    procedure Unprepare; override;
    function Bind(const TextureUnit: Cardinal): boolean; override;
    function Enable(const TextureUnit: Cardinal;
      Shader: TShader; const Env: TTextureEnv): boolean; override;
    function GLName: TGLTextureId; override;
  end;

  TGLRenderedTextureNode = class(TGL2DTextureNode)
  strict private
    FGLName: TGLuint;
    RenderToTexture: TGLRenderToTexture;
  protected
    class function IsClassForTextureNode(ANode: TAbstractTextureNode): boolean; override;
    procedure PrepareCore(State: TX3DGraphTraverseState); override;
  public
    { The actual decided image size and mipmap status. }
    Width, Height: Cardinal;
    NeedsMipmaps: boolean;
    DepthMap: boolean;

    function TextureNode: TRenderedTextureNode;

    procedure Unprepare; override;
    function Bind(const TextureUnit: Cardinal): boolean; override;
    function Enable(const TextureUnit: Cardinal;
      Shader: TShader; const Env: TTextureEnv): boolean; override;
    function GLName: TGLTextureId; override;

    procedure Update(
      const Render: TRenderFromViewFunction;
      const ProjectionNear, ProjectionFar: Single;
      var NeedsRestoreViewport: boolean;
      const CurrentViewpoint: TAbstractViewpointNode;
      const CameraViewKnown: boolean;
      const CameraPosition, CameraDirection, CameraUp: TVector3;
      const ShapeBoundingBox: TBox3D;
      const GeometryTransform: TMatrix4;
      const GeometryCoords: TVector3List;
      var MirrorPlaneUniforms: TMirrorPlaneUniforms);
  end;

  { Common handling for texture nodes of TAbstractEnvironmentTextureNode. }
  TGLCubeMapTextureNode = class(TGLSingleTextureNode)
  public
    GLName: TGLuint;

    function TextureNode: TAbstractEnvironmentTextureNode;

    { Releases GLName by TextureCubeMap_DecReference.
      Suitable for descendants tht initialize GLName by
      TextureCubeMap_IncReference. }
    procedure Unprepare; override;

    function Bind(const TextureUnit: Cardinal): boolean; override;
    function Enable(const TextureUnit: Cardinal;
      Shader: TShader; const Env: TTextureEnv): boolean; override;
  end;

  TGLComposedCubeMapTextureNode = class(TGLCubeMapTextureNode)
  protected
    class function IsClassForTextureNode(ANode: TAbstractTextureNode): boolean; override;
    procedure PrepareCore(State: TX3DGraphTraverseState); override;
  public
    function TextureNode: TComposedCubeMapTextureNode;
  end;

  TGLImageCubeMapTextureNode = class(TGLCubeMapTextureNode)
  protected
    class function IsClassForTextureNode(ANode: TAbstractTextureNode): boolean; override;
    procedure PrepareCore(State: TX3DGraphTraverseState); override;
  public
    function TextureNode: TImageCubeMapTextureNode;
  end;

  TGLGeneratedCubeMapTextureNode = class(TGLCubeMapTextureNode)
  private
    RenderToTexture: TGLRenderToTexture;
  protected
    class function IsClassForTextureNode(ANode: TAbstractTextureNode): boolean; override;
    procedure PrepareCore(State: TX3DGraphTraverseState); override;
  public
    { The right size of the texture,
      that satisfies all OpenGL cube map sizes requirements
      (IsCubeMapTextureSized). }
    Size: Cardinal;

    { Does Filter need mipmaps. }
    NeedsMipmaps: boolean;

    function TextureNode: TGeneratedCubeMapTextureNode;

    procedure Unprepare; override;

    procedure Update(
      const Render: TRenderFromViewFunction;
      const ProjectionNear, ProjectionFar: Single;
      var NeedsRestoreViewport: boolean;
      const CubeMiddle: TVector3);
  end;

  TGL3DTextureNode = class(TGLSingleTextureNode)
  protected
    class function IsClassForTextureNode(ANode: TAbstractTextureNode): boolean; override;
    procedure PrepareCore(State: TX3DGraphTraverseState); override;
  public
    GLName: TGLuint;

    function TextureNode: TAbstractTexture3DNode;

    procedure Unprepare; override;
    function Bind(const TextureUnit: Cardinal): boolean; override;
    function Enable(const TextureUnit: Cardinal;
      Shader: TShader; const Env: TTextureEnv): boolean; override;
  end;

  TGLGeneratedShadowMap = class(TGL2DTextureNode)
  strict private
    RenderToTexture: TGLRenderToTexture;
    { VarianceShadowMaps calculated at the PrepareCore time. }
    VarianceShadowMaps: boolean;
    NeedsMipmaps: boolean;
    FGLName: TGLuint;
  protected
    class function IsClassForTextureNode(ANode: TAbstractTextureNode): boolean; override;
    procedure PrepareCore(State: TX3DGraphTraverseState); override;
  public
    { The right size of the texture,
      that satisfies all OpenGL sizes requirements. }
    Size: Cardinal;

    { Do we visualize depth map, because of Attributes.VisualizeDepthMap
      or compareMode = NONE. }
    VisualizeDepthMap: boolean;

    function TextureNode: TGeneratedShadowMapNode;

    procedure Unprepare; override;
    function Bind(const TextureUnit: Cardinal): boolean; override;
    function Enable(const TextureUnit: Cardinal;
      Shader: TShader; const Env: TTextureEnv): boolean; override;
    function GLName: TGLTextureId; override;

    procedure Update(
      const Render: TRenderFromViewFunction;
      const ProjectionNear, ProjectionFar: Single;
      var NeedsRestoreViewport: boolean;
      Light: TAbstractLightNode);

    { Check would we use Variance Shadow Maps with current ARenderer
      attributes and OpenGL version/extensions. }
    class function ClassVarianceShadowMaps(Attributes: TRenderingAttributes): boolean;
  end;

  TGLShaderTexture = class(TGLSingleTextureNode)
  protected
    class function IsClassForTextureNode(ANode: TAbstractTextureNode): boolean; override;
    procedure PrepareCore(State: TX3DGraphTraverseState); override;
  public
    procedure Unprepare; override;
    function Bind(const TextureUnit: Cardinal): boolean; override;
    function Enable(const TextureUnit: Cardinal;
      Shader: TShader; const Env: TTextureEnv): boolean; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{$ifndef OpenGLES}
const
  CombineGL: array [TCombine] of TGLint = (
    GL_MODULATE, GL_REPLACE, GL_ADD_SIGNED_EXT, GL_ADD, GL_SUBTRACT,
    GL_INTERPOLATE_EXT, GL_DOT3_RGB_ARB, GL_DOT3_RGBA_ARB );
{$endif}

function TextureRepeatToGL(const VrmlTextureRepeat: boolean): TGLenum;
begin
  if VrmlTextureRepeat then
    Result := GL_REPEAT else
    { GL_CLAMP is useless if VRML doesn't allow to control texture border color,
      and CLAMP_TO_EDGE is the more natural clamping method anyway...
      Hm, but X3D specification seems to indicate that normal clamp is OpenGL's CLAMP,
      and CLAMP_TO_EDGE is available by TextureProperties.boundaryMode*.
      But until this will get implemented, it's much safer (and more sensible?)
      to use GL_CLAMP_TO_EDGE here. }
    Result := GLFeatures.CLAMP_TO_EDGE;
end;

{ TGLTextureNode ------------------------------------------------------------- }

constructor TGLTextureNode.Create(ARenderer: TGLRenderer; ANode: TX3DNode);
begin
  Assert(ANode is TAbstractTextureNode, 'TGLTextureNode.Create acceps as Node only TAbstractTextureNode');
  inherited;
end;

procedure TGLTextureNode.HandleTextureProperties(
  TextureProperties: TTexturePropertiesNode;
  out Filter: TTextureFilter; out Anisotropy: TGLfloat; out GUITexture: boolean);

  { Convert TAutoMinificationFilter to TMinificationFilter,
    resolving the "default"/"fastest"/"nicest" aliases to specific values. }
  function FinalMinificationFilter(const FilterAuto: TAutoMinificationFilter): TMinificationFilter;
  begin
    case FilterAuto of
      minDefault: Result := Renderer.Attributes.MinificationFilter;
      minFastest: Result := minNearest;
      minNicest : Result := minLinearMipmapLinear;
      else        Result := FilterAuto;
    end;
  end;

  { Convert TAutoMagnificationFilter to TMagnificationFilter,
    resolving the "default"/"fastest"/"nicest" aliases to specific values. }
  function FinalMagnificationFilter(const FilterAuto: TAutoMagnificationFilter): TMagnificationFilter;
  begin
    case FilterAuto of
      magDefault: Result := Renderer.Attributes.MagnificationFilter;
      magFastest: Result := magNearest;
      magNicest : Result := magLinear;
      else        Result := FilterAuto;
    end;
  end;

begin
  if TextureProperties <> nil then
  begin
    Filter.Minification := FinalMinificationFilter(TextureProperties.MinificationFilter);
    Filter.Magnification := FinalMagnificationFilter(TextureProperties.MagnificationFilter);
    Anisotropy := TextureProperties.AnisotropicDegree;
    GUITexture := TextureProperties.GUITexture;
  end else
  begin
    Filter := Renderer.Attributes.TextureFilter;
    Anisotropy := 1;
    GUITexture := false;
  end;
end;

class function TGLTextureNode.ClassForTextureNode(
  ANode: TAbstractTextureNode): TGLTextureNodeClass;

  function TryResult(C: TGLTextureNodeClass): boolean;
  begin
    Result := C.IsClassForTextureNode(ANode);
    if Result then
      ClassForTextureNode := C;
  end;

begin
  { TODO: in the future, some way of registering class for this will
    be done. For now, just try known final TGLTextureNode descendants. }
  if not (TryResult(TGLMultiTextureNode) or
          TryResult(TGLImageTextureNode) or
          TryResult(TGLMovieTextureNode) or
          TryResult(TGLRenderedTextureNode) or
          TryResult(TGLComposedCubeMapTextureNode) or
          TryResult(TGLImageCubeMapTextureNode) or
          TryResult(TGLGeneratedCubeMapTextureNode) or
          TryResult(TGL3DTextureNode) or
          TryResult(TGLGeneratedShadowMap) or
          TryResult(TGLShaderTexture) ) then
    Result := nil;
end;

procedure TGLTextureNode.Prepare(State: TX3DGraphTraverseState);
begin
  try
    PrepareCore(State);
  except
    on E: EFramebufferError do
    begin
      Unprepare;
      WritelnWarning('VRML/X3D', 'Framebuffer error, generated texture not possible: '
        + E.Message);
    end;

    on E: ETextureLoadError do
    begin
      Unprepare;
      WritelnWarning('VRML/X3D', 'Cannot load texture to OpenGL: ' + E.Message);
    end;
  end;
end;

function TGLTextureNode.TextureNode: TAbstractTextureNode;
begin
  Result := TAbstractTextureNode(inherited Node);
end;

{ TGLTextureNodes ------------------------------------------------------------ }

function TGLTextureNodes.TextureNodeIndex(ANode: TAbstractTextureNode): Integer;
begin
  Result := inherited NodeIndex(ANode);
end;

function TGLTextureNodes.TextureNode(ANode: TAbstractTextureNode): TGLTextureNode;
begin
  Result := TGLTextureNode(inherited Node(ANode));
end;

function TGLTextureNodes.GetItems(const Index: Integer): TGLTextureNode;
begin
  Result := TGLTextureNode(inherited Items[Index]);
end;

function TGLTextureNodes.Prepare(State: TX3DGraphTraverseState;
  ANode: TAbstractTextureNode;
  ARenderer: TGLRenderer): TGLTextureNode;
var
  GLTextureNodeClass: TGLTextureNodeClass;
begin
  Result := nil;

  if ANode <> nil then
  begin
    GLTextureNodeClass := TGLTextureNodeClass.ClassForTextureNode(ANode);

    if (GLTextureNodeClass <> nil { Ignore if not handled node. }) and
       (TextureNodeIndex(ANode) = -1) then
    begin
      Result := GLTextureNodeClass.Create(ARenderer, ANode);
      Result.Prepare(State);
      Add(Result);
    end;
  end;
end;

function TGLTextureNodes.Bind(ANode: TAbstractTextureNode;
  const TextureUnit: Cardinal): boolean;
var
  GLTextureNode: TGLTextureNode;
begin
  GLTextureNode := TextureNode(ANode);
  Result := GLTextureNode <> nil;
  if Result then
    Result := GLTextureNode.Bind(TextureUnit);
end;

function TGLTextureNodes.Enable(ANode: TAbstractTextureNode;
  const TextureUnit: Cardinal; Shader: TShader; const Env: TTextureEnv): boolean;
var
  GLTextureNode: TGLTextureNode;
begin
  GLTextureNode := TextureNode(ANode);
  Result := GLTextureNode <> nil;
  if Result then
    Result := GLTextureNode.Enable(TextureUnit, Shader, Env);
end;

{ TGLSingleTextureNode ------------------------------------------------------- }

procedure TGLSingleTextureNode.SetTextureRGBFromImage(Image: TEncodedImage);
begin
  TextureRGB :=
    (Image is TGPUCompressedImage) or
    (Image is TRGBImage) or
    (Image is TRGBAlphaImage) or
    (Image is TRGBFloatImage);
end;

procedure TGLSingleTextureNode.EnableAll(const TextureUnitsCount: Cardinal;
  var TextureSlotsUsed: Cardinal; Shader: TShader);

{ Default texture mode for single texturing.
  For X3D MultiTexture nodes, they are always explicitly given
  by MultiTexture.mode field. This field applies only to non-multi textures.

  Note that VRML 2 / X3D specifications say that the texture color should
  replace material color (see "Lighting model" spec) *in case of RGB
  (not grayscale) textures*. We contradict spec, instead modulating
  with material color because of GL_MODULATE below. See
  https://castle-engine.io/x3d_multi_texturing.php#section_default_texture_mode .

  For alpha channel: specification says clearly that texture alpha
  should replace (never modulate) alpha channel, if only texture
  has any alpha channel. Again, we contradict it, instead multiplying alpha.

  Note that using GL_REPLACE below would not really make us conforming
  to the spec: it would mean that texture color replaces the whole resulting
  lighting calculation (not just material color, which is input to
  lighting calculation). So it would not be conforming either. }
const
  TextureModeGrayscale = coModulate;
  TextureModeRGB = coModulate;
var
  Env: TTextureEnv;
begin
  { initialize Env using TextureModeRGB/Grayscale }
  if TextureRGB then
    Env.Init(TextureModeRGB) else
    Env.Init(TextureModeGrayscale);

  if (TextureUnitsCount > 0) and
     Enable(TextureSlotsUsed, Shader, Env) then
  begin
    if GLFeatures.EnableFixedFunction then
    begin
      {$ifndef OpenGLES}
      { Here we know that Env was initialized above.
        So we know that mode is simple, the same for rgb and alpha,
        and can be set using GL_TEXTURE_ENV_MODE
        (doesn't require using GL_COMBINE). }
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, CombineGL[Env.Combine[cRGB]]);
      {$endif}
    end;
    Inc(TextureSlotsUsed);
  end;
end;

{ TGLMultiTextureNode -------------------------------------------------------- }

constructor TGLMultiTextureNode.Create(ARenderer: TGLRenderer; ANode: TX3DNode);
begin
  Assert(ANode is TMultiTextureNode, 'TGLMultiTextureNode.Create acceps as Node only TMultiTextureNode');
  inherited;
  PreparedTextureChildren := TX3DNodeList.Create(false);
end;

destructor TGLMultiTextureNode.Destroy;
begin
  FreeAndNil(PreparedTextureChildren);
  inherited;
end;

class function TGLMultiTextureNode.IsClassForTextureNode(
  ANode: TAbstractTextureNode): boolean;
begin
  Result := ANode is TMultiTextureNode;
end;

function TGLMultiTextureNode.TextureNode: TMultiTextureNode;
begin
  Result := TMultiTextureNode(inherited TextureNode);
end;

procedure TGLMultiTextureNode.PrepareCore(State: TX3DGraphTraverseState);
var
  ChildTex: TX3DNode;
  I: Integer;
begin
  { Prepare all children.
    Fill PreparedTextureChildren along the way,
    they will be used in Unprepare. }
  PreparedTextureChildren.Count := 0;

  for I := 0 to TextureNode.FdTexture.Count - 1 do
  begin
    ChildTex := TextureNode.FdTexture[I];
    if (ChildTex <> nil) and
       (ChildTex is TAbstractTextureNode) then
    begin
      if ChildTex is TMultiTextureNode then
        WritelnWarning('VRML/X3D', 'Child of MultiTexture node cannot be another MultiTexture node') else
      begin
        PreparedTextureChildren.Add(ChildTex);
        Renderer.GLTextureNodes.Prepare(State, TAbstractTextureNode(ChildTex), Renderer);
      end;
    end;
  end;
end;

procedure TGLMultiTextureNode.Unprepare;
var
  I: Integer;
begin
  { Below comments concerns engine before 2.0.0. After 2.0.0 this could actually
    by simplified, no need for PreparedTextureChildren.

    We cannot look at TextureNode.FdTexture here.

    Reason: TextureNode may be already freed at the Unprepare time.
    CastleRenderer explicitly allows to free VRML/X3D nodes before
    unpreparing them. This is used e.g. by doing TCastleSceneCore.Load
    (that calls ChangedAll, that calls Unprepare, only at the end,
    *after* old nodes have been already freed.)

    Accessing here TextureNode.FdTexture.Count e.g. crashes on
    multiple_viewports: open data/bridge_final.x3dv, then open
    data/boxes.x3dv. (Note that multiple_viewports loads new scene
    by simple Scene.Load.)

    That's why we use PreparedTextureChildren saved.

    Note: we cannot save TGLTextureNode values, as they could be freed
    by something beside our control. So PreparedTextureChildren holds
    only vrml nodes, passed to Renderer.GLTextureNodes.Unprepare. }

  for I := 0 to PreparedTextureChildren.Count - 1 do
    Renderer.GLTextureNodes.Unprepare(PreparedTextureChildren[I]);
end;

function TGLMultiTextureNode.Bind(const TextureUnit: Cardinal): boolean;
begin
  { TGLMultiTextureNode cannot set only one texture unit.
    This may be called from GLSL shader, when someone will use MultiTexture
    node for a shader uniform field. I don't know how this should be handled,
    I guess returning failure is Ok for now. }
  Result := false;
end;

function TGLMultiTextureNode.Enable(const TextureUnit: Cardinal;
  Shader: TShader; const Env: TTextureEnv): boolean;
begin
  { This should never be called. TGLMultiTextureNode cannot set only one
    texture unit. }
  Result := false;
end;

procedure TGLMultiTextureNode.EnableAll(const TextureUnitsCount: Cardinal;
  var TextureSlotsUsed: Cardinal; Shader: TShader);
{$ifndef OpenGLES}
const
  ColorSourceGL: array [TColorSource] of TGLint =
  ( GL_PRIMARY_COLOR, GL_TEXTURE, GL_CONSTANT, GL_PREVIOUS );
  Argument: array [ta0..ta1] of Integer = ( 0, 1 );
{$endif}
var
  InitialTextureSlotsUsed: Cardinal;
  ChildTex: TX3DNode;
  I: Integer;
  Success: boolean;
  Env: TTextureEnv;
  ModeStr, SourceStr, FunctionStr: string;
begin
  { calculate TextureSlotsUsed }
  InitialTextureSlotsUsed := TextureSlotsUsed;
  TextureSlotsUsed := TextureSlotsUsed + TextureNode.FdTexture.Count;
  MinVar(TextureSlotsUsed, TextureUnitsCount);
  if not GLFeatures.UseMultiTexturing then
    MinVar(TextureSlotsUsed, 1);

  for I := 0 to TextureSlotsUsed - InitialTextureSlotsUsed - 1 do
  begin
    ChildTex := TextureNode.FdTexture[I];
    Success := false;

    if (ChildTex <> nil) and
       (ChildTex is TAbstractTextureNode) then
    begin
      if I < TextureNode.FdMode.Count then
        ModeStr := TextureNode.FdMode.Items[I] else
        ModeStr := '';
      if I < TextureNode.FdSource.Count then
        SourceStr := TextureNode.FdSource.Items[I] else
        SourceStr := '';
      if I < TextureNode.FdFunction.Count then
        FunctionStr := TextureNode.FdFunction.Items[I] else
        FunctionStr := '';
      Env.Init(ModeStr, SourceStr, FunctionStr);

      if ChildTex is TMultiTextureNode then
        WritelnWarning('VRML/X3D', 'Child of MultiTexture node cannot be another MultiTexture node') else
        Success := Renderer.GLTextureNodes.Enable(
          TAbstractTextureNode(ChildTex),
          InitialTextureSlotsUsed + I, Shader, Env);

      { Apply Env for fixed-function pipeline. }
      if Success and GLFeatures.UseMultiTexturing { needed OpenGL exts available } then
      begin
        { Set all the multitexture mode-related stuff.
          Below we handle TextureNode.mode, source, color, alpha,
          function fields. }

        if Env.Disabled then
        begin
          { When mode=OFF, turn off the texture unit. }
          Renderer.DisableCurrentTexture;
        end else
        begin
          if GLFeatures.EnableFixedFunction then
          begin
            {$ifndef OpenGLES}
            glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE);

            glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB, CombineGL[Env.Combine[cRGB]]);
            glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_ALPHA, CombineGL[Env.Combine[cAlpha]]);

            glTexEnvf(GL_TEXTURE_ENV, GL_RGB_SCALE, Env.Scale[cRGB]);
            glTexEnvf(GL_TEXTURE_ENV, GL_ALPHA_SCALE, Env.Scale[cAlpha]);

            if Env.CurrentTextureArgument[cRGB] <> taNone then
            begin
              glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB + Argument[Env.CurrentTextureArgument[cRGB]], GL_TEXTURE);
              glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB + Argument[Env.CurrentTextureArgument[cRGB]], GL_SRC_COLOR);
            end;

            if Env.CurrentTextureArgument[cAlpha] <> taNone then
            begin
              glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_ALPHA + Argument[Env.CurrentTextureArgument[cAlpha]], GL_TEXTURE);
              glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_ALPHA + Argument[Env.CurrentTextureArgument[cAlpha]], GL_SRC_ALPHA);
            end;

            if Env.SourceArgument[cRGB] <> taNone then
            begin
              glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB + Argument[Env.SourceArgument[cRGB]], ColorSourceGL[Env.Source[cRGB]]);
              glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB + Argument[Env.SourceArgument[cRGB]], GL_SRC_COLOR);
            end;

            if Env.SourceArgument[cAlpha] <> taNone then
            begin
              glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_ALPHA + Argument[Env.SourceArgument[cAlpha]], ColorSourceGL[Env.Source[cAlpha]]);
              glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_ALPHA + Argument[Env.SourceArgument[cAlpha]], GL_SRC_ALPHA);
            end;

            if (Env.Combine[cRGB] = coBlend) or
               (Env.Combine[cAlpha] = coBlend) then
            begin
              { Whole source2 (both RGB and alpha) is filled by alpha from color
                specified by BlendAlphaSource. }
              glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE2_RGB, ColorSourceGL[Env.BlendAlphaSource]);
              glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE2_ALPHA, ColorSourceGL[Env.BlendAlphaSource]);
              glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND2_RGB, GL_SRC_ALPHA);
              glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND2_ALPHA, GL_SRC_ALPHA);
            end;

            if Env.NeedsConstantColor then
            begin
              { Assign constant color now, when we know it should be used. }
              glTexEnvv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, Vector4(
                TextureNode.FdColor.Value,
                TextureNode.FdAlpha.Value));
            end;
            {$endif}
          end;
        end;
      end;
    end;

    if not Success then
      Renderer.DisableTexture(I);
  end;
end;

{ TGLImageTextureNode -------------------------------------------------------- }

class function TGLImageTextureNode.IsClassForTextureNode(
  ANode: TAbstractTextureNode): boolean;
begin
  Result := (ANode is TAbstractTexture2DNode) and
    TAbstractTexture2DNode(ANode).IsTextureImage;
end;

function TGLImageTextureNode.TextureNode: TAbstractTexture2DNode;
begin
  Result := TAbstractTexture2DNode(inherited TextureNode);
end;

procedure TGLImageTextureNode.PrepareCore(State: TX3DGraphTraverseState);
var
  Filter: TTextureFilter;
  Anisotropy: TGLfloat;
  TextureWrap: TTextureWrap2D;
  GUITexture: boolean;
begin
  { HandledNode already made sure IsTextureImage = @true }

  HandleTextureProperties(TextureNode.TextureProperties,
    Filter, Anisotropy, GUITexture);

  TextureWrap.Data[0] := TextureRepeatToGL(TextureNode.RepeatS);
  TextureWrap.Data[1] := TextureRepeatToGL(TextureNode.RepeatT);

  SetTextureRGBFromImage(TextureNode.TextureImage);

  FGLName := Renderer.Cache.TextureImage_IncReference(
    TextureNode.TextureImage,
    TextureNode.TextureUsedFullUrl,
    TextureNode,
    Filter,
    Anisotropy,
    TextureWrap,
    TextureNode.TextureComposite,
    GUITexture);
end;

procedure TGLImageTextureNode.Unprepare;
begin
  if FGLName <> 0 then
  begin
    Renderer.Cache.TextureImage_DecReference(FGLName);
    FGLName := 0;
  end;
end;

function TGLImageTextureNode.Bind(const TextureUnit: Cardinal): boolean;
begin
  Result := FGLName <> 0;
  if not Result then Exit;

  Renderer.ActiveTexture(TextureUnit);
  glBindTexture(GL_TEXTURE_2D, FGLName);
end;

function TGLImageTextureNode.Enable(const TextureUnit: Cardinal;
  Shader: TShader; const Env: TTextureEnv): boolean;
begin
  Result := Bind(TextureUnit);
  if not Result then Exit;

  Shader.EnableTexture(TextureUnit, tt2D, TextureNode, Env);
end;

function TGLImageTextureNode.GLName: TGLTextureId;
begin
  Result := FGLName;
end;

{ TGLMovieTextureNode -------------------------------------------------------- }

class function TGLMovieTextureNode.IsClassForTextureNode(
  ANode: TAbstractTextureNode): boolean;
begin
  { Although for most code TGLMovieTextureNode, it would be enought
    to have any TAbstractTexture2DNode with IsTextureVideo = @true.
    For when rendering, we'll need some TMovieTextureNode properties
    to choose video frame.

    Anyway, TMovieTextureNode is for now the only texture node possible
    that may have IsTextureVideo = @true, so it's not a real problem for now. }

  Result := (ANode is TMovieTextureNode) and
    TMovieTextureNode(ANode).IsTextureVideo;
end;

function TGLMovieTextureNode.TextureNode: TMovieTextureNode;
begin
  Result := TMovieTextureNode(inherited TextureNode);
end;

procedure TGLMovieTextureNode.PrepareCore(State: TX3DGraphTraverseState);
var
  Filter: TTextureFilter;
  Anisotropy: TGLfloat;
  TextureWrap: TTextureWrap2D;
  GUITexture: boolean;
begin
  { HandledNode already made sure IsTextureVideo = @true }

  HandleTextureProperties(TextureNode.TextureProperties,
    Filter, Anisotropy, GUITexture);

  TextureWrap.Data[0] := TextureRepeatToGL(TextureNode.RepeatS);
  TextureWrap.Data[1] := TextureRepeatToGL(TextureNode.RepeatT);

  SetTextureRGBFromImage(TextureNode.TextureVideo.Items[0]);

  GLVideo := Renderer.Cache.TextureVideo_IncReference(
    TextureNode.TextureVideo,
    TextureNode.TextureUsedFullUrl,
    TextureNode,
    Filter,
    Anisotropy,
    TextureWrap,
    GUITexture);
end;

procedure TGLMovieTextureNode.Unprepare;
begin
  if GLVideo <> nil then
    Renderer.Cache.TextureVideo_DecReference(GLVideo);
end;

function TGLMovieTextureNode.GLName: TGLTextureId;
var
  VideoUnscaledTime: TFloatTime;
begin
  { Note: don't call IsTextureImage, IsTextureVideo here --- this
    would cause reloading images/videos, nullifying
    TCastleSceneCore.FreeResources([frTextureDataInNodes]) purpose.

    Actually, it would be safe to call this for non-MovieTexture nodes,
    as they should be prepared to GL resources before doing
    FreeResources. But for MovieTexture nodes it's forbidden,
    as it's called at every frame render. }

  if GLVideo <> nil then
  begin
    VideoUnscaledTime :=
      TextureNode.InternalTimeDependentHandler.ElapsedTimeInCycle *
      TextureNode.Speed;
    if TextureNode.Speed < 0 then
      VideoUnscaledTime := TextureNode.Duration + VideoUnscaledTime;
    Result := GLVideo.GLTextureFromTime(VideoUnscaledTime);
  end else
    Result := 0;
end;

function TGLMovieTextureNode.Bind(const TextureUnit: Cardinal): boolean;
var
  N: TGLTextureId;
begin
  N := GLName;
  Result := N <> 0;
  if Result then
  begin
    Renderer.ActiveTexture(TextureUnit);
    glBindTexture(GL_TEXTURE_2D, N);
  end;
end;

function TGLMovieTextureNode.Enable(const TextureUnit: Cardinal;
  Shader: TShader; const Env: TTextureEnv): boolean;
begin
  Result := Bind(TextureUnit);
  if not Result then Exit;

  Shader.EnableTexture(TextureUnit, tt2D, TextureNode, Env);
end;

{ TGLRenderedTextureNode ----------------------------------------------------- }

class function TGLRenderedTextureNode.IsClassForTextureNode(
  ANode: TAbstractTextureNode): boolean;
begin
  Result := ANode is TRenderedTextureNode;
end;

function TGLRenderedTextureNode.TextureNode: TRenderedTextureNode;
begin
  Result := TRenderedTextureNode(inherited TextureNode);
end;

procedure TGLRenderedTextureNode.PrepareCore(State: TX3DGraphTraverseState);
var
  InitialImage: TCastleImage;
  Filter: TTextureFilter;
  Anisotropy: TGLfloat;
  TextureWrap: TTextureWrap2D;
  NodeWidth, NodeHeight: Cardinal;
  GUITexture: boolean;
  Sizing: TTextureSizing;
begin
  HandleTextureProperties(TextureNode.TextureProperties,
    Filter, Anisotropy, GUITexture);

  { calculate Filter, Anisotropy, NeedsMipmaps }
  NeedsMipmaps := Filter.NeedsMipmaps;
  if NeedsMipmaps and not HasGenerateMipmap then
  begin
    WritelnWarning('VRML/X3D' { This may be caused by OpenGL implementation
      limits, so it may be impossible to predict by VRML author,
      so it's "ignorable" warning. },
      'OpenGL implementation doesn''t allow any glGenerateMipmap* version, so you cannot use mipmaps for RenderedTexture');
    Filter.Minification := minLinear;
    NeedsMipmaps := false;
  end;

  TextureWrap.Data[0] := TextureRepeatToGL(TextureNode.FdRepeatS.Value);
  TextureWrap.Data[1] := TextureRepeatToGL(TextureNode.FdRepeatT.Value);

  { calculate Width, Height }
  if TextureNode.FdDimensions.Items.Count - 1 >= 0 then
    NodeWidth := Max(TextureNode.FdDimensions.Items[0], 0) else
    NodeWidth := DefaultRenderedTextureWidth;
  if TextureNode.FdDimensions.Items.Count - 1 >= 1 then
    NodeHeight := Max(TextureNode.FdDimensions.Items[1], 0) else
    NodeHeight := DefaultRenderedTextureHeight;
  Width  := NodeWidth ;
  Height := NodeHeight;
  if GUITexture then
    Sizing := tsAny else
    Sizing := tsScalablePowerOf2;
  if not IsTextureSized(Width, Height, Sizing) then
  begin
    Width  := ResizeToTextureSize(Width , Sizing);
    Height := ResizeToTextureSize(Height, Sizing);
    WritelnWarning('VRML/X3D' { This may be caused by OpenGL implementation
      limits, so it may be impossible to predict by VRML author,
      so it's "ignorable" warning. },
      Format('Rendered texture size %d x %d is incorrect (texture size must be a power of two, > 0 and <= GL_MAX_TEXTURE_SIZE = %d), corrected to %d x %d',
        [ NodeWidth, NodeHeight,
          GLFeatures.MaxTextureSize,
          Width, Height]));
  end;

  { calculate DepthMap }
  if TextureNode.FdDepthMap.Count > 0 then
    DepthMap := TextureNode.FdDepthMap.Items[0] else
    DepthMap := false;

  if DepthMap then
  begin
    if not GLFeatures.TextureDepth then
    begin
      WritelnWarning('VRML/X3D', 'Your graphic card doesn''t support depth textures, cannot use RenderedTexture with depthMap = TRUE');
      Exit;
    end;

    FGLName := Renderer.Cache.TextureDepth_IncReference(
      TextureNode, TextureWrap, smNone, Width, Height, false);
  end else
  begin
    InitialImage := TRGBImage.Create(Width, Height);
    try
      InitialImage.URL := 'generated:/' + TextureNode.NiceName;
      SetTextureRGBFromImage(InitialImage);

      { Fill with deliberately stupid (but constant) color,
        to recognize easily RenderedTexture which don't have textures
        updated. }
      InitialImage.Clear(Vector4Byte(255, 0, 255, 255));

      FGLName := Renderer.Cache.TextureImage_IncReference(
        InitialImage,
        '',
        TextureNode,
        Filter,
        Anisotropy,
        TextureWrap,
        nil,
        GUITexture);

      { RenderedTexture never has any normal / height map
        (Hm, although it would be possible to generate some in theory
        --- after all, we generate it from 3D data. Idea for the future.)
      NormalMap := 0;
      HeightMap := 0;
      }
    finally FreeAndNil(InitialImage) end;
  end;

  RenderToTexture := TGLRenderToTexture.Create(Width, Height);
  RenderToTexture.SetTexture(FGLName, GL_TEXTURE_2D);
  if DepthMap then
    RenderToTexture.Buffer := tbDepth else
    RenderToTexture.Buffer := tbColor;
  { TODO: This doesn't handle for now situation when we want to capture depth
    map, and we should use stencil buffer. It's not really a useful situation
    now... But it could be implemented:
    - Add GLFeatures.HasStencil to detect we have stencil buffer.
    - Change TextureDepth_IncReference to create packed depth+stencil texture
      when requested (when GLFeatures.PackedDepthStencil and we currently
      GLFeatures.HasStencil).
    - set below RenderToTexture.Stencil := GLFeatures.HasStencil; }
  RenderToTexture.Stencil := not DepthMap;
  RenderToTexture.GLContextOpen;
end;

procedure TGLRenderedTextureNode.Unprepare;
begin
  FreeAndNil(RenderToTexture);

  if FGLName <> 0 then
  begin
    if DepthMap then
      Renderer.Cache.TextureDepth_DecReference(FGLName) else
      Renderer.Cache.TextureImage_DecReference(FGLName);
    FGLName := 0;
  end;
end;

function TGLRenderedTextureNode.Bind(const TextureUnit: Cardinal): boolean;
begin
  Result := FGLName <> 0;
  if not Result then Exit;

  Renderer.ActiveTexture(TextureUnit);
  glBindTexture(GL_TEXTURE_2D, FGLName);
end;

function TGLRenderedTextureNode.Enable(const TextureUnit: Cardinal;
  Shader: TShader; const Env: TTextureEnv): boolean;
begin
  Result := Bind(TextureUnit);
  if not Result then Exit;

  Shader.EnableTexture(TextureUnit, tt2D, TextureNode, Env);
end;

procedure TGLRenderedTextureNode.Update(
  const Render: TRenderFromViewFunction;
  const ProjectionNear, ProjectionFar: Single;
  var NeedsRestoreViewport: boolean;
  const CurrentViewpoint: TAbstractViewpointNode;
  const CameraViewKnown: boolean;
  const CameraPosition, CameraDirection, CameraUp: TVector3;
  const ShapeBoundingBox: TBox3D;
  const GeometryTransform: TMatrix4;
  const GeometryCoords: TVector3List;
  var MirrorPlaneUniforms: TMirrorPlaneUniforms);

  function GetProjectionMatrix(const Viewpoint: TAbstractViewpointNode): TMatrix4;
  { This somewhat follows the logic similar to TCastleScene.GLProjectionCore:
    we have to calculate projection, given Viewpoint node.

    We have to calculate things a little differently, e.g. we have
    no NavigationInfo (it would be bad to use scene bound NavigationInfo,
    since VRML author cannot change it by any RenderedTexture field,
    and adding such field could be too much...),
    no knowledge of scene box (although we do have ProjectionNear / Far
    already).
  }

    procedure DoPerspective;
    var
      FieldOfView, AngleOfViewY: Single;
      FieldOfViewForceVertical: boolean;
      Angles: TVector2;
    begin
      if (Viewpoint <> nil) and
         (Viewpoint is TViewpointNode) then
      begin
        FieldOfView := TViewpointNode(Viewpoint).FieldOfView;
        FieldOfViewForceVertical := TViewpointNode(Viewpoint).FieldOfViewForceVertical;
      end else
      if (Viewpoint <> nil) and
         (Viewpoint is TPerspectiveCameraNode_1) then
      begin
        FieldOfView := TPerspectiveCameraNode_1(Viewpoint).FdHeightAngle.Value;
        FieldOfViewForceVertical := false;
      end else
      begin
        FieldOfView := DefaultViewpointFieldOfView;
        FieldOfViewForceVertical := false;
      end;

      Angles := TViewpointNode.ViewpointAngleOfView(
        FieldOfView, FieldOfViewForceVertical, Width, Height);
      AngleOfViewY := RadToDeg(Angles[1]);

      Result := PerspectiveProjectionMatrixDeg(AngleOfViewY, Width / Height,
        ProjectionNear, ProjectionFar);
    end;

    procedure DoOrthographic;
    var
      FieldOfView: TSingleList;
      FinalFieldOfView: TFloatRectangle;
    begin
      { default VRML/X3D fov }
      FinalFieldOfView.Left   := -1;
      FinalFieldOfView.Bottom := -1;
      FinalFieldOfView.Width  :=  2;
      FinalFieldOfView.Height :=  2;

      { update left / right / bottom / top using OrthoViewpoint.fieldOfView }
      if (Viewpoint <> nil) and
         (Viewpoint is TOrthoViewpointNode) then
      begin
        FieldOfView := TOrthoViewpointNode(Viewpoint).FdFieldOfView.Items;
        if FieldOfView.Count > 0 then FinalFieldOfView.Left   := FieldOfView.Items[0];
        if FieldOfView.Count > 1 then FinalFieldOfView.Bottom := FieldOfView.Items[1];
        if FieldOfView.Count > 2 then FinalFieldOfView.Width  := FieldOfView.Items[2] - FinalFieldOfView.Left;
        if FieldOfView.Count > 3 then FinalFieldOfView.Height := FieldOfView.Items[3] - FinalFieldOfView.Bottom;
      end else
      if (Viewpoint <> nil) and
         (Viewpoint is TOrthographicCameraNode_1) then
      begin
        FinalFieldOfView.Left   := -TOrthographicCameraNode_1(Viewpoint).FdHeight.Value / 2;
        FinalFieldOfView.Bottom := -TOrthographicCameraNode_1(Viewpoint).FdHeight.Value / 2;
        FinalFieldOfView.Width  :=  TOrthographicCameraNode_1(Viewpoint).FdHeight.Value;
        FinalFieldOfView.Height :=  TOrthographicCameraNode_1(Viewpoint).FdHeight.Value;
      end;

      TOrthoViewpointNode.AspectFieldOfView(FinalFieldOfView, Width / Height);

      Result := OrthoProjectionMatrix(FinalFieldOfView, ProjectionNear, ProjectionFar);
    end;

  var
    ProjectionType: TProjectionType;
  begin
    if Viewpoint <> nil then
      ProjectionType := Viewpoint.ProjectionType
    else
      ProjectionType := ptPerspective;

    case ProjectionType of
      ptPerspective: DoPerspective;
      ptOrthographic: DoOrthographic;
      ptFrustum: raise EInternalError.Create('TGLRenderedTextureNode.Update: X3D Viewpoint node should not be able to specify ptFrustum projection');
      else raise EInternalError.Create('TGLRenderedTextureNode.Update-ProjectionType?');
    end;
  end;

  procedure GetRenderedTextureCamera(const Viewpoint: TAbstractViewpointNode;
    out Pos, Dir, Up: TVector3);
  var
    { It's returned by Viewpoint.GetView, but we'll ignore it. }
    GravityUp: TVector3;

    procedure GetFromCurrent;
    begin
      if CameraViewKnown then
      begin
        Pos := CameraPosition;
        Dir := CameraDirection;
        Up  := CameraUp;
      end else
      if CurrentViewpoint <> nil then
        CurrentViewpoint.GetView(Pos, Dir, Up, GravityUp) else
      begin
        { If all else fails (no viewpoint node bound, not known current
          camera settings) then use defaults. }
        Pos := DefaultX3DCameraPosition[cvVrml2_X3d];
        Dir := DefaultX3DCameraDirection;
        Up  := DefaultX3DCameraUp;
      end;
    end;

  begin
    if Viewpoint = CurrentViewpoint then
      GetFromCurrent else
    begin
      { Viewpoint gets assigned something different than CurrentViewpoint
        only when it's non-nil. }
      Assert(Viewpoint <> nil);
      Viewpoint.GetView(Pos, Dir, Up, GravityUp);
    end;
  end;

  { Best plane for a set of points.
    The plane direction (first 3 components) is guaranteed to be normalized. }
  function PointsPlane(const Points: TVector3List; out Plane: TVector4): Boolean;
  var
    Normal: TVector3 absolute Plane;
    Tri: TTriangle3;
    I: Integer;
    PlaneShift: Single;
    Point: TVector3;
  begin
    if Points.Count < 3 then Exit(false);

    { Calculate plane Normal, averaging normals of all valid triangles.
      Assumes polygon is convex. }

    Normal := TVector3.Zero;

    Tri[0] := Points.List^[0];
    Tri[1] := Points.List^[1];
    Tri[2] := Points.List^[2];
    if Tri.IsValid then
      Normal := Tri.Normal;

    for I := 3 to Points.Count - 1 do
    begin
      Tri[1] := Tri[2];
      Tri[2] := Points.List^[I];
      if Tri.IsValid then
        Normal := Normal + Tri.Normal;
    end;

    if Normal.IsPerfectlyZero then
      Exit(false); // no valid triangle

    { calculate Plane[3] by averaging possible shift for all Points }
    PlaneShift := 0;
    for Point in Points do
    begin
      PlaneShift := PlaneShift +
        -Normal.Data[0] * Point.Data[0]
        -Normal.Data[1] * Point.Data[1]
        -Normal.Data[2] * Point.Data[2];
    end;
    PlaneShift := PlaneShift / Points.Count;

    Plane[3] := PlaneShift;
    Result := true;
  end;

  procedure GetMatricesForViewpoint(const Viewpoint: TAbstractViewpointNode;
    out CameraMatrix, CameraRotationMatrix, ProjectionMatrix: TMatrix4);
  var
    Pos, Dir, Up: TVector3;
  begin
    ProjectionMatrix := GetProjectionMatrix(Viewpoint);

    GetRenderedTextureCamera(Viewpoint, Pos, Dir, Up);
    CameraMatrix := LookDirMatrix(Pos, Dir, Up);
    CameraRotationMatrix := LookDirMatrix(TVector3.Zero, Dir, Up);
  end;

  procedure GetMatricesForViewpointMirror(const Viewpoint: TViewpointMirrorNode;
    out CameraMatrix, CameraRotationMatrix, ProjectionMatrix: TMatrix4);
  var
    Pos, Dir, Up, PosOnPlane, Side, Coord, PlaneCoord: TVector3;
    PlaneCoordProjected: TVector2;
    Plane: TVector4;
    ZNear: Single;
    FrustumDimensions: TFloatRectangle;
    ShapeCorners: TBoxCorners;
  begin
    if GeometryCoords = nil then
    begin
      WritelnWarning('ViewpointMirror can only be used on node with coordinates');
      GetMatricesForViewpoint(
        CurrentViewpoint, CameraMatrix, CameraRotationMatrix, ProjectionMatrix);
      Exit;
    end;

    GetRenderedTextureCamera(CurrentViewpoint, Pos, Dir, Up);

    { calculate Plane of the mirror }
    if not PointsPlane(GeometryCoords, Plane) then
    begin
      WritelnWarning('ViewpointMirror plane cannot be determined, the shape does not define a plane');
      GetMatricesForViewpoint(
        CurrentViewpoint, CameraMatrix, CameraRotationMatrix, ProjectionMatrix);
      Exit;
    end;
    Plane := PlaneTransform(Plane, GeometryTransform);

    { reflect Pos, Dir, Up for mirror view }
    PosOnPlane := PointOnPlaneClosestToPoint(Plane, Pos);
    Pos := 2 * PosOnPlane - Pos; // mirror Pos versus Plane
    Dir := PosOnPlane - Pos;
    //Up := keep current Up

    { calculate Side, and make Dir, Up normalized and orthogonal }
    Dir.NormalizeMe;
    //Up.NormalizeMe; // no need to
    MakeVectorsOrthoOnTheirPlane(Up, Dir);
    Side := TVector3.CrossProduct(Dir, Up);

    CameraMatrix := LookDirMatrix(Pos, Dir, Side, Up);
    CameraRotationMatrix := LookDirMatrix(TVector3.Zero, Dir, Side, Up);

    { calculate FrustumDimensions,
      to include ShapeBoundingBox projected on the Plane. }
    ShapeBoundingBox.Corners(ShapeCorners);
    FrustumDimensions := TFloatRectangle.Empty;
    for Coord in ShapeCorners do
    begin
      PlaneCoord := PointOnPlaneClosestToPoint(Plane, Coord) - PosOnPlane;
      PlaneCoordProjected := Vector2(
        TVector3.DotProduct(PlaneCoord, Side),
        TVector3.DotProduct(PlaneCoord, Up)
      );
      FrustumDimensions := FrustumDimensions.Include(PlaneCoordProjected);
    end;

    ZNear := PointsDistance(Pos, PosOnPlane) + Viewpoint.DistanceFromShape;
    ProjectionMatrix := FrustumProjectionMatrix(FrustumDimensions,
      ZNear, ProjectionFar);

    if MirrorPlaneUniforms = nil then
      MirrorPlaneUniforms := TMirrorPlaneUniforms.Create;
    MirrorPlaneUniforms.NormalizedPlane := Plane;
    MirrorPlaneUniforms.CameraPositionOnPlane := PosOnPlane;
    MirrorPlaneUniforms.CameraSide := Side;
    MirrorPlaneUniforms.CameraUp := Up;
    MirrorPlaneUniforms.FrustumDimensions := FrustumDimensions;
  end;

var
  CameraMatrix, CameraRotationMatrix, SavedProjectionMatrix, NewProjectionMatrix: TMatrix4;
  Camera: TRenderingCamera;
begin
  if FGLName = 0 then Exit;

  { calculate Viewpoint }
  if TextureNode.FdViewpoint.Value is TViewpointMirrorNode then
    GetMatricesForViewpointMirror(
      TViewpointMirrorNode(TextureNode.FdViewpoint.Value),
      CameraMatrix, CameraRotationMatrix, NewProjectionMatrix)
  else
  if TextureNode.FdViewpoint.Value is TAbstractViewpointNode then
    GetMatricesForViewpoint(
      TAbstractViewpointNode(TextureNode.FdViewpoint.Value),
      CameraMatrix, CameraRotationMatrix, NewProjectionMatrix)
  else
    GetMatricesForViewpoint(
      CurrentViewpoint,
      CameraMatrix, CameraRotationMatrix, NewProjectionMatrix);

  TextureNode.EventProjection.Send(NewProjectionMatrix);
  TextureNode.EventViewing.Send(CameraMatrix);
  TextureNode.EventRendering.Send(true);

  Camera := TRenderingCamera.Create;
  try
    Camera.Target := rfRenderedTexture;
    Camera.FromMatrix(CameraMatrix, CameraRotationMatrix, NewProjectionMatrix);

    RenderToTexture.RenderBegin;

      RenderContext.Viewport := Rectangle(0, 0, Width, Height);

      SavedProjectionMatrix := RenderContext.ProjectionMatrix;
      RenderContext.ProjectionMatrix := NewProjectionMatrix;

      Render(Camera);

      RenderContext.ProjectionMatrix := SavedProjectionMatrix;

    RenderToTexture.RenderEnd;
  finally FreeAndNil(Camera) end;

  { depth maps (created by TextureDepth_IncReference, with GL_DEPTH_COMPONENT)
    cannot have mipmaps regenerated. On Mesa 7.6 (ubuntu 10.4, Mesa DRI Intel
    on "domek") glGenerateMipmapEXT even causes "invalid operation" OpenGL
    error (test e.g. view3dscene on rendered_texture.x3dv). }
  if NeedsMipmaps and (not DepthMap) then
    RenderToTexture.GenerateMipmap;

  TextureNode.EventRendering.Send(false);

  NeedsRestoreViewport := true;
end;

function TGLRenderedTextureNode.GLName: TGLTextureId;
begin
  Result := FGLName;
end;

{ TGLCubeMapTextureNode ------------------------------------------------------ }

function TGLCubeMapTextureNode.TextureNode: TAbstractEnvironmentTextureNode;
begin
  Result := TAbstractEnvironmentTextureNode(inherited TextureNode);
end;

procedure TGLCubeMapTextureNode.Unprepare;
begin
  if GLName <> 0 then
  begin
    Renderer.Cache.TextureCubeMap_DecReference(GLName);
    GLName := 0;
  end;
end;

function TGLCubeMapTextureNode.Bind(const TextureUnit: Cardinal): boolean;
begin
  Result := GLName <> 0;
  if not Result then Exit;

  Renderer.ActiveTexture(TextureUnit);
  glBindTexture(GL_TEXTURE_CUBE_MAP, GLName);
end;

function TGLCubeMapTextureNode.Enable(const TextureUnit: Cardinal;
  Shader: TShader; const Env: TTextureEnv): boolean;
begin
  Result := Bind(TextureUnit);
  if not Result then Exit;

  Shader.EnableTexture(TextureUnit, ttCubeMap, TextureNode, Env);
end;

{ TGLComposedCubeMapTextureNode ---------------------------------------------- }

class function TGLComposedCubeMapTextureNode.IsClassForTextureNode(
  ANode: TAbstractTextureNode): boolean;
begin
  Result := ANode is TComposedCubeMapTextureNode;
end;

function TGLComposedCubeMapTextureNode.TextureNode: TComposedCubeMapTextureNode;
begin
  Result := TComposedCubeMapTextureNode(inherited TextureNode);
end;

procedure TGLComposedCubeMapTextureNode.PrepareCore(State: TX3DGraphTraverseState);
var
  Filter: TTextureFilter;
  Anisotropy: TGLfloat;
  BackRot, FrontRot, LeftRot, RightRot: TCastleImage;
  GUITexture: boolean;
begin
  if GLFeatures.TextureCubeMap = gsNone then
  begin
    WritelnWarning('VRML/X3D', 'Your OpenGL doesn''t support ARB_texture_cube_map, cannot use CubeMapTexture');
    Exit;
  end;

  if not TextureNode.LoadSides then
  begin
    WritelnWarning('VRML/X3D', 'Not all sides of a CubeMapTexture are correctly set and loaded, cannot use cube map');
    Exit;
  end;

  HandleTextureProperties(TextureNode.TextureProperties,
    Filter, Anisotropy, GUITexture);

  try
    { To match expected orientation for OpenGL, we have to rotate images.
      (source images are oriented as for VRML Background.)
      We safely cast them to TCastleImage below, SideLoaded above checked
      that they are indeed of TCastleImage class. }
    BackRot  := (TAbstractTexture2DNode(TextureNode.FdBack .Value).TextureImage as TCastleImage).MakeRotated(2);
    FrontRot := (TAbstractTexture2DNode(TextureNode.FdFront.Value).TextureImage as TCastleImage).MakeRotated(2);
    LeftRot  := (TAbstractTexture2DNode(TextureNode.FdLeft .Value).TextureImage as TCastleImage).MakeRotated(2);
    RightRot := (TAbstractTexture2DNode(TextureNode.FdRight.Value).TextureImage as TCastleImage).MakeRotated(2);

    SetTextureRGBFromImage(RightRot);

    GLName := Renderer.Cache.TextureCubeMap_IncReference(
      TextureNode,
      Filter, Anisotropy,
      { positive x } RightRot,
      { negative x } LeftRot,
      { positive y } TAbstractTexture2DNode(TextureNode.FdTop   .Value).TextureImage as TCastleImage,
      { negative y } TAbstractTexture2DNode(TextureNode.FdBottom.Value).TextureImage as TCastleImage,
      { positive z } BackRot,
      { negative z } FrontRot,
      nil);
  finally
    FreeAndNil(BackRot);
    FreeAndNil(FrontRot);
    FreeAndNil(LeftRot);
    FreeAndNil(RightRot);
  end;
end;

{ TGLImageCubeMapTextureNode ------------------------------------------------- }

class function TGLImageCubeMapTextureNode.IsClassForTextureNode(
  ANode: TAbstractTextureNode): boolean;
begin
  Result := ANode is TImageCubeMapTextureNode;
end;

function TGLImageCubeMapTextureNode.TextureNode: TImageCubeMapTextureNode;
begin
  Result := TImageCubeMapTextureNode(inherited TextureNode);
end;

procedure TGLImageCubeMapTextureNode.PrepareCore(State: TX3DGraphTraverseState);
var
  Filter: TTextureFilter;
  Anisotropy: TGLfloat;
  GUITexture: boolean;
  Composite: TCompositeImage;
begin
  if GLFeatures.TextureCubeMap = gsNone then
  begin
    WritelnWarning('VRML/X3D', 'Your OpenGL doesn''t support ARB_texture_cube_map, cannot use CubeMapTexture');
    Exit;
  end;

  Composite := TextureNode.LoadImage;
  { If TextureNode doesn't contain anything useful, just exit.
    TextureNode.LoadImage already did necessary WritelnWarnings. }
  if Composite = nil then Exit;

  try

    HandleTextureProperties(TextureNode.TextureProperties,
      Filter, Anisotropy, GUITexture);

    { TODO: this is a quick and dirty method:
      - We call LoadImage each time, while load calls should
        be minimized (to avoid loading image many times, but also
        to avoid making repeated warnings in case image fails).
        Should be cached, like for 2D texture nodes.
      - We do not use cube map mipmaps stored inside Composite file.
    }

    SetTextureRGBFromImage(Composite.CubeMapImage(csPositiveX));

    GLName := Renderer.Cache.TextureCubeMap_IncReference(
      TextureNode,
      Filter, Anisotropy,
      Composite.CubeMapImage(csPositiveX),
      Composite.CubeMapImage(csNegativeX),
      Composite.CubeMapImage(csPositiveY),
      Composite.CubeMapImage(csNegativeY),
      Composite.CubeMapImage(csPositiveZ),
      Composite.CubeMapImage(csNegativeZ),
      Composite);
  finally FreeAndNil(Composite); end;
end;

{ TGLGeneratedCubeMapTextureNode --------------------------------------------- }

class function TGLGeneratedCubeMapTextureNode.IsClassForTextureNode(
  ANode: TAbstractTextureNode): boolean;
begin
  Result := ANode is TGeneratedCubeMapTextureNode;
end;

function TGLGeneratedCubeMapTextureNode.TextureNode: TGeneratedCubeMapTextureNode;
begin
  Result := TGeneratedCubeMapTextureNode(inherited TextureNode);
end;

procedure TGLGeneratedCubeMapTextureNode.PrepareCore(State: TX3DGraphTraverseState);
var
  Filter: TTextureFilter;
  Anisotropy: TGLfloat;
  GUITexture: boolean;
  InitialImage: TCastleImage;
begin
  if GLFeatures.TextureCubeMap = gsNone then
  begin
    WritelnWarning('VRML/X3D', 'Your OpenGL doesn''t support ARB_texture_cube_map, cannot use CubeMapTexture');
    Exit;
  end;

  HandleTextureProperties(TextureNode.TextureProperties,
    Filter, Anisotropy, GUITexture);

  { calculate Filter, Anisotropy, NeedsMipmaps }
  NeedsMipmaps := Filter.NeedsMipmaps;
  if NeedsMipmaps and not HasGenerateMipmap then
  begin
    WritelnWarning('VRML/X3D' { This may be caused by OpenGL implementation
      limits, so it may be impossible to predict by VRML author,
      so it's "ignorable" warning. },
      'OpenGL implementation doesn''t allow any glGenerateMipmap* version, so you cannot use mipmaps for GeneratedCubeMapTexture');
    Filter.Minification := minLinear;
    NeedsMipmaps := false;
  end;

  { calculate Size }
  Size := Max(TextureNode.FdSize.Value, 0);
  if not IsCubeMapTextureSized(Size) then
  begin
    Size := ResizeToCubeMapTextureSize(Size);
    WritelnWarning('VRML/X3D' { This may be caused by OpenGL implementation
      limits, so it may be impossible to predict by VRML author,
      so it's "ignorable" warning. },
      Format('Cube map texture size %d is incorrect (cube map texture size must be a power of two, > 0 and <= GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB = %d), corrected to %d',
        [ TextureNode.FdSize.Value, GLFeatures.MaxCubeMapTextureSize, Size]));
  end;

  InitialImage := TRGBImage.Create(Size, Size);
  try
    InitialImage.URL := 'generated:/' + TextureNode.NiceName;

    { Fill with deliberately stupid (but constant) color,
      to recognize easily GeneratedCubeMapTexture which don't have textures
      updated. }
    if not GLVersion.BuggyGenerateCubeMap then
      InitialImage.Clear(Vector4Byte(255, 0, 255, 255))
    else
      InitialImage.Clear(Vector4Byte(237, 237, 237, 255)); { when buggy, use some reasonable color }

    SetTextureRGBFromImage(InitialImage);

    GLName := Renderer.Cache.TextureCubeMap_IncReference(
      TextureNode,
      Filter, Anisotropy,
      InitialImage, InitialImage,
      InitialImage, InitialImage,
      InitialImage, InitialImage,
      nil);
  finally FreeAndNil(InitialImage) end;

  RenderToTexture := TGLRenderToTexture.Create(Size, Size);
  RenderToTexture.SetTexture(GLName, GL_TEXTURE_CUBE_MAP_POSITIVE_X);
  RenderToTexture.GLContextOpen;

  { Workaround for NVidia GeForce FX 5200 bug:
    (Confirmed it's needed on Linux, both 32 and 64bit, Kambi's "kocury".
     Confirmed it's *not* needed on Radeon (Linux 32bit, fglrx, Kambi's chantal).)

    Although TextureCubeMap_IncReference (glTextureCubeMap inside) already
    called initial GenerateMipmap (for our InitialImage), it's not enough.
    It seems that assigning texture to FBO destroys it's mipmaps (at least
    their contents).
    So you have to call GenerateMipmap *after* RenderToTexture.GLContextInit
    before showing this texture, to recreate mipmaps.

    To see bug, comment below, and run view3scene on
    demo_models/x3d/cubemap_generated_recursive.x3dv
    Before pressing [space] (which triggers Update method), teapots
    (or any other objects, reproducible with various IndexedFaceSets)
    will have seemingly random mipmaps (base level 0 is Ok, RGB(255,0,255),
    but mipmaps are seemingly filled with random garbage).
    Line below fixes it at negligible cost (we'll generate mipmaps at loading
    one more time than necessary). Other fix that works is to move
    RenderToTexture.GLContextOpen to first Update call, but this obfuscates code. }
  if (GLVersion.VendorType = gvNvidia) and NeedsMipmaps then
  begin
    RenderToTexture.CompleteTextureTarget := GL_TEXTURE_CUBE_MAP;
    RenderToTexture.GenerateMipmap;
  end;
end;

procedure TGLGeneratedCubeMapTextureNode.Unprepare;
begin
  FreeAndNil(RenderToTexture);
  inherited;
end;

procedure TGLGeneratedCubeMapTextureNode.Update(
  const Render: TRenderFromViewFunction;
  const ProjectionNear, ProjectionFar: Single;
  var NeedsRestoreViewport: boolean;
  const CubeMiddle: TVector3);
begin
  if GLName = 0 then Exit;

  if GLVersion.BuggyGenerateCubeMap then Exit;

  GLCaptureCubeMapTexture(GLName, Size,
    CubeMiddle,
    Render, ProjectionNear, ProjectionFar,
    RenderToTexture);

  if NeedsMipmaps then
    RenderToTexture.GenerateMipmap;

  NeedsRestoreViewport := true;
end;

{ TGL3DTextureNode ----------------------------------------------------------- }

class function TGL3DTextureNode.IsClassForTextureNode(
  ANode: TAbstractTextureNode): boolean;
begin
  Result := ANode is TAbstractTexture3DNode;
end;

function TGL3DTextureNode.TextureNode: TAbstractTexture3DNode;
begin
  Result := TAbstractTexture3DNode(inherited TextureNode);
end;

procedure TGL3DTextureNode.PrepareCore(State: TX3DGraphTraverseState);
var
  Filter: TTextureFilter;
  Anisotropy: TGLfloat;
  GUITexture: boolean;
  TextureWrap: TTextureWrap3D;
begin
  if GLFeatures.Texture3D = gsNone then
  begin
    WritelnWarning('VRML/X3D', 'Your OpenGL doesn''t support 3D textures, cannot use Texture3D nodes');
    Exit;
  end;

  TextureNode.TextureLoaded := true;

  { If TextureImage doesn't contain anything useful, just exit.
    Setting TextureLoaded already did necessary WritelnWarnings. }
  if TextureNode.TextureImage = nil then Exit;

  HandleTextureProperties(TextureNode.TextureProperties,
    Filter, Anisotropy, GUITexture);

  { calculate TextureWrap }
  TextureWrap.Data[0] := TextureRepeatToGL(TextureNode.FdRepeatS.Value);
  TextureWrap.Data[1] := TextureRepeatToGL(TextureNode.FdRepeatT.Value);
  TextureWrap.Data[2] := TextureRepeatToGL(TextureNode.FdRepeatR.Value);

  SetTextureRGBFromImage(TextureNode.TextureImage);

  GLName := Renderer.Cache.Texture3D_IncReference(
    TextureNode, Filter, Anisotropy,
    TextureWrap, TextureNode.TextureImage, TextureNode.TextureComposite);
end;

procedure TGL3DTextureNode.Unprepare;
begin
  if GLName <> 0 then
  begin
    Renderer.Cache.Texture3D_DecReference(GLName);
    GLName := 0;
  end;
end;

function TGL3DTextureNode.Bind(const TextureUnit: Cardinal): boolean;
begin
  Result := GLName <> 0;
  if not Result then Exit;

  Renderer.ActiveTexture(TextureUnit);
  {$ifndef OpenGLES} // TODO-OpenGLES3 (3D textures are only available in OpenGLES3)
  glBindTexture(GL_TEXTURE_3D, GLName);
  {$endif}
end;

function TGL3DTextureNode.Enable(const TextureUnit: Cardinal;
  Shader: TShader; const Env: TTextureEnv): boolean;
begin
  Result := Bind(TextureUnit);
  if not Result then Exit;

  Shader.EnableTexture(TextureUnit, tt3D, TextureNode, Env);
end;

{ TGLGeneratedShadowMap ------------------------------------------------------ }

class function TGLGeneratedShadowMap.IsClassForTextureNode(
  ANode: TAbstractTextureNode): boolean;
begin
  Result := ANode is TGeneratedShadowMapNode;
end;

function TGLGeneratedShadowMap.TextureNode: TGeneratedShadowMapNode;
begin
  Result := TGeneratedShadowMapNode(inherited TextureNode);
end;

class function TGLGeneratedShadowMap.ClassVarianceShadowMaps(
  Attributes: TRenderingAttributes): boolean;
begin
  Result :=
    (Attributes.ShadowSampling = ssVarianceShadowMaps) and
    (GLFeatures.Shaders <> gsNone) and
    GLFeatures.TextureFloat;
end;

procedure TGLGeneratedShadowMap.PrepareCore(State: TX3DGraphTraverseState);
var
  TextureWrap: TTextureWrap2D;
  Filter: TTextureFilter;
begin
  { Float texture extensions: there is also NV_float_buffer,
    which is the only way for a float texture on old nvidia GPUs.
    But it's only for NV_texture_rectangle, without bilinear filtering
    or mipmapping, so it's not useful for VSM at all. }

  VarianceShadowMaps := ClassVarianceShadowMaps(Renderer.Attributes);

  if Log then
    WritelnLog('Shadows', Format('Variance Shadow Maps used: %s (Reasons: Attributes.Shadow sampling = %s, GLSL support = %s, texture_float support = %s)',
      [ BoolToStr(VarianceShadowMaps, true),
        ShadowSamplingNames[Renderer.Attributes.ShadowSampling],
        GLSupportNames[GLFeatures.Shaders],
        BoolToStr(GLFeatures.TextureFloat, true) ]));

  { TODO: fix TextureNode.FdSize.Value if needed }
  Size := TextureNode.FdSize.Value;

  VisualizeDepthMap := (TextureNode.FdCompareMode.Value = 'NONE') or
    Renderer.Attributes.VisualizeDepthMap;

  { At least in case of float textures for VSM, CLAMP_TO_EDGE is needed
    (instead of standard clamp to border). Didn't see any difference
    for depth textures. }
  TextureWrap.Data[0] := GLFeatures.CLAMP_TO_EDGE;
  TextureWrap.Data[1] := GLFeatures.CLAMP_TO_EDGE;

  { calculate Filter, NeedsMipmaps }
  Filter.Magnification := magLinear;
  if VarianceShadowMaps and HasGenerateMipmap then
  begin
    NeedsMipmaps := true;
    Filter.Minification := minLinearMipmapLinear;
  end else
  begin
    NeedsMipmaps := false;
    Filter.Minification := minLinear;
  end;

  if VarianceShadowMaps then
  begin
    FGLName := Renderer.Cache.TextureFloat_IncReference(
      TextureNode, Filter, TextureWrap, Size, Size, true);
  end else
  begin
    if not GLFeatures.TextureDepth then
    begin
      WritelnWarning('VRML/X3D', 'Your graphic card doesn''t support depth textures, cannot use GeneratedShadowMap nodes');
      Exit;
    end;

    FGLName := Renderer.Cache.TextureDepth_IncReference(
      TextureNode, TextureWrap, TextureNode.CompareMode, Size, Size,
      VisualizeDepthMap);
  end;

  RenderToTexture := TGLRenderToTexture.Create(Size, Size);
  RenderToTexture.SetTexture(FGLName, GL_TEXTURE_2D);
  if VarianceShadowMaps then
    RenderToTexture.Buffer := tbColor else
    RenderToTexture.Buffer := tbDepth;
  RenderToTexture.Stencil := false;
  RenderToTexture.GLContextOpen;
end;

procedure TGLGeneratedShadowMap.Unprepare;
begin
  FreeAndNil(RenderToTexture);

  if FGLName <> 0 then
  begin
    if VarianceShadowMaps then
      Renderer.Cache.TextureFloat_DecReference(FGLName) else
      Renderer.Cache.TextureDepth_DecReference(FGLName);
    FGLName := 0;
  end;
end;

function TGLGeneratedShadowMap.Bind(const TextureUnit: Cardinal): boolean;
begin
  Result := FGLName <> 0;
  if not Result then Exit;

  Renderer.ActiveTexture(TextureUnit);
  glBindTexture(GL_TEXTURE_2D, FGLName);
end;

function TGLGeneratedShadowMap.Enable(const TextureUnit: Cardinal;
  Shader: TShader; const Env: TTextureEnv): boolean;
var
  TextureType: TTextureType;
begin
  Result := Bind(TextureUnit);
  if not Result then Exit;

  if VisualizeDepthMap then
    TextureType := tt2D
  else
    TextureType := tt2DShadow;

  Shader.EnableTexture(TextureUnit,
    TextureType, TextureNode, Env, Size, TextureNode.Light);
end;

procedure TGLGeneratedShadowMap.Update(
  const Render: TRenderFromViewFunction;
  const ProjectionNear, ProjectionFar: Single;
  var NeedsRestoreViewport: boolean;
  Light: TAbstractLightNode);
var
  NewProjectionMatrix, SavedProjectionMatrix: TMatrix4;
  SavedDepthRange: TDepthRange;
  Camera: TRenderingCamera;
begin
  if FGLName = 0 then Exit;

  { Render view for shadow map }
  NewProjectionMatrix := Light.ProjectionMatrix;

  Camera := TRenderingCamera.Create;
  try
    if VarianceShadowMaps then
      Camera.Target := rtVarianceShadowMap
    else
      Camera.Target := rtShadowMap;

    Camera.FromMatrix(
      Light.ModelviewMatrix,
      Light.ModelviewRotationMatrix,
      NewProjectionMatrix);

    RenderToTexture.RenderBegin;

      RenderContext.Viewport := Rectangle(0, 0, Size, Size);

      SavedProjectionMatrix := RenderContext.ProjectionMatrix;
      RenderContext.ProjectionMatrix := NewProjectionMatrix;

        { fix RenderContext.DepthRange in case was changed by TPlayer.RenderOnTop }
        SavedDepthRange := RenderContext.DepthRange;
        RenderContext.DepthRange := drFull;

        {$ifndef OpenGLES} // TODO-es
        glPushAttrib(GL_POLYGON_BIT);
          { enable polygon offset for everything (whole scene).

            VarianceShadowMaps notes: can offset be avoided in this case?
            Practice (on "ATI Mobility Radeon HD 4300 Series") shows that
            offset is still needed for VSM. Without offset, VSM has some noise,
            and sunny_street demos have noise on walls. With offset, VSM is perfect. }
          glEnable(GL_POLYGON_OFFSET_FILL); { saved by GL_POLYGON_BIT }
          glEnable(GL_POLYGON_OFFSET_LINE); { saved by GL_POLYGON_BIT }
          glEnable(GL_POLYGON_OFFSET_POINT); { saved by GL_POLYGON_BIT }
          glPolygonOffset(TextureNode.FdScale.Value, TextureNode.FdBias.Value); { saved by GL_POLYGON_BIT }
        {$endif}

          Render(Camera);

        {$ifndef OpenGLES} // TODO-es
        glPopAttrib;
        {$endif}

        RenderContext.DepthRange := SavedDepthRange;

      RenderContext.ProjectionMatrix := SavedProjectionMatrix;

    RenderToTexture.RenderEnd;

  finally FreeAndNil(Camera) end;

  if NeedsMipmaps then
    RenderToTexture.GenerateMipmap;

  NeedsRestoreViewport := true;
end;

function TGLGeneratedShadowMap.GLName: TGLTextureId;
begin
  Result := FGLName;
end;

{ TGLShaderTexture ------------------------------------------------------ }

class function TGLShaderTexture.IsClassForTextureNode(
  ANode: TAbstractTextureNode): boolean;
begin
  Result := ANode is TShaderTextureNode;
end;

procedure TGLShaderTexture.PrepareCore(State: TX3DGraphTraverseState);
begin
  { no need to do anything }
end;

procedure TGLShaderTexture.Unprepare;
begin
  { no need to do anything }
end;

function TGLShaderTexture.Bind(const TextureUnit: Cardinal): boolean;
begin
  Result := true;
  { no need to actually do anything }

  { TODO: ShaderTexture should not need to increase the TextureUnit.
    It's not loaded to OpenGL, so it doesn't take the multitexture slot.
    For now we increase it, only to pass texture coords to it. }
end;

function TGLShaderTexture.Enable(const TextureUnit: Cardinal;
  Shader: TShader; const Env: TTextureEnv): boolean;
begin
  Result := Bind(TextureUnit);
  if not Result then Exit;

  Shader.EnableTexture(TextureUnit, ttShader, TextureNode, Env);
end;

{$endif read_implementation}
