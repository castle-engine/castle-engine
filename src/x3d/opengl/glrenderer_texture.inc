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
    procedure HandleTexturePropertiesCore(
      TextureProperties: TTexturePropertiesNode;
      out MinFilter, MagFilter: TGLint;
      out Anisotropy: TGLfloat);

    { Calculate things from TextureProperties node.
      If TextureProperties = @nil or not of TTexturePropertiesNode class,
      they are taken from defaults (possibly in Attributes).

      This is useful when interpreting VRML/X3D files,
      as you have no guarantee user didn't place there some disallowed node
      in "textureProperties" field. }
    procedure HandleTextureProperties(
      TextureProperties: TX3DNode;
      out MinFilter, MagFilter: TGLint;
      out Anisotropy: TGLfloat);

    { Decide if this class can handle given texture Node. }
    class function IsClassForTextureNode(
      ANode: TAbstractTextureNode): boolean; virtual; abstract;

    { Do the time-consuming preparations before rendering this texture.
      Called from TGLRenderer.Prepare, always through our
      Prepare method. Our Prepare method takes care to catch
      common exceptions from this (EFramebufferError, ETextureLoadError),
      convert them to OnWarning and make Unprepare to undo the preparations.
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
    { This is the alpha channel of this texture.
      Used by TGLRenderer.PreparedTextureAlphaChannelType and bump mapping.
      Should be calculated and set in Prepare.

      For textures with normal images, this is the saved result of
      TImage.AlphaChannelType. For videos, saved result of
      TVideo.AlphaChannelType.

      By default, atNone in this class.

      TODO: it would be better to move this inside, possibly
      only to applicable class types? And expose only function
      like UseBlending, and this is the main use of
      TGLRenderer.PreparedTextureAlphaChannelType ? }
    AlphaChannelType: TAlphaChannelType;

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
      GL_TEXTURE_CUBE_MAP_ARB / GL_TEXTURE_3D) ). }
    function Enable(const TextureUnit: Cardinal;
      Shader: TShader; const Env: TTextureEnv): boolean; virtual; abstract;

    { Sets texture state for many texture units, based on this node.
      On every texture unit where something is enabled,
      proper texture identifier must be bound.
      Also, has to set complete glTexEnv on every enabled texture unit.

      TextureUnitsCount says how many texture units can be enabled/disabled.

      TextureUnitsCount does *not* take into account whether multitexturing
      OpenGL extensions are available at all.
      Look at GLUseMultiTexturing for this.
      Think of GLUseMultiTexturing as capping TextureUnitsCount to 1
      (still, remember to honour TextureUnitsCount = 0 case in your implementation,
      even when GLUseMultiTexturing = @true).

      You have to set TexCoordsNeeded, this is the count of texture units
      where some texture coordinates should be generated.
      This means that all texture units above TexCoordsNeeded
      (to TextureUnitsCount - 1) should be disabled by the caller (no need to
      do this in EnableAll),
      and there's no need to generated texture coords for them.

      (
      Yes, there is some small optimization missed in the definition
      of TexCoordsNeeded: if some textures in the middle of
      multitexture children list failed to load, but some following children
      succeded, we'll generate tex coords even for the useless texture units
      in the middle. We could avoid generating texture coords for them,
      by changing TexCoordsNeeded into bool array.
      This optimization is not considered worthy implementing for now.
      )

      You have to set texture state of all texture units < TexCoordsNeeded,
      and only on them. }
    procedure EnableAll(
      const TextureUnitsCount: Cardinal;
      out TexCoordsNeeded: Cardinal;
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

    { Prepare / unprepare all textures inside Node.InterfaceDeclarations
      SFNode / MFNode fields. }
    procedure PrepareInterfaceDeclarationsTextures(const ANode: TX3DNode;
      const State: TX3DGraphTraverseState;
      const Renderer: TGLRenderer);
    procedure UnprepareInterfaceDeclarationsTextures(const ANode: TX3DNode;
      const Renderer: TGLRenderer);
  end;

  { Common class for all single (not multi-texture) texture nodes.

    Implements EnableAll method, by calling @link(Enable) call.
    Override only @link(Enable) in descendants. }
  TGLSingleTextureNode = class(TGLTextureNode)
  protected
    { Determines should texture mode be suited for RGB and grayscale texture.
      Default false, you should set this in descendant
      (latest possibility is to set this is a successfull Enable call,
      but usually you want to do it in a Prepare call).
      You can set it by SetTextureRGBFromImage. }
    TextureRGB: boolean;
    procedure SetTextureRGBFromImage(Image: TEncodedImage);
  public
    procedure EnableAll(const TextureUnitsCount: Cardinal;
      out TexCoordsNeeded: Cardinal; Shader: TShader); override;
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
      out TexCoordsNeeded: Cardinal; Shader: TShader); override;
  end;

  { Handler for TAbstractTexture2DNode with image (not a video). }
  TGLImageTextureNode = class(TGLSingleTextureNode)
  protected
    class function IsClassForTextureNode(ANode: TAbstractTextureNode): boolean; override;
    procedure PrepareCore(State: TX3DGraphTraverseState); override;
  public
    GLName: TGLuint;
    NormalMap, HeightMap: TGLuint;
    HeightMapScale: Single;

    function TextureNode: TAbstractTexture2DNode;

    procedure Unprepare; override;
    function Bind(const TextureUnit: Cardinal): boolean; override;
    function Enable(const TextureUnit: Cardinal;
      Shader: TShader; const Env: TTextureEnv): boolean; override;
  end;

  TGLMovieTextureNode = class(TGLSingleTextureNode)
  protected
    class function IsClassForTextureNode(ANode: TAbstractTextureNode): boolean; override;
    procedure PrepareCore(State: TX3DGraphTraverseState); override;
  public
    GLVideo: TGLVideo;

    function TextureNode: TMovieTextureNode;

    procedure Unprepare; override;
    function Bind(const TextureUnit: Cardinal): boolean; override;
    function Enable(const TextureUnit: Cardinal;
      Shader: TShader; const Env: TTextureEnv): boolean; override;
  end;

  TGLRenderedTextureNode = class(TGLSingleTextureNode)
  private
    RenderToTexture: TGLRenderToTexture;
  protected
    class function IsClassForTextureNode(ANode: TAbstractTextureNode): boolean; override;
    procedure PrepareCore(State: TX3DGraphTraverseState); override;
  public
    GLName: TGLuint;

    { The actual decided image size and mipmap status. }
    Width, Height: Cardinal;
    NeedsMipmaps: boolean;
    DepthMap: boolean;

    function TextureNode: TRenderedTextureNode;

    procedure Unprepare; override;
    function Bind(const TextureUnit: Cardinal): boolean; override;
    function Enable(const TextureUnit: Cardinal;
      Shader: TShader; const Env: TTextureEnv): boolean; override;

    procedure Update(
      const Render: TRenderFromViewFunction;
      const ProjectionNear, ProjectionFar: Single;
      var NeedsRestoreViewport: boolean;
      CurrentViewpoint: TAbstractViewpointNode;
      CameraViewKnown: boolean;
      const CameraPosition, CameraDirection, CameraUp: TVector3Single);
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

    { This says if MinFilter needs mipmaps. }
    NeedsMipmaps: boolean;

    function TextureNode: TGeneratedCubeMapTextureNode;

    procedure Unprepare; override;

    procedure Update(
      const Render: TRenderFromViewFunction;
      const ProjectionNear, ProjectionFar: Single;
      var NeedsRestoreViewport: boolean;
      const CubeMiddle: TVector3Single);
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

  TGLGeneratedShadowMap = class(TGLSingleTextureNode)
  private
    RenderToTexture: TGLRenderToTexture;
    { VarianceShadowMaps calculated at the PrepareCore time. }
    VarianceShadowMaps: boolean;
    NeedsMipmaps: boolean;
  protected
    class function IsClassForTextureNode(ANode: TAbstractTextureNode): boolean; override;
    procedure PrepareCore(State: TX3DGraphTraverseState); override;
  public
    GLName: TGLuint;

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
    Result := CastleGL_CLAMP_TO_EDGE;
end;

{ TGLTextureNode ------------------------------------------------------------- }

constructor TGLTextureNode.Create(ARenderer: TGLRenderer; ANode: TX3DNode);
begin
  Assert(ANode is TAbstractTextureNode, 'TGLTextureNode.Create acceps as Node only TAbstractTextureNode');
  inherited;
end;

procedure TGLTextureNode.HandleTexturePropertiesCore(
  TextureProperties: TTexturePropertiesNode;
  out MinFilter, MagFilter: TGLint;
  out Anisotropy: TGLfloat);

  function StrToMinFilter(S: string): TGLint;
  begin
    S := UpperCase(S);

    if S = 'AVG_PIXEL' then Result := GL_LINEAR else
    if S = 'AVG_PIXEL_AVG_MIPMAP' then Result := GL_LINEAR_MIPMAP_LINEAR else
    if S = 'AVG_PIXEL_NEAREST_MIPMAP' then Result := GL_LINEAR_MIPMAP_NEAREST else
    if S = 'NEAREST_PIXEL_AVG_MIPMAP' then Result := GL_NEAREST_MIPMAP_LINEAR else
    if S = 'NEAREST_PIXEL_NEAREST_MIPMAP' then Result := GL_NEAREST_MIPMAP_NEAREST else
    if S = 'NEAREST_PIXEL' then Result := GL_NEAREST else

    if S = 'DEFAULT' then Result := Renderer.Attributes.TextureMinFilter else

    if S = 'FASTEST' then Result := GL_NEAREST else
    if S = 'NICEST' then Result := GL_LINEAR_MIPMAP_LINEAR else

    if S = 'NEAREST' then
    begin
      OnWarning(wtMajor, 'VRML/X3D', Format('"%s" is not allowed texture minification, this is an Avalon-only extension, please fix to "NEAREST_PIXEL"', [S]));
      Result := GL_NEAREST;
    end else

    if S = 'LINEAR' then
    begin
      OnWarning(wtMajor, 'VRML/X3D', Format('"%s" is not allowed texture minification, this is an Avalon-only extension, please fix to "AVG_PIXEL"', [S]));
      Result := GL_LINEAR;
    end else

    begin
      Result := Renderer.Attributes.TextureMinFilter;
      OnWarning(wtMajor, 'VRML/X3D', Format('Unknown texture minification filter "%s"', [S]));
    end;
  end;

  function StrToMagFilter(S: string): TGLint;
  begin
    S := UpperCase(S);

    if S = 'AVG_PIXEL' then Result := GL_LINEAR else
    if S = 'NEAREST_PIXEL' then Result := GL_NEAREST else

    if S = 'DEFAULT' then Result := Renderer.Attributes.TextureMagFilter else

    if S = 'FASTEST' then Result := GL_NEAREST else
    if S = 'NICEST' then Result := GL_LINEAR else

    if S = 'NEAREST' then
    begin
      OnWarning(wtMajor, 'VRML/X3D', Format('"%s" is not allowed texture magnification, this is an Avalon-only extension, please fix to "NEAREST_PIXEL"', [S]));
      Result := GL_NEAREST;
    end else

    if S = 'LINEAR' then
    begin
      OnWarning(wtMajor, 'VRML/X3D', Format('"%s" is not allowed texture magnification, this is an Avalon-only extension, please fix to "AVG_PIXEL"', [S]));
      Result := GL_LINEAR;
    end else

    begin
      Result := Renderer.Attributes.TextureMagFilter;
      OnWarning(wtMajor, 'VRML/X3D', Format('Unknown texture minification filter "%s"', [S]));
    end;
  end;

begin { HandleTextureProperties }
  if TextureProperties <> nil then
  begin
    MinFilter := StrToMinFilter(TextureProperties.FdMinificationFilter.Value);
    MagFilter := StrToMagFilter(TextureProperties.FdMagnificationFilter.Value);
    Anisotropy := TextureProperties.FdAnisotropicDegree.Value;
  end else
  begin
    MinFilter := Renderer.Attributes.TextureMinFilter;
    MagFilter := Renderer.Attributes.TextureMagFilter;
    Anisotropy := 1;
  end;
end { HandleTextureProperties };

procedure TGLTextureNode.HandleTextureProperties(
  TextureProperties: TX3DNode;
  out MinFilter, MagFilter: TGLint;
  out Anisotropy: TGLfloat);
begin
  if (TextureProperties = nil) or
     not (TextureProperties is TTexturePropertiesNode) then
    HandleTexturePropertiesCore(nil, MinFilter, MagFilter, Anisotropy) else
    HandleTexturePropertiesCore(TTexturePropertiesNode(TextureProperties),
      MinFilter, MagFilter, Anisotropy);
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
      OnWarning(wtMinor, 'VRML/X3D', 'Framebuffer error, generated texture not possible: '
        + E.Message);
    end;

    on E: ETextureLoadError do
    begin
      Unprepare;
      OnWarning(wtMinor, 'VRML/X3D', 'Cannot load texture to OpenGL: ' + E.Message);
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

procedure TGLTextureNodes.PrepareInterfaceDeclarationsTextures(
  const ANode: TX3DNode;
  const State: TX3DGraphTraverseState; const Renderer: TGLRenderer);

  { If TextureNode <> @nil and is a texture node, prepare it. }
  procedure PrepareTexture(TextureNode: TX3DNode);
  begin
    if (TextureNode <> nil) and
       (TextureNode is TAbstractTextureNode) then
      Prepare(State, TAbstractTextureNode(TextureNode), Renderer);
  end;

var
  I, J: Integer;
  UniformField: TX3DField;
  IDecls: TX3DInterfaceDeclarationList;
begin
  IDecls := ANode.InterfaceDeclarations;
  if IDecls = nil then Exit; { ignore nodes without interface declararations }

  for I := 0 to IDecls.Count - 1 do
  begin
    UniformField := IDecls.Items[I].Field;

    if UniformField <> nil then
    begin
      if UniformField is TSFNode then
      begin
        PrepareTexture(TSFNode(UniformField).Value);
      end else
      if UniformField is TMFNode then
      begin
        for J := 0 to TMFNode(UniformField).Count - 1 do
          PrepareTexture(TMFNode(UniformField)[J]);
      end;
    end;
  end;
end;

procedure TGLTextureNodes.UnprepareInterfaceDeclarationsTextures(
  const ANode: TX3DNode; const Renderer: TGLRenderer);

  { If TextureNode <> @nil and is a texture node, prepare it. }
  procedure UnprepareTexture(TextureNode: TX3DNode);
  begin
    if (TextureNode <> nil) and
       (TextureNode is TAbstractTextureNode) then
      Renderer.GLTextureNodes.Unprepare(TAbstractTextureNode(TextureNode));
  end;

var
  I, J: Integer;
  UniformField: TX3DField;
  IDecls: TX3DInterfaceDeclarationList;
begin
  IDecls := ANode.InterfaceDeclarations;
  if IDecls = nil then Exit; { ignore nodes without interface declararations }

  for I := 0 to IDecls.Count - 1 do
  begin
    UniformField := IDecls.Items[I].Field;

    if UniformField <> nil then
    begin
      if UniformField is TSFNode then
      begin
        UnprepareTexture(TSFNode(UniformField).Value);
      end else
      if UniformField is TMFNode then
      begin
        for J := 0 to TMFNode(UniformField).Count - 1 do
          UnprepareTexture(TMFNode(UniformField)[J]);
      end;
    end;
  end;
end;

{ TGLSingleTextureNode ------------------------------------------------------- }

procedure TGLSingleTextureNode.SetTextureRGBFromImage(Image: TEncodedImage);
begin
  TextureRGB :=
    (Image is TS3TCImage) or
    (Image is TRGBImage) or
    (Image is TRGBAlphaImage) or
    (Image is TRGBFloatImage);
end;

procedure TGLSingleTextureNode.EnableAll(const TextureUnitsCount: Cardinal;
  out TexCoordsNeeded: Cardinal; Shader: TShader);
var
  Env: TTextureEnv;
begin
  TexCoordsNeeded := 0;

  { initialize Env using Attributes.TextureModeRGB/Grayscale }
  if TextureRGB then
    Env.Init(Renderer.Attributes.TextureModeRGB) else
    Env.Init(Renderer.Attributes.TextureModeGrayscale);

  if (TextureUnitsCount > 0) and
     Enable(0, Shader, Env) then
  begin
    { Here we know that Env was initialized above.
      So we know that mode is simple, the same for rgb and alpha,
      and can be set using GL_TEXTURE_ENV_MODE
      (doesn't require using GL_COMBINE). }
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, Env.Combine[cRGB]);
    TexCoordsNeeded := 1;
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

  procedure MaxTo1st(var A: TAlphaChannelType; const B: TAlphaChannelType);
  begin
    if B > A then A := B;
  end;

var
  ChildTex: TX3DNode;
  ChildPrepared: TGLTextureNode;
  I: Integer;
begin
  { Prepare all children.

    Also, calculate our AlphaChannelType:
    - if any texture has atFullRange, then we're atFullRange,
    - otherwise, if any texture has atSimpleYesNo, then we're atSimpleYesNo,
    - otherwise, we're atNone

    Thanks to how order of TAlphaChannelType is defined,
    this is equivalent to simple "our AlphaChannelType is maximum
    AlphaChannelType of our children".
  }

  AlphaChannelType := atNone; { default value, but set for paranoia anyway }

  { fill PreparedTextureChildren along the way.
    This will be used in Unprepare. }
  PreparedTextureChildren.Count := 0;

  for I := 0 to TextureNode.FdTexture.Count - 1 do
  begin
    ChildTex := TextureNode.FdTexture[I];
    if (ChildTex <> nil) and
       (ChildTex is TAbstractTextureNode) then
    begin
      if ChildTex is TMultiTextureNode then
        OnWarning(wtMajor, 'VRML/X3D', 'Child of MultiTexture node cannot be another MultiTexture node') else
      begin
        PreparedTextureChildren.Add(ChildTex);
        ChildPrepared := Renderer.GLTextureNodes.Prepare(State, TAbstractTextureNode(ChildTex), Renderer);
        if ChildPrepared <> nil then
          MaxTo1st(AlphaChannelType, ChildPrepared.AlphaChannelType);
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
    GLRenderer explicitly allows to free vrml nodes before
    unpreparing them. This is used e.g. by doing TCastleSceneCore.Load
    (that calls ChangedAll, that calls Unprepare, only at the end,
    *after* old nodes have been already freed.)

    Accessing here TextureNode.FdTexture.Count e.g. crashes on
    multiple_viewports: open models/bridge_final.x3dv, then open
    models/boxes.x3dv. (Note that multiple_viewports loads new scene
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
  out TexCoordsNeeded: Cardinal; Shader: TShader);
const
  ColorSourceGL: array [TColorSource] of TGLint =
  ( GL_PRIMARY_COLOR, GL_TEXTURE, GL_CONSTANT, GL_PREVIOUS );
  Argument: array [ta0..ta1] of Integer = ( 0, 1 );
var
  ChildTex: TX3DNode;
  I: Integer;
  Success: boolean;
  Env: TTextureEnv;
  ModeStr, SourceStr: string;
begin
  { calculate TexCoordsNeeded }
  TexCoordsNeeded := Min(TextureUnitsCount, TextureNode.FdTexture.Count);
  if not GLUseMultiTexturing then
    MinTo1st(TexCoordsNeeded, 1);

  for I := 0 to TexCoordsNeeded - 1 do
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
      Env.Init(ModeStr, SourceStr);

      if ChildTex is TMultiTextureNode then
        OnWarning(wtMajor, 'VRML/X3D', 'Child of MultiTexture node cannot be another MultiTexture node') else
        Success := Renderer.GLTextureNodes.Enable(
          TAbstractTextureNode(ChildTex), I, Shader, Env);

      { Apply Env for fixed-function pipeline. }
      if Success and GLUseMultiTexturing { needed OpenGL exts available } then
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
          glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE);

          glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB, Env.Combine[cRGB]);
          glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_ALPHA, Env.Combine[cAlpha]);

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

          if (Env.Combine[cRGB] = GL_INTERPOLATE) or
             (Env.Combine[cAlpha] = GL_INTERPOLATE) then
          begin
            { Whole source2 (both RGB and alpha) is filled by alpha from color
              specified by InterpolateAlphaSource. }
            glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE2_RGB, ColorSourceGL[Env.InterpolateAlphaSource]);
            glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE2_ALPHA, ColorSourceGL[Env.InterpolateAlphaSource]);
            glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND2_RGB, GL_SRC_ALPHA);
            glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND2_ALPHA, GL_SRC_ALPHA);
          end;

          if Env.NeedsConstantColor then
          begin
            { Assign constant color now, when we know it should be used. }
            glTexEnvv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, Vector4Single(
              TextureNode.FdColor.Value,
              TextureNode.FdAlpha.Value));
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
  MinFilter, MagFilter: TGLint;
  Anisotropy: TGLfloat;
  TextureWrap: TTextureWrap2D;
begin
  { HandledNode already made sure IsTextureImage = @true }

  HandleTexturePropertiesCore(TextureNode.TextureProperties,
    MinFilter, MagFilter, Anisotropy);

  TextureWrap[0] := TextureRepeatToGL(TextureNode.RepeatS);
  TextureWrap[1] := TextureRepeatToGL(TextureNode.RepeatT);

  SetTextureRGBFromImage(TextureNode.TextureImage);

  GLName := Renderer.Cache.TextureImage_IncReference(
    TextureNode.TextureImage,
    TextureNode.TextureUsedFullUrl,
    TextureNode,
    MinFilter,
    MagFilter,
    Anisotropy,
    TextureWrap,
    TextureNode.TextureDDS,
    { This way, our AlphaChannelType is calculated (or taken from cache)
      by TextureImage_IncReference }
    AlphaChannelType);
end;

procedure TGLImageTextureNode.Unprepare;
begin
  if GLName <> 0 then
  begin
    Renderer.Cache.TextureImage_DecReference(GLName);
    GLName := 0;
  end;
end;

function TGLImageTextureNode.Bind(const TextureUnit: Cardinal): boolean;
begin
  Result := GLName <> 0;
  if not Result then Exit;

  Renderer.ActiveTexture(TextureUnit);
  glBindTexture(GL_TEXTURE_2D, GLName);
end;

function TGLImageTextureNode.Enable(const TextureUnit: Cardinal;
  Shader: TShader; const Env: TTextureEnv): boolean;
begin
  Result := Bind(TextureUnit);
  if not Result then Exit;

  Shader.EnableTexture(TextureUnit, tt2D, TextureNode, Env);
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
  MinFilter, MagFilter: TGLint;
  Anisotropy: TGLfloat;
  TextureWrap: TTextureWrap2D;
begin
  { HandledNode already made sure IsTextureVideo = @true }

  HandleTexturePropertiesCore(TextureNode.TextureProperties,
    MinFilter, MagFilter, Anisotropy);

  TextureWrap[0] := TextureRepeatToGL(TextureNode.RepeatS);
  TextureWrap[1] := TextureRepeatToGL(TextureNode.RepeatT);

  SetTextureRGBFromImage(TextureNode.TextureVideo.Items[0]);

  GLVideo := Renderer.Cache.TextureVideo_IncReference(
    TextureNode.TextureVideo,
    TextureNode.TextureUsedFullUrl,
    TextureNode,
    MinFilter,
    MagFilter,
    Anisotropy,
    TextureWrap,
    { This way, our AlphaChannelType is calculated (or taken from cache)
      by TextureVideo_IncReference }
    AlphaChannelType);
end;

procedure TGLMovieTextureNode.Unprepare;
begin
  if GLVideo <> nil then
    Renderer.Cache.TextureVideo_DecReference(GLVideo);
end;

function TGLMovieTextureNode.Bind(const TextureUnit: Cardinal): boolean;
var
  VideoTime: TFloatTime;
begin
  { Note: don't call IsTextureImage, IsTextureVideo here --- this
    would cause reloading images/videos, nullifying
    TCastleSceneCore.FreeResources([frTextureDataInNodes]) purpose.

    Actually, it would be safe to call this for non-MovieTexture nodes,
    as they should be prepared to display lists before doing
    FreeResources. But for MovieTexture nodes it's forbidden,
    as it's called at every frame render. }

  Result := GLVideo <> nil;
  if not Result then Exit;

  VideoTime := TextureNode.TimeDependentNodeHandler.ElapsedTimeInCycle *
               TextureNode.FdSpeed.Value;
  if TextureNode.FdSpeed.Value < 0 then
    VideoTime := TextureNode.Duration + VideoTime;

  Renderer.ActiveTexture(TextureUnit);
  glBindTexture(GL_TEXTURE_2D, GLVideo.GLTextureFromTime(VideoTime));
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
  InitialImage: TImage;
  MinFilter, MagFilter: TGLint;
  Anisotropy: TGLfloat;
  TextureWrap: TTextureWrap2D;
  NodeWidth, NodeHeight: Cardinal;
begin
  HandleTextureProperties(TextureNode.FdTextureProperties.Value,
    MinFilter, MagFilter, Anisotropy);

  { calculate MinFilter, MagFilter, Anisotropy, NeedsMipmaps }
  NeedsMipmaps := TextureMinFilterNeedsMipmaps(MinFilter);
  if NeedsMipmaps and not HasGenerateMipmap then
  begin
    OnWarning(wtMinor, 'VRML/X3D' { This may be caused by OpenGL implementation
      limits, so it may be impossible to predict by VRML author,
      so it's "ignorable" warning. },
      'OpenGL implementation doesn''t allow any glGenerateMipmap* version, so you cannot use mipmaps for RenderedTexture');
    MinFilter := GL_LINEAR;
    NeedsMipmaps := false;
  end;

  TextureWrap[0] := TextureRepeatToGL(TextureNode.FdRepeatS.Value);
  TextureWrap[1] := TextureRepeatToGL(TextureNode.FdRepeatT.Value);

  { calculate Width, Height }
  if TextureNode.FdDimensions.Items.Count - 1 >= 0 then
    NodeWidth := Max(TextureNode.FdDimensions.Items[0], 0) else
    NodeWidth := DefaultRenderedTextureWidth;
  if TextureNode.FdDimensions.Items.Count - 1 >= 1 then
    NodeHeight := Max(TextureNode.FdDimensions.Items[1], 0) else
    NodeHeight := DefaultRenderedTextureHeight;
  Width  := NodeWidth ;
  Height := NodeHeight;
  if not IsTextureSized(Width, Height) then
  begin
    ResizeToTextureSize(Width, Height);
    OnWarning(wtMinor, 'VRML/X3D' { This may be caused by OpenGL implementation
      limits, so it may be impossible to predict by VRML author,
      so it's "ignorable" warning. },
      Format('Rendered texture size %d x %d is incorrect (texture size must be a power of two, > 0 and <= GL_MAX_TEXTURE_SIZE = %d), corrected to %d x %d',
        [ NodeWidth, NodeHeight,
          GLMaxTextureSize,
          Width, Height]));
  end;

  { calculate DepthMap }
  if TextureNode.FdDepthMap.Count > 0 then
    DepthMap := TextureNode.FdDepthMap.Items[0] else
    DepthMap := false;

  if DepthMap then
  begin
    if not GL_ARB_depth_texture then
    begin
      OnWarning(wtMajor, 'VRML/X3D', 'Your OpenGL doesn''t support ARB_depth_texture, cannot use RenderedTexture with depthMap = TRUE');
      Exit;
    end;

    GLName := Renderer.Cache.TextureDepth_IncReference(
      TextureNode, TextureWrap, nil, Width, Height, false);
  end else
  begin
    InitialImage := TRGBImage.Create(Width, Height);
    try
      SetTextureRGBFromImage(InitialImage);

      { Fill with deliberately stupid (but constant) color,
        to recognize easily RenderedTexture which don't have textures
        updated. }
      InitialImage.Clear(Vector4Byte(255, 0, 255, 255));

      GLName := Renderer.Cache.TextureImage_IncReference(
        InitialImage,
        '',
        TextureNode,
        MinFilter,
        MagFilter,
        Anisotropy,
        TextureWrap,
        nil,
        { This way, our AlphaChannelType is calculated (or taken from cache)
          by TextureImage_IncReference }
        AlphaChannelType);

      { RenderedTexture never has any normal / height map
        (Hm, although it would be possible to generate some in theory
        --- after all, we generate it from 3D data. Idea for the future.)
      NormalMap := 0;
      HeightMap := 0;
      }
    finally FreeAndNil(InitialImage) end;
  end;

  RenderToTexture := TGLRenderToTexture.Create(Width, Height);
  RenderToTexture.SetTexture(GLName, GL_TEXTURE_2D);
  if DepthMap then
    RenderToTexture.Buffer := tbDepth else
    RenderToTexture.Buffer := tbColor;
  RenderToTexture.Stencil := not DepthMap;
  RenderToTexture.GLContextOpen;
end;

procedure TGLRenderedTextureNode.Unprepare;
begin
  FreeAndNil(RenderToTexture);

  if GLName <> 0 then
  begin
    if DepthMap then
      Renderer.Cache.TextureDepth_DecReference(GLName) else
      Renderer.Cache.TextureImage_DecReference(GLName);
    GLName := 0;
  end;
end;

function TGLRenderedTextureNode.Bind(const TextureUnit: Cardinal): boolean;
begin
  Result := GLName <> 0;
  if not Result then Exit;

  Renderer.ActiveTexture(TextureUnit);
  glBindTexture(GL_TEXTURE_2D, GLName);
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
  CurrentViewpoint: TAbstractViewpointNode;
  CameraViewKnown: boolean;
  const CameraPosition, CameraDirection, CameraUp: TVector3Single);

var
  Viewpoint: TAbstractViewpointNode;

  function GetProjectionMatrix: TMatrix4Single;
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
      FieldOfView: Single;
      AngleOfViewX, AngleOfViewY: Single;
    begin
      if (Viewpoint <> nil) and
         (Viewpoint is TViewpointNode) then
        FieldOfView := TViewpointNode(Viewpoint).FdFieldOfView.Value else
      if (Viewpoint <> nil) and
         (Viewpoint is TPerspectiveCameraNode_1) then
        FieldOfView := TPerspectiveCameraNode_1(Viewpoint).FdHeightAngle.Value else
        FieldOfView := DefaultViewpointFieldOfView;

      AngleOfViewX := RadToDeg(TViewpointNode.ViewpointAngleOfView(
        FieldOfView, Width / Height));

      AngleOfViewY := AdjustViewAngleDegToAspectRatio(
        AngleOfViewX, Height / Width);

      Result := PerspectiveProjMatrixDeg(AngleOfViewY, Width / Height,
        ProjectionNear, ProjectionFar);
    end;

    procedure DoOrthographic;
    var
      FieldOfView: TSingleList;
      FinalFieldOfView: TVector4Single;
    begin
      { default VRML/X3D fov }
      FinalFieldOfView[0] := -1;
      FinalFieldOfView[1] := -1;
      FinalFieldOfView[2] :=  1;
      FinalFieldOfView[3] :=  1;

      { update left / right / bottom / top using OrthoViewpoint.fieldOfView }
      if (Viewpoint <> nil) and
         (Viewpoint is TOrthoViewpointNode) then
      begin
        FieldOfView := TOrthoViewpointNode(Viewpoint).FdFieldOfView.Items;
        if FieldOfView.Count > 0 then FinalFieldOfView[0] := FieldOfView.Items[0];
        if FieldOfView.Count > 1 then FinalFieldOfView[1] := FieldOfView.Items[1];
        if FieldOfView.Count > 2 then FinalFieldOfView[2] := FieldOfView.Items[2];
        if FieldOfView.Count > 3 then FinalFieldOfView[3] := FieldOfView.Items[3];
      end else
      if (Viewpoint <> nil) and
         (Viewpoint is TOrthographicCameraNode_1) then
      begin
        FinalFieldOfView[0] := -TOrthographicCameraNode_1(Viewpoint).FdHeight.Value / 2;
        FinalFieldOfView[1] := -TOrthographicCameraNode_1(Viewpoint).FdHeight.Value / 2;
        FinalFieldOfView[2] :=  TOrthographicCameraNode_1(Viewpoint).FdHeight.Value / 2;
        FinalFieldOfView[3] :=  TOrthographicCameraNode_1(Viewpoint).FdHeight.Value / 2;
      end;

      TOrthoViewpointNode.AspectFieldOfView(FinalFieldOfView, Width / Height);

      Result := OrthoProjMatrix(
        { Beware: order of OrthoViewpoint.fieldOfView (and FinalFieldOfView)
          is different than typical OpenGL and our OrthoProjMatrix. }
        FinalFieldOfView[0],
        FinalFieldOfView[2],
        FinalFieldOfView[1],
        FinalFieldOfView[3],
        ProjectionNear, ProjectionFar);
    end;

  var
    ProjectionType: TProjectionType;
  begin
    if Viewpoint <> nil then
      ProjectionType := Viewpoint.ProjectionType else
      ProjectionType := ptPerspective;

    case ProjectionType of
      ptPerspective: DoPerspective;
      ptOrthographic: DoOrthographic;
      else EInternalError.Create('TGLRenderedTextureNode.Update-ProjectionType?');
    end;
  end;

  procedure GetRenderedTextureCamera(out Pos, Dir, Up: TVector3Single);
  var
    { It's returned by Viewpoint.GetView, but we'll ignore it. }
    GravityUp: TVector3Single;

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

var
  CameraMatrix, CameraRotationMatrix, ProjectionMatrix: TMatrix4Single;
  Pos, Dir, Up: TVector3Single;
  Scene: TX3DEventsEngine;
begin
  if GLName = 0 then Exit;

  { calculate Viewpoint }
  if (TextureNode.FdViewpoint.Value <> nil) and
     (TextureNode.FdViewpoint.Value is TAbstractViewpointNode) then
    Viewpoint := TAbstractViewpointNode(TextureNode.FdViewpoint.Value) else
    Viewpoint := CurrentViewpoint;

  ProjectionMatrix := GetProjectionMatrix;
  TextureNode.EventProjection.Send(ProjectionMatrix);

  GetRenderedTextureCamera(Pos, Dir, Up);
  CameraMatrix := LookDirMatrix(Pos, Dir, Up);
  CameraRotationMatrix := LookDirMatrix(ZeroVector3Single, Dir, Up);
  TextureNode.EventViewing.Send(CameraMatrix);

  TextureNode.EventRendering.Send(true);

  RenderingCamera.Target := rfOffScreen;
  RenderingCamera.FromMatrix(CameraMatrix, CameraRotationMatrix, ProjectionMatrix);

  RenderToTexture.RenderBegin;

    glViewport(0, 0, Width, Height);

    glMatrixMode(GL_PROJECTION);
    glPushMatrix;
      glLoadMatrix(ProjectionMatrix);
      glMatrixMode(GL_MODELVIEW);
      Render;
      glMatrixMode(GL_PROJECTION);
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);

  RenderToTexture.RenderEnd;

  { depth maps (created by TextureDepth_IncReference, with GL_DEPTH_COMPONENT)
    cannot have mipmaps regenerated. On Mesa 7.6 (ubuntu 10.4, Mesa DRI Intel
    on "domek") glGenerateMipmapEXT even causes "invalid operation" OpenGL
    error (test e.g. view3dscene on rendered_texture.x3dv). }
  if NeedsMipmaps and (not DepthMap) then
    RenderToTexture.GenerateMipmap;

  { We want to call IncreaseTimeTick now, otherwise our rendering.Send(false)
    event would be always rejected (as the rendering.Send(true) already
    traveled through through the same route at the same time,
    so new event would be rejected as it can be a signal that we have
    a cycle in routes.) }
  Scene := TextureNode.Scene;
  if Scene <> nil then
    Scene.IncreaseTimeTick;

  TextureNode.EventRendering.Send(false);

  NeedsRestoreViewport := true;
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
  glBindTexture(GL_TEXTURE_CUBE_MAP_ARB, GLName);
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

  { Checks is given side has non-nil valid node class,
    and then if image there can be loaded. }
  function SideLoaded(SideField: TSFNode): boolean;
  var
    SideTex: TAbstractTexture2DNode;
  begin
    Result :=
      (SideField.Value <> nil) and
      (SideField.Value is TAbstractTexture2DNode);
    if Result then
    begin
      SideTex := TAbstractTexture2DNode(SideField.Value);
      Result := SideTex.IsTextureImage;

      if Result and not (SideTex.TextureImage is TImage) then
      begin
        OnWarning(wtMinor, 'VRML/X3D', 'ComposedCubeMapTexture cannot contain S3TC images, as we have to rotate images within, and we cannot do this (fast) with compressed textures');
        Result := false;
      end;
    end;
  end;

var
  MinFilter, MagFilter: TGLint;
  Anisotropy: TGLfloat;
  BackRot, FrontRot, LeftRot, RightRot: TImage;
begin
  if not GL_ARB_texture_cube_map then
  begin
    OnWarning(wtMajor, 'VRML/X3D', 'Your OpenGL doesn''t support ARB_texture_cube_map, cannot use CubeMapTexture');
    Exit;
  end;

  if not (SideLoaded(TextureNode.FdBack) and
     SideLoaded(TextureNode.FdBottom) and
     SideLoaded(TextureNode.FdFront) and
     SideLoaded(TextureNode.FdLeft) and
     SideLoaded(TextureNode.FdRight) and
     SideLoaded(TextureNode.FdTop)) then
  begin
    OnWarning(wtMajor, 'VRML/X3D', 'Not all sides of a CubeMapTexture are correctly set and loaded, cannot use cube map');
    Exit;
  end;

  HandleTextureProperties(TextureNode.FdTextureProperties.Value,
    MinFilter, MagFilter, Anisotropy);

  try
    { To match expected orientation for OpenGL, we have to rotate images.
      (source images are oriented as for VRML Background.)
      We safely cast them to TImage below, SideLoaded above checked
      that they are indeed of TImage class. }
    BackRot  := (TAbstractTexture2DNode(TextureNode.FdBack .Value).TextureImage as TImage).MakeRotated(2);
    FrontRot := (TAbstractTexture2DNode(TextureNode.FdFront.Value).TextureImage as TImage).MakeRotated(2);
    LeftRot  := (TAbstractTexture2DNode(TextureNode.FdLeft .Value).TextureImage as TImage).MakeRotated(2);
    RightRot := (TAbstractTexture2DNode(TextureNode.FdRight.Value).TextureImage as TImage).MakeRotated(2);

    SetTextureRGBFromImage(RightRot);

    GLName := Renderer.Cache.TextureCubeMap_IncReference(
      TextureNode,
      MinFilter, MagFilter, Anisotropy,
      { positive x } RightRot,
      { negative x } LeftRot,
      { positive y } TAbstractTexture2DNode(TextureNode.FdTop   .Value).TextureImage as TImage,
      { negative y } TAbstractTexture2DNode(TextureNode.FdBottom.Value).TextureImage as TImage,
      { positive z } BackRot,
      { negative z } FrontRot,
      nil,
      AlphaChannelType);
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
  MinFilter, MagFilter: TGLint;
  Anisotropy: TGLfloat;
  DDS: TDDSImage;
begin
  if not GL_ARB_texture_cube_map then
  begin
    OnWarning(wtMajor, 'VRML/X3D', 'Your OpenGL doesn''t support ARB_texture_cube_map, cannot use CubeMapTexture');
    Exit;
  end;

  DDS := TextureNode.LoadImage;
  { If TextureNode doesn't contain anything useful, just exit.
    TextureNode.LoadImage already did necessary OnWarnings. }
  if DDS = nil then Exit;

  try

    HandleTextureProperties(TextureNode.FdTextureProperties.Value,
      MinFilter, MagFilter, Anisotropy);

    { TODO: this is a quick and dirty method:
      - We call LoadImage each time, while load calls should
        be minimized (to avoid loading image many times, but also
        to avoid making repeated warnings in case image fails).
        Should be cached, like for 2D texture nodes.
      - We do not use cube map mipmaps stored inside DDS file.
    }

    SetTextureRGBFromImage(DDS.CubeMapImage(dcsPositiveX));

    GLName := Renderer.Cache.TextureCubeMap_IncReference(
      TextureNode,
      MinFilter, MagFilter, Anisotropy,
      DDS.CubeMapImage(dcsPositiveX),
      DDS.CubeMapImage(dcsNegativeX),
      { Swap meaning of positive/negative Y faces from DDS,
        see TDDSCubeMapSide for explanation. }
      DDS.CubeMapImage(dcsNegativeY),
      DDS.CubeMapImage(dcsPositiveY),
      DDS.CubeMapImage(dcsPositiveZ),
      DDS.CubeMapImage(dcsNegativeZ),
      DDS,
      AlphaChannelType);
  finally FreeAndNil(DDS); end;
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
  MinFilter, MagFilter: TGLint;
  Anisotropy: TGLfloat;
  InitialImage: TImage;
begin
  if not GL_ARB_texture_cube_map then
  begin
    OnWarning(wtMajor, 'VRML/X3D', 'Your OpenGL doesn''t support ARB_texture_cube_map, cannot use CubeMapTexture');
    Exit;
  end;

  HandleTextureProperties(TextureNode.FdTextureProperties.Value,
    MinFilter, MagFilter, Anisotropy);

  { calculate MinFilter, MagFilter, Anisotropy, NeedsMipmaps }
  NeedsMipmaps := TextureMinFilterNeedsMipmaps(MinFilter);
  if NeedsMipmaps and not HasGenerateMipmap then
  begin
    OnWarning(wtMinor, 'VRML/X3D' { This may be caused by OpenGL implementation
      limits, so it may be impossible to predict by VRML author,
      so it's "ignorable" warning. },
      'OpenGL implementation doesn''t allow any glGenerateMipmap* version, so you cannot use mipmaps for GeneratedCubeMapTexture');
    MinFilter := GL_LINEAR;
    NeedsMipmaps := false;
  end;

  { calculate Size }
  Size := Max(TextureNode.FdSize.Value, 0);
  if not IsCubeMapTextureSized(Size) then
  begin
    Size := ResizeToCubeMapTextureSize(Size);
    OnWarning(wtMinor, 'VRML/X3D' { This may be caused by OpenGL implementation
      limits, so it may be impossible to predict by VRML author,
      so it's "ignorable" warning. },
      Format('Cube map texture size %d is incorrect (cube map texture size must be a power of two, > 0 and <= GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB = %d), corrected to %d',
        [ TextureNode.FdSize.Value,
          GLMaxCubeMapTextureSizeARB,
          Size]));
  end;

  InitialImage := TRGBImage.Create(Size, Size);
  try
    { Fill with deliberately stupid (but constant) color,
      to recognize easily GeneratedCubeMapTexture which don't have textures
      updated. }
    InitialImage.Clear(Vector4Byte(255, 0, 255, 255));

    SetTextureRGBFromImage(InitialImage);

    GLName := Renderer.Cache.TextureCubeMap_IncReference(
      TextureNode,
      MinFilter, MagFilter, Anisotropy,
      InitialImage, InitialImage,
      InitialImage, InitialImage,
      InitialImage, InitialImage,
      nil,
      AlphaChannelType);
  finally FreeAndNil(InitialImage) end;

  RenderToTexture := TGLRenderToTexture.Create(Size, Size);
  RenderToTexture.SetTexture(GLName, GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB);
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
  if GLVersion.VendorNvidia and NeedsMipmaps then
  begin
    RenderToTexture.CompleteTextureTarget := GL_TEXTURE_CUBE_MAP_ARB;
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
  const CubeMiddle: TVector3Single);
begin
  if GLName = 0 then Exit;

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
  MinFilter, MagFilter: TGLint;
  Anisotropy: TGLfloat;
  TextureWrap: TTextureWrap3D;
begin
  if GL3DTextures = gsNone then
  begin
    OnWarning(wtMajor, 'VRML/X3D', 'Your OpenGL doesn''t support 3D textures, cannot use Texture3D nodes');
    Exit;
  end;

  TextureNode.TextureLoaded := true;

  { If TextureImage doesn't contain anything useful, just exit.
    Setting TextureLoaded already did necessary OnWarnings. }
  if TextureNode.TextureImage = nil then Exit;

  HandleTextureProperties(TextureNode.FdTextureProperties.Value,
    MinFilter, MagFilter, Anisotropy);

  { calculate TextureWrap }
  TextureWrap[0] := TextureRepeatToGL(TextureNode.FdRepeatS.Value);
  TextureWrap[1] := TextureRepeatToGL(TextureNode.FdRepeatT.Value);
  TextureWrap[2] := TextureRepeatToGL(TextureNode.FdRepeatR.Value);

  SetTextureRGBFromImage(TextureNode.TextureImage);

  GLName := Renderer.Cache.Texture3D_IncReference(
    TextureNode, MinFilter, MagFilter, Anisotropy,
    TextureWrap, TextureNode.TextureImage, TextureNode.TextureDDS,
    AlphaChannelType);
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
  glBindTexture(GL_TEXTURE_3D, GLName);
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
    (TGLSLProgram.ClassSupport <> gsNone) and
    (GL_ATI_texture_float or GL_ARB_texture_float);
end;

procedure TGLGeneratedShadowMap.PrepareCore(State: TX3DGraphTraverseState);
var
  TextureWrap: TTextureWrap2D;
  MinFilter: TGLint;
begin
  { Float texture extensions: there is also NV_float_buffer,
    which is the only way for a float texture on old nvidia GPUs.
    But it's only for NV_texture_rectangle, without bilinear filtering
    or mipmapping, so it's not useful for VSM at all. }

  VarianceShadowMaps := ClassVarianceShadowMaps(Renderer.Attributes);

  if Log then
    WritelnLog('Shadows', Format('Variance Shadow Maps used: %s (Reasons: Attributes.Shadow sampling = %s, GLSL support = %s, ATI_texture_float = %s, ARB_texture_float = %s)',
      [ BoolToStr[VarianceShadowMaps],
        ShadowSamplingNames[Renderer.Attributes.ShadowSampling],
        GLSupportNames[TGLSLProgram.ClassSupport],
        BoolToStr[GL_ATI_texture_float],
        BoolToStr[GL_ARB_texture_float] ]));

  { TODO: fix TextureNode.FdSize.Value if needed }
  Size := TextureNode.FdSize.Value;

  VisualizeDepthMap := (TextureNode.FdCompareMode.Value = 'NONE') or
    Renderer.Attributes.VisualizeDepthMap;

  { At least in case of float textures for VSM, CLAMP_TO_EDGE is needed
    (instead of standard clamp to border). Didn't see any difference
    for depth textures. }
  TextureWrap[0] := CastleGL_CLAMP_TO_EDGE;
  TextureWrap[1] := CastleGL_CLAMP_TO_EDGE;

  { calculate MinFilter, NeedsMipmaps }
  if VarianceShadowMaps and HasGenerateMipmap then
  begin
    NeedsMipmaps := true;
    MinFilter := GL_LINEAR_MIPMAP_LINEAR;
  end else
  begin
    NeedsMipmaps := false;
    MinFilter := GL_LINEAR;
  end;

  if VarianceShadowMaps then
  begin
    GLName := Renderer.Cache.TextureFloat_IncReference(
      TextureNode, MinFilter, GL_LINEAR, TextureWrap, Size, Size, true);
  end else
  begin
    if not GL_ARB_depth_texture then
    begin
      OnWarning(wtMajor, 'VRML/X3D', 'Your OpenGL doesn''t support ARB_depth_texture, cannot use GeneratedShadowMap nodes');
      Exit;
    end;

    GLName := Renderer.Cache.TextureDepth_IncReference(
      TextureNode, TextureWrap, TextureNode.FdCompareMode, Size, Size,
      VisualizeDepthMap);
  end;

  RenderToTexture := TGLRenderToTexture.Create(Size, Size);
  RenderToTexture.SetTexture(GLName, GL_TEXTURE_2D);
  if VarianceShadowMaps then
    RenderToTexture.Buffer := tbColor else
    RenderToTexture.Buffer := tbDepth;
  RenderToTexture.Stencil := false;
  RenderToTexture.GLContextOpen;
end;

procedure TGLGeneratedShadowMap.Unprepare;
begin
  FreeAndNil(RenderToTexture);

  if GLName <> 0 then
  begin
    if VarianceShadowMaps then
      Renderer.Cache.TextureFloat_DecReference(GLName) else
      Renderer.Cache.TextureDepth_DecReference(GLName);
    GLName := 0;
  end;
end;

function TGLGeneratedShadowMap.Bind(const TextureUnit: Cardinal): boolean;
begin
  Result := GLName <> 0;
  if not Result then Exit;

  Renderer.ActiveTexture(TextureUnit);
  glBindTexture(GL_TEXTURE_2D, GLName);
end;

function TGLGeneratedShadowMap.Enable(const TextureUnit: Cardinal;
  Shader: TShader; const Env: TTextureEnv): boolean;
begin
  Result := Bind(TextureUnit);
  if not Result then Exit;

  Shader.EnableTexture(TextureUnit, tt2DShadow, TextureNode, Env,
    Size, TextureNode.Light, VisualizeDepthMap);
end;

procedure TGLGeneratedShadowMap.Update(
  const Render: TRenderFromViewFunction;
  const ProjectionNear, ProjectionFar: Single;
  var NeedsRestoreViewport: boolean;
  Light: TAbstractLightNode);
var
  ProjectionMatrix: TMatrix4Single;
begin
  if GLName = 0 then Exit;

  { Render view for shadow map }
  ProjectionMatrix := Light.ProjectionMatrix;

  if VarianceShadowMaps then
    RenderingCamera.Target := rtVarianceShadowMap else
    RenderingCamera.Target := rtShadowMap;

  RenderingCamera.FromMatrix(
    Light.ModelviewMatrix,
    Light.ModelviewRotationMatrix,
    ProjectionMatrix);

  RenderToTexture.RenderBegin;

    glViewport(0, 0, Size, Size);

    glMatrixMode(GL_PROJECTION);
    glPushMatrix;
      glLoadMatrix(ProjectionMatrix);
      glMatrixMode(GL_MODELVIEW);

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

        Render;

      glPopAttrib;

      glMatrixMode(GL_PROJECTION);
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);

  RenderToTexture.RenderEnd;

  if NeedsMipmaps then
    RenderToTexture.GenerateMipmap;

  NeedsRestoreViewport := true;
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
