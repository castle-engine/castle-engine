{$ifdef read_interface}

{ TODO:
  - it would be nice to remove dependency on TVRMLOpenGLRenderer entirely.
    This would make it more independent from TVRMLOpenGLRenderer,
    so much simpler.
  - Maybe just remove Prepare/Unprepare and move them to constructor/destructor?
}
  TGLTextureNode = class;

  TGLTextureNodeClass = class of TGLTextureNode;

  TBumpMappingWanted = (
    bmwNone,
    bmwNoParallax,
    bmwParallax);

  TTextureEnableDisable = (etOff, et2D, etCubeMap, et3D);

  { OpenGL handling for VRML texture node. }
  TGLTextureNode = class
  private
    FRenderer: TVRMLOpenGLRenderer;
    FTextureNode: TNodeX3DTextureNode;
  protected
    { Calculate things from TextureProperties node.
      If TextureProperties = @nil, they are taken from defaults
      (possibly in Attributes). }
    procedure HandleTexturePropertiesCore(
      TextureProperties: TNodeTextureProperties;
      out MinFilter, MagFilter: TGLint;
      out Anisotropy: TGLfloat);

    { Calculate things from TextureProperties node.
      If TextureProperties = @nil or not of TNodeTextureProperties class,
      they are taken from defaults (possibly in Attributes).

      This is useful when interpreting VRML/X3D files,
      as you have no guarantee user didn't place there some disallowed node
      in "textureProperties" field. }
    procedure HandleTextureProperties(
      TextureProperties: TVRMLNode;
      out MinFilter, MagFilter: TGLint;
      out Anisotropy: TGLfloat);

    { Decide if this class can handle given texture Node. }
    class function IsClassForTextureNode(Cache: TTexturesImagesVideosCache;
      Node: TNodeX3DTextureNode): boolean; virtual; abstract;

    { Enable/disable texturing (2D and cube map) on current texture unit. }
    class procedure TextureEnableDisable(const Enable: TTextureEnableDisable);
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer; ATextureNode: TNodeX3DTextureNode);

    property Renderer: TVRMLOpenGLRenderer read FRenderer;

    { Reference to handled texture node.
      Never @nil.
      It's guaranteed to satisfy IsClassForTextureNode method of this class. }
    property TextureNode: TNodeX3DTextureNode read FTextureNode;

    { Find suitable TGLTextureNode class that can best handle given Node
      (with given Cache). Returns @nil if not found.

      @italic(Descedants implementors): override IsClassForTextureNode
      to be correctly recognized by this. }
    class function ClassForTextureNode(Cache: TTexturesImagesVideosCache;
      Node: TNodeX3DTextureNode): TGLTextureNodeClass;

    { Do the time-consuming preparations before rendering this texture.
      Must be called with BumpMappingWanted = initially bmwNone,
      implementation of this may set BumpMappingWanted <> bmwNone
      if some bump mapping data (height map, normal map) found.
      Called from TVRMLOpenGLRenderer.Prepare. }
    procedure Prepare(State: TVRMLGraphTraverseState;
      var BumpMappingWanted: TBumpMappingWanted); virtual; abstract;

    { Release all resources allocated by @link(Prepare). }
    procedure Unprepare; virtual; abstract;

    { This is the alpha channel of this texture.
      Used by TVRMLOpenGLRenderer.PreparedTextureAlphaChannelType.
      Should be calculated and set in Prepare.

      For textures with normal images, this is the saved result of
      TImage.AlphaChannelType. For videos, saved result of
      TVideo.AlphaChannelType.

      By default, atNone in this class.

      TODO: it would be better to move this inside, possibly
      only to applicable class types? And expose only function
      like UseBlending, and this is the main use of
      TVRMLOpenGLRenderer.PreparedTextureAlphaChannelType ? }
    AlphaChannelType: TAlphaChannelType;

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
      GL_TEXTURE_CUBE_MAP_ARB / GL_TEXTURE_3D_EXT) ).

      Primitives3DTextureCoords may be modified,
      but only to true, by this procedure. }
    function Enable(const TextureUnit: Cardinal;
      const UseForBumpMappingAllowed: boolean;
      var Primitives3DTextureCoords: boolean): boolean; virtual; abstract;

    { Sets texture state for many texture units, based on this node.
      On every texture unit where something is enabled,
      proper texture identifier must be bound.
      Also, has to set complete glTexEnv on every enabled texture unit.

      TextureUnitsCount says how many texture units can be enabled/disabled.

      TextureUnitsCount does *not* take into account whether multitexturing
      OpenGL extensions are available at all.
      Look at UseMultiTexturing for this.
      Think of UseMultiTexturing as capping TextureUnitsCount to 1
      (still, remember to honour TextureUnitsCount = 0 case in your implementation,
      even when UseMultiTexturing = @true).

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
      and only on them.

      Primitives3DTextureCoords may be modified,
      but only to true, by this procedure. }
    procedure EnableAll(
      const TextureUnitsCount: Cardinal;
      const UseMultiTexturing: boolean;
      out TexCoordsNeeded: Cardinal;
      var Primitives3DTextureCoords: boolean); virtual; abstract;
  end;

  TObjectsListItem_1 = TGLTextureNode;
  {$I objectslist_1.inc}
  TGLTextureNodes = class(TObjectsList_1)
  public
    { Looks for item with given Node.
      Returns -1 if not found. }
    function TextureNodeIndex(Node: TNodeX3DTextureNode): Integer;

    { Looks for item with given Node.
      Returns @nil if not found. }
    function TextureNode(Node: TNodeX3DTextureNode): TGLTextureNode;
  end;

  { Common class for all single (not multi-texture) texture nodes.

    Implements EnableAll method, by calling @link(Enable) call.
    Override only @link(Enable) in descendants. }
  TGLSingleTextureNode = class(TGLTextureNode)
  public
    procedure EnableAll(
      const TextureUnitsCount: Cardinal;
      const UseMultiTexturing: boolean;
      out TexCoordsNeeded: Cardinal;
      var Primitives3DTextureCoords: boolean); override;
  end;

  { Handler for TNodeMultiTexture.

    This is somewhat special, in that it will use other TGLTextureNode
    handlers to handle single textures inside --- but this is completely
    hidden from the interface. }
  TGLMultiTextureNode = class(TGLTextureNode)
  protected
    class function IsClassForTextureNode(Cache: TTexturesImagesVideosCache;
      Node: TNodeX3DTextureNode): boolean; override;
  public
    function TextureNode: TNodeMultiTexture;

    procedure Prepare(State: TVRMLGraphTraverseState;
      var BumpMappingWanted: TBumpMappingWanted); override;
    procedure Unprepare; override;
    function Enable(const TextureUnit: Cardinal;
      const UseForBumpMappingAllowed: boolean;
      var Primitives3DTextureCoords: boolean): boolean; override;
    procedure EnableAll(
      const TextureUnitsCount: Cardinal;
      const UseMultiTexturing: boolean;
      out TexCoordsNeeded: Cardinal;
      var Primitives3DTextureCoords: boolean); override;
  end;

  { Handler for TVRMLTextureNode with image (not a video). }
  TGLImageTextureNode = class(TGLSingleTextureNode)
  protected
    class function IsClassForTextureNode(Cache: TTexturesImagesVideosCache;
      Node: TNodeX3DTextureNode): boolean; override;
  public
    GLName: TGLuint;
    NormalMap, HeightMap: TGLuint;
    HeightMapScale: Single;

    function TextureNode: TVRMLTextureNode;

    procedure Prepare(State: TVRMLGraphTraverseState;
      var BumpMappingWanted: TBumpMappingWanted); override;
    procedure Unprepare; override;
    function Enable(const TextureUnit: Cardinal;
      const UseForBumpMappingAllowed: boolean;
      var Primitives3DTextureCoords: boolean): boolean; override;
  end;

  TGLMovieTextureNode = class(TGLSingleTextureNode)
  protected
    class function IsClassForTextureNode(Cache: TTexturesImagesVideosCache;
      Node: TNodeX3DTextureNode): boolean; override;
  public
    GLVideo: TGLVideo;

    function TextureNode: TNodeMovieTexture;

    procedure Prepare(State: TVRMLGraphTraverseState;
      var BumpMappingWanted: TBumpMappingWanted); override;
    procedure Unprepare; override;
    function Enable(const TextureUnit: Cardinal;
      const UseForBumpMappingAllowed: boolean;
      var Primitives3DTextureCoords: boolean): boolean; override;
  end;

  TGLRenderedTextureNode = class(TGLSingleTextureNode)
  protected
    class function IsClassForTextureNode(Cache: TTexturesImagesVideosCache;
      Node: TNodeX3DTextureNode): boolean; override;
  public
    GLName: TGLuint;

    { The actual decided image size and mipmap status. }
    Width, Height: Cardinal;
    NeedsMipmaps: boolean;
    DepthMap: boolean;

    function TextureNode: TNodeRenderedTexture;

    procedure Prepare(State: TVRMLGraphTraverseState;
      var BumpMappingWanted: TBumpMappingWanted); override;
    procedure Unprepare; override;
    function Enable(const TextureUnit: Cardinal;
      const UseForBumpMappingAllowed: boolean;
      var Primitives3DTextureCoords: boolean): boolean; override;
  end;

  { Common handling for texture nodes of TNodeX3DEnvironmentTextureNode. }
  TGLCubeMapTextureNode = class(TGLSingleTextureNode)
  public
    GLName: TGLuint;

    function TextureNode: TNodeX3DEnvironmentTextureNode;

    { Releases GLName by TextureCubeMap_DecReference.
      Suitable for descendants tht initialize GLName by
      TextureCubeMap_IncReference. }
    procedure Unprepare; override;

    function Enable(const TextureUnit: Cardinal;
      const UseForBumpMappingAllowed: boolean;
      var Primitives3DTextureCoords: boolean): boolean; override;
  end;

  TGLComposedCubeMapTextureNode = class(TGLCubeMapTextureNode)
  protected
    class function IsClassForTextureNode(Cache: TTexturesImagesVideosCache;
      Node: TNodeX3DTextureNode): boolean; override;
  public
    function TextureNode: TNodeComposedCubeMapTexture;

    procedure Prepare(State: TVRMLGraphTraverseState;
      var BumpMappingWanted: TBumpMappingWanted); override;
  end;

  TGLImageCubeMapTextureNode = class(TGLCubeMapTextureNode)
  protected
    class function IsClassForTextureNode(Cache: TTexturesImagesVideosCache;
      Node: TNodeX3DTextureNode): boolean; override;
  public
    function TextureNode: TNodeImageCubeMapTexture;

    procedure Prepare(State: TVRMLGraphTraverseState;
      var BumpMappingWanted: TBumpMappingWanted); override;
  end;

  TGLGeneratedCubeMapTextureNode = class(TGLCubeMapTextureNode)
  protected
    class function IsClassForTextureNode(Cache: TTexturesImagesVideosCache;
      Node: TNodeX3DTextureNode): boolean; override;
  public
    { The right size of the texture,
      that satisfies all OpenGL cube map sizes requirements
      (IsCubeMapTextureSized). }
    Size: Cardinal;

    { This says if MinFilter needs mipmaps. }
    NeedsMipmaps: boolean;

    function TextureNode: TNodeGeneratedCubeMapTexture;

    procedure Prepare(State: TVRMLGraphTraverseState;
      var BumpMappingWanted: TBumpMappingWanted); override;
  end;

  TGL3DTextureNode = class(TGLSingleTextureNode)
  protected
    class function IsClassForTextureNode(Cache: TTexturesImagesVideosCache;
      Node: TNodeX3DTextureNode): boolean; override;
  public
    GLName: TGLuint;

    function TextureNode: TNodeX3DTexture3DNode;

    procedure Prepare(State: TVRMLGraphTraverseState;
      var BumpMappingWanted: TBumpMappingWanted); override;
    procedure Unprepare; override;
    function Enable(const TextureUnit: Cardinal;
      const UseForBumpMappingAllowed: boolean;
      var Primitives3DTextureCoords: boolean): boolean; override;
  end;

  TGLGeneratedShadowMap = class(TGLSingleTextureNode)
  protected
    class function IsClassForTextureNode(Cache: TTexturesImagesVideosCache;
      Node: TNodeX3DTextureNode): boolean; override;
  public
    GLName: TGLuint;

    { The right size of the texture,
      that satisfies all OpenGL sizes requirements. }
    Size: Cardinal;

    function TextureNode: TNodeGeneratedShadowMap;

    procedure Prepare(State: TVRMLGraphTraverseState;
      var BumpMappingWanted: TBumpMappingWanted); override;
    procedure Unprepare; override;
    function Enable(const TextureUnit: Cardinal;
      const UseForBumpMappingAllowed: boolean;
      var Primitives3DTextureCoords: boolean): boolean; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

const
  TextureRepeatToGL: array [boolean] of TGLenum = (
    { GL_CLAMP is useless if VRML doesn't allow to control texture border color,
      and CLAMP_TO_EDGE is the more natural clamping method anyway...
      Hm, but X3D specification seems to indicate that normal clamp is OpenGL's CLAMP,
      and CLAMP_TO_EDGE is available by TextureProperties.boundaryMode*.
      But until this will get implemented, it's much safer (and more sensible?)
      to use GL_CLAMP_TO_EDGE here. }
    GL_CLAMP_TO_EDGE,
    GL_REPEAT);

{ TGLTextureNode ------------------------------------------------------------- }

constructor TGLTextureNode.Create(ARenderer: TVRMLOpenGLRenderer;
  ATextureNode: TNodeX3DTextureNode);
begin
  inherited Create;
  FRenderer := ARenderer;
  FTextureNode := ATextureNode;
end;

procedure TGLTextureNode.HandleTexturePropertiesCore(
  TextureProperties: TNodeTextureProperties;
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
      VRMLWarning(vwSerious, Format('"%s" is not allowed texture minification, this is an Avalon-only extension, please fix to "NEAREST_PIXEL"', [S]));
      Result := GL_NEAREST;
    end else

    begin
      Result := Renderer.Attributes.TextureMinFilter;
      VRMLWarning(vwSerious, Format('Unknown texture minification filter "%s"', [S]));
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
      VRMLWarning(vwSerious, Format('"%s" is not allowed texture minification, this is an Avalon-only extension, please fix to "NEAREST_PIXEL"', [S]));
      Result := GL_NEAREST;
    end else

    begin
      Result := Renderer.Attributes.TextureMagFilter;
      VRMLWarning(vwSerious, Format('Unknown texture minification filter "%s"', [S]));
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
  TextureProperties: TVRMLNode;
  out MinFilter, MagFilter: TGLint;
  out Anisotropy: TGLfloat);
begin
  if (TextureProperties = nil) or
     not (TextureProperties is TNodeTextureProperties) then
    HandleTexturePropertiesCore(nil, MinFilter, MagFilter, Anisotropy) else
    HandleTexturePropertiesCore(TNodeTextureProperties(TextureProperties),
      MinFilter, MagFilter, Anisotropy);
end;

class procedure TGLTextureNode.TextureEnableDisable(
  const Enable: TTextureEnableDisable);
begin
  case Enable of
    etOff:
      begin
        glDisable(GL_TEXTURE_2D);
        if GL_ARB_texture_cube_map then glDisable(GL_TEXTURE_CUBE_MAP_ARB);
        if GL_EXT_texture3D        then glDisable(GL_TEXTURE_3D_EXT);
      end;
    et2D:
      begin
        glEnable(GL_TEXTURE_2D);
        if GL_ARB_texture_cube_map then glDisable(GL_TEXTURE_CUBE_MAP_ARB);
        if GL_EXT_texture3D        then glDisable(GL_TEXTURE_3D_EXT);
      end;
    etCubeMap:
      begin
        glDisable(GL_TEXTURE_2D);
        if GL_ARB_texture_cube_map then glEnable(GL_TEXTURE_CUBE_MAP_ARB);
        if GL_EXT_texture3D        then glDisable(GL_TEXTURE_3D_EXT);
      end;
    et3D:
      begin
        glDisable(GL_TEXTURE_2D);
        if GL_ARB_texture_cube_map then glDisable(GL_TEXTURE_CUBE_MAP_ARB);
        if GL_EXT_texture3D        then glEnable(GL_TEXTURE_3D_EXT);
      end;
    else raise EInternalError.Create('TextureEnableDisable?');
  end;
end;

class function TGLTextureNode.ClassForTextureNode(Cache: TTexturesImagesVideosCache;
  Node: TNodeX3DTextureNode): TGLTextureNodeClass;

  function TryResult(C: TGLTextureNodeClass): boolean;
  begin
    Result := C.IsClassForTextureNode(Cache, Node);
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
          TryResult(TGLGeneratedShadowMap) ) then
    Result := nil;
end;

{ TGLTextureNodes ------------------------------------------------------------ }

function TGLTextureNodes.TextureNodeIndex(Node: TNodeX3DTextureNode): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].TextureNode = Node then Exit;
  Result := -1;
end;

function TGLTextureNodes.TextureNode(Node: TNodeX3DTextureNode): TGLTextureNode;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.TextureNode = Node then Exit;
  end;
  Result := nil;
end;

{ TGLSingleTextureNode ------------------------------------------------------- }

procedure TGLSingleTextureNode.EnableAll(
  const TextureUnitsCount: Cardinal;
  const UseMultiTexturing: boolean;
  out TexCoordsNeeded: Cardinal;
  var Primitives3DTextureCoords: boolean);
begin
  TexCoordsNeeded := 0;
  if (TextureUnitsCount > 0) and
     Enable(0, true, Primitives3DTextureCoords) then
  begin
    { TODO: this should be fixed, for non-multi tex decal is standard? }
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
    TexCoordsNeeded := 1;
  end;
end;

{ TGLMultiTextureNode -------------------------------------------------------- }

class function TGLMultiTextureNode.IsClassForTextureNode(Cache: TTexturesImagesVideosCache;
  Node: TNodeX3DTextureNode): boolean;
begin
  Result := Node is TNodeMultiTexture;
end;

function TGLMultiTextureNode.TextureNode: TNodeMultiTexture;
begin
  Result := TNodeMultiTexture(inherited TextureNode);
end;

procedure TGLMultiTextureNode.Prepare(State: TVRMLGraphTraverseState;
  var BumpMappingWanted: TBumpMappingWanted);

  procedure MaxTo1st(var A: TAlphaChannelType; const B: TAlphaChannelType);
  begin
    if B > A then A := B;
  end;

var
  ChildTex: TVRMLNode;
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

  for I := 0 to TextureNode.FdTexture.Count - 1 do
  begin
    ChildTex := TextureNode.FdTexture.Items[I];
    if (ChildTex <> nil) and
       (ChildTex is TNodeX3DTextureNode) then
    begin
      if ChildTex is TNodeMultiTexture then
        VRMLWarning(vwSerious, 'Child of MultiTexture node cannot be another MultiTexture node') else
      begin
        ChildPrepared := Renderer.PrepareTexture(State, TNodeX3DTextureNode(ChildTex));
        if ChildPrepared <> nil then
          MaxTo1st(AlphaChannelType, ChildPrepared.AlphaChannelType);
      end;
    end;
  end;
end;

procedure TGLMultiTextureNode.Unprepare;
var
  ChildTex: TVRMLNode;
  I: Integer;
begin
  for I := 0 to TextureNode.FdTexture.Count - 1 do
  begin
    ChildTex := TextureNode.FdTexture.Items[I];
    if (ChildTex <> nil) and
       (ChildTex is TNodeX3DTextureNode) then
      Renderer.UnprepareTexture(TNodeX3DTextureNode(ChildTex));
  end;
end;

function TGLMultiTextureNode.Enable(const TextureUnit: Cardinal;
  const UseForBumpMappingAllowed: boolean;
  var Primitives3DTextureCoords: boolean): boolean;
begin
  { This should never be called. TGLMultiTextureNode cannot set only
    texture unit. }
  Result := false;
end;

procedure TGLMultiTextureNode.EnableAll(
  const TextureUnitsCount: Cardinal;
  const UseMultiTexturing: boolean;
  out TexCoordsNeeded: Cardinal;
  var Primitives3DTextureCoords: boolean);
type
  TChannel = (cRGB, cAlpha);
  TGLIntPerChannel = array [TChannel] of TGLint;
  TCombinePerChannel = TGLIntPerChannel;
  TArgPerChannel = array [TChannel] of Integer;
  TScalePerChannel = array [TChannel] of TGLfloat;
  TStringPerChannel = array [TChannel] of string;

  { Simple type constructors, for ease of coding.
    Versions with only 1 argument set both channel (rgb and alpha) to the same. }
  function CombinePerChannel(const RGB, Alpha: TGLint): TCombinePerChannel;
  begin
    Result[cRGB] := RGB;
    Result[cAlpha] := Alpha;
  end;

  function CombinePerChannel(const Value: TGLint): TCombinePerChannel;
  begin
    Result := CombinePerChannel(Value, Value);
  end;

  function ArgPerChannel(const RGB, Alpha: Integer): TArgPerChannel;
  begin
    Result[cRGB] := RGB;
    Result[cAlpha] := Alpha;
  end;

  function ArgPerChannel(const Value: Integer): TArgPerChannel;
  begin
    Result := ArgPerChannel(Value, Value);
  end;

  function ScalePerChannel(const RGB, Alpha: TGLfloat): TScalePerChannel;
  begin
    Result[cRGB] := RGB;
    Result[cAlpha] := Alpha;
  end;

  function ScalePerChannel(const Value: TGLfloat): TScalePerChannel;
  begin
    Result := ScalePerChannel(Value, Value);
  end;

  { If S contains two separate modes (one for RGB, one for Alpha)
    returns @true and sets PerChannel to these separate strings.
    Strings returned in PerChannel will not contain the separator
    (slash, comma), and will not contain whitespace. }
  function SplitStringPerChannel(const S: string;
    out PerChannel: TStringPerChannel): boolean;
  var
    P: Integer;
  begin
    P := CharsPos(['/', ','], S);
    Result := P > 0;
    if Result then
    begin
      PerChannel[cRGB] := Trim(Copy(S, 1, P - 1));
      PerChannel[cAlpha] := Trim(SEnding(S, P + 1));
    end;
  end;

  { Return OpenGL values for
    GL_COMBINE_RGB_EXT, GL_COMBINE_ALPHA_EXT.

    Also, specify which argument (0, 1, 2 for
    GL_SOURCE0, GL_SOURCE1, GL_SOURCE2, or -1 if none) should represent
    current texture unit (this is "Arg1" in X3D spec wording),
    and which is the source (default is previous texture unit,
    although may change by "source" field; this is "Arg2" in X3D spec).

    You can assign here explicitly sources/operands for other
    arguments, if needed (e.g. "BLEND*" modes fill here 3rd source/operand
    directly).

    Also, specify Scale (remember OpenGL allows
    only 1.0, 2.0 and 4.0 scales). By default are 1. }
  procedure ModeFromString(const S: string;
    out Combine: TCombinePerChannel;
    out Arg1: TArgPerChannel;
    out Arg2: TArgPerChannel;
    var Scale: TScalePerChannel;
    var AlreadyHandled: boolean;
    var NeedsConstantColor: boolean);

    { Interpret simple mode name (this is for sure only one mode,
      without any "/" and whitespaces). This handles only the simplest
      modes, that behave the same and are allowed separately for
      both RGB and alpha channel.

      LS passed here must already be lowercase.

      Scale passed here must be initially 1.0. }
    procedure SimpleModeFromString(
      const LS: string;
      out Combine: TGLint;
      out Arg1, Arg2: Integer;
      var Scale: TGLfloat;
      const Channels: string);
    begin
      if LS = 'modulate' then
      begin
        Combine := GL_MODULATE;
        Arg1 := 0;
        Arg2 := 1;
      end else
      if LS = 'modulate2x' then
      begin
        Combine := GL_MODULATE;
        Arg1 := 0;
        Arg2 := 1;
        Scale := 2;
      end else
      if LS = 'modulate4x' then
      begin
        Combine := GL_MODULATE;
        Arg1 := 0;
        Arg2 := 1;
        Scale := 4;
      end else
      if (LS = 'replace') or (LS = 'selectarg1') then
      begin
        { SELECTARG1 is exactly the same as REPLACE.

          Note: don't get confused by X3D spec saying in table 18.3 that
          "REPLACE" takes the Arg2, that's an error, it takes
          from Arg1 to be consistent with other spec words.
          I wrote some remarks about this on
          http://vrmlengine.sourceforge.net/vrml_implementation_status.php }

        Combine := GL_REPLACE;
        Arg1 := 0;
        Arg2 := -1;
      end else
      if LS = 'selectarg2' then
      begin
        Combine := GL_REPLACE;
        Arg1 := -1;
        Arg2 := 0;
      end else
      if LS = 'add' then
      begin
        Combine := GL_ADD;
        Arg1 := 0;
        Arg2 := 1;
      end else
      if LS = 'addsigned' then
      begin
        Combine := GL_ADD_SIGNED_EXT;
        Arg1 := 0;
        Arg2 := 1;
      end else
      if LS = 'addsigned2x' then
      begin
        Combine := GL_ADD_SIGNED_EXT;
        Arg1 := 0;
        Arg2 := 1;
        Scale := 2;
      end else
      if LS = 'subtract' then
      begin
        Combine := GL_SUBTRACT;
        Arg1 := 0;
        Arg2 := 1;
      end else
      begin
        Combine := GL_MODULATE;
        Arg1 := 0;
        Arg2 := 1;
        VRMLWarning(vwSerious, Format('Not supported multi-texturing mode "%s" for channels "%s"', [LS, Channels]));
      end;
    end;

    procedure RGBModeFromString(
      const LS: string;
      out Combine: TGLint;
      out Arg1, Arg2: Integer;
      var Scale: TGLfloat);
    begin
      if LS = 'dotproduct3' then
      begin
        { We use DOT3_RGB_ARB here.
          This means it will fill only RGB values.

          This is our extension (X3D spec allows only DOTPRODUCT3
          for both channels, and to fill them both, this case is handled
          in BothModesFromString). }
        Combine := GL_DOT3_RGB_ARB;
        Arg1 := 0;
        Arg2 := 1;
      end else
        SimpleModeFromString(LS, Combine, Arg1, Arg2, Scale, 'RGB');
    end;

    procedure AlphaModeFromString(
      const LS: string;
      out Combine: TGLint;
      out Arg1, Arg2: Integer;
      var Scale: TGLfloat);
    begin
      SimpleModeFromString(LS, Combine, Arg1, Arg2, Scale, 'Alpha');
    end;

    procedure BothModesFromString(
      const LS: string;
      out Combine: TCombinePerChannel;
      out Arg1, Arg2: TArgPerChannel;
      var Scale: TScalePerChannel);
    begin
      if LS = '' then
      begin
        { LS = '' means that mode list was too short.
          X3D spec says explicitly that default mode is "MODULATE"
          in this case. (Accidentaly, this also will accept
          explict "" string as "MODULATE" --- not a worry, we don't
          have to produce error messages for all possible invalid VRMLs...). }

        Combine := CombinePerChannel(GL_MODULATE);
        Arg1 := ArgPerChannel(0);
        Arg2 := ArgPerChannel(1);
      end else
      if LS = 'off' then
      begin
        { For OFF, turn off the texture unit? This is the correct
          interpretation, right? }
        TGLTextureNode.TextureEnableDisable(etOff);
        AlreadyHandled := true;
      end else
      if LS = 'dotproduct3' then
      begin
        { We use DOT3_RGBA_ARB, not DOT3_RGB_ARB.
          See [http://www.opengl.org/registry/specs/ARB/texture_env_dot3.txt].

          This means that the dot (done on only RGB channels) will
          be replicated to all four channels (RGBA). This is exactly what
          the X3D specification requires, so we're happy.
          Yes, this means that COMBINE_ALPHA_ARB will be ignored. }

        Combine := CombinePerChannel(GL_DOT3_RGBA_ARB,
          GL_REPLACE { <- whatever, alpha combine will be ignored });
        Arg1 := ArgPerChannel(0);
        Arg2 := ArgPerChannel(1);
      end else
      if LS = 'blenddiffusealpha' then
      begin
        Combine := CombinePerChannel(GL_INTERPOLATE_EXT);
        Arg1 := ArgPerChannel(0);
        Arg2 := ArgPerChannel(1);

        { Whole source2 (both RGB and alpha) is filled by alpha of material
          (primary color). }
        glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE2_RGB_EXT, GL_PRIMARY_COLOR_EXT);
        glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND2_RGB_EXT, GL_SRC_ALPHA);
        glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE2_ALPHA_EXT, GL_PRIMARY_COLOR_EXT);
        glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND2_ALPHA_EXT, GL_SRC_ALPHA);
      end else
      if LS = 'blendtexturealpha' then
      begin
        Combine := CombinePerChannel(GL_INTERPOLATE_EXT);
        Arg1 := ArgPerChannel(0);
        Arg2 := ArgPerChannel(1);

        { Whole source2 (both RGB and alpha) is filled by alpha of current
          tex unit. }
        glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE2_RGB_EXT, GL_TEXTURE);
        glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND2_RGB_EXT, GL_SRC_ALPHA);
        glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE2_ALPHA_EXT, GL_TEXTURE);
        glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND2_ALPHA_EXT, GL_SRC_ALPHA);
      end else
      if LS = 'blendfactoralpha' then
      begin
        Combine := CombinePerChannel(GL_INTERPOLATE_EXT);
        Arg1 := ArgPerChannel(0);
        Arg2 := ArgPerChannel(1);

        { Whole source2 (both RGB and alpha) is filled by const alpha. }
        NeedsConstantColor := true;
        glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE2_RGB_EXT, GL_CONSTANT_EXT);
        glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND2_RGB_EXT, GL_SRC_ALPHA);
        glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE2_ALPHA_EXT, GL_CONSTANT_EXT);
        glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND2_ALPHA_EXT, GL_SRC_ALPHA);
      end else
      if LS = 'blendcurrentalpha' then
      begin
        Combine := CombinePerChannel(GL_INTERPOLATE_EXT);
        Arg1 := ArgPerChannel(0);
        Arg2 := ArgPerChannel(1);

        { Whole source2 (both RGB and alpha) is filled by alpha from prev tex. }
        glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE2_RGB_EXT, GL_PREVIOUS_EXT);
        glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND2_RGB_EXT, GL_SRC_ALPHA);
        glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE2_ALPHA_EXT, GL_PREVIOUS_EXT);
        glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND2_ALPHA_EXT, GL_SRC_ALPHA);
      end else
      begin
        SimpleModeFromString(LS,
          Combine[cRGB], Arg1[cRGB], Arg2[cRGB], Scale[cRGB], 'both RGB and Alpha');
        Combine[cAlpha] := Combine[cRGB];
        Arg1   [cAlpha] := Arg1   [cRGB];
        Arg2   [cAlpha] := Arg2   [cRGB];
        Scale  [cAlpha] := Scale  [cRGB];
      end;
    end;

  var
    LS: string;
    StringPerChannel: TStringPerChannel;
  begin
    LS := LowerCase(S);
    if SplitStringPerChannel(LS, StringPerChannel) then
    begin
      RGBModeFromString(StringPerChannel[cRGB], Combine[cRGB], Arg1[cRGB], Arg2[cRGB], Scale[cRGB]);
      AlphaModeFromString(StringPerChannel[cAlpha], Combine[cAlpha], Arg1[cAlpha], Arg2[cAlpha], Scale[cAlpha]);
    end else
      BothModesFromString(LS, Combine, Arg1, Arg2, Scale);
  end;

  procedure SourceFromString(const S: string; out Source: TGLIntPerChannel;
    var NeedsConstantColor: boolean);

    procedure SimpleSourceFromString(const LS: string;
      out Source: TGLint;
      var NeedsConstantColor: boolean);
    begin
      if LS = '' then
        Source := GL_PREVIOUS_EXT else
      if (LS = 'diffuse') or (LS = 'specular') then
        Source := GL_PRIMARY_COLOR_EXT else
      if LS = 'factor' then
      begin
        NeedsConstantColor := true;
        Source := GL_CONSTANT_EXT;
      end else
      begin
        Source := GL_PREVIOUS_EXT;
        VRMLWarning(vwSerious, Format('Not supported multi-texturing source "%s"', [LS]))
      end;
    end;

  var
    LS: string;
    SourcePerChannel: TStringPerChannel;
  begin
    LS := LowerCase(S);
    if SplitStringPerChannel(LS, SourcePerChannel) then
    begin
      SimpleSourceFromString(SourcePerChannel[cRGB  ], Source[cRGB  ], NeedsConstantColor);
      SimpleSourceFromString(SourcePerChannel[cAlpha], Source[cAlpha], NeedsConstantColor);
    end else
    begin
      SimpleSourceFromString(LS, Source[cRGB], NeedsConstantColor);
      Source[cAlpha] := Source[cRGB];
    end;
  end;

  procedure DisableTexture(const TextureUnit: Cardinal);
  begin
    Renderer.ActiveTexture(TextureUnit);
    TGLTextureNode.TextureEnableDisable(etOff);
  end;

var
  ChildTex: TVRMLNode;
  I: Integer;
  Success: boolean;
  Combine: TCombinePerChannel;
  Arg1, Arg2: TArgPerChannel;
  Scale: TScalePerChannel;
  S: string;
  AlreadyHandled: boolean;
  NeedsConstantColor: boolean;
  Source: TGLIntPerChannel;
begin
  { calculate TexCoordsNeeded }
  TexCoordsNeeded := Min(TextureUnitsCount, TextureNode.FdTexture.Count);
  if not UseMultiTexturing then
    MinTo1st(TexCoordsNeeded, 1);

  for I := 0 to TexCoordsNeeded - 1 do
  begin
    ChildTex := TextureNode.FdTexture.Items.Items[I];
    Success := false;

    if (ChildTex <> nil) and
       (ChildTex is TNodeX3DTextureNode) then
    begin
      if ChildTex is TNodeMultiTexture then
        VRMLWarning(vwSerious, 'Child of MultiTexture node cannot be another MultiTexture node') else
        Success := Renderer.EnableTexture(TNodeX3DTextureNode(ChildTex),
          I, false, Primitives3DTextureCoords);

      if Success then
      begin
        { Set all the multitexture mode-related stuff.
          Below we handle TextureNode.mode, source, color, alpha,
          function fields. }

        AlreadyHandled := false;
        Scale[cRGB] := 1;
        Scale[cAlpha] := 1;
        NeedsConstantColor := false;

        if I < TextureNode.FdMode.Count then
          S := TextureNode.FdMode.Items[I] else
          S := '';

        ModeFromString(S, Combine, Arg1, Arg2,
          Scale, AlreadyHandled, NeedsConstantColor);

        if not AlreadyHandled then
        begin
          glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_EXT);

          glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_EXT, Combine[cRGB]);
          glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_ALPHA_EXT, Combine[cAlpha]);

          glTexEnvf(GL_TEXTURE_ENV, GL_RGB_SCALE_EXT, Scale[cRGB]);
          glTexEnvf(GL_TEXTURE_ENV, GL_ALPHA_SCALE, Scale[cAlpha]);

          { Set Arg1 as source.
            First argument is always current texture unit.
            (This is indicated by X3D spec wording
            "The source field determines the colour source for the second argument.") }

          if Arg1[cRGB] <> -1 then
          begin
            glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_EXT + Arg1[cRGB], GL_TEXTURE);
            glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_EXT + Arg1[cRGB], GL_SRC_COLOR);
          end;

          if Arg1[cAlpha] <> -1 then
          begin
            glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_ALPHA_EXT + Arg1[cAlpha], GL_TEXTURE);
            glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_ALPHA_EXT + Arg1[cAlpha], GL_SRC_ALPHA);
          end;

          if (Arg2[cRGB] <> -1) or (Arg2[cAlpha] <> -1) then
          begin
            if I < TextureNode.FdSource.Count then
              S := TextureNode.FdSource.Items[I] else
              S := '';

            SourceFromString(S, Source, NeedsConstantColor);

            if Arg2[cRGB] <> -1 then
            begin
              glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_EXT + Arg2[cRGB], Source[cRGB]);
              glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_EXT + Arg2[cRGB], GL_SRC_COLOR);
            end;

            if Arg2[cAlpha] <> -1 then
            begin
              glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_ALPHA_EXT + Arg2[cAlpha], Source[cAlpha]);
              glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_ALPHA_EXT + Arg2[cAlpha], GL_SRC_ALPHA);
            end;
          end;

          if NeedsConstantColor then
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
      DisableTexture(I);
  end;
end;

{ TGLImageTextureNode -------------------------------------------------------- }

class function TGLImageTextureNode.IsClassForTextureNode(Cache: TTexturesImagesVideosCache;
  Node: TNodeX3DTextureNode): boolean;
begin
  if Node is TVRMLTextureNode then
  begin
    { You have to set Cache before using IsTextureImage. }
    TVRMLTextureNode(Node).Cache := Cache;
    Result := TVRMLTextureNode(Node).IsTextureImage;
  end else
    Result := false;
end;

function TGLImageTextureNode.TextureNode: TVRMLTextureNode;
begin
  Result := TVRMLTextureNode(inherited TextureNode);
end;

procedure TGLImageTextureNode.Prepare(State: TVRMLGraphTraverseState;
  var BumpMappingWanted: TBumpMappingWanted);
var
  HeightMapGrayscale: TGrayscaleImage;
  OriginalTexture: TEncodedImage;
  MinFilter, MagFilter: TGLint;
  Anisotropy: TGLfloat;
  TextureWrap: TTextureWrap2D;
begin
  { HandledNode already made sure IsTextureImage = @true }

  HandleTexturePropertiesCore(TextureNode.TextureProperties,
    MinFilter, MagFilter, Anisotropy);

  TextureWrap[0] := TextureRepeatToGL[TextureNode.RepeatS];
  TextureWrap[1] := TextureRepeatToGL[TextureNode.RepeatT];

  try
    GLName := Renderer.Cache.TextureImage_IncReference(
      TextureNode.TextureImage,
      TextureNode.TextureUsedFullUrl,
      TextureNode,
      MinFilter,
      MagFilter,
      Anisotropy,
      TextureWrap,
      Renderer.Attributes.ColorModulatorByte,
      { This way, our AlphaChannelType is calculated (or taken from cache)
        by TextureImage_IncReference }
      AlphaChannelType);
  except
    on E: ETextureLoadError do
    begin
      VRMLWarning(vwIgnorable, 'Cannot load 2D texture to OpenGL: ' + E.Message);
      Exit;
    end;
  end;

  { TODO: for now, bump mapping is used only if the node has normal texture
    too. It should be possible to use bump mapping even if the node is colored
    only by material (in this case we should remember to still generate
    texture coords etc.).

    TODO: Also, now for each texture, there must always be the same bump map
    (since we store it in the same TextureImageReference }

  NormalMap := 0;
  if (Renderer.BumpMappingMethod <> bmNone) and
     (State.ParentShape <> nil) and
     (State.ParentShape.NormalMap <> nil) then
  begin
    State.ParentShape.NormalMap.Cache := Renderer.Cache;
    if State.ParentShape.NormalMap.IsTextureImage then
    begin
      { TODO: normal map textures should be shared by Cache }
      NormalMap := LoadGLTexture(
        State.ParentShape.NormalMap.TextureImage,
        GL_LINEAR_MIPMAP_LINEAR, GL_LINEAR, TextureWrap);
    end;
  end;

  HeightMap := 0;
  if (Renderer.BumpMappingMethod <> bmNone) and
     (State.ParentShape <> nil) and
     (State.ParentShape.HeightMap <> nil) then
  begin
    State.ParentShape.HeightMap.Cache := Renderer.Cache;
    if State.ParentShape.HeightMap.IsTextureImage then
    begin
      { TODO: height map textures should be shared by Cache }

      OriginalTexture := State.ParentShape.HeightMap.TextureImage;

      { Calculate HeightMapGrayscale }
      { TODO: this is not nice to convert here, we should load
        straight to TGrayscalaImage }
      if OriginalTexture is TRGBImage then
        HeightMapGrayscale := TRGBImage(OriginalTexture).ToGrayscale else
      if OriginalTexture is TGrayscaleImage then
        HeightMapGrayscale := TGrayscaleImage(OriginalTexture) else
        HeightMapGrayscale := nil;

      if HeightMapGrayscale <> nil then
      try
        HeightMap := LoadGLTexture(HeightMapGrayscale,
            GL_LINEAR_MIPMAP_LINEAR, GL_LINEAR, TextureWrap);
        HeightMapScale := State.ParentShape.HeightMapScale;
      finally
        if HeightMapGrayscale <> OriginalTexture then
          FreeAndNil(HeightMapGrayscale);
      end;
    end;
  end;

  if NormalMap <> 0 then
  begin
    if (HeightMap <> 0) and
       (Renderer.BumpMappingMethod >= bmGLSLParallax) then
      BumpMappingWanted := bmwParallax else
      BumpMappingWanted := bmwNoParallax;
  end;
end;

procedure TGLImageTextureNode.Unprepare;
begin
  if GLName <> 0 then
    Renderer.Cache.TextureImage_DecReference(GLName);
end;

function TGLImageTextureNode.Enable(const TextureUnit: Cardinal;
  const UseForBumpMappingAllowed: boolean;
  var Primitives3DTextureCoords: boolean): boolean;

  procedure EnableClassicTexturing(GLTexture: TGLuint);
  begin
    Renderer.ActiveTexture(TextureUnit);
    TextureEnableDisable(et2D);

    glBindTexture(GL_TEXTURE_2D, GLTexture);
  end;

var
  IndexedFaceRenderer: TIndexedFaceSetRenderer;
  MeshRenderer: TVRMLMeshRenderer;
begin
  Result := GLName <> 0;
  if not Result then Exit;

  MeshRenderer := TVRMLMeshRenderer(Renderer.ExposedMeshRenderer);

  if UseForBumpMappingAllowed and
     (MeshRenderer <> nil) and
     MeshRenderer.BumpMappingAllowed and
     (Renderer.BumpMappingMethod <> bmNone) then
  begin
    if NormalMap <> 0 then
    begin
      MeshRenderer.BumpMappingMethod := Renderer.BumpMappingMethod;
      Assert(MeshRenderer is TIndexedFaceSetRenderer,
        'We assumed that only TIndexedFaceSetRenderer may actually have BumpMappingMethod <> bmNone');
      IndexedFaceRenderer := TIndexedFaceSetRenderer(MeshRenderer);
      IndexedFaceRenderer.TexNormalizationCube := Renderer.TexNormalizationCube;
      IndexedFaceRenderer.TexOriginal := GLName;
      IndexedFaceRenderer.TexOriginalAlpha := AlphaChannelType = atSimpleYesNo;
      IndexedFaceRenderer.TexNormalMap := NormalMap;
      IndexedFaceRenderer.TexHeightMap := HeightMap;
      IndexedFaceRenderer.TexHeightMapScale := HeightMapScale;
      { use parallax only if the model actually has heightMap }
      if (IndexedFaceRenderer.TexHeightMap = 0) and
         (MeshRenderer.BumpMappingMethod >= bmGLSLParallax) then
        MeshRenderer.BumpMappingMethod := bmGLSLNormal;

      { Bind specified TextureUnit. }
      Renderer.ActiveTexture(TextureUnit);
    end else
      EnableClassicTexturing(GLName);
  end else
    EnableClassicTexturing(GLName);

  Result := true;
end;

{ TGLMovieTextureNode -------------------------------------------------------- }

class function TGLMovieTextureNode.IsClassForTextureNode(Cache: TTexturesImagesVideosCache;
  Node: TNodeX3DTextureNode): boolean;
begin
  { Although for most code TGLMovieTextureNode, it would be enought
    to have any TVRMLTextureNode with IsTextureVideo = @true.
    For when rendering, we'll need some TNodeMovieTexture properties
    to choose video frame.

    Anyway, TNodeMovieTexture is for now the only texture node possible
    that may have IsTextureVideo = @true, so it's not a real problem for now. }

  if Node is TNodeMovieTexture then
  begin
    { You have to set Cache before using IsTextureImage. }
    TNodeMovieTexture(Node).Cache := Cache;
    Result := TNodeMovieTexture(Node).IsTextureVideo;
  end else
    Result := false;
end;

function TGLMovieTextureNode.TextureNode: TNodeMovieTexture;
begin
  Result := TNodeMovieTexture(inherited TextureNode);
end;

procedure TGLMovieTextureNode.Prepare(State: TVRMLGraphTraverseState;
  var BumpMappingWanted: TBumpMappingWanted);
var
  MinFilter, MagFilter: TGLint;
  Anisotropy: TGLfloat;
  TextureWrap: TTextureWrap2D;
begin
  { HandledNode already made sure IsTextureVideo = @true }

  HandleTexturePropertiesCore(TextureNode.TextureProperties,
    MinFilter, MagFilter, Anisotropy);

  TextureWrap[0] := TextureRepeatToGL[TextureNode.RepeatS];
  TextureWrap[1] := TextureRepeatToGL[TextureNode.RepeatT];

  GLVideo := Renderer.Cache.TextureVideo_IncReference(
    TextureNode.TextureVideo,
    TextureNode.TextureUsedFullUrl,
    TextureNode,
    MinFilter,
    MagFilter,
    Anisotropy,
    TextureWrap,
    Renderer.Attributes.ColorModulatorByte,
    { This way, our AlphaChannelType is calculated (or taken from cache)
      by TextureVideo_IncReference }
    AlphaChannelType);
end;

procedure TGLMovieTextureNode.Unprepare;
begin
  if GLVideo <> nil then
    Renderer.Cache.TextureVideo_DecReference(GLVideo);
end;

function TGLMovieTextureNode.Enable(const TextureUnit: Cardinal;
  const UseForBumpMappingAllowed: boolean;
  var Primitives3DTextureCoords: boolean): boolean;
var
  VideoTime: TKamTime;
begin
  { Note: don't call IsTextureImage, IsTextureVideo here --- this
    would cause reloading images/videos, nullifying
    TVRMLScene.FreeResources([frTextureDataInNodes]) purpose.

    Actually, it would be safe to call this for non-MovieTexture nodes,
    as they should be prepared to display lists before doing
    FreeResources. But for MovieTexture nodes it's forbidden,
    as it's called at every frame render. }

  Result := GLVideo <> nil;
  if not Result then Exit;

  VideoTime := TextureNode.TimeDependentNodeHandler.ElapsedTime *
               TextureNode.FdSpeed.Value;
  if TextureNode.FdSpeed.Value < 0 then
    VideoTime := TextureNode.Duration + VideoTime;

  Renderer.ActiveTexture(TextureUnit);
  TextureEnableDisable(et2D);

  glBindTexture(GL_TEXTURE_2D, GLVideo.GLTextureFromTime(VideoTime));

  Result := true;
end;

{ TGLRenderedTextureNode ----------------------------------------------------- }

class function TGLRenderedTextureNode.IsClassForTextureNode(Cache: TTexturesImagesVideosCache;
  Node: TNodeX3DTextureNode): boolean;
begin
  Result := Node is TNodeRenderedTexture;
end;

function TGLRenderedTextureNode.TextureNode: TNodeRenderedTexture;
begin
  Result := TNodeRenderedTexture(inherited TextureNode);
end;

procedure TGLRenderedTextureNode.Prepare(State: TVRMLGraphTraverseState;
  var BumpMappingWanted: TBumpMappingWanted);
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
    VRMLWarning(vwIgnorable { This may be caused by OpenGL implementation
      limits, so it may be impossible to predict by VRML author,
      so it's "ignorable" warning. },
      'OpenGL implementation doesn''t allow any glGenerateMipmap* version, so you cannot use mipmaps for RenderedTexture');
    MinFilter := GL_LINEAR;
    NeedsMipmaps := false;
  end;

  TextureWrap[0] := TextureRepeatToGL[TextureNode.FdRepeatS.Value];
  TextureWrap[1] := TextureRepeatToGL[TextureNode.FdRepeatT.Value];

  { calculate Width, Height }
  if TextureNode.FdDimensions.Items.High >= 0 then
    NodeWidth := Max(TextureNode.FdDimensions.Items[0], 0) else
    NodeWidth := DefaultRenderedTextureWidth;
  if TextureNode.FdDimensions.Items.High >= 1 then
    NodeHeight := Max(TextureNode.FdDimensions.Items[1], 0) else
    NodeHeight := DefaultRenderedTextureHeight;
  Width  := NodeWidth ;
  Height := NodeHeight;
  if not IsTextureSized(Width, Height) then
  begin
    ResizeToTextureSize(Width, Height);
    VRMLWarning(vwIgnorable { This may be caused by OpenGL implementation
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
      VRMLWarning(vwSerious, 'Your OpenGL doesn''t support ARB_depth_texture, cannot use RenderedTexture with depthMap = TRUE');
      Exit;
    end;

    GLName := Renderer.Cache.TextureDepth_IncReference(
      TextureNode, TextureWrap, nil, Width, Height);
  end else
  begin
    InitialImage := TRGBImage.Create(Width, Height);
    try
      { Fill with deliberately stupid (but constant) color,
        to recognize easily RenderedTexture which don't have textures
        updated. }
      InitialImage.Clear(Vector4Byte(255, 0, 255, 255));

      try
        GLName := Renderer.Cache.TextureImage_IncReference(
          InitialImage,
          '',
          TextureNode,
          MinFilter,
          MagFilter,
          Anisotropy,
          TextureWrap,
          Renderer.Attributes.ColorModulatorByte,
          { This way, our AlphaChannelType is calculated (or taken from cache)
            by TextureImage_IncReference }
          AlphaChannelType);
      except
        on E: ETextureLoadError do
        begin
          VRMLWarning(vwIgnorable, 'Cannot load 2D texture to OpenGL: ' + E.Message);
          Exit;
        end;
      end;

      { RenderedTexture never has any normal / height map
        (Hm, although it would be possible to generate some in theory
        --- after all, we generate it from 3D data. Idea for the future.)
      NormalMap := 0;
      HeightMap := 0;
      }
    finally FreeAndNil(InitialImage) end;
  end;
end;

procedure TGLRenderedTextureNode.Unprepare;
begin
  if GLName <> 0 then
    if DepthMap then
      Renderer.Cache.TextureDepth_DecReference(GLName) else
      Renderer.Cache.TextureImage_DecReference(GLName);
end;

function TGLRenderedTextureNode.Enable(const TextureUnit: Cardinal;
  const UseForBumpMappingAllowed: boolean;
  var Primitives3DTextureCoords: boolean): boolean;
begin
  Result := GLName <> 0;
  if not Result then Exit;

  Renderer.ActiveTexture(TextureUnit);
  glBindTexture(GL_TEXTURE_2D, GLName);
  TextureEnableDisable(et2D);
end;

{ TGLCubeMapTextureNode ------------------------------------------------------ }

function TGLCubeMapTextureNode.TextureNode: TNodeX3DEnvironmentTextureNode;
begin
  Result := TNodeX3DEnvironmentTextureNode(inherited TextureNode);
end;

procedure TGLCubeMapTextureNode.Unprepare;
begin
  Renderer.Cache.TextureCubeMap_DecReference(GLName);
end;

function TGLCubeMapTextureNode.Enable(const TextureUnit: Cardinal;
  const UseForBumpMappingAllowed: boolean;
  var Primitives3DTextureCoords: boolean): boolean;
begin
  Result := GLName <> 0;
  if not Result then Exit;

  Renderer.ActiveTexture(TextureUnit);
  glBindTexture(GL_TEXTURE_CUBE_MAP_ARB, GLName);
  TextureEnableDisable(etCubeMap);
end;

{ TGLComposedCubeMapTextureNode ---------------------------------------------- }

class function TGLComposedCubeMapTextureNode.IsClassForTextureNode(Cache: TTexturesImagesVideosCache;
  Node: TNodeX3DTextureNode): boolean;
begin
  Result := Node is TNodeComposedCubeMapTexture;
end;

function TGLComposedCubeMapTextureNode.TextureNode: TNodeComposedCubeMapTexture;
begin
  Result := TNodeComposedCubeMapTexture(inherited TextureNode);
end;

procedure TGLComposedCubeMapTextureNode.Prepare(State: TVRMLGraphTraverseState;
  var BumpMappingWanted: TBumpMappingWanted);

  { Checks is given side has non-nil valid node class,
    and then if image there can be loaded. }
  function SideLoaded(SideField: TSFNode): boolean;
  var
    SideTex: TVRMLTextureNode;
  begin
    Result :=
      (SideField.Value <> nil) and
      (SideField.Value is TVRMLTextureNode);
    if Result then
    begin
      SideTex := TVRMLTextureNode(SideField.Value);
      SideTex.Cache := Renderer.Cache;
      Result := SideTex.IsTextureImage;

      if Result and not (SideTex.TextureImage is TImage) then
      begin
        VRMLWarning(vwIgnorable, 'ComposedCubeMapTexture cannot contain S3TC images, as we have to rotate images within, and we cannot do this (fast) with compressed textures');
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
    VRMLWarning(vwSerious, 'Your OpenGL doesn''t support ARB_texture_cube_map, cannot use CubeMapTexture');
    Exit;
  end;

  if not (SideLoaded(TextureNode.FdBack) and
     SideLoaded(TextureNode.FdBottom) and
     SideLoaded(TextureNode.FdFront) and
     SideLoaded(TextureNode.FdLeft) and
     SideLoaded(TextureNode.FdRight) and
     SideLoaded(TextureNode.FdTop)) then
  begin
    VRMLWarning(vwSerious, 'Not all sides of a CubeMapTexture are correctly set and loaded, cannot use cube map');
    Exit;
  end;

  HandleTextureProperties(TextureNode.FdTextureProperties.Value,
    MinFilter, MagFilter, Anisotropy);

  try
    { To match expected orientation for OpenGL, we have to rotate images.
      (source images are oriented as for VRML Background.)
      We safely cast them to TImage below, SideLoaded above checked
      that they are indeed of TImage class. }
    BackRot  := (TVRMLTextureNode(TextureNode.FdBack .Value).TextureImage as TImage).MakeRotated(2);
    FrontRot := (TVRMLTextureNode(TextureNode.FdFront.Value).TextureImage as TImage).MakeRotated(2);
    LeftRot  := (TVRMLTextureNode(TextureNode.FdLeft .Value).TextureImage as TImage).MakeRotated(2);
    RightRot := (TVRMLTextureNode(TextureNode.FdRight.Value).TextureImage as TImage).MakeRotated(2);

    GLName := Renderer.Cache.TextureCubeMap_IncReference(
      TextureNode,
      MinFilter, MagFilter, Anisotropy,
      { positive x } RightRot,
      { negative x } LeftRot,
      { positive y } TVRMLTextureNode(TextureNode.FdTop   .Value).TextureImage as TImage,
      { negative y } TVRMLTextureNode(TextureNode.FdBottom.Value).TextureImage as TImage,
      { positive z } BackRot,
      { negative z } FrontRot,
      AlphaChannelType);
  finally
    FreeAndNil(BackRot);
    FreeAndNil(FrontRot);
    FreeAndNil(LeftRot);
    FreeAndNil(RightRot);
  end;
end;

{ TGLImageCubeMapTextureNode ------------------------------------------------- }

class function TGLImageCubeMapTextureNode.IsClassForTextureNode(Cache: TTexturesImagesVideosCache;
  Node: TNodeX3DTextureNode): boolean;
begin
  Result := Node is TNodeImageCubeMapTexture;
end;

function TGLImageCubeMapTextureNode.TextureNode: TNodeImageCubeMapTexture;
begin
  Result := TNodeImageCubeMapTexture(inherited TextureNode);
end;

procedure TGLImageCubeMapTextureNode.Prepare(State: TVRMLGraphTraverseState;
  var BumpMappingWanted: TBumpMappingWanted);
var
  MinFilter, MagFilter: TGLint;
  Anisotropy: TGLfloat;
  DDS: TDDSImage;
begin
  if not GL_ARB_texture_cube_map then
  begin
    VRMLWarning(vwSerious, 'Your OpenGL doesn''t support ARB_texture_cube_map, cannot use CubeMapTexture');
    Exit;
  end;

  DDS := TextureNode.LoadImage;
  { If TextureNode doesn't contain anything useful, just exit.
    TextureNode.LoadImage already did necessary VRMLWarnings. }
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
      - We crash ("as") on S3TC compressed cube maps.
    }

    GLName := Renderer.Cache.TextureCubeMap_IncReference(
      TextureNode,
      MinFilter, MagFilter, Anisotropy,
      DDS.CubeMapImage(dcsPositiveX) as TImage,
      DDS.CubeMapImage(dcsNegativeX) as TImage,
      { Swap meaning of positive/negative Y faces from DDS,
        see TDDSCubeMapSide for explanation. }
      DDS.CubeMapImage(dcsNegativeY) as TImage,
      DDS.CubeMapImage(dcsPositiveY) as TImage,
      DDS.CubeMapImage(dcsPositiveZ) as TImage,
      DDS.CubeMapImage(dcsNegativeZ) as TImage,
      AlphaChannelType);
  finally FreeAndNil(DDS); end;
end;

{ TGLGeneratedCubeMapTextureNode --------------------------------------------- }

class function TGLGeneratedCubeMapTextureNode.IsClassForTextureNode(Cache: TTexturesImagesVideosCache;
  Node: TNodeX3DTextureNode): boolean;
begin
  Result := Node is TNodeGeneratedCubeMapTexture;
end;

function TGLGeneratedCubeMapTextureNode.TextureNode: TNodeGeneratedCubeMapTexture;
begin
  Result := TNodeGeneratedCubeMapTexture(inherited TextureNode);
end;

procedure TGLGeneratedCubeMapTextureNode.Prepare(State: TVRMLGraphTraverseState;
  var BumpMappingWanted: TBumpMappingWanted);
var
  MinFilter, MagFilter: TGLint;
  Anisotropy: TGLfloat;
  InitialImage: TImage;
begin
  if not GL_ARB_texture_cube_map then
  begin
    VRMLWarning(vwSerious, 'Your OpenGL doesn''t support ARB_texture_cube_map, cannot use CubeMapTexture');
    Exit;
  end;

  HandleTextureProperties(TextureNode.FdTextureProperties.Value,
    MinFilter, MagFilter, Anisotropy);

  { calculate MinFilter, MagFilter, Anisotropy, NeedsMipmaps }
  NeedsMipmaps := TextureMinFilterNeedsMipmaps(MinFilter);
  if NeedsMipmaps and not HasGenerateMipmap then
  begin
    VRMLWarning(vwIgnorable { This may be caused by OpenGL implementation
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
    VRMLWarning(vwIgnorable { This may be caused by OpenGL implementation
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

    GLName := Renderer.Cache.TextureCubeMap_IncReference(
      TextureNode,
      MinFilter, MagFilter, Anisotropy,
      InitialImage, InitialImage,
      InitialImage, InitialImage,
      InitialImage, InitialImage,
      AlphaChannelType);
  finally FreeAndNil(InitialImage) end;
end;

{ TGL3DTextureNode ----------------------------------------------------------- }

class function TGL3DTextureNode.IsClassForTextureNode(Cache: TTexturesImagesVideosCache;
  Node: TNodeX3DTextureNode): boolean;
begin
  Result := Node is TNodeX3DTexture3DNode;
end;

function TGL3DTextureNode.TextureNode: TNodeX3DTexture3DNode;
begin
  Result := TNodeX3DTexture3DNode(inherited TextureNode);
end;

procedure TGL3DTextureNode.Prepare(State: TVRMLGraphTraverseState;
  var BumpMappingWanted: TBumpMappingWanted);
var
  MinFilter, MagFilter: TGLint;
  Anisotropy: TGLfloat;
  TextureWrap: TTextureWrap3D;
begin
  if not GL_EXT_texture3D then
  begin
    VRMLWarning(vwSerious, 'Your OpenGL doesn''t support EXT_texture3D, cannot use Texture3D nodes');
    Exit;
  end;

  TextureNode.Cache := Renderer.Cache;
  TextureNode.TextureLoaded := true;

  { If TextureImage doesn't contain anything useful, just exit.
    Setting TextureLoaded already did necessary VRMLWarnings. }
  if TextureNode.TextureImage = nil then Exit;

  HandleTextureProperties(TextureNode.FdTextureProperties.Value,
    MinFilter, MagFilter, Anisotropy);

  { calculate TextureWrap }
  TextureWrap[0] := TextureRepeatToGL[TextureNode.FdRepeatS.Value];
  TextureWrap[1] := TextureRepeatToGL[TextureNode.FdRepeatT.Value];
  TextureWrap[2] := TextureRepeatToGL[TextureNode.FdRepeatR.Value];

  { TODO: this is a quick and dirty method:
    - We do not use texture 3d mipmaps stored inside DDS file.
    - We crash (because of "Image as TImage") on S3TC compressed images.
      (although DDS doesn't allow compressed 3d textures, so this
      is not so important now.)
  }

  try
    GLName := Renderer.Cache.Texture3D_IncReference(
      TextureNode, MinFilter, MagFilter, Anisotropy,
      TextureWrap, TextureNode.TextureImage as TImage,
      AlphaChannelType);
  except
    on E: ETextureLoadError do
    begin
      VRMLWarning(vwIgnorable, 'Cannot load 3D texture to OpenGL: ' + E.Message);
      Exit;
    end;
  end;
end;

procedure TGL3DTextureNode.Unprepare;
begin
  if GLName <> 0 then
    Renderer.Cache.Texture3D_DecReference(GLName);
end;

function TGL3DTextureNode.Enable(const TextureUnit: Cardinal;
  const UseForBumpMappingAllowed: boolean;
  var Primitives3DTextureCoords: boolean): boolean;
begin
  Result := GLName <> 0;
  if not Result then Exit;

  Primitives3DTextureCoords := true;

  Renderer.ActiveTexture(TextureUnit);
  glBindTexture(GL_TEXTURE_3D_EXT, GLName);
  TextureEnableDisable(et3D);
end;

{ TGLGeneratedShadowMap ------------------------------------------------------ }

class function TGLGeneratedShadowMap.IsClassForTextureNode(Cache: TTexturesImagesVideosCache;
  Node: TNodeX3DTextureNode): boolean;
begin
  Result := Node is TNodeGeneratedShadowMap;
end;

function TGLGeneratedShadowMap.TextureNode: TNodeGeneratedShadowMap;
begin
  Result := TNodeGeneratedShadowMap(inherited TextureNode);
end;

procedure TGLGeneratedShadowMap.Prepare(State: TVRMLGraphTraverseState;
  var BumpMappingWanted: TBumpMappingWanted);
var
  TextureWrap: TTextureWrap2D;
begin
  if not GL_ARB_depth_texture then
  begin
    VRMLWarning(vwSerious, 'Your OpenGL doesn''t support ARB_depth_texture, cannot use GeneratedShadowMap nodes');
    Exit;
  end;

  { TODO: fix TextureNode.FdSize.Value if needed }
  Size := TextureNode.FdSize.Value;

  { In this case, clamp to border is Ok? TODO: test here }
  TextureWrap[0] := GL_CLAMP;
  TextureWrap[1] := GL_CLAMP;

  GLName := Renderer.Cache.TextureDepth_IncReference(
    TextureNode, TextureWrap, TextureNode.FdCompareMode, Size, Size);
end;

procedure TGLGeneratedShadowMap.Unprepare;
begin
  if GLName <> 0 then
    Renderer.Cache.TextureDepth_DecReference(GLName);
end;

function TGLGeneratedShadowMap.Enable(const TextureUnit: Cardinal;
  const UseForBumpMappingAllowed: boolean;
  var Primitives3DTextureCoords: boolean): boolean;
begin
  Result := GLName <> 0;
  if not Result then Exit;

  Renderer.ActiveTexture(TextureUnit);
  glBindTexture(GL_TEXTURE_2D, GLName);
  TextureEnableDisable(et2D);
end;

{$endif read_implementation}
