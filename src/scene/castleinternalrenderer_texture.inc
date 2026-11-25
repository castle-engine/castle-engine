{%MainUnit castleinternalrenderer.pas}
{
  Copyright 2002-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { }
  TTextureResource = class;

  TTextureResourceClass = class of TTextureResource;

  { OpenGL resource of a texture node. }
  TTextureResource = class(TRendererResource)
  protected
    FRHISampler: TRHISampler;
    FPreparedForRHI: Boolean;
    procedure CreateRHISampler(
      const AFilter: TTextureFilter;
      const ABoundaryS, ABoundaryT, ABoundaryR: TGLenum;
      const AAnisotropy: Single;
      const AMipLevels: Cardinal);
    { Calculate things from TextureProperties node.
      If TextureProperties = @nil, they are taken from defaults
      (possibly in RenderOptions) and BoundaryModeS/-T are taken
      from repeatS and repeatT fields.
      @groupBegin }
    procedure HandleTextureProperties(
      const TextureProperties: TTexturePropertiesNode;
      const RenderOptions: TCastleRenderOptions;
      const RepeatS, RepeatT, RepeatR: TGLenum;
      out Filter: TTextureFilter;
      out BoundaryModeS, BoundaryModeT, BoundaryModeR: TGLenum;
      out Anisotropy: TGLfloat;
      out GUITexture: boolean); overload;

    procedure HandleTextureProperties(
      const TextureProperties: TTexturePropertiesNode;
      const RenderOptions: TCastleRenderOptions;
      const RepeatS, RepeatT: TGLenum;
      out Filter: TTextureFilter;
      out BoundaryModeS, BoundaryModeT: TGLenum;
      out Anisotropy: TGLfloat;
      out GUITexture: boolean); overload;

    procedure HandleTextureProperties(
      const TextureProperties: TTexturePropertiesNode;
      const RenderOptions: TCastleRenderOptions;
      out Filter: TTextureFilter;
      out Anisotropy: TGLfloat;
      out GUITexture: boolean); overload;
    { @groupEnd }

    { Decide if this class can handle given texture Node. }
    class function IsClassForTextureNode(
      ANode: TAbstractTextureNode): boolean; virtual; abstract;
  public
    { ANode must be TAbstractTextureNode }
    constructor Create(const ANode: TX3DNode); override;

    { Reference to handled texture node.
      Never @nil.
      It's guaranteed to satisfy IsClassForTextureNode method of this class. }
    function TextureNode: TAbstractTextureNode;

    { Find suitable TTextureResource class that can best handle given Node.
      Returns @nil if not found.

      @italic(Descedants implementors): override IsClassForTextureNode
      to be correctly recognized by this. }
    class function ClassForTextureNode(
      ANode: TAbstractTextureNode): TTextureResourceClass;
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
      const Renderer: TRenderer;
      const TextureUnitsCount: Cardinal;
      var TextureSlotsUsed: Cardinal;
      const Shader: TShader;
      const HasPreviousMainTexture: Boolean); virtual; abstract;

    { How to query this texture type from shaders.
      @false if this texture type is not supported by simple query in shaders. }
    function ShaderTextureType(out TextureType: TTextureType): Boolean; virtual;
    property RHISampler: TRHISampler read FRHISampler;
  end;

  { Manage TTextureResource connected to X3D nodes.

    Non-instantiable class (has only class methods).

    This should be the only code accessing
    TAbstractTextureNode.InternalRendererResource field,
    everything else should go through this class. }
  TTextureResources = class abstract
  public
    { Renderer resource for given ANode.
      Allows ANode = @nil.
      Returns @nil if not found or ANode is @nil. }
    class function Get(const ANode: TAbstractTextureNode): TTextureResource;

    { Prepare renderer resources for given texture node,
      uploading texture to OpenGL if necessary.
      Accepts all texture nodes, multi texture or not-multi texture nodes.
      Accepts also (and ignores) @nil as ANode. }
    class procedure Prepare(
      const RenderOptions: TCastleRenderOptions;
      const ANode: TAbstractTextureNode);

    { Release OpenGL resources for given texture node.
      Consistently with @link(Prepare), accepts and ignores @nil as ANode. }
    class procedure Unprepare(const ANode: TAbstractTextureNode);

    { Bind texture node. Calls TTextureResource.Bind method.
      If no texture renderer is prepared for this node, returns @false. }
    class function Bind(ANode: TAbstractTextureNode;
      const TextureUnit: Cardinal): boolean;

    { Enable and bind texture node. Calls TTextureResource.Enable method.
      If no texture renderer is prepared for this node, returns @false. }
    class function Enable(ANode: TAbstractTextureNode;
      const TextureUnit: Cardinal;
      Shader: TShader; const Env: TTextureEnv): boolean;
  end;

  { Common class for all single (not multi-texture) texture nodes.

    Implements EnableAll method, by calling @link(Enable) call.
    Override only @link(Enable) in descendants. }
  TSingleTextureResource = class(TTextureResource)
  public
    procedure EnableAll(const Renderer: TRenderer;
      const TextureUnitsCount: Cardinal;
      var TextureSlotsUsed: Cardinal;
      const Shader: TShader;
      const HasPreviousMainTexture: Boolean); override;
  end;

  { Renderer resource for TMultiTextureNode.

    In EnableAll, this uses other TTextureResource (of children)
    to enable them too. You don't need to worry about it from the outside,
    just use EnableAll. }
  TMultiTextureResource = class(TTextureResource)
  protected
    class function IsClassForTextureNode(ANode: TAbstractTextureNode): boolean; override;
    procedure PrepareCore(
      const RenderOptions: TCastleRenderOptions); override;
    procedure UnprepareCore; override;
  public
    { ANode must be TMultiTextureNode }
    constructor Create(const ANode: TX3DNode); override;

    function TextureNode: TMultiTextureNode;

    function Bind(const TextureUnit: Cardinal): boolean; override;
    function Enable(const TextureUnit: Cardinal;
      Shader: TShader; const Env: TTextureEnv): boolean; override;
    procedure EnableAll(const Renderer: TRenderer;
      const TextureUnitsCount: Cardinal;
      var TextureSlotsUsed: Cardinal;
      const Shader: TShader;
      const HasPreviousMainTexture: Boolean); override;
  end;

  { Renderer resource for a 2D texture. }
  T2DTextureResource = class(TSingleTextureResource)
  public
    { OpenGL texture identifier.
      It may be 0 if the texture was initialized successfully. }
    function GLName: TGLTextureId; virtual; abstract;
  end;

  { Renderer resource for 2D texture with static image (not a video). }
  TImageTextureResource = class(T2DTextureResource)
  strict private
    FGLName: TGLTexture;
    FAllocatedWidth, FAllocatedHeight: Cardinal;
    FRHITexture: TRHITexture;
    FLastFloatBytes: array of Byte;
    function GetRHIJointFloatData: Pointer;
    function GetRHIJointFloatSize: PtrUInt;
  protected
    class function IsClassForTextureNode(ANode: TAbstractTextureNode): boolean; override;
    procedure PrepareCore(
      const RenderOptions: TCastleRenderOptions); override;
    procedure UnprepareCore; override;
  public
    NormalMap, HeightMap: TGLTexture;
    HeightMapScale: Single;

    const
      MyTextureType = tt2D;

    function TextureNode: TAbstractTexture2DNode;

    function Bind(const TextureUnit: Cardinal): boolean; override;
    function Enable(const TextureUnit: Cardinal;
      Shader: TShader; const Env: TTextureEnv): boolean; override;
    function ShaderTextureType(out TextureType: TTextureType): Boolean; override;
    function GLName: TGLTextureId; override;

    procedure UpdateTextureContentsRgbaFloat(
      const Width, Height: Cardinal; const Data: Pointer); override;

    property AllocatedWidth: Cardinal read FAllocatedWidth;
    property AllocatedHeight: Cardinal read FAllocatedHeight;
    property RHITexture: TRHITexture read FRHITexture;
    property RHIJointFloatData: Pointer read GetRHIJointFloatData;
    property RHIJointFloatSize: PtrUInt read GetRHIJointFloatSize;
  end;

  { Renderer resource for TMovieTextureNode. }
  TMovieTextureResource = class(T2DTextureResource)
  strict private
    FRHITextureFrames: array of TRHITexture;
    FFramesPerSecond: Single;
    FTimeLoop, FTimeBackwards: Boolean;
  protected
    class function IsClassForTextureNode(ANode: TAbstractTextureNode): boolean; override;
    procedure PrepareCore(
      const RenderOptions: TCastleRenderOptions); override;
    procedure UnprepareCore; override;
  public
    GLVideo: TGLVideo3D;

    const
      MyTextureType = tt2D;

    function TextureNode: TMovieTextureNode;

    function Bind(const TextureUnit: Cardinal): boolean; override;
    function Enable(const TextureUnit: Cardinal;
      Shader: TShader; const Env: TTextureEnv): boolean; override;
    function ShaderTextureType(out TextureType: TTextureType): Boolean; override;
    function GLName: TGLTextureId; override;
    function RHITexture: TRHITexture;
    function RHIFrameCount: Cardinal;
  end;

  { Renderer resource for TRenderedTextureNode. }
  TRenderedTextureResource = class(T2DTextureResource)
  strict private
    FGLName: TGLTexture;
    RenderToTexture: TGLRenderToTexture;
    FRHITexture: TRHITexture;
    FRHIDepthTexture: TRHITexture;
    FRHIUpdateCount: Cardinal;
  protected
    class function IsClassForTextureNode(ANode: TAbstractTextureNode): boolean; override;
    procedure PrepareCore(
      const RenderOptions: TCastleRenderOptions); override;
    procedure UnprepareCore; override;
  public
    { The actual decided image size and mipmap status. }
    Width, Height: Cardinal;
    NeedsMipmaps: boolean;

    const
      MyTextureType = tt2D;

    function TextureNode: TRenderedTextureNode;

    function Bind(const TextureUnit: Cardinal): boolean; override;
    function Enable(const TextureUnit: Cardinal;
      Shader: TShader; const Env: TTextureEnv): boolean; override;
    function ShaderTextureType(out TextureType: TTextureType): Boolean; override;
    function GLName: TGLTextureId; override;

    procedure Update(
      const Render: TRenderFromViewFunction;
      const ProjectionNear, ProjectionFar: Single;
      const CurrentViewpoint: TAbstractViewpointNode;
      const CameraViewKnown: boolean;
      const CameraView: TViewVectors;
      const ShapeForViewpointMirror: TX3DRendererShape);

    property RHITexture: TRHITexture read FRHITexture;
    property RHIDepthTexture: TRHITexture read FRHIDepthTexture;
    property RHIRenderToTexture: TGLRenderToTexture read RenderToTexture;
    property RHIUpdateCount: Cardinal read FRHIUpdateCount;
  end;

  { Abstract renderer resource for TAbstractEnvironmentTextureNode. }
  TCubeMapTextureResource = class(TSingleTextureResource)
  protected
    FRHITexture: TRHITexture;
    { Releases GLName by TextureCubeMap_DecReference.
      Suitable for descendants tht initialize GLName by
      TextureCubeMap_IncReference. }
    procedure UnprepareCore; override;
  public
    GLName: TGLTexture;

    const
      MyTextureType = ttCubeMap;

    function TextureNode: TAbstractEnvironmentTextureNode;

    function Bind(const TextureUnit: Cardinal): boolean; override;
    function Enable(const TextureUnit: Cardinal;
      Shader: TShader; const Env: TTextureEnv): boolean; override;
    function ShaderTextureType(out TextureType: TTextureType): Boolean; override;

    property RHITexture: TRHITexture read FRHITexture;
  end;

  { Renderer resource for TComposedCubeMapTextureNode. }
  TComposedCubeMapTextureResource = class(TCubeMapTextureResource)
  protected
    class function IsClassForTextureNode(ANode: TAbstractTextureNode): boolean; override;
    procedure PrepareCore(
      const RenderOptions: TCastleRenderOptions); override;
  public
    function TextureNode: TComposedCubeMapTextureNode;
  end;

  { Renderer resource for TImageCubeMapTextureNode. }
  TImageCubeMapTextureResource = class(TCubeMapTextureResource)
  protected
    class function IsClassForTextureNode(ANode: TAbstractTextureNode): boolean; override;
    procedure PrepareCore(
      const RenderOptions: TCastleRenderOptions); override;
  public
    function TextureNode: TImageCubeMapTextureNode;
  end;

  { Renderer resource for TGeneratedCubeMapTextureNode. }
  TGeneratedCubeMapTextureResource = class(TCubeMapTextureResource)
  private
    RenderToTexture: TGLRenderToTexture;
    FRHIDepthTexture: TRHITexture;
    FRHIFacesRenderedCount: Cardinal;
    FRHIUpdateCount: Cardinal;
  protected
    class function IsClassForTextureNode(ANode: TAbstractTextureNode): boolean; override;
    procedure PrepareCore(
      const RenderOptions: TCastleRenderOptions); override;
    procedure UnprepareCore; override;
  public
    { The right size of the texture,
      that satisfies all OpenGL cube map sizes requirements
      (IsCubeMapTextureSized). }
    Size: Cardinal;

    { Does Filter need mipmaps. }
    NeedsMipmaps: boolean;

    function TextureNode: TGeneratedCubeMapTextureNode;

    procedure Update(
      const Render: TRenderFromViewFunction;
      const ProjectionNear, ProjectionFar: Single;
      const CubeMiddle: TVector3);

    property RHIDepthTexture: TRHITexture read FRHIDepthTexture;
    property RHIRenderToTexture: TGLRenderToTexture read RenderToTexture;
    property RHIFacesRenderedCount: Cardinal read FRHIFacesRenderedCount;
    property RHIUpdateCount: Cardinal read FRHIUpdateCount;
  end;

  { Renderer resource for TAbstractTexture3DNode. }
  T3DTextureResource = class(TSingleTextureResource)
  strict private
    FRHITexture: TRHITexture;
  protected
    class function IsClassForTextureNode(ANode: TAbstractTextureNode): boolean; override;
    procedure PrepareCore(
      const RenderOptions: TCastleRenderOptions); override;
    procedure UnprepareCore; override;
  public
    GLName: TGLTexture;

    const
      MyTextureType = tt3D;

    function TextureNode: TAbstractTexture3DNode;

    function Bind(const TextureUnit: Cardinal): boolean; override;
    function Enable(const TextureUnit: Cardinal;
      Shader: TShader; const Env: TTextureEnv): boolean; override;
    function ShaderTextureType(out TextureType: TTextureType): Boolean; override;

    property RHITexture: TRHITexture read FRHITexture;
  end;

  { Renderer resource for TGeneratedShadowMapNode. }
  TGeneratedShadowMapResource = class(T2DTextureResource)
  strict private
    RenderToTexture: TGLRenderToTexture;
    { VarianceShadowMaps calculated at the PrepareCore time. }
    VarianceShadowMaps: boolean;
    NeedsMipmaps: boolean;
    FGLName: TGLTexture;
    WarninigDoneCannotEnable: Boolean;
    FRHITexture: TRHITexture;
    FRHIDepthTexture: TRHITexture;
    FRHIUpdateCount: Cardinal;
    function GetRHIDepthTexture: TRHITexture;
  protected
    class function IsClassForTextureNode(ANode: TAbstractTextureNode): boolean; override;
    procedure PrepareCore(
      const RenderOptions: TCastleRenderOptions); override;
    procedure UnprepareCore; override;
  public
    { The right size of the texture,
      that satisfies all OpenGL sizes requirements. }
    Size: Cardinal;

    function TextureNode: TGeneratedShadowMapNode;

    function Bind(const TextureUnit: Cardinal): boolean; override;
    function Enable(const TextureUnit: Cardinal;
      Shader: TShader; const Env: TTextureEnv): boolean; override;
    function GLName: TGLTextureId; override;

    procedure Update(
      const Render: TRenderFromViewFunction;
      const ProjectionNear, ProjectionFar: Single;
      const Light: TAbstractPunctualLightNode);

    { Check would we use Variance Shadow Maps with current ARenderer
      attributes and OpenGL version/extensions. }
    class function ClassVarianceShadowMaps(RenderOptions: TCastleRenderOptions): boolean;

    property RHITexture: TRHITexture read FRHITexture;
    property RHIDepthTexture: TRHITexture read GetRHIDepthTexture;
    property RHISampler: TRHISampler read FRHISampler;
    property RHIRenderToTexture: TGLRenderToTexture read RenderToTexture;
    property RHIUpdateCount: Cardinal read FRHIUpdateCount;
  end;

  { Renderer resource for TShaderTextureNode. }
  TShaderTextureResource = class(TSingleTextureResource)
  protected
    class function IsClassForTextureNode(ANode: TAbstractTextureNode): boolean; override;
    procedure PrepareCore(
      const RenderOptions: TCastleRenderOptions); override;
    procedure UnprepareCore; override;
  public
    const
      MyTextureType = ttShader;

    function TextureNode: TShaderTextureNode;
    function Bind(const TextureUnit: Cardinal): boolean; override;
    function Enable(const TextureUnit: Cardinal;
      Shader: TShader; const Env: TTextureEnv): boolean; override;
    function ShaderTextureType(out TextureType: TTextureType): Boolean; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{$ifndef OpenGLES}
const
  CombineGL: array [TCombine] of TGLint = (
    GL_MODULATE, GL_REPLACE, GL_ADD_SIGNED_EXT, GL_ADD, GL_SUBTRACT,
    GL_INTERPOLATE_EXT, GL_DOT3_RGB_ARB, GL_DOT3_RGBA_ARB
  );
{$endif}

function TextureRepeatToGL(const IsRepeat: boolean): TGLenum;
begin
  if IsRepeat then
    Result := GL_REPEAT
  else
  if GLFeatures <> nil then
    Result := GLFeatures.CLAMP_TO_EDGE
  else
    Result := GL_CLAMP_TO_EDGE;
end;

{ TTextureResource ------------------------------------------------------------- }

constructor TTextureResource.Create(const ANode: TX3DNode);
begin
  Assert(ANode is TAbstractTextureNode, 'TTextureResource.Create acceps as Node only TAbstractTextureNode');
  inherited;
end;

procedure TTextureResource.HandleTextureProperties(
  const TextureProperties: TTexturePropertiesNode;
  const RenderOptions: TCastleRenderOptions;
  const RepeatS, RepeatT, RepeatR: TGLenum;
  out Filter: TTextureFilter;
  out BoundaryModeS, BoundaryModeT, BoundaryModeR: TGLenum;
  out Anisotropy: TGLfloat;
  out GUITexture: boolean);

  function BoundaryModeGL(const Value: TBoundaryMode): TGLenum;
  const
    Map: array [TBoundaryMode] of TGLenum = (
      GL_CLAMP_TO_EDGE, // bmClamp: Undefined in X3D, we prefer to make it equal to bmClampToEdge as we don't support border.
      GL_CLAMP_TO_EDGE,
      GL_CLAMP_TO_EDGE, //< bmClampToBoundaryUnsupported: Border not supported, hence behave like bmClampToEdge.
      GL_MIRRORED_REPEAT,
      GL_REPEAT
    );
  begin
    Result := Map[Value];

    { Use GLFeatures.CLAMP_TO_EDGE instead of GL_CLAMP_TO_EDGE, to support even
      ancient OpenGL versions that don't support GL_CLAMP_TO_EDGE. }
    if (Result = GL_CLAMP_TO_EDGE) and (GLFeatures <> nil) then
      Result := GLFeatures.CLAMP_TO_EDGE;
  end;

  { Convert TAutoMinificationFilter to TMinificationFilter,
    resolving the "default"/"fastest"/"nicest" aliases to specific values. }
  function FinalMinificationFilter(const FilterAuto: TAutoMinificationFilter): TMinificationFilter;
  begin
    case FilterAuto of
      minDefault:
        if RenderOptions = nil then
          Result := TCastleRenderOptions.DefaultMinificationFilter
        else
        case RenderOptions.MinificationFilter of
          minDefault: Result := TCastleRenderOptions.DefaultMinificationFilter;
          minFastest: Result := minNearest;
          minNicest : Result := minLinearMipmapLinear;
          else        Result := RenderOptions.MinificationFilter;
        end;
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
      magDefault:
        if RenderOptions = nil then
          Result := TCastleRenderOptions.DefaultMagnificationFilter
        else
        case RenderOptions.MagnificationFilter of
          magDefault: Result := TCastleRenderOptions.DefaultMagnificationFilter;
          magFastest: Result := magNearest;
          magNicest : Result := magLinear;
          else        Result := RenderOptions.MagnificationFilter;
        end;
      magFastest: Result := magNearest;
      magNicest : Result := magLinear;
      else        Result := FilterAuto;
    end;
  end;

  function TextureFilter(const RenderOptions: TCastleRenderOptions): TTextureFilter;
  begin
    if RenderOptions = nil then
    begin
      Result.Minification := TCastleRenderOptions.DefaultMinificationFilter;
      Result.Magnification := TCastleRenderOptions.DefaultMagnificationFilter;
      Exit;
    end;

    case RenderOptions.MinificationFilter of
      minDefault: Result.Minification := TCastleRenderOptions.DefaultMinificationFilter;
      minFastest: Result.Minification := minNearest;
      minNicest : Result.Minification := minLinearMipmapLinear;
      else        Result.Minification := RenderOptions.MinificationFilter;
    end;

    case RenderOptions.MagnificationFilter of
      magDefault: Result.Magnification := TCastleRenderOptions.DefaultMagnificationFilter;
      magFastest: Result.Magnification := magNearest;
      magNicest : Result.Magnification := magLinear;
      else        Result.Magnification := RenderOptions.MagnificationFilter;
    end;
  end;

begin
  if TextureProperties <> nil then
  begin
    Filter.Minification := FinalMinificationFilter(TextureProperties.MinificationFilter);
    Filter.Magnification := FinalMagnificationFilter(TextureProperties.MagnificationFilter);
    if TextureProperties.GUITexture then
      Filter.DisableMipmaps;
    Anisotropy := TextureProperties.AnisotropicDegree;
    GUITexture := TextureProperties.GUITexture;

    if (TextureProperties.BoundaryModeS = bmClampToBoundaryUnsupported) or
       (TextureProperties.BoundaryModeT = bmClampToBoundaryUnsupported) or
       (TextureProperties.BoundaryModeR = bmClampToBoundaryUnsupported) then
      WritelnWarning('VRML/X3D', 'Boundary mode CLAMP_TO_BOUNDARY is not supported. Behaves like CLAMP_TO_EDGE.');

    BoundaryModeS := BoundaryModeGL(TextureProperties.BoundaryModeS);
    BoundaryModeT := BoundaryModeGL(TextureProperties.BoundaryModeT);
    BoundaryModeR := BoundaryModeGL(TextureProperties.BoundaryModeR);
  end else
  begin
    Filter := TextureFilter(RenderOptions);
    Anisotropy := 1;
    GUITexture := false;

    { Use repeatS/T/R
      as no texture property node is provided. [see 18.2.3] }
    BoundaryModeS := RepeatS;
    BoundaryModeT := RepeatT;
    BoundaryModeR := RepeatR;
  end;
end;

procedure TTextureResource.HandleTextureProperties(
  const TextureProperties: TTexturePropertiesNode;
  const RenderOptions: TCastleRenderOptions;
  const RepeatS, RepeatT: TGLenum;
  out Filter: TTextureFilter;
  out BoundaryModeS, BoundaryModeT: TGLenum;
  out Anisotropy: TGLfloat;
  out GUITexture: boolean);
var
  Dummy: TGLenum;
const
  DummyRepeatMode = GL_REPEAT;
begin
  HandleTextureProperties(TextureProperties, RenderOptions,
    RepeatS, RepeatT, DummyRepeatMode,
    Filter, BoundaryModeS, BoundaryModeT, Dummy, Anisotropy, GUITexture);
end;

procedure TTextureResource.HandleTextureProperties(
  const TextureProperties: TTexturePropertiesNode;
  const RenderOptions: TCastleRenderOptions;
  out Filter: TTextureFilter;
  out Anisotropy: TGLfloat;
  out GUITexture: boolean);
var
  Dummy: array[0..2] of TGLenum;
const
  DummyRepeatMode = GL_REPEAT;
begin
  HandleTextureProperties(
    TextureProperties, RenderOptions,
    DummyRepeatMode, DummyRepeatMode, DummyRepeatMode,
    Filter, Dummy[0], Dummy[1], Dummy[2], Anisotropy, GUITexture);
end;

class function TTextureResource.ClassForTextureNode(
  ANode: TAbstractTextureNode): TTextureResourceClass;

  function TryResult(C: TTextureResourceClass): boolean;
  begin
    Result := C.IsClassForTextureNode(ANode);
    if Result then
      ClassForTextureNode := C;
  end;

begin
  { TODO: in the future, some way of registering class for this will
    be done. For now, just try known final TTextureResource descendants. }
  if not (TryResult(TMultiTextureResource) or
          TryResult(TImageTextureResource) or
          TryResult(TMovieTextureResource) or
          TryResult(TRenderedTextureResource) or
          TryResult(TComposedCubeMapTextureResource) or
          TryResult(TImageCubeMapTextureResource) or
          TryResult(TGeneratedCubeMapTextureResource) or
          TryResult(T3DTextureResource) or
          TryResult(TGeneratedShadowMapResource) or
          TryResult(TShaderTextureResource) ) then
    Result := nil;
end;

function TTextureResource.TextureNode: TAbstractTextureNode;
begin
  Result := TAbstractTextureNode(inherited Node);
end;

function TTextureResource.ShaderTextureType(out TextureType: TTextureType): Boolean;
begin
  Result := false;
end;

procedure TTextureResource.CreateRHISampler(
  const AFilter: TTextureFilter;
  const ABoundaryS, ABoundaryT, ABoundaryR: TGLenum;
  const AAnisotropy: Single;
  const AMipLevels: Cardinal);

  function BoundaryGLToRHI(const ABoundary: TGLenum): TRHISamplerAddressMode;
  begin
    if ABoundary = GL_REPEAT then
      Result := amRepeat
    else if ABoundary = GL_MIRRORED_REPEAT then
      Result := amMirroredRepeat
    else
      Result := amClampToEdge;
  end;

var
  SamplerDesc: TRHISamplerDesc;
begin
  FreeAndNil(FRHISampler);
  if RHIDevice = nil then
    Exit;
  FillChar(SamplerDesc, SizeOf(SamplerDesc), 0);
  case AFilter.Minification of
    minNearest, minNearestMipmapNearest, minNearestMipmapLinear:
      SamplerDesc.MinFilter := sfNearest;
  else
    SamplerDesc.MinFilter := sfLinear;
  end;
  case AFilter.Magnification of
    magNearest: SamplerDesc.MagFilter := sfNearest;
  else
    SamplerDesc.MagFilter := sfLinear;
  end;
  case AFilter.Minification of
    minNearestMipmapLinear, minLinearMipmapLinear:
      SamplerDesc.MipFilter := sfLinear;
  else
    SamplerDesc.MipFilter := sfNearest;
  end;
  SamplerDesc.AddressModeU := BoundaryGLToRHI(ABoundaryS);
  SamplerDesc.AddressModeV := BoundaryGLToRHI(ABoundaryT);
  SamplerDesc.AddressModeW := BoundaryGLToRHI(ABoundaryR);
  SamplerDesc.MaxAnisotropy := Max(1.0, AAnisotropy);
  SamplerDesc.CompareEnable := False;
  SamplerDesc.CompareOp := coAlways;
  SamplerDesc.MinLod := 0.0;
  if AMipLevels > 1 then
    SamplerDesc.MaxLod := Single(AMipLevels - 1)
  else
    SamplerDesc.MaxLod := 0.0;
  FRHISampler := RHIDevice.CreateSampler(SamplerDesc);
end;

{ TTextureResources ------------------------------------------------------------ }

class function TTextureResources.Get(const ANode: TAbstractTextureNode): TTextureResource;
begin
  if ANode <> nil then
  begin
    Result := TTextureResource(ANode.InternalRendererResource);
    if (Result <> nil) and (Result.FPreparedForRHI <> (RHIDevice <> nil)) then
      Result := nil;
  end else
    Result := nil;
end;

class procedure TTextureResources.Prepare(
  const RenderOptions: TCastleRenderOptions;
  const ANode: TAbstractTextureNode);
var
  TextureRes: TTextureResource;
  TextureResClass: TTextureResourceClass;
begin
  if ANode = nil then
    Exit;
  if (ANode.InternalRendererResource <> nil) and
     (TTextureResource(ANode.InternalRendererResource).FPreparedForRHI <> (RHIDevice <> nil)) then
    ANode.InternalRendererResourceFree;
  if ANode.InternalRendererResource = nil then
  begin
    TextureResClass := TTextureResourceClass.ClassForTextureNode(ANode);
    if TextureResClass <> nil { Ignore if not handled node. } then
    begin
      TextureRes := TextureResClass.Create(ANode);
      TextureRes.FPreparedForRHI := RHIDevice <> nil;
      TextureRes.Prepare(RenderOptions);
      ANode.InternalRendererResource := TextureRes;
    end;
  end;
end;

class procedure TTextureResources.Unprepare(const ANode: TAbstractTextureNode);
begin
  if ANode <> nil then
    ANode.InternalRendererResourceFree;
end;

class function TTextureResources.Bind(ANode: TAbstractTextureNode;
  const TextureUnit: Cardinal): boolean;
var
  TextureRes: TTextureResource;
begin
  TextureRes := Get(ANode);
  Result := TextureRes <> nil;
  if Result then
    Result := TextureRes.Bind(TextureUnit);
end;

class function TTextureResources.Enable(ANode: TAbstractTextureNode;
  const TextureUnit: Cardinal; Shader: TShader; const Env: TTextureEnv): boolean;
var
  TextureRes: TTextureResource;
begin
  TextureRes := Get(ANode);
  Result := TextureRes <> nil;
  if Result then
    Result := TextureRes.Enable(TextureUnit, Shader, Env);
end;

{ TSingleTextureResource ------------------------------------------------------- }

procedure TSingleTextureResource.EnableAll(const Renderer: TRenderer;
  const TextureUnitsCount: Cardinal;
  var TextureSlotsUsed: Cardinal;
  const Shader: TShader;
  const HasPreviousMainTexture: Boolean);
var
  Env: TTextureEnv;
begin
  { coModulate is default texture mode for single texturing (not MultiTexture).

    Note: VRML 2 and X3D < 4.0 specifications say that the texture color should
    replace material color (see "Lighting model" spec) *in case of RGB
    (not grayscale) textures*. We contradict that specs, instead modulating
    with material color because of GL_MODULATE below. See
    https://castle-engine.io/x3d_multi_texturing.php#section_default_texture_mode ,
    https://github.com/michaliskambi/x3d-tests/wiki/Make-RGB-and-grayscale-textures-treatment-consistent .

    What we do follows X3D 4.0 spec.
    As explained above, we actually changed the spec, because "always doing
    coModulate" is better.

    For alpha channel: specification says clearly that texture alpha
    should replace (never modulate) alpha channel, if only texture
    has any alpha channel. Again, we contradict it, instead multiplying alpha.

    Note that using GL_REPLACE below would not really make us conforming
    to the spec: it would mean that texture color replaces the whole resulting
    lighting calculation (not just material color, which is input to
    lighting calculation). So it would not be conforming either. }

  Env.Init(coModulate);

  { HasPreviousMainTexture is passed when enabling Appearance.texture
    on Text node (geometry with FontTextureNode set),
    IOW when we have textured Text node.

    In this case, we already called EnableAll with FontTextureNode,
    and it has determined the initial color (and alpha) of the text.
    So our Source should indicate to mix with previous texture (FontTextureNode),
    and not with the material color (as is default, and would override
    FontTextureNode alpha).

    Testcases:
      demo-models/shadow_maps/primitives.x3dv
      demo-models/text/text_textured.x3dv
    Without this "if HasPreviousMainTexture then" check,
    the textured text displays every letter as a rectangle, as alpha
    of FontTextureNode is ignored. }
  if HasPreviousMainTexture then
  begin
    Env.Source[cRGB  ] := csPreviousTexture;
    Env.Source[cAlpha] := csPreviousTexture;
  end;

  if (TextureUnitsCount > 0) and
     Enable(TextureSlotsUsed, Shader, Env) then
  begin
    if (RHIDevice = nil) and (GLFeatures <> nil) and GLFeatures.EnableFixedFunction then
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

{ TMultiTextureResource -------------------------------------------------------- }

constructor TMultiTextureResource.Create(const ANode: TX3DNode);
begin
  Assert(ANode is TMultiTextureNode, 'TMultiTextureResource.Create acceps as Node only TMultiTextureNode');
  inherited;
end;

class function TMultiTextureResource.IsClassForTextureNode(
  ANode: TAbstractTextureNode): boolean;
begin
  Result := ANode is TMultiTextureNode;
end;

function TMultiTextureResource.TextureNode: TMultiTextureNode;
begin
  Result := TMultiTextureNode(inherited TextureNode);
end;

procedure TMultiTextureResource.PrepareCore(
  const RenderOptions: TCastleRenderOptions);
var
  ChildTex: TX3DNode;
  I: Integer;
begin
  for I := 0 to TextureNode.FdTexture.Count - 1 do
  begin
    ChildTex := TextureNode.FdTexture[I];
    if ChildTex is TAbstractTextureNode then // also checks ChildTex <> nil
    begin
      if ChildTex is TMultiTextureNode then
        WritelnWarning('VRML/X3D', 'Child of MultiTexture node cannot be another MultiTexture node') else
      begin
        TTextureResources.Prepare(RenderOptions, TAbstractTextureNode(ChildTex));
      end;
    end;
  end;
end;

procedure TMultiTextureResource.UnprepareCore;
var
  ChildTex: TX3DNode;
  I: Integer;
begin
  for I := 0 to TextureNode.FdTexture.Count - 1 do
  begin
    ChildTex := TextureNode.FdTexture[I];
    if (ChildTex is TAbstractTextureNode) and // also checks ChildTex <> nil
       (not (ChildTex is TMultiTextureNode)) then
      TTextureResources.Unprepare(TAbstractTextureNode(ChildTex));
  end;
end;

function TMultiTextureResource.Bind(const TextureUnit: Cardinal): boolean;
begin
  { TMultiTextureResource cannot set only one texture unit.
    This may be called from GLSL shader, when someone will use MultiTexture
    node for a shader uniform field. I don't know how this should be handled,
    I guess returning failure is Ok for now. }
  Result := false;
end;

function TMultiTextureResource.Enable(const TextureUnit: Cardinal;
  Shader: TShader; const Env: TTextureEnv): boolean;
begin
  { This should never be called. TMultiTextureResource cannot set only one
    texture unit. }
  Result := false;
end;

procedure TMultiTextureResource.EnableAll(const Renderer: TRenderer;
  const TextureUnitsCount: Cardinal;
  var TextureSlotsUsed: Cardinal;
  const Shader: TShader;
  const HasPreviousMainTexture: Boolean);
{$ifndef OpenGLES}
const
  ColorSourceGL: array [TColorSource] of TGLint = (
    GL_PRIMARY_COLOR, GL_TEXTURE, GL_CONSTANT, GL_PREVIOUS
  );
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
  if (RHIDevice = nil) and (GLFeatures <> nil) and (not GLFeatures.UseMultiTexturing) then
    MinVar(TextureSlotsUsed, 1);

  { TODO: Passing it to shader this way precludes ability to later change it
    (e.g. in case some ROUTE animates MultiTexture.color/alpha, or Pascal code changes it)
    without recompiling the shader.

    Right now changes to MultiTexture.color/alpha will recompile the shader.

    To make it efficient and corrent, this should be a uniform passed to shader.
    This is low-priority due to low MultiTexture usage. }
  Shader.MultiTextureColor := Vector4(TextureNode.Color, TextureNode.Alpha);

  for I := 0 to Integer(TextureSlotsUsed) - Integer(InitialTextureSlotsUsed) - 1 do
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
        WritelnWarning('VRML/X3D', 'Child of MultiTexture node cannot be another MultiTexture node')
      else
        Success := TTextureResources.Enable(
          TAbstractTextureNode(ChildTex),
          InitialTextureSlotsUsed + I, Shader, Env);

      { Apply Env for fixed-function pipeline. }
      if Success and (RHIDevice = nil) and (GLFeatures <> nil) and GLFeatures.UseMultiTexturing { needed OpenGL exts available } then
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

{ TImageTextureResource -------------------------------------------------------- }

class function TImageTextureResource.IsClassForTextureNode(
  ANode: TAbstractTextureNode): boolean;
begin
  Result := (ANode is TAbstractTexture2DNode) and
    TAbstractTexture2DNode(ANode).IsTextureImage;
end;

function TImageTextureResource.TextureNode: TAbstractTexture2DNode;
begin
  Result := TAbstractTexture2DNode(inherited TextureNode);
end;

procedure TImageTextureResource.PrepareCore(
  const RenderOptions: TCastleRenderOptions);
var
  Filter: TTextureFilter;
  Anisotropy: TGLfloat;
  BoundaryModeS: TGLenum;
  BoundaryModeT: TGLenum;
  TextureWrap: TTextureWrap2D;
  GUITexture, FlipVertically: boolean;
  MipLevels, MipIdx: Cardinal;
  TexFormat: TRHIPixelFormat;
  TexDesc: TRHITextureDesc;
  TempFlipped: TCastleImage;
  TempFloat: TRGBAlphaFloatImage;
  CacheFullUrl: String;
begin
  { HandledNode already made sure IsTextureImage = @true }

  HandleTextureProperties(TextureNode.TextureProperties, RenderOptions,
    TextureRepeatToGL(TextureNode.RepeatS), TextureRepeatToGL(TextureNode.RepeatT),
    Filter, BoundaryModeS, BoundaryModeT, Anisotropy, GUITexture);

  if RHIDevice <> nil then
  begin
    if TextureNode.TextureImage <> nil then
    begin
      if FRHITexture <> nil then
        RendererCache.RHITextureImage_DecReference(FRHITexture);
      FlipVertically :=
        (TextureNode is TImageTextureNode) and
        TImageTextureNode(TextureNode).FlipVertically;
      CacheFullUrl := Iff(TextureNode.TextureUsedFullUrl <> '',
        TextureNode.TextureUsedFullUrl, TextureNode.TextureImage.URL);
      TexFormat := EncodedImageToRHIPixelFormat(TextureNode.TextureImage);
      if (CacheFullUrl <> '') and (TexFormat <> pfRGBA32_Float) then
      begin
        FRHITexture := RendererCache.FindRHITextureImage(
          CacheFullUrl, FlipVertically, Filter, GUITexture, GLTextureScale,
          FAllocatedWidth, FAllocatedHeight);
        if FRHITexture <> nil then
        begin
          SetLength(FLastFloatBytes, 0);
          CreateRHISampler(Filter, BoundaryModeS, BoundaryModeT, GL_CLAMP_TO_EDGE,
            Anisotropy, Max(Cardinal(1), FRHITexture.Desc.MipLevels));
          Exit;
        end;
      end;
      FAllocatedWidth := Max(Cardinal(1), TextureNode.TextureImage.Width);
      FAllocatedHeight := Max(Cardinal(1), TextureNode.TextureImage.Height);
      if (not GUITexture) and (GLTextureScale > 1) and
         (TextureNode.TextureImage is TCastleImage) and
         (TexFormat <> pfRGBA32_Float) and
         ((TextureNode.TextureComposite = nil) or (not TextureNode.TextureComposite.Mipmaps)) then
      begin
        if FAllocatedWidth > GLTextureMinSize then
          FAllocatedWidth := Max(FAllocatedWidth shr (GLTextureScale - 1), GLTextureMinSize);
        if FAllocatedHeight > GLTextureMinSize then
          FAllocatedHeight := Max(FAllocatedHeight shr (GLTextureScale - 1), GLTextureMinSize);
      end;
      if Filter.NeedsMipmaps then
      begin
        if (TextureNode.TextureComposite <> nil) and TextureNode.TextureComposite.Mipmaps and
           (TextureNode.TextureComposite.Images.Count > 0) then
          MipLevels := Max(Cardinal(1), Cardinal(TextureNode.TextureComposite.Images.Count))
        else if TextureNode.TextureImage is TGPUCompressedImage then
          MipLevels := 1
        else
          MipLevels := CalculateTextureMipLevels(Max(FAllocatedWidth, FAllocatedHeight));
      end else
        MipLevels := 1;
      TexDesc := TRHITextureDesc.Create2D(FAllocatedWidth, FAllocatedHeight,
        TexFormat, [tuSampled, tuTransferDst, tuTransferSrc], MipLevels, 1);
      TexDesc.DebugName := 'ImageTexture';
      FRHITexture := RHIDevice.CreateTexture(TexDesc);
      if ((FlipVertically and (TextureNode.TextureUsedFullUrl = '')) or
          (FAllocatedWidth <> TextureNode.TextureImage.Width) or
          (FAllocatedHeight <> TextureNode.TextureImage.Height)) and
         (TextureNode.TextureImage is TCastleImage) then
      begin
        TempFlipped := TCastleImage(TextureNode.TextureImage).MakeCopy;
        try
          if FlipVertically and (TextureNode.TextureUsedFullUrl = '') then
            TempFlipped.FlipVertical;
          if (FAllocatedWidth <> TempFlipped.Width) or
             (FAllocatedHeight <> TempFlipped.Height) then
            TempFlipped.Resize(FAllocatedWidth, FAllocatedHeight, riBilinear);
          FRHITexture.UploadImage(TempFlipped, 0, 0);
        finally
          FreeAndNil(TempFlipped);
        end;
      end else
        FRHITexture.UploadImage(TextureNode.TextureImage, 0, 0);
      if (TexFormat = pfRGBA32_Float) and (TextureNode.TextureImage is TCastleImage) then
      begin
        SetLength(FLastFloatBytes, PtrUInt(FAllocatedWidth) * PtrUInt(FAllocatedHeight) * SizeOf(TVector4));
        if Length(FLastFloatBytes) > 0 then
        begin
          if (TextureNode.TextureImage is TRGBAlphaFloatImage) and
             (TextureNode.TextureImage.RawPixels <> nil) then
            Move(TextureNode.TextureImage.RawPixels^, FLastFloatBytes[0], Length(FLastFloatBytes))
          else
          begin
            TempFloat := TRGBAlphaFloatImage.Create(FAllocatedWidth, FAllocatedHeight);
            try
              TempFloat.Assign(TCastleImage(TextureNode.TextureImage));
              Move(TempFloat.RawPixels^, FLastFloatBytes[0], Length(FLastFloatBytes));
            finally
              FreeAndNil(TempFloat);
            end;
          end;
        end;
      end else
        SetLength(FLastFloatBytes, 0);
      if (MipLevels > 1) and (TextureNode.TextureComposite <> nil) and
         TextureNode.TextureComposite.Mipmaps and
         (TextureNode.TextureComposite.Images.Count > 1) then
      begin
        for MipIdx := 1 to Min(Cardinal(TextureNode.TextureComposite.Images.Count) - 1, MipLevels - 1) do
          FRHITexture.UploadImage(TextureNode.TextureComposite.Images[MipIdx], MipIdx, 0);
      end else
      if MipLevels > 1 then
        FRHITexture.GenerateMipmaps;
      TextureMemoryProfiler.AllocateRHI(FRHITexture,
        CacheFullUrl,
        TextureNode.TextureImage.ClassName,
        EncodedImageToRHIGpuBytes(TextureNode.TextureImage, FRHITexture.Desc.Format) *
          Int64(FAllocatedWidth) * Int64(FAllocatedHeight) div
          Max(Int64(1), Int64(TextureNode.TextureImage.Width) * Int64(TextureNode.TextureImage.Height)) +
        Iff(MipLevels > 1, TTextureMemoryProfiler.MipmapsSize(TextureNode.TextureImage), 0),
        MipLevels > 1, FAllocatedWidth, FAllocatedHeight, 1);
      if (CacheFullUrl <> '') and (TexFormat <> pfRGBA32_Float) then
        RendererCache.AddRHITextureImage(
          CacheFullUrl, FlipVertically, Filter, GUITexture, GLTextureScale,
          FAllocatedWidth, FAllocatedHeight, FRHITexture);
      CreateRHISampler(Filter, BoundaryModeS, BoundaryModeT, GL_CLAMP_TO_EDGE, Anisotropy, MipLevels);
    end;
    Exit;
  end;

  TextureWrap.Data[0] := BoundaryModeS;
  TextureWrap.Data[1] := BoundaryModeT;

  FlipVertically :=
    (TextureNode is TImageTextureNode) and
    TImageTextureNode(TextureNode).FlipVertically;

  FGLName := RendererCache.TextureImage_IncReference(
    TextureNode.TextureImage,
    TextureNode.TextureUsedFullUrl,
    Filter,
    Anisotropy,
    TextureWrap,
    TextureNode.TextureComposite,
    GUITexture,
    FlipVertically);
  FAllocatedWidth := TextureNode.TextureImage.Width;
  FAllocatedHeight := TextureNode.TextureImage.Height;
end;

procedure TImageTextureResource.UnprepareCore;
begin
  SetLength(FLastFloatBytes, 0);
  if FRHITexture <> nil then
    RendererCache.RHITextureImage_DecReference(FRHITexture);
  FreeAndNil(FRHISampler);
  if FGLName <> GLObjectNone then
  begin
    RendererCache.TextureImage_DecReference(FGLName);
    FGLName := GLObjectNone;
  end;
end;

function TImageTextureResource.GetRHIJointFloatData: Pointer;
begin
  if Length(FLastFloatBytes) > 0 then
    Result := @FLastFloatBytes[0]
  else
    Result := nil;
end;

function TImageTextureResource.GetRHIJointFloatSize: PtrUInt;
begin
  Result := Length(FLastFloatBytes);
end;

function TImageTextureResource.Bind(const TextureUnit: Cardinal): boolean;
begin
  if RHIDevice <> nil then
    Exit(FRHITexture <> nil);

  Result := FGLName <> GLObjectNone;
  if not Result then Exit;

  TRenderer.ActiveTexture(TextureUnit);
  glBindTexture(GL_TEXTURE_2D, FGLName);
end;

function TImageTextureResource.Enable(const TextureUnit: Cardinal;
  Shader: TShader; const Env: TTextureEnv): boolean;
begin
  Result := Bind(TextureUnit);
  if not Result then Exit;

  Shader.EnableTexture(TextureUnit, MyTextureType, TextureNode, Env);
end;

function TImageTextureResource.ShaderTextureType(out TextureType: TTextureType): Boolean;
begin
  TextureType := MyTextureType;
  Result := true;
end;

function TImageTextureResource.GLName: TGLTextureId;
begin
  Result := FGLName;
end;

procedure TImageTextureResource.UpdateTextureContentsRgbaFloat(
  const Width, Height: Cardinal; const Data: Pointer);
const
  // values below like UncompressedGLFormatAndInternalFormat for TRGBAlphaFloatImage
  ImageFormat = GL_RGBA;
  ImageInternalFormat = GL_RGBA32F;
  Level = 0; // always update mipmap level 0;
var
  GLData: {$ifdef CASTLE_WEBGL} IJSFloat32Array {$else} Pointer {$endif};
  TexDesc: TRHITextureDesc;
  TempFloatImg: TRGBAlphaFloatImage;
begin
  if RHIDevice <> nil then
  begin
    if (FRHITexture = nil) or
       (FAllocatedWidth <> Width) or
       (FAllocatedHeight <> Height) or
       (FRHITexture.Desc.Format <> pfRGBA32_Float) then
    begin
      if FRHITexture <> nil then
        RendererCache.RHITextureImage_DecReference(FRHITexture);
      FAllocatedWidth := Max(Cardinal(1), Width);
      FAllocatedHeight := Max(Cardinal(1), Height);
      TexDesc := TRHITextureDesc.Create2D(FAllocatedWidth, FAllocatedHeight,
        pfRGBA32_Float, [tuSampled, tuTransferDst, tuTransferSrc], 1, 1);
      TexDesc.DebugName := 'ImageTexture.RgbaFloat';
      FRHITexture := RHIDevice.CreateTexture(TexDesc);
      TextureMemoryProfiler.AllocateRHI(FRHITexture,
        Iff((TextureNode <> nil) and (TextureNode.TextureUsedFullUrl <> ''),
          TextureNode.TextureUsedFullUrl, 'generated:/ImageTexture.RgbaFloat'),
        'TRGBAlphaFloatImage',
        Int64(FAllocatedWidth) * Int64(FAllocatedHeight) * 16,
        False, FAllocatedWidth, FAllocatedHeight, 1);
    end;
    if (Data <> nil) and (Width > 0) and (Height > 0) and (FRHITexture <> nil) then
    begin
      SetLength(FLastFloatBytes, PtrUInt(Width) * PtrUInt(Height) * SizeOf(TVector4));
      if Length(FLastFloatBytes) > 0 then
        Move(Data^, FLastFloatBytes[0], Length(FLastFloatBytes));
      TempFloatImg := TRGBAlphaFloatImage.Create(Width, Height);
      try
        Move(Data^, TempFloatImg.RawPixels^, PtrUInt(Width) * PtrUInt(Height) * SizeOf(TVector4));
        FRHITexture.UploadImage(TempFloatImg, 0, 0);
      finally
        FreeAndNil(TempFloatImg);
      end;
    end;
    Exit;
  end;

  if not Bind(0) then Exit;

  {$ifdef CASTLE_WEBGL}
  { On web, the texture data must be copied to JS float32 array. }
  GLData := TJSFloat32Array.Create(Width * Height * 4);
  GLData.CopyFromMemory(Data, Width * Height * 4 * SizeOf(Single));
  {$else}
  GLData := Data;
  {$endif}

  { We assume we don't need to do some stuff from CastleTexImage2D:
    - no need for BeforeUnpackImage, as RGBA float textures are always
      already aligned to 4 bytes
    - no need for Mesa + fixed function hack
    - no need for texture swizzle, just map RGBA to RGBA. }

  { Using glTexSubImage2D is theoretically faster, as it doesn't have to
    change texture size on GPU?
    In practice, animate_bones_by_code_standalone with "Add Crowd"
    shows no difference between glTexImage2D and glTexSubImage2D. }
  {$define UpdateTextureContentsRgbaFloat_USE_TEXSUBIMAGE2D}

  {$ifdef UpdateTextureContentsRgbaFloat_USE_TEXSUBIMAGE2D}
  if (FAllocatedWidth <> Width) or (FAllocatedHeight <> Height) then
  {$endif}

  begin
    glTexImage2D(GL_TEXTURE_2D, Level, ImageInternalFormat,
      Width, Height, 0, ImageFormat, GL_FLOAT, GLData);
    FAllocatedWidth := Width;
    FAllocatedHeight := Height;
  end

  {$ifdef UpdateTextureContentsRgbaFloat_USE_TEXSUBIMAGE2D}
  else
  begin
    glTexSubImage2D(GL_TEXTURE_2D, Level, 0, 0, Width, Height,
      ImageFormat, GL_FLOAT, GLData);
  end
  {$endif};
end;

{ TMovieTextureResource -------------------------------------------------------- }

class function TMovieTextureResource.IsClassForTextureNode(
  ANode: TAbstractTextureNode): boolean;
begin
  { Although for most code TMovieTextureResource, it would be enought
    to have any TAbstractTexture2DNode with IsTextureVideo = @true.
    For when rendering, we'll need some TMovieTextureNode properties
    to choose video frame.

    Anyway, TMovieTextureNode is for now the only texture node possible
    that may have IsTextureVideo = @true, so it's not a real problem for now. }

  Result := (ANode is TMovieTextureNode) and
    TMovieTextureNode(ANode).IsTextureVideo;
end;

function TMovieTextureResource.TextureNode: TMovieTextureNode;
begin
  Result := TMovieTextureNode(inherited TextureNode);
end;

procedure TMovieTextureResource.PrepareCore(
  const RenderOptions: TCastleRenderOptions);
var
  Filter: TTextureFilter;
  Anisotropy: TGLfloat;
  BoundaryModeS: TGLenum;
  BoundaryModeT: TGLenum;
  TextureWrap: TTextureWrap2D;
  GUITexture: boolean;
  I: Integer;
  MipLevels, FrameW, FrameH: Cardinal;
  FrameImg, TempFlipped: TCastleImage;
  TexDesc: TRHITextureDesc;
begin
  { HandledNode already made sure IsTextureVideo = @true }

  HandleTextureProperties(TextureNode.TextureProperties, RenderOptions,
    TextureRepeatToGL(TextureNode.RepeatS), TextureRepeatToGL(TextureNode.RepeatT),
    Filter, BoundaryModeS, BoundaryModeT, Anisotropy, GUITexture);

  if RHIDevice <> nil then
  begin
    if (TextureNode.TextureVideo <> nil) and TextureNode.TextureVideo.Loaded and
       (TextureNode.TextureVideo.Count > 0) then
    begin
      FFramesPerSecond := TextureNode.TextureVideo.FramesPerSecond;
      FTimeLoop := TextureNode.TextureVideo.TimeLoop;
      FTimeBackwards := TextureNode.TextureVideo.TimeBackwards;
      MipLevels := 1;
      SetLength(FRHITextureFrames, TextureNode.TextureVideo.Count);
      for I := 0 to TextureNode.TextureVideo.Count - 1 do
      begin
        FrameImg := TextureNode.TextureVideo.Items[I];
        FrameW := Max(Cardinal(1), FrameImg.Width);
        FrameH := Max(Cardinal(1), FrameImg.Height);
        if (not GUITexture) and (GLTextureScale > 1) then
        begin
          if FrameW > GLTextureMinSize then
            FrameW := Max(FrameW shr (GLTextureScale - 1), GLTextureMinSize);
          if FrameH > GLTextureMinSize then
            FrameH := Max(FrameH shr (GLTextureScale - 1), GLTextureMinSize);
        end;
        if Filter.NeedsMipmaps then
          MipLevels := CalculateTextureMipLevels(Max(FrameW, FrameH))
        else
          MipLevels := 1;
        TexDesc := TRHITextureDesc.Create2D(
          FrameW,
          FrameH,
          pfRGBA8_UNorm, [tuSampled, tuTransferDst, tuTransferSrc], MipLevels, 1);
        TexDesc.DebugName := Format('MovieTexture.Frame[%d]', [I]);
        FRHITextureFrames[I] := RHIDevice.CreateTexture(TexDesc);
        if (TextureNode.FlipVertically and (TextureNode.TextureUsedFullUrl = '')) or
           (FrameW <> FrameImg.Width) or
           (FrameH <> FrameImg.Height) then
        begin
          TempFlipped := FrameImg.MakeCopy;
          try
            if TextureNode.FlipVertically and (TextureNode.TextureUsedFullUrl = '') then
              TempFlipped.FlipVertical;
            if (FrameW <> TempFlipped.Width) or (FrameH <> TempFlipped.Height) then
              TempFlipped.Resize(FrameW, FrameH, riBilinear);
            FRHITextureFrames[I].UploadImage(TempFlipped, 0, 0);
          finally
            FreeAndNil(TempFlipped);
          end;
        end else
          FRHITextureFrames[I].UploadImage(FrameImg, 0, 0);
        if MipLevels > 1 then
          FRHITextureFrames[I].GenerateMipmaps;
        TextureMemoryProfiler.AllocateRHI(FRHITextureFrames[I],
          Iff(TextureNode.TextureUsedFullUrl <> '', TextureNode.TextureUsedFullUrl, FrameImg.URL),
          FrameImg.ClassName,
          Int64(TexDesc.Width) * Int64(TexDesc.Height) * 4 +
          Iff(MipLevels > 1, TTextureMemoryProfiler.MipmapsSize(FrameImg), 0),
          MipLevels > 1, TexDesc.Width, TexDesc.Height, 1);
      end;
      CreateRHISampler(Filter, BoundaryModeS, BoundaryModeT, GL_CLAMP_TO_EDGE, Anisotropy, MipLevels);
    end;
    Exit;
  end;

  TextureWrap.Data[0] := BoundaryModeS;
  TextureWrap.Data[1] := BoundaryModeT;

  GLVideo := RendererCache.TextureVideo_IncReference(
    TextureNode.TextureVideo,
    TextureNode.TextureUsedFullUrl,
    TextureNode.FlipVertically,
    Filter,
    Anisotropy,
    TextureWrap,
    GUITexture);
end;

procedure TMovieTextureResource.UnprepareCore;
var
  I: Integer;
begin
  for I := 0 to High(FRHITextureFrames) do
    if FRHITextureFrames[I] <> nil then
    begin
      TextureMemoryProfiler.DeallocateRHI(FRHITextureFrames[I]);
      FreeAndNil(FRHITextureFrames[I]);
    end;
  SetLength(FRHITextureFrames, 0);
  FFramesPerSecond := 0;
  FTimeLoop := False;
  FTimeBackwards := False;
  FreeAndNil(FRHISampler);
  if GLVideo <> nil then
  begin
    RendererCache.TextureVideo_DecReference(GLVideo);
    GLVideo := nil;
  end;
end;

function TMovieTextureResource.RHITexture: TRHITexture;
var
  VideoUnscaledTime: TFloatTime;
  FrameIdx: Integer;
begin
  { Note: do not access TextureNode.TextureVideo or IsTextureVideo here ---
    doing so would reload the video from disk after
    TCastleSceneCore.FreeResources([frTextureDataInNodes]) unloaded CPU frames. }
  Result := nil;
  if Length(FRHITextureFrames) > 0 then
  begin
    if (TextureNode <> nil) and (FFramesPerSecond > 0) then
    begin
      VideoUnscaledTime :=
        TextureNode.TimeFunctionality.ElapsedTimeInCycle *
        TextureNode.Speed;
      if TextureNode.Speed < 0 then
        VideoUnscaledTime := TextureNode.Duration + VideoUnscaledTime;
      FrameIdx := EnsureRange(
        TVideo.FrameIndexFromTime(VideoUnscaledTime, Length(FRHITextureFrames),
          FFramesPerSecond, FTimeLoop, FTimeBackwards),
        0, High(FRHITextureFrames));
      Result := FRHITextureFrames[FrameIdx];
    end else
      Result := FRHITextureFrames[0];
  end;
end;

function TMovieTextureResource.RHIFrameCount: Cardinal;
begin
  Result := Cardinal(Length(FRHITextureFrames));
end;

function TMovieTextureResource.GLName: TGLTextureId;
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
      TextureNode.TimeFunctionality.ElapsedTimeInCycle *
      TextureNode.Speed;
    if TextureNode.Speed < 0 then
      VideoUnscaledTime := TextureNode.Duration + VideoUnscaledTime;
    Result := GLVideo.GLTextureFromTime(VideoUnscaledTime);
  end else
    Result := GLObjectNone;
end;

function TMovieTextureResource.Bind(const TextureUnit: Cardinal): boolean;
var
  N: TGLTextureId;
begin
  if RHIDevice <> nil then
    Exit(RHITexture <> nil);

  N := GLName;
  Result := N <> GLObjectNone;
  if Result then
  begin
    TRenderer.ActiveTexture(TextureUnit);
    glBindTexture(GL_TEXTURE_2D, N);
  end;
end;

function TMovieTextureResource.Enable(const TextureUnit: Cardinal;
  Shader: TShader; const Env: TTextureEnv): boolean;
begin
  Result := Bind(TextureUnit);
  if not Result then Exit;

  Shader.EnableTexture(TextureUnit, MyTextureType, TextureNode, Env);
end;

function TMovieTextureResource.ShaderTextureType(out TextureType: TTextureType): Boolean;
begin
  TextureType := MyTextureType;
  Result := true;
end;

{ TRenderedTextureResource ----------------------------------------------------- }

class function TRenderedTextureResource.IsClassForTextureNode(
  ANode: TAbstractTextureNode): boolean;
begin
  Result := ANode is TRenderedTextureNode;
end;

function TRenderedTextureResource.TextureNode: TRenderedTextureNode;
begin
  Result := TRenderedTextureNode(inherited TextureNode);
end;

procedure TRenderedTextureResource.PrepareCore(
  const RenderOptions: TCastleRenderOptions);
var
  InitialImage: TCastleImage;
  Filter: TTextureFilter;
  Anisotropy: TGLfloat;
  BoundaryModeS: TGLenum;
  BoundaryModeT: TGLenum;
  TextureWrap: TTextureWrap2D;
  NodeWidth, NodeHeight, MipLevels: Cardinal;
  GUITexture: boolean;
  Sizing: TTextureSizing;
  TexDesc: TRHITextureDesc;
begin
  HandleTextureProperties(TextureNode.TextureProperties, RenderOptions,
    TextureRepeatToGL(TextureNode.FdRepeatS.Value), TextureRepeatToGL(TextureNode.FdRepeatT.Value),
    Filter, BoundaryModeS, BoundaryModeT, Anisotropy, GUITexture);

  if RHIDevice <> nil then
  begin
    NeedsMipmaps := Filter.NeedsMipmaps;
    if TextureNode.FdDimensions.Items.Count - 1 >= 0 then
      NodeWidth := Max(TextureNode.FdDimensions.Items[0], 1)
    else
      NodeWidth := DefaultRenderedTextureWidth;
    if TextureNode.FdDimensions.Items.Count - 1 >= 1 then
      NodeHeight := Max(TextureNode.FdDimensions.Items[1], 1)
    else
      NodeHeight := DefaultRenderedTextureHeight;
    Width := NodeWidth;
    Height := NodeHeight;
    if (not GUITexture) and (GLTextureScale > 1) then
    begin
      if Width > GLTextureMinSize then
        Width := Max(Width shr (GLTextureScale - 1), GLTextureMinSize);
      if Height > GLTextureMinSize then
        Height := Max(Height shr (GLTextureScale - 1), GLTextureMinSize);
    end;
    if RHIDevice.Capabilities.MaxTextureSize > 0 then
    begin
      Width := Min(Width, RHIDevice.Capabilities.MaxTextureSize);
      Height := Min(Height, RHIDevice.Capabilities.MaxTextureSize);
    end;

    if (TextureNode.FdDepthMap.Count > 0) and
       TextureNode.FdDepthMap.Items[0] then
    begin
      WritelnWarning('VRML/X3D', 'RenderedTexture with depthMap = TRUE is no longer supported. For shadow maps, use dedicated CGE nodes.');
      Exit;
    end;

    if NeedsMipmaps then
      MipLevels := CalculateTextureMipLevels(Max(Width, Height))
    else
      MipLevels := 1;

    if FRHITexture <> nil then
    begin
      TextureMemoryProfiler.DeallocateRHI(FRHITexture);
      FreeAndNil(FRHITexture);
    end;
    TexDesc := TRHITextureDesc.Create2D(Width, Height, pfRGBA8_UNorm,
      [tuColorAttachment, tuSampled, tuTransferSrc, tuTransferDst], MipLevels, 1);
    TexDesc.DebugName := 'RenderedTexture.Color';
    FRHITexture := RHIDevice.CreateTexture(TexDesc);

    InitialImage := TRGBImage.Create(Width, Height);
    try
      InitialImage.URL := 'generated:/' + TextureNode.NiceName;
      InitialImage.Clear(Vector4Byte(255, 0, 255, 255));
      FRHITexture.UploadImage(InitialImage, 0, 0);
      if MipLevels > 1 then
        FRHITexture.GenerateMipmaps;
      TextureMemoryProfiler.AllocateRHI(FRHITexture,
        '', InitialImage.ClassName,
        Int64(Width) * Int64(Height) * 4 +
        Iff(MipLevels > 1, TTextureMemoryProfiler.MipmapsSize(InitialImage), 0),
        MipLevels > 1, Width, Height, 1);
    finally
      FreeAndNil(InitialImage);
    end;

    FreeAndNil(FRHIDepthTexture);
    TexDesc := TRHITextureDesc.Create2D(Width, Height, pfD24_UNorm_S8_UInt,
      [tuDepthStencil, tuSampled, tuTransferSrc], 1, 1);
    TexDesc.DebugName := 'RenderedTexture.DepthStencil';
    FRHIDepthTexture := RHIDevice.CreateTexture(TexDesc);

    FreeAndNil(RenderToTexture);
    RenderToTexture := TGLRenderToTexture.Create(Width, Height);
    RenderToTexture.SetRHITexture(FRHITexture, FRHIDepthTexture);
    RenderToTexture.Buffer := tbColor;
    RenderToTexture.Stencil := true;
    RenderToTexture.GLContextOpen;
    CreateRHISampler(Filter, BoundaryModeS, BoundaryModeT, GL_CLAMP_TO_EDGE, Anisotropy, MipLevels);
    Exit;
  end;

  { calculate Filter, Anisotropy, NeedsMipmaps }
  NeedsMipmaps := Filter.NeedsMipmaps;
  if NeedsMipmaps and not HasGenerateMipmap then
  begin
    WritelnWarning('VRML/X3D' { This may be caused by OpenGL implementation
      limits, so it may be impossible to predict by VRML author,
      so it's "ignorable" warning. },
      'OpenGL(ES) doesn''t have glGenerateMipmap, so you cannot use mipmaps for RenderedTexture');
    Filter.Minification := minLinear;
    NeedsMipmaps := false;
  end;

  TextureWrap.Data[0] := BoundaryModeS;
  TextureWrap.Data[1] := BoundaryModeT;

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

  if (TextureNode.FdDepthMap.Count > 0) and
     TextureNode.FdDepthMap.Items[0] then
  begin
    WritelnWarning('VRML/X3D', 'RenderedTexture with depthMap = TRUE is no longer supported. For shadow maps, use dedicated CGE nodes.');
    Exit;
  end;

  InitialImage := TRGBImage.Create(Width, Height);
  try
    InitialImage.URL := 'generated:/' + TextureNode.NiceName;

    { Fill with deliberately stupid (but constant) color,
      to recognize easily RenderedTexture which don't have textures
      updated. }
    InitialImage.Clear(Vector4Byte(255, 0, 255, 255));

    FGLName := RendererCache.TextureImage_IncReference(
      InitialImage,
      '' { generated texture contents means empty URL },
      Filter,
      Anisotropy,
      TextureWrap,
      nil,
      GUITexture,
      false);

    { RenderedTexture never has any normal / height map
      (Hm, although it would be possible to generate some in theory
      --- after all, we generate it from 3D data. Idea for the future.)
    NormalMap := GLObjectNone;
    HeightMap := GLObjectNone;
    }
  finally FreeAndNil(InitialImage) end;

  RenderToTexture := TGLRenderToTexture.Create(Width, Height);
  RenderToTexture.SetTexture(FGLName, GL_TEXTURE_2D);
  RenderToTexture.Buffer := tbColor;
  RenderToTexture.Stencil := true;
  RenderToTexture.GLContextOpen;
end;

procedure TRenderedTextureResource.UnprepareCore;
begin
  FreeAndNil(RenderToTexture);
  if FRHITexture <> nil then
  begin
    TextureMemoryProfiler.DeallocateRHI(FRHITexture);
    FreeAndNil(FRHITexture);
  end;
  FreeAndNil(FRHIDepthTexture);
  FreeAndNil(FRHISampler);

  if FGLName <> GLObjectNone then
  begin
    RendererCache.TextureImage_DecReference(FGLName);
    FGLName := GLObjectNone;
  end;
end;

function TRenderedTextureResource.Bind(const TextureUnit: Cardinal): boolean;
begin
  if RHIDevice <> nil then
    Exit(FRHITexture <> nil);

  Result := FGLName <> GLObjectNone;
  if not Result then Exit;

  TRenderer.ActiveTexture(TextureUnit);
  glBindTexture(GL_TEXTURE_2D, FGLName);
end;

function TRenderedTextureResource.Enable(const TextureUnit: Cardinal;
  Shader: TShader; const Env: TTextureEnv): boolean;
begin
  Result := Bind(TextureUnit);
  if not Result then Exit;

  Shader.EnableTexture(TextureUnit, MyTextureType, TextureNode, Env);
end;

function TRenderedTextureResource.ShaderTextureType(out TextureType: TTextureType): Boolean;
begin
  TextureType := MyTextureType;
  Result := true;
end;

procedure TRenderedTextureResource.Update(
  const Render: TRenderFromViewFunction;
  const ProjectionNear, ProjectionFar: Single;
  const CurrentViewpoint: TAbstractViewpointNode;
  const CameraViewKnown: boolean;
  const CameraView: TViewVectors;
  const ShapeForViewpointMirror: TX3DRendererShape);

  function GetProjectionMatrix(const Viewpoint: TAbstractViewpointNode): TMatrix4;
  { We have to calculate projection, given Viewpoint node.
    Similar to TCastleSceneCore.InternalUpdateCamera.

    We have to calculate things a little differently, e.g. we have
    no NavigationInfo (it could be undesirable to use scene bound NavigationInfo,
    since X3D author cannot change it by any RenderedTexture field,
    and for now we have no field RenderedTexture.navigationInfo...),
    no knowledge of scene box (although we do have ProjectionNear / Far
    already).
  }

    procedure DoPerspective;
    var
      FieldOfView, AngleOfViewY: Single;
      FieldOfViewAxis: TFieldOfViewAxis;
      Angles: TVector2;
    begin
      if (Viewpoint <> nil) and
         (Viewpoint is TViewpointNode) then
      begin
        FieldOfView := TViewpointNode(Viewpoint).FieldOfView;
        if TViewpointNode(Viewpoint).FieldOfViewForceVertical then
          FieldOfViewAxis := faVertical
        else
          FieldOfViewAxis := faSmallest;
      end else
      if (Viewpoint <> nil) and
         (Viewpoint is TPerspectiveCameraNode_1) then
      begin
        FieldOfView := TPerspectiveCameraNode_1(Viewpoint).FdHeightAngle.Value;
        FieldOfViewAxis := faSmallest;
      end else
      begin
        FieldOfView := DefaultViewpointFieldOfView;
        FieldOfViewAxis := faSmallest;
      end;

      Angles := TViewpointNode.InternalFieldOfView(FieldOfView, FieldOfViewAxis, Width, Height);
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

      FinalFieldOfView := TOrthoViewpointNode.InternalFieldOfView(
        FinalFieldOfView, Width, Height);

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
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('TRenderedTextureResource.Update-ProjectionType?');
      {$endif}
    end;
  end;

  procedure GetRenderedTextureCamera(const Viewpoint: TAbstractViewpointNode;
    out CameraRender: TViewVectors);

    procedure GetFromCurrent;
    begin
      if CameraViewKnown then
      begin
        CameraRender := CameraView;
      end else
      if CurrentViewpoint <> nil then
        CurrentViewpoint.GetView(CameraRender)
      else
      begin
        { If all else fails (no viewpoint node bound, not known current
          camera settings) then use defaults. }
        CameraRender := DefaultX3DCameraView;
      end;
    end;

  begin
    if Viewpoint = CurrentViewpoint then
      GetFromCurrent else
    begin
      { Viewpoint gets assigned something different than CurrentViewpoint
        only when it's non-nil. }
      Assert(Viewpoint <> nil);
      Viewpoint.GetView(CameraRender);
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

    Tri[0] := Points.L[0];
    Tri[1] := Points.L[1];
    Tri[2] := Points.L[2];
    if Tri.IsValid then
      Normal := Tri.Normal;

    for I := 3 to Points.Count - 1 do
    begin
      Tri[1] := Tri[2];
      Tri[2] := Points.L[I];
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
        -Normal.X * Point.X
        -Normal.Y * Point.Y
        -Normal.Z * Point.Z;
    end;
    PlaneShift := PlaneShift / Points.Count;

    Plane.W := PlaneShift;
    Result := true;
  end;

  procedure GetMatricesForViewpoint(const Viewpoint: TAbstractViewpointNode;
    out CameraRender: TViewVectors;
    out ProjectionMatrix: TMatrix4);
  begin
    ProjectionMatrix := GetProjectionMatrix(Viewpoint);
    GetRenderedTextureCamera(Viewpoint, CameraRender);
  end;

  procedure GetMatricesForViewpointMirror(const ViewpointMirror: TViewpointMirrorNode;
    out CameraRender: TViewVectors;
    out ProjectionMatrix: TMatrix4);
  var
    PosOnPlane, Side, Coord, PlaneCoord: TVector3;
    PlaneCoordProjected: TVector2;
    Plane: TVector4;
    ZNear: Single;
    FrustumDimensions: TFloatRectangle;
    ShapeBox: TBox3d;
    ShapeCorners: TBoxCorners;
    GeometryCoordsField: TMFVec3f;
    GeometryCoords: TVector3List;
  begin
    if (ShapeForViewpointMirror.ParentScene = nil) or
       (not ShapeForViewpointMirror.ParentScene.HasWorldTransform) then
    begin
      WritelnWarning('ViewpointMirror can only work in TCastleScene that is part of viewport exactly once (without TCastleTransformReference)');
      GetMatricesForViewpoint(CurrentViewpoint, CameraRender, ProjectionMatrix);
      Exit;
    end;

    { calculate camera CameraRender (in world space) }
    if not CameraViewKnown then
    begin
      WritelnWarning('ViewpointMirror will not work correctly until camera vectors are known');
      GetMatricesForViewpoint(CurrentViewpoint, CameraRender, ProjectionMatrix);
      Exit;
    end;
    GetRenderedTextureCamera(CurrentViewpoint, CameraRender);

    { calculate GeometryCoords }
    GeometryCoords := nil;
    if ShapeForViewpointMirror.Geometry.InternalCoord(ShapeForViewpointMirror.State, GeometryCoordsField) and
       (GeometryCoordsField <> nil) then
      GeometryCoords := GeometryCoordsField.Items;
    if GeometryCoords = nil then
    begin
      WritelnWarning('ViewpointMirror can only be used on node with coordinates');
      GetMatricesForViewpoint(CurrentViewpoint, CameraRender, ProjectionMatrix);
      Exit;
    end;

    { calculate Plane of the mirror from GeometryCoords (in local shape coords) }
    if not PointsPlane(GeometryCoords, Plane) then
    begin
      WritelnWarning('ViewpointMirror plane cannot be determined, the shape does not define a plane');
      GetMatricesForViewpoint(CurrentViewpoint, CameraRender, ProjectionMatrix);
      Exit;
    end;

    { convert Plane to world coords }
    Plane := PlaneTransform(Plane,
      ShapeForViewpointMirror.ParentScene.WorldTransform *
      ShapeForViewpointMirror.State.Transformation.Transform);

    { reflect Pos, Dir, Up for mirror view }
    PosOnPlane := PointOnPlaneClosestToPoint(Plane, CameraRender.Translation);
    CameraRender.Translation := 2 * PosOnPlane - CameraRender.Translation; // mirror Pos versus Plane
    CameraRender.Direction := PosOnPlane - CameraRender.Translation;
    //CameraRender.Up := no need to change

    { calculate Side, and make Dir, Up normalized and orthogonal }
    CameraRender.Direction := CameraRender.Direction.Normalize;
    //CameraRender.Up := CameraRender.Up.Normalize; // no need to
    MakeVectorsOrthoOnTheirPlane(CameraRender.Up, CameraRender.Direction);
    Side := TVector3.CrossProduct(CameraRender.Direction, CameraRender.Up);

    { At this point CameraRender is ready. }

    { calculate FrustumDimensions,
      to include ShapeForViewpointMirror bounding box projected on the Plane. }
    ShapeBox := ShapeForViewpointMirror.BoundingBox.Transform(
      ShapeForViewpointMirror.ParentScene.WorldTransform);
    ShapeBox.Corners(ShapeCorners);
    FrustumDimensions := TFloatRectangle.Empty;
    for Coord in ShapeCorners do
    begin
      PlaneCoord := PointOnPlaneClosestToPoint(Plane, Coord) - PosOnPlane;
      PlaneCoordProjected := Vector2(
        TVector3.DotProduct(PlaneCoord, Side),
        TVector3.DotProduct(PlaneCoord, CameraRender.Up)
      );
      FrustumDimensions := FrustumDimensions.Include(PlaneCoordProjected);
    end;

    ZNear := PointsDistance(CameraRender.Translation, PosOnPlane) +
      ViewpointMirror.DistanceFromShape;
    ProjectionMatrix := FrustumProjectionMatrix(FrustumDimensions,
      ZNear, ProjectionFar);

    if ShapeForViewpointMirror.MirrorPlaneUniforms = nil then
      ShapeForViewpointMirror.MirrorPlaneUniforms := TMirrorPlaneUniforms.Create;
    ShapeForViewpointMirror.MirrorPlaneUniforms.NormalizedPlane := Plane;
    ShapeForViewpointMirror.MirrorPlaneUniforms.CameraPositionOnPlane := PosOnPlane;
    ShapeForViewpointMirror.MirrorPlaneUniforms.CameraSide := Side;
    ShapeForViewpointMirror.MirrorPlaneUniforms.CameraUp := CameraRender.Up;
    ShapeForViewpointMirror.MirrorPlaneUniforms.FrustumDimensions := FrustumDimensions;
  end;

var
  CameraRender: TViewVectors;
  SavedProjectionMatrix, NewProjectionMatrix: TMatrix4;
  Camera: TRenderingCamera;
  SavedViewport: TRectangle;
begin
  if (FGLName = GLObjectNone) and (FRHITexture = nil) then Exit;

  { calculate matrices to use when rendering,
    using algorithm based on TextureNode.Viewpoint }
  if TextureNode.FdViewpoint.Value is TViewpointMirrorNode then
    GetMatricesForViewpointMirror(
      TViewpointMirrorNode(TextureNode.FdViewpoint.Value),
      CameraRender, NewProjectionMatrix)
  else
  if TextureNode.FdViewpoint.Value is TAbstractViewpointNode then
    GetMatricesForViewpoint(
      TAbstractViewpointNode(TextureNode.FdViewpoint.Value),
      CameraRender, NewProjectionMatrix)
  else
    GetMatricesForViewpoint(
      CurrentViewpoint,
      CameraRender, NewProjectionMatrix);

  TextureNode.EventProjection.Send(NewProjectionMatrix);
  TextureNode.EventViewing.Send(CameraRender.Matrix);
  TextureNode.EventRendering.Send(true);

  Camera := TRenderingCamera.Create;
  try
    Camera.Target := rfRenderedTexture;
    Camera.FromViewVectors(CameraRender, NewProjectionMatrix);

    RenderToTexture.RenderBegin;

      if RenderContext <> nil then
      begin
        SavedViewport := RenderContext.Viewport;
        RenderContext.Viewport := Rectangle(0, 0, Width, Height);

        SavedProjectionMatrix := RenderContext.ProjectionMatrix;
        RenderContext.ProjectionMatrix := NewProjectionMatrix;
      end;

      Render(Camera);

      if RenderContext <> nil then
      begin
        RenderContext.ProjectionMatrix := SavedProjectionMatrix;
        RenderContext.Viewport := SavedViewport;
      end;

    RenderToTexture.RenderEnd;
  finally FreeAndNil(Camera) end;

  if NeedsMipmaps then
    RenderToTexture.GenerateMipmap;

  if RHIDevice <> nil then
    Inc(FRHIUpdateCount);

  TextureNode.EventRendering.Send(false);
end;

function TRenderedTextureResource.GLName: TGLTextureId;
begin
  Result := FGLName;
end;

{ TCubeMapTextureResource ------------------------------------------------------ }

function TCubeMapTextureResource.TextureNode: TAbstractEnvironmentTextureNode;
begin
  Result := TAbstractEnvironmentTextureNode(inherited TextureNode);
end;

procedure TCubeMapTextureResource.UnprepareCore;
begin
  if FRHITexture <> nil then
    RendererCache.RHITextureCubeMap_DecReference(FRHITexture);
  FreeAndNil(FRHISampler);
  if GLName <> GLObjectNone then
  begin
    RendererCache.TextureCubeMap_DecReference(GLName);
    GLName := GLObjectNone;
  end;
end;

function TCubeMapTextureResource.Bind(const TextureUnit: Cardinal): boolean;
begin
  if RHIDevice <> nil then
    Exit(FRHITexture <> nil);

  Result := GLName <> GLObjectNone;
  if not Result then Exit;

  TRenderer.ActiveTexture(TextureUnit);
  glBindTexture(GL_TEXTURE_CUBE_MAP, GLName);
end;

function TCubeMapTextureResource.Enable(const TextureUnit: Cardinal;
  Shader: TShader; const Env: TTextureEnv): boolean;
begin
  Result := Bind(TextureUnit);
  if not Result then Exit;

  Shader.EnableTexture(TextureUnit, MyTextureType, TextureNode, Env);
end;

function TCubeMapTextureResource.ShaderTextureType(out TextureType: TTextureType): Boolean;
begin
  TextureType := MyTextureType;
  Result := true;
end;

{ TComposedCubeMapTextureResource ---------------------------------------------- }

class function TComposedCubeMapTextureResource.IsClassForTextureNode(
  ANode: TAbstractTextureNode): boolean;
begin
  Result := ANode is TComposedCubeMapTextureNode;
end;

function TComposedCubeMapTextureResource.TextureNode: TComposedCubeMapTextureNode;
begin
  Result := TComposedCubeMapTextureNode(inherited TextureNode);
end;

procedure TComposedCubeMapTextureResource.PrepareCore(
  const RenderOptions: TCastleRenderOptions);

  function SideUrl(const SideField: TSFNode): String;
  var
    SideNode: TAbstractTexture2DNode;
  begin
    if (SideField.Value <> nil) and (SideField.Value is TAbstractTexture2DNode) then
    begin
      SideNode := TAbstractTexture2DNode(SideField.Value);
      Result := SideNode.TextureUsedFullUrl;
      if (Result = '') and (SideNode.TextureImage <> nil) then
        Result := SideNode.TextureImage.URL;
    end else
      Result := '';
  end;

  function ComposedCubeMapFullUrl: String;
  var
    U0, U1, U2, U3, U4, U5: String;
  begin
    U0 := SideUrl(TextureNode.FdBack);
    U1 := SideUrl(TextureNode.FdFront);
    U2 := SideUrl(TextureNode.FdLeft);
    U3 := SideUrl(TextureNode.FdRight);
    U4 := SideUrl(TextureNode.FdTop);
    U5 := SideUrl(TextureNode.FdBottom);
    if (U0 <> '') and (U1 <> '') and (U2 <> '') and
       (U3 <> '') and (U4 <> '') and (U5 <> '') then
      Result := U0 + '|' + U1 + '|' + U2 + '|' + U3 + '|' + U4 + '|' + U5
    else
      Result := '';
  end;

var
  Filter: TTextureFilter;
  Anisotropy: TGLfloat;
  BackRot, FrontRot, LeftRot, RightRot, TopImg, BottomImg, TopResized, BottomResized: TCastleImage;
  GUITexture: boolean;
  CubeSize, MipLevels: Cardinal;
  TexFormat: TRHIPixelFormat;
  TexDesc: TRHITextureDesc;
  CacheFullUrl: String;
begin
  if RHIDevice <> nil then
  begin
    if not TextureNode.LoadSides then
    begin
      WritelnWarning('VRML/X3D', 'Not all sides of a CubeMapTexture are correctly set and loaded, cannot use cube map');
      Exit;
    end;

    HandleTextureProperties(TextureNode.TextureProperties, RenderOptions,
      Filter, Anisotropy, GUITexture);

    if FRHITexture <> nil then
      RendererCache.RHITextureCubeMap_DecReference(FRHITexture);
    CacheFullUrl := ComposedCubeMapFullUrl;
    if CacheFullUrl <> '' then
    begin
      FRHITexture := RendererCache.FindRHITextureCubeMap(
        CacheFullUrl, Filter, GUITexture, GLTextureScale);
      if FRHITexture <> nil then
      begin
        CreateRHISampler(Filter, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_EDGE,
          Anisotropy, Max(Cardinal(1), FRHITexture.Desc.MipLevels));
        Exit;
      end;
    end;

    BackRot       := nil;
    FrontRot      := nil;
    LeftRot       := nil;
    RightRot      := nil;
    TopResized    := nil;
    BottomResized := nil;
    try
      BackRot   := (TAbstractTexture2DNode(TextureNode.FdBack  .Value).TextureImage as TCastleImage).MakeRotated(2);
      FrontRot  := (TAbstractTexture2DNode(TextureNode.FdFront .Value).TextureImage as TCastleImage).MakeRotated(2);
      LeftRot   := (TAbstractTexture2DNode(TextureNode.FdLeft  .Value).TextureImage as TCastleImage).MakeRotated(2);
      RightRot  := (TAbstractTexture2DNode(TextureNode.FdRight .Value).TextureImage as TCastleImage).MakeRotated(2);
      TopImg    :=  TAbstractTexture2DNode(TextureNode.FdTop   .Value).TextureImage as TCastleImage;
      BottomImg :=  TAbstractTexture2DNode(TextureNode.FdBottom.Value).TextureImage as TCastleImage;

      CubeSize := Max(1, Max(Max(RightRot.Width, RightRot.Height),
        Max(Max(LeftRot.Width, LeftRot.Height),
        Max(Max(TopImg.Width, TopImg.Height),
        Max(Max(BottomImg.Width, BottomImg.Height),
        Max(Max(BackRot.Width, BackRot.Height),
            Max(FrontRot.Width, FrontRot.Height)))))));
      if (not GUITexture) and (GLTextureScale > 1) and (CubeSize > GLTextureMinSize) then
        CubeSize := Max(CubeSize shr (GLTextureScale - 1), GLTextureMinSize);
      if RHIDevice.Capabilities.MaxTextureSize > 0 then
        CubeSize := Min(CubeSize, RHIDevice.Capabilities.MaxTextureSize);

      if (RightRot.Width <> CubeSize) or (RightRot.Height <> CubeSize) then
        RightRot.Resize(CubeSize, CubeSize, riBilinear);
      if (LeftRot.Width <> CubeSize) or (LeftRot.Height <> CubeSize) then
        LeftRot.Resize(CubeSize, CubeSize, riBilinear);
      if (BackRot.Width <> CubeSize) or (BackRot.Height <> CubeSize) then
        BackRot.Resize(CubeSize, CubeSize, riBilinear);
      if (FrontRot.Width <> CubeSize) or (FrontRot.Height <> CubeSize) then
        FrontRot.Resize(CubeSize, CubeSize, riBilinear);
      if (TopImg.Width <> CubeSize) or (TopImg.Height <> CubeSize) then
      begin
        TopResized := TopImg.MakeResized(CubeSize, CubeSize, riBilinear);
        TopImg := TopResized;
      end;
      if (BottomImg.Width <> CubeSize) or (BottomImg.Height <> CubeSize) then
      begin
        BottomResized := BottomImg.MakeResized(CubeSize, CubeSize, riBilinear);
        BottomImg := BottomResized;
      end;

      if Filter.NeedsMipmaps then
        MipLevels := CalculateTextureMipLevels(CubeSize)
      else
        MipLevels := 1;

      TexFormat := EncodedImageToRHIPixelFormat(RightRot);
      TexDesc := TRHITextureDesc.CreateCube(CubeSize, TexFormat,
        [tuSampled, tuTransferDst, tuTransferSrc], MipLevels);
      TexDesc.DebugName := 'ComposedCubeMapTexture';
      FRHITexture := RHIDevice.CreateTexture(TexDesc);
      FRHITexture.UploadImage(RightRot,  0, 0);
      FRHITexture.UploadImage(LeftRot,   0, 1);
      FRHITexture.UploadImage(TopImg,    0, 2);
      FRHITexture.UploadImage(BottomImg, 0, 3);
      FRHITexture.UploadImage(BackRot,   0, 4);
      FRHITexture.UploadImage(FrontRot,  0, 5);
      if MipLevels > 1 then
        FRHITexture.GenerateMipmaps;
      TextureMemoryProfiler.AllocateRHI(FRHITexture,
        Iff(CacheFullUrl <> '', CacheFullUrl, RightRot.URL),
        RightRot.ClassName,
        EncodedImageToRHIGpuBytes(RightRot, FRHITexture.Desc.Format) * 6,
        MipLevels > 1, CubeSize, CubeSize, 6);
      if CacheFullUrl <> '' then
        RendererCache.AddRHITextureCubeMap(
          CacheFullUrl, Filter, GUITexture, GLTextureScale, FRHITexture);
      CreateRHISampler(Filter, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_EDGE, Anisotropy, MipLevels);
    finally
      FreeAndNil(BackRot);
      FreeAndNil(FrontRot);
      FreeAndNil(LeftRot);
      FreeAndNil(RightRot);
      FreeAndNil(TopResized);
      FreeAndNil(BottomResized);
    end;
    Exit;
  end;

  if not GLFeatures.TextureCubeMap then
  begin
    WritelnWarning('VRML/X3D', 'Rendering context doesn''t support cube maps, cannot use ComposedCubeMapTexture');
    Exit;
  end;

  if not TextureNode.LoadSides then
  begin
    WritelnWarning('VRML/X3D', 'Not all sides of a CubeMapTexture are correctly set and loaded, cannot use cube map');
    Exit;
  end;

  HandleTextureProperties(TextureNode.TextureProperties, RenderOptions,
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

    GLName := RendererCache.TextureCubeMap_IncReference(
      { TODO: we could implement TComposedCubeMapTextureNode.TextureUsedFullUrl
        by glueing 6 URLs for 6 sides, and pass it below. }
      '',
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

{ TImageCubeMapTextureResource ------------------------------------------------- }

class function TImageCubeMapTextureResource.IsClassForTextureNode(
  ANode: TAbstractTextureNode): boolean;
begin
  Result := ANode is TImageCubeMapTextureNode;
end;

function TImageCubeMapTextureResource.TextureNode: TImageCubeMapTextureNode;
begin
  Result := TImageCubeMapTextureNode(inherited TextureNode);
end;

procedure TImageCubeMapTextureResource.PrepareCore(
  const RenderOptions: TCastleRenderOptions);
var
  Filter: TTextureFilter;
  Anisotropy: TGLfloat;
  GUITexture: boolean;
  Composite: TCompositeImage;
  CubeSize, MipLevels, MipIdx: Cardinal;
  TexFormat: TRHIPixelFormat;
  TexDesc: TRHITextureDesc;
  Side: TCubeMapSide;
  SideImg: TEncodedImage;
  ResizedSide: TCastleImage;
  CacheFullUrl: String;
begin
  if RHIDevice <> nil then
  begin
    Composite := TextureNode.LoadImage;
    if Composite = nil then Exit;
    try
      HandleTextureProperties(TextureNode.TextureProperties, RenderOptions,
        Filter, Anisotropy, GUITexture);
      SideImg := Composite.CubeMapImage(csPositiveX);
      CacheFullUrl := SideImg.URL;
      if FRHITexture <> nil then
        RendererCache.RHITextureCubeMap_DecReference(FRHITexture);
      if CacheFullUrl <> '' then
      begin
        FRHITexture := RendererCache.FindRHITextureCubeMap(
          CacheFullUrl, Filter, GUITexture, GLTextureScale);
        if FRHITexture <> nil then
        begin
          CreateRHISampler(Filter, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_EDGE,
            Anisotropy, Max(Cardinal(1), FRHITexture.Desc.MipLevels));
          Exit;
        end;
      end;
      TexFormat := EncodedImageToRHIPixelFormat(SideImg);
      CubeSize := Max(1, Composite.Width);
      if (not GUITexture) and (GLTextureScale > 1) and (CubeSize > GLTextureMinSize) and
         (SideImg is TCastleImage) and
         not (Composite.Mipmaps and (Composite.MipmapsCount > 1)) then
        CubeSize := Max(CubeSize shr (GLTextureScale - 1), GLTextureMinSize);
      if RHIDevice.Capabilities.MaxTextureSize > 0 then
        CubeSize := Min(CubeSize, RHIDevice.Capabilities.MaxTextureSize);
      if Filter.NeedsMipmaps then
      begin
        if Composite.Mipmaps and (Composite.MipmapsCount > 0) then
          MipLevels := Max(Cardinal(1), Composite.MipmapsCount)
        else if SideImg is TGPUCompressedImage then
          MipLevels := 1
        else
          MipLevels := CalculateTextureMipLevels(CubeSize);
      end else
        MipLevels := 1;
      TexDesc := TRHITextureDesc.CreateCube(CubeSize, TexFormat,
        [tuSampled, tuTransferDst, tuTransferSrc], MipLevels);
      TexDesc.DebugName := 'ImageCubeMapTexture';
      FRHITexture := RHIDevice.CreateTexture(TexDesc);
      for Side := Low(TCubeMapSide) to High(TCubeMapSide) do
      begin
        SideImg := Composite.CubeMapImage(Side, 0);
        if (SideImg is TCastleImage) and
           ((SideImg.Width <> CubeSize) or (SideImg.Height <> CubeSize)) then
        begin
          ResizedSide := TCastleImage(SideImg).MakeResized(CubeSize, CubeSize, riBilinear);
          try
            FRHITexture.UploadImage(ResizedSide, 0, Ord(Side));
          finally
            FreeAndNil(ResizedSide);
          end;
        end else
          FRHITexture.UploadImage(SideImg, 0, Ord(Side));
        if (MipLevels > 1) and Composite.Mipmaps and (Composite.MipmapsCount > 1) then
        begin
          for MipIdx := 1 to Min(Composite.MipmapsCount - 1, MipLevels - 1) do
            FRHITexture.UploadImage(Composite.CubeMapImage(Side, MipIdx), MipIdx, Ord(Side));
        end;
      end;
      if (MipLevels > 1) and not (Composite.Mipmaps and (Composite.MipmapsCount > 1)) then
        FRHITexture.GenerateMipmaps;
      SideImg := Composite.CubeMapImage(csPositiveX);
      TextureMemoryProfiler.AllocateRHI(FRHITexture,
        CacheFullUrl,
        SideImg.ClassName,
        EncodedImageToRHIGpuBytes(SideImg, FRHITexture.Desc.Format) *
          Int64(CubeSize) * Int64(CubeSize) div
          Max(Int64(1), Int64(SideImg.Width) * Int64(SideImg.Height)) * 6,
        MipLevels > 1, CubeSize, CubeSize, 6);
      if CacheFullUrl <> '' then
        RendererCache.AddRHITextureCubeMap(
          CacheFullUrl, Filter, GUITexture, GLTextureScale, FRHITexture);
      CreateRHISampler(Filter, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_EDGE, Anisotropy, MipLevels);
    finally
      FreeAndNil(Composite);
    end;
    Exit;
  end;

  if not GLFeatures.TextureCubeMap then
  begin
    WritelnWarning('VRML/X3D', 'Rendering context doesn''t support cube maps, cannot use ImageCubeMapTexture');
    Exit;
  end;

  Composite := TextureNode.LoadImage;
  { If TextureNode doesn't contain anything useful, just exit.
    TextureNode.LoadImage already did necessary WritelnWarnings. }
  if Composite = nil then Exit;

  try

    HandleTextureProperties(TextureNode.TextureProperties, RenderOptions,
      Filter, Anisotropy, GUITexture);

    { TODO: this is a quick and dirty method:
      - We call LoadImage each time, while load calls should
        be minimized (to avoid loading image many times, but also
        to avoid making repeated warnings in case image fails).
        Should be cached, like for 2D texture nodes.
      - We do not use cube map mipmaps stored inside Composite file.
    }

    GLName := RendererCache.TextureCubeMap_IncReference(
      { TODO: we could implement TImageCubeMapTextureNode.TextureUsedFullUrl and pass it below. }
      '',
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

{ TGeneratedCubeMapTextureResource --------------------------------------------- }

class function TGeneratedCubeMapTextureResource.IsClassForTextureNode(
  ANode: TAbstractTextureNode): boolean;
begin
  Result := ANode is TGeneratedCubeMapTextureNode;
end;

function TGeneratedCubeMapTextureResource.TextureNode: TGeneratedCubeMapTextureNode;
begin
  Result := TGeneratedCubeMapTextureNode(inherited TextureNode);
end;

procedure TGeneratedCubeMapTextureResource.PrepareCore(
  const RenderOptions: TCastleRenderOptions);
var
  Filter: TTextureFilter;
  Anisotropy: TGLfloat;
  GUITexture: boolean;
  InitialImage: TCastleImage;
  MipLevels, FaceIdx: Cardinal;
  TexDesc: TRHITextureDesc;
begin
  if RHIDevice <> nil then
  begin
    HandleTextureProperties(TextureNode.TextureProperties, RenderOptions,
      Filter, Anisotropy, GUITexture);
    NeedsMipmaps := Filter.NeedsMipmaps;
    Size := Max(1, TextureNode.FdSize.Value);
    if not IsPowerOf2(Size) then
      Size := 1 shl Biggest2Exponent(Size);
    if (not GUITexture) and (GLTextureScale > 1) and (Size > GLTextureMinSize) then
      Size := Max(Size shr (GLTextureScale - 1), GLTextureMinSize);
    if (RHIDevice.Capabilities.MaxTextureSize > 0) and (Size > RHIDevice.Capabilities.MaxTextureSize) then
      Size := RHIDevice.Capabilities.MaxTextureSize;
    if NeedsMipmaps then
      MipLevels := CalculateTextureMipLevels(Size)
    else
      MipLevels := 1;

    if FRHITexture <> nil then
    begin
      TextureMemoryProfiler.DeallocateRHI(FRHITexture);
      FreeAndNil(FRHITexture);
    end;
    TexDesc := TRHITextureDesc.CreateCube(Size, pfRGBA8_UNorm,
      [tuColorAttachment, tuSampled, tuTransferSrc, tuTransferDst], MipLevels);
    TexDesc.DebugName := 'GeneratedCubeMapTexture';
    FRHITexture := RHIDevice.CreateTexture(TexDesc);

    InitialImage := TRGBImage.Create(Size, Size);
    try
      InitialImage.URL := 'generated:/' + TextureNode.NiceName;
      InitialImage.Clear(Vector4Byte(255, 0, 255, 255));
      for FaceIdx := 0 to 5 do
        FRHITexture.UploadImage(InitialImage, 0, FaceIdx);
      if MipLevels > 1 then
        FRHITexture.GenerateMipmaps;
      TextureMemoryProfiler.AllocateRHI(FRHITexture,
        '', InitialImage.ClassName,
        Int64(Size) * Int64(Size) * 4 * 6 +
        Iff(MipLevels > 1, 6 * TTextureMemoryProfiler.MipmapsSize(InitialImage), 0),
        MipLevels > 1, Size, Size, 6);
    finally
      FreeAndNil(InitialImage);
    end;

    FreeAndNil(FRHIDepthTexture);
    TexDesc := TRHITextureDesc.Create2D(Size, Size, pfD24_UNorm_S8_UInt,
      [tuDepthStencil, tuSampled, tuTransferSrc], 1, 1);
    TexDesc.DebugName := 'GeneratedCubeMapTexture.Depth';
    FRHIDepthTexture := RHIDevice.CreateTexture(TexDesc);

    FreeAndNil(RenderToTexture);
    RenderToTexture := TGLRenderToTexture.Create(Size, Size);
    RenderToTexture.SetRHITexture(FRHITexture, FRHIDepthTexture);
    RenderToTexture.Buffer := tbColor;
    RenderToTexture.GLContextOpen;
    CreateRHISampler(Filter, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_EDGE, Anisotropy, MipLevels);
    Exit;
  end;

  if not GLFeatures.TextureCubeMap then
  begin
    WritelnWarning('VRML/X3D', 'Rendering context doesn''t support cube maps, cannot use GeneratedCubeMapTexture');
    Exit;
  end;

  HandleTextureProperties(TextureNode.TextureProperties, RenderOptions,
    Filter, Anisotropy, GUITexture);

  { calculate Filter, Anisotropy, NeedsMipmaps }
  NeedsMipmaps := Filter.NeedsMipmaps;
  if NeedsMipmaps and not HasGenerateMipmap then
  begin
    WritelnWarning('VRML/X3D' { This may be caused by OpenGL implementation
      limits, so it may be impossible to predict by VRML author,
      so it's "ignorable" warning. },
      'OpenGL(ES) doesn''t have glGenerateMipmap, so you cannot use mipmaps for GeneratedCubeMapTexture');
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

    GLName := RendererCache.TextureCubeMap_IncReference(
      '' { generated texture contents means empty URL; this also prevents sharing textures by TextureCubeMap_IncReference },
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

procedure TGeneratedCubeMapTextureResource.UnprepareCore;
begin
  FreeAndNil(RenderToTexture);
  FreeAndNil(FRHIDepthTexture);
  inherited;
end;

procedure TGeneratedCubeMapTextureResource.Update(
  const Render: TRenderFromViewFunction;
  const ProjectionNear, ProjectionFar: Single;
  const CubeMiddle: TVector3);
begin
  if (GLName = GLObjectNone) and (FRHITexture = nil) then Exit;

  if (RHIDevice = nil) and (GLVersion <> nil) and GLVersion.BuggyGenerateCubeMap then Exit;

  GLCaptureCubeMapTexture(GLName, Size,
    CubeMiddle,
    Render, ProjectionNear, ProjectionFar,
    RenderToTexture);

  if RHIDevice <> nil then
  begin
    Inc(FRHIUpdateCount);
    Inc(FRHIFacesRenderedCount, 6);
  end;

  if NeedsMipmaps then
    RenderToTexture.GenerateMipmap;
end;

{ T3DTextureResource ----------------------------------------------------------- }

class function T3DTextureResource.IsClassForTextureNode(
  ANode: TAbstractTextureNode): boolean;
begin
  Result := ANode is TAbstractTexture3DNode;
end;

function T3DTextureResource.TextureNode: TAbstractTexture3DNode;
begin
  Result := TAbstractTexture3DNode(inherited TextureNode);
end;

procedure T3DTextureResource.PrepareCore(
  const RenderOptions: TCastleRenderOptions);
var
  Filter: TTextureFilter;
  Anisotropy: TGLfloat;
  BoundaryModeS: TGLenum;
  BoundaryModeT: TGLenum;
  BoundaryModeR: TGLenum;
  GUITexture: boolean;
  TextureWrap: TTextureWrap3D;
  TexFormat: TRHIPixelFormat;
  TexDesc: TRHITextureDesc;
  MaxDim, MipLevels, MipIdx: Cardinal;
  CacheFullUrl: String;
begin
  if RHIDevice <> nil then
  begin
    TextureNode.TextureLoaded := true;
    if TextureNode.TextureImage = nil then Exit;

    HandleTextureProperties(TextureNode.TextureProperties, RenderOptions,
      TextureRepeatToGL(TextureNode.FdRepeatS.Value), TextureRepeatToGL(TextureNode.FdRepeatT.Value),
      TextureRepeatToGL(TextureNode.FdRepeatR.Value), Filter, BoundaryModeS, BoundaryModeT, BoundaryModeR, Anisotropy, GUITexture);

    if FRHITexture <> nil then
      RendererCache.RHITexture3D_DecReference(FRHITexture);
    CacheFullUrl := TextureNode.TextureImage.URL;
    if CacheFullUrl <> '' then
    begin
      FRHITexture := RendererCache.FindRHITexture3D(CacheFullUrl, Filter);
      if FRHITexture <> nil then
      begin
        CreateRHISampler(Filter, BoundaryModeS, BoundaryModeT, BoundaryModeR,
          Anisotropy, Max(Cardinal(1), FRHITexture.Desc.MipLevels));
        Exit;
      end;
    end;

    MaxDim := Max(1, Max(TextureNode.TextureImage.Width,
      Max(TextureNode.TextureImage.Height, TextureNode.TextureImage.Depth)));
    if Filter.NeedsMipmaps then
    begin
      if (TextureNode.TextureComposite <> nil) and TextureNode.TextureComposite.Mipmaps and
         (TextureNode.TextureComposite.Images.Count > 0) then
        MipLevels := Max(Cardinal(1), Cardinal(TextureNode.TextureComposite.Images.Count))
      else if TextureNode.TextureImage is TGPUCompressedImage then
        MipLevels := 1
      else
        MipLevels := CalculateTextureMipLevels(MaxDim);
    end else
      MipLevels := 1;

    TexFormat := EncodedImageToRHIPixelFormat(TextureNode.TextureImage);
    TexDesc := TRHITextureDesc.Create3D(
      Max(1, TextureNode.TextureImage.Width),
      Max(1, TextureNode.TextureImage.Height),
      Max(1, TextureNode.TextureImage.Depth),
      TexFormat, [tuSampled, tuTransferDst, tuTransferSrc], MipLevels);
    TexDesc.DebugName := 'Texture3D';
    FRHITexture := RHIDevice.CreateTexture(TexDesc);
    FRHITexture.UploadImage(TextureNode.TextureImage, 0, 0);
    if (MipLevels > 1) and (TextureNode.TextureComposite <> nil) and
       TextureNode.TextureComposite.Mipmaps and
       (TextureNode.TextureComposite.Images.Count > 1) then
    begin
      for MipIdx := 1 to Min(Cardinal(TextureNode.TextureComposite.Images.Count) - 1, MipLevels - 1) do
        FRHITexture.UploadImage(TextureNode.TextureComposite.Images[MipIdx], MipIdx, 0);
    end else
    if MipLevels > 1 then
      FRHITexture.GenerateMipmaps;
    TextureMemoryProfiler.AllocateRHI(FRHITexture,
      CacheFullUrl,
      TextureNode.TextureImage.ClassName,
      EncodedImageToRHIGpuBytes(TextureNode.TextureImage, FRHITexture.Desc.Format),
      MipLevels > 1, TexDesc.Width, TexDesc.Height, TexDesc.DepthOrArrayLayers);
    if CacheFullUrl <> '' then
      RendererCache.AddRHITexture3D(CacheFullUrl, Filter, FRHITexture);
    CreateRHISampler(Filter, BoundaryModeS, BoundaryModeT, BoundaryModeR, Anisotropy, MipLevels);
    Exit;
  end;
{$H+}

  if not GLFeatures.Texture3D then
  begin
    WritelnWarning('VRML/X3D', 'Rendering context doesn''t support 3D textures, cannot use Texture3D nodes');
    Exit;
  end;

  TextureNode.TextureLoaded := true;

  { If TextureImage doesn't contain anything useful, just exit.
    Setting TextureLoaded already did necessary WritelnWarnings. }
  if TextureNode.TextureImage = nil then Exit;

  HandleTextureProperties(TextureNode.TextureProperties, RenderOptions,
    TextureRepeatToGL(TextureNode.FdRepeatS.Value), TextureRepeatToGL(TextureNode.FdRepeatT.Value),
    TextureRepeatToGL(TextureNode.FdRepeatR.Value), Filter, BoundaryModeS, BoundaryModeT, BoundaryModeR, Anisotropy, GUITexture);

  { calculate TextureWrap }
  TextureWrap.Data[0] := BoundaryModeS;
  TextureWrap.Data[1] := BoundaryModeT;
  TextureWrap.Data[2] := BoundaryModeR;

  GLName := RendererCache.Texture3D_IncReference(
    { TODO: We could implement TAbstractTexture3DNode.TextureUsedFullUrl }
    '',
    Filter, Anisotropy,
    TextureWrap, TextureNode.TextureImage, TextureNode.TextureComposite);
end;

procedure T3DTextureResource.UnprepareCore;
begin
  if FRHITexture <> nil then
    RendererCache.RHITexture3D_DecReference(FRHITexture);
  FreeAndNil(FRHISampler);
  if GLName <> GLObjectNone then
  begin
    RendererCache.Texture3D_DecReference(GLName);
    GLName := GLObjectNone;
  end;
end;

function T3DTextureResource.Bind(const TextureUnit: Cardinal): boolean;
begin
  if RHIDevice <> nil then
    Exit(FRHITexture <> nil);

  Result := GLName <> GLObjectNone;
  if not Result then Exit;

  TRenderer.ActiveTexture(TextureUnit);
  glBindTexture(GL_TEXTURE_3D, GLName);
end;

function T3DTextureResource.Enable(const TextureUnit: Cardinal;
  Shader: TShader; const Env: TTextureEnv): boolean;
begin
  Result := Bind(TextureUnit);
  if not Result then Exit;

  Shader.EnableTexture(TextureUnit, MyTextureType, TextureNode, Env);
end;

function T3DTextureResource.ShaderTextureType(out TextureType: TTextureType): Boolean;
begin
  TextureType := MyTextureType;
  Result := true;
end;

{ TGeneratedShadowMapResource ------------------------------------------------------ }

class function TGeneratedShadowMapResource.IsClassForTextureNode(
  ANode: TAbstractTextureNode): boolean;
begin
  Result := ANode is TGeneratedShadowMapNode;
end;

function TGeneratedShadowMapResource.TextureNode: TGeneratedShadowMapNode;
begin
  Result := TGeneratedShadowMapNode(inherited TextureNode);
end;

class function TGeneratedShadowMapResource.ClassVarianceShadowMaps(
  RenderOptions: TCastleRenderOptions): boolean;
begin
  if RHIDevice <> nil then
    Exit(RenderOptions.ShadowSampling = ssVarianceShadowMaps);

  Result :=
    (RenderOptions.ShadowSampling = ssVarianceShadowMaps) and
    GLFeatures.Shaders and
    GLFeatures.TextureFloat;
end;

procedure TGeneratedShadowMapResource.PrepareCore(
  const RenderOptions: TCastleRenderOptions);
var
  TextureWrap: TTextureWrap2D;
  Filter: TTextureFilter;
  MipLevels: Cardinal;
  TexDesc: TRHITextureDesc;
  SamplerDesc: TRHISamplerDesc;
begin
  // in fixed-function, do not create shadow maps, do not render to shadow maps
  if (GLFeatures <> nil) and GLFeatures.EnableFixedFunction then Exit;

  if RHIDevice <> nil then
  begin
    VarianceShadowMaps := ClassVarianceShadowMaps(RenderOptions);
    Size := Max(1, TextureNode.FdSize.Value);
    if (RHIDevice.Capabilities.MaxTextureSize > 0) and (Size > RHIDevice.Capabilities.MaxTextureSize) then
      Size := RHIDevice.Capabilities.MaxTextureSize;

    NeedsMipmaps := VarianceShadowMaps;
    if NeedsMipmaps then
      MipLevels := CalculateTextureMipLevels(Size)
    else
      MipLevels := 1;

    if TextureNode.CompareMode <> smCompareRLEqual then
      WritelnWarning('VRML/X3D', 'Only the standard GeneratedShadowMap.compareMode value, "COMPARE_R_LEQUAL" (in Pascal: smCompareRLEqual), is supported now. This is the only value sensible for shadow maps.');

    if FRHITexture <> nil then
    begin
      TextureMemoryProfiler.DeallocateRHI(FRHITexture);
      FreeAndNil(FRHITexture);
    end;
    FreeAndNil(FRHIDepthTexture);
    FreeAndNil(FRHISampler);
    FreeAndNil(RenderToTexture);

    if VarianceShadowMaps then
    begin
      TexDesc := TRHITextureDesc.Create2D(Size, Size, pfRGBA32_Float,
        [tuColorAttachment, tuSampled, tuTransferSrc, tuTransferDst], MipLevels, 1);
      TexDesc.DebugName := 'GeneratedShadowMap.VSM';
      FRHITexture := RHIDevice.CreateTexture(TexDesc);
      TextureMemoryProfiler.AllocateRHI(FRHITexture,
        '', 'rgb-float',
        Int64(Size) * Int64(Size) * 16 +
        Iff(MipLevels > 1, Int64(Size) * Int64(Size) * 16 div 3, 0),
        MipLevels > 1, Size, Size, 1);

      TexDesc := TRHITextureDesc.Create2D(Size, Size, pfD24_UNorm_S8_UInt,
        [tuDepthStencil, tuSampled, tuTransferSrc], 1, 1);
      TexDesc.DebugName := 'GeneratedShadowMap.VSMDepth';
      FRHIDepthTexture := RHIDevice.CreateTexture(TexDesc);
    end else
    begin
      TexDesc := TRHITextureDesc.Create2D(Size, Size, pfD32_Float,
        [tuDepthStencil, tuSampled, tuTransferSrc], 1, 1);
      TexDesc.DebugName := 'GeneratedShadowMap.Depth';
      FRHITexture := RHIDevice.CreateTexture(TexDesc);
      FRHIDepthTexture := nil;
      TextureMemoryProfiler.AllocateRHI(FRHITexture,
        '', 'depth-int',
        Int64(Size) * Int64(Size) * 4,
        False, Size, Size, 1);
    end;

    FillChar(SamplerDesc, SizeOf(SamplerDesc), 0);
    SamplerDesc.MinFilter := sfLinear;
    SamplerDesc.MagFilter := sfLinear;
    if NeedsMipmaps then
      SamplerDesc.MipFilter := sfLinear
    else
      SamplerDesc.MipFilter := sfNearest;
    SamplerDesc.AddressModeU := amClampToEdge;
    SamplerDesc.AddressModeV := amClampToEdge;
    SamplerDesc.AddressModeW := amClampToEdge;
    SamplerDesc.MaxAnisotropy := 1.0;
    SamplerDesc.CompareEnable := not VarianceShadowMaps;
    SamplerDesc.CompareOp := coLessOrEqual;
    SamplerDesc.MinLod := 0.0;
    SamplerDesc.MaxLod := 1000.0;
    FRHISampler := RHIDevice.CreateSampler(SamplerDesc);

    RenderToTexture := TGLRenderToTexture.Create(Size, Size);
    if VarianceShadowMaps then
    begin
      RenderToTexture.Buffer := tbColor;
      RenderToTexture.SetRHITexture(FRHITexture, FRHIDepthTexture);
    end else
    begin
      RenderToTexture.Buffer := tbDepth;
      RenderToTexture.SetRHITexture(nil, FRHITexture);
    end;
    RenderToTexture.Stencil := false;
    RenderToTexture.GLContextOpen;
    Exit;
  end;

  { Float texture extensions: there is also NV_float_buffer,
    which is the only way for a float texture on old nvidia GPUs.
    But it's only for NV_texture_rectangle, without bilinear filtering
    or mipmapping, so it's not useful for VSM at all. }

  VarianceShadowMaps := ClassVarianceShadowMaps(RenderOptions);

  WritelnLog('Shadows', Format('Variance Shadow Maps used: %s (Reasons: RenderOptions.Shadow sampling = %s, GLSL support = %s, texture_float support = %s)', [
    BoolToStr(VarianceShadowMaps, true),
    ShadowSamplingNames[RenderOptions.ShadowSampling],
    BoolToStr(GLFeatures.Shaders, true),
    BoolToStr(GLFeatures.TextureFloat, true)
  ]));

  { TODO: fix TextureNode.FdSize.Value if needed }
  Size := TextureNode.FdSize.Value;

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

  if TextureNode.CompareMode <> smCompareRLEqual then
    WritelnWarning('VRML/X3D', 'Only the standard GeneratedShadowMap.compareMode value, "COMPARE_R_LEQUAL" (in Pascal: smCompareRLEqual), is supported now. This is the only value sensible for shadow maps.');

  if VarianceShadowMaps then
  begin
    FGLName := RendererCache.TextureFloat_IncReference(
      '' { generated texture contents means empty URL },
      Filter, TextureWrap, Size, Size, true);
  end else
  begin
    if not GLFeatures.TextureDepthCompare then
    begin
      WritelnWarning('VRML/X3D', 'Shadow maps not supported. OpenGL(ES) doesn''t support shadow samplers with comparison (TextureDepthCompare), cannot use GeneratedShadowMap nodes.');
      Exit;
    end;

    FGLName := RendererCache.TextureDepth_IncReference(
      '' { generated texture contents means empty URL },
      TextureWrap, Size, Size);
  end;

  RenderToTexture := TGLRenderToTexture.Create(Size, Size);
  RenderToTexture.SetTexture(FGLName, GL_TEXTURE_2D);
  if VarianceShadowMaps then
    RenderToTexture.Buffer := tbColor
  else
    RenderToTexture.Buffer := tbDepth;
  RenderToTexture.Stencil := false;
  RenderToTexture.GLContextOpen;
end;

function TGeneratedShadowMapResource.GetRHIDepthTexture: TRHITexture;
begin
  if FRHIDepthTexture <> nil then
    Result := FRHIDepthTexture
  else
    Result := FRHITexture;
end;

procedure TGeneratedShadowMapResource.UnprepareCore;
begin
  FreeAndNil(RenderToTexture);
  FreeAndNil(FRHISampler);
  FreeAndNil(FRHIDepthTexture);
  if FRHITexture <> nil then
  begin
    TextureMemoryProfiler.DeallocateRHI(FRHITexture);
    FreeAndNil(FRHITexture);
  end;

  if FGLName <> GLObjectNone then
  begin
    if VarianceShadowMaps then
      RendererCache.TextureFloat_DecReference(FGLName)
    else
      RendererCache.TextureDepth_DecReference(FGLName);
    FGLName := GLObjectNone;
  end;
end;

function TGeneratedShadowMapResource.Bind(const TextureUnit: Cardinal): boolean;
begin
  if RHIDevice <> nil then
    Exit(FRHITexture <> nil);

  Result := FGLName <> GLObjectNone;
  if not Result then Exit;

  TRenderer.ActiveTexture(TextureUnit);
  glBindTexture(GL_TEXTURE_2D, FGLName);
end;

function TGeneratedShadowMapResource.Enable(const TextureUnit: Cardinal;
  Shader: TShader; const Env: TTextureEnv): boolean;
begin
  Result := Bind(TextureUnit);
  if not Result then Exit;

  if not WarninigDoneCannotEnable then
  begin
    WarninigDoneCannotEnable := true;
    WritelnWarning('Not supported anymore: GeneratedShadowMap placed directly within Appearance.textures of a shadow receiver. Use instead Appearance.receiveShadows or light.shadows to activate shadow maps.');
  end;
end;

procedure TGeneratedShadowMapResource.Update(
  const Render: TRenderFromViewFunction;
  const ProjectionNear, ProjectionFar: Single;
  const Light: TAbstractPunctualLightNode);
var
  NewProjectionMatrix, SavedProjectionMatrix: TMatrix4;
  SavedViewport: TRectangle;
  Camera: TRenderingCamera;
  SavedPolygonOffset: TPolygonOffset;
  CameraRender: TViewVectors;
begin
  if (FGLName = GLObjectNone) and (FRHITexture = nil) then Exit;

  { Render view for shadow map }
  if Light <> nil then
    NewProjectionMatrix := Light.ProjectionMatrix
  else
    NewProjectionMatrix := PerspectiveProjectionMatrixDeg(90, 1.0, ProjectionNear, ProjectionFar);

  Camera := TRenderingCamera.Create;
  try
    if VarianceShadowMaps then
      Camera.Target := rtVarianceShadowMap
    else
      Camera.Target := rtShadowMap;

    if Light <> nil then
      Light.GetView(CameraRender)
    else
      CameraRender := DefaultX3DCameraView;
    Camera.FromViewVectors(CameraRender, NewProjectionMatrix);

    RenderToTexture.RenderBegin;

      if RenderContext <> nil then
      begin
        SavedViewport := RenderContext.Viewport;
        RenderContext.Viewport := Rectangle(0, 0, Size, Size);

        SavedProjectionMatrix := RenderContext.ProjectionMatrix;
        RenderContext.ProjectionMatrix := NewProjectionMatrix;

        { Enable polygon offset for everything (whole scene).

          VarianceShadowMaps notes: can offset be avoided in this case?
          Practice (on "ATI Mobility Radeon HD 4300 Series") shows that
          offset is still needed for VSM. Without offset, VSM has some noise,
          and sunny_street demos have noise on walls. With offset, VSM is perfect. }
        SavedPolygonOffset := RenderContext.PolygonOffset;
        RenderContext.PolygonOffsetEnable(TextureNode.FdScale.Value, TextureNode.FdBias.Value);
      end;
      if (RHIDevice <> nil) and (ActiveCommandList <> nil) then
        ActiveCommandList.SetDepthBias(TextureNode.FdBias.Value, TextureNode.FdScale.Value);

      Render(Camera);

      if RenderContext <> nil then
      begin
        RenderContext.PolygonOffset := SavedPolygonOffset;
        RenderContext.ProjectionMatrix := SavedProjectionMatrix;
        RenderContext.Viewport := SavedViewport;
      end;
      if (RHIDevice <> nil) and (ActiveCommandList <> nil) then
        ActiveCommandList.SetDepthBias(0, 0);

    RenderToTexture.RenderEnd;

  finally FreeAndNil(Camera) end;

  if NeedsMipmaps then
    RenderToTexture.GenerateMipmap;

  if RHIDevice <> nil then
    Inc(FRHIUpdateCount);
end;

function TGeneratedShadowMapResource.GLName: TGLTextureId;
begin
  Result := FGLName;
end;

{ TShaderTextureResource ------------------------------------------------------ }

class function TShaderTextureResource.IsClassForTextureNode(
  ANode: TAbstractTextureNode): boolean;
begin
  Result := ANode is TShaderTextureNode;
end;

procedure TShaderTextureResource.PrepareCore(
  const RenderOptions: TCastleRenderOptions);
begin
  { no need to do anything }
end;

procedure TShaderTextureResource.UnprepareCore;
begin
  { no need to do anything }
end;

function TShaderTextureResource.Bind(const TextureUnit: Cardinal): boolean;
begin
  Result := true;
  { no need to actually do anything }

  { TODO: ShaderTexture should not need to increase the TextureUnit.
    It's not loaded to OpenGL, so it doesn't take the multitexture slot.
    For now we increase it, only to pass texture coords to it. }
end;

function TShaderTextureResource.Enable(const TextureUnit: Cardinal;
  Shader: TShader; const Env: TTextureEnv): boolean;
begin
  Result := Bind(TextureUnit);
  if not Result then Exit;

  Shader.EnableTexture(TextureUnit, MyTextureType, TextureNode, Env);
end;

function TShaderTextureResource.ShaderTextureType(out TextureType: TTextureType): Boolean;
begin
  TextureType := MyTextureType;
  Result := true;
end;

function TShaderTextureResource.TextureNode: TShaderTextureNode;
begin
  Result := TShaderTextureNode(inherited TextureNode);
end;

{$endif read_implementation}
