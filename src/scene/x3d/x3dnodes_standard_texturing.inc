{%MainUnit x3dnodes.pas}
{
  Copyright 2002-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { Base for all nodes which specify texture coordinates. }
  TAbstractTextureCoordinateNode = class(TAbstractGeometricPropertyNode)
  {$I auto_generated_node_helpers/x3dnodes_x3dtexturecoordinatenode.inc}
  end;

  { Base for all nodes which specify a texture coordinate, but not MultiTextureCoordinate. }
  TAbstractSingleTextureCoordinateNode = class(TAbstractTextureCoordinateNode)
  {$I auto_generated_node_helpers/x3dnodes_x3dsingletexturecoordinatenode.inc}
  end;

  TInternalRendererResource = class
  end;

  { Base for all nodes which specify a texture, possibly multi-texture. }
  TAbstractTextureNode = class(TAbstractAppearanceChildNode)
  strict private
    FInternalRendererResource: TInternalRendererResource;
    FContextCloseEventSetup: Boolean;
    procedure SetInternalRendererResource(const Value: TInternalRendererResource);
    procedure GLContextCloseEvent(Sender: TObject);
  strict protected
    { Alpha channel detected from image contents (or children nodes),
      ignoring our FdAlphaChannel field.
      Actually set only when image data is loaded
      (e.g. TAbstractTexture2DNode.IsTextureLoaded in case of TAbstractTexture2DNode
      descendant). }
    function AlphaChannelData: TAlphaChannel; virtual;
  public
    { Internal data stored here and used by the renderer.
      Specific to this TX3DNode.

      This holds a reference count to OpenGL resource in RendererCache,
      freeing this InternalRendererResource decreases the reference count,
      and it is done when destroying this node or when last GL context
      is lost.

      @exclude }
    property InternalRendererResource: TInternalRendererResource
      read FInternalRendererResource write FInternalRendererResource;

    { Free InternalRendererResource, if any. }
    procedure InternalRendererResourceFree;

    destructor Destroy; override;

    { Short description how texture is defined, is it inline or loaded
      from URL, is it video of simple image texture.
      "none" if it's not defined at all.

      Calling this @italic(may not) cause automatically loading
      the texture data (for exampe, from file in case of TAbstractTexture2DNode).
      So it cannot describe the actually loaded data.

      In this class, simply returns X3DType.
      Override to say something more descriptive. }
    function TextureDescription: string; virtual;

    { Alpha channel of the loaded texture data.
      Looks at loaded texture data, and at alphaChannel field
      (see https://castle-engine.io/x3d_extensions.php#section_ext_alpha_channel_detection ).
      In case of MultiTexture node, looks at children. }
    function AlphaChannelFinal: TAlphaChannel; virtual;
  {$I auto_generated_node_helpers/x3dnodes_x3dtexturenode.inc}
  end;

  { Base for all nodes which specify a texture, but not multi-texture. }
  TAbstractSingleTextureNode = class(TAbstractTextureNode)
  public
    function AlphaChannelFinal: TAlphaChannel; override;

  {$I auto_generated_node_helpers/x3dnodes_x3dsingletexturenode.inc}
  end;

  { 2D texture. }
  TAbstractTexture2DNode = class(TAbstractSingleTextureNode)
  private
    { Together we call FTextureImage, FTextureComposite,
      FTextureVideo, FAlphaChannelData as "texture data". }

    { FTextureImage is <> nil if texture is currently loaded (IsTextureLoaded)
      and it was loaded to an image (not video).

      Note that this may still have zero size (IsEmpty = @true),
      IsTextureImage checks this also. }
    FTextureImage: TEncodedImage;

    { Only if FTextureImage is <> nil, then FTextureComposite may also be <> nil,
      it this image is part of Composite file. }
    FTextureComposite: TCompositeImage;

    { Analogous to FTextureImage, this is the loaded video file.
      Assigned here, should always have TVideo.Loaded = @true. }
    FTextureVideo: TVideo;

    FAlphaChannelData: TAlphaChannel;

    { Non-nil only if FTextureImage, FTextureComposite or FTextueVideo
      should be freed using
      UsedCache (TextureImage_DecReference or Video_DecReference).
      Also loaded FTextureVideo should always have it's own Cache property set
      to this. }
    UsedCache: TTexturesVideosCache;

    FIsTextureLoaded: boolean;
    procedure SetIsTextureLoaded(Value: boolean);
    procedure FreeAndNilTextureData;
  strict protected
    FTextureUsedFullUrl: string;

    { Loads texture data (image or video file).

      It should set either FTextureImage or FTextureVideo to something non-nil
      (don't care here about the previous value of these fields --- it's for
      sure @nil). If FTextureImage is set, you can also set FTextureComposite.
      If you leave them as @nil, this means that loading failed
      (and WritelnWarning didn't cause an exception).
      You should also set FAlphaChannelData.

      You do not care in this method about things like
      IsImageLoaded --- this method should just always,
      unconditionally, make everything it can do to load texture data from
      file(s).

      You can use WritelnWarning inside,
      so we're prepared that this may even exit with exception
      (since WritelnWarning can raise exception).

      If you set FTextureImage, you have to set it to something
      returned by LoadTextureImage. See TextureImage docs.

      Set WasCacheUsed here. @true means you loaded the data using Cache.
      For FTextureVideo, it's Cache property should also be set to
      our Cache, this happens automatically in Video_IncReference.

      Also, set FTextureUsedFullUrl here.

      In this class, this simply produces WritelnWarning with
      "not implemented" message and returns @nil. It's not declared
      as abstract, because there may be classes descending from this,
      and we want to at least be able to parse them
      and then ignore (while not overriden abstract method would cause
      simple crashes). }
    procedure LoadTextureData(out WasCacheUsed: boolean); virtual;

    function AlphaChannelData: TAlphaChannel; override;
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    destructor Destroy; override;

    { TextureImage, TextureComposite and TextureVideo contain actual texture data.
      TextureImage may come from inline VRML texture or could be loaded
      from file (including from some URL), this doesn't concern you here.

      Calls to TextureImage, TextureVideo, IsTextureImage, IsTextureVideo,
      TextureComposite
      will automatically load the data, so in simple situations you really
      don't need to do anything. Just check and use them when you want,
      and things will just work. See IsTextureLoaded for more control
      about loading / unloading.

      Note that either one of TextureImage or TextureVideo may be @nil,
      if the other one is loaded. Or when loading failed
      (warning will be reported by WritelnWarning).
      IsTextureImage checks that TextureImage is non-nil (so it's successfully
      loaded) and additionally that texture size is not zero.
      Similar for IsTextureVideo.

      Note that when image failed to load, or URL was just empty,
      we can have IsTextureLoaded = @true but both IsTextureImage = @false
      and IsTextureVideo = @false.

      TextureImage may have any class allowed by LoadTextureImage.

      @groupBegin }
    function TextureImage: TEncodedImage;
    function IsTextureImage: boolean;
    function TextureComposite: TCompositeImage;
    function TextureVideo: TVideo;
    function IsTextureVideo: boolean;
    { @groupEnd }

    { Is the texture data already loaded.
      Since the texture will be loaded automatically, you're usually
      not interested in this property. You can read it to e.g. predict
      if next TextureImage / TextureVideo call may take a long time.
      (You know that if IsTextureLoaded = @true then TextureImage
      just returns ready image instantly).

      You can also set IsTextureLoaded.
      Setting to @true means that you request the texture to be loaded @italic(now),
      if it's not loaded already. Setting to @false may be useful if you want
      to release resources (e.g. when you want to keep TTextureNode instance
      loaded but you know that you will not need
      TextureImage / TextureComposite / TextureVideo anymore).
      You can also set it to @false and then back to @true if you want to
      request reloading the texture from URL (e.g. if you suspect that
      the URL contents changed).

      Note that IsTextureLoaded is set to @true, even if actual loading
      failed. You still have to check afterwards IsTextureImage and
      IsTextureVideo to know if loading was actually successful.
      This is deliberate --- it means that each call to TextureImage etc.
      will not unnecessarily read the disk (or even connect to internet)
      when the file does not exist. Also, the loading errors reported
      by WritelnWarning will not be repeated --- they will
      occur only once, when IsTextureLoaded changes from @false to @true. }
    property IsTextureLoaded: boolean
      read FIsTextureLoaded write SetIsTextureLoaded;

    { Once the texture data (image or video) is loaded,
      this is set to the URL that was used to load,
      or '' if no URL was used. "No URL was used" may mean that
      no URL was valid, or inlined image was used.

      This is always a full, expanded (i.e. not relative) URL.

      In case of data: URLs, this doesn't contain actual data (it would
      be too long then, and TextureUsedFullUrl is mainly for showing to the
      user), it's cutted. }
    property TextureUsedFullUrl: string read FTextureUsedFullUrl;

  {$I auto_generated_node_helpers/x3dnodes_x3dtexture2dnode.inc}
  end;

  TAbstractX3DTexture2DNode = TAbstractTexture2DNode deprecated 'use TAbstractTexture2DNode';

  { Base for all nodes which specify a transformation of texture coordinates. }
  TAbstractTextureTransformNode = class(TAbstractAppearanceChildNode)
  public
    function TransformMatrix: TMatrix4; virtual; abstract;
  {$I auto_generated_node_helpers/x3dnodes_x3dtexturetransformnode.inc}
  end;

  { Base for all nodes which specify a texture coordinate transform, but not MultiTextureTransform. }
  TAbstractSingleTextureTransformNode = class(TAbstractTextureTransformNode)
  {$I auto_generated_node_helpers/x3dnodes_x3dsingletexturetransformnode.inc}
  end;

  { Texture image loaded from a file. }
  TImageTextureNode = class(TAbstractTexture2DNode)
  strict private
    function LoadOptions: TLoadImageOptions;
  strict protected
    procedure LoadTextureData(out WasCacheUsed: boolean); override;
  public
    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;

    function TextureDescription: string; override;

    { Load image contents from a stream.

      If UpdateUrl is not empty, then this also sets the @link(FdUrl) field
      to the indicated value (without actually loading the image from this URL).
      This is useful if you later may serialize this TImageTextureNode:
      thus URL will represent this image in the output.
      Note: aside from normal URLs (file, http and such protocols),
      you can always set this URL to a "data URI" that would encode the image contents.
      See CGE example tools/to-data-uri/to-data-uri.lpr for an example. }
    procedure LoadFromStream(const Stream: TStream; const MimeType: String; const UpdateUrl: String);

    { Load image contents from a ready TEncodedImage instance.

      If TakeImageOwnership = @true then we take given Image reference,
      and the lifetime of this Image object will be further managed by this TImageTextureNode
      class (so you should no longer free it yourself).
      If TakeImageOwnership = @false then we only copy Image contents
      (this is slower, but it means that ownership and freeing of given
      Image instance remains on your side).

      If UpdateUrl is not empty, then this also sets the @link(FdUrl) field
      to the indicated value (without actually loading the image from this URL).
      This is useful if you later may serialize this TImageTextureNode:
      thus URL will represent this image in the output.
      Note: aside from normal URLs (file, http and such protocols),
      you can always set this URL to a "data URI" that would encode the image contents.
      See CGE example tools/to-data-uri/to-data-uri.lpr for an example. }
    procedure LoadFromImage(const Image: TEncodedImage;
      const TakeImageOwnership: Boolean; const UpdateUrl: String);

    {$I auto_generated_node_helpers/x3dnodes_imagetexture.inc}
  end;

  { Movie file, that can be played and displayed as a texture. }
  TMovieTextureNode = class(TAbstractTexture2DNode)
  strict private
    FDuration: TFloatTime;
    FTimeFunctionality: TTimeDependentFunctionality;
    { Final length (in seconds) of this movie, adjusted by Speed.
      See @link(TTimeDependentFunctionality.CycleInterval),
      this must return the relevant value. }
    function GetCycleInterval: TFloatTime;
    function IgnoreWhenActive(const ChangingField: TX3DField; const Event: TX3DEvent;
      const Value: TX3DField; const Time: TX3DTime): Boolean;
    function IgnoreStopTime(const ChangingField: TX3DField; const Event: TX3DEvent;
      const Value: TX3DField; const Time: TX3DTime): Boolean;
  strict protected
    procedure LoadTextureData(out WasCacheUsed: boolean); override;
  public
    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;
    function TextureDescription: string; override;

    { Duration of this movie, in seconds.
      Duration is initialized from loaded video length (default is -1).
      So it's automatically initialized when you call IsTextureVideo,
      TextureVideo methods.

      This is not multiplied by @link(Speed).
      Use @code(MyMovieTextureNode.TimeFunctionality.CycleInterval)
      to get this duration multiplied by Abs(Speed).

      This duration is not reset when video is freed (like when you set
      IsTextureLoaded to @false, maybe implicitly by calling
      TCastleSceneCore.FreeResources with frTextureDataInNodes).
      This way this is available even you freed the texture video data to
      save memory. }
    property Duration: TFloatTime read FDuration;

  {$I auto_generated_node_helpers/x3dnodes_movietexture.inc}
  end;

  { Application of several individual textures on top of each other,
    used instead of a single texture when desired. }
  TMultiTextureNode = class(TAbstractTextureNode)
  public
    function AlphaChannelData: TAlphaChannel; override;
  {$I auto_generated_node_helpers/x3dnodes_multitexture.inc}
  end;

  { Multiple texture coordinates per vertex,
    to be used with multi-texturing by @link(TMultiTextureNode). }
  TMultiTextureCoordinateNode = class(TAbstractTextureCoordinateNode)
  {$I auto_generated_node_helpers/x3dnodes_multitexturecoordinate.inc}
  end;

  { Multiple texture transforms,
    to be used with multi-texturing by @link(TMultiTextureNode). }
  TMultiTextureTransformNode = class(TAbstractTextureTransformNode)
  public
    { For MultiTextureTransform, this always raises an internal error.
      Reason: you cannot get single texture transform matrix from
      MultiTextureTransform.

      @raises(EInternalError Always, since this method has no sense
        for MultiTextureTransform.) }
    function TransformMatrix: TMatrix4; override;

  {$I auto_generated_node_helpers/x3dnodes_multitexturetransform.inc}
  end;

  { Texture specified as an explicit array of pixel values (see @link(FdImage) field).

    This is useful when authoring X3D, as you can place texture contents directly
    inside the X3D file (without the need for any external file).

    PixelTexture is also comfortable when using Object Pascal to construct
    the X3D graph. In this case, it allows to provide an image as a ready
    TCastleImage instance, by changing the value of the
    @code(TPixelTextrueNode.FdImage.Value).

    An alternative to PixelTexture is to use "data URI".
    With "data URI", you can embed any file contents inside an URL.
    This way, you can use @link(TImageTextureNode ImageTexture),
    and inside @code(ImageTexture.url) field
    you can place an embedded image contents (instead of a normal URL or filename).
    To convert your image file to a "data URI", you can our tool "to_data_uri".
    The image contents stay compressed this way (although expressed in base64),
    so it may be more compact than PixelTexture.
  }
  TPixelTextureNode = class(TAbstractTexture2DNode)
  strict protected
    procedure LoadTextureData(out WasCacheUsed: boolean); override;
  public
    function TextureDescription: string; override;
  {$I auto_generated_node_helpers/x3dnodes_pixeltexture.inc}
  end;

  { 2D texture coordinates used by vertex-based geometry nodes. }
  TTextureCoordinateNode = class(TAbstractSingleTextureCoordinateNode)
  {$I auto_generated_node_helpers/x3dnodes_texturecoordinate.inc}
  end;

  { Automatic generation of texture coordinates. }
  TTextureCoordinateGeneratorNode = class(TAbstractSingleTextureCoordinateNode)
  {$I auto_generated_node_helpers/x3dnodes_texturecoordinategenerator.inc}
  end;

  { Boundary modes for texture coordinates.
    See @link(TTexturePropertiesNode.BoundaryModeS),
    @link(TTexturePropertiesNode.BoundaryModeT),
    @link(TTexturePropertiesNode.BoundaryModeR). }
  TBoundaryMode = (
    bmClamp, //< Clamp texture coordinates to [0..1]. Equivalent to bmClampToEdge.
    bmClampToEdge, //< Clamp texture coordinates to [0..1].
    bmClampToBoundaryUnsupported, //< Clamp texture coordinates to [0..1]. Equivalent to bmClampToEdge. See https://castle-engine.io/x3d_implementation_texturing.php.
    bmMirroredRepeat, //< Repeat the texture, switching between original and flipped texture.
    bmRepeat //< Repeat the texture.
  );

  { Adjust the texture application properties. }
  TTexturePropertiesNode = class(TAbstractNode)
  strict private
    function GetMinificationFilter: TAutoMinificationFilter;
    procedure SetMinificationFilter(const Value: TAutoMinificationFilter);
    function GetMagnificationFilter: TAutoMagnificationFilter;
    procedure SetMagnificationFilter(const Value: TAutoMagnificationFilter);
  public
    property MagnificationFilter: TAutoMagnificationFilter
      read GetMagnificationFilter write SetMagnificationFilter;

    property MinificationFilter: TAutoMinificationFilter
      read GetMinificationFilter write SetMinificationFilter;

    {$I auto_generated_node_helpers/x3dnodes_textureproperties.inc}
  end;

  { 2D transformation that can be applied to texture coordinates (e.g. from
    @link(TTextureCoordinateNode). }
  TTextureTransformNode = class(TAbstractSingleTextureTransformNode)
  public
    function TransformMatrix: TMatrix4; override;
  {$I auto_generated_node_helpers/x3dnodes_texturetransform.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TAbstractTextureNode ------------------------------------------------------- }

procedure TAbstractTextureNode.SetInternalRendererResource(const Value: TInternalRendererResource);
begin
  if FInternalRendererResource <> Value then
  begin
    FInternalRendererResource := Value;
    if not FContextCloseEventSetup then
    begin
      FContextCloseEventSetup := true;
      ApplicationProperties.OnGLContextCloseObject.Add(
        {$ifdef FPC}@{$endif} GLContextCloseEvent);
    end;
  end;
end;

procedure TAbstractTextureNode.GLContextCloseEvent(Sender: TObject);
begin
  InternalRendererResourceFree;
end;

procedure TAbstractTextureNode.InternalRendererResourceFree;
begin
  FreeAndNil(FInternalRendererResource);
end;

destructor TAbstractTextureNode.Destroy;
begin
  GLContextCloseEvent(nil);
  if FContextCloseEventSetup then
    ApplicationProperties.OnGLContextCloseObject.Remove(
      {$ifdef FPC}@{$endif} GLContextCloseEvent);
  inherited;
end;

function TAbstractTextureNode.AlphaChannelData: TAlphaChannel;
begin
  Result := acNone;
end;

function TAbstractTextureNode.AlphaChannelFinal: TAlphaChannel;
begin
  Result := AlphaChannelData;
end;

function TAbstractTextureNode.TextureDescription: string;
begin
  Result := X3DType;
end;

{ TAbstractSingleTextureNode -------------------------------------------------------- }

function TAbstractSingleTextureNode.AlphaChannelFinal: TAlphaChannel;
var
  Res: TAutoAlphaChannel;
begin
  Res := AlphaChannel;
  if Res = acAuto then
    Result := AlphaChannelData
  else
    Result := Res;
end;

{ TAbstractTexture2DNode ----------------------------------------------------------- }

constructor TAbstractTexture2DNode.Create(const AX3DName: string = ''; const ABaseUrl: string = '');
begin
  inherited;
  UsedCache := nil;
  FIsTextureLoaded := false;
end;

destructor TAbstractTexture2DNode.Destroy;
begin
  FreeAndNilTextureData;
  inherited;
end;

procedure TAbstractTexture2DNode.FreeAndNilTextureData;
begin
  if FTextureImage <> nil then
  begin
    if UsedCache <> nil then
    begin
      UsedCache.TextureImage_DecReference(FTextureImage, FTextureComposite);
      UsedCache := nil;
    end else
    begin
      FreeAndNil(FTextureImage);
      FreeAndNil(FTextureComposite);
    end;
  end;

  if FTextureVideo <> nil then
  begin
    if UsedCache <> nil then
    begin
      UsedCache.Video_DecReference(FTextureVideo);
      UsedCache := nil;
    end else
      FreeAndNil(FTextureVideo);
  end;
end;

function TAbstractTexture2DNode.TextureImage: TEncodedImage;
begin
  { Setting IsTextureLoaded property will initialize FTextureImage. }
  IsTextureLoaded := true;

  Result := FTextureImage;
end;

function TAbstractTexture2DNode.IsTextureImage: boolean;
begin
  Result := (TextureImage <> nil) and (not TextureImage.IsEmpty);
end;

function TAbstractTexture2DNode.TextureComposite: TCompositeImage;
begin
  { Setting IsTextureLoaded property will initialize
    FTextureImage, FTextureComposite. }
  IsTextureLoaded := true;

  Result := FTextureComposite;
end;

function TAbstractTexture2DNode.TextureVideo: TVideo;
begin
  { Setting IsTextureLoaded property will initialize FTextureVideo. }
  IsTextureLoaded := true;

  Result := FTextureVideo;
end;

function TAbstractTexture2DNode.IsTextureVideo: boolean;
begin
  Result := (TextureVideo <> nil) and
    (TextureVideo.Width <> 0) and
    (TextureVideo.Height <> 0);
end;

procedure TAbstractTexture2DNode.SetIsTextureLoaded(Value: boolean);

  procedure DoLoadTexture;
  var
    WasCacheUsed: boolean;
  begin
    FreeAndNilTextureData;

    LoadTextureData(WasCacheUsed);
    if WasCacheUsed then
      UsedCache := X3DCache;
  end;

begin
  if Value <> FIsTextureLoaded then
  begin
    if Value then
    begin
      { actually load the texture }
      DoLoadTexture;
    end else
    begin
      { unload the texture }
      FreeAndNilTextureData;
    end;

    FIsTextureLoaded := Value;
  end;
end;

procedure TAbstractTexture2DNode.LoadTextureData(out WasCacheUsed: boolean);
begin
  WasCacheUsed := false;
  FTextureUsedFullUrl := '';

  WritelnWarning('VRML/X3D', Format('Loading textures from "%s" node not implemented', [X3DType]));
end;

function TAbstractTexture2DNode.AlphaChannelData: TAlphaChannel;
begin
  Result := FAlphaChannelData;
end;

{ TImageTextureNode ---------------------------------------------------------- }

constructor TImageTextureNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  AddFunctionality(TUrlFunctionality.Create(Self));
end;

function TImageTextureNode.LoadOptions: TLoadImageOptions;
begin
  Result := [];
  if FlipVertically then
    Include(Result, liFlipVertically);
end;

procedure TImageTextureNode.LoadTextureData(out WasCacheUsed: boolean);
var
  I: Integer;
  FullUrl, ErrorMessage: string;
  Success: boolean;
begin
  WasCacheUsed := false;
  FTextureUsedFullUrl := '';
  Success := false;
  ErrorMessage := '';

  for I := 0 to FdUrl.Count - 1 do
    if FdUrl.Items[I] = '' then
    begin
      { Special error message in this case, otherwise empty URL
        would be expanded by PathFromBaseUrl to directory
        and produce unclear error message. }
      ErrorMessage := ErrorMessage + NL + '  Empty URL ignored';
    end else
    begin
      FullUrl := PathFromBaseUrl(FdUrl.Items[I]);
      try
        FTextureImage := X3DCache.TextureImage_IncReference(
          FullUrl, FTextureComposite, FAlphaChannelData, LoadOptions);
        WasCacheUsed := true;
        if WarnAboutAbsoluteFilenames and
           AbsoluteFileURI(FdUrl.Items[I]) and
           { AbsoluteFileURI mistakes Blender relative paths '//xxx' as absolute,
             so eliminate them below. }
           not BlenderRelativePath(FdUrl.Items[I]) then
          WritelnWarning('VRML/X3D', Format('Loaded data from an absolute filename "%s", this makes the data possibly unportable (it will probably not work on other systems/locations). Always use relative paths.',
            [FdUrl.Items[I]]));
        FTextureUsedFullUrl := URIDisplay(FullUrl);
        Success := true;
        Break;
      except
        on E: Exception do
          { Silence the Exception, continue trying to load more textures. }
          ErrorMessage := ErrorMessage + NL + '  ' + Format(SLoadError,
            [E.ClassName, 'texture', URIDisplay(FullUrl), E.Message]);
      end;
    end;

  if ErrorMessage <> '' then
  begin
    if Success then
      WritelnLog('Texture', 'Some of the texture URLs failed to load, although we found one URL that succeeded:' +
        ErrorMessage)
    else
      WritelnWarning('Texture', 'Cannot load the texture:' + ErrorMessage);
  end;
  { Note that when Success = false but ErrorMessage = '', then we're quiet
    about it. This happens when Urls.Count = 0. }
end;

function TImageTextureNode.TextureDescription: string;
begin
  if TextureUsedFullUrl <> '' then
    Result := 'image from file "' + TextureUsedFullUrl + '"' else
    Result := 'none';
end;

procedure TImageTextureNode.LoadFromStream(const Stream: TStream; const MimeType: String; const UpdateUrl: String);
var
  Image: TEncodedImage;
begin
  Image := LoadEncodedImage(Stream, MimeType, [], LoadOptions);
  LoadFromImage(Image, true, UpdateUrl);
end;

procedure TImageTextureNode.LoadFromImage(const Image: TEncodedImage;
  const TakeImageOwnership: Boolean; const UpdateUrl: String);
begin
  FreeAndNilTextureData;

  if TakeImageOwnership then
    FTextureImage := Image
  else
    FTextureImage := Image.CreateCopy;

  { update things that need to be updated when loading image }
  FAlphaChannelData := FTextureImage.AlphaChannel;
  FIsTextureLoaded := true;
  UsedCache := nil;

  if UpdateUrl <> '' then
  begin
    FTextureUsedFullUrl := UpdateUrl;
    { Deliberately change FdUrl value in low-level fashion,
      without triggering Scene.InternalChangedField,
      because we never want to actually load contents of UpdateUrl. }
    FdUrl.Items.Clear;
    FdUrl.Items.Add(UpdateUrl);
  end;

  if Scene <> nil then
    Scene.InternalChangedField(FdUrl);
end;

{ TMovieTextureNode ---------------------------------------------------------- }

constructor TMovieTextureNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  FdStartTime.OnInputIgnore := {$ifdef FPC}@{$endif} IgnoreWhenActive;
  FdStopTime.OnInputIgnore := {$ifdef FPC}@{$endif} IgnoreStopTime;
  FDuration := -1;

  AddFunctionality(TUrlFunctionality.Create(Self));

  FTimeFunctionality := TTimeDependentFunctionality.Create(Self);
  FTimeFunctionality.Fdloop := FdLoop;
  FTimeFunctionality.FdpauseTime := FdPauseTime;
  FTimeFunctionality.FdresumeTime := FdResumeTime;
  FTimeFunctionality.FdstartTime := FdStartTime;
  FTimeFunctionality.FdstopTime := FdStopTime;
  FTimeFunctionality.EventisActive:= EventisActive;
  FTimeFunctionality.EventisPaused := EventisPaused;
  FTimeFunctionality.EventelapsedTime := EventelapsedTime;
  FTimeFunctionality.OnCycleInterval := {$ifdef FPC}@{$endif} GetCycleInterval;

  AddFunctionality(FTimeFunctionality);
end;

function TMovieTextureNode.IgnoreWhenActive(
  const ChangingField: TX3DField; const Event: TX3DEvent;
  const Value: TX3DField; const Time: TX3DTime): Boolean;
begin
  Result := {(InternalNeverIgnore = 0) and} FTimeFunctionality.IsActive;
end;

function TMovieTextureNode.IgnoreStopTime(
  const ChangingField: TX3DField; const Event: TX3DEvent;
  const Value: TX3DField; const Time: TX3DTime): Boolean;
begin
  Result := {(InternalNeverIgnore = 0) and}
    FTimeFunctionality.IsActive and
    ( (Value as TSFTime).Value <= FdStartTime.Value );
end;

procedure TMovieTextureNode.LoadTextureData(out WasCacheUsed: boolean);

  function LoadOptions: TLoadImageOptions;
  begin
    Result := [];
    if FlipVertically then
      Include(Result, liFlipVertically);
  end;

var
  I: Integer;
  FullUrl: string;
begin
  WasCacheUsed := true;
  FTextureUsedFullUrl := '';

  for I := 0 to FdUrl.Count - 1 do
  begin
    FullUrl := PathFromBaseUrl(FdUrl.Items[I]);
    try
      FTextureVideo := X3DCache.Video_IncReference(
        FullUrl, FAlphaChannelData, LoadOptions);

      { if loading succeded, set WasCacheUsed and others and break. }
      WasCacheUsed := true;
      FTextureUsedFullUrl := FullUrl;
      FDuration := FTextureVideo.TimeDuration;
      if Scene <> nil then
        EventDuration_Changed.Send(FDuration, Scene.NextEventTime);
      Break;
    except
      on E: Exception do
        { Remember that WritelnWarning *may* raise an exception. }
        WritelnWarning('Video', Format(SLoadError,
          [E.ClassName, 'video', URIDisplay(FullUrl), E.Message]));
    end;
  end;
end;

function TMovieTextureNode.GetCycleInterval: TFloatTime;
var
  Sp: Single;
begin
  Sp := Speed;
  if Sp <> 0 then
    Result := FDuration / Abs(Sp)
  else
    Result := 0;
end;

function TMovieTextureNode.TextureDescription: string;
begin
  if TextureUsedFullUrl <> '' then
    Result := 'video from file "' + TextureUsedFullUrl + '"' else
    Result := 'none';
end;

function TMultiTextureNode.AlphaChannelData: TAlphaChannel;
var
  ChildTex: TX3DNode;
  I: Integer;
begin
  Result := acNone;
  for I := 0 to FdTexture.Count - 1 do
  begin
    ChildTex := FdTexture[I];
    if (ChildTex <> nil) and
       (ChildTex is TAbstractSingleTextureNode) then
      AlphaMaxVar(Result, TAbstractSingleTextureNode(ChildTex).AlphaChannelFinal);
  end;
end;

function TMultiTextureTransformNode.TransformMatrix: TMatrix4;
begin
  raise EInternalError.Create('You cannot get single TransformMatrix from MultiTextureTransform node');
  Result := TMatrix4.Identity; { avoid warnings that result not set }
end;

{ TPixelTextureNode ---------------------------------------------------------- }

procedure TPixelTextureNode.LoadTextureData(out WasCacheUsed: boolean);
begin
  WasCacheUsed := false;

  if not FdImage.Value.IsEmpty then
  begin
    FTextureImage := FdImage.Value.MakeCopy;
    FAlphaChannelData := FdImage.Value.AlphaChannel;
  end;
end;

function TPixelTextureNode.TextureDescription: string;
begin
  if not FdImage.Value.IsEmpty then
    result := Format('inlined image (width = %d; height = %d; with alpha = %s)',
      [ FdImage.Value.Width,
        FdImage.Value.Height,
        BoolToStr(FdImage.Value.HasAlpha, true) ]) else
    result := 'none';
end;

const
  BoundaryModeNames: array [TBoundaryMode] of string = (
    'CLAMP',
    'CLAMP_TO_EDGE',
    'CLAMP_TO_BOUNDARY',
    'MIRRORED_REPEAT',
    'REPEAT'
  );

function TTexturePropertiesNode.GetMinificationFilter: TAutoMinificationFilter;
var
  S: string;
begin
  S := UpperCase(FdMinificationFilter.Value);

  if S = 'AVG_PIXEL' then Result := minLinear else
  if S = 'AVG_PIXEL_AVG_MIPMAP' then Result := minLinearMipmapLinear else
  if S = 'AVG_PIXEL_NEAREST_MIPMAP' then Result := minLinearMipmapNearest else
  if S = 'NEAREST_PIXEL_AVG_MIPMAP' then Result := minNearestMipmapLinear else
  if S = 'NEAREST_PIXEL_NEAREST_MIPMAP' then Result := minNearestMipmapNearest else
  if S = 'NEAREST_PIXEL' then Result := minNearest else

  if S = 'DEFAULT' then Result := minDefault else
  if S = 'FASTEST' then Result := minFastest else
  if S = 'NICEST' then Result := minNicest else

  if S = 'NEAREST' then
  begin
    WritelnWarning('VRML/X3D', Format('"%s" is not allowed texture minification, this is an InstantReality extension, please fix to "NEAREST_PIXEL"', [S]));
    Result := minNearest;
  end else

  if S = 'LINEAR' then
  begin
    WritelnWarning('VRML/X3D', Format('"%s" is not allowed texture minification, this is an InstantReality extension, please fix to "AVG_PIXEL"', [S]));
    Result := minLinear;
  end else

  begin
    Result := minDefault;
    WritelnWarning('VRML/X3D', Format('Unknown texture minification filter "%s"', [S]));
  end;
end;

procedure TTexturePropertiesNode.SetMinificationFilter(const Value: TAutoMinificationFilter);
const
  Names: array [TAutoMinificationFilter] of string = (
    'NEAREST_PIXEL',
    'AVG_PIXEL',
    'NEAREST_PIXEL_NEAREST_MIPMAP',
    'NEAREST_PIXEL_AVG_MIPMAP',
    'AVG_PIXEL_NEAREST_MIPMAP',
    'AVG_PIXEL_AVG_MIPMAP',
    'DEFAULT',
    'FASTEST',
    'NICEST'
  );
begin
  FdMinificationFilter.Send(Names[Value]);
end;

function TTexturePropertiesNode.GetMagnificationFilter: TAutoMagnificationFilter;
var
  S: string;
begin
  S := UpperCase(FdMagnificationFilter.Value);

  if S = 'AVG_PIXEL' then Result := magLinear else
  if S = 'NEAREST_PIXEL' then Result := magNearest else

  if S = 'DEFAULT' then Result := magDefault else
  if S = 'FASTEST' then Result := magFastest else
  if S = 'NICEST' then Result := magNicest else

  if S = 'NEAREST' then
  begin
    WritelnWarning('VRML/X3D', Format('"%s" is not allowed texture magnification, this is an InstantReality extension, please fix to "NEAREST_PIXEL"', [S]));
    Result := magNearest;
  end else

  if S = 'LINEAR' then
  begin
    WritelnWarning('VRML/X3D', Format('"%s" is not allowed texture magnification, this is an InstantReality extension, please fix to "AVG_PIXEL"', [S]));
    Result := magLinear;
  end else

  begin
    Result := magDefault;
    WritelnWarning('VRML/X3D', Format('Unknown texture minification filter "%s"', [S]));
  end;
end;

procedure TTexturePropertiesNode.SetMagnificationFilter(const Value: TAutoMagnificationFilter);
const
  Names: array [TAutoMagnificationFilter] of string = (
    'NEAREST_PIXEL',
    'AVG_PIXEL',
    'DEFAULT',
    'FASTEST',
    'NICEST'
  );
begin
  FdMagnificationFilter.Send(Names[Value]);
end;

{ TTextureTransformNode ------------------------------------------------------ }

function TTextureTransformNode.TransformMatrix: TMatrix4;
begin
  { Yes, VRML 2 and X3D specs say in effect that the order of operations
    is *reversed* with regards to VRML 1 spec.
          VRML 1 spec says it's (in order) scale, rotation, translation.
    VRML 2 / X3D spec say  it's (in order) translation, rotation, scale.

    Moreover, VRML 2 / X3D spec give explicit formula which we follow:
      Tc' = -C * S * R * C * T * Tc

    To test that this order matters, check e.g. VRML NIST test suite,
    "Appearance -> TextureTransform" tests 15, 16 (ImageTexture),
    31, 32 (MovieTexture) and 47, 48 (PixelTexture).
    Current implementation passes it (results match images).
    Also, results match Xj3d and OpenVRML results.

    Other links:

    - At least looking at source code, some old version of
      FreeWRL had it reversed (matching VRML 1):
      [http://search.cpan.org/src/LUKKA/FreeWRL-0.14/VRMLFunc.xs]
      function TextureTransform_Rend.

    - Other: [http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4320634] }

  Result :=
    TranslationMatrix(Vector3( -FdCenter.Value[0], -FdCenter.Value[1], 0 )) *
    ScalingMatrix(Vector3( FdScale.Value[0], FdScale.Value[1], 1 )) *
    RotationMatrixRad(FdRotation.Value, Vector3(0, 0, 1)) *
    TranslationMatrix( Vector3(
      FdTranslation.Value[0] + FdCenter.Value[0],
      FdTranslation.Value[1] + FdCenter.Value[1], 0));
end;

procedure RegisterTexturingNodes;
begin
  NodesManager.RegisterNodeClasses([
    TImageTextureNode,
    TMovieTextureNode,
    TMultiTextureNode,
    TMultiTextureCoordinateNode,
    TMultiTextureTransformNode,
    TPixelTextureNode,
    TTextureCoordinateNode,
    TTextureCoordinateGeneratorNode,
    TTexturePropertiesNode,
    TTextureTransformNode
  ]);
end;

{$endif read_implementation}
