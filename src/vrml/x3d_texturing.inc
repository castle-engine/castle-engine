{
  Copyright 2002-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { }
  TNodeX3DTextureCoordinateNode = class(TNodeX3DGeometricPropertyNode)
  public
    procedure CreateNode; override;
  end;

  TNodeX3DTextureNode = class(TNodeX3DAppearanceChildNode)
  public
    procedure CreateNode; override;

    { Short description how texture is defined, is it inline or loaded
      from URL, is it video of simple image texture.
      "none" if it's not defined at all.

      Calling this @italic(may not) cause automatically loading
      the texture data (for exampe, from file in case of TVRML2DTextureNode).
      So it cannot describe the actually loaded data.

      In this class, simply returns NodeTypeName.
      Override to say something more descriptive. }
    function TextureDescription: string; virtual;

    private FFdEffects: TMFNode;
    public property FdEffects: TMFNode read FFdEffects;
  end;

  TNodeTextureProperties = class;

  { VRML/X3D texture that is 2D and is loaded (from file or some other stream).

    For X3D, this descends from X3DTextureNode and is an ancestor
    for X3DTexture2DNode, so X3D hierarchy is nicely preserved. }
  TVRML2DTextureNode = class(TNodeX3DTextureNode)
  private
    { Together we call FTextureImage, FTextureDDS,
      FTextureVideo as "texture data". }

    { FTextureImage is <> nil if texture is currently loaded (IsTextureLoaded)
      and it was loaded to an image (not video).

      Note that this may still have zero size (IsNull = @true),
      IsTextureImage checks this also. }
    FTextureImage: TEncodedImage;

    { Only if FTextureImage is <> nil, then FTextureDDS may also be <> nil,
      it this image is part of DDS file. }
    FTextureDDS: TDDSImage;

    { Analogous to FTextureImage, this is the loaded video file.
      Assigned here, should always have TVideo.Loaded = @true. }
    FTextureVideo: TVideo;

    { Non-nil only if FTextureImage, FTextureDDS or FTextueVideo
      should be freed using
      UsedCache (TextureImage_DecReference or Video_DecReference).
      Also loaded FTextureVideo should always have it's own Cache property set
      to this. }
    UsedCache: TTexturesImagesVideosCache;

    FIsTextureLoaded: boolean;
    procedure SetIsTextureLoaded(Value: boolean);
    procedure FreeAndNilTextureData;
  protected
    FTextureUsedFullUrl: string;

    { This loads texture data, actual image or video file.

      It should set either FTextureImage or FTextureVideo to something non-nil
      (don't care here about the previous value of these fields --- it's for
      sure @nil). If FTextureImage is set, you can also set FTextureDDS.
      If you leave them as @nil, this means that loading failed
      (and VRMLWarning or DataWarning don't cause an exception,
      so we can ignore this).

      You do not care in this method about things like
      IsImageLoaded --- this method should just always,
      unconditionally, make everything it can do to load texture data from
      file(s).

      You can use VRMLWarning or DataWarning inside,
      so we're prepared that this may even exit with exception
      (since VRML/DataWarning can raise exception).

      If you set FTextureImage, you have to set it to something
      returned by LoadTextureImage. See TextureImage docs.

      Set WasCacheUsed here. @true means you loaded the data using Cache.
      For FTextureVideo, it's Cache property should also be set to
      our Cache, this happens automatically in Video_IncReference.

      Also, set FTextureUsedFullUrl here.

      In this class, this simply produces VRMLWarning with
      "not implemented" message and returns @nil. It's not declared
      as abstract, because there may be classes descending from this,
      and we want to at least be able to parse them
      and then ignore (while not overriden abstract method would cause
      simple crashes). }
    procedure LoadTextureData(out WasCacheUsed: boolean); virtual;
  public
    procedure CreateNode; override;
    destructor Destroy; override;

    { TextureImage, TextureDDS and TextureVideo contain actual texture data.
      TextureImage may come from inline VRML texture or could be loaded
      from file (including from some URL), this doesn't concern you here.

      Calls to TextureImage, TextureVideo, IsTextureImage, IsTextureVideo,
      TextureDDS
      will automatically load the data, so in simple situations you really
      don't need to do anything. Just check and use them when you want,
      and things will just work. See IsTextureLoaded for more control
      about loading / unloading.

      Note that either one of TextureImage or TextureVideo may be @nil,
      if the other one is loaded. Or when loading failed with non-fatal
      error (reported by VRMLWarning and eventually DataWarning).
      IsTextureImage checks that TextureImage is non-nil (so it's successfully
      loaded) and additionally that texture size is not zero.
      Similar for IsTextureVideo.

      TextureImage may have any class allowed by LoadTextureImage.

      @groupBegin }
    function TextureImage: TEncodedImage;
    function IsTextureImage: boolean;
    function TextureDDS: TDDSImage;
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
      to release resources (e.g. when you want to keep TNodeTexture instance
      loaded but you know that you will not need
      TextureImage / TextureDDS / TextureVideo anymore).
      You can also set it to @false and then back to @true if you want to
      request reloading the texture from URL (e.g. if you suspect that
      the URL contents changed).

      Note that IsTextureLoaded is set to @true, even if actual loading
      failed. You still have to check afterwards IsTextureImage and
      IsTextureVideo to know if loading was actually successfull.
      This is deliberate --- it means that each call to TextureImage etc.
      will not unnecessarily read the disk (or even connect to internet)
      when the file does not exist. Also, the loading errors reported
      by VRML/DataWarning will not be repeated --- they will
      occur only once, when IsTextureLoaded changes from @false to @true. }
    property IsTextureLoaded: boolean
      read FIsTextureLoaded write SetIsTextureLoaded;

    function RepeatS: boolean; virtual; abstract;
    function RepeatT: boolean; virtual; abstract;

    { Once the texture data (image or video) is loaded,
      this is set to the URL that was used to load,
      or '' if no URL was used. "No URL was used" may mean that
      no URL was valid, or inlined image was used.

      This is always a full, expanded (i.e. not relative) URL.

      In case of data: URLs, this doesn't contain actual data (it would
      be too long then, and TextureUsedFullUrl is mainly for showing to the
      user), it's cutted. }
    property TextureUsedFullUrl: string read FTextureUsedFullUrl;

    { Returns TextureProperties node, if this node has some "textureProperties"
      field and it specifies TextureProperties node. Otherwise returns @nil. }
    function TextureProperties: TNodeTextureProperties; virtual;

    private FFdAlphaChannel: TSFString;
    public property FdAlphaChannel: TSFString read FFdAlphaChannel;

    { Allows VRML authors to override automatic alpha channel detection
      of the texture, see TImage.AlphaChannelType.

      Result of this is based on the value of field "alphaChannel",
      see
      [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_alpha_channel_detection]. }
    function DetectAlphaChannel: TDetectAlphaChannel;
  end;

  TNodeX3DTexture2DNode = class(TVRML2DTextureNode)
  public
    procedure CreateNode; override;

    private FFdRepeatS: TSFBool;
    public property FdRepeatS: TSFBool read FFdRepeatS;

    private FFdRepeatT: TSFBool;
    public property FdRepeatT: TSFBool read FFdRepeatT;

    private FFdTextureProperties: TSFNode;
    public property FdTextureProperties: TSFNode read FFdTextureProperties;

    function RepeatS: boolean; override;
    function RepeatT: boolean; override;

    function TextureProperties: TNodeTextureProperties; override;
  end;

  TNodeX3DTextureTransformNode = class(TNodeX3DAppearanceChildNode)
  public
    procedure CreateNode; override;

    function TransformMatrix: TMatrix4Single; virtual; abstract;
  end;

  TNodeImageTexture = class(TNodeX3DTexture2DNode, INodeX3DUrlObject)
  protected
    procedure LoadTextureData(out WasCacheUsed: boolean); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdUrl: TMFString;
    public property FdUrl: TMFString read FFdUrl;

    function TextureDescription: string; override;
  end;

  TNodeMovieTexture = class(TNodeX3DTexture2DNode, INodeX3DSoundSourceNode,
    INodeX3DUrlObject, INodeX3DTimeDependentNode)
  private
    FDuration: TKamTime;
    FTimeDependentNodeHandler: TTimeDependentNodeHandler;
    function CycleInterval: TKamTime;
    function GetTimeDependentNodeHandler: TTimeDependentNodeHandler;
  protected
    procedure LoadTextureData(out WasCacheUsed: boolean); override;
  public
    procedure CreateNode; override;
    destructor Destroy; override;

    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdDescription: TSFString;
    public property FdDescription: TSFString read FFdDescription;

    private FFdLoop: TSFBool;
    public property FdLoop: TSFBool read FFdLoop;

    private FFdPauseTime: TSFTime;
    public property FdPauseTime: TSFTime read FFdPauseTime;

    private FFdResumeTime: TSFTime;
    public property FdResumeTime: TSFTime read FFdResumeTime;

    private FFdSpeed: TSFFloat;
    public property FdSpeed: TSFFloat read FFdSpeed;

    private FFdStartTime: TSFTime;
    public property FdStartTime: TSFTime read FFdStartTime;

    private FFdStopTime: TSFTime;
    public property FdStopTime: TSFTime read FFdStopTime;

    private FFdUrl: TMFString;
    public property FdUrl: TMFString read FFdUrl;

    { Event: SFTime, out } { }
    private FEventDuration_changed: TVRMLEvent;
    public property EventDuration_changed: TVRMLEvent read FEventDuration_changed;

    { Event: SFTime, out } { }
    private FEventElapsedTime: TVRMLEvent;
    public property EventElapsedTime: TVRMLEvent read FEventElapsedTime;

    { Event: SFBool, out } { }
    private FEventIsActive: TVRMLEvent;
    public property EventIsActive: TVRMLEvent read FEventIsActive;

    { Event: SFBool, out } { }
    private FEventIsPaused: TVRMLEvent;
    public property EventIsPaused: TVRMLEvent read FEventIsPaused;

    function TextureDescription: string; override;

    property TimeDependentNodeHandler: TTimeDependentNodeHandler
      read FTimeDependentNodeHandler;

    { Duration for this time-dependent node.
      Duration is initialized from loaded video length (default is -1).
      So it's automatically initialized when you call IsTextureVideo,
      TextureVideo methods.

      cycleInterval is just set to duration scaled by 1/Abs(speed),
      like required by X3D spec.

      Duration (and so, also cycleInterval) is not reset when video
      is freed (like when you set
      IsTextureLoaded to @false, maybe implicitly by calling
      TVRMLScene.FreeResources with frTextureDataInNodes).
      This way this is available even you freed the texture video data to
      save memory. }
    property Duration: TKamTime read FDuration;
  end;

  TNodeMultiTexture = class(TNodeX3DTextureNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAlpha: TSFFloat;
    public property FdAlpha: TSFFloat read FFdAlpha;

    private FFdColor: TSFColor;
    public property FdColor: TSFColor read FFdColor;

    private FFdFunction: TMFString;
    public property FdFunction: TMFString read FFdFunction;

    private FFdMode: TMFString;
    public property FdMode: TMFString read FFdMode;

    private FFdSource: TMFString;
    public property FdSource: TMFString read FFdSource;

    private FFdTexture: TMFNode;
    public property FdTexture: TMFNode read FFdTexture;
  end;

  TNodeMultiTextureCoordinate = class(TNodeX3DTextureCoordinateNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdTexCoord: TMFNode;
    public property FdTexCoord: TMFNode read FFdTexCoord;
  end;

  TNodeMultiTextureTransform = class(TNodeX3DTextureTransformNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdTextureTransform: TMFNode;
    public property FdTextureTransform: TMFNode read FFdTextureTransform;

    { For MultiTextureTransform, this always raises an internal error.
      Reason: you cannot get single texture transform matrix from
      MultiTextureTransform.

      @raises(EInternalError Always, since this method has no sense
        for MultiTextureTransform.) }
    function TransformMatrix: TMatrix4Single; override;
  end;

  TNodePixelTexture = class(TNodeX3DTexture2DNode)
  protected
    procedure LoadTextureData(out WasCacheUsed: boolean); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdImage: TSFImage;
    public property FdImage: TSFImage read FFdImage;

    function TextureDescription: string; override;
  end;

  TNodeTextureCoordinate = class(TNodeX3DTextureCoordinateNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdPoint: TMFVec2f;
    public property FdPoint: TMFVec2f read FFdPoint;
  end;

  TNodeTextureCoordinateGenerator = class(TNodeX3DTextureCoordinateNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdMode: TSFString;
    public property FdMode: TSFString read FFdMode;

    private FFdParameter: TMFFloat;
    public property FdParameter: TMFFloat read FFdParameter;

    private FFdProjectedLight: TSFNode;
    public property FdProjectedLight: TSFNode read FFdProjectedLight;
  end;

  { Old BS Contact name for TextureCoordinateGenerator.
    See examples from [http://www.bitmanagement.de/developer/contact/relnotes6.html] }
  TNodeTextureCoordGen = class(TNodeTextureCoordinateGenerator)
  public
    class function ClassNodeTypeName: string; override;
  end;

  TNodeTextureProperties = class(TNodeX3DNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAnisotropicDegree: TSFFloat;
    public property FdAnisotropicDegree: TSFFloat read FFdAnisotropicDegree;

    private FFdBorderColor: TSFColorRGBA;
    public property FdBorderColor: TSFColorRGBA read FFdBorderColor;

    private FFdBorderWidth: TSFInt32;
    public property FdBorderWidth: TSFInt32 read FFdBorderWidth;

    private FFdBoundaryModeS: TSFString;
    public property FdBoundaryModeS: TSFString read FFdBoundaryModeS;

    private FFdBoundaryModeT: TSFString;
    public property FdBoundaryModeT: TSFString read FFdBoundaryModeT;

    private FFdBoundaryModeR: TSFString;
    public property FdBoundaryModeR: TSFString read FFdBoundaryModeR;

    private FFdMagnificationFilter: TSFString;
    public property FdMagnificationFilter: TSFString read FFdMagnificationFilter;

    private FFdMinificationFilter: TSFString;
    public property FdMinificationFilter: TSFString read FFdMinificationFilter;

    private FFdTextureCompression: TSFString;
    public property FdTextureCompression: TSFString read FFdTextureCompression;

    private FFdTexturePriority: TSFFloat;
    public property FdTexturePriority: TSFFloat read FFdTexturePriority;

    private FFdGenerateMipMaps: TSFBool;
    public property FdGenerateMipMaps: TSFBool read FFdGenerateMipMaps;
  end;

  TNodeTextureTransform = class(TNodeX3DTextureTransformNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdCenter: TSFVec2f;
    public property FdCenter: TSFVec2f read FFdCenter;

    private FFdRotation: TSFFloat;
    public property FdRotation: TSFFloat read FFdRotation;

    private FFdScale: TSFVec2f;
    public property FdScale: TSFVec2f read FFdScale;

    private FFdTranslation: TSFVec2f;
    public property FdTranslation: TSFVec2f read FFdTranslation;

    function TransformMatrix: TMatrix4Single; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TNodeX3DTextureCoordinateNode ---------------------------------------------- }

procedure TNodeX3DTextureCoordinateNode.CreateNode;
begin
  inherited;

  { X3D XML spec doesn't specify containerField for abstract X3D classes.
    texCoord seems most sensible for this case. }
  DefaultContainerField := 'texCoord';
end;

{ TNodeX3DTextureNode -------------------------------------------------------- }

procedure TNodeX3DTextureNode.CreateNode;
begin
  inherited;

  FFdEffects := TMFNode.Create(Self, 'effects', [TNodeEffect]);
   FdEffects.Exposed := false;
   FdEffects.ChangesAlways := [chEverything];
  Fields.Add(FFdEffects);

  DefaultContainerField := 'texture';
end;

function TNodeX3DTextureNode.TextureDescription: string;
begin
  Result := NodeTypeName;
end;

{ TVRML2DTextureNode ----------------------------------------------------------- }

procedure TVRML2DTextureNode.CreateNode;
begin
  inherited;

  FFdAlphaChannel := TSFString.Create(Self, 'alphaChannel', 'AUTO');
   FdAlphaChannel.Exposed := false;
   FdAlphaChannel.ChangesAlways := [chVisibleVRML1State];
  Fields.Add(FFdAlphaChannel);

  UsedCache := nil;
  FIsTextureLoaded := false;
end;

destructor TVRML2DTextureNode.Destroy;
begin
  FreeAndNilTextureData;
  inherited;
end;

procedure TVRML2DTextureNode.FreeAndNilTextureData;
begin
  if FTextureImage <> nil then
  begin
    if UsedCache <> nil then
    begin
      UsedCache.TextureImage_DecReference(FTextureImage, FTextureDDS);
      UsedCache := nil;
    end else
    begin
      FreeAndNil(FTextureImage);
      FreeAndNil(FTextureDDS);
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

function TVRML2DTextureNode.TextureImage: TEncodedImage;
begin
  { Setting IsTextureLoaded property will initialize FTextureImage. }
  IsTextureLoaded := true;

  Result := FTextureImage;
end;

function TVRML2DTextureNode.IsTextureImage: boolean;
begin
  Result := (TextureImage <> nil) and (not TextureImage.IsNull);
end;

function TVRML2DTextureNode.TextureDDS: TDDSImage;
begin
  { Setting IsTextureLoaded property will initialize
    FTextureImage, FTextureDDS. }
  IsTextureLoaded := true;

  Result := FTextureDDS;
end;

function TVRML2DTextureNode.TextureVideo: TVideo;
begin
  { Setting IsTextureLoaded property will initialize FTextureVideo. }
  IsTextureLoaded := true;

  Result := FTextureVideo;
end;

function TVRML2DTextureNode.IsTextureVideo: boolean;
begin
  Result := (TextureVideo <> nil) and
    (TextureVideo.Width <> 0) and
    (TextureVideo.Height <> 0);
end;

procedure TVRML2DTextureNode.SetIsTextureLoaded(Value: boolean);

  procedure DoLoadTexture;
  var
    WasCacheUsed: boolean;
  begin
    FreeAndNilTextureData;

    LoadTextureData(WasCacheUsed);
    if WasCacheUsed then
      UsedCache := VRMLCache;
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

procedure TVRML2DTextureNode.LoadTextureData(out WasCacheUsed: boolean);
begin
  WasCacheUsed := false;
  FTextureUsedFullUrl := '';

  VRMLWarning(vwSerious, Format('Loading textures from "%s" node not implemented', [NodeTypeName]));
end;

function TVRML2DTextureNode.TextureProperties: TNodeTextureProperties;
begin
  Result := nil;
end;

function TVRML2DTextureNode.DetectAlphaChannel: TDetectAlphaChannel;

  function DetectAlphaChannelFromString(S: string): TDetectAlphaChannel;
  begin
    S := UpperCase(S);
    if S = 'AUTO' then
      Result := daAuto else
    if S = 'SIMPLE_YES_NO' then
      Result := daSimpleYesNo else
    if S = 'FULL_RANGE' then
      Result := daFullRange else
    begin
      VRMLWarning(vwSerious, Format('Invalid "alphaChannel" field value "%s"', [S]));
      Result := daAuto;
    end;
  end;

begin
  Result := DetectAlphaChannelFromString(FdAlphaChannel.Value);
end;

{ TNodeX3DTexture2DNode ------------------------------------------------------ }

procedure TNodeX3DTexture2DNode.CreateNode;
begin
  inherited;

  FFdrepeatS := TSFBool.Create(Self, 'repeatS', true);
   FdrepeatS.Exposed := false;
   FdrepeatS.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdrepeatS);

  FFdrepeatT := TSFBool.Create(Self, 'repeatT', true);
   FdrepeatT.Exposed := false;
   FdrepeatT.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdrepeatT);

  FFdtextureProperties := TSFNode.Create(Self, 'textureProperties', [TNodeTextureProperties]);
   FdtextureProperties.Exposed := false;
   FdtextureProperties.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdtextureProperties);
end;

function TNodeX3DTexture2DNode.RepeatS: boolean;
begin
  Result := FdRepeatS.Value;
end;

function TNodeX3DTexture2DNode.RepeatT: boolean;
begin
  Result := FdRepeatT.Value;
end;

function TNodeX3DTexture2DNode.TextureProperties: TNodeTextureProperties;
begin
  if (FdTextureProperties.Value <> nil) and
     (FdTextureProperties.Value is TNodeTextureProperties) then
    Result := TNodeTextureProperties(FdTextureProperties.Value) else
    Result := nil;
end;

procedure TNodeX3DTextureTransformNode.CreateNode;
begin
  inherited;

  DefaultContainerField := 'textureTransform';
end;

procedure TNodeImageTexture.CreateNode;
begin
  inherited;

  FFdurl := TMFString.Create(Self, 'url', []);
   Fdurl.ChangesAlways := [chTextureImage];
  Fields.Add(FFdurl);
  { X3D specification comment: [URI] }
end;

class function TNodeImageTexture.ClassNodeTypeName: string;
begin
  Result := 'ImageTexture';
end;

class function TNodeImageTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeImageTexture.LoadTextureData(out WasCacheUsed: boolean);

  { Load image from URL containing "data:" protocol
    (http://en.wikipedia.org/wiki/Data_URI_scheme).
    If success,
    - sets FTextureImage, FTextureDDS, WasCacheUsed, FTextureUsedFullUrl
    - returns true
    otherwise
    - makes VRMLWarning
    - returns false }
  function LoadImageFromDataProtocol(const URL: string): boolean;
  var
    Data: TDataURI;
    ImageFormat: TImageFormat;
  begin
    Data := TDataURI.Create;
    try
      Data.URI := URL;

      if not Data.Valid then Exit(false);

      if Log then
        WritelnLog('Texture loading',
          Format('Loading from data URI with mime "%s", charset "%s", base64 "%s"',
          [Data.Mime, Data.Charset, BoolToStr[Data.Base64]]));

      if Data.Mime = 'image/bmp' then
        ImageFormat := ifBMP else
      if Data.Mime = 'image/png' then
        ImageFormat := ifPNG else
      if (Data.Mime = 'image/jpeg') or
         (Data.Mime = 'image/jpg') then
        ImageFormat := ifJPEG else
      if Data.Mime = 'image/gif' then
        ImageFormat := ifGIF else
      begin
        VRMLWarning(vwSerious, Format('Data mime-type "%s" not recognized, not loading. If this is a valid image mime type that should be supported, please report.', [Data.Mime]));
        Exit(false);
      end;

      try
        FTextureImage := LoadImage(Data.Stream, ImageFormat,
          [TRGBImage, TRGBAlphaImage, TGrayscaleImage, TGrayscaleAlphaImage], []);
        WasCacheUsed := false;
        FTextureUsedFullUrl := Data.URIPrefix;
        Result := true;
      except
        on E: Exception do
        begin
          { Remember that VRMLWarning *may* raise an exception. }
          VRMLWarning(vwIgnorable, Format(SDataURILoadError,
            [E.ClassName, 'texture', Data.URIPrefix, E.Message]));
          Result := false;
        end;
      end;
    finally FreeAndNil(Data) end;
  end;

var
  I: Integer;
  FullUrl: string;
begin
  WasCacheUsed := false;
  FTextureUsedFullUrl := '';

  for I := 0 to FdUrl.Count - 1 do
    if FdUrl.Items[I] = '' then
    begin
      { Empty URL would be expanded by PathFromWWWBasePath to directory
        and produce unclear error message. }
      VRMLWarning(vwIgnorable, Format('Empty URL for %s ignored', [NodeTypeName]));
    end else
    if TDataURI.IsDataURI(FdUrl.Items[I]) then
    begin
      if LoadImageFromDataProtocol(FdUrl.Items[I]) then Break;
    end else
    begin
      FullUrl := PathFromWWWBasePath(FdUrl.Items[I]);
      try
        FTextureImage := VRMLCache.TextureImage_IncReference(FullUrl, FTextureDDS);
        WasCacheUsed := true;
        FTextureUsedFullUrl := FullUrl;
        Break;
      except
        on E: Exception do
          { Remember that VRMLWarning *may* raise an exception. }
          VRMLWarning(vwIgnorable, Format(SLoadError,
            [E.ClassName, 'texture', FullUrl, E.Message]));
      end;
    end;
end;

function TNodeImageTexture.TextureDescription: string;
begin
  if TextureUsedFullUrl <> '' then
    Result := 'image from file "' + TextureUsedFullUrl + '"' else
    Result := 'none';
end;

procedure TNodeMovieTexture.CreateNode;
begin
  inherited;

  FFddescription := TSFString.Create(Self, 'description', '');
  Fields.Add(FFddescription);

  FFdloop := TSFBool.Create(Self, 'loop', false);
  Fields.Add(FFdloop);

  FFdpauseTime := TSFTime.Create(Self, 'pauseTime', 0);
   FdpauseTime.ChangesAlways := [chTimeStopStart];
  Fields.Add(FFdpauseTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdresumeTime := TSFTime.Create(Self, 'resumeTime', 0);
   FdresumeTime.ChangesAlways := [chTimeStopStart];
  Fields.Add(FFdresumeTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdspeed := TSFFloat.Create(Self, 'speed', 1.0);
  Fields.Add(FFdspeed);
  { X3D specification comment: (-Inf,Inf) }

  FFdstartTime := TSFTimeIgnoreWhenActive.Create(Self, 'startTime', 0);
   FdstartTime.ChangesAlways := [chTimeStopStart];
  Fields.Add(FFdstartTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdstopTime := TSFStopTime.Create(Self, 'stopTime', 0);
   FdstopTime.ChangesAlways := [chTimeStopStart];
  Fields.Add(FFdstopTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdurl := TMFString.Create(Self, 'url', []);
   Fdurl.ChangesAlways := [chTextureImage];
  Fields.Add(FFdurl);
  { X3D specification comment: [URI] }

  FEventduration_changed := TVRMLEvent.Create(Self, 'duration_changed', TSFTime, false);
  Events.Add(FEventduration_changed);

  FEventelapsedTime := TVRMLEvent.Create(Self, 'elapsedTime', TSFTime, false);
  Events.Add(FEventelapsedTime);

  FEventisActive := TVRMLEvent.Create(Self, 'isActive', TSFBool, false);
  Events.Add(FEventisActive);

  FEventisPaused := TVRMLEvent.Create(Self, 'isPaused', TSFBool, false);
  Events.Add(FEventisPaused);

  FDuration := -1;

  FTimeDependentNodeHandler := TTimeDependentNodeHandler.Create;
  FTimeDependentNodeHandler.Node := Self;
  FTimeDependentNodeHandler.Fdloop := FdLoop;
  FTimeDependentNodeHandler.FdpauseTime := FdPauseTime;
  FTimeDependentNodeHandler.FdresumeTime := FdResumeTime;
  FTimeDependentNodeHandler.FdstartTime := FdStartTime;
  FTimeDependentNodeHandler.FdstopTime := FdStopTime;
  FTimeDependentNodeHandler.EventisActive:= EventisActive;
  FTimeDependentNodeHandler.EventisPaused := EventisPaused;
  FTimeDependentNodeHandler.EventelapsedTime := EventelapsedTime;
  FTimeDependentNodeHandler.OnCycleInterval := @CycleInterval;
end;

function TNodeMovieTexture.GetTimeDependentNodeHandler: TTimeDependentNodeHandler;
begin
  Result := FTimeDependentNodeHandler;
end;

destructor TNodeMovieTexture.Destroy;
begin
  FreeAndNil(FTimeDependentNodeHandler);
  inherited;
end;

class function TNodeMovieTexture.ClassNodeTypeName: string;
begin
  Result := 'MovieTexture';
end;

class function TNodeMovieTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeMovieTexture.LoadTextureData(out WasCacheUsed: boolean);
var
  I: Integer;
  FullUrl: string;
begin
  WasCacheUsed := true;
  FTextureUsedFullUrl := '';

  for I := 0 to FdUrl.Count - 1 do
  begin
    FullUrl := PathFromWWWBasePath(FdUrl.Items[I]);
    try
      FTextureVideo := VRMLCache.Video_IncReference(FullUrl);

      { if loading succeded, set WasCacheUsed and others and break. }
      WasCacheUsed := true;
      FTextureUsedFullUrl := FullUrl;
      FDuration := FTextureVideo.TimeDuration;
      if Scene <> nil then
        EventDuration_Changed.Send(FDuration, Scene.GetTime);
      Break;
    except
      on E: Exception do
        { Remember that VRMLWarning *may* raise an exception. }
        VRMLWarning(vwIgnorable, Format(SLoadError,
          [E.ClassName, 'video', FullUrl, E.Message]));
    end;
  end;
end;

function TNodeMovieTexture.CycleInterval: TKamTime;
begin
  if FdSpeed.Value <> 0 then
    Result := Duration / Abs(FdSpeed.Value) else
    Result := 0;
end;

function TNodeMovieTexture.TextureDescription: string;
begin
  if TextureUsedFullUrl <> '' then
    Result := 'video from file "' + TextureUsedFullUrl + '"' else
    Result := 'none';
end;

procedure TNodeMultiTexture.CreateNode;
begin
  inherited;

  FFdalpha := TSFFloat.Create(Self, 'alpha', 1);
   Fdalpha.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdalpha);
  { X3D specification comment: [0,1] }

  FFdcolor := TSFColor.Create(Self, 'color', Vector3Single(1, 1, 1));
   Fdcolor.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdcolor);
  { X3D specification comment: [0,1] }

  FFdfunction := TMFString.Create(Self, 'function', []);
   Fdfunction.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdfunction);

  FFdmode := TMFString.Create(Self, 'mode', []);
   Fdmode.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdmode);

  FFdsource := TMFString.Create(Self, 'source', []);
   Fdsource.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdsource);

  FFdtexture := TMFNode.Create(Self, 'texture', [TNodeX3DTextureNode]);
   Fdtexture.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdtexture);
end;

class function TNodeMultiTexture.ClassNodeTypeName: string;
begin
  Result := 'MultiTexture';
end;

class function TNodeMultiTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeMultiTextureCoordinate.CreateNode;
begin
  inherited;

  FFdtexCoord := TMFNode.Create(Self, 'texCoord', [TNodeX3DTextureCoordinateNode]);
   FdtexCoord.ChangesAlways := [chTextureCoordinate];
  Fields.Add(FFdtexCoord);
end;

class function TNodeMultiTextureCoordinate.ClassNodeTypeName: string;
begin
  Result := 'MultiTextureCoordinate';
end;

class function TNodeMultiTextureCoordinate.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeMultiTextureTransform.CreateNode;
begin
  inherited;

  FFdtextureTransform := TMFNode.Create(Self, 'textureTransform', [TNodeX3DTextureTransformNode]);
   FdtextureTransform.ChangesAlways := [chEverything];
  Fields.Add(FFdtextureTransform);
end;

class function TNodeMultiTextureTransform.ClassNodeTypeName: string;
begin
  Result := 'MultiTextureTransform';
end;

class function TNodeMultiTextureTransform.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TNodeMultiTextureTransform.TransformMatrix: TMatrix4Single;
begin
  raise EInternalError.Create('You cannot get single TransformMatrix from MultiTextureTransform node');
  Result := IdentityMatrix4Single; { avoid warnings that result not set }
end;

procedure TNodePixelTexture.CreateNode;
begin
  inherited;

  FFdimage := TSFImage.Create(Self, 'image', nil);
   Fdimage.ChangesAlways := [chTextureImage];
  Fields.Add(FFdimage);
  { X3D specification comment: 0 0 }
  end;

class function TNodePixelTexture.ClassNodeTypeName: string;
begin
  Result := 'PixelTexture';
end;

class function TNodePixelTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodePixelTexture.LoadTextureData(out WasCacheUsed: boolean);
begin
  WasCacheUsed := false;

  if not FdImage.Value.IsNull then
    FTextureImage := FdImage.Value.MakeCopy;
end;

function TNodePixelTexture.TextureDescription: string;
begin
  if not FdImage.Value.IsNull then
    result := Format('inlined image (width = %d; height = %d; with alpha = %s)',
      [ FdImage.Value.Width, FdImage.Value.Height,
        BoolToStr[FdImage.Value.HasAlpha] ]) else
    result := 'none';
end;

procedure TNodeTextureCoordinate.CreateNode;
begin
  inherited;

  FFdpoint := TMFVec2f.Create(Self, 'point', []);
   Fdpoint.ChangesAlways := [chTextureCoordinate];
  Fields.Add(FFdpoint);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TNodeTextureCoordinate.ClassNodeTypeName: string;
begin
  Result := 'TextureCoordinate';
end;

class function TNodeTextureCoordinate.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeTextureCoordinateGenerator.CreateNode;
begin
  inherited;

  FFdmode := TSFString.Create(Self, 'mode', 'SPHERE');
   Fdmode.ChangesAlways := [chTextureCoordinate];
  Fields.Add(FFdmode);
  { X3D specification comment: [see Table 18.6] }

  FFdparameter := TMFFloat.Create(Self, 'parameter', []);
   Fdparameter.ChangesAlways := [chTextureCoordinate];
  Fields.Add(FFdparameter);
  { X3D specification comment: [see Table 18.6] }

  { Note that projectedLight node is not enumerated as an active node
    for traversing (in DirectEnumerateActive), because the light doesn't
    shine here. We don't want
    to override it's transform with transformation of this
    TextureCoordinateGenerator. }
  FFdprojectedLight := TSFNode.Create(Self, 'projectedLight',
    [TNodeSpotLight_1, TNodeDirectionalLight_1,
     TNodeSpotLight  , TNodeDirectionalLight  ]);
   FdprojectedLight.ChangesAlways := [chTextureCoordinate];
  Fields.Add(FFdprojectedLight);
end;

class function TNodeTextureCoordinateGenerator.ClassNodeTypeName: string;
begin
  Result := 'TextureCoordinateGenerator';
end;

class function TNodeTextureCoordinateGenerator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeTextureCoordGen.ClassNodeTypeName: string;
begin
  Result := 'TextureCoordGen';
end;

procedure TNodeTextureProperties.CreateNode;
begin
  inherited;

  FFdanisotropicDegree := TSFFloat.Create(Self, 'anisotropicDegree', 1.0);
   FdanisotropicDegree.ChangesAlways := [chTexturePropertiesNode];
  Fields.Add(FFdanisotropicDegree);
  { X3D specification comment: [1,Inf) }

  FFdborderColor := TSFColorRGBA.Create(Self, 'borderColor', Vector4Single(0, 0, 0, 0));
   FdborderColor.ChangesAlways := [chTexturePropertiesNode];
  Fields.Add(FFdborderColor);
  { X3D specification comment: [0,1] }

  FFdborderWidth := TSFInt32.Create(Self, 'borderWidth', 0);
   FdborderWidth.ChangesAlways := [chTexturePropertiesNode];
  Fields.Add(FFdborderWidth);
  { X3D specification comment: [0,1] }

  FFdboundaryModeS := TSFString.Create(Self, 'boundaryModeS', 'REPEAT');
   FdboundaryModeS.ChangesAlways := [chTexturePropertiesNode];
  Fields.Add(FFdboundaryModeS);
  { X3D specification comment: [see Table 18.7] }

  FFdboundaryModeT := TSFString.Create(Self, 'boundaryModeT', 'REPEAT');
   FdboundaryModeT.ChangesAlways := [chTexturePropertiesNode];
  Fields.Add(FFdboundaryModeT);
  { X3D specification comment: [see Table 18.7] }

  FFdboundaryModeR := TSFString.Create(Self, 'boundaryModeR', 'REPEAT');
   FdboundaryModeR.ChangesAlways := [chTexturePropertiesNode];
  Fields.Add(FFdboundaryModeR);
  { X3D specification comment: [see Table 18.7] }

  FFdmagnificationFilter := TSFString.Create(Self, 'magnificationFilter', 'FASTEST');
   FdmagnificationFilter.ChangesAlways := [chTexturePropertiesNode];
  Fields.Add(FFdmagnificationFilter);
  { X3D specification comment: [see Table 18.8] }

  FFdminificationFilter := TSFString.Create(Self, 'minificationFilter', 'FASTEST');
   FdminificationFilter.ChangesAlways := [chTexturePropertiesNode];
  Fields.Add(FFdminificationFilter);
  { X3D specification comment: [see Table 18.9] }

  FFdtextureCompression := TSFString.Create(Self, 'textureCompression', 'FASTEST');
   FdtextureCompression.ChangesAlways := [chTexturePropertiesNode];
  Fields.Add(FFdtextureCompression);
  { X3D specification comment: [see Table 18.10] }

  FFdtexturePriority := TSFFloat.Create(Self, 'texturePriority', 0);
   FdtexturePriority.ChangesAlways := [chTexturePropertiesNode];
  Fields.Add(FFdtexturePriority);
  { X3D specification comment: [0,1] }

  FFdgenerateMipMaps := TSFBool.Create(Self, 'generateMipMaps', false);
   FdgenerateMipMaps.Exposed := false;
   FdgenerateMipMaps.ChangesAlways := [chTexturePropertiesNode];
  Fields.Add(FFdgenerateMipMaps);

  { X3D XML spec (edition 2) mistakenly claims it should be
    "lineProperties", which is nonsense... I set this to "textureProperties". }
  DefaultContainerField := 'textureProperties';
end;

class function TNodeTextureProperties.ClassNodeTypeName: string;
begin
  Result := 'TextureProperties';
end;

class function TNodeTextureProperties.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeTextureTransform.CreateNode;
begin
  inherited;

  FFdcenter := TSFVec2f.Create(Self, 'center', Vector2Single(0, 0));
   Fdcenter.ChangesAlways := [chTextureTransform];
  Fields.Add(FFdcenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdrotation := TSFFloat.Create(Self, 'rotation', 0);
   FFdrotation.ChangesAlways := [chTextureTransform];
  Fields.Add(FFdrotation);
  { X3D specification comment: (-Inf,Inf) }

  FFdscale := TSFVec2f.Create(Self, 'scale', Vector2Single(1, 1));
   Fdscale.ChangesAlways := [chTextureTransform];
  Fields.Add(FFdscale);
  { X3D specification comment: (-Inf,Inf) }

  FFdtranslation := TSFVec2f.Create(Self, 'translation', Vector2Single(0, 0));
   Fdtranslation.ChangesAlways := [chTextureTransform];
  Fields.Add(FFdtranslation);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TNodeTextureTransform.ClassNodeTypeName: string;
begin
  Result := 'TextureTransform';
end;

class function TNodeTextureTransform.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TNodeTextureTransform.TransformMatrix: TMatrix4Single;
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

  Result := TranslationMatrix(
    Vector3Single( -FdCenter.Value[0], -FdCenter.Value[1], 0 ));
  Result := MatrixMult(Result,
    ScalingMatrix(
      Vector3Single( FdScale.Value[0], FdScale.Value[1], 1 )));
  Result := MatrixMult(Result,
    RotationMatrixRad(FdRotation.Value, Vector3Single(0, 0, 1)));
  Result := MatrixMult(Result,
    TranslationMatrix( Vector3Single(
      FdTranslation.Value[0] + FdCenter.Value[0],
      FdTranslation.Value[1] + FdCenter.Value[1], 0)));
end;

procedure RegisterTexturingNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeImageTexture,
    TNodeMovieTexture,
    TNodeMultiTexture,
    TNodeMultiTextureCoordinate,
    TNodeMultiTextureTransform,
    TNodePixelTexture,
    TNodeTextureCoordinate,
    TNodeTextureCoordinateGenerator,
    TNodeTextureCoordGen,
    TNodeTextureProperties,
    TNodeTextureTransform
  ]);
end;

{$endif read_implementation}
