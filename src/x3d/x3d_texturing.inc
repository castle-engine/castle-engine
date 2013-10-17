{
  Copyright 2002-2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { }
  TAbstractTextureCoordinateNode = class(TAbstractGeometricPropertyNode)
  public
    procedure CreateNode; override;
  end;

  TAbstractTextureNode = class(TAbstractAppearanceChildNode)
  private
    AlphaChannelWarningDone: boolean;
  protected
    { Alpha channel detected from image contents (or children nodes),
      ignoring our FdAlphaChannel field.
      Actually set only when image data is loaded
      (e.g. TAbstractTexture2DNode.IsTextureLoaded in case of TAbstractTexture2DNode
      descendant). }
    function AlphaChannelData: TAlphaChannel; virtual;
  public
    procedure CreateNode; override;

    { Short description how texture is defined, is it inline or loaded
      from URL, is it video of simple image texture.
      "none" if it's not defined at all.

      Calling this @italic(may not) cause automatically loading
      the texture data (for exampe, from file in case of TAbstractTexture2DNode).
      So it cannot describe the actually loaded data.

      In this class, simply returns NodeTypeName.
      Override to say something more descriptive. }
    function TextureDescription: string; virtual;

    private FFdEffects: TMFNode;
    public property FdEffects: TMFNode read FFdEffects;

    { Alpha channel of the loaded texture data.
      Looks at loaded texture data, and at alphaChannel field
      (see http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_alpha_channel_detection ) }
    function AlphaChannel: TAlphaChannel;

    private FFdAlphaChannel: TSFString;
    public property FdAlphaChannel: TSFString read FFdAlphaChannel;
  end;

  TTexturePropertiesNode = class;

  { VRML/X3D texture that is 2D and is loaded (from file or some other stream).

    For X3D, this descends from X3DTextureNode and is an ancestor
    for X3DTexture2DNode, so X3D hierarchy is nicely preserved. }
  TAbstractTexture2DNode = class(TAbstractTextureNode)
  private
    { Together we call FTextureImage, FTextureDDS,
      FTextureVideo, FAlphaChannelData as "texture data". }

    { FTextureImage is <> nil if texture is currently loaded (IsTextureLoaded)
      and it was loaded to an image (not video).

      Note that this may still have zero size (IsEmpty = @true),
      IsTextureImage checks this also. }
    FTextureImage: TEncodedImage;

    { Only if FTextureImage is <> nil, then FTextureDDS may also be <> nil,
      it this image is part of DDS file. }
    FTextureDDS: TDDSImage;

    { Analogous to FTextureImage, this is the loaded video file.
      Assigned here, should always have TVideo.Loaded = @true. }
    FTextureVideo: TVideo;

    FAlphaChannelData: TAlphaChannel;

    { Non-nil only if FTextureImage, FTextureDDS or FTextueVideo
      should be freed using
      UsedCache (TextureImage_DecReference or Video_DecReference).
      Also loaded FTextureVideo should always have it's own Cache property set
      to this. }
    UsedCache: TTexturesVideosCache;

    FIsTextureLoaded: boolean;
    procedure SetIsTextureLoaded(Value: boolean);
    procedure FreeAndNilTextureData;
  protected
    function AlphaChannelData: TAlphaChannel; override;
  protected
    FTextureUsedFullUrl: string;

    { Loads texture data (image or video file).

      It should set either FTextureImage or FTextureVideo to something non-nil
      (don't care here about the previous value of these fields --- it's for
      sure @nil). If FTextureImage is set, you can also set FTextureDDS.
      If you leave them as @nil, this means that loading failed
      (and OnWarning didn't cause an exception).
      You should also set FAlphaChannelData.

      You do not care in this method about things like
      IsImageLoaded --- this method should just always,
      unconditionally, make everything it can do to load texture data from
      file(s).

      You can use OnWarning inside,
      so we're prepared that this may even exit with exception
      (since OnWarning can raise exception).

      If you set FTextureImage, you have to set it to something
      returned by LoadTextureImage. See TextureImage docs.

      Set WasCacheUsed here. @true means you loaded the data using Cache.
      For FTextureVideo, it's Cache property should also be set to
      our Cache, this happens automatically in Video_IncReference.

      Also, set FTextureUsedFullUrl here.

      In this class, this simply produces OnWarning with
      "not implemented" message and returns @nil. It's not declared
      as abstract, because there may be classes descending from this,
      and we want to at least be able to parse them
      and then ignore (while not overriden abstract method would cause
      simple crashes). }
    procedure LoadTextureData(out WasCacheUsed: boolean); virtual;

    function GetRepeatS: boolean; virtual; abstract;
    function GetRepeatT: boolean; virtual; abstract;
    procedure SetRepeatS(const Value: boolean); virtual; abstract;
    procedure SetRepeatT(const Value: boolean); virtual; abstract;
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
      if the other one is loaded. Or when loading failed
      (warning will be reported by OnWarning).
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
      to release resources (e.g. when you want to keep TTextureNode instance
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
      by OnWarning will not be repeated --- they will
      occur only once, when IsTextureLoaded changes from @false to @true. }
    property IsTextureLoaded: boolean
      read FIsTextureLoaded write SetIsTextureLoaded;

    { Whether the texture repeats or clamps in given direction.
      Getting or setting this is the most comfortable way to change underlying
      node fields, setting this automatically does all necessary (sends events
      etc., see TVRMLField.Send).
      @groupBegin }
    property RepeatS: boolean read GetRepeatS write SetRepeatS;
    property RepeatT: boolean read GetRepeatT write SetRepeatT;
    { @groupEnd }

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
    function TextureProperties: TTexturePropertiesNode; virtual;
  end;

  TAbstractX3DTexture2DNode = class(TAbstractTexture2DNode)
  protected
    function GetRepeatS: boolean; override;
    function GetRepeatT: boolean; override;
    procedure SetRepeatS(const Value: boolean); override;
    procedure SetRepeatT(const Value: boolean); override;
  public
    procedure CreateNode; override;

    private FFdRepeatS: TSFBool;
    public property FdRepeatS: TSFBool read FFdRepeatS;

    private FFdRepeatT: TSFBool;
    public property FdRepeatT: TSFBool read FFdRepeatT;

    private FFdTextureProperties: TSFNode;
    public property FdTextureProperties: TSFNode read FFdTextureProperties;

    function TextureProperties: TTexturePropertiesNode; override;
  end;

  TAbstractTextureTransformNode = class(TAbstractAppearanceChildNode)
  public
    procedure CreateNode; override;

    function TransformMatrix: TMatrix4Single; virtual; abstract;
  end;

  TImageTextureNode = class(TAbstractX3DTexture2DNode, IAbstractUrlObject)
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

  TMovieTextureNode = class(TAbstractX3DTexture2DNode, IAbstractSoundSourceNode,
    IAbstractUrlObject, IAbstractTimeDependentNode)
  private
    FDuration: TFloatTime;
    FTimeDependentNodeHandler: TTimeDependentNodeHandler;
    function CycleInterval: TFloatTime;
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
    private FEventDuration_changed: TX3DEvent;
    public property EventDuration_changed: TX3DEvent read FEventDuration_changed;

    { Event: SFTime, out } { }
    private FEventElapsedTime: TX3DEvent;
    public property EventElapsedTime: TX3DEvent read FEventElapsedTime;

    { Event: SFBool, out } { }
    private FEventIsActive: TX3DEvent;
    public property EventIsActive: TX3DEvent read FEventIsActive;

    { Event: SFBool, out } { }
    private FEventIsPaused: TX3DEvent;
    public property EventIsPaused: TX3DEvent read FEventIsPaused;

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
      TCastleSceneCore.FreeResources with frTextureDataInNodes).
      This way this is available even you freed the texture video data to
      save memory. }
    property Duration: TFloatTime read FDuration;
  end;

  TMultiTextureNode = class(TAbstractTextureNode)
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

    function AlphaChannelData: TAlphaChannel; override;
  end;

  TMultiTextureCoordinateNode = class(TAbstractTextureCoordinateNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdTexCoord: TMFNode;
    public property FdTexCoord: TMFNode read FFdTexCoord;
  end;

  TMultiTextureTransformNode = class(TAbstractTextureTransformNode)
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

  TPixelTextureNode = class(TAbstractX3DTexture2DNode)
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

  TTextureCoordinateNode = class(TAbstractTextureCoordinateNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdPoint: TMFVec2f;
    public property FdPoint: TMFVec2f read FFdPoint;
  end;

  TTextureCoordinateGeneratorNode = class(TAbstractTextureCoordinateNode)
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
  TTextureCoordGenNode = class(TTextureCoordinateGeneratorNode)
  public
    class function ClassNodeTypeName: string; override;
  end;

  TTexturePropertiesNode = class(TAbstractNode)
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

  TTextureTransformNode = class(TAbstractTextureTransformNode)
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

{ TAbstractTextureCoordinateNode ---------------------------------------------- }

procedure TAbstractTextureCoordinateNode.CreateNode;
begin
  inherited;

  { X3D XML spec doesn't specify containerField for abstract X3D classes.
    texCoord seems most sensible for this case. }
  DefaultContainerField := 'texCoord';
end;

{ TAbstractTextureNode -------------------------------------------------------- }

procedure TAbstractTextureNode.CreateNode;
begin
  inherited;

  FFdEffects := TMFNode.Create(Self, 'effects', [TEffectNode]);
   FdEffects.Exposed := false;
   FdEffects.ChangesAlways := [chEverything];
  Fields.Add(FFdEffects);

  FFdAlphaChannel := TSFString.Create(Self, 'alphaChannel', 'AUTO');
   FdAlphaChannel.Exposed := false;
   FdAlphaChannel.ChangesAlways := [chVisibleVRML1State];
  Fields.Add(FFdAlphaChannel);

  DefaultContainerField := 'texture';
end;

function TAbstractTextureNode.TextureDescription: string;
begin
  Result := NodeTypeName;
end;

function TAbstractTextureNode.AlphaChannelData: TAlphaChannel;
begin
  Result := acNone;
end;

function TAbstractTextureNode.AlphaChannel: TAlphaChannel;
var
  Res: TAutoAlphaChannel;
begin
  Res := StringToAlpha(FdAlphaChannel.Value, AlphaChannelWarningDone);
  if Res = acAuto then
    Result := AlphaChannelData else
    Result := Res;
end;

{ TAbstractTexture2DNode ----------------------------------------------------------- }

procedure TAbstractTexture2DNode.CreateNode;
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

function TAbstractTexture2DNode.TextureDDS: TDDSImage;
begin
  { Setting IsTextureLoaded property will initialize
    FTextureImage, FTextureDDS. }
  IsTextureLoaded := true;

  Result := FTextureDDS;
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

  OnWarning(wtMajor, 'VRML/X3D', Format('Loading textures from "%s" node not implemented', [NodeTypeName]));
end;

function TAbstractTexture2DNode.TextureProperties: TTexturePropertiesNode;
begin
  Result := nil;
end;

function TAbstractTexture2DNode.AlphaChannelData: TAlphaChannel;
begin
  Result := FAlphaChannelData;
end;

{ TAbstractX3DTexture2DNode ------------------------------------------------------ }

procedure TAbstractX3DTexture2DNode.CreateNode;
begin
  inherited;

  FFdRepeatS := TSFBool.Create(Self, 'repeatS', true);
   FdRepeatS.Exposed := false;
   FdRepeatS.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdRepeatS);

  FFdRepeatT := TSFBool.Create(Self, 'repeatT', true);
   FdRepeatT.Exposed := false;
   FdRepeatT.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdRepeatT);

  FFdTextureProperties := TSFNode.Create(Self, 'textureProperties', [TTexturePropertiesNode]);
   FdTextureProperties.Exposed := false;
   FdTextureProperties.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdTextureProperties);
end;

function TAbstractX3DTexture2DNode.GetRepeatS: boolean;
begin
  Result := FdRepeatS.Value;
end;

function TAbstractX3DTexture2DNode.GetRepeatT: boolean;
begin
  Result := FdRepeatT.Value;
end;

procedure TAbstractX3DTexture2DNode.SetRepeatS(const Value: boolean);
begin
  FdRepeatS.Send(Value);
end;

procedure TAbstractX3DTexture2DNode.SetRepeatT(const Value: boolean);
begin
  FdRepeatT.Send(Value);
end;

function TAbstractX3DTexture2DNode.TextureProperties: TTexturePropertiesNode;
begin
  if (FdTextureProperties.Value <> nil) and
     (FdTextureProperties.Value is TTexturePropertiesNode) then
    Result := TTexturePropertiesNode(FdTextureProperties.Value) else
    Result := nil;
end;

procedure TAbstractTextureTransformNode.CreateNode;
begin
  inherited;

  DefaultContainerField := 'textureTransform';
end;

procedure TImageTextureNode.CreateNode;
begin
  inherited;

  FFdUrl := TMFString.Create(Self, 'url', []);
   FdUrl.ChangesAlways := [chTextureImage];
  Fields.Add(FFdUrl);
  { X3D specification comment: [URI] }
end;

class function TImageTextureNode.ClassNodeTypeName: string;
begin
  Result := 'ImageTexture';
end;

class function TImageTextureNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TImageTextureNode.LoadTextureData(out WasCacheUsed: boolean);
var
  I: Integer;
  FullUrl: string;
begin
  WasCacheUsed := false;
  FTextureUsedFullUrl := '';

  for I := 0 to FdUrl.Count - 1 do
    if FdUrl.Items[I] = '' then
    begin
      { Empty URL would be expanded by PathFromBaseUrl to directory
        and produce unclear error message. }
      OnWarning(wtMinor, 'VRML/X3D', Format('Empty URL for %s ignored', [NodeTypeName]));
    end else
    begin
      FullUrl := PathFromBaseUrl(FdUrl.Items[I]);
      try
        FTextureImage := X3DCache.TextureImage_IncReference(FullUrl, FTextureDDS,
          FAlphaChannelData);
        WasCacheUsed := true;
        FTextureUsedFullUrl := URIDisplay(FullUrl);
        Break;
      except
        on E: Exception do
          { Remember that OnWarning *may* raise an exception. }
          OnWarning(wtMinor, 'Texture', Format(SLoadError,
            [E.ClassName, 'texture', URIDisplay(FullUrl), E.Message]));
      end;
    end;
end;

function TImageTextureNode.TextureDescription: string;
begin
  if TextureUsedFullUrl <> '' then
    Result := 'image from file "' + TextureUsedFullUrl + '"' else
    Result := 'none';
end;

procedure TMovieTextureNode.CreateNode;
begin
  inherited;

  FFdDescription := TSFString.Create(Self, 'description', '');
  Fields.Add(FFdDescription);

  FFdLoop := TSFBool.Create(Self, 'loop', false);
  Fields.Add(FFdLoop);

  FFdPauseTime := TSFTime.Create(Self, 'pauseTime', 0);
   FdPauseTime.ChangesAlways := [chTimeStopStart];
  Fields.Add(FFdPauseTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdResumeTime := TSFTime.Create(Self, 'resumeTime', 0);
   FdResumeTime.ChangesAlways := [chTimeStopStart];
  Fields.Add(FFdResumeTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdSpeed := TSFFloat.Create(Self, 'speed', 1.0);
  Fields.Add(FFdSpeed);
  { X3D specification comment: (-Inf,Inf) }

  FFdStartTime := TSFTimeIgnoreWhenActive.Create(Self, 'startTime', 0);
   FdStartTime.ChangesAlways := [chTimeStopStart];
  Fields.Add(FFdStartTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdStopTime := TSFStopTime.Create(Self, 'stopTime', 0);
   FdStopTime.ChangesAlways := [chTimeStopStart];
  Fields.Add(FFdStopTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdUrl := TMFString.Create(Self, 'url', []);
   FdUrl.ChangesAlways := [chTextureImage];
  Fields.Add(FFdUrl);
  { X3D specification comment: [URI] }

  FEventDuration_changed := TX3DEvent.Create(Self, 'duration_changed', TSFTime, false);
  Events.Add(FEventDuration_changed);

  FEventElapsedTime := TX3DEvent.Create(Self, 'elapsedTime', TSFTime, false);
  Events.Add(FEventElapsedTime);

  FEventIsActive := TX3DEvent.Create(Self, 'isActive', TSFBool, false);
  Events.Add(FEventIsActive);

  FEventIsPaused := TX3DEvent.Create(Self, 'isPaused', TSFBool, false);
  Events.Add(FEventIsPaused);

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

function TMovieTextureNode.GetTimeDependentNodeHandler: TTimeDependentNodeHandler;
begin
  Result := FTimeDependentNodeHandler;
end;

destructor TMovieTextureNode.Destroy;
begin
  FreeAndNil(FTimeDependentNodeHandler);
  inherited;
end;

class function TMovieTextureNode.ClassNodeTypeName: string;
begin
  Result := 'MovieTexture';
end;

class function TMovieTextureNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TMovieTextureNode.LoadTextureData(out WasCacheUsed: boolean);
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
      FTextureVideo := X3DCache.Video_IncReference(FullUrl, FAlphaChannelData);

      { if loading succeded, set WasCacheUsed and others and break. }
      WasCacheUsed := true;
      FTextureUsedFullUrl := FullUrl;
      FDuration := FTextureVideo.TimeDuration;
      if Scene <> nil then
        EventDuration_Changed.Send(FDuration, Scene.GetTime);
      Break;
    except
      on E: Exception do
        { Remember that OnWarning *may* raise an exception. }
        OnWarning(wtMinor, 'Video', Format(SLoadError,
          [E.ClassName, 'video', URIDisplay(FullUrl), E.Message]));
    end;
  end;
end;

function TMovieTextureNode.CycleInterval: TFloatTime;
begin
  if FdSpeed.Value <> 0 then
    Result := Duration / Abs(FdSpeed.Value) else
    Result := 0;
end;

function TMovieTextureNode.TextureDescription: string;
begin
  if TextureUsedFullUrl <> '' then
    Result := 'video from file "' + TextureUsedFullUrl + '"' else
    Result := 'none';
end;

procedure TMultiTextureNode.CreateNode;
begin
  inherited;

  FFdAlpha := TSFFloat.Create(Self, 'alpha', 1);
   FdAlpha.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdAlpha);
  { X3D specification comment: [0,1] }

  FFdColor := TSFColor.Create(Self, 'color', Vector3Single(1, 1, 1));
   FdColor.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdColor);
  { X3D specification comment: [0,1] }

  FFdFunction := TMFString.Create(Self, 'function', []);
   FdFunction.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdFunction);

  FFdMode := TMFString.Create(Self, 'mode', []);
   FdMode.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdMode);

  FFdSource := TMFString.Create(Self, 'source', []);
   FdSource.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdSource);

  FFdTexture := TMFNode.Create(Self, 'texture', [TAbstractTextureNode]);
   FdTexture.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdTexture);
end;

class function TMultiTextureNode.ClassNodeTypeName: string;
begin
  Result := 'MultiTexture';
end;

class function TMultiTextureNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
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
       (ChildTex is TAbstractTextureNode) then
      AlphaMaxTo1st(Result, TAbstractTextureNode(ChildTex).AlphaChannel);
  end;
end;

procedure TMultiTextureCoordinateNode.CreateNode;
begin
  inherited;

  FFdTexCoord := TMFNode.Create(Self, 'texCoord', [TAbstractTextureCoordinateNode]);
   FdTexCoord.ChangesAlways := [chTextureCoordinate];
  Fields.Add(FFdTexCoord);
end;

class function TMultiTextureCoordinateNode.ClassNodeTypeName: string;
begin
  Result := 'MultiTextureCoordinate';
end;

class function TMultiTextureCoordinateNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TMultiTextureTransformNode.CreateNode;
begin
  inherited;

  FFdTextureTransform := TMFNode.Create(Self, 'textureTransform', [TAbstractTextureTransformNode]);
   FdTextureTransform.ChangesAlways := [chEverything];
  Fields.Add(FFdTextureTransform);
end;

class function TMultiTextureTransformNode.ClassNodeTypeName: string;
begin
  Result := 'MultiTextureTransform';
end;

class function TMultiTextureTransformNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TMultiTextureTransformNode.TransformMatrix: TMatrix4Single;
begin
  raise EInternalError.Create('You cannot get single TransformMatrix from MultiTextureTransform node');
  Result := IdentityMatrix4Single; { avoid warnings that result not set }
end;

procedure TPixelTextureNode.CreateNode;
begin
  inherited;

  FFdImage := TSFImage.Create(Self, 'image', nil);
   FdImage.ChangesAlways := [chTextureImage];
  Fields.Add(FFdImage);
  { X3D specification comment: 0 0 }
  end;

class function TPixelTextureNode.ClassNodeTypeName: string;
begin
  Result := 'PixelTexture';
end;

class function TPixelTextureNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TPixelTextureNode.LoadTextureData(out WasCacheUsed: boolean);
begin
  WasCacheUsed := false;

  if not FdImage.Value.IsEmpty then
    FTextureImage := FdImage.Value.MakeCopy;
end;

function TPixelTextureNode.TextureDescription: string;
begin
  if not FdImage.Value.IsEmpty then
    result := Format('inlined image (width = %d; height = %d; with alpha = %s)',
      [ FdImage.Value.Width, FdImage.Value.Height,
        BoolToStr[FdImage.Value.HasAlpha] ]) else
    result := 'none';
end;

procedure TTextureCoordinateNode.CreateNode;
begin
  inherited;

  FFdPoint := TMFVec2f.Create(Self, 'point', []);
   FdPoint.ChangesAlways := [chTextureCoordinate];
  Fields.Add(FFdPoint);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TTextureCoordinateNode.ClassNodeTypeName: string;
begin
  Result := 'TextureCoordinate';
end;

class function TTextureCoordinateNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TTextureCoordinateGeneratorNode.CreateNode;
begin
  inherited;

  FFdMode := TSFString.Create(Self, 'mode', 'SPHERE');
   FdMode.ChangesAlways := [chTextureCoordinate];
  Fields.Add(FFdMode);
  { X3D specification comment: [see Table 18.6] }

  FFdParameter := TMFFloat.Create(Self, 'parameter', []);
   FdParameter.ChangesAlways := [chTextureCoordinate];
  Fields.Add(FFdParameter);
  { X3D specification comment: [see Table 18.6] }

  { Note that projectedLight node is not enumerated as an active node
    for traversing (in DirectEnumerateActive), because the light doesn't
    shine here. We don't want
    to override it's transform with transformation of this
    TextureCoordinateGenerator. }
  FFdProjectedLight := TSFNode.Create(Self, 'projectedLight',
    [TSpotLightNode_1, TDirectionalLightNode_1,
     TSpotLightNode  , TDirectionalLightNode  ]);
   FdProjectedLight.ChangesAlways := [chTextureCoordinate];
  Fields.Add(FFdProjectedLight);
end;

class function TTextureCoordinateGeneratorNode.ClassNodeTypeName: string;
begin
  Result := 'TextureCoordinateGenerator';
end;

class function TTextureCoordinateGeneratorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TTextureCoordGenNode.ClassNodeTypeName: string;
begin
  Result := 'TextureCoordGen';
end;

procedure TTexturePropertiesNode.CreateNode;
begin
  inherited;

  FFdAnisotropicDegree := TSFFloat.Create(Self, 'anisotropicDegree', 1.0);
   FdAnisotropicDegree.ChangesAlways := [chTexturePropertiesNode];
  Fields.Add(FFdAnisotropicDegree);
  { X3D specification comment: [1,Inf) }

  FFdBorderColor := TSFColorRGBA.Create(Self, 'borderColor', Vector4Single(0, 0, 0, 0));
   FdBorderColor.ChangesAlways := [chTexturePropertiesNode];
  Fields.Add(FFdBorderColor);
  { X3D specification comment: [0,1] }

  FFdBorderWidth := TSFInt32.Create(Self, 'borderWidth', 0);
   FdBorderWidth.ChangesAlways := [chTexturePropertiesNode];
  Fields.Add(FFdBorderWidth);
  { X3D specification comment: [0,1] }

  FFdBoundaryModeS := TSFString.Create(Self, 'boundaryModeS', 'REPEAT');
   FdBoundaryModeS.ChangesAlways := [chTexturePropertiesNode];
  Fields.Add(FFdBoundaryModeS);
  { X3D specification comment: [see Table 18.7] }

  FFdBoundaryModeT := TSFString.Create(Self, 'boundaryModeT', 'REPEAT');
   FdBoundaryModeT.ChangesAlways := [chTexturePropertiesNode];
  Fields.Add(FFdBoundaryModeT);
  { X3D specification comment: [see Table 18.7] }

  FFdBoundaryModeR := TSFString.Create(Self, 'boundaryModeR', 'REPEAT');
   FdBoundaryModeR.ChangesAlways := [chTexturePropertiesNode];
  Fields.Add(FFdBoundaryModeR);
  { X3D specification comment: [see Table 18.7] }

  FFdMagnificationFilter := TSFString.Create(Self, 'magnificationFilter', 'FASTEST');
   FdMagnificationFilter.ChangesAlways := [chTexturePropertiesNode];
  Fields.Add(FFdMagnificationFilter);
  { X3D specification comment: [see Table 18.8] }

  FFdMinificationFilter := TSFString.Create(Self, 'minificationFilter', 'FASTEST');
   FdMinificationFilter.ChangesAlways := [chTexturePropertiesNode];
  Fields.Add(FFdMinificationFilter);
  { X3D specification comment: [see Table 18.9] }

  FFdTextureCompression := TSFString.Create(Self, 'textureCompression', 'FASTEST');
   FdTextureCompression.ChangesAlways := [chTexturePropertiesNode];
  Fields.Add(FFdTextureCompression);
  { X3D specification comment: [see Table 18.10] }

  FFdTexturePriority := TSFFloat.Create(Self, 'texturePriority', 0);
   FdTexturePriority.ChangesAlways := [chTexturePropertiesNode];
  Fields.Add(FFdTexturePriority);
  { X3D specification comment: [0,1] }

  FFdGenerateMipMaps := TSFBool.Create(Self, 'generateMipMaps', false);
   FdGenerateMipMaps.Exposed := false;
   FdGenerateMipMaps.ChangesAlways := [chTexturePropertiesNode];
  Fields.Add(FFdGenerateMipMaps);

  { X3D XML spec (edition 2) mistakenly claims it should be
    "lineProperties", which is nonsense... I set this to "textureProperties". }
  DefaultContainerField := 'textureProperties';
end;

class function TTexturePropertiesNode.ClassNodeTypeName: string;
begin
  Result := 'TextureProperties';
end;

class function TTexturePropertiesNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TTextureTransformNode.CreateNode;
begin
  inherited;

  FFdCenter := TSFVec2f.Create(Self, 'center', Vector2Single(0, 0));
   FdCenter.ChangesAlways := [chTextureTransform];
  Fields.Add(FFdCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdRotation := TSFFloat.Create(Self, 'rotation', 0);
   FFdrotation.Angle := true;
   FFdrotation.ChangesAlways := [chTextureTransform];
  Fields.Add(FFdRotation);
  { X3D specification comment: (-Inf,Inf) }

  FFdScale := TSFVec2f.Create(Self, 'scale', Vector2Single(1, 1));
   FdScale.ChangesAlways := [chTextureTransform];
  Fields.Add(FFdScale);
  { X3D specification comment: (-Inf,Inf) }

  FFdTranslation := TSFVec2f.Create(Self, 'translation', Vector2Single(0, 0));
   FdTranslation.ChangesAlways := [chTextureTransform];
  Fields.Add(FFdTranslation);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TTextureTransformNode.ClassNodeTypeName: string;
begin
  Result := 'TextureTransform';
end;

class function TTextureTransformNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TTextureTransformNode.TransformMatrix: TMatrix4Single;
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
    TImageTextureNode,
    TMovieTextureNode,
    TMultiTextureNode,
    TMultiTextureCoordinateNode,
    TMultiTextureTransformNode,
    TPixelTextureNode,
    TTextureCoordinateNode,
    TTextureCoordinateGeneratorNode,
    TTextureCoordGenNode,
    TTexturePropertiesNode,
    TTextureTransformNode
  ]);
end;

{$endif read_implementation}
