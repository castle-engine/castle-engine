{
  Copyright 2002-2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{$ifdef read_interface}
  { }
  TNodeX3DTextureCoordinateNode = class(TNodeX3DGeometricPropertyNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
  end;

  TNodeX3DTextureNode = class(TNodeX3DAppearanceChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
  end;

  TNodeTextureProperties = class;

  { Common texture node for all VRML / X3D texture nodes.

    For X3D, this descends from X3DTextureNode and is an ancestor
    for X3DTexture2DNode, so X3D hierarchy is nicely preserved. }
  TVRMLTextureNode = class(TNodeX3DTextureNode)
  private
    { Together we call FTextureImage and FTextureVideo as "texture data". }

    { FTextureImage is <> nil if texture is currently loaded (IsTextureLoaded)
      and it was loaded to an image (not video).

      Note that this may still have zero size (IsNull = @true),
      IsTextureImage checks this also. }
    FTextureImage: TImage;

    { Analogous to FTextureImage, this is the loaded video file.
      Assigned here, should always have TVideo.Loaded = @true. }
    FTextureVideo: TVideo;

    { Non-nil only if FTextureImage or FTextueVideo should be freed using
      TextureDataUsedCache (LoadImage_DecReference or Video_DecReference).
      Also loaded FTextureVideo should always have it's own Cache property set
      to this. }
    TextureDataUsedCache: TImagesVideosCache;

    FIsTextureLoaded: boolean;
    procedure SetIsTextureLoaded(Value: boolean);

    FImagesVideosCache: TImagesVideosCache;

    procedure FreeAndNilTextureData;
  protected
    { This loads texture data, actual image or video file.

      It should set either FTextureImage or FTextureVideo to something non-nil
      (don't care here about the previous value of these fields --- it's for
      sure @nil). If you leave them as @nil, this means that loading failed
      (and VRMLNonFatalError or DataNonFatalError don't cause an exception,
      so we can ignore this).

      You do not care in this method about things like
      IsImageLoaded --- this method should just always,
      unconditionally, make everything it can do to load texture data from
      file(s).

      You can use VRMLNonFatalError or DataNonFatalError inside,
      so we're prepared that this may even exit with exception
      (since VRML/DataNonFatalError can raise exception).

      If you set FTextureImage, you have to set it to TRGBImage or TAlphaImage,
      see TextureImage docs.

      Set CacheUsed here. @true means you loaded the data using ImagesVideosCache.
      For FTextureVideo, it's Cache property should also be set to
      ImagesVideosCache, this happens automatically in Video_IncReference.

      Also, set FTextureUsedFullUrl here.

      In this class, this simply produces VRMLNonFatalError with
      "not implemented" message and returns @nil. It's not declared
      as abstract, because there may be classes descending from this,
      and we want to at least be able to parse them
      and then ignore (while not overriden abstract method would cause
      simple crashes). }
    procedure LoadTextureData(out CacheUsed: boolean); virtual;
    FTextureUsedFullUrl: string;
  public
    constructor Create(const ANodeName: string;
      const AWWWBasePath: string); override;
    destructor Destroy; override;

    { TextureImage and TextureVideo contain actual texture data.
      TextureImage may come from inline VRML texture or could be loaded
      from file (including from some URL), this doesn't concern you here.

      Calls to TextureImage, TextureVideo, IsTextureImage, IsTextureVideo
      will automatically load the data, so in simple situations you really
      don't need to do anything. Just check and use them when you want,
      and things will just work. See IsTextureLoaded for more control
      about loading / unloading.

      Be sure to set ImagesVideosCache before loading the texture data.

      Note that either one of TextureImage or TextureVideo may be @nil,
      if the other one is loaded. Or when loading failed with non-fatal
      error (reported by VRMLNonFatalError and eventually DataNonFatalError).
      IsTextureImage checks that TextureImage is non-nil (so it's successfully
      loaded) and additionally that texture size is not zero.
      Similar for IsTextureVideo.

      TextureImage class is always in (TRGBImage, TAlphaImage)
      simply because only these formats are accepted by GLImages.
      TODO: this is not nice, we do not want any OpenGL dependency in this unit,
      even such "ideological" one.

      @groupBegin }
    function TextureImage: TImage;
    function IsTextureImage: boolean;
    function TextureVideo: TVideo;
    function IsTextureVideo: boolean;
    { @groupEnd }

    { This says whether the texture data is already loaded.
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
      TextureImage / TextureVideo anymore).
      You can also set it to @false and then back to @true if you want to
      request reloading the texture from URL (e.g. if you suspect that
      the URL contents changed).

      Note that IsTextureLoaded is set to @true, even if actual loading
      failed. You still have to check afterwards IsTextureImage and
      IsTextureVideo to know if loading was actually successfull.
      This is deliberate --- it means that each call to TextureImage etc.
      will not unnecessarily read the disk (or even connect to internet)
      when the file does not exist. Also, the loading errors reported
      by VRML/DataNonFatalError will not be repeated --- they will
      occur only once, when IsTextureLoaded changes from @false to @true. }
    property IsTextureLoaded: boolean
      read FIsTextureLoaded write SetIsTextureLoaded;

    { TImagesVideosCache instance used to load images and videos.
      Be sure to set ImagesVideosCache before loading the texture.
      TODO: allow ImagesVideosCache to be @nil. }
    property ImagesVideosCache: TImagesVideosCache
      read FImagesVideosCache write FImagesVideosCache;

    { Short description how texture is defined, is it inline or loaded
      from URL, is it video of simple image texture.
      "none" if it's not defined at all.

      This doesn't specify what texture is actually loaded
      (besides from which URL it was loaded, if any).
      The idea is that calling this doesn't cause automatic
      loading of the texture data.

      In this class, simply returns NodeTypeName and informs it's
      not implemented. You most definitely
      want to override this and say something more descriptive. }
    function TextureDescription: string; virtual;

    function RepeatS: boolean; virtual; abstract;
    function RepeatT: boolean; virtual; abstract;

    { Once the texture data (image or video) is loaded,
      this is set to the URL that was used to load,
      or '' if no URL was used. "No URL was used" may mean that
      no URL was valid, or inlined image was used.

      This is always a full, expanded (i.e. not relative) URL. }
    property TextureUsedFullUrl: string read FTextureUsedFullUrl;

    { Returns TextureProperties node, if this node has some "textureProperties"
      field and it specifies TextureProperties node. Otherwise returns @nil. }
    function TextureProperties: TNodeTextureProperties; virtual;
  end;

  TNodeX3DTexture2DNode = class(TVRMLTextureNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdrepeatS: TSFBool;
    public property FdrepeatS: TSFBool read FFdrepeatS;

    private FFdrepeatT: TSFBool;
    public property FdrepeatT: TSFBool read FFdrepeatT;

    private FFdtextureProperties: TSFNode;
    public property FdtextureProperties: TSFNode read FFdtextureProperties;

    function RepeatS: boolean; override;
    function RepeatT: boolean; override;

    function TextureProperties: TNodeTextureProperties; override;
  end;

  TNodeX3DTextureTransformNode = class(TNodeX3DAppearanceChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    function TransformMatrix: TMatrix4Single; virtual; abstract;
  end;

  TNodeImageTexture = class(TNodeX3DTexture2DNode, INodeX3DUrlObject)
  protected
    procedure LoadTextureData(out CacheUsed: boolean); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

    function TextureDescription: string; override;
  end;

  TNodeMovieTexture = class(TNodeX3DTexture2DNode, INodeX3DSoundSourceNode, INodeX3DUrlObject)
  private
    FDuration: TKamTime;
    FTimeDependentNodeHandler: TTimeDependentNodeHandler;
    function CycleInterval: TKamTime;
  protected
    procedure LoadTextureData(out CacheUsed: boolean); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    destructor Destroy; override;

    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFddescription: TSFString;
    public property Fddescription: TSFString read FFddescription;

    private FFdloop: TSFBool;
    public property Fdloop: TSFBool read FFdloop;

    private FFdpauseTime: TSFTime;
    public property FdpauseTime: TSFTime read FFdpauseTime;

    private FFdresumeTime: TSFTime;
    public property FdresumeTime: TSFTime read FFdresumeTime;

    private FFdspeed: TSFFloat;
    public property Fdspeed: TSFFloat read FFdspeed;

    private FFdstartTime: TSFTime;
    public property FdstartTime: TSFTime read FFdstartTime;

    private FFdstopTime: TSFTime;
    public property FdstopTime: TSFTime read FFdstopTime;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

    { Event: SFTime, out } { }
    private FEventduration_changed: TVRMLEvent;
    public property Eventduration_changed: TVRMLEvent read FEventduration_changed;

    { Event: SFTime, out } { }
    private FEventelapsedTime: TVRMLEvent;
    public property EventelapsedTime: TVRMLEvent read FEventelapsedTime;

    { Event: SFBool, out } { }
    private FEventisActive: TVRMLEvent;
    public property EventisActive: TVRMLEvent read FEventisActive;

    { Event: SFBool, out } { }
    private FEventisPaused: TVRMLEvent;
    public property EventisPaused: TVRMLEvent read FEventisPaused;

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
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdalpha: TSFFloat;
    public property Fdalpha: TSFFloat read FFdalpha;

    private FFdcolor: TSFColor;
    public property Fdcolor: TSFColor read FFdcolor;

    private FFdfunction: TMFString;
    public property Fdfunction: TMFString read FFdfunction;

    private FFdmode: TMFString;
    public property Fdmode: TMFString read FFdmode;

    private FFdsource: TMFString;
    public property Fdsource: TMFString read FFdsource;

    private FFdtexture: TMFNode;
    public property Fdtexture: TMFNode read FFdtexture;
  end;

  TNodeMultiTextureCoordinate = class(TNodeX3DTextureCoordinateNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdtexCoord: TMFNode;
    public property FdtexCoord: TMFNode read FFdtexCoord;
  end;

  TNodeMultiTextureTransform = class(TNodeX3DTextureTransformNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdtextureTransform: TMFNode;
    public property FdtextureTransform: TMFNode read FFdtextureTransform;

    function TransformMatrix: TMatrix4Single; override;
  end;

  TNodePixelTexture = class(TNodeX3DTexture2DNode)
  protected
    procedure LoadTextureData(out CacheUsed: boolean); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdimage: TSFImage;
    public property Fdimage: TSFImage read FFdimage;

    function TextureDescription: string; override;
  end;

  TNodeTextureCoordinate = class(TNodeX3DTextureCoordinateNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdpoint: TMFVec2f;
    public property Fdpoint: TMFVec2f read FFdpoint;
  end;

  TNodeTextureCoordinateGenerator = class(TNodeX3DTextureCoordinateNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdmode: TSFString;
    public property Fdmode: TSFString read FFdmode;

    private FFdparameter: TMFFloat;
    public property Fdparameter: TMFFloat read FFdparameter;
  end;

  TNodeTextureProperties = class(TNodeX3DNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdanisotropicDegree: TSFFloat;
    public property FdanisotropicDegree: TSFFloat read FFdanisotropicDegree;

    private FFdborderColor: TSFColorRGBA;
    public property FdborderColor: TSFColorRGBA read FFdborderColor;

    private FFdborderWidth: TSFInt32;
    public property FdborderWidth: TSFInt32 read FFdborderWidth;

    private FFdboundaryModeS: TSFString;
    public property FdboundaryModeS: TSFString read FFdboundaryModeS;

    private FFdboundaryModeT: TSFString;
    public property FdboundaryModeT: TSFString read FFdboundaryModeT;

    private FFdboundaryModeR: TSFString;
    public property FdboundaryModeR: TSFString read FFdboundaryModeR;

    private FFdmagnificationFilter: TSFString;
    public property FdmagnificationFilter: TSFString read FFdmagnificationFilter;

    private FFdminificationFilter: TSFString;
    public property FdminificationFilter: TSFString read FFdminificationFilter;

    private FFdtextureCompression: TSFString;
    public property FdtextureCompression: TSFString read FFdtextureCompression;

    private FFdtexturePriority: TSFFloat;
    public property FdtexturePriority: TSFFloat read FFdtexturePriority;

    private FFdgenerateMipMaps: TSFBool;
    public property FdgenerateMipMaps: TSFBool read FFdgenerateMipMaps;
  end;

  TNodeTextureTransform = class(TNodeX3DTextureTransformNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcenter: TSFVec2f;
    public property Fdcenter: TSFVec2f read FFdcenter;

    private FFdrotation: TSFFloat;
    public property Fdrotation: TSFFloat read FFdrotation;

    private FFdscale: TSFVec2f;
    public property Fdscale: TSFVec2f read FFdscale;

    private FFdtranslation: TSFVec2f;
    public property Fdtranslation: TSFVec2f read FFdtranslation;

    function TransformMatrix: TMatrix4Single; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TNodeX3DTextureCoordinateNode ---------------------------------------------- }

constructor TNodeX3DTextureCoordinateNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  { X3D XML spec doesn't specify containerField for abstract X3D classes.
    texCoord seems most sensible for this case. }
  DefaultContainerField := 'texCoord';
end;

{ TNodeX3DTextureNode -------------------------------------------------------- }

constructor TNodeX3DTextureNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'texture';
end;

{ TVRMLTextureNode ----------------------------------------------------------- }

constructor TVRMLTextureNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;
  TextureDataUsedCache := nil;
  FIsTextureLoaded := false;
end;

destructor TVRMLTextureNode.Destroy;
begin
  FreeAndNilTextureData;
  inherited;
end;

procedure TVRMLTextureNode.FreeAndNilTextureData;
begin
  if FTextureImage <> nil then
  begin
    if TextureDataUsedCache <> nil then
    begin
      TextureDataUsedCache.LoadImage_DecReference(FTextureImage);
      TextureDataUsedCache := nil;
    end else
      FreeAndNil(FTextureImage);
  end;

  if FTextureVideo <> nil then
  begin
    if TextureDataUsedCache <> nil then
    begin
      TextureDataUsedCache.Video_DecReference(FTextureVideo);
      TextureDataUsedCache := nil;
    end else
      FreeAndNil(FTextureVideo);
  end;
end;

function TVRMLTextureNode.TextureImage: TImage;
begin
  { Setting IsTextureLoaded property will initialize FTextureImage. }
  IsTextureLoaded := true;

  Result := FTextureImage;
end;

function TVRMLTextureNode.IsTextureImage: boolean;
begin
  Result := (TextureImage <> nil) and (not TextureImage.IsNull);
end;

function TVRMLTextureNode.TextureVideo: TVideo;
begin
  { Setting IsTextureLoaded property will initialize FTextureVideo. }
  IsTextureLoaded := true;

  Result := FTextureVideo;
end;

function TVRMLTextureNode.IsTextureVideo: boolean;
begin
  Result := (TextureVideo <> nil) and
    (TextureVideo.Width <> 0) and
    (TextureVideo.Height <> 0);
end;

procedure TVRMLTextureNode.SetIsTextureLoaded(Value: boolean);

  procedure DoLoadTexture;
  var
    UsedCache: boolean;
  begin
    Assert(ImagesVideosCache <> nil, 'ImagesVideosCache must be set before loading the texture');

    FreeAndNilTextureData;

    LoadTextureData(UsedCache);
    if UsedCache then
      TextureDataUsedCache := ImagesVideosCache;
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

procedure TVRMLTextureNode.LoadTextureData(out CacheUsed: boolean);
begin
  CacheUsed := false;
  FTextureUsedFullUrl := '';

  VRMLNonFatalError(Format('Loading textures from "%s" node not implemented', [NodeTypeName]));
end;

function TVRMLTextureNode.TextureDescription: string;
begin
  Result := Format('Texture type "%s" (not implemented)', [NodeTypeName]);
end;

function TVRMLTextureNode.TextureProperties: TNodeTextureProperties;
begin
  Result := nil;
end;

{ TNodeX3DTexture2DNode ------------------------------------------------------ }

constructor TNodeX3DTexture2DNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdrepeatS := TSFBool.Create(Self, 'repeatS', true);
  FFdrepeatS.Exposed := false;
  Fields.Add(FFdrepeatS);

  FFdrepeatT := TSFBool.Create(Self, 'repeatT', true);
  FFdrepeatT.Exposed := false;
  Fields.Add(FFdrepeatT);

  FFdtextureProperties := TSFNode.Create(Self, 'textureProperties', [TNodeTextureProperties]);
  FFdtextureProperties.Exposed := false;
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

constructor TNodeX3DTextureTransformNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'textureTransform';
end;

constructor TNodeImageTexture.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdurl := TMFString.Create(Self, 'url', []);
  Fields.Add(FFdurl);
  { X3D specification comment: [URI] }
end;

class function TNodeImageTexture.ClassNodeTypeName: string;
begin
  Result := 'ImageTexture';
end;

function TNodeImageTexture.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeImageTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeImageTexture.LoadTextureData(out CacheUsed: boolean);
var
  I: Integer;
  FullUrl: string;
begin
  CacheUsed := false;
  FTextureUsedFullUrl := '';

  for I := 0 to FdUrl.Count - 1 do
    if FdUrl.Items[I] = '' then
    begin
      { Empty URL would be expanded by PathFromWWWBasePath to directory
        and produce unclear error message. }
      VRMLNonFatalError(Format('Empty URL for %s ignored', [NodeTypeName]));
    end else
    begin
      FullUrl := PathFromWWWBasePath(FdUrl.Items[I]);
      try
        FTextureImage := ImagesVideosCache.LoadImage_IncReference(FullUrl);
        CacheUsed := true;
        FTextureUsedFullUrl := FullUrl;
        Break;
      except
        on E: Exception do
          { pamietajmy ze VRMLNonFatalError moze spowodowac rzucenie wyjatku
            (chociaz nie musi) }
          VRMLNonFatalError('Exception ' + E.ClassName +
            ' occured when trying to load ' +
            'texture from filename "' + FullUrl + '" : ' + E.Message);
      end;
    end;
end;

function TNodeImageTexture.TextureDescription: string;
begin
  if TextureUsedFullUrl <> '' then
    Result := 'image from file "' + TextureUsedFullUrl + '"' else
    Result := 'none';
end;

constructor TNodeMovieTexture.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFddescription := TSFString.Create(Self, 'description', '');
  Fields.Add(FFddescription);

  FFdloop := TSFBool.Create(Self, 'loop', false);
  Fields.Add(FFdloop);

  FFdpauseTime := TSFTime.Create(Self, 'pauseTime', 0);
  Fields.Add(FFdpauseTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdresumeTime := TSFTime.Create(Self, 'resumeTime', 0);
  Fields.Add(FFdresumeTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdspeed := TSFFloat.Create(Self, 'speed', 1.0);
  Fields.Add(FFdspeed);
  { X3D specification comment: (-Inf,Inf) }

  FFdstartTime := TSFTime.Create(Self, 'startTime', 0);
  Fields.Add(FFdstartTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdstopTime := TSFTime.Create(Self, 'stopTime', 0);
  Fields.Add(FFdstopTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdurl := TMFString.Create(Self, 'url', []);
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
  FTimeDependentNodeHandler.Fdloop := Fdloop;
  FTimeDependentNodeHandler.FdpauseTime := FdpauseTime;
  FTimeDependentNodeHandler.FdresumeTime := FdresumeTime;
  FTimeDependentNodeHandler.FdstartTime := FdstartTime;
  FTimeDependentNodeHandler.FdstopTime := FdstopTime;
  FTimeDependentNodeHandler.EventisActive:= EventisActive;
  FTimeDependentNodeHandler.EventisPaused := EventisPaused;
  FTimeDependentNodeHandler.EventelapsedTime := EventelapsedTime;
  FTimeDependentNodeHandler.OnCycleInterval := @CycleInterval;
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

function TNodeMovieTexture.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeMovieTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeMovieTexture.LoadTextureData(out CacheUsed: boolean);
var
  I: Integer;
  FullUrl: string;
begin
  CacheUsed := true;
  FTextureUsedFullUrl := '';

  for I := 0 to FdUrl.Count - 1 do
  begin
    FullUrl := PathFromWWWBasePath(FdUrl.Items[I]);
    try
      FTextureVideo := ImagesVideosCache.Video_IncReference(FullUrl);

      { it loading succeded, set CacheUsed and others and break. }
      CacheUsed := true;
      FTextureUsedFullUrl := FullUrl;
      FDuration := FTextureVideo.TimeDuration;
      if ParentEventsProcessor <> nil then
        EventDuration_Changed.Send(FDuration,
          (ParentEventsProcessor as TVRMLScene).WorldTime);
      Break;
    except
      on E: Exception do
        { pamietajmy ze VRMLNonFatalError moze spowodowac rzucenie wyjatku
          (chociaz nie musi) }
        VRMLNonFatalError('Exception ' + E.ClassName +
          ' occured when trying to load ' +
          'video from filename "' + FullUrl + '" : ' + E.Message);
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

constructor TNodeMultiTexture.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdalpha := TSFFloat.Create(Self, 'alpha', 1);
  Fields.Add(FFdalpha);
  { X3D specification comment: [0,1] }

  FFdcolor := TSFColor.Create(Self, 'color', Vector3Single(1, 1, 1));
  Fields.Add(FFdcolor);
  { X3D specification comment: [0,1] }

  FFdfunction := TMFString.Create(Self, 'function', []);
  Fields.Add(FFdfunction);

  FFdmode := TMFString.Create(Self, 'mode', []);
  Fields.Add(FFdmode);

  FFdsource := TMFString.Create(Self, 'source', []);
  Fields.Add(FFdsource);

  FFdtexture := TMFNode.Create(Self, 'texture', [TNodeX3DTextureNode]);
  Fields.Add(FFdtexture);
end;

class function TNodeMultiTexture.ClassNodeTypeName: string;
begin
  Result := 'MultiTexture';
end;

function TNodeMultiTexture.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeMultiTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeMultiTextureCoordinate.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdtexCoord := TMFNode.Create(Self, 'texCoord', [TNodeX3DTextureCoordinateNode]);
  Fields.Add(FFdtexCoord);
end;

class function TNodeMultiTextureCoordinate.ClassNodeTypeName: string;
begin
  Result := 'MultiTextureCoordinate';
end;

function TNodeMultiTextureCoordinate.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeMultiTextureCoordinate.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeMultiTextureTransform.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdtextureTransform := TMFNode.Create(Self, 'textureTransform', [TNodeX3DTextureTransformNode]);
  Fields.Add(FFdtextureTransform);
end;

class function TNodeMultiTextureTransform.ClassNodeTypeName: string;
begin
  Result := 'MultiTextureTransform';
end;

function TNodeMultiTextureTransform.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeMultiTextureTransform.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TNodeMultiTextureTransform.TransformMatrix: TMatrix4Single;
var
  I: Integer;
begin
  { Since we don't support multitexturing yet, this simply returns
    the first valid texture transform item. }
  for I := 0 to FdtextureTransform.Count - 1 do
    if (FdtextureTransform.Items[I] <> nil) and
       (FdtextureTransform.Items[I] is TNodeX3DTextureTransformNode) then
    begin
      Result := TNodeX3DTextureTransformNode(FdtextureTransform.Items[I]).TransformMatrix;
      Exit;
    end;
  VRMLNonFatalError('No valid nodes on MultiTextureTransform.textureTransform, assuming no transform');
  Result := IdentityMatrix4Single;
end;

constructor TNodePixelTexture.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdimage := TSFImage.Create(Self, 'image', nil);
  Fields.Add(FFdimage);
  { X3D specification comment: 0 0 }
end;

class function TNodePixelTexture.ClassNodeTypeName: string;
begin
  Result := 'PixelTexture';
end;

function TNodePixelTexture.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodePixelTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodePixelTexture.LoadTextureData(out CacheUsed: boolean);
begin
  CacheUsed := false;

  if not FdImage.Value.IsNull then
    FTextureImage := FdImage.Value.MakeCopy;
end;

function TNodePixelTexture.TextureDescription: string;
begin
  if not FdImage.Value.IsNull then
    result := Format('inlined image (width = %d; height = %d; with alpha = %s)',
      [ FdImage.Value.Width, FdImage.Value.Height,
        BoolToStr[FdImage.Value is TAlphaImage] ]) else
    result := 'none';
end;

constructor TNodeTextureCoordinate.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdpoint := TMFVec2f.Create(Self, 'point', []);
  Fields.Add(FFdpoint);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TNodeTextureCoordinate.ClassNodeTypeName: string;
begin
  Result := 'TextureCoordinate';
end;

function TNodeTextureCoordinate.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeTextureCoordinate.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeTextureCoordinateGenerator.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdmode := TSFString.Create(Self, 'mode', 'SPHERE');
  Fields.Add(FFdmode);
  { X3D specification comment: [see Table 18.6] }

  FFdparameter := TMFFloat.Create(Self, 'parameter', []);
  Fields.Add(FFdparameter);
  { X3D specification comment: [see Table 18.6] }
end;

class function TNodeTextureCoordinateGenerator.ClassNodeTypeName: string;
begin
  Result := 'TextureCoordinateGenerator';
end;

function TNodeTextureCoordinateGenerator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeTextureCoordinateGenerator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeTextureProperties.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdanisotropicDegree := TSFFloat.Create(Self, 'anisotropicDegree', 1.0);
  Fields.Add(FFdanisotropicDegree);
  { X3D specification comment: [1,Inf) }

  FFdborderColor := TSFColorRGBA.Create(Self, 'borderColor', Vector4Single(0, 0, 0, 0));
  Fields.Add(FFdborderColor);
  { X3D specification comment: [0,1] }

  FFdborderWidth := TSFInt32.Create(Self, 'borderWidth', 0);
  Fields.Add(FFdborderWidth);
  { X3D specification comment: [0,1] }

  FFdboundaryModeS := TSFString.Create(Self, 'boundaryModeS', 'REPEAT');
  Fields.Add(FFdboundaryModeS);
  { X3D specification comment: [see Table 18.7] }

  FFdboundaryModeT := TSFString.Create(Self, 'boundaryModeT', 'REPEAT');
  Fields.Add(FFdboundaryModeT);
  { X3D specification comment: [see Table 18.7] }

  FFdboundaryModeR := TSFString.Create(Self, 'boundaryModeR', 'REPEAT');
  Fields.Add(FFdboundaryModeR);
  { X3D specification comment: [see Table 18.7] }

  FFdmagnificationFilter := TSFString.Create(Self, 'magnificationFilter', 'FASTEST');
  Fields.Add(FFdmagnificationFilter);
  { X3D specification comment: [see Table 18.8] }

  FFdminificationFilter := TSFString.Create(Self, 'minificationFilter', 'FASTEST');
  Fields.Add(FFdminificationFilter);
  { X3D specification comment: [see Table 18.9] }

  FFdtextureCompression := TSFString.Create(Self, 'textureCompression', 'FASTEST');
  Fields.Add(FFdtextureCompression);
  { X3D specification comment: [see Table 18.10] }

  FFdtexturePriority := TSFFloat.Create(Self, 'texturePriority', 0);
  Fields.Add(FFdtexturePriority);
  { X3D specification comment: [0,1] }

  FFdgenerateMipMaps := TSFBool.Create(Self, 'generateMipMaps', false);
  FFdgenerateMipMaps.Exposed := false;
  Fields.Add(FFdgenerateMipMaps);

  { X3D XML spec (edition 2) mistakenyl claims it should be
    "lineProperties", which is nonsense... I set this to "textureProperties". }
  DefaultContainerField := 'textureProperties';
end;

class function TNodeTextureProperties.ClassNodeTypeName: string;
begin
  Result := 'TextureProperties';
end;

function TNodeTextureProperties.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeTextureProperties.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeTextureTransform.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcenter := TSFVec2f.Create(Self, 'center', Vector2Single(0, 0));
  Fields.Add(FFdcenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdrotation := TSFFloat.Create(Self, 'rotation', 0);
  Fields.Add(FFdrotation);
  { X3D specification comment: (-Inf,Inf) }

  FFdscale := TSFVec2f.Create(Self, 'scale', Vector2Single(1, 1));
  Fields.Add(FFdscale);
  { X3D specification comment: (-Inf,Inf) }

  FFdtranslation := TSFVec2f.Create(Self, 'translation', Vector2Single(0, 0));
  Fields.Add(FFdtranslation);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TNodeTextureTransform.ClassNodeTypeName: string;
begin
  Result := 'TextureTransform';
end;

function TNodeTextureTransform.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeTextureTransform.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TNodeTextureTransform.TransformMatrix: TMatrix4Single;
begin
  { Note: don't be fooled by a little confusing VRML 2.0 spec
    wording for TextureTransform, that suggests that VRML 2.0
    TextureTransform should multiply matrices in reversed
    order than VRML 1.0. I'm talking about the paragraph

       In matrix transformation notation, where Tc is the
       untransformed texture coordinate, Tc' is the transformed
       texture coordinate, C (center), T (translation),
       R (rotation), and S (scale) are the intermediate
       transformation matrices,

         Tc' = -C × S × R × C × T × Tc

    VRML TextureTransform node transforms texture *coordinates*.
    OpenGL texture matrix transforms texture *coordinates*.
    And above paragraph says about texture *coordinates*.
    So I'm either ultra-dumb or ultra-smart, but for me
    the above paragraph is wrong: it says precisely that I should load
    the *reversed* matrix of what I'm actually loading.
    If I would follow this, I would actually *not* be
    conforming to the rest of TextureTransform description in VRML spec.

    Am I the only one fooled by this ? No:
    [http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4320634]

    So what's the truth (i.e. the correct order, consistent
    with the whole rest of TextureTransform specification and with
    other implementations) ?
    Actually the order is the same for
    both VRML 1.0 and 2.0, confirmed by experience (reversed
    order just will not work correctly, e.g. TextureTransform
    with rotation and center 0.5 0.5 fields specified will
    not behave correctly) and other implementations
    (see [http://search.cpan.org/src/LUKKA/FreeWRL-0.14/VRMLFunc.xs]
    function TextureTransform_Rend). }

  Result :=
    TranslationMatrix( Vector3Single(
      FdTranslation.Value[0] + FdCenter.Value[0],
      FdTranslation.Value[1] + FdCenter.Value[1], 0));
  Result := MatrixMult(Result,
    RotationMatrixRad(FdRotation.Value, Vector3Single(0, 0, 1)));
  Result := MatrixMult(Result,
    ScalingMatrix(
      Vector3Single( FdScale.Value[0], FdScale.Value[1], 1 )));
  Result := MatrixMult(Result,
    TranslationMatrix(
      Vector3Single( -FdCenter.Value[0], -FdCenter.Value[1], 0 )));
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
    TNodeTextureProperties,
    TNodeTextureTransform
  ]);
end;

{$endif read_implementation}
