{
  Copyright 2002-2010 Michalis Kamburelis.

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
  TNodeX3DBackgroundNode = class(TNodeX3DBindableNode)
  private
    procedure EventSet_BindReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    function TransformationChange: TNodeTransformationChange; override;

    private FFdgroundAngle: TMFFloat;
    public property FdgroundAngle: TMFFloat read FFdgroundAngle;

    private FFdgroundColor: TMFColor;
    public property FdgroundColor: TMFColor read FFdgroundColor;

    private FFdskyAngle: TMFFloat;
    public property FdskyAngle: TMFFloat read FFdskyAngle;

    private FFdskyColor: TMFColor;
    public property FdskyColor: TMFColor read FFdskyColor;

    private FFdtransparency: TSFFloat;
    public property Fdtransparency: TSFFloat read FFdtransparency;
  end;

  INodeX3DFogObject = interface(IVRMLNode)
  ['{F5C4E0A9-2214-4E3F-8E90-1E0305B2EBC6}']
    property Fdcolor: TSFColor { read GetFdcolor }; { }
    property FdfogType: TSFString { read GetFdfogType }; { }
    property FdvisibilityRange: TSFFloat { read GetFdvisibilityRange }; { }
  end;

  TBackgroundSide = (bsBack, bsBottom, bsFront, bsLeft, bsRight, bsTop);
  TBackgroundSides = set of TBackgroundSide;

  { Six background images. }
  TBackgroundImages = object
  public
    Images: array [TBackgroundSide] of TEncodedImage;

    constructor InitClear;

    { Release and set to @nil all images.
      If Cache is @nil, release images by simple FreeAndNil,
      otherwise they are released from the cache by TextureImage_DecReference. }
    procedure FreeAll(Cache: TTexturesImagesVideosCache);
  end;

  TNodeBackground = class(TNodeX3DBackgroundNode)
  private
    FBgImagesLoaded: boolean; { = false }

    { if not FBgImagesLoaded it should be always equal to BackgroundImagesNone
      (this is not important for interface of this class but we will use
      this in our implementation) }
    FBgImages: TBackgroundImages;

    { Non-nil only if FBgImages should be freed using
      UsedCache.TextureImage_DecReference. }
    UsedCache: TTexturesImagesVideosCache;
    FCache: TTexturesImagesVideosCache;

    procedure SetBgImagesLoaded(const Value: boolean);

    function GetBgImages: TBackgroundImages;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    destructor Destroy; override;

    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdbackUrl: TMFString;
    public property FdbackUrl: TMFString read FFdbackUrl;

    private FFdbottomUrl: TMFString;
    public property FdbottomUrl: TMFString read FFdbottomUrl;

    private FFdfrontUrl: TMFString;
    public property FdfrontUrl: TMFString read FFdfrontUrl;

    private FFdleftUrl: TMFString;
    public property FdleftUrl: TMFString read FFdleftUrl;

    private FFdrightUrl: TMFString;
    public property FdrightUrl: TMFString read FFdrightUrl;

    private FFdtopUrl: TMFString;
    public property FdtopUrl: TMFString read FFdtopUrl;

    procedure Parse(Lexer: TVRMLLexer; Names: TVRMLNames); override;

    { Pierwsze uzycie BgImages (albo pierwsze uzycie BgImages po Parse)
      automatycznie zaladuje obrazki z URLi
      BackUrl/BottomUrl itd. Obrazki dla ktorych zaden osiagalny URL nie byl
      podany zostana ustawione na ImageNone.

      Images may have classes allowed by LoadTextureImage.

      Obrazki beda zawsze odpowiadaly URLom w polach Fd*Url - dlatego po
      zrobieniu Parse na tym obiekcie nastepne wywolanie BgImages bedzie
      ladowalo je od nowa. }
    property BgImages: TBackgroundImages read GetBgImages;

    { Czy obrazki juz sa zaladowane ? W zasadzie ta funkcja nigdy nie powinna
      ci byc potrzebna skoro cale ladowanie / zwalnianie obrazkow jest
      robione automatycznie. Ale moze ci przydac : ona mowi czy najblizsze
      wywolanie BgImages wywola ReloadBgImages czy nie.

      You can set this to @true to force loading of images now, and you
      can set to this to @false to force release of background images now.
      In normal circumstances, you don't need it --- BgImages are automatically
      loaded on first use, and are automatically freed when necessary.

      If you're sure you want to reload the background images (e.g. when
      you suspect that images on the disk changed), you may force reload
      by setting this to @false and then to @true again. }
    property BgImagesLoaded: boolean read FBgImagesLoaded
      write SetBgImagesLoaded;

    { Set this to non-nil before background images are loaded, this way bacground
      images will be loaded from this cache.

      They will also be freed using this
      cache then (so remember that used cache must remain valid for the lifetime
      of this node, or you have to call BgImagesLoaded := false explicitly to
      break the connection from this node to the cache). }
    property Cache: TTexturesImagesVideosCache read FCache write FCache;
  end;

  TNodeFog = class(TNodeX3DBindableNode, INodeX3DFogObject)
  private
    procedure EventSet_BindReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
    function TransformationChange: TNodeTransformationChange; override;

    private FFdcolor: TSFColor;
    public property Fdcolor: TSFColor read FFdcolor;

    private FFdfogType: TSFString;
    public property FdfogType: TSFString read FFdfogType;

    private FFdvisibilityRange: TSFFloat;
    public property FdvisibilityRange: TSFFloat read FFdvisibilityRange;

    { Fields below are Kambi's fog extensions, see
      http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#ext_fog_volumetric

      @groupBegin }
    private FFdVolumetric: TSFBool;
    public property FdVolumetric: TSFBool read FFdVolumetric;

    private FFdVolumetricDirection: TSFVec3f;
    public property FdVolumetricDirection: TSFVec3f read FFdVolumetricDirection;

    private FFdVolumetricVisibilityStart: TSFFloat;
    public property FdVolumetricVisibilityStart: TSFFloat read FFdVolumetricVisibilityStart;

    private FFdAlternative: TSFNode;
    public property FdAlternative: TSFNode read FFdAlternative;
    { @groupEnd }

    { Returns FdAlternative.Value already typecasted to TNodeFog.
      If FdAlternative.Value is not of TNodeFog class, returns nil
      (returns also nil when FdAlternative.Value is nil, obviously). }
    function Alternative: TNodeFog;
  end;

  TNodeFogCoordinate = class(TNodeX3DGeometricPropertyNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFddepth: TMFFloat;
    public property Fddepth: TMFFloat read FFddepth;
  end;

  TNodeLocalFog = class(TNodeX3DChildNode, INodeX3DFogObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcolor: TSFColor;
    public property Fdcolor: TSFColor read FFdcolor;

    private FFdenabled: TSFBool;
    public property Fdenabled: TSFBool read FFdenabled;

    private FFdfogType: TSFString;
    public property FdfogType: TSFString read FFdfogType;

    private FFdvisibilityRange: TSFFloat;
    public property FdvisibilityRange: TSFFloat read FFdvisibilityRange;
  end;

  TNodeTextureBackground = class(TNodeX3DBackgroundNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdbackTexture: TSFNode;
    public property FdbackTexture: TSFNode read FFdbackTexture;

    private FFdbottomTexture: TSFNode;
    public property FdbottomTexture: TSFNode read FFdbottomTexture;

    private FFdfrontTexture: TSFNode;
    public property FdfrontTexture: TSFNode read FFdfrontTexture;

    private FFdleftTexture: TSFNode;
    public property FdleftTexture: TSFNode read FFdleftTexture;

    private FFdrightTexture: TSFNode;
    public property FdrightTexture: TSFNode read FFdrightTexture;

    private FFdtopTexture: TSFNode;
    public property FdtopTexture: TSFNode read FFdtopTexture;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DBackgroundNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdgroundAngle := TMFFloat.Create(Self, 'groundAngle', []);
   FdgroundAngle.ChangesAlways := [chEverything];
  Fields.Add(FFdgroundAngle);
  { X3D specification comment: [0,Pi/2] }

  FFdgroundColor := TMFColor.Create(Self, 'groundColor', []);
   FdgroundColor.ChangesAlways := [chEverything];
  Fields.Add(FFdgroundColor);
  { X3D specification comment: [0,1] }

  FFdskyAngle := TMFFloat.Create(Self, 'skyAngle', []);
   FdskyAngle.ChangesAlways := [chEverything];
  Fields.Add(FFdskyAngle);
  { X3D specification comment: [0,Pi] }

  FFdskyColor := TMFColor.Create(Self, 'skyColor', Vector3Single(0, 0, 0));
   FdskyColor.ChangesAlways := [chEverything];
  Fields.Add(FFdskyColor);
  { X3D specification comment: [0,1] }

  FFdtransparency := TSFFloat.Create(Self, 'transparency', 0);
   Fdtransparency.ChangesAlways := [chEverything];
  Fields.Add(FFdtransparency);
  { X3D specification comment: [0,1] }

  DefaultContainerField := 'children';

  Eventset_bind.OnReceive.Add(@EventSet_BindReceive);
end;

procedure TNodeX3DBackgroundNode.EventSet_BindReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  if EventsEngine <> nil then
  begin
    EventsEngine.GetBackgroundStack.Set_Bind(Self, (Value as TSFBool).Value);
    { Background is something visible, so we must actually redisplay
      for user to see the new scene. This is not done by Set_Bind method
      automatically, as this is specific to Background node. }
    EventsEngine.VisibleChangeHere([vcVisibleNonGeometry]);
  end;
end;

function TNodeX3DBackgroundNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcBackground;
end;

constructor TBackgroundImages.InitClear;
begin
  Images[bsBack  ] := nil;
  Images[bsBottom] := nil;
  Images[bsFront ] := nil;
  Images[bsLeft  ] := nil;
  Images[bsRight ] := nil;
  Images[bsTop   ] := nil;
end;

procedure TBackgroundImages.FreeAll(Cache: TTexturesImagesVideosCache);
var
  bs: TBackgroundSide;
begin
  if Cache <> nil then
  begin
    for bs := Low(bs) to High(bs) do
      { Cache.TextureImage_DecReference is not prepared for nil parameters,
        and some of our Images[bs] may be nil. }
      if Images[bs] <> nil then
        Cache.TextureImage_DecReference(Images[bs]);
  end else
  begin
    for bs := Low(bs) to High(bs) do FreeAndNil(Images[bs]);
  end;
end;

constructor TNodeBackground.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdbackUrl := TMFString.Create(Self, 'backUrl', []);
   FdbackUrl.ChangesAlways := [chEverything];
  Fields.Add(FFdbackUrl);
  { X3D specification comment: [URI] }

  FFdbottomUrl := TMFString.Create(Self, 'bottomUrl', []);
   FdbottomUrl.ChangesAlways := [chEverything];
  Fields.Add(FFdbottomUrl);
  { X3D specification comment: [URI] }

  FFdfrontUrl := TMFString.Create(Self, 'frontUrl', []);
   FdfrontUrl.ChangesAlways := [chEverything];
  Fields.Add(FFdfrontUrl);
  { X3D specification comment: [URI] }

  FFdleftUrl := TMFString.Create(Self, 'leftUrl', []);
   FdleftUrl.ChangesAlways := [chEverything];
  Fields.Add(FFdleftUrl);
  { X3D specification comment: [URI] }

  FFdrightUrl := TMFString.Create(Self, 'rightUrl', []);
   FdrightUrl.ChangesAlways := [chEverything];
  Fields.Add(FFdrightUrl);
  { X3D specification comment: [URI] }

  FFdtopUrl := TMFString.Create(Self, 'topUrl', []);
   FdtopUrl.ChangesAlways := [chEverything];
  Fields.Add(FFdtopUrl);
  { X3D specification comment: [URI] }

  DefaultContainerField := 'children';

  FBgImagesLoaded := false;
  FBgImages.InitClear;
end;

destructor TNodeBackground.Destroy;
begin
  BgImagesLoaded := false;
  inherited;
end;

class function TNodeBackground.ClassNodeTypeName: string;
begin
  Result := 'Background';
end;

function TNodeBackground.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 100);
end;

class function TNodeBackground.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeBackground.SetBgImagesLoaded(const Value: boolean);

  procedure LoadImg(bs: TBackgroundSide; Urls: TMFString);
  var
    I: Integer;
    URL: string;
  begin
    FBgImages.Images[bs] := nil;
    for i := 0 to Urls.Count-1 do
    begin
      try
        URL := PathFromWWWBasePath(Urls.Items.Items[i]);
        if Cache <> nil then
        begin
          FBgImages.Images[bs] := Cache.TextureImage_IncReference(URL);
        end else
          FBgImages.Images[bs] := LoadTextureImage(URL);
        Break;
      except
        on E: Exception do
        begin
          VRMLWarning(vwIgnorable, 'Exception ' + E.ClassName +
            ' occurred when trying to load ' +
            'background image from URL "' + URL + '" : '+E.Message);
          { and silence exception }
        end;
      end;
    end;
  end;

begin
  if Value <> BgImagesLoaded then
  begin
    if Value then
    begin
      try
        LoadImg(bsBack, FdBackUrl);
        LoadImg(bsBottom, FdBottomUrl);
        LoadImg(bsFront, FdFrontUrl);
        LoadImg(bsLeft, FdLeftUrl);
        LoadImg(bsRight, FdRightUrl);
        LoadImg(bsTop, FdTopUrl);
      except
        { In case of trouble, free the images that were loaded.
          This may happen if VRMLWarning raises exception and some
          image cannot be loaded. }
        FbgImages.FreeAll(Cache);
        raise;
      end;
      UsedCache := Cache;
    end else
    begin
      FbgImages.FreeAll(UsedCache);
      UsedCache := nil;
    end;

    FBgImagesLoaded := Value;
  end;
end;

procedure TNodeBackground.Parse(Lexer: TVRMLLexer; Names: TVRMLNames);
begin
  inherited;
  { Force reload on next BgImages use, since our fields are different now }
  BgImagesLoaded := false;
end;

function TNodeBackground.GetBgImages: TBackgroundImages;
begin
  BgImagesLoaded := true;
  Result := FBgImages;
end;

constructor TNodeFog.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcolor := TSFColor.Create(Self, 'color', Vector3Single(1, 1, 1));
   Fdcolor.ChangesAlways := [chEverything];
  Fields.Add(FFdcolor);
  { X3D specification comment: [0,1] }

  FFdfogType := TSFString.Create(Self, 'fogType', 'LINEAR');
   FdfogType.ChangesAlways := [chEverything];
  Fields.Add(FFdfogType);
  { X3D specification comment: ["LINEAR"|"EXPONENTIAL"] }

  FFdvisibilityRange := TSFFloat.Create(Self, 'visibilityRange', 0);
   FdvisibilityRange.ChangesAlways := [chEverything];
  Fields.Add(FFdvisibilityRange);
  { X3D specification comment: [0,Inf) }

  FFdvolumetric := TSFBool.Create(Self, 'volumetric', false);
   Fdvolumetric.ChangesAlways := [chEverything];
  Fields.Add(FFdvolumetric);

  FFdvolumetricDirection := TSFVec3f.Create(Self, 'volumetricDirection', Vector3Single(0, -1, 0));
   FdvolumetricDirection.ChangesAlways := [chEverything];
  Fields.Add(FFdvolumetricDirection);

  FFdvolumetricVisibilityStart := TSFFloat.Create(Self, 'volumetricVisibilityStart', 0);
   FdvolumetricVisibilityStart.ChangesAlways := [chEverything];
  Fields.Add(FFdvolumetricVisibilityStart);

  FFdalternative := TSFNode.Create(Self, 'alternative', [TNodeFog]);
   Fdalternative.ChangesAlways := [chEverything];
  Fields.Add(FFdalternative);

  DefaultContainerField := 'children';

  Eventset_bind.OnReceive.Add(@EventSet_BindReceive);
end;

class function TNodeFog.ClassNodeTypeName: string;
begin
  Result := 'Fog';
end;

function TNodeFog.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 100);
end;

class function TNodeFog.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TNodeFog.Alternative: TNodeFog;
begin
  if (FdAlternative.Value <> nil) and (FdAlternative.Value is TNodeFog) then
    Result := TNodeFog(FdAlternative.Value) else
    Result := nil;
end;

procedure TNodeFog.EventSet_BindReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  if EventsEngine <> nil then
  begin
    EventsEngine.GetFogStack.Set_Bind(Self, (Value as TSFBool).Value);
    { Fog is something visible, so we must actually redisplay
      for user to see the new scene. This is not done by Set_Bind method
      automatically, as this is specific to Fog node. }
    EventsEngine.VisibleChangeHere([vcVisibleNonGeometry]);
  end;
end;

function TNodeFog.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcFog;
end;

constructor TNodeFogCoordinate.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFddepth := TMFFloat.Create(Self, 'depth', []);
  Fields.Add(FFddepth);
  { X3D specification comment: [0,1] }

  DefaultContainerField := 'fogCoord';
end;

class function TNodeFogCoordinate.ClassNodeTypeName: string;
begin
  Result := 'FogCoordinate';
end;

function TNodeFogCoordinate.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeFogCoordinate.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeLocalFog.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcolor := TSFColor.Create(Self, 'color', Vector3Single(1, 1, 1));
  Fields.Add(FFdcolor);
  { X3D specification comment: [0,1] }

  FFdenabled := TSFBool.Create(Self, 'enabled', true);
  Fields.Add(FFdenabled);

  FFdfogType := TSFString.Create(Self, 'fogType', 'LINEAR');
  Fields.Add(FFdfogType);
  { X3D specification comment: ["LINEAR"|"EXPONENTIAL"] }

  FFdvisibilityRange := TSFFloat.Create(Self, 'visibilityRange', 0);
  Fields.Add(FFdvisibilityRange);
  { X3D specification comment: [0,-Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeLocalFog.ClassNodeTypeName: string;
begin
  Result := 'LocalFog';
end;

function TNodeLocalFog.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeLocalFog.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeTextureBackground.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdbackTexture := TSFNode.Create(Self, 'backTexture', [TNodeX3DTextureNode]);
  Fields.Add(FFdbackTexture);

  FFdbottomTexture := TSFNode.Create(Self, 'bottomTexture', [TNodeX3DTextureNode]);
  Fields.Add(FFdbottomTexture);

  FFdfrontTexture := TSFNode.Create(Self, 'frontTexture', [TNodeX3DTextureNode]);
  Fields.Add(FFdfrontTexture);

  FFdleftTexture := TSFNode.Create(Self, 'leftTexture', [TNodeX3DTextureNode]);
  Fields.Add(FFdleftTexture);

  FFdrightTexture := TSFNode.Create(Self, 'rightTexture', [TNodeX3DTextureNode]);
  Fields.Add(FFdrightTexture);

  FFdtopTexture := TSFNode.Create(Self, 'topTexture', [TNodeX3DTextureNode]);
  Fields.Add(FFdtopTexture);

  DefaultContainerField := 'children';
end;

class function TNodeTextureBackground.ClassNodeTypeName: string;
begin
  Result := 'TextureBackground';
end;

function TNodeTextureBackground.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeTextureBackground.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterEnvironmentalEffectsNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeBackground,
    TNodeFog,
    TNodeFogCoordinate,
    TNodeLocalFog,
    TNodeTextureBackground
  ]);
end;

{$endif read_implementation}
