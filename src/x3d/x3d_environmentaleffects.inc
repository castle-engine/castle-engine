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
  TAbstractBackgroundNode = class(TAbstractBindableNode)
  private
    procedure EventSet_BindReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
  public
    procedure CreateNode; override;
    function TransformationChange: TNodeTransformationChange; override;

    private FFdGroundAngle: TMFFloat;
    public property FdGroundAngle: TMFFloat read FFdGroundAngle;

    private FFdGroundColor: TMFColor;
    public property FdGroundColor: TMFColor read FFdGroundColor;

    private FFdSkyAngle: TMFFloat;
    public property FdSkyAngle: TMFFloat read FFdSkyAngle;

    private FFdSkyColor: TMFColor;
    public property FdSkyColor: TMFColor read FFdSkyColor;

    private FFdTransparency: TSFFloat;
    public property FdTransparency: TSFFloat read FFdTransparency;

    function TransformRotation: TMatrix4Single;
  end;

  TFogTypeOrNone = (ftLinear, ftExp, ftNone);
  TFogType = ftLinear..ftExp;

  IAbstractFogObject = interface(IX3DNode)
  ['{F5C4E0A9-2214-4E3F-8E90-1E0305B2EBC6}']
    function GetFdcolor: TSFColor;
    function GetFdfogType: TSFString;
    function GetFdvisibilityRange: TSFFloat;
    function GetFdvolumetric: TSFBool;
    function GetFdvolumetricDirection: TSFVec3f;
    function GetFdvolumetricVisibilityStart: TSFFloat;

    property FdColor: TSFColor read GetFdcolor;
    property FdFogType: TSFString read GetFdfogType;
    property FdVisibilityRange: TSFFloat read GetFdvisibilityRange;
    property FdVolumetric: TSFBool read GetFdvolumetric;
    property FdVolumetricDirection: TSFVec3f read GetFdvolumetricDirection;
    property FdVolumetricVisibilityStart: TSFFloat read GetFdvolumetricVisibilityStart;

    function GetTransformScale: Single;
    property TransformScale: Single read GetTransformScale;

    function FogType: TFogType;
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
    procedure FreeAll(Cache: TTexturesVideosCache);
  end;

  TBackgroundNode = class(TAbstractBackgroundNode)
  private
    FBgImagesLoaded: boolean; { = false }

    { if not FBgImagesLoaded it should be always equal to BackgroundImagesNone
      (this is not important for interface of this class but we will use
      this in our implementation) }
    FBgImages: TBackgroundImages;

    procedure SetBgImagesLoaded(const Value: boolean);

    function GetBgImages: TBackgroundImages;
  public
    procedure CreateNode; override;
    destructor Destroy; override;

    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdBackUrl: TMFString;
    public property FdBackUrl: TMFString read FFdBackUrl;

    private FFdBottomUrl: TMFString;
    public property FdBottomUrl: TMFString read FFdBottomUrl;

    private FFdFrontUrl: TMFString;
    public property FdFrontUrl: TMFString read FFdFrontUrl;

    private FFdLeftUrl: TMFString;
    public property FdLeftUrl: TMFString read FFdLeftUrl;

    private FFdRightUrl: TMFString;
    public property FdRightUrl: TMFString read FFdRightUrl;

    private FFdTopUrl: TMFString;
    public property FdTopUrl: TMFString read FFdTopUrl;

    procedure Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames); override;

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
  end;

  TFogNode = class(TAbstractBindableNode, IAbstractFogObject)
  private
    function GetFdcolor: TSFColor;
    function GetFdfogType: TSFString;
    function GetFdvisibilityRange: TSFFloat;
    function GetFdvolumetric: TSFBool;
    function GetFdvolumetricDirection: TSFVec3f;
    function GetFdvolumetricVisibilityStart: TSFFloat;
    function GetTransformScale: Single;
    procedure EventSet_BindReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
    function TransformationChange: TNodeTransformationChange; override;

    private FFdColor: TSFColor;
    public property FdColor: TSFColor read FFdColor;

    private FFdFogType: TSFString;
    public property FdFogType: TSFString read FFdFogType;

    private FFdVisibilityRange: TSFFloat;
    public property FdVisibilityRange: TSFFloat read FFdVisibilityRange;

    { Fields below are Kambi's fog extensions, see
      http://castle-engine.sourceforge.net/x3d_extensions.php#ext_fog_volumetric

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

    { Fog type. }
    function FogType: TFogType;

    { Fog type, but may also return ftNone if we are @nil (checks Self <> nil)
      or visibilityRange = 0. }
    function FogTypeOrNone: TFogTypeOrNone;

    { Apply fog to the color of the vertex.

      This can be used by software renderers (ray-tracers etc.)
      to calculate pixel color following VRML/X3D specifications.
      After all the lighting is summed up (see TX3DGraphTraverseState.Emission and
      TLightInstance.Contribution), process color by this method to apply fog.

      Does nothing if AFogType = ftNone. In this special case, it's also
      allowed to call this on @nil instance. }
    procedure ApplyFog(var Color: TVector3Single;
      const Position, VertexPos: TVector3Single; const AFogType: TFogTypeOrNone);
  end;

  TFogCoordinateNode = class(TAbstractGeometricPropertyNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdDepth: TMFFloat;
    public property FdDepth: TMFFloat read FFdDepth;
  end;

  TLocalFogNode = class(TAbstractChildNode, IAbstractFogObject)
  private
    FTransformScale: Single;
    function GetFdcolor: TSFColor;
    function GetFdfogType: TSFString;
    function GetFdvisibilityRange: TSFFloat;
    function GetFdvolumetric: TSFBool;
    function GetFdvolumetricDirection: TSFVec3f;
    function GetFdvolumetricVisibilityStart: TSFFloat;
    function GetTransformScale: Single;
  protected
    procedure BeforeTraverse(StateStack: TX3DGraphTraverseStateStack); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdColor: TSFColor;
    public property FdColor: TSFColor read FFdColor;

    private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    private FFdFogType: TSFString;
    public property FdFogType: TSFString read FFdFogType;

    private FFdVisibilityRange: TSFFloat;
    public property FdVisibilityRange: TSFFloat read FFdVisibilityRange;

    { Fields below are Kambi's fog extensions, see
      http://castle-engine.sourceforge.net/x3d_extensions.php#ext_fog_volumetric

      @groupBegin }
    private FFdVolumetric: TSFBool;
    public property FdVolumetric: TSFBool read FFdVolumetric;

    private FFdVolumetricDirection: TSFVec3f;
    public property FdVolumetricDirection: TSFVec3f read FFdVolumetricDirection;

    private FFdVolumetricVisibilityStart: TSFFloat;
    public property FdVolumetricVisibilityStart: TSFFloat read FFdVolumetricVisibilityStart;
    { @groupEnd }

    function FogType: TFogType;
  end;

  TTextureBackgroundNode = class(TAbstractBackgroundNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdBackTexture: TSFNode;
    public property FdBackTexture: TSFNode read FFdBackTexture;

    private FFdBottomTexture: TSFNode;
    public property FdBottomTexture: TSFNode read FFdBottomTexture;

    private FFdFrontTexture: TSFNode;
    public property FdFrontTexture: TSFNode read FFdFrontTexture;

    private FFdLeftTexture: TSFNode;
    public property FdLeftTexture: TSFNode read FFdLeftTexture;

    private FFdRightTexture: TSFNode;
    public property FdRightTexture: TSFNode read FFdRightTexture;

    private FFdTopTexture: TSFNode;
    public property FdTopTexture: TSFNode read FFdTopTexture;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TAbstractBackgroundNode.CreateNode;
begin
  inherited;

  FFdGroundAngle := TMFFloat.Create(Self, 'groundAngle', []);
   FdGroundAngle.Angle := true;
   FdGroundAngle.ChangesAlways := [chBackground];
  Fields.Add(FFdGroundAngle);
  { X3D specification comment: [0,Pi/2] }

  FFdGroundColor := TMFColor.Create(Self, 'groundColor', []);
   FdGroundColor.ChangesAlways := [chBackground];
  Fields.Add(FFdGroundColor);
  { X3D specification comment: [0,1] }

  FFdSkyAngle := TMFFloat.Create(Self, 'skyAngle', []);
   FdSkyAngle.Angle := true;
   FdSkyAngle.ChangesAlways := [chBackground];
  Fields.Add(FFdSkyAngle);
  { X3D specification comment: [0,Pi] }

  FFdSkyColor := TMFColor.Create(Self, 'skyColor', Vector3Single(0, 0, 0));
   FdSkyColor.ChangesAlways := [chBackground];
  Fields.Add(FFdSkyColor);
  { X3D specification comment: [0,1] }

  FFdTransparency := TSFFloat.Create(Self, 'transparency', 0);
   FdTransparency.ChangesAlways := [chBackground];
  Fields.Add(FFdTransparency);
  { X3D specification comment: [0,1] }

  DefaultContainerField := 'children';

  Eventset_bind.OnReceive.Add(@EventSet_BindReceive);
end;

procedure TAbstractBackgroundNode.EventSet_BindReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
begin
  if Scene <> nil then
  begin
    Scene.GetBackgroundStack.Set_Bind(Self, (Value as TSFBool).Value);
    { Background is something visible, so we must actually redisplay
      for user to see the new scene. This is not done by Set_Bind method
      automatically, as this is specific to Background node. }
    Scene.VisibleChangeHere([vcVisibleNonGeometry]);
  end;
end;

function TAbstractBackgroundNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcBackground;
end;

function TAbstractBackgroundNode.TransformRotation: TMatrix4Single;
begin
  Result := Transform;

  { TODO: We should extract here only rotation from BgTransform matrix.
    Below is a very hacky way of at least cancelling the translation.
    This will work OK for any rigid body matrix, i.e. composed only from
    rotation and translation. }
  Result[3][0] := 0;
  Result[3][1] := 0;
  Result[3][2] := 0;
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

procedure TBackgroundImages.FreeAll(Cache: TTexturesVideosCache);
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

procedure TBackgroundNode.CreateNode;
begin
  inherited;

  FFdBackUrl := TMFString.Create(Self, 'backUrl', []);
   FdBackUrl.ChangesAlways := [chEverything];
  Fields.Add(FFdBackUrl);
  { X3D specification comment: [URI] }

  FFdBottomUrl := TMFString.Create(Self, 'bottomUrl', []);
   FdBottomUrl.ChangesAlways := [chEverything];
  Fields.Add(FFdBottomUrl);
  { X3D specification comment: [URI] }

  FFdFrontUrl := TMFString.Create(Self, 'frontUrl', []);
   FdFrontUrl.ChangesAlways := [chEverything];
  Fields.Add(FFdFrontUrl);
  { X3D specification comment: [URI] }

  FFdLeftUrl := TMFString.Create(Self, 'leftUrl', []);
   FdLeftUrl.ChangesAlways := [chEverything];
  Fields.Add(FFdLeftUrl);
  { X3D specification comment: [URI] }

  FFdRightUrl := TMFString.Create(Self, 'rightUrl', []);
   FdRightUrl.ChangesAlways := [chEverything];
  Fields.Add(FFdRightUrl);
  { X3D specification comment: [URI] }

  FFdTopUrl := TMFString.Create(Self, 'topUrl', []);
   FdTopUrl.ChangesAlways := [chEverything];
  Fields.Add(FFdTopUrl);
  { X3D specification comment: [URI] }

  DefaultContainerField := 'children';

  FBgImagesLoaded := false;
  FBgImages.InitClear;
end;

destructor TBackgroundNode.Destroy;
begin
  BgImagesLoaded := false;
  inherited;
end;

class function TBackgroundNode.ClassNodeTypeName: string;
begin
  Result := 'Background';
end;

class function TBackgroundNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TBackgroundNode.SetBgImagesLoaded(const Value: boolean);

  procedure LoadImg(bs: TBackgroundSide; Urls: TMFString);
  var
    I: Integer;
    URL: string;
    IgnoreAlpha: TAlphaChannel;
  begin
    FBgImages.Images[bs] := nil;
    for i := 0 to Urls.Count-1 do
    begin
      try
        URL := PathFromBaseUrl(Urls.Items[i]);
        FBgImages.Images[bs] := X3DCache.TextureImage_IncReference(URL, IgnoreAlpha);
        Break;
      except
        on E: Exception do
        begin
          OnWarning(wtMinor, 'VRML/X3D', Format(SLoadError,
            [E.ClassName, 'background image', URL, E.Message]));
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
          This may happen if OnWarning raises exception and some
          image cannot be loaded. }
        FbgImages.FreeAll(X3DCache);
        raise;
      end;
    end else
    begin
      FbgImages.FreeAll(X3DCache);
    end;

    FBgImagesLoaded := Value;
  end;
end;

procedure TBackgroundNode.Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames);
begin
  inherited;
  { Force reload on next BgImages use, since our fields are different now }
  BgImagesLoaded := false;
end;

function TBackgroundNode.GetBgImages: TBackgroundImages;
begin
  BgImagesLoaded := true;
  Result := FBgImages;
end;

procedure TFogNode.CreateNode;
begin
  inherited;

  FFdColor := TSFColor.Create(Self, 'color', Vector3Single(1, 1, 1));
   FdColor.ChangesAlways := [chEverything];
  Fields.Add(FFdColor);
  { X3D specification comment: [0,1] }

  FFdFogType := TSFString.Create(Self, 'fogType', 'LINEAR');
   FdFogType.ChangesAlways := [chEverything];
  Fields.Add(FFdFogType);
  { X3D specification comment: ["LINEAR"|"EXPONENTIAL"] }

  FFdVisibilityRange := TSFFloat.Create(Self, 'visibilityRange', 0);
   FdVisibilityRange.ChangesAlways := [chEverything];
  Fields.Add(FFdVisibilityRange);
  { X3D specification comment: [0,Inf) }

  FFdVolumetric := TSFBool.Create(Self, 'volumetric', false);
   FdVolumetric.ChangesAlways := [chEverything];
  Fields.Add(FFdVolumetric);

  FFdVolumetricDirection := TSFVec3f.Create(Self, 'volumetricDirection', Vector3Single(0, -1, 0));
   FdVolumetricDirection.ChangesAlways := [chEverything];
  Fields.Add(FFdVolumetricDirection);

  FFdVolumetricVisibilityStart := TSFFloat.Create(Self, 'volumetricVisibilityStart', 0);
   FdVolumetricVisibilityStart.ChangesAlways := [chEverything];
  Fields.Add(FFdVolumetricVisibilityStart);

  { Only for backward compatibility. Not used anymore. }
  FFdAlternative := TSFNode.Create(Self, 'alternative', [TFogNode]);
   FdAlternative.ChangesAlways := [chEverything];
  Fields.Add(FFdAlternative);

  DefaultContainerField := 'children';

  Eventset_bind.OnReceive.Add(@EventSet_BindReceive);
end;

class function TFogNode.ClassNodeTypeName: string;
begin
  Result := 'Fog';
end;

class function TFogNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TFogNode.EventSet_BindReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
begin
  if Scene <> nil then
  begin
    Scene.GetFogStack.Set_Bind(Self, (Value as TSFBool).Value);
    { Fog is something visible, so we must actually redisplay
      for user to see the new scene. This is not done by Set_Bind method
      automatically, as this is specific to Fog node. }
    Scene.VisibleChangeHere([vcVisibleNonGeometry]);
  end;
end;

function TFogNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcFog;
end;

function TFogNode.GetFdcolor: TSFColor;
begin
  Result := FFdColor;
end;

function TFogNode.GetFdfogType: TSFString;
begin
  Result := FdFogType;
end;

function TFogNode.GetFdvisibilityRange: TSFFloat;
begin
  Result := FdVisibilityRange;
end;

function TFogNode.GetFdvolumetric: TSFBool;
begin
  Result := FFdVolumetric;
end;

function TFogNode.GetFdvolumetricDirection: TSFVec3f;
begin
  Result := FFdVolumetricDirection;
end;

function TFogNode.GetFdvolumetricVisibilityStart: TSFFloat;
begin
  Result := FFdVolumetricVisibilityStart;
end;

function TFogNode.GetTransformScale: Single;
begin
  Result := TransformScale;
end;

function TFogNode.FogType: TFogType;
begin
  if FdFogType.Value = 'LINEAR' then
    Result := ftLinear else
  if FdFogType.Value = 'EXPONENTIAL' then
    Result := ftExp else
  begin
    OnWarning(wtMajor, 'VRML/X3D', 'Unknown fog type "' + FdFogType.Value + '"');
    Result := ftLinear;
  end;
end;

function TFogNode.FogTypeOrNone: TFogTypeOrNone;
begin
  if (Self = nil) or
     (FdVisibilityRange.Value = 0.0) then
    Exit(ftNone);

  Result := FogType;
end;

procedure TFogNode.ApplyFog(var Color: TVector3Single;
  const Position, VertexPos: TVector3Single; const AFogType: TFogTypeOrNone);
var
  FogVisibilityRangeScaled: Single;

  procedure ApplyDistance(const Distance: Single);
  var
    F: Single;
  begin
    case AFogType of
      ftLinear: F := (FogVisibilityRangeScaled - Distance) / FogVisibilityRangeScaled;
      ftExp   : F := Exp(-Distance / (FogVisibilityRangeScaled - Distance));
    end;
    Color := Vector_Init_Lerp(F, FdColor.Value, Color);
  end;

var
  FogVolumetricVisibilityStart: Single;
  Distance, DistanceSqr: Single;
  VertProjected: TVector3Single;
begin
  if AFogType <> ftNone then
  begin
    FogVisibilityRangeScaled := FdVisibilityRange.Value * TransformScale;

    if FdVolumetric.Value then
    begin
      FogVolumetricVisibilityStart := FdVolumetricVisibilityStart.Value * TransformScale;

      { Calculation of Distance for volumetric fog below is analogous to
        GetFogVolumetric inside ArraysGenerator. }

      VertProjected := PointOnLineClosestToPoint(
        ZeroVector3Single, FdVolumetricDirection.Value, VertexPos);
      Distance := VectorLen(VertProjected);
      if not AreParallelVectorsSameDirection(
        VertProjected, FdVolumetricDirection.Value) then
        Distance := -Distance;
      { Now I want
        - Distance = FogVolumetricVisibilityStart -> 0
        - Distance = FogVolumetricVisibilityStart + X -> X
          (so that Distance = FogVolumetricVisibilityStart +
          FogVisibilityRangeScaled -> FogVisibilityRangeScaled) }
      Distance -= FogVolumetricVisibilityStart;

      { When Distance < 0 our intention is to have no fog.
        So Distance < 0 should be equivalent to Distance = 0. }
      MaxTo1st(Distance, 0);

      if Distance >= FogVisibilityRangeScaled - SingleEqualityEpsilon then
        Color := FdColor.Value else
        ApplyDistance(Distance);
    end else
    begin
      DistanceSqr := PointsDistanceSqr(Position, VertexPos);

      if DistanceSqr >= Sqr(FogVisibilityRangeScaled - SingleEqualityEpsilon) then
        Color := FdColor.Value else
        ApplyDistance(Sqrt(DistanceSqr));

    end;
  end;
end;

procedure TFogCoordinateNode.CreateNode;
begin
  inherited;

  FFdDepth := TMFFloat.Create(Self, 'depth', []);
  Fields.Add(FFdDepth);
  { X3D specification comment: [0,1] }

  DefaultContainerField := 'fogCoord';
end;

class function TFogCoordinateNode.ClassNodeTypeName: string;
begin
  Result := 'FogCoordinate';
end;

class function TFogCoordinateNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TLocalFogNode.CreateNode;
begin
  inherited;

  FFdColor := TSFColor.Create(Self, 'color', Vector3Single(1, 1, 1));
  Fields.Add(FFdColor);
  { X3D specification comment: [0,1] }

  FFdEnabled := TSFBool.Create(Self, 'enabled', true);
  Fields.Add(FFdEnabled);

  FFdFogType := TSFString.Create(Self, 'fogType', 'LINEAR');
  Fields.Add(FFdFogType);
  { X3D specification comment: ["LINEAR"|"EXPONENTIAL"] }

  FFdVisibilityRange := TSFFloat.Create(Self, 'visibilityRange', 0);
  Fields.Add(FFdVisibilityRange);
  { X3D specification comment: [0,-Inf) }

  FFdVolumetric := TSFBool.Create(Self, 'volumetric', false);
   FdVolumetric.ChangesAlways := [chEverything];
  Fields.Add(FFdVolumetric);

  FFdVolumetricDirection := TSFVec3f.Create(Self, 'volumetricDirection', Vector3Single(0, -1, 0));
   FdVolumetricDirection.ChangesAlways := [chEverything];
  Fields.Add(FFdVolumetricDirection);

  FFdVolumetricVisibilityStart := TSFFloat.Create(Self, 'volumetricVisibilityStart', 0);
   FdVolumetricVisibilityStart.ChangesAlways := [chEverything];
  Fields.Add(FFdVolumetricVisibilityStart);

  DefaultContainerField := 'children';

  FTransformScale := 1;
end;

procedure TLocalFogNode.BeforeTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  inherited;
  FTransformScale := StateStack.Top.TransformScale;
end;

class function TLocalFogNode.ClassNodeTypeName: string;
begin
  Result := 'LocalFog';
end;

class function TLocalFogNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TLocalFogNode.GetFdcolor: TSFColor;
begin
  Result := FFdColor;
end;

function TLocalFogNode.GetFdfogType: TSFString;
begin
  Result := FdFogType;
end;

function TLocalFogNode.GetFdvisibilityRange: TSFFloat;
begin
  Result := FdVisibilityRange;
end;

function TLocalFogNode.GetFdvolumetric: TSFBool;
begin
  Result := FFdVolumetric;
end;

function TLocalFogNode.GetFdvolumetricDirection: TSFVec3f;
begin
  Result := FFdVolumetricDirection;
end;

function TLocalFogNode.GetFdvolumetricVisibilityStart: TSFFloat;
begin
  Result := FFdVolumetricVisibilityStart;
end;

function TLocalFogNode.GetTransformScale: Single;
begin
  { TODO: this is actually bad, each LocalFog occurrence may have
    different scale, depending on parent transform node.
    We should keep LocalFogScale in State, and pass it along
    to RenderShape. This method should not exist. }
  Result := FTransformScale;
end;

function TLocalFogNode.FogType: TFogType;
begin
  if FdFogType.Value = 'LINEAR' then
    Result := ftLinear else
  if FdFogType.Value = 'EXPONENTIAL' then
    Result := ftExp else
  begin
    OnWarning(wtMajor, 'VRML/X3D', 'Unknown fog type "' + FdFogType.Value + '"');
    Result := ftLinear;
  end;
end;

procedure TTextureBackgroundNode.CreateNode;
begin
  inherited;

  FFdBackTexture := TSFNode.Create(Self, 'backTexture', [TAbstractTextureNode]);
  Fields.Add(FFdBackTexture);

  FFdBottomTexture := TSFNode.Create(Self, 'bottomTexture', [TAbstractTextureNode]);
  Fields.Add(FFdBottomTexture);

  FFdFrontTexture := TSFNode.Create(Self, 'frontTexture', [TAbstractTextureNode]);
  Fields.Add(FFdFrontTexture);

  FFdLeftTexture := TSFNode.Create(Self, 'leftTexture', [TAbstractTextureNode]);
  Fields.Add(FFdLeftTexture);

  FFdRightTexture := TSFNode.Create(Self, 'rightTexture', [TAbstractTextureNode]);
  Fields.Add(FFdRightTexture);

  FFdTopTexture := TSFNode.Create(Self, 'topTexture', [TAbstractTextureNode]);
  Fields.Add(FFdTopTexture);

  DefaultContainerField := 'children';
end;

class function TTextureBackgroundNode.ClassNodeTypeName: string;
begin
  Result := 'TextureBackground';
end;

class function TTextureBackgroundNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterEnvironmentalEffectsNodes;
begin
  NodesManager.RegisterNodeClasses([
    TBackgroundNode,
    TFogNode,
    TFogCoordinateNode,
    TLocalFogNode,
    TTextureBackgroundNode
  ]);
end;

{$endif read_implementation}
