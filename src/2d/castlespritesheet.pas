unit CastleSpriteSheet;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, Generics.Collections,
  X3DNodes,
  CastleImages, CastleVectors;

type
  TCastleSpriteSheetAnimation = class;
  TCastleSpriteSheetFrame = class;

  TCastleSpriteSheetAnimationList = specialize TObjectList<TCastleSpriteSheetAnimation>;
  TCastleSpriteSheetFrameList = specialize TObjectList<TCastleSpriteSheetFrame>;

  TCastleSpriteSheet = class
    strict private
      FAnimationList: TCastleSpriteSheetAnimationList;
      FURL: String;

    public
      constructor Create;
      destructor Destroy; override;

      procedure Load(const URL: String);
      procedure Save(const URL: String);
      class function LoadToX3D(const URL: String): TX3DRootNode;

      function AnimationByName(const Name:String): TCastleSpriteSheetAnimation;
      function AnimationByIndex(const Index: Integer): TCastleSpriteSheetAnimation;
      function AnimationCount: Integer;
      function HasAnimation(const Name:String): Boolean;
      function AddAnimation(const Name:String): TCastleSpriteSheetAnimation;
      procedure RemoveAnimationByName(const Name:String);
      procedure RemoveAnimationByIndex(const Index: Integer);

      property URL: String read FURL write FURL;
  end;

  TCastleSpriteSheetAnimation = class
    strict private
      FName: String;
      FFramesPerSecond: Single;
      FFrameList: TCastleSpriteSheetFrameList;

      function GetFrame(const Index: Integer): TCastleSpriteSheetFrame;
    public
      constructor Create(AName: String);
      destructor Destroy; override;


      function FrameCount: Integer;
      procedure AddFrame(AFrame: TCastleSpriteSheetFrame);
      function AllFramesHasTheSameSize: Boolean;
      function GetBigestFrameSize(const MaxWidth, MaxHeight: Integer): TVector2Integer;

      property Name: String read FName write FName;
      property Frame[Index: Integer]: TCastleSpriteSheetFrame read GetFrame;
      property FramesPerSecond: Single read FFramesPerSecond write FFramesPerSecond;
  end;

  TCastleSpriteSheetFrame = class
    public
      X: Integer; // x on image
      Y: Integer; // y on image
      Width: Integer; // width on image
      Height: Integer; // height on image
      FrameX: Integer; // relative frame x coordinate
      FrameY: Integer; // relative frame y coordinate
      FrameWidth: Integer; // real frame width (if not present Width have real frame width
      FrameHeight: Integer; // real frame height (if not present Width have real frame height

      Trimmed: Boolean; // FrameX, FrameY, FrameWidth, FrameHeight has real values?
      FrameImage: TCastleImage;

      destructor Destroy; override;

  end;

  { Starling XML file is not correct }
  EInvalidCastleSpriteSheetXml = class(Exception);

implementation

uses StrUtils, DOM,
  CastleLog, CastleStringUtils, CastleTextureImages,
  CastleURIUtils, CastleUtils, CastleXMLUtils;

type
  { Frame names in starling file can be named freely, but in the case of our loader,
    we have to define what is the next frame of the animation and what should
    be recognized as a separate animation.
    See https://github.com/castle-engine/castle-engine/wiki/Sprite-sheets }
  TStarlingAnimationNaming = (
    { Default behavior treats as animation frames only those subtextures whose
      names ends with an underscore followed by a number. }
    anStrictUnderscore,
    { In many cases, the consecutive frames of one animation are named
      without underscore. }
    anTralingNumber
  );

  TCastleSpriteSheetLoader = class
  strict private
    type
      { Class that represents SubTexture from Starling xml file }
      TSubTexture = class
      private
        FAnimationNaming: TStarlingAnimationNaming;
        procedure ParseAnimationName(const SubTextureName: String);
      public
        AnimationName: String;
        X: Integer;
        Y: Integer;
        Width: Integer;
        Height: Integer;
        FrameX: Integer;
        FrameY: Integer;
        FrameWidth: Integer;
        FrameHeight: Integer;
        Trimmed: Boolean;

        procedure ReadFormXMLNode(const SubTextureNode: TDOMElement; const ImageWidth, ImageHeight: Integer);
        constructor Create;
      end;

      TLoadStrategy = class
      protected
        FLoader: TCastleSpriteSheetLoader;
      public
        constructor Create(Loader: TCastleSpriteSheetLoader);

        procedure PrepareContainer; virtual; abstract;
        procedure CalculateFrameCoords(const SubTexture: TSubTexture); virtual; abstract;
        procedure PrepareAnimation(const Name: String); virtual; abstract;
        procedure AddAnimation(const FrameCount: Integer); virtual; abstract;
        procedure AddFrame; virtual; abstract;
      end;

      TLoadToX3D = class (TLoadStrategy)
      private
        procedure CalculateAnchors(const SubTexture: TSubTexture);
        procedure PrepareTexCordsForX3D(const SubTexture: TSubTexture;
          const ImageWidth, ImageHeight: Integer);
      public
        FRoot: TX3DRootNode;
        FShapeCoord: TCoordinateNode;
        FShapeTexCoord: TTextureCoordinateNode;

        FCoordArray: array of TVector3;
        FTexCoordArray: array of TVector2;

        TimeSensor: TTimeSensorNode;
        CoordInterp: TCoordinateInterpolatorNode;
        TexCoordInterp: TCoordinateInterpolator2DNode;

        { Current frame cords and anchors }
        X1: Single;
        Y1: Single;
        X2: Single;
        Y2: Single;
        AnchorX: Single;
        AnchorY: Single;

        constructor Create(Loader: TCastleSpriteSheetLoader; RootNode: TX3DRootNode);

        { In case of X3D here we prepare X3D root node and shape }
        procedure PrepareContainer; override;
        { In case of X3D here we calculate anchors and set frame cords }
        procedure CalculateFrameCoords(const SubTexture: TSubTexture); override;
        { In case of X3D here we create TimeSensor, CoordInterp,
          TexCoordInterp for animation }
        procedure PrepareAnimation(const Name: String); override;
        { In case of X3D here we add TimeSensor, CoordInterp, TexCoordInterp
          and routes to root }
        procedure AddAnimation(const FrameCount: Integer); override;
        { In case of X3D here we add frame coords to CoordInterp and TexCoordInterp }
        procedure AddFrame; override;
      end;

      TLoadToSpriteSheetModel = class (TLoadStrategy)
      private
        FSpriteSheet: TCastleSpriteSheet;
        FCurrentAnimation: TCastleSpriteSheetAnimation;
        FSubTexture: TSubTexture;
        FImage: TCastleImage;
      public
        constructor Create(Loader: TCastleSpriteSheetLoader; SpriteSheet: TCastleSpriteSheet);

        destructor Destroy; override;

        { In case of TCastleSpriteSheet here we only set URL }
        procedure PrepareContainer; override;
        { In case of TCastleSpriteSheet here we only get SubTexture pointer }
        procedure CalculateFrameCoords(const SubTexture: TSubTexture); override;
        { In case of TCastleSpriteSheet here we only create animation }
        procedure PrepareAnimation(const Name: String); override;
        { In case of TCastleSpriteSheet here nothing to do }
        procedure AddAnimation(const FrameCount: Integer); override;
        { In case of TCastleSpriteSheet here we add frame to animation }
        procedure AddFrame; override;
      end;

    var
      FURL: String;
      FDisplayURL: String;

      { Load settings. }
      FFramesPerSecond: Single;

      FImageWidth, FImageHeight: Integer;
      FImagePath: String;

      FSubTexture: TSubTexture;

      { Animation list to check if the file has any mixed SubTexture nodes. }
      FAnimationList: TStringList;

    procedure ReadImportSettings;

    procedure ReadImageProperties(const URL: String; const AtlasNode: TDOMElement);

    function CheckAnimationNameAvailable(const AnimationName: String): Boolean;

    procedure Load(LoadTarget: TLoadStrategy);

  public
    constructor Create(const URL: String);
    destructor Destroy; override;

    function LoadToX3D: TX3DRootNode;
    procedure LoadToCastleSpriteSheet(SpriteSheet: TCastleSpriteSheet);
    function LoadToCastleSpriteSheet: TCastleSpriteSheet;
  end;

{ TCastleSpriteSheetFrame }

destructor TCastleSpriteSheetFrame.Destroy;
begin
  FreeAndNil(FrameImage);
  inherited Destroy;
end;

{ TCastleSpriteSheetLoader.TLoadToSpriteSheetModel }

constructor TCastleSpriteSheetLoader.TLoadToSpriteSheetModel.Create(
  Loader: TCastleSpriteSheetLoader; SpriteSheet: TCastleSpriteSheet);
begin
  inherited Create(Loader);
  FSpriteSheet := SpriteSheet;
end;

destructor TCastleSpriteSheetLoader.TLoadToSpriteSheetModel.Destroy;
begin
  FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TCastleSpriteSheetLoader.TLoadToSpriteSheetModel.PrepareContainer;
begin
  FSpriteSheet.URL := FLoader.FURL;
  FImage := LoadImage(FLoader.FImagePath);
end;

procedure TCastleSpriteSheetLoader.TLoadToSpriteSheetModel.CalculateFrameCoords(
  const SubTexture: TSubTexture);
begin
  // just remember SubTexture for AddFrame
  FSubTexture := SubTexture;
end;

procedure TCastleSpriteSheetLoader.TLoadToSpriteSheetModel.PrepareAnimation(
  const Name: String);
begin
  FCurrentAnimation := FSpriteSheet.AddAnimation(Name);
end;

procedure TCastleSpriteSheetLoader.TLoadToSpriteSheetModel.AddAnimation(
  const FrameCount: Integer);
begin
  // nothing to do
end;

procedure TCastleSpriteSheetLoader.TLoadToSpriteSheetModel.AddFrame;
var
  Frame: TCastleSpriteSheetFrame;
  FrameImage: TCastleImage;
begin
  Frame := TCastleSpriteSheetFrame.Create;
  Frame.X := FSubTexture.X;
  Frame.Y := FSubTexture.Y;
  Frame.Width := FSubTexture.Width;
  Frame.Height := FSubTexture.Height;
  Frame.Trimmed := FSubTexture.Trimmed;
  if Frame.Trimmed then
  begin
    Frame.FrameWidth := FSubTexture.FrameWidth;
    Frame.FrameHeight := FSubTexture.FrameHeight;
    Frame.FrameX := FSubTexture.FrameY;
    Frame.FrameY := FSubTexture.FrameY;
  end else
  begin
    // make data always OK
    Frame.FrameX := Frame.X;
    Frame.FrameY := Frame.Y;
    Frame.FrameWidth := Frame.Width;
    Frame.FrameHeight := Frame.Height;
  end;

  if not Frame.Trimmed then
    Frame.FrameImage := FImage.MakeExtracted(Frame.X,
      FLoader.FImageHeight - Frame.Y - Frame.Height,
      Frame.Width, Frame.Height)
  else
  begin
    { When Image is trimmed we can't simply extract image part }
    Frame.FrameImage := TRGBAlphaImage.Create(Frame.FrameWidth, Frame.FrameHeight);
    Frame.FrameImage.DrawFrom(FImage,
      0, // destination X
      0, // destination Y
      Frame.X, // source X
      FLoader.FImageHeight - Frame.Y - Frame.Height, // source Y
      Frame.Width,
      Frame.Height
    );
  end;

  FCurrentAnimation.AddFrame(Frame);
end;

{ TCastleSpriteSheetAnimation }

constructor TCastleSpriteSheetAnimation.Create(AName: String);
begin
  FName := AName;
  FFramesPerSecond := DefaultSpriteSheetFramesPerSecond;
  FFrameList := TCastleSpriteSheetFrameList.Create;
end;

destructor TCastleSpriteSheetAnimation.Destroy;
begin
  FreeAndNil(FFrameList);
  inherited Destroy;
end;

function TCastleSpriteSheetAnimation.GetFrame(const Index: Integer
  ): TCastleSpriteSheetFrame;
begin
  Result := FFrameList[Index];
end;

function TCastleSpriteSheetAnimation.FrameCount: Integer;
begin
  Result := FFrameList.Count;
end;

procedure TCastleSpriteSheetAnimation.AddFrame(AFrame: TCastleSpriteSheetFrame);
begin
  FFrameList.Add(AFrame);
end;

function TCastleSpriteSheetAnimation.AllFramesHasTheSameSize: Boolean;
var
  I: Integer;
begin
  if FrameCount < 2 then
    Exit(true);

  for I := 1 to FrameCount - 1 do
  begin
    if (Frame[I].FrameWidth <> Frame[I - 1].FrameWidth) or
      (Frame[I].FrameHeight <> Frame[I - 1].FrameHeight) then
      Exit(false);
  end;

  Result := true;
end;

function TCastleSpriteSheetAnimation.GetBigestFrameSize(
  const MaxWidth, MaxHeight: Integer): TVector2Integer;
var
  I: Integer;
  AFrame: TCastleSpriteSheetFrame;
begin
  Result.X := 0;
  Result.Y := 0;
  for I := 0 to FrameCount - 1 do
  begin
    AFrame := Frame[I];

    if (AFrame.FrameWidth > MaxWidth) or (AFrame.FrameHeight > MaxHeight) then
      continue;

    if AFrame.FrameWidth > Result.X then
    begin
      Result.X := AFrame.FrameWidth;
      Result.Y := AFrame.FrameHeight;
    end else
    if (AFrame.FrameHeight > Result.Y) and (AFrame.FrameHeight > Result.X) then
    begin
      { Use bigger frame with Y only if FrameHeight is bigger than FrameWidth }
      Result.X := AFrame.FrameWidth;
      Result.Y := AFrame.FrameHeight;
    end;
  end;
end;

{ TCastleSpriteSheetLoader.TLoadToX3D }

procedure TCastleSpriteSheetLoader.TLoadToX3D.CalculateAnchors(
  const SubTexture: TSubTexture);
var
  FrameAnchorX: Integer;
  FrameAnchorY: Integer;
begin
  { I found some starling files which may have the last frame of the animation
    with the size set to 0 so we need check this here (division by zero error)
    example:
    https://github.com/pammimeow/fatty-starling-as3-game/blob/master/assets/sprite%20elements.xml }
  if SubTexture.Trimmed and (SubTexture.Width <> 0) and (SubTexture.Height <> 0) then
  begin
    { When frame is trimmed Width and Height does not mean the full size
      of the frame, so we have to calculate the appropriate
      anchor to get the correct position because it will not be (0.5, 0.5) }

    { Anchor in pixels (Without translation to correct texture point
      because we don't need that. Just add X1, Y1 to have correct position.) }
    FrameAnchorX := SubTexture.FrameWidth div 2 + SubTexture.FrameX;
    FrameAnchorY := SubTexture.FrameHeight div 2 + SubTexture.FrameY;

    { Convert to 0.0..1.0 coordinate system }
    AnchorX := 1 / SubTexture.Width * FrameAnchorX;
    AnchorY := 1 / SubTexture.Height * FrameAnchorY;
  end else
  begin
    AnchorX := 0.5;
    AnchorY := 0.5;
  end;
end;

procedure TCastleSpriteSheetLoader.TLoadToX3D.PrepareTexCordsForX3D(
  const SubTexture: TSubTexture;
  const ImageWidth, ImageHeight: Integer);
begin
  { The input data (X1, Y1) are the coordinates in the texture.
    We need those coordinates to compute X2, Y2 and after that we
    recalculate X1, X2 for X3D. }
  X1 := SubTexture.X;
  Y1 := SubTexture.Y;
  X2 := 1 / ImageWidth * (X1 + SubTexture.Width);
  Y2 := 1 - 1 / ImageHeight * (Y1 + SubTexture.Height);

  X1 := 1 / ImageWidth * X1;
  Y1 := 1 - 1 / ImageHeight * Y1;
end;

constructor TCastleSpriteSheetLoader.TLoadToX3D.Create(
  Loader: TCastleSpriteSheetLoader; RootNode: TX3DRootNode);
begin
  inherited Create(Loader);
  FRoot := RootNode;
  SetLength(FCoordArray, 6);
  SetLength(FTexCoordArray, 6);
end;

procedure TCastleSpriteSheetLoader.TLoadToX3D.PrepareContainer;
var
  Shape: TShapeNode;
  Tri: TTriangleSetNode;
  Tex: TImageTextureNode;
begin
  FRoot.Meta['generator'] := 'Castle Game Engine, https://castle-engine.io';
  FRoot.Meta['source'] := ExtractURIName(FLoader.FURL);

  Shape := TShapeNode.Create;
  Shape.Material := TUnlitMaterialNode.Create;

  Tex := TImageTextureNode.Create;
  Tex.FdUrl.Send(FLoader.FImagePath);
  Tex.RepeatS := false;
  Tex.RepeatT := false;
  Shape.Texture := Tex;

  Tri := TTriangleSetNode.Create;
  Tri.Solid := false;

  FShapeCoord := TCoordinateNode.Create('coord');
  FShapeCoord.SetPoint([
      FCoordArray[0],
      FCoordArray[1],
      FCoordArray[2],
      FCoordArray[3],
      FCoordArray[4],
      FCoordArray[5]]);

  FShapeTexCoord := TTextureCoordinateNode.Create('texcoord');
  FShapeTexCoord.SetPoint([
       FTexCoordArray[0],
       FTexCoordArray[1],
       FTexCoordArray[2],
       FTexCoordArray[3],
       FTexCoordArray[4],
       FTexCoordArray[5]]);

  Tri.Coord := FShapeCoord;
  Tri.TexCoord := FShapeTexCoord;
  Shape.Geometry := Tri;

  FRoot.AddChildren(Shape);
end;

procedure TCastleSpriteSheetLoader.TLoadToX3D.CalculateFrameCoords(
  const SubTexture: TSubTexture);
begin
  CalculateAnchors(SubTexture);
  PrepareTexCordsForX3D(SubTexture, FLoader.FImageWidth, FLoader.FImageHeight);

  FCoordArray[0] := Vector3(-SubTexture.Width * (AnchorX),
      SubTexture.Height * (AnchorY), 0);

  FCoordArray[1] := Vector3(SubTexture.Width * (1 - AnchorX),
      SubTexture.Height * (AnchorY), 0);

  FCoordArray[2] := Vector3(SubTexture.Width * (1 - AnchorX),
      -SubTexture.Height * (1 - AnchorY), 0);

  FCoordArray[3] := Vector3(-SubTexture.Width * AnchorX,
      SubTexture.Height * AnchorY, 0);

  FCoordArray[4] := Vector3(SubTexture.Width * (1 - AnchorX),
      -SubTexture.Height * (1 - AnchorY), 0);

  FCoordArray[5] := Vector3(-SubTexture.Width * AnchorX,
      -SubTexture.Height * (1 - AnchorY), 0);

  FTexCoordArray[0] := Vector2(X1, Y1);
  FTexCoordArray[1] := Vector2(X2, Y1);
  FTexCoordArray[2] := Vector2(X2, Y2);
  FTexCoordArray[3] := Vector2(X1, Y1);
  FTexCoordArray[4] := Vector2(X2, Y2);
  FTexCoordArray[5] := Vector2(X1, Y2);
end;

procedure TCastleSpriteSheetLoader.TLoadToX3D.PrepareAnimation(
  const Name: String);
begin
  TimeSensor := TTimeSensorNode.Create(Name);
  CoordInterp := TCoordinateInterpolatorNode.Create(Name + '_Coord');
  TexCoordInterp := TCoordinateInterpolator2DNode.Create(Name + '_TexCoord');
end;

procedure TCastleSpriteSheetLoader.TLoadToX3D.AddAnimation(
  const FrameCount: Integer);
var
  I: Integer;
  Key: Single;
begin
  { Set Cycle Interval becouse we know now frame count }
  TimeSensor.CycleInterval := FrameCount / FLoader.FFramesPerSecond;

  { Generate list of keys. }
  for I := 0 to FrameCount - 1 do
  begin
    Key := I / FrameCount;

    CoordInterp.FdKey.Items.Add(Key);
    TexCoordInterp.FdKey.Items.Add(Key);
    if I > 0 then
    begin
      CoordInterp.FdKey.Items.Add(Key);
      TexCoordInterp.FdKey.Items.Add(Key);
    end;
  end;

  { This way, we have keys like
    0 0.333 0.333 0.666 0.666 1
    That is, all keys are repeated, except 0 and 1. }
  CoordInterp.FdKey.Items.Add(1.0);
  TexCoordInterp.FdKey.Items.Add(1.0);

  { Add TimeSensor, CoordinateInterpolatorNode,
    CoordinateInterpolator2DNode to Root node }
  FRoot.AddChildren(TimeSensor);
  FRoot.AddChildren(CoordInterp);
  FRoot.AddChildren(TexCoordInterp);
  { Create routes. }
  FRoot.AddRoute(TimeSensor.EventFraction_changed, CoordInterp.EventSet_fraction);
  FRoot.AddRoute(TimeSensor.EventFraction_changed, TexCoordInterp.EventSet_fraction);
  FRoot.AddRoute(CoordInterp.EventValue_changed, FShapeCoord.FdPoint);
  FRoot.AddRoute(TexCoordInterp.EventValue_changed, FShapeTexCoord.FdPoint);
end;

procedure TCastleSpriteSheetLoader.TLoadToX3D.AddFrame;
begin
  CoordInterp.FdKeyValue.Items.AddRange(FCoordArray);
  TexCoordInterp.FdKeyValue.Items.AddRange(FTexCoordArray);
  { Repeat all keyValues, to avoid interpolating them smoothly between two keys }
  CoordInterp.FdKeyValue.Items.AddRange(FCoordArray);
  TexCoordInterp.FdKeyValue.Items.AddRange(FTexCoordArray);
end;

{ TCastleSpriteSheetLoader.TLoadStrategy }

constructor TCastleSpriteSheetLoader.TLoadStrategy.Create(
  Loader: TCastleSpriteSheetLoader);
begin
  inherited Create;
  FLoader := Loader;
end;

{ TCastleSpriteSheet }

constructor TCastleSpriteSheet.Create;
begin
  FAnimationList := TCastleSpriteSheetAnimationList.Create;
end;

destructor TCastleSpriteSheet.Destroy;
begin
  FreeAndNil(FAnimationList);
  inherited Destroy;
end;

procedure TCastleSpriteSheet.Load(const URL: String);
var
  SpriteSheetLoader: TCastleSpriteSheetLoader;
begin
  SpriteSheetLoader := TCastleSpriteSheetLoader.Create(URL);
  try
    SpriteSheetLoader.LoadToCastleSpriteSheet(Self);
  finally
    FreeAndNil(SpriteSheetLoader);
  end;
end;

procedure TCastleSpriteSheet.Save(const URL: String);
begin

end;

class function TCastleSpriteSheet.LoadToX3D(const URL: String): TX3DRootNode;
var
  SpriteSheetLoader: TCastleSpriteSheetLoader;
begin
  SpriteSheetLoader := TCastleSpriteSheetLoader.Create(URL);
  try
    Result := SpriteSheetLoader.LoadToX3D;
  finally
    FreeAndNil(SpriteSheetLoader);
  end;
end;

function TCastleSpriteSheet.AnimationByName(const Name: String): TCastleSpriteSheetAnimation;
var
  Animation: TCastleSpriteSheetAnimation;
begin
  for Animation in FAnimationList do
  begin
    if Animation.Name = Name then
      Exit(Animation);
  end;
  Result := nil;
end;

function TCastleSpriteSheet.AnimationByIndex(const Index: Integer): TCastleSpriteSheetAnimation;
begin
  Result := FAnimationList[Index];
end;

function TCastleSpriteSheet.AnimationCount: Integer;
begin
  Result := FAnimationList.Count;
end;

function TCastleSpriteSheet.HasAnimation(const Name: String): Boolean;
var
  Animation: TCastleSpriteSheetAnimation;
begin
  for Animation in FAnimationList do
  begin
    if Animation.Name = Name then
      Exit(true);
  end;
  Result := false;
end;

function TCastleSpriteSheet.AddAnimation(const Name: String): TCastleSpriteSheetAnimation;
var
  Animation: TCastleSpriteSheetAnimation;
begin
  Animation := TCastleSpriteSheetAnimation.Create(Name);
  FAnimationList.Add(Animation);
  Result := Animation;
end;

procedure TCastleSpriteSheet.RemoveAnimationByName(const Name: String);
var
  I: Integer;
  Animation: TCastleSpriteSheetAnimation;
begin
  for I := 0 to FAnimationList.Count - 1 do
  begin
    Animation := FAnimationList[I];
    if Animation.Name = Name then
    begin
      FAnimationList.Delete(I);
    end;
  end;
end;

procedure TCastleSpriteSheet.RemoveAnimationByIndex(const Index: Integer);
begin
  FAnimationList.Delete(Index);
end;

{ TCastleSpriteSheetLoader ---------------------------------------------------}

procedure TCastleSpriteSheetLoader.ReadImportSettings;
var
  SettingsMap: TStringStringMap;
  Setting: TStringStringMap.TDictionaryPair;
begin
  // default values
  FFramesPerSecond := DefaultSpriteSheetFramesPerSecond;

  SettingsMap := TStringStringMap.Create;
  try
    URIExtractSettingsFromAnchor(FURL, SettingsMap);
    for Setting in SettingsMap do
    begin
      if LowerCase(Setting.Key) = 'fps' then
      begin
        FFramesPerSecond := StrToFloatDot(Setting.Value);
      end else
      if LowerCase(Setting.Key) = 'anim-naming' then
      begin
        if Setting.Value = 'strict-underscore' then
          FSubTexture.FAnimationNaming := anStrictUnderscore
        else if Setting.Value = 'trailing-number' then
          FSubTexture.FAnimationNaming := anTralingNumber
        else
          WritelnWarning('Starling', 'Unknown anim-naming value (%s) in "%s" anchor.',
            [Setting.Value, FDisplayURL]);
      end else
        WritelnWarning('Starling', 'Unknown setting (%s) in "%s" anchor.',
          [Setting.Key, FDisplayURL]);
    end;
  finally
    FreeAndNil(SettingsMap);
  end;
end;

procedure TCastleSpriteSheetLoader.ReadImageProperties(const URL: String;
  const AtlasNode: TDOMElement);
var
  Image: TCastleImage;
begin
  FImagePath := ExtractURIPath(URL) + AtlasNode.AttributeString('imagePath');
  { Some exporters like Free Texture Packer add width and height attributes.
    In this case we don't need load image to check them. }
  if AtlasNode.HasAttribute('width') and AtlasNode.HasAttribute('height') then
  begin
    FImageWidth := AtlasNode.AttributeInteger('width');
    FImageHeight := AtlasNode.AttributeInteger('height');
  end else
  begin
    Image := LoadImage(FImagePath);
    try
      FImageWidth := Image.Width;
      FImageHeight := Image.Height;
    finally
      FreeAndNil(Image);
    end;
  end;
end;

function TCastleSpriteSheetLoader.CheckAnimationNameAvailable(
    const AnimationName: String): Boolean;
begin
  if FAnimationList.IndexOf(AnimationName) > -1 then
  begin
    WritelnWarning('Starling', 'Mixed animations tags (animation: %s) in "%s".',
      [AnimationName, FDisplayURL]);
    Exit(false);
  end;

  FAnimationList.Add(AnimationName);
  Result := true;
end;


constructor TCastleSpriteSheetLoader.Create(const URL: String);
begin
  inherited Create;
  FURL := URL;
  FDisplayURL := URIDisplay(FURL);

  FSubTexture := TSubTexture.Create;
  FAnimationList := TStringList.Create;
end;

destructor TCastleSpriteSheetLoader.Destroy;
begin
  FreeAndNil(FSubTexture);
  FreeAndNil(FAnimationList);
  inherited Destroy;
end;

function TCastleSpriteSheetLoader.LoadToX3D: TX3DRootNode;
var
  ALoadToX3D: TLoadToX3D;
  RootNode: TX3DRootNode;
begin
  RootNode := TX3DRootNode.Create;
  try
    ALoadToX3D := TLoadToX3D.Create(Self, RootNode);
    try
      Load(ALoadToX3D);
      Result := RootNode;
    finally
      FreeAndNil(ALoadToX3D);
    end;
  except
    FreeAndNil(RootNode);
    raise;
  end;
end;

procedure TCastleSpriteSheetLoader.LoadToCastleSpriteSheet(
  SpriteSheet: TCastleSpriteSheet);
var
  ALoadToCastleSpriteSheet: TLoadToSpriteSheetModel;
begin
  ALoadToCastleSpriteSheet := TLoadToSpriteSheetModel.Create(Self, SpriteSheet);
  try
    Load(ALoadToCastleSpriteSheet);
  finally
    FreeAndNil(ALoadToCastleSpriteSheet);
  end;
end;

function TCastleSpriteSheetLoader.LoadToCastleSpriteSheet: TCastleSpriteSheet;
var
  SpriteSheet: TCastleSpriteSheet;
begin
  SpriteSheet := TCastleSpriteSheet.Create;
  try
    LoadToCastleSpriteSheet(SpriteSheet);
    Result := SpriteSheet;
  except
    FreeAndNil(SpriteSheet);
    raise;
  end;
end;

procedure TCastleSpriteSheetLoader.Load(LoadTarget: TLoadStrategy);
var
  Doc: TXMLDocument;
  AtlasNode: TDOMElement;
  I: TXMLElementIterator;
  LastAnimationName: String;
  CurrentAnimFrameCount: Integer;
  FirstFrameInFirstAnimation: Boolean;
begin
  ReadImportSettings;

  LastAnimationName := '';

  Doc := nil;
  try
    Doc := URLReadXML(FURL);

    Check(Doc.DocumentElement.TagName8 = 'TextureAtlas',
      'Root of CastleSpriteSheet file must be <TextureAtlas>');

    AtlasNode := Doc.DocumentElement;
    ReadImageProperties(FURL, AtlasNode);

    CurrentAnimFrameCount := 0;
    FirstFrameInFirstAnimation := true;

    I := AtlasNode.ChildrenIterator('SubTexture');
    try
      while I.GetNext do
      begin
        { Read frame from XML }
        FSubTexture.ReadFormXMLNode(I.Current, FImageWidth, FImageHeight);

        LoadTarget.CalculateFrameCoords(FSubTexture);
        { After calculate first frame cords and tex cord we need create shape. }
        if FirstFrameInFirstAnimation then
        begin
          LoadTarget.PrepareContainer;
          ///PrepareShape(FCoordArray, FTexCoordArray);
          FirstFrameInFirstAnimation := false;
        end;

        if LastAnimationName <> FSubTexture.AnimationName then
        begin
          { First frame of animation loaded. }

          if CurrentAnimFrameCount > 0 then
            LoadTarget.AddAnimation(CurrentAnimFrameCount);

          if not CheckAnimationNameAvailable(FSubTexture.AnimationName) then
          begin
            CurrentAnimFrameCount := 0;
            Continue;
          end;

          { Reset variables for new animation }
          LastAnimationName := FSubTexture.AnimationName;
          CurrentAnimFrameCount := 1;

          LoadTarget.PrepareAnimation(LastAnimationName);
          ///TimeSensor := TTimeSensorNode.Create(LastAnimationName);
          ///CoordInterp := TCoordinateInterpolatorNode.Create(LastAnimationName + '_Coord');
          ///TexCoordInterp := TCoordinateInterpolator2DNode.Create(LastAnimationName + '_TexCoord');

          LoadTarget.AddFrame;
          ///AddFrameCoords(CoordInterp, TexCoordInterp);
        end else
        begin
          { Next frame of animation }
          Inc(CurrentAnimFrameCount);

          //AddFrameCoords(CoordInterp, TexCoordInterp);
          LoadTarget.AddFrame;
        end;
      end;

      { Add last animation }
      if CurrentAnimFrameCount > 0 then
        LoadTarget.AddAnimation(CurrentAnimFrameCount);
        ///AddAnimation(CurrentAnimFrameCount, TimeSensor, CoordInterp, TexCoordInterp);

    finally
      FreeAndNil(I);
    end;
  finally
    FreeAndNil(Doc);
  end;
end;

{ TStarlingSubTexture }

procedure TCastleSpriteSheetLoader.TSubTexture.ParseAnimationName(
  const SubTextureName: String);
var
  UnderscorePos: SizeInt;
  FrameNumberStr: String;
  FrameNumber: Integer;
  AnimationNameLength: Integer;
begin
  { Some files has file type Extensions like "walk/0001.png" or walk_1.png }
  AnimationName := DeleteFileExt(SubTextureName);

  case FAnimationNaming of
    anStrictUnderscore:
      begin
        UnderscorePos := rpos('_', AnimationName);

        { Check characters after underscore is number if not don't change name. }
        AnimationNameLength := Length(AnimationName);
        if (UnderscorePos > 0) and (AnimationNameLength > UnderscorePos) then
        begin
          FrameNumberStr := Copy(AnimationName, UnderscorePos + 1, AnimationNameLength - UnderscorePos);
          if TryStrToInt(FrameNumberStr,  FrameNumber) then
          begin
            Delete(AnimationName, UnderscorePos, AnimationNameLength - UnderscorePos + 1);
          end;
        end;
      end;
    anTralingNumber:
      begin
        RemoveTrailingChars(AnimationName, ['0'..'9']);

        if AnimationName <> '_' then // do not remove underscore if it's the only character in name
          AnimationName := SuffixRemove('_', AnimationName, false);
      end;
  end;

  if AnimationName = '' then
  begin
    WritelnWarning('Starling', 'Incorrect animation name (%s), I set to "unknown"',
      [SubTextureName]);
    AnimationName := 'unknown';
  end;
end;

procedure TCastleSpriteSheetLoader.TSubTexture.ReadFormXMLNode(
    const SubTextureNode: TDOMElement; const ImageWidth, ImageHeight: Integer);
begin
  ParseAnimationName(SubTextureNode.AttributeString('name'));

  X := SubTextureNode.AttributeInteger('x');
  Y := SubTextureNode.AttributeInteger('y');
  Width := SubTextureNode.AttributeInteger('width');
  Height := SubTextureNode.AttributeInteger('height');

  Trimmed := SubTextureNode.HasAttribute('frameX');
  if Trimmed then
  begin
    { When frame is trimmed Width and Height does not mean the full size
      of the frame, so we have to calculate the appropriate
      anchor to get the correct position because it will not be (0.5, 0.5) }

    FrameX := SubTextureNode.AttributeIntegerDef('frameX', 0);
    FrameY := SubTextureNode.AttributeIntegerDef('frameY', 0);
    FrameWidth := SubTextureNode.AttributeIntegerDef('frameWidth', Width);
    FrameHeight := SubTextureNode.AttributeIntegerDef('frameHeight', Height);
  end;
end;

constructor TCastleSpriteSheetLoader.TSubTexture.Create;
begin
  inherited Create;
  FAnimationNaming := anStrictUnderscore;
end;


end.

