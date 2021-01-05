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

  TCastleSpriteSheetAbstractAtlasGen = class;

  TCastleSpriteSheetFrameEvent = procedure (Frame: TCastleSpriteSheetFrame) of object;
  TCastleSpriteSheetFrameMoveEvent = procedure (
      const Frame: TCastleSpriteSheetFrame;
      const OldIndex, NewIndex: Integer) of object;
  TCastleSpriteSheetAnimationEvent = procedure (
      Animation: TCastleSpriteSheetAnimation) of object;
  TCastleSpriteSheetAnimationMoveEvent = procedure (
      const Animation: TCastleSpriteSheetAnimation;
      const OldIndex, NewIndex: Integer) of object;
  TCastleSpriteSheetSizeChanged = procedure (
      const Width, Height: Integer) of object;

  TCastleSpriteSheet = class
    strict private
      FAnimationList: TCastleSpriteSheetAnimationList;

      FURL: String;
      FLoadedAtlasPath: String;
      FRelativeAtlasPath: String;

      FModifiedState: Boolean;
      FLoadingPending: Boolean;
      FOnModifiedStateChanged: TNotifyEvent;
      FOnAnimationAdded: TCastleSpriteSheetAnimationEvent;
      FOnAnimationMoved: TCastleSpriteSheetAnimationMoveEvent;
      FBeforeAnimationRemoved: TCastleSpriteSheetAnimationEvent;
      FBeforeAnimationFrameRemoved: TCastleSpriteSheetFrameEvent;
      FOnFrameAdded: TCastleSpriteSheetFrameEvent;
      FOnFrameMoved: TCastleSpriteSheetFrameMoveEvent;
      FOnMaxAtlasSizeChanged: TCastleSpriteSheetSizeChanged;

    private
      { Size of atlas after loading }
      FAtlasWidth: Integer;
      FAtlasHeight: Integer;
      { Max atlas size }
      FMaxAtlasWidth: Integer;
      FMaxAtlasHeight: Integer;

      FGeneratedAtlas: TCastleImage;
    protected
      { Sets sprite sheet state as modified. }
      procedure SetModifiedState;
      { Clears modified state e.g. after saving }
      procedure ClearModifiedState;
      { Blocks ModifiedState changing }
      procedure BeginLoad;
      { Removes ModifiedState changing blocking }
      procedure EndLoad;
    public
      constructor Create;
      destructor Destroy; override;

      { Returns true if spritesheet was modified }
      function IsModified: Boolean;

      procedure Load(const URL: String);
      { Saves file to castle sprite sheet. If SaveSaveCopy = true then don't clears
        Modified state, don't change URL, don't save image paths }
      procedure Save(const URL: String; const SaveCopy: Boolean = false);
      class function LoadToX3D(const URL: String): TX3DRootNode;

      { Arranges and creates atlas image }
      procedure RegenerateAtlas;
      function AtlasCanBeRegenrated: Boolean;
      procedure SetMaxAtlasSize(const NewMaxAtlasWidth,
          NewMaxAtlasHeight: Integer);

      procedure GetMinAtlasSize(out MinWidth, MinHeight: Integer);

      function AnimationByName(const Name:String): TCastleSpriteSheetAnimation;
      function AnimationByIndex(const Index: Integer): TCastleSpriteSheetAnimation;
      function AnimationCount: Integer;
      function AnimationIndex(const Animation: TCastleSpriteSheetAnimation): Integer;
      function HasAnimation(const Name:String): Boolean;
      function AddAnimation(const Name:String): TCastleSpriteSheetAnimation;
      procedure MoveAnimationUp(const Animation: TCastleSpriteSheetAnimation);
      procedure MoveAnimationDown(const Animation: TCastleSpriteSheetAnimation);
      procedure MoveAnimationToTop(const Animation: TCastleSpriteSheetAnimation);
      procedure MoveAnimationToEnd(const Animation: TCastleSpriteSheetAnimation);
      procedure MoveAnimation(const OldIndex, NewIndex: Integer);
      procedure RemoveAnimation(const Animation: TCastleSpriteSheetAnimation);
      procedure RemoveAnimationByName(const Name: String);
      procedure RemoveAnimationByIndex(const Index: Integer);
      function ProposeAnimationName: String;

      { Last Load/Save URL. }
      property URL: String read FURL write FURL;
      { Full image path to loaded image, if empty sprite sheet was created from
        scratch. }
      property LoadedAtlasPath: String read FLoadedAtlasPath write FLoadedAtlasPath;
      { Image name with path relative to Castle Sprite Sheet/Starling XML file.
        Usually image is located next to Castle Sprite Sheet/Starling XML so
        this is only file name but it's not a rule. This file name/path is
        saved in Castle Sprite Sheet/Starling XML file.

        If empty sprite sheet was created from scratch and never saved. }
      property RelativeAtlasPath: String read FRelativeAtlasPath write FRelativeAtlasPath;

      property OnModifiedStateChanged: TNotifyEvent read FOnModifiedStateChanged
        write FOnModifiedStateChanged;

      property OnAnimationAdded: TCastleSpriteSheetAnimationEvent
        read FOnAnimationAdded write FOnAnimationAdded;

      property OnAnimationMoved: TCastleSpriteSheetAnimationMoveEvent
        read FOnAnimationMoved write FOnAnimationMoved;

      property BeforeAnimationRemoved: TCastleSpriteSheetAnimationEvent
        read FBeforeAnimationRemoved write FBeforeAnimationRemoved;

      property BeforeFrameRemoved: TCastleSpriteSheetFrameEvent read
        FBeforeAnimationFrameRemoved write FBeforeAnimationFrameRemoved;

      property OnFrameAdded: TCastleSpriteSheetFrameEvent read FOnFrameAdded
        write FOnFrameAdded;

      property OnFrameMoved: TCastleSpriteSheetFrameMoveEvent read FOnFrameMoved
        write FOnFrameMoved;

      property OnMaxAtlasSizeChanged: TCastleSpriteSheetSizeChanged read
          FOnMaxAtlasSizeChanged write FOnMaxAtlasSizeChanged;

      property AtlasWidth: Integer read FAtlasWidth;
      property AtlasHeight: Integer read FAtlasHeight;
      property MaxAtlasWidth: Integer read FMaxAtlasWidth;
      property MaxAtlasHeight: Integer read FMaxAtlasHeight;
  end;

  TCastleSpriteSheetAnimation = class
    strict private
      FName: String;
      FFramesPerSecond: Single;
      FFrameList: TCastleSpriteSheetFrameList;
      FSpriteSheet: TCastleSpriteSheet;

      procedure SetName(const NewName: String);

      function GetFrame(const Index: Integer): TCastleSpriteSheetFrame;
    private
      { Sets sprite sheet state as modified. }
      procedure SetModifiedState;
    public
      constructor Create(SpriteSheet: TCastleSpriteSheet; AName: String);
      destructor Destroy; override;

      function FrameCount: Integer;
      function FrameIndex(const Frame: TCastleSpriteSheetFrame): Integer;
      function AddFrame: TCastleSpriteSheetFrame;
      function AddFrame(const FrameImageURL: String): TCastleSpriteSheetFrame;
      function AllFramesHasTheSameSize: Boolean;
      procedure RemoveFrame(const Frame: TCastleSpriteSheetFrame);
      procedure MoveFrameUp(const Frame: TCastleSpriteSheetFrame);
      procedure MoveFrameDown(const Frame: TCastleSpriteSheetFrame);
      procedure MoveFrameToTop(const Frame: TCastleSpriteSheetFrame);
      procedure MoveFrameToEnd(const Frame: TCastleSpriteSheetFrame);
      procedure MoveFrame(const OldIndex, NewIndex: Integer);
      function GetBigestFrameSize(const MaxWidth, MaxHeight: Integer): TVector2Integer;

      property Name: String read FName write SetName;
      property Frame[Index: Integer]: TCastleSpriteSheetFrame read GetFrame;
      property FramesPerSecond: Single read FFramesPerSecond write FFramesPerSecond;
  end;

  TCastleSpriteSheetFrame = class
    strict private
      FAnimation: TCastleSpriteSheetAnimation;

      FX: Integer; // x on image
      FY: Integer; // y on image
      FWidth: Integer; // width on image
      FHeight: Integer; // height on image
      FFrameX: Integer; // relative frame x coordinate
      FFrameY: Integer; // relative frame y coordinate
      FFrameWidth: Integer; // real frame width (if not present Width have real frame width)
      FFrameHeight: Integer; // real frame height (if not present Width have real frame height)

      FTrimmed: Boolean; // FrameX, FrameY, FrameWidth, FrameHeight has real values?
      FFrameImage: TCastleImage; // Full frame image (even trimmed here is with margins)

      procedure SetX(const NewX: Integer);
      procedure SetY(const NewY: Integer);
      procedure SetWidth(const NewWidth: Integer);
      procedure SetHeight(const NewHeight: Integer);
      procedure SetFrameX(const NewFrameX: Integer);
      procedure SetFrameY(const NewFrameY: Integer);
      procedure SetFrameWidth(const NewFrameWidth: Integer);
      procedure SetFrameHeight(const NewFrameHeight: Integer);
      procedure SetTrimmed(const NewTrimmed: Boolean);

      { Sets sprite sheet state as modified. }
      procedure SetModifiedState;
    public
      constructor Create(const Animation: TCastleSpriteSheetAnimation);
      destructor Destroy; override;

      function HasFrameImage: Boolean;

      { Copies image and sets frame size. }
      procedure SetFrameImage(const SourceImage: TCastleImage);

      { Copies a fragment of the image and sets the image parameters }
      procedure SetFrameImage(const SourceImage: TCastleImage; const DestX, DestY,
          AFrameWidth, AFrameHeight, SourceX, SourceY,
          SourceWidthToCopy, SourceHeightToCopy: Integer);

      procedure DrawToImage(const DestImage: TCastleImage; const DestX, DestY,
          SourceX, SourceY, SourceWidthToDraw, SourceHeightToDraw: Integer);

      function MakeResized(const Width, Height: Integer): TCastleImage;

      procedure SaveFrameImage(const URL: String);

      class function FlipYCoordToCGE(StarlingY, ImageHeight:Integer): Integer;
      class function FlipYCoordToCGE(StarlingY, Height, ImageHeight:Integer): Integer;

      property X: Integer read FX write SetX;
      property Y: Integer read FY write SetY;
      property Width: Integer read FWidth write SetWidth;
      property Height: Integer read FHeight write SetHeight;
      property FrameX: Integer read FFrameX write SetFrameX;
      property FrameY: Integer read FFrameY write SetFrameY;
      property FrameWidth: Integer read FFrameWidth write SetFrameWidth;
      property FrameHeight: Integer read FFrameHeight write SetFrameHeight;
      property Trimmed: Boolean read FTrimmed write SetTrimmed;
      property Animation: TCastleSpriteSheetAnimation read FAnimation;
  end;

  { Abstract class for frame image "arranger", it enables the implementation
    of many algorithms }
  TCastleSpriteSheetAbstractAtlasGen = class
    protected
      FSpriteSheet: TCastleSpriteSheet;
      FSpriteSheetMaxWidth: Integer;
      FSpriteSheetMaxHeight: Integer;
    public
      constructor Create(const ASpriteSheet: TCastleSpriteSheet;
          const MaxWidth, MaxHeight: Integer);
      function WillFramesFitInSize(
          AtlasWidth, AtlasHeight: Integer): Boolean; virtual; abstract;
      procedure Generate; virtual; abstract;
  end;

  { Most simple implementation of frame arranger - for debug purposes }
  TCastleSpriteSheetBasicAtlasGen = class (TCastleSpriteSheetAbstractAtlasGen)
    private
      type
        TLayoutOperation = (
          loMeasure,
          loArrange
        );
      function LayoutFrames(const Operation: TLayoutOperation;
          const MaxAtlasWidth, MaxAtlasHeight: integer;
          out MinAtlasWidth, MinAtlasHeight: Integer): Boolean;
      procedure GenerateAtlas(const AtlasWidth, AtlasHeight: Integer);
    public
      function WillFramesFitInSize(
          AtlasWidth, AtlasHeight: Integer): Boolean; override;
      procedure GetMinAtlasSize(out MinWidth, MinHeight: Integer);
      procedure Generate; override;
  end;

  { TODO: Advaned sprite sheet generator with all features like trimming,
    padding and everything else we want }
  TCastleSpriteSheetAdvancedImageGen = class (TCastleSpriteSheetAbstractAtlasGen)
    public
      procedure Generate; override;
  end;


  { Castle Sprite Sheet XML file is not correct }
  EInvalidCastleSpriteSheetXml = class(Exception);
  ECastleSpriteSheetAtlasToSmall = class(Exception);
  ECantRegenerateAtlas = class(Exception);

implementation

uses StrUtils, DOM, Math,
  CastleFilesUtils, CastleLog, CastleStringUtils, CastleTextureImages,
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
        FLoadForEdit: Boolean;
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
      { Image name with full path }
      FAbsoluteImagePath: String;
      { Image name from CastleSpriteSheet/Starling file }
      FRelativeImagePath: String;

      FSubTexture: TSubTexture;

      { Animation list to check if the file has any mixed SubTexture nodes. }
      FAnimationList: TStringList;

    procedure ReadImportSettings;

    procedure ReadImageProperties(const URL: String; const AtlasNode: TDOMElement);

    procedure ReadFramesPerSecond(const SubTextureNode: TDOMElement);

    function CheckAnimationNameAvailable(const AnimationName: String): Boolean;

    procedure Load(LoadTarget: TLoadStrategy);

  public
    constructor Create(const URL: String);
    destructor Destroy; override;

    function LoadToX3D: TX3DRootNode;
    procedure LoadToCastleSpriteSheet(SpriteSheet: TCastleSpriteSheet);
    function LoadToCastleSpriteSheet: TCastleSpriteSheet;
  end;

  TCastleSpriteSheetXMLExporter = class
    private
      FSpriteSheet: TCastleSpriteSheet;
    public
      constructor Create(const SpriteSheet: TCastleSpriteSheet);

      function ExportToXML: TXMLDocument;
  end;

{ TCastleSpriteSheetAdvancedImageGen }

procedure TCastleSpriteSheetAdvancedImageGen.Generate;
begin
  // TODO
end;

{ TCastleSpriteSheetAbstractAtlasGen }

constructor TCastleSpriteSheetAbstractAtlasGen.Create(
  const ASpriteSheet: TCastleSpriteSheet; const MaxWidth, MaxHeight: Integer);
begin
  FSpriteSheet := ASpriteSheet;
  FSpriteSheetMaxWidth := MaxWidth;
  FSpriteSheetMaxHeight := MaxHeight;
end;

{ TCastleSpriteSheetBasicAtlasGen }

function TCastleSpriteSheetBasicAtlasGen.LayoutFrames(
  const Operation: TLayoutOperation;
  const MaxAtlasWidth, MaxAtlasHeight: integer;
  out MinAtlasWidth, MinAtlasHeight: Integer): Boolean;
var
  PreviousLineMaxY: Integer;
  I, J: Integer;
  X, Y: Integer;
  Animation: TCastleSpriteSheetAnimation;
  Frame: TCastleSpriteSheetFrame;
  CurrentMaxLineHeight: Integer;
  BigestFrameWidth, BigestFrameHeight: Integer;
  UsedMaxAtlasWidth: Integer;

  procedure CheckBigestFrameSize;
  var
    I, J: Integer;
  begin
    BigestFrameWidth := 0;
    BigestFrameHeight := 0;

    for I := 0 to FSpriteSheet.AnimationCount - 1 do
    begin
      Animation := FSpriteSheet.AnimationByIndex(I);

      for J := 0 to Animation.FrameCount - 1 do
      begin
        BigestFrameWidth := Max(BigestFrameWidth, Animation.Frame[J].Width);
        BigestFrameHeight := Max(BigestFrameHeight, Animation.Frame[J].Height);
      end;
    end;
  end;

  procedure SetFrameCords(const AFrame: TCastleSpriteSheetFrame;
    const X, Y: Integer);
  begin
    if Operation = loArrange then
    begin
      AFrame.X := X;
      AFrame.Y := Y;
    end;
  end;

begin
  X := 0;
  Y := 0;
  PreviousLineMaxY := 0;
  CurrentMaxLineHeight := 0;

  { Min size that we need for atlas }
  MinAtlasHeight := 0;
  MinAtlasWidth := 0;

  { Check if some frame is bigger than max atlas size }
  CheckBigestFrameSize;

  { If BigestFrameWidth > MaxAtlasWidth we use BigestFrameWidth to calculate
    height }
  UsedMaxAtlasWidth := Max(MaxAtlasWidth, BigestFrameWidth);

  for I := 0 to FSpriteSheet.AnimationCount - 1 do
  begin
    Animation := FSpriteSheet.AnimationByIndex(I);
    for J := 0 to Animation.FrameCount - 1 do
    begin
      Frame := Animation.Frame[J];

      // add to this line ?
      if X + Frame.FrameWidth <= UsedMaxAtlasWidth then
      begin
        // yes

        { check free height, only save if we don't have free space }
        if Frame.FrameHeight + PreviousLineMaxY > MaxAtlasHeight then
        begin
          { return that we need more space }
          MinAtlasHeight := Max(Frame.FrameHeight + PreviousLineMaxY,
            MinAtlasHeight);
        end;

        SetFrameCords(Frame, X, Y);

        X := X + Frame.FrameWidth;
        CurrentMaxLineHeight := Max(CurrentMaxLineHeight, Frame.FrameHeight);
        continue;
      end;

      { save max line width }
      MinAtlasWidth := Max(MinAtlasWidth, X);

      { Start new line (add to new line) }
      PreviousLineMaxY := PreviousLineMaxY + CurrentMaxLineHeight;
      Y := PreviousLineMaxY;
      X := 0;

      // check size
      if (Frame.FrameHeight + PreviousLineMaxY > MaxAtlasHeight) then
      begin
        MinAtlasHeight := Max(Frame.FrameHeight + PreviousLineMaxY,
          MinAtlasHeight);
      end;

      // add frame
      SetFrameCords(Frame, X, Y);

      X := X + Frame.FrameWidth;
      CurrentMaxLineHeight := Max(CurrentMaxLineHeight, Frame.FrameHeight);
    end;
  end;

  Result := (MinAtlasWidth <= MaxAtlasWidth) and
    (MinAtlasHeight <= MaxAtlasHeight);

  { Image must have size bigger than zero }
  MinAtlasWidth := Max(MinAtlasWidth, 1);
  MinAtlasHeight := Max(MinAtlasHeight, 1);
end;

procedure TCastleSpriteSheetBasicAtlasGen.GenerateAtlas(const AtlasWidth,
  AtlasHeight: Integer);
var
  Animation: TCastleSpriteSheetAnimation;
  Frame: TCastleSpriteSheetFrame;
  I, J: Integer;
begin
  FreeAndNil(FSpriteSheet.FGeneratedAtlas);
  FSpriteSheet.FGeneratedAtlas := TRGBAlphaImage.Create(AtlasWidth, AtlasHeight);
  FSpriteSheet.FGeneratedAtlas.Clear(Vector4Byte(0, 0, 0, 0));

  for I := 0 to FSpriteSheet.AnimationCount - 1 do
  begin
    Animation := FSpriteSheet.AnimationByIndex(I);
    for J := 0 to Animation.FrameCount - 1 do
    begin
      Frame := Animation.Frame[J];

      Frame.DrawToImage(FSpriteSheet.FGeneratedAtlas, Frame.X, Frame.Y, 0, 0,
        Frame.FrameWidth, Frame.FrameHeight);
    end;
  end;
  FSpriteSheet.FAtlasWidth := AtlasWidth;
  FSpriteSheet.FAtlasHeight := AtlasWidth;
end;

function TCastleSpriteSheetBasicAtlasGen.WillFramesFitInSize(AtlasWidth,
  AtlasHeight: Integer): Boolean;
var
  MinWidth, MinHeight: Integer;
begin
  Result := LayoutFrames(loMeasure, AtlasWidth, AtlasHeight, MinWidth, MinHeight);
end;

procedure TCastleSpriteSheetBasicAtlasGen.GetMinAtlasSize(out MinWidth,
  MinHeight: Integer);
begin
  LayoutFrames(loMeasure, FSpriteSheetMaxWidth, FSpriteSheetMaxHeight, MinWidth, MinHeight);
end;

procedure TCastleSpriteSheetBasicAtlasGen.Generate;
var
  MinWidth, MinHeight: Integer;
begin
  { Try use the same size }
  if LayoutFrames(loArrange, FSpriteSheet.AtlasWidth, FSpriteSheet.AtlasHeight, MinWidth,
    MinHeight) then
  begin
    GenerateAtlas(FSpriteSheet.AtlasWidth, FSpriteSheet.AtlasHeight);
    Exit;
  end;

  { Try use max size }
  if not LayoutFrames(loArrange, FSpriteSheetMaxWidth, FSpriteSheetMaxHeight,
    MinWidth, MinHeight) then
    raise ECastleSpriteSheetAtlasToSmall.Create(Format(
    'Min sprite sheet size atlas to small (%d x %d), we need %d x %d ',
    [FSpriteSheetMaxWidth, FSpriteSheetMaxHeight, MinWidth, MinHeight]));
  GenerateAtlas(FSpriteSheetMaxWidth, FSpriteSheetMaxHeight);
end;

{ TCastleSpriteSheetXMLExporter }

constructor TCastleSpriteSheetXMLExporter.Create(
  const SpriteSheet: TCastleSpriteSheet);
begin
  inherited Create;
  FSpriteSheet := SpriteSheet;
end;

function TCastleSpriteSheetXMLExporter.ExportToXML: TXMLDocument;
var
  I, J: Integer;
  Animation: TCastleSpriteSheetAnimation;
  Frame: TCastleSpriteSheetFrame;
  RootNode: TDOMNode;
  SubTextureNode: TDOMNode;
begin
  Result := TXMLDocument.Create;

  RootNode := Result.CreateElement('TextureAtlas');
  Result.AppendChild(RootNode);

  TDOMElement(RootNode).SetAttribute('imagePath', FSpriteSheet.RelativeAtlasPath);

  for I := 0 to FSpriteSheet.AnimationCount - 1 do
  begin
    Animation := FSpriteSheet.AnimationByIndex(I);

    if Animation.FrameCount = 0 then
      continue;

    for J := 0  to Animation.FrameCount - 1 do
    begin
      Frame := Animation.Frame[J];
      SubTextureNode := Result.CreateElement('SubTexture');
      RootNode.AppendChild(SubTextureNode);

      TDOMElement(SubTextureNode).SetAttribute('name', Animation.Name + '_' + IntToStr(J + 1));
      TDOMElement(SubTextureNode).SetAttribute('x', IntToStr(Frame.X));
      TDOMElement(SubTextureNode).SetAttribute('y', IntToStr(FSpriteSheet.AtlasHeight - Frame.Y - Frame.Height));
      TDOMElement(SubTextureNode).SetAttribute('width', IntToStr(Frame.Width));
      TDOMElement(SubTextureNode).SetAttribute('height', IntToStr(Frame.Height));
      if Frame.Trimmed then
      begin
        TDOMElement(SubTextureNode).SetAttribute('frameX', IntToStr(Frame.FrameX));
        TDOMElement(SubTextureNode).SetAttribute('frameY', IntToStr(Frame.FrameHeight - Frame.FrameY - Frame.Height));
        TDOMElement(SubTextureNode).SetAttribute('frameWidth', IntToStr(Frame.FrameWidth));
        TDOMElement(SubTextureNode).SetAttribute('frameHeight', IntToStr(Frame.FrameHeight));
      end;

      if J = 0 then
        TDOMElement(SubTextureNode).SetAttribute('fps', FloatToStrDot(Animation.FramesPerSecond));
    end;
  end;
end;

{ TCastleSpriteSheetFrame }

procedure TCastleSpriteSheetFrame.SetX(const NewX: Integer);
begin
  if FX = NewX then
    Exit;

  FX := NewX;
  SetModifiedState;
end;

procedure TCastleSpriteSheetFrame.SetY(const NewY: Integer);
begin
  if FY = NewY then
    Exit;

  FY := NewY;
  SetModifiedState;
end;

procedure TCastleSpriteSheetFrame.SetWidth(const NewWidth: Integer);
begin
  if FWidth = NewWidth then
    Exit;

  FWidth := NewWidth;
  SetModifiedState;
end;

procedure TCastleSpriteSheetFrame.SetHeight(const NewHeight: Integer);
begin
  if FHeight = NewHeight then
    Exit;

  FHeight := NewHeight;
  SetModifiedState;
end;

procedure TCastleSpriteSheetFrame.SetFrameX(const NewFrameX: Integer);
begin
  if FFrameX = NewFrameX then
    Exit;

  FFrameX := NewFrameX;
  SetModifiedState;
end;

procedure TCastleSpriteSheetFrame.SetFrameY(const NewFrameY: Integer);
begin
  if FFrameY = NewFrameY then
    Exit;

  FFrameY := NewFrameY;
  SetModifiedState;
end;

procedure TCastleSpriteSheetFrame.SetFrameWidth(const NewFrameWidth: Integer);
begin
  if FFrameWidth = NewFrameWidth then
    Exit;

  FFrameWidth := NewFrameWidth;
  SetModifiedState;
end;

procedure TCastleSpriteSheetFrame.SetFrameHeight(const NewFrameHeight: Integer);
begin
  if FFrameHeight = NewFrameHeight then
    Exit;

  FFrameHeight := NewFrameHeight;
  SetModifiedState;
end;

procedure TCastleSpriteSheetFrame.SetTrimmed(const NewTrimmed: Boolean);
begin
  if FTrimmed = NewTrimmed then
    Exit;

  FTrimmed := NewTrimmed;
  SetModifiedState;
end;

procedure TCastleSpriteSheetFrame.SetModifiedState;
begin
  FAnimation.SetModifiedState;
end;

constructor TCastleSpriteSheetFrame.Create(
  const Animation: TCastleSpriteSheetAnimation);
begin
  Assert(Animation <> nil, 'Animation can''t be nil when creating frame!');

  FAnimation := Animation;
end;

destructor TCastleSpriteSheetFrame.Destroy;
begin
  FreeAndNil(FFrameImage);
  inherited Destroy;
end;

function TCastleSpriteSheetFrame.HasFrameImage: Boolean;
begin
  Result := FFrameImage <> nil;
end;

procedure TCastleSpriteSheetFrame.SetFrameImage(const SourceImage: TCastleImage);
begin
  FreeAndNil(FFrameImage);
  Width := SourceImage.Width;
  Height := SourceImage.Height;

  FrameX := 0;
  FrameY := 0;
  FrameWidth := SourceImage.Width;
  FrameHeight := SourceImage.Height;

  FFrameImage := SourceImage.MakeCopy;
  SetModifiedState;
end;

procedure TCastleSpriteSheetFrame.SetFrameImage(
  const SourceImage: TCastleImage; const DestX, DestY, AFrameWidth,
  AFrameHeight, SourceX, SourceY, SourceWidthToCopy, SourceHeightToCopy: Integer);
begin
  Trimmed := not ((DestX = 0) and (DestY = 0));

  { update frame settings }
  X := SourceX;
  Y := SourceY;
  Width := SourceWidthToCopy;
  Height := SourceHeightToCopy;

  FrameX := DestX;
  FrameY := DestY;

  FrameWidth := AFrameWidth;
  FrameHeight := AFrameHeight;

  FreeAndNil(FFrameImage);

  if not Trimmed then
    FFrameImage := SourceImage.MakeExtracted(X, Y, Width, Height)
  else
  begin
    { When Image is trimmed we can't simply extract image part }
    FFrameImage := TRGBAlphaImage.Create(FrameWidth, FrameHeight);
    FFrameImage.Clear(Vector4Byte(0, 0, 0, 0));
    FFrameImage.DrawFrom(SourceImage,
      FrameX, // destination X
      FrameY, // destination Y
      X, // source X
      Y, // source Y
      Width,
      Height,
      dmOverwrite
    );
  end;

  SetModifiedState;
end;

procedure TCastleSpriteSheetFrame.DrawToImage(const DestImage: TCastleImage;
  const DestX, DestY, SourceX, SourceY, SourceWidthToDraw,
  SourceHeightToDraw: Integer);
begin
  Assert(FFrameImage <> nil, 'No frame image to draw.');
  DestImage.DrawFrom(FFrameImage, DestX, DestY, SourceX, SourceY, SourceWidthToDraw,
    SourceHeightToDraw, dmOverwrite);
end;

function TCastleSpriteSheetFrame.MakeResized(const Width, Height: Integer
  ): TCastleImage;
begin
  Assert(FFrameImage <> nil, 'No frame image to resize.');
  Result := FFrameImage.MakeResized(Width, Height);
end;

procedure TCastleSpriteSheetFrame.SaveFrameImage(const URL: String);
begin
  SaveImage(FFrameImage, URL);
end;

class function TCastleSpriteSheetFrame.FlipYCoordToCGE(StarlingY,
  ImageHeight: Integer): Integer;
begin
  Result := ImageHeight - StarlingY;
end;

class function TCastleSpriteSheetFrame.FlipYCoordToCGE(StarlingY, Height,
  ImageHeight: Integer): Integer;
begin
  Result := ImageHeight - (StarlingY + Height);
end;

{ TCastleSpriteSheetLoader.TLoadToSpriteSheetModel }

constructor TCastleSpriteSheetLoader.TLoadToSpriteSheetModel.Create(
  Loader: TCastleSpriteSheetLoader; SpriteSheet: TCastleSpriteSheet);
begin
  inherited Create(Loader);
  FSpriteSheet := SpriteSheet;
  FLoadForEdit := true;
end;

destructor TCastleSpriteSheetLoader.TLoadToSpriteSheetModel.Destroy;
begin
  FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TCastleSpriteSheetLoader.TLoadToSpriteSheetModel.PrepareContainer;
begin
  FSpriteSheet.URL := FLoader.FURL;
  FSpriteSheet.LoadedAtlasPath := FLoader.FAbsoluteImagePath;
  FSpriteSheet.RelativeAtlasPath := FLoader.FRelativeImagePath;
  FImage := LoadImage(FLoader.FAbsoluteImagePath);
  FSpriteSheet.FAtlasWidth := FLoader.FImageWidth;
  FSpriteSheet.FAtlasHeight := FLoader.FImageHeight;
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
  FrameYInCGECoords: Integer;
  YInCGECoords: Integer;
begin
  Frame := FCurrentAnimation.AddFrame;

  // we need to go to CGE coords here
  if Frame.Trimmed then
    FrameYInCGECoords := FSubTexture.FrameHeight - FSubTexture.FrameY - FSubTexture.Height
  else
    FrameYInCGECoords := 0;

  YInCGECoords := FLoader.FImageHeight - FSubTexture.Y - FSubTexture.Height;

  if not FLoadForEdit then
  begin
    Frame.Width := FSubTexture.Width;
    Frame.Height := FSubTexture.Height;
    Frame.Trimmed := FSubTexture.Trimmed;
    Frame.X := FSubTexture.X;
    Frame.Y := YInCGECoords;
    if Frame.Trimmed then
    begin
      Frame.FrameWidth := FSubTexture.FrameWidth;
      Frame.FrameHeight := FSubTexture.FrameHeight;
      Frame.FrameX := FSubTexture.FrameX;
      Frame.FrameY := FrameYInCGECoords;
    end else
    begin
      // make data always OK
      Frame.FrameX := 0;
      Frame.FrameY := 0;
      Frame.FrameWidth := Frame.Width;
      Frame.FrameHeight := Frame.Height;
    end;
    Exit;
  end;

  { If we want load sprite sheet for edit }
  Frame.SetFrameImage(FImage,
    FSubTexture.FrameX,
    FrameYInCGECoords,
    FSubTexture.FrameWidth,
    FSubTexture.FrameHeight,
    FSubTexture.X,
    YInCGECoords,
    FSubTexture.Width,
    FSubTexture.Height
  );
end;

{ TCastleSpriteSheetAnimation }

constructor TCastleSpriteSheetAnimation.Create(SpriteSheet: TCastleSpriteSheet; AName: String);
begin
  Assert(SpriteSheet <> nil, 'Sprite sheet can''t be nil when creating animation!');
  FSpriteSheet := SpriteSheet;
  FName := AName;
  FFramesPerSecond := DefaultSpriteSheetFramesPerSecond;
  FFrameList := TCastleSpriteSheetFrameList.Create;
end;

destructor TCastleSpriteSheetAnimation.Destroy;
begin
  FreeAndNil(FFrameList);
  inherited Destroy;
end;

procedure TCastleSpriteSheetAnimation.SetModifiedState;
begin
  FSpriteSheet.SetModifiedState;
end;

procedure TCastleSpriteSheetAnimation.SetName(const NewName: String);
begin
  if NewName = FName then
    Exit;

  FName := NewName;
  SetModifiedState;
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

function TCastleSpriteSheetAnimation.FrameIndex(
  const Frame: TCastleSpriteSheetFrame): Integer;
begin
  Result := FFrameList.IndexOf(Frame);
end;

function TCastleSpriteSheetAnimation.AddFrame: TCastleSpriteSheetFrame;
var
  AFrame: TCastleSpriteSheetFrame;
begin
  AFrame := TCastleSpriteSheetFrame.Create(Self);
  FFrameList.Add(AFrame);
  Result := AFrame;
  if Assigned(FSpriteSheet.OnFrameAdded) then
    FSpriteSheet.OnFrameAdded(AFrame);
  SetModifiedState;
end;

function TCastleSpriteSheetAnimation.AddFrame(const FrameImageURL: String
  ): TCastleSpriteSheetFrame;
var
  AFrame: TCastleSpriteSheetFrame;
  Image: TCastleImage;
begin
  AFrame := TCastleSpriteSheetFrame.Create(Self);
  try
    Image := LoadImage(FrameImageURL);
    try
      AFrame.SetFrameImage(Image);
    finally
      FreeAndNil(Image);
    end;

    FFrameList.Add(AFrame);

    if Assigned(FSpriteSheet.OnFrameAdded) then
      FSpriteSheet.OnFrameAdded(AFrame);

    SetModifiedState;
  except
    FreeAndNil(AFrame);
  end;
  Result := AFrame;
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

procedure TCastleSpriteSheetAnimation.RemoveFrame(
  const Frame: TCastleSpriteSheetFrame);
begin
  if (Assigned(FSpriteSheet.BeforeFrameRemoved)) and
    (FFrameList.Contains(Frame)) then
    FSpriteSheet.BeforeFrameRemoved(Frame);
  FFrameList.Remove(Frame);
  SetModifiedState;
end;

procedure TCastleSpriteSheetAnimation.MoveFrameUp(
  const Frame: TCastleSpriteSheetFrame);
var
  Index: Integer;
begin
  Index := FFrameList.IndexOf(Frame);
  if Index < 1 then
    Exit;

  MoveFrame(Index, Index - 1);
end;

procedure TCastleSpriteSheetAnimation.MoveFrameDown(
  const Frame: TCastleSpriteSheetFrame);
var
  Index: Integer;
begin
  Index := FFrameList.IndexOf(Frame);
  if (Index = FFrameList.Count - 1) or (Index = -1) then
    Exit;

  MoveFrame(Index, Index + 1);
end;

procedure TCastleSpriteSheetAnimation.MoveFrameToTop(
  const Frame: TCastleSpriteSheetFrame);
var
  Index: Integer;
begin
  Index := FFrameList.IndexOf(Frame);
  if Index < 1 then
    Exit;

  MoveFrame(Index, 0);
end;

procedure TCastleSpriteSheetAnimation.MoveFrameToEnd(
  const Frame: TCastleSpriteSheetFrame);
var
  Index: Integer;
begin
  Index := FFrameList.IndexOf(Frame);
  if (Index = FFrameList.Count - 1) or (Index = -1) then
    Exit;

  MoveFrame(Index, FFrameList.Count - 1);
end;

procedure TCastleSpriteSheetAnimation.MoveFrame(const OldIndex,
  NewIndex: Integer);
begin
  FFrameList.Move(OldIndex, NewIndex);

  if Assigned(FSpriteSheet.OnFrameMoved) then
    FSpriteSheet.OnFrameMoved(FFrameList[NewIndex], OldIndex, NewIndex);

  SetModifiedState;
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
  Tex.FdUrl.Send(FLoader.FAbsoluteImagePath);
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

procedure TCastleSpriteSheet.SetModifiedState;
begin
  if FModifiedState then
    Exit;

  if FLoadingPending then
    Exit;

  FModifiedState := true;
  if Assigned(FOnModifiedStateChanged) then
    FOnModifiedStateChanged(Self);
end;

procedure TCastleSpriteSheet.ClearModifiedState;
begin
  if not FModifiedState then
    Exit;

  FModifiedState := false;

  if Assigned(FOnModifiedStateChanged) then
    FOnModifiedStateChanged(Self);
end;

function TCastleSpriteSheet.IsModified: Boolean;
begin
  Result := FModifiedState;
end;

procedure TCastleSpriteSheet.BeginLoad;
begin
  FLoadingPending := true;
end;

procedure TCastleSpriteSheet.EndLoad;
begin
  FLoadingPending := false;
end;

constructor TCastleSpriteSheet.Create;
begin
  FAnimationList := TCastleSpriteSheetAnimationList.Create;
  FAtlasWidth := 1024;
  FAtlasHeight := 1024;
  FMaxAtlasWidth := 1024;
  FMaxAtlasHeight := 1024;
end;

destructor TCastleSpriteSheet.Destroy;
begin
  FreeAndNil(FGeneratedAtlas);
  FreeAndNil(FAnimationList);
  inherited Destroy;
end;

procedure TCastleSpriteSheet.Load(const URL: String);
var
  SpriteSheetLoader: TCastleSpriteSheetLoader;
begin
  SpriteSheetLoader := nil;
  BeginLoad;
  try
    SpriteSheetLoader := TCastleSpriteSheetLoader.Create(URL);
    SpriteSheetLoader.LoadToCastleSpriteSheet(Self);
  finally
    FreeAndNil(SpriteSheetLoader);
    EndLoad;
  end;
end;

procedure TCastleSpriteSheet.Save(const URL: String;
  const SaveCopy: Boolean = false);
var
  ExporterXML: TCastleSpriteSheetXMLExporter;
  XMLDoc: TXMLDocument;
  AtlasURL: String;

  FOldRelativeImagePath: String;
begin
  if SaveCopy then
  begin
    FOldRelativeImagePath := FRelativeAtlasPath;
  end;

  if IsModified or (LoadedAtlasPath = '') then
    RegenerateAtlas;
  try
    { Generate atlas file name/path, use PNG as main image file format.
      Maybe we should add option to set file name by user. }

    if FRelativeAtlasPath = '' then
    begin
      FRelativeAtlasPath := DeleteURIExt(ExtractURIName(URL)) + '.png';
    end;
    AtlasURL := URIIncludeSlash(ExtractURIPath(URL)) + FRelativeAtlasPath;

    { Save image file }
    if FGeneratedAtlas = nil then
      CheckCopyFile(URIToFilenameSafe(LoadedAtlasPath), URIToFilenameSafe(AtlasURL))
    else
      SaveImage(FGeneratedAtlas, AtlasURL);

    { Save xml (Starling) file }
    ExporterXML := nil;
    XMLDoc := nil;
    try
      ExporterXML := TCastleSpriteSheetXMLExporter.Create(Self);
      XMLDoc := ExporterXML.ExportToXML;
      URLWriteXML(XMLDoc, URL);
    finally
      FreeAndNil(ExporterXML);
      FreeAndNil(XMLDoc);
    end;

    if not SaveCopy then
      FURL := URL;
  finally
    if SaveCopy then
      FRelativeAtlasPath := FOldRelativeImagePath;
  end;
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

procedure TCastleSpriteSheet.RegenerateAtlas;
var
  BasicImageGen: TCastleSpriteSheetBasicAtlasGen;
begin
  if not AtlasCanBeRegenrated then
    raise ECantRegenerateAtlas.Create('Frames images not loaded can''t regenerate atlas.');

  BasicImageGen := TCastleSpriteSheetBasicAtlasGen.Create(Self, FMaxAtlasWidth,
    FMaxAtlasHeight);
  try
    BasicImageGen.Generate;
  finally
    FreeAndNil(BasicImageGen);
  end;
end;

function TCastleSpriteSheet.AtlasCanBeRegenrated: Boolean;
var
  I, J: Integer;
  Animation: TCastleSpriteSheetAnimation;
  Frame: TCastleSpriteSheetFrame;
begin
  for I := 0 to AnimationCount - 1 do
  begin
    Animation := AnimationByIndex(I);
    for J := 0 to Animation.FrameCount - 1 do
    begin
      Frame := Animation.Frame[J];
      if not Frame.HasFrameImage then
        Exit(false);
    end;
  end;
  Result := true;
end;

procedure TCastleSpriteSheet.SetMaxAtlasSize(const NewMaxAtlasWidth,
  NewMaxAtlasHeight: Integer);
begin
  if (FMaxAtlasWidth = NewMaxAtlasWidth) and
    (FMaxAtlasHeight = NewMaxAtlasHeight) then
    Exit;

  FMaxAtlasWidth := NewMaxAtlasWidth;
  FMaxAtlasHeight := NewMaxAtlasHeight;
  if Assigned(FOnMaxAtlasSizeChanged) then
    FOnMaxAtlasSizeChanged(FMaxAtlasWidth, FMaxAtlasHeight);
  SetModifiedState;
end;

procedure TCastleSpriteSheet.GetMinAtlasSize(out MinWidth, MinHeight: Integer);
var
  BasicImageGen: TCastleSpriteSheetBasicAtlasGen;
begin
  BasicImageGen := TCastleSpriteSheetBasicAtlasGen.Create(Self, FMaxAtlasWidth,
    FMaxAtlasHeight);
  try
    BasicImageGen.GetMinAtlasSize(MinWidth, MinHeight);
  finally
    FreeAndNil(BasicImageGen);
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

function TCastleSpriteSheet.AnimationIndex(
  const Animation: TCastleSpriteSheetAnimation): Integer;
begin
  Result := FAnimationList.IndexOf(Animation);
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
  Animation := TCastleSpriteSheetAnimation.Create(Self, Name);
  FAnimationList.Add(Animation);
  if Assigned(FOnAnimationAdded) then
    FOnAnimationAdded(Animation);
  SetModifiedState;
  Result := Animation;
end;

procedure TCastleSpriteSheet.MoveAnimationUp(
  const Animation: TCastleSpriteSheetAnimation);
var
  Index: Integer;
begin
  Index := FAnimationList.IndexOf(Animation);
  if Index < 1 then
    Exit;

  MoveAnimation(Index, Index - 1);
end;

procedure TCastleSpriteSheet.MoveAnimationDown(
  const Animation: TCastleSpriteSheetAnimation);
var
  Index: Integer;
begin
  Index := FAnimationList.IndexOf(Animation);
  if (Index = FAnimationList.Count - 1) or (Index = -1) then
    Exit;

  MoveAnimation(Index, Index + 1);
end;

procedure TCastleSpriteSheet.MoveAnimationToTop(
  const Animation: TCastleSpriteSheetAnimation);
var
  Index: Integer;
begin
  Index := FAnimationList.IndexOf(Animation);
  if Index < 1 then
    Exit;

  MoveAnimation(Index, 0);
end;

procedure TCastleSpriteSheet.MoveAnimationToEnd(
  const Animation: TCastleSpriteSheetAnimation);
var
  Index: Integer;
begin
  Index := FAnimationList.IndexOf(Animation);
  if (Index = FAnimationList.Count - 1) or (Index = -1) then
    Exit;

  MoveAnimation(Index, FAnimationList.Count - 1);
end;

procedure TCastleSpriteSheet.MoveAnimation(const OldIndex, NewIndex: Integer);
begin
  FAnimationList.Move(OldIndex, NewIndex);

  if Assigned(FOnAnimationMoved) then
    FOnAnimationMoved(FAnimationList[NewIndex], OldIndex, NewIndex);

  SetModifiedState;
end;

procedure TCastleSpriteSheet.RemoveAnimation(
  const Animation: TCastleSpriteSheetAnimation);
begin
  if (Assigned(FBeforeAnimationRemoved)) and
    (FAnimationList.Contains(Animation)) then
    FBeforeAnimationRemoved(Animation);
  FAnimationList.Remove(Animation);
  SetModifiedState;
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
      if Assigned(FBeforeAnimationRemoved) then
        FBeforeAnimationRemoved(Animation);
      FAnimationList.Delete(I);
      SetModifiedState;
    end;
  end;
end;

procedure TCastleSpriteSheet.RemoveAnimationByIndex(const Index: Integer);
begin
  if Assigned(FBeforeAnimationRemoved) then
    FBeforeAnimationRemoved(FAnimationList[Index]);

  FAnimationList.Delete(Index);
  SetModifiedState;
end;

function TCastleSpriteSheet.ProposeAnimationName: String;
const
  NamePrefix = 'NewAnim';
var
  I: Integer;
begin
  if not HasAnimation(NamePrefix) then
    Exit (NamePrefix);

  I := 1;

  while true do
  begin
    if not HasAnimation(NamePrefix + IntTostr(I)) then
      Exit(NamePrefix + IntTostr(I));
    Inc(I);
  end;
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
  FRelativeImagePath := AtlasNode.AttributeString('imagePath');
  FAbsoluteImagePath := ExtractURIPath(URL) + FRelativeImagePath;
  { Some exporters like Free Texture Packer add width and height attributes.
    In this case we don't need load image to check them. }
  if AtlasNode.HasAttribute('width') and AtlasNode.HasAttribute('height') then
  begin
    FImageWidth := AtlasNode.AttributeInteger('width');
    FImageHeight := AtlasNode.AttributeInteger('height');
  end else
  begin
    Image := LoadImage(FAbsoluteImagePath);
    try
      FImageWidth := Image.Width;
      FImageHeight := Image.Height;
    finally
      FreeAndNil(Image);
    end;
  end;
end;

procedure TCastleSpriteSheetLoader.ReadFramesPerSecond(
  const SubTextureNode: TDOMElement);
begin
  FFramesPerSecond := SubTextureNode.AttributeSingleDef('fps', FFramesPerSecond);
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

          { Check fps (CastleSpriteSheet extension) }
          ReadFramesPerSecond(I.Current);

          LoadTarget.PrepareAnimation(LastAnimationName);

          LoadTarget.AddFrame;
        end else
        begin
          { Next frame of animation }
          Inc(CurrentAnimFrameCount);

          LoadTarget.AddFrame;
        end;
      end;

      { Add last animation }
      if CurrentAnimFrameCount > 0 then
        LoadTarget.AddAnimation(CurrentAnimFrameCount);

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
  FrameX := SubTextureNode.AttributeIntegerDef('frameX', 0);
  FrameY := SubTextureNode.AttributeIntegerDef('frameY', 0);
  FrameWidth := SubTextureNode.AttributeIntegerDef('frameWidth', Width);
  FrameHeight := SubTextureNode.AttributeIntegerDef('frameHeight', Height);
end;

constructor TCastleSpriteSheetLoader.TSubTexture.Create;
begin
  inherited Create;
  FAnimationNaming := anStrictUnderscore;
end;


end.

