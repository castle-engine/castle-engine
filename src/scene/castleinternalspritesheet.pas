{
  Copyright 2020-2021 Andrzej Kilijanski.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Sprite sheet representation for loading and editing purposes (TCastleSpriteSheet). }
unit CastleInternalSpriteSheet;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, Generics.Collections,
  X3DNodes,
  CastleImages, CastleVectors;

type
  TCastleSpriteSheetAnimation = class;
  TCastleSpriteSheetFrame = class;

  TCastleSpriteSheetAnimationList = {$ifdef FPC}specialize{$endif} TObjectList<TCastleSpriteSheetAnimation>;
  TCastleSpriteSheetFrameList = {$ifdef FPC}specialize{$endif} TObjectList<TCastleSpriteSheetFrame>;

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
  TURLContentChanged = procedure (const ChangedURL: String);

  { Sprite sheet representation for loading and editing purposes. }
  TCastleSpriteSheet = class
  strict private
    FAnimationList: TCastleSpriteSheetAnimationList;

    FURL: String;
    FLoadedAtlasPath: String;
    FRelativeAtlasPath: String;

    FEditMode: Boolean;

    FModifiedState: Boolean;
    FLoadingPending: Boolean;

    FOnURLChanged: TNotifyEvent;
    FOnModifiedStateChanged: TNotifyEvent;
    FOnAnimationAdded: TCastleSpriteSheetAnimationEvent;
    FOnAnimationMoved: TCastleSpriteSheetAnimationMoveEvent;
    FBeforeAnimationRemoved: TCastleSpriteSheetAnimationEvent;
    FBeforeAnimationFrameRemoved: TCastleSpriteSheetFrameEvent;
    FOnFrameAdded: TCastleSpriteSheetFrameEvent;
    FOnFrameMoved: TCastleSpriteSheetFrameMoveEvent;
    FOnMaxAtlasSizeChanged: TCastleSpriteSheetSizeChanged;
    FOnFileSaved: TURLContentChanged;

  private
    { Size of atlas after loading }
    FAtlasWidth: Integer;
    FAtlasHeight: Integer;
    { Max atlas size }
    FMaxAtlasWidth: Integer;
    FMaxAtlasHeight: Integer;

    FGeneratedAtlas: TCastleImage;

    procedure SetURL(const NewURL: String);

  protected
    { Sets sprite sheet state as modified. }
    procedure SetModifiedState;
    { Clears modified state e.g. after saving }
    procedure ClearModifiedState;
    { Blocks ModifiedState changing }
    procedure BeginLoad;
    { Removes ModifiedState changing blocking }
    procedure EndLoad;
    { Throws EReadOnlyMode exception when EditMode = false }
    procedure CheckEditMode;
  public
    constructor Create(AEditMode: Boolean);
    destructor Destroy; override;

    { Returns true if spritesheet was modified }
    function IsModified: Boolean;

    procedure Load(const URL: String); overload;
    procedure Load(const Stream: TStream; const BaseUrl: String); overload;
    { Saves file to castle sprite sheet. If SaveSaveCopy = true then don't clears
      Modified state, don't change URL, don't save image paths }
    procedure Save(const AURL: String; const SaveCopy: Boolean = false);

    function ToX3D: TX3DRootNode;

    { Arranges and creates atlas image }
    procedure RegenerateAtlas;
    { This checks: Have all frames loaded frame images? }
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

    { Last Load/Save URL. Can be empty when loaded from Starling file. }
    property URL: String read FURL write SetURL;
    { Full image path to loaded image, if empty sprite sheet was created from
      scratch. }
    property LoadedAtlasPath: String read FLoadedAtlasPath write FLoadedAtlasPath;
    { Image name with path relative to Castle Sprite Sheet/Starling XML file.
      Usually image is located next to Castle Sprite Sheet/Starling XML so
      this is only file name but it's not a rule. This file name/path is
      saved in Castle Sprite Sheet/Starling XML file.

      Can be empty when sprite sheet was created from scratch and never saved. }
    property RelativeAtlasPath: String read FRelativeAtlasPath
        write FRelativeAtlasPath;

    property OnURLChanged: TNotifyEvent read FOnURLChanged write FOnURLChanged;

    { OnFileSaved event can be used for reload TCastleScenes with this sprite
      sheet loaded }
    property OnFileSaved: TURLContentChanged read FOnFileSaved
        write FOnFileSaved;

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
    property EditMode: Boolean read FEditMode;
  end;

  { Sprite sheet animation, part of TCastleSpriteSheet. }
  TCastleSpriteSheetAnimation = class
  strict private
    FName: String;
    FFramesPerSecond: Single;
    FFrameList: TCastleSpriteSheetFrameList;
    FSpriteSheet: TCastleSpriteSheet;

    procedure SetName(const NewName: String);
    procedure SetFramesPerSecond(const NewFPS: Single);

    function GetFrame(const Index: Integer): TCastleSpriteSheetFrame;

  private
    { Sets sprite sheet state as modified. }
    procedure SetModifiedState;

    { Throws EReadOnlyMode exception when EditMode = false }
    procedure CheckEditMode;
  public
    constructor Create(SpriteSheet: TCastleSpriteSheet; AName: String);
    destructor Destroy; override;

    function FrameCount: Integer;
    function FrameIndex(const Frame: TCastleSpriteSheetFrame): Integer;
    function AddFrame: TCastleSpriteSheetFrame; overload;
    function AddFrameCopy(const SrcFrame: TCastleSpriteSheetFrame
        ): TCastleSpriteSheetFrame;
    function AddFrame(const FrameImageURL: String):
      TCastleSpriteSheetFrame; overload;
    function AddFrame(const SourceImage: TCastleImage; const DestX, DestY,
        AFrameWidth, AFrameHeight, SourceX, SourceY,
        SourceWidthToCopy, SourceHeightToCopy: Integer):
        TCastleSpriteSheetFrame; overload;
    function AllFramesHasTheSameSize: Boolean;
    procedure RemoveFrame(const Frame: TCastleSpriteSheetFrame);
    procedure MoveFrameLeft(const Frame: TCastleSpriteSheetFrame);
    procedure MoveFrameRight(const Frame: TCastleSpriteSheetFrame);
    procedure MoveFrameToTop(const Frame: TCastleSpriteSheetFrame);
    procedure MoveFrameToEnd(const Frame: TCastleSpriteSheetFrame);
    procedure MoveFrame(const OldIndex, NewIndex: Integer);
    procedure ImportAtlas(AtlasImageURL: String; Cols, Rows: Integer;
        ImportByColumns: Boolean);
    function GetBigestFrameSize(const MaxWidth, MaxHeight: Integer): TVector2Integer;

    property Name: String read FName write SetName;
    property Frame[const Index: Integer]: TCastleSpriteSheetFrame read GetFrame;
    property FramesPerSecond: Single read FFramesPerSecond write SetFramesPerSecond;
  end;

  { Sprite sheet animation frame, part of TCastleSpriteSheetAnimation. }
  TCastleSpriteSheetFrame = class
  strict private
    FAnimation: TCastleSpriteSheetAnimation;

    FXInAtlas: Integer; // x in atlas
    FYInAtlas: Integer; // y in atlas
    FWidthInAtlas: Integer; // width in atlas, this always will be shape width
    FHeightInAtlas: Integer; // height in atlas, this always will be shape height

    { When frame is trimmed, then atlas rect is not full size of the
      frame. So our shape is smaller too, to make it apear in right place we
      need shift all the coordinates of the our shape. We do that by calculating
      anchor from LeftOffset, FrameWidth, and shift vertices coordinates.
      See TCastleSpriteSheetX3DExporter.CalculateAnchors().

      In default Anchor is 0.5, 0.5 so shape is centered, when
      (FrameWidth - WidthInAtlas) / 2 = LeftOffset anchor will be 0.5
      but when LeftOffset * 2 + WidthInAtlas <> FrameWidth anchor will change
      and shape coords will be shifted.

      Note that the left offset may be different from the right one.
      Likewise, top and bottom offset. Right and bottom offset is not saved in
      Starling XML.
    }
    FTrimmed: Boolean; // is frame trimmed ?
    FLeftOffset: Integer; // relative x cord offset, used for trimming (left offset)
    FTopOffset: Integer; // relative y cord offset, used for trimming (top offset)
    FFrameWidth: Integer; // real frame width
    FFrameHeight: Integer; // real frame height

    FFrameImage: TCastleImage; // Full frame image (even trimmed here is with margins)

    procedure SetXInAtlas(const NewX: Integer);
    procedure SetYInAtlas(const NewY: Integer);
    procedure SetWidthInAtlas(const NewWidth: Integer);
    procedure SetHeightInAtlas(const NewHeight: Integer);
    procedure SetLeftOffset(const NewLeftOffset: Integer);
    procedure SetTopOffset(const NewTopOffset: Integer);
    procedure SetFrameWidth(const NewFrameWidth: Integer);
    procedure SetFrameHeight(const NewFrameHeight: Integer);
    procedure SetTrimmed(const NewTrimmed: Boolean);

    { Sets sprite sheet state as modified. }
    procedure SetModifiedState;
    procedure CheckEditMode;
  protected
    { Currently only reset to full frame size }
    procedure UpdateTrimming;
  public
    constructor Create(const Animation: TCastleSpriteSheetAnimation); overload;
    { Construct full/deep copy of SrcFrame }
    constructor Create(const Animation: TCastleSpriteSheetAnimation;
        const SrcFrame: TCastleSpriteSheetFrame); overload;
    destructor Destroy; override;

    function HasFrameImage: Boolean;

    { Copies image and sets frame size. }
    procedure SetFrameImage(const SourceImage: TCastleImage); overload;

    { Copies a fragment of the image and sets the image parameters }
    procedure SetFrameImage(const SourceImage: TCastleImage; const DestX, DestY,
        AFrameWidth, AFrameHeight, SourceX, SourceY,
        SourceWidthToCopy, SourceHeightToCopy: Integer); overload;

    procedure DrawToImage(const DestImage: TCastleImage; const DestX, DestY,
        SourceX, SourceY, SourceWidthToDraw, SourceHeightToDraw: Integer);

    function MakeResized(const Width, Height: Integer): TCastleImage;
    function MakeResizedWithBg(const Width, Height: Integer;
        const BgColor: TVector4): TCastleImage;
    function MakeImageCopy: TCastleImage;
    function MakeImageCopyWithBg(const BgColor: TVector4): TCastleImage;
    function CenterOnBiggerImage(const Width, Height: Integer;
        const BgColor: TVector4): TCastleImage;

    procedure SaveFrameImage(const URL: String);

    class function FlipYCoordToCGE(StarlingY, ImageHeight:Integer):
      Integer; overload;
    class function FlipYCoordToCGE(StarlingY, Height, ImageHeight:Integer):
      Integer; overload;

    property XInAtlas: Integer read FXInAtlas write SetXInAtlas;
    property YInAtlas: Integer read FYInAtlas write SetYInAtlas;
    property WidthInAtlas: Integer read FWidthInAtlas write SetWidthInAtlas;
    property HeightInAtlas: Integer read FHeightInAtlas write SetHeightInAtlas;
    property LeftOffset: Integer read FLeftOffset write SetLeftOffset;
    property TopOffset: Integer read FTopOffset write SetTopOffset;
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
    procedure GetMinAtlasSize(
        out MinWidth, MinHeight: Integer); virtual; abstract;
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
        const MaxAtlasWidth, MaxAtlasHeight: Integer;
        out MinAtlasWidth, MinAtlasHeight: Integer): Boolean;
    procedure GenerateAtlas(const AtlasWidth, AtlasHeight: Integer);
  public
    function WillFramesFitInSize(
        AtlasWidth, AtlasHeight: Integer): Boolean; override;
    procedure GetMinAtlasSize(out MinWidth, MinHeight: Integer); override;
    procedure Generate; override;
  end;

  { TODO: Advanced sprite sheet generator with all features like trimming,
    padding and everything else we want }
  TCastleSpriteSheetAdvancedImageGen = class (TCastleSpriteSheetAbstractAtlasGen)
  public
    procedure Generate; override;
  end;

  { Castle Sprite Sheet XML file is not correct }
  EInvalidCastleSpriteSheetXml = class(Exception);
  ECastleSpriteSheetAtlasToSmall = class(Exception);
  ECantRegenerateAtlas = class(Exception);
  EReadOnlyMode = class(Exception);

function LoadCastleSpriteSheet(const Stream: TStream; const BaseUrl: String): TX3DRootNode;

implementation

uses StrUtils, DOM, Math, XMLRead,
  CastleDownload, CastleFilesUtils, CastleLog, CastleStringUtils,
  CastleTextureImages, CastleURIUtils, CastleUtils, CastleXMLUtils;

type
  { Frame names in starling file can be named freely, but in the case of our loader,
    we have to define what is the next frame of the animation and what should
    be recognized as a separate animation.
    See https://castle-engine.io/sprite_sheets }
  TAnimationNaming = (
    { Default behavior for Castle Sprite Sheet and Starling. It treats
      as animation frames only those subtextures whose names ends with
      an underscore followed by a number. }
    anStrictUnderscore,
    { In many cases, the consecutive frames of one animation are named
      without underscore. Only for Starling files. }
    anTralingNumber
  );

  TCastleSpriteSheetLoader = class
  strict private
    type
      { Class that represents SubTexture from Starling xml file }
      TSubTexture = class
      private
        FAnimationNaming: TAnimationNaming;
        procedure ParseAnimationName(const SubTextureName: String);
      public
        AnimationName: String;
        X: Integer;
        Y: Integer;
        Width: Integer;  // width in texture
        Height: Integer; // height in texture
        FrameX: Integer; // left offset (for trimming)
        FrameY: Integer; // top offset (for trimming)
        FrameWidth: Integer;
        FrameHeight: Integer;
        Trimmed: Boolean;

        procedure ReadFormXMLNode(const SubTextureNode: TDOMElement;
            const ImageWidth, ImageHeight: Integer);
        constructor Create;
      end;

    var
      FStream: TStream;
      FBaseUrl: String;
      FDisplayURL: String;

      { Load settings. }
      FFramesPerSecond: Single;

      FImageWidth, FImageHeight: Integer;
      { Image name with full path }
      FAbsoluteImagePath: String;
      { Image name from CastleSpriteSheet/Starling file }
      FRelativeImagePath: String;

      FSubTexture: TSubTexture;
      { True means that we will load extra edit fields and create
        images in frames }
      FLoadForEdit: Boolean;
      { True means that we attemp to load Starling file }
      FStarlingLoading : Boolean;

      FImage: TCastleImage;

    procedure ReadImportSettings;

    procedure ReadImageProperties(const URL: String; const AtlasNode: TDOMElement);

    procedure ReadFramesPerSecond(const SubTextureNode: TDOMElement);

    procedure PrepareSpriteSheet(const SpriteSheet: TCastleSpriteSheet);

    procedure AddFrame(const Animation: TCastleSpriteSheetAnimation);
  public
    constructor Create(const Stream: TStream; const BaseUrl: String;
        LoadForEdit: Boolean);
    destructor Destroy; override;

    procedure Load(const SpriteSheet: TCastleSpriteSheet); overload;
    function Load: TCastleSpriteSheet; overload;
  end;

  TCastleSpriteSheetXMLExporter = class
  strict private
    FSpriteSheet: TCastleSpriteSheet;
  public
    constructor Create(const SpriteSheet: TCastleSpriteSheet);

    function ExportToXML: TXMLDocument;
  end;

  TCastleSpriteSheetX3DExporter = class
  strict private
    FRoot: TX3DRootNode;
    FShapeCoord: TCoordinateNode;
    FShapeTexCoord: TTextureCoordinateNode;

    FCoordArray: array [0..3] of TVector3;
    FTexCoordArray: array [0..3] of TVector2;

    TimeSensor: TTimeSensorNode;
    CoordInterp: TCoordinateInterpolatorNode;
    TexCoordInterp: TCoordinateInterpolator2DNode;

    FSpriteSheet: TCastleSpriteSheet;

    procedure PrepareContainer;

    { In case of X3D here we calculate anchors and set frame cords }
    procedure CalculateFrameCoords(const Frame: TCastleSpriteSheetFrame);

    { In case of X3D here we create TimeSensor, CoordInterp,
      TexCoordInterp for animation }
    procedure PrepareAnimation(const Name: String);
    { In case of X3D here we add TimeSensor, CoordInterp, TexCoordInterp
      and routes to root }
    procedure AddAnimation(const Animation: TCastleSpriteSheetAnimation);
    { In case of X3D here we add frame coords to CoordInterp and TexCoordInterp }
    procedure AddFrame;
  public
    constructor Create(const SpriteSheet: TCastleSpriteSheet);

    function ExportToX3D: TX3DRootNode;
  end;

function LoadCastleSpriteSheet(const Stream: TStream; const BaseUrl: String): TX3DRootNode;
var
  SpriteSheet: TCastleSpriteSheet;
begin
  SpriteSheet := TCastleSpriteSheet.Create(false);
  try
    SpriteSheet.Load(Stream, BaseUrl);
    Result := SpriteSheet.ToX3D;
  finally
    FreeAndNil(SpriteSheet);
  end;
end;

{ TCastleSpriteSheetX3DExporter }

procedure TCastleSpriteSheetX3DExporter.PrepareContainer;
var
  Shape: TShapeNode;
  Material: TUnlitMaterialNode;
  Appearance: TAppearanceNode;
  Tri: TIndexedTriangleSetNode;
  Tex: TAbstractTextureNode;
  TexProperties: TTexturePropertiesNode;
  FdUrl: String;
begin
  FRoot.Meta['generator'] := 'Castle Game Engine, https://castle-engine.io';
  FRoot.Meta['source'] := ExtractURIName(FSpriteSheet.URL);

  Material := TUnlitMaterialNode.Create;

  Appearance := TAppearanceNode.Create;
  Appearance.Material := Material;

  Shape := TShapeNode.Create;
  Shape.Appearance := Appearance;

  TexProperties := TTexturePropertiesNode.Create;
  TexProperties.MagnificationFilter := magDefault;
  TexProperties.MinificationFilter := minDefault;
  TexProperties.BoundaryModeS := bmClampToEdge;
  TexProperties.BoundaryModeT := bmClampToEdge;
  { Do not force "power of 2" size, which may prevent mipmaps.
    This seems like a better default (otherwise the resizing underneath
    may cause longer loading time, and loss of quality, if not expected).
    See https://github.com/castle-engine/castle-engine/issues/249 }
  TexProperties.GuiTexture := true;

  if FSpriteSheet.EditMode and (FSpriteSheet.FGeneratedAtlas <> nil) then
  begin
    Tex := TPixelTextureNode.Create;
    TPixelTextureNode(Tex).FdImage.Value := FSpriteSheet.FGeneratedAtlas.MakeCopy;
    { No point in adjusting RepeatS/T: TextureProperties override it.
    TPixelTextureNode(Tex).RepeatS := false;
    TPixelTextureNode(Tex).RepeatT := false; }
    TPixelTextureNode(Tex).TextureProperties := TexProperties;
  end else
  begin
    Tex := TImageTextureNode.Create;
    { Check is this file loaded from Starling in that case use LoadedAtlasPath }
    if FSpriteSheet.URL <> '' then
      FdUrl := ExtractURIPath(FSpriteSheet.URL) + FSpriteSheet.RelativeAtlasPath
    else
      FdUrl := FSpriteSheet.LoadedAtlasPath;

    TImageTextureNode(Tex).FdUrl.Send(FdUrl);
    { No point in adjusting RepeatS/T: TextureProperties override it.
    TImageTextureNode(Tex).RepeatS := false;
    TImageTextureNode(Tex).RepeatT := false; }
    TImageTextureNode(Tex).TextureProperties := TexProperties;
  end;
  Appearance.Texture := Tex;

  //Tri := TTriangleSetNode.Create;
  Tri := TIndexedTriangleSetNode.Create;
  Tri.SetIndex([0, 1, 2, 0, 2, 3]);
  Tri.Solid := false;

  FShapeCoord := TCoordinateNode.Create;
  FShapeCoord.SetPoint([
    FCoordArray[0],
    FCoordArray[1],
    FCoordArray[2],
    FCoordArray[3]
  ]);

  FShapeTexCoord := TTextureCoordinateNode.Create;
  FShapeTexCoord.SetPoint([
    FTexCoordArray[0],
    FTexCoordArray[1],
    FTexCoordArray[2],
    FTexCoordArray[3]
  ]);

  Tri.Coord := FShapeCoord;
  Tri.TexCoord := FShapeTexCoord;
  Shape.Geometry := Tri;

  FRoot.AddChildren(Shape);
end;

procedure TCastleSpriteSheetX3DExporter.CalculateFrameCoords(
  const Frame: TCastleSpriteSheetFrame);

  procedure CalculateAnchors(const Frame: TCastleSpriteSheetFrame; out AnchorX, AnchorY: Single);
  var
    FrameAnchorX: Integer;
    FrameAnchorY: Integer;
  begin
    { I found some starling files which may have the last frame of the animation
      with the size set to 0 so we need check this here (division by zero error)
      example:
      https://github.com/pammimeow/fatty-starling-as3-game/blob/master/assets/sprite%20elements.xml }
    if Frame.Trimmed and (Frame.WidthInAtlas <> 0) and (Frame.HeightInAtlas <> 0) then
    begin
      { When frame is trimmed Width and Height does not mean the full size
        of the frame, so we have to calculate the appropriate
        anchor to get the correct position because it will not be (0.5, 0.5) }

      { Anchor in pixels (Without translation to correct texture point
        because we don't need that. Just add X1, Y1 to have correct position.) }
      FrameAnchorX := Frame.FrameWidth div 2 + Frame.LeftOffset;
      FrameAnchorY := Frame.FrameHeight div 2 + Frame.TopOffset;

      { Convert to 0.0..1.0 coordinate system }
      AnchorX := 1 / Frame.WidthInAtlas * FrameAnchorX;
      AnchorY := 1 / Frame.HeightInAtlas * FrameAnchorY;
    end else
    begin
      AnchorX := 0.5;
      AnchorY := 0.5;
    end;
  end;

  procedure AddCoords(const Frame: TCastleSpriteSheetFrame);
  var
    AnchorX, AnchorY: Single;
    X1: Single;
    Y1: Single;
    X2: Single;
    Y2: Single;
  begin
    CalculateAnchors(Frame, AnchorX, AnchorY);

    X1 := -Frame.WidthInAtlas * AnchorX;
    Y1 := -Frame.HeightInAtlas * (1 - AnchorY);
    X2 := Frame.WidthInAtlas * (1 - AnchorX);
    Y2 := Frame.HeightInAtlas * AnchorY;

    FCoordArray[0] := Vector3(X1, Y1, 0);
    FCoordArray[1] := Vector3(X2, Y1, 0);
    FCoordArray[2] := Vector3(X2, Y2, 0);
    FCoordArray[3] := Vector3(X1, Y2, 0);
  end;

  procedure AddTexCords(
    const Frame: TCastleSpriteSheetFrame; const ImageWidth, ImageHeight: Integer);
  var
    { Current frame tex cords }
    X1: Single;
    Y1: Single;
    X2: Single;
    Y2: Single;
  begin
    X1 := 1 / FSpriteSheet.AtlasWidth * Frame.XInAtlas;
    Y1 := 1 / FSpriteSheet.AtlasHeight * Frame.YInAtlas;
    X2 := 1 / FSpriteSheet.AtlasWidth * (Frame.XInAtlas + Frame.WidthInAtlas);
    Y2 := 1 / FSpriteSheet.AtlasHeight * (Frame.YInAtlas + Frame.HeightInAtlas);

    FTexCoordArray[0] := Vector2(X1, Y1);
    FTexCoordArray[1] := Vector2(X2, Y1);
    FTexCoordArray[2] := Vector2(X2, Y2);
    FTexCoordArray[3] := Vector2(X1, Y2);
  end;

begin
  AddCoords(Frame);
  AddTexCords(Frame, FSpriteSheet.FAtlasWidth, FSpriteSheet.FAtlasHeight);
end;

procedure TCastleSpriteSheetX3DExporter.PrepareAnimation(const Name: String);
begin
  TimeSensor := TTimeSensorNode.Create(Name);
  CoordInterp := TCoordinateInterpolatorNode.Create(Name + '_Coord');
  CoordInterp.Interpolation := inStep;
  TexCoordInterp := TCoordinateInterpolator2DNode.Create(Name + '_TexCoord');
  TexCoordInterp.Interpolation := inStep;
end;

procedure TCastleSpriteSheetX3DExporter.AddAnimation(
  const Animation: TCastleSpriteSheetAnimation);
var
  FrameCount: Integer;

  { CoordInterp often has a dummy animation, that actually wants the same coordinates
    every frame. In this case it can be simplified. }
  procedure OptimizeCoordInterp;
  const
    PerFrameValues = 4;
  var
    Values: TVector3List;
    I: Integer;
  begin
    Assert(CoordInterp.FdKeyValue.Count = PerFrameValues * FrameCount);
    Assert(FShapeCoord.FdPoint.Count = PerFrameValues);

    Values := CoordInterp.FdKeyValue.Items;
    for I := 1 to FrameCount - 1 do
    begin
      if not CompareMem(Values.L, Values.Ptr(I * PerFrameValues), SizeOf(TVector3) * PerFrameValues) then
        Exit; // optimization not possible
    end;

    { optimization possible: simplify CoordInterp to 1 frame, or even remove CoordInterp }
    if CompareMem(Values.L, FShapeCoord.FdPoint.Items.L, SizeOf(TVector3) * PerFrameValues) then
    begin
      FreeIfUnusedAndNil(CoordInterp);
    end else
    begin
      CoordInterp.FdKey.Count := 1;
      CoordInterp.FdKeyValue.Count := PerFrameValues;
    end;
  end;

var
  I: Integer;
  Key: Single;
begin
  FrameCount := Animation.FrameCount;
  { Set Cycle Interval becouse we know now frame count }
  TimeSensor.CycleInterval := FrameCount / Animation.FramesPerSecond;

  { Generate list of keys. }
  for I := 0 to FrameCount - 1 do
  begin
    Key := I / FrameCount;
    CoordInterp.FdKey.Items.Add(Key);
    TexCoordInterp.FdKey.Items.Add(Key);
  end;

  OptimizeCoordInterp;

  { Add TimeSensor to Root node }
  FRoot.AddChildren(TimeSensor);

  { Add TextureCoordinate animation }
  FRoot.AddChildren(TexCoordInterp);
  FRoot.AddRoute(TimeSensor.EventFraction_changed, TexCoordInterp.EventSet_fraction);
  FRoot.AddRoute(TexCoordInterp.EventValue_changed, FShapeTexCoord.FdPoint);

  { Add Coordinate animation }
  if CoordInterp <> nil then
  begin
    FRoot.AddChildren(CoordInterp);
    FRoot.AddRoute(TimeSensor.EventFraction_changed, CoordInterp.EventSet_fraction);
    FRoot.AddRoute(CoordInterp.EventValue_changed, FShapeCoord.FdPoint);
  end;
end;

procedure TCastleSpriteSheetX3DExporter.AddFrame;
begin
  CoordInterp.FdKeyValue.Items.AddRange(FCoordArray);
  TexCoordInterp.FdKeyValue.Items.AddRange(FTexCoordArray);
end;

constructor TCastleSpriteSheetX3DExporter.Create(
  const SpriteSheet: TCastleSpriteSheet);
begin
  inherited Create;
  FSpriteSheet := SpriteSheet;
end;

function TCastleSpriteSheetX3DExporter.ExportToX3D: TX3DRootNode;
var
  I, J: Integer;
  Animation: TCastleSpriteSheetAnimation;
  Frame: TCastleSpriteSheetFrame;
  FirstFrameInFirstAnimation: Boolean;
begin
  FirstFrameInFirstAnimation := true;

  FRoot := TX3DRootNode.Create;
  try
    for I := 0 to FSpriteSheet.AnimationCount - 1 do
    begin
      Animation := FSpriteSheet.AnimationByIndex(I);

      if Animation.FrameCount = 0 then
        continue;

      PrepareAnimation(Animation.Name);

      for J := 0  to Animation.FrameCount - 1 do
      begin
        Frame := Animation.Frame[J];
        CalculateFrameCoords(Frame);

        { We want the shape has by default size of first frame
          of first animation. }
        if FirstFrameInFirstAnimation then
        begin
          FirstFrameInFirstAnimation := false;
          PrepareContainer;
        end;

        AddFrame;
      end;

      AddAnimation(Animation);

    end;
    Result := FRoot;
  except
    FreeAndNil(FRoot);
    raise;
  end;
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
  const MaxAtlasWidth, MaxAtlasHeight: Integer;
  out MinAtlasWidth, MinAtlasHeight: Integer): Boolean;
var
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
        Frame := Animation.Frame[J];
        Frame.UpdateTrimming;

        BigestFrameWidth := Max(BigestFrameWidth, Frame.WidthInAtlas);
        BigestFrameHeight := Max(BigestFrameHeight, Frame.HeightInAtlas);
      end;
    end;
  end;

  procedure SetFrameCords(const AFrame: TCastleSpriteSheetFrame;
    const X, Y: Integer);
  begin
    if Operation = loArrange then
    begin
      AFrame.XInAtlas := X;
      AFrame.YInAtlas := Y;
    end;
  end;

begin
  X := 0;
  Y := 0;
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
        if Y + Frame.FrameHeight > MaxAtlasHeight then
        begin
          { return that we need more space }
          MinAtlasHeight := Max(Y + Frame.FrameHeight,
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
      Inc(Y, CurrentMaxLineHeight);
      X := 0;

      // check size
      if (Y + Frame.FrameHeight > MaxAtlasHeight) then
      begin
        MinAtlasHeight := Max(Y + Frame.FrameHeight,
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

      Frame.DrawToImage(FSpriteSheet.FGeneratedAtlas, Frame.XInAtlas, Frame.YInAtlas, 0, 0,
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
  LayoutFrames(loMeasure, FSpriteSheetMaxWidth, FSpriteSheetMaxHeight, MinWidth,
    MinHeight);
end;

procedure TCastleSpriteSheetBasicAtlasGen.Generate;
var
  MinWidth, MinHeight: Integer;
begin
  if not LayoutFrames(loArrange, FSpriteSheetMaxWidth, FSpriteSheetMaxHeight,
    MinWidth, MinHeight) then
    raise ECastleSpriteSheetAtlasToSmall.Create(Format(
    'Minimum sprite sheet atlas size too small (%d x %d), we need at least %d x %d ',
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
  RootNode: TDOMElement;
  SubTextureNode: TDOMElement;
begin
  Result := TXMLDocument.Create;

  RootNode := Result.CreateElement('TextureAtlas');
  Result.AppendChild(RootNode);

  RootNode.AttributeSet('imagePath', FSpriteSheet.RelativeAtlasPath);

  for I := 0 to FSpriteSheet.AnimationCount - 1 do
  begin
    Animation := FSpriteSheet.AnimationByIndex(I);

    if Animation.FrameCount = 0 then
      continue;

    for J := 0  to Animation.FrameCount - 1 do
    begin
      Frame := Animation.Frame[J];
      SubTextureNode := RootNode.CreateChild('SubTexture');

      SubTextureNode.AttributeSet('name', Animation.Name + '_' + IntToStr(J + 1));
      SubTextureNode.AttributeSet('x', Frame.XInAtlas);
      SubTextureNode.AttributeSet('y', FSpriteSheet.AtlasHeight - Frame.YInAtlas - Frame.HeightInAtlas);
      SubTextureNode.AttributeSet('width', Frame.WidthInAtlas);
      SubTextureNode.AttributeSet('height', Frame.HeightInAtlas);
      if Frame.Trimmed then
      begin
        SubTextureNode.AttributeSet('frameX', Frame.LeftOffset);
        SubTextureNode.AttributeSet('frameY', Frame.TopOffset);
        SubTextureNode.AttributeSet('frameWidth', Frame.FrameWidth);
        SubTextureNode.AttributeSet('frameHeight', Frame.FrameHeight);
      end;

      if J = 0 then
        SubTextureNode.AttributeSet('fps', Animation.FramesPerSecond);
    end;
  end;
end;

{ TCastleSpriteSheetFrame }

procedure TCastleSpriteSheetFrame.SetXInAtlas(const NewX: Integer);
begin
  if FXInAtlas = NewX then
    Exit;

  FXInAtlas := NewX;
  SetModifiedState;
end;

procedure TCastleSpriteSheetFrame.SetYInAtlas(const NewY: Integer);
begin
  if FYInAtlas = NewY then
    Exit;

  FYInAtlas := NewY;
  SetModifiedState;
end;

procedure TCastleSpriteSheetFrame.SetWidthInAtlas(const NewWidth: Integer);
begin
  if FWidthInAtlas = NewWidth then
    Exit;

  FWidthInAtlas := NewWidth;
  SetModifiedState;
end;

procedure TCastleSpriteSheetFrame.SetHeightInAtlas(const NewHeight: Integer);
begin
  if FHeightInAtlas = NewHeight then
    Exit;

  FHeightInAtlas := NewHeight;
  SetModifiedState;
end;

procedure TCastleSpriteSheetFrame.SetLeftOffset(const NewLeftOffset: Integer);
begin
  if FLeftOffset = NewLeftOffset then
    Exit;

  FLeftOffset := NewLeftOffset;
  SetModifiedState;
end;

procedure TCastleSpriteSheetFrame.SetTopOffset(const NewTopOffset: Integer);
begin
  if FTopOffset = NewTopOffset then
    Exit;

  FTopOffset := NewTopOffset;
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

procedure TCastleSpriteSheetFrame.CheckEditMode;
begin
  Animation.CheckEditMode;
end;

procedure TCastleSpriteSheetFrame.UpdateTrimming;
begin
  { TODO: add trimming, currently only reset to full frame size }
  LeftOffset := 0;
  TopOffset := 0;

  WidthInAtlas := FrameWidth;
  HeightInAtlas := FrameHeight;
end;

constructor TCastleSpriteSheetFrame.Create(
  const Animation: TCastleSpriteSheetAnimation);
begin
  Assert(Animation <> nil, 'Animation can''t be nil when creating frame!');

  FAnimation := Animation;
end;

constructor TCastleSpriteSheetFrame.Create(
  const Animation: TCastleSpriteSheetAnimation;
  const SrcFrame: TCastleSpriteSheetFrame);
begin
  Create(Animation);

  FXInAtlas := SrcFrame.FXInAtlas;
  FYInAtlas := SrcFrame.FXInAtlas;

  FWidthInAtlas := SrcFrame.FWidthInAtlas;
  FHeightInAtlas := SrcFrame.FHeightInAtlas;
  FLeftOffset := SrcFrame.FLeftOffset;
  FTopOffset := SrcFrame.FTopOffset;
  FFrameWidth := SrcFrame.FFrameWidth;
  FFrameHeight := SrcFrame.FFrameHeight;

  FTrimmed := SrcFrame.FTrimmed;
  if SrcFrame.FFrameImage <> nil then
    FFrameImage := SrcFrame.FFrameImage.MakeCopy
  else
    FFrameImage := nil;
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
  CheckEditMode;

  FreeAndNil(FFrameImage);
  WidthInAtlas := SourceImage.Width;
  HeightInAtlas := SourceImage.Height;

  LeftOffset := 0;
  TopOffset := 0;
  FrameWidth := SourceImage.Width;
  FrameHeight := SourceImage.Height;

  FFrameImage := SourceImage.MakeCopy;
  SetModifiedState;
end;

procedure TCastleSpriteSheetFrame.SetFrameImage(
  const SourceImage: TCastleImage; const DestX, DestY, AFrameWidth,
  AFrameHeight, SourceX, SourceY, SourceWidthToCopy, SourceHeightToCopy: Integer);
begin
  CheckEditMode;

  Trimmed := not ((DestX = 0) and (DestY = 0));

  { update frame settings }
  XInAtlas := SourceX;
  YInAtlas := SourceY;
  WidthInAtlas := SourceWidthToCopy;
  HeightInAtlas := SourceHeightToCopy;
  FrameWidth := AFrameWidth;
  FrameHeight := AFrameHeight;

  LeftOffset := -DestX;
  TopOffset := -AFrameHeight + DestY + HeightInAtlas;

  FreeAndNil(FFrameImage);

  if not Trimmed then
    FFrameImage := SourceImage.MakeExtracted(XInAtlas, YInAtlas, WidthInAtlas, HeightInAtlas)
  else
  begin
    { When Image is trimmed we can't simply extract image part }
    FFrameImage := TCastleImageClass(SourceImage.ClassType).Create(FrameWidth, FrameHeight, SourceImage.Depth);
    FFrameImage.Clear(Vector4Byte(0, 0, 0, 0));
    FFrameImage.DrawFrom(SourceImage,
      DestX, // destination XInAtlas
      DestY, // destination YInAtlas
      XInAtlas, // source XInAtlas
      YInAtlas, // source YInAtlas
      WidthInAtlas,
      HeightInAtlas,
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
  CheckEditMode;

  DestImage.DrawFrom(FFrameImage, DestX, DestY, SourceX, SourceY, SourceWidthToDraw,
    SourceHeightToDraw, dmOverwrite);
end;

function TCastleSpriteSheetFrame.MakeResized(const Width, Height: Integer
  ): TCastleImage;
begin
  Assert(FFrameImage <> nil, 'No frame image to resize.');
  CheckEditMode;

  Result := FFrameImage.MakeResized(Width, Height);
end;

function TCastleSpriteSheetFrame.MakeResizedWithBg(const Width,
  Height: Integer; const BgColor: TVector4): TCastleImage;
var
  ResizedImage: TCastleImage;
  DrawMode: TDrawMode;
begin
  Assert(FFrameImage <> nil, 'No frame image to resize.');
  CheckEditMode;

  Result := TCastleImageClass(FFrameImage.ClassType).Create(Width, Height, FFrameImage.Depth);
  Result.Clear(BgColor);

  if BgColor.IsPerfectlyZero then
    DrawMode := dmOverwrite
  else
    DrawMode := dmBlend;

  ResizedImage := FFrameImage.MakeResized(Width, Height);
  try
    Result.DrawFrom(FFrameImage, 0, 0, 0, 0, ResizedImage.Width,
      ResizedImage.Height, DrawMode);
  finally
    FreeAndNil(ResizedImage);
  end;
end;

function TCastleSpriteSheetFrame.MakeImageCopy: TCastleImage;
begin
  Assert(FFrameImage <> nil, 'No frame image to make copy.');
  CheckEditMode;

  Result := FFrameImage.MakeCopy;
end;

function TCastleSpriteSheetFrame.MakeImageCopyWithBg(const BgColor: TVector4
  ): TCastleImage;
var
  DrawMode: TDrawMode;
begin
  Assert(FFrameImage <> nil, 'No frame image to make copy.');
  CheckEditMode;

  Result := TCastleImageClass(FFrameImage.ClassType).Create(FFrameImage.Width,
    FFrameImage.Height, FFrameImage.Depth);
  Result.Clear(BgColor);

  if BgColor.IsPerfectlyZero then
    DrawMode := dmOverwrite
  else
    DrawMode := dmBlend;

  Result.DrawFrom(FFrameImage, 0, 0, 0, 0, FFrameImage.Width,
    FFrameImage.Height, DrawMode);
end;

function TCastleSpriteSheetFrame.CenterOnBiggerImage(const Width,
  Height: Integer; const BgColor: TVector4): TCastleImage;
var
  DrawMode: TDrawMode;
begin
  if (FrameWidth > Width) or (FrameHeight > Height) then
    raise Exception.Create('Frame image bigger than gived size');

  CheckEditMode;

  Result := TCastleImageClass(FFrameImage.ClassType).Create(Width, Height, FFrameImage.Depth);
  Result.Clear(BgColor);

  if BgColor.IsPerfectlyZero then
    DrawMode := dmOverwrite
  else
    DrawMode := dmBlend;

  Result.DrawFrom(FFrameImage, (Width - FFrameImage.Width) div 2,
    (Height - FFrameImage.Height) div 2, 0, 0, FFrameImage.Width,
    FFrameImage.Height, DrawMode);
end;

procedure TCastleSpriteSheetFrame.SaveFrameImage(const URL: String);
begin
  CheckEditMode;

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

procedure TCastleSpriteSheetAnimation.SetFramesPerSecond(const NewFPS: Single);
begin
  FFramesPerSecond := NewFPS;
  SetModifiedState;
end;

function TCastleSpriteSheetAnimation.GetFrame(const Index: Integer
  ): TCastleSpriteSheetFrame;
begin
  Result := FFrameList[Index];
end;

procedure TCastleSpriteSheetAnimation.CheckEditMode;
begin
  FSpriteSheet.CheckEditMode;
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
  CheckEditMode;

  AFrame := TCastleSpriteSheetFrame.Create(Self);
  FFrameList.Add(AFrame);
  Result := AFrame;
  if Assigned(FSpriteSheet.OnFrameAdded) then
    FSpriteSheet.OnFrameAdded(AFrame);
  SetModifiedState;
end;

function TCastleSpriteSheetAnimation.AddFrameCopy(
  const SrcFrame: TCastleSpriteSheetFrame): TCastleSpriteSheetFrame;
var
  AFrame: TCastleSpriteSheetFrame;
begin
  CheckEditMode;

  AFrame := TCastleSpriteSheetFrame.Create(Self, SrcFrame);
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
  CheckEditMode;

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

function TCastleSpriteSheetAnimation.AddFrame(const SourceImage: TCastleImage;
  const DestX, DestY, AFrameWidth, AFrameHeight, SourceX, SourceY,
  SourceWidthToCopy, SourceHeightToCopy: Integer): TCastleSpriteSheetFrame;
var
  AFrame: TCastleSpriteSheetFrame;
begin
  CheckEditMode;

  AFrame := TCastleSpriteSheetFrame.Create(Self);
  FFrameList.Add(AFrame);
  Result := AFrame;
  AFrame.SetFrameImage(SourceImage, DestX, DestY, AFrameWidth, AFrameHeight,
  SourceX, SourceY, SourceWidthToCopy, SourceHeightToCopy);
  if Assigned(FSpriteSheet.OnFrameAdded) then
    FSpriteSheet.OnFrameAdded(AFrame);
  SetModifiedState;
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
  CheckEditMode;

  if (Assigned(FSpriteSheet.BeforeFrameRemoved)) and
    (FFrameList.Contains(Frame)) then
    FSpriteSheet.BeforeFrameRemoved(Frame);
  FFrameList.Remove(Frame);
  SetModifiedState;
end;

procedure TCastleSpriteSheetAnimation.MoveFrameLeft(
  const Frame: TCastleSpriteSheetFrame);
var
  Index: Integer;
begin
  CheckEditMode;

  Index := FFrameList.IndexOf(Frame);
  if Index < 1 then
    Exit;

  MoveFrame(Index, Index - 1);
end;

procedure TCastleSpriteSheetAnimation.MoveFrameRight(
  const Frame: TCastleSpriteSheetFrame);
var
  Index: Integer;
begin
  CheckEditMode;

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
  CheckEditMode;

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
  CheckEditMode;

  Index := FFrameList.IndexOf(Frame);
  if (Index = FFrameList.Count - 1) or (Index = -1) then
    Exit;

  MoveFrame(Index, FFrameList.Count - 1);
end;

procedure TCastleSpriteSheetAnimation.MoveFrame(const OldIndex,
  NewIndex: Integer);
begin
  CheckEditMode;

  FFrameList.Move(OldIndex, NewIndex);

  if Assigned(FSpriteSheet.OnFrameMoved) then
    FSpriteSheet.OnFrameMoved(FFrameList[NewIndex], OldIndex, NewIndex);

  SetModifiedState;
end;

procedure TCastleSpriteSheetAnimation.ImportAtlas(AtlasImageURL: String; Cols,
  Rows: Integer; ImportByColumns: Boolean);
var
  AtlasToImport: TCastleImage;
  FrameWidth: Integer;
  FrameHeight: Integer;
  I, J: Integer;
begin
  CheckEditMode;

  AtlasToImport := LoadImage(AtlasImageURL);
  try
    FrameWidth := AtlasToImport.Width div Cols;
    FrameHeight := AtlasToImport.Height div Rows;

    if ImportByColumns then
    begin
      for J := Rows - 1 downto 0 do
        for I := 0  to Cols - 1 do
        begin
          AddFrame(AtlasToImport, 0, 0, FrameWidth, FrameHeight,
          I * FrameWidth, J * FrameHeight, FrameWidth, FrameHeight);
        end;
    end else
    for I := 0  to Cols - 1 do
      for J := Rows - 1 downto 0 do
      begin
        AddFrame(AtlasToImport, 0, 0, FrameWidth, FrameHeight,
        I * FrameWidth, J * FrameHeight, FrameWidth, FrameHeight);
      end;

  finally
    FreeAndNil(AtlasToImport);
  end;
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

{ TCastleSpriteSheet }

procedure TCastleSpriteSheet.SetURL(const NewURL: String);
begin
  if FURL = NewURL then
    Exit;

  FURL := NewURL;

  if Assigned(FOnURLChanged) then
    FOnURLChanged(Self);

  SetModifiedState;
end;

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

procedure TCastleSpriteSheet.CheckEditMode;
begin
  if FLoadingPending then
    Exit;

  if not EditMode then
    raise EReadOnlyMode.Create(
      'Can''t edit spritesheet in read only state (reload in edit mode).');
end;

constructor TCastleSpriteSheet.Create(AEditMode: Boolean);
begin
  FEditMode := AEditMode;
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
  URLWithoutAnchor: String;
  Stream: TStream;
begin
  URLWithoutAnchor := URIDeleteAnchor(URL, true);

  Stream := Download(URLWithoutAnchor);
  try
    Load(Stream, URL);
  finally FreeAndNil(Stream) end;
end;

procedure TCastleSpriteSheet.Load(const Stream: TStream; const BaseUrl: String);
var
  SpriteSheetLoader: TCastleSpriteSheetLoader;
begin
  SpriteSheetLoader := nil;
  BeginLoad;
  try
    SpriteSheetLoader := TCastleSpriteSheetLoader.Create(Stream, BaseUrl, EditMode);
    SpriteSheetLoader.Load(Self);
  finally
    FreeAndNil(SpriteSheetLoader);
    EndLoad;
  end;
end;

procedure TCastleSpriteSheet.Save(const AURL: String;
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
      Maybe we should add option to set file name by user.

      When FRelativeAtlasPath is empty or URL changes (so we have save as,
      we need generate new name. }
    if (FRelativeAtlasPath = '') or (AURL <> URL) then
    begin
      FRelativeAtlasPath := DeleteURIExt(ExtractURIName(AURL)) + '.png';
    end;
    AtlasURL := URIIncludeSlash(ExtractURIPath(AURL)) + FRelativeAtlasPath;

    { Save image file }
    if FGeneratedAtlas = nil then
      CheckCopyFile(URIToFilenameSafe(LoadedAtlasPath), URIToFilenameSafe(AtlasURL))
    else
      SaveImage(FGeneratedAtlas, AtlasURL);

    { Save xml (CastleSpriteSheet) file }
    ExporterXML := nil;
    XMLDoc := nil;
    try
      ExporterXML := TCastleSpriteSheetXMLExporter.Create(Self);
      XMLDoc := ExporterXML.ExportToXML;
      URLWriteXML(XMLDoc, AURL);
    finally
      FreeAndNil(ExporterXML);
      FreeAndNil(XMLDoc);
    end;

    if not SaveCopy then
    begin
      URL := AURL;
      ClearModifiedState;
      if Assigned(FOnFileSaved) then
        FOnFileSaved(URL);
    end;
  finally
    { In case of copy go back to old relative image path when everything
      is exported }
    if SaveCopy then
      FRelativeAtlasPath := FOldRelativeImagePath;
  end;
end;

function TCastleSpriteSheet.ToX3D: TX3DRootNode;
var
  Exporter: TCastleSpriteSheetX3DExporter;
begin
  if IsModified then
    RegenerateAtlas;

  Exporter := TCastleSpriteSheetX3DExporter.Create(Self);
  try
    Result := Exporter.ExportToX3D;
  finally
    FreeAndNil(Exporter);
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

  CheckEditMode;

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
  if HasAnimation(Name) then
    Exit(AnimationByName(Name));

  CheckEditMode;

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
  CheckEditMode;

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
  CheckEditMode;

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
  CheckEditMode;

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
  CheckEditMode;

  Index := FAnimationList.IndexOf(Animation);
  if (Index = FAnimationList.Count - 1) or (Index = -1) then
    Exit;

  MoveAnimation(Index, FAnimationList.Count - 1);
end;

procedure TCastleSpriteSheet.MoveAnimation(const OldIndex, NewIndex: Integer);
begin
  CheckEditMode;

  FAnimationList.Move(OldIndex, NewIndex);

  if Assigned(FOnAnimationMoved) then
    FOnAnimationMoved(FAnimationList[NewIndex], OldIndex, NewIndex);

  SetModifiedState;
end;

procedure TCastleSpriteSheet.RemoveAnimation(
  const Animation: TCastleSpriteSheetAnimation);
begin
  CheckEditMode;

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
  CheckEditMode;

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
  CheckEditMode;

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
  Setting: {$ifdef FPC}TStringStringMap.TDictionaryPair{$else}TPair<String, String>{$endif};
begin
  // default values
  FFramesPerSecond := DefaultSpriteSheetFramesPerSecond;

  if FStarlingLoading then
  begin
    SettingsMap := TStringStringMap.Create;
    try
      URIGetSettingsFromAnchor(FBaseUrl, SettingsMap);
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
  end else
    FSubTexture.FAnimationNaming := anStrictUnderscore;
end;

procedure TCastleSpriteSheetLoader.ReadImageProperties(const URL: String;
  const AtlasNode: TDOMElement);
var
  Image: TCastleImage;
begin
  FRelativeImagePath := AtlasNode.AttributeString('imagePath');
  FAbsoluteImagePath := ExtractURIPath(URIDeleteAnchor(URL, true)) + FRelativeImagePath;
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

procedure TCastleSpriteSheetLoader.PrepareSpriteSheet(
    const SpriteSheet: TCastleSpriteSheet);
begin
  if not FStarlingLoading then
    SpriteSheet.URL := FBaseUrl;
  SpriteSheet.LoadedAtlasPath := FAbsoluteImagePath;
  SpriteSheet.RelativeAtlasPath := FRelativeImagePath;
  FreeAndNil(FImage);
  if FLoadForEdit then
    FImage := LoadImage(FAbsoluteImagePath);
  SpriteSheet.FAtlasWidth := FImageWidth;
  SpriteSheet.FAtlasHeight := FImageHeight;
  SpriteSheet.FMaxAtlasWidth := FImageWidth;
  SpriteSheet.FMaxAtlasHeight := FImageHeight;
end;

procedure TCastleSpriteSheetLoader.AddFrame(
    const Animation: TCastleSpriteSheetAnimation);
var
  Frame: TCastleSpriteSheetFrame;
  FrameYInCGECoords: Integer;
  YInCGECoords: Integer;
begin
  Frame := Animation.AddFrame;

  // we need to go to CGE coords here
  YInCGECoords := FImageHeight - FSubTexture.Y - FSubTexture.Height;

  { When we load not for edit }
  if not FLoadForEdit then
  begin
    Frame.WidthInAtlas := FSubTexture.Width;
    Frame.HeightInAtlas := FSubTexture.Height;
    Frame.Trimmed := FSubTexture.Trimmed;
    Frame.XInAtlas := FSubTexture.X;
    Frame.YInAtlas := YInCGECoords;
    if Frame.Trimmed then
    begin
      Frame.FrameWidth := FSubTexture.FrameWidth;
      Frame.FrameHeight := FSubTexture.FrameHeight;
      Frame.LeftOffset := FSubTexture.FrameX;
      Frame.TopOffset := FSubTexture.FrameY;
    end else
    begin
      // make data always OK
      Frame.LeftOffset := 0;
      Frame.TopOffset := 0;
      Frame.FrameWidth := Frame.WidthInAtlas;
      Frame.FrameHeight := Frame.HeightInAtlas;
    end;
    Exit;
  end;

  { If we want load sprite sheet for edit }
  FrameYInCGECoords := FSubTexture.FrameHeight - (-FSubTexture.FrameY +
    FSubTexture.Height);

  Frame.SetFrameImage(FImage,
    -FSubTexture.FrameX,
    FrameYInCGECoords,
    FSubTexture.FrameWidth,
    FSubTexture.FrameHeight,
    FSubTexture.X,
    YInCGECoords,
    FSubTexture.Width,
    FSubTexture.Height
  );
end;

constructor TCastleSpriteSheetLoader.Create(const Stream: TStream; const BaseUrl: String; LoadForEdit: Boolean);
var
  MimeType: String;
begin
  inherited Create;
  FStream := Stream;
  FBaseUrl := BaseUrl;
  FDisplayURL := URIDisplay(FBaseUrl);

  MimeType := URIMimeType(FBaseUrl);
  FStarlingLoading :=
    (MimeType = 'application/x-starling-sprite-sheet') or
    (MimeType = 'application/xml');

  FSubTexture := TSubTexture.Create;
  FLoadForEdit := LoadForEdit;
end;

destructor TCastleSpriteSheetLoader.Destroy;
begin
  FreeAndNil(FSubTexture);
  FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TCastleSpriteSheetLoader.Load(
  const SpriteSheet: TCastleSpriteSheet);
var
  Doc: TXMLDocument;
  AtlasNode: TDOMElement;
  I: TXMLElementIterator;
  LastAnimationName: String;
  CurrentAnimation: TCastleSpriteSheetAnimation;
begin
  ReadImportSettings;

  LastAnimationName := '';
  CurrentAnimation := nil;

  Doc := nil;
  try
    ReadXMLFile(Doc, FStream);

    Check(Doc.DocumentElement.TagName8 = 'TextureAtlas',
      'Root of CastleSpriteSheet file must be <TextureAtlas>');

    AtlasNode := Doc.DocumentElement;
    ReadImageProperties(FBaseUrl, AtlasNode);

    PrepareSpriteSheet(SpriteSheet);

    I := AtlasNode.ChildrenIterator('SubTexture');
    try
      while I.GetNext do
      begin
        { Read frame from XML }
        FSubTexture.ReadFormXMLNode(I.Current, FImageWidth, FImageHeight);

        if LastAnimationName <> FSubTexture.AnimationName then
        begin
          { First frame of animation loaded. }
          LastAnimationName := FSubTexture.AnimationName;
          CurrentAnimation := SpriteSheet.AddAnimation(LastAnimationName);

          { Check fps (CastleSpriteSheet extension) }
          ReadFramesPerSecond(I.Current);
          CurrentAnimation.FramesPerSecond := FFramesPerSecond;

          AddFrame(CurrentAnimation);
        end else
        begin
          { Next frame of animation }
          AddFrame(CurrentAnimation);
        end;
      end;
    finally
      FreeAndNil(I);
    end;
  finally
    FreeAndNil(Doc);
  end;
end;

function TCastleSpriteSheetLoader.Load: TCastleSpriteSheet;
var
  SpriteSheet: TCastleSpriteSheet;
begin
  SpriteSheet := TCastleSpriteSheet.Create(FLoadForEdit);
  SpriteSheet.BeginLoad;
  try
    try
      Load(SpriteSheet);
      Result := SpriteSheet;
    except
      FreeAndNil(SpriteSheet);
      raise;
    end;
  finally
    SpriteSheet.EndLoad;
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
        UnderscorePos := BackPos('_', AnimationName);

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
        {$ifdef FPC}
        RemoveTrailingChars(AnimationName, ['0'..'9']);
        {$else}
        AnimationName := AnimationName.TrimRight(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']);
        {$endif}

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
