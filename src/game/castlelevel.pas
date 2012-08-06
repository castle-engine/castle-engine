{
  Copyright 2006-2012 Michalis Kamburelis.

  This file is part of "castle".

  "castle" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "castle" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "castle"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ Scene manager that can easily load game levels (TGameSceneManager),
  management of available game levels (LevelsAvailable). }
unit CastleLevel;

interface

uses VectorMath, CastleSceneCore, CastleScene, Boxes3D,
  X3DNodes, X3DFields, CastleItems, Cameras, CastleCreatures, Background,
  CastleUtils, CastleClassUtils, CastlePlayer, CastleResources,
  ProgressUnit, PrecalculatedAnimation,
  DOM, ALSoundEngine, Base3D, Shape, GL, CastleConfig, Images,
  Classes, CastleTimeUtils, CastleSceneManager, GLRendererShader, FGL;

const
  DefaultThunderAmbientIntensity = 1.0;
  DefaultThunderColor: TVector3Single = (1, 1, 1);
  DefaultThunderDirection: TVector3Single = (0, 0, -1);

type
  TLevel = class;
  TLevelClass = class of TLevel;
  TCastleSceneClass = class of TCastleScene;
  TCastlePrecalculatedAnimationClass = class of TCastlePrecalculatedAnimation;
  TGameSceneManager = class;

  TLevelAvailable = class
  private
    { We keep Document reference through lifetime of this object,
      because actual TLevel instance also reads some stuff from it. }
    Document: TXMLDocument;
    DocumentBasePath: string;
    FMusicSound: TSoundType;
    procedure LoadFromDocument;
  public
    constructor Create;
    destructor Destroy; override;
  public
    AvailableForNewGame: boolean;
    DefaultAvailableForNewGame: boolean;

    { Level logic class. }
    LevelClass: TLevelClass;

    { Unique identifier for this level.
      Should be a suitable identifier in Pascal.
      @noAutoLinkHere }
    Id: string;

    { 3D file to load level. }
    SceneFileName: string;

    { Nice name of the level. }
    Title: string;
    TitleHint: string;

    { Level number, shown for the player in the menu.
      This *does not* determine the order in which levels are played,
      as levels do not have to be played in linear order.
      However, they are displayed in menu in linear order, and that's
      why this is needed. }
    Number: Integer;

    { Background image when loading, @nil if none. }
    LoadingImage: TRGBImage;

    { Position of the progress bar when loading, suitable for
      TProgressUserInterface.BarYPosition.
      Used only if LoadingImage <> @nil (as the only purpose of this property
      is to match LoadingImage look). }
    LoadingImageBarYPosition: Single;

    Element: TDOMElement;

    Resources: T3DResourceList;

    Demo: boolean;

    property MusicSound: TSoundType read FMusicSound write FMusicSound
      default stNone;

    { Load level from file, create camera, octrees, prepare for OpenGL and such. }
    procedure LoadLevel(const SceneManager: TGameSceneManager;
      const MenuBackground: boolean = false);
  end;

  TLevelAvailableList = class(specialize TFPGObjectList<TLevelAvailable>)
  private
    { How many TGameSceneManager have references to our children by
      TGameSceneManager.Info? }
    References: Cardinal;
    procedure LoadIndexXml(const FileName: string);
    { Save AvailableForNewGame properties of every item. }
    procedure SaveToConfig(const Config: TCastleConfig);
  public
    { raises Exception if such Id is not on the list. }
    function FindId(const AId: string): TLevelAvailable;

    procedure SortByNumber;

    { Add all available levels found by scanning for index.xml inside directory
      LevelsPath. For the specification of index.xml format see
      castle/data/README_about_index_xml_files.txt.
      For example, you can use @code(ProgramDataPath + 'data' +  PathDelim + 'levels')
      as LevelsPath.

      All AvailableForNewGame are initially set to @false.
      You must later call LoadFromConfig to read user preferences
      and set AvailableForNewGame correctly (depending on levels that user
      already finished, looking at DefaultAvailableForNewGame).
      That's why LoadFromConfig has to be called explicitly,
      it isn't added to Config.OnLoad list.

      Also, this can be done only once creatures and items resources are known,
      as they may be referenced in levels XML files. }
    procedure LoadFromFiles(const LevelsPath: string);

    { For all available levels, read their TLevelAvailable.AvailableForNewGame
      from user preferences.

      This is useful only if you actually look at
      TLevelAvailable.AvailableForNewGame for any purpose (for example,
      to decide which levels are displayed in the menu). By default,
      our engine doesn't look at AvailableForNewGame for anything. }
    procedure LoadFromConfig;
  end;

  { Invisible and non-colliding 3D area that has some special purpose.
    What exactly this "purpose" is, is defined in each TLevelArea descendant.

    This class defines only a properties to define the area.
    For now, each area is just one TBox3D. }
  TLevelArea = class(T3D)
  private
    FId: string;
    FBox: TBox3D;

    { Area. Default value is EmptyBox3D. }
    property Box: TBox3D read FBox write FBox;
  public
    constructor Create(AOwner: TComponent); override;

    { Name used to recognize this object's area in level VRML/X3D file.

      If this object is present during ChangeLevelScene call
      then the shape with a parent named like @link(Id)
      will be removed from VRML/X3D file, and it's BoundingBox will be used
      as Box3D of this object.

      This way you can easily configure area of this object in Blender:
      just add a cube, set it's mesh name to match with this @link(Id),
      and then this cube defines Box3D of this object. }
    property Id: string read FId write FId;

    function PointInside(const Point: TVector3Single): boolean;

    function BoundingBox: TBox3D; override;

    { Called when loading level. This is the place when you
      can modify MainScene, e.g. by calling MainScene.RemoveBlenderBox. }
    procedure ChangeLevelScene(MainScene: TCastleScene);
  end;

  { Area on the level that causes
    a Notification to be displayed when player enters inside.
    The natural use for it is to display various hint messages when player
    is close to something. }
  TLevelHintArea = class(TLevelArea)
  private
    FMessage: string;
    FMessageDone: boolean;
  public
    { Message to display when player enters our volume.
      Some formatting strings are allowed inside:
      @unorderedList(
        @item @code(%i) produces InteractInputDescription in the message.
        @item @code(%%) produces one @code(%) in the message.
      ) }
    property Message: string read FMessage write FMessage;

    { Was the @link(Message) already displayed ? If @true,
      then it will not be displayed again (unless you will
      reset MessageDone to @false from your TLevel descendant code). }
    property MessageDone: boolean read FMessageDone write FMessageDone
      default false;

    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;
  end;

  TGameSceneManager = class(TCastleSceneManager)
  private
    FSickProjection: boolean;
    FSickProjectionSpeed: TFloatTime;

    FLevel: TLevel;
    FInfo: TLevelAvailable;
    MenuBackground: boolean;

    procedure SetSickProjection(const Value: boolean);
    procedure SetSickProjectionSpeed(const Value: TFloatTime);

    procedure LoadLevel(const AInfo: TLevelAvailable;
      const AMenuBackground: boolean);
  protected
    procedure InitializeLights(const Lights: TLightInstancesList); override;
    procedure ApplyProjection; override;
    procedure PointingDeviceActivateFailed(const Active: boolean); override;
  public
    destructor Destroy; override;

    { Level logic and state. }
    property Level: TLevel read FLevel;

    { Level information, independent from current level state. }
    property Info: TLevelAvailable read FInfo;

    property SickProjection: boolean
      read FSickProjection write SetSickProjection;
    property SickProjectionSpeed: TFloatTime
      read FSickProjectionSpeed write SetSickProjectionSpeed;

    function CollisionIgnoreItem(
      const Sender: TObject;
      const Triangle: P3DTriangle): boolean; override;
    function Background: TBackground; override;
    procedure Idle(const CompSpeed: Single;
      const HandleMouseAndKeys: boolean;
      var LetOthersHandleMouseAndKeys: boolean); override;
  end;

  { Rendering and making sound of a thunder (like in a storm) effect. }
  TThunder = class
  private
    Time, LastBeginTime, NextBeginTime: Single;
    LightNode: TDirectionalLightNode;
    Light: TLightInstance;
    { Add thunder light, if visible. }
    procedure AddLight(const BaseLights: TLightInstancesList);
    procedure Idle(const CompSpeed: Single);
    function GetAmbientIntensity: Single;
    procedure SetAmbientIntensity(const Value: Single);
    function GetColor: TVector3Single;
    procedure SetColor(const Value: TVector3Single);
    function GetDirection: TVector3Single;
    procedure SetDirection(const Value: TVector3Single);
  public
    constructor Create;
    destructor Destroy; override;

    { Force thunder to happen @italic(now). }
    procedure ForceNow;

    property AmbientIntensity: Single read GetAmbientIntensity write SetAmbientIntensity default DefaultThunderAmbientIntensity;
    { Color. Default value is DefaultThunderColor. }
    property Color: TVector3Single read GetColor write SetColor;
    { Direction. Default value is DefaultThunderDirection. }
    property Direction: TVector3Single read GetDirection write SetDirection;
  end;

  { Level logic. We use T3D descendant, since this is the comfortable
    way to add any behavior to the 3D world (it doesn't matter that
    "level logic" is not a usual 3D object --- it doesn't have to collide
    or be visible). And we add some game-specific stuff,
    like BossCreatureIndicator. }
  TLevel = class(T3D)
  private
    FAnimationTime: TFloatTime;
    FThunder: TThunder;
  protected
    { Scene manager containing this level. }
    SceneManager: TGameSceneManager;

    FBossCreature: TCreature;

    { Load TCastlePrecalculatedAnimation from *.kanim file, doing common tasks.
      @unorderedList(
        @item sets Attributes according to AnimationAttributesSet
        @item optionally creates triangle octree for the FirstScene and/or LastScene
        @item(call PrepareResources, with prRender, prBoundingBox, prShadowVolume
          (if shadow volumes possible at all in this OpenGL context))
        @item FreeExternalResources, since they will not be needed anymore
        @item TimePlaying is by default @false, so the animation is not playing.
      ) }
    function LoadLevelAnimation(
      const FileName: string;
      CreateFirstOctreeCollisions,
      CreateLastOctreeCollisions: boolean;
      const AnimationClass: TCastlePrecalculatedAnimationClass): TCastlePrecalculatedAnimation;
    function LoadLevelAnimation(
      const FileName: string;
      CreateFirstOctreeCollisions,
      CreateLastOctreeCollisions: boolean): TCastlePrecalculatedAnimation;

    { Just load TCastleScene from file, doing some common tasks:
      @unorderedList(
        @item sets Attributes according to AttributesSet
        @item optionally create triangle octree
        @item(call PrepareResources, with prRender, prBoundingBox, prShadowVolume
          (if shadow volumes possible at all in this OpenGL context), optionally
          with prBackground)
        @item FreeExternalResources, since they will not be needed anymore
      ) }
    function LoadLevelScene(const FileName: string;
      CreateOctreeCollisions, PrepareBackground: boolean;
      const SceneClass: TCastleSceneClass): TCastleScene;
    function LoadLevelScene(const FileName: string;
      CreateOctreeCollisions, PrepareBackground: boolean): TCastleScene;
  public
    { Create new level instance. Called when creatures and hints are already
      initialized. But before creating resources like octrees,
      so you can modify MainScene contents. }
    constructor Create(AOwner: TComponent; AWorld: T3DWorld;
      MainScene: TCastleScene; DOMElement: TDOMElement); reintroduce; virtual;
    destructor Destroy; override;
    function BoundingBox: TBox3D; override;

    { Called when new player starts game on this level.
      This is supposed to equip the player with some basic weapon/items.

      Usually level design assumes that player came to level from some
      other level in the game, so he already owns some weapon / items etc.
      But when player uses "New Game" command to get to some already
      AvailableForNewGame non-first level, this method will be called and it should
      give player some basic weapon / items suitable for starting this level.

      In TLevel class implementation of this does nothing.  }
    procedure PrepareNewPlayer(NewPlayer: TPlayer); virtual;

    { What to show on boss creature indicator.
      Default implementation in this class uses BossCreature property:
      if it's non-nil and BossCreature is alive, then indicator shows
      BossCreature life. }
    function BossCreatureIndicator(out Life, MaxLife: Single): boolean; virtual;

    { Instance of boss creature, if any, on the level. @nil if no boss creature
      exists on this level. }
    property BossCreature: TCreature read FBossCreature;

    { Time of the level, in seconds. Time 0 when level is created.
      This is updated in our Idle. }
    property AnimationTime: TFloatTime read FAnimationTime;

    { Thunder effect, making sound and blinking light like a thunder in a storm.
      @nil if no thunder effect should be done for this level.
      Descendants can configure and assign this, we will own it (free). }
    property Thunder: TThunder read FThunder write FThunder;

    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;

    { Override background of the world. Leave @nil to let scene manager
      use default (from MainScene.Background). }
    function Background: TBackground; virtual;
  end;

  TLevelClasses = specialize TFPGMap<string, TLevelClass>;

function LevelClasses: TLevelClasses;

var
  { List of all available levels.
    This has all the information needed to present user a list of levels,
    and to actually load a given level (create suitable TLevel instance).
    Created in initialization of this unit, destroyed in finalization.
    Owns it's Items. }
  LevelsAvailable: TLevelAvailableList;

implementation

uses SysUtils, Triangle, CastleLog, CastleGLUtils,
  CastleFilesUtils, CastleStringUtils, GLImages, UIControls, XMLRead,
  CastleGameNotifications, CastleInputs, CastleGameCache, CastleXMLUtils,
  GLRenderer, RenderingCameraUnit, Math, CastleWarnings;

{ TLevelArea ----------------------------------------------------------------- }

constructor TLevelArea.Create(AOwner: TComponent);
begin
  inherited;
  FBox := EmptyBox3D;
  { Actually, the fact that our BoundingBox is empty also prevents collisions.
    But for some methods, knowing that Collides = false allows them to exit
    faster. }
  Collides := false;
end;

function TLevelArea.BoundingBox: TBox3D;
begin
  { This object is invisible and non-colliding. }
  Result := EmptyBox3D;
end;

procedure TLevelArea.ChangeLevelScene(MainScene: TCastleScene);
begin
  inherited;
  MainScene.RemoveBlenderBoxCheck(FBox, Id);
end;

function TLevelArea.PointInside(const Point: TVector3Single): boolean;
begin
  Result := Box.PointInside(Point);
end;

{ TLevelHintArea ----------------------------------------------------------- }

procedure TLevelHintArea.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);
var
  ReplaceInteractInput: TPercentReplace;
begin
  inherited;
  if (not MessageDone) and
     (World.Player <> nil) and
     PointInside(World.Player.Position) then
  begin
    ReplaceInteractInput.C := 'i';
    ReplaceInteractInput.S := CastleInput_Interact.Description;
    Notifications.Show(SPercentReplace(Message, [ReplaceInteractInput], true));
    MessageDone := true;
  end;
end;

{ TGameSceneManager --------------------------------------------------------------------- }

procedure TGameSceneManager.LoadLevel(const AInfo: TLevelAvailable;
  const AMenuBackground: boolean);
var
  { Sometimes it's not comfortable
    to remove the items while traversing --- so we will instead
    put them on this list.

    Be careful: never add here two nodes such that one may be parent
    of another, otherwise freeing one could free the other one too
    early. }
  ItemsToRemove: TX3DNodeList;

  procedure LoadAreas(Element: TDOMElement);

    procedure MissingRequiredAttribute(const AttrName, ElementName: string);
    begin
      raise Exception.CreateFmt(
        'Missing required attribute "%s" of <%s> element', [AttrName, ElementName]);
    end;

    function LevelAreaFromDOMElement(Element: TDOMElement): TLevelHintArea;
    var
      Child: TDOMElement;
    begin
      if Element.TagName = 'area' then
      begin
        Child := DOMGetOneChildElement(Element);
        if Child.TagName = 'hint' then
        begin
          Result := TLevelHintArea.Create(Self);
          Result.Message := DOMGetTextData(Child);
        end else
          raise Exception.CreateFmt('Not allowed children element of <area>: "%s"',
            [Child.TagName]);
        if not DOMGetAttribute(Element, 'id', Result.FId) then
          MissingRequiredAttribute('id', 'area');
      end else
      if (Element.TagName = 'resources') or
         (Element.TagName = 'bump_mapping_light') then
      begin
        { These are handled elsewhere, and don't produce any T3D. }
        Result := nil;
      end else
        raise Exception.CreateFmt('Not allowed children element of <level>: "%s"',
          [Element.TagName]);
    end;

  var
    I: TXMLElementIterator;
    NewArea: TLevelArea;
  begin
    I := TXMLElementIterator.Create(Element);
    try
      while I.GetNext do
      begin
        NewArea := LevelAreaFromDOMElement(I.Current);
        if NewArea <> nil then
        begin
          NewArea.ChangeLevelScene(MainScene);
          Items.Add(NewArea);
        end;
      end;
    finally FreeAndNil(I) end;
  end;

  procedure TraverseForItems(Shape: TShape);

    procedure CreateNewItem(const ItemNodeName: string);
    var
      Resource: T3DResource;
      ItemKind: TItemKind;
      IgnoredBegin, ItemQuantityBegin: Integer;
      ItemKindQuantity, ItemKindId: string;
      ItemQuantity: Cardinal;
      ItemStubBoundingBox: TBox3D;
      ItemPosition: TVector3Single;
    begin
      { Calculate ItemKindQuantity }
      IgnoredBegin := Pos('_', ItemNodeName);
      if IgnoredBegin = 0 then
        ItemKindQuantity := ItemNodeName else
        ItemKindQuantity := Copy(ItemNodeName, 1, IgnoredBegin - 1);

      { Calculate ItemKindId, ItemQuantity }
      ItemQuantityBegin := CharsPos(['0'..'9'], ItemKindQuantity);
      if ItemQuantityBegin = 0 then
      begin
        ItemKindId := ItemKindQuantity;
        ItemQuantity := 1;
      end else
      begin
        ItemKindId := Copy(ItemKindQuantity, 1, ItemQuantityBegin - 1);
        ItemQuantity := StrToInt(SEnding(ItemKindQuantity, ItemQuantityBegin));
      end;

      Resource := AllResources.FindId(ItemKindId);
      if not (Resource is TItemKind) then
        raise Exception.CreateFmt('Resource "%s" is not an item, but is referenced in model with Item prefix',
          [ItemKindId]);
      ItemKind := TItemKind(Resource);

      ItemStubBoundingBox := Shape.BoundingBox;
      ItemPosition[0] := (ItemStubBoundingBox.Data[0, 0] + ItemStubBoundingBox.Data[1, 0]) / 2;
      ItemPosition[1] := (ItemStubBoundingBox.Data[0, 1] + ItemStubBoundingBox.Data[1, 1]) / 2;
      ItemPosition[2] := ItemStubBoundingBox.Data[0, 2];

      ItemKind.CreateItem(ItemQuantity).PutOnLevel(Items, ItemPosition);
    end;

  const
    ItemPrefix = 'Item';
  begin
    if IsPrefix(ItemPrefix, Shape.BlenderMeshName) then
    begin
      { For MenuBackground, item models may be not loaded yet }
      if not MenuBackground then
        CreateNewItem(SEnding(Shape.BlenderMeshName, Length(ItemPrefix) + 1));
      { Don't remove BlenderObjectNode now --- will be removed later.
        This avoids problems with removing nodes while traversing. }
      ItemsToRemove.Add(Shape.BlenderObjectNode);
    end;
  end;

  procedure TraverseForCreatures(Shape: TShape);

    procedure CreateNewCreature(const CreatureNodeName: string);
    var
      StubBoundingBox: TBox3D;
      CreaturePosition, CreatureDirection: TVector3Single;
      Resource: T3DResource;
      CreatureKind: TCreatureKind;
      CreatureKindName: string;
      IgnoredBegin: Integer;
      MaxLifeBegin: Integer;
      IsMaxLife: boolean;
      MaxLife: Single;
    begin
      { calculate CreatureKindName }
      IgnoredBegin := Pos('_', CreatureNodeName);
      if IgnoredBegin = 0 then
        CreatureKindName := CreatureNodeName else
        CreatureKindName := Copy(CreatureNodeName, 1, IgnoredBegin - 1);

      { possibly calculate MaxLife by truncating last part of CreatureKindName }
      MaxLifeBegin := CharsPos(['0'..'9'], CreatureKindName);
      IsMaxLife := MaxLifeBegin <> 0;
      if IsMaxLife then
      begin
        MaxLife := StrToFloat(SEnding(CreatureKindName, MaxLifeBegin));
        CreatureKindName := Copy(CreatureKindName, 1, MaxLifeBegin - 1);
      end;

      { calculate CreaturePosition }
      StubBoundingBox := Shape.BoundingBox;
      CreaturePosition[0] := (StubBoundingBox.Data[0, 0] + StubBoundingBox.Data[1, 0]) / 2;
      CreaturePosition[1] := (StubBoundingBox.Data[0, 1] + StubBoundingBox.Data[1, 1]) / 2;
      CreaturePosition[2] := StubBoundingBox.Data[0, 2];

      { calculate CreatureKind }
      Resource := AllResources.FindId(CreatureKindName);
      if not (Resource is TCreatureKind) then
        raise Exception.CreateFmt('Resource "%s" is not a creature, but is referenced in model with Crea prefix',
          [CreatureKindName]);
      CreatureKind := TCreatureKind(Resource);
      if not CreatureKind.Prepared then
        OnWarning(wtMajor, 'Resource', Format('Creature "%s" is initially present on the level, but was not prepared yet --- which probably means you did not add it to <resources> inside level index.xml file. This causes loading on-demand, which is less comfortable for player.',
          [CreatureKind.Id]));

      { calculate CreatureDirection }
      { TODO --- CreatureDirection configurable.
        Right now, it just points to the player start pos --- this is
        more-or-less sensible, usually. }
      CreatureDirection := VectorSubtract(Camera.GetPosition, CreaturePosition);
      if not CreatureKind.Flying then
        MakeVectorsOrthoOnTheirPlane(CreatureDirection, GravityUp);

      { make sure that MaxLife is initialized now }
      if not IsMaxLife then
      begin
        IsMaxLife := true;
        MaxLife := CreatureKind.DefaultMaxLife;
      end;

      CreatureKind.CreateCreature(Items, CreaturePosition, CreatureDirection, MaxLife);
    end;

  const
    CreaturePrefix = 'Crea';
  begin
    if IsPrefix(CreaturePrefix, Shape.BlenderMeshName) then
    begin
      { For MenuBackground, creature models may be not loaded yet }
      if not MenuBackground then
        CreateNewCreature(SEnding(Shape.BlenderMeshName, Length(CreaturePrefix) + 1));
      { Don't remove BlenderObjectNode now --- will be removed later.
        This avoids problems with removing nodes while traversing. }
      ItemsToRemove.Add(Shape.BlenderObjectNode);
    end;
  end;

  procedure RemoveItemsToRemove;
  var
    I: Integer;
  begin
    MainScene.BeforeNodesFree;
    for I := 0 to ItemsToRemove.Count - 1 do
      ItemsToRemove.Items[I].FreeRemovingFromAllParents;
    MainScene.ChangedAll;
  end;

  { Assign Camera, knowing MainScene and Player.
    We need to assign Camera early, as initial Camera also is used
    when placing initial creatures on the level (to determine their
    gravity up, initial direciton etc.) }
  procedure InitializeCamera;
  var
    InitialPosition: TVector3Single;
    InitialDirection: TVector3Single;
    InitialUp: TVector3Single;
    GravityUp: TVector3Single;
    CameraRadius, PreferredHeight: Single;
    NavigationNode: TNavigationInfoNode;
    WalkCamera: TWalkCamera;
  begin
    MainScene.GetPerspectiveViewpoint(InitialPosition,
      InitialDirection, InitialUp, GravityUp);

    NavigationNode := MainScene.NavigationInfoStack.Top as TNavigationInfoNode;

    if (NavigationNode <> nil) and (NavigationNode.FdAvatarSize.Count >= 1) then
      CameraRadius := NavigationNode.FdAvatarSize.Items[0] else
      CameraRadius := MainScene.BoundingBox.AverageSize(false, 1) * 0.007;

    if (NavigationNode <> nil) and (NavigationNode.FdAvatarSize.Count >= 2) then
      PreferredHeight := NavigationNode.FdAvatarSize.Items[1] else
      PreferredHeight := CameraRadius * 5;
    CorrectPreferredHeight(PreferredHeight, CameraRadius,
      DefaultCrouchHeight, DefaultHeadBobbing);

    if Player <> nil then
      WalkCamera := (Player as TPlayer).Camera else
      { If you don't initialize Player (like for castle1 background level
        or castle-view-level or lets_take_a_walk) then just create a camera. }
      WalkCamera := TWalkCamera.Create(Self);

    { initialize some navigation settings of player }
    if Player <> nil then
    begin
      (Player as TPlayer).DefaultPreferredHeight := PreferredHeight;
      if NavigationNode <> nil then
        (Player as TPlayer).DefaultMoveHorizontalSpeed := NavigationNode.FdSpeed.Value else
        (Player as TPlayer).DefaultMoveHorizontalSpeed := 1.0;
      (Player as TPlayer).DefaultMoveVerticalSpeed := 20;
    end else
    begin
      { if you use Player with TGameSceneManager, then Player will automatically
        update camera's speed properties. But if not, we have to set them
        here. }
      WalkCamera.PreferredHeight := PreferredHeight;
      if NavigationNode <> nil then
        WalkCamera.MoveHorizontalSpeed := NavigationNode.FdSpeed.Value else
        WalkCamera.MoveHorizontalSpeed := 1.0;
      WalkCamera.MoveVerticalSpeed := 20;
    end;

    { Check GravityUp }
    if not VectorsEqual(GravityUp, Vector3Single(0, 0, 1), 0.001) then
      if Log then
        WritelnLog('Camera', 'Gravity up vector is not +Z. Everything should work fine, but it''s not fully tested');

    Camera := WalkCamera;

    WalkCamera.Init(InitialPosition, InitialDirection,
      InitialUp, GravityUp, PreferredHeight, CameraRadius);
    WalkCamera.CancelFallingDown;
  end;

var
  Options: TPrepareResourcesOptions;
  NewCameraBox, NewWaterBox: TBox3D;
  SI: TShapeTreeIterator;
  PreviousResources: T3DResourceList;
  I: Integer;
begin
  MenuBackground := AMenuBackground;

  { release stuff from previous level. Our items must be clean.
    This releases previous Level (logic), MainScene, our areas added in LoadAreas,
    and our creatures and items --- the ones added in TraverseForCreatures/Items,
    but also the ones created dynamically (when creature is added to scene manager,
    e.g. because player/creature shoots a missile, or when player drops an item).
    The only thing that can (and should) remain is Player. }
  I := 0;
  while I < Items.Count do
    if Items[I] <> Player then
      Items[I].Free else
      Inc(I);
  FLevel := nil; { it's freed now }

  UseGlobalLights := true;
  ApproximateActivation := true;
  Input_PointingDeviceActivate.Assign(CastleInput_Interact.Shortcut, false);

  { save PreviousResources, before Info is overridden with new level.
    This allows us to keep PreviousResources while new resources are required,
    and this means that resources already loaded for previous level
    don't need to be reloaded for new. }
  PreviousResources := T3DResourceList.Create(false);
  if (Info <> nil) then
    PreviousResources.Assign(Info.Resources);

  FInfo := AInfo;
  Inc(LevelsAvailable.References);
  Info.Resources.Prepare(BaseLights);

  PreviousResources.Release;
  FreeAndNil(PreviousResources);

  Progress.Init(1, 'Loading level "' + Info.Title + '"');
  try
    { disconnect previous Camera from SceneManager.
      Othwerwise, it would be updated by MainScene loading binding new
      NavigationInfo (with it's speed) and Viewpoint.
      We prefer to do it ourselves in InitializeCamera. }
    Camera := nil;

    MainScene := TCastleScene.CreateCustomCache(Self, GLContextCache);
    MainScene.Load(Info.SceneFileName);

    MainScene.Attributes.UseSceneLights := true;

    { Scene must be the first one on Items, this way MoveAllowed will
      use Scene for wall-sliding (see T3DList.MoveAllowed implementation). }
    Items.Insert(0, MainScene);

    LoadAreas(Info.Element);

    InitializeCamera;

    ItemsToRemove := TX3DNodeList.Create(false);
    try
      { Initialize Items }
      SI := TShapeTreeIterator.Create(MainScene.Shapes, { OnlyActive } true);
      try
        while SI.GetNext do TraverseForItems(SI.Current);
      finally SysUtils.FreeAndNil(SI) end;

      { Initialize Creatures }
      SI := TShapeTreeIterator.Create(MainScene.Shapes, { OnlyActive } true);
      try
        while SI.GetNext do TraverseForCreatures(SI.Current);
      finally SysUtils.FreeAndNil(SI) end;

      RemoveItemsToRemove;
    finally ItemsToRemove.Free end;

    { Calculate CameraBox. }
    if not MainScene.RemoveBlenderBox(NewCameraBox, 'LevelBox') then
    begin
      { Set CameraBox to MainScene.BoundingBox, and make maximum Z larger. }
      NewCameraBox := MainScene.BoundingBox;
      NewCameraBox.Data[1, 2] += 4 * (NewCameraBox.Data[1, 2] - NewCameraBox.Data[0, 2]);
    end;
    CameraBox := NewCameraBox;

    if MainScene.RemoveBlenderBox(NewWaterBox, 'WaterBox') then
      WaterBox := NewWaterBox;

    CreateSectors(MainScene);

    { create Level after creatures and hint areas are initialized
      (some TLevel descendant constructors depend on this),
      but still before preparing resources like octrees (because we still
      may want to modify MainScene inside Level constructor). }
    FLevel := Info.LevelClass.Create(Self, Items, MainScene, Info.Element);
    Items.Add(Level);

    { calculate Options for PrepareResources }
    Options := [prRender, prBackground, prBoundingBox];
    if GLShadowVolumesPossible then
      Options := Options + prShadowVolume;

    MainScene.PrepareResources(Options, false, BaseLights);

    MainScene.FreeResources([frTextureDataInNodes]);

    Progress.Step;
  finally
    Progress.Fini;
  end;

  { Loading octree have their own Progress, so we load them outside our
    progress. }

  if not MenuBackground then
  begin
    MainScene.TriangleOctreeProgressTitle := 'Loading level (triangle octree)';
    MainScene.ShapeOctreeProgressTitle := 'Loading level (Shape octree)';
    MainScene.Spatial := [ssRendering, ssDynamicCollisions];
    MainScene.PrepareResources([prSpatial], false, BaseLights);
  end;

  MainScene.ProcessEvents := true;

  { Needed for sick projection effect, that potentially updates
    DistortFieldOfViewY and such every frame. }
  AlwaysApplyProjection := true;
end;

destructor TGameSceneManager.Destroy;
begin
  if Info <> nil then
  begin
    if Info.Resources <> nil then
      Info.Resources.Release;

    Dec(LevelsAvailable.References);
    if LevelsAvailable.References = 0 then
      FreeAndNil(LevelsAvailable);
  end;

  inherited;
end;

procedure TGameSceneManager.InitializeLights(const Lights: TLightInstancesList);
begin
  inherited;

  { This is used to prepare BaseLights, which may be necessary in constructor
    before we even assign Level. }
  if (Level <> nil) and (Level.Thunder <> nil) then
    Level.Thunder.AddLight(Lights);
end;

procedure TGameSceneManager.ApplyProjection;
var
  S, C: Extended;
begin
  { After LoadLevel, we always have here Camera <> nil.
    But it's also possible to have Camera = nil if no LoadLevel was called yet. }

  DistortFieldOfViewY := 1;
  DistortViewAspect := 1;
  if SickProjection then
  begin
    SinCos(Level.AnimationTime * SickProjectionSpeed, S, C);
    DistortFieldOfViewY += C * 0.03;
    DistortViewAspect += S * 0.03;
  end;

  inherited;
end;

procedure TGameSceneManager.SetSickProjection(const Value: boolean);
begin
  if FSickProjection <> Value then
  begin
    FSickProjection := Value;
    ApplyProjectionNeeded := true;
  end;
end;

procedure TGameSceneManager.SetSickProjectionSpeed(const Value: TFloatTime);
begin
  if FSickProjectionSpeed <> Value then
  begin
    FSickProjectionSpeed := Value;
    if SickProjection then ApplyProjectionNeeded := true;
  end;
end;

procedure TGameSceneManager.PointingDeviceActivateFailed(const Active: boolean);
begin
  inherited;
  if Active then
    SoundEngine.Sound(stPlayerInteractFailed);
end;

function TGameSceneManager.CollisionIgnoreItem(
  const Sender: TObject; const Triangle: P3DTriangle): boolean;
begin
  Result :=
    (inherited CollisionIgnoreItem(Sender, Triangle)) or
    (PTriangle(Triangle)^.State.LastNodes.Material.NodeName = 'MatWater');
end;

function TGameSceneManager.Background: TBackground;
begin
  if Level <> nil then
    Result := Level.Background else
    Result := nil;
  if Result = nil then
    Result := inherited;
end;

procedure TGameSceneManager.Idle(const CompSpeed: Single;
  const HandleMouseAndKeys: boolean; var LetOthersHandleMouseAndKeys: boolean);
begin
  inherited;
  if MenuBackground or
    ( (Player <> nil) and
      ( ((Player is TPlayer) and TPlayer(Player).Blocked) or
        Player.Dead ) ) then
    Input_PointingDeviceActivate.MakeClear else
    Input_PointingDeviceActivate.Assign(CastleInput_Interact.Shortcut, false);
end;

{ TThunder ------------------------------------------------------------- }

constructor TThunder.Create;
begin
  inherited;
  LightNode := TDirectionalLightNode.Create('', '');
  LightNode.FdAmbientIntensity.Value := DefaultThunderAmbientIntensity;
  LightNode.FdColor.Value := DefaultThunderColor;
  LightNode.FdDirection.Value := DefaultThunderDirection;

  Light.Node := LightNode;
  Light.Transform := IdentityMatrix4Single;
  Light.TransformScale := 1;
  Light.Location := ZeroVector3Single;
  Light.Direction := Normalized(LightNode.FdDirection.Value);
  Light.Radius := MaxSingle;
  Light.WorldCoordinates := true;
end;

destructor TThunder.Destroy;
begin
  FreeAndNil(LightNode);
  inherited;
end;

function TThunder.GetAmbientIntensity: Single;
begin
  Result := LightNode.FdAmbientIntensity.Value;
end;

procedure TThunder.SetAmbientIntensity(const Value: Single);
begin
  LightNode.FdAmbientIntensity.Send(Value);
end;

function TThunder.GetColor: TVector3Single;
begin
  Result := LightNode.FdColor.Value;
end;

procedure TThunder.SetColor(const Value: TVector3Single);
begin
  LightNode.FdColor.Send(Value);
end;

function TThunder.GetDirection: TVector3Single;
begin
  Result := LightNode.FdDirection.Value;
end;

procedure TThunder.SetDirection(const Value: TVector3Single);
begin
  LightNode.FdDirection.Send(Value);
  Light.Direction := Normalized(Value);
end;

procedure TThunder.AddLight(const BaseLights: TLightInstancesList);

  function Visible: boolean;
  var
    ThunderTime: Single;
  begin
    Result := false;
    if LastBeginTime <> 0 then
    begin
      ThunderTime := Time - LastBeginTime;
      if (ThunderTime < 1.0) or
         ((1.5 < ThunderTime) and (ThunderTime < 2.5)) then
        Result := true;
    end;
  end;

begin
  if Visible then
    BaseLights.Add(Light);
end;

procedure TThunder.Idle(const CompSpeed: Single);
begin
  Time += CompSpeed;

  if NextBeginTime = 0 then
    NextBeginTime := Time + 10 + Random(10);

  if NextBeginTime <= Time then
  begin
    LastBeginTime := Time;
    NextBeginTime := Time + 10 + Random(20);

    {ThunderAllocatedSound := }SoundEngine.Sound(stThunder);
  end;
end;

procedure TThunder.ForceNow;
begin
  NextBeginTime := Time;
end;

{ TLevel ---------------------------------------------------------------- }

constructor TLevel.Create(AOwner: TComponent; AWorld: T3DWorld;
  MainScene: TCastleScene; DOMElement: TDOMElement);
begin
  inherited Create(AOwner);
  SceneManager := AOwner as TGameSceneManager;
  { Actually, the fact that our BoundingBox is empty also prevents collisions.
    But for some methods, knowing that Collides = false allows them to exit
    faster. }
  Collides := false;
end;

destructor TLevel.Destroy;
begin
  FreeAndNil(FThunder);
  inherited;
end;

function TLevel.BoundingBox: TBox3D;
begin
  { This object is invisible and non-colliding. }
  Result := EmptyBox3D;
end;

function TLevel.BossCreatureIndicator(out Life, MaxLife: Single): boolean;
begin
  Result := (BossCreature <> nil) and (not BossCreature.Dead);
  if Result then
  begin
    Life := BossCreature.Life;
    MaxLife := BossCreature.MaxLife;
  end;
end;

procedure TLevel.PrepareNewPlayer(NewPlayer: TPlayer);
begin
  { Nothing to do in this class. }
end;

function TLevel.LoadLevelScene(const FileName: string;
  CreateOctreeCollisions, PrepareBackground: boolean;
  const SceneClass: TCastleSceneClass): TCastleScene;
var
  Options: TPrepareResourcesOptions;
begin
  Result := SceneClass.CreateCustomCache(Self, GLContextCache);
  Result.Load(FileName);

  { calculate Options for PrepareResources }
  Options := [prRender, prBoundingBox { always needed }];
  if PrepareBackground then
    Include(Options, prBackground);
  if GLShadowVolumesPossible then
    Options := Options + prShadowVolume;

  Result.PrepareResources(Options, false, SceneManager.BaseLights);

  if CreateOctreeCollisions then
    Result.Spatial := [ssDynamicCollisions];

  Result.FreeResources([frTextureDataInNodes]);

  Result.ProcessEvents := true;
end;

function TLevel.LoadLevelScene(const FileName: string;
  CreateOctreeCollisions, PrepareBackground: boolean): TCastleScene;
begin
  Result := LoadLevelScene(FileName, CreateOctreeCollisions, PrepareBackground,
    TCastleScene);
end;

function TLevel.LoadLevelAnimation(
  const FileName: string;
  CreateFirstOctreeCollisions,
  CreateLastOctreeCollisions: boolean;
  const AnimationClass: TCastlePrecalculatedAnimationClass): TCastlePrecalculatedAnimation;
var
  Options: TPrepareResourcesOptions;
begin
  Result := AnimationClass.CreateCustomCache(Self, GLContextCache);
  Result.LoadFromFile(FileName, false, true, 1);

  { calculate Options for PrepareResources }
  Options := [prRender, prBoundingBox { always needed }];
  if GLShadowVolumesPossible then
    Options := Options + prShadowVolume;

  Result.PrepareResources(Options, false, SceneManager.BaseLights);

  if CreateFirstOctreeCollisions then
    Result.FirstScene.Spatial := [ssDynamicCollisions];

  if CreateLastOctreeCollisions then
    Result.LastScene.Spatial := [ssDynamicCollisions];

  Result.FreeResources([frTextureDataInNodes]);

  Result.TimePlaying := false;
end;

function TLevel.LoadLevelAnimation(
  const FileName: string;
  CreateFirstOctreeCollisions,
  CreateLastOctreeCollisions: boolean): TCastlePrecalculatedAnimation;
begin
  Result := LoadLevelAnimation(FileName,
    CreateFirstOctreeCollisions, CreateLastOctreeCollisions,
    TCastlePrecalculatedAnimation);
end;

procedure TLevel.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  FAnimationTime += CompSpeed;
  if Thunder <> nil then
    Thunder.Idle(CompSpeed);
end;

function TLevel.Background: TBackground;
begin
  Result := nil;
end;

{ TLevelAvailable ------------------------------------------------------------ }

constructor TLevelAvailable.Create;
begin
  inherited;
  Resources := T3DResourceList.Create(false);
end;

destructor TLevelAvailable.Destroy;
begin
  FreeAndNil(Document);
  FreeAndNil(Resources);
  FreeAndNil(LoadingImage);
  inherited;
end;

procedure TLevelAvailable.LoadFromDocument;

  procedure MissingRequiredAttribute(const AttrName: string);
  begin
    raise Exception.CreateFmt(
      'Missing required attribute "%s" of <level> element', [AttrName]);
  end;

  { Like DOMGetAttribute, but reads TLevelClass value. }
  function DOMGetLevelClassAttribute(const Element: TDOMElement;
    const AttrName: string; var Value: TLevelClass): boolean;
  var
    ValueStr: string;
    LevelClassIndex: Integer;
  begin
    Result := DOMGetAttribute(Element, AttrName, ValueStr);
    LevelClassIndex := LevelClasses.IndexOf(ValueStr);
    if LevelClassIndex <> -1 then
      Value := LevelClasses.Data[LevelClassIndex] else
      raise Exception.CreateFmt('Unknown level type "%s"', [ValueStr]);
  end;

  { Add all item kinds (TItemKind from AllResources) to the Resources.
    We use this now, as all levels should prepare all items -- always.
    This is easier, as player may pick/drop any item on any level,
    so it's best to have all items prepared. }
  procedure AddItems(Resources: T3DResourceList);
  var
    I: Integer;
  begin
    for I := 0 to AllResources.Count - 1 do
      if (AllResources[I] is TItemKind) and
         (Resources.IndexOf(AllResources[I]) = -1) then
      Resources.Add(AllResources[I]);
  end;

var
  LoadingImageFileName: string;
  SoundName: string;
begin
  Element := Document.DocumentElement;

  if Element.TagName <> 'level' then
    raise Exception.CreateFmt('Root node of levels/*/index.xml file must be <level>, but is "%s", in index.xml inside "%s"',
      [Element.TagName, DocumentBasePath]);

  { Required atttributes }

  if not DOMGetAttribute(Element, 'id', Id) then
    MissingRequiredAttribute('id');

  if not DOMGetAttribute(Element, 'scene', SceneFileName) then
    MissingRequiredAttribute('scene');
  SceneFileName := CombinePaths(DocumentBasePath, SceneFileName);

  if not DOMGetAttribute(Element, 'title', Title) then
    MissingRequiredAttribute('title');

  { Optional attributes }

  if not DOMGetIntegerAttribute(Element, 'number', Number) then
    Number := 0;

  if not DOMGetBooleanAttribute(Element, 'demo', Demo) then
    Demo := false;

  if not DOMGetAttribute(Element, 'title_hint', TitleHint) then
    TitleHint := '';

  if not DOMGetBooleanAttribute(Element, 'default_available_for_new_game',
    DefaultAvailableForNewGame) then
    DefaultAvailableForNewGame := false;

  if not DOMGetLevelClassAttribute(Element, 'type', LevelClass) then
    LevelClass := TLevel;

  FreeAndNil(LoadingImage); { make sure LoadingImage is clear first }
  if DOMGetAttribute(Element, 'loading_image', LoadingImageFileName) then
  begin
    LoadingImageFileName := CombinePaths(DocumentBasePath, LoadingImageFileName);
    LoadingImage := LoadImage(LoadingImageFileName, [TRGBImage], []) as TRGBImage;
  end;

  if not DOMGetSingleAttribute(Element, 'loading_image_bar_y_position',
    LoadingImageBarYPosition) then
    LoadingImageBarYPosition := DefaultImageBarYPosition;

  Resources.LoadResources(Element);
  AddItems(Resources);

  if DOMGetAttribute(Element, 'music_sound', SoundName) then
    MusicSound := SoundEngine.SoundFromName(SoundName) else
    MusicSound := stNone;
end;

procedure TLevelAvailable.LoadLevel(const SceneManager: TGameSceneManager;
  const MenuBackground: boolean);

  procedure LoadLevelCore;
  begin
    SceneManager.LoadLevel(Self, MenuBackground);
    if not MenuBackground then
      AvailableForNewGame := true;
  end;

var
  SavedImage: TRGBImage;
  SavedImageBarYPosition: Single;
begin
  if LoadingImage <> nil then
  begin
    SavedImage := Progress.UserInterface.Image;
    SavedImageBarYPosition := Progress.UserInterface.ImageBarYPosition;
    try
      Progress.UserInterface.Image := LoadingImage;
      Progress.UserInterface.ImageBarYPosition := LoadingImageBarYPosition;
      LoadLevelCore;
    finally
      Progress.UserInterface.Image := SavedImage;
      Progress.UserInterface.ImageBarYPosition := SavedImageBarYPosition;
    end;
  end else
    LoadLevelCore;
end;

{ TLevelAvailableList ------------------------------------------------------- }

function TLevelAvailableList.FindId(const AId: string): TLevelAvailable;
var
  I: Integer;
  S: string;
begin
  for I := 0 to Count - 1 do
    if Items[I].Id = AId then
      Exit(Items[I]);

  S := Format('Level identifier "%s" is not found on the list (LevelsAvailable)', [AId]);
  if Count = 0 then
    S += '.' + NL + NL + 'Warning: there are no levels available on the list at all. This means that the game data was not correctly installed (as we did not find any index.xml files defining any levels). Or the developer forgot to call LevelsAvailable.LoadFromFiles.';
  raise Exception.Create(S);
end;

function IsSmallerByNumber(const A, B: TLevelAvailable): Integer;
begin
  Result := A.Number - B.Number;
end;

procedure TLevelAvailableList.SortByNumber;
begin
  Sort(@IsSmallerByNumber);
end;

procedure TLevelAvailableList.LoadFromConfig;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].AvailableForNewGame := Config.GetValue(
      'levels_available/' + Items[I].Id,
      Items[I].DefaultAvailableForNewGame);
end;

procedure TLevelAvailableList.SaveToConfig(const Config: TCastleConfig);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Config.SetDeleteValue(
      'levels_available/' + Items[I].Id,
      Items[I].AvailableForNewGame,
      Items[I].DefaultAvailableForNewGame);
end;

procedure TLevelAvailableList.LoadIndexXml(const FileName: string);
var
  NewLevelAvailable: TLevelAvailable;
begin
  NewLevelAvailable := TLevelAvailable.Create;
  Add(NewLevelAvailable);
  NewLevelAvailable.AvailableForNewGame := false;

  ReadXMLFile(NewLevelAvailable.Document, FileName);
  NewLevelAvailable.DocumentBasePath := ExtractFilePath(FileName);
  NewLevelAvailable.LoadFromDocument;
end;

procedure TLevelAvailableList.LoadFromFiles(const LevelsPath: string);
begin
  ScanForFiles(LevelsPath, 'index.xml', @LoadIndexXml);
end;

{ globals -------------------------------------------------------------------- }

var
  FLevelClasses: TLevelClasses;

function LevelClasses: TLevelClasses;
begin
  if FLevelClasses = nil then
  begin
    FLevelClasses := TLevelClasses.Create;
    FLevelClasses['Level'] := TLevel;
  end;
  Result := FLevelClasses;
end;

{ initialization / finalization ---------------------------------------------- }

initialization
  LevelsAvailable := TLevelAvailableList.Create(true);
  Inc(LevelsAvailable.References);

  Config.OnSave.Add(@LevelsAvailable.SaveToConfig);
finalization
  FreeAndNil(FLevelClasses);

  if (LevelsAvailable <> nil) and (Config <> nil) then
    Config.OnSave.Remove(@LevelsAvailable.SaveToConfig);

  { there may still exist TGameSceneManager instances that refer to our
    TLevelAvailable instances. So we don't always free LevelsAvailable below. }
  if LevelsAvailable <> nil then
  begin
    Dec(LevelsAvailable.References);
    if LevelsAvailable.References = 0 then
      FreeAndNil(LevelsAvailable);
  end;
end.
