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
  X3DNodes, X3DFields, CastleItems, Cameras, CastleCreatures,
  CastleUtils, CastleClassUtils, CastlePlayer, CastleResources,
  ProgressUnit, PrecalculatedAnimation,
  DOM, CastleSoundEngine, Base3D, Shape, GL, CastleConfig, Images,
  Classes, CastleTimeUtils, CastleSceneManager, GLRendererShader, FGL;

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
    Name: string;

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

    { Load game level.
      This clears all 3D items from @link(TCastleSceneManager.Items)
      list (except @link(TCastleSceneManager.Player)), clears
      @link(TCastleSceneManager.Camera) and @link(TCastleSceneManager.MainScene)
      as well. Then it loads a new level and camera, adding to
      @link(TCastleSceneManager.Items) all resources (creatures and items) defined
      by placeholders in the level 3D file.
      It also prepares level for fast processing
      and rendering (creating octrees, OpenGL resources etc.). }
    procedure LoadLevel(const SceneManager: TGameSceneManager;
      const MenuBackground: boolean = false);
  end;

  TLevelAvailableList = class(specialize TFPGObjectList<TLevelAvailable>)
  private
    { How many TGameSceneManager have references to our children by
      TGameSceneManager.Info? }
    References: Cardinal;
    procedure LoadLevelXml(const FileName: string);
    { Save AvailableForNewGame properties of every item. }
    procedure SaveToConfig(const Config: TCastleConfig);
  public
    { raises Exception if such Name is not on the list. }
    function FindName(const AName: string): TLevelAvailable;

    procedure SortByNumber;

    { Add all available levels found by scanning for level.xml inside data directory.
      Overloaded version without parameter just looks inside ProgramDataPath.
      For the specification of level.xml format see
      http://svn.code.sf.net/p/castle-engine/code/trunk/castle_game_engine/doc/README_about_index_xml_files.txt .

      All AvailableForNewGame are initially set to @false.
      You must later call LoadFromConfig to read user preferences
      and set AvailableForNewGame correctly (depending on levels that user
      already finished, looking at DefaultAvailableForNewGame).
      That's why LoadFromConfig has to be called explicitly,
      it isn't added to Config.OnLoad list.

      This should be called only after resources (creatures and items) are known,
      as they may be referenced by level.xml files.
      So call AllResources.LoadFromFiles before calling this (if you use
      any creatures / items at all, of course).
      @groupBegin }
    procedure LoadFromFiles(const LevelsPath: string);
    procedure LoadFromFiles;
    { @groupEnd }

    { For all available levels, read their TLevelAvailable.AvailableForNewGame
      from user preferences.

      This is useful only if you actually look at
      TLevelAvailable.AvailableForNewGame for any purpose (for example,
      to decide which levels are displayed in the menu). By default,
      our engine doesn't look at AvailableForNewGame for anything. }
    procedure LoadFromConfig;
  end;

  TGameSceneManager = class(TCastleSceneManager)
  private
    FLevel: TLevel;
    FInfo: TLevelAvailable;
    MenuBackground: boolean;
    SickProjectionTime: TFloatTime;

    procedure LoadLevel(const AInfo: TLevelAvailable;
      const AMenuBackground: boolean);
  public
    destructor Destroy; override;

    { Level logic and state. }
    property Level: TLevel read FLevel;

    { Level information, independent from current level state. }
    property Info: TLevelAvailable read FInfo;

    procedure Idle(const CompSpeed: Single;
      const HandleMouseAndKeys: boolean;
      var LetOthersHandleMouseAndKeys: boolean); override;
  end;

  { Level logic. We use T3D descendant, since this is the comfortable
    way to add any behavior to the 3D world (it doesn't matter that
    "level logic" is not a usual 3D object --- it doesn't have to collide
    or be visible). }
  TLevel = class(T3D)
  private
    FTime: TFloatTime;
    FWorld: T3DWorld;
  protected
    { Load 3D precalculated animation from (*.kanim) file, doing common tasks.
      @unorderedList(
        @item optionally creates triangle octree for the FirstScene and/or LastScene
        @item(call PrepareResources, with prRender, prBoundingBox, prShadowVolume
          (if shadow volumes possible at all in this OpenGL context))
        @item Free texture data, since they will not be needed anymore
        @item TimePlaying is by default @false, so the animation is not playing.
      )
      @groupBegin }
    function LoadLevelAnimation(const FileName: string;
      const CreateFirstOctreeCollisions, CreateLastOctreeCollisions: boolean;
      const AnimationClass: TCastlePrecalculatedAnimationClass): TCastlePrecalculatedAnimation;
    function LoadLevelAnimation(const FileName: string;
      const CreateFirstOctreeCollisions, CreateLastOctreeCollisions: boolean): TCastlePrecalculatedAnimation;
    { @groupEnd }

    { Load 3D scene from file, doing common tasks.
      @unorderedList(
        @item optionally create triangle octree
        @item(call PrepareResources, with prRender, prBoundingBox, prShadowVolume
          (if shadow volumes possible at all in this OpenGL context),)
        @item Free texture data, since they will not be needed anymore
      )
      @groupBegin }
    function LoadLevelScene(const FileName: string;
      const CreateOctreeCollisions: boolean;
      const SceneClass: TCastleSceneClass): TCastleScene;
    function LoadLevelScene(const FileName: string;
      const CreateOctreeCollisions: boolean): TCastleScene;
    { @groupEnd }
  public
    { Create new level instance. Called when resources (creatures and items)
      are already initialized. But before creating octrees,
      so you can modify MainScene contents.

      You have to provide AWorld instance at construction,
      and you have to add created TLevel instance to this AWorld,
      and you cannot change it later. This is necessary, as TLevel descendants
      at construction may actually modify your world, and depend on it later. }
    constructor Create(AOwner: TComponent; AWorld: T3DWorld;
      MainScene: TCastleScene; DOMElement: TDOMElement); reintroduce; virtual;
    function BoundingBox: TBox3D; override;
    function World: T3DWorld; override;

    { Called when new player starts game on this level.
      This is supposed to equip the player with some basic weapon/items.

      Usually level design assumes that player came to level from some
      other level in the game, so he already owns some weapon / items etc.
      But when player uses "New Game" command to get to some already
      AvailableForNewGame non-first level, this method will be called and it should
      give player some basic weapon / items suitable for starting this level.

      In TLevel class implementation of this does nothing.  }
    procedure PrepareNewPlayer(NewPlayer: TPlayer); virtual;

    { Time of the level, in seconds. Time 0 when level is created.
      This is updated in our Idle. }
    property Time: TFloatTime read FTime;

    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;
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

{ TGameSceneManager ---------------------------------------------------------- }

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

  procedure TraverseForResources(Shape: TShape);
  const
    ResourcePrefix = 'Res';
  var
    S, ResourceName: string;
    ResourceNumberPresent: boolean;
    Resource: T3DResource;
    Box: TBox3D;
    Position, Direction: TVector3Single;
    IgnoredBegin, NumberBegin: Integer;
    ResourceNumber: Int64;
  begin
    if IsPrefix(ResourcePrefix, Shape.BlenderMeshName) then
    begin
      { For MenuBackground, resource models may be not loaded yet }
      if not MenuBackground then
      begin
        { S is now <resource_name>[<resource_number>][_<ignored>] }
        S := SEnding(Shape.BlenderMeshName, Length(ResourcePrefix) + 1);

        { cut off optional [_<ignored>] suffix }
        IgnoredBegin := Pos('_', S);
        if IgnoredBegin <> 0 then
          S := Copy(S, 1, IgnoredBegin - 1);

        { calculate ResourceName, ResourceNumber, ResourceNumberPresent }
        NumberBegin := CharsPos(['0'..'9'], S);
        ResourceNumberPresent := NumberBegin <> 0;
        if ResourceNumberPresent then
        begin
          ResourceName := Copy(S, 1, NumberBegin - 1);
          ResourceNumber := StrToInt(SEnding(S, NumberBegin));
        end else
        begin
          ResourceName := S;
          ResourceNumber := 0;
        end;

        Resource := AllResources.FindName(ResourceName);
        if not Resource.Prepared then
          OnWarning(wtMajor, 'Resource', Format('Resource "%s" is initially present on the level, but was not prepared yet --- which probably means you did not add it to <resources> inside level level.xml file. This causes loading on-demand, which is less comfortable for player.',
            [Resource.Name]));

        Box := Shape.BoundingBox;
        Position[0] := (Box.Data[0, 0] + Box.Data[1, 0]) / 2;
        Position[1] := (Box.Data[0, 1] + Box.Data[1, 1]) / 2;
        Position[2] := Box.Data[0, 2];

        { TODO: for now, Direction is not configurable, it just points
          to the player start pos. This is more-or-less sensible for creatures. }
        Direction := Camera.GetPosition - Position;

        Resource.InstantiatePlaceholder(Items, Position, Direction,
          ResourceNumberPresent, ResourceNumber);
      end;

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
    when placing initial resources on the level (to determine their
    initial direciton, World.GravityUp etc.) }
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
    This releases previous Level (logic), MainScene,
    and our creatures and items --- the ones added in TraverseForResources,
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
  if Info <> nil then
  begin
    PreviousResources.Assign(Info.Resources);
    Dec(LevelsAvailable.References);
    FInfo := nil;
  end;

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

    InitializeCamera;

    ItemsToRemove := TX3DNodeList.Create(false);
    try
      SI := TShapeTreeIterator.Create(MainScene.Shapes, { OnlyActive } true);
      try
        while SI.GetNext do TraverseForResources(SI.Current);
      finally SysUtils.FreeAndNil(SI) end;
      RemoveItemsToRemove;
    finally ItemsToRemove.Free end;

    { Calculate CameraBox }
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

    { create Level after resources (creatures and items) are initialized
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

  if (Player <> nil) and (Player is TPlayer) then
    TPlayer(Player).LevelChanged;

  SoundEngine.MusicPlayer.Sound := Info.MusicSound;
  if not MenuBackground then
    Notifications.Show('Loaded level "' + Info.Title + '"');

  MainScene.ProcessEvents := true;
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

procedure TGameSceneManager.Idle(const CompSpeed: Single;
  const HandleMouseAndKeys: boolean; var LetOthersHandleMouseAndKeys: boolean);
var
  S, C: Extended;
begin
  inherited;

  DistortFieldOfViewY := 1;
  DistortViewAspect := 1;
  if (Player is TPlayer) and
     (TPlayer(Player).Swimming = psUnderWater) then
  begin
    SickProjectionTime += CompSpeed;
    SinCos(SickProjectionTime * TPlayer(Player).SickProjectionSpeed, S, C);
    DistortFieldOfViewY += C * 0.03;
    DistortViewAspect += S * 0.03;
  end;

  if MenuBackground or
    ( (Player <> nil) and
      ( ((Player is TPlayer) and TPlayer(Player).Blocked) or
        Player.Dead ) ) then
    Input_PointingDeviceActivate.MakeClear else
    Input_PointingDeviceActivate.Assign(CastleInput_Interact.Shortcut, false);
end;

{ TLevel ---------------------------------------------------------------- }

constructor TLevel.Create(AOwner: TComponent; AWorld: T3DWorld;
  MainScene: TCastleScene; DOMElement: TDOMElement);
begin
  inherited Create(AOwner);
  FWorld := AWorld;
  { Actually, the fact that our BoundingBox is empty also prevents collisions.
    But for some methods, knowing that Collides = false allows them to exit
    faster. }
  Collides := false;
end;

function TLevel.World: T3DWorld;
begin
  Result := FWorld;

  Assert(Result <> nil,
    'TLevel.World should never be nil, you have to provide World at TLevel constructor');
  Assert( ((inherited World) = nil) or ((inherited World) = Result),
    'World specified at TLevel constructor must be the same world where TLevel instance is added');
end;

function TLevel.BoundingBox: TBox3D;
begin
  { This object is invisible and non-colliding. }
  Result := EmptyBox3D;
end;

procedure TLevel.PrepareNewPlayer(NewPlayer: TPlayer);
begin
  { Nothing to do in this class. }
end;

function TLevel.LoadLevelScene(
  const FileName: string;
  const CreateOctreeCollisions: boolean;
  const SceneClass: TCastleSceneClass): TCastleScene;
var
  Options: TPrepareResourcesOptions;
begin
  Result := SceneClass.CreateCustomCache(Self, GLContextCache);
  Result.Load(FileName);

  { calculate Options for PrepareResources }
  Options := [prRender, prBoundingBox { always needed }];
  if GLShadowVolumesPossible then
    Options := Options + prShadowVolume;

  Result.PrepareResources(Options, false, World.BaseLights);

  if CreateOctreeCollisions then
    Result.Spatial := [ssDynamicCollisions];

  Result.FreeResources([frTextureDataInNodes]);

  Result.ProcessEvents := true;
end;

function TLevel.LoadLevelScene(
  const FileName: string;
  const CreateOctreeCollisions: boolean): TCastleScene;
begin
  Result := LoadLevelScene(FileName, CreateOctreeCollisions, TCastleScene);
end;

function TLevel.LoadLevelAnimation(
  const FileName: string;
  const CreateFirstOctreeCollisions, CreateLastOctreeCollisions: boolean;
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

  Result.PrepareResources(Options, false, World.BaseLights);

  if CreateFirstOctreeCollisions then
    Result.FirstScene.Spatial := [ssDynamicCollisions];

  if CreateLastOctreeCollisions then
    Result.LastScene.Spatial := [ssDynamicCollisions];

  Result.FreeResources([frTextureDataInNodes]);

  Result.TimePlaying := false;
end;

function TLevel.LoadLevelAnimation(
  const FileName: string;
  const CreateFirstOctreeCollisions, CreateLastOctreeCollisions: boolean): TCastlePrecalculatedAnimation;
begin
  Result := LoadLevelAnimation(FileName,
    CreateFirstOctreeCollisions, CreateLastOctreeCollisions,
    TCastlePrecalculatedAnimation);
end;

procedure TLevel.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  FTime += CompSpeed;
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
    raise Exception.CreateFmt('Root node of level.xml file must be <level>, but is "%s", in "%s"',
      [Element.TagName, DocumentBasePath]);

  { Required atttributes }

  if not DOMGetAttribute(Element, 'name', Name) then
    MissingRequiredAttribute('name');

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

function TLevelAvailableList.FindName(const AName: string): TLevelAvailable;
var
  I: Integer;
  S: string;
begin
  for I := 0 to Count - 1 do
    if Items[I].Name = AName then
      Exit(Items[I]);

  S := Format('Level name "%s" is not found on the list (LevelsAvailable)', [AName]);
  if Count = 0 then
    S += '.' + NL + NL + 'Warning: there are no levels available on the list at all. This means that the game data was not correctly installed (as we did not find any level.xml files defining any levels). Or the developer forgot to call LevelsAvailable.LoadFromFiles.';
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
      'levels_available/' + Items[I].Name,
      Items[I].DefaultAvailableForNewGame);
end;

procedure TLevelAvailableList.SaveToConfig(const Config: TCastleConfig);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Config.SetDeleteValue(
      'levels_available/' + Items[I].Name,
      Items[I].AvailableForNewGame,
      Items[I].DefaultAvailableForNewGame);
end;

procedure TLevelAvailableList.LoadLevelXml(const FileName: string);
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
  ScanForFiles(LevelsPath, 'level.xml', @LoadLevelXml);
end;

procedure TLevelAvailableList.LoadFromFiles;
begin
  LoadFromFiles(ProgramDataPath);
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
