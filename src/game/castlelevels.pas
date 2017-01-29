{
  Copyright 2006-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Scene manager that can easily load game levels (TGameSceneManager),
  management of available game levels (TLevelInfo, @link(Levels)). }
unit CastleLevels;

{$I castleconf.inc}

interface

uses Classes, DOM, FGL,
  CastleVectors, CastleSceneCore, CastleScene, CastleBoxes, X3DNodes,
  X3DFields, CastleCameras, CastleSectors, CastleUtils, CastleClassUtils,
  CastlePlayer, CastleResources, CastleProgress, CastlePrecalculatedAnimation,
  CastleSoundEngine, Castle3D, CastleShapes, CastleXMLConfig, CastleImages,
  CastleTimeUtils, CastleSceneManager, CastleFindFiles;

type
  TLevelLogic = class;
  TLevelLogicClass = class of TLevelLogic;
  TCastleSceneClass = class of TCastleScene;
  {$warnings off}
  TCastlePrecalculatedAnimationClass = class of TCastlePrecalculatedAnimation;
  {$warnings on}
  TGameSceneManager = class;

  TLevelInfo = class
  private
    FLogicClass: TLevelLogicClass;
    FName: string;
    FSceneURL: string;
    FTitle: string;
    FTitleHint: string;
    FNumber: Integer;
    FDemo: boolean;
    FPlayed: boolean;
    FDefaultPlayed: boolean;
    FLoadingImage: TRGBImage;
    FLoadingBarYPosition: Single;
    FPlaceholderName: TPlaceholderName;
    FPlaceholderReferenceDirection: TVector3Single;
    FMusicSound: TSoundType;
    { We keep XML Document reference through the lifetime of this object,
      to allow the particular level logic (TLevelLogic descendant)
      to read some level-logic-specific variables from it. }
    Document: TXMLDocument;
    DocumentBaseURL: string;
    LevelResources: T3DResourceList;
    procedure LoadFromDocument;
  protected
    Element: TDOMElement;
  public
    constructor Create;
    destructor Destroy; override;

    (*Level logic class. This is indicated by the "type" attribute in level.xml
      file. By default level logic uses a base TLevelLogic class,
      which corresponds to using @code(type="Level") in level.xml file.

      Changing this allows to use a different ObjectPascal class to implement
      the level behavior.
      You can create your own TLevelLogic descendants and register them
      like

      @longCode(#
      type
        TMyLevelLogic = class(TLevelLogic)
          { ... override whatever you want ... }
        end;

      ...
      LevelLogicClasses['MyLevel'] := TMyLevelLogic;
      #)

      And then you can use @code(type="MyLevel") inside level.xml
      to use your class.
      Many advanced tricks are possible by implementing your own TLevelLogic
      descendant. See Castle1 GameLevelSpecific.pas unit for some examples.
    *)
    property LogicClass: TLevelLogicClass read FLogicClass write FLogicClass;

    { Unique identifier of this level. This name may be useful in scripts,
      as TGameSceneManager.LoadLevel parameter and such.

      For all (current and future) uses it should be a valid VRML/X3D
      and ObjectPascal identifier, so use only (English) letters,
      underscores and digits (and don't start with a digit). }
    property Name: string read FName write FName;

    { Main level 3D model. When the level is loaded, this scene will be set
      as TCastleSceneManager.MainScene,
      so it determines the default viewpoint, background and such.

      Usually it also contains the most (if not all) of 3D level visible geometry,
      scripts and such. Although level logic (TLevelLogic descendant determined
      by LevelClass) may also add any number of additional 3D objects
      (T3D instances) to the 3D world. }
    property SceneURL: string read FSceneURL write FSceneURL;

    { @deprecated Deprecated name for SceneURL. }
    property SceneFileName: string read FSceneURL write FSceneURL; deprecated;

    { Nice name of the level for user. This should be user-friendly,
      so it can use spaces, non-English letters and such.

      The core engine (like this CastleLevels unit) doesn't use this title
      much, it's only for progress bars and log messages.

      It is up to the final game code to use this title in more interesting
      ways. For example to show a choice of starting level to the user.
      The core engine doesn't make such "New Game" menus automatically
      (because various games may have widly different ideas how to make it
      and how to show it). But we give the developer tools to make such
      menus easily, for example you can use TCastleOnScreenMenu for this. }
    property Title: string read FTitle write FTitle;

    { Additional text that may be displayed near level title.

      The engine doesn't use this property at all, it's only loaded from level.xml
      file. It is available for your "New Game" (or similar screen) implementation
      (see @link(Title) for more comments about this). }
    property TitleHint: string read FTitleHint write FTitleHint;

    { Level number.

      The engine doesn't use this property at all, it's only loaded from level.xml
      file. It is available for your "New Game" (or similar screen) implementation
      (see @link(Title) for more comments about this).

      For example level number may be used to order levels in the menu.
      This @italic(does not) determine the order in which levels are played,
      as levels do not have to be played in a linear order. }
    property Number: Integer read FNumber write FNumber default 0;

    { Is it a demo level.

      The engine doesn't use this property at all, it's only loaded from level.xml
      file. It is available for your "New Game" (or similar screen) implementation
      (see @link(Title) for more comments about this). }
    property Demo: boolean read FDemo write FDemo default false;

    { Was the level played.

      This is automatically managed. Basically, we set it to @true
      when the level is played, and we save it to user preferences.

      Details:
      @unorderedList(
        @item(It is set to @true when loading level.)

        @item(It is saved to disk (user preferences file) when game exits,
          and loaded when game starts. As long as you call @code(Config.Load),
          @code(Config.Save) from CastleConfig unit. To load, you must
          also call explicitly @link(TLevelInfoList.LoadFromConfig Levels.LoadFromConfig) now.)

        @item(The default value comes from DefaultPlayed property,
          which in turn is loaded from level.xml file, and by default is @false.
          To allows you to artificially mark some levels as "Played" on
          the first game run, which may be helpful if you use this property
          e.g. to filter the levels in "New Game" menu.)
      )

      The engine doesn't look at this property for anything,
      whether the level is played or not has no influence over how we work.
      It is just available for your game, for example to use in your
      "New Game" (or similar screen) implementation
      (see @link(Title) for more comments about this). }
    property Played: boolean read FPlayed write FPlayed;

    { Should the level be initially considered "played".
      This determines the initial value of @link(Played) property
      (if nothing was stored in user preferences about this level,
      see TLevelInfoList.LoadFromConfig; or if TLevelInfoList.LoadFromConfig
      is not called by the game).

      How is this information useful, depends on a particular game.
      For example, some games may decide to show in the "New Game"
      menu levels only with Played=true. Some games may simply ignore it. }
    property DefaultPlayed: boolean read FDefaultPlayed write FDefaultPlayed;

    { Background image shown when loading the level, @nil if none.
      This is loaded from URL indicated by attribute loading_image
      in level.xml. }
    property LoadingImage: TRGBImage read FLoadingImage write FLoadingImage;

    { Vertical position of progress bar when loading the level.
      Between 0 and 1, default value 0.5 means "middle of the screen".
      Should be synchronized with how LoadingImage looks (if you set it),
      to make the progress when loading level look nice.

      Technically, this is used for TProgressUserInterface.BarYPosition. }
    property LoadingBarYPosition: Single
      read FLoadingBarYPosition write FLoadingBarYPosition
      default TProgressUserInterface.DefaultBarYPosition;

    { Placeholder detection method. See TPlaceholderName, and see
      TGameSceneManager.LoadLevel for a description when we use placeholders.
      Default value is @code(PlaceholderNames['x3dshape']). }
    property PlaceholderName: TPlaceholderName
      read FPlaceholderName write FPlaceholderName;

    { How to interpret the placeholders direction (like the initial
      creature direction on the level).
      Some placeholders (currently, only creatures) may be used to determine
      initial direction of the resource. For example, the direction
      the creature is initially facing.
      This direction is calculated as the transformation
      of given placeholder applied to this 3D vector.

      The correct value may depend on the exporter you used to create 3D models,
      and on the exporter settings (how and if it rotates the model when exporting,
      and is this rotation recorded in placeholder transformation
      or applied directly to mesh coordinates). It may also depend on personal
      preference, as it determines how you set resources in your 3D modelling tool
      (like Blender).

      Fortunately, the default value (+X vector) is suitable for at least
      2 common situations:

      @orderedList(
        @item(If your exporter rotates the world to turn +Z up into +Y up.
          (This is the case of default Blender X3D exporter with default settings.))

        @item(If your exporter doesn't rotate the world.
          (You can configure Blender exporter to behave like this.
          You may also then configure engine to use +Z as up vector for everything,
          see "Which way is up?" on [http://castle-engine.sourceforge.net/tutorial_up.php].))
      )

      In Blender it's useful to enable the "Display -> Wire" option for placeholder
      objects, then Blender will show arrows inside the placeholder.
      +X of the arrow determines the default direction understood by our engine. }
    property PlaceholderReferenceDirection: TVector3Single
      read FPlaceholderReferenceDirection write FPlaceholderReferenceDirection;

    { Music played when entering the level.
      None (stNone) by default. }
    property MusicSound: TSoundType read FMusicSound write FMusicSound;
  end;

  TLevelInfoList = class(specialize TFPGObjectList<TLevelInfo>)
  private
    { How many TGameSceneManager have references to our children by
      TGameSceneManager.Info? }
    References: Cardinal;
    procedure AddFromInfo(const Info: TFileInfo; var StopSearch: boolean);
  public
    { raises Exception if such Name is not on the list. }
    function FindName(const AName: string): TLevelInfo;

    { Add all available levels found by scanning for level.xml inside data
      directory.
      Overloaded version without parameter just looks inside ApplicationData.
      For the specification of level.xml format see
      [http://castle-engine.sourceforge.net/creating_data_levels.php] .

      This should be called after resources (creatures and items) are known,
      as they may be referenced by level.xml files.
      So call @link(T3DResourceList.LoadFromFiles Resources.LoadFromFiles)
      @italic(before) calling this (if you
      want to use any creatures / items at all, of course).

      All TLevelInfo.Played values are initially set to @false.
      You must call LoadFromConfig @italic(after) calling this
      to read TLevelInfo.Played values from user preferences file.

      Note that on Android, searching the Android asset filesystem
      recursively is not possible (this is a fault of Android NDK API...).
      So instead of this method, you should use AddFromFile repeatedly
      to explicitly list all level.xml locations.

      @groupBegin }
    procedure LoadFromFiles(const LevelsPath: string);
    procedure LoadFromFiles;
    { @groupEnd }

    { Add a single level information from the XML file at given location.
      The given XML file must have <level> root element and be written
      according to
      http://castle-engine.sourceforge.net/creating_data_levels.php .  }
    procedure AddFromFile(const URL: string);

    { Sort by @link(TLevelInfo.Number).
      Done automatically at the end of @link(LoadFromFiles),
      you may want to call it explicitly after doing @link(AddFromFile). }
    procedure SortByNumber;

    { For all available levels, read their TLevelInfo.Played
      from config file (like @link(UserConfig)).

      This is useful only if you actually look at
      TLevelInfo.Played for any purpose (for example,
      to decide which levels are displayed in the menu). By default,
      our engine doesn't look at TLevelInfo.Played for anything. }
    procedure LoadFromConfig(const Config: TCastleConfig);

    { Save Played properties of every level
      to a config file (like @link(UserConfig)). }
    procedure SaveToConfig(const Config: TCastleConfig);
  end;

  { Scene manager that can comfortably load and manage a 3D game level.
    It really adds only one new method to TCastleSceneManager:
    @link(LoadLevel), see it's documentation to know what it gives you.
    It also exposes @link(Logic) and @link(Info) properties
    corresponding to the currently loaded level. }
  TGameSceneManager = class(TCastleSceneManager)
  private
    FLogic: TLevelLogic;
    FInfo: TLevelInfo;
    LevelResourcesPrepared: boolean;
    { Like LoadLevel, but doesn't care about AInfo.LoadingImage. }
    procedure LoadLevelCore(const AInfo: TLevelInfo);
    { Unload Items from previous level, keeps only Player on Items.
      Returns previous resources. You have to call Release and free them. }
    function UnloadLevelCore: T3DResourceList;
    function Placeholder(Shape: TShape; PlaceholderName: string): boolean;
  public
    destructor Destroy; override;

    { Load game level.

      @unorderedList(
        @item(@bold(Set scene manager 3D items):

          Clear all 3D items from @link(TCastleSceneManager.Items)
          list (except @link(TCastleSceneManager.Player)), clear
          @link(TCastleAbstractViewport.Camera Camera)
          and @link(TCastleSceneManager.MainScene) as well.
          Then load a new main scene and camera, adding to
          @link(TCastleSceneManager.Items) all 3D resources (creatures and items)
          defined by placeholders named CasRes* in the main level 3D file.)

        @item(@bold(Make sure 3D resources are ready:)

          Resources are T3DResource instances on @link(Resources) list.
          They are heavy (in terms of memory use and preparation time),
          so you don't want to just load everything for every level.
          This method makes sure that all resources required by this level
          are prepared. All resources requested in level.xml file
          (in <resources> element in level.xml),
          as well as resources requested in player.xml file,
          as well as resources with AlwaysPrepared (usually: all possible items
          that can be dropped from player inventory on any level)
          will be prepared.)

        @item(@bold(Initialize move limits and water volume from placeholders).
          Special object names CasMoveLimit and CasWater (in the future:
          CasWater* with any suffix) in the main scene 3D model
          determine the places where player can go and where water is.

          When CasMoveLimit object is missing,
          we calculate it to include the level bounding box, with some
          additional space above (to allow flying).)

        @item(@bold(Initialize sectors and waypoints from placeholders).
          Special shape names CasSector* and CasWaypoint* can be used
          in level 3D model to help creature AI.
          You can add and name such shapes in 3D modeler, like Blender,
          and they will be automatically understood by the engine when loading level.

          Objects named CasSector<index>[_<ignored>] define sectors.
          The geometry of a sector with given <index> is set to be the sum
          of all CasSector<index>* boxes.
          Sectors are numbered from 0.

          Objects named CasWaypoint[<ignored>] define waypoints.
          Each waypoint is stored as a 3D point, this point is the middle of the
          bounding box of the object named CasWaypoint[<ignored>].

          After extracting from level 3D model,
          sectors and waypoints are then connected together by
          TSectorList.LinkToWaypoints. The idea is that you can go from
          one sector to the other through the waypoint that is placed on
          the border of both of them.)

        @item(@bold(Prepare everything possible for rendering and collision
          detection) to avoid later preparing things on-demand (which would cause
          unpleasant delay during gameplay).
          E.g. prepares octree and OpenGL resources.)
      )

      The overloaded version with a LevelName string searches the @link(Levels)
      list for a level with given name (and raises exception if it cannot
      be found). It makes sense if you filled the @link(Levels) list before,
      usually by @link(TLevelInfoList.LoadFromFiles Levels.LoadFromFiles)
      call. So you can easily define a level in your data with @code(name="xxx")
      in the @code(level.xml) file, and then you can load it
      by @code(LoadLevel('xxx')) call.

      It's important to note that @bold(you do not have to use
      this method to make a 3D game). You may as well just load the 3D scene
      yourself, and add things to TCastleSceneManager.Items and
      TCastleSceneManager.MainScene directly.
      This method is just a very comfortable way to set your 3D world in one call
      --- but it's not the only way.

      @groupBegin }
    procedure LoadLevel(const LevelName: string);
    procedure LoadLevel(const AInfo: TLevelInfo);
    { @groupEnd }

    { Level logic and state. }
    property Logic: TLevelLogic read FLogic;

    { Level information, independent from current level state. }
    property Info: TLevelInfo read FInfo;

    { Release everything loaded by LoadLevel, clearing the 3D world
      (only Player is left).

      You do not have to call this in normal circumstances,
      as each LoadLevel automatically clears previous 3D world. If fact,
      you should not call this in normal circumstances:
      calling this prevents the next LoadLevel to reuse resources
      that were needed by both old and new level.

      Call this only if you really want to conserve memory @italic(right now).
      Or when you want to force reload of resources at next LoadLevel
      call (for example, if you changed BakedAnimationSmoothness, it is useful
      --- otherwise the old animations will remain loaded with old BakedAnimationSmoothness
      setting). }
    procedure UnloadLevel;
  end;

  { Level logic. We use T3D descendant, since this is the comfortable
    way to add any behavior to the 3D world (it doesn't matter that
    "level logic" is not a usual 3D object --- it doesn't have to collide
    or be visible). }
  TLevelLogic = class(T3D)
  private
    FTime: TFloatTime;
  protected
    { Load 3D precalculated animation (from *.castle-anim-frames or *.md3 file), doing common tasks.
      @unorderedList(
        @item optionally creates triangle octree for the FirstScene and/or LastScene
        @item(call PrepareResources, with prRender, prBoundingBox, prShadowVolume
          (if shadow volumes possible at all in this OpenGL context))
        @item Free texture data, since they will not be needed anymore
        @item TimePlaying is by default @false, so the animation is not playing.
      )
      @groupBegin }
    function LoadLevelAnimation(const URL: string;
      const CreateFirstOctreeCollisions, CreateLastOctreeCollisions: boolean;
      const AnimationClass: TCastlePrecalculatedAnimationClass): TCastlePrecalculatedAnimation;
      deprecated 'whole TCastlePrecalculatedAnimation is deprecated, use TCastleScene for rendering all animations';
    function LoadLevelAnimation(const URL: string;
      const CreateFirstOctreeCollisions, CreateLastOctreeCollisions: boolean): TCastlePrecalculatedAnimation;
      deprecated 'whole TCastlePrecalculatedAnimation is deprecated, use TCastleScene for rendering all animations';
    { @groupEnd }

    { Load 3D scene from file, doing common tasks.
      @unorderedList(
        @item optionally create triangle octree
        @item(call PrepareResources, with prRender, prBoundingBox, prShadowVolume
          (if shadow volumes possible at all in this OpenGL context),)
        @item Free texture data, since they will not be needed anymore
      )
      @groupBegin }
    function LoadLevelScene(const URL: string;
      const PrepareForCollisions: boolean;
      const SceneClass: TCastleSceneClass): TCastleScene;
    function LoadLevelScene(const URL: string;
      const PrepareForCollisions: boolean): TCastleScene;
    { @groupEnd }

    { Handle a placeholder named in external modeler.
      Return @true if this is indeed a recognized placeholder name,
      and it was handled and relevant shape should be removed from level
      geometry (to not be rendered). }
    function Placeholder(const Shape: TShape; const PlaceholderName: string): boolean; virtual;

    { Called after all placeholders have been processed,
      that is after TGameSceneManager.LoadLevel placed initial creatures,
      items and other stuff on the level.
      Override it to do anything you want. }
    procedure PlaceholdersEnd; virtual;
  public
    { Create new level instance. Called before resources (creatures and items)
      are initialized (override PlaceholdersEnd if you need to do something
      after creatures and items are added).
      You can modify MainScene contents here.

      @param(AWorld

        3D world items. We provide AWorld instance at construction,
        and the created TLevelLogic instance will be added to this AWorld,
        and you cannot change it later. This is necessary,
        as TLevelLogic descendants at construction may actually modify your world,
        and depend on it later.)

      @param(DOMElement

        An XML tree of level.xml file. You can read it
        however you want, to handle additional attributes in level.xml.
        You can use standard FPC DOM unit and classes,
        and add a handful of simple comfortable routines in CastleXMLUtils unit,
        for example you can use this to read a string attribute:

        @longCode(#
          MyAttribute := DOMElement.AttributeStringDef('my_attribute', 'default value');
          MyRequiredAttribute := DOMElement.AttributeString('my_required_attribute');
        #)
      )
    }
    constructor Create(AOwner: TComponent; AWorld: T3DWorld;
      MainScene: TCastleScene; DOMElement: TDOMElement); reintroduce; virtual;
    function BoundingBox: TBox3D; override;

    { Called when new player starts new game on this level.
      This may be used to equip the player with some basic weapon / items.

      This is never called or used by the engine itself.
      This does nothing in the default TLevelLogic class implementation.

      Your particular game, where you can best decide when the player
      "starts a new game" and when the player merely "continues the previous
      game", may call it. And you may override this in your TLevelLogic descendants
      to equip the player. }
    procedure PrepareNewPlayer(NewPlayer: TPlayer); virtual;

    { Time of the level, in seconds. Time 0 when level is created.
      This is updated in our @link(Update). }
    property Time: TFloatTime read FTime;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

  TLevelLogicClasses = specialize TFPGMap<string, TLevelLogicClass>;

function LevelLogicClasses: TLevelLogicClasses;

{ All known levels. You can use this to show a list of available levels to user.
  You can also search it and use TGameSceneManager.LoadLevel to load
  a given TLevelInfo instance. }
function Levels: TLevelInfoList;

implementation

uses SysUtils, CastleGLUtils, CastleFilesUtils, CastleStringUtils,
  CastleGLImages, CastleUIControls, XMLRead, CastleInputs, CastleXMLUtils,
  CastleRenderingCamera, Math, CastleLog, X3DCameraUtils,
  CastleGLVersion, CastleURIUtils, CastleDownload;

{ globals -------------------------------------------------------------------- }

var
  FLevelLogicClasses: TLevelLogicClasses;

function LevelLogicClasses: TLevelLogicClasses;
begin
  if FLevelLogicClasses = nil then
  begin
    FLevelLogicClasses := TLevelLogicClasses.Create;
    FLevelLogicClasses['Level'] := TLevelLogic;
  end;
  Result := FLevelLogicClasses;
end;

var
  { Created in initialization of this unit, destroyed in finalization
    (or when the last TGameSceneManager referring to TLevelInfo is destroyed).
    Owns it's Items. }
  FLevels: TLevelInfoList;

function Levels: TLevelInfoList;
begin
  Result := FLevels;
end;

{ TGameSceneManager ---------------------------------------------------------- }

function TGameSceneManager.Placeholder(Shape: TShape;
  PlaceholderName: string): boolean;
const
  { Prefix of all placeholders that we seek on 3D models. }
  PlaceholderPrefix = 'Cas';
  ResourcePrefix = PlaceholderPrefix + 'Res';
  MoveLimitName = PlaceholderPrefix + 'MoveLimit';
  WaterName = PlaceholderPrefix + 'Water';
  SectorPrefix = PlaceholderPrefix + 'Sector';
  WaypointPrefix = PlaceholderPrefix + 'Waypoint';

  procedure PlaceholderResource(Shape: TShape; PlaceholderName: string);
  var
    ResourceName: string;
    ResourceNumberPresent: boolean;
    Resource: T3DResource;
    Box: TBox3D;
    Position, Direction: TVector3Single;
    IgnoredBegin, NumberBegin: Integer;
    ResourceNumber: Int64;
  begin
    { PlaceholderName is now <resource_name>[<resource_number>][_<ignored>] }

    { cut off optional [_<ignored>] suffix }
    IgnoredBegin := Pos('_', PlaceholderName);
    if IgnoredBegin <> 0 then
      PlaceholderName := Copy(PlaceholderName, 1, IgnoredBegin - 1);

    { calculate ResourceName, ResourceNumber, ResourceNumberPresent }
    NumberBegin := CharsPos(['0'..'9'], PlaceholderName);
    ResourceNumberPresent := NumberBegin <> 0;
    if ResourceNumberPresent then
    begin
      ResourceName := Copy(PlaceholderName, 1, NumberBegin - 1);
      ResourceNumber := StrToInt(SEnding(PlaceholderName, NumberBegin));
    end else
    begin
      ResourceName := PlaceholderName;
      ResourceNumber := 0;
    end;

    Resource := Resources.FindName(ResourceName);
    if not Resource.Prepared then
      WritelnWarning('Resource', 'Resource "%s" is initially present on the level, but was not prepared yet --- which probably means you did not add it to <resources> inside level level.xml file. This causes loading on-demand, which is less comfortable for player.',
        [Resource.Name]);

    Box := Shape.BoundingBox;
    Position := Box.Center;
    Position[Items.GravityCoordinate] := Box.Data[0, Items.GravityCoordinate];

    Direction := Info.PlaceholderReferenceDirection;
    Direction := MatrixMultDirection(Shape.State.Transform, Direction);

    Resource.InstantiatePlaceholder(Items, Position, Direction,
      ResourceNumberPresent, ResourceNumber);
  end;

  { Shapes placed under the name CasWaypoint[_<ignored>]
    are removed from the Scene, and are added as new waypoint.
    Waypoint's Position is set to the middle point of shape's bounding box. }
  procedure PlaceholderWaypoint(Shape: TShape);
  var
    Waypoint: TWaypoint;
  begin
    Waypoint := TWaypoint.Create;
    Waypoint.Box := Shape.BoundingBox;
    Waypoint.Position := Waypoint.Box.Center;
    Waypoints.Add(Waypoint);

    { Tests:
    Writeln('Waypoint ', Waypoints.Count - 1, ': at position ',
      VectorToNiceStr(Waypoint.Position));}
  end;

  { Shapes placed under the name CasSector<index>[_<ignored>]
    are removed from the Scene, and are added to sector <index> BoundingBoxes.

    Count of the Sectors list is enlarged, if necessary,
    to include all sectors indicated in the Scene. }
  procedure PlaceholderSector(Shape: TShape; const SectorNodeName: string);
  var
    IgnoredBegin, SectorIndex: Integer;
  begin
    { Calculate SectorIndex }
    IgnoredBegin := Pos('_', SectorNodeName);
    if IgnoredBegin = 0 then
      SectorIndex := StrToInt(SectorNodeName) else
      SectorIndex := StrToInt(Copy(SectorNodeName, 1, IgnoredBegin - 1));

    Sectors.Count := Max(Sectors.Count, SectorIndex + 1);
    if Sectors[SectorIndex] = nil then
      Sectors[SectorIndex] := TSector.Create;

    Sectors[SectorIndex].Boxes.Add(Shape.BoundingBox);

    { Tests:
    Writeln('Sector ', SectorIndex, ': added box ',
      SectorBoundingBox.ToNiceStr); }
  end;

begin
  Result := true;
  if IsPrefix(ResourcePrefix, PlaceholderName) then
    PlaceholderResource(Shape, SEnding(PlaceholderName, Length(ResourcePrefix) + 1)) else
  if PlaceholderName = MoveLimitName then
    MoveLimit := Shape.BoundingBox else
  if PlaceholderName = WaterName then
    Water := Shape.BoundingBox else
  if IsPrefix(SectorPrefix, PlaceholderName) then
    PlaceholderSector(Shape, SEnding(PlaceholderName, Length(SectorPrefix) + 1)) else
  if IsPrefix(WaypointPrefix, PlaceholderName) then
    PlaceholderWaypoint(Shape) else
    Result := Logic.Placeholder(Shape, PlaceholderName);
end;

function TGameSceneManager.UnloadLevelCore: T3DResourceList;
var
  I: Integer;
begin
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
  FLogic := nil; { we freed FLogic above, since it was on Items list }

  { save PreviousResources, before Info is overridden with new level.
    This allows us to keep PreviousResources while new resources are required,
    and this means that resources already loaded for previous level
    don't need to be reloaded for new. }
  Result := T3DResourceList.Create(false);
  if Info <> nil then
  begin
    Result.Assign(Info.LevelResources);
    Dec(Levels.References);
    FInfo := nil;
  end;
end;

procedure TGameSceneManager.UnloadLevel;
var
  PreviousResources: T3DResourceList;
begin
  PreviousResources := UnloadLevelCore;
  PreviousResources.Release;
  FreeAndNil(PreviousResources);
end;

procedure TGameSceneManager.LoadLevelCore(const AInfo: TLevelInfo);
var
  { Sometimes it's not comfortable
    to remove the items while traversing --- so we will instead
    put them on this list.

    Be careful: never add here two nodes such that one may be parent
    of another, otherwise freeing one could free the other one too
    early. }
  ItemsToRemove: TX3DNodeList;

  procedure TraverseForPlaceholders(Shape: TShape);
  var
    PlaceholderName: string;
  begin
    PlaceholderName := Info.PlaceholderName(Shape);
    if (PlaceholderName <> '') and Placeholder(Shape, PlaceholderName) then
    begin
      { Don't remove OriginalGeometry node now --- will be removed later.
        This avoids problems with removing nodes while traversing. }
      if ItemsToRemove.IndexOf(Shape.OriginalGeometry) = -1 then
        ItemsToRemove.Add(Shape.OriginalGeometry);
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

  { After placeholders are processed, finish some stuff. }
  procedure PlaceholdersEnd;
  var
    NewMoveLimit: TBox3D;
  begin
    if MoveLimit.IsEmpty then
    begin
      { Set MoveLimit to MainScene.BoundingBox, and make maximum up larger. }
      NewMoveLimit := MainScene.BoundingBox;
      NewMoveLimit.Data[1, Items.GravityCoordinate] +=
        4 * (NewMoveLimit.Data[1, Items.GravityCoordinate] -
             NewMoveLimit.Data[0, Items.GravityCoordinate]);
      MoveLimit := NewMoveLimit;
    end;

    Sectors.LinkToWaypoints(Waypoints);

    Logic.PlaceholdersEnd;
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
    if MainScene.ViewpointStack.Top <> nil then
      MainScene.ViewpointStack.Top.GetView(InitialPosition,
        InitialDirection, InitialUp, GravityUp) else
    begin
      InitialPosition := DefaultX3DCameraPosition[cvVrml2_X3d];
      InitialDirection := DefaultX3DCameraDirection;
      InitialUp := DefaultX3DCameraUp;
      GravityUp := DefaultX3DGravityUp;
    end;

    NavigationNode := MainScene.NavigationInfoStack.Top;

    if (NavigationNode <> nil) and (NavigationNode.FdAvatarSize.Count >= 1) then
      CameraRadius := NavigationNode.FdAvatarSize.Items[0] else
      CameraRadius := MainScene.BoundingBox.AverageSize(false, 1) * 0.007;

    if (NavigationNode <> nil) and (NavigationNode.FdAvatarSize.Count >= 2) then
      PreferredHeight := NavigationNode.FdAvatarSize.Items[1] else
      PreferredHeight := CameraRadius * 5;
    CorrectPreferredHeight(PreferredHeight, CameraRadius,
      TWalkCamera.DefaultCrouchHeight, TWalkCamera.DefaultHeadBobbing);

    if Player <> nil then
      WalkCamera := Player.Camera else
      { If you don't initialize Player (like for castle1 background level
        or castle-view-level or lets_take_a_walk) then just create a camera. }
      WalkCamera := TWalkCamera.Create(Self);

    { initialize some navigation settings of player }
    if Player <> nil then
    begin
      Player.DefaultPreferredHeight := PreferredHeight;
      if NavigationNode <> nil then
        Player.DefaultMoveHorizontalSpeed := NavigationNode.FdSpeed.Value else
        Player.DefaultMoveHorizontalSpeed := 1.0;
      Player.DefaultMoveVerticalSpeed := 20;
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

    Camera := WalkCamera;

    WalkCamera.Init(InitialPosition, InitialDirection,
      InitialUp, GravityUp, PreferredHeight, CameraRadius);
    WalkCamera.CancelFalling;
  end;

var
  Options: TPrepareResourcesOptions;
  SI: TShapeTreeIterator;
  PreviousResources: T3DResourceList;
begin
  { We want OpenGL context, but we don't want to require that this scene manager
    is actually added to Window.Controls yet. This would prevent taking a screenshot
    from previous 3D contents as a background when loading level, which is actually
    used by castle1.
    So we do not check field "not GLInitialized", instead we look at global
    GLVersion. }
  if GLVersion = nil then
    raise Exception.Create('OpenGL context is not initialized yet. You have to initialize OpenGL (for example by calling TCastleWindow.Open, or by waiting for TCastleControl.OnGLContextOpen) before using TGameSceneManager.LoadLevel.');

  PreviousResources := UnloadLevelCore;

  FInfo := AInfo;
  Inc(Levels.References);
  Info.Played := true;
  LevelResourcesPrepared := false;

  Progress.Init(1, 'Loading level "' + Info.Title + '"');
  try
    { disconnect previous Camera from SceneManager.
      Otherwise, it would be updated by MainScene loading binding new
      NavigationInfo (with it's speed) and Viewpoint.
      We prefer to do it ourselves in InitializeCamera. }
    Camera := nil;

    MainScene := TCastleScene.Create(Self);
    Inc(MainScene.InternalDirty);
    MainScene.Load(Info.SceneURL);

    { Scene must be the first one on Items, this way Items.MoveCollision will
      use Scene for wall-sliding (see T3DList.MoveCollision implementation). }
    Items.Insert(0, MainScene);

    InitializeCamera;

    Progress.Step;
  finally
    Progress.Fini;
  end;

  { load new resources (and release old unused). This must be done after
    InitializeCamera (because it uses GravityUp), which in turn must
    be after loading MainScene (because initial camera looks at MainScene
    contents).
    It will show it's own progress bar. }
  Info.LevelResources.Prepare(BaseLights, GravityUp);
  LevelResourcesPrepared := true;
  PreviousResources.Release;
  FreeAndNil(PreviousResources);

  Progress.Init(1, 'Loading level "' + Info.Title + '"');
  try
    { create new Logic }
    FLogic := Info.LogicClass.Create(Self, Items, MainScene, Info.Element);
    Items.Add(Logic);

    { We will calculate new Sectors and Waypoints and other stuff
      based on placeholders. Initialize them now to be empty. }
    FreeAndNil(FSectors);
    FreeAndNil(Waypoints);
    FSectors := TSectorList.Create(true);
    Waypoints := TWaypointList.Create(true);
    MoveLimit := EmptyBox3D;
    Water := EmptyBox3D;

    ItemsToRemove := TX3DNodeList.Create(false);
    try
      SI := TShapeTreeIterator.Create(MainScene.Shapes, { OnlyActive } true);
      try
        while SI.GetNext do TraverseForPlaceholders(SI.Current);
      finally SysUtils.FreeAndNil(SI) end;
      RemoveItemsToRemove;
    finally ItemsToRemove.Free end;

    PlaceholdersEnd;

    { calculate Options for PrepareResources }
    Options := [prRender, prBackground, prBoundingBox];
    if (GLFeatures <> nil) and GLFeatures.ShadowVolumesPossible then
      Include(Options, prShadowVolume);

    MainScene.PrepareResources(Options, false, BaseLights);

    MainScene.FreeResources([frTextureDataInNodes]);

    Progress.Step;
  finally
    Progress.Fini;
  end;

  { Loading octree have their own Progress, so we load them outside our
    progress. }
  MainScene.TriangleOctreeProgressTitle := 'Loading level (triangle octree)';
  MainScene.ShapeOctreeProgressTitle := 'Loading level (Shape octree)';
  MainScene.Spatial := [ssRendering, ssDynamicCollisions];
  MainScene.PrepareResources([prSpatial], false, BaseLights);

  if (Player <> nil) then
    Player.LevelChanged;

  SoundEngine.MusicPlayer.Sound := Info.MusicSound;

  { Initialize SoundEngine if we're sure that this game uses sounds.
    Doing this now is good to avoid making a delay later in the middle
    of the game when 1st sound is playing. }
  if (not SoundEngine.ALInitialized) and (SoundEngine.Sounds.Count > 1) then
    SoundEngine.ALContextOpen;

  MainScene.ProcessEvents := true;

  Dec(MainScene.InternalDirty);
end;

procedure TGameSceneManager.LoadLevel(const AInfo: TLevelInfo);
var
  SavedImage: TObject;
  SavedBarYPosition: Single;
  SavedOwnsImage: boolean;
begin
  SavedOwnsImage := Progress.UserInterface.OwnsImage;
  SavedImage := Progress.UserInterface.Image;
  SavedBarYPosition := Progress.UserInterface.BarYPosition;
  try
    Progress.UserInterface.OwnsImage := false; // never free Image at next assignment
    Progress.UserInterface.Image := AInfo.LoadingImage;
    Progress.UserInterface.BarYPosition := AInfo.LoadingBarYPosition;
    LoadLevelCore(AInfo);
  finally
    Progress.UserInterface.Image := nil; // unassign AInfo.LoadingImage, while OwnsImage = false
    Progress.UserInterface.OwnsImage := SavedOwnsImage;
    Progress.UserInterface.Image := SavedImage;
    Progress.UserInterface.BarYPosition := SavedBarYPosition;
  end;
end;

procedure TGameSceneManager.LoadLevel(const LevelName: string);
begin
  LoadLevel(Levels.FindName(LevelName));
end;

destructor TGameSceneManager.Destroy;
begin
  if Info <> nil then
  begin
    { we check LevelResourcesPrepared, to avoid calling
      Info.LevelResources.Release when Info.LevelResources.Prepare
      was not called (which may happen if there was an exception if LoadLevelCore
      at MainScene.Load(SceneURL). }
    if (Info.LevelResources <> nil) and LevelResourcesPrepared then
      Info.LevelResources.Release;

    Dec(FLevels.References);
    if FLevels.References = 0 then
      FreeAndNil(FLevels);
  end;

  inherited;
end;

{ TLevelLogic ---------------------------------------------------------------- }

constructor TLevelLogic.Create(AOwner: TComponent; AWorld: T3DWorld;
  MainScene: TCastleScene; DOMElement: TDOMElement);
begin
  inherited Create(AOwner);
  SetWorld(AWorld);
  Assert(AWorld <> nil, 'TLevelLogic.World should never be nil, you have to provide World at TLevelLogic constructor');
  { Actually, the fact that our BoundingBox is empty also prevents collisions.
    But for some methods, knowing that Collides = false allows them to exit
    faster. }
  Collides := false;
end;

function TLevelLogic.BoundingBox: TBox3D;
begin
  { This object is invisible and non-colliding. }
  Result := EmptyBox3D;
end;

procedure TLevelLogic.PrepareNewPlayer(NewPlayer: TPlayer);
begin
  { Nothing to do in this class. }
end;

function TLevelLogic.LoadLevelScene(
  const URL: string;
  const PrepareForCollisions: boolean;
  const SceneClass: TCastleSceneClass): TCastleScene;
var
  Options: TPrepareResourcesOptions;
begin
  Result := SceneClass.Create(Self);
  Result.Load(URL);

  { calculate Options for PrepareResources }
  Options := [prRender, prBoundingBox { always needed }];
  if (GLFeatures <> nil) and GLFeatures.ShadowVolumesPossible then
    Include(Options, prShadowVolume);

  Result.PrepareResources(Options, false, World.BaseLights);

  if PrepareForCollisions then
    Result.Spatial := [ssDynamicCollisions];

  Result.FreeResources([frTextureDataInNodes]);

  Result.ProcessEvents := true;
end;

function TLevelLogic.LoadLevelScene(
  const URL: string;
  const PrepareForCollisions: boolean): TCastleScene;
begin
  Result := LoadLevelScene(URL, PrepareForCollisions, TCastleScene);
end;

function TLevelLogic.LoadLevelAnimation(
  const URL: string;
  const CreateFirstOctreeCollisions, CreateLastOctreeCollisions: boolean;
  const AnimationClass: TCastlePrecalculatedAnimationClass): TCastlePrecalculatedAnimation;
var
  Options: TPrepareResourcesOptions;
begin
  Result := AnimationClass.Create(Self);
  Result.LoadFromFile(URL, false, true);

  { calculate Options for PrepareResources }
  Options := [prRender, prBoundingBox { always needed }];
  if (GLFeatures <> nil) and GLFeatures.ShadowVolumesPossible then
    Include(Options, prShadowVolume);

  Result.PrepareResources(Options, false, World.BaseLights);

  if CreateFirstOctreeCollisions then
    Result.FirstScene.Spatial := [ssDynamicCollisions];

  if CreateLastOctreeCollisions then
    Result.LastScene.Spatial := [ssDynamicCollisions];

  Result.FreeResources([frTextureDataInNodes]);

  Result.TimePlaying := false;
end;

function TLevelLogic.LoadLevelAnimation(
  const URL: string;
  const CreateFirstOctreeCollisions, CreateLastOctreeCollisions: boolean): TCastlePrecalculatedAnimation;
begin
  {$warnings off}
  { knowingly using deprecated function in another deprecated function }
  Result := LoadLevelAnimation(URL,
    CreateFirstOctreeCollisions, CreateLastOctreeCollisions,
    TCastlePrecalculatedAnimation);
  {$warnings on}
end;

procedure TLevelLogic.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  FTime += SecondsPassed;
end;

function TLevelLogic.Placeholder(const Shape: TShape;
  const PlaceholderName: string): boolean;
begin
  Result := false;
end;

procedure TLevelLogic.PlaceholdersEnd;
begin
  { Nothing to do in this class. }
end;

{ TLevelInfo ------------------------------------------------------------ }

constructor TLevelInfo.Create;
begin
  inherited;
  LevelResources := T3DResourceList.Create(false);
end;

destructor TLevelInfo.Destroy;
begin
  FreeAndNil(Document);
  FreeAndNil(LevelResources);
  FreeAndNil(FLoadingImage);
  inherited;
end;

procedure TLevelInfo.LoadFromDocument;

  procedure MissingRequiredAttribute(const AttrName: string);
  begin
    raise Exception.CreateFmt(
      'Missing required attribute "%s" of <level> element', [AttrName]);
  end;

  { Like DOMGetAttribute, but reads TLevelLogicClass value. }
  function DOMGetLevelLogicClassAttribute(const Element: TDOMElement;
    const AttrName: string; var Value: TLevelLogicClass): boolean;
  var
    ValueStr: string;
    LevelClassIndex: Integer;
  begin
    Result := Element.AttributeString(AttrName, ValueStr);
    LevelClassIndex := LevelLogicClasses.IndexOf(ValueStr);
    if LevelClassIndex <> -1 then
      Value := LevelLogicClasses.Data[LevelClassIndex] else
      raise Exception.CreateFmt('Unknown level type "%s"', [ValueStr]);
  end;

  { Add all resources with AlwaysPrepared = true to the LevelResources. }
  procedure AddAlwaysPreparedResources;
  var
    I: Integer;
  begin
    for I := 0 to Resources.Count - 1 do
      if Resources[I].AlwaysPrepared and
         (LevelResources.IndexOf(Resources[I]) = -1) then
      LevelResources.Add(Resources[I]);
  end;

const
  DefaultPlaceholderReferenceDirection: TVector3Single = (1, 0, 0);
var
  LoadingImageURL: string;
  SoundName: string;
  PlaceholdersKey: string;
  S: string;
begin
  Element := Document.DocumentElement;

  if Element.TagName <> 'level' then
    raise Exception.CreateFmt('Root node of level.xml file must be <level>, but is "%s", in "%s"',
      [Element.TagName, DocumentBaseURL]);

  { Required atttributes }

  if not Element.AttributeString('name', FName) then
    MissingRequiredAttribute('name');

  if not Element.AttributeString('scene', FSceneURL) then
    MissingRequiredAttribute('scene');
  SceneURL := CombineURI(DocumentBaseURL, SceneURL);

  if not Element.AttributeString('title', FTitle) then
    MissingRequiredAttribute('title');

  { Optional attributes }

  if not Element.AttributeInteger('number', FNumber) then
    Number := 0;

  if not Element.AttributeBoolean('demo', FDemo) then
    Demo := false;

  if not Element.AttributeString('title_hint', FTitleHint) then
    TitleHint := '';

  if not Element.AttributeBoolean('default_played', FDefaultPlayed) then
    DefaultPlayed := false;

  if not DOMGetLevelLogicClassAttribute(Element, 'type', FLogicClass) then
    LogicClass := TLevelLogic;

  PlaceholderName := PlaceholderNames['x3dshape'];
  if Element.AttributeString('placeholders', PlaceholdersKey) then
    PlaceholderName := PlaceholderNames[PlaceholdersKey];

  FreeAndNil(FLoadingImage); { make sure LoadingImage is clear first }
  if Element.AttributeString('loading_image', LoadingImageURL) then
  begin
    LoadingImageURL := CombineURI(DocumentBaseURL, LoadingImageURL);
    LoadingImage := LoadImage(LoadingImageURL, [TRGBImage]) as TRGBImage;
  end;

  if (not Element.AttributeSingle('loading_bar_y_position',
       FLoadingBarYPosition)) and
     { handle loading_image_bar_y_position for backward compatibility }
     (not Element.AttributeSingle('loading_image_bar_y_position',
       FLoadingBarYPosition)) then
    LoadingBarYPosition := TProgressUserInterface.DefaultBarYPosition;

  if Element.AttributeString('placeholder_reference_direction', S) then
    PlaceholderReferenceDirection := Vector3SingleFromStr(S) else
    PlaceholderReferenceDirection := DefaultPlaceholderReferenceDirection;

  LevelResources.LoadResources(Element);
  AddAlwaysPreparedResources;

  if Element.AttributeString('music_sound', SoundName) then
    MusicSound := SoundEngine.SoundFromName(SoundName) else
    MusicSound := stNone;
end;

{ TLevelInfoList ------------------------------------------------------- }

function TLevelInfoList.FindName(const AName: string): TLevelInfo;
var
  I: Integer;
  S: string;
begin
  for I := 0 to Count - 1 do
    if Items[I].Name = AName then
      Exit(Items[I]);

  S := Format('Level name "%s" is not found on the Levels list', [AName]);
  if Count = 0 then
    S += '.' + NL + NL + 'Warning: there are no levels available on the list at all. This means that the game data was not correctly installed (as we did not find any level.xml files defining any levels). Or the developer forgot to call Levels.LoadFromFiles.';
  raise Exception.Create(S);
end;

function IsSmallerByNumber(const A, B: TLevelInfo): Integer;
begin
  Result := A.Number - B.Number;
end;

procedure TLevelInfoList.LoadFromConfig(const Config: TCastleConfig);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Played := Config.GetValue(
      'levels_available/' + Items[I].Name + '/played',
      Items[I].DefaultPlayed);
end;

procedure TLevelInfoList.SaveToConfig(const Config: TCastleConfig);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Config.SetDeleteValue(
      'levels_available/' + Items[I].Name + '/played',
      Items[I].Played,
      Items[I].DefaultPlayed);
end;

procedure TLevelInfoList.AddFromInfo(const Info: TFileInfo; var StopSearch: boolean);
begin
  AddFromFile(Info.URL);
end;

procedure TLevelInfoList.AddFromFile(const URL: string);
var
  NewLevelInfo: TLevelInfo;
begin
  NewLevelInfo := TLevelInfo.Create;
  Add(NewLevelInfo);
  NewLevelInfo.Played := false;

  URLReadXML(NewLevelInfo.Document, URL);
  NewLevelInfo.DocumentBaseURL := URL;
  NewLevelInfo.LoadFromDocument;
end;

procedure TLevelInfoList.LoadFromFiles(const LevelsPath: string);
begin
  FindFiles(LevelsPath, 'level.xml', false, @AddFromInfo, [ffRecursive]);
end;

procedure TLevelInfoList.LoadFromFiles;
begin
  LoadFromFiles(ApplicationData(''));
  SortByNumber;
end;

procedure TLevelInfoList.SortByNumber;
begin
  Sort(@IsSmallerByNumber);
end;

{ initialization / finalization ---------------------------------------------- }

initialization
  FLevels := TLevelInfoList.Create(true);
  Inc(FLevels.References);
finalization
  FreeAndNil(FLevelLogicClasses);
  { there may still exist TGameSceneManager instances that refer to our
    TLevelInfo instances. So we don't always free Levels below. }
  if FLevels <> nil then
  begin
    Dec(FLevels.References);
    if FLevels.References = 0 then
      FreeAndNil(FLevels);
  end;
end.
