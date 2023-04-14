{
  Copyright 2006-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Loading of typical 3D game level with placeholders (TLevel),
  management of available game levels (TLevelInfo, @link(Levels)). }
unit CastleLevels
  deprecated 'design levels as a collection of TCastleTransform descendans, like TCastleScene, without resource.xml necessary';

{$I castleconf.inc}

interface

{$warnings off} // using deprecated in deprecated unit

uses Classes, DOM, Generics.Collections,
  CastleVectors, CastleSceneCore, CastleScene, CastleBoxes, X3DNodes,
  X3DFields, CastleCameras, CastleSectors, CastleUtils, CastleClassUtils,
  CastlePlayer, CastleResources, CastleProgress, CastleInputs,
  CastleSoundEngine, CastleTransform, CastleShapes, CastleXMLConfig, CastleImages,
  CastleTimeUtils, CastleViewport, CastleFindFiles, CastleKeysMouse;

{$warnings on}

type
  TLevelLogic = class;
  TLevelLogicClass = class of TLevelLogic;

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
    FPlaceholderReferenceDirection: TVector3;
    FMusicSound: TCastleSound;
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
      as @link(TLevel.Load) parameter and such.

      For all (current and future) uses it should be a valid VRML/X3D
      and ObjectPascal identifier, so use only (English) letters,
      underscores and digits (and don't start with a digit). }
    property Name: string read FName write FName;

    { Main level 3D model. When the level is loaded, this scene will be set
      as TCastleRootTransform.MainScene,
      so it determines the default viewpoint, background and such.

      Usually it also contains the most (if not all) of the visible level geometry,
      scripts and such. Although level logic (TLevelLogic descendant determined
      by LevelClass) may also add any number of additional objects
      (TCastleTransform instances) to the world. }
    property SceneURL: string read FSceneURL write FSceneURL;

    {$ifdef FPC}
    { @deprecated Deprecated name for SceneURL. }
    property SceneFileName: string read FSceneURL write FSceneURL; deprecated;
    {$endif}

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
      {$ifdef FPC}default TProgressUserInterface.DefaultBarYPosition{$endif};

    { Placeholder detection method. See TPlaceholderName, and see
      @link(TLevel.Load) for a description when we use placeholders.
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
          see "Which way is up?" on https://castle-engine.io/manual_up.php .))
      )

      In Blender it's useful to enable the "Display -> Wire" option for placeholder
      objects, then Blender will show arrows inside the placeholder.
      +X of the arrow determines the default direction understood by our engine. }
    property PlaceholderReferenceDirection: TVector3
      read FPlaceholderReferenceDirection write FPlaceholderReferenceDirection;

    { Music played when entering the level.
      None (nil) by default. }
    property MusicSound: TCastleSound read FMusicSound write FMusicSound;
  end;

  TLevelInfoList = class({$ifdef FPC}specialize{$endif} TObjectList<TLevelInfo>)
  private
    { How many TLevel have references to our children by
      TLevel.Info? }
    References: Cardinal;
    procedure AddFromInfo(const Info: TFileInfo; var StopSearch: boolean);
  public
    { raises Exception if such Name is not on the list. }
    function FindName(const AName: string): TLevelInfo;

    { Add all available levels found by scanning for level.xml inside data
      directory.
      Overloaded version without parameter just looks inside 'castle-data:/'.
      For the specification of level.xml format see
      [https://castle-engine.io/creating_data_levels.php] .

      This should be called after resources (creatures and items) are known,
      as they may be referenced by level.xml files.
      So call @link(T3DResourceList.LoadFromFiles Resources.LoadFromFiles)
      @italic(before) calling this (if you
      want to use any creatures / items at all, of course).

      All TLevelInfo.Played values are initially set to @false.
      You must call LoadFromConfig @italic(after) calling this
      to read TLevelInfo.Played values from user preferences file.

      @groupBegin }
    procedure LoadFromFiles(const LevelsPath: string); overload;
    procedure LoadFromFiles; overload;
    { @groupEnd }

    { Add a single level information from the XML file at given location.
      The given XML file must have <level> root element and be written
      according to
      https://castle-engine.io/creating_data_levels.php .  }
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

  { Manage (load and unload) game level.

    For basic usage, assign @link(Viewport) and call @link(Load).
    It is similar to creating regular TCastleScene and adding it to
    @code(Viewport.Items), but has some extra features. Most notably,
    it will instantiate also creatures and items looking at special
    "placeholders" in the model file. }
  TLevel = class(TAbstractLevel)
  strict private
    type
      TLevelInternalLogic = class(TCastleTransform)
        Level: TLevel;
        procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
      end;
    var
      FViewport: TCastleViewport;
      FInternalLogic: TLevelInternalLogic;
      FCreaturesRoot, FItemsRoot: TCastleTransform;
      FLogic: TLevelLogic;
      FInfo: TLevelInfo;
      LevelResourcesPrepared: boolean;
      FWater: TBox3D;
      FSectors: TSectorList;
      Waypoints: TWaypointList;
      FPlayer: TPlayer;
      FPlayerSwimming: TPlayerSwimming;
      SickProjectionTime: TFloatTime;
      FFreeAtUnload: TComponent;
    { Like TLevel.Load, but doesn't care about AInfo.LoadingImage. }
    procedure LoadCore(const AInfo: TLevelInfo);
    { Unload Items from previous level, keeps only Player on Items.
      Returns previous resources. You have to call Release and free them. }
    function UnloadCore: T3DResourceList;
    function Placeholder(Shape: TShape; PlaceholderName: string): boolean;
    procedure SetPlayer(const Value: TPlayer);
    { Assigns Camera and Navigation on level loading and setting/change player }
    procedure InitializeCamera;
    function Items: TCastleRootTransform;
    procedure Update(const SecondsPassed: Single);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetPlayer: TCastleTransform; override;
    function GetSectors: TSectorList; override;
    function RootTransform: TCastleRootTransform; override;
    function CreaturesRoot: TCastleTransform; override;
    function ItemsRoot: TCastleTransform; override;
    function PrepareParams: TPrepareParams; override;
    function FreeAtUnload: TComponent; override;

    { Viewport whose contents will be adjusted to show given level.
      Must be assigned before @link(Load). }
    property Viewport: TCastleViewport read FViewport write FViewport;

    { Load game level.

      @unorderedList(
        @item(@bold(Set Viewport.Items):

          Clear @link(TCastleViewport.Items),
          clear @link(TCastleViewport.Navigation Navigation),
          clear @link(TCastleRootTransform.MainScene) as well.
          Then load a new main scene and set navigation, adding to
          @link(TCastleViewport.Items) all resources (creatures and items)
          defined by placeholders named CasRes* in the main level file.)

        @item(@bold(Make sure resources are ready:)

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
          unpleasant delay during gameplay).)
      )

      The overloaded version with a LevelName string searches the @link(Levels)
      list for a level with given name (and raises exception if it cannot
      be found). It makes sense if you filled the @link(Levels) list before,
      usually by @link(TLevelInfoList.LoadFromFiles Levels.LoadFromFiles)
      call. So you can easily define a level in your data with @code(name="xxx")
      in the @code(level.xml) file, and then you can load it
      by @code(Load('xxx')) call.

      It's important to note that @bold(you do not have to use
      this method to make a 3D game). You may as well just load the 3D scene
      yourself, and add things to TCastleViewport.Items and
      TCastleRootTransform.MainScene directly.
      This method is just a comfortable way to set a world,
      with creatures and items, using placeholders.

      @groupBegin }
    procedure Load(const LevelName: string); overload;
    procedure Load(const AInfo: TLevelInfo); overload;
    { @groupEnd }

    { Level logic and state. }
    property Logic: TLevelLogic read FLogic;

    { Level information, independent from current level state. }
    property Info: TLevelInfo read FInfo;

    { Release everything loaded by @link(Load), clearing the 3D world.

      You do not have to call this in normal circumstances,
      as each @link(Load) automatically clears previous 3D world. If fact,
      you should not call this in normal circumstances:
      calling this prevents the next Load to reuse resources
      that were needed by both old and new level.

      Call this only if you really want to conserve memory @italic(right now).
      Or when you want to force reload of resources at next Load
      call (for example, if you changed BakedAnimationSmoothness, it is useful
      --- otherwise the old animations will remain loaded with old BakedAnimationSmoothness
      setting). }
    procedure Unload;

    { Sectors and waypoints of this world, for AI in 3D.
      Initialized by @link(Load).
      @nil if you never call @link(Load). }
    property Sectors: TSectorList read FSectors;

    { Water volume in the scene. It may be used by various 3D objects
      to indicate appropriate behavior --- some things swim,
      some things drown and such. For now, this is only used by TPlayer
      class to detect swimming (and make appropriate sounds, special rendering,
      drowning and such).

      For now, this is just a simple TBox3D. It will
      be extended to represent a set of flexible 3D volumes in the future.

      Empty initially. Initialize it however you want. }
    property Water: TBox3D read FWater write FWater;

    { Player in this 3D world. This serves various purposes:

      @unorderedList(
        @item(This object never collides with the navigation.
          See @link(TCastleViewport.AvoidNavigationCollisions).)

        @item(
          Player transformation will be automatically synchronized
          with current @link(TCastleViewport.Camera) position.
          As such, TPlayer represents the volume of player in 1st person games.)

        @item(For AI in CastleCreatures, hostile creatures will attack
          this player. So this determines the target position that
          creatures try to reach, where they shoot missiles etc.
          More advanced AI, with friendlies/companions, or cooperating
          factions of creatures, may have other mechanisms to determine who
          wants to attack who.)

        @item(For items on level in CastleItems, this player will pick up the items
          lying on the ground, and will be able to equip weapons.
          This functionality may be generalized in the future, to allow
          anyone to pick up and carry and equip items.)
      )
    }
    property Player: TPlayer read FPlayer write SetPlayer;
  end;

  TLevelProperties = TAbstractLevel deprecated 'use TAbstractLevel';

  { Viewport that can comfortably load and manage a 3D game level.
    It really adds only one new method to TCastleSceneManager:
    @link(LoadLevel), see it's documentation to know what it gives you.
    It also exposes @link(Logic) and @link(Info) properties
    corresponding to the currently loaded level.

    @deprecated @exclude Use @link(TLevel) instead. }
  TGameSceneManager = class(TCastleSceneManager)
  strict private
    FLevel: TLevel;
    function GetSectors: TSectorList;
    function GetWater: TBox3D;
    procedure SetWater(const Value: TBox3D);
    function GetPlayer: TPlayer;
    procedure SetPlayer(const Value: TPlayer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadLevel(const LevelName: string); overload;
    procedure LoadLevel(const AInfo: TLevelInfo); overload;
    function Logic: TLevelLogic;
    function Info: TLevelInfo;
    procedure UnloadLevel;
    property Sectors: TSectorList read GetSectors;
    property Water: TBox3D read GetWater write SetWater;
    property Player: TPlayer read GetPlayer write SetPlayer;
    function LevelProperties: TAbstractLevel;
  end deprecated 'use TLevel together with a TCastleViewport';

  { Level logic. }
  TLevelLogic = class(TCastleBehavior)
  strict private
    FTime: TFloatTime;
    FLevel: TAbstractLevel;
  protected
    { Level that we are part of. }
    property Level: TAbstractLevel read FLevel;

    { Load scene from file, doing common tasks.
      @unorderedList(
        @item optionally create triangle octree
        @item(call PrepareResources, with prRenderSelf, prBoundingBox, prShadowVolume
          (if shadow volumes possible at all in this OpenGL context),)
        @item Free texture data, since they will not be needed anymore
      )
      @groupBegin }
    function LoadLevelScene(const URL: string;
      const PrepareForCollisions: boolean;
      const SceneClass: TCastleSceneClass): TCastleScene; overload; deprecated 'create and prepare TCastleScene instance directly';
    function LoadLevelScene(const URL: string;
      const PrepareForCollisions: boolean): TCastleScene; overload; deprecated 'create and prepare TCastleScene instance directly';
    { @groupEnd }

    { Handle a placeholder named in external modeler.
      Return @true if this is indeed a recognized placeholder name,
      and it was handled and relevant shape should be removed from level
      geometry (to not be rendered). }
    function Placeholder(const Shape: TShape; const PlaceholderName: string): boolean; virtual;

    { Called after all placeholders have been processed,
      that is after @link(TLevel.Load) placed initial creatures,
      items and other stuff on the level.
      Override it to do anything you want. }
    procedure PlaceholdersEnd; virtual;
  public
    { Create new level instance. Called before resources (creatures and items)
      are initialized (override PlaceholdersEnd if you need to do something
      after creatures and items are added).
      You can modify MainScene contents here.

      @param(Level

        Instance of TLevel, in particular with the root of level transformation
        in Level.RootTransform.

        The created TLevelLogic instance will be added to this Level.RootTransform,
        and it must stay there always.

        Passing it in constructor is necessary, as TLevelLogic descendants at
        construction may actually modify your world, and depend on it later.)

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
    constructor Create(const AOwner: TComponent;
      const ALevel: TAbstractLevel;
      const MainScene: TCastleScene; const DOMElement: TDOMElement); reintroduce; virtual;

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

  TLevelLogicClasses = class({$ifdef FPC}specialize{$endif} TDictionary<string, TLevelLogicClass>)
  strict private
    function GetItems(const AKey: string): TLevelLogicClass;
    procedure SetItems(const AKey: string; const AValue: TLevelLogicClass);
  public
    { Access dictionary items.
      Setting this is allowed regardless if the key previously existed or not,
      in other words: setting this does AddOrSetValue, contrary to the ancestor TDictionary
      that only allows setting when the key already exists. }
    property Items [const AKey: string]: TLevelLogicClass read GetItems write SetItems; default;
  end;

function LevelLogicClasses: TLevelLogicClasses;

{ All known levels. You can use this to show a list of available levels to user.
  You can also search it and use @link(TLevel.Load) to load
  a given TLevelInfo instance. }
function Levels: TLevelInfoList;

implementation

uses SysUtils, Generics.Defaults, Math,
  CastleGLUtils, CastleFilesUtils, CastleStringUtils,
  CastleGLImages, CastleUIControls, XMLRead, CastleXMLUtils, CastleLog,
  X3DCameraUtils, CastleGLVersion, CastleURIUtils, CastleDownload, CastleThirdPersonNavigation;

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
    (or when the last TLevel referring to TLevelInfo is destroyed).
    Owns it's Items. }
  FLevels: TLevelInfoList;

function Levels: TLevelInfoList;
begin
  Result := FLevels;
end;

{ TLevelInternalLogic -------------------------------------------------------- }

procedure TLevel.TLevelInternalLogic.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  Level.Update(SecondsPassed);
end;

{ TLevel ---------------------------------------------------------- }

constructor TLevel.Create(AOwner: TComponent);
begin
  inherited;
  FWater := TBox3D.Empty;
  FInternalLogic := TLevelInternalLogic.Create(Self);
  FInternalLogic.Level := Self;
end;

destructor TLevel.Destroy;
begin
  if Info <> nil then
  begin
    if (Info.LevelResources <> nil) and
       { check LevelResourcesPrepared, to avoid calling
         Info.LevelResources.Release when Info.LevelResources.Prepare
         was not called (which may happen if there was an exception if LoadCore
         at MainScene.Load(SceneURL). }
       LevelResourcesPrepared and
       { check "Resources <> nil" is a hack to avoid calling Release
         on T3DResource when all T3DResource were already freed in CastleResources
         unit finalization.
         Testcase: darkest_before_dawn.

         TODO: This is not a general solution -- it would be better to implement in T3DResourceList
         a way to observe freeing of T3DResource, maybe even make T3DResource a TComponent
         and then make T3DResourceList as TComponentList. T3DResourceList should automatically
         remove items freed elsewhere. }
       (Resources <> nil) then
      Info.LevelResources.Release;

    Dec(FLevels.References);
    if FLevels.References = 0 then
      FreeAndNil(FLevels);
  end;

  { unregister free notification from this }
  Player := nil;

  FreeAndNil(FSectors);
  FreeAndNil(Waypoints);
  FreeAndNil(FInternalLogic);
  FreeAndNil(FFreeAtUnload); // free stuff owned by FFreeAtUnload now

  inherited;
end;

function TLevel.FreeAtUnload: TComponent;
begin
  if FFreeAtUnload = nil then
    FFreeAtUnload := TComponent.Create(nil);
  Result := FFreeAtUnload;
end;

function TLevel.GetPlayer: TCastleTransform;
begin
  Result := FPlayer;
end;

function TLevel.GetSectors: TSectorList;
begin
  Result := FSectors;
end;

function TLevel.RootTransform: TCastleRootTransform;
begin
  Result := Viewport.Items;
end;

function TLevel.CreaturesRoot: TCastleTransform;
begin
  Result := FCreaturesRoot;
end;

function TLevel.ItemsRoot: TCastleTransform;
begin
  Result := FItemsRoot;
end;

function TLevel.PrepareParams: TPrepareParams;
begin
  Result := Viewport.PrepareParams;
end;

function TLevel.Placeholder(Shape: TShape; PlaceholderName: string): boolean;
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
    Position, Direction: TVector3;
    IgnoredBegin, NumberBegin: Integer;
    ResourceNumber: Int64;
  begin
    { PlaceholderName is now <resource_name>[<resource_number>][_<ignored>][.<ignored>] }

    { cut off optional [_<ignored>] suffix }
    IgnoredBegin := Pos('_', PlaceholderName);
    if IgnoredBegin <> 0 then
      PlaceholderName := Copy(PlaceholderName, 1, IgnoredBegin - 1);
    IgnoredBegin := Pos('.', PlaceholderName);
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
    Position.Data[Items.GravityCoordinate] := Box.Data[0].Data[Items.GravityCoordinate];

    Direction := Info.PlaceholderReferenceDirection;
    Direction := Shape.State.Transformation.Transform.MultDirection(Direction);

    Resource.InstantiatePlaceholder(Self, Position, Direction,
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
      Waypoint.Position.ToString);}
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
    Items.MoveLimit := Shape.BoundingBox else
  if PlaceholderName = WaterName then
    Water := Shape.BoundingBox else
  if IsPrefix(SectorPrefix, PlaceholderName) then
    PlaceholderSector(Shape, SEnding(PlaceholderName, Length(SectorPrefix) + 1)) else
  if IsPrefix(WaypointPrefix, PlaceholderName) then
    PlaceholderWaypoint(Shape) else
    Result := Logic.Placeholder(Shape, PlaceholderName);
end;

function TLevel.UnloadCore: T3DResourceList;
begin
  Items.ClearExceptCameras;

  { free stuff like creatures, items, level logic.
    Note that things not owned by FreeAtUnload, like usual Player and FInternalLogic,
    remain untouched. }
  FreeAndNil(FFreeAtUnload);

  { We freed FLogic etc. above, since they are always owned by FFreeAtUnload.
    To be safe, set them to nil. }
  FLogic := nil;
  FCreaturesRoot := nil;
  FItemsRoot := nil;

  { save PreviousResources, before Info is overridden with new level.
    This allows us to keep PreviousResources while new resources are required,
    and this means that resources already loaded for previous level
    don't need to be reloaded for new. }
  Result := T3DResourceList.Create(false);
  if Info <> nil then
  begin
    Result.Clear;
    Result.AddRange(Info.LevelResources);
    Dec(Levels.References);
    FInfo := nil;
  end;
end;

procedure TLevel.Unload;
var
  PreviousResources: T3DResourceList;
begin
  PreviousResources := UnloadCore;
  PreviousResources.Release;
  FreeAndNil(PreviousResources);
end;

function TLevel.Items: TCastleRootTransform;
begin
  Result := Viewport.Items;
end;

procedure TLevel.LoadCore(const AInfo: TLevelInfo);
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
    {$warnings off} // using deprecated in deprecated unit
    Items.MainScene.BeforeNodesFree;
    for I := 0 to ItemsToRemove.Count - 1 do
      ItemsToRemove.Items[I].FreeRemovingFromAllParents;
    Items.MainScene.ChangedAll;
    {$warnings on}
  end;

  { After placeholders are processed, finish some stuff. }
  procedure PlaceholdersEnd;
  var
    NewMoveLimit: TBox3D;
  begin
    if Items.MoveLimit.IsEmpty then
    begin
      { Set MoveLimit to Items.MainScene.BoundingBox, and make maximum up larger. }
      {$warnings off} // using deprecated in deprecated unit
      NewMoveLimit := Items.MainScene.BoundingBox;
      {$warnings on}
      NewMoveLimit.Data[1].Data[Items.GravityCoordinate] :=
      NewMoveLimit.Data[1].Data[Items.GravityCoordinate] +
        4 * (NewMoveLimit.Data[1].Data[Items.GravityCoordinate] -
             NewMoveLimit.Data[0].Data[Items.GravityCoordinate]);
      Items.MoveLimit := NewMoveLimit;
    end;

    Sectors.LinkToWaypoints(Waypoints);

    Logic.PlaceholdersEnd;
  end;

var
  Options: TPrepareResourcesOptions;
  ShapeList: TShapeList;
  Shape: TShape;
  PreviousResources: T3DResourceList;
begin
  PreviousResources := UnloadCore;

  FInfo := AInfo;
  Inc(Levels.References);
  Info.Played := true;
  LevelResourcesPrepared := false;

  Progress.Init(1, 'Loading level "' + Info.Title + '"');
  try
    { Disconnect previous Viewport.Navigation.
      Otherwise, it would be updated by Items.MainScene loading binding new
      NavigationInfo (with it's speed) and Viewpoint.
      We prefer to do it ourselves in InitializeCamera. }
    {$warnings off} // using deprecated in deprecated unit
    Viewport.Navigation := nil;

    Items.MainScene := TCastleScene.Create(Self);
    Inc(Items.MainScene.InternalDirty);
    Items.MainScene.Load(Info.SceneURL);

    { Scene must be the first one on Items, this way Items.MoveCollision will
      use Scene for wall-sliding (see TCastleTransform.LocalMoveCollision implementation). }
    Items.Insert(0, Items.MainScene);
    {$warnings on}

    InitializeCamera;

    Progress.Step;
  finally
    Progress.Fini;
  end;

  { load new resources (and release old unused). This must be done after
    InitializeCamera (because it uses GravityUp), which in turn must
    be after loading Items.MainScene (because initial camera looks at Items.MainScene
    contents).
    It will show it's own progress bar. }
  Info.LevelResources.Prepare(Viewport.PrepareParams);
  LevelResourcesPrepared := true;
  PreviousResources.Release;
  FreeAndNil(PreviousResources);

  Progress.Init(1, 'Loading level "' + Info.Title + '"');
  try
    { add Player to Items.

      It is necessary to keep Player as 1st item:
      Testcase darkest-before-the-dawn:
      - Doing Info.LogicClass.Create creates a number of TCastleLinearMoving
        and adds them to Items.

      - If the TCastleLinearMoving are *before* Player on Items,
        things are broken when player is moved
        by TCastleLinearMoving (standing on elevator):

        TCastleLinearMoving.Update changes Player transformation,
        causing TPlayer.ChangedTransform
        causing TPlayer.SynchronizeToNavigation.

        But in effect any changes possibly done by user input in the same frame
        (e.g. moving around) are lost.
        When we reach TPlayer.Update, it calls SynchronizeFromNavigation
        that does nothing, as Navigation and Player transformation
        are for sure synchronized now.

      - OTOH, when Player is *before* everything else:

        TPlayer.Update calls SynchronizeFromNavigation which can apply
        to Player transformation changes done to Viewport.Camera
        (done by TCastleWalkNavigation receiving inputs).

      IOW, TPlayer now requires that you let SynchronizeFromNavigation to happen
      in each frame, before any SynchronizeToNavigation.

      Do not add Player if was already added.
      This happens if UseThirdPerson was used,
      then ThirdPersonNavigation.AvatarHierarchy is already added to world,
      and it is equal to Player.
    }
    if (Player <> nil) and (Player.World = nil) then
      Items.Add(Player);

    { add FInternalLogic to Items }
    Items.Add(FInternalLogic);

    { add CreaturesRoot, ItemsRoot to Items }
    FCreaturesRoot := TCastleTransform.Create(FreeAtUnload);
    Items.Add(FCreaturesRoot);
    FItemsRoot := TCastleTransform.Create(FreeAtUnload);
    Items.Add(FItemsRoot);

    { add FLogic (new Info.LogicClass instance) }
    {$warnings off} // using deprecated in deprecated unit
    FLogic := Info.LogicClass.Create(FreeAtUnload, Self, Items.MainScene, Info.Element);
    {$warnings on}
    Items.AddBehavior(Logic);

    { We will calculate new Sectors and Waypoints and other stuff
      based on placeholders. Initialize them now to be empty. }
    FreeAndNil(FSectors);
    FreeAndNil(Waypoints);
    FSectors := TSectorList.Create(true);
    Waypoints := TWaypointList.Create(true);
    Items.MoveLimit := TBox3D.Empty;
    Water := TBox3D.Empty;

    ItemsToRemove := TX3DNodeList.Create(false);
    try
      {$warnings off} // using deprecated in deprecated unit
      ShapeList := Items.MainScene.Shapes.TraverseList({ OnlyActive } true);
      {$warnings on}
      for Shape in ShapeList do
        TraverseForPlaceholders(Shape);
      RemoveItemsToRemove;
    finally ItemsToRemove.Free end;

    PlaceholdersEnd;

    { calculate Options for PrepareResources }
    Options := [prRenderSelf, prBackground, prBoundingBox];
    if (GLFeatures <> nil) and GLFeatures.ShadowVolumesPossible then
      Include(Options, prShadowVolume);

    {$warnings off} // using deprecated in deprecated unit
    Items.MainScene.PrepareResources(Options, false, Viewport.PrepareParams);

    Items.MainScene.FreeResources([frTextureDataInNodes]);
    {$warnings on}

    Progress.Step;
  finally
    Progress.Fini;
  end;

  {$warnings off} // using deprecated in deprecated unit
  Items.MainScene.PreciseCollisions := true;
  Items.MainScene.PrepareResources([prSpatial], Viewport.PrepareParams);
  {$warnings on}

  if (Player <> nil) then
    Player.LevelChanged;

  SoundEngine.LoopingChannel[0].Sound := Info.MusicSound;
  {$warnings off} // using deprecated in deprecated unit
  SoundEngine.PrepareResources;

  Items.MainScene.ProcessEvents := true;

  Dec(Items.MainScene.InternalDirty);
  {$warnings on}
end;

procedure TLevel.Load(const AInfo: TLevelInfo);
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
    LoadCore(AInfo);
  finally
    Progress.UserInterface.Image := nil; // unassign AInfo.LoadingImage, while OwnsImage = false
    Progress.UserInterface.OwnsImage := SavedOwnsImage;
    Progress.UserInterface.Image := SavedImage;
    Progress.UserInterface.BarYPosition := SavedBarYPosition;
  end;
end;

procedure TLevel.Load(const LevelName: string);
begin
  Load(Levels.FindName(LevelName));
end;

procedure TLevel.Update(const SecondsPassed: Single);

  procedure UpdatePlayerSwimming;
  begin
    if Player = nil then
    begin
      FPlayerSwimming := psNo;
      Exit;
    end;

    if Water.Contains(Player.Translation) then
      FPlayerSwimming := psUnderWater
    else
    { TODO: swimming on water surface is not ready for 3rd-person navigation. }
    if (not Player.UseThirdPerson) and
       Water.Contains(Player.Translation - Viewport.Camera.GravityUp * Player.WalkNavigation.PreferredHeight) then
      FPlayerSwimming := psAboveWater
    else
      FPlayerSwimming := psNo;

    Player.Swimming := FPlayerSwimming;
  end;

  procedure UpdateSickProjection;
  var
    DistortFov, DistortAspect: Single;
    S, C: Extended;
  begin
    DistortFov := 1;
    DistortAspect := 1;
    if FPlayerSwimming = psUnderWater then
    begin
      SickProjectionTime := SickProjectionTime + SecondsPassed * Items.TimeScale;
      SinCos(SickProjectionTime * Player.SickProjectionSpeed, S, C);
      DistortFov := DistortFov + (C * 0.03);
      DistortAspect := DistortAspect + (S * 0.03);
    end;
    Viewport.InternalDistortFieldOfViewY := DistortFov;
    Viewport.InternalDistortViewAspect := DistortAspect;
  end;

begin
  UpdatePlayerSwimming;
  UpdateSickProjection;
end;

procedure TLevel.SetPlayer(const Value: TPlayer);
begin
  if FPlayer <> Value then
  begin
    if FPlayer <> nil then
    begin
      FPlayer.RemoveFreeNotification(Self);
      Items.Remove(FPlayer);
    end;
    FPlayer := Value;
    if FPlayer <> nil then
    begin
      FPlayer.FreeNotification(Self);
      FPlayer.InternalLevel := Self;
    end;
    Viewport.AvoidNavigationCollisions := Value;

    { Reinitialize camera and navigation only when level was loaded. }
    if FInfo <> nil then
    begin
      InitializeCamera;
      if FPlayer <> nil then
      begin
        { Add Player to Items,
          as the second item (see LoadCore for explanation why),
          making sure it's not already there (because InitializeCamera
          may have already added it when UseThirdPerson). }
        if FPlayer.World = nil then
        begin
          Items.Insert(1, FPlayer);
          FPlayer.LevelChanged;
        end;
      end;
    end;
  end;
end;

procedure TLevel.InitializeCamera;
var
  InitialPosition: TVector3;
  InitialDirection: TVector3;
  InitialUp: TVector3;
  GravityUp: TVector3;
  Radius, PreferredHeight: Single;
  ProjectionNear: Single;
  NavigationNode: TNavigationInfoNode;
  WalkNavigation: TCastleWalkNavigation;
  ThirdPersonNavigation: TCastleThirdPersonNavigation;
begin
  {$warnings off} // using deprecated in deprecated unit
  if Items.MainScene.ViewpointStack.Top <> nil then
    Items.MainScene.ViewpointStack.Top.GetView(InitialPosition,
      InitialDirection, InitialUp, GravityUp) else
  {$warnings on}
  begin
    InitialPosition := DefaultX3DCameraPosition[cvVrml2_X3d];
    InitialDirection := DefaultX3DCameraDirection;
    InitialUp := DefaultX3DCameraUp;
    GravityUp := DefaultX3DGravityUp;
  end;

  {$warnings off} // using deprecated in deprecated unit
  NavigationNode := Items.MainScene.NavigationInfoStack.Top;
  {$warnings on}

  // calculate Radius
  Radius := 0;
  if (NavigationNode <> nil) and
     (NavigationNode.FdAvatarSize.Count >= 1) then
    Radius := NavigationNode.FdAvatarSize.Items[0];
  // If radius not specified, or invalid (<0), calculate it
  {$warnings off} // using deprecated in deprecated unit
  if Radius <= 0 then
    Radius := DefaultCameraRadius;
  {$warnings on}
  Assert(Radius > 0, 'Navigation Radius must be > 0');

  // calculate ProjectionNear
  ProjectionNear := Radius * RadiusToProjectionNear;
  Viewport.Camera.ProjectionNear := ProjectionNear;

  if (NavigationNode <> nil) and
    (NavigationNode.FdAvatarSize.Count >= 2) then
    PreferredHeight := NavigationNode.FdAvatarSize.Items[1]
  else
    PreferredHeight := Max(TCastleNavigation.DefaultPreferredHeight, Radius * RadiusToPreferredHeightMin);
  CorrectPreferredHeight(PreferredHeight, Radius,
    TCastleWalkNavigation.DefaultCrouchHeight, TCastleWalkNavigation.DefaultHeadBobbing);

  WalkNavigation := nil;
  ThirdPersonNavigation := nil;
  if Player <> nil then
  begin
    if Player.UseThirdPerson then
      ThirdPersonNavigation := Player.ThirdPersonNavigation
    else
      WalkNavigation := Player.WalkNavigation;
  end else
    { If you don't initialize Player (like for castle1 background level
      or castle-view-level or lets_take_a_walk) then just create a camera. }
    WalkNavigation := TCastleWalkNavigation.Create(Self);

  { initialize some navigation settings of player }
  if Player <> nil then
  begin
    Player.DefaultPreferredHeight := PreferredHeight;
    if NavigationNode <> nil then
      Player.DefaultMoveHorizontalSpeed := NavigationNode.FdSpeed.Value else
      Player.DefaultMoveHorizontalSpeed := 1.0;
    Player.DefaultMoveVerticalSpeed := 20;
  end else
  if WalkNavigation <> nil then
  begin
    { if you use TCastlePlayer, then it will automatically update navigation
      speed properties. But if not (when Player = nil), we have to set them
      explicitly here. }
    WalkNavigation.PreferredHeight := PreferredHeight;
    if NavigationNode <> nil then
      WalkNavigation.MoveHorizontalSpeed := NavigationNode.FdSpeed.Value else
      WalkNavigation.MoveHorizontalSpeed := 1.0;
    WalkNavigation.MoveVerticalSpeed := 20;
  end;


  // will be overridden by ThirdPersonNavigation.Init, possibly
  {$warnings off} // using deprecated in deprecated unit
  Viewport.Camera.Init(InitialPosition, InitialDirection, InitialUp, GravityUp);
  {$warnings on}

  if WalkNavigation <> nil then
  begin
    WalkNavigation.PreferredHeight := PreferredHeight;
    WalkNavigation.Radius := Radius;
    WalkNavigation.CorrectPreferredHeight;
    WalkNavigation.CancelFalling;

    {$warnings off} // using deprecated in deprecated unit
    Viewport.Navigation := WalkNavigation;
    {$warnings on}
  end else
  begin
    {$warnings off} // using deprecated in deprecated unit
    Viewport.Navigation := ThirdPersonNavigation;
    {$warnings on}

    { Use InitialXxx vectors to position avatar, camera will be derived from it.
      Also add the avatar to Items (avatar needs to be part of hierarchy when
      ThirdPersonNavigation.Init is called). }
    if ThirdPersonNavigation.AvatarHierarchy <> nil then
    begin
      ThirdPersonNavigation.AvatarHierarchy.SetView(InitialPosition, InitialDirection, InitialUp);

      if (Player <> nil) and (ThirdPersonNavigation.AvatarHierarchy = Player) then
      begin
        Items.Insert(1, ThirdPersonNavigation.AvatarHierarchy);
        Player.LevelChanged;
      end else
        Items.Add(ThirdPersonNavigation.AvatarHierarchy);
    end else
    if ThirdPersonNavigation.Avatar <> nil then
    begin
      ThirdPersonNavigation.Avatar.SetView(InitialPosition, InitialDirection, InitialUp);
      Items.Add(ThirdPersonNavigation.Avatar);
    end;
    ThirdPersonNavigation.Init;
  end;
end;

procedure TLevel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FPlayer) then
    Player := nil;
end;

{ TGameSceneManager ---------------------------------------------------------- }

{ TGameSceneManager is deprecated, just passes all the work
  to internal TLevel instance. }

constructor TGameSceneManager.Create(AOwner: TComponent);
begin
  inherited;
  FLevel := TLevel.Create(nil);
  FLevel.Viewport := Self;
end;

destructor TGameSceneManager.Destroy;
begin
  FreeAndNil(FLevel);
  inherited;
end;

function TGameSceneManager.GetSectors: TSectorList;
begin
  Result := FLevel.Sectors;
end;

function TGameSceneManager.GetWater: TBox3D;
begin
  Result := FLevel.Water;
end;

procedure TGameSceneManager.SetWater(const Value: TBox3D);
begin
  FLevel.Water := Value;
end;

function TGameSceneManager.GetPlayer: TPlayer;
begin
  Result := FLevel.Player;
end;

procedure TGameSceneManager.SetPlayer(const Value: TPlayer);
begin
  FLevel.Player := Value;
end;

procedure TGameSceneManager.LoadLevel(const LevelName: string);
begin
  FLevel.Load(LevelName);
end;

procedure TGameSceneManager.LoadLevel(const AInfo: TLevelInfo);
begin
  FLevel.Load(AInfo);
end;

function TGameSceneManager.Logic: TLevelLogic;
begin
  Result := FLevel.Logic;
end;

function TGameSceneManager.Info: TLevelInfo;
begin
  Result := FLevel.Info;
end;

procedure TGameSceneManager.UnloadLevel;
begin
  FLevel.Unload;
end;

function TGameSceneManager.LevelProperties: TAbstractLevel;
begin
  Result := FLevel;
end;

{ TLevelLogic ---------------------------------------------------------------- }

constructor TLevelLogic.Create(const AOwner: TComponent;
  const ALevel: TAbstractLevel;
  const MainScene: TCastleScene; const DOMElement: TDOMElement);
begin
  inherited Create(AOwner);
  FLevel := ALevel;
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
  Options := [prRenderSelf, prBoundingBox { always needed }];
  if (GLFeatures <> nil) and GLFeatures.ShadowVolumesPossible then
    Include(Options, prShadowVolume);

  Result.PrepareResources(Options, FLevel.PrepareParams);

  if PrepareForCollisions then
    Result.PreciseCollisions := true;

  Result.FreeResources([frTextureDataInNodes]);

  Result.ProcessEvents := true;
end;

function TLevelLogic.LoadLevelScene(
  const URL: string;
  const PrepareForCollisions: boolean): TCastleScene;
begin
  {$warnings off} // using deprecated in deprecated
  Result := LoadLevelScene(URL, PrepareForCollisions, TCastleScene);
  {$warnings on}
end;

procedure TLevelLogic.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  FTime := FTime + SecondsPassed;
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
  begin
    Result := Element.AttributeString(AttrName, ValueStr);
    if not LevelLogicClasses.TryGetValue(ValueStr, Value) then
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
  DefaultPlaceholderReferenceDirection: TVector3 = (X: 1; Y: 0; Z: 0);
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

  { Required attributes }

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
    PlaceholderReferenceDirection := Vector3FromStr(S) else
    PlaceholderReferenceDirection := DefaultPlaceholderReferenceDirection;

  LevelResources.LoadResources(Element);
  AddAlwaysPreparedResources;

  {$warnings off} // using deprecated in deprecated unit
  if Element.AttributeString('music_sound', SoundName) then
    MusicSound := SoundEngine.SoundFromName(SoundName)
  else
    MusicSound := nil;
  {$warnings on}
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
    S := S + '.' + NL + NL + 'Warning: there are no levels available on the list at all. This means that the game data was not correctly installed (as we did not find any level.xml files defining any levels). Or the developer forgot to call Levels.LoadFromFiles.';
  raise Exception.Create(S);
end;

function IsSmallerByNumber({$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} A, B: TLevelInfo): Integer;
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
  FindFiles(LevelsPath, 'level.xml', false,
    {$ifdef FPC}@{$endif}AddFromInfo, [ffRecursive]);
end;

procedure TLevelInfoList.LoadFromFiles;
begin
  LoadFromFiles('castle-data:/');
  SortByNumber;
end;

procedure TLevelInfoList.SortByNumber;
type
  TLevelInfoComparer = {$ifdef FPC}specialize{$endif} TComparer<TLevelInfo>;
begin
  Sort(TLevelInfoComparer.Construct({$ifdef FPC}@{$endif}IsSmallerByNumber));
end;

{ TLevelLogicClasses --------------------------------------------------------- }

function TLevelLogicClasses.GetItems(const AKey: string): TLevelLogicClass;
begin
  Result := inherited Items[AKey];
end;

procedure TLevelLogicClasses.SetItems(const AKey: string; const AValue: TLevelLogicClass);
begin
  AddOrSetValue(AKey, AValue);
end;

{ initialization / finalization ---------------------------------------------- }

initialization
  FLevels := TLevelInfoList.Create(true);
  Inc(FLevels.References);
finalization
  FreeAndNil(FLevelLogicClasses);
  { there may still exist TLevel instances that refer to our
    TLevelInfo instances. So we don't always free Levels below. }
  if FLevels <> nil then
  begin
    Dec(FLevels.References);
    if FLevels.References = 0 then
      FreeAndNil(FLevels);
  end;
end.
