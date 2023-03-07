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

{ Manage resources with animations that may be reused by multiple instances.
  This is in practice used for creatures and items animations in our
  "Utilities for typical 3D games", https://castle-engine.io/manual_high_level_3d_classes.php .

  Note that you @italic(do not) need this unit in many normal cases when dealing with animations.
  It is generally simpler and more adviced to use TCastleScene and TCastleScene.PlayAnimation
  to play animations, see https://castle-engine.io/manual_scene.php . }
unit CastleResources
  deprecated 'manage scenes and their animations using TCastleScene';

{$I castleconf.inc}

interface

uses Classes, DOM, Generics.Collections,
  CastleVectors, CastleXMLConfig, CastleTimeUtils, CastleFrustum, CastleClassUtils,
  CastleScene, X3DNodes, CastleTransform, CastleBoxes, CastleFindFiles,
  CastleSectors;

type
  T3DResource = class;

  { Animation defined by T3DResource. }
  T3DResourceAnimation = class
  private
    type
      { Container for a loaded TCastleScene,
        which may be specific inside animation (T3DResourceAnimation),
        or may be common to all animations (one scene for the whole T3DResource). }
      TSceneState = record
        Scene: TCastleScene;
        InitialBox: TBox3D; //< Defined only once Scene <> nil

        ForcedAnimationName: String;
        ForcedLoop: Boolean;
        ForcedActualTime: TFloatTime;

        { Prepare scene loading from given URL.
          Loads the scene only if URL is not empty and if it's not already loaded (that is, when Scene = nil).
          Prepares for fast rendering and other processing by TCastleTransform.PrepareResources.
          Calls Progress.Step 2 times, if DoProgress. }
        procedure Prepare(const URL: String; const Resource: T3DResource;
          const PrepareParams: TPrepareParams; const DoProgress: Boolean);
      end;
    var
      FName: string;
      FRequired: boolean;
      FOwner: T3DResource;
      FSceneState: TSceneState;
      FDuration: Single;
      FURL: string;
      FAnimationName: string;
    procedure Prepare(const Params: TPrepareParams; const DoProgress: boolean);
    procedure Release;
    procedure LoadFromFile(ResourceConfig: TCastleConfig);
    property Owner: T3DResource read FOwner;

    { Current Scene to render for given time.

      Looping is automatically done here, if parameter Loop is @true.
      When it is @false, there is no looping, which means that
      when Time is < 0, we show the first frame,
      and when Time is > @link(Duration), we show the last frame forever.

      This returns the scene (TCastleScene) with state reflecting given time
      (TimeSensor forced to given time).

      Note that this should not be used if owner resource uses pooling.
      In case of pooling, not ForceAnimationPose calls should be done
      (which is what this method does). }
    function SceneAtTime(const Time: TFloatTime; const Loop: boolean): TCastleScene;
  public
    constructor Create(const AOwner: T3DResource;
      const AName: string; const ARequired: boolean = true);

    { Duration of the animation. See engine tutorial about how resources animations
      duration is calculated. Always 0 if not @link(Defined). }
    property Duration: Single read FDuration;
    function BoundingBox: TBox3D;

    { Was the animation state defined in resource.xml file.
      May be @false only if @link(Required) was @false, or before we actually
      read animation info from resource.xml file. }
    function Defined: boolean;

    { Scene URL, only when each animation is inside a separate 3D file.
      See [https://castle-engine.io/creating_data_resources.php]
      for documentation how you can define creature animations. }
    property URL: string read FURL write FURL; {$ifdef FPC}deprecated 'do not use separate URLs for each animation; use one URL with all animations; see https://castle-engine.io/creating_data_resources.php';{$endif}

    { Animation name (like for @link(TCastleSceneCore.PlayAnimation)),
      which is equal to TimeSensor node name.
      All animations are started by X3D TimeSensor node.
      If not given, we assume it's just 'animation', which is fine
      at least for castle-anim-frames and MD3 files loaded using TNodeInterpolator.
      This refers to an X3D TimeSensor node inside
      animation model (from @link(URL)) or, when not defined,
      inside whole resource model (from @link(T3DResource.ModelURL)).

      See [https://castle-engine.io/creating_data_resources.php]
      for documentation how you can define creature animations. }

    property AnimationName: string read FAnimationName write FAnimationName;
    {$ifdef FPC}
    property TimeSensor: string read FAnimationName write FAnimationName;
      deprecated 'use AnimationName';
    {$endif}
    property Name: string read FName;
    property Required: boolean read FRequired;
  end;

  T3DResourceAnimationList = class({$ifdef FPC}specialize{$endif} TObjectList<T3DResourceAnimation>)
    { Find an animation by name.
      @raises Exception if not found. }
    function FindName(const AName: string): T3DResourceAnimation;
  end;

  TAbstractLevel = class;

  { Display a specified frame of the specified animation.
    This is reliable even when multiple TResourceFrame request different frames
    from the same animation. }
  TResourceFrame = class(TCastleTransform)
  strict private
    FAnimation: T3DResourceAnimation;
    FTime: TFloatTime;
    FLoop: boolean;
    CurrentChild: TCastleScene;
    CurrentChildFromPool: Boolean;
    ResourceObserver: TFreeNotificationObserver;
    procedure ResourceFreeNotification(const Sender: TFreeNotificationObserver);
  protected
    procedure LocalRender(const Params: TRenderParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Animation: T3DResourceAnimation read FAnimation;
    { Time within the ResourceAnimation. }
    property Time: TFloatTime read FTime;
    { Should we loop within ResourceAnimation. }
    property Loop: boolean read FLoop;
    { Set which animation and animation frame to display. }
    procedure SetFrame(const Level: TAbstractLevel;
      const AnAnimation: T3DResourceAnimation;
      const ATime: TFloatTime; const ALoop: boolean);
  end;

  { Abstract level information, with information useful to spawn resources
    like creatures and items. }
  TAbstractLevel = class(TComponent)
  public
    function GetPlayer: TCastleTransform; virtual; abstract;
    function GetSectors: TSectorList; virtual; abstract;
    function RootTransform: TCastleRootTransform; virtual; abstract;
    { All creatures are children of this.
      Allows to easily hide them. }
    function CreaturesRoot: TCastleTransform; virtual; abstract;
    { All items are children of this.
      Allows to easily hide them. }
    function ItemsRoot: TCastleTransform; virtual; abstract;
    { Parameters to prepare rendering for,
      see @link(TCastleViewport.PrepareParams). }
    function PrepareParams: TPrepareParams; virtual; abstract;
    { Use this as Owner of any TComponent, to have something automatically freed
      when level is being unloaded (at @link(TLevel.Unload) call,
      or at @link(TLevel.Load) of new level,
      or at destruction of this TLevel). }
    function FreeAtUnload: TComponent; virtual; abstract;
  end;

  { Resource used for rendering and processing of 3D objects.
    By itself this doesn't render or do anything.
    But some 3D objects may need to have such resource prepared to work.

    It can also load it's configuration from XML config file.
    For this purpose, it has a unique identifier in @link(Name) property.

    Name (from TComponent.Name) is a unique identifier of this resource.
    Used to refer to this resource from level placeholders
    (see TLevel.Load about placeholders),
    from other XML files (for example one creature may shoot another
    creature as a missile using @link(TWalkAttackCreatureResource.FireMissileName)),
    and in other places.

    Name can use only letters, use CamelCase.
    Reason: This must be a valid identifier in both VRML/X3D and ObjectPascal.
    Also digits and underscores are reserved, as we may use them to get other
    information from placeholder names. }
  T3DResource = class(TComponent)
  private
    FPrepared: boolean;
    FUsageCount: Cardinal;
    FConfigAlwaysPrepared: boolean;
    FFallSpeed, FGrowSpeed: Single;
    FAnimations: T3DResourceAnimationList;
    FReceiveShadowVolumes: boolean;
    FCastShadowVolumes: boolean;
    FDefaultAnimationTransition: Single;
    FModelURL: string;
    { Model loaded from ModelURL }
    ModelState: T3DResourceAnimation.TSceneState;
    { Non-nil only if we're using Pool to allocate scenes for resource instances.
      See @link(Pool) description. }
    ScenePool: TCastleSceneList;
    { First ScenePoolUsed items on ScenePool are used, rest is unused. }
    ScenePoolUsed: Cardinal;
    FPool: Cardinal;
    FOrientation: TOrientationType;
    function AllocateSceneFromPool(const Level: TAbstractLevel): TCastleScene;
    procedure ReleaseSceneFromPool(const Scene: TCastleScene);
    function CreateSceneForPool(const Params: TPrepareParams): TCastleScene;
  protected
    { Prepare or release everything needed to use this resource.
      PrepareCore and ReleaseCore should never be called directly,
      they are only to be overridden in descendants.
      These are used by actual @link(Prepare) and @link(Release)
      when the actual allocation / deallocation should take place
      (when UsageCount raises from zero or drops back to zero).

      ReleaseCore is also called in destructor, regardless of UsageCount.
      This is done to free resources even if user forgot to call Release
      before destroying this resource instance.

      PrepareCore must call Progress.Step exactly PrepareCoreSteps times,
      only if DoProgress.
      This allows to make nice progress bar in @link(Prepare).
      In this class, PrepareCoreSteps returns 0.
      @groupBegin }
    procedure PrepareCore(const Params: TPrepareParams;
      const DoProgress: boolean); virtual;
    function PrepareCoreSteps: Cardinal; virtual;
    procedure ReleaseCore; virtual;
    { @groupEnd }
  public
    const
      DefaultFallSpeed = 10.0;
      DefaultGrowSpeed = 5.0;
      DefaultReceiveShadowVolumes = true;
      DefaultCastShadowVolumes = true;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Are we in a (fully) prepared state. That is after a (fully successful)
      @link(Prepare) call and before @link(Release).
      Note that this is slightly different than checking @code(UsageCount <> 0):
      in some situations, UsageCount may be non-zero while the preparation
      is not finished yet. This property is guaranteed to be @true only if
      preparation was fully successfully (no exceptions) finished. }
    property Prepared: boolean read FPrepared;

    procedure LoadFromFile(ResourceConfig: TCastleConfig); virtual;

    { Release and then immediately prepare again this resource.
      Call only when UsageCount <> 0, that is when resource is prepared.
      Shows nice progress bar, using @link(Progress). }
    procedure RedoPrepare(const Params: TPrepareParams);

    { How many times this resource is used. Used by Prepare and Release:
      actual allocation / deallocation happens when this raises from zero
      or drops back to zero. }
    property UsageCount: Cardinal
      read FUsageCount write FUsageCount default 0;

    { Prepare or release everything needed to use this resource.

      There is an internal counter tracking how many times given
      resource was prepared and released. Which means that preparing
      and releasing resource multiple times is correct --- but make
      sure that every single call to prepare is paired with exactly one
      call to release. Actual allocation / deallocation
      (when protected methods PrepareCore, ReleaseCore are called)
      happens only when UsageCount raises from zero or drops back to zero.

      Show nice progress bar, using @link(Progress).

      @param(Params
        World parameters to prepare for.
        See @link(TCastleTransform.PrepareResources) for more comments.)

      @groupBegin }
    procedure Prepare(const Params: TPrepareParams; const GravityUp: TVector3); overload;
      deprecated 'use Prepare overload without the GravityUp parameter';
    procedure Prepare(const Params: TPrepareParams); overload;
    procedure Release;
    { @groupEnd }

    { Place an instance of this resource on World, using information
      from the placeholder on the level. }
    procedure InstantiatePlaceholder(
      const ALevel: TAbstractLevel;
      const APosition, ADirection: TVector3;
      const NumberPresent: boolean; const Number: Int64); virtual; abstract;

    { Animations of this resource.

      The first animation, if exists, right now determines the default radius
      calculation. So the first animation should have the bounding box
      representative for all animations.
      Other than that, the order on this list doesn't matter.

      The properties of these animations are automatically loaded from
      resource.xml file in LoadFromFile. The animations are automatically
      prepared / released by our @link(Prepare) / @link(Release) methods. }
    property Animations: T3DResourceAnimationList read FAnimations;

    { Mechanics of given game may suggest that some 3D resources should
      always be prepared. For example, in typical 3D game when player
      has inventory and can drop items from inventory on the ground,
      then all items should be prepared for all levels, since you can in theory
      drop everything anywhere.

      Return @true if this is such resource.

      Default implementation in T3DResource returns here the ConfigAlwaysPrepared
      value, which may be set in resource.xml and by default is false.
      This allows to configure this using resource.xml files.
      Descendants may choose to override this, to override value from resource.xml
      file. }
    function AlwaysPrepared: boolean; virtual;

    property ConfigAlwaysPrepared: boolean
      read FConfigAlwaysPrepared write FConfigAlwaysPrepared default false;

    { The speed (in units per second) of falling down because of gravity.
      Note that the gravity direction is controlled by your level 3D model,
      see "Which way is up" section in the engine tutorial
      https://castle-engine.io/manual_up.php .

      Currently, falling down of creatures and items just uses this constant speed.
      In the future, we plan to add properties to control mass and air friction
      and perform more physically-correct simulation of falling down.

      This has no effect for creatures with TCreatureResource.Flying = @true.
      This also has no effect for missile creatures (their
      TCreatureResource.Flying is ignored, they have special approach
      to gravity).

      See TCastleTransform.FallSpeed for precise definition, this works the same,
      except our default value is non-zero, and by default TCastleTransform.Gravity
      and TCastleTransform.PreferredHeight are already sensible for creatures/items. }
    property FallSpeed: Single
      read FFallSpeed write FFallSpeed {$ifdef FPC}default DefaultFallSpeed{$endif};

    { The speed (in units per second) of growing.

      "Growing" is used to allow non-flying creatures to climb stairs.
      The creature can move whenever a sphere (see TCreatureResource.MiddleHeight
      and TCreatureResource.Radius) can move. This means that part of the bounding
      box (part of the TCastleTransform.PreferredHeight) may temporarily
      "sink" into the ground. Then growing, controlled by this property,
      pushes the creature up.

      See TCastleTransform.GrowSpeed, this works the same,
      except the default value is non-zero, and by default TCastleTransform.Gravity
      and TCastleTransform.PreferredHeight are already sensible for creatures/items. }
    property GrowSpeed: Single
      read FGrowSpeed write FGrowSpeed {$ifdef FPC}default DefaultGrowSpeed{$endif};

    property ReceiveShadowVolumes: boolean
      read FReceiveShadowVolumes write FReceiveShadowVolumes
      default DefaultReceiveShadowVolumes;
    property CastShadowVolumes: boolean
      read FCastShadowVolumes write FCastShadowVolumes
      default DefaultCastShadowVolumes;

    { See @link(TCastleSceneCore.DefaultAnimationTransition) }
    property DefaultAnimationTransition: Single
      read FDefaultAnimationTransition write FDefaultAnimationTransition {$ifdef FPC}default 0.0{$endif};

    { See @link(TCastleTransform.Orientation), by default this is @link(TCastleTransform.DefaultOrientation).

      In the resource.xml file, this value can be specified using following strings:

      @definitionList(
        @itemLabel @code(default)
        @item Use TCastleTransform.DefaultOrientation.
        @itemLabel @code(up:y,direction:-z)
        @item Use otUpYDirectionMinusZ.
        @itemLabel @code(up:y,direction:z)
        @item Use otUpYDirectionZ. Matches conventional glTF orientation.
        @itemLabel @code(up:z,direction:-y)
        @item Use otUpZDirectionMinusY.
        @itemLabel @code(up:z,direction:x)
        @item Use otUpZDirectionX.
      ) }
    property Orientation: TOrientationType read FOrientation write FOrientation;

    { Model URL, only when you define multiple animations inside
      a single 3D file. See
      [https://castle-engine.io/creating_data_resources.php]
      for notes about <model> element in resource.xml files. }
    property ModelURL: string read FModelURL write FModelURL;

    { If non-zero, use a pool of TCastleScene to create resource instances.

      To understand what this controls, some explanation is necessary:
      Multiple instances of the same creature / item may be visible.
      They all refer to the same "resource" holding one set of data.
      Our resources mechanism is prepared to handle it efficiently, in 2 ways:

      @orderedList(
        @item(
          Without pool: We have a single TCastleScene, which is internally continuosly changed
          back-and-forth to show various animations (and various moment of these animations),
          to display all resource instances correctly.

          Advantages: Less loading time, less memory usage (no need to create pool).

          Disadvantages:
          Worse FPS (need to switch back-and-forth) and no animation blending support.
        )

        @item(
          With pool: Each resource instance gets a copy of TCastleScene.
          This avoids switching one scene back-and-forth.
          It's particularly beneficial for animations like from glTF or Spine JSON
          (in general: animations from formats other than castle-anim-frames).

          We keep a pool of TCastleScene that can be allocated for the needed instances.

          The "pool" attribute specifies the initial pool size.
          It should be large enough to cover practical needs.
          If it's not large enough then, when necessary, we will increase this pool at runtime,
          which works but causes one-time lag during game execution
          (as we need to make TCastleScene.Clone).

          This approach also allows for animation blending. (TODO: in the future)

          Disadvantages: More loading time and memory usage (for pool).

          Advantages:
          Better FPS and support for animation blending.
          In general, this uses TCastleScene in more standard way.
        )
      )

      Note that Pool only matters if you use a single file
      for all resource animations. Using multiples files (separate file for each animation)
      is deprecated anyway, see https://castle-engine.io/creating_data_resources.php .

      In the @code(resource.xml), you can specify this value an explicit integer
      (0 means to not use pool), or you can write @code("auto") to automatically guess
      the best value. The best value means to use pool, unless the ModelURL indicates castle-anim-frames
      or MD3 formats.
    }
    property Pool: Cardinal read FPool write FPool default 0;
  end;

  T3DResourceClass = class of T3DResource;

  T3DResourceList = class({$ifdef FPC}specialize{$endif} TObjectList<T3DResource>)
  private
    ResourceXmlReload: boolean;
    procedure AddFromInfo(const FileInfo: TFileInfo; var StopSearch: boolean);
    procedure AddFromFileDefaultReload(const URL: string);
  public
    { Find resource with given T3DResource.Name.
      @raises Exception if not found and NilWhenNotFound = false. }
    function FindName(const AName: string; const NilWhenNotFound: boolean = false): T3DResource;

    { Load all resources (creatures and items) information from
      resource.xml files found in given Path.
      Overloaded version without Path just scans the whole castle-data:/
      directory.

      Note that on Android, searching the Android asset filesystem
      recursively is not possible (this is a fault of Android NDK API...).
      So instead of this method, you should use AddFromFile repeatedly
      to explicitly list all resource.xml locations.

      @param(Reload
        If Reload, then we will not clear the initial list contents.
        Instead, resource.xml files found that refer to the existing T3DResource.Name
        will cause T3DResource.LoadFromFile call on an existing resource.
        Using Reload is a nice debug feature, if you want to reload configuration
        from resource.xml files (and eventually add new resources in new resource.xml files),
        but you don't want to recreate existing resource instances.)

      @groupBegin }
    procedure LoadFromFiles(const Path: string; const Reload: boolean = false); overload;
    procedure LoadFromFiles(const Reload: boolean = false); overload;
    { @groupEnd }

    { Load a single resource from resource.xml file.

      @param(Reload If @true, and the loaded resource will have a name
        matching existing T3DResource.Name, we will replace the current resource.
        Otherwise, we'll make an exception.) }
    procedure AddFromFile(const URL: string; const Reload: boolean = false);

    { Reads <prepare_resources> XML element.
      <prepare_resources> element is an optional child of given ParentElement.
      Sets current list value with all mentioned required
      resources (subset of @link(Resources)). }
    procedure LoadResources(ParentElement: TDOMElement);

    { Prepare / release all resources on list.
      @groupBegin }
    procedure Prepare(const Params: TPrepareParams;
      const ResourcesName: string = 'resources');
    procedure Release;
    { @groupEnd }
  end;

{ All known resources.
  Usually you call @link(T3DResourceList.LoadFromFiles Resources.LoadFromFiles)
  to fill this list, based on resource.xml files present in your data. }
function Resources: T3DResourceList;

{ Register a resource class, to allow creating resources (like a creature or item)
  of this class by using appropriate type="xxx" inside resource.xml file. }
procedure RegisterResourceClass(const AClass: T3DResourceClass; const TypeName: string);

implementation

{$warnings off} // using deprecated CastleProgress in deprecated
uses SysUtils,
  CastleProgress, CastleXMLUtils, CastleUtils, CastleSceneCore,
  CastleStringUtils, CastleLog, CastleConfig, CastleApplicationProperties,
  CastleFilesUtils, CastleInternalNodeInterpolator, CastleURIUtils;
{$warnings on}

var
  UnitFinalization: Boolean;

const
  ScenePrepareResources = [prRenderSelf, prBoundingBox, prShadowVolume];

{ TResourceClasses ---------------------------------------------------------- }

type
  TResourceClasses = class({$ifdef FPC}specialize{$endif} TDictionary<string, T3DResourceClass>)
  strict private
    function GetItems(const AKey: string): T3DResourceClass;
    procedure SetItems(const AKey: string; const AValue: T3DResourceClass);
  public
    { Access dictionary items.
      Setting this is allowed regardless if the key previously existed or not,
      in other words: setting this does AddOrSetValue, contrary to the ancestor TDictionary
      that only allows setting when the key already exists. }
    property Items [const AKey: string]: T3DResourceClass read GetItems write SetItems; default;
  end;

function TResourceClasses.GetItems(const AKey: string): T3DResourceClass;
begin
  Result := inherited Items[AKey];
end;

procedure TResourceClasses.SetItems(const AKey: string; const AValue: T3DResourceClass);
begin
  AddOrSetValue(AKey, AValue);
end;

var
  FResourceClasses: TResourceClasses;

function ResourceClasses: TResourceClasses;
begin
  if (FResourceClasses = nil) and not UnitFinalization then
    FResourceClasses := TResourceClasses.Create;
  Result := FResourceClasses;
end;

{ T3DResourceAnimation.TSceneState ------------------------------------------- }

procedure T3DResourceAnimation.TSceneState.Prepare(
  const URL: String; const Resource: T3DResource;
  const PrepareParams: TPrepareParams; const DoProgress: Boolean);
begin
  if (URL <> '') and (Scene = nil) then
  begin
    Scene := TCastleScene.Create(nil);
    Scene.Load(Url);
    Scene.ReceiveShadowVolumes := Resource.ReceiveShadowVolumes;

    { save initial Scene bounding box,
      to avoid later changing T3DResourceAnimation.BoundingBox
      result after some animation run and modified scene.
      This would cause creature radius change.

      Testcase: examples/animations/deprecated_resource_animations,
      load same resource 2x in a row,
      bounding sphere should not change. }
    InitialBox := Scene.BoundingBox;

    // reset ForcedXxx fields
    ForcedAnimationName := '';
    ForcedLoop := false;
    ForcedActualTime := 0;
  end;
  if DoProgress then Progress.Step;

  if Scene <> nil then
    Scene.PrepareResources(ScenePrepareResources, PrepareParams);
  if DoProgress then Progress.Step;
end;

{ T3DResourceAnimation ------------------------------------------------------- }

constructor T3DResourceAnimation.Create(const AOwner: T3DResource;
  const AName: string; const ARequired: boolean);
begin
  inherited Create;
  FName := AName;
  FRequired := ARequired;
  FOwner := AOwner;
  AOwner.Animations.Add(Self);
end;

{ $define STATISTICS_FORCING_OPTIMIZATION}
{$ifdef STATISTICS_FORCING_OPTIMIZATION}
var
  Necessary, Avoided: Int64;
{$endif}

function T3DResourceAnimation.SceneAtTime(const Time: TFloatTime;
  const Loop: boolean): TCastleScene;

  procedure ForceTime(var SceneState: TSceneState);
  var
    GoodAnimationName: string;
    ActualTime: TFloatTime;
    ForceNecessary: boolean;
  begin
    if AnimationName <> '' then
      GoodAnimationName := AnimationName
    else
      GoodAnimationName := TNodeInterpolator.DefaultAnimationName;

    // if Defined and (Duration = 0) then
    //   WritelnWarning('Animation "%s" duration is zero on resource "%s"',
    //     [GoodAnimationName, Owner.Name]);

    { Calculate Time with looping/clamping applied, because we want to have
      ForceNecessary = false as often as possible (to avoid doing work).
      Test Duration <> 0 to avoid dividing by 0 in FloatModulo(Time, 0)
      (testcase: fps_game debug build on Android) }
    if Loop and (Duration <> 0) then
      ActualTime := FloatModulo(Time, Duration)
    else
      ActualTime := Clamped(Time, 0, Duration);

    // call (costly) ForceAnimationPose only if necessary
    ForceNecessary :=
      (SceneState.ForcedAnimationName <> GoodAnimationName) or
      (SceneState.ForcedLoop <> Loop) or
      (SceneState.ForcedActualTime <> ActualTime);
    if ForceNecessary then
    begin
      SceneState.ForcedAnimationName := GoodAnimationName;
      SceneState.ForcedLoop := Loop;
      SceneState.ForcedActualTime := ActualTime;
      SceneState.Scene.ForceAnimationPose(GoodAnimationName, ActualTime, Loop);
    end;

    {$ifdef STATISTICS_FORCING_OPTIMIZATION}
    if ForceNecessary then
      Inc(Necessary)
    else
      Inc(Avoided);
    if (Necessary + Avoided) mod 100 = 0 then
      WritelnLog('T3DResourceAnimation.Scene forcing optimization: %d necessary vs %d avoided => %f fraction of work avoided',
        [Necessary, Avoided, Avoided / (Necessary + Avoided)]);
    {$endif}
  end;

begin
  if FSceneState.Scene <> nil then
  begin
    ForceTime(FSceneState);
    Result := FSceneState.Scene;
  end else
  if Owner.ModelState.Scene <> nil then
  begin
    ForceTime(Owner.ModelState);
    Result := Owner.ModelState.Scene;
  end else
    Result := nil;
end;

function T3DResourceAnimation.BoundingBox: TBox3D;
begin
  if FSceneState.Scene <> nil then
    Result := FSceneState.InitialBox
  else
  if Owner.ModelState.Scene <> nil then
    Result := Owner.ModelState.InitialBox
  else
    { animation 3D model not loaded }
    Result := TBox3D.Empty;
end;

function T3DResourceAnimation.Defined: boolean;
begin
  {$warnings off} // using deprecated to keep it working
  Result := (URL <> '') or (AnimationName <> '');
  {$warnings on}
end;

procedure T3DResourceAnimation.Prepare(const Params: TPrepareParams;
  const DoProgress: boolean);
begin
  {$warnings off} // using deprecated to keep it working
  if URL <> '' then
  begin
    FSceneState.Prepare(URL, Owner, Params, DoProgress);
    if AnimationName <> '' then
      FDuration := FSceneState.Scene.AnimationDuration(AnimationName)
    else
      FDuration := FSceneState.Scene.AnimationDuration(TNodeInterpolator.DefaultAnimationName);
  end else
  {$warnings on}
  if AnimationName <> '' then
  begin
    if Owner.ModelState.Scene = nil then
      raise Exception.CreateFmt('Animation "%s" of resource "%s": animation_name is defined, but model url is not defined (neither specific to this animation nor containing multiple animations)',
        [Name, Owner.Name]);
    FDuration := Owner.ModelState.Scene.AnimationDuration(AnimationName);
  end else
  if Required then
    raise Exception.CreateFmt('No definition for required animation "%s" of resource "%s". You have to define url or animation_name for this animation in appropriate resource.xml file',
      [Name, Owner.Name]);
end;

procedure T3DResourceAnimation.Release;
begin
  FreeAndNil(FSceneState.Scene);
end;

procedure T3DResourceAnimation.LoadFromFile(ResourceConfig: TCastleConfig);
begin
  {$warnings off} // using deprecated to keep it working
  if ResourceConfig.GetValue('model/' + Name + '/file_name', '') <> '' then
  begin
    URL := ResourceConfig.GetURL('model/' + Name + '/file_name', true);
    WritelnWarning('Deprecated', 'Reading from deprecated "file_name" attribute inside resource.xml. Use "url" instead.');
  end else
    URL := ResourceConfig.GetURL('model/' + Name + '/url', true);

  if URL <> '' then
    WritelnWarning('Animation "%s" of "%s" has it''s own URL, this is deprecated. Use one URL for all animations. See https://castle-engine.io/creating_data_resources.php .', [
      Name,
      Owner.Name
    ]);
  {$warnings on}

  AnimationName := ResourceConfig.GetValue('model/' + Name + '/animation_name', '');
  if AnimationName = '' then
  begin
    AnimationName := ResourceConfig.GetValue('model/' + Name + '/time_sensor', '');
    if AnimationName <> '' then
      WritelnWarning('Deprecated', 'Reading from deprecated "time_sensor" attribute inside resource.xml. Use "animation_name" instead.');
  end;
end;

{ T3DResourceAnimationList --------------------------------------------------- }

function T3DResourceAnimationList.FindName(const AName: string): T3DResourceAnimation;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.Name = AName then
      Exit;
  end;
  raise Exception.CreateFmt('No resource animation named "%s"', [AName]);
end;

{ TResourceFrame ------------------------------------------------------------- }

constructor TResourceFrame.Create(AOwner: TComponent);
begin
  inherited;
  ResourceObserver := TFreeNotificationObserver.Create(Self);
  ResourceObserver.OnFreeNotification := {$ifdef FPC}@{$endif}ResourceFreeNotification;
end;

destructor TResourceFrame.Destroy;
begin
  if CurrentChildFromPool then
  begin
    CurrentChildFromPool := false;
    FAnimation.FOwner.ReleaseSceneFromPool(CurrentChild);
    Remove(CurrentChild);
    CurrentChild := nil;
  end;
  inherited;
end;

procedure TResourceFrame.LocalRender(const Params: TRenderParams);

  procedure UpdateChild;
  var
    NewChild: TCastleScene;
  begin
    if (FAnimation <> nil) and FAnimation.FOwner.Prepared then
      NewChild := FAnimation.SceneAtTime(FTime, FLoop)
    else
      NewChild := nil;

    if CurrentChild <> NewChild then
    begin
      if CurrentChild <> nil then
        Remove(CurrentChild);
      CurrentChild := NewChild;
      if CurrentChild <> nil then
        Add(CurrentChild);
    end;
  end;

begin
  // before rendering, set correct child with correct time
  if not CurrentChildFromPool then
    UpdateChild;
  inherited;
end;

procedure TResourceFrame.SetFrame(const Level: TAbstractLevel;
  const AnAnimation: T3DResourceAnimation;
  const ATime: TFloatTime; const ALoop: boolean);
var
  OldResource, NewResource: T3DResource;
  AnimationChanges: Boolean;
begin
  if FAnimation <> nil then
    OldResource := FAnimation.Owner
  else
    OldResource := nil;
  if AnAnimation <> nil then
    NewResource := AnAnimation.Owner
  else
    NewResource := nil;

  // if changing resource, release previous scene from pool
  if (OldResource <> NewResource) and
     (OldResource <> nil) and
     CurrentChildFromPool then
  begin
    CurrentChildFromPool := false;
    FAnimation.Owner.ReleaseSceneFromPool(CurrentChild);
    Remove(CurrentChild);
    CurrentChild := nil;
  end;

  // change current animation properties
  AnimationChanges := FAnimation <> AnAnimation;
  FAnimation := AnAnimation;
  FTime := ATime;
  FLoop := ALoop;

  if FAnimation <> nil then
    ResourceObserver.Observed := FAnimation.FOwner
  else
    ResourceObserver.Observed := nil;

  // if setting new resource, allocate new scene from pool
  if (not CurrentChildFromPool) and
     (FAnimation <> nil) and
     FAnimation.FOwner.Prepared and
     (FAnimation.FSceneState.Scene = nil) and
     (FAnimation.FOwner.ScenePool <> nil) then
  begin
    CurrentChild := FAnimation.FOwner.AllocateSceneFromPool(Level);
    CurrentChildFromPool := true;
    Add(CurrentChild);
  end;

  // change current animation on scene from pool
  if CurrentChildFromPool and AnimationChanges then
  begin
    if AnAnimation <> nil then
    begin
      if not CurrentChild.PlayAnimation(FAnimation.AnimationName, FLoop) then
        WritelnWarning('Missing animation "%s"', [FAnimation.AnimationName]);
    end else
      CurrentChild.StopAnimation;
  end;
end;

procedure TResourceFrame.ResourceFreeNotification(const Sender: TFreeNotificationObserver);
begin
  { When T3DResource (FAnimation.Owner) is freed, unassociate from FAnimation
    (that is/will be freed alongside), to avoid later crash at TResourceFrame.Destroy.
    The FreeAndNil(Resources) may happen before all creatures are destroyed.
    Testcase: fps_game, crash at game exit. }
  SetFrame(nil, nil, 0, false);
  Assert(FAnimation = nil);
  Assert(not CurrentChildFromPool);
end;

{ T3DResource ---------------------------------------------------------------- }

constructor T3DResource.Create(AOwner: TComponent);
begin
  inherited;
  FFallSpeed := DefaultFallSpeed;
  FGrowSpeed := DefaultGrowSpeed;
  FReceiveShadowVolumes := DefaultReceiveShadowVolumes;
  FCastShadowVolumes := DefaultCastShadowVolumes;
  FAnimations := T3DResourceAnimationList.Create;
  FOrientation := TCastleTransform.DefaultOrientation;
end;

destructor T3DResource.Destroy;
begin
  FPrepared := false;
  ReleaseCore;
  inherited;
  { Freeing FAnimations *after* inherited is necessary,
    because inherited notifies all TResourceFrame about freeing of related T3DResource instance.
    At the point of that notification, TResourceFrame may assume that referenced FAnimation
    still exists.
    Testcase: just run and close fps_game. }
  FreeAndNil(FAnimations);
end;

function T3DResource.CreateSceneForPool(const Params: TPrepareParams): TCastleScene;
begin
  Result := ModelState.Scene.Clone(nil);
  Result.DefaultAnimationTransition := DefaultAnimationTransition;
  Result.PrepareResources(ScenePrepareResources, Params);
end;

procedure T3DResource.PrepareCore(const Params: TPrepareParams;
  const DoProgress: boolean);
var
  I: Integer;
  TimeStart: TCastleProfilerTime;
begin
  TimeStart := Profiler.Start('Prepare Animations of Resource ' + Name);

  ModelState.Prepare(ModelURL, Self, Params, DoProgress);

  if (ModelState.Scene <> nil) and (Pool <> 0) then
  begin
    ScenePool := TCastleSceneList.Create(true);
    ScenePool.Count := Pool;
    for I := 0 to ScenePool.Count - 1 do
      ScenePool[I] := CreateSceneForPool(Params);
    ScenePoolUsed := 0;
  end;

  for I := 0 to Animations.Count - 1 do
    Animations[I].Prepare(Params, DoProgress);

  Profiler.Stop(TimeStart);
end;

function T3DResource.PrepareCoreSteps: Cardinal;
begin
  Result := 2 + Animations.Count * 2;
end;

procedure T3DResource.ReleaseCore;
var
  I: Integer;
begin
  FreeAndNil(ModelState.Scene);
  FreeAndNil(ScenePool);
  ScenePoolUsed := 0;
  if Animations <> nil then
    for I := 0 to Animations.Count - 1 do
      Animations[I].Release;
end;

procedure T3DResource.LoadFromFile(ResourceConfig: TCastleConfig);
var
  I: Integer;
  PoolStr: String;
begin
  ConfigAlwaysPrepared := ResourceConfig.GetValue('always_prepared', false);
  FFallSpeed := ResourceConfig.GetFloat('fall_speed', DefaultFallSpeed);
  FGrowSpeed := ResourceConfig.GetFloat('grow_speed', DefaultGrowSpeed);
  FReceiveShadowVolumes := ResourceConfig.GetValue('receive_shadow_volumes',
    DefaultReceiveShadowVolumes);
  FCastShadowVolumes := ResourceConfig.GetValue('cast_shadow_volumes',
    DefaultCastShadowVolumes);
  FOrientation := StrToOrientationType(ResourceConfig.GetValue('orientation', 'default'));
  FDefaultAnimationTransition := ResourceConfig.GetFloat('model/default_animation_transition', 0.0);
  if ResourceConfig.GetValue('model/file_name', '') <> '' then
  begin
    FModelURL := ResourceConfig.GetURL('model/file_name', true);
    WritelnLog('Deprecated', 'Reading from deprecated "file_name" attribute inside resource.xml. Use "url" instead.');
  end else
    FModelURL := ResourceConfig.GetURL('model/url', true);

  { calculate Pool }
  PoolStr := ResourceConfig.GetValue('model/pool', 'auto');
  if PoolStr = 'auto' then
  begin
    { Do not use Pool for model formats that are
      - fast to switch current frame (ForceAnimationPose is fast)
      - do not support animation blending anyway
      In practice, this means formats with precalculated animation frames, like castle-anim-frames. }
    if (FModelURL <> '') and
       ( (URIMimeType(FModelURL) = 'application/x-castle-anim-frames') or
         (URIMimeType(FModelURL) = 'application/x-md3')
       ) then
      Pool := 0
    else
      Pool := 1;
    WritelnLog('Pool for "%s" auto-determined as %d', [Name, Pool]);
  end else
    Pool := StrToInt(PoolStr);

  for I := 0 to Animations.Count - 1 do
    Animations[I].LoadFromFile(ResourceConfig);
end;

procedure T3DResource.RedoPrepare(const Params: TPrepareParams);
var
  DoProgress: boolean;
begin
  Assert(UsageCount <> 0);
  DoProgress := not Progress.Active;
  if DoProgress then Progress.Init(PrepareCoreSteps, 'Loading ' + Name);
  try
    { It's important to do ReleaseCore after Progress.Init.
      That is because Progress.Init may do TCastleWindow.SaveScreenToDisplayList,
      and this may call Window.OnRender, and this may want to redraw
      the object (e.g. if creature of given resource already exists
      on the screen) and this requires Prepare to be already done.

      So we should call Progress.Init before we make outselves unprepared. }
    FPrepared := false;
    ReleaseCore;
    PrepareCore(Params, DoProgress);
    FPrepared := true;
  finally
    if DoProgress then Progress.Fini;
  end;
end;

procedure T3DResource.Prepare(const Params: TPrepareParams;
  const GravityUp: TVector3);
begin
  Prepare(Params);
end;

procedure T3DResource.Prepare(const Params: TPrepareParams);
var
  List: T3DResourceList;
begin
  List := T3DResourceList.Create(false);
  try
    List.Add(Self);
    List.Prepare(Params);
  finally FreeAndNil(List) end;
end;

procedure T3DResource.Release;
var
  List: T3DResourceList;
begin
  List := T3DResourceList.Create(false);
  try
    List.Add(Self);
    List.Release;
  finally FreeAndNil(List) end;
end;

function T3DResource.AlwaysPrepared: boolean;
begin
  Result := ConfigAlwaysPrepared;
end;

function T3DResource.AllocateSceneFromPool(const Level: TAbstractLevel): TCastleScene;
begin
  Assert(ScenePool <> nil);

  if ScenePoolUsed < ScenePool.Count then
  begin
    Result := ScenePool[ScenePoolUsed];
  end else
  begin
    WritelnLog('Need to increase pool of %s at runtime to %d. Better declare larger initial pool, to avoid delay at game runtime.', [
      Name,
      ScenePoolUsed + 1
    ]);
    Result := CreateSceneForPool(Level.PrepareParams);
    ScenePool.Add(Result);
  end;

  Inc(ScenePoolUsed);
end;

procedure T3DResource.ReleaseSceneFromPool(const Scene: TCastleScene);
var
  I: Integer;
begin
  // Assert(ScenePool <> nil); // possible to happen during destructor
  if ScenePool = nil then
    Exit;

  I := ScenePool.IndexOf(Scene);
  Assert(I <> -1);
  { exchange items on ScenePool, to keep the used scenes at the beginning }
  if I <> ScenePoolUsed - 1 then
    ScenePool.Exchange(I, ScenePoolUsed - 1);
  Dec(ScenePoolUsed);
end;

{ T3DResourceList ------------------------------------------------------------- }

procedure T3DResourceList.AddFromInfo(const FileInfo: TFileInfo; var StopSearch: boolean);
begin
  AddFromFileDefaultReload(FileInfo.URL);
end;

procedure T3DResourceList.AddFromFileDefaultReload(const URL: string);
var
  Xml: TCastleConfig;
  ResourceClassName, ResourceName: string;
  ResourceClass: T3DResourceClass;
  Resource: T3DResource;
begin
  Xml := TCastleConfig.Create(nil);
  try
    try
      Xml.RootName := 'resource';
      Xml.NotModified; { otherwise changing RootName makes it modified, and saved back at freeing }
      Xml.URL := URL;
      // WritelnLog('Resources', Format('Loading T3DResource from "%s"', [URL]));

      ResourceClassName := Xml.GetStringNonEmpty('type');
      if not ResourceClasses.TryGetValue(ResourceClassName, ResourceClass) then
        raise Exception.CreateFmt('Resource type "%s" not found, mentioned in file "%s"',
          [ResourceClassName, URL]);

      ResourceName := Xml.GetStringNonEmpty('name');
      if CharsPos(AllChars - ['a'..'z', 'A'..'Z'], ResourceName) <> 0 then
        raise Exception.CreateFmt('Resource name "%s" is invalid. Resource names may only use English letters (not even digits or underscores are allowed).',
          [ResourceName]);
      Resource := FindName(ResourceName, true);
      if Resource <> nil then
      begin
        if ResourceXmlReload then
        begin
          if ResourceClass <> Resource.ClassType then
            raise Exception.CreateFmt('Resource name "%s" already exists, but with different type. Old class is %s, new class is %s. Cannot reload resource.xml file in this situation',
              [ResourceName, Resource.ClassType.ClassName, ResourceClass.ClassName]);
        end else
          raise Exception.CreateFmt('Resource name "%s" already exists. All resource names inside resource.xml files must be unique',
            [ResourceName]);
      end else
      begin
        Resource := ResourceClass.Create(nil);
        Resource.Name := ResourceName;
        Add(Resource);
      end;

      Resource.LoadFromFile(Xml);
    except
      { enhance EMissingAttribute with information about the XML file where
        it occured }
      on E: EMissingAttribute do
      begin
        E.Message := E.Message + ' (When reading "' + URL + '")';
        raise;
      end;
    end;
  finally FreeAndNil(Xml) end;
end;

procedure T3DResourceList.AddFromFile(const URL: string; const Reload: boolean);
begin
  ResourceXmlReload := Reload;
  AddFromFileDefaultReload(URL);
end;

procedure T3DResourceList.LoadFromFiles(const Path: string; const Reload: boolean);
begin
  if not Reload then
    Clear;
  ResourceXmlReload := Reload;
  FindFiles(Path, 'resource.xml', false,
    {$ifdef FPC}@{$endif}AddFromInfo, [ffRecursive]);
end;

procedure T3DResourceList.LoadFromFiles(const Reload: boolean);
begin
  LoadFromFiles('castle-data:/', Reload);
end;

function T3DResourceList.FindName(const AName: string; const NilWhenNotFound: boolean): T3DResource;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.Name = AName then
      Exit;
  end;

  if NilWhenNotFound then
    Result := nil else
    raise Exception.CreateFmt('Not existing resource name "%s"', [AName]);
end;

procedure T3DResourceList.LoadResources(ParentElement: TDOMElement);
var
  ResourcesElement: TDOMElement;
  ResourceName: string;
  I: TXMLElementIterator;
begin
  Clear;

  ResourcesElement := ParentElement.ChildElement('prepare_resources', false);

  if ResourcesElement <> nil then
  begin
    I := ResourcesElement.ChildrenIterator;
    try
      while I.GetNext do
      begin
        if I.Current.TagName <> 'resource' then
          raise Exception.CreateFmt(
            'Element "%s" is not allowed in <prepare_resources>',
            [I.Current.TagName]);
        if not I.Current.AttributeString('name', ResourceName) then
          raise Exception.Create('<resource> must have a "name" attribute');
        Add(Resources.FindName(ResourceName));
      end;
    finally FreeAndNil(I) end;
  end;
end;

procedure T3DResourceList.Prepare(const Params: TPrepareParams;
  const ResourcesName: string);
var
  I: Integer;
  Resource: T3DResource;
  PrepareSteps: Cardinal;
  PrepareNeeded, DoProgress: boolean;
  TimeStart: TCastleProfilerTime;
begin
  { We iterate two times over Items, first time only to calculate
    PrepareSteps, 2nd time does actual work.
    1st time increments UsageCount (as 2nd pass may be optimized
    out, if not needed). }

  PrepareSteps := 0;
  PrepareNeeded := false;
  for I := 0 to Count - 1 do
  begin
    Resource := Items[I];
    Resource.UsageCount := Resource.UsageCount + 1;
    if Resource.UsageCount = 1 then
    begin
      PrepareSteps := PrepareSteps + Resource.PrepareCoreSteps;
      PrepareNeeded := true;
    end;
  end;

  if PrepareNeeded then
  begin
    TimeStart := Profiler.Start('Loading ' + ResourcesName);

    DoProgress := not Progress.Active;
    if DoProgress then Progress.Init(PrepareSteps, 'Loading ' + ResourcesName);
    try
      for I := 0 to Count - 1 do
      begin
        Resource := Items[I];
        if Resource.UsageCount = 1 then
        begin
          Assert(not Resource.Prepared);
          Resource.PrepareCore(Params, DoProgress);
          Resource.FPrepared := true;
        end;
      end;
    finally
      if DoProgress then Progress.Fini;
    end;

    Profiler.Stop(TimeStart);
  end;
end;

procedure T3DResourceList.Release;
var
  I: Integer;
  Resource: T3DResource;
begin
  for I := 0 to Count - 1 do
  begin
    Resource := Items[I];
    Assert(Resource.UsageCount > 0);
    {
    if Resource.UsageCount = 0 then
    begin
      WritelnWarning('Resources', Format(
        'Resource "%s" is already unused, but it is being released. Make sure T3DResource.Released are matched 1-1 with T3DResource.Prepare calls.', [Resource.Name]));
      Continue;
    end;
    }

    Resource.UsageCount := Resource.UsageCount - 1;
    if Resource.UsageCount = 0 then
    begin
      WritelnLog('Resources', Format(
        'Resource "%s" is no longer used, releasing', [Resource.Name]));
      Resource.FPrepared := false;
      Resource.ReleaseCore;
    end;
  end;
end;

{ resource classes ----------------------------------------------------------- }

procedure RegisterResourceClass(const AClass: T3DResourceClass; const TypeName: string);
begin
  ResourceClasses[TypeName] := AClass;
end;

{ initialization / finalization ---------------------------------------------- }

var
  FResources: T3DResourceList;

function Resources: T3DResourceList;
begin
  if (FResources = nil) and not UnitFinalization then
    FResources := T3DResourceList.Create(true);
  Result := FResources;
end;

initialization // Empty but Delphi needs this

finalization
  UnitFinalization := true;
  FreeAndNil(FResources);
  FreeAndNil(FResourceClasses);
end.
