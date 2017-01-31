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

{ Manage large 3D resources (scenes and such)
  that need to be loaded and reference counted. }
unit CastleResources;

{$I castleconf.inc}

interface

uses Classes, DOM, FGL,
  CastleVectors, CastleXMLConfig, CastleTimeUtils,
  CastleScene, X3DNodes, Castle3D, CastleBoxes, CastleFindFiles;

type
  T3DResource = class;

  { Animation defined by T3DResource. }
  T3DResourceAnimation = class
  private
    FName: string;
    FRequired: boolean;
    FOwner: T3DResource;
    FSceneForAnimation: TCastleScene;
    FDuration: Single;
    FURL: string;
    FAnimationName: string;
    procedure Prepare(const BaseLights: TAbstractLightInstancesList;
      const DoProgress: boolean);
    procedure Release;
    procedure LoadFromFile(ResourceConfig: TCastleConfig);
    property Owner: T3DResource read FOwner;
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

    { Current Scene to render for given time.

      Looping is automatically done here, if parameter Loop is @true.
      When it is @false, there is no looping, which means that
      when Time is < 0, we show the first frame,
      and when Time is > @link(Duration), we show the last frame forever.

      This looping (or not looping) is done regardless of whether the 3D model
      wants (or not) looping. For example, in case of castle-anim-frames files,
      we ignore their loop boolean attribute.
      In case of X3D, we ignore TimeSensor.loop field.
      In other words, any looping settings inside 3D model are ignored.
      You control looping fully by the Loop parameter to this method.

      This returns the scene (TCastleScene) with state reflecting given time
      (TimeSensor forced to given time). }
    function Scene(const Time: TFloatTime; const Loop: boolean): TCastleScene;

    { Scene URL, only when each animation is inside a separate 3D file.
      See [http://castle-engine.sourceforge.net/creating_data_resources.php]
      for documentation how you can define creature animations. }
    property URL: string read FURL write FURL;

    { Animation name (like for @link(TCastleSceneCore.PlayAnimation)),
      which is equal to TimeSensor node name.
      All animations are started by X3D TimeSensor node.
      If not given, we assume it's just 'animation', which is fine
      at least for castle-anim-frames and MD3 files loaded using TNodeInterpolator.
      This refers to an X3D TimeSensor node inside
      animation model (from @link(URL)) or, when not defined,
      inside whole resource model (from @link(T3DResource.ModelURL)).

      See [http://castle-engine.sourceforge.net/creating_data_resources.php]
      for documentation how you can define creature animations. }
    property AnimationName: string read FAnimationName write FAnimationName;
    property TimeSensor: string read FAnimationName write FAnimationName;
      deprecated 'use AnimationName';

    property Name: string read FName;
    property Required: boolean read FRequired;
  end;

  T3DResourceAnimationList = class(specialize TFPGObjectList<T3DResourceAnimation>)
    { Find an animation by name.
      @raises Exception if not found. }
    function FindName(const AName: string): T3DResourceAnimation;
  end;

  { Resource used for rendering and processing of 3D objects.
    By itself this doesn't render or do anything.
    But some 3D objects may need to have such resource prepared to work.

    It can also load it's configuration from XML config file.
    For this purpose, it has a unique identifier in @link(Name) property. }
  T3DResource = class
  private
  { Internal design notes: Having resource expressed as
    T3DResource instance, as opposed to overusing dummy T3D instances
    for it, is sometimes good. That's because such resource may be shared by many
    3D objects, may be used for different purposes by various 3D objects
    (e.g. various creatures may be in different state / animation time),
    it's users (3D objects) may not always initially exist on the level
    (e.g. TInventoryItem, that is not even T3D, may refer to it), etc.
    There were ideas to unify T3DResource to be like a T3D descendant
    (or ancestor), but they turned out to cause more confusion (special cases,
    special treatment) than the gain from unification (which would
    be no need of Resources list in TCastleSceneManager, simple
    TCastleSceneManager.Items would suffice.) }

    FName: string;
    FPrepared: boolean;
    FUsageCount: Cardinal;
    FConfigAlwaysPrepared: boolean;
    FFallSpeed, FGrowSpeed: Single;
    FAnimations: T3DResourceAnimationList;
    FReceiveShadowVolumes: boolean;
    FCastShadowVolumes: boolean;
    FModelURL: string;
    { Model loaded from ModelURL }
    Model: TCastleScene;
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
    procedure PrepareCore(const BaseLights: TAbstractLightInstancesList;
      const GravityUp: TVector3Single;
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

    constructor Create(const AName: string); virtual;
    destructor Destroy; override;

    { Are we in a (fully) prepared state. That is after a (fully successfull)
      @link(Prepare) call and before @link(Release).
      Note that this is slightly different than checking @code(UsageCount <> 0):
      in some situations, UsageCount may be non-zero while the preparation
      is not finished yet. This property is guaranteed to be @true only if
      preparation was fully successfully (no exceptions) finished. }
    property Prepared: boolean read FPrepared;

    { Unique identifier of this resource.
      Used to refer to this resource from level placeholders
      (see TGameSceneManager.LoadLevel about placeholders),
      from other XML files (for example one creature may shoot another
      creature as a missile using @link(TWalkAttackCreatureResource.FireMissileName)),
      and in other places.

      This can use only letters, use CamelCase.
      Reason: This must be a valid identifier in both VRML/X3D and ObjectPascal.
      Also digits and underscores are reserved, as we may use them to get other
      information from placeholder names. }
    property Name: string read FName;

    procedure LoadFromFile(ResourceConfig: TCastleConfig); virtual;

    { Release and then immediately prepare again this resource.
      Call only when UsageCount <> 0, that is when resource is prepared.
      Shows nice progress bar, using @link(Progress). }
    procedure RedoPrepare(const BaseLights: TAbstractLightInstancesList;
      const GravityUp: TVector3Single);

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

      @groupBegin }
    procedure Prepare(const BaseLights: TAbstractLightInstancesList;
      const GravityUp: TVector3Single);
    procedure Release;
    { @groupEnd }

    { Place an instance of this resource on World, using information
      from the placeholder on the level. }
    procedure InstantiatePlaceholder(World: T3DWorld;
      const APosition, ADirection: TVector3Single;
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
      read FConfigAlwaysPrepared write FConfigAlwaysPrepared;

    { The speed (in units per second) of falling down because of gravity.
      Note that the gravity direction is controlled by your level 3D model,
      see "Which way is up" section in the engine tutorial
      [http://castle-engine.sourceforge.net/tutorial_up.php].

      Currently, falling down of creatures and items just uses this constant speed.
      In the future, we plan to add properties to control mass and air friction
      and perform more physically-correct simulation of falling down.

      This has no effect for creatures with TCreatureResource.Flying = @true.
      This also has no effect for missile creatures (their
      TCreatureResource.Flying is ignored, they have special approach
      to gravity).

      See T3D.FallSpeed for precise definition, this works the same,
      except our default value is non-zero, and by default T3D.Gravity
      and T3D.PreferredHeight are already sensible for creatures/items. }
    property FallSpeed: Single
      read FFallSpeed write FFallSpeed default DefaultFallSpeed;

    { The speed (in units per second) of growing.

      "Growing" is used to allow non-flying creatures to climb stairs.
      The creature can move whenever a sphere (see TCreatureResource.MiddleHeight
      and TCreatureResource.Radius) can move. This means that part of the bounding
      box (part of the T3DCustomTransform.PreferredHeight) may temporarily
      "sink" into the ground. Then growing, controlled by this property,
      pushes the creature up.

      See T3D.GrowSpeed, this works the same,
      except the default value is non-zero, and by default T3D.Gravity
      and T3D.PreferredHeight are already sensible for creatures/items. }
    property GrowSpeed: Single
      read FGrowSpeed write FGrowSpeed default DefaultGrowSpeed;

    property ReceiveShadowVolumes: boolean
      read FReceiveShadowVolumes write FReceiveShadowVolumes
      default DefaultReceiveShadowVolumes;
    property CastShadowVolumes: boolean
      read FCastShadowVolumes write FCastShadowVolumes
      default DefaultCastShadowVolumes;

    { Model URL, only when you define multiple animations inside
      a single 3D file. See
      [http://castle-engine.sourceforge.net/creating_data_resources.php]
      for notes about <model> element in resource.xml files. }
    property ModelURL: string read FModelURL write FModelURL;
  end;

  T3DResourceClass = class of T3DResource;

  T3DResourceList = class(specialize TFPGObjectList<T3DResource>)
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
      Overloaded version without Path just scans the whole ApplicationData
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
    procedure LoadFromFiles(const Path: string; const Reload: boolean = false);
    procedure LoadFromFiles(const Reload: boolean = false);
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
    procedure Prepare(const BaseLights: TAbstractLightInstancesList;
      const GravityUp: TVector3Single;
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

uses SysUtils,
  CastleProgress, CastleXMLUtils, CastleUtils, CastleSceneCore,
  CastleStringUtils, CastleLog, CastleConfig, CastleApplicationProperties,
  CastleFilesUtils, CastleInternalNodeInterpolator;

type
  TResourceClasses = specialize TFPGMap<string, T3DResourceClass>;
var
  ResourceClasses: TResourceClasses;

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

function T3DResourceAnimation.Scene(const Time: TFloatTime;
  const Loop: boolean): TCastleScene;
var
  Looping: TPlayAnimationLooping;
  GoodAnimationName: string;
begin
  if FSceneForAnimation <> nil then
    Result := FSceneForAnimation else
  if Owner.Model <> nil then
    Result := Owner.Model else
    Result := nil;

  if Result <> nil then
  begin
    if AnimationName <> '' then
      GoodAnimationName := AnimationName else
      GoodAnimationName := TNodeInterpolator.DefaultAnimationName;
    if Loop then
      Looping := paForceLooping else
      Looping := paForceNotLooping;
    Result.ForceAnimationPose(GoodAnimationName, Time, Looping);
  end;
end;

function T3DResourceAnimation.BoundingBox: TBox3D;
begin
  if FSceneForAnimation <> nil then
    Result := FSceneForAnimation.BoundingBox else
  if Owner.Model <> nil then
    Result := Owner.Model.BoundingBox else
    { animation 3D model not loaded }
    Result := EmptyBox3D;
end;

function T3DResourceAnimation.Defined: boolean;
begin
  Result := (URL <> '') or (AnimationName <> '');
end;

procedure T3DResourceAnimation.Prepare(const BaseLights: TAbstractLightInstancesList;
  const DoProgress: boolean);

  { Prepare 3D resource loading it from given URL.
    Loads the resource only if URL is not empty,
    and only if it's not already loaded (that is,
    when Scene = nil).
    Prepares for fast rendering and other processing by T3D.PrepareResources.
    Calls Progress.Step 2 times, if DoProgress. }
  procedure PrepareScene(var Scene: TCastleScene; const URL: string);
  begin
    if (URL <> '') and (Scene = nil) then
    begin
      Scene := TCastleScene.Create(nil);
      Scene.Load(URL);
      Scene.ReceiveShadowVolumes := Owner.ReceiveShadowVolumes;
    end;
    if DoProgress then Progress.Step;

    if Scene <> nil then
      Scene.PrepareResources([prRender, prBoundingBox, prShadowVolume],
        false, BaseLights);
    if DoProgress then Progress.Step;
  end;

begin
  if URL <> '' then
  begin
    PrepareScene(FSceneForAnimation, URL);
    if AnimationName <> '' then
      FDuration := FSceneForAnimation.AnimationDuration(AnimationName)
    else
      FDuration := FSceneForAnimation.AnimationDuration(TNodeInterpolator.DefaultAnimationName);
  end else
  if AnimationName <> '' then
  begin
    if Owner.ModelURL = '' then
      raise Exception.CreateFmt('Animation "%s" of resource "%s": time_sensor is defined, but 3D model url is not defined (neither specific to this animation nor containing multiple animations)',
        [Name, Owner.Name]);
    PrepareScene(Owner.Model, Owner.ModelURL);
    FDuration := Owner.Model.AnimationDuration(AnimationName);
  end else
  if Required then
    raise Exception.CreateFmt('No definition for required animation "%s" of resource "%s". You have to define url or time_sensor for this animation in appropriate resource.xml file',
      [Name, Owner.Name]);
end;

procedure T3DResourceAnimation.Release;
begin
  FreeAndNil(FSceneForAnimation);
end;

procedure T3DResourceAnimation.LoadFromFile(ResourceConfig: TCastleConfig);
begin
  if ResourceConfig.GetValue('model/' + Name + '/file_name', '') <> '' then
  begin
    URL := ResourceConfig.GetURL('model/' + Name + '/file_name', true);
    WritelnWarning('Deprecated', 'Reading from deprecated "file_name" attribute inside resource.xml. Use "url" instead.');
  end else
    URL := ResourceConfig.GetURL('model/' + Name + '/url', true);
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

{ T3DResource ---------------------------------------------------------------- }

constructor T3DResource.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FFallSpeed := DefaultFallSpeed;
  FGrowSpeed := DefaultGrowSpeed;
  FReceiveShadowVolumes := DefaultReceiveShadowVolumes;
  FCastShadowVolumes := DefaultCastShadowVolumes;
  FAnimations := T3DResourceAnimationList.Create;
end;

destructor T3DResource.Destroy;
begin
  FPrepared := false;
  ReleaseCore;
  FreeAndNil(FAnimations);
  inherited;
end;

procedure T3DResource.PrepareCore(const BaseLights: TAbstractLightInstancesList;
  const GravityUp: TVector3Single; const DoProgress: boolean);
var
  I: Integer;
begin
  for I := 0 to Animations.Count - 1 do
    Animations[I].Prepare(BaseLights, DoProgress);
end;

function T3DResource.PrepareCoreSteps: Cardinal;
begin
  Result := Animations.Count * 2;
end;

procedure T3DResource.ReleaseCore;
var
  I: Integer;
begin
  if Model <> nil then
    FreeAndNil(Model);
  if Animations <> nil then
    for I := 0 to Animations.Count - 1 do
      Animations[I].Release;
end;

procedure T3DResource.LoadFromFile(ResourceConfig: TCastleConfig);
var
  I: Integer;
begin
  ConfigAlwaysPrepared := ResourceConfig.GetValue('always_prepared', false);
  FFallSpeed := ResourceConfig.GetFloat('fall_speed', DefaultFallSpeed);
  FGrowSpeed := ResourceConfig.GetFloat('grow_speed', DefaultGrowSpeed);
  FReceiveShadowVolumes := ResourceConfig.GetValue('receive_shadow_volumes',
    DefaultReceiveShadowVolumes);
  FCastShadowVolumes := ResourceConfig.GetValue('cast_shadow_volumes',
    DefaultCastShadowVolumes);
  if ResourceConfig.GetValue('model/file_name', '') <> '' then
  begin
    FModelURL := ResourceConfig.GetURL('model/file_name', true);
    WritelnLog('Deprecated', 'Reading from deprecated "file_name" attribute inside resource.xml. Use "url" instead.');
  end else
    FModelURL := ResourceConfig.GetURL('model/url', true);

  for I := 0 to Animations.Count - 1 do
    Animations[I].LoadFromFile(ResourceConfig);
end;

procedure T3DResource.RedoPrepare(const BaseLights: TAbstractLightInstancesList;
  const GravityUp: TVector3Single);
var
  DoProgress: boolean;
begin
  Assert(UsageCount <> 0);
  DoProgress := not Progress.Active;
  if DoProgress then Progress.Init(PrepareCoreSteps, 'Loading ' + Name);
  try
    { It's important to do ReleaseCore after Progress.Init.
      That is because Progress.Init may do TCastleWindowCustom.SaveScreenToDisplayList,
      and this may call Window.OnRender, and this may want to redraw
      the object (e.g. if creature of given resource already exists
      on the screen) and this requires Prepare to be already done.

      So we should call Progress.Init before we make outselves unprepared. }
    FPrepared := false;
    ReleaseCore;
    PrepareCore(BaseLights, GravityUp, DoProgress);
    FPrepared := true;
  finally
    if DoProgress then Progress.Fini;
  end;
end;

procedure T3DResource.Prepare(const BaseLights: TAbstractLightInstancesList;
  const GravityUp: TVector3Single);
var
  List: T3DResourceList;
begin
  List := T3DResourceList.Create(false);
  try
    List.Add(Self);
    List.Prepare(BaseLights, GravityUp);
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

{ T3DResourceList ------------------------------------------------------------- }

procedure T3DResourceList.AddFromInfo(const FileInfo: TFileInfo; var StopSearch: boolean);
begin
  AddFromFileDefaultReload(FileInfo.URL);
end;

procedure T3DResourceList.AddFromFileDefaultReload(const URL: string);
var
  Xml: TCastleConfig;
  ResourceClassName, ResourceName: string;
  ResourceClassIndex: Integer;
  ResourceClass: T3DResourceClass;
  Resource: T3DResource;
begin
  Xml := TCastleConfig.Create(nil);
  try
    try
      Xml.RootName := 'resource';
      Xml.NotModified; { otherwise changing RootName makes it modified, and saved back at freeing }
      Xml.URL := URL;
      if Log then
        WritelnLog('Resources', Format('Loading T3DResource from "%s"', [URL]));

      ResourceClassName := Xml.GetStringNonEmpty('type');
      ResourceClassIndex := ResourceClasses.IndexOf(ResourceClassName);
      if ResourceClassIndex <> -1 then
        ResourceClass := ResourceClasses.Data[ResourceClassIndex] else
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
        Resource := ResourceClass.Create(ResourceName);
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
  FindFiles(Path, 'resource.xml', false, @AddFromInfo, [ffRecursive]);
end;

procedure T3DResourceList.LoadFromFiles(const Reload: boolean);
begin
  LoadFromFiles(ApplicationData(''), Reload);
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

procedure T3DResourceList.Prepare(const BaseLights: TAbstractLightInstancesList;
  const GravityUp: TVector3Single;
  const ResourcesName: string);
var
  I: Integer;
  Resource: T3DResource;
  PrepareSteps: Cardinal;
  TimeBegin: TProcessTimerResult;
  PrepareNeeded, DoProgress: boolean;
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
      PrepareSteps += Resource.PrepareCoreSteps;
      PrepareNeeded := true;
    end;
  end;

  if PrepareNeeded then
  begin
    if Log then
      TimeBegin := ProcessTimer;

    DoProgress := not Progress.Active;
    if DoProgress then Progress.Init(PrepareSteps, 'Loading ' + ResourcesName);
    try
      for I := 0 to Count - 1 do
      begin
        Resource := Items[I];
        if Resource.UsageCount = 1 then
        begin
          if Log then
            WritelnLog('Resources', Format(
              'Resource "%s" becomes used, preparing', [Resource.Name]));
          Assert(not Resource.Prepared);
          Resource.PrepareCore(BaseLights, GravityUp, DoProgress);
          Resource.FPrepared := true;
        end;
      end;
    finally
      if DoProgress then Progress.Fini;
    end;

    if Log then
      WritelnLog('Resources', Format('Loading %s time: %f seconds',
        [ ResourcesName,
          ProcessTimerSeconds(ProcessTimer, TimeBegin) ]));
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

    Resource.UsageCount := Resource.UsageCount - 1;
    if Resource.UsageCount = 0 then
    begin
      if Log then
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
  Result := FResources;
end;

initialization
  FResources := T3DResourceList.Create(true);
  ResourceClasses := TResourceClasses.Create;
finalization
  FreeAndNil(FResources);
  FreeAndNil(ResourceClasses);
end.
