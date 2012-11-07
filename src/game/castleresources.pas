{
  Copyright 2006-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Manage large 3D resources (scenes, precalculated animations and such)
  that need to be loaded and reference counted. }
unit CastleResources;

interface

uses VectorMath, Classes, CastleXMLConfig, PrecalculatedAnimation,
  CastleScene, X3DNodes, Base3D, DOM, FGL;

const
  DefaultFallSpeed = 10.0;
  DefaultGrowSpeed = 5.0;

type
  T3DResource = class;

  { Animation defined by T3DResource. }
  T3DResourceAnimation = class
  private
    FName: string;
    FRequired: boolean;
  public
    { TODO: for now, the T3DResourceAnimation is just a wrapper around
      TCastlePrecalculatedAnimation. }
    Animation: TCastlePrecalculatedAnimation;
    FileName: string;
    property Name: string read FName;
    property Required: boolean read FRequired;
    constructor Create(const AOwner: T3DResource;
      const AName: string; const ARequired: boolean = true);
  end;

  T3DResourceAnimationList = specialize TFPGObjectList<T3DResourceAnimation>;

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
    ConfigAlwaysPrepared: boolean;
    FFallSpeed, FGrowSpeed: Single;
    FAnimations: T3DResourceAnimationList;

    { Prepare 3D resource loading it from given filename.
      Loads the resource only if filename is not empty,
      and only if it's not already loaded (that is, when Anim = nil).
      Sets rendering attributes and prepares for fast rendering
      and other processing by T3D.PrepareResources.

      Call only in PrepareCore overrides.

      It calls Progress.Step 2 times, if DoProgress.

      Animation is automatically added to our list of prepared 3D resources.
      So it's OpenGL resources will be automatically released in
      @link(GLContextClose), it will be fully released
      in @link(ReleaseCore) and destructor.

      @groupBegin }
    procedure PreparePrecalculatedAnimation(
      var Anim: TCastlePrecalculatedAnimation;
      const AnimationFile: string;
      const BaseLights: TAbstractLightInstancesList;
      const DoProgress: boolean);
    { @groupEnd }
  protected
    { Animations of this resource.

      The first animation, if exists, right now determines the default radius
      calculation. So the first animation should have the bounding box
      representative for all animations.
      Other than that, the order on this list doesn't matter.

      The properties of these animations are automatically loaded from
      resource.xml file in LoadFromFile. The animations are automatically
      prepared / released by our @link(Prepare) / @link(Release) methods. }
    property Animations: T3DResourceAnimationList read FAnimations;

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
    constructor Create(const AName: string); virtual;
    destructor Destroy; override;

    { Are we in a (fully) prepared state. That is after a (fully successfull)
      @link(Prepare) call and before @link(Release).
      Note that this is slightly different than checking @code(UsageCount <> 0):
      in some situations, UsageCount may be non-zero while the preparation
      is not finished yet. This property is guaranteed to be @true only if
      preparation was fully successfully (no exceptions) finished. }
    property Prepared: boolean read FPrepared;

    { Free any association with current OpenGL context. }
    procedure GLContextClose; virtual;

    { Unique identifier of this resource.
      Used to refer to this kind from VRML/X3D models, XML files and other data.

      This must be composed of only letters, use CamelCase.
      (Reason: This must be a valid identifier in all possible languages.
      Also digits and underscore are reserved, as we may use them internally
      for other info in VRML/X3D and XML node names.) }
    property Name: string read FName;

    procedure LoadFromFile(KindsConfig: TCastleConfig); virtual;

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

    { Falling down speed. See T3D.FallSpeed, this works the same,
      except the default value is non-zero, and by default T3D.Gravity
      and T3D.PreferredHeight are already sensible for creatures/items. }
    property FallSpeed: Single
      read FFallSpeed write FFallSpeed default DefaultFallSpeed;

    { See T3D.GrowSpeed, this works the same,
      except the default value is non-zero, and by default T3D.Gravity
      and T3D.PreferredHeight are already sensible for creatures/items. }
    property GrowSpeed: Single
      read FGrowSpeed write FGrowSpeed default DefaultGrowSpeed;
  end;

  T3DResourceClass = class of T3DResource;

  T3DResourceList = class(specialize TFPGObjectList<T3DResource>)
  private
    ResourceXmlReload: boolean;
    procedure LoadResourceXml(const FileName: string);
  public
    { Find resource with given T3DResource.Name.
      @raises Exception if not found and NilWhenNotFound = false. }
    function FindName(const AName: string; const NilWhenNotFound: boolean = false): T3DResource;

    { Load all resources (creatures and items) information from
      resource.xml files found in given Path.
      Overloaded version without Path just scans the whole ProgramDataPath.

      If Reload, then we will not clear the initial list contents.
      Instead, resource.xml files found that refer to the existing id
      will cause T3DResource.LoadFromFile call on an existing resource.
      Using Reload is a nice debug feature, if you want to reload configuration
      from resource.xml files (and eventually add new resources in new resource.xml files),
      but you don't want to recreate existing resource instances.

      @groupBegin }
    procedure LoadFromFiles(const Path: string; const Reload: boolean = false);
    procedure LoadFromFiles(const Reload: boolean = false);
    { @groupEnd }

    { Reads <resources> XML element. <resources> element
      is an optional child of given ParentElement.
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

uses SysUtils, ProgressUnit, CastleXMLUtils, CastleTimeUtils,
  CastleStringUtils, CastleLog, CastleFilesUtils, CastleConfig, UIControls;

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
  AOwner.Animations.Add(Self);
end;

{ T3DResource ---------------------------------------------------------------- }

constructor T3DResource.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FFallSpeed := DefaultFallSpeed;
  FGrowSpeed := DefaultGrowSpeed;
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
  const GravityUp: TVector3Single;
  const DoProgress: boolean);
var
  I: Integer;
begin
  for I := 0 to Animations.Count - 1 do
    PreparePrecalculatedAnimation(Animations[I].Animation, Animations[I].FileName,
      BaseLights, DoProgress);
end;

function T3DResource.PrepareCoreSteps: Cardinal;
begin
  Result := Animations.Count * 2;
end;

procedure T3DResource.ReleaseCore;
var
  I: Integer;
begin
  if Animations <> nil then
    for I := 0 to Animations.Count - 1 do
      FreeAndNil(Animations[I].Animation);
end;

procedure T3DResource.GLContextClose;
var
  I: Integer;
begin
  for I := 0 to Animations.Count - 1 do
    if Animations[I].Animation <> nil then
      Animations[I].Animation.GLContextClose;
end;

procedure T3DResource.LoadFromFile(KindsConfig: TCastleConfig);
var
  I: Integer;
begin
  ConfigAlwaysPrepared := KindsConfig.GetValue('always_prepared', false);
  FFallSpeed := KindsConfig.GetFloat('fall_speed', DefaultFallSpeed);
  FGrowSpeed := KindsConfig.GetFloat('grow_speed', DefaultGrowSpeed);

  for I := 0 to Animations.Count - 1 do
    Animations[I].FileName := KindsConfig.GetFileName(
      'model/' + Animations[I].Name + '/file_name', not Animations[I].Required);
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
      That is because Progress.Init may do TCastleWindowBase.SaveScreenToDisplayList,
      and this may call Window.OnDraw, and this may want to redraw
      the object (e.g. if creature of given kind already exists
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

procedure T3DResource.PreparePrecalculatedAnimation(
  var Anim: TCastlePrecalculatedAnimation;
  const AnimationFile: string;
  const BaseLights: TAbstractLightInstancesList;
  const DoProgress: boolean);
begin
  if (AnimationFile <> '') and (Anim = nil) then
  begin
    Anim := TCastlePrecalculatedAnimation.Create(nil);
    Anim.LoadFromFile(AnimationFile, { AllowStdIn } false, { LoadTime } true);
  end;
  if DoProgress then Progress.Step;

  if Anim <> nil then
    Anim.PrepareResources([prRender, prBoundingBox] + prShadowVolume,
      false, BaseLights);
  if DoProgress then Progress.Step;
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

procedure T3DResourceList.LoadResourceXml(const FileName: string);
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
      Xml.FileName := FileName;
      if Log then
        WritelnLog('Resources', Format('Loading T3DResource from "%s"', [FileName]));

      ResourceClassName := Xml.GetNonEmptyValue('type');
      ResourceClassIndex := ResourceClasses.IndexOf(ResourceClassName);
      if ResourceClassIndex <> -1 then
        ResourceClass := ResourceClasses.Data[ResourceClassIndex] else
        raise Exception.CreateFmt('Resource type "%s" not found, mentioned in file "%s"',
          [ResourceClassName, FileName]);

      ResourceName := Xml.GetNonEmptyValue('name');
      Resource := FindName(ResourceName, true);
      if Resource <> nil then
      begin
        if ResourceXmlReload then
        begin
          if ResourceClass <> Resource.ClassType then
            raise Exception.CreateFmt('Resource id "%s" already exists, but with different type. Old class is %s, new class is %s. Cannot reload resource.xml file in this situation',
              [ResourceName, Resource.ClassType.ClassName, ResourceClass.ClassName]);
        end else
          raise Exception.CreateFmt('Resource id "%s" already exists. All resource ids inside resource.xml files must be unique',
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
        E.Message := E.Message + ' (When reading "' + FileName + '")';
        raise;
      end;
    end;
  finally FreeAndNil(Xml) end;
end;

procedure T3DResourceList.LoadFromFiles(const Path: string; const Reload: boolean);
begin
  if not Reload then
    Clear;
  ResourceXmlReload := Reload;
  ScanForFiles(Path, 'resource.xml', @LoadResourceXml);
end;

procedure T3DResourceList.LoadFromFiles(const Reload: boolean);
begin
  LoadFromFiles(ProgramDataPath, Reload);
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

  ResourcesElement := DOMGetChildElement(ParentElement, 'prepare_resources', false);

  if ResourcesElement <> nil then
  begin
    I := TXMLElementIterator.Create(ResourcesElement);
    try
      while I.GetNext do
      begin
        if I.Current.TagName <> 'resource' then
          raise Exception.CreateFmt(
            'Element "%s" is not allowed in <prepare_resources>',
            [I.Current.TagName]);
        if not DOMGetAttribute(I.Current, 'name', ResourceName) then
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
      TimeBegin := ProcessTimerNow;

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
          ProcessTimerDiff(ProcessTimerNow, TimeBegin) / ProcessTimersPerSec ]));
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

procedure WindowClose(const Container: IUIContainer);
var
  I: Integer;
begin
  { Resources may be nil here, because
    WindowClose may be called from CastleWindow unit finalization
    that will be done after this unit's finalization (DoFinalization).

    That's OK --- DoFinalization already freed
    every item on Resources, and this implicitly did GLContextClose,
    so everything is OK. }

  if Resources <> nil then
  begin
    for I := 0 to Resources.Count - 1 do
      Resources[I].GLContextClose;
  end;
end;

var
  FResources: T3DResourceList;

function Resources: T3DResourceList;
begin
  Result := FResources;
end;

initialization
  OnGLContextClose.Add(@WindowClose);
  FResources := T3DResourceList.Create(true);
  ResourceClasses := TResourceClasses.Create;
finalization
  FreeAndNil(FResources);
  FreeAndNil(ResourceClasses);
end.
