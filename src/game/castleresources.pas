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

{ Manage large 3D resources (scenes, precalculated animations and such)
  that need to be loaded and reference counted. }
unit CastleResources;

interface

uses VectorMath, Classes, CastleXMLConfig, PrecalculatedAnimation,
  CastleScene, X3DNodes, Base3D, DOM, FGL;

type
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
    (e.g. TItem, that is not even T3D, may refer to it), etc.
    There were ideas to unify T3DResource to be like a T3D descendant
    (or ancestor), but they turned out to cause more confusion (special cases,
    special treatment) than the gain from unification (which would
    be no need of Resources list in TCastleSceneManager, simple
    TCastleSceneManager.Items would suffice.) }

    FName: string;
    FPrepared: boolean;
    Allocated: T3DListCore;
    FUsageCount: Cardinal;
  protected
    { Prepare 3D resource loading it from given filename.
      Loads the resource only if filename is not empty,
      and only if it's not already loaded (that is, when Anim = nil).
      Sets rendering attributes and prepares for fast rendering
      and other processing by T3D.PrepareResources.

      Call only in PrepareCore overrides.

      It calls Progress.Step 2 times, if DoProgress.

      Animation or Scene is automatically added to our list of prepared
      3D resources.
      So it's OpenGL resources will be automatically released in
      @link(GLContextClose), it will be fully released
      in @link(ReleaseCore) and destructor.

      @groupBegin }
    procedure PreparePrecalculatedAnimation(
      var Anim: TCastlePrecalculatedAnimation;
      const AnimationFile: string;
      const BaseLights: TAbstractLightInstancesList;
      const DoProgress: boolean);
    procedure PrepareScene(
      var Scene: TCastleScene;
      const SceneFileName: string;
      const BaseLights: TAbstractLightInstancesList;
      const DoProgress: boolean);
    { @groupEnd }

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
    procedure RedoPrepare(const BaseLights: TAbstractLightInstancesList);

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
    procedure Prepare(const BaseLights: TAbstractLightInstancesList);
    procedure Release;
    { @groupEnd }

    { Place an instance of this resource on World, using information
      from the placeholder on the level. }
    procedure InstantiatePlaceholder(World: T3DWorld;
      const APosition, ADirection: TVector3Single;
      const NumberPresent: boolean; const Number: Int64); virtual; abstract;
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
      resources (subset of AllResources). }
    procedure LoadResources(ParentElement: TDOMElement);

    { Prepare / release all resources on list.
      @groupBegin }
    procedure Prepare(const BaseLights: TAbstractLightInstancesList;
      const ResourcesName: string = 'resources');
    procedure Release;
    { @groupEnd }
  end;

var
  AllResources: T3DResourceList;

{ Register a class, to allow user to create resource (like a creature or item)
  of this class by using appropriate type="xxx" inside resource.xml file. }
procedure RegisterResourceClass(const AClass: T3DResourceClass; const TypeName: string);

implementation

uses SysUtils, ProgressUnit, CastleGameCache, CastleXMLUtils, CastleTimeUtils,
  CastleStringUtils, CastleLog, CastleFilesUtils, CastleConfig, UIControls;

type
  TResourceClasses = specialize TFPGMap<string, T3DResourceClass>;
var
  ResourceClasses: TResourceClasses;

{ T3DResource ---------------------------------------------------------------- }

constructor T3DResource.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  Allocated := T3DListCore.Create(true, nil);
end;

destructor T3DResource.Destroy;
begin
  FPrepared := false;
  ReleaseCore;
  FreeAndNil(Allocated);
  inherited;
end;

procedure T3DResource.PrepareCore(const BaseLights: TAbstractLightInstancesList;
  const DoProgress: boolean);
begin
end;

function T3DResource.PrepareCoreSteps: Cardinal;
begin
  Result := 0;
end;

procedure T3DResource.ReleaseCore;
begin
  if Allocated <> nil then
  begin
    { since Allocated owns all it's items, this is enough to free them }
    Allocated.Clear;
  end;
end;

procedure T3DResource.GLContextClose;
var
  I: Integer;
begin
  for I := 0 to Allocated.Count - 1 do
    Allocated[I].GLContextClose;
end;

procedure T3DResource.LoadFromFile(KindsConfig: TCastleConfig);
begin
  { Nothing to do in this class. }
end;

procedure T3DResource.RedoPrepare(const BaseLights: TAbstractLightInstancesList);
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
    PrepareCore(BaseLights, DoProgress);
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
    Anim := TCastlePrecalculatedAnimation.CreateCustomCache(nil, GLContextCache);
    Allocated.Add(Anim);
    Anim.LoadFromFile(AnimationFile, { AllowStdIn } false, { LoadTime } true);
  end;
  if DoProgress then Progress.Step;

  if Anim <> nil then
    Anim.PrepareResources([prRender, prBoundingBox] + prShadowVolume,
      false, BaseLights);
  if DoProgress then Progress.Step;
end;

procedure T3DResource.PrepareScene(
  var Scene: TCastleScene;
  const SceneFileName: string;
  const BaseLights: TAbstractLightInstancesList;
  const DoProgress: boolean);
begin
  if (SceneFileName <> '') and (Scene = nil) then
  begin
    Scene := TCastleScene.CreateCustomCache(nil, GLContextCache);
    Allocated.Add(Scene);
    Scene.Load(SceneFileName);
  end;
  if DoProgress then Progress.Step;

  if Scene <> nil then
    Scene.PrepareResources([prRender, prBoundingBox] + prShadowVolume,
      false, BaseLights);
  if DoProgress then Progress.Step;
end;

procedure T3DResource.Prepare(const BaseLights: TAbstractLightInstancesList);
var
  List: T3DResourceList;
begin
  List := T3DResourceList.Create(false);
  try
    List.Add(Self);
    List.Prepare(BaseLights);
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

  ResourcesElement := DOMGetChildElement(ParentElement, 'resources', false);

  if ResourcesElement <> nil then
  begin
    I := TXMLElementIterator.Create(ResourcesElement);
    try
      while I.GetNext do
      begin
        if I.Current.TagName <> 'resource' then
          raise Exception.CreateFmt(
            'Element "%s" is not allowed in <resources>',
            [I.Current.TagName]);
        if not DOMGetAttribute(I.Current, 'name', ResourceName) then
          raise Exception.Create('<resource> must have a "name" attribute');
        Add(AllResources.FindName(ResourceName));
      end;
    finally FreeAndNil(I) end;
  end;
end;

procedure T3DResourceList.Prepare(const BaseLights: TAbstractLightInstancesList;
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
          Resource.PrepareCore(BaseLights, DoProgress);
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
  { AllResources may be nil here, because
    WindowClose may be called from CastleWindow unit finalization
    that will be done after this unit's finalization (DoFinalization).

    That's OK --- DoFinalization already freed
    every item on AllResources, and this implicitly did GLContextClose,
    so everything is OK. }

  if AllResources <> nil then
  begin
    for I := 0 to AllResources.Count - 1 do
      AllResources[I].GLContextClose;
  end;
end;

initialization
  OnGLContextClose.Add(@WindowClose);
  AllResources := T3DResourceList.Create(true);
  ResourceClasses := TResourceClasses.Create;
finalization
  FreeAndNil(AllResources);
  FreeAndNil(ResourceClasses);
end.
