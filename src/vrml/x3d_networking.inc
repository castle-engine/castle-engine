{
  Copyright 2002-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { }
  TNodeX3DNetworkSensorNode = class(TNodeX3DSensorNode)
  public
    procedure CreateNode; override;
  end;

  INodeX3DUrlObject = interface(IVRMLNode)
  ['{668420DB-20F7-4A5E-BB89-52F29CA39057}']
    property FdUrl: TMFString { read GetFdurl }; { }
  end;

  TVRMLViewpointNode = class;

  TNodeAnchor = class(TNodeX3DGroupingNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdDescription: TSFString;
    public property FdDescription: TSFString read FFdDescription;

    private FFdParameter: TMFString;
    public property FdParameter: TMFString read FFdParameter;

    private FFdUrl: TMFString;
    public property FdUrl: TMFString read FFdUrl;

    { Load the new world and viewpoint referenced by this Anchor node.

      This iterates over url field, for each URL trying to load
      referenced file. If URL specifies viewpoint name, we also search
      for viewpoint within this file.
      If it will succeed for some URL, returns @true,
      setting output parameters. Returns @false if no loadable URL is found.

      Output NewRootNode is the loaded world,
      or @nil if actually we should stay within the same world.
      @nil happens for URLs like @code(#ViewpointName), see X3D spec.

      NewViewpoint is the new viewpoint that should be bound,
      or @nil if URL doesn't specify viewpoint name.
      This viewpoint is part of NewRootNode graph, if NewRootNode <> @nil,
      or is part of CurrentRootNode if we should stay within
      current world (NewRootNode = @nil).

      Note that this means that NewRootNode = nil and
      NewViewpoint = nil are valid output from this method,
      and indicate that nothing should be done.
      And this is Ok, following X3D spec AFAIK.

      CurrentRootNode is required to detect invalid viewpoint names
      within current file. An explanation why this is needed follows:

      Invalid viewpoint names within newly loaded
      files are harmless, that is newly loaded file is accepted anyway
      and invalid viewpoint name is ignored. X3D spec says this explicitly.

      But what should happen for invalid viewpoint names within current file?
      X3D spec doesn't say anything clear here (the part before "#" is
      valid in this case, since it just indicates current file, so someone
      could argue that URL is valid, only viewpoint name is wrong and
      so it should be ignored...). Consider url field like

@preformatted(
  [ "#NotExistingViewpoint", "#ExistingViewpoint" ]
)

      It seems sensible that we detect "#NotExistingViewpoint" as invalid,
      and move on to the next URL (which may be local or not).
      That's why we need CurrentRootNode instance.
      When CurrentRootNode = nil, we understand that no file is currently
      loaded (at least, you don't have it's VRML graph). It means that
      any "#Viewpoint" within current model is considered invalid. }
    function LoadAnchor(
      out NewRootNode: TVRMLRootNode;
      out NewViewpoint: TVRMLViewpointNode;
      CurrentRootNode: TVRMLRootNode): boolean;
  end;

  { Inline node.

    The loaded inline contents are accessible in the @link(Inlined) object.
    Currently this is also kept in the Children list
    of this node. Originally, Children list is for VRML 1.0 children,
    but for inline node this is just used for inlined content (regardless
    of VRML/X3D version).

    This means that most methods, like TVRMLNode.EnumerateNodes,
    descend into inlined content normally. }
  TNodeInline = class(TVRMLGroupingNode, INodeX3DBoundedObject, INodeX3DUrlObject)
  private
    procedure EventLoadReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
    procedure EventUrlReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
  private
    { Is the inline currently loaded. This means "loaded",
      not "successfully loaded": when Loaded := true,
      we condider "url" field as handled, and loaded *if possible*.
      If "url" didn't contain anything useful, Loaded is still set to @true,
      to avoid constantly trying to reload. }
    Loaded: boolean;
    FInlined: TVRMLNode;
  protected
    procedure BeforeTraverse(StateStack: TVRMLGraphTraverseStateStack); override;
    procedure AfterTraverse(StateStack: TVRMLGraphTraverseStateStack); override;
    { You can override this to do anything after the Inline children were
      loaded. It's called only after actual loading occurred, e.g. it's not
      called when LoadInlined was called on the already-loaded node with
      CanReload = @false. }
    procedure AfterLoadInlined; virtual;
    function DeepCopyCore(CopyState: TVRMLNodeDeepCopyState): TVRMLNode; override;
    procedure ParseAfter(Names: TVRMLNames); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdLoad: TSFBool;
    public property FdLoad: TSFBool read FFdLoad;

    private FFdUrl: TMFString;
    public property FdUrl: TMFString read FFdUrl;

    private FFdBboxCenter: TSFVec3f;
    public property FdBboxCenter: TSFVec3f read FFdBboxCenter;

    private FFdBboxSize: TSFVec3f;
    public property FdBboxSize: TSFVec3f read FFdBboxSize;

    class function VRML1ChildrenSaveToStream: boolean; override;

    { Load inlined VRML content @bold(now).
      If @link(Inlined) is already loaded:
      if CanReload = @true then it will be freed and loaded again,
      else (if CanReload = @false) nothing will happen.

      Note that this has a really simple support for "load" field
      (this concerns VRML 97 amendment 1 "InlineLoadControl.load" and
      X3D (actually used also in VRML 97 handling) "Inline.load" fields).
      It simply doesn't do any loading when "load" = @false.

      Note that this doesn't perform setting the "load" field,
      or sending any notifications to Scene
      about "load" field. It's the caller's job to keep loaded state
      synchronized with "load" field value.

      LoadInlined(false) will be called automatically, to ensure the inlined
      contents are always loaded. It will be called at the end of Parse
      (this is needed to handle VRML/X3D IMPORT/EXPORT mechanism),
      it will also be called in BeforeTraverse (in case you constructed node
      by code, not by parsing).

      @param(Exported If non-nil, we will assign here node names
        exported from the file. Used to handle IMPORT/EXPORT X3D mechanism.) }
    procedure LoadInlined(CanReload: boolean; Exported: TVRMLNodeNames = nil);

    procedure UnLoadInlined;

    { Call this to indicate that you loaded the inline contents directly,
      by using AddChild yourself. }
    procedure LoadedInlineDirectly;

    { The inlined VRML content (loaded from inlined url).
      @nil if not yet loaded, or no loading was successful.

      This is also kept in VRML 1.0 children as Children[0],
      but for future it's better to not depend on that. }
    property Inlined: TVRMLNode read FInlined;
  end;

  { InlineLoadControl (VRML 97 node).

    Note that our Inline node conforms to X3D, and so our Inline already
    has a "load" field. So InlineLoadControl doesn't really have much use...
    InlineLoadControl is implemented mostly for compatibility with VRML 97.
    It has additional "children" output field. }
  TNodeInlineLoadControl = class(TNodeInline)
  protected
    procedure AfterLoadInlined; override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFNode, out } { }
    private FEventChildren: TVRMLEvent;
    public property EventChildren: TVRMLEvent read FEventChildren;
  end;

  TNodeLoadSensor = class(TNodeX3DNetworkSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdTimeOut: TSFTime;
    public property FdTimeOut: TSFTime read FFdTimeOut;

    private FFdWatchList: TMFNode;
    public property FdWatchList: TMFNode read FFdWatchList;

    { Event: SFBool, out } { }
    private FEventIsLoaded: TVRMLEvent;
    public property EventIsLoaded: TVRMLEvent read FEventIsLoaded;

    { Event: SFTime, out } { }
    private FEventLoadTime: TVRMLEvent;
    public property EventLoadTime: TVRMLEvent read FEventLoadTime;

    { Event: SFFloat, out } { }
    private FEventProgress: TVRMLEvent;
    public property EventProgress: TVRMLEvent read FEventProgress;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TNodeX3DNetworkSensorNode.CreateNode;
begin
  inherited;

  DefaultContainerField := 'children';
end;

procedure TNodeAnchor.CreateNode;
begin
  inherited;

  FFdDescription := TSFString.Create(Self, 'description', '');
  Fields.Add(FFdDescription);

  FFdParameter := TMFString.Create(Self, 'parameter', []);
  Fields.Add(FFdParameter);

  FFdUrl := TMFString.Create(Self, 'url', []);
  Fields.Add(FFdUrl);
  { X3D specification comment: [url or urn] }

  DefaultContainerField := 'children';
end;

class function TNodeAnchor.ClassNodeTypeName: string;
begin
  Result := 'Anchor';
end;

class function TNodeAnchor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeAnchor.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
var
  I: Integer;
begin
  for I := 0 to FdChildren.Count - 1 do
    Func(Self, FdChildren.Items[I]);
end;

function TNodeAnchor.LoadAnchor(
  out NewRootNode: TVRMLRootNode;
  out NewViewpoint: TVRMLViewpointNode;
  CurrentRootNode: TVRMLRootNode): boolean;

  procedure ViewpointNameInvalid(const NewViewpointName: string);
  begin
    OnWarning(wtMajor, 'VRML/X3D', Format('Viewpoint "%s" specified by Anchor not found',
      [NewViewpointName]));
  end;

var
  I: Integer;
  FullUrl, NewViewpointName: string;
begin
  for I := 0 to FdUrl.Count - 1 do
  begin
    FullUrl := FdUrl.Items[I];

    { We have to call URLExtractAnchor first, to be able to detect
      cases when the URL before # doesn't exist (so NewRootNode = nil
      and exit with success). Otherwise, PathFromWWWBasePath would append
      current path to empty URL, making it non-empty (and invalid...). }
    URLExtractAnchor(FullUrl, NewViewpointName);

    if FullUrl = '' then
    begin
      NewRootNode := nil;

      if NewViewpointName = '' then
      begin
        NewViewpoint := nil;
        Exit(true);
      end else
      begin
        if CurrentRootNode <> nil then
        begin
          NewViewpoint := CurrentRootNode.TryFindNodeByName(TVRMLViewpointNode,
            NewViewpointName, true) as TVRMLViewpointNode;
          if NewViewpoint <> nil then
            Exit(true) else
            ViewpointNameInvalid(NewViewpointName);
        end else
          ViewpointNameInvalid(NewViewpointName);

        { In this case invalid viewpoint name is treated as a failure,
          that is we try to find next URL.
          This happens when CurrentRootNode = nil or when
          CurrentRootNode doesn't contain NewViewpointName. }
      end;

    end else
    begin
      FullUrl := PathFromWWWBasePath(FullUrl);
      try
        NewRootNode := LoadVRML(FullUrl, false);
      except
        on E: Exception do
        begin
          { Catch exception and produce OnWarning, go to next URL }
          OnWarning(wtMajor, 'VRML/X3D', Format('Loading world from Anchor URL "%s" failed: %s',
            [FullUrl, E.Message]));
          Continue;
        end;
      end;

      if NewViewpointName = '' then
      begin
        NewViewpoint := nil;
      end else
      begin
        NewViewpoint := NewRootNode.TryFindNodeByName(TVRMLViewpointNode,
          NewViewpointName, true) as TVRMLViewpointNode;
        if NewViewpoint = nil then
          ViewpointNameInvalid(NewViewpointName);

        { In this case invalid viewpoint name is ignored,
          that is we still accept this URL. This follows X3D spec
          (also, this is sensible, otherwise we would have to free
          loaded model just becuase viewpoint is invalid). }
      end;

      Exit(true);
    end;
  end;
  Result := false;
end;

procedure TNodeInline.CreateNode;
begin
  inherited;

  FFdLoad := TSFBool.Create(Self, 'load', true);
   FdLoad.OnReceive.Add(@EventLoadReceive);
  Fields.Add(FFdLoad);

  FFdUrl := TMFString.Create(Self, 'url', []);
   FdUrl.OnReceive.Add(@EventUrlReceive);
  Fields.Add(FFdUrl);
  { X3D specification comment: [url or urn] }

  FFdBboxCenter := TSFVec3f.Create(Self, 'bboxCenter', Vector3Single(0, 0, 0));
   FdBboxCenter.Exposed := false;
  Fields.Add(FFdBboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdBboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
   FdBboxSize.Exposed := false;
  Fields.Add(FFdBboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }

  DefaultContainerField := 'children';

  { To allow inlined children (they are loaded as VRML 1.0 style children) }
  VRML1ChildrenAllowed := true;
  VRML1ChildrenParsingAllowed := false;

  FInlined := nil;
end;

class function TNodeInline.ClassNodeTypeName: string;
begin
  Result := 'Inline';
end;

class function TNodeInline.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeInline.ParseAfter(Names: TVRMLNames);
var
  Exported: TVRMLNodeNames;
begin
  inherited;

  if NodeName <> '' then
    Exported := TVRMLNodeNames.Create(false) else
    Exported := nil;

  LoadInlined(false, Exported);

  { do not add Exported to Importable list, if nothing was exported.
    This just saves the size of the Importable list. }
  if (Exported <> nil) and (Exported.Count = 0) then
    FreeAndNil(Exported);

  if Exported <> nil then
  begin
    { connect Exported nodes with our inline NodeName }
    Assert(NodeName <> '');
    Names.Importable.Bind(NodeName, Exported);
  end;
end;

procedure TNodeInline.UnLoadInlined;
begin
  VRML1ChildrenClear;
  FInlined := nil;
  Loaded := false;
end;

procedure TNodeInline.LoadInlined(CanReload: boolean; Exported: TVRMLNodeNames = nil);

  { The Inlined contents are usually TVRMLRootNode,
    but they should not be saved as such. Although usually this doesn't
    get saved (since we do not save inline contents), but it may happen
    if you copy VRML nodes around, e.g. "expanding" inlines like
    castle-process-3d-model. }
  procedure InlinedIsNotRootNode;
  begin
    if FInlined is TVRMLRootNode then
      (FInlined as TVRMLRootNode).SaveAsRootNode := false;
  end;

var
  I: Integer;
  FullUrl: string;
begin
  if Exported <> nil then Exported.Clear;

  if Loaded then
  begin
    if CanReload then UnLoadInlined else Exit;
  end;

  Assert(not Loaded);
  Assert(FInlined = nil);
  Assert(VRML1ChildrenCount = 0);

  if not FdLoad.Value then Exit;

  FInlined := nil;

  for I := 0 to FdUrl.Items.Count - 1 do
    { For VRML 1.0 WWWInline, an item on "url" (alias "name") field
      may be empty, and should be nicely ignored then.
      For VRML >= 2.0 we also can ignore it (otherwise confusing warning
      about unrecognized 3D model file extension for dir name is done). }
    if FdUrl.Items[I] <> '' then
    begin
      FullUrl := PathFromWWWBasePath(FdUrl.Items[I]);
      try
        FInlined := LoadVRML(FullUrl, false);
        if (Exported <> nil) and
           (TVRMLRootNode(FInlined).ExportedNames <> nil) then
          Exported.Assign(TVRMLRootNode(FInlined).ExportedNames);
        Break;
      except
        on E: Exception do
          { Careful here, as OnWarning may also raise exception. }
          OnWarning(wtMinor, 'VRML/X3D', Format(SLoadError,
            [E.ClassName, 'inline file', FullUrl, E.Message]));
      end;
    end;

  if FInlined <> nil then
  begin
    InlinedIsNotRootNode;
    VRML1ChildAdd(FInlined);
    AfterLoadInlined;
  end;

  { Whether or not we successfully got FInlined <> nil, we always set
    Loaded to true here. This way we will not try to reload this inline
    at next LoadInlined(false) (this would be bad, as we already did
    OnWarning in case loading failed and such). }
  Loaded := true;
end;

procedure TNodeInline.LoadedInlineDirectly;
begin
  Loaded := true;
  if VRML1ChildrenCount <> 0 then
    FInlined := VRML1Children[0] else
    FInlined := nil;
end;

function TNodeInline.DeepCopyCore(CopyState: TVRMLNodeDeepCopyState): TVRMLNode;
var
  ResultI: TNodeInline;
begin
  Result := inherited DeepCopyCore(CopyState);
  ResultI := Result as TNodeInline;

  { Since inherited copied our Children, we should also set "Loaded"
    and "Inlined" field to the same value. }
  ResultI.Loaded := Loaded;
  if ResultI.VRML1ChildrenCount <> 0 then
    ResultI.FInlined := ResultI.VRML1Children[0] else
    ResultI.FInlined := nil;
end;

procedure TNodeInline.AfterLoadInlined;
begin
  { Nothing to do in this class. }
end;

procedure TNodeInline.BeforeTraverse(StateStack: TVRMLGraphTraverseStateStack);
begin
  inherited;

  try
    LoadInlined(false);
  except
    { LoadInlined may easily fail with exception, it's imprtant in such case
      to still do AfterTraverse, to make "inherited AfterTraverse",
      to finalize what "inherited BeginTraverse" did: usually StateStack.Pop. }
    inherited AfterTraverse(StateStack);
    raise;
  end;

  Inc(StateStack.Top.InsideInline);
end;

procedure TNodeInline.AfterTraverse(StateStack: TVRMLGraphTraverseStateStack);
begin
  Dec(StateStack.Top.InsideInline);
  inherited;
end;

class function TNodeInline.VRML1ChildrenSaveToStream: boolean;
begin
  Result := false;
end;

procedure TNodeInline.EventLoadReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  { Changing the "load" and "url" fields causes LoadInlined or UnLoadInlined,
    so the VRML graph changes, so they deserve ChangedAll.

    Note that, in theory, actually LoadInlined and UnLoadInlined
    should already call Scene.ChangedAll already.
    But this opens a can of worms, because our BeforeTraverse also
    calls LoadInlined. And you can't really reliably call ChangedAll
    in the middle of traversing (since this means you may call ChangedAll
    in the middle of ChangedAll, for which we're not ready).

    Moreover, we need to use BeforeNodesFree. Otherwise our UnLoadInlined
    will free items to which existing TVRMLScene / TVRMLShape may keep
    references. Testcase: inline_load_control_test_1.x3dv,
    with --debug-log-vrml-changes,
    press ctrl then shift, without BeforeNodesFree will fail ->
    because OriginalGeometry.ClassName is no longer available. }

  if Scene <> nil then Scene.BeforeNodesFree;

  if FdLoad.Value then
    LoadInlined(false) else
    UnLoadInlined;

  if Scene <> nil then Scene.ChangedAll;
end;

procedure TNodeInline.EventUrlReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  if FdLoad.Value then
  begin
    if Scene <> nil then Scene.BeforeNodesFree;
    UnLoadInlined;
    LoadInlined(false);
    if Scene <> nil then Scene.ChangedAll;
  end;
end;

procedure TNodeInlineLoadControl.CreateNode;
begin
  inherited;

  FEventChildren := TVRMLEvent.Create(Self, 'children', TMFNode, false);
  Events.Add(FEventChildren);

  FInlined := nil;
end;

class function TNodeInlineLoadControl.ClassNodeTypeName: string;
begin
  Result := 'InlineLoadControl';
end;

class function TNodeInlineLoadControl.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

procedure TNodeInlineLoadControl.AfterLoadInlined;
var
  ChildrenToSend: TMFNode;
begin
  { TODO: InlineLoadControl should load it's contents to
    children MFNode, and we should make a way (analogous
    to TNodeInlineLoadControl.VRML1ChildrenSaveToStream)
    to say that "we don't want to save to stream "children" field".
    For now it's not really important (user doesn't see
    where it's loaded), but it will be later for scripts. }

  if EventChildren.SendNeeded and (Scene <> nil) then
  begin
    ChildrenToSend := TMFNode.CreateUndefined(Self, EventChildren.Name, false);
    try
      ChildrenToSend.Add(FInlined);
      EventChildren.Send(ChildrenToSend, Scene.GetTime);
    finally FreeAndNil(ChildrenToSend) end;
  end;
end;

procedure TNodeLoadSensor.CreateNode;
begin
  inherited;

  FFdTimeOut := TSFTime.Create(Self, 'timeOut', 0);
  Fields.Add(FFdTimeOut);

  FFdWatchList := TMFNode.Create(Self, 'watchList', INodeX3DUrlObject);
  Fields.Add(FFdWatchList);

  FEventIsLoaded := TVRMLEvent.Create(Self, 'isLoaded', TSFBool, false);
  Events.Add(FEventIsLoaded);

  FEventLoadTime := TVRMLEvent.Create(Self, 'loadTime', TSFTime, false);
  Events.Add(FEventLoadTime);

  FEventProgress := TVRMLEvent.Create(Self, 'progress', TSFFloat, false);
  Events.Add(FEventProgress);

  DefaultContainerField := 'children';
end;

class function TNodeLoadSensor.ClassNodeTypeName: string;
begin
  Result := 'LoadSensor';
end;

class function TNodeLoadSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterNetworkingNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeAnchor,
    TNodeInline,
    TNodeInlineLoadControl,
    TNodeLoadSensor
  ]);
end;

{$endif read_implementation}
