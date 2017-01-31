{
  Copyright 2002-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { Basis for all sensors that generate events based on network activity. }
  TAbstractNetworkSensorNode = class(TAbstractSensorNode)
  public
    procedure CreateNode; override;

    {$I auto_generated_node_helpers/x3dnodes_x3dnetworksensornode.inc}
  end;

  { Abstract interface used by all nodes that contain data located
    on the World Wide Web. }
  IAbstractUrlObject = interface(IX3DNode)
  ['{668420DB-20F7-4A5E-BB89-52F29CA39057}']
    property FdUrl: TMFString { read GetFdurl }; { }

    {$I auto_generated_node_helpers/x3dnodes_x3durlobject.inc}
  end;

  TAbstractViewpointNode = class;

  { Load the URL when the user activates (clicks)
    some geometry contained within the Anchor node's children. }
  TAnchorNode = class(TAbstractX3DGroupingNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdDescription: TSFString;
    public property FdDescription: TSFString read FFdDescription;

    private FFdParameter: TMFString;
    public property FdParameter: TMFString read FFdParameter;

    private FFdUrl: TMFString;
    public property FdUrl: TMFString read FFdUrl;

    { Load the document, world and/or viewpoint referenced by this Anchor node.

      This iterates over url field, for each URL trying to load
      referenced file. If URL specifies viewpoint name within 3D file (or current
      3D world), we also search for viewpoint.
      If it will succeed for some URL, returns @true,
      setting output parameters. Returns @false if no loadable URL is found.

      Output NewRootNode is the loaded world,
      or @nil if actually we should stay within the same world.
      @nil happens for URLs like @code(#ViewpointName),
      or when URL refers to some non-3D document (that is opened with appropriate
      application on user system).

      NewViewpoint is the new viewpoint that should be bound,
      or @nil if URL doesn't specify viewpoint name within 3D file.
      This viewpoint is part of NewRootNode graph, if NewRootNode <> @nil,
      or is part of CurrentRootNode if we should stay within
      current world (NewRootNode = @nil).

      Note that this means that NewRootNode = nil and
      NewViewpoint = nil are valid output from this method,
      and indicate that nothing should be done.

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
      loaded (at least, you don't have it's X3D graph). It means that
      any "#Viewpoint" within current model is considered invalid. }
    function LoadAnchor(
      out NewRootNode: TX3DRootNode;
      out NewViewpoint: TAbstractViewpointNode;
      CurrentRootNode: TX3DRootNode): boolean;

    {$I auto_generated_node_helpers/x3dnodes_anchor.inc}
  end;

  { Embed another X3D scene into the current scene. }
  TInlineNode = class(TAbstractGroupingNode, IAbstractBoundedObject, IAbstractUrlObject)
  private
    procedure EventLoadReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
    procedure EventUrlReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
  private
    { Is the inline currently loaded. This means "loaded",
      not "successfully loaded": when InlinedLoaded := true,
      we condider "url" field as handled, and loaded *if possible*.
      If "url" didn't contain anything useful, InlinedLoaded is still set to @true,
      to avoid constantly trying to reload. }
    InlinedLoaded: boolean;
    FInlined: TX3DRootNode;
  protected
    procedure BeforeTraverse(StateStack: TX3DGraphTraverseStateStack); override;
    procedure AfterTraverse(StateStack: TX3DGraphTraverseStateStack); override;
    { You can override this to do anything after the Inline children were
      loaded. It's called only after actual loading occurred, e.g. it's not
      called when LoadInlined was called on the already-loaded node with
      CanReload = @false. }
    procedure AfterLoadInlined; virtual;
    function DeepCopyCore(CopyState: TX3DNodeDeepCopyState): TX3DNode; override;
    procedure ParseAfter(Reader: TX3DReaderNames); override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
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

    { Load inlined content @bold(now).
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
      by code, not by parsing; this also happens when node was made by expanding
      a PROTO).

      @param(Exported If non-nil, we will assign here node names
        exported from the file. Used to handle IMPORT/EXPORT X3D mechanism.) }
    procedure LoadInlined(CanReload: boolean;
      const CurrentLengthConversionFactor: Float = 1.0;
      Exported: TX3DNodeNames = nil);

    procedure UnLoadInlined;

    { Call this to indicate that you loaded the inline contents directly,
      by using AddChild yourself. }
    procedure LoadedInlineDirectly;

    { The inlined content (loaded from inlined url).
      @nil if not yet loaded, or no loading was successful.

      This is also kept in VRML 1.0 children list as VRML1Children[0],
      but for future it's better to not depend on that.
      But it will always be enumerated by things like TX3DNode.EnumerateNodes. }
    property Inlined: TX3DRootNode read FInlined;

    {$I auto_generated_node_helpers/x3dnodes_inline.inc}
  end;

  { Embed another X3D scene into the current scene,
    with explicit "load" control (VRML 97 version).

    Note that our TInlineNode conforms to the newer X3D standard,
    and so it already has a @link(TInlineNode.Load) property.
    This node is implemented only for compatibility with VRML 97.
    It has additional "children" output field. }
  TInlineLoadControlNode = class(TInlineNode)
  protected
    procedure AfterLoadInlined; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event out } { }
    private FEventChildren: TMFNodeEvent;
    public property EventChildren: TMFNodeEvent read FEventChildren;
  end;

  { Monitors the progress and success of downloading URL elements over a network. }
  TLoadSensorNode = class(TAbstractNetworkSensorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdTimeOut: TSFTime;
    public property FdTimeOut: TSFTime read FFdTimeOut;

    private FFdWatchList: TMFNode;
    public property FdWatchList: TMFNode read FFdWatchList;

    { Event out } { }
    private FEventIsLoaded: TSFBoolEvent;
    public property EventIsLoaded: TSFBoolEvent read FEventIsLoaded;

    { Event out } { }
    private FEventLoadTime: TSFTimeEvent;
    public property EventLoadTime: TSFTimeEvent read FEventLoadTime;

    { Event out } { }
    private FEventProgress: TSFFloatEvent;
    public property EventProgress: TSFFloatEvent read FEventProgress;

    {$I auto_generated_node_helpers/x3dnodes_loadsensor.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TAbstractNetworkSensorNode.CreateNode;
begin
  inherited;

  DefaultContainerField := 'children';
end;

procedure TAnchorNode.CreateNode;
begin
  inherited;

  FFdDescription := TSFString.Create(Self, 'description', '');
  AddField(FFdDescription);

  FFdParameter := TMFString.Create(Self, 'parameter', []);
  AddField(FFdParameter);

  FFdUrl := TMFString.Create(Self, 'url', []);
  AddField(FFdUrl);
  { X3D specification comment: [url or urn] }

  DefaultContainerField := 'children';
end;

class function TAnchorNode.ClassX3DType: string;
begin
  Result := 'Anchor';
end;

class function TAnchorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

function TAnchorNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FdChildren.Count - 1 do
  begin
    Result := Func(Self, FdChildren[I]);
    if Result <> nil then Exit;
  end;
end;

function TAnchorNode.LoadAnchor(
  out NewRootNode: TX3DRootNode;
  out NewViewpoint: TAbstractViewpointNode;
  CurrentRootNode: TX3DRootNode): boolean;

  procedure ViewpointNameInvalid(const NewViewpointName: string);
  begin
    WritelnWarning('VRML/X3D', Format('Viewpoint "%s" specified by Anchor not found',
      [NewViewpointName]));
  end;

var
  I: Integer;
  Url, UrlWithoutViewpoint, NewViewpointName: string;
begin
  for I := 0 to FdUrl.Count - 1 do
  begin
    Url := FdUrl.Items[I];

    { We have to call URIExtractAnchor before PathFromBaseUrl.
      Otherwise, in case of Urls like '' or '#viewpoint_in_this_world'
      PathFromBaseUrl would prefix it with our path,
      making it invalid. }
    UrlWithoutViewpoint := Url;
    URIExtractAnchor(UrlWithoutViewpoint, NewViewpointName);

    if UrlWithoutViewpoint = '' then
    begin
      { in this case, we have either a viewpoint in current 3D world, or nothing }
      NewRootNode := nil;

      if NewViewpointName = '' then
      begin
        NewViewpoint := nil;
        Exit(true);
      end else
      begin
        if CurrentRootNode <> nil then
        begin
          NewViewpoint := CurrentRootNode.TryFindNodeByName(TAbstractViewpointNode,
            NewViewpointName, true) as TAbstractViewpointNode;
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
      { in this case, we have either a new 3D world, or a document }
      UrlWithoutViewpoint := PathFromBaseUrl(UrlWithoutViewpoint);
      try
        NewRootNode := Load3D(UrlWithoutViewpoint, false, true);

        if NewRootNode = nil then
        begin
          { we have a document URL }
          if OpenURL(PathFromBaseUrl(Url)) then
          begin
            NewViewpoint := nil;
            Exit(true);
          end else
          begin
            WritelnWarning('VRML/X3D', Format('Cannot open browser to handle document on this URL: %s',
              [PathFromBaseUrl(Url)]));
            Continue;
          end;
        end;

      except
        on E: Exception do
        begin
          { Catch exception and produce WritelnWarning, go to next URL }
          WritelnWarning('VRML/X3D', Format('Loading world from Anchor URL "%s" failed: %s',
            [UrlWithoutViewpoint, E.Message]));
          Continue;
        end;
      end;

      if NewViewpointName = '' then
      begin
        NewViewpoint := nil;
      end else
      begin
        NewViewpoint := NewRootNode.TryFindNodeByName(TAbstractViewpointNode,
          NewViewpointName, true) as TAbstractViewpointNode;
        if NewViewpoint = nil then
          ViewpointNameInvalid(NewViewpointName);

        { In this case invalid viewpoint name is ignored,
          that is we still accept this URL. This follows X3D spec
          (also, this is sensible, otherwise we would have to free
          loaded model just because viewpoint is invalid). }
      end;

      Exit(true);
    end;
  end;
  Result := false;
end;

procedure TInlineNode.CreateNode;
begin
  inherited;

  FFdLoad := TSFBool.Create(Self, 'load', true);
   FdLoad.OnReceive.Add(@EventLoadReceive);
  AddField(FFdLoad);

  FFdUrl := TMFString.Create(Self, 'url', []);
   FdUrl.OnReceive.Add(@EventUrlReceive);
  AddField(FFdUrl);
  { X3D specification comment: [url or urn] }

  FFdBboxCenter := TSFVec3f.Create(Self, 'bboxCenter', Vector3Single(0, 0, 0));
   FdBboxCenter.Exposed := false;
  AddField(FFdBboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdBboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
   FdBboxSize.Exposed := false;
  AddField(FFdBboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }

  DefaultContainerField := 'children';

  { To allow inlined children (they are loaded as VRML 1.0 style children) }
  VRML1ChildrenAllowed := true;
  VRML1ChildrenParsingAllowed := false;

  FInlined := nil;
end;

class function TInlineNode.ClassX3DType: string;
begin
  Result := 'Inline';
end;

class function TInlineNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TInlineNode.ParseAfter(Reader: TX3DReaderNames);
var
  Exported: TX3DNodeNames;
begin
  inherited;

  if X3DName <> '' then
    Exported := TX3DNodeNames.Create(false)
  else
    Exported := nil;

  LoadInlined(false, Reader.LengthConversionFactor, Exported);

  { do not add Exported to Importable list, if nothing was exported.
    This just saves the size of the Importable list. }
  if (Exported <> nil) and (Exported.Count = 0) then
    FreeAndNil(Exported);

  if Exported <> nil then
  begin
    { connect Exported nodes with our inline Name }
    Assert(X3DName <> '');
    Reader.Importable.Bind(X3DName, Exported);
  end;
end;

procedure TInlineNode.UnLoadInlined;
begin
  VRML1ChildrenClear;
  FInlined := nil;
  InlinedLoaded := false;
end;

procedure TInlineNode.LoadInlined(CanReload: boolean;
  const CurrentLengthConversionFactor: Float;
  Exported: TX3DNodeNames);
var
  I: Integer;
  FullUrl: string;
begin
  if Exported <> nil then Exported.Clear;

  if InlinedLoaded then
  begin
    if CanReload then UnLoadInlined else Exit;
  end;

  Assert(not InlinedLoaded);
  Assert(FInlined = nil);
  Assert(VRML1ChildrenCount = 0);

  if (not FdLoad.Value) or
     { Do not do anything, in particular do not set InlinedLoaded:=true,
       when url field is empty. This is important in case "url" field value
       has an IS clause, and it will be really known only after expanding
       the PROTO, see demo_models/prototypes/proto_inline.x3dv testcase. }
     (FdUrl.Items.Count = 0) then
    Exit;

  for I := 0 to FdUrl.Items.Count - 1 do
    { For VRML 1.0 WWWInline, an item on "url" (alias "name") field
      may be empty, and should be nicely ignored then.
      For VRML >= 2.0 we also can ignore it (otherwise confusing warning
      about unrecognized 3D model file extension for dir name is done). }
    if FdUrl.Items[I] <> '' then
    begin
      FullUrl := PathFromBaseUrl(FdUrl.Items[I]);
      try
        FInlined := Load3D(FullUrl, false);
        if (Exported <> nil) and
           (TX3DRootNode(FInlined).ExportedNames <> nil) then
          Exported.Assign(TX3DRootNode(FInlined).ExportedNames);
        Break;
      except
        on E: Exception do
          { Careful here, as WritelnWarning may also raise exception. }
          WritelnWarning('VRML/X3D', Format(SLoadError,
            [E.ClassName, 'inline file', URIDisplay(FullUrl), E.Message]));
      end;
    end;

  if FInlined <> nil then
  begin
    { The Inlined contents are usually TX3DRootNode,
      but they should not be saved as such. Although usually this doesn't
      get saved (since we do not save inline contents), but it may happen
      if you copy nodes around, e.g. "expanding" inlines (see view3dscene
      planned feature, in view3dscene/TODO-expand-inlines.txt).

      TODO: Such expanding must inherently break mismatching UNIT clauses
      for LengthConversionFactor. To overcome this, our TX3DRootNode
      should descend from TTransformNode and have it's scale actually saved. }
    FInlined.SaveAsRootNode := false;

    { FInlined.Scale is adjusted now to make 1 unit = 1 meter inside.
      However, we are already scaled by CurrentLengthConversionFactor.

      (The check for CurrentLengthConversionFactor <> 1 is only to make sure
      we keep Scale equal precisely 1.0 (no fp errors) when nothing uses
      UNIT length.)  }
    if CurrentLengthConversionFactor <> 1 then
      FInlined.Scale := FInlined.Scale / CurrentLengthConversionFactor;

    VRML1ChildAdd(FInlined);
    AfterLoadInlined;
  end;

  { Whether or not we successfully got FInlined <> nil, we always set
    InlinedLoaded to true here. This way we will not try to reload this inline
    at next LoadInlined(false) (this would be bad, as we already did
    WritelnWarning in case loading failed and such). }
  InlinedLoaded := true;
end;

procedure TInlineNode.LoadedInlineDirectly;
begin
  InlinedLoaded := true;
  if VRML1ChildrenCount <> 0 then
    FInlined := VRML1Children[0] as TX3DRootNode else
    FInlined := nil;
end;

function TInlineNode.DeepCopyCore(CopyState: TX3DNodeDeepCopyState): TX3DNode;
var
  ResultI: TInlineNode;
begin
  Result := inherited DeepCopyCore(CopyState);
  ResultI := Result as TInlineNode;

  { Since inherited copied our Children, we should also set "InlinedLoaded"
    and "Inlined" field to the same value. }
  ResultI.InlinedLoaded := InlinedLoaded;
  if ResultI.VRML1ChildrenCount <> 0 then
    ResultI.FInlined := ResultI.VRML1Children[0] as TX3DRootNode else
    ResultI.FInlined := nil;
end;

procedure TInlineNode.AfterLoadInlined;
begin
  { Nothing to do in this class. }
end;

procedure TInlineNode.BeforeTraverse(StateStack: TX3DGraphTraverseStateStack);
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

procedure TInlineNode.AfterTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  Dec(StateStack.Top.InsideInline);
  inherited;
end;

class function TInlineNode.VRML1ChildrenSaveToStream: boolean;
begin
  Result := false;
end;

procedure TInlineNode.EventLoadReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
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
    will free items to which existing TCastleSceneCore / TShape may keep
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

procedure TInlineNode.EventUrlReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
begin
  if FdLoad.Value then
  begin
    if Scene <> nil then Scene.BeforeNodesFree;
    UnLoadInlined;
    LoadInlined(false);
    if Scene <> nil then Scene.ChangedAll;
  end;
end;

procedure TInlineLoadControlNode.CreateNode;
begin
  inherited;

  FEventChildren := TMFNodeEvent.Create(Self, 'children', false);
  AddEvent(FEventChildren);

  FInlined := nil;
end;

class function TInlineLoadControlNode.ClassX3DType: string;
begin
  Result := 'InlineLoadControl';
end;

class function TInlineLoadControlNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType);
end;

procedure TInlineLoadControlNode.AfterLoadInlined;
var
  ChildrenToSend: TMFNode;
begin
  { TODO: InlineLoadControl should load it's contents to
    children MFNode, and we should make a way (analogous
    to TInlineLoadControlNode.VRML1ChildrenSaveToStream)
    to say that "we don't want to save to stream "children" field".
    For now it's not really important (user doesn't see
    where it's loaded), but it will be later for scripts. }

  if EventChildren.SendNeeded and (Scene <> nil) then
  begin
    ChildrenToSend := TMFNode.CreateUndefined(Self, EventChildren.X3DName, false);
    try
      ChildrenToSend.Add(FInlined);
      EventChildren.Send(ChildrenToSend, Scene.NextEventTime);
    finally FreeAndNil(ChildrenToSend) end;
  end;
end;

procedure TLoadSensorNode.CreateNode;
begin
  inherited;

  FFdTimeOut := TSFTime.Create(Self, 'timeOut', 0);
  AddField(FFdTimeOut);

  FFdWatchList := TMFNode.Create(Self, 'watchList', IAbstractUrlObject);
  AddField(FFdWatchList);

  FEventIsLoaded := TSFBoolEvent.Create(Self, 'isLoaded', false);
  AddEvent(FEventIsLoaded);

  FEventLoadTime := TSFTimeEvent.Create(Self, 'loadTime', false);
  AddEvent(FEventLoadTime);

  FEventProgress := TSFFloatEvent.Create(Self, 'progress', false);
  AddEvent(FEventProgress);

  DefaultContainerField := 'children';
end;

class function TLoadSensorNode.ClassX3DType: string;
begin
  Result := 'LoadSensor';
end;

class function TLoadSensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure RegisterNetworkingNodes;
begin
  NodesManager.RegisterNodeClasses([
    TAnchorNode,
    TInlineNode,
    TInlineLoadControlNode,
    TLoadSensorNode
  ]);
end;

{$endif read_implementation}
