{
  Copyright 2002-2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{$ifdef read_interface}
  { }
  TNodeX3DNetworkSensorNode = class(TNodeX3DSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
  end;

  INodeX3DUrlObject = interface(IVRMLNode)
  ['{668420DB-20F7-4A5E-BB89-52F29CA39057}']
    property Fdurl: TMFString { read GetFdurl }; { }
  end;

  TVRMLViewpointNode = class;

  TNodeAnchor = class(TNodeX3DGroupingNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFddescription: TSFString;
    public property Fddescription: TSFString read FFddescription;

    private FFdparameter: TMFString;
    public property Fdparameter: TMFString read FFdparameter;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

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
      out NewRootNode: TVRMLNode;
      out NewViewpoint: TVRMLViewpointNode;
      CurrentRootNode: TVRMLNode): boolean;
  end;

  TNodeInline = class(TVRMLGroupingNode, INodeX3DBoundedObject, INodeX3DUrlObject,
    IVRMLInlineNode)
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
  protected
    procedure BeforeTraverse(StateStack: TVRMLGraphTraverseStateStack); override;
    procedure AfterTraverse(StateStack: TVRMLGraphTraverseStateStack); override;
    { You can override this to do anything after the Inline children were
      loaded. It's called only after actual loading occured, e.g. it's not
      called when LoadInlined was called on the already-loaded node with
      CanReload = @false. }
    procedure AfterLoadInlined; virtual;
    function DeepCopyCore(CopyState: TVRMLNodeDeepCopyState): TVRMLNode; override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdload: TSFBool;
    public property Fdload: TSFBool read FFdload;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

    private FFdbboxCenter: TSFVec3f;
    public property FdbboxCenter: TSFVec3f read FFdbboxCenter;

    private FFdbboxSize: TSFVec3f;
    public property FdbboxSize: TSFVec3f read FFdbboxSize;

    class function ChildrenSaveToStream: boolean; override;

    procedure LoadInlined(CanReload: boolean);
    procedure UnLoadInlined;

    { Call this to indicate that you loaded the inline contents directly,
      by using AddChild yourself. }
    procedure LoadedInlineDirectly;
  end;

  { InlineLoadControl VRML 97 node. It's not part of X3D, X3D normal "Inline"
    node already has a "load" field. }
  TNodeInlineLoadControl = class(TVRMLGroupingNode, IVRMLInlineNode)
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
  protected
    procedure BeforeTraverse(StateStack: TVRMLGraphTraverseStateStack); override;
    procedure AfterTraverse(StateStack: TVRMLGraphTraverseStateStack); override;
    function DeepCopyCore(CopyState: TVRMLNodeDeepCopyState): TVRMLNode; override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdload: TSFBool;
    public property Fdload: TSFBool read FFdload;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

    private FFdbboxCenter: TSFVec3f;
    public property FdbboxCenter: TSFVec3f read FFdbboxCenter;

    private FFdbboxSize: TSFVec3f;
    public property FdbboxSize: TSFVec3f read FFdbboxSize;

    { Event: MFNode, out } { }
    private FEventchildren: TVRMLEvent;
    public property Eventchildren: TVRMLEvent read FEventchildren;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    class function ChildrenSaveToStream: boolean; override;

    procedure LoadInlined(CanReload: boolean);
    procedure UnLoadInlined;

    { Call this to indicate that you loaded the inline contents directly,
      by using AddChild yourself. }
    procedure LoadedInlineDirectly;
  end;

  TNodeLoadSensor = class(TNodeX3DNetworkSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdtimeOut: TSFTime;
    public property FdtimeOut: TSFTime read FFdtimeOut;

    private FFdwatchList: TMFNode;
    public property FdwatchList: TMFNode read FFdwatchList;

    { Event: SFBool, out } { }
    private FEventisLoaded: TVRMLEvent;
    public property EventisLoaded: TVRMLEvent read FEventisLoaded;

    { Event: SFTime, out } { }
    private FEventloadTime: TVRMLEvent;
    public property EventloadTime: TVRMLEvent read FEventloadTime;

    { Event: SFFloat, out } { }
    private FEventprogress: TVRMLEvent;
    public property Eventprogress: TVRMLEvent read FEventprogress;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DNetworkSensorNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'children';
end;

constructor TNodeAnchor.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFddescription := TSFString.Create(Self, 'description', '');
  Fields.Add(FFddescription);

  FFdparameter := TMFString.Create(Self, 'parameter', []);
  Fields.Add(FFdparameter);

  FFdurl := TMFString.Create(Self, 'url', []);
  Fields.Add(FFdurl);
  { X3D specification comment: [url or urn] }

  DefaultContainerField := 'children';
end;

class function TNodeAnchor.ClassNodeTypeName: string;
begin
  Result := 'Anchor';
end;

function TNodeAnchor.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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
  out NewRootNode: TVRMLNode;
  out NewViewpoint: TVRMLViewpointNode;
  CurrentRootNode: TVRMLNode): boolean;

  procedure ViewpointNameInvalid(const NewViewpointName: string);
  begin
    VRMLWarning(vwSerious, Format('Viewpoint "%s" specified by Anchor not found',
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
        NewRootNode := LoadAsVRML(FullUrl, false);
      except
        on E: Exception do
        begin
          { Catch exception and produce VRMLWarning, go to next URL }
          VRMLWarning(vwSerious, Format('Loading world from Anchor URL "%s" failed: %s',
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

constructor TNodeInline.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  { It's safer to register on EventOuts below, because then I already
    know that exposed eventIn handler set appropriate field value
    before calling our eventOut handler.
    In EventIn, you would not really know
    whether exposed event handler was already done or not (depends
    on the of calbacks on OnReceive list).

    Even better, I know that notification to ParentEventsProcessor
    (from exposed eventIn handler)
    is done *after* running our eventOut handler.
    This means that ParentEventsProcessor will be notified about
    changes *after* EventLoadReceive or EventUrlReceive changed the nodes
    to load/unload, so TVRMLScene.ChangedAll will be called with
    the *new* graph state, so it will properly update everything to
    new state. }

  FFdload := TSFBool.Create(Self, 'load', true);
  Fields.Add(FFdload);
  Fdload.EventOut.OnReceive.AppendItem(@EventLoadReceive);

  FFdurl := TMFString.Create(Self, 'url', []);
  Fields.Add(FFdurl);
  Fdurl.EventOut.OnReceive.AppendItem(@EventUrlReceive);
  { X3D specification comment: [url or urn] }

  FFdbboxCenter := TSFVec3f.Create(Self, 'bboxCenter', Vector3Single(0, 0, 0));
  FFdbboxCenter.Exposed := false;
  Fields.Add(FFdbboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdbboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
  FFdbboxSize.Exposed := false;
  Fields.Add(FFdbboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }

  DefaultContainerField := 'children';

  { To allow inlined children (they are loaded as VRML 1.0 style children) }
  FParsingAllowedChildren := false;
  FAllowedChildren := true;
end;

class function TNodeInline.ClassNodeTypeName: string;
begin
  Result := 'Inline';
end;

function TNodeInline.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeInline.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeInline.UnLoadInlined;
begin
  RemoveAllChildren;
  Loaded := false;
end;

procedure TNodeInline.LoadInlined(CanReload: boolean);
var
  I: Integer;
  FullUrl: string;
  NewNode: TVRMLNode;
begin
  if Loaded then
  begin
    if CanReload then UnLoadInlined else Exit;
  end;

  Assert(not Loaded);
  Assert(ChildrenCount = 0);

  if not FdLoad.Value then Exit;

  NewNode := nil;

  for I := 0 to FdUrl.Items.Count - 1 do
  begin
    FullUrl := PathFromWWWBasePath(FdUrl.Items[I]);
    try
      NewNode := LoadAsVRML(PathFromWWWBasePath(FullUrl), false);
      Break;
    except
      on E: Exception do
        { pamietajmy ze VRMLWarning moze spowodowac rzucenie wyjatku
          (chociaz nie musi) }
        VRMLWarning(vwIgnorable, 'Exception ' + E.ClassName +
          ' occured when trying to load '+
          'inline file from URL "' + FullUrl + ' : ' + E.Message);
    end;
  end;

  if NewNode <> nil then
  begin
    AddChild(NewNode);
    AfterLoadInlined;
  end;

  { Whether or not we successfully got NewNode <> nil, we always set
    Loaded to true here. This way we will not try to reload this inline
    at next BeforeTraverse (this would be bad, as we already did
    VRMLWarning in case loading failed and such). }
  Loaded := true;
end;

procedure TNodeInline.LoadedInlineDirectly;
begin
  Loaded := true;
end;

function TNodeInline.DeepCopyCore(CopyState: TVRMLNodeDeepCopyState): TVRMLNode;
begin
  Result := inherited DeepCopyCore(CopyState);
  { Since inherited copied our Children, we should also set "Loaded"
    field to the same value. }
  (Result as TNodeInline).Loaded := Loaded;
end;

procedure TNodeInline.AfterLoadInlined;
begin
  { Nothing to do in this class. }
end;

procedure TNodeInline.BeforeTraverse(StateStack: TVRMLGraphTraverseStateStack);
begin
  inherited;
  LoadInlined(false);
  Inc(StateStack.Top.InsideInline);
end;

procedure TNodeInline.AfterTraverse(StateStack: TVRMLGraphTraverseStateStack);
begin
  Dec(StateStack.Top.InsideInline);
  inherited;
end;

class function TNodeInline.ChildrenSaveToStream: boolean;
begin
  Result := false;
end;

procedure TNodeInline.EventLoadReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  if FdLoad.Value then
    LoadInlined(false) else
    UnLoadInlined;
end;

procedure TNodeInline.EventUrlReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  if FdLoad.Value then
  begin
    UnLoadInlined;
    LoadInlined(false);
  end;
end;

constructor TNodeInlineLoadControl.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  { It's safer to register on EventOuts below, because then I already
    know that exposed eventIn handler set appropriate field value
    before calling our eventOut handler.
    In EventIn, you would not really know
    whether exposed event handler was already done or not (depends
    on the of calbacks on OnReceive list).

    Even better, I know that notification to ParentEventsProcessor
    (from exposed eventIn handler)
    is done *after* running our eventOut handler.
    This means that ParentEventsProcessor will be notified about
    changes *after* EventLoadReceive or EventUrlReceive changed the nodes
    to load/unload, so TVRMLScene.ChangedAll will be called with
    the *new* graph state, so it will properly update everything to
    new state. }

  FFdload := TSFBool.Create(Self, 'load', TRUE);
  Fields.Add(FFdload);
  Fdload.EventOut.OnReceive.AppendItem(@EventLoadReceive);

  FFdurl := TMFString.Create(Self, 'url', []);
  Fields.Add(FFdurl);
  Fdurl.EventOut.OnReceive.AppendItem(@EventUrlReceive);

  FFdbboxCenter := TSFVec3f.Create(Self, 'bboxCenter', ZeroVector3Single);
  FFdbboxCenter.Exposed := false;
  Fields.Add(FFdbboxCenter);

  FFdbboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
  FFdbboxSize.Exposed := false;
  Fields.Add(FFdbboxSize);

  FEventchildren := TVRMLEvent.Create(Self, 'children', TMFNode, false);
  Events.Add(FEventchildren);

  FParsingAllowedChildren := false;
  FAllowedChildren := true;
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

procedure TNodeInlineLoadControl.UnLoadInlined;
begin
  RemoveAllChildren;
  Loaded := false;
end;

procedure TNodeInlineLoadControl.LoadInlined(CanReload: boolean);
var
  I: Integer;
  FullUrl: string;
  NewNode: TVRMLNode;
  ChildrenToSend: TMFNode;
begin
  { TODO: InlineLoadControl should load it's contents to
    children MFNode, and we should make a way (analogous
    to TNodeInlineLoadControl.ChildrenSaveToStream)
    to say that "we don't want to save to stream "children" field".
    For now it's not really important (user doesn't see
    where it's loaded), but it will be later for scripts. }

  if Loaded then
  begin
    if CanReload then UnLoadInlined else Exit;
  end;

  Assert(not Loaded);
  Assert(ChildrenCount = 0);

  if not FdLoad.Value then Exit;

  NewNode := nil;

  for I := 0 to FdUrl.Items.Count - 1 do
  begin
    FullUrl := PathFromWWWBasePath(FdUrl.Items[I]);
    try
      NewNode := LoadAsVRML(PathFromWWWBasePath(FullUrl), false);
      Break;
    except
      on E: Exception do
        { pamietajmy ze VRMLWarning moze spowodowac rzucenie wyjatku
          (chociaz nie musi) }
        VRMLWarning(vwIgnorable, 'Exception ' + E.ClassName +
          ' occured when trying to load '+
          'inline file from URL "' + FullUrl + ' : ' + E.Message);
    end;
  end;

  if NewNode <> nil then
  begin
    AddChild(NewNode);
    if EventChildren.SendNeeded and (ParentEventsProcessor <> nil) then
    begin
      ChildrenToSend := TMFNode.CreateUndefined(Self, EventChildren.Name);
      try
        ChildrenToSend.AddItem(NewNode);
        EventChildren.Send(ChildrenToSend,
          (ParentEventsProcessor as TVRMLScene).WorldTime);
      finally FreeAndNil(ChildrenToSend) end;
    end;
  end;

  { Whether or not we successfully got NewNode <> nil, we always set
    Loaded to true here. This way we will not try to reload this inline
    at next BeforeTraverse (this would be bad, as we already did
    VRMLWarning in case loading failed and such). }
  Loaded := true;
end;

procedure TNodeInlineLoadControl.LoadedInlineDirectly;
begin
  Loaded := true;
end;

function TNodeInlineLoadControl.DeepCopyCore(CopyState: TVRMLNodeDeepCopyState): TVRMLNode;
begin
  Result := inherited DeepCopyCore(CopyState);
  { Since inherited copied our Children, we should also set "Loaded"
    field to the same value. }
  (Result as TNodeInlineLoadControl).Loaded := Loaded;
end;

procedure TNodeInlineLoadControl.BeforeTraverse(StateStack: TVRMLGraphTraverseStateStack);
begin
  inherited;
  LoadInlined(false);
  Inc(StateStack.Top.InsideInline);
end;

procedure TNodeInlineLoadControl.AfterTraverse(StateStack: TVRMLGraphTraverseStateStack);
begin
  Dec(StateStack.Top.InsideInline);
  inherited;
end;

procedure TNodeInlineLoadControl.EventLoadReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  if FdLoad.Value then
    LoadInlined(false) else
    UnLoadInlined;
end;

procedure TNodeInlineLoadControl.EventUrlReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  if FdLoad.Value then
  begin
    UnLoadInlined;
    LoadInlined(false);
  end;
end;

class function TNodeInlineLoadControl.ChildrenSaveToStream: boolean;
begin
  Result := false;
end;

function TNodeInlineLoadControl.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

constructor TNodeLoadSensor.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdtimeOut := TSFTime.Create(Self, 'timeOut', 0);
  Fields.Add(FFdtimeOut);

  FFdwatchList := TMFNode.Create(Self, 'watchList', INodeX3DUrlObject);
  Fields.Add(FFdwatchList);

  FEventisLoaded := TVRMLEvent.Create(Self, 'isLoaded', TSFBool, false);
  Events.Add(FEventisLoaded);

  FEventloadTime := TVRMLEvent.Create(Self, 'loadTime', TSFTime, false);
  Events.Add(FEventloadTime);

  FEventprogress := TVRMLEvent.Create(Self, 'progress', TSFFloat, false);
  Events.Add(FEventprogress);

  DefaultContainerField := 'children';
end;

class function TNodeLoadSensor.ClassNodeTypeName: string;
begin
  Result := 'LoadSensor';
end;

function TNodeLoadSensor.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
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
