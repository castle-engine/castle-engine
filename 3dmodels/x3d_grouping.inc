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
  { This is a VRML >= 2.0 grouping node.
    This will push/pop full TVRMLGraphTraverseState in Before/AfterTraverse.
    It also propagates DirectionalLights in any child to all children
    in VRML2ActiceLights.

    To keep X3D hierarchy, this descends from X3DChildNode.
    This way TNodeX3DGroupingNode and TNodeStaticGroup inherit
    also from X3DChildNode. }
  TVRMLGroupingNode = class(TNodeX3DChildNode)
  private
    OriginalState: TVRMLGraphTraverseState;
  protected
    procedure BeforeTraverse(var State: TVRMLGraphTraverseState); override;
    procedure AfterTraverse(var State: TVRMLGraphTraverseState); override;
  end;

  INodeX3DBoundedObject = interface(IVRMLNode)
  ['{0EF47711-754B-4BA8-97E5-0A9601E9C542}']
    property FdbboxCenter: TSFVec3f { read GetFdbboxCenter }; { }
    property FdbboxSize: TSFVec3f { read GetFdbboxSize }; { }
  end;

  TNodeX3DGroupingNode = class(TVRMLGroupingNode, INodeX3DBoundedObject)
  private
    procedure EventAddChildrenReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TKamTime);
    procedure EventRemoveChildrenReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TKamTime);
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    { Event: MFNode, in } { }
    private FEventaddChildren: TVRMLEvent;
    public property EventaddChildren: TVRMLEvent read FEventaddChildren;

    { Event: MFNode, in } { }
    private FEventremoveChildren: TVRMLEvent;
    public property EventremoveChildren: TVRMLEvent read FEventremoveChildren;

    private FFdchildren: TMFNode;
    public property Fdchildren: TMFNode read FFdchildren;

    private FFdbboxCenter: TSFVec3f;
    public property FdbboxCenter: TSFVec3f read FFdbboxCenter;

    private FFdbboxSize: TSFVec3f;
    public property FdbboxSize: TSFVec3f read FFdbboxSize;

    private FFdrender: TSFBool;
    { This is an extension from Avalon, see
      [http://instant-reality.com/documentation/nodetype/ChildGroup/].
      We simply ignore it for now. }
    public property Fdrender: TSFBool read FFdrender;

    function ChildrenField: TMFNode; override;
  end;

  TNodeGroup_2 = class(TNodeX3DGroupingNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  { Interface common for all TVRMLRootNode_* versions. }
  IVRMLRootNode = interface(IVRMLNode)
  ['{3BBD44B0-792A-4715-BA62-352A76756D59}']
  end;

  { A Group node that is added when VRML >= 2.0 file contains more
    than one node at the root level. (Or contains any prototypes,
    routes etc. at the root level, as they must all be stored inside
    some node's properties).

    See TVRMLRootNode_1 for some more comments about why this
    descends from TNodeGroup_2. }
  TVRMLRootNode_2 = class(TNodeGroup_2, IVRMLRootNode)
  private
    FForceVersion: boolean;
    FForceVersionMajor: Integer;
    FForceVersionMinor: Integer;
    FX3DProfile: string;
    FX3DComponentNames: TDynStringArray;
    FX3DComponentLevels: TDynLongIntArray;
    FX3DMetaKeys: TDynStringArray;
    FX3DMetaValues: TDynStringArray;
  protected
    function DeepCopyCore(CopyState: TVRMLNodeDeepCopyState): TVRMLNode; override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    destructor Destroy; override;

    (*Save TVRMLRootNode_* contents to stream.

      TVRMLRootNode_* is saved to stream in a special way, so that actually
      only it's contents are written, without surrounding { } braces.
      This way, when saving, we "undo" the artificial wrapping in
      TVRMLRootNode_* that was done by ParseVRMLStatements and ParseVRMLFile. *)
    procedure SaveToStream(SaveProperties: TVRMLSaveToStreamProperties);  override;

    { Set ForceVersion to @true to make SuggestedVRMLVersion of this class
      return suggest ForceVersionMajor, ForceVersionMinor with strong
      priority.

      @groupBegin }
    property ForceVersion: boolean read FForceVersion write FForceVersion default false;
    property ForceVersionMajor: Integer read FForceVersionMajor write FForceVersionMajor;
    property ForceVersionMinor: Integer read FForceVersionMinor write FForceVersionMinor;
    { @groupEnd }

    { If this is the root of X3D file, and profile was specified
      (it always should), this is it. }
    property X3DProfile: string read FX3DProfile write FX3DProfile;

    { If this is the root of X3D file, these are specified components
      and their levels. Lengths of these arrays must always be equal.
      @groupBegin }
    property X3DComponentNames: TDynStringArray read FX3DComponentNames;
    property X3DComponentLevels: TDynLongIntArray read FX3DComponentLevels;
    { @groupEnd }

    { If this is the root of X3D file, these are specified meta keys
      and their values. Lengths of these arrays must always be equal.
      @groupBegin }
    property X3DMetaKeys: TDynStringArray read FX3DMetaKeys;
    property X3DMetaValues: TDynStringArray read FX3DMetaValues;
    { @groupEnd }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeStaticGroup = class(TVRMLGroupingNode, INodeX3DBoundedObject)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdchildren: TMFNode;
    public property Fdchildren: TMFNode read FFdchildren;

    private FFdbboxCenter: TSFVec3f;
    public property FdbboxCenter: TSFVec3f read FFdbboxCenter;

    private FFdbboxSize: TSFVec3f;
    public property FdbboxSize: TSFVec3f read FFdbboxSize;

    function ChildrenField: TMFNode; override;
  end;

  { Switch node.

    Compatibility node: this class is used for both VRML 97 and X3D.
    Although field name changed for X3D (in VRML 97, "children"
    was called "choice"), this is handled seamlessly using our
    TVRMLFieldOrEvent.AddAlternativeField mechanism. In other words,
    programmer simply works with FdChildren field, and when reading/writing
    VRML file we take care to actually use either "children" or "choice" name,
    depending on used VRML version. }
  TNodeSwitch_2 = class(TNodeX3DGroupingNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdwhichChoice: TSFInt32;
    public property FdwhichChoice: TSFInt32 read FFdwhichChoice;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function ChildrenField: TMFNode; override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeTransform_2 = class(TNodeX3DGroupingNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
    procedure BeforeTraverse(var State: TVRMLGraphTraverseState); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    private FFdcenter: TSFVec3f;
    public property Fdcenter: TSFVec3f read FFdcenter;

    private FFdrotation: TSFRotation;
    public property Fdrotation: TSFRotation read FFdrotation;

    private FFdscale: TSFVec3f;
    public property Fdscale: TSFVec3f read FFdscale;

    private FFdscaleOrientation: TSFRotation;
    public property FdscaleOrientation: TSFRotation read FFdscaleOrientation;

    private FFdtranslation: TSFVec3f;
    public property Fdtranslation: TSFVec3f read FFdtranslation;
  end;

{$endif read_interface}

{$ifdef read_implementation}
type
  TGeneralGroupingEnumerator = class
    Lights: TDynActiveLightArray;
    State: TVRMLGraphTraverseState;
    WasPointingDeviceSensor: boolean;
    procedure EnumerateChildrenFunction(Node, Child: TVRMLNode);
  end;

  procedure TGeneralGroupingEnumerator.EnumerateChildrenFunction(
    Node, Child: TVRMLNode);
  begin
    if Child is TVRMLDirectionalLightNode then
      { We do CreateActiveLight with State from TVRMLGroupingNode node,
        while precisely we should rather use State from TNodeDirectionalLight
        traverse. But, fortunately, State of TNodeDirectionalLight doesn't
        change inside (TNodeDirectionalLight doesn't do anything inside
        BeforeTraverse), so this is the same thing. }
      State.VRML2ActiveLights.AppendItem(
        TVRMLDirectionalLightNode(Child).CreateActiveLight(State)) else
    if Child is TNodeX3DPointingDeviceSensorNode then
    begin
      { Testing for WasPointingDeviceSensor guarantees that multiple
        pointing devices within the same grouping node will all work. }
      if not WasPointingDeviceSensor then
      begin
        State.PointingDeviceSensors.Clear;
        WasPointingDeviceSensor := true;
      end;

      State.PointingDeviceSensors.Add(Child);
    end;
  end;

procedure TVRMLGroupingNode.BeforeTraverse(
  var State: TVRMLGraphTraverseState);
var
  Enumerator: TGeneralGroupingEnumerator;
begin
  inherited;
  OriginalState := State;
  State := TVRMLGraphTraverseState.CreateCopy(OriginalState);

  (*We append all directional lights to the current State.
    This is how we implement DirectionalLight scope according to VRML 2.0
    specification.

    This may seem wasteful to enumerate children twice
    (first we enumerate here, then actual Traverse enumerates,
    this time recursively, for the second time). But it turned out to
    be the fastest and simplest method of propagating DirectionalLight
    correctly...

    First approach was to add ParentGroup to TActiveLight
    and do it as post-processing step, i.e. in TVRMLScene in
    UpdateVRML2ActiveLights take this ParentGroup and add light everywhere.
    But that was 1. even slower, since it must traverse ParentGroup once again
    for each directional light, and for each shape within this ParentGroup,
    it must find all it's occurences inside ShapeStates, and add light there
    2. it fails ugly in case of DEF / USE of shapes.

    See kambi_vrml_test_suite/vrml_2/directional_light_scope.wrl test. Imagine
    (simplified VRML below) :

      Group {
        DEF S Shape { .... some sphere .... }
        Group {
          USE S
          DirectionalLight { }
          USE S
        }
        USE S
      }

    What would happen here ? ParentGroup.Enumerate would find S *two* times.
    For each of these occurences, it would find *four* shape states with
    matching node value. So 1. all four spheres would be lighted (incorrect,
    only two spheres in the middle should be lighted) 2. all of them would
    be lighted two times by the same light... Fix for 1st problem would
    require us to record some list of parents within State (which would
    awfully slow down Traverse work, that already is too crowded). Fix for 2nd
    problem would require some intelligent avoiding of duplicates
    (set light only for first node, that is both matching and has the light
    not set yet).

    It would be quite more convoluted and much slower than
    simple, correct solution below. *)

  { We also use it to collect State.PointingDeviceSensors,
    as it turns our that the same approach is possible, for the same reasons.
    Moreover, we do it all in a single DirectEnumerateActive call,
    so this doesn't slow down the code.
  }

  { It's a little unclean to test "Self is ...", since usually virtual
    methods should replace this. But in this case, this is (for now)
    the simplest solution. Anchor node serves as a pointing device
    for all it's children, so it just places itself in
    State.PointingDeviceSensors.

    Note that if Anchor node itself has other pointing device sensors
    inside, they will override the Anchor --- as far as I know, this
    is what VRML specificaion expects? (if not, we should just set
    Enumerator.WasPointingDeviceSensor := true when inside Anchor,
    so the solution will be trivial). }
  if Self is TNodeAnchor then
  begin
    State.PointingDeviceSensors.Clear;
    State.PointingDeviceSensors.Add(Self);
  end;

  Enumerator := TGeneralGroupingEnumerator.Create;
  try
    Enumerator.State := State;
    Enumerator.WasPointingDeviceSensor := false;
    DirectEnumerateActive(
      {$ifdef FPC_OBJFPC} @ {$endif} Enumerator.EnumerateChildrenFunction);
  finally FreeAndNil(Enumerator) end;
end;

procedure TVRMLGroupingNode.AfterTraverse(
  var State: TVRMLGraphTraverseState);
begin
  FreeAndNil(State);
  State := OriginalState;
  inherited;
end;

constructor TNodeX3DGroupingNode.Create(
  const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FEventaddChildren := TVRMLEvent.Create(Self, 'addChildren', TMFNode, true);
  Events.Add(FEventaddChildren);
  EventaddChildren.OnReceive.AppendItem(@EventAddChildrenReceive);

  FEventremoveChildren := TVRMLEvent.Create(Self, 'removeChildren', TMFNode, true);
  Events.Add(FEventremoveChildren);
  EventRemoveChildren.OnReceive.AppendItem(@EventRemoveChildrenReceive);

  FFdchildren := TMFNode.Create(Self, 'children', INodeX3DChildNode);
  Fields.Add(FFdchildren);

  FFdbboxCenter := TSFVec3f.Create(Self, 'bboxCenter', ZeroVector3Single);
  FFdbboxCenter.Exposed := false;
  Fields.Add(FFdbboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdbboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
  FFdbboxSize.Exposed := false;
  Fields.Add(FFdbboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }

  FFdrender := TSFBool.Create(Self, 'render', true);
  Fields.Add(FFdrender);

  DefaultContainerField := 'children';
end;

function TNodeX3DGroupingNode.ChildrenField: TMFNode;
begin
  Result := FdChildren;
end;

procedure TNodeX3DGroupingNode.EventAddChildrenReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TKamTime);
var
  MF: TMFNode;
  I: Integer;
begin
  MF := Value as TMFNode;
  for I := 0 to MF.Count - 1 do
    if FdChildren.Items.IndexOf(MF.Items[I]) = -1 then
      FdChildren.AddItem(MF.Items[I]);

  { X3D spec says explicitly
    "A children field is not allowed to contain nodes that are
    ancestors of the grouping node"
    so correct X3D file cannot produce cycles. Cycles would be very bad,
    since
    - they will make memory leaks (reference count will be > 0 for grouping node)
    - and, worse, TVRMLNode.Traverse will hang infinitely

    TODO: So invalid VRML file can produce mem leaks and hang the engine... }
end;

procedure TNodeX3DGroupingNode.EventRemoveChildrenReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TKamTime);
var
  MF: TMFNode;
  I: Integer;
begin
  MF := Value as TMFNode;

  { Note that X3D specification directly says
    "A children field is not allowed to directly contain
    multiple instances of the same node."
    Thus the question how we should deal with multiple items to remove
    (use Delete or DeleteAll in implementation below?) doesn't exist.

    Note that I didn't find such notice in VRML 97 specification.
    This seems to be undefined case in VRML 97? }

  for I := 0 to MF.Count - 1 do
    FdChildren.Items.Delete(MF.Items[I]);
end;

constructor TNodeGroup_2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'children';
end;

class function TNodeGroup_2.ClassNodeTypeName: string;
begin
  Result := 'Group';
end;

class function TNodeGroup_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeGroup_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

procedure TNodeGroup_2.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
var
  I: Integer;
begin
  for I := 0 to FdChildren.Count - 1 do
    Func(Self, FdChildren.Items[I]);
end;

function TNodeGroup_2.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

constructor TVRMLRootNode_2.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;
  FX3DComponentNames := TDynStringArray.Create;
  FX3DComponentLevels := TDynLongIntArray.Create;
  FX3DMetaKeys := TDynStringArray.Create;
  FX3DMetaValues := TDynStringArray.Create;
end;

destructor TVRMLRootNode_2.Destroy;
begin
  FreeAndNil(FX3DComponentNames);
  FreeAndNil(FX3DComponentLevels);
  FreeAndNil(FX3DMetaKeys);
  FreeAndNil(FX3DMetaValues);
  inherited;
end;

procedure TVRMLRootNode_2.SaveToStream(SaveProperties: TVRMLSaveToStreamProperties);
var
  I: integer;
  FileItems: TVRMLFileItemsList;
begin
  { Special things for TVRMLRootNode_2 saving:
    NodeName is ignored (should be '').
    Only field "children" is used, and in a special way (we write it's
    nodes, without writing field name "children [" "]").
    Other fields are ignored (should never have any meaning). }

  FileItems := TVRMLFileItemsList.Create;
  try
    for I := 0 to Prototypes.Count - 1 do
      FileItems.Add(Prototypes[I]);

    for i := 0 to FdChildren.Count - 1 do
      FileItems.Add(FdChildren.Items[I]);

    for I := 0 to Routes.Count - 1 do
      FileItems.Add(Routes[I]);

    FileItems.SaveToStream(SaveProperties);
  finally FreeAndNil(FileItems) end;
end;

function TVRMLRootNode_2.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  if ForceVersion then
    AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
      true, ForceVersionMajor, ForceVersionMinor, 10 * 1000);
end;

function TVRMLRootNode_2.DeepCopyCore(CopyState: TVRMLNodeDeepCopyState): TVRMLNode;
var
  Res: TVRMLRootNode_2;
begin
  Result := inherited;

  Res := Result as TVRMLRootNode_2;

  Res.ForceVersion := ForceVersion;
  Res.ForceVersionMajor := ForceVersionMajor;
  Res.ForceVersionMinor := ForceVersionMinor;

  Res.X3DProfile := X3DProfile;

  Res.X3DComponentNames.Assign(X3DComponentNames);
  Res.X3DComponentLevels.Assign(X3DComponentLevels);
  Res.X3DMetaKeys.Assign(X3DMetaKeys);
  Res.X3DMetaValues.Assign(X3DMetaValues);
end;

constructor TNodeStaticGroup.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdchildren := TMFNode.Create(Self, 'children', INodeX3DChildNode);
  FFdchildren.Exposed := false;
  Fields.Add(FFdchildren);

  FFdbboxCenter := TSFVec3f.Create(Self, 'bboxCenter', Vector3Single(0, 0, 0));
  FFdbboxCenter.Exposed := false;
  Fields.Add(FFdbboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdbboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
  FFdbboxSize.Exposed := false;
  Fields.Add(FFdbboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }

  DefaultContainerField := 'children';
end;

class function TNodeStaticGroup.ClassNodeTypeName: string;
begin
  Result := 'StaticGroup';
end;

function TNodeStaticGroup.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeStaticGroup.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TNodeStaticGroup.ChildrenField: TMFNode;
begin
  Result := FdChildren;
end;

procedure TNodeStaticGroup.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
var
  I: Integer;
begin
  for I := 0 to FdChildren.Count - 1 do
    Func(Self, FdChildren.Items[I]);
end;

constructor TNodeSwitch_2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fdchildren.AddAlternativeName('choice', 2);

  FFdwhichChoice := TSFInt32.Create(Self, 'whichChoice', -1);
  Fields.Add(FFdwhichChoice);
  { X3D specification comment: [-1,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeSwitch_2.ClassNodeTypeName: string;
begin
  Result := 'Switch';
end;

class function TNodeSwitch_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeSwitch_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

function TNodeSwitch_2.ChildrenField: TMFNode;
begin
  Result := FdChildren;
end;

procedure TNodeSwitch_2.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
begin
  if Between(FdWhichChoice.Value, 0, FdChildren.Count - 1) then
    Func(Self, FdChildren.Items[FdWhichChoice.Value]);
end;

function TNodeSwitch_2.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

constructor TNodeTransform_2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdcenter := TSFVec3f.Create(Self, 'center', Vector3Single(0, 0, 0));
  Fields.Add(FFdcenter);
  Fdcenter.Transform := true;
  { X3D specification comment: (-Inf,Inf) }

  FFdrotation := TSFRotation.Create(Self, 'rotation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdrotation);
  Fdrotation.Transform := true;
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdscale := TSFVec3f.Create(Self, 'scale', Vector3Single(1, 1, 1));
  Fields.Add(FFdscale);
  Fdscale.Transform := true;
  { X3D specification comment: (-Inf, Inf) }

  FFdscaleOrientation := TSFRotation.Create(Self, 'scaleOrientation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdscaleOrientation);
  FdscaleOrientation.Transform := true;
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdtranslation := TSFVec3f.Create(Self, 'translation', Vector3Single(0, 0, 0));
  Fields.Add(FFdtranslation);
  Fdtranslation.Transform := true;
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeTransform_2.ClassNodeTypeName: string;
begin
  Result := 'Transform';
end;

class function TNodeTransform_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeTransform_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

procedure TNodeTransform_2.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
var
  I: Integer;
begin
  for I := 0 to FdChildren.Count - 1 do
    Func(Self, FdChildren.Items[I]);
end;

{ Apply transformation to a TVRMLGraphTraverseState.
  Useful for various classes that mimic Transform node behavior,
  but for any reason do not want to descend from TNodeTransform_2. }

procedure TransformState(State: TVRMLGraphTraverseState;
  center: TSFVec3f;
  rotation: TSFRotation;
  scale: TSFVec3f;
  scaleOrientation: TSFRotation;
  translation: TSFVec3f);
var
  M, IM, MRotateScaleOrient, IMRotateScaleOrient: TMatrix4Single;
begin
  { Inherited TVRMLGroupingNode already saved State.Transform and such.

    To make InvertedTransform, we multiply inverted matrices in inverted order
    below. }

  TranslationMatrices(VectorAdd(Translation.Value, Center.Value), M, IM);
  State.Transform := MatrixMult(State.Transform, M);
  State.InvertedTransform := MatrixMult(IM, State.InvertedTransform);

  { Note that even Axis = zero is OK, both M and IM will be identity in
    this case. }
  RotationMatricesRad(Rotation.RotationRad, Rotation.Axis, M, IM);
  State.Transform := MatrixMult(State.Transform, M);
  State.InvertedTransform := MatrixMult(IM, State.InvertedTransform);

  if (Scale.Value[0] <> 1) or
     (Scale.Value[1] <> 1) or
     (Scale.Value[2] <> 1) then
  begin
    RotationMatricesRad(ScaleOrientation.RotationRad, ScaleOrientation.Axis,
      MRotateScaleOrient, IMRotateScaleOrient);

    State.Transform := MatrixMult(State.Transform, MRotateScaleOrient);
    State.InvertedTransform := MatrixMult(IMRotateScaleOrient, State.InvertedTransform);

    { For scaling, we explicitly request that if ScalingFactor contains
      zero, IM will be forced to be identity. That's because VRML allows
      scaling factor to have 0 components (we need InvertedTransform only
      for special tricks). }

    ScalingMatrices(Scale.Value, true, M, IM);
    State.Transform := MatrixMult(State.Transform, M);
    State.InvertedTransform := MatrixMult(IM, State.InvertedTransform);

    { That's right, we reuse MRotateScaleOrient and IMRotateScaleOrient
      matrices below. Since we want to reverse them now, so normal
      Transform is multiplied by IM and InvertedTransform is multiplied by M. }
    State.Transform := MatrixMult(State.Transform, IMRotateScaleOrient);
    State.InvertedTransform := MatrixMult(MRotateScaleOrient, State.InvertedTransform);
  end;

  TranslationMatrices(VectorNegate(Center.Value), M, IM);
  State.Transform := MatrixMult(State.Transform, M);
  State.InvertedTransform := MatrixMult(IM, State.InvertedTransform);

  State.AverageScaleTransform *=
    (Scale.Value[0] + Scale.Value[1] + Scale.Value[2]) / 3;
end;

procedure TransformSimpleState(
  State: TVRMLGraphTraverseState;
  rotation: TSFRotation;
  scale: TSFVec3f;
  translation: TSFVec3f);
var
  M, IM: TMatrix4Single;
begin
  { Inherited TVRMLGroupingNode already saved State.Transform and such.

    To make InvertedTransform, we multiply inverted matrices in inverted order
    below. }

  TranslationMatrices(Translation.Value, M, IM);
  State.Transform := MatrixMult(State.Transform, M);
  State.InvertedTransform := MatrixMult(IM, State.InvertedTransform);

  { Note that even Axis = zero is OK, both M and IM will be identity in
    this case. }
  RotationMatricesRad(Rotation.RotationRad, Rotation.Axis, M, IM);
  State.Transform := MatrixMult(State.Transform, M);
  State.InvertedTransform := MatrixMult(IM, State.InvertedTransform);

  if (Scale.Value[0] <> 1) or
     (Scale.Value[1] <> 1) or
     (Scale.Value[2] <> 1) then
  begin
    { For scaling, we explicitly request that if ScalingFactor contains
      zero, IM will be forced to be identity. That's because VRML allows
      scaling factor to have 0 components (we need InvertedTransform only
      for special tricks). }

    ScalingMatrices(Scale.Value, true, M, IM);
    State.Transform := MatrixMult(State.Transform, M);
    State.InvertedTransform := MatrixMult(IM, State.InvertedTransform);
  end;

  State.AverageScaleTransform *=
    (Scale.Value[0] + Scale.Value[1] + Scale.Value[2]) / 3;
end;

procedure TNodeTransform_2.BeforeTraverse(var State: TVRMLGraphTraverseState);
begin
  inherited;
  TransformState(State, Fdcenter, Fdrotation, Fdscale, FdscaleOrientation,
    Fdtranslation);
end;

function TNodeTransform_2.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

procedure RegisterGroupingNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeGroup_2,
    TNodeStaticGroup,
    TNodeSwitch_2,
    TNodeTransform_2
  ]);
end;
{$endif read_implementation}
