{
  Copyright 2002-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { Base node that may contain children nodes.
    Note that this descends from TAbstractChildNode,
    so it can be inserted as child of another grouping node.
    So, you can create a hierarchy of nodes with any depth.

    Handles TTransformFunctionality for any descendant that defines an instance of it. }
  TAbstractGroupingNode = class(TAbstractChildNode)
  strict private
    type
      TGeneralGroupingEnumerator = class
        State: TX3DGraphTraverseState;
        WasPointingDeviceSensor: boolean;
        function EnumerateChildrenFunction(Node, Child: TX3DNode): Pointer;
      end;
    var
      BeforeTraversePushedState: boolean;
      TempEnumerator: TGeneralGroupingEnumerator;
      TempEnumeratorUsed: Boolean;
  protected
    { If @true (default implementation in this class), then this really
      behaves like a grouping node. If @false, this allows everything
      to "leak out" (transform changes, VRML 1.0 state changes etc.).
      Possibility useful for VRML 1.0 extension
      https://castle-engine.io/x3d_extensions.php#section_ext_wwwinline_separate }
    function SeparateGroup: boolean; virtual;

    procedure BeforeTraverse(StateStack: TX3DGraphTraverseStateStack); override;
    procedure AfterTraverse(StateStack: TX3DGraphTraverseStateStack); override;
  public
    procedure CreateNode; override;
    destructor Destroy; override;
  end;

  { Base node that contains children nodes, in X3D and VRML 2.0. }
  TAbstractX3DGroupingNode = class(TAbstractGroupingNode)
  strict private
    type
      TChildrenArray = array of TAbstractChildNode;
    function ToChildrenArray(const Nodes: TMFNode): TChildrenArray;
    procedure EventAddChildrenReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
    procedure EventRemoveChildrenReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
  public
    procedure CreateNode; override;

    { Event in } { }
    strict private FEventAddChildren: TMFNodeEvent;
    public property EventAddChildren: TMFNodeEvent read FEventAddChildren;

    { Event in } { }
    strict private FEventRemoveChildren: TMFNodeEvent;
    public property EventRemoveChildren: TMFNodeEvent read FEventRemoveChildren;

    strict private FFdChildren: TMFNode;
    public property FdChildren: TMFNode read FFdChildren;

    strict private FFdBboxCenter: TSFVec3f;
    public property FdBboxCenter: TSFVec3f read FFdBboxCenter;

    strict private FFdBboxSize: TSFVec3f;
    public property FdBboxSize: TSFVec3f read FFdBboxSize;

    {$ifndef CASTLE_SLIM_NODES}
    strict private FFdVisible: TSFBool;
    public property FdVisible: TSFBool read FFdVisible;

    strict private FFdBBoxDisplay: TSFBool;
    public property FdBBoxDisplay: TSFBool read FFdBBoxDisplay;
    {$endif}

    { Add the nodes as children.

      By default, adding the same node multiple times makes
      the node present on the list multiple times.
      This AddChildren behavior is consistent with how
      @code(FdChildren.Add(Child)) behaves,
      and consistent with how @link(RemoveChildren) works
      (it removes only a single copy in case of duplicates).
      Our engine allows duplicates on the children list ---
      you can specify them in the X3D file,
      or you can add them using @link(FdChildren).Add.

      If you specify AllowDuplicates = @false,
      then adding a node that already exists on the children list is ignored.
      This has a tiny performance hit (we need to check for existence first),
      but it is consistent with how X3D event "addChildren" should behave
      according to the spec.
      The X3D specification doesn't allow duplicates on the children list.

      This propagates the changes appropriately to the parent TCastleSceneCore,
      calling TCastleSceneCore.ChangedAll or something similar. }
    procedure AddChildren(const Children: array of TAbstractChildNode;
      const AllowDuplicates: Boolean = true); overload;
    procedure AddChildren(const Child: TAbstractChildNode;
      const AllowDuplicates: Boolean = true); overload;

    { Remove the nodes from the children list.
      Removing a node that does not exist on the children list is ignored.

      If a node exists on the current children list multiple times,
      then a single occurrence of this node is removed,
      for each occurrence of this node in the parameters list.

      This propagates the changes appropriately to the parent TCastleSceneCore,
      calling TCastleSceneCore.ChangedAll or something similar. }
    procedure RemoveChildren(const Children: array of TAbstractChildNode); overload;
    procedure RemoveChildren(const Child: TAbstractChildNode); overload;

    { Clear all children from the list.
      This automatically propagates the changes appropriately
      to the parent TCastleSceneCore. }
    procedure ClearChildren;

    {$I auto_generated_node_helpers/x3dnodes_x3dgroupingnode.inc}
  end;

  { Contains children nodes without introducing a new transformation.
    It is equivalent to a TTransformNode containing an identity transform. }
  TGroupNode = class(TAbstractX3DGroupingNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;

    {$I auto_generated_node_helpers/x3dnodes_group.inc}
  end;
  TGroupNode_2 = TGroupNode;

  TStringLongIntMap = class({$ifdef FPC}specialize{$endif} TDictionary<string, LongInt>)
  strict private
    function GetItems(const AKey: string): LongInt;
    procedure SetItems(const AKey: string; const AValue: LongInt);
  public
    { Assign contents (all keys, values) of another TStringLongIntMap instance. }
    procedure Assign(const Source: TStringLongIntMap);

    { Access dictionary items.
      Setting this is allowed regardless if the key previously existed or not,
      in other words: setting this does AddOrSetValue, contrary to the ancestor TDictionary
      that only allows setting when the key already exists. }
    property Items [const AKey: string]: LongInt read GetItems write SetItems; default;
  end;

  { Node functionality for all X3D transformation nodes.
    These nodes transform all their children (the ones enumerated
    by traversing, so all active children).

    The transformation change may be signalled by TX3DField.Changes
    including chTransform flag. For some special nodes, like Billboard,
    the transformation changes are automatically managed. }
  TTransformFunctionality = class(TNodeFunctionality)
  public
    { Change State.Transformation for children nodes.
      Almost all descendants should override only the overload changing
      TTransformation. }
    procedure ApplyTransform(const State: TX3DGraphTraverseState); overload; virtual;
    procedure ApplyTransform(var Transformation: TTransformation); overload; virtual;
  end;

  { A top-level VRML/X3D node. This is what you get by loading 3D model from file.

    It is declared as a descendant of VRML/X3D >= 2.0 Group node,
    but it's used with all VRML/X3D versions (including VRML 1.0 and Inventor).
    This makes things simple (previously we had two separate TX3DRootNode_1
    and TX3DRootNode, which was complicating stuff).
    Children (for all VRML/X3D versions) are inside FdChildren field.

    This way VRML/X3D files may have many nodes at the top level
    (which is a standard feature of VRML/X3D >= 2.0, but we also allow it for
    VRML 1.0 as a commonly used extension). It may also have prototypes,
    routes etc. at the root level.

    This also allows us to record in one place some information that
    is returned by the parser. Like parsed VRML/X3D version, X3D profile,
    some namespace information (exported names and such). }
  TX3DRootNode = class(TGroupNode)
  strict private
    type
      TX3DRootNodeTransformFunctionality = class(TTransformFunctionality)
      strict private
        FParent: TX3DRootNode;
      public
        constructor Create(const AParent: TX3DRootNode);
        property Parent: TX3DRootNode read FParent;
        procedure ApplyTransform(var Transformation: TTransformation); override;
      end;
    var
      FTransformFunctionality: TX3DRootNodeTransformFunctionality;
      FHasForceVersion: boolean;
      FSaveAsRootNode: boolean;
      FProfile: string;
      FComponents: TStringLongIntMap;
      FMeta: TStringStringMap;
      FScale: Single;
  private
    FPrototypeNames: TX3DPrototypeNames;
    FExportedNames: TX3DExportList;
    FImportedNames: TX3DNodeNames;
    procedure TopLevelCheckChild(const Child: TX3DNode);
  protected
    function DeepCopyCore(CopyState: TX3DNodeDeepCopyState): TX3DNode; override;

    { Root node never saves/restores the traversing state.
      This means that all state changes "leak out" from a root node.
      This is a good thing: if root node isn't used as a final TCastleSceneCore.RootNode,
      then it is placed inside Inline node, which will do save/restore anyway.
      And this way VRML 1.0 WWWInline with separate=FALSE may also work too. }
    function SeparateGroup: boolean; override;
  public
    ForceVersion: TX3DVersion;

    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;
    destructor Destroy; override;
    function TransformationChange: TNodeTransformationChange; override;

    { Should SaveToStream take care to nicely save us, treating as
      a root node of whole VRML file or prototype.

      If this node isn't a root node, you should set this to @false
      (or saving this node may generate dumb contents).
      Although, generally, you should avoid using the TX3DRootNode
      class at all for non-root nodes. }
    property SaveAsRootNode: boolean read FSaveAsRootNode write FSaveAsRootNode
      default true;

    { Save contents to the stream.

      If SaveAsRootNode then this is saved in a special way,
      such that only the contents are written, without surrounding
      braces (for classic encoding) or XML element (for xml encoding).
      This way, when saving, we hide the fact that everything was wrapped
      in an artificial TX3DRootNode. }
    procedure SaveToStream(Writer: TX3DWriter); override;

    { Set HasForceVersion to @true to force saving this model with
      given ForceVersion. }
    property HasForceVersion: boolean
      read FHasForceVersion write FHasForceVersion default false;

    { Profile required for this X3D file. See X3D spec about profiles.
      Every X3D file must always define a profile.
      (However, we use this class also for VRML 1.0 and 2.0 root nodes,
      where profile is empty.)
      Relevant only if this node is the root of X3D file. }
    property Profile: string read FProfile write FProfile;

    { Components and levels required for this X3D file.
      Relevant if this node is the root of X3D file. }
    property Components: TStringLongIntMap read FComponents;

    { Meta keys and values of this X3D file.
      Relevant if this node is the root of X3D file. }
    property Meta: TStringStringMap read FMeta;

    { Set properties to force saving this node graph as X3D.
      If we're already configured to save as X3D (major version >= 3)
      then do nothing. Otherwise, sets major/minor versions
      (by default: for X3D 3.2; make sure AMajor is >= 3)
      and sets X3D profile (default: Interchange). }
    procedure ForceSaveAsX3D(const AMajor: Integer = 3;
      const AMinor: Integer = 2;
      const AProfile: string = 'Interchange');

    { Global prototype namespace at the end of parsing the file.
      Useful for things like EXTERNPROTO implementation. }
    property PrototypeNames: TX3DPrototypeNames read FPrototypeNames;

    { Node names exported from the file by EXPORT keyword.
      Useful for importing them later, to handle IMPORT/EXPORT X3D mechanism. }
    property ExportedNames: TX3DExportList read FExportedNames;

    { Node names imported into this file by the IMPORT keyword.
      You may want to take this into account when searching nodes by name,
      this allows the user to rename nodes from Inlined files. }
    property ImportedNames: TX3DNodeNames read FImportedNames;

    { Scale of the model.
      When loading, this is set to honor @code("UNIT length ...")
      declaration in X3D 3.3.
      It scales model such that 1 unit = 1 meter. When one model is inlined
      inside another (e.g. using X3D Inline node), this is adjusted to
      the existing scale of the outer model (always to make the resulting
      complete node graph keep the assumption 1 unit = 1 meter). }
    property Scale: Single read FScale write FScale {$ifdef FPC}default 1.0{$endif};

    { Export node, to have its name known by X3D graph that includes this graph by "Inline" node.

      Use this if you build X3D graph by hand, and you want do add the EXPORT X3D clause.
      It adds the clause (using @link(AddExport), so it can be saved back to X3D file
      reliably) and also makes it immediately working (adding it to the proper namespace).

      @groupBegin }
    procedure ExportNode(const Node: TX3DNode; const ExportedAlias: string); overload;
    procedure ExportNode(const Node: TX3DNode); overload;
    { @groupEnd }

    { @exclude
      Fix X3D names inside to:
      - make sure all TX3DNode.X3DName within Node are unique
      - make sure all nodes that are part of ROUTE have non-empty names. }
    procedure InternalFixNodeNames;
  end;

  { Children nodes which cannot be modified.
    StaticGroup children are guaranteed to not change,
    send events, receive events or contain any USE
    references outside the StaticGroup.
    This allows to optimize this content for faster rendering and less memory usage. }
  TStaticGroupNode = class(TAbstractGroupingNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdChildren: TMFNode;
    public property FdChildren: TMFNode read FFdChildren;

    strict private FFdBboxCenter: TSFVec3f;
    public property FdBboxCenter: TSFVec3f read FFdBboxCenter;

    strict private FFdBboxSize: TSFVec3f;
    public property FdBboxSize: TSFVec3f read FFdBboxSize;

    {$I auto_generated_node_helpers/x3dnodes_staticgroup.inc}
  end;

  { Choose only one (or none) of the child nodes for processing.
    This allows to choose between a number of nodes for processing
    (rendering, collision detection), and/or to hide some nodes.

    Compatibility node: this class is used for both VRML 97 and X3D.
    Although field name changed for X3D (in VRML 97, "children"
    was called "choice"), this is handled seamlessly using our
    TX3DFieldOrEvent.AddAlternativeField mechanism. In other words,
    programmer simply works with FdChildren field, and when reading/writing
    VRML file we take care to actually use either "children" or "choice" name,
    depending on used VRML version. }
  TSwitchNode = class(TAbstractX3DGroupingNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdWhichChoice: TSFInt32;
    public property FdWhichChoice: TSFInt32 read FFdWhichChoice;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;


    function TransformationChange: TNodeTransformationChange; override;

    {$I auto_generated_node_helpers/x3dnodes_switch.inc}
  end;
  TSwitchNode_2 = TSwitchNode;

  { Grouping node that transforms (moves, rotates, scales) it's children. }
  TTransformNode = class(TAbstractX3DGroupingNode)
  strict private
    type
      TTransformNodeTransformFunctionality = class(TTransformFunctionality)
      strict private
        FParent: TTransformNode;
        //WarningNegativeScaleDone: boolean;
      public
        constructor Create(const AParent: TTransformNode);
        property Parent: TTransformNode read FParent;
        procedure ApplyTransform(var Transformation: TTransformation); override;
      end;
    var
      FTransformFunctionality: TTransformNodeTransformFunctionality;
  protected
    { }
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;
    function TransformationChange: TNodeTransformationChange; override;

    strict private FFdCenter: TSFVec3f;
    public property FdCenter: TSFVec3f read FFdCenter;

    strict private FFdRotation: TSFRotation;
    public property FdRotation: TSFRotation read FFdRotation;

    strict private FFdScale: TSFVec3f;
    public property FdScale: TSFVec3f read FFdScale;

    strict private FFdScaleOrientation: TSFRotation;
    public property FdScaleOrientation: TSFRotation read FFdScaleOrientation;

    strict private FFdTranslation: TSFVec3f;
    public property FdTranslation: TSFVec3f read FFdTranslation;

    {$I auto_generated_node_helpers/x3dnodes_transform.inc}
  end;
  TTransformNode_2 = TTransformNode;

{$endif read_interface}

{$ifdef read_implementation}

{ TGeneralGroupingEnumerator ------------------------------------------------- }

function TAbstractGroupingNode.TGeneralGroupingEnumerator.EnumerateChildrenFunction(
  Node, Child: TX3DNode): Pointer;
begin
  Result := nil;
  Child.GroupBeforeTraverse(State, WasPointingDeviceSensor);
end;

procedure GroupBeforeTraverse_AddPointingDeviceSensor(const Child: TX3DNode;
  const State: TX3DGraphTraverseState; var WasPointingDeviceSensor: Boolean);
begin
  { When we encounter the *first* pointing device sensor within a grouping node,
    it obscures the other pointing devices (from parent nodes).
    Other pointing device sensors within the same grouping node are added. }

  if not WasPointingDeviceSensor then
  begin
    State.PointingDeviceSensors.Clear;
    State.PointingDeviceSensors.Transformation := State.Transformation;
    WasPointingDeviceSensor := true;
  end;

  State.PointingDeviceSensors.Add(Child);
end;

{ TAbstractGroupingNode ------------------------------------------------------ }

procedure TAbstractGroupingNode.CreateNode;
begin
  inherited;
  TempEnumerator := TGeneralGroupingEnumerator.Create;
  TempEnumeratorUsed := false;
end;

destructor TAbstractGroupingNode.Destroy;
begin
  FreeAndNil(TempEnumerator);
  inherited;
end;

function TAbstractGroupingNode.SeparateGroup: boolean;
begin
  Result := true;
end;

procedure TAbstractGroupingNode.BeforeTraverse(
  StateStack: TX3DGraphTraverseStateStack);
var
  Enumerator: TGeneralGroupingEnumerator;
begin
  inherited;

  BeforeTraversePushedState := SeparateGroup;
  if BeforeTraversePushedState then
    StateStack.Push;

  if TransformFunctionality <> nil then
  begin
    { Note: Applying transformation cannot be done in overriden BeforeTraverse.

      Reason: for storing ClipPlanes transformations, we need to know
      transformed matrix inside TAbstractGroupingNode.BeforeTraverse implementation.
      And we also do StateStack.Push inside TAbstractGroupingNode.BeforeTraverse
      implementation... So ApplyTransform must happen in the middle of
      TAbstractGroupingNode.BeforeTraverse call. }

    TransformFunctionality.ApplyTransform(StateStack.Top);
  end;

  { We also use it to collect State.PointingDeviceSensors,
    as it turns our that the same approach is possible, for the same reasons.
    And the same goes for State.ClipPlanes.
    Moreover, we do it all in a single DirectEnumerateActive call,
    so this doesn't slow down the code.
  }

  if not TempEnumeratorUsed then
  begin
    // for speed, usually we can reuse the same TempEnumerator instance
    Enumerator := TempEnumerator;
    TempEnumeratorUsed := true;
  end else
    Enumerator := TGeneralGroupingEnumerator.Create;

  try
    // initialize complete Enumerator state, as it may be a reused TempEnumerator instance
    Enumerator.State := StateStack.Top;
    Enumerator.WasPointingDeviceSensor := false;

    { It's a little unclean to test "Self is ...", usually virtual
      methods should replace such constructs. But in this case, this is
      just the simplest solution. Anchor node serves as a pointing device
      for all it's children, so it just places itself in
      State.PointingDeviceSensors.

      Note that if Anchor node itself has other pointing device sensors
      directly inside, they can be siblings of Anchor.
      Tested with InstantReality and g3l_stapes_dbg.wrl from Robert Funnell.
      So it's correct to just call AddPointingDeviceSensor that wil set
      WasPointingDeviceSensor := true. }
    if Self is TAnchorNode then
      GroupBeforeTraverse_AddPointingDeviceSensor(Self, Enumerator.State, Enumerator.WasPointingDeviceSensor);

    DirectEnumerateActive({$ifdef FPC}@{$endif} Enumerator.EnumerateChildrenFunction);
  finally
    if Enumerator = TempEnumerator then
      TempEnumeratorUsed := false
    else
      FreeAndNil(Enumerator);
  end;
end;

procedure TAbstractGroupingNode.AfterTraverse(
  StateStack: TX3DGraphTraverseStateStack);
begin
  if BeforeTraversePushedState then
    StateStack.Pop;

  inherited;
end;

{ TAbstractX3DGroupingNode --------------------------------------------------- }

procedure TAbstractX3DGroupingNode.CreateNode;
begin
  inherited;

  FEventAddChildren := TMFNodeEvent.Create(Self, 'addChildren', true);
  AddEvent(FEventAddChildren);
  EventaddChildren.AddNotification({$ifdef FPC}@{$endif} EventAddChildrenReceive);

  FEventRemoveChildren := TMFNodeEvent.Create(Self, 'removeChildren', true);
  AddEvent(FEventRemoveChildren);
  EventRemoveChildren.AddNotification({$ifdef FPC}@{$endif} EventRemoveChildrenReceive);

  { Add TAbstractGeometryNode_1 to allow VRML 1.0 geometry nodes (that cannot be TAbstractChildNode)
    to be present at top-level of the file.
    Testcase: demo-models/vrml_1/castle_extensions/multi_root.wrl }
  FFdChildren := TMFNode.Create(Self, true, 'children', [TAbstractChildNode, TAbstractGeometryNode_1]);
   FdChildren.ChangeAlways := chGroupChildren;
  AddField(FFdChildren);

  FFdBboxCenter := TSFVec3f.Create(Self, false, 'bboxCenter', TVector3.Zero);
  AddField(FFdBboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdBboxSize := TSFVec3f.Create(Self, false, 'bboxSize', Vector3(-1, -1, -1));
  AddField(FFdBboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }

  {$ifndef CASTLE_SLIM_NODES}
  FFdVisible := TSFBool.Create(Self, true, 'visible', true);
  { Calling it "render" is an extension from InstantReality,
    see [http://instant-reality.com/documentation/nodetype/ChildGroup/].
    This extension was invented before X3D 4 added it. }
  FFdVisible.AddAlternativeName('render', 0);
  AddField(FFdVisible);

  FFdBBoxDisplay := TSFBool.Create(Self, true, 'bboxDisplay', false);
  AddField(FFdBBoxDisplay);
  {$endif}

  DefaultContainerField := 'children';
end;

function TAbstractX3DGroupingNode.ToChildrenArray(
  const Nodes: TMFNode): TChildrenArray;
var
  I: Integer;
begin
  SetLength(Result, Nodes.Count);
  for I := 0 to Nodes.Count - 1 do
  begin
    if not (Nodes[I] is TAbstractChildNode) then
    begin
      WritelnWarning('X3D', 'Node on addChildren or removeChildren list is not a TAbstractChildNode, ignoring event');
      SetLength(Result, 0);
      Exit;
    end;
    Result[I] := TAbstractChildNode(Nodes[I]);
  end;
end;

procedure TAbstractX3DGroupingNode.EventAddChildrenReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  AddChildren(ToChildrenArray(Value as TMFNode), false);
end;

procedure TAbstractX3DGroupingNode.EventRemoveChildrenReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  RemoveChildren(ToChildrenArray(Value as TMFNode));
end;

procedure TAbstractX3DGroupingNode.AddChildren(
  const Children: array of TAbstractChildNode;
  const AllowDuplicates: Boolean);
var
  N: TAbstractChildNode;
begin
  { ignore empty list, in particular from ToChildrenArray failure }
  if High(Children) = -1 then Exit;

  { Note: it would be slightly more correct to implement this inside
    EventAddChildrenReceive, and from AddChildren only create an event
    "addChildren". This would allow other receivers of EventAddChildren
    to react to this.

    But it is not necessary in practice,
    and it would be a little wasteful (creating a temporary TMFNode to send
    for each AddChildren call). So we don't do it now. }

  for N in Children do
    if AllowDuplicates or (FdChildren.IndexOf(N) = -1) then
      FdChildren.Add(N);

  if Scene <> nil then
    Scene.InternalChangedField(FdChildren, chGroupChildrenAdd);
end;

procedure TAbstractX3DGroupingNode.RemoveChildren(
  const Children: array of TAbstractChildNode);
var
  N: TAbstractChildNode;
begin
  { ignore empty list, in particular from ToChildrenArray failure }
  if High(Children) = -1 then Exit;

  { Note that X3D specification directly says
    "A children field is not allowed to directly contain
    multiple instances of the same node."
    Note that I didn't find such notice in VRML 97 specification.
    This seems to be undefined case in VRML 97?

    But our engine allows it, it makes AddChildren / RemoveChildren
    faster by default, and it's consistent with normal list operations in Pascal.
  }

  if Scene <> nil then
    Scene.BeforeNodesFree;

  for N in Children do
    FdChildren.Remove(N);

  if Scene <> nil then
    Scene.InternalChangedField(FdChildren);
end;

procedure TAbstractX3DGroupingNode.AddChildren(
  const Child: TAbstractChildNode;
  const AllowDuplicates: Boolean);
begin
  AddChildren([Child], AllowDuplicates);
end;

procedure TAbstractX3DGroupingNode.RemoveChildren(
  const Child: TAbstractChildNode);
begin
  RemoveChildren([Child]);
end;

procedure TAbstractX3DGroupingNode.ClearChildren;
begin
  if FdChildren.Count <> 0 then
  begin
    if Scene <> nil then
      Scene.BeforeNodesFree;
    FdChildren.Clear;
    if Scene <> nil then
      Scene.ChangedAll;
  end;
end;

procedure TGroupNode.CreateNode;
begin
  inherited;

  DefaultContainerField := 'children';
end;

class function TGroupNode.ClassX3DType: string;
begin
  Result := 'Group';
end;

class function TGroupNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

function TGroupNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := inherited;
  if Result <> nil then Exit;

  Result := FdChildren.Enumerate(Func);
  if Result <> nil then Exit;
end;

{ TStringLongIntMap ---------------------------------------------------------- }

procedure TStringLongIntMap.Assign(const Source: TStringLongIntMap);
var
  Pair: {$ifdef FPC}TDictionaryPair{$else}TPair<string, LongInt>{$endif};
begin
  Clear;
  for Pair in Source do
    Items[Pair.Key] := Pair.Value;
end;

function TStringLongIntMap.GetItems(const AKey: string): LongInt;
begin
  Result := inherited Items[AKey];
end;

procedure TStringLongIntMap.SetItems(const AKey: string; const AValue: LongInt);
begin
  AddOrSetValue(AKey, AValue);
end;

{ TTransformFunctionality ---------------------------------------------------- }

procedure TTransformFunctionality.ApplyTransform(const State: TX3DGraphTraverseState);
begin
  ApplyTransform(State.Transformation);
end;

procedure TTransformFunctionality.ApplyTransform(var Transformation: TTransformation);
begin
end;

{ TFixNamesHandler ----------------------------------------------------------- }

type
  TFixNamesHandler = class
  strict private
    var
      Visited: TList;
      Names: TCastleStringList;
      LogsDone: Cardinal;
    const
      MaxLogsDone = 5;
  public
    constructor Create;
    destructor Destroy; override;
    function HandleNode(ParentNode, Node: TX3DNode): Pointer;
  end;

constructor TFixNamesHandler.Create;
begin
  inherited;
  Names := TCastleStringList.Create;
  Names.CaseSensitive := true;
  Visited := TList.Create;
end;

destructor TFixNamesHandler.Destroy;
begin
  FreeAndNil(Names);
  FreeAndNil(Visited);
  inherited;
end;

function TFixNamesHandler.HandleNode(ParentNode, Node: TX3DNode): Pointer;

  { Change Node.X3DName to something not yet existing in Names.
    This knows that Node.X3DName is <> '' and it exists in Names,
    IOW on entry the Node is named, and the name is not unique. }
  procedure RenameUniquely(const Node: TX3DNode);

    procedure RenameUniquelyCore(const Node: TX3DNode; const Prefix: String; NextNumber: Int64);
    begin
      while Names.IndexOf(Prefix + IntToStr(NextNumber)) <> -1 do
        Inc(NextNumber);
      Node.X3DName := Prefix + IntToStr(NextNumber);
    end;

  var
    I: Integer;
    Number: Int64;
    OriginalName: String;
  begin
    OriginalName := Node.X3DName;
    I := BackPos('_', Node.X3DName);
    if I <> 0 then
    begin
      Number := StrToIntDef(SEnding(Node.X3DName, I + 1), -1);
      if Number >= 0 then
        RenameUniquelyCore(Node, Copy(Node.X3DName, 1, I - 1), Number + 1)
      else
        RenameUniquelyCore(Node, Node.X3DName + '_', 2);
    end else
      RenameUniquelyCore(Node, Node.X3DName + '_', 2);

    if LogsDone < MaxLogsDone then
    begin
      WritelnLog('Renaming node "%s" -> "%s" to have unique name', [
        OriginalName,
        Node.X3DName
      ]);
      Inc(LogsDone);
      if LogsDone = MaxLogsDone then
        WritelnLog('Further renames done in this file will not be reported, to not flood log with messages');
    end;
  end;

  { Assuming Node.X3DName <> '', make sure it is unique name. }
  procedure MakeUniquelyNamed(const Node: TX3DNode);
  var
    I: Integer;
  begin
    I := Names.IndexOf(Node.X3DName);
    if I = - 1 then
    begin
      // new name
      Names.AddObject(Node.X3DName, Node);
    end else
    if Names.Objects[I] <> Node then
    begin
      // name already taken by a different node
      RenameUniquely(Node);
      Names.AddObject(Node.X3DName, Node);
    end;
  end;

  { Make sure Node has a non-empty name, and unique name.
    Given Node may be nil (it is ignored then). }
  procedure MakeNamed(const Node: TX3DNode);
  begin
    if (Node <> nil) and (Node.X3DName = '') then
    begin
      Node.X3DName := Node.X3DType;
      if Node.X3DName = '' then
        Node.X3DName := 'Node';
      MakeUniquelyNamed(Node);
    end;
  end;

var
  Route: TX3DRoute;
  I: Integer;
begin
  Result := nil;

  { early exit if this node was already visited,
    to save time when traversing graph with DEF/USE. }
  if Visited.IndexOf(Node) <> -1 then
    Exit;
  Visited.Add(Node);

  { make sure node name is unique }
  if Node.X3DName <> '' then
    MakeUniquelyNamed(Node);

  { make sure nodes used in ROUTEs are uniquely named }
  for I := 0 to Node.RoutesCount - 1 do
  begin
    Route := Node.Routes[I];
    MakeNamed(Route.SourceNode);
    MakeNamed(Route.DestinationNode);
  end;

  { recursively check children }
  Node.DirectEnumerateAll({$ifdef FPC}@{$endif} Self.HandleNode);
end;

{ TX3DRootNode.TX3DRootNodeTransformFunctionality -------------------------------------- }

constructor TX3DRootNode.TX3DRootNodeTransformFunctionality.Create(const AParent: TX3DRootNode);
begin
  inherited Create(AParent);
  FParent := AParent;
end;

procedure TX3DRootNode.TX3DRootNodeTransformFunctionality.ApplyTransform(var Transformation: TTransformation);
var
  Scale: Single;
  M, IM: TMatrix4;
begin
  inherited;

  Scale := Parent.Scale;
  if Scale <> 1 then
  begin
    ScalingMatrices(Vector3(Scale, Scale, Scale), true, M, IM);
    Transformation.Transform := Transformation.Transform * M;
    Transformation.InverseTransform := IM * Transformation.InverseTransform;
    Transformation.Scale := Transformation.Scale * Scale;
  end;
end;

{ TX3DRootNode --------------------------------------------------------------- }

constructor TX3DRootNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  FComponents := TStringLongIntMap.Create;
  FMeta := TStringStringMap.Create;
  FSaveAsRootNode := true;
  FScale := 1;

  FTransformFunctionality := TX3DRootNodeTransformFunctionality.Create(Self);
  AddFunctionality(FTransformFunctionality);
end;

destructor TX3DRootNode.Destroy;
begin
  FreeAndNil(FComponents);
  FreeAndNil(FMeta);
  FreeAndNil(FPrototypeNames);
  FreeAndNil(FExportedNames);
  FreeAndNil(FImportedNames);
  inherited;
end;

procedure TX3DRootNode.SaveToStream(Writer: TX3DWriter);
var
  I: integer;
  FileItems: TX3DFileItemList;
begin
  if SaveAsRootNode then
  begin
    { Special things for root node saving:
      - Name is ignored (should be '').
      - Fields are ignored, except "children", that holds children
        (for any VRML version, including VRML 1).
        It's output in a special way, without writing "children [...]" around.
        Other fields are ignored (should never have any meaning). }

    FileItems := TX3DFileItemList.Create(false);
    try
      for I := 0 to PrototypesCount - 1 do
        FileItems.Add(Prototypes[I]);

      Assert(VRML1ChildrenCount = 0);

      for i := 0 to FdChildren.Count - 1 do
        FileItems.Add(FdChildren[I]);

      for I := 0 to RoutesCount - 1 do
        FileItems.Add(Routes[I]);

      for I := 0 to ImportsCount - 1 do
        FileItems.Add(ImportsList[I]);

      for I := 0 to ExportsCount - 1 do
        FileItems.Add(ExportsList[I]);

      FileItems.SaveToStream(Writer);
    finally FreeAndNil(FileItems) end;
  end else
    inherited;
end;

function TX3DRootNode.DeepCopyCore(CopyState: TX3DNodeDeepCopyState): TX3DNode;
var
  Res: TX3DRootNode;
begin
  Result := inherited;

  Res := Result as TX3DRootNode;

  Res.HasForceVersion := HasForceVersion;
  Res.ForceVersion := ForceVersion;
  Res.Scale := Scale;
  Res.Profile := Profile;
  Res.Components.Assign(Components);
  Res.Meta.Assign(Meta);
  if ExportedNames <> nil then
  begin
    FreeAndNil(Res.FExportedNames);
    Res.FExportedNames := ExportedNames.DeepCopy(CopyState);
  end;
  if ImportedNames <> nil then
  begin
    FreeAndNil(Res.FImportedNames);
    Res.FImportedNames := ImportedNames.DeepCopy(CopyState);
  end;
end;

procedure TX3DRootNode.ForceSaveAsX3D(const AMajor: Integer = 3;
  const AMinor: Integer = 2;
  const AProfile: string = 'Interchange');
begin
  if (not HasForceVersion) or
     (ForceVersion.Major < 3) then
  begin
    HasForceVersion := true;
    ForceVersion.Major := AMajor;
    ForceVersion.Minor := AMinor;
    Profile := AProfile;
  end;
end;

function TX3DRootNode.SeparateGroup: boolean;
begin
  Result := false;
end;

procedure TX3DRootNode.TopLevelCheckChild(const Child: TX3DNode);
begin
  { We could just do here FdChildren.WarningIfChildNotAllowed(Child),
    but the warning message of it is a little unfriendly: it uncovers
    to user that we have a special TX3DRootNode that descends from Group node.
    So instead, do it by hand.

    Also, add LayerSet (not allowed in Group.FdChildren).
    This is special (see http://www.web3d.org/x3d/specifications/ISO-IEC-19775-1.2-X3D-AbstractSpecification/Part01/concepts.html#Rootnodes),
    "4.3.2 Root nodes" from X3D 3.2 spec.

    Also add TAbstractGeometryNode_1 to enable VRML 1.0 geometry nodes at top-level,
    like in demo-models/vrml_1/castle_extensions/multi_root.wrl .
  }

  if not (
       (Child is TAbstractChildNode) or
       (Child is TAbstractGeometryNode_1) or
       (Child is TLayerSetNode)
     ) then
    WritelnWarning('VRML/X3D', Format('Node "%s" is not allowed at the top level of the X3D file',
      [Child.X3DType]));
end;

procedure TX3DRootNode.ExportNode(const Node: TX3DNode; const ExportedAlias: string);
var
  ExportItem: TX3DExport;
begin
  if Node.X3DName = '' then
    raise Exception.Create('You cannot export nodes with empty names');

  ExportItem := TX3DExport.Create;
  AddExport(ExportItem);
  ExportItem.ExportedNode := Node;
  if ExportedAlias <> Node.X3DName then
    ExportItem.ExportedAlias := ExportedAlias;

  { merely adding ExportItem is not enough, we have to also replicate what
    parsing of EXPORT clause does, and add it to ExportedNames. }
  if FExportedNames = nil then
    FExportedNames := TX3DExportList.Create(false); // do not own TX3DExport instances, they are owned by TX3DNode.Exports
  FExportedNames.Add(ExportItem);
end;

procedure TX3DRootNode.ExportNode(const Node: TX3DNode);
begin
  ExportNode(Node, Node.X3DName);
end;

function TX3DRootNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcTransform;
end;

procedure TX3DRootNode.InternalFixNodeNames;
var
  Handler: TFixNamesHandler;
begin
  Handler := TFixNamesHandler.Create;
  try
    Handler.HandleNode(nil, Self);
    DirectEnumerateAll({$ifdef FPC}@{$endif} Handler.HandleNode);
  finally FreeAndNil(Handler) end;
end;

{ TStaticGroupNode ----------------------------------------------------------- }

procedure TStaticGroupNode.CreateNode;
begin
  inherited;

  FFdChildren := TMFNode.Create(Self, false, 'children', [TAbstractChildNode]);
   FdChildren.ChangeAlways := chGroupChildren;
  AddField(FFdChildren);

  FFdBboxCenter := TSFVec3f.Create(Self, false, 'bboxCenter', Vector3(0, 0, 0));
  AddField(FFdBboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdBboxSize := TSFVec3f.Create(Self, false, 'bboxSize', Vector3(-1, -1, -1));
  AddField(FFdBboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }

  DefaultContainerField := 'children';
end;

class function TStaticGroupNode.ClassX3DType: string;
begin
  Result := 'StaticGroup';
end;

function TStaticGroupNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FdChildren.Enumerate(Func);
  if Result <> nil then Exit;
end;

{ TSwitchNode ---------------------------------------------------------------- }

procedure TSwitchNode.CreateNode;
begin
  inherited;

  Fdchildren.AddAlternativeName('choice', 2);

  FFdWhichChoice := TSFInt32.Create(Self, true, 'whichChoice', -1);
   FdWhichChoice.ChangeAlways := chSwitch2;
  AddField(FFdWhichChoice);
  { X3D specification comment: [-1,Inf) }

  DefaultContainerField := 'children';
end;

class function TSwitchNode.ClassX3DType: string;
begin
  Result := 'Switch';
end;

class function TSwitchNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

function TSwitchNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := nil;
  if Between(FdWhichChoice.Value, 0, FdChildren.Count - 1) then
  begin
    Result := Func(Self, FdChildren[FdWhichChoice.Value]);
    if Result <> nil then Exit;
  end;
end;

function TSwitchNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcSwitch;
end;

{ TTransformNode.TTransformNodeTransformFunctionality --------------------------------------- }

constructor TTransformNode.TTransformNodeTransformFunctionality.Create(const AParent: TTransformNode);
begin
  inherited Create(AParent);
  FParent := AParent;
end;

procedure TTransformNode.TTransformNodeTransformFunctionality.ApplyTransform(var Transformation: TTransformation);
begin
  // inherited; // no need to call, conserve speed in this critical place

  Transformation.Multiply(
    Parent.Center,
    Parent.Rotation,
    Parent.Scale,
    Parent.ScaleOrientation,
    Parent.Translation);

  {
  if ((Fdscale.Value[0] < 0) or
      (Fdscale.Value[1] < 0) or
      (Fdscale.Value[2] < 0)) and not WarningNegativeScaleDone then
  begin
    WarningNegativeScaleDone := true;
    WritelnLog('VRML/X3D', 'Note: Transform.scale has negative component. It allows some tricks (especially sensible for unlit 2D stuff), but it makes lighting act weird. Make sure you use it only for unlit objects.');
  end;
  }
end;

{ TTransformNode ------------------------------------------------------------- }

constructor TTransformNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  FTransformFunctionality := TTransformNodeTransformFunctionality.Create(Self);
  AddFunctionality(FTransformFunctionality);
end;

procedure TTransformNode.CreateNode;
begin
  inherited;

  FFdCenter := TSFVec3f.Create(Self, true, 'center', Vector3(0, 0, 0));
   FdCenter.ChangeAlways := chTransform;
  AddField(FFdCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdRotation := TSFRotation.Create(Self, true, 'rotation', Vector3(0, 0, 1), 0);
   FdRotation.ChangeAlways := chTransform;
  AddField(FFdRotation);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdScale := TSFVec3f.Create(Self, true, 'scale', Vector3(1, 1, 1));
   FdScale.ChangeAlways := chTransform;
  AddField(FFdScale);
  { X3D specification comment: (-Inf, Inf) }

  FFdScaleOrientation := TSFRotation.Create(Self, true, 'scaleOrientation', Vector3(0, 0, 1), 0);
   FdScaleOrientation.ChangeAlways := chTransform;
  AddField(FFdScaleOrientation);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdTranslation := TSFVec3f.Create(Self, true, 'translation', Vector3(0, 0, 0));
   FdTranslation.ChangeAlways := chTransform;
  AddField(FFdTranslation);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TTransformNode.ClassX3DType: string;
begin
  Result := 'Transform';
end;

class function TTransformNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

function TTransformNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FdChildren.Enumerate(Func);
  if Result <> nil then Exit;
end;

function TTransformNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcTransform;
end;

procedure RegisterGroupingNodes;
begin
  NodesManager.RegisterNodeClasses([
    TGroupNode,
    TStaticGroupNode,
    TSwitchNode,
    TTransformNode
  ]);
end;
{$endif read_implementation}
