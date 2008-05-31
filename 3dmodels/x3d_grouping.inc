{
  Copyright 2008 Michalis Kamburelis.

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
    This way TNodeX3DGroupingNode inherits also from X3DChildNode. }
  TVRMLGroupingNode = class(TNodeX3DChildNode)
  private
    OriginalState: TVRMLGraphTraverseState;
  protected
    procedure BeforeTraverse(var State: TVRMLGraphTraverseState); override;
    procedure AfterTraverse(var State: TVRMLGraphTraverseState); override;
  end;

  INodeX3DBoundedObject = interface(IVRMLNode)
    property FdbboxCenter: TSFVec3f { read GetFdbboxCenter }; { }
    property FdbboxSize: TSFVec3f { read GetFdbboxSize }; { }
  end;

  TNodeX3DGroupingNode = class(TVRMLGroupingNode, INodeX3DBoundedObject)
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

    function ChildrenField: TMFNode; override;
  end;

  { X3D Switch node.

    This is implemented as a different class than VRML 2.0 Switch
    node (TNodeSwitch_2) as VRML 2.0 Switch is not perfectly compatible.
    In particular, in X3D field name changed from "choice" to "children".
    This seems like a trivial change, but note that events, routes,
    scripts may all refer to the field name, so implementing a simple
    "alias field name" is actually non-trivial (and not worthy, if it
    can be fixed by simply making separate TNodeSwitch_3). }
  TNodeSwitch_3 = class(TNodeX3DGroupingNode)
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

{$endif read_interface}

{$ifdef read_implementation}
type
  TGeneralGroupingEnumerator = class
    Lights: TDynActiveLightArray;
    State: TVRMLGraphTraverseState;
    procedure EnumerateChildrenFunction(Node, Child: TVRMLNode);
  end;

  procedure TGeneralGroupingEnumerator.EnumerateChildrenFunction(
    Node, Child: TVRMLNode);
  begin
    if Child is TNodeGeneralDirectionalLight then
      { We do CreateActiveLight with State from TVRMLGroupingNode node,
        while precisely we should rather use State from TNodeDirectionalLight
        traverse. But, fortunately, State of TNodeDirectionalLight doesn't
        change inside (TNodeDirectionalLight doesn't do anything inside
        BeforeTraverse), so this is the same thing. }
      State.VRML2ActiveLights.AppendItem(
        TNodeGeneralDirectionalLight(Child).CreateActiveLight(State));
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
    and do it as post-processing step, i.e. in TVRMLFlatScene in
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

  Enumerator := TGeneralGroupingEnumerator.Create;
  try
    Enumerator.State := State;
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

  FEventaddChildren := TVRMLEvent.Create('addChildren', TMFNode, true);
  Events.Add(FEventaddChildren);

  FEventremoveChildren := TVRMLEvent.Create('removeChildren', TMFNode, true);
  Events.Add(FEventremoveChildren);

  FFdchildren := TMFNode.Create(Self, 'children', AllowedChildrenNodes);
  Fields.Add(FFdchildren);

{ will be added automatically, since TNodeX3DGroupingNode
  will have X3DNode as ancestor
  FFdmetadata := TSFNode.Create(Self, 'metadata', [INodeX3DMetadataObject_Descendants]);
  Fields.Add(FFdmetadata);}

  FFdbboxCenter := TSFVec3f.Create('bboxCenter', ZeroVector3Single);
  FFdbboxCenter.Exposed := false;
  Fields.Add(FFdbboxCenter);

  FFdbboxSize := TSFVec3f.Create('bboxSize', Vector3Single(-1, -1, -1));
  FFdbboxSize.Exposed := false;
  Fields.Add(FFdbboxSize);
end;

function TNodeX3DGroupingNode.ChildrenField: TMFNode;
begin
  Result := FdChildren;
end;

class function TNodeSwitch_3.ClassNodeTypeName: string;
begin
  Result := 'Switch';
end;

class function TNodeSwitch_3.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeSwitch_3.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdwhichChoice := TSFInt32.Create('whichChoice', -1);
  Fields.Add(FFdwhichChoice);

  DefaultContainerField := 'children';
end;

class function TNodeSwitch_3.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 3;
end;

function TNodeSwitch_3.ChildrenField: TMFNode;
begin
  Result := FdChildren;
end;

procedure TNodeSwitch_3.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
begin
  if Between(FdWhichChoice.Value, 0, FdChildren.Count - 1) then
    Func(Self, FdChildren.Items[FdWhichChoice.Value]);
end;

{$endif read_implementation}
