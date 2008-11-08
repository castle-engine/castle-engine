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

{ This file defines all nodes from VRML 2.0 (aka 97) specification
  (that didn't make it into X3D). } { }

{$ifdef read_interface}
  TNodeCoordinateDeformer = class(TVRMLGroupingNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    { Event: MFNode, in } { }
    private FEventaddChildren: TVRMLEvent;
    public property EventaddChildren: TVRMLEvent read FEventaddChildren;

    { Event: MFNode, in } { }
    private FEventremoveChildren: TVRMLEvent;
    public property EventremoveChildren: TVRMLEvent read FEventremoveChildren;

    private FFdchildren: TMFNode;
    public property Fdchildren: TMFNode read FFdchildren;

    private FFdcontrolPoint: TMFVec3f;
    public property FdcontrolPoint: TMFVec3f read FFdcontrolPoint;

    private FFdinputCoord: TMFNode;
    public property FdinputCoord: TMFNode read FFdinputCoord;

    private FFdinputTransform: TMFNode;
    public property FdinputTransform: TMFNode read FFdinputTransform;

    private FFdoutputCoord: TMFNode;
    public property FdoutputCoord: TMFNode read FFdoutputCoord;

    private FFdweight: TMFFloat;
    public property Fdweight: TMFFloat read FFdweight;

    private FFdbboxCenter: TSFVec3f;
    public property FdbboxCenter: TSFVec3f read FFdbboxCenter;

    private FFdbboxSize: TSFVec3f;
    public property FdbboxSize: TSFVec3f read FFdbboxSize;

    private FFduDimension: TSFInt32;
    public property FduDimension: TSFInt32 read FFduDimension;

    private FFduKnot: TMFFloat;
    public property FduKnot: TMFFloat read FFduKnot;

    private FFduOrder: TSFInt32;
    public property FduOrder: TSFInt32 read FFduOrder;

    private FFdvDimension: TSFInt32;
    public property FdvDimension: TSFInt32 read FFdvDimension;

    private FFdvKnot: TMFFloat;
    public property FdvKnot: TMFFloat read FFdvKnot;

    private FFdvOrder: TSFInt32;
    public property FdvOrder: TSFInt32 read FFdvOrder;

    private FFdwDimension: TSFInt32;
    public property FdwDimension: TSFInt32 read FFdwDimension;

    private FFdwKnot: TMFFloat;
    public property FdwKnot: TMFFloat read FFdwKnot;

    private FFdwOrder: TSFInt32;
    public property FdwOrder: TSFInt32 read FFdwOrder;

    function ChildrenField: TMFNode; override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeInlineLoadControl = class(TVRMLGroupingNode, IVRMLInlineNode)
  private
    procedure EventLoadReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
    procedure EventUrlReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
  protected
    procedure BeforeTraverse(var State: TVRMLGraphTraverseState); override;
    procedure AfterTraverse(var State: TVRMLGraphTraverseState); override;
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
  end;

  TNodeNurbsCurve_2 = class(TNodeX3DGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdcontrolPoint: TMFVec3f;
    public property FdcontrolPoint: TMFVec3f read FFdcontrolPoint;

    private FFdweight: TMFFloat;
    public property Fdweight: TMFFloat read FFdweight;

    private FFdtessellation: TSFInt32;
    public property Fdtessellation: TSFInt32 read FFdtessellation;

    private FFdknot: TMFFloat;
    public property Fdknot: TMFFloat read FFdknot;

    private FFdorder: TSFInt32;
    public property Fdorder: TSFInt32 read FFdorder;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean; override;

    { Geometry node not implemented } { }
    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeNurbsGroup = class(TNodeX3DGroupingNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdtessellationScale: TSFFloat;
    public property FdtessellationScale: TSFFloat read FFdtessellationScale;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeNurbsPositionInterpolator_2 = class(TNodeX3DChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    { Event: SFFloat, in } { }
    private FEventset_fraction: TVRMLEvent;
    public property Eventset_fraction: TVRMLEvent read FEventset_fraction;

    private FFddimension: TSFInt32;
    public property Fddimension: TSFInt32 read FFddimension;

    private FFdkeyValue: TMFVec3f;
    public property FdkeyValue: TMFVec3f read FFdkeyValue;

    private FFdkeyWeight: TMFFloat;
    public property FdkeyWeight: TMFFloat read FFdkeyWeight;

    private FFdknot: TMFFloat;
    public property Fdknot: TMFFloat read FFdknot;

    private FFdorder: TSFInt32;
    public property Fdorder: TSFInt32 read FFdorder;

    { Event: SFVec3f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean; override;
  end;

  TNodeNurbsSurface = class(TNodeX3DGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdcontrolPoint: TMFVec3f;
    public property FdcontrolPoint: TMFVec3f read FFdcontrolPoint;

    private FFdtexCoord: TSFNode;
    public property FdtexCoord: TSFNode read FFdtexCoord;

    private FFduTessellation: TSFInt32;
    public property FduTessellation: TSFInt32 read FFduTessellation;

    private FFdvTessellation: TSFInt32;
    public property FdvTessellation: TSFInt32 read FFdvTessellation;

    private FFdweight: TMFFloat;
    public property Fdweight: TMFFloat read FFdweight;

    private FFdccw: TSFBool;
    public property Fdccw: TSFBool read FFdccw;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;

    private FFduDimension: TSFInt32;
    public property FduDimension: TSFInt32 read FFduDimension;

    private FFduKnot: TMFFloat;
    public property FduKnot: TMFFloat read FFduKnot;

    private FFduOrder: TSFInt32;
    public property FduOrder: TSFInt32 read FFduOrder;

    private FFdvDimension: TSFInt32;
    public property FdvDimension: TSFInt32 read FFdvDimension;

    private FFdvKnot: TMFFloat;
    public property FdvKnot: TMFFloat read FFdvKnot;

    private FFdvOrder: TSFInt32;
    public property FdvOrder: TSFInt32 read FFdvOrder;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Geometry node not implemented } { }
    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeNurbsTextureSurface = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdcontrolPoint: TMFVec2f;
    public property FdcontrolPoint: TMFVec2f read FFdcontrolPoint;

    private FFdweight: TMFFloat;
    public property Fdweight: TMFFloat read FFdweight;

    private FFduDimension: TSFInt32;
    public property FduDimension: TSFInt32 read FFduDimension;

    private FFduKnot: TMFFloat;
    public property FduKnot: TMFFloat read FFduKnot;

    private FFduOrder: TSFInt32;
    public property FduOrder: TSFInt32 read FFduOrder;

    private FFdvDimension: TSFInt32;
    public property FdvDimension: TSFInt32 read FFdvDimension;

    private FFdvKnot: TMFFloat;
    public property FdvKnot: TMFFloat read FFdvKnot;

    private FFdvOrder: TSFInt32;
    public property FdvOrder: TSFInt32 read FFdvOrder;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeTrimmedSurface = class(TNodeX3DGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    { Event: MFNode, in } { }
    private FEventaddTrimmingContour: TVRMLEvent;
    public property EventaddTrimmingContour: TVRMLEvent read FEventaddTrimmingContour;

    { Event: MFNode, in } { }
    private FEventremoveTrimmingContour: TVRMLEvent;
    public property EventremoveTrimmingContour: TVRMLEvent read FEventremoveTrimmingContour;

    private FFdtrimmingContour: TMFNode;
    public property FdtrimmingContour: TMFNode read FFdtrimmingContour;

    private FFdsurface: TSFNode;
    public property Fdsurface: TSFNode read FFdsurface;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Geometry node not implemented } { }
    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;
{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeCoordinateDeformer.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FEventaddChildren := TVRMLEvent.Create(Self, 'addChildren', TMFNode, true);
  Events.Add(FEventaddChildren);

  FEventremoveChildren := TVRMLEvent.Create(Self, 'removeChildren', TMFNode, true);
  Events.Add(FEventremoveChildren);

  FFdchildren := TMFNode.Create(Self, 'children', INodeX3DChildNode);
  Fields.Add(FFdchildren);

  FFdcontrolPoint := TMFVec3f.Create(Self, 'controlPoint', []);
  Fields.Add(FFdcontrolPoint);

  FFdinputCoord := TMFNode.Create(Self, 'inputCoord', [TNodeCoordinate]);
  Fields.Add(FFdinputCoord);

  FFdinputTransform := TMFNode.Create(Self, 'inputTransform', [TNodeTransform_2]);
  Fields.Add(FFdinputTransform);

  FFdoutputCoord := TMFNode.Create(Self, 'outputCoord', [TNodeCoordinate]);
  Fields.Add(FFdoutputCoord);

  FFdweight := TMFFloat.Create(Self, 'weight', []);
  Fields.Add(FFdweight);

  FFdbboxCenter := TSFVec3f.Create(Self, 'bboxCenter', ZeroVector3Single);
  FFdbboxCenter.Exposed := false;
  Fields.Add(FFdbboxCenter);

  FFdbboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
  FFdbboxSize.Exposed := false;
  Fields.Add(FFdbboxSize);

  FFduDimension := TSFInt32.Create(Self, 'uDimension', 0);
  FFduDimension.Exposed := false;
  Fields.Add(FFduDimension);

  FFduKnot := TMFFloat.Create(Self, 'uKnot', []);
  FFduKnot.Exposed := false;
  Fields.Add(FFduKnot);

  FFduOrder := TSFInt32.Create(Self, 'uOrder', 2);
  FFduOrder.Exposed := false;
  Fields.Add(FFduOrder);

  FFdvDimension := TSFInt32.Create(Self, 'vDimension', 0);
  FFdvDimension.Exposed := false;
  Fields.Add(FFdvDimension);

  FFdvKnot := TMFFloat.Create(Self, 'vKnot', []);
  FFdvKnot.Exposed := false;
  Fields.Add(FFdvKnot);

  FFdvOrder := TSFInt32.Create(Self, 'vOrder', 2);
  FFdvOrder.Exposed := false;
  Fields.Add(FFdvOrder);

  FFdwDimension := TSFInt32.Create(Self, 'wDimension', 0);
  FFdwDimension.Exposed := false;
  Fields.Add(FFdwDimension);

  FFdwKnot := TMFFloat.Create(Self, 'wKnot', []);
  FFdwKnot.Exposed := false;
  Fields.Add(FFdwKnot);

  FFdwOrder := TSFInt32.Create(Self, 'wOrder', 2);
  FFdwOrder.Exposed := false;
  Fields.Add(FFdwOrder);

  DefaultContainerField := 'children';
end;

class function TNodeCoordinateDeformer.ClassNodeTypeName: string;
begin
  Result := 'CoordinateDeformer';
end;

class function TNodeCoordinateDeformer.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeCoordinateDeformer.ChildrenField: TMFNode;
begin
  Result := FdChildren;
end;

function TNodeCoordinateDeformer.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

  if ChildrenCount > 0 then
  begin
    if CanReload then RemoveAllChildren else Exit;
  end;

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
        { pamietajmy ze VRMLNonFatalError moze spowodowac rzucenie wyjatku
          (chociaz nie musi) }
        VRMLNonFatalError('Exception ' + E.ClassName +
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
end;

procedure TNodeInlineLoadControl.BeforeTraverse(var State: TVRMLGraphTraverseState);
begin
  inherited;
  LoadInlined(false);
  Inc(State.InsideInline);
end;

procedure TNodeInlineLoadControl.AfterTraverse(var State: TVRMLGraphTraverseState);
begin
  Dec(State.InsideInline);
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

constructor TNodeNurbsCurve_2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdcontrolPoint := TMFVec3f.Create(Self, 'controlPoint', []);
  Fields.Add(FFdcontrolPoint);

  FFdweight := TMFFloat.Create(Self, 'weight', []);
  Fields.Add(FFdweight);

  FFdtessellation := TSFInt32.Create(Self, 'tessellation', 0);
  Fields.Add(FFdtessellation);

  FFdknot := TMFFloat.Create(Self, 'knot', []);
  FFdknot.Exposed := false;
  Fields.Add(FFdknot);

  FFdorder := TSFInt32.Create(Self, 'order', 3);
  FFdorder.Exposed := false;
  Fields.Add(FFdorder);
end;

class function TNodeNurbsCurve_2.ClassNodeTypeName: string;
begin
  Result := 'NurbsCurve';
end;

class function TNodeNurbsCurve_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeNurbsCurve_2.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeNurbsCurve_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor = 2;
end;

{$define TGeometryNotImplemented := TNodeNurbsCurve_2}
GeometryNotImplemented

constructor TNodeNurbsGroup.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdtessellationScale := TSFFloat.Create(Self, 'tessellationScale', 1.0);
  Fields.Add(FFdtessellationScale);
end;

class function TNodeNurbsGroup.ClassNodeTypeName: string;
begin
  Result := 'NurbsGroup';
end;

class function TNodeNurbsGroup.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeNurbsGroup.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

constructor TNodeNurbsPositionInterpolator_2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FEventset_fraction := TVRMLEvent.Create(Self, 'set_fraction', TSFFloat, true);
  Events.Add(FEventset_fraction);

  FFddimension := TSFInt32.Create(Self, 'dimension', 0);
  Fields.Add(FFddimension);

  FFdkeyValue := TMFVec3f.Create(Self, 'keyValue', []);
  Fields.Add(FFdkeyValue);

  FFdkeyWeight := TMFFloat.Create(Self, 'keyWeight', []);
  Fields.Add(FFdkeyWeight);

  FFdknot := TMFFloat.Create(Self, 'knot', []);
  Fields.Add(FFdknot);

  FFdorder := TSFInt32.Create(Self, 'order', 4);
  Fields.Add(FFdorder);

  FEventvalue_changed := TVRMLEvent.Create(Self, 'value_changed', TSFVec3f, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';
end;

class function TNodeNurbsPositionInterpolator_2.ClassNodeTypeName: string;
begin
  Result := 'NurbsPositionInterpolator';
end;

class function TNodeNurbsPositionInterpolator_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeNurbsPositionInterpolator_2.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeNurbsPositionInterpolator_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor = 2;
end;

constructor TNodeNurbsSurface.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdcontrolPoint := TMFVec3f.Create(Self, 'controlPoint', []);
  Fields.Add(FFdcontrolPoint);

  FFdtexCoord := TSFNode.Create(Self, 'texCoord', [TNodeTextureCoordinate, TNodeNurbsTextureSurface]);
  Fields.Add(FFdtexCoord);

  FFduTessellation := TSFInt32.Create(Self, 'uTessellation', 0);
  Fields.Add(FFduTessellation);

  FFdvTessellation := TSFInt32.Create(Self, 'vTessellation', 0);
  Fields.Add(FFdvTessellation);

  FFdweight := TMFFloat.Create(Self, 'weight', []);
  Fields.Add(FFdweight);

  FFdccw := TSFBool.Create(Self, 'ccw', TRUE);
  FFdccw.Exposed := false;
  Fields.Add(FFdccw);

  FFdsolid := TSFBool.Create(Self, 'solid', TRUE);
  FFdsolid.Exposed := false;
  Fields.Add(FFdsolid);

  FFduDimension := TSFInt32.Create(Self, 'uDimension', 0);
  FFduDimension.Exposed := false;
  Fields.Add(FFduDimension);

  FFduKnot := TMFFloat.Create(Self, 'uKnot', []);
  FFduKnot.Exposed := false;
  Fields.Add(FFduKnot);

  FFduOrder := TSFInt32.Create(Self, 'uOrder', 3);
  FFduOrder.Exposed := false;
  Fields.Add(FFduOrder);

  FFdvDimension := TSFInt32.Create(Self, 'vDimension', 0);
  FFdvDimension.Exposed := false;
  Fields.Add(FFdvDimension);

  FFdvKnot := TMFFloat.Create(Self, 'vKnot', []);
  FFdvKnot.Exposed := false;
  Fields.Add(FFdvKnot);

  FFdvOrder := TSFInt32.Create(Self, 'vOrder', 3);
  FFdvOrder.Exposed := false;
  Fields.Add(FFdvOrder);
end;

class function TNodeNurbsSurface.ClassNodeTypeName: string;
begin
  Result := 'NurbsSurface';
end;

class function TNodeNurbsSurface.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeNurbsSurface.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

{$define TGeometryNotImplemented := TNodeNurbsSurface}
GeometryNotImplemented

constructor TNodeNurbsTextureSurface.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdcontrolPoint := TMFVec2f.Create(Self, 'controlPoint', []);
  Fields.Add(FFdcontrolPoint);

  FFdweight := TMFFloat.Create(Self, 'weight', []);
  Fields.Add(FFdweight);

  FFduDimension := TSFInt32.Create(Self, 'uDimension', 0);
  FFduDimension.Exposed := false;
  Fields.Add(FFduDimension);

  FFduKnot := TMFFloat.Create(Self, 'uKnot', []);
  FFduKnot.Exposed := false;
  Fields.Add(FFduKnot);

  FFduOrder := TSFInt32.Create(Self, 'uOrder', 3);
  FFduOrder.Exposed := false;
  Fields.Add(FFduOrder);

  FFdvDimension := TSFInt32.Create(Self, 'vDimension', 0);
  FFdvDimension.Exposed := false;
  Fields.Add(FFdvDimension);

  FFdvKnot := TMFFloat.Create(Self, 'vKnot', []);
  FFdvKnot.Exposed := false;
  Fields.Add(FFdvKnot);

  FFdvOrder := TSFInt32.Create(Self, 'vOrder', 3);
  FFdvOrder.Exposed := false;
  Fields.Add(FFdvOrder);
end;

class function TNodeNurbsTextureSurface.ClassNodeTypeName: string;
begin
  Result := 'NurbsTextureSurface';
end;

class function TNodeNurbsTextureSurface.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeNurbsTextureSurface.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

constructor TNodeTrimmedSurface.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FEventaddTrimmingContour := TVRMLEvent.Create(Self, 'addTrimmingContour', TMFNode, true);
  Events.Add(FEventaddTrimmingContour);

  FEventremoveTrimmingContour := TVRMLEvent.Create(Self, 'removeTrimmingContour', TMFNode, true);
  Events.Add(FEventremoveTrimmingContour);

  FFdtrimmingContour := TMFNode.Create(Self, 'trimmingContour', [TNodeContour2D]);
  Fields.Add(FFdtrimmingContour);

  FFdsurface := TSFNode.Create(Self, 'surface', [TNodeNurbsSurface]);
  Fields.Add(FFdsurface);
end;

class function TNodeTrimmedSurface.ClassNodeTypeName: string;
begin
  Result := 'TrimmedSurface';
end;

class function TNodeTrimmedSurface.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeTrimmedSurface.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

{$define TGeometryNotImplemented := TNodeTrimmedSurface}
GeometryNotImplemented

procedure RegisterVRML97Nodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeCoordinateDeformer,
    TNodeInlineLoadControl,
    TNodeNurbsCurve_2,
    TNodeNurbsGroup,
    TNodeNurbsPositionInterpolator_2,
    TNodeNurbsSurface,
    TNodeNurbsTextureSurface,
    TNodeTrimmedSurface
  ]);
end;
{$endif read_implementation}
