{
  Copyright 2008-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ GEO X3D component.

  Note that this is slightly incompatible with the VRML 97 definition of
  the same nodes. And this incompatibility is not fixed here.
  When (if ever) I decide to implement Geo, it will be X3D-compatible only.
  Since I didn't ever implement VRML 97 GEO component, and I do not have current
  plans to implement X3D GEO component, and GEO is optional anyway in VRML 97,
  I decided GEO is just not important enough for me to try to
  parse VRML 97 GEO nodes.

  Incompatible differences in VRML 97 are mainly caused by the fact
  that double-precision field types were not available in VRML 97.
  And GEO really needed them. So many fields had to be defined as SFString
  type, while in X3D that are SFDouble, SFVec2d, SFVec3d etc.
  Examples:
  - GeoElevationGrid: xSpacing, zSpacing, geoGridOrigin
  - GeoOrigin: geoCoords
}

{$ifdef read_interface}
  TGeoOriginNode = class;

  { }
  TGeoCoordinateNode = class(TAbstractCoordinateNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: String; override;

    strict private FFdPoint: TMFVec3d;
    public property FdPoint: TMFVec3d read FFdPoint;

    strict private FFdGeoOrigin: TSFNode;
    public property FdGeoOrigin: TSFNode read FFdGeoOrigin;

    strict private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;

    function CoordCount: Cardinal; override;

    {$I auto_generated_node_helpers/x3dnodes_geocoordinate.inc}
  end;

  { Elevation grid expressed in geo-centric coordinates.
    @bold(Rendering of this node is not implemented yet.) }
  TGeoElevationGridNode = class(TAbstractGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: String; override;

    { Event in } { }
    strict private FEventSet_height: TMFDoubleEvent;
    public property EventSet_height: TMFDoubleEvent read FEventSet_height;

    strict private FFdColor: TSFNode;
    public property FdColor: TSFNode read FFdColor;

    strict private FFdNormal: TSFNode;
    public property FdNormal: TSFNode read FFdNormal;

    strict private FFdTexCoord: TSFNode;
    public property FdTexCoord: TSFNode read FFdTexCoord;

    strict private FFdYScale: TSFFloat;
    public property FdYScale: TSFFloat read FFdYScale;

    strict private FFdCcw: TSFBool;
    public property FdCcw: TSFBool read FFdCcw;

    strict private FFdColorPerVertex: TSFBool;
    public property FdColorPerVertex: TSFBool read FFdColorPerVertex;

    strict private FFdCreaseAngle: TSFDouble;
    public property FdCreaseAngle: TSFDouble read FFdCreaseAngle;

    strict private FFdGeoGridOrigin: TSFVec3d;
    public property FdGeoGridOrigin: TSFVec3d read FFdGeoGridOrigin;

    strict private FFdGeoOrigin: TSFNode;
    public property FdGeoOrigin: TSFNode read FFdGeoOrigin;

    strict private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;

    strict private FFdHeight: TMFDouble;
    public property FdHeight: TMFDouble read FFdHeight;

    strict private FFdNormalPerVertex: TSFBool;
    public property FdNormalPerVertex: TSFBool read FFdNormalPerVertex;

    strict private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    strict private FFdXDimension: TSFInt32;
    public property FdXDimension: TSFInt32 read FFdXDimension;

    strict private FFdXSpacing: TSFDouble;
    public property FdXSpacing: TSFDouble read FFdXSpacing;

    strict private FFdZDimension: TSFInt32;
    public property FdZDimension: TSFInt32 read FFdZDimension;

    strict private FFdZSpacing: TSFDouble;
    public property FdZSpacing: TSFDouble read FFdZSpacing;

    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: Boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: Boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    {$I auto_generated_node_helpers/x3dnodes_geoelevationgrid.inc}
  end;

  TGeoLocationNode = class(TAbstractX3DGroupingNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: String; override;

    strict private FFdGeoCoords: TSFVec3d;
    public property FdGeoCoords: TSFVec3d read FFdGeoCoords;

    strict private FFdGeoOrigin: TSFNode;
    public property FdGeoOrigin: TSFNode read FFdGeoOrigin;

    strict private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;

    {$I auto_generated_node_helpers/x3dnodes_geolocation.inc}
  end;

  TGeoLODNode = class(TAbstractGroupingNode, IAbstractBoundedObject)
  public
    procedure CreateNode; override;
    class function ClassX3DType: String; override;

    { Event out } { }
    strict private FEventChildren: TMFNodeEvent;
    public property EventChildren: TMFNodeEvent read FEventChildren;

    { Event out } { }
    strict private FEventLevel_changed: TSFInt32Event;
    public property EventLevel_changed: TSFInt32Event read FEventLevel_changed;

    strict private FFdCenter: TSFVec3d;
    public property FdCenter: TSFVec3d read FFdCenter;

    strict private FFdChild1Url: TMFString;
    public property FdChild1Url: TMFString read FFdChild1Url;

    strict private FFdChild2Url: TMFString;
    public property FdChild2Url: TMFString read FFdChild2Url;

    strict private FFdChild3Url: TMFString;
    public property FdChild3Url: TMFString read FFdChild3Url;

    strict private FFdChild4Url: TMFString;
    public property FdChild4Url: TMFString read FFdChild4Url;

    strict private FFdGeoOrigin: TSFNode;
    public property FdGeoOrigin: TSFNode read FFdGeoOrigin;

    strict private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;

    strict private FFdRange: TSFFloat;
    public property FdRange: TSFFloat read FFdRange;

    strict private FFdRootUrl: TMFString;
    public property FdRootUrl: TMFString read FFdRootUrl;

    strict private FFdRootNode: TMFNode;
    public property FdRootNode: TMFNode read FFdRootNode;

    strict private FFdBboxCenter: TSFVec3f;
    public property FdBboxCenter: TSFVec3f read FFdBboxCenter;

    strict private FFdBboxSize: TSFVec3f;
    public property FdBboxSize: TSFVec3f read FFdBboxSize;

    {$I auto_generated_node_helpers/x3dnodes_geolod.inc}
  end;

  TGeoMetadataNode = class(TAbstractInfoNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: String; override;

    strict private FFdData: TMFNode;
    public property FdData: TMFNode read FFdData;

    strict private FFdSummary: TMFString;
    public property FdSummary: TMFString read FFdSummary;

    strict private FFdUrl: TMFString;
    public property FdUrl: TMFString read FFdUrl;

    {$I auto_generated_node_helpers/x3dnodes_geometadata.inc}
  end;

  TGeoOriginNode = class(TAbstractNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: String; override;

    strict private FFdGeoCoords: TSFVec3d;
    public property FdGeoCoords: TSFVec3d read FFdGeoCoords;

    strict private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;

    strict private FFdRotateYUp: TSFBool;
    public property FdRotateYUp: TSFBool read FFdRotateYUp;

    {$I auto_generated_node_helpers/x3dnodes_geoorigin.inc}
  end;

  TGeoPositionInterpolatorNode = class(TAbstractInterpolatorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: String; override;

    strict private FFdKeyValue: TMFVec3d;
    public property FdKeyValue: TMFVec3d read FFdKeyValue;

    { Event out } { }
    strict private FEventGeovalue_changed: TSFVec3dEvent;
    public property EventGeovalue_changed: TSFVec3dEvent read FEventGeovalue_changed;

    { Event out } { }
    strict private FEventValue_changed: TSFVec3fEvent;
    public property EventValue_changed: TSFVec3fEvent read FEventValue_changed;

    strict private FFdGeoOrigin: TSFNode;
    public property FdGeoOrigin: TSFNode read FFdGeoOrigin;

    strict private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;

    {$I auto_generated_node_helpers/x3dnodes_geopositioninterpolator.inc}
  end;

  TGeoProximitySensorNode = class(TAbstractEnvironmentalSensorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: String; override;

    strict private FFdGeoCenter: TSFVec3d;
    public property FdGeoCenter: TSFVec3d read FFdGeoCenter;

    { Event out } { }
    strict private FEventCenterOfRotation_changed: TSFVec3fEvent;
    public property EventCenterOfRotation_changed: TSFVec3fEvent read FEventCenterOfRotation_changed;

    { Event out } { }
    strict private FEventGeoCoord_changed: TSFVec3dEvent;
    public property EventGeoCoord_changed: TSFVec3dEvent read FEventGeoCoord_changed;

    { Event out } { }
    strict private FEventOrientation_changed: TSFRotationEvent;
    public property EventOrientation_changed: TSFRotationEvent read FEventOrientation_changed;

    { Event out } { }
    strict private FEventPosition_changed: TSFVec3fEvent;
    public property EventPosition_changed: TSFVec3fEvent read FEventPosition_changed;

    strict private FFdGeoOrigin: TSFNode;
    public property FdGeoOrigin: TSFNode read FFdGeoOrigin;

    strict private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;

    {$I auto_generated_node_helpers/x3dnodes_geoproximitysensor.inc}
  end;

  TGeoTouchSensorNode = class(TAbstractTouchSensorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: String; override;

    { Event out } { }
    strict private FEventHitNormal_changed: TSFVec3fEvent;
    public property EventHitNormal_changed: TSFVec3fEvent read FEventHitNormal_changed;

    { Event out } { }
    strict private FEventHitPoint_changed: TSFVec3fEvent;
    public property EventHitPoint_changed: TSFVec3fEvent read FEventHitPoint_changed;

    { Event out } { }
    strict private FEventHitTexCoord_changed: TSFVec2fEvent;
    public property EventHitTexCoord_changed: TSFVec2fEvent read FEventHitTexCoord_changed;

    { Event out } { }
    strict private FEventHitGeoCoord_changed: TSFVec3dEvent;
    public property EventHitGeoCoord_changed: TSFVec3dEvent read FEventHitGeoCoord_changed;

    strict private FFdGeoOrigin: TSFNode;
    public property FdGeoOrigin: TSFNode read FFdGeoOrigin;

    strict private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;

    {$I auto_generated_node_helpers/x3dnodes_geotouchsensor.inc}
  end;

  TGeoTransformNode = class(TAbstractX3DGroupingNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: String; override;

    strict private FFdGeoCenter: TSFVec3d;
    public property FdGeoCenter: TSFVec3d read FFdGeoCenter;

    strict private FFdRotation: TSFRotation;
    public property FdRotation: TSFRotation read FFdRotation;

    strict private FFdScale: TSFVec3f;
    public property FdScale: TSFVec3f read FFdScale;

    strict private FFdScaleOrientation: TSFRotation;
    public property FdScaleOrientation: TSFRotation read FFdScaleOrientation;

    strict private FFdTranslation: TSFVec3f;
    public property FdTranslation: TSFVec3f read FFdTranslation;

    strict private FFdGeoOrigin: TSFNode;
    public property FdGeoOrigin: TSFNode read FFdGeoOrigin;

    strict private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;

    {$I auto_generated_node_helpers/x3dnodes_geotransform.inc}
  end;

  TGeoViewpointNode = class(TAbstractViewpointNode)
  strict private
    DummyPositionSingle: TSFVec3f;
  strict protected
    function PositionField: TSFVec3f; override;
  public
    procedure CreateNode; override;
    destructor Destroy; override;
    class function ClassX3DType: String; override;

    { Event in } { }
    strict private FEventSet_orientation: TSFRotationEvent;
    public property EventSet_orientation: TSFRotationEvent read FEventSet_orientation;

    { Event in } { }
    strict private FEventSet_position: TSFVec3dEvent;
    public property EventSet_position: TSFVec3dEvent read FEventSet_position;

    strict private FFdFieldOfView: TSFFloat;
    public property FdFieldOfView: TSFFloat read FFdFieldOfView;

    strict private FFdHeadlight: TSFBool;
    public property FdHeadlight: TSFBool read FFdHeadlight;

    strict private FFdNavType: TMFString;
    public property FdNavType: TMFString read FFdNavType;

    strict private FFdGeoOrigin: TSFNode;
    public property FdGeoOrigin: TSFNode read FFdGeoOrigin;

    strict private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;

    strict private FFdPosition: TSFVec3d;
    public property FdPosition: TSFVec3d read FFdPosition;

    strict private FFdSpeedFactor: TSFFloat;
    public property FdSpeedFactor: TSFFloat read FFdSpeedFactor;

    {$I auto_generated_node_helpers/x3dnodes_geoviewpoint.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TGeoCoordinateNode.CreateNode;
begin
  inherited;

  FFdPoint := TMFVec3d.Create(Self, true, 'point', []);
  AddField(FFdPoint);
  { X3D specification comment: (-Inf,Inf) }

  FFdGeoOrigin := TSFNode.Create(Self, false, 'geoOrigin', [TGeoOriginNode]);
  AddField(FFdGeoOrigin);

  FFdGeoSystem := TMFString.Create(Self, false, 'geoSystem', ['GD','WE']);
  AddField(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }
end;

class function TGeoCoordinateNode.ClassX3DType: String;
begin
  Result := 'GeoCoordinate';
end;

function TGeoCoordinateNode.CoordCount: Cardinal;
begin
  Result := FdPoint.Items.Count;
end;

procedure TGeoElevationGridNode.CreateNode;
begin
  inherited;

  FEventSet_height := TMFDoubleEvent.Create(Self, 'set_height', true);
  AddEvent(FEventSet_height);

  FFdColor := TSFNode.Create(Self, true, 'color', [TAbstractColorNode]);
   FdColor.ChangeAlways := chGeometry;
  AddField(FFdColor);

  FFdNormal := TSFNode.Create(Self, true, 'normal', [TAbstractNormalNode]);
   FdNormal.ChangeAlways := chGeometry;
  AddField(FFdNormal);

  FFdTexCoord := TSFNode.Create(Self, true, 'texCoord', [TAbstractTextureCoordinateNode]);
   FdTexCoord.ChangeAlways := chGeometry;
  AddField(FFdTexCoord);

  FFdYScale := TSFFloat.Create(Self, true, 'yScale', 1.0);
   FdYScale.ChangeAlways := chGeometry;
  AddField(FFdYScale);
  { X3D specification comment: [0,Inf) }

  FFdCcw := TSFBool.Create(Self, false, 'ccw', true);
   FdCcw.ChangeAlways := chGeometry;
  AddField(FFdCcw);

  FFdColorPerVertex := TSFBool.Create(Self, false, 'colorPerVertex', true);
   FdColorPerVertex.ChangeAlways := chGeometry;
  AddField(FFdColorPerVertex);

  FFdCreaseAngle := TSFDouble.Create(Self, false, 'creaseAngle', 0);
   FdCreaseAngle.Angle := true;
   FdCreaseAngle.ChangeAlways := chGeometry;
  AddField(FFdCreaseAngle);
  { X3D specification comment: [0,Inf) }

  FFdGeoGridOrigin := TSFVec3d.Create(Self, false, 'geoGridOrigin', Vector3Double(0, 0, 0));
   FdGeoGridOrigin.ChangeAlways := chGeometry;
  AddField(FFdGeoGridOrigin);
  { X3D specification comment: (-Inf,Inf) }

  FFdGeoOrigin := TSFNode.Create(Self, false, 'geoOrigin', [TGeoOriginNode]);
   FdGeoOrigin.ChangeAlways := chGeometry;
  AddField(FFdGeoOrigin);

  FFdGeoSystem := TMFString.Create(Self, false, 'geoSystem', ['GD','WE']);
   FdGeoSystem.ChangeAlways := chGeometry;
  AddField(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }

  FFdHeight := TMFDouble.Create(Self, false, 'height', [0, 0]);
   FdHeight.ChangeAlways := chGeometry;
  AddField(FFdHeight);
  { X3D specification comment: (-Inf,Inf) }

  FFdNormalPerVertex := TSFBool.Create(Self, false, 'normalPerVertex', true);
   FdNormalPerVertex.ChangeAlways := chGeometry;
  AddField(FFdNormalPerVertex);

  FFdSolid := TSFBool.Create(Self, false, 'solid', true);
   FdSolid.ChangeAlways := chGeometry;
  AddField(FFdSolid);

  FFdXDimension := TSFInt32.Create(Self, false, 'xDimension', 0);
   FdXDimension.ChangeAlways := chGeometry;
  AddField(FFdXDimension);
  { X3D specification comment: (0,Inf) }

  FFdXSpacing := TSFDouble.Create(Self, false, 'xSpacing', 1.0);
   FdXSpacing.ChangeAlways := chGeometry;
  AddField(FFdXSpacing);
  { X3D specification comment: [0,Inf) }

  FFdZDimension := TSFInt32.Create(Self, false, 'zDimension', 0);
   FdZDimension.ChangeAlways := chGeometry;
  AddField(FFdZDimension);
  { X3D specification comment: (0,Inf) }

  FFdZSpacing := TSFDouble.Create(Self, false, 'zSpacing', 1.0);
   FdZSpacing.ChangeAlways := chGeometry;
  AddField(FFdZSpacing);
  { X3D specification comment: [0,Inf) }
end;

class function TGeoElevationGridNode.ClassX3DType: String;
begin
  Result := 'GeoElevationGrid';
end;

function TGeoElevationGridNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  { Rendering of TGeoElevationGridNode not implemented. }
  Result := TBox3D.Empty;
end;

function TGeoElevationGridNode.VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: Boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TGeoElevationGridNode not implemented. }
  Result := 0;
end;

function TGeoElevationGridNode.TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: Boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TGeoElevationGridNode not implemented. }
  Result := 0;
end;

procedure TGeoLocationNode.CreateNode;
begin
  inherited;

  FFdGeoCoords := TSFVec3d.Create(Self, true, 'geoCoords', Vector3Double(0, 0, 0));
  AddField(FFdGeoCoords);
  { X3D specification comment: (-Inf,Inf) }

  FFdGeoOrigin := TSFNode.Create(Self, false, 'geoOrigin', [TGeoOriginNode]);
  AddField(FFdGeoOrigin);

  FFdGeoSystem := TMFString.Create(Self, false, 'geoSystem', ['GD','WE']);
  AddField(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }
end;

class function TGeoLocationNode.ClassX3DType: String;
begin
  Result := 'GeoLocation';
end;

procedure TGeoLODNode.CreateNode;
begin
  inherited;

  FEventChildren := TMFNodeEvent.Create(Self, 'children', false);
  AddEvent(FEventChildren);

  FEventLevel_changed := TSFInt32Event.Create(Self, 'level_changed', false);
  AddEvent(FEventLevel_changed);

  FFdCenter := TSFVec3d.Create(Self, false, 'center', Vector3Double(0, 0, 0));
  AddField(FFdCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdChild1Url := TMFString.Create(Self, false, 'child1Url', []);
  AddField(FFdChild1Url);
  { X3D specification comment: [URI] }

  FFdChild2Url := TMFString.Create(Self, false, 'child2Url', []);
  AddField(FFdChild2Url);
  { X3D specification comment: [URI] }

  FFdChild3Url := TMFString.Create(Self, false, 'child3Url', []);
  AddField(FFdChild3Url);
  { X3D specification comment: [URI] }

  FFdChild4Url := TMFString.Create(Self, false, 'child4Url', []);
  AddField(FFdChild4Url);
  { X3D specification comment: [URI] }

  FFdGeoOrigin := TSFNode.Create(Self, false, 'geoOrigin', [TGeoOriginNode]);
  AddField(FFdGeoOrigin);

  FFdGeoSystem := TMFString.Create(Self, false, 'geoSystem', ['GD','WE']);
  AddField(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }

  FFdRange := TSFFloat.Create(Self, false, 'range', 10);
  AddField(FFdRange);
  { X3D specification comment: [0,Inf) }

  FFdRootUrl := TMFString.Create(Self, false, 'rootUrl', []);
  AddField(FFdRootUrl);
  { X3D specification comment: [URI] }

  FFdRootNode := TMFNode.Create(Self, false, 'rootNode', [TAbstractChildNode]);
  AddField(FFdRootNode);

  FFdBboxCenter := TSFVec3f.Create(Self, false, 'bboxCenter', Vector3(0, 0, 0));
  AddField(FFdBboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdBboxSize := TSFVec3f.Create(Self, false, 'bboxSize', Vector3(-1, -1, -1));
  AddField(FFdBboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }
end;

class function TGeoLODNode.ClassX3DType: String;
begin
  Result := 'GeoLOD';
end;

procedure TGeoMetadataNode.CreateNode;
begin
  inherited;

  FFdData := TMFNode.Create(Self, true, 'data', []);
  AddField(FFdData);

  FFdSummary := TMFString.Create(Self, true, 'summary', []);
  AddField(FFdSummary);

  FFdUrl := TMFString.Create(Self, true, 'url', []);
  AddField(FFdUrl);
  { X3D specification comment: [URI] }
end;

class function TGeoMetadataNode.ClassX3DType: String;
begin
  Result := 'GeoMetadata';
end;

procedure TGeoOriginNode.CreateNode;
begin
  inherited;

  FFdGeoCoords := TSFVec3d.Create(Self, true, 'geoCoords', Vector3Double(0, 0, 0));
  AddField(FFdGeoCoords);
  { X3D specification comment: (-Inf,Inf) }

  FFdGeoSystem := TMFString.Create(Self, false, 'geoSystem', ['GD','WE']);
  AddField(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }

  FFdRotateYUp := TSFBool.Create(Self, false, 'rotateYUp', false);
  AddField(FFdRotateYUp);

  DefaultContainerField := 'geoOrigin';
end;

class function TGeoOriginNode.ClassX3DType: String;
begin
  Result := 'GeoOrigin';
end;

procedure TGeoPositionInterpolatorNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFVec3d.Create(Self, true, 'keyValue', []);
  AddField(FFdKeyValue);

  FEventGeovalue_changed := TSFVec3dEvent.Create(Self, 'geovalue_changed', false);
  AddEvent(FEventGeovalue_changed);

  FEventValue_changed := TSFVec3fEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  FFdGeoOrigin := TSFNode.Create(Self, false, 'geoOrigin', [TGeoOriginNode]);
  AddField(FFdGeoOrigin);

  FFdGeoSystem := TMFString.Create(Self, false, 'geoSystem', ['GD','WE']);
  AddField(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }
end;

class function TGeoPositionInterpolatorNode.ClassX3DType: String;
begin
  Result := 'GeoPositionInterpolator';
end;

procedure TGeoProximitySensorNode.CreateNode;
begin
  inherited;

  FFdGeoCenter := TSFVec3d.Create(Self, true, 'geoCenter', Vector3Double(0, 0, 0));
  AddField(FFdGeoCenter);
  { X3D specification comment: (-Inf,Inf) }

  FEventCenterOfRotation_changed := TSFVec3fEvent.Create(Self, 'centerOfRotation_changed', false);
  AddEvent(FEventCenterOfRotation_changed);

  FEventGeoCoord_changed := TSFVec3dEvent.Create(Self, 'geoCoord_changed', false);
  AddEvent(FEventGeoCoord_changed);

  FEventOrientation_changed := TSFRotationEvent.Create(Self, 'orientation_changed', false);
  AddEvent(FEventOrientation_changed);

  FEventPosition_changed := TSFVec3fEvent.Create(Self, 'position_changed', false);
  AddEvent(FEventPosition_changed);

  FFdGeoOrigin := TSFNode.Create(Self, false, 'geoOrigin', [TGeoOriginNode]);
  AddField(FFdGeoOrigin);

  FFdGeoSystem := TMFString.Create(Self, false, 'geoSystem', ['GD','WE']);
  AddField(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }
end;

class function TGeoProximitySensorNode.ClassX3DType: String;
begin
  Result := 'GeoProximitySensor';
end;

procedure TGeoTouchSensorNode.CreateNode;
begin
  inherited;

  FEventHitNormal_changed := TSFVec3fEvent.Create(Self, 'hitNormal_changed', false);
  AddEvent(FEventHitNormal_changed);

  FEventHitPoint_changed := TSFVec3fEvent.Create(Self, 'hitPoint_changed', false);
  AddEvent(FEventHitPoint_changed);

  FEventHitTexCoord_changed := TSFVec2fEvent.Create(Self, 'hitTexCoord_changed', false);
  AddEvent(FEventHitTexCoord_changed);

  FEventHitGeoCoord_changed := TSFVec3dEvent.Create(Self, 'hitGeoCoord_changed', false);
  AddEvent(FEventHitGeoCoord_changed);

  FFdGeoOrigin := TSFNode.Create(Self, false, 'geoOrigin', [TGeoOriginNode]);
  AddField(FFdGeoOrigin);

  FFdGeoSystem := TMFString.Create(Self, false, 'geoSystem', ['GD','WE']);
  AddField(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }
end;

class function TGeoTouchSensorNode.ClassX3DType: String;
begin
  Result := 'GeoTouchSensor';
end;

procedure TGeoTransformNode.CreateNode;
begin
  inherited;

  FFdGeoCenter := TSFVec3d.Create(Self, true, 'geoCenter', Vector3Double(0, 0, 0));
  AddField(FFdGeoCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdRotation := TSFRotation.Create(Self, true, 'rotation', Vector3(0, 0, 1), 0);
  AddField(FFdRotation);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdScale := TSFVec3f.Create(Self, true, 'scale', Vector3(1, 1, 1));
  AddField(FFdScale);
  { X3D specification comment: (0,Inf) }

  FFdScaleOrientation := TSFRotation.Create(Self, true, 'scaleOrientation', Vector3(0, 0, 1), 0);
  AddField(FFdScaleOrientation);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdTranslation := TSFVec3f.Create(Self, true, 'translation', Vector3(0, 0, 0));
  AddField(FFdTranslation);
  { X3D specification comment: (-Inf,Inf) }

  FFdGeoOrigin := TSFNode.Create(Self, false, 'geoOrigin', [TGeoOriginNode]);
  AddField(FFdGeoOrigin);

  FFdGeoSystem := TMFString.Create(Self, false, 'geoSystem', ['GD','WE']);
  AddField(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }
end;

class function TGeoTransformNode.ClassX3DType: String;
begin
  Result := 'GeoTransform';
end;

procedure TGeoViewpointNode.CreateNode;
begin
  inherited;

  DummyPositionSingle := TSFVec3f.Create(Self, true, 'position', Vector3(0, 0, 100000));

  FEventSet_orientation := TSFRotationEvent.Create(Self, 'set_orientation', true);
  AddEvent(FEventSet_orientation);

  FEventSet_position := TSFVec3dEvent.Create(Self, 'set_position', true);
  AddEvent(FEventSet_position);

  FFdFieldOfView := TSFFloat.Create(Self, true, 'fieldOfView', Pi/4);
  AddField(FFdFieldOfView);
  { X3D specification comment: (0,Pi) }

  FFdHeadlight := TSFBool.Create(Self, true, 'headlight', true);
  AddField(FFdHeadlight);

  FFdNavType := TMFString.Create(Self, true, 'navType', ['EXAMINE','ANY']);
  AddField(FFdNavType);

  FFdGeoOrigin := TSFNode.Create(Self, false, 'geoOrigin', [TGeoOriginNode]);
  AddField(FFdGeoOrigin);

  FFdGeoSystem := TMFString.Create(Self, false, 'geoSystem', ['GD','WE']);
  AddField(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }

  FFdPosition := TSFVec3d.Create(Self, false, 'position', Vector3Double(0, 0, 100000));
  AddField(FFdPosition);
  { X3D specification comment: (-Inf,Inf) }

  FFdSpeedFactor := TSFFloat.Create(Self, false, 'speedFactor', 1.0);
  AddField(FFdSpeedFactor);
  { X3D specification comment: [0,Inf) }
end;

destructor TGeoViewpointNode.Destroy;
begin
  FreeAndNil(DummyPositionSingle);
  inherited;
end;

class function TGeoViewpointNode.ClassX3DType: String;
begin
  Result := 'GeoViewpoint';
end;

function TGeoViewpointNode.PositionField: TSFVec3f;
begin
  // TODO: dummy value, just to avoid PositionField crashing
  Result := DummyPositionSingle;
end;

procedure RegisterGeospatialNodes;
begin
  NodesManager.RegisterNodeClasses([
    TGeoCoordinateNode,
    TGeoElevationGridNode,
    TGeoLocationNode,
    TGeoLODNode,
    TGeoMetadataNode,
    TGeoOriginNode,
    TGeoPositionInterpolatorNode,
    TGeoProximitySensorNode,
    TGeoTouchSensorNode,
    TGeoTransformNode,
    TGeoViewpointNode
  ]);
end;

{$endif read_implementation}
