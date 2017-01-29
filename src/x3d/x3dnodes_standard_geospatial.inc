{
  Copyright 2008-2017 Michalis Kamburelis.

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
  { }
  TGeoCoordinateNode = class(TAbstractCoordinateNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdPoint: TMFVec3d;
    public property FdPoint: TMFVec3d read FFdPoint;

    private FFdGeoOrigin: TSFNode;
    public property FdGeoOrigin: TSFNode read FFdGeoOrigin;

    private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;

    function CoordCount: Cardinal; override;

    {$I auto_generated_node_helpers/x3dnodes_geocoordinate.inc}
  end;

  { Elevation grid expressed in geo-centric coordinates.
    @bold(Rendering of this node is not implemented yet.) }
  TGeoElevationGridNode = class(TAbstractX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event in } { }
    private FEventSet_height: TMFDoubleEvent;
    public property EventSet_height: TMFDoubleEvent read FEventSet_height;

    private FFdColor: TSFNode;
    public property FdColor: TSFNode read FFdColor;

    private FFdNormal: TSFNode;
    public property FdNormal: TSFNode read FFdNormal;

    private FFdTexCoord: TSFNode;
    public property FdTexCoord: TSFNode read FFdTexCoord;

    private FFdYScale: TSFFloat;
    public property FdYScale: TSFFloat read FFdYScale;

    private FFdCcw: TSFBool;
    public property FdCcw: TSFBool read FFdCcw;

    private FFdColorPerVertex: TSFBool;
    public property FdColorPerVertex: TSFBool read FFdColorPerVertex;

    private FFdCreaseAngle: TSFDouble;
    public property FdCreaseAngle: TSFDouble read FFdCreaseAngle;

    private FFdGeoGridOrigin: TSFVec3d;
    public property FdGeoGridOrigin: TSFVec3d read FFdGeoGridOrigin;

    private FFdGeoOrigin: TSFNode;
    public property FdGeoOrigin: TSFNode read FFdGeoOrigin;

    private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;

    private FFdHeight: TMFDouble;
    public property FdHeight: TMFDouble read FFdHeight;

    private FFdNormalPerVertex: TSFBool;
    public property FdNormalPerVertex: TSFBool read FFdNormalPerVertex;

    private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    private FFdXDimension: TSFInt32;
    public property FdXDimension: TSFInt32 read FFdXDimension;

    private FFdXSpacing: TSFDouble;
    public property FdXSpacing: TSFDouble read FFdXSpacing;

    private FFdZDimension: TSFInt32;
    public property FdZDimension: TSFInt32 read FFdZDimension;

    private FFdZSpacing: TSFDouble;
    public property FdZSpacing: TSFDouble read FFdZSpacing;

    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    {$I auto_generated_node_helpers/x3dnodes_geoelevationgrid.inc}
  end;

  TGeoLocationNode = class(TAbstractX3DGroupingNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdGeoCoords: TSFVec3d;
    public property FdGeoCoords: TSFVec3d read FFdGeoCoords;

    private FFdGeoOrigin: TSFNode;
    public property FdGeoOrigin: TSFNode read FFdGeoOrigin;

    private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;

    {$I auto_generated_node_helpers/x3dnodes_geolocation.inc}
  end;

  TGeoLODNode = class(TAbstractGroupingNode, IAbstractBoundedObject)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event out } { }
    private FEventChildren: TMFNodeEvent;
    public property EventChildren: TMFNodeEvent read FEventChildren;

    { Event out } { }
    private FEventLevel_changed: TSFInt32Event;
    public property EventLevel_changed: TSFInt32Event read FEventLevel_changed;

    private FFdCenter: TSFVec3d;
    public property FdCenter: TSFVec3d read FFdCenter;

    private FFdChild1Url: TMFString;
    public property FdChild1Url: TMFString read FFdChild1Url;

    private FFdChild2Url: TMFString;
    public property FdChild2Url: TMFString read FFdChild2Url;

    private FFdChild3Url: TMFString;
    public property FdChild3Url: TMFString read FFdChild3Url;

    private FFdChild4Url: TMFString;
    public property FdChild4Url: TMFString read FFdChild4Url;

    private FFdGeoOrigin: TSFNode;
    public property FdGeoOrigin: TSFNode read FFdGeoOrigin;

    private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;

    private FFdRange: TSFFloat;
    public property FdRange: TSFFloat read FFdRange;

    private FFdRootUrl: TMFString;
    public property FdRootUrl: TMFString read FFdRootUrl;

    private FFdRootNode: TMFNode;
    public property FdRootNode: TMFNode read FFdRootNode;

    private FFdBboxCenter: TSFVec3f;
    public property FdBboxCenter: TSFVec3f read FFdBboxCenter;

    private FFdBboxSize: TSFVec3f;
    public property FdBboxSize: TSFVec3f read FFdBboxSize;

    {$I auto_generated_node_helpers/x3dnodes_geolod.inc}
  end;

  TGeoMetadataNode = class(TAbstractInfoNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdData: TMFNode;
    public property FdData: TMFNode read FFdData;

    private FFdSummary: TMFString;
    public property FdSummary: TMFString read FFdSummary;

    private FFdUrl: TMFString;
    public property FdUrl: TMFString read FFdUrl;

    {$I auto_generated_node_helpers/x3dnodes_geometadata.inc}
  end;

  TGeoOriginNode = class(TAbstractNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdGeoCoords: TSFVec3d;
    public property FdGeoCoords: TSFVec3d read FFdGeoCoords;

    private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;

    private FFdRotateYUp: TSFBool;
    public property FdRotateYUp: TSFBool read FFdRotateYUp;

    {$I auto_generated_node_helpers/x3dnodes_geoorigin.inc}
  end;

  TGeoPositionInterpolatorNode = class(TAbstractInterpolatorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFVec3d;
    public property FdKeyValue: TMFVec3d read FFdKeyValue;

    { Event out } { }
    private FEventGeovalue_changed: TSFVec3dEvent;
    public property EventGeovalue_changed: TSFVec3dEvent read FEventGeovalue_changed;

    { Event out } { }
    private FEventValue_changed: TSFVec3fEvent;
    public property EventValue_changed: TSFVec3fEvent read FEventValue_changed;

    private FFdGeoOrigin: TSFNode;
    public property FdGeoOrigin: TSFNode read FFdGeoOrigin;

    private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;

    {$I auto_generated_node_helpers/x3dnodes_geopositioninterpolator.inc}
  end;

  TGeoProximitySensorNode = class(TAbstractEnvironmentalSensorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdGeoCenter: TSFVec3d;
    public property FdGeoCenter: TSFVec3d read FFdGeoCenter;

    { Event out } { }
    private FEventCenterOfRotation_changed: TSFVec3fEvent;
    public property EventCenterOfRotation_changed: TSFVec3fEvent read FEventCenterOfRotation_changed;

    { Event out } { }
    private FEventGeoCoord_changed: TSFVec3dEvent;
    public property EventGeoCoord_changed: TSFVec3dEvent read FEventGeoCoord_changed;

    { Event out } { }
    private FEventOrientation_changed: TSFRotationEvent;
    public property EventOrientation_changed: TSFRotationEvent read FEventOrientation_changed;

    { Event out } { }
    private FEventPosition_changed: TSFVec3fEvent;
    public property EventPosition_changed: TSFVec3fEvent read FEventPosition_changed;

    private FFdGeoOrigin: TSFNode;
    public property FdGeoOrigin: TSFNode read FFdGeoOrigin;

    private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;

    {$I auto_generated_node_helpers/x3dnodes_geoproximitysensor.inc}
  end;

  TGeoTouchSensorNode = class(TAbstractTouchSensorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event out } { }
    private FEventHitNormal_changed: TSFVec3fEvent;
    public property EventHitNormal_changed: TSFVec3fEvent read FEventHitNormal_changed;

    { Event out } { }
    private FEventHitPoint_changed: TSFVec3fEvent;
    public property EventHitPoint_changed: TSFVec3fEvent read FEventHitPoint_changed;

    { Event out } { }
    private FEventHitTexCoord_changed: TSFVec2fEvent;
    public property EventHitTexCoord_changed: TSFVec2fEvent read FEventHitTexCoord_changed;

    { Event out } { }
    private FEventHitGeoCoord_changed: TSFVec3dEvent;
    public property EventHitGeoCoord_changed: TSFVec3dEvent read FEventHitGeoCoord_changed;

    private FFdGeoOrigin: TSFNode;
    public property FdGeoOrigin: TSFNode read FFdGeoOrigin;

    private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;

    {$I auto_generated_node_helpers/x3dnodes_geotouchsensor.inc}
  end;

  TGeoTransformNode = class(TAbstractX3DGroupingNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdGeoCenter: TSFVec3d;
    public property FdGeoCenter: TSFVec3d read FFdGeoCenter;

    private FFdRotation: TSFRotation;
    public property FdRotation: TSFRotation read FFdRotation;

    private FFdScale: TSFVec3f;
    public property FdScale: TSFVec3f read FFdScale;

    private FFdScaleOrientation: TSFRotation;
    public property FdScaleOrientation: TSFRotation read FFdScaleOrientation;

    private FFdTranslation: TSFVec3f;
    public property FdTranslation: TSFVec3f read FFdTranslation;

    private FFdGeoOrigin: TSFNode;
    public property FdGeoOrigin: TSFNode read FFdGeoOrigin;

    private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;

    {$I auto_generated_node_helpers/x3dnodes_geotransform.inc}
  end;

  TGeoViewpointNode = class(TAbstractX3DViewpointNode)
  strict private
    DummyPositionSingle: TSFVec3f;
  strict protected
    function PositionField: TSFVec3f; override;
  public
    procedure CreateNode; override;
    destructor Destroy; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event in } { }
    private FEventSet_orientation: TSFRotationEvent;
    public property EventSet_orientation: TSFRotationEvent read FEventSet_orientation;

    { Event in } { }
    private FEventSet_position: TSFVec3dEvent;
    public property EventSet_position: TSFVec3dEvent read FEventSet_position;

    private FFdFieldOfView: TSFFloat;
    public property FdFieldOfView: TSFFloat read FFdFieldOfView;

    private FFdHeadlight: TSFBool;
    public property FdHeadlight: TSFBool read FFdHeadlight;

    private FFdNavType: TMFString;
    public property FdNavType: TMFString read FFdNavType;

    private FFdGeoOrigin: TSFNode;
    public property FdGeoOrigin: TSFNode read FFdGeoOrigin;

    private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;

    private FFdPosition: TSFVec3d;
    public property FdPosition: TSFVec3d read FFdPosition;

    private FFdSpeedFactor: TSFFloat;
    public property FdSpeedFactor: TSFFloat read FFdSpeedFactor;

    {$I auto_generated_node_helpers/x3dnodes_geoviewpoint.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TGeoCoordinateNode.CreateNode;
begin
  inherited;

  FFdPoint := TMFVec3d.Create(Self, 'point', []);
  AddField(FFdPoint);
  { X3D specification comment: (-Inf,Inf) }

  FFdGeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TGeoOriginNode]);
  FFdGeoOrigin.Exposed := false;
  AddField(FFdGeoOrigin);

  FFdGeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
  FFdGeoSystem.Exposed := false;
  AddField(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }
end;

class function TGeoCoordinateNode.ClassX3DType: string;
begin
  Result := 'GeoCoordinate';
end;

class function TGeoCoordinateNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
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

  FFdColor := TSFNode.Create(Self, 'color', [TAbstractColorNode]);
   FdColor.ChangesAlways := [chGeometry];
  AddField(FFdColor);

  FFdNormal := TSFNode.Create(Self, 'normal', [TAbstractNormalNode]);
   FdNormal.ChangesAlways := [chGeometry];
  AddField(FFdNormal);

  FFdTexCoord := TSFNode.Create(Self, 'texCoord', [TAbstractTextureCoordinateNode]);
   FdTexCoord.ChangesAlways := [chGeometry];
  AddField(FFdTexCoord);

  FFdYScale := TSFFloat.Create(Self, 'yScale', 1.0);
   FdYScale.ChangesAlways := [chGeometry];
  AddField(FFdYScale);
  { X3D specification comment: [0,Inf) }

  FFdCcw := TSFBool.Create(Self, 'ccw', true);
   FdCcw.Exposed := false;
   FdCcw.ChangesAlways := [chGeometry];
  AddField(FFdCcw);

  FFdColorPerVertex := TSFBool.Create(Self, 'colorPerVertex', true);
   FdColorPerVertex.Exposed := false;
   FdColorPerVertex.ChangesAlways := [chGeometry];
  AddField(FFdColorPerVertex);

  FFdCreaseAngle := TSFDouble.Create(Self, 'creaseAngle', 0);
   FdCreaseAngle.Angle := true;
   FdCreaseAngle.Exposed := false;
   FdCreaseAngle.ChangesAlways := [chGeometry];
  AddField(FFdCreaseAngle);
  { X3D specification comment: [0,Inf) }

  FFdGeoGridOrigin := TSFVec3d.Create(Self, 'geoGridOrigin', Vector3Double(0, 0, 0));
   FdGeoGridOrigin.Exposed := false;
   FdGeoGridOrigin.ChangesAlways := [chGeometry];
  AddField(FFdGeoGridOrigin);
  { X3D specification comment: (-Inf,Inf) }

  FFdGeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TGeoOriginNode]);
   FdGeoOrigin.Exposed := false;
   FdGeoOrigin.ChangesAlways := [chGeometry];
  AddField(FFdGeoOrigin);

  FFdGeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
   FdGeoSystem.Exposed := false;
   FdGeoSystem.ChangesAlways := [chGeometry];
  AddField(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }

  FFdHeight := TMFDouble.Create(Self, 'height', [0, 0]);
   FdHeight.Exposed := false;
   FdHeight.ChangesAlways := [chGeometry];
  AddField(FFdHeight);
  { X3D specification comment: (-Inf,Inf) }

  FFdNormalPerVertex := TSFBool.Create(Self, 'normalPerVertex', true);
   FdNormalPerVertex.Exposed := false;
   FdNormalPerVertex.ChangesAlways := [chGeometry];
  AddField(FFdNormalPerVertex);

  FFdSolid := TSFBool.Create(Self, 'solid', true);
   FdSolid.Exposed := false;
   FdSolid.ChangesAlways := [chGeometry];
  AddField(FFdSolid);

  FFdXDimension := TSFInt32.Create(Self, 'xDimension', 0);
   FdXDimension.Exposed := false;
   FdXDimension.ChangesAlways := [chGeometry];
  AddField(FFdXDimension);
  { X3D specification comment: (0,Inf) }

  FFdXSpacing := TSFDouble.Create(Self, 'xSpacing', 1.0);
   FdXSpacing.Exposed := false;
   FdXSpacing.ChangesAlways := [chGeometry];
  AddField(FFdXSpacing);
  { X3D specification comment: [0,Inf) }

  FFdZDimension := TSFInt32.Create(Self, 'zDimension', 0);
   FdZDimension.Exposed := false;
   FdZDimension.ChangesAlways := [chGeometry];
  AddField(FFdZDimension);
  { X3D specification comment: (0,Inf) }

  FFdZSpacing := TSFDouble.Create(Self, 'zSpacing', 1.0);
   FdZSpacing.Exposed := false;
   FdZSpacing.ChangesAlways := [chGeometry];
  AddField(FFdZSpacing);
  { X3D specification comment: [0,Inf) }
end;

class function TGeoElevationGridNode.ClassX3DType: string;
begin
  Result := 'GeoElevationGrid';
end;

class function TGeoElevationGridNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    { There is very similar, but not 100% compatible node in VRML 97 }
    { (URN = URNVRML97Nodes + ClassX3DType) or }
    (URN = URNX3DNodes + ClassX3DType);
end;

{$define TGeometryNotImplemented := TGeoElevationGridNode}
GeometryNotImplemented

procedure TGeoLocationNode.CreateNode;
begin
  inherited;

  FFdGeoCoords := TSFVec3d.Create(Self, 'geoCoords', Vector3Double(0, 0, 0));
  AddField(FFdGeoCoords);
  { X3D specification comment: (-Inf,Inf) }

  FFdGeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TGeoOriginNode]);
  FFdGeoOrigin.Exposed := false;
  AddField(FFdGeoOrigin);

  FFdGeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
  FFdGeoSystem.Exposed := false;
  AddField(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }
end;

class function TGeoLocationNode.ClassX3DType: string;
begin
  Result := 'GeoLocation';
end;

class function TGeoLocationNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TGeoLODNode.CreateNode;
begin
  inherited;

  FEventChildren := TMFNodeEvent.Create(Self, 'children', false);
  AddEvent(FEventChildren);

  FEventLevel_changed := TSFInt32Event.Create(Self, 'level_changed', false);
  AddEvent(FEventLevel_changed);

  FFdCenter := TSFVec3d.Create(Self, 'center', Vector3Double(0, 0, 0));
  FFdCenter.Exposed := false;
  AddField(FFdCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdChild1Url := TMFString.Create(Self, 'child1Url', []);
  FFdChild1Url.Exposed := false;
  AddField(FFdChild1Url);
  { X3D specification comment: [URI] }

  FFdChild2Url := TMFString.Create(Self, 'child2Url', []);
  FFdChild2Url.Exposed := false;
  AddField(FFdChild2Url);
  { X3D specification comment: [URI] }

  FFdChild3Url := TMFString.Create(Self, 'child3Url', []);
  FFdChild3Url.Exposed := false;
  AddField(FFdChild3Url);
  { X3D specification comment: [URI] }

  FFdChild4Url := TMFString.Create(Self, 'child4Url', []);
  FFdChild4Url.Exposed := false;
  AddField(FFdChild4Url);
  { X3D specification comment: [URI] }

  FFdGeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TGeoOriginNode]);
  FFdGeoOrigin.Exposed := false;
  AddField(FFdGeoOrigin);

  FFdGeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
  FFdGeoSystem.Exposed := false;
  AddField(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }

  FFdRange := TSFFloat.Create(Self, 'range', 10);
  FFdRange.Exposed := false;
  AddField(FFdRange);
  { X3D specification comment: [0,Inf) }

  FFdRootUrl := TMFString.Create(Self, 'rootUrl', []);
  FFdRootUrl.Exposed := false;
  AddField(FFdRootUrl);
  { X3D specification comment: [URI] }

  FFdRootNode := TMFNode.Create(Self, 'rootNode', [TAbstractChildNode]);
  FFdRootNode.Exposed := false;
  AddField(FFdRootNode);

  FFdBboxCenter := TSFVec3f.Create(Self, 'bboxCenter', Vector3Single(0, 0, 0));
  FFdBboxCenter.Exposed := false;
  AddField(FFdBboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdBboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
  FFdBboxSize.Exposed := false;
  AddField(FFdBboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }
end;

class function TGeoLODNode.ClassX3DType: string;
begin
  Result := 'GeoLOD';
end;

class function TGeoLODNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TGeoMetadataNode.CreateNode;
begin
  inherited;

  FFdData := TMFNode.Create(Self, 'data', []);
  AddField(FFdData);

  FFdSummary := TMFString.Create(Self, 'summary', []);
  AddField(FFdSummary);

  FFdUrl := TMFString.Create(Self, 'url', []);
  AddField(FFdUrl);
  { X3D specification comment: [URI] }
end;

class function TGeoMetadataNode.ClassX3DType: string;
begin
  Result := 'GeoMetadata';
end;

class function TGeoMetadataNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TGeoOriginNode.CreateNode;
begin
  inherited;

  FFdGeoCoords := TSFVec3d.Create(Self, 'geoCoords', Vector3Double(0, 0, 0));
  AddField(FFdGeoCoords);
  { X3D specification comment: (-Inf,Inf) }

  FFdGeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
  FFdGeoSystem.Exposed := false;
  AddField(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }

  FFdRotateYUp := TSFBool.Create(Self, 'rotateYUp', false);
  FFdRotateYUp.Exposed := false;
  AddField(FFdRotateYUp);

  DefaultContainerField := 'geoOrigin';
end;

class function TGeoOriginNode.ClassX3DType: string;
begin
  Result := 'GeoOrigin';
end;

class function TGeoOriginNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TGeoPositionInterpolatorNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFVec3d.Create(Self, 'keyValue', []);
  AddField(FFdKeyValue);

  FEventGeovalue_changed := TSFVec3dEvent.Create(Self, 'geovalue_changed', false);
  AddEvent(FEventGeovalue_changed);

  FEventValue_changed := TSFVec3fEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  FFdGeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TGeoOriginNode]);
  FFdGeoOrigin.Exposed := false;
  AddField(FFdGeoOrigin);

  FFdGeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
  FFdGeoSystem.Exposed := false;
  AddField(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }
end;

class function TGeoPositionInterpolatorNode.ClassX3DType: string;
begin
  Result := 'GeoPositionInterpolator';
end;

class function TGeoPositionInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TGeoProximitySensorNode.CreateNode;
begin
  inherited;

  FFdGeoCenter := TSFVec3d.Create(Self, 'geoCenter', Vector3Double(0, 0, 0));
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

  FFdGeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TGeoOriginNode]);
  FFdGeoOrigin.Exposed := false;
  AddField(FFdGeoOrigin);

  FFdGeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
  FFdGeoSystem.Exposed := false;
  AddField(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }
end;

class function TGeoProximitySensorNode.ClassX3DType: string;
begin
  Result := 'GeoProximitySensor';
end;

class function TGeoProximitySensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
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

  FFdGeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TGeoOriginNode]);
  FFdGeoOrigin.Exposed := false;
  AddField(FFdGeoOrigin);

  FFdGeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
  FFdGeoSystem.Exposed := false;
  AddField(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }
end;

class function TGeoTouchSensorNode.ClassX3DType: string;
begin
  Result := 'GeoTouchSensor';
end;

class function TGeoTouchSensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TGeoTransformNode.CreateNode;
begin
  inherited;

  FFdGeoCenter := TSFVec3d.Create(Self, 'geoCenter', Vector3Double(0, 0, 0));
  AddField(FFdGeoCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdRotation := TSFRotation.Create(Self, 'rotation', Vector3Single(0, 0, 1), 0);
  AddField(FFdRotation);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdScale := TSFVec3f.Create(Self, 'scale', Vector3Single(1, 1, 1));
  AddField(FFdScale);
  { X3D specification comment: (0,Inf) }

  FFdScaleOrientation := TSFRotation.Create(Self, 'scaleOrientation', Vector3Single(0, 0, 1), 0);
  AddField(FFdScaleOrientation);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdTranslation := TSFVec3f.Create(Self, 'translation', Vector3Single(0, 0, 0));
  AddField(FFdTranslation);
  { X3D specification comment: (-Inf,Inf) }

  FFdGeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TGeoOriginNode]);
  FFdGeoOrigin.Exposed := false;
  AddField(FFdGeoOrigin);

  FFdGeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
  FFdGeoSystem.Exposed := false;
  AddField(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }
end;

class function TGeoTransformNode.ClassX3DType: string;
begin
  Result := 'GeoTransform';
end;

class function TGeoTransformNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TGeoViewpointNode.CreateNode;
begin
  inherited;

  DummyPositionSingle := TSFVec3f.Create(Self, 'position', Vector3Single(0, 0, 100000));

  FEventSet_orientation := TSFRotationEvent.Create(Self, 'set_orientation', true);
  AddEvent(FEventSet_orientation);

  FEventSet_position := TSFVec3dEvent.Create(Self, 'set_position', true);
  AddEvent(FEventSet_position);

  FFdFieldOfView := TSFFloat.Create(Self, 'fieldOfView', Pi/4);
  AddField(FFdFieldOfView);
  { X3D specification comment: (0,Pi) }

  FFdHeadlight := TSFBool.Create(Self, 'headlight', true);
  AddField(FFdHeadlight);

  FFdNavType := TMFString.Create(Self, 'navType', ['EXAMINE','ANY']);
  AddField(FFdNavType);

  FFdGeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TGeoOriginNode]);
  FFdGeoOrigin.Exposed := false;
  AddField(FFdGeoOrigin);

  FFdGeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
  FFdGeoSystem.Exposed := false;
  AddField(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }

  FFdPosition := TSFVec3d.Create(Self, 'position', Vector3Double(0, 0, 100000));
  FFdPosition.Exposed := false;
  AddField(FFdPosition);
  { X3D specification comment: (-Inf,Inf) }

  FFdSpeedFactor := TSFFloat.Create(Self, 'speedFactor', 1.0);
  FFdSpeedFactor.Exposed := false;
  AddField(FFdSpeedFactor);
  { X3D specification comment: [0,Inf) }
end;

destructor TGeoViewpointNode.Destroy;
begin
  FreeAndNil(DummyPositionSingle);
  inherited;
end;

class function TGeoViewpointNode.ClassX3DType: string;
begin
  Result := 'GeoViewpoint';
end;

class function TGeoViewpointNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
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
