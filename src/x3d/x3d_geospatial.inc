{
  Copyright 2008-2011 Michalis Kamburelis.

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
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdPoint: TMFVec3d;
    public property FdPoint: TMFVec3d read FFdPoint;

    private FFdGeoOrigin: TSFNode;
    public property FdGeoOrigin: TSFNode read FFdGeoOrigin;

    private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;

    function CoordCount: Cardinal; override;
  end;

  TGeoElevationGridNode = class(TAbstractX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFDouble, in } { }
    private FEventSet_height: TX3DEvent;
    public property EventSet_height: TX3DEvent read FEventSet_height;

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

    { Geometry node not implemented } { }
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
  end;

  TGeoLocationNode = class(TAbstractX3DGroupingNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdGeoCoords: TSFVec3d;
    public property FdGeoCoords: TSFVec3d read FFdGeoCoords;

    private FFdGeoOrigin: TSFNode;
    public property FdGeoOrigin: TSFNode read FFdGeoOrigin;

    private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;
  end;

  TGeoLODNode = class(TAbstractGroupingNode, IAbstractBoundedObject)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFNode, out } { }
    private FEventChildren: TX3DEvent;
    public property EventChildren: TX3DEvent read FEventChildren;

    { Event: SFInt32, out } { }
    private FEventLevel_changed: TX3DEvent;
    public property EventLevel_changed: TX3DEvent read FEventLevel_changed;

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
  end;

  TGeoMetadataNode = class(TAbstractInfoNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdData: TMFNode;
    public property FdData: TMFNode read FFdData;

    private FFdSummary: TMFString;
    public property FdSummary: TMFString read FFdSummary;

    private FFdUrl: TMFString;
    public property FdUrl: TMFString read FFdUrl;
  end;

  TGeoOriginNode = class(TAbstractNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdGeoCoords: TSFVec3d;
    public property FdGeoCoords: TSFVec3d read FFdGeoCoords;

    private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;

    private FFdRotateYUp: TSFBool;
    public property FdRotateYUp: TSFBool read FFdRotateYUp;
  end;

  TGeoPositionInterpolatorNode = class(TAbstractInterpolatorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFVec3d;
    public property FdKeyValue: TMFVec3d read FFdKeyValue;

    { Event: SFVec3d, out } { }
    private FEventGeovalue_changed: TX3DEvent;
    public property EventGeovalue_changed: TX3DEvent read FEventGeovalue_changed;

    { Event: SFVec3f, out } { }
    private FEventValue_changed: TX3DEvent;
    public property EventValue_changed: TX3DEvent read FEventValue_changed;

    private FFdGeoOrigin: TSFNode;
    public property FdGeoOrigin: TSFNode read FFdGeoOrigin;

    private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;
  end;

  TGeoProximitySensorNode = class(TAbstractEnvironmentalSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdGeoCenter: TSFVec3d;
    public property FdGeoCenter: TSFVec3d read FFdGeoCenter;

    { Event: SFVec3f, out } { }
    private FEventCenterOfRotation_changed: TX3DEvent;
    public property EventCenterOfRotation_changed: TX3DEvent read FEventCenterOfRotation_changed;

    { Event: SFVec3d, out } { }
    private FEventGeoCoord_changed: TX3DEvent;
    public property EventGeoCoord_changed: TX3DEvent read FEventGeoCoord_changed;

    { Event: SFRotation, out } { }
    private FEventOrientation_changed: TX3DEvent;
    public property EventOrientation_changed: TX3DEvent read FEventOrientation_changed;

    { Event: SFVec3f, out } { }
    private FEventPosition_changed: TX3DEvent;
    public property EventPosition_changed: TX3DEvent read FEventPosition_changed;

    private FFdGeoOrigin: TSFNode;
    public property FdGeoOrigin: TSFNode read FFdGeoOrigin;

    private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;
  end;

  TGeoTouchSensorNode = class(TAbstractTouchSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFVec3f, out } { }
    private FEventHitNormal_changed: TX3DEvent;
    public property EventHitNormal_changed: TX3DEvent read FEventHitNormal_changed;

    { Event: SFVec3f, out } { }
    private FEventHitPoint_changed: TX3DEvent;
    public property EventHitPoint_changed: TX3DEvent read FEventHitPoint_changed;

    { Event: SFVec2f, out } { }
    private FEventHitTexCoord_changed: TX3DEvent;
    public property EventHitTexCoord_changed: TX3DEvent read FEventHitTexCoord_changed;

    { Event: SFVec3d, out } { }
    private FEventHitGeoCoord_changed: TX3DEvent;
    public property EventHitGeoCoord_changed: TX3DEvent read FEventHitGeoCoord_changed;

    private FFdGeoOrigin: TSFNode;
    public property FdGeoOrigin: TSFNode read FFdGeoOrigin;

    private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;
  end;

  TGeoTransformNode = class(TAbstractX3DGroupingNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
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
  end;

  TGeoViewpointNode = class(TAbstractX3DViewpointNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFRotation, in } { }
    private FEventSet_orientation: TX3DEvent;
    public property EventSet_orientation: TX3DEvent read FEventSet_orientation;

    { Event: SFVec3d, in } { }
    private FEventSet_position: TX3DEvent;
    public property EventSet_position: TX3DEvent read FEventSet_position;

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
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TGeoCoordinateNode.CreateNode;
begin
  inherited;

  FFdPoint := TMFVec3d.Create(Self, 'point', []);
  Fields.Add(FFdPoint);
  { X3D specification comment: (-Inf,Inf) }

  FFdGeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TGeoOriginNode]);
  FFdGeoOrigin.Exposed := false;
  Fields.Add(FFdGeoOrigin);

  FFdGeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
  FFdGeoSystem.Exposed := false;
  Fields.Add(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }
end;

class function TGeoCoordinateNode.ClassNodeTypeName: string;
begin
  Result := 'GeoCoordinate';
end;

class function TGeoCoordinateNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TGeoCoordinateNode.CoordCount: Cardinal;
begin
  Result := FdPoint.Items.Count;
end;

procedure TGeoElevationGridNode.CreateNode;
begin
  inherited;

  FEventSet_height := TX3DEvent.Create(Self, 'set_height', TMFDouble, true);
  Events.Add(FEventSet_height);

  FFdColor := TSFNode.Create(Self, 'color', [TAbstractColorNode]);
   FdColor.ChangesAlways := [chGeometry];
  Fields.Add(FFdColor);

  FFdNormal := TSFNode.Create(Self, 'normal', [TAbstractNormalNode]);
   FdNormal.ChangesAlways := [chGeometry];
  Fields.Add(FFdNormal);

  FFdTexCoord := TSFNode.Create(Self, 'texCoord', [TAbstractTextureCoordinateNode]);
   FdTexCoord.ChangesAlways := [chGeometry];
  Fields.Add(FFdTexCoord);

  FFdYScale := TSFFloat.Create(Self, 'yScale', 1.0);
   FdYScale.ChangesAlways := [chGeometry];
  Fields.Add(FFdYScale);
  { X3D specification comment: [0,Inf) }

  FFdCcw := TSFBool.Create(Self, 'ccw', true);
   FdCcw.Exposed := false;
   FdCcw.ChangesAlways := [chGeometry];
  Fields.Add(FFdCcw);

  FFdColorPerVertex := TSFBool.Create(Self, 'colorPerVertex', true);
   FdColorPerVertex.Exposed := false;
   FdColorPerVertex.ChangesAlways := [chGeometry];
  Fields.Add(FFdColorPerVertex);

  FFdCreaseAngle := TSFDouble.Create(Self, 'creaseAngle', 0);
   FdCreaseAngle.Exposed := false;
   FdCreaseAngle.ChangesAlways := [chGeometry];
  Fields.Add(FFdCreaseAngle);
  { X3D specification comment: [0,Inf) }

  FFdGeoGridOrigin := TSFVec3d.Create(Self, 'geoGridOrigin', Vector3Double(0, 0, 0));
   FdGeoGridOrigin.Exposed := false;
   FdGeoGridOrigin.ChangesAlways := [chGeometry];
  Fields.Add(FFdGeoGridOrigin);
  { X3D specification comment: (-Inf,Inf) }

  FFdGeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TGeoOriginNode]);
   FdGeoOrigin.Exposed := false;
   FdGeoOrigin.ChangesAlways := [chGeometry];
  Fields.Add(FFdGeoOrigin);

  FFdGeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
   FdGeoSystem.Exposed := false;
   FdGeoSystem.ChangesAlways := [chGeometry];
  Fields.Add(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }

  FFdHeight := TMFDouble.Create(Self, 'height', [0, 0]);
   FdHeight.Exposed := false;
   FdHeight.ChangesAlways := [chGeometry];
  Fields.Add(FFdHeight);
  { X3D specification comment: (-Inf,Inf) }

  FFdNormalPerVertex := TSFBool.Create(Self, 'normalPerVertex', true);
   FdNormalPerVertex.Exposed := false;
   FdNormalPerVertex.ChangesAlways := [chGeometry];
  Fields.Add(FFdNormalPerVertex);

  FFdSolid := TSFBool.Create(Self, 'solid', true);
   FdSolid.Exposed := false;
   FdSolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdSolid);

  FFdXDimension := TSFInt32.Create(Self, 'xDimension', 0);
   FdXDimension.Exposed := false;
   FdXDimension.ChangesAlways := [chGeometry];
  Fields.Add(FFdXDimension);
  { X3D specification comment: (0,Inf) }

  FFdXSpacing := TSFDouble.Create(Self, 'xSpacing', 1.0);
   FdXSpacing.Exposed := false;
   FdXSpacing.ChangesAlways := [chGeometry];
  Fields.Add(FFdXSpacing);
  { X3D specification comment: [0,Inf) }

  FFdZDimension := TSFInt32.Create(Self, 'zDimension', 0);
   FdZDimension.Exposed := false;
   FdZDimension.ChangesAlways := [chGeometry];
  Fields.Add(FFdZDimension);
  { X3D specification comment: (0,Inf) }

  FFdZSpacing := TSFDouble.Create(Self, 'zSpacing', 1.0);
   FdZSpacing.Exposed := false;
   FdZSpacing.ChangesAlways := [chGeometry];
  Fields.Add(FFdZSpacing);
  { X3D specification comment: [0,Inf) }
end;

class function TGeoElevationGridNode.ClassNodeTypeName: string;
begin
  Result := 'GeoElevationGrid';
end;

class function TGeoElevationGridNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    { There is very similar, but not 100% compatible node in VRML 97 }
    { (URN = URNVRML97Nodes + ClassNodeTypeName) or }
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

{$define TGeometryNotImplemented := TGeoElevationGridNode}
GeometryNotImplemented

procedure TGeoLocationNode.CreateNode;
begin
  inherited;

  FFdGeoCoords := TSFVec3d.Create(Self, 'geoCoords', Vector3Double(0, 0, 0));
  Fields.Add(FFdGeoCoords);
  { X3D specification comment: (-Inf,Inf) }

  FFdGeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TGeoOriginNode]);
  FFdGeoOrigin.Exposed := false;
  Fields.Add(FFdGeoOrigin);

  FFdGeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
  FFdGeoSystem.Exposed := false;
  Fields.Add(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }
end;

class function TGeoLocationNode.ClassNodeTypeName: string;
begin
  Result := 'GeoLocation';
end;

class function TGeoLocationNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TGeoLODNode.CreateNode;
begin
  inherited;

  FEventChildren := TX3DEvent.Create(Self, 'children', TMFNode, false);
  Events.Add(FEventChildren);

  FEventLevel_changed := TX3DEvent.Create(Self, 'level_changed', TSFInt32, false);
  Events.Add(FEventLevel_changed);

  FFdCenter := TSFVec3d.Create(Self, 'center', Vector3Double(0, 0, 0));
  FFdCenter.Exposed := false;
  Fields.Add(FFdCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdChild1Url := TMFString.Create(Self, 'child1Url', []);
  FFdChild1Url.Exposed := false;
  Fields.Add(FFdChild1Url);
  { X3D specification comment: [URI] }

  FFdChild2Url := TMFString.Create(Self, 'child2Url', []);
  FFdChild2Url.Exposed := false;
  Fields.Add(FFdChild2Url);
  { X3D specification comment: [URI] }

  FFdChild3Url := TMFString.Create(Self, 'child3Url', []);
  FFdChild3Url.Exposed := false;
  Fields.Add(FFdChild3Url);
  { X3D specification comment: [URI] }

  FFdChild4Url := TMFString.Create(Self, 'child4Url', []);
  FFdChild4Url.Exposed := false;
  Fields.Add(FFdChild4Url);
  { X3D specification comment: [URI] }

  FFdGeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TGeoOriginNode]);
  FFdGeoOrigin.Exposed := false;
  Fields.Add(FFdGeoOrigin);

  FFdGeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
  FFdGeoSystem.Exposed := false;
  Fields.Add(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }

  FFdRange := TSFFloat.Create(Self, 'range', 10);
  FFdRange.Exposed := false;
  Fields.Add(FFdRange);
  { X3D specification comment: [0,Inf) }

  FFdRootUrl := TMFString.Create(Self, 'rootUrl', []);
  FFdRootUrl.Exposed := false;
  Fields.Add(FFdRootUrl);
  { X3D specification comment: [URI] }

  FFdRootNode := TMFNode.Create(Self, 'rootNode', [TAbstractChildNode]);
  FFdRootNode.Exposed := false;
  Fields.Add(FFdRootNode);

  FFdBboxCenter := TSFVec3f.Create(Self, 'bboxCenter', Vector3Single(0, 0, 0));
  FFdBboxCenter.Exposed := false;
  Fields.Add(FFdBboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdBboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
  FFdBboxSize.Exposed := false;
  Fields.Add(FFdBboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }
end;

class function TGeoLODNode.ClassNodeTypeName: string;
begin
  Result := 'GeoLOD';
end;

class function TGeoLODNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TGeoMetadataNode.CreateNode;
begin
  inherited;

  FFdData := TMFNode.Create(Self, 'data', []);
  Fields.Add(FFdData);

  FFdSummary := TMFString.Create(Self, 'summary', []);
  Fields.Add(FFdSummary);

  FFdUrl := TMFString.Create(Self, 'url', []);
  Fields.Add(FFdUrl);
  { X3D specification comment: [URI] }
end;

class function TGeoMetadataNode.ClassNodeTypeName: string;
begin
  Result := 'GeoMetadata';
end;

class function TGeoMetadataNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TGeoOriginNode.CreateNode;
begin
  inherited;

  FFdGeoCoords := TSFVec3d.Create(Self, 'geoCoords', Vector3Double(0, 0, 0));
  Fields.Add(FFdGeoCoords);
  { X3D specification comment: (-Inf,Inf) }

  FFdGeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
  FFdGeoSystem.Exposed := false;
  Fields.Add(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }

  FFdRotateYUp := TSFBool.Create(Self, 'rotateYUp', false);
  FFdRotateYUp.Exposed := false;
  Fields.Add(FFdRotateYUp);

  DefaultContainerField := 'geoOrigin';
end;

class function TGeoOriginNode.ClassNodeTypeName: string;
begin
  Result := 'GeoOrigin';
end;

class function TGeoOriginNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TGeoPositionInterpolatorNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFVec3d.Create(Self, 'keyValue', []);
  Fields.Add(FFdKeyValue);

  FEventGeovalue_changed := TX3DEvent.Create(Self, 'geovalue_changed', TSFVec3d, false);
  Events.Add(FEventGeovalue_changed);

  FEventValue_changed := TX3DEvent.Create(Self, 'value_changed', TSFVec3f, false);
  Events.Add(FEventValue_changed);

  FFdGeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TGeoOriginNode]);
  FFdGeoOrigin.Exposed := false;
  Fields.Add(FFdGeoOrigin);

  FFdGeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
  FFdGeoSystem.Exposed := false;
  Fields.Add(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }
end;

class function TGeoPositionInterpolatorNode.ClassNodeTypeName: string;
begin
  Result := 'GeoPositionInterpolator';
end;

class function TGeoPositionInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TGeoProximitySensorNode.CreateNode;
begin
  inherited;

  FFdGeoCenter := TSFVec3d.Create(Self, 'geoCenter', Vector3Double(0, 0, 0));
  Fields.Add(FFdGeoCenter);
  { X3D specification comment: (-Inf,Inf) }

  FEventCenterOfRotation_changed := TX3DEvent.Create(Self, 'centerOfRotation_changed', TSFVec3f, false);
  Events.Add(FEventCenterOfRotation_changed);

  FEventGeoCoord_changed := TX3DEvent.Create(Self, 'geoCoord_changed', TSFVec3d, false);
  Events.Add(FEventGeoCoord_changed);

  FEventOrientation_changed := TX3DEvent.Create(Self, 'orientation_changed', TSFRotation, false);
  Events.Add(FEventOrientation_changed);

  FEventPosition_changed := TX3DEvent.Create(Self, 'position_changed', TSFVec3f, false);
  Events.Add(FEventPosition_changed);

  FFdGeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TGeoOriginNode]);
  FFdGeoOrigin.Exposed := false;
  Fields.Add(FFdGeoOrigin);

  FFdGeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
  FFdGeoSystem.Exposed := false;
  Fields.Add(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }
end;

class function TGeoProximitySensorNode.ClassNodeTypeName: string;
begin
  Result := 'GeoProximitySensor';
end;

class function TGeoProximitySensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TGeoTouchSensorNode.CreateNode;
begin
  inherited;

  FEventHitNormal_changed := TX3DEvent.Create(Self, 'hitNormal_changed', TSFVec3f, false);
  Events.Add(FEventHitNormal_changed);

  FEventHitPoint_changed := TX3DEvent.Create(Self, 'hitPoint_changed', TSFVec3f, false);
  Events.Add(FEventHitPoint_changed);

  FEventHitTexCoord_changed := TX3DEvent.Create(Self, 'hitTexCoord_changed', TSFVec2f, false);
  Events.Add(FEventHitTexCoord_changed);

  FEventHitGeoCoord_changed := TX3DEvent.Create(Self, 'hitGeoCoord_changed', TSFVec3d, false);
  Events.Add(FEventHitGeoCoord_changed);

  FFdGeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TGeoOriginNode]);
  FFdGeoOrigin.Exposed := false;
  Fields.Add(FFdGeoOrigin);

  FFdGeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
  FFdGeoSystem.Exposed := false;
  Fields.Add(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }
end;

class function TGeoTouchSensorNode.ClassNodeTypeName: string;
begin
  Result := 'GeoTouchSensor';
end;

class function TGeoTouchSensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TGeoTransformNode.CreateNode;
begin
  inherited;

  FFdGeoCenter := TSFVec3d.Create(Self, 'geoCenter', Vector3Double(0, 0, 0));
  Fields.Add(FFdGeoCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdRotation := TSFRotation.Create(Self, 'rotation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdRotation);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdScale := TSFVec3f.Create(Self, 'scale', Vector3Single(1, 1, 1));
  Fields.Add(FFdScale);
  { X3D specification comment: (0,Inf) }

  FFdScaleOrientation := TSFRotation.Create(Self, 'scaleOrientation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdScaleOrientation);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdTranslation := TSFVec3f.Create(Self, 'translation', Vector3Single(0, 0, 0));
  Fields.Add(FFdTranslation);
  { X3D specification comment: (-Inf,Inf) }

  FFdGeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TGeoOriginNode]);
  FFdGeoOrigin.Exposed := false;
  Fields.Add(FFdGeoOrigin);

  FFdGeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
  FFdGeoSystem.Exposed := false;
  Fields.Add(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }
end;

class function TGeoTransformNode.ClassNodeTypeName: string;
begin
  Result := 'GeoTransform';
end;

class function TGeoTransformNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TGeoViewpointNode.CreateNode;
begin
  inherited;

  FEventSet_orientation := TX3DEvent.Create(Self, 'set_orientation', TSFRotation, true);
  Events.Add(FEventSet_orientation);

  FEventSet_position := TX3DEvent.Create(Self, 'set_position', TSFVec3d, true);
  Events.Add(FEventSet_position);

  FFdFieldOfView := TSFFloat.Create(Self, 'fieldOfView', Pi/4);
  Fields.Add(FFdFieldOfView);
  { X3D specification comment: (0,Pi) }

  FFdHeadlight := TSFBool.Create(Self, 'headlight', true);
  Fields.Add(FFdHeadlight);

  FFdNavType := TMFString.Create(Self, 'navType', ['EXAMINE','ANY']);
  Fields.Add(FFdNavType);

  FFdGeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TGeoOriginNode]);
  FFdGeoOrigin.Exposed := false;
  Fields.Add(FFdGeoOrigin);

  FFdGeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
  FFdGeoSystem.Exposed := false;
  Fields.Add(FFdGeoSystem);
  { X3D specification comment: [see 25.2.3] }

  FFdPosition := TSFVec3d.Create(Self, 'position', Vector3Double(0, 0, 100000));
  FFdPosition.Exposed := false;
  Fields.Add(FFdPosition);
  { X3D specification comment: (-Inf,Inf) }

  FFdSpeedFactor := TSFFloat.Create(Self, 'speedFactor', 1.0);
  FFdSpeedFactor.Exposed := false;
  Fields.Add(FFdSpeedFactor);
  { X3D specification comment: [0,Inf) }
end;

class function TGeoViewpointNode.ClassNodeTypeName: string;
begin
  Result := 'GeoViewpoint';
end;

class function TGeoViewpointNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
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
