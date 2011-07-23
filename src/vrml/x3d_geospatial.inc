{
  Copyright 2008-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
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
  TNodeGeoCoordinate = class(TNodeX3DCoordinateNode)
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

  TNodeGeoElevationGrid = class(TNodeX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFDouble, in } { }
    private FEventSet_height: TVRMLEvent;
    public property EventSet_height: TVRMLEvent read FEventSet_height;

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
    function LocalBoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
  end;

  TNodeGeoLocation = class(TNodeX3DGroupingNode)
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

  TNodeGeoLOD = class(TVRMLGroupingNode, INodeX3DBoundedObject)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFNode, out } { }
    private FEventChildren: TVRMLEvent;
    public property EventChildren: TVRMLEvent read FEventChildren;

    { Event: SFInt32, out } { }
    private FEventLevel_changed: TVRMLEvent;
    public property EventLevel_changed: TVRMLEvent read FEventLevel_changed;

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

  TNodeGeoMetadata = class(TNodeX3DInfoNode)
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

  TNodeGeoOrigin = class(TNodeX3DNode)
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

  TNodeGeoPositionInterpolator = class(TNodeX3DInterpolatorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFVec3d;
    public property FdKeyValue: TMFVec3d read FFdKeyValue;

    { Event: SFVec3d, out } { }
    private FEventGeovalue_changed: TVRMLEvent;
    public property EventGeovalue_changed: TVRMLEvent read FEventGeovalue_changed;

    { Event: SFVec3f, out } { }
    private FEventValue_changed: TVRMLEvent;
    public property EventValue_changed: TVRMLEvent read FEventValue_changed;

    private FFdGeoOrigin: TSFNode;
    public property FdGeoOrigin: TSFNode read FFdGeoOrigin;

    private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;
  end;

  TNodeGeoProximitySensor = class(TNodeX3DEnvironmentalSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdGeoCenter: TSFVec3d;
    public property FdGeoCenter: TSFVec3d read FFdGeoCenter;

    { Event: SFVec3f, out } { }
    private FEventCenterOfRotation_changed: TVRMLEvent;
    public property EventCenterOfRotation_changed: TVRMLEvent read FEventCenterOfRotation_changed;

    { Event: SFVec3d, out } { }
    private FEventGeoCoord_changed: TVRMLEvent;
    public property EventGeoCoord_changed: TVRMLEvent read FEventGeoCoord_changed;

    { Event: SFRotation, out } { }
    private FEventOrientation_changed: TVRMLEvent;
    public property EventOrientation_changed: TVRMLEvent read FEventOrientation_changed;

    { Event: SFVec3f, out } { }
    private FEventPosition_changed: TVRMLEvent;
    public property EventPosition_changed: TVRMLEvent read FEventPosition_changed;

    private FFdGeoOrigin: TSFNode;
    public property FdGeoOrigin: TSFNode read FFdGeoOrigin;

    private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;
  end;

  TNodeGeoTouchSensor = class(TNodeX3DTouchSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFVec3f, out } { }
    private FEventHitNormal_changed: TVRMLEvent;
    public property EventHitNormal_changed: TVRMLEvent read FEventHitNormal_changed;

    { Event: SFVec3f, out } { }
    private FEventHitPoint_changed: TVRMLEvent;
    public property EventHitPoint_changed: TVRMLEvent read FEventHitPoint_changed;

    { Event: SFVec2f, out } { }
    private FEventHitTexCoord_changed: TVRMLEvent;
    public property EventHitTexCoord_changed: TVRMLEvent read FEventHitTexCoord_changed;

    { Event: SFVec3d, out } { }
    private FEventHitGeoCoord_changed: TVRMLEvent;
    public property EventHitGeoCoord_changed: TVRMLEvent read FEventHitGeoCoord_changed;

    private FFdGeoOrigin: TSFNode;
    public property FdGeoOrigin: TSFNode read FFdGeoOrigin;

    private FFdGeoSystem: TMFString;
    public property FdGeoSystem: TMFString read FFdGeoSystem;
  end;

  TNodeGeoTransform = class(TNodeX3DGroupingNode)
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

  TNodeGeoViewpoint = class(TNodeX3DViewpointNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFRotation, in } { }
    private FEventSet_orientation: TVRMLEvent;
    public property EventSet_orientation: TVRMLEvent read FEventSet_orientation;

    { Event: SFVec3d, in } { }
    private FEventSet_position: TVRMLEvent;
    public property EventSet_position: TVRMLEvent read FEventSet_position;

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
procedure TNodeGeoCoordinate.CreateNode;
begin
  inherited;

  FFdpoint := TMFVec3d.Create(Self, 'point', []);
  Fields.Add(FFdpoint);
  { X3D specification comment: (-Inf,Inf) }

  FFdgeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TNodeGeoOrigin]);
  FFdgeoOrigin.Exposed := false;
  Fields.Add(FFdgeoOrigin);

  FFdgeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
  FFdgeoSystem.Exposed := false;
  Fields.Add(FFdgeoSystem);
  { X3D specification comment: [see 25.2.3] }
end;

class function TNodeGeoCoordinate.ClassNodeTypeName: string;
begin
  Result := 'GeoCoordinate';
end;

class function TNodeGeoCoordinate.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TNodeGeoCoordinate.CoordCount: Cardinal;
begin
  Result := FdPoint.Items.Count;
end;

procedure TNodeGeoElevationGrid.CreateNode;
begin
  inherited;

  FEventset_height := TVRMLEvent.Create(Self, 'set_height', TMFDouble, true);
  Events.Add(FEventset_height);

  FFdcolor := TSFNode.Create(Self, 'color', [TNodeX3DColorNode]);
   Fdcolor.ChangesAlways := [chGeometry];
  Fields.Add(FFdcolor);

  FFdnormal := TSFNode.Create(Self, 'normal', [TNodeX3DNormalNode]);
   Fdnormal.ChangesAlways := [chGeometry];
  Fields.Add(FFdnormal);

  FFdtexCoord := TSFNode.Create(Self, 'texCoord', [TNodeX3DTextureCoordinateNode]);
   FdtexCoord.ChangesAlways := [chGeometry];
  Fields.Add(FFdtexCoord);

  FFdyScale := TSFFloat.Create(Self, 'yScale', 1.0);
   FdyScale.ChangesAlways := [chGeometry];
  Fields.Add(FFdyScale);
  { X3D specification comment: [0,Inf) }

  FFdccw := TSFBool.Create(Self, 'ccw', true);
   Fdccw.Exposed := false;
   Fdccw.ChangesAlways := [chGeometry];
  Fields.Add(FFdccw);

  FFdcolorPerVertex := TSFBool.Create(Self, 'colorPerVertex', true);
   FdcolorPerVertex.Exposed := false;
   FdcolorPerVertex.ChangesAlways := [chGeometry];
  Fields.Add(FFdcolorPerVertex);

  FFdcreaseAngle := TSFDouble.Create(Self, 'creaseAngle', 0);
   FdcreaseAngle.Exposed := false;
   FdcreaseAngle.ChangesAlways := [chGeometry];
  Fields.Add(FFdcreaseAngle);
  { X3D specification comment: [0,Inf) }

  FFdgeoGridOrigin := TSFVec3d.Create(Self, 'geoGridOrigin', Vector3Double(0, 0, 0));
   FdgeoGridOrigin.Exposed := false;
   FdgeoGridOrigin.ChangesAlways := [chGeometry];
  Fields.Add(FFdgeoGridOrigin);
  { X3D specification comment: (-Inf,Inf) }

  FFdgeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TNodeGeoOrigin]);
   FdgeoOrigin.Exposed := false;
   FdgeoOrigin.ChangesAlways := [chGeometry];
  Fields.Add(FFdgeoOrigin);

  FFdgeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
   FdgeoSystem.Exposed := false;
   FdgeoSystem.ChangesAlways := [chGeometry];
  Fields.Add(FFdgeoSystem);
  { X3D specification comment: [see 25.2.3] }

  FFdheight := TMFDouble.Create(Self, 'height', [0, 0]);
   Fdheight.Exposed := false;
   Fdheight.ChangesAlways := [chGeometry];
  Fields.Add(FFdheight);
  { X3D specification comment: (-Inf,Inf) }

  FFdnormalPerVertex := TSFBool.Create(Self, 'normalPerVertex', true);
   FdnormalPerVertex.Exposed := false;
   FdnormalPerVertex.ChangesAlways := [chGeometry];
  Fields.Add(FFdnormalPerVertex);

  FFdsolid := TSFBool.Create(Self, 'solid', true);
   Fdsolid.Exposed := false;
   Fdsolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdsolid);

  FFdxDimension := TSFInt32.Create(Self, 'xDimension', 0);
   FdxDimension.Exposed := false;
   FdxDimension.ChangesAlways := [chGeometry];
  Fields.Add(FFdxDimension);
  { X3D specification comment: (0,Inf) }

  FFdxSpacing := TSFDouble.Create(Self, 'xSpacing', 1.0);
   FdxSpacing.Exposed := false;
   FdxSpacing.ChangesAlways := [chGeometry];
  Fields.Add(FFdxSpacing);
  { X3D specification comment: [0,Inf) }

  FFdzDimension := TSFInt32.Create(Self, 'zDimension', 0);
   FdzDimension.Exposed := false;
   FdzDimension.ChangesAlways := [chGeometry];
  Fields.Add(FFdzDimension);
  { X3D specification comment: (0,Inf) }

  FFdzSpacing := TSFDouble.Create(Self, 'zSpacing', 1.0);
   FdzSpacing.Exposed := false;
   FdzSpacing.ChangesAlways := [chGeometry];
  Fields.Add(FFdzSpacing);
  { X3D specification comment: [0,Inf) }
end;

class function TNodeGeoElevationGrid.ClassNodeTypeName: string;
begin
  Result := 'GeoElevationGrid';
end;

class function TNodeGeoElevationGrid.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    { There is very similar, but not 100% compatible node in VRML 97 }
    { (URN = URNVRML97Nodes + ClassNodeTypeName) or }
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

{$define TGeometryNotImplemented := TNodeGeoElevationGrid}
GeometryNotImplemented

procedure TNodeGeoLocation.CreateNode;
begin
  inherited;

  FFdgeoCoords := TSFVec3d.Create(Self, 'geoCoords', Vector3Double(0, 0, 0));
  Fields.Add(FFdgeoCoords);
  { X3D specification comment: (-Inf,Inf) }

  FFdgeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TNodeGeoOrigin]);
  FFdgeoOrigin.Exposed := false;
  Fields.Add(FFdgeoOrigin);

  FFdgeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
  FFdgeoSystem.Exposed := false;
  Fields.Add(FFdgeoSystem);
  { X3D specification comment: [see 25.2.3] }
end;

class function TNodeGeoLocation.ClassNodeTypeName: string;
begin
  Result := 'GeoLocation';
end;

class function TNodeGeoLocation.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeGeoLOD.CreateNode;
begin
  inherited;

  FEventchildren := TVRMLEvent.Create(Self, 'children', TMFNode, false);
  Events.Add(FEventchildren);

  FEventlevel_changed := TVRMLEvent.Create(Self, 'level_changed', TSFInt32, false);
  Events.Add(FEventlevel_changed);

  FFdcenter := TSFVec3d.Create(Self, 'center', Vector3Double(0, 0, 0));
  FFdcenter.Exposed := false;
  Fields.Add(FFdcenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdchild1Url := TMFString.Create(Self, 'child1Url', []);
  FFdchild1Url.Exposed := false;
  Fields.Add(FFdchild1Url);
  { X3D specification comment: [URI] }

  FFdchild2Url := TMFString.Create(Self, 'child2Url', []);
  FFdchild2Url.Exposed := false;
  Fields.Add(FFdchild2Url);
  { X3D specification comment: [URI] }

  FFdchild3Url := TMFString.Create(Self, 'child3Url', []);
  FFdchild3Url.Exposed := false;
  Fields.Add(FFdchild3Url);
  { X3D specification comment: [URI] }

  FFdchild4Url := TMFString.Create(Self, 'child4Url', []);
  FFdchild4Url.Exposed := false;
  Fields.Add(FFdchild4Url);
  { X3D specification comment: [URI] }

  FFdgeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TNodeGeoOrigin]);
  FFdgeoOrigin.Exposed := false;
  Fields.Add(FFdgeoOrigin);

  FFdgeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
  FFdgeoSystem.Exposed := false;
  Fields.Add(FFdgeoSystem);
  { X3D specification comment: [see 25.2.3] }

  FFdrange := TSFFloat.Create(Self, 'range', 10);
  FFdrange.Exposed := false;
  Fields.Add(FFdrange);
  { X3D specification comment: [0,Inf) }

  FFdrootUrl := TMFString.Create(Self, 'rootUrl', []);
  FFdrootUrl.Exposed := false;
  Fields.Add(FFdrootUrl);
  { X3D specification comment: [URI] }

  FFdrootNode := TMFNode.Create(Self, 'rootNode', [TNodeX3DChildNode]);
  FFdrootNode.Exposed := false;
  Fields.Add(FFdrootNode);

  FFdbboxCenter := TSFVec3f.Create(Self, 'bboxCenter', Vector3Single(0, 0, 0));
  FFdbboxCenter.Exposed := false;
  Fields.Add(FFdbboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdbboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
  FFdbboxSize.Exposed := false;
  Fields.Add(FFdbboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }
end;

class function TNodeGeoLOD.ClassNodeTypeName: string;
begin
  Result := 'GeoLOD';
end;

class function TNodeGeoLOD.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeGeoMetadata.CreateNode;
begin
  inherited;

  FFddata := TMFNode.Create(Self, 'data', []);
  Fields.Add(FFddata);

  FFdsummary := TMFString.Create(Self, 'summary', []);
  Fields.Add(FFdsummary);

  FFdurl := TMFString.Create(Self, 'url', []);
  Fields.Add(FFdurl);
  { X3D specification comment: [URI] }
end;

class function TNodeGeoMetadata.ClassNodeTypeName: string;
begin
  Result := 'GeoMetadata';
end;

class function TNodeGeoMetadata.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeGeoOrigin.CreateNode;
begin
  inherited;

  FFdgeoCoords := TSFVec3d.Create(Self, 'geoCoords', Vector3Double(0, 0, 0));
  Fields.Add(FFdgeoCoords);
  { X3D specification comment: (-Inf,Inf) }

  FFdgeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
  FFdgeoSystem.Exposed := false;
  Fields.Add(FFdgeoSystem);
  { X3D specification comment: [see 25.2.3] }

  FFdrotateYUp := TSFBool.Create(Self, 'rotateYUp', false);
  FFdrotateYUp.Exposed := false;
  Fields.Add(FFdrotateYUp);

  DefaultContainerField := 'geoOrigin';
end;

class function TNodeGeoOrigin.ClassNodeTypeName: string;
begin
  Result := 'GeoOrigin';
end;

class function TNodeGeoOrigin.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeGeoPositionInterpolator.CreateNode;
begin
  inherited;

  FFdkeyValue := TMFVec3d.Create(Self, 'keyValue', []);
  Fields.Add(FFdkeyValue);

  FEventgeovalue_changed := TVRMLEvent.Create(Self, 'geovalue_changed', TSFVec3d, false);
  Events.Add(FEventgeovalue_changed);

  FEventvalue_changed := TVRMLEvent.Create(Self, 'value_changed', TSFVec3f, false);
  Events.Add(FEventvalue_changed);

  FFdgeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TNodeGeoOrigin]);
  FFdgeoOrigin.Exposed := false;
  Fields.Add(FFdgeoOrigin);

  FFdgeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
  FFdgeoSystem.Exposed := false;
  Fields.Add(FFdgeoSystem);
  { X3D specification comment: [see 25.2.3] }
end;

class function TNodeGeoPositionInterpolator.ClassNodeTypeName: string;
begin
  Result := 'GeoPositionInterpolator';
end;

class function TNodeGeoPositionInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeGeoProximitySensor.CreateNode;
begin
  inherited;

  FFdgeoCenter := TSFVec3d.Create(Self, 'geoCenter', Vector3Double(0, 0, 0));
  Fields.Add(FFdgeoCenter);
  { X3D specification comment: (-Inf,Inf) }

  FEventcenterOfRotation_changed := TVRMLEvent.Create(Self, 'centerOfRotation_changed', TSFVec3f, false);
  Events.Add(FEventcenterOfRotation_changed);

  FEventgeoCoord_changed := TVRMLEvent.Create(Self, 'geoCoord_changed', TSFVec3d, false);
  Events.Add(FEventgeoCoord_changed);

  FEventorientation_changed := TVRMLEvent.Create(Self, 'orientation_changed', TSFRotation, false);
  Events.Add(FEventorientation_changed);

  FEventposition_changed := TVRMLEvent.Create(Self, 'position_changed', TSFVec3f, false);
  Events.Add(FEventposition_changed);

  FFdgeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TNodeGeoOrigin]);
  FFdgeoOrigin.Exposed := false;
  Fields.Add(FFdgeoOrigin);

  FFdgeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
  FFdgeoSystem.Exposed := false;
  Fields.Add(FFdgeoSystem);
  { X3D specification comment: [see 25.2.3] }
end;

class function TNodeGeoProximitySensor.ClassNodeTypeName: string;
begin
  Result := 'GeoProximitySensor';
end;

class function TNodeGeoProximitySensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeGeoTouchSensor.CreateNode;
begin
  inherited;

  FEventhitNormal_changed := TVRMLEvent.Create(Self, 'hitNormal_changed', TSFVec3f, false);
  Events.Add(FEventhitNormal_changed);

  FEventhitPoint_changed := TVRMLEvent.Create(Self, 'hitPoint_changed', TSFVec3f, false);
  Events.Add(FEventhitPoint_changed);

  FEventhitTexCoord_changed := TVRMLEvent.Create(Self, 'hitTexCoord_changed', TSFVec2f, false);
  Events.Add(FEventhitTexCoord_changed);

  FEventhitGeoCoord_changed := TVRMLEvent.Create(Self, 'hitGeoCoord_changed', TSFVec3d, false);
  Events.Add(FEventhitGeoCoord_changed);

  FFdgeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TNodeGeoOrigin]);
  FFdgeoOrigin.Exposed := false;
  Fields.Add(FFdgeoOrigin);

  FFdgeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
  FFdgeoSystem.Exposed := false;
  Fields.Add(FFdgeoSystem);
  { X3D specification comment: [see 25.2.3] }
end;

class function TNodeGeoTouchSensor.ClassNodeTypeName: string;
begin
  Result := 'GeoTouchSensor';
end;

class function TNodeGeoTouchSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeGeoTransform.CreateNode;
begin
  inherited;

  FFdgeoCenter := TSFVec3d.Create(Self, 'geoCenter', Vector3Double(0, 0, 0));
  Fields.Add(FFdgeoCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdrotation := TSFRotation.Create(Self, 'rotation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdrotation);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdscale := TSFVec3f.Create(Self, 'scale', Vector3Single(1, 1, 1));
  Fields.Add(FFdscale);
  { X3D specification comment: (0,Inf) }

  FFdscaleOrientation := TSFRotation.Create(Self, 'scaleOrientation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdscaleOrientation);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdtranslation := TSFVec3f.Create(Self, 'translation', Vector3Single(0, 0, 0));
  Fields.Add(FFdtranslation);
  { X3D specification comment: (-Inf,Inf) }

  FFdgeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TNodeGeoOrigin]);
  FFdgeoOrigin.Exposed := false;
  Fields.Add(FFdgeoOrigin);

  FFdgeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
  FFdgeoSystem.Exposed := false;
  Fields.Add(FFdgeoSystem);
  { X3D specification comment: [see 25.2.3] }
end;

class function TNodeGeoTransform.ClassNodeTypeName: string;
begin
  Result := 'GeoTransform';
end;

class function TNodeGeoTransform.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeGeoViewpoint.CreateNode;
begin
  inherited;

  FEventset_orientation := TVRMLEvent.Create(Self, 'set_orientation', TSFRotation, true);
  Events.Add(FEventset_orientation);

  FEventset_position := TVRMLEvent.Create(Self, 'set_position', TSFVec3d, true);
  Events.Add(FEventset_position);

  FFdfieldOfView := TSFFloat.Create(Self, 'fieldOfView', Pi/4);
  Fields.Add(FFdfieldOfView);
  { X3D specification comment: (0,Pi) }

  FFdheadlight := TSFBool.Create(Self, 'headlight', true);
  Fields.Add(FFdheadlight);

  FFdnavType := TMFString.Create(Self, 'navType', ['EXAMINE','ANY']);
  Fields.Add(FFdnavType);

  FFdgeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TNodeGeoOrigin]);
  FFdgeoOrigin.Exposed := false;
  Fields.Add(FFdgeoOrigin);

  FFdgeoSystem := TMFString.Create(Self, 'geoSystem', ['GD','WE']);
  FFdgeoSystem.Exposed := false;
  Fields.Add(FFdgeoSystem);
  { X3D specification comment: [see 25.2.3] }

  FFdposition := TSFVec3d.Create(Self, 'position', Vector3Double(0, 0, 100000));
  FFdposition.Exposed := false;
  Fields.Add(FFdposition);
  { X3D specification comment: (-Inf,Inf) }

  FFdspeedFactor := TSFFloat.Create(Self, 'speedFactor', 1.0);
  FFdspeedFactor.Exposed := false;
  Fields.Add(FFdspeedFactor);
  { X3D specification comment: [0,Inf) }
end;

class function TNodeGeoViewpoint.ClassNodeTypeName: string;
begin
  Result := 'GeoViewpoint';
end;

class function TNodeGeoViewpoint.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterGeospatialNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeGeoCoordinate,
    TNodeGeoElevationGrid,
    TNodeGeoLocation,
    TNodeGeoLOD,
    TNodeGeoMetadata,
    TNodeGeoOrigin,
    TNodeGeoPositionInterpolator,
    TNodeGeoProximitySensor,
    TNodeGeoTouchSensor,
    TNodeGeoTransform,
    TNodeGeoViewpoint
  ]);
end;

{$endif read_implementation}
