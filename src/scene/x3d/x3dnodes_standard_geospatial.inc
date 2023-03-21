{
  Copyright 2008-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ GEO X3D component.

  Note that X3D geospatial nodes are slightly incompatible with the VRML 97 geospatial nodes.
  We don't implement VRML 97 geospatial nodes -- the effort seems not worth it.
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
    function CoordCount: Cardinal; override;
  {$I auto_generated_node_helpers/x3dnodes_geocoordinate.inc}
  end;

  { Elevation grid expressed in geo-centric coordinates.
    @bold(Rendering of this node is not implemented yet.) }
  TGeoElevationGridNode = class(TAbstractGeometryNode)
  public
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function SolidField: TSFBool; override;
  {$I auto_generated_node_helpers/x3dnodes_geoelevationgrid.inc}
  end;

  TGeoLocationNode = class(TAbstractGroupingNode)
  {$I auto_generated_node_helpers/x3dnodes_geolocation.inc}
  end;

  TGeoLODNode = class(TAbstractInternalGroupingNode)
  {$I auto_generated_node_helpers/x3dnodes_geolod.inc}
  end;

  TGeoMetadataNode = class(TAbstractInfoNode)
  {$I auto_generated_node_helpers/x3dnodes_geometadata.inc}
  end;

  TGeoOriginNode = class(TAbstractNode)
  {$I auto_generated_node_helpers/x3dnodes_geoorigin.inc}
  end;

  TGeoPositionInterpolatorNode = class(TAbstractInterpolatorNode)
  {$I auto_generated_node_helpers/x3dnodes_geopositioninterpolator.inc}
  end;

  TGeoProximitySensorNode = class(TAbstractEnvironmentalSensorNode)
  {$I auto_generated_node_helpers/x3dnodes_geoproximitysensor.inc}
  end;

  TGeoTouchSensorNode = class(TAbstractTouchSensorNode)
  {$I auto_generated_node_helpers/x3dnodes_geotouchsensor.inc}
  end;

  TGeoTransformNode = class(TAbstractGroupingNode)
  {$I auto_generated_node_helpers/x3dnodes_geotransform.inc}
  end;

  TGeoViewpointNode = class(TAbstractViewpointNode)
  strict private
    DummyPositionSingle: TSFVec3f;
  strict protected
    function PositionField: TSFVec3f; override;
  public
    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;
    destructor Destroy; override;
  {$I auto_generated_node_helpers/x3dnodes_geoviewpoint.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TGeoCoordinateNode --------------------------------------------------------- }

function TGeoCoordinateNode.CoordCount: Cardinal;
begin
  Result := FdPoint.Items.Count;
end;

{ TGeoElevationGridNode ------------------------------------------------------ }

function TGeoElevationGridNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  { Rendering of TGeoElevationGridNode not implemented. }
  Result := TBox3D.Empty;
end;

function TGeoElevationGridNode.VerticesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TGeoElevationGridNode not implemented. }
  Result := 0;
end;

function TGeoElevationGridNode.TrianglesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TGeoElevationGridNode not implemented. }
  Result := 0;
end;

function TGeoElevationGridNode.SolidField: TSFBool;
begin
  Result := FdSolid;
end;

{ TGeoViewpointNode ---------------------------------------------------------- }

constructor TGeoViewpointNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  DummyPositionSingle := TSFVec3f.Create(Self, true, 'position', Vector3(0, 0, 100000));
end;

destructor TGeoViewpointNode.Destroy;
begin
  FreeAndNil(DummyPositionSingle);
  inherited;
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
