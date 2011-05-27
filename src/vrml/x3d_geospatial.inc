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
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdpoint: TMFVec3d;
    public property Fdpoint: TMFVec3d read FFdpoint;

    private FFdgeoOrigin: TSFNode;
    public property FdgeoOrigin: TSFNode read FFdgeoOrigin;

    private FFdgeoSystem: TMFString;
    public property FdgeoSystem: TMFString read FFdgeoSystem;

    function CoordCount: Cardinal; override;
  end;

  TNodeGeoElevationGrid = class(TNodeX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFDouble, in } { }
    private FEventset_height: TVRMLEvent;
    public property Eventset_height: TVRMLEvent read FEventset_height;

    private FFdcolor: TSFNode;
    public property Fdcolor: TSFNode read FFdcolor;

    private FFdnormal: TSFNode;
    public property Fdnormal: TSFNode read FFdnormal;

    private FFdtexCoord: TSFNode;
    public property FdtexCoord: TSFNode read FFdtexCoord;

    private FFdyScale: TSFFloat;
    public property FdyScale: TSFFloat read FFdyScale;

    private FFdccw: TSFBool;
    public property Fdccw: TSFBool read FFdccw;

    private FFdcolorPerVertex: TSFBool;
    public property FdcolorPerVertex: TSFBool read FFdcolorPerVertex;

    private FFdcreaseAngle: TSFDouble;
    public property FdcreaseAngle: TSFDouble read FFdcreaseAngle;

    private FFdgeoGridOrigin: TSFVec3d;
    public property FdgeoGridOrigin: TSFVec3d read FFdgeoGridOrigin;

    private FFdgeoOrigin: TSFNode;
    public property FdgeoOrigin: TSFNode read FFdgeoOrigin;

    private FFdgeoSystem: TMFString;
    public property FdgeoSystem: TMFString read FFdgeoSystem;

    private FFdheight: TMFDouble;
    public property Fdheight: TMFDouble read FFdheight;

    private FFdnormalPerVertex: TSFBool;
    public property FdnormalPerVertex: TSFBool read FFdnormalPerVertex;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;

    private FFdxDimension: TSFInt32;
    public property FdxDimension: TSFInt32 read FFdxDimension;

    private FFdxSpacing: TSFDouble;
    public property FdxSpacing: TSFDouble read FFdxSpacing;

    private FFdzDimension: TSFInt32;
    public property FdzDimension: TSFInt32 read FFdzDimension;

    private FFdzSpacing: TSFDouble;
    public property FdzSpacing: TSFDouble read FFdzSpacing;

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
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdgeoCoords: TSFVec3d;
    public property FdgeoCoords: TSFVec3d read FFdgeoCoords;

    private FFdgeoOrigin: TSFNode;
    public property FdgeoOrigin: TSFNode read FFdgeoOrigin;

    private FFdgeoSystem: TMFString;
    public property FdgeoSystem: TMFString read FFdgeoSystem;
  end;

  TNodeGeoLOD = class(TVRMLGroupingNode, INodeX3DBoundedObject)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFNode, out } { }
    private FEventchildren: TVRMLEvent;
    public property Eventchildren: TVRMLEvent read FEventchildren;

    { Event: SFInt32, out } { }
    private FEventlevel_changed: TVRMLEvent;
    public property Eventlevel_changed: TVRMLEvent read FEventlevel_changed;

    private FFdcenter: TSFVec3d;
    public property Fdcenter: TSFVec3d read FFdcenter;

    private FFdchild1Url: TMFString;
    public property Fdchild1Url: TMFString read FFdchild1Url;

    private FFdchild2Url: TMFString;
    public property Fdchild2Url: TMFString read FFdchild2Url;

    private FFdchild3Url: TMFString;
    public property Fdchild3Url: TMFString read FFdchild3Url;

    private FFdchild4Url: TMFString;
    public property Fdchild4Url: TMFString read FFdchild4Url;

    private FFdgeoOrigin: TSFNode;
    public property FdgeoOrigin: TSFNode read FFdgeoOrigin;

    private FFdgeoSystem: TMFString;
    public property FdgeoSystem: TMFString read FFdgeoSystem;

    private FFdrange: TSFFloat;
    public property Fdrange: TSFFloat read FFdrange;

    private FFdrootUrl: TMFString;
    public property FdrootUrl: TMFString read FFdrootUrl;

    private FFdrootNode: TMFNode;
    public property FdrootNode: TMFNode read FFdrootNode;

    private FFdbboxCenter: TSFVec3f;
    public property FdbboxCenter: TSFVec3f read FFdbboxCenter;

    private FFdbboxSize: TSFVec3f;
    public property FdbboxSize: TSFVec3f read FFdbboxSize;
  end;

  TNodeGeoMetadata = class(TNodeX3DInfoNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFddata: TMFNode;
    public property Fddata: TMFNode read FFddata;

    private FFdsummary: TMFString;
    public property Fdsummary: TMFString read FFdsummary;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;
  end;

  TNodeGeoOrigin = class(TNodeX3DNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdgeoCoords: TSFVec3d;
    public property FdgeoCoords: TSFVec3d read FFdgeoCoords;

    private FFdgeoSystem: TMFString;
    public property FdgeoSystem: TMFString read FFdgeoSystem;

    private FFdrotateYUp: TSFBool;
    public property FdrotateYUp: TSFBool read FFdrotateYUp;
  end;

  TNodeGeoPositionInterpolator = class(TNodeX3DInterpolatorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdkeyValue: TMFVec3d;
    public property FdkeyValue: TMFVec3d read FFdkeyValue;

    { Event: SFVec3d, out } { }
    private FEventgeovalue_changed: TVRMLEvent;
    public property Eventgeovalue_changed: TVRMLEvent read FEventgeovalue_changed;

    { Event: SFVec3f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;

    private FFdgeoOrigin: TSFNode;
    public property FdgeoOrigin: TSFNode read FFdgeoOrigin;

    private FFdgeoSystem: TMFString;
    public property FdgeoSystem: TMFString read FFdgeoSystem;
  end;

  TNodeGeoProximitySensor = class(TNodeX3DEnvironmentalSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdgeoCenter: TSFVec3d;
    public property FdgeoCenter: TSFVec3d read FFdgeoCenter;

    { Event: SFVec3f, out } { }
    private FEventcenterOfRotation_changed: TVRMLEvent;
    public property EventcenterOfRotation_changed: TVRMLEvent read FEventcenterOfRotation_changed;

    { Event: SFVec3d, out } { }
    private FEventgeoCoord_changed: TVRMLEvent;
    public property EventgeoCoord_changed: TVRMLEvent read FEventgeoCoord_changed;

    { Event: SFRotation, out } { }
    private FEventorientation_changed: TVRMLEvent;
    public property Eventorientation_changed: TVRMLEvent read FEventorientation_changed;

    { Event: SFVec3f, out } { }
    private FEventposition_changed: TVRMLEvent;
    public property Eventposition_changed: TVRMLEvent read FEventposition_changed;

    private FFdgeoOrigin: TSFNode;
    public property FdgeoOrigin: TSFNode read FFdgeoOrigin;

    private FFdgeoSystem: TMFString;
    public property FdgeoSystem: TMFString read FFdgeoSystem;
  end;

  TNodeGeoTouchSensor = class(TNodeX3DTouchSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFVec3f, out } { }
    private FEventhitNormal_changed: TVRMLEvent;
    public property EventhitNormal_changed: TVRMLEvent read FEventhitNormal_changed;

    { Event: SFVec3f, out } { }
    private FEventhitPoint_changed: TVRMLEvent;
    public property EventhitPoint_changed: TVRMLEvent read FEventhitPoint_changed;

    { Event: SFVec2f, out } { }
    private FEventhitTexCoord_changed: TVRMLEvent;
    public property EventhitTexCoord_changed: TVRMLEvent read FEventhitTexCoord_changed;

    { Event: SFVec3d, out } { }
    private FEventhitGeoCoord_changed: TVRMLEvent;
    public property EventhitGeoCoord_changed: TVRMLEvent read FEventhitGeoCoord_changed;

    private FFdgeoOrigin: TSFNode;
    public property FdgeoOrigin: TSFNode read FFdgeoOrigin;

    private FFdgeoSystem: TMFString;
    public property FdgeoSystem: TMFString read FFdgeoSystem;
  end;

  TNodeGeoTransform = class(TNodeX3DGroupingNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdgeoCenter: TSFVec3d;
    public property FdgeoCenter: TSFVec3d read FFdgeoCenter;

    private FFdrotation: TSFRotation;
    public property Fdrotation: TSFRotation read FFdrotation;

    private FFdscale: TSFVec3f;
    public property Fdscale: TSFVec3f read FFdscale;

    private FFdscaleOrientation: TSFRotation;
    public property FdscaleOrientation: TSFRotation read FFdscaleOrientation;

    private FFdtranslation: TSFVec3f;
    public property Fdtranslation: TSFVec3f read FFdtranslation;

    private FFdgeoOrigin: TSFNode;
    public property FdgeoOrigin: TSFNode read FFdgeoOrigin;

    private FFdgeoSystem: TMFString;
    public property FdgeoSystem: TMFString read FFdgeoSystem;
  end;

  TNodeGeoViewpoint = class(TNodeX3DViewpointNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFRotation, in } { }
    private FEventset_orientation: TVRMLEvent;
    public property Eventset_orientation: TVRMLEvent read FEventset_orientation;

    { Event: SFVec3d, in } { }
    private FEventset_position: TVRMLEvent;
    public property Eventset_position: TVRMLEvent read FEventset_position;

    private FFdfieldOfView: TSFFloat;
    public property FdfieldOfView: TSFFloat read FFdfieldOfView;

    private FFdheadlight: TSFBool;
    public property Fdheadlight: TSFBool read FFdheadlight;

    private FFdnavType: TMFString;
    public property FdnavType: TMFString read FFdnavType;

    private FFdgeoOrigin: TSFNode;
    public property FdgeoOrigin: TSFNode read FFdgeoOrigin;

    private FFdgeoSystem: TMFString;
    public property FdgeoSystem: TMFString read FFdgeoSystem;

    private FFdposition: TSFVec3d;
    public property Fdposition: TSFVec3d read FFdposition;

    private FFdspeedFactor: TSFFloat;
    public property FdspeedFactor: TSFFloat read FFdspeedFactor;
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

function TNodeGeoCoordinate.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, VRML2Version, 1000);
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

function TNodeGeoElevationGrid.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  { There is very similar, but not 100% compatible node in VRML 97 }
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeGeoLocation.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeGeoLOD.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeGeoMetadata.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeGeoOrigin.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeGeoPositionInterpolator.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeGeoProximitySensor.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeGeoTouchSensor.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeGeoTransform.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeGeoViewpoint.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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
