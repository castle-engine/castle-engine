{
  Copyright 2002-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { }
  TNodeBox = class(TNodeX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdSize: TSFVec3f;
    public property FdSize: TSFVec3f read FFdSize;

    private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    private FFdTexCoord: TSFNode;
    public property FdTexCoord: TSFNode read FFdTexCoord;
    function TexCoordField: TSFNode; override;

    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;

    function BoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function LocalBoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
    function AutoGenerate3DTexCoords: boolean; override;
  end;

  TNodeCone = class(TNodeX3DGeometryNode)
  protected
    procedure DirectEnumerateActive(Func: TEnumerateChildrenFunction); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdBottom: TSFBool;
    public property FdBottom: TSFBool read FFdBottom;

    private FFdBottomRadius: TSFFloat;
    public property FdBottomRadius: TSFFloat read FFdBottomRadius;

    private FFdHeight: TSFFloat;
    public property FdHeight: TSFFloat read FFdHeight;

    private FFdSide: TSFBool;
    public property FdSide: TSFBool read FFdSide;

    private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    class function ForVRMLVersion(const Version: TVRMLVersion): boolean;
      override;

    private FFdTexCoord: TSFNode;
    public property FdTexCoord: TSFNode read FFdTexCoord;
    function TexCoordField: TSFNode; override;

    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;

    function BoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function LocalBoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
    function AutoGenerate3DTexCoords: boolean; override;
  end;
  TNodeCone_2 = TNodeCone;

  TNodeCylinder = class(TNodeX3DGeometryNode)
  protected
    procedure DirectEnumerateActive(Func: TEnumerateChildrenFunction); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdBottom: TSFBool;
    public property FdBottom: TSFBool read FFdBottom;

    private FFdHeight: TSFFloat;
    public property FdHeight: TSFFloat read FFdHeight;

    private FFdRadius: TSFFloat;
    public property FdRadius: TSFFloat read FFdRadius;

    private FFdSide: TSFBool;
    public property FdSide: TSFBool read FFdSide;

    private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    private FFdTop: TSFBool;
    public property FdTop: TSFBool read FFdTop;

    class function ForVRMLVersion(const Version: TVRMLVersion): boolean;
      override;

    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;

    private FFdTexCoord: TSFNode;
    public property FdTexCoord: TSFNode read FFdTexCoord;
    function TexCoordField: TSFNode; override;

    function BoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function LocalBoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
    function AutoGenerate3DTexCoords: boolean; override;
  end;
  TNodeCylinder_2 = TNodeCylinder;

  TNodeElevationGrid = class(TNodeX3DGeometryNode)
  private
    procedure EventSet_HeightReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFFloat, in } { }
    private FEventSet_height: TVRMLEvent;
    public property EventSet_height: TVRMLEvent read FEventSet_height;

    private FFdAttrib: TMFNode;
    public property FdAttrib: TMFNode read FFdAttrib;

    private FFdColor: TSFNode;
    public property FdColor: TSFNode read FFdColor;

    private FFdFogCoord: TSFNode;
    public property FdFogCoord: TSFNode read FFdFogCoord;

    private FFdNormal: TSFNode;
    public property FdNormal: TSFNode read FFdNormal;

    private FFdTexCoord: TSFNode;
    public property FdTexCoord: TSFNode read FFdTexCoord;

    private FFdCcw: TSFBool;
    public property FdCcw: TSFBool read FFdCcw;

    private FFdColorPerVertex: TSFBool;
    public property FdColorPerVertex: TSFBool read FFdColorPerVertex;

    private FFdCreaseAngle: TSFFloat;
    public property FdCreaseAngle: TSFFloat read FFdCreaseAngle;

    private FFdHeight: TMFFloat;
    public property FdHeight: TMFFloat read FFdHeight;

    private FFdNormalPerVertex: TSFBool;
    public property FdNormalPerVertex: TSFBool read FFdNormalPerVertex;

    private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    private FFdXDimension: TSFInt32;
    public property FdXDimension: TSFInt32 read FFdXDimension;

    private FFdXSpacing: TSFFloat;
    public property FdXSpacing: TSFFloat read FFdXSpacing;

    private FFdZDimension: TSFInt32;
    public property FdZDimension: TSFInt32 read FFdZDimension;

    private FFdZSpacing: TSFFloat;
    public property FdZSpacing: TSFFloat read FFdZSpacing;

    { This checks whether xDimension and zDimension are >= 2,
      xSpacing and zSpacing are > 0 and height has at least the
      required number of values. If this returns @false then
      it is understood that ElevationGrid is not rendered, doesn't
      have any vertices/triangles etc. }
    function IsNotEmpty: boolean;

    function Color: TMFVec3f; override;
    function ColorRGBA: TMFColorRGBA; override;
    function FogCoord: TMFFloat; override;
    function Attrib: TMFNode; override;

    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;
    function ProxyUsesOverTriangulate: boolean; override;

    function LocalBoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
  end;

  TNodeExtrusion = class(TNodeX3DGeometryNode)
  private
    procedure Eventset_crossSectionReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
    procedure Eventset_orientationReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
    procedure Eventset_scaleReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
    procedure Eventset_spineReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFVec2f, in } { }
    private FEventSet_crossSection: TVRMLEvent;
    public property EventSet_crossSection: TVRMLEvent read FEventSet_crossSection;

    { Event: MFRotation, in } { }
    private FEventSet_orientation: TVRMLEvent;
    public property EventSet_orientation: TVRMLEvent read FEventSet_orientation;

    { Event: MFVec2f, in } { }
    private FEventSet_scale: TVRMLEvent;
    public property EventSet_scale: TVRMLEvent read FEventSet_scale;

    { Event: MFVec3f, in } { }
    private FEventSet_spine: TVRMLEvent;
    public property EventSet_spine: TVRMLEvent read FEventSet_spine;

    private FFdBeginCap: TSFBool;
    public property FdBeginCap: TSFBool read FFdBeginCap;

    private FFdCcw: TSFBool;
    public property FdCcw: TSFBool read FFdCcw;

    private FFdConvex: TSFBool;
    public property FdConvex: TSFBool read FFdConvex;
    function Convex: boolean; override;

    private FFdCreaseAngle: TSFFloat;
    public property FdCreaseAngle: TSFFloat read FFdCreaseAngle;

    private FFdCrossSection: TMFVec2f;
    public property FdCrossSection: TMFVec2f read FFdCrossSection;

    private FFdEndCap: TSFBool;
    public property FdEndCap: TSFBool read FFdEndCap;

    private FFdOrientation: TMFRotation;
    public property FdOrientation: TMFRotation read FFdOrientation;

    private FFdScale: TMFVec2f;
    public property FdScale: TMFVec2f read FFdScale;

    private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    private FFdSpine: TMFVec3f;
    public property FdSpine: TMFVec3f read FFdSpine;

    { For given Index, return appropriate FdCrossIndex item as 3D vertex.
      That is, uses FdCrossIndex values as X, Z of 3D vertex and sets Y = 0
      (that's how Extrusion is defined in VRML / X3D). }
    function CrossSection3D(Index: integer): TVector3Single;

    { Check is spine closed. This happens when "spine" field is non-empty
      and it's first and last points are equal. }
    function SpineClosed: boolean;

    { Check is crossSection closed. This happens when "crossSection"
      field is non-empty and it's first and last points are equal. }
    function CrossSectionClosed: boolean;

    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;
    function ProxyUsesOverTriangulate: boolean; override;

    function LocalBoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function BoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
  end;

  TNodeIndexedFaceSet = class(TNodeX3DComposedGeometryNode)
  private
    procedure Eventset_colorIndexReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
    procedure Eventset_coordIndexReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
    procedure Eventset_normalIndexReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
    procedure Eventset_texCoordIndexReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFInt32, in } { }
    private FEventSet_colorIndex: TVRMLEvent;
    public property EventSet_colorIndex: TVRMLEvent read FEventSet_colorIndex;

    { Event: MFInt32, in } { }
    private FEventSet_coordIndex: TVRMLEvent;
    public property EventSet_coordIndex: TVRMLEvent read FEventSet_coordIndex;

    { Event: MFInt32, in } { }
    private FEventSet_normalIndex: TVRMLEvent;
    public property EventSet_normalIndex: TVRMLEvent read FEventSet_normalIndex;

    { Event: MFInt32, in } { }
    private FEventSet_texCoordIndex: TVRMLEvent;
    public property EventSet_texCoordIndex: TVRMLEvent read FEventSet_texCoordIndex;

    private FFdColorIndex: TMFInt32;
    public property FdColorIndex: TMFInt32 read FFdColorIndex;

    private FFdConvex: TSFBool;
    public property FdConvex: TSFBool read FFdConvex;
    function Convex: boolean; override;

    private FFdCoordIndex: TMFInt32;
    public property FdCoordIndex: TMFInt32 read FFdCoordIndex;

    private FFdCreaseAngle: TSFFloat;
    public property FdCreaseAngle: TSFFloat read FFdCreaseAngle;

    private FFdNormalIndex: TMFInt32;
    public property FdNormalIndex: TMFInt32 read FFdNormalIndex;

    private FFdTexCoordIndex: TMFInt32;
    public property FdTexCoordIndex: TMFInt32 read FFdTexCoordIndex;

    class function ForVRMLVersion(const Version: TVRMLVersion): boolean;
      override;

    function CoordIndex: TMFLong; override;

    procedure CoordPolygons(
      State: TVRMLGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;

    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
  end;
  TNodeIndexedFaceSet_2 = TNodeIndexedFaceSet;

  TNodeSphere = class(TNodeX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdRadius: TSFFloat;
    public property FdRadius: TSFFloat read FFdRadius;

    private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    class function ForVRMLVersion(const Version: TVRMLVersion): boolean;
      override;

    private FFdTexCoord: TSFNode;
    public property FdTexCoord: TSFNode read FFdTexCoord;
    function TexCoordField: TSFNode; override;

    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;

    function BoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function LocalBoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
    function AutoGenerate3DTexCoords: boolean; override;
  end;
  TNodeSphere_2 = TNodeSphere;

{$endif read_interface}

{$ifdef read_implementation}
procedure TNodeBox.CreateNode;
begin
  inherited;

  FFdSize := TSFVec3f.Create(Self, 'size', Vector3Single(2, 2, 2));
   FdSize.Exposed := false;
   FdSize.ChangesAlways := [chGeometry];
  Fields.Add(FFdSize);
  { X3D specification comment: (0,Inf) }

  FFdSolid := TSFBool.Create(Self, 'solid', true);
   FdSolid.Exposed := false;
   FdSolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdSolid);

  FFdTexCoord := TSFNode.Create(Self, 'texCoord', [TNodeTextureCoordinateGenerator, TNodeProjectedTextureCoordinate, TNodeMultiTextureCoordinate]);
   FdTexCoord.ChangesAlways := [chGeometry];
  Fields.Add(FFdTexCoord);
end;

class function TNodeBox.ClassNodeTypeName: string;
begin
  Result := 'Box';
end;

class function TNodeBox.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TNodeBox.TexCoordField: TSFNode;
begin
  Result := FdTexCoord;
end;

function TNodeBox.AutoGenerate3DTexCoords: boolean;
begin
  Result := (FdTexCoord.Value = nil) or not FdTexCoord.CurrentChildAllowed;
end;

procedure TNodeCone.CreateNode;
begin
  inherited;

  FFdBottom := TSFBool.Create(Self, 'bottom', true);
   FdBottom.Exposed := false;
   FdBottom.ChangesAlways := [chGeometry];
  Fields.Add(FFdBottom);

  FFdBottomRadius := TSFFloat.Create(Self, 'bottomRadius', 1);
   FdBottomRadius.Exposed := false;
   FdBottomRadius.ChangesAlways := [chGeometry];
  Fields.Add(FFdBottomRadius);
  { X3D specification comment: (0,Inf) }

  FFdHeight := TSFFloat.Create(Self, 'height', 2);
   FdHeight.Exposed := false;
   FdHeight.ChangesAlways := [chGeometry];
  Fields.Add(FFdHeight);
  { X3D specification comment: (0,Inf) }

  FFdSide := TSFBool.Create(Self, 'side', true);
   FdSide.Exposed := false;
   FdSide.ChangesAlways := [chGeometry];
  Fields.Add(FFdSide);

  FFdSolid := TSFBool.Create(Self, 'solid', true);
   FdSolid.Exposed := false;
   FdSolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdSolid);

  FFdTexCoord := TSFNode.Create(Self, 'texCoord', [TNodeTextureCoordinateGenerator, TNodeProjectedTextureCoordinate, TNodeMultiTextureCoordinate]);
   FdTexCoord.ChangesAlways := [chGeometry];
  Fields.Add(FFdTexCoord);
end;

class function TNodeCone.ClassNodeTypeName: string;
begin
  Result := 'Cone';
end;

class function TNodeCone.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeCone.ForVRMLVersion(const Version: TVRMLVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

procedure TNodeCone.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
begin
  inherited;
  FdtexCoord.EnumerateValid(Func);
end;

function TNodeCone.TexCoordField: TSFNode;
begin
  Result := FdTexCoord;
end;

function TNodeCone.AutoGenerate3DTexCoords: boolean;
begin
  Result := (FdTexCoord.Value = nil) or not FdTexCoord.CurrentChildAllowed;
end;

procedure TNodeCylinder.CreateNode;
begin
  inherited;

  FFdBottom := TSFBool.Create(Self, 'bottom', true);
   FdBottom.Exposed := false;
   FdBottom.ChangesAlways := [chGeometry];
  Fields.Add(FFdBottom);

  FFdHeight := TSFFloat.Create(Self, 'height', 2);
   FdHeight.Exposed := false;
   FdHeight.ChangesAlways := [chGeometry];
  Fields.Add(FFdHeight);
  { X3D specification comment: (0,Inf) }

  FFdRadius := TSFFloat.Create(Self, 'radius', 1);
   FdRadius.Exposed := false;
   FdRadius.ChangesAlways := [chGeometry];
  Fields.Add(FFdRadius);
  { X3D specification comment: (0,Inf) }

  FFdSide := TSFBool.Create(Self, 'side', true);
   FdSide.Exposed := false;
   FdSide.ChangesAlways := [chGeometry];
  Fields.Add(FFdSide);

  FFdSolid := TSFBool.Create(Self, 'solid', true);
   FdSolid.Exposed := false;
   FdSolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdSolid);

  FFdTop := TSFBool.Create(Self, 'top', true);
   FdTop.Exposed := false;
   FdTop.ChangesAlways := [chGeometry];
  Fields.Add(FFdTop);

  FFdTexCoord := TSFNode.Create(Self, 'texCoord', [TNodeTextureCoordinateGenerator, TNodeProjectedTextureCoordinate, TNodeMultiTextureCoordinate]);
   FdTexCoord.ChangesAlways := [chGeometry];
  Fields.Add(FFdTexCoord);
end;

class function TNodeCylinder.ClassNodeTypeName: string;
begin
  Result := 'Cylinder';
end;

class function TNodeCylinder.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeCylinder.ForVRMLVersion(const Version: TVRMLVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

procedure TNodeCylinder.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
begin
  inherited;
  FdtexCoord.EnumerateValid(Func);
end;

function TNodeCylinder.TexCoordField: TSFNode;
begin
  Result := FdTexCoord;
end;

function TNodeCylinder.AutoGenerate3DTexCoords: boolean;
begin
  Result := (FdTexCoord.Value = nil) or not FdTexCoord.CurrentChildAllowed;
end;

procedure TNodeElevationGrid.CreateNode;
begin
  inherited;

  FEventSet_height := TVRMLEvent.Create(Self, 'set_height', TMFFloat, true);
  Events.Add(FEventSet_height);
  Eventset_height.OnReceive.Add(@EventSet_HeightReceive);

  FFdAttrib := TMFNode.Create(Self, 'attrib', [TNodeX3DVertexAttributeNode]);
   FdAttrib.ChangesAlways := [chGeometry];
  Fields.Add(FFdAttrib);

  FFdColor := TSFNode.Create(Self, 'color', [TNodeX3DColorNode]);
   FdColor.ChangesAlways := [chGeometry];
  Fields.Add(FFdColor);

  FFdFogCoord := TSFNode.Create(Self, 'fogCoord', [TNodeFogCoordinate]);
   FdFogCoord.ChangesAlways := [chGeometry];
  Fields.Add(FFdFogCoord);

  FFdNormal := TSFNode.Create(Self, 'normal', [TNodeX3DNormalNode]);
   FdNormal.ChangesAlways := [chGeometry];
  Fields.Add(FFdNormal);

  FFdTexCoord := TSFNode.Create(Self, 'texCoord', [TNodeX3DTextureCoordinateNode]);
   FdTexCoord.ChangesAlways := [chGeometry];
  Fields.Add(FFdTexCoord);

  FFdCcw := TSFBool.Create(Self, 'ccw', true);
   FdCcw.Exposed := false;
   FdCcw.ChangesAlways := [chGeometry];
  Fields.Add(FFdCcw);

  FFdColorPerVertex := TSFBool.Create(Self, 'colorPerVertex', true);
   FdColorPerVertex.Exposed := false;
   FdColorPerVertex.ChangesAlways := [chGeometry];
  Fields.Add(FFdColorPerVertex);

  FFdCreaseAngle := TSFFloat.Create(Self, 'creaseAngle', 0);
   FdCreaseAngle.Exposed := false;
   FdCreaseAngle.ChangesAlways := [chGeometry];
  Fields.Add(FFdCreaseAngle);
  { X3D specification comment: [0,Inf) }

  FFdHeight := TMFFloat.Create(Self, 'height', []);
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
  { X3D specification comment: [0,Inf) }

  FFdXSpacing := TSFFloat.Create(Self, 'xSpacing', 1.0);
   FdXSpacing.Exposed := false;
   FdXSpacing.ChangesAlways := [chGeometry];
  Fields.Add(FFdXSpacing);
  { X3D specification comment: (0,Inf) }

  FFdZDimension := TSFInt32.Create(Self, 'zDimension', 0);
   FdZDimension.Exposed := false;
   FdZDimension.ChangesAlways := [chGeometry];
  Fields.Add(FFdZDimension);
  { X3D specification comment: [0,Inf) }

  FFdZSpacing := TSFFloat.Create(Self, 'zSpacing', 1.0);
   FdZSpacing.Exposed := false;
   FdZSpacing.ChangesAlways := [chGeometry];
  Fields.Add(FFdZSpacing);
  { X3D specification comment: (0,Inf) }
end;

class function TNodeElevationGrid.ClassNodeTypeName: string;
begin
  Result := 'ElevationGrid';
end;

class function TNodeElevationGrid.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TNodeElevationGrid.IsNotEmpty: boolean;
begin
  Result :=
    (FdXDimension.Value >= 2) and
    (FdZDimension.Value >= 2) and
    { VRML spec says that xSpacing and ySpacing shall be > 0.
      So I understand that when they are = 0 (or < 0) nothing
      should be rendered. }
    (FdXSpacing.Value > 0) and
    (FdZSpacing.Value > 0) and
    (FdHeight.Count >= FdXDimension.Value * FdZDimension.Value);
end;

function TNodeElevationGrid.Color: TMFVec3f;
begin
  if (FdColor.Value <> nil) and
     (FdColor.Value is TNodeColor) then
    Result := TNodeColor(FdColor.Value).FdColor else
    Result := nil;
end;

function TNodeElevationGrid.ColorRGBA: TMFColorRGBA;
begin
  if (FdColor.Value <> nil) and
     (FdColor.Value is TNodeColorRGBA) then
    Result := TNodeColorRGBA(FdColor.Value).FdColor else
    Result := nil;
end;

function TNodeElevationGrid.FogCoord: TMFFloat;
begin
  if (FdFogCoord.Value <> nil) and
     (FdFogCoord.Value is TNodeFogCoordinate) then
    Result := TNodeFogCoordinate(FdFogCoord.Value).FdDepth else
    Result := nil;
end;

function TNodeElevationGrid.Attrib: TMFNode;
begin
  Result := FdAttrib;
end;

procedure TNodeElevationGrid.EventSet_HeightReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  FdHeight.Assign(Value);
  FdHeight.Changed;
end;

procedure TNodeExtrusion.CreateNode;
begin
  inherited;

  FEventSet_crossSection := TVRMLEvent.Create(Self, 'set_crossSection', TMFVec2f, true);
  Events.Add(FEventSet_crossSection);
  Eventset_crossSection.OnReceive.Add(@EventSet_crossSectionReceive);

  FEventSet_orientation := TVRMLEvent.Create(Self, 'set_orientation', TMFRotation, true);
  Events.Add(FEventSet_orientation);
  Eventset_orientation.OnReceive.Add(@EventSet_orientationReceive);

  FEventSet_scale := TVRMLEvent.Create(Self, 'set_scale', TMFVec2f, true);
  Events.Add(FEventSet_scale);
  Eventset_scale.OnReceive.Add(@EventSet_scaleReceive);

  FEventSet_spine := TVRMLEvent.Create(Self, 'set_spine', TMFVec3f, true);
  Events.Add(FEventSet_spine);
  Eventset_spine.OnReceive.Add(@EventSet_spineReceive);

  FFdBeginCap := TSFBool.Create(Self, 'beginCap', true);
   FdBeginCap.Exposed := false;
   FdBeginCap.ChangesAlways := [chGeometry];
  Fields.Add(FFdBeginCap);

  FFdCcw := TSFBool.Create(Self, 'ccw', true);
   FdCcw.Exposed := false;
   FdCcw.ChangesAlways := [chGeometry];
  Fields.Add(FFdCcw);

  FFdConvex := TSFBool.Create(Self, 'convex', true);
   FdConvex.Exposed := false;
   FdConvex.ChangesAlways := [chGeometry];
  Fields.Add(FFdConvex);

  FFdCreaseAngle := TSFFloat.Create(Self, 'creaseAngle', 0);
   FdCreaseAngle.Exposed := false;
   FdCreaseAngle.ChangesAlways := [chGeometry];
  Fields.Add(FFdCreaseAngle);
  { X3D specification comment: [0,Inf) }

  FFdCrossSection := TMFVec2f.Create(Self, 'crossSection', [ Vector2Single(1, 1), Vector2Single(1, -1),  Vector2Single(-1, -1), Vector2Single(-1, 1), Vector2Single(1, 1) ]);
   FdCrossSection.Exposed := false;
   FdCrossSection.ChangesAlways := [chGeometry];
  Fields.Add(FFdCrossSection);
  { X3D specification comment: (-Inf,Inf) }

  FFdEndCap := TSFBool.Create(Self, 'endCap', true);
   FdEndCap.Exposed := false;
   FdEndCap.ChangesAlways := [chGeometry];
  Fields.Add(FFdEndCap);

  FFdOrientation := TMFRotation.Create(Self, 'orientation', [ Vector4Single(0, 0, 1, 0) ]);
   FdOrientation.Exposed := false;
   FdOrientation.ChangesAlways := [chGeometry];
  Fields.Add(FFdOrientation);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdScale := TMFVec2f.Create(Self, 'scale', Vector2Single(1, 1));
   FdScale.Exposed := false;
   FdScale.ChangesAlways := [chGeometry];
  Fields.Add(FFdScale);
  { X3D specification comment: (0,Inf) }

  FFdSolid := TSFBool.Create(Self, 'solid', true);
   FdSolid.Exposed := false;
   FdSolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdSolid);

  FFdSpine := TMFVec3f.Create(Self, 'spine', [ Vector3Single(0, 0, 0), Vector3Single(0, 1, 0) ]);
   FdSpine.Exposed := false;
   FdSpine.ChangesAlways := [chGeometry];
  Fields.Add(FFdSpine);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TNodeExtrusion.ClassNodeTypeName: string;
begin
  Result := 'Extrusion';
end;

class function TNodeExtrusion.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TNodeExtrusion.CrossSection3D(Index: integer): TVector3Single;
begin
  Result[0] := FdCrossSection.Items.List^[Index][0];
  Result[1] := 0;
  Result[2] := FdCrossSection.Items.List^[Index][1];
end;

function TNodeExtrusion.SpineClosed: boolean;
var
  SpinePoints: TDynVector3SingleArray;
begin
  SpinePoints := FdSpine.Items;
  Result := (SpinePoints.Count <> 0) and
    VectorsPerfectlyEqual(SpinePoints.Last,
                          SpinePoints.First);
end;

function TNodeExtrusion.CrossSectionClosed: boolean;
var
  CrossSectionPoints: TDynVector2SingleArray;
begin
  CrossSectionPoints := FdCrossSection.Items;
  Result := (CrossSectionPoints.Count <> 0) and
    VectorsPerfectlyEqual(CrossSectionPoints.Last,
                          CrossSectionPoints.First);
end;

function TNodeExtrusion.Proxy(var State: TVRMLGraphTraverseState;
  const OverTriangulate: boolean): TVRMLGeometryNode;

{ Some of the things that are non-optimal when you will render
  Extrusion using this Proxy instead of directly rendering node:

  - In specialized Extrusion rendering, you now that sides are
    composed of quads, and so you can use QUAD_STRIP to render them.
    This allows vertices sharing. IndexedFaceSet uses vertex arrays,
    that also provides sharing but may be slightly worse.

  - In specialized Extrusion rendering, you know that sides' quads
    are convex. There's never any need to triangulate them carefully.
    For IndexedFaceSet, when caps may be non-convex, you have to set
    convex field to false, so sides will be unnecessarily triangulated
    with the slower algorithm.

  You gain:

  - creaseAngle support, which is really the crucial point why
    I'm implementing this. I already have a specialized Extrusion
    rendering, but adding there creaseAngle support is a major pain.

    You would have to calculate all coordinates first, and then calculate
    their normal vectors using a specialized routine like CreateNormals
    that knows about Extrusion connectivity. This is painful to implement,
    and naive implementation would even suffer from a slowdown not present
    in Proxy approach: calculating all coordinates two times (before
    calculating normals, and later for actual rendering). Sure, smarter
    approach would remember these coordinates. Proxy approach provides
    this automatically, saving coordinates inside Coordinate node,
    for both normals calculation and rendering.

  - A little ease of implementation: 230 lines of Proxy approach
    versus 390 lines of TExtrusionRenderer. And remember that
    actually handling creaseAngle would complicate TExtrusionRenderer
    even more (that's why I didn't dare to do it...), and that
    Proxy approach could also be used to implement things like
    bounding box and triangulate eventually (although for now we use
    our optimized methods).
}

var
  IFS: TNodeIndexedFaceSet absolute Result;
  Index: Integer;
  E: TVRMLExtrusion;

  procedure NextIndex(SpineIndex, CrossSectionIndex: Integer;
    SpecialTex: boolean; SpecialTexIndex: Integer);
  begin
    if SpecialTex then
      IFS.FdTexCoordIndex.Items.List^[Index] := SpecialTexIndex else
      IFS.FdTexCoordIndex.Items.List^[Index] :=
        SpineIndex * FdCrossSection.Count + CrossSectionIndex;

    { For closed spine and/or crossSection, specify the same indexes.
      For coordIndex (for texCoordIndex, this is not correct, texture
      may have a seam there).
      This will allow the correct smoothing between spine/crossSection
      edges when calculating normal vectors. }

   if (SpineIndex = FdSpine.Items.Count - 1) and
       E.BeginEndCapsMatching then
      SpineIndex := 0;

    if (CrossSectionIndex = FdCrossSection.Items.Count - 1) and
       E.CrossSectionClosed then
      CrossSectionIndex := 0;

    IFS.FdCoordIndex.Items.List^[Index] :=
      SpineIndex * FdCrossSection.Count + CrossSectionIndex;

    Inc(Index);
  end;

  procedure NextIndexFaceDelimiter;
  begin
    IFS.FdCoordIndex.Items.List^[Index] := -1;
    IFS.FdTexCoordIndex.Items.List^[Index] := -1;
    Inc(Index);
  end;

  function SideTexCoord(SpineIndex, CrossSectionIndex: Integer): TVector2Single;
  begin
    { Check ranges, to avoid dividing by zero (eventually the
      corresponding coord is set to a predictable zero :) ). }
    if FdCrossSection.Count > 1 then
      Result[0] := CrossSectionIndex / (FdCrossSection.Count - 1) else
      Result[0] := 0;
    if E.High > 0 then
      Result[1] := SpineIndex / E.High else
      Result[1] := 0;
  end;

var
  CoordNode: TNodeCoordinate;
  TexCoordNode: TNodeTextureCoordinate;
  Transform: TMatrix4Single;
  I, J, Len, SidesCount: Integer;
  LastY, LastZ: TVector3Single;
  TexCapsMin, TexCapsMax: TVector2Single;
  TexCapsTranslation, TexCapsScale: Single;
begin
  IFS := TNodeIndexedFaceSet.Create(NodeName, WWWBasePath);
  try
    E := TVRMLExtrusion.Create;
    try
      E.Node := Self;

      { calculate SidesCount = number of verts on sides, often used number }
      SidesCount := (E.High + 1) * FdCrossSection.Count;

      { Fill CoordNode }

      CoordNode := TNodeCoordinate.Create('', WWWBasePath);
      IFS.FdCoord.Value := CoordNode;

      CoordNode.FdPoint.Items.Count := SidesCount;

      for I := 0 to E.High do
      begin
        E.SpineTransformTo1st(I, LastY, LastZ, Transform);
        for J := 0 to FdCrossSection.Count - 1 do
        begin
          CoordNode.FdPoint.Items.List^[I * FdCrossSection.Count + J] :=
            MatrixMultPoint(Transform, CrossSection3D(J));
        end;
      end;

      { Fill TexCoordNode }

      TexCoordNode := TNodeTextureCoordinate.Create('', WWWBasePath);
      IFS.FdTexCoord.Value := TexCoordNode;

      Len := SidesCount;
      if FdBeginCap.Value or FdEndCap.Value then
        Len += FdCrossSection.Count;
      TexCoordNode.FdPoint.Items.Count := Len;

      for I := 0 to E.High do
        for J := 0 to FdCrossSection.Count - 1 do
          TexCoordNode.FdPoint.Items.List^[I * FdCrossSection.Count + J] :=
            SideTexCoord(I, J);

      { calculate TexCapsTranslation, TexCapsScale }
      if FdBeginCap.Value or FdEndCap.Value then
      begin
        TexCapsTranslation := 0;
        TexCapsScale := 1;
        if FdCrossSection.Items.MinMax(TexCapsMin, TexCapsMax) then
        begin
          if TexCapsMax[0] - TexCapsMin[0] >=
             TexCapsMax[1] - TexCapsMin[1] then
          begin
            if not Zero(TexCapsMax[0] - TexCapsMin[0]) then
            begin
              { scale such that U is along the X of cap.
                Coord calculation will go like
                  u := (x - TexCapsMin[0]) / (TexCapsMax[0] - TexCapsMin[0]) }
              TexCapsTranslation := -TexCapsMin[0];
              TexCapsScale := 1 /  (TexCapsMax[0] - TexCapsMin[0]);
            end;
          end else
          begin
            if not Zero(TexCapsMax[1] - TexCapsMin[1]) then
            begin
              TexCapsTranslation := -TexCapsMin[1];
              TexCapsScale := 1 /  (TexCapsMax[1] - TexCapsMin[1]);
            end;
          end;
        end;

        for I := 0 to FdCrossSection.Count - 1 do
        begin
          TexCoordNode.FdPoint.Items.List^[
            SidesCount + I] :=
            VectorScale(VectorAdd(
              FdCrossSection.Items.List^[I],
              Vector2Single(TexCapsTranslation, TexCapsTranslation)),
              TexCapsScale);
        end;
      end;

      { Fill CoordIndex and TexCoordIndex }

      Len := 5 * E.High * (FdCrossSection.Count - 1);
      if FdBeginCap.Value then
        Len += FdCrossSection.Count + 1;
      if FdEndCap  .Value then
        Len += FdCrossSection.Count + 1;
      IFS.FdCoordIndex.Items.Count := Len;
      IFS.FdTexCoordIndex.Items.Count := Len;

      Index := 0;

      for I := 1 to E.High do
      begin
        for J := 1 to FdCrossSection.Count - 1 do
        begin
          { The VRML / X3D specifications say that ordering of sides
            should be
              spine[0](crossSection[0], crossSection[1])
              spine[1](crossSection[1], crossSection[0])
            This is important, because we want faces to be ordered just
            like the specification requires. This makes using "ccw"
            and "solid" fields predictable for VRML author, since he
            knows which side will be CCW and which CW.

            So below we produce the same order. }

          NextIndex(I - 1, J - 1, false, 0);
          NextIndex(I - 1, J    , false, 0);
          NextIndex(I    , J    , false, 0);
          NextIndex(I    , J - 1, false, 0);
          NextIndexFaceDelimiter;
        end;
      end;

      if FdBeginCap.Value then
      begin
        { "downto" order, to match the ordering of sides, this makes
          things such a "solid TRUE" work Ok. }
        for J := FdCrossSection.Count - 1 downto 0 do
          NextIndex(0, J, true, SidesCount + J);
        NextIndexFaceDelimiter;
      end;

      if FdEndCap.Value then
      begin
        for J := 0 to FdCrossSection.Count - 1 do
          NextIndex(E.High, J, true, SidesCount + J);
        NextIndexFaceDelimiter;
      end;

      { Check that we actually filled all indexes we planned }
      Assert(Index = IFS.FdCoordIndex.Items.Count);

    finally FreeAndNil(E) end;

    IFS.FdSolid.Value := FdSolid.Value;
    IFS.FdCcw.Value := FdCcw.Value;
    IFS.FdCreaseAngle.Value := FdCreaseAngle.Value;
    IFS.FdConvex.Value := FdConvex.Value;
  except FreeAndNil(Result); raise end;
end;

function TNodeExtrusion.ProxyUsesOverTriangulate: boolean;
begin
  Result := false;
end;

procedure TNodeExtrusion.EventSet_crossSectionReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  FdcrossSection.Assign(Value);
  FdcrossSection.Changed;
end;

procedure TNodeExtrusion.EventSet_orientationReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  Fdorientation.Assign(Value);
  Fdorientation.Changed;
end;

procedure TNodeExtrusion.EventSet_scaleReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  Fdscale.Assign(Value);
  Fdscale.Changed;
end;

procedure TNodeExtrusion.EventSet_spineReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  Fdspine.Assign(Value);
  Fdspine.Changed;
end;

function TNodeExtrusion.Convex: boolean;
begin
  Result := FdConvex.Value;
end;

procedure TNodeIndexedFaceSet.CreateNode;
begin
  inherited;

  FEventSet_colorIndex := TVRMLEvent.Create(Self, 'set_colorIndex', TMFInt32, true);
  Events.Add(FEventSet_colorIndex);
  Eventset_colorIndex.OnReceive.Add(@EventSet_colorIndexReceive);

  FEventSet_coordIndex := TVRMLEvent.Create(Self, 'set_coordIndex', TMFInt32, true);
  Events.Add(FEventSet_coordIndex);
  Eventset_coordIndex.OnReceive.Add(@EventSet_coordIndexReceive);

  FEventSet_normalIndex := TVRMLEvent.Create(Self, 'set_normalIndex', TMFInt32, true);
  Events.Add(FEventSet_normalIndex);
  Eventset_normalIndex.OnReceive.Add(@EventSet_normalIndexReceive);

  FEventSet_texCoordIndex := TVRMLEvent.Create(Self, 'set_texCoordIndex', TMFInt32, true);
  Events.Add(FEventSet_texCoordIndex);
  Eventset_texCoordIndex.OnReceive.Add(@EventSet_texCoordIndexReceive);

  FFdColorIndex := TMFInt32.Create(Self, 'colorIndex', []);
   FdColorIndex.SaveToStreamLineUptoNegative := true;
   FdColorIndex.Exposed := false;
   FdColorIndex.ChangesAlways := [chGeometry];
  Fields.Add(FFdColorIndex);
  { X3D specification comment: [0,Inf) or -1 }

  FFdConvex := TSFBool.Create(Self, 'convex', true);
   FdConvex.Exposed := false;
   FdConvex.ChangesAlways := [chGeometry];
  Fields.Add(FFdConvex);

  FFdCoordIndex := TMFInt32.Create(Self, 'coordIndex', []);
   FdCoordIndex.SaveToStreamLineUptoNegative := true;
   FdCoordIndex.Exposed := false;
   FdCoordIndex.ChangesAlways := [chGeometry];
  Fields.Add(FFdCoordIndex);
  { X3D specification comment: [0,Inf) or -1 }

  FFdCreaseAngle := TSFFloat.Create(Self, 'creaseAngle', 0);
   FdCreaseAngle.Exposed := false;
   FdCreaseAngle.ChangesAlways := [chGeometry];
  Fields.Add(FFdCreaseAngle);
  { X3D specification comment: [0,Inf) }

  FFdNormalIndex := TMFInt32.Create(Self, 'normalIndex', []);
   FdNormalIndex.SaveToStreamLineUptoNegative := true;
   FdNormalIndex.Exposed := false;
   FdNormalIndex.ChangesAlways := [chGeometry];
  Fields.Add(FFdNormalIndex);
  { X3D specification comment: [0,Inf) or -1 }

  FFdTexCoordIndex := TMFInt32.Create(Self, 'texCoordIndex', []);
   FdTexCoordIndex.SaveToStreamLineUptoNegative := true;
   FdTexCoordIndex.Exposed := false;
   FdTexCoordIndex.ChangesAlways := [chGeometry];
  Fields.Add(FFdTexCoordIndex);
  { X3D specification comment: [-1,Inf) }
end;

class function TNodeIndexedFaceSet.ClassNodeTypeName: string;
begin
  Result := 'IndexedFaceSet';
end;

class function TNodeIndexedFaceSet.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeIndexedFaceSet.ForVRMLVersion(const Version: TVRMLVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

function TNodeIndexedFaceSet.CoordIndex: TMFLong;
begin
  Result := FdCoordIndex;
end;

procedure TNodeIndexedFaceSet.EventSet_colorIndexReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  FdcolorIndex.Assign(Value);
  FdcolorIndex.Changed;
end;

procedure TNodeIndexedFaceSet.EventSet_coordIndexReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  FdcoordIndex.Assign(Value);
  FdcoordIndex.Changed;
end;

procedure TNodeIndexedFaceSet.EventSet_normalIndexReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  FdnormalIndex.Assign(Value);
  FdnormalIndex.Changed;
end;

procedure TNodeIndexedFaceSet.EventSet_texCoordIndexReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  FdtexCoordIndex.Assign(Value);
  FdtexCoordIndex.Changed;
end;

function TNodeIndexedFaceSet.Convex: boolean;
begin
  Result := FdConvex.Value;
end;

procedure TNodeSphere.CreateNode;
begin
  inherited;

  FFdRadius := TSFFloat.Create(Self, 'radius', 1);
   FdRadius.Exposed := false;
   FdRadius.ChangesAlways := [chGeometry];
  Fields.Add(FFdRadius);
  { X3D specification comment: (0,Inf) }

  FFdSolid := TSFBool.Create(Self, 'solid', true);
   FdSolid.Exposed := false;
   FdSolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdSolid);

  FFdTexCoord := TSFNode.Create(Self, 'texCoord', [TNodeTextureCoordinateGenerator, TNodeProjectedTextureCoordinate, TNodeMultiTextureCoordinate]);
   FdTexCoord.ChangesAlways := [chGeometry];
  Fields.Add(FFdTexCoord);
end;

class function TNodeSphere.ClassNodeTypeName: string;
begin
  Result := 'Sphere';
end;

class function TNodeSphere.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeSphere.ForVRMLVersion(const Version: TVRMLVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

function TNodeSphere.TexCoordField: TSFNode;
begin
  Result := FdTexCoord;
end;

function TNodeSphere.AutoGenerate3DTexCoords: boolean;
begin
  Result := (FdTexCoord.Value = nil) or not FdTexCoord.CurrentChildAllowed;
end;

procedure RegisterGeometry3DNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeBox,
    TNodeCone,
    TNodeCylinder,
    TNodeElevationGrid,
    TNodeExtrusion,
    TNodeIndexedFaceSet,
    TNodeSphere
  ]);
end;

{$endif read_implementation}
