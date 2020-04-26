{
  Copyright 2002-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { Rectangular box. }
  TBoxNode = class(TAbstractGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdSize: TSFVec3f;
    public property FdSize: TSFVec3f read FFdSize;

    strict private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    strict private FFdCcw: TSFBool;
    public property FdCcw: TSFBool read FFdCcw;

    strict private FFdTexCoord: TSFNode;
    public property FdTexCoord: TSFNode read FFdTexCoord;
    function TexCoordField: TSFNode; override;
    function SolidField: TSFBool; override;

    strict private FFdDivisions: TSFInt32;
    public property FdDivisions: TSFInt32 read FFdDivisions;
    function CalculateDivisions: Cardinal;

    function Proxy(var State: TX3DGraphTraverseState;
      const OverTriangulate: boolean): TAbstractGeometryNode; override;
    function BoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function AutoGenerate3DTexCoords: boolean; override;

    {$I auto_generated_node_helpers/x3dnodes_box.inc}
  end;

  { Cone. }
  TConeNode = class(TAbstractGeometryNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdBottom: TSFBool;
    public property FdBottom: TSFBool read FFdBottom;

    strict private FFdBottomRadius: TSFFloat;
    public property FdBottomRadius: TSFFloat read FFdBottomRadius;

    strict private FFdHeight: TSFFloat;
    public property FdHeight: TSFFloat read FFdHeight;

    strict private FFdSide: TSFBool;
    public property FdSide: TSFBool read FFdSide;

    strict private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    strict private FFdTexCoord: TSFNode;
    public property FdTexCoord: TSFNode read FFdTexCoord;
    function TexCoordField: TSFNode; override;
    function SolidField: TSFBool; override;

    strict private FFdSlices: TSFInt32;
    public property FdSlices: TSFInt32 read FFdSlices;
    function CalculateSlices: Cardinal;

    strict private FFdStacks: TSFInt32;
    public property FdStacks: TSFInt32 read FFdStacks;
    function CalculateStacks: Cardinal;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;
    function Proxy(var State: TX3DGraphTraverseState;
      const OverTriangulate: boolean): TAbstractGeometryNode; override;
    function BoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function AutoGenerate3DTexCoords: boolean; override;

    {$I auto_generated_node_helpers/x3dnodes_cone.inc}
  end;
  TConeNode_2 = TConeNode;

  { Cylinder. }
  TCylinderNode = class(TAbstractGeometryNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdBottom: TSFBool;
    public property FdBottom: TSFBool read FFdBottom;

    strict private FFdHeight: TSFFloat;
    public property FdHeight: TSFFloat read FFdHeight;

    strict private FFdRadius: TSFFloat;
    public property FdRadius: TSFFloat read FFdRadius;

    strict private FFdSide: TSFBool;
    public property FdSide: TSFBool read FFdSide;

    strict private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    strict private FFdTop: TSFBool;
    public property FdTop: TSFBool read FFdTop;

    strict private FFdTexCoord: TSFNode;
    public property FdTexCoord: TSFNode read FFdTexCoord;
    function TexCoordField: TSFNode; override;
    function SolidField: TSFBool; override;

    strict private FFdSlices: TSFInt32;
    public property FdSlices: TSFInt32 read FFdSlices;
    function CalculateSlices: Cardinal;

    strict private FFdStacks: TSFInt32;
    public property FdStacks: TSFInt32 read FFdStacks;
    function CalculateStacks: Cardinal;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;
    function Proxy(var State: TX3DGraphTraverseState;
      const OverTriangulate: boolean): TAbstractGeometryNode; override;
    function BoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function AutoGenerate3DTexCoords: boolean; override;

    {$I auto_generated_node_helpers/x3dnodes_cylinder.inc}
  end;
  TCylinderNode_2 = TCylinderNode;

  { Uniform rectangular grid of varying height above the Y=0 plane,
    aka "height map". }
  TElevationGridNode = class(TAbstractGeometryNode)
  strict private
    procedure EventSet_HeightReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    { Event in } { }
    strict private FEventSet_height: TMFFloatEvent;
    public property EventSet_height: TMFFloatEvent read FEventSet_height;

    strict private FFdAttrib: TMFNode;
    public property FdAttrib: TMFNode read FFdAttrib;

    strict private FFdColor: TSFNode;
    public property FdColor: TSFNode read FFdColor;

    strict private FFdFogCoord: TSFNode;
    public property FdFogCoord: TSFNode read FFdFogCoord;

    strict private FFdNormal: TSFNode;
    public property FdNormal: TSFNode read FFdNormal;

    strict private FFdTexCoord: TSFNode;
    public property FdTexCoord: TSFNode read FFdTexCoord;

    strict private FFdCcw: TSFBool;
    public property FdCcw: TSFBool read FFdCcw;

    strict private FFdColorPerVertex: TSFBool;
    public property FdColorPerVertex: TSFBool read FFdColorPerVertex;

    strict private FFdCreaseAngle: TSFFloat;
    public property FdCreaseAngle: TSFFloat read FFdCreaseAngle;

    strict private FFdHeight: TMFFloat;
    public property FdHeight: TMFFloat read FFdHeight;

    strict private FFdNormalPerVertex: TSFBool;
    public property FdNormalPerVertex: TSFBool read FFdNormalPerVertex;

    strict private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    strict private FFdXDimension: TSFInt32;
    public property FdXDimension: TSFInt32 read FFdXDimension;

    strict private FFdXSpacing: TSFFloat;
    public property FdXSpacing: TSFFloat read FFdXSpacing;

    strict private FFdZDimension: TSFInt32;
    public property FdZDimension: TSFInt32 read FFdZDimension;

    strict private FFdZSpacing: TSFFloat;
    public property FdZSpacing: TSFFloat read FFdZSpacing;

    { This checks whether xDimension and zDimension are >= 2,
      xSpacing and zSpacing are > 0 and height has at least the
      required number of values. If this returns @false then
      it is understood that ElevationGrid is not rendered, doesn't
      have any vertices/triangles etc. }
    function IsNotEmpty: boolean;

    function ColorField: TSFNode; override;
    function InternalFogCoord: TMFFloat; override;
    function AttribField: TMFNode; override;
    function SolidField: TSFBool; override;
    function FieldSetByEvent(const Event: TX3DEvent): TX3DField; override;

    function Proxy(var State: TX3DGraphTraverseState;
      const OverTriangulate: boolean): TAbstractGeometryNode; override;
    function ProxyUsesOverTriangulate: boolean; override;

    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    {$I auto_generated_node_helpers/x3dnodes_elevationgrid.inc}
  end;

  { 2D cross-section shape extruded along a 3D spine. }
  TExtrusionNode = class(TAbstractGeometryNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  strict private
    procedure Eventset_crossSectionReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
    procedure Eventset_orientationReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
    procedure Eventset_scaleReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
    procedure Eventset_spineReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    { Event in } { }
    strict private FEventSet_crossSection: TMFVec2fEvent;
    public property EventSet_crossSection: TMFVec2fEvent read FEventSet_crossSection;

    { Event in } { }
    strict private FEventSet_orientation: TMFRotationEvent;
    public property EventSet_orientation: TMFRotationEvent read FEventSet_orientation;

    { Event in } { }
    strict private FEventSet_scale: TMFVec2fEvent;
    public property EventSet_scale: TMFVec2fEvent read FEventSet_scale;

    { Event in } { }
    strict private FEventSet_spine: TMFVec3fEvent;
    public property EventSet_spine: TMFVec3fEvent read FEventSet_spine;

    strict private FFdBeginCap: TSFBool;
    public property FdBeginCap: TSFBool read FFdBeginCap;

    strict private FFdCcw: TSFBool;
    public property FdCcw: TSFBool read FFdCcw;

    strict private FFdConvex: TSFBool;
    public property FdConvex: TSFBool read FFdConvex;

    strict private FFdCreaseAngle: TSFFloat;
    public property FdCreaseAngle: TSFFloat read FFdCreaseAngle;

    strict private FFdCrossSection: TMFVec2f;
    public property FdCrossSection: TMFVec2f read FFdCrossSection;

    strict private FFdEndCap: TSFBool;
    public property FdEndCap: TSFBool read FFdEndCap;

    strict private FFdOrientation: TMFRotation;
    public property FdOrientation: TMFRotation read FFdOrientation;

    strict private FFdScale: TMFVec2f;
    public property FdScale: TMFVec2f read FFdScale;

    strict private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    strict private FFdSpine: TMFVec3f;
    public property FdSpine: TMFVec3f read FFdSpine;

    strict private FFdTexCoord: TSFNode;
    public property FdTexCoord: TSFNode read FFdTexCoord;
    function TexCoordField: TSFNode; override;
    function SolidField: TSFBool; override;
    function ConvexField: TSFBool; override;

    { For given Index, return appropriate FdCrossIndex item as 3D vertex.
      That is, uses FdCrossIndex values as X, Z of 3D vertex and sets Y = 0
      (that's how Extrusion is defined in VRML / X3D). }
    function CrossSection3D(Index: integer): TVector3;

    { Check is spine closed. This happens when "spine" field is non-empty
      and it's first and last points are equal. }
    function SpineClosed: boolean;

    { Check is crossSection closed. This happens when "crossSection"
      field is non-empty and it's first and last points are equal. }
    function CrossSectionClosed: boolean;

    function Proxy(var State: TX3DGraphTraverseState;
      const OverTriangulate: boolean): TAbstractGeometryNode; override;
    function ProxyUsesOverTriangulate: boolean; override;

    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function BoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function AutoGenerate3DTexCoords: boolean; override;

    {$I auto_generated_node_helpers/x3dnodes_extrusion.inc}
  end;

  { Mesh with faces (polygons) constructed from vertices.
    This is probably the most universal, and most often used,
    geometry node in X3D. }
  TIndexedFaceSetNode = class(TAbstractComposedGeometryNode)
  strict private
    procedure Eventset_colorIndexReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
    procedure Eventset_coordIndexReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
    procedure Eventset_normalIndexReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
    procedure Eventset_texCoordIndexReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    {$ifndef CASTLE_SLIM_NODES}
    { Event in } { }
    strict private FEventSet_colorIndex: TMFInt32Event;
    public property EventSet_colorIndex: TMFInt32Event read FEventSet_colorIndex;

    { Event in } { }
    strict private FEventSet_coordIndex: TMFInt32Event;
    public property EventSet_coordIndex: TMFInt32Event read FEventSet_coordIndex;

    { Event in } { }
    strict private FEventSet_normalIndex: TMFInt32Event;
    public property EventSet_normalIndex: TMFInt32Event read FEventSet_normalIndex;

    { Event in } { }
    strict private FEventSet_texCoordIndex: TMFInt32Event;
    public property EventSet_texCoordIndex: TMFInt32Event read FEventSet_texCoordIndex;
    {$endif}

    strict private FFdColorIndex: TMFInt32;
    public property FdColorIndex: TMFInt32 read FFdColorIndex;

    strict private FFdConvex: TSFBool;
    public property FdConvex: TSFBool read FFdConvex;
    function ConvexField: TSFBool; override;

    strict private FFdCoordIndex: TMFInt32;
    public property FdCoordIndex: TMFInt32 read FFdCoordIndex;

    strict private FFdCreaseAngle: TSFFloat;
    public property FdCreaseAngle: TSFFloat read FFdCreaseAngle;

    strict private FFdNormalIndex: TMFInt32;
    public property FdNormalIndex: TMFInt32 read FFdNormalIndex;

    strict private FFdTexCoordIndex: TMFInt32;
    public property FdTexCoordIndex: TMFInt32 read FFdTexCoordIndex;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;

    function CoordIndexField: TMFLong; override;

    procedure InternalCoordPolygons(
      State: TX3DGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;

    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    {$I auto_generated_node_helpers/x3dnodes_indexedfaceset.inc}
  end;
  TIndexedFaceSetNode_2 = TIndexedFaceSetNode;

  { Sphere. }
  TSphereNode = class(TAbstractGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdRadius: TSFFloat;
    public property FdRadius: TSFFloat read FFdRadius;

    strict private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    strict private FFdTexCoord: TSFNode;
    public property FdTexCoord: TSFNode read FFdTexCoord;
    function TexCoordField: TSFNode; override;
    function SolidField: TSFBool; override;

    strict private FFdSlices: TSFInt32;
    public property FdSlices: TSFInt32 read FFdSlices;
    function CalculateSlices: Cardinal;

    strict private FFdStacks: TSFInt32;
    public property FdStacks: TSFInt32 read FFdStacks;
    function CalculateStacks: Cardinal;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;
    function Proxy(var State: TX3DGraphTraverseState;
      const OverTriangulate: boolean): TAbstractGeometryNode; override;

    function BoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function AutoGenerate3DTexCoords: boolean; override;

    {$I auto_generated_node_helpers/x3dnodes_sphere.inc}
  end;
  TSphereNode_2 = TSphereNode;

{$endif read_interface}

{$ifdef read_implementation}
procedure TBoxNode.CreateNode;
begin
  inherited;

  FFdSize := TSFVec3f.Create(Self, false, 'size', Vector3(2, 2, 2));
   FdSize.ChangeAlways := chGeometry;
  AddField(FFdSize);
  { X3D specification comment: (0,Inf) }

  FFdSolid := TSFBool.Create(Self, false, 'solid', true);
   FdSolid.ChangeAlways := chGeometry;
  AddField(FFdSolid);

  FFdCcw := TSFBool.Create(Self, false, 'ccw', true);
   FdCcw.ChangeAlways := chGeometry;
  AddField(FFdCcw);

  FFdTexCoord := TSFNode.Create(Self, true, 'texCoord', [TTextureCoordinateGeneratorNode, TProjectedTextureCoordinateNode, TMultiTextureCoordinateNode]);
   FdTexCoord.ChangeAlways := chGeometry;
  AddField(FFdTexCoord);

  FFdDivisions := TSFInt32.Create(Self, true, 'divisions', -1);
   FdDivisions.ChangeAlways := chGeometry;
  AddField(FFdDivisions);
end;

class function TBoxNode.ClassX3DType: string;
begin
  Result := 'Box';
end;

function TBoxNode.TexCoordField: TSFNode;
begin
  Result := FdTexCoord;
end;

function TBoxNode.AutoGenerate3DTexCoords: boolean;
begin
  Result := (FdTexCoord.Value = nil) or not FdTexCoord.CurrentChildAllowed;
end;

function TBoxNode.SolidField: TSFBool;
begin
  Result := FdSolid;
end;

function TBoxNode.CalculateDivisions: Cardinal;
begin
  // use default in case of -1 or invalid value
  if FdDivisions.Value < MinTriangulationDivisions then
    Result := DefaultTriangulationDivisions
  else
    Result := FdDivisions.Value;
end;

{ TConeNode ------------------------------------------------------------------ }

procedure TConeNode.CreateNode;
begin
  inherited;

  FFdBottom := TSFBool.Create(Self, false, 'bottom', true);
   FdBottom.ChangeAlways := chGeometry;
  AddField(FFdBottom);

  FFdBottomRadius := TSFFloat.Create(Self, false, 'bottomRadius', 1);
   FdBottomRadius.ChangeAlways := chGeometry;
  AddField(FFdBottomRadius);
  { X3D specification comment: (0,Inf) }

  FFdHeight := TSFFloat.Create(Self, false, 'height', 2);
   FdHeight.ChangeAlways := chGeometry;
  AddField(FFdHeight);
  { X3D specification comment: (0,Inf) }

  FFdSide := TSFBool.Create(Self, false, 'side', true);
   FdSide.ChangeAlways := chGeometry;
  AddField(FFdSide);

  FFdSolid := TSFBool.Create(Self, false, 'solid', true);
   FdSolid.ChangeAlways := chGeometry;
  AddField(FFdSolid);

  FFdTexCoord := TSFNode.Create(Self, true, 'texCoord', [TTextureCoordinateGeneratorNode, TProjectedTextureCoordinateNode, TMultiTextureCoordinateNode]);
   FdTexCoord.ChangeAlways := chGeometry;
  AddField(FFdTexCoord);

  FFdSlices := TSFInt32.Create(Self, true, 'slices', -1);
   FdSlices.ChangeAlways := chGeometry;
  AddField(FFdSlices);

  FFdStacks := TSFInt32.Create(Self, true, 'stacks', -1);
   FdStacks.ChangeAlways := chGeometry;
  AddField(FFdStacks);
end;

class function TConeNode.ClassX3DType: string;
begin
  Result := 'Cone';
end;

class function TConeNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

function TConeNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := inherited;
  if Result <> nil then Exit;

  Result := FdtexCoord.Enumerate(Func);
  if Result <> nil then Exit;
end;

function TConeNode.TexCoordField: TSFNode;
begin
  Result := FdTexCoord;
end;

function TConeNode.AutoGenerate3DTexCoords: boolean;
begin
  Result := (FdTexCoord.Value = nil) or not FdTexCoord.CurrentChildAllowed;
end;

function TConeNode.SolidField: TSFBool;
begin
  Result := FdSolid;
end;

function TConeNode.CalculateSlices: Cardinal;
begin
  // use default in case of -1 or invalid value
  if FdSlices.Value < MinTriangulationSlices then
    Result := DefaultTriangulationSlices
  else
    Result := FdSlices.Value;
end;

function TConeNode.CalculateStacks: Cardinal;
begin
  // use default in case of -1 or invalid value
  if FdStacks.Value < MinTriangulationStacks then
    Result := DefaultTriangulationStacks
  else
    Result := FdStacks.Value;
end;

{ TCylinderNode -------------------------------------------------------------- }

procedure TCylinderNode.CreateNode;
begin
  inherited;

  FFdBottom := TSFBool.Create(Self, false, 'bottom', true);
   FdBottom.ChangeAlways := chGeometry;
  AddField(FFdBottom);

  FFdHeight := TSFFloat.Create(Self, false, 'height', 2);
   FdHeight.ChangeAlways := chGeometry;
  AddField(FFdHeight);
  { X3D specification comment: (0,Inf) }

  FFdRadius := TSFFloat.Create(Self, false, 'radius', 1);
   FdRadius.ChangeAlways := chGeometry;
  AddField(FFdRadius);
  { X3D specification comment: (0,Inf) }

  FFdSide := TSFBool.Create(Self, false, 'side', true);
   FdSide.ChangeAlways := chGeometry;
  AddField(FFdSide);

  FFdSolid := TSFBool.Create(Self, false, 'solid', true);
   FdSolid.ChangeAlways := chGeometry;
  AddField(FFdSolid);

  FFdTop := TSFBool.Create(Self, false, 'top', true);
   FdTop.ChangeAlways := chGeometry;
  AddField(FFdTop);

  FFdTexCoord := TSFNode.Create(Self, true, 'texCoord', [TTextureCoordinateGeneratorNode, TProjectedTextureCoordinateNode, TMultiTextureCoordinateNode]);
   FdTexCoord.ChangeAlways := chGeometry;
  AddField(FFdTexCoord);

  FFdSlices := TSFInt32.Create(Self, true, 'slices', -1);
   FdSlices.ChangeAlways := chGeometry;
  AddField(FFdSlices);

  FFdStacks := TSFInt32.Create(Self, true, 'stacks', -1);
   FdStacks.ChangeAlways := chGeometry;
  AddField(FFdStacks);
end;

class function TCylinderNode.ClassX3DType: string;
begin
  Result := 'Cylinder';
end;

class function TCylinderNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

function TCylinderNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := inherited;
  if Result <> nil then Exit;

  Result := FdtexCoord.Enumerate(Func);
  if Result <> nil then Exit;
end;

function TCylinderNode.TexCoordField: TSFNode;
begin
  Result := FdTexCoord;
end;

function TCylinderNode.AutoGenerate3DTexCoords: boolean;
begin
  Result := (FdTexCoord.Value = nil) or not FdTexCoord.CurrentChildAllowed;
end;

function TCylinderNode.SolidField: TSFBool;
begin
  Result := FdSolid;
end;

function TCylinderNode.CalculateSlices: Cardinal;
begin
  // use default in case of -1 or invalid value
  if FdSlices.Value < MinTriangulationSlices then
    Result := DefaultTriangulationSlices
  else
    Result := FdSlices.Value;
end;

function TCylinderNode.CalculateStacks: Cardinal;
begin
  // use default in case of -1 or invalid value
  if FdStacks.Value < MinTriangulationStacks then
    Result := DefaultTriangulationStacks
  else
    Result := FdStacks.Value;
end;

{ TElevationGridNode --------------------------------------------------------- }

procedure TElevationGridNode.CreateNode;
begin
  inherited;

  FEventSet_height := TMFFloatEvent.Create(Self, 'set_height', true);
  AddEvent(FEventSet_height);
  Eventset_height.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventSet_HeightReceive);

  FFdAttrib := TMFNode.Create(Self, true, 'attrib', [TAbstractVertexAttributeNode]);
   FdAttrib.ChangeAlways := chGeometry;
  AddField(FFdAttrib);

  FFdColor := TSFNode.Create(Self, true, 'color', [TAbstractColorNode]);
   FdColor.ChangeAlways := chGeometry;
  AddField(FFdColor);

  FFdFogCoord := TSFNode.Create(Self, true, 'fogCoord', [TFogCoordinateNode]);
   FdFogCoord.ChangeAlways := chGeometry;
  AddField(FFdFogCoord);

  FFdNormal := TSFNode.Create(Self, true, 'normal', [TAbstractNormalNode]);
   FdNormal.ChangeAlways := chGeometry;
  AddField(FFdNormal);

  FFdTexCoord := TSFNode.Create(Self, true, 'texCoord', [TAbstractTextureCoordinateNode]);
   FdTexCoord.ChangeAlways := chGeometry;
  AddField(FFdTexCoord);

  FFdCcw := TSFBool.Create(Self, false, 'ccw', true);
   FdCcw.ChangeAlways := chGeometry;
  AddField(FFdCcw);

  FFdColorPerVertex := TSFBool.Create(Self, false, 'colorPerVertex', true);
   FdColorPerVertex.ChangeAlways := chGeometry;
  AddField(FFdColorPerVertex);

  FFdCreaseAngle := TSFFloat.Create(Self, false, 'creaseAngle', 0);
   FdCreaseAngle.Angle := true;
   FdCreaseAngle.ChangeAlways := chGeometry;
  AddField(FFdCreaseAngle);
  { X3D specification comment: [0,Inf) }

  FFdHeight := TMFFloat.Create(Self, false, 'height', []);
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
  { X3D specification comment: [0,Inf) }

  FFdXSpacing := TSFFloat.Create(Self, false, 'xSpacing', 1.0);
   FdXSpacing.ChangeAlways := chGeometry;
  AddField(FFdXSpacing);
  { X3D specification comment: (0,Inf) }

  FFdZDimension := TSFInt32.Create(Self, false, 'zDimension', 0);
   FdZDimension.ChangeAlways := chGeometry;
  AddField(FFdZDimension);
  { X3D specification comment: [0,Inf) }

  FFdZSpacing := TSFFloat.Create(Self, false, 'zSpacing', 1.0);
   FdZSpacing.ChangeAlways := chGeometry;
  AddField(FFdZSpacing);
  { X3D specification comment: (0,Inf) }
end;

class function TElevationGridNode.ClassX3DType: string;
begin
  Result := 'ElevationGrid';
end;

function TElevationGridNode.IsNotEmpty: boolean;
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

function TElevationGridNode.ColorField: TSFNode;
begin
  Result := FdColor;
end;

function TElevationGridNode.InternalFogCoord: TMFFloat;
begin
  if (FdFogCoord.Value <> nil) and
     (FdFogCoord.Value is TFogCoordinateNode) then
    Result := TFogCoordinateNode(FdFogCoord.Value).FdDepth else
    Result := nil;
end;

function TElevationGridNode.AttribField: TMFNode;
begin
  Result := FdAttrib;
end;

procedure TElevationGridNode.EventSet_HeightReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
begin
  FdHeight.Send(Value);
end;

function TElevationGridNode.SolidField: TSFBool;
begin
  Result := FdSolid;
end;

function TElevationGridNode.FieldSetByEvent(const Event: TX3DEvent): TX3DField;
begin
  { Thanks to this method, animations that modify ElevationGrid
    by sending set_height event are correctly accounted
    in TCastleSceneCore "affected fields", thus e.g. ResetAnimationState
    correctly resets them. }
  if Event = EventSet_height then
    Exit(FdHeight);
  Result := inherited;
end;

{ TExtrusionNode ------------------------------------------------------------- }

procedure TExtrusionNode.CreateNode;
begin
  inherited;

  FEventSet_crossSection := TMFVec2fEvent.Create(Self, 'set_crossSection', true);
  AddEvent(FEventSet_crossSection);
  Eventset_crossSection.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventSet_crossSectionReceive);

  FEventSet_orientation := TMFRotationEvent.Create(Self, 'set_orientation', true);
  AddEvent(FEventSet_orientation);
  Eventset_orientation.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventSet_orientationReceive);

  FEventSet_scale := TMFVec2fEvent.Create(Self, 'set_scale', true);
  AddEvent(FEventSet_scale);
  Eventset_scale.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventSet_scaleReceive);

  FEventSet_spine := TMFVec3fEvent.Create(Self, 'set_spine', true);
  AddEvent(FEventSet_spine);
  Eventset_spine.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventSet_spineReceive);

  FFdBeginCap := TSFBool.Create(Self, false, 'beginCap', true);
   FdBeginCap.ChangeAlways := chGeometry;
  AddField(FFdBeginCap);

  FFdCcw := TSFBool.Create(Self, false, 'ccw', true);
   FdCcw.ChangeAlways := chGeometry;
  AddField(FFdCcw);

  FFdConvex := TSFBool.Create(Self, false, 'convex', true);
   FdConvex.ChangeAlways := chGeometry;
  AddField(FFdConvex);

  FFdCreaseAngle := TSFFloat.Create(Self, false, 'creaseAngle', 0);
   FdCreaseAngle.Angle := true;
   FdCreaseAngle.ChangeAlways := chGeometry;
  AddField(FFdCreaseAngle);
  { X3D specification comment: [0,Inf) }

  FFdCrossSection := TMFVec2f.Create(Self, false, 'crossSection', [ Vector2(1, 1), Vector2(1, -1),  Vector2(-1, -1), Vector2(-1, 1), Vector2(1, 1) ]);
   FdCrossSection.ChangeAlways := chGeometry;
  AddField(FFdCrossSection);
  { X3D specification comment: (-Inf,Inf) }

  FFdEndCap := TSFBool.Create(Self, false, 'endCap', true);
   FdEndCap.ChangeAlways := chGeometry;
  AddField(FFdEndCap);

  FFdOrientation := TMFRotation.Create(Self, false, 'orientation', [ Vector4(0, 0, 1, 0) ]);
   FdOrientation.ChangeAlways := chGeometry;
  AddField(FFdOrientation);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdScale := TMFVec2f.Create(Self, false, 'scale', Vector2(1, 1));
   FdScale.ChangeAlways := chGeometry;
  AddField(FFdScale);
  { X3D specification comment: (0,Inf) }

  FFdSolid := TSFBool.Create(Self, false, 'solid', true);
   FdSolid.ChangeAlways := chGeometry;
  AddField(FFdSolid);

  FFdSpine := TMFVec3f.Create(Self, false, 'spine', [ Vector3(0, 0, 0), Vector3(0, 1, 0) ]);
   FdSpine.ChangeAlways := chGeometry;
  AddField(FFdSpine);
  { X3D specification comment: (-Inf,Inf) }

  FFdTexCoord := TSFNode.Create(Self, true, 'texCoord', [TTextureCoordinateGeneratorNode, TProjectedTextureCoordinateNode, TMultiTextureCoordinateNode]);
   FdTexCoord.ChangeAlways := chGeometry;
  AddField(FFdTexCoord);
end;

class function TExtrusionNode.ClassX3DType: string;
begin
  Result := 'Extrusion';
end;

function TExtrusionNode.CrossSection3D(Index: integer): TVector3;
begin
  Result[0] := FdCrossSection.Items.List^[Index][0];
  Result[1] := 0;
  Result[2] := FdCrossSection.Items.List^[Index][1];
end;

function TExtrusionNode.SpineClosed: boolean;
var
  SpinePoints: TVector3List;
begin
  SpinePoints := FdSpine.Items;
  Result := (SpinePoints.Count <> 0) and
    TVector3.PerfectlyEquals(SpinePoints.Last,
                             SpinePoints.First);
end;

function TExtrusionNode.CrossSectionClosed: boolean;
var
  CrossSectionPoints: TVector2List;
begin
  CrossSectionPoints := FdCrossSection.Items;
  Result := (CrossSectionPoints.Count <> 0) and
    // Makes Internal Error with FPC 3.0.2 on Linux x86_64.
    // TVector2.PerfectlyEquals(CrossSectionPoints.Last,
    //                          CrossSectionPoints.First);
    TVector2.PerfectlyEquals(CrossSectionPoints[CrossSectionPoints.Count - 1],
                             CrossSectionPoints[0]);
end;

function TExtrusionNode.Proxy(var State: TX3DGraphTraverseState;
  const OverTriangulate: boolean): TAbstractGeometryNode;

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
  IFS: TIndexedFaceSetNode absolute Result;
  Index: Integer;
  E: TVRMLExtrusion;
  TexCoords: TVector2List;

  procedure NextIndex(SpineIndex, CrossSectionIndex: Integer;
    SpecialTex: boolean; SpecialTexIndex: Integer);
  begin
    if TexCoords <> nil then
    begin
      if SpecialTex then
        IFS.FdTexCoordIndex.Items.List^[Index] := SpecialTexIndex else
        IFS.FdTexCoordIndex.Items.List^[Index] :=
          SpineIndex * FdCrossSection.Count + CrossSectionIndex;
    end;

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
    if TexCoords <> nil then
      IFS.FdTexCoordIndex.Items.List^[Index] := -1;
    Inc(Index);
  end;

  function SideTexCoord(SpineIndex, CrossSectionIndex: Integer): TVector2;
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
  CoordNode: TCoordinateNode;
  TexCoordNode: TTextureCoordinateNode;
  Transform: TMatrix4;
  I, J, Len, SidesCount: Integer;
  LastY, LastZ: TVector3;
  TexCapsMin, TexCapsMax: TVector2;
  TexCapsTranslation, TexCapsScale: Single;
begin
  IFS := TIndexedFaceSetNode.Create(X3DName, BaseUrl);
  try
    E := TVRMLExtrusion.Create;
    try
      E.Node := Self;

      { calculate SidesCount = number of verts on sides, often used number }
      SidesCount := (E.High + 1) * FdCrossSection.Count;

      { Fill CoordNode }

      CoordNode := TCoordinateNode.Create('', BaseUrl);
      IFS.FdCoord.Value := CoordNode;

      CoordNode.FdPoint.Items.Count := SidesCount;

      for I := 0 to E.High do
      begin
        E.SpineTransformVar(I, LastY, LastZ, Transform);
        for J := 0 to FdCrossSection.Count - 1 do
        begin
          CoordNode.FdPoint.Items.List^[I * FdCrossSection.Count + J] :=
            Transform.MultPoint(CrossSection3D(J));
        end;
      end;

      { Fill TexCoordNode }

      if (FdTexCoord.Value <> nil) and FdTexCoord.CurrentChildAllowed then
      begin
        { No need for to create tex coords. }
        IFS.FdTexCoord.Value := FdTexCoord.Value;
        TexCoords := nil;
      end else
      begin
        TexCoordNode := TTextureCoordinateNode.Create('', BaseUrl);
        IFS.FdTexCoord.Value := TexCoordNode;
        TexCoords := TexCoordNode.FdPoint.Items;
      end;

      if TexCoords <> nil then
      begin
        Len := SidesCount;
        if FdBeginCap.Value or FdEndCap.Value then
          Len := Len + FdCrossSection.Count;
        TexCoords.Count := Len;

        for I := 0 to E.High do
          for J := 0 to FdCrossSection.Count - 1 do
            TexCoords.List^[I * FdCrossSection.Count + J] := SideTexCoord(I, J);

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
              if not IsZero(TexCapsMax[0] - TexCapsMin[0]) then
              begin
                { scale such that U is along the X of cap.
                  Coord calculation will go like
                    u := (x - TexCapsMin[0]) / (TexCapsMax[0] - TexCapsMin[0]) }
                TexCapsTranslation := -TexCapsMin[0];
                TexCapsScale := 1 /  (TexCapsMax[0] - TexCapsMin[0]);
              end;
            end else
            begin
              if not IsZero(TexCapsMax[1] - TexCapsMin[1]) then
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
                (FdCrossSection.Items.List^[I] +
                 Vector2(TexCapsTranslation, TexCapsTranslation))
                * TexCapsScale;
          end;
        end;
      end;

      { Fill CoordIndex and TexCoordIndex }

      if E.High > 0 then
        Len := 5 * E.High * (FdCrossSection.Count - 1) else
        Len := 0; { E.High may be -1, then Len should remain 0 }
      if FdBeginCap.Value then
        Len := Len + (FdCrossSection.Count + 1 - E.CrossSectionOmit);
      if FdEndCap.Value then
        Len := Len + (FdCrossSection.Count + 1 - E.CrossSectionOmit);
      IFS.FdCoordIndex.Items.Count := Len;
      if TexCoords <> nil then
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
        for J := FdCrossSection.Count - 1 downto E.CrossSectionOmit do
          NextIndex(0, J, true, SidesCount + J);
        NextIndexFaceDelimiter;
      end;

      if FdEndCap.Value then
      begin
        for J := E.CrossSectionOmit to FdCrossSection.Count - 1 do
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

function TExtrusionNode.ProxyUsesOverTriangulate: boolean;
begin
  Result := false;
end;

function TExtrusionNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := inherited;
  if Result <> nil then Exit;

  Result := FdtexCoord.Enumerate(Func);
  if Result <> nil then Exit;
end;

function TExtrusionNode.TexCoordField: TSFNode;
begin
  Result := FdTexCoord;
end;

function TExtrusionNode.AutoGenerate3DTexCoords: boolean;
begin
  Result := (FdTexCoord.Value = nil) or not FdTexCoord.CurrentChildAllowed;
end;

procedure TExtrusionNode.EventSet_crossSectionReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
begin
  FdcrossSection.Assign(Value);
  FdcrossSection.Changed;
end;

procedure TExtrusionNode.EventSet_orientationReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
begin
  Fdorientation.Assign(Value);
  Fdorientation.Changed;
end;

procedure TExtrusionNode.EventSet_scaleReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
begin
  Fdscale.Assign(Value);
  Fdscale.Changed;
end;

procedure TExtrusionNode.EventSet_spineReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
begin
  Fdspine.Assign(Value);
  Fdspine.Changed;
end;

function TExtrusionNode.ConvexField: TSFBool;
begin
  Result := FdConvex;
end;

function TExtrusionNode.SolidField: TSFBool;
begin
  Result := FdSolid;
end;

{ TIndexedFaceSetNode -------------------------------------------------------- }

procedure TIndexedFaceSetNode.CreateNode;
begin
  inherited;

  {$ifndef CASTLE_SLIM_NODES}
  FEventSet_colorIndex := TMFInt32Event.Create(Self, 'set_colorIndex', true);
  AddEvent(FEventSet_colorIndex);
  Eventset_colorIndex.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventSet_colorIndexReceive);

  FEventSet_coordIndex := TMFInt32Event.Create(Self, 'set_coordIndex', true);
  AddEvent(FEventSet_coordIndex);
  Eventset_coordIndex.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventSet_coordIndexReceive);

  FEventSet_normalIndex := TMFInt32Event.Create(Self, 'set_normalIndex', true);
  AddEvent(FEventSet_normalIndex);
  Eventset_normalIndex.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventSet_normalIndexReceive);

  FEventSet_texCoordIndex := TMFInt32Event.Create(Self, 'set_texCoordIndex', true);
  AddEvent(FEventSet_texCoordIndex);
  Eventset_texCoordIndex.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventSet_texCoordIndexReceive);
  {$endif}

  FFdColorIndex := TMFInt32.Create(Self, false, 'colorIndex', []);
   FdColorIndex.SaveToStreamLineUptoNegative := true;
   FdColorIndex.ChangeAlways := chGeometry;
  AddField(FFdColorIndex);
  { X3D specification comment: [0,Inf) or -1 }

  FFdConvex := TSFBool.Create(Self, false, 'convex', true);
   FdConvex.ChangeAlways := chGeometry;
  AddField(FFdConvex);

  FFdCoordIndex := TMFInt32.Create(Self, false, 'coordIndex', []);
   FdCoordIndex.SaveToStreamLineUptoNegative := true;
   FdCoordIndex.ChangeAlways := chGeometry;
  AddField(FFdCoordIndex);
  { X3D specification comment: [0,Inf) or -1 }

  FFdCreaseAngle := TSFFloat.Create(Self, false, 'creaseAngle', 0);
   FdCreaseAngle.Angle := true;
   FdCreaseAngle.ChangeAlways := chGeometry;
  AddField(FFdCreaseAngle);
  { X3D specification comment: [0,Inf) }

  FFdNormalIndex := TMFInt32.Create(Self, false, 'normalIndex', []);
   FdNormalIndex.SaveToStreamLineUptoNegative := true;
   FdNormalIndex.ChangeAlways := chGeometry;
  AddField(FFdNormalIndex);
  { X3D specification comment: [0,Inf) or -1 }

  FFdTexCoordIndex := TMFInt32.Create(Self, false, 'texCoordIndex', []);
   FdTexCoordIndex.SaveToStreamLineUptoNegative := true;
   FdTexCoordIndex.ChangeAlways := chGeometry;
  AddField(FFdTexCoordIndex);
  { X3D specification comment: [-1,Inf) }
end;

class function TIndexedFaceSetNode.ClassX3DType: string;
begin
  Result := 'IndexedFaceSet';
end;

class function TIndexedFaceSetNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

function TIndexedFaceSetNode.CoordIndexField: TMFLong;
begin
  Result := FdCoordIndex;
end;

procedure TIndexedFaceSetNode.EventSet_colorIndexReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
begin
  FdcolorIndex.Assign(Value);
  FdcolorIndex.Changed;
end;

procedure TIndexedFaceSetNode.EventSet_coordIndexReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
begin
  FdcoordIndex.Assign(Value);
  FdcoordIndex.Changed;
end;

procedure TIndexedFaceSetNode.EventSet_normalIndexReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
begin
  FdnormalIndex.Assign(Value);
  FdnormalIndex.Changed;
end;

procedure TIndexedFaceSetNode.EventSet_texCoordIndexReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
begin
  FdtexCoordIndex.Assign(Value);
  FdtexCoordIndex.Changed;
end;

function TIndexedFaceSetNode.ConvexField: TSFBool;
begin
  Result := FdConvex;
end;

procedure TSphereNode.CreateNode;
begin
  inherited;

  FFdRadius := TSFFloat.Create(Self, false, 'radius', 1);
   FdRadius.ChangeAlways := chGeometry;
  AddField(FFdRadius);
  { X3D specification comment: (0,Inf) }

  FFdSolid := TSFBool.Create(Self, false, 'solid', true);
   FdSolid.ChangeAlways := chGeometry;
  AddField(FFdSolid);

  FFdTexCoord := TSFNode.Create(Self, true, 'texCoord', [TTextureCoordinateGeneratorNode, TProjectedTextureCoordinateNode, TMultiTextureCoordinateNode]);
   FdTexCoord.ChangeAlways := chGeometry;
  AddField(FFdTexCoord);

  FFdSlices := TSFInt32.Create(Self, true, 'slices', -1);
   FdSlices.ChangeAlways := chGeometry;
  AddField(FFdSlices);

  FFdStacks := TSFInt32.Create(Self, true, 'stacks', -1);
   FdStacks.ChangeAlways := chGeometry;
  AddField(FFdStacks);
end;

class function TSphereNode.ClassX3DType: string;
begin
  Result := 'Sphere';
end;

class function TSphereNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

function TSphereNode.TexCoordField: TSFNode;
begin
  Result := FdTexCoord;
end;

function TSphereNode.AutoGenerate3DTexCoords: boolean;
begin
  Result := (FdTexCoord.Value = nil) or not FdTexCoord.CurrentChildAllowed;
end;

function TSphereNode.SolidField: TSFBool;
begin
  Result := FdSolid;
end;

function TSphereNode.CalculateSlices: Cardinal;
begin
  // use default in case of -1 or invalid value
  if FdSlices.Value < MinTriangulationSlices then
    Result := DefaultTriangulationSlices
  else
    Result := FdSlices.Value;
end;

function TSphereNode.CalculateStacks: Cardinal;
begin
  // use default in case of -1 or invalid value
  if FdStacks.Value < MinTriangulationStacks then
    Result := DefaultTriangulationStacks
  else
    Result := FdStacks.Value;
end;

{ routines ------------------------------------------------------------------- }

procedure RegisterGeometry3DNodes;
begin
  NodesManager.RegisterNodeClasses([
    TBoxNode,
    TConeNode,
    TCylinderNode,
    TElevationGridNode,
    TExtrusionNode,
    TIndexedFaceSetNode,
    TSphereNode
  ]);
end;

{$endif read_implementation}
