{%MainUnit x3dnodes.pas}
{
  Copyright 2002-2024 Michalis Kamburelis.

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
    function TexCoordField: TSFNode; override;
    function SolidField: TSFBool; override;

    function Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode; override;
    function BoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function AutoGenerate3DTexCoords: boolean; override;

  {$I auto_generated_node_helpers/x3dnodes_box.inc}
  end;

  { Cone. }
  TConeNode = class(TAbstractGeometryNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    function TexCoordField: TSFNode; override;
    function SolidField: TSFBool; override;
    function CalculateSlices: Cardinal;
    function CalculateStacks: Cardinal;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;
    function Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode; override;
    function BoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TX3DGraphTraverseState;
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
    function TexCoordField: TSFNode; override;
    function SolidField: TSFBool; override;
    function CalculateSlices: Cardinal;
    function CalculateStacks: Cardinal;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;
    function Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode; override;
    function BoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TX3DGraphTraverseState;
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
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
    class function ClassIsNotEmpty(
      const AXDimension, AZDimension: Integer;
      const AXSpacing, AZSpacing: Single;
      const AHeight: TSingleList): Boolean;
  public
    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;

    { Do we have (correct) data to render.

      Checks whether
      - xDimension and zDimension are >= 2,
      - xSpacing and zSpacing are > 0
      - height list has at least the required number of values.
    }
    function IsNotEmpty: boolean;

    function ColorField: TSFNode; override;
    function InternalFogCoord: TMFFloat; override;
    function AttribField: TMFNode; override;
    function SolidField: TSFBool; override;
    function FieldSetByEvent(const Event: TX3DEvent): TX3DField; override;

    function Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode; override;

    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    { Create triangle set to render the elevation grid.
      Useful for
      - Proxy in this class
      - TCastleTerrain which wants direct TIndexedTriangleSetNode
        (faster to later update than to maintain TElevationGridNode). }
    class function InternalCreateTriangles(
      const AXDimension, AZDimension: Integer;
      const AXSpacing, AZSpacing: Single;
      const AHeight: TSingleList;
      const ATexCoord: TAbstractTextureCoordinateNode;
      const TriangulationIgnoreHeights: Boolean): TIndexedTriangleSetNode;

  {$I auto_generated_node_helpers/x3dnodes_elevationgrid.inc}
  end;

  { 2D cross-section shape extruded along a 3D spine. }
  TExtrusionNode = class(TAbstractGeometryNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  strict private
    procedure Eventset_crossSectionReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
    procedure Eventset_orientationReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
    procedure Eventset_scaleReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
    procedure Eventset_spineReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
  public
    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;

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

    function Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode; override;

    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function BoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TX3DGraphTraverseState;
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
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
    procedure Eventset_coordIndexReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
    procedure Eventset_normalIndexReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
    procedure Eventset_texCoordIndexReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
  public
    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;

    function ConvexField: TSFBool; override;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;

    function CoordIndexField: TMFLong; override;

    procedure InternalCoordPolygons(
      State: TX3DGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;

    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

  {$I auto_generated_node_helpers/x3dnodes_indexedfaceset.inc}
  end;
  TIndexedFaceSetNode_2 = TIndexedFaceSetNode;

  { Sphere. }
  TSphereNode = class(TAbstractGeometryNode)
  public
    function TexCoordField: TSFNode; override;
    function SolidField: TSFBool; override;
    function CalculateSlices: Cardinal;
    function CalculateStacks: Cardinal;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;
    function Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode; override;

    function BoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function AutoGenerate3DTexCoords: boolean; override;

  {$I auto_generated_node_helpers/x3dnodes_sphere.inc}
  end;
  TSphereNode_2 = TSphereNode;

{$endif read_interface}

{$ifdef read_implementation}

{ TBoxNode ------------------------------------------------------------------- }

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

{ TConeNode ------------------------------------------------------------------ }

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
  // use default in case of 0 or invalid value
  if FdSlices.Value < MinTriangulationSlices then
    Result := DefaultTriangulationSlices
  else
    Result := FdSlices.Value;
end;

function TConeNode.CalculateStacks: Cardinal;
begin
  // use default in case of 0 or invalid value
  if FdStacks.Value < MinTriangulationStacks then
    Result := DefaultTriangulationStacks
  else
    Result := FdStacks.Value;
end;

{ TCylinderNode -------------------------------------------------------------- }

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
  // use default in case of 0 or invalid value
  if FdSlices.Value < MinTriangulationSlices then
    Result := DefaultTriangulationSlices
  else
    Result := FdSlices.Value;
end;

function TCylinderNode.CalculateStacks: Cardinal;
begin
  Result := 1;

  { As we use Phong shading now by default,
    there's no point in using stacks for Cylinders -- they do not
    improve geometry, they do not improve shading.

    (Note that we still use stacks for Cone, where they improve the
    shading on sides.)
  }

(*

  // use default in case of 0 or invalid value
  if FdStacks.Value < MinTriangulationStacks then
    Result := DefaultTriangulationStacks
  else
    Result := FdStacks.Value;
*)
end;

{ TElevationGridNode --------------------------------------------------------- }

constructor TElevationGridNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  Eventset_height.AddNotification({$ifdef FPC}@{$endif} EventSet_HeightReceive);
end;

class function TElevationGridNode.ClassIsNotEmpty(
  const AXDimension, AZDimension: Integer;
  const AXSpacing, AZSpacing: Single;
  const AHeight: TSingleList): Boolean;
begin
  Result :=
    (AXDimension >= 2) and
    (AZDimension >= 2) and
    { VRML spec says that xSpacing and ySpacing shall be > 0.
      So when they are = 0 (or < 0) we don't render anything. }
    (AXSpacing > 0) and
    (AZSpacing > 0) and
    (AHeight.Count >= AXDimension * AZDimension);
end;

function TElevationGridNode.IsNotEmpty: Boolean;
begin
  Result := ClassIsNotEmpty(
    FdXDimension.Value, FdZDimension.Value,
    FdXSpacing.Value, FdZSpacing.Value,
    FdHeight.Items);
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
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
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

constructor TExtrusionNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  Eventset_crossSection.AddNotification({$ifdef FPC}@{$endif} EventSet_crossSectionReceive);
  Eventset_orientation.AddNotification({$ifdef FPC}@{$endif} EventSet_orientationReceive);
  Eventset_scale.AddNotification({$ifdef FPC}@{$endif} EventSet_scaleReceive);
  Eventset_spine.AddNotification({$ifdef FPC}@{$endif} EventSet_spineReceive);
end;

function TExtrusionNode.CrossSection3D(Index: integer): TVector3;
begin
  Result.X := FdCrossSection.Items.L[Index].X;
  Result.Y := 0;
  Result.Z := FdCrossSection.Items.L[Index].Y;
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

{ Save the TAbstractGeometryNode to a file.
  Useful to debug Proxy methods. }
{.$define CASTLE_DEBUG_PROXY_EXTRUSION}
{$ifdef CASTLE_DEBUG_PROXY_EXTRUSION}
procedure DebugSaveGeometry(const Geometry: TAbstractGeometryNode);
var
  Shape: TShapeNode;
  Appearance: TAppearanceNode;
  Material: TMaterialNode;
  Root: TX3DRootNode;
begin
  Material := TMaterialNode.Create;

  Appearance := TAppearanceNode.Create;
  Appearance.Material := Material;

  Shape := TShapeNode.Create;
  Shape.Geometry := Geometry;
  Shape.Appearance := Appearance;

  Root := TX3DRootNode.Create;
  Root.AddChildren(Shape);

  SaveNode(Root, '/tmp/debug_geometry.x3dv');
end;
{$endif}

function TExtrusionNode.Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode;
var
  IFS: TIndexedFaceSetNode absolute Result;
  Index: Integer;
  E: TExtrusionUtility;
  TexCoords: TVector2List;

  procedure NextIndex(SpineIndex, CrossSectionIndex: Integer;
    SpecialTex: boolean; SpecialTexIndex: Integer);
  begin
    if TexCoords <> nil then
    begin
      if SpecialTex then
        IFS.FdTexCoordIndex.Items.L[Index] := SpecialTexIndex else
        IFS.FdTexCoordIndex.Items.L[Index] :=
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

    IFS.FdCoordIndex.Items.L[Index] :=
      SpineIndex * FdCrossSection.Count + CrossSectionIndex;

    Inc(Index);
  end;

  procedure NextIndexFaceDelimiter;
  begin
    IFS.FdCoordIndex.Items.L[Index] := -1;
    if TexCoords <> nil then
      IFS.FdTexCoordIndex.Items.L[Index] := -1;
    Inc(Index);
  end;

  function SideTexCoord(SpineIndex, CrossSectionIndex: Integer): TVector2;
  begin
    { Check ranges, to avoid dividing by zero (eventually the
      corresponding coord is set to a predictable zero :) ). }
    if FdCrossSection.Count > 1 then
      Result.X := CrossSectionIndex / (FdCrossSection.Count - 1) else
      Result.X := 0;
    if E.High > 0 then
      Result.Y := SpineIndex / E.High else
      Result.Y := 0;
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
    E := TExtrusionUtility.Create;
    try
      E.Node := Self;
      if E.Empty then
        Exit;

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
          CoordNode.FdPoint.Items.L[I * FdCrossSection.Count + J] :=
            Transform.MultPoint(CrossSection3D(J));
        end;
      end;

      { Fill TexCoordNode }

      if (FdTexCoord.Value <> nil) and FdTexCoord.CurrentChildAllowed then
      begin
        { No need for to create tex coords. }
        IFS.FdTexCoord.Value := FdTexCoord.Value;
        TexCoordNode := nil;
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
            TexCoords.L[I * FdCrossSection.Count + J] := SideTexCoord(I, J);

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
            TexCoordNode.FdPoint.Items.L[SidesCount + I] :=
                (FdCrossSection.Items.L[I] +
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
            and "solid" fields predictable for VRML / X3D author, since (s)he
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

    { The sides' quads are known to be convex.
      However, the caps may be non-convex.
      So we need to set IFS.FdConvex.Value to more secure value, false. }
    IFS.FdConvex.Value := FdConvex.Value;

    {$ifdef CASTLE_DEBUG_PROXY_EXTRUSION}
    DebugSaveGeometry(IFS);
    {$endif}
  except FreeAndNil(Result); raise end;
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
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  FdcrossSection.Assign(Value);
  FdcrossSection.Changed;
end;

procedure TExtrusionNode.EventSet_orientationReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  Fdorientation.Assign(Value);
  Fdorientation.Changed;
end;

procedure TExtrusionNode.EventSet_scaleReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  Fdscale.Assign(Value);
  Fdscale.Changed;
end;

procedure TExtrusionNode.EventSet_spineReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
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

constructor TIndexedFaceSetNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  {$ifndef CASTLE_SLIM_NODES}
  Eventset_colorIndex.AddNotification({$ifdef FPC}@{$endif} EventSet_colorIndexReceive);
  Eventset_coordIndex.AddNotification({$ifdef FPC}@{$endif} EventSet_coordIndexReceive);
  Eventset_normalIndex.AddNotification({$ifdef FPC}@{$endif} EventSet_normalIndexReceive);
  Eventset_texCoordIndex.AddNotification({$ifdef FPC}@{$endif} EventSet_texCoordIndexReceive);
  {$endif}
  FdColorIndex.SaveToStreamLineUptoNegative := true;
  FdCoordIndex.SaveToStreamLineUptoNegative := true;
  FdNormalIndex.SaveToStreamLineUptoNegative := true;
  FdTexCoordIndex.SaveToStreamLineUptoNegative := true;
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
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  FdcolorIndex.Assign(Value);
  FdcolorIndex.Changed;
end;

procedure TIndexedFaceSetNode.EventSet_coordIndexReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  FdcoordIndex.Assign(Value);
  FdcoordIndex.Changed;
end;

procedure TIndexedFaceSetNode.EventSet_normalIndexReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  FdnormalIndex.Assign(Value);
  FdnormalIndex.Changed;
end;

procedure TIndexedFaceSetNode.EventSet_texCoordIndexReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  FdtexCoordIndex.Assign(Value);
  FdtexCoordIndex.Changed;
end;

function TIndexedFaceSetNode.ConvexField: TSFBool;
begin
  Result := FdConvex;
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
  // use default in case of 0 or invalid value
  if FdSlices.Value < MinTriangulationSlices then
    Result := DefaultTriangulationSlices
  else
    Result := FdSlices.Value;
end;

function TSphereNode.CalculateStacks: Cardinal;
begin
  // use default in case of 0 or invalid value
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
