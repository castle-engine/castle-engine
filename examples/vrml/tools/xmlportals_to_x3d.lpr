{
  Copyright 2008-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Convert $1 (XML file with 3D scene + portals and sectors
  from Remigiusz Zukowski) into $2 (X3D file). }
program xmlportals_to_x3d;

{$I kambiconf.inc}

uses SysUtils, KambiUtils, KambiXMLRead, DOM, KambiXMLUtils, VectorMath,
  VRMLNodes, Contnrs, KambiStringUtils, Classes, KambiClassUtils;

function DOMGetCardinalChild(Elem: TDOMElement; const ChildName: string): Cardinal;
begin
  Result := StrToInt(DOMGetTextChild(Elem, ChildName));
end;

function DOMGetVector3(VElem: TDOMElement): TVector3Single;
begin
  Result[0] := StrToFloat(DOMGetTextChild(VElem, 'x'));
  Result[1] := StrToFloat(DOMGetTextChild(VElem, 'y'));
  Result[2] := StrToFloat(DOMGetTextChild(VElem, 'z'));
end;

function DOMGetVector3Child(Elem: TDOMElement; const ChildName: string): TVector3Single;
begin
  Result := DOMGetVector3(DOMGetChildElement(Elem, ChildName, true));
end;

function DOMGetVector2Child(Elem: TDOMElement; const ChildName: string): TVector2Single;
var
  VElem: TDOMElement;
begin
  VElem := DOMGetChildElement(Elem, ChildName, true);
  Result[0] := StrToFloat(DOMGetTextChild(VElem, 'x'));
  Result[1] := StrToFloat(DOMGetTextChild(VElem, 'y'));
end;

var
  { List of <px> elements with object_id="...." (put when first serializing
    an element). This is crucial for later resolving of objects with
    object_id_reference="...." (put when serializing the same object
    instance another time). }
  Px: TObjectList;

procedure CalculatePx(E: TDOMElement);
var
  I: TXMLElementIterator;
  NextId: Integer;
begin
  if (E.TagName = 'px') and
     (E.AttribStrings['object_id'] <> '') then
  begin
    { We actively use here the fact that object_id read this way
      will be successive. This makes CalculatePx implementation
      (and using Px) much simpler, since you know that all indexes
      from 0 to Px.Count - 1 are successively filled. }
    NextId := StrToInt(SEnding(E.AttribStrings['object_id'], 2));
    Check(NextId = Px.Count, 'object_id not successive');
    Px.Count := Px.Count + 1;
    Px.Items[Px.Count - 1] := E;
  end;

  I := TXMLElementIterator.Create(E);
  try
    while I.GetNext do CalculatePx(I.Current);
  finally FreeAndNil(I) end;
end;

{ Assuming Px is <px> element, resolve it. That is, if this is only
  a reference (object_id_reference attribute), then replace it with
  the actual content (node with matching object_id attribute). }
procedure ResolvePx(var PxElem: TDOMElement);
var
  Id: Integer;
begin
  Assert(PxElem.TagName = 'px');
  if PxElem.AttribStrings['object_id'] = '' then
  begin
    { So this is a reference - resolve using object_id_reference
      and our Px table. }

    Id := StrToInt(SEnding(PxElem.AttribStrings['object_id_reference'], 2));
    PxElem := TDOMElement(Px.Items[Id]);
  end;
end;

var
  MeshPx: TDOMElement;

procedure ReadGeometry(OutputGroup: TNodeGroup_2);
var
  MVertices, MTriangles: TDOMElement;
  I: TXMLElementIterator;

  Shape: TNodeShape;
  IFS: TNodeIndexedFaceSet_2;
  CoordIndex: TDynLongIntArray;
  Coord: TDynVector3SingleArray;
  Normal:TDynVector3SingleArray;
  TexCoord: TDynVector2SingleArray;

  VerticesCount, TrianglesCount: Cardinal;
begin
  { Prepare XML input }
  MVertices := DOMGetChildElement(DOMGetChildElement(
    MeshPx, 'm_vertexContainer', true), 'm_vertices', true);
  VerticesCount := DOMGetCardinalChild(MVertices, 'count');
  Writeln('Vertices: ', VerticesCount);

  MTriangles := DOMGetChildElement(MeshPx, 'm_triangles', true);
  TrianglesCount := DOMGetCardinalChild(MTriangles, 'count');
  Writeln('Triangles: ', TrianglesCount);

  { Prepare VRML output }
  Shape := TNodeShape.Create('', '');
  Shape.FdAppearance.Value := TNodeAppearance.Create('', '');
  TNodeAppearance(Shape.FdAppearance.Value).FdMaterial.Value :=
    TNodeMaterial_2.Create('', '');
  OutputGroup.SmartAddChild(Shape);

  IFS := TNodeIndexedFaceSet_2.Create('', '');
  Shape.FdGeometry.Value := IFS;
  IFS.FdSolid.Value := false;

  IFS.FdCoord.Value := TNodeCoordinate.Create('', '');
  Coord := TNodeCoordinate(IFS.FdCoord.Value).FdPoint.Items;
  Coord.AllowedCapacityOverflow := VerticesCount;

  IFS.FdNormal.Value := TNodeNormal.Create('', '');
  Normal := TNodeNormal(IFS.FdNormal.Value).FdVector.Items;
  Normal.AllowedCapacityOverflow := VerticesCount;

  IFS.FdNormalPerVertex.Value := true;

  IFS.FdTexCoord.Value := TNodeTextureCoordinate.Create('', '');
  TexCoord := TNodeTextureCoordinate(IFS.FdTexCoord.Value).FdPoint.Items;
  TexCoord.AllowedCapacityOverflow := VerticesCount;

  { Actually read XML data to VRML }
  I := TXMLElementIterator.Create(MVertices);
  try
    while I.GetNext do
      if I.Current.TagName = 'item' then
      begin
        Coord.Add(DOMGetVector3Child(I.Current, 'position'));
        Normal.Add(DOMGetVector3Child(I.Current, 'normal'));
        TexCoord.Add(DOMGetVector2Child(I.Current, 'texCoord'));
      end;
  finally FreeAndNil(I) end;

  CoordIndex := IFS.FdCoordIndex.Items;
  CoordIndex.AllowedCapacityOverflow := TrianglesCount * 4;

  I := TXMLElementIterator.Create(MTriangles);
  try
    while I.GetNext do
      if I.Current.TagName = 'item' then
      begin
        CoordIndex.Add(DOMGetCardinalChild(I.Current, 'm_v0'));
        CoordIndex.Add(DOMGetCardinalChild(I.Current, 'm_v1'));
        CoordIndex.Add(DOMGetCardinalChild(I.Current, 'm_v2'));
        CoordIndex.Add(-1);
      end;
  finally FreeAndNil(I) end;
end;

procedure AddPortalPoly(PortalPx: TDOMElement;
  CoordIndex: TDynLongIntArray;
  Coord: TDynVector3SingleArray);
var
  MVertices: TDOMElement;
  {VerticesCount: Cardinal; not needed}
  I: TXMLElementIterator;
begin
  MVertices := DOMGetChildElement(DOMGetChildElement(
    PortalPx, 'm_poly', true), 'm_vertities', true);
  {VerticesCount := DOMGetCardinalChild(MVertices, 'count'); not needed}

  I := TXMLElementIterator.Create(MVertices);
  try
    while I.GetNext do
      if I.Current.TagName = 'item' then
      begin
        Coord.Add(DOMGetVector3(I.Current));
        CoordIndex.Add(Coord.High);
      end;
  finally FreeAndNil(I) end;

  CoordIndex.Add(-1);
end;

procedure ReadPortals(OutputGroup: TVRMLGroupingNode);
var
  PortalPx: TDOMElement;
  MPortals: TDOMElement;
  PortalsCount: Cardinal;
  I: TXMLElementIterator;

  Shape: TNodeShape;
  IFS: TNodeIndexedFaceSet_2;
  PortalsAppearance: TNodeAppearance;
  PortalsMaterial: TNodeMaterial_2;
  CoordIndex: TDynLongIntArray;
  Coord: TDynVector3SingleArray;
begin
  { Prepare XML input }
  MPortals := DOMGetChildElement(MeshPx, 'm_portals', true);
  PortalsCount := DOMGetCardinalChild(MPortals, 'count');
  Writeln('Portals: ', PortalsCount);

  { Prepare VRML output }
  Shape := TNodeShape.Create('', '');
  OutputGroup.SmartAddChild(Shape);

  PortalsAppearance := TNodeAppearance.Create('PortalsAppearance', '');

  PortalsMaterial := TNodeMaterial_2.Create('PortalsMaterial', '');
  PortalsAppearance.FdMaterial.Value := PortalsMaterial;
  PortalsMaterial.FdDiffuseColor.Value := Vector3Single(1, 1, 0);
  PortalsMaterial.FdTransparency.Value := 0.9;

  Shape.FdAppearance.Value := PortalsAppearance;

  IFS := TNodeIndexedFaceSet_2.Create('Portals', '');
  Shape.FdGeometry.Value := IFS;
  IFS.FdSolid.Value := false;

  IFS.FdCoord.Value := TNodeCoordinate.Create('', '');
  Coord := TNodeCoordinate(IFS.FdCoord.Value).FdPoint.Items;
  Coord.AllowedCapacityOverflow := 1000;

  CoordIndex := IFS.FdCoordIndex.Items;
  CoordIndex.AllowedCapacityOverflow := 1000;

  I := TXMLElementIterator.Create(MPortals);
  try
    while I.GetNext do
      if I.Current.TagName = 'item' then
      begin
        PortalPx := DOMGetChildElement(I.Current, 'px', true);
        ResolvePx(PortalPx);
        AddPortalPoly(PortalPx, CoordIndex, Coord);
      end;
  finally FreeAndNil(I) end;
end;

procedure ReadPortalsInSuperSectors(OutputGroup: TVRMLGroupingNode);
var
  CoordIndex: TDynLongIntArray;
  Coord: TDynVector3SingleArray;

  procedure OutputOneSuperSector(SuperSectorPx: TDOMElement);
  var
    MExternalPortals: TDOMElement;
    ExternalPortalsCount: Cardinal;
    PortalPx: TDOMElement;
    I: TXMLElementIterator;
  begin
    MExternalPortals := DOMGetChildElement(SuperSectorPx, 'm_externalPortals', true);
    ExternalPortalsCount := DOMGetCardinalChild(MExternalPortals, 'count');
    Writeln('External portals (within one super sector): ', ExternalPortalsCount);

    I := TXMLElementIterator.Create(MExternalPortals);
    try
      while I.GetNext do
        if I.Current.TagName = 'item' then
        begin
          PortalPx := DOMGetChildElement(I.Current, 'px', true);
          ResolvePx(PortalPx);
          AddPortalPoly(PortalPx, CoordIndex, Coord);
        end;
    finally FreeAndNil(I) end;
  end;

var
  SuperSectorPx: TDOMElement;
  MSuperSectors: TDOMElement;
  SuperSectorsCount: Cardinal;
  I: TXMLElementIterator;

  Shape: TNodeShape;
  IFS: TNodeIndexedFaceSet_2;
  PortalsAppearance: TNodeAppearance;
  PortalsMaterial: TNodeMaterial_2;
begin
  { Prepare XML input }
  MSuperSectors := DOMGetChildElement(MeshPx, 'm_superSectors', true);
  SuperSectorsCount := DOMGetCardinalChild(MSuperSectors, 'count');
  Writeln('Super sectors: ', SuperSectorsCount);

  { Prepare VRML output }
  Shape := TNodeShape.Create('', '');
  OutputGroup.SmartAddChild(Shape);

  PortalsAppearance := TNodeAppearance.Create('SuperSectorsPortalsAppearance', '');

  PortalsMaterial := TNodeMaterial_2.Create('SuperSectorsPortalsMaterial', '');
  PortalsAppearance.FdMaterial.Value := PortalsMaterial;
  PortalsMaterial.FdDiffuseColor.Value := Vector3Single(0, 0, 1);
  PortalsMaterial.FdTransparency.Value := 0.9;

  Shape.FdAppearance.Value := PortalsAppearance;

  IFS := TNodeIndexedFaceSet_2.Create('SuperSectorsPortals', '');
  Shape.FdGeometry.Value := IFS;
  IFS.FdSolid.Value := false;

  IFS.FdCoord.Value := TNodeCoordinate.Create('', '');
  Coord := TNodeCoordinate(IFS.FdCoord.Value).FdPoint.Items;
  Coord.AllowedCapacityOverflow := 1000;

  CoordIndex := IFS.FdCoordIndex.Items;
  CoordIndex.AllowedCapacityOverflow := 1000;

  I := TXMLElementIterator.Create(MSuperSectors);
  try
    while I.GetNext do
      if I.Current.TagName = 'item' then
      begin
        SuperSectorPx := DOMGetChildElement(I.Current, 'px', true);
        ResolvePx(SuperSectorPx);
        OutputOneSuperSector(SuperSectorPx);
      end;
  finally FreeAndNil(I) end;
end;

procedure SaveVRMLPortalsFile(Node: TVRMLNode;
  const Filename, PrecedingComment: string);
const
  SceneSuffix = {$I xmlportals_to_x3d_suffix.inc};
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(Filename, fmCreate);
  try
    SaveVRMLClassic(Node, Stream, PrecedingComment);
    WritelnStr(Stream, SceneSuffix);
  finally Stream.Free end;
end;

var
  Doc: TXMLDocument;
  OutputRoot: TVRMLRootNode;
  OutputSwitch: TNodeSwitch_2;
begin
  Parameters.CheckHigh(2);

  try
    { ReadXMLFile always sets TXMLDocument param (possibly to nil),
      even in case of exception. So place it inside try..finally. }
    ReadXMLFile(Doc, Parameters[1]);

    Check(Doc.DocumentElement.TagName = 'boost_serialization',
      'Root node must be <boost_serialization>');

    Px := TObjectList.Create(false);
    CalculatePx(Doc.DocumentElement);

    MeshPx := DOMGetChildElement(DOMGetChildElement(
      Doc.DocumentElement, 'm_pMesh', true), 'px', true);

    OutputRoot := TVRMLRootNode.Create('', '');
    try
      ReadGeometry(OutputRoot);

      OutputSwitch := TNodeSwitch_2.Create('PortalsDisplaySwitch', '');
      OutputSwitch.FdWhichChoice.Value := 0;
      OutputRoot.SmartAddChild(OutputSwitch);

      ReadPortals(OutputSwitch);
      ReadPortalsInSuperSectors(OutputSwitch);

      { Content xmlportals_to_x3d_suffix.wrl uses X3D features
        (KeySensor), to I have to force this to be X3D (not just VRML 2.0)
        to get correct file. }
      OutputRoot.ForceVersion := true;
      OutputRoot.ForceVersionMajor := 3;
      OutputRoot.ForceVersionMinor := 2;

      SaveVRMLPortalsFile(OutputRoot, Parameters[2],
        'By xmlportals_to_x3d, from ' + ExtractFileName(Parameters[1]));
    finally FreeAndNil(OutputRoot) end;
  finally
    FreeAndNil(Doc);
    FreeAndNil(Px)
  end;
end.
