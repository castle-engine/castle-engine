{
  Copyright 2002-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Load 3D Videoscape GEO models.
  See [http://local.wasp.uwa.edu.au/~pbourke/dataformats/geo/].
  We handle basic geometry, we can open files exported by Blender exporter. }
unit X3DLoadInternalGEO;

{$I castleconf.inc}

interface

uses SysUtils, Classes,
  X3DNodes;

function LoadGEO(const Stream: TStream; const BaseUrl: String): TX3DRootNode;

implementation

uses CastleVectors, CastleUtils, CastleLog,
  CastleClassUtils, CastleDownload, CastleURIUtils,
  CastleFilesUtils, CastleStringUtils, X3DLoadInternalUtils;

{ TObject3DGEO ---------------------------------------------------------------- }

type
  { Reader of GEO files.
    Note that contents of Verts and Faces are read-only for user of this unit. }
  TObject3DGEO = class
  public
    Verts: TVector3List;
    Faces: TVector3CardinalList;
    constructor Create(const Stream: TStream);
    destructor Destroy; override;
  end;

constructor TObject3DGEO.Create(const Stream: TStream);
type
  TGEOFormatFlavor = (gfOld, gfMeshColorFaces, gfMeshColorVerts);
var
  Flavor: TGEOFormatFlavor;

  function ReadVertexIndex(const S: string): Cardinal;
  begin
    Result := StrToInt(S);
    { In older format, vertex index is 1-based. }
    if Flavor = gfOld then Dec(Result);
  end;

  { Read exactly one line of GEO file, reading new face information.
    Updates Faces. }
  procedure ReadGEOFace(const Line: string);
  var
    J, ThisPolyCount: Integer;
    FirstVert, LastVert: Cardinal;
    CurrentFace: TVector3Cardinal;
    LineTokens: TCastleStringList;
  begin
    LineTokens := CreateTokens(Line);
    try
      if LineTokens.Count = 0 then
      begin
        WritelnWarning('GEO', 'Empty line');
        Exit;
      end;

      ThisPolyCount := StrToInt(LineTokens[0]);
      if ThisPolyCount < 3 then
      begin
        WritelnWarning('GEO', 'Polygon with less than 3 vertexes');
        Exit;
      end;

      if LineTokens.Count < ThisPolyCount + 1 then
      begin
        WritelnWarning('GEO', 'Not enough vertex indexes on line');
        Exit;
      end;

      { the polygon is always at least a triangle, read it }
      for j := 0 to 2 do
        CurrentFace.Data[j] := ReadVertexIndex(LineTokens[J + 1]);
      Faces.Add(CurrentFace);

      FirstVert := CurrentFace.X;
      LastVert := CurrentFace.Z;

      { for each following vertex, add new triangle by connecting
        FirstVert, LastVert and new vertexa.
        Watch out for the order --- make all triangles oriented consistently. }
      for j := 3 to ThisPolyCount - 1 do
      begin
        CurrentFace.X := FirstVert;
        CurrentFace.Y := LastVert;
        CurrentFace.Z := ReadVertexIndex(LineTokens[J + 1]);
        Faces.Add(CurrentFace);

        LastVert := CurrentFace.Z;
      end;
    finally FreeAndNil(LineTokens) end;
  end;

var
  Reader: TTextReader;
  i: Integer;
  Line: string;
  VertsCount, PolysCount, VertsInPolysCount: Integer;
begin
 inherited Create;
 Verts := TVector3List.Create;
 Faces := TVector3CardinalList.Create;

 Reader := TTextReader.Create(Stream, false);
 try
  { Read first line: magic number (or not existent in older GEO format) }
  Line := Reader.Readln;
  Line := Trim(Line);
  if SameText(Line, '3DG1') then
    Flavor := gfMeshColorFaces else
  if SameText(Line, 'GOUR') then
    Flavor := gfMeshColorVerts else
    Flavor := gfOld;

  if Flavor = gfOld then
  begin
    { Use current value of Line, for older format the first line contains
      these counts. }
    DeFormat(Line, '%d %d %d', [@VertsCount, @PolysCount, @VertsInPolysCount]);

    { Ile mamy Faces trojkatnych ? Mamy liczbe
      wielokatow = PolysCount. Mamy sumaryczna liczbe wierzcholkow
      w nich. Na kazdy polygon przypadaja co najmniej 3 wierzcholki
      i one daja jeden trojkat. Kazdy nadmiarowy wierzcholek,
      bez wzgledu na to w ktorym polygonie sie znajdzie, spowoduje
      utworzenie nowego trojkata. Stad
        FFacesCount := PolysCount + (VertsInPolysCount - PolysCount * 3);
      czyli
        Faces.SetLength(VertsInPolysCount - PolysCount * 2);

      To cooperate with other Flavor, we do not set Faces.Count directly,
      instead we set only Capacity.
    }
    Faces.Capacity := VertsInPolysCount - PolysCount * 2;
  end else
  begin
    { In newer formats, 2nd line contains just VertsCount. }
    VertsCount := StrToInt(Reader.Readln);
    PolysCount := -1;
  end;

  Verts.Count := VertsCount;
  for i := 0 to Verts.Count-1 do
    Verts.List^[I] := Vector3FromStr(Reader.Readln);

  if PolysCount <> -1 then
  begin
    for i := 0 to PolysCount - 1 do
      ReadGEOFace(Reader.Readln);
  end else
  begin
    { PolysCount not known. So we just read the file as fast as we can. }
    while not Reader.Eof do
      ReadGEOFace(Reader.Readln);
  end;
 finally FreeAndNil(Reader) end;
end;

destructor TObject3DGEO.Destroy;
begin
 Verts.Free;
 Faces.Free;
 inherited;
end;

{ LoadGEO -------------------------------------------------------------------- }

function LoadGEO(const Stream: TStream; const BaseUrl: String): TX3DRootNode;
var
  geo: TObject3DGEO;
  verts: TCoordinateNode;
  faces: TIndexedFaceSetNode;
  Shape: TShapeNode;
  i: integer;
begin
  geo := TObject3DGEO.Create(Stream);
  try
    result := TX3DRootNode.Create('', BaseUrl);
    try
      Result.HasForceVersion := true;
      Result.ForceVersion := X3DVersion;

      Shape := TShapeNode.Create('', BaseUrl);
      result.AddChildren(Shape);
      Shape.Material := TMaterialNode.Create('', BaseUrl);

      faces := TIndexedFaceSetNode.Create('', BaseUrl);
      Shape.FdGeometry.Value := faces;
      faces.FdCreaseAngle.Value := NiceCreaseAngle;
      faces.FdSolid.Value := false;
      faces.FdCoordIndex.Count := geo.Faces.Count * 4;
      for i := 0 to geo.Faces.Count-1 do
      begin
        faces.FdCoordIndex.Items.List^[i * 4    ] := geo.Faces.List^[i].X;
        faces.FdCoordIndex.Items.List^[i * 4 + 1] := geo.Faces.List^[i].Y;
        faces.FdCoordIndex.Items.List^[i * 4 + 2] := geo.Faces.List^[i].Z;
        faces.FdCoordIndex.Items.List^[i * 4 + 3] := -1;
      end;

      verts := TCoordinateNode.Create('', BaseUrl);
      faces.Coord := verts;
      verts.SetPoint(geo.Verts);
    except result.Free; raise end;
  finally geo.Free end;
end;

end.
