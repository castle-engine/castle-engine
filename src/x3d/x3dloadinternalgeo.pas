{
  Copyright 2002-2012 Michalis Kamburelis.

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

interface

uses X3DNodes;

function LoadGEO(const filename: string): TX3DRootNode;

implementation

uses CastleVectors, CastleUtils, Classes, SysUtils,
  CastleFilesUtils, CastleStringUtils, X3DLoadInternalUtils;

{ TObject3DGEO ---------------------------------------------------------------- }

type
  { Reader of GEO files.
    Note that contents of Verts and Faces are read-only for user of this unit. }
  TObject3DGEO = class
  public
    Verts: TVector3SingleList;
    Faces: TVector3CardinalList;
    constructor Create(const fname: string);
    destructor Destroy; override;
  end;

constructor TObject3DGEO.Create(const fname: string);
type
  TGEOFormatFlavor = (gfOld, gfMeshColorFaces, gfMeshColorVerts);
var
  Flavor: TGEOFormatFlavor;

  function ReadVertexIndex(var F: TextFile): Cardinal;
  begin
    Read(F, Result);
    { In older format, vertex index is 1-based. }
    if Flavor = gfOld then Dec(Result);
  end;

  { Read exactly one line of GEO file, reading new face information.
    Updates Faces. }
  procedure ReadGEOFace(var F: TextFile);
  var
    J, ThisPolyCount: Integer;
    FirstVert, LastVert: Cardinal;
    CurrentFace: PVector3Cardinal;
  begin
    Read(F, ThisPolyCount);

    CurrentFace := Faces.Add;

    { odczytaj "na pewniaka" pierwszy trojkat }
    for j := 0 to 2 do
      CurrentFace^[j] := ReadVertexIndex(F);

    FirstVert := CurrentFace^[0];
    LastVert := CurrentFace^[2];

    { dla kazdego nastepnego vertexa polygonu tworz nowy trojkat jako
      sklejenie FirstVert, LastVert i nowego vertexa. Pilnuj kolejnosci
      aby wszystkie trojkaty z tego polygonu byly tak zorientowane jak ten
      polygon.}
    for j := 3 to ThisPolyCount - 1 do
    begin
      CurrentFace := Faces.Add;

      CurrentFace^[0] := FirstVert;
      CurrentFace^[1] := LastVert;
      CurrentFace^[2] := ReadVertexIndex(F);

      LastVert := CurrentFace^[2];
    end;
    Readln(f);
  end;

var
  f: TextFile;
  i: Integer;
  Line: string;
  VertsCount, PolysCount, VertsInPolysCount: Integer;
begin
 inherited Create;
 Verts := TVector3SingleList.Create;
 Faces := TVector3CardinalList.Create;

 SafeReset(f, fname, true);
 try
  { Read first line: magic number (or not existent in older GEO format) }
  Readln(F, Line);
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
    Readln(F, VertsCount);
    PolysCount := -1;
  end;

  Verts.Count := VertsCount;
  for i := 0 to Verts.Count-1 do
    Readln(f, Verts.L[i][0], Verts.L[i][1], Verts.L[i][2]);

  if PolysCount <> -1 then
  begin
    for i := 0 to PolysCount - 1 do
      ReadGEOFace(F);
  end else
  begin
    { PolysCount not known. So we just read the file as fas as we can. }
    while not SeekEof(F) do
      ReadGEOFace(F);
  end;
 finally CloseFile(f) end;
end;

destructor TObject3DGEO.Destroy;
begin
 Verts.Free;
 Faces.Free;
 inherited;
end;

{ LoadGEO -------------------------------------------------------------------- }

function LoadGEO(const filename: string): TX3DRootNode;
var
  geo: TObject3DGEO;
  verts: TCoordinateNode;
  faces: TIndexedFaceSetNode;
  Shape: TShapeNode;
  i: integer;
  BaseUrl: string;
begin
  BaseUrl := ExtractFilePath(ExpandFilename(filename));
  geo := TObject3DGEO.Create(filename);
  try
    result := TX3DRootNode.Create('', BaseUrl);
    try
      Result.HasForceVersion := true;
      Result.ForceVersion := X3DVersion;

      Shape := TShapeNode.Create('', BaseUrl);
      result.FdChildren.Add(Shape);
      Shape.Material := TMaterialNode.Create('', BaseUrl);

      faces := TIndexedFaceSetNode.Create('', BaseUrl);
      Shape.FdGeometry.Value := faces;
      faces.FdCreaseAngle.Value := NiceCreaseAngle;
      faces.FdSolid.Value := false;
      faces.FdCoordIndex.Count := geo.Faces.Count * 4;
      for i := 0 to geo.Faces.Count-1 do
      begin
        faces.FdCoordIndex.Items.L[i * 4    ] := geo.Faces.L[i][0];
        faces.FdCoordIndex.Items.L[i * 4 + 1] := geo.Faces.L[i][1];
        faces.FdCoordIndex.Items.L[i * 4 + 2] := geo.Faces.L[i][2];
        faces.FdCoordIndex.Items.L[i * 4 + 3] := -1;
      end;

      verts := TCoordinateNode.Create('', BaseUrl);
      faces.FdCoord.Value := verts;
      verts.FdPoint.Items.Assign(geo.Verts);
    except result.Free; raise end;
  finally geo.Free end;
end;

end.
