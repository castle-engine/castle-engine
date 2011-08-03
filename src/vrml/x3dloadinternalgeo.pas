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

{ 3D Videoscape GEO files handling (TObject3DGEO).
  See [http://local.wasp.uwa.edu.au/~pbourke/dataformats/geo/],
  handles at least geometry exported by Blender exporter. }
unit X3DLoadInternalGEO;

interface

uses VectorMath, KambiUtils, Classes, SysUtils, Boxes3D, Base3D;

type
  { Reader of GEO files.
    Note that contents of Verts and Faces are read-only for user of this unit. }
  TObject3DGEO = class(T3D)
  private
    FBoundingBox: TBox3D;
  public
    Verts: TDynVector3SingleArray;
    Faces: TDynVector3CardinalArray;

    constructor Create(const fname: string); reintroduce;
    destructor Destroy; override;
    function BoundingBox: TBox3D; override;
  end;

implementation

uses KambiFilesUtils, KambiStringUtils;

{ TObject3DGEO ---------------------------------------------------------------- }

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
 inherited Create(nil);
 Verts := TDynVector3SingleArray.Create;
 Faces := TDynVector3CardinalArray.Create;

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
      instead we set only AllowedCapacityOverflow.
    }
    Faces.AllowedCapacityOverflow := VertsInPolysCount - PolysCount * 2;
  end else
  begin
    { In newer formats, 2nd line contains just VertsCount. }
    Readln(F, VertsCount);
    PolysCount := -1;
  end;

  Verts.SetLength(VertsCount);
  for i := 0 to Verts.Count-1 do
    Readln(f, Verts.Items[i][0], Verts.Items[i][1], Verts.Items[i][2]);

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

 fBoundingBox := CalculateBoundingBox(
   PVector3Single(Verts.Items), Verts.Count, SizeOf(TVector3Single));
end;

destructor TObject3DGEO.Destroy;
begin
 Verts.Free;
 Faces.Free;
 inherited;
end;

function TObject3DGEO.BoundingBox: TBox3D;
begin
  result := FBoundingBox
end;

end.
