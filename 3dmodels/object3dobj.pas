{
  Copyright 2002-2005 Michalis Kamburelis.

  This file is part of "Kambi's 3dmodels Pascal units".

  "Kambi's 3dmodels Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's 3dmodels Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's 3dmodels Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(Handling of 3D models in Wavefront OBJ format.)

  First implementation vased on information from [http://www.gametutorials.com].
  Written without any particular reason --- I just saw some code doing so,
  and it seemed extremely easy, and so I decided to implement it too.
  And it works: we simply read "v", "vt" and "f" lines and that's all.

  Later extended to handle also normal vectors and materials,
  based on [http://www.fileformat.info/format/wavefrontobj/]
  and [http://www.fileformat.info/format/material/].
  Texture filename is also read from material file. }

unit Object3dOBJ;

interface

uses VectorMath, KambiUtils, Classes, KambiClassUtils, SysUtils, Boxes3d;

{$define read_interface}

type
  TOBJFace = record
    VertIndices, TexCoordIndices, NormalIndices: TVector3Cardinal;
    HasTexCoords: boolean;
    HasNormals: boolean;
  end;
  POBJFace = ^TOBJFace;

  TDynArrayItem_1 = TOBJFace;
  PDynArrayItem_1 = POBJFace;
  {$define DYNARRAY_1_IS_STRUCT}
  {$I DynArray_1.inc}
  TDynOBJFaceArray = TDynArray_1;

  { 3D model in OBJ file format. }
  TObject3dOBJ = class(TObjectBBox)
  private
    FVerts: TDynVector3SingleArray;
    FTexCoords: TDynVector2SingleArray;
    FNormals: TDynVector3SingleArray;
    FFaces: TDynOBJFaceArray;
    FBoundingBox: TBox3d;
  public
    constructor Create(const fname: string);
    destructor Destroy; override;

    { @groupBegin

      Model data.

      Contents of Verts, TexCoords, Normals and Faces are read-only
      for users of this class. }
    property Verts: TDynVector3SingleArray read FVerts;
    property TexCoords: TDynVector2SingleArray read FTexCoords;
    property Normals: TDynVector3SingleArray read FNormals;
    property Faces: TDynOBJFaceArray read FFaces;
    { @groupEnd }

    function BoundingBox: TBox3d; override;
  end;

  EInvalidOBJFile = class(Exception);

{$undef read_interface}

implementation

uses KambiStringUtils, KambiFilesUtils;

{$define read_implementation}
{$I dynarray_1.inc}

constructor TObject3dOBJ.Create(const fname: string);

  procedure ReadFacesFromOBJLine(const line: string);
  var
    face: TOBJFace;

    { Zainicjuj indeksy numer indiceNum face na podstawie VertexStr }
    procedure ReadIndices(const VertexStr: string;
      IndiceNum: integer);
    var
      VertexSeekPos: Integer;

      { Reads and updates VertexStr and VertexSeekPos, and sets
        IndiceExists and IndiceValue. IndiceExists is set to @false
        if next indice is indeed empty,
        or if we're standing at the end of VertexStr string.

        Just call it in sequence to read indices from VertexStr.
        Remember that empty indice may be followed by non-empty,
        e.g. VectorStr = '2//3' is allowed, and means that vertex
        index is 2, there's no texCoord index, and normal index is 3. }
      procedure NextIndice(out IndiceExists: boolean;
        out IndiceValue: Cardinal);
      var
        NewVertexSeekPos: Integer;
      begin
        NewVertexSeekPos := VertexSeekPos;

        while (NewVertexSeekPos <= Length(VertexStr)) and
          (VertexStr[NewVertexSeekPos] <> '/') do
          Inc(NewVertexSeekPos);

        IndiceExists := NewVertexSeekPos > VertexSeekPos;
        if IndiceExists then
          { we subtract 1, because indexed in OBJ are 1-based and we
            prefer 0-based }
          IndiceValue := StrToInt(CopyPos(
            VertexStr, VertexSeekPos, NewVertexSeekPos - 1)) - 1;

        { We add +1 to skip our ending '/' char.
          Note that we add this even if VertexSeekPos was already
          > Length(VertexStr), but this is harmless. }
        VertexSeekPos := NewVertexSeekPos + 1;
      end;

    var
      IndiceHasVertex, IndiceHasTexCoord, IndiceHasNormal: boolean;
    begin
      VertexSeekPos := 1;

      { read vertex index }
      NextIndice(IndiceHasVertex, Face.VertIndices[IndiceNum]);
      if not IndiceHasVertex then
        raise EInvalidOBJFile.CreateFmt(
          'Invalid OBJ vertex indexes "%s"', [VertexStr]);

      { read texCoord index }
      NextIndice(IndiceHasTexCoord, Face.TexCoordIndices[IndiceNum]);

      { read normal index }
      NextIndice(IndiceHasNormal, Face.NormalIndices[IndiceNum]);

      { update Face.HasXxx using IndiceHasXxx }
      Face.HasTexCoords := Face.HasTexCoords and IndiceHasTexCoord;
      Face.HasNormals := Face.HasNormals and IndiceHasNormal;
    end;

  var
    SeekPos: integer;

    function NextVertex: string;
    begin
      Result := NextToken(Line, SeekPos);
      if Result = '' then
        raise EInvalidOBJFile.CreateFmt(
          'Incomplete OBJ face specification "%s"', [Line]);
      while SCharIs(Line, SeekPos, WhiteSpaces) do Inc(SeekPos);
    end;

  begin
    { ReadIndices will eventually change this to @false, if for any
      vertex normal or texCoord will not be present. }
    Face.HasTexCoords := true;
    Face.HasNormals := true;

    SeekPos := 1;

    ReadIndices(NextVertex, 0);

    { we check this, and eventually exit (without generating any face),
      because blender exporter writes 2-item faces when the mesh is
      edges-only. TODO: we should hadle 2-item faces as edges
      (and probably 1-item faces as single points?). }
    if SeekPos > Length(Line) then Exit;

    ReadIndices(NextVertex, 1);
    if SeekPos > Length(Line) then Exit;

    ReadIndices(NextVertex, 2);

    Faces.AppendItem(Face);

    while SeekPos <= Length(Line) do
    begin
      Face.VertIndices[1] := Face.VertIndices[2];
      Face.TexCoordIndices[1] := Face.TexCoordIndices[2];
      Face.NormalIndices[1] := Face.NormalIndices[2];

      ReadIndices(NextVertex, 2);

      Faces.AppendItem(Face);
    end;
  end;

  function ReadTexCoordFromOBJLine(const line: string): TVector2Single;
  var
    SeekPos: integer;
  begin
    SeekPos := 1;
    result[0] := StrToFloat(NextToken(line, SeekPos));
    result[1] := StrToFloat(NextToken(line, SeekPos));
    { nie uzywamy DeFormat - bo tex coord w OBJ moze byc 3d (z trzema
      parametrami) a my uzywamy i tak tylko dwoch pierwszych }
  end;

var
  f: TextFile;
  linetok, s,lineAfterMarker: string;
  SeekPosAfterMarker: integer;
  GroupName: string;
begin
  inherited Create;
  FVerts := TDynVector3SingleArray.Create;
  FTexCoords := TDynVector2SingleArray.Create;
  FNormals := TDynVector3SingleArray.Create;
  FFaces := TDynOBJFaceArray.Create;

  SafeReset(f, fname, true);
  try
    while not Eof(f) do
    begin
      Readln(f, s);
      s := STruncateHash(s);

      { evaluate first line token }
      SeekPosAfterMarker := 1;
      lineTok := NextToken(s, SeekPosAfterMarker);
      lineAfterMarker := SEnding(s, SeekPosAfterMarker);
      { lineTok = '' means "this line is a comment" }
      if lineTok = '' then Continue;

      { specialized token line parsing }
      case ArrayPosText(lineTok, ['v', 'vt', 'f', 'vn', 'g']) of
        0: Verts.AppendItem(Vector3SingleFromStr(lineAfterMarker));
        1: TexCoords.AppendItem(ReadTexCoordFromOBJLine(lineAfterMarker));
        2: ReadFacesFromOBJLine(lineAfterMarker);
        3: Normals.AppendItem(Vector3SingleFromStr(lineAfterMarker));
        4: GroupName := Trim(LineAfterMarker);
        else { we ignore other linetoks };
      end;
    end;
  finally CloseFile(f) end;

  FBoundingBox := CalculateBoundingBox(
    PVector3Single(Verts.Items), Verts.Count, SizeOf(TVector3Single));
end;

destructor TObject3dOBJ.Destroy;
begin
  FreeAndNil(FVerts);
  FreeAndNil(FTexCoords);
  FreeAndNil(FNormals);
  FreeAndNil(FFaces);
  inherited;
end;

function TObject3dOBJ.BoundingBox: TBox3d;
begin
  Result := FBoundingBox;
end;

end.
