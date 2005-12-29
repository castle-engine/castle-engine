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

{ @abstract(Basic handling of 3d model info in .OBJ (Wavefront) file.)

  Based on information from [http://www.gametutorials.com].
  Written without any particular reason --- I just saw some code doing so,
  and it seemed extremely easy, and so I decided to implement it too.
  And it works : we simply read "v", "vt" and "f" lines and that's all.

  This unit (probably) will not be ever extended (too much...). }

unit Object3dOBJ;

interface

uses VectorMath, KambiUtils, Classes, KambiClassUtils, SysUtils, Boxes3d;

{$define read_interface}

type
  TOBJFace = record
    VertIndices, TexCoordIndices: TVector3Cardinal;
    HasTexCoords: boolean;
  end;
  POBJFace = ^TOBJFace;

  TDynArrayItem_1 = TOBJFace;
  PDynArrayItem_1 = POBJFace;
  {$define DYNARRAY_1_IS_STRUCT}
  {$I DynArray_1.inc}
  TDynOBJFaceArray = TDynArray_1;

  { Simple reader of OBJ files.
    Contents of Verts, TexCoords and Faces are read-only
    for users of this class. }
  TObject3dOBJ = class(TObjectBBox)
  private
    FBoundingBox: TBox3d;
  public
    Verts :TDynVector3SingleArray;
    TexCoords :TDynVector2SingleArray;
    Faces :TDynOBJFaceArray;
    constructor Create(const fname: string);
    destructor Destroy; override;

    function BoundingBox: TBox3d; override;
  end;

  EInvalidOBJFile = class(Exception);

{$undef read_interface}

implementation

{$define read_implementation}
{$I DynArray_1.inc}

constructor TObject3dOBJ.Create(const fname: string);

  procedure ReadFacesFromOBJLine(const line: string);
  var SeekPos: integer;
      face: TOBJFace;

    procedure ReadIndices(indiceNum: integer);
    { zainicjuj indeksy numer indiceNum face na podstawie line od pozycji SeekPos
      (i przesun SeekPos odpowiednio) }
    var indiceStr: string;
        IndiceWithTexCoords: boolean;
        Indice1, Indice2, Indice3: integer;
    begin
     indiceStr := NextToken(line, SeekPos);
     if indiceStr = '' then
      raise EInvalidOBJFile.Create('Uncomplete face specification : '+line);

     case TryDeFormat(indiceStr, '%d/%d/%d', [@Indice1, @Indice2, @Indice3]) of
      1:begin
         IndiceWithTexCoords := false;
         face.VertIndices[indiceNum] := Indice1;
        end;
      2, 3: 
        begin
         IndiceWithTexCoords := true;
         face.VertIndices[indiceNum] := Indice1;
         face.TexCoordIndices[indiceNum] := Indice2;
        end;
      else raise EInvalidOBJFile.Create(
        'Wrong OBJ face indices specification : '+indiceStr);
     end;

     {chcemy miec Vert i TexCoord indices liczone od zera, nie od 1}
     Dec(face.VertIndices[indiceNum]);
     if IndiceWithTexCoords then Dec(face.TexCoordIndices[indiceNum]);

     face.HasTexCoords := face.HasTexCoords and IndiceWithTexCoords;
    end;

  begin
   SeekPos := 1;
   face.HasTexCoords := true;
   ReadIndices(0);
   ReadIndices(1);
   ReadIndices(2);
   Faces.AppendItem(face);
   while SeekPos <= Length(line) do
   begin
    face.VertIndices[1] := face.VertIndices[2];
    face.TexCoordIndices[1] := face.TexCoordIndices[2];
    ReadIndices(2);
    Faces.AppendItem(face);
   end;
  end;

  function ReadTexCoordFromOBJLine(const line: string): TVector2Single;
  var SeekPos: integer;
  begin
   SeekPos := 1;
   result[0] := StrToFloat(NextToken(line, SeekPos));
   result[1] := StrToFloat(NextToken(line, SeekPos));
   { nie uzywamy DeFormat - bo tex coord w OBJ moze byc 3d (z trzema
     parametrami) a my uzywamy i tak tylko dwoch pierwszych }
  end;

var f: TextFile;
    linetok, s,lineAfterMarker: string;
    SeekPosAfterMarker: integer;
begin
 inherited Create;
 Verts := TDynVector3SingleArray.Create;
 TexCoords := TDynVector2SingleArray.Create;
 Faces := TDynOBJFaceArray.Create;

 SafeReset(f, fname, true);
 try
  while not Eof(f) do
  begin
   Readln(f, s);
   s := STruncateHash(s);

   {evaluate first line token}
   SeekPosAfterMarker := 1;
   lineTok := NextToken(s, SeekPosAfterMarker);
   lineAfterMarker := SEnding(s, SeekPosAfterMarker);
   {lineTok = '' means "this line is a comment"; we don't want to update
    lastTokWasF variable, we don't want to do anything }
   if lineTok = '' then Continue;

   {specialized token line parsing}
   case ArrayPosText(lineTok, ['v', 'vt', 'f']) of
    0: Verts.AppendItem(Vector3SingleFromStr(lineAfterMarker));
    1: TexCoords.AppendItem(ReadTexCoordFromOBJLine(lineAfterMarker));
    2: ReadFacesFromOBJLine(lineAfterMarker);
    else {we ignore other linetoks like "vn" or "g"};
   end;
  end;
 finally CloseFile(f) end;

 fBoundingBox := CalculateBoundingBox(
   PVector3Single(Verts.Items), Verts.Count, SizeOf(TVector3Single));
end;

destructor TObject3dOBJ.Destroy;
begin
 Verts.Free;
 TexCoords.Free;
 Faces.Free;
 inherited;
end;

function TObject3dOBJ.BoundingBox: TBox3d;
begin result := FBoundingBox end;

end.
