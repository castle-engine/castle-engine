{
  Copyright 2002-2004 Michalis Kamburelis.

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

{ @abstract(@link(TObject3dGEO) class to read 3d object description from
  GEO files.) }

unit Object3dGEO;

interface

uses VectorMath, KambiUtils, Classes, KambiClassUtils, SysUtils, Boxes3d;

type
  { Simple reader of GEO files.
    Note that contents of Verts and Faces are read-only for user of this unit. }
  TObject3dGEO = class(TObjectBBox)
  private
    FPolysCount, VertsInPolysCount:Cardinal;
    FBoundingBox:TBox3d;
  public
    Verts:TDynVector3SingleArray;
    Faces:TDynVector3CardinalArray;
    property PolysCount:Cardinal read FPolysCount;

    constructor Create(const fname:string);
    destructor Destroy; override;
    function BoundingBox:TBox3d; override;
  end;

implementation

{ TObject3dGEO ---------------------------------------------------------------- }

constructor TObject3dGEO.Create(const fname:string);
var f:TextFile;
    FaceNum, i,j,ThisPolyCount:integer;
    FirstVert, LastVert, VertsCount:Cardinal;
begin
 inherited Create;
 Verts:=TDynVector3SingleArray.Create;
 Faces:=TDynVector3CardinalArray.Create;

 SafeReset(f, fname, true);
 try
  Readln(f, VertsCount, FPolysCount, VertsInPolysCount);

  Verts.SetLength(VertsCount);
  for i:=0 to Verts.Count-1 do
   Readln(f, Verts.Items[i][0], Verts.Items[i][1], Verts.Items[i][2]);

  { no wlasnie, ile mamy Faces trojkatnych ? Mamy liczbe
    wielokatow = PolysCount. Mamy sumaryczna liczbe wierzcholkow
    w nich. Na kazdy polygon przypadaja co najmniej 3 wierzcholki
    i one daja jeden trojkat. Kazdy nadmiarowy wierzcholek,
    bez wzgledu na to w ktorym polygonie sie znajdzie, spowoduje
    utworzenie nowego trojkata. Stad
  FFacesCount:=PolysCount + (VertsInPolysCount - PolysCount*3);
    czyli }
  Faces.SetLength(VertsInPolysCount - PolysCount*2);

  FaceNum:=0; { FaceNum to numer nastepnej wolnej Face[] }
  for i:=0 to PolysCount-1 do
  begin
   Read(f, ThisPolyCount);
   {odczytaj "na pewniaka" pierwszy trojkat. Pamietaj ze w pliku indeksy
    sa numerowane od 1 a my chcemy od zera.}
   for j:=0 to 2 do
   begin
    Read(f, Faces.Items[FaceNum][j]);
    Dec(Faces.Items[FaceNum][j]);
   end;
   FirstVert:=Faces.Items[FaceNum][0];
   LastVert:=Faces.Items[FaceNum][2];
   Inc(FaceNum);
   {dla kazdego nastepnego vertexa polygonu tworz nowy trojkat jako
    sklejenie FirstVert, LastVert i nowego vertexa. Pilnuj kolejnosci
    aby wszystkie trojkaty z tego polygonu byly tak zorientowane jak ten
    polygon.}
   for j:=3 to ThisPolyCount-1 do
   begin
    Faces.Items[FaceNum][0]:=FirstVert;
    Faces.Items[FaceNum][1]:=LastVert;
    Read(f, Faces.Items[FaceNum][2]);
    Dec(Faces.Items[FaceNum][2]);
    LastVert:=Faces.Items[FaceNum][2];
    Inc(FaceNum);
   end;
   Readln(f);
  end;
  Assert(FaceNum = Faces.Count, 'GEO file '+fname+' incorrect');
 finally CloseFile(f) end;

 fBoundingBox:=CalculateBoundingBox(
   PVector3Single(Verts.Items), Verts.Count, SizeOf(TVector3Single));
end;

destructor TObject3dGEO.Destroy;
begin
 Verts.Free;
 Faces.Free;
 inherited;
end;

function TObject3dGEO.BoundingBox:TBox3d;
begin result:=FBoundingBox end;

end.
