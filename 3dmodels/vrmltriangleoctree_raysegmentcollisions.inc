{
  Copyright 2003-2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ implementacja funkcji CommonSegmentLeaf i CommonRayLeaf w jednym kodzie.
  Implementujac segment zdefiniuj SEGMENT_COLLISION zebysmy mogli
  w odpowiednich miejscach uzyc specyficznego tylko dla Odcinka lub tylko
  dla Promienia kawalka kodu.

  Roznice w obsludze SegmentCollision :
  - zasadniczo, dla poprawnosci dzialania wystarczylo
    a) uzywac TryTriangleSegmentDirCollision do testowania bezposrednich przeciec
       trojkat-segment w DoLeaf (zamiast TryTriangleRayCollision).
    b) schodzic rekurencyjnie w DoNotLeaf przez SegmentCollision (zamiast
       przez RayCollision)
    W kazdym innym przypadku mozna traktowac segment Pos1, Pos2
    jak promien dla ktorego Ray0 = Pos1, RayVector = Pos2-Pos1.

  - ale natuaralnie tracilibysmy wtedy sporo dla optymalnosci.
    O ile DoLeaf nie mozna juz zoptymalizowac w jakis specyficzny dla
    odcinka sposob o tyle DoNotLeaf mozna zoptymalizowac znacznie :
    chodzi o to zeby przy obliczaniu przeciec PlaneIntersects
    uwzgledniac fakt ze mamy do czynienia z odcinkiem, w zwiazku z czym
    czasem mimo ze mamy przeciecie z promieniem odcinka - nie mamy
    przeciecia z samym odcinkiem.
}

{ nie uzywam tu mechanizmu DUAL_IMPLEMENTATION. Ale kiedys zaczalem to robic
  i niewykluczone ze kiedys jednak bede chcial to tu miec. Dlatego zapisuje
  funkcje *Collision uzywajac zdefiniowanych ponizej makr ktore w razie czego
  pozwala latwo zmienic uzywane typy. }

{$define TYPE_SCALAR := Single}
{$define TYPE_VECTOR2 := TVector2Single}
{$define TYPE_VECTOR3 := TVector3Single}
{$define TYPE_VECTOR4 := TVector4Single}
{$define TYPE_TRIANGLE2 := TTriangle2Single}
{$define TYPE_TRIANGLE3 := TTriangle3Single}
{$define TYPE_MATRIX4 := TMatrix4Single}
{$define SCALAR_EQUALITY_EPSILON := SingleEqualityEpsilon}
{$define ONE_VECTOR := OneVectorSingle}
{$define IDENTITY_MATRIX := IdentityMatrix4Single}

{ CommonRay/SegmentLeaf begins --------------------------------------------- }

{$ifdef SEGMENT_COLLISION}
var
  Ray0: TYPE_VECTOR3 absolute Pos1;
  RayVector: TYPE_VECTOR3;
{$endif}

  function TryTriangleCollision(
    out Intersection: TYPE_VECTOR3;
    out IntersectionDistance: TYPE_SCALAR;
    var Item: TVRMLTriangle): boolean;
  begin
    Result:=
      {$ifdef SEGMENT_COLLISION}
      Item.SegmentDirCollision(Intersection, IntersectionDistance,
        Pos1, RayVector,
        {$ifdef OCTREE_ITEM_USE_MAILBOX} RayOdcTag, {$endif}
        ParentTree.DirectCollisionTestsCounter)
      {$else}
      Item.RayCollision(Intersection, IntersectionDistance,
        Ray0, RayVector,
        {$ifdef OCTREE_ITEM_USE_MAILBOX} RayOdcTag, {$endif}
        ParentTree.DirectCollisionTestsCounter)
      {$endif}
  end;

  { makro ITERATE_INTERSECTIONS iteruje zmienna Iterate_ItemNum po tablicy
    Items i wywoluje makro ON_INTERSECTION_FOUND gdy Iterate_ItemNum
    wskazuje na trojkat z ktorym jest przeciecie.

    Podczas wywolywania ON_INTERSECTION_FOUND zmienne Iterate_Intersection
    and Iterate_IntersectionDistance are set to calculated intersection point.
  }
  {$define ITERATE_INTERSECTIONS:=
  for Iterate_ItemNumber := 0 to ItemsCount-1 do
  begin
    Iterate_Item := Items[Iterate_ItemNumber];
    if TryTriangleCollision(Iterate_Intersection, Iterate_IntersectionDistance,
         Iterate_Item^) and
       ( TriangleToIgnore <> Iterate_Item ) and
       ( (not IgnoreMarginAtStart) or
         (not VectorsEqual(Iterate_Intersection, Ray0)) ) and
       ( (not Assigned(TrianglesToIgnoreFunc)) or
         (not TrianglesToIgnoreFunc(ParentTree, Iterate_Item)) ) then
     ON_INTERSECTION_FOUND
  end;}

var
  Iterate_ItemNumber: integer;
  Iterate_Item: PVRMLTriangle;
  Iterate_Intersection: TYPE_VECTOR3;
  Iterate_IntersectionDistance: TYPE_SCALAR;
begin
 {$ifdef SEGMENT_COLLISION}
 RayVector := VectorSubtract(Pos2, Pos1);
 {$endif}

 if ReturnClosestIntersection then
 begin
  Result := nil;

  {$define ON_INTERSECTION_FOUND :=
  begin
    if (Result = nil) or
       (Iterate_IntersectionDistance < IntersectionDistance) then
    begin
      Result               := Iterate_Item;
      Intersection         := Iterate_Intersection;
      IntersectionDistance := Iterate_IntersectionDistance;
    end;
  end;}
  ITERATE_INTERSECTIONS
 end else
 begin
  {$define ON_INTERSECTION_FOUND :=
  begin
    Result               := Iterate_Item;
    Intersection         := Iterate_Intersection;
    IntersectionDistance := Iterate_IntersectionDistance;
    Exit;
  end;}
  ITERATE_INTERSECTIONS
  Exit(nil);
 end;
end;
{$undef ITERATE_INTERSECTIONS}
{$undef ON_INTERSECTION_FOUND}
