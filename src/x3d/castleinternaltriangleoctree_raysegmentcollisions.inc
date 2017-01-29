{
  Copyright 2003-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
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
    jak promien dla ktorego RayOrigin = Pos1, RayDirection = Pos2-Pos1.

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

{ To silence warning
  "Warning: Variable "IntersectionDistance" does not seem to be initialized"
  (FPC cannot figure out that we use IntersectionDistance only when
  Result <> nil, when IntersectionDistance is initialized). }
{$warnings off}

{$ifdef SEGMENT_COLLISION}
var
  RayOrigin: TYPE_VECTOR3 absolute Pos1;
  RayDirection: TYPE_VECTOR3;
{$endif}

  function TryTriangleCollision(
    out Intersection: TYPE_VECTOR3;
    out IntersectionDistance: TYPE_SCALAR;
    var Item: TTriangle): boolean;
  begin
    Result:=
      {$ifdef SEGMENT_COLLISION}
      Item.SegmentDirCollision(Intersection, IntersectionDistance,
        Pos1, RayDirection, Tag)
      {$else}
      Item.RayCollision(Intersection, IntersectionDistance,
        RayOrigin, RayDirection, Tag)
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
         (not VectorsEqual(Iterate_Intersection, RayOrigin)) ) and
       ( (not Assigned(TrianglesToIgnoreFunc)) or
         (not TrianglesToIgnoreFunc(ParentTree, Iterate_Item)) ) then
     ON_INTERSECTION_FOUND
  end;}

var
  Iterate_ItemNumber: integer;
  Iterate_Item: PTriangle;
  Iterate_Intersection: TYPE_VECTOR3;
  Iterate_IntersectionDistance: TYPE_SCALAR;
begin
 {$ifdef SEGMENT_COLLISION}
 RayDirection := VectorSubtract(Pos2, Pos1);
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

{$warnings on}