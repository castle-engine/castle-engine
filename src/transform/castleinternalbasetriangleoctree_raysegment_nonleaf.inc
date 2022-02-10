{
  Copyright 2003-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Implementacja funkcji CommonSegment i CommonRay w jednym kodzie.
  See castleinternaltriangleoctree_raysegmentcollisions.inc for some comments.
}

{ CommonRay/Segment begins --------------------------------------------------- }

  function DoNotLeaf: PTriangle;
  {$ifdef SEGMENT_COLLISION}
  var
    RayOrigin: TVector3 absolute Pos1;
    RayDirection: TVector3;
  {$endif}

    { For demo_models/vrml_1/flat_triangles_octree_test.wrl,
      this really needs some tolerance. And SingleEpsilon
      was too small. }
    function Box3DContainsTolerant(
      const pt: TVector3; const box: TBox3D): boolean;
    begin
      if box.IsEmpty then exit(false);
      result := Between(pt[0], Box.Data[0].X - 0.001, Box.Data[1].X + 0.001) and
                Between(pt[1], Box.Data[0].Y - 0.001, Box.Data[1].Y + 0.001) and
                Between(pt[2], Box.Data[0].Z - 0.001, Box.Data[1].Z + 0.001);
    end;

  var
    { FirstBoxIntr = first ray with box intersection if RayOrigin is outside
      if the Box or simply RayOrigin if RayOrigin is inside our Box  }
    FirstBoxIntr: TVector3;

    { poczatkowy Subnode, inicjowany na poczatku na SubnodeWithPoint(FirstBoxIntr).
      Moze byc modyfikowany w pewnym specjalnym przypadku przy okazji obliczania
      PlaneIntersects }
    StartSubnode: TOctreeSubnodeIndex;

    { Element [0] tej tablicy okesla przeciecie z plaszczyzna X = MiddlePoint[0],
        element [1] - z plaszczyzna Y = MiddlePoint[1] itd.
      Exists mowi czy w ogole promien przecina ta plaszczyzne,
        jezeli tak to IntrT to T z jakim zaszlo intersection miedzy
        promieniem FirstBoxIntr, RayDirection (lub odcinkiem FirstBoxIntr, Pos2).
        Czyli im wieksze T tym dalej przeciecie jest od FirstBoxIntr a wiec
        i od RayOrigin/Pos1.
      PlaneIntersectsCount to liczba planes ktore maja Exists.
        PlaneIntersectsCount moze byc <= 3.}
    PlaneIntersects: array[0..2]of record
      Exists: boolean;
      PlaneIntersectionDistance: Single;
    end;
    ExistingPlaneIntersectsCount: integer;

    { posortowane wzgledem SqrDistanceFromRayOrigin (od najmniejszego do najwiekszego)
      PlaneIntersects, tzn. SortedPlaneIntersects[0] to indeks
      najmniejszego PlaneIntersects, itd. az
      do SortedPlaneIntersects[ExistingPlaneIntersectsCount-1] }
    SortedPlaneIntersects: array[0..2]of integer;

    { SubnodesToCheck to ulozone w kolejnosci Subnode'y ktore powinnismy
        sprawdzic czy jest w nich przeciecie promienia ze scena. Tzn. najpierw
        powinnismy sprawdzic Subnode nr 0 (w nim jest punkt RayOrigin), potem
        Subnode 1 itd - az do SubnodesToCheckCount-1.
      Jak widac z rozmiaru tej tablicy wiemy ze bedziemy musieli wejsc
        w maksymalnie 4 subnode'y - bo przeciez bedziemy mieli maksymalnie
        3 przeciecia z 3 plaszczyznami dzielacymi ten wezel drzewa, co dzieli
        nam promien na maksymalnie 4 segmenty. }
    SubnodesToCheck: array[0..3]of TOctreeSubnodeIndex;
    SubnodesToCheckCount: integer;

    procedure CalculatePlaneIntersects;
    var i: integer;
        PlaneIntersect: TVector3;
    begin
     ExistingPlaneIntersectsCount := 0;

     for i := 0 to 2 do
      with PlaneIntersects[i] do
      begin
       if SameValue(Box.Data[1].Data[I], Box.Data[0].Data[I]) then
       begin
         { This means that the Box is practically flat, so all 3 planes
           (Box.Data[0].Data[I], MiddlePoint[I], Box.Data[1].Data[I]) are practically
           coplanar.

           We want in this case to make sure that we actually
           enter both sides of MiddlePoint[I] plane (since it's pretty random
           where did we put some flat triangles). So we want to make
           sure Exists is set to @true.

           Starting StartSubnode[i] should be the one closer the RayOrigin.

           See demo_models/vrml_1/flat_triangles_octree_test.wrl
           for example when this is needed (with new okDynamicCollisions,
           this plane is flat within it's octree, even when it's transformed
           by rotation). }

         { We could here just use TrySimplePlaneRayIntersection always.
           But that would be unoptimal for segment, since a ray may collide while
           segment doesn't. }

         Exists:=
           {$ifdef SEGMENT_COLLISION}
           TrySimplePlaneSegmentIntersection(PlaneIntersect, PlaneIntersectionDistance,
             i, MiddlePoint[i], FirstBoxIntr, Pos2)
           {$else}
           TrySimplePlaneRayIntersection(PlaneIntersect, PlaneIntersectionDistance,
             i, MiddlePoint[i], FirstBoxIntr, RayDirection)
           {$endif};

         if not Exists then
         begin
           { Just force Exists to true.
             We know that FirstBoxIntr is practically lying on MiddlePoint[I] plane,
             so it's a good candidate. }

           Exists := true;
           PlaneIntersect := FirstBoxIntr;
           PlaneIntersectionDistance := 0;
         end;

         StartSubnode[i] := RayDirection[i] <= 0;
       end else
       if IsZero(PointToSimplePlaneDistance(FirstBoxIntr, i, MiddlePoint[i])) and
          not IsZero(RayDirection[i]) then
       begin
         { This means that FirstBoxIntr lies on MiddlePoint[I] plane.
           Although the box is not flat (this case was eliminated above).
           Which means that the ray
           somehow enters the box exactly in the middle, or maybe the
           RayOrigin was exactly in the middle (e.g. consider RayOrigin = exactly
           middle point of octree box).

           We handle it specially, because normal code would be numerically
           unstable: on one hand, StartSubnode may be detected on any side
           --- it's practically random. On the other hand, whether
           Exists is set is also random. So various strange things
           could happen (e.g. StartSubNode is on the farther (RayDirection) side,
           but intersection is detected too, so we incorrectly go back;
           or StartSubNode is on the closer (-RayDirection) side,
           but intersection is not detected, so we incorrectly do not change
           sides).

           The correct solution in this case is to have StartSubNode
           on the farther side, and do not see intersection with middle plane. }
         StartSubnode[i] := RayDirection[i] > 0;
         Exists := false;
       end else
       begin
         { jezeli przeciecie nie znajduje sie w naszym Box to na pewno nie
           chcemy go dolaczyc do PlaneIntersect wiec ustawiamy Exists := false;
           Dzieki temu ze badamy przeciecia od FirstBoxIntr a nie od RayOrigin
           (a nie od Pos1, w przypadku odcinka) to
           wiemy ze przeciecie jest juz POTEM jak promien wyjdzie z naszego
           Boxa (a nie ZANIM promien wszedl do naszego Boxa, co byloby tu troche
           klopotliwe;) }
         Exists:=
           {$ifdef SEGMENT_COLLISION}
           TrySimplePlaneSegmentIntersection(PlaneIntersect, PlaneIntersectionDistance,
             i, MiddlePoint[i], FirstBoxIntr, Pos2)
           {$else}
           TrySimplePlaneRayIntersection(PlaneIntersect, PlaneIntersectionDistance,
             i, MiddlePoint[i], FirstBoxIntr, RayDirection)
           {$endif}
             and Box.Contains(PlaneIntersect);
       end;

       if Exists then Inc(ExistingPlaneIntersectsCount);
      end;
    end;

    procedure CalculateSortedPlaneIntersects;
    var SortedPlaneIntersectsCount: integer;

      procedure NextIntersect(c: integer);
      begin
       SortedPlaneIntersects[SortedPlaneIntersectsCount] := c;
       Inc(SortedPlaneIntersectsCount);
      end;

    var
      C1, C2, CM: T3DAxis;
    begin
     SortedPlaneIntersectsCount := 0;

     {wyznacz c1 i c2, ew. wrzucajac juz pierwsze przeciecie do SortedPlaneIntersects}
     if ExistingPlaneIntersectsCount = 3 then
     begin
      cm := IndexMin(PlaneIntersects[0].PlaneIntersectionDistance,
                     PlaneIntersects[1].PlaneIntersectionDistance,
                     PlaneIntersects[2].PlaneIntersectionDistance);
      RestOf3DCoords(CM, C1, C2);
      NextIntersect(cm);
     end else
     if not PlaneIntersects[0].Exists then
      begin c1 := 1; c2 := 2 end else
     if not PlaneIntersects[1].Exists then
      begin c1 := 0; c2 := 2 end else
     {wiec wiemy ze (not PlaneIntersects[2].Exists)}
      begin c1 := 0; c2 := 1 end;

     case ExistingPlaneIntersectsCount-SortedPlaneIntersectsCount of
      1: if PlaneIntersects[c1].Exists then
         NextIntersect(c1) else
         NextIntersect(c2);
      2: begin
         if PlaneIntersects[c1].PlaneIntersectionDistance <
            PlaneIntersects[c2].PlaneIntersectionDistance then
          begin NextIntersect(c1); NextIntersect(c2) end else
          begin NextIntersect(c2); NextIntersect(c1) end;
        end;
     end;
    end;

    procedure CalculateSubnodesToCheck;
    var i, IntersectPlane: integer;
    begin
     SubnodesToCheckCount := ExistingPlaneIntersectsCount+1;
     SubnodesToCheck[0] := StartSubnode;
     for i := 1 to ExistingPlaneIntersectsCount do
     begin
      SubnodesToCheck[i] := SubnodesToCheck[i-1];
      { jezeli wchodzimy do tego Subnode'a z powodu przeciecia z plane dzielacym
        o stalym X (czyli IntersectPlane = 0) to znaczy ze wspolrzedna X promienia
        przechodzi na druga strone tej plaszczyzny (a pozostale wspolrzedne
        zostaja w tych czesciach gdzie byly). Dlatego wystarczy zanegowac
        zmienna b[IntersectPlane] w nowym plane. }
      IntersectPlane := SortedPlaneIntersects[i-1];
      SubnodesToCheck[i, IntersectPlane] := not SubnodesToCheck[i, IntersectPlane];
     end;
    end;

  var
    i: integer;
    SubNode: TBaseTrianglesOctreeNode;
  begin
   {$ifdef SEGMENT_COLLISION}
   RayDirection := Pos2 - Pos1;
   {$endif}

   if not Box.TryRayEntrance(FirstBoxIntr, RayOrigin, RayDirection) then
     Exit(nil);
   StartSubnode := SubnodeWithPoint(FirstBoxIntr);

   CalculatePlaneIntersects;
   CalculateSortedPlaneIntersects;
   CalculateSubnodesToCheck;

   { szukamy najblizszego Intersection wchodzac w SubNodes. Zauwazmy ze
     w calym DoNotLeaf zawsze dzialamy jakby ReturnClosestIntersection = true
     za wyjatkiem malego szczegoliku ponizej przy "if result <> nil ...". }
   result := nil;
   for i := 0 to SubnodesToCheckCount - 1 do
   begin
    SubNode := TBaseTrianglesOctreeNode(TreeSubNodes[
      SubnodesToCheck[i, 0],
      SubnodesToCheck[i, 1],
      SubnodesToCheck[i, 2]]);
    result := SubNode.
      { jest ponizej bardzo wazne zebysmy nie zmienili Pos1 ani RayOrigin wywolujac
        sie rekurencyjnie (np. zeby nie przekazac FirstBoxIntr) bo to
        spowodowaloby ze zmienna IgnoreMarginAtStart powinna byc (byc moze)
        ustawiona na false. }
      {$ifdef SEGMENT_COLLISION}
      CommonSegment(Intersection, IntersectionDistance, Pos1, Pos2,
        Tag,
        ReturnClosestIntersection, TriangleToIgnore, IgnoreMarginAtStart,
        TrianglesToIgnoreFunc)
      {$else}
      CommonRay(Intersection, IntersectionDistance, RayOrigin, RayDirection,
        Tag,
        ReturnClosestIntersection, TriangleToIgnore, IgnoreMarginAtStart,
        TrianglesToIgnoreFunc)
      {$endif}
    ;

    { musimy sie tu upewnic ze zwrocone Intersection jest rzeczywiscie
      w pytanym SubnodesToCheck[i]. Jezeli nie to znaczy ze to przeciecie
      nalezy do innego subnodea'a i to on je zwroci (a my nie powinnismy
      teraz wychodzic jesli ReturnClosestIntersection bo byc moze to przeciecie
      nie jest najblizszym z mozliwych) }
    if (result <> nil) and
       ((not ReturnClosestIntersection) or
        Box3DContainsTolerant(Intersection, SubNode.Box)) then
     Exit;
   end;
  end;

begin
  if not IsLeaf then
    Result := DoNotLeaf else
    Result :=
      {$ifdef SEGMENT_COLLISION}
      CommonSegmentLeaf(Intersection, IntersectionDistance, Pos1, Pos2,
        Tag,
        ReturnClosestIntersection, TriangleToIgnore, IgnoreMarginAtStart,
        TrianglesToIgnoreFunc)
      {$else}
      CommonRayLeaf(Intersection, IntersectionDistance, RayOrigin, RayDirection,
        Tag,
        ReturnClosestIntersection, TriangleToIgnore, IgnoreMarginAtStart,
        TrianglesToIgnoreFunc)
      {$endif}
    ;
end;
