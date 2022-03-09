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

{ Implementation of all XxxCollision methods in CastleSceneCore unit. }

{ TCastleSceneCore ----------------------------------------------------------- }

function TCastleSceneCore.LocalHeightCollision(const APosition, GravityUp: TVector3;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc;
  out AboveHeight: Single; out AboveGround: PTriangle): boolean;
var
  NewResult: boolean;
  NewAboveHeight: Single;
  NewAboveGround: PTriangle;
  BoxIntersection: TVector3;
begin
  if CheckCollides then
  begin
    Result := inherited LocalHeightCollision(APosition, GravityUp,
      TrianglesToIgnoreFunc, AboveHeight, AboveGround);

    if UseInternalOctreeCollisions then
    begin
      NewResult := InternalOctreeCollisions.HeightCollision(APosition, GravityUp,
        NewAboveHeight, NewAboveGround, nil, TrianglesToIgnoreFunc);
    end else
    begin
      { collide with LocalBoundingBox }
      { Using TryRayEntrance here would also be sensible, but sometimes too eager:
        In case creature walks over an item, it would cause the item to go upward
        (because the creature is collidable (item is not), so item's gravity
        would cause it to grow). Sometimes also the creatures would too easily
        climb on top of each other.
        It may be changed in the future back into TryRayEntrance? Item problems
        could be solved by using GrowSpeed = 0 for items. }
      NewResult := LocalBoundingBox.TryRayClosestIntersection(BoxIntersection, NewAboveHeight, APosition, -GravityUp);
      NewAboveGround := nil;
    end;

    if NewResult and (NewAboveHeight < AboveHeight) then
    begin
      Result := NewResult;
      AboveHeight := NewAboveHeight;
      AboveGround := NewAboveGround;
    end;
  end else
  begin
    Result := false;
    AboveHeight := MaxSingle;
    AboveGround := nil;
  end;
end;

function TCastleSceneCore.LocalBoundingVolumeMoveCollision(
  const OldPos, NewPos: TVector3;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D): boolean;
var
  MyBox: TBox3D;

  { P1 is closer to our middle than P2. }
  function CloserToMiddle(const P1, P2: TVector3): boolean;
  var
    M: TVector3;
  begin
    M := Middle;
    Result := PointsDistanceSqr(M, P1) < PointsDistanceSqr(M, P2);
  end;

var
  OldCollision, NewCollision: boolean;
begin
  { check collision with our bounding box.

    We do not look here at our own sphere. When other objects move,
    it's better to treat ourself as larger (not smaller), to prevent
    collisions rather then allow them in case of uncertainty.
    So we ignore Self.Sphere method.

    But we do take into account that other (moving) object may prefer to
    be treated as a sphere, so we take into account IsRadius, Radius parameters.
    This allows a player to climb on top of dead corpses (with flat
    bbox), since player's sphere is slightly above the ground.
    And it allows the missiles (like arrow) to use their spheres
    for determining what is hit, which is good because e.g. arrow
    has a very large bbox, sphere is much better (otherwise it may be too easy
    to hit with arrow). }

  Result := true;

  MyBox := LocalBoundingBox;

  if IsRadius then
  begin
    OldCollision := MyBox.SphereCollision(OldPos, Radius);
    NewCollision := MyBox.SphereCollision(NewPos, Radius);
  end else
  begin
    OldCollision := MyBox.Collision(OldBox);
    NewCollision := MyBox.Collision(NewBox);
  end;

  if NewCollision then
  begin
    { We now know that we have a collision with new position.
      Strictly thinking, move should be disallowed
      (we should exit with false). But it's not that simple.

      There is a weakness in collision checking with dynamic objects,
      like creatures, because when LifeTime changes then effectively
      LocalBoundingBox changes, and there is no way how I can prevent collisions
      from occuring (we cannot stop/reverse an arbitrary animation,
      this would look bad and require AI preparations, see @link(Sphere) comments).

      So we must allow some moves, to allow player/creature that is already
      stuck (already collidable with Self) to get out of the collision.
      To do this, we are going to enable a move, only if *old position
      was already collidable (so the other object is stuck with us already)
      and new position is further from us (so the other object tries
      to get unstuck)". }
    if (not OldCollision) or CloserToMiddle(NewPos, OldPos) then
      Exit(false);
  end else
  if (not OldCollision) and
     { new and old positions are Ok (not collidable), so check also
       line segment. Otherwise fast moving player could run through slim
       creature. }
     MyBox.SegmentCollision(OldPos, NewPos) then
    Exit(false);

{ Simpler implementation that doesn't allow others to become "unstuck".
  It's also slightly less optimal, as internally LocalBoundingBox and CheckCollides
  will be calculated many times (although they should be lighting-fast,
  still their time matters, as this is the basis of our AI and may be called
  many times per frame).
  OTOH, this simpler version is a little cleaner: it delegates work
  to other methods, they may use LocalBoundingBox or something else.

  if IsRadius then
    Result := not ( CheckCollides and
      ( LocalSegmentCollision(OldPos, ProposedNewPos, TrianglesToIgnoreFunc, false) or
        LocalSphereCollision(ProposedNewPos, Radius, TrianglesToIgnoreFunc) ) )
  else
    Result := not ( CheckCollides and
      ( LocalSegmentCollision(OldPos, ProposedNewPos, TrianglesToIgnoreFunc, false) or
        LocalBoxCollision(NewBox, TrianglesToIgnoreFunc) ) );
}
end;

function TCastleSceneCore.LocalMoveCollision(
  const OldPos, ProposedNewPos: TVector3; out NewPos: TVector3;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;
begin
  Result := true;
  NewPos := ProposedNewPos;

  if CheckCollides then
  begin
    if UseInternalOctreeCollisions then
    begin
      Result := InternalOctreeCollisions.MoveCollision(OldPos, ProposedNewPos, NewPos,
        IsRadius, Radius, OldBox, NewBox, nil, TrianglesToIgnoreFunc);
    end else
    begin
      { no wall-sliding with bounding volume }
      Result := LocalBoundingVolumeMoveCollision(OldPos, ProposedNewPos,
        IsRadius, Radius, OldBox, NewBox);
    end;
    if not Result then Exit;

    { no wall-sliding in inherited }
    Result := inherited LocalMoveCollision(OldPos, ProposedNewPos,
      IsRadius, Radius, OldBox, NewBox, TrianglesToIgnoreFunc);
  end;
end;

function TCastleSceneCore.LocalMoveCollision(
  const OldPos, NewPos: TVector3;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;
begin
  Result := true;

  if CheckCollides then
  begin
    if UseInternalOctreeCollisions then
    begin
      Result := InternalOctreeCollisions.MoveCollision(OldPos, NewPos,
        IsRadius, Radius, OldBox, NewBox, nil, TrianglesToIgnoreFunc);
    end else
    begin
      Result := LocalBoundingVolumeMoveCollision(OldPos, NewPos,
        IsRadius, Radius, OldBox, NewBox);
    end;
    if not Result then Exit;

    Result := inherited LocalMoveCollision(OldPos, NewPos,
      IsRadius, Radius, OldBox, NewBox, TrianglesToIgnoreFunc);
  end;
end;

function TCastleSceneCore.LocalSegmentCollision(const Pos1, Pos2: TVector3;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc;
  const ALineOfSight: boolean): boolean;
begin
  Result := false;

  if CheckCollides or (ALineOfSight and Exists) then
  begin
    if UseInternalOctreeCollisions then
    begin
      Result := InternalOctreeCollisions.IsSegmentCollision(
        Pos1, Pos2, nil, false, TrianglesToIgnoreFunc)
    end else
    begin
      Result := LocalBoundingBox.SegmentCollision(Pos1, Pos2);
    end;
    if Result then Exit;

    Result := inherited LocalSegmentCollision(Pos1, Pos2, TrianglesToIgnoreFunc, ALineOfSight);
  end;
end;

function TCastleSceneCore.LocalSphereCollision(
  const Pos: TVector3; const Radius: Single;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;
begin
  Result := false;

  if CheckCollides then
  begin
    if UseInternalOctreeCollisions then
    begin
      Result := InternalOctreeCollisions.IsSphereCollision(
        Pos, Radius, nil, TrianglesToIgnoreFunc)
    end else
    begin
      Result := LocalBoundingBox.SphereCollision(Pos, Radius);
    end;
    if Result then Exit;

    Result := inherited LocalSphereCollision(Pos, Radius, TrianglesToIgnoreFunc);
  end;
end;

function TCastleSceneCore.LocalSphereCollision2D(
  const Pos: TVector2; const Radius: Single;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc;
  const Details: TCollisionDetails): boolean;
begin
  Result := false;

  if CheckCollides then
  begin
    if UseInternalOctreeCollisions then
    begin
      Result := InternalOctreeCollisions.IsSphereCollision2D(
        Pos, Radius, nil, TrianglesToIgnoreFunc);
    end else
    begin
      Result := LocalBoundingBox.SphereCollision2D(Pos, Radius);
    end;

    if Result then
    begin
      if Details <> nil then
      begin
        Details.Clear;
        Details.Add(Self);
      end;
      Exit;
    end;

    Result := inherited LocalSphereCollision2D(Pos, Radius, TrianglesToIgnoreFunc, Details);
  end;
end;

function TCastleSceneCore.LocalPointCollision2D(const Point: TVector2;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;
begin
  Result := false;

  if CheckCollides then
  begin
    if UseInternalOctreeCollisions then
    begin
      Result := InternalOctreeCollisions.IsPointCollision2D(
        Point, nil, TrianglesToIgnoreFunc);
    end else
    begin
      Result := LocalBoundingBox.Contains2D(Point);
    end;
    if Result then Exit;

    Result := inherited LocalPointCollision2D(Point, TrianglesToIgnoreFunc);
  end;
end;

function TCastleSceneCore.LocalBoxCollision(const Box: TBox3D;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;
begin
  Result := false;

  if CheckCollides then
  begin
    if UseInternalOctreeCollisions then
    begin
      Result := InternalOctreeCollisions.IsBoxCollision(
        Box,  nil, TrianglesToIgnoreFunc);
    end else
    begin
      Result := LocalBoundingBox.Collision(Box);
    end;
    if Result then Exit;

    Result := inherited LocalBoxCollision(Box, TrianglesToIgnoreFunc);
  end;
end;

function TCastleSceneCore.LocalRayCollision(const RayOrigin, RayDirection: TVector3;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): TRayCollision;
var
  Triangle: PTriangle;
  Intersection: TVector3;
  IntersectionDistance: Single;
  NewNode: PRayCollisionNode;
  NewResult: TRayCollision;
begin
  Result := nil;

  if CheckPickable then
  begin
    if UseInternalOctreeCollisions then
    begin
      Triangle := InternalOctreeCollisions.RayCollision(
        Intersection, IntersectionDistance, RayOrigin, RayDirection,
        { ReturnClosestIntersection } true,
        { TriangleToIgnore } nil,
        { IgnoreMarginAtStart } false, TrianglesToIgnoreFunc);
      if Triangle <> nil then
      begin
        Result := TRayCollision.Create;
        Result.Distance := IntersectionDistance;
        NewNode := PRayCollisionNode(Result.Add);
        NewNode^.Item := Self;
        NewNode^.Point := Intersection;
        NewNode^.Triangle := Triangle;
        NewNode^.RayOrigin := RayOrigin;
        NewNode^.RayDirection := RayDirection;
      end;
    end else
    begin
      if LocalBoundingBox.TryRayEntrance(Intersection, IntersectionDistance, RayOrigin, RayDirection) then
      begin
        Result := TRayCollision.Create;
        Result.Distance := IntersectionDistance;

        NewNode := PRayCollisionNode(Result.Add);
        NewNode^.Item := Self;
        NewNode^.Point := Intersection;
        NewNode^.Triangle := nil;
        NewNode^.RayOrigin := RayOrigin;
        NewNode^.RayDirection := RayDirection;
      end;
    end;

    NewResult := inherited LocalRayCollision(RayOrigin, RayDirection, TrianglesToIgnoreFunc);
    if NewResult <> nil then
    begin
      if (Result = nil) or (NewResult.Distance < Result.Distance) then
      begin
        SysUtils.FreeAndNil(Result);
        Result := NewResult;
      end else
        FreeAndNil(NewResult);
    end;
  end;
end;
