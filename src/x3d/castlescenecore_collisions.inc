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

{ Implementation of all XxxCollision methods in CastleSceneCore unit. }

{ TCastleSceneCore ----------------------------------------------------------- }

function TCastleSceneCore.HeightCollision(const APosition, GravityUp: TVector3;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
  out AboveHeight: Single; out AboveGround: P3DTriangle): boolean;
var
  NewResult: boolean;
  NewAboveHeight: Single;
  NewAboveGround: P3DTriangle;
  BoxIntersection: TVector3;
begin
  if GetCollides then
  begin
    Result := inherited HeightCollision(APosition, GravityUp,
      TrianglesToIgnoreFunc, AboveHeight, AboveGround);

    if UseInternalOctreeCollisions then
    begin
      NewResult := InternalOctreeCollisions.HeightCollision(APosition, GravityUp,
        NewAboveHeight, PTriangle(NewAboveGround), nil, TrianglesToIgnoreFunc);
    end else
    begin
      { collide with BoundingBox }
      { Using TryRayEntrance here would also be sensible, but sometimes too eager:
        In case creature walks over an item, it would cause the item to go upward
        (because the creature is collidable (item is not), so item's gravity
        would cause it to grow). Sometimes also the creatures would too easily
        climb on top of each other.
        It may be changed in the future back into TryRayEntrance? Item problems
        could be solved by using GrowSpeed = 0 for items. }
      NewResult := BoundingBox.TryRayClosestIntersection(BoxIntersection, NewAboveHeight, APosition, -GravityUp);
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

function TCastleSceneCore.BoundingVolumeMoveCollision(
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

  MyBox := BoundingBox;

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
      BoundingBox changes, and there is no way how I can prevent collisions
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
  It's also slightly less optimal, as internally BoundingBox and GetCollides
  will be calculated many times (although they should be lighting-fast,
  still their time matters, as this is the basis of our AI and may be called
  many times per frame).
  OTOH, this simpler version is a little cleaner: it delegates work
  to other methods, they may use BoundingBox or something else.

  if IsRadius then
    Result := not ( GetCollides and
      ( SegmentCollision(OldPos, ProposedNewPos, TrianglesToIgnoreFunc, false) or
        SphereCollision(ProposedNewPos, Radius, TrianglesToIgnoreFunc) ) ) else
    Result := not ( GetCollides and
      ( SegmentCollision(OldPos, ProposedNewPos, TrianglesToIgnoreFunc, false) or
        BoxCollision(NewBox, TrianglesToIgnoreFunc) ) );
}
end;

function TCastleSceneCore.MoveCollision(
  const OldPos, ProposedNewPos: TVector3; out NewPos: TVector3;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := true;
  NewPos := ProposedNewPos;

  if GetCollides then
  begin
    if UseInternalOctreeCollisions then
    begin
      Result := InternalOctreeCollisions.MoveCollision(OldPos, ProposedNewPos, NewPos,
        IsRadius, Radius, OldBox, NewBox, nil, TrianglesToIgnoreFunc);
    end else
    begin
      { no wall-sliding with bounding volume }
      Result := BoundingVolumeMoveCollision(OldPos, ProposedNewPos,
        IsRadius, Radius, OldBox, NewBox);
    end;
    if not Result then Exit;

    { no wall-sliding in inherited }
    Result := inherited MoveCollision(OldPos, ProposedNewPos,
      IsRadius, Radius, OldBox, NewBox, TrianglesToIgnoreFunc);
  end;
end;

function TCastleSceneCore.MoveCollision(
  const OldPos, NewPos: TVector3;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := true;

  if GetCollides then
  begin
    if UseInternalOctreeCollisions then
    begin
      Result := InternalOctreeCollisions.MoveCollision(OldPos, NewPos,
        IsRadius, Radius, OldBox, NewBox, nil, TrianglesToIgnoreFunc);
    end else
    begin
      Result := BoundingVolumeMoveCollision(OldPos, NewPos,
        IsRadius, Radius, OldBox, NewBox);
    end;
    if not Result then Exit;

    Result := inherited MoveCollision(OldPos, NewPos,
      IsRadius, Radius, OldBox, NewBox, TrianglesToIgnoreFunc);
  end;
end;

function TCastleSceneCore.SegmentCollision(const Pos1, Pos2: TVector3;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
  const ALineOfSight: boolean): boolean;
begin
  Result := false;

  if GetCollides or (ALineOfSight and GetExists) then
  begin
    if UseInternalOctreeCollisions then
    begin
      Result := InternalOctreeCollisions.IsSegmentCollision(
        Pos1, Pos2, nil, false, TrianglesToIgnoreFunc)
    end else
    begin
      Result := BoundingBox.SegmentCollision(Pos1, Pos2);
    end;
    if Result then Exit;

    Result := inherited SegmentCollision(Pos1, Pos2, TrianglesToIgnoreFunc, ALineOfSight);
  end;
end;

function TCastleSceneCore.SphereCollision(
  const Pos: TVector3; const Radius: Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := false;

  if GetCollides then
  begin
    if UseInternalOctreeCollisions then
    begin
      Result := InternalOctreeCollisions.IsSphereCollision(
        Pos, Radius, nil, TrianglesToIgnoreFunc)
    end else
    begin
      Result := BoundingBox.SphereCollision(Pos, Radius);
    end;
    if Result then Exit;

    Result := inherited SphereCollision(Pos, Radius, TrianglesToIgnoreFunc);
  end;
end;

function TCastleSceneCore.SphereCollision2D(
  const Pos: TVector2; const Radius: Single;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc;
  const Details: TCollisionDetails): boolean;
begin
  Result := false;

  if GetCollides then
  begin
    if UseInternalOctreeCollisions then
    begin
      Result := InternalOctreeCollisions.IsSphereCollision2D(
        Pos, Radius, nil, TrianglesToIgnoreFunc);
    end else
    begin
      Result := BoundingBox.SphereCollision2D(Pos, Radius);
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

    Result := inherited SphereCollision2D(Pos, Radius, TrianglesToIgnoreFunc, Details);
  end;
end;

function TCastleSceneCore.PointCollision2D(const Point: TVector2;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := false;

  if GetCollides then
  begin
    if UseInternalOctreeCollisions then
    begin
      Result := InternalOctreeCollisions.IsPointCollision2D(
        Point, nil, TrianglesToIgnoreFunc);
    end else
    begin
      Result := BoundingBox.Contains2D(Point);
    end;
    if Result then Exit;

    Result := inherited PointCollision2D(Point, TrianglesToIgnoreFunc);
  end;
end;

function TCastleSceneCore.BoxCollision(const Box: TBox3D;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): boolean;
begin
  Result := false;

  if GetCollides then
  begin
    if UseInternalOctreeCollisions then
    begin
      Result := InternalOctreeCollisions.IsBoxCollision(
        Box,  nil, TrianglesToIgnoreFunc);
    end else
    begin
      Result := BoundingBox.Collision(Box);
    end;
    if Result then Exit;

    Result := inherited BoxCollision(Box, TrianglesToIgnoreFunc);
  end;
end;

function TCastleSceneCore.RayCollision(const RayOrigin, RayDirection: TVector3;
  const TrianglesToIgnoreFunc: T3DTriangleIgnoreFunc): TRayCollision;
var
  Triangle: PTriangle;
  Intersection: TVector3;
  IntersectionDistance: Single;
  NewNode: PRayCollisionNode;
  NewResult: TRayCollision;
begin
  Result := nil;

  if GetPickable then
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
        NewNode := Result.Add;
        NewNode^.Item := Self;
        NewNode^.Point := Intersection;
        NewNode^.Triangle := Triangle;
        NewNode^.RayOrigin := RayOrigin;
        NewNode^.RayDirection := RayDirection;
      end;
    end else
    begin
      if BoundingBox.TryRayEntrance(Intersection, IntersectionDistance, RayOrigin, RayDirection) then
      begin
        Result := TRayCollision.Create;
        Result.Distance := IntersectionDistance;

        NewNode := Result.Add;
        NewNode^.Item := Self;
        NewNode^.Point := Intersection;
        { better T3D implementation could assign here something nice to NewNode^.Triangle,
          to inform T3D.PointingDeviceMove/Activate about the intersected material. }
        NewNode^.Triangle := nil;
        NewNode^.RayOrigin := RayOrigin;
        NewNode^.RayDirection := RayDirection;
      end;
    end;

    NewResult := inherited RayCollision(RayOrigin, RayDirection, TrianglesToIgnoreFunc);
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
