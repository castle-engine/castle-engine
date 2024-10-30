{%MainUnit castletransform.pas}
{
  Copyright 2010-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Implementation of all XxxCollision methods in CastleTransform unit. }

{ TCastleTransform -------------------------------------------------------------------- }

function TCastleTransform.LocalHeightCollision(const APosition, GravityUp: TVector3;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc;
  out AboveHeight: Single; out AboveGround: PTriangle): boolean;
var
  I: Integer;
  NewResult: boolean;
  NewAboveHeight: Single;
  NewAboveGround: PTriangle;
begin
  Result := false;
  AboveHeight := MaxSingle;
  AboveGround := nil;

  if CheckCollides then
  begin
    for I := 0 to List.Count - 1 do
    begin
      NewResult := List[I].HeightCollision(APosition, GravityUp, TrianglesToIgnoreFunc,
        NewAboveHeight, NewAboveGround);

      if NewAboveHeight < AboveHeight then
      begin
        Result := NewResult;
        AboveHeight := NewAboveHeight;
        AboveGround := NewAboveGround;
      end;
    end;
  end;
end;

function TCastleTransform.LocalMoveCollision(
  const OldPos, ProposedNewPos: TVector3; out NewPos: TVector3;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;
var
  I: Integer;
begin
  Result := true;
  NewPos := ProposedNewPos;

  if CheckCollides then
  begin
    { We call MoveCollision with separate ProposedNewPos and NewPos
      only on the first scene.
      This means that only first scene collisions provide wall sliding.
      Collisions with other 3D objects will simply block the player.

      Otherwise, various MoveCollision could modify NewPos
      making it colliding with other items, already checked. This would
      be wrong.

      TODO: this could be improved, to call MoveCollision
      with separate ProposedNewPos and NewPos
      on the first scene
      where the simple move is not allowed. This would make it more general,
      although also slower. Is there any way to make it as fast and
      more general? }

    if List.Count <> 0 then
    begin
      Result := List[0].MoveCollision(OldPos, ProposedNewPos, NewPos,
        IsRadius, Radius, OldBox, NewBox, TrianglesToIgnoreFunc);
      if not Result then Exit;

      for I := 1 to List.Count - 1 do
      begin
        Result := List[I].MoveCollision(OldPos, NewPos,
          IsRadius, Radius, OldBox, NewBox, TrianglesToIgnoreFunc);
        if not Result then Exit;
      end;
    end;
  end;
end;

function TCastleTransform.LocalMoveCollision(
  const OldPos, NewPos: TVector3;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;
var
  I: Integer;
begin
  Result := true;

  if CheckCollides then
  begin
    for I := 0 to List.Count - 1 do
    begin
      Result := List[I].MoveCollision(OldPos, NewPos,
        IsRadius, Radius, OldBox, NewBox, TrianglesToIgnoreFunc);
      if not Result then Exit;
    end;
  end;
end;

function TCastleTransform.LocalSegmentCollision(const Pos1, Pos2: TVector3;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc;
  const ALineOfSight: boolean): boolean;
var
  I: Integer;
begin
  Result := false;

  if CheckCollides or (ALineOfSight and Exists) then
  begin
    for I := 0 to List.Count - 1 do
    begin
      Result := List[I].SegmentCollision(Pos1, Pos2, TrianglesToIgnoreFunc, ALineOfSight);
      if Result then Exit;
    end;
  end;
end;

function TCastleTransform.LocalSphereCollision(const Pos: TVector3; const Radius: Single;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;
var
  I: Integer;
begin
  Result := false;

  if CheckCollides then
  begin
    for I := 0 to List.Count - 1 do
    begin
      Result := List[I].SphereCollision(Pos, Radius, TrianglesToIgnoreFunc);
      if Result then Exit;
    end;
  end;
end;

function TCastleTransform.LocalSphereCollision2D(const Pos: TVector2; const Radius: Single;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc;
  const Details: TCollisionDetails): boolean;
var
  I: Integer;
begin
  Result := false;

  if CheckCollides then
  begin
    for I := 0 to List.Count - 1 do
    begin
      Result := List[I].SphereCollision2D(Pos, Radius, TrianglesToIgnoreFunc, Details);
      if Result then
      begin
        if Details <> nil then
          Details.Add(Self);
        Exit;
      end;
    end;
  end;
end;

function TCastleTransform.LocalPointCollision2D(const Point: TVector2;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;
var
  I: Integer;
begin
  Result := false;

  if CheckCollides then
  begin
    for I := 0 to List.Count - 1 do
    begin
      Result := List[I].PointCollision2D(Point, TrianglesToIgnoreFunc);
      if Result then Exit;
    end;
  end;
end;

function TCastleTransform.LocalBoxCollision(const Box: TBox3D;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;
var
  I: Integer;
begin
  Result := false;

  if CheckCollides then
  begin
    for I := 0 to List.Count - 1 do
    begin
      Result := List[I].BoxCollision(Box, TrianglesToIgnoreFunc);
      if Result then Exit;
    end;
  end;
end;

function TCastleTransform.LocalRayCollision(const RayOrigin, RayDirection: TVector3;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): TRayCollision;

  procedure AddNewResult(NewResult: TRayCollision);
  begin
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

var
  I: Integer;

  NewNode, PreviousNode: PRayCollisionNode;
begin
  Result := nil;

  if CheckPickable then
  begin
    for I := 0 to List.Count - 1 do
      AddNewResult(List[I].RayCollision(RayOrigin, RayDirection, TrianglesToIgnoreFunc));

    if Result <> nil then
    begin
      NewNode := PRayCollisionNode(Result.Add);
      PreviousNode := PRayCollisionNode(Result.Ptr(Result.Count - 2));
      NewNode^.Item := Self;
      NewNode^.Point := PreviousNode^.Point;
      NewNode^.Triangle := nil;
      NewNode^.RayOrigin := RayOrigin;
      NewNode^.RayDirection := RayDirection;
    end;
  end;
end;

{ TCastleTransform ----------------------------------------------------------- }

function TCastleTransform.HeightCollision(const APosition, GravityUp: TVector3;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc;
  out AboveHeight: Single; out AboveGround: PTriangle): boolean;
var
  MInverse: TMatrix4;
begin
  { LocalHeightCollision will check these anyway. But by checking them here,
    we can potentially avoid the cost of transforming into local space. }
  if not CheckCollides then
  begin
    Result := false;
    AboveHeight := MaxSingle;
    AboveGround := nil;
    Exit;
  end;

  if FOnlyTranslation then
    Result := LocalHeightCollision(
      APosition - Translation, GravityUp, TrianglesToIgnoreFunc,
      AboveHeight, AboveGround) else
  begin
    MInverse := InverseTransform;
    Result := LocalHeightCollision(
      MInverse.MultPoint(APosition),
      MInverse.MultDirection(GravityUp), TrianglesToIgnoreFunc,
        AboveHeight, AboveGround);
    { Note that we should not scale resulting AboveHeight by AverageScale.
      That is because AboveHeight is relative to GravityUp length,
      so it's automatically correct. }
  end;
end;

function TCastleTransform.MoveCollision(
  const OldPos, ProposedNewPos: TVector3; out NewPos: TVector3;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;
var
  T: TVector3;
  Tr: TTransformation;
begin
  { LocalMoveCollision will check these anyway. But by checking them here,
    we can potentially avoid the cost of transforming into local space. }
  if not CheckCollides then
  begin
    NewPos := ProposedNewPos;
    Exit(true);
  end;

  if FOnlyTranslation then
  begin
    T := Translation;
    Result := LocalMoveCollision(
      OldPos         - T,
      ProposedNewPos - T, NewPos,
      IsRadius, Radius,
      OldBox.AntiTranslate(T),
      NewBox.AntiTranslate(T), TrianglesToIgnoreFunc);
    { translate calculated NewPos back }
    if Result then
      NewPos := NewPos + T;
  end else
  begin
    InternalTransformation(Tr);
    Result := LocalMoveCollision(
      Tr.InverseTransform.MultPoint(OldPos),
      Tr.InverseTransform.MultPoint(ProposedNewPos), NewPos,
      IsRadius, Radius / AverageScale,
      OldBox.Transform(Tr.InverseTransform),
      NewBox.Transform(Tr.InverseTransform), TrianglesToIgnoreFunc);
    { transform calculated NewPos back }
    if Result then
      NewPos := Tr.Transform.MultPoint(NewPos);
  end;
end;

function TCastleTransform.MoveCollision(
  const OldPos, NewPos: TVector3;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;
var
  T: TVector3;
  MInverse: TMatrix4;
begin
  { LocalMoveCollision will check these anyway. But by checking them here,
    we can potentially avoid the cost of transforming into local space. }
  if not CheckCollides then Exit(true);

  if FOnlyTranslation then
  begin
    { I have to check collision between
        Items + Translation and (OldPos, NewPos).
      So it's equivalent to checking for collision between
        Items and (OldPos, NewPos) - Translation
      And this way I can use LocalMoveCollision. }
    T := Translation;
    Result := LocalMoveCollision(
      OldPos - T,
      NewPos - T,
      IsRadius, Radius,
      OldBox.AntiTranslate(T),
      NewBox.AntiTranslate(T), TrianglesToIgnoreFunc);
  end else
  begin
    MInverse := InverseTransform;
    Result := LocalMoveCollision(
      MInverse.MultPoint(OldPos),
      MInverse.MultPoint(NewPos),
      IsRadius, Radius / AverageScale,
      OldBox.Transform(MInverse),
      NewBox.Transform(MInverse), TrianglesToIgnoreFunc);
  end;
end;

function TCastleTransform.SegmentCollision(const Pos1, Pos2: TVector3;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc;
  const ALineOfSight: boolean): boolean;
var
  T: TVector3;
  MInverse: TMatrix4;
begin
  { LocalSegmentCollision will check these anyway. But by checking them here,
    we can potentially avoid the cost of transforming into local space. }
  if not (CheckCollides or (ALineOfSight and Exists)) then Exit(false);

  if FOnlyTranslation then
  begin
    T := Translation;
    Result := LocalSegmentCollision(Pos1 - T, Pos2 - T, TrianglesToIgnoreFunc, ALineOfSight);
  end else
  begin
    MInverse := InverseTransform;
    Result := LocalSegmentCollision(
      MInverse.MultPoint(Pos1),
      MInverse.MultPoint(Pos2), TrianglesToIgnoreFunc, ALineOfSight);
  end;
end;

function TCastleTransform.SphereCollision(
  const Pos: TVector3; const Radius: Single;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;
begin
  { LocalSphereCollision will check these anyway. But by checking them here,
    we can potentially avoid the cost of transforming into local space. }
  if not CheckCollides then Exit(false);

  if FOnlyTranslation then
    Result := LocalSphereCollision(
      Pos - Translation, Radius, TrianglesToIgnoreFunc) else
    Result := LocalSphereCollision(
      InverseTransform.MultPoint(Pos), Radius / AverageScale, TrianglesToIgnoreFunc);
end;

function TCastleTransform.SphereCollision2D(
  const Pos: TVector2; const Radius: Single;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc;
  const Details: TCollisionDetails): boolean;
begin
  { LocalSphereCollision2D will check these anyway. But by checking them here,
    we can potentially avoid the cost of transforming into local space. }
  if not CheckCollides then Exit(false);

  if FOnlyTranslation then
    Result := LocalSphereCollision2D(
      Pos - TranslationXY, Radius, TrianglesToIgnoreFunc, Details) else
    Result := LocalSphereCollision2D(
      InverseTransform.MultPoint(Pos), Radius / AverageScale2D, TrianglesToIgnoreFunc, Details);
end;

function TCastleTransform.PointCollision2D(
  const Point: TVector2;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;
begin
  { LocalPointCollision2D will check these anyway. But by checking them here,
    we can potentially avoid the cost of transforming into local space. }
  if not CheckCollides then Exit(false);

  if FOnlyTranslation then
    Result := LocalPointCollision2D(
      Point - TranslationXY, TrianglesToIgnoreFunc) else
    Result := LocalPointCollision2D(
      InverseTransform.MultPoint(Point), TrianglesToIgnoreFunc);
end;

function TCastleTransform.BoxCollision(
  const Box: TBox3D;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): boolean;
begin
  { LocalBoxCollision will check these anyway. But by checking them here,
    we can potentially avoid the cost of transforming into local space. }
  if not CheckCollides then Exit(false);

  if FOnlyTranslation then
    Result := LocalBoxCollision(
      Box.AntiTranslate(Translation), TrianglesToIgnoreFunc)
  else
    Result := LocalBoxCollision(
      Box.Transform(InverseTransform), TrianglesToIgnoreFunc);
end;

function TCastleTransform.OutsideToLocal(const Pos: TVector3): TVector3;
begin
  if FOnlyTranslation then
    Result := Pos - Translation
  else
    Result := InverseTransform.MultPoint(Pos);
end;

function TCastleTransform.LocalToOutside(const Pos: TVector3): TVector3;
begin
  if FOnlyTranslation then
    Result := Pos + Translation
  else
    Result := Transform.MultPoint(Pos);
end;

function TCastleTransform.WorldToLocal(const Pos: TVector3): TVector3;
begin
  Result := WorldInverseTransform.MultPoint(Pos);
end;

function TCastleTransform.LocalToWorld(const Pos: TVector3): TVector3;
begin
  Result := WorldTransform.MultPoint(Pos);
end;

function TCastleTransform.WorldToLocalDirection(const Dir: TVector3): TVector3;
begin
  Result := WorldInverseTransform.MultDirection(Dir);
end;

function TCastleTransform.LocalToWorldDirection(const Dir: TVector3): TVector3;
begin
  Result := WorldTransform.MultDirection(Dir);
end;

function TCastleTransform.LocalToWorldDistance(const Distance: Single): Single;
var
  S: Single;
begin
  S := Approximate3DScale(WorldTransform);
  Result := Distance * S;
end;

function TCastleTransform.WorldToLocalDistance(const Distance: Single): Single;
var
  S: Single;
begin
  S := 1 / Approximate3DScale(WorldTransform);

  { In principle, this should be the same:
  S := Approximate3DScale(WorldInverseTransform);
    Except there may be some edge cases in case of e.g. zero scale.
    So WorldInverseTransform and WorldTransform do not transform everything perfectly
    in both directions without any loss.
    And we want to satisfy the common expectation that
    LocalToWorldDistance and WorldToLocalDistance invert each other. }

  Result := Distance * S;
end;

function TCastleTransform.InternalRayCollision(const RayOrigin, RayDirection: TVector3): TRayCollision;
begin
  Result := RayCollision(RayOrigin, RayDirection, nil);
end;

function TCastleTransform.RayCollision(const RayOrigin, RayDirection: TVector3;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): TRayCollision;
var
  T: TVector3;
  LastNode: PRayCollisionNode;
  Tr: TTransformation;
begin
  { LocalRayCollision will check these anyway. But by checking them here,
    we can potentially avoid the cost of transforming into local space. }
  if not CheckPickable then Exit(nil);

  if FOnlyTranslation then
  begin
    T := Translation;
    Result := LocalRayCollision(RayOrigin - T, RayDirection, TrianglesToIgnoreFunc);
    if Result <> nil then
    begin
      LastNode := PRayCollisionNode(Result.Ptr(Result.Count - 1));
      LastNode^.Point := LastNode^.Point + T;
      { untransform the ray }
      LastNode^.RayOrigin := RayOrigin;
      LastNode^.RayDirection := RayDirection;
    end;
  end else
  begin
    InternalTransformation(Tr);
    Result := LocalRayCollision(
      Tr.InverseTransform.MultPoint(RayOrigin),
      Tr.InverseTransform.MultDirection(RayDirection), TrianglesToIgnoreFunc);
    if Result <> nil then
    begin
      LastNode := PRayCollisionNode(Result.Ptr(Result.Count - 1));
      LastNode^.Point := Tr.Transform.MultPoint(LastNode^.Point);
      { untransform the ray }
      LastNode^.RayOrigin := RayOrigin;
      LastNode^.RayDirection := RayDirection;

      { Note that we should not scale Result.Distance by AverageScale.
        That is because Result.Distance is relative to RayDirection length,
        so it's automatically correct. }
    end;
  end;
end;
