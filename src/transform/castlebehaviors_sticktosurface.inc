{%MainUnit castlebehaviors.pas}
{
  Copyright 2022-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

type
  { Behavior to make parent TCastleTransform stick to a surface of another
    TCastleTransform @link(Target).
    The example use-case is to stick things to the surface of the terrain underneath. }
  TCastleStickToSurface = class(TCastleBehavior)
  strict private
    FTarget: TCastleTransform;
    FTargetObserver: TFreeNotificationObserver;
    FDirection: TVector3;
    FOnlyAtDesign: Boolean;
    DoneWarningTerrainChild, DoneWarningExpectedTarget: Boolean;
    procedure SetTarget(const Value: TCastleTransform);
    procedure TargetFreeNotification(const Sender: TFreeNotificationObserver);
  protected
    function CanAttachToParent(const NewParent: TCastleTransform;
      out ReasonWhyCannot: String): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Direction in which we check for collisions to stick the parent to @link(Target).
      The default direction is -Y (that is, (0, 1, 0)) which makes the parent
      stick to the object under it, as if the gravity was forcing it down immediately. }
    property Direction: TVector3 read FDirection write FDirection;
  published
    { Target to which Parent of this behavior sticks.

      TODO: Rotating the target right now doesn't make proper behavior,
      test by rotating terrain in examples/terrain/. }
    property Target: TCastleTransform read FTarget write SetTarget;

    { Perform stickiness only at design-time.
      This is @true by default and makes this behavior trivially optimized
      at runtime. }
    property OnlyAtDesign: Boolean read FOnlyAtDesign write FOnlyAtDesign default true;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlesticktosurface_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlesticktosurface_persistent_vectors.inc}
{$undef read_implementation_methods}

constructor TCastleStickToSurface.Create(AOwner: TComponent);
begin
  inherited;
  FOnlyAtDesign := true;
  FDirection := Vector3(0, -1, 0);

  FTargetObserver := TFreeNotificationObserver.Create(Self);
  FTargetObserver.OnFreeNotification := {$ifdef FPC}@{$endif} TargetFreeNotification;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlesticktosurface_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleStickToSurface.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastlesticktosurface_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

function TCastleStickToSurface.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'DirectionPersistent', 'Target', 'OnlyAtDesign'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleStickToSurface.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);

  procedure WarningTerrainChild;
  begin
    WritelnWarningOnce(DoneWarningTerrainChild, 'TCastleStickToSurface "%s" with parent "%s": Testing for collision with Target "%s" resulted in a collision with ourselves, aborting. ' +
      'The object with TCastleStickToSurface should not be a child of Target.', [
      Name,
      Parent.Name,
      Target.Name
    ]);
  end;

  procedure WarningExpectedTarget;
  begin
    WritelnWarningOnce(DoneWarningExpectedTarget, 'TCastleStickToSurface "%s" with parent "%s": Testing for collision with Target "%s" resolved, but Target not on colliders list', [
      Name,
      Parent.Name,
      Target.Name
    ]);
  end;

  { Find a position of collision point with Target.
    Returned position is in the coordinate system of Target.Parent
    (or world, if Target.Parent = nil).

    Returns @false if a weird/invalid setup detected and we should
    abort trying to stick to surface. }
  function FindPositionInTarget(const RayCollision: TRayCollision;
    out PositionInTarget: TVector3): Boolean;
  var
    I: Integer;
  begin
    for I := 0 to RayCollision.Count - 1 do
    begin
      if RayCollision[I].Item = Parent then
      begin
        WarningTerrainChild;
        Exit(false);
      end;

      if RayCollision[I].Item = Target then
      begin
        PositionInTarget := RayCollision[I].Point;
        Exit(true);
      end;
    end;

    WarningExpectedTarget;
    Exit(false);
  end;

var
  RayDirection, RayOrigin, Collision, TranslationToTarget: TVector3;
  RayCollision: TRayCollision;
begin
  inherited;

  Assert(Parent <> nil);

  { We update Parent in each @link(Update).
    This is simplest: this way it works when anything changes (Parent,
    Parent's Parent..., Target, Target's Parent...).
    And it doesn't work when Parent does not exist,
    i.e. we don't perform this merely when Direction/Target property changes.

    Disadvantage:
    - This is not efficient, we don't check whether anything changed,
      we always calculate collision here, every frame.
    - This may have a visible delay, user may see previous position
      when the object was not stuck to Target.

    These are both not a problem when using this only at design-time. }

  if (CastleDesignMode or not OnlyAtDesign) and
     (Target <> nil) and
     Parent.HasWorldTransform and
     (Parent.Parent <> nil) and
     Target.HasWorldTransform then
  begin
    { Convert Direction to Target coordinate system }
    RayDirection := Target.WorldToLocalDirection(
      Parent.LocalToWorldDirection(Direction));

    { Get suitable ray position, outside of Target bbox,
      in Target coordinate system }
    TranslationToTarget := Target.WorldToLocal(
      Parent.LocalToWorld(TVector3.Zero));
    RayOrigin := TranslationToTarget - RayDirection * Target.LocalBoundingBox.Diagonal;

    RayCollision := Target.InternalRayCollision(RayOrigin, RayDirection);
    if RayCollision <> nil then
    try
      if not FindPositionInTarget(RayCollision, Collision) then
        Exit;
      { Convert collision from Target.Parent coordinate system to world.
        Testcase: move Terrain in examples/terrain/, trees should follow. }
      if Target.Parent <> nil then
        Collision := Target.Parent.LocalToWorld(Collision);
      { Convert collision from world to Parent.Parent }
      Collision := Parent.Parent.WorldToLocal(Collision);
      Parent.Translation := Collision;
    finally FreeAndNil(RayCollision) end;
  end;
end;

function TCastleStickToSurface.CanAttachToParent(const NewParent: TCastleTransform;
  out ReasonWhyCannot: String): Boolean;
begin
  Result := inherited;
  if not Result then Exit;

  if NewParent.FindBehavior(TCastleStickToSurface) <> nil then
  begin
    ReasonWhyCannot := 'Only one TCastleStickToSurface behavior can be added to a given TCastleTransform';
    Result := false;
  end;
end;

procedure TCastleStickToSurface.SetTarget(const Value: TCastleTransform);
begin
  if FTarget <> Value then
  begin
    FTarget := Value;
    FTargetObserver.Observed := Value;
    DoneWarningExpectedTarget := false;
    DoneWarningTerrainChild := false;
  end;
end;

procedure TCastleStickToSurface.TargetFreeNotification(const Sender: TFreeNotificationObserver);
begin
  // set property to nil when the referenced component is freed
  Target := nil;
end;

{$endif read_implementation}
