{%MainUnit castletransform.pas}
{
  Copyright 2017-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

//type
  { Result of @link(TCastleAbstractRootTransform.PhysicsRayCast) and
    @link(TCastleRigidBody.PhysicsRayCast). }
  TPhysicsRayCastResult = record
    { Other fields are guaranteed to be all zero if Hit = @false. }
    Hit: Boolean;
    Transform: TCastleTransform;
    { Distance from ray origin to the hit point. }
    Distance: Single;
    Point, Normal: TVector3;
  end;

  { Root of transformations and scenes (tree of TCastleTransform and TCastleScene).
    This is the base abstract class, the non-abstract descendant is @link(TCastleRootTransform). }
  TCastleAbstractRootTransform = class(TCastleTransform)
  strict private
    WasPhysicsStep: boolean;
    TimeAccumulator: TFloatTime;
    FEnablePhysics: boolean;
    FMoveLimit: TBox3D;
    FPhysicsProperties: TPhysicsProperties;
    UpdateFrameId: TFrameId;
    FTimeScale: Single;
    FPaused: boolean;
    FMainCamera: TCastleCamera;
    FMainCameraObserver: TFreeNotificationObserver;
    FInternalPressReleaseListeners: TCastleTransformList;
    procedure SetPaused(const Value: boolean);
    procedure SetMainCamera(const Value: TCastleCamera);
    procedure MainCameraFreeNotification(const Sender: TFreeNotificationObserver);
    { Check all conditions (EnablePhysics, CastleApplicationMode) to find out
      is physics running. }
    function IsPhysicsRunning: Boolean;
  private
    FKraftEngine: TKraft;
    { Create FKraftEngine, if not assigned yet. }
    procedure InitializePhysicsEngine;
    { Destroy everything related to physics, if present. }
    procedure DestroyPhysicsEngine;
    procedure RegisterPressRelease(const T: TCastleTransform);
    procedure UnregisterPressRelease(const T: TCastleTransform);
  protected
    { Ignore (return @true) transparent triangles
      (with Material.Transparency > 0).

      This is suitable for TTriangleIgnoreFunc function, you can pass
      this to RayCollision and such. }
    function IgnoreTransparentItem(
      const Sender: TObject; const Triangle: PTriangle): Boolean; virtual; abstract;
  public
    OnCursorChange: TNotifyEvent;

    { Event to render whole world.
      Used by generated textures to update their contents.
      @exclude }
    InternalRenderEverythingEvent: TRenderFromViewFunction;

    { Projection near/far used. May be used when updating generated textures,
      to determine their projection parameters.
      @exclude }
    InternalProjectionNear, InternalProjectionFar: Single;

    { Changes every time MainCamera vectors change.
      Allows to track camera changes in scenes.
      @exclude }
    InternalMainCameraStateId: TFrameId;

    { Latest frame when VisibleChangeNotification was called (with any params, even empty). }
    InternalVisibleStateId: TFrameId;
    { Latest frame when VisibleChangeNotification was called (with vcVisibleGeometry). }
    InternalVisibleGeometryStateId: TFrameId;
    { Latest frame when VisibleChangeNotification was called (with vcVisibleNonGeometry). }
    InternalVisibleNonGeometryStateId: TFrameId;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { @exclude
      TCastleTransform instances that listen on press/release.
      May be @nil, equivalent to empty. }
    property InternalPressReleaseListeners: TCastleTransformList read FInternalPressReleaseListeners;

    { The major axis of gravity vector: 0, 1 or 2.
      This is trivially derived from the known camera
      GravityUp value. It can only truly express
      GravityUp vector values (1,0,0) or (0,1,0) or (0,0,1),
      although in practice this is enough for normal games (normal 3D scenes
      use up either +Y or +Z).

      We try to avoid using it in
      the engine, and use full GravityUp vector wherever possible.
      Full GravityUp vector may allow for more fun with weird gravity
      in future games. }
    function GravityCoordinate: Integer;

    function GravityUp: TVector3;
      // TODO: I would like to deprecate it,
      // but it is so often useful, and the alternative MainCamera.GravityUp looks convoluted.
      // Leave it be for now.
      // TODO: deprecated 'use MainCamera.GravityUp if MainCamera <> nil';

    { Is the move from OldPos to ProposedNewPos possible.
      Returns true and sets NewPos if some move is allowed, thus allows for wall-sliding.

      This checks collisions with world
      (everything inside this @link(TCastleAbstractRootTransform)).

      This checks collision with all objects that have @link(Collides)
      and @link(Exists) equal @true (using @link(CheckCollides)).

      If your query originates from some existing TCastleTransform instance,
      you should prefer to instead use @link(TCastleTransform.MoveAllowed)
      method, that automatically prevents "collisions with yourself".

      @seealso TCastleTransform.MoveAllowed }
    function WorldMoveAllowed(
      const OldPos, ProposedNewPos: TVector3; out NewPos: TVector3;
      const IsRadius: boolean; const Radius: Single;
      const OldBox, NewBox: TBox3D;
      const BecauseOfGravity: boolean): boolean; overload;

    { Is the move from OldPos to NewPos possible.

      This checks collisions with world
      (everything inside this @link(TCastleAbstractRootTransform)).

      This checks collision with all objects that have @link(Collides)
      and @link(Exists) equal @true (using @link(CheckCollides)).

      If your query originates from some existing TCastleTransform instance,
      you should prefer to instead use @link(TCastleTransform.MoveAllowed)
      method, that automatically prevents "collisions with yourself".

      @seealso TCastleTransform.MoveAllowed }
    function WorldMoveAllowed(
      const OldPos, NewPos: TVector3;
      const IsRadius: boolean; const Radius: Single;
      const OldBox, NewBox: TBox3D;
      const BecauseOfGravity: boolean): boolean; overload;

    { Get height of point APosition above the world.
      Looks at current @link(MainCamera) to know the gravity direction.

      This checks collisions with world
      (everything inside this @link(TCastleAbstractRootTransform)).

      This checks collision with all objects that have @link(Collides)
      and @link(Exists) equal @true (using @link(CheckCollides)).

      If your query originates from some existing TCastleTransform instance,
      you should prefer to instead use @link(TCastleTransform.MoveAllowed)
      method, that automatically prevents "collisions with yourself". }
    function WorldHeight(const APosition: TVector3;
      out AboveHeight: Single; out AboveGround: PTriangle): boolean;

    { Check that the line segment between 2 points that not collide with anything
      (that has opaque material).

      This checks collisions with world
      (everything inside this @link(TCastleAbstractRootTransform)).

      This checks collision with all objects that have @link(Collides)
      and @link(Exists) equal @true (using @link(CheckCollides)). }
    function WorldLineOfSight(const Pos1, Pos2: TVector3): boolean;

    { What is hit by this ray.

      This checks collisions with world
      (everything inside this @link(TCastleAbstractRootTransform)).

      This checks collision with all objects that have @link(Collides)
      and @link(Exists) equal @true (using @link(CheckCollides)). }
    function WorldRay(const RayOrigin, RayDirection: TVector3): TRayCollision;

    { What is hit by this ray.
      Returns the TCastleTransform that is hit (this is the "leaf" TCastleTransform
      in the TCastleTransform tree that is hit)
      and a distance from RayOrigin to the hit point.
      Returns @nil (Distance is undefined in this case) if nothing was hit.
      Use @link(WorldRay) for a more advanced version of this, with more complicated result.
      @groupBegin }
    function WorldRayCast(const RayOrigin, RayDirection: TVector3;
      out Distance: Single): TCastleTransform; overload;
    function WorldRayCast(const RayOrigin,
      RayDirection: TVector3): TCastleTransform; overload;
    { @groupEnd }

    { Check whether something collides with axis-aligned box in 3D.

      This checks collisions with world
      (everything inside this @link(TCastleAbstractRootTransform)).

      This checks collision with all objects that have @link(Collides)
      and @link(Exists) equal @true (using @link(CheckCollides)). }
    function WorldBoxCollision(const Box: TBox3D): boolean;

    { Check whether something collides with a line segment.

      This checks collisions with world
      (everything inside this @link(TCastleAbstractRootTransform)).

      This checks collision with all objects that have @link(Collides)
      and @link(Exists) equal @true (using @link(CheckCollides)). }
    function WorldSegmentCollision(const Pos1, Pos2: TVector3): boolean;

    { Check whether something collides with a sphere.

      This checks collisions with world
      (everything inside this @link(TCastleAbstractRootTransform)).

      This checks collision with all objects that have @link(Collides)
      and @link(Exists) equal @true (using @link(CheckCollides)). }
    function WorldSphereCollision(const Pos: TVector3; const Radius: Single): boolean;

    { Check whether something collides with a sphere in 2D
      (a circle, extruded to infinity along the Z axis).

      This checks collisions with world
      (everything inside this @link(TCastleAbstractRootTransform)).

      This checks collision with all objects that have @link(Collides)
      and @link(Exists) equal @true (using @link(CheckCollides)). }
    function WorldSphereCollision2D(const Pos: TVector2; const Radius: Single;
      const Details: TCollisionDetails = nil): boolean;

    { Check whether something collides with a point in 2D
      (which is an infinite line along the Z axis in 3D).

      This checks collisions with world
      (everything inside this @link(TCastleAbstractRootTransform)).

      This checks collision with all objects that have @link(Collides)
      and @link(Exists) equal @true (using @link(CheckCollides)). }
    function WorldPointCollision2D(const Point: TVector2): boolean;

    { Cast a ray using physics engine, see what is hit.

      Returns the TPhysicsRayCastResult record.
      Read TPhysicsRayCastResult.Hit (boolean), TPhysicsRayCastResult.Transform
      (guaranteed to be @nil when TPhysicsRayCastResult.Hit is @false) and other fields
      to know what was hit.

      The given RayOrigin, RayDirection, Distance are in the world coordinate system.

      It returns TPhysicsRayCastResult, with information there
      also in the world coordinate system.

      This ignores the collider of this body IgnoreBody, if IgnoreBody is not @nil.

      Note: The @link(TCastleTransform.Pickable) property is ignored by this method,
      i.e. it considers all colliders regardless of their @link(TCastleTransform.Pickable) value.
      The @link(TCastleTransform.Pickable) is only taken into account by the routines
      of the old physics system
      (see https://castle-engine.io/physics#_old_system_for_collisions_and_gravity ).
      In the new system, a similar role will be filled by layers. }
    function PhysicsRayCast(const RayOrigin, RayDirection: TVector3;
      const MaxDistance: Single = MaxSingle;
      const IgnoreBody: TCastleRigidBody = nil): TPhysicsRayCastResult;

    { Yoo can temporarily disable physics (no transformations will be updated
      by the physics engine) by setting this property to @false. }
    property EnablePhysics: boolean read FEnablePhysics write FEnablePhysics
      default true;

    { Limit the movement allowed by @link(WorldMoveAllowed).
      Ignored when empty (default).

      This property allows to easily limit the possible places
      where player and creatures go.
      Player is honoring this if it uses @link(WorldMoveAllowed),
      in particular our @link(TCastleWalkNavigation) navigation honors it.
      Creatures honor it if they use @link(WorldMoveAllowed) for their decision. }
    property MoveLimit: TBox3D read FMoveLimit write FMoveLimit;

    { Central camera, that controls the features that require
      a single "main" camera (features that do not make sense
      with multiple cameras from multiple viewports).

      This camera controls:

      @unorderedList(
        @item(Nodes that "sense" camera like TProximitySensorNode, TBillboardNode, TLODNode.)

        @item(TCastleBillboard.)

        @item(Audio listener (controlling the spatial sound).)

        @item(Headlight position/direction, if you enable headlight by @link(TCastleRootTransform.UseHeadlight).)

        @item(When X3D nodes change Viewpoint/NavigationInfo (possible if TCastleViewport.AutoCamera),
          they apply changes to this camera.)

        @item(Rendering of TRenderedTextureNode contents (which camera this texture shows).)

        @item(At design-time: gizmo size is determined with respect to this camera.)
      )

      Note that it means that "headlight" done by
      @link(TCastleRootTransform.UseHeadlight) is assigned to one camera
      in case of multiple viewports looking at the same world.
      You cannot have a different "headlight" in each viewport,
      this would cause subtle problems since it's not how it would work in reality
      (where every light is visible in all viewports),
      e.g. mirror textures (like GeneratedCubeMapTexture)
      would need different contents in different viewpoints.

      Note that we no longer advise using @link(TCastleRootTransform.UseHeadlight)
      for headlight. It's easier to just place light as child of whatever camera
      you want, like adding TCastleDirectionalLight as child of TCastleCamera.
      See examples/viewport_and_scenes/headlight/ .
      In this case MainCamera doesn't matter, instead you explicitly make headlight
      by placing light as child of chosen camera.

      Note that features like LOD or Billboard or ProximitySensor
      need to transform this camera to scene local space.
      Which is not possible if the same scene instance
      is used multiple times (e.g. under many different TCastleTransform parents).
      If you need to use these features of TCastleScene,
      then simply do not use the same scene reference multiple times
      (instead clone the scene by @link(TCastleScene.Clone)).

      By default this is set to @link(TCastleViewport.Camera) of the @link(TCastleViewport)
      that created this @link(TCastleAbstractRootTransform) instance.
      And if something changes the @link(TCastleViewport.Camera) of that viewport,
      then this @name is also updated.
      So in simple cases (when you just have one @link(TCastleViewport)
      and add your scenes to it's already-created @link(TCastleViewport.Items))
      you don't have to do anything, it just works.
      In general, you can change this to any camera (currently used by some
      associated @link(TCastleViewport) or not),
      or @nil (in case no camera should be that "central" camera). }
    property MainCamera: TCastleCamera read FMainCamera write SetMainCamera;

    { Remove all items except TCastleCamera instances.
      This only avoids removing TCastleCamera instances that are direct children
      of this TCastleAbstractRootTransform, the TCastleCamera instances that are deeper
      (e.g. TCastleCamera inside some TCastleTransform) will still be removed. }
    procedure ClearExceptCameras;
  published
    { Adjust physics properties. }
    property PhysicsProperties: TPhysicsProperties read FPhysicsProperties;

    { Time scale used when not @link(Paused). }
    property TimeScale: Single read FTimeScale write FTimeScale {$ifdef FPC}default 1{$endif};

    { Pausing means that no events (key, mouse, update) are processed.
      So time doesn't move, and input is not processed.

      Navigation also doesn't work (this part is implemented by TCastleViewport
      and each TCastleNavigation).

      This is useful if you want to unconditionally make your world temporary
      still (for example, useful when entering some modal dialog box
      and you want the world to behave as a still background).

      @italic(See also): For other pausing methods,
      there are other methods of pausing / disabling
      some events processing for the world:

      @unorderedList(
        @item(You can set TCastleScene.TimePlaying to @false.
          This is roughly equivalent to not running their @link(Update) methods.
          This means that time will "stand still" for them,
          so their animations will not play. Although they may
          still react and change in response to mouse clicks / key presses,
          if TCastleScene.ProcessEvents.)

        @item(You can set TCastleScene.ProcessEvents to @false.
          This means that scene will not receive and process any
          key / mouse and other events (through VRML/X3D sensors).
          Some animations (not depending on VRML/X3D events processing)
          may still run, for example MovieTexture will still animate,
          if only TCastleScene.TimePlaying.)

        @item(For navigation, you can set @code(TCastleNavigation.Input := []) to ignore
          key / mouse clicks.

          Or you can set @code(TCastleNavigation.Exists) to @false,
          this is actually equivalent to what pausing does now for TCastleNavigation.
        )
      ) }
    property Paused: boolean read FPaused write SetPaused default false;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleAbstractRootTransform ------------------------------------------------------------------- }

constructor TCastleAbstractRootTransform.Create(AOwner: TComponent);
begin
  inherited;

  FPhysicsProperties := TPhysicsProperties.Create(Self);
  FPhysicsProperties.SetSubComponent(true);
  FPhysicsProperties.Name := 'PhysicsProperties';
  FPhysicsProperties.RootTransform := Self;

  FMainCameraObserver := TFreeNotificationObserver.Create(Self);
  FMainCameraObserver.OnFreeNotification := {$ifdef FPC}@{$endif}MainCameraFreeNotification;

  FTimeScale := 1;
  FMoveLimit := TBox3D.Empty;
  FEnablePhysics := true;

  { everything inside is part of this world }
  AddToWorld(Self);
end;

destructor TCastleAbstractRootTransform.Destroy;
begin
  DestroyPhysicsEngine;
  FreeAndNil(FInternalPressReleaseListeners);
  inherited;
end;

function TCastleAbstractRootTransform.GravityCoordinate: Integer;
begin
  Result := MaxAbsVectorCoord(GravityUp);
end;

function TCastleAbstractRootTransform.WorldBoxCollision(const Box: TBox3D): boolean;
begin
  Result := BoxCollision(Box, nil);
end;

function TCastleAbstractRootTransform.WorldSegmentCollision(const Pos1, Pos2: TVector3): boolean;
begin
  Result := SegmentCollision(Pos1, Pos2, nil, false);
end;

function TCastleAbstractRootTransform.WorldSphereCollision(const Pos: TVector3;
  const Radius: Single): boolean;
begin
  Result := SphereCollision(Pos, Radius, nil);
end;

function TCastleAbstractRootTransform.WorldSphereCollision2D(const Pos: TVector2;
  const Radius: Single;
  const Details: TCollisionDetails): boolean;
begin
  Result := SphereCollision2D(Pos, Radius, nil, Details);
end;

function TCastleAbstractRootTransform.WorldPointCollision2D(const Point: TVector2): boolean;
begin
  Result := PointCollision2D(Point, nil);
end;

function TCastleAbstractRootTransform.IsPhysicsRunning: Boolean;
begin
  Result :=
    EnablePhysics and
    (FKraftEngine <> nil) and
    (CastleApplicationMode in [appRunning, appSimulation]);
end;

function TCastleAbstractRootTransform.WorldHeight(const APosition: TVector3;
  out AboveHeight: Single; out AboveGround: PTriangle): boolean;
begin
  Result := HeightCollision(APosition, GravityUp, nil,
    AboveHeight, AboveGround);
end;

function TCastleAbstractRootTransform.WorldLineOfSight(const Pos1, Pos2: TVector3): boolean;
begin
  Result := not SegmentCollision(Pos1, Pos2,
    { Ignore transparent materials, this means that creatures can see through
      glass --- even though they can't walk through it. }
    {$ifdef FPC}@{$endif} IgnoreTransparentItem,
    true);
end;

function TCastleAbstractRootTransform.WorldRay(
  const RayOrigin, RayDirection: TVector3): TRayCollision;
begin
  Result := RayCollision(RayOrigin, RayDirection, nil);
end;

function TCastleAbstractRootTransform.WorldRayCast(const RayOrigin, RayDirection: TVector3; out Distance: Single): TCastleTransform;
var
  RayColl: TRayCollision;
begin
  Result := nil;
  Distance := 0; // just to make it defined

  RayColl := WorldRay(RayOrigin, RayDirection);
  if RayColl <> nil then
  try
    if RayColl.Count <> 0 then
    begin
      Result := RayColl.First.Item;
      Distance := RayColl.Distance;
    end;
  finally FreeAndNil(RayColl) end;
end;

function TCastleAbstractRootTransform.WorldRayCast(const RayOrigin, RayDirection: TVector3): TCastleTransform;
var
  IgnoredDistance: Single;
begin
  Result := WorldRayCast(RayOrigin, RayDirection, IgnoredDistance);
end;

function TCastleAbstractRootTransform.WorldMoveAllowed(
  const OldPos, ProposedNewPos: TVector3; out NewPos: TVector3;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D;
  const BecauseOfGravity: boolean): boolean;
begin
  Result := MoveCollision(OldPos, ProposedNewPos, NewPos, IsRadius, Radius,
    OldBox, NewBox, nil);
  if Result then
    Result := MoveLimit.IsEmpty or MoveLimit.Contains(NewPos);
end;

function TCastleAbstractRootTransform.WorldMoveAllowed(
  const OldPos, NewPos: TVector3;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D;
  const BecauseOfGravity: boolean): boolean;
begin
  Result := MoveCollision(OldPos, NewPos, IsRadius, Radius,
    OldBox, NewBox, nil);
  if Result then
    Result := MoveLimit.IsEmpty or MoveLimit.Contains(NewPos);
end;

procedure TCastleAbstractRootTransform.SetPaused(const Value: boolean);
begin
  if FPaused <> Value then
  begin
    FPaused := Value;
    { TODO: update the viewport cursor when Paused changed. }
    // RecalculateCursor(Self);
  end;
end;

procedure TCastleAbstractRootTransform.SetMainCamera(const Value: TCastleCamera);
begin
  if FMainCamera <> Value then
  begin
    FMainCameraObserver.Observed := Value;
    FMainCamera := Value;
    VisibleChangeHere([]);
  end;
end;

procedure TCastleAbstractRootTransform.MainCameraFreeNotification(const Sender: TFreeNotificationObserver);
begin
  MainCamera := nil;
end;

function TCastleAbstractRootTransform.GravityUp: TVector3;
begin
  if MainCamera = nil then
    { This is only to keep deprecated GravityUp/GravityCoordinate
      sensible, even for old code that doesn't check MainCamera <> nil. }
    Result := DefaultCameraUp
  else
    Result := MainCamera.GravityUp;
end;

procedure TCastleAbstractRootTransform.RegisterPressRelease(const T: TCastleTransform);
begin
  if FInternalPressReleaseListeners = nil then
    FInternalPressReleaseListeners := TCastleTransformList.Create(false);
  FInternalPressReleaseListeners.Add(T);
end;

procedure TCastleAbstractRootTransform.UnregisterPressRelease(const T: TCastleTransform);
var
  I: Integer;
begin
  if FInternalPressReleaseListeners = nil then
    I := -1
  else
    I := FInternalPressReleaseListeners.IndexOf(T);

  if I = -1 then
  begin
    WritelnWarning('Transformation called UnregisterPressRelease, but it was not listening to Press/Release');
    Exit;
  end;

  FInternalPressReleaseListeners.Delete(I);
end;

procedure TCastleAbstractRootTransform.ClearExceptCameras;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if not (Items[I] is TCastleCamera) then
      Delete(I);
end;

function TCastleAbstractRootTransform.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'TimeScale', 'Paused', 'PhysicsProperties'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$endif read_implementation}
