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
  TCastleAbstractRootTransform = class;
  TCastleTransform = class;
  TRenderingCamera = class;
  TRigidBody = class;
  TCastleRigidBody = class;
  TPhysicsProperties = class;
  TCastleCamera = class;

  TCastleTransformClass = class of TCastleTransform;

  ECannotAddToAnotherWorld = class(Exception);
  ETransformParentUndefined = class(Exception);
  EMultipleReferencesInWorld = class(ETransformParentUndefined);
  ENotAddedToWorld = class(ETransformParentUndefined);
  EPhysicsError = class(Exception);

  TRenderFromViewFunction = procedure (const RenderingCamera: TRenderingCamera) of object;

  { Describe what visible thing changed for TCastleTransform.VisibleChangeHere. }
  TVisibleChange = (
    { Visible geometry (actual 3D shape) changed.
      It's not used when only light conditions, materials, textures
      and such changed.

      In practice, this means that the depth buffer from some point in space
      changed. Which means that shadow maps should be regenerated. }
    vcVisibleGeometry,

    { Something visible, but not the geometry shape, changes.
      For example, material or texture on a visible surface changed. }
    vcVisibleNonGeometry
  );
  TVisibleChanges = set of TVisibleChange;

  TVisibleChangeEvent = procedure (const Sender: TCastleTransform; const Changes: TVisibleChanges) of object;

  { Various things that TCastleTransform.PrepareResources may prepare. }
  TPrepareResourcesOption = (
    { Prepare resources for rendering *this* scene
      (on which TCastleTransform.PrepareResources is called). }
    prRenderSelf,
    { Prepare resources for rendering clones of the scene.
      E.g. textures, which are shared by clones using the cache. }
    prRenderClones,
    prBackground,
    prBoundingBox,
    prShadowVolume,
    { Prepare octrees (determined by things like TCastleSceneCore.Spatial). }
    prSpatial,
    prScreenEffects);
  TPrepareResourcesOptions = set of TPrepareResourcesOption;

  { Shadow volumes helper, not depending on OpenGL. }
  TBaseShadowVolumeRenderer = class
  end;

  TCastleTransformList = class;

  { Information about ray collision with a single 3D object.
    Everything (Point, RayOrigin, RayDirection) is expressed
    in the parent coordinate system of this TCastleTransform (in @link(Item)). }
  TRayCollisionNode = record
  public
    { Colliding object. }
    Item: TCastleTransform;

    { Position, in local coordinate system of this 3D object,
      of the picked 3D point.

      If the ray hit empty space (@link(Triangle) field is @nil),
      then this is undefined.
      Such case may occur because, once TCastleTransform
      handles TCastleTransform.PointingDevicePress,
      we make sure to inform it about PointingDeviceMove and PointingDeviceRelease,
      regardless if ray still hits this TCastleTransform instance. }
    Point: TVector3;

    { Triangle that was hit. This triangle is always a part of @link(Item).

      If the ray hit empty space then this is @nil.
      Such case may occur because, once TCastleTransform
      handles TCastleTransform.PointingDevicePress,
      we make sure to inform it about PointingDeviceMove and PointingDeviceRelease,
      regardless if ray still hits this TCastleTransform instance.

      May also be @nil if RayCollision implementation for the 3D object
      simply left it @nil. For example,

      @unorderedList(
        @item(If the collision occured with a TCastleSceneCore with
          @link(TCastleSceneCore.Spatial) that includes a collidable
          structure (ssDynamicCollisions or ssStaticCollisions),
          then this triangle is set.)

        @item(If the collision occured merely with a TCastleTransform bounding box,
          the triangle is left as @nil.)
      )
    }
    Triangle: PTriangle;

    { Ray used to cause the collision,
      in the parent coordinate system of this TCastleTransform.
      RayDirection is @italic(not) necessarily normalized! }
    RayOrigin, RayDirection: TVector3;
  end;
  PRayCollisionNode = ^TRayCollisionNode;

  { Represents a ray collision with TCastleTransform (TCastleTransform that
    may have parents).

    We advise to access the useful information using only the @link(Transform)
    or @link(Info) methods or read @link(Distance) field.

    Everything else is somewhat internal information and things get complicated
    if you try to make sense of it. It's a TODO: to maybe deprecate this structure
    in favor of something simpler.

    The full description is that this is a list, from the innermost TCastleTransform
    that was hit by the ray,
    to the root of the TCastleTransform tree.
    The @code(First) item is the collision information
    with the final TCastleTransform. The rest of items are parent,
    grandparent etc. of this TCastleTransform object
    (so it is a path within @link(TCastleViewport.Items) hierarchy tree).

    But be aware that CGE uses some internal TCastleTransform instances.
    E.g. TCastleBox, TCastleTiledMap have internal TCastleScene instances.
    These internal components are marked with csTransient flag in ComponentState.

    We, again, advise to not iterate over this list or access e.g. first
    item directly. Use only @link(Transform)
    or @link(Info) methods or read @link(Distance) field! Things are simple then.

    This is never an empty list when returned by RayCollision. }
  TRayCollision = class({$ifdef FPC}specialize{$endif} TStructList<TRayCollisionNode>)
  public
    { Distance, in world coordinate system, from the current
      camera to the picked point. The suggested usage is to decide if player
      is close enough to reach the 3D object --- for example, you may not
      want to allow player to open a door by clicking on it from a far distance.

      If the ray hit empty space, the distance is MaxSingle.
      Such case may occur because, once TCastleTransform
      handles TCastleTransform.PointingDevicePress,
      we make sure to inform it about PointingDeviceMove and PointingDeviceRelease,
      regardless if ray still hits this TCastleTransform instance. }
    Distance: Single;

    { Index of node with given Item, -1 if none. }
    function IndexOfItem(const Item: TCastleTransform): Integer; overload;

    { Index of node with given ItemClass, -1 if none. }
    function IndexOfItem(const ItemClass: TCastleTransformClass): Integer; overload;

    { TCastleTransform that was hit by given ray.

      To be precise, this returns the inner-most TCastleTransform
      in the TCastleTransform tree,
      except it doesn't return internal TCastleTransform (that has csTransient
      flag is ComponentState).
      This way it doesn't return internal components, like internal TCastleScene
      inside TCastleBox or TCastleTiledMap.

      Returns @nil if nothing was hit (Count is zero). }
    function Transform: TCastleTransform;

    { Returns information about what was hit by given ray.

      To be precise, this returns the first TRayCollisionNode on the list
      except it doesn't return TRayCollisionNode that relates to
      internal TCastleTransform (that has csTransient
      flag is ComponentState).
      This way it doesn't return internal components, like internal TCastleScene
      inside TCastleBox or TCastleTiledMap.

      Returns @false if nothing was hit (Count is zero),
      HitInfo is undefined in this case. }
    function Info(out HitInfo: TRayCollisionNode): Boolean;
  end;

  { Detailed information about collision with a single 3D object. }
  TCollisionDetailsItem = record
  public
    { Colliding 3D object. }
    Item: TCastleTransform;
  end;
  PCollisionDetailsItem = ^TCollisionDetailsItem;

  { Represents a collision with a 3D objects tree.
    Just access the @code(First) item for the collision information
    with the final 3D object. The rest of items are containers of this 3D
    object (a path within @link(TCastleViewport.Items) hierarchy tree,
    usually).

    This list is a path in the TCastleTransform tree leading from the
    final colliding object to the root of the TCastleTransform tree.
    This allows you to track the TCastleTransform
    containers that contain given collision.

    This is never an empty list when returned by XxxCollision method. }
  TCollisionDetails = class({$ifdef FPC}specialize{$endif} TStructList<TCollisionDetailsItem>)
  public
    { Index of node with given Item. }
    function IndexOfItem(const Item: TCastleTransform): Integer;
    procedure Add(const Item: TCastleTransform); reintroduce;
  end;

  TRemoveType = (rtNone, rtRemove, rtRemoveAndFree);

  { Orientation of the model is 3D world, determining where is
    the conceptual "up" direction of the model,
    and where is it facing.

    This type is used by the @link(TCastleTransform.Orientation)
    and @link(TCastleTransform.DefaultOrientation).

    Orientation determines how @link(TCastleTransform.Direction),
    @link(TCastleTransform.Up) work, i.e. what is
    @link(TCastleTransform.Direction) and @link(TCastleTransform.Up)
    relation to @link(TCastleTransform.Rotation).
    The names of the enum values describe what is the direction/up
    when rotation is zero. }
  TOrientationType = (
    { Model up is +Y axis,
      direction is -Z which matches cameras of glTF, OpenGL, X3D.

      This also matches default export from Blender to X3D, with Blender "front"
      pointing toward -Z in the exported model.
      (But it mismatches default export from Blender to glTF in this regard.)

      Gravity pulls in -Y and GravityUp vector is +Y.
      Transformation makes -Z and +Y match (respectively) Direction and Up.

      For example, using this value for @link(TCastleTransform.Orientation) (or even
      @link(TCastleTransform.DefaultOrientation)) is sensible if you use default
      Blender X3D exporter, and you let the exporter to make
      a default transformation (to make +Z up into +Y up).
      Then you can follow the standard Blender view names
      ("front", "top" and such) when modelling, and Blender tools like
      "X-axis mirror" will work best. }
    otUpYDirectionMinusZ,

    { Model up is +Y axis,
      direction is +Z which matches export from Blender to glTF or Wavefront OBJ.
      This matches glTF specification that explicitly says "The front of a glTF asset faces +Z".

      Gravity pulls in -Y and GravityUp vector is +Y.
      Transformation makes +Z and +Y match (respectively) Direction and Up.

      This does not match default direction of OpenGL, X3D and glTF cameras.
      When viewed from the default direction of OpenGL, X3D and glTF cameras,
      you will see the front of the model,
      which means that the model's direction is the opposite of the camera direction.

      For example, using this value for @link(TCastleTransform.Orientation) (or even
      @link(TCastleTransform.DefaultOrientation)) is sensible if you use default
      Blender glTF or OBJ exporter, and you let the exporter to make
      a default transformation (to make +Z up into +Y up). This is the default setting.
      Then you can follow the standard Blender view names
      ("front", "top" and such) when modelling, and Blender tools like
      "X-axis mirror" will work best. }
    otUpYDirectionZ,

    { Orientation sensible for models oriented around Z axis.
      Transformation makes -Y and +Z match (respectively) Direction and Up.

      Using this value for @link(TCastleTransform.Orientation) (or even
      @link(TCastleTransform.DefaultOrientation)) is sensible if you export your models
      from Blender to X3D @italic(without transforming them during export).
      Note that @italic(this is not the default Blender X3D exporter behavior).
      But you can configure the exporter to work like this (not transform),
      and then you can follow the standard Blender view names
      ("front", "top" and such) when modelling. }
    otUpZDirectionMinusY,

    { @deprecated Up in +Z (like otUpZDirectionMinusY) and direction
      in +X. Should not be used in new models. }
    otUpZDirectionX
  );

{$endif read_interface}

{$ifdef read_implementation}

{ TRayCollision --------------------------------------------------------------- }

function TRayCollision.IndexOfItem(const Item: TCastleTransform): Integer;
begin
  for Result := 0 to Count - 1 do
    if L[Result].Item = Item then Exit;
  Result := -1;
end;

function TRayCollision.IndexOfItem(const ItemClass: TCastleTransformClass): Integer;
begin
  for Result := 0 to Count - 1 do
    if L[Result].Item is ItemClass then Exit;
  Result := -1;
end;

function TRayCollision.Transform: TCastleTransform;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := L[I].Item;
    if not (csTransient in Result.ComponentStyle) then
      Exit;
  end;

  // Return nil if all items on MouseRayHit list are csTransient
  Result := nil;
end;

(*
// Equivalent implementation, just a tiny bit slower as it copies TRayCollisionNode without need
function TRayCollision.Transform: TCastleTransform;
var
  HitInfo: TRayCollisionNode;
begin
  if Info(HitInfo) then
    Result := HitInfo.Item
  else
    Result := nil;
end;
*)

function TRayCollision.Info(out HitInfo: TRayCollisionNode): Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if not (csTransient in L[I].Item.ComponentStyle) then
    begin
      HitInfo := L[I];
      Exit(true);
    end;
  end;

  // Return false if all items on MouseRayHit list are csTransient
  Result := false;
end;

{ TCollisionDetails ------------------------------------------------------------ }

function TCollisionDetails.IndexOfItem(const Item: TCastleTransform): Integer;
begin
  for Result := 0 to Count - 1 do
    if L[Result].Item = Item then Exit;
  Result := -1;
end;

procedure TCollisionDetails.Add(const Item: TCastleTransform);
var
  NewItem: PCollisionDetailsItem;
begin
  NewItem := PCollisionDetailsItem(inherited Add());
  NewItem^.Item := Item;
end;

{$endif read_implementation}
