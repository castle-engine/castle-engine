{
  Copyright 2017-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Implements the game logic. }
unit Game;

interface

implementation

uses SysUtils, Classes, Generics.Collections, Kraft,
  CastleWindow, CastleScene, CastleControls, CastleLog, X3DNodes, Castle3D,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors,
  CastleCameras, CastleVectors, CastleRenderer, CastleBoxes;

type
  { Shape used for collision detection of a rigid body,
    placed in @link(TRigidBody.Collider) property. }
  TCollider = class
  end;

  TRigidBody = class
  public
    { Disable motion (@link(T3DTransform.Translation) change) along
      the particular (world) axis.

      For 2D games, you will usually want to disable motion along the Z axis.
      You can do this comfortably by calling @link(Setup2D). }
    LockPosition: set of 0..2;

    { Disable rotation (@link(T3DTransform.Rotation) change) along
      the particular (world) axis.
      You can do this comfortably by calling @link(Setup2D). }
    LockRotation: set of 0..2;

    { Mass in kg. }
    Mass: Single;

    { Whether this object is affected by gravity. }
    Gravity: boolean;

    Collider: TCollider;

    { Utility function to set common values for physics in 2D games.
      Locks moving along the Z axis,
      locks rotating along the X and Y axes. }
    procedure Setup2D;
  end;

  TPhysicsTransform = class(T3DTransform)
  strict private
    FRigidBody: TRigidBody;
  public
    { Participate in rigid body physics simulation.
      This makes this object collidable with other rigid bodies
      (if @link(TRigidBody.Collider) is assigned)
      and it allows to move and rotate because of gravity
      or because of collisions with other objects
      (if @link(TRigidBody.Static) is @false).

      Setting this makes this object a single rigid body for the physics engine.

      If this property is assigned and the @link(TRigidBody.Static) is @false
      then this object is moved and rotated using the physics engine.
      It will move because of gravity (if @link(TRigidBody.Gravity)),
      and because of collisions with other objects.

      Usually you want to also assign a @link(TRigidBody.Collider) instance.
      Otherwise, the rigid body will not collide with anything,
      and (if not @link(TRigidBody.Static)) it will simply fall down because of gravity.

      @bold(This is an experimental API, subject to change in future releases.)

      @bold(Our engine (for now) also has internal, simple physics simulation,
      used to perform collisions with player, creatures, and optional gravity.)
      These two physics systems are independent, and use separate properties for now.

      @unorderedList(
        @item(@link(T3D.Collides) property has no effect on whether this
          object is collidable for the physics engine.
          The @link(T3D.Collides) only determines whether
          the object is collidable for our internal collision detection
          (that doesn't use physics engine) which is used for collisions with
          player (camera), creatures, and for our internal gravity (@link(T3DTransform.Gravity)).
        )

        @item(@link(T3DTransform.Gravity) property has no effect on whether this
          object is affected by gravity simulation of the physics engine.
          The physics engine is controlled by independent @link(TRigidBody.Gravity) property,
          which is @true by default (while the @link(T3DTransform.Gravity) is @false
          by default).

          It doesn't make sense to set @link(T3DTransform.Gravity) to @true
          if also use @link(TRigidBody.Gravity), it would mean that
          @italic(gravity simulation is performed twice).
          In the future, @link(T3DTransform.Gravity) may be removed
          (or merged with @link(TRigidBody.Gravity), but then old games
          may by surprised by a new default @true.)
        )

        @item(The collider used by our internal, simple physics is independent
          from the @link(TRigidBody.Collider).
          The internal, simple physics uses colliders implicitly implemented
          in the overridden @link(T3D.HeightCollision),
          @link(T3D.MoveCollision) and friends.
          In case of @link(TCastleSceneCore), the rule is simple:

          @unorderedList(
            @item(If the @link(TCastleSceneCore.Spatial) contains
              ssDynamicCollisions or ssStaticCollisions, then the object collides
              as a mesh. This makes precise collisions with all the triangles.
              Any mesh, convex or concave, is correctly solved.)
            @item(Otherwise, the object collides as it's axis-aligned bounding box.)
          )
        )
      )
    }
    property RigidBody: TRigidBody read FRigidBody write FRigidBody;
  end;

  TPhysicsTransformList = specialize TObjectList<TPhysicsTransform>;

procedure TRigidBody.Setup2D;
begin
  LockPosition := [2];
  LockRotation := [0, 1];
end;

var
  Window: TCastleWindow;
  Level: TPhysicsTransform;
  BoxTemplate, SphereTemplate: TCastleScene;
  DynamicTransforms: TPhysicsTransformList;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
var
  LevelScene: TCastleScene;
  MoveLimit: TBox3D;
begin
  LevelScene := TCastleScene.Create(Application);
  LevelScene.Load(ApplicationData('level.x3dv'));
  LevelScene.Spatial := [ssRendering, ssDynamicCollisions];
  LevelScene.ProcessEvents := true;
  LevelScene.Attributes.Shaders := srAlways; // nicer lighting

  Level := TPhysicsTransform.Create(Application);
  Level.Add(LevelScene);

  Window.SceneManager.Items.Add(Level);
  Window.SceneManager.MainScene := LevelScene;

  // make gravity work even if your position is over the world bbox
  MoveLimit := Window.SceneManager.Items.BoundingBox;
  MoveLimit.Max := MoveLimit.Max + Vector3(0, 1000, 0);
  Window.SceneManager.MoveLimit := MoveLimit;

  // rotating by dragging would cause trouble when clicking to spawn boxes/spheres
  Window.SceneManager.NavigationType := ntWalk;
  Window.SceneManager.Camera.Input :=
    Window.SceneManager.Camera.Input - [ciMouseDragging];

  DynamicTransforms := TPhysicsTransformList.Create(false);

  BoxTemplate := TCastleScene.Create(Application);
  BoxTemplate.Load(ApplicationData('box.x3d'));

  SphereTemplate := TCastleScene.Create(Application);
  SphereTemplate.Load(ApplicationData('sphere.x3d'));
end;

procedure WindowRender(Container: TUIContainer);
begin
  UIFont.PrintStrings(10, 10, Yellow, [
    Format('FPS: %f', [Container.Fps.RealTime]),
    'Left mouse button - spawn box',
    'Right mouse button - spawn sphere',
    'AWSD, arrows - move, rotate',
    'F4 - toggle mouse look'
  ], false, 0);
end;

procedure WindowUpdate(Container: TUIContainer);
begin
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);

  procedure Spawn(const Template: TCastleScene);
  var
    Scene: TCastleScene;
    Transform: TPhysicsTransform;
    CameraPos, CameraDir, CameraUp: TVector3;
  begin
    Scene := Template.Clone(Application);

    Transform := TPhysicsTransform.Create(Application);
    Window.SceneManager.Camera.GetView(CameraPos, CameraDir, CameraUp);
    Transform.Translation := CameraPos + CameraDir * 2;
    // TODO: apply Transform.Direction from Window.SceneManager.Camera.Direction
    // This code will be much simpler once we merge various T3D descendants
    // into TCastleTransform,
    // and make TCastleScene descend from TCastleTransform.
    Transform.Add(Scene);

    DynamicTransforms.Add(Transform);

    Window.SceneManager.Items.Add(Transform);
  end;

var
  C: TWalkCamera;
begin
  if Event.IsKey(K_F4) then
  begin
    C := Window.SceneManager.WalkCamera;
    C.MouseLook := not C.MouseLook;
  end;

  if Event.IsMouseButton(mbLeft) then
    Spawn(BoxTemplate);

  if Event.IsMouseButton(mbRight) then
    Spawn(SphereTemplate);
end;

function MyGetApplicationName: string;
begin
  Result := 'physics_3d_demo';
end;

initialization
  { This sets SysUtils.ApplicationName.
    It is useful to make sure it is correct (as early as possible)
    as our log routines use it. }
  OnGetApplicationName := @MyGetApplicationName;

  InitializeLog;

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;
  Window.OnRender := @WindowRender;
  Window.OnUpdate := @WindowUpdate;
  Window.OnPress := @WindowPress;
finalization
  FreeAndNil(DynamicTransforms);
end.
