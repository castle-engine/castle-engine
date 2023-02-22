{
  Copyright 2010-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Group and transform scenes (TCastleTransform). }
unit CastleTransform;

{$I castleconf.inc}

interface

uses SysUtils, Classes, Math, Generics.Collections, Contnrs, Kraft,
  CastleVectors, CastleFrustum, CastleBoxes, CastleClassUtils, CastleKeysMouse,
  CastleRectangles, CastleUtils, CastleTimeUtils, CastleComponentSerialize,
  CastleSoundEngine, CastleTriangles, CastleRenderOptions, CastleProjection,
  CastleUIControls, CastleQuaternions, CastleColors, CastleInternalClassUtils;

type
  {$define read_interface}
  {$I castletransform_initial_types.inc}
  {$I castletransform_renderparams.inc}
  {$I castletransform_behavior.inc}
  {$I castletransform_transformlist.inc}
  {$I castletransform_transform.inc}
  {$I castletransform_abstractroottransform.inc}
  {$I castletransform_design.inc}
  {$I castletransform_reference.inc}
  {$I castletransform_camera.inc}

{$I castletransform_physics.inc}
{$I castletransform_physics_deprecated.inc}
{$I castletransform_joints.inc}
{$I castletransform_joints_experimental.inc}
{$I castletransform_serialize.inc}
{$I castletransform_miscellaneous_globals.inc}
{$I castletransform_camera_utils.inc}
{$undef read_interface}

implementation

uses CastleLog, CastleApplicationProperties, CastleURIUtils, CastleInternalRays,
  CastleRenderContext,
  // TODO: this breaks unit dependencies, transform->scene
  X3DNodes, CastleScene, CastleInternalPhysicsVisualization;

{$define read_implementation}
{$I castletransform_initial_types.inc}
{$I castletransform_renderparams.inc}
{$I castletransform_physics.inc}
{$I castletransform_physics_deprecated.inc}
{$I castletransform_joints.inc}
{$I castletransform_joints_experimental.inc}
{$I castletransform_collisions.inc}
{$I castletransform_behavior.inc}
{$I castletransform_serialize.inc}
{$I castletransform_transformlist.inc}
{$I castletransform_transform.inc}
{$I castletransform_abstractroottransform.inc}
{$I castletransform_design.inc}
{$I castletransform_reference.inc}
{$I castletransform_camera.inc}
{$I castletransform_miscellaneous_globals.inc}
{$I castletransform_camera_utils.inc}
{$undef read_implementation}

var
  R: TRegisteredComponent;
initialization
  TCastleTransform.DefaultOrientation := otUpYDirectionZ;
  TCastleCollider.AutoSizeMinThickness := 0.01;
  TCastleCollider.AutoSizeMinThickness2D := 1;
  TCastleCollider.AutoSizeMinThickness2DDepth := 100;
  GlobalIdentityMatrix := TMatrix4.Identity;

  RegisterSerializableComponent(TCastleTransform, 'Transform');
  RegisterSerializableComponent(TCastleTransformDesign, 'Transform Design (Use Another castle-transform File)');
  RegisterSerializableComponent(TCastleTransformReference, 'Reference Another Transform');
  RegisterSerializableComponent(TCastleCamera, 'Camera');

  RegisterSerializableComponent(TCastleRigidBody, ['Physics', 'Rigid Body']);

  R := TRegisteredComponent.Create;
  R.ComponentClass := TCastleRigidBody;
  R.Caption := ['Physics', 'Rigid Body (2D)'];
  R.OnCreate := {$ifdef FPC}@{$endif}TCastleRigidBody.CreateComponent2D;
  RegisterSerializableComponent(R);

  RegisterSerializableComponent(TCastleBoxCollider, ['Physics', 'Collider', 'Box']);

  R := TRegisteredComponent.Create;
  R.ComponentClass := TCastleBoxCollider;
  R.Caption := ['Physics', 'Collider 2D', 'Box'];
  R.OnCreate := {$ifdef FPC}@{$endif}TCastleBoxCollider.CreateComponent2D;
  RegisterSerializableComponent(R);

  RegisterSerializableComponent(TCastleCapsuleCollider, ['Physics', 'Collider', 'Capsule']);

  R := TRegisteredComponent.Create;
  R.ComponentClass := TCastleCapsuleCollider;
  R.Caption := ['Physics', 'Collider 2D', 'Capsule'];
  R.OnCreate := {$ifdef FPC}@{$endif}TCastleCapsuleCollider.CreateComponent2D;
  RegisterSerializableComponent(R);

  RegisterSerializableComponent(TCastlePlaneCollider, ['Physics', 'Collider', 'Plane']);

  R := TRegisteredComponent.Create;
  R.ComponentClass := TCastlePlaneCollider;
  R.Caption := ['Physics', 'Collider 2D', 'Plane'];
  R.OnCreate := {$ifdef FPC}@{$endif}TCastlePlaneCollider.CreateComponent2D;
  RegisterSerializableComponent(R);

  RegisterSerializableComponent(TCastleSphereCollider, ['Physics', 'Collider', 'Sphere']);

  R := TRegisteredComponent.Create;
  R.ComponentClass := TCastleSphereCollider;
  R.Caption := ['Physics', 'Collider 2D', 'Sphere'];
  R.OnCreate := {$ifdef FPC}@{$endif}TCastleSphereCollider.CreateComponent2D;
  RegisterSerializableComponent(R);

  RegisterSerializableComponent(TCastleMeshCollider, ['Physics', 'Collider', 'Mesh']);

  R := TRegisteredComponent.Create;
  R.ComponentClass := TCastleMeshCollider;
  R.Caption := ['Physics', 'Collider 2D', 'Mesh'];
  R.OnCreate := {$ifdef FPC}@{$endif}TCastleMeshCollider.CreateComponent2D;
  RegisterSerializableComponent(R);

  RegisterSerializableComponent(TCastleRopeJoint, ['Physics', 'Joint', 'Rope']);
  RegisterSerializableComponent(TCastleHingeJoint, ['Physics', 'Joint', 'Hinge ']);
  RegisterSerializableComponent(TCastleBallJoint, ['Physics', 'Joint', 'Ball']);
  RegisterSerializableComponent(TCastleDistanceJoint, ['Physics', 'Joint', 'Distance']);
  RegisterSerializableComponent(TCastleGrabJoint, ['Physics', 'Joint', 'Grab']);
  {$ifdef CASTLE_EXPERIMENTAL_JOINTS}
  RegisterSerializableComponent(TCastleFixedJoint, ['Physics', 'Joint', 'Fixed']);
  RegisterSerializableComponent(TCastlePulleyJoint, ['Physics', 'Joint', 'Pulley']);
  RegisterSerializableComponent(TCastleWorldPlaneDistanceJoint, ['Physics', 'Joint', 'World Plane Distance']);
  RegisterSerializableComponent(TCastleSliderJoint, ['Physics', 'Joint', 'Slider']);
  {$endif CASTLE_EXPERIMENTAL_JOINTS}
end.
