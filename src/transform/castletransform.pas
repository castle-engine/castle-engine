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
{ Group and transform scenes (TCastleTransform). }
unit CastleTransform;

{$I castleconf.inc}

interface

uses SysUtils, Classes, Math, Generics.Collections, Contnrs, Kraft,
  CastleVectors, CastleFrustum, CastleBoxes, CastleClassUtils, CastleKeysMouse,
  CastleRectangles, CastleUtils, CastleTimeUtils, CastleComponentSerialize,
  CastleSoundEngine, CastleTriangles, CastleRenderOptions, CastleProjection,
  CastleUIControls, CastleQuaternions;

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
{$I castletransform_serialize.inc}
{$I castletransform_miscellaneous_globals.inc}
{$I castletransform_camera_utils.inc}
{$undef read_interface}

implementation

uses CastleLog, CastleApplicationProperties, CastleURIUtils, CastleInternalRays,
  CastleRenderContext,
  // TODO: this breaks unit dependencies, transform->scene
  X3DNodes, CastleScene;

{$define read_implementation}
{$I castletransform_initial_types.inc}
{$I castletransform_renderparams.inc}
{$I castletransform_physics.inc}
{$I castletransform_physics_deprecated.inc}
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
  GlobalIdentityMatrix := TMatrix4.Identity;

  RegisterSerializableComponent(TCastleTransform, 'Transform');
  RegisterSerializableComponent(TCastleTransformDesign, 'Transform Design (Use Another castle-transform File)');
  RegisterSerializableComponent(TCastleTransformReference, 'Reference Another Transform');
  RegisterSerializableComponent(TCastleCamera, 'Camera');

  RegisterSerializableComponent(TCastleRigidBody, 'Rigid Body');

  R := TRegisteredComponent.Create;
  R.ComponentClass := TCastleRigidBody;
  R.Caption := 'Rigid Body (2D)';
  R.OnCreate := {$ifdef FPC}@{$endif}TCastleRigidBody{$ifdef FPC}(nil){$endif}.CreateComponent2D;
  RegisterSerializableComponent(R);

  RegisterSerializableComponent(TCastleBoxCollider, 'Box Colllider');

  R := TRegisteredComponent.Create;
  R.ComponentClass := TCastleBoxCollider;
  R.Caption := 'Box Colllider (2D)';
  R.OnCreate := {$ifdef FPC}@{$endif}TCastleBoxCollider{$ifdef FPC}(nil){$endif}.CreateComponent2D;
  RegisterSerializableComponent(R);

  RegisterSerializableComponent(TCastleCapsuleCollider, 'Capsule Colllider');

  R := TRegisteredComponent.Create;
  R.ComponentClass := TCastleCapsuleCollider;
  R.Caption := 'Capsule Colllider (2D)';
  R.OnCreate := {$ifdef FPC}@{$endif}TCastleCapsuleCollider{$ifdef FPC}(nil){$endif}.CreateComponent2D;
  RegisterSerializableComponent(R);

  RegisterSerializableComponent(TCastlePlaneCollider, 'Plane Colllider');
  RegisterSerializableComponent(TCastleSphereCollider, 'Sphere Colllider');

  R := TRegisteredComponent.Create;
  R.ComponentClass := TCastleSphereCollider;
  R.Caption := 'Sphere Colllider (2D)';
  R.OnCreate := {$ifdef FPC}@{$endif}TCastleSphereCollider{$ifdef FPC}(nil){$endif}.CreateComponent2D;
  RegisterSerializableComponent(R);
end.
