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
  CastleSoundEngine, CastleCameras, CastleTriangles, CastleRenderOptions;

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

{$I castletransform_physics.inc}
{$I castletransform_serialize.inc}
{$I castletransform_miscellaneous_globals.inc}
{$undef read_interface}

implementation

uses CastleLog, CastleQuaternions, CastleApplicationProperties,
  CastleURIUtils;

{$define read_implementation}
{$I castletransform_initial_types.inc}
{$I castletransform_renderparams.inc}
{$I castletransform_physics.inc}
{$I castletransform_collisions.inc}
{$I castletransform_behavior.inc}
{$I castletransform_serialize.inc}
{$I castletransform_transformlist.inc}
{$I castletransform_transform.inc}
{$I castletransform_abstractroottransform.inc}
{$I castletransform_design.inc}
{$I castletransform_reference.inc}
{$I castletransform_miscellaneous_globals.inc}
{$undef read_implementation}

initialization
  TCastleTransform.DefaultOrientation := otUpYDirectionZ;
  GlobalIdentityMatrix := TMatrix4.Identity;
  RegisterSerializableComponent(TCastleTransform, 'Transform');
  RegisterSerializableComponent(TCastleTransformDesign, 'Transform Design (Use Another castle-transform File)');
  RegisterSerializableComponent(TCastleTransformReference, 'Reference Another Transform');
end.
