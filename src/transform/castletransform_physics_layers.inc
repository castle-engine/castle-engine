{%MainUnit castletransform.pas}
{
  Copyright 2023-2023 Michalis Kamburelis, Andrzej Kilijański.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Physics layers. Must be defined early, as method declarations use AllLayers. }

{$ifdef read_interface}

type
  { Each physics rigid body is a specific layer,
    @link(TCastleRigidBody.Layer).
    It is configurable if objects at different layers collide or not,
    and routines @link(TCastleRigidBody.PhysicsRayCast) allow to specify
    which layers are checked. }
  TPhysicsLayer = 0..19;

  TPhysicsLayers = set of TPhysicsLayer;

const
  AllLayers = [Low(TPhysicsLayer)..High(TPhysicsLayer)];

{$endif read_interface}

