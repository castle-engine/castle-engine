{
  Copyright 2023-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Unsupported X3D nodes.
  These nodes are defined to follow the latest X3D specification,
  and we can perfectly read and write them in X3D, but nothing more.
  These nodes do not cause any functionality (rendering, interaction, etc.)
  in Castle Game Engine @italic(for now).
  So there's no point using them in Pascal application (unless you'd want
  to generate X3D files to be consumed by other applications, but this is
  not a common CGE use-case).

  We deliberately define them in this internal unit, not in X3DNodes unit,
  to limit their usage by mistake.
  E.g. we don't want developers to accidentally use (unsupported)
  TTextureProjectorNode in their code, inatead of (supported)
  TProjectedTextureCoordinateNode. }
unit CastleInternalNodesUnsupported;

interface

uses X3DNodes, X3DFields, CastleVectors, CastleStringUtils, CastleColors,
  CastleTimeUtils, CastleUtils, CastleBoxes;

{$define read_interface}

type
  {$I x3dnodes_standard_geospatial.inc}
  {$I x3dnodes_standard_dis.inc}
  {$I x3dnodes_standard_layering.inc}
  {$I x3dnodes_standard_layout.inc}
  {$I x3dnodes_standard_rigidbodyphysics.inc}
  {$I x3dnodes_standard_picking.inc}
  {$I x3dnodes_standard_followers.inc}
  {$I x3dnodes_standard_particlesystems.inc}
  {$I x3dnodes_standard_textureprojector.inc}

{$undef read_interface}

procedure RegisterUnsupportedNodes;

implementation

uses SysUtils;

{$define read_implementation}

{$I x3dnodes_standard_geospatial.inc}
{$I x3dnodes_standard_dis.inc}
{$I x3dnodes_standard_layering.inc}
{$I x3dnodes_standard_layout.inc}
{$I x3dnodes_standard_rigidbodyphysics.inc}
{$I x3dnodes_standard_picking.inc}
{$I x3dnodes_standard_followers.inc}
{$I x3dnodes_standard_particlesystems.inc}
{$I x3dnodes_standard_textureprojector.inc}

procedure RegisterUnsupportedNodes;
begin
  RegisterDISNodes;
  RegisterGeospatialNodes;
  RegisterLayeringNodes;
  RegisterLayoutNodes;
  RegisterRigidBodyPhysicsNodes;
  RegisterPickingNodes;
  RegisterFollowersNodes;
  RegisterParticleSystemsNodes;
  RegisterTextureProjectorNodes;
end;

end.