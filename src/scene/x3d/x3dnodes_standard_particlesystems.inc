{%MainUnit x3dnodes.pas}
{
  Copyright 2008-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { Type of @link(TParticleSystemNode.GeometryType).
    See X3D specification about the possible geometry types for particles:
    https://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/particle_systems.html#ParticleSystem }
  TParticlesGeometry = (pgLine, pgPoint, pgQuad, pgSprite, pgTriangle, pgGeometry);

  { }
  TAbstractParticleEmitterNode = class(TAbstractNode)
    {$I auto_generated_node_helpers/x3dnodes_x3dparticleemitternode.inc}
  end;

  TAbstractParticlePhysicsModelNode = class(TAbstractNode)
    {$I auto_generated_node_helpers/x3dnodes_x3dparticlephysicsmodelnode.inc}
  end;

  TBoundedPhysicsModelNode = class(TAbstractParticlePhysicsModelNode)
    {$I auto_generated_node_helpers/x3dnodes_boundedphysicsmodel.inc}
  end;

  TConeEmitterNode = class(TAbstractParticleEmitterNode)
    {$I auto_generated_node_helpers/x3dnodes_coneemitter.inc}
  end;

  TExplosionEmitterNode = class(TAbstractParticleEmitterNode)
    {$I auto_generated_node_helpers/x3dnodes_explosionemitter.inc}
  end;

  TForcePhysicsModelNode = class(TAbstractParticlePhysicsModelNode)
    {$I auto_generated_node_helpers/x3dnodes_forcephysicsmodel.inc}
  end;

  TParticleSystemNode = class(TAbstractShapeNode)
    {$I auto_generated_node_helpers/x3dnodes_particlesystem.inc}
  end;

  TPointEmitterNode = class(TAbstractParticleEmitterNode)
    {$I auto_generated_node_helpers/x3dnodes_pointemitter.inc}
  end;

  TPolylineEmitterNode = class(TAbstractParticleEmitterNode)
    {$I auto_generated_node_helpers/x3dnodes_polylineemitter.inc}
  end;

  TSurfaceEmitterNode = class(TAbstractParticleEmitterNode)
    {$I auto_generated_node_helpers/x3dnodes_surfaceemitter.inc}
  end;

  TVolumeEmitterNode = class(TAbstractParticleEmitterNode)
    {$I auto_generated_node_helpers/x3dnodes_volumeemitter.inc}
  end;

  TWindPhysicsModelNode = class(TAbstractParticlePhysicsModelNode)
    {$I auto_generated_node_helpers/x3dnodes_windphysicsmodel.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

const
  ParticlesGeometryNames: array [TParticlesGeometry] of String =
  ('LINE', 'POINT', 'QUAD', 'SPRITE', 'TRIANGLE', 'GEOMETRY');

procedure RegisterParticleSystemsNodes;
begin
  NodesManager.RegisterNodeClasses([
    TBoundedPhysicsModelNode,
    TConeEmitterNode,
    TExplosionEmitterNode,
    TForcePhysicsModelNode,
    TParticleSystemNode,
    TPointEmitterNode,
    TPolylineEmitterNode,
    TSurfaceEmitterNode,
    TVolumeEmitterNode,
    TWindPhysicsModelNode
  ]);
end;

{$endif read_implementation}
