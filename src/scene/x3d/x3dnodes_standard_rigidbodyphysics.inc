{%MainUnit x3dnodes.pas}
{
  Copyright 2008-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  TRigidBodyNode = class;

  { }
  TAbstractNBodyCollidableNode = class(TAbstractChildNode)
  {$I auto_generated_node_helpers/x3dnodes_x3dnbodycollidablenode.inc}
  end;

  TAbstractNBodyCollisionSpaceNode = class(TAbstractNode)
  {$I auto_generated_node_helpers/x3dnodes_x3dnbodycollisionspacenode.inc}
  end;

  TAbstractRigidJointNode = class(TAbstractNode)
  {$I auto_generated_node_helpers/x3dnodes_x3drigidjointnode.inc}
  end;

  TBallJointNode = class(TAbstractRigidJointNode)
  {$I auto_generated_node_helpers/x3dnodes_balljoint.inc}
  end;

  TCollidableOffsetNode = class(TAbstractNBodyCollidableNode)
  {$I auto_generated_node_helpers/x3dnodes_collidableoffset.inc}
  end;

  TCollidableShapeNode = class(TAbstractNBodyCollidableNode)
  {$I auto_generated_node_helpers/x3dnodes_collidableshape.inc}
  end;

  TCollisionCollectionNode = class(TAbstractChildNode)
  {$I auto_generated_node_helpers/x3dnodes_collisioncollection.inc}
  end;

  TCollisionSensorNode = class(TAbstractSensorNode)
  {$I auto_generated_node_helpers/x3dnodes_collisionsensor.inc}
  end;

  TCollisionSpaceNode = class(TAbstractNBodyCollisionSpaceNode)
  {$I auto_generated_node_helpers/x3dnodes_collisionspace.inc}
  end;

  TContactNode = class(TAbstractNode)
  {$I auto_generated_node_helpers/x3dnodes_contact.inc}
  end;

  TDoubleAxisHingeJointNode = class(TAbstractRigidJointNode)
  {$I auto_generated_node_helpers/x3dnodes_doubleaxishingejoint.inc}
  end;

  TMotorJointNode = class(TAbstractRigidJointNode)
  {$I auto_generated_node_helpers/x3dnodes_motorjoint.inc}
  end;

  TRigidBodyNode = class(TAbstractNode)
  {$I auto_generated_node_helpers/x3dnodes_rigidbody.inc}
  end;

  TRigidBodyCollectionNode = class(TAbstractChildNode)
  {$I auto_generated_node_helpers/x3dnodes_rigidbodycollection.inc}
  end;

  TSingleAxisHingeJointNode = class(TAbstractRigidJointNode)
  {$I auto_generated_node_helpers/x3dnodes_singleaxishingejoint.inc}
  end;

  TSliderJointNode = class(TAbstractRigidJointNode)
  {$I auto_generated_node_helpers/x3dnodes_sliderjoint.inc}
  end;

  TUniversalJointNode = class(TAbstractRigidJointNode)
  {$I auto_generated_node_helpers/x3dnodes_universaljoint.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{$I auto_generated_node_helpers/x3dnodes_x3dnbodycollidablenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3dnbodycollisionspacenode.inc}
{$I auto_generated_node_helpers/x3dnodes_x3drigidjointnode.inc}
{$I auto_generated_node_helpers/x3dnodes_balljoint.inc}
{$I auto_generated_node_helpers/x3dnodes_collidableoffset.inc}
{$I auto_generated_node_helpers/x3dnodes_collidableshape.inc}
{$I auto_generated_node_helpers/x3dnodes_collisioncollection.inc}
{$I auto_generated_node_helpers/x3dnodes_collisionsensor.inc}
{$I auto_generated_node_helpers/x3dnodes_collisionspace.inc}
{$I auto_generated_node_helpers/x3dnodes_contact.inc}
{$I auto_generated_node_helpers/x3dnodes_doubleaxishingejoint.inc}
{$I auto_generated_node_helpers/x3dnodes_motorjoint.inc}
{$I auto_generated_node_helpers/x3dnodes_rigidbody.inc}
{$I auto_generated_node_helpers/x3dnodes_rigidbodycollection.inc}
{$I auto_generated_node_helpers/x3dnodes_singleaxishingejoint.inc}
{$I auto_generated_node_helpers/x3dnodes_sliderjoint.inc}
{$I auto_generated_node_helpers/x3dnodes_universaljoint.inc}

procedure RegisterRigidBodyPhysicsNodes;
begin
  NodesManager.RegisterNodeClasses([
    TBallJointNode,
    TCollidableOffsetNode,
    TCollidableShapeNode,
    TCollisionCollectionNode,
    TCollisionSensorNode,
    TCollisionSpaceNode,
    TContactNode,
    TDoubleAxisHingeJointNode,
    TMotorJointNode,
    TRigidBodyNode,
    TRigidBodyCollectionNode,
    TSingleAxisHingeJointNode,
    TSliderJointNode,
    TUniversalJointNode
  ]);
end;

{$endif read_implementation}
