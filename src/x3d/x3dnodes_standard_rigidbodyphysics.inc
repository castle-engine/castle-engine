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
  TAbstractNBodyCollidableNode = class(TAbstractChildNode, IAbstractBoundedObject)
  public
    procedure CreateNode; override;

    strict private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    strict private FFdRotation: TSFRotation;
    public property FdRotation: TSFRotation read FFdRotation;

    strict private FFdTranslation: TSFVec3f;
    public property FdTranslation: TSFVec3f read FFdTranslation;

    strict private FFdBboxCenter: TSFVec3f;
    public property FdBboxCenter: TSFVec3f read FFdBboxCenter;

    strict private FFdBboxSize: TSFVec3f;
    public property FdBboxSize: TSFVec3f read FFdBboxSize;

    {$I auto_generated_node_helpers/x3dnodes_x3dnbodycollidablenode.inc}
  end;

  TAbstractNBodyCollisionSpaceNode = class(TAbstractNode, IAbstractBoundedObject)
  public
    procedure CreateNode; override;

    strict private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    strict private FFdBboxCenter: TSFVec3f;
    public property FdBboxCenter: TSFVec3f read FFdBboxCenter;

    strict private FFdBboxSize: TSFVec3f;
    public property FdBboxSize: TSFVec3f read FFdBboxSize;

    {$I auto_generated_node_helpers/x3dnodes_x3dnbodycollisionspacenode.inc}
  end;

  TAbstractRigidJointNode = class(TAbstractNode)
  public
    procedure CreateNode; override;

    strict private FFdBody1: TSFNode;
    public property FdBody1: TSFNode read FFdBody1;

    strict private FFdBody2: TSFNode;
    public property FdBody2: TSFNode read FFdBody2;

    strict private FFdForceOutput: TMFString;
    public property FdForceOutput: TMFString read FFdForceOutput;

    {$I auto_generated_node_helpers/x3dnodes_x3drigidjointnode.inc}
  end;

  TBallJointNode = class(TAbstractRigidJointNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdAnchorPoint: TSFVec3f;
    public property FdAnchorPoint: TSFVec3f read FFdAnchorPoint;

    { Event out } { }
    strict private FEventBody1AnchorPoint: TSFVec3fEvent;
    public property EventBody1AnchorPoint: TSFVec3fEvent read FEventBody1AnchorPoint;

    { Event out } { }
    strict private FEventBody2AnchorPoint: TSFVec3fEvent;
    public property EventBody2AnchorPoint: TSFVec3fEvent read FEventBody2AnchorPoint;

    {$I auto_generated_node_helpers/x3dnodes_balljoint.inc}
  end;

  TCollidableOffsetNode = class(TAbstractNBodyCollidableNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdCollidable: TSFNode;
    public property FdCollidable: TSFNode read FFdCollidable;

    {$I auto_generated_node_helpers/x3dnodes_collidableoffset.inc}
  end;

  TCollidableShapeNode = class(TAbstractNBodyCollidableNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdShape: TSFNode;
    public property FdShape: TSFNode read FFdShape;

    {$I auto_generated_node_helpers/x3dnodes_collidableshape.inc}
  end;

  TCollisionCollectionNode = class(TAbstractChildNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdAppliedParameters: TMFString;
    public property FdAppliedParameters: TMFString read FFdAppliedParameters;

    strict private FFdBounce: TSFFloat;
    public property FdBounce: TSFFloat read FFdBounce;

    strict private FFdCollidables: TMFNode;
    public property FdCollidables: TMFNode read FFdCollidables;

    strict private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    strict private FFdFrictionCoefficients: TSFVec2f;
    public property FdFrictionCoefficients: TSFVec2f read FFdFrictionCoefficients;

    strict private FFdMinBounceSpeed: TSFFloat;
    public property FdMinBounceSpeed: TSFFloat read FFdMinBounceSpeed;

    strict private FFdSlipFactors: TSFVec2f;
    public property FdSlipFactors: TSFVec2f read FFdSlipFactors;

    strict private FFdSoftnessConstantForceMix: TSFFloat;
    public property FdSoftnessConstantForceMix: TSFFloat read FFdSoftnessConstantForceMix;

    strict private FFdSoftnessErrorCorrection: TSFFloat;
    public property FdSoftnessErrorCorrection: TSFFloat read FFdSoftnessErrorCorrection;

    strict private FFdSurfaceSpeed: TSFVec2f;
    public property FdSurfaceSpeed: TSFVec2f read FFdSurfaceSpeed;

    {$I auto_generated_node_helpers/x3dnodes_collisioncollection.inc}
  end;

  TCollisionSensorNode = class(TAbstractSensorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdCollider: TSFNode;
    public property FdCollider: TSFNode read FFdCollider;

    { Event out } { }
    strict private FEventIntersections: TMFNodeEvent;
    public property EventIntersections: TMFNodeEvent read FEventIntersections;

    { Event out } { }
    strict private FEventContacts: TMFNodeEvent;
    public property EventContacts: TMFNodeEvent read FEventContacts;

    {$I auto_generated_node_helpers/x3dnodes_collisionsensor.inc}
  end;

  TCollisionSpaceNode = class(TAbstractNBodyCollisionSpaceNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdCollidables: TMFNode;
    public property FdCollidables: TMFNode read FFdCollidables;

    strict private FFdUseGeometry: TSFBool;
    public property FdUseGeometry: TSFBool read FFdUseGeometry;

    {$I auto_generated_node_helpers/x3dnodes_collisionspace.inc}
  end;

  TContactNode = class(TAbstractNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdAppliedParameters: TMFString;
    public property FdAppliedParameters: TMFString read FFdAppliedParameters;

    strict private FFdBody1: TSFNode;
    public property FdBody1: TSFNode read FFdBody1;

    strict private FFdBody2: TSFNode;
    public property FdBody2: TSFNode read FFdBody2;

    strict private FFdBounce: TSFFloat;
    public property FdBounce: TSFFloat read FFdBounce;

    strict private FFdContactNormal: TSFVec3f;
    public property FdContactNormal: TSFVec3f read FFdContactNormal;

    strict private FFdDepth: TSFFloat;
    public property FdDepth: TSFFloat read FFdDepth;

    strict private FFdFrictionCoefficients: TSFVec2f;
    public property FdFrictionCoefficients: TSFVec2f read FFdFrictionCoefficients;

    strict private FFdFrictionDirection: TSFVec3f;
    public property FdFrictionDirection: TSFVec3f read FFdFrictionDirection;

    strict private FFdGeometry1: TSFNode;
    public property FdGeometry1: TSFNode read FFdGeometry1;

    strict private FFdGeometry2: TSFNode;
    public property FdGeometry2: TSFNode read FFdGeometry2;

    strict private FFdMinbounceSpeed: TSFFloat;
    public property FdMinbounceSpeed: TSFFloat read FFdMinbounceSpeed;

    strict private FFdPosition: TSFVec3f;
    public property FdPosition: TSFVec3f read FFdPosition;

    strict private FFdSlipCoefficients: TSFVec2f;
    public property FdSlipCoefficients: TSFVec2f read FFdSlipCoefficients;

    strict private FFdSoftnessConstantForceMix: TSFFloat;
    public property FdSoftnessConstantForceMix: TSFFloat read FFdSoftnessConstantForceMix;

    strict private FFdSoftnessErrorCorrection: TSFFloat;
    public property FdSoftnessErrorCorrection: TSFFloat read FFdSoftnessErrorCorrection;

    strict private FFdSurfaceSpeed: TSFVec2f;
    public property FdSurfaceSpeed: TSFVec2f read FFdSurfaceSpeed;

    {$I auto_generated_node_helpers/x3dnodes_contact.inc}
  end;

  TDoubleAxisHingeJointNode = class(TAbstractRigidJointNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdAnchorPoint: TSFVec3f;
    public property FdAnchorPoint: TSFVec3f read FFdAnchorPoint;

    strict private FFdAxis1: TSFVec3f;
    public property FdAxis1: TSFVec3f read FFdAxis1;

    strict private FFdAxis2: TSFVec3f;
    public property FdAxis2: TSFVec3f read FFdAxis2;

    strict private FFdDesiredAngularVelocity1: TSFFloat;
    public property FdDesiredAngularVelocity1: TSFFloat read FFdDesiredAngularVelocity1;

    strict private FFdDesiredAngularVelocity2: TSFFloat;
    public property FdDesiredAngularVelocity2: TSFFloat read FFdDesiredAngularVelocity2;

    strict private FFdMaxAngle1: TSFFloat;
    public property FdMaxAngle1: TSFFloat read FFdMaxAngle1;

    strict private FFdMaxTorque1: TSFFloat;
    public property FdMaxTorque1: TSFFloat read FFdMaxTorque1;

    strict private FFdMaxTorque2: TSFFloat;
    public property FdMaxTorque2: TSFFloat read FFdMaxTorque2;

    strict private FFdMinAngle1: TSFFloat;
    public property FdMinAngle1: TSFFloat read FFdMinAngle1;

    strict private FFdStopBounce1: TSFFloat;
    public property FdStopBounce1: TSFFloat read FFdStopBounce1;

    strict private FFdStopConstantForceMix1: TSFFloat;
    public property FdStopConstantForceMix1: TSFFloat read FFdStopConstantForceMix1;

    strict private FFdStopErrorCorrection1: TSFFloat;
    public property FdStopErrorCorrection1: TSFFloat read FFdStopErrorCorrection1;

    strict private FFdSuspensionErrorCorrection: TSFFloat;
    public property FdSuspensionErrorCorrection: TSFFloat read FFdSuspensionErrorCorrection;

    strict private FFdSuspensionForce: TSFFloat;
    public property FdSuspensionForce: TSFFloat read FFdSuspensionForce;

    { Event out } { }
    strict private FEventBody1AnchorPoint: TSFVec3fEvent;
    public property EventBody1AnchorPoint: TSFVec3fEvent read FEventBody1AnchorPoint;

    { Event out } { }
    strict private FEventBody1Axis: TSFVec3fEvent;
    public property EventBody1Axis: TSFVec3fEvent read FEventBody1Axis;

    { Event out } { }
    strict private FEventBody2AnchorPoint: TSFVec3fEvent;
    public property EventBody2AnchorPoint: TSFVec3fEvent read FEventBody2AnchorPoint;

    { Event out } { }
    strict private FEventBody2Axis: TSFVec3fEvent;
    public property EventBody2Axis: TSFVec3fEvent read FEventBody2Axis;

    { Event out } { }
    strict private FEventHinge1Angle: TSFFloatEvent;
    public property EventHinge1Angle: TSFFloatEvent read FEventHinge1Angle;

    { Event out } { }
    strict private FEventHinge1AngleRate: TSFFloatEvent;
    public property EventHinge1AngleRate: TSFFloatEvent read FEventHinge1AngleRate;

    { Event out } { }
    strict private FEventHinge2Angle: TSFFloatEvent;
    public property EventHinge2Angle: TSFFloatEvent read FEventHinge2Angle;

    { Event out } { }
    strict private FEventHinge2AngleRate: TSFFloatEvent;
    public property EventHinge2AngleRate: TSFFloatEvent read FEventHinge2AngleRate;

    {$I auto_generated_node_helpers/x3dnodes_doubleaxishingejoint.inc}
  end;

  TMotorJointNode = class(TAbstractRigidJointNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdAxis1Angle: TSFFloat;
    public property FdAxis1Angle: TSFFloat read FFdAxis1Angle;

    strict private FFdAxis1Torque: TSFFloat;
    public property FdAxis1Torque: TSFFloat read FFdAxis1Torque;

    strict private FFdAxis2Angle: TSFFloat;
    public property FdAxis2Angle: TSFFloat read FFdAxis2Angle;

    strict private FFdAxis2Torque: TSFFloat;
    public property FdAxis2Torque: TSFFloat read FFdAxis2Torque;

    strict private FFdAxis3Angle: TSFFloat;
    public property FdAxis3Angle: TSFFloat read FFdAxis3Angle;

    strict private FFdAxis3Torque: TSFFloat;
    public property FdAxis3Torque: TSFFloat read FFdAxis3Torque;

    strict private FFdEnabledAxes: TSFInt32;
    public property FdEnabledAxes: TSFInt32 read FFdEnabledAxes;

    strict private FFdMotor1Axis: TSFVec3f;
    public property FdMotor1Axis: TSFVec3f read FFdMotor1Axis;

    strict private FFdMotor2Axis: TSFVec3f;
    public property FdMotor2Axis: TSFVec3f read FFdMotor2Axis;

    strict private FFdMotor3Axis: TSFVec3f;
    public property FdMotor3Axis: TSFVec3f read FFdMotor3Axis;

    strict private FFdStop1Bounce: TSFFloat;
    public property FdStop1Bounce: TSFFloat read FFdStop1Bounce;

    strict private FFdStop1ErrorCorrection: TSFFloat;
    public property FdStop1ErrorCorrection: TSFFloat read FFdStop1ErrorCorrection;

    strict private FFdStop2Bounce: TSFFloat;
    public property FdStop2Bounce: TSFFloat read FFdStop2Bounce;

    strict private FFdStop2ErrorCorrection: TSFFloat;
    public property FdStop2ErrorCorrection: TSFFloat read FFdStop2ErrorCorrection;

    strict private FFdStop3Bounce: TSFFloat;
    public property FdStop3Bounce: TSFFloat read FFdStop3Bounce;

    strict private FFdStop3ErrorCorrection: TSFFloat;
    public property FdStop3ErrorCorrection: TSFFloat read FFdStop3ErrorCorrection;

    { Event out } { }
    strict private FEventMotor1Angle: TSFFloatEvent;
    public property EventMotor1Angle: TSFFloatEvent read FEventMotor1Angle;

    { Event out } { }
    strict private FEventMotor1AngleRate: TSFFloatEvent;
    public property EventMotor1AngleRate: TSFFloatEvent read FEventMotor1AngleRate;

    { Event out } { }
    strict private FEventMotor2Angle: TSFFloatEvent;
    public property EventMotor2Angle: TSFFloatEvent read FEventMotor2Angle;

    { Event out } { }
    strict private FEventMotor2AngleRate: TSFFloatEvent;
    public property EventMotor2AngleRate: TSFFloatEvent read FEventMotor2AngleRate;

    { Event out } { }
    strict private FEventMotor3Angle: TSFFloatEvent;
    public property EventMotor3Angle: TSFFloatEvent read FEventMotor3Angle;

    { Event out } { }
    strict private FEventMotor3AngleRate: TSFFloatEvent;
    public property EventMotor3AngleRate: TSFFloatEvent read FEventMotor3AngleRate;

    strict private FFdAutoCalc: TSFBool;
    public property FdAutoCalc: TSFBool read FFdAutoCalc;

    {$I auto_generated_node_helpers/x3dnodes_motorjoint.inc}
  end;

  TRigidBodyNode = class(TAbstractNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdAngularDampingFactor: TSFFloat;
    public property FdAngularDampingFactor: TSFFloat read FFdAngularDampingFactor;

    strict private FFdAngularVelocity: TSFVec3f;
    public property FdAngularVelocity: TSFVec3f read FFdAngularVelocity;

    strict private FFdAutoDamp: TSFBool;
    public property FdAutoDamp: TSFBool read FFdAutoDamp;

    strict private FFdAutoDisable: TSFBool;
    public property FdAutoDisable: TSFBool read FFdAutoDisable;

    strict private FFdCenterOfMass: TSFVec3f;
    public property FdCenterOfMass: TSFVec3f read FFdCenterOfMass;

    strict private FFdDisableAngularSpeed: TSFFloat;
    public property FdDisableAngularSpeed: TSFFloat read FFdDisableAngularSpeed;

    strict private FFdDisableLinearSpeed: TSFFloat;
    public property FdDisableLinearSpeed: TSFFloat read FFdDisableLinearSpeed;

    strict private FFdDisableTime: TSFFloat;
    public property FdDisableTime: TSFFloat read FFdDisableTime;

    strict private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    strict private FFdFiniteRotationAxis: TSFVec3f;
    public property FdFiniteRotationAxis: TSFVec3f read FFdFiniteRotationAxis;

    strict private FFdFixed: TSFBool;
    public property FdFixed: TSFBool read FFdFixed;

    strict private FFdForces: TMFVec3f;
    public property FdForces: TMFVec3f read FFdForces;

    strict private FFdGeometry: TMFNode;
    public property FdGeometry: TMFNode read FFdGeometry;

    strict private FFdInertia: TSFMatrix3f;
    public property FdInertia: TSFMatrix3f read FFdInertia;

    strict private FFdLinearDampingFactor: TSFFloat;
    public property FdLinearDampingFactor: TSFFloat read FFdLinearDampingFactor;

    strict private FFdLinearVelocity: TSFVec3f;
    public property FdLinearVelocity: TSFVec3f read FFdLinearVelocity;

    strict private FFdMass: TSFFloat;
    public property FdMass: TSFFloat read FFdMass;

    strict private FFdMassDensityModel: TSFNode;
    public property FdMassDensityModel: TSFNode read FFdMassDensityModel;

    strict private FFdOrientation: TSFRotation;
    public property FdOrientation: TSFRotation read FFdOrientation;

    strict private FFdPosition: TSFVec3f;
    public property FdPosition: TSFVec3f read FFdPosition;

    strict private FFdTorques: TMFVec3f;
    public property FdTorques: TMFVec3f read FFdTorques;

    strict private FFdUseFiniteRotation: TSFBool;
    public property FdUseFiniteRotation: TSFBool read FFdUseFiniteRotation;

    strict private FFdUseGlobalGravity: TSFBool;
    public property FdUseGlobalGravity: TSFBool read FFdUseGlobalGravity;

    {$I auto_generated_node_helpers/x3dnodes_rigidbody.inc}
  end;

  TRigidBodyCollectionNode = class(TAbstractChildNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    { Event in } { }
    strict private FEventSet_contacts: TMFNodeEvent;
    public property EventSet_contacts: TMFNodeEvent read FEventSet_contacts;

    strict private FFdAutoDisable: TSFBool;
    public property FdAutoDisable: TSFBool read FFdAutoDisable;

    strict private FFdBodies: TMFNode;
    public property FdBodies: TMFNode read FFdBodies;

    strict private FFdConstantForceMix: TSFFloat;
    public property FdConstantForceMix: TSFFloat read FFdConstantForceMix;

    strict private FFdContactSurfaceThickness: TSFFloat;
    public property FdContactSurfaceThickness: TSFFloat read FFdContactSurfaceThickness;

    strict private FFdDisableAngularSpeed: TSFFloat;
    public property FdDisableAngularSpeed: TSFFloat read FFdDisableAngularSpeed;

    strict private FFdDisableLinearSpeed: TSFFloat;
    public property FdDisableLinearSpeed: TSFFloat read FFdDisableLinearSpeed;

    strict private FFdDisableTime: TSFFloat;
    public property FdDisableTime: TSFFloat read FFdDisableTime;

    strict private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    strict private FFdErrorCorrection: TSFFloat;
    public property FdErrorCorrection: TSFFloat read FFdErrorCorrection;

    strict private FFdGravity: TSFVec3f;
    public property FdGravity: TSFVec3f read FFdGravity;

    strict private FFdIterations: TSFInt32;
    public property FdIterations: TSFInt32 read FFdIterations;

    strict private FFdJoints: TMFNode;
    public property FdJoints: TMFNode read FFdJoints;

    strict private FFdMaxCorrectionSpeed: TSFFloat;
    public property FdMaxCorrectionSpeed: TSFFloat read FFdMaxCorrectionSpeed;

    strict private FFdPreferAccuracy: TSFBool;
    public property FdPreferAccuracy: TSFBool read FFdPreferAccuracy;

    strict private FFdCollider: TSFNode;
    public property FdCollider: TSFNode read FFdCollider;

    {$I auto_generated_node_helpers/x3dnodes_rigidbodycollection.inc}
  end;

  TSingleAxisHingeJointNode = class(TAbstractRigidJointNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdAnchorPoint: TSFVec3f;
    public property FdAnchorPoint: TSFVec3f read FFdAnchorPoint;

    strict private FFdAxis: TSFVec3f;
    public property FdAxis: TSFVec3f read FFdAxis;

    strict private FFdMaxAngle: TSFFloat;
    public property FdMaxAngle: TSFFloat read FFdMaxAngle;

    strict private FFdMinAngle: TSFFloat;
    public property FdMinAngle: TSFFloat read FFdMinAngle;

    strict private FFdStopBounce: TSFFloat;
    public property FdStopBounce: TSFFloat read FFdStopBounce;

    strict private FFdStopErrorCorrection: TSFFloat;
    public property FdStopErrorCorrection: TSFFloat read FFdStopErrorCorrection;

    { Event out } { }
    strict private FEventAngle: TSFFloatEvent;
    public property EventAngle: TSFFloatEvent read FEventAngle;

    { Event out } { }
    strict private FEventAngleRate: TSFFloatEvent;
    public property EventAngleRate: TSFFloatEvent read FEventAngleRate;

    { Event out } { }
    strict private FEventBody1AnchorPoint: TSFVec3fEvent;
    public property EventBody1AnchorPoint: TSFVec3fEvent read FEventBody1AnchorPoint;

    { Event out } { }
    strict private FEventBody2AnchorPoint: TSFVec3fEvent;
    public property EventBody2AnchorPoint: TSFVec3fEvent read FEventBody2AnchorPoint;

    {$I auto_generated_node_helpers/x3dnodes_singleaxishingejoint.inc}
  end;

  TSliderJointNode = class(TAbstractRigidJointNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdAxis: TSFVec3f;
    public property FdAxis: TSFVec3f read FFdAxis;

    strict private FFdMaxSeparation: TSFFloat;
    public property FdMaxSeparation: TSFFloat read FFdMaxSeparation;

    strict private FFdMinSeparation: TSFFloat;
    public property FdMinSeparation: TSFFloat read FFdMinSeparation;

    strict private FFdSliderForce: TSFFloat;
    public property FdSliderForce: TSFFloat read FFdSliderForce;

    strict private FFdStopBounce: TSFFloat;
    public property FdStopBounce: TSFFloat read FFdStopBounce;

    strict private FFdStopErrorCorrection: TSFFloat;
    public property FdStopErrorCorrection: TSFFloat read FFdStopErrorCorrection;

    { Event out } { }
    strict private FEventSeparation: TSFFloatEvent;
    public property EventSeparation: TSFFloatEvent read FEventSeparation;

    { Event out } { }
    strict private FEventSeparationRate: TSFFloatEvent;
    public property EventSeparationRate: TSFFloatEvent read FEventSeparationRate;

    {$I auto_generated_node_helpers/x3dnodes_sliderjoint.inc}
  end;

  TUniversalJointNode = class(TAbstractRigidJointNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdAnchorPoint: TSFVec3f;
    public property FdAnchorPoint: TSFVec3f read FFdAnchorPoint;

    strict private FFdAxis1: TSFVec3f;
    public property FdAxis1: TSFVec3f read FFdAxis1;

    strict private FFdAxis2: TSFVec3f;
    public property FdAxis2: TSFVec3f read FFdAxis2;

    strict private FFdStopBounce1: TSFFloat;
    public property FdStopBounce1: TSFFloat read FFdStopBounce1;

    strict private FFdStop1ErrorCorrection: TSFFloat;
    public property FdStop1ErrorCorrection: TSFFloat read FFdStop1ErrorCorrection;

    strict private FFdStop2Bounce: TSFFloat;
    public property FdStop2Bounce: TSFFloat read FFdStop2Bounce;

    strict private FFdStop2ErrorCorrection: TSFFloat;
    public property FdStop2ErrorCorrection: TSFFloat read FFdStop2ErrorCorrection;

    { Event out } { }
    strict private FEventBody1AnchorPoint: TSFVec3fEvent;
    public property EventBody1AnchorPoint: TSFVec3fEvent read FEventBody1AnchorPoint;

    { Event out } { }
    strict private FEventBody1Axis: TSFVec3fEvent;
    public property EventBody1Axis: TSFVec3fEvent read FEventBody1Axis;

    { Event out } { }
    strict private FEventBody2AnchorPoint: TSFVec3fEvent;
    public property EventBody2AnchorPoint: TSFVec3fEvent read FEventBody2AnchorPoint;

    { Event out } { }
    strict private FEventBody2Axis: TSFVec3fEvent;
    public property EventBody2Axis: TSFVec3fEvent read FEventBody2Axis;

    {$I auto_generated_node_helpers/x3dnodes_universaljoint.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TAbstractNBodyCollidableNode.CreateNode;
begin
  inherited;

  FFdEnabled := TSFBool.Create(Self, true, 'enabled', true);
  AddField(FFdEnabled);

  FFdRotation := TSFRotation.Create(Self, true, 'rotation', Vector3(0, 0, 1), 0);
  AddField(FFdRotation);
  { X3D specification comment: [0,1] }

  FFdTranslation := TSFVec3f.Create(Self, true, 'translation', Vector3(0, 0, 0));
  AddField(FFdTranslation);
  { X3D specification comment: (-Inf,Inf) }

  FFdBboxCenter := TSFVec3f.Create(Self, false, 'bboxCenter', Vector3(0, 0, 0));
  AddField(FFdBboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdBboxSize := TSFVec3f.Create(Self, false, 'bboxSize', Vector3(-1, -1, -1));
  AddField(FFdBboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }
end;

procedure TAbstractNBodyCollisionSpaceNode.CreateNode;
begin
  inherited;

  FFdEnabled := TSFBool.Create(Self, true, 'enabled', true);
  AddField(FFdEnabled);

  FFdBboxCenter := TSFVec3f.Create(Self, false, 'bboxCenter', Vector3(0, 0, 0));
  AddField(FFdBboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdBboxSize := TSFVec3f.Create(Self, false, 'bboxSize', Vector3(-1, -1, -1));
  AddField(FFdBboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }
end;

procedure TAbstractRigidJointNode.CreateNode;
begin
  inherited;

  FFdBody1 := TSFNode.Create(Self, true, 'body1', [TRigidBodyNode]);
  AddField(FFdBody1);

  FFdBody2 := TSFNode.Create(Self, true, 'body2', [TRigidBodyNode]);
  AddField(FFdBody2);

  FFdForceOutput := TMFString.Create(Self, true, 'forceOutput', ['NONE']);
  AddField(FFdForceOutput);
  { X3D specification comment: ["ALL","NONE",...] }

  DefaultContainerField := 'joints';
end;

procedure TBallJointNode.CreateNode;
begin
  inherited;

  FFdAnchorPoint := TSFVec3f.Create(Self, true, 'anchorPoint', Vector3(0, 0, 0));
  AddField(FFdAnchorPoint);

  FEventBody1AnchorPoint := TSFVec3fEvent.Create(Self, 'body1AnchorPoint', false);
  AddEvent(FEventBody1AnchorPoint);

  FEventBody2AnchorPoint := TSFVec3fEvent.Create(Self, 'body2AnchorPoint', false);
  AddEvent(FEventBody2AnchorPoint);
end;

class function TBallJointNode.ClassX3DType: string;
begin
  Result := 'BallJoint';
end;

procedure TCollidableOffsetNode.CreateNode;
begin
  inherited;

  FFdCollidable := TSFNode.Create(Self, false, 'collidable', [TAbstractNBodyCollidableNode]);
  AddField(FFdCollidable);
end;

class function TCollidableOffsetNode.ClassX3DType: string;
begin
  Result := 'CollidableOffset';
end;

procedure TCollidableShapeNode.CreateNode;
begin
  inherited;

  FFdShape := TSFNode.Create(Self, false, 'shape', [TShapeNode]);
  AddField(FFdShape);
end;

class function TCollidableShapeNode.ClassX3DType: string;
begin
  Result := 'CollidableShape';
end;

procedure TCollisionCollectionNode.CreateNode;
begin
  inherited;

  FFdAppliedParameters := TMFString.Create(Self, true, 'appliedParameters', ['BOUNCE']);
  AddField(FFdAppliedParameters);
  { X3D specification comment: [] }

  FFdBounce := TSFFloat.Create(Self, true, 'bounce', 0);
  AddField(FFdBounce);
  { X3D specification comment: [0,1] }

  FFdCollidables := TMFNode.Create(Self, true, 'collidables', [TAbstractNBodyCollisionSpaceNode, TAbstractNBodyCollidableNode]);
  AddField(FFdCollidables);

  FFdEnabled := TSFBool.Create(Self, true, 'enabled', true);
  AddField(FFdEnabled);

  FFdFrictionCoefficients := TSFVec2f.Create(Self, true, 'frictionCoefficients', Vector2(0, 0));
  AddField(FFdFrictionCoefficients);
  { X3D specification comment: [0,Inf) }

  FFdMinBounceSpeed := TSFFloat.Create(Self, true, 'minBounceSpeed', 0.1);
  AddField(FFdMinBounceSpeed);
  { X3D specification comment: [0,Inf) }

  FFdSlipFactors := TSFVec2f.Create(Self, true, 'slipFactors', Vector2(0, 0));
  AddField(FFdSlipFactors);
  { X3D specification comment: (-Inf,Inf) }

  FFdSoftnessConstantForceMix := TSFFloat.Create(Self, true, 'softnessConstantForceMix', 0.0001);
  AddField(FFdSoftnessConstantForceMix);
  { X3D specification comment: [0,1] }

  FFdSoftnessErrorCorrection := TSFFloat.Create(Self, true, 'softnessErrorCorrection', 0.8);
  AddField(FFdSoftnessErrorCorrection);
  { X3D specification comment: [0,1] }

  FFdSurfaceSpeed := TSFVec2f.Create(Self, true, 'surfaceSpeed', Vector2(0, 0));
  AddField(FFdSurfaceSpeed);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TCollisionCollectionNode.ClassX3DType: string;
begin
  Result := 'CollisionCollection';
end;

procedure TCollisionSensorNode.CreateNode;
begin
  inherited;

  FFdCollider := TSFNode.Create(Self, true, 'collider', [TCollisionCollectionNode]);
  AddField(FFdCollider);

  FEventIntersections := TMFNodeEvent.Create(Self, 'intersections', false);
  AddEvent(FEventIntersections);

  FEventContacts := TMFNodeEvent.Create(Self, 'contacts', false);
  AddEvent(FEventContacts);
end;

class function TCollisionSensorNode.ClassX3DType: string;
begin
  Result := 'CollisionSensor';
end;

procedure TCollisionSpaceNode.CreateNode;
begin
  inherited;

  FFdCollidables := TMFNode.Create(Self, true, 'collidables', [TAbstractNBodyCollisionSpaceNode, TAbstractNBodyCollidableNode]);
  AddField(FFdCollidables);

  FFdUseGeometry := TSFBool.Create(Self, true, 'useGeometry', false);
  AddField(FFdUseGeometry);

  DefaultContainerField := 'children';
end;

class function TCollisionSpaceNode.ClassX3DType: string;
begin
  Result := 'CollisionSpace';
end;

procedure TContactNode.CreateNode;
begin
  inherited;

  FFdAppliedParameters := TMFString.Create(Self, true, 'appliedParameters', ['BOUNCE']);
  AddField(FFdAppliedParameters);
  { X3D specification comment: [] }

  FFdBody1 := TSFNode.Create(Self, true, 'body1', [TRigidBodyNode]);
  AddField(FFdBody1);

  FFdBody2 := TSFNode.Create(Self, true, 'body2', [TRigidBodyNode]);
  AddField(FFdBody2);

  FFdBounce := TSFFloat.Create(Self, true, 'bounce', 0);
  AddField(FFdBounce);
  { X3D specification comment: [0,1] }

  FFdContactNormal := TSFVec3f.Create(Self, true, 'contactNormal', Vector3(0, 1, 0));
  AddField(FFdContactNormal);
  { X3D specification comment: (-Inf,Inf) }

  FFdDepth := TSFFloat.Create(Self, true, 'depth', 0);
  AddField(FFdDepth);
  { X3D specification comment: (-Inf,Inf) }

  FFdFrictionCoefficients := TSFVec2f.Create(Self, true, 'frictionCoefficients', Vector2(0, 0));
  AddField(FFdFrictionCoefficients);
  { X3D specification comment: [0,Inf) }

  FFdFrictionDirection := TSFVec3f.Create(Self, true, 'frictionDirection', Vector3(0, 1, 0));
  AddField(FFdFrictionDirection);
  { X3D specification comment: (-Inf,Inf) }

  FFdGeometry1 := TSFNode.Create(Self, true, 'geometry1', [TAbstractNBodyCollidableNode]);
  AddField(FFdGeometry1);

  FFdGeometry2 := TSFNode.Create(Self, true, 'geometry2', [TAbstractNBodyCollidableNode]);
  AddField(FFdGeometry2);

  FFdMinbounceSpeed := TSFFloat.Create(Self, true, 'minbounceSpeed', 0);
  AddField(FFdMinbounceSpeed);
  { X3D specification comment: [0,Inf) }

  FFdPosition := TSFVec3f.Create(Self, true, 'position', Vector3(0, 0, 0));
  AddField(FFdPosition);
  { X3D specification comment: (-Inf,Inf) }

  FFdSlipCoefficients := TSFVec2f.Create(Self, true, 'slipCoefficients', Vector2(0, 0));
  AddField(FFdSlipCoefficients);
  { X3D specification comment: (-Inf,Inf) }

  FFdSoftnessConstantForceMix := TSFFloat.Create(Self, true, 'softnessConstantForceMix', 0.0001);
  AddField(FFdSoftnessConstantForceMix);
  { X3D specification comment: [0,1] }

  FFdSoftnessErrorCorrection := TSFFloat.Create(Self, true, 'softnessErrorCorrection', 0.8);
  AddField(FFdSoftnessErrorCorrection);
  { X3D specification comment: [0,1] }

  FFdSurfaceSpeed := TSFVec2f.Create(Self, true, 'surfaceSpeed', Vector2(0, 0));
  AddField(FFdSurfaceSpeed);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TContactNode.ClassX3DType: string;
begin
  Result := 'Contact';
end;

procedure TDoubleAxisHingeJointNode.CreateNode;
begin
  inherited;

  FFdAnchorPoint := TSFVec3f.Create(Self, true, 'anchorPoint', Vector3(0, 0, 0));
  AddField(FFdAnchorPoint);

  FFdAxis1 := TSFVec3f.Create(Self, true, 'axis1', Vector3(0, 0, 0));
  AddField(FFdAxis1);

  FFdAxis2 := TSFVec3f.Create(Self, true, 'axis2', Vector3(0, 0, 0));
  AddField(FFdAxis2);

  FFdDesiredAngularVelocity1 := TSFFloat.Create(Self, true, 'desiredAngularVelocity1', 0);
  AddField(FFdDesiredAngularVelocity1);
  { X3D specification comment: (-Inf,Inf) }

  FFdDesiredAngularVelocity2 := TSFFloat.Create(Self, true, 'desiredAngularVelocity2', 0);
  AddField(FFdDesiredAngularVelocity2);
  { X3D specification comment: (-Inf,Inf) }

  FFdMaxAngle1 := TSFFloat.Create(Self, true, 'maxAngle1', Pi);
  AddField(FFdMaxAngle1);
  { X3D specification comment: [-Pi,Pi] }

  FFdMaxTorque1 := TSFFloat.Create(Self, true, 'maxTorque1', 0);
  AddField(FFdMaxTorque1);
  { X3D specification comment: (-Inf,Inf) }

  FFdMaxTorque2 := TSFFloat.Create(Self, true, 'maxTorque2', 0);
  AddField(FFdMaxTorque2);
  { X3D specification comment: (-Inf,Inf) }

  FFdMinAngle1 := TSFFloat.Create(Self, true, 'minAngle1', -Pi);
  AddField(FFdMinAngle1);
  { X3D specification comment: [-Pi,Pi] }

  FFdStopBounce1 := TSFFloat.Create(Self, true, 'stopBounce1', 0);
  AddField(FFdStopBounce1);
  { X3D specification comment: [0,1] }

  FFdStopConstantForceMix1 := TSFFloat.Create(Self, true, 'stopConstantForceMix1', 0.001);
  AddField(FFdStopConstantForceMix1);
  { X3D specification comment: [0,Inf) }

  FFdStopErrorCorrection1 := TSFFloat.Create(Self, true, 'stopErrorCorrection1', 0.8);
  AddField(FFdStopErrorCorrection1);
  { X3D specification comment: [0,1] }

  FFdSuspensionErrorCorrection := TSFFloat.Create(Self, true, 'suspensionErrorCorrection', 0.8);
  AddField(FFdSuspensionErrorCorrection);
  { X3D specification comment: [0,1] }

  FFdSuspensionForce := TSFFloat.Create(Self, true, 'suspensionForce', 0);
  AddField(FFdSuspensionForce);
  { X3D specification comment: [0,Inf) }

  FEventBody1AnchorPoint := TSFVec3fEvent.Create(Self, 'body1AnchorPoint', false);
  AddEvent(FEventBody1AnchorPoint);

  FEventBody1Axis := TSFVec3fEvent.Create(Self, 'body1Axis', false);
  AddEvent(FEventBody1Axis);

  FEventBody2AnchorPoint := TSFVec3fEvent.Create(Self, 'body2AnchorPoint', false);
  AddEvent(FEventBody2AnchorPoint);

  FEventBody2Axis := TSFVec3fEvent.Create(Self, 'body2Axis', false);
  AddEvent(FEventBody2Axis);

  FEventHinge1Angle := TSFFloatEvent.Create(Self, 'hinge1Angle', false);
  AddEvent(FEventHinge1Angle);

  FEventHinge1AngleRate := TSFFloatEvent.Create(Self, 'hinge1AngleRate', false);
  AddEvent(FEventHinge1AngleRate);

  FEventHinge2Angle := TSFFloatEvent.Create(Self, 'hinge2Angle', false);
  AddEvent(FEventHinge2Angle);

  FEventHinge2AngleRate := TSFFloatEvent.Create(Self, 'hinge2AngleRate', false);
  AddEvent(FEventHinge2AngleRate);
end;

class function TDoubleAxisHingeJointNode.ClassX3DType: string;
begin
  Result := 'DoubleAxisHingeJoint';
end;

procedure TMotorJointNode.CreateNode;
begin
  inherited;

  FFdAxis1Angle := TSFFloat.Create(Self, true, 'axis1Angle', 0);
  AddField(FFdAxis1Angle);
  { X3D specification comment: [-Pi,Pi] }

  FFdAxis1Torque := TSFFloat.Create(Self, true, 'axis1Torque', 0);
  AddField(FFdAxis1Torque);
  { X3D specification comment: (-Inf,Inf) }

  FFdAxis2Angle := TSFFloat.Create(Self, true, 'axis2Angle', 0);
  AddField(FFdAxis2Angle);
  { X3D specification comment: [-Pi,Pi] }

  FFdAxis2Torque := TSFFloat.Create(Self, true, 'axis2Torque', 0);
  AddField(FFdAxis2Torque);
  { X3D specification comment: (-Inf,Inf) }

  FFdAxis3Angle := TSFFloat.Create(Self, true, 'axis3Angle', 0);
  AddField(FFdAxis3Angle);
  { X3D specification comment: [-Pi,Pi] }

  FFdAxis3Torque := TSFFloat.Create(Self, true, 'axis3Torque', 0);
  AddField(FFdAxis3Torque);
  { X3D specification comment: (-Inf,Inf) }

  FFdEnabledAxes := TSFInt32.Create(Self, true, 'enabledAxes', 1);
  AddField(FFdEnabledAxes);
  { X3D specification comment: [0,3] }

  FFdMotor1Axis := TSFVec3f.Create(Self, true, 'motor1Axis', Vector3(0, 0, 0));
  AddField(FFdMotor1Axis);

  FFdMotor2Axis := TSFVec3f.Create(Self, true, 'motor2Axis', Vector3(0, 0, 0));
  AddField(FFdMotor2Axis);

  FFdMotor3Axis := TSFVec3f.Create(Self, true, 'motor3Axis', Vector3(0, 0, 0));
  AddField(FFdMotor3Axis);

  FFdStop1Bounce := TSFFloat.Create(Self, true, 'stop1Bounce', 0);
  AddField(FFdStop1Bounce);
  { X3D specification comment: [0,1] }

  FFdStop1ErrorCorrection := TSFFloat.Create(Self, true, 'stop1ErrorCorrection', 0.8);
  AddField(FFdStop1ErrorCorrection);
  { X3D specification comment: [0,1] }

  FFdStop2Bounce := TSFFloat.Create(Self, true, 'stop2Bounce', 0);
  AddField(FFdStop2Bounce);
  { X3D specification comment: [0,1] }

  FFdStop2ErrorCorrection := TSFFloat.Create(Self, true, 'stop2ErrorCorrection', 0.8);
  AddField(FFdStop2ErrorCorrection);
  { X3D specification comment: [0,1] }

  FFdStop3Bounce := TSFFloat.Create(Self, true, 'stop3Bounce', 0);
  AddField(FFdStop3Bounce);
  { X3D specification comment: [0,1] }

  FFdStop3ErrorCorrection := TSFFloat.Create(Self, true, 'stop3ErrorCorrection', 0.8);
  AddField(FFdStop3ErrorCorrection);
  { X3D specification comment: [0,1] }

  FEventMotor1Angle := TSFFloatEvent.Create(Self, 'motor1Angle', false);
  AddEvent(FEventMotor1Angle);

  FEventMotor1AngleRate := TSFFloatEvent.Create(Self, 'motor1AngleRate', false);
  AddEvent(FEventMotor1AngleRate);

  FEventMotor2Angle := TSFFloatEvent.Create(Self, 'motor2Angle', false);
  AddEvent(FEventMotor2Angle);

  FEventMotor2AngleRate := TSFFloatEvent.Create(Self, 'motor2AngleRate', false);
  AddEvent(FEventMotor2AngleRate);

  FEventMotor3Angle := TSFFloatEvent.Create(Self, 'motor3Angle', false);
  AddEvent(FEventMotor3Angle);

  FEventMotor3AngleRate := TSFFloatEvent.Create(Self, 'motor3AngleRate', false);
  AddEvent(FEventMotor3AngleRate);

  FFdAutoCalc := TSFBool.Create(Self, false, 'autoCalc', false);
  AddField(FFdAutoCalc);
end;

class function TMotorJointNode.ClassX3DType: string;
begin
  Result := 'MotorJoint';
end;

procedure TRigidBodyNode.CreateNode;
begin
  inherited;

  FFdAngularDampingFactor := TSFFloat.Create(Self, true, 'angularDampingFactor', 0.001);
  AddField(FFdAngularDampingFactor);
  { X3D specification comment: [0,1] }

  FFdAngularVelocity := TSFVec3f.Create(Self, true, 'angularVelocity', Vector3(0, 0, 0));
  AddField(FFdAngularVelocity);
  { X3D specification comment: (-Inf,Inf) }

  FFdAutoDamp := TSFBool.Create(Self, true, 'autoDamp', false);
  AddField(FFdAutoDamp);

  FFdAutoDisable := TSFBool.Create(Self, true, 'autoDisable', false);
  AddField(FFdAutoDisable);

  FFdCenterOfMass := TSFVec3f.Create(Self, true, 'centerOfMass', Vector3(0, 0, 0));
  AddField(FFdCenterOfMass);
  { X3D specification comment: (-Inf,Inf) }

  FFdDisableAngularSpeed := TSFFloat.Create(Self, true, 'disableAngularSpeed', 0);
  AddField(FFdDisableAngularSpeed);
  { X3D specification comment: [0,Inf) }

  FFdDisableLinearSpeed := TSFFloat.Create(Self, true, 'disableLinearSpeed', 0);
  AddField(FFdDisableLinearSpeed);
  { X3D specification comment: [0,Inf) }

  FFdDisableTime := TSFFloat.Create(Self, true, 'disableTime', 0);
  AddField(FFdDisableTime);
  { X3D specification comment: [0,Inf) }

  FFdEnabled := TSFBool.Create(Self, true, 'enabled', true);
  AddField(FFdEnabled);

  FFdFiniteRotationAxis := TSFVec3f.Create(Self, true, 'finiteRotationAxis', Vector3(0, 0, 0));
  AddField(FFdFiniteRotationAxis);
  { X3D specification comment: [-1,1] }

  FFdFixed := TSFBool.Create(Self, true, 'fixed', false);
  AddField(FFdFixed);

  FFdForces := TMFVec3f.Create(Self, true, 'forces', []);
  AddField(FFdForces);

  FFdGeometry := TMFNode.Create(Self, true, 'geometry', [TAbstractNBodyCollidableNode]);
  AddField(FFdGeometry);

  FFdInertia := TSFMatrix3f.Create(Self, true, 'inertia', TMatrix3.Identity);
  AddField(FFdInertia);

  FFdLinearDampingFactor := TSFFloat.Create(Self, true, 'linearDampingFactor', 0.001);
  AddField(FFdLinearDampingFactor);
  { X3D specification comment: [0,1] }

  FFdLinearVelocity := TSFVec3f.Create(Self, true, 'linearVelocity', Vector3(0, 0, 0));
  AddField(FFdLinearVelocity);
  { X3D specification comment: (-Inf,Inf) }

  FFdMass := TSFFloat.Create(Self, true, 'mass', 1);
  AddField(FFdMass);
  { X3D specification comment: (0,Inf) }

  FFdMassDensityModel := TSFNode.Create(Self, true, 'massDensityModel', [TSphereNode, TBoxNode, TConeNode]);
  AddField(FFdMassDensityModel);

  FFdOrientation := TSFRotation.Create(Self, true, 'orientation', Vector3(0, 0, 1), 0);
  AddField(FFdOrientation);
  { X3D specification comment: [0,1] }

  FFdPosition := TSFVec3f.Create(Self, true, 'position', Vector3(0, 0, 0));
  AddField(FFdPosition);
  { X3D specification comment: (-Inf,Inf) }

  FFdTorques := TMFVec3f.Create(Self, true, 'torques', []);
  AddField(FFdTorques);

  FFdUseFiniteRotation := TSFBool.Create(Self, true, 'useFiniteRotation', false);
  AddField(FFdUseFiniteRotation);

  FFdUseGlobalGravity := TSFBool.Create(Self, true, 'useGlobalGravity', true);
  AddField(FFdUseGlobalGravity);

  DefaultContainerField := 'bodies';
end;

class function TRigidBodyNode.ClassX3DType: string;
begin
  Result := 'RigidBody';
end;

procedure TRigidBodyCollectionNode.CreateNode;
begin
  inherited;

  FEventSet_contacts := TMFNodeEvent.Create(Self, 'set_contacts', true);
  AddEvent(FEventSet_contacts);

  FFdAutoDisable := TSFBool.Create(Self, true, 'autoDisable', false);
  AddField(FFdAutoDisable);

  FFdBodies := TMFNode.Create(Self, true, 'bodies', [TRigidBodyNode]);
  AddField(FFdBodies);

  FFdConstantForceMix := TSFFloat.Create(Self, true, 'constantForceMix', 0.0001);
  AddField(FFdConstantForceMix);
  { X3D specification comment: [0,Inf) }

  FFdContactSurfaceThickness := TSFFloat.Create(Self, true, 'contactSurfaceThickness', 0);
  AddField(FFdContactSurfaceThickness);
  { X3D specification comment: [0,Inf) }

  FFdDisableAngularSpeed := TSFFloat.Create(Self, true, 'disableAngularSpeed', 0);
  AddField(FFdDisableAngularSpeed);
  { X3D specification comment: [0,Inf) }

  FFdDisableLinearSpeed := TSFFloat.Create(Self, true, 'disableLinearSpeed', 0);
  AddField(FFdDisableLinearSpeed);
  { X3D specification comment: [0,Inf) }

  FFdDisableTime := TSFFloat.Create(Self, true, 'disableTime', 0);
  AddField(FFdDisableTime);
  { X3D specification comment: [0,Inf) }

  FFdEnabled := TSFBool.Create(Self, true, 'enabled', true);
  AddField(FFdEnabled);

  FFdErrorCorrection := TSFFloat.Create(Self, true, 'errorCorrection', 0.8);
  AddField(FFdErrorCorrection);
  { X3D specification comment: [0,1] }

  FFdGravity := TSFVec3f.Create(Self, true, 'gravity', Vector3(0, -9.8, 0));
  AddField(FFdGravity);

  FFdIterations := TSFInt32.Create(Self, true, 'iterations', 10);
  AddField(FFdIterations);
  { X3D specification comment: [0,Inf) }

  FFdJoints := TMFNode.Create(Self, true, 'joints', [TAbstractRigidJointNode]);
  AddField(FFdJoints);

  FFdMaxCorrectionSpeed := TSFFloat.Create(Self, true, 'maxCorrectionSpeed', -1);
  AddField(FFdMaxCorrectionSpeed);
  { X3D specification comment: [0,Inf) or -1 }

  FFdPreferAccuracy := TSFBool.Create(Self, true, 'preferAccuracy', false);
  AddField(FFdPreferAccuracy);

  FFdCollider := TSFNode.Create(Self, false, 'collider', [TCollisionCollectionNode]);
  AddField(FFdCollider);
end;

class function TRigidBodyCollectionNode.ClassX3DType: string;
begin
  Result := 'RigidBodyCollection';
end;

procedure TSingleAxisHingeJointNode.CreateNode;
begin
  inherited;

  FFdAnchorPoint := TSFVec3f.Create(Self, true, 'anchorPoint', Vector3(0, 0, 0));
  AddField(FFdAnchorPoint);

  FFdAxis := TSFVec3f.Create(Self, true, 'axis', Vector3(0, 0, 0));
  AddField(FFdAxis);

  FFdMaxAngle := TSFFloat.Create(Self, true, 'maxAngle', Pi);
  AddField(FFdMaxAngle);

  FFdMinAngle := TSFFloat.Create(Self, true, 'minAngle', -Pi);
  AddField(FFdMinAngle);

  FFdStopBounce := TSFFloat.Create(Self, true, 'stopBounce', 0);
  AddField(FFdStopBounce);
  { X3D specification comment: [0,1] }

  FFdStopErrorCorrection := TSFFloat.Create(Self, true, 'stopErrorCorrection', 0.8);
  AddField(FFdStopErrorCorrection);
  { X3D specification comment: [0,1] }

  FEventAngle := TSFFloatEvent.Create(Self, 'angle', false);
  AddEvent(FEventAngle);

  FEventAngleRate := TSFFloatEvent.Create(Self, 'angleRate', false);
  AddEvent(FEventAngleRate);

  FEventBody1AnchorPoint := TSFVec3fEvent.Create(Self, 'body1AnchorPoint', false);
  AddEvent(FEventBody1AnchorPoint);

  FEventBody2AnchorPoint := TSFVec3fEvent.Create(Self, 'body2AnchorPoint', false);
  AddEvent(FEventBody2AnchorPoint);
end;

class function TSingleAxisHingeJointNode.ClassX3DType: string;
begin
  Result := 'SingleAxisHingeJoint';
end;

procedure TSliderJointNode.CreateNode;
begin
  inherited;

  FFdAxis := TSFVec3f.Create(Self, true, 'axis', Vector3(0, 1, 0));
  AddField(FFdAxis);

  FFdMaxSeparation := TSFFloat.Create(Self, true, 'maxSeparation', 1);
  AddField(FFdMaxSeparation);
  { X3D specification comment: [0,Inf) }

  FFdMinSeparation := TSFFloat.Create(Self, true, 'minSeparation', 0);
  AddField(FFdMinSeparation);
  { X3D specification comment: [0,Inf) }

  FFdSliderForce := TSFFloat.Create(Self, true, 'sliderForce', 0);
  AddField(FFdSliderForce);
  { X3D specification comment: [0,Inf) }

  FFdStopBounce := TSFFloat.Create(Self, true, 'stopBounce', 0);
  AddField(FFdStopBounce);
  { X3D specification comment: [0,1] }

  FFdStopErrorCorrection := TSFFloat.Create(Self, true, 'stopErrorCorrection', 1);
  AddField(FFdStopErrorCorrection);
  { X3D specification comment: [0,1] }

  FEventSeparation := TSFFloatEvent.Create(Self, 'separation', false);
  AddEvent(FEventSeparation);

  FEventSeparationRate := TSFFloatEvent.Create(Self, 'separationRate', false);
  AddEvent(FEventSeparationRate);
end;

class function TSliderJointNode.ClassX3DType: string;
begin
  Result := 'SliderJoint';
end;

procedure TUniversalJointNode.CreateNode;
begin
  inherited;

  FFdAnchorPoint := TSFVec3f.Create(Self, true, 'anchorPoint', Vector3(0, 0, 0));
  AddField(FFdAnchorPoint);

  FFdAxis1 := TSFVec3f.Create(Self, true, 'axis1', Vector3(0, 0, 0));
  AddField(FFdAxis1);

  FFdAxis2 := TSFVec3f.Create(Self, true, 'axis2', Vector3(0, 0, 0));
  AddField(FFdAxis2);

  FFdStopBounce1 := TSFFloat.Create(Self, true, 'stopBounce1', 0);
  AddField(FFdStopBounce1);
  { X3D specification comment: [0,1] }

  FFdStop1ErrorCorrection := TSFFloat.Create(Self, true, 'stop1ErrorCorrection', 0.8);
  AddField(FFdStop1ErrorCorrection);
  { X3D specification comment: [0,1] }

  FFdStop2Bounce := TSFFloat.Create(Self, true, 'stop2Bounce', 0);
  AddField(FFdStop2Bounce);
  { X3D specification comment: [0,1] }

  FFdStop2ErrorCorrection := TSFFloat.Create(Self, true, 'stop2ErrorCorrection', 0.8);
  AddField(FFdStop2ErrorCorrection);
  { X3D specification comment: [0,1] }

  FEventBody1AnchorPoint := TSFVec3fEvent.Create(Self, 'body1AnchorPoint', false);
  AddEvent(FEventBody1AnchorPoint);

  FEventBody1Axis := TSFVec3fEvent.Create(Self, 'body1Axis', false);
  AddEvent(FEventBody1Axis);

  FEventBody2AnchorPoint := TSFVec3fEvent.Create(Self, 'body2AnchorPoint', false);
  AddEvent(FEventBody2AnchorPoint);

  FEventBody2Axis := TSFVec3fEvent.Create(Self, 'body2Axis', false);
  AddEvent(FEventBody2Axis);
end;

class function TUniversalJointNode.ClassX3DType: string;
begin
  Result := 'UniversalJoint';
end;

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
