{
  Copyright 2008-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { }
  TAbstractNBodyCollidableNode = class(TAbstractChildNode, IAbstractBoundedObject)
  public
    procedure CreateNode; override;

    private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    private FFdRotation: TSFRotation;
    public property FdRotation: TSFRotation read FFdRotation;

    private FFdTranslation: TSFVec3f;
    public property FdTranslation: TSFVec3f read FFdTranslation;

    private FFdBboxCenter: TSFVec3f;
    public property FdBboxCenter: TSFVec3f read FFdBboxCenter;

    private FFdBboxSize: TSFVec3f;
    public property FdBboxSize: TSFVec3f read FFdBboxSize;

    {$I auto_generated_node_helpers/x3dnodes_x3dnbodycollidablenode.inc}
  end;

  TAbstractNBodyCollisionSpaceNode = class(TAbstractNode, IAbstractBoundedObject)
  public
    procedure CreateNode; override;

    private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    private FFdBboxCenter: TSFVec3f;
    public property FdBboxCenter: TSFVec3f read FFdBboxCenter;

    private FFdBboxSize: TSFVec3f;
    public property FdBboxSize: TSFVec3f read FFdBboxSize;

    {$I auto_generated_node_helpers/x3dnodes_x3dnbodycollisionspacenode.inc}
  end;

  TAbstractRigidJointNode = class(TAbstractNode)
  public
    procedure CreateNode; override;

    private FFdBody1: TSFNode;
    public property FdBody1: TSFNode read FFdBody1;

    private FFdBody2: TSFNode;
    public property FdBody2: TSFNode read FFdBody2;

    private FFdForceOutput: TMFString;
    public property FdForceOutput: TMFString read FFdForceOutput;

    {$I auto_generated_node_helpers/x3dnodes_x3drigidjointnode.inc}
  end;

  TBallJointNode = class(TAbstractRigidJointNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAnchorPoint: TSFVec3f;
    public property FdAnchorPoint: TSFVec3f read FFdAnchorPoint;

    { Event out } { }
    private FEventBody1AnchorPoint: TSFVec3fEvent;
    public property EventBody1AnchorPoint: TSFVec3fEvent read FEventBody1AnchorPoint;

    { Event out } { }
    private FEventBody2AnchorPoint: TSFVec3fEvent;
    public property EventBody2AnchorPoint: TSFVec3fEvent read FEventBody2AnchorPoint;

    {$I auto_generated_node_helpers/x3dnodes_balljoint.inc}
  end;

  TCollidableOffsetNode = class(TAbstractNBodyCollidableNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdCollidable: TSFNode;
    public property FdCollidable: TSFNode read FFdCollidable;

    {$I auto_generated_node_helpers/x3dnodes_collidableoffset.inc}
  end;

  TCollidableShapeNode = class(TAbstractNBodyCollidableNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdShape: TSFNode;
    public property FdShape: TSFNode read FFdShape;

    {$I auto_generated_node_helpers/x3dnodes_collidableshape.inc}
  end;

  TCollisionCollectionNode = class(TAbstractChildNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAppliedParameters: TMFString;
    public property FdAppliedParameters: TMFString read FFdAppliedParameters;

    private FFdBounce: TSFFloat;
    public property FdBounce: TSFFloat read FFdBounce;

    private FFdCollidables: TMFNode;
    public property FdCollidables: TMFNode read FFdCollidables;

    private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    private FFdFrictionCoefficients: TSFVec2f;
    public property FdFrictionCoefficients: TSFVec2f read FFdFrictionCoefficients;

    private FFdMinBounceSpeed: TSFFloat;
    public property FdMinBounceSpeed: TSFFloat read FFdMinBounceSpeed;

    private FFdSlipFactors: TSFVec2f;
    public property FdSlipFactors: TSFVec2f read FFdSlipFactors;

    private FFdSoftnessConstantForceMix: TSFFloat;
    public property FdSoftnessConstantForceMix: TSFFloat read FFdSoftnessConstantForceMix;

    private FFdSoftnessErrorCorrection: TSFFloat;
    public property FdSoftnessErrorCorrection: TSFFloat read FFdSoftnessErrorCorrection;

    private FFdSurfaceSpeed: TSFVec2f;
    public property FdSurfaceSpeed: TSFVec2f read FFdSurfaceSpeed;

    {$I auto_generated_node_helpers/x3dnodes_collisioncollection.inc}
  end;

  TCollisionSensorNode = class(TAbstractSensorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdCollider: TSFNode;
    public property FdCollider: TSFNode read FFdCollider;

    { Event out } { }
    private FEventIntersections: TMFNodeEvent;
    public property EventIntersections: TMFNodeEvent read FEventIntersections;

    { Event out } { }
    private FEventContacts: TMFNodeEvent;
    public property EventContacts: TMFNodeEvent read FEventContacts;

    {$I auto_generated_node_helpers/x3dnodes_collisionsensor.inc}
  end;

  TCollisionSpaceNode = class(TAbstractNBodyCollisionSpaceNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdCollidables: TMFNode;
    public property FdCollidables: TMFNode read FFdCollidables;

    private FFdUseGeometry: TSFBool;
    public property FdUseGeometry: TSFBool read FFdUseGeometry;

    {$I auto_generated_node_helpers/x3dnodes_collisionspace.inc}
  end;

  TContactNode = class(TAbstractNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAppliedParameters: TMFString;
    public property FdAppliedParameters: TMFString read FFdAppliedParameters;

    private FFdBody1: TSFNode;
    public property FdBody1: TSFNode read FFdBody1;

    private FFdBody2: TSFNode;
    public property FdBody2: TSFNode read FFdBody2;

    private FFdBounce: TSFFloat;
    public property FdBounce: TSFFloat read FFdBounce;

    private FFdContactNormal: TSFVec3f;
    public property FdContactNormal: TSFVec3f read FFdContactNormal;

    private FFdDepth: TSFFloat;
    public property FdDepth: TSFFloat read FFdDepth;

    private FFdFrictionCoefficients: TSFVec2f;
    public property FdFrictionCoefficients: TSFVec2f read FFdFrictionCoefficients;

    private FFdFrictionDirection: TSFVec3f;
    public property FdFrictionDirection: TSFVec3f read FFdFrictionDirection;

    private FFdGeometry1: TSFNode;
    public property FdGeometry1: TSFNode read FFdGeometry1;

    private FFdGeometry2: TSFNode;
    public property FdGeometry2: TSFNode read FFdGeometry2;

    private FFdMinbounceSpeed: TSFFloat;
    public property FdMinbounceSpeed: TSFFloat read FFdMinbounceSpeed;

    private FFdPosition: TSFVec3f;
    public property FdPosition: TSFVec3f read FFdPosition;

    private FFdSlipCoefficients: TSFVec2f;
    public property FdSlipCoefficients: TSFVec2f read FFdSlipCoefficients;

    private FFdSoftnessConstantForceMix: TSFFloat;
    public property FdSoftnessConstantForceMix: TSFFloat read FFdSoftnessConstantForceMix;

    private FFdSoftnessErrorCorrection: TSFFloat;
    public property FdSoftnessErrorCorrection: TSFFloat read FFdSoftnessErrorCorrection;

    private FFdSurfaceSpeed: TSFVec2f;
    public property FdSurfaceSpeed: TSFVec2f read FFdSurfaceSpeed;

    {$I auto_generated_node_helpers/x3dnodes_contact.inc}
  end;

  TDoubleAxisHingeJointNode = class(TAbstractRigidJointNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAnchorPoint: TSFVec3f;
    public property FdAnchorPoint: TSFVec3f read FFdAnchorPoint;

    private FFdAxis1: TSFVec3f;
    public property FdAxis1: TSFVec3f read FFdAxis1;

    private FFdAxis2: TSFVec3f;
    public property FdAxis2: TSFVec3f read FFdAxis2;

    private FFdDesiredAngularVelocity1: TSFFloat;
    public property FdDesiredAngularVelocity1: TSFFloat read FFdDesiredAngularVelocity1;

    private FFdDesiredAngularVelocity2: TSFFloat;
    public property FdDesiredAngularVelocity2: TSFFloat read FFdDesiredAngularVelocity2;

    private FFdMaxAngle1: TSFFloat;
    public property FdMaxAngle1: TSFFloat read FFdMaxAngle1;

    private FFdMaxTorque1: TSFFloat;
    public property FdMaxTorque1: TSFFloat read FFdMaxTorque1;

    private FFdMaxTorque2: TSFFloat;
    public property FdMaxTorque2: TSFFloat read FFdMaxTorque2;

    private FFdMinAngle1: TSFFloat;
    public property FdMinAngle1: TSFFloat read FFdMinAngle1;

    private FFdStopBounce1: TSFFloat;
    public property FdStopBounce1: TSFFloat read FFdStopBounce1;

    private FFdStopConstantForceMix1: TSFFloat;
    public property FdStopConstantForceMix1: TSFFloat read FFdStopConstantForceMix1;

    private FFdStopErrorCorrection1: TSFFloat;
    public property FdStopErrorCorrection1: TSFFloat read FFdStopErrorCorrection1;

    private FFdSuspensionErrorCorrection: TSFFloat;
    public property FdSuspensionErrorCorrection: TSFFloat read FFdSuspensionErrorCorrection;

    private FFdSuspensionForce: TSFFloat;
    public property FdSuspensionForce: TSFFloat read FFdSuspensionForce;

    { Event out } { }
    private FEventBody1AnchorPoint: TSFVec3fEvent;
    public property EventBody1AnchorPoint: TSFVec3fEvent read FEventBody1AnchorPoint;

    { Event out } { }
    private FEventBody1Axis: TSFVec3fEvent;
    public property EventBody1Axis: TSFVec3fEvent read FEventBody1Axis;

    { Event out } { }
    private FEventBody2AnchorPoint: TSFVec3fEvent;
    public property EventBody2AnchorPoint: TSFVec3fEvent read FEventBody2AnchorPoint;

    { Event out } { }
    private FEventBody2Axis: TSFVec3fEvent;
    public property EventBody2Axis: TSFVec3fEvent read FEventBody2Axis;

    { Event out } { }
    private FEventHinge1Angle: TSFFloatEvent;
    public property EventHinge1Angle: TSFFloatEvent read FEventHinge1Angle;

    { Event out } { }
    private FEventHinge1AngleRate: TSFFloatEvent;
    public property EventHinge1AngleRate: TSFFloatEvent read FEventHinge1AngleRate;

    { Event out } { }
    private FEventHinge2Angle: TSFFloatEvent;
    public property EventHinge2Angle: TSFFloatEvent read FEventHinge2Angle;

    { Event out } { }
    private FEventHinge2AngleRate: TSFFloatEvent;
    public property EventHinge2AngleRate: TSFFloatEvent read FEventHinge2AngleRate;

    {$I auto_generated_node_helpers/x3dnodes_doubleaxishingejoint.inc}
  end;

  TMotorJointNode = class(TAbstractRigidJointNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAxis1Angle: TSFFloat;
    public property FdAxis1Angle: TSFFloat read FFdAxis1Angle;

    private FFdAxis1Torque: TSFFloat;
    public property FdAxis1Torque: TSFFloat read FFdAxis1Torque;

    private FFdAxis2Angle: TSFFloat;
    public property FdAxis2Angle: TSFFloat read FFdAxis2Angle;

    private FFdAxis2Torque: TSFFloat;
    public property FdAxis2Torque: TSFFloat read FFdAxis2Torque;

    private FFdAxis3Angle: TSFFloat;
    public property FdAxis3Angle: TSFFloat read FFdAxis3Angle;

    private FFdAxis3Torque: TSFFloat;
    public property FdAxis3Torque: TSFFloat read FFdAxis3Torque;

    private FFdEnabledAxes: TSFInt32;
    public property FdEnabledAxes: TSFInt32 read FFdEnabledAxes;

    private FFdMotor1Axis: TSFVec3f;
    public property FdMotor1Axis: TSFVec3f read FFdMotor1Axis;

    private FFdMotor2Axis: TSFVec3f;
    public property FdMotor2Axis: TSFVec3f read FFdMotor2Axis;

    private FFdMotor3Axis: TSFVec3f;
    public property FdMotor3Axis: TSFVec3f read FFdMotor3Axis;

    private FFdStop1Bounce: TSFFloat;
    public property FdStop1Bounce: TSFFloat read FFdStop1Bounce;

    private FFdStop1ErrorCorrection: TSFFloat;
    public property FdStop1ErrorCorrection: TSFFloat read FFdStop1ErrorCorrection;

    private FFdStop2Bounce: TSFFloat;
    public property FdStop2Bounce: TSFFloat read FFdStop2Bounce;

    private FFdStop2ErrorCorrection: TSFFloat;
    public property FdStop2ErrorCorrection: TSFFloat read FFdStop2ErrorCorrection;

    private FFdStop3Bounce: TSFFloat;
    public property FdStop3Bounce: TSFFloat read FFdStop3Bounce;

    private FFdStop3ErrorCorrection: TSFFloat;
    public property FdStop3ErrorCorrection: TSFFloat read FFdStop3ErrorCorrection;

    { Event out } { }
    private FEventMotor1Angle: TSFFloatEvent;
    public property EventMotor1Angle: TSFFloatEvent read FEventMotor1Angle;

    { Event out } { }
    private FEventMotor1AngleRate: TSFFloatEvent;
    public property EventMotor1AngleRate: TSFFloatEvent read FEventMotor1AngleRate;

    { Event out } { }
    private FEventMotor2Angle: TSFFloatEvent;
    public property EventMotor2Angle: TSFFloatEvent read FEventMotor2Angle;

    { Event out } { }
    private FEventMotor2AngleRate: TSFFloatEvent;
    public property EventMotor2AngleRate: TSFFloatEvent read FEventMotor2AngleRate;

    { Event out } { }
    private FEventMotor3Angle: TSFFloatEvent;
    public property EventMotor3Angle: TSFFloatEvent read FEventMotor3Angle;

    { Event out } { }
    private FEventMotor3AngleRate: TSFFloatEvent;
    public property EventMotor3AngleRate: TSFFloatEvent read FEventMotor3AngleRate;

    private FFdAutoCalc: TSFBool;
    public property FdAutoCalc: TSFBool read FFdAutoCalc;

    {$I auto_generated_node_helpers/x3dnodes_motorjoint.inc}
  end;

  TRigidBodyNode = class(TAbstractNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAngularDampingFactor: TSFFloat;
    public property FdAngularDampingFactor: TSFFloat read FFdAngularDampingFactor;

    private FFdAngularVelocity: TSFVec3f;
    public property FdAngularVelocity: TSFVec3f read FFdAngularVelocity;

    private FFdAutoDamp: TSFBool;
    public property FdAutoDamp: TSFBool read FFdAutoDamp;

    private FFdAutoDisable: TSFBool;
    public property FdAutoDisable: TSFBool read FFdAutoDisable;

    private FFdCenterOfMass: TSFVec3f;
    public property FdCenterOfMass: TSFVec3f read FFdCenterOfMass;

    private FFdDisableAngularSpeed: TSFFloat;
    public property FdDisableAngularSpeed: TSFFloat read FFdDisableAngularSpeed;

    private FFdDisableLinearSpeed: TSFFloat;
    public property FdDisableLinearSpeed: TSFFloat read FFdDisableLinearSpeed;

    private FFdDisableTime: TSFFloat;
    public property FdDisableTime: TSFFloat read FFdDisableTime;

    private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    private FFdFiniteRotationAxis: TSFVec3f;
    public property FdFiniteRotationAxis: TSFVec3f read FFdFiniteRotationAxis;

    private FFdFixed: TSFBool;
    public property FdFixed: TSFBool read FFdFixed;

    private FFdForces: TMFVec3f;
    public property FdForces: TMFVec3f read FFdForces;

    private FFdGeometry: TMFNode;
    public property FdGeometry: TMFNode read FFdGeometry;

    private FFdInertia: TSFMatrix3f;
    public property FdInertia: TSFMatrix3f read FFdInertia;

    private FFdLinearDampingFactor: TSFFloat;
    public property FdLinearDampingFactor: TSFFloat read FFdLinearDampingFactor;

    private FFdLinearVelocity: TSFVec3f;
    public property FdLinearVelocity: TSFVec3f read FFdLinearVelocity;

    private FFdMass: TSFFloat;
    public property FdMass: TSFFloat read FFdMass;

    private FFdMassDensityModel: TSFNode;
    public property FdMassDensityModel: TSFNode read FFdMassDensityModel;

    private FFdOrientation: TSFRotation;
    public property FdOrientation: TSFRotation read FFdOrientation;

    private FFdPosition: TSFVec3f;
    public property FdPosition: TSFVec3f read FFdPosition;

    private FFdTorques: TMFVec3f;
    public property FdTorques: TMFVec3f read FFdTorques;

    private FFdUseFiniteRotation: TSFBool;
    public property FdUseFiniteRotation: TSFBool read FFdUseFiniteRotation;

    private FFdUseGlobalGravity: TSFBool;
    public property FdUseGlobalGravity: TSFBool read FFdUseGlobalGravity;

    {$I auto_generated_node_helpers/x3dnodes_rigidbody.inc}
  end;

  TRigidBodyCollectionNode = class(TAbstractChildNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event in } { }
    private FEventSet_contacts: TMFNodeEvent;
    public property EventSet_contacts: TMFNodeEvent read FEventSet_contacts;

    private FFdAutoDisable: TSFBool;
    public property FdAutoDisable: TSFBool read FFdAutoDisable;

    private FFdBodies: TMFNode;
    public property FdBodies: TMFNode read FFdBodies;

    private FFdConstantForceMix: TSFFloat;
    public property FdConstantForceMix: TSFFloat read FFdConstantForceMix;

    private FFdContactSurfaceThickness: TSFFloat;
    public property FdContactSurfaceThickness: TSFFloat read FFdContactSurfaceThickness;

    private FFdDisableAngularSpeed: TSFFloat;
    public property FdDisableAngularSpeed: TSFFloat read FFdDisableAngularSpeed;

    private FFdDisableLinearSpeed: TSFFloat;
    public property FdDisableLinearSpeed: TSFFloat read FFdDisableLinearSpeed;

    private FFdDisableTime: TSFFloat;
    public property FdDisableTime: TSFFloat read FFdDisableTime;

    private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    private FFdErrorCorrection: TSFFloat;
    public property FdErrorCorrection: TSFFloat read FFdErrorCorrection;

    private FFdGravity: TSFVec3f;
    public property FdGravity: TSFVec3f read FFdGravity;

    private FFdIterations: TSFInt32;
    public property FdIterations: TSFInt32 read FFdIterations;

    private FFdJoints: TMFNode;
    public property FdJoints: TMFNode read FFdJoints;

    private FFdMaxCorrectionSpeed: TSFFloat;
    public property FdMaxCorrectionSpeed: TSFFloat read FFdMaxCorrectionSpeed;

    private FFdPreferAccuracy: TSFBool;
    public property FdPreferAccuracy: TSFBool read FFdPreferAccuracy;

    private FFdCollider: TSFNode;
    public property FdCollider: TSFNode read FFdCollider;

    {$I auto_generated_node_helpers/x3dnodes_rigidbodycollection.inc}
  end;

  TSingleAxisHingeJointNode = class(TAbstractRigidJointNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAnchorPoint: TSFVec3f;
    public property FdAnchorPoint: TSFVec3f read FFdAnchorPoint;

    private FFdAxis: TSFVec3f;
    public property FdAxis: TSFVec3f read FFdAxis;

    private FFdMaxAngle: TSFFloat;
    public property FdMaxAngle: TSFFloat read FFdMaxAngle;

    private FFdMinAngle: TSFFloat;
    public property FdMinAngle: TSFFloat read FFdMinAngle;

    private FFdStopBounce: TSFFloat;
    public property FdStopBounce: TSFFloat read FFdStopBounce;

    private FFdStopErrorCorrection: TSFFloat;
    public property FdStopErrorCorrection: TSFFloat read FFdStopErrorCorrection;

    { Event out } { }
    private FEventAngle: TSFFloatEvent;
    public property EventAngle: TSFFloatEvent read FEventAngle;

    { Event out } { }
    private FEventAngleRate: TSFFloatEvent;
    public property EventAngleRate: TSFFloatEvent read FEventAngleRate;

    { Event out } { }
    private FEventBody1AnchorPoint: TSFVec3fEvent;
    public property EventBody1AnchorPoint: TSFVec3fEvent read FEventBody1AnchorPoint;

    { Event out } { }
    private FEventBody2AnchorPoint: TSFVec3fEvent;
    public property EventBody2AnchorPoint: TSFVec3fEvent read FEventBody2AnchorPoint;

    {$I auto_generated_node_helpers/x3dnodes_singleaxishingejoint.inc}
  end;

  TSliderJointNode = class(TAbstractRigidJointNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAxis: TSFVec3f;
    public property FdAxis: TSFVec3f read FFdAxis;

    private FFdMaxSeparation: TSFFloat;
    public property FdMaxSeparation: TSFFloat read FFdMaxSeparation;

    private FFdMinSeparation: TSFFloat;
    public property FdMinSeparation: TSFFloat read FFdMinSeparation;

    private FFdSliderForce: TSFFloat;
    public property FdSliderForce: TSFFloat read FFdSliderForce;

    private FFdStopBounce: TSFFloat;
    public property FdStopBounce: TSFFloat read FFdStopBounce;

    private FFdStopErrorCorrection: TSFFloat;
    public property FdStopErrorCorrection: TSFFloat read FFdStopErrorCorrection;

    { Event out } { }
    private FEventSeparation: TSFFloatEvent;
    public property EventSeparation: TSFFloatEvent read FEventSeparation;

    { Event out } { }
    private FEventSeparationRate: TSFFloatEvent;
    public property EventSeparationRate: TSFFloatEvent read FEventSeparationRate;

    {$I auto_generated_node_helpers/x3dnodes_sliderjoint.inc}
  end;

  TUniversalJointNode = class(TAbstractRigidJointNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAnchorPoint: TSFVec3f;
    public property FdAnchorPoint: TSFVec3f read FFdAnchorPoint;

    private FFdAxis1: TSFVec3f;
    public property FdAxis1: TSFVec3f read FFdAxis1;

    private FFdAxis2: TSFVec3f;
    public property FdAxis2: TSFVec3f read FFdAxis2;

    private FFdStopBounce1: TSFFloat;
    public property FdStopBounce1: TSFFloat read FFdStopBounce1;

    private FFdStop1ErrorCorrection: TSFFloat;
    public property FdStop1ErrorCorrection: TSFFloat read FFdStop1ErrorCorrection;

    private FFdStop2Bounce: TSFFloat;
    public property FdStop2Bounce: TSFFloat read FFdStop2Bounce;

    private FFdStop2ErrorCorrection: TSFFloat;
    public property FdStop2ErrorCorrection: TSFFloat read FFdStop2ErrorCorrection;

    { Event out } { }
    private FEventBody1AnchorPoint: TSFVec3fEvent;
    public property EventBody1AnchorPoint: TSFVec3fEvent read FEventBody1AnchorPoint;

    { Event out } { }
    private FEventBody1Axis: TSFVec3fEvent;
    public property EventBody1Axis: TSFVec3fEvent read FEventBody1Axis;

    { Event out } { }
    private FEventBody2AnchorPoint: TSFVec3fEvent;
    public property EventBody2AnchorPoint: TSFVec3fEvent read FEventBody2AnchorPoint;

    { Event out } { }
    private FEventBody2Axis: TSFVec3fEvent;
    public property EventBody2Axis: TSFVec3fEvent read FEventBody2Axis;

    {$I auto_generated_node_helpers/x3dnodes_universaljoint.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TAbstractNBodyCollidableNode.CreateNode;
begin
  inherited;

  FFdEnabled := TSFBool.Create(Self, 'enabled', true);
  AddField(FFdEnabled);

  FFdRotation := TSFRotation.Create(Self, 'rotation', Vector3Single(0, 0, 1), 0);
  AddField(FFdRotation);
  { X3D specification comment: [0,1] }

  FFdTranslation := TSFVec3f.Create(Self, 'translation', Vector3Single(0, 0, 0));
  AddField(FFdTranslation);
  { X3D specification comment: (-Inf,Inf) }

  FFdBboxCenter := TSFVec3f.Create(Self, 'bboxCenter', Vector3Single(0, 0, 0));
  FFdBboxCenter.Exposed := false;
  AddField(FFdBboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdBboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
  FFdBboxSize.Exposed := false;
  AddField(FFdBboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }
end;

procedure TAbstractNBodyCollisionSpaceNode.CreateNode;
begin
  inherited;

  FFdEnabled := TSFBool.Create(Self, 'enabled', true);
  AddField(FFdEnabled);

  FFdBboxCenter := TSFVec3f.Create(Self, 'bboxCenter', Vector3Single(0, 0, 0));
  FFdBboxCenter.Exposed := false;
  AddField(FFdBboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdBboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
  FFdBboxSize.Exposed := false;
  AddField(FFdBboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }
end;

procedure TAbstractRigidJointNode.CreateNode;
begin
  inherited;

  FFdBody1 := TSFNode.Create(Self, 'body1', [TRigidBodyNode]);
  AddField(FFdBody1);

  FFdBody2 := TSFNode.Create(Self, 'body2', [TRigidBodyNode]);
  AddField(FFdBody2);

  FFdForceOutput := TMFString.Create(Self, 'forceOutput', ['NONE']);
  AddField(FFdForceOutput);
  { X3D specification comment: ["ALL","NONE",...] }

  DefaultContainerField := 'joints';
end;

procedure TBallJointNode.CreateNode;
begin
  inherited;

  FFdAnchorPoint := TSFVec3f.Create(Self, 'anchorPoint', Vector3Single(0, 0, 0));
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

class function TBallJointNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TCollidableOffsetNode.CreateNode;
begin
  inherited;

  FFdCollidable := TSFNode.Create(Self, 'collidable', [TAbstractNBodyCollidableNode]);
  FFdCollidable.Exposed := false;
  AddField(FFdCollidable);
end;

class function TCollidableOffsetNode.ClassX3DType: string;
begin
  Result := 'CollidableOffset';
end;

class function TCollidableOffsetNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TCollidableShapeNode.CreateNode;
begin
  inherited;

  FFdShape := TSFNode.Create(Self, 'shape', [TShapeNode]);
  FFdShape.Exposed := false;
  AddField(FFdShape);
end;

class function TCollidableShapeNode.ClassX3DType: string;
begin
  Result := 'CollidableShape';
end;

class function TCollidableShapeNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TCollisionCollectionNode.CreateNode;
begin
  inherited;

  FFdAppliedParameters := TMFString.Create(Self, 'appliedParameters', ['BOUNCE']);
  AddField(FFdAppliedParameters);
  { X3D specification comment: [] }

  FFdBounce := TSFFloat.Create(Self, 'bounce', 0);
  AddField(FFdBounce);
  { X3D specification comment: [0,1] }

  FFdCollidables := TMFNode.Create(Self, 'collidables', [TAbstractNBodyCollisionSpaceNode, TAbstractNBodyCollidableNode]);
  AddField(FFdCollidables);

  FFdEnabled := TSFBool.Create(Self, 'enabled', true);
  AddField(FFdEnabled);

  FFdFrictionCoefficients := TSFVec2f.Create(Self, 'frictionCoefficients', Vector2Single(0, 0));
  AddField(FFdFrictionCoefficients);
  { X3D specification comment: [0,Inf) }

  FFdMinBounceSpeed := TSFFloat.Create(Self, 'minBounceSpeed', 0.1);
  AddField(FFdMinBounceSpeed);
  { X3D specification comment: [0,Inf) }

  FFdSlipFactors := TSFVec2f.Create(Self, 'slipFactors', Vector2Single(0, 0));
  AddField(FFdSlipFactors);
  { X3D specification comment: (-Inf,Inf) }

  FFdSoftnessConstantForceMix := TSFFloat.Create(Self, 'softnessConstantForceMix', 0.0001);
  AddField(FFdSoftnessConstantForceMix);
  { X3D specification comment: [0,1] }

  FFdSoftnessErrorCorrection := TSFFloat.Create(Self, 'softnessErrorCorrection', 0.8);
  AddField(FFdSoftnessErrorCorrection);
  { X3D specification comment: [0,1] }

  FFdSurfaceSpeed := TSFVec2f.Create(Self, 'surfaceSpeed', Vector2Single(0, 0));
  AddField(FFdSurfaceSpeed);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TCollisionCollectionNode.ClassX3DType: string;
begin
  Result := 'CollisionCollection';
end;

class function TCollisionCollectionNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TCollisionSensorNode.CreateNode;
begin
  inherited;

  FFdCollider := TSFNode.Create(Self, 'collider', [TCollisionCollectionNode]);
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

class function TCollisionSensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TCollisionSpaceNode.CreateNode;
begin
  inherited;

  FFdCollidables := TMFNode.Create(Self, 'collidables', [TAbstractNBodyCollisionSpaceNode, TAbstractNBodyCollidableNode]);
  AddField(FFdCollidables);

  FFdUseGeometry := TSFBool.Create(Self, 'useGeometry', false);
  AddField(FFdUseGeometry);

  DefaultContainerField := 'children';
end;

class function TCollisionSpaceNode.ClassX3DType: string;
begin
  Result := 'CollisionSpace';
end;

class function TCollisionSpaceNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TContactNode.CreateNode;
begin
  inherited;

  FFdAppliedParameters := TMFString.Create(Self, 'appliedParameters', ['BOUNCE']);
  AddField(FFdAppliedParameters);
  { X3D specification comment: [] }

  FFdBody1 := TSFNode.Create(Self, 'body1', [TRigidBodyNode]);
  AddField(FFdBody1);

  FFdBody2 := TSFNode.Create(Self, 'body2', [TRigidBodyNode]);
  AddField(FFdBody2);

  FFdBounce := TSFFloat.Create(Self, 'bounce', 0);
  AddField(FFdBounce);
  { X3D specification comment: [0,1] }

  FFdContactNormal := TSFVec3f.Create(Self, 'contactNormal', Vector3Single(0, 1, 0));
  AddField(FFdContactNormal);
  { X3D specification comment: (-Inf,Inf) }

  FFdDepth := TSFFloat.Create(Self, 'depth', 0);
  AddField(FFdDepth);
  { X3D specification comment: (-Inf,Inf) }

  FFdFrictionCoefficients := TSFVec2f.Create(Self, 'frictionCoefficients', Vector2Single(0, 0));
  AddField(FFdFrictionCoefficients);
  { X3D specification comment: [0,Inf) }

  FFdFrictionDirection := TSFVec3f.Create(Self, 'frictionDirection', Vector3Single(0, 1, 0));
  AddField(FFdFrictionDirection);
  { X3D specification comment: (-Inf,Inf) }

  FFdGeometry1 := TSFNode.Create(Self, 'geometry1', [TAbstractNBodyCollidableNode]);
  AddField(FFdGeometry1);

  FFdGeometry2 := TSFNode.Create(Self, 'geometry2', [TAbstractNBodyCollidableNode]);
  AddField(FFdGeometry2);

  FFdMinbounceSpeed := TSFFloat.Create(Self, 'minbounceSpeed', 0);
  AddField(FFdMinbounceSpeed);
  { X3D specification comment: [0,Inf) }

  FFdPosition := TSFVec3f.Create(Self, 'position', Vector3Single(0, 0, 0));
  AddField(FFdPosition);
  { X3D specification comment: (-Inf,Inf) }

  FFdSlipCoefficients := TSFVec2f.Create(Self, 'slipCoefficients', Vector2Single(0, 0));
  AddField(FFdSlipCoefficients);
  { X3D specification comment: (-Inf,Inf) }

  FFdSoftnessConstantForceMix := TSFFloat.Create(Self, 'softnessConstantForceMix', 0.0001);
  AddField(FFdSoftnessConstantForceMix);
  { X3D specification comment: [0,1] }

  FFdSoftnessErrorCorrection := TSFFloat.Create(Self, 'softnessErrorCorrection', 0.8);
  AddField(FFdSoftnessErrorCorrection);
  { X3D specification comment: [0,1] }

  FFdSurfaceSpeed := TSFVec2f.Create(Self, 'surfaceSpeed', Vector2Single(0, 0));
  AddField(FFdSurfaceSpeed);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TContactNode.ClassX3DType: string;
begin
  Result := 'Contact';
end;

class function TContactNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TDoubleAxisHingeJointNode.CreateNode;
begin
  inherited;

  FFdAnchorPoint := TSFVec3f.Create(Self, 'anchorPoint', Vector3Single(0, 0, 0));
  AddField(FFdAnchorPoint);

  FFdAxis1 := TSFVec3f.Create(Self, 'axis1', Vector3Single(0, 0, 0));
  AddField(FFdAxis1);

  FFdAxis2 := TSFVec3f.Create(Self, 'axis2', Vector3Single(0, 0, 0));
  AddField(FFdAxis2);

  FFdDesiredAngularVelocity1 := TSFFloat.Create(Self, 'desiredAngularVelocity1', 0);
  AddField(FFdDesiredAngularVelocity1);
  { X3D specification comment: (-Inf,Inf) }

  FFdDesiredAngularVelocity2 := TSFFloat.Create(Self, 'desiredAngularVelocity2', 0);
  AddField(FFdDesiredAngularVelocity2);
  { X3D specification comment: (-Inf,Inf) }

  FFdMaxAngle1 := TSFFloat.Create(Self, 'maxAngle1', Pi);
  AddField(FFdMaxAngle1);
  { X3D specification comment: [-Pi,Pi] }

  FFdMaxTorque1 := TSFFloat.Create(Self, 'maxTorque1', 0);
  AddField(FFdMaxTorque1);
  { X3D specification comment: (-Inf,Inf) }

  FFdMaxTorque2 := TSFFloat.Create(Self, 'maxTorque2', 0);
  AddField(FFdMaxTorque2);
  { X3D specification comment: (-Inf,Inf) }

  FFdMinAngle1 := TSFFloat.Create(Self, 'minAngle1', -Pi);
  AddField(FFdMinAngle1);
  { X3D specification comment: [-Pi,Pi] }

  FFdStopBounce1 := TSFFloat.Create(Self, 'stopBounce1', 0);
  AddField(FFdStopBounce1);
  { X3D specification comment: [0,1] }

  FFdStopConstantForceMix1 := TSFFloat.Create(Self, 'stopConstantForceMix1', 0.001);
  AddField(FFdStopConstantForceMix1);
  { X3D specification comment: [0,Inf) }

  FFdStopErrorCorrection1 := TSFFloat.Create(Self, 'stopErrorCorrection1', 0.8);
  AddField(FFdStopErrorCorrection1);
  { X3D specification comment: [0,1] }

  FFdSuspensionErrorCorrection := TSFFloat.Create(Self, 'suspensionErrorCorrection', 0.8);
  AddField(FFdSuspensionErrorCorrection);
  { X3D specification comment: [0,1] }

  FFdSuspensionForce := TSFFloat.Create(Self, 'suspensionForce', 0);
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

class function TDoubleAxisHingeJointNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TMotorJointNode.CreateNode;
begin
  inherited;

  FFdAxis1Angle := TSFFloat.Create(Self, 'axis1Angle', 0);
  AddField(FFdAxis1Angle);
  { X3D specification comment: [-Pi,Pi] }

  FFdAxis1Torque := TSFFloat.Create(Self, 'axis1Torque', 0);
  AddField(FFdAxis1Torque);
  { X3D specification comment: (-Inf,Inf) }

  FFdAxis2Angle := TSFFloat.Create(Self, 'axis2Angle', 0);
  AddField(FFdAxis2Angle);
  { X3D specification comment: [-Pi,Pi] }

  FFdAxis2Torque := TSFFloat.Create(Self, 'axis2Torque', 0);
  AddField(FFdAxis2Torque);
  { X3D specification comment: (-Inf,Inf) }

  FFdAxis3Angle := TSFFloat.Create(Self, 'axis3Angle', 0);
  AddField(FFdAxis3Angle);
  { X3D specification comment: [-Pi,Pi] }

  FFdAxis3Torque := TSFFloat.Create(Self, 'axis3Torque', 0);
  AddField(FFdAxis3Torque);
  { X3D specification comment: (-Inf,Inf) }

  FFdEnabledAxes := TSFInt32.Create(Self, 'enabledAxes', 1);
  AddField(FFdEnabledAxes);
  { X3D specification comment: [0,3] }

  FFdMotor1Axis := TSFVec3f.Create(Self, 'motor1Axis', Vector3Single(0, 0, 0));
  AddField(FFdMotor1Axis);

  FFdMotor2Axis := TSFVec3f.Create(Self, 'motor2Axis', Vector3Single(0, 0, 0));
  AddField(FFdMotor2Axis);

  FFdMotor3Axis := TSFVec3f.Create(Self, 'motor3Axis', Vector3Single(0, 0, 0));
  AddField(FFdMotor3Axis);

  FFdStop1Bounce := TSFFloat.Create(Self, 'stop1Bounce', 0);
  AddField(FFdStop1Bounce);
  { X3D specification comment: [0,1] }

  FFdStop1ErrorCorrection := TSFFloat.Create(Self, 'stop1ErrorCorrection', 0.8);
  AddField(FFdStop1ErrorCorrection);
  { X3D specification comment: [0,1] }

  FFdStop2Bounce := TSFFloat.Create(Self, 'stop2Bounce', 0);
  AddField(FFdStop2Bounce);
  { X3D specification comment: [0,1] }

  FFdStop2ErrorCorrection := TSFFloat.Create(Self, 'stop2ErrorCorrection', 0.8);
  AddField(FFdStop2ErrorCorrection);
  { X3D specification comment: [0,1] }

  FFdStop3Bounce := TSFFloat.Create(Self, 'stop3Bounce', 0);
  AddField(FFdStop3Bounce);
  { X3D specification comment: [0,1] }

  FFdStop3ErrorCorrection := TSFFloat.Create(Self, 'stop3ErrorCorrection', 0.8);
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

  FFdAutoCalc := TSFBool.Create(Self, 'autoCalc', false);
  FFdAutoCalc.Exposed := false;
  AddField(FFdAutoCalc);
end;

class function TMotorJointNode.ClassX3DType: string;
begin
  Result := 'MotorJoint';
end;

class function TMotorJointNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TRigidBodyNode.CreateNode;
begin
  inherited;

  FFdAngularDampingFactor := TSFFloat.Create(Self, 'angularDampingFactor', 0.001);
  AddField(FFdAngularDampingFactor);
  { X3D specification comment: [0,1] }

  FFdAngularVelocity := TSFVec3f.Create(Self, 'angularVelocity', Vector3Single(0, 0, 0));
  AddField(FFdAngularVelocity);
  { X3D specification comment: (-Inf,Inf) }

  FFdAutoDamp := TSFBool.Create(Self, 'autoDamp', false);
  AddField(FFdAutoDamp);

  FFdAutoDisable := TSFBool.Create(Self, 'autoDisable', false);
  AddField(FFdAutoDisable);

  FFdCenterOfMass := TSFVec3f.Create(Self, 'centerOfMass', Vector3Single(0, 0, 0));
  AddField(FFdCenterOfMass);
  { X3D specification comment: (-Inf,Inf) }

  FFdDisableAngularSpeed := TSFFloat.Create(Self, 'disableAngularSpeed', 0);
  AddField(FFdDisableAngularSpeed);
  { X3D specification comment: [0,Inf) }

  FFdDisableLinearSpeed := TSFFloat.Create(Self, 'disableLinearSpeed', 0);
  AddField(FFdDisableLinearSpeed);
  { X3D specification comment: [0,Inf) }

  FFdDisableTime := TSFFloat.Create(Self, 'disableTime', 0);
  AddField(FFdDisableTime);
  { X3D specification comment: [0,Inf) }

  FFdEnabled := TSFBool.Create(Self, 'enabled', true);
  AddField(FFdEnabled);

  FFdFiniteRotationAxis := TSFVec3f.Create(Self, 'finiteRotationAxis', Vector3Single(0, 0, 0));
  AddField(FFdFiniteRotationAxis);
  { X3D specification comment: [-1,1] }

  FFdFixed := TSFBool.Create(Self, 'fixed', false);
  AddField(FFdFixed);

  FFdForces := TMFVec3f.Create(Self, 'forces', []);
  AddField(FFdForces);

  FFdGeometry := TMFNode.Create(Self, 'geometry', [TAbstractNBodyCollidableNode]);
  AddField(FFdGeometry);

  FFdInertia := TSFMatrix3f.Create(Self, 'inertia', IdentityMatrix3Single);
  AddField(FFdInertia);

  FFdLinearDampingFactor := TSFFloat.Create(Self, 'linearDampingFactor', 0.001);
  AddField(FFdLinearDampingFactor);
  { X3D specification comment: [0,1] }

  FFdLinearVelocity := TSFVec3f.Create(Self, 'linearVelocity', Vector3Single(0, 0, 0));
  AddField(FFdLinearVelocity);
  { X3D specification comment: (-Inf,Inf) }

  FFdMass := TSFFloat.Create(Self, 'mass', 1);
  AddField(FFdMass);
  { X3D specification comment: (0,Inf) }

  FFdMassDensityModel := TSFNode.Create(Self, 'massDensityModel', [TSphereNode, TBoxNode, TConeNode]);
  AddField(FFdMassDensityModel);

  FFdOrientation := TSFRotation.Create(Self, 'orientation', Vector3Single(0, 0, 1), 0);
  AddField(FFdOrientation);
  { X3D specification comment: [0,1] }

  FFdPosition := TSFVec3f.Create(Self, 'position', Vector3Single(0, 0, 0));
  AddField(FFdPosition);
  { X3D specification comment: (-Inf,Inf) }

  FFdTorques := TMFVec3f.Create(Self, 'torques', []);
  AddField(FFdTorques);

  FFdUseFiniteRotation := TSFBool.Create(Self, 'useFiniteRotation', false);
  AddField(FFdUseFiniteRotation);

  FFdUseGlobalGravity := TSFBool.Create(Self, 'useGlobalGravity', true);
  AddField(FFdUseGlobalGravity);

  DefaultContainerField := 'bodies';
end;

class function TRigidBodyNode.ClassX3DType: string;
begin
  Result := 'RigidBody';
end;

class function TRigidBodyNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TRigidBodyCollectionNode.CreateNode;
begin
  inherited;

  FEventSet_contacts := TMFNodeEvent.Create(Self, 'set_contacts', true);
  AddEvent(FEventSet_contacts);

  FFdAutoDisable := TSFBool.Create(Self, 'autoDisable', false);
  AddField(FFdAutoDisable);

  FFdBodies := TMFNode.Create(Self, 'bodies', [TRigidBodyNode]);
  AddField(FFdBodies);

  FFdConstantForceMix := TSFFloat.Create(Self, 'constantForceMix', 0.0001);
  AddField(FFdConstantForceMix);
  { X3D specification comment: [0,Inf) }

  FFdContactSurfaceThickness := TSFFloat.Create(Self, 'contactSurfaceThickness', 0);
  AddField(FFdContactSurfaceThickness);
  { X3D specification comment: [0,Inf) }

  FFdDisableAngularSpeed := TSFFloat.Create(Self, 'disableAngularSpeed', 0);
  AddField(FFdDisableAngularSpeed);
  { X3D specification comment: [0,Inf) }

  FFdDisableLinearSpeed := TSFFloat.Create(Self, 'disableLinearSpeed', 0);
  AddField(FFdDisableLinearSpeed);
  { X3D specification comment: [0,Inf) }

  FFdDisableTime := TSFFloat.Create(Self, 'disableTime', 0);
  AddField(FFdDisableTime);
  { X3D specification comment: [0,Inf) }

  FFdEnabled := TSFBool.Create(Self, 'enabled', true);
  AddField(FFdEnabled);

  FFdErrorCorrection := TSFFloat.Create(Self, 'errorCorrection', 0.8);
  AddField(FFdErrorCorrection);
  { X3D specification comment: [0,1] }

  FFdGravity := TSFVec3f.Create(Self, 'gravity', Vector3Single(0, -9.8, 0));
  AddField(FFdGravity);

  FFdIterations := TSFInt32.Create(Self, 'iterations', 10);
  AddField(FFdIterations);
  { X3D specification comment: [0,Inf) }

  FFdJoints := TMFNode.Create(Self, 'joints', [TAbstractRigidJointNode]);
  AddField(FFdJoints);

  FFdMaxCorrectionSpeed := TSFFloat.Create(Self, 'maxCorrectionSpeed', -1);
  AddField(FFdMaxCorrectionSpeed);
  { X3D specification comment: [0,Inf) or -1 }

  FFdPreferAccuracy := TSFBool.Create(Self, 'preferAccuracy', false);
  AddField(FFdPreferAccuracy);

  FFdCollider := TSFNode.Create(Self, 'collider', [TCollisionCollectionNode]);
  FFdCollider.Exposed := false;
  AddField(FFdCollider);
end;

class function TRigidBodyCollectionNode.ClassX3DType: string;
begin
  Result := 'RigidBodyCollection';
end;

class function TRigidBodyCollectionNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TSingleAxisHingeJointNode.CreateNode;
begin
  inherited;

  FFdAnchorPoint := TSFVec3f.Create(Self, 'anchorPoint', Vector3Single(0, 0, 0));
  AddField(FFdAnchorPoint);

  FFdAxis := TSFVec3f.Create(Self, 'axis', Vector3Single(0, 0, 0));
  AddField(FFdAxis);

  FFdMaxAngle := TSFFloat.Create(Self, 'maxAngle', Pi);
  AddField(FFdMaxAngle);

  FFdMinAngle := TSFFloat.Create(Self, 'minAngle', -Pi);
  AddField(FFdMinAngle);

  FFdStopBounce := TSFFloat.Create(Self, 'stopBounce', 0);
  AddField(FFdStopBounce);
  { X3D specification comment: [0,1] }

  FFdStopErrorCorrection := TSFFloat.Create(Self, 'stopErrorCorrection', 0.8);
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

class function TSingleAxisHingeJointNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TSliderJointNode.CreateNode;
begin
  inherited;

  FFdAxis := TSFVec3f.Create(Self, 'axis', Vector3Single(0, 1, 0));
  AddField(FFdAxis);

  FFdMaxSeparation := TSFFloat.Create(Self, 'maxSeparation', 1);
  AddField(FFdMaxSeparation);
  { X3D specification comment: [0,Inf) }

  FFdMinSeparation := TSFFloat.Create(Self, 'minSeparation', 0);
  AddField(FFdMinSeparation);
  { X3D specification comment: [0,Inf) }

  FFdSliderForce := TSFFloat.Create(Self, 'sliderForce', 0);
  AddField(FFdSliderForce);
  { X3D specification comment: [0,Inf) }

  FFdStopBounce := TSFFloat.Create(Self, 'stopBounce', 0);
  AddField(FFdStopBounce);
  { X3D specification comment: [0,1] }

  FFdStopErrorCorrection := TSFFloat.Create(Self, 'stopErrorCorrection', 1);
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

class function TSliderJointNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TUniversalJointNode.CreateNode;
begin
  inherited;

  FFdAnchorPoint := TSFVec3f.Create(Self, 'anchorPoint', Vector3Single(0, 0, 0));
  AddField(FFdAnchorPoint);

  FFdAxis1 := TSFVec3f.Create(Self, 'axis1', Vector3Single(0, 0, 0));
  AddField(FFdAxis1);

  FFdAxis2 := TSFVec3f.Create(Self, 'axis2', Vector3Single(0, 0, 0));
  AddField(FFdAxis2);

  FFdStopBounce1 := TSFFloat.Create(Self, 'stopBounce1', 0);
  AddField(FFdStopBounce1);
  { X3D specification comment: [0,1] }

  FFdStop1ErrorCorrection := TSFFloat.Create(Self, 'stop1ErrorCorrection', 0.8);
  AddField(FFdStop1ErrorCorrection);
  { X3D specification comment: [0,1] }

  FFdStop2Bounce := TSFFloat.Create(Self, 'stop2Bounce', 0);
  AddField(FFdStop2Bounce);
  { X3D specification comment: [0,1] }

  FFdStop2ErrorCorrection := TSFFloat.Create(Self, 'stop2ErrorCorrection', 0.8);
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

class function TUniversalJointNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
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
