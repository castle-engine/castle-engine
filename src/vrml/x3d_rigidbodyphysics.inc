{
  Copyright 2008-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { }
  TNodeX3DNBodyCollidableNode = class(TNodeX3DChildNode, INodeX3DBoundedObject)
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
  end;

  TNodeX3DNBodyCollisionSpaceNode = class(TNodeX3DNode, INodeX3DBoundedObject)
  public
    procedure CreateNode; override;

    private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    private FFdBboxCenter: TSFVec3f;
    public property FdBboxCenter: TSFVec3f read FFdBboxCenter;

    private FFdBboxSize: TSFVec3f;
    public property FdBboxSize: TSFVec3f read FFdBboxSize;
  end;

  TNodeX3DRigidJointNode = class(TNodeX3DNode)
  public
    procedure CreateNode; override;

    private FFdBody1: TSFNode;
    public property FdBody1: TSFNode read FFdBody1;

    private FFdBody2: TSFNode;
    public property FdBody2: TSFNode read FFdBody2;

    private FFdForceOutput: TMFString;
    public property FdForceOutput: TMFString read FFdForceOutput;
  end;

  TNodeBallJoint = class(TNodeX3DRigidJointNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAnchorPoint: TSFVec3f;
    public property FdAnchorPoint: TSFVec3f read FFdAnchorPoint;

    { Event: SFVec3f, out } { }
    private FEventBody1AnchorPoint: TVRMLEvent;
    public property EventBody1AnchorPoint: TVRMLEvent read FEventBody1AnchorPoint;

    { Event: SFVec3f, out } { }
    private FEventBody2AnchorPoint: TVRMLEvent;
    public property EventBody2AnchorPoint: TVRMLEvent read FEventBody2AnchorPoint;
  end;

  TNodeCollidableOffset = class(TNodeX3DNBodyCollidableNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdCollidable: TSFNode;
    public property FdCollidable: TSFNode read FFdCollidable;
  end;

  TNodeCollidableShape = class(TNodeX3DNBodyCollidableNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdShape: TSFNode;
    public property FdShape: TSFNode read FFdShape;
  end;

  TNodeCollisionCollection = class(TNodeX3DChildNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
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
  end;

  TNodeCollisionSensor = class(TNodeX3DSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdCollider: TSFNode;
    public property FdCollider: TSFNode read FFdCollider;

    { Event: MFNode, out } { }
    private FEventIntersections: TVRMLEvent;
    public property EventIntersections: TVRMLEvent read FEventIntersections;

    { Event: MFNode, out } { }
    private FEventContacts: TVRMLEvent;
    public property EventContacts: TVRMLEvent read FEventContacts;
  end;

  TNodeCollisionSpace = class(TNodeX3DNBodyCollisionSpaceNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdCollidables: TMFNode;
    public property FdCollidables: TMFNode read FFdCollidables;

    private FFdUseGeometry: TSFBool;
    public property FdUseGeometry: TSFBool read FFdUseGeometry;
  end;

  TNodeContact = class(TNodeX3DNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
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
  end;

  TNodeDoubleAxisHingeJoint = class(TNodeX3DRigidJointNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
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

    { Event: SFVec3f, out } { }
    private FEventBody1AnchorPoint: TVRMLEvent;
    public property EventBody1AnchorPoint: TVRMLEvent read FEventBody1AnchorPoint;

    { Event: SFVec3f, out } { }
    private FEventBody1Axis: TVRMLEvent;
    public property EventBody1Axis: TVRMLEvent read FEventBody1Axis;

    { Event: SFVec3f, out } { }
    private FEventBody2AnchorPoint: TVRMLEvent;
    public property EventBody2AnchorPoint: TVRMLEvent read FEventBody2AnchorPoint;

    { Event: SFVec3f, out } { }
    private FEventBody2Axis: TVRMLEvent;
    public property EventBody2Axis: TVRMLEvent read FEventBody2Axis;

    { Event: SFFloat, out } { }
    private FEventHinge1Angle: TVRMLEvent;
    public property EventHinge1Angle: TVRMLEvent read FEventHinge1Angle;

    { Event: SFFloat, out } { }
    private FEventHinge1AngleRate: TVRMLEvent;
    public property EventHinge1AngleRate: TVRMLEvent read FEventHinge1AngleRate;

    { Event: SFFloat, out } { }
    private FEventHinge2Angle: TVRMLEvent;
    public property EventHinge2Angle: TVRMLEvent read FEventHinge2Angle;

    { Event: SFFloat, out } { }
    private FEventHinge2AngleRate: TVRMLEvent;
    public property EventHinge2AngleRate: TVRMLEvent read FEventHinge2AngleRate;
  end;

  TNodeMotorJoint = class(TNodeX3DRigidJointNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
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

    { Event: SFFloat, out } { }
    private FEventMotor1Angle: TVRMLEvent;
    public property EventMotor1Angle: TVRMLEvent read FEventMotor1Angle;

    { Event: SFFloat, out } { }
    private FEventMotor1AngleRate: TVRMLEvent;
    public property EventMotor1AngleRate: TVRMLEvent read FEventMotor1AngleRate;

    { Event: SFFloat, out } { }
    private FEventMotor2Angle: TVRMLEvent;
    public property EventMotor2Angle: TVRMLEvent read FEventMotor2Angle;

    { Event: SFFloat, out } { }
    private FEventMotor2AngleRate: TVRMLEvent;
    public property EventMotor2AngleRate: TVRMLEvent read FEventMotor2AngleRate;

    { Event: SFFloat, out } { }
    private FEventMotor3Angle: TVRMLEvent;
    public property EventMotor3Angle: TVRMLEvent read FEventMotor3Angle;

    { Event: SFFloat, out } { }
    private FEventMotor3AngleRate: TVRMLEvent;
    public property EventMotor3AngleRate: TVRMLEvent read FEventMotor3AngleRate;

    private FFdAutoCalc: TSFBool;
    public property FdAutoCalc: TSFBool read FFdAutoCalc;
  end;

  TNodeRigidBody = class(TNodeX3DNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
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
  end;

  TNodeRigidBodyCollection = class(TNodeX3DChildNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFNode, in } { }
    private FEventSet_contacts: TVRMLEvent;
    public property EventSet_contacts: TVRMLEvent read FEventSet_contacts;

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
  end;

  TNodeSingleAxisHingeJoint = class(TNodeX3DRigidJointNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
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

    { Event: SFFloat, out } { }
    private FEventAngle: TVRMLEvent;
    public property EventAngle: TVRMLEvent read FEventAngle;

    { Event: SFFloat, out } { }
    private FEventAngleRate: TVRMLEvent;
    public property EventAngleRate: TVRMLEvent read FEventAngleRate;

    { Event: SFVec3f, out } { }
    private FEventBody1AnchorPoint: TVRMLEvent;
    public property EventBody1AnchorPoint: TVRMLEvent read FEventBody1AnchorPoint;

    { Event: SFVec3f, out } { }
    private FEventBody2AnchorPoint: TVRMLEvent;
    public property EventBody2AnchorPoint: TVRMLEvent read FEventBody2AnchorPoint;
  end;

  TNodeSliderJoint = class(TNodeX3DRigidJointNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAxis: TSFVec3f;
    public property FdAxis: TSFVec3f read FFdAxis;

    private FFdMaxSeparation: TSFFloat;
    public property FdMaxSeparation: TSFFloat read FFdMaxSeparation;

    private FFdMinSeparation: TSFFloat;
    public property FdMinSeparation: TSFFloat read FFdMinSeparation;

    private FFdStopBounce: TSFFloat;
    public property FdStopBounce: TSFFloat read FFdStopBounce;

    private FFdStopErrorCorrection: TSFFloat;
    public property FdStopErrorCorrection: TSFFloat read FFdStopErrorCorrection;

    { Event: SFFloat, out } { }
    private FEventSeparation: TVRMLEvent;
    public property EventSeparation: TVRMLEvent read FEventSeparation;

    { Event: SFFloat, out } { }
    private FEventSeparationRate: TVRMLEvent;
    public property EventSeparationRate: TVRMLEvent read FEventSeparationRate;
  end;

  TNodeUniversalJoint = class(TNodeX3DRigidJointNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
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

    { Event: SFVec3f, out } { }
    private FEventBody1AnchorPoint: TVRMLEvent;
    public property EventBody1AnchorPoint: TVRMLEvent read FEventBody1AnchorPoint;

    { Event: SFVec3f, out } { }
    private FEventBody1Axis: TVRMLEvent;
    public property EventBody1Axis: TVRMLEvent read FEventBody1Axis;

    { Event: SFVec3f, out } { }
    private FEventBody2AnchorPoint: TVRMLEvent;
    public property EventBody2AnchorPoint: TVRMLEvent read FEventBody2AnchorPoint;

    { Event: SFVec3f, out } { }
    private FEventBody2Axis: TVRMLEvent;
    public property EventBody2Axis: TVRMLEvent read FEventBody2Axis;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TNodeX3DNBodyCollidableNode.CreateNode;
begin
  inherited;

  FFdenabled := TSFBool.Create(Self, 'enabled', true);
  Fields.Add(FFdenabled);

  FFdrotation := TSFRotation.Create(Self, 'rotation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdrotation);
  { X3D specification comment: [0,1] }

  FFdtranslation := TSFVec3f.Create(Self, 'translation', Vector3Single(0, 0, 0));
  Fields.Add(FFdtranslation);
  { X3D specification comment: (-Inf,Inf) }

  FFdbboxCenter := TSFVec3f.Create(Self, 'bboxCenter', Vector3Single(0, 0, 0));
  FFdbboxCenter.Exposed := false;
  Fields.Add(FFdbboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdbboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
  FFdbboxSize.Exposed := false;
  Fields.Add(FFdbboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }
end;

procedure TNodeX3DNBodyCollisionSpaceNode.CreateNode;
begin
  inherited;

  FFdenabled := TSFBool.Create(Self, 'enabled', true);
  Fields.Add(FFdenabled);

  FFdbboxCenter := TSFVec3f.Create(Self, 'bboxCenter', Vector3Single(0, 0, 0));
  FFdbboxCenter.Exposed := false;
  Fields.Add(FFdbboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdbboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
  FFdbboxSize.Exposed := false;
  Fields.Add(FFdbboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }
end;

procedure TNodeX3DRigidJointNode.CreateNode;
begin
  inherited;

  FFdbody1 := TSFNode.Create(Self, 'body1', [TNodeRigidBody]);
  Fields.Add(FFdbody1);

  FFdbody2 := TSFNode.Create(Self, 'body2', [TNodeRigidBody]);
  Fields.Add(FFdbody2);

  FFdforceOutput := TMFString.Create(Self, 'forceOutput', ['NONE']);
  Fields.Add(FFdforceOutput);
  { X3D specification comment: ["ALL","NONE",...] }

  DefaultContainerField := 'joints';
end;

procedure TNodeBallJoint.CreateNode;
begin
  inherited;

  FFdanchorPoint := TSFVec3f.Create(Self, 'anchorPoint', Vector3Single(0, 0, 0));
  Fields.Add(FFdanchorPoint);

  FEventbody1AnchorPoint := TVRMLEvent.Create(Self, 'body1AnchorPoint', TSFVec3f, false);
  Events.Add(FEventbody1AnchorPoint);

  FEventbody2AnchorPoint := TVRMLEvent.Create(Self, 'body2AnchorPoint', TSFVec3f, false);
  Events.Add(FEventbody2AnchorPoint);
end;

class function TNodeBallJoint.ClassNodeTypeName: string;
begin
  Result := 'BallJoint';
end;

class function TNodeBallJoint.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeCollidableOffset.CreateNode;
begin
  inherited;

  FFdcollidable := TSFNode.Create(Self, 'collidable', [TNodeX3DNBodyCollidableNode]);
  FFdcollidable.Exposed := false;
  Fields.Add(FFdcollidable);
end;

class function TNodeCollidableOffset.ClassNodeTypeName: string;
begin
  Result := 'CollidableOffset';
end;

class function TNodeCollidableOffset.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeCollidableShape.CreateNode;
begin
  inherited;

  FFdshape := TSFNode.Create(Self, 'shape', [TNodeShape]);
  FFdshape.Exposed := false;
  Fields.Add(FFdshape);
end;

class function TNodeCollidableShape.ClassNodeTypeName: string;
begin
  Result := 'CollidableShape';
end;

class function TNodeCollidableShape.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeCollisionCollection.CreateNode;
begin
  inherited;

  FFdappliedParameters := TMFString.Create(Self, 'appliedParameters', ['BOUNCE']);
  Fields.Add(FFdappliedParameters);
  { X3D specification comment: [] }

  FFdbounce := TSFFloat.Create(Self, 'bounce', 0);
  Fields.Add(FFdbounce);
  { X3D specification comment: [0,1] }

  FFdcollidables := TMFNode.Create(Self, 'collidables', [TNodeX3DNBodyCollisionSpaceNode, TNodeX3DNBodyCollidableNode]);
  Fields.Add(FFdcollidables);

  FFdenabled := TSFBool.Create(Self, 'enabled', true);
  Fields.Add(FFdenabled);

  FFdfrictionCoefficients := TSFVec2f.Create(Self, 'frictionCoefficients', Vector2Single(0, 0));
  Fields.Add(FFdfrictionCoefficients);
  { X3D specification comment: [0,Inf) }

  FFdminBounceSpeed := TSFFloat.Create(Self, 'minBounceSpeed', 0.1);
  Fields.Add(FFdminBounceSpeed);
  { X3D specification comment: [0,Inf) }

  FFdslipFactors := TSFVec2f.Create(Self, 'slipFactors', Vector2Single(0, 0));
  Fields.Add(FFdslipFactors);
  { X3D specification comment: (-Inf,Inf) }

  FFdsoftnessConstantForceMix := TSFFloat.Create(Self, 'softnessConstantForceMix', 0.0001);
  Fields.Add(FFdsoftnessConstantForceMix);
  { X3D specification comment: [0,1] }

  FFdsoftnessErrorCorrection := TSFFloat.Create(Self, 'softnessErrorCorrection', 0.8);
  Fields.Add(FFdsoftnessErrorCorrection);
  { X3D specification comment: [0,1] }

  FFdsurfaceSpeed := TSFVec2f.Create(Self, 'surfaceSpeed', Vector2Single(0, 0));
  Fields.Add(FFdsurfaceSpeed);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TNodeCollisionCollection.ClassNodeTypeName: string;
begin
  Result := 'CollisionCollection';
end;

class function TNodeCollisionCollection.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeCollisionSensor.CreateNode;
begin
  inherited;

  FFdcollider := TSFNode.Create(Self, 'collider', [TNodeCollisionCollection]);
  Fields.Add(FFdcollider);

  FEventintersections := TVRMLEvent.Create(Self, 'intersections', TMFNode, false);
  Events.Add(FEventintersections);

  FEventcontacts := TVRMLEvent.Create(Self, 'contacts', TMFNode, false);
  Events.Add(FEventcontacts);
end;

class function TNodeCollisionSensor.ClassNodeTypeName: string;
begin
  Result := 'CollisionSensor';
end;

class function TNodeCollisionSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeCollisionSpace.CreateNode;
begin
  inherited;

  FFdcollidables := TMFNode.Create(Self, 'collidables', [TNodeX3DNBodyCollisionSpaceNode, TNodeX3DNBodyCollidableNode]);
  Fields.Add(FFdcollidables);

  FFduseGeometry := TSFBool.Create(Self, 'useGeometry', false);
  Fields.Add(FFduseGeometry);

  DefaultContainerField := 'children';
end;

class function TNodeCollisionSpace.ClassNodeTypeName: string;
begin
  Result := 'CollisionSpace';
end;

class function TNodeCollisionSpace.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeContact.CreateNode;
begin
  inherited;

  FFdappliedParameters := TMFString.Create(Self, 'appliedParameters', ['BOUNCE']);
  Fields.Add(FFdappliedParameters);
  { X3D specification comment: [] }

  FFdbody1 := TSFNode.Create(Self, 'body1', [TNodeRigidBody]);
  Fields.Add(FFdbody1);

  FFdbody2 := TSFNode.Create(Self, 'body2', [TNodeRigidBody]);
  Fields.Add(FFdbody2);

  FFdbounce := TSFFloat.Create(Self, 'bounce', 0);
  Fields.Add(FFdbounce);
  { X3D specification comment: [0,1] }

  FFdcontactNormal := TSFVec3f.Create(Self, 'contactNormal', Vector3Single(0, 1, 0));
  Fields.Add(FFdcontactNormal);
  { X3D specification comment: (-Inf,Inf) }

  FFddepth := TSFFloat.Create(Self, 'depth', 0);
  Fields.Add(FFddepth);
  { X3D specification comment: (-Inf,Inf) }

  FFdfrictionCoefficients := TSFVec2f.Create(Self, 'frictionCoefficients', Vector2Single(0, 0));
  Fields.Add(FFdfrictionCoefficients);
  { X3D specification comment: [0,Inf) }

  FFdfrictionDirection := TSFVec3f.Create(Self, 'frictionDirection', Vector3Single(0, 1, 0));
  Fields.Add(FFdfrictionDirection);
  { X3D specification comment: (-Inf,Inf) }

  FFdgeometry1 := TSFNode.Create(Self, 'geometry1', [TNodeX3DNBodyCollidableNode]);
  Fields.Add(FFdgeometry1);

  FFdgeometry2 := TSFNode.Create(Self, 'geometry2', [TNodeX3DNBodyCollidableNode]);
  Fields.Add(FFdgeometry2);

  FFdminbounceSpeed := TSFFloat.Create(Self, 'minbounceSpeed', 0);
  Fields.Add(FFdminbounceSpeed);
  { X3D specification comment: [0,Inf) }

  FFdposition := TSFVec3f.Create(Self, 'position', Vector3Single(0, 0, 0));
  Fields.Add(FFdposition);
  { X3D specification comment: (-Inf,Inf) }

  FFdslipCoefficients := TSFVec2f.Create(Self, 'slipCoefficients', Vector2Single(0, 0));
  Fields.Add(FFdslipCoefficients);
  { X3D specification comment: (-Inf,Inf) }

  FFdsoftnessConstantForceMix := TSFFloat.Create(Self, 'softnessConstantForceMix', 0.0001);
  Fields.Add(FFdsoftnessConstantForceMix);
  { X3D specification comment: [0,1] }

  FFdsoftnessErrorCorrection := TSFFloat.Create(Self, 'softnessErrorCorrection', 0.8);
  Fields.Add(FFdsoftnessErrorCorrection);
  { X3D specification comment: [0,1] }

  FFdsurfaceSpeed := TSFVec2f.Create(Self, 'surfaceSpeed', Vector2Single(0, 0));
  Fields.Add(FFdsurfaceSpeed);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeContact.ClassNodeTypeName: string;
begin
  Result := 'Contact';
end;

class function TNodeContact.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeDoubleAxisHingeJoint.CreateNode;
begin
  inherited;

  FFdanchorPoint := TSFVec3f.Create(Self, 'anchorPoint', Vector3Single(0, 0, 0));
  Fields.Add(FFdanchorPoint);

  FFdaxis1 := TSFVec3f.Create(Self, 'axis1', Vector3Single(0, 0, 0));
  Fields.Add(FFdaxis1);

  FFdaxis2 := TSFVec3f.Create(Self, 'axis2', Vector3Single(0, 0, 0));
  Fields.Add(FFdaxis2);

  FFddesiredAngularVelocity1 := TSFFloat.Create(Self, 'desiredAngularVelocity1', 0);
  Fields.Add(FFddesiredAngularVelocity1);
  { X3D specification comment: (-Inf,Inf) }

  FFddesiredAngularVelocity2 := TSFFloat.Create(Self, 'desiredAngularVelocity2', 0);
  Fields.Add(FFddesiredAngularVelocity2);
  { X3D specification comment: (-Inf,Inf) }

  FFdmaxAngle1 := TSFFloat.Create(Self, 'maxAngle1', Pi);
  Fields.Add(FFdmaxAngle1);
  { X3D specification comment: [-Pi,Pi] }

  FFdmaxTorque1 := TSFFloat.Create(Self, 'maxTorque1', 0);
  Fields.Add(FFdmaxTorque1);
  { X3D specification comment: (-Inf,Inf) }

  FFdmaxTorque2 := TSFFloat.Create(Self, 'maxTorque2', 0);
  Fields.Add(FFdmaxTorque2);
  { X3D specification comment: (-Inf,Inf) }

  FFdminAngle1 := TSFFloat.Create(Self, 'minAngle1', -Pi);
  Fields.Add(FFdminAngle1);
  { X3D specification comment: [-Pi,Pi] }

  FFdstopBounce1 := TSFFloat.Create(Self, 'stopBounce1', 0);
  Fields.Add(FFdstopBounce1);
  { X3D specification comment: [0,1] }

  FFdstopConstantForceMix1 := TSFFloat.Create(Self, 'stopConstantForceMix1', 0.001);
  Fields.Add(FFdstopConstantForceMix1);
  { X3D specification comment: [0,Inf) }

  FFdstopErrorCorrection1 := TSFFloat.Create(Self, 'stopErrorCorrection1', 0.8);
  Fields.Add(FFdstopErrorCorrection1);
  { X3D specification comment: [0,1] }

  FFdsuspensionErrorCorrection := TSFFloat.Create(Self, 'suspensionErrorCorrection', 0.8);
  Fields.Add(FFdsuspensionErrorCorrection);
  { X3D specification comment: [0,1] }

  FFdsuspensionForce := TSFFloat.Create(Self, 'suspensionForce', 0);
  Fields.Add(FFdsuspensionForce);
  { X3D specification comment: [0,Inf) }

  FEventbody1AnchorPoint := TVRMLEvent.Create(Self, 'body1AnchorPoint', TSFVec3f, false);
  Events.Add(FEventbody1AnchorPoint);

  FEventbody1Axis := TVRMLEvent.Create(Self, 'body1Axis', TSFVec3f, false);
  Events.Add(FEventbody1Axis);

  FEventbody2AnchorPoint := TVRMLEvent.Create(Self, 'body2AnchorPoint', TSFVec3f, false);
  Events.Add(FEventbody2AnchorPoint);

  FEventbody2Axis := TVRMLEvent.Create(Self, 'body2Axis', TSFVec3f, false);
  Events.Add(FEventbody2Axis);

  FEventhinge1Angle := TVRMLEvent.Create(Self, 'hinge1Angle', TSFFloat, false);
  Events.Add(FEventhinge1Angle);

  FEventhinge1AngleRate := TVRMLEvent.Create(Self, 'hinge1AngleRate', TSFFloat, false);
  Events.Add(FEventhinge1AngleRate);

  FEventhinge2Angle := TVRMLEvent.Create(Self, 'hinge2Angle', TSFFloat, false);
  Events.Add(FEventhinge2Angle);

  FEventhinge2AngleRate := TVRMLEvent.Create(Self, 'hinge2AngleRate', TSFFloat, false);
  Events.Add(FEventhinge2AngleRate);
end;

class function TNodeDoubleAxisHingeJoint.ClassNodeTypeName: string;
begin
  Result := 'DoubleAxisHingeJoint';
end;

class function TNodeDoubleAxisHingeJoint.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeMotorJoint.CreateNode;
begin
  inherited;

  FFdaxis1Angle := TSFFloat.Create(Self, 'axis1Angle', 0);
  Fields.Add(FFdaxis1Angle);
  { X3D specification comment: [-Pi,Pi] }

  FFdaxis1Torque := TSFFloat.Create(Self, 'axis1Torque', 0);
  Fields.Add(FFdaxis1Torque);
  { X3D specification comment: (-Inf,Inf) }

  FFdaxis2Angle := TSFFloat.Create(Self, 'axis2Angle', 0);
  Fields.Add(FFdaxis2Angle);
  { X3D specification comment: [-Pi,Pi] }

  FFdaxis2Torque := TSFFloat.Create(Self, 'axis2Torque', 0);
  Fields.Add(FFdaxis2Torque);
  { X3D specification comment: (-Inf,Inf) }

  FFdaxis3Angle := TSFFloat.Create(Self, 'axis3Angle', 0);
  Fields.Add(FFdaxis3Angle);
  { X3D specification comment: [-Pi,Pi] }

  FFdaxis3Torque := TSFFloat.Create(Self, 'axis3Torque', 0);
  Fields.Add(FFdaxis3Torque);
  { X3D specification comment: (-Inf,Inf) }

  FFdenabledAxes := TSFInt32.Create(Self, 'enabledAxes', 1);
  Fields.Add(FFdenabledAxes);
  { X3D specification comment: [0,3] }

  FFdmotor1Axis := TSFVec3f.Create(Self, 'motor1Axis', Vector3Single(0, 0, 0));
  Fields.Add(FFdmotor1Axis);

  FFdmotor2Axis := TSFVec3f.Create(Self, 'motor2Axis', Vector3Single(0, 0, 0));
  Fields.Add(FFdmotor2Axis);

  FFdmotor3Axis := TSFVec3f.Create(Self, 'motor3Axis', Vector3Single(0, 0, 0));
  Fields.Add(FFdmotor3Axis);

  FFdstop1Bounce := TSFFloat.Create(Self, 'stop1Bounce', 0);
  Fields.Add(FFdstop1Bounce);
  { X3D specification comment: [0,1] }

  FFdstop1ErrorCorrection := TSFFloat.Create(Self, 'stop1ErrorCorrection', 0.8);
  Fields.Add(FFdstop1ErrorCorrection);
  { X3D specification comment: [0,1] }

  FFdstop2Bounce := TSFFloat.Create(Self, 'stop2Bounce', 0);
  Fields.Add(FFdstop2Bounce);
  { X3D specification comment: [0,1] }

  FFdstop2ErrorCorrection := TSFFloat.Create(Self, 'stop2ErrorCorrection', 0.8);
  Fields.Add(FFdstop2ErrorCorrection);
  { X3D specification comment: [0,1] }

  FFdstop3Bounce := TSFFloat.Create(Self, 'stop3Bounce', 0);
  Fields.Add(FFdstop3Bounce);
  { X3D specification comment: [0,1] }

  FFdstop3ErrorCorrection := TSFFloat.Create(Self, 'stop3ErrorCorrection', 0.8);
  Fields.Add(FFdstop3ErrorCorrection);
  { X3D specification comment: [0,1] }

  FEventmotor1Angle := TVRMLEvent.Create(Self, 'motor1Angle', TSFFloat, false);
  Events.Add(FEventmotor1Angle);

  FEventmotor1AngleRate := TVRMLEvent.Create(Self, 'motor1AngleRate', TSFFloat, false);
  Events.Add(FEventmotor1AngleRate);

  FEventmotor2Angle := TVRMLEvent.Create(Self, 'motor2Angle', TSFFloat, false);
  Events.Add(FEventmotor2Angle);

  FEventmotor2AngleRate := TVRMLEvent.Create(Self, 'motor2AngleRate', TSFFloat, false);
  Events.Add(FEventmotor2AngleRate);

  FEventmotor3Angle := TVRMLEvent.Create(Self, 'motor3Angle', TSFFloat, false);
  Events.Add(FEventmotor3Angle);

  FEventmotor3AngleRate := TVRMLEvent.Create(Self, 'motor3AngleRate', TSFFloat, false);
  Events.Add(FEventmotor3AngleRate);

  FFdautoCalc := TSFBool.Create(Self, 'autoCalc', false);
  FFdautoCalc.Exposed := false;
  Fields.Add(FFdautoCalc);
end;

class function TNodeMotorJoint.ClassNodeTypeName: string;
begin
  Result := 'MotorJoint';
end;

class function TNodeMotorJoint.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeRigidBody.CreateNode;
begin
  inherited;

  FFdangularDampingFactor := TSFFloat.Create(Self, 'angularDampingFactor', 0.001);
  Fields.Add(FFdangularDampingFactor);
  { X3D specification comment: [0,1] }

  FFdangularVelocity := TSFVec3f.Create(Self, 'angularVelocity', Vector3Single(0, 0, 0));
  Fields.Add(FFdangularVelocity);
  { X3D specification comment: (-Inf,Inf) }

  FFdautoDamp := TSFBool.Create(Self, 'autoDamp', false);
  Fields.Add(FFdautoDamp);

  FFdautoDisable := TSFBool.Create(Self, 'autoDisable', false);
  Fields.Add(FFdautoDisable);

  FFdcenterOfMass := TSFVec3f.Create(Self, 'centerOfMass', Vector3Single(0, 0, 0));
  Fields.Add(FFdcenterOfMass);
  { X3D specification comment: (-Inf,Inf) }

  FFddisableAngularSpeed := TSFFloat.Create(Self, 'disableAngularSpeed', 0);
  Fields.Add(FFddisableAngularSpeed);
  { X3D specification comment: [0,Inf) }

  FFddisableLinearSpeed := TSFFloat.Create(Self, 'disableLinearSpeed', 0);
  Fields.Add(FFddisableLinearSpeed);
  { X3D specification comment: [0,Inf) }

  FFddisableTime := TSFFloat.Create(Self, 'disableTime', 0);
  Fields.Add(FFddisableTime);
  { X3D specification comment: [0,Inf) }

  FFdenabled := TSFBool.Create(Self, 'enabled', true);
  Fields.Add(FFdenabled);

  FFdfiniteRotationAxis := TSFVec3f.Create(Self, 'finiteRotationAxis', Vector3Single(0, 0, 0));
  Fields.Add(FFdfiniteRotationAxis);
  { X3D specification comment: [-1,1] }

  FFdfixed := TSFBool.Create(Self, 'fixed', false);
  Fields.Add(FFdfixed);

  FFdforces := TMFVec3f.Create(Self, 'forces', []);
  Fields.Add(FFdforces);

  FFdgeometry := TMFNode.Create(Self, 'geometry', [TNodeX3DNBodyCollidableNode]);
  Fields.Add(FFdgeometry);

  FFdinertia := TSFMatrix3f.Create(Self, 'inertia', IdentityMatrix3Single);
  Fields.Add(FFdinertia);

  FFdlinearDampingFactor := TSFFloat.Create(Self, 'linearDampingFactor', 0.001);
  Fields.Add(FFdlinearDampingFactor);
  { X3D specification comment: [0,1] }

  FFdlinearVelocity := TSFVec3f.Create(Self, 'linearVelocity', Vector3Single(0, 0, 0));
  Fields.Add(FFdlinearVelocity);
  { X3D specification comment: (-Inf,Inf) }

  FFdmass := TSFFloat.Create(Self, 'mass', 1);
  Fields.Add(FFdmass);
  { X3D specification comment: (0,Inf) }

  FFdmassDensityModel := TSFNode.Create(Self, 'massDensityModel', [TNodeSphere, TNodeBox, TNodeCone]);
  Fields.Add(FFdmassDensityModel);

  FFdorientation := TSFRotation.Create(Self, 'orientation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdorientation);
  { X3D specification comment: [0,1] }

  FFdposition := TSFVec3f.Create(Self, 'position', Vector3Single(0, 0, 0));
  Fields.Add(FFdposition);
  { X3D specification comment: (-Inf,Inf) }

  FFdtorques := TMFVec3f.Create(Self, 'torques', []);
  Fields.Add(FFdtorques);

  FFduseFiniteRotation := TSFBool.Create(Self, 'useFiniteRotation', false);
  Fields.Add(FFduseFiniteRotation);

  FFduseGlobalGravity := TSFBool.Create(Self, 'useGlobalGravity', true);
  Fields.Add(FFduseGlobalGravity);

  DefaultContainerField := 'bodies';
end;

class function TNodeRigidBody.ClassNodeTypeName: string;
begin
  Result := 'RigidBody';
end;

class function TNodeRigidBody.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeRigidBodyCollection.CreateNode;
begin
  inherited;

  FEventset_contacts := TVRMLEvent.Create(Self, 'set_contacts', TMFNode, true);
  Events.Add(FEventset_contacts);

  FFdautoDisable := TSFBool.Create(Self, 'autoDisable', false);
  Fields.Add(FFdautoDisable);

  FFdbodies := TMFNode.Create(Self, 'bodies', [TNodeRigidBody]);
  Fields.Add(FFdbodies);

  FFdconstantForceMix := TSFFloat.Create(Self, 'constantForceMix', 0.0001);
  Fields.Add(FFdconstantForceMix);
  { X3D specification comment: [0,Inf) }

  FFdcontactSurfaceThickness := TSFFloat.Create(Self, 'contactSurfaceThickness', 0);
  Fields.Add(FFdcontactSurfaceThickness);
  { X3D specification comment: [0,Inf) }

  FFddisableAngularSpeed := TSFFloat.Create(Self, 'disableAngularSpeed', 0);
  Fields.Add(FFddisableAngularSpeed);
  { X3D specification comment: [0,Inf) }

  FFddisableLinearSpeed := TSFFloat.Create(Self, 'disableLinearSpeed', 0);
  Fields.Add(FFddisableLinearSpeed);
  { X3D specification comment: [0,Inf) }

  FFddisableTime := TSFFloat.Create(Self, 'disableTime', 0);
  Fields.Add(FFddisableTime);
  { X3D specification comment: [0,Inf) }

  FFdenabled := TSFBool.Create(Self, 'enabled', true);
  Fields.Add(FFdenabled);

  FFderrorCorrection := TSFFloat.Create(Self, 'errorCorrection', 0.8);
  Fields.Add(FFderrorCorrection);
  { X3D specification comment: [0,1] }

  FFdgravity := TSFVec3f.Create(Self, 'gravity', Vector3Single(0, -9.8, 0));
  Fields.Add(FFdgravity);

  FFditerations := TSFInt32.Create(Self, 'iterations', 10);
  Fields.Add(FFditerations);
  { X3D specification comment: [0,Inf) }

  FFdjoints := TMFNode.Create(Self, 'joints', [TNodeX3DRigidJointNode]);
  Fields.Add(FFdjoints);

  FFdmaxCorrectionSpeed := TSFFloat.Create(Self, 'maxCorrectionSpeed', -1);
  Fields.Add(FFdmaxCorrectionSpeed);
  { X3D specification comment: [0,Inf) or -1 }

  FFdpreferAccuracy := TSFBool.Create(Self, 'preferAccuracy', false);
  Fields.Add(FFdpreferAccuracy);

  FFdcollider := TSFNode.Create(Self, 'collider', [TNodeCollisionCollection]);
  FFdcollider.Exposed := false;
  Fields.Add(FFdcollider);
end;

class function TNodeRigidBodyCollection.ClassNodeTypeName: string;
begin
  Result := 'RigidBodyCollection';
end;

class function TNodeRigidBodyCollection.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeSingleAxisHingeJoint.CreateNode;
begin
  inherited;

  FFdanchorPoint := TSFVec3f.Create(Self, 'anchorPoint', Vector3Single(0, 0, 0));
  Fields.Add(FFdanchorPoint);

  FFdaxis := TSFVec3f.Create(Self, 'axis', Vector3Single(0, 0, 0));
  Fields.Add(FFdaxis);

  FFdmaxAngle := TSFFloat.Create(Self, 'maxAngle', Pi);
  Fields.Add(FFdmaxAngle);

  FFdminAngle := TSFFloat.Create(Self, 'minAngle', -Pi);
  Fields.Add(FFdminAngle);

  FFdstopBounce := TSFFloat.Create(Self, 'stopBounce', 0);
  Fields.Add(FFdstopBounce);
  { X3D specification comment: [0,1] }

  FFdstopErrorCorrection := TSFFloat.Create(Self, 'stopErrorCorrection', 0.8);
  Fields.Add(FFdstopErrorCorrection);
  { X3D specification comment: [0,1] }

  FEventangle := TVRMLEvent.Create(Self, 'angle', TSFFloat, false);
  Events.Add(FEventangle);

  FEventangleRate := TVRMLEvent.Create(Self, 'angleRate', TSFFloat, false);
  Events.Add(FEventangleRate);

  FEventbody1AnchorPoint := TVRMLEvent.Create(Self, 'body1AnchorPoint', TSFVec3f, false);
  Events.Add(FEventbody1AnchorPoint);

  FEventbody2AnchorPoint := TVRMLEvent.Create(Self, 'body2AnchorPoint', TSFVec3f, false);
  Events.Add(FEventbody2AnchorPoint);
end;

class function TNodeSingleAxisHingeJoint.ClassNodeTypeName: string;
begin
  Result := 'SingleAxisHingeJoint';
end;

class function TNodeSingleAxisHingeJoint.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeSliderJoint.CreateNode;
begin
  inherited;

  FFdaxis := TSFVec3f.Create(Self, 'axis', Vector3Single(0, 1, 0));
  Fields.Add(FFdaxis);

  FFdmaxSeparation := TSFFloat.Create(Self, 'maxSeparation', 1);
  Fields.Add(FFdmaxSeparation);
  { X3D specification comment: [0,Inf) }

  FFdminSeparation := TSFFloat.Create(Self, 'minSeparation', 0);
  Fields.Add(FFdminSeparation);
  { X3D specification comment: [0,Inf) }

  FFdstopBounce := TSFFloat.Create(Self, 'stopBounce', 0);
  Fields.Add(FFdstopBounce);
  { X3D specification comment: [0,1] }

  FFdstopErrorCorrection := TSFFloat.Create(Self, 'stopErrorCorrection', 1);
  Fields.Add(FFdstopErrorCorrection);
  { X3D specification comment: [0,1] }

  FEventseparation := TVRMLEvent.Create(Self, 'separation', TSFFloat, false);
  Events.Add(FEventseparation);

  FEventseparationRate := TVRMLEvent.Create(Self, 'separationRate', TSFFloat, false);
  Events.Add(FEventseparationRate);
end;

class function TNodeSliderJoint.ClassNodeTypeName: string;
begin
  Result := 'SliderJoint';
end;

class function TNodeSliderJoint.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeUniversalJoint.CreateNode;
begin
  inherited;

  FFdanchorPoint := TSFVec3f.Create(Self, 'anchorPoint', Vector3Single(0, 0, 0));
  Fields.Add(FFdanchorPoint);

  FFdaxis1 := TSFVec3f.Create(Self, 'axis1', Vector3Single(0, 0, 0));
  Fields.Add(FFdaxis1);

  FFdaxis2 := TSFVec3f.Create(Self, 'axis2', Vector3Single(0, 0, 0));
  Fields.Add(FFdaxis2);

  FFdstopBounce1 := TSFFloat.Create(Self, 'stopBounce1', 0);
  Fields.Add(FFdstopBounce1);
  { X3D specification comment: [0,1] }

  FFdstop1ErrorCorrection := TSFFloat.Create(Self, 'stop1ErrorCorrection', 0.8);
  Fields.Add(FFdstop1ErrorCorrection);
  { X3D specification comment: [0,1] }

  FFdstop2Bounce := TSFFloat.Create(Self, 'stop2Bounce', 0);
  Fields.Add(FFdstop2Bounce);
  { X3D specification comment: [0,1] }

  FFdstop2ErrorCorrection := TSFFloat.Create(Self, 'stop2ErrorCorrection', 0.8);
  Fields.Add(FFdstop2ErrorCorrection);
  { X3D specification comment: [0,1] }

  FEventbody1AnchorPoint := TVRMLEvent.Create(Self, 'body1AnchorPoint', TSFVec3f, false);
  Events.Add(FEventbody1AnchorPoint);

  FEventbody1Axis := TVRMLEvent.Create(Self, 'body1Axis', TSFVec3f, false);
  Events.Add(FEventbody1Axis);

  FEventbody2AnchorPoint := TVRMLEvent.Create(Self, 'body2AnchorPoint', TSFVec3f, false);
  Events.Add(FEventbody2AnchorPoint);

  FEventbody2Axis := TVRMLEvent.Create(Self, 'body2Axis', TSFVec3f, false);
  Events.Add(FEventbody2Axis);
end;

class function TNodeUniversalJoint.ClassNodeTypeName: string;
begin
  Result := 'UniversalJoint';
end;

class function TNodeUniversalJoint.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterRigidBodyPhysicsNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeBallJoint,
    TNodeCollidableOffset,
    TNodeCollidableShape,
    TNodeCollisionCollection,
    TNodeCollisionSensor,
    TNodeCollisionSpace,
    TNodeContact,
    TNodeDoubleAxisHingeJoint,
    TNodeMotorJoint,
    TNodeRigidBody,
    TNodeRigidBodyCollection,
    TNodeSingleAxisHingeJoint,
    TNodeSliderJoint,
    TNodeUniversalJoint
  ]);
end;

{$endif read_implementation}
