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

    private FFdenabled: TSFBool;
    public property Fdenabled: TSFBool read FFdenabled;

    private FFdrotation: TSFRotation;
    public property Fdrotation: TSFRotation read FFdrotation;

    private FFdtranslation: TSFVec3f;
    public property Fdtranslation: TSFVec3f read FFdtranslation;

    private FFdbboxCenter: TSFVec3f;
    public property FdbboxCenter: TSFVec3f read FFdbboxCenter;

    private FFdbboxSize: TSFVec3f;
    public property FdbboxSize: TSFVec3f read FFdbboxSize;
  end;

  TNodeX3DNBodyCollisionSpaceNode = class(TNodeX3DNode, INodeX3DBoundedObject)
  public
    procedure CreateNode; override;

    private FFdenabled: TSFBool;
    public property Fdenabled: TSFBool read FFdenabled;

    private FFdbboxCenter: TSFVec3f;
    public property FdbboxCenter: TSFVec3f read FFdbboxCenter;

    private FFdbboxSize: TSFVec3f;
    public property FdbboxSize: TSFVec3f read FFdbboxSize;
  end;

  TNodeX3DRigidJointNode = class(TNodeX3DNode)
  public
    procedure CreateNode; override;

    private FFdbody1: TSFNode;
    public property Fdbody1: TSFNode read FFdbody1;

    private FFdbody2: TSFNode;
    public property Fdbody2: TSFNode read FFdbody2;

    private FFdforceOutput: TMFString;
    public property FdforceOutput: TMFString read FFdforceOutput;
  end;

  TNodeBallJoint = class(TNodeX3DRigidJointNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdanchorPoint: TSFVec3f;
    public property FdanchorPoint: TSFVec3f read FFdanchorPoint;

    { Event: SFVec3f, out } { }
    private FEventbody1AnchorPoint: TVRMLEvent;
    public property Eventbody1AnchorPoint: TVRMLEvent read FEventbody1AnchorPoint;

    { Event: SFVec3f, out } { }
    private FEventbody2AnchorPoint: TVRMLEvent;
    public property Eventbody2AnchorPoint: TVRMLEvent read FEventbody2AnchorPoint;
  end;

  TNodeCollidableOffset = class(TNodeX3DNBodyCollidableNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcollidable: TSFNode;
    public property Fdcollidable: TSFNode read FFdcollidable;
  end;

  TNodeCollidableShape = class(TNodeX3DNBodyCollidableNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdshape: TSFNode;
    public property Fdshape: TSFNode read FFdshape;
  end;

  TNodeCollisionCollection = class(TNodeX3DChildNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdappliedParameters: TMFString;
    public property FdappliedParameters: TMFString read FFdappliedParameters;

    private FFdbounce: TSFFloat;
    public property Fdbounce: TSFFloat read FFdbounce;

    private FFdcollidables: TMFNode;
    public property Fdcollidables: TMFNode read FFdcollidables;

    private FFdenabled: TSFBool;
    public property Fdenabled: TSFBool read FFdenabled;

    private FFdfrictionCoefficients: TSFVec2f;
    public property FdfrictionCoefficients: TSFVec2f read FFdfrictionCoefficients;

    private FFdminBounceSpeed: TSFFloat;
    public property FdminBounceSpeed: TSFFloat read FFdminBounceSpeed;

    private FFdslipFactors: TSFVec2f;
    public property FdslipFactors: TSFVec2f read FFdslipFactors;

    private FFdsoftnessConstantForceMix: TSFFloat;
    public property FdsoftnessConstantForceMix: TSFFloat read FFdsoftnessConstantForceMix;

    private FFdsoftnessErrorCorrection: TSFFloat;
    public property FdsoftnessErrorCorrection: TSFFloat read FFdsoftnessErrorCorrection;

    private FFdsurfaceSpeed: TSFVec2f;
    public property FdsurfaceSpeed: TSFVec2f read FFdsurfaceSpeed;
  end;

  TNodeCollisionSensor = class(TNodeX3DSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcollider: TSFNode;
    public property Fdcollider: TSFNode read FFdcollider;

    { Event: MFNode, out } { }
    private FEventintersections: TVRMLEvent;
    public property Eventintersections: TVRMLEvent read FEventintersections;

    { Event: MFNode, out } { }
    private FEventcontacts: TVRMLEvent;
    public property Eventcontacts: TVRMLEvent read FEventcontacts;
  end;

  TNodeCollisionSpace = class(TNodeX3DNBodyCollisionSpaceNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcollidables: TMFNode;
    public property Fdcollidables: TMFNode read FFdcollidables;

    private FFduseGeometry: TSFBool;
    public property FduseGeometry: TSFBool read FFduseGeometry;
  end;

  TNodeContact = class(TNodeX3DNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdappliedParameters: TMFString;
    public property FdappliedParameters: TMFString read FFdappliedParameters;

    private FFdbody1: TSFNode;
    public property Fdbody1: TSFNode read FFdbody1;

    private FFdbody2: TSFNode;
    public property Fdbody2: TSFNode read FFdbody2;

    private FFdbounce: TSFFloat;
    public property Fdbounce: TSFFloat read FFdbounce;

    private FFdcontactNormal: TSFVec3f;
    public property FdcontactNormal: TSFVec3f read FFdcontactNormal;

    private FFddepth: TSFFloat;
    public property Fddepth: TSFFloat read FFddepth;

    private FFdfrictionCoefficients: TSFVec2f;
    public property FdfrictionCoefficients: TSFVec2f read FFdfrictionCoefficients;

    private FFdfrictionDirection: TSFVec3f;
    public property FdfrictionDirection: TSFVec3f read FFdfrictionDirection;

    private FFdgeometry1: TSFNode;
    public property Fdgeometry1: TSFNode read FFdgeometry1;

    private FFdgeometry2: TSFNode;
    public property Fdgeometry2: TSFNode read FFdgeometry2;

    private FFdminbounceSpeed: TSFFloat;
    public property FdminbounceSpeed: TSFFloat read FFdminbounceSpeed;

    private FFdposition: TSFVec3f;
    public property Fdposition: TSFVec3f read FFdposition;

    private FFdslipCoefficients: TSFVec2f;
    public property FdslipCoefficients: TSFVec2f read FFdslipCoefficients;

    private FFdsoftnessConstantForceMix: TSFFloat;
    public property FdsoftnessConstantForceMix: TSFFloat read FFdsoftnessConstantForceMix;

    private FFdsoftnessErrorCorrection: TSFFloat;
    public property FdsoftnessErrorCorrection: TSFFloat read FFdsoftnessErrorCorrection;

    private FFdsurfaceSpeed: TSFVec2f;
    public property FdsurfaceSpeed: TSFVec2f read FFdsurfaceSpeed;
  end;

  TNodeDoubleAxisHingeJoint = class(TNodeX3DRigidJointNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdanchorPoint: TSFVec3f;
    public property FdanchorPoint: TSFVec3f read FFdanchorPoint;

    private FFdaxis1: TSFVec3f;
    public property Fdaxis1: TSFVec3f read FFdaxis1;

    private FFdaxis2: TSFVec3f;
    public property Fdaxis2: TSFVec3f read FFdaxis2;

    private FFddesiredAngularVelocity1: TSFFloat;
    public property FddesiredAngularVelocity1: TSFFloat read FFddesiredAngularVelocity1;

    private FFddesiredAngularVelocity2: TSFFloat;
    public property FddesiredAngularVelocity2: TSFFloat read FFddesiredAngularVelocity2;

    private FFdmaxAngle1: TSFFloat;
    public property FdmaxAngle1: TSFFloat read FFdmaxAngle1;

    private FFdmaxTorque1: TSFFloat;
    public property FdmaxTorque1: TSFFloat read FFdmaxTorque1;

    private FFdmaxTorque2: TSFFloat;
    public property FdmaxTorque2: TSFFloat read FFdmaxTorque2;

    private FFdminAngle1: TSFFloat;
    public property FdminAngle1: TSFFloat read FFdminAngle1;

    private FFdstopBounce1: TSFFloat;
    public property FdstopBounce1: TSFFloat read FFdstopBounce1;

    private FFdstopConstantForceMix1: TSFFloat;
    public property FdstopConstantForceMix1: TSFFloat read FFdstopConstantForceMix1;

    private FFdstopErrorCorrection1: TSFFloat;
    public property FdstopErrorCorrection1: TSFFloat read FFdstopErrorCorrection1;

    private FFdsuspensionErrorCorrection: TSFFloat;
    public property FdsuspensionErrorCorrection: TSFFloat read FFdsuspensionErrorCorrection;

    private FFdsuspensionForce: TSFFloat;
    public property FdsuspensionForce: TSFFloat read FFdsuspensionForce;

    { Event: SFVec3f, out } { }
    private FEventbody1AnchorPoint: TVRMLEvent;
    public property Eventbody1AnchorPoint: TVRMLEvent read FEventbody1AnchorPoint;

    { Event: SFVec3f, out } { }
    private FEventbody1Axis: TVRMLEvent;
    public property Eventbody1Axis: TVRMLEvent read FEventbody1Axis;

    { Event: SFVec3f, out } { }
    private FEventbody2AnchorPoint: TVRMLEvent;
    public property Eventbody2AnchorPoint: TVRMLEvent read FEventbody2AnchorPoint;

    { Event: SFVec3f, out } { }
    private FEventbody2Axis: TVRMLEvent;
    public property Eventbody2Axis: TVRMLEvent read FEventbody2Axis;

    { Event: SFFloat, out } { }
    private FEventhinge1Angle: TVRMLEvent;
    public property Eventhinge1Angle: TVRMLEvent read FEventhinge1Angle;

    { Event: SFFloat, out } { }
    private FEventhinge1AngleRate: TVRMLEvent;
    public property Eventhinge1AngleRate: TVRMLEvent read FEventhinge1AngleRate;

    { Event: SFFloat, out } { }
    private FEventhinge2Angle: TVRMLEvent;
    public property Eventhinge2Angle: TVRMLEvent read FEventhinge2Angle;

    { Event: SFFloat, out } { }
    private FEventhinge2AngleRate: TVRMLEvent;
    public property Eventhinge2AngleRate: TVRMLEvent read FEventhinge2AngleRate;
  end;

  TNodeMotorJoint = class(TNodeX3DRigidJointNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdaxis1Angle: TSFFloat;
    public property Fdaxis1Angle: TSFFloat read FFdaxis1Angle;

    private FFdaxis1Torque: TSFFloat;
    public property Fdaxis1Torque: TSFFloat read FFdaxis1Torque;

    private FFdaxis2Angle: TSFFloat;
    public property Fdaxis2Angle: TSFFloat read FFdaxis2Angle;

    private FFdaxis2Torque: TSFFloat;
    public property Fdaxis2Torque: TSFFloat read FFdaxis2Torque;

    private FFdaxis3Angle: TSFFloat;
    public property Fdaxis3Angle: TSFFloat read FFdaxis3Angle;

    private FFdaxis3Torque: TSFFloat;
    public property Fdaxis3Torque: TSFFloat read FFdaxis3Torque;

    private FFdenabledAxes: TSFInt32;
    public property FdenabledAxes: TSFInt32 read FFdenabledAxes;

    private FFdmotor1Axis: TSFVec3f;
    public property Fdmotor1Axis: TSFVec3f read FFdmotor1Axis;

    private FFdmotor2Axis: TSFVec3f;
    public property Fdmotor2Axis: TSFVec3f read FFdmotor2Axis;

    private FFdmotor3Axis: TSFVec3f;
    public property Fdmotor3Axis: TSFVec3f read FFdmotor3Axis;

    private FFdstop1Bounce: TSFFloat;
    public property Fdstop1Bounce: TSFFloat read FFdstop1Bounce;

    private FFdstop1ErrorCorrection: TSFFloat;
    public property Fdstop1ErrorCorrection: TSFFloat read FFdstop1ErrorCorrection;

    private FFdstop2Bounce: TSFFloat;
    public property Fdstop2Bounce: TSFFloat read FFdstop2Bounce;

    private FFdstop2ErrorCorrection: TSFFloat;
    public property Fdstop2ErrorCorrection: TSFFloat read FFdstop2ErrorCorrection;

    private FFdstop3Bounce: TSFFloat;
    public property Fdstop3Bounce: TSFFloat read FFdstop3Bounce;

    private FFdstop3ErrorCorrection: TSFFloat;
    public property Fdstop3ErrorCorrection: TSFFloat read FFdstop3ErrorCorrection;

    { Event: SFFloat, out } { }
    private FEventmotor1Angle: TVRMLEvent;
    public property Eventmotor1Angle: TVRMLEvent read FEventmotor1Angle;

    { Event: SFFloat, out } { }
    private FEventmotor1AngleRate: TVRMLEvent;
    public property Eventmotor1AngleRate: TVRMLEvent read FEventmotor1AngleRate;

    { Event: SFFloat, out } { }
    private FEventmotor2Angle: TVRMLEvent;
    public property Eventmotor2Angle: TVRMLEvent read FEventmotor2Angle;

    { Event: SFFloat, out } { }
    private FEventmotor2AngleRate: TVRMLEvent;
    public property Eventmotor2AngleRate: TVRMLEvent read FEventmotor2AngleRate;

    { Event: SFFloat, out } { }
    private FEventmotor3Angle: TVRMLEvent;
    public property Eventmotor3Angle: TVRMLEvent read FEventmotor3Angle;

    { Event: SFFloat, out } { }
    private FEventmotor3AngleRate: TVRMLEvent;
    public property Eventmotor3AngleRate: TVRMLEvent read FEventmotor3AngleRate;

    private FFdautoCalc: TSFBool;
    public property FdautoCalc: TSFBool read FFdautoCalc;
  end;

  TNodeRigidBody = class(TNodeX3DNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdangularDampingFactor: TSFFloat;
    public property FdangularDampingFactor: TSFFloat read FFdangularDampingFactor;

    private FFdangularVelocity: TSFVec3f;
    public property FdangularVelocity: TSFVec3f read FFdangularVelocity;

    private FFdautoDamp: TSFBool;
    public property FdautoDamp: TSFBool read FFdautoDamp;

    private FFdautoDisable: TSFBool;
    public property FdautoDisable: TSFBool read FFdautoDisable;

    private FFdcenterOfMass: TSFVec3f;
    public property FdcenterOfMass: TSFVec3f read FFdcenterOfMass;

    private FFddisableAngularSpeed: TSFFloat;
    public property FddisableAngularSpeed: TSFFloat read FFddisableAngularSpeed;

    private FFddisableLinearSpeed: TSFFloat;
    public property FddisableLinearSpeed: TSFFloat read FFddisableLinearSpeed;

    private FFddisableTime: TSFFloat;
    public property FddisableTime: TSFFloat read FFddisableTime;

    private FFdenabled: TSFBool;
    public property Fdenabled: TSFBool read FFdenabled;

    private FFdfiniteRotationAxis: TSFVec3f;
    public property FdfiniteRotationAxis: TSFVec3f read FFdfiniteRotationAxis;

    private FFdfixed: TSFBool;
    public property Fdfixed: TSFBool read FFdfixed;

    private FFdforces: TMFVec3f;
    public property Fdforces: TMFVec3f read FFdforces;

    private FFdgeometry: TMFNode;
    public property Fdgeometry: TMFNode read FFdgeometry;

    private FFdinertia: TSFMatrix3f;
    public property Fdinertia: TSFMatrix3f read FFdinertia;

    private FFdlinearDampingFactor: TSFFloat;
    public property FdlinearDampingFactor: TSFFloat read FFdlinearDampingFactor;

    private FFdlinearVelocity: TSFVec3f;
    public property FdlinearVelocity: TSFVec3f read FFdlinearVelocity;

    private FFdmass: TSFFloat;
    public property Fdmass: TSFFloat read FFdmass;

    private FFdmassDensityModel: TSFNode;
    public property FdmassDensityModel: TSFNode read FFdmassDensityModel;

    private FFdorientation: TSFRotation;
    public property Fdorientation: TSFRotation read FFdorientation;

    private FFdposition: TSFVec3f;
    public property Fdposition: TSFVec3f read FFdposition;

    private FFdtorques: TMFVec3f;
    public property Fdtorques: TMFVec3f read FFdtorques;

    private FFduseFiniteRotation: TSFBool;
    public property FduseFiniteRotation: TSFBool read FFduseFiniteRotation;

    private FFduseGlobalGravity: TSFBool;
    public property FduseGlobalGravity: TSFBool read FFduseGlobalGravity;
  end;

  TNodeRigidBodyCollection = class(TNodeX3DChildNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFNode, in } { }
    private FEventset_contacts: TVRMLEvent;
    public property Eventset_contacts: TVRMLEvent read FEventset_contacts;

    private FFdautoDisable: TSFBool;
    public property FdautoDisable: TSFBool read FFdautoDisable;

    private FFdbodies: TMFNode;
    public property Fdbodies: TMFNode read FFdbodies;

    private FFdconstantForceMix: TSFFloat;
    public property FdconstantForceMix: TSFFloat read FFdconstantForceMix;

    private FFdcontactSurfaceThickness: TSFFloat;
    public property FdcontactSurfaceThickness: TSFFloat read FFdcontactSurfaceThickness;

    private FFddisableAngularSpeed: TSFFloat;
    public property FddisableAngularSpeed: TSFFloat read FFddisableAngularSpeed;

    private FFddisableLinearSpeed: TSFFloat;
    public property FddisableLinearSpeed: TSFFloat read FFddisableLinearSpeed;

    private FFddisableTime: TSFFloat;
    public property FddisableTime: TSFFloat read FFddisableTime;

    private FFdenabled: TSFBool;
    public property Fdenabled: TSFBool read FFdenabled;

    private FFderrorCorrection: TSFFloat;
    public property FderrorCorrection: TSFFloat read FFderrorCorrection;

    private FFdgravity: TSFVec3f;
    public property Fdgravity: TSFVec3f read FFdgravity;

    private FFditerations: TSFInt32;
    public property Fditerations: TSFInt32 read FFditerations;

    private FFdjoints: TMFNode;
    public property Fdjoints: TMFNode read FFdjoints;

    private FFdmaxCorrectionSpeed: TSFFloat;
    public property FdmaxCorrectionSpeed: TSFFloat read FFdmaxCorrectionSpeed;

    private FFdpreferAccuracy: TSFBool;
    public property FdpreferAccuracy: TSFBool read FFdpreferAccuracy;

    private FFdcollider: TSFNode;
    public property Fdcollider: TSFNode read FFdcollider;
  end;

  TNodeSingleAxisHingeJoint = class(TNodeX3DRigidJointNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdanchorPoint: TSFVec3f;
    public property FdanchorPoint: TSFVec3f read FFdanchorPoint;

    private FFdaxis: TSFVec3f;
    public property Fdaxis: TSFVec3f read FFdaxis;

    private FFdmaxAngle: TSFFloat;
    public property FdmaxAngle: TSFFloat read FFdmaxAngle;

    private FFdminAngle: TSFFloat;
    public property FdminAngle: TSFFloat read FFdminAngle;

    private FFdstopBounce: TSFFloat;
    public property FdstopBounce: TSFFloat read FFdstopBounce;

    private FFdstopErrorCorrection: TSFFloat;
    public property FdstopErrorCorrection: TSFFloat read FFdstopErrorCorrection;

    { Event: SFFloat, out } { }
    private FEventangle: TVRMLEvent;
    public property Eventangle: TVRMLEvent read FEventangle;

    { Event: SFFloat, out } { }
    private FEventangleRate: TVRMLEvent;
    public property EventangleRate: TVRMLEvent read FEventangleRate;

    { Event: SFVec3f, out } { }
    private FEventbody1AnchorPoint: TVRMLEvent;
    public property Eventbody1AnchorPoint: TVRMLEvent read FEventbody1AnchorPoint;

    { Event: SFVec3f, out } { }
    private FEventbody2AnchorPoint: TVRMLEvent;
    public property Eventbody2AnchorPoint: TVRMLEvent read FEventbody2AnchorPoint;
  end;

  TNodeSliderJoint = class(TNodeX3DRigidJointNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdaxis: TSFVec3f;
    public property Fdaxis: TSFVec3f read FFdaxis;

    private FFdmaxSeparation: TSFFloat;
    public property FdmaxSeparation: TSFFloat read FFdmaxSeparation;

    private FFdminSeparation: TSFFloat;
    public property FdminSeparation: TSFFloat read FFdminSeparation;

    private FFdstopBounce: TSFFloat;
    public property FdstopBounce: TSFFloat read FFdstopBounce;

    private FFdstopErrorCorrection: TSFFloat;
    public property FdstopErrorCorrection: TSFFloat read FFdstopErrorCorrection;

    { Event: SFFloat, out } { }
    private FEventseparation: TVRMLEvent;
    public property Eventseparation: TVRMLEvent read FEventseparation;

    { Event: SFFloat, out } { }
    private FEventseparationRate: TVRMLEvent;
    public property EventseparationRate: TVRMLEvent read FEventseparationRate;
  end;

  TNodeUniversalJoint = class(TNodeX3DRigidJointNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdanchorPoint: TSFVec3f;
    public property FdanchorPoint: TSFVec3f read FFdanchorPoint;

    private FFdaxis1: TSFVec3f;
    public property Fdaxis1: TSFVec3f read FFdaxis1;

    private FFdaxis2: TSFVec3f;
    public property Fdaxis2: TSFVec3f read FFdaxis2;

    private FFdstopBounce1: TSFFloat;
    public property FdstopBounce1: TSFFloat read FFdstopBounce1;

    private FFdstop1ErrorCorrection: TSFFloat;
    public property Fdstop1ErrorCorrection: TSFFloat read FFdstop1ErrorCorrection;

    private FFdstop2Bounce: TSFFloat;
    public property Fdstop2Bounce: TSFFloat read FFdstop2Bounce;

    private FFdstop2ErrorCorrection: TSFFloat;
    public property Fdstop2ErrorCorrection: TSFFloat read FFdstop2ErrorCorrection;

    { Event: SFVec3f, out } { }
    private FEventbody1AnchorPoint: TVRMLEvent;
    public property Eventbody1AnchorPoint: TVRMLEvent read FEventbody1AnchorPoint;

    { Event: SFVec3f, out } { }
    private FEventbody1Axis: TVRMLEvent;
    public property Eventbody1Axis: TVRMLEvent read FEventbody1Axis;

    { Event: SFVec3f, out } { }
    private FEventbody2AnchorPoint: TVRMLEvent;
    public property Eventbody2AnchorPoint: TVRMLEvent read FEventbody2AnchorPoint;

    { Event: SFVec3f, out } { }
    private FEventbody2Axis: TVRMLEvent;
    public property Eventbody2Axis: TVRMLEvent read FEventbody2Axis;
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

function TNodeBallJoint.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeCollidableOffset.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeCollidableShape.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeCollisionCollection.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeCollisionSensor.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeCollisionSpace.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeContact.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeDoubleAxisHingeJoint.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeMotorJoint.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

  FFdmassDensityModel := TSFNode.Create(Self, 'massDensityModel', [TNodeSphere_2, TNodeBox, TNodeCone_2]);
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

function TNodeRigidBody.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeRigidBodyCollection.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeSingleAxisHingeJoint.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeSliderJoint.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeUniversalJoint.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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
