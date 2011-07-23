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
  TNodeDISEntityManager = class(TNodeX3DChildNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAddress: TSFString;
    public property FdAddress: TSFString read FFdAddress;

    private FFdApplicationID: TSFInt32;
    public property FdApplicationID: TSFInt32 read FFdApplicationID;

    private FFdMapping: TMFNode;
    public property FdMapping: TMFNode read FFdMapping;

    private FFdPort: TSFInt32;
    public property FdPort: TSFInt32 read FFdPort;

    private FFdSiteID: TSFInt32;
    public property FdSiteID: TSFInt32 read FFdSiteID;

    { Event: MFNode, out } { }
    private FEventAddedEntities: TVRMLEvent;
    public property EventAddedEntities: TVRMLEvent read FEventAddedEntities;

    { Event: MFNode, out } { }
    private FEventRemovedEntities: TVRMLEvent;
    public property EventRemovedEntities: TVRMLEvent read FEventRemovedEntities;
  end;

  TNodeDISEntityTypeMapping = class(TNodeX3DInfoNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdUrl: TMFString;
    public property FdUrl: TMFString read FFdUrl;

    private FFdCategory: TSFInt32;
    public property FdCategory: TSFInt32 read FFdCategory;

    private FFdCountry: TSFInt32;
    public property FdCountry: TSFInt32 read FFdCountry;

    private FFdDomain: TSFInt32;
    public property FdDomain: TSFInt32 read FFdDomain;

    private FFdExtra: TSFInt32;
    public property FdExtra: TSFInt32 read FFdExtra;

    private FFdKind: TSFInt32;
    public property FdKind: TSFInt32 read FFdKind;

    private FFdSpecific: TSFInt32;
    public property FdSpecific: TSFInt32 read FFdSpecific;

    private FFdSubcategory: TSFInt32;
    public property FdSubcategory: TSFInt32 read FFdSubcategory;
  end;

  TNodeEspduTransform = class(TNodeX3DGroupingNode, INodeX3DSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFFloat, in } { }
    private FEventSet_articulationParameterValue0: TVRMLEvent;
    public property EventSet_articulationParameterValue0: TVRMLEvent read FEventSet_articulationParameterValue0;

    { Event: SFFloat, in } { }
    private FEventSet_articulationParameterValue1: TVRMLEvent;
    public property EventSet_articulationParameterValue1: TVRMLEvent read FEventSet_articulationParameterValue1;

    { Event: SFFloat, in } { }
    private FEventSet_articulationParameterValue2: TVRMLEvent;
    public property EventSet_articulationParameterValue2: TVRMLEvent read FEventSet_articulationParameterValue2;

    { Event: SFFloat, in } { }
    private FEventSet_articulationParameterValue3: TVRMLEvent;
    public property EventSet_articulationParameterValue3: TVRMLEvent read FEventSet_articulationParameterValue3;

    { Event: SFFloat, in } { }
    private FEventSet_articulationParameterValue4: TVRMLEvent;
    public property EventSet_articulationParameterValue4: TVRMLEvent read FEventSet_articulationParameterValue4;

    { Event: SFFloat, in } { }
    private FEventSet_articulationParameterValue5: TVRMLEvent;
    public property EventSet_articulationParameterValue5: TVRMLEvent read FEventSet_articulationParameterValue5;

    { Event: SFFloat, in } { }
    private FEventSet_articulationParameterValue6: TVRMLEvent;
    public property EventSet_articulationParameterValue6: TVRMLEvent read FEventSet_articulationParameterValue6;

    { Event: SFFloat, in } { }
    private FEventSet_articulationParameterValue7: TVRMLEvent;
    public property EventSet_articulationParameterValue7: TVRMLEvent read FEventSet_articulationParameterValue7;

    private FFdAddress: TSFString;
    public property FdAddress: TSFString read FFdAddress;

    private FFdApplicationID: TSFInt32;
    public property FdApplicationID: TSFInt32 read FFdApplicationID;

    private FFdArticulationParameterCount: TSFInt32;
    public property FdArticulationParameterCount: TSFInt32 read FFdArticulationParameterCount;

    private FFdArticulationParameterDesignatorArray: TMFInt32;
    public property FdArticulationParameterDesignatorArray: TMFInt32 read FFdArticulationParameterDesignatorArray;

    private FFdArticulationParameterChangeIndicatorArray: TMFInt32;
    public property FdArticulationParameterChangeIndicatorArray: TMFInt32 read FFdArticulationParameterChangeIndicatorArray;

    private FFdArticulationParameterIdPartAttachedToArray: TMFInt32;
    public property FdArticulationParameterIdPartAttachedToArray: TMFInt32 read FFdArticulationParameterIdPartAttachedToArray;

    private FFdArticulationParameterTypeArray: TMFInt32;
    public property FdArticulationParameterTypeArray: TMFInt32 read FFdArticulationParameterTypeArray;

    private FFdArticulationParameterArray: TMFFloat;
    public property FdArticulationParameterArray: TMFFloat read FFdArticulationParameterArray;

    private FFdCenter: TSFVec3f;
    public property FdCenter: TSFVec3f read FFdCenter;

    private FFdCollisionType: TSFInt32;
    public property FdCollisionType: TSFInt32 read FFdCollisionType;

    private FFdDeadReckoning: TSFInt32;
    public property FdDeadReckoning: TSFInt32 read FFdDeadReckoning;

    private FFdDetonationLocation: TSFVec3f;
    public property FdDetonationLocation: TSFVec3f read FFdDetonationLocation;

    private FFdDetonationRelativeLocation: TSFVec3f;
    public property FdDetonationRelativeLocation: TSFVec3f read FFdDetonationRelativeLocation;

    private FFdDetonationResult: TSFInt32;
    public property FdDetonationResult: TSFInt32 read FFdDetonationResult;

    private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    private FFdEntityCategory: TSFInt32;
    public property FdEntityCategory: TSFInt32 read FFdEntityCategory;

    private FFdEntityCountry: TSFInt32;
    public property FdEntityCountry: TSFInt32 read FFdEntityCountry;

    private FFdEntityDomain: TSFInt32;
    public property FdEntityDomain: TSFInt32 read FFdEntityDomain;

    private FFdEntityExtra: TSFInt32;
    public property FdEntityExtra: TSFInt32 read FFdEntityExtra;

    private FFdEntityID: TSFInt32;
    public property FdEntityID: TSFInt32 read FFdEntityID;

    private FFdEntityKind: TSFInt32;
    public property FdEntityKind: TSFInt32 read FFdEntityKind;

    private FFdEntitySpecific: TSFInt32;
    public property FdEntitySpecific: TSFInt32 read FFdEntitySpecific;

    private FFdEntitySubCategory: TSFInt32;
    public property FdEntitySubCategory: TSFInt32 read FFdEntitySubCategory;

    private FFdEventApplicationID: TSFInt32;
    public property FdEventApplicationID: TSFInt32 read FFdEventApplicationID;

    private FFdEventEntityID: TSFInt32;
    public property FdEventEntityID: TSFInt32 read FFdEventEntityID;

    private FFdEventNumber: TSFInt32;
    public property FdEventNumber: TSFInt32 read FFdEventNumber;

    private FFdEventSiteID: TSFInt32;
    public property FdEventSiteID: TSFInt32 read FFdEventSiteID;

    private FFdFired1: TSFBool;
    public property FdFired1: TSFBool read FFdFired1;

    private FFdFired2: TSFBool;
    public property FdFired2: TSFBool read FFdFired2;

    private FFdFireMissionIndex: TSFInt32;
    public property FdFireMissionIndex: TSFInt32 read FFdFireMissionIndex;

    private FFdFiringRange: TSFFloat;
    public property FdFiringRange: TSFFloat read FFdFiringRange;

    private FFdFiringRate: TSFInt32;
    public property FdFiringRate: TSFInt32 read FFdFiringRate;

    private FFdForceID: TSFInt32;
    public property FdForceID: TSFInt32 read FFdForceID;

    private FFdFuse: TSFInt32;
    public property FdFuse: TSFInt32 read FFdFuse;

    private FFdLinearVelocity: TSFVec3f;
    public property FdLinearVelocity: TSFVec3f read FFdLinearVelocity;

    private FFdLinearAcceleration: TSFVec3f;
    public property FdLinearAcceleration: TSFVec3f read FFdLinearAcceleration;

    private FFdMarking: TSFString;
    public property FdMarking: TSFString read FFdMarking;

    private FFdMulticastRelayHost: TSFString;
    public property FdMulticastRelayHost: TSFString read FFdMulticastRelayHost;

    private FFdMulticastRelayPort: TSFInt32;
    public property FdMulticastRelayPort: TSFInt32 read FFdMulticastRelayPort;

    private FFdMunitionApplicationID: TSFInt32;
    public property FdMunitionApplicationID: TSFInt32 read FFdMunitionApplicationID;

    private FFdMunitionEndPoint: TSFVec3f;
    public property FdMunitionEndPoint: TSFVec3f read FFdMunitionEndPoint;

    private FFdMunitionEntityID: TSFInt32;
    public property FdMunitionEntityID: TSFInt32 read FFdMunitionEntityID;

    private FFdMunitionQuantity: TSFInt32;
    public property FdMunitionQuantity: TSFInt32 read FFdMunitionQuantity;

    private FFdMunitionSiteID: TSFInt32;
    public property FdMunitionSiteID: TSFInt32 read FFdMunitionSiteID;

    private FFdMunitionStartPoint: TSFVec3f;
    public property FdMunitionStartPoint: TSFVec3f read FFdMunitionStartPoint;

    private FFdNetworkMode: TSFString;
    public property FdNetworkMode: TSFString read FFdNetworkMode;

    private FFdPort: TSFInt32;
    public property FdPort: TSFInt32 read FFdPort;

    private FFdReadInterval: TSFTime;
    public property FdReadInterval: TSFTime read FFdReadInterval;

    private FFdRotation: TSFRotation;
    public property FdRotation: TSFRotation read FFdRotation;

    private FFdScale: TSFVec3f;
    public property FdScale: TSFVec3f read FFdScale;

    private FFdScaleOrientation: TSFRotation;
    public property FdScaleOrientation: TSFRotation read FFdScaleOrientation;

    private FFdSiteID: TSFInt32;
    public property FdSiteID: TSFInt32 read FFdSiteID;

    private FFdTranslation: TSFVec3f;
    public property FdTranslation: TSFVec3f read FFdTranslation;

    private FFdWarhead: TSFInt32;
    public property FdWarhead: TSFInt32 read FFdWarhead;

    private FFdWriteInterval: TSFTime;
    public property FdWriteInterval: TSFTime read FFdWriteInterval;

    { Event: SFFloat, out } { }
    private FEventArticulationParameterValue0_changed: TVRMLEvent;
    public property EventArticulationParameterValue0_changed: TVRMLEvent read FEventArticulationParameterValue0_changed;

    { Event: SFFloat, out } { }
    private FEventArticulationParameterValue1_changed: TVRMLEvent;
    public property EventArticulationParameterValue1_changed: TVRMLEvent read FEventArticulationParameterValue1_changed;

    { Event: SFFloat, out } { }
    private FEventArticulationParameterValue2_changed: TVRMLEvent;
    public property EventArticulationParameterValue2_changed: TVRMLEvent read FEventArticulationParameterValue2_changed;

    { Event: SFFloat, out } { }
    private FEventArticulationParameterValue3_changed: TVRMLEvent;
    public property EventArticulationParameterValue3_changed: TVRMLEvent read FEventArticulationParameterValue3_changed;

    { Event: SFFloat, out } { }
    private FEventArticulationParameterValue4_changed: TVRMLEvent;
    public property EventArticulationParameterValue4_changed: TVRMLEvent read FEventArticulationParameterValue4_changed;

    { Event: SFFloat, out } { }
    private FEventArticulationParameterValue5_changed: TVRMLEvent;
    public property EventArticulationParameterValue5_changed: TVRMLEvent read FEventArticulationParameterValue5_changed;

    { Event: SFFloat, out } { }
    private FEventArticulationParameterValue6_changed: TVRMLEvent;
    public property EventArticulationParameterValue6_changed: TVRMLEvent read FEventArticulationParameterValue6_changed;

    { Event: SFFloat, out } { }
    private FEventArticulationParameterValue7_changed: TVRMLEvent;
    public property EventArticulationParameterValue7_changed: TVRMLEvent read FEventArticulationParameterValue7_changed;

    { Event: SFTime, out } { }
    private FEventCollideTime: TVRMLEvent;
    public property EventCollideTime: TVRMLEvent read FEventCollideTime;

    { Event: SFTime, out } { }
    private FEventDetonateTime: TVRMLEvent;
    public property EventDetonateTime: TVRMLEvent read FEventDetonateTime;

    { Event: SFTime, out } { }
    private FEventFiredTime: TVRMLEvent;
    public property EventFiredTime: TVRMLEvent read FEventFiredTime;

    { Event: SFBool, out } { }
    private FEventIsActive: TVRMLEvent;
    public property EventIsActive: TVRMLEvent read FEventIsActive;

    { Event: SFBool, out } { }
    private FEventIsCollided: TVRMLEvent;
    public property EventIsCollided: TVRMLEvent read FEventIsCollided;

    { Event: SFBool, out } { }
    private FEventIsDetonated: TVRMLEvent;
    public property EventIsDetonated: TVRMLEvent read FEventIsDetonated;

    { Event: SFBool, out } { }
    private FEventIsNetworkReader: TVRMLEvent;
    public property EventIsNetworkReader: TVRMLEvent read FEventIsNetworkReader;

    { Event: SFBool, out } { }
    private FEventIsNetworkWriter: TVRMLEvent;
    public property EventIsNetworkWriter: TVRMLEvent read FEventIsNetworkWriter;

    { Event: SFBool, out } { }
    private FEventIsRtpHeaderHeard: TVRMLEvent;
    public property EventIsRtpHeaderHeard: TVRMLEvent read FEventIsRtpHeaderHeard;

    { Event: SFBool, out } { }
    private FEventIsStandAlone: TVRMLEvent;
    public property EventIsStandAlone: TVRMLEvent read FEventIsStandAlone;

    { Event: SFTime, out } { }
    private FEventTimestamp: TVRMLEvent;
    public property EventTimestamp: TVRMLEvent read FEventTimestamp;

    private FFdRtpHeaderExpected: TSFBool;
    public property FdRtpHeaderExpected: TSFBool read FFdRtpHeaderExpected;
  end;

  TNodeReceiverPdu = class(TNodeX3DSensorNode, INodeX3DBoundedObject)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAddress: TSFString;
    public property FdAddress: TSFString read FFdAddress;

    private FFdApplicationID: TSFInt32;
    public property FdApplicationID: TSFInt32 read FFdApplicationID;

    private FFdEntityID: TSFInt32;
    public property FdEntityID: TSFInt32 read FFdEntityID;

    private FFdMulticastRelayHost: TSFString;
    public property FdMulticastRelayHost: TSFString read FFdMulticastRelayHost;

    private FFdMulticastRelayPort: TSFInt32;
    public property FdMulticastRelayPort: TSFInt32 read FFdMulticastRelayPort;

    private FFdNetworkMode: TSFString;
    public property FdNetworkMode: TSFString read FFdNetworkMode;

    private FFdPort: TSFInt32;
    public property FdPort: TSFInt32 read FFdPort;

    private FFdRadioID: TSFInt32;
    public property FdRadioID: TSFInt32 read FFdRadioID;

    private FFdReadInterval: TSFFloat;
    public property FdReadInterval: TSFFloat read FFdReadInterval;

    private FFdReceivedPower: TSFFloat;
    public property FdReceivedPower: TSFFloat read FFdReceivedPower;

    private FFdReceiverState: TSFInt32;
    public property FdReceiverState: TSFInt32 read FFdReceiverState;

    private FFdRtpHeaderExpected: TSFBool;
    public property FdRtpHeaderExpected: TSFBool read FFdRtpHeaderExpected;

    private FFdSiteID: TSFInt32;
    public property FdSiteID: TSFInt32 read FFdSiteID;

    private FFdTransmitterApplicationID: TSFInt32;
    public property FdTransmitterApplicationID: TSFInt32 read FFdTransmitterApplicationID;

    private FFdTransmitterEntityID: TSFInt32;
    public property FdTransmitterEntityID: TSFInt32 read FFdTransmitterEntityID;

    private FFdTransmitterRadioID: TSFInt32;
    public property FdTransmitterRadioID: TSFInt32 read FFdTransmitterRadioID;

    private FFdTransmitterSiteID: TSFInt32;
    public property FdTransmitterSiteID: TSFInt32 read FFdTransmitterSiteID;

    private FFdWhichGeometry: TSFInt32;
    public property FdWhichGeometry: TSFInt32 read FFdWhichGeometry;

    private FFdWriteInterval: TSFFloat;
    public property FdWriteInterval: TSFFloat read FFdWriteInterval;

    { Event: SFBool, out } { }
    private FEventIsNetworkReader: TVRMLEvent;
    public property EventIsNetworkReader: TVRMLEvent read FEventIsNetworkReader;

    { Event: SFBool, out } { }
    private FEventIsNetworkWriter: TVRMLEvent;
    public property EventIsNetworkWriter: TVRMLEvent read FEventIsNetworkWriter;

    { Event: SFBool, out } { }
    private FEventIsRtpHeaderHeard: TVRMLEvent;
    public property EventIsRtpHeaderHeard: TVRMLEvent read FEventIsRtpHeaderHeard;

    { Event: SFBool, out } { }
    private FEventIsStandAlone: TVRMLEvent;
    public property EventIsStandAlone: TVRMLEvent read FEventIsStandAlone;

    { Event: SFTime, out } { }
    private FEventTimestamp: TVRMLEvent;
    public property EventTimestamp: TVRMLEvent read FEventTimestamp;

    private FFdBboxCenter: TSFVec3f;
    public property FdBboxCenter: TSFVec3f read FFdBboxCenter;

    private FFdBboxSize: TSFVec3f;
    public property FdBboxSize: TSFVec3f read FFdBboxSize;
  end;

  TNodeSignalPdu = class(TNodeX3DSensorNode, INodeX3DBoundedObject)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAddress: TSFString;
    public property FdAddress: TSFString read FFdAddress;

    private FFdApplicationID: TSFInt32;
    public property FdApplicationID: TSFInt32 read FFdApplicationID;

    private FFdData: TMFInt32;
    public property FdData: TMFInt32 read FFdData;

    private FFdDataLength: TSFInt32;
    public property FdDataLength: TSFInt32 read FFdDataLength;

    private FFdEncodingScheme: TSFInt32;
    public property FdEncodingScheme: TSFInt32 read FFdEncodingScheme;

    private FFdEntityID: TSFInt32;
    public property FdEntityID: TSFInt32 read FFdEntityID;

    private FFdMulticastRelayHost: TSFString;
    public property FdMulticastRelayHost: TSFString read FFdMulticastRelayHost;

    private FFdMulticastRelayPort: TSFInt32;
    public property FdMulticastRelayPort: TSFInt32 read FFdMulticastRelayPort;

    private FFdNetworkMode: TSFString;
    public property FdNetworkMode: TSFString read FFdNetworkMode;

    private FFdPort: TSFInt32;
    public property FdPort: TSFInt32 read FFdPort;

    private FFdRadioID: TSFInt32;
    public property FdRadioID: TSFInt32 read FFdRadioID;

    private FFdReadInterval: TSFFloat;
    public property FdReadInterval: TSFFloat read FFdReadInterval;

    private FFdRtpHeaderExpected: TSFBool;
    public property FdRtpHeaderExpected: TSFBool read FFdRtpHeaderExpected;

    private FFdSampleRate: TSFInt32;
    public property FdSampleRate: TSFInt32 read FFdSampleRate;

    private FFdSamples: TSFInt32;
    public property FdSamples: TSFInt32 read FFdSamples;

    private FFdSiteID: TSFInt32;
    public property FdSiteID: TSFInt32 read FFdSiteID;

    private FFdTdlType: TSFInt32;
    public property FdTdlType: TSFInt32 read FFdTdlType;

    private FFdWhichGeometry: TSFInt32;
    public property FdWhichGeometry: TSFInt32 read FFdWhichGeometry;

    private FFdWriteInterval: TSFFloat;
    public property FdWriteInterval: TSFFloat read FFdWriteInterval;

    { Event: SFBool, out } { }
    private FEventIsNetworkReader: TVRMLEvent;
    public property EventIsNetworkReader: TVRMLEvent read FEventIsNetworkReader;

    { Event: SFBool, out } { }
    private FEventIsNetworkWriter: TVRMLEvent;
    public property EventIsNetworkWriter: TVRMLEvent read FEventIsNetworkWriter;

    { Event: SFBool, out } { }
    private FEventIsRtpHeaderHeard: TVRMLEvent;
    public property EventIsRtpHeaderHeard: TVRMLEvent read FEventIsRtpHeaderHeard;

    { Event: SFBool, out } { }
    private FEventIsStandAlone: TVRMLEvent;
    public property EventIsStandAlone: TVRMLEvent read FEventIsStandAlone;

    { Event: SFTime, out } { }
    private FEventTimestamp: TVRMLEvent;
    public property EventTimestamp: TVRMLEvent read FEventTimestamp;

    private FFdBboxCenter: TSFVec3f;
    public property FdBboxCenter: TSFVec3f read FFdBboxCenter;

    private FFdBboxSize: TSFVec3f;
    public property FdBboxSize: TSFVec3f read FFdBboxSize;
  end;

  TNodeTransmitterPdu = class(TNodeX3DSensorNode, INodeX3DBoundedObject)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAddress: TSFString;
    public property FdAddress: TSFString read FFdAddress;

    private FFdAntennaLocation: TSFVec3f;
    public property FdAntennaLocation: TSFVec3f read FFdAntennaLocation;

    private FFdAntennaPatternLength: TSFInt32;
    public property FdAntennaPatternLength: TSFInt32 read FFdAntennaPatternLength;

    private FFdAntennaPatternType: TSFInt32;
    public property FdAntennaPatternType: TSFInt32 read FFdAntennaPatternType;

    private FFdApplicationID: TSFInt32;
    public property FdApplicationID: TSFInt32 read FFdApplicationID;

    private FFdCryptoKeyID: TSFInt32;
    public property FdCryptoKeyID: TSFInt32 read FFdCryptoKeyID;

    private FFdCryptoSystem: TSFInt32;
    public property FdCryptoSystem: TSFInt32 read FFdCryptoSystem;

    private FFdEntityID: TSFInt32;
    public property FdEntityID: TSFInt32 read FFdEntityID;

    private FFdFrequency: TSFInt32;
    public property FdFrequency: TSFInt32 read FFdFrequency;

    private FFdInputSource: TSFInt32;
    public property FdInputSource: TSFInt32 read FFdInputSource;

    private FFdLengthOfModulationParameters: TSFInt32;
    public property FdLengthOfModulationParameters: TSFInt32 read FFdLengthOfModulationParameters;

    private FFdModulationTypeDetail: TSFInt32;
    public property FdModulationTypeDetail: TSFInt32 read FFdModulationTypeDetail;

    private FFdModulationTypeMajor: TSFInt32;
    public property FdModulationTypeMajor: TSFInt32 read FFdModulationTypeMajor;

    private FFdModulationTypeSpreadSpectrum: TSFInt32;
    public property FdModulationTypeSpreadSpectrum: TSFInt32 read FFdModulationTypeSpreadSpectrum;

    private FFdModulationTypeSystem: TSFInt32;
    public property FdModulationTypeSystem: TSFInt32 read FFdModulationTypeSystem;

    private FFdMulticastRelayHost: TSFString;
    public property FdMulticastRelayHost: TSFString read FFdMulticastRelayHost;

    private FFdMulticastRelayPort: TSFInt32;
    public property FdMulticastRelayPort: TSFInt32 read FFdMulticastRelayPort;

    private FFdNetworkMode: TSFString;
    public property FdNetworkMode: TSFString read FFdNetworkMode;

    private FFdPort: TSFInt32;
    public property FdPort: TSFInt32 read FFdPort;

    private FFdPower: TSFFloat;
    public property FdPower: TSFFloat read FFdPower;

    private FFdRadioEntityTypeCategory: TSFInt32;
    public property FdRadioEntityTypeCategory: TSFInt32 read FFdRadioEntityTypeCategory;

    private FFdRadioEntityTypeCountry: TSFInt32;
    public property FdRadioEntityTypeCountry: TSFInt32 read FFdRadioEntityTypeCountry;

    private FFdRadioEntityTypeDomain: TSFInt32;
    public property FdRadioEntityTypeDomain: TSFInt32 read FFdRadioEntityTypeDomain;

    private FFdRadioEntityTypeKind: TSFInt32;
    public property FdRadioEntityTypeKind: TSFInt32 read FFdRadioEntityTypeKind;

    private FFdRadioEntityTypeNomenclature: TSFInt32;
    public property FdRadioEntityTypeNomenclature: TSFInt32 read FFdRadioEntityTypeNomenclature;

    private FFdRadioEntityTypeNomenclatureVersion: TSFInt32;
    public property FdRadioEntityTypeNomenclatureVersion: TSFInt32 read FFdRadioEntityTypeNomenclatureVersion;

    private FFdRadioID: TSFInt32;
    public property FdRadioID: TSFInt32 read FFdRadioID;

    private FFdReadInterval: TSFFloat;
    public property FdReadInterval: TSFFloat read FFdReadInterval;

    private FFdRelativeAntennaLocation: TSFVec3f;
    public property FdRelativeAntennaLocation: TSFVec3f read FFdRelativeAntennaLocation;

    private FFdRtpHeaderExpected: TSFBool;
    public property FdRtpHeaderExpected: TSFBool read FFdRtpHeaderExpected;

    private FFdSiteID: TSFInt32;
    public property FdSiteID: TSFInt32 read FFdSiteID;

    private FFdTransmitFrequencyBandwidth: TSFFloat;
    public property FdTransmitFrequencyBandwidth: TSFFloat read FFdTransmitFrequencyBandwidth;

    private FFdTransmitState: TSFInt32;
    public property FdTransmitState: TSFInt32 read FFdTransmitState;

    private FFdWhichGeometry: TSFInt32;
    public property FdWhichGeometry: TSFInt32 read FFdWhichGeometry;

    private FFdWriteInterval: TSFFloat;
    public property FdWriteInterval: TSFFloat read FFdWriteInterval;

    { Event: SFBool, out } { }
    private FEventIsNetworkReader: TVRMLEvent;
    public property EventIsNetworkReader: TVRMLEvent read FEventIsNetworkReader;

    { Event: SFBool, out } { }
    private FEventIsNetworkWriter: TVRMLEvent;
    public property EventIsNetworkWriter: TVRMLEvent read FEventIsNetworkWriter;

    { Event: SFBool, out } { }
    private FEventIsRtpHeaderHeard: TVRMLEvent;
    public property EventIsRtpHeaderHeard: TVRMLEvent read FEventIsRtpHeaderHeard;

    { Event: SFBool, out } { }
    private FEventIsStandAlone: TVRMLEvent;
    public property EventIsStandAlone: TVRMLEvent read FEventIsStandAlone;

    { Event: SFTime, out } { }
    private FEventTimestamp: TVRMLEvent;
    public property EventTimestamp: TVRMLEvent read FEventTimestamp;

    private FFdBboxCenter: TSFVec3f;
    public property FdBboxCenter: TSFVec3f read FFdBboxCenter;

    private FFdBboxSize: TSFVec3f;
    public property FdBboxSize: TSFVec3f read FFdBboxSize;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TNodeDISEntityManager.CreateNode;
begin
  inherited;

  FFdaddress := TSFString.Create(Self, 'address', 'localhost');
  Fields.Add(FFdaddress);

  FFdapplicationID := TSFInt32.Create(Self, 'applicationID', 1);
  Fields.Add(FFdapplicationID);
  { X3D specification comment: [0,65535] }

  FFdmapping := TMFNode.Create(Self, 'mapping', [TNodeDISEntityTypeMapping]);
  Fields.Add(FFdmapping);

  FFdport := TSFInt32.Create(Self, 'port', 0);
  Fields.Add(FFdport);
  { X3D specification comment: [0,65535] }

  FFdsiteID := TSFInt32.Create(Self, 'siteID', 0);
  Fields.Add(FFdsiteID);
  { X3D specification comment: [0,65535] }

  FEventaddedEntities := TVRMLEvent.Create(Self, 'addedEntities', TMFNode, false);
  Events.Add(FEventaddedEntities);

  FEventremovedEntities := TVRMLEvent.Create(Self, 'removedEntities', TMFNode, false);
  Events.Add(FEventremovedEntities);

  DefaultContainerField := 'children';
end;

class function TNodeDISEntityManager.ClassNodeTypeName: string;
begin
  Result := 'DISEntityManager';
end;

class function TNodeDISEntityManager.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeDISEntityTypeMapping.CreateNode;
begin
  inherited;

  FFdurl := TMFString.Create(Self, 'url', []);
  Fields.Add(FFdurl);
  { X3D specification comment: [URI] }

  FFdcategory := TSFInt32.Create(Self, 'category', 0);
  FFdcategory.Exposed := false;
  Fields.Add(FFdcategory);
  { X3D specification comment: [0,255] }

  FFdcountry := TSFInt32.Create(Self, 'country', 0);
  FFdcountry.Exposed := false;
  Fields.Add(FFdcountry);
  { X3D specification comment: [0,65535] }

  FFddomain := TSFInt32.Create(Self, 'domain', 0);
  FFddomain.Exposed := false;
  Fields.Add(FFddomain);
  { X3D specification comment: [0,255] }

  FFdextra := TSFInt32.Create(Self, 'extra', 0);
  FFdextra.Exposed := false;
  Fields.Add(FFdextra);
  { X3D specification comment: [0,255] }

  FFdkind := TSFInt32.Create(Self, 'kind', 0);
  FFdkind.Exposed := false;
  Fields.Add(FFdkind);
  { X3D specification comment: [0,255] }

  FFdspecific := TSFInt32.Create(Self, 'specific', 0);
  FFdspecific.Exposed := false;
  Fields.Add(FFdspecific);
  { X3D specification comment: [0,255] }

  FFdsubcategory := TSFInt32.Create(Self, 'subcategory', 0);
  FFdsubcategory.Exposed := false;
  Fields.Add(FFdsubcategory);
  { X3D specification comment: [0,255] }

  DefaultContainerField := 'children';
end;

class function TNodeDISEntityTypeMapping.ClassNodeTypeName: string;
begin
  Result := 'DISEntityTypeMapping';
end;

class function TNodeDISEntityTypeMapping.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeEspduTransform.CreateNode;
begin
  inherited;

  FEventset_articulationParameterValue0 := TVRMLEvent.Create(Self, 'set_articulationParameterValue0', TSFFloat, true);
  Events.Add(FEventset_articulationParameterValue0);

  FEventset_articulationParameterValue1 := TVRMLEvent.Create(Self, 'set_articulationParameterValue1', TSFFloat, true);
  Events.Add(FEventset_articulationParameterValue1);

  FEventset_articulationParameterValue2 := TVRMLEvent.Create(Self, 'set_articulationParameterValue2', TSFFloat, true);
  Events.Add(FEventset_articulationParameterValue2);

  FEventset_articulationParameterValue3 := TVRMLEvent.Create(Self, 'set_articulationParameterValue3', TSFFloat, true);
  Events.Add(FEventset_articulationParameterValue3);

  FEventset_articulationParameterValue4 := TVRMLEvent.Create(Self, 'set_articulationParameterValue4', TSFFloat, true);
  Events.Add(FEventset_articulationParameterValue4);

  FEventset_articulationParameterValue5 := TVRMLEvent.Create(Self, 'set_articulationParameterValue5', TSFFloat, true);
  Events.Add(FEventset_articulationParameterValue5);

  FEventset_articulationParameterValue6 := TVRMLEvent.Create(Self, 'set_articulationParameterValue6', TSFFloat, true);
  Events.Add(FEventset_articulationParameterValue6);

  FEventset_articulationParameterValue7 := TVRMLEvent.Create(Self, 'set_articulationParameterValue7', TSFFloat, true);
  Events.Add(FEventset_articulationParameterValue7);

  FFdaddress := TSFString.Create(Self, 'address', 'localhost');
  Fields.Add(FFdaddress);

  FFdapplicationID := TSFInt32.Create(Self, 'applicationID', 1);
  Fields.Add(FFdapplicationID);
  { X3D specification comment: [0,65535] }

  FFdarticulationParameterCount := TSFInt32.Create(Self, 'articulationParameterCount', 0);
  Fields.Add(FFdarticulationParameterCount);
  { X3D specification comment: [0,78] }

  FFdarticulationParameterDesignatorArray := TMFInt32.Create(Self, 'articulationParameterDesignatorArray', []);
  Fields.Add(FFdarticulationParameterDesignatorArray);
  { X3D specification comment: [0,255] }

  FFdarticulationParameterChangeIndicatorArray := TMFInt32.Create(Self, 'articulationParameterChangeIndicatorArray', []);
  Fields.Add(FFdarticulationParameterChangeIndicatorArray);
  { X3D specification comment: [0,255] }

  FFdarticulationParameterIdPartAttachedToArray := TMFInt32.Create(Self, 'articulationParameterIdPartAttachedToArray', []);
  Fields.Add(FFdarticulationParameterIdPartAttachedToArray);
  { X3D specification comment: [0,65535] }

  FFdarticulationParameterTypeArray := TMFInt32.Create(Self, 'articulationParameterTypeArray', []);
  Fields.Add(FFdarticulationParameterTypeArray);

  FFdarticulationParameterArray := TMFFloat.Create(Self, 'articulationParameterArray', []);
  Fields.Add(FFdarticulationParameterArray);
  { X3D specification comment: (-Inf,Inf) }

  FFdcenter := TSFVec3f.Create(Self, 'center', Vector3Single(0, 0, 0));
  Fields.Add(FFdcenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdcollisionType := TSFInt32.Create(Self, 'collisionType', 0);
  Fields.Add(FFdcollisionType);
  { X3D specification comment: [0,255] }

  FFddeadReckoning := TSFInt32.Create(Self, 'deadReckoning', 0);
  Fields.Add(FFddeadReckoning);
  { X3D specification comment: [0,255] }

  FFddetonationLocation := TSFVec3f.Create(Self, 'detonationLocation', Vector3Single(0, 0, 0));
  Fields.Add(FFddetonationLocation);
  { X3D specification comment: (-Inf,Inf) }

  FFddetonationRelativeLocation := TSFVec3f.Create(Self, 'detonationRelativeLocation', Vector3Single(0, 0, 0));
  Fields.Add(FFddetonationRelativeLocation);
  { X3D specification comment: (-Inf,Inf) }

  FFddetonationResult := TSFInt32.Create(Self, 'detonationResult', 0);
  Fields.Add(FFddetonationResult);
  { X3D specification comment: [0,255] }

  FFdenabled := TSFBool.Create(Self, 'enabled', true);
  Fields.Add(FFdenabled);

  FFdentityCategory := TSFInt32.Create(Self, 'entityCategory', 0);
  Fields.Add(FFdentityCategory);
  { X3D specification comment: [0,255] }

  FFdentityCountry := TSFInt32.Create(Self, 'entityCountry', 0);
  Fields.Add(FFdentityCountry);
  { X3D specification comment: [0,65535] }

  FFdentityDomain := TSFInt32.Create(Self, 'entityDomain', 0);
  Fields.Add(FFdentityDomain);
  { X3D specification comment: [0,255] }

  FFdentityExtra := TSFInt32.Create(Self, 'entityExtra', 0);
  Fields.Add(FFdentityExtra);
  { X3D specification comment: [0,255] }

  FFdentityID := TSFInt32.Create(Self, 'entityID', 0);
  Fields.Add(FFdentityID);
  { X3D specification comment: [0,65535] }

  FFdentityKind := TSFInt32.Create(Self, 'entityKind', 0);
  Fields.Add(FFdentityKind);
  { X3D specification comment: [0,255] }

  FFdentitySpecific := TSFInt32.Create(Self, 'entitySpecific', 0);
  Fields.Add(FFdentitySpecific);
  { X3D specification comment: [0,255] }

  FFdentitySubCategory := TSFInt32.Create(Self, 'entitySubCategory', 0);
  Fields.Add(FFdentitySubCategory);
  { X3D specification comment: [0,255] }

  FFdeventApplicationID := TSFInt32.Create(Self, 'eventApplicationID', 1);
  Fields.Add(FFdeventApplicationID);
  { X3D specification comment: [0,65535] }

  FFdeventEntityID := TSFInt32.Create(Self, 'eventEntityID', 0);
  Fields.Add(FFdeventEntityID);
  { X3D specification comment: [0,65535] }

  FFdeventNumber := TSFInt32.Create(Self, 'eventNumber', 0);
  Fields.Add(FFdeventNumber);
  { X3D specification comment: [0,65355] }

  FFdeventSiteID := TSFInt32.Create(Self, 'eventSiteID', 0);
  Fields.Add(FFdeventSiteID);
  { X3D specification comment: [0,65535] }

  FFdfired1 := TSFBool.Create(Self, 'fired1', false);
  Fields.Add(FFdfired1);

  FFdfired2 := TSFBool.Create(Self, 'fired2', false);
  Fields.Add(FFdfired2);

  FFdfireMissionIndex := TSFInt32.Create(Self, 'fireMissionIndex', 0);
  Fields.Add(FFdfireMissionIndex);
  { X3D specification comment: [0,65535] }

  FFdfiringRange := TSFFloat.Create(Self, 'firingRange', 0.0);
  Fields.Add(FFdfiringRange);
  { X3D specification comment: (0,Inf) }

  FFdfiringRate := TSFInt32.Create(Self, 'firingRate', 0);
  Fields.Add(FFdfiringRate);
  { X3D specification comment: [0,65535] }

  FFdforceID := TSFInt32.Create(Self, 'forceID', 0);
  Fields.Add(FFdforceID);
  { X3D specification comment: [0,255] }

  FFdfuse := TSFInt32.Create(Self, 'fuse', 0);
  Fields.Add(FFdfuse);
  { X3D specification comment: [0,65535] }

  FFdlinearVelocity := TSFVec3f.Create(Self, 'linearVelocity', Vector3Single(0, 0, 0));
  Fields.Add(FFdlinearVelocity);
  { X3D specification comment: (-Inf,Inf) }

  FFdlinearAcceleration := TSFVec3f.Create(Self, 'linearAcceleration', Vector3Single(0, 0, 0));
  Fields.Add(FFdlinearAcceleration);
  { X3D specification comment: (-Inf,Inf) }

  FFdmarking := TSFString.Create(Self, 'marking', '');
  Fields.Add(FFdmarking);

  FFdmulticastRelayHost := TSFString.Create(Self, 'multicastRelayHost', '');
  Fields.Add(FFdmulticastRelayHost);

  FFdmulticastRelayPort := TSFInt32.Create(Self, 'multicastRelayPort', 0);
  Fields.Add(FFdmulticastRelayPort);

  FFdmunitionApplicationID := TSFInt32.Create(Self, 'munitionApplicationID', 1);
  Fields.Add(FFdmunitionApplicationID);
  { X3D specification comment: [0,65535] }

  FFdmunitionEndPoint := TSFVec3f.Create(Self, 'munitionEndPoint', Vector3Single(0, 0, 0));
  Fields.Add(FFdmunitionEndPoint);
  { X3D specification comment: (-Inf,Inf) }

  FFdmunitionEntityID := TSFInt32.Create(Self, 'munitionEntityID', 0);
  Fields.Add(FFdmunitionEntityID);
  { X3D specification comment: [0,65535] }

  FFdmunitionQuantity := TSFInt32.Create(Self, 'munitionQuantity', 0);
  Fields.Add(FFdmunitionQuantity);
  { X3D specification comment: [0,65535] }

  FFdmunitionSiteID := TSFInt32.Create(Self, 'munitionSiteID', 0);
  Fields.Add(FFdmunitionSiteID);
  { X3D specification comment: [0,65535] }

  FFdmunitionStartPoint := TSFVec3f.Create(Self, 'munitionStartPoint', Vector3Single(0, 0, 0));
  Fields.Add(FFdmunitionStartPoint);
  { X3D specification comment: (-Inf,Inf) }

  FFdnetworkMode := TSFString.Create(Self, 'networkMode', 'standAlone');
  Fields.Add(FFdnetworkMode);
  { X3D specification comment: ["standAlone"|"networkReader"|"networkWriter"] }

  FFdport := TSFInt32.Create(Self, 'port', 0);
  Fields.Add(FFdport);
  { X3D specification comment: [0,65535] }

  FFdreadInterval := TSFTime.Create(Self, 'readInterval', 0.1);
  Fields.Add(FFdreadInterval);
  { X3D specification comment: [0,Inf) }

  FFdrotation := TSFRotation.Create(Self, 'rotation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdrotation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdscale := TSFVec3f.Create(Self, 'scale', Vector3Single(1, 1, 1));
  Fields.Add(FFdscale);
  { X3D specification comment: (-Inf,Inf) }

  FFdscaleOrientation := TSFRotation.Create(Self, 'scaleOrientation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdscaleOrientation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdsiteID := TSFInt32.Create(Self, 'siteID', 0);
  Fields.Add(FFdsiteID);
  { X3D specification comment: [0,65535] }

  FFdtranslation := TSFVec3f.Create(Self, 'translation', Vector3Single(0, 0, 0));
  Fields.Add(FFdtranslation);
  { X3D specification comment: (-Inf,Inf) }

  FFdwarhead := TSFInt32.Create(Self, 'warhead', 0);
  Fields.Add(FFdwarhead);
  { X3D specification comment: [0,65535] }

  FFdwriteInterval := TSFTime.Create(Self, 'writeInterval', 1.0);
  Fields.Add(FFdwriteInterval);
  { X3D specification comment: [0,Inf) }

  FEventarticulationParameterValue0_changed := TVRMLEvent.Create(Self, 'articulationParameterValue0_changed', TSFFloat, false);
  Events.Add(FEventarticulationParameterValue0_changed);

  FEventarticulationParameterValue1_changed := TVRMLEvent.Create(Self, 'articulationParameterValue1_changed', TSFFloat, false);
  Events.Add(FEventarticulationParameterValue1_changed);

  FEventarticulationParameterValue2_changed := TVRMLEvent.Create(Self, 'articulationParameterValue2_changed', TSFFloat, false);
  Events.Add(FEventarticulationParameterValue2_changed);

  FEventarticulationParameterValue3_changed := TVRMLEvent.Create(Self, 'articulationParameterValue3_changed', TSFFloat, false);
  Events.Add(FEventarticulationParameterValue3_changed);

  FEventarticulationParameterValue4_changed := TVRMLEvent.Create(Self, 'articulationParameterValue4_changed', TSFFloat, false);
  Events.Add(FEventarticulationParameterValue4_changed);

  FEventarticulationParameterValue5_changed := TVRMLEvent.Create(Self, 'articulationParameterValue5_changed', TSFFloat, false);
  Events.Add(FEventarticulationParameterValue5_changed);

  FEventarticulationParameterValue6_changed := TVRMLEvent.Create(Self, 'articulationParameterValue6_changed', TSFFloat, false);
  Events.Add(FEventarticulationParameterValue6_changed);

  FEventarticulationParameterValue7_changed := TVRMLEvent.Create(Self, 'articulationParameterValue7_changed', TSFFloat, false);
  Events.Add(FEventarticulationParameterValue7_changed);

  FEventcollideTime := TVRMLEvent.Create(Self, 'collideTime', TSFTime, false);
  Events.Add(FEventcollideTime);

  FEventdetonateTime := TVRMLEvent.Create(Self, 'detonateTime', TSFTime, false);
  Events.Add(FEventdetonateTime);

  FEventfiredTime := TVRMLEvent.Create(Self, 'firedTime', TSFTime, false);
  Events.Add(FEventfiredTime);

  FEventisActive := TVRMLEvent.Create(Self, 'isActive', TSFBool, false);
  Events.Add(FEventisActive);

  FEventisCollided := TVRMLEvent.Create(Self, 'isCollided', TSFBool, false);
  Events.Add(FEventisCollided);

  FEventisDetonated := TVRMLEvent.Create(Self, 'isDetonated', TSFBool, false);
  Events.Add(FEventisDetonated);

  FEventisNetworkReader := TVRMLEvent.Create(Self, 'isNetworkReader', TSFBool, false);
  Events.Add(FEventisNetworkReader);

  FEventisNetworkWriter := TVRMLEvent.Create(Self, 'isNetworkWriter', TSFBool, false);
  Events.Add(FEventisNetworkWriter);

  FEventisRtpHeaderHeard := TVRMLEvent.Create(Self, 'isRtpHeaderHeard', TSFBool, false);
  Events.Add(FEventisRtpHeaderHeard);

  FEventisStandAlone := TVRMLEvent.Create(Self, 'isStandAlone', TSFBool, false);
  Events.Add(FEventisStandAlone);

  FEventtimestamp := TVRMLEvent.Create(Self, 'timestamp', TSFTime, false);
  Events.Add(FEventtimestamp);

  FFdrtpHeaderExpected := TSFBool.Create(Self, 'rtpHeaderExpected', false);
  FFdrtpHeaderExpected.Exposed := false;
  Fields.Add(FFdrtpHeaderExpected);

  DefaultContainerField := 'children';
end;

class function TNodeEspduTransform.ClassNodeTypeName: string;
begin
  Result := 'EspduTransform';
end;

class function TNodeEspduTransform.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeReceiverPdu.CreateNode;
begin
  inherited;

  FFdaddress := TSFString.Create(Self, 'address', 'localhost');
  Fields.Add(FFdaddress);

  FFdapplicationID := TSFInt32.Create(Self, 'applicationID', 1);
  Fields.Add(FFdapplicationID);
  { X3D specification comment: [0,65535] }

  FFdentityID := TSFInt32.Create(Self, 'entityID', 0);
  Fields.Add(FFdentityID);
  { X3D specification comment: [0,65535] }

  FFdmulticastRelayHost := TSFString.Create(Self, 'multicastRelayHost', '');
  Fields.Add(FFdmulticastRelayHost);

  FFdmulticastRelayPort := TSFInt32.Create(Self, 'multicastRelayPort', 0);
  Fields.Add(FFdmulticastRelayPort);

  FFdnetworkMode := TSFString.Create(Self, 'networkMode', 'standAlone');
  Fields.Add(FFdnetworkMode);
  { X3D specification comment: ["standAlone"|"networkReader"|"networkWriter"] }

  FFdport := TSFInt32.Create(Self, 'port', 0);
  Fields.Add(FFdport);
  { X3D specification comment: [0,65535] }

  FFdradioID := TSFInt32.Create(Self, 'radioID', 0);
  Fields.Add(FFdradioID);
  { X3D specification comment: [0,65535] }

  FFdreadInterval := TSFFloat.Create(Self, 'readInterval', 0.1);
  Fields.Add(FFdreadInterval);
  { X3D specification comment: [0,Inf) }

  FFdreceivedPower := TSFFloat.Create(Self, 'receivedPower', 0.0);
  Fields.Add(FFdreceivedPower);
  { X3D specification comment: [0,Inf) }

  FFdreceiverState := TSFInt32.Create(Self, 'receiverState', 0);
  Fields.Add(FFdreceiverState);
  { X3D specification comment: [0,65535] }

  FFdrtpHeaderExpected := TSFBool.Create(Self, 'rtpHeaderExpected', false);
  Fields.Add(FFdrtpHeaderExpected);

  FFdsiteID := TSFInt32.Create(Self, 'siteID', 0);
  Fields.Add(FFdsiteID);
  { X3D specification comment: [0,65535] }

  FFdtransmitterApplicationID := TSFInt32.Create(Self, 'transmitterApplicationID', 1);
  Fields.Add(FFdtransmitterApplicationID);
  { X3D specification comment: [0,65535] }

  FFdtransmitterEntityID := TSFInt32.Create(Self, 'transmitterEntityID', 0);
  Fields.Add(FFdtransmitterEntityID);
  { X3D specification comment: [0,65535] }

  FFdtransmitterRadioID := TSFInt32.Create(Self, 'transmitterRadioID', 0);
  Fields.Add(FFdtransmitterRadioID);
  { X3D specification comment: [0,65535] }

  FFdtransmitterSiteID := TSFInt32.Create(Self, 'transmitterSiteID', 0);
  Fields.Add(FFdtransmitterSiteID);
  { X3D specification comment: [0,65535] }

  FFdwhichGeometry := TSFInt32.Create(Self, 'whichGeometry', 1);
  Fields.Add(FFdwhichGeometry);
  { X3D specification comment: [-1,Inf) }

  FFdwriteInterval := TSFFloat.Create(Self, 'writeInterval', 1.0);
  Fields.Add(FFdwriteInterval);
  { X3D specification comment: [0,Inf) }

  FEventisNetworkReader := TVRMLEvent.Create(Self, 'isNetworkReader', TSFBool, false);
  Events.Add(FEventisNetworkReader);

  FEventisNetworkWriter := TVRMLEvent.Create(Self, 'isNetworkWriter', TSFBool, false);
  Events.Add(FEventisNetworkWriter);

  FEventisRtpHeaderHeard := TVRMLEvent.Create(Self, 'isRtpHeaderHeard', TSFBool, false);
  Events.Add(FEventisRtpHeaderHeard);

  FEventisStandAlone := TVRMLEvent.Create(Self, 'isStandAlone', TSFBool, false);
  Events.Add(FEventisStandAlone);

  FEventtimestamp := TVRMLEvent.Create(Self, 'timestamp', TSFTime, false);
  Events.Add(FEventtimestamp);

  FFdbboxCenter := TSFVec3f.Create(Self, 'bboxCenter', Vector3Single(0, 0, 0));
  FFdbboxCenter.Exposed := false;
  Fields.Add(FFdbboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdbboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
  FFdbboxSize.Exposed := false;
  Fields.Add(FFdbboxSize);
  { X3D specification comment: [0,Inf) [0,Inf) [0,Inf) or -1 -1 -1 }

  DefaultContainerField := 'children';
end;

class function TNodeReceiverPdu.ClassNodeTypeName: string;
begin
  Result := 'ReceiverPdu';
end;

class function TNodeReceiverPdu.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeSignalPdu.CreateNode;
begin
  inherited;

  FFdaddress := TSFString.Create(Self, 'address', 'localhost');
  Fields.Add(FFdaddress);

  FFdapplicationID := TSFInt32.Create(Self, 'applicationID', 1);
  Fields.Add(FFdapplicationID);
  { X3D specification comment: [0,65535] }

  FFddata := TMFInt32.Create(Self, 'data', []);
  Fields.Add(FFddata);
  { X3D specification comment: [0,255] }

  FFddataLength := TSFInt32.Create(Self, 'dataLength', 0);
  Fields.Add(FFddataLength);
  { X3D specification comment: [0,65535] }

  FFdencodingScheme := TSFInt32.Create(Self, 'encodingScheme', 0);
  Fields.Add(FFdencodingScheme);
  { X3D specification comment: [0,65535] }

  FFdentityID := TSFInt32.Create(Self, 'entityID', 0);
  Fields.Add(FFdentityID);
  { X3D specification comment: [0,65535] }

  FFdmulticastRelayHost := TSFString.Create(Self, 'multicastRelayHost', '');
  Fields.Add(FFdmulticastRelayHost);

  FFdmulticastRelayPort := TSFInt32.Create(Self, 'multicastRelayPort', 0);
  Fields.Add(FFdmulticastRelayPort);

  FFdnetworkMode := TSFString.Create(Self, 'networkMode', 'standAlone');
  Fields.Add(FFdnetworkMode);
  { X3D specification comment: ["standAlone"|"networkReader"|"networkWriter"] }

  FFdport := TSFInt32.Create(Self, 'port', 0);
  Fields.Add(FFdport);
  { X3D specification comment: [0,65535] }

  FFdradioID := TSFInt32.Create(Self, 'radioID', 0);
  Fields.Add(FFdradioID);
  { X3D specification comment: [0,65535] }

  FFdreadInterval := TSFFloat.Create(Self, 'readInterval', 0.1);
  Fields.Add(FFdreadInterval);
  { X3D specification comment: [0,Inf) }

  FFdrtpHeaderExpected := TSFBool.Create(Self, 'rtpHeaderExpected', false);
  Fields.Add(FFdrtpHeaderExpected);

  FFdsampleRate := TSFInt32.Create(Self, 'sampleRate', 0);
  Fields.Add(FFdsampleRate);
  { X3D specification comment: [0,65535] }

  FFdsamples := TSFInt32.Create(Self, 'samples', 0);
  Fields.Add(FFdsamples);
  { X3D specification comment: [0,65535] }

  FFdsiteID := TSFInt32.Create(Self, 'siteID', 0);
  Fields.Add(FFdsiteID);
  { X3D specification comment: [0,65535] }

  FFdtdlType := TSFInt32.Create(Self, 'tdlType', 0);
  Fields.Add(FFdtdlType);
  { X3D specification comment: [0,65535] }

  FFdwhichGeometry := TSFInt32.Create(Self, 'whichGeometry', 1);
  Fields.Add(FFdwhichGeometry);
  { X3D specification comment: [-1,Inf) }

  FFdwriteInterval := TSFFloat.Create(Self, 'writeInterval', 1.0);
  Fields.Add(FFdwriteInterval);
  { X3D specification comment: [0,Inf) }

  FEventisNetworkReader := TVRMLEvent.Create(Self, 'isNetworkReader', TSFBool, false);
  Events.Add(FEventisNetworkReader);

  FEventisNetworkWriter := TVRMLEvent.Create(Self, 'isNetworkWriter', TSFBool, false);
  Events.Add(FEventisNetworkWriter);

  FEventisRtpHeaderHeard := TVRMLEvent.Create(Self, 'isRtpHeaderHeard', TSFBool, false);
  Events.Add(FEventisRtpHeaderHeard);

  FEventisStandAlone := TVRMLEvent.Create(Self, 'isStandAlone', TSFBool, false);
  Events.Add(FEventisStandAlone);

  FEventtimestamp := TVRMLEvent.Create(Self, 'timestamp', TSFTime, false);
  Events.Add(FEventtimestamp);

  FFdbboxCenter := TSFVec3f.Create(Self, 'bboxCenter', Vector3Single(0, 0, 0));
  FFdbboxCenter.Exposed := false;
  Fields.Add(FFdbboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdbboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
  FFdbboxSize.Exposed := false;
  Fields.Add(FFdbboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }

  DefaultContainerField := 'children';
end;

class function TNodeSignalPdu.ClassNodeTypeName: string;
begin
  Result := 'SignalPdu';
end;

class function TNodeSignalPdu.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeTransmitterPdu.CreateNode;
begin
  inherited;

  FFdaddress := TSFString.Create(Self, 'address', 'localhost');
  Fields.Add(FFdaddress);

  FFdantennaLocation := TSFVec3f.Create(Self, 'antennaLocation', Vector3Single(0, 0, 0));
  Fields.Add(FFdantennaLocation);
  { X3D specification comment: (-Inf,Inf) }

  FFdantennaPatternLength := TSFInt32.Create(Self, 'antennaPatternLength', 0);
  Fields.Add(FFdantennaPatternLength);
  { X3D specification comment: [0,65535] }

  FFdantennaPatternType := TSFInt32.Create(Self, 'antennaPatternType', 0);
  Fields.Add(FFdantennaPatternType);
  { X3D specification comment: [0,65535] }

  FFdapplicationID := TSFInt32.Create(Self, 'applicationID', 1);
  Fields.Add(FFdapplicationID);
  { X3D specification comment: [0,65535] }

  FFdcryptoKeyID := TSFInt32.Create(Self, 'cryptoKeyID', 0);
  Fields.Add(FFdcryptoKeyID);
  { X3D specification comment: [0,65535] }

  FFdcryptoSystem := TSFInt32.Create(Self, 'cryptoSystem', 0);
  Fields.Add(FFdcryptoSystem);
  { X3D specification comment: [0,65535] }

  FFdentityID := TSFInt32.Create(Self, 'entityID', 0);
  Fields.Add(FFdentityID);
  { X3D specification comment: [0,65535] }

  FFdfrequency := TSFInt32.Create(Self, 'frequency', 0);
  Fields.Add(FFdfrequency);

  FFdinputSource := TSFInt32.Create(Self, 'inputSource', 0);
  Fields.Add(FFdinputSource);
  { X3D specification comment: [0,255] }

  FFdlengthOfModulationParameters := TSFInt32.Create(Self, 'lengthOfModulationParameters', 0);
  Fields.Add(FFdlengthOfModulationParameters);
  { X3D specification comment: [0,255] }

  FFdmodulationTypeDetail := TSFInt32.Create(Self, 'modulationTypeDetail', 0);
  Fields.Add(FFdmodulationTypeDetail);
  { X3D specification comment: [0,65535] }

  FFdmodulationTypeMajor := TSFInt32.Create(Self, 'modulationTypeMajor', 0);
  Fields.Add(FFdmodulationTypeMajor);
  { X3D specification comment: [0,65535] }

  FFdmodulationTypeSpreadSpectrum := TSFInt32.Create(Self, 'modulationTypeSpreadSpectrum', 0);
  Fields.Add(FFdmodulationTypeSpreadSpectrum);
  { X3D specification comment: [0,65535] }

  FFdmodulationTypeSystem := TSFInt32.Create(Self, 'modulationTypeSystem', 0);
  Fields.Add(FFdmodulationTypeSystem);
  { X3D specification comment: [0,65535] }

  FFdmulticastRelayHost := TSFString.Create(Self, 'multicastRelayHost', '');
  Fields.Add(FFdmulticastRelayHost);

  FFdmulticastRelayPort := TSFInt32.Create(Self, 'multicastRelayPort', 0);
  Fields.Add(FFdmulticastRelayPort);

  FFdnetworkMode := TSFString.Create(Self, 'networkMode', 'standAlone');
  Fields.Add(FFdnetworkMode);
  { X3D specification comment: ["standAlone"|"networkReader"|"networkWriter"] }

  FFdport := TSFInt32.Create(Self, 'port', 0);
  Fields.Add(FFdport);
  { X3D specification comment: [0,65535] }

  FFdpower := TSFFloat.Create(Self, 'power', 0.0);
  Fields.Add(FFdpower);
  { X3D specification comment: [0,Inf) }

  FFdradioEntityTypeCategory := TSFInt32.Create(Self, 'radioEntityTypeCategory', 0);
  Fields.Add(FFdradioEntityTypeCategory);
  { X3D specification comment: [0,255] }

  FFdradioEntityTypeCountry := TSFInt32.Create(Self, 'radioEntityTypeCountry', 0);
  Fields.Add(FFdradioEntityTypeCountry);
  { X3D specification comment: [0,65535] }

  FFdradioEntityTypeDomain := TSFInt32.Create(Self, 'radioEntityTypeDomain', 0);
  Fields.Add(FFdradioEntityTypeDomain);
  { X3D specification comment: [0,255] }

  FFdradioEntityTypeKind := TSFInt32.Create(Self, 'radioEntityTypeKind', 0);
  Fields.Add(FFdradioEntityTypeKind);
  { X3D specification comment: [0,255] }

  FFdradioEntityTypeNomenclature := TSFInt32.Create(Self, 'radioEntityTypeNomenclature', 0);
  Fields.Add(FFdradioEntityTypeNomenclature);
  { X3D specification comment: [0,255] }

  FFdradioEntityTypeNomenclatureVersion := TSFInt32.Create(Self, 'radioEntityTypeNomenclatureVersion', 0);
  Fields.Add(FFdradioEntityTypeNomenclatureVersion);
  { X3D specification comment: [0,65535] }

  FFdradioID := TSFInt32.Create(Self, 'radioID', 0);
  Fields.Add(FFdradioID);
  { X3D specification comment: [0,255] }

  FFdreadInterval := TSFFloat.Create(Self, 'readInterval', 0.1);
  Fields.Add(FFdreadInterval);
  { X3D specification comment: [0,Inf) }

  FFdrelativeAntennaLocation := TSFVec3f.Create(Self, 'relativeAntennaLocation', Vector3Single(0, 0, 0));
  Fields.Add(FFdrelativeAntennaLocation);
  { X3D specification comment: (-Inf,Inf) }

  FFdrtpHeaderExpected := TSFBool.Create(Self, 'rtpHeaderExpected', false);
  Fields.Add(FFdrtpHeaderExpected);

  FFdsiteID := TSFInt32.Create(Self, 'siteID', 0);
  Fields.Add(FFdsiteID);
  { X3D specification comment: [0,65535] }

  FFdtransmitFrequencyBandwidth := TSFFloat.Create(Self, 'transmitFrequencyBandwidth', 0.0);
  Fields.Add(FFdtransmitFrequencyBandwidth);
  { X3D specification comment: (-Inf,Inf) }

  FFdtransmitState := TSFInt32.Create(Self, 'transmitState', 0);
  Fields.Add(FFdtransmitState);
  { X3D specification comment: [0,255] }

  FFdwhichGeometry := TSFInt32.Create(Self, 'whichGeometry', 1);
  Fields.Add(FFdwhichGeometry);
  { X3D specification comment: [-1,Inf) }

  FFdwriteInterval := TSFFloat.Create(Self, 'writeInterval', 1.0);
  Fields.Add(FFdwriteInterval);
  { X3D specification comment: [0,Inf) }

  FEventisNetworkReader := TVRMLEvent.Create(Self, 'isNetworkReader', TSFBool, false);
  Events.Add(FEventisNetworkReader);

  FEventisNetworkWriter := TVRMLEvent.Create(Self, 'isNetworkWriter', TSFBool, false);
  Events.Add(FEventisNetworkWriter);

  FEventisRtpHeaderHeard := TVRMLEvent.Create(Self, 'isRtpHeaderHeard', TSFBool, false);
  Events.Add(FEventisRtpHeaderHeard);

  FEventisStandAlone := TVRMLEvent.Create(Self, 'isStandAlone', TSFBool, false);
  Events.Add(FEventisStandAlone);

  FEventtimestamp := TVRMLEvent.Create(Self, 'timestamp', TSFTime, false);
  Events.Add(FEventtimestamp);

  FFdbboxCenter := TSFVec3f.Create(Self, 'bboxCenter', Vector3Single(0, 0, 0));
  FFdbboxCenter.Exposed := false;
  Fields.Add(FFdbboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdbboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
  FFdbboxSize.Exposed := false;
  Fields.Add(FFdbboxSize);
  { X3D specification comment: [0,Inf) [0,Inf) [0,Inf) or -1 -1 -1 }

  DefaultContainerField := 'children';
end;

class function TNodeTransmitterPdu.ClassNodeTypeName: string;
begin
  Result := 'TransmitterPdu';
end;

class function TNodeTransmitterPdu.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterDISNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeDISEntityManager,
    TNodeDISEntityTypeMapping,
    TNodeEspduTransform,
    TNodeReceiverPdu,
    TNodeSignalPdu,
    TNodeTransmitterPdu
  ]);
end;

{$endif read_implementation}
