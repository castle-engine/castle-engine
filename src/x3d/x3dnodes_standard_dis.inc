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
  TDISEntityManagerNode = class(TAbstractChildNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
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

    { Event out } { }
    private FEventAddedEntities: TMFNodeEvent;
    public property EventAddedEntities: TMFNodeEvent read FEventAddedEntities;

    { Event out } { }
    private FEventRemovedEntities: TMFNodeEvent;
    public property EventRemovedEntities: TMFNodeEvent read FEventRemovedEntities;

    {$I auto_generated_node_helpers/x3dnodes_disentitymanager.inc}
  end;

  TDISEntityTypeMappingNode = class(TAbstractInfoNode, IAbstractUrlObject)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
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

    {$I auto_generated_node_helpers/x3dnodes_disentitytypemapping.inc}
  end;

  TEspduTransformNode = class(TAbstractX3DGroupingNode, IAbstractSensorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event in } { }
    private FEventSet_articulationParameterValue0: TSFFloatEvent;
    public property EventSet_articulationParameterValue0: TSFFloatEvent read FEventSet_articulationParameterValue0;

    { Event in } { }
    private FEventSet_articulationParameterValue1: TSFFloatEvent;
    public property EventSet_articulationParameterValue1: TSFFloatEvent read FEventSet_articulationParameterValue1;

    { Event in } { }
    private FEventSet_articulationParameterValue2: TSFFloatEvent;
    public property EventSet_articulationParameterValue2: TSFFloatEvent read FEventSet_articulationParameterValue2;

    { Event in } { }
    private FEventSet_articulationParameterValue3: TSFFloatEvent;
    public property EventSet_articulationParameterValue3: TSFFloatEvent read FEventSet_articulationParameterValue3;

    { Event in } { }
    private FEventSet_articulationParameterValue4: TSFFloatEvent;
    public property EventSet_articulationParameterValue4: TSFFloatEvent read FEventSet_articulationParameterValue4;

    { Event in } { }
    private FEventSet_articulationParameterValue5: TSFFloatEvent;
    public property EventSet_articulationParameterValue5: TSFFloatEvent read FEventSet_articulationParameterValue5;

    { Event in } { }
    private FEventSet_articulationParameterValue6: TSFFloatEvent;
    public property EventSet_articulationParameterValue6: TSFFloatEvent read FEventSet_articulationParameterValue6;

    { Event in } { }
    private FEventSet_articulationParameterValue7: TSFFloatEvent;
    public property EventSet_articulationParameterValue7: TSFFloatEvent read FEventSet_articulationParameterValue7;

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

    { Event out } { }
    private FEventArticulationParameterValue0_changed: TSFFloatEvent;
    public property EventArticulationParameterValue0_changed: TSFFloatEvent read FEventArticulationParameterValue0_changed;

    { Event out } { }
    private FEventArticulationParameterValue1_changed: TSFFloatEvent;
    public property EventArticulationParameterValue1_changed: TSFFloatEvent read FEventArticulationParameterValue1_changed;

    { Event out } { }
    private FEventArticulationParameterValue2_changed: TSFFloatEvent;
    public property EventArticulationParameterValue2_changed: TSFFloatEvent read FEventArticulationParameterValue2_changed;

    { Event out } { }
    private FEventArticulationParameterValue3_changed: TSFFloatEvent;
    public property EventArticulationParameterValue3_changed: TSFFloatEvent read FEventArticulationParameterValue3_changed;

    { Event out } { }
    private FEventArticulationParameterValue4_changed: TSFFloatEvent;
    public property EventArticulationParameterValue4_changed: TSFFloatEvent read FEventArticulationParameterValue4_changed;

    { Event out } { }
    private FEventArticulationParameterValue5_changed: TSFFloatEvent;
    public property EventArticulationParameterValue5_changed: TSFFloatEvent read FEventArticulationParameterValue5_changed;

    { Event out } { }
    private FEventArticulationParameterValue6_changed: TSFFloatEvent;
    public property EventArticulationParameterValue6_changed: TSFFloatEvent read FEventArticulationParameterValue6_changed;

    { Event out } { }
    private FEventArticulationParameterValue7_changed: TSFFloatEvent;
    public property EventArticulationParameterValue7_changed: TSFFloatEvent read FEventArticulationParameterValue7_changed;

    { Event out } { }
    private FEventCollideTime: TSFTimeEvent;
    public property EventCollideTime: TSFTimeEvent read FEventCollideTime;

    { Event out } { }
    private FEventDetonateTime: TSFTimeEvent;
    public property EventDetonateTime: TSFTimeEvent read FEventDetonateTime;

    { Event out } { }
    private FEventFiredTime: TSFTimeEvent;
    public property EventFiredTime: TSFTimeEvent read FEventFiredTime;

    { Event out } { }
    private FEventIsActive: TSFBoolEvent;
    public property EventIsActive: TSFBoolEvent read FEventIsActive;

    { Event out } { }
    private FEventIsCollided: TSFBoolEvent;
    public property EventIsCollided: TSFBoolEvent read FEventIsCollided;

    { Event out } { }
    private FEventIsDetonated: TSFBoolEvent;
    public property EventIsDetonated: TSFBoolEvent read FEventIsDetonated;

    { Event out } { }
    private FEventIsNetworkReader: TSFBoolEvent;
    public property EventIsNetworkReader: TSFBoolEvent read FEventIsNetworkReader;

    { Event out } { }
    private FEventIsNetworkWriter: TSFBoolEvent;
    public property EventIsNetworkWriter: TSFBoolEvent read FEventIsNetworkWriter;

    { Event out } { }
    private FEventIsRtpHeaderHeard: TSFBoolEvent;
    public property EventIsRtpHeaderHeard: TSFBoolEvent read FEventIsRtpHeaderHeard;

    { Event out } { }
    private FEventIsStandAlone: TSFBoolEvent;
    public property EventIsStandAlone: TSFBoolEvent read FEventIsStandAlone;

    { Event out } { }
    private FEventTimestamp: TSFTimeEvent;
    public property EventTimestamp: TSFTimeEvent read FEventTimestamp;

    private FFdRtpHeaderExpected: TSFBool;
    public property FdRtpHeaderExpected: TSFBool read FFdRtpHeaderExpected;

    {$I auto_generated_node_helpers/x3dnodes_espdutransform.inc}
  end;

  TReceiverPduNode = class(TAbstractSensorNode, IAbstractBoundedObject)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
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

    { Event out } { }
    private FEventIsNetworkReader: TSFBoolEvent;
    public property EventIsNetworkReader: TSFBoolEvent read FEventIsNetworkReader;

    { Event out } { }
    private FEventIsNetworkWriter: TSFBoolEvent;
    public property EventIsNetworkWriter: TSFBoolEvent read FEventIsNetworkWriter;

    { Event out } { }
    private FEventIsRtpHeaderHeard: TSFBoolEvent;
    public property EventIsRtpHeaderHeard: TSFBoolEvent read FEventIsRtpHeaderHeard;

    { Event out } { }
    private FEventIsStandAlone: TSFBoolEvent;
    public property EventIsStandAlone: TSFBoolEvent read FEventIsStandAlone;

    { Event out } { }
    private FEventTimestamp: TSFTimeEvent;
    public property EventTimestamp: TSFTimeEvent read FEventTimestamp;

    private FFdBboxCenter: TSFVec3f;
    public property FdBboxCenter: TSFVec3f read FFdBboxCenter;

    private FFdBboxSize: TSFVec3f;
    public property FdBboxSize: TSFVec3f read FFdBboxSize;

    {$I auto_generated_node_helpers/x3dnodes_receiverpdu.inc}
  end;

  TSignalPduNode = class(TAbstractSensorNode, IAbstractBoundedObject)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
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

    { Event out } { }
    private FEventIsNetworkReader: TSFBoolEvent;
    public property EventIsNetworkReader: TSFBoolEvent read FEventIsNetworkReader;

    { Event out } { }
    private FEventIsNetworkWriter: TSFBoolEvent;
    public property EventIsNetworkWriter: TSFBoolEvent read FEventIsNetworkWriter;

    { Event out } { }
    private FEventIsRtpHeaderHeard: TSFBoolEvent;
    public property EventIsRtpHeaderHeard: TSFBoolEvent read FEventIsRtpHeaderHeard;

    { Event out } { }
    private FEventIsStandAlone: TSFBoolEvent;
    public property EventIsStandAlone: TSFBoolEvent read FEventIsStandAlone;

    { Event out } { }
    private FEventTimestamp: TSFTimeEvent;
    public property EventTimestamp: TSFTimeEvent read FEventTimestamp;

    private FFdBboxCenter: TSFVec3f;
    public property FdBboxCenter: TSFVec3f read FFdBboxCenter;

    private FFdBboxSize: TSFVec3f;
    public property FdBboxSize: TSFVec3f read FFdBboxSize;

    {$I auto_generated_node_helpers/x3dnodes_signalpdu.inc}
  end;

  TTransmitterPduNode = class(TAbstractSensorNode, IAbstractBoundedObject)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
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

    { Event out } { }
    private FEventIsNetworkReader: TSFBoolEvent;
    public property EventIsNetworkReader: TSFBoolEvent read FEventIsNetworkReader;

    { Event out } { }
    private FEventIsNetworkWriter: TSFBoolEvent;
    public property EventIsNetworkWriter: TSFBoolEvent read FEventIsNetworkWriter;

    { Event out } { }
    private FEventIsRtpHeaderHeard: TSFBoolEvent;
    public property EventIsRtpHeaderHeard: TSFBoolEvent read FEventIsRtpHeaderHeard;

    { Event out } { }
    private FEventIsStandAlone: TSFBoolEvent;
    public property EventIsStandAlone: TSFBoolEvent read FEventIsStandAlone;

    { Event out } { }
    private FEventTimestamp: TSFTimeEvent;
    public property EventTimestamp: TSFTimeEvent read FEventTimestamp;

    private FFdBboxCenter: TSFVec3f;
    public property FdBboxCenter: TSFVec3f read FFdBboxCenter;

    private FFdBboxSize: TSFVec3f;
    public property FdBboxSize: TSFVec3f read FFdBboxSize;

    {$I auto_generated_node_helpers/x3dnodes_transmitterpdu.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TDISEntityManagerNode.CreateNode;
begin
  inherited;

  FFdAddress := TSFString.Create(Self, 'address', 'localhost');
  AddField(FFdAddress);

  FFdApplicationID := TSFInt32.Create(Self, 'applicationID', 1);
  AddField(FFdApplicationID);
  { X3D specification comment: [0,65535] }

  FFdMapping := TMFNode.Create(Self, 'mapping', [TDISEntityTypeMappingNode]);
  AddField(FFdMapping);

  FFdPort := TSFInt32.Create(Self, 'port', 0);
  AddField(FFdPort);
  { X3D specification comment: [0,65535] }

  FFdSiteID := TSFInt32.Create(Self, 'siteID', 0);
  AddField(FFdSiteID);
  { X3D specification comment: [0,65535] }

  FEventAddedEntities := TMFNodeEvent.Create(Self, 'addedEntities', false);
  AddEvent(FEventAddedEntities);

  FEventRemovedEntities := TMFNodeEvent.Create(Self, 'removedEntities', false);
  AddEvent(FEventRemovedEntities);

  DefaultContainerField := 'children';
end;

class function TDISEntityManagerNode.ClassX3DType: string;
begin
  Result := 'DISEntityManager';
end;

class function TDISEntityManagerNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TDISEntityTypeMappingNode.CreateNode;
begin
  inherited;

  FFdUrl := TMFString.Create(Self, 'url', []);
  AddField(FFdUrl);
  { X3D specification comment: [URI] }

  FFdCategory := TSFInt32.Create(Self, 'category', 0);
  FFdCategory.Exposed := false;
  AddField(FFdCategory);
  { X3D specification comment: [0,255] }

  FFdCountry := TSFInt32.Create(Self, 'country', 0);
  FFdCountry.Exposed := false;
  AddField(FFdCountry);
  { X3D specification comment: [0,65535] }

  FFdDomain := TSFInt32.Create(Self, 'domain', 0);
  FFdDomain.Exposed := false;
  AddField(FFdDomain);
  { X3D specification comment: [0,255] }

  FFdExtra := TSFInt32.Create(Self, 'extra', 0);
  FFdExtra.Exposed := false;
  AddField(FFdExtra);
  { X3D specification comment: [0,255] }

  FFdKind := TSFInt32.Create(Self, 'kind', 0);
  FFdKind.Exposed := false;
  AddField(FFdKind);
  { X3D specification comment: [0,255] }

  FFdSpecific := TSFInt32.Create(Self, 'specific', 0);
  FFdSpecific.Exposed := false;
  AddField(FFdSpecific);
  { X3D specification comment: [0,255] }

  FFdSubcategory := TSFInt32.Create(Self, 'subcategory', 0);
  FFdSubcategory.Exposed := false;
  AddField(FFdSubcategory);
  { X3D specification comment: [0,255] }

  DefaultContainerField := 'children';
end;

class function TDISEntityTypeMappingNode.ClassX3DType: string;
begin
  Result := 'DISEntityTypeMapping';
end;

class function TDISEntityTypeMappingNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TEspduTransformNode.CreateNode;
begin
  inherited;

  FEventSet_articulationParameterValue0 := TSFFloatEvent.Create(Self, 'set_articulationParameterValue0', true);
  AddEvent(FEventSet_articulationParameterValue0);

  FEventSet_articulationParameterValue1 := TSFFloatEvent.Create(Self, 'set_articulationParameterValue1', true);
  AddEvent(FEventSet_articulationParameterValue1);

  FEventSet_articulationParameterValue2 := TSFFloatEvent.Create(Self, 'set_articulationParameterValue2', true);
  AddEvent(FEventSet_articulationParameterValue2);

  FEventSet_articulationParameterValue3 := TSFFloatEvent.Create(Self, 'set_articulationParameterValue3', true);
  AddEvent(FEventSet_articulationParameterValue3);

  FEventSet_articulationParameterValue4 := TSFFloatEvent.Create(Self, 'set_articulationParameterValue4', true);
  AddEvent(FEventSet_articulationParameterValue4);

  FEventSet_articulationParameterValue5 := TSFFloatEvent.Create(Self, 'set_articulationParameterValue5', true);
  AddEvent(FEventSet_articulationParameterValue5);

  FEventSet_articulationParameterValue6 := TSFFloatEvent.Create(Self, 'set_articulationParameterValue6', true);
  AddEvent(FEventSet_articulationParameterValue6);

  FEventSet_articulationParameterValue7 := TSFFloatEvent.Create(Self, 'set_articulationParameterValue7', true);
  AddEvent(FEventSet_articulationParameterValue7);

  FFdAddress := TSFString.Create(Self, 'address', 'localhost');
  AddField(FFdAddress);

  FFdApplicationID := TSFInt32.Create(Self, 'applicationID', 1);
  AddField(FFdApplicationID);
  { X3D specification comment: [0,65535] }

  FFdArticulationParameterCount := TSFInt32.Create(Self, 'articulationParameterCount', 0);
  AddField(FFdArticulationParameterCount);
  { X3D specification comment: [0,78] }

  FFdArticulationParameterDesignatorArray := TMFInt32.Create(Self, 'articulationParameterDesignatorArray', []);
  AddField(FFdArticulationParameterDesignatorArray);
  { X3D specification comment: [0,255] }

  FFdArticulationParameterChangeIndicatorArray := TMFInt32.Create(Self, 'articulationParameterChangeIndicatorArray', []);
  AddField(FFdArticulationParameterChangeIndicatorArray);
  { X3D specification comment: [0,255] }

  FFdArticulationParameterIdPartAttachedToArray := TMFInt32.Create(Self, 'articulationParameterIdPartAttachedToArray', []);
  AddField(FFdArticulationParameterIdPartAttachedToArray);
  { X3D specification comment: [0,65535] }

  FFdArticulationParameterTypeArray := TMFInt32.Create(Self, 'articulationParameterTypeArray', []);
  AddField(FFdArticulationParameterTypeArray);

  FFdArticulationParameterArray := TMFFloat.Create(Self, 'articulationParameterArray', []);
  AddField(FFdArticulationParameterArray);
  { X3D specification comment: (-Inf,Inf) }

  FFdCenter := TSFVec3f.Create(Self, 'center', Vector3Single(0, 0, 0));
  AddField(FFdCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdCollisionType := TSFInt32.Create(Self, 'collisionType', 0);
  AddField(FFdCollisionType);
  { X3D specification comment: [0,255] }

  FFdDeadReckoning := TSFInt32.Create(Self, 'deadReckoning', 0);
  AddField(FFdDeadReckoning);
  { X3D specification comment: [0,255] }

  FFdDetonationLocation := TSFVec3f.Create(Self, 'detonationLocation', Vector3Single(0, 0, 0));
  AddField(FFdDetonationLocation);
  { X3D specification comment: (-Inf,Inf) }

  FFdDetonationRelativeLocation := TSFVec3f.Create(Self, 'detonationRelativeLocation', Vector3Single(0, 0, 0));
  AddField(FFdDetonationRelativeLocation);
  { X3D specification comment: (-Inf,Inf) }

  FFdDetonationResult := TSFInt32.Create(Self, 'detonationResult', 0);
  AddField(FFdDetonationResult);
  { X3D specification comment: [0,255] }

  FFdEnabled := TSFBool.Create(Self, 'enabled', true);
  AddField(FFdEnabled);

  FFdEntityCategory := TSFInt32.Create(Self, 'entityCategory', 0);
  AddField(FFdEntityCategory);
  { X3D specification comment: [0,255] }

  FFdEntityCountry := TSFInt32.Create(Self, 'entityCountry', 0);
  AddField(FFdEntityCountry);
  { X3D specification comment: [0,65535] }

  FFdEntityDomain := TSFInt32.Create(Self, 'entityDomain', 0);
  AddField(FFdEntityDomain);
  { X3D specification comment: [0,255] }

  FFdEntityExtra := TSFInt32.Create(Self, 'entityExtra', 0);
  AddField(FFdEntityExtra);
  { X3D specification comment: [0,255] }

  FFdEntityID := TSFInt32.Create(Self, 'entityID', 0);
  AddField(FFdEntityID);
  { X3D specification comment: [0,65535] }

  FFdEntityKind := TSFInt32.Create(Self, 'entityKind', 0);
  AddField(FFdEntityKind);
  { X3D specification comment: [0,255] }

  FFdEntitySpecific := TSFInt32.Create(Self, 'entitySpecific', 0);
  AddField(FFdEntitySpecific);
  { X3D specification comment: [0,255] }

  FFdEntitySubCategory := TSFInt32.Create(Self, 'entitySubCategory', 0);
  AddField(FFdEntitySubCategory);
  { X3D specification comment: [0,255] }

  FFdEventApplicationID := TSFInt32.Create(Self, 'eventApplicationID', 1);
  AddField(FFdEventApplicationID);
  { X3D specification comment: [0,65535] }

  FFdEventEntityID := TSFInt32.Create(Self, 'eventEntityID', 0);
  AddField(FFdEventEntityID);
  { X3D specification comment: [0,65535] }

  FFdEventNumber := TSFInt32.Create(Self, 'eventNumber', 0);
  AddField(FFdEventNumber);
  { X3D specification comment: [0,65355] }

  FFdEventSiteID := TSFInt32.Create(Self, 'eventSiteID', 0);
  AddField(FFdEventSiteID);
  { X3D specification comment: [0,65535] }

  FFdFired1 := TSFBool.Create(Self, 'fired1', false);
  AddField(FFdFired1);

  FFdFired2 := TSFBool.Create(Self, 'fired2', false);
  AddField(FFdFired2);

  FFdFireMissionIndex := TSFInt32.Create(Self, 'fireMissionIndex', 0);
  AddField(FFdFireMissionIndex);
  { X3D specification comment: [0,65535] }

  FFdFiringRange := TSFFloat.Create(Self, 'firingRange', 0.0);
  AddField(FFdFiringRange);
  { X3D specification comment: (0,Inf) }

  FFdFiringRate := TSFInt32.Create(Self, 'firingRate', 0);
  AddField(FFdFiringRate);
  { X3D specification comment: [0,65535] }

  FFdForceID := TSFInt32.Create(Self, 'forceID', 0);
  AddField(FFdForceID);
  { X3D specification comment: [0,255] }

  FFdFuse := TSFInt32.Create(Self, 'fuse', 0);
  AddField(FFdFuse);
  { X3D specification comment: [0,65535] }

  FFdLinearVelocity := TSFVec3f.Create(Self, 'linearVelocity', Vector3Single(0, 0, 0));
  AddField(FFdLinearVelocity);
  { X3D specification comment: (-Inf,Inf) }

  FFdLinearAcceleration := TSFVec3f.Create(Self, 'linearAcceleration', Vector3Single(0, 0, 0));
  AddField(FFdLinearAcceleration);
  { X3D specification comment: (-Inf,Inf) }

  FFdMarking := TSFString.Create(Self, 'marking', '');
  AddField(FFdMarking);

  FFdMulticastRelayHost := TSFString.Create(Self, 'multicastRelayHost', '');
  AddField(FFdMulticastRelayHost);

  FFdMulticastRelayPort := TSFInt32.Create(Self, 'multicastRelayPort', 0);
  AddField(FFdMulticastRelayPort);

  FFdMunitionApplicationID := TSFInt32.Create(Self, 'munitionApplicationID', 1);
  AddField(FFdMunitionApplicationID);
  { X3D specification comment: [0,65535] }

  FFdMunitionEndPoint := TSFVec3f.Create(Self, 'munitionEndPoint', Vector3Single(0, 0, 0));
  AddField(FFdMunitionEndPoint);
  { X3D specification comment: (-Inf,Inf) }

  FFdMunitionEntityID := TSFInt32.Create(Self, 'munitionEntityID', 0);
  AddField(FFdMunitionEntityID);
  { X3D specification comment: [0,65535] }

  FFdMunitionQuantity := TSFInt32.Create(Self, 'munitionQuantity', 0);
  AddField(FFdMunitionQuantity);
  { X3D specification comment: [0,65535] }

  FFdMunitionSiteID := TSFInt32.Create(Self, 'munitionSiteID', 0);
  AddField(FFdMunitionSiteID);
  { X3D specification comment: [0,65535] }

  FFdMunitionStartPoint := TSFVec3f.Create(Self, 'munitionStartPoint', Vector3Single(0, 0, 0));
  AddField(FFdMunitionStartPoint);
  { X3D specification comment: (-Inf,Inf) }

  FFdNetworkMode := TSFString.Create(Self, 'networkMode', 'standAlone');
  AddField(FFdNetworkMode);
  { X3D specification comment: ["standAlone"|"networkReader"|"networkWriter"] }

  FFdPort := TSFInt32.Create(Self, 'port', 0);
  AddField(FFdPort);
  { X3D specification comment: [0,65535] }

  FFdReadInterval := TSFTime.Create(Self, 'readInterval', 0.1);
  AddField(FFdReadInterval);
  { X3D specification comment: [0,Inf) }

  FFdRotation := TSFRotation.Create(Self, 'rotation', Vector3Single(0, 0, 1), 0);
  AddField(FFdRotation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdScale := TSFVec3f.Create(Self, 'scale', Vector3Single(1, 1, 1));
  AddField(FFdScale);
  { X3D specification comment: (-Inf,Inf) }

  FFdScaleOrientation := TSFRotation.Create(Self, 'scaleOrientation', Vector3Single(0, 0, 1), 0);
  AddField(FFdScaleOrientation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdSiteID := TSFInt32.Create(Self, 'siteID', 0);
  AddField(FFdSiteID);
  { X3D specification comment: [0,65535] }

  FFdTranslation := TSFVec3f.Create(Self, 'translation', Vector3Single(0, 0, 0));
  AddField(FFdTranslation);
  { X3D specification comment: (-Inf,Inf) }

  FFdWarhead := TSFInt32.Create(Self, 'warhead', 0);
  AddField(FFdWarhead);
  { X3D specification comment: [0,65535] }

  FFdWriteInterval := TSFTime.Create(Self, 'writeInterval', 1.0);
  AddField(FFdWriteInterval);
  { X3D specification comment: [0,Inf) }

  FEventArticulationParameterValue0_changed := TSFFloatEvent.Create(Self, 'articulationParameterValue0_changed', false);
  AddEvent(FEventArticulationParameterValue0_changed);

  FEventArticulationParameterValue1_changed := TSFFloatEvent.Create(Self, 'articulationParameterValue1_changed', false);
  AddEvent(FEventArticulationParameterValue1_changed);

  FEventArticulationParameterValue2_changed := TSFFloatEvent.Create(Self, 'articulationParameterValue2_changed', false);
  AddEvent(FEventArticulationParameterValue2_changed);

  FEventArticulationParameterValue3_changed := TSFFloatEvent.Create(Self, 'articulationParameterValue3_changed', false);
  AddEvent(FEventArticulationParameterValue3_changed);

  FEventArticulationParameterValue4_changed := TSFFloatEvent.Create(Self, 'articulationParameterValue4_changed', false);
  AddEvent(FEventArticulationParameterValue4_changed);

  FEventArticulationParameterValue5_changed := TSFFloatEvent.Create(Self, 'articulationParameterValue5_changed', false);
  AddEvent(FEventArticulationParameterValue5_changed);

  FEventArticulationParameterValue6_changed := TSFFloatEvent.Create(Self, 'articulationParameterValue6_changed', false);
  AddEvent(FEventArticulationParameterValue6_changed);

  FEventArticulationParameterValue7_changed := TSFFloatEvent.Create(Self, 'articulationParameterValue7_changed', false);
  AddEvent(FEventArticulationParameterValue7_changed);

  FEventCollideTime := TSFTimeEvent.Create(Self, 'collideTime', false);
  AddEvent(FEventCollideTime);

  FEventDetonateTime := TSFTimeEvent.Create(Self, 'detonateTime', false);
  AddEvent(FEventDetonateTime);

  FEventFiredTime := TSFTimeEvent.Create(Self, 'firedTime', false);
  AddEvent(FEventFiredTime);

  FEventIsActive := TSFBoolEvent.Create(Self, 'isActive', false);
  AddEvent(FEventIsActive);

  FEventIsCollided := TSFBoolEvent.Create(Self, 'isCollided', false);
  AddEvent(FEventIsCollided);

  FEventIsDetonated := TSFBoolEvent.Create(Self, 'isDetonated', false);
  AddEvent(FEventIsDetonated);

  FEventIsNetworkReader := TSFBoolEvent.Create(Self, 'isNetworkReader', false);
  AddEvent(FEventIsNetworkReader);

  FEventIsNetworkWriter := TSFBoolEvent.Create(Self, 'isNetworkWriter', false);
  AddEvent(FEventIsNetworkWriter);

  FEventIsRtpHeaderHeard := TSFBoolEvent.Create(Self, 'isRtpHeaderHeard', false);
  AddEvent(FEventIsRtpHeaderHeard);

  FEventIsStandAlone := TSFBoolEvent.Create(Self, 'isStandAlone', false);
  AddEvent(FEventIsStandAlone);

  FEventTimestamp := TSFTimeEvent.Create(Self, 'timestamp', false);
  AddEvent(FEventTimestamp);

  FFdRtpHeaderExpected := TSFBool.Create(Self, 'rtpHeaderExpected', false);
  FFdRtpHeaderExpected.Exposed := false;
  AddField(FFdRtpHeaderExpected);

  DefaultContainerField := 'children';
end;

class function TEspduTransformNode.ClassX3DType: string;
begin
  Result := 'EspduTransform';
end;

class function TEspduTransformNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TReceiverPduNode.CreateNode;
begin
  inherited;

  FFdAddress := TSFString.Create(Self, 'address', 'localhost');
  AddField(FFdAddress);

  FFdApplicationID := TSFInt32.Create(Self, 'applicationID', 1);
  AddField(FFdApplicationID);
  { X3D specification comment: [0,65535] }

  FFdEntityID := TSFInt32.Create(Self, 'entityID', 0);
  AddField(FFdEntityID);
  { X3D specification comment: [0,65535] }

  FFdMulticastRelayHost := TSFString.Create(Self, 'multicastRelayHost', '');
  AddField(FFdMulticastRelayHost);

  FFdMulticastRelayPort := TSFInt32.Create(Self, 'multicastRelayPort', 0);
  AddField(FFdMulticastRelayPort);

  FFdNetworkMode := TSFString.Create(Self, 'networkMode', 'standAlone');
  AddField(FFdNetworkMode);
  { X3D specification comment: ["standAlone"|"networkReader"|"networkWriter"] }

  FFdPort := TSFInt32.Create(Self, 'port', 0);
  AddField(FFdPort);
  { X3D specification comment: [0,65535] }

  FFdRadioID := TSFInt32.Create(Self, 'radioID', 0);
  AddField(FFdRadioID);
  { X3D specification comment: [0,65535] }

  FFdReadInterval := TSFFloat.Create(Self, 'readInterval', 0.1);
  AddField(FFdReadInterval);
  { X3D specification comment: [0,Inf) }

  FFdReceivedPower := TSFFloat.Create(Self, 'receivedPower', 0.0);
  AddField(FFdReceivedPower);
  { X3D specification comment: [0,Inf) }

  FFdReceiverState := TSFInt32.Create(Self, 'receiverState', 0);
  AddField(FFdReceiverState);
  { X3D specification comment: [0,65535] }

  FFdRtpHeaderExpected := TSFBool.Create(Self, 'rtpHeaderExpected', false);
  AddField(FFdRtpHeaderExpected);

  FFdSiteID := TSFInt32.Create(Self, 'siteID', 0);
  AddField(FFdSiteID);
  { X3D specification comment: [0,65535] }

  FFdTransmitterApplicationID := TSFInt32.Create(Self, 'transmitterApplicationID', 1);
  AddField(FFdTransmitterApplicationID);
  { X3D specification comment: [0,65535] }

  FFdTransmitterEntityID := TSFInt32.Create(Self, 'transmitterEntityID', 0);
  AddField(FFdTransmitterEntityID);
  { X3D specification comment: [0,65535] }

  FFdTransmitterRadioID := TSFInt32.Create(Self, 'transmitterRadioID', 0);
  AddField(FFdTransmitterRadioID);
  { X3D specification comment: [0,65535] }

  FFdTransmitterSiteID := TSFInt32.Create(Self, 'transmitterSiteID', 0);
  AddField(FFdTransmitterSiteID);
  { X3D specification comment: [0,65535] }

  FFdWhichGeometry := TSFInt32.Create(Self, 'whichGeometry', 1);
  AddField(FFdWhichGeometry);
  { X3D specification comment: [-1,Inf) }

  FFdWriteInterval := TSFFloat.Create(Self, 'writeInterval', 1.0);
  AddField(FFdWriteInterval);
  { X3D specification comment: [0,Inf) }

  FEventIsNetworkReader := TSFBoolEvent.Create(Self, 'isNetworkReader', false);
  AddEvent(FEventIsNetworkReader);

  FEventIsNetworkWriter := TSFBoolEvent.Create(Self, 'isNetworkWriter', false);
  AddEvent(FEventIsNetworkWriter);

  FEventIsRtpHeaderHeard := TSFBoolEvent.Create(Self, 'isRtpHeaderHeard', false);
  AddEvent(FEventIsRtpHeaderHeard);

  FEventIsStandAlone := TSFBoolEvent.Create(Self, 'isStandAlone', false);
  AddEvent(FEventIsStandAlone);

  FEventTimestamp := TSFTimeEvent.Create(Self, 'timestamp', false);
  AddEvent(FEventTimestamp);

  FFdBboxCenter := TSFVec3f.Create(Self, 'bboxCenter', Vector3Single(0, 0, 0));
  FFdBboxCenter.Exposed := false;
  AddField(FFdBboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdBboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
  FFdBboxSize.Exposed := false;
  AddField(FFdBboxSize);
  { X3D specification comment: [0,Inf) [0,Inf) [0,Inf) or -1 -1 -1 }

  DefaultContainerField := 'children';
end;

class function TReceiverPduNode.ClassX3DType: string;
begin
  Result := 'ReceiverPdu';
end;

class function TReceiverPduNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TSignalPduNode.CreateNode;
begin
  inherited;

  FFdAddress := TSFString.Create(Self, 'address', 'localhost');
  AddField(FFdAddress);

  FFdApplicationID := TSFInt32.Create(Self, 'applicationID', 1);
  AddField(FFdApplicationID);
  { X3D specification comment: [0,65535] }

  FFdData := TMFInt32.Create(Self, 'data', []);
  AddField(FFdData);
  { X3D specification comment: [0,255] }

  FFdDataLength := TSFInt32.Create(Self, 'dataLength', 0);
  AddField(FFdDataLength);
  { X3D specification comment: [0,65535] }

  FFdEncodingScheme := TSFInt32.Create(Self, 'encodingScheme', 0);
  AddField(FFdEncodingScheme);
  { X3D specification comment: [0,65535] }

  FFdEntityID := TSFInt32.Create(Self, 'entityID', 0);
  AddField(FFdEntityID);
  { X3D specification comment: [0,65535] }

  FFdMulticastRelayHost := TSFString.Create(Self, 'multicastRelayHost', '');
  AddField(FFdMulticastRelayHost);

  FFdMulticastRelayPort := TSFInt32.Create(Self, 'multicastRelayPort', 0);
  AddField(FFdMulticastRelayPort);

  FFdNetworkMode := TSFString.Create(Self, 'networkMode', 'standAlone');
  AddField(FFdNetworkMode);
  { X3D specification comment: ["standAlone"|"networkReader"|"networkWriter"] }

  FFdPort := TSFInt32.Create(Self, 'port', 0);
  AddField(FFdPort);
  { X3D specification comment: [0,65535] }

  FFdRadioID := TSFInt32.Create(Self, 'radioID', 0);
  AddField(FFdRadioID);
  { X3D specification comment: [0,65535] }

  FFdReadInterval := TSFFloat.Create(Self, 'readInterval', 0.1);
  AddField(FFdReadInterval);
  { X3D specification comment: [0,Inf) }

  FFdRtpHeaderExpected := TSFBool.Create(Self, 'rtpHeaderExpected', false);
  AddField(FFdRtpHeaderExpected);

  FFdSampleRate := TSFInt32.Create(Self, 'sampleRate', 0);
  AddField(FFdSampleRate);
  { X3D specification comment: [0,65535] }

  FFdSamples := TSFInt32.Create(Self, 'samples', 0);
  AddField(FFdSamples);
  { X3D specification comment: [0,65535] }

  FFdSiteID := TSFInt32.Create(Self, 'siteID', 0);
  AddField(FFdSiteID);
  { X3D specification comment: [0,65535] }

  FFdTdlType := TSFInt32.Create(Self, 'tdlType', 0);
  AddField(FFdTdlType);
  { X3D specification comment: [0,65535] }

  FFdWhichGeometry := TSFInt32.Create(Self, 'whichGeometry', 1);
  AddField(FFdWhichGeometry);
  { X3D specification comment: [-1,Inf) }

  FFdWriteInterval := TSFFloat.Create(Self, 'writeInterval', 1.0);
  AddField(FFdWriteInterval);
  { X3D specification comment: [0,Inf) }

  FEventIsNetworkReader := TSFBoolEvent.Create(Self, 'isNetworkReader', false);
  AddEvent(FEventIsNetworkReader);

  FEventIsNetworkWriter := TSFBoolEvent.Create(Self, 'isNetworkWriter', false);
  AddEvent(FEventIsNetworkWriter);

  FEventIsRtpHeaderHeard := TSFBoolEvent.Create(Self, 'isRtpHeaderHeard', false);
  AddEvent(FEventIsRtpHeaderHeard);

  FEventIsStandAlone := TSFBoolEvent.Create(Self, 'isStandAlone', false);
  AddEvent(FEventIsStandAlone);

  FEventTimestamp := TSFTimeEvent.Create(Self, 'timestamp', false);
  AddEvent(FEventTimestamp);

  FFdBboxCenter := TSFVec3f.Create(Self, 'bboxCenter', Vector3Single(0, 0, 0));
  FFdBboxCenter.Exposed := false;
  AddField(FFdBboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdBboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
  FFdBboxSize.Exposed := false;
  AddField(FFdBboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }

  DefaultContainerField := 'children';
end;

class function TSignalPduNode.ClassX3DType: string;
begin
  Result := 'SignalPdu';
end;

class function TSignalPduNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TTransmitterPduNode.CreateNode;
begin
  inherited;

  FFdAddress := TSFString.Create(Self, 'address', 'localhost');
  AddField(FFdAddress);

  FFdAntennaLocation := TSFVec3f.Create(Self, 'antennaLocation', Vector3Single(0, 0, 0));
  AddField(FFdAntennaLocation);
  { X3D specification comment: (-Inf,Inf) }

  FFdAntennaPatternLength := TSFInt32.Create(Self, 'antennaPatternLength', 0);
  AddField(FFdAntennaPatternLength);
  { X3D specification comment: [0,65535] }

  FFdAntennaPatternType := TSFInt32.Create(Self, 'antennaPatternType', 0);
  AddField(FFdAntennaPatternType);
  { X3D specification comment: [0,65535] }

  FFdApplicationID := TSFInt32.Create(Self, 'applicationID', 1);
  AddField(FFdApplicationID);
  { X3D specification comment: [0,65535] }

  FFdCryptoKeyID := TSFInt32.Create(Self, 'cryptoKeyID', 0);
  AddField(FFdCryptoKeyID);
  { X3D specification comment: [0,65535] }

  FFdCryptoSystem := TSFInt32.Create(Self, 'cryptoSystem', 0);
  AddField(FFdCryptoSystem);
  { X3D specification comment: [0,65535] }

  FFdEntityID := TSFInt32.Create(Self, 'entityID', 0);
  AddField(FFdEntityID);
  { X3D specification comment: [0,65535] }

  FFdFrequency := TSFInt32.Create(Self, 'frequency', 0);
  AddField(FFdFrequency);

  FFdInputSource := TSFInt32.Create(Self, 'inputSource', 0);
  AddField(FFdInputSource);
  { X3D specification comment: [0,255] }

  FFdLengthOfModulationParameters := TSFInt32.Create(Self, 'lengthOfModulationParameters', 0);
  AddField(FFdLengthOfModulationParameters);
  { X3D specification comment: [0,255] }

  FFdModulationTypeDetail := TSFInt32.Create(Self, 'modulationTypeDetail', 0);
  AddField(FFdModulationTypeDetail);
  { X3D specification comment: [0,65535] }

  FFdModulationTypeMajor := TSFInt32.Create(Self, 'modulationTypeMajor', 0);
  AddField(FFdModulationTypeMajor);
  { X3D specification comment: [0,65535] }

  FFdModulationTypeSpreadSpectrum := TSFInt32.Create(Self, 'modulationTypeSpreadSpectrum', 0);
  AddField(FFdModulationTypeSpreadSpectrum);
  { X3D specification comment: [0,65535] }

  FFdModulationTypeSystem := TSFInt32.Create(Self, 'modulationTypeSystem', 0);
  AddField(FFdModulationTypeSystem);
  { X3D specification comment: [0,65535] }

  FFdMulticastRelayHost := TSFString.Create(Self, 'multicastRelayHost', '');
  AddField(FFdMulticastRelayHost);

  FFdMulticastRelayPort := TSFInt32.Create(Self, 'multicastRelayPort', 0);
  AddField(FFdMulticastRelayPort);

  FFdNetworkMode := TSFString.Create(Self, 'networkMode', 'standAlone');
  AddField(FFdNetworkMode);
  { X3D specification comment: ["standAlone"|"networkReader"|"networkWriter"] }

  FFdPort := TSFInt32.Create(Self, 'port', 0);
  AddField(FFdPort);
  { X3D specification comment: [0,65535] }

  FFdPower := TSFFloat.Create(Self, 'power', 0.0);
  AddField(FFdPower);
  { X3D specification comment: [0,Inf) }

  FFdRadioEntityTypeCategory := TSFInt32.Create(Self, 'radioEntityTypeCategory', 0);
  AddField(FFdRadioEntityTypeCategory);
  { X3D specification comment: [0,255] }

  FFdRadioEntityTypeCountry := TSFInt32.Create(Self, 'radioEntityTypeCountry', 0);
  AddField(FFdRadioEntityTypeCountry);
  { X3D specification comment: [0,65535] }

  FFdRadioEntityTypeDomain := TSFInt32.Create(Self, 'radioEntityTypeDomain', 0);
  AddField(FFdRadioEntityTypeDomain);
  { X3D specification comment: [0,255] }

  FFdRadioEntityTypeKind := TSFInt32.Create(Self, 'radioEntityTypeKind', 0);
  AddField(FFdRadioEntityTypeKind);
  { X3D specification comment: [0,255] }

  FFdRadioEntityTypeNomenclature := TSFInt32.Create(Self, 'radioEntityTypeNomenclature', 0);
  AddField(FFdRadioEntityTypeNomenclature);
  { X3D specification comment: [0,255] }

  FFdRadioEntityTypeNomenclatureVersion := TSFInt32.Create(Self, 'radioEntityTypeNomenclatureVersion', 0);
  AddField(FFdRadioEntityTypeNomenclatureVersion);
  { X3D specification comment: [0,65535] }

  FFdRadioID := TSFInt32.Create(Self, 'radioID', 0);
  AddField(FFdRadioID);
  { X3D specification comment: [0,255] }

  FFdReadInterval := TSFFloat.Create(Self, 'readInterval', 0.1);
  AddField(FFdReadInterval);
  { X3D specification comment: [0,Inf) }

  FFdRelativeAntennaLocation := TSFVec3f.Create(Self, 'relativeAntennaLocation', Vector3Single(0, 0, 0));
  AddField(FFdRelativeAntennaLocation);
  { X3D specification comment: (-Inf,Inf) }

  FFdRtpHeaderExpected := TSFBool.Create(Self, 'rtpHeaderExpected', false);
  AddField(FFdRtpHeaderExpected);

  FFdSiteID := TSFInt32.Create(Self, 'siteID', 0);
  AddField(FFdSiteID);
  { X3D specification comment: [0,65535] }

  FFdTransmitFrequencyBandwidth := TSFFloat.Create(Self, 'transmitFrequencyBandwidth', 0.0);
  AddField(FFdTransmitFrequencyBandwidth);
  { X3D specification comment: (-Inf,Inf) }

  FFdTransmitState := TSFInt32.Create(Self, 'transmitState', 0);
  AddField(FFdTransmitState);
  { X3D specification comment: [0,255] }

  FFdWhichGeometry := TSFInt32.Create(Self, 'whichGeometry', 1);
  AddField(FFdWhichGeometry);
  { X3D specification comment: [-1,Inf) }

  FFdWriteInterval := TSFFloat.Create(Self, 'writeInterval', 1.0);
  AddField(FFdWriteInterval);
  { X3D specification comment: [0,Inf) }

  FEventIsNetworkReader := TSFBoolEvent.Create(Self, 'isNetworkReader', false);
  AddEvent(FEventIsNetworkReader);

  FEventIsNetworkWriter := TSFBoolEvent.Create(Self, 'isNetworkWriter', false);
  AddEvent(FEventIsNetworkWriter);

  FEventIsRtpHeaderHeard := TSFBoolEvent.Create(Self, 'isRtpHeaderHeard', false);
  AddEvent(FEventIsRtpHeaderHeard);

  FEventIsStandAlone := TSFBoolEvent.Create(Self, 'isStandAlone', false);
  AddEvent(FEventIsStandAlone);

  FEventTimestamp := TSFTimeEvent.Create(Self, 'timestamp', false);
  AddEvent(FEventTimestamp);

  FFdBboxCenter := TSFVec3f.Create(Self, 'bboxCenter', Vector3Single(0, 0, 0));
  FFdBboxCenter.Exposed := false;
  AddField(FFdBboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdBboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
  FFdBboxSize.Exposed := false;
  AddField(FFdBboxSize);
  { X3D specification comment: [0,Inf) [0,Inf) [0,Inf) or -1 -1 -1 }

  DefaultContainerField := 'children';
end;

class function TTransmitterPduNode.ClassX3DType: string;
begin
  Result := 'TransmitterPdu';
end;

class function TTransmitterPduNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure RegisterDISNodes;
begin
  NodesManager.RegisterNodeClasses([
    TDISEntityManagerNode,
    TDISEntityTypeMappingNode,
    TEspduTransformNode,
    TReceiverPduNode,
    TSignalPduNode,
    TTransmitterPduNode
  ]);
end;

{$endif read_implementation}
