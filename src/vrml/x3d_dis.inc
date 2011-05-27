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
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdaddress: TSFString;
    public property Fdaddress: TSFString read FFdaddress;

    private FFdapplicationID: TSFInt32;
    public property FdapplicationID: TSFInt32 read FFdapplicationID;

    private FFdmapping: TMFNode;
    public property Fdmapping: TMFNode read FFdmapping;

    private FFdport: TSFInt32;
    public property Fdport: TSFInt32 read FFdport;

    private FFdsiteID: TSFInt32;
    public property FdsiteID: TSFInt32 read FFdsiteID;

    { Event: MFNode, out } { }
    private FEventaddedEntities: TVRMLEvent;
    public property EventaddedEntities: TVRMLEvent read FEventaddedEntities;

    { Event: MFNode, out } { }
    private FEventremovedEntities: TVRMLEvent;
    public property EventremovedEntities: TVRMLEvent read FEventremovedEntities;
  end;

  TNodeDISEntityTypeMapping = class(TNodeX3DInfoNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

    private FFdcategory: TSFInt32;
    public property Fdcategory: TSFInt32 read FFdcategory;

    private FFdcountry: TSFInt32;
    public property Fdcountry: TSFInt32 read FFdcountry;

    private FFddomain: TSFInt32;
    public property Fddomain: TSFInt32 read FFddomain;

    private FFdextra: TSFInt32;
    public property Fdextra: TSFInt32 read FFdextra;

    private FFdkind: TSFInt32;
    public property Fdkind: TSFInt32 read FFdkind;

    private FFdspecific: TSFInt32;
    public property Fdspecific: TSFInt32 read FFdspecific;

    private FFdsubcategory: TSFInt32;
    public property Fdsubcategory: TSFInt32 read FFdsubcategory;
  end;

  TNodeEspduTransform = class(TNodeX3DGroupingNode, INodeX3DSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFFloat, in } { }
    private FEventset_articulationParameterValue0: TVRMLEvent;
    public property Eventset_articulationParameterValue0: TVRMLEvent read FEventset_articulationParameterValue0;

    { Event: SFFloat, in } { }
    private FEventset_articulationParameterValue1: TVRMLEvent;
    public property Eventset_articulationParameterValue1: TVRMLEvent read FEventset_articulationParameterValue1;

    { Event: SFFloat, in } { }
    private FEventset_articulationParameterValue2: TVRMLEvent;
    public property Eventset_articulationParameterValue2: TVRMLEvent read FEventset_articulationParameterValue2;

    { Event: SFFloat, in } { }
    private FEventset_articulationParameterValue3: TVRMLEvent;
    public property Eventset_articulationParameterValue3: TVRMLEvent read FEventset_articulationParameterValue3;

    { Event: SFFloat, in } { }
    private FEventset_articulationParameterValue4: TVRMLEvent;
    public property Eventset_articulationParameterValue4: TVRMLEvent read FEventset_articulationParameterValue4;

    { Event: SFFloat, in } { }
    private FEventset_articulationParameterValue5: TVRMLEvent;
    public property Eventset_articulationParameterValue5: TVRMLEvent read FEventset_articulationParameterValue5;

    { Event: SFFloat, in } { }
    private FEventset_articulationParameterValue6: TVRMLEvent;
    public property Eventset_articulationParameterValue6: TVRMLEvent read FEventset_articulationParameterValue6;

    { Event: SFFloat, in } { }
    private FEventset_articulationParameterValue7: TVRMLEvent;
    public property Eventset_articulationParameterValue7: TVRMLEvent read FEventset_articulationParameterValue7;

    private FFdaddress: TSFString;
    public property Fdaddress: TSFString read FFdaddress;

    private FFdapplicationID: TSFInt32;
    public property FdapplicationID: TSFInt32 read FFdapplicationID;

    private FFdarticulationParameterCount: TSFInt32;
    public property FdarticulationParameterCount: TSFInt32 read FFdarticulationParameterCount;

    private FFdarticulationParameterDesignatorArray: TMFInt32;
    public property FdarticulationParameterDesignatorArray: TMFInt32 read FFdarticulationParameterDesignatorArray;

    private FFdarticulationParameterChangeIndicatorArray: TMFInt32;
    public property FdarticulationParameterChangeIndicatorArray: TMFInt32 read FFdarticulationParameterChangeIndicatorArray;

    private FFdarticulationParameterIdPartAttachedToArray: TMFInt32;
    public property FdarticulationParameterIdPartAttachedToArray: TMFInt32 read FFdarticulationParameterIdPartAttachedToArray;

    private FFdarticulationParameterTypeArray: TMFInt32;
    public property FdarticulationParameterTypeArray: TMFInt32 read FFdarticulationParameterTypeArray;

    private FFdarticulationParameterArray: TMFFloat;
    public property FdarticulationParameterArray: TMFFloat read FFdarticulationParameterArray;

    private FFdcenter: TSFVec3f;
    public property Fdcenter: TSFVec3f read FFdcenter;

    private FFdcollisionType: TSFInt32;
    public property FdcollisionType: TSFInt32 read FFdcollisionType;

    private FFddeadReckoning: TSFInt32;
    public property FddeadReckoning: TSFInt32 read FFddeadReckoning;

    private FFddetonationLocation: TSFVec3f;
    public property FddetonationLocation: TSFVec3f read FFddetonationLocation;

    private FFddetonationRelativeLocation: TSFVec3f;
    public property FddetonationRelativeLocation: TSFVec3f read FFddetonationRelativeLocation;

    private FFddetonationResult: TSFInt32;
    public property FddetonationResult: TSFInt32 read FFddetonationResult;

    private FFdenabled: TSFBool;
    public property Fdenabled: TSFBool read FFdenabled;

    private FFdentityCategory: TSFInt32;
    public property FdentityCategory: TSFInt32 read FFdentityCategory;

    private FFdentityCountry: TSFInt32;
    public property FdentityCountry: TSFInt32 read FFdentityCountry;

    private FFdentityDomain: TSFInt32;
    public property FdentityDomain: TSFInt32 read FFdentityDomain;

    private FFdentityExtra: TSFInt32;
    public property FdentityExtra: TSFInt32 read FFdentityExtra;

    private FFdentityID: TSFInt32;
    public property FdentityID: TSFInt32 read FFdentityID;

    private FFdentityKind: TSFInt32;
    public property FdentityKind: TSFInt32 read FFdentityKind;

    private FFdentitySpecific: TSFInt32;
    public property FdentitySpecific: TSFInt32 read FFdentitySpecific;

    private FFdentitySubCategory: TSFInt32;
    public property FdentitySubCategory: TSFInt32 read FFdentitySubCategory;

    private FFdeventApplicationID: TSFInt32;
    public property FdeventApplicationID: TSFInt32 read FFdeventApplicationID;

    private FFdeventEntityID: TSFInt32;
    public property FdeventEntityID: TSFInt32 read FFdeventEntityID;

    private FFdeventNumber: TSFInt32;
    public property FdeventNumber: TSFInt32 read FFdeventNumber;

    private FFdeventSiteID: TSFInt32;
    public property FdeventSiteID: TSFInt32 read FFdeventSiteID;

    private FFdfired1: TSFBool;
    public property Fdfired1: TSFBool read FFdfired1;

    private FFdfired2: TSFBool;
    public property Fdfired2: TSFBool read FFdfired2;

    private FFdfireMissionIndex: TSFInt32;
    public property FdfireMissionIndex: TSFInt32 read FFdfireMissionIndex;

    private FFdfiringRange: TSFFloat;
    public property FdfiringRange: TSFFloat read FFdfiringRange;

    private FFdfiringRate: TSFInt32;
    public property FdfiringRate: TSFInt32 read FFdfiringRate;

    private FFdforceID: TSFInt32;
    public property FdforceID: TSFInt32 read FFdforceID;

    private FFdfuse: TSFInt32;
    public property Fdfuse: TSFInt32 read FFdfuse;

    private FFdlinearVelocity: TSFVec3f;
    public property FdlinearVelocity: TSFVec3f read FFdlinearVelocity;

    private FFdlinearAcceleration: TSFVec3f;
    public property FdlinearAcceleration: TSFVec3f read FFdlinearAcceleration;

    private FFdmarking: TSFString;
    public property Fdmarking: TSFString read FFdmarking;

    private FFdmulticastRelayHost: TSFString;
    public property FdmulticastRelayHost: TSFString read FFdmulticastRelayHost;

    private FFdmulticastRelayPort: TSFInt32;
    public property FdmulticastRelayPort: TSFInt32 read FFdmulticastRelayPort;

    private FFdmunitionApplicationID: TSFInt32;
    public property FdmunitionApplicationID: TSFInt32 read FFdmunitionApplicationID;

    private FFdmunitionEndPoint: TSFVec3f;
    public property FdmunitionEndPoint: TSFVec3f read FFdmunitionEndPoint;

    private FFdmunitionEntityID: TSFInt32;
    public property FdmunitionEntityID: TSFInt32 read FFdmunitionEntityID;

    private FFdmunitionQuantity: TSFInt32;
    public property FdmunitionQuantity: TSFInt32 read FFdmunitionQuantity;

    private FFdmunitionSiteID: TSFInt32;
    public property FdmunitionSiteID: TSFInt32 read FFdmunitionSiteID;

    private FFdmunitionStartPoint: TSFVec3f;
    public property FdmunitionStartPoint: TSFVec3f read FFdmunitionStartPoint;

    private FFdnetworkMode: TSFString;
    public property FdnetworkMode: TSFString read FFdnetworkMode;

    private FFdport: TSFInt32;
    public property Fdport: TSFInt32 read FFdport;

    private FFdreadInterval: TSFTime;
    public property FdreadInterval: TSFTime read FFdreadInterval;

    private FFdrotation: TSFRotation;
    public property Fdrotation: TSFRotation read FFdrotation;

    private FFdscale: TSFVec3f;
    public property Fdscale: TSFVec3f read FFdscale;

    private FFdscaleOrientation: TSFRotation;
    public property FdscaleOrientation: TSFRotation read FFdscaleOrientation;

    private FFdsiteID: TSFInt32;
    public property FdsiteID: TSFInt32 read FFdsiteID;

    private FFdtranslation: TSFVec3f;
    public property Fdtranslation: TSFVec3f read FFdtranslation;

    private FFdwarhead: TSFInt32;
    public property Fdwarhead: TSFInt32 read FFdwarhead;

    private FFdwriteInterval: TSFTime;
    public property FdwriteInterval: TSFTime read FFdwriteInterval;

    { Event: SFFloat, out } { }
    private FEventarticulationParameterValue0_changed: TVRMLEvent;
    public property EventarticulationParameterValue0_changed: TVRMLEvent read FEventarticulationParameterValue0_changed;

    { Event: SFFloat, out } { }
    private FEventarticulationParameterValue1_changed: TVRMLEvent;
    public property EventarticulationParameterValue1_changed: TVRMLEvent read FEventarticulationParameterValue1_changed;

    { Event: SFFloat, out } { }
    private FEventarticulationParameterValue2_changed: TVRMLEvent;
    public property EventarticulationParameterValue2_changed: TVRMLEvent read FEventarticulationParameterValue2_changed;

    { Event: SFFloat, out } { }
    private FEventarticulationParameterValue3_changed: TVRMLEvent;
    public property EventarticulationParameterValue3_changed: TVRMLEvent read FEventarticulationParameterValue3_changed;

    { Event: SFFloat, out } { }
    private FEventarticulationParameterValue4_changed: TVRMLEvent;
    public property EventarticulationParameterValue4_changed: TVRMLEvent read FEventarticulationParameterValue4_changed;

    { Event: SFFloat, out } { }
    private FEventarticulationParameterValue5_changed: TVRMLEvent;
    public property EventarticulationParameterValue5_changed: TVRMLEvent read FEventarticulationParameterValue5_changed;

    { Event: SFFloat, out } { }
    private FEventarticulationParameterValue6_changed: TVRMLEvent;
    public property EventarticulationParameterValue6_changed: TVRMLEvent read FEventarticulationParameterValue6_changed;

    { Event: SFFloat, out } { }
    private FEventarticulationParameterValue7_changed: TVRMLEvent;
    public property EventarticulationParameterValue7_changed: TVRMLEvent read FEventarticulationParameterValue7_changed;

    { Event: SFTime, out } { }
    private FEventcollideTime: TVRMLEvent;
    public property EventcollideTime: TVRMLEvent read FEventcollideTime;

    { Event: SFTime, out } { }
    private FEventdetonateTime: TVRMLEvent;
    public property EventdetonateTime: TVRMLEvent read FEventdetonateTime;

    { Event: SFTime, out } { }
    private FEventfiredTime: TVRMLEvent;
    public property EventfiredTime: TVRMLEvent read FEventfiredTime;

    { Event: SFBool, out } { }
    private FEventisActive: TVRMLEvent;
    public property EventisActive: TVRMLEvent read FEventisActive;

    { Event: SFBool, out } { }
    private FEventisCollided: TVRMLEvent;
    public property EventisCollided: TVRMLEvent read FEventisCollided;

    { Event: SFBool, out } { }
    private FEventisDetonated: TVRMLEvent;
    public property EventisDetonated: TVRMLEvent read FEventisDetonated;

    { Event: SFBool, out } { }
    private FEventisNetworkReader: TVRMLEvent;
    public property EventisNetworkReader: TVRMLEvent read FEventisNetworkReader;

    { Event: SFBool, out } { }
    private FEventisNetworkWriter: TVRMLEvent;
    public property EventisNetworkWriter: TVRMLEvent read FEventisNetworkWriter;

    { Event: SFBool, out } { }
    private FEventisRtpHeaderHeard: TVRMLEvent;
    public property EventisRtpHeaderHeard: TVRMLEvent read FEventisRtpHeaderHeard;

    { Event: SFBool, out } { }
    private FEventisStandAlone: TVRMLEvent;
    public property EventisStandAlone: TVRMLEvent read FEventisStandAlone;

    { Event: SFTime, out } { }
    private FEventtimestamp: TVRMLEvent;
    public property Eventtimestamp: TVRMLEvent read FEventtimestamp;

    private FFdrtpHeaderExpected: TSFBool;
    public property FdrtpHeaderExpected: TSFBool read FFdrtpHeaderExpected;
  end;

  TNodeReceiverPdu = class(TNodeX3DSensorNode, INodeX3DBoundedObject)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdaddress: TSFString;
    public property Fdaddress: TSFString read FFdaddress;

    private FFdapplicationID: TSFInt32;
    public property FdapplicationID: TSFInt32 read FFdapplicationID;

    private FFdentityID: TSFInt32;
    public property FdentityID: TSFInt32 read FFdentityID;

    private FFdmulticastRelayHost: TSFString;
    public property FdmulticastRelayHost: TSFString read FFdmulticastRelayHost;

    private FFdmulticastRelayPort: TSFInt32;
    public property FdmulticastRelayPort: TSFInt32 read FFdmulticastRelayPort;

    private FFdnetworkMode: TSFString;
    public property FdnetworkMode: TSFString read FFdnetworkMode;

    private FFdport: TSFInt32;
    public property Fdport: TSFInt32 read FFdport;

    private FFdradioID: TSFInt32;
    public property FdradioID: TSFInt32 read FFdradioID;

    private FFdreadInterval: TSFFloat;
    public property FdreadInterval: TSFFloat read FFdreadInterval;

    private FFdreceivedPower: TSFFloat;
    public property FdreceivedPower: TSFFloat read FFdreceivedPower;

    private FFdreceiverState: TSFInt32;
    public property FdreceiverState: TSFInt32 read FFdreceiverState;

    private FFdrtpHeaderExpected: TSFBool;
    public property FdrtpHeaderExpected: TSFBool read FFdrtpHeaderExpected;

    private FFdsiteID: TSFInt32;
    public property FdsiteID: TSFInt32 read FFdsiteID;

    private FFdtransmitterApplicationID: TSFInt32;
    public property FdtransmitterApplicationID: TSFInt32 read FFdtransmitterApplicationID;

    private FFdtransmitterEntityID: TSFInt32;
    public property FdtransmitterEntityID: TSFInt32 read FFdtransmitterEntityID;

    private FFdtransmitterRadioID: TSFInt32;
    public property FdtransmitterRadioID: TSFInt32 read FFdtransmitterRadioID;

    private FFdtransmitterSiteID: TSFInt32;
    public property FdtransmitterSiteID: TSFInt32 read FFdtransmitterSiteID;

    private FFdwhichGeometry: TSFInt32;
    public property FdwhichGeometry: TSFInt32 read FFdwhichGeometry;

    private FFdwriteInterval: TSFFloat;
    public property FdwriteInterval: TSFFloat read FFdwriteInterval;

    { Event: SFBool, out } { }
    private FEventisNetworkReader: TVRMLEvent;
    public property EventisNetworkReader: TVRMLEvent read FEventisNetworkReader;

    { Event: SFBool, out } { }
    private FEventisNetworkWriter: TVRMLEvent;
    public property EventisNetworkWriter: TVRMLEvent read FEventisNetworkWriter;

    { Event: SFBool, out } { }
    private FEventisRtpHeaderHeard: TVRMLEvent;
    public property EventisRtpHeaderHeard: TVRMLEvent read FEventisRtpHeaderHeard;

    { Event: SFBool, out } { }
    private FEventisStandAlone: TVRMLEvent;
    public property EventisStandAlone: TVRMLEvent read FEventisStandAlone;

    { Event: SFTime, out } { }
    private FEventtimestamp: TVRMLEvent;
    public property Eventtimestamp: TVRMLEvent read FEventtimestamp;

    private FFdbboxCenter: TSFVec3f;
    public property FdbboxCenter: TSFVec3f read FFdbboxCenter;

    private FFdbboxSize: TSFVec3f;
    public property FdbboxSize: TSFVec3f read FFdbboxSize;
  end;

  TNodeSignalPdu = class(TNodeX3DSensorNode, INodeX3DBoundedObject)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdaddress: TSFString;
    public property Fdaddress: TSFString read FFdaddress;

    private FFdapplicationID: TSFInt32;
    public property FdapplicationID: TSFInt32 read FFdapplicationID;

    private FFddata: TMFInt32;
    public property Fddata: TMFInt32 read FFddata;

    private FFddataLength: TSFInt32;
    public property FddataLength: TSFInt32 read FFddataLength;

    private FFdencodingScheme: TSFInt32;
    public property FdencodingScheme: TSFInt32 read FFdencodingScheme;

    private FFdentityID: TSFInt32;
    public property FdentityID: TSFInt32 read FFdentityID;

    private FFdmulticastRelayHost: TSFString;
    public property FdmulticastRelayHost: TSFString read FFdmulticastRelayHost;

    private FFdmulticastRelayPort: TSFInt32;
    public property FdmulticastRelayPort: TSFInt32 read FFdmulticastRelayPort;

    private FFdnetworkMode: TSFString;
    public property FdnetworkMode: TSFString read FFdnetworkMode;

    private FFdport: TSFInt32;
    public property Fdport: TSFInt32 read FFdport;

    private FFdradioID: TSFInt32;
    public property FdradioID: TSFInt32 read FFdradioID;

    private FFdreadInterval: TSFFloat;
    public property FdreadInterval: TSFFloat read FFdreadInterval;

    private FFdrtpHeaderExpected: TSFBool;
    public property FdrtpHeaderExpected: TSFBool read FFdrtpHeaderExpected;

    private FFdsampleRate: TSFInt32;
    public property FdsampleRate: TSFInt32 read FFdsampleRate;

    private FFdsamples: TSFInt32;
    public property Fdsamples: TSFInt32 read FFdsamples;

    private FFdsiteID: TSFInt32;
    public property FdsiteID: TSFInt32 read FFdsiteID;

    private FFdtdlType: TSFInt32;
    public property FdtdlType: TSFInt32 read FFdtdlType;

    private FFdwhichGeometry: TSFInt32;
    public property FdwhichGeometry: TSFInt32 read FFdwhichGeometry;

    private FFdwriteInterval: TSFFloat;
    public property FdwriteInterval: TSFFloat read FFdwriteInterval;

    { Event: SFBool, out } { }
    private FEventisNetworkReader: TVRMLEvent;
    public property EventisNetworkReader: TVRMLEvent read FEventisNetworkReader;

    { Event: SFBool, out } { }
    private FEventisNetworkWriter: TVRMLEvent;
    public property EventisNetworkWriter: TVRMLEvent read FEventisNetworkWriter;

    { Event: SFBool, out } { }
    private FEventisRtpHeaderHeard: TVRMLEvent;
    public property EventisRtpHeaderHeard: TVRMLEvent read FEventisRtpHeaderHeard;

    { Event: SFBool, out } { }
    private FEventisStandAlone: TVRMLEvent;
    public property EventisStandAlone: TVRMLEvent read FEventisStandAlone;

    { Event: SFTime, out } { }
    private FEventtimestamp: TVRMLEvent;
    public property Eventtimestamp: TVRMLEvent read FEventtimestamp;

    private FFdbboxCenter: TSFVec3f;
    public property FdbboxCenter: TSFVec3f read FFdbboxCenter;

    private FFdbboxSize: TSFVec3f;
    public property FdbboxSize: TSFVec3f read FFdbboxSize;
  end;

  TNodeTransmitterPdu = class(TNodeX3DSensorNode, INodeX3DBoundedObject)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdaddress: TSFString;
    public property Fdaddress: TSFString read FFdaddress;

    private FFdantennaLocation: TSFVec3f;
    public property FdantennaLocation: TSFVec3f read FFdantennaLocation;

    private FFdantennaPatternLength: TSFInt32;
    public property FdantennaPatternLength: TSFInt32 read FFdantennaPatternLength;

    private FFdantennaPatternType: TSFInt32;
    public property FdantennaPatternType: TSFInt32 read FFdantennaPatternType;

    private FFdapplicationID: TSFInt32;
    public property FdapplicationID: TSFInt32 read FFdapplicationID;

    private FFdcryptoKeyID: TSFInt32;
    public property FdcryptoKeyID: TSFInt32 read FFdcryptoKeyID;

    private FFdcryptoSystem: TSFInt32;
    public property FdcryptoSystem: TSFInt32 read FFdcryptoSystem;

    private FFdentityID: TSFInt32;
    public property FdentityID: TSFInt32 read FFdentityID;

    private FFdfrequency: TSFInt32;
    public property Fdfrequency: TSFInt32 read FFdfrequency;

    private FFdinputSource: TSFInt32;
    public property FdinputSource: TSFInt32 read FFdinputSource;

    private FFdlengthOfModulationParameters: TSFInt32;
    public property FdlengthOfModulationParameters: TSFInt32 read FFdlengthOfModulationParameters;

    private FFdmodulationTypeDetail: TSFInt32;
    public property FdmodulationTypeDetail: TSFInt32 read FFdmodulationTypeDetail;

    private FFdmodulationTypeMajor: TSFInt32;
    public property FdmodulationTypeMajor: TSFInt32 read FFdmodulationTypeMajor;

    private FFdmodulationTypeSpreadSpectrum: TSFInt32;
    public property FdmodulationTypeSpreadSpectrum: TSFInt32 read FFdmodulationTypeSpreadSpectrum;

    private FFdmodulationTypeSystem: TSFInt32;
    public property FdmodulationTypeSystem: TSFInt32 read FFdmodulationTypeSystem;

    private FFdmulticastRelayHost: TSFString;
    public property FdmulticastRelayHost: TSFString read FFdmulticastRelayHost;

    private FFdmulticastRelayPort: TSFInt32;
    public property FdmulticastRelayPort: TSFInt32 read FFdmulticastRelayPort;

    private FFdnetworkMode: TSFString;
    public property FdnetworkMode: TSFString read FFdnetworkMode;

    private FFdport: TSFInt32;
    public property Fdport: TSFInt32 read FFdport;

    private FFdpower: TSFFloat;
    public property Fdpower: TSFFloat read FFdpower;

    private FFdradioEntityTypeCategory: TSFInt32;
    public property FdradioEntityTypeCategory: TSFInt32 read FFdradioEntityTypeCategory;

    private FFdradioEntityTypeCountry: TSFInt32;
    public property FdradioEntityTypeCountry: TSFInt32 read FFdradioEntityTypeCountry;

    private FFdradioEntityTypeDomain: TSFInt32;
    public property FdradioEntityTypeDomain: TSFInt32 read FFdradioEntityTypeDomain;

    private FFdradioEntityTypeKind: TSFInt32;
    public property FdradioEntityTypeKind: TSFInt32 read FFdradioEntityTypeKind;

    private FFdradioEntityTypeNomenclature: TSFInt32;
    public property FdradioEntityTypeNomenclature: TSFInt32 read FFdradioEntityTypeNomenclature;

    private FFdradioEntityTypeNomenclatureVersion: TSFInt32;
    public property FdradioEntityTypeNomenclatureVersion: TSFInt32 read FFdradioEntityTypeNomenclatureVersion;

    private FFdradioID: TSFInt32;
    public property FdradioID: TSFInt32 read FFdradioID;

    private FFdreadInterval: TSFFloat;
    public property FdreadInterval: TSFFloat read FFdreadInterval;

    private FFdrelativeAntennaLocation: TSFVec3f;
    public property FdrelativeAntennaLocation: TSFVec3f read FFdrelativeAntennaLocation;

    private FFdrtpHeaderExpected: TSFBool;
    public property FdrtpHeaderExpected: TSFBool read FFdrtpHeaderExpected;

    private FFdsiteID: TSFInt32;
    public property FdsiteID: TSFInt32 read FFdsiteID;

    private FFdtransmitFrequencyBandwidth: TSFFloat;
    public property FdtransmitFrequencyBandwidth: TSFFloat read FFdtransmitFrequencyBandwidth;

    private FFdtransmitState: TSFInt32;
    public property FdtransmitState: TSFInt32 read FFdtransmitState;

    private FFdwhichGeometry: TSFInt32;
    public property FdwhichGeometry: TSFInt32 read FFdwhichGeometry;

    private FFdwriteInterval: TSFFloat;
    public property FdwriteInterval: TSFFloat read FFdwriteInterval;

    { Event: SFBool, out } { }
    private FEventisNetworkReader: TVRMLEvent;
    public property EventisNetworkReader: TVRMLEvent read FEventisNetworkReader;

    { Event: SFBool, out } { }
    private FEventisNetworkWriter: TVRMLEvent;
    public property EventisNetworkWriter: TVRMLEvent read FEventisNetworkWriter;

    { Event: SFBool, out } { }
    private FEventisRtpHeaderHeard: TVRMLEvent;
    public property EventisRtpHeaderHeard: TVRMLEvent read FEventisRtpHeaderHeard;

    { Event: SFBool, out } { }
    private FEventisStandAlone: TVRMLEvent;
    public property EventisStandAlone: TVRMLEvent read FEventisStandAlone;

    { Event: SFTime, out } { }
    private FEventtimestamp: TVRMLEvent;
    public property Eventtimestamp: TVRMLEvent read FEventtimestamp;

    private FFdbboxCenter: TSFVec3f;
    public property FdbboxCenter: TSFVec3f read FFdbboxCenter;

    private FFdbboxSize: TSFVec3f;
    public property FdbboxSize: TSFVec3f read FFdbboxSize;
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

function TNodeDISEntityManager.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeDISEntityTypeMapping.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeEspduTransform.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeReceiverPdu.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeSignalPdu.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeTransmitterPdu.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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
