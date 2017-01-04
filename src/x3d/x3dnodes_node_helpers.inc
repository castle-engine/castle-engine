{ -*- buffer-read-only: t -*-

  Copyright 2015-2016 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Automatically generated X3D node class helpers.
  Do not edit manually, instead regenerate using
  castle_game_engine/src/x3d/doc/generate_x3d_nodes_to_pascal.lpr . }

{$ifdef read_interface}

type
  TMatrixTransformNodeHelper = class helper for TMatrixTransformNode
  private
    function GetMatrix: TMatrix4Single;
    procedure SetMatrix(const Value: TMatrix4Single);
  public
    property Matrix: TMatrix4Single read GetMatrix write SetMatrix;
  end;

  TAbstractProductStructureChildNodeHelper = class helper for TAbstractProductStructureChildNode
  private
    function GetNameField: string;
    procedure SetNameField(const Value: string);
  public
    property NameField: string read GetNameField write SetNameField;
  end;

  TCADAssemblyNodeHelper = class helper for TCADAssemblyNode
  private
    function GetNameField: string;
    procedure SetNameField(const Value: string);
  public
    property NameField: string read GetNameField write SetNameField;
  end;

  TCADFaceNodeHelper = class helper for TCADFaceNode
  private
    function GetBboxCenter: TVector3Single;
    procedure SetBboxCenter(const Value: TVector3Single);
    function GetBboxSize: TVector3Single;
    procedure SetBboxSize(const Value: TVector3Single);
  public
    property BboxCenter: TVector3Single read GetBboxCenter write SetBboxCenter;
    property BboxSize: TVector3Single read GetBboxSize write SetBboxSize;
  end;

  TCADLayerNodeHelper = class helper for TCADLayerNode
  private
    function GetNameField: string;
    procedure SetNameField(const Value: string);
  public
    property NameField: string read GetNameField write SetNameField;
  end;

  TCADPartNodeHelper = class helper for TCADPartNode
  private
    function GetCenter: TVector3Single;
    procedure SetCenter(const Value: TVector3Single);
    function GetNameField: string;
    procedure SetNameField(const Value: string);
    function GetRotation: TVector4Single;
    procedure SetRotation(const Value: TVector4Single);
    function GetScale: TVector3Single;
    procedure SetScale(const Value: TVector3Single);
    function GetScaleOrientation: TVector4Single;
    procedure SetScaleOrientation(const Value: TVector4Single);
    function GetTranslation: TVector3Single;
    procedure SetTranslation(const Value: TVector3Single);
  public
    property Center: TVector3Single read GetCenter write SetCenter;
    property NameField: string read GetNameField write SetNameField;
    property Rotation: TVector4Single read GetRotation write SetRotation;
    property Scale: TVector3Single read GetScale write SetScale;
    property ScaleOrientation: TVector4Single read GetScaleOrientation write SetScaleOrientation;
    property Translation: TVector3Single read GetTranslation write SetTranslation;
  end;

  TAbstractSensorNodeHelper = class helper for TAbstractSensorNode
  private
    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
  public
    property Enabled: boolean read GetEnabled write SetEnabled;
  end;

  TMetadataDoubleNodeHelper = class helper for TMetadataDoubleNode
  private
    function GetNameField: string;
    procedure SetNameField(const Value: string);
    function GetReference: string;
    procedure SetReference(const Value: string);
  public
    property NameField: string read GetNameField write SetNameField;
    property Reference: string read GetReference write SetReference;
  end;

  TMetadataFloatNodeHelper = class helper for TMetadataFloatNode
  private
    function GetNameField: string;
    procedure SetNameField(const Value: string);
    function GetReference: string;
    procedure SetReference(const Value: string);
  public
    property NameField: string read GetNameField write SetNameField;
    property Reference: string read GetReference write SetReference;
  end;

  TMetadataIntegerNodeHelper = class helper for TMetadataIntegerNode
  private
    function GetNameField: string;
    procedure SetNameField(const Value: string);
    function GetReference: string;
    procedure SetReference(const Value: string);
  public
    property NameField: string read GetNameField write SetNameField;
    property Reference: string read GetReference write SetReference;
  end;

  TMetadataSetNodeHelper = class helper for TMetadataSetNode
  private
    function GetNameField: string;
    procedure SetNameField(const Value: string);
    function GetReference: string;
    procedure SetReference(const Value: string);
  public
    property NameField: string read GetNameField write SetNameField;
    property Reference: string read GetReference write SetReference;
  end;

  TMetadataStringNodeHelper = class helper for TMetadataStringNode
  private
    function GetNameField: string;
    procedure SetNameField(const Value: string);
    function GetReference: string;
    procedure SetReference(const Value: string);
  public
    property NameField: string read GetNameField write SetNameField;
    property Reference: string read GetReference write SetReference;
  end;

  TWorldInfoNodeHelper = class helper for TWorldInfoNode
  private
    function GetTitle: string;
    procedure SetTitle(const Value: string);
  public
    property Title: string read GetTitle write SetTitle;
  end;

  TGeneratedCubeMapTextureNodeHelper = class helper for TGeneratedCubeMapTextureNode
  private
    function GetSize: Integer;
    procedure SetSize(const Value: Integer);
  public
    property Size: Integer read GetSize write SetSize;
  end;

  TDISEntityManagerNodeHelper = class helper for TDISEntityManagerNode
  private
    function GetAddress: string;
    procedure SetAddress(const Value: string);
    function GetApplicationID: Integer;
    procedure SetApplicationID(const Value: Integer);
    function GetPort: Integer;
    procedure SetPort(const Value: Integer);
    function GetSiteID: Integer;
    procedure SetSiteID(const Value: Integer);
  public
    property Address: string read GetAddress write SetAddress;
    property ApplicationID: Integer read GetApplicationID write SetApplicationID;
    property Port: Integer read GetPort write SetPort;
    property SiteID: Integer read GetSiteID write SetSiteID;
  end;

  TDISEntityTypeMappingNodeHelper = class helper for TDISEntityTypeMappingNode
  private
    function GetCategory: Integer;
    procedure SetCategory(const Value: Integer);
    function GetCountry: Integer;
    procedure SetCountry(const Value: Integer);
    function GetDomain: Integer;
    procedure SetDomain(const Value: Integer);
    function GetExtra: Integer;
    procedure SetExtra(const Value: Integer);
    function GetKind: Integer;
    procedure SetKind(const Value: Integer);
    function GetSpecific: Integer;
    procedure SetSpecific(const Value: Integer);
    function GetSubcategory: Integer;
    procedure SetSubcategory(const Value: Integer);
  public
    property Category: Integer read GetCategory write SetCategory;
    property Country: Integer read GetCountry write SetCountry;
    property Domain: Integer read GetDomain write SetDomain;
    property Extra: Integer read GetExtra write SetExtra;
    property Kind: Integer read GetKind write SetKind;
    property Specific: Integer read GetSpecific write SetSpecific;
    property Subcategory: Integer read GetSubcategory write SetSubcategory;
  end;

  TEspduTransformNodeHelper = class helper for TEspduTransformNode
  private
    function GetAddress: string;
    procedure SetAddress(const Value: string);
    function GetApplicationID: Integer;
    procedure SetApplicationID(const Value: Integer);
    function GetArticulationParameterCount: Integer;
    procedure SetArticulationParameterCount(const Value: Integer);
    function GetCenter: TVector3Single;
    procedure SetCenter(const Value: TVector3Single);
    function GetCollisionType: Integer;
    procedure SetCollisionType(const Value: Integer);
    function GetDeadReckoning: Integer;
    procedure SetDeadReckoning(const Value: Integer);
    function GetDetonationLocation: TVector3Single;
    procedure SetDetonationLocation(const Value: TVector3Single);
    function GetDetonationRelativeLocation: TVector3Single;
    procedure SetDetonationRelativeLocation(const Value: TVector3Single);
    function GetDetonationResult: Integer;
    procedure SetDetonationResult(const Value: Integer);
    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
    function GetEntityCategory: Integer;
    procedure SetEntityCategory(const Value: Integer);
    function GetEntityCountry: Integer;
    procedure SetEntityCountry(const Value: Integer);
    function GetEntityDomain: Integer;
    procedure SetEntityDomain(const Value: Integer);
    function GetEntityExtra: Integer;
    procedure SetEntityExtra(const Value: Integer);
    function GetEntityID: Integer;
    procedure SetEntityID(const Value: Integer);
    function GetEntityKind: Integer;
    procedure SetEntityKind(const Value: Integer);
    function GetEntitySpecific: Integer;
    procedure SetEntitySpecific(const Value: Integer);
    function GetEntitySubCategory: Integer;
    procedure SetEntitySubCategory(const Value: Integer);
    function GetEventApplicationID: Integer;
    procedure SetEventApplicationID(const Value: Integer);
    function GetEventEntityID: Integer;
    procedure SetEventEntityID(const Value: Integer);
    function GetEventNumber: Integer;
    procedure SetEventNumber(const Value: Integer);
    function GetEventSiteID: Integer;
    procedure SetEventSiteID(const Value: Integer);
    function GetFired1: boolean;
    procedure SetFired1(const Value: boolean);
    function GetFired2: boolean;
    procedure SetFired2(const Value: boolean);
    function GetFireMissionIndex: Integer;
    procedure SetFireMissionIndex(const Value: Integer);
    function GetFiringRange: Single;
    procedure SetFiringRange(const Value: Single);
    function GetFiringRate: Integer;
    procedure SetFiringRate(const Value: Integer);
    function GetForceID: Integer;
    procedure SetForceID(const Value: Integer);
    function GetFuse: Integer;
    procedure SetFuse(const Value: Integer);
    function GetLinearVelocity: TVector3Single;
    procedure SetLinearVelocity(const Value: TVector3Single);
    function GetLinearAcceleration: TVector3Single;
    procedure SetLinearAcceleration(const Value: TVector3Single);
    function GetMarking: string;
    procedure SetMarking(const Value: string);
    function GetMulticastRelayHost: string;
    procedure SetMulticastRelayHost(const Value: string);
    function GetMulticastRelayPort: Integer;
    procedure SetMulticastRelayPort(const Value: Integer);
    function GetMunitionApplicationID: Integer;
    procedure SetMunitionApplicationID(const Value: Integer);
    function GetMunitionEndPoint: TVector3Single;
    procedure SetMunitionEndPoint(const Value: TVector3Single);
    function GetMunitionEntityID: Integer;
    procedure SetMunitionEntityID(const Value: Integer);
    function GetMunitionQuantity: Integer;
    procedure SetMunitionQuantity(const Value: Integer);
    function GetMunitionSiteID: Integer;
    procedure SetMunitionSiteID(const Value: Integer);
    function GetMunitionStartPoint: TVector3Single;
    procedure SetMunitionStartPoint(const Value: TVector3Single);
    function GetPort: Integer;
    procedure SetPort(const Value: Integer);
    function GetReadInterval: TFloatTime;
    procedure SetReadInterval(const Value: TFloatTime);
    function GetRotation: TVector4Single;
    procedure SetRotation(const Value: TVector4Single);
    function GetScale: TVector3Single;
    procedure SetScale(const Value: TVector3Single);
    function GetScaleOrientation: TVector4Single;
    procedure SetScaleOrientation(const Value: TVector4Single);
    function GetSiteID: Integer;
    procedure SetSiteID(const Value: Integer);
    function GetTranslation: TVector3Single;
    procedure SetTranslation(const Value: TVector3Single);
    function GetWarhead: Integer;
    procedure SetWarhead(const Value: Integer);
    function GetWriteInterval: TFloatTime;
    procedure SetWriteInterval(const Value: TFloatTime);
    function GetRtpHeaderExpected: boolean;
    procedure SetRtpHeaderExpected(const Value: boolean);
  public
    property Address: string read GetAddress write SetAddress;
    property ApplicationID: Integer read GetApplicationID write SetApplicationID;
    property ArticulationParameterCount: Integer read GetArticulationParameterCount write SetArticulationParameterCount;
    property Center: TVector3Single read GetCenter write SetCenter;
    property CollisionType: Integer read GetCollisionType write SetCollisionType;
    property DeadReckoning: Integer read GetDeadReckoning write SetDeadReckoning;
    property DetonationLocation: TVector3Single read GetDetonationLocation write SetDetonationLocation;
    property DetonationRelativeLocation: TVector3Single read GetDetonationRelativeLocation write SetDetonationRelativeLocation;
    property DetonationResult: Integer read GetDetonationResult write SetDetonationResult;
    property Enabled: boolean read GetEnabled write SetEnabled;
    property EntityCategory: Integer read GetEntityCategory write SetEntityCategory;
    property EntityCountry: Integer read GetEntityCountry write SetEntityCountry;
    property EntityDomain: Integer read GetEntityDomain write SetEntityDomain;
    property EntityExtra: Integer read GetEntityExtra write SetEntityExtra;
    property EntityID: Integer read GetEntityID write SetEntityID;
    property EntityKind: Integer read GetEntityKind write SetEntityKind;
    property EntitySpecific: Integer read GetEntitySpecific write SetEntitySpecific;
    property EntitySubCategory: Integer read GetEntitySubCategory write SetEntitySubCategory;
    property EventApplicationID: Integer read GetEventApplicationID write SetEventApplicationID;
    property EventEntityID: Integer read GetEventEntityID write SetEventEntityID;
    property EventNumber: Integer read GetEventNumber write SetEventNumber;
    property EventSiteID: Integer read GetEventSiteID write SetEventSiteID;
    property Fired1: boolean read GetFired1 write SetFired1;
    property Fired2: boolean read GetFired2 write SetFired2;
    property FireMissionIndex: Integer read GetFireMissionIndex write SetFireMissionIndex;
    property FiringRange: Single read GetFiringRange write SetFiringRange;
    property FiringRate: Integer read GetFiringRate write SetFiringRate;
    property ForceID: Integer read GetForceID write SetForceID;
    property Fuse: Integer read GetFuse write SetFuse;
    property LinearVelocity: TVector3Single read GetLinearVelocity write SetLinearVelocity;
    property LinearAcceleration: TVector3Single read GetLinearAcceleration write SetLinearAcceleration;
    property Marking: string read GetMarking write SetMarking;
    property MulticastRelayHost: string read GetMulticastRelayHost write SetMulticastRelayHost;
    property MulticastRelayPort: Integer read GetMulticastRelayPort write SetMulticastRelayPort;
    property MunitionApplicationID: Integer read GetMunitionApplicationID write SetMunitionApplicationID;
    property MunitionEndPoint: TVector3Single read GetMunitionEndPoint write SetMunitionEndPoint;
    property MunitionEntityID: Integer read GetMunitionEntityID write SetMunitionEntityID;
    property MunitionQuantity: Integer read GetMunitionQuantity write SetMunitionQuantity;
    property MunitionSiteID: Integer read GetMunitionSiteID write SetMunitionSiteID;
    property MunitionStartPoint: TVector3Single read GetMunitionStartPoint write SetMunitionStartPoint;
    property Port: Integer read GetPort write SetPort;
    property ReadInterval: TFloatTime read GetReadInterval write SetReadInterval;
    property Rotation: TVector4Single read GetRotation write SetRotation;
    property Scale: TVector3Single read GetScale write SetScale;
    property ScaleOrientation: TVector4Single read GetScaleOrientation write SetScaleOrientation;
    property SiteID: Integer read GetSiteID write SetSiteID;
    property Translation: TVector3Single read GetTranslation write SetTranslation;
    property Warhead: Integer read GetWarhead write SetWarhead;
    property WriteInterval: TFloatTime read GetWriteInterval write SetWriteInterval;
    property RtpHeaderExpected: boolean read GetRtpHeaderExpected write SetRtpHeaderExpected;
  end;

  TReceiverPduNodeHelper = class helper for TReceiverPduNode
  private
    function GetAddress: string;
    procedure SetAddress(const Value: string);
    function GetApplicationID: Integer;
    procedure SetApplicationID(const Value: Integer);
    function GetEntityID: Integer;
    procedure SetEntityID(const Value: Integer);
    function GetMulticastRelayHost: string;
    procedure SetMulticastRelayHost(const Value: string);
    function GetMulticastRelayPort: Integer;
    procedure SetMulticastRelayPort(const Value: Integer);
    function GetPort: Integer;
    procedure SetPort(const Value: Integer);
    function GetRadioID: Integer;
    procedure SetRadioID(const Value: Integer);
    function GetReadInterval: Single;
    procedure SetReadInterval(const Value: Single);
    function GetReceivedPower: Single;
    procedure SetReceivedPower(const Value: Single);
    function GetReceiverState: Integer;
    procedure SetReceiverState(const Value: Integer);
    function GetRtpHeaderExpected: boolean;
    procedure SetRtpHeaderExpected(const Value: boolean);
    function GetSiteID: Integer;
    procedure SetSiteID(const Value: Integer);
    function GetTransmitterApplicationID: Integer;
    procedure SetTransmitterApplicationID(const Value: Integer);
    function GetTransmitterEntityID: Integer;
    procedure SetTransmitterEntityID(const Value: Integer);
    function GetTransmitterRadioID: Integer;
    procedure SetTransmitterRadioID(const Value: Integer);
    function GetTransmitterSiteID: Integer;
    procedure SetTransmitterSiteID(const Value: Integer);
    function GetWhichGeometry: Integer;
    procedure SetWhichGeometry(const Value: Integer);
    function GetWriteInterval: Single;
    procedure SetWriteInterval(const Value: Single);
    function GetBboxCenter: TVector3Single;
    procedure SetBboxCenter(const Value: TVector3Single);
    function GetBboxSize: TVector3Single;
    procedure SetBboxSize(const Value: TVector3Single);
  public
    property Address: string read GetAddress write SetAddress;
    property ApplicationID: Integer read GetApplicationID write SetApplicationID;
    property EntityID: Integer read GetEntityID write SetEntityID;
    property MulticastRelayHost: string read GetMulticastRelayHost write SetMulticastRelayHost;
    property MulticastRelayPort: Integer read GetMulticastRelayPort write SetMulticastRelayPort;
    property Port: Integer read GetPort write SetPort;
    property RadioID: Integer read GetRadioID write SetRadioID;
    property ReadInterval: Single read GetReadInterval write SetReadInterval;
    property ReceivedPower: Single read GetReceivedPower write SetReceivedPower;
    property ReceiverState: Integer read GetReceiverState write SetReceiverState;
    property RtpHeaderExpected: boolean read GetRtpHeaderExpected write SetRtpHeaderExpected;
    property SiteID: Integer read GetSiteID write SetSiteID;
    property TransmitterApplicationID: Integer read GetTransmitterApplicationID write SetTransmitterApplicationID;
    property TransmitterEntityID: Integer read GetTransmitterEntityID write SetTransmitterEntityID;
    property TransmitterRadioID: Integer read GetTransmitterRadioID write SetTransmitterRadioID;
    property TransmitterSiteID: Integer read GetTransmitterSiteID write SetTransmitterSiteID;
    property WhichGeometry: Integer read GetWhichGeometry write SetWhichGeometry;
    property WriteInterval: Single read GetWriteInterval write SetWriteInterval;
    property BboxCenter: TVector3Single read GetBboxCenter write SetBboxCenter;
    property BboxSize: TVector3Single read GetBboxSize write SetBboxSize;
  end;

  TSignalPduNodeHelper = class helper for TSignalPduNode
  private
    function GetAddress: string;
    procedure SetAddress(const Value: string);
    function GetApplicationID: Integer;
    procedure SetApplicationID(const Value: Integer);
    function GetDataLength: Integer;
    procedure SetDataLength(const Value: Integer);
    function GetEncodingScheme: Integer;
    procedure SetEncodingScheme(const Value: Integer);
    function GetEntityID: Integer;
    procedure SetEntityID(const Value: Integer);
    function GetMulticastRelayHost: string;
    procedure SetMulticastRelayHost(const Value: string);
    function GetMulticastRelayPort: Integer;
    procedure SetMulticastRelayPort(const Value: Integer);
    function GetPort: Integer;
    procedure SetPort(const Value: Integer);
    function GetRadioID: Integer;
    procedure SetRadioID(const Value: Integer);
    function GetReadInterval: Single;
    procedure SetReadInterval(const Value: Single);
    function GetRtpHeaderExpected: boolean;
    procedure SetRtpHeaderExpected(const Value: boolean);
    function GetSampleRate: Integer;
    procedure SetSampleRate(const Value: Integer);
    function GetSamples: Integer;
    procedure SetSamples(const Value: Integer);
    function GetSiteID: Integer;
    procedure SetSiteID(const Value: Integer);
    function GetTdlType: Integer;
    procedure SetTdlType(const Value: Integer);
    function GetWhichGeometry: Integer;
    procedure SetWhichGeometry(const Value: Integer);
    function GetWriteInterval: Single;
    procedure SetWriteInterval(const Value: Single);
    function GetBboxCenter: TVector3Single;
    procedure SetBboxCenter(const Value: TVector3Single);
    function GetBboxSize: TVector3Single;
    procedure SetBboxSize(const Value: TVector3Single);
  public
    property Address: string read GetAddress write SetAddress;
    property ApplicationID: Integer read GetApplicationID write SetApplicationID;
    property DataLength: Integer read GetDataLength write SetDataLength;
    property EncodingScheme: Integer read GetEncodingScheme write SetEncodingScheme;
    property EntityID: Integer read GetEntityID write SetEntityID;
    property MulticastRelayHost: string read GetMulticastRelayHost write SetMulticastRelayHost;
    property MulticastRelayPort: Integer read GetMulticastRelayPort write SetMulticastRelayPort;
    property Port: Integer read GetPort write SetPort;
    property RadioID: Integer read GetRadioID write SetRadioID;
    property ReadInterval: Single read GetReadInterval write SetReadInterval;
    property RtpHeaderExpected: boolean read GetRtpHeaderExpected write SetRtpHeaderExpected;
    property SampleRate: Integer read GetSampleRate write SetSampleRate;
    property Samples: Integer read GetSamples write SetSamples;
    property SiteID: Integer read GetSiteID write SetSiteID;
    property TdlType: Integer read GetTdlType write SetTdlType;
    property WhichGeometry: Integer read GetWhichGeometry write SetWhichGeometry;
    property WriteInterval: Single read GetWriteInterval write SetWriteInterval;
    property BboxCenter: TVector3Single read GetBboxCenter write SetBboxCenter;
    property BboxSize: TVector3Single read GetBboxSize write SetBboxSize;
  end;

  TTransmitterPduNodeHelper = class helper for TTransmitterPduNode
  private
    function GetAddress: string;
    procedure SetAddress(const Value: string);
    function GetAntennaLocation: TVector3Single;
    procedure SetAntennaLocation(const Value: TVector3Single);
    function GetAntennaPatternLength: Integer;
    procedure SetAntennaPatternLength(const Value: Integer);
    function GetAntennaPatternType: Integer;
    procedure SetAntennaPatternType(const Value: Integer);
    function GetApplicationID: Integer;
    procedure SetApplicationID(const Value: Integer);
    function GetCryptoKeyID: Integer;
    procedure SetCryptoKeyID(const Value: Integer);
    function GetCryptoSystem: Integer;
    procedure SetCryptoSystem(const Value: Integer);
    function GetEntityID: Integer;
    procedure SetEntityID(const Value: Integer);
    function GetFrequency: Integer;
    procedure SetFrequency(const Value: Integer);
    function GetInputSource: Integer;
    procedure SetInputSource(const Value: Integer);
    function GetLengthOfModulationParameters: Integer;
    procedure SetLengthOfModulationParameters(const Value: Integer);
    function GetModulationTypeDetail: Integer;
    procedure SetModulationTypeDetail(const Value: Integer);
    function GetModulationTypeMajor: Integer;
    procedure SetModulationTypeMajor(const Value: Integer);
    function GetModulationTypeSpreadSpectrum: Integer;
    procedure SetModulationTypeSpreadSpectrum(const Value: Integer);
    function GetModulationTypeSystem: Integer;
    procedure SetModulationTypeSystem(const Value: Integer);
    function GetMulticastRelayHost: string;
    procedure SetMulticastRelayHost(const Value: string);
    function GetMulticastRelayPort: Integer;
    procedure SetMulticastRelayPort(const Value: Integer);
    function GetPort: Integer;
    procedure SetPort(const Value: Integer);
    function GetPower: Single;
    procedure SetPower(const Value: Single);
    function GetRadioEntityTypeCategory: Integer;
    procedure SetRadioEntityTypeCategory(const Value: Integer);
    function GetRadioEntityTypeCountry: Integer;
    procedure SetRadioEntityTypeCountry(const Value: Integer);
    function GetRadioEntityTypeDomain: Integer;
    procedure SetRadioEntityTypeDomain(const Value: Integer);
    function GetRadioEntityTypeKind: Integer;
    procedure SetRadioEntityTypeKind(const Value: Integer);
    function GetRadioEntityTypeNomenclature: Integer;
    procedure SetRadioEntityTypeNomenclature(const Value: Integer);
    function GetRadioEntityTypeNomenclatureVersion: Integer;
    procedure SetRadioEntityTypeNomenclatureVersion(const Value: Integer);
    function GetRadioID: Integer;
    procedure SetRadioID(const Value: Integer);
    function GetReadInterval: Single;
    procedure SetReadInterval(const Value: Single);
    function GetRelativeAntennaLocation: TVector3Single;
    procedure SetRelativeAntennaLocation(const Value: TVector3Single);
    function GetRtpHeaderExpected: boolean;
    procedure SetRtpHeaderExpected(const Value: boolean);
    function GetSiteID: Integer;
    procedure SetSiteID(const Value: Integer);
    function GetTransmitFrequencyBandwidth: Single;
    procedure SetTransmitFrequencyBandwidth(const Value: Single);
    function GetTransmitState: Integer;
    procedure SetTransmitState(const Value: Integer);
    function GetWhichGeometry: Integer;
    procedure SetWhichGeometry(const Value: Integer);
    function GetWriteInterval: Single;
    procedure SetWriteInterval(const Value: Single);
    function GetBboxCenter: TVector3Single;
    procedure SetBboxCenter(const Value: TVector3Single);
    function GetBboxSize: TVector3Single;
    procedure SetBboxSize(const Value: TVector3Single);
  public
    property Address: string read GetAddress write SetAddress;
    property AntennaLocation: TVector3Single read GetAntennaLocation write SetAntennaLocation;
    property AntennaPatternLength: Integer read GetAntennaPatternLength write SetAntennaPatternLength;
    property AntennaPatternType: Integer read GetAntennaPatternType write SetAntennaPatternType;
    property ApplicationID: Integer read GetApplicationID write SetApplicationID;
    property CryptoKeyID: Integer read GetCryptoKeyID write SetCryptoKeyID;
    property CryptoSystem: Integer read GetCryptoSystem write SetCryptoSystem;
    property EntityID: Integer read GetEntityID write SetEntityID;
    property Frequency: Integer read GetFrequency write SetFrequency;
    property InputSource: Integer read GetInputSource write SetInputSource;
    property LengthOfModulationParameters: Integer read GetLengthOfModulationParameters write SetLengthOfModulationParameters;
    property ModulationTypeDetail: Integer read GetModulationTypeDetail write SetModulationTypeDetail;
    property ModulationTypeMajor: Integer read GetModulationTypeMajor write SetModulationTypeMajor;
    property ModulationTypeSpreadSpectrum: Integer read GetModulationTypeSpreadSpectrum write SetModulationTypeSpreadSpectrum;
    property ModulationTypeSystem: Integer read GetModulationTypeSystem write SetModulationTypeSystem;
    property MulticastRelayHost: string read GetMulticastRelayHost write SetMulticastRelayHost;
    property MulticastRelayPort: Integer read GetMulticastRelayPort write SetMulticastRelayPort;
    property Port: Integer read GetPort write SetPort;
    property Power: Single read GetPower write SetPower;
    property RadioEntityTypeCategory: Integer read GetRadioEntityTypeCategory write SetRadioEntityTypeCategory;
    property RadioEntityTypeCountry: Integer read GetRadioEntityTypeCountry write SetRadioEntityTypeCountry;
    property RadioEntityTypeDomain: Integer read GetRadioEntityTypeDomain write SetRadioEntityTypeDomain;
    property RadioEntityTypeKind: Integer read GetRadioEntityTypeKind write SetRadioEntityTypeKind;
    property RadioEntityTypeNomenclature: Integer read GetRadioEntityTypeNomenclature write SetRadioEntityTypeNomenclature;
    property RadioEntityTypeNomenclatureVersion: Integer read GetRadioEntityTypeNomenclatureVersion write SetRadioEntityTypeNomenclatureVersion;
    property RadioID: Integer read GetRadioID write SetRadioID;
    property ReadInterval: Single read GetReadInterval write SetReadInterval;
    property RelativeAntennaLocation: TVector3Single read GetRelativeAntennaLocation write SetRelativeAntennaLocation;
    property RtpHeaderExpected: boolean read GetRtpHeaderExpected write SetRtpHeaderExpected;
    property SiteID: Integer read GetSiteID write SetSiteID;
    property TransmitFrequencyBandwidth: Single read GetTransmitFrequencyBandwidth write SetTransmitFrequencyBandwidth;
    property TransmitState: Integer read GetTransmitState write SetTransmitState;
    property WhichGeometry: Integer read GetWhichGeometry write SetWhichGeometry;
    property WriteInterval: Single read GetWriteInterval write SetWriteInterval;
    property BboxCenter: TVector3Single read GetBboxCenter write SetBboxCenter;
    property BboxSize: TVector3Single read GetBboxSize write SetBboxSize;
  end;

  TAbstractBackgroundNodeHelper = class helper for TAbstractBackgroundNode
  private
    function GetTransparency: Single;
    procedure SetTransparency(const Value: Single);
  public
    property Transparency: Single read GetTransparency write SetTransparency;
  end;

  TFogNodeHelper = class helper for TFogNode
  private
    function GetColor: TCastleColorRGB;
    procedure SetColor(const Value: TCastleColorRGB);
    function GetVisibilityRange: Single;
    procedure SetVisibilityRange(const Value: Single);
  public
    property Color: TCastleColorRGB read GetColor write SetColor;
    property VisibilityRange: Single read GetVisibilityRange write SetVisibilityRange;
  end;

  TLocalFogNodeHelper = class helper for TLocalFogNode
  private
    function GetColor: TCastleColorRGB;
    procedure SetColor(const Value: TCastleColorRGB);
    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
    function GetVisibilityRange: Single;
    procedure SetVisibilityRange(const Value: Single);
  public
    property Color: TCastleColorRGB read GetColor write SetColor;
    property Enabled: boolean read GetEnabled write SetEnabled;
    property VisibilityRange: Single read GetVisibilityRange write SetVisibilityRange;
  end;

  TAbstractEnvironmentalSensorNodeHelper = class helper for TAbstractEnvironmentalSensorNode
  private
    function GetCenter: TVector3Single;
    procedure SetCenter(const Value: TVector3Single);
    function GetSize: TVector3Single;
    procedure SetSize(const Value: TVector3Single);
  public
    property Center: TVector3Single read GetCenter write SetCenter;
    property Size: TVector3Single read GetSize write SetSize;
  end;

  TBooleanToggleNodeHelper = class helper for TBooleanToggleNode
  private
    function GetToggle: boolean;
    procedure SetToggle(const Value: boolean);
  public
    property Toggle: boolean read GetToggle write SetToggle;
  end;

  TIntegerTriggerNodeHelper = class helper for TIntegerTriggerNode
  private
    function GetIntegerKey: Integer;
    procedure SetIntegerKey(const Value: Integer);
  public
    property IntegerKey: Integer read GetIntegerKey write SetIntegerKey;
  end;

  TAbstractChaserNodeHelper = class helper for TAbstractChaserNode
  private
    function GetDuration: TFloatTime;
    procedure SetDuration(const Value: TFloatTime);
  public
    property Duration: TFloatTime read GetDuration write SetDuration;
  end;

  TAbstractDamperNodeHelper = class helper for TAbstractDamperNode
  private
    function GetTau: TFloatTime;
    procedure SetTau(const Value: TFloatTime);
    function GetTolerance: Single;
    procedure SetTolerance(const Value: Single);
    function GetOrder: Integer;
    procedure SetOrder(const Value: Integer);
  public
    property Tau: TFloatTime read GetTau write SetTau;
    property Tolerance: Single read GetTolerance write SetTolerance;
    property Order: Integer read GetOrder write SetOrder;
  end;

  TColorDamperNodeHelper = class helper for TColorDamperNode
  private
    function GetInitialDestination: TCastleColorRGB;
    procedure SetInitialDestination(const Value: TCastleColorRGB);
    function GetInitialValue: TCastleColorRGB;
    procedure SetInitialValue(const Value: TCastleColorRGB);
  public
    property InitialDestination: TCastleColorRGB read GetInitialDestination write SetInitialDestination;
    property InitialValue: TCastleColorRGB read GetInitialValue write SetInitialValue;
  end;

  TOrientationChaserNodeHelper = class helper for TOrientationChaserNode
  private
    function GetInitialDestination: TVector4Single;
    procedure SetInitialDestination(const Value: TVector4Single);
    function GetInitialValue: TVector4Single;
    procedure SetInitialValue(const Value: TVector4Single);
  public
    property InitialDestination: TVector4Single read GetInitialDestination write SetInitialDestination;
    property InitialValue: TVector4Single read GetInitialValue write SetInitialValue;
  end;

  TOrientationDamperNodeHelper = class helper for TOrientationDamperNode
  private
    function GetInitialDestination: TVector4Single;
    procedure SetInitialDestination(const Value: TVector4Single);
    function GetInitialValue: TVector4Single;
    procedure SetInitialValue(const Value: TVector4Single);
  public
    property InitialDestination: TVector4Single read GetInitialDestination write SetInitialDestination;
    property InitialValue: TVector4Single read GetInitialValue write SetInitialValue;
  end;

  TPositionChaserNodeHelper = class helper for TPositionChaserNode
  private
    function GetInitialDestination: TVector3Single;
    procedure SetInitialDestination(const Value: TVector3Single);
    function GetInitialValue: TVector3Single;
    procedure SetInitialValue(const Value: TVector3Single);
  public
    property InitialDestination: TVector3Single read GetInitialDestination write SetInitialDestination;
    property InitialValue: TVector3Single read GetInitialValue write SetInitialValue;
  end;

  TPositionChaser2DNodeHelper = class helper for TPositionChaser2DNode
  private
    function GetInitialDestination: TVector2Single;
    procedure SetInitialDestination(const Value: TVector2Single);
    function GetInitialValue: TVector2Single;
    procedure SetInitialValue(const Value: TVector2Single);
  public
    property InitialDestination: TVector2Single read GetInitialDestination write SetInitialDestination;
    property InitialValue: TVector2Single read GetInitialValue write SetInitialValue;
  end;

  TPositionDamperNodeHelper = class helper for TPositionDamperNode
  private
    function GetInitialDestination: TVector3Single;
    procedure SetInitialDestination(const Value: TVector3Single);
    function GetInitialValue: TVector3Single;
    procedure SetInitialValue(const Value: TVector3Single);
  public
    property InitialDestination: TVector3Single read GetInitialDestination write SetInitialDestination;
    property InitialValue: TVector3Single read GetInitialValue write SetInitialValue;
  end;

  TPositionDamper2DNodeHelper = class helper for TPositionDamper2DNode
  private
    function GetInitialDestination: TVector2Single;
    procedure SetInitialDestination(const Value: TVector2Single);
    function GetInitialValue: TVector2Single;
    procedure SetInitialValue(const Value: TVector2Single);
  public
    property InitialDestination: TVector2Single read GetInitialDestination write SetInitialDestination;
    property InitialValue: TVector2Single read GetInitialValue write SetInitialValue;
  end;

  TScalarChaserNodeHelper = class helper for TScalarChaserNode
  private
    function GetInitialDestination: Single;
    procedure SetInitialDestination(const Value: Single);
    function GetInitialValue: Single;
    procedure SetInitialValue(const Value: Single);
  public
    property InitialDestination: Single read GetInitialDestination write SetInitialDestination;
    property InitialValue: Single read GetInitialValue write SetInitialValue;
  end;

  TArc2DNodeHelper = class helper for TArc2DNode
  private
    function GetEndAngle: Single;
    procedure SetEndAngle(const Value: Single);
    function GetRadius: Single;
    procedure SetRadius(const Value: Single);
    function GetStartAngle: Single;
    procedure SetStartAngle(const Value: Single);
  public
    property EndAngle: Single read GetEndAngle write SetEndAngle;
    property Radius: Single read GetRadius write SetRadius;
    property StartAngle: Single read GetStartAngle write SetStartAngle;
  end;

  TArcClose2DNodeHelper = class helper for TArcClose2DNode
  private
    function GetEndAngle: Single;
    procedure SetEndAngle(const Value: Single);
    function GetRadius: Single;
    procedure SetRadius(const Value: Single);
    function GetStartAngle: Single;
    procedure SetStartAngle(const Value: Single);
  public
    property EndAngle: Single read GetEndAngle write SetEndAngle;
    property Radius: Single read GetRadius write SetRadius;
    property StartAngle: Single read GetStartAngle write SetStartAngle;
  end;

  TCircle2DNodeHelper = class helper for TCircle2DNode
  private
    function GetRadius: Single;
    procedure SetRadius(const Value: Single);
    function GetSlices: Integer;
    procedure SetSlices(const Value: Integer);
  public
    property Radius: Single read GetRadius write SetRadius;
    property Slices: Integer read GetSlices write SetSlices;
  end;

  TDisk2DNodeHelper = class helper for TDisk2DNode
  private
    function GetInnerRadius: Single;
    procedure SetInnerRadius(const Value: Single);
    function GetOuterRadius: Single;
    procedure SetOuterRadius(const Value: Single);
  public
    property InnerRadius: Single read GetInnerRadius write SetInnerRadius;
    property OuterRadius: Single read GetOuterRadius write SetOuterRadius;
  end;

  TRectangle2DNodeHelper = class helper for TRectangle2DNode
  private
    function GetSize: TVector2Single;
    procedure SetSize(const Value: TVector2Single);
  public
    property Size: TVector2Single read GetSize write SetSize;
  end;

  TBoxNodeHelper = class helper for TBoxNode
  private
    function GetSize: TVector3Single;
    procedure SetSize(const Value: TVector3Single);
    function GetDivisions: Integer;
    procedure SetDivisions(const Value: Integer);
  public
    property Size: TVector3Single read GetSize write SetSize;
    property Divisions: Integer read GetDivisions write SetDivisions;
  end;

  TConeNodeHelper = class helper for TConeNode
  private
    function GetBottom: boolean;
    procedure SetBottom(const Value: boolean);
    function GetBottomRadius: Single;
    procedure SetBottomRadius(const Value: Single);
    function GetHeight: Single;
    procedure SetHeight(const Value: Single);
    function GetSide: boolean;
    procedure SetSide(const Value: boolean);
    function GetSlices: Integer;
    procedure SetSlices(const Value: Integer);
    function GetStacks: Integer;
    procedure SetStacks(const Value: Integer);
  public
    property Bottom: boolean read GetBottom write SetBottom;
    property BottomRadius: Single read GetBottomRadius write SetBottomRadius;
    property Height: Single read GetHeight write SetHeight;
    property Side: boolean read GetSide write SetSide;
    property Slices: Integer read GetSlices write SetSlices;
    property Stacks: Integer read GetStacks write SetStacks;
  end;

  TCylinderNodeHelper = class helper for TCylinderNode
  private
    function GetBottom: boolean;
    procedure SetBottom(const Value: boolean);
    function GetHeight: Single;
    procedure SetHeight(const Value: Single);
    function GetRadius: Single;
    procedure SetRadius(const Value: Single);
    function GetSide: boolean;
    procedure SetSide(const Value: boolean);
    function GetTop: boolean;
    procedure SetTop(const Value: boolean);
    function GetSlices: Integer;
    procedure SetSlices(const Value: Integer);
    function GetStacks: Integer;
    procedure SetStacks(const Value: Integer);
  public
    property Bottom: boolean read GetBottom write SetBottom;
    property Height: Single read GetHeight write SetHeight;
    property Radius: Single read GetRadius write SetRadius;
    property Side: boolean read GetSide write SetSide;
    property Top: boolean read GetTop write SetTop;
    property Slices: Integer read GetSlices write SetSlices;
    property Stacks: Integer read GetStacks write SetStacks;
  end;

  TElevationGridNodeHelper = class helper for TElevationGridNode
  private
    function GetCcw: boolean;
    procedure SetCcw(const Value: boolean);
    function GetColorPerVertex: boolean;
    procedure SetColorPerVertex(const Value: boolean);
    function GetCreaseAngle: Single;
    procedure SetCreaseAngle(const Value: Single);
    function GetNormalPerVertex: boolean;
    procedure SetNormalPerVertex(const Value: boolean);
    function GetXDimension: Integer;
    procedure SetXDimension(const Value: Integer);
    function GetXSpacing: Single;
    procedure SetXSpacing(const Value: Single);
    function GetZDimension: Integer;
    procedure SetZDimension(const Value: Integer);
    function GetZSpacing: Single;
    procedure SetZSpacing(const Value: Single);
  public
    property Ccw: boolean read GetCcw write SetCcw;
    property ColorPerVertex: boolean read GetColorPerVertex write SetColorPerVertex;
    property CreaseAngle: Single read GetCreaseAngle write SetCreaseAngle;
    property NormalPerVertex: boolean read GetNormalPerVertex write SetNormalPerVertex;
    property XDimension: Integer read GetXDimension write SetXDimension;
    property XSpacing: Single read GetXSpacing write SetXSpacing;
    property ZDimension: Integer read GetZDimension write SetZDimension;
    property ZSpacing: Single read GetZSpacing write SetZSpacing;
  end;

  TExtrusionNodeHelper = class helper for TExtrusionNode
  private
    function GetBeginCap: boolean;
    procedure SetBeginCap(const Value: boolean);
    function GetCcw: boolean;
    procedure SetCcw(const Value: boolean);
    function GetConvex: boolean;
    procedure SetConvex(const Value: boolean);
    function GetCreaseAngle: Single;
    procedure SetCreaseAngle(const Value: Single);
    function GetEndCap: boolean;
    procedure SetEndCap(const Value: boolean);
  public
    property BeginCap: boolean read GetBeginCap write SetBeginCap;
    property Ccw: boolean read GetCcw write SetCcw;
    property Convex: boolean read GetConvex write SetConvex;
    property CreaseAngle: Single read GetCreaseAngle write SetCreaseAngle;
    property EndCap: boolean read GetEndCap write SetEndCap;
  end;

  TIndexedFaceSetNodeHelper = class helper for TIndexedFaceSetNode
  private
    function GetConvex: boolean;
    procedure SetConvex(const Value: boolean);
    function GetCreaseAngle: Single;
    procedure SetCreaseAngle(const Value: Single);
  public
    property Convex: boolean read GetConvex write SetConvex;
    property CreaseAngle: Single read GetCreaseAngle write SetCreaseAngle;
  end;

  TSphereNodeHelper = class helper for TSphereNode
  private
    function GetRadius: Single;
    procedure SetRadius(const Value: Single);
    function GetSlices: Integer;
    procedure SetSlices(const Value: Integer);
    function GetStacks: Integer;
    procedure SetStacks(const Value: Integer);
  public
    property Radius: Single read GetRadius write SetRadius;
    property Slices: Integer read GetSlices write SetSlices;
    property Stacks: Integer read GetStacks write SetStacks;
  end;

  TGeoElevationGridNodeHelper = class helper for TGeoElevationGridNode
  private
    function GetYScale: Single;
    procedure SetYScale(const Value: Single);
    function GetCcw: boolean;
    procedure SetCcw(const Value: boolean);
    function GetColorPerVertex: boolean;
    procedure SetColorPerVertex(const Value: boolean);
    function GetCreaseAngle: Double;
    procedure SetCreaseAngle(const Value: Double);
    function GetGeoGridOrigin: TVector3Double;
    procedure SetGeoGridOrigin(const Value: TVector3Double);
    function GetNormalPerVertex: boolean;
    procedure SetNormalPerVertex(const Value: boolean);
    function GetXDimension: Integer;
    procedure SetXDimension(const Value: Integer);
    function GetXSpacing: Double;
    procedure SetXSpacing(const Value: Double);
    function GetZDimension: Integer;
    procedure SetZDimension(const Value: Integer);
    function GetZSpacing: Double;
    procedure SetZSpacing(const Value: Double);
  public
    property YScale: Single read GetYScale write SetYScale;
    property Ccw: boolean read GetCcw write SetCcw;
    property ColorPerVertex: boolean read GetColorPerVertex write SetColorPerVertex;
    property CreaseAngle: Double read GetCreaseAngle write SetCreaseAngle;
    property GeoGridOrigin: TVector3Double read GetGeoGridOrigin write SetGeoGridOrigin;
    property NormalPerVertex: boolean read GetNormalPerVertex write SetNormalPerVertex;
    property XDimension: Integer read GetXDimension write SetXDimension;
    property XSpacing: Double read GetXSpacing write SetXSpacing;
    property ZDimension: Integer read GetZDimension write SetZDimension;
    property ZSpacing: Double read GetZSpacing write SetZSpacing;
  end;

  TGeoLocationNodeHelper = class helper for TGeoLocationNode
  private
    function GetGeoCoords: TVector3Double;
    procedure SetGeoCoords(const Value: TVector3Double);
  public
    property GeoCoords: TVector3Double read GetGeoCoords write SetGeoCoords;
  end;

  TGeoLODNodeHelper = class helper for TGeoLODNode
  private
    function GetCenter: TVector3Double;
    procedure SetCenter(const Value: TVector3Double);
    function GetRange: Single;
    procedure SetRange(const Value: Single);
    function GetBboxCenter: TVector3Single;
    procedure SetBboxCenter(const Value: TVector3Single);
    function GetBboxSize: TVector3Single;
    procedure SetBboxSize(const Value: TVector3Single);
  public
    property Center: TVector3Double read GetCenter write SetCenter;
    property Range: Single read GetRange write SetRange;
    property BboxCenter: TVector3Single read GetBboxCenter write SetBboxCenter;
    property BboxSize: TVector3Single read GetBboxSize write SetBboxSize;
  end;

  TGeoOriginNodeHelper = class helper for TGeoOriginNode
  private
    function GetGeoCoords: TVector3Double;
    procedure SetGeoCoords(const Value: TVector3Double);
    function GetRotateYUp: boolean;
    procedure SetRotateYUp(const Value: boolean);
  public
    property GeoCoords: TVector3Double read GetGeoCoords write SetGeoCoords;
    property RotateYUp: boolean read GetRotateYUp write SetRotateYUp;
  end;

  TGeoProximitySensorNodeHelper = class helper for TGeoProximitySensorNode
  private
    function GetGeoCenter: TVector3Double;
    procedure SetGeoCenter(const Value: TVector3Double);
  public
    property GeoCenter: TVector3Double read GetGeoCenter write SetGeoCenter;
  end;

  TGeoTransformNodeHelper = class helper for TGeoTransformNode
  private
    function GetGeoCenter: TVector3Double;
    procedure SetGeoCenter(const Value: TVector3Double);
    function GetRotation: TVector4Single;
    procedure SetRotation(const Value: TVector4Single);
    function GetScale: TVector3Single;
    procedure SetScale(const Value: TVector3Single);
    function GetScaleOrientation: TVector4Single;
    procedure SetScaleOrientation(const Value: TVector4Single);
    function GetTranslation: TVector3Single;
    procedure SetTranslation(const Value: TVector3Single);
  public
    property GeoCenter: TVector3Double read GetGeoCenter write SetGeoCenter;
    property Rotation: TVector4Single read GetRotation write SetRotation;
    property Scale: TVector3Single read GetScale write SetScale;
    property ScaleOrientation: TVector4Single read GetScaleOrientation write SetScaleOrientation;
    property Translation: TVector3Single read GetTranslation write SetTranslation;
  end;

  TGeoViewpointNodeHelper = class helper for TGeoViewpointNode
  private
    function GetFieldOfView: Single;
    procedure SetFieldOfView(const Value: Single);
    function GetHeadlight: boolean;
    procedure SetHeadlight(const Value: boolean);
    function GetPosition: TVector3Double;
    procedure SetPosition(const Value: TVector3Double);
    function GetSpeedFactor: Single;
    procedure SetSpeedFactor(const Value: Single);
  public
    property FieldOfView: Single read GetFieldOfView write SetFieldOfView;
    property Headlight: boolean read GetHeadlight write SetHeadlight;
    property Position: TVector3Double read GetPosition write SetPosition;
    property SpeedFactor: Single read GetSpeedFactor write SetSpeedFactor;
  end;

  TAbstractX3DGroupingNodeHelper = class helper for TAbstractX3DGroupingNode
  private
    function GetBboxCenter: TVector3Single;
    procedure SetBboxCenter(const Value: TVector3Single);
    function GetBboxSize: TVector3Single;
    procedure SetBboxSize(const Value: TVector3Single);
  public
    property BboxCenter: TVector3Single read GetBboxCenter write SetBboxCenter;
    property BboxSize: TVector3Single read GetBboxSize write SetBboxSize;
  end;

  TStaticGroupNodeHelper = class helper for TStaticGroupNode
  private
    function GetBboxCenter: TVector3Single;
    procedure SetBboxCenter(const Value: TVector3Single);
    function GetBboxSize: TVector3Single;
    procedure SetBboxSize(const Value: TVector3Single);
  public
    property BboxCenter: TVector3Single read GetBboxCenter write SetBboxCenter;
    property BboxSize: TVector3Single read GetBboxSize write SetBboxSize;
  end;

  TSwitchNodeHelper = class helper for TSwitchNode
  private
    function GetWhichChoice: Integer;
    procedure SetWhichChoice(const Value: Integer);
  public
    property WhichChoice: Integer read GetWhichChoice write SetWhichChoice;
  end;

  TTransformNodeHelper = class helper for TTransformNode
  private
    function GetCenter: TVector3Single;
    procedure SetCenter(const Value: TVector3Single);
    function GetRotation: TVector4Single;
    procedure SetRotation(const Value: TVector4Single);
    function GetScale: TVector3Single;
    procedure SetScale(const Value: TVector3Single);
    function GetScaleOrientation: TVector4Single;
    procedure SetScaleOrientation(const Value: TVector4Single);
    function GetTranslation: TVector3Single;
    procedure SetTranslation(const Value: TVector3Single);
  public
    property Center: TVector3Single read GetCenter write SetCenter;
    property Rotation: TVector4Single read GetRotation write SetRotation;
    property Scale: TVector3Single read GetScale write SetScale;
    property ScaleOrientation: TVector4Single read GetScaleOrientation write SetScaleOrientation;
    property Translation: TVector3Single read GetTranslation write SetTranslation;
  end;

  THAnimDisplacerNodeHelper = class helper for THAnimDisplacerNode
  private
    function GetNameField: string;
    procedure SetNameField(const Value: string);
    function GetWeight: Single;
    procedure SetWeight(const Value: Single);
  public
    property NameField: string read GetNameField write SetNameField;
    property Weight: Single read GetWeight write SetWeight;
  end;

  THAnimHumanoidNodeHelper = class helper for THAnimHumanoidNode
  private
    function GetCenter: TVector3Single;
    procedure SetCenter(const Value: TVector3Single);
    function GetNameField: string;
    procedure SetNameField(const Value: string);
    function GetRotation: TVector4Single;
    procedure SetRotation(const Value: TVector4Single);
    function GetScale: TVector3Single;
    procedure SetScale(const Value: TVector3Single);
    function GetScaleOrientation: TVector4Single;
    procedure SetScaleOrientation(const Value: TVector4Single);
    function GetTranslation: TVector3Single;
    procedure SetTranslation(const Value: TVector3Single);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetBboxCenter: TVector3Single;
    procedure SetBboxCenter(const Value: TVector3Single);
    function GetBboxSize: TVector3Single;
    procedure SetBboxSize(const Value: TVector3Single);
  public
    property Center: TVector3Single read GetCenter write SetCenter;
    property NameField: string read GetNameField write SetNameField;
    property Rotation: TVector4Single read GetRotation write SetRotation;
    property Scale: TVector3Single read GetScale write SetScale;
    property ScaleOrientation: TVector4Single read GetScaleOrientation write SetScaleOrientation;
    property Translation: TVector3Single read GetTranslation write SetTranslation;
    property Version: string read GetVersion write SetVersion;
    property BboxCenter: TVector3Single read GetBboxCenter write SetBboxCenter;
    property BboxSize: TVector3Single read GetBboxSize write SetBboxSize;
  end;

  THAnimJointNodeHelper = class helper for THAnimJointNode
  private
    function GetCenter: TVector3Single;
    procedure SetCenter(const Value: TVector3Single);
    function GetLimitOrientation: TVector4Single;
    procedure SetLimitOrientation(const Value: TVector4Single);
    function GetNameField: string;
    procedure SetNameField(const Value: string);
    function GetRotation: TVector4Single;
    procedure SetRotation(const Value: TVector4Single);
    function GetScale: TVector3Single;
    procedure SetScale(const Value: TVector3Single);
    function GetScaleOrientation: TVector4Single;
    procedure SetScaleOrientation(const Value: TVector4Single);
    function GetTranslation: TVector3Single;
    procedure SetTranslation(const Value: TVector3Single);
  public
    property Center: TVector3Single read GetCenter write SetCenter;
    property LimitOrientation: TVector4Single read GetLimitOrientation write SetLimitOrientation;
    property NameField: string read GetNameField write SetNameField;
    property Rotation: TVector4Single read GetRotation write SetRotation;
    property Scale: TVector3Single read GetScale write SetScale;
    property ScaleOrientation: TVector4Single read GetScaleOrientation write SetScaleOrientation;
    property Translation: TVector3Single read GetTranslation write SetTranslation;
  end;

  THAnimSegmentNodeHelper = class helper for THAnimSegmentNode
  private
    function GetCenterOfMass: TVector3Single;
    procedure SetCenterOfMass(const Value: TVector3Single);
    function GetMass: Single;
    procedure SetMass(const Value: Single);
    function GetNameField: string;
    procedure SetNameField(const Value: string);
  public
    property CenterOfMass: TVector3Single read GetCenterOfMass write SetCenterOfMass;
    property Mass: Single read GetMass write SetMass;
    property NameField: string read GetNameField write SetNameField;
  end;

  THAnimSiteNodeHelper = class helper for THAnimSiteNode
  private
    function GetCenter: TVector3Single;
    procedure SetCenter(const Value: TVector3Single);
    function GetNameField: string;
    procedure SetNameField(const Value: string);
    function GetRotation: TVector4Single;
    procedure SetRotation(const Value: TVector4Single);
    function GetScale: TVector3Single;
    procedure SetScale(const Value: TVector3Single);
    function GetScaleOrientation: TVector4Single;
    procedure SetScaleOrientation(const Value: TVector4Single);
    function GetTranslation: TVector3Single;
    procedure SetTranslation(const Value: TVector3Single);
  public
    property Center: TVector3Single read GetCenter write SetCenter;
    property NameField: string read GetNameField write SetNameField;
    property Rotation: TVector4Single read GetRotation write SetRotation;
    property Scale: TVector3Single read GetScale write SetScale;
    property ScaleOrientation: TVector4Single read GetScaleOrientation write SetScaleOrientation;
    property Translation: TVector3Single read GetTranslation write SetTranslation;
  end;

  TSplinePositionInterpolatorNodeHelper = class helper for TSplinePositionInterpolatorNode
  private
    function GetClosed: boolean;
    procedure SetClosed(const Value: boolean);
    function GetNormalizeVelocity: boolean;
    procedure SetNormalizeVelocity(const Value: boolean);
  public
    property Closed: boolean read GetClosed write SetClosed;
    property NormalizeVelocity: boolean read GetNormalizeVelocity write SetNormalizeVelocity;
  end;

  TSplinePositionInterpolator2DNodeHelper = class helper for TSplinePositionInterpolator2DNode
  private
    function GetClosed: boolean;
    procedure SetClosed(const Value: boolean);
    function GetNormalizeVelocity: boolean;
    procedure SetNormalizeVelocity(const Value: boolean);
  public
    property Closed: boolean read GetClosed write SetClosed;
    property NormalizeVelocity: boolean read GetNormalizeVelocity write SetNormalizeVelocity;
  end;

  TSplineScalarInterpolatorNodeHelper = class helper for TSplineScalarInterpolatorNode
  private
    function GetClosed: boolean;
    procedure SetClosed(const Value: boolean);
    function GetNormalizeVelocity: boolean;
    procedure SetNormalizeVelocity(const Value: boolean);
  public
    property Closed: boolean read GetClosed write SetClosed;
    property NormalizeVelocity: boolean read GetNormalizeVelocity write SetNormalizeVelocity;
  end;

  TSquadOrientationInterpolatorNodeHelper = class helper for TSquadOrientationInterpolatorNode
  private
    function GetNormalizeVelocity: boolean;
    procedure SetNormalizeVelocity(const Value: boolean);
  public
    property NormalizeVelocity: boolean read GetNormalizeVelocity write SetNormalizeVelocity;
  end;

  TStringSensorNodeHelper = class helper for TStringSensorNode
  private
    function GetDeletionAllowed: boolean;
    procedure SetDeletionAllowed(const Value: boolean);
  public
    property DeletionAllowed: boolean read GetDeletionAllowed write SetDeletionAllowed;
  end;

  TAbstractLayerNodeHelper = class helper for TAbstractLayerNode
  private
    function GetIsPickable: boolean;
    procedure SetIsPickable(const Value: boolean);
  public
    property IsPickable: boolean read GetIsPickable write SetIsPickable;
  end;

  TLayerSetNodeHelper = class helper for TLayerSetNode
  private
    function GetActiveLayer: Integer;
    procedure SetActiveLayer(const Value: Integer);
  public
    property ActiveLayer: Integer read GetActiveLayer write SetActiveLayer;
  end;

  TScreenFontStyleNodeHelper = class helper for TScreenFontStyleNode
  private
    function GetHorizontal: boolean;
    procedure SetHorizontal(const Value: boolean);
    function GetLanguage: string;
    procedure SetLanguage(const Value: string);
    function GetLeftToRight: boolean;
    procedure SetLeftToRight(const Value: boolean);
    function GetPointSize: Single;
    procedure SetPointSize(const Value: Single);
    function GetSpacing: Single;
    procedure SetSpacing(const Value: Single);
    function GetTopToBottom: boolean;
    procedure SetTopToBottom(const Value: boolean);
  public
    property Horizontal: boolean read GetHorizontal write SetHorizontal;
    property Language: string read GetLanguage write SetLanguage;
    property LeftToRight: boolean read GetLeftToRight write SetLeftToRight;
    property PointSize: Single read GetPointSize write SetPointSize;
    property Spacing: Single read GetSpacing write SetSpacing;
    property TopToBottom: boolean read GetTopToBottom write SetTopToBottom;
  end;

  TAbstractLightNodeHelper = class helper for TAbstractLightNode
  private
    function GetAmbientIntensity: Single;
    procedure SetAmbientIntensity(const Value: Single);
    function GetColor: TCastleColorRGB;
    procedure SetColor(const Value: TCastleColorRGB);
    function GetGlobal: boolean;
    procedure SetGlobal(const Value: boolean);
    function GetIntensity: Single;
    procedure SetIntensity(const Value: Single);
    function GetIsOn: boolean;
    procedure SetIsOn(const Value: boolean);
    function GetShadows: boolean;
    procedure SetShadows(const Value: boolean);
    function GetShadowVolumes: boolean;
    procedure SetShadowVolumes(const Value: boolean);
    function GetShadowVolumesMain: boolean;
    procedure SetShadowVolumesMain(const Value: boolean);
    function GetProjectionNear: Single;
    procedure SetProjectionNear(const Value: Single);
    function GetProjectionFar: Single;
    procedure SetProjectionFar(const Value: Single);
    function GetUp: TVector3Single;
    procedure SetUp(const Value: TVector3Single);
  public
    property AmbientIntensity: Single read GetAmbientIntensity write SetAmbientIntensity;
    property Color: TCastleColorRGB read GetColor write SetColor;
    property Global: boolean read GetGlobal write SetGlobal;
    property Intensity: Single read GetIntensity write SetIntensity;
    property IsOn: boolean read GetIsOn write SetIsOn;
    property Shadows: boolean read GetShadows write SetShadows;
    property ShadowVolumes: boolean read GetShadowVolumes write SetShadowVolumes;
    property ShadowVolumesMain: boolean read GetShadowVolumesMain write SetShadowVolumesMain;
    property ProjectionNear: Single read GetProjectionNear write SetProjectionNear;
    property ProjectionFar: Single read GetProjectionFar write SetProjectionFar;
    property Up: TVector3Single read GetUp write SetUp;
  end;

  TSpotLightNodeHelper = class helper for TSpotLightNode
  private
    function GetAttenuation: TVector3Single;
    procedure SetAttenuation(const Value: TVector3Single);
    function GetBeamWidth: Single;
    procedure SetBeamWidth(const Value: Single);
    function GetCutOffAngle: Single;
    procedure SetCutOffAngle(const Value: Single);
    function GetDirection: TVector3Single;
    procedure SetDirection(const Value: TVector3Single);
    function GetLocation: TVector3Single;
    procedure SetLocation(const Value: TVector3Single);
    function GetRadius: Single;
    procedure SetRadius(const Value: Single);
    function GetProjectionAngle: Single;
    procedure SetProjectionAngle(const Value: Single);
  public
    property Attenuation: TVector3Single read GetAttenuation write SetAttenuation;
    property BeamWidth: Single read GetBeamWidth write SetBeamWidth;
    property CutOffAngle: Single read GetCutOffAngle write SetCutOffAngle;
    property Direction: TVector3Single read GetDirection write SetDirection;
    property Location: TVector3Single read GetLocation write SetLocation;
    property Radius: Single read GetRadius write SetRadius;
    property ProjectionAngle: Single read GetProjectionAngle write SetProjectionAngle;
  end;

  TAbstractX3DViewpointNodeHelper = class helper for TAbstractX3DViewpointNode
  private
    function GetCenterOfRotation: TVector3Single;
    procedure SetCenterOfRotation(const Value: TVector3Single);
    function GetDescription: string;
    procedure SetDescription(const Value: string);
    function GetJump: boolean;
    procedure SetJump(const Value: boolean);
    function GetOrientation: TVector4Single;
    procedure SetOrientation(const Value: TVector4Single);
    function GetRetainUserOffsets: boolean;
    procedure SetRetainUserOffsets(const Value: boolean);
    function GetCameraMatrixSendAlsoOnOffscreenRendering: boolean;
    procedure SetCameraMatrixSendAlsoOnOffscreenRendering(const Value: boolean);
    function GetGravityUp: TVector3Single;
    procedure SetGravityUp(const Value: TVector3Single);
  public
    property CenterOfRotation: TVector3Single read GetCenterOfRotation write SetCenterOfRotation;
    property Description: string read GetDescription write SetDescription;
    property Jump: boolean read GetJump write SetJump;
    property Orientation: TVector4Single read GetOrientation write SetOrientation;
    property RetainUserOffsets: boolean read GetRetainUserOffsets write SetRetainUserOffsets;
    property CameraMatrixSendAlsoOnOffscreenRendering: boolean read GetCameraMatrixSendAlsoOnOffscreenRendering write SetCameraMatrixSendAlsoOnOffscreenRendering;
    property GravityUp: TVector3Single read GetGravityUp write SetGravityUp;
  end;

  TBillboardNodeHelper = class helper for TBillboardNode
  private
    function GetAxisOfRotation: TVector3Single;
    procedure SetAxisOfRotation(const Value: TVector3Single);
  public
    property AxisOfRotation: TVector3Single read GetAxisOfRotation write SetAxisOfRotation;
  end;

  TCollisionNodeHelper = class helper for TCollisionNode
  private
    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
  public
    property Enabled: boolean read GetEnabled write SetEnabled;
  end;

  TLODNodeHelper = class helper for TLODNode
  private
    function GetCenter: TVector3Single;
    procedure SetCenter(const Value: TVector3Single);
    function GetForceTransitions: boolean;
    procedure SetForceTransitions(const Value: boolean);
  public
    property Center: TVector3Single read GetCenter write SetCenter;
    property ForceTransitions: boolean read GetForceTransitions write SetForceTransitions;
  end;

  TNavigationInfoNodeHelper = class helper for TNavigationInfoNode
  private
    function GetHeadlight: boolean;
    procedure SetHeadlight(const Value: boolean);
    function GetSpeed: Single;
    procedure SetSpeed(const Value: Single);
    function GetTransitionTime: TFloatTime;
    procedure SetTransitionTime(const Value: TFloatTime);
    function GetVisibilityLimit: Single;
    procedure SetVisibilityLimit(const Value: Single);
  public
    property Headlight: boolean read GetHeadlight write SetHeadlight;
    property Speed: Single read GetSpeed write SetSpeed;
    property TransitionTime: TFloatTime read GetTransitionTime write SetTransitionTime;
    property VisibilityLimit: Single read GetVisibilityLimit write SetVisibilityLimit;
  end;

  TOrthoViewpointNodeHelper = class helper for TOrthoViewpointNode
  private
    function GetPosition: TVector3Single;
    procedure SetPosition(const Value: TVector3Single);
  public
    property Position: TVector3Single read GetPosition write SetPosition;
  end;

  TViewpointNodeHelper = class helper for TViewpointNode
  private
    function GetFieldOfView: Single;
    procedure SetFieldOfView(const Value: Single);
    function GetPosition: TVector3Single;
    procedure SetPosition(const Value: TVector3Single);
  public
    property FieldOfView: Single read GetFieldOfView write SetFieldOfView;
    property Position: TVector3Single read GetPosition write SetPosition;
  end;

  TViewpointGroupNodeHelper = class helper for TViewpointGroupNode
  private
    function GetCenter: TVector3Single;
    procedure SetCenter(const Value: TVector3Single);
    function GetDescription: string;
    procedure SetDescription(const Value: string);
    function GetDisplayed: boolean;
    procedure SetDisplayed(const Value: boolean);
    function GetRetainUserOffsets: boolean;
    procedure SetRetainUserOffsets(const Value: boolean);
    function GetSize: TVector3Single;
    procedure SetSize(const Value: TVector3Single);
  public
    property Center: TVector3Single read GetCenter write SetCenter;
    property Description: string read GetDescription write SetDescription;
    property Displayed: boolean read GetDisplayed write SetDisplayed;
    property RetainUserOffsets: boolean read GetRetainUserOffsets write SetRetainUserOffsets;
    property Size: TVector3Single read GetSize write SetSize;
  end;

  TAnchorNodeHelper = class helper for TAnchorNode
  private
    function GetDescription: string;
    procedure SetDescription(const Value: string);
  public
    property Description: string read GetDescription write SetDescription;
  end;

  TInlineNodeHelper = class helper for TInlineNode
  private
    function GetLoad: boolean;
    procedure SetLoad(const Value: boolean);
    function GetBboxCenter: TVector3Single;
    procedure SetBboxCenter(const Value: TVector3Single);
    function GetBboxSize: TVector3Single;
    procedure SetBboxSize(const Value: TVector3Single);
  public
    property Load: boolean read GetLoad write SetLoad;
    property BboxCenter: TVector3Single read GetBboxCenter write SetBboxCenter;
    property BboxSize: TVector3Single read GetBboxSize write SetBboxSize;
  end;

  TLoadSensorNodeHelper = class helper for TLoadSensorNode
  private
    function GetTimeOut: TFloatTime;
    procedure SetTimeOut(const Value: TFloatTime);
  public
    property TimeOut: TFloatTime read GetTimeOut write SetTimeOut;
  end;

  TAbstractNurbsSurfaceGeometryNodeHelper = class helper for TAbstractNurbsSurfaceGeometryNode
  private
    function GetUTessellation: Integer;
    procedure SetUTessellation(const Value: Integer);
    function GetVTessellation: Integer;
    procedure SetVTessellation(const Value: Integer);
    function GetUClosed: boolean;
    procedure SetUClosed(const Value: boolean);
    function GetUDimension: Integer;
    procedure SetUDimension(const Value: Integer);
    function GetUOrder: Integer;
    procedure SetUOrder(const Value: Integer);
    function GetVClosed: boolean;
    procedure SetVClosed(const Value: boolean);
    function GetVDimension: Integer;
    procedure SetVDimension(const Value: Integer);
    function GetVOrder: Integer;
    procedure SetVOrder(const Value: Integer);
  public
    property UTessellation: Integer read GetUTessellation write SetUTessellation;
    property VTessellation: Integer read GetVTessellation write SetVTessellation;
    property UClosed: boolean read GetUClosed write SetUClosed;
    property UDimension: Integer read GetUDimension write SetUDimension;
    property UOrder: Integer read GetUOrder write SetUOrder;
    property VClosed: boolean read GetVClosed write SetVClosed;
    property VDimension: Integer read GetVDimension write SetVDimension;
    property VOrder: Integer read GetVOrder write SetVOrder;
  end;

  TNurbsCurveNodeHelper = class helper for TNurbsCurveNode
  private
    function GetTessellation: Integer;
    procedure SetTessellation(const Value: Integer);
    function GetClosed: boolean;
    procedure SetClosed(const Value: boolean);
    function GetOrder: Integer;
    procedure SetOrder(const Value: Integer);
  public
    property Tessellation: Integer read GetTessellation write SetTessellation;
    property Closed: boolean read GetClosed write SetClosed;
    property Order: Integer read GetOrder write SetOrder;
  end;

  TNurbsCurve2DNodeHelper = class helper for TNurbsCurve2DNode
  private
    function GetTessellation: Integer;
    procedure SetTessellation(const Value: Integer);
    function GetClosed: boolean;
    procedure SetClosed(const Value: boolean);
    function GetOrder: Integer;
    procedure SetOrder(const Value: Integer);
  public
    property Tessellation: Integer read GetTessellation write SetTessellation;
    property Closed: boolean read GetClosed write SetClosed;
    property Order: Integer read GetOrder write SetOrder;
  end;

  TNurbsOrientationInterpolatorNodeHelper = class helper for TNurbsOrientationInterpolatorNode
  private
    function GetOrder: Integer;
    procedure SetOrder(const Value: Integer);
  public
    property Order: Integer read GetOrder write SetOrder;
  end;

  TNurbsPositionInterpolatorNodeHelper = class helper for TNurbsPositionInterpolatorNode
  private
    function GetOrder: Integer;
    procedure SetOrder(const Value: Integer);
  public
    property Order: Integer read GetOrder write SetOrder;
  end;

  TNurbsSetNodeHelper = class helper for TNurbsSetNode
  private
    function GetTessellationScale: Single;
    procedure SetTessellationScale(const Value: Single);
    function GetBboxCenter: TVector3Single;
    procedure SetBboxCenter(const Value: TVector3Single);
    function GetBboxSize: TVector3Single;
    procedure SetBboxSize(const Value: TVector3Single);
  public
    property TessellationScale: Single read GetTessellationScale write SetTessellationScale;
    property BboxCenter: TVector3Single read GetBboxCenter write SetBboxCenter;
    property BboxSize: TVector3Single read GetBboxSize write SetBboxSize;
  end;

  TNurbsSurfaceInterpolatorNodeHelper = class helper for TNurbsSurfaceInterpolatorNode
  private
    function GetUDimension: Integer;
    procedure SetUDimension(const Value: Integer);
    function GetUOrder: Integer;
    procedure SetUOrder(const Value: Integer);
    function GetVDimension: Integer;
    procedure SetVDimension(const Value: Integer);
    function GetVOrder: Integer;
    procedure SetVOrder(const Value: Integer);
  public
    property UDimension: Integer read GetUDimension write SetUDimension;
    property UOrder: Integer read GetUOrder write SetUOrder;
    property VDimension: Integer read GetVDimension write SetVDimension;
    property VOrder: Integer read GetVOrder write SetVOrder;
  end;

  TNurbsSweptSurfaceNodeHelper = class helper for TNurbsSweptSurfaceNode
  private
    function GetCcw: boolean;
    procedure SetCcw(const Value: boolean);
  public
    property Ccw: boolean read GetCcw write SetCcw;
  end;

  TNurbsSwungSurfaceNodeHelper = class helper for TNurbsSwungSurfaceNode
  private
    function GetCcw: boolean;
    procedure SetCcw(const Value: boolean);
  public
    property Ccw: boolean read GetCcw write SetCcw;
  end;

  TNurbsTextureCoordinateNodeHelper = class helper for TNurbsTextureCoordinateNode
  private
    function GetUDimension: Integer;
    procedure SetUDimension(const Value: Integer);
    function GetUOrder: Integer;
    procedure SetUOrder(const Value: Integer);
    function GetVDimension: Integer;
    procedure SetVDimension(const Value: Integer);
    function GetVOrder: Integer;
    procedure SetVOrder(const Value: Integer);
  public
    property UDimension: Integer read GetUDimension write SetUDimension;
    property UOrder: Integer read GetUOrder write SetUOrder;
    property VDimension: Integer read GetVDimension write SetVDimension;
    property VOrder: Integer read GetVOrder write SetVOrder;
  end;

  TAbstractParticleEmitterNodeHelper = class helper for TAbstractParticleEmitterNode
  private
    function GetSpeed: Single;
    procedure SetSpeed(const Value: Single);
    function GetVariation: Single;
    procedure SetVariation(const Value: Single);
    function GetMass: Single;
    procedure SetMass(const Value: Single);
    function GetSurfaceArea: Single;
    procedure SetSurfaceArea(const Value: Single);
  public
    property Speed: Single read GetSpeed write SetSpeed;
    property Variation: Single read GetVariation write SetVariation;
    property Mass: Single read GetMass write SetMass;
    property SurfaceArea: Single read GetSurfaceArea write SetSurfaceArea;
  end;

  TAbstractParticlePhysicsModelNodeHelper = class helper for TAbstractParticlePhysicsModelNode
  private
    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
  public
    property Enabled: boolean read GetEnabled write SetEnabled;
  end;

  TConeEmitterNodeHelper = class helper for TConeEmitterNode
  private
    function GetAngle: Single;
    procedure SetAngle(const Value: Single);
    function GetDirection: TVector3Single;
    procedure SetDirection(const Value: TVector3Single);
    function GetPosition: TVector3Single;
    procedure SetPosition(const Value: TVector3Single);
  public
    property Angle: Single read GetAngle write SetAngle;
    property Direction: TVector3Single read GetDirection write SetDirection;
    property Position: TVector3Single read GetPosition write SetPosition;
  end;

  TExplosionEmitterNodeHelper = class helper for TExplosionEmitterNode
  private
    function GetPosition: TVector3Single;
    procedure SetPosition(const Value: TVector3Single);
  public
    property Position: TVector3Single read GetPosition write SetPosition;
  end;

  TParticleSystemNodeHelper = class helper for TParticleSystemNode
  private
    function GetCreateParticles: boolean;
    procedure SetCreateParticles(const Value: boolean);
    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
    function GetLifetimeVariation: Single;
    procedure SetLifetimeVariation(const Value: Single);
    function GetMaxParticles: Integer;
    procedure SetMaxParticles(const Value: Integer);
    function GetParticleLifetime: Single;
    procedure SetParticleLifetime(const Value: Single);
    function GetParticleSize: TVector2Single;
    procedure SetParticleSize(const Value: TVector2Single);
  public
    property CreateParticles: boolean read GetCreateParticles write SetCreateParticles;
    property Enabled: boolean read GetEnabled write SetEnabled;
    property LifetimeVariation: Single read GetLifetimeVariation write SetLifetimeVariation;
    property MaxParticles: Integer read GetMaxParticles write SetMaxParticles;
    property ParticleLifetime: Single read GetParticleLifetime write SetParticleLifetime;
    property ParticleSize: TVector2Single read GetParticleSize write SetParticleSize;
  end;

  TPointEmitterNodeHelper = class helper for TPointEmitterNode
  private
    function GetDirection: TVector3Single;
    procedure SetDirection(const Value: TVector3Single);
    function GetPosition: TVector3Single;
    procedure SetPosition(const Value: TVector3Single);
  public
    property Direction: TVector3Single read GetDirection write SetDirection;
    property Position: TVector3Single read GetPosition write SetPosition;
  end;

  TPolylineEmitterNodeHelper = class helper for TPolylineEmitterNode
  private
    function GetDirection: TVector3Single;
    procedure SetDirection(const Value: TVector3Single);
  public
    property Direction: TVector3Single read GetDirection write SetDirection;
  end;

  TVolumeEmitterNodeHelper = class helper for TVolumeEmitterNode
  private
    function GetDirection: TVector3Single;
    procedure SetDirection(const Value: TVector3Single);
    function GetInternal: boolean;
    procedure SetInternal(const Value: boolean);
  public
    property Direction: TVector3Single read GetDirection write SetDirection;
    property Internal: boolean read GetInternal write SetInternal;
  end;

  TWindPhysicsModelNodeHelper = class helper for TWindPhysicsModelNode
  private
    function GetDirection: TVector3Single;
    procedure SetDirection(const Value: TVector3Single);
    function GetGustiness: Single;
    procedure SetGustiness(const Value: Single);
    function GetSpeed: Single;
    procedure SetSpeed(const Value: Single);
    function GetTurbulence: Single;
    procedure SetTurbulence(const Value: Single);
  public
    property Direction: TVector3Single read GetDirection write SetDirection;
    property Gustiness: Single read GetGustiness write SetGustiness;
    property Speed: Single read GetSpeed write SetSpeed;
    property Turbulence: Single read GetTurbulence write SetTurbulence;
  end;

  TPickableGroupNodeHelper = class helper for TPickableGroupNode
  private
    function GetPickable: boolean;
    procedure SetPickable(const Value: boolean);
  public
    property Pickable: boolean read GetPickable write SetPickable;
  end;

  TAbstractDragSensorNodeHelper = class helper for TAbstractDragSensorNode
  private
    function GetAutoOffset: boolean;
    procedure SetAutoOffset(const Value: boolean);
  public
    property AutoOffset: boolean read GetAutoOffset write SetAutoOffset;
  end;

  TAbstractPointingDeviceSensorNodeHelper = class helper for TAbstractPointingDeviceSensorNode
  private
    function GetDescription: string;
    procedure SetDescription(const Value: string);
  public
    property Description: string read GetDescription write SetDescription;
  end;

  TCylinderSensorNodeHelper = class helper for TCylinderSensorNode
  private
    function GetAxisRotation: TVector4Single;
    procedure SetAxisRotation(const Value: TVector4Single);
    function GetDiskAngle: Single;
    procedure SetDiskAngle(const Value: Single);
    function GetMaxAngle: Single;
    procedure SetMaxAngle(const Value: Single);
    function GetMinAngle: Single;
    procedure SetMinAngle(const Value: Single);
    function GetOffset: Single;
    procedure SetOffset(const Value: Single);
  public
    property AxisRotation: TVector4Single read GetAxisRotation write SetAxisRotation;
    property DiskAngle: Single read GetDiskAngle write SetDiskAngle;
    property MaxAngle: Single read GetMaxAngle write SetMaxAngle;
    property MinAngle: Single read GetMinAngle write SetMinAngle;
    property Offset: Single read GetOffset write SetOffset;
  end;

  TPlaneSensorNodeHelper = class helper for TPlaneSensorNode
  private
    function GetAxisRotation: TVector4Single;
    procedure SetAxisRotation(const Value: TVector4Single);
    function GetMaxPosition: TVector2Single;
    procedure SetMaxPosition(const Value: TVector2Single);
    function GetMinPosition: TVector2Single;
    procedure SetMinPosition(const Value: TVector2Single);
    function GetOffset: TVector3Single;
    procedure SetOffset(const Value: TVector3Single);
  public
    property AxisRotation: TVector4Single read GetAxisRotation write SetAxisRotation;
    property MaxPosition: TVector2Single read GetMaxPosition write SetMaxPosition;
    property MinPosition: TVector2Single read GetMinPosition write SetMinPosition;
    property Offset: TVector3Single read GetOffset write SetOffset;
  end;

  TSphereSensorNodeHelper = class helper for TSphereSensorNode
  private
    function GetOffset: TVector4Single;
    procedure SetOffset(const Value: TVector4Single);
  public
    property Offset: TVector4Single read GetOffset write SetOffset;
  end;

  TAbstractComposedGeometryNodeHelper = class helper for TAbstractComposedGeometryNode
  private
    function GetCcw: boolean;
    procedure SetCcw(const Value: boolean);
    function GetColorPerVertex: boolean;
    procedure SetColorPerVertex(const Value: boolean);
    function GetNormalPerVertex: boolean;
    procedure SetNormalPerVertex(const Value: boolean);
  public
    property Ccw: boolean read GetCcw write SetCcw;
    property ColorPerVertex: boolean read GetColorPerVertex write SetColorPerVertex;
    property NormalPerVertex: boolean read GetNormalPerVertex write SetNormalPerVertex;
  end;

  TClipPlaneNodeHelper = class helper for TClipPlaneNode
  private
    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
    function GetPlane: TVector4Single;
    procedure SetPlane(const Value: TVector4Single);
  public
    property Enabled: boolean read GetEnabled write SetEnabled;
    property Plane: TVector4Single read GetPlane write SetPlane;
  end;

  TIndexedLineSetNodeHelper = class helper for TIndexedLineSetNode
  private
    function GetColorPerVertex: boolean;
    procedure SetColorPerVertex(const Value: boolean);
  public
    property ColorPerVertex: boolean read GetColorPerVertex write SetColorPerVertex;
  end;

  TAbstractNBodyCollidableNodeHelper = class helper for TAbstractNBodyCollidableNode
  private
    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
    function GetRotation: TVector4Single;
    procedure SetRotation(const Value: TVector4Single);
    function GetTranslation: TVector3Single;
    procedure SetTranslation(const Value: TVector3Single);
    function GetBboxCenter: TVector3Single;
    procedure SetBboxCenter(const Value: TVector3Single);
    function GetBboxSize: TVector3Single;
    procedure SetBboxSize(const Value: TVector3Single);
  public
    property Enabled: boolean read GetEnabled write SetEnabled;
    property Rotation: TVector4Single read GetRotation write SetRotation;
    property Translation: TVector3Single read GetTranslation write SetTranslation;
    property BboxCenter: TVector3Single read GetBboxCenter write SetBboxCenter;
    property BboxSize: TVector3Single read GetBboxSize write SetBboxSize;
  end;

  TAbstractNBodyCollisionSpaceNodeHelper = class helper for TAbstractNBodyCollisionSpaceNode
  private
    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
    function GetBboxCenter: TVector3Single;
    procedure SetBboxCenter(const Value: TVector3Single);
    function GetBboxSize: TVector3Single;
    procedure SetBboxSize(const Value: TVector3Single);
  public
    property Enabled: boolean read GetEnabled write SetEnabled;
    property BboxCenter: TVector3Single read GetBboxCenter write SetBboxCenter;
    property BboxSize: TVector3Single read GetBboxSize write SetBboxSize;
  end;

  TBallJointNodeHelper = class helper for TBallJointNode
  private
    function GetAnchorPoint: TVector3Single;
    procedure SetAnchorPoint(const Value: TVector3Single);
  public
    property AnchorPoint: TVector3Single read GetAnchorPoint write SetAnchorPoint;
  end;

  TCollisionCollectionNodeHelper = class helper for TCollisionCollectionNode
  private
    function GetBounce: Single;
    procedure SetBounce(const Value: Single);
    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
    function GetFrictionCoefficients: TVector2Single;
    procedure SetFrictionCoefficients(const Value: TVector2Single);
    function GetMinBounceSpeed: Single;
    procedure SetMinBounceSpeed(const Value: Single);
    function GetSlipFactors: TVector2Single;
    procedure SetSlipFactors(const Value: TVector2Single);
    function GetSoftnessConstantForceMix: Single;
    procedure SetSoftnessConstantForceMix(const Value: Single);
    function GetSoftnessErrorCorrection: Single;
    procedure SetSoftnessErrorCorrection(const Value: Single);
    function GetSurfaceSpeed: TVector2Single;
    procedure SetSurfaceSpeed(const Value: TVector2Single);
  public
    property Bounce: Single read GetBounce write SetBounce;
    property Enabled: boolean read GetEnabled write SetEnabled;
    property FrictionCoefficients: TVector2Single read GetFrictionCoefficients write SetFrictionCoefficients;
    property MinBounceSpeed: Single read GetMinBounceSpeed write SetMinBounceSpeed;
    property SlipFactors: TVector2Single read GetSlipFactors write SetSlipFactors;
    property SoftnessConstantForceMix: Single read GetSoftnessConstantForceMix write SetSoftnessConstantForceMix;
    property SoftnessErrorCorrection: Single read GetSoftnessErrorCorrection write SetSoftnessErrorCorrection;
    property SurfaceSpeed: TVector2Single read GetSurfaceSpeed write SetSurfaceSpeed;
  end;

  TCollisionSpaceNodeHelper = class helper for TCollisionSpaceNode
  private
    function GetUseGeometry: boolean;
    procedure SetUseGeometry(const Value: boolean);
  public
    property UseGeometry: boolean read GetUseGeometry write SetUseGeometry;
  end;

  TContactNodeHelper = class helper for TContactNode
  private
    function GetBounce: Single;
    procedure SetBounce(const Value: Single);
    function GetContactNormal: TVector3Single;
    procedure SetContactNormal(const Value: TVector3Single);
    function GetDepth: Single;
    procedure SetDepth(const Value: Single);
    function GetFrictionCoefficients: TVector2Single;
    procedure SetFrictionCoefficients(const Value: TVector2Single);
    function GetFrictionDirection: TVector3Single;
    procedure SetFrictionDirection(const Value: TVector3Single);
    function GetMinbounceSpeed: Single;
    procedure SetMinbounceSpeed(const Value: Single);
    function GetPosition: TVector3Single;
    procedure SetPosition(const Value: TVector3Single);
    function GetSlipCoefficients: TVector2Single;
    procedure SetSlipCoefficients(const Value: TVector2Single);
    function GetSoftnessConstantForceMix: Single;
    procedure SetSoftnessConstantForceMix(const Value: Single);
    function GetSoftnessErrorCorrection: Single;
    procedure SetSoftnessErrorCorrection(const Value: Single);
    function GetSurfaceSpeed: TVector2Single;
    procedure SetSurfaceSpeed(const Value: TVector2Single);
  public
    property Bounce: Single read GetBounce write SetBounce;
    property ContactNormal: TVector3Single read GetContactNormal write SetContactNormal;
    property Depth: Single read GetDepth write SetDepth;
    property FrictionCoefficients: TVector2Single read GetFrictionCoefficients write SetFrictionCoefficients;
    property FrictionDirection: TVector3Single read GetFrictionDirection write SetFrictionDirection;
    property MinbounceSpeed: Single read GetMinbounceSpeed write SetMinbounceSpeed;
    property Position: TVector3Single read GetPosition write SetPosition;
    property SlipCoefficients: TVector2Single read GetSlipCoefficients write SetSlipCoefficients;
    property SoftnessConstantForceMix: Single read GetSoftnessConstantForceMix write SetSoftnessConstantForceMix;
    property SoftnessErrorCorrection: Single read GetSoftnessErrorCorrection write SetSoftnessErrorCorrection;
    property SurfaceSpeed: TVector2Single read GetSurfaceSpeed write SetSurfaceSpeed;
  end;

  TDoubleAxisHingeJointNodeHelper = class helper for TDoubleAxisHingeJointNode
  private
    function GetAnchorPoint: TVector3Single;
    procedure SetAnchorPoint(const Value: TVector3Single);
    function GetAxis1: TVector3Single;
    procedure SetAxis1(const Value: TVector3Single);
    function GetAxis2: TVector3Single;
    procedure SetAxis2(const Value: TVector3Single);
    function GetDesiredAngularVelocity1: Single;
    procedure SetDesiredAngularVelocity1(const Value: Single);
    function GetDesiredAngularVelocity2: Single;
    procedure SetDesiredAngularVelocity2(const Value: Single);
    function GetMaxAngle1: Single;
    procedure SetMaxAngle1(const Value: Single);
    function GetMaxTorque1: Single;
    procedure SetMaxTorque1(const Value: Single);
    function GetMaxTorque2: Single;
    procedure SetMaxTorque2(const Value: Single);
    function GetMinAngle1: Single;
    procedure SetMinAngle1(const Value: Single);
    function GetStopBounce1: Single;
    procedure SetStopBounce1(const Value: Single);
    function GetStopConstantForceMix1: Single;
    procedure SetStopConstantForceMix1(const Value: Single);
    function GetStopErrorCorrection1: Single;
    procedure SetStopErrorCorrection1(const Value: Single);
    function GetSuspensionErrorCorrection: Single;
    procedure SetSuspensionErrorCorrection(const Value: Single);
    function GetSuspensionForce: Single;
    procedure SetSuspensionForce(const Value: Single);
  public
    property AnchorPoint: TVector3Single read GetAnchorPoint write SetAnchorPoint;
    property Axis1: TVector3Single read GetAxis1 write SetAxis1;
    property Axis2: TVector3Single read GetAxis2 write SetAxis2;
    property DesiredAngularVelocity1: Single read GetDesiredAngularVelocity1 write SetDesiredAngularVelocity1;
    property DesiredAngularVelocity2: Single read GetDesiredAngularVelocity2 write SetDesiredAngularVelocity2;
    property MaxAngle1: Single read GetMaxAngle1 write SetMaxAngle1;
    property MaxTorque1: Single read GetMaxTorque1 write SetMaxTorque1;
    property MaxTorque2: Single read GetMaxTorque2 write SetMaxTorque2;
    property MinAngle1: Single read GetMinAngle1 write SetMinAngle1;
    property StopBounce1: Single read GetStopBounce1 write SetStopBounce1;
    property StopConstantForceMix1: Single read GetStopConstantForceMix1 write SetStopConstantForceMix1;
    property StopErrorCorrection1: Single read GetStopErrorCorrection1 write SetStopErrorCorrection1;
    property SuspensionErrorCorrection: Single read GetSuspensionErrorCorrection write SetSuspensionErrorCorrection;
    property SuspensionForce: Single read GetSuspensionForce write SetSuspensionForce;
  end;

  TMotorJointNodeHelper = class helper for TMotorJointNode
  private
    function GetAxis1Angle: Single;
    procedure SetAxis1Angle(const Value: Single);
    function GetAxis1Torque: Single;
    procedure SetAxis1Torque(const Value: Single);
    function GetAxis2Angle: Single;
    procedure SetAxis2Angle(const Value: Single);
    function GetAxis2Torque: Single;
    procedure SetAxis2Torque(const Value: Single);
    function GetAxis3Angle: Single;
    procedure SetAxis3Angle(const Value: Single);
    function GetAxis3Torque: Single;
    procedure SetAxis3Torque(const Value: Single);
    function GetEnabledAxes: Integer;
    procedure SetEnabledAxes(const Value: Integer);
    function GetMotor1Axis: TVector3Single;
    procedure SetMotor1Axis(const Value: TVector3Single);
    function GetMotor2Axis: TVector3Single;
    procedure SetMotor2Axis(const Value: TVector3Single);
    function GetMotor3Axis: TVector3Single;
    procedure SetMotor3Axis(const Value: TVector3Single);
    function GetStop1Bounce: Single;
    procedure SetStop1Bounce(const Value: Single);
    function GetStop1ErrorCorrection: Single;
    procedure SetStop1ErrorCorrection(const Value: Single);
    function GetStop2Bounce: Single;
    procedure SetStop2Bounce(const Value: Single);
    function GetStop2ErrorCorrection: Single;
    procedure SetStop2ErrorCorrection(const Value: Single);
    function GetStop3Bounce: Single;
    procedure SetStop3Bounce(const Value: Single);
    function GetStop3ErrorCorrection: Single;
    procedure SetStop3ErrorCorrection(const Value: Single);
    function GetAutoCalc: boolean;
    procedure SetAutoCalc(const Value: boolean);
  public
    property Axis1Angle: Single read GetAxis1Angle write SetAxis1Angle;
    property Axis1Torque: Single read GetAxis1Torque write SetAxis1Torque;
    property Axis2Angle: Single read GetAxis2Angle write SetAxis2Angle;
    property Axis2Torque: Single read GetAxis2Torque write SetAxis2Torque;
    property Axis3Angle: Single read GetAxis3Angle write SetAxis3Angle;
    property Axis3Torque: Single read GetAxis3Torque write SetAxis3Torque;
    property EnabledAxes: Integer read GetEnabledAxes write SetEnabledAxes;
    property Motor1Axis: TVector3Single read GetMotor1Axis write SetMotor1Axis;
    property Motor2Axis: TVector3Single read GetMotor2Axis write SetMotor2Axis;
    property Motor3Axis: TVector3Single read GetMotor3Axis write SetMotor3Axis;
    property Stop1Bounce: Single read GetStop1Bounce write SetStop1Bounce;
    property Stop1ErrorCorrection: Single read GetStop1ErrorCorrection write SetStop1ErrorCorrection;
    property Stop2Bounce: Single read GetStop2Bounce write SetStop2Bounce;
    property Stop2ErrorCorrection: Single read GetStop2ErrorCorrection write SetStop2ErrorCorrection;
    property Stop3Bounce: Single read GetStop3Bounce write SetStop3Bounce;
    property Stop3ErrorCorrection: Single read GetStop3ErrorCorrection write SetStop3ErrorCorrection;
    property AutoCalc: boolean read GetAutoCalc write SetAutoCalc;
  end;

  TRigidBodyNodeHelper = class helper for TRigidBodyNode
  private
    function GetAngularDampingFactor: Single;
    procedure SetAngularDampingFactor(const Value: Single);
    function GetAngularVelocity: TVector3Single;
    procedure SetAngularVelocity(const Value: TVector3Single);
    function GetAutoDamp: boolean;
    procedure SetAutoDamp(const Value: boolean);
    function GetAutoDisable: boolean;
    procedure SetAutoDisable(const Value: boolean);
    function GetCenterOfMass: TVector3Single;
    procedure SetCenterOfMass(const Value: TVector3Single);
    function GetDisableAngularSpeed: Single;
    procedure SetDisableAngularSpeed(const Value: Single);
    function GetDisableLinearSpeed: Single;
    procedure SetDisableLinearSpeed(const Value: Single);
    function GetDisableTime: Single;
    procedure SetDisableTime(const Value: Single);
    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
    function GetFiniteRotationAxis: TVector3Single;
    procedure SetFiniteRotationAxis(const Value: TVector3Single);
    function GetFixed: boolean;
    procedure SetFixed(const Value: boolean);
    function GetInertia: TMatrix3Single;
    procedure SetInertia(const Value: TMatrix3Single);
    function GetLinearDampingFactor: Single;
    procedure SetLinearDampingFactor(const Value: Single);
    function GetLinearVelocity: TVector3Single;
    procedure SetLinearVelocity(const Value: TVector3Single);
    function GetMass: Single;
    procedure SetMass(const Value: Single);
    function GetOrientation: TVector4Single;
    procedure SetOrientation(const Value: TVector4Single);
    function GetPosition: TVector3Single;
    procedure SetPosition(const Value: TVector3Single);
    function GetUseFiniteRotation: boolean;
    procedure SetUseFiniteRotation(const Value: boolean);
    function GetUseGlobalGravity: boolean;
    procedure SetUseGlobalGravity(const Value: boolean);
  public
    property AngularDampingFactor: Single read GetAngularDampingFactor write SetAngularDampingFactor;
    property AngularVelocity: TVector3Single read GetAngularVelocity write SetAngularVelocity;
    property AutoDamp: boolean read GetAutoDamp write SetAutoDamp;
    property AutoDisable: boolean read GetAutoDisable write SetAutoDisable;
    property CenterOfMass: TVector3Single read GetCenterOfMass write SetCenterOfMass;
    property DisableAngularSpeed: Single read GetDisableAngularSpeed write SetDisableAngularSpeed;
    property DisableLinearSpeed: Single read GetDisableLinearSpeed write SetDisableLinearSpeed;
    property DisableTime: Single read GetDisableTime write SetDisableTime;
    property Enabled: boolean read GetEnabled write SetEnabled;
    property FiniteRotationAxis: TVector3Single read GetFiniteRotationAxis write SetFiniteRotationAxis;
    property Fixed: boolean read GetFixed write SetFixed;
    property Inertia: TMatrix3Single read GetInertia write SetInertia;
    property LinearDampingFactor: Single read GetLinearDampingFactor write SetLinearDampingFactor;
    property LinearVelocity: TVector3Single read GetLinearVelocity write SetLinearVelocity;
    property Mass: Single read GetMass write SetMass;
    property Orientation: TVector4Single read GetOrientation write SetOrientation;
    property Position: TVector3Single read GetPosition write SetPosition;
    property UseFiniteRotation: boolean read GetUseFiniteRotation write SetUseFiniteRotation;
    property UseGlobalGravity: boolean read GetUseGlobalGravity write SetUseGlobalGravity;
  end;

  TRigidBodyCollectionNodeHelper = class helper for TRigidBodyCollectionNode
  private
    function GetAutoDisable: boolean;
    procedure SetAutoDisable(const Value: boolean);
    function GetConstantForceMix: Single;
    procedure SetConstantForceMix(const Value: Single);
    function GetContactSurfaceThickness: Single;
    procedure SetContactSurfaceThickness(const Value: Single);
    function GetDisableAngularSpeed: Single;
    procedure SetDisableAngularSpeed(const Value: Single);
    function GetDisableLinearSpeed: Single;
    procedure SetDisableLinearSpeed(const Value: Single);
    function GetDisableTime: Single;
    procedure SetDisableTime(const Value: Single);
    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
    function GetErrorCorrection: Single;
    procedure SetErrorCorrection(const Value: Single);
    function GetGravity: TVector3Single;
    procedure SetGravity(const Value: TVector3Single);
    function GetIterations: Integer;
    procedure SetIterations(const Value: Integer);
    function GetMaxCorrectionSpeed: Single;
    procedure SetMaxCorrectionSpeed(const Value: Single);
    function GetPreferAccuracy: boolean;
    procedure SetPreferAccuracy(const Value: boolean);
  public
    property AutoDisable: boolean read GetAutoDisable write SetAutoDisable;
    property ConstantForceMix: Single read GetConstantForceMix write SetConstantForceMix;
    property ContactSurfaceThickness: Single read GetContactSurfaceThickness write SetContactSurfaceThickness;
    property DisableAngularSpeed: Single read GetDisableAngularSpeed write SetDisableAngularSpeed;
    property DisableLinearSpeed: Single read GetDisableLinearSpeed write SetDisableLinearSpeed;
    property DisableTime: Single read GetDisableTime write SetDisableTime;
    property Enabled: boolean read GetEnabled write SetEnabled;
    property ErrorCorrection: Single read GetErrorCorrection write SetErrorCorrection;
    property Gravity: TVector3Single read GetGravity write SetGravity;
    property Iterations: Integer read GetIterations write SetIterations;
    property MaxCorrectionSpeed: Single read GetMaxCorrectionSpeed write SetMaxCorrectionSpeed;
    property PreferAccuracy: boolean read GetPreferAccuracy write SetPreferAccuracy;
  end;

  TSingleAxisHingeJointNodeHelper = class helper for TSingleAxisHingeJointNode
  private
    function GetAnchorPoint: TVector3Single;
    procedure SetAnchorPoint(const Value: TVector3Single);
    function GetAxis: TVector3Single;
    procedure SetAxis(const Value: TVector3Single);
    function GetMaxAngle: Single;
    procedure SetMaxAngle(const Value: Single);
    function GetMinAngle: Single;
    procedure SetMinAngle(const Value: Single);
    function GetStopBounce: Single;
    procedure SetStopBounce(const Value: Single);
    function GetStopErrorCorrection: Single;
    procedure SetStopErrorCorrection(const Value: Single);
  public
    property AnchorPoint: TVector3Single read GetAnchorPoint write SetAnchorPoint;
    property Axis: TVector3Single read GetAxis write SetAxis;
    property MaxAngle: Single read GetMaxAngle write SetMaxAngle;
    property MinAngle: Single read GetMinAngle write SetMinAngle;
    property StopBounce: Single read GetStopBounce write SetStopBounce;
    property StopErrorCorrection: Single read GetStopErrorCorrection write SetStopErrorCorrection;
  end;

  TSliderJointNodeHelper = class helper for TSliderJointNode
  private
    function GetAxis: TVector3Single;
    procedure SetAxis(const Value: TVector3Single);
    function GetMaxSeparation: Single;
    procedure SetMaxSeparation(const Value: Single);
    function GetMinSeparation: Single;
    procedure SetMinSeparation(const Value: Single);
    function GetStopBounce: Single;
    procedure SetStopBounce(const Value: Single);
    function GetStopErrorCorrection: Single;
    procedure SetStopErrorCorrection(const Value: Single);
  public
    property Axis: TVector3Single read GetAxis write SetAxis;
    property MaxSeparation: Single read GetMaxSeparation write SetMaxSeparation;
    property MinSeparation: Single read GetMinSeparation write SetMinSeparation;
    property StopBounce: Single read GetStopBounce write SetStopBounce;
    property StopErrorCorrection: Single read GetStopErrorCorrection write SetStopErrorCorrection;
  end;

  TUniversalJointNodeHelper = class helper for TUniversalJointNode
  private
    function GetAnchorPoint: TVector3Single;
    procedure SetAnchorPoint(const Value: TVector3Single);
    function GetAxis1: TVector3Single;
    procedure SetAxis1(const Value: TVector3Single);
    function GetAxis2: TVector3Single;
    procedure SetAxis2(const Value: TVector3Single);
    function GetStopBounce1: Single;
    procedure SetStopBounce1(const Value: Single);
    function GetStop1ErrorCorrection: Single;
    procedure SetStop1ErrorCorrection(const Value: Single);
    function GetStop2Bounce: Single;
    procedure SetStop2Bounce(const Value: Single);
    function GetStop2ErrorCorrection: Single;
    procedure SetStop2ErrorCorrection(const Value: Single);
  public
    property AnchorPoint: TVector3Single read GetAnchorPoint write SetAnchorPoint;
    property Axis1: TVector3Single read GetAxis1 write SetAxis1;
    property Axis2: TVector3Single read GetAxis2 write SetAxis2;
    property StopBounce1: Single read GetStopBounce1 write SetStopBounce1;
    property Stop1ErrorCorrection: Single read GetStop1ErrorCorrection write SetStop1ErrorCorrection;
    property Stop2Bounce: Single read GetStop2Bounce write SetStop2Bounce;
    property Stop2ErrorCorrection: Single read GetStop2ErrorCorrection write SetStop2ErrorCorrection;
  end;

  TScriptNodeHelper = class helper for TScriptNode
  private
    function GetDirectOutput: boolean;
    procedure SetDirectOutput(const Value: boolean);
    function GetMustEvaluate: boolean;
    procedure SetMustEvaluate(const Value: boolean);
  public
    property DirectOutput: boolean read GetDirectOutput write SetDirectOutput;
    property MustEvaluate: boolean read GetMustEvaluate write SetMustEvaluate;
  end;

  TAbstractVertexAttributeNodeHelper = class helper for TAbstractVertexAttributeNode
  private
    function GetNameField: string;
    procedure SetNameField(const Value: string);
  public
    property NameField: string read GetNameField write SetNameField;
  end;

  TFloatVertexAttributeNodeHelper = class helper for TFloatVertexAttributeNode
  private
    function GetNumComponents: Integer;
    procedure SetNumComponents(const Value: Integer);
  public
    property NumComponents: Integer read GetNumComponents write SetNumComponents;
  end;

  TAbstractShapeNodeHelper = class helper for TAbstractShapeNode
  private
    function GetBboxCenter: TVector3Single;
    procedure SetBboxCenter(const Value: TVector3Single);
    function GetBboxSize: TVector3Single;
    procedure SetBboxSize(const Value: TVector3Single);
    function GetRender: boolean;
    procedure SetRender(const Value: boolean);
  public
    property BboxCenter: TVector3Single read GetBboxCenter write SetBboxCenter;
    property BboxSize: TVector3Single read GetBboxSize write SetBboxSize;
    property Render: boolean read GetRender write SetRender;
  end;

  TFillPropertiesNodeHelper = class helper for TFillPropertiesNode
  private
    function GetFilled: boolean;
    procedure SetFilled(const Value: boolean);
    function GetHatchColor: TCastleColorRGB;
    procedure SetHatchColor(const Value: TCastleColorRGB);
    function GetHatched: boolean;
    procedure SetHatched(const Value: boolean);
    function GetHatchStyle: Integer;
    procedure SetHatchStyle(const Value: Integer);
  public
    property Filled: boolean read GetFilled write SetFilled;
    property HatchColor: TCastleColorRGB read GetHatchColor write SetHatchColor;
    property Hatched: boolean read GetHatched write SetHatched;
    property HatchStyle: Integer read GetHatchStyle write SetHatchStyle;
  end;

  TLinePropertiesNodeHelper = class helper for TLinePropertiesNode
  private
    function GetApplied: boolean;
    procedure SetApplied(const Value: boolean);
    function GetLinewidthScaleFactor: Single;
    procedure SetLinewidthScaleFactor(const Value: Single);
  public
    property Applied: boolean read GetApplied write SetApplied;
    property LinewidthScaleFactor: Single read GetLinewidthScaleFactor write SetLinewidthScaleFactor;
  end;

  TMaterialNodeHelper = class helper for TMaterialNode
  private
    function GetAmbientIntensity: Single;
    procedure SetAmbientIntensity(const Value: Single);
    function GetDiffuseColor: TCastleColorRGB;
    procedure SetDiffuseColor(const Value: TCastleColorRGB);
    function GetEmissiveColor: TCastleColorRGB;
    procedure SetEmissiveColor(const Value: TCastleColorRGB);
    function GetShininess: Single;
    procedure SetShininess(const Value: Single);
    function GetSpecularColor: TCastleColorRGB;
    procedure SetSpecularColor(const Value: TCastleColorRGB);
    function GetTransparency: Single;
    procedure SetTransparency(const Value: Single);
    function GetMirror: Single;
    procedure SetMirror(const Value: Single);
    function GetReflSpecularExp: Single;
    procedure SetReflSpecularExp(const Value: Single);
    function GetTransSpecularExp: Single;
    procedure SetTransSpecularExp(const Value: Single);
  public
    property AmbientIntensity: Single read GetAmbientIntensity write SetAmbientIntensity;
    property DiffuseColor: TCastleColorRGB read GetDiffuseColor write SetDiffuseColor;
    property EmissiveColor: TCastleColorRGB read GetEmissiveColor write SetEmissiveColor;
    property Shininess: Single read GetShininess write SetShininess;
    property SpecularColor: TCastleColorRGB read GetSpecularColor write SetSpecularColor;
    property Transparency: Single read GetTransparency write SetTransparency;
    property Mirror: Single read GetMirror write SetMirror;
    property ReflSpecularExp: Single read GetReflSpecularExp write SetReflSpecularExp;
    property TransSpecularExp: Single read GetTransSpecularExp write SetTransSpecularExp;
  end;

  TTwoSidedMaterialNodeHelper = class helper for TTwoSidedMaterialNode
  private
    function GetAmbientIntensity: Single;
    procedure SetAmbientIntensity(const Value: Single);
    function GetBackAmbientIntensity: Single;
    procedure SetBackAmbientIntensity(const Value: Single);
    function GetBackDiffuseColor: TCastleColorRGB;
    procedure SetBackDiffuseColor(const Value: TCastleColorRGB);
    function GetBackEmissiveColor: TCastleColorRGB;
    procedure SetBackEmissiveColor(const Value: TCastleColorRGB);
    function GetBackShininess: Single;
    procedure SetBackShininess(const Value: Single);
    function GetBackSpecularColor: TCastleColorRGB;
    procedure SetBackSpecularColor(const Value: TCastleColorRGB);
    function GetBackTransparency: Single;
    procedure SetBackTransparency(const Value: Single);
    function GetDiffuseColor: TCastleColorRGB;
    procedure SetDiffuseColor(const Value: TCastleColorRGB);
    function GetEmissiveColor: TCastleColorRGB;
    procedure SetEmissiveColor(const Value: TCastleColorRGB);
    function GetShininess: Single;
    procedure SetShininess(const Value: Single);
    function GetSeparateBackColor: boolean;
    procedure SetSeparateBackColor(const Value: boolean);
    function GetSpecularColor: TCastleColorRGB;
    procedure SetSpecularColor(const Value: TCastleColorRGB);
    function GetTransparency: Single;
    procedure SetTransparency(const Value: Single);
  public
    property AmbientIntensity: Single read GetAmbientIntensity write SetAmbientIntensity;
    property BackAmbientIntensity: Single read GetBackAmbientIntensity write SetBackAmbientIntensity;
    property BackDiffuseColor: TCastleColorRGB read GetBackDiffuseColor write SetBackDiffuseColor;
    property BackEmissiveColor: TCastleColorRGB read GetBackEmissiveColor write SetBackEmissiveColor;
    property BackShininess: Single read GetBackShininess write SetBackShininess;
    property BackSpecularColor: TCastleColorRGB read GetBackSpecularColor write SetBackSpecularColor;
    property BackTransparency: Single read GetBackTransparency write SetBackTransparency;
    property DiffuseColor: TCastleColorRGB read GetDiffuseColor write SetDiffuseColor;
    property EmissiveColor: TCastleColorRGB read GetEmissiveColor write SetEmissiveColor;
    property Shininess: Single read GetShininess write SetShininess;
    property SeparateBackColor: boolean read GetSeparateBackColor write SetSeparateBackColor;
    property SpecularColor: TCastleColorRGB read GetSpecularColor write SetSpecularColor;
    property Transparency: Single read GetTransparency write SetTransparency;
  end;

  TAbstractSoundSourceNodeHelper = class helper for TAbstractSoundSourceNode
  private
    function GetDescription: string;
    procedure SetDescription(const Value: string);
    function GetPitch: Single;
    procedure SetPitch(const Value: Single);
  public
    property Description: string read GetDescription write SetDescription;
    property Pitch: Single read GetPitch write SetPitch;
  end;

  TSoundNodeHelper = class helper for TSoundNode
  private
    function GetDirection: TVector3Single;
    procedure SetDirection(const Value: TVector3Single);
    function GetIntensity: Single;
    procedure SetIntensity(const Value: Single);
    function GetLocation: TVector3Single;
    procedure SetLocation(const Value: TVector3Single);
    function GetMaxBack: Single;
    procedure SetMaxBack(const Value: Single);
    function GetMaxFront: Single;
    procedure SetMaxFront(const Value: Single);
    function GetMinBack: Single;
    procedure SetMinBack(const Value: Single);
    function GetMinFront: Single;
    procedure SetMinFront(const Value: Single);
    function GetPriority: Single;
    procedure SetPriority(const Value: Single);
    function GetSpatialize: boolean;
    procedure SetSpatialize(const Value: boolean);
  public
    property Direction: TVector3Single read GetDirection write SetDirection;
    property Intensity: Single read GetIntensity write SetIntensity;
    property Location: TVector3Single read GetLocation write SetLocation;
    property MaxBack: Single read GetMaxBack write SetMaxBack;
    property MaxFront: Single read GetMaxFront write SetMaxFront;
    property MinBack: Single read GetMinBack write SetMinBack;
    property MinFront: Single read GetMinFront write SetMinFront;
    property Priority: Single read GetPriority write SetPriority;
    property Spatialize: boolean read GetSpatialize write SetSpatialize;
  end;

  TFontStyleNodeHelper = class helper for TFontStyleNode
  private
    function GetHorizontal: boolean;
    procedure SetHorizontal(const Value: boolean);
    function GetLanguage: string;
    procedure SetLanguage(const Value: string);
    function GetLeftToRight: boolean;
    procedure SetLeftToRight(const Value: boolean);
    function GetSize: Single;
    procedure SetSize(const Value: Single);
    function GetSpacing: Single;
    procedure SetSpacing(const Value: Single);
    function GetTopToBottom: boolean;
    procedure SetTopToBottom(const Value: boolean);
  public
    property Horizontal: boolean read GetHorizontal write SetHorizontal;
    property Language: string read GetLanguage write SetLanguage;
    property LeftToRight: boolean read GetLeftToRight write SetLeftToRight;
    property Size: Single read GetSize write SetSize;
    property Spacing: Single read GetSpacing write SetSpacing;
    property TopToBottom: boolean read GetTopToBottom write SetTopToBottom;
  end;

  TTextNodeHelper = class helper for TTextNode
  private
    function GetMaxExtent: Single;
    procedure SetMaxExtent(const Value: Single);
  public
    property MaxExtent: Single read GetMaxExtent write SetMaxExtent;
  end;

  TAbstractTexture3DNodeHelper = class helper for TAbstractTexture3DNode
  private
    function GetRepeatR: boolean;
    procedure SetRepeatR(const Value: boolean);
  public
    property RepeatR: boolean read GetRepeatR write SetRepeatR;
  end;

  TTextureTransformMatrix3DNodeHelper = class helper for TTextureTransformMatrix3DNode
  private
    function GetMatrix: TMatrix4Single;
    procedure SetMatrix(const Value: TMatrix4Single);
  public
    property Matrix: TMatrix4Single read GetMatrix write SetMatrix;
  end;

  TTextureTransform3DNodeHelper = class helper for TTextureTransform3DNode
  private
    function GetCenter: TVector3Single;
    procedure SetCenter(const Value: TVector3Single);
    function GetRotation: TVector4Single;
    procedure SetRotation(const Value: TVector4Single);
    function GetScale: TVector3Single;
    procedure SetScale(const Value: TVector3Single);
    function GetTranslation: TVector3Single;
    procedure SetTranslation(const Value: TVector3Single);
  public
    property Center: TVector3Single read GetCenter write SetCenter;
    property Rotation: TVector4Single read GetRotation write SetRotation;
    property Scale: TVector3Single read GetScale write SetScale;
    property Translation: TVector3Single read GetTranslation write SetTranslation;
  end;

  TMovieTextureNodeHelper = class helper for TMovieTextureNode
  private
    function GetDescription: string;
    procedure SetDescription(const Value: string);
    function GetLoop: boolean;
    procedure SetLoop(const Value: boolean);
    function GetPauseTime: TFloatTime;
    procedure SetPauseTime(const Value: TFloatTime);
    function GetResumeTime: TFloatTime;
    procedure SetResumeTime(const Value: TFloatTime);
    function GetSpeed: Single;
    procedure SetSpeed(const Value: Single);
    function GetStartTime: TFloatTime;
    procedure SetStartTime(const Value: TFloatTime);
    function GetStopTime: TFloatTime;
    procedure SetStopTime(const Value: TFloatTime);
  public
    property Description: string read GetDescription write SetDescription;
    property Loop: boolean read GetLoop write SetLoop;
    property PauseTime: TFloatTime read GetPauseTime write SetPauseTime;
    property ResumeTime: TFloatTime read GetResumeTime write SetResumeTime;
    property Speed: Single read GetSpeed write SetSpeed;
    property StartTime: TFloatTime read GetStartTime write SetStartTime;
    property StopTime: TFloatTime read GetStopTime write SetStopTime;
  end;

  TMultiTextureNodeHelper = class helper for TMultiTextureNode
  private
    function GetAlpha: Single;
    procedure SetAlpha(const Value: Single);
    function GetColor: TCastleColorRGB;
    procedure SetColor(const Value: TCastleColorRGB);
  public
    property Alpha: Single read GetAlpha write SetAlpha;
    property Color: TCastleColorRGB read GetColor write SetColor;
  end;

  TTextureCoordinateGeneratorNodeHelper = class helper for TTextureCoordinateGeneratorNode
  private
    function GetMode: string;
    procedure SetMode(const Value: string);
  public
    property Mode: string read GetMode write SetMode;
  end;

  TTexturePropertiesNodeHelper = class helper for TTexturePropertiesNode
  private
    function GetAnisotropicDegree: Single;
    procedure SetAnisotropicDegree(const Value: Single);
    function GetBorderColor: TCastleColor;
    procedure SetBorderColor(const Value: TCastleColor);
    function GetBorderWidth: Integer;
    procedure SetBorderWidth(const Value: Integer);
    function GetBoundaryModeS: string;
    procedure SetBoundaryModeS(const Value: string);
    function GetBoundaryModeT: string;
    procedure SetBoundaryModeT(const Value: string);
    function GetBoundaryModeR: string;
    procedure SetBoundaryModeR(const Value: string);
    function GetMagnificationFilter: string;
    procedure SetMagnificationFilter(const Value: string);
    function GetMinificationFilter: string;
    procedure SetMinificationFilter(const Value: string);
    function GetTextureCompression: string;
    procedure SetTextureCompression(const Value: string);
    function GetTexturePriority: Single;
    procedure SetTexturePriority(const Value: Single);
    function GetGenerateMipMaps: boolean;
    procedure SetGenerateMipMaps(const Value: boolean);
  public
    property AnisotropicDegree: Single read GetAnisotropicDegree write SetAnisotropicDegree;
    property BorderColor: TCastleColor read GetBorderColor write SetBorderColor;
    property BorderWidth: Integer read GetBorderWidth write SetBorderWidth;
    property BoundaryModeS: string read GetBoundaryModeS write SetBoundaryModeS;
    property BoundaryModeT: string read GetBoundaryModeT write SetBoundaryModeT;
    property BoundaryModeR: string read GetBoundaryModeR write SetBoundaryModeR;
    property MagnificationFilter: string read GetMagnificationFilter write SetMagnificationFilter;
    property MinificationFilter: string read GetMinificationFilter write SetMinificationFilter;
    property TextureCompression: string read GetTextureCompression write SetTextureCompression;
    property TexturePriority: Single read GetTexturePriority write SetTexturePriority;
    property GenerateMipMaps: boolean read GetGenerateMipMaps write SetGenerateMipMaps;
  end;

  TTextureTransformNodeHelper = class helper for TTextureTransformNode
  private
    function GetCenter: TVector2Single;
    procedure SetCenter(const Value: TVector2Single);
    function GetRotation: Single;
    procedure SetRotation(const Value: Single);
    function GetScale: TVector2Single;
    procedure SetScale(const Value: TVector2Single);
    function GetTranslation: TVector2Single;
    procedure SetTranslation(const Value: TVector2Single);
  public
    property Center: TVector2Single read GetCenter write SetCenter;
    property Rotation: Single read GetRotation write SetRotation;
    property Scale: TVector2Single read GetScale write SetScale;
    property Translation: TVector2Single read GetTranslation write SetTranslation;
  end;

  TAbstractTimeDependentNodeHelper = class helper for TAbstractTimeDependentNode
  private
    function GetLoop: boolean;
    procedure SetLoop(const Value: boolean);
    function GetPauseTime: TFloatTime;
    procedure SetPauseTime(const Value: TFloatTime);
    function GetResumeTime: TFloatTime;
    procedure SetResumeTime(const Value: TFloatTime);
    function GetStartTime: TFloatTime;
    procedure SetStartTime(const Value: TFloatTime);
    function GetStopTime: TFloatTime;
    procedure SetStopTime(const Value: TFloatTime);
  public
    property Loop: boolean read GetLoop write SetLoop;
    property PauseTime: TFloatTime read GetPauseTime write SetPauseTime;
    property ResumeTime: TFloatTime read GetResumeTime write SetResumeTime;
    property StartTime: TFloatTime read GetStartTime write SetStartTime;
    property StopTime: TFloatTime read GetStopTime write SetStopTime;
  end;

  TTimeSensorNodeHelper = class helper for TTimeSensorNode
  private
    function GetCycleInterval: TFloatTime;
    procedure SetCycleInterval(const Value: TFloatTime);
    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
  public
    property CycleInterval: TFloatTime read GetCycleInterval write SetCycleInterval;
    property Enabled: boolean read GetEnabled write SetEnabled;
  end;

  TAppearanceNodeHelper = class helper for TAppearanceNode
  private
    function GetShadowCaster: boolean;
    procedure SetShadowCaster(const Value: boolean);
    function GetHeightMapScale: Single;
    procedure SetHeightMapScale(const Value: Single);
  public
    property ShadowCaster: boolean read GetShadowCaster write SetShadowCaster;
    property HeightMapScale: Single read GetHeightMapScale write SetHeightMapScale;
  end;

  TGeneratedShadowMapNodeHelper = class helper for TGeneratedShadowMapNode
  private
    function GetSize: Integer;
    procedure SetSize(const Value: Integer);
    function GetScale: Single;
    procedure SetScale(const Value: Single);
    function GetBias: Single;
    procedure SetBias(const Value: Single);
  public
    property Size: Integer read GetSize write SetSize;
    property Scale: Single read GetScale write SetScale;
    property Bias: Single read GetBias write SetBias;
  end;

  TText3DNodeHelper = class helper for TText3DNode
  private
    function GetMaxExtent: Single;
    procedure SetMaxExtent(const Value: Single);
    function GetDepth: Single;
    procedure SetDepth(const Value: Single);
  public
    property MaxExtent: Single read GetMaxExtent write SetMaxExtent;
    property Depth: Single read GetDepth write SetDepth;
  end;

  TKambiNavigationInfoNodeHelper = class helper for TKambiNavigationInfoNode
  private
    function GetTimeOriginAtLoad: boolean;
    procedure SetTimeOriginAtLoad(const Value: boolean);
    function GetHeadBobbing: Single;
    procedure SetHeadBobbing(const Value: Single);
    function GetHeadBobbingTime: Single;
    procedure SetHeadBobbingTime(const Value: Single);
  public
    property TimeOriginAtLoad: boolean read GetTimeOriginAtLoad write SetTimeOriginAtLoad;
    property HeadBobbing: Single read GetHeadBobbing write SetHeadBobbing;
    property HeadBobbingTime: Single read GetHeadBobbingTime write SetHeadBobbingTime;
  end;

  TKambiHeadLightNodeHelper = class helper for TKambiHeadLightNode
  private
    function GetAmbientIntensity: Single;
    procedure SetAmbientIntensity(const Value: Single);
    function GetAttenuation: TVector3Single;
    procedure SetAttenuation(const Value: TVector3Single);
    function GetColor: TCastleColorRGB;
    procedure SetColor(const Value: TCastleColorRGB);
    function GetIntensity: Single;
    procedure SetIntensity(const Value: Single);
    function GetSpot: boolean;
    procedure SetSpot(const Value: boolean);
    function GetSpotDropOffRate: Single;
    procedure SetSpotDropOffRate(const Value: Single);
    function GetSpotCutOffAngle: Single;
    procedure SetSpotCutOffAngle(const Value: Single);
  public
    property AmbientIntensity: Single read GetAmbientIntensity write SetAmbientIntensity;
    property Attenuation: TVector3Single read GetAttenuation write SetAttenuation;
    property Color: TCastleColorRGB read GetColor write SetColor;
    property Intensity: Single read GetIntensity write SetIntensity;
    property Spot: boolean read GetSpot write SetSpot;
    property SpotDropOffRate: Single read GetSpotDropOffRate write SetSpotDropOffRate;
    property SpotCutOffAngle: Single read GetSpotCutOffAngle write SetSpotCutOffAngle;
  end;

  TKambiOctreePropertiesNodeHelper = class helper for TKambiOctreePropertiesNode
  private
    function GetMaxDepth: Integer;
    procedure SetMaxDepth(const Value: Integer);
    function GetLeafCapacity: Integer;
    procedure SetLeafCapacity(const Value: Integer);
  public
    property MaxDepth: Integer read GetMaxDepth write SetMaxDepth;
    property LeafCapacity: Integer read GetLeafCapacity write SetLeafCapacity;
  end;

  TTeapotNodeHelper = class helper for TTeapotNode
  private
    function GetSize: TVector3Single;
    procedure SetSize(const Value: TVector3Single);
    function GetManifold: boolean;
    procedure SetManifold(const Value: boolean);
  public
    property Size: TVector3Single read GetSize write SetSize;
    property Manifold: boolean read GetManifold write SetManifold;
  end;

  TRenderedTextureNodeHelper = class helper for TRenderedTextureNode
  private
    function GetRepeatR: boolean;
    procedure SetRepeatR(const Value: boolean);
  public
    property RepeatR: boolean read GetRepeatR write SetRepeatR;
  end;

  TLoggerNodeHelper = class helper for TLoggerNode
  private
    function GetLevel: Integer;
    procedure SetLevel(const Value: Integer);
    function GetLogFile: string;
    procedure SetLogFile(const Value: string);
    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
  public
    property Level: Integer read GetLevel write SetLevel;
    property LogFile: string read GetLogFile write SetLogFile;
    property Enabled: boolean read GetEnabled write SetEnabled;
  end;

  TScreenEffectNodeHelper = class helper for TScreenEffectNode
  private
    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
    function GetNeedsDepth: boolean;
    procedure SetNeedsDepth(const Value: boolean);
  public
    property Enabled: boolean read GetEnabled write SetEnabled;
    property NeedsDepth: boolean read GetNeedsDepth write SetNeedsDepth;
  end;

  TEffectNodeHelper = class helper for TEffectNode
  private
    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
  public
    property Enabled: boolean read GetEnabled write SetEnabled;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TMatrixTransformNode ----------------------------------------------- }

function TMatrixTransformNodeHelper.GetMatrix: TMatrix4Single;
begin
  Result := FdMatrix.Value;
end;

procedure TMatrixTransformNodeHelper.SetMatrix(const Value: TMatrix4Single);
begin
  FdMatrix.Send(Value);
end;

{ TAbstractProductStructureChildNode ----------------------------------------------- }

function TAbstractProductStructureChildNodeHelper.GetNameField: string;
begin
  Result := FdName.Value;
end;

procedure TAbstractProductStructureChildNodeHelper.SetNameField(const Value: string);
begin
  FdName.Send(Value);
end;

{ TCADAssemblyNode ----------------------------------------------- }

function TCADAssemblyNodeHelper.GetNameField: string;
begin
  Result := FdName.Value;
end;

procedure TCADAssemblyNodeHelper.SetNameField(const Value: string);
begin
  FdName.Send(Value);
end;

{ TCADFaceNode ----------------------------------------------- }

function TCADFaceNodeHelper.GetBboxCenter: TVector3Single;
begin
  Result := FdBboxCenter.Value;
end;

procedure TCADFaceNodeHelper.SetBboxCenter(const Value: TVector3Single);
begin
  FdBboxCenter.Send(Value);
end;

function TCADFaceNodeHelper.GetBboxSize: TVector3Single;
begin
  Result := FdBboxSize.Value;
end;

procedure TCADFaceNodeHelper.SetBboxSize(const Value: TVector3Single);
begin
  FdBboxSize.Send(Value);
end;

{ TCADLayerNode ----------------------------------------------- }

function TCADLayerNodeHelper.GetNameField: string;
begin
  Result := FdName.Value;
end;

procedure TCADLayerNodeHelper.SetNameField(const Value: string);
begin
  FdName.Send(Value);
end;

{ TCADPartNode ----------------------------------------------- }

function TCADPartNodeHelper.GetCenter: TVector3Single;
begin
  Result := FdCenter.Value;
end;

procedure TCADPartNodeHelper.SetCenter(const Value: TVector3Single);
begin
  FdCenter.Send(Value);
end;

function TCADPartNodeHelper.GetNameField: string;
begin
  Result := FdName.Value;
end;

procedure TCADPartNodeHelper.SetNameField(const Value: string);
begin
  FdName.Send(Value);
end;

function TCADPartNodeHelper.GetRotation: TVector4Single;
begin
  Result := FdRotation.Value;
end;

procedure TCADPartNodeHelper.SetRotation(const Value: TVector4Single);
begin
  FdRotation.Send(Value);
end;

function TCADPartNodeHelper.GetScale: TVector3Single;
begin
  Result := FdScale.Value;
end;

procedure TCADPartNodeHelper.SetScale(const Value: TVector3Single);
begin
  FdScale.Send(Value);
end;

function TCADPartNodeHelper.GetScaleOrientation: TVector4Single;
begin
  Result := FdScaleOrientation.Value;
end;

procedure TCADPartNodeHelper.SetScaleOrientation(const Value: TVector4Single);
begin
  FdScaleOrientation.Send(Value);
end;

function TCADPartNodeHelper.GetTranslation: TVector3Single;
begin
  Result := FdTranslation.Value;
end;

procedure TCADPartNodeHelper.SetTranslation(const Value: TVector3Single);
begin
  FdTranslation.Send(Value);
end;

{ TAbstractSensorNode ----------------------------------------------- }

function TAbstractSensorNodeHelper.GetEnabled: boolean;
begin
  Result := FdEnabled.Value;
end;

procedure TAbstractSensorNodeHelper.SetEnabled(const Value: boolean);
begin
  FdEnabled.Send(Value);
end;

{ TMetadataDoubleNode ----------------------------------------------- }

function TMetadataDoubleNodeHelper.GetNameField: string;
begin
  Result := FdName.Value;
end;

procedure TMetadataDoubleNodeHelper.SetNameField(const Value: string);
begin
  FdName.Send(Value);
end;

function TMetadataDoubleNodeHelper.GetReference: string;
begin
  Result := FdReference.Value;
end;

procedure TMetadataDoubleNodeHelper.SetReference(const Value: string);
begin
  FdReference.Send(Value);
end;

{ TMetadataFloatNode ----------------------------------------------- }

function TMetadataFloatNodeHelper.GetNameField: string;
begin
  Result := FdName.Value;
end;

procedure TMetadataFloatNodeHelper.SetNameField(const Value: string);
begin
  FdName.Send(Value);
end;

function TMetadataFloatNodeHelper.GetReference: string;
begin
  Result := FdReference.Value;
end;

procedure TMetadataFloatNodeHelper.SetReference(const Value: string);
begin
  FdReference.Send(Value);
end;

{ TMetadataIntegerNode ----------------------------------------------- }

function TMetadataIntegerNodeHelper.GetNameField: string;
begin
  Result := FdName.Value;
end;

procedure TMetadataIntegerNodeHelper.SetNameField(const Value: string);
begin
  FdName.Send(Value);
end;

function TMetadataIntegerNodeHelper.GetReference: string;
begin
  Result := FdReference.Value;
end;

procedure TMetadataIntegerNodeHelper.SetReference(const Value: string);
begin
  FdReference.Send(Value);
end;

{ TMetadataSetNode ----------------------------------------------- }

function TMetadataSetNodeHelper.GetNameField: string;
begin
  Result := FdName.Value;
end;

procedure TMetadataSetNodeHelper.SetNameField(const Value: string);
begin
  FdName.Send(Value);
end;

function TMetadataSetNodeHelper.GetReference: string;
begin
  Result := FdReference.Value;
end;

procedure TMetadataSetNodeHelper.SetReference(const Value: string);
begin
  FdReference.Send(Value);
end;

{ TMetadataStringNode ----------------------------------------------- }

function TMetadataStringNodeHelper.GetNameField: string;
begin
  Result := FdName.Value;
end;

procedure TMetadataStringNodeHelper.SetNameField(const Value: string);
begin
  FdName.Send(Value);
end;

function TMetadataStringNodeHelper.GetReference: string;
begin
  Result := FdReference.Value;
end;

procedure TMetadataStringNodeHelper.SetReference(const Value: string);
begin
  FdReference.Send(Value);
end;

{ TWorldInfoNode ----------------------------------------------- }

function TWorldInfoNodeHelper.GetTitle: string;
begin
  Result := FdTitle.Value;
end;

procedure TWorldInfoNodeHelper.SetTitle(const Value: string);
begin
  FdTitle.Send(Value);
end;

{ TGeneratedCubeMapTextureNode ----------------------------------------------- }

function TGeneratedCubeMapTextureNodeHelper.GetSize: Integer;
begin
  Result := FdSize.Value;
end;

procedure TGeneratedCubeMapTextureNodeHelper.SetSize(const Value: Integer);
begin
  FdSize.Send(Value);
end;

{ TDISEntityManagerNode ----------------------------------------------- }

function TDISEntityManagerNodeHelper.GetAddress: string;
begin
  Result := FdAddress.Value;
end;

procedure TDISEntityManagerNodeHelper.SetAddress(const Value: string);
begin
  FdAddress.Send(Value);
end;

function TDISEntityManagerNodeHelper.GetApplicationID: Integer;
begin
  Result := FdApplicationID.Value;
end;

procedure TDISEntityManagerNodeHelper.SetApplicationID(const Value: Integer);
begin
  FdApplicationID.Send(Value);
end;

function TDISEntityManagerNodeHelper.GetPort: Integer;
begin
  Result := FdPort.Value;
end;

procedure TDISEntityManagerNodeHelper.SetPort(const Value: Integer);
begin
  FdPort.Send(Value);
end;

function TDISEntityManagerNodeHelper.GetSiteID: Integer;
begin
  Result := FdSiteID.Value;
end;

procedure TDISEntityManagerNodeHelper.SetSiteID(const Value: Integer);
begin
  FdSiteID.Send(Value);
end;

{ TDISEntityTypeMappingNode ----------------------------------------------- }

function TDISEntityTypeMappingNodeHelper.GetCategory: Integer;
begin
  Result := FdCategory.Value;
end;

procedure TDISEntityTypeMappingNodeHelper.SetCategory(const Value: Integer);
begin
  FdCategory.Send(Value);
end;

function TDISEntityTypeMappingNodeHelper.GetCountry: Integer;
begin
  Result := FdCountry.Value;
end;

procedure TDISEntityTypeMappingNodeHelper.SetCountry(const Value: Integer);
begin
  FdCountry.Send(Value);
end;

function TDISEntityTypeMappingNodeHelper.GetDomain: Integer;
begin
  Result := FdDomain.Value;
end;

procedure TDISEntityTypeMappingNodeHelper.SetDomain(const Value: Integer);
begin
  FdDomain.Send(Value);
end;

function TDISEntityTypeMappingNodeHelper.GetExtra: Integer;
begin
  Result := FdExtra.Value;
end;

procedure TDISEntityTypeMappingNodeHelper.SetExtra(const Value: Integer);
begin
  FdExtra.Send(Value);
end;

function TDISEntityTypeMappingNodeHelper.GetKind: Integer;
begin
  Result := FdKind.Value;
end;

procedure TDISEntityTypeMappingNodeHelper.SetKind(const Value: Integer);
begin
  FdKind.Send(Value);
end;

function TDISEntityTypeMappingNodeHelper.GetSpecific: Integer;
begin
  Result := FdSpecific.Value;
end;

procedure TDISEntityTypeMappingNodeHelper.SetSpecific(const Value: Integer);
begin
  FdSpecific.Send(Value);
end;

function TDISEntityTypeMappingNodeHelper.GetSubcategory: Integer;
begin
  Result := FdSubcategory.Value;
end;

procedure TDISEntityTypeMappingNodeHelper.SetSubcategory(const Value: Integer);
begin
  FdSubcategory.Send(Value);
end;

{ TEspduTransformNode ----------------------------------------------- }

function TEspduTransformNodeHelper.GetAddress: string;
begin
  Result := FdAddress.Value;
end;

procedure TEspduTransformNodeHelper.SetAddress(const Value: string);
begin
  FdAddress.Send(Value);
end;

function TEspduTransformNodeHelper.GetApplicationID: Integer;
begin
  Result := FdApplicationID.Value;
end;

procedure TEspduTransformNodeHelper.SetApplicationID(const Value: Integer);
begin
  FdApplicationID.Send(Value);
end;

function TEspduTransformNodeHelper.GetArticulationParameterCount: Integer;
begin
  Result := FdArticulationParameterCount.Value;
end;

procedure TEspduTransformNodeHelper.SetArticulationParameterCount(const Value: Integer);
begin
  FdArticulationParameterCount.Send(Value);
end;

function TEspduTransformNodeHelper.GetCenter: TVector3Single;
begin
  Result := FdCenter.Value;
end;

procedure TEspduTransformNodeHelper.SetCenter(const Value: TVector3Single);
begin
  FdCenter.Send(Value);
end;

function TEspduTransformNodeHelper.GetCollisionType: Integer;
begin
  Result := FdCollisionType.Value;
end;

procedure TEspduTransformNodeHelper.SetCollisionType(const Value: Integer);
begin
  FdCollisionType.Send(Value);
end;

function TEspduTransformNodeHelper.GetDeadReckoning: Integer;
begin
  Result := FdDeadReckoning.Value;
end;

procedure TEspduTransformNodeHelper.SetDeadReckoning(const Value: Integer);
begin
  FdDeadReckoning.Send(Value);
end;

function TEspduTransformNodeHelper.GetDetonationLocation: TVector3Single;
begin
  Result := FdDetonationLocation.Value;
end;

procedure TEspduTransformNodeHelper.SetDetonationLocation(const Value: TVector3Single);
begin
  FdDetonationLocation.Send(Value);
end;

function TEspduTransformNodeHelper.GetDetonationRelativeLocation: TVector3Single;
begin
  Result := FdDetonationRelativeLocation.Value;
end;

procedure TEspduTransformNodeHelper.SetDetonationRelativeLocation(const Value: TVector3Single);
begin
  FdDetonationRelativeLocation.Send(Value);
end;

function TEspduTransformNodeHelper.GetDetonationResult: Integer;
begin
  Result := FdDetonationResult.Value;
end;

procedure TEspduTransformNodeHelper.SetDetonationResult(const Value: Integer);
begin
  FdDetonationResult.Send(Value);
end;

function TEspduTransformNodeHelper.GetEnabled: boolean;
begin
  Result := FdEnabled.Value;
end;

procedure TEspduTransformNodeHelper.SetEnabled(const Value: boolean);
begin
  FdEnabled.Send(Value);
end;

function TEspduTransformNodeHelper.GetEntityCategory: Integer;
begin
  Result := FdEntityCategory.Value;
end;

procedure TEspduTransformNodeHelper.SetEntityCategory(const Value: Integer);
begin
  FdEntityCategory.Send(Value);
end;

function TEspduTransformNodeHelper.GetEntityCountry: Integer;
begin
  Result := FdEntityCountry.Value;
end;

procedure TEspduTransformNodeHelper.SetEntityCountry(const Value: Integer);
begin
  FdEntityCountry.Send(Value);
end;

function TEspduTransformNodeHelper.GetEntityDomain: Integer;
begin
  Result := FdEntityDomain.Value;
end;

procedure TEspduTransformNodeHelper.SetEntityDomain(const Value: Integer);
begin
  FdEntityDomain.Send(Value);
end;

function TEspduTransformNodeHelper.GetEntityExtra: Integer;
begin
  Result := FdEntityExtra.Value;
end;

procedure TEspduTransformNodeHelper.SetEntityExtra(const Value: Integer);
begin
  FdEntityExtra.Send(Value);
end;

function TEspduTransformNodeHelper.GetEntityID: Integer;
begin
  Result := FdEntityID.Value;
end;

procedure TEspduTransformNodeHelper.SetEntityID(const Value: Integer);
begin
  FdEntityID.Send(Value);
end;

function TEspduTransformNodeHelper.GetEntityKind: Integer;
begin
  Result := FdEntityKind.Value;
end;

procedure TEspduTransformNodeHelper.SetEntityKind(const Value: Integer);
begin
  FdEntityKind.Send(Value);
end;

function TEspduTransformNodeHelper.GetEntitySpecific: Integer;
begin
  Result := FdEntitySpecific.Value;
end;

procedure TEspduTransformNodeHelper.SetEntitySpecific(const Value: Integer);
begin
  FdEntitySpecific.Send(Value);
end;

function TEspduTransformNodeHelper.GetEntitySubCategory: Integer;
begin
  Result := FdEntitySubCategory.Value;
end;

procedure TEspduTransformNodeHelper.SetEntitySubCategory(const Value: Integer);
begin
  FdEntitySubCategory.Send(Value);
end;

function TEspduTransformNodeHelper.GetEventApplicationID: Integer;
begin
  Result := FdEventApplicationID.Value;
end;

procedure TEspduTransformNodeHelper.SetEventApplicationID(const Value: Integer);
begin
  FdEventApplicationID.Send(Value);
end;

function TEspduTransformNodeHelper.GetEventEntityID: Integer;
begin
  Result := FdEventEntityID.Value;
end;

procedure TEspduTransformNodeHelper.SetEventEntityID(const Value: Integer);
begin
  FdEventEntityID.Send(Value);
end;

function TEspduTransformNodeHelper.GetEventNumber: Integer;
begin
  Result := FdEventNumber.Value;
end;

procedure TEspduTransformNodeHelper.SetEventNumber(const Value: Integer);
begin
  FdEventNumber.Send(Value);
end;

function TEspduTransformNodeHelper.GetEventSiteID: Integer;
begin
  Result := FdEventSiteID.Value;
end;

procedure TEspduTransformNodeHelper.SetEventSiteID(const Value: Integer);
begin
  FdEventSiteID.Send(Value);
end;

function TEspduTransformNodeHelper.GetFired1: boolean;
begin
  Result := FdFired1.Value;
end;

procedure TEspduTransformNodeHelper.SetFired1(const Value: boolean);
begin
  FdFired1.Send(Value);
end;

function TEspduTransformNodeHelper.GetFired2: boolean;
begin
  Result := FdFired2.Value;
end;

procedure TEspduTransformNodeHelper.SetFired2(const Value: boolean);
begin
  FdFired2.Send(Value);
end;

function TEspduTransformNodeHelper.GetFireMissionIndex: Integer;
begin
  Result := FdFireMissionIndex.Value;
end;

procedure TEspduTransformNodeHelper.SetFireMissionIndex(const Value: Integer);
begin
  FdFireMissionIndex.Send(Value);
end;

function TEspduTransformNodeHelper.GetFiringRange: Single;
begin
  Result := FdFiringRange.Value;
end;

procedure TEspduTransformNodeHelper.SetFiringRange(const Value: Single);
begin
  FdFiringRange.Send(Value);
end;

function TEspduTransformNodeHelper.GetFiringRate: Integer;
begin
  Result := FdFiringRate.Value;
end;

procedure TEspduTransformNodeHelper.SetFiringRate(const Value: Integer);
begin
  FdFiringRate.Send(Value);
end;

function TEspduTransformNodeHelper.GetForceID: Integer;
begin
  Result := FdForceID.Value;
end;

procedure TEspduTransformNodeHelper.SetForceID(const Value: Integer);
begin
  FdForceID.Send(Value);
end;

function TEspduTransformNodeHelper.GetFuse: Integer;
begin
  Result := FdFuse.Value;
end;

procedure TEspduTransformNodeHelper.SetFuse(const Value: Integer);
begin
  FdFuse.Send(Value);
end;

function TEspduTransformNodeHelper.GetLinearVelocity: TVector3Single;
begin
  Result := FdLinearVelocity.Value;
end;

procedure TEspduTransformNodeHelper.SetLinearVelocity(const Value: TVector3Single);
begin
  FdLinearVelocity.Send(Value);
end;

function TEspduTransformNodeHelper.GetLinearAcceleration: TVector3Single;
begin
  Result := FdLinearAcceleration.Value;
end;

procedure TEspduTransformNodeHelper.SetLinearAcceleration(const Value: TVector3Single);
begin
  FdLinearAcceleration.Send(Value);
end;

function TEspduTransformNodeHelper.GetMarking: string;
begin
  Result := FdMarking.Value;
end;

procedure TEspduTransformNodeHelper.SetMarking(const Value: string);
begin
  FdMarking.Send(Value);
end;

function TEspduTransformNodeHelper.GetMulticastRelayHost: string;
begin
  Result := FdMulticastRelayHost.Value;
end;

procedure TEspduTransformNodeHelper.SetMulticastRelayHost(const Value: string);
begin
  FdMulticastRelayHost.Send(Value);
end;

function TEspduTransformNodeHelper.GetMulticastRelayPort: Integer;
begin
  Result := FdMulticastRelayPort.Value;
end;

procedure TEspduTransformNodeHelper.SetMulticastRelayPort(const Value: Integer);
begin
  FdMulticastRelayPort.Send(Value);
end;

function TEspduTransformNodeHelper.GetMunitionApplicationID: Integer;
begin
  Result := FdMunitionApplicationID.Value;
end;

procedure TEspduTransformNodeHelper.SetMunitionApplicationID(const Value: Integer);
begin
  FdMunitionApplicationID.Send(Value);
end;

function TEspduTransformNodeHelper.GetMunitionEndPoint: TVector3Single;
begin
  Result := FdMunitionEndPoint.Value;
end;

procedure TEspduTransformNodeHelper.SetMunitionEndPoint(const Value: TVector3Single);
begin
  FdMunitionEndPoint.Send(Value);
end;

function TEspduTransformNodeHelper.GetMunitionEntityID: Integer;
begin
  Result := FdMunitionEntityID.Value;
end;

procedure TEspduTransformNodeHelper.SetMunitionEntityID(const Value: Integer);
begin
  FdMunitionEntityID.Send(Value);
end;

function TEspduTransformNodeHelper.GetMunitionQuantity: Integer;
begin
  Result := FdMunitionQuantity.Value;
end;

procedure TEspduTransformNodeHelper.SetMunitionQuantity(const Value: Integer);
begin
  FdMunitionQuantity.Send(Value);
end;

function TEspduTransformNodeHelper.GetMunitionSiteID: Integer;
begin
  Result := FdMunitionSiteID.Value;
end;

procedure TEspduTransformNodeHelper.SetMunitionSiteID(const Value: Integer);
begin
  FdMunitionSiteID.Send(Value);
end;

function TEspduTransformNodeHelper.GetMunitionStartPoint: TVector3Single;
begin
  Result := FdMunitionStartPoint.Value;
end;

procedure TEspduTransformNodeHelper.SetMunitionStartPoint(const Value: TVector3Single);
begin
  FdMunitionStartPoint.Send(Value);
end;

function TEspduTransformNodeHelper.GetPort: Integer;
begin
  Result := FdPort.Value;
end;

procedure TEspduTransformNodeHelper.SetPort(const Value: Integer);
begin
  FdPort.Send(Value);
end;

function TEspduTransformNodeHelper.GetReadInterval: TFloatTime;
begin
  Result := FdReadInterval.Value;
end;

procedure TEspduTransformNodeHelper.SetReadInterval(const Value: TFloatTime);
begin
  FdReadInterval.Send(Value);
end;

function TEspduTransformNodeHelper.GetRotation: TVector4Single;
begin
  Result := FdRotation.Value;
end;

procedure TEspduTransformNodeHelper.SetRotation(const Value: TVector4Single);
begin
  FdRotation.Send(Value);
end;

function TEspduTransformNodeHelper.GetScale: TVector3Single;
begin
  Result := FdScale.Value;
end;

procedure TEspduTransformNodeHelper.SetScale(const Value: TVector3Single);
begin
  FdScale.Send(Value);
end;

function TEspduTransformNodeHelper.GetScaleOrientation: TVector4Single;
begin
  Result := FdScaleOrientation.Value;
end;

procedure TEspduTransformNodeHelper.SetScaleOrientation(const Value: TVector4Single);
begin
  FdScaleOrientation.Send(Value);
end;

function TEspduTransformNodeHelper.GetSiteID: Integer;
begin
  Result := FdSiteID.Value;
end;

procedure TEspduTransformNodeHelper.SetSiteID(const Value: Integer);
begin
  FdSiteID.Send(Value);
end;

function TEspduTransformNodeHelper.GetTranslation: TVector3Single;
begin
  Result := FdTranslation.Value;
end;

procedure TEspduTransformNodeHelper.SetTranslation(const Value: TVector3Single);
begin
  FdTranslation.Send(Value);
end;

function TEspduTransformNodeHelper.GetWarhead: Integer;
begin
  Result := FdWarhead.Value;
end;

procedure TEspduTransformNodeHelper.SetWarhead(const Value: Integer);
begin
  FdWarhead.Send(Value);
end;

function TEspduTransformNodeHelper.GetWriteInterval: TFloatTime;
begin
  Result := FdWriteInterval.Value;
end;

procedure TEspduTransformNodeHelper.SetWriteInterval(const Value: TFloatTime);
begin
  FdWriteInterval.Send(Value);
end;

function TEspduTransformNodeHelper.GetRtpHeaderExpected: boolean;
begin
  Result := FdRtpHeaderExpected.Value;
end;

procedure TEspduTransformNodeHelper.SetRtpHeaderExpected(const Value: boolean);
begin
  FdRtpHeaderExpected.Send(Value);
end;

{ TReceiverPduNode ----------------------------------------------- }

function TReceiverPduNodeHelper.GetAddress: string;
begin
  Result := FdAddress.Value;
end;

procedure TReceiverPduNodeHelper.SetAddress(const Value: string);
begin
  FdAddress.Send(Value);
end;

function TReceiverPduNodeHelper.GetApplicationID: Integer;
begin
  Result := FdApplicationID.Value;
end;

procedure TReceiverPduNodeHelper.SetApplicationID(const Value: Integer);
begin
  FdApplicationID.Send(Value);
end;

function TReceiverPduNodeHelper.GetEntityID: Integer;
begin
  Result := FdEntityID.Value;
end;

procedure TReceiverPduNodeHelper.SetEntityID(const Value: Integer);
begin
  FdEntityID.Send(Value);
end;

function TReceiverPduNodeHelper.GetMulticastRelayHost: string;
begin
  Result := FdMulticastRelayHost.Value;
end;

procedure TReceiverPduNodeHelper.SetMulticastRelayHost(const Value: string);
begin
  FdMulticastRelayHost.Send(Value);
end;

function TReceiverPduNodeHelper.GetMulticastRelayPort: Integer;
begin
  Result := FdMulticastRelayPort.Value;
end;

procedure TReceiverPduNodeHelper.SetMulticastRelayPort(const Value: Integer);
begin
  FdMulticastRelayPort.Send(Value);
end;

function TReceiverPduNodeHelper.GetPort: Integer;
begin
  Result := FdPort.Value;
end;

procedure TReceiverPduNodeHelper.SetPort(const Value: Integer);
begin
  FdPort.Send(Value);
end;

function TReceiverPduNodeHelper.GetRadioID: Integer;
begin
  Result := FdRadioID.Value;
end;

procedure TReceiverPduNodeHelper.SetRadioID(const Value: Integer);
begin
  FdRadioID.Send(Value);
end;

function TReceiverPduNodeHelper.GetReadInterval: Single;
begin
  Result := FdReadInterval.Value;
end;

procedure TReceiverPduNodeHelper.SetReadInterval(const Value: Single);
begin
  FdReadInterval.Send(Value);
end;

function TReceiverPduNodeHelper.GetReceivedPower: Single;
begin
  Result := FdReceivedPower.Value;
end;

procedure TReceiverPduNodeHelper.SetReceivedPower(const Value: Single);
begin
  FdReceivedPower.Send(Value);
end;

function TReceiverPduNodeHelper.GetReceiverState: Integer;
begin
  Result := FdReceiverState.Value;
end;

procedure TReceiverPduNodeHelper.SetReceiverState(const Value: Integer);
begin
  FdReceiverState.Send(Value);
end;

function TReceiverPduNodeHelper.GetRtpHeaderExpected: boolean;
begin
  Result := FdRtpHeaderExpected.Value;
end;

procedure TReceiverPduNodeHelper.SetRtpHeaderExpected(const Value: boolean);
begin
  FdRtpHeaderExpected.Send(Value);
end;

function TReceiverPduNodeHelper.GetSiteID: Integer;
begin
  Result := FdSiteID.Value;
end;

procedure TReceiverPduNodeHelper.SetSiteID(const Value: Integer);
begin
  FdSiteID.Send(Value);
end;

function TReceiverPduNodeHelper.GetTransmitterApplicationID: Integer;
begin
  Result := FdTransmitterApplicationID.Value;
end;

procedure TReceiverPduNodeHelper.SetTransmitterApplicationID(const Value: Integer);
begin
  FdTransmitterApplicationID.Send(Value);
end;

function TReceiverPduNodeHelper.GetTransmitterEntityID: Integer;
begin
  Result := FdTransmitterEntityID.Value;
end;

procedure TReceiverPduNodeHelper.SetTransmitterEntityID(const Value: Integer);
begin
  FdTransmitterEntityID.Send(Value);
end;

function TReceiverPduNodeHelper.GetTransmitterRadioID: Integer;
begin
  Result := FdTransmitterRadioID.Value;
end;

procedure TReceiverPduNodeHelper.SetTransmitterRadioID(const Value: Integer);
begin
  FdTransmitterRadioID.Send(Value);
end;

function TReceiverPduNodeHelper.GetTransmitterSiteID: Integer;
begin
  Result := FdTransmitterSiteID.Value;
end;

procedure TReceiverPduNodeHelper.SetTransmitterSiteID(const Value: Integer);
begin
  FdTransmitterSiteID.Send(Value);
end;

function TReceiverPduNodeHelper.GetWhichGeometry: Integer;
begin
  Result := FdWhichGeometry.Value;
end;

procedure TReceiverPduNodeHelper.SetWhichGeometry(const Value: Integer);
begin
  FdWhichGeometry.Send(Value);
end;

function TReceiverPduNodeHelper.GetWriteInterval: Single;
begin
  Result := FdWriteInterval.Value;
end;

procedure TReceiverPduNodeHelper.SetWriteInterval(const Value: Single);
begin
  FdWriteInterval.Send(Value);
end;

function TReceiverPduNodeHelper.GetBboxCenter: TVector3Single;
begin
  Result := FdBboxCenter.Value;
end;

procedure TReceiverPduNodeHelper.SetBboxCenter(const Value: TVector3Single);
begin
  FdBboxCenter.Send(Value);
end;

function TReceiverPduNodeHelper.GetBboxSize: TVector3Single;
begin
  Result := FdBboxSize.Value;
end;

procedure TReceiverPduNodeHelper.SetBboxSize(const Value: TVector3Single);
begin
  FdBboxSize.Send(Value);
end;

{ TSignalPduNode ----------------------------------------------- }

function TSignalPduNodeHelper.GetAddress: string;
begin
  Result := FdAddress.Value;
end;

procedure TSignalPduNodeHelper.SetAddress(const Value: string);
begin
  FdAddress.Send(Value);
end;

function TSignalPduNodeHelper.GetApplicationID: Integer;
begin
  Result := FdApplicationID.Value;
end;

procedure TSignalPduNodeHelper.SetApplicationID(const Value: Integer);
begin
  FdApplicationID.Send(Value);
end;

function TSignalPduNodeHelper.GetDataLength: Integer;
begin
  Result := FdDataLength.Value;
end;

procedure TSignalPduNodeHelper.SetDataLength(const Value: Integer);
begin
  FdDataLength.Send(Value);
end;

function TSignalPduNodeHelper.GetEncodingScheme: Integer;
begin
  Result := FdEncodingScheme.Value;
end;

procedure TSignalPduNodeHelper.SetEncodingScheme(const Value: Integer);
begin
  FdEncodingScheme.Send(Value);
end;

function TSignalPduNodeHelper.GetEntityID: Integer;
begin
  Result := FdEntityID.Value;
end;

procedure TSignalPduNodeHelper.SetEntityID(const Value: Integer);
begin
  FdEntityID.Send(Value);
end;

function TSignalPduNodeHelper.GetMulticastRelayHost: string;
begin
  Result := FdMulticastRelayHost.Value;
end;

procedure TSignalPduNodeHelper.SetMulticastRelayHost(const Value: string);
begin
  FdMulticastRelayHost.Send(Value);
end;

function TSignalPduNodeHelper.GetMulticastRelayPort: Integer;
begin
  Result := FdMulticastRelayPort.Value;
end;

procedure TSignalPduNodeHelper.SetMulticastRelayPort(const Value: Integer);
begin
  FdMulticastRelayPort.Send(Value);
end;

function TSignalPduNodeHelper.GetPort: Integer;
begin
  Result := FdPort.Value;
end;

procedure TSignalPduNodeHelper.SetPort(const Value: Integer);
begin
  FdPort.Send(Value);
end;

function TSignalPduNodeHelper.GetRadioID: Integer;
begin
  Result := FdRadioID.Value;
end;

procedure TSignalPduNodeHelper.SetRadioID(const Value: Integer);
begin
  FdRadioID.Send(Value);
end;

function TSignalPduNodeHelper.GetReadInterval: Single;
begin
  Result := FdReadInterval.Value;
end;

procedure TSignalPduNodeHelper.SetReadInterval(const Value: Single);
begin
  FdReadInterval.Send(Value);
end;

function TSignalPduNodeHelper.GetRtpHeaderExpected: boolean;
begin
  Result := FdRtpHeaderExpected.Value;
end;

procedure TSignalPduNodeHelper.SetRtpHeaderExpected(const Value: boolean);
begin
  FdRtpHeaderExpected.Send(Value);
end;

function TSignalPduNodeHelper.GetSampleRate: Integer;
begin
  Result := FdSampleRate.Value;
end;

procedure TSignalPduNodeHelper.SetSampleRate(const Value: Integer);
begin
  FdSampleRate.Send(Value);
end;

function TSignalPduNodeHelper.GetSamples: Integer;
begin
  Result := FdSamples.Value;
end;

procedure TSignalPduNodeHelper.SetSamples(const Value: Integer);
begin
  FdSamples.Send(Value);
end;

function TSignalPduNodeHelper.GetSiteID: Integer;
begin
  Result := FdSiteID.Value;
end;

procedure TSignalPduNodeHelper.SetSiteID(const Value: Integer);
begin
  FdSiteID.Send(Value);
end;

function TSignalPduNodeHelper.GetTdlType: Integer;
begin
  Result := FdTdlType.Value;
end;

procedure TSignalPduNodeHelper.SetTdlType(const Value: Integer);
begin
  FdTdlType.Send(Value);
end;

function TSignalPduNodeHelper.GetWhichGeometry: Integer;
begin
  Result := FdWhichGeometry.Value;
end;

procedure TSignalPduNodeHelper.SetWhichGeometry(const Value: Integer);
begin
  FdWhichGeometry.Send(Value);
end;

function TSignalPduNodeHelper.GetWriteInterval: Single;
begin
  Result := FdWriteInterval.Value;
end;

procedure TSignalPduNodeHelper.SetWriteInterval(const Value: Single);
begin
  FdWriteInterval.Send(Value);
end;

function TSignalPduNodeHelper.GetBboxCenter: TVector3Single;
begin
  Result := FdBboxCenter.Value;
end;

procedure TSignalPduNodeHelper.SetBboxCenter(const Value: TVector3Single);
begin
  FdBboxCenter.Send(Value);
end;

function TSignalPduNodeHelper.GetBboxSize: TVector3Single;
begin
  Result := FdBboxSize.Value;
end;

procedure TSignalPduNodeHelper.SetBboxSize(const Value: TVector3Single);
begin
  FdBboxSize.Send(Value);
end;

{ TTransmitterPduNode ----------------------------------------------- }

function TTransmitterPduNodeHelper.GetAddress: string;
begin
  Result := FdAddress.Value;
end;

procedure TTransmitterPduNodeHelper.SetAddress(const Value: string);
begin
  FdAddress.Send(Value);
end;

function TTransmitterPduNodeHelper.GetAntennaLocation: TVector3Single;
begin
  Result := FdAntennaLocation.Value;
end;

procedure TTransmitterPduNodeHelper.SetAntennaLocation(const Value: TVector3Single);
begin
  FdAntennaLocation.Send(Value);
end;

function TTransmitterPduNodeHelper.GetAntennaPatternLength: Integer;
begin
  Result := FdAntennaPatternLength.Value;
end;

procedure TTransmitterPduNodeHelper.SetAntennaPatternLength(const Value: Integer);
begin
  FdAntennaPatternLength.Send(Value);
end;

function TTransmitterPduNodeHelper.GetAntennaPatternType: Integer;
begin
  Result := FdAntennaPatternType.Value;
end;

procedure TTransmitterPduNodeHelper.SetAntennaPatternType(const Value: Integer);
begin
  FdAntennaPatternType.Send(Value);
end;

function TTransmitterPduNodeHelper.GetApplicationID: Integer;
begin
  Result := FdApplicationID.Value;
end;

procedure TTransmitterPduNodeHelper.SetApplicationID(const Value: Integer);
begin
  FdApplicationID.Send(Value);
end;

function TTransmitterPduNodeHelper.GetCryptoKeyID: Integer;
begin
  Result := FdCryptoKeyID.Value;
end;

procedure TTransmitterPduNodeHelper.SetCryptoKeyID(const Value: Integer);
begin
  FdCryptoKeyID.Send(Value);
end;

function TTransmitterPduNodeHelper.GetCryptoSystem: Integer;
begin
  Result := FdCryptoSystem.Value;
end;

procedure TTransmitterPduNodeHelper.SetCryptoSystem(const Value: Integer);
begin
  FdCryptoSystem.Send(Value);
end;

function TTransmitterPduNodeHelper.GetEntityID: Integer;
begin
  Result := FdEntityID.Value;
end;

procedure TTransmitterPduNodeHelper.SetEntityID(const Value: Integer);
begin
  FdEntityID.Send(Value);
end;

function TTransmitterPduNodeHelper.GetFrequency: Integer;
begin
  Result := FdFrequency.Value;
end;

procedure TTransmitterPduNodeHelper.SetFrequency(const Value: Integer);
begin
  FdFrequency.Send(Value);
end;

function TTransmitterPduNodeHelper.GetInputSource: Integer;
begin
  Result := FdInputSource.Value;
end;

procedure TTransmitterPduNodeHelper.SetInputSource(const Value: Integer);
begin
  FdInputSource.Send(Value);
end;

function TTransmitterPduNodeHelper.GetLengthOfModulationParameters: Integer;
begin
  Result := FdLengthOfModulationParameters.Value;
end;

procedure TTransmitterPduNodeHelper.SetLengthOfModulationParameters(const Value: Integer);
begin
  FdLengthOfModulationParameters.Send(Value);
end;

function TTransmitterPduNodeHelper.GetModulationTypeDetail: Integer;
begin
  Result := FdModulationTypeDetail.Value;
end;

procedure TTransmitterPduNodeHelper.SetModulationTypeDetail(const Value: Integer);
begin
  FdModulationTypeDetail.Send(Value);
end;

function TTransmitterPduNodeHelper.GetModulationTypeMajor: Integer;
begin
  Result := FdModulationTypeMajor.Value;
end;

procedure TTransmitterPduNodeHelper.SetModulationTypeMajor(const Value: Integer);
begin
  FdModulationTypeMajor.Send(Value);
end;

function TTransmitterPduNodeHelper.GetModulationTypeSpreadSpectrum: Integer;
begin
  Result := FdModulationTypeSpreadSpectrum.Value;
end;

procedure TTransmitterPduNodeHelper.SetModulationTypeSpreadSpectrum(const Value: Integer);
begin
  FdModulationTypeSpreadSpectrum.Send(Value);
end;

function TTransmitterPduNodeHelper.GetModulationTypeSystem: Integer;
begin
  Result := FdModulationTypeSystem.Value;
end;

procedure TTransmitterPduNodeHelper.SetModulationTypeSystem(const Value: Integer);
begin
  FdModulationTypeSystem.Send(Value);
end;

function TTransmitterPduNodeHelper.GetMulticastRelayHost: string;
begin
  Result := FdMulticastRelayHost.Value;
end;

procedure TTransmitterPduNodeHelper.SetMulticastRelayHost(const Value: string);
begin
  FdMulticastRelayHost.Send(Value);
end;

function TTransmitterPduNodeHelper.GetMulticastRelayPort: Integer;
begin
  Result := FdMulticastRelayPort.Value;
end;

procedure TTransmitterPduNodeHelper.SetMulticastRelayPort(const Value: Integer);
begin
  FdMulticastRelayPort.Send(Value);
end;

function TTransmitterPduNodeHelper.GetPort: Integer;
begin
  Result := FdPort.Value;
end;

procedure TTransmitterPduNodeHelper.SetPort(const Value: Integer);
begin
  FdPort.Send(Value);
end;

function TTransmitterPduNodeHelper.GetPower: Single;
begin
  Result := FdPower.Value;
end;

procedure TTransmitterPduNodeHelper.SetPower(const Value: Single);
begin
  FdPower.Send(Value);
end;

function TTransmitterPduNodeHelper.GetRadioEntityTypeCategory: Integer;
begin
  Result := FdRadioEntityTypeCategory.Value;
end;

procedure TTransmitterPduNodeHelper.SetRadioEntityTypeCategory(const Value: Integer);
begin
  FdRadioEntityTypeCategory.Send(Value);
end;

function TTransmitterPduNodeHelper.GetRadioEntityTypeCountry: Integer;
begin
  Result := FdRadioEntityTypeCountry.Value;
end;

procedure TTransmitterPduNodeHelper.SetRadioEntityTypeCountry(const Value: Integer);
begin
  FdRadioEntityTypeCountry.Send(Value);
end;

function TTransmitterPduNodeHelper.GetRadioEntityTypeDomain: Integer;
begin
  Result := FdRadioEntityTypeDomain.Value;
end;

procedure TTransmitterPduNodeHelper.SetRadioEntityTypeDomain(const Value: Integer);
begin
  FdRadioEntityTypeDomain.Send(Value);
end;

function TTransmitterPduNodeHelper.GetRadioEntityTypeKind: Integer;
begin
  Result := FdRadioEntityTypeKind.Value;
end;

procedure TTransmitterPduNodeHelper.SetRadioEntityTypeKind(const Value: Integer);
begin
  FdRadioEntityTypeKind.Send(Value);
end;

function TTransmitterPduNodeHelper.GetRadioEntityTypeNomenclature: Integer;
begin
  Result := FdRadioEntityTypeNomenclature.Value;
end;

procedure TTransmitterPduNodeHelper.SetRadioEntityTypeNomenclature(const Value: Integer);
begin
  FdRadioEntityTypeNomenclature.Send(Value);
end;

function TTransmitterPduNodeHelper.GetRadioEntityTypeNomenclatureVersion: Integer;
begin
  Result := FdRadioEntityTypeNomenclatureVersion.Value;
end;

procedure TTransmitterPduNodeHelper.SetRadioEntityTypeNomenclatureVersion(const Value: Integer);
begin
  FdRadioEntityTypeNomenclatureVersion.Send(Value);
end;

function TTransmitterPduNodeHelper.GetRadioID: Integer;
begin
  Result := FdRadioID.Value;
end;

procedure TTransmitterPduNodeHelper.SetRadioID(const Value: Integer);
begin
  FdRadioID.Send(Value);
end;

function TTransmitterPduNodeHelper.GetReadInterval: Single;
begin
  Result := FdReadInterval.Value;
end;

procedure TTransmitterPduNodeHelper.SetReadInterval(const Value: Single);
begin
  FdReadInterval.Send(Value);
end;

function TTransmitterPduNodeHelper.GetRelativeAntennaLocation: TVector3Single;
begin
  Result := FdRelativeAntennaLocation.Value;
end;

procedure TTransmitterPduNodeHelper.SetRelativeAntennaLocation(const Value: TVector3Single);
begin
  FdRelativeAntennaLocation.Send(Value);
end;

function TTransmitterPduNodeHelper.GetRtpHeaderExpected: boolean;
begin
  Result := FdRtpHeaderExpected.Value;
end;

procedure TTransmitterPduNodeHelper.SetRtpHeaderExpected(const Value: boolean);
begin
  FdRtpHeaderExpected.Send(Value);
end;

function TTransmitterPduNodeHelper.GetSiteID: Integer;
begin
  Result := FdSiteID.Value;
end;

procedure TTransmitterPduNodeHelper.SetSiteID(const Value: Integer);
begin
  FdSiteID.Send(Value);
end;

function TTransmitterPduNodeHelper.GetTransmitFrequencyBandwidth: Single;
begin
  Result := FdTransmitFrequencyBandwidth.Value;
end;

procedure TTransmitterPduNodeHelper.SetTransmitFrequencyBandwidth(const Value: Single);
begin
  FdTransmitFrequencyBandwidth.Send(Value);
end;

function TTransmitterPduNodeHelper.GetTransmitState: Integer;
begin
  Result := FdTransmitState.Value;
end;

procedure TTransmitterPduNodeHelper.SetTransmitState(const Value: Integer);
begin
  FdTransmitState.Send(Value);
end;

function TTransmitterPduNodeHelper.GetWhichGeometry: Integer;
begin
  Result := FdWhichGeometry.Value;
end;

procedure TTransmitterPduNodeHelper.SetWhichGeometry(const Value: Integer);
begin
  FdWhichGeometry.Send(Value);
end;

function TTransmitterPduNodeHelper.GetWriteInterval: Single;
begin
  Result := FdWriteInterval.Value;
end;

procedure TTransmitterPduNodeHelper.SetWriteInterval(const Value: Single);
begin
  FdWriteInterval.Send(Value);
end;

function TTransmitterPduNodeHelper.GetBboxCenter: TVector3Single;
begin
  Result := FdBboxCenter.Value;
end;

procedure TTransmitterPduNodeHelper.SetBboxCenter(const Value: TVector3Single);
begin
  FdBboxCenter.Send(Value);
end;

function TTransmitterPduNodeHelper.GetBboxSize: TVector3Single;
begin
  Result := FdBboxSize.Value;
end;

procedure TTransmitterPduNodeHelper.SetBboxSize(const Value: TVector3Single);
begin
  FdBboxSize.Send(Value);
end;

{ TAbstractBackgroundNode ----------------------------------------------- }

function TAbstractBackgroundNodeHelper.GetTransparency: Single;
begin
  Result := FdTransparency.Value;
end;

procedure TAbstractBackgroundNodeHelper.SetTransparency(const Value: Single);
begin
  FdTransparency.Send(Value);
end;

{ TFogNode ----------------------------------------------- }

function TFogNodeHelper.GetColor: TCastleColorRGB;
begin
  Result := FdColor.Value;
end;

procedure TFogNodeHelper.SetColor(const Value: TCastleColorRGB);
begin
  FdColor.Send(Value);
end;

function TFogNodeHelper.GetVisibilityRange: Single;
begin
  Result := FdVisibilityRange.Value;
end;

procedure TFogNodeHelper.SetVisibilityRange(const Value: Single);
begin
  FdVisibilityRange.Send(Value);
end;

{ TLocalFogNode ----------------------------------------------- }

function TLocalFogNodeHelper.GetColor: TCastleColorRGB;
begin
  Result := FdColor.Value;
end;

procedure TLocalFogNodeHelper.SetColor(const Value: TCastleColorRGB);
begin
  FdColor.Send(Value);
end;

function TLocalFogNodeHelper.GetEnabled: boolean;
begin
  Result := FdEnabled.Value;
end;

procedure TLocalFogNodeHelper.SetEnabled(const Value: boolean);
begin
  FdEnabled.Send(Value);
end;

function TLocalFogNodeHelper.GetVisibilityRange: Single;
begin
  Result := FdVisibilityRange.Value;
end;

procedure TLocalFogNodeHelper.SetVisibilityRange(const Value: Single);
begin
  FdVisibilityRange.Send(Value);
end;

{ TAbstractEnvironmentalSensorNode ----------------------------------------------- }

function TAbstractEnvironmentalSensorNodeHelper.GetCenter: TVector3Single;
begin
  Result := FdCenter.Value;
end;

procedure TAbstractEnvironmentalSensorNodeHelper.SetCenter(const Value: TVector3Single);
begin
  FdCenter.Send(Value);
end;

function TAbstractEnvironmentalSensorNodeHelper.GetSize: TVector3Single;
begin
  Result := FdSize.Value;
end;

procedure TAbstractEnvironmentalSensorNodeHelper.SetSize(const Value: TVector3Single);
begin
  FdSize.Send(Value);
end;

{ TBooleanToggleNode ----------------------------------------------- }

function TBooleanToggleNodeHelper.GetToggle: boolean;
begin
  Result := FdToggle.Value;
end;

procedure TBooleanToggleNodeHelper.SetToggle(const Value: boolean);
begin
  FdToggle.Send(Value);
end;

{ TIntegerTriggerNode ----------------------------------------------- }

function TIntegerTriggerNodeHelper.GetIntegerKey: Integer;
begin
  Result := FdIntegerKey.Value;
end;

procedure TIntegerTriggerNodeHelper.SetIntegerKey(const Value: Integer);
begin
  FdIntegerKey.Send(Value);
end;

{ TAbstractChaserNode ----------------------------------------------- }

function TAbstractChaserNodeHelper.GetDuration: TFloatTime;
begin
  Result := FdDuration.Value;
end;

procedure TAbstractChaserNodeHelper.SetDuration(const Value: TFloatTime);
begin
  FdDuration.Send(Value);
end;

{ TAbstractDamperNode ----------------------------------------------- }

function TAbstractDamperNodeHelper.GetTau: TFloatTime;
begin
  Result := FdTau.Value;
end;

procedure TAbstractDamperNodeHelper.SetTau(const Value: TFloatTime);
begin
  FdTau.Send(Value);
end;

function TAbstractDamperNodeHelper.GetTolerance: Single;
begin
  Result := FdTolerance.Value;
end;

procedure TAbstractDamperNodeHelper.SetTolerance(const Value: Single);
begin
  FdTolerance.Send(Value);
end;

function TAbstractDamperNodeHelper.GetOrder: Integer;
begin
  Result := FdOrder.Value;
end;

procedure TAbstractDamperNodeHelper.SetOrder(const Value: Integer);
begin
  FdOrder.Send(Value);
end;

{ TColorDamperNode ----------------------------------------------- }

function TColorDamperNodeHelper.GetInitialDestination: TCastleColorRGB;
begin
  Result := FdInitialDestination.Value;
end;

procedure TColorDamperNodeHelper.SetInitialDestination(const Value: TCastleColorRGB);
begin
  FdInitialDestination.Send(Value);
end;

function TColorDamperNodeHelper.GetInitialValue: TCastleColorRGB;
begin
  Result := FdInitialValue.Value;
end;

procedure TColorDamperNodeHelper.SetInitialValue(const Value: TCastleColorRGB);
begin
  FdInitialValue.Send(Value);
end;

{ TOrientationChaserNode ----------------------------------------------- }

function TOrientationChaserNodeHelper.GetInitialDestination: TVector4Single;
begin
  Result := FdInitialDestination.Value;
end;

procedure TOrientationChaserNodeHelper.SetInitialDestination(const Value: TVector4Single);
begin
  FdInitialDestination.Send(Value);
end;

function TOrientationChaserNodeHelper.GetInitialValue: TVector4Single;
begin
  Result := FdInitialValue.Value;
end;

procedure TOrientationChaserNodeHelper.SetInitialValue(const Value: TVector4Single);
begin
  FdInitialValue.Send(Value);
end;

{ TOrientationDamperNode ----------------------------------------------- }

function TOrientationDamperNodeHelper.GetInitialDestination: TVector4Single;
begin
  Result := FdInitialDestination.Value;
end;

procedure TOrientationDamperNodeHelper.SetInitialDestination(const Value: TVector4Single);
begin
  FdInitialDestination.Send(Value);
end;

function TOrientationDamperNodeHelper.GetInitialValue: TVector4Single;
begin
  Result := FdInitialValue.Value;
end;

procedure TOrientationDamperNodeHelper.SetInitialValue(const Value: TVector4Single);
begin
  FdInitialValue.Send(Value);
end;

{ TPositionChaserNode ----------------------------------------------- }

function TPositionChaserNodeHelper.GetInitialDestination: TVector3Single;
begin
  Result := FdInitialDestination.Value;
end;

procedure TPositionChaserNodeHelper.SetInitialDestination(const Value: TVector3Single);
begin
  FdInitialDestination.Send(Value);
end;

function TPositionChaserNodeHelper.GetInitialValue: TVector3Single;
begin
  Result := FdInitialValue.Value;
end;

procedure TPositionChaserNodeHelper.SetInitialValue(const Value: TVector3Single);
begin
  FdInitialValue.Send(Value);
end;

{ TPositionChaser2DNode ----------------------------------------------- }

function TPositionChaser2DNodeHelper.GetInitialDestination: TVector2Single;
begin
  Result := FdInitialDestination.Value;
end;

procedure TPositionChaser2DNodeHelper.SetInitialDestination(const Value: TVector2Single);
begin
  FdInitialDestination.Send(Value);
end;

function TPositionChaser2DNodeHelper.GetInitialValue: TVector2Single;
begin
  Result := FdInitialValue.Value;
end;

procedure TPositionChaser2DNodeHelper.SetInitialValue(const Value: TVector2Single);
begin
  FdInitialValue.Send(Value);
end;

{ TPositionDamperNode ----------------------------------------------- }

function TPositionDamperNodeHelper.GetInitialDestination: TVector3Single;
begin
  Result := FdInitialDestination.Value;
end;

procedure TPositionDamperNodeHelper.SetInitialDestination(const Value: TVector3Single);
begin
  FdInitialDestination.Send(Value);
end;

function TPositionDamperNodeHelper.GetInitialValue: TVector3Single;
begin
  Result := FdInitialValue.Value;
end;

procedure TPositionDamperNodeHelper.SetInitialValue(const Value: TVector3Single);
begin
  FdInitialValue.Send(Value);
end;

{ TPositionDamper2DNode ----------------------------------------------- }

function TPositionDamper2DNodeHelper.GetInitialDestination: TVector2Single;
begin
  Result := FdInitialDestination.Value;
end;

procedure TPositionDamper2DNodeHelper.SetInitialDestination(const Value: TVector2Single);
begin
  FdInitialDestination.Send(Value);
end;

function TPositionDamper2DNodeHelper.GetInitialValue: TVector2Single;
begin
  Result := FdInitialValue.Value;
end;

procedure TPositionDamper2DNodeHelper.SetInitialValue(const Value: TVector2Single);
begin
  FdInitialValue.Send(Value);
end;

{ TScalarChaserNode ----------------------------------------------- }

function TScalarChaserNodeHelper.GetInitialDestination: Single;
begin
  Result := FdInitialDestination.Value;
end;

procedure TScalarChaserNodeHelper.SetInitialDestination(const Value: Single);
begin
  FdInitialDestination.Send(Value);
end;

function TScalarChaserNodeHelper.GetInitialValue: Single;
begin
  Result := FdInitialValue.Value;
end;

procedure TScalarChaserNodeHelper.SetInitialValue(const Value: Single);
begin
  FdInitialValue.Send(Value);
end;

{ TArc2DNode ----------------------------------------------- }

function TArc2DNodeHelper.GetEndAngle: Single;
begin
  Result := FdEndAngle.Value;
end;

procedure TArc2DNodeHelper.SetEndAngle(const Value: Single);
begin
  FdEndAngle.Send(Value);
end;

function TArc2DNodeHelper.GetRadius: Single;
begin
  Result := FdRadius.Value;
end;

procedure TArc2DNodeHelper.SetRadius(const Value: Single);
begin
  FdRadius.Send(Value);
end;

function TArc2DNodeHelper.GetStartAngle: Single;
begin
  Result := FdStartAngle.Value;
end;

procedure TArc2DNodeHelper.SetStartAngle(const Value: Single);
begin
  FdStartAngle.Send(Value);
end;

{ TArcClose2DNode ----------------------------------------------- }

function TArcClose2DNodeHelper.GetEndAngle: Single;
begin
  Result := FdEndAngle.Value;
end;

procedure TArcClose2DNodeHelper.SetEndAngle(const Value: Single);
begin
  FdEndAngle.Send(Value);
end;

function TArcClose2DNodeHelper.GetRadius: Single;
begin
  Result := FdRadius.Value;
end;

procedure TArcClose2DNodeHelper.SetRadius(const Value: Single);
begin
  FdRadius.Send(Value);
end;

function TArcClose2DNodeHelper.GetStartAngle: Single;
begin
  Result := FdStartAngle.Value;
end;

procedure TArcClose2DNodeHelper.SetStartAngle(const Value: Single);
begin
  FdStartAngle.Send(Value);
end;

{ TCircle2DNode ----------------------------------------------- }

function TCircle2DNodeHelper.GetRadius: Single;
begin
  Result := FdRadius.Value;
end;

procedure TCircle2DNodeHelper.SetRadius(const Value: Single);
begin
  FdRadius.Send(Value);
end;

function TCircle2DNodeHelper.GetSlices: Integer;
begin
  Result := FdSlices.Value;
end;

procedure TCircle2DNodeHelper.SetSlices(const Value: Integer);
begin
  FdSlices.Send(Value);
end;

{ TDisk2DNode ----------------------------------------------- }

function TDisk2DNodeHelper.GetInnerRadius: Single;
begin
  Result := FdInnerRadius.Value;
end;

procedure TDisk2DNodeHelper.SetInnerRadius(const Value: Single);
begin
  FdInnerRadius.Send(Value);
end;

function TDisk2DNodeHelper.GetOuterRadius: Single;
begin
  Result := FdOuterRadius.Value;
end;

procedure TDisk2DNodeHelper.SetOuterRadius(const Value: Single);
begin
  FdOuterRadius.Send(Value);
end;

{ TRectangle2DNode ----------------------------------------------- }

function TRectangle2DNodeHelper.GetSize: TVector2Single;
begin
  Result := FdSize.Value;
end;

procedure TRectangle2DNodeHelper.SetSize(const Value: TVector2Single);
begin
  FdSize.Send(Value);
end;

{ TBoxNode ----------------------------------------------- }

function TBoxNodeHelper.GetSize: TVector3Single;
begin
  Result := FdSize.Value;
end;

procedure TBoxNodeHelper.SetSize(const Value: TVector3Single);
begin
  FdSize.Send(Value);
end;

function TBoxNodeHelper.GetDivisions: Integer;
begin
  Result := FdDivisions.Value;
end;

procedure TBoxNodeHelper.SetDivisions(const Value: Integer);
begin
  FdDivisions.Send(Value);
end;

{ TConeNode ----------------------------------------------- }

function TConeNodeHelper.GetBottom: boolean;
begin
  Result := FdBottom.Value;
end;

procedure TConeNodeHelper.SetBottom(const Value: boolean);
begin
  FdBottom.Send(Value);
end;

function TConeNodeHelper.GetBottomRadius: Single;
begin
  Result := FdBottomRadius.Value;
end;

procedure TConeNodeHelper.SetBottomRadius(const Value: Single);
begin
  FdBottomRadius.Send(Value);
end;

function TConeNodeHelper.GetHeight: Single;
begin
  Result := FdHeight.Value;
end;

procedure TConeNodeHelper.SetHeight(const Value: Single);
begin
  FdHeight.Send(Value);
end;

function TConeNodeHelper.GetSide: boolean;
begin
  Result := FdSide.Value;
end;

procedure TConeNodeHelper.SetSide(const Value: boolean);
begin
  FdSide.Send(Value);
end;

function TConeNodeHelper.GetSlices: Integer;
begin
  Result := FdSlices.Value;
end;

procedure TConeNodeHelper.SetSlices(const Value: Integer);
begin
  FdSlices.Send(Value);
end;

function TConeNodeHelper.GetStacks: Integer;
begin
  Result := FdStacks.Value;
end;

procedure TConeNodeHelper.SetStacks(const Value: Integer);
begin
  FdStacks.Send(Value);
end;

{ TCylinderNode ----------------------------------------------- }

function TCylinderNodeHelper.GetBottom: boolean;
begin
  Result := FdBottom.Value;
end;

procedure TCylinderNodeHelper.SetBottom(const Value: boolean);
begin
  FdBottom.Send(Value);
end;

function TCylinderNodeHelper.GetHeight: Single;
begin
  Result := FdHeight.Value;
end;

procedure TCylinderNodeHelper.SetHeight(const Value: Single);
begin
  FdHeight.Send(Value);
end;

function TCylinderNodeHelper.GetRadius: Single;
begin
  Result := FdRadius.Value;
end;

procedure TCylinderNodeHelper.SetRadius(const Value: Single);
begin
  FdRadius.Send(Value);
end;

function TCylinderNodeHelper.GetSide: boolean;
begin
  Result := FdSide.Value;
end;

procedure TCylinderNodeHelper.SetSide(const Value: boolean);
begin
  FdSide.Send(Value);
end;

function TCylinderNodeHelper.GetTop: boolean;
begin
  Result := FdTop.Value;
end;

procedure TCylinderNodeHelper.SetTop(const Value: boolean);
begin
  FdTop.Send(Value);
end;

function TCylinderNodeHelper.GetSlices: Integer;
begin
  Result := FdSlices.Value;
end;

procedure TCylinderNodeHelper.SetSlices(const Value: Integer);
begin
  FdSlices.Send(Value);
end;

function TCylinderNodeHelper.GetStacks: Integer;
begin
  Result := FdStacks.Value;
end;

procedure TCylinderNodeHelper.SetStacks(const Value: Integer);
begin
  FdStacks.Send(Value);
end;

{ TElevationGridNode ----------------------------------------------- }

function TElevationGridNodeHelper.GetCcw: boolean;
begin
  Result := FdCcw.Value;
end;

procedure TElevationGridNodeHelper.SetCcw(const Value: boolean);
begin
  FdCcw.Send(Value);
end;

function TElevationGridNodeHelper.GetColorPerVertex: boolean;
begin
  Result := FdColorPerVertex.Value;
end;

procedure TElevationGridNodeHelper.SetColorPerVertex(const Value: boolean);
begin
  FdColorPerVertex.Send(Value);
end;

function TElevationGridNodeHelper.GetCreaseAngle: Single;
begin
  Result := FdCreaseAngle.Value;
end;

procedure TElevationGridNodeHelper.SetCreaseAngle(const Value: Single);
begin
  FdCreaseAngle.Send(Value);
end;

function TElevationGridNodeHelper.GetNormalPerVertex: boolean;
begin
  Result := FdNormalPerVertex.Value;
end;

procedure TElevationGridNodeHelper.SetNormalPerVertex(const Value: boolean);
begin
  FdNormalPerVertex.Send(Value);
end;

function TElevationGridNodeHelper.GetXDimension: Integer;
begin
  Result := FdXDimension.Value;
end;

procedure TElevationGridNodeHelper.SetXDimension(const Value: Integer);
begin
  FdXDimension.Send(Value);
end;

function TElevationGridNodeHelper.GetXSpacing: Single;
begin
  Result := FdXSpacing.Value;
end;

procedure TElevationGridNodeHelper.SetXSpacing(const Value: Single);
begin
  FdXSpacing.Send(Value);
end;

function TElevationGridNodeHelper.GetZDimension: Integer;
begin
  Result := FdZDimension.Value;
end;

procedure TElevationGridNodeHelper.SetZDimension(const Value: Integer);
begin
  FdZDimension.Send(Value);
end;

function TElevationGridNodeHelper.GetZSpacing: Single;
begin
  Result := FdZSpacing.Value;
end;

procedure TElevationGridNodeHelper.SetZSpacing(const Value: Single);
begin
  FdZSpacing.Send(Value);
end;

{ TExtrusionNode ----------------------------------------------- }

function TExtrusionNodeHelper.GetBeginCap: boolean;
begin
  Result := FdBeginCap.Value;
end;

procedure TExtrusionNodeHelper.SetBeginCap(const Value: boolean);
begin
  FdBeginCap.Send(Value);
end;

function TExtrusionNodeHelper.GetCcw: boolean;
begin
  Result := FdCcw.Value;
end;

procedure TExtrusionNodeHelper.SetCcw(const Value: boolean);
begin
  FdCcw.Send(Value);
end;

function TExtrusionNodeHelper.GetConvex: boolean;
begin
  Result := FdConvex.Value;
end;

procedure TExtrusionNodeHelper.SetConvex(const Value: boolean);
begin
  FdConvex.Send(Value);
end;

function TExtrusionNodeHelper.GetCreaseAngle: Single;
begin
  Result := FdCreaseAngle.Value;
end;

procedure TExtrusionNodeHelper.SetCreaseAngle(const Value: Single);
begin
  FdCreaseAngle.Send(Value);
end;

function TExtrusionNodeHelper.GetEndCap: boolean;
begin
  Result := FdEndCap.Value;
end;

procedure TExtrusionNodeHelper.SetEndCap(const Value: boolean);
begin
  FdEndCap.Send(Value);
end;

{ TIndexedFaceSetNode ----------------------------------------------- }

function TIndexedFaceSetNodeHelper.GetConvex: boolean;
begin
  Result := FdConvex.Value;
end;

procedure TIndexedFaceSetNodeHelper.SetConvex(const Value: boolean);
begin
  FdConvex.Send(Value);
end;

function TIndexedFaceSetNodeHelper.GetCreaseAngle: Single;
begin
  Result := FdCreaseAngle.Value;
end;

procedure TIndexedFaceSetNodeHelper.SetCreaseAngle(const Value: Single);
begin
  FdCreaseAngle.Send(Value);
end;

{ TSphereNode ----------------------------------------------- }

function TSphereNodeHelper.GetRadius: Single;
begin
  Result := FdRadius.Value;
end;

procedure TSphereNodeHelper.SetRadius(const Value: Single);
begin
  FdRadius.Send(Value);
end;

function TSphereNodeHelper.GetSlices: Integer;
begin
  Result := FdSlices.Value;
end;

procedure TSphereNodeHelper.SetSlices(const Value: Integer);
begin
  FdSlices.Send(Value);
end;

function TSphereNodeHelper.GetStacks: Integer;
begin
  Result := FdStacks.Value;
end;

procedure TSphereNodeHelper.SetStacks(const Value: Integer);
begin
  FdStacks.Send(Value);
end;

{ TGeoElevationGridNode ----------------------------------------------- }

function TGeoElevationGridNodeHelper.GetYScale: Single;
begin
  Result := FdYScale.Value;
end;

procedure TGeoElevationGridNodeHelper.SetYScale(const Value: Single);
begin
  FdYScale.Send(Value);
end;

function TGeoElevationGridNodeHelper.GetCcw: boolean;
begin
  Result := FdCcw.Value;
end;

procedure TGeoElevationGridNodeHelper.SetCcw(const Value: boolean);
begin
  FdCcw.Send(Value);
end;

function TGeoElevationGridNodeHelper.GetColorPerVertex: boolean;
begin
  Result := FdColorPerVertex.Value;
end;

procedure TGeoElevationGridNodeHelper.SetColorPerVertex(const Value: boolean);
begin
  FdColorPerVertex.Send(Value);
end;

function TGeoElevationGridNodeHelper.GetCreaseAngle: Double;
begin
  Result := FdCreaseAngle.Value;
end;

procedure TGeoElevationGridNodeHelper.SetCreaseAngle(const Value: Double);
begin
  FdCreaseAngle.Send(Value);
end;

function TGeoElevationGridNodeHelper.GetGeoGridOrigin: TVector3Double;
begin
  Result := FdGeoGridOrigin.Value;
end;

procedure TGeoElevationGridNodeHelper.SetGeoGridOrigin(const Value: TVector3Double);
begin
  FdGeoGridOrigin.Send(Value);
end;

function TGeoElevationGridNodeHelper.GetNormalPerVertex: boolean;
begin
  Result := FdNormalPerVertex.Value;
end;

procedure TGeoElevationGridNodeHelper.SetNormalPerVertex(const Value: boolean);
begin
  FdNormalPerVertex.Send(Value);
end;

function TGeoElevationGridNodeHelper.GetXDimension: Integer;
begin
  Result := FdXDimension.Value;
end;

procedure TGeoElevationGridNodeHelper.SetXDimension(const Value: Integer);
begin
  FdXDimension.Send(Value);
end;

function TGeoElevationGridNodeHelper.GetXSpacing: Double;
begin
  Result := FdXSpacing.Value;
end;

procedure TGeoElevationGridNodeHelper.SetXSpacing(const Value: Double);
begin
  FdXSpacing.Send(Value);
end;

function TGeoElevationGridNodeHelper.GetZDimension: Integer;
begin
  Result := FdZDimension.Value;
end;

procedure TGeoElevationGridNodeHelper.SetZDimension(const Value: Integer);
begin
  FdZDimension.Send(Value);
end;

function TGeoElevationGridNodeHelper.GetZSpacing: Double;
begin
  Result := FdZSpacing.Value;
end;

procedure TGeoElevationGridNodeHelper.SetZSpacing(const Value: Double);
begin
  FdZSpacing.Send(Value);
end;

{ TGeoLocationNode ----------------------------------------------- }

function TGeoLocationNodeHelper.GetGeoCoords: TVector3Double;
begin
  Result := FdGeoCoords.Value;
end;

procedure TGeoLocationNodeHelper.SetGeoCoords(const Value: TVector3Double);
begin
  FdGeoCoords.Send(Value);
end;

{ TGeoLODNode ----------------------------------------------- }

function TGeoLODNodeHelper.GetCenter: TVector3Double;
begin
  Result := FdCenter.Value;
end;

procedure TGeoLODNodeHelper.SetCenter(const Value: TVector3Double);
begin
  FdCenter.Send(Value);
end;

function TGeoLODNodeHelper.GetRange: Single;
begin
  Result := FdRange.Value;
end;

procedure TGeoLODNodeHelper.SetRange(const Value: Single);
begin
  FdRange.Send(Value);
end;

function TGeoLODNodeHelper.GetBboxCenter: TVector3Single;
begin
  Result := FdBboxCenter.Value;
end;

procedure TGeoLODNodeHelper.SetBboxCenter(const Value: TVector3Single);
begin
  FdBboxCenter.Send(Value);
end;

function TGeoLODNodeHelper.GetBboxSize: TVector3Single;
begin
  Result := FdBboxSize.Value;
end;

procedure TGeoLODNodeHelper.SetBboxSize(const Value: TVector3Single);
begin
  FdBboxSize.Send(Value);
end;

{ TGeoOriginNode ----------------------------------------------- }

function TGeoOriginNodeHelper.GetGeoCoords: TVector3Double;
begin
  Result := FdGeoCoords.Value;
end;

procedure TGeoOriginNodeHelper.SetGeoCoords(const Value: TVector3Double);
begin
  FdGeoCoords.Send(Value);
end;

function TGeoOriginNodeHelper.GetRotateYUp: boolean;
begin
  Result := FdRotateYUp.Value;
end;

procedure TGeoOriginNodeHelper.SetRotateYUp(const Value: boolean);
begin
  FdRotateYUp.Send(Value);
end;

{ TGeoProximitySensorNode ----------------------------------------------- }

function TGeoProximitySensorNodeHelper.GetGeoCenter: TVector3Double;
begin
  Result := FdGeoCenter.Value;
end;

procedure TGeoProximitySensorNodeHelper.SetGeoCenter(const Value: TVector3Double);
begin
  FdGeoCenter.Send(Value);
end;

{ TGeoTransformNode ----------------------------------------------- }

function TGeoTransformNodeHelper.GetGeoCenter: TVector3Double;
begin
  Result := FdGeoCenter.Value;
end;

procedure TGeoTransformNodeHelper.SetGeoCenter(const Value: TVector3Double);
begin
  FdGeoCenter.Send(Value);
end;

function TGeoTransformNodeHelper.GetRotation: TVector4Single;
begin
  Result := FdRotation.Value;
end;

procedure TGeoTransformNodeHelper.SetRotation(const Value: TVector4Single);
begin
  FdRotation.Send(Value);
end;

function TGeoTransformNodeHelper.GetScale: TVector3Single;
begin
  Result := FdScale.Value;
end;

procedure TGeoTransformNodeHelper.SetScale(const Value: TVector3Single);
begin
  FdScale.Send(Value);
end;

function TGeoTransformNodeHelper.GetScaleOrientation: TVector4Single;
begin
  Result := FdScaleOrientation.Value;
end;

procedure TGeoTransformNodeHelper.SetScaleOrientation(const Value: TVector4Single);
begin
  FdScaleOrientation.Send(Value);
end;

function TGeoTransformNodeHelper.GetTranslation: TVector3Single;
begin
  Result := FdTranslation.Value;
end;

procedure TGeoTransformNodeHelper.SetTranslation(const Value: TVector3Single);
begin
  FdTranslation.Send(Value);
end;

{ TGeoViewpointNode ----------------------------------------------- }

function TGeoViewpointNodeHelper.GetFieldOfView: Single;
begin
  Result := FdFieldOfView.Value;
end;

procedure TGeoViewpointNodeHelper.SetFieldOfView(const Value: Single);
begin
  FdFieldOfView.Send(Value);
end;

function TGeoViewpointNodeHelper.GetHeadlight: boolean;
begin
  Result := FdHeadlight.Value;
end;

procedure TGeoViewpointNodeHelper.SetHeadlight(const Value: boolean);
begin
  FdHeadlight.Send(Value);
end;

function TGeoViewpointNodeHelper.GetPosition: TVector3Double;
begin
  Result := FdPosition.Value;
end;

procedure TGeoViewpointNodeHelper.SetPosition(const Value: TVector3Double);
begin
  FdPosition.Send(Value);
end;

function TGeoViewpointNodeHelper.GetSpeedFactor: Single;
begin
  Result := FdSpeedFactor.Value;
end;

procedure TGeoViewpointNodeHelper.SetSpeedFactor(const Value: Single);
begin
  FdSpeedFactor.Send(Value);
end;

{ TAbstractX3DGroupingNode ----------------------------------------------- }

function TAbstractX3DGroupingNodeHelper.GetBboxCenter: TVector3Single;
begin
  Result := FdBboxCenter.Value;
end;

procedure TAbstractX3DGroupingNodeHelper.SetBboxCenter(const Value: TVector3Single);
begin
  FdBboxCenter.Send(Value);
end;

function TAbstractX3DGroupingNodeHelper.GetBboxSize: TVector3Single;
begin
  Result := FdBboxSize.Value;
end;

procedure TAbstractX3DGroupingNodeHelper.SetBboxSize(const Value: TVector3Single);
begin
  FdBboxSize.Send(Value);
end;

{ TStaticGroupNode ----------------------------------------------- }

function TStaticGroupNodeHelper.GetBboxCenter: TVector3Single;
begin
  Result := FdBboxCenter.Value;
end;

procedure TStaticGroupNodeHelper.SetBboxCenter(const Value: TVector3Single);
begin
  FdBboxCenter.Send(Value);
end;

function TStaticGroupNodeHelper.GetBboxSize: TVector3Single;
begin
  Result := FdBboxSize.Value;
end;

procedure TStaticGroupNodeHelper.SetBboxSize(const Value: TVector3Single);
begin
  FdBboxSize.Send(Value);
end;

{ TSwitchNode ----------------------------------------------- }

function TSwitchNodeHelper.GetWhichChoice: Integer;
begin
  Result := FdWhichChoice.Value;
end;

procedure TSwitchNodeHelper.SetWhichChoice(const Value: Integer);
begin
  FdWhichChoice.Send(Value);
end;

{ TTransformNode ----------------------------------------------- }

function TTransformNodeHelper.GetCenter: TVector3Single;
begin
  Result := FdCenter.Value;
end;

procedure TTransformNodeHelper.SetCenter(const Value: TVector3Single);
begin
  FdCenter.Send(Value);
end;

function TTransformNodeHelper.GetRotation: TVector4Single;
begin
  Result := FdRotation.Value;
end;

procedure TTransformNodeHelper.SetRotation(const Value: TVector4Single);
begin
  FdRotation.Send(Value);
end;

function TTransformNodeHelper.GetScale: TVector3Single;
begin
  Result := FdScale.Value;
end;

procedure TTransformNodeHelper.SetScale(const Value: TVector3Single);
begin
  FdScale.Send(Value);
end;

function TTransformNodeHelper.GetScaleOrientation: TVector4Single;
begin
  Result := FdScaleOrientation.Value;
end;

procedure TTransformNodeHelper.SetScaleOrientation(const Value: TVector4Single);
begin
  FdScaleOrientation.Send(Value);
end;

function TTransformNodeHelper.GetTranslation: TVector3Single;
begin
  Result := FdTranslation.Value;
end;

procedure TTransformNodeHelper.SetTranslation(const Value: TVector3Single);
begin
  FdTranslation.Send(Value);
end;

{ THAnimDisplacerNode ----------------------------------------------- }

function THAnimDisplacerNodeHelper.GetNameField: string;
begin
  Result := FdName.Value;
end;

procedure THAnimDisplacerNodeHelper.SetNameField(const Value: string);
begin
  FdName.Send(Value);
end;

function THAnimDisplacerNodeHelper.GetWeight: Single;
begin
  Result := FdWeight.Value;
end;

procedure THAnimDisplacerNodeHelper.SetWeight(const Value: Single);
begin
  FdWeight.Send(Value);
end;

{ THAnimHumanoidNode ----------------------------------------------- }

function THAnimHumanoidNodeHelper.GetCenter: TVector3Single;
begin
  Result := FdCenter.Value;
end;

procedure THAnimHumanoidNodeHelper.SetCenter(const Value: TVector3Single);
begin
  FdCenter.Send(Value);
end;

function THAnimHumanoidNodeHelper.GetNameField: string;
begin
  Result := FdName.Value;
end;

procedure THAnimHumanoidNodeHelper.SetNameField(const Value: string);
begin
  FdName.Send(Value);
end;

function THAnimHumanoidNodeHelper.GetRotation: TVector4Single;
begin
  Result := FdRotation.Value;
end;

procedure THAnimHumanoidNodeHelper.SetRotation(const Value: TVector4Single);
begin
  FdRotation.Send(Value);
end;

function THAnimHumanoidNodeHelper.GetScale: TVector3Single;
begin
  Result := FdScale.Value;
end;

procedure THAnimHumanoidNodeHelper.SetScale(const Value: TVector3Single);
begin
  FdScale.Send(Value);
end;

function THAnimHumanoidNodeHelper.GetScaleOrientation: TVector4Single;
begin
  Result := FdScaleOrientation.Value;
end;

procedure THAnimHumanoidNodeHelper.SetScaleOrientation(const Value: TVector4Single);
begin
  FdScaleOrientation.Send(Value);
end;

function THAnimHumanoidNodeHelper.GetTranslation: TVector3Single;
begin
  Result := FdTranslation.Value;
end;

procedure THAnimHumanoidNodeHelper.SetTranslation(const Value: TVector3Single);
begin
  FdTranslation.Send(Value);
end;

function THAnimHumanoidNodeHelper.GetVersion: string;
begin
  Result := FdVersion.Value;
end;

procedure THAnimHumanoidNodeHelper.SetVersion(const Value: string);
begin
  FdVersion.Send(Value);
end;

function THAnimHumanoidNodeHelper.GetBboxCenter: TVector3Single;
begin
  Result := FdBboxCenter.Value;
end;

procedure THAnimHumanoidNodeHelper.SetBboxCenter(const Value: TVector3Single);
begin
  FdBboxCenter.Send(Value);
end;

function THAnimHumanoidNodeHelper.GetBboxSize: TVector3Single;
begin
  Result := FdBboxSize.Value;
end;

procedure THAnimHumanoidNodeHelper.SetBboxSize(const Value: TVector3Single);
begin
  FdBboxSize.Send(Value);
end;

{ THAnimJointNode ----------------------------------------------- }

function THAnimJointNodeHelper.GetCenter: TVector3Single;
begin
  Result := FdCenter.Value;
end;

procedure THAnimJointNodeHelper.SetCenter(const Value: TVector3Single);
begin
  FdCenter.Send(Value);
end;

function THAnimJointNodeHelper.GetLimitOrientation: TVector4Single;
begin
  Result := FdLimitOrientation.Value;
end;

procedure THAnimJointNodeHelper.SetLimitOrientation(const Value: TVector4Single);
begin
  FdLimitOrientation.Send(Value);
end;

function THAnimJointNodeHelper.GetNameField: string;
begin
  Result := FdName.Value;
end;

procedure THAnimJointNodeHelper.SetNameField(const Value: string);
begin
  FdName.Send(Value);
end;

function THAnimJointNodeHelper.GetRotation: TVector4Single;
begin
  Result := FdRotation.Value;
end;

procedure THAnimJointNodeHelper.SetRotation(const Value: TVector4Single);
begin
  FdRotation.Send(Value);
end;

function THAnimJointNodeHelper.GetScale: TVector3Single;
begin
  Result := FdScale.Value;
end;

procedure THAnimJointNodeHelper.SetScale(const Value: TVector3Single);
begin
  FdScale.Send(Value);
end;

function THAnimJointNodeHelper.GetScaleOrientation: TVector4Single;
begin
  Result := FdScaleOrientation.Value;
end;

procedure THAnimJointNodeHelper.SetScaleOrientation(const Value: TVector4Single);
begin
  FdScaleOrientation.Send(Value);
end;

function THAnimJointNodeHelper.GetTranslation: TVector3Single;
begin
  Result := FdTranslation.Value;
end;

procedure THAnimJointNodeHelper.SetTranslation(const Value: TVector3Single);
begin
  FdTranslation.Send(Value);
end;

{ THAnimSegmentNode ----------------------------------------------- }

function THAnimSegmentNodeHelper.GetCenterOfMass: TVector3Single;
begin
  Result := FdCenterOfMass.Value;
end;

procedure THAnimSegmentNodeHelper.SetCenterOfMass(const Value: TVector3Single);
begin
  FdCenterOfMass.Send(Value);
end;

function THAnimSegmentNodeHelper.GetMass: Single;
begin
  Result := FdMass.Value;
end;

procedure THAnimSegmentNodeHelper.SetMass(const Value: Single);
begin
  FdMass.Send(Value);
end;

function THAnimSegmentNodeHelper.GetNameField: string;
begin
  Result := FdName.Value;
end;

procedure THAnimSegmentNodeHelper.SetNameField(const Value: string);
begin
  FdName.Send(Value);
end;

{ THAnimSiteNode ----------------------------------------------- }

function THAnimSiteNodeHelper.GetCenter: TVector3Single;
begin
  Result := FdCenter.Value;
end;

procedure THAnimSiteNodeHelper.SetCenter(const Value: TVector3Single);
begin
  FdCenter.Send(Value);
end;

function THAnimSiteNodeHelper.GetNameField: string;
begin
  Result := FdName.Value;
end;

procedure THAnimSiteNodeHelper.SetNameField(const Value: string);
begin
  FdName.Send(Value);
end;

function THAnimSiteNodeHelper.GetRotation: TVector4Single;
begin
  Result := FdRotation.Value;
end;

procedure THAnimSiteNodeHelper.SetRotation(const Value: TVector4Single);
begin
  FdRotation.Send(Value);
end;

function THAnimSiteNodeHelper.GetScale: TVector3Single;
begin
  Result := FdScale.Value;
end;

procedure THAnimSiteNodeHelper.SetScale(const Value: TVector3Single);
begin
  FdScale.Send(Value);
end;

function THAnimSiteNodeHelper.GetScaleOrientation: TVector4Single;
begin
  Result := FdScaleOrientation.Value;
end;

procedure THAnimSiteNodeHelper.SetScaleOrientation(const Value: TVector4Single);
begin
  FdScaleOrientation.Send(Value);
end;

function THAnimSiteNodeHelper.GetTranslation: TVector3Single;
begin
  Result := FdTranslation.Value;
end;

procedure THAnimSiteNodeHelper.SetTranslation(const Value: TVector3Single);
begin
  FdTranslation.Send(Value);
end;

{ TSplinePositionInterpolatorNode ----------------------------------------------- }

function TSplinePositionInterpolatorNodeHelper.GetClosed: boolean;
begin
  Result := FdClosed.Value;
end;

procedure TSplinePositionInterpolatorNodeHelper.SetClosed(const Value: boolean);
begin
  FdClosed.Send(Value);
end;

function TSplinePositionInterpolatorNodeHelper.GetNormalizeVelocity: boolean;
begin
  Result := FdNormalizeVelocity.Value;
end;

procedure TSplinePositionInterpolatorNodeHelper.SetNormalizeVelocity(const Value: boolean);
begin
  FdNormalizeVelocity.Send(Value);
end;

{ TSplinePositionInterpolator2DNode ----------------------------------------------- }

function TSplinePositionInterpolator2DNodeHelper.GetClosed: boolean;
begin
  Result := FdClosed.Value;
end;

procedure TSplinePositionInterpolator2DNodeHelper.SetClosed(const Value: boolean);
begin
  FdClosed.Send(Value);
end;

function TSplinePositionInterpolator2DNodeHelper.GetNormalizeVelocity: boolean;
begin
  Result := FdNormalizeVelocity.Value;
end;

procedure TSplinePositionInterpolator2DNodeHelper.SetNormalizeVelocity(const Value: boolean);
begin
  FdNormalizeVelocity.Send(Value);
end;

{ TSplineScalarInterpolatorNode ----------------------------------------------- }

function TSplineScalarInterpolatorNodeHelper.GetClosed: boolean;
begin
  Result := FdClosed.Value;
end;

procedure TSplineScalarInterpolatorNodeHelper.SetClosed(const Value: boolean);
begin
  FdClosed.Send(Value);
end;

function TSplineScalarInterpolatorNodeHelper.GetNormalizeVelocity: boolean;
begin
  Result := FdNormalizeVelocity.Value;
end;

procedure TSplineScalarInterpolatorNodeHelper.SetNormalizeVelocity(const Value: boolean);
begin
  FdNormalizeVelocity.Send(Value);
end;

{ TSquadOrientationInterpolatorNode ----------------------------------------------- }

function TSquadOrientationInterpolatorNodeHelper.GetNormalizeVelocity: boolean;
begin
  Result := FdNormalizeVelocity.Value;
end;

procedure TSquadOrientationInterpolatorNodeHelper.SetNormalizeVelocity(const Value: boolean);
begin
  FdNormalizeVelocity.Send(Value);
end;

{ TStringSensorNode ----------------------------------------------- }

function TStringSensorNodeHelper.GetDeletionAllowed: boolean;
begin
  Result := FdDeletionAllowed.Value;
end;

procedure TStringSensorNodeHelper.SetDeletionAllowed(const Value: boolean);
begin
  FdDeletionAllowed.Send(Value);
end;

{ TAbstractLayerNode ----------------------------------------------- }

function TAbstractLayerNodeHelper.GetIsPickable: boolean;
begin
  Result := FdIsPickable.Value;
end;

procedure TAbstractLayerNodeHelper.SetIsPickable(const Value: boolean);
begin
  FdIsPickable.Send(Value);
end;

{ TLayerSetNode ----------------------------------------------- }

function TLayerSetNodeHelper.GetActiveLayer: Integer;
begin
  Result := FdActiveLayer.Value;
end;

procedure TLayerSetNodeHelper.SetActiveLayer(const Value: Integer);
begin
  FdActiveLayer.Send(Value);
end;

{ TScreenFontStyleNode ----------------------------------------------- }

function TScreenFontStyleNodeHelper.GetHorizontal: boolean;
begin
  Result := FdHorizontal.Value;
end;

procedure TScreenFontStyleNodeHelper.SetHorizontal(const Value: boolean);
begin
  FdHorizontal.Send(Value);
end;

function TScreenFontStyleNodeHelper.GetLanguage: string;
begin
  Result := FdLanguage.Value;
end;

procedure TScreenFontStyleNodeHelper.SetLanguage(const Value: string);
begin
  FdLanguage.Send(Value);
end;

function TScreenFontStyleNodeHelper.GetLeftToRight: boolean;
begin
  Result := FdLeftToRight.Value;
end;

procedure TScreenFontStyleNodeHelper.SetLeftToRight(const Value: boolean);
begin
  FdLeftToRight.Send(Value);
end;

function TScreenFontStyleNodeHelper.GetPointSize: Single;
begin
  Result := FdPointSize.Value;
end;

procedure TScreenFontStyleNodeHelper.SetPointSize(const Value: Single);
begin
  FdPointSize.Send(Value);
end;

function TScreenFontStyleNodeHelper.GetSpacing: Single;
begin
  Result := FdSpacing.Value;
end;

procedure TScreenFontStyleNodeHelper.SetSpacing(const Value: Single);
begin
  FdSpacing.Send(Value);
end;

function TScreenFontStyleNodeHelper.GetTopToBottom: boolean;
begin
  Result := FdTopToBottom.Value;
end;

procedure TScreenFontStyleNodeHelper.SetTopToBottom(const Value: boolean);
begin
  FdTopToBottom.Send(Value);
end;

{ TAbstractLightNode ----------------------------------------------- }

function TAbstractLightNodeHelper.GetAmbientIntensity: Single;
begin
  Result := FdAmbientIntensity.Value;
end;

procedure TAbstractLightNodeHelper.SetAmbientIntensity(const Value: Single);
begin
  FdAmbientIntensity.Send(Value);
end;

function TAbstractLightNodeHelper.GetColor: TCastleColorRGB;
begin
  Result := FdColor.Value;
end;

procedure TAbstractLightNodeHelper.SetColor(const Value: TCastleColorRGB);
begin
  FdColor.Send(Value);
end;

function TAbstractLightNodeHelper.GetGlobal: boolean;
begin
  Result := FdGlobal.Value;
end;

procedure TAbstractLightNodeHelper.SetGlobal(const Value: boolean);
begin
  FdGlobal.Send(Value);
end;

function TAbstractLightNodeHelper.GetIntensity: Single;
begin
  Result := FdIntensity.Value;
end;

procedure TAbstractLightNodeHelper.SetIntensity(const Value: Single);
begin
  FdIntensity.Send(Value);
end;

function TAbstractLightNodeHelper.GetIsOn: boolean;
begin
  Result := FdOn.Value;
end;

procedure TAbstractLightNodeHelper.SetIsOn(const Value: boolean);
begin
  FdOn.Send(Value);
end;

function TAbstractLightNodeHelper.GetShadows: boolean;
begin
  Result := FdShadows.Value;
end;

procedure TAbstractLightNodeHelper.SetShadows(const Value: boolean);
begin
  FdShadows.Send(Value);
end;

function TAbstractLightNodeHelper.GetShadowVolumes: boolean;
begin
  Result := FdShadowVolumes.Value;
end;

procedure TAbstractLightNodeHelper.SetShadowVolumes(const Value: boolean);
begin
  FdShadowVolumes.Send(Value);
end;

function TAbstractLightNodeHelper.GetShadowVolumesMain: boolean;
begin
  Result := FdShadowVolumesMain.Value;
end;

procedure TAbstractLightNodeHelper.SetShadowVolumesMain(const Value: boolean);
begin
  FdShadowVolumesMain.Send(Value);
end;

function TAbstractLightNodeHelper.GetProjectionNear: Single;
begin
  Result := FdProjectionNear.Value;
end;

procedure TAbstractLightNodeHelper.SetProjectionNear(const Value: Single);
begin
  FdProjectionNear.Send(Value);
end;

function TAbstractLightNodeHelper.GetProjectionFar: Single;
begin
  Result := FdProjectionFar.Value;
end;

procedure TAbstractLightNodeHelper.SetProjectionFar(const Value: Single);
begin
  FdProjectionFar.Send(Value);
end;

function TAbstractLightNodeHelper.GetUp: TVector3Single;
begin
  Result := FdUp.Value;
end;

procedure TAbstractLightNodeHelper.SetUp(const Value: TVector3Single);
begin
  FdUp.Send(Value);
end;

{ TSpotLightNode ----------------------------------------------- }

function TSpotLightNodeHelper.GetAttenuation: TVector3Single;
begin
  Result := FdAttenuation.Value;
end;

procedure TSpotLightNodeHelper.SetAttenuation(const Value: TVector3Single);
begin
  FdAttenuation.Send(Value);
end;

function TSpotLightNodeHelper.GetBeamWidth: Single;
begin
  Result := FdBeamWidth.Value;
end;

procedure TSpotLightNodeHelper.SetBeamWidth(const Value: Single);
begin
  FdBeamWidth.Send(Value);
end;

function TSpotLightNodeHelper.GetCutOffAngle: Single;
begin
  Result := FdCutOffAngle.Value;
end;

procedure TSpotLightNodeHelper.SetCutOffAngle(const Value: Single);
begin
  FdCutOffAngle.Send(Value);
end;

function TSpotLightNodeHelper.GetDirection: TVector3Single;
begin
  Result := FdDirection.Value;
end;

procedure TSpotLightNodeHelper.SetDirection(const Value: TVector3Single);
begin
  FdDirection.Send(Value);
end;

function TSpotLightNodeHelper.GetLocation: TVector3Single;
begin
  Result := FdLocation.Value;
end;

procedure TSpotLightNodeHelper.SetLocation(const Value: TVector3Single);
begin
  FdLocation.Send(Value);
end;

function TSpotLightNodeHelper.GetRadius: Single;
begin
  Result := FdRadius.Value;
end;

procedure TSpotLightNodeHelper.SetRadius(const Value: Single);
begin
  FdRadius.Send(Value);
end;

function TSpotLightNodeHelper.GetProjectionAngle: Single;
begin
  Result := FdProjectionAngle.Value;
end;

procedure TSpotLightNodeHelper.SetProjectionAngle(const Value: Single);
begin
  FdProjectionAngle.Send(Value);
end;

{ TAbstractX3DViewpointNode ----------------------------------------------- }

function TAbstractX3DViewpointNodeHelper.GetCenterOfRotation: TVector3Single;
begin
  Result := FdCenterOfRotation.Value;
end;

procedure TAbstractX3DViewpointNodeHelper.SetCenterOfRotation(const Value: TVector3Single);
begin
  FdCenterOfRotation.Send(Value);
end;

function TAbstractX3DViewpointNodeHelper.GetDescription: string;
begin
  Result := FdDescription.Value;
end;

procedure TAbstractX3DViewpointNodeHelper.SetDescription(const Value: string);
begin
  FdDescription.Send(Value);
end;

function TAbstractX3DViewpointNodeHelper.GetJump: boolean;
begin
  Result := FdJump.Value;
end;

procedure TAbstractX3DViewpointNodeHelper.SetJump(const Value: boolean);
begin
  FdJump.Send(Value);
end;

function TAbstractX3DViewpointNodeHelper.GetOrientation: TVector4Single;
begin
  Result := FdOrientation.Value;
end;

procedure TAbstractX3DViewpointNodeHelper.SetOrientation(const Value: TVector4Single);
begin
  FdOrientation.Send(Value);
end;

function TAbstractX3DViewpointNodeHelper.GetRetainUserOffsets: boolean;
begin
  Result := FdRetainUserOffsets.Value;
end;

procedure TAbstractX3DViewpointNodeHelper.SetRetainUserOffsets(const Value: boolean);
begin
  FdRetainUserOffsets.Send(Value);
end;

function TAbstractX3DViewpointNodeHelper.GetCameraMatrixSendAlsoOnOffscreenRendering: boolean;
begin
  Result := FdCameraMatrixSendAlsoOnOffscreenRendering.Value;
end;

procedure TAbstractX3DViewpointNodeHelper.SetCameraMatrixSendAlsoOnOffscreenRendering(const Value: boolean);
begin
  FdCameraMatrixSendAlsoOnOffscreenRendering.Send(Value);
end;

function TAbstractX3DViewpointNodeHelper.GetGravityUp: TVector3Single;
begin
  Result := FdGravityUp.Value;
end;

procedure TAbstractX3DViewpointNodeHelper.SetGravityUp(const Value: TVector3Single);
begin
  FdGravityUp.Send(Value);
end;

{ TBillboardNode ----------------------------------------------- }

function TBillboardNodeHelper.GetAxisOfRotation: TVector3Single;
begin
  Result := FdAxisOfRotation.Value;
end;

procedure TBillboardNodeHelper.SetAxisOfRotation(const Value: TVector3Single);
begin
  FdAxisOfRotation.Send(Value);
end;

{ TCollisionNode ----------------------------------------------- }

function TCollisionNodeHelper.GetEnabled: boolean;
begin
  Result := FdEnabled.Value;
end;

procedure TCollisionNodeHelper.SetEnabled(const Value: boolean);
begin
  FdEnabled.Send(Value);
end;

{ TLODNode ----------------------------------------------- }

function TLODNodeHelper.GetCenter: TVector3Single;
begin
  Result := FdCenter.Value;
end;

procedure TLODNodeHelper.SetCenter(const Value: TVector3Single);
begin
  FdCenter.Send(Value);
end;

function TLODNodeHelper.GetForceTransitions: boolean;
begin
  Result := FdForceTransitions.Value;
end;

procedure TLODNodeHelper.SetForceTransitions(const Value: boolean);
begin
  FdForceTransitions.Send(Value);
end;

{ TNavigationInfoNode ----------------------------------------------- }

function TNavigationInfoNodeHelper.GetHeadlight: boolean;
begin
  Result := FdHeadlight.Value;
end;

procedure TNavigationInfoNodeHelper.SetHeadlight(const Value: boolean);
begin
  FdHeadlight.Send(Value);
end;

function TNavigationInfoNodeHelper.GetSpeed: Single;
begin
  Result := FdSpeed.Value;
end;

procedure TNavigationInfoNodeHelper.SetSpeed(const Value: Single);
begin
  FdSpeed.Send(Value);
end;

function TNavigationInfoNodeHelper.GetTransitionTime: TFloatTime;
begin
  Result := FdTransitionTime.Value;
end;

procedure TNavigationInfoNodeHelper.SetTransitionTime(const Value: TFloatTime);
begin
  FdTransitionTime.Send(Value);
end;

function TNavigationInfoNodeHelper.GetVisibilityLimit: Single;
begin
  Result := FdVisibilityLimit.Value;
end;

procedure TNavigationInfoNodeHelper.SetVisibilityLimit(const Value: Single);
begin
  FdVisibilityLimit.Send(Value);
end;

{ TOrthoViewpointNode ----------------------------------------------- }

function TOrthoViewpointNodeHelper.GetPosition: TVector3Single;
begin
  Result := FdPosition.Value;
end;

procedure TOrthoViewpointNodeHelper.SetPosition(const Value: TVector3Single);
begin
  FdPosition.Send(Value);
end;

{ TViewpointNode ----------------------------------------------- }

function TViewpointNodeHelper.GetFieldOfView: Single;
begin
  Result := FdFieldOfView.Value;
end;

procedure TViewpointNodeHelper.SetFieldOfView(const Value: Single);
begin
  FdFieldOfView.Send(Value);
end;

function TViewpointNodeHelper.GetPosition: TVector3Single;
begin
  Result := FdPosition.Value;
end;

procedure TViewpointNodeHelper.SetPosition(const Value: TVector3Single);
begin
  FdPosition.Send(Value);
end;

{ TViewpointGroupNode ----------------------------------------------- }

function TViewpointGroupNodeHelper.GetCenter: TVector3Single;
begin
  Result := FdCenter.Value;
end;

procedure TViewpointGroupNodeHelper.SetCenter(const Value: TVector3Single);
begin
  FdCenter.Send(Value);
end;

function TViewpointGroupNodeHelper.GetDescription: string;
begin
  Result := FdDescription.Value;
end;

procedure TViewpointGroupNodeHelper.SetDescription(const Value: string);
begin
  FdDescription.Send(Value);
end;

function TViewpointGroupNodeHelper.GetDisplayed: boolean;
begin
  Result := FdDisplayed.Value;
end;

procedure TViewpointGroupNodeHelper.SetDisplayed(const Value: boolean);
begin
  FdDisplayed.Send(Value);
end;

function TViewpointGroupNodeHelper.GetRetainUserOffsets: boolean;
begin
  Result := FdRetainUserOffsets.Value;
end;

procedure TViewpointGroupNodeHelper.SetRetainUserOffsets(const Value: boolean);
begin
  FdRetainUserOffsets.Send(Value);
end;

function TViewpointGroupNodeHelper.GetSize: TVector3Single;
begin
  Result := FdSize.Value;
end;

procedure TViewpointGroupNodeHelper.SetSize(const Value: TVector3Single);
begin
  FdSize.Send(Value);
end;

{ TAnchorNode ----------------------------------------------- }

function TAnchorNodeHelper.GetDescription: string;
begin
  Result := FdDescription.Value;
end;

procedure TAnchorNodeHelper.SetDescription(const Value: string);
begin
  FdDescription.Send(Value);
end;

{ TInlineNode ----------------------------------------------- }

function TInlineNodeHelper.GetLoad: boolean;
begin
  Result := FdLoad.Value;
end;

procedure TInlineNodeHelper.SetLoad(const Value: boolean);
begin
  FdLoad.Send(Value);
end;

function TInlineNodeHelper.GetBboxCenter: TVector3Single;
begin
  Result := FdBboxCenter.Value;
end;

procedure TInlineNodeHelper.SetBboxCenter(const Value: TVector3Single);
begin
  FdBboxCenter.Send(Value);
end;

function TInlineNodeHelper.GetBboxSize: TVector3Single;
begin
  Result := FdBboxSize.Value;
end;

procedure TInlineNodeHelper.SetBboxSize(const Value: TVector3Single);
begin
  FdBboxSize.Send(Value);
end;

{ TLoadSensorNode ----------------------------------------------- }

function TLoadSensorNodeHelper.GetTimeOut: TFloatTime;
begin
  Result := FdTimeOut.Value;
end;

procedure TLoadSensorNodeHelper.SetTimeOut(const Value: TFloatTime);
begin
  FdTimeOut.Send(Value);
end;

{ TAbstractNurbsSurfaceGeometryNode ----------------------------------------------- }

function TAbstractNurbsSurfaceGeometryNodeHelper.GetUTessellation: Integer;
begin
  Result := FdUTessellation.Value;
end;

procedure TAbstractNurbsSurfaceGeometryNodeHelper.SetUTessellation(const Value: Integer);
begin
  FdUTessellation.Send(Value);
end;

function TAbstractNurbsSurfaceGeometryNodeHelper.GetVTessellation: Integer;
begin
  Result := FdVTessellation.Value;
end;

procedure TAbstractNurbsSurfaceGeometryNodeHelper.SetVTessellation(const Value: Integer);
begin
  FdVTessellation.Send(Value);
end;

function TAbstractNurbsSurfaceGeometryNodeHelper.GetUClosed: boolean;
begin
  Result := FdUClosed.Value;
end;

procedure TAbstractNurbsSurfaceGeometryNodeHelper.SetUClosed(const Value: boolean);
begin
  FdUClosed.Send(Value);
end;

function TAbstractNurbsSurfaceGeometryNodeHelper.GetUDimension: Integer;
begin
  Result := FdUDimension.Value;
end;

procedure TAbstractNurbsSurfaceGeometryNodeHelper.SetUDimension(const Value: Integer);
begin
  FdUDimension.Send(Value);
end;

function TAbstractNurbsSurfaceGeometryNodeHelper.GetUOrder: Integer;
begin
  Result := FdUOrder.Value;
end;

procedure TAbstractNurbsSurfaceGeometryNodeHelper.SetUOrder(const Value: Integer);
begin
  FdUOrder.Send(Value);
end;

function TAbstractNurbsSurfaceGeometryNodeHelper.GetVClosed: boolean;
begin
  Result := FdVClosed.Value;
end;

procedure TAbstractNurbsSurfaceGeometryNodeHelper.SetVClosed(const Value: boolean);
begin
  FdVClosed.Send(Value);
end;

function TAbstractNurbsSurfaceGeometryNodeHelper.GetVDimension: Integer;
begin
  Result := FdVDimension.Value;
end;

procedure TAbstractNurbsSurfaceGeometryNodeHelper.SetVDimension(const Value: Integer);
begin
  FdVDimension.Send(Value);
end;

function TAbstractNurbsSurfaceGeometryNodeHelper.GetVOrder: Integer;
begin
  Result := FdVOrder.Value;
end;

procedure TAbstractNurbsSurfaceGeometryNodeHelper.SetVOrder(const Value: Integer);
begin
  FdVOrder.Send(Value);
end;

{ TNurbsCurveNode ----------------------------------------------- }

function TNurbsCurveNodeHelper.GetTessellation: Integer;
begin
  Result := FdTessellation.Value;
end;

procedure TNurbsCurveNodeHelper.SetTessellation(const Value: Integer);
begin
  FdTessellation.Send(Value);
end;

function TNurbsCurveNodeHelper.GetClosed: boolean;
begin
  Result := FdClosed.Value;
end;

procedure TNurbsCurveNodeHelper.SetClosed(const Value: boolean);
begin
  FdClosed.Send(Value);
end;

function TNurbsCurveNodeHelper.GetOrder: Integer;
begin
  Result := FdOrder.Value;
end;

procedure TNurbsCurveNodeHelper.SetOrder(const Value: Integer);
begin
  FdOrder.Send(Value);
end;

{ TNurbsCurve2DNode ----------------------------------------------- }

function TNurbsCurve2DNodeHelper.GetTessellation: Integer;
begin
  Result := FdTessellation.Value;
end;

procedure TNurbsCurve2DNodeHelper.SetTessellation(const Value: Integer);
begin
  FdTessellation.Send(Value);
end;

function TNurbsCurve2DNodeHelper.GetClosed: boolean;
begin
  Result := FdClosed.Value;
end;

procedure TNurbsCurve2DNodeHelper.SetClosed(const Value: boolean);
begin
  FdClosed.Send(Value);
end;

function TNurbsCurve2DNodeHelper.GetOrder: Integer;
begin
  Result := FdOrder.Value;
end;

procedure TNurbsCurve2DNodeHelper.SetOrder(const Value: Integer);
begin
  FdOrder.Send(Value);
end;

{ TNurbsOrientationInterpolatorNode ----------------------------------------------- }

function TNurbsOrientationInterpolatorNodeHelper.GetOrder: Integer;
begin
  Result := FdOrder.Value;
end;

procedure TNurbsOrientationInterpolatorNodeHelper.SetOrder(const Value: Integer);
begin
  FdOrder.Send(Value);
end;

{ TNurbsPositionInterpolatorNode ----------------------------------------------- }

function TNurbsPositionInterpolatorNodeHelper.GetOrder: Integer;
begin
  Result := FdOrder.Value;
end;

procedure TNurbsPositionInterpolatorNodeHelper.SetOrder(const Value: Integer);
begin
  FdOrder.Send(Value);
end;

{ TNurbsSetNode ----------------------------------------------- }

function TNurbsSetNodeHelper.GetTessellationScale: Single;
begin
  Result := FdTessellationScale.Value;
end;

procedure TNurbsSetNodeHelper.SetTessellationScale(const Value: Single);
begin
  FdTessellationScale.Send(Value);
end;

function TNurbsSetNodeHelper.GetBboxCenter: TVector3Single;
begin
  Result := FdBboxCenter.Value;
end;

procedure TNurbsSetNodeHelper.SetBboxCenter(const Value: TVector3Single);
begin
  FdBboxCenter.Send(Value);
end;

function TNurbsSetNodeHelper.GetBboxSize: TVector3Single;
begin
  Result := FdBboxSize.Value;
end;

procedure TNurbsSetNodeHelper.SetBboxSize(const Value: TVector3Single);
begin
  FdBboxSize.Send(Value);
end;

{ TNurbsSurfaceInterpolatorNode ----------------------------------------------- }

function TNurbsSurfaceInterpolatorNodeHelper.GetUDimension: Integer;
begin
  Result := FdUDimension.Value;
end;

procedure TNurbsSurfaceInterpolatorNodeHelper.SetUDimension(const Value: Integer);
begin
  FdUDimension.Send(Value);
end;

function TNurbsSurfaceInterpolatorNodeHelper.GetUOrder: Integer;
begin
  Result := FdUOrder.Value;
end;

procedure TNurbsSurfaceInterpolatorNodeHelper.SetUOrder(const Value: Integer);
begin
  FdUOrder.Send(Value);
end;

function TNurbsSurfaceInterpolatorNodeHelper.GetVDimension: Integer;
begin
  Result := FdVDimension.Value;
end;

procedure TNurbsSurfaceInterpolatorNodeHelper.SetVDimension(const Value: Integer);
begin
  FdVDimension.Send(Value);
end;

function TNurbsSurfaceInterpolatorNodeHelper.GetVOrder: Integer;
begin
  Result := FdVOrder.Value;
end;

procedure TNurbsSurfaceInterpolatorNodeHelper.SetVOrder(const Value: Integer);
begin
  FdVOrder.Send(Value);
end;

{ TNurbsSweptSurfaceNode ----------------------------------------------- }

function TNurbsSweptSurfaceNodeHelper.GetCcw: boolean;
begin
  Result := FdCcw.Value;
end;

procedure TNurbsSweptSurfaceNodeHelper.SetCcw(const Value: boolean);
begin
  FdCcw.Send(Value);
end;

{ TNurbsSwungSurfaceNode ----------------------------------------------- }

function TNurbsSwungSurfaceNodeHelper.GetCcw: boolean;
begin
  Result := FdCcw.Value;
end;

procedure TNurbsSwungSurfaceNodeHelper.SetCcw(const Value: boolean);
begin
  FdCcw.Send(Value);
end;

{ TNurbsTextureCoordinateNode ----------------------------------------------- }

function TNurbsTextureCoordinateNodeHelper.GetUDimension: Integer;
begin
  Result := FdUDimension.Value;
end;

procedure TNurbsTextureCoordinateNodeHelper.SetUDimension(const Value: Integer);
begin
  FdUDimension.Send(Value);
end;

function TNurbsTextureCoordinateNodeHelper.GetUOrder: Integer;
begin
  Result := FdUOrder.Value;
end;

procedure TNurbsTextureCoordinateNodeHelper.SetUOrder(const Value: Integer);
begin
  FdUOrder.Send(Value);
end;

function TNurbsTextureCoordinateNodeHelper.GetVDimension: Integer;
begin
  Result := FdVDimension.Value;
end;

procedure TNurbsTextureCoordinateNodeHelper.SetVDimension(const Value: Integer);
begin
  FdVDimension.Send(Value);
end;

function TNurbsTextureCoordinateNodeHelper.GetVOrder: Integer;
begin
  Result := FdVOrder.Value;
end;

procedure TNurbsTextureCoordinateNodeHelper.SetVOrder(const Value: Integer);
begin
  FdVOrder.Send(Value);
end;

{ TAbstractParticleEmitterNode ----------------------------------------------- }

function TAbstractParticleEmitterNodeHelper.GetSpeed: Single;
begin
  Result := FdSpeed.Value;
end;

procedure TAbstractParticleEmitterNodeHelper.SetSpeed(const Value: Single);
begin
  FdSpeed.Send(Value);
end;

function TAbstractParticleEmitterNodeHelper.GetVariation: Single;
begin
  Result := FdVariation.Value;
end;

procedure TAbstractParticleEmitterNodeHelper.SetVariation(const Value: Single);
begin
  FdVariation.Send(Value);
end;

function TAbstractParticleEmitterNodeHelper.GetMass: Single;
begin
  Result := FdMass.Value;
end;

procedure TAbstractParticleEmitterNodeHelper.SetMass(const Value: Single);
begin
  FdMass.Send(Value);
end;

function TAbstractParticleEmitterNodeHelper.GetSurfaceArea: Single;
begin
  Result := FdSurfaceArea.Value;
end;

procedure TAbstractParticleEmitterNodeHelper.SetSurfaceArea(const Value: Single);
begin
  FdSurfaceArea.Send(Value);
end;

{ TAbstractParticlePhysicsModelNode ----------------------------------------------- }

function TAbstractParticlePhysicsModelNodeHelper.GetEnabled: boolean;
begin
  Result := FdEnabled.Value;
end;

procedure TAbstractParticlePhysicsModelNodeHelper.SetEnabled(const Value: boolean);
begin
  FdEnabled.Send(Value);
end;

{ TConeEmitterNode ----------------------------------------------- }

function TConeEmitterNodeHelper.GetAngle: Single;
begin
  Result := FdAngle.Value;
end;

procedure TConeEmitterNodeHelper.SetAngle(const Value: Single);
begin
  FdAngle.Send(Value);
end;

function TConeEmitterNodeHelper.GetDirection: TVector3Single;
begin
  Result := FdDirection.Value;
end;

procedure TConeEmitterNodeHelper.SetDirection(const Value: TVector3Single);
begin
  FdDirection.Send(Value);
end;

function TConeEmitterNodeHelper.GetPosition: TVector3Single;
begin
  Result := FdPosition.Value;
end;

procedure TConeEmitterNodeHelper.SetPosition(const Value: TVector3Single);
begin
  FdPosition.Send(Value);
end;

{ TExplosionEmitterNode ----------------------------------------------- }

function TExplosionEmitterNodeHelper.GetPosition: TVector3Single;
begin
  Result := FdPosition.Value;
end;

procedure TExplosionEmitterNodeHelper.SetPosition(const Value: TVector3Single);
begin
  FdPosition.Send(Value);
end;

{ TParticleSystemNode ----------------------------------------------- }

function TParticleSystemNodeHelper.GetCreateParticles: boolean;
begin
  Result := FdCreateParticles.Value;
end;

procedure TParticleSystemNodeHelper.SetCreateParticles(const Value: boolean);
begin
  FdCreateParticles.Send(Value);
end;

function TParticleSystemNodeHelper.GetEnabled: boolean;
begin
  Result := FdEnabled.Value;
end;

procedure TParticleSystemNodeHelper.SetEnabled(const Value: boolean);
begin
  FdEnabled.Send(Value);
end;

function TParticleSystemNodeHelper.GetLifetimeVariation: Single;
begin
  Result := FdLifetimeVariation.Value;
end;

procedure TParticleSystemNodeHelper.SetLifetimeVariation(const Value: Single);
begin
  FdLifetimeVariation.Send(Value);
end;

function TParticleSystemNodeHelper.GetMaxParticles: Integer;
begin
  Result := FdMaxParticles.Value;
end;

procedure TParticleSystemNodeHelper.SetMaxParticles(const Value: Integer);
begin
  FdMaxParticles.Send(Value);
end;

function TParticleSystemNodeHelper.GetParticleLifetime: Single;
begin
  Result := FdParticleLifetime.Value;
end;

procedure TParticleSystemNodeHelper.SetParticleLifetime(const Value: Single);
begin
  FdParticleLifetime.Send(Value);
end;

function TParticleSystemNodeHelper.GetParticleSize: TVector2Single;
begin
  Result := FdParticleSize.Value;
end;

procedure TParticleSystemNodeHelper.SetParticleSize(const Value: TVector2Single);
begin
  FdParticleSize.Send(Value);
end;

{ TPointEmitterNode ----------------------------------------------- }

function TPointEmitterNodeHelper.GetDirection: TVector3Single;
begin
  Result := FdDirection.Value;
end;

procedure TPointEmitterNodeHelper.SetDirection(const Value: TVector3Single);
begin
  FdDirection.Send(Value);
end;

function TPointEmitterNodeHelper.GetPosition: TVector3Single;
begin
  Result := FdPosition.Value;
end;

procedure TPointEmitterNodeHelper.SetPosition(const Value: TVector3Single);
begin
  FdPosition.Send(Value);
end;

{ TPolylineEmitterNode ----------------------------------------------- }

function TPolylineEmitterNodeHelper.GetDirection: TVector3Single;
begin
  Result := FdDirection.Value;
end;

procedure TPolylineEmitterNodeHelper.SetDirection(const Value: TVector3Single);
begin
  FdDirection.Send(Value);
end;

{ TVolumeEmitterNode ----------------------------------------------- }

function TVolumeEmitterNodeHelper.GetDirection: TVector3Single;
begin
  Result := FdDirection.Value;
end;

procedure TVolumeEmitterNodeHelper.SetDirection(const Value: TVector3Single);
begin
  FdDirection.Send(Value);
end;

function TVolumeEmitterNodeHelper.GetInternal: boolean;
begin
  Result := FdInternal.Value;
end;

procedure TVolumeEmitterNodeHelper.SetInternal(const Value: boolean);
begin
  FdInternal.Send(Value);
end;

{ TWindPhysicsModelNode ----------------------------------------------- }

function TWindPhysicsModelNodeHelper.GetDirection: TVector3Single;
begin
  Result := FdDirection.Value;
end;

procedure TWindPhysicsModelNodeHelper.SetDirection(const Value: TVector3Single);
begin
  FdDirection.Send(Value);
end;

function TWindPhysicsModelNodeHelper.GetGustiness: Single;
begin
  Result := FdGustiness.Value;
end;

procedure TWindPhysicsModelNodeHelper.SetGustiness(const Value: Single);
begin
  FdGustiness.Send(Value);
end;

function TWindPhysicsModelNodeHelper.GetSpeed: Single;
begin
  Result := FdSpeed.Value;
end;

procedure TWindPhysicsModelNodeHelper.SetSpeed(const Value: Single);
begin
  FdSpeed.Send(Value);
end;

function TWindPhysicsModelNodeHelper.GetTurbulence: Single;
begin
  Result := FdTurbulence.Value;
end;

procedure TWindPhysicsModelNodeHelper.SetTurbulence(const Value: Single);
begin
  FdTurbulence.Send(Value);
end;

{ TPickableGroupNode ----------------------------------------------- }

function TPickableGroupNodeHelper.GetPickable: boolean;
begin
  Result := FdPickable.Value;
end;

procedure TPickableGroupNodeHelper.SetPickable(const Value: boolean);
begin
  FdPickable.Send(Value);
end;

{ TAbstractDragSensorNode ----------------------------------------------- }

function TAbstractDragSensorNodeHelper.GetAutoOffset: boolean;
begin
  Result := FdAutoOffset.Value;
end;

procedure TAbstractDragSensorNodeHelper.SetAutoOffset(const Value: boolean);
begin
  FdAutoOffset.Send(Value);
end;

{ TAbstractPointingDeviceSensorNode ----------------------------------------------- }

function TAbstractPointingDeviceSensorNodeHelper.GetDescription: string;
begin
  Result := FdDescription.Value;
end;

procedure TAbstractPointingDeviceSensorNodeHelper.SetDescription(const Value: string);
begin
  FdDescription.Send(Value);
end;

{ TCylinderSensorNode ----------------------------------------------- }

function TCylinderSensorNodeHelper.GetAxisRotation: TVector4Single;
begin
  Result := FdAxisRotation.Value;
end;

procedure TCylinderSensorNodeHelper.SetAxisRotation(const Value: TVector4Single);
begin
  FdAxisRotation.Send(Value);
end;

function TCylinderSensorNodeHelper.GetDiskAngle: Single;
begin
  Result := FdDiskAngle.Value;
end;

procedure TCylinderSensorNodeHelper.SetDiskAngle(const Value: Single);
begin
  FdDiskAngle.Send(Value);
end;

function TCylinderSensorNodeHelper.GetMaxAngle: Single;
begin
  Result := FdMaxAngle.Value;
end;

procedure TCylinderSensorNodeHelper.SetMaxAngle(const Value: Single);
begin
  FdMaxAngle.Send(Value);
end;

function TCylinderSensorNodeHelper.GetMinAngle: Single;
begin
  Result := FdMinAngle.Value;
end;

procedure TCylinderSensorNodeHelper.SetMinAngle(const Value: Single);
begin
  FdMinAngle.Send(Value);
end;

function TCylinderSensorNodeHelper.GetOffset: Single;
begin
  Result := FdOffset.Value;
end;

procedure TCylinderSensorNodeHelper.SetOffset(const Value: Single);
begin
  FdOffset.Send(Value);
end;

{ TPlaneSensorNode ----------------------------------------------- }

function TPlaneSensorNodeHelper.GetAxisRotation: TVector4Single;
begin
  Result := FdAxisRotation.Value;
end;

procedure TPlaneSensorNodeHelper.SetAxisRotation(const Value: TVector4Single);
begin
  FdAxisRotation.Send(Value);
end;

function TPlaneSensorNodeHelper.GetMaxPosition: TVector2Single;
begin
  Result := FdMaxPosition.Value;
end;

procedure TPlaneSensorNodeHelper.SetMaxPosition(const Value: TVector2Single);
begin
  FdMaxPosition.Send(Value);
end;

function TPlaneSensorNodeHelper.GetMinPosition: TVector2Single;
begin
  Result := FdMinPosition.Value;
end;

procedure TPlaneSensorNodeHelper.SetMinPosition(const Value: TVector2Single);
begin
  FdMinPosition.Send(Value);
end;

function TPlaneSensorNodeHelper.GetOffset: TVector3Single;
begin
  Result := FdOffset.Value;
end;

procedure TPlaneSensorNodeHelper.SetOffset(const Value: TVector3Single);
begin
  FdOffset.Send(Value);
end;

{ TSphereSensorNode ----------------------------------------------- }

function TSphereSensorNodeHelper.GetOffset: TVector4Single;
begin
  Result := FdOffset.Value;
end;

procedure TSphereSensorNodeHelper.SetOffset(const Value: TVector4Single);
begin
  FdOffset.Send(Value);
end;

{ TAbstractComposedGeometryNode ----------------------------------------------- }

function TAbstractComposedGeometryNodeHelper.GetCcw: boolean;
begin
  Result := FdCcw.Value;
end;

procedure TAbstractComposedGeometryNodeHelper.SetCcw(const Value: boolean);
begin
  FdCcw.Send(Value);
end;

function TAbstractComposedGeometryNodeHelper.GetColorPerVertex: boolean;
begin
  Result := FdColorPerVertex.Value;
end;

procedure TAbstractComposedGeometryNodeHelper.SetColorPerVertex(const Value: boolean);
begin
  FdColorPerVertex.Send(Value);
end;

function TAbstractComposedGeometryNodeHelper.GetNormalPerVertex: boolean;
begin
  Result := FdNormalPerVertex.Value;
end;

procedure TAbstractComposedGeometryNodeHelper.SetNormalPerVertex(const Value: boolean);
begin
  FdNormalPerVertex.Send(Value);
end;

{ TClipPlaneNode ----------------------------------------------- }

function TClipPlaneNodeHelper.GetEnabled: boolean;
begin
  Result := FdEnabled.Value;
end;

procedure TClipPlaneNodeHelper.SetEnabled(const Value: boolean);
begin
  FdEnabled.Send(Value);
end;

function TClipPlaneNodeHelper.GetPlane: TVector4Single;
begin
  Result := FdPlane.Value;
end;

procedure TClipPlaneNodeHelper.SetPlane(const Value: TVector4Single);
begin
  FdPlane.Send(Value);
end;

{ TIndexedLineSetNode ----------------------------------------------- }

function TIndexedLineSetNodeHelper.GetColorPerVertex: boolean;
begin
  Result := FdColorPerVertex.Value;
end;

procedure TIndexedLineSetNodeHelper.SetColorPerVertex(const Value: boolean);
begin
  FdColorPerVertex.Send(Value);
end;

{ TAbstractNBodyCollidableNode ----------------------------------------------- }

function TAbstractNBodyCollidableNodeHelper.GetEnabled: boolean;
begin
  Result := FdEnabled.Value;
end;

procedure TAbstractNBodyCollidableNodeHelper.SetEnabled(const Value: boolean);
begin
  FdEnabled.Send(Value);
end;

function TAbstractNBodyCollidableNodeHelper.GetRotation: TVector4Single;
begin
  Result := FdRotation.Value;
end;

procedure TAbstractNBodyCollidableNodeHelper.SetRotation(const Value: TVector4Single);
begin
  FdRotation.Send(Value);
end;

function TAbstractNBodyCollidableNodeHelper.GetTranslation: TVector3Single;
begin
  Result := FdTranslation.Value;
end;

procedure TAbstractNBodyCollidableNodeHelper.SetTranslation(const Value: TVector3Single);
begin
  FdTranslation.Send(Value);
end;

function TAbstractNBodyCollidableNodeHelper.GetBboxCenter: TVector3Single;
begin
  Result := FdBboxCenter.Value;
end;

procedure TAbstractNBodyCollidableNodeHelper.SetBboxCenter(const Value: TVector3Single);
begin
  FdBboxCenter.Send(Value);
end;

function TAbstractNBodyCollidableNodeHelper.GetBboxSize: TVector3Single;
begin
  Result := FdBboxSize.Value;
end;

procedure TAbstractNBodyCollidableNodeHelper.SetBboxSize(const Value: TVector3Single);
begin
  FdBboxSize.Send(Value);
end;

{ TAbstractNBodyCollisionSpaceNode ----------------------------------------------- }

function TAbstractNBodyCollisionSpaceNodeHelper.GetEnabled: boolean;
begin
  Result := FdEnabled.Value;
end;

procedure TAbstractNBodyCollisionSpaceNodeHelper.SetEnabled(const Value: boolean);
begin
  FdEnabled.Send(Value);
end;

function TAbstractNBodyCollisionSpaceNodeHelper.GetBboxCenter: TVector3Single;
begin
  Result := FdBboxCenter.Value;
end;

procedure TAbstractNBodyCollisionSpaceNodeHelper.SetBboxCenter(const Value: TVector3Single);
begin
  FdBboxCenter.Send(Value);
end;

function TAbstractNBodyCollisionSpaceNodeHelper.GetBboxSize: TVector3Single;
begin
  Result := FdBboxSize.Value;
end;

procedure TAbstractNBodyCollisionSpaceNodeHelper.SetBboxSize(const Value: TVector3Single);
begin
  FdBboxSize.Send(Value);
end;

{ TBallJointNode ----------------------------------------------- }

function TBallJointNodeHelper.GetAnchorPoint: TVector3Single;
begin
  Result := FdAnchorPoint.Value;
end;

procedure TBallJointNodeHelper.SetAnchorPoint(const Value: TVector3Single);
begin
  FdAnchorPoint.Send(Value);
end;

{ TCollisionCollectionNode ----------------------------------------------- }

function TCollisionCollectionNodeHelper.GetBounce: Single;
begin
  Result := FdBounce.Value;
end;

procedure TCollisionCollectionNodeHelper.SetBounce(const Value: Single);
begin
  FdBounce.Send(Value);
end;

function TCollisionCollectionNodeHelper.GetEnabled: boolean;
begin
  Result := FdEnabled.Value;
end;

procedure TCollisionCollectionNodeHelper.SetEnabled(const Value: boolean);
begin
  FdEnabled.Send(Value);
end;

function TCollisionCollectionNodeHelper.GetFrictionCoefficients: TVector2Single;
begin
  Result := FdFrictionCoefficients.Value;
end;

procedure TCollisionCollectionNodeHelper.SetFrictionCoefficients(const Value: TVector2Single);
begin
  FdFrictionCoefficients.Send(Value);
end;

function TCollisionCollectionNodeHelper.GetMinBounceSpeed: Single;
begin
  Result := FdMinBounceSpeed.Value;
end;

procedure TCollisionCollectionNodeHelper.SetMinBounceSpeed(const Value: Single);
begin
  FdMinBounceSpeed.Send(Value);
end;

function TCollisionCollectionNodeHelper.GetSlipFactors: TVector2Single;
begin
  Result := FdSlipFactors.Value;
end;

procedure TCollisionCollectionNodeHelper.SetSlipFactors(const Value: TVector2Single);
begin
  FdSlipFactors.Send(Value);
end;

function TCollisionCollectionNodeHelper.GetSoftnessConstantForceMix: Single;
begin
  Result := FdSoftnessConstantForceMix.Value;
end;

procedure TCollisionCollectionNodeHelper.SetSoftnessConstantForceMix(const Value: Single);
begin
  FdSoftnessConstantForceMix.Send(Value);
end;

function TCollisionCollectionNodeHelper.GetSoftnessErrorCorrection: Single;
begin
  Result := FdSoftnessErrorCorrection.Value;
end;

procedure TCollisionCollectionNodeHelper.SetSoftnessErrorCorrection(const Value: Single);
begin
  FdSoftnessErrorCorrection.Send(Value);
end;

function TCollisionCollectionNodeHelper.GetSurfaceSpeed: TVector2Single;
begin
  Result := FdSurfaceSpeed.Value;
end;

procedure TCollisionCollectionNodeHelper.SetSurfaceSpeed(const Value: TVector2Single);
begin
  FdSurfaceSpeed.Send(Value);
end;

{ TCollisionSpaceNode ----------------------------------------------- }

function TCollisionSpaceNodeHelper.GetUseGeometry: boolean;
begin
  Result := FdUseGeometry.Value;
end;

procedure TCollisionSpaceNodeHelper.SetUseGeometry(const Value: boolean);
begin
  FdUseGeometry.Send(Value);
end;

{ TContactNode ----------------------------------------------- }

function TContactNodeHelper.GetBounce: Single;
begin
  Result := FdBounce.Value;
end;

procedure TContactNodeHelper.SetBounce(const Value: Single);
begin
  FdBounce.Send(Value);
end;

function TContactNodeHelper.GetContactNormal: TVector3Single;
begin
  Result := FdContactNormal.Value;
end;

procedure TContactNodeHelper.SetContactNormal(const Value: TVector3Single);
begin
  FdContactNormal.Send(Value);
end;

function TContactNodeHelper.GetDepth: Single;
begin
  Result := FdDepth.Value;
end;

procedure TContactNodeHelper.SetDepth(const Value: Single);
begin
  FdDepth.Send(Value);
end;

function TContactNodeHelper.GetFrictionCoefficients: TVector2Single;
begin
  Result := FdFrictionCoefficients.Value;
end;

procedure TContactNodeHelper.SetFrictionCoefficients(const Value: TVector2Single);
begin
  FdFrictionCoefficients.Send(Value);
end;

function TContactNodeHelper.GetFrictionDirection: TVector3Single;
begin
  Result := FdFrictionDirection.Value;
end;

procedure TContactNodeHelper.SetFrictionDirection(const Value: TVector3Single);
begin
  FdFrictionDirection.Send(Value);
end;

function TContactNodeHelper.GetMinbounceSpeed: Single;
begin
  Result := FdMinbounceSpeed.Value;
end;

procedure TContactNodeHelper.SetMinbounceSpeed(const Value: Single);
begin
  FdMinbounceSpeed.Send(Value);
end;

function TContactNodeHelper.GetPosition: TVector3Single;
begin
  Result := FdPosition.Value;
end;

procedure TContactNodeHelper.SetPosition(const Value: TVector3Single);
begin
  FdPosition.Send(Value);
end;

function TContactNodeHelper.GetSlipCoefficients: TVector2Single;
begin
  Result := FdSlipCoefficients.Value;
end;

procedure TContactNodeHelper.SetSlipCoefficients(const Value: TVector2Single);
begin
  FdSlipCoefficients.Send(Value);
end;

function TContactNodeHelper.GetSoftnessConstantForceMix: Single;
begin
  Result := FdSoftnessConstantForceMix.Value;
end;

procedure TContactNodeHelper.SetSoftnessConstantForceMix(const Value: Single);
begin
  FdSoftnessConstantForceMix.Send(Value);
end;

function TContactNodeHelper.GetSoftnessErrorCorrection: Single;
begin
  Result := FdSoftnessErrorCorrection.Value;
end;

procedure TContactNodeHelper.SetSoftnessErrorCorrection(const Value: Single);
begin
  FdSoftnessErrorCorrection.Send(Value);
end;

function TContactNodeHelper.GetSurfaceSpeed: TVector2Single;
begin
  Result := FdSurfaceSpeed.Value;
end;

procedure TContactNodeHelper.SetSurfaceSpeed(const Value: TVector2Single);
begin
  FdSurfaceSpeed.Send(Value);
end;

{ TDoubleAxisHingeJointNode ----------------------------------------------- }

function TDoubleAxisHingeJointNodeHelper.GetAnchorPoint: TVector3Single;
begin
  Result := FdAnchorPoint.Value;
end;

procedure TDoubleAxisHingeJointNodeHelper.SetAnchorPoint(const Value: TVector3Single);
begin
  FdAnchorPoint.Send(Value);
end;

function TDoubleAxisHingeJointNodeHelper.GetAxis1: TVector3Single;
begin
  Result := FdAxis1.Value;
end;

procedure TDoubleAxisHingeJointNodeHelper.SetAxis1(const Value: TVector3Single);
begin
  FdAxis1.Send(Value);
end;

function TDoubleAxisHingeJointNodeHelper.GetAxis2: TVector3Single;
begin
  Result := FdAxis2.Value;
end;

procedure TDoubleAxisHingeJointNodeHelper.SetAxis2(const Value: TVector3Single);
begin
  FdAxis2.Send(Value);
end;

function TDoubleAxisHingeJointNodeHelper.GetDesiredAngularVelocity1: Single;
begin
  Result := FdDesiredAngularVelocity1.Value;
end;

procedure TDoubleAxisHingeJointNodeHelper.SetDesiredAngularVelocity1(const Value: Single);
begin
  FdDesiredAngularVelocity1.Send(Value);
end;

function TDoubleAxisHingeJointNodeHelper.GetDesiredAngularVelocity2: Single;
begin
  Result := FdDesiredAngularVelocity2.Value;
end;

procedure TDoubleAxisHingeJointNodeHelper.SetDesiredAngularVelocity2(const Value: Single);
begin
  FdDesiredAngularVelocity2.Send(Value);
end;

function TDoubleAxisHingeJointNodeHelper.GetMaxAngle1: Single;
begin
  Result := FdMaxAngle1.Value;
end;

procedure TDoubleAxisHingeJointNodeHelper.SetMaxAngle1(const Value: Single);
begin
  FdMaxAngle1.Send(Value);
end;

function TDoubleAxisHingeJointNodeHelper.GetMaxTorque1: Single;
begin
  Result := FdMaxTorque1.Value;
end;

procedure TDoubleAxisHingeJointNodeHelper.SetMaxTorque1(const Value: Single);
begin
  FdMaxTorque1.Send(Value);
end;

function TDoubleAxisHingeJointNodeHelper.GetMaxTorque2: Single;
begin
  Result := FdMaxTorque2.Value;
end;

procedure TDoubleAxisHingeJointNodeHelper.SetMaxTorque2(const Value: Single);
begin
  FdMaxTorque2.Send(Value);
end;

function TDoubleAxisHingeJointNodeHelper.GetMinAngle1: Single;
begin
  Result := FdMinAngle1.Value;
end;

procedure TDoubleAxisHingeJointNodeHelper.SetMinAngle1(const Value: Single);
begin
  FdMinAngle1.Send(Value);
end;

function TDoubleAxisHingeJointNodeHelper.GetStopBounce1: Single;
begin
  Result := FdStopBounce1.Value;
end;

procedure TDoubleAxisHingeJointNodeHelper.SetStopBounce1(const Value: Single);
begin
  FdStopBounce1.Send(Value);
end;

function TDoubleAxisHingeJointNodeHelper.GetStopConstantForceMix1: Single;
begin
  Result := FdStopConstantForceMix1.Value;
end;

procedure TDoubleAxisHingeJointNodeHelper.SetStopConstantForceMix1(const Value: Single);
begin
  FdStopConstantForceMix1.Send(Value);
end;

function TDoubleAxisHingeJointNodeHelper.GetStopErrorCorrection1: Single;
begin
  Result := FdStopErrorCorrection1.Value;
end;

procedure TDoubleAxisHingeJointNodeHelper.SetStopErrorCorrection1(const Value: Single);
begin
  FdStopErrorCorrection1.Send(Value);
end;

function TDoubleAxisHingeJointNodeHelper.GetSuspensionErrorCorrection: Single;
begin
  Result := FdSuspensionErrorCorrection.Value;
end;

procedure TDoubleAxisHingeJointNodeHelper.SetSuspensionErrorCorrection(const Value: Single);
begin
  FdSuspensionErrorCorrection.Send(Value);
end;

function TDoubleAxisHingeJointNodeHelper.GetSuspensionForce: Single;
begin
  Result := FdSuspensionForce.Value;
end;

procedure TDoubleAxisHingeJointNodeHelper.SetSuspensionForce(const Value: Single);
begin
  FdSuspensionForce.Send(Value);
end;

{ TMotorJointNode ----------------------------------------------- }

function TMotorJointNodeHelper.GetAxis1Angle: Single;
begin
  Result := FdAxis1Angle.Value;
end;

procedure TMotorJointNodeHelper.SetAxis1Angle(const Value: Single);
begin
  FdAxis1Angle.Send(Value);
end;

function TMotorJointNodeHelper.GetAxis1Torque: Single;
begin
  Result := FdAxis1Torque.Value;
end;

procedure TMotorJointNodeHelper.SetAxis1Torque(const Value: Single);
begin
  FdAxis1Torque.Send(Value);
end;

function TMotorJointNodeHelper.GetAxis2Angle: Single;
begin
  Result := FdAxis2Angle.Value;
end;

procedure TMotorJointNodeHelper.SetAxis2Angle(const Value: Single);
begin
  FdAxis2Angle.Send(Value);
end;

function TMotorJointNodeHelper.GetAxis2Torque: Single;
begin
  Result := FdAxis2Torque.Value;
end;

procedure TMotorJointNodeHelper.SetAxis2Torque(const Value: Single);
begin
  FdAxis2Torque.Send(Value);
end;

function TMotorJointNodeHelper.GetAxis3Angle: Single;
begin
  Result := FdAxis3Angle.Value;
end;

procedure TMotorJointNodeHelper.SetAxis3Angle(const Value: Single);
begin
  FdAxis3Angle.Send(Value);
end;

function TMotorJointNodeHelper.GetAxis3Torque: Single;
begin
  Result := FdAxis3Torque.Value;
end;

procedure TMotorJointNodeHelper.SetAxis3Torque(const Value: Single);
begin
  FdAxis3Torque.Send(Value);
end;

function TMotorJointNodeHelper.GetEnabledAxes: Integer;
begin
  Result := FdEnabledAxes.Value;
end;

procedure TMotorJointNodeHelper.SetEnabledAxes(const Value: Integer);
begin
  FdEnabledAxes.Send(Value);
end;

function TMotorJointNodeHelper.GetMotor1Axis: TVector3Single;
begin
  Result := FdMotor1Axis.Value;
end;

procedure TMotorJointNodeHelper.SetMotor1Axis(const Value: TVector3Single);
begin
  FdMotor1Axis.Send(Value);
end;

function TMotorJointNodeHelper.GetMotor2Axis: TVector3Single;
begin
  Result := FdMotor2Axis.Value;
end;

procedure TMotorJointNodeHelper.SetMotor2Axis(const Value: TVector3Single);
begin
  FdMotor2Axis.Send(Value);
end;

function TMotorJointNodeHelper.GetMotor3Axis: TVector3Single;
begin
  Result := FdMotor3Axis.Value;
end;

procedure TMotorJointNodeHelper.SetMotor3Axis(const Value: TVector3Single);
begin
  FdMotor3Axis.Send(Value);
end;

function TMotorJointNodeHelper.GetStop1Bounce: Single;
begin
  Result := FdStop1Bounce.Value;
end;

procedure TMotorJointNodeHelper.SetStop1Bounce(const Value: Single);
begin
  FdStop1Bounce.Send(Value);
end;

function TMotorJointNodeHelper.GetStop1ErrorCorrection: Single;
begin
  Result := FdStop1ErrorCorrection.Value;
end;

procedure TMotorJointNodeHelper.SetStop1ErrorCorrection(const Value: Single);
begin
  FdStop1ErrorCorrection.Send(Value);
end;

function TMotorJointNodeHelper.GetStop2Bounce: Single;
begin
  Result := FdStop2Bounce.Value;
end;

procedure TMotorJointNodeHelper.SetStop2Bounce(const Value: Single);
begin
  FdStop2Bounce.Send(Value);
end;

function TMotorJointNodeHelper.GetStop2ErrorCorrection: Single;
begin
  Result := FdStop2ErrorCorrection.Value;
end;

procedure TMotorJointNodeHelper.SetStop2ErrorCorrection(const Value: Single);
begin
  FdStop2ErrorCorrection.Send(Value);
end;

function TMotorJointNodeHelper.GetStop3Bounce: Single;
begin
  Result := FdStop3Bounce.Value;
end;

procedure TMotorJointNodeHelper.SetStop3Bounce(const Value: Single);
begin
  FdStop3Bounce.Send(Value);
end;

function TMotorJointNodeHelper.GetStop3ErrorCorrection: Single;
begin
  Result := FdStop3ErrorCorrection.Value;
end;

procedure TMotorJointNodeHelper.SetStop3ErrorCorrection(const Value: Single);
begin
  FdStop3ErrorCorrection.Send(Value);
end;

function TMotorJointNodeHelper.GetAutoCalc: boolean;
begin
  Result := FdAutoCalc.Value;
end;

procedure TMotorJointNodeHelper.SetAutoCalc(const Value: boolean);
begin
  FdAutoCalc.Send(Value);
end;

{ TRigidBodyNode ----------------------------------------------- }

function TRigidBodyNodeHelper.GetAngularDampingFactor: Single;
begin
  Result := FdAngularDampingFactor.Value;
end;

procedure TRigidBodyNodeHelper.SetAngularDampingFactor(const Value: Single);
begin
  FdAngularDampingFactor.Send(Value);
end;

function TRigidBodyNodeHelper.GetAngularVelocity: TVector3Single;
begin
  Result := FdAngularVelocity.Value;
end;

procedure TRigidBodyNodeHelper.SetAngularVelocity(const Value: TVector3Single);
begin
  FdAngularVelocity.Send(Value);
end;

function TRigidBodyNodeHelper.GetAutoDamp: boolean;
begin
  Result := FdAutoDamp.Value;
end;

procedure TRigidBodyNodeHelper.SetAutoDamp(const Value: boolean);
begin
  FdAutoDamp.Send(Value);
end;

function TRigidBodyNodeHelper.GetAutoDisable: boolean;
begin
  Result := FdAutoDisable.Value;
end;

procedure TRigidBodyNodeHelper.SetAutoDisable(const Value: boolean);
begin
  FdAutoDisable.Send(Value);
end;

function TRigidBodyNodeHelper.GetCenterOfMass: TVector3Single;
begin
  Result := FdCenterOfMass.Value;
end;

procedure TRigidBodyNodeHelper.SetCenterOfMass(const Value: TVector3Single);
begin
  FdCenterOfMass.Send(Value);
end;

function TRigidBodyNodeHelper.GetDisableAngularSpeed: Single;
begin
  Result := FdDisableAngularSpeed.Value;
end;

procedure TRigidBodyNodeHelper.SetDisableAngularSpeed(const Value: Single);
begin
  FdDisableAngularSpeed.Send(Value);
end;

function TRigidBodyNodeHelper.GetDisableLinearSpeed: Single;
begin
  Result := FdDisableLinearSpeed.Value;
end;

procedure TRigidBodyNodeHelper.SetDisableLinearSpeed(const Value: Single);
begin
  FdDisableLinearSpeed.Send(Value);
end;

function TRigidBodyNodeHelper.GetDisableTime: Single;
begin
  Result := FdDisableTime.Value;
end;

procedure TRigidBodyNodeHelper.SetDisableTime(const Value: Single);
begin
  FdDisableTime.Send(Value);
end;

function TRigidBodyNodeHelper.GetEnabled: boolean;
begin
  Result := FdEnabled.Value;
end;

procedure TRigidBodyNodeHelper.SetEnabled(const Value: boolean);
begin
  FdEnabled.Send(Value);
end;

function TRigidBodyNodeHelper.GetFiniteRotationAxis: TVector3Single;
begin
  Result := FdFiniteRotationAxis.Value;
end;

procedure TRigidBodyNodeHelper.SetFiniteRotationAxis(const Value: TVector3Single);
begin
  FdFiniteRotationAxis.Send(Value);
end;

function TRigidBodyNodeHelper.GetFixed: boolean;
begin
  Result := FdFixed.Value;
end;

procedure TRigidBodyNodeHelper.SetFixed(const Value: boolean);
begin
  FdFixed.Send(Value);
end;

function TRigidBodyNodeHelper.GetInertia: TMatrix3Single;
begin
  Result := FdInertia.Value;
end;

procedure TRigidBodyNodeHelper.SetInertia(const Value: TMatrix3Single);
begin
  FdInertia.Send(Value);
end;

function TRigidBodyNodeHelper.GetLinearDampingFactor: Single;
begin
  Result := FdLinearDampingFactor.Value;
end;

procedure TRigidBodyNodeHelper.SetLinearDampingFactor(const Value: Single);
begin
  FdLinearDampingFactor.Send(Value);
end;

function TRigidBodyNodeHelper.GetLinearVelocity: TVector3Single;
begin
  Result := FdLinearVelocity.Value;
end;

procedure TRigidBodyNodeHelper.SetLinearVelocity(const Value: TVector3Single);
begin
  FdLinearVelocity.Send(Value);
end;

function TRigidBodyNodeHelper.GetMass: Single;
begin
  Result := FdMass.Value;
end;

procedure TRigidBodyNodeHelper.SetMass(const Value: Single);
begin
  FdMass.Send(Value);
end;

function TRigidBodyNodeHelper.GetOrientation: TVector4Single;
begin
  Result := FdOrientation.Value;
end;

procedure TRigidBodyNodeHelper.SetOrientation(const Value: TVector4Single);
begin
  FdOrientation.Send(Value);
end;

function TRigidBodyNodeHelper.GetPosition: TVector3Single;
begin
  Result := FdPosition.Value;
end;

procedure TRigidBodyNodeHelper.SetPosition(const Value: TVector3Single);
begin
  FdPosition.Send(Value);
end;

function TRigidBodyNodeHelper.GetUseFiniteRotation: boolean;
begin
  Result := FdUseFiniteRotation.Value;
end;

procedure TRigidBodyNodeHelper.SetUseFiniteRotation(const Value: boolean);
begin
  FdUseFiniteRotation.Send(Value);
end;

function TRigidBodyNodeHelper.GetUseGlobalGravity: boolean;
begin
  Result := FdUseGlobalGravity.Value;
end;

procedure TRigidBodyNodeHelper.SetUseGlobalGravity(const Value: boolean);
begin
  FdUseGlobalGravity.Send(Value);
end;

{ TRigidBodyCollectionNode ----------------------------------------------- }

function TRigidBodyCollectionNodeHelper.GetAutoDisable: boolean;
begin
  Result := FdAutoDisable.Value;
end;

procedure TRigidBodyCollectionNodeHelper.SetAutoDisable(const Value: boolean);
begin
  FdAutoDisable.Send(Value);
end;

function TRigidBodyCollectionNodeHelper.GetConstantForceMix: Single;
begin
  Result := FdConstantForceMix.Value;
end;

procedure TRigidBodyCollectionNodeHelper.SetConstantForceMix(const Value: Single);
begin
  FdConstantForceMix.Send(Value);
end;

function TRigidBodyCollectionNodeHelper.GetContactSurfaceThickness: Single;
begin
  Result := FdContactSurfaceThickness.Value;
end;

procedure TRigidBodyCollectionNodeHelper.SetContactSurfaceThickness(const Value: Single);
begin
  FdContactSurfaceThickness.Send(Value);
end;

function TRigidBodyCollectionNodeHelper.GetDisableAngularSpeed: Single;
begin
  Result := FdDisableAngularSpeed.Value;
end;

procedure TRigidBodyCollectionNodeHelper.SetDisableAngularSpeed(const Value: Single);
begin
  FdDisableAngularSpeed.Send(Value);
end;

function TRigidBodyCollectionNodeHelper.GetDisableLinearSpeed: Single;
begin
  Result := FdDisableLinearSpeed.Value;
end;

procedure TRigidBodyCollectionNodeHelper.SetDisableLinearSpeed(const Value: Single);
begin
  FdDisableLinearSpeed.Send(Value);
end;

function TRigidBodyCollectionNodeHelper.GetDisableTime: Single;
begin
  Result := FdDisableTime.Value;
end;

procedure TRigidBodyCollectionNodeHelper.SetDisableTime(const Value: Single);
begin
  FdDisableTime.Send(Value);
end;

function TRigidBodyCollectionNodeHelper.GetEnabled: boolean;
begin
  Result := FdEnabled.Value;
end;

procedure TRigidBodyCollectionNodeHelper.SetEnabled(const Value: boolean);
begin
  FdEnabled.Send(Value);
end;

function TRigidBodyCollectionNodeHelper.GetErrorCorrection: Single;
begin
  Result := FdErrorCorrection.Value;
end;

procedure TRigidBodyCollectionNodeHelper.SetErrorCorrection(const Value: Single);
begin
  FdErrorCorrection.Send(Value);
end;

function TRigidBodyCollectionNodeHelper.GetGravity: TVector3Single;
begin
  Result := FdGravity.Value;
end;

procedure TRigidBodyCollectionNodeHelper.SetGravity(const Value: TVector3Single);
begin
  FdGravity.Send(Value);
end;

function TRigidBodyCollectionNodeHelper.GetIterations: Integer;
begin
  Result := FdIterations.Value;
end;

procedure TRigidBodyCollectionNodeHelper.SetIterations(const Value: Integer);
begin
  FdIterations.Send(Value);
end;

function TRigidBodyCollectionNodeHelper.GetMaxCorrectionSpeed: Single;
begin
  Result := FdMaxCorrectionSpeed.Value;
end;

procedure TRigidBodyCollectionNodeHelper.SetMaxCorrectionSpeed(const Value: Single);
begin
  FdMaxCorrectionSpeed.Send(Value);
end;

function TRigidBodyCollectionNodeHelper.GetPreferAccuracy: boolean;
begin
  Result := FdPreferAccuracy.Value;
end;

procedure TRigidBodyCollectionNodeHelper.SetPreferAccuracy(const Value: boolean);
begin
  FdPreferAccuracy.Send(Value);
end;

{ TSingleAxisHingeJointNode ----------------------------------------------- }

function TSingleAxisHingeJointNodeHelper.GetAnchorPoint: TVector3Single;
begin
  Result := FdAnchorPoint.Value;
end;

procedure TSingleAxisHingeJointNodeHelper.SetAnchorPoint(const Value: TVector3Single);
begin
  FdAnchorPoint.Send(Value);
end;

function TSingleAxisHingeJointNodeHelper.GetAxis: TVector3Single;
begin
  Result := FdAxis.Value;
end;

procedure TSingleAxisHingeJointNodeHelper.SetAxis(const Value: TVector3Single);
begin
  FdAxis.Send(Value);
end;

function TSingleAxisHingeJointNodeHelper.GetMaxAngle: Single;
begin
  Result := FdMaxAngle.Value;
end;

procedure TSingleAxisHingeJointNodeHelper.SetMaxAngle(const Value: Single);
begin
  FdMaxAngle.Send(Value);
end;

function TSingleAxisHingeJointNodeHelper.GetMinAngle: Single;
begin
  Result := FdMinAngle.Value;
end;

procedure TSingleAxisHingeJointNodeHelper.SetMinAngle(const Value: Single);
begin
  FdMinAngle.Send(Value);
end;

function TSingleAxisHingeJointNodeHelper.GetStopBounce: Single;
begin
  Result := FdStopBounce.Value;
end;

procedure TSingleAxisHingeJointNodeHelper.SetStopBounce(const Value: Single);
begin
  FdStopBounce.Send(Value);
end;

function TSingleAxisHingeJointNodeHelper.GetStopErrorCorrection: Single;
begin
  Result := FdStopErrorCorrection.Value;
end;

procedure TSingleAxisHingeJointNodeHelper.SetStopErrorCorrection(const Value: Single);
begin
  FdStopErrorCorrection.Send(Value);
end;

{ TSliderJointNode ----------------------------------------------- }

function TSliderJointNodeHelper.GetAxis: TVector3Single;
begin
  Result := FdAxis.Value;
end;

procedure TSliderJointNodeHelper.SetAxis(const Value: TVector3Single);
begin
  FdAxis.Send(Value);
end;

function TSliderJointNodeHelper.GetMaxSeparation: Single;
begin
  Result := FdMaxSeparation.Value;
end;

procedure TSliderJointNodeHelper.SetMaxSeparation(const Value: Single);
begin
  FdMaxSeparation.Send(Value);
end;

function TSliderJointNodeHelper.GetMinSeparation: Single;
begin
  Result := FdMinSeparation.Value;
end;

procedure TSliderJointNodeHelper.SetMinSeparation(const Value: Single);
begin
  FdMinSeparation.Send(Value);
end;

function TSliderJointNodeHelper.GetStopBounce: Single;
begin
  Result := FdStopBounce.Value;
end;

procedure TSliderJointNodeHelper.SetStopBounce(const Value: Single);
begin
  FdStopBounce.Send(Value);
end;

function TSliderJointNodeHelper.GetStopErrorCorrection: Single;
begin
  Result := FdStopErrorCorrection.Value;
end;

procedure TSliderJointNodeHelper.SetStopErrorCorrection(const Value: Single);
begin
  FdStopErrorCorrection.Send(Value);
end;

{ TUniversalJointNode ----------------------------------------------- }

function TUniversalJointNodeHelper.GetAnchorPoint: TVector3Single;
begin
  Result := FdAnchorPoint.Value;
end;

procedure TUniversalJointNodeHelper.SetAnchorPoint(const Value: TVector3Single);
begin
  FdAnchorPoint.Send(Value);
end;

function TUniversalJointNodeHelper.GetAxis1: TVector3Single;
begin
  Result := FdAxis1.Value;
end;

procedure TUniversalJointNodeHelper.SetAxis1(const Value: TVector3Single);
begin
  FdAxis1.Send(Value);
end;

function TUniversalJointNodeHelper.GetAxis2: TVector3Single;
begin
  Result := FdAxis2.Value;
end;

procedure TUniversalJointNodeHelper.SetAxis2(const Value: TVector3Single);
begin
  FdAxis2.Send(Value);
end;

function TUniversalJointNodeHelper.GetStopBounce1: Single;
begin
  Result := FdStopBounce1.Value;
end;

procedure TUniversalJointNodeHelper.SetStopBounce1(const Value: Single);
begin
  FdStopBounce1.Send(Value);
end;

function TUniversalJointNodeHelper.GetStop1ErrorCorrection: Single;
begin
  Result := FdStop1ErrorCorrection.Value;
end;

procedure TUniversalJointNodeHelper.SetStop1ErrorCorrection(const Value: Single);
begin
  FdStop1ErrorCorrection.Send(Value);
end;

function TUniversalJointNodeHelper.GetStop2Bounce: Single;
begin
  Result := FdStop2Bounce.Value;
end;

procedure TUniversalJointNodeHelper.SetStop2Bounce(const Value: Single);
begin
  FdStop2Bounce.Send(Value);
end;

function TUniversalJointNodeHelper.GetStop2ErrorCorrection: Single;
begin
  Result := FdStop2ErrorCorrection.Value;
end;

procedure TUniversalJointNodeHelper.SetStop2ErrorCorrection(const Value: Single);
begin
  FdStop2ErrorCorrection.Send(Value);
end;

{ TScriptNode ----------------------------------------------- }

function TScriptNodeHelper.GetDirectOutput: boolean;
begin
  Result := FdDirectOutput.Value;
end;

procedure TScriptNodeHelper.SetDirectOutput(const Value: boolean);
begin
  FdDirectOutput.Send(Value);
end;

function TScriptNodeHelper.GetMustEvaluate: boolean;
begin
  Result := FdMustEvaluate.Value;
end;

procedure TScriptNodeHelper.SetMustEvaluate(const Value: boolean);
begin
  FdMustEvaluate.Send(Value);
end;

{ TAbstractVertexAttributeNode ----------------------------------------------- }

function TAbstractVertexAttributeNodeHelper.GetNameField: string;
begin
  Result := FdName.Value;
end;

procedure TAbstractVertexAttributeNodeHelper.SetNameField(const Value: string);
begin
  FdName.Send(Value);
end;

{ TFloatVertexAttributeNode ----------------------------------------------- }

function TFloatVertexAttributeNodeHelper.GetNumComponents: Integer;
begin
  Result := FdNumComponents.Value;
end;

procedure TFloatVertexAttributeNodeHelper.SetNumComponents(const Value: Integer);
begin
  FdNumComponents.Send(Value);
end;

{ TAbstractShapeNode ----------------------------------------------- }

function TAbstractShapeNodeHelper.GetBboxCenter: TVector3Single;
begin
  Result := FdBboxCenter.Value;
end;

procedure TAbstractShapeNodeHelper.SetBboxCenter(const Value: TVector3Single);
begin
  FdBboxCenter.Send(Value);
end;

function TAbstractShapeNodeHelper.GetBboxSize: TVector3Single;
begin
  Result := FdBboxSize.Value;
end;

procedure TAbstractShapeNodeHelper.SetBboxSize(const Value: TVector3Single);
begin
  FdBboxSize.Send(Value);
end;

function TAbstractShapeNodeHelper.GetRender: boolean;
begin
  Result := FdRender.Value;
end;

procedure TAbstractShapeNodeHelper.SetRender(const Value: boolean);
begin
  FdRender.Send(Value);
end;

{ TFillPropertiesNode ----------------------------------------------- }

function TFillPropertiesNodeHelper.GetFilled: boolean;
begin
  Result := FdFilled.Value;
end;

procedure TFillPropertiesNodeHelper.SetFilled(const Value: boolean);
begin
  FdFilled.Send(Value);
end;

function TFillPropertiesNodeHelper.GetHatchColor: TCastleColorRGB;
begin
  Result := FdHatchColor.Value;
end;

procedure TFillPropertiesNodeHelper.SetHatchColor(const Value: TCastleColorRGB);
begin
  FdHatchColor.Send(Value);
end;

function TFillPropertiesNodeHelper.GetHatched: boolean;
begin
  Result := FdHatched.Value;
end;

procedure TFillPropertiesNodeHelper.SetHatched(const Value: boolean);
begin
  FdHatched.Send(Value);
end;

function TFillPropertiesNodeHelper.GetHatchStyle: Integer;
begin
  Result := FdHatchStyle.Value;
end;

procedure TFillPropertiesNodeHelper.SetHatchStyle(const Value: Integer);
begin
  FdHatchStyle.Send(Value);
end;

{ TLinePropertiesNode ----------------------------------------------- }

function TLinePropertiesNodeHelper.GetApplied: boolean;
begin
  Result := FdApplied.Value;
end;

procedure TLinePropertiesNodeHelper.SetApplied(const Value: boolean);
begin
  FdApplied.Send(Value);
end;

function TLinePropertiesNodeHelper.GetLinewidthScaleFactor: Single;
begin
  Result := FdLinewidthScaleFactor.Value;
end;

procedure TLinePropertiesNodeHelper.SetLinewidthScaleFactor(const Value: Single);
begin
  FdLinewidthScaleFactor.Send(Value);
end;

{ TMaterialNode ----------------------------------------------- }

function TMaterialNodeHelper.GetAmbientIntensity: Single;
begin
  Result := FdAmbientIntensity.Value;
end;

procedure TMaterialNodeHelper.SetAmbientIntensity(const Value: Single);
begin
  FdAmbientIntensity.Send(Value);
end;

function TMaterialNodeHelper.GetDiffuseColor: TCastleColorRGB;
begin
  Result := FdDiffuseColor.Value;
end;

procedure TMaterialNodeHelper.SetDiffuseColor(const Value: TCastleColorRGB);
begin
  FdDiffuseColor.Send(Value);
end;

function TMaterialNodeHelper.GetEmissiveColor: TCastleColorRGB;
begin
  Result := FdEmissiveColor.Value;
end;

procedure TMaterialNodeHelper.SetEmissiveColor(const Value: TCastleColorRGB);
begin
  FdEmissiveColor.Send(Value);
end;

function TMaterialNodeHelper.GetShininess: Single;
begin
  Result := FdShininess.Value;
end;

procedure TMaterialNodeHelper.SetShininess(const Value: Single);
begin
  FdShininess.Send(Value);
end;

function TMaterialNodeHelper.GetSpecularColor: TCastleColorRGB;
begin
  Result := FdSpecularColor.Value;
end;

procedure TMaterialNodeHelper.SetSpecularColor(const Value: TCastleColorRGB);
begin
  FdSpecularColor.Send(Value);
end;

function TMaterialNodeHelper.GetTransparency: Single;
begin
  Result := FdTransparency.Value;
end;

procedure TMaterialNodeHelper.SetTransparency(const Value: Single);
begin
  FdTransparency.Send(Value);
end;

function TMaterialNodeHelper.GetMirror: Single;
begin
  Result := FdMirror.Value;
end;

procedure TMaterialNodeHelper.SetMirror(const Value: Single);
begin
  FdMirror.Send(Value);
end;

function TMaterialNodeHelper.GetReflSpecularExp: Single;
begin
  Result := FdReflSpecularExp.Value;
end;

procedure TMaterialNodeHelper.SetReflSpecularExp(const Value: Single);
begin
  FdReflSpecularExp.Send(Value);
end;

function TMaterialNodeHelper.GetTransSpecularExp: Single;
begin
  Result := FdTransSpecularExp.Value;
end;

procedure TMaterialNodeHelper.SetTransSpecularExp(const Value: Single);
begin
  FdTransSpecularExp.Send(Value);
end;

{ TTwoSidedMaterialNode ----------------------------------------------- }

function TTwoSidedMaterialNodeHelper.GetAmbientIntensity: Single;
begin
  Result := FdAmbientIntensity.Value;
end;

procedure TTwoSidedMaterialNodeHelper.SetAmbientIntensity(const Value: Single);
begin
  FdAmbientIntensity.Send(Value);
end;

function TTwoSidedMaterialNodeHelper.GetBackAmbientIntensity: Single;
begin
  Result := FdBackAmbientIntensity.Value;
end;

procedure TTwoSidedMaterialNodeHelper.SetBackAmbientIntensity(const Value: Single);
begin
  FdBackAmbientIntensity.Send(Value);
end;

function TTwoSidedMaterialNodeHelper.GetBackDiffuseColor: TCastleColorRGB;
begin
  Result := FdBackDiffuseColor.Value;
end;

procedure TTwoSidedMaterialNodeHelper.SetBackDiffuseColor(const Value: TCastleColorRGB);
begin
  FdBackDiffuseColor.Send(Value);
end;

function TTwoSidedMaterialNodeHelper.GetBackEmissiveColor: TCastleColorRGB;
begin
  Result := FdBackEmissiveColor.Value;
end;

procedure TTwoSidedMaterialNodeHelper.SetBackEmissiveColor(const Value: TCastleColorRGB);
begin
  FdBackEmissiveColor.Send(Value);
end;

function TTwoSidedMaterialNodeHelper.GetBackShininess: Single;
begin
  Result := FdBackShininess.Value;
end;

procedure TTwoSidedMaterialNodeHelper.SetBackShininess(const Value: Single);
begin
  FdBackShininess.Send(Value);
end;

function TTwoSidedMaterialNodeHelper.GetBackSpecularColor: TCastleColorRGB;
begin
  Result := FdBackSpecularColor.Value;
end;

procedure TTwoSidedMaterialNodeHelper.SetBackSpecularColor(const Value: TCastleColorRGB);
begin
  FdBackSpecularColor.Send(Value);
end;

function TTwoSidedMaterialNodeHelper.GetBackTransparency: Single;
begin
  Result := FdBackTransparency.Value;
end;

procedure TTwoSidedMaterialNodeHelper.SetBackTransparency(const Value: Single);
begin
  FdBackTransparency.Send(Value);
end;

function TTwoSidedMaterialNodeHelper.GetDiffuseColor: TCastleColorRGB;
begin
  Result := FdDiffuseColor.Value;
end;

procedure TTwoSidedMaterialNodeHelper.SetDiffuseColor(const Value: TCastleColorRGB);
begin
  FdDiffuseColor.Send(Value);
end;

function TTwoSidedMaterialNodeHelper.GetEmissiveColor: TCastleColorRGB;
begin
  Result := FdEmissiveColor.Value;
end;

procedure TTwoSidedMaterialNodeHelper.SetEmissiveColor(const Value: TCastleColorRGB);
begin
  FdEmissiveColor.Send(Value);
end;

function TTwoSidedMaterialNodeHelper.GetShininess: Single;
begin
  Result := FdShininess.Value;
end;

procedure TTwoSidedMaterialNodeHelper.SetShininess(const Value: Single);
begin
  FdShininess.Send(Value);
end;

function TTwoSidedMaterialNodeHelper.GetSeparateBackColor: boolean;
begin
  Result := FdSeparateBackColor.Value;
end;

procedure TTwoSidedMaterialNodeHelper.SetSeparateBackColor(const Value: boolean);
begin
  FdSeparateBackColor.Send(Value);
end;

function TTwoSidedMaterialNodeHelper.GetSpecularColor: TCastleColorRGB;
begin
  Result := FdSpecularColor.Value;
end;

procedure TTwoSidedMaterialNodeHelper.SetSpecularColor(const Value: TCastleColorRGB);
begin
  FdSpecularColor.Send(Value);
end;

function TTwoSidedMaterialNodeHelper.GetTransparency: Single;
begin
  Result := FdTransparency.Value;
end;

procedure TTwoSidedMaterialNodeHelper.SetTransparency(const Value: Single);
begin
  FdTransparency.Send(Value);
end;

{ TAbstractSoundSourceNode ----------------------------------------------- }

function TAbstractSoundSourceNodeHelper.GetDescription: string;
begin
  Result := FdDescription.Value;
end;

procedure TAbstractSoundSourceNodeHelper.SetDescription(const Value: string);
begin
  FdDescription.Send(Value);
end;

function TAbstractSoundSourceNodeHelper.GetPitch: Single;
begin
  Result := FdPitch.Value;
end;

procedure TAbstractSoundSourceNodeHelper.SetPitch(const Value: Single);
begin
  FdPitch.Send(Value);
end;

{ TSoundNode ----------------------------------------------- }

function TSoundNodeHelper.GetDirection: TVector3Single;
begin
  Result := FdDirection.Value;
end;

procedure TSoundNodeHelper.SetDirection(const Value: TVector3Single);
begin
  FdDirection.Send(Value);
end;

function TSoundNodeHelper.GetIntensity: Single;
begin
  Result := FdIntensity.Value;
end;

procedure TSoundNodeHelper.SetIntensity(const Value: Single);
begin
  FdIntensity.Send(Value);
end;

function TSoundNodeHelper.GetLocation: TVector3Single;
begin
  Result := FdLocation.Value;
end;

procedure TSoundNodeHelper.SetLocation(const Value: TVector3Single);
begin
  FdLocation.Send(Value);
end;

function TSoundNodeHelper.GetMaxBack: Single;
begin
  Result := FdMaxBack.Value;
end;

procedure TSoundNodeHelper.SetMaxBack(const Value: Single);
begin
  FdMaxBack.Send(Value);
end;

function TSoundNodeHelper.GetMaxFront: Single;
begin
  Result := FdMaxFront.Value;
end;

procedure TSoundNodeHelper.SetMaxFront(const Value: Single);
begin
  FdMaxFront.Send(Value);
end;

function TSoundNodeHelper.GetMinBack: Single;
begin
  Result := FdMinBack.Value;
end;

procedure TSoundNodeHelper.SetMinBack(const Value: Single);
begin
  FdMinBack.Send(Value);
end;

function TSoundNodeHelper.GetMinFront: Single;
begin
  Result := FdMinFront.Value;
end;

procedure TSoundNodeHelper.SetMinFront(const Value: Single);
begin
  FdMinFront.Send(Value);
end;

function TSoundNodeHelper.GetPriority: Single;
begin
  Result := FdPriority.Value;
end;

procedure TSoundNodeHelper.SetPriority(const Value: Single);
begin
  FdPriority.Send(Value);
end;

function TSoundNodeHelper.GetSpatialize: boolean;
begin
  Result := FdSpatialize.Value;
end;

procedure TSoundNodeHelper.SetSpatialize(const Value: boolean);
begin
  FdSpatialize.Send(Value);
end;

{ TFontStyleNode ----------------------------------------------- }

function TFontStyleNodeHelper.GetHorizontal: boolean;
begin
  Result := FdHorizontal.Value;
end;

procedure TFontStyleNodeHelper.SetHorizontal(const Value: boolean);
begin
  FdHorizontal.Send(Value);
end;

function TFontStyleNodeHelper.GetLanguage: string;
begin
  Result := FdLanguage.Value;
end;

procedure TFontStyleNodeHelper.SetLanguage(const Value: string);
begin
  FdLanguage.Send(Value);
end;

function TFontStyleNodeHelper.GetLeftToRight: boolean;
begin
  Result := FdLeftToRight.Value;
end;

procedure TFontStyleNodeHelper.SetLeftToRight(const Value: boolean);
begin
  FdLeftToRight.Send(Value);
end;

function TFontStyleNodeHelper.GetSize: Single;
begin
  Result := FdSize.Value;
end;

procedure TFontStyleNodeHelper.SetSize(const Value: Single);
begin
  FdSize.Send(Value);
end;

function TFontStyleNodeHelper.GetSpacing: Single;
begin
  Result := FdSpacing.Value;
end;

procedure TFontStyleNodeHelper.SetSpacing(const Value: Single);
begin
  FdSpacing.Send(Value);
end;

function TFontStyleNodeHelper.GetTopToBottom: boolean;
begin
  Result := FdTopToBottom.Value;
end;

procedure TFontStyleNodeHelper.SetTopToBottom(const Value: boolean);
begin
  FdTopToBottom.Send(Value);
end;

{ TTextNode ----------------------------------------------- }

function TTextNodeHelper.GetMaxExtent: Single;
begin
  Result := FdMaxExtent.Value;
end;

procedure TTextNodeHelper.SetMaxExtent(const Value: Single);
begin
  FdMaxExtent.Send(Value);
end;

{ TAbstractTexture3DNode ----------------------------------------------- }

function TAbstractTexture3DNodeHelper.GetRepeatR: boolean;
begin
  Result := FdRepeatR.Value;
end;

procedure TAbstractTexture3DNodeHelper.SetRepeatR(const Value: boolean);
begin
  FdRepeatR.Send(Value);
end;

{ TTextureTransformMatrix3DNode ----------------------------------------------- }

function TTextureTransformMatrix3DNodeHelper.GetMatrix: TMatrix4Single;
begin
  Result := FdMatrix.Value;
end;

procedure TTextureTransformMatrix3DNodeHelper.SetMatrix(const Value: TMatrix4Single);
begin
  FdMatrix.Send(Value);
end;

{ TTextureTransform3DNode ----------------------------------------------- }

function TTextureTransform3DNodeHelper.GetCenter: TVector3Single;
begin
  Result := FdCenter.Value;
end;

procedure TTextureTransform3DNodeHelper.SetCenter(const Value: TVector3Single);
begin
  FdCenter.Send(Value);
end;

function TTextureTransform3DNodeHelper.GetRotation: TVector4Single;
begin
  Result := FdRotation.Value;
end;

procedure TTextureTransform3DNodeHelper.SetRotation(const Value: TVector4Single);
begin
  FdRotation.Send(Value);
end;

function TTextureTransform3DNodeHelper.GetScale: TVector3Single;
begin
  Result := FdScale.Value;
end;

procedure TTextureTransform3DNodeHelper.SetScale(const Value: TVector3Single);
begin
  FdScale.Send(Value);
end;

function TTextureTransform3DNodeHelper.GetTranslation: TVector3Single;
begin
  Result := FdTranslation.Value;
end;

procedure TTextureTransform3DNodeHelper.SetTranslation(const Value: TVector3Single);
begin
  FdTranslation.Send(Value);
end;

{ TMovieTextureNode ----------------------------------------------- }

function TMovieTextureNodeHelper.GetDescription: string;
begin
  Result := FdDescription.Value;
end;

procedure TMovieTextureNodeHelper.SetDescription(const Value: string);
begin
  FdDescription.Send(Value);
end;

function TMovieTextureNodeHelper.GetLoop: boolean;
begin
  Result := FdLoop.Value;
end;

procedure TMovieTextureNodeHelper.SetLoop(const Value: boolean);
begin
  FdLoop.Send(Value);
end;

function TMovieTextureNodeHelper.GetPauseTime: TFloatTime;
begin
  Result := FdPauseTime.Value;
end;

procedure TMovieTextureNodeHelper.SetPauseTime(const Value: TFloatTime);
begin
  FdPauseTime.Send(Value);
end;

function TMovieTextureNodeHelper.GetResumeTime: TFloatTime;
begin
  Result := FdResumeTime.Value;
end;

procedure TMovieTextureNodeHelper.SetResumeTime(const Value: TFloatTime);
begin
  FdResumeTime.Send(Value);
end;

function TMovieTextureNodeHelper.GetSpeed: Single;
begin
  Result := FdSpeed.Value;
end;

procedure TMovieTextureNodeHelper.SetSpeed(const Value: Single);
begin
  FdSpeed.Send(Value);
end;

function TMovieTextureNodeHelper.GetStartTime: TFloatTime;
begin
  Result := FdStartTime.Value;
end;

procedure TMovieTextureNodeHelper.SetStartTime(const Value: TFloatTime);
begin
  FdStartTime.Send(Value);
end;

function TMovieTextureNodeHelper.GetStopTime: TFloatTime;
begin
  Result := FdStopTime.Value;
end;

procedure TMovieTextureNodeHelper.SetStopTime(const Value: TFloatTime);
begin
  FdStopTime.Send(Value);
end;

{ TMultiTextureNode ----------------------------------------------- }

function TMultiTextureNodeHelper.GetAlpha: Single;
begin
  Result := FdAlpha.Value;
end;

procedure TMultiTextureNodeHelper.SetAlpha(const Value: Single);
begin
  FdAlpha.Send(Value);
end;

function TMultiTextureNodeHelper.GetColor: TCastleColorRGB;
begin
  Result := FdColor.Value;
end;

procedure TMultiTextureNodeHelper.SetColor(const Value: TCastleColorRGB);
begin
  FdColor.Send(Value);
end;

{ TTextureCoordinateGeneratorNode ----------------------------------------------- }

function TTextureCoordinateGeneratorNodeHelper.GetMode: string;
begin
  Result := FdMode.Value;
end;

procedure TTextureCoordinateGeneratorNodeHelper.SetMode(const Value: string);
begin
  FdMode.Send(Value);
end;

{ TTexturePropertiesNode ----------------------------------------------- }

function TTexturePropertiesNodeHelper.GetAnisotropicDegree: Single;
begin
  Result := FdAnisotropicDegree.Value;
end;

procedure TTexturePropertiesNodeHelper.SetAnisotropicDegree(const Value: Single);
begin
  FdAnisotropicDegree.Send(Value);
end;

function TTexturePropertiesNodeHelper.GetBorderColor: TCastleColor;
begin
  Result := FdBorderColor.Value;
end;

procedure TTexturePropertiesNodeHelper.SetBorderColor(const Value: TCastleColor);
begin
  FdBorderColor.Send(Value);
end;

function TTexturePropertiesNodeHelper.GetBorderWidth: Integer;
begin
  Result := FdBorderWidth.Value;
end;

procedure TTexturePropertiesNodeHelper.SetBorderWidth(const Value: Integer);
begin
  FdBorderWidth.Send(Value);
end;

function TTexturePropertiesNodeHelper.GetBoundaryModeS: string;
begin
  Result := FdBoundaryModeS.Value;
end;

procedure TTexturePropertiesNodeHelper.SetBoundaryModeS(const Value: string);
begin
  FdBoundaryModeS.Send(Value);
end;

function TTexturePropertiesNodeHelper.GetBoundaryModeT: string;
begin
  Result := FdBoundaryModeT.Value;
end;

procedure TTexturePropertiesNodeHelper.SetBoundaryModeT(const Value: string);
begin
  FdBoundaryModeT.Send(Value);
end;

function TTexturePropertiesNodeHelper.GetBoundaryModeR: string;
begin
  Result := FdBoundaryModeR.Value;
end;

procedure TTexturePropertiesNodeHelper.SetBoundaryModeR(const Value: string);
begin
  FdBoundaryModeR.Send(Value);
end;

function TTexturePropertiesNodeHelper.GetMagnificationFilter: string;
begin
  Result := FdMagnificationFilter.Value;
end;

procedure TTexturePropertiesNodeHelper.SetMagnificationFilter(const Value: string);
begin
  FdMagnificationFilter.Send(Value);
end;

function TTexturePropertiesNodeHelper.GetMinificationFilter: string;
begin
  Result := FdMinificationFilter.Value;
end;

procedure TTexturePropertiesNodeHelper.SetMinificationFilter(const Value: string);
begin
  FdMinificationFilter.Send(Value);
end;

function TTexturePropertiesNodeHelper.GetTextureCompression: string;
begin
  Result := FdTextureCompression.Value;
end;

procedure TTexturePropertiesNodeHelper.SetTextureCompression(const Value: string);
begin
  FdTextureCompression.Send(Value);
end;

function TTexturePropertiesNodeHelper.GetTexturePriority: Single;
begin
  Result := FdTexturePriority.Value;
end;

procedure TTexturePropertiesNodeHelper.SetTexturePriority(const Value: Single);
begin
  FdTexturePriority.Send(Value);
end;

function TTexturePropertiesNodeHelper.GetGenerateMipMaps: boolean;
begin
  Result := FdGenerateMipMaps.Value;
end;

procedure TTexturePropertiesNodeHelper.SetGenerateMipMaps(const Value: boolean);
begin
  FdGenerateMipMaps.Send(Value);
end;

{ TTextureTransformNode ----------------------------------------------- }

function TTextureTransformNodeHelper.GetCenter: TVector2Single;
begin
  Result := FdCenter.Value;
end;

procedure TTextureTransformNodeHelper.SetCenter(const Value: TVector2Single);
begin
  FdCenter.Send(Value);
end;

function TTextureTransformNodeHelper.GetRotation: Single;
begin
  Result := FdRotation.Value;
end;

procedure TTextureTransformNodeHelper.SetRotation(const Value: Single);
begin
  FdRotation.Send(Value);
end;

function TTextureTransformNodeHelper.GetScale: TVector2Single;
begin
  Result := FdScale.Value;
end;

procedure TTextureTransformNodeHelper.SetScale(const Value: TVector2Single);
begin
  FdScale.Send(Value);
end;

function TTextureTransformNodeHelper.GetTranslation: TVector2Single;
begin
  Result := FdTranslation.Value;
end;

procedure TTextureTransformNodeHelper.SetTranslation(const Value: TVector2Single);
begin
  FdTranslation.Send(Value);
end;

{ TAbstractTimeDependentNode ----------------------------------------------- }

function TAbstractTimeDependentNodeHelper.GetLoop: boolean;
begin
  Result := FdLoop.Value;
end;

procedure TAbstractTimeDependentNodeHelper.SetLoop(const Value: boolean);
begin
  FdLoop.Send(Value);
end;

function TAbstractTimeDependentNodeHelper.GetPauseTime: TFloatTime;
begin
  Result := FdPauseTime.Value;
end;

procedure TAbstractTimeDependentNodeHelper.SetPauseTime(const Value: TFloatTime);
begin
  FdPauseTime.Send(Value);
end;

function TAbstractTimeDependentNodeHelper.GetResumeTime: TFloatTime;
begin
  Result := FdResumeTime.Value;
end;

procedure TAbstractTimeDependentNodeHelper.SetResumeTime(const Value: TFloatTime);
begin
  FdResumeTime.Send(Value);
end;

function TAbstractTimeDependentNodeHelper.GetStartTime: TFloatTime;
begin
  Result := FdStartTime.Value;
end;

procedure TAbstractTimeDependentNodeHelper.SetStartTime(const Value: TFloatTime);
begin
  FdStartTime.Send(Value);
end;

function TAbstractTimeDependentNodeHelper.GetStopTime: TFloatTime;
begin
  Result := FdStopTime.Value;
end;

procedure TAbstractTimeDependentNodeHelper.SetStopTime(const Value: TFloatTime);
begin
  FdStopTime.Send(Value);
end;

{ TTimeSensorNode ----------------------------------------------- }

function TTimeSensorNodeHelper.GetCycleInterval: TFloatTime;
begin
  Result := FdCycleInterval.Value;
end;

procedure TTimeSensorNodeHelper.SetCycleInterval(const Value: TFloatTime);
begin
  FdCycleInterval.Send(Value);
end;

function TTimeSensorNodeHelper.GetEnabled: boolean;
begin
  Result := FdEnabled.Value;
end;

procedure TTimeSensorNodeHelper.SetEnabled(const Value: boolean);
begin
  FdEnabled.Send(Value);
end;

{ TAppearanceNode ----------------------------------------------- }

function TAppearanceNodeHelper.GetShadowCaster: boolean;
begin
  Result := FdShadowCaster.Value;
end;

procedure TAppearanceNodeHelper.SetShadowCaster(const Value: boolean);
begin
  FdShadowCaster.Send(Value);
end;

function TAppearanceNodeHelper.GetHeightMapScale: Single;
begin
  Result := FdHeightMapScale.Value;
end;

procedure TAppearanceNodeHelper.SetHeightMapScale(const Value: Single);
begin
  FdHeightMapScale.Send(Value);
end;

{ TGeneratedShadowMapNode ----------------------------------------------- }

function TGeneratedShadowMapNodeHelper.GetSize: Integer;
begin
  Result := FdSize.Value;
end;

procedure TGeneratedShadowMapNodeHelper.SetSize(const Value: Integer);
begin
  FdSize.Send(Value);
end;

function TGeneratedShadowMapNodeHelper.GetScale: Single;
begin
  Result := FdScale.Value;
end;

procedure TGeneratedShadowMapNodeHelper.SetScale(const Value: Single);
begin
  FdScale.Send(Value);
end;

function TGeneratedShadowMapNodeHelper.GetBias: Single;
begin
  Result := FdBias.Value;
end;

procedure TGeneratedShadowMapNodeHelper.SetBias(const Value: Single);
begin
  FdBias.Send(Value);
end;

{ TText3DNode ----------------------------------------------- }

function TText3DNodeHelper.GetMaxExtent: Single;
begin
  Result := FdMaxExtent.Value;
end;

procedure TText3DNodeHelper.SetMaxExtent(const Value: Single);
begin
  FdMaxExtent.Send(Value);
end;

function TText3DNodeHelper.GetDepth: Single;
begin
  Result := FdDepth.Value;
end;

procedure TText3DNodeHelper.SetDepth(const Value: Single);
begin
  FdDepth.Send(Value);
end;

{ TKambiNavigationInfoNode ----------------------------------------------- }

function TKambiNavigationInfoNodeHelper.GetTimeOriginAtLoad: boolean;
begin
  Result := FdTimeOriginAtLoad.Value;
end;

procedure TKambiNavigationInfoNodeHelper.SetTimeOriginAtLoad(const Value: boolean);
begin
  FdTimeOriginAtLoad.Send(Value);
end;

function TKambiNavigationInfoNodeHelper.GetHeadBobbing: Single;
begin
  Result := FdHeadBobbing.Value;
end;

procedure TKambiNavigationInfoNodeHelper.SetHeadBobbing(const Value: Single);
begin
  FdHeadBobbing.Send(Value);
end;

function TKambiNavigationInfoNodeHelper.GetHeadBobbingTime: Single;
begin
  Result := FdHeadBobbingTime.Value;
end;

procedure TKambiNavigationInfoNodeHelper.SetHeadBobbingTime(const Value: Single);
begin
  FdHeadBobbingTime.Send(Value);
end;

{ TKambiHeadLightNode ----------------------------------------------- }

function TKambiHeadLightNodeHelper.GetAmbientIntensity: Single;
begin
  Result := FdAmbientIntensity.Value;
end;

procedure TKambiHeadLightNodeHelper.SetAmbientIntensity(const Value: Single);
begin
  FdAmbientIntensity.Send(Value);
end;

function TKambiHeadLightNodeHelper.GetAttenuation: TVector3Single;
begin
  Result := FdAttenuation.Value;
end;

procedure TKambiHeadLightNodeHelper.SetAttenuation(const Value: TVector3Single);
begin
  FdAttenuation.Send(Value);
end;

function TKambiHeadLightNodeHelper.GetColor: TCastleColorRGB;
begin
  Result := FdColor.Value;
end;

procedure TKambiHeadLightNodeHelper.SetColor(const Value: TCastleColorRGB);
begin
  FdColor.Send(Value);
end;

function TKambiHeadLightNodeHelper.GetIntensity: Single;
begin
  Result := FdIntensity.Value;
end;

procedure TKambiHeadLightNodeHelper.SetIntensity(const Value: Single);
begin
  FdIntensity.Send(Value);
end;

function TKambiHeadLightNodeHelper.GetSpot: boolean;
begin
  Result := FdSpot.Value;
end;

procedure TKambiHeadLightNodeHelper.SetSpot(const Value: boolean);
begin
  FdSpot.Send(Value);
end;

function TKambiHeadLightNodeHelper.GetSpotDropOffRate: Single;
begin
  Result := FdSpotDropOffRate.Value;
end;

procedure TKambiHeadLightNodeHelper.SetSpotDropOffRate(const Value: Single);
begin
  FdSpotDropOffRate.Send(Value);
end;

function TKambiHeadLightNodeHelper.GetSpotCutOffAngle: Single;
begin
  Result := FdSpotCutOffAngle.Value;
end;

procedure TKambiHeadLightNodeHelper.SetSpotCutOffAngle(const Value: Single);
begin
  FdSpotCutOffAngle.Send(Value);
end;

{ TKambiOctreePropertiesNode ----------------------------------------------- }

function TKambiOctreePropertiesNodeHelper.GetMaxDepth: Integer;
begin
  Result := FdMaxDepth.Value;
end;

procedure TKambiOctreePropertiesNodeHelper.SetMaxDepth(const Value: Integer);
begin
  FdMaxDepth.Send(Value);
end;

function TKambiOctreePropertiesNodeHelper.GetLeafCapacity: Integer;
begin
  Result := FdLeafCapacity.Value;
end;

procedure TKambiOctreePropertiesNodeHelper.SetLeafCapacity(const Value: Integer);
begin
  FdLeafCapacity.Send(Value);
end;

{ TTeapotNode ----------------------------------------------- }

function TTeapotNodeHelper.GetSize: TVector3Single;
begin
  Result := FdSize.Value;
end;

procedure TTeapotNodeHelper.SetSize(const Value: TVector3Single);
begin
  FdSize.Send(Value);
end;

function TTeapotNodeHelper.GetManifold: boolean;
begin
  Result := FdManifold.Value;
end;

procedure TTeapotNodeHelper.SetManifold(const Value: boolean);
begin
  FdManifold.Send(Value);
end;

{ TRenderedTextureNode ----------------------------------------------- }

function TRenderedTextureNodeHelper.GetRepeatR: boolean;
begin
  Result := FdRepeatR.Value;
end;

procedure TRenderedTextureNodeHelper.SetRepeatR(const Value: boolean);
begin
  FdRepeatR.Send(Value);
end;

{ TLoggerNode ----------------------------------------------- }

function TLoggerNodeHelper.GetLevel: Integer;
begin
  Result := FdLevel.Value;
end;

procedure TLoggerNodeHelper.SetLevel(const Value: Integer);
begin
  FdLevel.Send(Value);
end;

function TLoggerNodeHelper.GetLogFile: string;
begin
  Result := FdLogFile.Value;
end;

procedure TLoggerNodeHelper.SetLogFile(const Value: string);
begin
  FdLogFile.Send(Value);
end;

function TLoggerNodeHelper.GetEnabled: boolean;
begin
  Result := FdEnabled.Value;
end;

procedure TLoggerNodeHelper.SetEnabled(const Value: boolean);
begin
  FdEnabled.Send(Value);
end;

{ TScreenEffectNode ----------------------------------------------- }

function TScreenEffectNodeHelper.GetEnabled: boolean;
begin
  Result := FdEnabled.Value;
end;

procedure TScreenEffectNodeHelper.SetEnabled(const Value: boolean);
begin
  FdEnabled.Send(Value);
end;

function TScreenEffectNodeHelper.GetNeedsDepth: boolean;
begin
  Result := FdNeedsDepth.Value;
end;

procedure TScreenEffectNodeHelper.SetNeedsDepth(const Value: boolean);
begin
  FdNeedsDepth.Send(Value);
end;

{ TEffectNode ----------------------------------------------- }

function TEffectNodeHelper.GetEnabled: boolean;
begin
  Result := FdEnabled.Value;
end;

procedure TEffectNodeHelper.SetEnabled(const Value: boolean);
begin
  FdEnabled.Send(Value);
end;

{$endif read_implementation}
