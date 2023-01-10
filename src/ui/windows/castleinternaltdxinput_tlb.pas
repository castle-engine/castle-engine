unit CastleInternalTDxInput_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 30.05.2007 18:27:57 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Program Files\3Dconnexion\3Dconnexion 3DxSoftware\3DxWare64\win32\TDxInput.dll (1)
// LIBID: {7858B9E0-5793-4BE4-9B53-661D922790D2}
// LCID: 0
// Helpfile: 
// HelpString: 3Dconnexion TDxInput 1.0 Type Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
// Errors:
//   Hint: Parameter 'label' of IKeyboard.GetKeyLabel changed to 'label_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Error creating palette bitmap of (TAngleAxis) : Server C:\Program Files\3Dconnexion\3Dconnexion 3DxSoftware\3DxWare64\win32\TDxInput.dll contains no icons
//   Error creating palette bitmap of (TVector3D) : Server C:\Program Files\3Dconnexion\3Dconnexion 3DxSoftware\3DxWare64\win32\TDxInput.dll contains no icons
//   Error creating palette bitmap of (TSensor) : Server C:\Program Files\3Dconnexion\3Dconnexion 3DxSoftware\3DxWare64\win32\TDxInput.dll contains no icons
//   Error creating palette bitmap of (TKeyboard) : Server C:\Program Files\3Dconnexion\3Dconnexion 3DxSoftware\3DxWare64\win32\TDxInput.dll contains no icons
//   Error creating palette bitmap of (TDevice) : Server C:\Program Files\3Dconnexion\3Dconnexion 3DxSoftware\3DxWare64\win32\TDxInput.dll contains no icons
//   Error creating palette bitmap of (TTDxInfo) : Server C:\Program Files\3Dconnexion\3Dconnexion 3DxSoftware\3DxWare64\win32\TDxInput.dll contains no icons
// ************************************************************************ //
// *************************************************************************//
// NOTE:                                                                      
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties  
// which return objects that may need to be explicitly created via a function 
// call prior to any access via the property. These items have been disabled  
// in order to prevent accidental use from within the object inspector. You   
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively   
// removing them from the $IFDEF blocks. However, such items must still be    
// programmatically created via a method of the appropriate CoClass before    
// they can be used.                                                          
{$I castleconf.inc}
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, {Graphics, }{$ifdef FPC}OleServer,{$else}Vcl.OleServer,{$endif} {StdVCL, }Variants;


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  TDxInputMajorVersion = 1;
  TDxInputMinorVersion = 0;

  LIBID_TDxInput: TGUID = '{7858B9E0-5793-4BE4-9B53-661D922790D2}';

  IID_IAngleAxis: TGUID = '{1EF2BAFF-54E9-4706-9F61-078F7134FD35}';
  CLASS_AngleAxis: TGUID = '{512A6C3E-3010-401B-8623-E413E2ACC138}';
  IID_IVector3D: TGUID = '{8C2AA71D-2B23-43F5-A6ED-4DF57E9CD8D5}';
  CLASS_Vector3D: TGUID = '{740A7479-C7C1-44DA-8A84-B5DE63C78B32}';
  DIID__ISensorEvents: TGUID = '{E6929A4A-6F41-46C6-9252-A8CC53472CB1}';
  IID_ISensor: TGUID = '{F3A6775E-6FA1-4829-BF32-5B045C29078F}';
  CLASS_Sensor: TGUID = '{85004B00-1AA7-4777-B1CE-8427301B942D}';
  DIID__IKeyboardEvents: TGUID = '{6B6BB0A8-4491-40CF-B1A9-C15A801FE151}';
  IID_IKeyboard: TGUID = '{D6F968E7-2993-48D7-AF24-8B602D925B2C}';
  CLASS_Keyboard: TGUID = '{25BBE090-583A-4903-A61B-D0EC629AC4EC}';
  DIID__ISimpleDeviceEvents: TGUID = '{8FE3A216-E235-49A6-9136-F9D81FDADEF5}';
  IID_ISimpleDevice: TGUID = '{CB3BF65E-0816-482A-BB11-64AF1E837812}';
  CLASS_Device: TGUID = '{82C5AB54-C92C-4D52-AAC5-27E25E22604C}';
  IID_ITDxInfo: TGUID = '{00612962-8FB6-47B2-BF98-4E8C0FF5F559}';
  CLASS_TDxInfo: TGUID = '{1A960ECE-0E57-4A68-B694-8373114F1FF4}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IAngleAxis = interface;
  IAngleAxisDisp = dispinterface;
  IVector3D = interface;
  IVector3DDisp = dispinterface;
  _ISensorEvents = dispinterface;
  ISensor = interface;
  ISensorDisp = dispinterface;
  _IKeyboardEvents = dispinterface;
  IKeyboard = interface;
  IKeyboardDisp = dispinterface;
  _ISimpleDeviceEvents = dispinterface;
  ISimpleDevice = interface;
  ISimpleDeviceDisp = dispinterface;
  ITDxInfo = interface;
  ITDxInfoDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  AngleAxis = IAngleAxis;
  Vector3D = IVector3D;
  Sensor = ISensor;
  Keyboard = IKeyboard;
  Device = ISimpleDevice;
  TDxInfo = ITDxInfo;


// *********************************************************************//
// Interface: IAngleAxis
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {1EF2BAFF-54E9-4706-9F61-078F7134FD35}
// *********************************************************************//
  IAngleAxis = interface(IDispatch)
    ['{1EF2BAFF-54E9-4706-9F61-078F7134FD35}']
    function Get_X: Double; safecall;
    procedure Set_X(pVal: Double); safecall;
    function Get_Y: Double; safecall;
    procedure Set_Y(pVal: Double); safecall;
    function Get_Z: Double; safecall;
    procedure Set_Z(pVal: Double); safecall;
    function Get_Angle: Double; safecall;
    procedure Set_Angle(pVal: Double); safecall;
    property X: Double read Get_X write Set_X;
    property Y: Double read Get_Y write Set_Y;
    property Z: Double read Get_Z write Set_Z;
    property Angle: Double read Get_Angle write Set_Angle;
  end;

// *********************************************************************//
// DispIntf:  IAngleAxisDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {1EF2BAFF-54E9-4706-9F61-078F7134FD35}
// *********************************************************************//
  IAngleAxisDisp = dispinterface
    ['{1EF2BAFF-54E9-4706-9F61-078F7134FD35}']
    property X: Double dispid 1;
    property Y: Double dispid 2;
    property Z: Double dispid 3;
    property Angle: Double dispid 4;
  end;

// *********************************************************************//
// Interface: IVector3D
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8C2AA71D-2B23-43F5-A6ED-4DF57E9CD8D5}
// *********************************************************************//
  IVector3D = interface(IDispatch)
    ['{8C2AA71D-2B23-43F5-A6ED-4DF57E9CD8D5}']
    function Get_X: Double; safecall;
    procedure Set_X(pVal: Double); safecall;
    function Get_Y: Double; safecall;
    procedure Set_Y(pVal: Double); safecall;
    function Get_Z: Double; safecall;
    procedure Set_Z(pVal: Double); safecall;
    function Get_Length: Double; safecall;
    procedure Set_Length(pVal: Double); safecall;
    property X: Double read Get_X write Set_X;
    property Y: Double read Get_Y write Set_Y;
    property Z: Double read Get_Z write Set_Z;
    property Length: Double read Get_Length write Set_Length;
  end;

// *********************************************************************//
// DispIntf:  IVector3DDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8C2AA71D-2B23-43F5-A6ED-4DF57E9CD8D5}
// *********************************************************************//
  IVector3DDisp = dispinterface
    ['{8C2AA71D-2B23-43F5-A6ED-4DF57E9CD8D5}']
    property X: Double dispid 1;
    property Y: Double dispid 2;
    property Z: Double dispid 3;
    property Length: Double dispid 4;
  end;

// *********************************************************************//
// DispIntf:  _ISensorEvents
// Flags:     (4096) Dispatchable
// GUID:      {E6929A4A-6F41-46C6-9252-A8CC53472CB1}
// *********************************************************************//
  _ISensorEvents = dispinterface
    ['{E6929A4A-6F41-46C6-9252-A8CC53472CB1}']
    procedure SensorInput; dispid 1;
  end;

// *********************************************************************//
// Interface: ISensor
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F3A6775E-6FA1-4829-BF32-5B045C29078F}
// *********************************************************************//
  ISensor = interface(IDispatch)
    ['{F3A6775E-6FA1-4829-BF32-5B045C29078F}']
    function Get_Translation: IVector3D; safecall;
    function Get_Rotation: IAngleAxis; safecall;
    function Get_Device: IDispatch; safecall;
    function Get_Period: Double; safecall;
    property Translation: IVector3D read Get_Translation;
    property Rotation: IAngleAxis read Get_Rotation;
    property Device: IDispatch read Get_Device;
    property Period: Double read Get_Period;
  end;

// *********************************************************************//
// DispIntf:  ISensorDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F3A6775E-6FA1-4829-BF32-5B045C29078F}
// *********************************************************************//
  ISensorDisp = dispinterface
    ['{F3A6775E-6FA1-4829-BF32-5B045C29078F}']
    property Translation: IVector3D readonly dispid 1;
    property Rotation: IAngleAxis readonly dispid 2;
    property Device: IDispatch readonly dispid 3;
    property Period: Double readonly dispid 4;
  end;

// *********************************************************************//
// DispIntf:  _IKeyboardEvents
// Flags:     (4096) Dispatchable
// GUID:      {6B6BB0A8-4491-40CF-B1A9-C15A801FE151}
// *********************************************************************//
  _IKeyboardEvents = dispinterface
    ['{6B6BB0A8-4491-40CF-B1A9-C15A801FE151}']
    procedure KeyDown(keyCode: SYSINT); dispid 1;
    procedure KeyUp(keyCode: SYSINT); dispid 2;
  end;

// *********************************************************************//
// Interface: IKeyboard
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D6F968E7-2993-48D7-AF24-8B602D925B2C}
// *********************************************************************//
  IKeyboard = interface(IDispatch)
    ['{D6F968E7-2993-48D7-AF24-8B602D925B2C}']
    function Get_Keys: Integer; safecall;
    function Get_ProgrammableKeys: Integer; safecall;
    function GetKeyLabel(key: Integer): WideString; safecall;
    function GetKeyName(key: Integer): WideString; safecall;
    function Get_Device: IDispatch; safecall;
    function IsKeyDown(key: Integer): WordBool; safecall;
    function IsKeyUp(key: Integer): WordBool; safecall;
    property Keys: Integer read Get_Keys;
    property ProgrammableKeys: Integer read Get_ProgrammableKeys;
    property Device: IDispatch read Get_Device;
  end;

// *********************************************************************//
// DispIntf:  IKeyboardDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D6F968E7-2993-48D7-AF24-8B602D925B2C}
// *********************************************************************//
  IKeyboardDisp = dispinterface
    ['{D6F968E7-2993-48D7-AF24-8B602D925B2C}']
    property Keys: Integer readonly dispid 1;
    property ProgrammableKeys: Integer readonly dispid 2;
    function GetKeyLabel(key: Integer): WideString; dispid 3;
    function GetKeyName(key: Integer): WideString; dispid 4;
    property Device: IDispatch readonly dispid 5;
    function IsKeyDown(key: Integer): WordBool; dispid 6;
    function IsKeyUp(key: Integer): WordBool; dispid 7;
  end;

// *********************************************************************//
// DispIntf:  _ISimpleDeviceEvents
// Flags:     (4096) Dispatchable
// GUID:      {8FE3A216-E235-49A6-9136-F9D81FDADEF5}
// *********************************************************************//
  _ISimpleDeviceEvents = dispinterface
    ['{8FE3A216-E235-49A6-9136-F9D81FDADEF5}']
    procedure DeviceChange(reserved: Integer); dispid 1;
  end;

// *********************************************************************//
// Interface: ISimpleDevice
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CB3BF65E-0816-482A-BB11-64AF1E837812}
// *********************************************************************//
  ISimpleDevice = interface(IDispatch)
    ['{CB3BF65E-0816-482A-BB11-64AF1E837812}']
    procedure Connect; safecall;
    procedure Disconnect; safecall;
    function Get_Sensor: ISensor; safecall;
    function Get_Keyboard: IKeyboard; safecall;
    function Get_type_: Integer; safecall;
    function Get_IsConnected: WordBool; safecall;
    procedure LoadPreferences(const PreferencesName: WideString); safecall;
    property Sensor: ISensor read Get_Sensor;
    property Keyboard: IKeyboard read Get_Keyboard;
    property type_: Integer read Get_type_;
    property IsConnected: WordBool read Get_IsConnected;
  end;

// *********************************************************************//
// DispIntf:  ISimpleDeviceDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CB3BF65E-0816-482A-BB11-64AF1E837812}
// *********************************************************************//
  ISimpleDeviceDisp = dispinterface
    ['{CB3BF65E-0816-482A-BB11-64AF1E837812}']
    procedure Connect; dispid 1;
    procedure Disconnect; dispid 2;
    property Sensor: ISensor readonly dispid 3;
    property Keyboard: IKeyboard readonly dispid 4;
    property type_: Integer readonly dispid 5;
    property IsConnected: WordBool readonly dispid 6;
    procedure LoadPreferences(const PreferencesName: WideString); dispid 7;
  end;

// *********************************************************************//
// Interface: ITDxInfo
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {00612962-8FB6-47B2-BF98-4E8C0FF5F559}
// *********************************************************************//
  ITDxInfo = interface(IDispatch)
    ['{00612962-8FB6-47B2-BF98-4E8C0FF5F559}']
    function RevisionNumber: WideString; safecall;
  end;

// *********************************************************************//
// DispIntf:  ITDxInfoDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {00612962-8FB6-47B2-BF98-4E8C0FF5F559}
// *********************************************************************//
  ITDxInfoDisp = dispinterface
    ['{00612962-8FB6-47B2-BF98-4E8C0FF5F559}']
    function RevisionNumber: WideString; dispid 1;
  end;

// *********************************************************************//
// The Class CoAngleAxis provides a Create and CreateRemote method to          
// create instances of the default interface IAngleAxis exposed by              
// the CoClass AngleAxis. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoAngleAxis = class
    class function Create: IAngleAxis;
    class function CreateRemote(const MachineName: string): IAngleAxis;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TAngleAxis
// Help String      : AngleAxis Class
// Default Interface: IAngleAxis
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (1026) CanCreate Aggregatable
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TAngleAxisProperties= class;
{$ENDIF}
  TAngleAxis = class(TOleServer)
  private
    FIntf: IAngleAxis;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TAngleAxisProperties;
    function GetServerProperties: TAngleAxisProperties;
{$ENDIF}
    function GetDefaultInterface: IAngleAxis;
  protected
    procedure InitServerData; override;
    function Get_X: Double;
    procedure Set_X(pVal: Double);
    function Get_Y: Double;
    procedure Set_Y(pVal: Double);
    function Get_Z: Double;
    procedure Set_Z(pVal: Double);
    function Get_Angle: Double;
    procedure Set_Angle(pVal: Double);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IAngleAxis);
    procedure Disconnect; override;
    property DefaultInterface: IAngleAxis read GetDefaultInterface;
    property X: Double read Get_X write Set_X;
    property Y: Double read Get_Y write Set_Y;
    property Z: Double read Get_Z write Set_Z;
    property Angle: Double read Get_Angle write Set_Angle;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TAngleAxisProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TAngleAxis
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TAngleAxisProperties = class(TPersistent)
  private
    FServer:    TAngleAxis;
    function    GetDefaultInterface: IAngleAxis;
    constructor Create(AServer: TAngleAxis);
  protected
    function Get_X: Double;
    procedure Set_X(pVal: Double);
    function Get_Y: Double;
    procedure Set_Y(pVal: Double);
    function Get_Z: Double;
    procedure Set_Z(pVal: Double);
    function Get_Angle: Double;
    procedure Set_Angle(pVal: Double);
  public
    property DefaultInterface: IAngleAxis read GetDefaultInterface;
  published
    property X: Double read Get_X write Set_X;
    property Y: Double read Get_Y write Set_Y;
    property Z: Double read Get_Z write Set_Z;
    property Angle: Double read Get_Angle write Set_Angle;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoVector3D provides a Create and CreateRemote method to          
// create instances of the default interface IVector3D exposed by              
// the CoClass Vector3D. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoVector3D = class
    class function Create: IVector3D;
    class function CreateRemote(const MachineName: string): IVector3D;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TVector3D
// Help String      : Vector3D Class
// Default Interface: IVector3D
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (1026) CanCreate Aggregatable
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TVector3DProperties= class;
{$ENDIF}
  TVector3D = class(TOleServer)
  private
    FIntf: IVector3D;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TVector3DProperties;
    function GetServerProperties: TVector3DProperties;
{$ENDIF}
    function GetDefaultInterface: IVector3D;
  protected
    procedure InitServerData; override;
    function Get_X: Double;
    procedure Set_X(pVal: Double);
    function Get_Y: Double;
    procedure Set_Y(pVal: Double);
    function Get_Z: Double;
    procedure Set_Z(pVal: Double);
    function Get_Length: Double;
    procedure Set_Length(pVal: Double);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IVector3D);
    procedure Disconnect; override;
    property DefaultInterface: IVector3D read GetDefaultInterface;
    property X: Double read Get_X write Set_X;
    property Y: Double read Get_Y write Set_Y;
    property Z: Double read Get_Z write Set_Z;
    property Length: Double read Get_Length write Set_Length;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TVector3DProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TVector3D
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TVector3DProperties = class(TPersistent)
  private
    FServer:    TVector3D;
    function    GetDefaultInterface: IVector3D;
    constructor Create(AServer: TVector3D);
  protected
    function Get_X: Double;
    procedure Set_X(pVal: Double);
    function Get_Y: Double;
    procedure Set_Y(pVal: Double);
    function Get_Z: Double;
    procedure Set_Z(pVal: Double);
    function Get_Length: Double;
    procedure Set_Length(pVal: Double);
  public
    property DefaultInterface: IVector3D read GetDefaultInterface;
  published
    property X: Double read Get_X write Set_X;
    property Y: Double read Get_Y write Set_Y;
    property Z: Double read Get_Z write Set_Z;
    property Length: Double read Get_Length write Set_Length;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoSensor provides a Create and CreateRemote method to          
// create instances of the default interface ISensor exposed by              
// the CoClass Sensor. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSensor = class
    class function Create: ISensor;
    class function CreateRemote(const MachineName: string): ISensor;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TSensor
// Help String      : Sensor Class
// Default Interface: ISensor
// Def. Intf. DISP? : No
// Event   Interface: _ISensorEvents
// TypeFlags        : (1026) CanCreate Aggregatable
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TSensorProperties= class;
{$ENDIF}
  TSensor = class(TOleServer)
  private
    FOnSensorInput: TNotifyEvent;
    FIntf: ISensor;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TSensorProperties;
    function GetServerProperties: TSensorProperties;
{$ENDIF}
    function GetDefaultInterface: ISensor;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
    function Get_Translation: IVector3D;
    function Get_Rotation: IAngleAxis;
    function Get_Device: IDispatch;
    function Get_Period: Double;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ISensor);
    procedure Disconnect; override;
    property DefaultInterface: ISensor read GetDefaultInterface;
    property Translation: IVector3D read Get_Translation;
    property Rotation: IAngleAxis read Get_Rotation;
    property Device: IDispatch read Get_Device;
    property Period: Double read Get_Period;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TSensorProperties read GetServerProperties;
{$ENDIF}
    property OnSensorInput: TNotifyEvent read FOnSensorInput write FOnSensorInput;
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TSensor
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TSensorProperties = class(TPersistent)
  private
    FServer:    TSensor;
    function    GetDefaultInterface: ISensor;
    constructor Create(AServer: TSensor);
  protected
    function Get_Translation: IVector3D;
    function Get_Rotation: IAngleAxis;
    function Get_Device: IDispatch;
    function Get_Period: Double;
  public
    property DefaultInterface: ISensor read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoKeyboard provides a Create and CreateRemote method to          
// create instances of the default interface IKeyboard exposed by              
// the CoClass Keyboard. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoKeyboard = class
    class function Create: IKeyboard;
    class function CreateRemote(const MachineName: string): IKeyboard;
  end;

  TKeyboardKeyDown = procedure(ASender: TObject; keyCode: SYSINT) of object;
  TKeyboardKeyUp = procedure(ASender: TObject; keyCode: SYSINT) of object;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TKeyboard
// Help String      : Keyboard Class
// Default Interface: IKeyboard
// Def. Intf. DISP? : No
// Event   Interface: _IKeyboardEvents
// TypeFlags        : (1026) CanCreate Aggregatable
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TKeyboardProperties= class;
{$ENDIF}
  TKeyboard = class(TOleServer)
  private
    FOnKeyDown: TKeyboardKeyDown;
    FOnKeyUp: TKeyboardKeyUp;
    FIntf: IKeyboard;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TKeyboardProperties;
    function GetServerProperties: TKeyboardProperties;
{$ENDIF}
    function GetDefaultInterface: IKeyboard;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
    function Get_Keys: Integer;
    function Get_ProgrammableKeys: Integer;
    function Get_Device: IDispatch;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IKeyboard);
    procedure Disconnect; override;
    function GetKeyLabel(key: Integer): WideString;
    function GetKeyName(key: Integer): WideString;
    function IsKeyDown(key: Integer): WordBool;
    function IsKeyUp(key: Integer): WordBool;
    property DefaultInterface: IKeyboard read GetDefaultInterface;
    property Keys: Integer read Get_Keys;
    property ProgrammableKeys: Integer read Get_ProgrammableKeys;
    property Device: IDispatch read Get_Device;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TKeyboardProperties read GetServerProperties;
{$ENDIF}
    property OnKeyDown: TKeyboardKeyDown read FOnKeyDown write FOnKeyDown;
    property OnKeyUp: TKeyboardKeyUp read FOnKeyUp write FOnKeyUp;
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TKeyboard
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TKeyboardProperties = class(TPersistent)
  private
    FServer:    TKeyboard;
    function    GetDefaultInterface: IKeyboard;
    constructor Create(AServer: TKeyboard);
  protected
    function Get_Keys: Integer;
    function Get_ProgrammableKeys: Integer;
    function Get_Device: IDispatch;
  public
    property DefaultInterface: IKeyboard read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoDevice provides a Create and CreateRemote method to          
// create instances of the default interface ISimpleDevice exposed by              
// the CoClass Device. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoDevice = class
    class function Create: ISimpleDevice;
    class function CreateRemote(const MachineName: string): ISimpleDevice;
  end;

  TDeviceDeviceChange = procedure(ASender: TObject; reserved: Integer) of object;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TDevice
// Help String      : Device Class
// Default Interface: ISimpleDevice
// Def. Intf. DISP? : No
// Event   Interface: _ISimpleDeviceEvents
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TDeviceProperties= class;
{$ENDIF}
  TDevice = class(TOleServer)
  private
    FOnDeviceChange: TDeviceDeviceChange;
    FIntf: ISimpleDevice;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TDeviceProperties;
    function GetServerProperties: TDeviceProperties;
{$ENDIF}
    function GetDefaultInterface: ISimpleDevice;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
    function Get_Sensor: ISensor;
    function Get_Keyboard: IKeyboard;
    function Get_type_: Integer;
    function Get_IsConnected: WordBool;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ISimpleDevice);
    procedure Disconnect; override;
    procedure Connect1;
    procedure Disconnect1;
    procedure LoadPreferences(const PreferencesName: WideString);
    property DefaultInterface: ISimpleDevice read GetDefaultInterface;
    property Sensor: ISensor read Get_Sensor;
    property Keyboard: IKeyboard read Get_Keyboard;
    property type_: Integer read Get_type_;
    property IsConnected: WordBool read Get_IsConnected;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TDeviceProperties read GetServerProperties;
{$ENDIF}
    property OnDeviceChange: TDeviceDeviceChange read FOnDeviceChange write FOnDeviceChange;
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TDevice
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TDeviceProperties = class(TPersistent)
  private
    FServer:    TDevice;
    function    GetDefaultInterface: ISimpleDevice;
    constructor Create(AServer: TDevice);
  protected
    function Get_Sensor: ISensor;
    function Get_Keyboard: IKeyboard;
    function Get_type_: Integer;
    function Get_IsConnected: WordBool;
  public
    property DefaultInterface: ISimpleDevice read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoTDxInfo provides a Create and CreateRemote method to          
// create instances of the default interface ITDxInfo exposed by              
// the CoClass TDxInfo. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoTDxInfo = class
    class function Create: ITDxInfo;
    class function CreateRemote(const MachineName: string): ITDxInfo;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TTDxInfo
// Help String      : TDxInfo Class
// Default Interface: ITDxInfo
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TTDxInfoProperties= class;
{$ENDIF}
  TTDxInfo = class(TOleServer)
  private
    FIntf: ITDxInfo;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TTDxInfoProperties;
    function GetServerProperties: TTDxInfoProperties;
{$ENDIF}
    function GetDefaultInterface: ITDxInfo;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ITDxInfo);
    procedure Disconnect; override;
    function RevisionNumber: WideString;
    property DefaultInterface: ITDxInfo read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TTDxInfoProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TTDxInfo
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TTDxInfoProperties = class(TPersistent)
  private
    FServer:    TTDxInfo;
    function    GetDefaultInterface: ITDxInfo;
    constructor Create(AServer: TTDxInfo);
  protected
  public
    property DefaultInterface: ITDxInfo read GetDefaultInterface;
  published
  end;
{$ENDIF}

// CGE: Do not register any components
//procedure Register;

resourcestring
  dtlServerPage = 'ActiveX';

  dtlOcxPage = 'ActiveX';

implementation

uses ComObj;

class function CoAngleAxis.Create: IAngleAxis;
begin
  Result := CreateComObject(CLASS_AngleAxis) as IAngleAxis;
end;

class function CoAngleAxis.CreateRemote(const MachineName: string): IAngleAxis;
begin
  Result := CreateRemoteComObject(UTF8Decode(MachineName), CLASS_AngleAxis) as IAngleAxis;
end;

procedure TAngleAxis.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{512A6C3E-3010-401B-8623-E413E2ACC138}';
    IntfIID:   '{1EF2BAFF-54E9-4706-9F61-078F7134FD35}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500;
    InstanceCount: 0);
begin
  ServerData := @CServerData;
end;

procedure TAngleAxis.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IAngleAxis;
  end;
end;

procedure TAngleAxis.ConnectTo(svrIntf: IAngleAxis);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TAngleAxis.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TAngleAxis.GetDefaultInterface: IAngleAxis;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TAngleAxis.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TAngleAxisProperties.Create(Self);
{$ENDIF}
end;

destructor TAngleAxis.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TAngleAxis.GetServerProperties: TAngleAxisProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TAngleAxis.Get_X: Double;
begin
    Result := DefaultInterface.X;
end;

procedure TAngleAxis.Set_X(pVal: Double);
begin
  DefaultInterface.Set_X(pVal);
end;

function TAngleAxis.Get_Y: Double;
begin
    Result := DefaultInterface.Y;
end;

procedure TAngleAxis.Set_Y(pVal: Double);
begin
  DefaultInterface.Set_Y(pVal);
end;

function TAngleAxis.Get_Z: Double;
begin
    Result := DefaultInterface.Z;
end;

procedure TAngleAxis.Set_Z(pVal: Double);
begin
  DefaultInterface.Set_Z(pVal);
end;

function TAngleAxis.Get_Angle: Double;
begin
    Result := DefaultInterface.Angle;
end;

procedure TAngleAxis.Set_Angle(pVal: Double);
begin
  DefaultInterface.Set_Angle(pVal);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TAngleAxisProperties.Create(AServer: TAngleAxis);
begin
  inherited Create;
  FServer := AServer;
end;

function TAngleAxisProperties.GetDefaultInterface: IAngleAxis;
begin
  Result := FServer.DefaultInterface;
end;

function TAngleAxisProperties.Get_X: Double;
begin
    Result := DefaultInterface.X;
end;

procedure TAngleAxisProperties.Set_X(pVal: Double);
begin
  DefaultInterface.Set_X(pVal);
end;

function TAngleAxisProperties.Get_Y: Double;
begin
    Result := DefaultInterface.Y;
end;

procedure TAngleAxisProperties.Set_Y(pVal: Double);
begin
  DefaultInterface.Set_Y(pVal);
end;

function TAngleAxisProperties.Get_Z: Double;
begin
    Result := DefaultInterface.Z;
end;

procedure TAngleAxisProperties.Set_Z(pVal: Double);
begin
  DefaultInterface.Set_Z(pVal);
end;

function TAngleAxisProperties.Get_Angle: Double;
begin
    Result := DefaultInterface.Angle;
end;

procedure TAngleAxisProperties.Set_Angle(pVal: Double);
begin
  DefaultInterface.Set_Angle(pVal);
end;

{$ENDIF}

class function CoVector3D.Create: IVector3D;
begin
  Result := CreateComObject(CLASS_Vector3D) as IVector3D;
end;

class function CoVector3D.CreateRemote(const MachineName: string): IVector3D;
begin
  Result := CreateRemoteComObject(UTF8Decode(MachineName), CLASS_Vector3D) as IVector3D;
end;

procedure TVector3D.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{740A7479-C7C1-44DA-8A84-B5DE63C78B32}';
    IntfIID:   '{8C2AA71D-2B23-43F5-A6ED-4DF57E9CD8D5}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500;
    InstanceCount: 0);
begin
  ServerData := @CServerData;
end;

procedure TVector3D.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IVector3D;
  end;
end;

procedure TVector3D.ConnectTo(svrIntf: IVector3D);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TVector3D.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TVector3D.GetDefaultInterface: IVector3D;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TVector3D.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TVector3DProperties.Create(Self);
{$ENDIF}
end;

destructor TVector3D.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TVector3D.GetServerProperties: TVector3DProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TVector3D.Get_X: Double;
begin
    Result := DefaultInterface.X;
end;

procedure TVector3D.Set_X(pVal: Double);
begin
  DefaultInterface.Set_X(pVal);
end;

function TVector3D.Get_Y: Double;
begin
    Result := DefaultInterface.Y;
end;

procedure TVector3D.Set_Y(pVal: Double);
begin
  DefaultInterface.Set_Y(pVal);
end;

function TVector3D.Get_Z: Double;
begin
    Result := DefaultInterface.Z;
end;

procedure TVector3D.Set_Z(pVal: Double);
begin
  DefaultInterface.Set_Z(pVal);
end;

function TVector3D.Get_Length: Double;
begin
    Result := DefaultInterface.Length;
end;

procedure TVector3D.Set_Length(pVal: Double);
begin
  DefaultInterface.Set_Length(pVal);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TVector3DProperties.Create(AServer: TVector3D);
begin
  inherited Create;
  FServer := AServer;
end;

function TVector3DProperties.GetDefaultInterface: IVector3D;
begin
  Result := FServer.DefaultInterface;
end;

function TVector3DProperties.Get_X: Double;
begin
    Result := DefaultInterface.X;
end;

procedure TVector3DProperties.Set_X(pVal: Double);
begin
  DefaultInterface.Set_X(pVal);
end;

function TVector3DProperties.Get_Y: Double;
begin
    Result := DefaultInterface.Y;
end;

procedure TVector3DProperties.Set_Y(pVal: Double);
begin
  DefaultInterface.Set_Y(pVal);
end;

function TVector3DProperties.Get_Z: Double;
begin
    Result := DefaultInterface.Z;
end;

procedure TVector3DProperties.Set_Z(pVal: Double);
begin
  DefaultInterface.Set_Z(pVal);
end;

function TVector3DProperties.Get_Length: Double;
begin
    Result := DefaultInterface.Length;
end;

procedure TVector3DProperties.Set_Length(pVal: Double);
begin
  DefaultInterface.Set_Length(pVal);
end;

{$ENDIF}

class function CoSensor.Create: ISensor;
begin
  Result := CreateComObject(CLASS_Sensor) as ISensor;
end;

class function CoSensor.CreateRemote(const MachineName: string): ISensor;
begin
  Result := CreateRemoteComObject(UTF8Decode(MachineName), CLASS_Sensor) as ISensor;
end;

procedure TSensor.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{85004B00-1AA7-4777-B1CE-8427301B942D}';
    IntfIID:   '{F3A6775E-6FA1-4829-BF32-5B045C29078F}';
    EventIID:  '{E6929A4A-6F41-46C6-9252-A8CC53472CB1}';
    LicenseKey: nil;
    Version: 500;
    InstanceCount: 0);
begin
  ServerData := @CServerData;
end;

procedure TSensor.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk as ISensor;
  end;
end;

procedure TSensor.ConnectTo(svrIntf: ISensor);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

procedure TSensor.DisConnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TSensor.GetDefaultInterface: ISensor;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TSensor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TSensorProperties.Create(Self);
{$ENDIF}
end;

destructor TSensor.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TSensor.GetServerProperties: TSensorProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TSensor.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
    1: if Assigned(FOnSensorInput) then
         FOnSensorInput(Self);
  end; {case DispID}
end;

function TSensor.Get_Translation: IVector3D;
begin
    Result := DefaultInterface.Translation;
end;

function TSensor.Get_Rotation: IAngleAxis;
begin
    Result := DefaultInterface.Rotation;
end;

function TSensor.Get_Device: IDispatch;
begin
    Result := DefaultInterface.Device;
end;

function TSensor.Get_Period: Double;
begin
    Result := DefaultInterface.Period;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TSensorProperties.Create(AServer: TSensor);
begin
  inherited Create;
  FServer := AServer;
end;

function TSensorProperties.GetDefaultInterface: ISensor;
begin
  Result := FServer.DefaultInterface;
end;

function TSensorProperties.Get_Translation: IVector3D;
begin
    Result := DefaultInterface.Translation;
end;

function TSensorProperties.Get_Rotation: IAngleAxis;
begin
    Result := DefaultInterface.Rotation;
end;

function TSensorProperties.Get_Device: IDispatch;
begin
    Result := DefaultInterface.Device;
end;

function TSensorProperties.Get_Period: Double;
begin
    Result := DefaultInterface.Period;
end;

{$ENDIF}

class function CoKeyboard.Create: IKeyboard;
begin
  Result := CreateComObject(CLASS_Keyboard) as IKeyboard;
end;

class function CoKeyboard.CreateRemote(const MachineName: string): IKeyboard;
begin
  Result := CreateRemoteComObject(UTF8Decode(MachineName), CLASS_Keyboard) as IKeyboard;
end;

procedure TKeyboard.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{25BBE090-583A-4903-A61B-D0EC629AC4EC}';
    IntfIID:   '{D6F968E7-2993-48D7-AF24-8B602D925B2C}';
    EventIID:  '{6B6BB0A8-4491-40CF-B1A9-C15A801FE151}';
    LicenseKey: nil;
    Version: 500;
    InstanceCount: 0);
begin
  ServerData := @CServerData;
end;

procedure TKeyboard.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk as IKeyboard;
  end;
end;

procedure TKeyboard.ConnectTo(svrIntf: IKeyboard);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

procedure TKeyboard.DisConnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TKeyboard.GetDefaultInterface: IKeyboard;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TKeyboard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TKeyboardProperties.Create(Self);
{$ENDIF}
end;

destructor TKeyboard.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TKeyboard.GetServerProperties: TKeyboardProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TKeyboard.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
    1: if Assigned(FOnKeyDown) then
         FOnKeyDown(Self, Params[0] {SYSINT});
    2: if Assigned(FOnKeyUp) then
         FOnKeyUp(Self, Params[0] {SYSINT});
  end; {case DispID}
end;

function TKeyboard.Get_Keys: Integer;
begin
    Result := DefaultInterface.Keys;
end;

function TKeyboard.Get_ProgrammableKeys: Integer;
begin
    Result := DefaultInterface.ProgrammableKeys;
end;

function TKeyboard.Get_Device: IDispatch;
begin
    Result := DefaultInterface.Device;
end;

function TKeyboard.GetKeyLabel(key: Integer): WideString;
begin
  Result := DefaultInterface.GetKeyLabel(key);
end;

function TKeyboard.GetKeyName(key: Integer): WideString;
begin
  Result := DefaultInterface.GetKeyName(key);
end;

function TKeyboard.IsKeyDown(key: Integer): WordBool;
begin
  Result := DefaultInterface.IsKeyDown(key);
end;

function TKeyboard.IsKeyUp(key: Integer): WordBool;
begin
  Result := DefaultInterface.IsKeyUp(key);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TKeyboardProperties.Create(AServer: TKeyboard);
begin
  inherited Create;
  FServer := AServer;
end;

function TKeyboardProperties.GetDefaultInterface: IKeyboard;
begin
  Result := FServer.DefaultInterface;
end;

function TKeyboardProperties.Get_Keys: Integer;
begin
    Result := DefaultInterface.Keys;
end;

function TKeyboardProperties.Get_ProgrammableKeys: Integer;
begin
    Result := DefaultInterface.ProgrammableKeys;
end;

function TKeyboardProperties.Get_Device: IDispatch;
begin
    Result := DefaultInterface.Device;
end;

{$ENDIF}

class function CoDevice.Create: ISimpleDevice;
begin
  Result := CreateComObject(CLASS_Device) as ISimpleDevice;
end;

class function CoDevice.CreateRemote(const MachineName: string): ISimpleDevice;
begin
  Result := CreateRemoteComObject(UTF8Decode(MachineName), CLASS_Device) as ISimpleDevice;
end;

procedure TDevice.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{82C5AB54-C92C-4D52-AAC5-27E25E22604C}';
    IntfIID:   '{CB3BF65E-0816-482A-BB11-64AF1E837812}';
    EventIID:  '{8FE3A216-E235-49A6-9136-F9D81FDADEF5}';
    LicenseKey: nil;
    Version: 500;
    InstanceCount: 0);
begin
  ServerData := @CServerData;
end;

procedure TDevice.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk as ISimpleDevice;
  end;
end;

procedure TDevice.ConnectTo(svrIntf: ISimpleDevice);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

procedure TDevice.DisConnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TDevice.GetDefaultInterface: ISimpleDevice;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TDevice.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TDeviceProperties.Create(Self);
{$ENDIF}
end;

destructor TDevice.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TDevice.GetServerProperties: TDeviceProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TDevice.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
    1: if Assigned(FOnDeviceChange) then
         FOnDeviceChange(Self, Params[0] {Integer});
  end; {case DispID}
end;

function TDevice.Get_Sensor: ISensor;
begin
    Result := DefaultInterface.Sensor;
end;

function TDevice.Get_Keyboard: IKeyboard;
begin
    Result := DefaultInterface.Keyboard;
end;

function TDevice.Get_type_: Integer;
begin
    Result := DefaultInterface.type_;
end;

function TDevice.Get_IsConnected: WordBool;
begin
    Result := DefaultInterface.IsConnected;
end;

procedure TDevice.Connect1;
begin
  DefaultInterface.Connect;
end;

procedure TDevice.Disconnect1;
begin
  DefaultInterface.Disconnect;
end;

procedure TDevice.LoadPreferences(const PreferencesName: WideString);
begin
  DefaultInterface.LoadPreferences(PreferencesName);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TDeviceProperties.Create(AServer: TDevice);
begin
  inherited Create;
  FServer := AServer;
end;

function TDeviceProperties.GetDefaultInterface: ISimpleDevice;
begin
  Result := FServer.DefaultInterface;
end;

function TDeviceProperties.Get_Sensor: ISensor;
begin
    Result := DefaultInterface.Sensor;
end;

function TDeviceProperties.Get_Keyboard: IKeyboard;
begin
    Result := DefaultInterface.Keyboard;
end;

function TDeviceProperties.Get_type_: Integer;
begin
    Result := DefaultInterface.type_;
end;

function TDeviceProperties.Get_IsConnected: WordBool;
begin
    Result := DefaultInterface.IsConnected;
end;

{$ENDIF}

class function CoTDxInfo.Create: ITDxInfo;
begin
  Result := CreateComObject(CLASS_TDxInfo) as ITDxInfo;
end;

class function CoTDxInfo.CreateRemote(const MachineName: string): ITDxInfo;
begin
  Result := CreateRemoteComObject(UTF8Decode(MachineName), CLASS_TDxInfo) as ITDxInfo;
end;

procedure TTDxInfo.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{1A960ECE-0E57-4A68-B694-8373114F1FF4}';
    IntfIID:   '{00612962-8FB6-47B2-BF98-4E8C0FF5F559}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500;
    InstanceCount: 0);
begin
  ServerData := @CServerData;
end;

procedure TTDxInfo.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as ITDxInfo;
  end;
end;

procedure TTDxInfo.ConnectTo(svrIntf: ITDxInfo);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TTDxInfo.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TTDxInfo.GetDefaultInterface: ITDxInfo;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TTDxInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TTDxInfoProperties.Create(Self);
{$ENDIF}
end;

destructor TTDxInfo.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TTDxInfo.GetServerProperties: TTDxInfoProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TTDxInfo.RevisionNumber: WideString;
begin
  Result := DefaultInterface.RevisionNumber;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TTDxInfoProperties.Create(AServer: TTDxInfo);
begin
  inherited Create;
  FServer := AServer;
end;

function TTDxInfoProperties.GetDefaultInterface: ITDxInfo;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

(*
procedure Register;
begin
  RegisterComponents(dtlServerPage, [TAngleAxis, TVector3D, TSensor, TKeyboard, 
    TDevice, TTDxInfo]);
end;
*)

end.
