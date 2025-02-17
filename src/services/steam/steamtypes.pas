unit SteamTypes;

interface
{$I castleconf.inc}

{$ifdef FPC}
  {$packrecords C}
{$else}
  {$ALIGN 4}
{$endif}

uses CTypes, SysUtils;

{ STEAM_API_VERSION allows switching between an established tested API
  and a newer API that is being tested. It makes upgrading between two
  APIs far simpler as when defined an exception may be raised when
  loading the library. The 'testing' library should be renamed to
  include it's version as a suffix e.g. steam_api64.dll would be
  renamed to steam_api64_161.dll and STEAMLIBVER, below, set to the matching
  suffix.

  After successful testing the constants below should all be the same
  until a new API upgrade is required - essentially making the define = !define
  and STEAMLIBVER should be an empty string ('')
}

type
  HSteamPipe = Int32;
  HSteamUser = Int32;
  { It's a struct in C headers but can be passed as UInt64,
    defined in C headers with explicit statement that it's 64-bit sized. }
  CSteamId = UInt64;
  { It's a struct in C headers but can be passed as UInt64,
    defined in C headers with explicit ability to be typecasted as 64-bit int. }
  CGameID = UInt64;
  CUserID = UInt64;
  EResult = UInt32;
  TAppId = UInt32;
  PSteamErrMsg = PChar;

  { Internal structure used in manual callback dispatch
    (CallbackMsg_t in C headers). }
  TCallbackMsg = record
    // Specific user to whom this callback applies.
    m_hSteamUser: HSteamUser;
    // Callback identifier.  (Corresponds to the k_iCallback enum in the callback structure.)
    m_iCallback: UInt32;
    // Points to the callback structure
    // (C headers literal translation would be Puint8, but it's pointless,
    // this is not pointer to UInt8, this is a pointer to callback-specific blob).
    m_pubParam: Pointer;
    // Size of the data pointed to by m_pubParam
    m_cubParam: UInt32;
  end;
  PCallbackMsg = ^TCallbackMsg;

  TIconSize = (IconSmall, IconMedium, IconLarge);

const
  STEAM_API_VERSION = 1.57;
  STEAM_INPUT_MAX_COUNT = 16;

  { Versions of Steam API interfaces.
    Correspond to Steamworks 1.xx controlled by API_XXX with fallback to 1.57 version. }
{$if STEAM_API_VERSION = 1.61}
  STEAMCLIENT_INTERFACE_VERSION = 'SteamClient021'; //< isteamclient.h
  STEAMUSER_INTERFACE_VERSION = 'SteamUser023'; //< isteamuser.h
  STEAMUSERSTATS_INTERFACE_VERSION = 'STEAMUSERSTATS_INTERFACE_VERSION013'; //< isteamuserstats.h
  STEAMFRIENDS_INTERFACE_VERSION = 'SteamFriends017'; //< isteamuserstats.h
  STEAMUTILS_INTERFACE_VERSION = 'SteamUtils010'; //< isteamuser.h
  STEAMINPUT_INTERFACE_VERSION = 'SteamInput006'; //< isteaminput.h
  STEAMAPPS_INTERFACE_VERSION = 'STEAMAPPS_INTERFACE_VERSION008'; //< isteaminput.h
  VersionSteamUtils = '010'; //< matches STEAMUTILS_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  VersionSteamApps = '008'; //< matches STEAMAPPS_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  VersionSteamUser = '023'; //< matches STEAMUSER_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  VersionSteamFriends = '017'; //< matches STEAMFRIENDS_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  VersionSteamInput = '006'; //< matches STEAMINPUT_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  STEAMLIBVER = '_161';
{$elseif STEAM_API_VERSION = 1.57}
  STEAMCLIENT_INTERFACE_VERSION = 'SteamClient020'; //< isteamclient.h
  STEAMUSER_INTERFACE_VERSION = 'SteamUser023'; //< isteamuser.h
  STEAMUSERSTATS_INTERFACE_VERSION = 'STEAMUSERSTATS_INTERFACE_VERSION012'; //< isteamuserstats.h
  STEAMFRIENDS_INTERFACE_VERSION = 'SteamFriends017'; //< isteamuserstats.h
  STEAMAPPS_INTERFACE_VERSION = 'STEAMAPPS_INTERFACE_VERSION008'; //< isteaminput.h
  STEAMUTILS_INTERFACE_VERSION = 'SteamUtils010'; //< isteamuser.h
  STEAMINPUT_INTERFACE_VERSION = 'SteamInput006'; //< isteaminput.h
  VersionSteamUtils = '010'; //< matches STEAMUTILS_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  VersionSteamApps = '008'; //< matches STEAMAPPS_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  VersionSteamUser = '023'; //< matches STEAMUSER_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  VersionSteamFriends = '017'; //< matches STEAMAPPS_INTERFACE_VERSION *and* accessor in steam_api_flat.h
  VersionSteamInput = '006'; //< matches accessor in steam_api_flat.h
  STEAMLIBVER = '';
{$endif}

const
  k_cchStatNameMax = 128;

type
  ISteamInput = record Ptr: Pointer; end;
  // Input Types
  ESteamInputActionEventType = (
    ESteamInputActionEventType_DigitalAction,
    ESteamInputActionEventType_AnalogAction
    );

  EInputSourceMode = (
    k_EInputSourceMode_None,
    k_EInputSourceMode_Dpad,
    k_EInputSourceMode_Buttons,
    k_EInputSourceMode_FourButtons,
    k_EInputSourceMode_AbsoluteMouse,
    k_EInputSourceMode_RelativeMouse,
    k_EInputSourceMode_JoystickMove,
    k_EInputSourceMode_JoystickMouse,
    k_EInputSourceMode_JoystickCamera,
    k_EInputSourceMode_ScrollWheel,
    k_EInputSourceMode_Trigger,
    k_EInputSourceMode_TouchMenu,
    k_EInputSourceMode_MouseJoystick,
    k_EInputSourceMode_MouseRegion,
    k_EInputSourceMode_RadialMenu,
    k_EInputSourceMode_SingleButton,
    k_EInputSourceMode_Switches
  );

  TSteamInputActionSetHandle = UInt64;
  TInputAnalogActionHandle = UInt64;
  TInputDigitalActionHandle = UInt64;
  PSteamInputActionSetHandle = ^TSteamInputActionSetHandle;
  { These handles are used to refer to a specific in-game action or action set
    All action handles should be queried during initialization for performance reasons }
  TSteamInputAnalogActionHandle  =	UInt64;
  PSteamInputAnalogActionHandle = ^TSteamInputAnalogActionHandle;
  { A handle to an analog action. This can be obtained from ISteamInput::GetAnalogActionHandle. }
  TSteamInputDigitalActionHandle =	UInt64;
  PSteamInputDigitalActionHandle = ^TSteamInputDigitalActionHandle;
  { A handle to a digital action. This can be obtained from ISteamInput::GetDigitalActionHandle. }
  TInputHandle = UInt64;
  PSteamInputHandle = ^TInputHandle;
  {	This handle will consistently identify a controller, even if it is disconnected and re-connected }

  TInputAnalogActionData = record
    // Type of data coming from this action, this will match what got specified in the action set
	  eMode: EInputSourceMode;
    // The current state of this action; will be delta updates for mouse actions
     x, y: Single;
    // Whether or not this action is currently available to be bound in the active action set
    bActive: LongBool;
  end;

  TInputDigitalActionData = record
    // The current state of this action; will be true if currently pressed
    bState: LongBool;
    // Whether or not this action is currently available to be bound in the active action set
    bActive: LongBool;
  end;

  TAnalogAction = record
    actionHandle: TInputAnalogActionHandle;
    analogActionData: TInputAnalogActionData;
  end;

  TDigitalAction = record
		actionHandle: TInputDigitalActionHandle;
		digitalActionData: TInputDigitalActionData;
  end;

  TSteamInputActionEvent = record
    controllerHandle: TInputHandle;
    eEventType: ESteamInputActionEventType;
    case Integer of
      0: (analogAction: TAnalogAction);
      1: (digitalAction: TDigitalAction);
  end;
  PSteamInputActionEvent = ^TSteamInputActionEvent;

  TStatName = Array [0..(k_cchStatNameMax - 1)] of AnsiChar;
  PStatName = ^TStatName;

  TActionCallback = Procedure(P: PSteamInputActionEvent) of object;

  ESteamInputType = (
    k_ESteamInputType_Unknown,              //  0
    k_ESteamInputType_SteamController,      //  1
    k_ESteamInputType_XBox360Controller,    //  2
    k_ESteamInputType_XBoxOneController,    //  3
    k_ESteamInputType_GenericGamepad,		    //  4 = DirectInput controllers
    k_ESteamInputType_PS4Controller,        //  5
    k_ESteamInputType_AppleMFiController,	  //  6 = Unused
    k_ESteamInputType_AndroidController,	  //  7 = Unused
    k_ESteamInputType_SwitchJoyConPair,		  //  8 = Unused
    k_ESteamInputType_SwitchJoyConSingle,	  //  9 = Unused
    k_ESteamInputType_SwitchProController,  // 10
    k_ESteamInputType_MobileTouch,			    // 11 = Steam Link App On-screen Virtual Controller
    k_ESteamInputType_PS3Controller,		    // 12 = Currently uses PS4 Origins
    k_ESteamInputType_PS5Controller,		    // 13 = Added in SDK 151
    k_ESteamInputType_SteamDeckController,  // 14 = Added in SDK 153
    k_ESteamInputType_Count,                // 15
    k_ESteamInputType_MaximumPossibleValue = 255
  );

  // SteamUserStats Callbacks

  // Purpose: called when the latests stats and achievements have been received
  // from the server
  TUserAchievementIconFetched = packed record
  const
    k_iCallback = 1109;
  var
    // The Game ID this achievement is for.
    m_nGameID: CGameID;
    // The name of the achievement that this callback is for.
    m_rgchAchievementName: TStatName;
    // Returns whether the icon for the achieved (true) or unachieved (false) version.
    m_bAchieved: LongBool;
    // Handle to the image, which can be used with ISteamUtils::GetImageRGBA to get the image data.
    // 0 means no image is set for the achievement.
    m_nIconHandle: Int32;
  end;
  PUserAchievementIconFetched = ^TUserAchievementIconFetched;

  // Purpose: called when the latests stats and achievements have been received
  // from the server
  TUserStatsReceived = record
  const
    k_iCallback = 1101;
  var
    // Game these stats are for
    GameID: CGameID;
    // Success / error fetching the stats
    Result: EResult;
    // The user for whom the stats are retrieved for
    SteamID: CSteamId;
  end;
  PUserStatsReceived = ^TUserStatsReceived;

  // SteamInput Callbacks
  TSteamInputDeviceConnected = record
  const
    k_iCallback = 2801;
  var
    m_ulConnectedDeviceHandle: TInputHandle;
  end;
  PSteamInputDeviceConnected = ^TSteamInputDeviceConnected;

  TSteamInputDeviceDisconnected = record
  const
    k_iCallback = 2802;
  var
    m_ulDisconnectedDeviceHandle: TInputHandle;
  end;
  PSteamInputDeviceDisconnected = ^TSteamInputDeviceDisconnected;

  TSteamInputConfigurationLoaded = record
  const
    k_iCallback = 2803;
  var
    m_unAppID: TAppId;
    m_ulDeviceHandle: TInputHandle;
    m_ulMappingCreator: CSteamID;
    m_unMajorRevision: UInt32;
    m_unMinorRevision: UInt32;
    m_bUsesSteamInputAPI: LongBool;
    m_bUsesGamepadAPI: LongBool;
  end;
  PSteamInputConfigurationLoaded = ^TSteamInputConfigurationLoaded;

  TSteamInputGamepadSlotChange = record
  const
    k_iCallback = 2804;
  var
    m_unAppID: TAppId;
    m_ulDeviceHandle: TInputHandle;
    m_eDeviceType: ESteamInputType;
    m_nOldGamepadSlot: Int32;
    m_nNewGamepadSlot: Int32;
  end;
  PSteamInputGamepadSlotChange = ^TSteamInputGamepadSlotChange;

  { TSteamBitmap is a non-standard and hungry 24 bit RGBA data format used by Steam

    It is necessary to convert this format to something your Application can use.

    Ideally these bitmaps need caching (TBD)
  }

  TSteamBitmap = class
  strict private
    FIsValid: Boolean;
    FWidth: Integer;
    FHeight: Integer;
    FBPP: Integer;
    FImage: Pointer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetImageFormat(const ImWidth, ImHeight, BytesPerPixel: Integer);
    function GetImageMemorySize(): UInt64;
    procedure SetSteamImage(const AImage: Pointer);
    property IsValid: Boolean read FIsValid write FIsValid;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property BPP: Integer read FBPP;
    property Image: Pointer read FImage;
  end;


implementation

{ TSteamBitmap }

constructor TSteamBitmap.Create;
begin

end;

destructor TSteamBitmap.Destroy;
begin
  if Assigned(FImage) then
    FreeMem(Fimage);
  inherited;
end;

function TSteamBitmap.GetImageMemorySize: UInt64;
begin
  Result := FWidth * FHeight * FBPP;
end;

procedure TSteamBitmap.SetSteamImage(const AImage: Pointer);
begin
  FImage := AImage;
end;

procedure TSteamBitmap.SetImageFormat(const ImWidth, ImHeight, BytesPerPixel: Integer);
begin
  FWidth := ImWidth;
  FHeight := ImHeight;
  FBPP := BytesPerPixel;
end;


end.
