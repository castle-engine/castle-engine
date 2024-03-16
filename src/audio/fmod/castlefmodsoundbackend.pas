{
  Copyright 2019-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Sound engine backend using FMOD.
  See https://castle-engine.io/fmod
  about using FMOD with CGE.
}
unit CastleFMODSoundBackend;

{$I castleconf.inc}

interface

{ Use this to set sound engine backend to FMOD.
  You can call this at any point of your application.
  If you plan to use FMOD for your entire application,
  then it is beneficial to call this before any sound loading/playing,
  as then the default sound backend (OpenAL on most platforms)
  wil not even be initialized.

  This does nothing (and shows a warning) if the dynamic FMOD library
  could not be found. Therefore, applications on platforms where FMOD
  is dynamic (all platforms except iOS and Nintendo Switch now)
  gracefully fallback from FMOD to the default backend, if FMOD library cannot be found. }
procedure UseFMODSoundBackend;

implementation

uses SysUtils, Classes, Math, StrUtils, CTypes,
  CastleVectors, CastleTimeUtils, CastleLog, CastleUtils, CastleUriUtils,
  CastleClassUtils, CastleStringUtils, CastleInternalSoundFile,
  CastleInternalAbstractSoundBackend, CastleSoundBase, CastleSoundEngine,
  {$ifdef ANDROID} JNI, CastleAndroidNativeAppGlue, CastleAndroidInternalAssetStream, {$endif}
  CastleInternalFMOD;

{ sound backend classes interface -------------------------------------------- }

type
  TFMODSoundBufferBackend = class(TSoundBufferBackend)
  strict private
    FDuration: TFloatTime;
    FDataFormat: TSoundDataFormat;
    FFrequency: Cardinal;
  private
    FSoundLoading: TSoundLoading;
    FMODSound: PFMOD_SOUND;
    function FMODSystem: PFMOD_SYSTEM;
  public
    constructor Create(const ASoundEngine: TSoundEngineBackend;
      const ASoundLoading: TSoundLoading);
    procedure ContextOpen(const AUrl: String); override;
    procedure ContextClose; override;
    function Duration: TFloatTime; override;
    function DataFormat: TSoundDataFormat; override;
    function Frequency: Cardinal; override;
  end;

  TFMODSoundSourceBackend = class(TSoundSourceBackend)
  strict private
    FMODChannel: PFMOD_CHANNEL;
    FBuffer: TFMODSoundBufferBackend;
    FPosition, FVelocity: TVector3;
    FLoop, FSpatial: Boolean;
    FReferenceDistance, FMaxDistance: Single;
    function FMODSystem: PFMOD_SYSTEM;
    function Mode: TFMOD_MODE;
  public
    procedure ContextOpen; override;
    procedure ContextClose; override;
    function PlayingOrPaused: boolean; override;
    procedure Play(const BufferChangedRecently: Boolean); override;
    procedure Stop; override;
    procedure SetPosition(const Value: TVector3); override;
    procedure SetVelocity(const Value: TVector3); override;
    procedure SetLoop(const Value: boolean); override;
    procedure SetSpatial(const Value: boolean); override;
    procedure SetVolume(const Value: Single); override;
    procedure SetMinGain(const Value: Single); override;
    procedure SetMaxGain(const Value: Single); override;
    procedure SetBuffer(const Value: TSoundBufferBackend); override;
    procedure SetPitch(const Value: Single); override;
    procedure SetReferenceDistance(const Value: Single); override;
    procedure SetMaxDistance(const Value: Single); override;
    procedure SetPriority(const Value: Single); override;
    function GetOffset: Single; override;
    procedure SetOffset(const Value: Single); override;
  end;

  TFMODSoundEngineBackend = class(TSoundEngineBackend)
  private
    FMODSystem: PFMOD_SYSTEM;
    FDistanceModel: TSoundDistanceModel;
  public
    function ContextOpen(const ADevice: String; out Information, InformationSummary: String): Boolean; override;
    procedure ContextClose; override;
    function CreateBuffer(const SoundLoading: TSoundLoading): TSoundBufferBackend; override;
    function CreateSource: TSoundSourceBackend; override;

    procedure Update; override;
    procedure SetVolume(const Value: Single); override;
    { TODO: This just sets default distance model for all newly created sounds.
      So it will only work if executed early. }
    procedure SetDistanceModel(const Value: TSoundDistanceModel); override;
    procedure SetDopplerFactor(const Value: Single); override;
    procedure SetListener(const Position, Direction, Up: TVector3); override;
  end;

{ private unit helpers ------------------------------------------------------- }

type
  EFMODError = class(Exception);

procedure CheckFMOD(const FMODResult: TFMOD_RESULT; const When: String = ''; const Warning: Boolean = false);
var
  ErrorStr: String;
begin
  if FMODResult <> FMOD_OK then
  begin
    // FPC error "No type info available for this type", because it's an enum with assignments
    //ErrorStr := GetEnumName(TypeInfo(TFMOD_RESULT), Ord(FMODResult));
    System.WriteStr(ErrorStr, FMODResult);
    ErrorStr := 'FMOD error ' + ErrorStr;
    if When <> '' then
      ErrorStr := ErrorStr + ' when doing ' + When;
    if Warning then
      WritelnWarning(ErrorStr)
    else
      raise EFMODError.Create(ErrorStr);
  end;
end;

function SoundTypeToStr(const SoundType: TFMOD_SOUND_TYPE): String;
begin
  // FPC error "No type info available for this type", because it's an enum with assignments
  //Result := GetEnumName(TypeInfo(TFMOD_SOUND_TYPE), Ord(SoundType));
  System.WriteStr(Result, SoundType);
end;

function SoundFormatToStr(const SoundFormat: TFMOD_SOUND_FORMAT): String;
begin
  // FPC error "No type info available for this type", because it's an enum with assignments
  //Result := GetEnumName(TypeInfo(TFMOD_SOUND_FORMAT), Ord(SoundFormat));
  System.WriteStr(Result, SoundFormat);
end;

{ TFMODSoundBufferBackend -------------------------------------------------- }

constructor TFMODSoundBufferBackend.Create(
  const ASoundEngine: TSoundEngineBackend; const ASoundLoading: TSoundLoading);
begin
  inherited Create(ASoundEngine);
  FSoundLoading := ASoundLoading;
end;

function TFMODSoundBufferBackend.FMODSystem: PFMOD_SYSTEM;
begin
  Result := (SoundEngine as TFMODSoundEngineBackend).FMODSystem;
end;

procedure TFMODSoundBufferBackend.ContextOpen(const AUrl: String);
var
  TimeStart: TCastleProfilerTime;

  procedure CalculateProperties;
  var
    SoundType: TFMOD_SOUND_TYPE;
    SoundFormat: TFMOD_SOUND_FORMAT;
    SoundChannels, SoundBits: CInt;
    Miliseconds, PcmSamples: CUInt;
  begin
    // calculate FDuration
    CheckFMOD(FMOD_Sound_GetLength(FMODSound, @Miliseconds, FMOD_TIMEUNIT_MS));
    if Miliseconds = $FFFFFFFF then
      FDuration := 0
    else
      FDuration := Miliseconds / 1000;
    if FDuration = 0 then
      WritelnWarning('Cannot determine correct duration of sound file "%s"', [UriDisplay(AUrl)]);

    // calculate FFrequency.
    CheckFMOD(FMOD_Sound_GetLength(FMODSound, @PcmSamples, FMOD_TIMEUNIT_PCM));
    // We know that PcmSamples = Miliseconds * Frequency / 1000.
    FFrequency := Int64(PcmSamples) * 1000 div Miliseconds;

    // calculate FDataFormat
    CheckFMOD(FMOD_Sound_GetFormat(FMODSound, @SoundType, @SoundFormat, @SoundChannels, @SoundBits));
    if SoundChannels >= 2 then
    begin
      if SoundBits >= 16 then
        FDataFormat := sfStereo16
      else
        FDataFormat := sfStereo8;
    end else
    begin
      if SoundBits >= 16 then
        FDataFormat := sfMono16
      else
        FDataFormat := sfMono8;
    end;

    if LogSoundLoading then
      WritelnLog('FMOD loaded "%s": type %s, format: %s, channels: %d, bits: %d (%s), frequency: %d, duration: %f', [
        UriDisplay(AUrl),
        SoundTypeToStr(SoundType),
        SoundFormatToStr(SoundFormat),
        SoundChannels,
        SoundBits,
        DataFormatToStr(FDataFormat),
        FFrequency,
        FDuration
      ]);
  end;

var
  Mode: TFMOD_MODE;
  FmodName: String;
begin
  inherited;
  TimeStart := Profiler.Start('Loading "' + UriDisplay(AUrl) + '" (TFMODSoundBufferBackend)');
  try
    FmodName := ResolveCastleDataUrl(Url); // resolve castle-data:/, as FMOD cannot understand it
    if URIProtocol(FmodName) = 'file' then
      FmodName := UriToFilenameSafe(FmodName) // resolve file:/, as FMOD cannot understand it
    {$ifdef ANDROID}
    else
    if URIProtocol(FmodName) = 'castle-android-assets' then
    begin
      { Resolve CGE URL to Android URL that FMOD will handle.
        See https://www.fmod.com/resources/documentation-api?version=2.1&page=platforms-android.html#asset-manager }
      FmodName := 'file:///android_asset/' + URIToAssetPath(FmodName);
    end
    {$endif}
    ;

    { Note: it seems using FMOD_2D here doesn't cause any trouble
      to make spatial sounds, we set FMOD_3D on FMOD sound later if needed. }
    Mode := FMOD_DEFAULT or FMOD_2D;
    if FSoundLoading = slStreaming then
      Mode := Mode or FMOD_CREATESTREAM;

    CheckFMOD(FMOD_System_CreateSound(FMODSystem, PCharOrNil(FmodName), Mode,
      nil { @SoundInfo }, @FMODSound));

    CalculateProperties;
  finally Profiler.Stop(TimeStart) end;
end;

procedure TFMODSoundBufferBackend.ContextClose;
begin
  CheckFMOD(FMOD_Sound_Release(FMODSound));
  FMODSound := nil;
  inherited;
end;

function TFMODSoundBufferBackend.Duration: TFloatTime;
begin
  Result := FDuration;
end;

function TFMODSoundBufferBackend.DataFormat: TSoundDataFormat;
begin
  Result := FDataFormat;
end;

function TFMODSoundBufferBackend.Frequency: Cardinal;
begin
  Result := FFrequency;
end;

{ TFMODSoundSourceBackend -------------------------------------------------- }

function TFMODSoundSourceBackend.FMODSystem: PFMOD_SYSTEM;
begin
  Result := (SoundEngine as TFMODSoundEngineBackend).FMODSystem;
end;

procedure TFMODSoundSourceBackend.ContextOpen;
begin
  // reflect FMOD defaults
  FReferenceDistance := 1;
  FMaxDistance := 10000;
end;

procedure TFMODSoundSourceBackend.ContextClose;
begin
  Stop;
end;

function TFMODSoundSourceBackend.PlayingOrPaused: boolean;
var
  B: TFMOD_BOOL;
  IsPlayingError: TFMOD_RESULT;
begin
  if FMODChannel = nil then Exit(false);

  { Note that Loop sound will have IsPlaying forever until it's explicitly stopped,
    and that's what we want. }
  IsPlayingError := FMOD_Channel_IsPlaying(FMODChannel, @B);

  // When sound stopped playing, Channel may become invalid
  if (IsPlayingError = FMOD_ERR_INVALID_HANDLE) or
     (IsPlayingError = FMOD_ERR_CHANNEL_STOLEN) then
  begin
    FMODChannel := nil;
    Exit(false);
  end;

  CheckFMOD(IsPlayingError);
  Result := B <> 0;
end;

procedure TFMODSoundSourceBackend.Play(const BufferChangedRecently: Boolean);
begin
  if FMODChannel = nil then Exit;

  CheckFMOD(FMOD_Channel_SetPaused(FMODChannel, 0));
end;

procedure TFMODSoundSourceBackend.Stop;
begin
  if FMODChannel <> nil then
  begin
    { This causes FMOD_ERR_INVALID_HANDLE sometimes on Windows when exiting
      application (destroying FMOD backend).
      No other problem is visible and it seems completely random,
      so I assume it is a random FMOD error. }
    CheckFMOD(FMOD_Channel_Stop(FMODChannel), 'FMOD_Channel_Stop', true);
    FMODChannel := nil; // FMODChannel should not be used anymore
  end;
end;

procedure TFMODSoundSourceBackend.SetPosition(const Value: TVector3);
begin
  FPosition := Value;
  if FMODChannel = nil then Exit;
  if FSpatial then // only if Spatial, otherwise FMOD raises error
    CheckFMOD(FMOD_Channel_Set3DAttributes(FMODChannel, @FPosition, @FVelocity));
end;

function TFMODSoundSourceBackend.Mode: TFMOD_MODE;
begin
  Result := 0;

  if FLoop then
    Result := Result or FMOD_LOOP_NORMAL
  else
    Result := Result or FMOD_LOOP_OFF;

  if FSpatial then
  begin
    Result := Result or FMOD_3D;
    { These flags are only allowed with FMOD_3D }
    case (SoundEngine as TFMODSoundEngineBackend).FDistanceModel of
      dmInverse: Result := Result or FMOD_3D_INVERSEROLLOFF;
      dmLinear : Result := Result or FMOD_3D_LINEARROLLOFF;
    end;
  end else
    Result := Result or FMOD_2D;
end;

procedure TFMODSoundSourceBackend.SetVelocity(const Value: TVector3);
begin
  FVelocity := Value;
  if FMODChannel = nil then Exit;
  if FSpatial then // only if Spatial, otherwise FMOD raises error
    CheckFMOD(FMOD_Channel_Set3DAttributes(FMODChannel, @FPosition, @FVelocity));
end;

procedure TFMODSoundSourceBackend.SetLoop(const Value: boolean);
begin
  FLoop := Value;
  if FMODChannel = nil then Exit;
  CheckFMOD(FMOD_Channel_SetMode(FMODChannel, Mode));
end;

procedure TFMODSoundSourceBackend.SetSpatial(const Value: boolean);
begin
  FSpatial := Value;
  if FMODChannel = nil then Exit;
  CheckFMOD(FMOD_Channel_SetMode(FMODChannel, Mode));

  // call FMOD_Channel_Set3DXxx to update settings
  if FSpatial then // only if Spatial, otherwise FMOD raises error
  begin
    CheckFMOD(FMOD_Channel_Set3DAttributes(FMODChannel, @FPosition, @FVelocity));
    CheckFMOD(FMOD_Channel_Set3DMinMaxDistance(FMODChannel, FReferenceDistance, FMaxDistance));
  end;
end;

procedure TFMODSoundSourceBackend.SetVolume(const Value: Single);
begin
  if FMODChannel = nil then Exit;

  CheckFMOD(FMOD_Channel_SetVolume(FMODChannel, Value));
end;

procedure TFMODSoundSourceBackend.SetMinGain(const Value: Single);
begin
  // not possible (well, not straightforward) with FMOD backend
end;

procedure TFMODSoundSourceBackend.SetMaxGain(const Value: Single);
begin
  // not possible (well, not straightforward) with FMOD backend
end;

procedure TFMODSoundSourceBackend.SetBuffer(const Value: TSoundBufferBackend);
begin
  FBuffer := Value as TFMODSoundBufferBackend;

  if FMODChannel <> nil then
    Stop;
  Assert(FMODChannel = nil);
  if FBuffer <> nil then
  begin
    // Start in paused state, allows to adjust parameters like loop before starting
    CheckFMOD(FMOD_System_PlaySound(FMODSystem, FBuffer.FMODSound, nil,
      { paused } 1, @FMODChannel));
  end;
end;

procedure TFMODSoundSourceBackend.SetPitch(const Value: Single);
begin
  if FMODChannel = nil then Exit;

  CheckFMOD(FMOD_Channel_SetPitch(FMODChannel, Value));
end;

procedure TFMODSoundSourceBackend.SetReferenceDistance(const Value: Single);
begin
  FReferenceDistance := Value;
  if FMODChannel = nil then Exit;
  if not FSpatial then Exit; // apply this only if Spatial
  CheckFMOD(FMOD_Channel_Set3DMinMaxDistance(FMODChannel, FReferenceDistance, FMaxDistance));
end;

procedure TFMODSoundSourceBackend.SetMaxDistance(const Value: Single);
begin
  FMaxDistance := Value;
  if FMODChannel = nil then Exit;
  if not FSpatial then Exit; // apply this only if Spatial
  CheckFMOD(FMOD_Channel_Set3DMinMaxDistance(FMODChannel, FReferenceDistance, FMaxDistance));
end;

procedure TFMODSoundSourceBackend.SetPriority(const Value: Single);
begin
  { TODO: although we pass it to FMOD, we also manually manage limited sources
    in TSoundAllocator.
    We should instead allocate many sound sources (MaxAllocatedSources large?)
    and let FMOD to do its job. }

  if FMODChannel = nil then Exit;
  CheckFMOD(FMOD_Channel_SetPriority(FMODChannel, Round(Value * 256)));
end;

function TFMODSoundSourceBackend.GetOffset: Single;
var
  Pos: CUInt;
begin
  { Make sure to set FMODChannel to nil if not playing now,
    otherwise FMOD would report error FMOD_ERR_INVALID_HANDLE when the sound is near the end.
    Testcase: fmod-test-editor, play sound using button. }
  PlayingOrPaused;

  if FMODChannel = nil then Exit(0);
  CheckFMOD(FMOD_Channel_GetPosition(FMODChannel, @Pos, FMOD_TIMEUNIT_MS));
  Result := Pos / 1000;
end;

procedure TFMODSoundSourceBackend.SetOffset(const Value: Single);
var
  Pos: CUInt;
begin
  if FMODChannel = nil then Exit;
  Pos := Round(Value * 1000);
  CheckFMOD(FMOD_Channel_SetPosition(FMODChannel, Pos, FMOD_TIMEUNIT_MS));
end;

{ TFMODSoundEngineBackend -------------------------------------------------- }

function TFMODSoundEngineBackend.CreateBuffer(const SoundLoading: TSoundLoading): TSoundBufferBackend;
begin
  Result := TFMODSoundBufferBackend.Create(Self, SoundLoading);
end;

function TFMODSoundEngineBackend.CreateSource: TSoundSourceBackend;
begin
  Result := TFMODSoundSourceBackend.Create(Self);
end;

function TFMODSoundEngineBackend.ContextOpen(const ADevice: String;
  out Information, InformationSummary: String): Boolean;
var
  Version: CUInt;
  {$ifdef ANDROID} Env: PJNIEnv; {$endif}
begin
  FmodLibraryUsingBegin;

  {$ifdef ANDROID}
  { This is necessary, otherwise FMOD shows an error
    and FMOD_System_Create exits with internal error:

      FMOD_JNI_GetEnv: Native threads must be attached to the Java virtual machine, please call JavaVM::AttachCurrentThread before invocation.
  }
  Env := nil; // make sure uninitialized
  AndroidMainApp^.Activity^.VM^^.AttachCurrentThread(AndroidMainApp^.Activity^.VM, @Env, nil);
  {$endif}

  { Uncomment FMOD_Debug_Initialize call to get additional logs.

    Note that FMOD_Debug_Initialize will only work if you use
    "logging version" of the FMOD library.
    Just rename libfmodL.so to libfmod.so.

    On Android: Run "adb logcat | grep fmod" to see useful output,
    it is not visible on just "castle-engine run --target=android". }
  // CheckFMOD(FMOD_Debug_Initialize(FMOD_DEBUG_LEVEL_LOG or FMOD_DEBUG_DISPLAY_TIMESTAMPS,
  //   FMOD_DEBUG_MODE_TTY, nil, nil), 'FMOD_Debug_Initialize', true);

  CheckFMOD(FMOD_System_Create(@FMODSystem), 'FMOD_System_Create');
  { Use FMOD_INIT_3D_RIGHTHANDED, as by default FMOD uses left-handed coordinate system. }
  CheckFMOD(FMOD_System_Init(FMODSystem, 256, FMOD_INIT_NORMAL or FMOD_INIT_3D_RIGHTHANDED, nil), 'FMOD_System_Init');
  CheckFMOD(FMOD_System_GetVersion(FMODSystem, @Version), 'FMOD_System_GetVersion');
  Information := Format('FMOD %d.%d.%d', [
    Version shr 16,
    (Version and $FF00) shr 8,
    Version and $FF
  ]);
  InformationSummary := Information;
  Result := true;
end;

procedure TFMODSoundEngineBackend.ContextClose;
begin
  CheckFMOD(FMOD_System_Close(FMODSystem));
  CheckFMOD(FMOD_System_Release(FMODSystem));
  FMODSystem := nil;
  FmodLibraryUsingEnd;
end;

procedure TFMODSoundEngineBackend.Update;
begin
  inherited;
  CheckFMOD(FMOD_System_Update(FMODSystem));
end;

procedure TFMODSoundEngineBackend.SetVolume(const Value: Single);
var
  MasterChannel: PFMOD_CHANNELGROUP;
begin
  CheckFMOD(FMOD_System_GetMasterChannelGroup(FMODSystem, @MasterChannel));
  CheckFMOD(FMOD_ChannelGroup_SetVolume(MasterChannel, Value));
end;

procedure TFMODSoundEngineBackend.SetDistanceModel(const Value: TSoundDistanceModel);
begin
  FDistanceModel := Value;
end;

procedure TFMODSoundEngineBackend.SetDopplerFactor(const Value: Single);
begin
  CheckFMOD(FMOD_System_Set3DSettings(FMODSystem, Value, 1, 1));
end;

procedure TFMODSoundEngineBackend.SetListener(const Position, Direction, Up: TVector3);
const
  ListenerVelocity: TVector3 = (X: 0; Y: 0; Z: 0);
begin
  CheckFMOD(FMOD_System_Set3DListenerAttributes(FMODSystem, 0,  @Position, @ListenerVelocity, @Direction, @Up));
end;

{ globals -------------------------------------------------------------------- }

procedure UseFMODSoundBackend;
begin
  {$ifdef ANDROID}
  { Calling InitializeFmodLibrary is necessary on Android to load dynamic library,
    otherwise InitializeFmodLibrary is never called and fmod entry points
    are not loaded from libfmod.so, so FmodLibraryAvailable = false.

    This could be fixed better (since this problem is not specific to FMOD):

    - Initialization in castle-engine/src/audio/fmod/castleinternalfmod_dynamic.inc
      should register a callback that would be later done from TCastleApplication.Run.

    - We could also try exposing JNI_OnLoad, and then since FPC 3.2.0 maybe we can ignore
      the issue and remove whole ALLOW_DLOPEN_FROM_UNIT_INITIALIZATION complication?
      FPC recommends it, https://wiki.freepascal.org/Android -
      "if you are creating a JNI shared library, always export JNI_OnLoad, even if it is empty". }
  InitializeFmodLibrary;
  {$endif}

  if not FmodLibraryAvailable then
  begin
    WritelnWarning('FMOD library not available, aborting setting FMOD as sound backend');
    Exit;
  end;

  SoundEngine.InternalBackend := TFMODSoundEngineBackend.Create;
end;

initialization
  // our TVector3 is equal in memory to TFMOD_VECTOR, we rely on it here
  Assert(SizeOf(TFMOD_VECTOR) = SizeOf(TVector3));
end.
