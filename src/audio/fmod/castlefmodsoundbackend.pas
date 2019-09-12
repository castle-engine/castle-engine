{
  Copyright 2019-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Sound engine backend using FMOD.
  See https://github.com/castle-engine/castle-engine/wiki/FMOD
  about using FMOD with CGE.
}
unit CastleFMODSoundBackend;

{$I castleconf.inc}

interface

{ Use this to set sound engine backend to FMOD.
  You can call this at any point of your application.
  If you call it before any sound loading/playing,
  then the previous sound backend wil not even be initialized.

  This does nothing (and shows a warning) if the FMOD library
  is dynamic on this platform (like Windows, Linux) and it could not be found. }
procedure UseFMODSoundBackend;

implementation

uses SysUtils, Classes, Math, StrUtils, CTypes,
  CastleVectors, CastleTimeUtils, CastleLog, CastleUtils, CastleURIUtils,
  CastleClassUtils, CastleStringUtils, CastleInternalSoundFile,
  CastleInternalAbstractSoundBackend, CastleSoundBase, CastleSoundEngine,
  CastleInternalFMOD;

{ sound backend classes interface -------------------------------------------- }

type
  TFMODSoundBufferBackend = class(TSoundBufferBackend)
  strict private
    FDuration: TFloatTime;
    FDataFormat: TSoundDataFormat;
    FFrequency: LongWord;
  private
    FSoundLoading: TSoundLoading;
    FMODSound: PFMOD_SOUND;
    function FMODSystem: PFMOD_SYSTEM;
  public
    constructor Create(const ASoundEngine: TSoundEngineBackend;
      const ASoundLoading: TSoundLoading);
    procedure ContextOpen(const AURL: String); override;
    procedure ContextClose; override;
    function Duration: TFloatTime; override;
    function DataFormat: TSoundDataFormat; override;
    function Frequency: LongWord; override;
  end;

  TFMODSoundSourceBackend = class(TSoundSourceBackend)
  strict private
    FMODChannel: PFMOD_CHANNEL;
    FBuffer: TFMODSoundBufferBackend;
    function FMODSystem: PFMOD_SYSTEM;
  public
    procedure ContextOpen; override;
    procedure ContextClose; override;
    function PlayingOrPaused: boolean; override;
    procedure Play(const BufferChangedRecently: Boolean); override;
    procedure Stop; override;
    procedure SetPosition(const Value: TVector3); override;
    procedure SetVelocity(const Value: TVector3); override;
    procedure SetLooping(const Value: boolean); override;
    procedure SetRelative(const Value: boolean); override;
    procedure SetGain(const Value: Single); override;
    procedure SetMinGain(const Value: Single); override;
    procedure SetMaxGain(const Value: Single); override;
    procedure SetBuffer(const Value: TSoundBufferBackend); override;
    procedure SetPitch(const Value: Single); override;
    procedure SetRolloffFactor(const Value: Single); override;
    procedure SetReferenceDistance(const Value: Single); override;
    procedure SetMaxDistance(const Value: Single); override;
    function GetOffset: Single; override;
    procedure SetOffset(const Value: Single); override;
  end;

  TFMODSoundEngineBackend = class(TSoundEngineBackend)
  private
    FMODSystem: PFMOD_SYSTEM;
  public
    function ContextOpen(const ADevice: String; out Information: String): Boolean; override;
    procedure ContextClose; override;
    function CreateBuffer(const SoundLoading: TSoundLoading): TSoundBufferBackend; override;
    function CreateSource: TSoundSourceBackend; override;

    procedure Update; override;
    procedure SetGain(const Value: Single); override;
    procedure SetDistanceModel(const Value: TSoundDistanceModel); override;
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

procedure TFMODSoundBufferBackend.ContextOpen(const AURL: String);
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
      WritelnWarning('Cannot determine correct duration of sound file "%s"', [URIDisplay(AURL)]);

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
        URIDisplay(AURL),
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
  TimeStart := Profiler.Start('Loading "' + URIDisplay(AURL) + '" (TFMODSoundBufferBackend)');
  try
    FmodName := ResolveCastleDataURL(URL); // resolve castle-data:/, as FMOD cannot understand it
    if URIProtocol(FmodName) = 'file' then
      FmodName := URIToFilenameSafe(FmodName); // resolve file:/, as FMOD cannot understand it

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

function TFMODSoundBufferBackend.Frequency: LongWord;
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

  { Note that Looping sound will have IsPlaying forever until it's explicitly stopped,
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
  // TODO
end;

procedure TFMODSoundSourceBackend.SetVelocity(const Value: TVector3);
begin
  // TODO
end;

procedure TFMODSoundSourceBackend.SetLooping(const Value: boolean);
begin
  if FMODChannel = nil then Exit;

  if Value then
    CheckFMOD(FMOD_Channel_SetMode(FMODChannel, FMOD_LOOP_NORMAL))
  else
    CheckFMOD(FMOD_Channel_SetMode(FMODChannel, FMOD_LOOP_OFF));
end;

procedure TFMODSoundSourceBackend.SetRelative(const Value: boolean);
begin
  // TODO
end;

procedure TFMODSoundSourceBackend.SetGain(const Value: Single);
begin
  if FMODChannel = nil then Exit;

  CheckFMOD(FMOD_Channel_SetVolume(FMODChannel, Value));
end;

procedure TFMODSoundSourceBackend.SetMinGain(const Value: Single);
begin
  // TODO
end;

procedure TFMODSoundSourceBackend.SetMaxGain(const Value: Single);
begin
  // TODO
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

procedure TFMODSoundSourceBackend.SetRolloffFactor(const Value: Single);
begin
  // TODO
end;

procedure TFMODSoundSourceBackend.SetReferenceDistance(const Value: Single);
begin
  // TODO
end;

procedure TFMODSoundSourceBackend.SetMaxDistance(const Value: Single);
begin
  // TODO
end;

function TFMODSoundSourceBackend.GetOffset: Single;
begin
  // TODO
  Result := 0;
end;

procedure TFMODSoundSourceBackend.SetOffset(const Value: Single);
begin
  // TODO
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
  out Information: String): Boolean;
var
  Version: CUInt;
begin
  FmodLibraryUsingBegin;
  CheckFMOD(FMOD_System_Create(@FMODSystem));
  CheckFMOD(FMOD_System_Init(FMODSystem, 256, FMOD_INIT_NORMAL, nil));
  CheckFMOD(FMOD_System_GetVersion(FMODSystem, @Version));
  Information := Format('FMOD version %d.%d.%d initialized', [
    Version shr 16,
    (Version and $FF00) shr 8,
    Version and $FF
  ]);
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

procedure TFMODSoundEngineBackend.SetGain(const Value: Single);
var
  MasterChannel: PFMOD_CHANNELGROUP;
begin
  CheckFMOD(FMOD_System_GetMasterChannelGroup(FMODSystem, @MasterChannel));
  CheckFMOD(FMOD_ChannelGroup_SetVolume(MasterChannel, Value));
end;

procedure TFMODSoundEngineBackend.SetDistanceModel(const Value: TSoundDistanceModel);
begin
  // TODO
end;

procedure TFMODSoundEngineBackend.SetListener(const Position, Direction, Up: TVector3);
begin
  // TODO
end;

{ globals -------------------------------------------------------------------- }

procedure UseFMODSoundBackend;
begin
  if not FmodLibraryAvailable then
  begin
    WritelnWarning('FMOD library not available, aborting setting FMOD as sound backend');
    Exit;
  end;

  SoundEngine.InternalBackend := TFMODSoundEngineBackend.Create;
end;

end.
