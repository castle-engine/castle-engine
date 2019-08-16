{
  Copyright 2010-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Abstract sound engine backend types. }
unit CastleInternalAbstractSoundBackend;

{$I castleconf.inc}

interface

uses CastleTimeUtils, CastleVectors, CastleSoundBase, CastleInternalSoundFile;

type
  TSoundEngineBackend = class;

  { Abstract sound engine sound buffer: sound file data, not playing and not placed in 3D space. }
  TSoundBufferBackend = class
  strict private
    { Set by ContextOpen. }
    FURL: string;
    FSoundEngine: TSoundEngineBackend;
  protected
    property SoundEngine: TSoundEngineBackend read FSoundEngine;
  public
    { Absolute URL.
      Never empty (do not create TSoundBuffer instances for invalid / empty URL,
      like the ones that can be created by TRepoSoundEngine for not defined sounds.) }
    property URL: String read FURL;

    { Sound buffer information. }
    function Duration: TFloatTime; virtual; abstract;
    function DataFormat: TSoundDataFormat; virtual; abstract;
    function Frequency: LongWord; virtual; abstract;

    constructor Create(const ASoundEngine: TSoundEngineBackend);

    { Load from @link(URL), set a couple of properties.
      When overriding, call inherited first.
      @raises Exception In case sound loading failed for any reason. }
    procedure ContextOpen(const AURL: String); virtual;

    { Guaranteed to be called always after ContextOpen that didn't raise exception,
      and before destructor. }
    procedure ContextClose; virtual;
  end;

  { TSoundBufferBackend descendant that loads sound files using TSoundFile.
    Should be used by sound backends that cannot load sound files themselves,
    and rely on TSoundFile to do it (right now, this applies to all backends
    except FMOD). }
  TSoundBufferBackendFromSoundFile = class(TSoundBufferBackend)
  strict private
    FDuration: TFloatTime;
    FDataFormat: TSoundDataFormat;
    FFrequency: LongWord;
  protected
    { Optionally change (pre-process in some way) SoundFile contents
      before loading it. Called by @link(ContextOpen),
      always before @link(ContextOpenFromSoundFile).
      E.g. if backend only supports 16-bit sound formats,
      here you can call SoundFile.ConvertTo16bit. }
    procedure ContextOpenPreProcess(const SoundFile: TSoundFile); virtual;

    { Load from @link(SoundFile).
      When overriding, call inherited first.
      @raises Exception In case sound loading failed for any reason. }
    procedure ContextOpenFromSoundFile(const SoundFile: TSoundFile); virtual;
  public
    procedure ContextOpen(const AURL: String); override;
    function Duration: TFloatTime; override;
    function DataFormat: TSoundDataFormat; override;
    function Frequency: LongWord; override;
  end;

  { TSoundBufferBackend descendant that loads sound files using TStreamedSoundFile.

    Note that this doesn't actually open file immediately in ContextOpen
    (it does not create TStreamedSoundFile instance at that point).
    Instead the file will be opened on-demand when needed (when playing this,
    or accessing Duration / DataFormat / Frequency).

    This is especially useful on Android, where switching from/to the application
    requires to close/reopen the OpenAL context.
    The sounds that are streamed do not need to be immediately loaded
    in this case (only the playing sounds need to be loaded on-demand).
    So starting and switching back to the application is much faster.

    In contrast, slComplete loads complete data immediately,
    making it more predictable (no additional work at runtime)
    but introducing a loading time.
  }
  TSoundBufferBackendFromStreamedFile = class(TSoundBufferBackend)
  strict private
    FStreamConfigRead: Boolean;
    // Fields below are valid only if FStreamConfigRead
    FDuration: TFloatTime;
    FDataFormat: TSoundDataFormat;
    FFrequency: LongWord;
    procedure ReadStreamConfigFromTemp;
  protected
    procedure ReadStreamConfig(const StreamedSoundFile: TStreamedSoundFile);
  public
    function Duration: TFloatTime; override;
    function DataFormat: TSoundDataFormat; override;
    function Frequency: LongWord; override;
  end;

  { Abstract sound engine sound source: something in 3D that plays sound. }
  TSoundSourceBackend = class
  strict private
    FSoundEngine: TSoundEngineBackend;
  protected
    property SoundEngine: TSoundEngineBackend read FSoundEngine;
  public
    constructor Create(const ASoundEngine: TSoundEngineBackend);

    procedure ContextOpen; virtual; abstract;
    { Guaranteed to be called always after ContextOpen that didn't raise exception,
      and before destructor. }
    procedure ContextClose; virtual; abstract;

    { All the methods below are guaranteed to be called only after ContextOpen. }
    { }

    { Check whether the sound is still playing. }
    function PlayingOrPaused: boolean; virtual; abstract;
    { Play sound from the beginning.
      May be called on an already playing sound, should then restart playing it from beginning. }
    procedure Play(const BufferChangedRecently: Boolean); virtual; abstract;
    { Stop the sound playing.
      May be called on an already stopped sound, should do nothing then. }
    procedure Stop; virtual; abstract;

    { Called continuously on a playing sound source. }
    procedure Update(const SecondsPassed: TFloatTime); virtual;

    procedure SetPosition(const Value: TVector3); virtual; abstract;
    procedure SetVelocity(const Value: TVector3); virtual; abstract;
    procedure SetLooping(const Value: boolean); virtual; abstract;
    procedure SetRelative(const Value: boolean); virtual; abstract;
    procedure SetGain(const Value: Single); virtual; abstract;
    procedure SetMinGain(const Value: Single); virtual; abstract;
    procedure SetMaxGain(const Value: Single); virtual; abstract;
    { We guarantee that SetBuffer is only called on a stopped sound source
      (when PlayingOrPaused is false, because it finished playing or because Stop
      was explicitly called). }
    procedure SetBuffer(const Value: TSoundBufferBackend); virtual; abstract;
    procedure SetPitch(const Value: Single); virtual; abstract;
    procedure SetRolloffFactor(const Value: Single); virtual; abstract;
    procedure SetReferenceDistance(const Value: Single); virtual; abstract;
    procedure SetMaxDistance(const Value: Single); virtual; abstract;
    function GetOffset: Single; virtual; abstract;
    procedure SetOffset(const Value: Single); virtual; abstract;
    property Offset: Single read GetOffset write SetOffset;
  end;

  { Abstract sound engine backend. }
  TSoundEngineBackend = class
  public
    { Open backend. Returns whether it was success.
      Regardless whether it returns @true or @false,
      also fills Information (with reasons of failure,
      or with details about initialized sound backend). }
    function ContextOpen(const ADevice: String; out Information: String): Boolean; virtual; abstract;

    { Close backend.
      Guaranteed to be called only after successfull (returning @true) ContextOpen. }
    procedure ContextClose; virtual; abstract;

    { Create suitable non-abstract TSoundBufferBackend descendant. }
    function CreateBuffer(const SoundLoading: TSoundLoading): TSoundBufferBackend; virtual; abstract;

    { Create suitable non-abstract TSoundSourceBackend descendant. }
    function CreateSource: TSoundSourceBackend; virtual; abstract;

    { Add available sound devices.
      There is always at least one default device, name '',
      it is already added to Devices list when calling this method. }
    procedure DetectDevices(const Devices: TSoundDeviceList); virtual;

    procedure Update; virtual;

    { All the methods below are guaranteed to be called only after ContextOpen. }
    { }
    procedure SetGain(const Value: Single); virtual; abstract;
    procedure SetDistanceModel(const Value: TSoundDistanceModel); virtual; abstract;
    procedure SetListener(const Position, Direction, Up: TVector3); virtual; abstract;
  end;

implementation

uses SysUtils;

{ TSoundBufferBackend -------------------------------------------------------- }

constructor TSoundBufferBackend.Create(const ASoundEngine: TSoundEngineBackend);
begin
  inherited Create;
  FSoundEngine := ASoundEngine;
end;

procedure TSoundBufferBackend.ContextOpen(const AURL: String);
begin
  FURL := AURL;
end;

procedure TSoundBufferBackend.ContextClose;
begin
end;

{ TSoundBufferBackendFromSoundFile -------------------------------------------}

function TSoundBufferBackendFromSoundFile.Duration: TFloatTime;
begin
  Result := FDuration;
end;

function TSoundBufferBackendFromSoundFile.DataFormat: TSoundDataFormat;
begin
  Result := FDataFormat;
end;

function TSoundBufferBackendFromSoundFile.Frequency: LongWord;
begin
  Result := FFrequency;
end;

procedure TSoundBufferBackendFromSoundFile.ContextOpenFromSoundFile(const SoundFile: TSoundFile);
begin
end;

procedure TSoundBufferBackendFromSoundFile.ContextOpenPreProcess(const SoundFile: TSoundFile);
begin
end;

procedure TSoundBufferBackendFromSoundFile.ContextOpen(const AURL: String);
var
  F: TSoundFile;
begin
  inherited;

  F := TSoundFile.Create(URL);
  try
    FDuration := F.Duration;
    FDataFormat := F.DataFormat;
    FFrequency := F.Frequency;
    ContextOpenFromSoundFile(F);
  finally FreeAndNil(F) end;
end;

{ TSoundBufferBackendFromStreamedFile ---------------------------------------- }

function TSoundBufferBackendFromStreamedFile.Duration: TFloatTime;
begin
  if not FStreamConfigRead then
    ReadStreamConfigFromTemp;
  Result := FDuration;
end;

function TSoundBufferBackendFromStreamedFile.DataFormat: TSoundDataFormat;
begin
  if not FStreamConfigRead then
    ReadStreamConfigFromTemp;
  Result := FDataFormat;
end;

function TSoundBufferBackendFromStreamedFile.Frequency: LongWord;
begin
  if not FStreamConfigRead then
    ReadStreamConfigFromTemp;
  Result := FFrequency;
end;

procedure TSoundBufferBackendFromStreamedFile.ReadStreamConfig(const StreamedSoundFile: TStreamedSoundFile);
begin
  if not FStreamConfigRead then
  begin
    FDuration := StreamedSoundFile.Duration;
    FDataFormat := StreamedSoundFile.DataFormat;
    FFrequency := StreamedSoundFile.Frequency;
    FStreamConfigRead := true;
  end;
end;

procedure TSoundBufferBackendFromStreamedFile.ReadStreamConfigFromTemp;
var
  F: TStreamedSoundFile;
begin
  F := TStreamedSoundFile.Create(URL);
  try
    ReadStreamConfig(F);
  finally FreeAndNil(F) end;
end;

{ TSoundSourceBackend -------------------------------------------------------- }

constructor TSoundSourceBackend.Create(const ASoundEngine: TSoundEngineBackend);
begin
  inherited Create;
  FSoundEngine := ASoundEngine;
end;

procedure TSoundSourceBackend.Update(const SecondsPassed: TFloatTime);
begin
end;

{ TSoundEngineBackend -------------------------------------------------------- }

procedure TSoundEngineBackend.DetectDevices(const Devices: TSoundDeviceList);
begin
  // do nothing
end;

procedure TSoundEngineBackend.Update;
begin
  // do nothing
end;

end.
