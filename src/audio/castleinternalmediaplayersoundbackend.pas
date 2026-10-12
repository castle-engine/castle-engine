{
  Copyright 2026-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Sound engine backend using TMediaPlayer (from FireMonkey's FMX.Audio).

  This is not very functional or optimal (just simple sound playback,
  no spatialization) but it does the job in simple case (when you don't
  need spatialized sound).

  Delphi VCL seems to also have a similar TMediaPlayer API (Vcl.MPlayer).
  We don't support it now (it would have no use -- on Delphi/Windows we
  already do sound by full-featured OpenAL by default, and offer more options
  like FMOD). But in case it will make sense in the future, this unit should
  be easy to adapt.

  Automatically used on Delphi/iOS and Delphi/Android right now. }
unit CastleInternalMediaPlayerSoundBackend;

{$I castleconf.inc}

interface

{$ifdef FPC}
{ FPC does not have FMX.Audio or other TMediaPlayer implementation.
  It also doesn't need one -- we ship OpenAL with FPC/Android and FPC/iOS
  builds, which is fully-featured 3D sound playback. }

implementation
end.
{$else}

{ Use TMediaPlayer (from FMX.Audio) to play sound in Castle Game Engine. }
procedure UseMediaPlayerSoundBackend;

implementation

uses SysUtils, Classes, FMX.Media,
  CastleInternalAbstractSoundBackend, CastleSoundBase, CastleSoundEngine,
  CastleLog, CastleUriUtils, CastleTimeUtils, CastleVectors, CastleClassUtils,
  CastleFilesUtils, CastleDownload;

type
  TMediaPlayerSoundBufferBackend = class(TSoundBufferBackend)
  strict private
    { Always both empty, or both non-empty.
      If not empty, we will remove it in ContextClose. }
    FTemporaryUrl, FTemporaryFileName: String;
  private
    FFileName: String;
    FDuration: TFloatTime;
  public
    procedure ContextOpen(const AUrl: String); override;
    procedure ContextClose; override;

    { Duration is initialized on first play (from TMediaPlayer.Duration). }
    function Duration: TFloatTime; override;

    { Unknown for this backend, always returns 1 (assumed mono). }
    function Channels: Cardinal; override;

    { Unknown for this backend, always returns 0. }
    function Frequency: TSoundFrequency; override;
  end;

  TMediaPlayerSoundSourceBackend = class(TSoundSourceBackend)
  strict private
    FBuffer: TMediaPlayerSoundBufferBackend;
    FMediaPlayer: TMediaPlayer;
    FVolume, FLastFinalVolume: Single;
    FLoop: Boolean;
    { Sound source volume multiplied by the sound engine volume. }
    function FinalVolume: Single;
  public
    constructor Create(const ASoundEngine: TSoundEngineBackend);
    procedure ContextOpen; override;
    procedure ContextClose; override;
    function PlayingOrPaused: boolean; override;
    procedure Play(const BufferChangedRecently: Boolean;
      const InitialOffset: TFloatTime); override;
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
    procedure Update(const SecondsPassed: TFloatTime); override;
  end;

  TMediaPlayerSoundEngineBackend = class(TSoundEngineBackend)
  private
    FVolume: Single;
  public
    constructor Create;
    function ContextOpen(const ADevice: String; out Information, InformationSummary: String): Boolean; override;
    procedure ContextClose; override;
    function CreateBuffer(const SoundLoading: TSoundLoading): TSoundBufferBackend; override;
    function CreateSource: TSoundSourceBackend; override;
    procedure SetVolume(const Value: Single); override;
    procedure SetDistanceModel(const Value: TSoundDistanceModel); override;
    procedure SetDopplerFactor(const Value: Single); override;
    procedure SetListener(const Position, Direction, Up: TVector3); override;
  end;

{ TMediaPlayerSoundBufferBackend -------------------------------------------------- }

procedure TMediaPlayerSoundBufferBackend.ContextOpen(const AUrl: String);
var
  InputS, OutputS: TStream;
begin
  inherited;

  { For Fmx.Audio, we need a filename (not URL or TStream) to play the sound.

    If URL is not a filename (maybe Android asset, maybe some other non-file URL
    like https), we copy it to a local temporary file in castle-config:/ .
    This is good enough for all platforms where it matters (which means only
    Android and iOS now, and desktops just for testing).
  }
  FFileName := UriToFilenameSafe(AUrl);
  if FFileName = '' then
  begin
    FTemporaryUrl := FileNameAutoInc('castle-config:/castle-media-player-tmp/',
      '%d' + ExtractUriExt(AUrl));
    FTemporaryFileName := UriToFilenameSafe(FTemporaryUrl);
    if FTemporaryFileName = '' then
    begin
      FTemporaryUrl := ''; // abort creating temp file
      raise ESoundFileError.Create('URL "castle-config:/..." does not translate to a filename, and Fmx.Audio can only play local files');
    end;

    WritelnLog('Creating temporary copy "%s" for URL "%s" (to pass filename to TMediaPlayer)', [
      FTemporaryUrl,
      AUrl
    ]);
    InputS := Download(AUrl);
    try
      OutputS := UrlSaveStream(FTemporaryUrl);
      try
        ReadGrowingStream(InputS, OutputS, false);
      finally FreeAndNil(OutputS) end;
    finally FreeAndNil(InputS) end;

    FFileName := FTemporaryFileName;
  end;
end;

procedure TMediaPlayerSoundBufferBackend.ContextClose;
begin
  if FTemporaryFileName <> '' then
  begin
    CheckDeleteFile(FTemporaryFileName, { warn } true);
    FTemporaryFileName := '';
    FTemporaryUrl := '';
  end;
  FFileName := '';
  inherited;
end;

function TMediaPlayerSoundBufferBackend.Duration: TFloatTime;
begin
  Result := FDuration;
end;

function TMediaPlayerSoundBufferBackend.Channels: Cardinal;
begin
  Result := 1; // dummy reasonable value, TMediaPlayer doesn't expose this
end;

function TMediaPlayerSoundBufferBackend.Frequency: TSoundFrequency;
begin
  Result := 0; // dummy value, TMediaPlayer doesn't expose this
end;

{ TMediaPlayerSoundSourceBackend -------------------------------------------------- }

constructor TMediaPlayerSoundSourceBackend.Create(const ASoundEngine: TSoundEngineBackend);
begin
  inherited;
  FVolume := 1.0; // default volume
end;

procedure TMediaPlayerSoundSourceBackend.ContextOpen;
begin
end;

procedure TMediaPlayerSoundSourceBackend.ContextClose;
begin
end;

function TMediaPlayerSoundSourceBackend.FinalVolume: Single;
begin
  Result := FVolume *
    (SoundEngine as TMediaPlayerSoundEngineBackend).FVolume;
end;

function TMediaPlayerSoundSourceBackend.PlayingOrPaused: boolean;
begin
  { Note: This relies that FMediaPlayer state changes immediately to
    TMediaState.Playing after Play. Otherwise we will decide sound (non-looping)
    is "not playing"
    and release this sound source too early.

    It seems it's all good (the state changes immediately to TMediaState.Playing
    after Play) at least on Delphi/Android, so we don't need any more complicated
    solution. }

  Result := (FMediaPlayer <> nil) and
   ( (FMediaPlayer.State = TMediaState.Playing) or FLoop );
end;

procedure TMediaPlayerSoundSourceBackend.Play(const BufferChangedRecently: Boolean;
  const InitialOffset: TFloatTime);
begin
  FreeAndNil(FMediaPlayer);

  FLastFinalVolume := FinalVolume;

  { We create FMediaPlayer only for the playback duration.
    This avoids keeping the FMediaPlayer instance around when
    temporary filename no longer exists. }
  FMediaPlayer := TMediaPlayer.Create(nil);
  FMediaPlayer.Volume := FLastFinalVolume;
  FMediaPlayer.FileName := FBuffer.FFileName;
  FMediaPlayer.CurrentTime := Round(InitialOffset * MediaTimeScale); // seek before playing
  FMediaPlayer.Play;

  { update FBuffer.FDuration, by the way }
  if FBuffer.FDuration <= 0 then
  begin
    FBuffer.FDuration := FMediaPlayer.Duration / MediaTimeScale;
    WritelnLog('Duration of %s determined: %f', [
      FBuffer.Url,
      FBuffer.Duration
    ]);
  end;
end;

procedure TMediaPlayerSoundSourceBackend.Stop;
begin
  { Setting FMediaPlayer.FileName to '' is not allowed, so we cannot do this:

      FMediaPlayer.Stop;
      FMediaPlayer.FileName := '';

    But we want to somehow "unload" file, as we may remove the temporary file
    soon, if it was from FTemporaryFileName.
    So destroy FMediaPlayer instead. }
  FreeAndNil(FMediaPlayer);
end;

procedure TMediaPlayerSoundSourceBackend.SetPosition(const Value: TVector3);
begin
end;

procedure TMediaPlayerSoundSourceBackend.SetVelocity(const Value: TVector3);
begin
end;

procedure TMediaPlayerSoundSourceBackend.SetLoop(const Value: boolean);
begin
  FLoop := Value;
end;

procedure TMediaPlayerSoundSourceBackend.SetSpatial(const Value: boolean);
begin
end;

procedure TMediaPlayerSoundSourceBackend.SetVolume(const Value: Single);
begin
  FVolume := Value;
  if FMediaPlayer <> nil then
  begin
    FLastFinalVolume := FinalVolume;
    FMediaPlayer.Volume := FLastFinalVolume;
  end;
end;

procedure TMediaPlayerSoundSourceBackend.SetMinGain(const Value: Single);
begin
end;

procedure TMediaPlayerSoundSourceBackend.SetMaxGain(const Value: Single);
begin
end;

procedure TMediaPlayerSoundSourceBackend.SetBuffer(const Value: TSoundBufferBackend);
begin
  FBuffer := Value as TMediaPlayerSoundBufferBackend;
end;

procedure TMediaPlayerSoundSourceBackend.SetPitch(const Value: Single);
begin
end;

procedure TMediaPlayerSoundSourceBackend.SetReferenceDistance(const Value: Single);
begin
end;

procedure TMediaPlayerSoundSourceBackend.SetMaxDistance(const Value: Single);
begin
end;

procedure TMediaPlayerSoundSourceBackend.SetPriority(const Value: Single);
begin
end;

function TMediaPlayerSoundSourceBackend.GetOffset: Single;
begin
  if FMediaPlayer = nil then
  begin
    WritelnWarning('GetOffset called but FMediaPlayer is nil');
    Result := 0;
    Exit;
  end;

  Result := FMediaPlayer.CurrentTime / MediaTimeScale;
end;

procedure TMediaPlayerSoundSourceBackend.SetOffset(const Value: Single);
begin
  if FMediaPlayer = nil then
  begin
    WritelnWarning('SetOffset called but FMediaPlayer is nil');
    Exit;
  end;

  FMediaPlayer.CurrentTime := Round(Value * MediaTimeScale);
end;

procedure TMediaPlayerSoundSourceBackend.Update(const SecondsPassed: TFloatTime);
var
  NewFinalVolume: Single;
begin
  if FMediaPlayer <> nil then
  begin
    { Update FMediaPlayer.Volume, in case SoundEngine.Volume changed.
      Note: In case setting FMediaPlayer.Volume to same value is expensive,
      we optimize by comparing. }
    NewFinalVolume := FinalVolume;
    if FLastFinalVolume <> NewFinalVolume then
    begin
      FLastFinalVolume := NewFinalVolume;
      FMediaPlayer.Volume := FLastFinalVolume;
    end;

    { Implement naive looping: if the playback stopped, but should be looping,
      just start it again.
      This is synchronized with what PlayingOrPaused does. }
    if (FMediaPlayer.State <> TMediaState.Playing) and FLoop then
    begin
      FMediaPlayer.CurrentTime := 0; // it's unsure if FMediaPlayer.Play would do this always automatically
      FMediaPlayer.Play;
    end;
  end;
end;

{ TMediaPlayerSoundEngineBackend -------------------------------------------------- }

constructor TMediaPlayerSoundEngineBackend.Create;
begin
  inherited;
  FVolume := 1.0;
end;

function TMediaPlayerSoundEngineBackend.ContextOpen(const ADevice: String;
  out Information, InformationSummary: String): Boolean;
begin
  Result := true;
  Information := 'TMediaPlayer (FMX.Media)';
  InformationSummary := Information;
end;

procedure TMediaPlayerSoundEngineBackend.ContextClose;
begin
end;

procedure TMediaPlayerSoundEngineBackend.SetVolume(const Value: Single);
begin
  FVolume := Value;
end;

procedure TMediaPlayerSoundEngineBackend.SetDistanceModel(const Value: TSoundDistanceModel);
begin
end;

procedure TMediaPlayerSoundEngineBackend.SetDopplerFactor(const Value: Single);
begin
end;

procedure TMediaPlayerSoundEngineBackend.SetListener(const Position, Direction, Up: TVector3);
begin
end;

function TMediaPlayerSoundEngineBackend.CreateBuffer(const SoundLoading: TSoundLoading): TSoundBufferBackend;
begin
  // Ignore SoundLoading
  Result := TMediaPlayerSoundBufferBackend.Create(Self);
end;

function TMediaPlayerSoundEngineBackend.CreateSource: TSoundSourceBackend;
begin
  Result := TMediaPlayerSoundSourceBackend.Create(Self);
end;

{ globals -------------------------------------------------------------------- }

procedure UseMediaPlayerSoundBackend;
begin
  SoundEngine.InternalBackend := TMediaPlayerSoundEngineBackend.Create;
end;

{$endif FPC}
end.
