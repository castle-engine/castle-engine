{%MainUnit castlesoundengine.pas}
{
  Copyright 2010-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

//type // allow type declaration to continue

  { Looping sound management, to easily play music or other looping sounds.

    Instance of this class should be created only internally
    by the TRepoSoundEngine, always use this through
    @link(TRepoSoundEngine.MusicPlayer)
    or @link(TRepoSoundEngine.LoopingChannel). }
  TLoopingChannel = class
  private
    { Engine that owns this. }
    FEngine: TRepoSoundEngine;

    { @nil if we don't play sound right now.
      This may happen for many reasons -- e.g. because sound context is not open,
      or Sound = stNone, or had URL = '' in RepositoryURL (sounds XML file),
      or TInternalPlayingSound instance was necessary for higher-priority sounds. }
    FAllocatedSource: TInternalPlayingSound;

    FVolume, FPitch: Single;
    { Values of TSoundInfo associated with Sound, will be multiplied by our Volume/Pitch. }
    SoundInfoVolume, SoundInfoPitch: Single;

    FSound: TSoundType;
    procedure SetSound(const Value: TSoundType);
    procedure AllocatedSourceRelease(Sender: TInternalPlayingSound);

    { Called by ContextOpen. You should check here if
      Sound <> stNone and eventually initialize FAllocatedSource. }
    procedure AllocateSource;
    function GetVolume: Single;
    procedure SetVolume(const Value: Single);
    function GetPitch: Single;
    procedure SetPitch(const Value: Single);
  public
    const
      DefaultVolume = 1.0;
      DefaultMusicVolume = 1.0 deprecated 'use DefaultVolume';

    constructor Create(AnEngine: TRepoSoundEngine);
    destructor Destroy; override;

    { Currently played sound.
      Set to stNone to stop playing.
      Set to anything else to play.

      Changing value of this property (when both the old and new values
      are <> stNone and are different) restarts playing the sound.

      By default none (stNone). }
    property Sound: TSoundType read FSound write SetSound;

    { Volume. This must always be within 0..1 range.
      0.0 means that there is no music (this case should be optimized).}
    property Volume: Single read GetVolume write SetVolume default DefaultVolume;

    property MusicVolume: Single read GetVolume write SetVolume default DefaultVolume;
      deprecated 'use Volume';

    property Pitch: Single read GetPitch write SetPitch;
  end;

  TMusicPlayer = TLoopingChannel deprecated 'use TLoopingChannel';

{$endif read_interface}

{$ifdef read_implementation}

{ TLoopingChannel --------------------------------------------------------------- }

constructor TLoopingChannel.Create(AnEngine: TRepoSoundEngine);
begin
  inherited Create;
  FVolume := DefaultVolume;
  FPitch := 1.0;
  FEngine := AnEngine;
end;

destructor TLoopingChannel.Destroy;
begin
  if FAllocatedSource <> nil then
    FAllocatedSource.Release;
  inherited;
end;

procedure TLoopingChannel.AllocateSource;
var
  Parameters: TPlaySoundParameters;
begin
  if Sound = nil then Exit;

  SoundInfoVolume := Sound.Volume;
  SoundInfoPitch := Sound.Pitch;

  Parameters := TPlaySoundParameters.Create;
  try
    Parameters.Buffer     := Sound.Buffer;
    Parameters.Spatial    := false;
    Parameters.Looping    := true;
    Parameters.Importance := { TODO SoundInfo.DefaultImportance } DefaultSoundImportance;
    Parameters.Volume     := Volume * SoundInfoVolume;
    {$warnings off} // using unportable symbols knowingly
    Parameters.MinGain    := Sound.MinGain;
    Parameters.MaxGain    := Sound.MaxGain;
    {$warnings on}
    Parameters.Pitch      := Pitch * SoundInfoPitch;
    FAllocatedSource := FEngine.PlaySound(Parameters);
  finally FreeAndNil(Parameters) end;

  if FAllocatedSource <> nil then
    FAllocatedSource.OnRelease :=
      {$ifdef CASTLE_OBJFPC}@{$endif} AllocatedSourceRelease
  else
    WritelnWarning('Could not allocate sound source, to play looping sound %s', [
      Sound.Name
    ]);
end;

procedure TLoopingChannel.SetSound(const Value: TSoundType);
begin
  if Value <> FSound then
  begin
    if FAllocatedSource <> nil then
    begin
      FAllocatedSource.Release;
      { AllocatedSourceRelease should set FAllocatedSource to nil. }
      Assert(FAllocatedSource = nil);
    end;

    FSound := Value;

    AllocateSource;
  end;
end;

procedure TLoopingChannel.AllocatedSourceRelease(Sender: TInternalPlayingSound);
begin
  Assert(Sender = FAllocatedSource);
  FAllocatedSource := nil;
end;

function TLoopingChannel.GetVolume: Single;
begin
  Result := FVolume;
end;

procedure TLoopingChannel.SetVolume(const Value: Single);
begin
  if Value <> FVolume then
  begin
    FVolume := Value;
    if FAllocatedSource <> nil then
      FAllocatedSource.Volume := Volume * SoundInfoVolume;
  end;
end;

function TLoopingChannel.GetPitch: Single;
begin
  Result := FPitch;
end;

procedure TLoopingChannel.SetPitch(const Value: Single);
begin
  if Value <> FPitch then
  begin
    FPitch := Value;
    if FAllocatedSource <> nil then
      FAllocatedSource.Pitch := Pitch * SoundInfoPitch;
  end;
end;

{$endif read_implementation}
