{%MainUnit castlesoundengine.pas}
{
  Copyright 2010-2023 Michalis Kamburelis.

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
  TLoopingChannel = class(TComponent)
  private
    { Engine that owns this. }
    FEngine: TRepoSoundEngine;

    { @nil if we don't play sound right now.
      This may happen for many reasons -- e.g. because sound context is not open,
      or Sound = nil, or had URL = '' in RepositoryURL (sounds XML file),
      or TInternalSoundSource instance was necessary for higher-priority sounds. }
    FAllocatedSource: TInternalSoundSource;

    FVolume, FPitch: Single;
    FSound: TCastleSound;
    procedure SetSound(const Value: TCastleSound);
    procedure AllocatedSourceRelease(Sender: TInternalSoundSource);

    { Called by ContextOpen. You should check here if
      Sound <> nil and eventually initialize FAllocatedSource. }
    procedure AllocateSource;
    function GetVolume: Single;
    procedure SetVolume(const Value: Single);
    function GetPitch: Single;
    procedure SetPitch(const Value: Single);
    procedure SoundChange(const Sender: TCastleSound; const Change: TSoundChange);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    const
      DefaultVolume = 1.0;
      {$ifdef FPC}DefaultMusicVolume = 1.0 deprecated 'use DefaultVolume';{$endif}

    constructor Create(AnEngine: TRepoSoundEngine); reintroduce;
    destructor Destroy; override;

    { Currently played looping sound. Set to @nil to just stop playing.

      This only plays non-spatial sounds, as it doesn't give any control over
      the sound 3D position.
      Instead of this method, use TCastleSoundSource
      (attached to some TCastleTransform) for spatial sounds to control their 3D position. }
    property Sound: TCastleSound read FSound write SetSound;

    { Volume. This must always be within 0..1 range.
      0.0 means that there is no music (this case should be optimized).}
    property Volume: Single read GetVolume write SetVolume {$ifdef FPC}default DefaultVolume{$endif};

    {$ifdef FPC}
    property MusicVolume: Single read GetVolume write SetVolume default DefaultVolume;
      deprecated 'use Volume';
    {$endif}

    property Pitch: Single read GetPitch write SetPitch;
  end;

  TMusicPlayer = TLoopingChannel deprecated 'use TLoopingChannel';

{$endif read_interface}

{$ifdef read_implementation}

{ TLoopingChannel --------------------------------------------------------------- }

constructor TLoopingChannel.Create(AnEngine: TRepoSoundEngine);
begin
  inherited Create(nil); // TRepoSoundEngine will free us manually
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
  if (Sound = nil) or (Sound.InternalBuffer = nil) then Exit;

  Parameters := TPlaySoundParameters.Create;
  try
    Parameters.Buffer     := Sound.InternalBuffer;
    Parameters.Spatial    := false;
    Parameters.Loop       := true;
    Parameters.Priority   := Sound.Priority;
    Parameters.Pitch      := Pitch * Sound.Pitch;
    Parameters.Volume     := Volume * Sound.Volume;
    {$warnings off} // using unportable symbols knowingly
    Parameters.MinGain    := Sound.MinGain;
    Parameters.MaxGain    := Sound.MaxGain;
    {$warnings on}
    {$warnings off} // using deprecated that should be internal
    FAllocatedSource := FEngine.PlaySound(Parameters);
    {$warnings on}
  finally FreeAndNil(Parameters) end;

  if FAllocatedSource <> nil then
    FAllocatedSource.OnRelease :=
      {$ifdef FPC}@{$endif} AllocatedSourceRelease
  else
  { Make a warning only if FEngine.Enabled.
    When FEngine.Enabled = false (like when --no-sound was passed) this is normal. }
  if FEngine.Enabled then
    WritelnWarning('Could not allocate sound source, to play looping sound %s', [
      Sound.Name
    ]);
end;

procedure TLoopingChannel.SetSound(const Value: TCastleSound);
begin
  if Value <> FSound then
  begin
    if FSound <> nil then
    begin
      FSound.InternalRemoveChangeNotification({$ifdef FPC}@{$endif}SoundChange);
      FSound.RemoveFreeNotification(Self);
      if FAllocatedSource <> nil then
      begin
        FAllocatedSource.Release;
        { AllocatedSourceRelease should set FAllocatedSource to nil. }
        Assert(FAllocatedSource = nil);
      end;
    end;

    FSound := Value;

    if FSound <> nil then
    begin
      FSound.InternalAddChangeNotification({$ifdef FPC}@{$endif}SoundChange);
      FSound.FreeNotification(Self);
      AllocateSource;
    end;
  end;
end;

procedure TLoopingChannel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  { set to nil by setter to clean nicely, and stop playing }
  if (Operation = opRemove) and (AComponent = FSound) then
    Sound := nil;
end;

procedure TLoopingChannel.SoundChange(const Sender: TCastleSound; const Change: TSoundChange);
begin
  case Change of
    scBeforeBufferDestroyed:
      begin
        if FAllocatedSource <> nil then
          FAllocatedSource.Release;
        Assert(FAllocatedSource = nil);
      end;
    scAfterBufferCreated:
      AllocateSource;
    scAfterOther:
      begin
        if FAllocatedSource <> nil then
        begin
          FAllocatedSource.Priority := Sound.Priority;
          FAllocatedSource.Pitch   := Pitch * Sound.Pitch;
          FAllocatedSource.Volume  := Volume * Sound.Volume;
          {$warnings off} // using unportable symbols knowingly
          FAllocatedSource.MinGain := Sound.MinGain;
          FAllocatedSource.MaxGain := Sound.MaxGain;
          {$warnings on}
        end;
      end;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TLoopingChannel.SoundChange:Change');
    {$endif}
  end;
end;

procedure TLoopingChannel.AllocatedSourceRelease(Sender: TInternalSoundSource);
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
      FAllocatedSource.Volume := Volume * Sound.Volume;
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
      FAllocatedSource.Pitch := Pitch * Sound.Pitch;
  end;
end;

{$endif read_implementation}
