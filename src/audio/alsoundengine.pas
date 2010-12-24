{
  Copyright 2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ OpenAL sound engine (TALSoundEngine). }
unit ALSoundEngine;

interface

uses SysUtils, Classes, KambiOpenAL, ALSourceAllocator, VectorMath;

const
  DefaultALMinAllocatedSources = 4;
  DefaultALMaxAllocatedSources = 16;

type
  TALBuffer = TALuint;

  EALBufferNotLoaded = class(Exception);

  { OpenAL sound engine. Takes care of all the 3D sound stuff,
    wrapping OpenAL is a nice and comfortable interface.

    There should always be only one instance of this class,
    in global SoundEngine variable. You can create and assign it explicitly,
    then you're also responsible for calling ALContextOpen,
    then ALContextClose and freeing it at the end.
    Or you can let the first TKamSceneManager to take care of this:
    if the first TKamSceneManager.SoundEngine call will see that
    global SoundEngine is not assigned, it will take care to create
    it and initialize (and later close) OpenAL device.

    This way you can just let TKamSceneManager to create sound engine
    (on-demand, e.g. only when you open VRML/X3D file with Sound node).
    Or you can explicitly create it, and then you're independent from
    TKamSceneManager (you can create and destroy TKamSceneManager instances,
    and keep the same sound engine instance). }
  TALSoundEngine = class
  private
    FSoundInitializationReport: string;
    SourceAllocator: TALSourceAllocator;

    { When SourceAllocator <> nil, these correspond to it's properties. }
    FALMinAllocatedSources: Cardinal;
    FALMaxAllocatedSources: Cardinal;

    function GetALMinAllocatedSources: Cardinal;
    procedure SetALMinAllocatedSources(const Value: Cardinal);

    function GetALMaxAllocatedSources: Cardinal;
    procedure SetALMaxAllocatedSources(const Value: Cardinal);
  public
    constructor Create;

    { Initialize OpenAL library, and output device and context.
      Sets SoundOpenializationReport and ALActive.
      You can set ALCDevice before calling this.

      Note that we continue (without any exception) if the initialization
      failed for any reason (maybe OpenAL library is not available,
      or no sound output device is available).
      You can check things like ALActivationErrorMessage
      and ALActive (see TryBeginAL documentation), but generally this class
      will hide from you the fact that sound is not initialized. }
    procedure ALContextOpen(const WasParam_NoSound: boolean); virtual;

    { Call this always to release OpenAL things.
      This is ignored if not ALActive. }
    procedure ALContextClose; virtual;

    property SoundInitializationReport: string read FSoundInitializationReport;

    { If ALActive, then will append some info about current OpenAL used. }
    procedure AppendALInformation(S: TStrings);
    function ALInformation: string;

    { Change ALCDevice while OpenAL is already initialized.
      This cleanly closes the old device (ALContextClose),
      changes ALCDevice value, initializes context again
      (ALContextOpen). }
    procedure ALChangeDevice(const NewALCDevice: string);

    { Min/max number of allocated OpenAL sources.

      These properties are used when creating TALSourceAllocator.
      When TALSourceAllocator is already created, these properties
      correspond to allocator properties (setting them sets also
      allocator properties).

      In summary, you can treat these properties just like analogous
      TALSourceAllocator properties, but you can freely operate on them
      even when OpenAL is not initialized. Which is useful if user disabled
      sound or you want to load/save these values from some config files
      at time when OpenAL couldn't be initialized yet --- in such cases
      AL allocator doesn't exist, but you can operate on these properties
      without worry.

      When changing Min/MaxAllocatedSources, remember to always keep
      MinAllocatedSources <= MaxAllocatedSources.

      @groupBegin }
    property ALMinAllocatedSources: Cardinal
      read GetALMinAllocatedSources write SetALMinAllocatedSources
      default DefaultALMinAllocatedSources;

    property ALMaxAllocatedSources: Cardinal
      read GetALMaxAllocatedSources write SetALMaxAllocatedSources
      default DefaultALMaxAllocatedSources;
    { @groupEnd }

    { Load a sound file into OpenAL buffer. Result is never 0.

      The buffer should be released by FreeBuffer later when it's not needed.
      Although we will take care to always free remaining buffers
      before closing OpenAL context anyway. (And OpenAL would also free
      the buffer anyway at closing, although some OpenAL versions
      could write a warning about this.)

      We have a cache of sound files here. An absolute (expanded) filename
      will be recorded as being loaded to given buffer. Loading the same
      filename second time returns the same OpenAL buffer. The buffer
      is released only once you call FreeBuffer as many times as you called
      LoadBuffer for it. }
    function LoadBuffer(const FileName: string): TALBuffer;

    { Free a sound file buffer. Ignored when buffer is zero.
      Buffer is always set to zero after this.

      @raises(EALBufferNotLoaded When invalid (not zero,
        and not returned by LoadBuffer) buffer identifier is given.) }
    procedure FreeBuffer(var Buffer: TALBuffer);

    { Play a sound from given buffer.

      We use a smart OpenAL sound allocator, so the sound will be actually
      played only if resources allow. Use higher Importance to indicate
      sounds that are more important to play.

      We set the sound properties and start playing it.

      Both spatialized (3D) and not sounds are possible.
      When Spatial = @false, then Position is ignored
      (you can pass anything, like ZeroVector3Single).

      @returns(The allocated sound as TALSourceAllocator.

        Returns @nil when there were no resources to play another sound
        (and it wasn't important enough to override another sound).
        Always returns @nil when ALBuffer is zero (indicating that buffer
        was not loaded).

        In simple cases you can just ignore the result of this method.
        In advanced cases, you can use it to observe and update the sound
        later.) }
    function PlaySound(const ALBuffer: TALBuffer;
      const Spatial, Looping: boolean; const Importance: Cardinal;
      const Gain, MinGain, MaxGain: Single;
      const Position: TVector3Single): TALAllocatedSource;

    { Allocate sound for playing. You should initialize the OpenAL sound
      properties and start playing the sound (you have
      OpenAL sound identifier in TALAllocatedSource.ALSource).

      Note that if you don't call alSourcePlay, the source may be detected
      as unused (and recycled for another sound) at the next AllocateSound,
      PlaySound, RefreshUsedSources and such calls. }
    function AllocateSound(const Importance: Cardinal): TALAllocatedSource;

    { Detect unused sounds. If you rely on your sources receiving
      TALAllocatedSource.OnUsingEnd in a timely manner, be sure to call
      this method often. Otherwise, it's not needed to call this at all
      (unused sounds will be detected automatically on-demand anyway).

      See TALSourceAllocator.RefreshUsed for info.
      This silently ignored when not ALActive. }
    procedure RefreshUsedSources;

    { Stop all the sources currently playing. Especially useful since
      you have to stop a source before releasing it's associated buffer. }
    procedure StopAllSources;
  end;

var
  SoundEngine: TALSoundEngine;
  WasParam_NoSound: boolean = false;

implementation

uses KambiUtils, KambiStringUtils, ALUtils, KambiLog, ProgressUnit,
  SoundFile, VorbisFile, KambiTimeUtils;

constructor TALSoundEngine.Create;
begin
  inherited;

  FALMinAllocatedSources := DefaultALMinAllocatedSources;
  FALMaxAllocatedSources := DefaultALMaxAllocatedSources;
end;

procedure TALSoundEngine.ALContextOpen(const WasParam_NoSound: boolean);
begin
  Assert(not ALActive);

  if WasParam_NoSound then
    FSoundInitializationReport :=
      'Sound disabled by --no-sound command-line option' else
  if not TryBeginAL(false) then
    FSoundInitializationReport :=
      'OpenAL initialization failed : ' +ALActivationErrorMessage +nl+
      'SOUND IS DISABLED' else
  begin
    FSoundInitializationReport :=
      'OpenAL initialized, sound enabled';

    try
      SourceAllocator := TALSourceAllocator.Create(
        FALMinAllocatedSources, FALMaxAllocatedSources);
      CheckAL('initializing sounds (ALContextOpen)');
    except
      ALContextClose;
      raise;
    end;
  end;

  if Log then
    WritelnLogMultiline('Sound initialization',
      SoundInitializationReport + nl + ALInformation);
end;

procedure TALSoundEngine.ALContextClose;
begin
  if ALActive then
  begin
    FreeAndNil(SourceAllocator);

    { EndAL may take a while on Unix OpenAL, so provide feedback
      for user here (otherwise (s)he may think that program hanged). }
    Progress.Init(1, 'Closing sound device, please wait');
    try
      EndAL;
      Progress.Step;
    finally Progress.Fini; end;
  end;
end;

procedure TALSoundEngine.AppendALInformation(S: TStrings);
begin
  if ALActive then
  begin
    S.Append('');
    S.Append('Version : ' + alGetString(AL_VERSION));
    S.Append('Renderer : ' + alGetString(AL_RENDERER));
    S.Append('Vendor : ' + alGetString(AL_VENDOR));
    S.Append('Extensions : ' + alGetString(AL_EXTENSIONS));
    S.Append('');
    S.Append(Format('Allocated OpenAL sources: %d (min %d, max %d)',
      [ SourceAllocator.AllocatedSources.Count,
        SourceAllocator.MinAllocatedSources,
        SourceAllocator.MaxAllocatedSources ]));
    S.Append('');
    S.Append('OggVorbis handling method: ' + TSoundOggVorbis.VorbisMethod);
    S.Append('vorbisfile library available: ' + BoolToStr[VorbisFileInited]);
  end;
end;

function TALSoundEngine.ALInformation: string;
var
  S: TStringList;
begin
  S := TStringList.Create;
  try
    AppendALInformation(S);
    Result := S.Text;
  finally S.Free end;
end;

procedure TALSoundEngine.ALChangeDevice(const NewALCDevice: string);
begin
  ALContextClose;
  OpenALRestart;
  ALCDevice := NewALCDevice;
  ALContextOpen(false);
end;

function TALSoundEngine.GetALMinAllocatedSources: Cardinal;
begin
  Result := FALMinAllocatedSources;
end;

procedure TALSoundEngine.SetALMinAllocatedSources(const Value: Cardinal);
begin
  if Value <> FALMinAllocatedSources then
  begin
    FALMinAllocatedSources := Value;
    if SourceAllocator <> nil then
      SourceAllocator.MinAllocatedSources := FALMinAllocatedSources;
  end;
end;

function TALSoundEngine.GetALMaxAllocatedSources: Cardinal;
begin
  Result := FALMaxAllocatedSources;
end;

procedure TALSoundEngine.SetALMaxAllocatedSources(const Value: Cardinal);
begin
  if Value <> FALMaxAllocatedSources then
  begin
    FALMaxAllocatedSources := Value;
    if SourceAllocator <> nil then
      SourceAllocator.MaxAllocatedSources := FALMaxAllocatedSources;
  end;
end;

function TALSoundEngine.AllocateSound(const Importance: Cardinal): TALAllocatedSource;
begin
  if ALActive then
    Result := SourceAllocator.AllocateSource(Importance) else
    Result := nil;
end;

function TALSoundEngine.PlaySound(const ALBuffer: TALBuffer;
  const Spatial, Looping: boolean; const Importance: Cardinal;
  const Gain, MinGain, MaxGain: Single;
  const Position: TVector3Single): TALAllocatedSource;

  procedure alCommonSourceSetup(ALSource: TALuint);
  begin
    alSourcei(ALSource, AL_BUFFER, ALBuffer);
    alSourcei(ALSource, AL_LOOPING, BoolToAL[Looping]);
    alSourcef(ALSource, AL_GAIN, Gain);
    alSourcef(ALSource, AL_MIN_GAIN, MinGain);
    alSourcef(ALSource, AL_MAX_GAIN, MaxGain);

    if Spatial then
    begin
      { Set attenuation by distance. }
      alSourcef(ALSource, AL_ROLLOFF_FACTOR, 0.1);
      alSourcef(ALSource, AL_REFERENCE_DISTANCE, 2.0);

      alSourcei(ALSource, AL_SOURCE_RELATIVE, AL_FALSE);
      alSourceVector3f(ALSource, AL_POSITION, Position);
    end else
    begin
      { No attenuation by distance. }
      alSourcef(ALSource, AL_ROLLOFF_FACTOR, 0);

      { Although AL_ROLLOFF_FACTOR := 0 turns off
        attenuation by distance, we still have to turn off
        any changes from player's orientation (so that the sound
        is not played on left or right side, but normally).
        That's why setting source position exactly on the player
        is needed here. }
      alSourcei(ALSource, AL_SOURCE_RELATIVE, AL_TRUE);
      alSourceVector3f(ALSource, AL_POSITION, ZeroVector3Single);
    end;
  end;

const
  { For now, just always use CheckBufferLoaded. It doesn't seem to cause
    any slowdown for normal sound playing. }
  CheckBufferLoaded = true;
begin
  Result := nil;

  if ALActive and (ALBuffer <> 0) then
  begin
    Result := SourceAllocator.AllocateSource(Importance);
    if Result <> nil then
    begin
      alCommonSourceSetup(Result.ALSource);

      if CheckBufferLoaded then
      begin
        { This is a workaround needed on Apple OpenAL implementation
          (although I think that at some time I experienced similar
          problems (that would be cured by this workaround) on Linux
          (Loki OpenAL implementation)).

          The problem: music on some
          levels doesn't play. This happens seemingly random: sometimes
          when you load a level music starts playing, sometimes it's
          silent. Then when you go to another level, then go back to the
          same level, music plays.

          Investigation: I found that sometimes changing the buffer
          of the sound doesn't work immediately. Simple
            Writeln(SoundInfos.Items[PlayedSound].Buffer, ' ',
              alGetSource1ui(FAllocatedSource.ALSource, AL_BUFFER));
          right after alCommonSourceSetup shows this (may output
          two different values). Then if you wait a little, OpenAL
          reports correct buffer. This probably means that OpenAL
          internally finishes some tasks related to loading buffer
          into source. Whatever it is, it seems that it doesn't
          occur (or rather, is not noticeable) on normal game sounds
          that are short --- but it's noticeable delay with larger
          sounds, like typical music.

          So the natural workaround below follows. For OpenAL implementations
          that immediately load the buffer, this will not cause any delay. }
        while ALBuffer <> alGetSource1ui(Result.ALSource, AL_BUFFER) do
          Delay(10);
      end;

      alSourcePlay(Result.ALSource);
    end;
  end;
end;

procedure TALSoundEngine.RefreshUsedSources;
begin
  if SourceAllocator <> nil then
    SourceAllocator.RefreshUsed;
end;

function TALSoundEngine.LoadBuffer(const FileName: string): TALBuffer;
begin
  { TODO: for now, no cache }
  Result := TALSoundFile.alCreateBufferDataFromFile(FileName);
end;

procedure TALSoundEngine.FreeBuffer(var Buffer: TALBuffer);
begin
  { TODO: for now, no cache.
    TODO: Also, invalid buffer will cause OpenAL error.
    TODO: also, remaining buffers not freed at exit. }
  alFreeBuffer(Buffer);
end;

procedure TALSoundEngine.StopAllSources;
begin
  SourceAllocator.StopAllSources;
end;

end.
