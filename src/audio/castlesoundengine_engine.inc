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

  { Sound engine, responsible for loading and playing sounds.
    See @url(https://castle-engine.io/manual_sound.php
    manual about sound in Castle Game Engine) for an overview of sound support.

    There should always be only one instance of this class,
    accessed through the global @link(SoundEngine) routine. }
  TSoundEngine = class(TSoundAllocator)
  private
    type
      TInternalSoundBuffersList = {$ifdef FPC}specialize{$endif} TObjectList<TInternalSoundBuffer>;
    var
      FInformation: string;
      FInformationSummary: string;
      FDevice: string;
      FVolume: Single;
      FEnabled: boolean;
      FIsContextOpen: boolean;
      FDistanceModel: TSoundDistanceModel;
      LoadedBuffers: TInternalSoundBuffersList;
      FDevices: TSoundDeviceList;
      FOnOpenClose: TNotifyEventList;
      FResumeToInitialized, FPaused: boolean;
      FListenerPosition, FListenerDirection, FListenerUp: TVector3;
      FDopplerFactor: Single;

    procedure SetVolume(const Value: Single);
    procedure SetDistanceModel(const Value: TSoundDistanceModel);
    procedure SetDevice(const Value: String);
    procedure SetEnabled(const Value: boolean);
    procedure SetPaused(const Value: boolean);
    procedure ReinitializeJavaActivity(Sender: TObject);
    procedure ApplicationPause(Sender: TObject);
    procedure ApplicationResume(Sender: TObject);
    { Pause the sound engine, useful when Android activity gets inactive.
      When paused, sound backend is for sure inactive, and it cannot be activated
      (calling ContextOpen, or playing a sound, will @bold(not) activate it). }
    property Paused: boolean read FPaused write SetPaused;
    procedure SetInternalBackend(const Value: TSoundEngineBackend);
    procedure SetDopplerFactor(const Value: Single);

    procedure ContextOpenCore; override;
    procedure ContextCloseCore; override;

    class function GetLogSoundLoading: Boolean; static;
    class procedure SetLogSoundLoading(const Value: Boolean); static;

    { Load a sound file contents such that they can be immediately played.

      This method tries to initialize backend (like OpenAL) context,
      and loads the buffer contents. But even when it fails, it still returns
      a valid (non-nil) TInternalSoundBuffer instance. As the OpenAL context
      may be lost while the game is ongoing (in case of Android) we take care
      to reload it when necessary.

      The buffer should be released by @link(FreeBuffer) later when it's not needed.
      Although we will take care to always free remaining buffers
      before closing OpenAL context anyway.

      We have a cache of sound files here. An absolute URL
      will be recorded as being loaded to given buffer. Loading the same
      URL second time returns the same buffer instance. The buffer
      is released only once you call @link(FreeBuffer) as many times as you called
      LoadBuffer for it.

      @param(ExceptionOnError When to raise exception,
        or merely make a warning by WritelnWarning,
        in case of error when loading.

        Note that it always returns non-nil, even in case of ExceptionOnError=@false
        and an error at loading. But in this case, TInternalSoundBuffer is mostly a useless
        empty instance. Do not depend on this behavior, and check for @nil
        result in case you use ExceptionOnError=@false.
      )

      @param(SoundLoading Specifies whether to load sound at once, or use streaming.
        See @link(TSoundLoading) for details.)

      @raises(ESoundFileError If loading of this sound file failed.
        There are many reasons why this may happen: we cannot read given URL,
        or it may contain invalid contents,
        or a library required to decompress e.g. OggVorbis is missing.)

      @groupBegin }
    function LoadBuffer(const Url: String; const SoundLoading: TSoundLoading;
      const ExceptionOnError: Boolean = true): TInternalSoundBuffer; overload;
    function LoadBuffer(const Url: String;
      const ExceptionOnError: Boolean = true): TInternalSoundBuffer; overload;
    function LoadBuffer(const Url: String; out Duration: TFloatTime): TInternalSoundBuffer;
      overload;
    { @groupEnd }

    { Free a sound file buffer. Ignored when buffer is @nil.
      Buffer is always set to @nil after this.

      @raises(ESoundBufferNotLoaded When invalid (not @nil,
        and not returned by LoadBuffer) buffer identifier is given.) }
    procedure FreeBuffer(var Buffer: TInternalSoundBuffer);

    { Play a sound from given buffer.

      We use a smart sound allocator, so the sound will be actually
      played only if resources allow. Use higher priority to indicate
      sounds that are more important to play.

      We set the sound properties and start playing it.

      Both spatialized (3D) and not spatialized sounds are possible.
      See the @link(TPlaySoundParameters) for a full list of sound parameters.
      You can pass all the sound parameters as a @link(TPlaySoundParameters) instance.
      You can destroy the @link(TPlaySoundParameters) instance right after calling
      this method, the reference to it is not saved anywhere.

      @returns(The allocated sound as TInternalSoundSource.

        Returns @nil when there were no resources to play another sound
        (and it wasn't important enough to override another sound).
        Always returns @nil when SoundBuffer is zero (indicating that buffer
        was not loaded).

        In simple cases you can just ignore the result of this method.
        In advanced cases, you can use it to observe and update the sound
        later.)

      @exclude
    }
    function PlaySound(const Parameters: TPlaySoundParameters): TInternalSoundSource;
  public
    const
      DefaultVolume = 1.0;
      DefaultDistanceModel = dmInverse;
      DefaultDevice = '';
      DefaultEnabled = true;

    class var
      LogVerbose: Boolean;

    constructor Create;
    destructor Destroy; override;

    { Initialize sound backend.
      Sets @link(IsContextOpen), @link(IsContextOpenSuccess), @link(Information).

      You don't usually need to call this -- everything will be initialized
      automatically when you first load a sound (or try to load and play it).

      You can set @link(Device) before calling this.

      Note that we continue (without any exception) if the initialization
      failed for any reason (e.g. OpenAL library is not available,
      or no sound output device is available).
      You can check @link(IsContextOpenSuccess) and @link(Information) to know if
      the initialization was actually successful. But you can also ignore it,
      the sound engine will silently (literally) keep working even if OpenAL
      could not be initialized. }
    procedure ContextOpen;

    { Close the sound backend, releasing all sound resources and stopping playback.
      This sets @link(IsContextOpen) and @link(IsContextOpenSuccess) to @false.
      It's allowed and harmless to call this when one of them is already @false.

      You don't usually need to call this -- everything will be released
      automatically when the sound engine is destroyed (at program finalization). }
    procedure ContextClose;

    { Sound backend, like OpenAL or FMOD or SOX.
      See @url(https://castle-engine.io/openal OpenAL) or
      @url(https://castle-engine.io/fmod FMOD).

      Do not change or access this yourself.
      You can change this only by calling procedure like UseFMODSoundBackend
      from CastleFMODSoundBackend unit. }
    property InternalBackend: TSoundEngineBackend read Backend write SetInternalBackend;

    { Did we attempt to initialize sound rendering context.
      This indicates that ContextOpen was called,
      and not closed with ContextClose yet.
      Contrary to IsContextOpenSuccess,
      this @italic(doesn't care if ContextOpen was a success). }
    property IsContextOpen: boolean read FIsContextOpen;

    { Multiline information about the currently initialized
      sound backend (OpenAL, FMOD etc.). }
    property Information: string read FInformation;

    { One-line information about the currently initialized
      sound backend (OpenAL, FMOD etc.). }
    property InformationSummary: string read FInformationSummary;

    { Play given sound once (not looping).

      This is the simplest method to just play the sound and "forget about it"
      (no need to manage the playback afterwards).

      It is allowed to set ASound to @nil. Nothing will be played then.

      Note: Instead of this method, use @code(Play(TCastlePlayingSound))
      overload to have more control over the sound
      (before and after it starts playing).

      This only plays non-spatial sounds, as it doesn't give any control over
      the sound 3D position.
      Instead of this method, use TCastleSoundSource
      (attached to some TCastleTransform) for spatial sounds to control their 3D position. }
    procedure Play(const ASound: TCastleSound); overload;

    { Play given sound.

      Set @link(TCastlePlayingSound.Sound) before passing it to this method.
      Otherwise, if @link(TCastlePlayingSound.Sound) is left @nil, nothing will be played.

      Use the PlayingSound (TCastlePlayingSound) properties to provide additional information
      about the sound playback.
      E.g. assign @link(TCastlePlayingSound.Loop)
      @link(TCastlePlayingSound.OnStop) or control sound afterwards by
      @link(TCastlePlayingSound.Stop), @link(TCastlePlayingSound.Offset).

      Consider using @link(TCastlePlayingSound.FreeOnStop) if you don't want
      to manage the lifetime of the @link(TCastlePlayingSound) instance.

      This only plays non-spatial sounds, as it doesn't give any control over
      the sound 3D position.
      Instead of this method, use TCastleSoundSource
      (attached to some TCastleTransform) for spatial sounds to control their 3D position. }
    procedure Play(const PlayingSound: TCastlePlayingSound); overload;

    { Internal, a bit more flexible, variant of @link(Play). @exclude }
    procedure InternalPlay(const PlayingSound: TCastlePlayingSound;
      const ASpatial: Boolean; const APosition: TVector3);

    { Parse parameters in @link(Parameters) and interpret and remove
      recognized options. Internally it uses Parameters.Parse with
      ParseOnlyKnownLongOptions = @true. Recognized options:

      @definitionList(
        @itemLabel @--audio-device DEVICE-NAME
        @item Set @link(Device) variable to given argument.

        @itemLabel @--no-sound
        @item Disable any sound (sets @link(Enabled) to @false).
      )

      More user-oriented documentation for the above options is here:
      [https://castle-engine.io/openal_notes.php#section_options] }
    procedure ParseParameters;

    { Help string for options parsed by ParseParameters.

      Note that it also lists the available sound output @link(Devices),
      as they are valid arguments for the @--audio-device option. }
    function ParseParametersHelp: string;

    { Set the sound listener position and orientation. @exclude }
    procedure InternalUpdateListener(const Position, Direction, Up: TVector3);

    { List of available sound devices. Read-only.

      Use @code(Devices[].Name) as @link(Device) values.

      On some backend implementations, also some other @link(Device) values may
      be possible. E.g. old Loki implementation of OpenAL allowed some hints
      to be encoded in Lisp-like language inside the @link(Device) string. }
    function Devices: TSoundDeviceList;

    function DeviceCaption: string;

    { Events fired after sound context is being open or closed.
      More precisely, when IsContextOpen changes (and so, possibly, IsContextOpenSuccess
      changed). }
    property OnOpenClose: TNotifyEventList read FOnOpenClose;

    class property LogSoundLoading: Boolean
      read GetLogSoundLoading write SetLogSoundLoading;

    { Sound volume, affects all sounds (effects and music).
      Must be within 0..1 range.

      Setting volume to 0.0 means that there is no sound output
      (this case should be optimized). The sounds played are still tracked. }
    property Volume: Single read FVolume write SetVolume
      {$ifdef FPC}default DefaultVolume{$endif};

    { Sound output device, used when initializing sound context.

      You can change it even when context is already initialized.
      Then we'll close the old device (ContextClose),
      change @link(Device) value, and initialize context again (ContextOpen).
      Note that you will need to reload your buffers and sources again. }
    property Device: string read FDevice write SetDevice;

    { Enable sound. Value @true, the default, means that we will initialize
      sound backend and play all sounds as you expect.

      If this is @false, then we will not initialize any sound backend even
      if you try to load or play some sound.
      This is useful if you simply want to disable any sound output
      (or backend, like OpenAL, usage), even when sound library (like OpenAL)
      is available.

      Setting this to @false when the sound backend
      is already initialized will close it, stopping the playback of
      all existing sound sources.

      Note that when sound backend is not initialized,
      sounds are not tracked, and non-looping sounds are just discarded.
      So toggling this property between @false and @true
      will simply restart all looping sounds from the beginning.
      So setting this to @false is not a way to "pause" or "mute" sound playback,
      it's a way to "stop" sound playback.
      If you want to mute all sounds, but be able to unmute them when necessary,
      set the @link(Volume) to zero. }
    property Enabled: boolean read FEnabled write SetEnabled default DefaultEnabled;

    { How does the distance affect spatial sounds (with @link(TCastleSoundSource.Spatial)).
      See TSoundDistanceModel for the description of possible options. }
    property DistanceModel: TSoundDistanceModel
      read FDistanceModel write SetDistanceModel {$ifdef FPC}default DefaultDistanceModel{$endif};

    { Emphasize or deemphasize the Doppler effect.
      See https://en.wikipedia.org/wiki/Doppler_effect and CGE demo
      examples/audio/doppler_effect to see what it is.

      Allowed values are anything > 0.
      Value 0 disables the Doppler effect.
      This is the default, otherwise the Doppler effect would be way too audible in 2D games.
      Value 1 makes a correct Doppler effect if the distance in your world of 1 unit
      corresponds to 1 meter.
      Larger values allow to emphasize the effect more. }
    property DopplerFactor: Single read FDopplerFactor write SetDopplerFactor {$ifdef FPC}default 0.0{$endif};
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TSoundEngine --------------------------------------------------------------- }

constructor TSoundEngine.Create;
begin
  inherited;
  FVolume := DefaultVolume;
  FDistanceModel := DefaultDistanceModel;
  FDopplerFactor := 0.0;
  FEnabled := DefaultEnabled;
  FDevice := DefaultDevice;
  LoadedBuffers := TInternalSoundBuffersList.Create(true);
  FOnOpenClose := TNotifyEventList.Create;

  { Default listener attributes }
  FListenerPosition := TVector3.Zero;
  FListenerDirection := Vector3(0, 0, -1);
  FListenerUp := Vector3(0, 1, 0);

  ApplicationProperties.OnInitializeJavaActivity.Add(
    {$ifdef FPC}@{$endif} ReinitializeJavaActivity);
  ApplicationProperties.OnPause.Add(
    {$ifdef FPC}@{$endif} ApplicationPause);
  ApplicationProperties.OnResume.Add(
    {$ifdef FPC}@{$endif} ApplicationResume);
end;

destructor TSoundEngine.Destroy;
begin
  if ApplicationProperties(false) <> nil then
  begin
    ApplicationProperties(false).OnInitializeJavaActivity.Remove(
      {$ifdef FPC}@{$endif} ReinitializeJavaActivity);
    ApplicationProperties(false).OnPause.Remove(
      {$ifdef FPC}@{$endif} ApplicationPause);
    ApplicationProperties(false).OnResume.Remove(
      {$ifdef FPC}@{$endif} ApplicationResume);
  end;

  ContextClose;

  Inc(ValidSoundBufferFree);
  try
    FreeAndNil(LoadedBuffers);
  finally Dec(ValidSoundBufferFree) end;

  FreeAndNil(FDevices);
  FreeAndNil(FOnOpenClose);
  inherited;
end;

function TSoundEngine.Devices: TSoundDeviceList;
begin
  { Create devices on demand (not immediately in TSoundEngine.Create),
    because in case of OpenAL merely using alcGetString(nil, ALC_DEVICE_SPECIFIER)
    may perform some OpenAL initialization (discovery of available devices).
    E.g. with OpenAL Soft 1.13 in Debian. This is not very harmful,
    but it causes output (on stdout or stderr) like

      AL lib: pulseaudio.c:612: Context did not connect: Connection refused
      ALSA lib pcm.c:2190:(snd_pcm_open_noupdate) Unknown PCM cards.pcm.rear
      ALSA lib pcm.c:2190:(snd_pcm_open_noupdate) Unknown PCM cards.pcm.center_lfe
      ALSA lib pcm.c:2190:(snd_pcm_open_noupdate) Unknown PCM cards.pcm.side
      ALSA lib pcm_dmix.c:957:(snd_pcm_dmix_open) The dmix plugin supports only playback stream

    and it causes a temporary slowdown. So we want to defer this (until really
    needed, or until explicit ContextOpen call). }

  if FDevices = nil then
  begin
    FDevices := TSoundDeviceList.Create;
    FDevices.Add('', 'Default Device');
    Backend.DetectDevices(FDevices);
  end;
  Result := FDevices;
end;

procedure TSoundEngine.ContextOpenCore;

  { initialize resources inside LoadedBuffers }
  procedure LoadedBuffersOpen;
  var
    Buffer: TInternalSoundBuffer;
  begin
    // check LoadedBuffers.Count, because we don't want to do progress with no LoadedBuffers
    if LoadedBuffers.Count <> 0 then
    begin
      { call ContextOpen on all buffers }
      for Buffer in LoadedBuffers do
        Buffer.ContextOpen(false);
    end;
  end;

var
  BackendOpenInformation, BackendOpenInformationSummary: String;
begin
  Assert(not IsContextOpenSuccess, 'Sound context is already active');

  if not Enabled then
  begin
    FInformation := 'Sound initialization aborted: Sound engine is disabled';
    FInformationSummary := 'Disabled';
  end else
  begin
    FIsContextOpenSuccess := Backend.ContextOpen(FDevice, BackendOpenInformation, BackendOpenInformationSummary);
    if not IsContextOpenSuccess then
    begin
      FInformation := 'Sound backend initialization failed:' + NL +
        BackendOpenInformation;
      FInformationSummary := 'Failed: ' + BackendOpenInformationSummary;
    end else
    begin
      FInformation :=
        'Sound backend initialized successfully:' + NL +
        BackendOpenInformation + NL +
        NL+
        Format('Allocated sound sources: min %d, max %d' + NL +
          NL+
          'Library to decode OggVorbis available: %s', [
            MinAllocatedSources, MaxAllocatedSources,
            BoolToStr(VorbisFileInitialized, true)
          ]);
      FInformationSummary := 'Initialized: ' + BackendOpenInformationSummary;

      try
        Backend.SetVolume(Volume);
        Backend.SetDistanceModel(DistanceModel);
        Backend.SetDopplerFactor(DopplerFactor);
        Backend.SetListener(
          FListenerPosition,
          FListenerDirection,
          FListenerUp);
        inherited; { initialize sound allocator }
        LoadedBuffersOpen;
      except
        ContextClose;
        raise;
      end;
    end;
  end;

  if LogVerbose then
    WritelnLogMultiline('Sound', Information)
  else
    WritelnLog('Sound', InformationSummary + ' (for more info: TSoundEngine.LogVerbose:=true)');

  OnOpenClose.ExecuteAll(Self);
end;

procedure TSoundEngine.ContextCloseCore;
var
  Buffer: TInternalSoundBuffer;
begin
  if IsContextOpenSuccess then
  begin
    { release sound allocator first. This also stops all the sources,
      which is required before we try to release their buffers. }
    inherited;
    { free backend resources allocated inside LoadedBuffers }
    for Buffer in LoadedBuffers do
      Buffer.ContextClose;
    Backend.ContextClose;
    FIsContextOpenSuccess := false;
  end;

  WritelnLog('Sound', 'Sound backend closed');

  OnOpenClose.ExecuteAll(Self);
end;

procedure TSoundEngine.ContextOpen;
begin
  if Paused then
    Exit; // do not even set IsContextOpen to true

  if not IsContextOpen then
  begin
    FIsContextOpen := true; // set it early, so that OnOpenClose knows it's true
    ContextOpenCore;
  end;
end;

procedure TSoundEngine.ContextClose;
begin
  if IsContextOpen then
  begin
    FIsContextOpen := false; // set it early, so that OnOpenClose knows it's false
    ContextCloseCore;
  end;
end;

function TSoundEngine.PlaySound(const Parameters: TPlaySoundParameters): TInternalSoundSource;
begin
  Result := nil;

  { Open context, if not open yet.
    In general loading a buffer (LoadBuffer) should cause this already,
    and loading a buffer should happen before this call (since you need to initialize Parameters.Buffer),
    so usually calling ContextOpen here does nothing.

    But in case you later close the audio context, and want to reopen it at PlaySound
    -- we should call ContextOpen here.
    Doing ContextOpen also calls TInternalSoundBuffer.ContextOpen on all open buffers. }
  ContextOpen;

  if IsContextOpenSuccess and
     (Parameters.Buffer <> nil) and
     { Buffer.BackendIsOpen may be false if file failed to load, e.g. file not found }
     Parameters.Buffer.BackendIsOpen then
  begin
    Result := AllocateSound(Parameters.Priority);
    if Result <> nil then
    begin
      Result.Buffer  := Parameters.Buffer;
      Result.Loop    := Parameters.Loop;
      Result.Pitch   := Parameters.Pitch;
      Result.Volume  := Parameters.Volume;
      {$warnings off} // using unportable symbols knowingly
      Result.MinGain := Parameters.MinGain;
      Result.MaxGain := Parameters.MaxGain;
      {$warnings on}
      Result.Offset  := Parameters.Offset;
      Result.Spatial := Parameters.Spatial;

      Result.Position := Parameters.Position; // will be ignored when Spatial = false, which is OK
      { Set default attenuation by distance. }
      Result.ReferenceDistance := Parameters.ReferenceDistance; // will be ignored when Spatial = false, which is OK
      Result.MaxDistance       := Parameters.MaxDistance; // will be ignored when Spatial = false, which is OK

      Result.Backend.Play(true);
    end;
  end;
end;

procedure TSoundEngine.Play(const ASound: TCastleSound);
var
  PlayingSound: TCastlePlayingSound;
begin
  PlayingSound := TCastlePlayingSound.Create(nil);
  PlayingSound.Sound := ASound;
  PlayingSound.FreeOnStop := true;
  Play(PlayingSound);

  { Note: we could make this a function and return TCastlePlayingSound.
    But it could be risky for user not aware that (s)he @italic(must)
    change FreeOnStop := false, otherwise TCastlePlayingSound instance
    could be freed at any point in the future. }
end;

procedure TSoundEngine.Play(const PlayingSound: TCastlePlayingSound);
begin
  InternalPlay(PlayingSound, false, { APosition doesn't matter when spatial = false } TVector3.Zero);
end;

procedure TSoundEngine.InternalPlay(const PlayingSound: TCastlePlayingSound;
  const ASpatial: Boolean; const APosition: TVector3);
var
  ASound: TCastleSound;
  Params: TPlaySoundParameters;
  Source: TInternalSoundSource;
begin
  Assert(PlayingSound <> nil);
  ASound := PlayingSound.Sound;

  if ASound = nil then
  begin
    // sound cannot be played, as nil
    PlayingSound.DoStop; // apply PlayingSound.OnStop, FreeOnStop
    Exit;
  end;

  if ASound.InternalBuffer = nil then
  begin
    // sound cannot be played, as not loaded
    PlayingSound.DoStop; // apply PlayingSound.OnStop, FreeOnStop
    Exit;
  end;

  { TODO: This should not use
    - TPlaySoundParameters
    - PlaySound
    We should remove (only private now) TPlaySoundParameters and PlaySound.
    Other classes, like TCastleSound and TCastlePlayingSound,
    with simpler API, carry all information now.
  }

  Params := TPlaySoundParameters.Create;
  try
    Params.Buffer     := ASound.InternalBuffer;
    Params.Spatial    := ASpatial;
    Params.Position   := APosition;
    Params.Loop       := PlayingSound.Loop;
    Params.Priority   := ASound.Priority;
    Params.Pitch      := PlayingSound.InternalFinalPitch;
    Params.Volume     := PlayingSound.InternalFinalVolume;
    {$warnings off} // using unportable symbols knowingly
    Params.MinGain    := ASound.MinGain;
    Params.MaxGain    := ASound.MaxGain;
    {$warnings on}
    Params.Offset     := PlayingSound.InitialOffset;
    Params.ReferenceDistance := ASound.ReferenceDistance; // will be ignored when Spatial = false, which is OK
    Params.MaxDistance       := ASound.MaxDistance; // will be ignored when Spatial = false, which is OK

    Source := PlaySound(Params);

    if Source <> nil then
    begin
      PlayingSound.Start(Source);
    end else
    begin
      // sound cannot be played, as no free TInternalSoundSource slot
      PlayingSound.DoStop; // apply PlayingSound.OnStop, FreeOnStop
    end;
  finally FreeAndNil(Params) end;
end;

function TSoundEngine.LoadBuffer(const Url: String; out Duration: TFloatTime): TInternalSoundBuffer;
begin
  Result := LoadBuffer(Url);
  Duration := Result.Duration;
end;

function TSoundEngine.LoadBuffer(const Url: String; const SoundLoading: TSoundLoading; const ExceptionOnError: Boolean): TInternalSoundBuffer;
var
  I: Integer;
  FullUrl: String;
begin
  ContextOpen;

  FullUrl := AbsoluteURI(Url);

  { try to load from cache Result }
  for I := 0 to LoadedBuffers.Count - 1 do
    if LoadedBuffers[I].Url = FullUrl then
    begin
      Result := LoadedBuffers[I];
      Inc(Result.References);
      if LogSoundLoading then
        WritelnLog('Sound', Format('Loaded sound buffer "%s" from cache, now it has %d references',
          [UriDisplay(FullUrl), Result.References]));
      Exit;
    end;

  Result := TInternalSoundBuffer.Create(Backend, SoundLoading);
  Result.FUrl := FullUrl;
  Result.References := 1;
  LoadedBuffers.Add(Result);

  if IsContextOpenSuccess then
    { let LoadBuffer raise exception on missing sound file }
    Result.ContextOpen(ExceptionOnError);
end;

function TSoundEngine.LoadBuffer(const Url: String; const ExceptionOnError: Boolean): TInternalSoundBuffer;
begin
  Result := LoadBuffer(Url, slComplete, ExceptionOnError);
end;

procedure TSoundEngine.FreeBuffer(var Buffer: TInternalSoundBuffer);
var
  I: Integer;
begin
  if Buffer = nil then Exit;

  I := LoadedBuffers.IndexOf(Buffer);
  if I <> -1 then
  begin
    Dec(Buffer.References);
    if Buffer.References = 0 then
    begin
      // this will free Buffer, also calling Buffer.ContextClose;
      Inc(ValidSoundBufferFree);
      try
        LoadedBuffers.Delete(I);
      finally Dec(ValidSoundBufferFree) end;
    end;
    Buffer := nil;
  end else
    raise ESoundBufferNotLoaded.CreateFmt('Sound buffer "%s" not loaded by this sound engine',
      [UriDisplay(Buffer.Url)]);
end;

procedure TSoundEngine.SetVolume(const Value: Single);
begin
  if Value <> FVolume then
  begin
    FVolume := Value;
    if IsContextOpenSuccess then
      Backend.SetVolume(Value);
  end;
end;

procedure TSoundEngine.SetDistanceModel(const Value: TSoundDistanceModel);
begin
  if Value <> FDistanceModel then
  begin
    FDistanceModel := Value;
    if IsContextOpenSuccess then
      Backend.SetDistanceModel(Value);
  end;
end;

procedure TSoundEngine.SetDopplerFactor(const Value: Single);
begin
  if FDopplerFactor <> Value then
  begin
    FDopplerFactor := Value;
    if IsContextOpenSuccess then
      Backend.SetDopplerFactor(Value);
  end;
end;

procedure TSoundEngine.SetDevice(const Value: String);
begin
  if Value <> FDevice then
  begin
    if IsContextOpen then
    begin
      ContextClose;
      FDevice := Value;
      ContextOpen;
    end else
      FDevice := Value;
  end;
end;

procedure TSoundEngine.SetEnabled(const Value: boolean);
begin
  if Value <> FEnabled then
  begin
    if IsContextOpen then
    begin
      ContextClose;
      FEnabled := Value;
      ContextOpen;
    end else
      FEnabled := Value;
  end;
end;

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
var
  Engine: TSoundEngine;
begin
  Engine := TSoundEngine(Data);
  case OptionNum of
    0: begin
         Engine.Device := Argument;
       end;
    1: begin
         Engine.Enabled := false;
       end;
    else raise EInternalError.Create('CastleSoundEngine.OptionProc');
  end;
end;

procedure TSoundEngine.ParseParameters;
const
  SoundOptions: array [0..1] of TOption =
  ( (Short: #0; Long: 'audio-device'; Argument: oaRequired),
    (Short: #0; Long: 'no-sound'; Argument: oaNone)
  );
begin
  Parameters.Parse(SoundOptions, @OptionProc, Self, true);
end;

function TSoundEngine.ParseParametersHelp: string;

  function DevicesHelp: string;
  var
    I: Integer;
  begin
    Result := Format('                        Available devices (%d):', [Devices.Count]) + NL;
    for i := 0 to Devices.Count - 1 do
    begin
      Result := Result + '                          ' + Devices[i].Caption;
      if Devices[i].Name <> Devices[i].Caption then
        Result := Result + ' (internal name: "' + Devices[i].Name + '")';
      Result := Result + NL;
    end;
  end;

begin
  Result :=
    '  --audio-device DEVICE-NAME' +nl+
    '                        Choose sound output device.' +nl+
    DevicesHelp +
    '  --no-sound            Turn off sound.';
end;

procedure TSoundEngine.InternalUpdateListener(const Position, Direction, Up: TVector3);
begin
  FListenerPosition := Position;
  FListenerDirection := Direction;
  FListenerUp := Up;
  if IsContextOpenSuccess then
    Backend.SetListener(Position, Direction, Up);
end;

function TSoundEngine.DeviceCaption: string;
var
  I: Integer;
begin
  for I := 0 to Devices.Count - 1 do
    if Devices[I].Name = Device then
      Exit(Devices[I].Caption);

  Result := 'Unnamed Sound Device'; // some default
end;

procedure TSoundEngine.ReinitializeJavaActivity(Sender: TObject);
begin
  { in case Java activity got killed and is created again, be sure to resume }
  Paused := false;
end;

procedure TSoundEngine.ApplicationPause(Sender: TObject);
begin
  Paused := true;
end;

procedure TSoundEngine.ApplicationResume(Sender: TObject);
begin
  Paused := false;
end;

procedure TSoundEngine.SetPaused(const Value: boolean);
begin
  if FPaused <> Value then
  begin
    FPaused := Value;
    if FPaused then
    begin
      FResumeToInitialized := IsContextOpen;
      ContextClose;
    end else
    begin
      if FResumeToInitialized then
        ContextOpen;
    end;
  end;
end;

procedure TSoundEngine.SetInternalBackend(const Value: TSoundEngineBackend);
begin
  ContextClose;
  FreeAndNil(Backend);
  Backend := Value;
end;

class function TSoundEngine.GetLogSoundLoading: Boolean;
begin
  Result := CastleInternalSoundFile.LogSoundLoading;
end;

class procedure TSoundEngine.SetLogSoundLoading(const Value: Boolean);
begin
  CastleInternalSoundFile.LogSoundLoading := Value;
end;

{$endif read_implementation}
