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

{ Cross-platform, spatial sound playback engine (@link(SoundEngine) singleton). }
unit CastleSoundEngine;

{$I castleconf.inc}

{$ifdef CASTLE_NINTENDO_SWITCH}
  // Nintendo Switch has different default backend
{$else}
  { Full-featured backend using OpenAL. }
  {$define CASTLE_SOUND_BACKEND_DEFAULT_OPENAL}
{$endif}

interface

uses SysUtils, Classes, Math, Generics.Collections, DOM,
  CastleVectors, CastleTimeUtils, CastleClassUtils, CastleStringUtils,
  CastleSoundBase, CastleInternalSoundFile, CastleInternalAbstractSoundBackend,
  CastleXMLConfig;

type
  ENoMoreSources = CastleSoundBase.ENoMoreSources;
  ENoMoreOpenALSources = ENoMoreSources deprecated 'use ENoMoreSources';
  ESoundBufferNotLoaded = class(Exception);
  EInvalidSoundBufferFree = class(Exception);
  ESoundFileError = CastleInternalSoundFile.ESoundFileError;
  EInvalidSoundRepositoryXml = class(Exception);

  TSound = class;
  TSoundAllocator = class;

  { Sound buffer represents contents of a sound file, like Wav or OggVorbis,
    that (may be) played.

    It can be only allocated by @link(TSoundEngine.LoadBuffer)
    and freed by @link(TSoundEngine.FreeBuffer).
    @bold(Do not create or free TSoundBuffer instances yourself.) }
  TSoundBuffer = class
  private
    FURL: string;
    FDuration: TFloatTime;
    FDataFormat: TSoundDataFormat;
    FFrequency: LongWord;
    References: Cardinal;
    Backend: TSoundBufferBackend;
    BackendIsOpen: Boolean;
    procedure ContextOpen(const ExceptionOnError: boolean);
    procedure ContextClose;
  public
    constructor Create(const SoundEngineBackend: TSoundEngineBackend);
    destructor Destroy; override;

    { Duration of the sound, in seconds. Zero if not loaded yet. }
    property Duration: TFloatTime read FDuration;

    { Absolute sound file URL.
      Never empty (do not create TSoundBuffer instances for invalid / empty URL,
      like the ones that can be created by TRepoSoundEngine for not defined sounds.) }
    property URL: string read FURL;

    { Data format (bits per sample, stereo or mono) of the loaded sound file.
      Typical applications don't need this value, this is just an information
      about the loaded sound file.
      Undefined if not loaded yet. }
    property DataFormat: TSoundDataFormat read FDataFormat;

    { Frequency (sample rate) of the loaded sound file.
      Typical applications don't need this value, this is just an information
      about the loaded sound file.
      Undefined if not loaded yet. }
    property Frequency: LongWord read FFrequency;
  end;

  TSoundEvent = procedure (Sender: TSound) of object;

  { Sound source that can be immediately played. }
  TSound = class
  private
    FUsed: boolean;
    FOnRelease: TSoundEvent;
    FImportance: Integer;
    FUserData: TObject;
    FPosition, FVelocity: TVector3;
    FLooping, FRelative: boolean;
    FGain, FMinGain, FMaxGain, FPitch: Single;
    FBuffer: TSoundBuffer;
    FRolloffFactor, FReferenceDistance, FMaxDistance: Single;
    Backend: TSoundSourceBackend;
    BackendIsOpen: Boolean;
    procedure SetPosition(const Value: TVector3);
    procedure SetVelocity(const Value: TVector3);
    procedure SetLooping(const Value: boolean);
    procedure SetRelative(const Value: boolean);
    procedure SetGain(const Value: Single);
    procedure SetMinGain(const Value: Single);
    procedure SetMaxGain(const Value: Single);
    procedure SetBuffer(const Value: TSoundBuffer);
    procedure SetPitch(const Value: Single);
    procedure SetRolloffFactor(const Value: Single);
    procedure SetReferenceDistance(const Value: Single);
    procedure SetMaxDistance(const Value: Single);
    function GetOffset: Single;
    procedure SetOffset(const Value: Single);

    { Call backend Update on sound source,
      and checks whether the source is still playing (or is paused).
      If not, it calls @link(TSound.Release) (thus setting
      TSound.Used to @false and triggering TSound.OnRelease) for this source. }
    procedure Update(const SecondsPassed: TFloatTime);
  public
    { Create sound.
      This allocates sound source using the sound backend (like OpenAL source).
      @raises(ENoMoreSources If no more sources available.
        It should be caught and silenced by TSoundAllocator.AllocateSound.) }
    constructor Create(const SoundEngineBackend: TSoundEngineBackend);
    destructor Destroy; override;

    { Do we play something.
      Sources that are not Used are still allocated on the sound backend (like
      OpenAL), and will be used when we will need them. }
    property Used: boolean read FUsed default false;

    { The priority of keeping this source, relevant only when @link(Used).

      Higher Importance means that it's more important to keep it.
      (I didn't name this property "Priority" so that it's obvious
      that higher Importance means more important sound). }
    property Importance: Integer read FImportance default 0;

    { Any data comfortable to keep here by the caller of
      TSoundAllocator.AllocateSound. It should be initialized
      after calling TSoundAllocator.AllocateSound, and should
      be finalized in OnRelease. }
    property UserData: TObject read FUserData write FUserData;

    { Called when this sound will no longer be used.
      This may happen because it simply finished playing,
      or when there are more demanding
      sounds (see @link(Importance) and to keep MaxAllocatedSources)
      and we must use this sound source for something else.

      When this event occurs, you should forget (e.g. set to @nil) all
      your references to this sound instance. That's because this TSound instance
      may be freed (or reused for other sounds) after calling OnRelease.
      For the same reason, right after calling this event, we always clear it
      (set OnRelease to @nil).

      It's guaranteed that when this will be called,
      @link(Used) will be @false and @link(PlayingOrPaused) will be @false.

      Note that we do not guarantee that sources that
      stopped playing will be immediately reported to OnRelease.
      A source may have Used = @true state
      for a short time when it stopped playing (when PlayingOrPaused
      is already @false). }
    property OnRelease: TSoundEvent read FOnRelease write FOnRelease;

    { Stops playing the source,
      sets Used to @false, and calls OnRelease (if assigned).

      You can call this if you want to stop playing the sound.
      This will also immediately set Used property
      to @false and will call OnRelease.

      You can call this only when Used = @true. }
    procedure Release; virtual;

    property Position: TVector3 read FPosition write SetPosition;
    property Velocity: TVector3 read FVelocity write SetVelocity;
    property Looping: boolean read FLooping write SetLooping;
    property Relative: boolean read FRelative write SetRelative;
    property Gain: Single read FGain write SetGain;
    property MinGain: Single read FMinGain write SetMinGain;
    property MaxGain: Single read FMaxGain write SetMaxGain;
    property Buffer: TSoundBuffer read FBuffer write SetBuffer;
    property Pitch: Single read FPitch write SetPitch;
    property RolloffFactor: Single read FRolloffFactor write SetRolloffFactor;
    property ReferenceDistance: Single read FReferenceDistance write SetReferenceDistance;
    property MaxDistance: Single read FMaxDistance write SetMaxDistance;

    { Playback time of this sound, expressed in seconds.

      This value will loop back to zero for looping sound sources.
      Setting this to something larger than the @italic(sound buffer duration)
      is ignored.

      This offset refers to the sound like it had a @link(Pitch) equal 1.0
      (when the sound is not slowed down or sped up).
      So this offset will vary from 0 to the @italic(sound buffer duration),
      regardless of the current @link(Pitch) value.
      The @italic(actual) seconds passed since the sound started
      playing may be different, if you will change the @link(Pitch)
      to something else than 1.0.

      Setting this on a not-yet playing sound source
      (this is done by @link(TSoundEngine.PlaySound))
      causes the sound to start playing from that offset. }
    property Offset: Single read GetOffset write SetOffset;

    { Is the sound playing or paused. This is almost always @true for sounds
      returned by TSoundAllocator.AllocateSound, when it stops being @true
      --- the sound engine will realize it (soon), which will cause @link(Release)
      and OnRelease being automatically called, and this TSound may then
      be reused for playing other sounds. }
    function PlayingOrPaused: boolean;

    { Make sure that the sound keeps playing, in case it stopped playing.

      This is an alternative approach to play a sound many times,
      like in a loop, but without using the @link(Looping) property.
      The idea is that you leave @link(Looping) set to @false,
      and you keep calling this method from some "update" event
      (like some @link(TInputListener.Update) implementation).
      Once you stop calling this method, the sound will automatically stop
      (once it finishes the current cycle).

      Note that you still (as always when using TSound) must observe
      the @link(TSound.OnRelease). When it's called, it means that the sound
      engine (TSoundEngine) decided that this sound should be used for other purposes
      (there's also a very small chance that the sound engine "caught"
      the sound as unused, in a short time when it stopped playing but you didn't
      yet call this method).
      In such case, you must stop doing anything with this TSound instance
      (including calling this method, @name, on it).
      You have to start playing the sound again by @link(TSoundEngine.PlaySound)
      instead.

      Note that calling this method is better than observing @link(TSound.OnRelease),
      to start playing a new sound when the previous one stopped.
      That's because @link(TSound.OnRelease) may be called with some small delay
      after the sound actually stopped, and it may be noticeable (e.g. in case
      of using this for a short rhytmic sound, like footsteps). }
    procedure KeepPlaying;
  end;

  TSoundList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TSound>)
  public
    { Sort sounds by Used + Importance, descending.
      First all sounds with Used = @true are placed,
      starting from the sound with largest Importance, and so on
      until the sound with smallest Importance.
      Then all sounds with Used = @false are placed (in any, arbitrary order).

      List must not contain nil values when calling this. }
    procedure SortByImportance;
  end;

  { Manager of allocated sounds.

    For efficiency, the pool of available sound sources (things that are actually
    audible at a given time) is limited.
    This limit comes from the backend limits (like OpenAL or APIs underneath OpenAL),
    it may also come from sound hardware limitations,
    and in general you should not have too many playing sources,
    as mixing many sources is time-consuming.
    So you cannot simply allocate new sound source for each of 100 creatures
    you display in the game.

    This class hides this limitation, by managing a pool of sound sources,
    and returning on demand (by @link(AllocateSound) method) the next unused
    sound source (or @nil). It can prioritize sound sources,
    it can reuse sound sources that finished playing,
    it can even interrupt a lower-priority sound when necessary to play
    a higher-priority sound.

    This is automatically used by higher-level comfortable methods to play
    sounds: @link(TSoundEngine.PlaySound),
    @link(TRepoSoundEngine.Sound) and @link(TRepoSoundEngine.Sound3D). }
  TSoundAllocator = class
  strict private
    FAllocatedSources: TSoundList;
    FMinAllocatedSources: Cardinal;
    FMaxAllocatedSources: Cardinal;
    LastSoundRefresh: TTimerResult;

    procedure Update(Sender: TObject);
    procedure SetMinAllocatedSources(const Value: Cardinal);
    procedure SetMaxAllocatedSources(const Value: Cardinal);
  private
    Backend: TSoundEngineBackend;
    FIsContextOpenSuccess: boolean;
    procedure ContextOpenCore; virtual;
    procedure ContextCloseCore; virtual;
  public
    const
      DefaultMinAllocatedSources = 4;
      DefaultMaxAllocatedSources = 16;

    constructor Create;
    destructor Destroy; override;

    { Internal: Allocate sound for playing. You should initialize the sound source
      properties and start playing the sound.

      Note that if you don't call TSound.Backend.Play immediately, the source may be detected
      as unused (and recycled for another sound) at the next
      sound allocation, play, update etc.

      If we can't allocate new sound source, we return nil.
      This may happen if your sound context is not initialized.
      It may also happen if we cannot create more sources (because
      we hit MaxAllocatedSources limit, or backend (like OpenAL) just refuses to create
      more sources) and all existing sounds are used and their
      Importance is > given here Importance.

      Note for looping sounds: just like any other sound, looping sound
      may be stopped because the sounds are needed for other sounds.
      If you want to try to restart the looping sound, you will have
      to implement it yourself. Or you can just set Importance of looping
      sounds high enough, and don't use too many looping sounds,
      to never let them be eliminated by other sounds. }
    function AllocateSound(const Importance: Integer): TSound;

    { All allocated (not necessarily used) sources.
      Accessing this is useful only for debugging tasks,
      in normal circumstances this is internal.
      This is @nil when ContextOpen was not yet called. }
    property AllocatedSources: TSoundList read FAllocatedSources;

    procedure Refresh; deprecated 'this does not do anything now; refreshing is done automatically if you use CastleWindow unit (with TCastleApplication, TCastleWindow) or TCastleControl; in other cases, you shoud call ApplicationProperties._Update yourself';

    { Stop all the sources currently playing. Especially useful since
      you have to stop a source before releasing it's associated buffer. }
    procedure StopAllSources;

    { Load and save into the config file sound engine properties.
      For example use with @link(UserConfig) to store sound preferences
      along with other user preferences.
      Everything is loaded / saved under the path "sound/" inside Config.

      TSoundAllocator saves MinAllocatedSources, MaxAllocatedSources.
      Descendant TSoundEngine additionally saves current Device, Enable
      (unless Enable was set by @--no-sound command-line option).
      Descendant TRepoSoundEngine additionally saves sound and music volume.

      @groupBegin }
    procedure LoadFromConfig(const Config: TCastleConfig); virtual;
    procedure SaveToConfig(const Config: TCastleConfig); virtual;
    { @groupEnd }
  published
    { Minimum / maximum number of allocated sources.
      Always keep MinAllocatedSources <= MaxAllocatedSources.

      For the sake of speed, we always keep allocated at least
      MinAllocatedSources sources. This must be >= 1.
      Setting MinAllocatedSources too large value will raise
      ENoMoreSources.

      At most MaxAllocatedSources sources may be simultaneously used (played).
      This prevents us from allocating too many sounds,
      which would be bad for speed (not to mention that it may
      be impossible under some backends, like OpenAL on Windows).
      When all MaxAllocatedSources sources are playing, the only way
      to play another sound is to use appropriately high @code(Importance)
      to AllocateSound.

      @groupBegin }
    property MinAllocatedSources: Cardinal
      read FMinAllocatedSources write SetMinAllocatedSources
      default DefaultMinAllocatedSources;

    property MaxAllocatedSources: Cardinal
      read FMaxAllocatedSources write SetMaxAllocatedSources
      default DefaultMaxAllocatedSources;
    { @groupEnd }
  end;

  TSoundDistanceModel = CastleSoundBase.TSoundDistanceModel;

  TSoundDevice = CastleSoundBase.TSoundDevice;

  { Parameters to use when playing sound, see @link(TSoundEngine.PlaySound). }
  TSoundParameters = class
    Buffer: TSoundBuffer;
    Spatial, Looping: boolean;
    Importance: Cardinal;
    { Gain is the volume of sound.
      MinGain and MaxGain determine how it can change because of spatialization
      (where the sound may get quieter / louder as you get further / closer to it).
      By default, Gain and MaxGain are 1.0, and MinGain is 0.0. }
    Gain, MinGain, MaxGain: Single;
    { The position of sound in 3D space.
      Used only if @link(Spatial) = @true. }
    Position: TVector3;
    { Pitch allows to play the sound faster. By default it is 1.0. }
    Pitch: Single;
    { See @link(TSoundEngine.DefaultRolloffFactor) for description.
      The @link(TSoundEngine.DefaultRolloffFactor) is also the default value of this field. }
    RolloffFactor: Single;
    { See @link(TSoundEngine.DefaultReferenceDistance) for description.
      The @link(TSoundEngine.DefaultReferenceDistance) is also the default value of this field. }
    ReferenceDistance: Single;
    { See @link(TSoundEngine.DefaultMaxDistance) for description.
      The @link(TSoundEngine.DefaultMaxDistance) is also the default value of this field. }
    MaxDistance: Single;
    { Offset is a position in time of the sound. }
    Offset: Single;
    constructor Create;
  end;

  { Sound engine, responsible for loading and playing sound.

    There should always be only one instance of this class,
    accessed through the global @link(SoundEngine) variable.
    See docs at @link(SoundEngine) for more details.

    The sound engine is actually a wrapper over a backend, like OpenAL.
    You can explicitly initialize OpenAL context by ContextOpen,
    and explicitly close it by ContextClose. If you did not call ContextOpen
    explicitly (that is, IsContextOpen is @false), then the first LoadBuffer
    or TRepoSoundEngine.Sound or TRepoSoundEngine.Sound3D
    will automatically do it for you. If you do not call ContextClose
    explicitly, then at destructor we'll do it automatically. }
  TSoundEngine = class(TSoundAllocator)
  private
    type
      TSoundBuffersList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TSoundBuffer>;
    var
      FInformation: string;
      FDevice: string;
      FVolume: Single;
      FEnabled: boolean;
      FIsContextOpen: boolean;
      FDefaultRolloffFactor: Single;
      FDefaultReferenceDistance: Single;
      FDefaultMaxDistance: Single;
      FDistanceModel: TSoundDistanceModel;
      LoadedBuffers: TSoundBuffersList;
      FDevices: TSoundDeviceList;
      FOnOpenClose: TNotifyEventList;
      FResumeToInitialized, FPaused: boolean;
      FListenerPosition, FListenerDirection, FListenerUp: TVector3;
      FEnableSaveToConfig, DeviceSaveToConfig: boolean;
      FInitialPitchMultiplier: Single;

    procedure SetVolume(const Value: Single);
    procedure SetDistanceModel(const Value: TSoundDistanceModel);
    procedure SetDevice(const Value: string);
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

    procedure ContextOpenCore; override;
    procedure ContextCloseCore; override;

    class function GetLogSoundLoading: Boolean; static;
    class procedure SetLogSoundLoading(const Value: Boolean); static;
  public
    const
      DefaultVolume = 1.0;
      DefaultDefaultRolloffFactor = 1.0;
      DefaultDefaultReferenceDistance = 1.0;
      DefaultDefaultMaxDistance = MaxSingle;
      DefaultDistanceModel = dmLinearDistanceClamped;
      DefaultDevice = '';
      DefaultEnabled = true;

    constructor Create;
    destructor Destroy; override;

    { Initialize sound engine.
      Initializes sound backend (like OpenAL library).
      Sets @link(IsContextOpen), @link(IsContextOpenSuccess), @link(Information).

      You can set @link(Device) before calling this.

      Note that we continue (without any exception) if the initialization
      failed for any reason (e.g. OpenAL library is not available,
      or no sound output device is available).
      You can check @link(IsContextOpenSuccess) and @link(Information) to know if
      the initialization was actually successfull. But you can also ignore it,
      the sound engine will silently (literally) keep working even if OpenAL
      could not be initialized. }
    procedure ContextOpen;

    { Release sound backend resources.
      This sets @link(IsContextOpen) and @link(IsContextOpenSuccess) to @false.
      It's allowed and harmless to call this when one of them is already @false. }
    procedure ContextClose;

    { Sound backend, like OpenAL or FMOD or SOX.
      Do not change or access this yourself.
      You can change this only by calling procedure like UseFMODSoundBackend
      from CastleFMODSoundBackend unit. }
    property InternalBackend: TSoundEngineBackend read Backend write SetInternalBackend;

    procedure ALContextOpen; deprecated 'use ContextOpen';
    procedure ALContextClose; deprecated 'use ContextClose';

    procedure LoadFromConfig(const Config: TCastleConfig); override;
    procedure SaveToConfig(const Config: TCastleConfig); override;

    { Do we have active sound rendering context.
      This is @true when you successfully
      called ContextOpen (and you didn't call ContextClose yet).

      You should not need this property much.
      The whole CastleSoundEngine API works regardless if the context
      was successfully open or not.
      However, reading this is useful to display to user warning
      e.g. "Sound could not be initialized for some reason" (use @link(Information)
      to get the details).

      In case of OpenAL backend, this also implies that OpenAL library is loaded. }
    property IsContextOpenSuccess: boolean read FIsContextOpenSuccess;
    property ALActive: boolean read FIsContextOpenSuccess;
      deprecated 'use IsContextOpenSuccess';

    { Did we attempt to initialize sound rendering context.
      This indicates that ContextOpen  was called,
      and not closed with ContextClose yet.
      Contrary to IsContextOpenSuccess,
      this @italic(doesn't care if ContextOpen was a success). }
    property IsContextOpen: boolean read FIsContextOpen;
    property ALInitialized: Boolean read FIsContextOpen; deprecated 'use IsContextOpen';

    property SoundInitializationReport: string read FInformation;
      deprecated 'use Information';

    property Information: string read FInformation;

    { Load a sound file contents such that they can be immediately played.

      This method tries to initialize backend (like OpenAL) context,
      and loads the buffer contents. But even when it fails, it still returns
      a valid (non-nil) TSoundBuffer instance. The @link(PlaySound) must be
      ready anyway to always load the buffer on-demand (because OpenAL context
      may be lost while the game is ongoing, in case of Android).

      The buffer should be released by @link(FreeBuffer) later when it's not needed.
      Although we will take care to always free remaining buffers
      before closing OpenAL context anyway.

      We have a cache of sound files here. An absolute URL
      will be recorded as being loaded to given buffer. Loading the same
      URL second time returns the same buffer instance. The buffer
      is released only once you call @link(FreeBuffer) as many times as you called
      LoadBuffer for it.

      @raises(ESoundFileError If loading of this sound file failed.
        There are many reasons why this may happen: we cannot read given URL,
        or it may contain invalid contents,
        or a library required to decompress e.g. OggVorbis is missing.)

      @groupBegin }
    function LoadBuffer(const URL: string; const ExceptionOnError: boolean = true): TSoundBuffer; overload;
    function LoadBuffer(const URL: string; out Duration: TFloatTime): TSoundBuffer;
      overload;
      deprecated 'use LoadBuffer without Duration parameter, and just read TSoundBuffer.Duration after loading';
    { @groupEnd }

    { Free a sound file buffer. Ignored when buffer is @nil.
      Buffer is always set to @nil after this.

      @raises(ESoundBufferNotLoaded When invalid (not @nil,
        and not returned by LoadBuffer) buffer identifier is given.) }
    procedure FreeBuffer(var Buffer: TSoundBuffer);

    { Play a sound from given buffer.

      We use a smart sound allocator, so the sound will be actually
      played only if resources allow. Use higher Importance to indicate
      sounds that are more important to play.

      We set the sound properties and start playing it.

      Both spatialized (3D) and not spatialized sounds are possible.
      See the @link(TSoundParameters) for a full list of sound parameters.
      You can pass all the sound parameters as a @link(TSoundParameters) instance.
      You can destroy the @link(TSoundParameters) instance right after calling
      this method, the reference to it is not saved anywhere.

      @returns(The allocated sound as TSound.

        Returns @nil when there were no resources to play another sound
        (and it wasn't important enough to override another sound).
        Always returns @nil when SoundBuffer is zero (indicating that buffer
        was not loaded).

        In simple cases you can just ignore the result of this method.
        In advanced cases, you can use it to observe and update the sound
        later.)
    }
    function PlaySound(const Buffer: TSoundBuffer): TSound; overload;
    function PlaySound(const Buffer: TSoundBuffer;
      const Spatial, Looping: boolean; const Importance: Cardinal;
      const Gain, MinGain, MaxGain: Single;
      const Position: TVector3;
      const Pitch: Single = 1): TSound; overload;
    function PlaySound(const Buffer: TSoundBuffer;
      const Spatial, Looping: boolean; const Importance: Cardinal;
      const Gain, MinGain, MaxGain: Single;
      const Position: TVector3;
      const Pitch: Single;
      const ReferenceDistance: Single;
      const MaxDistance: Single): TSound; overload; deprecated 'use PlaySound that gets TSoundParameters instance';
    function PlaySound(const Parameters: TSoundParameters): TSound; overload;

    { Parse parameters in @link(Parameters) and interpret and remove
      recognized options. Internally it uses Parameters.Parse with
      ParseOnlyKnownLongOptions = @true. Recognized options:

      @definitionList(
        @itemLabel @--audio-device DEVICE-NAME
        @item Set @link(Device) variable to given argument.

        @itemLabel @--no-sound
        @item Disable any sound (sets @link(Enable) to @false).
      )

      More user-oriented documentation for the above options is here:
      [https://castle-engine.io/openal_notes.php#section_options] }
    procedure ParseParameters;

    { Help string for options parsed by ParseParameters.

      Note that it also lists the available sound output @link(Devices),
      as they are valid arguments for the @--audio-device option. }
    function ParseParametersHelp: string;

    { Set the sound listener position and orientation. }
    procedure UpdateListener(const Position, Direction, Up: TVector3);

    { List of available sound devices. Read-only.

      Use @code(Devices[].Name) as @link(Device) values.

      On some backend implementations, also some other @link(Device) values may
      be possible. E.g. old Loki implementation of OpenAL allowed some hints
      to be encoded in Lisp-like language inside the @link(Device) string. }
    function Devices: TSoundDeviceList;

    function DeviceNiceName: string; deprecated 'use DeviceCaption';
    function DeviceCaption: string;

    { Events fired after sound context is being open or closed.
      More precisely, when IsContextOpen changes (and so, possibly, IsContextOpenSuccess
      changed). }
    property OnOpenClose: TNotifyEventList read FOnOpenClose;

    { Should we save @link(Enable) to config file in SaveToConfig call.
      This is always reset to @true after setting @link(Enable) value. }
    property EnableSaveToConfig: boolean
      read FEnableSaveToConfig write FEnableSaveToConfig default true;

    class property LogSoundLoading: Boolean
      read GetLogSoundLoading write SetLogSoundLoading;

    { Newly played sounds will have @link(TSound.Pitch) multiplied by this. }
    property InitialPitchMultiplier: Single read FInitialPitchMultiplier write FInitialPitchMultiplier default 1.0;
  published
    { Sound volume, affects all sounds (effects and music).
      This must always be within 0..1 range.
      0.0 means that there are no effects (this case should be optimized). }
    property Volume: Single read FVolume write SetVolume
      default DefaultVolume;

    { Sound output device, used when initializing sound context.

      You can change it even when context is already initialized.
      Then we'll close the old device (ContextClose),
      change @link(Device) value, and initialize context again (ContextOpen).
      Note that you will need to reload your buffers and sources again. }
    property Device: string read FDevice write SetDevice;

    { Enable sound.

      If @false, then ContextOpen will not initialize any device.
      This is useful if you simply want to disable any sound output
      (or backend, like OpenAL, usage), even when sound library (like OpenAL)
      is available.

      If the sound context is already initialized when setting this,
      we will eventually close it. (More precisely, we will
      do ContextClose and then ContextOpen again. This behaves correctly.) }
    property Enabled: boolean read FEnabled write SetEnabled default DefaultEnabled;

    property Enable: boolean read FEnabled write SetEnabled default DefaultEnabled; deprecated 'Use Enabled';

    { How the sound is attenuated with the distance.
      These are used only for spatialized sounds created with PlaySound.
      The DefaultReferenceDistance and DefaultMaxDistance values
      are used only if you don't supply explicit values to PlaySound.

      The exact interpretation of these depends on current
      DistanceModel. See OpenAL specification for exact equations.
      In short:

      @unorderedList(
        @item(Smaller Rolloff Factor makes the attenuation weaker.
          In particular 0 turns off attenuation by distance.
          Default is 1.)
        @item(Reference Distance is the distance at which exactly sound
          gain is heard. Default is 1.)
        @item(Max Distance interpretation depends on the model.
          For "inverse clamped model", the gain is no longer scaled down
          after reaching this distance. For linear models, the gain
          reaches zero at this distance. Default is maximum float
          (I don't know the interpretation of this for linear model).)
      )

      Our default values follow OpenAL default values.
      @groupBegin }
    property DefaultRolloffFactor: Single
      read FDefaultRolloffFactor write FDefaultRolloffFactor default DefaultDefaultRolloffFactor;
    property DefaultReferenceDistance: Single
      read FDefaultReferenceDistance write FDefaultReferenceDistance default DefaultDefaultReferenceDistance;
    property DefaultMaxDistance: Single
      read FDefaultMaxDistance write FDefaultMaxDistance default DefaultDefaultMaxDistance;
    { @groupEnd }

    { How the sources are spatialized. For precise meaning, see OpenAL
      specification of alDistanceModel.

      Note that some models are actually available only since OpenAL 1.1
      version. Older OpenAL versions may (but don't have to) support them
      through extensions. We will internally do everything possible to
      request given model, but eventually may fallback on some other model.
      This probably will not be a problem in practice, as all modern OS
      versions (Linux distros, Windows OpenAL installers etc.) include OpenAL
      1.1.

      The default distance model, DefaultDistanceModel, is the linear model
      most conforming to VRML/X3D sound requirements. You can change it
      if you want (for example, OpenAL default is dmInverseDistanceClamped). }
    property DistanceModel: TSoundDistanceModel
      read FDistanceModel write SetDistanceModel default DefaultDistanceModel;
  end;

  TLoopingChannel = class;

  { Unique sound type identifier for sounds used within TRepoSoundEngine. }
  TSoundType = record
  private
    { Index to TRepoSoundEngine.FSounds array. }
    Index: Cardinal;
  public
    class operator {$ifdef FPC}={$else}Equals{$endif} (const SoundType1, SoundType2: TSoundType): boolean;
  end;

  { Sound engine that keeps a repository of sounds, defined in a nice XML file.
    This allows to have simple @link(Sound) and @link(Sound3D) methods,
    that take a sound identifier (managing sound buffers will just happen
    automatically under the hood).

    It extends TSoundEngine, so you can always still load new buffers
    and play them by TSoundEngine.LoadBuffer, TSoundEngine.PlaySound
    and all other methods. This only adds easy preloaded sounds,
    but you're not limited to them.

    To initialize your sounds repository, you have to set the RepositoryURL
    property. }
  TRepoSoundEngine = class(TSoundEngine)
  private
    {$ifndef PASDOC} // PasDoc cannot parse the private section of this correctly yet
    type
      TSoundInfoBuffer = class;
      TSoundInfoList = class;
      TSoundGroup = class;

      { Sound that can be played -- actual sound with buffer, or an alias to it. }
      TSoundInfo = class
      strict private
        { Although we only support one instance of TSoundEngine,
          in @link(SoundEngine), but it seems more future-proof
          to store in TSoundInfo own reference to TSoundEngine.
          Automatically assigned in ReadElement. }
        FOwningSoundEngine: TSoundEngine;
      strict protected
        property OwningSoundEngine: TSoundEngine read FOwningSoundEngine;
      public
        { Unique sound name (including parent group names). Empty for the special sound stNone. }
        Name: String;

        { Like Name, but without parent group names.
          Unique within the ParentGroup (not necessarily unique among all sounds). }
        ShortName: String;

        { A group (one among FSoundGroups, or @nil if not in any group). }
        ParentGroup: TSoundGroup;

        { Read a sound from XML element <sound> or <alias>. }
        procedure ReadElement(const Element: TDOMElement;
          const AParentGroup: TSoundGroup;
          const BaseUrl: String; const ASoundEngine: TRepoSoundEngine); virtual;

        { Do some finalization once all sounds are known, and their names are known. }
        procedure ResolveNames(const AllSounds: TSoundInfoList); virtual;

        { Get the final TSoundInfoBuffer, resolving aliases. }
        function FinalSound(const RecursionDepth: Cardinal): TSoundInfoBuffer; virtual; abstract;
      end;

      { List of TSoundInfo. }
      TSoundInfoList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TSoundInfo>)
      public
        { Index of sound with given TSoundInfo.Name, or -1 if not found. }
        function IndexOfName(const SoundName: String): Integer;
      end;

      { Sound that can be played, as an alias to a number of other TSoundInfo instances
        (an alias may lead to another alias, that's OK, as long as eventually it
        resolves into actual sound name). }
      TSoundInfoAlias = class(TSoundInfo)
      strict private
        Target: TCastleStringList;
      public
        constructor Create;
        destructor Destroy; override;
        procedure ReadElement(const Element: TDOMElement;
          const AParentGroup: TSoundGroup;
          const BaseUrl: String; const ASoundEngine: TRepoSoundEngine); override;
        procedure ResolveNames(const AllSounds: TSoundInfoList); override;
        function FinalSound(const RecursionDepth: Cardinal): TSoundInfoBuffer; override;
      end;

      { Sound that can be played, with a buffer.
        Most fields of this classs correspond to appropriate attributes in
        the XML file loaded by setting @link(TRepoSoundEngine.RepositoryURL). }
      TSoundInfoBuffer = class(TSoundInfo)
      public
        { Buffer of this sound. @nil if buffer is not yet loaded,
          which may happen only if TRepoSoundEngine.ContextOpen was not yet
          called or when sound has URL = ''. }
        Buffer: TSoundBuffer;

        { URL from which to load sound data.
          Absolute (including parent group URL parts).

          Empty means that the sound data is not defined,
          so the OpenAL buffer will not be initialized and trying to play
          this sound (with methods like TSoundEngine.Sound or TSoundEngine.Sound3D)
          will do nothing. This is useful if you want to use a sound name
          in code, but you do not have the actual sound file for this yet. }
        URL: string;

        { Gain (how loud the sound is).
          They are mapped directly to respective OpenAL source properties,
          so see OpenAL specification for exact details what they mean.
          In short:

          @unorderedList(
            @item(Gain scales the sound loudness. Use this to indicate that
              e.g. a plane engine is louder than a mouse squeak (when heard
              from the same distance).

              Do @italic(not) make the actual sound data (in wav, ogg and such files)
              louder/more silent for this purpose.
              This is usually bad for sound quality. Instead, keep your sound data
              at max loudness (normalized), and use this @link(Gain) property
              to scale sound.

              It can be antything from 0 to +infinity. The default is 1.)

            @item(MinGain and MaxGain force a minimum/maximum sound loudness.
              These can be used to "cheat" around default distance attenuation
              calculation.

              These must be in [0, 1] range. By default MinGain is 0 and MaxGain is 1.)
          )

          Note that Gain value > 1 is allowed.
          Although sound backend (like OpenAL) may clip the resulting sound (after all
          calculations taking into account 3D position will be done).
          The resulting sound is also clamped by MaxGain
          (that generally must be in [0, 1], although some OpenAL implementations
          allow values > 1).

          When this sound is used for @link(TLoopingChannel.Sound):
          @orderedList(
            @item(MinGain, MaxGain are ignored.)
            @item(Effective Gain (passed to OpenAL sound source) is the
              @link(TLoopingChannel.Volume) multiplied by our @link(Gain).)
          ) }
        Gain, MinGain, MaxGain: Single;

        { How important the sound is. Influences what happens when we have a lot
          of sounds playing at once. See TSound.Importance.

          Ignored when this sound is used for @link(TLoopingChannel.Sound). }
        DefaultImportance: Cardinal;

        procedure ReadElement(const Element: TDOMElement;
          const AParentGroup: TSoundGroup;
          const BaseUrl: String; const ASoundEngine: TRepoSoundEngine); override;
        function FinalSound(const RecursionDepth: Cardinal): TSoundInfoBuffer; override;
        destructor Destroy; override;
      end;

      TSoundGroup = class(TSoundInfoList)
      public
        { Group name (including parent group names). }
        Name: string;
        { Group URL.
          Absolute (including parent group URL parts).
          Always ends with slash. }
        URL: string;
        { A parent group (one among FSoundGroups, or @nil if not in any group). }
        ParentGroup: TSoundGroup;
      end;

      TSoundGroupList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TSoundGroup>)
      public
        { Index of group with given TSoundGroup.Name, or -1 if not found. }
        function IndexOfName(const GroupName: String): Integer;
      end;

      TLoopingChannelList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TLoopingChannel>;

    var
      FSoundImportanceNames: TStringList;
      { A list of sounds used by your program.
        Each sound has a unique name, used to identify sound in
        the XML file and for SoundFromName function.

        At the beginning, this list always contains exactly one sound: empty stNone.
        This is a special "sound type" that has index 0 (should be always
        expressed as TSoundType value stNone) and name ''.
        stNone is a special sound as it actually means "no sound" in many cases. }
      FSounds: TSoundInfoList;
      FSoundGroups: TSoundGroupList;
      FRepositoryURL: string;
      FLoopingChannels: TLoopingChannelList;
    {$endif PASDOC}

    procedure SetRepositoryURL(const Value: string);
    { Reinitialize looping channels sounds.
      Should be called as soon as Sounds changes and we may have OpenAL context. }
    procedure RestartLoopingChannels;
    function GetMusicPlayer: TLoopingChannel;
    function GetLoopingChannel(const Index: Cardinal): TLoopingChannel;
    function FinalSound(const SoundType: TSoundType): TSoundInfoBuffer;
    procedure ContextOpenCore; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromConfig(const Config: TCastleConfig); override;
    procedure SaveToConfig(const Config: TCastleConfig); override;

    { The XML file that contains description of your sounds.
      This should be an URL (in simple cases, just a filename)
      pointing to an XML file describing your sounds.
      See https://castle-engine.io/creating_data_sound.php and
      engine examples for details (@code(examples/audio/sample_sounds.xml)
      contains an example file with lots of comments).

      When you set RepositoryURL property, we read sound information from
      given XML file. You usually set RepositoryURL at the very beginning
      of the application.
      Right after setting RepositoryURL you usually call SoundFromName
      a couple of times to convert some names into TSoundType values,
      to later use these TSoundType values with @link(Sound) and @link(Sound3D)
      methods.

      When the sound context is initialized (or when you set this property,
      if the sound context is initialized already)
      then sound buffers will actually be loaded.

      If this is empty (the default), then no sounds are loaded,
      and TRepoSoundEngine doesn't really give you much above standard
      TSoundEngine.

      If you want to actually use TRepoSoundEngine features
      (like the @link(Sound) and @link(Sound3D) methods) you have to set this
      property. For example like this:

      @longCode(#
        SoundEngine.RepositoryURL := 'castle-data:/sounds.xml';
        stMySound1 := SoundEngine.SoundFromName('my_sound_1');
        stMySound2 := SoundEngine.SoundFromName('my_sound_2');
        // ... and later in your game you can do stuff like this:
        SoundEngine.Sound(stMySound1);
        SoundEngine.Sound3D(stMySound1, Vector3(0, 0, 10));
      #)

      See https://castle-engine.io/manual_data_directory.php
      for information about the castle-data:/ protocol. In short, on desktop,
      this just indicates the "data" subdirectory of your project.
    }
    property RepositoryURL: string read FRepositoryURL write SetRepositoryURL;

    { Deprecated name for RepositoryURL. @deprecated }
    property SoundsFileName: string read FRepositoryURL write SetRepositoryURL; deprecated;

    { Reload the RepositoryURL and all referenced buffers.
      Useful as a tool for game designers, to reload the sounds XML file
      without restarting the game and sound engine. }
    procedure ReloadSounds;

    { Return sound with given name.
      Available names are given in SoundNames, defined in XML file pointed
      by RepositoryURL.
      Always for SoundName = '' it will return stNone.

      @param(Required

        If Required = @true, it will make a warning when the sound name
        is not found. This may mean that sound is missing in your sounds.xml
        file (so you should correct your sounds.xml),
        or that you didn't load the sounds.xml file yet
        (so you should correct your code to set @link(TRepoSoundEngine.RepositoryURL)
        early enough), or that you specified invalid sound name.
        When Required = @false, missing sound is silently ignored,
        which is sensible if it was optional.

        Regardless of the Required value, we return stNone for missing sound.
        So the Required parameter only determines whether we make a warning,
        or not.)
    }
    function SoundFromName(const SoundName: string; const Required: boolean = true): TSoundType;

    { Play given sound. This should be used to play sounds
      that are not spatial, i.e. have no place in 3D space.

      Returns used TSound (or nil if none was available).
      You don't have to do anything with this returned TSound. }
    function Sound(const SoundType: TSoundType;
      const Looping: boolean = false): TSound;

    { Play given sound at appropriate position in 3D space.

      Returns used TSound (or nil if none was available).
      You don't have to do anything with this returned TSound.

      @noAutoLinkHere }
    function Sound3D(const SoundType: TSoundType;
      const Position: TVector3;
      const Looping: boolean = false): TSound; overload;

    { Sound importance names and values.
      Each item is a name (as a string) and a value (that is stored in Objects
      property of the item as a pointer; add new importances by
      AddSoundImportanceName for comfort).

      These can be used within sounds.xml file.
      Before using ContextOpen, you can fill this list with values.

      Initially, it contains a couple of useful values (ordered here
      from most to least important):

      @unorderedList(
        @item 'max' - MaxSoundImportance
        @item 'level_event' - LevelEventSoundImportance
        @item 'player' - PlayerSoundImportance
        @item 'default_creature' - DefaultCreatureSoundImportance
        @item 'minor_non_spatial' - MinorNonSpatialSoundImportance
      ) }
    property SoundImportanceNames: TStringList read FSoundImportanceNames;

    procedure AddSoundImportanceName(const Name: string; Importance: Integer);

    { Comfortable way to play and control the music.
      Simply assign @link(TLoopingChannel.Sound MusicPlayer.Sound)
      to play music. Set it to @link(stNone) to stop playing music.
      This is just a shortcut for @link(LoopingChannel LoopingChannel[0]). }
    property MusicPlayer: TLoopingChannel read GetMusicPlayer;

    { Comfortable way to play and control looping sounds, like a music track.
      Using this is an alternative way to playing looping sounds using
      @link(Sound) with Looping=true parameter.

      The TLoopingChannel instance automatically remembers the sound it plays.
      You start playing by setting @link(TLoopingChannel.Sound)
      to some sound. For example:

      @longCode(# LoopingChannel[0].Sound := SoundFromName('my_music'); #)

      You stop by setting @link(TLoopingChannel.Sound) to something else,
      which can be @link(stNone) to just stop playing any looping
      sound on this channel. For example:

      @longCode(# LoopingChannel[0].Sound := stNone; #)

      Each channel has it's own @link(TLoopingChannel.Volume)
      that can be changed at any point.

      All the looping channels (managed through this) play simultaneously,
      in addition to all other (looping and non-looping) sounds created
      by @link(Sound). }
    property LoopingChannel [const Index: Cardinal]: TLoopingChannel
      read GetLoopingChannel;

    { Opens sound context and loads sound files,
      but only if RepositoryURL was set and contains some sounds.

      The idea is that you can call this during "loading" stage for any game that
      *possibly but not necessarily* uses sound. If a game doesn't use sound,
      this does nothing (doesn't waste time to even initialize sound context,
      which on some systems may cause some warnings).
      If a game uses sound (through RepositoryURL), this will initialize
      sound backend and load these sound files, to play them without any delay
      in game.

      Note that, if this does nothing, but you later set @link(RepositoryURL)
      or do @link(LoadBuffer) or @link(PlaySound), then sound context will
      be createdon-demand anyway. So calling this is always optional. }
    procedure PrepareResources;
  end;

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
      or TSound instance was necessary for higher-priority sounds. }
    FAllocatedSource: TSound;

    FVolume, FPitch: Single;
    { Constant Gain of TSoundInfo associated with Sound,
      will be multiplied by Volume. }
    SoundInfoGain, SoundInfoPitch: Single;

    FSound: TSoundType;
    procedure SetSound(const Value: TSoundType);
    procedure AllocatedSourceRelease(Sender: TSound);

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

  TMusicPlayer = TLoopingChannel;

var
  { Common sounds.

    The sounds types listed below are automatically
    initialized when you set TRepoSoundEngine.RepositoryURL.
    All engine units can use them if you define them in your sounds XML file.
    If they are not defined in your XML file (or if you don't even have
    an XML file, that is you leave TRepoSoundEngine.RepositoryURL empty)
    then they remain stNone (and nothing will happen if anything will try
    to play them by TRepoSoundEngine.Sound or TRepoSoundEngine.Sound3D).

    Simply define them in your sounds XML file (see
    TRepoSoundEngine.RepositoryURL) under a suitable name with underscores,
    like 'player_dies' for stPlayerDies. }

  { Player sounds.
    @groupBegin }
  stPlayerInteractFailed,
  stPlayerPickItem,
  stPlayerDropItem,
  stPlayerSwimming,
  stPlayerDrowning,
  stPlayerFootstepsDefault,
  stPlayerToxicPain,
  stPlayerSuddenPain,
  stPlayerDies,
  stPlayerSwimmingChange,
  { @groupEnd }

  { Sounds used by TCastleOnScreenMenu.
    @groupBegin }
  stMenuCurrentItemChanged,
  stMenuClick
  { @groupEnd }
    :TSoundType;

const
  { Special sound type that indicates that there is actually no sound.
    @link(TRepoSoundEngine.Sound) and @link(TRepoSoundEngine.Sound3D)
    will do nothing when called with this sound type. }
  stNone: TSoundType = (Index: 0);

  MaxSoundImportance = MaxInt;
  LevelEventSoundImportance      = 100000;
  PlayerSoundImportance          = 10000;
  DefaultCreatureSoundImportance = 1000;
  MinorNonSpatialSoundImportance = 100;

  dmNone                    = CastleSoundBase.dmNone;
  dmInverseDistance         = CastleSoundBase.dmInverseDistance;
  dmInverseDistanceClamped  = CastleSoundBase.dmInverseDistanceClamped;
  dmLinearDistance          = CastleSoundBase.dmLinearDistance;
  dmLinearDistanceClamped   = CastleSoundBase.dmLinearDistanceClamped;
  dmExponentDistance        = CastleSoundBase.dmExponentDistance;
  dmExponentDistanceClamped = CastleSoundBase.dmExponentDistanceClamped;

  { Supported sound file formats.
    Use these filters with LCL file dialog (easily set by FileFiltersToDialog)
    or TCastleWindowBase.FileDialog. }
  LoadSound_FileFilters =
  'All Files|*|' +
  '*All Sound Files|*.wav;*.ogg|' +
  'WAV (*.wav)|*.wav|' +
  'OggVorbis (*.ogg)|*.ogg';

{ The sound engine. Singleton instance of TRepoSoundEngine, the most capable
  engine class. Created on first call to this function. }
function SoundEngine: TRepoSoundEngine;

implementation

{ use a deprecated unit below, only to have it compiled together with Lazarus
  castle_base.lpk package }
{$warnings off}
uses XMLRead, StrUtils, Generics.Defaults,
  CastleUtils, CastleLog, CastleProgress, CastleInternalVorbisFile,
  CastleParameters, CastleXMLUtils, CastleFilesUtils, CastleConfig,
  CastleURIUtils, CastleDownload, CastleMessaging, CastleApplicationProperties,
  {$ifdef CASTLE_SOUND_BACKEND_DEFAULT_OPENAL} CastleOpenALSoundBackend, {$endif}
  // unit below is deprecated
  CastleSoundAllocator;
{$warnings on}

var
  FSoundEngine: TRepoSoundEngine;

{ TSoundBuffer --------------------------------------------------------------- }

constructor TSoundBuffer.Create(const SoundEngineBackend: TSoundEngineBackend);
begin
  inherited Create;
  Backend := SoundEngineBackend.CreateBuffer;
end;

procedure TSoundBuffer.ContextOpen(const ExceptionOnError: boolean);

  procedure OpenCore;
  begin
    FURL := URL;
    Backend.ContextOpen(URL);
    FDuration := Backend.Duration;
    FDataFormat := Backend.DataFormat;
    FFrequency := Backend.Frequency;
    BackendIsOpen := true;
  end;

begin
  if BackendIsOpen then
    Exit; // do not initialize already-initialized

  if ExceptionOnError then
  begin
    OpenCore;
  end else
  try
    OpenCore;
  except
    on E: Exception do
    begin
      WritelnWarning('Sound', Format('Sound file "%s" cannot be loaded: %s',
        [URIDisplay(URL), E.Message]));
    end;
  end;
end;

procedure TSoundBuffer.ContextClose;
begin
  if BackendIsOpen then
  begin
    Backend.ContextClose;
    BackendIsOpen := false;
  end;
end;

var
  ValidSoundBufferFree: Cardinal;

destructor TSoundBuffer.Destroy;
begin
  if ValidSoundBufferFree = 0 then
    raise EInvalidSoundBufferFree.Create('Do not free TSoundBuffer instance directly, use SoundEngine.FreeBuffer');
  ContextClose;
  FreeAndNil(Backend);
  inherited;
end;

{ TSound ---------------------------------------------------------- }

constructor TSound.Create(const SoundEngineBackend: TSoundEngineBackend);
begin
  inherited Create;

  Backend := SoundEngineBackend.CreateSource;
  { For now, TSound always refers to an open (on backend side) sound source. }
  Backend.ContextOpen;
  { This way, TSound.Destroy will not call Backend.ContextClose
    if Backend.ContextOpen failed.
    (So e.g. OpenAL Backend.ContextClose doesn't try to call
    alDeleteSources on invalid sound source.) }
  BackendIsOpen := true;
end;

destructor TSound.Destroy;
begin
  if (Backend <> nil) and BackendIsOpen then
    Backend.ContextClose;
  FreeAndNil(Backend);
  inherited;
end;

procedure TSound.Release;
begin
  FUsed := false;

  Backend.Stop;

  { Detach the buffer from source. Otherwise we couldn't free the buffer
    while it's associated with the source. Also, this would be a problem
    once we implement streaming on some sources: you have to reset
    buffer to 0 before queing buffers on source. }
  Buffer := nil;

  if Assigned(OnRelease) then
  begin
    OnRelease(Self);
    OnRelease := nil;
  end;
end;

procedure TSound.SetPosition(const Value: TVector3);
begin
  FPosition := Value;
  Backend.SetPosition(Value);
end;

procedure TSound.SetVelocity(const Value: TVector3);
begin
  FVelocity := Value;
  Backend.SetVelocity(Value);
end;

procedure TSound.SetLooping(const Value: boolean);
begin
  FLooping := Value;
  Backend.SetLooping(Value);
end;

procedure TSound.SetRelative(const Value: boolean);
begin
  FRelative := Value;
  Backend.SetRelative(Value);
end;

procedure TSound.SetGain(const Value: Single);
begin
  FGain := Value;
  Backend.SetGain(Value);
end;

procedure TSound.SetMinGain(const Value: Single);
begin
  FMinGain := Value;
  Backend.SetMinGain(Value);
end;

procedure TSound.SetMaxGain(const Value: Single);
begin
  FMaxGain := Value;
  Backend.SetMaxGain(Value);
end;

procedure TSound.SetBuffer(const Value: TSoundBuffer);
begin
  FBuffer := Value;
  if (Value <> nil) and Value.BackendIsOpen then
    Backend.SetBuffer(Value.Backend)
  else
    Backend.SetBuffer(nil);
end;

procedure TSound.SetPitch(const Value: Single);
begin
  FPitch := Value;
  Backend.SetPitch(Value);
end;

procedure TSound.SetRolloffFactor(const Value: Single);
begin
  FRolloffFactor := Value;
  Backend.SetRolloffFactor(Value);
end;

procedure TSound.SetReferenceDistance(const Value: Single);
begin
  FReferenceDistance := Value;
  Backend.SetReferenceDistance(Value);
end;

procedure TSound.SetMaxDistance(const Value: Single);
begin
  FMaxDistance := Value;
  Backend.SetMaxDistance(Value);
end;

function TSound.GetOffset: Single;
begin
  Result := Backend.Offset;
end;

procedure TSound.SetOffset(const Value: Single);
begin
  Backend.Offset := Value;
end;

function TSound.PlayingOrPaused: boolean;
begin
  Result := Backend.PlayingOrPaused;
end;

procedure TSound.KeepPlaying;
begin
  if not PlayingOrPaused then
    Backend.Play(false);
end;

procedure TSound.Update(const SecondsPassed: TFloatTime);
begin
  if Used then
  begin
    if not BackendIsOpen then
      Release
    else
    begin
      Backend.Update(SecondsPassed);
      if not PlayingOrPaused then
        Release;
    end;
  end;
end;

{ TSoundList ----------------------------------------------------- }

function IsSmallerByImportance(constref AA, BB: TSound): Integer;
begin
  if (AA.Used and (not BB.Used)) or
     (AA.Used and BB.Used and (AA.Importance > BB.Importance)) then
    Result := -1 else
  if (BB.Used and (not AA.Used)) or
     (BB.Used and AA.Used and (BB.Importance > AA.Importance)) then
    Result :=  1 else
    Result :=  0;
end;

procedure TSoundList.SortByImportance;
type
  TSoundComparer = {$ifdef CASTLE_OBJFPC}specialize{$endif} TComparer<TSound>;
begin
  Sort(TSoundComparer.Construct(
    {$ifdef CASTLE_OBJFPC}@{$endif} IsSmallerByImportance));
end;

{ TSoundAllocator ---------------------------------------------------------- }

constructor TSoundAllocator.Create;
begin
  inherited;
  FMinAllocatedSources := DefaultMinAllocatedSources;
  FMaxAllocatedSources := DefaultMaxAllocatedSources;

  {$ifdef CASTLE_SOUND_BACKEND_DEFAULT_OPENAL} UseOpenALSoundBackend; {$endif}

  // automatic loading/saving is more troublesome than it's worth
  // Config.AddLoadListener(@LoadFromConfig);
  // Config.AddSaveListener(@SaveToConfig);
end;

destructor TSoundAllocator.Destroy;
begin
  // automatic loading/saving is more troublesome than it's worth
  // if Config <> nil then
  // begin
  //   Config.RemoveLoadListener(@LoadFromConfig);
  //   Config.RemoveSaveListener(@SaveToConfig);
  // end;
  FreeAndNil(Backend);
  inherited;
end;

procedure TSoundAllocator.ContextOpenCore;
var
  I: Integer;
begin
  FAllocatedSources := TSoundList.Create(true);
  FAllocatedSources.Count := MinAllocatedSources;
  for I := 0 to FAllocatedSources.Count - 1 do
    FAllocatedSources[I] := TSound.Create(Backend);

  ApplicationProperties.OnUpdate.Add({$ifdef CASTLE_OBJFPC}@{$endif} Update);
end;

procedure TSoundAllocator.ContextCloseCore;
var
  I: Integer;
begin
  if FAllocatedSources <> nil then
  begin
    { Stop using and free allocated sounds. }
    for I := 0 to FAllocatedSources.Count - 1 do
      { Although usually we are sure that every FAllocatedSources[I] <> nil,
        in this case we must take into account that maybe our constructor
        raise ENonMoreSources and so some FAllocatedSources[I] were
        not initialized. }
      if FAllocatedSources[I] <> nil then
      begin
        if FAllocatedSources[I].Used then
          FAllocatedSources[I].Release;
        { This will free FAllocatedSources[I], as FAllocatedSources owns children }
        FAllocatedSources[I] := nil;
      end;

    FreeAndNil(FAllocatedSources);
  end;

  if ApplicationProperties(false) <> nil then
    ApplicationProperties(false).OnUpdate.Remove({$ifdef CASTLE_OBJFPC}@{$endif} Update);
end;

function TSoundAllocator.AllocateSound(
  const Importance: Integer): TSound;
var
  I: Integer;
  MinImportanceIndex: Integer;
begin
  Result := nil;

  { Sound context not initialized yet }
  if FAllocatedSources = nil then Exit;

  { Try: maybe we have already allocated unused sound ?
    If no unused sound will be found, it will calculate
    MinImportanceIndex, this will be useful later. }
  MinImportanceIndex := -1;
  for I := 0 to FAllocatedSources.Count - 1 do
    if not FAllocatedSources[I].Used then
    begin
      Result := FAllocatedSources[I];
      { Breaking here means that MinImportanceIndex will not be calculated
        correctly (because we did not iterate to the end of FAllocatedSources
        list). But that's OK, because if Result <> nil here, then we will
        not need MinImportanceIndex later. }
      Break;
    end else
    begin
      { Update MinImportanceIndex }
      if (MinImportanceIndex = -1) or
         (FAllocatedSources[I].Importance <
          FAllocatedSources[MinImportanceIndex].Importance) then
         MinImportanceIndex := I;
    end;

  { Try: maybe one of the allocated sounds is marked as Used,
    but actually it's not used anymore ? }
  if Result = nil then
  begin
    for I := 0 to FAllocatedSources.Count - 1 do
      if not FAllocatedSources[I].PlayingOrPaused then
      begin
        Result := FAllocatedSources[I];
        Break;
      end;
  end;

  { Try: maybe we can allocate one more sound ? }
  if (Result = nil) and
     (Cardinal(FAllocatedSources.Count) < MaxAllocatedSources) then
  begin
    try
      Result := TSound.Create(Backend);
      FAllocatedSources.Add(Result);
    except
      { If TSound.Create raises ENoMoreSources ---
        then silence the exception and leave Result = nil. }
      on ENoMoreSources do ;
    end;
  end;

  { Try: maybe we can remove one more sound ?

    If Result = nil then we know that MinImportanceIndex <> -1, because
    all sounds must be used and MinAllocatedSources is always > 0,
    so some sound must be used.

    Note that if FAllocatedSources[MinImportanceIndex].Importance
    is equal to Importance, we *do* interrupt already playing sound.
    The assumption is here that the newer sound is more imoportant. }
  if (Result = nil) and
     (FAllocatedSources[MinImportanceIndex].Importance <= Importance) then
  begin
    Result := FAllocatedSources[MinImportanceIndex];
  end;

  if Result <> nil then
  begin
    { Prepare Result }
    if Result.Used then
      Result.Release;
    Result.FImportance := Importance;
    Result.FUsed := true;
  end;
end;

procedure TSoundAllocator.SetMinAllocatedSources(const Value: Cardinal);
var
  I: Integer;
  OldAllocatedSourcesCount: Cardinal;
begin
  if Value <> FMinAllocatedSources then
  begin
    FMinAllocatedSources := Value;
    if (FAllocatedSources <> nil) and
       (Cardinal(FAllocatedSources.Count) < MinAllocatedSources) then
    begin
      OldAllocatedSourcesCount := FAllocatedSources.Count;
      FAllocatedSources.Count := MinAllocatedSources;
      for I := OldAllocatedSourcesCount to FAllocatedSources.Count - 1 do
        FAllocatedSources[I] := TSound.Create(Backend);
    end;
  end;
end;

procedure TSoundAllocator.SetMaxAllocatedSources(const Value: Cardinal);
var
  I: Integer;
begin
  if Value <> FMaxAllocatedSources then
  begin
    FMaxAllocatedSources := Value;
    if (FAllocatedSources <> nil) and
       (Cardinal(FAllocatedSources.Count) > MaxAllocatedSources) then
    begin
      FAllocatedSources.SortByImportance;

      for I := MaxAllocatedSources to FAllocatedSources.Count - 1 do
      begin
        if FAllocatedSources[I].Used then
          FAllocatedSources[I].Release;
        { This will free FAllocatedSources[I], as FAllocatedSources owns children }
        FAllocatedSources[I] := nil;
      end;
      FAllocatedSources.Count := MaxAllocatedSources;
    end;
  end;
end;

procedure TSoundAllocator.Refresh;
begin
end;

procedure TSoundAllocator.Update(Sender: TObject);

  { Call Update on all sources, and detect unused sound sources. }
  procedure UpdateSounds(const SecondsPassed: TFloatTime);
  var
    I: Integer;
  begin
    if FAllocatedSources <> nil then
      for I := 0 to FAllocatedSources.Count - 1 do
        FAllocatedSources[I].Update(SecondsPassed);
  end;

const
  { Delay between calling DetectUnusedSounds, in seconds. }
  SoundRefreshDelay = 0.1;
var
  TimeNow: TTimerResult;
  SecondsPassed: TFloatTime;
begin
  if FIsContextOpenSuccess then
    Backend.Update;

  { Calling UpdateSounds relatively often is important,
    to call OnRelease for sound sources that finished playing. }
  if FAllocatedSources <> nil then
  begin
    TimeNow := Timer;
    SecondsPassed := TimerSeconds(TimeNow, LastSoundRefresh);
    if SecondsPassed > SoundRefreshDelay then
    begin
      LastSoundRefresh := TimeNow;
      UpdateSounds(SecondsPassed);
    end;
  end;
end;

procedure TSoundAllocator.StopAllSources;
var
  I: Integer;
begin
  if FAllocatedSources <> nil then
    for I := 0 to FAllocatedSources.Count - 1 do
      if FAllocatedSources[I].Used then
        FAllocatedSources[I].Release;
end;

procedure TSoundAllocator.LoadFromConfig(const Config: TCastleConfig);
begin
  MinAllocatedSources := Config.GetValue(
    'sound/allocated_sources/min', DefaultMinAllocatedSources);
  MaxAllocatedSources := Config.GetValue(
    'sound/allocated_sources/max', DefaultMaxAllocatedSources);
end;

procedure TSoundAllocator.SaveToConfig(const Config: TCastleConfig);
begin
  Config.SetDeleteValue('sound/allocated_sources/min',
    MinAllocatedSources, DefaultMinAllocatedSources);
  Config.SetDeleteValue('sound/allocated_sources/max',
    MaxAllocatedSources, DefaultMaxAllocatedSources);
end;

{ TSoundParameters ----------------------------------------------------------- }

constructor TSoundParameters.Create;
begin
  inherited;
  Gain := 1;
  MaxGain := 1;
  Pitch := 1;
  RolloffFactor     := SoundEngine.DefaultRolloffFactor;
  ReferenceDistance := SoundEngine.DefaultReferenceDistance;
  MaxDistance       := SoundEngine.DefaultMaxDistance;
end;

{ TSoundEngine --------------------------------------------------------------- }

constructor TSoundEngine.Create;
begin
  inherited;
  FVolume := DefaultVolume;
  FDefaultRolloffFactor := DefaultDefaultRolloffFactor;
  FDefaultReferenceDistance := DefaultDefaultReferenceDistance;
  FDefaultMaxDistance := DefaultDefaultMaxDistance;
  FDistanceModel := DefaultDistanceModel;
  FEnabled := DefaultEnabled;
  FDevice := DefaultDevice;
  FEnableSaveToConfig := true;
  DeviceSaveToConfig := true;
  LoadedBuffers := TSoundBuffersList.Create(true);
  FOnOpenClose := TNotifyEventList.Create;
  FInitialPitchMultiplier := 1.0;

  { Default listener attributes }
  FListenerPosition := TVector3.Zero;
  FListenerDirection := Vector3(0, 0, -1);
  FListenerUp := Vector3(0, 1, 0);

  // automatic loading/saving is more troublesome than it's worth
  // Config.AddLoadListener(@LoadFromConfig);
  // Config.AddSaveListener(@SaveToConfig);

  ApplicationProperties.OnInitializeJavaActivity.Add(
    {$ifdef CASTLE_OBJFPC}@{$endif} ReinitializeJavaActivity);
  ApplicationProperties.OnPause.Add(
    {$ifdef CASTLE_OBJFPC}@{$endif} ApplicationPause);
  ApplicationProperties.OnResume.Add(
    {$ifdef CASTLE_OBJFPC}@{$endif} ApplicationResume);
end;

destructor TSoundEngine.Destroy;
begin
  if ApplicationProperties(false) <> nil then
  begin
    ApplicationProperties(false).OnInitializeJavaActivity.Remove(
      {$ifdef CASTLE_OBJFPC}@{$endif} ReinitializeJavaActivity);
    ApplicationProperties(false).OnPause.Remove(
      {$ifdef CASTLE_OBJFPC}@{$endif} ApplicationPause);
    ApplicationProperties(false).OnResume.Remove(
      {$ifdef CASTLE_OBJFPC}@{$endif} ApplicationResume);
  end;

  // automatic loading/saving is more troublesome than it's worth
  // if Config <> nil then
  // begin
  //   Config.RemoveLoadListener(@LoadFromConfig);
  //   Config.RemoveSaveListener(@SaveToConfig);
  // end;

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
    but it causes like output (on stdout or stderr) like

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
    Buffer: TSoundBuffer;
  begin
    // check LoadedBuffers.Count, because we don't want to do progress with no LoadedBuffers
    if LoadedBuffers.Count <> 0 then
    begin
      if Progress.Active then
      begin
        { call ContextOpen on all buffers }
        for Buffer in LoadedBuffers do
          Buffer.ContextOpen(false);
      end else
      begin
        { same as above, but with added Progress.Init / Step / Fini }
        Progress.Init(LoadedBuffers.Count, 'Loading sounds');
        try
          for Buffer in LoadedBuffers do
          begin
            Buffer.ContextOpen(false);
            Progress.Step;
          end;
        finally Progress.Fini end;
      end;
    end;
  end;

var
  BackendOpenInformation: string;
begin
  Assert(not IsContextOpenSuccess, 'Sound context is already active');

  if not Enabled then
  begin
    FInformation := 'Sound initialization aborted: Sound engine is disabled';
  end else
  begin
    FIsContextOpenSuccess := Backend.ContextOpen(FDevice, BackendOpenInformation);
    if not IsContextOpenSuccess then
      FInformation :=
        'Sound backend initialization failed:' + NL +
        BackendOpenInformation else
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

      try
        Backend.SetGain(Volume);
        Backend.SetDistanceModel(DistanceModel);
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

  WritelnLogMultiline('Sound', Information);

  OnOpenClose.ExecuteAll(Self);
end;

procedure TSoundEngine.ContextCloseCore;
var
  Buffer: TSoundBuffer;
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

procedure TSoundEngine.ALContextOpen;
begin
  ContextOpen;
end;

procedure TSoundEngine.ALContextClose;
begin
  ContextClose;
end;

function TSoundEngine.PlaySound(const Parameters: TSoundParameters): TSound;
begin
  Result := nil;

  if IsContextOpenSuccess and
     (Parameters.Buffer <> nil) and
     { Buffer.BackendIsOpen may be false if file failed to load, e.g. file not found }
     Parameters.Buffer.BackendIsOpen then
  begin
    Result := AllocateSound(Parameters.Importance);
    if Result <> nil then
    begin
      Result.Buffer  := Parameters.Buffer;
      Result.Looping := Parameters.Looping;
      Result.Gain    := Parameters.Gain;
      Result.MinGain := Parameters.MinGain;
      Result.MaxGain := Parameters.MaxGain;
      Result.Pitch   := Parameters.Pitch * InitialPitchMultiplier;
      Result.Offset  := Parameters.Offset;

      if Parameters.Spatial then
      begin
        { Set default attenuation by distance. }
        Result.RolloffFactor     := Parameters.RolloffFactor;
        Result.ReferenceDistance := Parameters.ReferenceDistance;
        Result.MaxDistance       := Parameters.MaxDistance;

        Result.Relative := false;
        Result.Position := Parameters.Position;
      end else
      begin
        { No attenuation by distance. }
        Result.RolloffFactor := 0;
        { ReferenceDistance, MaxDistance don't matter in this case }

        { Although AL_ROLLOFF_FACTOR := 0 turns off
          attenuation by distance, we still have to turn off
          any changes from player's orientation (so that the sound
          is not played on left or right side, but normally).
          That's why setting source position exactly on the player
          is needed here. }
        Result.Relative := true;
        Result.Position := TVector3.Zero;
      end;

      Result.Backend.Play(true);
    end;
  end;
end;

function TSoundEngine.PlaySound(const Buffer: TSoundBuffer): TSound;
var
  Parameters: TSoundParameters;
begin
  Parameters := TSoundParameters.Create;
  try
    Parameters.Buffer := Buffer;
    Result := PlaySound(Parameters);
  finally FreeAndNil(Parameters) end;
end;

function TSoundEngine.PlaySound(const Buffer: TSoundBuffer;
  const Spatial, Looping: boolean; const Importance: Cardinal;
  const Gain, MinGain, MaxGain: Single;
  const Position: TVector3;
  const Pitch: Single): TSound;
var
  Parameters: TSoundParameters;
begin
  Parameters := TSoundParameters.Create;
  try
    Parameters.Buffer     := Buffer;
    Parameters.Spatial    := Spatial;
    Parameters.Looping    := Looping;
    Parameters.Importance := Importance;
    Parameters.Gain       := Gain;
    Parameters.MinGain    := MinGain;
    Parameters.MaxGain    := MaxGain;
    Parameters.Position   := Position;
    Parameters.Pitch      := Pitch;
    Result := PlaySound(Parameters);
  finally FreeAndNil(Parameters) end;
end;

function TSoundEngine.PlaySound(const Buffer: TSoundBuffer;
  const Spatial, Looping: boolean; const Importance: Cardinal;
  const Gain, MinGain, MaxGain: Single;
  const Position: TVector3;
  const Pitch, ReferenceDistance, MaxDistance: Single): TSound;
var
  Parameters: TSoundParameters;
begin
  Parameters := TSoundParameters.Create;
  try
    Parameters.Buffer     := Buffer;
    Parameters.Spatial    := Spatial;
    Parameters.Looping    := Looping;
    Parameters.Importance := Importance;
    Parameters.Gain       := Gain;
    Parameters.MinGain    := MinGain;
    Parameters.MaxGain    := MaxGain;
    Parameters.Position   := Position;
    Parameters.Pitch      := Pitch;
    Parameters.ReferenceDistance := ReferenceDistance;
    Parameters.MaxDistance       := MaxDistance;
    Result := PlaySound(Parameters);
  finally FreeAndNil(Parameters) end;
end;

function TSoundEngine.LoadBuffer(const URL: string; out Duration: TFloatTime): TSoundBuffer;
begin
  Result := LoadBuffer(URL);
  Duration := Result.Duration;
end;

function TSoundEngine.LoadBuffer(const URL: string; const ExceptionOnError: boolean): TSoundBuffer;
var
  I: Integer;
  FullURL: string;
begin
  ContextOpen;

  FullURL := AbsoluteURI(URL);

  { try to load from cache Result }
  for I := 0 to LoadedBuffers.Count - 1 do
    if LoadedBuffers[I].URL = FullURL then
    begin
      Result := LoadedBuffers[I];
      Inc(Result.References);
      if LogSoundLoading then
        WritelnLog('Sound', Format('Loaded sound buffer "%s" from cache, now it has %d references',
          [URIDisplay(FullURL), Result.References]));
      Exit;
    end;

  Result := TSoundBuffer.Create(Backend);
  Result.FURL := FullURL;
  Result.References := 1;
  LoadedBuffers.Add(Result);

  if IsContextOpenSuccess then
    { let LoadBuffer raise exception on missing sound file }
    Result.ContextOpen(ExceptionOnError);
end;

procedure TSoundEngine.FreeBuffer(var Buffer: TSoundBuffer);
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
      [URIDisplay(Buffer.URL)]);
end;

procedure TSoundEngine.SetVolume(const Value: Single);
begin
  if Value <> FVolume then
  begin
    FVolume := Value;
    if IsContextOpenSuccess then
      Backend.SetGain(Value);
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

procedure TSoundEngine.SetDevice(const Value: string);
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
    DeviceSaveToConfig := true; // caller will eventually change it to false
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
    FEnableSaveToConfig := true; // caller will eventually change it to false
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
         Engine.DeviceSaveToConfig := false;
       end;
    1: begin
         Engine.Enabled := false;
         Engine.EnableSaveToConfig := false;
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

procedure TSoundEngine.UpdateListener(const Position, Direction, Up: TVector3);
begin
  FListenerPosition := Position;
  FListenerDirection := Direction;
  FListenerUp := Up;
  if IsContextOpenSuccess then
    Backend.SetListener(Position, Direction, Up);
end;

function TSoundEngine.DeviceNiceName: string;
begin
  Result := DeviceCaption;
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

procedure TSoundEngine.LoadFromConfig(const Config: TCastleConfig);
begin
  inherited;
  Device := Config.GetValue('sound/device', DefaultDevice);
  Enabled := Config.GetValue('sound/enable', DefaultEnabled);
end;

procedure TSoundEngine.SaveToConfig(const Config: TCastleConfig);
begin
  if DeviceSaveToConfig then
    Config.SetDeleteValue('sound/device', Device, DefaultDevice);
  if EnableSaveToConfig then
    Config.SetDeleteValue('sound/enable', Enabled, DefaultEnabled);
  inherited;
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

{ TSoundType ----------------------------------------------------------------- }

class operator TSoundType.{$ifdef FPC}={$else}Equals{$endif}
  (const SoundType1, SoundType2: TSoundType): boolean;
begin
  Result := SoundType1.Index = SoundType2.Index;
end;

{ TSoundInfoList ------------------------------------------------------------- }

function TRepoSoundEngine.TSoundInfoList.IndexOfName(const SoundName: String): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].Name = SoundName then
      Exit;
  Result := -1;
end;

{ TSoundGroupList ------------------------------------------------------------- }

function TRepoSoundEngine.TSoundGroupList.IndexOfName(
  const GroupName: String): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].Name = GroupName then
      Exit;
  Result := -1;
end;

{ TRepoSoundEngine.TSoundInfo ------------------------------------------------ }

procedure TRepoSoundEngine.TSoundInfo.ReadElement(const Element: TDOMElement;
  const AParentGroup: TSoundGroup;
  const BaseUrl: String; const ASoundEngine: TRepoSoundEngine);
begin
  FOwningSoundEngine := ASoundEngine;
  ShortName := Element.AttributeString('name');
  Name := ShortName;

  ParentGroup := AParentGroup;
  if ParentGroup <> nil then
    Name := ParentGroup.Name + '/' + Name;
end;

procedure TRepoSoundEngine.TSoundInfo.ResolveNames(const AllSounds: TSoundInfoList);
begin
end;

{ TRepoSoundEngine.TSoundInfoAlias ------------------------------------------- }

constructor TRepoSoundEngine.TSoundInfoAlias.Create;
begin
  inherited;
  Target := TCastleStringList.Create;
end;

destructor TRepoSoundEngine.TSoundInfoAlias.Destroy;
begin
  FreeAndNil(Target);
  inherited;
end;

procedure TRepoSoundEngine.TSoundInfoAlias.ReadElement(const Element: TDOMElement;
  const AParentGroup: TSoundGroup;
  const BaseUrl: String; const ASoundEngine: TRepoSoundEngine);
var
  I: TXMLElementIterator;
  TargetName: String;
begin
  inherited;

  I := Element.ChildrenIterator('target');
  try
    while I.GetNext do
    begin
      TargetName := I.Current.AttributeString('name');
      if ParentGroup <> nil then
        TargetName := ParentGroup.Name + '/' + TargetName;
      Target.Add(TargetName);
    end;
  finally FreeAndNil(I) end;

  if Target.Count = 0 then
    raise EInvalidSoundRepositoryXml.CreateFmt('Alias "%s" does not define any targets',
      [Name]);
end;

procedure TRepoSoundEngine.TSoundInfoAlias.ResolveNames(const AllSounds: TSoundInfoList);
var
  I, TargetIndex: Integer;
begin
  inherited;

  for I := 0 to Target.Count - 1 do
  begin
    TargetIndex := AllSounds.IndexOfName(Target[I]);
    if TargetIndex = -1 then
      raise EInvalidSoundRepositoryXml.CreateFmt('Alias "%s" target "%s" not found',
        [Name, Target[I]]);
    Target.Objects[I] := AllSounds[TargetIndex];
  end;
end;

function TRepoSoundEngine.TSoundInfoAlias.FinalSound(const RecursionDepth: Cardinal): TSoundInfoBuffer;
const
  MaxRecursionDepth = 100;
var
  RandomTarget: TSoundInfo;
begin
  RandomTarget := Target.Objects[Random(Target.Count)] as TSoundInfo;
  { Instead of returning RandomTarget, we return RandomTarget.FinalSound.
    This way an <alias> may point to another <alias>.
    To avoid infinite loop, we use MaxRecursionDepth. }
  if RecursionDepth > MaxRecursionDepth then
    raise EInvalidSoundRepositoryXml.CreateFmt('Possible infinite loop when trying to resolve sound alias, detected at alias "%s". Recursion depth %s reached.',
      [Name, RecursionDepth]);
  Result := RandomTarget.FinalSound(RecursionDepth + 1);
end;

{ TRepoSoundEngine.TSoundInfoBuffer ------------------------------------------ }

procedure TRepoSoundEngine.TSoundInfoBuffer.ReadElement(const Element: TDOMElement;
  const AParentGroup: TSoundGroup;
  const BaseUrl: String; const ASoundEngine: TRepoSoundEngine);
var
  ImportanceStr, URLPrefix: String;
  SoundImportanceIndex: Integer;
begin
  inherited;

  { init to default values }
  Gain := 1;
  MinGain := 0;
  MaxGain := 1;
  DefaultImportance := MaxSoundImportance;

  { retrieve URL using AttributeString
    (that internally uses Element.Attributes.GetNamedItem),
    because we have to distinguish between the case when url/file_name
    attribute is not present (in this case URL is left as it was)
    and when it's present and set to empty string
    (in this case URL must also be set to empty string).
    Standard Element.GetAttribute wouldn't allow me this. }
  if (Element.AttributeString('url', URL) or
      Element.AttributeString('file_name', URL)) and
     (URL <> '') then
  begin
    URL := CombineURI(BaseUrl, URL)
  end else
  begin
    URLPrefix := CombineURI(BaseUrl, ShortName);
    if URIFileExists(URLPrefix + '.ogg') then
      URL := URLPrefix + '.ogg'
    else
    if URIFileExists(URLPrefix + '.wav') then
      URL := URLPrefix + '.wav'
    else
      WritelnWarning('No matching sound file found for sound "%s"', [Name]);
  end;

  Element.AttributeSingle('gain', Gain);
  Element.AttributeSingle('min_gain', MinGain);
  Element.AttributeSingle('max_gain', MaxGain);

  { MaxGain is max 1. Although some sound backends (like some OpenAL backends)
    implementations allow > 1,
    Windows implementation of OpenAL (from Creative) doesn't.
    For consistent results, we don't allow it anywhere. }
  if MaxGain > 1 then
    MaxGain := 1;

  if Element.AttributeString('default_importance', ImportanceStr) then
  begin
    SoundImportanceIndex := ASoundEngine.SoundImportanceNames.IndexOf(ImportanceStr);
    if SoundImportanceIndex = -1 then
      DefaultImportance := StrToInt(ImportanceStr)
    else
      DefaultImportance := PtrUInt(ASoundEngine.SoundImportanceNames.Objects[SoundImportanceIndex]);
  end;

  { set Buffer at the end, when URL is set }
  if URL <> '' then
    Buffer := ASoundEngine.LoadBuffer(URL, false);
end;

function TRepoSoundEngine.TSoundInfoBuffer.FinalSound(const RecursionDepth: Cardinal): TSoundInfoBuffer;
begin
  Result := Self;
end;

destructor TRepoSoundEngine.TSoundInfoBuffer.Destroy;
begin
  if OwningSoundEngine <> nil then
    OwningSoundEngine.FreeBuffer(Buffer);
  inherited;
end;

{ TRepoSoundEngine ----------------------------------------------------------- }

constructor TRepoSoundEngine.Create;
begin
  // We need to set global FSoundEngine early, to make UseOpenALSoundBackend work
  FSoundEngine := Self;

  inherited;

  { Sound importance names and sound names are case-sensitive because
    XML traditionally is also case-sensitive.
    Maybe in the future we'll relax this,
    there's no definite reason actually to not let them ignore case. }
  FSoundImportanceNames := TStringList.Create;
  FSoundImportanceNames.CaseSensitive := true;
  AddSoundImportanceName('max', MaxSoundImportance);
  AddSoundImportanceName('level_event', LevelEventSoundImportance);
  AddSoundImportanceName('player', PlayerSoundImportance);
  AddSoundImportanceName('default_creature', DefaultCreatureSoundImportance);
  AddSoundImportanceName('minor_non_spatial', MinorNonSpatialSoundImportance);

  FSoundGroups := TSoundGroupList.Create;

  FSounds := TSoundInfoList.Create;
  { add stNone sound }
  FSounds.Add(TSoundInfoBuffer.Create);

  FLoopingChannels := TLoopingChannelList.Create(true);

  // automatic loading/saving is more troublesome than it's worth
  // Config.AddLoadListener(@LoadFromConfig);
  // Config.AddSaveListener(@SaveToConfig);
end;

destructor TRepoSoundEngine.Destroy;
begin
  // automatic loading/saving is more troublesome than it's worth
  // if Config <> nil then
  // begin
  //   Config.RemoveLoadListener(@LoadFromConfig);
  //   Config.RemoveSaveListener(@SaveToConfig);
  // end;

  FreeAndNil(FSoundImportanceNames);
  FreeAndNil(FSounds);
  FreeAndNil(FSoundGroups);
  FreeAndNil(FLoopingChannels);
  inherited;
end;

procedure TRepoSoundEngine.ContextOpenCore;
begin
  inherited;
  RestartLoopingChannels;
end;

procedure TRepoSoundEngine.RestartLoopingChannels;
var
  L: TLoopingChannel;
begin
  { allocate sound for all TLoopingChannel }
  if IsContextOpenSuccess then
    for L in FLoopingChannels do
      if L <> nil then
        L.AllocateSource;
end;

function TRepoSoundEngine.FinalSound(const SoundType: TSoundType): TSoundInfoBuffer;
begin
  if SoundType.Index = 0 then
    Exit(nil);

  Result := FSounds[SoundType.Index].FinalSound(0);
  if Result.URL = '' then
    Exit(nil);
end;

function TRepoSoundEngine.Sound(const SoundType: TSoundType;
  const Looping: boolean): TSound;
var
  SoundInfo: TSoundInfoBuffer;
begin
  { If there is no actual sound, exit early without initializing sound backend.
    - SoundType is stNone (which includes sound names not defined in sounds.xml,
      since SoundFromName returns stNone for them).
    - SoundType is <> stNone but URL = '' (if sound name is defined in
      sounds.xml with explicit url="", like this:
      <sound name="player_sudden_pain" url="" />)
  }
  SoundInfo := FinalSound(SoundType);
  if SoundInfo = nil then Exit(nil);

  ContextOpen;

  { Check this only after ContextOpen,
    since before ContextOpen the Buffer always = nil. }
  if SoundInfo.Buffer = nil then Exit(nil);

  Result := PlaySound(
    SoundInfo.Buffer, false, Looping,
    SoundInfo.DefaultImportance,
    SoundInfo.Gain,
    SoundInfo.MinGain,
    SoundInfo.MaxGain,
    TVector3.Zero);
end;

function TRepoSoundEngine.Sound3D(const SoundType: TSoundType;
  const Position: TVector3;
  const Looping: boolean): TSound;
var
  SoundInfo: TSoundInfoBuffer;
begin
  { If there is no actual sound, exit early without initializing sound backend.
    See Sound for more comments why we do this. }
  SoundInfo := FinalSound(SoundType);
  if SoundInfo = nil then Exit(nil);

  ContextOpen;

  if SoundInfo.Buffer = nil then Exit(nil);

  Result := PlaySound(
    SoundInfo.Buffer, true, Looping,
    SoundInfo.DefaultImportance,
    SoundInfo.Gain,
    SoundInfo.MinGain,
    SoundInfo.MaxGain,
    Position);
end;

procedure TRepoSoundEngine.SetRepositoryURL(const Value: string);

  { Check that SoundInfo.Name is unique. }
  procedure CheckUniqueSoundInfo(var SoundInfo: TSoundInfo);
  begin
    if FSounds.IndexOfName(SoundInfo.Name) <> -1 then
      raise EInvalidSoundRepositoryXml.CreateFmt('Sound name "%s" is not unique',
        [SoundInfo.Name]);
  end;

  { Check that Group.Name is unique. }
  procedure CheckUniqueGroup(var Group: TSoundGroup);
  begin
    if FSoundGroups.IndexOfName(Group.Name) <> -1 then
      raise EInvalidSoundRepositoryXml.CreateFmt('Group name "%s" is not unique',
        [Group.Name]);
  end;

  procedure ReadGroup(const Group: TSoundGroup;
    const Element: TDOMElement;
    const ParentGroup: TSoundGroup;
    const BaseUrl: String); forward;

  { Read <group> child XML element, like <sound> or <alias> or another <group>. }
  procedure ReadGroupChild(const Element: TDOMElement;
    const ParentGroup: TSoundGroup;
    const BaseUrl: String);
  var
    SoundInfo: TSoundInfo;
    Group: TSoundGroup;
  begin
    if Element.TagName = 'sound' then
    begin
      SoundInfo := TSoundInfoBuffer.Create;
      try
        SoundInfo.ReadElement(Element, ParentGroup, BaseUrl, Self);
        CheckUniqueSoundInfo(SoundInfo);
      except
        { In case SoundInfo.ReadElement fails (e.g. because LoadBuffer failed),
          or in case CheckUniqueSoundInfo fails (name not unique), avoid memory leaks. }
        FreeAndNil(SoundInfo);
        raise;
      end;
      FSounds.Add(SoundInfo);
    end else
    if Element.TagName = 'alias' then
    begin
      SoundInfo := TSoundInfoAlias.Create;
      try
        SoundInfo.ReadElement(Element, ParentGroup, BaseUrl, Self);
        CheckUniqueSoundInfo(SoundInfo);
      except
        FreeAndNil(SoundInfo);
        raise;
      end;
      FSounds.Add(SoundInfo);
    end else
    if Element.TagName = 'group' then
    begin
      Group := TSoundGroup.Create;
      try
        ReadGroup(Group, Element, ParentGroup, BaseUrl);
        CheckUniqueGroup(Group);
      except
        FreeAndNil(Group);
        raise;
      end;
      { Adding Group to FSoundGroups makes TSoundGroup freed later. }
      FSoundGroups.Add(Group);
    end else
      raise EInvalidSoundRepositoryXml.CreateFmt('Invalid XML element "%s" in sounds XML file',
        [Element.TagName]);
  end;

  { Read a group of sounds from XML element <group>. }
  procedure ReadGroup(const Group: TSoundGroup;
    const Element: TDOMElement;
    const ParentGroup: TSoundGroup;
    const BaseUrl: String);
  var
    I: TXMLElementIterator;
    Subdirectory: String;
  begin
    Group.ParentGroup := ParentGroup;

    { calculate Group.Name }
    Group.Name := Element.AttributeString('name');
    if ParentGroup <> nil then
      Group.Name := ParentGroup.Name + '/' + Group.Name;

    Group.URL := BaseUrl;
    Subdirectory := Element.AttributeStringDef('subdirectory', '');
    if Subdirectory <> '' then
      Group.URL := Group.URL + Subdirectory + '/';

    I := Element.ChildrenIterator;
    try
      while I.GetNext do
        ReadGroupChild(I.Current, Group, Group.URL);
    finally FreeAndNil(I) end;
  end;

  procedure ResolveNames;
  var
    S: TSoundInfo;
  begin
    for S in FSounds do
      S.ResolveNames(FSounds);
  end;

var
  SoundConfig: TXMLDocument;
  Stream: TStream;
  I: TXMLElementIterator;
  BaseUrl: string;
  TimeStart: TCastleProfilerTime;
begin
  if FRepositoryURL = Value then Exit;
  FRepositoryURL := Value;

  FSoundGroups.Clear;
  FSounds.Clear;
  { add stNone sound }
  FSounds.Add(TSoundInfo.Create);

  { if no sounds XML file, then that's it --- no more sounds }
  if RepositoryURL = '' then Exit;

  TimeStart := Profiler.Start('Loading All Sounds From ' + RepositoryURL + ' (TRepoSoundEngine)');

  { This must be an absolute path, since FSounds[].URL should be
    absolute (to not depend on the current dir when loading sound files. }
  BaseUrl := AbsoluteURI(RepositoryURL);

  Stream := Download(BaseUrl);
  try
    ReadXMLFile(SoundConfig, Stream, BaseUrl);
  finally FreeAndNil(Stream) end;

  // cut off last part from BaseUrl, for ReadSound / ReadGroup calls
  BaseUrl := ExtractURIPath(BaseUrl);

  try
    Check(SoundConfig.DocumentElement.TagName = 'sounds',
      'Root node of sounds/index.xml must be <sounds>');

    { TODO: This could display a progress bar using Progress.Init / Fini
      if IsContextOpenSuccess, since it loads sounds in this case (calls LoadBuffer),
      so can take a while. }

    I := SoundConfig.DocumentElement.ChildrenIterator;
    try
      while I.GetNext do
        ReadGroupChild(I.Current, nil, BaseUrl);
    finally FreeAndNil(I) end;
  finally
    FreeAndNil(SoundConfig);
  end;

  ResolveNames;

  { read common sound names }
  stPlayerInteractFailed       := SoundFromName('player_interact_failed', false);
  stPlayerSuddenPain           := SoundFromName('player_sudden_pain', false);
  stPlayerPickItem             := SoundFromName('player_pick_item', false);
  stPlayerDropItem             := SoundFromName('player_drop_item', false);
  stPlayerDies                 := SoundFromName('player_dies', false);
  stPlayerSwimmingChange       := SoundFromName('player_swimming_change', false);
  stPlayerSwimming             := SoundFromName('player_swimming', false);
  stPlayerDrowning             := SoundFromName('player_drowning', false);
  stPlayerFootstepsDefault     := SoundFromName('player_footsteps_default', false);
  stPlayerToxicPain            := SoundFromName('player_toxic_pain', false);

  stMenuCurrentItemChanged := SoundFromName('menu_current_item_changed', false);
  stMenuClick              := SoundFromName('menu_click'               , false);

  { in case you set RepositoryURL when sound context is already
    initialized, start playing music immediately if necessary }
  RestartLoopingChannels;

  Profiler.Stop(TimeStart);
end;

procedure TRepoSoundEngine.ReloadSounds;
var
  OldRepositoryURL: string;
begin
  if RepositoryURL <> '' then
  begin
    OldRepositoryURL := RepositoryURL;
    RepositoryURL := '';
    RepositoryURL := OldRepositoryURL;
  end;
end;

function TRepoSoundEngine.SoundFromName(const SoundName: string;
  const Required: boolean): TSoundType;
var
  SoundIndex: Integer;
begin
  SoundIndex := FSounds.IndexOfName(SoundName);
  if SoundIndex <> -1 then
  begin
    Result.Index := SoundIndex;
    Exit;
  end;

  if Required then
    WritelnWarning('Sound', Format('Unknown sound name "%s"', [SoundName]));
  Result.Index := 0;
end;

procedure TRepoSoundEngine.AddSoundImportanceName(const Name: string;
  Importance: Integer);
begin
  FSoundImportanceNames.AddObject(Name, TObject(Pointer(PtrUInt(Importance))));
end;

procedure TRepoSoundEngine.PrepareResources;
begin
  if not IsContextOpen and (FSounds.Count > 1) then
    ContextOpen;
end;

procedure TRepoSoundEngine.LoadFromConfig(const Config: TCastleConfig);
begin
  inherited;
  Volume := Config.GetFloat('sound/volume', DefaultVolume);
  MusicPlayer.Volume := Config.GetFloat('sound/music/volume',
    TLoopingChannel.DefaultVolume);
end;

procedure TRepoSoundEngine.SaveToConfig(const Config: TCastleConfig);
begin
  Config.SetDeleteFloat('sound/volume', Volume, DefaultVolume);
  { This may be called from destructors and the like, so better check
    that MusicPlayer is not nil. }
  if FLoopingChannels <> nil then
    Config.SetDeleteFloat('sound/music/volume',
      MusicPlayer.Volume, TLoopingChannel.DefaultVolume);
  inherited;
end;

function TRepoSoundEngine.GetLoopingChannel(const Index: Cardinal): TLoopingChannel;
begin
  { On demand, resize FLoopingChannels list and create new TLoopingChannel.
    Note that FLoopingChannels may have nils along the way. }
  if Index >= FLoopingChannels.Count then
    FLoopingChannels.Count := Index + 1;
  Assert(Index < FLoopingChannels.Count);

  if FLoopingChannels[Index] = nil then
    FLoopingChannels[Index] := TLoopingChannel.Create(Self);

  Result := FLoopingChannels[Index];
end;

function TRepoSoundEngine.GetMusicPlayer: TLoopingChannel;
begin
  Result := LoopingChannel[0];
end;

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
  SoundInfo: TRepoSoundEngine.TSoundInfoBuffer;
  Parameters: TSoundParameters;
begin
  SoundInfo := FEngine.FinalSound(Sound);
  if SoundInfo = nil then Exit;

  SoundInfoGain := SoundInfo.Gain;
  SoundInfoPitch := 1.0; // SoundInfo.Pitch; // for now there is no setting for it

  Parameters := TSoundParameters.Create;
  try
    Parameters.Buffer     := SoundInfo.Buffer;
    Parameters.Spatial    := false;
    Parameters.Looping    := true;
    Parameters.Importance := SoundInfo.DefaultImportance;
    Parameters.Gain       := Volume * SoundInfoGain;
    Parameters.MinGain    := SoundInfo.MinGain;
    Parameters.MaxGain    := SoundInfo.MaxGain;
    Parameters.Pitch      := Pitch * SoundInfoPitch;
    FAllocatedSource := FEngine.PlaySound(Parameters);
  finally FreeAndNil(Parameters) end;

  if FAllocatedSource <> nil then
    FAllocatedSource.OnRelease :=
      {$ifdef CASTLE_OBJFPC}@{$endif} AllocatedSourceRelease;
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

procedure TLoopingChannel.AllocatedSourceRelease(Sender: TSound);
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
      FAllocatedSource.Gain := Volume * SoundInfoGain;
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

{ globals -------------------------------------------------------------------- }

function SoundEngine: TRepoSoundEngine;
begin
  if FSoundEngine = nil then
  begin
    TRepoSoundEngine.Create;
    // TRepoSoundEngine.Create already assigns FSoundEngine
    Assert(FSoundEngine <> nil);
  end;

  Result := FSoundEngine;
end;

finalization
  FreeAndNil(FSoundEngine);
end.
