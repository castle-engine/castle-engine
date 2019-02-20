{
  Copyright 2010-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ 3D sound engine (TSoundEngine and TRepoSoundEngine). }
unit CastleSoundEngine;

{$I castleconf.inc}

interface

uses SysUtils, Classes, Math, Generics.Collections,
  CastleInternalOpenAL, CastleVectors, CastleTimeUtils, CastleXMLConfig,
  CastleClassUtils, CastleStringUtils, CastleInternalSoundFile;

type
  ENoMoreOpenALSources = class(Exception);
  ESoundBufferNotLoaded = class(Exception);
  EInvalidSoundBufferFree = class(Exception);
  ESoundFileError = CastleInternalSoundFile.ESoundFileError;
  EInvalidSoundRepositoryXml = class(Exception);

  TSound = class;
  TSoundAllocator = class;

  { Sound buffer represents contents of a sound file, like Wav or OggVorbis,
    that (may be) loaded to OpenAL.
    It can be only allocated by @link(TSoundEngine.LoadBuffer)
    and freed by @link(TSoundEngine.FreeBuffer).
    @bold(Do not free TSoundBuffer instances yourself.) }
  TSoundBuffer = class
  private
    ALBuffer: TALuint;
    { Absolute URL.
      Never empty (do not create TSoundBuffer instances for invalid / empty URL,
      like the ones that can be created by TRepoSoundEngine for not defined sounds.) }
    URL: string;
    FDuration: TFloatTime;
    References: Cardinal;
    procedure ALContextOpen(const ExceptionOnError: boolean);
    procedure ALContextClose;
  public
    destructor Destroy; override;

    { Duration of the sound, in seconds. Zero if not loaded yet. }
    property Duration: TFloatTime read FDuration;
  end;

  TSoundEvent = procedure (Sender: TSound) of object;

  { Sound.
    Internally, this corresponds to an allocated OpenAL sound source. }
  TSound = class
  private
    FUsed: boolean;
    FOnRelease: TSoundEvent;
    FImportance: Integer;
    FALSource: TALuint;
    { This must be @true for the whole lifetime of this object
      except the situation at the beginning of the constructor,
      and in destructor (if constructor exited with ENoMoreOpenALSources). }
    FALSourceAllocated: boolean;
    FUserData: TObject;
    FPosition, FVelocity: TVector3;
    FLooping, FRelative: boolean;
    FGain, FMinGain, FMaxGain, FPitch: Single;
    FBuffer: TSoundBuffer;
    FRolloffFactor, FReferenceDistance, FMaxDistance: Single;
    FAllocator: TSoundAllocator;
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
  public
    { Create sound. This allocates actual OpenAL source.
      @raises(ENoMoreOpenALSources If no more sources available.
        It should be caught and silenced by TSoundAllocator.AllocateSound.) }
    constructor Create(const AnAllocator: TSoundAllocator);
    destructor Destroy; override;

    { Internal: OpenAL sound identifier. }
    property ALSource: TALuint read FALSource;

    { Do we play something.
      Sources that are not Used are simply OpenAL allocated sources
      that are not used right now, and will be used when we will
      need them. }
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

    { Called when this OpenAL allocated sound will no longer
      be used. It may stop be used because there are more demanding
      sources (see @link(Importance) and to keep MaxAllocatedSources)
      and we must assign this OpenAL sound slot to something else,
      or it may stop be used because it simply stopped playing.

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

      You can call this yourself if you want to stop playing the sound.
      It's preferable to call this (instead of manually calling
      alSourceStop), because this will immediately mark Used property
      as @false and will call OnRelease. Otherwise we would have to
      get source state at some time (they are checked in AllocateSound)
      and check it, then see that it's no longer playing.

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
    audible at a given time) is limited. This limit is not only in OpenAL,
    it may also stem from sound hardware limitations,
    or limitations of APIs underneath OpenAL.
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

    { Detect unused sound sources.
      For every source that is marked as Used, this checks
      whether this source is actually in playing/paused state
      right now. If not, it calls @link(TSound.Release) (thus setting
      TSound.Used to @false and triggering TSound.OnRelease) for this source. }
    procedure DetectUnusedSounds;
    procedure Update(Sender: TObject);
    procedure SetMinAllocatedSources(const Value: Cardinal);
    procedure SetMaxAllocatedSources(const Value: Cardinal);
  private
    procedure ALContextOpenCore; virtual;
    procedure ALContextCloseCore; virtual;
  public
    const
      DefaultMinAllocatedSources = 4;
      DefaultMaxAllocatedSources = 16;

    constructor Create;
    destructor Destroy; override;

    { Is the OpenAL version at least @code(AMajor.AMinor).
      Available only when OpenAL is initialized, that is:
      between @link(TSoundEngine.ALContextOpen) and @link(TSoundEngine.ALContextClose),
      only when @link(TSoundEngine.ALActive). }
    function ALVersionAtLeast(const AMajor, AMinor: Integer): boolean; virtual; abstract;

    { Internal: Allocate sound for playing. You should initialize the OpenAL sound
      properties and start playing the sound (you have
      OpenAL sound identifier in TSound.ALSource).

      Note that if you don't call alSourcePlay, the source may be detected
      as unused (and recycled for another sound) at the next AllocateSound,
      PlaySound, DetectUnusedSounds and such calls.

      If we can't allocate new OpenAL sound, we return nil.
      This may happen your OpenAL context is not initialized.
      It may also happen if we cannot create more sources (because
      we hit MaxAllocatedSources limit, or OpenAL just refuses to create
      more sources) and all existing sounds are used and their
      Importance is > given here Importance.

      Note for looping sounds: just like any other sound, looping sound
      may be stopped because the sounds are needed for other sounds.
      If you want to try to restart the looping sound, you will have
      to implement it yourself. Or you can just set Importance of looping
      sounds high enough, and don't use too many looping sounds,
      to never let them be eliminated by other sounds. }
    function AllocateSound(const Importance: Integer): TSound;

    { All allocated (not necessarily used) OpenAL sources.
      Accessing this is useful only for debugging tasks,
      in normal circumstances this is internal.
      This is @nil when ALContextOpen was not yet called. }
    property AllocatedSources: TSoundList read FAllocatedSources;

    procedure Refresh; deprecated 'do not call this method yourself, it will be called directly if you use CastleWindow unit (with TCastleApplication, TCastleWindow) or TCastleControl; in other cases, you shoud call ApplicationProperties._Update yourself';

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
    { Minimum / maximum number of allocated OpenAL sources.
      Always keep MinAllocatedSources <= MaxAllocatedSources.

      For the sake of speed, we always keep allocated at least
      MinAllocatedSources OpenAL sources. This must be >= 1.
      Setting MinAllocatedSources too large value will raise
      ENoMoreOpenALSources.

      At most MaxAllocatedSources sources may be simultaneously used (played).
      This prevents us from allocating too many sounds,
      which would be bad for OpenAL speed (not to mention that it may
      be impossible under some OpenAL implementations, like Windows one).
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

  TSoundDistanceModel = (dmNone,
    dmInverseDistance , dmInverseDistanceClamped,
    dmLinearDistance  , dmLinearDistanceClamped,
    dmExponentDistance, dmExponentDistanceClamped);

  TSoundDevice = class
  private
    FName, FCaption: string;
  public
    { Short device name, used for @link(TSoundEngine.Device). }
    property Name: string read FName;
    { Nice device name to show user. }
    property Caption: string read FCaption;
    property NiceName: string read FCaption; deprecated 'use Caption';
  end;
  TSoundDeviceList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TSoundDevice>;

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

    The sound engine is actually a wrapper over OpenAL.
    You can explicitly initialize OpenAL context by ALContextOpen,
    and explicitly close it by ALContextClose. If you did not call ALContextOpen
    explicitly (that is, ALInitialized is @false), then the first LoadBuffer
    or TRepoSoundEngine.Sound or TRepoSoundEngine.Sound3D
    will automatically do it for you. If you do not call ALContextClose
    explicitly, then at destructor we'll do it automatically. }
  TSoundEngine = class(TSoundAllocator)
  private
    type
      TSoundBuffersList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TSoundBuffer>;
    var
      FInformation: string;
      FDevice: string;
      FALActive: boolean;
      FALMajorVersion, FALMinorVersion: Integer;
      FEFXSupported: boolean;
      FVolume: Single;
      ALDevice: PALCdevice;
      ALContext: PALCcontext;
      FEnabled: boolean;
      FALInitialized: boolean;
      FDefaultRolloffFactor: Single;
      FDefaultReferenceDistance: Single;
      FDefaultMaxDistance: Single;
      FDistanceModel: TSoundDistanceModel;
      LoadedBuffers: TSoundBuffersList;
      FDevices: TSoundDeviceList;
      FOnOpenClose: TNotifyEventList;
      FResumeToInitialized, FPaused: boolean;

      { We record listener state regardless of ALActive. This way at the ALContextOpen
        call we can immediately set the good listener parameters. }
      ListenerPosition: TVector3;
      ListenerOrientation: TALTwoVectors3f;

      FEnableSaveToConfig, DeviceSaveToConfig: boolean;

    { Check ALC errors. Requires valid ALDevice. }
    procedure CheckALC(const Situation: string);

    procedure SetVolume(const Value: Single);
    procedure SetDistanceModel(const Value: TSoundDistanceModel);
    { Call alDistanceModel with parameter derived from current DistanceModel.
      Use only when ALActive. }
    procedure UpdateDistanceModel;
    procedure SetDevice(const Value: string);
    procedure SetEnabled(const Value: boolean);
    procedure SetPaused(const Value: boolean);
    procedure ReinitializeJavaActivity(Sender: TObject);
    procedure ApplicationPause(Sender: TObject);
    procedure ApplicationResume(Sender: TObject);
    { Pause the sound engine, useful when Android activity gets inactive.
      When paused, OpenAL is for sure inactive, and it cannot be activated
      (calling ALContextOpen, or playing a sound, will @bold(not) activate it). }
    property Paused: boolean read FPaused write SetPaused;

    procedure ALContextOpenCore; override;
    procedure ALContextCloseCore; override;

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
      Initializes OpenAL library.
      Sets @link(ALInitialized), @link(ALActive),
      @link(Information), @link(EFXSupported).

      You can set @link(Device) before calling this.

      Note that we continue (without any exception) if the initialization
      failed for any reason (maybe OpenAL library is not available,
      or no sound output device is available).
      You can check @link(ALActive) and @link(Information) to know if
      the initialization was actually successfull. But you can also ignore it,
      the sound engine will silently (literally) keep working even if OpenAL
      could not be initialized. }
    procedure ALContextOpen;

    { Release OpenAL resources.
      This sets @link(ALInitialized) and @link(ALActive) to @false.
      It's allowed and harmless to call this when one of them is already @false. }
    procedure ALContextClose;

    procedure LoadFromConfig(const Config: TCastleConfig); override;
    procedure SaveToConfig(const Config: TCastleConfig); override;

    { Is the OpenAL version at least @code(AMajor.AMinor). }
    function ALVersionAtLeast(const AMajor, AMinor: Integer): boolean; override;

    { Do we have active OpenAL context. This is @true when you successfully
      called ALContextOpen (and you didn't call ALContextClose yet).
      This also implies that OpenAL library is loaded. }
    property ALActive: boolean read FALActive;

    { Did we attempt to initialize OpenAL context. This indicates that ALContextOpen
      was called, and not closed with ALContextClose yet. Contrary to ALActive,
      this @italic(doesn't care if ALContextOpen was a success). }
    property ALInitialized: boolean read FALInitialized;

    { Are OpenAL effects (EFX) extensions supported.
      Meaningful only when ALActive, that is it's initialized by ALContextOpen. }
    property EFXSupported: boolean read FEFXSupported;

    property SoundInitializationReport: string read FInformation;
      deprecated 'use Information';

    property Information: string read FInformation;

    { Wrapper for alcGetString. }
    function GetContextString(Enum: TALCenum): string;

    { Load a sound file contents such that they can be immediately played.

      This method tries to initialize OpenAL context, and internally load
      the buffer contents to OpenAL. But even when it fails, it still returns
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

      We use a smart OpenAL sound allocator, so the sound will be actually
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

      Note that it also lists the available OpenAL @link(Devices),
      as they are valid arguments for the @--audio-device option. }
    function ParseParametersHelp: string;

    { Set OpenAL listener position and orientation. }
    procedure UpdateListener(const Position, Direction, Up: TVector3);

    { List of available OpenAL sound devices. Read-only.

      Use @code(Devices[].Name) as @link(Device) values.
      On some OpenAL implementations, some other @link(Device) values may
      be possible, e.g. old Loki implementation allowed some hints
      to be encoded in Lisp-like language inside the @link(Device) string. }
    function Devices: TSoundDeviceList;

    function DeviceNiceName: string; deprecated 'use DeviceCaption';
    function DeviceCaption: string;

    { Events fired after OpenAL context and device are being open or closed.
      More precisely, when ALInitialized changes (and so, possibly, ALActive
      changed). }
    property OnOpenClose: TNotifyEventList read FOnOpenClose;

    { Should we save @link(Enable) to config file in SaveToConfig call.
      This is always reset to @true after setting @link(Enable) value. }
    property EnableSaveToConfig: boolean
      read FEnableSaveToConfig write FEnableSaveToConfig default true;

    class property LogSoundLoading: Boolean
      read GetLogSoundLoading write SetLogSoundLoading;
  published
    { Sound volume, affects all OpenAL sounds (effects and music).
      This must always be within 0..1 range.
      0.0 means that there are no effects (this case should be optimized). }
    property Volume: Single read FVolume write SetVolume
      default DefaultVolume;

    { Sound output device, used when initializing OpenAL context.

      You can change it even when OpenAL is already initialized.
      Then we'll close the old device (ALContextClose),
      change @link(Device) value, and initialize context again (ALContextOpen).
      Note that you will need to reload your buffers and sources again. }
    property Device: string read FDevice write SetDevice;

    { Enable sound.

      If @false, then ALContextOpen will not initialize any OpenAL device.
      This is useful if you simply want to disable any sound output
      (or OpenAL usage), even when OpenAL library is available.

      If the OpenAL context is already initialized when setting this,
      we will eventually close it. (More precisely, we will
      do ALContextClose and then ALContextOpen again. This behaves correctly.) }
    property Enabled: boolean read FEnabled write SetEnabled default DefaultEnabled;

    property Enable : boolean read FEnabled write SetEnabled default DefaultEnabled; deprecated 'Use Enabled';

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
  TSoundInfo = class;

  { Unique sound type identifier for sounds used within TRepoSoundEngine. }
  TSoundType = record
  private
    { Just an index to TRepoSoundEngine.SoundNames array. }
    Index: Cardinal;
  public
    function InternalInfo: TSoundInfo;
    class operator {$ifdef FPC}={$else}Equals{$endif} (const SoundType1, SoundType2: TSoundType): boolean;
  end;

  { List of TSoundInfo.

    @exclude
    @bold(This is an internal class, and in the future will not be publicly available). }
  TSoundInfoList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TSoundInfo>)
    { Index of sound with given TSoundInfo.Name, or -1 if not found. }
    function IndexOfName(const SoundName: String): Integer;
  end;

  { Sound information.
    Most fields of this classs correspond to appropriate attributes in
    the XML file loaded by setting @link(TRepoSoundEngine.RepositoryURL).

    @exclude
    @bold(This is an internal class, and in the future will not be publicly available). }
  TSoundInfo = class
  private
    FBuffer: TSoundBuffer;

    { OpenAL buffer of this sound. @nil if buffer is not yet loaded,
      which may happen only if TRepoSoundEngine.ALContextOpen was not yet
      called or when sound has URL = ''. }
    property Buffer: TSoundBuffer read FBuffer;
  public
    { Unique sound name (including parent group names). Empty for the special sound stNone. }
    Name: string;

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
      Although OpenAL may clip the resulting sound (after all
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

    { A group (one among FSoundGroups, or @nil if not in any group). }
    ParentGroup: TSoundInfoList;
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
    type
      TSoundGroup = class(TSoundInfoList)
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
        { Index of group with given TSoundGroup.Name, or -1 if not found. }
        function IndexOfName(const GroupName: String): Integer;
      end;

      TLoopingChannelList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TLoopingChannel>;

    var
      FSoundImportanceNames: TStringList;
      FSounds: TSoundInfoList;
      FSoundGroups: TSoundGroupList;
      FRepositoryURL: string;
      FLoopingChannels: TLoopingChannelList;

    procedure SetRepositoryURL(const Value: string);

    { Reinitialize looping channels sounds.
      Should be called as soon as Sounds changes and we may have OpenAL context. }
    procedure RestartLoopingChannels;

    procedure ALContextOpenCore; override;

    function GetMusicPlayer: TLoopingChannel;
    function GetLoopingChannel(const Index: Cardinal): TLoopingChannel;
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
      given XML file. You usually set RepositoryURL at the very beginning,
      before OpenAL context is initialized (although it's also Ok to do this after).
      Right after setting RepositoryURL you usually call SoundFromName
      a couple of times to convert some names into TSoundType values,
      to later use these TSoundType values with @link(Sound) and @link(Sound3D)
      methods.

      When OpenAL is initialized, sound buffers will actually be loaded.

      If this is empty (the default), then no sounds are loaded,
      and TRepoSoundEngine doesn't really give you much above standard
      TSoundEngine.

      If you want to actually use TRepoSoundEngine features
      (like the @link(Sound) and @link(Sound3D) methods) you have to set this
      property. For example like this:

      @longCode(#
        SoundEngine.RepositoryURL := ApplicationData('sounds.xml');
        stMySound1 := SoundEngine.SoundFromName('my_sound_1');
        stMySound2 := SoundEngine.SoundFromName('my_sound_2');
        // ... and later in your game you can do stuff like this:
        SoundEngine.Sound(stMySound1);
        SoundEngine.Sound3D(stMySound1, Vector3(0, 0, 10));
      #)

      See CastleFilesUtils unit for docs of ApplicationData function.
    }
    property RepositoryURL: string read FRepositoryURL write SetRepositoryURL;

    { Deprecated name for RepositoryURL. @deprecated }
    property SoundsFileName: string read FRepositoryURL write SetRepositoryURL; deprecated;

    { Reload the RepositoryURL and all referenced buffers.
      Useful as a tool for game designers, to reload the sounds XML file
      without restarting the game and sound engine. }
    procedure ReloadSounds;

    { A list of sounds used by your program.
      Each sound has a unique name, used to identify sound in
      the XML file and for SoundFromName function.

      At the beginning, this list always contains exactly one sound: empty stNone.
      This is a special "sound type" that has index 0 (should be always
      expressed as TSoundType value stNone) and name ''.
      stNone is a special sound as it actually means "no sound" in many cases. }
    property Sounds: TSoundInfoList read FSounds; deprecated 'do not use this, it is internal information';

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
    function Sound(SoundType: TSoundType;
      const Looping: boolean = false): TSound;

    { Play given sound at appropriate position in 3D space.

      Returns used TSound (or nil if none was available).
      You don't have to do anything with this returned TSound.

      @noAutoLinkHere }
    function Sound3D(SoundType: TSoundType;
      const Position: TVector3;
      const Looping: boolean = false): TSound; overload;

    { Sound importance names and values.
      Each item is a name (as a string) and a value (that is stored in Objects
      property of the item as a pointer; add new importances by
      AddSoundImportanceName for comfort).

      These can be used within sounds.xml file.
      Before using ALContextOpen, you can fill this list with values.

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

    { Opens sound context (OpenAL) and loads sound files,
      but only if RepositoryURL was set and contains some sounds.

      The idea is that you can call this during "loading" stage for any game that
      *possibly but not necessarily* uses sound. If a game doesn't use sound,
      this does nothing (doesn't waste time to even initialize OpenAL,
      which on some systems may cause some warnings).
      If a game uses sound (through RepositoryURL), this will initialize
      OpenAL and load these sound files, to play them without any delay
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

    { This is nil if we don't play sound right now
      (because OpenAL is not initialized, or Sound = stNone,
      or PlayerSound.URL = '' (sound not existing)). }
    FAllocatedSource: TSound;

    FVolume: Single;

    FSound: TSoundType;
    procedure SetSound(const Value: TSoundType);
    procedure AllocatedSourceRelease(Sender: TSound);

    { Called by ALContextOpen. You should check here if
      Sound <> stNone and eventually initialize FAllocatedSource. }
    procedure AllocateSource;
    function GetVolume: Single;
    procedure SetVolume(const Value: Single);
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

{ The sound engine. Singleton instance of TRepoSoundEngine, the most capable
  engine class. Created on first call to this function. }
function SoundEngine: TRepoSoundEngine;

implementation

{ use a deprecated unit below, only to have it compiled together with Lazarus
  castle_base.lpk package }
{$warnings off}
uses DOM, XMLRead, StrUtils, Generics.Defaults,
  CastleUtils, CastleInternalALUtils, CastleLog, CastleProgress,
  CastleInternalVorbisFile, CastleInternalEFX,
  CastleParameters, CastleXMLUtils, CastleFilesUtils, CastleConfig,
  CastleURIUtils, CastleDownload, CastleMessaging, CastleApplicationProperties,
  // this is deprecated
  CastleSoundAllocator;
{$warnings on}

{ TSoundBuffer --------------------------------------------------------------- }

procedure TSoundBuffer.ALContextOpen(const ExceptionOnError: boolean);

  procedure OpenCore;
  begin
    alCreateBuffers(1, @ALBuffer);
    try
      alBufferDataFromFile(ALBuffer, URL, FDuration);
    except alDeleteBuffers(1, @ALBuffer); raise end;
  end;

begin
  if ExceptionOnError then
  begin
    OpenCore;
  end else
  try
    OpenCore;
  except
    on E: Exception do
    begin
      ALBuffer := 0;
      WritelnWarning('Sound', Format('Sound file "%s" cannot be loaded: %s',
        [URIDisplay(URL), E.Message]));
    end;
  end;
end;

procedure TSoundBuffer.ALContextClose;
begin
  alFreeBuffer(ALBuffer);
end;

var
  ValidSoundBufferFree: Cardinal;

destructor TSoundBuffer.Destroy;
begin
  if ValidSoundBufferFree = 0 then
    raise EInvalidSoundBufferFree.Create('Do not free TSoundBuffer instance directly, use SoundEngine.FreeBuffer');
  ALContextClose;
  inherited;
end;

{ TSound ---------------------------------------------------------- }

constructor TSound.Create(const AnAllocator: TSoundAllocator);
var
  ErrorCode: TALenum;
begin
  inherited Create;

  FAllocator := AnAllocator;

  { We have to check alGetError now, because I may need to catch
    (and convert to ENoMoreOpenALSources exception) alGetError after
    alCreateSources. So I want to have "clean error state" first. }
  CheckAL('Checking before TSound.Create work');

  alCreateSources(1, @FALSource);

  ErrorCode := alGetError();
  if ErrorCode = AL_INVALID_VALUE then
    raise ENoMoreOpenALSources.Create('No more sound sources available') else
  if ErrorCode <> AL_NO_ERROR then
    raise EALError.Create(ErrorCode,
      'OpenAL error AL_xxx at creation of sound : ' + alGetString(ErrorCode));

  { This signals to TSound.Destroy that FALSource contains
    valid source name, that should be deleted by alDeleteSources. }
  FALSourceAllocated := true;
end;

destructor TSound.Destroy;
begin
  if FALSourceAllocated then
    alDeleteSources(1, @FALSource);
  inherited;
end;

procedure TSound.Release;
begin
  FUsed := false;

  { Note that alSourceStop is a valid NOP for source states like
    AL_STOPPED or AL_INITIAL. So I don't check here current state
    (like CurrentState := alGetSource1i(ALSource, AL_SOURCE_STATE))
    and simply always call alSourceStop. }
  alSourceStop(ALSource);

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
  alSourceVector3f(ALSource, AL_POSITION, Value);
end;

procedure TSound.SetVelocity(const Value: TVector3);
begin
  FVelocity := Value;
  alSourceVector3f(ALSource, AL_VELOCITY, Value);
end;

procedure TSound.SetLooping(const Value: boolean);
begin
  FLooping := Value;
  alSourcei(ALSource, AL_LOOPING, BoolToAL[Value]);
end;

procedure TSound.SetRelative(const Value: boolean);
begin
  FRelative := Value;
  alSourcei(ALSource, AL_SOURCE_RELATIVE, BoolToAL[Value]);
end;

procedure TSound.SetGain(const Value: Single);
begin
  FGain := Value;
  alSourcef(ALSource, AL_GAIN, Value);
end;

procedure TSound.SetMinGain(const Value: Single);
begin
  FMinGain := Value;
  alSourcef(ALSource, AL_MIN_GAIN, Value);
end;

procedure TSound.SetMaxGain(const Value: Single);
begin
  FMaxGain := Value;
  alSourcef(ALSource, AL_MAX_GAIN, Value);
end;

procedure TSound.SetBuffer(const Value: TSoundBuffer);
begin
  FBuffer := Value;
  if Value <> nil then
  begin
    { TSoundBuffer is unsigned, while alSourcei is declared as taking signed integer.
      But we know we can pass TSoundBuffer to alSourcei, just typecasting it to
      whatever alSourcei requires. }
    {$I norqcheckbegin.inc}
    alSourcei(ALSource, AL_BUFFER, Value.ALBuffer);
    {$I norqcheckend.inc}
  end else
    alSourcei(ALSource, AL_BUFFER, 0);
end;

procedure TSound.SetPitch(const Value: Single);
begin
  FPitch := Value;
  alSourcef(ALSource, AL_PITCH, Value);
end;

procedure TSound.SetRolloffFactor(const Value: Single);
begin
  FRolloffFactor := Value;
  alSourcef(ALSource, AL_ROLLOFF_FACTOR, Value);
end;

procedure TSound.SetReferenceDistance(const Value: Single);
begin
  FReferenceDistance := Value;
  alSourcef(ALSource, AL_REFERENCE_DISTANCE, Value);
end;

procedure TSound.SetMaxDistance(const Value: Single);
begin
  FMaxDistance := Value;
  alSourcef(ALSource, AL_MAX_DISTANCE, Value);
end;

function TSound.GetOffset: Single;
begin
  if FAllocator.ALVersionAtLeast(1, 1) then
    Result := alGetSource1f(ALSource, AL_SEC_OFFSET)
  else
    Result := 0;
end;

procedure TSound.SetOffset(const Value: Single);
var
  ErrorCode: TALenum;
begin
  if FAllocator.ALVersionAtLeast(1, 1) then
  begin
    { We have to check alGetError now, because we need to catch
      AL_INVALID_VALUE later. }
    CheckAL('Checking before TSound.SetOffset work');

    alSourcef(ALSource, AL_SEC_OFFSET, Value);

    { capture AL_INVALID_VALUE, otherwise it would be too easy to make mistake
      at setting offset to something like "duration-epsilon". }

    ErrorCode := alGetError();
    if ErrorCode = AL_INVALID_VALUE then
      WritelnWarning('Ignoring TSound.SetOffset with offset %f', [Value])
    else
    if ErrorCode <> AL_NO_ERROR then
      raise EALError.Create(ErrorCode,
        'OpenAL error AL_xxx at setting sound offset : ' + alGetString(ErrorCode));
  end;
end;

function TSound.PlayingOrPaused: boolean;
var
  SourceState: TALuint;
begin
  SourceState := alGetSource1i(ALSource, AL_SOURCE_STATE);
  Result := (SourceState = AL_PLAYING) or (SourceState = AL_PAUSED);
end;

procedure TSound.KeepPlaying;
begin
  if not PlayingOrPaused then
    alSourcePlay(ALSource);
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
  inherited;
end;

procedure TSoundAllocator.ALContextOpenCore;
var
  I: Integer;
begin
  FAllocatedSources := TSoundList.Create(true);
  FAllocatedSources.Count := MinAllocatedSources;
  for I := 0 to FAllocatedSources.Count - 1 do
    FAllocatedSources[I] := TSound.Create(Self);

  ApplicationProperties.OnUpdate.Add({$ifdef CASTLE_OBJFPC}@{$endif} Update);
end;

procedure TSoundAllocator.ALContextCloseCore;
var
  I: Integer;
begin
  if FAllocatedSources <> nil then
  begin
    { Stop using and free allocated sounds. }
    for I := 0 to FAllocatedSources.Count - 1 do
      { Although usually we are sure that every FAllocatedSources[I] <> nil,
        in this case we must take into account that maybe our constructor
        raise ENonMoreOpenALSources and so some FAllocatedSources[I] were
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

  { OpenAL context not initialized yet }
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
      Result := TSound.Create(Self);
      FAllocatedSources.Add(Result);
    except
      { If TSound.Create raises ENoMoreOpenALSources ---
        then silence the exception and leave Result = nil. }
      on ENoMoreOpenALSources do ;
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

  CheckAL('allocating sound source (TSoundAllocator.AllocateSound)');
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
        FAllocatedSources[I] := TSound.Create(Self);
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
      { DetectUnusedSounds is needed here to release the *currently* unused sources. }
      DetectUnusedSounds;
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
  DetectUnusedSounds;
end;

procedure TSoundAllocator.DetectUnusedSounds;
var
  I: Integer;
begin
  CheckAL('before DetectUnusedSounds');

  if FAllocatedSources <> nil then
    for I := 0 to FAllocatedSources.Count - 1 do
      if FAllocatedSources[I].Used and
         (not FAllocatedSources[I].PlayingOrPaused) then
      begin
        FAllocatedSources[I].Release;
        // WritelnLog('Sound stopped playing');
      end;
end;

procedure TSoundAllocator.Update(Sender: TObject);
const
  { Delay between calling DetectUnusedSounds, in seconds. }
  SoundRefreshDelay = 0.1;
var
  TimeNow: TTimerResult;
begin
  { Calling DetectUnusedSounds relatively often is important,
    to call OnRelease for sound sources that finished playing. }
  if FAllocatedSources <> nil then
  begin
    TimeNow := Timer;
    if TimerSeconds(TimeNow, LastSoundRefresh) > SoundRefreshDelay then
    begin
      LastSoundRefresh := TimeNow;
      DetectUnusedSounds;
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

  { Default OpenAL listener attributes }
  ListenerPosition := TVector3.Zero;
  ListenerOrientation[0] := Vector3(0, 0, -1);
  ListenerOrientation[1] := Vector3(0, 1, 0);

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

  ALContextClose;

  Inc(ValidSoundBufferFree);
  try
    FreeAndNil(LoadedBuffers);
  finally Dec(ValidSoundBufferFree) end;

  FreeAndNil(FDevices);
  FreeAndNil(FOnOpenClose);
  inherited;
end;

function TSoundEngine.Devices: TSoundDeviceList;

  { Find available OpenAL devices, add them to FDevices.

    It tries to use ALC_ENUMERATION_EXT extension, available on all modern
    OpenAL implementations. If it fails, and we're dealing with
    OpenAL "sample implementation" (older OpenAL Unix implementation)
    then we return a hardcoded list of devices known to be supported
    by this implementation.
    This makes it working sensibly under all OpenAL implementations in use
    today.

    Also for every OpenAL implementation, we add an implicit
    OpenAL default device named '' (empty string). }
  procedure UpdateDevices;

    procedure Add(const AName, ACaption: string);
    var
      D: TSoundDevice;
    begin
      D := TSoundDevice.Create;
      D.FName := AName;
      D.FCaption := ACaption;
      FDevices.Add(D);
    end;

    function SampleImpALCDeviceName(const ShortDeviceName: string): string;
    begin
      Result := '''(( devices ''(' + ShortDeviceName + ') ))';
    end;

  var
    pDeviceList: PChar;
  begin
    Add('', 'Default OpenAL device');

    if ALLibraryAvailable and EnumerationExtPresent(pDeviceList) then
    begin
      { parse pDeviceList }
      while pDeviceList^ <> #0 do
      begin
        { automatic conversion PChar -> AnsiString below }
        Add(pDeviceList, pDeviceList);

        { advance position of pDeviceList }
        pDeviceList := StrEnd(pDeviceList);
        Inc(pDeviceList);
      end;
    end else
    if ALLibraryAvailable and OpenALSampleImplementation then
    begin
      Add(SampleImpALCDeviceName('native'), 'Operating system native');
      Add(SampleImpALCDeviceName('sdl'), 'SDL (Simple DirectMedia Layer)');

      { aRts device is too unstable on my Linux:

        When trying to initialize <tt>arts</tt> backend
        I can bring the OpenAL library (and, consequently, whole program
        using it) to crash with message <i>can't create mcop
        directory</i>. Right after running konqueror, I get also
        crash with message <i>*** glibc detected *** double free or corruption (out):
        0x08538d88 ***</i>.

        This is so unstable, that I think that I do a service
        for users by *not* listing aRts in available OpenAL
        devices. It's listed on [https://castle-engine.io/openal_notes.php]
        and that's enough.

      Add(SampleImpALCDeviceName('arts'), 'aRts (analog Real time synthesizer)');
      }

      Add(SampleImpALCDeviceName('esd'), 'Esound (Enlightened Sound Daemon)');
      Add(SampleImpALCDeviceName('alsa'), 'ALSA (Advanced Linux Sound Architecture)');
      Add(SampleImpALCDeviceName('waveout'), 'WAVE file output');
      Add(SampleImpALCDeviceName('null'), 'Null device (no output)');
    end;
  end;

begin
  { Create devices on demand (not immediately in TSoundEngine.Create),
    because merely using alcGetString(nil, ALC_DEVICE_SPECIFIER)
    may perform some OpenAL initialization (discovery of available devices).
    E.g. with OpenAL Soft 1.13 in Debian. This is not very harmful,
    but it causes like output (on stdout or stderr) like

      AL lib: pulseaudio.c:612: Context did not connect: Connection refused
      ALSA lib pcm.c:2190:(snd_pcm_open_noupdate) Unknown PCM cards.pcm.rear
      ALSA lib pcm.c:2190:(snd_pcm_open_noupdate) Unknown PCM cards.pcm.center_lfe
      ALSA lib pcm.c:2190:(snd_pcm_open_noupdate) Unknown PCM cards.pcm.side
      ALSA lib pcm_dmix.c:957:(snd_pcm_dmix_open) The dmix plugin supports only playback stream

    and it causes a temporary slowdown. So we want to defer this (until really
    needed, or until explicit ALContextOpen call). }

  if FDevices = nil then
  begin
    FDevices := TSoundDeviceList.Create;
    UpdateDevices;
  end;
  Result := FDevices;
end;

procedure TSoundEngine.CheckALC(const Situation: string);
var
  ErrCode: TALenum;
  ErrDescription: PChar;
  ErrDescriptionStr: string;
begin
  ErrCode := alcGetError(ALDevice);
  if ErrCode <> ALC_NO_ERROR then
  begin
    { Secure, in case alcGetError returns nil (so the error code is incorrect),
      which happened long time ago on Creative Windows OpenAL implementation. }
    ErrDescription := alcGetString(ALDevice, ErrCode);
    if ErrDescription = nil then
      ErrDescriptionStr := Format('Unknown OpenAL (alc) error number: %d', [ErrCode])
    else
      ErrDescriptionStr := ErrDescription;

    raise EALCError.Create(ErrCode,
      'OpenAL error ALC_xxx at ' + Situation + ' : ' + ErrDescriptionStr);
  end;
end;

function TSoundEngine.GetContextString(Enum: TALCenum): string;
begin
  Result := alcGetString(ALDevice, Enum);
  try
    CheckALC('alcGetString');
    { Check also normal al error (alGetError instead
      of alcGetError). Seems that when Darwin (macOS) Apple's OpenAL
      implementation fails to return some alcGetString
      it reports this by setting AL error (instead of ALC one)
      to "invalid value". Although (after fixes to detect OpenALSampleImplementation
      at runtime and change constants values) this shouldn't happen anymore
      if you pass correct consts to this function. }
    CheckAL('alcGetString');
  except
    on E: EALCError do result := '('+E.Message+')';
    on E: EALError do result := '('+E.Message+')';
  end;
end;

procedure TSoundEngine.ALContextOpenCore;

  procedure ParseVersion(const Version: string; out Major, Minor: Integer);
  var
    DotP, SpaceP: Integer;
  begin
    { version unknown }
    Major := 0;
    Minor := 0;

    DotP := Pos('.', Version);
    if DotP <> 0 then
    try
      Major := StrToInt(Trim(Copy(Version, 1, DotP - 1)));
      SpaceP := PosEx(' ', Version, DotP + 1);
      if SpaceP <> 0 then
        Minor := StrToInt(Trim(Copy(Version, DotP + 1, SpaceP - DotP))) else
        Minor := StrToInt(Trim(SEnding(Version, DotP + 1)));
    except
      on EConvertError do
      begin
        Major := 0;
        Minor := 0;
      end;
    end;
  end;

  { Try to initialize OpenAL.
    Sets ALActive, EFXSupported.
    If not ALActive, then ALActivationErrorMessage contains error description. }
  procedure BeginAL(out ALActivationErrorMessage: string);
  begin
    { We don't do alcProcessContext/alcSuspendContext, no need
      (spec says that context is initially in processing state). }

    try
      //raise EOpenALError.Create('Test pretend OpenAL fails');

      FALActive := false;
      FEFXSupported := false;
      ALActivationErrorMessage := '';
      FALMajorVersion := 0;
      FALMinorVersion := 0;

      if not ALLibraryAvailable then
        raise EOpenALInitError.Create('OpenAL library is not available');

      Assert(Assigned(alcOpenDevice), 'Assigned(alcOpenDevice)');

      ALDevice := alcOpenDevice(PCharOrNil(Device));
      if (ALDevice = nil) then
        raise EOpenALError.CreateFmt(
          'OpenAL''s audio device "%s" is not available', [Device]);

      ALContext := alcCreateContext(ALDevice, nil);
      CheckALC('initializing OpenAL (alcCreateContext)');

      alcMakeContextCurrent(ALContext);
      CheckALC('initializing OpenAL (alcMakeContextCurrent)');

      FALActive := true;
      FEFXSupported := Load_EFX(ALDevice);
      ParseVersion(alGetString(AL_VERSION), FALMajorVersion, FALMinorVersion);
    except
      on E: EOpenALError do
        ALActivationErrorMessage := E.Message;
    end;
  end;

  function ALInformation: string;
  begin
    Assert(ALActive);

    Result := Format(
      NL+
      'Version : %s' +NL+
      'Version Parsed : major: %d, minor: %d' +NL+
      'Renderer : %s' +NL+
      'Vendor : %s' +NL+
      'Extensions : %s' +NL+
      NL+
      'Allocated OpenAL sources: min %d, max %d' +NL+
      NL+
      'Library to decode OggVorbis available: %s',
      [ alGetString(AL_VERSION),
        FALMajorVersion, FALMinorVersion,
        alGetString(AL_RENDERER),
        alGetString(AL_VENDOR),
        alGetString(AL_EXTENSIONS),
        MinAllocatedSources, MaxAllocatedSources,
        BoolToStr(VorbisFileInitialized, true)
      ]);
  end;

  { initialize OpenAL resources inside LoadedBuffers }
  procedure LoadedBuffersOpen;
  var
    Buffer: TSoundBuffer;
  begin
    // check LoadedBuffers.Count, because we don't want to do progress with no LoadedBuffers
    if LoadedBuffers.Count <> 0 then
    begin
      if Progress.Active then
      begin
        { call ALContextOpen on all buffers }
        for Buffer in LoadedBuffers do
          Buffer.ALContextOpen(false);
      end else
      begin
        { same as above, but with added Progress.Init / Step / Fini }
        Progress.Init(LoadedBuffers.Count, 'Loading sounds');
        try
          for Buffer in LoadedBuffers do
          begin
            Buffer.ALContextOpen(false);
            Progress.Step;
          end;
        finally Progress.Fini end;
      end;
    end;
  end;

var
  ALActivationErrorMessage: string;
begin
  Assert(not ALActive, 'OpenAL context is already active');

  if not Enabled then
    FInformation :=
      'OpenAL initialization aborted: sound is disabled (by --no-sound command-line option, or menu item or such)' else
  begin
    BeginAL(ALActivationErrorMessage);
    if not ALActive then
      FInformation :=
        'OpenAL initialization failed:' +NL+ ALActivationErrorMessage else
    begin
      FInformation :=
        'OpenAL initialized successfully' +NL+ ALInformation;

      try
        alListenerf(AL_GAIN, Volume);
        UpdateDistanceModel;
        inherited; { initialize sound allocator }
        CheckAL('initializing sounds (ALContextOpen)');
        LoadedBuffersOpen;
      except
        ALContextClose;
        raise;
      end;
    end;
  end;

  WritelnLogMultiline('Sound', Information);

  OnOpenClose.ExecuteAll(Self);
end;

procedure TSoundEngine.ALContextCloseCore;

  procedure EndAL;
  begin
    FALActive := false;
    FEFXSupported := false;

    { CheckALC first, in case some error is "hanging" not caught yet. }
    CheckALC('right before closing OpenAL context');

    if ALContext <> nil then
    begin
      (* The OpenAL specification says

         "The correct way to destroy a context is to first release
         it using alcMakeCurrent with a NULL context. Applications
         should not attempt to destroy a current context – doing so
         will not work and will result in an ALC_INVALID_OPERATION error."

         (See [http://openal.org/openal_webstf/specs/oal11spec_html/oal11spec6.html])

         However, sample implementation (used on most Unixes,
         before OpenAL soft came) can hang
         on alcMakeContextCurrent(nil) call. Actually, it doesn't hang,
         but it stops for a *very* long time (even a couple of minutes).
         This is a known problem, see
         [http://opensource.creative.com/pipermail/openal-devel/2005-March/002823.html]
         and
         [http://lists.berlios.de/pipermail/warzone-dev/2005-August/000441.html].

         Tremulous code workarounds it like

           if( Q_stricmp((const char* )qalGetString( AL_VENDOR ), "J. Valenzuela" ) ) {
                   qalcMakeContextCurrent( NULL );
           }

         ... and this seems a good idea, we do it also here.
         Initially I wanted to do $ifdef UNIX, but checking for Sample implementation
         with alGetString(AL_VENDOR) is more elegant (i.e. affecting more precisely
         the problematic OpenAL implementations, e.g. allowing us to work
         correctly with OpenAL soft too). *)

      if not OpenALSampleImplementation then
        alcMakeContextCurrent(nil);

      alcDestroyContext(ALContext);
      ALContext := nil;
      CheckALC('closing OpenAL context');
    end;

    if ALDevice <> nil then
    begin
      alcCloseDevice(ALDevice);
      { w/g specyfikacji OpenAL generuje teraz error ALC_INVALID_DEVICE jesli
        device bylo nieprawidlowe; ale niby jak mam sprawdzic ten blad ?
        Przeciez zeby sprawdzic alcGetError potrzebuje miec valid device w reku,
        a po wywolaniu alcCloseDevice(device) device jest invalid (bez wzgledu
        na czy przed wywolaniem alcCloseDevice bylo valid) }
      ALDevice := nil;
    end;
  end;

var
  Buffer: TSoundBuffer;
begin
  if ALActive then
  begin
    { release sound allocator first. This also stops all the sources,
      which is required before we try to release their buffers. }
    inherited;
    { free OpenAL resources allocated inside LoadedBuffers }
    for Buffer in LoadedBuffers do
      Buffer.ALContextClose;
    EndAL;
  end;

  WritelnLog('Sound', 'OpenAL closed');

  OnOpenClose.ExecuteAll(Self);
end;


procedure TSoundEngine.ALContextOpen;
begin
  if Paused then
    Exit; // do not even set ALInitialized to true

  if not ALInitialized then
  begin
    FALInitialized := true; // set it early, so that OnOpenClose knows it's true
    ALContextOpenCore;
  end;
end;

procedure TSoundEngine.ALContextClose;
begin
  if ALInitialized then
  begin
    FALInitialized := false; // set it early, so that OnOpenClose knows it's false
    ALContextCloseCore;
  end;
end;

function TSoundEngine.PlaySound(const Parameters: TSoundParameters): TSound;
const
  { For now, just always use CheckBufferLoaded. It doesn't seem to cause
    any slowdown for normal sound playing. }
  CheckBufferLoaded = true;
begin
  Result := nil;

  if ALActive and
     (Parameters.Buffer <> nil) and
     { ALBuffer may be = 0 if file failed to load, e.g. file not found }
     (Parameters.Buffer.ALBuffer <> 0) then
  begin
    Result := AllocateSound(Parameters.Importance);
    if Result <> nil then
    begin
      Result.Buffer  := Parameters.Buffer;
      Result.Looping := Parameters.Looping;
      Result.Gain    := Parameters.Gain;
      Result.MinGain := Parameters.MinGain;
      Result.MaxGain := Parameters.MaxGain;
      Result.Pitch   := Parameters.Pitch;
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
            Writeln(SoundInfos.List^[Sound].Buffer, ' ',
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

        { We have to do CheckAL first, to catch eventual errors.
          Otherwise the loop could hang. }
        CheckAL('PlaySound');
        while Parameters.Buffer.ALBuffer <> alGetSource1ui(Result.ALSource, AL_BUFFER) do
          Sleep(10);
      end;

      alSourcePlay(Result.ALSource);
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
  ALContextOpen;

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

  Result := TSoundBuffer.Create;
  Result.URL := FullURL;
  Result.References := 1;
  LoadedBuffers.Add(Result);

  if ALActive then
    { let LoadBuffer raise exception on missing sound file }
    Result.ALContextOpen(ExceptionOnError);
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
      // this will free Buffer, also calling Buffer.ALContextClose;
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
    if ALActive then
      alListenerf(AL_GAIN, Volume);
  end;
end;

function TSoundEngine.ALVersionAtLeast(const AMajor, AMinor: Integer): boolean;
begin
  Result :=
      (AMajor < FALMajorVersion) or
    ( (AMajor = FALMajorVersion) and (AMinor <= FALMinorVersion) );
end;

procedure TSoundEngine.UpdateDistanceModel;
const
  ALDistanceModelConsts: array [TSoundDistanceModel] of TALenum =
  ( AL_NONE,
    AL_INVERSE_DISTANCE, AL_INVERSE_DISTANCE_CLAMPED,
    AL_LINEAR_DISTANCE, AL_LINEAR_DISTANCE_CLAMPED,
    AL_EXPONENT_DISTANCE, AL_EXPONENT_DISTANCE_CLAMPED );
var
  Is11: boolean;
begin
  Is11 := ALVersionAtLeast(1, 1);
  if (not Is11) and (DistanceModel in [dmLinearDistance, dmExponentDistance]) then
    alDistanceModel(AL_INVERSE_DISTANCE) else
  if (not Is11) and (DistanceModel in [dmLinearDistanceClamped, dmExponentDistanceClamped]) then
    alDistanceModel(AL_INVERSE_DISTANCE_CLAMPED) else
    alDistanceModel(ALDistanceModelConsts[DistanceModel]);
end;

procedure TSoundEngine.SetDistanceModel(const Value: TSoundDistanceModel);
begin
  if Value <> FDistanceModel then
  begin
    FDistanceModel := Value;
    if ALActive then UpdateDistanceModel;
  end;
end;

procedure TSoundEngine.SetDevice(const Value: string);
begin
  if Value <> FDevice then
  begin
    if ALInitialized then
    begin
      ALContextClose;
      OpenALRestart;
      FDevice := Value;
      ALContextOpen;
    end else
      FDevice := Value;
    DeviceSaveToConfig := true; // caller will eventually change it to false
  end;
end;

procedure TSoundEngine.SetEnabled(const Value: boolean);
begin
  if Value <> FEnabled then
  begin
    if ALInitialized then
    begin
      ALContextClose;
      FEnabled := Value;
      ALContextOpen;
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
    else raise EInternalError.Create('OpenALOptionProc');
  end;
end;

procedure TSoundEngine.ParseParameters;
const
  OpenALOptions: array [0..1] of TOption =
  ( (Short: #0; Long: 'audio-device'; Argument: oaRequired),
    (Short: #0; Long: 'no-sound'; Argument: oaNone)
  );
begin
  Parameters.Parse(OpenALOptions, @OptionProc, Self, true);
end;

function TSoundEngine.ParseParametersHelp: string;

  function DevicesHelp: string;
  var
    DefaultDeviceName: string;
    I: Integer;
  begin
    if not ALLibraryAvailable then
      Result := '                        Warning: OpenAL is not available, cannot print' +NL+
                '                        available audio devices.' +NL else
    if not EnumerationExtPresent then
      Result := '                        Warning: OpenAL does not support getting the list'+NL+
                '                        of available audio devices' +NL+
                '                        (missing ALC_ENUMERATION_EXT), probably old OpenAL.' else
    begin
      DefaultDeviceName := alcGetString(nil, ALC_DEFAULT_DEVICE_SPECIFIER);

      Result := Format('                        Available devices (%d):', [Devices.Count]) + nl;
      for i := 0 to Devices.Count - 1 do
      begin
        Result := Result + '                          ' + Devices[i].Caption;
        if Devices[i].Name <> Devices[i].Caption then
          Result := Result + ' (Real OpenAL name: "' + Devices[i].Name + '")';
        if Devices[i].Name = DefaultDeviceName then
          Result := Result + ' (Equivalent to default device)';
        Result := Result + nl;
      end;
    end;
  end;

begin
  Result :=
    '  --audio-device DEVICE-NAME' +nl+
    '                        Choose specific OpenAL audio device.' +nl+
    DevicesHelp +
    '  --no-sound            Turn off sound.';
end;

procedure TSoundEngine.UpdateListener(const Position, Direction, Up: TVector3);
begin
  ListenerPosition := Position;
  ListenerOrientation[0] := Direction;
  ListenerOrientation[1] := Up;
  if ALActive then
  begin
    alListenerVector3f(AL_POSITION, Position);
    alListenerOrientation(Direction, Up);
  end;
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

  Result := 'Some OpenAL device'; // some default
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
      FResumeToInitialized := ALInitialized;
      ALContextClose;
    end else
    begin
      if FResumeToInitialized then
        ALContextOpen;
    end;
  end;
end;

class function TSoundEngine.GetLogSoundLoading: Boolean;
begin
  Result := CastleInternalALUtils.LogSoundLoading;
end;

class procedure TSoundEngine.SetLogSoundLoading(const Value: Boolean);
begin
  CastleInternalALUtils.LogSoundLoading := Value;
end;

{ TSoundType ----------------------------------------------------------------- }

function TSoundType.InternalInfo: TSoundInfo;
begin
  Result := SoundEngine.FSounds[Index];
end;

class operator TSoundType.{$ifdef FPC}={$else}Equals{$endif}
  (const SoundType1, SoundType2: TSoundType): boolean;
begin
  Result := SoundType1.Index = SoundType2.Index;
end;

{ TSoundInfoList ------------------------------------------------------------- }

function TSoundInfoList.IndexOfName(const SoundName: String): Integer;
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

{ TRepoSoundEngine ----------------------------------------------------------- }

constructor TRepoSoundEngine.Create;
begin
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
  FSounds.Add(TSoundInfo.Create);

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

procedure TRepoSoundEngine.ALContextOpenCore;
begin
  inherited;
  RestartLoopingChannels;
end;

procedure TRepoSoundEngine.RestartLoopingChannels;
var
  L: TLoopingChannel;
begin
  { allocate sound for all TLoopingChannel }
  if ALActive then
    for L in FLoopingChannels do
      if L <> nil then
        L.AllocateSource;
end;

function TRepoSoundEngine.Sound(SoundType: TSoundType;
  const Looping: boolean): TSound;
begin
  { If there is no actual sound, exit early without initializing OpenAL.
    - SoundType is stNone if not defined in sounds.xml.
    - SoundType is <> stNone but URL = '' if sound name is defined in
      sounds.xml with explicit url="", like this:
      <sound name="player_sudden_pain" url="" />
  }
  if (SoundType.Index = 0) or (FSounds[SoundType.Index].URL = '') then Exit(nil);

  ALContextOpen;

  if FSounds[SoundType.Index].Buffer = nil then Exit(nil);

  Result := PlaySound(
    FSounds[SoundType.Index].Buffer, false, Looping,
    FSounds[SoundType.Index].DefaultImportance,
    FSounds[SoundType.Index].Gain,
    FSounds[SoundType.Index].MinGain,
    FSounds[SoundType.Index].MaxGain,
    TVector3.Zero);
end;

function TRepoSoundEngine.Sound3D(SoundType: TSoundType;
  const Position: TVector3;
  const Looping: boolean): TSound;
begin
  { If there is no actual sound, exit early without initializing OpenAL.
    See Sound for duplicate of this "if" and more comments. }
  if (SoundType.Index = 0) or (FSounds[SoundType.Index].URL = '') then Exit(nil);

  ALContextOpen;

  if FSounds[SoundType.Index].Buffer = nil then Exit(nil);

  Result := PlaySound(
    FSounds[SoundType.Index].Buffer, true, Looping,
    FSounds[SoundType.Index].DefaultImportance,
    FSounds[SoundType.Index].Gain,
    FSounds[SoundType.Index].MinGain,
    FSounds[SoundType.Index].MaxGain,
    Position);
end;

procedure TRepoSoundEngine.SetRepositoryURL(const Value: string);

  { Add a sound from XML element <sound>.
    BaseUrl must end with slash. }
  procedure ReadSound(const Element: TDOMElement;
    const ParentGroup: TSoundGroup;
    const BaseUrl: String);
  var
    S: TSoundInfo;
    ImportanceStr, ShortName, URLPrefix: String;
    SoundImportanceIndex: Integer;
  begin
    S := TSoundInfo.Create;
    ShortName := Element.AttributeString('name');
    S.Name := ShortName;

    S.ParentGroup := ParentGroup;
    if ParentGroup <> nil then
      S.Name := ParentGroup.Name + '/' + S.Name;

    { init to default values }
    S.Gain := 1;
    S.MinGain := 0;
    S.MaxGain := 1;
    S.DefaultImportance := MaxSoundImportance;

    if FSounds.IndexOfName(S.Name) <> -1 then
      raise EInvalidSoundRepositoryXml.CreateFmt('Sound name "%s" is not unique',
        [S.Name]);
    FSounds.Add(S);

    { retrieve URL using AttributeString
      (that internally uses Element.Attributes.GetNamedItem),
      because we have to distinguish between the case when url/file_name
      attribute is not present (in this case S.URL is left as it was)
      and when it's present and set to empty string
      (in this case S.URL must also be set to empty string).
      Standard Element.GetAttribute wouldn't allow me this. }
    if (Element.AttributeString('url', S.URL) or
        Element.AttributeString('file_name', S.URL)) and
       (S.URL <> '') then
    begin
      S.URL := CombineURI(BaseUrl, S.URL)
    end else
    begin
      URLPrefix := CombineURI(BaseUrl, ShortName);
      if URIFileExists(URLPrefix + '.ogg') then
        S.URL := URLPrefix + '.ogg'
      else
      if URIFileExists(URLPrefix + '.wav') then
        S.URL := URLPrefix + '.wav'
      else
        WritelnWarning('No matching sound file found for sound "%s"', [S.Name]);
    end;

    Element.AttributeSingle('gain', S.Gain);
    Element.AttributeSingle('min_gain', S.MinGain);
    Element.AttributeSingle('max_gain', S.MaxGain);

    { MaxGain is max 1. Although some OpenAL implementations allow > 1,
      Windows impl (from Creative) doesn't. For consistent results,
      we don't allow it anywhere. }
    if S.MaxGain > 1 then
      S.MaxGain := 1;

    if Element.AttributeString('default_importance', ImportanceStr) then
    begin
      SoundImportanceIndex := SoundImportanceNames.IndexOf(ImportanceStr);
      if SoundImportanceIndex = -1 then
        S.DefaultImportance := StrToInt(ImportanceStr) else
        S.DefaultImportance :=
          PtrUInt(SoundImportanceNames.Objects[SoundImportanceIndex]);
    end;

    { set S.FBuffer at the end, when S.URL is set }
    if S.URL <> '' then
      S.FBuffer := LoadBuffer(S.URL, false);
  end;

  { Read a group of sounds from XML element <group>. }
  procedure ReadGroup(const Element: TDOMElement;
    const ParentGroup: TSoundGroup;
    const BaseUrl: String);
  var
    I: TXMLElementIterator;
    Group: TSoundGroup;
    Subdirectory: String;
  begin
    Group := TSoundGroup.Create;
    Group.ParentGroup := ParentGroup;

    { calculate Group.Name }
    Group.Name := Element.AttributeString('name');
    if ParentGroup <> nil then
      Group.Name := ParentGroup.Name + '/' + Group.Name;

    Group.URL := BaseUrl;
    Subdirectory := Element.AttributeStringDef('subdirectory', '');
    if Subdirectory <> '' then
      Group.URL := Group.URL + Subdirectory + '/';

    if FSoundGroups.IndexOfName(Group.Name) <> -1 then
      raise EInvalidSoundRepositoryXml.CreateFmt('Group name "%s" is not unique',
        [Group.Name]);

    { Add to a flat list of groups, to free TSoundGroup at end. }
    FSoundGroups.Add(Group);

    I := Element.ChildrenIterator;
    try
      while I.GetNext do
      begin
        if I.Current.TagName = 'sound' then
          ReadSound(I.Current, Group, Group.URL)
        else
        if I.Current.TagName = 'group' then
          ReadGroup(I.Current, Group, Group.URL)
        else
          raise EInvalidSoundRepositoryXml.CreateFmt('Invalid XML element "%s" in sounds XML file',
            [I.Current.TagName]);
      end;
    finally FreeAndNil(I) end;
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
      if ALActive, since it loads sounds in this case (calls LoadBuffer),
      so can take a while. }

    I := SoundConfig.DocumentElement.ChildrenIterator;
    try
      while I.GetNext do
      begin
        if I.Current.TagName = 'sound' then
          ReadSound(I.Current, nil, BaseUrl)
        else
        if I.Current.TagName = 'group' then
          ReadGroup(I.Current, nil, BaseUrl)
        else
          raise EInvalidSoundRepositoryXml.CreateFmt('Invalid XML element "%s" in sounds XML file',
            [I.Current.TagName]);
      end;
    finally FreeAndNil(I) end;
  finally
    FreeAndNil(SoundConfig);
  end;

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

  { in case you set RepositoryURL when OpenAL context is already
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
  if not ALInitialized and (FSounds.Count > 1) then
    ALContextOpen;
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
  FEngine := AnEngine;
end;

destructor TLoopingChannel.Destroy;
begin
  if FAllocatedSource <> nil then
    FAllocatedSource.Release;
  inherited;
end;

procedure TLoopingChannel.AllocateSource;
begin
  FAllocatedSource := FEngine.PlaySound(
    FEngine.FSounds[Sound.Index].Buffer, false, true,
    MaxSoundImportance,
    Volume * FEngine.FSounds[Sound.Index].Gain, 0, 1,
    TVector3.Zero);

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
      FAllocatedSource.Gain := Volume * FEngine.FSounds[Sound.Index].Gain;
  end;
end;

{ globals -------------------------------------------------------------------- }

var
  FSoundEngine: TRepoSoundEngine;

function SoundEngine: TRepoSoundEngine;
begin
  if FSoundEngine = nil then
    FSoundEngine := TRepoSoundEngine.Create;
  Result := FSoundEngine;
end;

finalization
  FreeAndNil(FSoundEngine);
end.
