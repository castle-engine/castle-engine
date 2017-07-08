{
  Copyright 2010-2017 Michalis Kamburelis.

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

uses SysUtils, Classes, Math, FGL,
  CastleOpenAL, CastleVectors, CastleTimeUtils, CastleXMLConfig,
  CastleClassUtils, CastleStringUtils;

type
  TSound = class;
  TSoundAllocator = class;

  TSoundBuffer = type TALuint;

  TSoundEvent = procedure (Sender: TSound) of object;

  ENoMoreOpenALSources = class(Exception);

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
    FPosition, FVelocity: TVector3Single;
    FLooping, FRelative: boolean;
    FGain, FMinGain, FMaxGain, FPitch: Single;
    FBuffer: TSoundBuffer;
    FRolloffFactor, FReferenceDistance, FMaxDistance: Single;
    FAllocator: TSoundAllocator;
    procedure SetPosition(const Value: TVector3Single);
    procedure SetVelocity(const Value: TVector3Single);
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

      But note that we do not make any guarantees that sources that
      stopped playing will be immediately reported to OnRelease.
      In fact, a source may be considered in Used = @true state
      for a long time until it stopped playing. That's not a problem
      for this unit --- TSoundAllocator.AllocateSound is smart,
      and it may actually check (and eventually mark with @link(Release))
      whether some sources are in playing state,
      to avoid allocating unnecessary sources.
      However, if this is a problem for you (because e.g. you do
      some expensive operations to update all used sources every time)
      and you really desire OnRelease to be called quickly after
      sound stoppped playing, you may call TSoundAllocator.Refresh
      from time to time.

      In this event you should make sure to delete all references
      to this sound, because the TSound instance may
      be freed (or reused for other means) after calling OnRelease.
      For the same reason, after calling this, we always clear it
      (set OnRelease to @nil).

      It's guaranteed that when this will be called,
      @link(Used) will be @false and @link(PlayingOrPaused) will be @false. }
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

    property Position: TVector3Single read FPosition write SetPosition;
    property Velocity: TVector3Single read FVelocity write SetVelocity;
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
  end;

  TSoundList = class(specialize TFPGObjectList<TSound>)
  public
    { Sort sounds by Used + Importance, descending.
      First all sounds with Used = @true are placed,
      starting from the sound with largest Importance, and so on
      until the sound with smallest Importance.
      Then all sounds with Used = @false are placed (in any, arbitrary order).

      List must not contain nil values when calling this. }
    procedure SortByImportance;
  end;

  { Manager of allocated OpenAL sounds.
    You leave to this class creating and deleting of OpenAL sounds.
    When you need OpenAL sound to do something, just call AllocateSound method.

    This class will manage OpenAL sources in an intelligent manner,
    which means when you need new sound, we may
    @orderedList(
      @item(Reuse already allocated sound that is not used to play anything.)
      @item(Allocate new sound (but we will keep allocated sounds count
        within MaxAllocatedSources sound limit, to not overload OpenAL
        implementation with work).)
      @item(We may simply interrupt already allocated sound, if new
        sound is more important.)
    )

    Our OpenAL resources are created in ALContextOpen, and released
    in ALContextClose.

    The very reason behind this class is to hide from you the fact that
    the number of OpenAL sources are limited. In particular, this
    means that when OpenAL will run out of sources, no OpenAL error
    (alGetError) will be left, and no exception will be raised.
    In the worst case TSoundAllocator.AllocateSound will return nil,
    but in more probable cases some other sources (unused, or with
    less priority) will be reused.

    Note that this means that the code in this unit must
    read in some situations alGetError. That's because reading alGetError
    is the only way to know when OpenAL implementation has run out of sources.
    So the code in this unit may in various places raise EALError if you
    made some error in your OpenAL code, and you didn't check alGetError
    yourself often enough. }
  TSoundAllocator = class
  private
    FAllocatedSources: TSoundList;
    FMinAllocatedSources: Cardinal;
    FMaxAllocatedSources: Cardinal;
    procedure SetMinAllocatedSources(const Value: Cardinal);
    procedure SetMaxAllocatedSources(const Value: Cardinal);
  public
    const
      DefaultMinAllocatedSources = 4;
      DefaultMaxAllocatedSources = 16;

    constructor Create;
    destructor Destroy; override;
    procedure ALContextOpen; virtual;
    procedure ALContextClose; virtual;

    { Is the OpenAL version at least @code(AMajor.AMinor).
      Available only when OpenAL is initialized, that is:
      between @link(ALContextOpen) and @link(ALContextClose),
      only when @link(TSoundEngine.ALActive). }
    function ALVersionAtLeast(const AMajor, AMinor: Integer): boolean; virtual; abstract;

    { Allocate sound for playing. You should initialize the OpenAL sound
      properties and start playing the sound (you have
      OpenAL sound identifier in TSound.ALSource).

      Note that if you don't call alSourcePlay, the source may be detected
      as unused (and recycled for another sound) at the next AllocateSound,
      PlaySound, @link(Refresh) and such calls.

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
      Useful only for advanced or debuging tasks, in normal circumstances
      we mange this completely ourselves. This is @nil when ALContextOpen
      was not yet called. }
    property AllocatedSources: TSoundList read FAllocatedSources;

    { Detect unused sound sources. If you rely on your sources receiving
      TSound.OnRelease in a timely manner, be sure to call
      this method often. Otherwise, it's not needed to call this at all
      (unused sounds will be detected automatically on-demand anyway).

      This is automatically called by TCastleSceneManager.

      For every source that is marked as Used, this checks
      whether this source is actually in playing/paused state
      right now. If not, it calls @link(TSound.Release) (thus setting
      Used to @false and triggering OnRelease) for this source. }
    procedure Refresh;

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

  ESoundBufferNotLoaded = class(Exception);

  TSoundBuffersCache = class
    URL: string; //< Absolute URL.
    Buffer: TSoundBuffer;
    Duration: TFloatTime;
    References: Cardinal;
  end;
  TSoundBuffersCacheList = specialize TFPGObjectList<TSoundBuffersCache>;

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
  TSoundDeviceList = specialize TFPGObjectList<TSoundDevice>;

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
    Position: TVector3Single;
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
    BuffersCache: TSoundBuffersCacheList;
    FDevices: TSoundDeviceList;
    FOnOpenClose: TNotifyEventList;
    FResumeToInitialized, FPaused: boolean;

    { We record listener state regardless of ALActive. This way at the ALContextOpen
      call we can immediately set the good listener parameters. }
    ListenerPosition: TVector3Single;
    ListenerOrientation: TALTwoVectors3f;

    FEnableSaveToConfig, DeviceSaveToConfig: boolean;

    { Check ALC errors. Requires valid ALDevice. }
    procedure CheckALC(const situation: string);

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

    procedure LoadFromConfig(const Config: TCastleConfig); override;
    procedure SaveToConfig(const Config: TCastleConfig); override;

    { Is the OpenAL version at least @code(AMajor.AMinor). }
    function ALVersionAtLeast(const AMajor, AMinor: Integer): boolean; override;

    { Initialize OpenAL library, and output device and context.
      Sets ALInitialized, ALActive, @link(Information), EFXSupported,
      ALMajorVersion, ALMinorVersion.
      You can set @link(Device) before calling this.

      Note that we continue (without any exception) if the initialization
      failed for any reason (maybe OpenAL library is not available,
      or no sound output device is available).
      You can check things like ALActive and @link(Information),
      but generally this class
      will hide from you the fact that sound is not initialized. }
    procedure ALContextOpen; override;

    { Release OpenAL context and resources.

      ALInitialized and ALActive are set to @false. It's allowed and harmless
      to cal this when one of them is already @false. }
    procedure ALContextClose; override;

    { Do we have active OpenAL context. This is @true when you successfully
      called ALContextOpen (and you didn't call ALContextClose yet).
      This also implies that OpenAL library is loaded, that is ALInitialized = @true. }
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

    { Load a sound file into OpenAL buffer.

      Result is 0 only if we don't have a valid OpenAL context.
      Note that this method will automatically call ALContextOpen if it wasn't
      called yet. So result = zero means that ALContextOpen was called, but for some
      reason failed.

      The buffer should be released by FreeBuffer later when it's not needed.
      Although we will take care to always free remaining buffers
      before closing OpenAL context anyway. (And OpenAL would also free
      the buffer anyway at closing, although some OpenAL versions
      could write a warning about this.)

      We have a cache of sound files here. An absolute URL
      will be recorded as being loaded to given buffer. Loading the same
      URL second time returns the same OpenAL buffer. The buffer
      is released only once you call FreeBuffer as many times as you called
      LoadBuffer for it.
      @groupBegin }
    function LoadBuffer(const URL: string; out Duration: TFloatTime): TSoundBuffer;
    function LoadBuffer(const URL: string): TSoundBuffer;
    { @groupEnd }

    { Free a sound file buffer. Ignored when buffer is zero.
      Buffer is always set to zero after this.

      @raises(ESoundBufferNotLoaded When invalid (not zero,
        and not returned by LoadBuffer) buffer identifier is given.) }
    procedure FreeBuffer(var Buffer: TSoundBuffer);

    { Play a sound from given buffer.

      We use a smart OpenAL sound allocator, so the sound will be actually
      played only if resources allow. Use higher Importance to indicate
      sounds that are more important to play.

      We set the sound properties and start playing it.

      Both spatialized (3D) and not sounds are possible.
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
    function PlaySound(const Buffer: TSoundBuffer): TSound;
    function PlaySound(const Buffer: TSoundBuffer;
      const Spatial, Looping: boolean; const Importance: Cardinal;
      const Gain, MinGain, MaxGain: Single;
      const Position: TVector3Single;
      const Pitch: Single = 1): TSound;
    function PlaySound(const Buffer: TSoundBuffer;
      const Spatial, Looping: boolean; const Importance: Cardinal;
      const Gain, MinGain, MaxGain: Single;
      const Position: TVector3Single;
      const Pitch: Single;
      const ReferenceDistance: Single;
      const MaxDistance: Single): TSound; deprecated 'use PlaySound that gets TSoundParameters instance';
    function PlaySound(const Parameters: TSoundParameters): TSound;

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
      [http://castle-engine.sourceforge.net/openal_notes.php#section_options] }
    procedure ParseParameters;

    { Help string for options parsed by ParseParameters.

      Note that it also lists the available OpenAL @link(Devices),
      as they are valid arguments for the @--audio-device option. }
    function ParseParametersHelp: string;

    { Set OpenAL listener position and orientation. }
    procedure UpdateListener(const Position, Direction, Up: TVector3Single);

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

  { Sound information, internally used by TRepoSoundEngine.

    The fields correspond to appropriate attributes in sounds XML file.
    All of the fields except Buffer are initialized only by ReadSounds.

    From the point of view of end-user the number of sounds
    is constant for given game and their properties (expressed in
    TSoundInfo below) are also constant.
    However, for the sake of debugging/testing the game,
    and for content designers, the actual values of Sounds are loaded
    at initialization by ReadSounds (called automatically by ALContextOpen)
    from sounds XML file,
    and later can be changed by calling ReadSounds once again during the
    game (debug menu may have command like "Reload sounds/index.xml"). }
  TSoundInfo = class
  private
    FBuffer: TSoundBuffer;

    { OpenAL buffer of this sound. Zero if buffer is not yet loaded,
      which may happen only if TRepoSoundEngine.ALContextOpen was not yet
      called or when sound has URL = ''. }
    property Buffer: TSoundBuffer read FBuffer;
  public
    { Unique sound name. Empty for the special sound stNone. }
    Name: string;

    { URL from which to load sound data.

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

      When this sound is used for MusicPlayer.Sound:
      @orderedList(
        @item(MinGain, MaxGain are ignored.)
        @item(Effective Gain (passed to OpenAL sound source) is the
          TMusicPlayer.MusicVolume multiplied by our @link(Gain).)
      ) }
    Gain, MinGain, MaxGain: Single;

    { How important the sound is. Influences what happens when we have a lot
      of sounds playing at once. See TSound.Importance.

      Ignored when this sound is used for MusicPlayer.Sound. }
    DefaultImportance: Cardinal;
  end;

  TSoundInfoList = specialize TFPGObjectList<TSoundInfo>;

  { Unique sound type identifier for sounds used within TRepoSoundEngine. }
  TSoundType = object
  private
    { Just an index to TRepoSoundEngine.SoundNames array. }
    Index: Cardinal;
  public
    function Info: TSoundInfo;
  end;

  TMusicPlayer = class;

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
    FSoundImportanceNames: TStringList;
    FSounds: TSoundInfoList;
    FRepositoryURL: string;
    { This is the only allowed instance of TMusicPlayer class,
      created and destroyed in this class create/destroy. }
    FMusicPlayer: TMusicPlayer;
    procedure SetRepositoryURL(const Value: string);
    { Load buffers for all Sounds. Should be called as soon as Sounds changes
      and we may have OpenAL context (but it checks ALActive and Sounds.Count,
      and does something only if we have OpenAL context and some sounds,
      so it's actually safe to call it always). }
    procedure LoadSoundsBuffers;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromConfig(const Config: TCastleConfig); override;
    procedure SaveToConfig(const Config: TCastleConfig); override;

    { In addition to initializing OpenAL context, this also loads sound files. }
    procedure ALContextOpen; override;
    procedure ALContextClose; override;

    { The XML file that contains description of your sounds.
      This should be an URL (in simple cases, just a filename)
      pointing to an XML file describing your sounds.
      See http://castle-engine.sourceforge.net/creating_data_sound.php and
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
        SoundEngine.Sound3D(stMySound1, Vector3Single(0, 0, 10));
      #)

      See CastleFilesUtils unit for docs of ApplicationData function.
    }
    property RepositoryURL: string read FRepositoryURL write SetRepositoryURL;

    { Deprecated name for RepositoryURL. @deprecated }
    property SoundsFileName: string read FRepositoryURL write SetRepositoryURL; deprecated;

    { Reload the RepositoryURL and all referenced buffers.
      Useful as a tool for 3D data designers, to reload the sounds XML file
      without restarting the game/sound engine etc. }
    procedure ReloadSounds;

    { A list of sounds used by your program.
      Each sound has a unique name, used to identify sound in
      the XML file and for SoundFromName function.

      At the beginning, this list always contains exactly one sound: empty stNone.
      This is a special "sound type" that has index 0 (should be always
      expressed as TSoundType value stNone) and name ''.
      stNone is a special sound as it actually means "no sound" in many cases. }
    property Sounds: TSoundInfoList read FSounds;

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
      const Position: TVector3Single;
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

    property MusicPlayer: TMusicPlayer read FMusicPlayer;
  end;

  { Music player, to easily play a sound preloaded by TRepoSoundEngine.
    Instance of this class should be created only internally
    by the TRepoSoundEngine, always use this through TRepoSoundEngine.MusicPlayer. }
  TMusicPlayer = class
  private
    { Engine that owns this music player. }
    FEngine: TRepoSoundEngine;

    { This is nil if we don't play music right now
      (because OpenAL is not initialized, or Sound = stNone,
      or PlayerSound.URL = '' (sound not existing)). }
    FAllocatedSource: TSound;

    FMusicVolume: Single;

    FSound: TSoundType;
    procedure SetSound(const Value: TSoundType);
    procedure AllocatedSourceRelease(Sender: TSound);

    { Called by ALContextOpen. You should check here if
      Sound <> stNone and eventually initialize FAllocatedSource. }
    procedure AllocateSource;
    function GetMusicVolume: Single;
    procedure SetMusicVolume(const Value: Single);
  public
    const
      DefaultMusicVolume = 1.0;
    constructor Create(AnEngine: TRepoSoundEngine);
    destructor Destroy; override;

    { Currently played music.
      Set to stNone to stop playing music.
      Set to anything else to play that music.

      Changing value of this property (when both the old and new values
      are <> stNone and are different) restarts playing the music.

      By default none (stNone). }
    property Sound: TSoundType read FSound write SetSound;

    { Music volume. This must always be within 0..1 range.
      0.0 means that there is no music (this case should be optimized).}
    property MusicVolume: Single read GetMusicVolume write SetMusicVolume
      default DefaultMusicVolume;
  end;

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

operator = (const SoundType1, SoundType2: TSoundType): boolean;

implementation

uses DOM, XMLRead, StrUtils,
  CastleUtils, CastleALUtils, CastleLog, CastleProgress,
  CastleSoundFile, CastleInternalVorbisFile, CastleEFX, CastleParameters,
  CastleXMLUtils, CastleFilesUtils, CastleConfig,
  CastleURIUtils, CastleDownload, CastleMessaging, CastleApplicationProperties;

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
  Buffer := 0;

  if Assigned(OnRelease) then
  begin
    OnRelease(Self);
    OnRelease := nil;
  end;
end;

procedure TSound.SetPosition(const Value: TVector3Single);
begin
  FPosition := Value;
  alSourceVector3f(ALSource, AL_POSITION, Value);
end;

procedure TSound.SetVelocity(const Value: TVector3Single);
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
  { TSoundBuffer is unsigned, while alSourcei is declared as taking signed integer.
    But we know we can pass TSoundBuffer to alSourcei, just typecasting it to
    whatever alSourcei requires. }
  {$I norqcheckbegin.inc}
  alSourcei(ALSource, AL_BUFFER, Value);
  {$I norqcheckend.inc}
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

{ TSoundList ----------------------------------------------------- }

function IsSmallerByImportance(const AA, BB: TSound): Integer;
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
begin
  Sort(@IsSmallerByImportance);
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

procedure TSoundAllocator.ALContextOpen;
var
  I: Integer;
begin
  FAllocatedSources := TSoundList.Create(true);
  FAllocatedSources.Count := MinAllocatedSources;
  for I := 0 to FAllocatedSources.Count - 1 do
    FAllocatedSources[I] := TSound.Create(Self);
end;

procedure TSoundAllocator.ALContextClose;
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
      { Refresh is useful here, so that we really cut off
        the *currently* unused sources. }
      Refresh;
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
var
  I: Integer;
begin
  CheckAL('before Refresh');

  if FAllocatedSources <> nil then
    for I := 0 to FAllocatedSources.Count - 1 do
      if FAllocatedSources[I].Used and
         (not FAllocatedSources[I].PlayingOrPaused) then
        FAllocatedSources[I].Release;
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
  BuffersCache := TSoundBuffersCacheList.Create;
  FOnOpenClose := TNotifyEventList.Create;

  { Default OpenAL listener attributes }
  ListenerPosition := ZeroVector3Single;
  ListenerOrientation[0] := Vector3Single(0, 0, -1);
  ListenerOrientation[1] := Vector3Single(0, 1, 0);

  // automatic loading/saving is more troublesome than it's worth
  // Config.AddLoadListener(@LoadFromConfig);
  // Config.AddSaveListener(@SaveToConfig);

  ApplicationProperties.OnInitializeJavaActivity.Add(@ReinitializeJavaActivity);
  ApplicationProperties.OnPause.Add(@ApplicationPause);
  ApplicationProperties.OnResume.Add(@ApplicationResume);
end;

destructor TSoundEngine.Destroy;
begin
  if ApplicationProperties(false) <> nil then
  begin
    ApplicationProperties(false).OnInitializeJavaActivity.Remove(@ReinitializeJavaActivity);
    ApplicationProperties(false).OnPause.Remove(@ApplicationPause);
    ApplicationProperties(false).OnResume.Remove(@ApplicationResume);
  end;

  // automatic loading/saving is more troublesome than it's worth
  // if Config <> nil then
  // begin
  //   Config.RemoveLoadListener(@LoadFromConfig);
  //   Config.RemoveSaveListener(@SaveToConfig);
  // end;

  ALContextClose;
  FreeAndNil(BuffersCache);
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

    if ALInitialized and EnumerationExtPresent(pDeviceList) then
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
    if ALInitialized and OpenALSampleImplementation then
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
        devices. It's listed on [http://castle-engine.sourceforge.net/openal_notes.php]
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

procedure TSoundEngine.CheckALC(const situation: string);
var
  err: TALenum;
  alcErrDescription: PChar;
  alcErrDescriptionStr: string;
begin
  err := alcGetError(ALDevice);
  if err <> ALC_NO_ERROR then
  begin
    { moznaby tu uproscic zapis eliminujac zmienne alcErrDescription i alcErrDescriptionStr
      i zamiast alcErrDescriptionStr uzyc po prostu alcGetString(ALDevice, err).
      Jedynym powodem dla ktorego jednak wprowadzam tu ta mala komplikacje jest fakt
      ze sytuacja ze alcGetError zwroci cos niespodziewanego (bledny kod bledu) niestety
      zdarza sie (implementacja Creative pod Windows nie jest doskonala...).
      W zwiazku z tym chcemy sie nia zajac. }
    alcErrDescription := alcGetString(ALDevice, err);
    if alcErrDescription = nil then
     alcErrDescriptionStr := Format('(alc does not recognize this error number : %d)', [err]) else
     alcErrDescriptionStr := alcErrDescription;

    raise EALCError.Create(err,
      'OpenAL error ALC_xxx at '+situation+' : '+alcErrDescriptionStr);
  end;
end;

function TSoundEngine.GetContextString(Enum: TALCenum): string;
begin
  result := alcGetString(ALDevice, enum);
  try
    CheckALC('alcGetString');
    { Check also normal al error (alGetError instead
      of alcGetError). Seems that when Darwin (Mac OS X) Apple's OpenAL
      implementation fails to return some alcGetString
      it reports this by setting AL error (instead of ALC one)
      to "invalid value". Although (after fixes to detect OpenALSampleImplementation
      at runtime and change constants values) this shouldn't happen anymore
      it you pass normal consts to this function. }
    CheckAL('alcGetString');
  except
    on E: EALCError do result := '('+E.Message+')';
    on E: EALError do result := '('+E.Message+')';
  end;
end;

procedure TSoundEngine.ALContextOpen;

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

      CheckALInitialized;

      ALDevice := alcOpenDevice(PCharOrNil(Device));
      if (ALDevice = nil) then
        raise EOpenALError.CreateFmt(
          'OpenAL''s audio device "%s" is not available', [Device]);

      ALContext := alcCreateContext(ALDevice, nil);
      CheckALC('initing OpenAL (alcCreateContext)');

      alcMakeContextCurrent(ALContext);
      CheckALC('initing OpenAL (alcMakeContextCurrent)');

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
      'OggVorbis handling method: %s' +NL+
      'vorbisfile library available: %s',
      [ alGetString(AL_VERSION),
        FALMajorVersion, FALMinorVersion,
        alGetString(AL_RENDERER),
        alGetString(AL_VENDOR),
        alGetString(AL_EXTENSIONS),
        MinAllocatedSources, MaxAllocatedSources,
        TSoundOggVorbis.VorbisMethod,
        BoolToStr(VorbisFileInitialized, true)
      ]);
  end;

var
  ALActivationErrorMessage: string;
begin
  if Paused then
    Exit; // do not even set ALInitialized to true

  Assert(not ALActive, 'OpenAL context is already active');
  Assert(not ALInitialized, 'OpenAL context initialization was already attempted');

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
      except
        ALContextClose;
        raise;
      end;
    end;
  end;

  FALInitialized := true;
  if Log then
    WritelnLogMultiline('Sound', Information);

  OnOpenClose.ExecuteAll(Self);
end;

procedure TSoundEngine.ALContextClose;

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
  I: Integer;
begin
  if ALInitialized then
  begin
    FALInitialized := false;
    if ALActive then
    begin
      { release sound allocator first. This also stops all the sources,
        which is required before we try to release their buffers. }
      inherited;

      for I := 0 to BuffersCache.Count - 1 do
        alFreeBuffer(BuffersCache[I].Buffer);
      BuffersCache.Count := 0;

      EndAL;
    end;

    if Log then
      WritelnLog('Sound', 'OpenAL closed');

    OnOpenClose.ExecuteAll(Self);
  end;
end;

function TSoundEngine.PlaySound(const Parameters: TSoundParameters): TSound;
const
  { For now, just always use CheckBufferLoaded. It doesn't seem to cause
    any slowdown for normal sound playing. }
  CheckBufferLoaded = true;
begin
  Result := nil;

  if ALActive and (Parameters.Buffer <> 0) then
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
        Result.Position := ZeroVector3Single;
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
            Writeln(SoundInfos.L[Sound].Buffer, ' ',
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
        while Parameters.Buffer <> alGetSource1ui(Result.ALSource, AL_BUFFER) do
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
  const Position: TVector3Single;
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
  const Position: TVector3Single;
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

function TSoundEngine.LoadBuffer(const URL: string;
  out Duration: TFloatTime): TSoundBuffer;
var
  I: Integer;
  Cache: TSoundBuffersCache;
  FullURL: string;
begin
  if not ALInitialized then ALContextOpen;
  if not ALActive then Exit(0);

  FullURL := AbsoluteURI(URL);

  { try to load from cache (Result and Duration) }
  for I := 0 to BuffersCache.Count - 1 do
    if BuffersCache[I].URL = FullURL then
    begin
      Inc(BuffersCache[I].References);
      if Log then
        WritelnLog('Sound', Format('Loaded "%s" from cache, now has %d references',
          [URIDisplay(FullURL), BuffersCache[I].References]));
      Duration := BuffersCache[I].Duration;
      Exit(BuffersCache[I].Buffer);
    end;

  { actually load, and add to cache }
  alCreateBuffers(1, @Result);
  try
    TALSoundFile.alBufferDataFromFile(Result, URL, Duration);
  except alDeleteBuffers(1, @Result); raise end;

  Cache := TSoundBuffersCache.Create;
  Cache.URL := FullURL;
  Cache.Buffer := Result;
  Cache.Duration := Duration;
  Cache.References := 1;
  BuffersCache.Add(Cache);
end;

function TSoundEngine.LoadBuffer(const URL: string): TSoundBuffer;
var
  Dummy: TFloatTime;
begin
  Result := LoadBuffer(URL, Dummy);
end;

procedure TSoundEngine.FreeBuffer(var Buffer: TSoundBuffer);
var
  I: Integer;
begin
  if Buffer = 0 then Exit;

  for I := 0 to BuffersCache.Count - 1 do
    if BuffersCache[I].Buffer = Buffer then
    begin
      Buffer := 0;
      Dec(BuffersCache[I].References);
      if BuffersCache[I].References = 0 then
      begin
        alFreeBuffer(BuffersCache[I].Buffer);
        BuffersCache.Delete(I);
      end;
      Exit;
    end;

  raise ESoundBufferNotLoaded.CreateFmt('OpenAL buffer %d not loaded', [Buffer]);
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
    if not ALInitialized then
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
        Result += '                          ' + Devices[i].Caption;
        if Devices[i].Name <> Devices[i].Caption then
          Result += ' (Real OpenAL name: "' + Devices[i].Name + '")';
        if Devices[i].Name = DefaultDeviceName then
          Result += ' (Equivalent to default device)';
        Result += nl;
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

procedure TSoundEngine.UpdateListener(const Position, Direction, Up: TVector3Single);
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

{ TSoundType ----------------------------------------------------------------- }

operator = (const SoundType1, SoundType2: TSoundType): boolean;
begin
  Result := SoundType1.Index = SoundType2.Index;
end;

function TSoundType.Info: TSoundInfo;
begin
  Result := SoundEngine.Sounds[Index];
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

  FSounds := TSoundInfoList.Create;
  { add stNone sound }
  Sounds.Add(TSoundInfo.Create);

  FMusicPlayer := TMusicPlayer.Create(Self);

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
  FreeAndNil(FMusicPlayer);
  inherited;
end;

procedure TRepoSoundEngine.ALContextOpen;
begin
  inherited;
  LoadSoundsBuffers;
end;

procedure TRepoSoundEngine.LoadSoundsBuffers;
var
  SoundIndex: Integer;
  ProgressActive: boolean;
begin
  { load sound buffers and allocate sound for music. Only if we have any sound
    (other than stNone). }
  if ALActive and (Sounds.Count > 1) then
  begin
    ProgressActive := Progress.Active;
    if not ProgressActive then
      Progress.Init(Sounds.Count - 1, 'Loading sounds');
    try
      { We do progress to "Sounds.Count - 1" because we start
        iterating from SoundIndex = 1 because SoundIndex = 0 never exists. }
      Assert(Sounds[0].URL = '');
      for SoundIndex := 1 to Sounds.Count - 1 do
      begin
        if Sounds[SoundIndex].URL <> '' then
        try
          Sounds[SoundIndex].FBuffer := LoadBuffer(Sounds[SoundIndex].URL);
        except
          on E: Exception do
          begin
            Sounds[SoundIndex].FBuffer := 0;
            WritelnWarning('Sound', Format('Sound file "%s" cannot be loaded: %s',
              [Sounds[SoundIndex].URL, E.Message]));
          end;
        end;
        if not ProgressActive then
          Progress.Step;
      end;
    finally
      if not ProgressActive then
        Progress.Fini;
    end;

    MusicPlayer.AllocateSource;
  end;
end;

procedure TRepoSoundEngine.ALContextClose;
var
  SoundIndex: Integer;
begin
  if ALActive then
  begin
    StopAllSources;
    { this is called from TSoundEngine.Destroy, so be secure and check
      Sounds for nil }
    if Sounds <> nil then
      for SoundIndex := 0 to Sounds.Count - 1 do
        FreeBuffer(Sounds[SoundIndex].FBuffer);
  end;
  inherited;
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
  if (SoundType.Index = 0) or (Sounds[SoundType.Index].URL = '') then Exit(nil);

  if not ALInitialized then ALContextOpen;

  { Check this *after* ALContextOpen, since Buffer is always zero before OpenAL
    is initialized. }
  if Sounds[SoundType.Index].Buffer = 0 then Exit(nil);

  Result := PlaySound(
    Sounds[SoundType.Index].Buffer, false, Looping,
    Sounds[SoundType.Index].DefaultImportance,
    Sounds[SoundType.Index].Gain,
    Sounds[SoundType.Index].MinGain,
    Sounds[SoundType.Index].MaxGain,
    ZeroVector3Single);
end;

function TRepoSoundEngine.Sound3D(SoundType: TSoundType;
  const Position: TVector3Single;
  const Looping: boolean): TSound;
begin
  { If there is no actual sound, exit early without initializing OpenAL.
    See Sound for duplicate of this "if" and more comments. }
  if (SoundType.Index = 0) or (Sounds[SoundType.Index].URL = '') then Exit(nil);

  if not ALInitialized then ALContextOpen;

  { Do it *after* ALContextOpen, since Buffer is always zero before OpenAL
    is initialized. }
  if Sounds[SoundType.Index].Buffer = 0 then Exit(nil);

  Result := PlaySound(
    Sounds[SoundType.Index].Buffer, true, Looping,
    Sounds[SoundType.Index].DefaultImportance,
    Sounds[SoundType.Index].Gain,
    Sounds[SoundType.Index].MinGain,
    Sounds[SoundType.Index].MaxGain,
    Position);
end;

procedure TRepoSoundEngine.SetRepositoryURL(const Value: string);
var
  SoundConfig: TXMLDocument;
  ImportanceStr: string;
  SoundImportanceIndex: Integer;
  I: TXMLElementIterator;
  RepositoryURLAbsolute: string;
  S: TSoundInfo;
  Stream: TStream;
begin
  if FRepositoryURL = Value then Exit;
  FRepositoryURL := Value;

  Sounds.Clear;
  { add stNone sound }
  Sounds.Add(TSoundInfo.Create);

  { if no sounds XML file, then that's it --- no more sounds }
  if RepositoryURL = '' then Exit;

  { This must be an absolute path, since Sounds[].URL should be
    absolute (to not depend on the current dir when loading sound files. }
  RepositoryURLAbsolute := AbsoluteURI(RepositoryURL);

  Stream := Download(RepositoryURLAbsolute);
  try
    ReadXMLFile(SoundConfig, Stream, RepositoryURLAbsolute);
  finally FreeAndNil(Stream) end;

  try
    Check(SoundConfig.DocumentElement.TagName = 'sounds',
      'Root node of sounds/index.xml must be <sounds>');

    I := SoundConfig.DocumentElement.ChildrenIterator;
    try
      while I.GetNext do
      begin
        Check(I.Current.TagName = 'sound',
          'Each child of sounds/index.xml root node must be the <sound> element');

        S := TSoundInfo.Create;
        S.Name := I.Current.AttributeString('name');

        { init to default values }
        S.URL := CombineURI(RepositoryURLAbsolute, S.Name + '.wav');
        S.Gain := 1;
        S.MinGain := 0;
        S.MaxGain := 1;
        S.DefaultImportance := MaxSoundImportance;
        S.FBuffer := 0; {< initially, will be loaded later }

        Sounds.Add(S);

        { retrieve URL using AttributeString
          (that internally uses I.Current.Attributes.GetNamedItem),
          because we have to distinguish between the case when url/file_name
          attribute is not present (in this case S.URL is left as it was)
          and when it's present and set to empty string
          (in this case S.URL must also be set to empty string).
          Standard I.Current.GetAttribute wouldn't allow me this. }
        if (I.Current.AttributeString('url', S.URL) or
            I.Current.AttributeString('file_name', S.URL)) and
           (S.URL <> '') then
          { Make URL absolute, using RepositoryURLAbsolute, if non-empty URL
            was specified in XML file. }
          S.URL := CombineURI(RepositoryURLAbsolute, S.URL);

        I.Current.AttributeSingle('gain', S.Gain);
        I.Current.AttributeSingle('min_gain', S.MinGain);
        I.Current.AttributeSingle('max_gain', S.MaxGain);

        { MaxGain is max 1. Although some OpenAL implementations allow > 1,
          Windows impl (from Creative) doesn't. For consistent results,
          we don't allow it anywhere. }
        if S.MaxGain > 1 then
          S.MaxGain := 1;

        if I.Current.AttributeString('default_importance', ImportanceStr) then
        begin
          SoundImportanceIndex := SoundImportanceNames.IndexOf(ImportanceStr);
          if SoundImportanceIndex = -1 then
            S.DefaultImportance := StrToInt(ImportanceStr) else
            S.DefaultImportance :=
              PtrUInt(SoundImportanceNames.Objects[SoundImportanceIndex]);
        end;
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
    initialized, load buffers now }
  LoadSoundsBuffers;
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
  for SoundIndex := 0 to Sounds.Count - 1 do
    if Sounds[SoundIndex].Name = SoundName then
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

procedure TRepoSoundEngine.LoadFromConfig(const Config: TCastleConfig);
begin
  inherited;
  Volume := Config.GetFloat('sound/volume', DefaultVolume);
  MusicPlayer.MusicVolume := Config.GetFloat('sound/music/volume',
    TMusicPlayer.DefaultMusicVolume);
end;

procedure TRepoSoundEngine.SaveToConfig(const Config: TCastleConfig);
begin
  Config.SetDeleteFloat('sound/volume', Volume, DefaultVolume);
  { This may be called from destructors and the like, so better check
    that MusicPlayer is not nil. }
  if MusicPlayer <> nil then
    Config.SetDeleteFloat('sound/music/volume',
      MusicPlayer.MusicVolume, TMusicPlayer.DefaultMusicVolume);
  inherited;
end;

{ TMusicPlayer --------------------------------------------------------------- }

constructor TMusicPlayer.Create(AnEngine: TRepoSoundEngine);
begin
  inherited Create;
  FMusicVolume := DefaultMusicVolume;
  FEngine := AnEngine;
end;

destructor TMusicPlayer.Destroy;
begin
  if FAllocatedSource <> nil then
    FAllocatedSource.Release;
  inherited;
end;

procedure TMusicPlayer.AllocateSource;
begin
  FAllocatedSource := FEngine.PlaySound(
    FEngine.Sounds[Sound.Index].Buffer, false, true,
    MaxSoundImportance,
    MusicVolume * FEngine.Sounds[Sound.Index].Gain, 0, 1,
    ZeroVector3Single);

  if FAllocatedSource <> nil then
    FAllocatedSource.OnRelease := @AllocatedSourceRelease;
end;

procedure TMusicPlayer.SetSound(const Value: TSoundType);
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

procedure TMusicPlayer.AllocatedSourceRelease(Sender: TSound);
begin
  Assert(Sender = FAllocatedSource);
  FAllocatedSource := nil;
end;

function TMusicPlayer.GetMusicVolume: Single;
begin
  Result := FMusicVolume;
end;

procedure TMusicPlayer.SetMusicVolume(const Value: Single);
begin
  if Value <> FMusicVolume then
  begin
    FMusicVolume := Value;
    if FAllocatedSource <> nil then
      FAllocatedSource.Gain := MusicVolume * FEngine.Sounds[Sound.Index].Gain;
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
