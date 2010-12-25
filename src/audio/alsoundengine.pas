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

uses SysUtils, Classes, KambiOpenAL, ALSoundAllocator, VectorMath;

const
  DefaultVolume = 1.0;

type
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
  TALSoundEngine = class(TALSoundAllocator)
  private
    FSoundInitializationReport: string;
    FDevice: string;
    FALActive: boolean;
    FEFXSupported: boolean;
    FVolume: Single;
    ALDevice: PALCdevice;
    ALContext: PALCcontext;
    FEnable: boolean;

    { Check ALC errors. Requires valid ALDevice. }
    procedure CheckALC(const situation: string);

    procedure SetVolume(const Value: Single);
  public
    constructor Create;

    { Initialize OpenAL library, and output device and context.
      Sets SoundInitializationReport and ALActive.
      You can set @link(Device) before calling this.

      If the context will be successfully activated, we will also try to init
      EFX extensions for it by doing EFXSupported := Load_EFX(Device).
      If the context will not be activated for whatever reason,
      EFXSupported will be set to @false.

      Note that we continue (without any exception) if the initialization
      failed for any reason (maybe OpenAL library is not available,
      or no sound output device is available).
      You can check things like ALActive and SoundInitializationReport,
      but generally this class
      will hide from you the fact that sound is not initialized. }
    procedure ALContextOpen; override;

    { Release OpenAL context and resources.

      ALActive is set to @false. This is ignored if ALActive is already @false. }
    procedure ALContextClose; override;

    { Do we have active OpenAL context. This is @true when you successfully
      called ALContextOpen (and you didn't call ALContextClose yet).
      This also implies that OpenAL library is loaded, that is ALInited = @true. }
    property ALActive: boolean read FALActive;

    { Are OpenAL effects (EFX) extensions supported.
      Meaningful only when ALActive, that is it's initialized by ALContextOpen. }
    property EFXSupported: boolean read FEFXSupported;

    property SoundInitializationReport: string read FSoundInitializationReport;

    { Wrapper for alcGetString. }
    function GetContextString(Enum: TALCenum): string;

    { If ALActive, then will append some info about current OpenAL used. }
    procedure AppendALInformation(S: TStrings);
    function ALInformation: string;

    { Change @link(Device) while OpenAL is already initialized.
      This cleanly closes the old device (ALContextClose),
      changes @link(Device) value, initializes context again (ALContextOpen). }
    procedure ALChangeDevice(const NewDevice: string);

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

      @returns(The allocated sound as TALSound.

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
      const Position: TVector3Single): TALSound;

    { Parse parameters in @link(Parameters) and interprets and removes
      recognized options. Internally it uses ParseParameters with
      ParseOnlyKnownLongOptions = @true. Recognized options:

      @definitionList(
        @itemLabel @--audio-device DEVICE-NAME
        @item Set @link(Device) variable to given argument.

        @itemLabel @--print-audio-devices
        @item(
          Use ALC_ENUMERATION_EXT to print all available OpenAL audio devices
          to stdout (uses InfoWrite, so on Windows when program is GUI, it will
          make a dialog box).
          If this extension is not present, write something
          like "Enumerating audio devices not supported by your OpenAL".

          Then do ProgramBreak.)

        @itemLabel @--no-sound
        @item Disable any sound (sets @link(Enable) to @false).
      )

      More user-oriented documentation for the above options is here:
      [http://vrmlengine.sourceforge.net/openal_notes.php#section_options] }
    procedure ParseParameters;

    { Help string for options parsed by ParseParameters.

      Formatting is consistent with Kambi standards
      (see file @code(../base/README.kambi_command_line_params)).

      If PrintCurrentDeviceAsDefault then it will also say (near
      the help for option @--audio-device) that "defauls device is ..."
      and will give here current value of Device.
      This is usually useful, e.g. if you don't intend to modify directly
      Device (only indirectly via ParseParameters)
      then you should give here true. }
    function ParseParametersHelp(PrintCurrentDeviceAsDefault: boolean): string;
  published
    { Sound volume, affects all OpenAL sounds (effects and music).
      This must always be within 0..1 range.
      0.0 means that there are no effects (this case should be optimized). }
    property Volume: Single read FVolume write SetVolume
      default DefaultVolume;

    { Device to be used when initializing OpenAL context. }
    property Device: string read FDevice write FDevice;

    { If not Enable, ALContextOpen will not initialize any OpenAL device.
      This is useful if you simply want to disable any sound output
      (or OpenAL usage), even when OpenAL library is available. }
    property Enable: boolean read FEnable write FEnable default true;
  end;

var
  SoundEngine: TALSoundEngine;

implementation

uses KambiUtils, KambiStringUtils, ALUtils, KambiLog, ProgressUnit,
  SoundFile, VorbisFile, KambiTimeUtils, EFX, ParseParametersUnit;

type
  { For alcGetError errors (ALC_xxx constants). }
  EALCError = class(EOpenALError)
  private
    FALCErrorNum: TALenum;
  public
    property ALCErrorNum: TALenum read FALCErrorNum;
    constructor Create(AALCErrorNum: TALenum; const AMessage: string);
  end;

constructor EALCError.Create(AALCErrorNum: TALenum; const AMessage: string);
begin
  FALCErrorNum := AALCErrorNum;
  inherited Create(AMessage);
end;

{ TALSoundEngine ------------------------------------------------------------- }

constructor TALSoundEngine.Create;
begin
  inherited;
  FVolume := DefaultVolume;
  FEnable := true;
end;

procedure TALSoundEngine.CheckALC(const situation: string);
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

function TALSoundEngine.GetContextString(Enum: TALCenum): string;
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

procedure TALSoundEngine.ALContextOpen;

  { Try to initialize OpenAL.
    Sets ALActive, EFXSupported.
    If not ALActive, then ALActivationErrorMessage contains error description. }
  procedure BeginAL(out ALActivationErrorMessage: string);
  begin
    { We don't do alcProcessContext/alcSuspendContext, no need
      (spec says that context is initially in processing state). }

    try
      FALActive := false;
      FEFXSupported := false;
      ALActivationErrorMessage := '';

      CheckALInited;

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
    except
      on E: EOpenALError do
        ALActivationErrorMessage := E.Message;
    end;
  end;

var
  ALActivationErrorMessage: string;
begin
  Assert(not ALActive);

  if not Enable then
    FSoundInitializationReport :=
      'Sound disabled by --no-sound command-line option' else
  begin
    BeginAL(ALActivationErrorMessage);
    if not ALActive then
      FSoundInitializationReport :=
        'OpenAL initialization failed : ' +ALActivationErrorMessage +nl+
        'SOUND IS DISABLED' else
    begin
      FSoundInitializationReport :=
        'OpenAL initialized, sound enabled';

      try
        alListenerf(AL_GAIN, Volume);
        inherited; { initialize sound allocator }
        CheckAL('initializing sounds (ALContextOpen)');
      except
        ALContextClose;
        raise;
      end;
    end;
  end;

  if Log then
    WritelnLogMultiline('Sound initialization',
      SoundInitializationReport + nl + ALInformation);
end;

procedure TALSoundEngine.ALContextClose;

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

begin
  if ALActive then
  begin
    inherited; { release sound allocator }

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
      [ AllocatedSources.Count,
        MinAllocatedSources,
        MaxAllocatedSources ]));
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

procedure TALSoundEngine.ALChangeDevice(const NewDevice: string);
begin
  ALContextClose;
  OpenALRestart;
  Device := NewDevice;
  ALContextOpen;
end;

function TALSoundEngine.PlaySound(const ALBuffer: TALBuffer;
  const Spatial, Looping: boolean; const Importance: Cardinal;
  const Gain, MinGain, MaxGain: Single;
  const Position: TVector3Single): TALSound;

  procedure alCommonSourceSetup(ALSource: TALuint);
  begin
    Result.Buffer := ALBuffer;
    Result.Looping := Looping;
    Result.Gain := Gain;
    Result.MinGain := MinGain;
    Result.MaxGain := MaxGain;

    if Spatial then
    begin
      { Set attenuation by distance. }
      alSourcef(ALSource, AL_ROLLOFF_FACTOR, 0.1);
      alSourcef(ALSource, AL_REFERENCE_DISTANCE, 2.0);

      Result.Relative := false;
      Result.Position := Position;
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
      Result.Relative := true;
      Result.Position := ZeroVector3Single;
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
    Result := AllocateSound(Importance);
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

procedure TALSoundEngine.SetVolume(const Value: Single);
begin
  if Value <> FVolume then
  begin
    FVolume := Value;
    if ALActive then
      alListenerf(AL_GAIN, Volume);
  end;
end;

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
var
  Message, DefaultDeviceName: string;
  DeviceList: TStringList;
  i, DefaultDeviceNum: Integer;
  Engine: TALSoundEngine;
begin
  Engine := TALSoundEngine(Data);
  case OptionNum of
    0: Engine.Device := Argument;
    1: begin
         if not ALInited then
           Message := 'OpenAL is not available - cannot print available audio devices' else
         if not EnumerationExtPresent then
           Message := 'Your OpenAL implementation does not support getting the list '+
             'of available audio devices (ALC_ENUMERATION_EXT extension not present ' +
             '(or not implemented correctly in case of Apple version)).' else
         begin
           DefaultDeviceName := alcGetString(nil, ALC_DEFAULT_DEVICE_SPECIFIER);
           DefaultDeviceNum := -1;

           DeviceList := TStringList.Create;
           try
             GetOpenALDevices(DeviceList);

             DefaultDeviceNum := DeviceList.IndexOf(DefaultDeviceName);

             Message := Format('%d available audio devices:', [DeviceList.Count]) + nl;
             for i := 0 to DeviceList.Count-1 do
             begin
               Message += '  ' + DeviceList[i];
               if i = DefaultDeviceNum then Message += ' (default OpenAL device)';
               Message += nl;
             end;

             if DefaultDeviceNum = -1 then
               Message += 'Default OpenAL device name is "' + DefaultDeviceName +
                 '" but this device was not listed as available device. ' +
                 'Bug in OpenAL ?';
           finally DeviceList.Free end;
         end;

         InfoWrite(Message);

         ProgramBreak;
       end;
    2: Engine.Enable := false;
    else raise EInternalError.Create('OpenALOptionProc');
  end;
end;

procedure TALSoundEngine.ParseParameters;
const
  OpenALOptions: array [0..2] of TOption =
  ( (Short: #0; Long: 'audio-device'; Argument: oaRequired),
    (Short: #0; Long: 'print-audio-devices'; Argument: oaNone),
    (Short: #0; Long: 'no-sound'; Argument: oaNone)
  );
begin
  ParseParametersUnit.ParseParameters(OpenALOptions, @OptionProc, Self, true);
end;

function TALSoundEngine.ParseParametersHelp(PrintCurrentDeviceAsDefault: boolean): string;
begin
  Result :=
    '  --audio-device DEVICE-NAME' +nl+
    '                        Choose specific OpenAL audio device';
  if PrintCurrentDeviceAsDefault then
    Result += nl+
      '                        Default audio device for this OS is:' +nl+
      '                        '+ Iff(Device = '', '(OpenAL default device)', Device);
  Result += nl+
    '  --print-audio-devices' +nl+
    '                        Print available audio devices' +nl+
    '                        (not supported by every OpenAL implementation)' +nl+
    '  --no-sound            Turn off sound';
end;

end.
