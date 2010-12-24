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

uses Classes, KambiOpenAL, ALSourceAllocator;

const
  DefaultALMinAllocatedSources = 4;
  DefaultALMaxAllocatedSources = 16;

type
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

    { When SourceAllocator <> nil, these correspond to it's properties. }
    FALMinAllocatedSources: Cardinal;
    FALMaxAllocatedSources: Cardinal;

    function GetALMinAllocatedSources: Cardinal;
    procedure SetALMinAllocatedSources(const Value: Cardinal);

    function GetALMaxAllocatedSources: Cardinal;
    procedure SetALMaxAllocatedSources(const Value: Cardinal);
  protected
    SourceAllocator: TALSourceAllocator;
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
  end;

var
  SoundEngine: TALSoundEngine;
  WasParam_NoSound: boolean = false;

implementation

uses SysUtils, KambiUtils, KambiStringUtils, ALUtils, KambiLog, ProgressUnit,
  SoundFile, VorbisFile;

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

    SourceAllocator := TALSourceAllocator.Create(
      FALMinAllocatedSources, FALMaxAllocatedSources);
    CheckAL('initializing sounds (ALContextOpen)');
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

end.
