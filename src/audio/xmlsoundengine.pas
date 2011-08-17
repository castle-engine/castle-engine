{
  Copyright 2006-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Sound engine loading sounds from XML file (TXmlSoundEngine). }
unit XmlSoundEngine;

{$I kambiconf.inc}

interface

uses Classes, VectorMath, SysUtils,
  KambiUtils, KambiXMLConfig, ALSoundEngine, ALSoundAllocator,
  FGL {$ifdef VER2_2}, FGLObjectList22 {$endif};

const
  MaxSoundImportance = MaxInt;

const
  DefaultXmlEngineVolume = 0.5;
  DefaultMusicVolume = 1.0;

type
  { This is a unique sound type identifier for sounds used within
    TXmlSoundEngine.

    This is actually just an index to appropriate
    TXmlSoundEngine.SoundNames array, but you should always treat
    this as an opaque type. }
  TSoundType = Cardinal;

const
  { Special sound type that indicates that there is actually none sound.
    @link(TXmlSoundEngine.Sound) and @link(TXmlSoundEngine.Sound3d)
    will do nothing when called with this sound type. }
  stNone = 0;

type
  { Sound information, internally used by TXmlSoundEngine.

    Although you still may want to familiarize with it's fields,
    as they correspond to appropriate fields in your sounds/index.xml file.
    All of the fields besides Buffer are initialized only by ReadSoundInfos.

    From the point of view of end-user the number of sounds
    is constant for given game and their properties (expressed in
    TSoundInfo below) are also constant.
    However, for the sake of debugging/testing the game,
    and for content designers, the actual values of SoundInfos are loaded
    at initialization by ReadSoundInfos (called automatically by ALContextOpen)
    from sounds/index.xml file,
    and later can be changed by calling ReadSoundInfos once again during the
    game (debug menu may have command like "Reload sounds/index.xml"). }
  TSoundInfo = class
    { '' means that this sound is not implemented and will never
      have any OpenAL buffer associated with it. }
    FileName: string;

    { XxxGain are mapped directly on respective OpenAL source properties.
      Note that Gain > 1 is allowed (because OpenAL allows it),
      although OpenAL may clip them for the resulting sound (after all
      calculations taking into account 3d position will be done).
      MaxGain is not allowed to be > 1 (under Windows impl, Linux impl
      allows it, but that's about it).

      When sound is used for MusicPlayer.PlayedSound:
      1. MinGain, MaxGain are ignored
      2. Gain is always multiplied by MusicVolume when setting AL_GAIN
         of the music source. }
    Gain, MinGain, MaxGain: Single;

    { Importance, as passed to TALSoundAllocator.
      This is ignored when sound is used for MusicPlayer.PlayedSound. }
    DefaultImportance: Cardinal;

    { OpenAL buffer of this sound. Zero if buffer is not yet loaded,
      which may happen only if TXmlSoundEngine.ALContextOpen was not yet
      called or when sound has FileName = ''. }
    Buffer: TALbuffer;
  end;

  TSoundInfoList = specialize TFPGObjectList<TSoundInfo>;

type
  TMusicPlayer = class;

  { Sound engine that loads it's sound data from a comfortable XML file.
    It extends TALSoundEngine, so you can always load new buffers
    and play them by TALSoundEngine.LoadBuffer, TALSoundEngine.PlaySound
    and all other methods. This only adds easy preloaded sounds.

    At ALContextOpen, right before initializing OpenAL stuff,
    this reads sounds information from SoundsXmlFileName file.
    When OpenAL is initialized, it loads all the sound files.
    Sound filenames are specified inside SoundsXmlFileName file
    (they may be relative filenames, relative to the location
    of SoundsXmlFileName file). }
  TXmlSoundEngine = class(TALSoundEngine)
  private
    FSoundImportanceNames: TStringList;
    FSoundNames: TStringList;
    FSoundsXmlFileName: string;

    SoundInfos: TSoundInfoList;

    { This is the only allowed instance of TMusicPlayer class,
      created and destroyed in this class create/destroy. }
    FMusicPlayer: TMusicPlayer;
  public
    constructor Create;
    destructor Destroy; override;

    { In addition to initializing OpenAL context, this also loads sound files. }
    procedure ALContextOpen; override;
    procedure ALContextClose; override;

    { The XML file that contains description of your sounds.
      See engine examples, @code(examples/audio/sample_sounds.xml) file,
      for a heavily commented example.

      It's crucial that you create such file, and eventually adjust
      this property before calling ReadSoundInfos (or ALContextOpen,
      that always calls ReadSoundInfos).

      By default (in our constryctor) this is initialized to
      @code(ProgramDataPath + 'data' +
        PathDelim + 'sounds' + PathDelim + 'index.xml')
      which may be good location for most programs. }
    property SoundsXmlFileName: string
      read FSoundsXmlFileName write FSoundsXmlFileName;

    { A list of sound names used by your program.
      Each sound has a unique name, used to identify sound in
      sounds/index.xml file and for SoundFromName function.
      These names are stored here.

      At the beginning, this list always contains exactly one item: empty string.
      This is a special "sound type" that has index 0 (should be always
      expressed as TSoundType value stNone) and name ''.
      stNone is a special sound as it actually means "no sound" in many cases.

      You can (and should !) fill this array with all the sound names
      your game is using @bold(before calling ALContextOpen)
      (or ReadSoundInfos, but ReadSoundInfos is usually called
      for the first time by ALContextOpen).

      TODO: in the future this may be automatically filled when
      ReadSoundInfos is called. For now, ReadSoundInfos just
      read sounds information for all sounds mentioned here --- in the future,
      ReadSoundInfos may also just fill this list. }
    property SoundNames: TStringList read FSoundNames;

    { Return sound with given name.
      Available names are given in SoundNames,
      and inside ../data/sounds/index.xml.
      Always for SoundName = '' it will return stNone.
      @raises Exception On invalid SoundName }
    function SoundFromName(const SoundName: string): TSoundType;

    { Play given sound. This should be used to play sounds
      that are not spatial, i.e. have no place in 3D space.

      Returns used TALSound (or nil if none was available).
      You don't have to do anything with this returned TALSound. }
    function Sound(SoundType: TSoundType;
      const Looping: boolean = false): TALSound;

    { Play given sound at appropriate position in 3D space.

      Returns used TALSound (or nil if none was available).
      You don't have to do anything with this returned TALSound.

      @noAutoLinkHere }
    function Sound3d(SoundType: TSoundType;
      const Position: TVector3Single;
      const Looping: boolean = false): TALSound; overload;

    property Volume default DefaultXmlEngineVolume;

    { Sound importance names and values.
      Each item is a name (as a string) and a value (that is stored in Objects
      property of the item as a pointer; add new importances by
      AddSoundImportanceName for comfort).

      These can be used within sounds.xml file.
      Before using ALContextOpen, you can fill this list with values.
      Initially, it contains only the 'max' value associated with
      MaxSoundImportance. }
    property SoundImportanceNames: TStringList read FSoundImportanceNames;

    procedure AddSoundImportanceName(const Name: string; Importance: Integer);

    procedure ReadSoundInfos;

    property MusicPlayer: TMusicPlayer read FMusicPlayer;

    procedure LoadFromConfig(ConfigFile: TKamXMLConfig); override;
    procedure SaveToConfig(ConfigFile: TKamXMLConfig); override;
  end;

  { Music player. Instance of this class should be created only internally
    by the TXmlSoundEngine. }
  TMusicPlayer = class
  private
    { Engine that owns this music player. }
    FEngine: TXmlSoundEngine;

    FPlayedSound: TSoundType;
    procedure SetPlayedSound(const Value: TSoundType);
  private
    { This is nil if we don't play music right now
      (because OpenAL is not initialized, or PlayedSound = stNone,
      or PlayerSound.FileName = '' (sound not existing)). }
    FAllocatedSource: TALSound;

    procedure AllocatedSourceUsingEnd(Sender: TALSound);

    { Called by ALContextOpen. You should check here if
      PlayedSound <> stNone and eventually initialize FAllocatedSource. }
    procedure AllocateSource;
  private
    FMusicVolume: Single;
    function GetMusicVolume: Single;
    procedure SetMusicVolume(const Value: Single);
  public
    constructor Create(AnEngine: TXmlSoundEngine);
    destructor Destroy; override;

    { Currently played music.
      Set to stNone to stop playing music.
      Set to anything else to play that music.

      Changing value of this property (when both the old and new values
      are <> stNone and are different) restarts playing the music. }
    property PlayedSound: TSoundType read FPlayedSound write SetPlayedSound
      default stNone;

    { Music volume. This must always be within 0..1 range.
      0.0 means that there is no music (this case should be optimized).}
    property MusicVolume: Single read GetMusicVolume write SetMusicVolume
      default DefaultMusicVolume;
  end;

implementation

uses ProgressUnit, KambiFilesUtils, DOM, XMLRead, KambiXMLUtils,
  VorbisFile, KambiStringUtils;

{ TXmlSoundEngine ----------------------------------------------------------- }

constructor TXmlSoundEngine.Create;
begin
  inherited;

  { Sound importance names and sound names are case-sensitive because
    XML traditionally is. Maybe in the future I'll relax this,
    there's no definite reason actually to not let them ignore case. }

  FSoundImportanceNames := TStringList.Create;
  FSoundImportanceNames.CaseSensitive := true;
  AddSoundImportanceName('max', MaxSoundImportance);

  Volume := DefaultXmlEngineVolume;

  FSoundNames := TStringList.Create;
  FSoundNames.CaseSensitive := true;
  FSoundNames.Append(''); { stNone entry }

  SoundInfos := TSoundInfoList.Create;

  FMusicPlayer := TMusicPlayer.Create(Self);

  FSoundsXmlFileName := ProgramDataPath + 'data' +
    PathDelim + 'sounds' + PathDelim + 'index.xml';
end;

destructor TXmlSoundEngine.Destroy;
begin
  FreeAndNil(FSoundImportanceNames);
  FreeAndNil(FSoundNames);
  FreeAndNil(SoundInfos);
  FreeAndNil(FMusicPlayer);
  inherited;
end;

procedure TXmlSoundEngine.ALContextOpen;
var
  ST: TSoundType;
begin
  inherited;

  { initialize SoundInfos regardless of ALActive }
  ReadSoundInfos;

  if ALActive then
  begin
    Progress.Init(SoundInfos.Count - 1, 'Loading sounds');
    try
      { We do progress to "SoundInfos.Count - 1" because we start
        iterating from ST = 1 because ST = 0 = stNone never exists. }
      Assert(SoundInfos[stNone].FileName = '');
      for ST := 1 to SoundInfos.Count - 1 do
      begin
        if SoundInfos[ST].FileName <> '' then
          SoundInfos[ST].Buffer := LoadBuffer(SoundInfos[ST].FileName);
        Progress.Step;
      end;
    finally Progress.Fini; end;

    MusicPlayer.AllocateSource;
  end;
end;

procedure TXmlSoundEngine.ALContextClose;
var
  ST: TSoundType;
begin
  if ALActive then
  begin
    StopAllSources;
    for ST := 0 to SoundInfos.Count - 1 do
      FreeBuffer(SoundInfos[ST].Buffer);
  end;
  inherited;
end;

function TXmlSoundEngine.Sound(SoundType: TSoundType;
  const Looping: boolean): TALSound;
begin
  Result := PlaySound(
    SoundInfos[SoundType].Buffer, false, Looping,
    SoundInfos[SoundType].DefaultImportance,
    SoundInfos[SoundType].Gain,
    SoundInfos[SoundType].MinGain,
    SoundInfos[SoundType].MaxGain,
    ZeroVector3Single);
end;

function TXmlSoundEngine.Sound3d(SoundType: TSoundType;
  const Position: TVector3Single;
  const Looping: boolean): TALSound;
begin
  Result := PlaySound(
    SoundInfos[SoundType].Buffer, true, Looping,
    SoundInfos[SoundType].DefaultImportance,
    SoundInfos[SoundType].Gain,
    SoundInfos[SoundType].MinGain,
    SoundInfos[SoundType].MaxGain,
    Position);
end;

procedure TXmlSoundEngine.ReadSoundInfos;
var
  ST: TSoundType;
  SoundConfig: TXMLDocument;
  SoundNode: TDOMNode;
  SoundElement: TDOMElement;
  SoundElements: TDOMNodeList;
  S: string;
  I, SoundImportanceIndex: Integer;
  SoundsXmlPath: string;
begin
  { This must be an absolute path, since SoundInfos[].FileName should be
    absolute (to not depend on the current dir when loading sound files. }
  SoundsXmlPath := ExtractFilePath(ExpandFileName(SoundsXmlFileName));

  try
    { ReadXMLFile always sets TXMLDocument param (possibly to nil),
      even in case of exception. So place it inside try..finally. }
    ReadXMLFile(SoundConfig, SoundsXmlFileName);

    Check(SoundConfig.DocumentElement.TagName = 'sounds',
      'Root node of sounds/index.xml must be <sounds>');

    { Init all SoundInfos to default values }
    SoundInfos.Count := SoundNames.Count;
    { stNone has specific info: FileName is '', Buffer is 0, rest doesn't matter }
    SoundInfos[stNone] := TSoundInfo.Create;
    SoundInfos[stNone].FileName := '';
    SoundInfos[stNone].Buffer := 0;
    { initialize other than stNone sounds }
    for ST := 1 to SoundInfos.Count - 1 do
    begin
      SoundInfos[ST] := TSoundInfo.Create;
      SoundInfos[ST].FileName :=
        CombinePaths(SoundsXmlPath, SoundNames[ST] + '.wav');
      SoundInfos[ST].Gain := 1;
      SoundInfos[ST].MinGain := 0;
      SoundInfos[ST].MaxGain := 1;
      SoundInfos[ST].DefaultImportance := MaxSoundImportance;
      SoundInfos[ST].Buffer := 0; {< initially, will be loaded later }
    end;

    SoundElements := SoundConfig.DocumentElement.ChildNodes;
    try
      for I := 0 to SoundElements.Count - 1 do
      begin
        SoundNode := SoundElements.Item[I];
        if SoundNode.NodeType = ELEMENT_NODE then
        begin
          SoundElement := SoundNode as TDOMElement;
          Check(SoundElement.TagName = 'sound',
            'Each child of sounds/index.xml root node must be the <sound> element');

          ST := SoundFromName(SoundElement.GetAttribute('name'));

          { I retrieve FileNameNode using DOMGetAttribute
            (that internally uses SoundElement.Attributes.GetNamedItem),
            because I have to distinguish between the case when file_name
            attribute is not present (in this case FileName is left as it was)
            and when it's present as set to empty string.
            Standard SoundElement.GetAttribute wouldn't allow me this. }
          if DOMGetAttribute(SoundElement, 'file_name',
            SoundInfos[ST].FileName) and
            (SoundInfos[ST].FileName <> '') then
            { Make FileName absolute, using SoundsXmlPath, if non-empty FileName
              was specified in XML file. }
            SoundInfos[ST].FileName := CombinePaths(
              SoundsXmlPath,
              SoundInfos[ST].FileName);

          DOMGetSingleAttribute(SoundElement, 'gain', SoundInfos[ST].Gain);
          DOMGetSingleAttribute(SoundElement, 'min_gain', SoundInfos[ST].MinGain);
          DOMGetSingleAttribute(SoundElement, 'max_gain', SoundInfos[ST].MaxGain);

          { MaxGain is max 1. Although some OpenAL implementations allow > 1,
            Windows impl (from Creative) doesn't. For consistent results,
            we don't allow it anywhere. }
          if SoundInfos[ST].MaxGain > 1 then
            SoundInfos[ST].MaxGain := 1;

          if DOMGetAttribute(SoundElement, 'default_importance', S) then
          begin
            SoundImportanceIndex := SoundImportanceNames.IndexOf(S);
            if SoundImportanceIndex = -1 then
              SoundInfos[ST].DefaultImportance := StrToInt(S) else
              SoundInfos[ST].DefaultImportance :=
                PtrUInt(SoundImportanceNames.Objects[SoundImportanceIndex]);
          end;
        end;
      end;
    finally FreeChildNodes(SoundElements); end;
  finally
    FreeAndNil(SoundConfig);
  end;
end;

function TXmlSoundEngine.SoundFromName(const SoundName: string): TSoundType;
var
  Index: Integer;
begin
  Index := SoundNames.IndexOf(SoundName);
  if Index = -1 then
    raise Exception.CreateFmt('Unknown sound name "%s"', [SoundName]) else
    Result := Index;
end;

procedure TXmlSoundEngine.AddSoundImportanceName(const Name: string;
  Importance: Integer);
begin
  FSoundImportanceNames.AddObject(Name, TObject(Pointer(PtrUInt(Importance))));
end;

procedure TXmlSoundEngine.LoadFromConfig(ConfigFile: TKamXMLConfig);
begin
  inherited;
  Volume := ConfigFile.GetFloat('sound/volume', DefaultXmlEngineVolume);
  MusicPlayer.MusicVolume := ConfigFile.GetFloat('sound/music/volume',
    DefaultMusicVolume);
end;

procedure TXmlSoundEngine.SaveToConfig(ConfigFile: TKamXMLConfig);
begin
  inherited;
  ConfigFile.SetDeleteFloat('sound/volume', Volume, DefaultXmlEngineVolume);
  { This may be called from destructors and the like, so better check
    that MusicPlayer is not nil. }
  if MusicPlayer <> nil then
    ConfigFile.SetDeleteFloat('sound/music/volume',
      MusicPlayer.MusicVolume, DefaultMusicVolume);
end;

{ TMusicPlayer --------------------------------------------------------------- }

constructor TMusicPlayer.Create(AnEngine: TXmlSoundEngine);
begin
  inherited Create;
  FMusicVolume := DefaultMusicVolume;
  FEngine := AnEngine;
end;

destructor TMusicPlayer.Destroy;
begin
  if FAllocatedSource <> nil then
    FAllocatedSource.DoUsingEnd;
  inherited;
end;

procedure TMusicPlayer.AllocateSource;
begin
  FAllocatedSource := FEngine.PlaySound(
    FEngine.SoundInfos[PlayedSound].Buffer, false, true,
    MaxSoundImportance,
    MusicVolume * FEngine.SoundInfos[PlayedSound].Gain, 0, 1,
    ZeroVector3Single);

  if FAllocatedSource <> nil then
    FAllocatedSource.OnUsingEnd :=
      {$ifdef FPC_OBJFPC} @ {$endif} AllocatedSourceUsingEnd;
end;

procedure TMusicPlayer.SetPlayedSound(const Value: TSoundType);
begin
  if Value <> FPlayedSound then
  begin
    if FAllocatedSource <> nil then
    begin
      FAllocatedSource.DoUsingEnd;
      { AllocatedSourceUsingEnd should set FAllocatedSource to nil. }
      Assert(FAllocatedSource = nil);
    end;

    FPlayedSound := Value;

    AllocateSource;
  end;
end;

procedure TMusicPlayer.AllocatedSourceUsingEnd(Sender: TALSound);
begin
  Assert(Sender = FAllocatedSource);
  FAllocatedSource.OnUsingEnd := nil;
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
      FAllocatedSource.Gain := MusicVolume * FEngine.SoundInfos[PlayedSound].Gain;
  end;
end;

end.
