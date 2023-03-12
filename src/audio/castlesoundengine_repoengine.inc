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

  { Unique sound type identifier for sounds used within TRepoSoundEngine. }
  TSoundType = TCastleSound deprecated 'use TCastleSound';

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
        { Unique sound name (including parent group names). Empty for the special sound stNone = nil. }
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
      TSoundInfoList = class({$ifdef FPC}specialize{$endif} TObjectList<TSoundInfo>)
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

      { Sound that can be played, with a buffer. }
      TSoundInfoBuffer = class(TSoundInfo)
      public
        Sound: TCastleSound;
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

      TSoundGroupList = class({$ifdef FPC}specialize{$endif} TObjectList<TSoundGroup>)
      public
        { Index of group with given TSoundGroup.Name, or -1 if not found. }
        function IndexOfName(const GroupName: String): Integer;
      end;

      TLoopingChannelList = {$ifdef FPC}specialize{$endif} TObjectList<TLoopingChannel>;

    var
      FSoundImportanceNames: TStringList;
      { A list of sounds used by your program.
        Each sound has a unique name, used to identify sound in
        the XML file and for SoundFromName function.

        At the beginning, this list always contains exactly one sound, empty.
        This is a special TSoundInfoBuffer that has Sound=nil and Name=''.
        It means "no sound" in many cases. }
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
    procedure ContextOpenCore; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromConfig(const Config: TCastleConfig); override; deprecated 'load sound properties explicitly from UserConfig';
    procedure SaveToConfig(const Config: TCastleConfig); override; deprecated 'save sound properties explicitly to UserConfig';

    { The XML file that contains description of your sounds.
      This should be an URL (in simple cases, just a filename)
      pointing to an XML file describing your sounds.
      See https://castle-engine.io/creating_data_sound.php#section_sample .

      When you set RepositoryURL property, we read sound information from
      given XML file. You usually set RepositoryURL at the very beginning
      of the application.
      Right after setting RepositoryURL you usually call SoundFromName
      a couple of times to convert some names into TCastleSound values,
      to later use these TCastleSound values with @link(Sound) and @link(Sound3D)
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
        MySound1 := SoundEngine.SoundFromName('my_sound_1');
        MySound2 := SoundEngine.SoundFromName('my_sound_2');
        // ... and later in your game you can do stuff like this:
        SoundEngine.Play(MySound1);
      #)

      See https://castle-engine.io/manual_data_directory.php
      for information about the castle-data:/ protocol. In short, on desktop,
      this just indicates the "data" subdirectory of your project.
    }
    property RepositoryURL: string read FRepositoryURL write SetRepositoryURL;
      {$ifdef FPC}deprecated 'instead of XML file, create design with .castle-component that contains a number of TCastleSound components';{$endif}

    {$ifdef FPC}
    { Deprecated name for RepositoryURL. @deprecated }
    property SoundsFileName: string read FRepositoryURL write SetRepositoryURL; deprecated 'use RepositoryURL';
    {$endif}

    { Reload the RepositoryURL and all referenced buffers.
      Useful as a tool for game designers, to reload the sounds XML file
      without restarting the game and sound engine. }
    procedure ReloadSounds; deprecated 'deprecated just like RepositoryURL';

    { Return sound with given name.
      Available names are given in SoundNames, defined in XML file pointed
      by RepositoryURL.
      Always for SoundName = '' it will return nil.

      @param(Required

        If Required = @true, it will make a warning when the sound name
        is not found. This may mean that sound is missing in your sounds.xml
        file (so you should correct your sounds.xml),
        or that you didn't load the sounds.xml file yet
        (so you should correct your code to set @link(TRepoSoundEngine.RepositoryURL)
        early enough), or that you specified invalid sound name.
        When Required = @false, missing sound is silently ignored,
        which is sensible if it was optional.

        Regardless of the Required value, we return nil for missing sound.
        So the Required parameter only determines whether we make a warning,
        or not.)
    }
    function SoundFromName(const SoundName: string; const Required: boolean = true): TCastleSound;
      deprecated 'deprecated just like RepositoryURL';

    {$ifdef FPC}
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
        @item 'default' - DefaultSoundImportance
      ) }
    property SoundImportanceNames: TStringList read FSoundImportanceNames;
      deprecated 'use Priority, with numeric values';

    procedure AddSoundImportanceName(const Name: string; Importance: Integer);
      deprecated 'use Priority, with numeric values';

    { Comfortable way to play and control the music.
      Simply assign @link(TLoopingChannel.Sound MusicPlayer.Sound)
      to play music. Set it to @nil to stop playing music.
      This is just a shortcut for @link(LoopingChannel LoopingChannel[0]). }
    property MusicPlayer: TLoopingChannel read GetMusicPlayer; deprecated 'use LoopingChannel[0]';
    {$endif FPC}

    { Comfortable way to play and control looping non-spatial sounds, like music tracks, ambience.

      The TLoopingChannel instance automatically remembers the sound it plays.
      You start playing just by setting @link(TLoopingChannel.Sound)
      to some sound. For example:

      @longCode(# LoopingChannel[0].Sound := MyMusicTrack; #)

      You stop by setting @link(TLoopingChannel.Sound) to something else,
      which can be @nil to just stop playing any looping
      sound on this channel. For example:

      @longCode(# LoopingChannel[0].Sound := nil; #)

      Each channel has it's own @link(TLoopingChannel.Volume)
      that can be changed at any point.

      All the looping channels play simultaneously.

      Note: Alternative way to play looping non-spatial sounds
      is to use @link(Play) with @link(TCastlePlayingSound) (setting
      @link(TCastlePlayingSound.Loop) to @true). }
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
      or do @link(LoadBuffer) or @link(Play), then sound context will
      be created on-demand anyway. So calling this is always optional. }
    procedure PrepareResources; deprecated 'deprecated just like RepositoryURL';
  end;

{$endif read_interface}

{$ifdef read_implementation}

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

  if Target.Count > 1 then
    WritelnWarning('Choosing sound at random is no longer supported, alias "%s" is no longer a random sound', [
      Name
    ]);
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
  URL, ImportanceStr, URLPrefix: String;
  SoundImportanceIndex: Integer;
begin
  inherited;

  FreeAndNil(Sound);
  Sound := TCastleSound.Create(nil);

  { retrieve URL using AttributeString
    (that internally uses Element.Attributes.GetNamedItem),
    because we have to distinguish between the case when url/file_name
    attribute is not present (in this case URL is left as it was)
    and when it's present and set to empty string
    (in this case URL must also be set to empty string).
    Standard Element.GetAttribute wouldn't allow me this. }
  URL := '';
  if (Element.AttributeString('url', URL) or
      Element.AttributeString('file_name', URL)) and
     (URL <> '') then
  begin
    URL := CombineURI(BaseUrl, URL);
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

  if Element.HasAttribute('gain') then
  begin
    Sound.Volume := Element.AttributeSingleDef('gain', 1);
    WritelnWarning('Sound "%s": "gain" has been deprecated in sounds XML, use "volume" now', [Name]);
  end else
    Sound.Volume := Element.AttributeSingleDef('volume', 1);

  {$ifdef FPC}
  {$warnings off} // using unportable symbols knowingly
  Sound.MinGain := Element.AttributeSingleDef('min_gain', 0);
  Sound.MaxGain := Element.AttributeSingleDef('max_gain', 1);

  { MaxGain is max 1. Although some sound backends (like some OpenAL backends)
    implementations allow > 1,
    Windows implementation of OpenAL (from Creative) doesn't.
    For consistent results, we don't allow it anywhere. }
  if Sound.MaxGain > 1 then
  begin
    WritelnWarning('Sound "%s": "max_gain" max value is 1.0', [Name]);
    Sound.MaxGain := 1;
  end;
  {$warnings on}
  {$endif}

  if Element.AttributeString('default_importance', ImportanceStr) then
  begin
    WritelnWarning('Sound "%s": Use "priority" (float, in 0..1 range) instead of "importance" in sounds XML file', [
      Name
    ]);

    {$ifdef FPC}
    {$warnings off} // using deprecated, to keep it working
    SoundImportanceIndex := ASoundEngine.SoundImportanceNames.IndexOf(ImportanceStr);
    if SoundImportanceIndex = -1 then
      Sound.Priority := ImportanceToPriority(StrToInt(ImportanceStr))
    else
      Sound.Priority := ImportanceToPriority(
        PtrUInt(ASoundEngine.SoundImportanceNames.Objects[SoundImportanceIndex]));
    {$warnings on}
    {$endif}

  end else
    Sound.Priority := Element.AttributeSingleDef('priority', 0.5);

  Sound.Stream := Element.AttributeBooleanDef('stream', false);

  { set URL at the end, to avoid reloading when Sound.Stream changes }
  Sound.URL := URL;
end;

function TRepoSoundEngine.TSoundInfoBuffer.FinalSound(const RecursionDepth: Cardinal): TSoundInfoBuffer;
begin
  Result := Self;
end;

destructor TRepoSoundEngine.TSoundInfoBuffer.Destroy;
begin
  FreeAndNil(Sound);
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

  {$ifdef FPC}
  {$warnings off} // using deprecated to keep it working
  AddSoundImportanceName('max', MaxSoundImportance);
  AddSoundImportanceName('level_event', LevelEventSoundImportance);
  AddSoundImportanceName('player', PlayerSoundImportance);
  AddSoundImportanceName('default_creature', DefaultCreatureSoundImportance);
  AddSoundImportanceName('minor_non_spatial', MinorNonSpatialSoundImportance);
  AddSoundImportanceName('default', DefaultSoundImportance);
  {$warnings on}
  {$endif}

  FSoundGroups := TSoundGroupList.Create;

  FSounds := TSoundInfoList.Create;
  { add stNone = nil sound }
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
      if (L <> nil) and (L.FAllocatedSource = nil) then
        L.AllocateSource;
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
  { add stNone = nil sound }
  FSounds.Add(TSoundInfoBuffer.Create);

  { if no sounds XML file, then that's it --- no more sounds }
  if FRepositoryURL = '' then Exit;

  TimeStart := Profiler.Start('Loading All Sounds From ' + FRepositoryURL + ' (TRepoSoundEngine)');
  try
    { This must be an absolute path, since FSounds[].URL should be
      absolute (to not depend on the current dir when loading sound files. }
    BaseUrl := AbsoluteURI(FRepositoryURL);

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
    {$warnings off} // initialize deprecated symbols knowingly
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
    {$warnings on}

    { in case you set RepositoryURL when sound context is already
      initialized, start playing music immediately if necessary }
    RestartLoopingChannels;

  finally Profiler.Stop(TimeStart) end;
end;

procedure TRepoSoundEngine.ReloadSounds;
var
  OldRepositoryURL: string;
begin
  {$warnings off} // using deprecated in deprecated
  if RepositoryURL <> '' then
  begin
    OldRepositoryURL := RepositoryURL;
    RepositoryURL := '';
    RepositoryURL := OldRepositoryURL;
  end;
  {$warnings on}
end;

function TRepoSoundEngine.SoundFromName(const SoundName: string;
  const Required: boolean): TCastleSound;
var
  SoundIndex: Integer;
begin
  SoundIndex := FSounds.IndexOfName(SoundName);
  if SoundIndex <> -1 then
  begin
    Result := FSounds[SoundIndex].FinalSound(0).Sound;
  end else
  begin
    Result := nil;
    if Required then
      WritelnWarning('Sound', Format('Unknown sound name "%s"', [SoundName]));
  end;
end;

{$ifdef FPC}
procedure TRepoSoundEngine.AddSoundImportanceName(const Name: string;
  Importance: Integer);
begin
  FSoundImportanceNames.AddObject(Name, TObject(Pointer(PtrUInt(Importance))));
end;
{$endif}

procedure TRepoSoundEngine.PrepareResources;
begin
  if not IsContextOpen and (FSounds.Count > 1) then
    ContextOpen;
end;

procedure TRepoSoundEngine.LoadFromConfig(const Config: TCastleConfig);
begin
  {$warnings off} // using deprecated from deprecated
  inherited;
  {$warnings on}

  Volume := Config.GetFloat('sound/volume', DefaultVolume);
  {$ifdef FPC}
  {$warnings off} // using deprecated from deprecated
  MusicPlayer.Volume := Config.GetFloat('sound/music/volume',
    TLoopingChannel.DefaultVolume);
  {$warnings on}
  {$endif}
end;

procedure TRepoSoundEngine.SaveToConfig(const Config: TCastleConfig);
begin
  Config.SetDeleteFloat('sound/volume', Volume, DefaultVolume);
  {$ifdef FPC}
  {$warnings off} // using deprecated from deprecated
  { This may be called from destructors and the like, so better check
    that MusicPlayer is not nil. }
  if FLoopingChannels <> nil then
    Config.SetDeleteFloat('sound/music/volume',
      MusicPlayer.Volume, TLoopingChannel.DefaultVolume);
  {$warnings on}
  {$endif}

  {$warnings off} // using deprecated from deprecated
  inherited;
  {$warnings on}
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

{$endif read_implementation}
