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

type
  ENoMoreSources = CastleSoundBase.ENoMoreSources;
  ENoMoreOpenALSources = ENoMoreSources deprecated 'use ENoMoreSources';
  ESoundBufferNotLoaded = class(Exception);
  EInvalidSoundBufferFree = class(Exception);
  ESoundFileError = CastleSoundBase.ESoundFileError;
  EInvalidSoundRepositoryXml = class(Exception);

var
  { Common sounds.

    The sounds types listed below are automatically
    initialized when you set TRepoSoundEngine.RepositoryURL.
    All engine units can use them if you define them in your sounds XML file.
    If they are not defined in your XML file (or if you don't even have
    an XML file, that is you leave TRepoSoundEngine.RepositoryURL empty)
    then they remain nil (and nothing will happen if anything will try
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
    :TCastleSound deprecated 'these predefined sounds should not be used';

const
  dmInverse = CastleSoundBase.dmInverse;
  dmLinear  = CastleSoundBase.dmLinear;

  { Supported sound file formats.
    Use these filters with LCL file dialog (easily set by FileFiltersToDialog)
    or TCastleWindow.FileDialog. }
  LoadSound_FileFilters =
  'All Files|*|' +
  '*All Sound Files|*.wav;*.ogg|' +
  'WAV (*.wav)|*.wav|' +
  'OggVorbis (*.ogg)|*.ogg';

{ The sound engine. Singleton instance of TRepoSoundEngine, the most capable
  engine class. Created on first call to this function. }
function SoundEngine: TRepoSoundEngine;

{ The sound engine, but may be @nil very early in initialization / very late in finalization.
  @exclude }
function InternalSoundEngine: TRepoSoundEngine;

{$endif read_interface}

{$ifdef read_implementation}

var
  FSoundEngine: TRepoSoundEngine;

{ Internal constants, only to support deprected "importance" things in XML files. }
const
  MaxSoundImportance             = MaxInt;
  LevelEventSoundImportance      = 100000;
  PlayerSoundImportance          = 10000 ;
  DefaultCreatureSoundImportance = 1000  ;
  MinorNonSpatialSoundImportance = 100   ;
  DefaultSoundImportance         = 10    ;

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

function InternalSoundEngine: TRepoSoundEngine;
begin
  Result := FSoundEngine;
end;

function ImportanceToPriority(const Importance: Cardinal): Single;
begin
  { Deprecated importance is translated to priority using the equation

      priority = sqrt(importance) / sqrt(100 * 1000) ~= sqrt(importance) / 316

    Note: 100 * 1000 = LevelEventSoundImportance. So we practically assume
    it is the maximum.

    Thus
    - importance = 100 * 1000 is equivalent to priority = 1.0 (max).
    - importance = 10 * 1000 is equivalent to priority = 0.31.
    - importance = 10 (DefaultSoundImportance) is equivalent to priority = 0.01.

    This maps the expected importance values into sensible range.
    And it means that DefaultSoundImportance will not get rounded to zero
    even with backends like FMOD or Wwise that put limits on priority granularity
    (FMOD has only 257 levels, Wwise has only 100 levels). }

  {$warnings off} // using deprecated LevelEventSoundImportance
  Result := Sqrt(Importance) / Sqrt(LevelEventSoundImportance);
  {$warnings on}
end;

function PriorityToImportance(const Priority: Single): Cardinal;
begin
  // just invert ImportanceToPriority
  {$warnings off} // using deprecated LevelEventSoundImportance
  Result := Round(Sqr(Priority * Sqrt(LevelEventSoundImportance)));
  {$warnings on}
end;

{$endif read_implementation}
