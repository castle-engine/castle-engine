{%MainUnit castlesoundengine.pas}
{
  Copyright 2010-2021 Michalis Kamburelis.

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
  DefaultSoundImportance         = 10;

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

{$endif read_interface}

{$ifdef read_implementation}

var
  FSoundEngine: TRepoSoundEngine;

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

{$endif read_implementation}
