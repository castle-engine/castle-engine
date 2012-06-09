{
  Copyright 2006-2012 Michalis Kamburelis.

  This file is part of "castle".

  "castle" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "castle" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "castle"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ User config file. }
unit CastleGameConfig;

interface

uses CastleUtils, CastleXMLConfig;

var
  { User config file for this game.
    Will be created (and FileName set) in initialization,
    will be flushed and freed in finalization. }
  ConfigFile: TCastleConfig;

implementation

uses SysUtils, CastleFilesUtils, ALSoundEngine;

{ initialization / finalization --------------------------------------------- }

initialization
  ConfigFile := TCastleConfig.Create(nil);
  ConfigFile.FileName := UserConfigFile('.conf');
finalization
  if ConfigFile <> nil then
  begin
    { Save SoundEngine to config now, otherwise SoundEngine will try to save
      at destruction, in GameSound or RiftSound unit finalization,
      but then ConfigFile is already nil. }
    SoundEngine.SaveToConfig(ConfigFile);
    ConfigFile.Flush;
    FreeAndNil(ConfigFile);
  end;
end.