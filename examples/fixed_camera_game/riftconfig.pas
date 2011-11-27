{
  Copyright 2006-2011 Michalis Kamburelis.

  This file is part of "the rift".

  "the rift" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "the rift" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "the rift"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ User config file.
  Note that this unit initializes OnGetApplicationName in initialization.

  Based on CastleConfig. }
unit RiftConfig;

interface

uses CastleUtils, CastleXMLConfig;

var
  { User config file for this game.
    Will be created (and FileName set) in initialization,
    will be flushed and freed in finalization. }
  UserConfig: TCastleConfig;

implementation

uses SysUtils, CastleFilesUtils, RiftSound;

{ initialization / finalization --------------------------------------------- }

function MyGetApplicationName: string;
begin
  Result := 'rift';
end;

{ Alternative implementation of UserConfigFile, using FPC GetAppConfigDir.
  I think that in the future I'll switch to this for all my programs. }
function UserConfigFile(const Extension: string): string;
var
  ConfigDir: string;
begin
  { calculate ConfigDir }
  ConfigDir := GetAppConfigDir(false);
  if not ForceDirectories(ConfigDir) then
    raise Exception.CreateFmt('Cannot create directory for config file: "%s"',
      [ConfigDir]);

  Result := IncludeTrailingPathDelimiter(ConfigDir) + ApplicationName +
    Extension;
end;

initialization
  { This is needed because
    - I sometimes display ApplicationName for user, and under Windows
      ParamStr(0) is ugly uppercased.
    - ParamStr(0) is unsure for Unixes.
    - ParamStr(0) is useless for upx executables.
    - Also, my alternative UserConfigFile above uses this. }
  OnGetApplicationName := {$ifdef FPC_OBJFPC} @ {$endif} MyGetApplicationName;

  UserConfig := TCastleConfig.Create(nil);
  UserConfig.FileName := UserConfigFile('.conf');
finalization
  if UserConfig <> nil then
  begin
    { Save SoundEngine to config now, otherwise SoundEngine will try to save
      at destruction, in ALSoundEngine unit finalization,
      and then ConfigFile is already nil. }
    SoundEngine.SaveToConfig(UserConfig);
    UserConfig.Flush;
    FreeAndNil(UserConfig);
  end;
end.
