{
  Copyright 2007-2018 Michalis Kamburelis.

  This file is part of "The Rift".

  "The Rift" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "The Rift" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "The Rift"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ Sound. }
unit GameSound;

interface

uses CastleSoundEngine;

var
  stIntroMusic,
  stMainMenuMusic
  : TSoundType;

procedure InitializeSound;

implementation

uses SysUtils, CastleFilesUtils;

procedure InitializeSound;
begin
  SoundEngine.RepositoryURL := ApplicationData('sounds/index.xml');
  stIntroMusic    := SoundEngine.SoundFromName('intro_music');
  stMainMenuMusic := SoundEngine.SoundFromName('main_menu_music');
end;

end.
