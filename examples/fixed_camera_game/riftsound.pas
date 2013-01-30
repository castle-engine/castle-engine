{
  Copyright 2007-2013 Michalis Kamburelis.

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

{ }
unit RiftSound;

interface

uses CastleSoundEngine;

var
  stIntroMusic,
  stMainMenuMusic
  : TSoundType;

implementation

uses SysUtils, CastleFilesUtils;

initialization
  SoundEngine.SoundsFileName := ProgramDataPath + 'data' +
    PathDelim + 'sounds' + PathDelim + 'index.xml';

  stIntroMusic    := SoundEngine.SoundFromName('intro_music');
  stMainMenuMusic := SoundEngine.SoundFromName('main_menu_music');
end.
