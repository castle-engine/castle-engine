{
  Copyright 2007-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

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
