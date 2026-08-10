{
  Copyright 2026-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Define songs (audio files) available in the application. }
unit GameSongs;

interface

uses Generics.Collections;

type
  TSong = class
    UrlArt: String;
    UrlMusic: String;
    Artist: String;
    Title: String;
  end;

  TSongList = {$ifdef FPC}specialize{$endif} TObjectList<TSong>;

var
  Songs: TSongList;

implementation

uses SysUtils;

var
  Song: TSong;
initialization
  Songs := TSongList.Create;

  Song := TSong.Create;
  Song.UrlMusic := 'castle-data:/songs/battle.ogg';
  Song.UrlArt := 'castle-data:/songs/battle_rainy_street.png';
  Song.Artist := 'The Cynic Project';
  Song.Title := 'Battle Theme A';
  Songs.Add(Song);

  Song := TSong.Create;
  Song.UrlMusic := 'castle-data:/songs/cave.ogg';
  Song.UrlArt := 'castle-data:/songs/cave_bluelarge.png';
  Song.Artist := 'The Cynic Project';
  Song.Title := 'Crystal Cave (song18)';
  Songs.Add(Song);

  Song := TSong.Create;
  Song.UrlMusic := 'castle-data:/songs/town.ogg';
  Song.UrlArt := 'castle-data:/songs/town_house.png';
  Song.Artist := 'The Cynic Project';
  Song.Title := 'Town Theme RPG';
  Songs.Add(Song);
finalization
  FreeAndNil(Songs);
end.
