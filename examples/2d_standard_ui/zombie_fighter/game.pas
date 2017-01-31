{
  Copyright 2016-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Initialize the game window and states. }
unit Game;

interface

uses CastleWindow;

var
  Window: TCastleWindowCustom;

implementation

uses SysUtils, Classes, CastleControls, CastleUtils, CastleFilesUtils,
  CastleColors, CastleUIControls, CastleUIState,
  GameStateMainMenu, GameStatePlay, GameStateAskDialog;

procedure ApplicationInitialize;
begin
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;

  { create all the states }
  StateMainMenu := TStateMainMenu.Create(Application);
  StatePlay := TStatePlay.Create(Application);
  StateAskDialog := TStateAskDialog.Create(Application);

  { initialize first state }
  TUIState.Current := StateMainMenu;
end;

function MyGetApplicationName: string;
begin
  Result := 'zombie_fighter';
end;

initialization
  { This should be done as early as possible to mark our log lines correctly. }
  OnGetApplicationName := @MyGetApplicationName;

  Window := TCastleWindowCustom.Create(Application);

  Application.MainWindow := Window;
  Application.OnInitialize := @ApplicationInitialize;
end.
