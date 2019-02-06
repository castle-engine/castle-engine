{
  Copyright 2016-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Initialize the game window and states. }
unit GameInitialize;

interface

implementation

uses SysUtils, Classes, CastleControls, CastleUtils, CastleFilesUtils,
  CastleColors, CastleUIControls, CastleUIState, CastleWindow,
  CastleApplicationProperties,
  GameStateMainMenu, GameStatePlay, GameStateAskDialog;

var
  Window: TCastleWindowBase;

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

initialization
  { Set ApplicationName early, as our log uses it. }
  ApplicationProperties.ApplicationName := 'zombie_fighter';

  Window := TCastleWindowBase.Create(Application);

  Application.MainWindow := Window;
  Application.OnInitialize := @ApplicationInitialize;
end.
