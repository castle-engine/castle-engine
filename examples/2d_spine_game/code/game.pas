{
  Copyright 2014-2014 Michalis Kamburelis.

  This file is part of "Castle Spine".

  "Castle Spine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Spine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Implements the game logic, independent from Android / standalone. }
unit Game;

interface

uses CastleWindow;

var
  Window: TCastleWindowCustom;

implementation

uses SysUtils,
  CastleKeysMouse, CastleFilesUtils, Castle2DSceneManager;

var
  SceneManager: T2DSceneManager;

{ One-time initialization. }
procedure ApplicationInitialize;
begin
  SceneManager := T2DSceneManager.Create(Application);
  Window.Controls.InsertFront(SceneManager);
end;

procedure WindowOpen(Container: TUIContainer);
begin
end;

procedure WindowResize(Container: TUIContainer);
begin
end;

procedure WindowUpdate(Container: TUIContainer);
begin
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.IsKey(K_F5) then
    Window.SaveScreen(FileNameAutoInc(ApplicationName + '_screen_%d.png'));
  if Event.IsKey(K_Escape) then
    Application.Quit;

  if Event.IsMouseButton(mbLeft) then
  begin
  end;
end;

function MyGetApplicationName: string;
begin
  Result := 'castle_spine';
end;

initialization
  { This should be done as early as possible to mark our log lines correctly. }
  OnGetApplicationName := @MyGetApplicationName;

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindowCustom.Create(Application);
  Window.OnOpen := @WindowOpen;
  Window.OnPress := @WindowPress;
  Window.OnUpdate := @WindowUpdate;
  Window.OnResize := @WindowResize;
  Window.FpsShowOnCaption := true;
  Application.MainWindow := Window;
end.
