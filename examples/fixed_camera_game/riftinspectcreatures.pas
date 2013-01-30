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
unit RiftInspectCreatures;

interface

procedure InspectCreatures;

implementation

uses CastleWindowModes, CastleCameras, CastleGLUtils, CastleWindow, GL, CastleVectors, SysUtils,
  CastleBitmapFont_BVSansMono_Bold_m15, CastleGLBitmapFonts,
  Classes, CastleStringUtils, CastleMessages, CastleFilesUtils,
  RiftVideoOptions, RiftGame, RiftWindow, RiftCreatures,
  CastleUIControls, RiftSceneManager, CastleColors, CastleKeysMouse;

var
  Creature: TCreature;
  UserQuit: boolean;
  StatusFont: TGLBitmapFont;
  SceneManager: TRiftSceneManager;

procedure Draw2D(Window: TCastleWindowBase);
var
  S: TStringList;
begin
  glLoadIdentity;

  glTranslatef(5, 5, 0);

  S := TStringList.Create;
  try
    S.Append(Format('Camera: pos %s, dir %s, up %s, Move speed (per sec): %f',
      [ VectorToNiceStr((SceneManager.Camera as TWalkCamera).Position),
        VectorToNiceStr((SceneManager.Camera as TWalkCamera).Direction),
        VectorToNiceStr((SceneManager.Camera as TWalkCamera).Up),
        (SceneManager.Camera as TWalkCamera).MoveSpeed ]));

    S.Append(Format('World time : %f', [WorldTime]));

    S.Append(Format('Creature state : %s', [CreatureStateName[Creature.State]]));

    StatusFont.PrintStringsBox(S, false, 0,
      Black4Single, Green4Single, Yellow4Single, 5);
  finally S.Free end;
end;

procedure Press(Window: TCastleWindowBase; const Event: TInputPressRelease);

  procedure ChangeState(NewState: TCreatureState);
  begin
    try
      Creature.State := NewState;
    except
      on E: ECreatureStateChangeNotPossible do
        MessageOk(Window, E.Message, taLeft);
    end;
  end;

begin
  if Event.EventType = itKey then
    case Event.KeyCharacter of
      CharEscape: UserQuit := true;
      'h': (SceneManager.Camera as TWalkCamera).GoToInitial;
      's': ChangeState(csStand);
      'w': ChangeState(csWalk);
      'b': ChangeState(csBored);
      else
        case Event.Key of
          K_F5: Window.SaveScreen(FileNameAutoInc('rift_screen_%d.png'));
        end;
    end;
end;

procedure Idle(Window: TCastleWindowBase);
begin
  WorldTime += Window.Fps.IdleSpeed;
end;

procedure InspectCreatures;
var
  SavedMode: TGLMode;
begin
  WorldTime := 0;

  SavedMode := TGLMode.CreateReset(Window, 0, false, nil, nil, @NoClose);
  try
    Window.FpsShowOnCaption := DebugMenuFps;
    Window.AutoRedisplay := true;

    SceneManager := TRiftSceneManager.Create(nil);
    Window.Controls.Add(SceneManager);

    SceneManager.Camera := TWalkCamera.Create(SceneManager);
    (SceneManager.Camera as TWalkCamera).Init(
      Vector3Single(3, 3, 4),
      Vector3Single(-1, -1, -1),
      Vector3Single(0, 0, 1) { this will be corrected for ortho to dir, don't worry },
      Vector3Single(0, 0, 1),
      0, 0.1);
    (SceneManager.Camera as TWalkCamera).MoveSpeed := 2.5;

    CreaturesKinds.Load(SceneManager.BaseLights);
    { TODO: allow to choose creature }
    Creature := TCreature.Create(PlayerKind);
    try
      Creature.Direction := Vector3Single(1, 0, 0);
      Creature.Up := Vector3Single(0, 0, 1);
      SceneManager.Items.Add(Creature);

      Window.OnDraw := @Draw2D;
      Window.OnPress := @Press;
      Window.OnIdle := @Idle;
      Window.OnDrawStyle := ds2D;

      Window.EventResize;

      UserQuit := false;
      repeat
        Application.ProcessMessage(true, true);
      until UserQuit;

    finally FreeAndNil(SavedMode); end;
  finally
    FreeAndNil(SceneManager);
    FreeAndNil(Creature);
  end;
end;

procedure WindowOpen(const Container: IUIContainer);
begin
  StatusFont := TGLBitmapFont.Create(BitmapFont_BVSansMono_Bold_m15);
end;

procedure WindowClose(const Container: IUIContainer);
begin
  FreeAndNil(StatusFont);
end;

initialization
  OnGLContextOpen.Add(@WindowOpen);
  OnGLContextClose.Add(@WindowClose);
end.
