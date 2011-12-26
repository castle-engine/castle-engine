{
  Copyright 2007-2011 Michalis Kamburelis.

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

uses WindowModes, Cameras, CastleGLUtils, CastleWindow, GL, VectorMath, SysUtils,
  BFNT_BitstreamVeraSansMono_Bold_m15_Unit, OpenGLBmpFonts,
  Classes, CastleStringUtils, CastleMessages, CastleFilesUtils,
  RiftVideoOptions, RiftGame, RiftWindow, RiftCreatures,
  UIControls, RiftSceneManager, CastleColors;

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
      [ VectorToNiceStr((SceneManager.Camera As TWalkCamera).Position),
        VectorToNiceStr((SceneManager.Camera As TWalkCamera).Direction),
        VectorToNiceStr((SceneManager.Camera As TWalkCamera).Up),
        (SceneManager.Camera As TWalkCamera).MoveSpeed ]));

    S.Append(Format('World time : %f', [WorldTime]));

    S.Append(Format('Creature state : %s', [CreatureStateName[Creature.State]]));

    StatusFont.PrintStringsBox(S, 0,
      Black4Single, Green4Single, Yellow4Single, nil, 5, 1, 1);
  finally S.Free end;
end;

procedure KeyDown(Window: TCastleWindowBase; key: TKey; c: char);

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
  case C of
    CharEscape: UserQuit := true;
    'h': (SceneManager.Camera As TWalkCamera).GoToInitial;
    's': ChangeState(csStand);
    'w': ChangeState(csWalk);
    'b': ChangeState(csBored);
    else
      case Key of
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
    (SceneManager.Camera As TWalkCamera).Init(
      Vector3Single(3, 3, 4),
      Vector3Single(-1, -1, -1),
      Vector3Single(0, 0, 1) { this will be corrected for ortho to dir, don't worry },
      Vector3Single(0, 0, 1),
      0, 0);
    (SceneManager.Camera As TWalkCamera).MoveSpeed := 2.5;

    CreaturesKinds.Load(SceneManager.BaseLights);
    { TODO: allow to choose creature }
    Creature := TCreature.Create(PlayerKind);
    try
      SceneManager.Items.Add(Creature);

      Window.OnDraw := @Draw2D;
      Window.OnKeyDown := @KeyDown;
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

procedure WindowOpen(Window: TCastleWindowBase);
begin
  StatusFont := TGLBitmapFont.Create(@BFNT_BitstreamVeraSansMono_Bold_m15);
end;

procedure WindowClose(Window: TCastleWindowBase);
begin
  FreeAndNil(StatusFont);
end;

initialization
  Window.OnOpenList.Add(@WindowOpen);
  Window.OnCloseList.Add(@WindowClose);
end.
