{
  Copyright 2007-2017 Michalis Kamburelis.

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

{ State to inspect creatures (right now: player) animations. }
unit RiftInspectCreatures;

interface

procedure InspectCreatures;

implementation

uses SysUtils, Classes,
  CastleWindowModes, CastleCameras, CastleGLUtils, CastleWindow, CastleVectors,
  CastleColors, CastleKeysMouse, CastleControls, CastleStringUtils, CastleMessages,
  CastleFilesUtils, CastleControlsImages, CastleUIControls, CastleUtils,
  RiftVideoOptions, RiftWindow, RiftCreatures, RiftSceneManager;

var
  Creature: TCreature;
  UserQuit: boolean;
  SceneManager: TRiftSceneManager;
  StatusText: TCastleLabel;

procedure Press(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.EventType = itKey then
    case Event.KeyCharacter of
      CharEscape: UserQuit := true;
      'h': SceneManager.WalkCamera.GoToInitial;
      's': Creature.State := csStand;
      'w': Creature.State := csWalk;
      'b': Creature.State := csBored;
      else
        case Event.Key of
          K_F5: Window.SaveScreen(FileNameAutoInc('rift_screen_%d.png'));
        end;
    end;
end;

procedure Update(Container: TUIContainer);
var
  Pos, Dir, Up: TVector3;
begin
  SceneManager.Camera.GetView(Pos, Dir, Up);
  StatusText.Caption :=
    Format('Camera: pos %s, dir %s, up %s',
      [Pos.ToString, Dir.ToString, Up.ToString]) + NL +
    Format('Creature state : %s',
      [CreatureStateName[Creature.State]]) + NL +
    'Press W B S - change state, F5 - screenshot, H - camera home';
end;

procedure InspectCreatures;
var
  SavedMode: TGLMode;
begin
  SavedMode := TGLMode.CreateReset(Window, nil, nil, @NoClose);
  try
    Window.FpsShowOnCaption := DebugMenuFps;
    Window.AutoRedisplay := true;

    SceneManager := TRiftSceneManager.Create(nil);
    Window.Controls.InsertBack(SceneManager);

    SceneManager.WalkCamera.Init(
      Vector3(3, 3, 4),
      Vector3(-1, -1, -1),
      Vector3(0, 0, 1) { this will be corrected for ortho to dir, don't worry },
      Vector3(0, 0, 1),
      0, 0.1);
    SceneManager.WalkCamera.MoveSpeed := 2.5;
    { by default, forward is also activated with "w", and backward with "s",
      which conflicts with our "walk" and "stand" shortcuts }
    SceneManager.WalkCamera.Input_Forward.Assign(K_Up);
    SceneManager.WalkCamera.Input_Backward.Assign(K_Down);
    CreaturesKinds.Load(SceneManager.PrepareParams);
    { TODO: allow to choose creature }
    Creature := TCreature.Create(PlayerKind);
    try
      Creature.Direction := Vector3(1, 0, 0);
      Creature.Up := Vector3(0, 0, 1);
      SceneManager.Items.Add(Creature);

      StatusText := TCastleLabel.Create(Window);
      StatusText.Padding := 5;
      StatusText.Left := 5;
      StatusText.Bottom := 5;
      StatusText.Color := Yellow;
      StatusText.Frame := true;
      Window.Controls.InsertFront(StatusText);

      Window.OnPress := @Press;
      Window.OnUpdate := @Update;

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

initialization
  Theme.Images[tiLabel] := FrameYellowBlack;
  Theme.Corners[tiLabel] := Vector4Integer(1, 1, 1, 1);
end.
