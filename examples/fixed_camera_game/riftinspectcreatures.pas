{
  Copyright 2007-2017 Michalis Kamburelis.

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ }
unit RiftInspectCreatures;

interface

procedure InspectCreatures;

implementation

uses CastleWindowModes, CastleCameras, CastleGLUtils, CastleWindow, CastleVectors, SysUtils,
  Classes, CastleStringUtils, CastleMessages, CastleFilesUtils,
  RiftVideoOptions, RiftGame, RiftWindow, RiftCreatures, CastleControlsImages,
  CastleUIControls, RiftSceneManager, CastleColors, CastleKeysMouse, CastleControls;

var
  Creature: TCreature;
  UserQuit: boolean;
  SceneManager: TRiftSceneManager;

{ TStatusText ---------------------------------------------------------------- }

type
  TStatusText = class(TCastleLabel)
    procedure Render; override;
  end;

procedure TStatusText.Render;
var
  Pos, Dir, Up: TVector3Single;
begin
  if not GetExists then Exit;

  { regenerate Text contents at every Render call }
  Text.Clear;
  SceneManager.Camera.GetView(Pos, Dir, Up);
  Text.Append(Format('Camera: pos %s, dir %s, up %s',
    [ VectorToNiceStr(Pos), VectorToNiceStr(Dir), VectorToNiceStr(Up) ]));
  Text.Append(Format('World time : %f', [WorldTime]));
  Text.Append(Format('Creature state : %s', [CreatureStateName[Creature.State]]));

  inherited;
end;

var
  StatusText: TStatusText;

procedure Press(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.EventType = itKey then
    case Event.KeyCharacter of
      CharEscape: UserQuit := true;
      'h': (SceneManager.Camera as TWalkCamera).GoToInitial;
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
begin
  WorldTime += Window.Fps.UpdateSecondsPassed;
end;

procedure InspectCreatures;
var
  SavedMode: TGLMode;
begin
  WorldTime := 0;

  SavedMode := TGLMode.CreateReset(Window, nil, nil, @NoClose);
  try
    Window.FpsShowOnCaption := DebugMenuFps;
    Window.AutoRedisplay := true;

    SceneManager := TRiftSceneManager.Create(nil);
    Window.Controls.InsertBack(SceneManager);

    SceneManager.Camera := TWalkCamera.Create(SceneManager);
    (SceneManager.Camera as TWalkCamera).Init(
      Vector3Single(3, 3, 4),
      Vector3Single(-1, -1, -1),
      Vector3Single(0, 0, 1) { this will be corrected for ortho to dir, don't worry },
      Vector3Single(0, 0, 1),
      0, 0.1);
    (SceneManager.Camera as TWalkCamera).MoveSpeed := 2.5;
    { by default, forward is also activated with "w", and backward with "s",
      which conflicts with our "walk" and "stand" shortcuts }
    (SceneManager.Camera as TWalkCamera).Input_Forward.Assign(K_Up);
    (SceneManager.Camera as TWalkCamera).Input_Backward.Assign(K_Down);
    CreaturesKinds.Load(SceneManager.BaseLights);
    { TODO: allow to choose creature }
    Creature := TCreature.Create(PlayerKind);
    try
      Creature.Direction := Vector3Single(1, 0, 0);
      Creature.Up := Vector3Single(0, 0, 1);
      SceneManager.Items.Add(Creature);

      StatusText := TStatusText.Create(Window);
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
