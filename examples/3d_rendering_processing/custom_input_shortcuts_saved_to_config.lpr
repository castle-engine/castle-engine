{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ An example how to create user-configurable input shortcuts
  (own TInputShortcut instances, saved and loaded to UserConfig file)
  and use them for a camera or for custom actions. }

uses
  Classes, SysUtils,
  CastleWindow, CastleSceneManager, CastleConfig, CastleFilesUtils,
  CastleScene, X3DNodes,
  CastleKeysMouse, CastleInputs, CastleCameras, CastleVectors, CastleLog;

var
  Window: TCastleWindow;

  MyInputs: TInputShortcutList;
  Input_MoveUp: TInputShortcut;
  Input_MoveDown: TInputShortcut;
  Input_MoveLeft: TInputShortcut;
  Input_MoveRight: TInputShortcut;
  Input_DoSomethingCrazy: TInputShortcut;

procedure ApplicationInitialize;
var
  SceneManager: TCastleSceneManager;
  Scene: TCastleScene;
begin
  { Create your TInputShortcut instances.

    You can freely invent your own names for TInputShortcut below
    (3rd parameter for TInputShortcut.Create).
    You can use 'move_up' or 'my_up_movement' or whatever -- the names you
    invent here do not conflict, and are unrelated, to the input names
    on other lists (like InputsAll in CGE).
    These names are only used to identify this input in the config file later.

    The user-friendly names (2nd parameter for TInputShortcut.Create)
    may be used by some user-interface. They don't matter at all,
    unless you will use them in your UI.
  }
  Input_MoveUp := TInputShortcut.Create(nil, 'Move Up', 'move_up', igLocal);
  Input_MoveUp.Assign(keyW);
  Input_MoveDown := TInputShortcut.Create(nil, 'Move Down', 'move_down', igLocal);
  Input_MoveDown.Assign(keyS);
  Input_MoveLeft := TInputShortcut.Create(nil, 'Move Left', 'move_left', igLocal);
  Input_MoveLeft.Assign(keyA);
  Input_MoveRight := TInputShortcut.Create(nil, 'Move Right', 'move_right', igLocal);
  Input_MoveRight.Assign(keyD);
  Input_DoSomethingCrazy := TInputShortcut.Create(nil, 'Do Something Crazy', 'do_something_crazy', igLocal);
  Input_DoSomethingCrazy.Assign(keyC);

  { Create MyInputs to comfortably save/load your controls.
    MyInputs list also "owns" the children, so freeing MyInputs will free all
    TInputShortcut instances created above. }
  MyInputs := TInputShortcutList.Create;
  MyInputs.Add(Input_MoveUp);
  MyInputs.Add(Input_MoveDown);
  MyInputs.Add(Input_MoveLeft);
  MyInputs.Add(Input_MoveRight);
  MyInputs.Add(Input_DoSomethingCrazy);

  UserConfig.Load;
  MyInputs.LoadFromConfig(UserConfig, 'my_inputs');

  { Now user can customize inputs.
    To do this, you only change non-default properties of Input_Xxx,
    e.g.
    - you set Input_MoveUp.Key1 := ...;,
    - or you call Input_MoveUp.AssignCurrent(...)
    - or you use MessageKeyMouse (returning TInputPressRelease)
      followed by Input_MoveUp.Add(...).

    After customizing the inputs, be sure to save them to a config file,
    by calling

      MyInputs.SaveToConfig(UserConfig, 'my_inputs');
      UserConfig.Save;

    On a desktop, it's enough to just always save them in the "finalization"
    section, see at the bottom of this file.

    Note: If you don't customize the inputs, you may notice that the config
    file is not written at all. This is 100% correct. When all the TInputShortcut
    instances have the "current" state equal their "default" state,
    then MyInputs.SaveToConfig just clears them from the config file.
    In effect, UserConfig simply remains empty, and may not even be written
    to disk at all. This is transparent from your point of view -- "UserConfig.Load"
    and "MyInputs.LoadFromConfig" will work as you expect (doing nothing)
    at the next program run.
  }

  { Initialize SceneManager.
    Since we use TCastleWindow, a full-screen scene manager is already created for us. }
  SceneManager := Window.SceneManager;

  { Add car model to SceneManager }
  Scene := TCastleScene.Create(Application { Owner that will free the Scene });
  Scene.Load('castle-data:/car.x3d');
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;
  SceneManager.Items.Add(Scene);
  SceneManager.MainScene := Scene;

  { initialize SceneManager.WalkCamera.Input_Xxx to follow our inputs. }
  SceneManager.WalkCamera.Input_LeftStrafe.Assign(Input_MoveLeft, false);
  SceneManager.WalkCamera.Input_RightStrafe.Assign(Input_MoveRight, false);
  SceneManager.WalkCamera.Input_Jump.Assign(Input_MoveUp, false);
  SceneManager.WalkCamera.Input_Crouch.Assign(Input_MoveDown, false);

  { clear other SceneManager.WalkCamera inputs }
  SceneManager.WalkCamera.Input_Forward.MakeClear;
  SceneManager.WalkCamera.Input_Backward.MakeClear;
  SceneManager.WalkCamera.Input_LeftRot.MakeClear;
  SceneManager.WalkCamera.Input_RightRot.MakeClear;
  SceneManager.WalkCamera.Input_UpRotate.MakeClear;
  SceneManager.WalkCamera.Input_DownRotate.MakeClear;
  SceneManager.WalkCamera.Input_IncreasePreferredHeight.MakeClear;
  SceneManager.WalkCamera.Input_DecreasePreferredHeight.MakeClear;
  SceneManager.WalkCamera.Input_GravityUp.MakeClear;
  SceneManager.WalkCamera.Input_Run.MakeClear;
  SceneManager.WalkCamera.Input_MoveSpeedInc.MakeClear;
  SceneManager.WalkCamera.Input_MoveSpeedDec.MakeClear;

  { configure other SceneManager.WalkCamera properties }
  SceneManager.WalkCamera.Gravity := false;
  SceneManager.WalkCamera.MoveSpeed := 10;
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
var
  CarMaterial: TMaterialNode;
begin
  { This is an example how to detect whether a given TInputShortcut was pressed,
    when you have Event as TInputPressRelease instance. }

  if Input_DoSomethingCrazy.IsEvent(Event) then
  begin
    CarMaterial := Window.SceneManager.MainScene.Node('MA_Material') as TMaterialNode;
    CarMaterial.DiffuseColor := Vector3(Random, Random, Random);
  end;
end;

begin
  InitializeLog;

  Application.OnInitialize := @ApplicationInitialize;

  Window := TCastleWindow.Create(Application);
  Window.OnPress := @WindowPress;
  Window.OpenAndRun;

  { Finalization (when OpenAndRun finished) }
  MyInputs.SaveToConfig(UserConfig, 'my_inputs');
  UserConfig.Save;
  FreeAndNil(MyInputs);
end.
