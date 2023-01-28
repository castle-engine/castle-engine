{
  Copyright 2018-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Store user-configurable shortcuts. }
unit GameInputs;

interface

uses CastleInputs;

{ Create TInputShortcut instances, MyInputs list, load input configuration from config file.

  You can call it from some state Start, or just from Application.OnInitialize if you need
  the inputs to exist throughout entire application. }
procedure InitializeInputs;

{ Release things created by InitializeInputs, save input configuration to config file.

  You can call it from some state Stop, or just "finalization" section of some unit if you need
  the inputs to exist throughout entire application. }
procedure FinalizeInputs;

var
  MyInputs: TInputShortcutList;
  Input_MoveForward: TInputShortcut;
  Input_MoveBackward: TInputShortcut;
  Input_MoveLeft: TInputShortcut;
  Input_MoveRight: TInputShortcut;
  Input_ChangeColor: TInputShortcut;

implementation

uses SysUtils,
  CastleConfig, CastleKeysMouse;

procedure InitializeInputs;
begin
  { Create your TInputShortcut instances.

    You can freely invent your own names for TInputShortcut below
    (3rd parameter for TInputShortcut.Create).
    You can use 'move_up' or 'my_up_movement' or whatever -- the names you
    invent here never conflict, and are unrelated, to the input names
    on other lists (like InputsAll in CGE).
    These names are only used to identify this input in the config file later.

    The user-friendly names (2nd parameter for TInputShortcut.Create)
    may be used by some user-interface. They don't matter at all,
    unless you will use them in your UI.
  }
  Input_MoveForward := TInputShortcut.Create(nil, 'Move Forward', 'move_forward', igLocal);
  Input_MoveForward.Assign(keyW);
  Input_MoveBackward := TInputShortcut.Create(nil, 'Move Backward', 'move_backward', igLocal);
  Input_MoveBackward.Assign(keyS);
  Input_MoveLeft := TInputShortcut.Create(nil, 'Move Left', 'move_left', igLocal);
  Input_MoveLeft.Assign(keyA);
  Input_MoveRight := TInputShortcut.Create(nil, 'Move Right', 'move_right', igLocal);
  Input_MoveRight.Assign(keyD);
  Input_ChangeColor := TInputShortcut.Create(nil, 'Change Color', 'change_color', igLocal);
  Input_ChangeColor.Assign(keyC);

  { Create MyInputs to comfortably save/load your controls.
    MyInputs list also "owns" the children, so freeing MyInputs will free all
    TInputShortcut instances created above. }
  MyInputs := TInputShortcutList.Create;
  MyInputs.Add(Input_MoveForward);
  MyInputs.Add(Input_MoveBackward);
  MyInputs.Add(Input_MoveLeft);
  MyInputs.Add(Input_MoveRight);
  MyInputs.Add(Input_ChangeColor);

  UserConfig.Load;
  MyInputs.LoadFromConfig(UserConfig, 'my_inputs');
end;

procedure FinalizeInputs;
begin
  { At stop, save settings.

    Note: if you target non-desktop platforms (like mobile) you should also save
    the UserConfig whenever user changes it, in the middle of the game.
    That's because on mobile, your process can be killed at any moment. }
  MyInputs.SaveToConfig(UserConfig, 'my_inputs');
  UserConfig.Save;
  FreeAndNil(MyInputs);
end;

end.
