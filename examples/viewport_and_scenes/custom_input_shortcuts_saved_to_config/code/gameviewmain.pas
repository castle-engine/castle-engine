{
  Copyright 2018-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleCameras, CastleScene;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    CarScene: TCastleScene;
    WalkNavigation: TCastleWalkNavigation;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  X3DNodes,
  GameInputs;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  InitializeInputs;

  { Now user can customize inputs.
    To do this, you only change non-default properties of Input_Xxx,
    e.g.
    - you set Input_MoveForward.Key1 := ...;,
    - or you call Input_MoveForward.AssignCurrent(...)
    - or you use MessageKeyMouse (returning TInputPressRelease)
      followed by Input_MoveForward.Add(...).

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

  { initialize WalkNavigation.Input_Xxx to follow our inputs. }
  WalkNavigation.Input_LeftStrafe.Assign(Input_MoveLeft, false);
  WalkNavigation.Input_RightStrafe.Assign(Input_MoveRight, false);
  WalkNavigation.Input_Forward.Assign(Input_MoveForward, false);
  WalkNavigation.Input_Backward.Assign(Input_MoveBackward, false);

  { clear other WalkNavigation inputs }
  WalkNavigation.Input_Jump.MakeClear;
  WalkNavigation.Input_Crouch.MakeClear;
  WalkNavigation.Input_LeftRotate.MakeClear;
  WalkNavigation.Input_RightRotate.MakeClear;
  WalkNavigation.Input_UpRotate.MakeClear;
  WalkNavigation.Input_DownRotate.MakeClear;
  WalkNavigation.Input_IncreasePreferredHeight.MakeClear;
  WalkNavigation.Input_DecreasePreferredHeight.MakeClear;
  WalkNavigation.Input_GravityUp.MakeClear;
  WalkNavigation.Input_Run.MakeClear;
  WalkNavigation.Input_MoveSpeedInc.MakeClear;
  WalkNavigation.Input_MoveSpeedDec.MakeClear;
end;

procedure TViewMain.Stop;
begin
  FinalizeInputs;
  inherited;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
var
  CarAppearance: TAppearanceNode;
  CarMaterial: TPhysicalMaterialNode;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { This is an example how to detect whether a given TInputShortcut was pressed,
    when you have Event as TInputPressRelease instance. }

  if Input_ChangeColor.IsEvent(Event) then
  begin
    { Car model was exported from Blender, using Blender->glTF exporter,
      and Blender material for shell was called just "Material".
      That's why we know that
      - it has a node TAppearanceNode called "Material"
      - this has a material of type TPhysicalMaterialNode.
    }
    CarAppearance := CarScene.Node('Material') as TAppearanceNode;
    CarMaterial := CarAppearance.Material as TPhysicalMaterialNode;
    CarMaterial.BaseColor := Vector3(Random, Random, Random);
  end;
end;

end.
