{
  Copyright 2023-2023 Michalis Kamburelis.

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
  CastleVectors, CastleComponentSerialize, CastleTransform,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene,
  GameSubAnimations;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ButtonLoadGranger, ButtonLoadDretch, ButtonLoadBasilisk,
      ButtonLoadMarauder, ButtonLoadDragoon, ButtonLoadTyrant: TCastleButton;
    SceneHalloweenPumpkinLanternKnight1: TCastleScene;
    VerticalGroupAnimations: TCastleVerticalGroup;
    CheckboxLoop: TCastleCheckbox;
  private
    SubAnimations: TSubAnimations;
    procedure ClickPlayAnimation(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;

  { Create SubAnimations behavior to SceneHalloweenPumpkinLanternKnight1.
    Define subanimations specific to it. }
  procedure LoadModelPumpkin;
  begin
    { Do not play long animation using TCastleScene.PlayAnimation / AutoAnimation
      (AutoAnimation could be set in CGE editor).
      We want SubAnimations to control model state, to reflect current subanimation. }
    SceneHalloweenPumpkinLanternKnight1.StopAnimation;

    SubAnimations := TSubAnimations.Create(FreeAtStop);
    SceneHalloweenPumpkinLanternKnight1.AddBehavior(SubAnimations);
    SubAnimations.Add('idle', 'Take 001', 0, 5.0);
    SubAnimations.Add('head_bounce', 'Take 001', 7.4, 2.0);
  end;

  procedure UpdateSubAnimationButtons;
  var
    I: Integer;
    NewButton: TCastleButton;
    SubAnimName: String;
  begin
    // clear VerticalGroupAnimations
    for I := VerticalGroupAnimations.ControlsCount - 1 downto 0 do
      if VerticalGroupAnimations.Controls[I] is TCastleButton then
        VerticalGroupAnimations.Controls[I].Free;

    // add VerticalGroupAnimations buttons based on ModelScene.SubAnimations
    for SubAnimName in SubAnimations.List.Keys do
    begin
      NewButton := TCastleButton.Create(FreeAtStop);
      NewButton.Caption := SubAnimName;
      NewButton.OnClick := {$ifdef FPC}@{$endif} ClickPlayAnimation;
      VerticalGroupAnimations.InsertFront(NewButton);
    end;
  end;

begin
  inherited;
  LoadModelPumpkin;
  UpdateSubAnimationButtons;
end;

procedure TViewMain.ClickPlayAnimation(Sender: TObject);
var
  SubAnimationName: String;
begin
  SubAnimationName := (Sender as TCastleButton).Caption;
  SubAnimations.Play(SubAnimationName, CheckboxLoop.Checked);
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

end.
