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
  GameSceneSubAnimations;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ButtonLoadGranger, ButtonLoadDretch, ButtonLoadBasilisk,
      ButtonLoadMarauder, ButtonLoadDragoon, ButtonLoadTyrant: TCastleButton;
    ModelParent: TCastleTransform;
    VerticalGroupAnimations: TCastleVerticalGroup;
    CheckboxLoop: TCastleCheckbox;
  private
    ModelScene: TSceneSubAnimations;
    procedure LoadModel(const ModelUrl: String);
    procedure ClickLoadGranger(Sender: TObject);
    procedure ClickLoadDretch(Sender: TObject);
    procedure ClickLoadBasilisk(Sender: TObject);
    procedure ClickLoadMarauder(Sender: TObject);
    procedure ClickLoadDragoon(Sender: TObject);
    procedure ClickLoadTyrant(Sender: TObject);
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
begin
  inherited;

  ButtonLoadGranger.OnClick := {$ifdef FPC}@{$endif} ClickLoadGranger;
  ButtonLoadDretch.OnClick := {$ifdef FPC}@{$endif} ClickLoadDretch;
  ButtonLoadBasilisk.OnClick := {$ifdef FPC}@{$endif} ClickLoadBasilisk;
  ButtonLoadMarauder.OnClick := {$ifdef FPC}@{$endif} ClickLoadMarauder;
  ButtonLoadDragoon.OnClick := {$ifdef FPC}@{$endif} ClickLoadDragoon;
  ButtonLoadTyrant.OnClick := {$ifdef FPC}@{$endif} ClickLoadTyrant;

  // load some initial model
  ClickLoadMarauder(nil);
end;

procedure TViewMain.ClickLoadGranger(Sender: TObject);
begin
  LoadModel('castle-data:/tremulous-data/builder/nonseg.md3');
end;

procedure TViewMain.ClickLoadDretch(Sender: TObject);
begin
  LoadModel('castle-data:/tremulous-data/level0/nonseg.md3');
end;

procedure TViewMain.ClickLoadBasilisk(Sender: TObject);
begin
  LoadModel('castle-data:/tremulous-data/level1/nonseg.md3');
end;

procedure TViewMain.ClickLoadMarauder(Sender: TObject);
begin
  LoadModel('castle-data:/tremulous-data/level2/nonseg.md3');
end;

procedure TViewMain.ClickLoadDragoon(Sender: TObject);
begin
  LoadModel('castle-data:/tremulous-data/level3/nonseg.md3');
end;

procedure TViewMain.ClickLoadTyrant(Sender: TObject);
begin
  LoadModel('castle-data:/tremulous-data/level4/nonseg.md3');
end;

procedure TViewMain.LoadModel(const ModelUrl: String);
var
  I: Integer;
  NewButton: TCastleButton;
  SubAnimName: String;
begin
  FreeAndNil(ModelScene);

  ModelScene := TSceneSubAnimations.Create(FreeAtStop);
  ModelScene.Load(ModelUrl);
  { Do not call PlayAnimation, as then both sensors started by PlayAnimation,
    and ForceAnimationPose done by PlaySubAnimation, would try to update the model
    -- it would be undefined which animation is eventually visible. }
  // ModelScene.PlayAnimation('animation', true);
  ModelParent.Add(ModelScene);

  // clear VerticalGroupAnimations
  for I := VerticalGroupAnimations.ControlsCount - 1 downto 0 do
    if VerticalGroupAnimations.Controls[I] is TCastleButton then
      VerticalGroupAnimations.Controls[I].Free;

  // add VerticalGroupAnimations buttons based on ModelScene.SubAnimations
  for SubAnimName in ModelScene.SubAnimations.Keys do
  begin
    NewButton := TCastleButton.Create(FreeAtStop);
    NewButton.Caption := SubAnimName;
    NewButton.OnClick := {$ifdef FPC}@{$endif} ClickPlayAnimation;
    VerticalGroupAnimations.InsertFront(NewButton);
  end;
end;

procedure TViewMain.ClickPlayAnimation(Sender: TObject);
var
  SubAnimationName: String;
begin
  SubAnimationName := (Sender as TCastleButton).Caption;
  ModelScene.PlaySubAnimation(SubAnimationName, CheckboxLoop.Checked);
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

end.
