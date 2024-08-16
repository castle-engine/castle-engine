{
  Copyright 2024-2024 Michalis Kamburelis.

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
  CastleVectors, CastleComponentSerialize, CastleCameras,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleViewport;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    FlyNavigation: TCastleWalkNavigation;
    CheckboxOptimizeExtensiveTransformations: TCastleCheckbox;
    CheckboxInternalFastTransformUpdate: TCastleCheckbox;
    CheckboxDynamicBatching: TCastleCheckbox;
    CheckboxAnimateOnlyWhenVisible: TCastleCheckbox;
    CheckboxAnimateSkipTicks1: TCastleCheckbox;
    MainViewport: TCastleViewport;
  private
    procedure CheckboxOptimizeExtensiveTransformationsChange(Sender: TObject);
    procedure CheckboxInternalFastTransformUpdateChange(Sender: TObject);
    procedure CheckboxDynamicBatchingChange(Sender: TObject);
    procedure CheckboxAnimateOnlyWhenVisibleChange(Sender: TObject);
    procedure CheckboxAnimateSkipTicks1Change(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, Math,
  CastleSceneCore, CastleScene, CastleTransform;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  CheckboxOptimizeExtensiveTransformations.OnChange := {$ifdef FPC}@{$endif} CheckboxOptimizeExtensiveTransformationsChange;
  CheckboxInternalFastTransformUpdate.OnChange := {$ifdef FPC}@{$endif} CheckboxInternalFastTransformUpdateChange;
  CheckboxDynamicBatching.OnChange := {$ifdef FPC}@{$endif} CheckboxDynamicBatchingChange;
  CheckboxAnimateOnlyWhenVisible.OnChange := {$ifdef FPC}@{$endif} CheckboxAnimateOnlyWhenVisibleChange;
  CheckboxAnimateSkipTicks1.OnChange := {$ifdef FPC}@{$endif} CheckboxAnimateSkipTicks1Change;

  // make these 2 optimizations "on" by default in this demo
  CheckboxOptimizeExtensiveTransformations.Checked := true;
  CheckboxOptimizeExtensiveTransformationsChange(nil);
  CheckboxInternalFastTransformUpdate.Checked := true;
  CheckboxInternalFastTransformUpdateChange(nil);
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;

  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  FlyNavigation.MouseLook := buttonRight in Container.MousePressed;
end;

procedure TViewMain.CheckboxOptimizeExtensiveTransformationsChange(Sender: TObject);
begin
  OptimizeExtensiveTransformations := CheckboxOptimizeExtensiveTransformations.Checked;
end;

procedure TViewMain.CheckboxInternalFastTransformUpdateChange(Sender: TObject);
begin
  InternalFastTransformUpdate := CheckboxInternalFastTransformUpdate.Checked;
end;

procedure TViewMain.CheckboxDynamicBatchingChange(Sender: TObject);
begin
  MainViewport.DynamicBatching := CheckboxDynamicBatching.Checked;
end;

procedure TViewMain.CheckboxAnimateOnlyWhenVisibleChange(Sender: TObject);
var
  Scene: TCastleScene;
  T: TCastleTransform;
begin
  for T in MainViewport.Items do
    if T is TCastleScene then
    begin
      Scene := TCastleScene(T);
      Scene.AnimateOnlyWhenVisible := CheckboxAnimateOnlyWhenVisible.Checked;
    end;
end;

procedure TViewMain.CheckboxAnimateSkipTicks1Change(Sender: TObject);
var
  Scene: TCastleScene;
  T: TCastleTransform;
begin
  for T in MainViewport.Items do
    if T is TCastleScene then
    begin
      Scene := TCastleScene(T);
      Scene.AnimateSkipTicks := IfThen(CheckboxAnimateSkipTicks1.Checked, 1, 0);
    end;
end;

end.
