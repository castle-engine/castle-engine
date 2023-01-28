{
  Copyright 2021-2023 Michalis Kamburelis.

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
  CastleControls, CastleWindow, CastleUIControls,
  CastleTiledMap, CastleKeysMouse;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    TiledMap: TCastleTiledMapControl;
    ButtonOpen: TCastleButton;
    CheckboxSmoothScaling: TCastleCheckbox;
    CheckboxSmoothScalingSafeBorder: TCastleCheckbox;
  private
    procedure ClickOpen(Sender: TObject);
    procedure MapMotion(const Sender: TCastleUserInterface;
      const Event: TInputMotion; var Handled: Boolean);
    procedure MapPress(const Sender: TCastleUserInterface;
      const Event: TInputPressRelease; var Handled: Boolean);
    procedure CheckboxSmoothScalingChange(Sender: TObject);
    procedure CheckboxSmoothScalingSafeBorderChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleComponentSerialize, CastleApplicationProperties, CastleParameters,
  CastleUtils;

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  { Assign events }
  ButtonOpen.OnClick := {$ifdef FPC}@{$endif} ClickOpen;
  TiledMap.OnMotion := {$ifdef FPC}@{$endif} MapMotion;
  TiledMap.OnPress := {$ifdef FPC}@{$endif} MapPress;
  CheckboxSmoothScaling.OnChange := {$ifdef FPC}@{$endif} CheckboxSmoothScalingChange;
  CheckboxSmoothScalingSafeBorder.OnChange := {$ifdef FPC}@{$endif} CheckboxSmoothScalingSafeBorderChange;

  { synchronize initial checkbox state with TiledMap state }
  CheckboxSmoothScaling.Checked := TiledMap.SmoothScaling;
  CheckboxSmoothScalingSafeBorder.Checked := TiledMap.SmoothScalingSafeBorder;

  { Load the map from parameter or default. }
  if Parameters.High = 1 then
    TiledMap.URL := Parameters[1]
  else
    TiledMap.URL := 'castle-data:/maps/desert.tmx';
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewMain.ClickOpen(Sender: TObject);
var
  Url: String;
begin
  Url := TiledMap.Url;
  if Application.MainWindow.FileDialog('Open Map', Url, true, 'Tiled Map (*.tmx)|*.tmx|All Files|*') then
    TiledMap.Url := Url;
end;

procedure TViewMain.MapMotion(const Sender: TCastleUserInterface;
  const Event: TInputMotion; var Handled: Boolean);
begin
  if buttonLeft in Event.Pressed then
  begin
    TiledMap.Origin := TiledMap.Origin -
      (Event.Position - Event.OldPosition) / TiledMap.Scale;
    Handled := true;
  end;
end;

procedure TViewMain.MapPress(const Sender: TCastleUserInterface;
  const Event: TInputPressRelease; var Handled: Boolean);
const
  MinScale = 0.05; //< this MinScale allows to view whole data/maps/desert_big.tmx
  MaxScale = 10;
begin
  if Event.IsMouseWheel(mwUp) then
  begin
    TiledMap.Scale := Clamped(TiledMap.Scale * 1.1, MinScale, MaxScale);
    Handled := true;
  end else
  if Event.IsMouseWheel(mwDown) then
  begin
    TiledMap.Scale := Clamped(TiledMap.Scale / 1.1, MinScale, MaxScale);
    Handled := true;
  end;
end;

procedure TViewMain.CheckboxSmoothScalingChange(Sender: TObject);
begin
  TiledMap.SmoothScaling := CheckboxSmoothScaling.Checked;
end;

procedure TViewMain.CheckboxSmoothScalingSafeBorderChange(Sender: TObject);
begin
  TiledMap.SmoothScalingSafeBorder := CheckboxSmoothScalingSafeBorder.Checked;
end;

end.
