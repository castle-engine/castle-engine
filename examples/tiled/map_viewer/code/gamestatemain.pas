{
  Copyright 2021-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main state, where most of the application logic takes place. }
unit GameStateMain;

interface

uses Classes,
  CastleUIState, CastleControls, CastleWindow, CastleUIControls,
  CastleTiledMap, CastleKeysMouse;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  strict private
    { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
    LabelFps: TCastleLabel;
    TiledMap: TCastleTiledMapControl;
    ButtonOpen: TCastleButton;
    CheckboxSmoothScaling: TCastleCheckbox;
    CheckboxSmoothScalingSafeBorder: TCastleCheckbox;

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
  StateMain: TStateMain;

implementation

uses SysUtils,
  CastleComponentSerialize, CastleApplicationProperties, CastleParameters,
  CastleUtils;

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  TiledMap := DesignedComponent('TiledMap') as TCastleTiledMapControl;
  ButtonOpen := DesignedComponent('ButtonOpen') as TCastleButton;
  CheckboxSmoothScaling := DesignedComponent('CheckboxSmoothScaling') as TCastleCheckbox;
  CheckboxSmoothScalingSafeBorder := DesignedComponent('CheckboxSmoothScalingSafeBorder') as TCastleCheckbox;

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

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TStateMain.ClickOpen(Sender: TObject);
var
  URL: String;
begin
  URL := TiledMap.URL;
  if Application.MainWindow.FileDialog('Open Map', URL, true, 'Tiled Map (*.tmx)|*.tmx|All Files|*') then
    TiledMap.URL := URL;
end;

procedure TStateMain.MapMotion(const Sender: TCastleUserInterface;
  const Event: TInputMotion; var Handled: Boolean);
begin
  if buttonLeft in Event.Pressed then
  begin
    TiledMap.Origin := TiledMap.Origin -
      (Event.Position - Event.OldPosition) / TiledMap.Scale;
    Handled := true;
  end;
end;

procedure TStateMain.MapPress(const Sender: TCastleUserInterface;
  const Event: TInputPressRelease; var Handled: Boolean);
const
  MinScale = 0.1;
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

procedure TStateMain.CheckboxSmoothScalingChange(Sender: TObject);
begin
  TiledMap.SmoothScaling := CheckboxSmoothScaling.Checked;
end;

procedure TStateMain.CheckboxSmoothScalingSafeBorderChange(Sender: TObject);
begin
  TiledMap.SmoothScalingSafeBorder := CheckboxSmoothScalingSafeBorder.Checked;
end;

end.
