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
  CastleVectors, CastleComponentSerialize, CastleTransform, CastleTiledMap,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    TiledMap: TCastleTiledMap;
    ButtonOpen, ButtonAnimations: TCastleButton;
    CheckboxSmoothScaling, CheckboxSmoothScalingSafeBorder: TCastleCheckbox;
    MapCamera: TCastleCamera;
  private
    TiledAnimations: Boolean;
    procedure ClickOpen(Sender: TObject);
    procedure ClickAnimations(Sender: TObject);
    procedure CheckboxSmoothScalingChange(Sender: TObject);
    procedure CheckboxSmoothScalingSafeBorderChange(Sender: TObject);
    procedure OpenMap(const MapUrl: String);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleParameters, CastleRenderOptions, CastleWindow, CastleUriUtils;

{ TViewMain ----------------------------------------------------------------- }

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
  ButtonAnimations.OnClick := {$ifdef FPC}@{$endif} ClickAnimations;
  CheckboxSmoothScaling.OnChange := {$ifdef FPC}@{$endif} CheckboxSmoothScalingChange;
  CheckboxSmoothScalingSafeBorder.OnChange := {$ifdef FPC}@{$endif} CheckboxSmoothScalingSafeBorderChange;

  { Synchronize initial checkbox state with map properties }
  CheckboxSmoothScaling.Checked := TiledMap.SmoothScaling ;
  CheckboxSmoothScalingSafeBorder.Checked := TiledMap.SmoothScalingSafeBorder;

  { Load the map from parameter or default. }
  if Parameters.High = 1 then
    OpenMap(Parameters[1])
  else
    OpenMap('castle-data:/maps/desert.tmx');
end;

procedure TViewMain.OpenMap(const MapUrl: String);
begin
  TiledMap.Url := MapUrl;
  MapCamera.Translation := TVector3.Zero;
  MapCamera.Orthographic.Height := 1000; // resets zoom in/out

  TiledAnimations := true;
  ButtonAnimations.Caption := 'Stop Animations';
  ButtonAnimations.Enabled := TiledMap.HasAnimations;
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
    OpenMap(Url);
end;

procedure TViewMain.ClickAnimations(Sender: TObject);
begin
  TiledAnimations := not TiledAnimations;
  if TiledAnimations then
  begin
    TiledMap.PlayAnimations;
    ButtonAnimations.Caption := 'Stop Animations';
  end else
  begin
    TiledMap.StopAnimations(false);
    ButtonAnimations.Caption := 'Play Animations';
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
