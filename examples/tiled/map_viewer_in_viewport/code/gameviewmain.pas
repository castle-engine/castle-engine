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
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    TiledScene: TCastleScene;
    ButtonOpen: TCastleButton;
    CheckboxSmoothScaling: TCastleCheckbox;
    MapCamera: TCastleCamera;
  private
    procedure ClickOpen(Sender: TObject);
    procedure CheckboxSmoothScalingChange(Sender: TObject);
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
  CastleParameters, CastleRenderOptions, CastleWindow;

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
  CheckboxSmoothScaling.OnChange := {$ifdef FPC}@{$endif} CheckboxSmoothScalingChange;

  { Synchronize TiledScene.RenderOptions.XxxFilter with initial checkbox state }
  CheckboxSmoothScalingChange(nil);

  { Load the map from parameter or default. }
  if Parameters.High = 1 then
    OpenMap(Parameters[1])
  else
    OpenMap('castle-data:/maps/desert.tmx');
end;

procedure TViewMain.OpenMap(const MapUrl: String);
begin
  TiledScene.Url := MapUrl;
  MapCamera.Translation := TVector3.Zero;
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
  Url := TiledScene.Url;
  if Application.MainWindow.FileDialog('Open Map', Url, true, 'Tiled Map (*.tmx)|*.tmx|All Files|*') then
    OpenMap(Url);
end;

procedure TViewMain.CheckboxSmoothScalingChange(Sender: TObject);
begin
  if CheckboxSmoothScaling.Checked then
  begin
    TiledScene.RenderOptions.MinificationFilter := minLinear;
    TiledScene.RenderOptions.MagnificationFilter := magLinear;
  end else
  begin
    TiledScene.RenderOptions.MinificationFilter := minNearest;
    TiledScene.RenderOptions.MagnificationFilter := magNearest;
  end;
end;

end.
