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
  CastleVectors, CastleComponentSerialize, CastleCameras,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleViewport,
  CastleTransform, CastleScene;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    MainWalkNavigation: TCastleWalkNavigation;
    MainViewport: TCastleViewport;
    CheckboxOcclusionQuery: TCastleCheckbox;
    Buildings, Creatures: TCastleTransform;
  private
    UseOcclusionQuery: Boolean;
    { Set RenderOptions.OcclusionQuery to our UseOcclusionQuery for all scenes of buildings and creatures. }
    procedure UpdateOcclusionQuery;
    procedure ChangeOcclusionQuery(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleUtils, CastleStringUtils;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  CheckboxOcclusionQuery.OnChange := {$ifdef FPC}@{$endif} ChangeOcclusionQuery;

  { initialize state of everything to use occlusion query }
  UseOcclusionQuery := true;
  CheckboxOcclusionQuery.Checked := UseOcclusionQuery;
  UpdateOcclusionQuery;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption :=
    'FPS: ' + Container.Fps.ToString + NL +
    'Rendered: ' + MainViewport.Statistics.ToString;
end;

procedure TViewMain.UpdateOcclusionQuery;
var
  T: TCastleTransform;
begin
  for T in Buildings do
    if T is TCastleScene then
      TCastleScene(T).RenderOptions.OcclusionQuery := UseOcclusionQuery;
  for T in Creatures do
    if T is TCastleScene then
      TCastleScene(T).RenderOptions.OcclusionQuery := UseOcclusionQuery;
end;

procedure TViewMain.ChangeOcclusionQuery(Sender: TObject);
begin
  UseOcclusionQuery := CheckboxOcclusionQuery.Checked;
  UpdateOcclusionQuery;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(CtrlO) then
  begin
    UseOcclusionQuery := not UseOcclusionQuery;
    CheckboxOcclusionQuery.Checked := UseOcclusionQuery;
    UpdateOcclusionQuery;
    Exit(true);
  end;

  if Event.IsMouseButton(buttonRight) then
  begin
    MainWalkNavigation.MouseLook := not MainWalkNavigation.MouseLook;
    Exit(true);
  end;
end;

end.
