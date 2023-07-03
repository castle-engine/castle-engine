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
    CheckboxOcclusionCulling: TCastleCheckbox;
  private
    procedure ChangeOcclusionCulling(Sender: TObject);
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

  CheckboxOcclusionCulling.OnChange := {$ifdef FPC}@{$endif} ChangeOcclusionCulling;

  { initialize UI }
  CheckboxOcclusionCulling.Checked := MainViewport.OcclusionCulling;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption :=
    'FPS: ' + Container.Fps.ToString + NL +
    'Rendered: ' + MainViewport.Statistics.ToString;
end;

procedure TViewMain.ChangeOcclusionCulling(Sender: TObject);
begin
  MainViewport.OcclusionCulling := CheckboxOcclusionCulling.Checked;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(CtrlO) then
  begin
    MainViewport.OcclusionCulling := not MainViewport.OcclusionCulling;
    CheckboxOcclusionCulling.Checked := MainViewport.OcclusionCulling;
    Exit(true);
  end;

  if Event.IsMouseButton(buttonRight) then
  begin
    MainWalkNavigation.MouseLook := not MainWalkNavigation.MouseLook;
    Exit(true);
  end;
end;

end.
