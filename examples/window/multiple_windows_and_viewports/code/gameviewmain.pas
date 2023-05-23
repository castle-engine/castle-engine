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
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleTransform,
  CastleViewport, CastleCameras;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ViewportTop, ViewportBottom: TCastleViewport;
    WalkNavigationTop, WalkNavigationBottom: TCastleWalkNavigation;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMainInWindow1: TViewMain;
  ViewMainInWindow2: TViewMain;

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
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  WalkNavigationBottom.MouseLook :=
    (buttonRight in Container.MousePressed) and ViewportBottom.Focused;
  WalkNavigationTop.MouseLook :=
    (buttonRight in Container.MousePressed) and ViewportTop.Focused;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;

  procedure DropFromViewport(const Viewport: TCastleViewport);
  var
    Box: TCastleTransform;
    CamPos, CamDir, CamUp: TVector3;
  begin
    Box := TransformLoad('castle-data:/drop_box.castle-transform', FreeAtStop);
    Viewport.Camera.GetWorldView(CamPos, CamDir, CamUp);
    Box.Translation := CamPos + CamDir * 10.0;
    Box.Direction := CamDir;
    Viewport.Items.Add(Box);
  end;

begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { We check Container.Focused and viewport's Focused,
    to only drop from 1 viewport in focused window. }
  if Event.IsKey(keyEnter) and Container.Focused then
  begin
    if ViewportBottom.Focused then
      DropFromViewport(ViewportBottom)
    else
    if ViewportTop.Focused then
      DropFromViewport(ViewportTop);
    Exit(true); // key was handled
  end;
end;

end.
