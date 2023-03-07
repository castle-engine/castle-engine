{
  Copyright 2022-2023 Michalis Kamburelis, Andrzej Kilijański.

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
  CastleUIControls, CastleControls, CastleKeysMouse, CastleTransform, CastleViewport;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ButtonScaleRedBox: TCastleButton;
    ButtonLocalScaleRedBoxCollider: TCastleButton;
    ButtonScaleGreenSphere: TCastleButton;
    ButtonScaleAll: TCastleButton;
    RedBox: TCastleTransform;
    GreenSphere: TCastleTransform;
    TransformAll: TCastleTransform;
    Viewport: TCastleViewport;
  private
    procedure ClickScaleAll(Sender: TObject);
    procedure ClickScaleGreenSphere(Sender: TObject);
    procedure ClickScaleRedBox(Sender: TObject);
    procedure ClickLocalScaleRedBoxCollider(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils;

{ TViewMain ----------------------------------------------------------------- }

procedure TViewMain.ClickScaleAll(Sender: TObject);
begin
  //TransformAll.Scale := Vector3(0.5, 0.5, 0.5);
  TransformAll.Scale := Vector3(1.5, 1.5, 1.5);
end;

procedure TViewMain.ClickScaleGreenSphere(Sender: TObject);
begin
  GreenSphere.Scale := Vector3(3,3,3);
end;

procedure TViewMain.ClickScaleRedBox(Sender: TObject);
begin
  //RedBox.Scale := Vector3(0.9,0.3,0.4);
  //RedBox.Scale := Vector3(3,3,3);
  //RedBox.Scale := Vector3(0.6,0.3,0.3);
  RedBox.Scale := Vector3(3,3,3);
  //RedBox.Scale := Vector3(0.6,5.3,0.3);
end;

procedure TViewMain.ClickLocalScaleRedBoxCollider(Sender: TObject);
var
  Collider: TCastleCollider;
begin
  Collider := RedBox.FindBehavior(TCastleCollider) as TCastleCollider;
  if Collider <> nil then
    Collider.SizeScale := 0.5;
end;

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  ButtonScaleAll.OnClick := {$ifdef FPC}@{$endif}ClickScaleAll;
  ButtonScaleGreenSphere.OnClick := {$ifdef FPC}@{$endif}ClickScaleGreenSphere;
  ButtonScaleRedBox.OnClick := {$ifdef FPC}@{$endif}ClickScaleRedBox;
  ButtonLocalScaleRedBoxCollider.OnClick := {$ifdef FPC}@{$endif}ClickLocalScaleRedBoxCollider;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { This virtual method is executed when user presses
    a key, a mouse button, or touches a touch-screen.

    Note that each UI control has also events like OnPress and OnClick.
    These events can be used to handle the "press", if it should do something
    specific when used in that UI control.
    The TViewMain.Press method should be used to handle keys
    not handled in children controls.
  }

  // Use this to handle keys:
  {
  if Event.IsKey(keyXxx) then
  begin
    // DoSomething;
    Exit(true); // key was handled
  end;
  }
end;

end.
