{
  Copyright 2022-2022 Michalis Kamburelis.

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
  CastleUIControls, CastleControls, CastleKeysMouse, CastleViewport,
  CastleTransform;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ButtonHinge: TCastleButton;
    ButtonHingePrettier: TCastleButton;
    ButtonBall: TCastleButton;
    ButtonGrab: TCastleButton;
    ButtonRope: TCastleButton;
    ButtonDistance: TCastleButton;
    DesignContent: TCastleDesign;
  private
    // Only defined during Grab demo
    InteractiveGrabJoint: TCastleGrabJoint;
    ViewportGrab: TCastleViewport;

    procedure ClickButtonHinge(Sender: TObject);
    procedure ClickButtonHingePrettier(Sender: TObject);
    procedure ClickButtonBall(Sender: TObject);
    procedure ClickButtonGrab(Sender: TObject);
    procedure ClickButtonRope(Sender: TObject);
    procedure ClickButtonDistance(Sender: TObject);
    procedure DesignChanged;
    procedure UpdateGrabPosition(const EventPosition: TVector2);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    function Motion(const Event: TInputMotion): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleLog;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  ButtonHinge.OnClick := {$ifdef FPC}@{$endif} ClickButtonHinge;
  ButtonHingePrettier.OnClick := {$ifdef FPC}@{$endif} ClickButtonHingePrettier;
  ButtonBall.OnClick := {$ifdef FPC}@{$endif} ClickButtonBall;
  ButtonGrab.OnClick := {$ifdef FPC}@{$endif} ClickButtonGrab;
  ButtonRope.OnClick := {$ifdef FPC}@{$endif} ClickButtonRope;
  ButtonDistance.OnClick := {$ifdef FPC}@{$endif} ClickButtonDistance;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewMain.DesignChanged;
begin
  // These will be defined only in Grab demo, nil otherwise
  ViewportGrab := DesignContent.DesignedComponent('ViewportGrab', false) as TCastleViewport;
  InteractiveGrabJoint := DesignContent.DesignedComponent('InteractiveGrabJoint', false) as TCastleGrabJoint;
end;

procedure TViewMain.ClickButtonHinge(Sender: TObject);
begin
  { Each joint demo is in a separate file, and we switch between them
    by changing DesignContent.Url.

    This is in contrast to another solution:
    having multiple controls all in the gameviewmain.castle-user-interface
    design and switching their existence like ViewporHinge.Exists := true/false.

    Using the DesignContent.Url is better in this case because:

    - It means that each change of demo restarts this demo
      (e.g. go to "Hinge", then "Ball", then "Hinge" again -- the "Hinge"
      demo will start again). This is desirable in case of this demo.

    - It's a bit easier to design and test in CGE editor:
      each joint demo is separate, must use separate components,
      and you can run the simulation of it.
  }
  DesignContent.Url := 'castle-data:/viewport_hinge.castle-user-interface';
  DesignChanged;
end;

procedure TViewMain.ClickButtonHingePrettier(Sender: TObject);
begin
  DesignContent.Url := 'castle-data:/viewport_hinge_prettier.castle-user-interface';
  DesignChanged;
end;

procedure TViewMain.ClickButtonBall(Sender: TObject);
begin
  DesignContent.Url := 'castle-data:/viewport_ball.castle-user-interface';
  DesignChanged;
end;

procedure TViewMain.ClickButtonGrab(Sender: TObject);
begin
  DesignContent.Url := 'castle-data:/viewport_grab.castle-user-interface';
  DesignChanged;
end;

procedure TViewMain.ClickButtonRope(Sender: TObject);
begin
  DesignContent.Url := 'castle-data:/viewport_rope.castle-user-interface';
  DesignChanged;
end;

procedure TViewMain.ClickButtonDistance(Sender: TObject);
begin
  DesignContent.Url := 'castle-data:/viewport_distance.castle-user-interface';
  DesignChanged;
end;

procedure TViewMain.UpdateGrabPosition(const EventPosition: TVector2);
var
  PickedPoint: TVector3;
begin
  if InteractiveGrabJoint <> nil then
  begin
    if ViewportGrab.PositionToWorldPlane(EventPosition, true, 1, 0, PickedPoint) then
    begin
      //WritelnLog('Grabbing to point %s', [PickedPoint.ToString]);
      InteractiveGrabJoint.TargetWorld := PickedPoint;
    end;
  end;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsMouseButton(buttonLeft) and (mkShift in Event.ModifiersDown) then
  begin
    UpdateGrabPosition(Event.Position);
    Exit(true);
  end;
end;

function TViewMain.Motion(const Event: TInputMotion): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if (buttonLeft in Event.Pressed) and (mkShift in Container.Pressed.Modifiers) then
  begin
    UpdateGrabPosition(Event.Position);
    Exit(true);
  end;
end;

end.
