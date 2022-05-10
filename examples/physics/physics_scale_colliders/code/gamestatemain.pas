{ Main state, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  (This code is in public domain, unlike most other CGE code which
  is covered by the LGPL license variant, see the COPYING.txt file.) }
unit GameStateMain;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleTransform;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
    LabelFps: TCastleLabel;
    ButtonScaleRedBox: TCastleButton;
    ButtonScaleGreenSphere: TCastleButton;
    ButtonScaleAll: TCastleButton;
    RedBox: TCastleTransform;
    GreenSphere: TCastleTransform;
    TransformAll: TCastleTransform;

    procedure ClickScaleAll(Sender: TObject);
    procedure ClickScaleGreenSphere(Sender: TObject);
    procedure ClickScaleRedBox(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils;

{ TStateMain ----------------------------------------------------------------- }

procedure TStateMain.ClickScaleAll(Sender: TObject);
begin
  TransformAll.Scale := Vector3(0.5, 0.5, 0.5);
end;

procedure TStateMain.ClickScaleGreenSphere(Sender: TObject);
begin
  GreenSphere.Scale := Vector3(3,3,3);
end;

procedure TStateMain.ClickScaleRedBox(Sender: TObject);
begin
  RedBox.Scale := Vector3(3,1,1);
end;

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

  ButtonScaleAll := DesignedComponent('ButtonScaleAll') as TCastleButton;
  ButtonScaleAll.OnClick := {$ifdef FPC}@{$endif}ClickScaleAll;

  ButtonScaleGreenSphere := DesignedComponent('ButtonScaleGreenSphere') as TCastleButton;
  ButtonScaleGreenSphere.OnClick := {$ifdef FPC}@{$endif}ClickScaleGreenSphere;

  ButtonScaleRedBox := DesignedComponent('ButtonScaleRedBox') as TCastleButton;
  ButtonScaleRedBox.OnClick := {$ifdef FPC}@{$endif}ClickScaleRedBox;

  RedBox := DesignedComponent('RedBox') as TCastleTransform;
  GreenSphere := DesignedComponent('GreenSphere') as TCastleTransform;
  TransformAll := DesignedComponent('TransformAll') as TCastleTransform;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { This virtual method is executed when user presses
    a key, a mouse button, or touches a touch-screen.

    Note that each UI control has also events like OnPress and OnClick.
    These events can be used to handle the "press", if it should do something
    specific when used in that UI control.
    The TStateMain.Press method should be used to handle keys
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
