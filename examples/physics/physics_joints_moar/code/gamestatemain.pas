{ Main state, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  (This code is in public domain, unlike most other CGE code which
  is covered by the LGPL license variant, see the COPYING.txt file.) }
unit GameStateMain;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleViewport;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ButtonHinge:TCastleButton;
    ButtonBall:TCastleButton;
    ButtonGrab:TCastleButton;
    ButtonRope:TCastleButton;
    ButtonDistance:TCastleButton;
    ViewportHinge:TCastleViewport;
    ViewportBall:TCastleViewport;
    ViewportGrab:TCastleViewport;
    ViewportRope:TCastleViewport;
    ViewportDistance:TCastleViewport;

    procedure ClickButton(Sender: TObject);

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

procedure TStateMain.ClickButton(Sender: TObject);
begin
  ViewportBall.Exists := (Sender = ButtonBall);
  ViewportDistance.Exists := (Sender = ButtonDistance);
  ViewportGrab.Exists := (Sender = ButtonGrab);
  ViewportHinge.Exists := (Sender = ButtonHinge);
  ViewportRope.Exists := (Sender = ButtonRope);
end;

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;
begin
  inherited;

  ButtonBall.OnClick := {$ifdef FPC}@{$endif}ClickButton;
  ButtonDistance.OnClick := {$ifdef FPC}@{$endif}ClickButton;
  ButtonGrab.OnClick := {$ifdef FPC}@{$endif}ClickButton;
  ButtonHinge.OnClick := {$ifdef FPC}@{$endif}ClickButton;
  ButtonRope.OnClick := {$ifdef FPC}@{$endif}ClickButton;
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
