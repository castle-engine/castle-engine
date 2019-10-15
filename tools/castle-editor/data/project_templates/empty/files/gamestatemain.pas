{ Main user interface class.
  This implements the majority of this application functionality.

  Feel free to use this code as a starting point for your own projects.
  (This code is in public domain, unlike most other CGE code which
  is covered by the LGPL license variant, see the COPYING.txt file.) }
unit GameStateMain;

interface

uses Classes,
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse;

type
  { Main user interface class.
    This implements the majority of this application functionality. }
  TStateMain = class(TUIState)
  private
    ClickCount: Cardinal;
    Button1: TCastleButton;
    Label1, LabelFps: TCastleLabel;
    procedure Button1Click(Sender: TObject);
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils,
  CastleWindow;

{ TStateMain ----------------------------------------------------------------- }

procedure TStateMain.Start;
var
  UiOwner: TComponent;
begin
  inherited;

  { Load designed user interface }
  InsertUserInterface('castle-data:/state_main.castle-user-interface', FreeAtStop, UiOwner);

  { Find a label to show frames per second information }
  LabelFps := UiOwner.FindRequiredComponent('LabelFps') as TCastleLabel;

  { Find a button, and assign OnClick handler }
  Button1 := UiOwner.FindRequiredComponent('Button1') as TCastleButton;
  Button1.OnClick := @Button1Click;

  { Find another label (will be used by Button1Click) }
  Label1 := UiOwner.FindRequiredComponent('Label1') as TCastleLabel;
end;

procedure TStateMain.Button1Click(Sender: TObject);
begin
  Inc(ClickCount);
  Label1.Caption := Format('You clicked the button %d times', [ClickCount]);
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
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

  if Event.IsKey(keyEscape) then
  begin
    Application.Terminate;
    Exit(true); // key was handled
  end;
end;

end.
