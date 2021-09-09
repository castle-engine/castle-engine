{ Main state, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  (This code is in public domain, unlike most other CGE code which
  is covered by the LGPL license variant, see the COPYING.txt file.) }
unit GameState${MAIN_STATE};

interface

uses Classes,
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse;

type
  { Main state, where most of the application logic takes place. }
  TState${MAIN_STATE} = class(TUIState)
  private
    { Components designed using CGE editor, loaded from gamestate${MAIN_STATE_LOWERCASE}.castle-user-interface. }
    LabelFps: TCastleLabel;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  State${MAIN_STATE}: TState${MAIN_STATE};

implementation

uses SysUtils;

{ TState${MAIN_STATE} ----------------------------------------------------------------- }

constructor TState${MAIN_STATE}.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestate${MAIN_STATE_LOWERCASE}.castle-user-interface';
end;

procedure TState${MAIN_STATE}.Start;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
end;

procedure TState${MAIN_STATE}.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TState${MAIN_STATE}.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { This virtual method is executed when user presses
    a key, a mouse button, or touches a touch-screen.

    Note that each UI control has also events like OnPress and OnClick.
    These events can be used to handle the "press", if it should do something
    specific when used in that UI control.
    The TState${MAIN_STATE}.Press method should be used to handle keys
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
