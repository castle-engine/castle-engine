{ Main view, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  This template code is in public domain, unlike most other CGE code which
  is covered by BSD or LGPL (see https://castle-engine.io/license). }
unit GameView${MAIN_VIEW};

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse;

type
  { Main view, where most of the application logic takes place. }
  TView${MAIN_VIEW} = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  View${MAIN_VIEW}: TView${MAIN_VIEW};

implementation

uses SysUtils;

{ TView${MAIN_VIEW} ----------------------------------------------------------------- }

constructor TView${MAIN_VIEW}.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameview${MAIN_VIEW_LOWERCASE}.castle-user-interface';
end;

procedure TView${MAIN_VIEW}.Start;
begin
  inherited;
end;

procedure TView${MAIN_VIEW}.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TView${MAIN_VIEW}.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { This virtual method is executed when user presses
    a key, a mouse button, or touches a touch-screen.

    Note that each UI control has also events like OnPress and OnClick.
    These events can be used to handle the "press", if it should do something
    specific when used in that UI control.
    The TView${MAIN_VIEW}.Press method should be used to handle keys
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
