{ Main view, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  This template code is in public domain, unlike most other CGE code which
  is covered by BSD or LGPL (see https://castle-engine.io/license). }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start.
      TODO: No, for now this is created by hand. }
    LabelFps: TCastleLabel;
  private
    LifeTime: Double;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    procedure Render; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleGLUtils, CastleRectangles, CastleColors;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  // TODO: No file reading with WebGL
  // DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  LabelFps := TCastleLabel.Create(Self);
  LabelFps.Anchor(hpLeft, 5);
  LabelFps.Anchor(vpTop, -5);
  LabelFps.Color := Gray;
  InsertFront(LabelFps);
end;

procedure TViewMain.Render;
begin
  inherited;
  DrawCircle(
    Vector2(
      Container.PixelsWidth / 2 + 100 * Sin(LifeTime * 5),
      Container.PixelsHeight / 2 + 100 * Cos(LifeTime * 5)
    ),
    20, 20, Yellow
  );
  DrawRectangle(FloatRectangle(5, 5, 30, 30), Yellow);
  DrawRectangle(FloatRectangle(
    Container.PixelsWidth - 35,
    Container.PixelsHeight - 35,
    30,
    30), Blue);
  FallbackFont.Print(5, 40, Red, FormatDateTime('yyyy-mm-dd, hh:nn:ss', Now));
  FallbackFont.Print(5, 80, Yellow, 'Hello Castle Game Engine on the Web!');
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }

  LifeTime := LifeTime + SecondsPassed;

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
