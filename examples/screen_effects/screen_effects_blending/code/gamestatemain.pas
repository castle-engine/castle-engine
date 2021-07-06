{ Main state, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  (This code is in public domain, unlike most other CGE code which
  is covered by the LGPL license variant, see the COPYING.txt file.) }
unit GameStateMain;

interface

uses Classes,
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, X3DNodes, X3DLoad;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
    LabelFps: TCastleLabel;
    BurnRoot: TX3DRootNode;
    BurnEffect: TScreenEffectNode;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StateMain: TStateMain;

implementation

uses
  SysUtils,
  CastleScreenEffects, CastleImages, CastleVectors;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;


procedure TStateMain.Start;
var
  SimpleBackground: TCastleSimpleBackground;
  ScreenEffect: TCastleScreenEffects;
  ImageControl: TCastleImageControl;
begin
  inherited;
  { Create the screen effect and load appropriate shader }
  ScreenEffect := TCastleScreenEffects.Create(Self);
  ScreenEffect.FullSize := true;
  ScreenEffect.Blending := true;
  BurnRoot := LoadNode('castle-data:/shaders/burn.x3dv');
  BurnEffect := BurnRoot.FindNode('MyScreenEffect') as TScreenEffectNode;
  ScreenEffect.AddScreenEffect(BurnEffect);
  InsertFront(ScreenEffect);

  { SimpleBackground is required to clear the background buffer
    otherwise its content will be undefined }
  SimpleBackground := TCastleSimpleBackground.Create(Self);
  SimpleBackground.Color := Vector4(1, 0, 0, 0);
  SimpleBackground.FullSize := true;
  ScreenEffect.InsertFront(SimpleBackground);

  { Add two images, that cover each other (because they have opaque center)
    And after that the resulting image is made transparent through the shader }
  ImageControl := TCastleImageControl.Create(Self);
  ImageControl.HorizontalAnchorDelta := 128;
  ImageControl.VerticalAnchorDelta := 128;
  ImageControl.Url := 'castle-data:/cge1.png';
  ScreenEffect.InsertFront(ImageControl);

  ImageControl := TCastleImageControl.Create(Self);
  ImageControl.Url := 'castle-data:/cge1.png';
  ImageControl.HorizontalAnchorDelta := 256;
  ImageControl.VerticalAnchorDelta := 256;
  ScreenEffect.InsertFront(ImageControl);

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
end;

procedure TStateMain.Stop;
begin
  BurnRoot.Free;
  inherited;
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
