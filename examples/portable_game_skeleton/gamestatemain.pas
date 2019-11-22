{ Main user interface class.
  This implements the majority of this application functionality.

  This code is independent from mobile / standalone platforms.
  It will be used by the appropriate .lpr file for desktop, Android or iOS.

  Feel free to use this code as a starting point for your own projects.
  (This code is in public domain, unlike most other CGE code which
  is covered by the LGPL license variant, see the COPYING.txt file.)
}
unit GameStateMain;

interface

uses CastleUIState, CastleScene, CastleControls,
  CastleKeysMouse, CastleColors, CastleSceneManager, CastleUIControls;

type
  { Main user interface class.
    This implements the majority of this application functionality. }
  TStateMain = class(TUIState)
  private
    SceneManager: TCastleSceneManager;
    Status: TCastleLabel;
    ExampleImage: TCastleImageControl;
    ExampleScene: TCastleScene;
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StateMain: TStateMain;

implementation

{ TStateMain ----------------------------------------------------------------- }

procedure TStateMain.Start;
begin
  inherited;

  { Create scene manager to show 3D stuff (in TCastleScene) }
  SceneManager := TCastleSceneManager.Create(FreeAtStop);
  InsertFront(SceneManager);

  { Show a 3D object (TCastleScene) inside a Window.SceneManager
    (which acts as a full-screen viewport by default). }
  ExampleScene := TCastleScene.Create(FreeAtStop);
  ExampleScene.Load('castle-data:/example_scene.x3dv');
  ExampleScene.Spatial := [ssRendering, ssDynamicCollisions];
  ExampleScene.ProcessEvents := true;
  SceneManager.Items.Add(ExampleScene);
  SceneManager.MainScene := ExampleScene;

  { Show a label with frames per second information }
  Status := TCastleLabel.Create(FreeAtStop);
  Status.Anchor(vpTop, -10);
  Status.Anchor(hpRight, -10);
  Status.Color := Yellow; // you could also use "Vector4(1, 1, 0, 1)" instead of Yellow
  InsertFront(Status);

  { Show 2D image }
  ExampleImage := TCastleImageControl.Create(FreeAtStop);
  ExampleImage.URL := 'castle-data:/example_image.png';
  ExampleImage.Bottom := 100;
  ExampleImage.Left := 100;
  InsertFront(ExampleImage);
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  // ... do something every frame
  Status.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;
  // ... react to press of key, mouse, touch
end;

end.
