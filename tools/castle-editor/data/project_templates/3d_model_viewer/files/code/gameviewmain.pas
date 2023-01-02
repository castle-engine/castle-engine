{ Main view, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  This template code is in public domain, unlike most other CGE code which
  is covered by BSD or LGPL (see https://castle-engine.io/license). }
unit GameView${MAIN_VIEW};

interface

uses Classes,
  CastleVectors, CastleComponentSerialize, CastleUIControls,
  CastleControls, CastleKeysMouse, CastleViewport, CastleScene;

type
  { Main view, where most of the application logic takes place. }
  TView${MAIN_VIEW} = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    Viewport: TCastleViewport;
    SceneMain: TCastleScene;
    ButtonLoadKnight: TCastleButton;
    ButtonLoadCar: TCastleButton;
    ButtonLoadCustom: TCastleButton;
    ButtonPlayAnimation: TCastleButton;
    ButtonStopAnimation: TCastleButton;
    LabelLoadedUrl, LabelFps: TCastleLabel;
  private
    procedure Load(const Url: String);
    { Methods assigned to handle buttons' OnClick events. }
    procedure ClickLoadKnight(Sender: TObject);
    procedure ClickLoadCar(Sender: TObject);
    procedure ClickLoadCustom(Sender: TObject);
    procedure ClickPlayAnimation(Sender: TObject);
    procedure ClickStopAnimation(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  View${MAIN_VIEW}: TView${MAIN_VIEW};

implementation

uses SysUtils,
  CastleWindow, X3DLoad;

{ TView${MAIN_VIEW} ----------------------------------------------------------------- }

constructor TView${MAIN_VIEW}.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameview${MAIN_VIEW_LOWERCASE}.castle-user-interface';
end;

procedure TView${MAIN_VIEW}.Start;
begin
  inherited;

  { Assign OnClick handler to buttons }
  ButtonLoadKnight.OnClick := {$ifdef FPC}@{$endif} ClickLoadKnight;
  ButtonLoadCar.OnClick := {$ifdef FPC}@{$endif} ClickLoadCar;
  ButtonLoadCustom.OnClick := {$ifdef FPC}@{$endif} ClickLoadCustom;
  ButtonPlayAnimation.OnClick := {$ifdef FPC}@{$endif} ClickPlayAnimation;
  ButtonStopAnimation.OnClick := {$ifdef FPC}@{$endif} ClickStopAnimation;

  { Although knight.gltf is already loaded at design-time,
    load it again, to always initialize all UI (e.g. LabelLoadedUrl.Caption)
    using our Load method. }
  Load('castle-data:/knight/knight.gltf');
end;

procedure TView${MAIN_VIEW}.Load(const Url: String);
begin
  SceneMain.Load(Url);
  LabelLoadedUrl.Caption := 'Loaded: ' + Url;
  Viewport.AssignDefaultCamera;
end;

procedure TView${MAIN_VIEW}.ClickLoadKnight(Sender: TObject);
begin
  { Note that you load here any filename or URL (file://, http:// etc.).
    - See https://castle-engine.io/manual_network.php about CGE supported URLs.
    - See https://castle-engine.io/manual_data_directory.php about the special
      URL protocol "castle-data:/" }
  Load('castle-data:/knight/knight.gltf');
end;

procedure TView${MAIN_VIEW}.ClickLoadCar(Sender: TObject);
begin
  Load('castle-data:/car/car.x3d');
end;

procedure TView${MAIN_VIEW}.ClickLoadCustom(Sender: TObject);
var
  Url: String;
begin
  Url := SceneMain.Url;
  if Application.MainWindow.FileDialog('Open Model', Url, true, LoadScene_FileFilters) then
    Load(Url);
end;

procedure TView${MAIN_VIEW}.ClickPlayAnimation(Sender: TObject);
begin
  { Play the 1st animation in the scene.
    In an actual game, you usually hardcode the animation name to play.
    In a full 3D model viewer, you can display all known animations,
    and allow user to start the chosen one. }
  if SceneMain.AnimationsList.Count > 0 then
    SceneMain.PlayAnimation(SceneMain.AnimationsList[0], true);
end;

procedure TView${MAIN_VIEW}.ClickStopAnimation(Sender: TObject);
begin
  SceneMain.StopAnimation;
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

  // when pressing Home, reset the camera, to view complete model
  if Event.IsKey(keyHome) then
  begin
    Viewport.AssignDefaultCamera;
    Exit(true); // key was handled
  end;
end;

end.
