{ Main state, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  (This code is in public domain, unlike most other CGE code which
  is covered by the LGPL license variant, see the COPYING.txt file.) }
unit GameState${MAIN_STATE};

interface

uses Classes,
  CastleVectors, CastleUIState, CastleComponentSerialize, CastleUIControls,
  CastleControls, CastleKeysMouse, CastleViewport, CastleScene;

type
  { Main state, where most of the application logic takes place. }
  TState${MAIN_STATE} = class(TUIState)
  private
    { Components designed using CGE editor, loaded from gamestate${MAIN_STATE_LOWERCASE}.castle-user-interface. }
    Viewport: TCastleViewport;
    SceneMain: TCastleScene;
    ButtonLoadKnight: TCastleButton;
    ButtonLoadCar: TCastleButton;
    ButtonLoadCustom: TCastleButton;
    ButtonPlayAnimation: TCastleButton;
    ButtonStopAnimation: TCastleButton;
    LabelLoadedUrl, LabelFps: TCastleLabel;

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
  State${MAIN_STATE}: TState${MAIN_STATE};

implementation

uses SysUtils,
  CastleWindow, X3DLoad;

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
  LabelLoadedUrl := DesignedComponent('LabelLoadedUrl') as TCastleLabel;
  ButtonLoadKnight := DesignedComponent('ButtonLoadKnight') as TCastleButton;
  ButtonLoadCar := DesignedComponent('ButtonLoadCar') as TCastleButton;
  ButtonLoadCustom := DesignedComponent('ButtonLoadCustom') as TCastleButton;
  ButtonPlayAnimation := DesignedComponent('ButtonPlayAnimation') as TCastleButton;
  ButtonStopAnimation := DesignedComponent('ButtonStopAnimation') as TCastleButton;
  Viewport := DesignedComponent('Viewport') as TCastleViewport;
  SceneMain := DesignedComponent('SceneMain') as TCastleScene;

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

procedure TState${MAIN_STATE}.Load(const Url: String);
begin
  SceneMain.Load(Url);
  LabelLoadedUrl.Caption := 'Loaded: ' + Url;
  Viewport.AssignDefaultCamera;
end;

procedure TState${MAIN_STATE}.ClickLoadKnight(Sender: TObject);
begin
  { Note that you load here any filename or URL (file://, http:// etc.).
    - See https://castle-engine.io/manual_network.php about CGE supported URLs.
    - See https://castle-engine.io/manual_data_directory.php about the special
      URL protocol "castle-data:/" }
  Load('castle-data:/knight/knight.gltf');
end;

procedure TState${MAIN_STATE}.ClickLoadCar(Sender: TObject);
begin
  Load('castle-data:/car/car.x3d');
end;

procedure TState${MAIN_STATE}.ClickLoadCustom(Sender: TObject);
var
  Url: String;
begin
  Url := SceneMain.Url;
  if Application.MainWindow.FileDialog('Open Model', Url, true, LoadScene_FileFilters) then
    Load(Url);
end;

procedure TState${MAIN_STATE}.ClickPlayAnimation(Sender: TObject);
begin
  { Play the 1st animation in the scene.
    In an actual game, you usually hardcode the animation name to play.
    In a full 3D model viewer, you can display all known animations,
    and allow user to start the chosen one. }
  if SceneMain.AnimationsList.Count > 0 then
    SceneMain.PlayAnimation(SceneMain.AnimationsList[0], true);
end;

procedure TState${MAIN_STATE}.ClickStopAnimation(Sender: TObject);
begin
  SceneMain.StopAnimation;
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

  // when pressing Home, reset the camera, to view complete model
  if Event.IsKey(keyHome) then
  begin
    Viewport.AssignDefaultCamera;
    Exit(true); // key was handled
  end;
end;

end.
