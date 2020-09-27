{ Main state, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  (This code is in public domain, unlike most other CGE code which
  is covered by the LGPL license variant, see the COPYING.txt file.) }
unit GameStateMain;

interface

uses Classes,
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleViewport, CastleScene;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from state_main.castle-user-interface. }
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
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils,
  CastleWindow, X3DLoad;

{ TStateMain ----------------------------------------------------------------- }

procedure TStateMain.Start;
var
  UiOwner: TComponent;
begin
  inherited;

  { Load designed user interface }
  InsertUserInterface('castle-data:/state_main.castle-user-interface', FreeAtStop, UiOwner);

  { Find components, by name, that we need to access from code }
  LabelFps := UiOwner.FindRequiredComponent('LabelFps') as TCastleLabel;
  LabelLoadedUrl := UiOwner.FindRequiredComponent('LabelLoadedUrl') as TCastleLabel;
  ButtonLoadKnight := UiOwner.FindRequiredComponent('ButtonLoadKnight') as TCastleButton;
  ButtonLoadCar := UiOwner.FindRequiredComponent('ButtonLoadCar') as TCastleButton;
  ButtonLoadCustom := UiOwner.FindRequiredComponent('ButtonLoadCustom') as TCastleButton;
  ButtonPlayAnimation := UiOwner.FindRequiredComponent('ButtonPlayAnimation') as TCastleButton;
  ButtonStopAnimation := UiOwner.FindRequiredComponent('ButtonStopAnimation') as TCastleButton;
  Viewport := UiOwner.FindRequiredComponent('Viewport') as TCastleViewport;
  SceneMain := UiOwner.FindRequiredComponent('SceneMain') as TCastleScene;

  { Assign OnClick handler to buttons }
  ButtonLoadKnight.OnClick := @ClickLoadKnight;
  ButtonLoadCar.OnClick := @ClickLoadCar;
  ButtonLoadCustom.OnClick := @ClickLoadCustom;
  ButtonPlayAnimation.OnClick := @ClickPlayAnimation;
  ButtonStopAnimation.OnClick := @ClickStopAnimation;

  { Although knight.gltf is already loaded at design-time,
    load it again, to always initialize all UI (e.g. LabelLoadedUrl.Caption)
    using our Load method. }
  Load('castle-data:/knight/knight.gltf');
end;

procedure TStateMain.Load(const Url: String);
begin
  SceneMain.Load(Url);
  LabelLoadedUrl.Caption := 'Loaded: ' + Url;
  Viewport.AssignDefaultCamera;
  Viewport.AssignDefaultNavigation;
end;

procedure TStateMain.ClickLoadKnight(Sender: TObject);
begin
  { Note that you load here any filename or URL (file://, http:// etc.).
    - See https://castle-engine.io/manual_network.php about CGE supported URLs.
    - See https://castle-engine.io/manual_data_directory.php about the special
      URL protocol "castle-data:/" }
  Load('castle-data:/knight/knight.gltf');
end;

procedure TStateMain.ClickLoadCar(Sender: TObject);
begin
  Load('castle-data:/car/car.x3d');
end;

procedure TStateMain.ClickLoadCustom(Sender: TObject);
var
  Url: String;
begin
  Url := SceneMain.Url;
  if Application.MainWindow.FileDialog('Open Model', Url, true, LoadScene_FileFilters) then
    Load(Url);
end;

procedure TStateMain.ClickPlayAnimation(Sender: TObject);
begin
  { Play the 1st animation in the scene.
    In an actual game, you usually hardcode the animation name to play.
    In a full 3D model viewer, you can display all known animations,
    and allow user to start the chosen one. }
  if SceneMain.AnimationsList.Count > 0 then
    SceneMain.PlayAnimation(SceneMain.AnimationsList[0], true);
end;

procedure TStateMain.ClickStopAnimation(Sender: TObject);
begin
  SceneMain.StopAnimation;
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

  // when pressing Home, reset the camera, to view complete model
  if Event.IsKey(keyHome) then
  begin
    Viewport.AssignDefaultCamera;
    Exit(true); // key was handled
  end;
end;

end.
