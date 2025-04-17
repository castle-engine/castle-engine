{ Main "playing game" view, where most of the game logic takes place.

  Feel free to use this code as a starting point for your own projects.
  This template code is in public domain, unlike most other CGE code which
  is covered by BSD or LGPL (see https://castle-engine.io/license). }
unit GameViewPlay;

interface

uses Classes,
  CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleViewport, CastleScene, CastleVectors, CastleCameras,
  CastleTransform,
  GameEnemy;

type
  { Main "playing game" view, where most of the game logic takes place. }
  TViewPlay = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    MainViewport: TCastleViewport;
    WalkNavigation: TCastleWalkNavigation;
    SceneEnemy1, SceneEnemy2, SceneEnemy3, SceneEnemy4: TCastleScene;
    SimpleCrosshair: TCastleCrosshair;
  private
    { Enemies behaviors }
    Enemies: TEnemyList;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewPlay: TViewPlay;

implementation

uses SysUtils, Math,
  CastleSoundEngine, CastleLog, CastleStringUtils, CastleFilesUtils,
  GameViewMenu, GameSound;

{ TViewPlay ----------------------------------------------------------------- }

constructor TViewPlay.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewplay.castle-user-interface';
end;

procedure TViewPlay.Start;

  procedure InitializeEnemy(const SceneEnemy: TCastleScene);
  var
    Enemy: TEnemy;
  begin
    { Below using nil as Owner of TEnemy, as the Enemies list already "owns"
      instances of this class, i.e. it will free them. }
    Enemy := TEnemy.Create(nil);
    SceneEnemy.AddBehavior(Enemy);
    Enemies.Add(Enemy);
  end;

begin
  inherited;

  { Create TEnemy instances, add them to Enemies list }
  Enemies := TEnemyList.Create(true);
  InitializeEnemy(SceneEnemy1);
  InitializeEnemy(SceneEnemy2);
  InitializeEnemy(SceneEnemy3);
  InitializeEnemy(SceneEnemy4);
end;

procedure TViewPlay.Stop;
begin
  FreeAndNil(Enemies);
  inherited;
end;

procedure TViewPlay.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TViewPlay.Press(const Event: TInputPressRelease): Boolean;
var
  HitTransform: TCastleTransform;
  HitEnemy: TEnemy;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { This virtual method is executed when user presses
    a key, a mouse button, or touches a touch-screen.

    Note that each UI control has also events like OnPress and OnClick.
    These events can be used to handle the "press", if it should do something
    specific when used in that UI control.
    The TViewPlay.Press method should be used to handle keys
    not handled in children controls.
  }

  if Event.IsMouseButton(buttonLeft) then
  begin
    SoundEngine.Play(NamedSound('Shoot'));

    { We clicked on enemy if
      - TransformUnderMouse indicates we hit something
      - It has a behavior of TEnemy.

      Note: TransformUnderMouse checks the TCastleTransform picked
      by the ray cast from the mouse pointer.
      When WalkNavigation.MouseLook is true (mouse cursor is hidden then)
      then it checks from the screen center.
      If you want to always make ray cast from the screen center,
      use "HitTransform := MainViewport.TransformHit(MainViewport.RenderRect.Center, true);" . }

    HitTransform := MainViewport.TransformUnderMouse;
    if (HitTransform <> nil) and
       (HitTransform.FindBehavior(TEnemy) <> nil) then
    begin
      HitEnemy := HitTransform.FindBehavior(TEnemy) as TEnemy;
      HitEnemy.Hurt;
    end;

    Exit(true);
  end;

  if Event.IsMouseButton(buttonRight) then
  begin
    WalkNavigation.MouseLook := not WalkNavigation.MouseLook;

    { This game allows to toggle WalkNavigation.MouseLook at run-time,
      which changes:
      - how can user rotate the camera (with mouse look -- just move the mouse
        around),
      - whether the mouse cursor is visible (mouse cursor is hidden when
        "mouse look" is active, as we internally reposition it to ~roughly
        the middle of the viewport all the time),
      - what the MainViewport.TransformUnderMouse queries
        (with mouse look -- it always makes a raycast from the exact middle of
        the viewport).

      The crosshair, at the middle of the screen, is thus useful
      (and even necessary) only when "mouse look" is active.

      It real games, it is usually simpler, because
      - You either use "mouse look" always, or never, during the game.
      - And then you have a crosshair always visible, or never visible.
      Just for the sake of in this demo we wanted to show both options. }
    SimpleCrosshair.Exists := WalkNavigation.MouseLook;

    Exit(true);
  end;

  if Event.IsKey(keyF5) then
  begin
    Container.SaveScreenToDefaultFile;
    Exit(true);
  end;

  if Event.IsKey(keyEscape) then
  begin
    Container.View := ViewMenu;
    Exit(true);
  end;
end;

end.
