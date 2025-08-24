{ Main view, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  This template code is in public domain, unlike most other CGE code which
  is covered by BSD or LGPL (see https://castle-engine.io/license). }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize, CastleCameras,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene,
  CastleViewport, CastleLivingBehaviors;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    WalkNavigation1: TCastleWalkNavigation;
    BoxWin: TCastleBox;
    Viewport1: TCastleViewport;
    LabelLife: TCastleLabel;
    PlayerLiving: TCastleLiving;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleUtils, CastleTransform,
  GameViewWin, GameViewGameOver;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
  WalkNavigation1.MouseLook := buttonRight in Container.MousePressed;

  if BoxWin.WorldBoundingBox.Contains(Viewport1.Camera.WorldTranslation) then
  begin
    Container.View := ViewWin;
    Exit; // do not access view stuff that could be freed when stopping view
  end;

  if PlayerLiving.Dead then
  begin
    Container.View := ViewGameOver;
    Exit;
  end;

  LabelLife.Caption := FormatDot('Life: %f', [PlayerLiving.Life]);
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
var
  HitTransform: TCastleTransform;
  HitLiving: TCastleLiving;
  HitScene: TCastleScene;
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

  if Event.IsMouseButton(buttonLeft) then
  begin
    { We clicked on enemy if
      - MainViewport.TransformHit(...) indicates we hit something
      - It has a behavior of TCastleLiving.
      We check usign TransformHit what was hit at the center of the screen.
      This is consistent with us showing a crosshair in the center of the screen.
    }
    HitTransform := Viewport1.TransformHit(Viewport1.RenderRect.Center, true);
    if (HitTransform <> nil) and
       (HitTransform.FindBehavior(TCastleLiving) <> nil) then
    begin
      HitLiving := HitTransform.FindBehavior(TCastleLiving) as TCastleLiving;
      HitLiving.Hurt(1000, Viewport1.Camera.WorldDirection);
      if HitLiving.Dead then
      begin
        HitScene := HitTransform as TCastleScene;
        // dead corpse no longer collides
        HitScene.Pickable := false;
        HitScene.Collides := false;
      end;
    end;

    Exit(true);
  end;
end;

end.
