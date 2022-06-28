{ Main state, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  (This code is in public domain, unlike most other CGE code which
  is covered by the LGPL license variant, see the COPYING.txt file.) }
unit GameStateMain;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleViewport, CastleTransform;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
    LabelFps: TCastleLabel;
    BoxRopeStartPoint, BoxRopeEndPoint: TCastleTransform;
    BoxHinge: TCastleTransform;
    SphereHinge: TCastleTransform;
    SphereFixed: TCastleTransform;
    BoxFixed: TCastleTransform;

    SphereBall: TCastleTransform;
    BoxBall: TCastleTransform;

    SphereDistance: TCastleTransform;
    BoxDistance: TCastleTransform;

    Viewport: TCastleViewport;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;
var
  J:TJointTest;
  JHinge: TJointHinge;
  JRope: TJointRope;
  JFixed: TJointFixed;
  JBall: TJointBall;
  JDistance: TJointDistance;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  Viewport := DesignedComponent('Viewport') as TCastleViewport;

  BoxRopeStartPoint := DesignedComponent('BoxRopeStartPoint') as TCastleTransform;
  BoxRopeEndPoint := DesignedComponent('BoxRopeEndPoint') as TCastleTransform;

  JRope := TJointRope.Create(Self);
  JRope.SecondTransform := BoxRopeEndPoint;
  JRope.Distance := 300;
  BoxRopeStartPoint.AddBehavior(JRope);

  BoxHinge := DesignedComponent('BoxHinge') as TCastleTransform;
  SphereHinge := DesignedComponent('SphereHinge') as TCastleTransform;

  JHinge := TJointHinge.Create(Self);
  JHinge.SecondTransform := BoxHinge;
  SphereHinge.AddBehavior(JHinge);

  SphereFixed := DesignedComponent('SphereFixed') as TCastleTransform;
  BoxFixed := DesignedComponent('BoxFixed') as TCastleTransform;

  JFixed := TJointFixed.Create(Self);
  JFixed.SecondTransform := BoxFixed;
  SphereFixed.AddBehavior(JFixed);

  SphereBall := DesignedComponent('SphereBall') as TCastleTransform;
  BoxBall := DesignedComponent('BoxBall') as TCastleTransform;

  JBall := TJointBall.Create(Self);
  JBall.SecondTransform := BoxBall;
  SphereBall.AddBehavior(JBall);

  SphereDistance := DesignedComponent('SphereDistance') as TCastleTransform;
  BoxDistance := DesignedComponent('BoxDistance') as TCastleTransform;

  JDistance := TJointDistance.Create(Self);
  JBall.SecondTransform := BoxDistance;
  SphereDistance.AddBehavior(JBall);

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
