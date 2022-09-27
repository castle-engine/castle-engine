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
//  DesignUrl := 'castle-data:/2d_joint_distance_example.castle-user-interface';
//  DesignUrl := 'castle-data:/2d_joint_distance_spring_example.castle-user-interface';
//  DesignUrl := 'castle-data:/2d_joint_fixed_example.castle-user-interface';
  DesignUrl := 'castle-data:/2d_joint_fixed_example_small_width.castle-user-interface';
//  DesignUrl := 'castle-data:/2d_joint_grab_example.castle-user-interface';
//  DesignUrl := 'castle-data:/2d_joint_hinge_break_force_example.castle-user-interface';
//  DesignUrl := 'castle-data:/2d_joint_hinge_car.castle-user-interface';
//  DesignUrl := 'castle-data:/2d_joint_hinge_limits_example.castle-user-interface';
//  DesignUrl := 'castle-data:/2d_joint_hinge_simple_example.castle-user-interface';
//  DesignUrl := 'castle-data:/2d_joint_plane_example.castle-user-interface';
//  DesignUrl := 'castle-data:/2d_joint_pulley_example.castle-user-interface';
//  DesignUrl := 'castle-data:/2d_joint_rope_chain.castle-user-interface';
//  DesignUrl := 'castle-data:/2d_joint_rope_simple.castle-user-interface';
//  DesignUrl := 'castle-data:/2d_joint_slide_example.castle-user-interface';
//  DesignUrl := 'castle-data:/3d_joint_fixed_example.castle-user-interface';
//  DesignUrl := 'castle-data:/3d_joint_hinge_door_break_force_example.castle-user-interface';
//  DesignUrl := 'castle-data:/3d_joint_hinge_door_example.castle-user-interface';
//  DesignUrl := 'castle-data:/3d_joint_pulley_example.castle-user-interface';
//  DesignUrl := 'castle-data:/3d_joint_rope_example.castle-user-interface';
//  DesignUrl := 'castle-data:/3d_joint_slide_example.castle-user-interface';
end;

procedure TStateMain.Start;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  //LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  //Viewport := DesignedComponent('Viewport') as TCastleViewport;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  //LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
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
