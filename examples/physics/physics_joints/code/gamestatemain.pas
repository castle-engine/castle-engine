{
  Copyright 2020-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main state, where most of the application logic takes place. }
unit GameStateMain;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleViewport, CastleTransform;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
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
  DesignUrl := 'castle-data:/2d_joint_distance_example.castle-user-interface';
//  DesignUrl := 'castle-data:/2d_joint_distance_spring_example.castle-user-interface';
//  DesignUrl := 'castle-data:/experimental/2d_joint_fixed_example.castle-user-interface';
//  DesignUrl := 'castle-data:/2d_joint_grab_example.castle-user-interface';
//  DesignUrl := 'castle-data:/2d_joint_hinge_break_force_example.castle-user-interface';
//  DesignUrl := 'castle-data:/2d_joint_hinge_car.castle-user-interface';
//  DesignUrl := 'castle-data:/2d_joint_hinge_limits_example.castle-user-interface';
//  DesignUrl := 'castle-data:/2d_joint_hinge_simple_example.castle-user-interface';
//  DesignUrl := 'castle-data:/experimental/2d_joint_plane_example.castle-user-interface';
//  DesignUrl := 'castle-data:/experimental/2d_joint_pulley_example.castle-user-interface';
//  DesignUrl := 'castle-data:/2d_joint_rope_chain.castle-user-interface';
//  DesignUrl := 'castle-data:/2d_joint_rope_simple.castle-user-interface';
//  DesignUrl := 'castle-data:/experimental/2d_joint_slide_example.castle-user-interface';
//  DesignUrl := 'castle-data:/experimental/3d_joint_fixed_example.castle-user-interface';
//  DesignUrl := 'castle-data:/3d_joint_hinge_door_break_force_example.castle-user-interface';
//  DesignUrl := 'castle-data:/3d_joint_hinge_door_example.castle-user-interface';
//  DesignUrl := 'castle-data:/experimental/3d_joint_pulley_example.castle-user-interface';
//  DesignUrl := 'castle-data:/3d_joint_rope_example.castle-user-interface';
//  DesignUrl := 'castle-data:/experimental/3d_joint_slide_example.castle-user-interface';
end;

procedure TStateMain.Start;
begin
  inherited;

  { Find components, by name, that we need to access from code }
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
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
