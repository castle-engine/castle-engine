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
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    DesignContent: TCastleDesign;
    ButtonHingeSimple: TCastleButton;
    ButtonHingeBreakable: TCastleButton;
    ButtonHingeCar: TCastleButton;
    ButtonHingeLimits: TCastleButton;
    ButtonDistance: TCastleButton;
    ButtonDistanceSpring: TCastleButton;
    ButtonRope: TCastleButton;
    ButtonRopeChain: TCastleButton;
  private
    procedure ClickHingeSimple(Sender: TObject);
    procedure ClickHingeBreakable(Sender: TObject);
    procedure ClickHingeCar(Sender: TObject);
    procedure ClickHingeLimits(Sender: TObject);
    procedure ClickDistance(Sender: TObject);
    procedure ClickDistanceSpring(Sender: TObject);
    procedure ClickRope(Sender: TObject);
    procedure ClickRopeChain(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
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
begin
  inherited;
  ButtonHingeSimple.OnClick := {$ifdef FPC}@{$endif} ClickHingeSimple;
  ButtonHingeBreakable.OnClick := {$ifdef FPC}@{$endif} ClickHingeBreakable;
  ButtonHingeCar.OnClick := {$ifdef FPC}@{$endif} ClickHingeCar;
  ButtonHingeLimits.OnClick := {$ifdef FPC}@{$endif} ClickHingeLimits;
  ButtonDistance.OnClick := {$ifdef FPC}@{$endif} ClickDistance;
  ButtonDistanceSpring.OnClick := {$ifdef FPC}@{$endif} ClickDistanceSpring;
  ButtonRope.OnClick := {$ifdef FPC}@{$endif} ClickRope;
  ButtonRopeChain.OnClick := {$ifdef FPC}@{$endif} ClickRopeChain;
end;

procedure TStateMain.ClickHingeSimple(Sender: TObject);
begin
  DesignContent.Url := 'castle-data:/2d_joint_hinge_simple_example.castle-user-interface';
end;

procedure TStateMain.ClickHingeBreakable(Sender: TObject);
begin
  DesignContent.Url := 'castle-data:/2d_joint_hinge_break_force_example.castle-user-interface';
end;

procedure TStateMain.ClickHingeCar(Sender: TObject);
begin
  DesignContent.Url := 'castle-data:/2d_joint_hinge_car.castle-user-interface';
end;

procedure TStateMain.ClickHingeLimits(Sender: TObject);
begin
  DesignContent.Url := 'castle-data:/2d_joint_hinge_limits_example.castle-user-interface';
end;

procedure TStateMain.ClickDistance(Sender: TObject);
begin
  DesignContent.Url := 'castle-data:/2d_joint_distance_example.castle-user-interface';
end;

procedure TStateMain.ClickDistanceSpring(Sender: TObject);
begin
  DesignContent.Url := 'castle-data:/2d_joint_distance_spring_example.castle-user-interface';
end;

procedure TStateMain.ClickRope(Sender: TObject);
begin
  DesignContent.Url := 'castle-data:/2d_joint_rope_simple.castle-user-interface';
end;

procedure TStateMain.ClickRopeChain(Sender: TObject);
begin
  DesignContent.Url := 'castle-data:/2d_joint_rope_chain.castle-user-interface';
end;

end.
