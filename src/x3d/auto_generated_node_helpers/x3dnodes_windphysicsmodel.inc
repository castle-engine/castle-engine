{ -*- buffer-read-only: t -*-

  Copyright 2015-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Automatically generated X3D node class helpers.
  Do not edit manually, instead regenerate using
  castle_game_engine/src/x3d/doc/generate_x3d_nodes_to_pascal.lpr . }

{$ifdef read_interface}

  private
    function GetDirection: TVector3Single;
    procedure SetDirection(const Value: TVector3Single);
    function GetGustiness: Single;
    procedure SetGustiness(const Value: Single);
    function GetSpeed: Single;
    procedure SetSpeed(const Value: Single);
    function GetTurbulence: Single;
    procedure SetTurbulence(const Value: Single);
  public
    property Direction: TVector3Single read GetDirection write SetDirection;
    property Gustiness: Single read GetGustiness write SetGustiness;
    property Speed: Single read GetSpeed write SetSpeed;
    property Turbulence: Single read GetTurbulence write SetTurbulence;

{$endif read_interface}

{$ifdef read_implementation}

{ TWindPhysicsModelNode ----------------------------------------------- }

function TWindPhysicsModelNode.GetDirection: TVector3Single;
begin
  Result := FdDirection.Value;
end;

procedure TWindPhysicsModelNode.SetDirection(const Value: TVector3Single);
begin
  FdDirection.Send(Value);
end;

function TWindPhysicsModelNode.GetGustiness: Single;
begin
  Result := FdGustiness.Value;
end;

procedure TWindPhysicsModelNode.SetGustiness(const Value: Single);
begin
  FdGustiness.Send(Value);
end;

function TWindPhysicsModelNode.GetSpeed: Single;
begin
  Result := FdSpeed.Value;
end;

procedure TWindPhysicsModelNode.SetSpeed(const Value: Single);
begin
  FdSpeed.Send(Value);
end;

function TWindPhysicsModelNode.GetTurbulence: Single;
begin
  Result := FdTurbulence.Value;
end;

procedure TWindPhysicsModelNode.SetTurbulence(const Value: Single);
begin
  FdTurbulence.Send(Value);
end;

{$endif read_implementation}
