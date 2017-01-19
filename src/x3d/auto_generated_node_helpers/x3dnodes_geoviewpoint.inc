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
    function GetFieldOfView: Single;
    procedure SetFieldOfView(const Value: Single);
    function GetHeadlight: boolean;
    procedure SetHeadlight(const Value: boolean);
    function GetSpeedFactor: Single;
    procedure SetSpeedFactor(const Value: Single);
  public
    property FieldOfView: Single read GetFieldOfView write SetFieldOfView;
    property Headlight: boolean read GetHeadlight write SetHeadlight;
    property SpeedFactor: Single read GetSpeedFactor write SetSpeedFactor;

{$endif read_interface}

{$ifdef read_implementation}

{ TGeoViewpointNode ----------------------------------------------- }

function TGeoViewpointNode.GetFieldOfView: Single;
begin
  Result := FdFieldOfView.Value;
end;

procedure TGeoViewpointNode.SetFieldOfView(const Value: Single);
begin
  FdFieldOfView.Send(Value);
end;

function TGeoViewpointNode.GetHeadlight: boolean;
begin
  Result := FdHeadlight.Value;
end;

procedure TGeoViewpointNode.SetHeadlight(const Value: boolean);
begin
  FdHeadlight.Send(Value);
end;

function TGeoViewpointNode.GetSpeedFactor: Single;
begin
  Result := FdSpeedFactor.Value;
end;

procedure TGeoViewpointNode.SetSpeedFactor(const Value: Single);
begin
  FdSpeedFactor.Send(Value);
end;

{$endif read_implementation}
