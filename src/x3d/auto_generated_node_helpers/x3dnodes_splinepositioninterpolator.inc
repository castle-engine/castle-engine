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
    function GetClosed: boolean;
    procedure SetClosed(const Value: boolean);
    function GetNormalizeVelocity: boolean;
    procedure SetNormalizeVelocity(const Value: boolean);
  public
    property Closed: boolean read GetClosed write SetClosed;
    property NormalizeVelocity: boolean read GetNormalizeVelocity write SetNormalizeVelocity;

{$endif read_interface}

{$ifdef read_implementation}

{ TSplinePositionInterpolatorNode ----------------------------------------------- }

function TSplinePositionInterpolatorNode.GetClosed: boolean;
begin
  Result := FdClosed.Value;
end;

procedure TSplinePositionInterpolatorNode.SetClosed(const Value: boolean);
begin
  FdClosed.Send(Value);
end;

function TSplinePositionInterpolatorNode.GetNormalizeVelocity: boolean;
begin
  Result := FdNormalizeVelocity.Value;
end;

procedure TSplinePositionInterpolatorNode.SetNormalizeVelocity(const Value: boolean);
begin
  FdNormalizeVelocity.Send(Value);
end;

{$endif read_implementation}
