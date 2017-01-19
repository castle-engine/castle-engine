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
    function GetOrder: Integer;
    procedure SetOrder(const Value: Integer);
  public
    property Order: Integer read GetOrder write SetOrder;

{$endif read_interface}

{$ifdef read_implementation}

{ TNurbsOrientationInterpolatorNode ----------------------------------------------- }

function TNurbsOrientationInterpolatorNode.GetOrder: Integer;
begin
  Result := FdOrder.Value;
end;

procedure TNurbsOrientationInterpolatorNode.SetOrder(const Value: Integer);
begin
  FdOrder.Send(Value);
end;

{$endif read_implementation}
