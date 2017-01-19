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
    function GetPickable: boolean;
    procedure SetPickable(const Value: boolean);
  public
    property Pickable: boolean read GetPickable write SetPickable;

{$endif read_interface}

{$ifdef read_implementation}

{ TPickableGroupNode ----------------------------------------------- }

function TPickableGroupNode.GetPickable: boolean;
begin
  Result := FdPickable.Value;
end;

procedure TPickableGroupNode.SetPickable(const Value: boolean);
begin
  FdPickable.Send(Value);
end;

{$endif read_implementation}
