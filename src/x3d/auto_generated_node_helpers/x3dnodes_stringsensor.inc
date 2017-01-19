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
    function GetDeletionAllowed: boolean;
    procedure SetDeletionAllowed(const Value: boolean);
  public
    property DeletionAllowed: boolean read GetDeletionAllowed write SetDeletionAllowed;

{$endif read_interface}

{$ifdef read_implementation}

{ TStringSensorNode ----------------------------------------------- }

function TStringSensorNode.GetDeletionAllowed: boolean;
begin
  Result := FdDeletionAllowed.Value;
end;

procedure TStringSensorNode.SetDeletionAllowed(const Value: boolean);
begin
  FdDeletionAllowed.Send(Value);
end;

{$endif read_implementation}
