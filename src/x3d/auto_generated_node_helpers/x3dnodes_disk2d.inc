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
    function GetInnerRadius: Single;
    procedure SetInnerRadius(const Value: Single);
    function GetOuterRadius: Single;
    procedure SetOuterRadius(const Value: Single);
  public
    property InnerRadius: Single read GetInnerRadius write SetInnerRadius;
    property OuterRadius: Single read GetOuterRadius write SetOuterRadius;

{$endif read_interface}

{$ifdef read_implementation}

{ TDisk2DNode ----------------------------------------------- }

function TDisk2DNode.GetInnerRadius: Single;
begin
  Result := FdInnerRadius.Value;
end;

procedure TDisk2DNode.SetInnerRadius(const Value: Single);
begin
  FdInnerRadius.Send(Value);
end;

function TDisk2DNode.GetOuterRadius: Single;
begin
  Result := FdOuterRadius.Value;
end;

procedure TDisk2DNode.SetOuterRadius(const Value: Single);
begin
  FdOuterRadius.Send(Value);
end;

{$endif read_implementation}
