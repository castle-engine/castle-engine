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
    function GetMaxExtent: Single;
    procedure SetMaxExtent(const Value: Single);
    function GetDepth: Single;
    procedure SetDepth(const Value: Single);
  public
    property MaxExtent: Single read GetMaxExtent write SetMaxExtent;
    property Depth: Single read GetDepth write SetDepth;

{$endif read_interface}

{$ifdef read_implementation}

{ TText3DNode ----------------------------------------------- }

function TText3DNode.GetMaxExtent: Single;
begin
  Result := FdMaxExtent.Value;
end;

procedure TText3DNode.SetMaxExtent(const Value: Single);
begin
  FdMaxExtent.Send(Value);
end;

function TText3DNode.GetDepth: Single;
begin
  Result := FdDepth.Value;
end;

procedure TText3DNode.SetDepth(const Value: Single);
begin
  FdDepth.Send(Value);
end;

{$endif read_implementation}
