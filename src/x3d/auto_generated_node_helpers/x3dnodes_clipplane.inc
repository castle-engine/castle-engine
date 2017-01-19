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
    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
    function GetPlane: TVector4Single;
    procedure SetPlane(const Value: TVector4Single);
  public
    property Enabled: boolean read GetEnabled write SetEnabled;
    property Plane: TVector4Single read GetPlane write SetPlane;

{$endif read_interface}

{$ifdef read_implementation}

{ TClipPlaneNode ----------------------------------------------- }

function TClipPlaneNode.GetEnabled: boolean;
begin
  Result := FdEnabled.Value;
end;

procedure TClipPlaneNode.SetEnabled(const Value: boolean);
begin
  FdEnabled.Send(Value);
end;

function TClipPlaneNode.GetPlane: TVector4Single;
begin
  Result := FdPlane.Value;
end;

procedure TClipPlaneNode.SetPlane(const Value: TVector4Single);
begin
  FdPlane.Send(Value);
end;

{$endif read_implementation}
