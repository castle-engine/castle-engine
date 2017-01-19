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
    function GetGeoCoords: TVector3Double;
    procedure SetGeoCoords(const Value: TVector3Double);
    function GetRotateYUp: boolean;
    procedure SetRotateYUp(const Value: boolean);
  public
    property GeoCoords: TVector3Double read GetGeoCoords write SetGeoCoords;
    property RotateYUp: boolean read GetRotateYUp write SetRotateYUp;

{$endif read_interface}

{$ifdef read_implementation}

{ TGeoOriginNode ----------------------------------------------- }

function TGeoOriginNode.GetGeoCoords: TVector3Double;
begin
  Result := FdGeoCoords.Value;
end;

procedure TGeoOriginNode.SetGeoCoords(const Value: TVector3Double);
begin
  FdGeoCoords.Send(Value);
end;

function TGeoOriginNode.GetRotateYUp: boolean;
begin
  Result := FdRotateYUp.Value;
end;

procedure TGeoOriginNode.SetRotateYUp(const Value: boolean);
begin
  FdRotateYUp.Send(Value);
end;

{$endif read_implementation}
