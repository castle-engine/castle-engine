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
    function GetSize: TVector3Single;
    procedure SetSize(const Value: TVector3Single);
    function GetDivisions: Integer;
    procedure SetDivisions(const Value: Integer);
  public
    property Size: TVector3Single read GetSize write SetSize;
    property Divisions: Integer read GetDivisions write SetDivisions;

{$endif read_interface}

{$ifdef read_implementation}

{ TBoxNode ----------------------------------------------- }

function TBoxNode.GetSize: TVector3Single;
begin
  Result := FdSize.Value;
end;

procedure TBoxNode.SetSize(const Value: TVector3Single);
begin
  FdSize.Send(Value);
end;

function TBoxNode.GetDivisions: Integer;
begin
  Result := FdDivisions.Value;
end;

procedure TBoxNode.SetDivisions(const Value: Integer);
begin
  FdDivisions.Send(Value);
end;

{$endif read_implementation}
