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
    function GetDirection: TVector3Single;
    procedure SetDirection(const Value: TVector3Single);
    function GetPosition: TVector3Single;
    procedure SetPosition(const Value: TVector3Single);
  public
    property Direction: TVector3Single read GetDirection write SetDirection;
    property Position: TVector3Single read GetPosition write SetPosition;

{$endif read_interface}

{$ifdef read_implementation}

{ TPointEmitterNode ----------------------------------------------- }

function TPointEmitterNode.GetDirection: TVector3Single;
begin
  Result := FdDirection.Value;
end;

procedure TPointEmitterNode.SetDirection(const Value: TVector3Single);
begin
  FdDirection.Send(Value);
end;

function TPointEmitterNode.GetPosition: TVector3Single;
begin
  Result := FdPosition.Value;
end;

procedure TPointEmitterNode.SetPosition(const Value: TVector3Single);
begin
  FdPosition.Send(Value);
end;

{$endif read_implementation}
