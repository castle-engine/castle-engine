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
    function GetEndAngle: Single;
    procedure SetEndAngle(const Value: Single);
    function GetRadius: Single;
    procedure SetRadius(const Value: Single);
    function GetStartAngle: Single;
    procedure SetStartAngle(const Value: Single);
  public
    property EndAngle: Single read GetEndAngle write SetEndAngle;
    property Radius: Single read GetRadius write SetRadius;
    property StartAngle: Single read GetStartAngle write SetStartAngle;

{$endif read_interface}

{$ifdef read_implementation}

{ TArcClose2DNode ----------------------------------------------- }

function TArcClose2DNode.GetEndAngle: Single;
begin
  Result := FdEndAngle.Value;
end;

procedure TArcClose2DNode.SetEndAngle(const Value: Single);
begin
  FdEndAngle.Send(Value);
end;

function TArcClose2DNode.GetRadius: Single;
begin
  Result := FdRadius.Value;
end;

procedure TArcClose2DNode.SetRadius(const Value: Single);
begin
  FdRadius.Send(Value);
end;

function TArcClose2DNode.GetStartAngle: Single;
begin
  Result := FdStartAngle.Value;
end;

procedure TArcClose2DNode.SetStartAngle(const Value: Single);
begin
  FdStartAngle.Send(Value);
end;

{$endif read_implementation}
