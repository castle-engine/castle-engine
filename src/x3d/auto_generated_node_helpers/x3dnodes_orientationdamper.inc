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
    function GetInitialDestination: TVector4Single;
    procedure SetInitialDestination(const Value: TVector4Single);
    function GetInitialValue: TVector4Single;
    procedure SetInitialValue(const Value: TVector4Single);
  public
    property InitialDestination: TVector4Single read GetInitialDestination write SetInitialDestination;
    property InitialValue: TVector4Single read GetInitialValue write SetInitialValue;

{$endif read_interface}

{$ifdef read_implementation}

{ TOrientationDamperNode ----------------------------------------------- }

function TOrientationDamperNode.GetInitialDestination: TVector4Single;
begin
  Result := FdInitialDestination.Value;
end;

procedure TOrientationDamperNode.SetInitialDestination(const Value: TVector4Single);
begin
  FdInitialDestination.Send(Value);
end;

function TOrientationDamperNode.GetInitialValue: TVector4Single;
begin
  Result := FdInitialValue.Value;
end;

procedure TOrientationDamperNode.SetInitialValue(const Value: TVector4Single);
begin
  FdInitialValue.Send(Value);
end;

{$endif read_implementation}
