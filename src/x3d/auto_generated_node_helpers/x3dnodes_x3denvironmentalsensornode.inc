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
    function GetCenter: TVector3Single;
    procedure SetCenter(const Value: TVector3Single);
    function GetSize: TVector3Single;
    procedure SetSize(const Value: TVector3Single);
  public
    property Center: TVector3Single read GetCenter write SetCenter;
    property Size: TVector3Single read GetSize write SetSize;

{$endif read_interface}

{$ifdef read_implementation}

{ TAbstractEnvironmentalSensorNode ----------------------------------------------- }

function TAbstractEnvironmentalSensorNode.GetCenter: TVector3Single;
begin
  Result := FdCenter.Value;
end;

procedure TAbstractEnvironmentalSensorNode.SetCenter(const Value: TVector3Single);
begin
  FdCenter.Send(Value);
end;

function TAbstractEnvironmentalSensorNode.GetSize: TVector3Single;
begin
  Result := FdSize.Value;
end;

procedure TAbstractEnvironmentalSensorNode.SetSize(const Value: TVector3Single);
begin
  FdSize.Send(Value);
end;

{$endif read_implementation}
