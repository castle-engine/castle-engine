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
    function GetInitialDestination: TCastleColorRGB;
    procedure SetInitialDestination(const Value: TCastleColorRGB);
    function GetInitialValue: TCastleColorRGB;
    procedure SetInitialValue(const Value: TCastleColorRGB);
  public
    property InitialDestination: TCastleColorRGB read GetInitialDestination write SetInitialDestination;
    property InitialValue: TCastleColorRGB read GetInitialValue write SetInitialValue;

{$endif read_interface}

{$ifdef read_implementation}

{ TColorDamperNode ----------------------------------------------- }

function TColorDamperNode.GetInitialDestination: TCastleColorRGB;
begin
  Result := FdInitialDestination.Value;
end;

procedure TColorDamperNode.SetInitialDestination(const Value: TCastleColorRGB);
begin
  FdInitialDestination.Send(Value);
end;

function TColorDamperNode.GetInitialValue: TCastleColorRGB;
begin
  Result := FdInitialValue.Value;
end;

procedure TColorDamperNode.SetInitialValue(const Value: TCastleColorRGB);
begin
  FdInitialValue.Send(Value);
end;

{$endif read_implementation}
