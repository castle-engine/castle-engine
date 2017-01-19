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
    function GetAlpha: Single;
    procedure SetAlpha(const Value: Single);
    function GetColor: TCastleColorRGB;
    procedure SetColor(const Value: TCastleColorRGB);
  public
    property Alpha: Single read GetAlpha write SetAlpha;
    property Color: TCastleColorRGB read GetColor write SetColor;

{$endif read_interface}

{$ifdef read_implementation}

{ TMultiTextureNode ----------------------------------------------- }

function TMultiTextureNode.GetAlpha: Single;
begin
  Result := FdAlpha.Value;
end;

procedure TMultiTextureNode.SetAlpha(const Value: Single);
begin
  FdAlpha.Send(Value);
end;

function TMultiTextureNode.GetColor: TCastleColorRGB;
begin
  Result := FdColor.Value;
end;

procedure TMultiTextureNode.SetColor(const Value: TCastleColorRGB);
begin
  FdColor.Send(Value);
end;

{$endif read_implementation}
