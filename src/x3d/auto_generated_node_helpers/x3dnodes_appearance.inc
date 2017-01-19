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
    function GetShadowCaster: boolean;
    procedure SetShadowCaster(const Value: boolean);
    function GetHeightMapScale: Single;
    procedure SetHeightMapScale(const Value: Single);
  public
    property ShadowCaster: boolean read GetShadowCaster write SetShadowCaster;
    property HeightMapScale: Single read GetHeightMapScale write SetHeightMapScale;

{$endif read_interface}

{$ifdef read_implementation}

{ TAppearanceNode ----------------------------------------------- }

function TAppearanceNode.GetShadowCaster: boolean;
begin
  Result := FdShadowCaster.Value;
end;

procedure TAppearanceNode.SetShadowCaster(const Value: boolean);
begin
  FdShadowCaster.Send(Value);
end;

function TAppearanceNode.GetHeightMapScale: Single;
begin
  Result := FdHeightMapScale.Value;
end;

procedure TAppearanceNode.SetHeightMapScale(const Value: Single);
begin
  FdHeightMapScale.Send(Value);
end;

{$endif read_implementation}
