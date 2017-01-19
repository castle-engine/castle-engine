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
    function GetSize: Integer;
    procedure SetSize(const Value: Integer);
    function GetScale: Single;
    procedure SetScale(const Value: Single);
    function GetBias: Single;
    procedure SetBias(const Value: Single);
  public
    property Size: Integer read GetSize write SetSize;
    property Scale: Single read GetScale write SetScale;
    property Bias: Single read GetBias write SetBias;

{$endif read_interface}

{$ifdef read_implementation}

{ TGeneratedShadowMapNode ----------------------------------------------- }

function TGeneratedShadowMapNode.GetSize: Integer;
begin
  Result := FdSize.Value;
end;

procedure TGeneratedShadowMapNode.SetSize(const Value: Integer);
begin
  FdSize.Send(Value);
end;

function TGeneratedShadowMapNode.GetScale: Single;
begin
  Result := FdScale.Value;
end;

procedure TGeneratedShadowMapNode.SetScale(const Value: Single);
begin
  FdScale.Send(Value);
end;

function TGeneratedShadowMapNode.GetBias: Single;
begin
  Result := FdBias.Value;
end;

procedure TGeneratedShadowMapNode.SetBias(const Value: Single);
begin
  FdBias.Send(Value);
end;

{$endif read_implementation}
