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
    function GetCenter: TVector2Single;
    procedure SetCenter(const Value: TVector2Single);
    function GetRotation: Single;
    procedure SetRotation(const Value: Single);
    function GetScale: TVector2Single;
    procedure SetScale(const Value: TVector2Single);
    function GetTranslation: TVector2Single;
    procedure SetTranslation(const Value: TVector2Single);
  public
    property Center: TVector2Single read GetCenter write SetCenter;
    property Rotation: Single read GetRotation write SetRotation;
    property Scale: TVector2Single read GetScale write SetScale;
    property Translation: TVector2Single read GetTranslation write SetTranslation;

{$endif read_interface}

{$ifdef read_implementation}

{ TTextureTransformNode ----------------------------------------------- }

function TTextureTransformNode.GetCenter: TVector2Single;
begin
  Result := FdCenter.Value;
end;

procedure TTextureTransformNode.SetCenter(const Value: TVector2Single);
begin
  FdCenter.Send(Value);
end;

function TTextureTransformNode.GetRotation: Single;
begin
  Result := FdRotation.Value;
end;

procedure TTextureTransformNode.SetRotation(const Value: Single);
begin
  FdRotation.Send(Value);
end;

function TTextureTransformNode.GetScale: TVector2Single;
begin
  Result := FdScale.Value;
end;

procedure TTextureTransformNode.SetScale(const Value: TVector2Single);
begin
  FdScale.Send(Value);
end;

function TTextureTransformNode.GetTranslation: TVector2Single;
begin
  Result := FdTranslation.Value;
end;

procedure TTextureTransformNode.SetTranslation(const Value: TVector2Single);
begin
  FdTranslation.Send(Value);
end;

{$endif read_implementation}
