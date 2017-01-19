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
    function GetGeoCenter: TVector3Double;
    procedure SetGeoCenter(const Value: TVector3Double);
    function GetRotation: TVector4Single;
    procedure SetRotation(const Value: TVector4Single);
    function GetScale: TVector3Single;
    procedure SetScale(const Value: TVector3Single);
    function GetScaleOrientation: TVector4Single;
    procedure SetScaleOrientation(const Value: TVector4Single);
    function GetTranslation: TVector3Single;
    procedure SetTranslation(const Value: TVector3Single);
  public
    property GeoCenter: TVector3Double read GetGeoCenter write SetGeoCenter;
    property Rotation: TVector4Single read GetRotation write SetRotation;
    property Scale: TVector3Single read GetScale write SetScale;
    property ScaleOrientation: TVector4Single read GetScaleOrientation write SetScaleOrientation;
    property Translation: TVector3Single read GetTranslation write SetTranslation;

{$endif read_interface}

{$ifdef read_implementation}

{ TGeoTransformNode ----------------------------------------------- }

function TGeoTransformNode.GetGeoCenter: TVector3Double;
begin
  Result := FdGeoCenter.Value;
end;

procedure TGeoTransformNode.SetGeoCenter(const Value: TVector3Double);
begin
  FdGeoCenter.Send(Value);
end;

function TGeoTransformNode.GetRotation: TVector4Single;
begin
  Result := FdRotation.Value;
end;

procedure TGeoTransformNode.SetRotation(const Value: TVector4Single);
begin
  FdRotation.Send(Value);
end;

function TGeoTransformNode.GetScale: TVector3Single;
begin
  Result := FdScale.Value;
end;

procedure TGeoTransformNode.SetScale(const Value: TVector3Single);
begin
  FdScale.Send(Value);
end;

function TGeoTransformNode.GetScaleOrientation: TVector4Single;
begin
  Result := FdScaleOrientation.Value;
end;

procedure TGeoTransformNode.SetScaleOrientation(const Value: TVector4Single);
begin
  FdScaleOrientation.Send(Value);
end;

function TGeoTransformNode.GetTranslation: TVector3Single;
begin
  Result := FdTranslation.Value;
end;

procedure TGeoTransformNode.SetTranslation(const Value: TVector3Single);
begin
  FdTranslation.Send(Value);
end;

{$endif read_implementation}
