{%MainUnit x3dfields.pas}
{
  Copyright 2002-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Global utilities related to X3D fields. }
{$ifdef read_interface}

{ Decode color from integer value, following VRML/X3D SFImage specification.
  @groupBegin }
procedure DecodeX3DColor(const Pixel: LongWord; var G: Byte); overload;
procedure DecodeX3DColor(const Pixel: LongWord; var GA: TVector2Byte); overload;
procedure DecodeX3DColor(const Pixel: LongWord; var RGB: TVector3Byte); overload;
procedure DecodeX3DColor(const Pixel: LongWord; var RGBA: TVector4Byte); overload;

procedure DecodeX3DColor(const Pixel: LongInt; var G: Byte); overload;
procedure DecodeX3DColor(const Pixel: LongInt; var GA: TVector2Byte); overload;
procedure DecodeX3DColor(const Pixel: LongInt; var RGB: TVector3Byte); overload;
procedure DecodeX3DColor(const Pixel: LongInt; var RGBA: TVector4Byte); overload;
{ @groupEnd }

const
  X3DChangeToStr: array [TX3DChange] of string = (
    'None',
    'Visible Geometry',
    'Visible Non-Geometry',
    'Redisplay',
    'Transform',
    'Coordinate',
    'Normal',
    'Tangent',
    'VRML 1.0 State (but not affecting geometry or Coordinate)',
    'VRML 1.0 State (affecting geometry, but not Coordinate)',
    'Light active property',
    'Light location/direction',
    'Light for shadow volumes',
    'Switch choice',
    'Color node',
    'Texture coordinate',
    'Texture transform',
    'Geometry',
    'Geometry + FontChanged',
    'Environmental sensor bounds',
    'Time stop/start/pause/resume',
    'Viewpoint vectors',
    'Viewpoint projection',
    'Texture image',
    'Texture renderer properties',
    'TextureProperties node',
    'Shadow caster',
    'Generated texture update',
    'FontStyle',
    'FontStyle + FontChanged',
    'HeadLight on',
    'ClipPlane',
    'X3DDragSensorNode.enabled',
    'NavigationInfo',
    'ScreenEffect.enabled',
    'Background',
    'Everything',
    'Shadow maps',
    'Wireframe',
    'Children',
    'Children added',
    'BBox'
  );

function X3DChangesToStr(const Changes: TX3DChanges): string;

{$endif read_interface}

{$ifdef read_implementation}

procedure DecodeX3DColor(const Pixel: LongWord; var G: Byte);
begin
  G := Pixel and $FF;
end;

procedure DecodeX3DColor(const Pixel: LongWord; var GA: TVector2Byte);
begin
  GA.X := (pixel shr 8) and $FF;
  GA.Y := pixel and $FF;
end;

procedure DecodeX3DColor(const Pixel: LongWord; var RGB: TVector3Byte);
begin
  RGB.X := (pixel shr 16) and $FF;
  RGB.Y := (pixel shr 8) and $FF;
  RGB.Z := pixel and $FF;
end;

procedure DecodeX3DColor(const Pixel: LongWord; var RGBA: TVector4Byte);
begin
  RGBA.X := (pixel shr 24) and $FF;
  RGBA.Y := (pixel shr 16) and $FF;
  RGBA.Z := (pixel shr 8) and $FF;
  RGBA.W := pixel and $FF;
end;

{ We have to turn range checking off, because converting from LongInt
  to LongWord below may cause range check errors. Yes, we want to
  directly treat LongInt as 4 bytes here, because DecodeX3DColor
  works on separate bytes. See
  https://castle-engine.io/x3d_implementation_texturing3d.php
  comments about PixelTexture3D. }

{$include norqcheckbegin.inc}

procedure DecodeX3DColor(const Pixel: LongInt; var G: Byte);
begin
  DecodeX3DColor(LongWord(Pixel), G);
end;

procedure DecodeX3DColor(const Pixel: LongInt; var GA: TVector2Byte);
begin
  DecodeX3DColor(LongWord(Pixel), GA);
end;

procedure DecodeX3DColor(const Pixel: LongInt; var RGB: TVector3Byte);
begin
  DecodeX3DColor(LongWord(Pixel), RGB);
end;

procedure DecodeX3DColor(const Pixel: LongInt; var RGBA: TVector4Byte);
begin
  DecodeX3DColor(LongWord(Pixel), RGBA);
end;

{$include norqcheckend.inc}

function X3DChangesToStr(const Changes: TX3DChanges): string;
var
  C: TX3DChange;
begin
  Result := '';
  for C := Low(C) to High(C) do
    if C in Changes then
    begin
      if Result <> '' then Result := Result + ',';
      Result := Result + X3DChangeToStr[C];
    end;
  Result := '[' + Result + ']';
end;

{$endif read_implementation}
