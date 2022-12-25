{%MainUnit castletransform.pas}
{
  Copyright 2017-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

type
  TSceneManagerWorld = TCastleAbstractRootTransform deprecated 'use TCastleRootTransform';

procedure TransformMatricesMult(var Transform, InverseTransform: TMatrix4;
  const Center: TVector3;
  const Rotation: TVector4;
  const Scale: TVector3;
  const ScaleOrientation: TVector4;
  const Translation: TVector3);
  deprecated 'use TTransformation.Multiply';

const
  rfOffScreen = rfRenderedTexture deprecated 'use rfRenderedTexture';

function StrToOrientationType(const S: String): TOrientationType;

var
  InternalGizmoBoundingBox: Cardinal;

{$endif read_interface}

{$ifdef read_implementation}

{ global routines ------------------------------------------------------------ }

{ TransformMatricesMult ------------------------------------------------------ }

{ Workaround FPC bug on Darwin for AArch64 (not on other platforms),
  causes "Fatal: Internal error 2014121702".
  Occurs with 3.0.4 and with 3.3.1 (r44333 from 2020/03/22). }
{$if defined(DARWIN) and defined(CPUAARCH64)}
  {$define COMPILER_BUGGY_PARAMETERS}
{$endif}

procedure TransformMatricesMult(var Transform, InverseTransform: TMatrix4;
  const Center: TVector3;
  const Rotation: TVector4;
  const Scale: TVector3;
  const ScaleOrientation: TVector4;
  const Translation: TVector3);

{$ifdef COMPILER_BUGGY_PARAMETERS}
  type
    TTransformData = record
      Transform, InverseTransform: TMatrix4;
      Center: TVector3;
      Rotation: TVector4;
      Scale: TVector3;
      ScaleOrientation: TVector4;
      Translation: TVector3;
    end;

  procedure MultiplyWorkaround(var T: TTransformation; const TransformData: TTransformData);
  begin
    T.Multiply(
      TransformData.Center,
      TransformData.Rotation,
      TransformData.Scale,
      TransformData.ScaleOrientation,
      TransformData.Translation);
  end;

var
  TransformData: TTransformData;
{$endif COMPILER_BUGGY_PARAMETERS}

var
  T: TTransformation;
begin
  T.Transform := Transform;
  T.InverseTransform := InverseTransform;
  // T.Scale := 1; // doesn't matter
{$ifdef COMPILER_BUGGY_PARAMETERS}
  TransformData.Center := Center;
  TransformData.Rotation := Rotation;
  TransformData.Scale := Scale;
  TransformData.ScaleOrientation := ScaleOrientation;
  TransformData.Translation := Translation;
  MultiplyWorkaround(T, TransformData);
{$else}
  T.Multiply(
    Center,
    Rotation,
    Scale,
    ScaleOrientation,
    Translation);
{$endif}
  Transform := T.Transform;
  InverseTransform := T.InverseTransform;
end;

const
  OrientationNames: array [TOrientationType] of String =  (
    'up:y,direction:-z',
    'up:y,direction:z',
    'up:z,direction:-y',
    'up:z,direction:x'
  );

function StrToOrientationType(const S: String): TOrientationType;
begin
  if S = 'default' then
    Exit(TCastleTransform.DefaultOrientation);
  for Result := Low(TOrientationType) to High(TOrientationType) do
    if OrientationNames[Result] = S then
      Exit;
  raise Exception.CreateFmt('Invalid orientation name "%s"', [S]);
end;

{$endif read_implementation}
