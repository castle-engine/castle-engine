{%MainUnit castlevectors.pas}
{
  Copyright 2003-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ CastleVectors routines that extract from transformation matrix back
  the transformation vectors. }

{$ifdef read_interface}

{ Decompose a matrix that is composition of 3D translation, rotation and scale.

  The returned Rotation is expressed as an axis-angle (first 3 components
  are axis, last component is an angle in radians),
  as usual in our engine (see e.g. @link(TCastleTransform.Rotation)). }
procedure MatrixDecompose(const Matrix: TMatrix4;
  out Translation: TVector3; out Rotation: TVector4; out Scale: TVector3);

{ Extract scale from matrix.
  This performs a subset of MatrixDecompose work. }
function ScaleFromMatrix(const Matrix: TMatrix4): TVector3;

{ Extract translation from matrix.
  This performs a subset of MatrixDecompose work. }
function TranslationFromMatrix(const M: TMatrix4): TVector3;

{$endif read_interface}

{$ifdef read_implementation}

procedure MatrixDecompose(const Matrix: TMatrix4;
  out Translation: TVector3; out Rotation: TVector4; out Scale: TVector3);
var
  I, J: Integer;
  Column: TVector3;
  RotationMatrix: TMatrix3;
  Quaternion: TQuaternion;
begin
  { See
    https://math.stackexchange.com/questions/237369/given-this-transformation-matrix-how-do-i-decompose-it-into-translation-rotati/1463487#1463487
  }

  // calculate Translation
  Translation := Vector3(Matrix.Data[3, 0], Matrix.Data[3, 1], Matrix.Data[3, 2]);

  // calculate Scale
  for I := 0 to 2 do
  begin
    Column := Vector3(Matrix.Data[I, 0], Matrix.Data[I, 1], Matrix.Data[I, 2]);
    Scale.Data[I] := Column.Length;
  end;

  // calculate Rotation
  for I := 0 to 2 do
    for J := 0 to 2 do
      RotationMatrix.Data[I, J] := Matrix.Data[I, J] / Scale[I];
  Quaternion := QuatFromRotationMatrix(RotationMatrix);
  Rotation := Quaternion.ToAxisAngle;
end;

function ScaleFromMatrix(const Matrix: TMatrix4): TVector3;
var
  I: Integer;
  Column: TVector3;
begin
  for I := 0 to 2 do
  begin
    Column := Vector3(Matrix.Data[I, 0], Matrix.Data[I, 1], Matrix.Data[I, 2]);
    Result.Data[I] := Column.Length;
  end;
end;

function TranslationFromMatrix(const M: TMatrix4): TVector3;
begin
  Result := Vector3(M.Data[3, 0], M.Data[3, 1], M.Data[3, 2]);
end;

{$endif read_implementation}
