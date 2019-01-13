{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Tests of CastleQuaternions unit. }
unit TestCastleQuaternions;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  CastleBaseTestCase, CastleVectors, CastleQuaternions;

type
  TTestCastleQuaternions = class(TCastleBaseTestCase)
  published
    procedure TestRotationMatrix;
    procedure TestDecompose;
    procedure TestDecomposeCastleTransform;
  end;

implementation

uses CastleTransform;

procedure TTestCastleQuaternions.TestRotationMatrix;
var
  NewQ, Q: TQuaternion;
  M: TMatrix3;
begin
  Q := QuatFromAxisAngle(Vector3(1, 2, 3), 6.666, true);
  M := Q.ToRotationMatrix3;
  NewQ := QuatFromRotationMatrix(M);

  // Writeln(Q.Data.Vector4.ToString);
  // Writeln(NewQ.Data.Vector4.ToString);

  // Writeln(Q.ToAxisAngle.ToString);
  // Writeln(NewQ.ToAxisAngle.ToString);

  // Writeln(Q.Rotate(Vector3(30, 20, 10)).ToString);
  // Writeln(NewQ.Rotate(Vector3(30, 20, 10)).ToString);

  { easy way to check that quaternions are equal: check that they rotate
    some point in the same way. }
  AssertVectorEquals(
    Q.Rotate(Vector3(30, 20, 10)),
    NewQ.Rotate(Vector3(30, 20, 10)));
end;

procedure TTestCastleQuaternions.TestDecompose;
var
  M: TMatrix4;
  Translation, Scale: TVector3;
  Rotation: TVector4;
begin
  { The order is the same as for TransformMatricesMult,
    and same as done by TCastleTransform: first translation matrix,
    multiplied by rotation matrix,
    multiplied by scaling matrix. }
  M := TranslationMatrix(Vector3(44, 100, 200)) *
    RotationMatrixRad(6.66, Vector3(1, 2, 3).Normalize) *
    ScalingMatrix(Vector3(9, 8, 7));

  MatrixDecompose(M, Translation, Rotation, Scale);

  AssertVectorEquals(Translation, Vector3(44, 100, 200), 0.1);
  AssertVectorEquals(Rotation.XYZ, Vector3(1, 2, 3).Normalize, 0.1);
  AssertVectorEquals(Scale, Vector3(9, 8, 7), 0.1);

  // Writeln(Translation.ToString);
  // Writeln(Rotation.ToString);
  // Writeln(Scale.ToString);
end;

procedure TTestCastleQuaternions.TestDecomposeCastleTransform;
var
  M: TMatrix4;
  Translation, Scale: TVector3;
  Rotation: TVector4;
  Transform: TCastleTransform;
begin
  M :=
    TranslationMatrix(Vector3(1, 2, 3)) *
    RotationMatrixRad(6.66, Vector3(5, 6, 7).Normalize) *
    ScalingMatrix(Vector3(9, 8, 7)) *
    TranslationMatrix(Vector3(10, 20, 30)) *
    RotationMatrixRad(6.66, Vector3(50, 60, 70).Normalize) *
    ScalingMatrix(Vector3(9, 8, 7));

  MatrixDecompose(M, Translation, Rotation, Scale);

  Transform := TCastleTransform.Create(nil);
  try
    Transform.Translation := Translation;
    Transform.Rotation := Rotation;
    Transform.Scale := Scale;
    AssertMatrixEquals(Transform.Transform, M, 5);
  finally FreeAndNil(Transform) end;
end;

initialization
  RegisterTest(TTestCastleQuaternions);
end.
