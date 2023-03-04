// -*- compile-command: "./test_single_testcase.sh TTestCastleQuaternions" -*-
{
  Copyright 2018-2023 Michalis Kamburelis.

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
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase{$else} CastleTester{$endif}, CastleVectors, CastleQuaternions;

type
  TTestCastleQuaternions = class(TCastleTestCase)
  published
    procedure TestRotationMatrix;
    procedure TestDecompose;
    procedure TestDecomposeCastleTransform;
    procedure TestSlerpEpsilon;
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
  { The order is the same as for @link(TTransformation,Multiply),
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

procedure TTestCastleQuaternions.TestSlerpEpsilon;

{ For some particular inputs, SLerp could calculate wrong results if the Epsilon in SLerp
  will be too large. Below are invalid calculations (with Epsilon = 1E-3)
  vs valid (Epsilon = 1E-6) on glTF model from https://github.com/castle-engine/castle-engine/issues/342 .

    SinTheta 0.00048828122090000000
    A 0.19590963423252106
    -0.49971881508827209 -0.49972546100616455 -0.50027436017990112 0.50028109550476074 0.50004816055297852 0.4999786913394928 0.50002127885818481 -0.49995192885398865
    good: 0.49978336691856384 0.49977511167526245 0.50022482872009277 -0.50021666288375854
    bad : -0.30385482311248779 -0.30387380719184875 -0.30430680513381958 0.30432581901550293
    DIFF!

    SinTheta 0.00048828122090000000
    A 0.5079689621925354
    -0.49971881508827209 -0.49972546100616455 -0.50027436017990112 0.50028109550476074 0.50004816055297852 0.4999786913394928 0.50002127885818481 -0.49995192885398865
    good: 0.499886155128479 0.49985414743423462 0.50014585256576538 -0.50011396408081055
    bad : 0.0081317871809005737 0.008093222975730896 0.0078447908163070679 -0.007806241512298584
    DIFF!

    SinTheta 0.00048828122090000000
    A 0.7983090877532959
    -0.49971881508827209 -0.49972546100616455 -0.50027436017990112 0.50028109550476074 0.50004816055297852 0.4999786913394928 0.50002127885818481 -0.49995192885398865
    good: 0.49998176097869873 0.49992763996124268 0.50007236003875732 -0.50001835823059082
    bad : 0.29840424656867981 0.29834744334220886 0.29827073216438293 -0.29821401834487915
    DIFF!

    SinTheta 0.00069053389600000000
    A 0.054238788783550262
    0.50011414289474487 0.50011003017425537 0.49988996982574463 -0.49988585710525513 -0.49970793724060059 -0.49970951676368713 -0.5002903938293457 0.50029188394546509
    good: -0.50009214878082275 -0.50008833408355713 -0.49991172552108765 0.49990791082382202
    bad : 0.44588500261306763 0.44588103890419006 0.44564139842987061 -0.44563743472099304
    DIFF!

    SinTheta 0.00069053389600000000
    A 0.3701384961605072
    0.50011414289474487 0.50011003017425537 0.49988996982574463 -0.49988585710525513 -0.49970793724060059 -0.49970951676368713 -0.5002903938293457 0.50029188394546509
    good: -0.49996381998062134 -0.49996179342269897 -0.50003820657730103 0.50003618001937866
    bad : 0.13004148006439209 0.13003829121589661 0.12968471646308899 -0.12968157231807709
    DIFF!

    SinTheta 0.00069053389600000000
    A 0.68378734588623047
    0.50011414289474487 0.50011003017425537 0.49988996982574463 -0.49988585710525513 -0.49970793724060059 -0.49970951676368713 -0.5002903938293457 0.50029188394546509
    good: -0.49983644485473633 -0.49983620643615723 -0.50016379356384277 0.50016355514526367
    bad : -0.18355154991149902 -0.18355391919612885 -0.18402071297168732 0.18402302265167236
    DIFF!

    SinTheta 0.00069053389600000000
    A 0.9893985390663147
    0.50011414289474487 0.50011003017425537 0.49988996982574463 -0.49988585710525513 -0.49970793724060059 -0.49970951676368713 -0.5002903938293457 0.50029188394546509
    good: -0.49971228837966919 -0.49971377849578857 -0.50028616189956665 0.50028759241104126
    bad : -0.48910835385322571 -0.48910996317863464 -0.48968702554702759 0.48968854546546936
    DIFF!

  These were calculated by test code in SLerp that detects when result of 2 branches
  would be different:

    if (SinTheta <= 0.001) and (SinTheta > 1E-6) then
    begin
      W1 := NegateOneQuaternion * Sin( (1-A) * Theta ) / SinTheta;
      W2 :=                       Sin(    A  * Theta ) / SinTheta;
      Good.Data.Vector4 := (Q1.Data.Vector4 * W1) + (Q2.Data.Vector4 * W2);

      W1 := 1 - A;
      W2 := A;
      Bad.Data.Vector4 := (Q1.Data.Vector4 * W1) + (Q2.Data.Vector4 * W2);

      if not TVector4.Equals(Bad.Data.Vector4, Good.Data.Vector4) then
      begin
        Writeln('SinTheta ', SinTheta:1:20);
        Writeln(Format('A %g', [A]));
        Writeln(Format('%s %s', [Q1.Data.Vector4.ToRawString, Q2.Data.Vector4.ToRawString]));
        Writeln('good: ', Good.Data.Vector4.ToRawString);
        Writeln('bad : ', Bad.Data.Vector4.ToRawString);
        Writeln('DIFF!');
      end;
    end;
}

var
  A: Single;
  Q1, Q2: TQuaternion;
begin
  A := 0.19590963423252106;
  Q1.Data.Vector4 := Vector4(-0.49971881508827209, -0.49972546100616455, -0.50027436017990112, 0.50028109550476074);
  Q2.Data.Vector4 := Vector4(0.50004816055297852, 0.4999786913394928, 0.50002127885818481, -0.49995192885398865);
  AssertVectorEquals(Vector4(0.49978336691856384, 0.49977511167526245, 0.50022482872009277, -0.50021666288375854),
    SLerp(A, Q1, Q2).Data.Vector4,
    0.01);

  A := 0.5079689621925354;
  Q1.Data.Vector4 := Vector4(-0.49971881508827209, -0.49972546100616455, -0.50027436017990112, 0.50028109550476074);
  Q2.Data.Vector4 := Vector4(0.50004816055297852, 0.4999786913394928, 0.50002127885818481, -0.49995192885398865);
  AssertVectorEquals(Vector4(0.499886155128479, 0.49985414743423462, 0.50014585256576538, -0.50011396408081055),
    SLerp(A, Q1, Q2).Data.Vector4,
    0.01);

  A := 0.7983090877532959;
  Q1.Data.Vector4 := Vector4(-0.49971881508827209, -0.49972546100616455, -0.50027436017990112, 0.50028109550476074);
  Q2.Data.Vector4 := Vector4(0.50004816055297852, 0.4999786913394928, 0.50002127885818481, -0.49995192885398865);
  AssertVectorEquals(Vector4(0.49998176097869873, 0.49992763996124268, 0.50007236003875732, -0.50001835823059082),
    SLerp(A, Q1, Q2).Data.Vector4,
    0.01);

  A := 0.054238788783550262;
  Q1.Data.Vector4 := Vector4(0.50011414289474487, 0.50011003017425537, 0.49988996982574463, -0.49988585710525513);
  Q2.Data.Vector4 := Vector4(-0.49970793724060059, -0.49970951676368713, -0.5002903938293457, 0.50029188394546509);
  AssertVectorEquals(Vector4(-0.50009214878082275, -0.50008833408355713, -0.49991172552108765, 0.49990791082382202),
    SLerp(A, Q1, Q2).Data.Vector4,
    0.01);

  A := 0.3701384961605072;
  Q1.Data.Vector4 := Vector4(0.50011414289474487, 0.50011003017425537, 0.49988996982574463, -0.49988585710525513);
  Q2.Data.Vector4 := Vector4(-0.49970793724060059, -0.49970951676368713, -0.5002903938293457, 0.50029188394546509);
  AssertVectorEquals(Vector4(-0.49996381998062134, -0.49996179342269897, -0.50003820657730103, 0.50003618001937866),
    SLerp(A, Q1, Q2).Data.Vector4,
    0.01);

  A := 0.68378734588623047;
  Q1.Data.Vector4 := Vector4(0.50011414289474487, 0.50011003017425537, 0.49988996982574463, -0.49988585710525513);
  Q2.Data.Vector4 := Vector4(-0.49970793724060059, -0.49970951676368713, -0.5002903938293457, 0.50029188394546509);
  AssertVectorEquals(Vector4(-0.49983644485473633, -0.49983620643615723, -0.50016379356384277, 0.50016355514526367),
    SLerp(A, Q1, Q2).Data.Vector4,
    0.01);

  A := 0.9893985390663147;
  Q1.Data.Vector4 := Vector4(0.50011414289474487, 0.50011003017425537, 0.49988996982574463, -0.49988585710525513);
  Q2.Data.Vector4 := Vector4(-0.49970793724060059, -0.49970951676368713, -0.5002903938293457, 0.50029188394546509);
  AssertVectorEquals(Vector4(-0.49971228837966919, -0.49971377849578857, -0.50028616189956665, 0.50028759241104126),
    SLerp(A, Q1, Q2).Data.Vector4,
    0.01);
end;

initialization
  RegisterTest(TTestCastleQuaternions);
end.
