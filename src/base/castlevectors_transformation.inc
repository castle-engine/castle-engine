{
  Copyright 2020-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TTransformation record. }

{$ifdef read_interface}

type
  { Describe transformation in a way comfortable to apply it in both ways. }
  TTransformation = record
    { Transformation from local to outside coordinate space. }
    Transform: TMatrix4;

    { Inverted @link(Transform) matrix.

      Note that any scale with zero component along the way will make this
      partially invalid (we'll substitute identity in place of inverted scaling
      matrix). This is unavoidable, there's no reverse matrix for scaling
      with zero factor, since one resulting point may correpond to infinitely many
      source points (i.e., it's natural that such scaling function cannot be
      reversed). }
    InverseTransform: TMatrix4;

    { A uniform scale of the matrix @link(Transform). If the matrix
      causes non-uniform scaling, this value represents an average scale. }
    Scale: Single;

    { Is the underlying scale uniform (which means that @link(Scale)
      accurately represents the scaling, is not only approximate). }
    UniformScale: Boolean;

    { Make no transformation (identity matrix, scale 1). }
    procedure Init;

    { Modify transformation by an additional translation, rotation, scaling.

      Multiplies at the same time transformation matrix in @link(Transform),
      and it's inverse in @link(InverseTransform).
      The precise meaning of Center, Translation and other parameters
      follows exactly the X3D Transform node definition (see
      http://www.web3d.org/files/specifications/19775-1/V3.2/Part01/components/group.html#Transform ).

      @param(Rotation Rotation is expressed as a 4D vector,
        in which the first 3 components
        specify the rotation axis (does not need to be normalized, but must be non-zero),
        and the last component is the rotation angle @italic(in radians).)
    }
    procedure Multiply(
      const Center: TVector3;
      const Rotation: TVector4;
      const ScaleVector: TVector3;
      const ScaleOrientation: TVector4;
      const Translation: TVector3); overload;
    procedure Multiply(
      const Rotation: TVector4;
      const ScaleVector: TVector3;
      const Translation: TVector3); overload;

    { Modify transformation by an additional transformation matrix.

      Using this is @bold(very discouraged), as we need to assume UniformScale=false,
      which in turn e.g. makes sphere collisions worse (at some transformations,
      they have to treat sphere as something larger).
      That is because matrix can contain any transformation,
      and even using MatrixDecompose and then analyzing whether resulting scale has X=Y=Z
      would be unreliable. Testcases:

     - Rendering ghost teeth in castle-game.

       This shows that we cannot just replace matrix with translation+rotation+scale
       by MatrixDecompose, matrix can contain other transformations like shear.
       So we need to account whole matrix.
       Replacing MultiplyMatrix(matrix) with MatrixDecompose + Multiply(vectors)
       makes wrong rendering.

     - Graves on "The Gate" level in castle-game.

       This shows that even calculating UniformScale based on scale resulting
       from MatrixDecompose doesn't work reliably.
       Replacing inside MultiplyMatrix the UniformScale detection,
       to assume true when scale (from MatrixDecompose) has X=Y=Z,
       results in blocked movement.
       MatrixDecompose cannot account fo arbitrary matrix that can contain anything.

     - Beginng position (blocked movement) on "castle hall" level in castle-game.

       Same as above.
       Shows that we cannot detect UniformScale smartly.

      Use the @link(Multiply) instead to provide separate translation,
      rotation etc. vectors.
      Use this method only when you are forced to do so,
      i.e. your input is already a matrix.
    }
    procedure MultiplyMatrix(const Matrix: TMatrix4);

    { Modify transformation by an additional translation. }
    procedure Translate(const Translation: TVector3);
  end;

  PTransformation = ^TTransformation;

  TTransformationList = class({$ifdef FPC}specialize{$endif} TStructList<TTransformation>)
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TTransformation ---------------------------------------------------- }

procedure TTransformation.Init;
begin
  Transform := TMatrix4.Identity;
  InverseTransform := TMatrix4.Identity;
  Scale := 1;
  UniformScale := true;
end;

procedure TTransformation.Translate(const Translation: TVector3);
begin
  MultMatricesTranslation(Transform, InverseTransform, Translation);
end;

procedure TTransformation.Multiply(
  const Center: TVector3;
  const Rotation: TVector4;
  const ScaleVector: TVector3;
  const ScaleOrientation: TVector4;
  const Translation: TVector3);
var
  M, IM: TMatrix4;
  MRotateScaleOrient, IMRotateScaleOrient: TMatrix4;
begin
  { To make InverseTransform, we multiply inverted matrices in inverted order
    below. }

  MultMatricesTranslation(Transform, InverseTransform, Translation + Center);

  { We avoid using RotationMatricesRad when angle = 0, since this
    is often the case, and it makes TransformState much faster
    (which is important --- TransformState is important for traversing state). }
  if Rotation.W <> 0 then
  begin
    { Note that even rotation Axis = zero is OK, both M and IM will be
      identity in this case. }
    RotationMatricesRad(Rotation, M, IM);
    Transform := Transform * M;
    InverseTransform := IM * InverseTransform;
  end;

  if (ScaleVector.X <> 1) or
     (ScaleVector.Y <> 1) or
     (ScaleVector.Z <> 1) then
  begin
    if ScaleOrientation.W <> 0 then
    begin
      RotationMatricesRad(ScaleOrientation, MRotateScaleOrient, IMRotateScaleOrient);
      Transform := Transform * MRotateScaleOrient;
      InverseTransform := IMRotateScaleOrient * InverseTransform;
    end;

    { For scaling, we explicitly request that if ScalingFactor contains
      zero, IM will be forced to be identity (the 2nd param to ScalingMatrices
      is "true"). That's because X3D allows
      scaling factor to have 0 components (we need InverseTransform only
      for special tricks). }

    ScalingMatrices(ScaleVector, true, M, IM);
    Transform := Transform * M;
    InverseTransform := IM * InverseTransform;

    if ScaleOrientation.W <> 0 then
    begin
      { That's right, we reuse MRotateScaleOrient and IMRotateScaleOrient
        matrices below. Since we want to reverse them now, so normal
        Transform is multiplied by IM and InverseTransform is multiplied by M. }
      Transform := Transform * IMRotateScaleOrient;
      InverseTransform := MRotateScaleOrient * InverseTransform;
    end;

    Scale := Scale * Approximate3DScale(ScaleVector);
    UniformScale := UniformScale and
      (ScaleVector.X = ScaleVector.Y) and
      (ScaleVector.Y = ScaleVector.Z);
  end;

  MultMatricesTranslation(Transform, InverseTransform, -Center);
end;

procedure TTransformation.Multiply(
  const Rotation: TVector4;
  const ScaleVector: TVector3;
  const Translation: TVector3);
var
  M, IM: TMatrix4;
begin
  { To make InverseTransform, we multiply inverted matrices in inverted order
    below. }

  MultMatricesTranslation(Transform, InverseTransform, Translation);

  { We avoid using RotationMatricesRad when angle = 0, since this
    is often the case, and it makes TransformState much faster
    (which is important --- TransformState is important for traversing state). }
  if Rotation.W <> 0 then
  begin
    { Note that even rotation Axis = zero is OK, both M and IM will be
      identity in this case. }
    RotationMatricesRad(Rotation, M, IM);
    Transform := Transform * M;
    InverseTransform := IM * InverseTransform;
  end;

  if (ScaleVector.X <> 1) or
     (ScaleVector.Y <> 1) or
     (ScaleVector.Z <> 1) then
  begin
    { For scaling, we explicitly request that if ScalingFactor contains
      zero, IM will be forced to be identity (the 2nd param to ScalingMatrices
      is "true"). That's because X3D allows
      scaling factor to have 0 components (we need InverseTransform only
      for special tricks). }

    ScalingMatrices(ScaleVector, true, M, IM);
    Transform := Transform * M;
    InverseTransform := IM * InverseTransform;

    Scale := Scale * Approximate3DScale(ScaleVector);
    UniformScale := UniformScale and
      (ScaleVector.X = ScaleVector.Y) and
      (ScaleVector.Y = ScaleVector.Z);
  end;
end;

procedure TTransformation.MultiplyMatrix(const Matrix: TMatrix4);
var
  InvertedMatrix: TMatrix4;
begin
  Transform := Transform * Matrix;

  if TryInverseHarder(Matrix, InvertedMatrix) then
  begin
    InverseTransform := InvertedMatrix * InverseTransform;
  end else
  begin
    WritelnLogMultiline('Matrix',
      'Cannot invert matrix:' + NL + Matrix.ToRawString('  '));

    { When determinant is zero, we treat inverted matrix
      like identity (like in TMatrixTransformNode_1.Transformation).
      So no need to multiply InverseTransform by anything. }
  end;

  Scale := Scale * Approximate3DScale(Matrix);

   // we have to assume non-uniform matrix, see comments in the interface
  UniformScale := false;
end;

{$endif read_implementation}
