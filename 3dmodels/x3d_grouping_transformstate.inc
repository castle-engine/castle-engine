 (State: TVRMLGraphTraverseState;
  {$ifndef TRANSFORM_SIMPLE} center: TSFVec3f; {$endif}
  rotation: TSFRotation;
  scale: TSFVec3f;
  {$ifndef TRANSFORM_SIMPLE} scaleOrientation: TSFRotation; {$endif}
  translation: TSFVec3f);
var
  M, IM, MRotateScaleOrient, IMRotateScaleOrient: TMatrix4Single;
begin
  { Inherited TVRMLGroupingNode already saved State.Transform and such.

    To make InvertedTransform, we multiply inverted matrices in inverted order
    below. }

  TranslationMatrices(
    {$ifndef TRANSFORM_SIMPLE} VectorAdd(Translation.Value, Center.Value)
    {$else} Translation.Value
    {$endif}, M, IM);
  State.Transform := MatrixMult(State.Transform, M);
  State.InvertedTransform := MatrixMult(IM, State.InvertedTransform);

  { We avoid using RotationMatricesRad when angle = 0, since this
    is often the case, and it makes TransformState much faster
    (which is important --- TransformState is important for traversing state). }
  if Rotation.RotationRad <> 0 then
  begin
    { Note that even Axis = zero is OK, both M and IM will be identity in
      this case. }
    RotationMatricesRad(Rotation.RotationRad, Rotation.Axis, M, IM);
    State.Transform := MatrixMult(State.Transform, M);
    State.InvertedTransform := MatrixMult(IM, State.InvertedTransform);
  end;

  if (Scale.Value[0] <> 1) or
     (Scale.Value[1] <> 1) or
     (Scale.Value[2] <> 1) then
  begin
    {$ifndef TRANSFORM_SIMPLE}
    if ScaleOrientation.RotationRad <> 0 then
    begin
      RotationMatricesRad(ScaleOrientation.RotationRad, ScaleOrientation.Axis,
        MRotateScaleOrient, IMRotateScaleOrient);

      State.Transform := MatrixMult(State.Transform, MRotateScaleOrient);
      State.InvertedTransform := MatrixMult(IMRotateScaleOrient, State.InvertedTransform);
    end;
    {$endif}

    { For scaling, we explicitly request that if ScalingFactor contains
      zero, IM will be forced to be identity. That's because VRML allows
      scaling factor to have 0 components (we need InvertedTransform only
      for special tricks). }

    ScalingMatrices(Scale.Value, true, M, IM);
    State.Transform := MatrixMult(State.Transform, M);
    State.InvertedTransform := MatrixMult(IM, State.InvertedTransform);

    State.AverageScaleTransform *=
      (Scale.Value[0] + Scale.Value[1] + Scale.Value[2]) / 3;

    {$ifndef TRANSFORM_SIMPLE}
    if ScaleOrientation.RotationRad <> 0 then
    begin
      { That's right, we reuse MRotateScaleOrient and IMRotateScaleOrient
        matrices below. Since we want to reverse them now, so normal
        Transform is multiplied by IM and InvertedTransform is multiplied by M. }
      State.Transform := MatrixMult(State.Transform, IMRotateScaleOrient);
      State.InvertedTransform := MatrixMult(MRotateScaleOrient, State.InvertedTransform);
    end;
    {$endif}
  end;

  {$ifndef TRANSFORM_SIMPLE}
  TranslationMatrices(VectorNegate(Center.Value), M, IM);
  State.Transform := MatrixMult(State.Transform, M);
  State.InvertedTransform := MatrixMult(IM, State.InvertedTransform);
  {$endif}
end;
