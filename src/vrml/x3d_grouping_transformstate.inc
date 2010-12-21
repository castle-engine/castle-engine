procedure TransformMatrices(var Transform, InvertedTransform: TMatrix4Single;
  var TransformScale: Single;
  center: TVector3Single;
  rotation: TVector4Single;
  scale: TVector3Single;
  scaleOrientation: TVector4Single;
  translation: TVector3Single);
var
  M, IM: TMatrix4Single;
  MRotateScaleOrient, IMRotateScaleOrient: TMatrix4Single;
begin
  { Inherited TVRMLGroupingNode already saved Transform and such.

    To make InvertedTransform, we multiply inverted matrices in inverted order
    below. }

  MultMatricesTranslation(Transform, InvertedTransform,
    VectorAdd(Translation, Center));

  { We avoid using RotationMatricesRad when angle = 0, since this
    is often the case, and it makes TransformState much faster
    (which is important --- TransformState is important for traversing state). }
  if Rotation[3] <> 0 then
  begin
    { Note that even rotation Axis = zero is OK, both M and IM will be
      identity in this case. }
    RotationMatricesRad(Rotation, M, IM);
    Transform := MatrixMult(Transform, M);
    InvertedTransform := MatrixMult(IM, InvertedTransform);
  end;

  if (Scale[0] <> 1) or
     (Scale[1] <> 1) or
     (Scale[2] <> 1) then
  begin
    if ScaleOrientation[3] <> 0 then
    begin
      RotationMatricesRad(ScaleOrientation, MRotateScaleOrient, IMRotateScaleOrient);
      Transform := MatrixMult(Transform, MRotateScaleOrient);
      InvertedTransform := MatrixMult(IMRotateScaleOrient, InvertedTransform);
    end;

    { For scaling, we explicitly request that if ScalingFactor contains
      zero, IM will be forced to be identity (the 2nd param to ScalingMatrices
      is "true"). That's because VRML allows
      scaling factor to have 0 components (we need InvertedTransform only
      for special tricks). }

    ScalingMatrices(Scale, true, M, IM);
    Transform := MatrixMult(Transform, M);
    InvertedTransform := MatrixMult(IM, InvertedTransform);

    TransformScale *= (Scale[0] + Scale[1] + Scale[2]) / 3;

    if ScaleOrientation[3] <> 0 then
    begin
      { That's right, we reuse MRotateScaleOrient and IMRotateScaleOrient
        matrices below. Since we want to reverse them now, so normal
        Transform is multiplied by IM and InvertedTransform is multiplied by M. }
      Transform := MatrixMult(Transform, IMRotateScaleOrient);
      InvertedTransform := MatrixMult(MRotateScaleOrient, InvertedTransform);
    end;
  end;

  MultMatricesTranslation(Transform, InvertedTransform, VectorNegate(Center));
end;

procedure TransformState(State: TVRMLGraphTraverseState;
  center: TVector3Single;
  rotation: TVector4Single;
  scale: TVector3Single;
  scaleOrientation: TVector4Single;
  translation: TVector3Single);
begin
  TransformMatrices(
    State.Transform, State.InvertedTransform, State.TransformScale,
    center, rotation, scale, scaleOrientation, translation);
end;
