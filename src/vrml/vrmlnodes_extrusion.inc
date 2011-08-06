{
  Copyright 2008-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}


type
  { Utility for dealing with Extrusion.

   Calculating vertices (and other properties) of Extrusion
   is non-trivial enough to be separated into this file. We want to reuse
   this by TNodeExtrusion.BoundingBox calculation,
   TNodeExtrusion.Triangulate etc. }
  TVRMLExtrusion = class
  private
    FHigh: Integer;
    FNode: TNodeExtrusion;
    FSpineClosed, FCrossSectionClosed: boolean;
    FBeginEndCapsMatching: boolean;
    procedure SetNode(Value: TNodeExtrusion);
  public
    constructor Create;

    { Node used. Always assign something non-nil, and generally
      use rest of this class only when this is assigned. }
    property Node: TNodeExtrusion read FNode write SetNode;

    { You have spines from 0 to High.
      Remember that this can be -1, if no spine points are defined.

      You can safely ask SpineXxxTransform about various values between
      0..High. }
    property High: Integer read FHigh;

    { Same thing as @link(TNodeExtrusion.SpineClosed Node.SpineClosed) and
      @link(TNodeExtrusion.CrossSectionClosed Node.CrossSectionClosed),
      just calculated once (when setting Node) for speed.

      @groupBegin }
    property SpineClosed: boolean read FSpineClosed;
    property CrossSectionClosed: boolean read FCrossSectionClosed;
    { @groupEnd }

    { Are begin and end caps at the same place.

      This is a stronger condition
      than just a SpineClosed: whole SpineTransformTo1st must be
      guaranteed the same at the beginning and end. The Extrusion rules are such
      that closed spine -> always produces the same automatic orientation
      calculated at the beginning and end (X, Y, Z vectors in
      TVRMLExtrusion.SpineTransformTo1st implementation). But we also
      have to compare Orientation and Scale factors --- only when they
      also match, the caps match. }
    property BeginEndCapsMatching: boolean read FBeginEndCapsMatching;

    { If Spine > 0, LastY and LastZ must contain what was set here by calling
      SpineTransformTo1st(Spine - 1, LastY, LastZ). }
    procedure SpineTransformTo1st(Spine: Cardinal;
      var LastY, LastZ: TVector3Single;
      out Transform: TMatrix4Single);

    procedure SpineScaleTo1st(Spine: Cardinal;
      out Scale: TVector2Single);

    procedure SpineOrientationTo1st(Spine: Cardinal;
      out Orientation: TVector4Single);
  end;

constructor TVRMLExtrusion.Create;
begin
  inherited;
end;

procedure TVRMLExtrusion.SetNode(Value: TNodeExtrusion);
var
  BeginOrientation, EndOrientation: TVector4Single;
  BeginScale, EndScale: TVector2Single;
begin
  FNode := Value;

  { calculate FHigh }

  if Node.FdSpine.Count = 0 then
  begin
    FHigh := -1;
    { ... and don't even check scale/orientation }
  end else
  if (Node.FdScale.Count = 0) or
     (Node.FdOrientation.Count = 0) then
  begin
    OnWarning(wtMajor, 'VRML/X3D', 'Extrusion has no scale or orientation specified');
    FHigh := -1;
  end else
  begin
    FHigh := Node.FdSpine.Count - 1;

    { We will handle all other scale/orientation counts.
      Excessive scale/orientation values will be ignored.
      If not enough will be available then we'll only use the first one
      (spec says the behavior is undefined then).
      We know that at least one is available, we checked this above. }

    if (Node.FdScale.Count > 1) and
       (Node.FdScale.Count < Node.FdSpine.Count) then
      OnWarning(wtMajor, 'VRML/X3D', 'Extrusion has more scales than 1, but not as much as spines. ' +
        'We''ll use only the first scale.');

    if (Node.FdOrientation.Count > 1) and
       (Node.FdOrientation.Count < Node.FdSpine.Count) then
      OnWarning(wtMajor, 'VRML/X3D', 'Extrusion has more orientations than 1, but not as much as spines. ' +
        'We''ll use only the first orientation.');
  end;

  FSpineClosed := Node.SpineClosed;
  FCrossSectionClosed := Node.CrossSectionClosed;

  if SpineClosed then
  begin
    SpineScaleTo1st(0, BeginScale);
    SpineScaleTo1st(High, EndScale);
    SpineOrientationTo1st(0, BeginOrientation);
    SpineOrientationTo1st(High, EndOrientation);
    FBeginEndCapsMatching :=
      VectorsPerfectlyEqual(BeginOrientation, EndOrientation) and
      VectorsPerfectlyEqual(BeginScale, EndScale);
  end else
    FBeginEndCapsMatching := false;
end;

procedure TVRMLExtrusion.SpineScaleTo1st(Spine: Cardinal;
  out Scale: TVector2Single);
begin
  Assert(Between(Spine, 0, High));
  { So High must be >= 0, by the way.
    Also, High checked that we have at least one scale. }

  if Node.FdScale.Count < Node.FdSpine.Count then
    Scale := Node.FdScale.Items.List^[0] else
    Scale := Node.FdScale.Items.List^[Spine];
end;

procedure TVRMLExtrusion.SpineOrientationTo1st(Spine: Cardinal;
  out Orientation: TVector4Single);
begin
  Assert(Between(Spine, 0, High));
  { So High must be >= 0, by the way.
    Also, High checked that we have at least one orientation. }

  if Node.FdOrientation.Count < Node.FdSpine.Count then
    Orientation := Node.FdOrientation.Items.List^[0] else
    Orientation := Node.FdOrientation.Items.List^[Spine];
end;

procedure TVRMLExtrusion.SpineTransformTo1st(Spine: Cardinal;
  var LastY, LastZ: TVector3Single;
  out Transform: TMatrix4Single);
var
  SpinePoints: TDynVector3SingleArray;

  { Calculate Z by searching for the first non-colinear three spine
    points. @false if not found. }
  function FindFirstNonColinear(out Z: TVector3Single): boolean;
  var
    I: Integer;
  begin
    Result := false;

    for I := 1 to High - 1 do
    begin
      { Try to find first spine point with Z defined as non-zero.
        This follows X3D / VRML spec ("If the Z-axis of the
        first point is undefined..."). }
      Z := VectorProduct(
        VectorSubtract(SpinePoints.List^[I + 1], SpinePoints.List^[I]),
        VectorSubtract(SpinePoints.List^[I - 1], SpinePoints.List^[I]));
      if not ZeroVector(Z) then
      begin
        Result := true;
        break;
      end;
    end;
  end;

  { Calculate Y by searching for the first non-coincident two spine
    points. Sets to (0, 1, 0) if not found.

    Y is always normalized. }
  procedure FindFirstNonCoincident(out Y: TVector3Single);
  var
    I: Integer;
  begin
    for I := 1 to High do
    begin
      Y := VectorSubtract(SpinePoints.List^[I], SpinePoints.List^[I - 1]);
      if not ZeroVector(Y) then
      begin
        NormalizeTo1st(Y);
        Exit;
      end;
    end;
    Y := UnitVector3Single[1];
  end;

  { Calculate Z based on Y (already normalized) if all spine points
    are colinear. }
  function AllColinear(const Y: TVector3Single): TVector3Single;
  var
    AngleRad: Single;
    Rotation, ForPositiveAngleRad, ForNegativeAngleRad: TVector3Single;
  begin
    { Spec: "If the entire spine is collinear,
      the SCP is computed by finding the rotation of a
      vector along the positive Y-axis (v1) to the vector
      formed by the spine points (v2). The Y=0 plane is then rotated
      by this value."

      v2 is actually just our Y variable. }
    AngleRad := AngleRadBetweenNormals(UnitVector3Single[1], Y);
    Rotation := VectorProduct(UnitVector3Single[1], Y);

    if ZeroVector(Rotation) then
    begin
      { This means that Y is actually just equal (0, 1, 0).
        So Z is just (0, 0, 1). }
      Result := UnitVector3Single[2];
    end else
    begin
      { Is the rotation by AngleRad or -AngleRad?
        Lame way of checking this below, we just check which result
        produces back Y when rotated.

        Note: the first implementation was like

          if not VectorsEqual(ForPositiveAngleRad, Y) then
          begin
            AngleRad := -AngleRad;
            Assert(VectorsEqual(ForNegativeAngleRad, Y));
          end;

        but this is obviously unsafe because of floating point errors,
        there was always a chance that both VectorsEqual fail.
        Fixed below to just choose the best one. }
      ForPositiveAngleRad := RotatePointAroundAxisRad( AngleRad, UnitVector3Single[1], Rotation);
      ForNegativeAngleRad := RotatePointAroundAxisRad(-AngleRad, UnitVector3Single[1], Rotation);

      if PointsDistanceSqr(ForPositiveAngleRad, Y) >
         PointsDistanceSqr(ForNegativeAngleRad, Y) then
        AngleRad := -AngleRad;

      Result := RotatePointAroundAxisRad(AngleRad, UnitVector3Single[2], Rotation);
    end;
  end;

  procedure CalculateYZForClosed(out Y, Z: TVector3Single);
  begin
    { Same for Spine = 0 and High, as this is the same point actually. }
    Y := VectorSubtract(SpinePoints.List^[1], SpinePoints.List^[High - 1]);

    if not ZeroVector(Y) then
      NormalizeTo1st(Y) else
      FindFirstNonCoincident(Y);

    Z := VectorProduct(
      VectorSubtract(SpinePoints.List^[1], SpinePoints.List^[0]),
      VectorSubtract(SpinePoints.List^[High - 1], SpinePoints.List^[0]));

    if not ZeroVector(Z) then
      NormalizeTo1st(Z) else
    if FindFirstNonColinear(Z) then
      NormalizeTo1st(Z) else
      Z := AllColinear(Y);
  end;

var
  X, Y, Z: TVector3Single;
  Scale: TVector2Single;
  Orientation: TVector4Single;
begin
  Assert(Between(Spine, 0, High));

  SpinePoints := Node.FdSpine.Items;

  if High = 0 then
  begin
    Y := UnitVector3Single[1];
    Z := UnitVector3Single[2];
  end else
  if Spine = 0 then
  begin
    if SpineClosed then
      CalculateYZForClosed(Y, Z) else
    begin
      Y := VectorSubtract(SpinePoints.List^[1], SpinePoints.List^[0]);
      if not ZeroVector(Y) then
        NormalizeTo1st(Y) else
        FindFirstNonCoincident(Y);

      if FindFirstNonColinear(Z) then
        NormalizeTo1st(Z) else
        Z := AllColinear(Y);
    end;
  end else
  if Integer(Spine) = High then
  begin
    if SpineClosed then
      CalculateYZForClosed(Y, Z) else
    begin
      Y := VectorSubtract(SpinePoints.List^[High], SpinePoints.List^[High - 1]);
      if not ZeroVector(Y) then
        NormalizeTo1st(Y) else
        Y := LastY;

      Z := LastZ;
    end;
  end else
  begin
    { Note that I avoided wasting time on checking SpineClosed
      in this case, and that's good. This is the most common case
      for this routine. }

    Y := VectorSubtract(SpinePoints.List^[Spine + 1], SpinePoints.List^[Spine - 1]);

    if not ZeroVector(Y) then
      NormalizeTo1st(Y) else
      Y := LastY;

    Z := VectorProduct(
      VectorSubtract(SpinePoints.List^[Spine + 1], SpinePoints.List^[Spine]),
      VectorSubtract(SpinePoints.List^[Spine - 1], SpinePoints.List^[Spine]));

    if not ZeroVector(Z) then
      NormalizeTo1st(Z) else
      Z := LastZ;
  end;

  if (Spine > 0) and (VectorDotProduct(LastZ, Z) < 0) then
  begin
    VectorNegateTo1st(Z);
  end;

  LastY := Y;
  LastZ := Z;

  X := VectorProduct(Y, Z);

  SpineScaleTo1st(Spine, Scale);
  SpineOrientationTo1st(Spine, Orientation);

  Transform := MatrixMult(
    TransformToCoordsMatrix(SpinePoints.List^[Spine], X, Y, Z),
    MatrixMult(RotationMatrixRad(Orientation[3],
      Orientation[0], Orientation[1], Orientation[2]),
      ScalingMatrix(Vector3Single(Scale[0], 1, Scale[1]))));
end;
