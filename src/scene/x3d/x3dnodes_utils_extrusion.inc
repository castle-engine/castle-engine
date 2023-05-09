{%MainUnit x3dnodes.pas}
{
  Copyright 2008-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}


type
  { Utility for dealing with Extrusion.

   Calculating vertices (and other properties) of Extrusion
   is non-trivial enough to be separated into this file. We want to reuse
   this by TExtrusionNode.BoundingBox calculation,
   TExtrusionNode.Triangulate etc. }
  TVRMLExtrusion = class
  strict private
    FHigh: Integer;
    FNode: TExtrusionNode;
    FSpineClosed, FCrossSectionClosed: boolean;
    FCrossSectionOmit: Cardinal;
    FBeginEndCapsMatching: boolean;
    procedure SetNode(Value: TExtrusionNode);
  public
    constructor Create;

    { Node used. Always assign something non-nil, and generally
      use rest of this class only when this is assigned. }
    property Node: TExtrusionNode read FNode write SetNode;

    { You have spines from 0 to High.
      Remember that this can be -1, if no spine points are defined.

      You can safely ask SpineXxxTransform about various values between
      0..High. }
    property High: Integer read FHigh;

    { Same thing as @link(TExtrusionNode.SpineClosed Node.SpineClosed) and
      @link(TExtrusionNode.CrossSectionClosed Node.CrossSectionClosed),
      just calculated once (when setting Node) for speed.

      @groupBegin }
    property SpineClosed: boolean read FSpineClosed;
    property CrossSectionClosed: boolean read FCrossSectionClosed;
    { @groupEnd }

    { If we should omit a first (or last) vertex when rendering caps,
      this is 1, otherwise it is 0.
      When crossSection is closed, we really should do it,
      otherwise our concave triangulation may get confused ---
      see extrusion_concave_cap.wrl testcase. }
    property CrossSectionOmit: Cardinal read FCrossSectionOmit;

    { Are begin and end caps at the same place.

      This is a stronger condition
      than just a SpineClosed: whole SpineTransformVar must be
      guaranteed the same at the beginning and end. The Extrusion rules are such
      that closed spine -> always produces the same automatic orientation
      calculated at the beginning and end (X, Y, Z vectors in
      TVRMLExtrusion.SpineTransformVar implementation). But we also
      have to compare Orientation and Scale factors --- only when they
      also match, the caps match. }
    property BeginEndCapsMatching: boolean read FBeginEndCapsMatching;

    { If Spine > 0, LastY and LastZ must contain what was set here by calling
      SpineTransformVar(Spine - 1, LastY, LastZ). }
    procedure SpineTransformVar(Spine: Cardinal;
      var LastY, LastZ: TVector3;
      out Transform: TMatrix4);

    procedure SpineScaleVar(Spine: Cardinal;
      out Scale: TVector2);

    procedure SpineOrientationVar(Spine: Cardinal;
      out Orientation: TVector4);
  end;

constructor TVRMLExtrusion.Create;
begin
  inherited;
end;

procedure TVRMLExtrusion.SetNode(Value: TExtrusionNode);
var
  BeginOrientation, EndOrientation: TVector4;
  BeginScale, EndScale: TVector2;
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
    WritelnWarning('VRML/X3D', 'Extrusion has no scale or orientation specified');
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
      WritelnWarning('VRML/X3D', 'Extrusion has more scales than 1, but not as much as spines. ' +
        'We''ll use only the first scale.');

    if (Node.FdOrientation.Count > 1) and
       (Node.FdOrientation.Count < Node.FdSpine.Count) then
      WritelnWarning('VRML/X3D', 'Extrusion has more orientations than 1, but not as much as spines. ' +
        'We''ll use only the first orientation.');
  end;

  FSpineClosed := Node.SpineClosed;
  FCrossSectionClosed := Node.CrossSectionClosed;

  if FCrossSectionClosed then
    FCrossSectionOmit := 1 else
    FCrossSectionOmit := 0;

  if SpineClosed then
  begin
    SpineScaleVar(0, BeginScale);
    SpineScaleVar(High, EndScale);
    SpineOrientationVar(0, BeginOrientation);
    SpineOrientationVar(High, EndOrientation);
    FBeginEndCapsMatching :=
      TVector4.PerfectlyEquals(BeginOrientation, EndOrientation) and
      TVector2.PerfectlyEquals(BeginScale, EndScale);
  end else
    FBeginEndCapsMatching := false;
end;

procedure TVRMLExtrusion.SpineScaleVar(Spine: Cardinal;
  out Scale: TVector2);
begin
  Assert(Between(Spine, 0, High));
  { So High must be >= 0, by the way.
    Also, High checked that we have at least one scale. }

  if Node.FdScale.Count < Node.FdSpine.Count then
    Scale := Node.FdScale.Items.L[0] else
    Scale := Node.FdScale.Items.L[Spine];
end;

procedure TVRMLExtrusion.SpineOrientationVar(Spine: Cardinal;
  out Orientation: TVector4);
begin
  Assert(Between(Spine, 0, High));
  { So High must be >= 0, by the way.
    Also, High checked that we have at least one orientation. }

  if Node.FdOrientation.Count < Node.FdSpine.Count then
    Orientation := Node.FdOrientation.Items.L[0] else
    Orientation := Node.FdOrientation.Items.L[Spine];
end;

procedure TVRMLExtrusion.SpineTransformVar(Spine: Cardinal;
  var LastY, LastZ: TVector3;
  out Transform: TMatrix4);
var
  SpinePoints: TVector3List;

  { Calculate Z by searching for the first non-colinear three spine
    points. @false if not found. }
  function FindFirstNonColinear(out Z: TVector3): boolean;
  var
    I: Integer;
  begin
    Result := false;

    for I := 1 to High - 1 do
    begin
      { Try to find first spine point with Z defined as non-zero.
        This follows X3D / VRML spec ("If the Z-axis of the
        first point is undefined..."). }
      Z := TVector3.CrossProduct(
        (SpinePoints.L[I + 1] - SpinePoints.L[I]),
        (SpinePoints.L[I - 1] - SpinePoints.L[I]));
      if not Z.IsZero then
      begin
        Result := true;
        break;
      end;
    end;
  end;

  { Calculate Y by searching for the first non-coincident two spine
    points. Sets to (0, 1, 0) if not found.

    Y is always normalized. }
  procedure FindFirstNonCoincident(out Y: TVector3);
  var
    I: Integer;
  begin
    for I := 1 to High do
    begin
      Y := (SpinePoints.L[I] - SpinePoints.L[I - 1]);
      if not Y.IsZero then
      begin
        Y := Y.Normalize;
        Exit;
      end;
    end;
    Y := TVector3.One[1];
  end;

  { Calculate Z based on Y (already normalized) if all spine points
    are colinear. }
  function AllColinear(const Y: TVector3): TVector3;
  var
    AngleRad: Single;
    Rotation, ForPositiveAngleRad, ForNegativeAngleRad: TVector3;
  begin
    { Spec: "If the entire spine is collinear,
      the SCP is computed by finding the rotation of a
      vector along the positive Y-axis (v1) to the vector
      formed by the spine points (v2). The Y=0 plane is then rotated
      by this value."

      v2 is actually just our Y variable. }
    AngleRad := AngleRadBetweenNormals(TVector3.One[1], Y);
    Rotation := TVector3.CrossProduct(TVector3.One[1], Y);

    if Rotation.IsZero then
    begin
      { This means that Y is actually just equal (0, 1, 0).
        So Z is just (0, 0, 1). }
      Result := TVector3.One[2];
    end else
    begin
      { Is the rotation by AngleRad or -AngleRad?
        Lame way of checking this below, we just check which result
        produces back Y when rotated.

        Note: the first implementation was like

          if not TVector3.Equals(ForPositiveAngleRad, Y) then
          begin
            AngleRad := -AngleRad;
            Assert(TVector3.Equals(ForNegativeAngleRad, Y));
          end;

        but this is obviously unsafe because of floating point errors,
        there was always a chance that both TVector3.Equals fail.
        Fixed below to just choose the best one. }
      ForPositiveAngleRad := RotatePointAroundAxisRad( AngleRad, TVector3.One[1], Rotation);
      ForNegativeAngleRad := RotatePointAroundAxisRad(-AngleRad, TVector3.One[1], Rotation);

      if PointsDistanceSqr(ForPositiveAngleRad, Y) >
         PointsDistanceSqr(ForNegativeAngleRad, Y) then
        AngleRad := -AngleRad;

      Result := RotatePointAroundAxisRad(AngleRad, TVector3.One[2], Rotation);
    end;
  end;

  procedure CalculateYZForClosed(out Y, Z: TVector3);
  begin
    { Same for Spine = 0 and High, as this is the same point actually. }
    Y := SpinePoints.L[1] - SpinePoints.L[High - 1];

    if not Y.IsZero then
      Y := Y.Normalize else
      FindFirstNonCoincident(Y);

    Z := TVector3.CrossProduct(
      (SpinePoints.L[1] - SpinePoints.L[0]),
      (SpinePoints.L[High - 1] - SpinePoints.L[0]));

    if not Z.IsZero then
      Z := Z.Normalize else
    if FindFirstNonColinear(Z) then
      Z := Z.Normalize else
      Z := AllColinear(Y);
  end;

var
  X, Y, Z: TVector3;
  Scale: TVector2;
  Orientation: TVector4;
begin
  Assert(Between(Spine, 0, High));

  SpinePoints := Node.FdSpine.Items;

  if High = 0 then
  begin
    Y := TVector3.One[1];
    Z := TVector3.One[2];
  end else
  if Spine = 0 then
  begin
    if SpineClosed then
      CalculateYZForClosed(Y, Z) else
    begin
      Y := (SpinePoints.L[1] - SpinePoints.L[0]);
      if not Y.IsZero then
        Y := Y.Normalize else
        FindFirstNonCoincident(Y);

      if FindFirstNonColinear(Z) then
        Z := Z.Normalize else
        Z := AllColinear(Y);
    end;
  end else
  if Integer(Spine) = High then
  begin
    if SpineClosed then
      CalculateYZForClosed(Y, Z) else
    begin
      Y := (SpinePoints.L[High] - SpinePoints.L[High - 1]);
      if not Y.IsZero then
        Y := Y.Normalize else
        Y := LastY;

      Z := LastZ;
    end;
  end else
  begin
    { Note that I avoided wasting time on checking SpineClosed
      in this case, and that's good. This is the most common case
      for this routine. }

    Y := (SpinePoints.L[Spine + 1] - SpinePoints.L[Spine - 1]);

    if not Y.IsZero then
      Y := Y.Normalize else
      Y := LastY;

    Z := TVector3.CrossProduct(
      (SpinePoints.L[Spine + 1] - SpinePoints.L[Spine]),
      (SpinePoints.L[Spine - 1] - SpinePoints.L[Spine]));

    if not Z.IsZero then
      Z := Z.Normalize else
      Z := LastZ;
  end;

  if (Spine > 0) and (TVector3.DotProduct(LastZ, Z) < 0) then
    Z := -Z;

  LastY := Y;
  LastZ := Z;

  X := TVector3.CrossProduct(Y, Z);

  SpineScaleVar(Spine, Scale);
  SpineOrientationVar(Spine, Orientation);

  Transform :=
    TransformToCoordsMatrix(SpinePoints.L[Spine], X, Y, Z) *
    RotationMatrixRad(Orientation[3],
      Orientation[0], Orientation[1], Orientation[2]) *
    ScalingMatrix(Vector3(Scale[0], 1, Scale[1]));
end;
