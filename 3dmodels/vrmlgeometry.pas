{
  Copyright 2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(Utilities for dealing with more difficult VRML geometry
  nodes.)

  For some geometry nodes, calculating their vertices (and other properties)
  is non-trivial enough to be separated into a unit. We want to reuse
  this by VRMLOpenGLRenderer (file
  ../3dmodels.gl/vrmlopenglrenderer_render_specificnodes.inc)
  and by BoundingBox calculation (file vrmlnodes_boundingboxes.inc),
  and possibly more in the future.

  Normal users of our engine should not use this unit directly
  (it's functionality is indirectly available to you when rendering,
  or calculating collisions with this geometry).
  Interface of this unit and it's classes is unstable, suited only to the
  current needs of other parts of the engine. }
unit VRMLGeometry;

interface

uses VectorMath, VRMLNodes;

type
  TVRMLExtrusion = class
  private
    FHigh: Integer;
    FNode: TNodeExtrusion;
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

implementation

uses VRMLErrors, KambiUtils;

constructor TVRMLExtrusion.Create;
begin
  inherited;
end;

procedure TVRMLExtrusion.SetNode(Value: TNodeExtrusion);
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
    VRMLNonFatalError('Extrusion has no scale or orientation specified');
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
      VRMLNonFatalError('Extrusion has more scales than 1, but not as much as spines. ' +
        'We''ll use only the first scale.');

    if (Node.FdOrientation.Count > 1) and
       (Node.FdOrientation.Count < Node.FdSpine.Count) then
      VRMLNonFatalError('Extrusion has more orientations than 1, but not as much as spines. ' +
        'We''ll use only the first orientation.');
  end;
end;

procedure TVRMLExtrusion.SpineScaleTo1st(Spine: Cardinal;
  out Scale: TVector2Single);
begin
  Assert(Between(Spine, 0, High));
  { So High must be >= 0, by the way.
    Also, High checked that we have at least one scale. }

  if Node.FdScale.Count < Node.FdSpine.Count then
    Scale := Node.FdScale.Items.Items[0] else
    Scale := Node.FdScale.Items.Items[Spine];
end;

procedure TVRMLExtrusion.SpineOrientationTo1st(Spine: Cardinal;
  out Orientation: TVector4Single);
begin
  Assert(Between(Spine, 0, High));
  { So High must be >= 0, by the way.
    Also, High checked that we have at least one orientation. }

  if Node.FdOrientation.Count < Node.FdSpine.Count then
    Orientation := Node.FdOrientation.Items.Items[0] else
    Orientation := Node.FdOrientation.Items.Items[Spine];
end;

procedure TVRMLExtrusion.SpineTransformTo1st(Spine: Cardinal;
  var LastY, LastZ: TVector3Single;
  out Transform: TMatrix4Single);
var
  SpinePoints: TDynVector3SingleArray;

  { Check is spine closed. }
  function Closed: boolean;
  begin
    Result := VectorsPerfectlyEqual(
      SpinePoints.Items[High], SpinePoints.Items[0]);
  end;

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
        VectorSubtract(SpinePoints.Items[I + 1], SpinePoints.Items[I]),
        VectorSubtract(SpinePoints.Items[I - 1], SpinePoints.Items[I]));
      if not IsZeroVector(Z) then
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
      Y := VectorSubtract(SpinePoints.Items[I], SpinePoints.Items[I - 1]);
      if not IsZeroVector(Y) then
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
    Rotation: TVector3Single;
  begin
    { Spec: "If the entire spine is collinear,
      the SCP is computed by finding the rotation of a
      vector along the positive Y-axis (v1) to the vector
      formed by the spine points (v2). The Y=0 plane is then rotated
      by this value."

      v2 is actually just our Y variable. }
    AngleRad := AngleRadBetweenNormals(UnitVector3Single[1], Y);
    Rotation := VectorProduct(UnitVector3Single[1], Y);

    if IsZeroVector(Rotation) then
    begin
      { This means that Y is actually just equal (0, 1, 0).
        So Z is just (0, 0, 1). }
      Result := UnitVector3Single[2];
    end else
    begin
      { Now is the rotation by AngleRad or -AngleRad?
        Lame way of checking this below. }
      if not VectorsEqual(
        RotatePointAroundAxisRad(AngleRad, UnitVector3Single[1], Rotation), Y) then
      begin
        AngleRad := -AngleRad;
        Assert(VectorsEqual(
          RotatePointAroundAxisRad(AngleRad, UnitVector3Single[1], Rotation), Y));
      end;

      Result := RotatePointAroundAxisRad(AngleRad, UnitVector3Single[2], Rotation);
    end;
  end;

  procedure CalculateYZForClosed(out Y, Z: TVector3Single);
  begin
    { Same for Spine = 0 and High, as this is the same point actually. }
    Y := VectorSubtract(SpinePoints.Items[1], SpinePoints.Items[High - 1]);
    if not IsZeroVector(Y) then
      NormalizeTo1st(Y) else
      FindFirstNonCoincident(Y);

    Z := VectorProduct(
      VectorSubtract(SpinePoints.Items[1], SpinePoints.Items[0]),
      VectorSubtract(SpinePoints.Items[High - 1], SpinePoints.Items[0]));

    if IsZeroVector(Z) then
    begin
      if FindFirstNonColinear(Z) then
        NormalizeTo1st(Z) else
        Z := AllColinear(Y);
    end else
      NormalizeTo1st(Z);
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
    if Closed then
      CalculateYZForClosed(Y, Z) else
    begin
      Y := VectorSubtract(SpinePoints.Items[1], SpinePoints.Items[0]);
      if not IsZeroVector(Y) then
        NormalizeTo1st(Y) else
        FindFirstNonCoincident(Y);

      if FindFirstNonColinear(Z) then
        NormalizeTo1st(Z) else
        Z := AllColinear(Y);
    end;
  end else
  if Integer(Spine) = High then
  begin
    if Closed then
      CalculateYZForClosed(Y, Z) else
    begin
      Y := VectorSubtract(SpinePoints.Items[High], SpinePoints.Items[High - 1]);
      if not IsZeroVector(Y) then
        NormalizeTo1st(Y) else
        Y := LastY;

      Z := LastZ;
    end;
  end else
  begin
    { Note that I avoided wasting time on checking Closed
      in this case, and that's good. This is the most common case
      for this routine. }

    Y := VectorSubtract(SpinePoints.Items[Spine + 1], SpinePoints.Items[Spine - 1]);
    if not IsZeroVector(Y) then
      NormalizeTo1st(Y) else
      Y := LastY;

    Z := VectorProduct(
      VectorSubtract(SpinePoints.Items[Spine + 1], SpinePoints.Items[Spine]),
      VectorSubtract(SpinePoints.Items[Spine - 1], SpinePoints.Items[Spine]));

    if IsZeroVector(Z) then
      Z := LastZ else
      NormalizeTo1st(Z);
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

  Transform := MultMatrices(
    TransformToCoordsMatrix(SpinePoints.Items[Spine], X, Y, Z),
    MultMatrices(RotationMatrixRad(Orientation[3],
      Orientation[0], Orientation[1], Orientation[2]),
      ScalingMatrix(Vector3Single(Scale[0], 1, Scale[1]))));
end;

end.
