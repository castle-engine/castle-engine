{ This is part of CastleBoxes unit implementation that uses TGenericFloat,
  TVector3GenericFloat etc. types, instead of normal Single/Double,
  TVector3Single/TVector3Double. }

function IsCenteredBox3DPlaneCollision(
  const BoxHalfSize: TVector3GenericFloat;
  const Plane: TVector4GenericFloat): boolean;
{ Implementation of this is based on
  [http://jgt.akpeters.com/papers/AkenineMoller01/tribox.html]
  planeBoxOverlap routine, by Tomas Akenine-Moller,
  mentioned in his paper [http://jgt.akpeters.com/papers/AkenineMoller01/]
  about "Fast 3D Triangle-Box Overlap Testing", downloadable from
  [http://www.cs.lth.se/home/Tomas_Akenine_Moller/pubs/tribox.pdf].

  The idea: we need to test plane equation with only two points
  (instead of eight points, as in naive version). Think about the plane
  normal vector; imagine 8 box points projected on this vector; now
  we can find 2 box points, one that has minimal value when projected
  on normal vector, and one that has maximum value. Now you need to test
  is the plane between these two points. }

{ **************** About precision: *********************

  Tests (see TTestCastleBoxes.TestIsBox3DTriangleCollisionEpsilonsSingle)
  show that this calculation should really be done on at least Double precision.
  The values for these tests were taken from debugging behavior on
  castle.wrl test VRML model, so yes, these errors produce real errors
  (they make some valid triangles not appear at all in the octree, so collision detection
  and picking fail on these areas).
  Otherwise floating point errors will force you to define really large EqualityEpsilon:
  when trying to set this experimentally, I had to set EqualityEpsilon = 1e-3 (even
  EqualityEpsilon = 1e-4 was still too small epsilon !).

  With Double, I can use EqualityEpsilon below. OK, that's stil a large epsilon...
  You can test on VRML models like malfunction/trunk/vrmls/wawoz.wrl:
  view3dscene (when compiled with -dDEBUG) with fail loading
  (Assert(AddedSomewhere) will trigger) with too small epsilon. }
{$define EqualityEpsilon := Box3DPlaneCollisionEqualityEpsilon}

var
  I: Integer;
  VMin, VMax: TVector3Double;
begin
  for I := 0 to 2 do
    if Plane[I] > 0 then
    begin
      VMin[I] := -BoxHalfSize[I];
      VMax[I] :=  BoxHalfSize[I];
    end else
    begin
      VMin[I] :=  BoxHalfSize[I];
      VMax[I] := -BoxHalfSize[I];
    end;

  { If VMin is above the plane (plane equation is > 0), then VMax
    is also above, no need to test anything else. }
  if Plane[0] * VMin[0] +
     Plane[1] * VMin[1] +
     Plane[2] * VMin[2] +
     Plane[3] > EqualityEpsilon then
    Exit(false);

  { So VMin is <= plane. So if VMax is >= 0, then there's a collision. }
  Result :=  Plane[0] * VMax[0] +
             Plane[1] * VMax[1] +
             Plane[2] * VMax[2] +
             Plane[3] >= -EqualityEpsilon;
end;

{$undef EqualityEpsilon}