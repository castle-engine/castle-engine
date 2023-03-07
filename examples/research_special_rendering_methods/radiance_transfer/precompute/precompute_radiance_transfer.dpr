{
  Copyright 2008-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Process arbitrary model for Precomputed Radiance Transfer.
  See README.md for detailed info. }
program precompute_radiance_transfer;

uses SysUtils, CastleUtils, CastleVectors, CastleSceneCore, X3DNodes, X3DLoad,
  CastleInternalSphereSampling, CastleProgress, CastleProgressConsole, CastleColors,
  CastleInternalSphericalHarmonics, CastleParameters, CastleTimeUtils, CastleShapes;

var
  Scene: TCastleSceneCore;
  Normals: TVector3List;
  SHBasisCount: Integer = 25;
  RaysPerVertex: Cardinal = 1000;

procedure ComputeTransfer(RadianceTransfer: TVector3List;
  Coord: TVector3List; const Transform: TMatrix4;
  const DiffuseColor: TVector3);
var
  I, J, SHBase: Integer;
  V, N: TVector3;
  RayDirectionPT: TVector2;
  RayDirection: TVector3;
  VertexTransfer: PVector3;
begin
  RadianceTransfer.Count := Coord.Count * SHBasisCount;

  Progress.Init(Coord.Count, 'Computing transfer for one shape');
  try
    for I := 0 to Coord.Count - 1 do
    begin
      VertexTransfer := Addr(RadianceTransfer.List^[I * SHBasisCount]);

      { V = scene-space vertex coord }
      V := Transform.MultPoint(Coord.List^[I]);

      { N = scene-space normal coord
        TODO: MultDirection will not work under non-uniform scaling matrix correctly. }
      N := Transform.MultDirection(Normals.List^[I]).Normalize;

      for SHBase := 0 to SHBasisCount - 1 do
        VertexTransfer[SHBase] := TVector3.Zero;

      { In some nasty cases, smoothed normal may be zero, see e.g. spider_queen
        legs. Leave VertexTransfer[SHBase] as zeros in this case. }

      if not N.IsZero then
      begin
        { Integrate over hemisphere (around N). Actually, we could integrate
          over the sphere, but this is (on most models) a waste of time
          (= a waste of half rays), since most rays outside of N hemisphere
          would go inside the model (where usually you do not hit anything). }

        for J := 0 to RaysPerVertex - 1 do
        begin
          RayDirectionPT := RandomHemispherePointConst;
          RayDirection := PhiThetaToXYZ(RayDirectionPT, N);
          if not Scene.InternalOctreeVisibleTriangles.IsRayCollision(V, RayDirection,
            nil, true { yes, ignore margin at start, to not hit V },
            nil) then
          begin
            { Previous RayDirectionPT assumed that (0, 0, 1) is N.
              That is, RayDirectionPT was in local vertex coords, not
              in actual world coords (like N). So now transform back
              RayDirection to RayDirectionPT, to get RayDirectionPT in world coords.
              This is an extremely important operation, without this SHBasis
              is calculated with wrong arguments, and the models are
              not lighted as they should with PRT. }

            RayDirectionPT := XYZToPhiTheta(RayDirection);
            for SHBase := 0 to SHBasisCount - 1 do
              { TVector3.DotProduct below must be >= 0, since RayDirection was
                chosen at random within hemisphere around N.
                So no need to do Max(0, TVector3.DotProduct(...)) below.

                We calculate only red component here, the rest will
                be copied from it later. }
              VertexTransfer[SHBase].X += SHBasis(SHBase, RayDirectionPT) *
                TVector3.DotProduct(N, RayDirection);
          end;
        end;

        for SHBase := 0 to SHBasisCount - 1 do
        begin
          { VertexTransfer[SHBase][0] is an integral over hemisphere,
            so normalize. }
          VertexTransfer[SHBase].X *= 2 * Pi / RaysPerVertex;

          { Calculate Green, Blue components of VertexTransfer
            (just copy from Red, since we didn't take DiffuseColor
            into account yet). }
          VertexTransfer[SHBase].Y := VertexTransfer[SHBase].X;
          VertexTransfer[SHBase].Z := VertexTransfer[SHBase].X;

          { Multiply by BRDF = DiffuseColor (since
            BRDF is simply constant, so we can simply multiply here).
            For diffuse surface, BRDF is just = DiffuseColor. }
          VertexTransfer[SHBase].X *= DiffuseColor[0];
          VertexTransfer[SHBase].Y *= DiffuseColor[1];
          VertexTransfer[SHBase].Z *= DiffuseColor[2];
        end;
      end;

      Progress.Step;
    end;
  finally Progress.Fini end;
end;

function MainColor(State: TX3DGraphTraverseState): TVector3;
var
  MaterialInfo: TMaterialInfo;
begin
  MaterialInfo := State.MaterialInfo;
  if MaterialInfo <> nil then
    Result := MaterialInfo.MainColor
  else
    Result := WhiteRGB;
end;

const
  Options: array[0..1] of TOption =
  ( (Short: 'c'; Long: 'sh-basis-count'; Argument: oaRequired),
    (Short: 'r'; Long: 'rays-per-vertex'; Argument: oaRequired)
  );

  procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
    const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
  begin
    case OptionNum of
      0: SHBasisCount := StrToInt(Argument);
      1: RaysPerVertex := StrToInt(Argument);
    end;
  end;

var
  Geometry: TAbstractGeometryNode;
  State: TX3DGraphTraverseState;
  RadianceTransfer: TVector3List;
  S: string;
  TimeStart: TProcessTimerResult;
  Seconds: TFloatTime;
  ShapeList: TShapeList;
  Shape: TShape;
begin
  Parameters.Parse(Options, @OptionProc, nil);
  Parameters.CheckHigh(2);

  Progress.UserInterface := ProgressConsoleInterface;

  Scene := TCastleSceneCore.Create(nil);
  try
    Scene.Load(Parameters[1]);
    Scene.Spatial := [ssVisibleTriangles];

    TimeStart := ProcessTimer;

    ShapeList := Scene.Shapes.TraverseList(false);
    for Shape in ShapeList do
    begin
      Geometry := Shape.Geometry;
      State := Shape.State;

      if Geometry is TAbstractComposedGeometryNode then
        RadianceTransfer := TAbstractComposedGeometryNode(Geometry).FdRadianceTransfer.Items else
      if Geometry is TIndexedFaceSetNode_1 then
        RadianceTransfer := TIndexedFaceSetNode_1(Geometry).FdRadianceTransfer.Items else
        RadianceTransfer := nil;

      { If we used Proxy to get Geometry, then don't calculate PRT.
        We could calculate it, but it would not be used: it would not
        be saved to the actual file, only to the temporary Proxy instance.
        To make it work, we may in the future implement actually inserting
        Proxy result into the VRML file. }
      if Geometry <> Shape.OriginalGeometry then
        RadianceTransfer := nil;

      if RadianceTransfer <> nil then
      begin
        { For PRT, we need a normal per-vertex, so always calculate
          smooth normals. Simple, and thanks to CastleInternalNormals
          this works for all VRML/X3D coord-based nodes (and only for
          those RadianceTransfer is defined). }
        Normals := Shape.NormalsSmooth(true);
        ComputeTransfer(RadianceTransfer,
          Geometry.InternalCoordinates(State).Items,
          State.Transformation.Transform, MainColor(State));
      end;
    end;

    Seconds := ProcessTimerSeconds(ProcessTimer, TimeStart);
    S := Format('SH bases %d, rays per vertex %d, done in %f secs',
      [SHBasisCount, RaysPerVertex, Seconds]);

    Writeln('Precomputing finished: ', S);

    SaveNode(Scene.RootNode, Parameters[2],
      'radianceTransfer computed by precompute_radiance_transfer: ' + S);
  finally FreeAndNil(Scene) end;
end.
