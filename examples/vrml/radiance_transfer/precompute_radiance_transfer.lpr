{
  Copyright 2008-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Process arbitrary model for PRT. Computes and adds "radianceTransfer"
  field to all geometry nodes (descending from X3DComposedGeometryNode,
  this includes most often used nodes).

  $1 is the source model, $2 is the output model (output is always VRML/X3D,
  so use .wrl/.x3dv extension).

  --sh-basis-count / -c COUNT

    Says how much basis SH functions to use. PRT paper advices between
    9 and 25.

  --rays-per-vertex / -r COUNT

    How many rays per vertex to generate. This linearly affects the speed
    of the program. Default if 1000, PRT paper advices 10 * 1000 to 30 * 1000
    for best effect.

  TODO: for now, radianceTransfer is calculated for whole model.
  This means that self-shadowing takes whole model into account,
  but also that whole model must remain static (or radianceTransfer must
  be animated along with coords).

  Alternative approach is possible: calculate radianceTransfer only
  for this specific shape. Then shape must stay static (or it's
  radianceTransfer must be animated along with it's coords), but it can
  move with respect to other shapes. But note that then self-shadowing
  takes only this shape into account... TODO: make this possible,
  and document on kambi_vrml_extensions.

  We compute radianceTransfer in scene space (not in local shape
  space). This is important, otherwise incoming light SH (calculated
  when rendering at every frame) would have to be transformed (rotated)
  for each shape. Right now, it only has to be rotated once, for each scene.

  Note that your geometry nodes shouldn't use DEF/USE mechanism.
  If the same shape is instantiated many times, it will have the same
  radianceTransfer. Which is bad, since self-shadowing may be different
  on different instances...
  TODO: move to kambi_vrml_extensions docs.
}

program precompute_radiance_transfer;

uses SysUtils, KambiUtils, VectorMath, VRMLScene, VRMLNodes,
  SphereSampling, ProgressUnit, ProgressConsole,
  SphericalHarmonics, ParseParametersUnit, KambiTimeUtils, VRMLShape;

var
  Scene: TVRMLScene;
  Normals: TDynVector3SingleArray;
  SHBasisCount: Integer = 25;
  RaysPerVertex: Cardinal = 1000;

procedure ComputeTransfer(RadianceTransfer: TDynVector3SingleArray;
  Coord: TDynVector3SingleArray; const Transform: TMatrix4Single;
  const DiffuseColor: TVector3Single);
var
  I, J, SHBase: Integer;
  V, N: TVector3Single;
  RayVectorPT: TVector2Single;
  RayVector: TVector3Single;
  VertexTransfer: PVector3Single;
begin
  RadianceTransfer.Count := Coord.Count * SHBasisCount;

  Progress.Init(Coord.Count, 'Computing transfer for one shape');
  try
    for I := 0 to Coord.Count - 1 do
    begin
      VertexTransfer := RadianceTransfer.Pointers[I * SHBasisCount];

      { V = scene-space vertex coord }
      V := MatrixMultPoint(Transform, Coord.Items[I]);

      { N = scene-space normal coord
        TODO: MatrixMultDirection will not work under non-uniform scaling
        matrix correctly. }
      N := Normalized(MatrixMultDirection(Transform, Normals.Items[I]));

      for SHBase := 0 to SHBasisCount - 1 do
        VertexTransfer[SHBase] := ZeroVector3Single;

      { In some nasty cases, smoothed normal may be zero, see e.g. spider_queen
        legs. Leave VertexTransfer[SHBase] as zeros in this case. }

      if not ZeroVector(N) then
      begin
        { Integrate over hemisphere (around N). Actually, we could integrate
          over the sphere, but this is (on most models) a waste of time
          (= a waste of half rays), since most rays outside of N hemisphere
          would go inside the model (where usually you do not hit anything). }

        for J := 0 to RaysPerVertex - 1 do
        begin
          RayVectorPT := RandomHemispherePointConst;
          RayVector := PhiThetaToXYZ(RayVectorPT, N);
          if not Scene.OctreeVisibleTriangles.IsRayCollision(V, RayVector,
            nil, true { yes, ignore margin at start, to not hit V },
            nil) then
          begin
            { Previous RayVectorPT assumed that (0, 0, 1) is N.
              That is, RayVectorPT was in local vertex coords, not
              in actual world coords (like N). So now transform back
              RayVector to RayVectorPT, to get RayVectorPT in world coords.
              This is an extremely important operation, without this SHBasis
              is calculated with wrong arguments, and the models are
              not lighted as they should with PRT. }

            RayVectorPT := XYZToPhiTheta(RayVector);
            for SHBase := 0 to SHBasisCount - 1 do
              { VectorDotProduct below must be >= 0, since RayVector was
                chosen at random within hemisphere around N.
                So no need to do Max(0, VectorDotProduct(...)) below.

                We calculate only red component here, the rest will
                be copied from it later. }
              VertexTransfer[SHBase][0] += SHBasis(SHBase, RayVectorPT) *
                VectorDotProduct(N, RayVector);
          end;
        end;

        for SHBase := 0 to SHBasisCount - 1 do
        begin
          { VertexTransfer[SHBase][0] is an integral over hemisphere,
            so normalize. }
          VertexTransfer[SHBase][0] *= 2 * Pi / RaysPerVertex;

          { Calculate Green, Blue components of VertexTransfer
            (just copy from Red, since we didn't take DiffuseColor
            into account yet). }
          VertexTransfer[SHBase][1] := VertexTransfer[SHBase][0];
          VertexTransfer[SHBase][2] := VertexTransfer[SHBase][0];

          { Multiply by BRDF = DiffuseColor (since
            BRDF is simply constant, so we can simply multiply here).
            For diffuse surface, BRDF is just = DiffuseColor. }
          VertexTransfer[SHBase][0] *= DiffuseColor[0];
          VertexTransfer[SHBase][1] *= DiffuseColor[1];
          VertexTransfer[SHBase][2] *= DiffuseColor[2];
        end;
      end;

      Progress.Step;
    end;
  finally Progress.Fini end;
end;

function DiffuseColor(State: TVRMLGraphTraverseState): TVector3Single;
var
  M1: TNodeMaterial_1;
  M2: TNodeMaterial_2;
begin
  if State.ShapeNode <> nil then
  begin
    M2 := State.ShapeNode.Material;
    if M2 <> nil then
      Result := M2.FdDiffuseColor.Value else
      { Default VRML 2.0 lighting properties. }
      Result := DefaultMaterialDiffuseColor;
  end else
  begin
    M1 := State.LastNodes.Material;
    Result := M1.DiffuseColor3Single(0);
  end;
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
  SI: TVRMLShapeTreeIterator;
  Geometry: TVRMLGeometryNode;
  State: TVRMLGraphTraverseState;
  RadianceTransfer: TDynVector3SingleArray;
  S: string;
begin
  ParseParameters(Options, @OptionProc, nil);
  Parameters.CheckHigh(2);

  Progress.UserInterface := ProgressConsoleInterface;

  Scene := TVRMLScene.Create(nil);
  try
    Scene.Load(Parameters[1]);
    Scene.TriangleOctreeProgressTitle := 'Building octree';
    Scene.Spatial := [ssVisibleTriangles];

    ProcessTimerBegin;

    SI := TVRMLShapeTreeIterator.Create(Scene.Shapes, false);
    try
      while SI.GetNext do
      begin
        Geometry := SI.Current.Geometry;
        State := SI.Current.State;

        if Geometry is TNodeX3DComposedGeometryNode then
          RadianceTransfer := TNodeX3DComposedGeometryNode(Geometry).FdRadianceTransfer.Items else
        if Geometry is TNodeIndexedFaceSet_1 then
          RadianceTransfer := TNodeIndexedFaceSet_1(Geometry).FdRadianceTransfer.Items else
          RadianceTransfer := nil;

        { If we used Proxy to get Geometry, then don't calculate PRT.
          We could calculate it, but it would not be used: it would not
          be saved to the actual file, only to the temporary Proxy instance.
          To make it work, we may in the future implement actually inserting
          Proxy result into the VRML file. }
        if Geometry <> SI.Current.OriginalGeometry then
          RadianceTransfer := nil;

        if RadianceTransfer <> nil then
        begin
          { For PRT, we need a normal per-vertex, so always calculate
            smooth normals. Simple, and thanks to NormalsCalculator
            this works for all VRML coord-based nodes (and only for
            those RadianceTransfer is defined). }
          Normals := SI.Current.NormalsSmooth(true);
          ComputeTransfer(RadianceTransfer, Geometry.Coordinates(State).Items,
            State.Transform, DiffuseColor(State));
        end;
      end;
    finally FreeAndNil(SI) end;

    S := Format('SH bases %d, rays per vertex %d, done in %f secs',
      [SHBasisCount, RaysPerVertex, ProcessTimerEnd]);

    Writeln('Precomputing finished: ', S);

    SaveVRML(Scene.RootNode, Parameters[2],
      'radianceTransfer computed by precompute_radiance_transfer: ' + S, '', xeClassic);
  finally FreeAndNil(Scene) end;
end.
