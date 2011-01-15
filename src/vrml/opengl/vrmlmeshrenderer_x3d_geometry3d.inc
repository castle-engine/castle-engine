{
  Copyright 2002-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TVRMLMeshRenderer descendants implementing nodes in X3D "Geometry3D"
  component. (Some simpler nodes are in separate file:
  see vrmlmeshrenderer_simple_nodes.inc file.) }

type
  TElevationGridRenderer = class(TVRMLMeshRenderer)
  protected
    procedure DoRender; override;
  public
    constructor Create(ARenderer: TVRMLGLRenderer);
    function NeedsSetColor: boolean; override;
  end;

  { Handling IndexedFaceSet for VRML <= 1.0 and >= 2.0.

    Note that this cannot deal with NorImplementation = niNone,
    you're expected to set NorImplementation to non-none in constructor.
    A simplest way to do this is to use AutoGenerateNormals. }
  TIndexedFaceSetRenderer = class(TAbstractNorColMatTexCoordinateRenderer)
  private
    { Helpers for bump mapping }
    HasTangentVectors: boolean;
    STangent, TTangent: TVector3Single;
    LightPositionObjectSpace: TVector3Single;

    IFSIndexesCount: Cardinal;
    IFSMaxIndex: Integer;
    IFSNextIndex: Cardinal;

    procedure PrepareIndexesTriangle1(const TriIndices: TVector3Longint);
    procedure PrepareIndexesTriangle2(const TriIndices: TVector3Longint);

    procedure PrepareIndexesCoordsRange1(
      const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);
    procedure PrepareIndexesCoordsRange2(
      const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);

    procedure GenerateTriangle(const TriIndices: TVector3Longint);
  protected
    procedure GenerateCoordinate; override;
    procedure GenerateCoordinateBegin; override;
    procedure GenerateCoordinateEnd; override;
    procedure GenerateVertex(IndexNum: Integer); override;
    procedure GenerateCoordsRange(const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer); override;

    { Auto-generate normals and set Normals, NormalsCcw, NorImplementation
      properties to use them. }
    procedure AutoGenerateNormals(const CreaseAngle: Single);
  protected
    { Set these in descendant's constructor. }
    FaceConvex: boolean;
    procedure PrepareIndexesPrimitives(var MaxIndex: Integer); override;
  public
    constructor Create(ARenderer: TVRMLGLRenderer);
  end;

  TIndexedFaceSet_1Renderer = class(TIndexedFaceSetRenderer)
  public
    constructor Create(ARenderer: TVRMLGLRenderer);
  end;

  TIndexedFaceSet_2Renderer = class(TIndexedFaceSetRenderer)
  public
    constructor Create(ARenderer: TVRMLGLRenderer);
  end;

{ TElevationGridRenderer --------------------------------------------------------- }

constructor TElevationGridRenderer.Create(ARenderer: TVRMLGLRenderer);
var
  Node: TNodeElevationGrid;
begin
  inherited;

  Node := Geometry as TNodeElevationGrid;

  { About normals orientation, ccw and solid fields:

    See Extrusion implementation for general notes.
    In short, ccw only says which side is visible(if solid)/lighted
    (if one-sided lighting). We always want to generate normals from
    CCW, and avoid calling glFrontFace.

    Spec says that xSpacing and zSpacing should always be > 0,
    so always up (where height points to) is visible when ccw = TRUE
    and solid = TRUE. So flipping ccw simply culls the other face. }

  CullBackFaces := Node.FdSolid.Value;
  FrontFaceCcw := Node.FdCcw.Value;
end;

procedure TElevationGridRenderer.DoRender;
var
  Node: TNodeElevationGrid;
  { Just shortcuts for appropriate Node fields. }
  XDimension, ZDimension: Integer;

  { Return normals, calculated or not, for given node.

    Calculated normals are ordered just like VRML / X3D spec says
    for normals recorded in VRML / X3D file, so you just don't need
    to care about it. }
  procedure CalculateNormals(
    out Normal: TDynVector3SingleArray;
    out NormalPerVertex: boolean;
    out NormalGenerated: boolean);

    procedure GenerateNormals;
    var
      FaceNormals: TDynVector3SingleArray;
      VertexNormals: array [0..3] of TVector3Single;
      VertexNormalsSum: TVector3Single;
      VertexNormalsCount: Integer;

      procedure AddVertexNormal(const I, J: Integer);
      begin
        if Between(I, 0, XDimension - 2) and
           Between(J, 0, ZDimension - 2) then
        begin
          VertexNormals[VertexNormalsCount] :=
            FaceNormals.Items[I  + J * (XDimension - 1)];
          VectorAddTo1st(VertexNormalsSum, VertexNormals[VertexNormalsCount]);
          Inc(VertexNormalsCount);
        end;
      end;

    var
      I, J: Integer;
      Last1, Last2, Next1, Next2: TVector3Single;
    begin
      NormalGenerated := true;

      { Always start by generating flat (per-quad) normals.
        Later, if creaseAngle > 0, we will use this flat normals to make
        smooth ones. }
      NormalPerVertex := false;
      Normal := TDynVector3SingleArray.Create((XDimension - 1) *
                                              (ZDimension - 1));

      for I := 1 to XDimension - 1 do
      begin
        Last1[0] := Node.FdXSpacing.Value * (I - 1);
        Last1[1] := Node.FdHeight.Items.Items[I - 1];
        Last1[2] := 0;
        Last2[0] := Node.FdXSpacing.Value * I;
        Last2[1] := Node.FdHeight.Items.Items[I];
        Last2[2] := 0;
        { Next X-coordinate will be const for the life of "for J := " below. }
        Next1[0] := Last1[0];
        Next2[0] := Last2[0];

        for J := 1 to ZDimension - 1 do
        begin
          Next1[1] := Node.FdHeight.Items.Items[
            I - 1 + J * XDimension];
          Next1[2] := Node.FdZSpacing.Value * J;
          Next2[1] := Node.FdHeight.Items.Items[
            I     + J * XDimension];
          Next2[2] := Next1[2];

          { As a normal we take avarage normal of two triangles of rendered
            quad. We have to normalize two sum components
            (use TriangleNormal, not TriangleDir), otherwise taking
            the average woudn't be good.

            Also, we normalize resulting VectorAdd, in case we'll want
            to generate smooth normals (for OpenGL flat rendering,
            this is actually not needed, as we have GL_NORMALIZE
            enabled anyway). }
          Normal.Items[I - 1 + (J - 1) * (XDimension - 1)] :=
            Normalized(VectorAdd(
              TriangleNormal(Last2, Last1, Next1),
              TriangleNormal(Last2, Next1, Next2)));

          Last1 := Next1;
          Last2 := Next2;
        end;
      end;

      if Attributes.SmoothShading and
         (Node.FdCreaseAngle.Value <> 0) then
      begin
        FaceNormals := Normal;
        Normal := nil;
        try
          NormalPerVertex := true;
          Normal := TDynVector3SingleArray.Create(XDimension * ZDimension);

          for I := 0 to XDimension - 1 do
          begin
            for J := 0 to ZDimension - 1 do
            begin
              VertexNormalsCount := 0;
              VertexNormalsSum := ZeroVector3Single;
              AddVertexNormal(I - 1, J - 1);
              AddVertexNormal(I    , J - 1);
              AddVertexNormal(I - 1, J    );
              AddVertexNormal(I    , J    );

              { TODO: For now, just ignore creaseAngle (we know it's > 0)
                and smooth all. }
              if VertexNormalsCount = 0 then
              begin
                { special case: no neighbor face for vertex }
                Assert(XDimension = 1);
                Assert(ZDimension = 1);
                Normal.Items[I + J * XDimension] := Vector3Single(0, 1, 0);
              end else
              begin
                Normal.Items[I + J * XDimension] :=
                  VectorScale(VertexNormalsSum, 1 / VertexNormalsCount);
              end;
            end;
          end;
        finally FreeAndNil(FaceNormals); end;
      end;
    end;

  const
    SNotEnoughNormals = 'Elevation grid specifies normal node, but with not enough items. Falling back to auto-generation of normals';
  begin
    if (Node.FdNormal.Value <> nil) and
       (Node.FdNormal.Value is TNodeNormal) then
    begin
      Normal := TNodeNormal(Node.FdNormal.Value).FdVector.Items;
      NormalPerVertex := Node.FdNormalPerVertex.Value;
      NormalGenerated := false;

      { check are enough items available }
      if (NormalPerVertex and
           (Normal.Count < XDimension *
                           ZDimension)) or
         ((not NormalPerVertex) and
           (Normal.Count < (XDimension - 1) *
                           (ZDimension - 1))) then
      begin
        VRMLWarning(vwSerious, SNotEnoughNormals);
        GenerateNormals;
      end else
      begin
        { Then user-supplied normals point from CW.
          But we want them from CCW... so we have to copy and negate them. }
        if not FrontFaceCcw then
        begin
          Normal := TDynVector3SingleArray.CreateCopy(Normal);
          Normal.Negate;
          NormalGenerated := true;
        end;
      end;
    end else
      GenerateNormals;
  end;

var
  TexCoordUseful: boolean;

  Normal: TDynVector3SingleArray;
  NormalPerVertex: boolean;
  NormalGenerated: boolean;

  ColorPerVertex: boolean;
  Color: TMFVec3f;
  ColorRGBA: TMFColorRGBA;

  procedure Vertex(const I, J: Integer; const V: TVector3Single);
  begin
    if Renderer.TexCoordsNeeded > 0 then
    begin
      if TexCoordUseful then
        glTexCoordv(
          TNodeTextureCoordinate(Node.FdTexCoord.Value).FdPoint.
            Items.Items[I + J * XDimension]) else
        glTexCoord2f(I / (XDimension - 1),
                     J / (ZDimension - 1));
    end;

    if NormalPerVertex then
      glNormalv(Normal.Items[I + J * XDimension]);

    if (Color <> nil) and ColorPerVertex then
      Renderer.SetColor(Color.ItemsSafe[I + J * XDimension]) else
    if (ColorRGBA <> nil) and ColorPerVertex then
      Renderer.SetColor(ColorRGBA.ItemsSafe[I + J * XDimension]);

    DoGLVertex(V);
  end;

var
  I, J: Integer;
  Last1, Last2, Next1, Next2: TVector3Single;
begin
  Node := Geometry as TNodeElevationGrid;
  XDimension := Node.FdXDimension.Value;
  ZDimension := Node.FdZDimension.Value;
  ColorPerVertex := Node.FdColorPerVertex.Value;
  Color := Node.Color;
  ColorRGBA := Node.ColorRGBA;

  if Node.IsNotEmpty then
  begin
    CalculateNormals(Normal, NormalPerVertex, NormalGenerated);

    TexCoordUseful :=
      (Node.FdTexCoord.Value <> nil) and
      (Node.FdTexCoord.Value is TNodeTextureCoordinate) and
      (TNodeTextureCoordinate(Node.FdTexCoord.Value).FdPoint.Count >=
        XDimension * ZDimension);

    glPushAttrib(GL_LIGHTING_BIT { for shade model });

      { Shading: unfortunately, since vertices are shared across quads,
        colorPerVertex and normalPerVertex collide if they have different
        values. true value on any field requires smooth shading,
        while false requires flat shading...

        Current approach: if Color/ColorRGBA <> nil, then ColorPerVertex decides
        about the shading (since it's usually more noticeable,
        and if the node is unlit it is the only thing noticeable).
        Otherwise NormalPerVertex decides. }

      if (Color <> nil) or (ColorRGBA <> nil) then
      begin
        if not ColorPerVertex then
          glShadeModel(GL_FLAT);
        if ColorPerVertex <> NormalPerVertex then
          WarningShadingProblems(ColorPerVertex, NormalPerVertex);
      end else
      begin
        if not NormalPerVertex then
          glShadeModel(GL_FLAT);
      end;

      for I := 1 to XDimension - 1 do
      begin
        glBegin(GL_QUAD_STRIP);
          Last1[0] := Node.FdXSpacing.Value * (I - 1);
          Last1[1] := Node.FdHeight.Items.Items[I - 1];
          Last1[2] := 0;
          Last2[0] := Node.FdXSpacing.Value * I;
          Last2[1] := Node.FdHeight.Items.Items[I];
          Last2[2] := 0;
          { Next X-coordinate will be const for the life of "for J := " below. }
          Next1[0] := Last1[0];
          Next2[0] := Last2[0];

          { Vertices are ordered such that face is CCW from up (i.e.
            looking from positive Y axis). }

          Vertex(I    , 0, Last2);
          Vertex(I - 1, 0, Last1);

          for J := 1 to ZDimension - 1 do
          begin
            Next1[1] := Node.FdHeight.Items.Items[
              I - 1 + J * XDimension];
            Next1[2] := Node.FdZSpacing.Value * J;
            Next2[1] := Node.FdHeight.Items.Items[
              I     + J * XDimension];
            Next2[2] := Next1[2];

            if not NormalPerVertex then
              glNormalv(Normal.Items[
                I - 1 + (J - 1) * (XDimension - 1)]);

            if (Color <> nil) and not ColorPerVertex then
              Renderer.SetColor(Color.ItemsSafe[I - 1 + (J - 1) * (XDimension - 1)]) else
            if (ColorRGBA <> nil) and not ColorPerVertex then
              Renderer.SetColor(ColorRGBA.ItemsSafe[I - 1 + (J - 1) * (XDimension - 1)]);

            Vertex(I    , J, Next2);
            Vertex(I - 1, J, Next1);

            Last1 := Next1;
            Last2 := Next2;
          end;
        glEnd;
      end;
    glPopAttrib;

    if NormalGenerated then
      FreeAndNil(Normal);
  end;
end;

function TElevationGridRenderer.NeedsSetColor: boolean;
begin
  Result :=
    ((Geometry as TNodeElevationGrid).Color <> nil) or
    ((Geometry as TNodeElevationGrid).ColorRGBA <> nil);
end;

{ TIndexedFaceSetRenderer -------------------------------------------------- }

constructor TIndexedFaceSetRenderer.Create(ARenderer: TVRMLGLRenderer);
begin
  inherited;
  BumpMappingAllowed := true;
end;

procedure TIndexedFaceSetRenderer.GenerateCoordinate;
begin
  { Making one TRIANGLES primitive is fast, and allows us to do everything
    we want.

    Old notes: I was experimenting also with making TRIANGLE_FAN (or POLYGON)
    per face.

    Advantages of TRIANGLE_FAN:

    - TRIANGLE_FAN *may* increase speed, because vertices
      are more shared and specified less times. For TRIANGLES, we waste 2 vertex
      calls for every vertex (above 3) for each face.

    - TRIANGLE_FAN workarounds NVidia GeForce 5200 bug for material
      binding PER_FACE_INDEXED, see
      kambi_vrml_test_suite/vrml_1/material_per_face_problematic_on_nvidia_gf_5200.wrl
      Although the problem disappeared by itself after using GL_COLOR_MATERIAL
      for VRML 1.0 per-face/vertex materials.

    Avantages of TRIANGLES:

    - TRIANGLE_FAN is highly unoptimal on newer GPUs (Radeon on chantal).
      The problem is you have to call glEnd / glBegin again
      a lot, and this hurts pipeline performance very badly. And the "sharing"
      argument for TRIANGLE_FAN is weak, as newer GPUs cache vertex data
      by index anyway.

      Tests on Radeon X1600 (chantal, fglrx) show
      TRIANGLES    FPS: 165/155
      TRIANGLE_FAN FPS: 60/55
      Clearly, TRIANGLES win.

      Although tests on NVidia (kocury, GeForce FX 5200) don't show such
      drastic difference.

    - TRIANGLES workaround Radeon X1600 Darwin (Mac OS X 10.4.9)
      OpenGL bugs. (With Linux fglrx on the same hardware there is no bug.)
      It seems not able to handle GL_TRIANGLE_FAN properly,
      which is visible on "The Gate" and
      "Cages" levels of "The Castle": some triangles on these
      levels (on "The Gate", it concerns that back wall and gate
      geometry) seem to randomly disappear.

    This is a shortcut of even longer old comments... For the full story,
    see revision 6432,
    http://vrmlengine.svn.sourceforge.net/viewvc/vrmlengine/trunk/kambi_vrml_game_engine/src/vrml/opengl/vrmlmeshrenderer_x3d_geometry3d.inc?revision=6432&view=markup
    and search for TryRenderingFaceAsFan comments.
  }

  Geometry.MakeCoordRanges(State, @GenerateCoordsRange);
end;

procedure TIndexedFaceSetRenderer.GenerateVertex(IndexNum: Integer);

  procedure DoBumpMapping;
  var
    Normal, Vertex: TVector3Single;

    function CalculateLightDirTangent: TVector3Single;

      function LightDirectionInTangentSpace(
        const Vertex, Normal, STangent, TTangent: TVector3Single): TVector3Single;
      var
        ToTangent: TMatrix3Single;
        I: Integer;
      begin
        for I := 0 to 2 do
        begin
          ToTangent[I, 0] := STangent[I];
          ToTangent[I, 1] := TTangent[I];
          ToTangent[I, 2] := Normal[I];
        end;

        Result := MatrixMultVector(ToTangent,
          VectorSubtract(LightPositionObjectSpace, Vertex));
      end;

    var
      LocalSTangent, LocalTTangent: TVector3Single;
    begin
      { calculate LightDirTangent }
      if HasTangentVectors and
        (Abs(VectorDotProduct(STangent, Normal)) < 0.95) and
        (Abs(VectorDotProduct(TTangent, Normal)) < 0.95) then
      begin
        if NormalsFlat then
        begin
          Result := LightDirectionInTangentSpace(
            Vertex, Normal, STangent, TTangent);
        end else
        begin
          { If not NormalsFlat, you want to calculate local STangent and TTangent,
            I mean STangent and TTangent adjusted to current vertex (since each
            vertex may have different normal on the face when not NormalsFlat).

            Without doing this, you would see strange artefacts, smoothed
            faces would look somewhat like flat faces. Concenptually, for smoothed
            faces, whole tangent space should vary for each vertex, so Normal,
            and both tangents may be different on each vertex. }

          LocalSTangent := STangent;
          MakeVectorsOrthoOnTheirPlane(LocalSTangent, Normal);

          LocalTTangent := TTangent;
          MakeVectorsOrthoOnTheirPlane(LocalTTangent, Normal);

          Result := LightDirectionInTangentSpace(
            Vertex, Normal, LocalSTangent, LocalTTangent);
        end;
      end else
        Result := Vector3Single(0, 0, 1);
    end;

    procedure SetBumpMappingAttribs;

      procedure SetMatrixToTangent(const Normal, STangent, TTangent: TVector3Single);
      var
        ToTangent: TMatrix3Single;
        I: Integer;
      begin
        for I := 0 to 2 do
        begin
          ToTangent[I, 0] := STangent[I];
          ToTangent[I, 1] := TTangent[I];
          ToTangent[I, 2] := Normal[I];
        end;

        Renderer.BmGLSLAttribObjectSpaceToTangent[
          BumpMappingMethod >= bmGLSLParallax].SetValue(ToTangent);
      end;

    var
      LocalSTangent, LocalTTangent: TVector3Single;
    begin
      { This somewhat duplicates CalculateLightDirTangent logic,
        but instead of calling LightDirectionInTangentSpace this must
        call SetMatrixToTangent. }
      if HasTangentVectors and
        (Abs(VectorDotProduct(STangent, Normal)) < 0.95) and
        (Abs(VectorDotProduct(TTangent, Normal)) < 0.95) then
      begin
        if NormalsFlat then
        begin
          SetMatrixToTangent(Normal, STangent, TTangent);
        end else
        begin
          { If not NormalsFlat, you want to calculate local STangent and TTangent,
            I mean STangent and TTangent adjusted to current vertex (since each
            vertex may have different normal on the face when not NormalsFlat).

            Without doing this, you would see strange artefacts, smoothed
            faces would look somewhat like flat faces. Concenptually, for smoothed
            faces, whole tangent space should vary for each vertex, so Normal,
            and both tangents may be different on each vertex. }

          LocalSTangent := STangent;
          MakeVectorsOrthoOnTheirPlane(LocalSTangent, Normal);

          LocalTTangent := TTangent;
          MakeVectorsOrthoOnTheirPlane(LocalTTangent, Normal);

          SetMatrixToTangent(Normal, LocalSTangent, LocalTTangent);
        end;
      end else
      begin
        SetMatrixToTangent(Normal,
          { would be more correct to set LocalSTangent as anything perpendicular
            to Normal, and LocalTTangent as vector product (normal, LocalSTangent) }
          Vector3Single(1, 0, 0), Vector3Single(0, 1, 0));
      end;
    end;

  var
    LightDirTangent: TVector3Single;
    Tex: TVector2Single;
  begin
    GetNormal(IndexNum, CurrentRangeNumber, Normal);

    { This is OK, we want Vertex in object space here. }
    Vertex := Coord.Items.ItemsArray^[CoordIndex.Items.Items[indexNum]];

    Check(GetTextureCoord(IndexNum, 0, Tex), 'TexCoord for BumpMapping');

    { set all textures coordinates }
    case BumpMappingMethod of
      bmMultiTexDotNotNormalized:
        begin
          LightDirTangent := CalculateLightDirTangent;
          NormalizeTo1st(LightDirTangent);

          { pass LightDirTangent as RGB color }
          glColor3f((LightDirTangent[0] + 1) / 2,
                    (LightDirTangent[1] + 1) / 2,
                    (LightDirTangent[2] + 1) / 2);

          glMultiTexCoord2fARB(GL_TEXTURE0_ARB, Tex[0], Tex[1]);
          glMultiTexCoord2fARB(GL_TEXTURE1_ARB, Tex[0], Tex[1]);
        end;

      bmMultiTexDotNormalized:
        begin
          LightDirTangent := CalculateLightDirTangent;
          glMultiTexCoord3fARB(GL_TEXTURE0_ARB,
            LightDirTangent[0],
            LightDirTangent[1],
            LightDirTangent[2]);

          glMultiTexCoord2fARB(GL_TEXTURE1_ARB, Tex[0], Tex[1]);
          glMultiTexCoord2fARB(GL_TEXTURE2_ARB, Tex[0], Tex[1]);
        end;

      bmGLSLNormal,
      bmGLSLParallax:
        begin
          SetBumpMappingAttribs;
          glMultiTexCoord2fARB(GL_TEXTURE0_ARB, Tex[0], Tex[1]);
          { No need to pass texture coords for 2nd texture, GL_TEXTURE1_ARB.
            Shader knows that they are the same, and will just use gl_TexCoord[0]
            to access both textures. }
        end;
      else raise EInternalError.Create('BumpMappingMethod ?');
    end;
  end;

begin
  if BumpMappingMethod <> bmNone then
    DoBumpMapping;

  inherited;
end;

procedure TIndexedFaceSetRenderer.GenerateCoordinateBegin;
begin
  inherited;

  if BumpMappingMethod <> bmNone then
    LightPositionObjectSpace := MatrixMultPoint(
      State.InvertedTransform,
      Renderer.BumpMappingLightPosition);
end;

procedure TIndexedFaceSetRenderer.AutoGenerateNormals(const CreaseAngle: Single);
begin
  { I could just do normal CreateNormals for all CreaseAngle values,
    so the test for "CreaseAngle <> 0" is not really required below.
    But I want to set NormalsFlat to @true whenever possible
    (as NormalsFlat allow some optimizations), so the test for
    "CreaseAngle <> 0" is useful there (as many VRML models will have
    default creaseAngle = 0, so NormalsFlat = true optimizations may be
    often applicable). }

  if Coord = nil then Exit;

  if Renderer.Attributes.SmoothShading and (CreaseAngle <> 0) then
  begin
    if CreaseAngle >= Pi then
    begin
      Normals := Shape.NormalsSmooth;
      NorImplementation := niPerVertexCoordIndexed;
    end else
    begin
      Normals := Shape.NormalsCreaseAngle(CreaseAngle);
      NorImplementation := niPerVertexNonIndexed;
    end;
  end else
  begin
    Normals := Shape.NormalsFlat;
    NorImplementation := niPerFace;
  end;
  NormalsCcw := true; { always generated from CCW }
end;

procedure TIndexedFaceSetRenderer.GenerateCoordinateEnd;
begin
  inherited;
end;

procedure TIndexedFaceSetRenderer.GenerateTriangle(
  const TriIndices: TVector3Longint);
begin
  GenerateVertex(TriIndices[0]);
  GenerateVertex(TriIndices[1]);
  GenerateVertex(TriIndices[2]);
end;

procedure TIndexedFaceSetRenderer.GenerateCoordsRange(
  const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);

  { Calculates HasTangentVectors, STangent, TTangent }
  procedure CalculateTangentVectors;

    { This procedure can change Triangle*, but only by swapping some vertexes,
      so we pass Triangle* by reference instead of by value, to avoid
      needless mem copying.

      Returns @false if cannot be calculated. }
    function CalculateTangent(IsSTangent: boolean; var Tangent: TVector3Single;
      var Triangle3D: TTriangle3Single;
      var TriangleTexCoord: TTriangle2Single): boolean;
    var
      D: TVector2Single;
      LineA, LineBC, DIn3D: TVector3Single;
      MiddleIndex: Integer;
      FarthestDistance, NewDistance, Alpha: Single;
      SearchCoord, OtherCoord: Cardinal;
    begin
      if ISSTangent then
        SearchCoord := 0 else
        SearchCoord := 1;
      OtherCoord := 1 - SearchCoord;

      { choose such that 1st and 2nd points have longest distance along
        OtherCoord, so 0 point is in the middle. }

      { MiddleIndex means that
        MiddleIndex, (MiddleIndex + 1) mod 3 are farthest. }

      MiddleIndex := 2;
      FarthestDistance := Abs(TriangleTexCoord[0][OtherCoord] - TriangleTexCoord[1][OtherCoord]);

      NewDistance := Abs(TriangleTexCoord[1][OtherCoord] - TriangleTexCoord[2][OtherCoord]);
      if NewDistance > FarthestDistance then
      begin
        MiddleIndex := 0;
        FarthestDistance := NewDistance;
      end;

      NewDistance := Abs(TriangleTexCoord[2][OtherCoord] - TriangleTexCoord[0][OtherCoord]);
      if NewDistance > FarthestDistance then
      begin
        MiddleIndex := 1;
        FarthestDistance := NewDistance;
      end;

      if Zero(FarthestDistance) then
        Exit(false);

      if MiddleIndex <> 0 then
      begin
        SwapValues(TriangleTexCoord[0], TriangleTexCoord[MiddleIndex]);
        SwapValues(Triangle3D      [0], Triangle3D      [MiddleIndex]);
      end;

      if IsSTangent then
      begin
        { we want line Y = TriangleTexCoord[0][1]. }
        LineA[0] := 0;
        LineA[1] := 1;
        LineA[2] := -TriangleTexCoord[0][1];
      end else
      begin
        { we want line X = TriangleTexCoord[0][0]. }
        LineA[0] := 1;
        LineA[1] := 0;
        LineA[2] := -TriangleTexCoord[0][0];
      end;
      LineBC := LineOfTwoDifferentPoints2d(
        TriangleTexCoord[1], TriangleTexCoord[2]);

      try
        D := Lines2DIntersection(LineA, LineBC);
      except
        on ELinesParallel do begin Result := false; Exit; end;
      end;

      { LineBC[0, 1] is vector 2D orthogonal to LineBC.
        If Abs(LineBC[0]) is *smaller* then it means that B and C points
        are most different on 0 coord. }
      if Abs(LineBC[0]) < Abs(LineBC[1]) then
        Alpha := (                  D[0] - TriangleTexCoord[1][0]) /
                 (TriangleTexCoord[2][0] - TriangleTexCoord[1][0]) else
        Alpha := (                  D[1] - TriangleTexCoord[1][1]) /
                 (TriangleTexCoord[2][1] - TriangleTexCoord[1][1]);

      DIn3D := VectorAdd(
        VectorScale(Triangle3D[1], 1 - Alpha),
        VectorScale(Triangle3D[2], Alpha));

      if D[SearchCoord] > TriangleTexCoord[0][SearchCoord] then
        Tangent := VectorSubtract(DIn3D, Triangle3D[0]) else
        Tangent := VectorSubtract(Triangle3D[0], DIn3D);

      NormalizeTo1st(Tangent);

      Result := true;
    end;

  var
    Triangle3D: TTriangle3Single;
    TriangleTexCoord: TTriangle2Single;
  begin
    HasTangentVectors := false;
    if (BumpMappingMethod <> bmNone) and (BeginIndex + 2 < EndIndex) then
    begin
      { calculate Triangle3D }
      Triangle3D[0] := Coord.Items.ItemsArray^[CoordIndex.Items.Items[BeginIndex]];
      Triangle3D[1] := Coord.Items.ItemsArray^[CoordIndex.Items.Items[BeginIndex + 1]];
      Triangle3D[2] := Coord.Items.ItemsArray^[CoordIndex.Items.Items[BeginIndex + 2]];

      { This is just to shut up FPC 2.2.0 warnings about
        TriangleTexCoord not initialized. }
      TriangleTexCoord[0][0] := 0.0;

      HasTangentVectors :=
        { calculate TriangleTexCoord }
        GetTextureCoord(BeginIndex    , 0, TriangleTexCoord[0]) and
        GetTextureCoord(BeginIndex + 1, 0, TriangleTexCoord[1]) and
        GetTextureCoord(BeginIndex + 2, 0, TriangleTexCoord[2]) and
        { calculate STangent, TTangent }
        CalculateTangent(true , STangent, Triangle3D, TriangleTexCoord) and
        CalculateTangent(false, TTangent, Triangle3D, TriangleTexCoord) and
        (Abs(VectorDotProduct(STangent, TTangent)) < 0.95);
    end;
  end;

var
  I: Integer;
begin
  inherited;

  CalculateTangentVectors;

  if not FaceConvex then
  begin
    TriangulateFace(Addr(CoordIndex.Items.Items[BeginIndex]),
      EndIndex - BeginIndex, Coord.Items.ItemsArray,
      @GenerateTriangle, BeginIndex);
  end else
  begin
    (* Alternative version:
    TriangulateConvexFace(EndIndex - BeginIndex, @GenerateVertex, BeginIndex); *)

    for I := BeginIndex to EndIndex - 3 do
    begin
      GenerateVertex(BeginIndex);
      GenerateVertex(I + 1);
      GenerateVertex(I + 2);
    end;
  end;
end;

procedure TIndexedFaceSetRenderer.PrepareIndexesTriangle1(
  const TriIndices: TVector3Longint);
begin
  IFSIndexesCount += 3;
end;

{ update IFSIndexesCount, IFSMaxIndex }
procedure TIndexedFaceSetRenderer.PrepareIndexesCoordsRange1(
  const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);
var
  I: Integer;
begin
  if not FaceConvex then
  begin
    TriangulateFace(Addr(CoordIndex.Items.Items[BeginIndex]),
      EndIndex - BeginIndex, Coord.Items.ItemsArray,
      @PrepareIndexesTriangle1, BeginIndex);
  end else
  begin
    { Render convex face }
    (* Alternative version:
       TriangulateConvexFace(EndIndex - BeginIndex,
         @GenerateVertex, BeginIndex); *)

    IFSIndexesCount += Max(EndIndex - BeginIndex - 2, 0) * 3;
  end;

  for I := BeginIndex to EndIndex - 1 do
    MaxTo1st(IFSMaxIndex, CoordIndex.Items.Items[EndIndex]);
end;

procedure TIndexedFaceSetRenderer.PrepareIndexesTriangle2(
  const TriIndices: TVector3Longint);
begin
  IndexesFromCoordIndex.Items[IFSNextIndex] := CoordIndex.Items.Items[TriIndices[0]]; Inc(IFSNextIndex);
  IndexesFromCoordIndex.Items[IFSNextIndex] := CoordIndex.Items.Items[TriIndices[1]]; Inc(IFSNextIndex);
  IndexesFromCoordIndex.Items[IFSNextIndex] := CoordIndex.Items.Items[TriIndices[2]]; Inc(IFSNextIndex);
end;

procedure TIndexedFaceSetRenderer.PrepareIndexesCoordsRange2(
  const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);
var
  I: Integer;
begin
  if not FaceConvex then
  begin
    TriangulateFace(Addr(CoordIndex.Items.Items[BeginIndex]),
      EndIndex - BeginIndex, Coord.Items.ItemsArray,
      @PrepareIndexesTriangle2, BeginIndex);
  end else
  begin
    { Render convex face }
    (* Alternative version:
       TriangulateConvexFace(EndIndex - BeginIndex,
         @GenerateVertex, BeginIndex); *)

    for I := BeginIndex to EndIndex - 3 do
    begin
      IndexesFromCoordIndex.Items[IFSNextIndex] := CoordIndex.Items.Items[BeginIndex]; Inc(IFSNextIndex);
      IndexesFromCoordIndex.Items[IFSNextIndex] := CoordIndex.Items.Items[I + 1];      Inc(IFSNextIndex);
      IndexesFromCoordIndex.Items[IFSNextIndex] := CoordIndex.Items.Items[I + 2];      Inc(IFSNextIndex);
    end;
  end;
end;

procedure TIndexedFaceSetRenderer.PrepareIndexesPrimitives(var MaxIndex: Integer);
begin
  { calculate IFSIndexesCount and IFSMaxIndex, by one iteration over coordIndex }
  IFSIndexesCount := 0;
  IFSMaxIndex := MaxIndex;
  Geometry.MakeCoordRanges(State, @PrepareIndexesCoordsRange1);
  MaxIndex := IFSMaxIndex;

  { calculate IndexesFromCoordIndex contents: triangulate }
  IndexesFromCoordIndex := TDynLongIntArray.Create(IFSIndexesCount);
  IFSNextIndex := 0;
  Geometry.MakeCoordRanges(State, @PrepareIndexesCoordsRange2);
  Assert(IFSNextIndex = IFSIndexesCount);
end;

{ TIndexedFaceSet_1Renderer -------------------------------------------------- }

constructor TIndexedFaceSet_1Renderer.Create(
  ARenderer: TVRMLGLRenderer);
var
  SH: TNodeShapeHints;
  ANode: TNodeIndexedFaceSet_1;
begin
  inherited Create(ARenderer);

  ANode := Geometry as TNodeIndexedFaceSet_1;

  TexCoordIndex := ANode.FdTextureCoordIndex;

  MaterialIndex := ANode.FdMaterialIndex;
  MaterialBinding := State.LastNodes.MaterialBinding.FdValue.Value;
  UpdateMat1Implementation;

  SH := State.LastNodes.ShapeHints;

  { W tym miejscu uznajemy VERTORDER_UNKNOWN_ORDERING za COUNTERCLOCKWISE
    (a autorzy VRMLi w ogole nie powinni podawac normali jesli
    nie podadza vertexOrdering innego niz UNKNOWN) }
  FrontFaceCcw := SH.FdVertexOrdering.Value <> VERTORDER_CLOCKWISE;

  CullBackFaces :=
    (SH.FdVertexOrdering.Value <> VERTORDER_UNKNOWN) and
    (SH.FdShapeType.Value = SHTYPE_SOLID);

  FaceConvex := SH.FdFaceType.Value = FACETYPE_CONVEX;

  NormalIndex := ANode.FdNormalIndex;
  Normals := State.LastNodes.Normal.FdVector.Items;
  NormalsCcw := FrontFaceCcw;
  NorImplementation := NorImplementationFromVRML1Binding(
    State.LastNodes.NormalBinding.FdValue.Value);
  if NorImplementation = niNone then
    AutoGenerateNormals(SH.FdCreaseAngle.Value);

  RadianceTransfer := ANode.FdRadianceTransfer.Items;
end;

{ TIndexedFaceSet_2Renderer -------------------------------------------------- }

constructor TIndexedFaceSet_2Renderer.Create(ARenderer: TVRMLGLRenderer);
var
  ANode: TNodeIndexedFaceSet_2;
begin
  inherited Create(ARenderer);

  ANode := Geometry as TNodeIndexedFaceSet_2;

  TexCoordIndex := ANode.FdTexCoordIndex;

  Normals := ANode.NormalItems;
  if Normals <> nil then
  begin
    { FrontFaceCcw is calculated already, TVRMLMeshRenderer.Create
      initializes it for all X3DComposedGeometryNode from ccw field. }
    NormalsCcw := FrontFaceCcw;
    NormalIndex := ANode.FdNormalIndex;
    if ANode.FdNormalPerVertex.Value then
    begin
      if NormalIndex.Count > 0 then
        NorImplementation := niPerVertexNormalIndexed else
        NorImplementation := niPerVertexCoordIndexed;
    end else
    begin
      if NormalIndex.Count > 0 then
        NorImplementation := niPerFaceNormalIndexed else
        NorImplementation := niPerFace;
    end;
  end else
    AutoGenerateNormals(ANode.FdCreaseAngle.Value);

  FaceConvex := ANode.FdConvex.Value;

  { calculate Color, ColorPerVertex, ColorIndex fields }
  Color := ANode.Color;
  ColorRGBA := ANode.ColorRGBA;
  ColorPerVertex := ANode.FdColorPerVertex.Value;
  ColorIndex := ANode.FdColorIndex;
end;
