{
  Copyright 2002-2008 Michalis Kamburelis.

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

{ TVRMLMeshRenderer descendants implementing nodes in X3D "Geometry3D"
  component. (Some simpler nodes are in separate file:
  see vrmlmeshrenderer_simple_nodes.inc file.) }

type
  TElevationGridRenderer = class(TVRMLMeshRenderer)
  protected
    procedure DoRender; override;
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer);
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

    procedure Render_Triangulated(const TriIndices: TVector3Longint);
  protected
    procedure RenderCoordinate; override;
    procedure RenderCoordinateBegin; override;
    procedure RenderCoordinateEnd; override;
    procedure RenderVertex(IndexNum: Integer); override;
    procedure RenderCoordsRange(const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer); override;

    { Auto-generate normals and set Normals, NormalsCcw, NorImplementation
      properties to use them. }
    procedure AutoGenerateNormals(const CreaseAngle: Single);

    { Set these in descendant's constructor. }
    FaceConvex: boolean;
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer);

    { These must be set by Renderer if BumpMappingMethod <> bmNone }
    TexOriginal, TexNormalMap, TexHeightMap, TexNormalizationCube: TGLuint;
    TexOriginalAlpha: boolean;
    TexHeightMapScale: Single;
  end;

  TIndexedFaceSet_1Renderer = class(TIndexedFaceSetRenderer)
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer);
  end;

  TIndexedFaceSet_2Renderer = class(TIndexedFaceSetRenderer)
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer);
  end;

{ TElevationGridRenderer --------------------------------------------------------- }

constructor TElevationGridRenderer.Create(ARenderer: TVRMLOpenGLRenderer);
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
      Renderer.SetColor(Color.ItemsSafe[I + J * XDimension]);

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

        Current approach: if Color <> nil, then ColorPerVertex decides
        about the shading (since it's usually more noticeable,
        and if the node is unlit it is the only thing noticeable).
        Otherwise NormalPerVertex decides. }

      if Color <> nil then
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
              Renderer.SetColor(Color.ItemsSafe[
                I - 1 + (J - 1) * (XDimension - 1)]);

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
  Result := (Geometry as TNodeElevationGrid).Color <> nil;
end;

{ TIndexedFaceSetRenderer -------------------------------------------------- }

constructor TIndexedFaceSetRenderer.Create(ARenderer: TVRMLOpenGLRenderer);
begin
  inherited;
  BumpMappingAllowed := true;
end;

procedure TIndexedFaceSetRenderer.RenderCoordinate;
begin
  Geometry.MakeCoordRanges(State, @RenderCoordsRange);
end;

procedure TIndexedFaceSetRenderer.RenderVertex(IndexNum: Integer);

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

procedure TIndexedFaceSetRenderer.RenderCoordinateBegin;

  { pass alpha from primary_color (this is actual material Opacity) *
    texture alpha }
  procedure glTexEnvAlphaFromMaterialAndTexture;
  begin
    if TexOriginalAlpha then
    begin
      glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_ALPHA_EXT, GL_MODULATE);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_ALPHA_EXT, GL_PRIMARY_COLOR_EXT);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_ALPHA_EXT, GL_SRC_ALPHA);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_ALPHA_EXT, GL_TEXTURE);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_ALPHA_EXT, GL_SRC_ALPHA);
    end else
    begin
      glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_ALPHA_EXT, GL_REPLACE);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_ALPHA_EXT, GL_PRIMARY_COLOR_EXT);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_ALPHA_EXT, GL_SRC_ALPHA);
    end;
  end;

  procedure EnableForMultiTexDotNotNormalized;
  begin
    Renderer.ActiveTexture(0);
    glBindTexture(GL_TEXTURE_2D, TexNormalMap);
    glEnable(GL_TEXTURE_2D);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_EXT);

    { texture unit 0: make dot between PRIMARY_COLOR and
      current texture (normal map). IOW, calculate diffuse factor. }
    glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_EXT, GL_DOT3_RGB_ARB);
    glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_EXT, GL_PRIMARY_COLOR_EXT);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_EXT, GL_SRC_COLOR);
    glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB_EXT, GL_TEXTURE);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_RGB_EXT, GL_SRC_COLOR);

    Renderer.ActiveTexture(1);
    glBindTexture(GL_TEXTURE_2D, TexOriginal);
    glEnable(GL_TEXTURE_2D);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_EXT);

    { texture unit 1: multiply diffuse factor by texture value. }
    glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_EXT, GL_MODULATE);
    glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_EXT, GL_PREVIOUS_EXT);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_EXT, GL_SRC_COLOR);
    glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB_EXT, GL_TEXTURE);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_RGB_EXT, GL_SRC_COLOR);

    glTexEnvAlphaFromMaterialAndTexture;

    LightPositionObjectSpace := MatrixMultPoint(
      State.InvertedTransform,
      Renderer.BumpMappingLightPosition);
  end;

  procedure EnableForMultiTexDotNormalized;
  begin
    { texture unit 0: just lookup your coordinates (which are
      3D lighting dir, not normalized) in cube map.
      This calculates normalized lighting dir. }
    Renderer.ActiveTexture(0);
    glBindTexture(GL_TEXTURE_CUBE_MAP_ARB, TexNormalizationCube);
    glEnable(GL_TEXTURE_CUBE_MAP_ARB);
    { make sure GL_TEXTURE_2D is disabled here (other bump mapping
      methods could enable this) }
    glDisable(GL_TEXTURE_2D);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_EXT);

    glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_EXT, GL_REPLACE);
    glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_EXT, GL_TEXTURE);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_EXT, GL_SRC_COLOR);

    Renderer.ActiveTexture(1);
    glBindTexture(GL_TEXTURE_2D, TexNormalMap);
    glEnable(GL_TEXTURE_2D);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_EXT);

    { texture unit 1: make dot between previous (light dir normalized) and
      current texture (normal map). IOW, calculate diffuse factor. }
    glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_EXT, GL_DOT3_RGB_ARB);
    glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_EXT, GL_PREVIOUS_EXT);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_EXT, GL_SRC_COLOR);
    glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB_EXT, GL_TEXTURE);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_RGB_EXT, GL_SRC_COLOR);

    Renderer.ActiveTexture(2);
    { We bind NormalAndBumpTex, although actually we don't use
      bump stored there as alpha channel (they are only for emboss
      method). }
    glBindTexture(GL_TEXTURE_2D, TexOriginal);
    glEnable(GL_TEXTURE_2D);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_EXT);

    { texture unit 2: multiply diffuse factor by texture value. }
    glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_EXT, GL_MODULATE);
    glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_EXT, GL_PREVIOUS_EXT);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_EXT, GL_SRC_COLOR);
    glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB_EXT, GL_TEXTURE);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_RGB_EXT, GL_SRC_COLOR);

    glTexEnvAlphaFromMaterialAndTexture;

    LightPositionObjectSpace := MatrixMultPoint(
      State.InvertedTransform,
      Renderer.BumpMappingLightPosition);
  end;

  procedure EnableForGLSL;
  var
    BmParallax: boolean;
  begin
    { This binds textures in the same way as EnableForMultiTexDotNotNormalized,
      but this doesn't call glTexEnvi or glEnable(GL_TEXTURE_2D)
      (it's useless, shader will control this). }
    Renderer.ActiveTexture(0);
    glBindTexture(GL_TEXTURE_2D, TexNormalMap);

    Renderer.ActiveTexture(1);
    glBindTexture(GL_TEXTURE_2D, TexOriginal);

    BmParallax := BumpMappingMethod >= bmGLSLParallax;

    if BmParallax then
    begin
      Renderer.ActiveTexture(2);
      glBindTexture(GL_TEXTURE_2D, TexHeightMap);
    end;

    Renderer.BmGLSLProgram[BmParallax].Enable;
    Renderer.BmGLSLProgram[BmParallax].SetUniform('world_space_to_object',
      State.InvertedTransform);

    if BmParallax then
    begin
      Renderer.BmGLSLProgram[BmParallax].SetUniform('scale', TexHeightMapScale);
      if not Renderer.BmSteepParallaxMapping then
        Renderer.BmGLSLProgram[BmParallax].SetUniform('bias', -TexHeightMapScale/2);
    end;

    glMaterialv(GL_FRONT_AND_BACK, GL_AMBIENT, Vector4Single(1, 1, 1, 1));
  end;

begin
  inherited;

  if BumpMappingMethod <> bmNone then
  begin
    case BumpMappingMethod of
      bmMultiTexDotNotNormalized: EnableForMultiTexDotNotNormalized;
      bmMultiTexDotNormalized   : EnableForMultiTexDotNormalized;
      bmGLSLNormal,
      bmGLSLParallax            : EnableForGLSL;
      else raise EInternalError.Create('BumpMappingMethod ?');
    end;
  end;
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
      Normals := CreateSmoothNormalsCoordinateNode(Geometry, State, true);
      NorImplementation := niPerVertexCoordIndexed;
    end else
    begin
      Normals := CreateNormals(
        CoordIndex.Items, Coord.Items, CreaseAngle, true);
      NorImplementation := niPerVertexNonIndexed;
    end;
  end else
  begin
    Normals := CreateFlatNormals(
      CoordIndex.Items, Coord.Items, true);
    NorImplementation := niPerFace;
  end;
  NormalsCcw := true; { always generated from CCW }
  NormalsFree := true;
end;

procedure TIndexedFaceSetRenderer.RenderCoordinateEnd;

  { DisableXxx procedures disable textures (2d and cube map)
    that were enabled by their SetXxx counterparts in RenderCoordinateBegin.
    In case of shader implementation, this also disables the shader.

    Note that they still leave some state behind:
    - currently bound textures
    - texture environments
    - the currently bound texture is left unspecified
    These must be set always when anything will enable some texture. }

  procedure DisableForMultiTexDotNotNormalized;
  begin
    Renderer.ActiveTexture(0);
    glDisable(GL_TEXTURE_2D);

    Renderer.ActiveTexture(1);
    glDisable(GL_TEXTURE_2D);
  end;

  procedure DisableForMultiTexDotNormalized;
  begin
    { texture unit 0: just lookup your coordinates (which are
      3D lighting dir, not normalized) in cube map.
      This calculates normalized lighting dir. }
    Renderer.ActiveTexture(0);
    glDisable(GL_TEXTURE_CUBE_MAP_ARB);

    Renderer.ActiveTexture(1);
    glDisable(GL_TEXTURE_2D);

    Renderer.ActiveTexture(2);
    glDisable(GL_TEXTURE_2D);;
  end;

  procedure DisableForGLSL;
  begin
    { No need to disable textures, as we didn't enable them in
      EnableForGLSL (shader doesn't need them enabled). }

    Renderer.BmGLSLProgram[BumpMappingMethod >= bmGLSLParallax].Disable;
  end;

begin
  if BumpMappingMethod <> bmNone then
  begin
    case BumpMappingMethod of
      bmMultiTexDotNotNormalized: DisableForMultiTexDotNotNormalized;
      bmMultiTexDotNormalized   : DisableForMultiTexDotNormalized;
      bmGLSLNormal,
      bmGLSLParallax            : DisableForGLSL;
      else raise EInternalError.Create('BumpMappingMethod ?');
    end;
  end;

  inherited;
end;

procedure TIndexedFaceSetRenderer.Render_Triangulated(
  const TriIndices: TVector3Longint);
begin
  RenderVertex(TriIndices[0]);
  RenderVertex(TriIndices[1]);
  RenderVertex(TriIndices[2]);
end;

procedure TIndexedFaceSetRenderer.RenderCoordsRange(
  const RangeNumber: Cardinal;
  BeginIndex, EndIndex: Integer);

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

begin
  inherited;

  CalculateTangentVectors;

 if not FaceConvex then
 begin
  glBegin(GL_TRIANGLES);
  TriangulateFace(Addr(CoordIndex.Items.Items[BeginIndex]),
    EndIndex-BeginIndex, Coord.Items.ItemsArray,
    {$ifdef FPC_OBJFPC} @ {$endif} Render_Triangulated, BeginIndex);
  glEnd;
 end else
 begin
   {$ifdef DARWIN}

   { On Mac OS X 10.4.9, with Radeon ATI graphic card
     (it's MacBook Pro computer), there's definitely a bug
     in OpenGL implementation: it seems not able to handle
     GL_TRIANGLE_FAN properly, which is visible on "The Gate" and
     "Cages" levels of "The Castle": some triangles on these
     levels (on "The Gate", it concerns that back wall and gate
     geometry) seem to randomly disappear.

     I didn't make enough
     tests to actually make sure what's the precise condition
     to trigger this bug (as other "The Castle" levels,
     and castle.wrl sample VRML work OK). I also didn't test
     on other Mac OS X systems. (However, I did test this on the
     same computer with Linux Radeon OpenGL, and on Linux
     it works OK --- so the problem is within software, OpenGL
     probably, on Mac OS X ?). For safety I disable using
     GL_TRIANGLE_FAN entirely for now on Darwin. }

  glBegin(GL_TRIANGLES);
  TriangulateConvexFace(EndIndex-BeginIndex,
    {$ifdef FPC_OBJFPC} @ {$endif} Render_Triangulated, BeginIndex);
  glEnd;

  {$else DARWIN}

  { zamiast GL_POLYGON uzywamy GL_TRIANGLES i robimy cos jak GL_TRIANGLE_FAN
    uzywajac glEdgeFlag. De facto, to co my chcemy zrobic to jest
    wlasnie wykonac GL_TRIANGLE_FAN tyle ze w triangle fan glEdgeFlag
    jest ignorowane (jakby zawsze bylo GL_TRUE).

    Zamiast GL_POLYGON uzywamy GL_TRIANGLE_FAN. Dlaczego ?
    Bo chcemy miec 1-znaczna triangulacje kazdego polygonu. To ma
    znaczenie kiedy np. renderujemy prostokat z cieniowaniem smooth
    i kazdy z 4 vertexow ma inny kolor. Wtedy widac wzdluz ktorej
    przekatnej biegnie triangulacja trojkata. I np. jezeli kawalek
    kwadratu wyjezdza za ekran wtedy widac jak (na skutek przycinania
    polygonu do frustum) przesuwa sie linia triangulacji. Wychodzi
    bardzo dziwny efekt, bo widac ze kolory na prostokacie sie nieco
    zmieniaja w miare jak ten wyjezdza za ekran, a to zdecydowanie nie jest
    efekt na jakim nam zalezy. Rozwiazaniem jest ustawlic 1-znaczna
    triangulacje prostokata, wtedy linia podzialu zawsze bedzie taka
    sama i problemu nie bedzie.

    Jedyna rzecz jaka (poza 1-znacznym wyznaczeniem triangulacji) robi
    GL_TRIANGLE_FAN to dodawanie w srodku dodatkowych krawedzi.
    Mozna to zobaczyc gdy przelaczymy sie na widok wireframe.
    Nie mozna uzywac glEdgeFlag z GL_TRIANGLE_FAN, jest przez nie
    ignorowane. Wiec zeby obejsc ten problem trzeba recznie realizowac
    GL_TRIANGLE_FAN przez GL_TRIANGLES i tam uzywac glEdgeFlag.
    Zrobilem to - patrz VRMLNodes_old_funcs.txt, ale niestety
    okazalo sie to bardzo zwalniac rendering (nic dziwnego -
    - musialem przeciez podawac wiele razy te same vertexy). Wiec
    chwilowo tego nie uzywam. A to ze GL_TRIANGLES_FAN powoduje ze
    w widoku wireframe widoczne jest wiecej krawedzi ? No coz,
    to zaden problem, przeciez nikt mi nie kaze dokladnie renderowac
    obiektu wireframe. }

  glBegin(GL_TRIANGLE_FAN);
  while BeginIndex < endIndex do
  begin
   RenderVertex(BeginIndex);
   Inc(BeginIndex);
  end;
  glEnd;

  {$endif DARWIN}
 end;
end;

{ TIndexedFaceSet_1Renderer -------------------------------------------------- }

constructor TIndexedFaceSet_1Renderer.Create(
  ARenderer: TVRMLOpenGLRenderer);
var
  SH: TNodeShapeHints;
  ANode: TNodeIndexedFaceSet_1;
begin
  inherited Create(ARenderer);

  ANode := Geometry as TNodeIndexedFaceSet_1;

  TexCoordIndex := ANode.FdTextureCoordIndex;
  TexCoord := State.LastNodes.TextureCoordinate2;

  MaterialIndex := ANode.FdMaterialIndex;
  MaterialBinding := State.LastNodes.MaterialBinding.FdValue.Value;

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

constructor TIndexedFaceSet_2Renderer.Create(
  ARenderer: TVRMLOpenGLRenderer);
var
  ANode: TNodeIndexedFaceSet_2;
begin
  inherited Create(ARenderer);

  ANode := Geometry as TNodeIndexedFaceSet_2;

  TexCoordIndex := ANode.FdTexCoordIndex;
  TexCoord := ANode.FdTexCoord.Value;

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
  ColorPerVertex := ANode.FdColorPerVertex.Value;
  ColorIndex := ANode.FdColorIndex;
end;
