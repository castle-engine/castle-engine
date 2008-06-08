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

{ Utilities for VRML / X3D mesh rendering.
  This is internal for VRMLOpenGLRenderer unit.

  Some ideas for implementation:

  - VRMLOpenGLRenderer unit, and TVRMLOpenGLRenderer class are already
    large and complicated. The idea of this file is to take some tasks
    from their hands. TVRMLMeshRenderer and descendants are created
    only for the time of one RenderShapeState call, and are used
    to render specific non-trivial VRML mesh nodes.

  - This tries to reuse and reimplement some ideas that I got
    from TGeneralIndexedRenderer. TGeneralIndexedRenderer hierarchy
    was a good idea, but it was never cleanly enough implemented.
    Time showed that cleaner implementation of such hierarchy is needed,
    to be usable for rendering *all* VRML nodes.

  - Since all classes within this file and VRMLOpenGLRenderer live
    in one Pascal unit, we could break it and access each other's
    private fields etc. We try to not do it. We try to have a clean
    documented interface. Experience with TGeneralIndexedRenderer
    taught me that this has to be really flexible and designed for
    long-term work, to encompass many nodes and features. So, while
    this is internal for VRMLOpenGLRenderer, it still must be implemented
    and documented cleanly.

  - In the future, this may be abstracted from OpenGL and uses as a basis
    also for TVRMLGeometryNode.Triangulate methods implementation.
}

type
  TVRMLMeshRenderer = class
  private
    { Common code before both DoGLVertex and DoGLArrayElement.

      In this class:
      - calls Renderer.OnBeforeGLVertex if assigned.
      - sets fog coordinate.

      Passed Vert is local vertex coordinate (in State.Transform space),
      just like for glVertex. }
    procedure DoBeforeGLVertex(const Vert: TVector3Single);

    FRenderer: TVRMLOpenGLRenderer;
    FState: TVRMLGraphTraverseState;
    FGeometry: TVRMLGeometryNode;
    FAttributes: TVRMLRenderingAttributes;
  protected
    { DoGLVertex and DoGLArrayElement are the only two methods
      that can be used to render vertexes. One calls glVertex, second one
      glArrayElemene, this should be self-explanatory.

      Both call DoBeforeGLVertex at the beginning.

      For DoGLArrayElement, we also need Verts: an array of vertexes
      in local coordinates. The same one that you used to initialize
      OpenGL vertex coord array (we may need to access vertex coord ourselves). }
    procedure DoGLVertex(const Vert: TVector3Single);
    procedure DoGLArrayElement(const Verts: PArray_Vector3Single; ith: TGLint);

    { Current rendering properties, constant for the whole
      lifetime of the renderer, set in constructor.
      Setting Renderer automatically sets State, Geometry, Attributes. }
    property Renderer: TVRMLOpenGLRenderer read FRenderer;
    property State: TVRMLGraphTraverseState read FState;
    property Geometry: TVRMLGeometryNode read FGeometry;
    property Attributes: TVRMLRenderingAttributes read FAttributes;

    { Initialized in our constructor to false.
      Set this @italic(in constructor) (not later) to true when you're in
      subclass able to do bump mapping. }
    BumpMappingAllowed: boolean;
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer);

    { Do the actual render of the whole geometry.

      For some details what is, and what is not yet set in OpenGL
      before this, you have to see TVRMLOpenGLRender.RenderShapeState
      implementation.

      For now, see comments at the beginning of
      vrmlopenglrenderer_render_specificnodes.inc
      for general direction what can and what can not be used here. }
    procedure Render; virtual; abstract;

    { Initialized in our constructor to bmNone.
      Renderer will set this to Renderer.BumpMappingMethod
      only if bump mapping should be used and BumpMappingAllowed. }
    BumpMappingMethod: TBumpMappingMethod;
  end;

  TPointSet_1Renderer = class(TVRMLMeshRenderer)
    procedure Render; override;
  end;

  TPointSet_2Renderer = class(TVRMLMeshRenderer)
    procedure Render; override;
  end;

  TElevationGridRenderer = class(TVRMLMeshRenderer)
    procedure Render; override;
  end;

  TExtrusionRenderer = class(TVRMLMeshRenderer)
    procedure Render; override;
  end;

  { Base abstract class for rendering nodes with explicit "coord" field.
    (Both IndexedXxx and not indexed).

    Usage:
    - You must set Coord field in constructor, or (if Coord = @nil)
      nothing will be rendered.

    - Don't override Render --- this is already overridden here
      to do everything necessary. Instead, override
        RenderCoordinate
        RenderCoordinateBegin
        RenderCoordinateEnd

      These are called only when Coord is assigned.

      Render pushes client attribs and sets up vertex array for Coord.
      Then calls RenderCoordinateBegin and then locks arrays.
      So RenderCoordinateBegin is a good place to initialize other
      OpenGL arrays (for example, for texture coordinates or normal
      coordinates) if you want.

      Inside, RenderCoordinate is called.

      Then arrays are unlocked, RenderCoordinateEnd is called (so it
      should finalize whatever needs to be finalized for previous
      RenderCoordinateBegin call) and client attribs are popped.
  }
  TAbstractCoordinateRenderer = class(TVRMLMeshRenderer)
  protected
    { Coordinates, usually taken from (coord as Coordinate).points field.
      If @nil then nothing will be rendered. }
    Coord: TMFVec3f;

    { Coordinate index. Set this in descendant's constructor if it's
      indexed geometry.

      If @nil, then RenderVertex (and all other
      routines taking some index) will just directly index Coord
      (this is useful for non-indexed geometry, like TriangleSet
      instead of IndexedTriangleSet). }
    CoordIndex: TMFLong;

    { Use this in descendants as analogy of glVertex / glDrawElement.
      This sends vertex to OpenGL (using TVRMLMeshRenderer.DoGLDrawElement,
      but this is internal information :) ). Given IndexNum
      indexes Coord, or if (CoordIndex is assigned) indexes CoordIndex
      (and CoordIndex then indexes actual Coord). }
    procedure RenderVertex(IndexNum: integer); virtual;

    { This gets vertex coordinate, exactly the same as would be
      rendered by RenderVertex. Returned vertex is in local coordinate space
      (use State.Transform if you want to get global coordinates). }
    function GetVertex(IndexNum: integer): TVector3f;

    { Count of indexes. You can pass index between 0 and CoordCount - 1
      to various methods taking an index, like RenderVertex. }
    function CoordCount: Integer;

    procedure Render; override;

    procedure RenderCoordinate; virtual; abstract;
    procedure RenderCoordinateBegin; virtual;
    procedure RenderCoordinateEnd; virtual;
  end;

  { Renderer for X3D IndexedTriangleSet and TriangleSet nodes. }
  TTriangleSetRenderer = class(TAbstractCoordinateRenderer)
    constructor Create(ARenderer: TVRMLOpenGLRenderer);
    procedure RenderCoordinate; override;
  end;

{ TVRMLMeshRenderer ---------------------------------------------------------- }

constructor TVRMLMeshRenderer.Create(ARenderer: TVRMLOpenGLRenderer);
begin
  inherited Create;

  FRenderer := ARenderer;
  FState := Renderer.Render_State;
  FGeometry := Renderer.Render_Node;
  FAttributes := Renderer.Attributes;
end;

procedure TVRMLMeshRenderer.DoBeforeGLVertex(const Vert: TVector3Single);
var
  VertGlobal, VertProjected: TVector3Single;
  VertFogDistance: Single;
begin
  { We check first, to avoid calculating VertGlobal when it's not needed. }
  if Assigned(Attributes.OnBeforeGLVertex) or
     Renderer.FogVolumetric then
  begin
    VertGlobal := MultMatrixPoint(State.Transform, Vert);

    if Renderer.FogVolumetric then
    begin
      VertProjected := PointOnLineClosestToPoint(
        ZeroVector3Single, Renderer.FogVolumetricDirection, VertGlobal);
      VertFogDistance := VectorLen(VertProjected);
      if not AreParallelVectorsSameDirection(
        VertProjected, Renderer.FogVolumetricDirection) then
        VertFogDistance := -VertFogDistance;
      { Now I want
        - VertFogDistance = FogVolumetricVisibilityStart -> 0
        - VertFogDistance = FogVolumetricVisibilityStart + X -> X
          (so that VertFogDistance = FogVolumetricVisibilityStart +
          FogVisibilityRangeScaled -> FogVisibilityRangeScaled) }
      VertFogDistance -= Renderer.FogVolumetricVisibilityStart;

      { When VertFogDistance < 0 our intention is to have no fog.
        So VertFogDistance < 0 should be equivalent to VertFogDistance = 0.
        However, OpenGL doesn't necessarily interpret it like this.

        Since factor given by glFogCoordfEXT is interpreted just like
        eye distance (i.e. it's processed by appopriate linear or exp or exp2
        equations), negative values may produce quite unexpected results
        (unless you really look at the equations).

        This is mentioned in the extension specification
        [http://oss.sgi.com/projects/ogl-sample/registry/EXT/fog_coord.txt].
        First is says:

          * Should the specified value be used directly as the fog weighting
            factor, or in place of the z input to the fog equations?

    	    As the z input; more flexible and meets ISV requests.

        ... which means that what glFogCoordfEXT gives is interpreted
        just like eye distance for normal fog (so it's e.g. affected
        by fog linear / exp / exp2 modes, affected by fog start and end values,
        etc.). Later it says:

          * Should the fog coordinate be restricted to non-negative values?

            Perhaps. Eye-coordinate distance of fragments will be
            non-negative due to clipping. Specifying explicit negative
            coordinates may result in very large computed f values, although
            they are defined to be clipped after computation.

        ... and this is precisely why specifying negative glFogCoordfEXT
        parameters is a bad idea: you don't really know what OpenGL
        implementation will do. NVidia OpenGL seems to actually assume
        that factor < 0 means the same as factor = 0, so my code
        worked OK without this "MaxTo1st(VertFogDistance, 0);" below
        (because NVidia OpenGL was actually doing it anyway).
        Mesa 3D (and Radeon, as I suspect, because similar problems
        were reported for "The Castle" on Radeon) seem to just use the negative
        value directly, which causes strange artifacts
        (see e.g. "The Castle" gate_final.wrl VRML file).

        The line "MaxTo1st(VertFogDistance, 0);" makes volumetric fog work
        OK as expected for all OpenGL implementations. }
      MaxTo1st(VertFogDistance, 0);
      glFogCoordfEXT(VertFogDistance);
    end;

    if Assigned(Attributes.OnBeforeGLVertex) then
      Attributes.OnBeforeGLVertex(Geometry, VertGlobal);
  end;
end;

procedure TVRMLMeshRenderer.DoGLVertex(const Vert: TVector3Single);
begin
  DoBeforeGLVertex(Vert);
  glVertexv(Vert);
end;

procedure TVRMLMeshRenderer.DoGLArrayElement(
  const Verts: PArray_Vector3Single; ith: TGLint);
begin
  DoBeforeGLVertex(Verts^[ith]);
  glArrayElement(ith);
end;

{ TPointSet_1Renderer -------------------------------------------------------- }

procedure TPointSet_1Renderer.Render;
var
  i, startIndex, numPoints: integer;
  Node: TNodePointSet_1;
begin
  Node := Geometry as TNodePointSet_1;
  { TODO: handle various possible material binding here }
  Renderer.Render_BindMaterial_1(0);

  { TODO: use here Last :
    Normal, NormalBinding, TextureCoordinate2, Texture2.
    For now light is already disabled by Render_Material
    from Render_BindMaterial_1 above. }

  { Render_BindMaterial_1 set unlit color to emissiveColor,
    so we correct it now. }
  if not Attributes.PureGeometry then
    with State.LastNodes.Material do
      glColorv(Vector4f(Attributes.ColorModulated(
        DiffuseColor3Single(0)), Opacity(0)));

  Node.CalculateRange(State.LastNodes.Coordinate3, startIndex, numPoints);
  glBegin(GL_POINTS);
  for i := startIndex to startIndex+numPoints-1 do
    DoGLVertex(State.LastNodes.Coordinate3.FdPoint.Items.Items[i]);
  glEnd;
end;

{ TPointSet_2Renderer -------------------------------------------------------- }

procedure TPointSet_2Renderer.Render;
var
  Node: TNodePointSet_2;

  procedure SetColor(const Color: TVector3Single);
  begin
    if not Attributes.PureGeometry then
      glColorv(Attributes.ColorModulated(Color));
  end;

var
  Coord: TMFVec3f;
  Color: TMFColor;
  Material: TNodeMaterial_2;
  I: Integer;
begin
  Node := Geometry as TNodePointSet_2;

  { In VRML 2.0 PointSet is always unlit and not textured.
    Light is already disabled by Render_Material. }

  if (Node.FdCoord.Value <> nil) and
     (Node.FdCoord.Value is TNodeCoordinate) then
  begin
    Coord := TNodeCoordinate(Node.FdCoord.Value).FdPoint;

    { PointSet color may come from various places:
      1. Color node, for each point, if it's not NULL
      2. Material.emissiveColor, for every point
      3. If no material, we use default White3Single, for every point
         (following general spec remark at Material node that
         Material = NULL makes unlit white color) }
    Color := nil;
    if (Node.FdColor.Value <> nil) and
       (Node.FdColor.Value is TNodeColor) then
    begin
      Color := TNodeColor(Node.FdColor.Value).FdColor;
      if Color.Count < Coord.Count then
      begin
        VRMLNonFatalError('Not enough colors specified for PointSet');
        Color := nil;
      end;
    end;

    if Color = nil then
    begin
      Material := State.ParentShape.Material;
      if Material <> nil then
        SetColor(Material.FdEmissiveColor.Value) else
        SetColor(White3Single);
    end;

    glBegin(GL_POINTS);
    try
      for I := 0 to Coord.Count - 1 do
      begin
        if Color <> nil then
          SetColor(Color.Items.Items[I]);
        DoGLVertex(Coord.Items.Items[i]);
      end;
    finally glEnd end;
  end;
end;

{ TElevationGridRenderer --------------------------------------------------------- }

procedure TElevationGridRenderer.Render;
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
        VRMLNonFatalError(SNotEnoughNormals);
        GenerateNormals;
      end;
    end else
      GenerateNormals;
  end;

var
  TexCoordUseful: boolean;

  procedure TextureCoord(const I, J: Integer);
  begin
    if Renderer.Render_TexCoordsNeeded then
    begin
      if TexCoordUseful then
        glTexCoordv(
          TNodeTextureCoordinate(Node.FdTexCoord.Value).FdPoint.
            Items.Items[I + J * XDimension]) else
        glTexCoord2f(I / (XDimension - 1),
                     J / (ZDimension - 1));
    end;
  end;

var
  Normal: TDynVector3SingleArray;
  NormalPerVertex: boolean;
  NormalGenerated: boolean;

  procedure VertexNormal(const I, J: Integer);
  begin
    if NormalPerVertex then
      glNormalv(Normal.Items[I + J * XDimension]);
  end;

var
  I, J: Integer;
  Last1, Last2, Next1, Next2: TVector3Single;
begin
  Node := Geometry as TNodeElevationGrid;
  XDimension := Node.FdXDimension.Value;
  ZDimension := Node.FdZDimension.Value;

  if Node.IsNotEmpty then
  begin
    CalculateNormals(Normal, NormalPerVertex, NormalGenerated);

    TexCoordUseful :=
      (Node.FdTexCoord.Value <> nil) and
      (Node.FdTexCoord.Value is TNodeTextureCoordinate) and
      (TNodeTextureCoordinate(Node.FdTexCoord.Value).FdPoint.Count >=
        XDimension * ZDimension);

    glPushAttrib(
      GL_LIGHTING_BIT { for shade model } or
      GL_POLYGON_BIT { for glFrontFace and GL_CULL_FACE });

      if not NormalPerVertex then
        glShadeModel(GL_FLAT);

      if Node.FdSolid.Value then
      begin
        { About normals orientation, ccw and solid fields:

          See Extrusion implementation for general notes.
          In short, ccw only says which side is visible(if solid)/lighted
          (if one-sided lighting). We always want to generate normals from
          CCW, and avoid calling glFrontFace.

          Spec says that xSpacing and zSpacing should always be > 0,
          so always up (where height points to) is visible when ccw = TRUE
          and solid = TRUE. So flipping ccw simply culls the other face. }

        if Node.FdCcw.Value then
          glCullFace(GL_BACK) else
          glCullFace(GL_FRONT);

        glEnable(GL_CULL_FACE);
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

          TextureCoord(I, 0);
          VertexNormal(I, 0);
          DoGLVertex(Last2);

          TextureCoord(I - 1, 0);
          VertexNormal(I - 1, 0);
          DoGLVertex(Last1);

          for J := 1 to ZDimension - 1 do
          begin
            Next1[1] := Node.FdHeight.Items.Items[
              I - 1 + J * XDimension];
            Next1[2] := Node.FdZSpacing.Value * J;
            Next2[1] := Node.FdHeight.Items.Items[
              I     + J * XDimension];
            Next2[2] := Next1[2];

            if not NormalPerVertex then
              glNormalv(Normal.Items[I - 1 + (J - 1) * (XDimension - 1)]);

            TextureCoord(I, J);
            VertexNormal(I, J);
            DoGLVertex(Next2);

            TextureCoord(I - 1, J);
            VertexNormal(I - 1, J);
            DoGLVertex(Next1);

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

{ TExtrusionRenderer --------------------------------------------------------- }

type
  TExtrusionTriangleProcData = record
    Node: TNodeExtrusion;
    Render_TexCoordsNeeded: boolean;
    TexCapsTranslation: Single;
    TexCapsScale: Single;
  end;
  PExtrusionTriangleProcData = ^TExtrusionTriangleProcData;

procedure ExtrusionCapTriangle(const Tri: TVector3Longint;
  TriangleProcData: Pointer);
var
  Data: PExtrusionTriangleProcData absolute TriangleProcData;

  procedure CapTexCoord(Index: Integer);
  begin
    with Data^ do
      if Render_TexCoordsNeeded then
      begin
        glTexCoord2f(
          (Node.FdCrossSection.Items.Items[Index][0] + TexCapsTranslation)
          * TexCapsScale,
          (Node.FdCrossSection.Items.Items[Index][1] + TexCapsTranslation)
          * TexCapsScale);
      end;
  end;

begin
  with Data^ do
  begin
    CapTexCoord(Tri[0]);
    glVertexv(Node.CrossSection3D(Tri[0]));
    CapTexCoord(Tri[1]);
    glVertexv(Node.CrossSection3D(Tri[1]));
    CapTexCoord(Tri[2]);
    glVertexv(Node.CrossSection3D(Tri[2]));
  end;
end;

procedure TExtrusionRenderer.Render;
var
  Node: TNodeExtrusion;
  TexCapsTranslation, TexCapsScale: Single;

  procedure DrawCap;

    procedure CapTexCoord(Index: Integer);
    begin
      if Renderer.Render_TexCoordsNeeded then
      begin
        glTexCoord2f(
          (Node.FdCrossSection.Items.Items[Index][0] + TexCapsTranslation)
          * TexCapsScale,
          (Node.FdCrossSection.Items.Items[Index][1] + TexCapsTranslation)
          * TexCapsScale);
      end;
    end;

    procedure DrawConvexCap;
    var
      I: Integer;
    begin
      glBegin(GL_TRIANGLE_FAN);
        for I := 0 to Node.FdCrossSection.Count - 1 do
        begin
          CapTexCoord(I);
          glVertexv(Node.CrossSection3D(I));
        end;
      glEnd();
    end;

  var
    TriangleProcData: TExtrusionTriangleProcData;
  begin
    if Node.FdConvex.Value then
    begin
      { We could call TriangulateConvexFace, but it's faster to do it directly,
        using OpenGL triangle_fan }
      DrawConvexCap;
    end else
    begin
      TriangleProcData.Node := Node;
      TriangleProcData.TexCapsTranslation := TexCapsTranslation;
      TriangleProcData.TexCapsScale := TexCapsScale;
      TriangleProcData.Render_TexCoordsNeeded := Renderer.Render_TexCoordsNeeded;

      glBegin(GL_TRIANGLES);
        TriangulateFace(nil, Node.FdCrossSection.Count,
          @Node.CrossSection3D, @ExtrusionCapTriangle,
          @TriangleProcData, 0);
      glEnd();
    end;
  end;

type
  T01 = 0..1;
var
  { We have separate pointers and data for matrix, to speed up
    Transform[0] := Transform[1] operation. It's much faster to
    copy a pointer than 16 Single floats. }
  TransformData: array [T01] of TMatrix4Single;
  Transform: array [T01] of PMatrix4Single;

  procedure MakeCrossPoint(out Point: TVector3Single; WhichTransform: T01;
    CrossIndex: Integer);
  var
    Point2d: PVector2Single;
  begin
    Point2d := @Node.FdCrossSection.Items.Items[CrossIndex];
    Point := MultMatrixPoint(Transform[WhichTransform]^,
      Vector3Single(Point2d^[0], 0, Point2d^[1]));
  end;

var
  E: TVRMLExtrusion;

  procedure SideTexCoord(SpineIndex, CrossSectionIndex: Integer);
  var
    U, V: Single;
  begin
    if Renderer.Render_TexCoordsNeeded then
    begin
      { Check ranges, to avoid dividing by zero (eventually the
        corresponding coord is set to a preditable zero :) ). }
      if E.High > 0 then
        V := SpineIndex / E.High else
        V := 0;
      if Node.FdCrossSection.Count > 1 then
        U := CrossSectionIndex / (Node.FdCrossSection.Count - 1) else
        U := 0;
      glTexCoord2f(U, V);
    end;
  end;

var
  I, J: Integer;
  LastY, LastZ: TVector3Single;
  Cross: array [T01, T01] of TVector3Single;
  TransformCap: TMatrix4Single;
  IsBeginCapCCW: boolean;
  Normal: TVector3Single;
  TexCapsMin, TexCapsMax: TVector2Single;
begin
  Node := Geometry as TNodeExtrusion;
  if Node.FdCrossSection.Count = 0 then Exit;

  { IsBeginCapCCW =
    when viewed from the positive axis (in local coords of spine 0)
    on begin cap, is it CCW?
    In other words, when IsBeginCapCCW then the normal vector
    (in local coords of spine 0) from CCW is (0, -1, 0).
    Remember that FdCrossSection specify X, Z values. }
  IsBeginCapCCW := IsPolygon2dCCW(Node.FdCrossSection.Items.ItemsArray,
    Node.FdCrossSection.Items.Count) > 0;

  glPushAttrib(GL_LIGHTING_BIT { for shade model } or
    GL_POLYGON_BIT { for glCullFace and CULL_FACE enable state });

  if Node.FdSolid.Value then
  begin
    { An explanation of ccw and solid fields interpretation for Extrusion:

      "ccw" field actually doesn't say how we should generate normals.
      Obviously, this is the decision of the implementor, like us,
      and we always want to generate normals from CCW (see comments
      at the beginning of this unit to know why we don't want ever to use
      glFrontFace in this unit).
      Instead, "ccw" says from which side the normal should go for the sake
      of interpreting solid (if enabled) and one-side lighting (if enabled ---
      although we always do two_side lighting, for now). The object is visible
      only from the side where faces are CCW, and lighted only from CCW,
      unless ccw = FALSE, when it's visible/lighted only when faces are CW.

      Also "ccw" says what normals the user provided, but this is not
      important for Extrusion --- user cannot provide normals for
      Extrusion, we always generate them.

      So what does CCW really say for Extrusion?
      It says which face is visible(if solid)/lighted.
      Assuming the order as spec says

        spine[0](crossSection[0], crossSection[1])
        spine[1](crossSection[1], crossSection[0])

      then sides which are CCW with the above order should be visible,
      and CW invisible (assuming solid = TRUE and ccw = TRUE;
      for ccw = FALSE, visibility is reverted).

      Note that this doesn't precisely say whether inside or outside
      is visible: direction of crossSection (ordered cw or ccw)
      determines whether the above
      (e.g. for solid = TRUE and ccw = TRUE) will result in culling
      outside or inside. And this is good, I mean the VRML author
      has to take care to write crossSection in good direction.
      We don't have to detect it.

      For sides:

      Now we generate normals from CCW with the same order
      as spec says. So when CCW = true, we have to cull CW,
      which is simply back. When CCW = false, we have to cull CCW,
      which is front.

      For caps:

      (As I understand VRML spec, I should just adjust so that
      always all outside or all inside is culled).
      Begin cap with our default order (which is just natural order
      of crossSection) gets CW on the same side where sides are CCW,
      so the glCullFace will be reverted there. End cap will have
      CCW on the same side as sides, so it will have glCullFace just
      like our sides. }

    { We set below glCullFace for sides of the extrusion.
      Caps will also be done under GL_CULL_FACE, but they will
      potentially change glCullFace. }

    if Node.FdCcw.Value then
      glCullFace(GL_BACK) else
      glCullFace(GL_FRONT);
    glEnable(GL_CULL_FACE);
  end;

  { TODO: for now we always generate flat normals(1 normal per face) }
  glShadeModel(GL_FLAT);

  Transform[0] := @TransformData[0];
  Transform[1] := @TransformData[1];

  E := TVRMLExtrusion.Create;
  try
    { This is just to shut up FPC 2.2.0 warnings about Cross not initialized. }
    Cross[0, 0][0] := 0.0;

    E.Node := Node;
    if E.High > 0 then
    begin
      E.SpineTransformTo1st(0, LastY, LastZ, Transform[0]^);
      for I := 1 to E.High do
      begin
        E.SpineTransformTo1st(I, LastY, LastZ, Transform[1]^);

        glBegin(GL_QUAD_STRIP);

        { I first render Cross[1, 0], then Cross[0, 0]
          as this sets direction for the first quad.
          This isn't really essential, but this way I render points
          in the same order as indicated by Extrusion X3D / VRML spec,
          which just helps me wrap my mind around it :) (especially
          when thinking about this ccw/solid). }

        MakeCrossPoint(Cross[1, 0], 1, 0);
        MakeCrossPoint(Cross[0, 0], 0, 0);

        SideTexCoord(I    , 0); glVertexv(Cross[1, 0]);
        SideTexCoord(I - 1, 0); glVertexv(Cross[0, 0]);

        for J := 1 to Node.FdCrossSection.Count - 1 do
        begin
          MakeCrossPoint(Cross[1, 1], 1, J);
          MakeCrossPoint(Cross[0, 1], 0, J);

          { Normal is average of normals of two triangles. }
          Normal := VectorAdd(
            TriangleNormal(Cross[1, 0], Cross[0, 0], Cross[0, 1]),
            TriangleNormal(Cross[0, 1], Cross[1, 1], Cross[1, 0]));
          glNormalv(Normal);

          { Now make a quad using 4 Cross points. }
          SideTexCoord(I    , J); glVertexv(Cross[1, 1]);
          SideTexCoord(I - 1, J); glVertexv(Cross[0, 1]);

          Cross[1, 0] := Cross[1, 1];
          Cross[0, 0] := Cross[0, 1];
        end;

        glEnd();

        { I don't care about preserving current Transform[0]^.
          But current Transform[1]^ must become Transform[0]^ in the
          next loop roll. So I exchange pointers. }
        SwapValues(Transform[0], Transform[1]);
      end;
    end;

    if Node.FdBeginCap.Value or
       Node.FdEndCap.Value then
    begin
      if Renderer.Render_TexCoordsNeeded then
      begin
        { calculate TexCapsTranslation, TexCapsScale }
        TexCapsTranslation := 0;
        TexCapsScale := 1;
        if Node.FdCrossSection.Items.MinMax(TexCapsMin, TexCapsMax) then
        begin
          if TexCapsMax[0] - TexCapsMin[0] >=
             TexCapsMax[1] - TexCapsMin[1] then
          begin
            if not IsZero(TexCapsMax[0] - TexCapsMin[0]) then
            begin
              { scale such that U is along the X of cap.
                Coord calculation will go like
                  u := (x - TexCapsMin[0]) / (TexCapsMax[0] - TexCapsMin[0]) }
              TexCapsTranslation := -TexCapsMin[0];
              TexCapsScale := 1 /  (TexCapsMax[0] - TexCapsMin[0]);
            end;
          end else
          begin
            if not IsZero(TexCapsMax[1] - TexCapsMin[1]) then
            begin
              TexCapsTranslation := -TexCapsMin[1];
              TexCapsScale := 1 /  (TexCapsMax[1] - TexCapsMin[1]);
            end;
          end;
        end;
      end;

      if Node.FdBeginCap.Value then
      begin
        { calculate TransformCap.
          Note that LastY, LastZ are just ignored here (they are "out only"
          for Spine = 0). }
        E.SpineTransformTo1st(0, LastY, LastZ, TransformCap);

        glPushMatrix;
          glMultMatrix(TransformCap);
          if Node.FdCcw.Value then
            glCullFace(GL_FRONT) else
            glCullFace(GL_BACK);
          if IsBeginCapCCW then
            glNormal3f(0, -1, 0) else
            glNormal3f(0, +1, 0);
          DrawCap;
        glPopMatrix;
      end;

      if Node.FdEndCap.Value then
      begin
        { calculate TriangleProcData.TransformEnd. }
        if E.High > 0 then
          { We already have it in Transform[0]^.
            This is most fortunate, otherwise calculating last transform
            is not easy (we have to iterate over whole spine for correct
            LastY, LastZ values). }
          TransformCap := Transform[0]^ else
          { Then this can be calculated just like TransformBegin
            (or copied from there, if available) }
        if not Node.FdBeginCap.Value then
          E.SpineTransformTo1st(0, LastY, LastZ, TransformCap);
          { else calculations for Node.FdBeginCap.Value already set TransformCap }

        { I used to have here
            TexCapsZScale := -1;
          (see diff between revision 2752 and 2753 for details).
          This way texture was correctly aligned on both begin and end cap
          sides. VRML / X3D spec doesn't say whether this is needed,
          saying only that Z axis aligns with texture T axis. Interpreting
          it naturally, this suggests that TexCapsZScale should not be done.

          For some time TexCapsZScale trick was present because the image in
          "Annotated VRML 97 reference" about Extrusion suggests this.

          On the other hand, NIST VRML test (Appearance.ImageTexture test 12,
          "Test of default Extrusion mapping for JPG files." shows that
          top cap has reverted texture.

          For now, TexCapsZScale is not done, as this seems more natural
          with spec (nothing suggests there to do such trick) and passes
          NIST test. }

        glPushMatrix;
          glMultMatrix(TransformCap);
          if Node.FdCcw.Value then
            glCullFace(GL_BACK) else
            glCullFace(GL_FRONT);
          if IsBeginCapCCW then
            glNormal3f(0, -1, 0) else
            glNormal3f(0, +1, 0);
          DrawCap;
        glPopMatrix;
      end;
    end;

  finally FreeAndNil(E) end;

  glPopAttrib;
end;

{ TAbstractCoordinateRenderer ------------------------------------------------ }

procedure TAbstractCoordinateRenderer.Render;
var
  { This is used only by LockArraysBegin and LockArraysEnd. }
  UseLockArrays: boolean;

  procedure LockArraysBegin;
  begin
    { See
      [http://www.opengl.org/documentation/specs/version1.2/EXTspecs/compiled_vertex_array.txt]
      for description of GL_EXT_compiled_vertex_array.

      Note that I can't use it when Coord has no items
      (because glLockArraysEXT(0, 0) causes OpenGL error "invalid value".) }
    UseLockArrays := GL_EXT_compiled_vertex_array and
      (Coord.Items.Count <> 0);
    if UseLockArrays then
      glLockArraysEXT(0, Coord.Items.Count);
  end;

  procedure LockArraysEnd;
  begin
    if UseLockArrays then
      glUnlockArraysEXT;
  end;

begin
  if Coord = nil then
    Exit;

  { Initialize vertex arrays that we we will use with indexed nodes.
    GL_CLIENT_VERTEX_ARRAY_BIT pushes state of every vertex array, including
    eventual normal and texture coord arrays (possibly set by
    RenderCoordinateBegin). }
  glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT);
  try
    glVertexPointer(3, GL_FLOAT, 0, Coord.Items.Items);
    glEnableClientState(GL_VERTEX_ARRAY);

    RenderCoordinateBegin;
    try
      { Lock arrays after setting up all arrays. }
      LockArraysBegin;
      try
        RenderCoordinate;
      finally LockArraysEnd; end;
    finally RenderCoordinateEnd; end;
  finally glPopClientAttrib; end;
end;

procedure TAbstractCoordinateRenderer.RenderCoordinateBegin;
begin
  { nothing to do in this class }
end;

procedure TAbstractCoordinateRenderer.RenderCoordinateEnd;
begin
  { nothing to do in this class }
end;

procedure TAbstractCoordinateRenderer.RenderVertex(IndexNum: integer);
var
  VertexNum: integer;
begin
  if CoordIndex <> nil then
    VertexNum := CoordIndex.Items.Items[IndexNum] else
    VertexNum := IndexNum;

  DoGLArrayElement(Coord.Items.ItemsArray, VertexNum);
end;

function TAbstractCoordinateRenderer.GetVertex(IndexNum: integer): TVector3f;
var
  VertexNum: integer;
begin
  if CoordIndex <> nil then
    VertexNum := CoordIndex.Items.Items[IndexNum] else
    VertexNum := IndexNum;

  Result := Coord.Items.Items[VertexNum];
end;

function TAbstractCoordinateRenderer.CoordCount: Integer;
begin
  if CoordIndex <> nil then
    Result := CoordIndex.Items.Count else
    Result := Coord.Items.Count;
end;

{ TTriangleSetRenderer ------------------------------------------------------- }

constructor TTriangleSetRenderer.Create(ARenderer: TVRMLOpenGLRenderer);
begin
  inherited;

  Coord := (Geometry as TNodeX3DComposedGeometryNode).Coord;
  if Geometry is TNodeIndexedTriangleSet then
  begin
    CoordIndex := TNodeIndexedTriangleSet(Geometry).FdIndex;
  end;
end;

procedure TTriangleSetRenderer.RenderCoordinate;
var
  I: Integer;
begin
  I := 0;
  glBegin(GL_TRIANGLES);
    { X3D spec says "If the Coordinate node does not contain
      a multiple of three coordinate values, the remaining
      vertices shall be ignored.".
      So we silently ignore any vertices above multiple of 3. }
    while I + 2 < CoordCount do
    begin
      { TODO: for now, we always generate flat normals }
      glNormalv(TriangleNormal(
        GetVertex(I    ),
        GetVertex(I + 1),
        GetVertex(I + 2)));

      RenderVertex(I    );
      RenderVertex(I + 1);
      RenderVertex(I + 2);

      I += 3;
    end;
  glEnd();
end;
