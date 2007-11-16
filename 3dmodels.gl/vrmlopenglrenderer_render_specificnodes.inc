{
  Copyright 2003-2006 Michalis Kamburelis.

  This file is part of "Kambi's 3dmodels.gl Pascal units".

  "Kambi's 3dmodels.gl Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's 3dmodels.gl Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's 3dmodels.gl Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ Notki do wyswietlania poszczegolnych node'ow :

  - Aktualny bound material jest niezdefiniowany na wejsciu dla node'ow VRMLa 1.0.
    So before you actually render anything for OpenGL, you must call
    Render_BindMaterial_1 at least once.
    Nie wolno tutaj wywolywac glMaterial lub glColor - tym zajmie sie
    Render_BindMaterial_1.

  - Tekstura wszystko co nalezy zrobic w shape'ach z tekstura to
    wygenerowac odpowiednie texCoordy na vertexach. Jezeli
    not Render_TexCoordsNeeded to mozna nie generowac texCoordow.
    (bo to znaczy ze tekstura jest i tak disabled)
    Nie wolno zmieniac tutaj bind tekstury czy texture matrix czy
    texture enabled.

  - Wszystkie wywolania ladujace poszczegolne vertexy OpenGL'owi
    musza byc robione przez DoGLVertex lub DoGLArrayElement.
    TODO: AsciiText, Cone, Cube, Cylinder, Sphere - nie uzywaja
    DoBeforeGLVertex w tej chwili, a powinny.
}

{ AsciiText ------------------------------------------------------------------ }

procedure Text_Render(
  const Size, Spacing: Single;
  const Justify: TVRMLFontJustify;
  const Strings: TDynStringArray;
  CurrentFont: TGLOutlineFont;
  const Depth: Single);

  { StringPos* ignore Size, just assume that CurrentFont has right size. }
  function StringPosX(i: integer): TGLfloat;
  begin
    case Justify of
      fjBegin : Result := 0;
      fjMiddle: Result := -CurrentFont.TextWidth(Strings[i]) / 2;
      fjend   : Result := -CurrentFont.TextWidth(Strings[i]);
      else raise EInternalError.Create('Invalid font justify value');
    end;
  end;

  function StringPosY(i: integer): TGLfloat;
  begin
    Result := -i * CurrentFont.RowHeight * Spacing;
  end;

var
  YScale, XScale: TGLfloat;
  i: integer;
begin
  YScale := Size / CurrentFont.RowHeight;
  { TODO: Use maxEntent, length for VRML 2.0.
    Use width for VRML 1.0. }
  XScale := YScale;

  Render_BindMaterial_1(0);

  glNormal3f(0, 0, -1);

  glPushMatrix;
    glScalef(1, YScale, 1);

    for I := 0 to Strings.Count - 1 do
    begin
      glPushMatrix;
        glScalef(XScale, 1, 1);

        { TODO: when using Justify <> fjBegin I should also
          change texOriginX here, at least for VRML 1.0 --- check
          for VRML 2.0. }
        glTranslatef(StringPosX(I), StringPosY(I), 0);

        { uzywamy ponizej print*AndMove bo przeciez za chwile i tak zrobimy
          popMatrix (wiec po co marnowac jedno miejsce na matrix stack ?) }
        if Render_TexCoordsNeeded then
          CurrentFont.PrintTexturedAndMove(Strings[i], 0, 0) else
          CurrentFont.PrintAndMove(Strings[i]);
      glPopMatrix;
    end;

    if Depth <> 0 then
    begin
      { TODO: backface culling could be used for fonts with kambiDepth <> 0 ? }

      for I := 0 to Strings.Count - 1 do
      begin
        glPushMatrix;
          glScalef(XScale, 1, 1);

          { TODO: when using Justify <> fjBegin I should also
            change texOriginX here, at least for VRML 1.0 --- check
            for VRML 2.0. }
          glTranslatef(StringPosX(I), StringPosY(I), -Depth);

          { uzywamy ponizej print*AndMove bo przeciez za chwile i tak zrobimy
            popMatrix (wiec po co marnowac jedno miejsce na matrix stack ?) }
          if Render_TexCoordsNeeded then
            CurrentFont.PrintTexturedAndMove(Strings[i], 0, 0) else
            CurrentFont.PrintAndMove(Strings[i]);
        glPopMatrix;
      end;

      { Normals will change during rendering of character extrusions,
        that's why I render extrusions after rendering all front and back caps.
        For now, normals for extrusions are suitable only for flat shading,
        so we set flat shading. }

      glPushAttrib(GL_LIGHTING_BIT);
        glShadeModel(GL_FLAT);

        for I := 0 to Strings.Count - 1 do
        begin
          glPushMatrix;
            glScalef(XScale, 1, 1);

            { TODO: when using Justify <> fjBegin I should also
              change texOriginX here, at least for VRML 1.0 --- check
              for VRML 2.0. }
            glTranslatef(StringPosX(I), StringPosY(I), -Depth);

            { uzywamy ponizej print*AndMove bo przeciez za chwile i tak zrobimy
              popMatrix (wiec po co marnowac jedno miejsce na matrix stack ?) }
            if Render_TexCoordsNeeded then
              CurrentFont.PrintTexturedExtrusionAndMove(Strings[i], Depth, 0, 0) else
              CurrentFont.PrintExtrusionAndMove(Strings[i], Depth);
          glPopMatrix;
        end;

      glPopAttrib;
    end;

  glPopMatrix;
end;

procedure RenderAsciiText(Node: TNodeAsciiText_1);
begin
  Text_Render(
    State.LastNodes.FontStyle.FdSize.Value,
    Node.FdSpacing.Value,
    Node.Justify,
    Node.FdString.Items,
    Cache.Fonts[
      State.LastNodes.FontStyle.Family,
      State.LastNodes.FontStyle.Bold,
      State.LastNodes.FontStyle.Italic].Instance,
    Node.FdKambiDepth.Value);
end;

procedure RenderText(Node: TNodeText);
var
  Size, Spacing: Single;
  Justify: TVRMLFontJustify;
  CurrentFont: TGLOutlineFont;
begin
  if Node.FontStyle = nil then
  begin
    Size := TNodeFontStyle_2.DefaultSize;
    Spacing := TNodeFontStyle_2.DefaultSpacing;
    Justify := TNodeFontStyle_2.DefaultJustify;
    CurrentFont := Cache.Fonts[
      TNodeFontStyle_2.DefaultFamily,
      TNodeFontStyle_2.DefaultBold,
      TNodeFontStyle_2.DefaultItalic].Instance;
  end else
  begin
    Size := Node.FontStyle.FdSize.Value;
    Spacing := Node.FontStyle.FdSpacing.Value;
    Justify := Node.FontStyle.Justify;
    CurrentFont := Cache.Fonts[
      Node.FontStyle.Family,
      Node.FontStyle.Bold,
      Node.FontStyle.Italic].Instance;
  end;

  Text_Render(Size, Spacing, Justify, Node.FdString.Items, CurrentFont,
    Node.FdKambiDepth.Value);
end;

{ Cone ----------------------------------------------------------------------- }

{ just a shortcuts for names often used in this file }
{$define QUADRIC_SLICES:=(State.LastNodes.KambiTriangulation.QuadricSlices)}
{$define QUADRIC_STACKS:=(State.LastNodes.KambiTriangulation.QuadricStacks)}
{$define RECT_DIVISIONS:=(State.LastNodes.KambiTriangulation.RectDivisions)}

procedure RenderBottomDisk(q: PGLuQuadric; radius: TGLdouble);
{ Render glu disk using quadric q. The disk will have texture applied
  in a way appropriate for bottom disk of VRML's cylinder and cone. }
begin
 if Render_TexCoordsNeeded then
 begin
  glPushMatrix;
  glRotatef(180, 1, 0, 0);
 end;
 gluDisk(q, 0, radius, QUADRIC_SLICES, 1);
 if Render_TexCoordsNeeded then glPopMatrix;
end;

procedure Cone_Render(
  const BottomRadius, Height: Single;
  const Side, Bottom: boolean);
var q: PGLUQuadric;
begin
 {cone uses its own normals and we apply texture according to VRML specification.
  Cone uses only material 0 or materials 0 and 1.}

 Render_BindMaterial_1(0);

 q := NewGLUQuadric(Ord(Render_TexCoordsNeeded),
   Attributes.SmoothNormalsGLU, GLU_OUTSIDE, GLU_FILL);
 try
  glPushMatrix;
  glRotatef(-90, 1, 0, 0);
  glTranslatef(0, 0, -Height/2);
  if Side then
   gluCylinder(q, BottomRadius, 0, Height, QUADRIC_SLICES, QUADRIC_STACKS);
  if Bottom then
  begin
   if State.LastNodes.MaterialBinding.FdValue.Value in
     [BIND_PER_PART, BIND_PER_PART_INDEXED] then
    Render_BindMaterial_1(1);
   RenderBottomDisk(q, BottomRadius);
  end;
  glPopMatrix;
 finally gluDeleteQuadric(q) end;
end;

procedure RenderCone_1(Node: TNodeCone_1);
begin
  Cone_Render(
    Node.FdBottomRadius.Value,
    Node.FdHeight.Value,
    Node.FdParts.Flags[CONE_PARTS_SIDES],
    Node.FdParts.Flags[CONE_PARTS_BOTTOM]);
end;

procedure RenderCone_2(Node: TNodeCone_2);
begin
  Cone_Render(
    Node.FdBottomRadius.Value,
    Node.FdHeight.Value,
    Node.FdSide.Value,
    Node.FdBottom.Value);
end;

{ Box ---------------------------------------------------------------------- }

procedure Box_Render(const SizeX, SizeY, SizeZ: Single);
var MatPerPart: boolean;

  procedure PartMat(MatNum: integer);
  begin
   if MatPerPart then Render_BindMaterial_1(MatNum);
  end;

{$define DetailX:=(State.LastNodes.KambiTriangulation.RectDivisions)}
{$define DetailY:=(State.LastNodes.KambiTriangulation.RectDivisions)}
{$define DetailZ:=(State.LastNodes.KambiTriangulation.RectDivisions)}

var x1, y1, z1, x2, y2, z2: TGLfloat;
begin
 MatPerPart := State.LastNodes.MaterialBinding.FdValue.Value in [
   BIND_PER_PART, BIND_PER_PART_INDEXED, BIND_PER_FACE, BIND_PER_FACE_INDEXED];
 if not MatPerPart then
  Render_BindMaterial_1(0); {use first material fot the whole cube}

 x2 := SizeX / 2; x1 := -x2;
 y2 := SizeY / 2; y1 := -y2;
 z2 := SizeZ / 2; z1 := -z2;

 {Do narysowania Cube swietnie nadawalaby sie procedura KambiGLUtils.drawGLBox -
  tyle ze ona nie pozwala mi zrobic Render_BindMaterial_1 przed kazda sciana i
  nie gwarantuje jak beda nakladane wspolrzedne tekstury.
  To co jest ponizej to po prostu implementacja drawGLBox z tymi dodatkami.}

 PartMat(2);
 DrawGLPlaneSpecialTex(y1, z1, y2, z2, x1, 0, DetailY, DetailZ, false, 0, 0, 1, 1, false);
 PartMat(3);
 DrawGLPlaneSpecialTex(y1, z1, y2, z2, x2, 0, DetailY, DetailZ, true,  0, 1, 1, 0, false);

 PartMat(5);
 DrawGLPlaneSpecialTex(x1, z1, x2, z2, y1, 1, DetailX, DetailZ, false, 0, 0, 1, 1, true);
 PartMat(4);
 DrawGLPlaneSpecialTex(x1, z1, x2, z2, y2, 1, DetailX, DetailZ, true,  0, 1, 1, 0, true);

 PartMat(1);
 DrawGLPlaneSpecialTex(x1, y1, x2, y2, z1, 2, DetailX, DetailY, false, 1, 0, 0, 1, true);
 PartMat(0);
 DrawGLPlaneSpecialTex(x1, y1, x2, y2, z2, 2, DetailX, DetailY, true,  0, 0, 1, 1, true);

 {$undef DetailX}
 {$undef DetailY}
 {$undef DetailZ}
end;

procedure RenderCube_1(Node: TNodeCube_1);
begin
  Box_Render(Node.FdWidth.Value, Node.FdHeight.Value, Node.FdDepth.Value);
end;

procedure RenderBox(Node: TNodeBox);
begin
  Box_Render(Node.FdSize.Value[0], Node.FdSize.Value[1], Node.FdSize.Value[2]);
end;

{ Cylinder ------------------------------------------------------------------ }

procedure Cylinder_Render(
  const Height, Radius: Single;
  const Bottom, Side, Top: boolean);
var q: PGLUQuadric;
begin
 {cylinder uses its own normals and we apply texture according to VRML
  specification. Cylinder uses only material 0 or materials 0, 1, 2}

 Render_BindMaterial_1(0);

 q := NewGLUQuadric(Ord(Render_TexCoordsNeeded),
   Attributes.SmoothNormalsGLU, GLU_OUTSIDE, GLU_FILL);
 try
  glPushMatrix;
  glRotatef(-90, 1, 0, 0);
  glTranslatef(0, 0, -Height/2);
  if Side then
   gluCylinder(q, Radius, Radius, Height,
     QUADRIC_SLICES, QUADRIC_STACKS);

  if Bottom then
  begin
   if State.LastNodes.MaterialBinding.FdValue.Value in
       [BIND_PER_PART, BIND_PER_PART_INDEXED] then
    Render_BindMaterial_1(2);
   RenderBottomDisk(q, Radius);
  end;

  if Top then
  begin
   if State.LastNodes.MaterialBinding.FdValue.Value in
       [BIND_PER_PART, BIND_PER_PART_INDEXED] then
    Render_BindMaterial_1(1);
   glTranslatef(0, 0, Height);
   gluDisk(q, 0, Radius, QUADRIC_SLICES, 1);
  end;

  glPopMatrix;
 finally gluDeleteQuadric(q) end;
end;

procedure RenderCylinder_1(Node: TNodeCylinder_1);
begin
  Cylinder_Render(
    Node.FdHeight.Value,
    Node.FdRadius.Value,
    Node.FdParts.Flags[CYLINDER_PARTS_BOTTOM],
    Node.FdParts.Flags[CYLINDER_PARTS_SIDES],
    Node.FdParts.Flags[CYLINDER_PARTS_TOP]);
end;

procedure RenderCylinder_2(Node: TNodeCylinder_2);
begin
  Cylinder_Render(
    Node.FdHeight.Value,
    Node.FdRadius.Value,
    Node.FdBottom.Value,
    Node.FdSide.Value,
    Node.FdTop.Value);
end;

{ PointSet --------------------------------------------------------------------- }

procedure RenderPointSet_1(Node: TNodePointSet_1);
var i, startIndex, numPoints: integer;
begin
  { TODO: handle various possible material binding here }
  Render_BindMaterial_1(0);

  { TODO: use here Last :
    Normal, NormalBinding, TextureCoordinate2, Texture2.
    For now light is already disabled by Render_Material
    from Render_BindMaterial_1 above. }

  { Render_BindMaterial_1 set unlit color to emissiveColor,
    so we correct it now. }
  with Render_State.LastNodes.Material do
    glColorv(Vector4f(Attributes.ColorModulated(
      DiffuseColor3Single(0)), Opacity(0)));

  Node.CalculateRange(State.LastNodes.Coordinate3, startIndex, numPoints);
  glBegin(GL_POINTS);
  for i := startIndex to startIndex+numPoints-1 do
    DoGLVertex(State.LastNodes.Coordinate3.FdPoint.Items.Items[i]);
  glEnd;
end;

procedure RenderPointSet_2(Node: TNodePointSet_2);

  procedure SetColor(const Color: TVector3Single);
  begin
    glColorv(Attributes.ColorModulated(Color));
  end;

var
  Coord: TMFVec3f;
  Color: TMFColor;
  Material: TNodeMaterial_2;
  I: Integer;
begin
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
      Material := Render_State.ParentShape.Material;
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

{ Sphere ---------------------------------------------------------------------- }

procedure Sphere_Render(const Radius: Single);
var q: PGLUQuadric;
begin
 {whole sphere uses materail num 0 and his own normals and we apply
  texture according to VRML specification}

 Render_BindMaterial_1(0);

 if Render_TexCoordsNeeded then
 begin
  {we have to rotate the sphere to get tex coords as specified by VRML}
  glPushMatrix;
  glRotatef(-90, 1, 0, 0);
 end;

 q := NewGLUQuadric(Ord(Render_TexCoordsNeeded),
   Attributes.SmoothNormalsGLU, GLU_OUTSIDE, GLU_FILL);
 try
  gluSphere(q, Radius, QUADRIC_SLICES, QUADRIC_STACKS);
 finally gluDeleteQuadric(q) end;

 if Render_TexCoordsNeeded then glPopMatrix;
end;

procedure RenderSphere_1(Node: TNodeSphere_1);
begin
  Sphere_Render(Node.FdRadius.Value);
end;

procedure RenderSphere_2(Node: TNodeSphere_2);
begin
  Sphere_Render(Node.FdRadius.Value);
end;

{$undef QUADRIC_SLICES}
{$undef QUADRIC_STACKS}
{$undef RECT_DIVISIONS}

{ TNodeElevationGrid --------------------------------------------------------- }

procedure RenderElevationGrid(Node: TNodeElevationGrid);
var
  TexCoordUseful: boolean;

  procedure TextureCoord(const I, J: Integer);
  begin
    if Render_TexCoordsNeeded then
    begin
      if TexCoordUseful then
        glTexCoordv(
          TNodeTextureCoordinate(Node.FdTexCoord.Value).FdPoint.
            Items.Items[I + J * Node.FdXDimension.Value]) else
        glTexCoord2f(I / (Node.FdXDimension.Value - 1),
                     J / (Node.FdZDimension.Value - 1));
    end;
  end;

var
  I, J: Integer;
  Last1, Last2, Next1, Next2: TVector3Single;
begin
  if Node.IsNotEmpty then
  begin
    TexCoordUseful :=
      (Node.FdTexCoord.Value <> nil) and
      (Node.FdTexCoord.Value is TNodeTextureCoordinate) and
      (TNodeTextureCoordinate(Node.FdTexCoord.Value).FdPoint.Count >=
        Node.FdXDimension.Value * Node.FdZDimension.Value);

    glPushAttrib(
      GL_LIGHTING_BIT { for shade model } or
      GL_POLYGON_BIT { for glFrontFace and GL_CULL_FACE });

      { TODO: for now we always generate normals, and always flat
        (1 normal per face) }
      glShadeModel(GL_FLAT);

      if not Node.FdCcw.Value then
        glFrontFace(GL_CW);

      if Node.FdSolid.Value then
        glEnable(GL_CULL_FACE);

      for I := 1 to Node.FdXDimension.Value - 1 do
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
          DoGLVertex(Last2);
          TextureCoord(I - 1, 0);
          DoGLVertex(Last1);

          for J := 1 to Node.FdZDimension.Value - 1 do
          begin
            Next1[1] := Node.FdHeight.Items.Items[
              I - 1 + J * Node.FdXDimension.Value];
            Next1[2] := Node.FdZSpacing.Value * J;
            Next2[1] := Node.FdHeight.Items.Items[
              I     + J * Node.FdXDimension.Value];
            Next2[2] := Next1[2];

            { As a normal we take avarage normal of two triangles of rendered
              quad. We do not normalize VectorAdd as we have GL_NORMALIZE
              enabled anyway. But we have to normalize two sum components
              (use TriangleNormal, not TriangleDir), otherwise taking
              the average woudn't be good. }
            glNormalv(VectorAdd(
              TriangleNormal(Last2, Last1, Next1),
              TriangleNormal(Last2, Next1, Next2)));

            TextureCoord(I, J);
            DoGLVertex(Next2);
            TextureCoord(I - 1, J);
            DoGLVertex(Next1);

            Last1 := Next1;
            Last2 := Next2;
          end;
        glEnd;
      end;
    glPopAttrib;
  end;
end;

{ eof ------------------------------------------------------------------------ }
