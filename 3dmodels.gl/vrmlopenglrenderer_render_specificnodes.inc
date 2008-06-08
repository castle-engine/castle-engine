{
  Copyright 2003-2008 Michalis Kamburelis.

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

{$ifdef read_interface}

procedure Text_Render(
  const Size, Spacing: Single;
  const Justify: TVRMLFontJustify;
  const Strings: TDynStringArray;
  CurrentFont: TGLOutlineFont;
  const Depth: Single;
  const MarkFrontFaces: boolean);
procedure RenderAsciiText(Node: TNodeAsciiText_1);
procedure RenderText(Node: TNodeText);
procedure RenderText3D(Node: TNodeText3D);

procedure RenderBottomDisk(q: PGLuQuadric; radius: TGLdouble);

procedure Cone_Render(
  const BottomRadius, Height: Single;
  const Side, Bottom: boolean);
procedure RenderCone_1(Node: TNodeCone_1);
procedure RenderCone_2(Node: TNodeCone_2);

procedure Box_Render(const SizeX, SizeY, SizeZ: Single);
procedure RenderCube_1(Node: TNodeCube_1);
procedure RenderBox(Node: TNodeBox);

procedure Cylinder_Render(
  const Height, Radius: Single;
  const Bottom, Side, Top: boolean);
procedure RenderCylinder_1(Node: TNodeCylinder_1);
procedure RenderCylinder_2(Node: TNodeCylinder_2);

procedure Sphere_Render(const Radius: Single);
procedure RenderSphere_1(Node: TNodeSphere_1);
procedure RenderSphere_2(Node: TNodeSphere_2);

{$endif read_interface}

{$ifdef read_implementation}

{ AsciiText ------------------------------------------------------------------ }

procedure TVRMLOpenGLRenderer.Text_Render(
  const Size, Spacing: Single;
  const Justify: TVRMLFontJustify;
  const Strings: TDynStringArray;
  CurrentFont: TGLOutlineFont;
  const Depth: Single;
  const MarkFrontFaces: boolean);

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

  glPushMatrix;
    glScalef(1, YScale, 1);

    if MarkFrontFaces then
    begin
      { Outside face is CW now, so cull CCW. CCW = front face
        (remember, we can't use glFrontFace, we have to work with glCullFace here) }
      glCullFace(GL_FRONT);
    end;

    { Normal pointing from CCW. }
    glNormal3f(0, 0, -1);

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
      if MarkFrontFaces then
      begin
        { Outside face is CCW now, so cull CW. CW = back face
          (remember, we can't use glFrontFace, we have to work with glCullFace here) }
        glCullFace(GL_BACK);
      end;

      { Normal pointing from CCW. }
      glNormal3f(0, 0, -1);

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

procedure TVRMLOpenGLRenderer.RenderAsciiText(Node: TNodeAsciiText_1);
begin
  Text_Render(
    Render_State.LastNodes.FontStyle.FdSize.Value,
    Node.FdSpacing.Value,
    Node.Justify,
    Node.FdString.Items,
    Cache.Fonts[
      Render_State.LastNodes.FontStyle.Family,
      Render_State.LastNodes.FontStyle.Bold,
      Render_State.LastNodes.FontStyle.Italic].Instance, 0, false);
end;

procedure TVRMLOpenGLRenderer.RenderText(Node: TNodeText);
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

  Text_Render(Size, Spacing, Justify, Node.FdString.Items, CurrentFont, 0, false);
end;

procedure TVRMLOpenGLRenderer.RenderText3D(Node: TNodeText3D);
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

  if Node.FdSolid.Value then
  begin
    glPushAttrib(GL_POLYGON_BIT);
    glEnable(GL_CULL_FACE);
  end;

  Text_Render(Size, Spacing, Justify, Node.FdString.Items, CurrentFont,
    Node.FdDepth.Value, Node.FdSolid.Value);

  if Node.FdSolid.Value then
  begin
    glPopAttrib;
  end;
end;

{ Cone ----------------------------------------------------------------------- }

{ just a shortcuts for names often used in this file }
{$define QUADRIC_SLICES:=(Render_State.LastNodes.KambiTriangulation.QuadricSlices)}
{$define QUADRIC_STACKS:=(Render_State.LastNodes.KambiTriangulation.QuadricStacks)}
{$define RECT_DIVISIONS:=(Render_State.LastNodes.KambiTriangulation.RectDivisions)}

procedure TVRMLOpenGLRenderer.RenderBottomDisk(
  q: PGLuQuadric; radius: TGLdouble);
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

procedure TVRMLOpenGLRenderer.Cone_Render(
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
   if Render_State.LastNodes.MaterialBinding.FdValue.Value in
     [BIND_PER_PART, BIND_PER_PART_INDEXED] then
    Render_BindMaterial_1(1);
   RenderBottomDisk(q, BottomRadius);
  end;
  glPopMatrix;
 finally gluDeleteQuadric(q) end;
end;

procedure TVRMLOpenGLRenderer.RenderCone_1(Node: TNodeCone_1);
begin
  Cone_Render(
    Node.FdBottomRadius.Value,
    Node.FdHeight.Value,
    Node.FdParts.Flags[CONE_PARTS_SIDES],
    Node.FdParts.Flags[CONE_PARTS_BOTTOM]);
end;

procedure TVRMLOpenGLRenderer.RenderCone_2(Node: TNodeCone_2);
begin
  Cone_Render(
    Node.FdBottomRadius.Value,
    Node.FdHeight.Value,
    Node.FdSide.Value,
    Node.FdBottom.Value);
end;

{ Box ---------------------------------------------------------------------- }

procedure TVRMLOpenGLRenderer.Box_Render(const SizeX, SizeY, SizeZ: Single);
var MatPerPart: boolean;

  procedure PartMat(MatNum: integer);
  begin
   if MatPerPart then Render_BindMaterial_1(MatNum);
  end;

{$define DetailX:=(Render_State.LastNodes.KambiTriangulation.RectDivisions)}
{$define DetailY:=(Render_State.LastNodes.KambiTriangulation.RectDivisions)}
{$define DetailZ:=(Render_State.LastNodes.KambiTriangulation.RectDivisions)}

var x1, y1, z1, x2, y2, z2: TGLfloat;
begin
 MatPerPart := Render_State.LastNodes.MaterialBinding.FdValue.Value in [
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

procedure TVRMLOpenGLRenderer.RenderCube_1(Node: TNodeCube_1);
begin
  Box_Render(Node.FdWidth.Value, Node.FdHeight.Value, Node.FdDepth.Value);
end;

procedure TVRMLOpenGLRenderer.RenderBox(Node: TNodeBox);
begin
  Box_Render(Node.FdSize.Value[0], Node.FdSize.Value[1], Node.FdSize.Value[2]);
end;

{ Cylinder ------------------------------------------------------------------ }

procedure TVRMLOpenGLRenderer.Cylinder_Render(
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
   if Render_State.LastNodes.MaterialBinding.FdValue.Value in
       [BIND_PER_PART, BIND_PER_PART_INDEXED] then
    Render_BindMaterial_1(2);
   RenderBottomDisk(q, Radius);
  end;

  if Top then
  begin
   if Render_State.LastNodes.MaterialBinding.FdValue.Value in
       [BIND_PER_PART, BIND_PER_PART_INDEXED] then
    Render_BindMaterial_1(1);
   glTranslatef(0, 0, Height);
   gluDisk(q, 0, Radius, QUADRIC_SLICES, 1);
  end;

  glPopMatrix;
 finally gluDeleteQuadric(q) end;
end;

procedure TVRMLOpenGLRenderer.RenderCylinder_1(Node: TNodeCylinder_1);
begin
  Cylinder_Render(
    Node.FdHeight.Value,
    Node.FdRadius.Value,
    Node.FdParts.Flags[CYLINDER_PARTS_BOTTOM],
    Node.FdParts.Flags[CYLINDER_PARTS_SIDES],
    Node.FdParts.Flags[CYLINDER_PARTS_TOP]);
end;

procedure TVRMLOpenGLRenderer.RenderCylinder_2(Node: TNodeCylinder_2);
begin
  Cylinder_Render(
    Node.FdHeight.Value,
    Node.FdRadius.Value,
    Node.FdBottom.Value,
    Node.FdSide.Value,
    Node.FdTop.Value);
end;

{ Sphere ---------------------------------------------------------------------- }

procedure TVRMLOpenGLRenderer.Sphere_Render(const Radius: Single);
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

procedure TVRMLOpenGLRenderer.RenderSphere_1(Node: TNodeSphere_1);
begin
  Sphere_Render(Node.FdRadius.Value);
end;

procedure TVRMLOpenGLRenderer.RenderSphere_2(Node: TNodeSphere_2);
begin
  Sphere_Render(Node.FdRadius.Value);
end;

{$undef QUADRIC_SLICES}
{$undef QUADRIC_STACKS}
{$undef RECT_DIVISIONS}

{$endif read_implementation}
