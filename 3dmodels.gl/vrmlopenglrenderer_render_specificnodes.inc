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

  - Aktualny bound material jest niezdefiniowany na wejsciu,
    wiec zawsze powinno byc przynajmniej jedno wywolanie Render_BindMaterial.
    Nie wolno tutaj wywolywac glMaterial lub glColor - tym zajmie sie
    Render_BindMaterial.

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

{ AsciiText -------------------------------------------------------------------- }

procedure RenderAsciiText(Node: TNodeAsciiText_1);

  { StringPos* ignore last FontStyle Size, just assume that
    CurrentFont has right size.
  }

  function StringPosX(i: integer): TGLfloat;
  begin
   case Node.FdJustification.Value of
    JUSTIFICATION_LEFT:
      Result := 0;
    JUSTIFICATION_CENTER:
      Result := -CurrentFont.TextWidth(Node.FdString.Items.Items[i])/2;
    JUSTIFICATION_RIGHT:
      Result := -CurrentFont.TextWidth(Node.FdString.Items.Items[i]);
    else raise EInternalError.Create('Invalid FdJustification.Value');
   end;
  end;

  function StringPosY(i: integer): TGLfloat;
  begin result := -i*CurrentFont.RowHeight* Node.FdSpacing.Value end;

var yscale: TGLfloat;
    i: integer;
begin
 yscale := State.LastNodes.FontStyle.FdSize.Value / CurrentFont.RowHeight;

 Render_BindMaterial(0);

 glNormal3f(0, 0, -1);

 glPushMatrix;
 glScalef(1, yscale, 1);
 for i := 0 to Node.FdString.Count-1 do
 begin
  glPushMatrix;
    glScalef(yscale{TODO:use here FdWidth}, 1, 1);

    { TODO: when using justification <> LEFT I should also
      change texOriginX here }
    glTranslatef(StringPosX(i), StringPosY(i), 0);

    { uzywamy ponizej print*AndMove bo przeciez za chwile i tak zrobimy
      popMatrix (wiec po co marnowac jedno miejsce na matrix stack ?) }
    if Render_TexCoordsNeeded then
     CurrentFont.PrintTexturedAndMove(Node.FdString.Items.Items[i], 0, 0) else
     CurrentFont.PrintAndMove(Node.FdString.Items.Items[i]);
  glPopMatrix;
 end;
 glPopMatrix;
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

procedure RenderCone(Node: TNodeCone_1);
var q: PGLUQuadric;
begin
 {cone uses its own normals and we apply texture according to VRML specification.
  Cone uses only material 0 or materials 0 and 1.}

 Render_BindMaterial(0);

 q := NewGLUQuadric(Ord(Render_TexCoordsNeeded),
   Attributes.SmoothNormalsGLU, GLU_OUTSIDE, GLU_FILL);
 try
  glPushMatrix;
  glRotatef(-90, 1, 0, 0);
  glTranslatef(0, 0, -Node.FdHeight.Value/2);
  if Node.FdParts.Flags[CONE_PARTS_SIDES] then
   gluCylinder(q, Node.FdBottomRadius.Value, 0, Node.FdHeight.Value, QUADRIC_SLICES, QUADRIC_STACKS);
  if Node.FdParts.Flags[CONE_PARTS_BOTTOM] then
  begin
   if State.LastNodes.MaterialBinding.FdValue.Value in [BIND_PER_PART, BIND_PER_PART_INDEXED] then
    Render_BindMaterial(1);
   RenderBottomDisk(q, Node.FdBottomRadius.Value);
  end;
  glPopMatrix;
 finally gluDeleteQuadric(q) end;
end;

{ Cube ------------------------------------------------------------------------ }

procedure RenderCube(Node: TNodeCube_1);
var MatPerPart: boolean;

  procedure PartMat(MatNum: integer);
  begin
   if MatPerPart then Render_BindMaterial(MatNum);
  end;

{$define DetailX:=(State.LastNodes.KambiTriangulation.RectDivisions)}
{$define DetailY:=(State.LastNodes.KambiTriangulation.RectDivisions)}
{$define DetailZ:=(State.LastNodes.KambiTriangulation.RectDivisions)}

var x1, y1, z1, x2, y2, z2: TGLfloat;
begin
 MatPerPart := State.LastNodes.MaterialBinding.FdValue.Value in [
   BIND_PER_PART, BIND_PER_PART_INDEXED, BIND_PER_FACE, BIND_PER_FACE_INDEXED];
 if not MatPerPart then
  Render_BindMaterial(0); {use first material fot the whole cube}

 x2 := Node.FdWidth.Value/2;  x1 := -x2;
 y2 := Node.FdHeight.Value/2; y1 := -y2;
 z2 := Node.FdDepth.Value/2;  z1 := -z2;

 {Do narysowania Cube swietnie nadawalaby sie procedura KambiGLUtils.drawGLBox -
  tyle ze ona nie pozwala mi zrobic Render_BindMaterial przed kazda sciana i
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

{ Cylinder --------------------------------------------------------------------- }

procedure RenderCylinder(Node: TNodeCylinder_1);
var q: PGLUQuadric;
begin
 {cylinder uses its own normals and we apply texture according to VRML
  specification. Cylinder uses only material 0 or materials 0, 1, 2}

 Render_BindMaterial(0);

 q := NewGLUQuadric(Ord(Render_TexCoordsNeeded),
   Attributes.SmoothNormalsGLU, GLU_OUTSIDE, GLU_FILL);
 try
  glPushMatrix;
  glRotatef(-90, 1, 0, 0);
  glTranslatef(0, 0, -Node.FdHeight.Value/2);
  if Node.FdParts.Flags[CYLINDER_PARTS_SIDES] then
   gluCylinder(q, Node.FdRadius.Value, Node.FdRadius.Value, Node.FdHeight.Value,
     QUADRIC_SLICES, QUADRIC_STACKS);

  if Node.FdParts.Flags[CYLINDER_PARTS_BOTTOM] then
  begin
   if State.LastNodes.MaterialBinding.FdValue.Value in [BIND_PER_PART, BIND_PER_PART_INDEXED] then
    Render_BindMaterial(2);
   RenderBottomDisk(q, Node.FdRadius.Value);
  end;
  if Node.FdParts.Flags[CYLINDER_PARTS_TOP] then
  begin
   if State.LastNodes.MaterialBinding.FdValue.Value in [BIND_PER_PART, BIND_PER_PART_INDEXED] then
    Render_BindMaterial(1);
   glTranslatef(0, 0, Node.FdHeight.Value);
   gluDisk(q, 0, Node.FdRadius.Value, QUADRIC_SLICES, 1);
  end;
  glPopMatrix;
 finally gluDeleteQuadric(q) end;
end;

{ PointSet --------------------------------------------------------------------- }

procedure RenderPointSet(Node: TNodePointSet_1);
var i, startIndex, numPoints: integer;
begin
 { TODO: use here Last :
   Normal, NormalBinding, TextureCoordinate2, Texture2}

 Render_BindMaterial(0);{TODO}

 {TODO: for now, we always draw points unlit}
 glPushAttrib(GL_LIGHTING_BIT);
 glDisable(GL_LIGHTING);

 Node.CalculateRange(State.LastNodes.Coordinate3, startIndex, numPoints);
 glBegin(GL_POINTS);
 for i := startIndex to startIndex+numPoints-1 do
  DoGLVertex(State.LastNodes.Coordinate3.FdPoint.Items.Items[i]);
 glEnd;

 glPopAttrib;
end;

{ Sphere ---------------------------------------------------------------------- }

procedure RenderSphere(const Radius: Single);
var q: PGLUQuadric;
begin
 {whole sphere uses materail num 0 and his own normals and we apply
  texture according to VRML specification}

 Render_BindMaterial(0);

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
  RenderSphere(Node.FdRadius.Value);
end;

procedure RenderSphere_2(Node: TNodeSphere_2);
begin
  RenderSphere(Node.FdRadius.Value);
end;

{$undef QUADRIC_SLICES}
{$undef QUADRIC_STACKS}
{$undef RECT_DIVISIONS}

{ eof ------------------------------------------------------------------------ }
