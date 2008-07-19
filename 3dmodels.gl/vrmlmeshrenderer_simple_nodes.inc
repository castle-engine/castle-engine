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

{
  TODO:
  - should render all vertexes by DoGLVertex or DoGLArrayElement,
    (so using GLU utils is not possible, or DrawGLPlane...,
    or text display lists... this will be difficult to implement
    without wasting resources)
  - should use CullBackFaces mechanism from TVRMLMeshRenderer
    to cull back faces.
}

type
  TAbstractTextRenderer = class(TVRMLMeshRenderer)
  protected
    procedure RenderText(
      const Size, Spacing: Single;
      const Justify: TVRMLFontJustify;
      const Strings: TDynStringArray;
      CurrentFont: TGLOutlineFont;
      const Depth: Single;
      const MarkFrontFaces: boolean);
  end;

  TAsciiTextRenderer = class(TAbstractTextRenderer)
  protected
    procedure DoRender; override;
  end;

  TTextRenderer = class(TAbstractTextRenderer)
  protected
    procedure DoRender; override;
  end;

  TText3DRenderer = class(TAbstractTextRenderer)
  protected
    procedure DoRender; override;
  end;

  TAbstractBoxRenderer = class(TVRMLMeshRenderer)
  protected
    procedure RenderBox(const SizeX, SizeY, SizeZ: Single);
  end;

  TCube_1Renderer = class(TAbstractBoxRenderer)
  protected
    procedure DoRender; override;
  end;

  TBoxRenderer = class(TAbstractBoxRenderer)
  protected
    procedure DoRender; override;
  end;

  { Common class for quadric rendering, with some helper methods/properties. }
  TQuadricRenderer = class(TVRMLMeshRenderer)
  private
    FQuadricSlices, FQuadricStacks: Cardinal;
  protected
    { Render GLU disk using quadric Q. The disk will have texture applied
      in a way appropriate for bottom disk of VRML's cylinder and cone. }
    procedure RenderBottomDisk(q: PGLuQuadric; radius: TGLdouble);
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer);
    property QuadricSlices: Cardinal read FQuadricSlices;
    property QuadricStacks: Cardinal read FQuadricStacks;
  end;

  TConeRenderer = class(TQuadricRenderer)
  protected
    procedure RenderCone(
      const BottomRadius, Height: Single;
      const Side, Bottom: boolean);
  end;

  TCone_1Renderer = class(TConeRenderer)
  protected
    procedure DoRender; override;
  end;

  TCone_2Renderer = class(TConeRenderer)
  protected
    procedure DoRender; override;
  end;

  TCylinderRenderer = class(TQuadricRenderer)
  protected
    procedure RenderCylinder(
      const Height, Radius: Single;
      const Bottom, Side, Top: boolean);
  end;

  TCylinder_1Renderer = class(TCylinderRenderer)
  protected
    procedure DoRender; override;
  end;

  TCylinder_2Renderer = class(TCylinderRenderer)
  protected
    procedure DoRender; override;
  end;

  TSphereRenderer = class(TQuadricRenderer)
  protected
    procedure RenderSphere(
      const Radius: Single);
  end;

  TSphere_1Renderer = class(TSphereRenderer)
  protected
    procedure DoRender; override;
  end;

  TSphere_2Renderer = class(TSphereRenderer)
  protected
    procedure DoRender; override;
  end;

{ Text ----------------------------------------------------------------------- }

procedure TAbstractTextRenderer.RenderText(
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

  Renderer.Render_BindMaterial_1(0);

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
        if Renderer.Render_TexCoordsNeeded then
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
          if Renderer.Render_TexCoordsNeeded then
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
            if Renderer.Render_TexCoordsNeeded then
              CurrentFont.PrintTexturedExtrusionAndMove(Strings[i], Depth, 0, 0) else
              CurrentFont.PrintExtrusionAndMove(Strings[i], Depth);
          glPopMatrix;
        end;

      glPopAttrib;
    end;

  glPopMatrix;
end;

procedure TAsciiTextRenderer.DoRender;
var
  Node: TNodeAsciiText_1;
begin
  Node := Geometry as TNodeAsciiText_1;

  RenderText(
    State.LastNodes.FontStyle.FdSize.Value,
    Node.FdSpacing.Value,
    Node.Justify,
    Node.FdString.Items,
    Renderer.Cache.Fonts[
      State.LastNodes.FontStyle.Family,
      State.LastNodes.FontStyle.Bold,
      State.LastNodes.FontStyle.Italic].Instance, 0, false);
end;

procedure TTextRenderer.DoRender;
var
  Node: TNodeText;
  Size, Spacing: Single;
  Justify: TVRMLFontJustify;
  CurrentFont: TGLOutlineFont;
begin
  Node := Geometry as TNodeText;

  if Node.FontStyle = nil then
  begin
    Size := TNodeFontStyle_2.DefaultSize;
    Spacing := TNodeFontStyle_2.DefaultSpacing;
    Justify := TNodeFontStyle_2.DefaultJustify;
    CurrentFont := Renderer.Cache.Fonts[
      TNodeFontStyle_2.DefaultFamily,
      TNodeFontStyle_2.DefaultBold,
      TNodeFontStyle_2.DefaultItalic].Instance;
  end else
  begin
    Size := Node.FontStyle.FdSize.Value;
    Spacing := Node.FontStyle.FdSpacing.Value;
    Justify := Node.FontStyle.Justify;
    CurrentFont := Renderer.Cache.Fonts[
      Node.FontStyle.Family,
      Node.FontStyle.Bold,
      Node.FontStyle.Italic].Instance;
  end;

  RenderText(Size, Spacing, Justify, Node.FdString.Items, CurrentFont, 0, false);
end;

procedure TText3DRenderer.DoRender;
var
  Size, Spacing: Single;
  Justify: TVRMLFontJustify;
  CurrentFont: TGLOutlineFont;
  Node: TNodeText3D;
begin
  Node := Geometry as TNodeText3D;

  if Node.FontStyle = nil then
  begin
    Size := TNodeFontStyle_2.DefaultSize;
    Spacing := TNodeFontStyle_2.DefaultSpacing;
    Justify := TNodeFontStyle_2.DefaultJustify;
    CurrentFont := Renderer.Cache.Fonts[
      TNodeFontStyle_2.DefaultFamily,
      TNodeFontStyle_2.DefaultBold,
      TNodeFontStyle_2.DefaultItalic].Instance;
  end else
  begin
    Size := Node.FontStyle.FdSize.Value;
    Spacing := Node.FontStyle.FdSpacing.Value;
    Justify := Node.FontStyle.Justify;
    CurrentFont := Renderer.Cache.Fonts[
      Node.FontStyle.Family,
      Node.FontStyle.Bold,
      Node.FontStyle.Italic].Instance;
  end;

  if Node.FdSolid.Value then
  begin
    glPushAttrib(GL_POLYGON_BIT);
    glEnable(GL_CULL_FACE);
  end;

  RenderText(Size, Spacing, Justify, Node.FdString.Items, CurrentFont,
    Node.FdDepth.Value, Node.FdSolid.Value);

  if Node.FdSolid.Value then
  begin
    glPopAttrib;
  end;
end;

{ Box ------------------------------------------------------------------------ }

procedure TAbstractBoxRenderer.RenderBox(const SizeX, SizeY, SizeZ: Single);
var MatPerPart: boolean;

  procedure PartMat(MatNum: integer);
  begin
   if MatPerPart then Renderer.Render_BindMaterial_1(MatNum);
  end;

{$define DetailX:=(State.LastNodes.KambiTriangulation.RectDivisions)}
{$define DetailY:=(State.LastNodes.KambiTriangulation.RectDivisions)}
{$define DetailZ:=(State.LastNodes.KambiTriangulation.RectDivisions)}

var x1, y1, z1, x2, y2, z2: TGLfloat;
begin
 MatPerPart := State.LastNodes.MaterialBinding.FdValue.Value in [
   BIND_PER_PART, BIND_PER_PART_INDEXED, BIND_PER_FACE, BIND_PER_FACE_INDEXED];
 if not MatPerPart then
  Renderer.Render_BindMaterial_1(0); {use first material fot the whole cube}

 x2 := SizeX / 2; x1 := -x2;
 y2 := SizeY / 2; y1 := -y2;
 z2 := SizeZ / 2; z1 := -z2;

 {Do narysowania Cube swietnie nadawalaby sie procedura KambiGLUtils.drawGLBox -
  tyle ze ona nie pozwala mi zrobic Renderer.Render_BindMaterial_1 przed kazda sciana i
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

procedure TCube_1Renderer.DoRender;
var
  Node: TNodeCube_1;
begin
  Node := Geometry as TNodeCube_1;
  RenderBox(Node.FdWidth.Value, Node.FdHeight.Value, Node.FdDepth.Value);
end;

procedure TBoxRenderer.DoRender;
var
  Node: TNodeBox;
begin
  Node := Geometry as TNodeBox;
  RenderBox(Node.FdSize.Value[0], Node.FdSize.Value[1], Node.FdSize.Value[2]);
end;

{ TQuadricRenderer ----------------------------------------------------------- }

constructor TQuadricRenderer.Create(ARenderer: TVRMLOpenGLRenderer);
begin
  inherited;
  FQuadricSlices := State.LastNodes.KambiTriangulation.QuadricSlices;
  FQuadricStacks := State.LastNodes.KambiTriangulation.QuadricStacks;
end;

procedure TQuadricRenderer.RenderBottomDisk(
  q: PGLuQuadric; radius: TGLdouble);
begin
  if Renderer.Render_TexCoordsNeeded then
  begin
    glPushMatrix;
    glRotatef(180, 1, 0, 0);
  end;
  gluDisk(q, 0, radius, QuadricSlices, 1);
  if Renderer.Render_TexCoordsNeeded then glPopMatrix;
end;

{ Cone ----------------------------------------------------------------------- }

procedure TConeRenderer.RenderCone(
  const BottomRadius, Height: Single;
  const Side, Bottom: boolean);
var q: PGLUQuadric;
begin
 {cone uses its own normals and we apply texture according to VRML specification.
  Cone uses only material 0 or materials 0 and 1.}

 Renderer.Render_BindMaterial_1(0);

 q := NewGLUQuadric(Ord(Renderer.Render_TexCoordsNeeded),
   Attributes.SmoothNormalsGLU, GLU_OUTSIDE, GLU_FILL);
 try
  glPushMatrix;
  glRotatef(-90, 1, 0, 0);
  glTranslatef(0, 0, -Height/2);
  if Side then
   gluCylinder(q, BottomRadius, 0, Height, QuadricSlices, QuadricStacks);
  if Bottom then
  begin
   if State.LastNodes.MaterialBinding.FdValue.Value in
     [BIND_PER_PART, BIND_PER_PART_INDEXED] then
    Renderer.Render_BindMaterial_1(1);
   RenderBottomDisk(q, BottomRadius);
  end;
  glPopMatrix;
 finally gluDeleteQuadric(q) end;
end;

procedure TCone_1Renderer.DoRender;
var
  Node: TNodeCone_1;
begin
  Node := Geometry as TNodeCone_1;
  RenderCone(
    Node.FdBottomRadius.Value,
    Node.FdHeight.Value,
    Node.FdParts.Flags[CONE_PARTS_SIDES],
    Node.FdParts.Flags[CONE_PARTS_BOTTOM]);
end;

procedure TCone_2Renderer.DoRender;
var
  Node: TNodeCone_2;
begin
  Node := Geometry as TNodeCone_2;
  RenderCone(
    Node.FdBottomRadius.Value,
    Node.FdHeight.Value,
    Node.FdSide.Value,
    Node.FdBottom.Value);
end;

{ Cylinder ------------------------------------------------------------------- }

procedure TCylinderRenderer.RenderCylinder(
  const Height, Radius: Single;
  const Bottom, Side, Top: boolean);
var q: PGLUQuadric;
begin
 {cylinder uses its own normals and we apply texture according to VRML
  specification. Cylinder uses only material 0 or materials 0, 1, 2}

 Renderer.Render_BindMaterial_1(0);

 q := NewGLUQuadric(Ord(Renderer.Render_TexCoordsNeeded),
   Attributes.SmoothNormalsGLU, GLU_OUTSIDE, GLU_FILL);
 try
  glPushMatrix;
  glRotatef(-90, 1, 0, 0);
  glTranslatef(0, 0, -Height/2);
  if Side then
   gluCylinder(q, Radius, Radius, Height,
     QuadricSlices, QuadricStacks);

  if Bottom then
  begin
   if State.LastNodes.MaterialBinding.FdValue.Value in
       [BIND_PER_PART, BIND_PER_PART_INDEXED] then
    Renderer.Render_BindMaterial_1(2);
   RenderBottomDisk(q, Radius);
  end;

  if Top then
  begin
   if State.LastNodes.MaterialBinding.FdValue.Value in
       [BIND_PER_PART, BIND_PER_PART_INDEXED] then
    Renderer.Render_BindMaterial_1(1);
   glTranslatef(0, 0, Height);
   gluDisk(q, 0, Radius, QuadricSlices, 1);
  end;

  glPopMatrix;
 finally gluDeleteQuadric(q) end;
end;

procedure TCylinder_1Renderer.DoRender;
var
  Node: TNodeCylinder_1;
begin
  Node := Geometry as TNodeCylinder_1;
  RenderCylinder(
    Node.FdHeight.Value,
    Node.FdRadius.Value,
    Node.FdParts.Flags[CYLINDER_PARTS_BOTTOM],
    Node.FdParts.Flags[CYLINDER_PARTS_SIDES],
    Node.FdParts.Flags[CYLINDER_PARTS_TOP]);
end;

procedure TCylinder_2Renderer.DoRender;
var
  Node: TNodeCylinder_2;
begin
  Node := Geometry as TNodeCylinder_2;
  RenderCylinder(
    Node.FdHeight.Value,
    Node.FdRadius.Value,
    Node.FdBottom.Value,
    Node.FdSide.Value,
    Node.FdTop.Value);
end;

{ Sphere --------------------------------------------------------------------- }

procedure TSphereRenderer.RenderSphere(const Radius: Single);
var q: PGLUQuadric;
begin
 {whole sphere uses materail num 0 and his own normals and we apply
  texture according to VRML specification}

 Renderer.Render_BindMaterial_1(0);

 if Renderer.Render_TexCoordsNeeded then
 begin
  {we have to rotate the sphere to get tex coords as specified by VRML}
  glPushMatrix;
  glRotatef(-90, 1, 0, 0);
 end;

 q := NewGLUQuadric(Ord(Renderer.Render_TexCoordsNeeded),
   Attributes.SmoothNormalsGLU, GLU_OUTSIDE, GLU_FILL);
 try
  gluSphere(q, Radius, QuadricSlices, QuadricStacks);
 finally gluDeleteQuadric(q) end;

 if Renderer.Render_TexCoordsNeeded then glPopMatrix;
end;

procedure TSphere_1Renderer.DoRender;
var
  Node: TNodeSphere_1;
begin
  Node := Geometry as TNodeSphere_1;
  RenderSphere(Node.FdRadius.Value);
end;

procedure TSphere_2Renderer.DoRender;
var
  Node: TNodeSphere_2;
begin
  Node := Geometry as TNodeSphere_2;
  RenderSphere(Node.FdRadius.Value);
end;
