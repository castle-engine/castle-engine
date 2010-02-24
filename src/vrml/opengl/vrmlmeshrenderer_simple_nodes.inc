{
  Copyright 2003-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{
  TODO:
  - Should render all vertexes by DoGLVertex or DoGLArrayElement,
    (so using GLU utils is not possible, or DrawGLPlane...,
    or text display lists... this will be difficult to implement
    without wasting resources)

  - We set texture coords only for texture unit 0 here.
    So using multi-texturing with these nodes will produce bad results:
    although many textures will be enabled, actually only the 0th will
    have predictable texture coords.
}

type
  { Used for primitives that should honour automatic 3D texture coordinate
    generation. (see X3D spec
    "33.2.4 Texture coordinate generation for primitive objects").

    This will setup appropriate texture coord gen in Render
    that takes care of Renderer.Primitives3DTextureCoords = @true
    situation.

    You should override DoRenderPrimitive (not DoRender!),
    and generate your own 2D texture coords only if
    PrimitiveMake2DTextureCoords (this checks "not Renderer.Primitives3DTextureCoords",
    also checks Renderer.TexCoordsNeeded). }
  TPrimitiveRenderer = class(TVRMLMeshRenderer)
  protected
    procedure DoRender; override;
    procedure DoRenderPrimitive; virtual; abstract;

    function PrimitiveMake2DTextureCoords: boolean;
  end;

  TAbstractTextRenderer = class(TVRMLMeshRenderer)
  protected
    procedure RenderText(
      const Size, Spacing: Single;
      const Justify: TVRMLFontJustify;
      const Strings: TDynStringArray;
      CurrentFont: TGLOutlineFont;
      const Depth: Single;
      const MarkFrontFaces: boolean;
      const EventLineBounds, EventOrigin, EventTextBounds: TVRMLEvent);
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
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer);
  end;

  TAbstractBoxRenderer = class(TPrimitiveRenderer)
  protected
    procedure RenderBox(const SizeX, SizeY, SizeZ: Single);
  end;

  TCube_1Renderer = class(TAbstractBoxRenderer)
  protected
    procedure DoRenderPrimitive; override;
  end;

  TBoxRenderer = class(TAbstractBoxRenderer)
  protected
    procedure DoRenderPrimitive; override;
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer);
  end;

  { Common class for quadric rendering, with some helper methods/properties. }
  TQuadricRenderer = class(TPrimitiveRenderer)
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
    procedure DoRenderPrimitive; override;
  end;

  TCone_2Renderer = class(TConeRenderer)
  protected
    procedure DoRenderPrimitive; override;
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer);
  end;

  TCylinderRenderer = class(TQuadricRenderer)
  protected
    procedure RenderCylinder(
      const Height, Radius: Single;
      const Bottom, Side, Top: boolean);
  end;

  TCylinder_1Renderer = class(TCylinderRenderer)
  protected
    procedure DoRenderPrimitive; override;
  end;

  TCylinder_2Renderer = class(TCylinderRenderer)
  protected
    procedure DoRenderPrimitive; override;
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer);
  end;

  TSphereRenderer = class(TQuadricRenderer)
  protected
    procedure RenderSphere(
      const Radius: Single);
  end;

  TSphere_1Renderer = class(TSphereRenderer)
  protected
    procedure DoRenderPrimitive; override;
  end;

  TSphere_2Renderer = class(TSphereRenderer)
  protected
    procedure DoRenderPrimitive; override;
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer);
  end;

{ TPrimitiveRenderer --------------------------------------------------------- }

procedure TPrimitiveRenderer.DoRender;

  procedure BeginPrimitives3DTextureCoords;
  var
    Box: TBox3D;
    XStart, YStart, ZStart, XSize, YSize, ZSize: Single;
  begin
    Box := Shape.LocalBoundingBox;

    { If 3D texture coordinates are wanted, it's easiest to generate them
      with glTexGen.

      Texture S should range from 0..1 when X changes from X1 .. X2.
      So S = X / (X2 - X1) - X1 / (X2 - X1).
      Same for T.
      For R, X3D spec says that coords go backwards, so just SwapValues.
    }

    SwapValues(Box[0][2], Box[1][2]);

    XStart := Box[0][0];
    YStart := Box[0][1];
    ZStart := Box[0][2];

    XSize := Box[1][0] - Box[0][0];
    YSize := Box[1][1] - Box[0][1];
    ZSize := Box[1][2] - Box[0][2];

    glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
    glTexGenv(GL_S, GL_OBJECT_PLANE, Vector4Single(1 / XSize, 0, 0, - XStart / XSize));
    glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
    glTexGenv(GL_T, GL_OBJECT_PLANE, Vector4Single(0, 1 / YSize, 0, - YStart / YSize));
    glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
    glTexGenv(GL_R, GL_OBJECT_PLANE, Vector4Single(0, 0, 1 / ZSize, - ZStart / ZSize));
    glEnable(GL_TEXTURE_GEN_S);
    glEnable(GL_TEXTURE_GEN_T);
    glEnable(GL_TEXTURE_GEN_R);
  end;

  procedure EndPrimitives3DTextureCoords;
  begin
    glDisable(GL_TEXTURE_GEN_S);
    glDisable(GL_TEXTURE_GEN_T);
    glDisable(GL_TEXTURE_GEN_R);
  end;

begin
  if Renderer.Primitives3DTextureCoords then
    BeginPrimitives3DTextureCoords;

  DoRenderPrimitive;

  if Renderer.Primitives3DTextureCoords then
    EndPrimitives3DTextureCoords;
end;

function TPrimitiveRenderer.PrimitiveMake2DTextureCoords: boolean;
begin
  Result :=
    (Renderer.TexCoordsNeeded > 0) and
    (not Renderer.Primitives3DTextureCoords);
end;

{ Text ----------------------------------------------------------------------- }

procedure TAbstractTextRenderer.RenderText(
  const Size, Spacing: Single;
  const Justify: TVRMLFontJustify;
  const Strings: TDynStringArray;
  CurrentFont: TGLOutlineFont;
  const Depth: Single;
  const MarkFrontFaces: boolean;
  const EventLineBounds, EventOrigin, EventTextBounds: TVRMLEvent);

var
  TextWidths: TDynSingleArray;

  { StringPos* ignore Size, just assume that CurrentFont has right size. }
  function StringPosX(I: Integer): TGLfloat;
  begin
    case Justify of
      fjBegin : Result := 0;
      fjMiddle: Result := - TextWidths.Items[I] / 2;
      fjEnd   : Result := - TextWidths.Items[I];
      else raise EInternalError.Create('Invalid font justify value');
    end;
  end;

  function StringPosY(I: Integer): TGLfloat;
  begin
    Result := - I * CurrentFont.RowHeight * Spacing;
  end;

var
  YScale, XScale: TGLfloat;
  MaxTextWidth: Single;

  procedure SendEvents;
  var
    LineBounds: TMFVec2f;
    Time: TVRMLTime;
    FinalRowHeight: Single;
    I: Integer;
  begin
    if (EventOrigin <> nil) and
       (Geometry.EventsProcessor <> nil) then
    begin
      { This is all simple, since we ignore topToBottom, leftToRight.
        Also, we don't honour the rule that the upper line of the text
        is exactly on Y = 0 (instead, our *row* (which is slightly higher
        than the upper text line) is on Y = 0, this is documented
        on [http://vrmlengine.sourceforge.net/vrml_implementation_status.php]
        by failed NIST test about it.

        So the lacks of current Text rendering implementation
        make this somewhat simple :) }

      Time := Geometry.EventsProcessor.GetTime;

      FinalRowHeight := CurrentFont.RowHeight * Spacing * YScale;

      case Justify of
        fjBegin : EventOrigin.Send(Vector3Single(0                         , -FinalRowHeight, 0), Time);
        fjMiddle: EventOrigin.Send(Vector3Single(-MaxTextWidth * XScale / 2, -FinalRowHeight, 0), Time);
        fjEnd   : EventOrigin.Send(Vector3Single(-MaxTextWidth * XScale    , -FinalRowHeight, 0), Time);
        else raise EInternalError.Create('Invalid font justify value');
      end;

      if EventLineBounds.SendNeeded then
      begin
        LineBounds := TMFVec2f.CreateUndefined(Geometry, EventLineBounds.Name);
        try
          LineBounds.Items.Count := TextWidths.Count;

          case Justify of
            fjBegin :
              for I := 0 to TextWidths.Count - 1 do
                LineBounds.Items.Items[I] := Vector2Single(
                  TextWidths[I] * XScale, FinalRowHeight);
            fjMiddle:
              for I := 0 to TextWidths.Count - 1 do
                LineBounds.Items.Items[I] := Vector2Single(
                  (MaxTextWidth + TextWidths[I]) * XScale / 2, FinalRowHeight);
            fjEnd   :
              for I := 0 to TextWidths.Count - 1 do
                LineBounds.Items.Items[I] := Vector2Single(
                  MaxTextWidth * XScale, FinalRowHeight);
            else raise EInternalError.Create('Invalid font justify value');
          end;

          EventLineBounds.Send(LineBounds, Time);
        finally FreeAndNil(LineBounds) end;
      end;

      EventTextBounds.Send(Vector2Single(
        MaxTextWidth * XScale, FinalRowHeight * Strings.Count), Time);
    end;
  end;

var
  I: Integer;
begin
  YScale := Size / CurrentFont.RowHeight;
  { TODO: Use maxEntent, length for VRML 2.0.
    Use width for VRML 1.0. }
  XScale := YScale;

  TextWidths := TDynSingleArray.Create(Strings.Count);
  try
    MaxTextWidth := 0;
    for I := 0 to TextWidths.Count - 1 do
    begin
      TextWidths.Items[I] := CurrentFont.TextWidth(Strings[I]);
      MaxTo1st(MaxTextWidth, TextWidths.Items[I]);
    end;

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
          if Renderer.TexCoordsNeeded > 0 then
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
            if Renderer.TexCoordsNeeded > 0 then
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
              if Renderer.TexCoordsNeeded > 0 then
                CurrentFont.PrintTexturedExtrusionAndMove(Strings[i], Depth, 0, 0) else
                CurrentFont.PrintExtrusionAndMove(Strings[i], Depth);
            glPopMatrix;
          end;

        glPopAttrib;
      end;

    glPopMatrix;

    { These events should be generated only when
      the default values of length and maxExtent are used.
      For now, we ignore length and maxExtent, so these events are
      simply always generated. }
    SendEvents;

  finally FreeAndNil(TextWidths) end;
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
      State.LastNodes.FontStyle.Italic].Instance, 0, false,
    nil, nil, nil);
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

  { TODO: this also has "solid" field, use! }

  RenderText(Size, Spacing, Justify, Node.FdString.Items, CurrentFont,
    0, false,
    Node.EventLineBounds, Node.EventOrigin, Node.EventTextBounds);
end;

constructor TText3DRenderer.Create(ARenderer: TVRMLOpenGLRenderer);
var
  Node: TNodeText3D;
begin
  inherited;

  Node := Geometry as TNodeText3D;

  CullBackFaces := Node.FdSolid.Value;
  { We don't set here FrontFaceCcw, it doesn't matter, we will
    set glCullFace ourselves anyway. }
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

  RenderText(Size, Spacing, Justify, Node.FdString.Items, CurrentFont,
    Node.FdDepth.Value, Node.FdSolid.Value,
    nil, nil, nil);
end;

{ Box ------------------------------------------------------------------------ }

procedure TAbstractBoxRenderer.RenderBox(const SizeX, SizeY, SizeZ: Single);
var MatPerPart: boolean;

  procedure PartMat(MatNum: integer);
  begin
   if MatPerPart then Renderer.Render_BindMaterial_1(MatNum);
  end;

var
  x1, y1, z1, x2, y2, z2: TGLfloat;
  DetailX, DetailY, DetailZ: Cardinal;
begin
 { Currently, all three Detail* used are always equal.
   Although the code is written to support different Detail* values,
   just in case in the future it will be useful. }
 DetailX := State.LastNodes.KambiTriangulation.RectDivisions;
 DetailY := DetailX;
 DetailZ := DetailX;

 MatPerPart := State.LastNodes.MaterialBinding.FdValue.Value in [
   BIND_PER_PART, BIND_PER_PART_INDEXED, BIND_PER_FACE, BIND_PER_FACE_INDEXED];
 if not MatPerPart then
  Renderer.Render_BindMaterial_1(0); {use first material for the whole cube}

 x2 := SizeX / 2; x1 := -x2;
 y2 := SizeY / 2; y1 := -y2;
 z2 := SizeZ / 2; z1 := -z2;

 {Do narysowania Cube swietnie nadawalaby sie procedura KambiGLUtils.drawGLBox -
  tyle ze ona nie pozwala mi zrobic Renderer.Render_BindMaterial_1 przed kazda sciana i
  nie gwarantuje jak beda nakladane wspolrzedne tekstury.
  To co jest ponizej to po prostu implementacja drawGLBox z tymi dodatkami.}

 { Thanks to using ConstCoordGivesNormal1 parameter, we generate correct
   normals pointing out from CCW. Also, they point outside, so we can
   do back-face culling with "solid" X3D field. }

 PartMat(2);
 DrawGLPlaneSpecialTex(y1, z1, y2, z2, x1, 0, DetailY, DetailZ, false, PrimitiveMake2DTextureCoords, 0, 0, 1, 1, false);
 PartMat(3);
 DrawGLPlaneSpecialTex(y1, z1, y2, z2, x2, 0, DetailY, DetailZ, true,  PrimitiveMake2DTextureCoords, 0, 1, 1, 0, false);

 PartMat(5);
 DrawGLPlaneSpecialTex(x1, z1, x2, z2, y1, 1, DetailX, DetailZ, false, PrimitiveMake2DTextureCoords, 0, 0, 1, 1, true );
 PartMat(4);
 DrawGLPlaneSpecialTex(x1, z1, x2, z2, y2, 1, DetailX, DetailZ, true,  PrimitiveMake2DTextureCoords, 0, 1, 1, 0, true );

 PartMat(1);
 DrawGLPlaneSpecialTex(x1, y1, x2, y2, z1, 2, DetailX, DetailY, false, PrimitiveMake2DTextureCoords, 1, 0, 0, 1, true );
 PartMat(0);
 DrawGLPlaneSpecialTex(x1, y1, x2, y2, z2, 2, DetailX, DetailY, true,  PrimitiveMake2DTextureCoords, 0, 0, 1, 1, true );
end;

procedure TCube_1Renderer.DoRenderPrimitive;
var
  Node: TNodeCube_1;
begin
  Node := Geometry as TNodeCube_1;
  RenderBox(Node.FdWidth.Value, Node.FdHeight.Value, Node.FdDepth.Value);
end;

constructor TBoxRenderer.Create(ARenderer: TVRMLOpenGLRenderer);
var
  Node: TNodeBox;
begin
  inherited;

  Node := Geometry as TNodeBox;

  CullBackFaces := Node.FdSolid.Value;
  FrontFaceCcw := true;
end;

procedure TBoxRenderer.DoRenderPrimitive;
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
  { The rotation is needed
    - for proper texture coordinates (these are actually only needed
      when PrimitiveMake2DTextureCoords)
    - also, so that "solid" field makes outside visible, inside invisible.
      (only when "solid" = @true).

    So we could avoid this glPushMatrix and glRotate in special cases,
    but it's not worth the trouble. (and in 99% cases solid = TRUE).
  }

  glPushMatrix;
    glRotatef(180, 1, 0, 0);
    gluDisk(q, 0, radius, QuadricSlices, 1);
  glPopMatrix;
end;

{ For all quadrics with "solid" field:

  OpenGL quadrics documentation (e.g.
  [http://glprogramming.com/red/chapter11.html]) says that
  - the polygons are drawn in a counterclockwise fashion with
    respect to their normals
  - orientation is either GLU_OUTSIDE (the default) or GLU_INSIDE,
    which controls the direction in which normals are pointing.
    (we always use GLU_OUTSIDE).

  This means that to implement "solid" field, we just set
  CullBackFaces and FrontFaceCcw to true, and everything will work Ok!
}

{ Cone ----------------------------------------------------------------------- }

procedure TConeRenderer.RenderCone(
  const BottomRadius, Height: Single;
  const Side, Bottom: boolean);
var q: PGLUQuadric;
begin
 {cone uses its own normals and we apply texture according to VRML specification.
  Cone uses only material 0 or materials 0 and 1.}

 Renderer.Render_BindMaterial_1(0);

 q := NewGLUQuadric(PrimitiveMake2DTextureCoords,
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

procedure TCone_1Renderer.DoRenderPrimitive;
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

constructor TCone_2Renderer.Create(ARenderer: TVRMLOpenGLRenderer);
var
  Node: TNodeCone_2;
begin
  inherited;

  Node := Geometry as TNodeCone_2;

  CullBackFaces := Node.FdSolid.Value;
  FrontFaceCcw := true;
end;

procedure TCone_2Renderer.DoRenderPrimitive;
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

 q := NewGLUQuadric(PrimitiveMake2DTextureCoords,
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

procedure TCylinder_1Renderer.DoRenderPrimitive;
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

constructor TCylinder_2Renderer.Create(ARenderer: TVRMLOpenGLRenderer);
var
  Node: TNodeCylinder_2;
begin
  inherited;

  Node := Geometry as TNodeCylinder_2;

  CullBackFaces := Node.FdSolid.Value;
  FrontFaceCcw := true;
end;

procedure TCylinder_2Renderer.DoRenderPrimitive;
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

 if PrimitiveMake2DTextureCoords then
 begin
  {we have to rotate the sphere to get tex coords as specified by VRML}
  glPushMatrix;
  glRotatef(-90, 1, 0, 0);
 end;

 q := NewGLUQuadric(PrimitiveMake2DTextureCoords,
   Attributes.SmoothNormalsGLU, GLU_OUTSIDE, GLU_FILL);
 try
  gluSphere(q, Radius, QuadricSlices, QuadricStacks);
 finally gluDeleteQuadric(q) end;

 if PrimitiveMake2DTextureCoords then glPopMatrix;
end;

procedure TSphere_1Renderer.DoRenderPrimitive;
var
  Node: TNodeSphere_1;
begin
  Node := Geometry as TNodeSphere_1;
  RenderSphere(Node.FdRadius.Value);
end;

constructor TSphere_2Renderer.Create(ARenderer: TVRMLOpenGLRenderer);
var
  Node: TNodeSphere_2;
begin
  inherited;

  Node := Geometry as TNodeSphere_2;

  CullBackFaces := Node.FdSolid.Value;
  FrontFaceCcw := true;
end;

procedure TSphere_2Renderer.DoRenderPrimitive;
var
  Node: TNodeSphere_2;
begin
  Node := Geometry as TNodeSphere_2;
  RenderSphere(Node.FdRadius.Value);
end;
