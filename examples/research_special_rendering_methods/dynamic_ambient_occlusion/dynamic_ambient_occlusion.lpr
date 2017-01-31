{
  Copyright 2009-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Sample implementation of "Dynamic ambient occlusion".
  See README for descriptions and links.

  Run with $1 = 3d model to open.
  Navigate by mouse/keys like in view3dscene (see
  http://castle-engine.sourceforge.net/view3dscene.php).

  Keys s / S scale shadow more / less (for more/less dramatic effect;
  the default scale is anyway 2 times larger than what math suggests,
  to be more dramatic).
}
program dynamic_ambient_occlusion;

{$I castleconf.inc}

uses SysUtils, Classes, Math,
  CastleVectors, CastleGL, CastleWindow, CastleTriangles,
  CastleClassUtils, CastleUtils, CastleKeysMouse,
  CastleGLUtils, CastleSceneCore, CastleScene, Castle3D, CastleParameters,
  CastleFilesUtils, CastleStringUtils, CastleGLShaders, CastleShapes,
  X3DFields, CastleImages, CastleGLImages, CastleMessages, CastleLog,
  CastleGLVersion, CastleSceneManager, CastleRenderingCamera,
  CastleGenericLists, CastleRectangles, CastleApplicationProperties;

type
  TDrawType = (dtNormalGL, dtElements, dtElementsIntensity, dtPass1, dtPass2);

var
  Window: TCastleWindowCustom;

  Scene: TCastleScene;
  GLSLProgram: array [0..1] of TGLSLProgram;
  DrawType: TDrawType = dtPass2;

  { Math correctly, this should be 1 / (2 * Pi)
    (= inverted hemisphere surface area).
    In practice, larger values are useful, to make shadows look more "dramatic". }
  ShadowScale: Single = 1 / (1 * Pi);

{ Storing and calculating "elements" for algorithm --------------------------- }

type
  TAOElement = record
    Area: Single;
    { Position and Normal are in *world* coordinates (not local!) }
    Position, Normal: TVector3Single;
  end;
  PAOElement = ^TAOElement;
  TAOElementList = specialize TGenericStructList<TAOElement>;

var
  Elements: TAOElementList;

type
  TShapeInfo = record
    Shape: TShape;
    CoordToElement: array of Integer;
  end;
  PShapeInfo = ^TShapeInfo;

var
  Shapes: array of TShapeInfo;

type
  TElementsCalculator = class
  public
    CoordIndex: TLongIntList;
    Coord: TVector3SingleList;
    ShapeElements: PAOElement;

    procedure Polygon(const Indexes: array of Cardinal);
  end;

procedure TElementsCalculator.Polygon(
  const Indexes: array of Cardinal);
var
  FaceNormal: TVector3Single;
  { DirectIndexes is LongInt, not Cardinal array, since we cannot
    guarantee that CoordIndex items are >= 0. }
  DirectIndexes: array of LongInt;
  I: Integer;
  FaceArea: Single;
begin
  SetLength(DirectIndexes, Length(Indexes));
  if CoordIndex <> nil then
  begin
    for I := 0 to Length(Indexes) - 1 do
      DirectIndexes[I] := CoordIndex.List^[Indexes[I]];
  end else
  begin
    for I := 0 to Length(Indexes) - 1 do
      DirectIndexes[I] := Indexes[I];
  end;

  FaceNormal := IndexedConvexPolygonNormal(
    PArray_LongInt(DirectIndexes), Length(DirectIndexes),
    { I pass ShapeElements, not Coord.List, pointer here,
      to calculate normals in world-coordinates (that are
      in ShapeElements[*].Position). }
    PVector3Single(Addr(ShapeElements[0].Position)), Coord.Count, SizeOf(TAOElement),
    Vector3Single(0, 0, 0));

  { We assume here that polygon is convex, while in fact it doesn't
    have to be. But this will be a good approximation anyway, usually. }

  FaceArea := IndexedConvexPolygonArea(
    PArray_LongInt(DirectIndexes), Length(DirectIndexes),
    { I pass ShapeElements, not Coord.List, pointer here,
      to calculate area in world-coordinates (that are
      in ShapeElements[*].Position). }
    PVector3Single(Addr(ShapeElements[0].Position)), Coord.Count, SizeOf(TAOElement));

  { Split FaceArea into the number of polygon corners. }
  FaceArea /= Length(Indexes);

  for I := 0 to Length(Indexes) - 1 do
    if DirectIndexes[I] >= 0 then
    begin
      VectorAddVar(ShapeElements[DirectIndexes[I]].Normal, FaceNormal);
      { Split FaceArea into the number of polygon corners. }
      ShapeElements[DirectIndexes[I]].Area += FaceArea;
    end;
end;

procedure CalculateElements;

  procedure AddShapeElements(ShapeIndex: Integer; Shape: TShape);
  var
    Coord: TMFVec3f;
    Calculator: TElementsCalculator;
    ShapeElementIndex: Cardinal;
    ShapeElements: PAOElement;
    I: Integer;
  begin
    Shapes[ShapeIndex].Shape := Shape;

    if Shape.Geometry.Coord(Shape.State, Coord) and (Coord <> nil) then
    begin
      { Grow Elements array }
      ShapeElementIndex := Elements.Count;
      Elements.Count := Elements.Count + Coord.Count;
      ShapeElements := Addr(Elements.List^[ShapeElementIndex]);

      SetLength(Shapes[ShapeIndex].CoordToElement, Coord.Count);

      { This does a modified and tweaked version of normal calculation
        from CreateSmoothNormalsCoordinateNode from CastleInternalNormals.
        We want to additionally store Position and calculate Area for
        each element (= just vertex). }

      { Initialize: set position,
        clear normals and area at the beginning, we will sum to them. }
      for I := 0 to Coord.Count - 1 do
      begin
        ShapeElements[I].Position :=
          MatrixMultPoint(Shape.State.Transform, Coord.Items.List^[I]);
        ShapeElements[I].Normal := ZeroVector3Single;
        ShapeElements[I].Area := 0;
      end;

      { Iterate over polygons, adding normals and areas of new elements }
      Calculator := TElementsCalculator.Create;
      try
        Calculator.Coord := Coord.Items;
        if Shape.Geometry.CoordIndex <> nil then
          Calculator.CoordIndex := Shape.Geometry.CoordIndex.Items else
          Calculator.CoordIndex := nil;
        Calculator.ShapeElements := ShapeElements;
        Shape.Geometry.CoordPolygons(Shape.State, @Calculator.Polygon);
      finally FreeAndNil(Calculator) end;

      { Normalize all new normals }
      for I := 0 to Coord.Count - 1 do
        NormalizeVar(ShapeElements[I].Normal);
    end else
    begin
      SetLength(Shapes[ShapeIndex].CoordToElement, 0);
    end;
  end;

var
  SI: TShapeTreeIterator;
  I, GoodElementsCount, ShapeIndex, ShapeCoord: Integer;
begin
  Elements.Count := 0;

  SetLength(Shapes, Scene.Shapes.ShapesCount(true, true, false));

  ShapeIndex := 0;
  SI := TShapeTreeIterator.Create(Scene.Shapes, true, true, false);
  try
    while SI.GetNext do
    begin
      AddShapeElements(ShapeIndex, SI.Current);
      Inc(ShapeIndex);
    end;
  finally FreeAndNil(SI) end;

  { Remove Elements with zero normal vector.

    This means that vertex isn't neighbor to any other vertex,
    which means e.g. that IndexedFaceSet has no coordIndex (or maybe
    just doesn't use this particular vertex), or maybe we have IndexedLineSet
    (that has coordinates, but no polygons).

    In any case, we want to remove them, as they are invalid (zero normal,
    zero area, besides unused vertexes may be outside of Scene.BoundingBox
    (as calculating Scene.BoundingBox may conserve bbox to not contain
    unused vertexes) --- so unused vertexes would fail to be "squeezed"
    inside texture component correctly.

    By the way, you also have to initialize Shapes[].CoordToElement.
    This provides a mapping from original (per-shape, and before this removal)
    coords back to elements. }

  GoodElementsCount := 0;

  { increase ShapeIndex, ShapeCoord to next coord }
  ShapeIndex := 0;
  ShapeCoord := 0;
  while (ShapeIndex < Length(Shapes)) and
        (Length(Shapes[ShapeIndex].CoordToElement) = 0) do
    Inc(ShapeIndex);

  for I := 0 to Elements.Count - 1 do
  begin
    if not PerfectlyZeroVector(Elements.List^[I].Normal) then
    begin
      { Then Element I should be on position GoodElementsCount. }
      if GoodElementsCount <> I then
        Elements.List^[GoodElementsCount] := Elements.List^[I];
      Shapes[ShapeIndex].CoordToElement[ShapeCoord] := GoodElementsCount;
      Inc(GoodElementsCount);
    end else
    begin
      Shapes[ShapeIndex].CoordToElement[ShapeCoord] := -1;
    end;

    { increase ShapeIndex, ShapeCoord to next coord }
    Inc(ShapeCoord);
    if ShapeCoord >= Length(Shapes[ShapeIndex].CoordToElement) then
    begin
      Inc(ShapeIndex);
      ShapeCoord := 0;
      while (ShapeIndex < Length(Shapes)) and
            (Length(Shapes[ShapeIndex].CoordToElement) = 0) do
        Inc(ShapeIndex);
    end;
  end;

  Assert(ShapeIndex = Length(Shapes));

  Writeln('Bad elements (vertexes with no neighbors) removed: ',
    Elements.Count - GoodElementsCount, ', remaining good elements: ',
    GoodElementsCount);
  Elements.Count := GoodElementsCount;

{ Tests:

  Writeln('Elements: ', Elements.Count);
  for I := 0 to Elements.Count - 1 do
  begin
    Writeln('pos ', VectorToNiceStr(Elements.List^[I].Position),
            ' nor ', VectorToNiceStr(Elements.List^[I].Normal),
            ' area ', Elements.List^[I].Area:1:10);
  end;}
end;

var
  ElementsPositionAreaTex: TRGBAlphaImage;
  ElementsNormalTex: TRGBImage;
  { All Elements*Tex are square and have the same size:
    ElementsTexSize x ElementsTexSize. }
  ElementsTexSize: Cardinal;

  GLElementsPositionAreaTex: TGLuint;
  GLElementsNormalTex: TGLuint;
  GLElementsIntensityTex: TGLuint;

  { We will pack position (with arbitrary 3 float) and areas (with arbitrary
    float >= 0) and normal components (with floats in -1..1 range)
    into the texture components, which are limited to 0..1 range.

    "Scale" and "shift" are used on CPU to squeeze value into texture,
    such that
      tex_value := value / scale - shift;
    Then GPU can do
      value := (tex_value + shift) * scale;
    (as you see, we do the more costly division on CPU, to allow GPU
    to run with maximum speed).

    For normal vector components, scale = 2, shift = -0.5.
    For area, shift = 0, scale = calculated using max area.
    For positions, both scale and shift are calculated, using bounding box
    of points. }
  PositionScale, PositionShift: TVector3Single;
  AreaScale: Single;

procedure CalculateElementsTex;

  { Trivially convert Value in 0..1 range to a byte.
    By the way, checks is the Value really in 0..1 range (eventually,
    slightly off due to fp errors). This way it checks if our scale/shift
    variables are not too large. }
  function ClampedCheck(const Value: Single): Byte;
  var
    ResultInt: Integer;
  begin
    ResultInt := Round(Value * High(Byte));

    { I check ResultInt below with 25 margin, which is 25/255 ~= 0.1 error
      tolerance. That's pretty large tolerance, values really should fit
      within. }
    Assert(ResultInt >= -25, 'Value squeezed into texture is < 0');
    Assert(ResultInt <= High(Byte)+25, 'Value squeezed into texture is > 1');

    if ResultInt < 0 then
      Result := 0 else
    if ResultInt > High(Byte) then
      Result := High(Byte) else
      Result := ResultInt;
  end;

var
  TexSizeExponent: Cardinal;
  I: Integer;
  PositionArea: PVector4Byte;
  Normal: PVector3Byte;
  Element: PAOElement;
begin
  TexSizeExponent := Smallest2Exponent(Elements.Count);
  if TexSizeExponent mod 2 = 0 then
    TexSizeExponent := TexSizeExponent div 2 else
    TexSizeExponent := TexSizeExponent div 2 + 1;
  ElementsTexSize := 1 shl TexSizeExponent;

  Assert(Sqr(ElementsTexSize) >= Elements.Count);
  Writeln('For elements ', Elements.Count,
    ' we use texture size ', ElementsTexSize, '^2 = ', Sqr(ElementsTexSize), ' pixels');

  { calculate maximum area, which is just AreaScale }
  AreaScale := 0;
  for I := 0 to Elements.Count - 1 do
    MaxVar(AreaScale, Elements.List^[I].Area);

  { calculate PositionScale, PositionShift.
    We have min/max in Scene.BoundingBox. }
  PositionScale := Scene.BoundingBox.Size;
  for I := 0 to 2 do
  begin
    if PositionScale[I] = 0 then
      PositionScale[I] := 1;
    PositionShift[I] := Scene.BoundingBox.Data[0][I] / PositionScale[I];
  end;

  Writeln('To squeeze area into texture we use area_scale = ', AreaScale:1:10);
  Writeln('To squeeze positions into texture we use scale = ',
    VectorToNiceStr(PositionScale), ' and shift ',
    VectorToNiceStr(PositionShift), ' (bbox is ',
    Scene.BoundingBox.ToNiceStr, ')');

  { initialize textures }
  FreeAndNil(ElementsPositionAreaTex);
  FreeAndNil(ElementsNormalTex);

  ElementsPositionAreaTex := TRGBAlphaImage.Create(ElementsTexSize, ElementsTexSize);
  ElementsNormalTex := TRGBImage.Create(ElementsTexSize, ElementsTexSize);

  { fill textures }
  PositionArea := ElementsPositionAreaTex.AlphaPixels;
  Normal := ElementsNormalTex.RGBPixels;
  Element := PAOElement(Elements.List);
  for I := 0 to Elements.Count - 1 do
  begin
    PositionArea^[0] := ClampedCheck(Element^.Position[0] / PositionScale[0] - PositionShift[0]);
    PositionArea^[1] := ClampedCheck(Element^.Position[1] / PositionScale[1] - PositionShift[1]);
    PositionArea^[2] := ClampedCheck(Element^.Position[2] / PositionScale[2] - PositionShift[2]);
    PositionArea^[3] := ClampedCheck(Element^.Area / AreaScale);
    Normal^[0] := ClampedCheck(Element^.Normal[0] / 2 + 0.5);
    Normal^[1] := ClampedCheck(Element^.Normal[1] / 2 + 0.5);
    Normal^[2] := ClampedCheck(Element^.Normal[2] / 2 + 0.5);
    Inc(Element);
    Inc(PositionArea);
    Inc(Normal);
  end;

  { Only for testing purposes, fill the rest of the texture will special
    value. This is only for testing, that is correct shader and other code
    should not even touch the remaining pixels --- but in case of trouble
    (if something will accidentally access other values),
    it's easier when things don't jump randomly (because of memory garbage),
    and can be easily detected by changing the special value below. }
  for I := Elements.Count to Sqr(ElementsTexSize) - 1 do
  begin
    PositionArea^[0] := 255;
    PositionArea^[1] := 0;
    PositionArea^[2] := 0;
    PositionArea^[3] := 255;
    Normal^[0] := 255;
    Normal^[1] := 0;
    Normal^[2] := 0;
    Inc(Element);
    Inc(PositionArea);
    Inc(Normal);
  end;

  GLElementsPositionAreaTex := LoadGLTexture(ElementsPositionAreaTex,
    TextureFilter(minNearest, magNearest), Texture2DRepeat);
  GLElementsNormalTex := LoadGLTexture(ElementsNormalTex,
    TextureFilter(minNearest, magNearest), Texture2DRepeat);

  { Prepare OpenGL texture for shaders output (and also input to 2nd pass
    of algorithm). }
  glGenTextures(1, @GLElementsIntensityTex);
  glBindTexture(GL_TEXTURE_2D, GLElementsIntensityTex);

  { Texture data is not initialized at this point,
    but we set texture width, height, format and such.
    This will allow texture to be updated by glCopyTexSubImage,
    which is faster than glCopyTexImage. }
  glTexImage2D(GL_TEXTURE_2D, 0, GL_LUMINANCE,
    ElementsTexSize, ElementsTexSize, 0,
    { Format and type don't matter, as far as I understand, since pixels
      pointer is nil. }
    GL_LUMINANCE, GL_UNSIGNED_BYTE, nil);
  TextureMemoryProfiler.Allocate(GLElementsIntensityTex, '', 'luminance',
    ElementsTexSize * ElementsTexSize,
    false, ElementsTexSize, ElementsTexSize, 1);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
end;

{ ---------------------------------------------------------------------------- }

var
  FullRenderShape: TShape;
  FullRenderShapeInfo: PShapeInfo;
  FullRenderIntensityTex: TGrayscaleImage;

type
  TMySceneManager = class(TCastleSceneManager)
    procedure RenderFromView3D(const Params: TRenderParams); override;
  end;

var
  SceneManager: TMySceneManager;

procedure TMySceneManager.RenderFromView3D(const Params: TRenderParams);

  { If ElementsIntensityTex = nil,
    then all element discs will have the same glMaterial.

    Otherwise, we will assign single glColor for each element,
    using values from ElementsIntensityTex. }
  procedure DoShowElements(ElementsIntensityTex: TGrayscaleImage);
  var
    I: Integer;
    Q: PGLUQuadric;
    NewX, NewY, NewZ: TVector3Single;
    Radius: Float;
    ElementIntensity: PByte;
  begin
    glPushAttrib(GL_ENABLE_BIT);
      glEnable(GL_DEPTH_TEST);
      glMaterialv(GL_FRONT_AND_BACK, GL_SPECULAR, Vector4Single(0, 0, 0, 1));

      if ElementsIntensityTex = nil then
      begin
        glMaterialv(GL_FRONT_AND_BACK, GL_DIFFUSE, Vector4Single(1, 1, 0, 1));
      end else
      begin
        ElementIntensity := ElementsIntensityTex.GrayscalePixels;
      end;

      Q := NewGLUQuadric(false, GLU_FLAT, GLU_OUTSIDE, GLU_FILL);

      for I := 0 to Elements.Count - 1 do
      begin
        if ElementsIntensityTex <> nil then
        begin
          glColor3ub(ElementIntensity^, ElementIntensity^, ElementIntensity^);
          Inc(ElementIntensity);
        end;

        glPushMatrix;
          NewZ := Elements.List^[I].Normal;
          NewX := AnyOrthogonalVector(NewZ);
          NewY := VectorProduct(NewZ, NewX);
          glMultMatrix(TransformToCoordsMatrix(Elements.List^[I].Position,
            NewX, NewY, NewZ));

          { Area = Pi * Radius^2, so Radius := Sqrt(Area/Pi) }
          Radius := Sqrt(Elements.List^[I].Area/Pi);
          gluDisk(Q, 0, Radius, 8, 2);

        glPopMatrix;
      end;

      gluDeleteQuadric(Q);
    glPopAttrib;
  end;

  function CaptureAORect(SizePower2: boolean): TGrayscaleImage;
  var
    TexHeight: Cardinal;
  begin
    if SizePower2 then
      TexHeight := ElementsTexSize else
      { If size doesn't have to be power of 2 (e.g. if this is grabbed
        only for CPU), then height may be smaller, which is a good thing
        --- we don't have to grab so much pixels from color buffer. }
      TexHeight := DivRoundUp(Cardinal(Elements.Count), ElementsTexSize);
    Result := SaveScreen_NoFlush(TGrayscaleImage,
      Rectangle(0, 0, ElementsTexSize, TexHeight), Window.SaveScreenBuffer) as
      TGrayscaleImage;
  end;

  procedure RenderAORect;

    procedure DoRender(Pass: Integer);
    begin
      GLSLProgram[Pass].Enable;

      glActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_2D, GLElementsPositionAreaTex);
      GLSLProgram[Pass].SetUniform('tex_elements_position_area', 0);

      glActiveTexture(GL_TEXTURE1);
      glBindTexture(GL_TEXTURE_2D, GLElementsNormalTex);
      GLSLProgram[Pass].SetUniform('tex_elements_normal', 1);

      if Pass = 1 then
      begin
        { GLElementsIntensityTex is already bound to GL_TEXTURE2 }
        GLSLProgram[Pass].SetUniform('tex_elements_intensity', 2);
      end;

      GLSLProgram[Pass].SetUniform('area_scale', AreaScale);
      GLSLProgram[Pass].SetUniform('position_scale', PositionScale);
      GLSLProgram[Pass].SetUniform('position_shift', PositionShift);

      if GLVersion.Fglrx then
      begin
        GLSLProgram[Pass].SetUniform('elements_count', Elements.Count);
        GLSLProgram[Pass].SetUniform('tex_elements_size', TGLint(ElementsTexSize));
      end;

      GLSLProgram[Pass].SetUniform('zero_5', 0.5);
      { GLSLProgram[Pass].SetUniform('pi', Pi); <- not used now in shader }
      GLSLProgram[Pass].SetUniform('shadow_scale', ShadowScale);

      { Render rectange with each pixel corresponding to one element
        that needs intensity calculated. }
      glRecti(0, 0, ElementsTexSize,
        DivRoundUp(Cardinal(Elements.Count), ElementsTexSize));

      GLSLProgram[Pass].Disable;
    end;

  var
    SavedProjectionMatrix: TMatrix4Single;
  begin
    SavedProjectionMatrix := ProjectionMatrix;
    OrthoProjection(0, Window.Width, 0, Window.Height);

    glPushMatrix;

      glLoadIdentity;
      DoRender(0);

      if DrawType = dtPass2 then
      begin
        { Alternative way to copy texture through CPU:
        var
          ElementsIntensityTex: TGrayscaleImage;
        ElementsIntensityTex := CaptureAORect(true);
        LoadGLGeneratedTexture(GLElementsIntensityTex, ElementsIntensityTex,
          GL_NEAREST, GL_NEAREST);}

        { Capture result of 1st pass into GLElementsIntensityTex
          using glCopyTexSubImage2D. Also, this BTW binds GLElementsIntensityTex
          to GL_TEXTURE2 texture unit, used by DoRender(1). }

        glActiveTexture(GL_TEXTURE2);
        glBindTexture(GL_TEXTURE_2D, GLElementsIntensityTex);
        glReadBuffer(GL_BACK);
        glCopyTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, 0, 0,
          ElementsTexSize, ElementsTexSize);

        DoRender(1);
      end;

    glPopMatrix;

    ProjectionMatrix := SavedProjectionMatrix;
  end;

var
  ElementsIntensityTex: TGrayscaleImage;
begin
  { RenderFromView3D must initialize some Params fields itself }
  Params.InShadow := false;

  case DrawType of
    dtNormalGL:
      begin
        Params.Transparent := false; Params.ShadowVolumesReceivers := false; Scene.Render(RenderingCamera.Frustum, Params);
        Params.Transparent := false; Params.ShadowVolumesReceivers := true ; Scene.Render(RenderingCamera.Frustum, Params);
        Params.Transparent := true ; Params.ShadowVolumesReceivers := false; Scene.Render(RenderingCamera.Frustum, Params);
        Params.Transparent := true ; Params.ShadowVolumesReceivers := true ; Scene.Render(RenderingCamera.Frustum, Params);
      end;
    dtElements:
      begin
        glPushAttrib(GL_ENABLE_BIT or GL_LIGHTING_BIT);
          glEnable(GL_LIGHTING);
          glEnable(GL_LIGHT0);
          DoShowElements(nil);
        glPopAttrib;
      end;
    dtElementsIntensity:
      begin
        RenderAORect;
        ElementsIntensityTex := CaptureAORect(false);
        try
          DoShowElements(ElementsIntensityTex);
        finally FreeAndNil(ElementsIntensityTex) end;
      end;
    dtPass1, dtPass2:
      begin
        RenderAORect;
        FullRenderIntensityTex := CaptureAORect(false);
        try
          FullRenderShape := nil;
          Params.Transparent := false; Params.ShadowVolumesReceivers := false; Scene.Render(RenderingCamera.Frustum, Params);
          Params.Transparent := false; Params.ShadowVolumesReceivers := true ; Scene.Render(RenderingCamera.Frustum, Params);
          Params.Transparent := true ; Params.ShadowVolumesReceivers := false; Scene.Render(RenderingCamera.Frustum, Params);
          Params.Transparent := true ; Params.ShadowVolumesReceivers := true ; Scene.Render(RenderingCamera.Frustum, Params);
        finally FreeAndNil(FullRenderIntensityTex) end;
      end;
  end;
end;

type
  THelper = class
    class procedure VertexColor(var Color: TVector3Single;
      Shape: TShape; const VertexPosition: TVector3Single;
      VertexIndex: Integer);

    class procedure SceneGeometryChanged(Scene: TCastleSceneCore;
      const SomeLocalGeometryChanged: boolean;
      OnlyShapeChanged: TShape);
  end;

class procedure THelper.VertexColor(var Color: TVector3Single;
  Shape: TShape; const VertexPosition: TVector3Single;
  VertexIndex: Integer);
var
  ElemIndex, I: Integer;
  Intensity: Single;
begin
  if FullRenderShape <> Shape then
  begin
    FullRenderShape := Shape;
    FullRenderShapeInfo := nil;
    for I := 0 to Length(Shapes) - 1 do
      if Shapes[I].Shape = Shape then
      begin
        FullRenderShapeInfo := @Shapes[I];
        Break;
      end;
    Assert(FullRenderShapeInfo <> nil, 'Shape info not found');
  end;

  ElemIndex := FullRenderShapeInfo^.CoordToElement[VertexIndex];
  if ElemIndex = -1 then
    Color := ZeroVector3Single { element invalid, probably separate vertex } else
  begin
    Intensity := FullRenderIntensityTex.GrayscalePixels[ElemIndex]/255;
    Color[0] *= Intensity;
    Color[1] *= Intensity;
    Color[2] *= Intensity;
  end;
end;

class procedure THelper.SceneGeometryChanged(Scene: TCastleSceneCore;
  const SomeLocalGeometryChanged: boolean;
  OnlyShapeChanged: TShape);
var
  OldElementsTexSize, OldElementsCount: Cardinal;
begin
  OldElementsTexSize := ElementsTexSize;
  OldElementsCount := Elements.Count;

  CalculateElements;
  CalculateElementsTex;

  if (OldElementsTexSize <> ElementsTexSize) or
     (OldElementsCount <> Cardinal(Elements.Count)) then
  begin
    Writeln('TODO: animation changed elements count / texture size. Shaders need to reinitialiazed.');
  end;
end;

{ CastleWindow callbacks --------------------------------------------------------- }

procedure Open(Container: TUIContainer);
const
  GLSLProgramBaseName = 'dynamic_ambient_occlusion';
var
  ShaderString: string;
begin
  if (Elements.Count = 0) or
     Scene.BoundingBox.IsEmpty then
  begin
    Window.Controls.Remove(SceneManager); { do not try to render }
    MessageOk(Window, 'No elements, or empty bounding box --- we cannot do dyn ambient occlusion. Exiting.');
    Window.Close;
    Exit;
  end;

  CalculateElementsTex;

  { initialize GLSL program }
  GLSLProgram[0] := TGLSLProgram.Create;

  if GLSLProgram[0].Support = gsNone then
  begin
    Window.Controls.Remove(SceneManager); { do not try to render }
    MessageOk(Window, 'Sorry, GLSL shaders not supported on your graphic card. Exiting.');
    Window.Close;
    Exit;
  end;

  ShaderString := FileToString(GLSLProgramBaseName + '.fs');

  if GLVersion.Fglrx then
  begin
    StringReplaceAllVar(ShaderString,
      '/*$defines*/',
      '/*$defines*/' + NL + '#define FGLRX');
  end else
  begin
    { Integer constants are really constant for the shader.
      This allows OpenGL to optimize them more.
      Especially important for $elements_count, since then the "for" loop
      inside the shader can be unrolled.
      Required e.g. by NVidia GPU "GeForce FX 5200/AGP/SSE2/3DNOW!" }
    StringReplaceAllVar(ShaderString, '$tex_elements_size', IntToStr(ElementsTexSize));
    StringReplaceAllVar(ShaderString, '$elements_count', IntToStr(Elements.Count));
  end;

  GLSLProgram[0].AttachFragmentShader(ShaderString);
  { For this test program, we eventually allow shader to run in software.
    We display debug info, so user should know what's going on. }
  GLSLProgram[0].Link;
  { Only warn on non-used uniforms. This is more comfortable for shader
    development, you can easily comment shader parts. }
  GLSLProgram[0].UniformNotFoundAction := uaWarning;
  Writeln('----------------------------- Shader for 1st pass:');
  Writeln(GLSLProgram[0].DebugInfo);

  { Analogously, load GLSLProgram[1] (for 2nd pass). The only difference
    is that we #define PASS_2 this time. }
  StringReplaceAllVar(ShaderString,
    '/*$defines*/',
    '/*$defines*/' + NL + '#define PASS_2');
  GLSLProgram[1] := TGLSLProgram.Create;
  GLSLProgram[1].AttachFragmentShader(ShaderString);
  GLSLProgram[1].UniformNotFoundAction := uaWarning;
  GLSLProgram[1].Link;
  Writeln('----------------------------- Shader for 2nd pass:');
  Writeln(GLSLProgram[1].DebugInfo);

  Writeln('--------------------------------------------------');
end;

procedure Close(Container: TUIContainer);
begin
  FreeAndNil(GLSLProgram[0]);
  FreeAndNil(GLSLProgram[1]);
end;

procedure Update(Container: TUIContainer);
begin
  if Window.Pressed.Characters['s'] then
  begin
    ShadowScale *= Power(1.1, Window.Fps.UpdateSecondsPassed * 20);
    Window.Invalidate;
  end;
  if Window.Pressed.Characters['S'] then
  begin
    ShadowScale *= Power(1/1.1, Window.Fps.UpdateSecondsPassed * 20);
    Window.Invalidate;
  end;
end;

{ menu ----------------------------------------------------------------------- }

function CreateMainMenu: TMenu;
var
  M: TMenu;
  Radio: TMenuItemRadio;
  RadioGroup: TMenuItemRadioGroup;
begin
  Result := TMenu.Create('Main menu');
  M := TMenu.Create('_Program');
    Radio := TMenuItemRadio.Create('Draw _Normal OpenGL', 100, DrawType = dtNormalGL, true);
    RadioGroup := Radio.Group;
    M.Append(Radio);

    Radio := TMenuItemRadio.Create('Draw _Elements', 101, DrawType = dtElements, true);
    Radio.Group := RadioGroup;
    M.Append(Radio);

    Radio := TMenuItemRadio.Create('Draw Elements _Intensity', 102, DrawType = dtElementsIntensity, true);
    Radio.Group := RadioGroup;
    M.Append(Radio);

    Radio := TMenuItemRadio.Create('Draw Dynamic AO (only _1st pass)', 103, DrawType = dtPass1, true);
    Radio.Group := RadioGroup;
    M.Append(Radio);

    Radio := TMenuItemRadio.Create('Draw Dynamic AO (_2 passes)', 104, DrawType = dtPass2, true);
    Radio.Group := RadioGroup;
    M.Append(Radio);

    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create(
      'Animation _Playing / Paused', 150, CtrlP, Scene.TimePlaying, true));

    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Exit', 200));
    Result.Append(M);
end;

procedure UpdateSceneAttribs;
begin
  case DrawType of
    dtNormalGL:
      begin
        Scene.Attributes.OnVertexColor := nil;
        Scene.Attributes.Lighting := true;
      end;
    dtPass1, dtPass2:
      begin
        Scene.Attributes.OnVertexColor := @THelper(nil).VertexColor;
        Scene.Attributes.Lighting := false;
      end;
    { else they don't matter }
  end;
end;

procedure MenuClick(Container: TUIContainer; Item: TMenuItem);
begin
  case Item.IntData of
    100: begin DrawType := dtNormalGL; UpdateSceneAttribs; end;
    101: DrawType := dtElements;
    102: DrawType := dtElementsIntensity;
    103: begin DrawType := dtPass1; UpdateSceneAttribs; end;
    104: begin DrawType := dtPass2; UpdateSceneAttribs; end;

    150: Scene.TimePlaying := not Scene.TimePlaying;

    200: Window.Close;
    else Exit;
  end;

  Window.Invalidate;
end;

var
  ModelURL: string =
    //'data/chinchilla_awakens.x3dv';
    'data/peach.wrl.gz';
begin
  Window := TCastleWindowCustom.Create(Application);

  Elements := TAOElementList.Create;

  Parameters.CheckHighAtMost(1);
  if Parameters.High = 1 then
    ModelURL := Parameters[1];

  try
    ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);

    Scene := TCastleScene.Create(Window);
    Scene.Load(ModelURL);
    UpdateSceneAttribs;

    CalculateElements;

    { start Scene animation }
    Scene.Spatial := [ssRendering, ssDynamicCollisions];
    Scene.OnGeometryChanged := @THelper(nil).SceneGeometryChanged;
    Scene.ProcessEvents := true;

    { init SceneManager, with a Scene inside }
    SceneManager := TMySceneManager.Create(Window);
    Window.Controls.InsertFront(SceneManager);
    SceneManager.MainScene := Scene;
    SceneManager.Items.Add(Scene);

    Window.MainMenu := CreateMainMenu;
    Window.OnMenuClick := @MenuClick;

    Window.OnOpen := @Open;
    Window.OnClose := @Close;
    Window.OnUpdate := @Update;
    Window.SetDemoOptions(K_F11, CharEscape, true);
    Window.OpenAndRun;
  finally
    FreeAndNil(SceneManager);
    FreeAndNil(Elements);
    FreeAndNil(ElementsPositionAreaTex);
    FreeAndNil(ElementsNormalTex);
  end;
end.
