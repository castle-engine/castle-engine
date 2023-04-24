{
  Copyright 2009-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Sample implementation of "Dynamic ambient occlusion".
  See README.txt for descriptions and links.

  By default opens the "peach" model from data/ subdirectory.
  You can pass on command-line URL of a different model to open,
  like "castle-data:/chinchilla_awakens.x3dv".

  Navigate by mouse/keys like in view3dscene (see https://castle-engine.io/view3dscene.php).

  Use keys s / S scale shadow more / less (for more/less dramatic effect;
  the default scale is anyway 2 times larger than what math suggests,
  to be more dramatic).
}
program dynamic_ambient_occlusion;

{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

uses SysUtils, Classes, Math,
  {$ifdef FPC} GL, GLExt, {$else} OpenGL, OpenGLext, {$endif}
  CastleVectors, CastleWindow, CastleTriangles, CastleTransform,
  CastleClassUtils, CastleUtils, CastleKeysMouse,
  CastleGLUtils, CastleSceneCore, CastleScene, CastleParameters,
  CastleFilesUtils, CastleStringUtils, CastleGLShaders, CastleShapes,
  X3DFields, X3DNodes, CastleImages, CastleGLImages, CastleMessages, CastleLog,
  CastleGLVersion, CastleViewport, CastleRectangles, CastleApplicationProperties,
  CastleRenderContext, CastleColors, CastleCameras,
  SceneUtilities;

type
  TDrawType = (dtNormal, dtElements, dtElementsIntensity, dtPass1, dtPass2);

var
  Window: TCastleWindow;

  Scene: TCastleScene;
  SceneElements: TCastleScene;
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
    Position, Normal: TVector3;
  end;
  PAOElement = ^TAOElement;
  TAOElementList = {$ifdef FPC}specialize{$endif} TStructList<TAOElement>;

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
    CoordIndex: TInt32List;
    Coord: TVector3List;
    ShapeElements: PAOElement;

    procedure Polygon(const Indexes: array of Cardinal);
  end;

procedure TElementsCalculator.Polygon(
  const Indexes: array of Cardinal);
var
  FaceNormal: TVector3;
  { DirectIndexes is Int32, not Cardinal array, since we cannot
    guarantee that CoordIndex items are >= 0. }
  DirectIndexes: array of Int32;
  I: Integer;
  FaceArea: Single;
begin
  SetLength(DirectIndexes, Length(Indexes));
  if CoordIndex <> nil then
  begin
    for I := 0 to Length(Indexes) - 1 do
      DirectIndexes[I] := CoordIndex[Indexes[I]];
  end else
  begin
    for I := 0 to Length(Indexes) - 1 do
      DirectIndexes[I] := Indexes[I];
  end;

  FaceNormal := IndexedConvexPolygonNormal(
    PInt32Array(DirectIndexes), Length(DirectIndexes),
    { I pass ShapeElements, not Coord.List, pointer here,
      to calculate normals in world-coordinates (that are
      in ShapeElements[*].Position). }
    PVector3Array(Addr(ShapeElements[0].Position)), Coord.Count, SizeOf(TAOElement),
    Vector3(0, 0, 0));

  { We assume here that polygon is convex, while in fact it doesn't
    have to be. But this will be a good approximation anyway, usually. }

  FaceArea := IndexedConvexPolygonArea(
    PInt32Array(DirectIndexes), Length(DirectIndexes),
    { I pass ShapeElements, not Coord.List, pointer here,
      to calculate area in world-coordinates (that are
      in ShapeElements[*].Position). }
    PVector3Array(Addr(ShapeElements[0].Position)), Coord.Count, SizeOf(TAOElement));

  { Split FaceArea into the number of polygon corners. }
  FaceArea := FaceArea / Length(Indexes);

  for I := 0 to Length(Indexes) - 1 do
    if DirectIndexes[I] >= 0 then
    begin
      ShapeElements[DirectIndexes[I]].Normal :=
        ShapeElements[DirectIndexes[I]].Normal + FaceNormal;
      { Split FaceArea into the number of polygon corners. }
      ShapeElements[DirectIndexes[I]].Area := ShapeElements[DirectIndexes[I]].Area + FaceArea;
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

    if Shape.Geometry.InternalCoord(Shape.State, Coord) and (Coord <> nil) then
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
          Shape.State.Transformation.Transform.MultPoint(Coord.Items.List^[I]);
        ShapeElements[I].Normal := TVector3.Zero;
        ShapeElements[I].Area := 0;
      end;

      { Iterate over polygons, adding normals and areas of new elements }
      Calculator := TElementsCalculator.Create;
      try
        Calculator.Coord := Coord.Items;
        if Shape.Geometry.CoordIndexField <> nil then
          Calculator.CoordIndex := Shape.Geometry.CoordIndexField.Items else
          Calculator.CoordIndex := nil;
        Calculator.ShapeElements := ShapeElements;
        Shape.Geometry.InternalCoordPolygons(Shape.State, @Calculator.Polygon);
      finally FreeAndNil(Calculator) end;

      { Normalize all new normals }
      for I := 0 to Coord.Count - 1 do
        ShapeElements[I].Normal := ShapeElements[I].Normal.Normalize;
    end else
    begin
      SetLength(Shapes[ShapeIndex].CoordToElement, 0);
    end;
  end;

var
  ShapeList: TShapeList;
  I, GoodElementsCount, ShapeIndex, ShapeCoord: Integer;
begin
  Elements.Count := 0;

  SetLength(Shapes, Scene.Shapes.ShapesCount(true, true, false));

  ShapeList := Scene.Shapes.TraverseList(true, true, false);
  for ShapeIndex := 0 to ShapeList.Count - 1 do
    AddShapeElements(ShapeIndex, ShapeList[ShapeIndex]);

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
    if not Elements.List^[I].Normal.IsPerfectlyZero then
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

  WritelnLog('Bad elements (vertexes with no neighbors) removed: %d, remaining good elements: %d', [
    Elements.Count - GoodElementsCount,
    GoodElementsCount
  ]);
  Elements.Count := GoodElementsCount;
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
  PositionScale, PositionShift: TVector3;
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
  WritelnLog('For elements %d we use texture size %d^2 = %d pixels', [
    Elements.Count,
    ElementsTexSize,
    Sqr(ElementsTexSize)
  ]);

  { calculate maximum area, which is just AreaScale }
  AreaScale := 0;
  for I := 0 to Elements.Count - 1 do
    MaxVar(AreaScale, Elements.List^[I].Area);

  { calculate PositionScale, PositionShift.
    We have min/max in Scene.BoundingBox. }
  PositionScale := Scene.BoundingBox.Size;
  for I := 0 to 2 do
  begin
    if PositionScale.Data[I] = 0 then
      PositionScale.Data[I] := 1;
    PositionShift.Data[I] := Scene.BoundingBox.Data[0][I] / PositionScale[I];
  end;

  WritelnLog('To squeeze area into texture we use area_scale = %f', [AreaScale]);
  WritelnLog('To squeeze positions into texture we use scale = %s and shift %s (bbox is %s)', [
    PositionScale.ToString,
    PositionShift.ToString,
    Scene.BoundingBox.ToString
  ]);

  { initialize textures }
  FreeAndNil(ElementsPositionAreaTex);
  FreeAndNil(ElementsNormalTex);

  ElementsPositionAreaTex := TRGBAlphaImage.Create(ElementsTexSize, ElementsTexSize);
  ElementsNormalTex := TRGBImage.Create(ElementsTexSize, ElementsTexSize);

  { fill textures }
  PositionArea := ElementsPositionAreaTex.Pixels;
  Normal := ElementsNormalTex.Pixels;
  Element := PAOElement(Elements.List);
  for I := 0 to Elements.Count - 1 do
  begin
    PositionArea^.X := ClampedCheck(Element^.Position.X / PositionScale.X - PositionShift.X);
    PositionArea^.Y := ClampedCheck(Element^.Position.Y / PositionScale.Y - PositionShift.Y);
    PositionArea^.Z := ClampedCheck(Element^.Position.Z / PositionScale.Z - PositionShift.Z);
    PositionArea^.W := ClampedCheck(Element^.Area / AreaScale);
    Normal^.X := ClampedCheck(Element^.Normal.X / 2 + 0.5);
    Normal^.Y := ClampedCheck(Element^.Normal.Y / 2 + 0.5);
    Normal^.Z := ClampedCheck(Element^.Normal.Z / 2 + 0.5);
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
    PositionArea^.X := 255;
    PositionArea^.Y := 0;
    PositionArea^.Z := 0;
    PositionArea^.W := 255;
    Normal^.X := 255;
    Normal^.Y := 0;
    Normal^.Z := 0;
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

{ Callbacks handle ----------------------------------------------------------- }

var
  FullRenderShape: TShape;
  FullRenderShapeInfo: PShapeInfo;
  FullRenderIntensityTex: TGrayscaleImage;

type
  THelper = class
    class function VertexColor(
      const Shape: TShape;
      const VertexPosition: TVector3;
      const VertexIndex: Integer): TCastleColorRGB;

    class procedure SceneGeometryChanged(Scene: TCastleSceneCore;
      const SomeLocalGeometryChanged: boolean;
      OnlyShapeChanged: TShape);
  end;

class function THelper.VertexColor(
  const Shape: TShape;
  const VertexPosition: TVector3;
  const VertexIndex: Integer): TCastleColorRGB;
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
    Result := TVector3.Zero { element invalid, probably separate vertex } else
  begin
    Intensity := FullRenderIntensityTex.Pixels[ElemIndex]/255;
    Result.X := Intensity;
    Result.Y := Intensity;
    Result.Z := Intensity;
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
    WritelnWarning('TODO: animation changed elements count / texture size. Shaders need to reinitialiazed.');
  end;
end;

{ ---------------------------------------------------------------------------- }

type
  TMyViewport = class(TCastleViewport)
    procedure RenderFromView3D(const Params: TRenderParams); override;
  end;

var
  Viewport: TMyViewport;
  RectVbo: TGLuint;
  RectVao: TVertexArrayObject;

procedure TMyViewport.RenderFromView3D(const Params: TRenderParams);

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

    { Use direct OpenGL(ES) calls to render rectangle with 2D projection.
      This is a bit like DrawPrimitive2D implementation, but simplified to only
      account for rectangle and for EnableFixedFunction = false case. }
    procedure RenderRect(const Rect: TFloatRectangle);
    var
      Points: array [0..3] of TVector2;
      UniformViewportSize: TGLSLUniform;
      AttribVertex: TGLSLAttribute;
    begin
      Points[0] := Vector2(Rect.Left , Rect.Bottom);
      Points[1] := Vector2(Rect.Right, Rect.Bottom);
      Points[2] := Vector2(Rect.Right, Rect.Top);
      Points[3] := Vector2(Rect.Left , Rect.Top);

      if RectVao = nil then
        RectVao := TVertexArrayObject.Create(nil);
      RenderContext.CurrentVao := RectVao;

      if RectVbo = 0 then
        glGenBuffers(1, @RectVbo);
      glBindBuffer(GL_ARRAY_BUFFER, RectVbo);
      glBufferData(GL_ARRAY_BUFFER, SizeOf(Points), @Points, GL_DYNAMIC_DRAW);

      UniformViewportSize := RenderContext.CurrentProgram.Uniform('viewport_size');
      AttribVertex := RenderContext.CurrentProgram.Attribute('vertex');

      AttribVertex.EnableArrayVector2(RectVao, SizeOf(TVector2), 0);
      UniformViewportSize.SetValue(Vector2(
        RenderContext.Viewport.Width,
        RenderContext.Viewport.Height
      ));

      glDrawArrays(GL_TRIANGLE_FAN, 0, High(Points) + 1);

      AttribVertex.DisableArray;
      glBindBuffer(GL_ARRAY_BUFFER, 0);
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    end;

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

      GLSLProgram[Pass].SetUniform('shadow_scale', ShadowScale);

      { Render rectangle with each pixel corresponding to one element
        that needs intensity calculated. }
      RenderRect(FloatRectangle(
        0,
        0,
        ElementsTexSize,
        DivRoundUp(Cardinal(Elements.Count), ElementsTexSize)
      ));

      GLSLProgram[Pass].Disable;
    end;

  begin
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
  end;

begin
  if DrawType in [dtPass1, dtPass2] then
  begin
    FreeAndNil(FullRenderIntensityTex); // free previous FullRenderIntensityTex

    RenderAORect;
    FullRenderIntensityTex := CaptureAORect(false);
    FullRenderShape := nil;
    SetSceneColors(Scene, @THelper(nil).VertexColor);
  end;
  inherited;
end;

{ CastleWindow callbacks --------------------------------------------------------- }

procedure Open(Container: TCastleContainer);

  procedure Error(const S: String);
  begin
    Window.Controls.Remove(Viewport); { do not try to render }
    MessageOk(Window, S);
    Window.Close;
  end;

var
  FragmentShader, VertexShader: string;
begin
  if not GLFeatures.Shaders then
  begin
    Error('This GPU cannot handle dynamic ambient occlusion. GLSL shaders not supported.');
    Exit;
  end;
  if GLFeatures.EnableFixedFunction then
  begin
    Error('This GPU cannot handle dynamic ambient occlusion. Color.mode="MODULATE" does not work in fixed-function pipeline.');
    Exit;
  end;
  if not GLFeatures.VertexBufferObject then
  begin
    Error('This GPU cannot handle dynamic ambient occlusion. We use direct OpenGL(ES) calls here that use VBO.');
    Exit;
  end;
  if (Elements.Count = 0) or Scene.BoundingBox.IsEmpty then
  begin
    Error('Scene has no elements, or empty bounding box --- we cannot do dynamic ambient occlusion.');
    Exit;
  end;
  if GLVersion.Fglrx then
  begin
    Error('This GPU cannot handle dynamic ambient occlusion. FGLRX implementation of GLSL had so many bugs that supporting it for this demo was very burdensome.');
  end;

  CalculateElementsTex;

  FragmentShader := FileToString('castle-data:/shaders/dynamic_ambient_occlusion.fs');
  VertexShader := FileToString('castle-data:/shaders/dynamic_ambient_occlusion.vs');

  { Integer constants are really constant for the shader.
    This allows OpenGL to optimize them more.
    Especially important for $elements_count, since then the "for" loop
    inside the shader can be unrolled.
    Required e.g. by NVidia GPU "GeForce FX 5200/AGP/SSE2/3DNOW!" }
  StringReplaceAllVar(FragmentShader, '$tex_elements_size', IntToStr(ElementsTexSize));
  StringReplaceAllVar(FragmentShader, '$elements_count', IntToStr(Elements.Count));

  { initialize GLSL program }
  GLSLProgram[0] := TGLSLProgram.Create;
  GLSLProgram[0].AttachVertexShader(VertexShader);
  GLSLProgram[0].AttachFragmentShader(FragmentShader);
  { For this test program, we eventually allow shader to run in software.
    We display debug info, so user should know what's going on. }
  GLSLProgram[0].Link;

  WritelnLogMultiline('Shader for 1st pass', GLSLProgram[0].DebugInfo);

  StringReplaceAllVar(FragmentShader,
    '/*$defines*/',
    '/*$defines*/' + NL + '#define PASS_2');

  GLSLProgram[1] := TGLSLProgram.Create;
  GLSLProgram[1].AttachVertexShader(VertexShader);
  GLSLProgram[1].AttachFragmentShader(FragmentShader);
  GLSLProgram[1].Link;

  WritelnLogMultiline('Shader for 2nd pass', GLSLProgram[1].DebugInfo);
end;

procedure Close(Container: TCastleContainer);
begin
  FreeAndNil(GLSLProgram[0]);
  FreeAndNil(GLSLProgram[1]);
end;

procedure Update(Container: TCastleContainer);
begin
  if Window.Pressed.Characters['s'] then
  begin
    ShadowScale := ShadowScale * (Power(1.1, Window.Fps.SecondsPassed * 20));
    Window.Invalidate;
  end;
  if Window.Pressed.Characters['S'] then
  begin
    ShadowScale := ShadowScale * (Power(1/1.1, Window.Fps.SecondsPassed * 20));
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
    Radio := TMenuItemRadio.Create('Draw _Normal', 100, DrawType = dtNormal, true);
    RadioGroup := Radio.Group;
    M.Append(Radio);

    Radio := TMenuItemRadio.Create('Draw _Elements', 101, DrawType = dtElements, true);
    Radio.Group := RadioGroup;
    M.Append(Radio);

    Radio := TMenuItemRadio.Create('Draw Elements _Intensity', 102, DrawType = dtElementsIntensity, true);
    Radio.Group := RadioGroup;
    M.Append(Radio);

    Radio := TMenuItemRadio.Create('Update and Draw Dynamic AO (only _1st pass)', 103, DrawType = dtPass1, true);
    Radio.Group := RadioGroup;
    M.Append(Radio);

    Radio := TMenuItemRadio.Create('Update and Draw Dynamic AO (_2 passes)', 104, DrawType = dtPass2, true);
    Radio.Group := RadioGroup;
    M.Append(Radio);

    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create(
      'Animation _Playing / Paused', 150, CtrlP, Scene.TimePlaying, true));

    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Exit', 200));
    Result.Append(M);
end;

procedure MenuClick(Container: TCastleContainer; Item: TMenuItem);

  { Update SceneElements contents.

    If ElementsIntensityTex = nil,
    then all element discs will have the same material.
    Otherwise, we will assign a color for each element,
    using values from ElementsIntensityTex. }
  procedure UpdateElements(const ElementsIntensityTex: TGrayscaleImage);
  var
    RootNode: TX3DRootNode;
    Mat: TUnlitMaterialNode;
    Disk: TDisk2DNode;
    DiskShape: TShapeNode;
    DiskTransform: TTransformNode;
    I: Integer;
    ElementIntensity: PByte;
  begin
    RootNode := TX3DRootNode.Create;

    if ElementsIntensityTex <> nil then
      ElementIntensity := ElementsIntensityTex.Pixels;

    for I := 0 to Elements.Count - 1 do
    begin
      Disk := TDisk2DNode.CreateWithTransform(DiskShape, DiskTransform);
      { Area = Pi * Radius^2, so Radius := Sqrt(Area/Pi) }
      Disk.OuterRadius := Sqrt(Elements.List^[I].Area / Pi);
      Disk.Solid := false;

      if ElementsIntensityTex <> nil then
      begin
        Mat := TUnlitMaterialNode.Create;
        Mat.EmissiveColor := Vector3(
          ElementIntensity^ / 255,
          ElementIntensity^ / 255,
          ElementIntensity^ / 255
        );
        DiskShape.Material := Mat;
        Inc(ElementIntensity);
      end;

      DiskTransform.Translation := Elements.List^[I].Position;
      DiskTransform.Rotation := OrientationFromDirectionUp(
        Elements.List^[I].Normal,
        AnyOrthogonalVector(Elements.List^[I].Normal)
      );
      RootNode.AddChildren(DiskTransform);
    end;

    SceneElements.Load(RootNode, true);
  end;

  procedure SetDrawType(const NewDrawType: TDrawType);
  begin
    if DrawType <> NewDrawType then
    begin
      DrawType := NewDrawType;
      Scene.Exists := DrawType in [dtNormal, dtPass1, dtPass2];
      SceneElements.Exists := DrawType in [dtElements, dtElementsIntensity];
      Window.Invalidate;

      case DrawType of
        dtNormal:
          RemoveSceneColors(Scene); // clear colors, otherwise dtPass1/2 would leave their colors
        dtElements:
          UpdateElements(nil);
        dtElementsIntensity:
          UpdateElements(FullRenderIntensityTex);
      end;
    end;
  end;

begin
  case Item.IntData of
    100: SetDrawType(dtNormal);
    101: SetDrawType(dtElements);
    102: SetDrawType(dtElementsIntensity);
    103: SetDrawType(dtPass1);
    104: SetDrawType(dtPass2);

    150: Scene.TimePlaying := not Scene.TimePlaying;

    200: Window.Close;
    else Exit;
  end;
end;

var
  ModelURL: string =
    //'castle-data:/chinchilla_awakens.x3dv';
    'castle-data:/peach.wrl.gz';
begin
  ApplicationProperties.ApplicationName := 'dynamic_ambient_occlusion';
  InitializeLog;

  Window := TCastleWindow.Create(Application);

  Elements := TAOElementList.Create;

  Parameters.CheckHighAtMost(1);
  if Parameters.High = 1 then
    ModelURL := Parameters[1];

  try
    { inititalize Viewport }
    Viewport := TMyViewport.Create(Application);
    Viewport.FullSize := true;
    Viewport.InsertBack(TCastleExamineNavigation.Create(Application));
    Window.Controls.InsertFront(Viewport);

    { initialize Scene }
    Scene := TCastleScene.Create(Application);
    Scene.Load(ModelURL);
    Scene.PreciseCollisions := true;
    Scene.OnGeometryChanged := @THelper(nil).SceneGeometryChanged;
    Scene.ProcessEvents := true; { allow Scene animation }
    CalculateElements;
    Viewport.Items.Add(Scene);

    // headlight
    Viewport.Camera.Add(TCastleDirectionalLight.Create(Application));

    // nice initial camera position
    Viewport.AssignDefaultCamera;

    { initialize SceneElements }
    SceneElements := TCastleScene.Create(Application);
    Viewport.Items.Add(SceneElements);

    Window.MainMenu := CreateMainMenu;
    Window.OnMenuClick := @MenuClick;

    Window.OnOpen := @Open;
    Window.OnClose := @Close;
    Window.OnUpdate := @Update;
    Window.SetDemoOptions(keyF11, CharEscape, true);
    Window.OpenAndRun;
  finally
    FreeAndNil(Elements);
    FreeAndNil(ElementsPositionAreaTex);
    FreeAndNil(ElementsNormalTex);
    FreeAndNil(FullRenderIntensityTex);
  end;
end.
