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

{ Main view, where most of the application logic takes place. }
unit GameViewMain;

{$ifndef FPC}
  {$pointermath on}
  {$warn COMPARING_SIGNED_UNSIGNED off}
{$endif}

interface

uses Classes,
  {$ifdef OpenGLES}
    CastleGLES,
  {$else}
    {$ifdef FPC} GL, GLExt, {$else} OpenGL, OpenGLext, {$endif}
  {$endif}
  CastleVectors, CastleComponentSerialize, CastleViewport,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleGLUtils,
  CastleTransform, CastleShapes, CastleUtils, CastleScene, CastleGLShaders,
  CastleImages, CastleColors, CastleSceneCore, CastleWindow;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    CheckboxPlayAnimation: TCastleCheckbox;
    ButtonDrawDisable: TCastleButton;
    ButtonDrawElements: TCastleButton;
    ButtonDrawElementsIntensity: TCastleButton;
    ButtonDrawPass1: TCastleButton;
    ButtonDrawPass2: TCastleButton;
 private
    type
      TMyViewport = class(TCastleViewport)
      public
        View: TViewMain;
        RectVbo: TGLuint;
        RectVao: TVertexArrayObject;
        procedure RenderFromView3D(const Params: TRenderParams); override;
      end;

      TShapeInfo = record
        Shape: TShape;
        CoordToElement: array of Integer;
      end;
      PShapeInfo = ^TShapeInfo;

      TAOElement = record
        Area: Single;
        { Position and Normal are in *world* coordinates (not local!) }
        Position, Normal: TVector3;
      end;
      PAOElement = ^TAOElement;
      TAOElementList = {$ifdef FPC}specialize{$endif} TStructList<TAOElement>;

      TElementsCalculator = class
      public
        CoordIndex: TInt32List;
        Coord: TVector3List;
        ShapeElements: PAOElement;

        procedure Polygon(const Indexes: array of Cardinal);
      end;

      TDrawType = (dtDisable, dtElements, dtElementsIntensity, dtPass1, dtPass2);

    var
      Scene: TCastleScene;
      SceneElements: TCastleScene;
      Viewport: TMyViewport;
      GLSLProgram: array [0..1] of TGLSLProgram;
      DrawType: TDrawType;
      ShadowScale: Single;

      Elements: TAOElementList;
      AoShapes: array of TShapeInfo;

      FullRenderShape: TShape;
      FullRenderShapeInfo: PShapeInfo;
      FullRenderIntensityTex: TGrayscaleImage;

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
      Verbose: Boolean; //< always left as false now, set to true in Start if needed to debug

    function VertexColor(
      const Shape: TShape;
      const VertexPosition: TVector3;
      const VertexIndex: Integer): TCastleColorRGB;
    procedure SceneGeometryChanged(AScene: TCastleSceneCore;
      const SomeLocalGeometryChanged: boolean;
      OnlyShapeChanged: TShape);

    { Storing and calculating "elements" for dynamic ambient occlusion algorithm. }
    procedure CalculateElements;
    procedure CalculateElementsTex;

    procedure SetDrawType(const NewDrawType: TDrawType);

    procedure CheckboxPlayAnimationChange(Sender: TObject);
    procedure ButtonDrawDisableClick(Sender: TObject);
    procedure ButtonDrawElementsClick(Sender: TObject);
    procedure ButtonDrawElementsIntensityClick(Sender: TObject);
    procedure ButtonDrawPass1Click(Sender: TObject);
    procedure ButtonDrawPass2Click(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, Math,
  CastleTriangles, CastleClassUtils, CastleParameters,
  CastleFilesUtils, CastleStringUtils,
  X3DFields, X3DNodes, CastleGLImages, CastleMessages, CastleLog,
  CastleGLVersion, CastleRectangles, CastleApplicationProperties,
  CastleRenderContext, CastleCameras,
  SceneUtilities;

{ TMyViewport ---------------------------------------------------------------- }

procedure TViewMain.TMyViewport.RenderFromView3D(const Params: TRenderParams);

  function CaptureAORect(SizePower2: boolean): TGrayscaleImage;
  var
    TexHeight: Cardinal;
  begin
    if SizePower2 then
      TexHeight := View.ElementsTexSize else
      { If size doesn't have to be power of 2 (e.g. if this is grabbed
        only for CPU), then height may be smaller, which is a good thing
        --- we don't have to grab so much pixels from color buffer. }
      TexHeight := DivRoundUp(Cardinal(View.Elements.Count), View.ElementsTexSize);
    Result := SaveScreen_NoFlush(TGrayscaleImage,
      Rectangle(0, 0, View.ElementsTexSize, TexHeight),
      Application.MainWindow.SaveScreenBuffer) as
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
    var
      Prog: TGLSLProgram;
    begin
      Prog := View.GLSLProgram[Pass];
      Prog.Enable;

      glActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_2D, View.GLElementsPositionAreaTex);
      Prog.SetUniform('tex_elements_position_area', 0);

      glActiveTexture(GL_TEXTURE1);
      glBindTexture(GL_TEXTURE_2D, View.GLElementsNormalTex);
      Prog.SetUniform('tex_elements_normal', 1);

      if Pass = 1 then
      begin
        { GLElementsIntensityTex is already bound to GL_TEXTURE2 }
        Prog.SetUniform('tex_elements_intensity', 2);
      end;

      Prog.SetUniform('area_scale', View.AreaScale);
      Prog.SetUniform('position_scale', View.PositionScale);
      Prog.SetUniform('position_shift', View.PositionShift);

      Prog.SetUniform('shadow_scale', View.ShadowScale);

      { Render rectangle with each pixel corresponding to one element
        that needs intensity calculated. }
      RenderRect(FloatRectangle(
        0,
        0,
        View.ElementsTexSize,
        DivRoundUp(Cardinal(View.Elements.Count), View.ElementsTexSize)
      ));

      Prog.Disable;
    end;

  begin
    DoRender(0);

    if View.DrawType = dtPass2 then
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
      glBindTexture(GL_TEXTURE_2D, View.GLElementsIntensityTex);
      glReadBuffer(GL_BACK);
      glCopyTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, 0, 0,
        View.ElementsTexSize, View.ElementsTexSize);

      DoRender(1);
    end;
  end;

begin
  if View.DrawType in [dtPass1, dtPass2] then
  begin
    FreeAndNil(View.FullRenderIntensityTex); // free previous FullRenderIntensityTex

    RenderAORect;
    View.FullRenderIntensityTex := CaptureAORect(false);
    View.FullRenderShape := nil;
    SetSceneColors(View.Scene, {$ifdef FPC}@{$endif} View.VertexColor);
  end;
  inherited;
end;

{ TElementsCalculator -------------------------------------------------------- }

procedure TViewMain.TElementsCalculator.Polygon(
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

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;

  procedure CreateShaders;
  var
    FragmentShader, VertexShader: String;
  begin
    if not GLFeatures.Shaders then
      raise Exception.Create('This GPU cannot handle dynamic ambient occlusion. GLSL shaders not supported.');
    if GLFeatures.EnableFixedFunction then
      raise Exception.Create('This GPU cannot handle dynamic ambient occlusion. Color.mode="MODULATE" does not work in fixed-function pipeline.');
    if not GLFeatures.VertexBufferObject then
      raise Exception.Create('This GPU cannot handle dynamic ambient occlusion. We use direct OpenGL(ES) calls here that use VBO.');
    if (Elements.Count = 0) or Scene.BoundingBox.IsEmpty then
      raise Exception.Create('Scene has no elements, or empty bounding box --- we cannot do dynamic ambient occlusion.');
    if GLVersion.Fglrx then
      raise Exception.Create('This GPU cannot handle dynamic ambient occlusion. FGLRX implementation of GLSL had so many bugs that supporting it for this demo was very burdensome.');

    CalculateElementsTex;

    FragmentShader := String(FileToString('castle-data:/shaders/dynamic_ambient_occlusion.fs'));
    VertexShader := String(FileToString('castle-data:/shaders/dynamic_ambient_occlusion.vs'));

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

var
  ModelURL: String;
begin
  inherited;

  DrawType := dtPass2;
  ButtonDrawPass2.Pressed := true;

  { Math correctly, this should be 1 / (2 * Pi)
    (= inverted hemisphere surface area).
    In practice, larger values are useful, to make shadows look more "dramatic". }
  ShadowScale := 1 / (1 * Pi);

  Elements := TAOElementList.Create;

  Parameters.CheckHighAtMost(1);
  if Parameters.High = 1 then
    ModelURL := Parameters[1]
  else
    ModelURL :=
      //'castle-data:/chinchilla_awakens.x3dv';
      //'castle-data:/simplico.wrl';
      //'castle-data:/ultra_simplico_human.wrl';
      'castle-data:/peach.wrl.gz';

  { inititalize Viewport }
  Viewport := TMyViewport.Create(Application);
  Viewport.View := Self;
  Viewport.FullSize := true;
  Viewport.InsertBack(TCastleExamineNavigation.Create(Application));
  InsertBack(Viewport);

  { initialize Scene }
  Scene := TCastleScene.Create(Application);
  Scene.Load(ModelURL);
  Scene.PreciseCollisions := true;
  Scene.OnGeometryChanged := {$ifdef FPC}@{$endif} SceneGeometryChanged;
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

  CreateShaders;

  CheckboxPlayAnimation.OnChange := {$ifdef FPC}@{$endif} CheckboxPlayAnimationChange;
  ButtonDrawDisable.OnClick := {$ifdef FPC}@{$endif} ButtonDrawDisableClick;
  ButtonDrawElements.OnClick := {$ifdef FPC}@{$endif} ButtonDrawElementsClick;
  ButtonDrawElementsIntensity.OnClick := {$ifdef FPC}@{$endif} ButtonDrawElementsIntensityClick;
  ButtonDrawPass1.OnClick := {$ifdef FPC}@{$endif} ButtonDrawPass1Click;
  ButtonDrawPass2.OnClick := {$ifdef FPC}@{$endif} ButtonDrawPass2Click;
end;

procedure TViewMain.Stop;

  procedure DestroyShaders;
  begin
    FreeAndNil(GLSLProgram[0]);
    FreeAndNil(GLSLProgram[1]);
  end;

begin
  FreeAndNil(Elements);
  FreeAndNil(ElementsPositionAreaTex);
  FreeAndNil(ElementsNormalTex);
  FreeAndNil(FullRenderIntensityTex);
  inherited;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;

  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  if Container.Pressed.Characters['s'] then
  begin
    ShadowScale := ShadowScale * (Power(1.1, SecondsPassed * 20));
    Container.Invalidate;
  end;
  if Container.Pressed.Characters['S'] then
  begin
    ShadowScale := ShadowScale * (Power(1/1.1, SecondsPassed * 20));
    Container.Invalidate;
  end;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { This virtual method is executed when user presses
    a key, a mouse button, or touches a touch-screen.

    Note that each UI control has also events like OnPress and OnClick.
    These events can be used to handle the "press", if it should do something
    specific when used in that UI control.
    The TViewMain.Press method should be used to handle keys
    not handled in children controls.
  }

  // Use this to handle keys:
  {
  if Event.IsKey(keyXxx) then
  begin
    // DoSomething;
    Exit(true); // key was handled
  end;
  }
end;

procedure TViewMain.CalculateElements;

  procedure AddShapeElements(ShapeIndex: Integer; Shape: TShape);
  var
    Coord: TMFVec3f;
    Calculator: TElementsCalculator;
    ShapeElementIndex: Cardinal;
    ShapeElements: PAOElement;
    I: Integer;
  begin
    AoShapes[ShapeIndex].Shape := Shape;

    if Shape.Geometry.InternalCoord(Shape.State, Coord) and (Coord <> nil) then
    begin
      { Grow Elements array }
      ShapeElementIndex := Elements.Count;
      Elements.Count := Elements.Count + Coord.Count;
      ShapeElements := PAOElement(Elements.Ptr(ShapeElementIndex));

      SetLength(AoShapes[ShapeIndex].CoordToElement, Coord.Count);

      { This does a modified and tweaked version of normal calculation
        from CreateSmoothNormalsCoordinateNode from CastleInternalNormals.
        We want to additionally store Position and calculate Area for
        each element (= just vertex). }

      { Initialize: set position,
        clear normals and area at the beginning, we will sum to them. }
      for I := 0 to Coord.Count - 1 do
      begin
        ShapeElements[I].Position :=
          Shape.State.Transformation.Transform.MultPoint(Coord.Items.L[I]);
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
        Shape.Geometry.InternalCoordPolygons(Shape.State,
          {$ifdef FPC}@{$endif} Calculator.Polygon);
      finally FreeAndNil(Calculator) end;

      { Normalize all new normals }
      for I := 0 to Coord.Count - 1 do
        ShapeElements[I].Normal := ShapeElements[I].Normal.Normalize;
    end else
    begin
      SetLength(AoShapes[ShapeIndex].CoordToElement, 0);
    end;
  end;

var
  ShapeList: TShapeList;
  I, GoodElementsCount, ShapeIndex, ShapeCoord: Integer;
begin
  Elements.Count := 0;

  SetLength(AoShapes, Scene.Shapes.ShapesCount(true, true, false));

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

    By the way, you also have to initialize AoShapes[].CoordToElement.
    This provides a mapping from original (per-shape, and before this removal)
    coords back to elements. }

  GoodElementsCount := 0;

  { increase ShapeIndex, ShapeCoord to next coord }
  ShapeIndex := 0;
  ShapeCoord := 0;
  while (ShapeIndex < Length(AoShapes)) and
        (Length(AoShapes[ShapeIndex].CoordToElement) = 0) do
    Inc(ShapeIndex);

  for I := 0 to Elements.Count - 1 do
  begin
    if not Elements.L[I].Normal.IsPerfectlyZero then
    begin
      { Then Element I should be on position GoodElementsCount. }
      if GoodElementsCount <> I then
        Elements.L[GoodElementsCount] := Elements.L[I];
      AoShapes[ShapeIndex].CoordToElement[ShapeCoord] := GoodElementsCount;
      Inc(GoodElementsCount);
    end else
    begin
      AoShapes[ShapeIndex].CoordToElement[ShapeCoord] := -1;
    end;

    { increase ShapeIndex, ShapeCoord to next coord }
    Inc(ShapeCoord);
    if ShapeCoord >= Length(AoShapes[ShapeIndex].CoordToElement) then
    begin
      Inc(ShapeIndex);
      ShapeCoord := 0;
      while (ShapeIndex < Length(AoShapes)) and
            (Length(AoShapes[ShapeIndex].CoordToElement) = 0) do
        Inc(ShapeIndex);
    end;
  end;

  Assert(ShapeIndex = Length(AoShapes));

  if Verbose then
    WritelnLog('Bad elements (vertexes with no neighbors) removed: %d, remaining good elements: %d', [
      Elements.Count - GoodElementsCount,
      GoodElementsCount
    ]);
  Elements.Count := GoodElementsCount;
end;

procedure TViewMain.CalculateElementsTex;

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

  if Verbose then
    WritelnLog('For elements %d we use texture size %d^2 = %d pixels', [
      Elements.Count,
      ElementsTexSize,
      Sqr(ElementsTexSize)
    ]);

  { calculate maximum area, which is just AreaScale }
  AreaScale := 0;
  for I := 0 to Elements.Count - 1 do
    MaxVar(AreaScale, Elements.L[I].Area);

  { calculate PositionScale, PositionShift.
    We have min/max in Scene.BoundingBox. }
  PositionScale := Scene.BoundingBox.Size;
  for I := 0 to 2 do
  begin
    if PositionScale.Data[I] = 0 then
      PositionScale.Data[I] := 1;
    PositionShift.Data[I] := Scene.BoundingBox.Data[0][I] / PositionScale[I];
  end;

  if Verbose then
  begin
    WritelnLog('To squeeze area into texture we use area_scale = %f', [AreaScale]);
    WritelnLog('To squeeze positions into texture we use scale = %s and shift %s (bbox is %s)', [
      PositionScale.ToString,
      PositionShift.ToString,
      Scene.BoundingBox.ToString
    ]);
  end;

  { initialize textures }
  FreeAndNil(ElementsPositionAreaTex);
  FreeAndNil(ElementsNormalTex);

  ElementsPositionAreaTex := TRGBAlphaImage.Create(ElementsTexSize, ElementsTexSize);
  ElementsNormalTex := TRGBImage.Create(ElementsTexSize, ElementsTexSize);

  { fill textures }
  PositionArea := ElementsPositionAreaTex.Pixels;
  Normal := ElementsNormalTex.Pixels;
  Element := PAOElement(Elements.L);
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
    // Inc(Element); // pointless, Element is not used further
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

function TViewMain.VertexColor(
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
    for I := 0 to Length(AoShapes) - 1 do
      if AoShapes[I].Shape = Shape then
      begin
        FullRenderShapeInfo := @AoShapes[I];
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

procedure TViewMain.SceneGeometryChanged(AScene: TCastleSceneCore;
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

procedure TViewMain.ButtonDrawDisableClick(Sender: TObject);
begin
  SetDrawType(dtDisable);
end;

procedure TViewMain.ButtonDrawElementsClick(Sender: TObject);
begin
  SetDrawType(dtElements);
end;

procedure TViewMain.ButtonDrawElementsIntensityClick(Sender: TObject);
begin
  SetDrawType(dtElementsIntensity);
end;

procedure TViewMain.ButtonDrawPass1Click(Sender: TObject);
begin
  SetDrawType(dtPass1);
end;

procedure TViewMain.ButtonDrawPass2Click(Sender: TObject);
begin
  SetDrawType(dtPass2);
end;

procedure TViewMain.CheckboxPlayAnimationChange(Sender: TObject);
begin
  Scene.TimePlaying := CheckboxPlayAnimation.Checked;
end;

procedure TViewMain.SetDrawType(const NewDrawType: TDrawType);

  { Update SceneElements contents.

    If ElementsIntensityTex = nil,
    then all element discs will have the same material.
    Otherwise, we will assign a color for each element,
    using values from ElementsIntensityTex. }
  procedure UpdateElements(const ElementsIntensityTex: TGrayscaleImage);
  var
    RootNode: TX3DRootNode;
    Mat: TUnlitMaterialNode;
    Appearance: TAppearanceNode;
    Disk: TDisk2DNode;
    DiskShape: TShapeNode;
    DiskTransform: TTransformNode;
    I: Integer;
    ElementIntensity: PByte;
  begin
    RootNode := TX3DRootNode.Create;

    if ElementsIntensityTex <> nil then
      ElementIntensity := ElementsIntensityTex.Pixels
    else
      ElementIntensity := nil; // silence Delphi warning

    for I := 0 to Elements.Count - 1 do
    begin
      Disk := TDisk2DNode.CreateWithTransform(DiskShape, DiskTransform);
      { Area = Pi * Radius^2, so Radius := Sqrt(Area/Pi) }
      Disk.OuterRadius := Sqrt(Elements.L[I].Area / Pi);
      Disk.Solid := false;

      if ElementsIntensityTex <> nil then
      begin
        Mat := TUnlitMaterialNode.Create;
        Mat.EmissiveColor := Vector3(
          ElementIntensity^ / 255,
          ElementIntensity^ / 255,
          ElementIntensity^ / 255
        );
        Appearance := TAppearanceNode.Create;
        Appearance.Material := Mat;
        DiskShape.Appearance := Appearance;
        Inc(ElementIntensity);
      end;

      DiskTransform.Translation := Elements.L[I].Position;
      DiskTransform.Rotation := OrientationFromDirectionUp(
        Elements.L[I].Normal,
        AnyOrthogonalVector(Elements.L[I].Normal)
      );
      RootNode.AddChildren(DiskTransform);
    end;

    SceneElements.Load(RootNode, true);
  end;

begin
  if DrawType <> NewDrawType then
  begin
    DrawType := NewDrawType;
    Scene.Exists := DrawType in [dtDisable, dtPass1, dtPass2];
    SceneElements.Exists := DrawType in [dtElements, dtElementsIntensity];
    Container.Invalidate;

    case DrawType of
      dtDisable:
        RemoveSceneColors(Scene); // clear colors, otherwise dtPass1/2 would leave their colors
      dtElements:
        UpdateElements(nil);
      dtElementsIntensity:
        UpdateElements(FullRenderIntensityTex);
    end;
  end;

  ButtonDrawDisable.Pressed := DrawType = dtDisable;
  ButtonDrawElements.Pressed := DrawType = dtElements;
  ButtonDrawElementsIntensity.Pressed := DrawType = dtElementsIntensity;
  ButtonDrawPass1.Pressed := DrawType = dtPass1;
  ButtonDrawPass2.Pressed := DrawType = dtPass2;
end;

end.
