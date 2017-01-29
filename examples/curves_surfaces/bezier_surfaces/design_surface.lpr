{
  Copyright 2006-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Design Bezier surface.
  See README.
  Test on sample_data/*.surface files. }
program design_surface;

{$I castleconf.inc}
{$apptype GUI}

uses Classes, CastleCameras, Surfaces, CastleWindow, CastleGL, CastleVectors,
  CastleGLUtils, CastleCurves, CastleBoxes, SysUtils, CastleUtils, CastleKeysMouse,
  CastleStringUtils, CastleMessages, CastleFilesUtils, CastleUIControls,
  CastleColors, Castle3D, CastleControls,
  CastleFrustum, CastleURIUtils, CastleClassUtils, CastleParameters;

type
  TShow = (shNone, shWire, shFill);

var
  Window: TCastleWindow;
  Camera: TExamineCamera;

  Surface: TSurface;
  CurrentCurve, CurrentPoint: Integer;
  SurfaceShow: TShow = shFill;
  ControlPointsShow: TShow = shWire;

  SurfaceXSegments: Cardinal = 16;
  SurfaceYSegments: Cardinal = 16;

  SurfaceURL: string = '';

  { True when dragging (between right mouse down and up). Other operations
    than mouse up may also cancel the dragging to keep assumptions below:

    - When CurrentCurve, CurrentPoint change Dragging stops.

      So CurrentCurve, CurrentPoint will always stay constant while
      Dragging = true.
      And always CurrentCurve, CurrentPoint <> -1 when Dragging. }
  Dragging: boolean = false;

{ various utility funcs ------------------------------------------------------ }

function ControlPoints(CurveNum: Integer): TVector3SingleList;
begin
  Result := (Surface.Curves.Items[CurveNum] as TControlPointsCurve).
    ControlPoints;
end;

procedure SetSurfaceURL(const Value: string);
var
  S: string;
begin
  SurfaceURL := Value;
  if Value <> '' then
    S := URICaption(Value) else
    S := 'Unsaved surface';
  S := S + ' - design Bezier surface';
  Window.Caption := S;
end;

{ surface new/load/save ------------------------------------------------------ }

procedure SurfaceNew(const CurvesCount, CurveControlPointsCount: Cardinal);
var
  I, J: Integer;
  MyCurve: TRationalBezierCurve;
begin
  FreeAndNil(Surface);

  Surface := TSurface.Create(0, 1, 0, 1);

  for I := 0 to CurvesCount - 1 do
  begin
    MyCurve := TRationalBezierCurve.Create(nil);
    MyCurve.TBegin := Surface.XBegin;
    MyCurve.TEnd := Surface.XEnd;
    for J := 0 to CurveControlPointsCount - 1 do
    begin
      MyCurve.ControlPoints.Add(Vector3Single(
        J / (CurveControlPointsCount - 1),
        I / (CurvesCount - 1), 0));
      MyCurve.Weights.Add(1.0);
    end;
    MyCurve.UpdateControlPoints;
    Surface.Curves.Add(MyCurve);
  end;

  SetSurfaceURL('');
end;

procedure SurfaceLoad(const URL: string);
var
  CurvesCount, CurveControlPointsCount: Cardinal;
  F: TTextReader;
  I, J: Integer;
  MyCurve: TRationalBezierCurve;
  V: TVector3Single;
  NewSurface: TSurface;
begin
  try
    NewSurface := TSurface.Create(0, 1, 0, 1);

    F := TTextReader.Create(URL);
    try
      CurvesCount := F.ReadInteger;
      CurveControlPointsCount := F.ReadInteger;
      F.Readln;
      for I := 0 to CurvesCount - 1 do
      begin
        MyCurve := TRationalBezierCurve.Create(nil);
        MyCurve.TBegin := NewSurface.XBegin;
        MyCurve.TEnd := NewSurface.XEnd;
        for J := 0 to CurveControlPointsCount - 1 do
        begin
          V := F.ReadVector3Single;
          MyCurve.ControlPoints.Add(V);
          MyCurve.Weights.Add(1.0);
        end;
        F.Readln;
        MyCurve.UpdateControlPoints;
        NewSurface.Curves.Add(MyCurve);
      end;
    finally FreeAndNil(F) end;
  except
    on E: Exception do
    begin
      FreeAndNil(NewSurface);
      MessageOK(Window, 'Error while loading file "' + URL +'" : ' + E.Message);
      Exit;
    end;
  end;

  { Only now, when loading succeeded, we dare to modify global variables.
    This means that in case of unavoidable errors (like not existing file
    or bad file format) existing surface data will be preserved, }
  FreeAndNil(Surface);
  Surface := NewSurface;
  SetSurfaceURL(URL);
end;

procedure SurfaceSave(const URL: string);
var
  CurveControlPointsCount: Cardinal;
  F: TTextWriter;
  I, J: Integer;
  V: TVector3Single;
begin
  CurveControlPointsCount := ControlPoints(0).Count;

  F := TTextWriter.Create(URL);
  try
    F.Writeln('%d %d', [Surface.Curves.Count, CurveControlPointsCount]);
    for I := 0 to Surface.Curves.Count - 1 do
    begin
      Assert(CurveControlPointsCount = Cardinal(ControlPoints(I).Count));
      for J := 0 to CurveControlPointsCount - 1 do
      begin
        V := ControlPoints(I).List^[J];
        F.Write('%g %g %g ', [V[0], V[1], V[2]]);
      end;
      F.Writeln;
    end;
  finally FreeAndNil(F) end;

  SetSurfaceURL(URL);
end;

{ CastleWindow callbacks --------------------------------------------------------- }

procedure Render(Container: TUIContainer);
var
  Text: TStringList;
begin
  Text := TStringList.Create;
  try
    Text.Append(Format('Surface X segments: %d', [SurfaceXSegments]));
    Text.Append(Format('Surface Y segments: %d', [SurfaceYSegments]));
    UIFont.PrintStrings(10, 10, Yellow, Text, false, 2);
  finally FreeAndNil(Text) end;
end;

type
  TSurface3D = class(T3D)
    function BoundingBox: TBox3D; override;
    procedure Render(const Frustum: TFrustum; const Params: TRenderParams); override;
  end;

function TSurface3D.BoundingBox: TBox3D;
begin
  Result := Surface.BoundingBox;
end;

procedure TSurface3D.Render(const Frustum: TFrustum; const Params: TRenderParams);
begin
  if Params.Transparent or (not Params.ShadowVolumesReceivers) then Exit;

  glPushAttrib(GL_ENABLE_BIT or GL_LIGHTING_BIT);
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);
    glEnable(GL_COLOR_MATERIAL);
    RenderContext.PointSize := 10;
    glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);

    case SurfaceShow of
      shNone: ;
      shWire:
        begin
          glColorv(WhiteRGB);
          glPushAttrib(GL_POLYGON_BIT or GL_LIGHTING_BIT);
            glPolygonMode(GL_FRONT_AND_BACK, GL_LINE); { saved by GL_POLYGON_BIT }
            glDisable(GL_LIGHTING); { saved by GL_LIGHTING_BIT }
            Surface.Render(SurfaceXSegments, SurfaceYSegments);
          glPopAttrib;
        end;
      shFill:
        begin
          glPushAttrib(GL_LIGHTING_BIT);
            glShadeModel(GL_FLAT); { saved by GL_LIGHTING_BIT }
            glColorv(WhiteRGB);
            Surface.Render(SurfaceXSegments, SurfaceYSegments);
          glPopAttrib;
        end;
    end;

    case ControlPointsShow of
      shNone: ;
      shWire:
        begin
          glColorv(Red);
          glPushAttrib(GL_POLYGON_BIT or GL_LIGHTING_BIT);
            glPolygonMode(GL_FRONT_AND_BACK, GL_LINE); { saved by GL_POLYGON_BIT }
            glDisable(GL_LIGHTING); { saved by GL_LIGHTING_BIT }
            Surface.RenderControlPoints;
          glPopAttrib;
        end;
      shFill:
        begin
          glColorv(Red);
          glPushAttrib(GL_LIGHTING_BIT);
            glShadeModel(GL_FLAT); { saved by GL_LIGHTING_BIT }
            Surface.RenderControlPoints;
          glPopAttrib;
        end;
    end;

    if (CurrentCurve <> - 1) and (CurrentPoint <> -1) then
    begin
      glColorv(Green);
      glPushAttrib(GL_ENABLE_BIT);
        glDisable(GL_LIGHTING); { saved by GL_ENABLE_BIT }
        glDisable(GL_DEPTH_TEST);
        glBegin(GL_POINTS);
          glVertexv(ControlPoints(CurrentCurve).List^[CurrentPoint]);
        glEnd;
      glPopAttrib;
    end;

  glPopAttrib;
end;

procedure Update(Container: TUIContainer);

  procedure Move(Coord, MoveDir: Integer);
  begin
    ControlPoints(CurrentCurve).List^[CurrentPoint][Coord] +=
      MoveDir * Window.Fps.UpdateSecondsPassed * 50 * 0.01;
    (Surface.Curves[CurrentCurve] as TControlPointsCurve).UpdateControlPoints;
    Window.Invalidate;
  end;

begin
  if (CurrentCurve <> - 1) and (CurrentPoint <> -1) then
  begin
    if Window.Pressed.Characters['8'] then Move(1, +1);
    if Window.Pressed.Characters['2'] then Move(1, -1);
    if Window.Pressed.Characters['4'] then Move(0, -1);
    if Window.Pressed.Characters['6'] then Move(0, +1);
    if Window.Pressed.Characters['9'] then Move(2, +1);
    if Window.Pressed.Characters['3'] then Move(2, -1);
  end;
end;

procedure Press(Container: TUIContainer; const Event: TInputPressRelease);

  procedure SelectClosestControlPoint;
  var
    ModelMatrix, ProjMatrix: T16dArray;
    Viewport: TViewPortArray;

    procedure Project(const V: TVector3Single; out WinX, WinY: TGLdouble);
    var
      WinZ: TGLdouble;
      // ProjectResult: TVector3Single;
    begin
      { Implementing glProject by ourselves is quite trivial,
        just multiply by modelview, projection,
        go to clip space then to window space. See e.g.
        http://www.gamedev.net/topic/436455-gluproject-efficient/ }
      (*
      ProjectResult := Vector3SinglePoint(
        ProjectionMatrix * Camera.Matrix * Vector4Single(V, 1.0));
      { Map x, y and z to range 0-1 }
      ProjectResult[0] := ProjectResult[0] * 0.5 + 0.5;
      ProjectResult[1] := ProjectResult[1] * 0.5 + 0.5;
      ProjectResult[2] := ProjectResult[2] * 0.5 + 0.5;
      Writeln('Own projection 1 resulting in ' + VectorToNiceStr(ProjectResult));
      { Map x,y to viewport }
      ProjectResult[0] := ProjectResult[0] * Viewport[2] + Viewport[0];
      ProjectResult[1] := ProjectResult[1] * Viewport[3] + Viewport[1];
      *)

      Check( gluProject(V[0], V[1], V[2],
        ModelMatrix, ProjMatrix, Viewport,
        @WinX, @WinY, @WinZ) = GL_TRUE, 'gluProject');
    end;

  var
    BestCurve: Integer;
    BestPoint: Integer;
    BestDistance, Distance: TGLdouble;
    I, J: Integer;
    WinX, WinY: TGLdouble;
  begin
    { Prepare for a series of gluProject calls. }
    ModelMatrix := T16dArray(Matrix4Double(Camera.Matrix));
    ProjMatrix := T16dArray(Matrix4Double(Camera.ProjectionMatrix));
    { Note: do not use here
        glGetDoublev(GL_PROJECTION_MATRIX, @ProjMatrix);
      or (always equivalent)
        ProjMatrix := T16dArray(Matrix4Double(ProjectionMatrix));
      because the current projection matrix may be set for 2D rendering
      (for Render). Only the Camera.ProjectionMatrix is guaranteed now to
      contain projection for 3D world in SceneManager. }
    glGetIntegerv(GL_VIEWPORT, @Viewport);
    // Equivalent: Viewport := SceneManager.Rect converted to TViewPortArray

    BestDistance := MaxSingle;
    for I := 0 to Surface.Curves.Count - 1 do
      for J := 0 to ControlPoints(I).Count - 1 do
      begin
        Project(ControlPoints(I).List^[J], WinX, WinY);
        Distance := Sqr(WinX - Event.Position[0]) +
                    Sqr(WinY - Event.Position[1]);
        if Distance < BestDistance then
        begin
          BestCurve := I;
          BestPoint := J;
          BestDistance := Distance;
        end;
      end;

    CurrentCurve := BestCurve;
    CurrentPoint := BestPoint;
    Window.Invalidate;
    Dragging := false;
  end;

begin
  if Event.IsMouseButton(mbLeft) then
    SelectClosestControlPoint else
  if Event.IsMouseButton(mbRight) then
  begin
    if (CurrentCurve <> -1) and (CurrentPoint <> -1) then
      Dragging := true;
  end;
end;

procedure Release(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.IsMouseButton(mbRight) then
    Dragging := false;
end;

procedure Motion(Container: TUIContainer; const Event: TInputMotion);
var
  ModelMatrix, ProjMatrix: T16dArray;
  Viewport: TViewPortArray;

  function ProjectToZ(const V: TVector3Single): Single;
  var
    WinX, WinY, WinZ: TGLdouble;
  begin
    Check( gluProject(V[0], V[1], V[2],
      ModelMatrix, ProjMatrix, Viewport,
      @WinX, @WinY, @WinZ) = GL_TRUE, 'gluProject');
    Result := WinZ;
  end;

  function UnProject(const WinX, WinY, WinZ: TGLdouble): TVector3Double;
  begin
    Check( gluUnProject(Winx, Winy, Winz,
      ModelMatrix, ProjMatrix, Viewport,
      @Result[0], @Result[1], @Result[2]) = GL_TRUE, 'gluUnProject');
  end;

var
  Move: TVector3Single;
  WinZ: Single;
begin
  if Dragging then
  begin
    ModelMatrix := T16dArray(Matrix4Double(Camera.Matrix));
    ProjMatrix := T16dArray(Matrix4Double(Camera.ProjectionMatrix));
    glGetIntegerv(GL_VIEWPORT, @Viewport);

    { My first try was to just take
        UnProject(Event.   Position[0], Event.   Position[1], 0),
        UnProject(Event.OldPosition[0], Event.OldPosition[1], 0)
      That is, to set WinZ parameter of UnProject to an arbitrary value.

      This was correct assumption
      --- but only for orthographic projection. You can see that it works
      by replacing GLPerspective inside Resize to
        OrthoProjection(-1, 1, -1, 1, 0.1, 100);

      For perspective projection, we have to carefully calculate WinZ,
      since the distance of the point from the camera determines
      how screen 2D distance corresponds to real space 3D.
      How ? gluProject does just that.

      (My first idea was just to calculate
      WinZ := MatrixMultPoint(Camera.Matrix,
        ControlPoints(CurrentCurve).List^[CurrentPoint])[2];
      but that's bad: WinZ value for gluUnProject is not the actual
      distance from the camera. It's expressed in the 0..1 range of
      depth buffer). }
    WinZ := ProjectToZ(ControlPoints(CurrentCurve).List^[CurrentPoint]);

    Move := Vector3Single(VectorSubtract(
      UnProject(Event.   Position[0], Event.   Position[1], WinZ),
      UnProject(Event.OldPosition[0], Event.OldPosition[1], WinZ)));
    VectorAddVar(ControlPoints(CurrentCurve).List^[CurrentPoint], Move);
    (Surface.Curves[CurrentCurve] as TControlPointsCurve).UpdateControlPoints;
    Window.Invalidate;
  end;
end;

type
  TDummy = class
    procedure VisibleChange(ChangedCamera: TObject);
  end;

var
  Dummy: TDummy;

procedure TDummy.VisibleChange(ChangedCamera: TObject);
begin
  Window.Invalidate;

  { Once I thought that I should turn here Dragging off,
    since the selected point moved (and Camera.Matrix changed...)
    But, thanks to the fact that we actually move the point only
    inside Motion (not e.g. in MouseUp), everything works OK.
  Dragging := false;
  }
end;

{ menu ------------------------------------------------------------ }

procedure MenuClick(Container: TUIContainer; MenuItem: TMenuItem);

  procedure New;
  var
    CurvesCount, CurvesControlPointsCount: Cardinal;
  begin
    CurvesCount := Surface.Curves.Count;
    CurvesControlPointsCount :=
      (Surface.Curves[0] as TControlPointsCurve).ControlPoints.Count;
    if not MessageInputQueryCardinal(Window,
      'Y size (number of curves, min 2) :',
      CurvesCount) then
      Exit;
    if not MessageInputQueryCardinal(Window,
      'X size (number of control points on each curve, min 2) :',
      CurvesControlPointsCount) then
      Exit;
    SurfaceNew(CurvesCount, CurvesControlPointsCount);
  end;

  procedure Open;
  var
    S: string;
  begin
    S := SurfaceURL;
    if Window.FileDialog('Open surface file', S, true) then
      SurfaceLoad(S);
  end;

  procedure Save;
  var
    S: string;
  begin
    S := SurfaceURL;
    if Window.FileDialog('Save surface file', S, false) then
      SurfaceSave(S);
  end;

  procedure ChangeCurrentCurve(Change: Integer);
  var
    Max: Integer;
  begin
    Max := Surface.Curves.Count - 1;
    if not Between(CurrentCurve, 0, Max) then
      CurrentCurve := 0 else
      CurrentCurve := ChangeIntCycle(CurrentCurve, Change, Max);
    Dragging := false;
  end;

  procedure ChangeCurrentPoint(Change: Integer);
  var
    Max: Integer;
  begin
    Max := ControlPoints(CurrentCurve).Count - 1;
    if not Between(CurrentPoint, 0, Max) then
      CurrentPoint := 0 else
      CurrentPoint := ChangeIntCycle(CurrentPoint, Change, Max);
    Dragging := false;
  end;

  procedure SetSurfaceXSegments(const Value: Cardinal);
  begin
    SurfaceXSegments := Value;
  end;

  procedure SetSurfaceYSegments(const Value: Cardinal);
  begin
    SurfaceYSegments := Value;
  end;

begin
  case MenuItem.IntData of
    10: New;
    20: Open;
    30: Save;
    40: SetSurfaceXSegments(SurfaceXSegments * 2);
    41: SetSurfaceXSegments(SurfaceXSegments div 2);
    45: SetSurfaceYSegments(SurfaceYSegments * 2);
    46: SetSurfaceYSegments(SurfaceYSegments div 2);
    50: ControlPointsShow := shNone;
    51: ControlPointsShow := shWire;
    52: ControlPointsShow := shFill;
    60: ChangeCurrentCurve(-1);
    61: ChangeCurrentCurve(+1);
    70: ChangeCurrentPoint(-1);
    71: ChangeCurrentPoint(+1);
    100: SurfaceShow := shNone;
    101: SurfaceShow := shWire;
    102: SurfaceShow := shFill;
    else Exit;
  end;
  Window.Invalidate;
end;

function CreateMainMenu: TMenu;
var
  M: TMenu;
  Radio: TMenuItemRadio;
  RadioGroup: TMenuItemRadioGroup;
begin
  Result := TMenu.Create('Main menu');
  M := TMenu.Create('_File');
    M.Append(TMenuItem.Create('New ...', 10, CtrlN));
    M.Append(TMenuItem.Create('Open ...', 20, CtrlO));
    M.Append(TMenuItem.Create('Save ...', 30, CtrlS));
    Result.Append(M);
  M := TMenu.Create('_View');
    M.Append(TMenuItem.Create('Surface X segments x 2', 40, 'x'));
    M.Append(TMenuItem.Create('Surface X segments / 2', 41, 'X'));
    M.Append(TMenuItem.Create('Surface Y segments x 2', 45, 'y'));
    M.Append(TMenuItem.Create('Surface Y segments / 2', 46, 'Y'));
    M.Append(TMenuSeparator.Create);
    Radio := TMenuItemRadio.Create(
      'Don''t show surface', 100, SurfaceShow = shNone, true);
    RadioGroup := Radio.Group;
    M.Append(Radio);
    Radio := TMenuItemRadio.Create(
      'Show surface as wireframe', 101, SurfaceShow = shWire, true);
    Radio.Group := RadioGroup;
    M.Append(Radio);
    Radio := TMenuItemRadio.Create(
      'Show surface filled', 102, SurfaceShow = shFill, true);
    Radio.Group := RadioGroup;
    M.Append(Radio);
    M.Append(TMenuSeparator.Create);
    Radio := TMenuItemRadio.Create(
      'Don''t show control points', 50, ControlPointsShow = shNone, true);
    RadioGroup := Radio.Group;
    M.Append(Radio);
    Radio := TMenuItemRadio.Create(
      'Show control points as wireframe', 51, ControlPointsShow = shWire, true);
    Radio.Group := RadioGroup;
    M.Append(Radio);
    Radio := TMenuItemRadio.Create(
      'Show control points as surface', 52, ControlPointsShow = shFill, true);
    Radio.Group := RadioGroup;
    M.Append(Radio);
    Result.Append(M);
  M := TMenu.Create('_Select');
    M.Append(TMenuItem.Create('Select previous curve', 60, 's'));
    M.Append(TMenuItem.Create('Select next curve', 61, 'w'));
    M.Append(TMenuItem.Create('Select previous point', 70, 'a'));
    M.Append(TMenuItem.Create('Select next point', 71, 'd'));
    Result.Append(M);
end;

{ main ----------------------------------------------------------------------- }

begin
  Parameters.CheckHigh(0);
  Window := TCastleWindow.Create(Application);

  Window.OnMenuClick := @MenuClick;
  Window.MainMenu := CreateMainMenu;

  Camera := TExamineCamera.Create(Window);
  Camera.OnVisibleChange := @Dummy.VisibleChange;
  Camera.Init(Box3D(Vector3Single(0, 0, -1),
                    Vector3Single(1, 1,  1)), 0.2);
  { conflicts with our Press / Release }
  Camera.Input := Camera.Input - [ciMouseDragging];
  Camera.Input_StopRotating.MouseButtonUse := false;
  Window.SceneManager.Camera := Camera;

  Window.SceneManager.Items.Add(TSurface3D.Create(Window));

  Window.OnUpdate := @Update;
  Window.OnPress := @Press;
  Window.OnRelease := @Release;
  Window.OnMotion := @Motion;
  Window.OnRender := @Render;
  Window.SetDemoOptions(K_F11, CharEscape, true);

  SurfaceNew(4, 4);
  try
    Window.OpenAndRun;
  finally FreeAndNil(Surface) end;
end.
