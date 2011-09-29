{
  Copyright 2006-2010 Michalis Kamburelis.

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

{$apptype GUI}

uses Cameras, Surfaces, CastleWindow, GL, GLU, VectorMath,
  CastleGLUtils, Curve, BezierCurve, Boxes3D, SysUtils, CastleUtils, KeysMouse,
  CastleStringUtils, Math, CastleMessages, CastleFilesUtils,
  BFNT_BitstreamVeraSans_Unit, OpenGLBmpFonts;

type
  TShow = (shNone, shWire, shFill);

var
  Window: TCastleWindowCustom;
  Camera: TExamineCamera;

  Surface: TSurface;
  CurrentCurve, CurrentPoint: Integer;
  SurfaceShow: TShow = shFill;
  ControlPointsShow: TShow = shWire;

  SurfaceXSegments: Cardinal = 16;
  SurfaceYSegments: Cardinal = 16;

  SurfaceFileName: string = '';

  { True when dragging (between right mouse down and up). Other operations
    than mouse up may also cancel the dragging to keep assumptions below:

    - When CurrentCurve, CurrentPoint change Dragging stops.

      So CurrentCurve, CurrentPoint will always stay constant while
      Dragging = true.
      And always CurrentCurve, CurrentPoint <> -1 when Dragging. }
  Dragging: boolean = false;

  ProjectionPerspective: boolean = true;

  StatusFont: TGLBitmapFont;

{ various utility funcs ------------------------------------------------------ }

function ControlPoints(CurveNum: Integer): TVector3SingleList;
begin
  Result := (Surface.Curves.Items[CurveNum] as TControlPointsCurve).
    ControlPoints;
end;

procedure SetSurfaceFileName(const Value: string);
var
  S: string;
begin
  SurfaceFileName := Value;
  if Value <> '' then
    S := ExtractFileName(Value) else
    S := 'Unsaved surface';
  S := S + ' - design Bezier surface';

  { This may be called from SurfaceNew when Fps.Active is not yet set.
    In this case it's best to set Window.Caption instead of Window.FpsBaseCaption.
    We shouldn't set here Window.FpsBaseCaption, because Window.FpsBaseCaption
    will be initialized later (when Fps.Active is set true) from Window.Caption... }

  if Window.Fps.Active then
    Window.FpsBaseCaption := S else
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
    MyCurve := TRationalBezierCurve.Create(Surface.XBegin, Surface.XEnd);
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

  SetSurfaceFileName('');
end;

procedure SurfaceLoad(const FileName: string);
var
  CurvesCount, CurveControlPointsCount: Cardinal;
  F: TextFile;
  I, J: Integer;
  MyCurve: TRationalBezierCurve;
  V: TVector3Single;
  NewSurface: TSurface;
begin
  try
    NewSurface := TSurface.Create(0, 1, 0, 1);

    SafeReset(F, FileName, true);
    try
      Readln(F, CurvesCount, CurveControlPointsCount);
      for I := 0 to CurvesCount - 1 do
      begin
        MyCurve := TRationalBezierCurve.Create(NewSurface.XBegin, NewSurface.XEnd);
        for J := 0 to CurveControlPointsCount - 1 do
        begin
          Read(F, V[0], V[1], V[2]);
          MyCurve.ControlPoints.Add(V);
          MyCurve.Weights.Add(1.0);
        end;
        Readln(F);
        MyCurve.UpdateControlPoints;
        NewSurface.Curves.Add(MyCurve);
      end;
    finally CloseFile(F) end;
  except
    on E: Exception do
    begin
      FreeAndNil(NewSurface);
      MessageOK(Window, 'Error while loading file "' + FileName +'" : ' + E.Message);
      Exit;
    end;
  end;

  { Only now, when loading succeeded, we dare to modify global variables.
    This means that in case of unavoidable errors (like not existing file
    or bad file format) existing surface data will be preserved, }
  FreeAndNil(Surface);
  Surface := NewSurface;
  SetSurfaceFileName(FileName);
end;

procedure SurfaceSave(const FileName: string);
var
  CurveControlPointsCount: Cardinal;
  F: TextFile;
  I, J: Integer;
  V: TVector3Single;
begin
  CurveControlPointsCount := ControlPoints(0).Count;

  SafeRewrite(F, FileName);
  try
    Writeln(F, Surface.Curves.Count, ' ', CurveControlPointsCount);
    for I := 0 to Surface.Curves.Count - 1 do
    begin
      Assert(CurveControlPointsCount = Cardinal(ControlPoints(I).Count));
      for J := 0 to CurveControlPointsCount - 1 do
      begin
        V := ControlPoints(I).L[J];
        Write(F, V[0], ' ', V[1], ' ', V[2], ' ');
      end;
      Writeln(F);
    end;
  finally CloseFile(F) end;

  SetSurfaceFileName(FileName);
end;

{ CastleWindow callbacks --------------------------------------------------------- }

procedure DrawStatus(Data: Pointer);
begin
  glLoadIdentity;
  glColorv(Yellow3Single);
  StatusFont.PrintStrings(
    [ Format('Surface X segments: %d', [SurfaceXSegments]),
      Format('Surface Y segments: %d', [SurfaceYSegments]) ],
    5, 10, 10);
end;

procedure Draw(Window: TCastleWindowBase);
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadMatrix(Camera.Matrix);

  case SurfaceShow of
    shNone: ;
    shWire:
      begin
        glColorv(White3Single);
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
          glColorv(White3Single);
          Surface.Render(SurfaceXSegments, SurfaceYSegments);
        glPopAttrib;
      end;
  end;

  case ControlPointsShow of
    shNone: ;
    shWire:
      begin
        glColorv(Red3Single);
        glPushAttrib(GL_POLYGON_BIT or GL_LIGHTING_BIT);
          glPolygonMode(GL_FRONT_AND_BACK, GL_LINE); { saved by GL_POLYGON_BIT }
          glDisable(GL_LIGHTING); { saved by GL_LIGHTING_BIT }
          Surface.RenderControlPoints;
        glPopAttrib;
      end;
    shFill:
      begin
        glColorv(Red3Single);
        glPushAttrib(GL_LIGHTING_BIT);
          glShadeModel(GL_FLAT); { saved by GL_LIGHTING_BIT }
          Surface.RenderControlPoints;
        glPopAttrib;
      end;
  end;

  if (CurrentCurve <> - 1) and (CurrentPoint <> -1) then
  begin
    glColorv(Green3Single);
    glPushAttrib(GL_ENABLE_BIT);
      glDisable(GL_LIGHTING); { saved by GL_ENABLE_BIT }
      glDisable(GL_DEPTH_TEST);
      glBegin(GL_POINTS);
        glVertexv(ControlPoints(CurrentCurve).L[CurrentPoint]);
      glEnd;
    glPopAttrib;
  end;

  glPushAttrib(GL_ENABLE_BIT);
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_LIGHTING);
    glProjectionPushPopOrtho2D(@DrawStatus, nil, 0, Window.Width, 0, Window.Height);
  glPopAttrib;
end;

procedure Open(Window: TCastleWindowBase);
begin
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glEnable(GL_COLOR_MATERIAL);
  glPointSize(10);
  glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
  StatusFont := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans);
end;

procedure Close(Window: TCastleWindowBase);
begin
  FreeAndNil(StatusFont);
end;

procedure Resize(Window: TCastleWindowBase);
begin
  glViewport(0, 0, Window.Width, Window.Height);
  if ProjectionPerspective then
    ProjectionGLPerspective(30, Window.Width/Window.Height, 0.1, 100) else
    ProjectionGLOrtho(-1, 1, -1, 1, 0.1, 100);
end;

procedure Idle(Window: TCastleWindowBase);

  procedure Move(Coord, MoveDir: Integer);
  begin
    ControlPoints(CurrentCurve).L[CurrentPoint][Coord] +=
      MoveDir * Window.Fps.IdleSpeed * 50 * 0.01;
    (Surface.Curves[CurrentCurve] as TControlPointsCurve).UpdateControlPoints;
    Window.PostRedisplay;
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

procedure MouseDown(Window: TCastleWindowBase; Btn: TMouseButton);

  procedure SelectClosestControlPoint;
  var
    ModelMatrix, ProjMatrix: T16dArray;
    Viewport: TViewPortArray;

    procedure Project(const V: TVector3Single; out WinX, WinY: TGLdouble);
    var
      WinZ: TGLdouble;
    begin
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
    glGetDoublev(GL_PROJECTION_MATRIX, @ProjMatrix);
    glGetIntegerv(GL_VIEWPORT, @Viewport);

    BestDistance := MaxSingle;
    for I := 0 to Surface.Curves.Count - 1 do
      for J := 0 to ControlPoints(I).Count - 1 do
      begin
        Project(ControlPoints(I).L[J], WinX, WinY);
        Distance := Sqr(WinX - Window.MouseX) +
                    Sqr(WinY - (Window.Height - Window.MouseY));
        if Distance < BestDistance then
        begin
          BestCurve := I;
          BestPoint := J;
          BestDistance := Distance;
        end;
      end;

    CurrentCurve := BestCurve;
    CurrentPoint := BestPoint;
    Window.PostRedisplay;
    Dragging := false;
  end;

begin
  case Btn of
    mbLeft:
      SelectClosestControlPoint;
    mbRight:
      if (CurrentCurve <> -1) and (CurrentPoint <> -1) then
        Dragging := true;
  end;
end;

procedure MouseUp(Window: TCastleWindowBase; Btn: TMouseButton);
begin
  case Btn of
    mbRight: Dragging := false;
  end;
end;

procedure MouseMove(Window: TCastleWindowBase; NewX, NewY: integer);
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
    glGetDoublev(GL_PROJECTION_MATRIX, @ProjMatrix);
    glGetIntegerv(GL_VIEWPORT, @Viewport);

    { My first try was to just take
        UnProjectGL(NewX        , Window.Height - NewY        , 0),
        UnProjectGL(Window.MouseX, Window.Height - Window.MouseY, 0)
      I.e. we can just set WinZ parameter of UnProjectGL to anything,
      it's only important to make it the same.

      This was correct assumption
      --- but only for orthographic projection. You can see that it works
      by replacing GLPerspective inside Resize to
        ProjectionGLOrtho(-1, 1, -1, 1, 0.1, 100);

      For perspective projection, we have to carefully calculate WinZ,
      since the distance of the point from the camera determines
      how screen 2D distance corresponds to real space 3D.
      How ? gluProject does just that.

      (My first idea was just to calculate
      WinZ := MatrixMultPoint(Camera.Matrix,
        ControlPoints(CurrentCurve).L[CurrentPoint])[2];
      but that's bad: WinZ value for gluUnProject is not the actual
      distance from the camera. It's expressed in the 0..1 range of
      depth buffer). }
    WinZ := ProjectToZ(ControlPoints(CurrentCurve).L[CurrentPoint]);

    Move := Vector3Single(VectorSubtract(
      UnProject(NewX        , Window.Height - NewY        , WinZ),
      UnProject(Window.MouseX, Window.Height - Window.MouseY, WinZ)));
    VectorAddTo1st(ControlPoints(CurrentCurve).L[CurrentPoint], Move);
    (Surface.Curves[CurrentCurve] as TControlPointsCurve).UpdateControlPoints;
    Window.PostRedisplay;
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
  Window.PostRedisplay;

  { Once I thought that I should turn here Dragging off,
    since the selected point moved (and Camera.Matrix changed...)
    But, thanks to the fact that we actually move the point only
    inside MouseMove (not e.g. in MouseUp), everything works OK.
  Dragging := false;
  }
end;

{ menu ------------------------------------------------------------ }

procedure MenuCommand(Window: TCastleWindowBase; MenuItem: TMenuItem);

  procedure New;
  var
    CurvesCount, CurvesControlPointsCount: Cardinal;
  begin
    CurvesCount := Surface.Curves.Count;
    CurvesControlPointsCount :=
      (Surface.Curves[0] as TControlPointsCurve).ControlPoints.Count;
    if not MessageInputQueryCardinal(Window,
      'Y size (number of curves, min 2) :',
      CurvesCount, taLeft) then
      Exit;
    if not MessageInputQueryCardinal(Window,
      'X size (number of control points on each curve, min 2) :',
      CurvesControlPointsCount, taLeft) then
      Exit;
    SurfaceNew(CurvesCount, CurvesControlPointsCount);
  end;

  procedure Open;
  var
    S: string;
  begin
    S := SurfaceFileName;
    if Window.FileDialog('Open surface file', S, true) then
      SurfaceLoad(S);
  end;

  procedure Save;
  var
    S: string;
  begin
    S := SurfaceFileName;
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
    80: begin
          ProjectionPerspective := not ProjectionPerspective;
          Window.EventResize;
        end;
    100: SurfaceShow := shNone;
    101: SurfaceShow := shWire;
    102: SurfaceShow := shFill;
    else Exit;
  end;
  Window.PostRedisplay;
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
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemChecked.Create(
      'Perspective projection', 80, CtrlP, ProjectionPerspective, true));
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
  Window := TCastleWindowCustom.Create(Application);

  Window.OnMenuCommand := @MenuCommand;
  Window.MainMenu := CreateMainMenu;

  Camera := TExamineCamera.Create(Window);
  Camera.OnVisibleChange := @Dummy.VisibleChange;
  Camera.Init(Box3D(Vector3Single(0, 0, -1),
                    Vector3Single(1, 1,  1)), 0.1);
  { conflicts with our MouseDown / MouseMove }
  Camera.MouseNavigation := false;
  Camera.Input_StopRotating.MouseButtonUse := false;
  Window.Controls.Add(Camera);

  Window.OnOpen := @Open;
  Window.OnClose := @Close;
  Window.OnResize := @Resize;
  Window.OnIdle := @Idle;
  Window.OnMouseDown := @MouseDown;
  Window.OnMouseUp := @MouseUp;
  Window.OnMouseMove := @MouseMove;
  Window.OnDraw := @Draw;
  Window.SetDemoOptions(K_F11, CharEscape, true);

  SurfaceNew(4, 4);
  try
    Window.OpenAndRun;
  finally FreeAndNil(Surface) end;
end.