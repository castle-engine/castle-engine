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

{ Animate between two Bezier surfaces.
  Test on sample_data/*.animation files. }
program animate_surface;

uses Cameras, Surfaces, CastleWindow, GL, GLU, VectorMath,
  CastleGLUtils, BezierCurve, Boxes3D, SysUtils, CastleUtils, KeysMouse,
  CastleStringUtils, CastleMessages, CastleFilesUtils, CastleParameters,
  CastleColors;

var
  Window: TCastleWindowCustom;
  Camera: TWalkCamera;
  Surface1, Surface2: TSurface;
  SurfacePos, SurfaceDir, SurfaceUp: TVector3Single;
  SurfaceMoveSpeed: Single;
  F: TGLfloat = 0.0;
  FUp: boolean = true;

procedure CameraHome;
begin
  Camera.Init(Box3D(Vector3Single(0, 0, -1),
                    Vector3Single(1, 1,  1)), 0.0);
end;

procedure CameraScene;
begin
  Camera.Init(SurfacePos, SurfaceDir, SurfaceUp, SurfaceUp, 0, 0);
  Camera.MoveSpeed := SurfaceMoveSpeed;
end;

procedure SurfacesLoad(const FileName: string);
var
  N: Cardinal;
  F: TextFile;

  procedure Load(Surface: TSurface);
  var
    I, J: Integer;
    MyCurve: TRationalBezierCurve;
    V: TVector3Single;
  begin
    for I := 0 to N - 1 do
    begin
      MyCurve := TRationalBezierCurve.Create(Surface.XBegin, Surface.XEnd);
      for J := 0 to N - 1 do
      begin
        Read(F, V[0], V[1], V[2]);
        MyCurve.ControlPoints.Add(V);
        MyCurve.Weights.Add(1.0);
      end;
      Readln(F);
      MyCurve.UpdateControlPoints;
      Surface.Curves.Add(MyCurve);
    end;
  end;

begin
  Surface1 := TSurface.Create(0, 1, 0, 1);
  Surface2 := TSurface.Create(0, 1, 0, 1);

  SafeReset(F, FileName, true);
  try
    Readln(F, SurfacePos[0], SurfacePos[1], SurfacePos[2],
              SurfaceDir[0], SurfaceDir[1], SurfaceDir[2],
              SurfaceUp [0], SurfaceUp [1], SurfaceUp [2]);
    { The lengths of our direction vectors express speed in old terms
      (1/50 of the second), rescale them here. }
    SurfaceMoveSpeed := VectorLen(SurfaceDir) * 50;
    NormalizeTo1st(SurfaceDir);
    Readln(F, N);
    Load(Surface1);
    Load(Surface2);
  finally CloseFile(F) end;

  CameraScene;
end;

procedure Draw(Window: TCastleWindowBase);
const
  SurfaceXSegments = 20;
  SurfaceYSegments = 20;
var
  Surface: TSurface;
  I, J: Integer;
  C1, C2, MyCurve: TRationalBezierCurve;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadMatrix(Camera.Matrix);

  glColorv(White3Single);

  Surface := TSurface.Create(
    Surface1.XBegin, Surface1.XEnd,
    Surface1.YBegin, Surface1.YEnd);
  try
    for I := 0 to Surface1.Curves.Count - 1 do
    begin
      C1 := Surface1.Curves.Items[I] as TRationalBezierCurve;
      C2 := Surface2.Curves.Items[I] as TRationalBezierCurve;
      MyCurve := TRationalBezierCurve.Create(C1.TBegin, C1.TEnd);
      for J := 0 to C1.ControlPoints.Count - 1 do
      begin
        MyCurve.ControlPoints.Add(
          Lerp(F, C1.ControlPoints.Items[J],
                  C2.ControlPoints.Items[J]));
        MyCurve.Weights.Add(1.0);
      end;
      MyCurve.UpdateControlPoints;
      Surface.Curves.Add(MyCurve);
    end;

    Surface.Render(SurfaceXSegments, SurfaceYSegments);
  finally FreeAndNil(Surface) end;
end;

procedure Open(Window: TCastleWindowBase);
begin
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glEnable(GL_COLOR_MATERIAL);
  glShadeModel(GL_FLAT);
end;

procedure Resize(Window: TCastleWindowBase);
begin
  glViewport(0, 0, Window.Width, Window.Height);
  ProjectionGLPerspective(30, Window.Width/Window.Height, 0.1, 100);
end;

procedure Idle(Window: TCastleWindowBase);
begin
  if FUp then
  begin
    F += 0.01 * Window.Fps.IdleSpeed * 50;
    if F >= 1.0 then
    begin
      F := 1.0;
      FUp := false;
    end;
  end else
  begin
    F -= 0.01 * Window.Fps.IdleSpeed * 50;
    if F <= 0.0 then
    begin
      F := 0.0;
      FUp := true;
    end;
  end;
end;

procedure KeyDown(Window: TCastleWindowBase; Key: TKey; C: char);
begin
  case C of
    'c': begin
           Writeln(Format('%f %f %f   %f %f %f   %f %f %f',
             [ Camera.Position[0],
               Camera.Position[1],
               Camera.Position[2],
               Camera.Direction[0],
               Camera.Direction[1],
               Camera.Direction[2],
               Camera.Up[0],
               Camera.Up[1],
               Camera.Up[2] ]));
         end;
    'h': CameraHome;
    's': CameraScene;
  end;
end;

begin
  Window := TCastleWindowCustom.Create(Application);

  Camera := TWalkCamera.Create(Window);
  Camera.PreferGravityUpForRotations := false;
  Camera.PreferGravityUpForMoving := false;
  Window.Controls.Add(Camera);

  Parameters.CheckHigh(1);
  SurfacesLoad(Parameters[1]);
  try
    Window.OnOpen := @Open;
    Window.OnResize := @Resize;
    Window.OnIdle := @Idle;
    Window.OnDraw := @Draw;
    Window.OnKeyDown := @KeyDown;
    Window.SetDemoOptions(K_F11, CharEscape, true);
    Window.AutoRedisplay := true;

    Window.OpenAndRun;
  finally
    FreeAndNil(Surface1);
    FreeAndNil(Surface2);
  end;
end.
