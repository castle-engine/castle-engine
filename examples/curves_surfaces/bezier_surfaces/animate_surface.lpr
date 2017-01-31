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

{ Animate between two Bezier surfaces.
  Test on sample_data/*.animation files. }
program animate_surface;

{$I castleconf.inc}

uses CastleCameras, Surfaces, CastleWindow, CastleGL, CastleVectors,
  CastleGLUtils, CastleCurves, CastleBoxes, SysUtils, CastleUtils, CastleKeysMouse,
  CastleStringUtils, CastleMessages, CastleFilesUtils, CastleParameters,
  CastleColors, Castle3D, CastleFrustum, CastleClassUtils;

var
  Window: TCastleWindow;
  Camera: TWalkCamera;
  Surface1, Surface2: TSurface;
  SurfacePos, SurfaceDir, SurfaceUp: TVector3Single;
  SurfaceMoveSpeed: Single;
  F: TGLfloat = 0.0;
  FUp: boolean = true;

procedure CameraHome;
begin
  Camera.Init(Box3D(Vector3Single(0, 0, -1),
                    Vector3Single(1, 1,  1)), Camera.Radius);
end;

procedure CameraScene;
begin
  Camera.Init(SurfacePos, SurfaceDir, SurfaceUp, SurfaceUp, 0, Camera.Radius);
  Camera.MoveSpeed := SurfaceMoveSpeed;
end;

procedure SurfacesLoad(const URL: string);
var
  N: Cardinal;
  F: TTextReader;

  procedure Load(Surface: TSurface);
  var
    I, J: Integer;
    MyCurve: TRationalBezierCurve;
    V: TVector3Single;
  begin
    for I := 0 to N - 1 do
    begin
      MyCurve := TRationalBezierCurve.Create(nil);
      MyCurve.TBegin := Surface.XBegin;
      MyCurve.TEnd := Surface.XEnd;
      for J := 0 to N - 1 do
      begin
        V := F.ReadVector3Single;
        MyCurve.ControlPoints.Add(V);
        MyCurve.Weights.Add(1.0);
      end;
      F.Readln;
      MyCurve.UpdateControlPoints;
      Surface.Curves.Add(MyCurve);
    end;
  end;

begin
  Surface1 := TSurface.Create(0, 1, 0, 1);
  Surface2 := TSurface.Create(0, 1, 0, 1);

  F := TTextReader.Create(URL);
  try
    SurfacePos := F.ReadVector3Single;
    SurfaceDir := F.ReadVector3Single;
    SurfaceUp := F.ReadVector3Single;
    { The lengths of our direction vectors express speed in old terms
      (1/50 of the second), rescale them here. }
    SurfaceMoveSpeed := VectorLen(SurfaceDir) * 50;
    NormalizeVar(SurfaceDir);
    N := F.ReadInteger;
    Load(Surface1);
    Load(Surface2);
  finally FreeAndNil(F) end;

  CameraScene;
end;

type
  TAnimatedCurve = class(T3D)
    function BoundingBox: TBox3D; override;
    procedure Render(const Frustum: TFrustum; const Params: TRenderParams); override;
  end;

function TAnimatedCurve.BoundingBox: TBox3D;
begin
  Result := Surface1.BoundingBox + Surface2.BoundingBox;
end;

procedure TAnimatedCurve.Render(const Frustum: TFrustum; const Params: TRenderParams);
const
  SurfaceXSegments = 20;
  SurfaceYSegments = 20;
var
  Surface: TSurface;
  I, J: Integer;
  C1, C2, MyCurve: TRationalBezierCurve;
begin
  if Params.Transparent or (not Params.ShadowVolumesReceivers) then Exit;

  glColorv(WhiteRGB);

  Surface := TSurface.Create(
    Surface1.XBegin, Surface1.XEnd,
    Surface1.YBegin, Surface1.YEnd);
  try
    for I := 0 to Surface1.Curves.Count - 1 do
    begin
      C1 := Surface1.Curves.Items[I] as TRationalBezierCurve;
      C2 := Surface2.Curves.Items[I] as TRationalBezierCurve;
      MyCurve := TRationalBezierCurve.Create(nil);
      MyCurve.TBegin := C1.TBegin;
      MyCurve.TEnd := C1.TEnd;
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

procedure Open(Container: TUIContainer);
begin
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glEnable(GL_COLOR_MATERIAL);
  glShadeModel(GL_FLAT);
end;

procedure Update(Container: TUIContainer);
begin
  if FUp then
  begin
    F += 0.01 * Window.Fps.UpdateSecondsPassed * 50;
    if F >= 1.0 then
    begin
      F := 1.0;
      FUp := false;
    end;
  end else
  begin
    F -= 0.01 * Window.Fps.UpdateSecondsPassed * 50;
    if F <= 0.0 then
    begin
      F := 0.0;
      FUp := true;
    end;
  end;
end;

procedure Press(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.EventType = itKey then
    case Event.KeyCharacter of
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

var
  URL: string = 'sample_data/sail.animation';
begin
  Window := TCastleWindow.Create(Application);

  Camera := TWalkCamera.Create(Window);
  Camera.PreferGravityUpForRotations := false;
  Camera.PreferGravityUpForMoving := false;
  Camera.Radius := 0.02;
  Window.SceneManager.Camera := Camera;

  Window.SceneManager.Items.Add(TAnimatedCurve.Create(Window));

  Parameters.CheckHighAtMost(1);
  if Parameters.High = 1 then
    URL := Parameters[1];

  SurfacesLoad(URL);
  try
    Window.OnOpen := @Open;
    Window.OnUpdate := @Update;
    Window.OnPress := @Press;
    Window.SetDemoOptions(K_F11, CharEscape, true);
    Window.AutoRedisplay := true;

    Window.OpenAndRun;
  finally
    FreeAndNil(Surface1);
    FreeAndNil(Surface2);
  end;
end.
