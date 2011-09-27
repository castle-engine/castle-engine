{
  Copyright 2008-2010 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Show Spherical harmonics function basis.
  Single SH function is shown on a sphere, yellow indicates positive values,
  blue negative values.

  Navigate with mouse or keyboard (like view3dscene in Examine mode).
}

program show_sh;

uses VectorMath, Boxes3D, GL, GLWindow, Base3D, UIControls,
  KambiClassUtils, KambiUtils, SysUtils, KambiFilesUtils,
  KambiGLUtils, Cameras, Math, SphereSampling, SphericalHarmonics,
  BFNT_BitstreamVeraSans_Unit, OpenGLBmpFonts, KambiSceneManager,
  KambiStringUtils;

var
  Window: TGLUIWindow;

  LM: Cardinal = 0;
  Font: TGLBitmapFont;

  MinSHValue, MaxSHValue: Float;

procedure Draw2D(Glwin: TGLWindow);
var
  L: Cardinal;
  M: Integer;
begin
  LMDecode(LM, L, M);
  glLoadIdentity;
  glColor3f(1, 1, 0);
  glRasterPos2i(10, 10);
  Font.Print(Format('Spherical harmonic number %d. (L, M) = (%d, %d). Resuls in range (%f, %f)',
    [LM, L, M, MinSHValue, MaxSHValue]));
end;

type
  TMySceneManager = class(TKamSceneManager)
  protected
    procedure Render3D(const Params: TRenderParams); override;
    procedure ApplyProjection; override;
  end;

var
  SceneManager: TMySceneManager;

procedure TMySceneManager.Render3D(const Params: TRenderParams);

  { This was brutally copied from vrmlnodes_triangulating.inc
    (Sphere_LocalTriangulate), then adjusted. See there for comments. }

  procedure RenderSphere;
  const
    RadiusValue = 1;

    QUADRIC_SLICES = 60;
    QUADRIC_STACKS = 60;

    procedure Vertex(const V: TVector3Single);
    var
      SH: Float;
    begin
      SH := SHBasis(LM, XYZToPhiTheta(V));
      if SH > MaxSHValue then MaxSHValue := SH;
      if SH < MinSHValue then MinSHValue := SH;

      if SH >= 0 then
        glColor3f(SH, SH, 0) else
        glColor3f(0, 0, -SH);

      glVertexv(V);
    end;

{$define LOOP_OVER_CIRCLE_DECLARE:=
var LOC_AngleRad: Single;
    LOC_LastPt, LOC_Pt: TVector3Single;
    LOC_i: integer;}

{$define LOOP_OVER_CIRCLE:=
begin
 LOC_LastPt := Vector3Single(0, LOOP_OVER_CIRCLE_HEIGHT, LOOP_OVER_CIRCLE_RADIUS);
 for LOC_i := 1 to QUADRIC_SLICES-1 do
 begin
  LOC_AngleRad:=(LOC_i/QUADRIC_SLICES)*2*Pi;
  LOC_Pt := Vector3Single(Sin(LOC_AngleRad)*LOOP_OVER_CIRCLE_RADIUS,
                        LOOP_OVER_CIRCLE_HEIGHT,
                        Cos(LOC_AngleRad)*LOOP_OVER_CIRCLE_RADIUS);
  LOOP_OVER_CIRCLE_PROC(LOC_LastPt, LOC_Pt);
  LOC_LastPt := LOC_Pt;
 end;

 LOC_Pt := Vector3Single(0, LOOP_OVER_CIRCLE_HEIGHT, LOOP_OVER_CIRCLE_RADIUS);
 LOOP_OVER_CIRCLE_PROC(LOC_LastPt, LOC_Pt);
end;}

    procedure StackHigherLine(StackNum: integer; out Radius, Height: Single);
    var
      alfa, s, c: Float;
    begin
      alfa := Pi * StackNum / QUADRIC_STACKS;
      SinCos(alfa, s, c);
      Radius := s * RadiusValue;
      Height := -c * RadiusValue;
    end;

  var
    BottomPt, TopPt: TVector3Single;
    BottomAndTopStackRadius, BottomHeight, TopHeight :Single;

    procedure LOOP_OVER_CIRCLE_PROC(LastPt, Pt: TVector3Single);
    begin
      Vertex(LastPt);
      Vertex(Pt);
      Vertex(BottomPt);

      Pt[1] := TopHeight;
      LastPt[1] := TopHeight;

      Vertex(LastPt);
      Vertex(Pt);
      Vertex(TopPt);
    end;

  var
    AngleRad, LowRadius, HighRadius, LowHeight, HighHeight: Single;
    HighLastPt, HighPt, LowLastPt, LowPt: TVector3Single;
    i, StackNum: integer;
    SinAngleRad, CosAngleRad: Float;
  LOOP_OVER_CIRCLE_DECLARE
  begin
    BottomPt := Vector3Single(0, -RadiusValue, 0);
    TopPt := Vector3Single(0, RadiusValue, 0);
    StackHigherLine(1, BottomAndTopStackRadius, BottomHeight);
    TopHeight := -BottomHeight;

    glBegin(GL_TRIANGLES);
      {$define LOOP_OVER_CIRCLE_RADIUS := BottomAndTopStackRadius}
      {$define LOOP_OVER_CIRCLE_HEIGHT := BottomHeight}
      LOOP_OVER_CIRCLE
    glEnd;

    glBegin(GL_QUADS);

      for StackNum := 2 to QUADRIC_STACKS-1 do
      begin
        StackHigherLine(StackNum-1, LowRadius, LowHeight);
        StackHigherLine(StackNum, HighRadius, HighHeight);

        LowLastPt := Vector3Single(0, LowHeight, LowRadius);
        HighLastPt := Vector3Single(0, HighHeight, HighRadius);

        for i := 1 to QUADRIC_SLICES do
        begin
          AngleRad := (i/QUADRIC_SLICES)*2*Pi;
          SinCos(AngleRad, SinAngleRad, CosAngleRad);

          LowPt  := Vector3Single(SinAngleRad*LowRadius , LowHeight , CosAngleRad*LowRadius );
          HighPt := Vector3Single(SinAngleRad*HighRadius, HighHeight, CosAngleRad*HighRadius);

          Vertex(LowLastPt);
          Vertex(LowPt);
          Vertex(HighPt);
          Vertex(HighLastPt);

          LowLastPt := LowPt;
          HighLastPt := HighPt;
        end;
      end;

    glEnd;
  end;

begin
  inherited;
  if not Params.Transparent then
  begin
    MinSHValue :=  MaxFloat;
    MaxSHValue := -MaxFloat;
    RenderSphere;
  end;
end;

procedure TMySceneManager.ApplyProjection;
begin
  glViewport(0, 0, ContainerWidth, ContainerHeight);
  ProjectionGLPerspective(45.0, ContainerWidth/ContainerHeight, 0.01, 10);
end;

procedure Open(Glwin: TGLWindow);
begin
  glEnable(GL_DEPTH_TEST);
  Font := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans);
end;

procedure Close(Glwin: TGLWindow);
begin
  FreeAndNil(Font);
end;

procedure MenuCommand(Glwin: TGLWindow; Item: TMenuItem);
begin
  case Item.IntData of
    10: LM := ChangeIntCycle(LM, -1, MaxSHBasis - 1);
    20: LM := ChangeIntCycle(LM, +1, MaxSHBasis - 1);
    else Exit;
  end;
  Window.PostRedisplay;
end;

var
  M: TMenu;
begin
  Window := TGLUIWindow.Create(Application);

  SceneManager := TMySceneManager.Create(Application);
  Window.Controls.Add(SceneManager);

  Window.MainMenu := TMenu.Create('Main menu');
  M := TMenu.Create('_Program');
    M.Append(TMenuItem.Create('_Previous basis', 10, 'p'));
    M.Append(TMenuItem.Create('_Next basis', 20, 'n'));
    Window.MainMenu.Append(M);

  SceneManager.Camera := TExamineCamera.Create(Window);
  (SceneManager.Camera as TExamineCamera).Init(Box3D(
    Vector3Single(-1, -1, -1),
    Vector3Single( 1,  1,  1)), 0.1);

  Window.OnOpen := @Open;
  Window.OnClose := @Close;
  Window.OnMenuCommand := @MenuCommand;
  Window.OnDrawStyle := ds2D;
  Window.SetDemoOptions(K_F11, CharEscape, true);
  Window.OpenAndRun(ProgramName, @Draw2D);
end.
