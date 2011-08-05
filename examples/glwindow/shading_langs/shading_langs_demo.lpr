{
  Copyright 2007-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Demo of using OpenGL shading languages (ARB vertex / fragment programs
  and high-level GLSL). All the OpenGL calls to handle these shaders
  are actually moved to separate unit, GLShaders, so if you want to see
  how to initialize OpenGL shaders --- see there.
}
program shading_langs_demo;

uses GLWindow, KambiGLUtils, KambiParameters,
  Base3D, Frustum, KambiSceneManager, Boxes3D, SysUtils, KambiUtils, VectorMath,
  KambiStringUtils, GLImages, GL, GLU, GLExt, GLShaders;

var
  Tex: array [0..1] of TGLuint;

  VertexProgram: TARBVertexProgram;
  FragmentProgram: TARBFragmentProgram;
  GLSLProgram: TGLSLProgram;

{ geometry ------------------------------------------------------------------- }

type
  TMyGeometry = class(T3D)
  public
    procedure Render(const Frustum: TFrustum; const Params: TRenderParams); override;
    function BoundingBox: TBox3D; override;
  end;

procedure TMyGeometry.Render(const Frustum: TFrustum; const Params: TRenderParams);

  { Draw cube using really old-fashioned approach (no vertex arrays,
    just specify by hand 6 quads, 4 vertexes and tex coords each). }
  procedure DrawCube;

    procedure TexCoord(const X, Y: Single);
    begin
      glTexCoord2f(X, Y);
    end;

  begin
    glBegin(GL_QUADS);
      glNormal3f( 0.0, 0.0, 1.0);
      TexCoord(0.0, 0.0); glVertex3f(-1.0, -1.0,  1.0);
      TexCoord(1.0, 0.0); glVertex3f( 1.0, -1.0,  1.0);
      TexCoord(1.0, 1.0); glVertex3f( 1.0,  1.0,  1.0);
      TexCoord(0.0, 1.0); glVertex3f(-1.0,  1.0,  1.0);

      glNormal3f( 0.0, 0.0,-1.0);
      TexCoord(1.0, 0.0); glVertex3f(-1.0, -1.0, -1.0);
      TexCoord(1.0, 1.0); glVertex3f(-1.0,  1.0, -1.0);
      TexCoord(0.0, 1.0); glVertex3f( 1.0,  1.0, -1.0);
      TexCoord(0.0, 0.0); glVertex3f( 1.0, -1.0, -1.0);

      glNormal3f( 0.0, 1.0, 0.0);
      TexCoord(0.0, 1.0); glVertex3f(-1.0,  1.0, -1.0);
      TexCoord(0.0, 0.0); glVertex3f(-1.0,  1.0,  1.0);
      TexCoord(1.0, 0.0); glVertex3f( 1.0,  1.0,  1.0);
      TexCoord(1.0, 1.0); glVertex3f( 1.0,  1.0, -1.0);

      glNormal3f( 0.0,-1.0, 0.0);
      TexCoord(1.0, 1.0); glVertex3f(-1.0, -1.0, -1.0);
      TexCoord(0.0, 1.0); glVertex3f( 1.0, -1.0, -1.0);
      TexCoord(0.0, 0.0); glVertex3f( 1.0, -1.0,  1.0);
      TexCoord(1.0, 0.0); glVertex3f(-1.0, -1.0,  1.0);

      glNormal3f( 1.0, 0.0, 0.0);
      TexCoord(1.0, 0.0); glVertex3f( 1.0, -1.0, -1.0);
      TexCoord(1.0, 1.0); glVertex3f( 1.0,  1.0, -1.0);
      TexCoord(0.0, 1.0); glVertex3f( 1.0,  1.0,  1.0);
      TexCoord(0.0, 0.0); glVertex3f( 1.0, -1.0,  1.0);

      glNormal3f(-1.0, 0.0, 0.0);
      TexCoord(0.0, 0.0); glVertex3f(-1.0, -1.0, -1.0);
      TexCoord(1.0, 0.0); glVertex3f(-1.0, -1.0,  1.0);
      TexCoord(1.0, 1.0); glVertex3f(-1.0,  1.0,  1.0);
      TexCoord(0.0, 1.0); glVertex3f(-1.0,  1.0, -1.0);
    glEnd();
  end;

  procedure DrawCubes;
  var
    Q: PGLUQuadric;
  begin
    glPushMatrix;
      glTranslatef(-6, 0, 0);
      glBindTexture(GL_TEXTURE_2D, Tex[0]);
      glEnable(GL_TEXTURE_2D);

      DrawCube;
    glPopMatrix;

    glPushMatrix;
      glTranslatef(-2, 0, 0);
      glBindTexture(GL_TEXTURE_2D, Tex[1]);
      glEnable(GL_TEXTURE_2D);

      DrawCube;
    glPopMatrix;

    glPushMatrix;
      glTranslatef(+2, 0, 0);
      glDisable(GL_TEXTURE_2D);
      DrawCube;
    glPopMatrix;

    glPushMatrix;
      glTranslatef(+6, 0, 0);
      glRotatef(-90, 1, 0, 0);
      Q := NewGLUQuadric(true, GLU_SMOOTH, GLU_OUTSIDE, GLU_FILL);
      try
        gluSphere(Q, 1, 20, 20);
      finally gluDeleteQuadric(Q) end;
    glPopMatrix;
  end;

begin
  if not (Params.TransparentGroup in [tgAll, tgOpaque]) then Exit;

  { headlight }
  glPushMatrix;
    glLoadIdentity;
    glLightv(GL_LIGHT0, GL_POSITION, Vector4Single(0, 0, 1));
  glPopMatrix;

  glMaterialv(GL_FRONT_AND_BACK, GL_AMBIENT, Vector4Single(1, 1, 0, 1));
  glMaterialv(GL_FRONT_AND_BACK, GL_DIFFUSE, Vector4Single(1, 1, 0, 1));
  glMaterialv(GL_FRONT_AND_BACK, GL_SPECULAR, Vector4Single(1, 1, 1, 1));
  glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 20.0);

  DrawCubes;

  glTranslatef(0, 3, 0);
  if VertexProgram.Support <> gsNone then
  begin
    VertexProgram.Enable;
    DrawCubes;
    VertexProgram.Disable;
  end;

  glTranslatef(0, 3, 0);
  if FragmentProgram.Support <> gsNone then
  begin
    FragmentProgram.Enable;
    DrawCubes;
    FragmentProgram.Disable;
  end;

  glTranslatef(0, 3, 0);
  if (VertexProgram.Support <> gsNone) and
     (FragmentProgram.Support <> gsNone) then
  begin
    FragmentProgram.Enable;
    VertexProgram.Enable;
    DrawCubes;
    FragmentProgram.Disable;
    VertexProgram.Disable;
  end;

  glTranslatef(0, 3, 0);
  if GLSLProgram.Support <> gsNone then
  begin
    GLSLProgram.Enable;
    DrawCubes;
    GLSLProgram.Disable;
  end;
end;

function TMyGeometry.BoundingBox: TBox3D;
const
  SceneBoundingBox: TBox3D =
  ( (-8, -3, -3),
    ( 8,  15,  3) );
begin
  Result := SceneBoundingBox;
end;

{ glw callbacks -------------------------------------------------------------- }

procedure Open(Window: TGLWindow);

  function LoadTexture(const FileName: string): TGLuint;
  begin
    Result := LoadGLTexture(FileName, GL_LINEAR_MIPMAP_LINEAR, GL_LINEAR, Texture2DClampToEdge);
  end;

const
  GLSLProgramBaseName =
  //'glsl_simple'
  'glsl_toon_shading'
  //'glsl_phong_shading'
  ;
begin
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glEnable(GL_DEPTH_TEST);

  { This is to 0, just because vertex_program_simulate_conventional.txt
    doesn't simulate global ambient, so it would look different. }
  glLightModelv(GL_LIGHT_MODEL_AMBIENT, Vector4Single(0.0, 0.0, 0.0, 1.0));

  glClearColor(0.3, 0.3, 0.3, 1);

  { initialize vertex program }
  VertexProgram := TARBVertexProgram.Create;
  VertexProgram.Load(FileToString('vertex_program_simulate_conventional.txt'));
  Writeln(VertexProgram.DebugInfo);

  { initialize fragment program }
  FragmentProgram := TARBFragmentProgram.Create;
  FragmentProgram.Load(FileToString('fragment_program_simulate_conventional.txt'));
  Writeln(FragmentProgram.DebugInfo);

  { initialize GLSL program }
  GLSLProgram := TGLSLProgram.Create;
  GLSLProgram.AttachVertexShader(FileToString(GLSLProgramBaseName + '.vs'));
  GLSLProgram.AttachFragmentShader(FileToString(GLSLProgramBaseName + '.fs'));
  { For this test program, we eventually allow shader to run in software }
  GLSLProgram.Link(false);
  Writeln(GLSLProgram.DebugInfo);

  Tex[0] := LoadTexture('../../../../castle/data/textures/023pierres.jpg');
  Tex[1] := LoadTexture('../../../../castle/data/textures/bridgerock512side.jpg');
end;

procedure Close(Window: TGLWindow);
begin
  FreeAndNil(VertexProgram);
  FreeAndNil(FragmentProgram);
  FreeAndNil(GLSLProgram);
end;

{ menu ----------------------------------------------------------------------- }

procedure MenuCommand(Window: TGLWindow; MenuItem: TMenuItem);

  procedure LoadVertexProgramMenu;
  var
    S: string;
  begin
    S := '';
    if Window.FileDialog('Open file with vertex program', S, true) then
    begin
      VertexProgram.Load(FileToString(S));
      Writeln(VertexProgram.DebugInfo);
    end;
  end;

  procedure LoadFragmentProgramMenu;
  var
    S: string;
  begin
    S := '';
    if Window.FileDialog('Open file with fragment program', S, true) then
    begin
      FragmentProgram.Load(FileToString(S));
      Writeln(FragmentProgram.DebugInfo);
    end;
  end;

  procedure LoadGLSLProgramMenu;
  const
    GLSL_FileFilters =
    'All files|*|' +
    '*GLSL vertex of fragment shader (*.vs, *.fs)|*.vs;*.fs';
  var
    S: string;
  begin
    S := '';
    if Window.FileDialog('Open file with GLSL vertex or fragment shader', S, true,
      GLSL_FileFilters) then
    begin
      GLSLProgram.DetachAllShaders;
      GLSLProgram.AttachVertexShader(FileToString(ChangeFileExt(S,'.vs')));
      GLSLProgram.AttachFragmentShader(FileToString(ChangeFileExt(S,'.fs')));
      { For this test program, we eventually allow shader to run in software }
      GLSLProgram.Link(false);
      Writeln(GLSLProgram.DebugInfo);
    end;
  end;

begin
  case MenuItem.IntData of
    10: LoadVertexProgramMenu;
    20: LoadFragmentProgramMenu;
    30: LoadGLSLProgramMenu;
    200: Window.Close;
  end;
end;

function CreateMainMenu: TMenu;
var
  M: TMenu;
begin
  Result := TMenu.Create('Main menu');
  M := TMenu.Create('_Program');
    M.Append(TMenuItem.Create('Load new ARB vertex program', 10));
    M.Append(TMenuItem.Create('Load new ARB fragment program', 20));
    M.Append(TMenuItem.Create('Load new GLSL program (vertex shader must have *.vs extension, fragment *.fs)', 30));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Exit', 200));
    Result.Append(M);
end;

{ main program --------------------------------------------------------------- }

var
  Window: TGLUIWindow;
  SceneManager: TKamSceneManager;
begin
  Window := TGLUIWindow.Create(Application);

  SceneManager := TKamSceneManager.Create(Application);
  Window.Controls.Add(SceneManager);
  SceneManager.Items.Add(TMyGeometry.Create(Application));

  { parse params }
  Window.ParseParameters(StandardParseOptions);
  Parameters.CheckHigh(0);

  Window.MainMenu := CreateMainMenu;
  Window.OnMenuCommand := @MenuCommand;

  Window.OnOpen := @Open;
  Window.OnClose := @Close;
  Window.OpenAndRun;
end.
