{ This is an OpenGL example using GL_EXT_fog_coord extension.
  Translated from C version, included as Mesa3D demo program
  in Mesa-6.5.2/progs/demos/fogcoord.c.
  I use my own GLWindow instead of glut and my own texture loading funcs.

  * Based on glutskel.c by Brian Paul
  * and NeHe's Volumetric fog tutorial!
  *
  * Daniel Borca
  *}

uses VectorMath, GLWindow, GL, GLU, GLExt, KambiGLUtils,
  KambiStringUtils, KambiUtils, GLImages;

{$I openglmac.inc}

{ use glDrawElements
  TODO: arrays version not translated yet. }
{ $define ARRAYS}

const
  DEPTH = 15.0;

type
  TFogCoordExtProc = procedure(coord: TGLfloat); OPENGL_CALL
  TFogCoordPointerExtProc =
    procedure(AType: TGLenum; stride: TGLsizei; p: Pointer); OPENGL_CALL

var
  Window: TGLWindowDemo;

  FogCoordExtProc: TFogCoordExtProc;
  FogCoordPointerExtProc: TFogCoordPointerExtProc;

var
  camz: TGLfloat;
  texture: TGLuint;

  fogMode: TGLint;
  fogCoord: boolean;
  fogDensity: TGLfloat  = 0.75;
  fogStart: TGLfloat  = 1.0;
  fogEnd: TGLfloat = 40.0;
  fogColor: TVector4Single = (0.6, 0.3, 0.0, 1.0);

procedure FogCoordExtProc_Nop(coord: TGLfloat); OPENGL_CALL
begin
end;

procedure SetFogMode(NewFogMode: TGLint);
begin
  FogMode := NewFogMode mod 4;

  case fogMode of
    0: begin
         glDisable(GL_FOG);
         Writeln('fog(disable)');
       end;
    1: begin
         glEnable(GL_FOG);
         glFogi(GL_FOG_MODE, GL_LINEAR);
         glFogf(GL_FOG_START, fogStart);
         glFogf(GL_FOG_END, fogEnd);
         Writeln('fog(GL_LINEAR, ', fogStart:1:2, ', ', fogEnd:1:2, ')');
       end;
    2: begin
         glEnable(GL_FOG);
         glFogi(GL_FOG_MODE, GL_EXP);
         glFogf(GL_FOG_DENSITY, fogDensity);
         Writeln('fog(GL_EXP, ', fogDensity:1:2, ')');
       end;
    3: begin
         glEnable(GL_FOG);
         glFogi(GL_FOG_MODE, GL_EXP2);
         glFogf(GL_FOG_DENSITY, fogDensity);
         Writeln('fog(GL_EXP2, ', fogDensity:1:2, ')');
       end;
  end;
end;

procedure SetFogCoord(NewFogCoord: boolean);
begin
  FogCoord := NewFogCoord;

  if GL_EXT_fog_coord then
  begin
    if FogCoord then
    begin
      glFogi(GL_FOG_COORDINATE_SOURCE_EXT, GL_FOG_COORDINATE_EXT);
      Writeln('fog(GL_FOG_COORDINATE_EXT)');
    end else
    begin
      glFogi(GL_FOG_COORDINATE_SOURCE_EXT, GL_FRAGMENT_DEPTH_EXT);
      Writeln('fog(GL_FRAGMENT_DEPTH_EXT)');
    end;
    FogCoordExtProc := glFogCoordfEXT;
  end else
  begin
    Writeln('EXT_fog_coord not available');
    FogCoordExtProc := @FogCoordExtProc_Nop;
  end;
end;

{$ifdef ARRAYS}(*
/* could reuse vertices */
static GLuint vertex_index[] = {
   /* Back */
   0, 1, 2, 3,

   /* Floor */
   4, 5, 6, 7,

   /* Roof */
   8, 9, 10, 11,

   /* Right */
   12, 13, 14, 15,

   /* Left */
   16, 17, 18, 19
};

static GLfloat vertex_pointer[][3] = {
   /* Back */
   {-2.5f,-2.5f,-DEPTH}, { 2.5f,-2.5f,-DEPTH}, { 2.5f, 2.5f,-DEPTH}, {-2.5f, 2.5f,-DEPTH},

   /* Floor */
   {-2.5f,-2.5f,-DEPTH}, { 2.5f,-2.5f,-DEPTH}, { 2.5f,-2.5f, DEPTH}, {-2.5f,-2.5f, DEPTH},

   /* Roof */
   {-2.5f, 2.5f,-DEPTH}, { 2.5f, 2.5f,-DEPTH}, { 2.5f, 2.5f, DEPTH}, {-2.5f, 2.5f, DEPTH},

   /* Right */
   { 2.5f,-2.5f, DEPTH}, { 2.5f, 2.5f, DEPTH}, { 2.5f, 2.5f,-DEPTH}, { 2.5f,-2.5f,-DEPTH},

   /* Left */
   {-2.5f,-2.5f, DEPTH}, {-2.5f, 2.5f, DEPTH}, {-2.5f, 2.5f,-DEPTH}, {-2.5f,-2.5f,-DEPTH}
};

static GLfloat texcoord_pointer[][2] = {
   /* Back */
   {0.0f, 0.0f}, {1.0f, 0.0f}, {1.0f, 1.0f}, {0.0f, 1.0f},

   /* Floor */
   {0.0f, 0.0f}, {1.0f, 0.0f}, {1.0f, 1.0f}, {0.0f, 1.0f},

   /* Roof */
   {0.0f, 0.0f}, {1.0f, 0.0f}, {1.0f, 1.0f}, {0.0f, 1.0f},

   /* Right */
   {0.0f, 0.0f}, {0.0f, 1.0f}, {1.0f, 1.0f}, {1.0f, 0.0f},

   /* Left */
   {0.0f, 0.0f}, {0.0f, 1.0f}, {1.0f, 1.0f}, {1.0f, 0.0f}
};

static GLfloat fogcoord_pointer[][1] = {
   /* Back */
   {1.0f}, {1.0f}, {1.0f}, {1.0f},

   /* Floor */
   {1.0f}, {1.0f}, {0.0f}, {0.0f},

   /* Roof */
   {1.0f}, {1.0f}, {0.0f}, {0.0f},

   /* Right */
   {0.0f}, {0.0f}, {1.0f}, {1.0f},

   /* Left */
   {0.0f}, {0.0f}, {1.0f}, {1.0f}
};*)
{$endif}

procedure Draw(Window: TGLWindow);
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;

  glTranslatef(0.0, 0.0, camz);

{$ifdef ARRAYS}
   glDrawElements(GL_QUADS, sizeof(vertex_index) / sizeof(vertex_index[0]), GL_UNSIGNED_INT, vertex_index);
{$else}
   { Back }
   glBegin(GL_QUADS);
   FogCoordExtProc(1.0); glTexCoord2f(0.0, 0.0); glVertex3f(-2.5,-2.5,-DEPTH);
   FogCoordExtProc(1.0); glTexCoord2f(1.0, 0.0); glVertex3f( 2.5,-2.5,-DEPTH);
   FogCoordExtProc(1.0); glTexCoord2f(1.0, 1.0); glVertex3f( 2.5, 2.5,-DEPTH);
   FogCoordExtProc(1.0); glTexCoord2f(0.0, 1.0); glVertex3f(-2.5, 2.5,-DEPTH);
   glEnd;

   { Floor }
   glBegin(GL_QUADS);
   FogCoordExtProc(1.0); glTexCoord2f(0.0, 0.0); glVertex3f(-2.5,-2.5,-DEPTH);
   FogCoordExtProc(1.0); glTexCoord2f(1.0, 0.0); glVertex3f( 2.5,-2.5,-DEPTH);
   FogCoordExtProc(0.0); glTexCoord2f(1.0, 1.0); glVertex3f( 2.5,-2.5, DEPTH);
   FogCoordExtProc(0.0); glTexCoord2f(0.0, 1.0); glVertex3f(-2.5,-2.5, DEPTH);
   glEnd;

   { Roof }
   glBegin(GL_QUADS);
   FogCoordExtProc(1.0); glTexCoord2f(0.0, 0.0); glVertex3f(-2.5, 2.5,-DEPTH);
   FogCoordExtProc(1.0); glTexCoord2f(1.0, 0.0); glVertex3f( 2.5, 2.5,-DEPTH);
   FogCoordExtProc(0.0); glTexCoord2f(1.0, 1.0); glVertex3f( 2.5, 2.5, DEPTH);
   FogCoordExtProc(0.0); glTexCoord2f(0.0, 1.0); glVertex3f(-2.5, 2.5, DEPTH);
   glEnd;

   { Right }
   glBegin(GL_QUADS);
   FogCoordExtProc(0.0); glTexCoord2f(0.0, 0.0); glVertex3f( 2.5,-2.5, DEPTH);
   FogCoordExtProc(0.0); glTexCoord2f(0.0, 1.0); glVertex3f( 2.5, 2.5, DEPTH);
   FogCoordExtProc(1.0); glTexCoord2f(1.0, 1.0); glVertex3f( 2.5, 2.5,-DEPTH);
   FogCoordExtProc(1.0); glTexCoord2f(1.0, 0.0); glVertex3f( 2.5,-2.5,-DEPTH);
   glEnd;

   { Left }
   glBegin(GL_QUADS);
   FogCoordExtProc(0.0); glTexCoord2f(0.0, 0.0); glVertex3f(-2.5,-2.5, DEPTH);
   FogCoordExtProc(0.0); glTexCoord2f(0.0, 1.0); glVertex3f(-2.5, 2.5, DEPTH);
   FogCoordExtProc(1.0); glTexCoord2f(1.0, 1.0); glVertex3f(-2.5, 2.5,-DEPTH);
   FogCoordExtProc(1.0); glTexCoord2f(1.0, 0.0); glVertex3f(-2.5,-2.5,-DEPTH);
   glEnd;
{$endif}
end;

procedure Resize(Window: TGLWindow);
begin
  glViewport(0, 0, Window.Width, Window.Height);
  ProjectionGLPerspective(45.0, Window.Width/Window.Height, 0.1, 100.0);
end;

procedure KeyDown(Window: TGLWindow; Key: TKey; C: char);
begin
  case C of
    'f': SetFogMode(fogMode + 1);
    '+': begin
           fogDensity += 0.05;
           MinTo1st(fogDensity, 1.0);
           SetFogMode(fogMode);
         end;
    '-': begin
           fogDensity -= 0.05;
           MaxTo1st(fogDensity, 0);
           SetFogMode(fogMode);
         end;
    's': begin
           if fogStart > 0.0 then fogStart -= 1.0;
           SetFogMode(fogMode);
         end;
    'S': begin
           if fogStart < fogEnd then fogStart += 1.0;
           SetFogMode(fogMode);
         end;
    'e': begin
           if fogEnd > fogStart then fogEnd -= 1.0;
           SetFogMode(fogMode);
         end;
    'E': begin
           if fogEnd < 100.0 then fogEnd += 1.0;
           SetFogMode(fogMode);
         end;
    'c': begin
           SetFogCoord(not fogCoord);
         end;
    CharEscape: Window.Close;
    else
      case Key of
        K_Up  : if camz < (DEPTH - 1.0) then camz += 1.0;
        K_Down: if camz > -19.0 then camz -= 1.0;
        else Exit;
      end;
  end;
  Window.PostRedisplay;
end;

procedure Open(Window: TGLWindow);
const
  Wrap: TTextureWrap2D = (GL_REPEAT, GL_REPEAT);
begin
  glEnable(GL_TEXTURE_2D);
  glClearColor(0.0, 0.0, 0.0, 0.5);
  glClearDepth(1.0);
  glDepthFunc(GL_LEQUAL);
  glEnable(GL_DEPTH_TEST);
  glShadeModel(GL_SMOOTH);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

  glFogfv(GL_FOG_COLOR, fogColor);
  glHint(GL_FOG_HINT, GL_NICEST);
  SetFogCoord(true); { try to enable fog_coord }
  SetFogMode(2);         { GL_EXP }

  camz := -19.0;

  Texture := LoadGLTexture('brickwall.png', GL_LINEAR, GL_LINEAR, Wrap);

{$ifdef ARRAYS}
   glEnableClientState(GL_VERTEX_ARRAY);
   glVertexPointer(3, GL_FLOAT, 0, vertex_pointer);

   glEnableClientState(GL_TEXTURE_COORD_ARRAY);
   glTexCoordPointer(2, GL_FLOAT, 0, texcoord_pointer);

   if (have_fog_coord)
   begin
      glFogCoordPointer_ext = (GLFOGCOORDPOINTEREXTPROC)glutGetProcAddress("glFogCoordPointerEXT");
      glEnableClientState(GL_FOG_COORDINATE_ARRAY_EXT);
      glFogCoordPointer_ext(GL_FLOAT, 0, fogcoord_pointer);
   end;
{$endif}
end;

begin
  Window := TGLWindowDemo.Create(Application);

  Window.OnOpen := @Open;
  Window.OnResize := @Resize;
  Window.OnKeyDown := @KeyDown;
  Window.OnDraw := @Draw;

  Window.Open;
  Application.Run;
end.
