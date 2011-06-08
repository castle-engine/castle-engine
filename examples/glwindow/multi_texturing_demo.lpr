{
  Copyright 2007-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple OpenGL multitexturing demo, using Kambi VRML game engine
  GLWindow and texture loading helpers.

  From the left:
  1st cube: 1st texture (it's strongly limited by material 1,1,0).
  2nd cube: 2nd texture (it's not much limited by material 1,1,0).
  3rd cube: material * 1st texture + 2nd texture
  4th cube: material * 2nd texture + 1st texture

  (just to show that order of textures in multitexturing obviosuly
  matters, as it determines the calculations done).
}

program multi_texturing_demo;

uses GLWindow, GL, GLU, GLExt, KambiGLUtils,
  Boxes3D, Frustum, SysUtils, KambiUtils, VectorMath,
  KambiSceneManager, GLImages, Base3D;

var
  Tex: array [0..1] of TGLuint;

{ geometry  ------------------------------------------------------------------ }

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
      glMultiTexCoord2fARB(GL_TEXTURE0_ARB, X, Y);
      glMultiTexCoord2fARB(GL_TEXTURE1_ARB, X, Y);
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

begin
  if not (Params.TransparentGroup in [tgAll, tgOpaque]) then Exit;

  glMaterialv(GL_FRONT_AND_BACK, GL_AMBIENT, Vector4Single(1, 1, 0, 1));
  glMaterialv(GL_FRONT_AND_BACK, GL_DIFFUSE, Vector4Single(1, 1, 0, 1));

  glActiveTextureARB(GL_TEXTURE1_ARB);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_ADD);

  glPushMatrix;
    glTranslatef(-6, 0, 0);

    glActiveTextureARB(GL_TEXTURE0_ARB);
    glBindTexture(GL_TEXTURE_2D, Tex[0]);
    glEnable(GL_TEXTURE_2D);

    glActiveTextureARB(GL_TEXTURE1_ARB);
    glDisable(GL_TEXTURE_2D);

    DrawCube;
  glPopMatrix;

  glPushMatrix;
    glTranslatef(-2, 0, 0);

    glActiveTextureARB(GL_TEXTURE0_ARB);
    glBindTexture(GL_TEXTURE_2D, Tex[1]);
    glEnable(GL_TEXTURE_2D);

    glActiveTextureARB(GL_TEXTURE1_ARB);
    glDisable(GL_TEXTURE_2D);

    DrawCube;
  glPopMatrix;

  glPushMatrix;
    glTranslatef(+2, 0, 0);

    glActiveTextureARB(GL_TEXTURE0_ARB);
    glBindTexture(GL_TEXTURE_2D, Tex[0]);
    glEnable(GL_TEXTURE_2D);

    glActiveTextureARB(GL_TEXTURE1_ARB);
    glBindTexture(GL_TEXTURE_2D, Tex[1]);
    glEnable(GL_TEXTURE_2D);

    DrawCube;
  glPopMatrix;

  glPushMatrix;
    glTranslatef(+6, 0, 0);

    glActiveTextureARB(GL_TEXTURE0_ARB);
    glBindTexture(GL_TEXTURE_2D, Tex[1]);
    glEnable(GL_TEXTURE_2D);

    glActiveTextureARB(GL_TEXTURE1_ARB);
    glBindTexture(GL_TEXTURE_2D, Tex[0]);
    glEnable(GL_TEXTURE_2D);

    DrawCube;
  glPopMatrix;
end;

function TMyGeometry.BoundingBox: TBox3D;
const
  SceneBoundingBox: TBox3D =
  ( (-8, -3, -3),
    ( 8,  3,  3) );
begin
  Result := SceneBoundingBox;
end;

{ glw callbacks -------------------------------------------------------------- }

procedure Open(Window: TGLWindow);

  function LoadTexture(const FileName: string): TGLuint;
  begin
    Result := LoadGLTexture(FileName, GL_LINEAR_MIPMAP_LINEAR, GL_LINEAR, Texture2DClampToEdge);
  end;

begin
  Check(GL_ARB_multitexture, 'GL_ARB_multitexture required for this demo');

  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glEnable(GL_DEPTH_TEST);

  glLightModelv(GL_LIGHT_MODEL_AMBIENT, Vector4Single(0.8, 0.8, 0.8, 1.0));

  Tex[0] := LoadTexture('../../../castle/data/textures/023pierres.jpg');
  Tex[1] := LoadTexture('../../../castle/data/textures/bridgerock512side.jpg');
end;

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

  Window.OnOpen := @Open;
  Window.OpenAndRun;
end.
