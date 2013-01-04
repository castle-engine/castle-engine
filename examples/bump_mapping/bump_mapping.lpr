{
  Copyright 2007-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Bump mapping demo. Methods:
  1. Emboss.
  2. Dot3 using multitexturing (with or without normalization).
  3. VRML auto-detected. This is not really a single method, VRML engine
     inside auto-detects and uses one of the following methods
     - dot3 using multitexturing (with or without normalization).
     - GLSL (various versions, with parallax mapping,
       and steep parallax mapping (with self-shadowing))
  GLSL method is by far the best, fastest and best looking approach, although
  requires the newest hardware (supporting GLSL through ARB extension
  or GL >= 2.0 standard).

  Keys:
  awsd change light position[x, y], qQ change light position[z]
  eE change scale of emboss effect.
  ijkl change emboss shift (if in "by hand" mode)
  enter changes navigation mode (examiner / walker)

  See menu shortcuts for other keys.

  For navigation:
  Keys the same as [http://castle-engine.sf.net/view3dscene.php] for examiner mode:
    page up, page down, arrows: rotate the scene
    space: stop rotating
    ctrl + page up, page down, arrows: move the scene
    + / -: scale the scene
    home: restore default rotation, translation and scale
  and for walker mode
    arrows, etc.: doom like walking, see view3dscene docs

  1st command-line param may be filename of VRML to initially load
  (otherwise will load default demo in data/ subdir).
  You may run with --log to see many log messages, in particular
  the ones starting with "Bump mapping" --- they will tell you e.g.
  whether steep parallax mapping program is successfully compiled and used.
}
program bump_mapping;

uses CastleWindow, GL, GLU, GLExt, CastleGLUtils,
  Cameras, Boxes3D, SysUtils, CastleUtils, CastleVectors,
  CastleClassUtils, CastleFilesUtils, CastleStringUtils,
  CastleMessages,  BFNT_BitstreamVeraSans_Unit, OpenGLBmpFonts, CastleImages, CastleKeysMouse,
  NormalizationCubeMap, GLImages, GLVersionUnit, X3DNodes,
  CastleParameters, CastleLog, RaysWindow, UIControls, Classes, CastleWarnings,
  CastleSceneCore, CastleScene, X3DLoad, CastleProgress, Background, CastleColors,
  GLRenderer, CastleSceneManager, CastleRenderingCamera, CastleControls;

const
  SceneBoundingBox: TBox3D = ( Data: (
    (-6, -4, -3),
    ( 14,  8,  3) ));

type
  TBumpMethod = (bmEmboss, bmMultiTexDot, bmVRML);

var
  Window: TCastleWindowCustom;

  { normal texture is for all methods, bumps in alpha for emboss only }
  NormalAndBumpTex: array [0..3] of TGLuint;

  Font: TGLBitmapFont;

  LightPosition: TVector3Single = (0, 0, 2);

  Method: TBumpMethod = bmVRML;

  RotatingColumn: Single;

  Examiner: TExamineCamera;
  Walker: TWalkCamera;

  Scene: TCastleScene;
  SceneBrightestLight: TAbstractPositionalLightNode;
  RenderParams: TBasicRenderParams;

  { Vars below for bmEmboss only }

  LighterAndBumpTex: array [0..High(NormalAndBumpTex)] of TGLuint;

  XShift: Single = 0;
  YShift: Single = 0;
  ShiftByHand: boolean = false;

  BumpImageWidth, BumpImageHeight: Cardinal;

  EmbossScale: Single = 1;

  EmbossAlphaMultiplyByBlending: boolean = false;

  { Vars below for bmMultiTexDot only }

  NormalMap: array [0..High(NormalAndBumpTex)] of TGLuint;

  NormalizationCubeTex: TGLuint;

  NormalizedDot3First: boolean = false;

{ ---------------------------------------------------------------------------- }

{ Returns (unnormalized) direction from Vertex to LightPosition,
  in tangent space.

  In many articles, the vectors forming tangent space I called
    tangent
    binormal
    normal
  referring to the fact that one approach is to get normal and tangent
  from called, and binormal is just calculated by normal x tangent.
  I call them tangent = STangent, binormal = TTangent to make
  the meaning of them more clear (S, T are texture coordinates directions).
  Also, I don't calculate TTangent from cross product, I take it
  as a parameter: this allows me to use STangent, TTangent, Normal
  look like left handed coord system (this allows me to orient texture
  freely on the face, otherwise normal + STangent force the direction
  of texture T coordinate).

  ModelInvertedTransformation is the inverted transformation from the
  space where LightPosition is (this is world space) to object space,
  i.e. the one where Vertex is.

  Think like:
  - LightPosition                 is in world space
  - ModelTransformation * Vertex  is also in world space
  - Vertex                                        is in object space
  - so ModelInvertedTrasformation * LightPosition is in object space

  IOW, ModelInvertedTrasformation inverts all transformations
  applied after the point where we were in space where LightPosition was good,
  that is after
    glLightv(GL_LIGHT0, GL_POSITION, Vector4Single(LightPosition, 1))
  call.

  This is needed, since LightPositionObjectSpace is crucial for this procedure.
}
function LightDirectionInTangentSpace(
  const ModelInvertedTrasformation: TMatrix4Single;
  const Vertex, Normal, STangent, TTangent: TVector3Single): TVector3Single;
var
  ToTangent: TMatrix4Single;
  I: Integer;
  LightPositionObjectSpace: TVector3Single;
begin
  { We want to have LightPositionObjectSpace (in the same space where Vertex
    already is), as this is also the space where we expressed our STangent,
    TTangent, Normal vectors. "Tangent space" is for current face, such that
    XY run along surface's S,T texture coords. }

  LightPositionObjectSpace := MatrixMultPoint(ModelInvertedTrasformation,
    LightPosition);

  { first init 4th row and column }
  ToTangent := IdentityMatrix4Single;

  for I := 0 to 2 do
  begin
    ToTangent[I, 0] := STangent[I];
    ToTangent[I, 1] := TTangent[I];
    ToTangent[I, 2] := Normal[I];
  end;

  Result := MatrixMultPoint(ToTangent,
    VectorSubtract(LightPositionObjectSpace, Vertex));
end;

{ ----------------------------------------------------------------------------
  Various TTexCoordVertexProc implementations.
  They should set texture coord(s), and call glVertex(Vertex).
  And do whatever else is needed for current bump mapping method.
  LightDirTangent is direction from current vertex to light,
  already in tangent space, not normalized. }

type
  TTexCoordVertexProc = procedure (const TexX, TexY: Single;
    const Vertex: TVector3Single; LightDirTangent: TVector3Single);

procedure TexCoordVertex_NoBump(const TexX, TexY: Single;
  const Vertex: TVector3Single; LightDirTangent: TVector3Single);
begin
  glMultiTexCoord2f(GL_TEXTURE0, TexX, TexY);
  glVertexv(Vertex);
end;

procedure TexCoordVertex_Emboss(const TexX, TexY: Single;
  const Vertex: TVector3Single; LightDirTangent: TVector3Single);
begin
  if not ShiftByHand then
  begin
    LightDirTangent[2] := 0;
    NormalizeTo1st(LightDirTangent);
    XShift := EmbossScale * LightDirTangent[0] / BumpImageWidth;
    YShift := EmbossScale * LightDirTangent[1] / BumpImageHeight;
  end;

  glMultiTexCoord2f(GL_TEXTURE0, TexX         , TexY         );
  glMultiTexCoord2f(GL_TEXTURE1, TexX + XShift, TexY + YShift);

  glVertexv(Vertex);
end;

procedure TexCoordVertex_Dot3_NotNormalized(const TexX, TexY: Single;
  const Vertex: TVector3Single; LightDirTangent: TVector3Single);
begin
  NormalizeTo1st(LightDirTangent);

  { pass LightDirTangent as RGB color }
  glColor3f((LightDirTangent[0] + 1) / 2,
            (LightDirTangent[1] + 1) / 2,
            (LightDirTangent[2] + 1) / 2);

  glMultiTexCoord2f(GL_TEXTURE0, TexX, TexY);
  glMultiTexCoord2f(GL_TEXTURE1, TexX, TexY);

  glVertexv(Vertex);
end;

procedure TexCoordVertex_Dot3_Normalized(const TexX, TexY: Single;
  const Vertex: TVector3Single; LightDirTangent: TVector3Single);
begin
  { no need to normalize LightDirTangent, it will be normalized at each
    fragment anyway by normalizing cube map }

  glMultiTexCoord3f(GL_TEXTURE0,
    LightDirTangent[0],
    LightDirTangent[1],
    LightDirTangent[2]);
  glMultiTexCoord2f(GL_TEXTURE1, TexX, TexY);
  glMultiTexCoord2f(GL_TEXTURE2, TexX, TexY);

  glVertexv(Vertex);
end;

{ scene manager ------------------------------------------------------------ }

procedure DisableTextures;
begin
  glActiveTexture(GL_TEXTURE0);
  glDisable(GL_TEXTURE_2D);

  glActiveTexture(GL_TEXTURE1);
  glDisable(GL_TEXTURE_2D);

  if GLMaxTextureUnits > 2 then
  begin
    glActiveTexture(GL_TEXTURE2);
    glDisable(GL_TEXTURE_2D);
  end;

  { reset as active 0 texture unit, to be in default state }
  glActiveTexture(GL_TEXTURE0);
end;

type
  TMySceneManager = class(TCastleSceneManager)
    procedure RenderFromViewEverything; override;
    procedure ApplyProjection; override;
  end;

var
  SceneManager: TMySceneManager;

procedure TMySceneManager.RenderFromViewEverything;
var
  ModelInvertedTrasformation: TMatrix4Single;

  { glTranslate, and update ModelInvertedTrasformation }
  procedure Translate(const X, Y, Z: Single);
  begin
    glTranslatef(X, Y, Z);
    { On ModelInvertedTrasformation, we add new matrix on the left side,
      to revert also the order of operations. This way we really get
      proper inverse. }
    ModelInvertedTrasformation := MatrixMult(
      TranslationMatrix(-X, -Y, -Z),
      ModelInvertedTrasformation);
  end;

  { glRotate, and update ModelInvertedTrasformation }
  procedure Rotate(const Angle, X, Y, Z: Single);
  begin
    glRotatef(Angle, X, Y, Z);
    ModelInvertedTrasformation := MatrixMult(
      RotationMatrixDeg(-Angle, X, Y, Z),
      ModelInvertedTrasformation);
  end;

  { glPushMatrix, and return current ModelInvertedTrasformation }
  function PushMatrix: TMatrix4Single;
  begin
    glPushMatrix;
    Result := ModelInvertedTrasformation;
  end;

  { glPopMatrix, and set given ModelInvertedTrasformation }
  procedure PopMatrix(const SavedMatrix: TMatrix4Single);
  begin
    glPopMatrix;
    ModelInvertedTrasformation := SavedMatrix;
  end;

  { Draw cube using really old-fashioned approach (no vertex arrays,
    just specify by hand 6 quads, 4 vertexes and tex coords each). }
  procedure DrawCube(const TexCoordVertex: TTexCoordVertexProc);

    { Pass Normal, STangent, TTangent to the DoTexCoordVertex
      as parameters, not through local variables inside DrawCube,
      to workaround FPC optimizer bug
      http://bugs.freepascal.org/view.php?id=17413 }

    procedure DoTexCoordVertex(const TexX, TexY: Single;
      const Vertex, Normal, STangent, TTangent: TVector3Single);
    begin
      TexCoordVertex(TexX, TexY, Vertex,
        LightDirectionInTangentSpace(ModelInvertedTrasformation, Vertex,
          Normal, STangent, TTangent));
    end;

  var
    Normal, STangent, TTangent: TVector3Single;
  begin
    glBegin(GL_QUADS);
      Normal   := Vector3Single( 0, 0, 1);
      STangent := Vector3Single( 1, 0, 0);
      TTangent := Vector3Single( 0, 1, 0);
      glNormalv(Normal);
      DoTexCoordVertex(0, 0, Vector3Single(-1, -1,  1), Normal, STangent, TTangent);
      DoTexCoordVertex(1, 0, Vector3Single( 1, -1,  1), Normal, STangent, TTangent);
      DoTexCoordVertex(1, 1, Vector3Single( 1,  1,  1), Normal, STangent, TTangent);
      DoTexCoordVertex(0, 1, Vector3Single(-1,  1,  1), Normal, STangent, TTangent);

      Normal   := Vector3Single( 0, 0,-1);
      STangent := Vector3Single(-1, 0, 0);
      TTangent := Vector3Single( 0, 1, 0);
      glNormalv(Normal);
      DoTexCoordVertex(1, 0, Vector3Single(-1, -1, -1), Normal, STangent, TTangent);
      DoTexCoordVertex(1, 1, Vector3Single(-1,  1, -1), Normal, STangent, TTangent);
      DoTexCoordVertex(0, 1, Vector3Single( 1,  1, -1), Normal, STangent, TTangent);
      DoTexCoordVertex(0, 0, Vector3Single( 1, -1, -1), Normal, STangent, TTangent);

      Normal   := Vector3Single( 0, 1, 0);
      STangent := Vector3Single( 1, 0, 0);
      TTangent := Vector3Single( 0, 0, -1);
      glNormalv(Normal);
      DoTexCoordVertex(0, 1, Vector3Single(-1,  1, -1), Normal, STangent, TTangent);
      DoTexCoordVertex(0, 0, Vector3Single(-1,  1,  1), Normal, STangent, TTangent);
      DoTexCoordVertex(1, 0, Vector3Single( 1,  1,  1), Normal, STangent, TTangent);
      DoTexCoordVertex(1, 1, Vector3Single( 1,  1, -1), Normal, STangent, TTangent);

      Normal   := Vector3Single( 0,-1, 0);
      STangent := Vector3Single(-1, 0, 0);
      TTangent := Vector3Single( 0, 0, -1);
      glNormalv(Normal);
      DoTexCoordVertex(1, 1, Vector3Single(-1, -1, -1), Normal, STangent, TTangent);
      DoTexCoordVertex(0, 1, Vector3Single( 1, -1, -1), Normal, STangent, TTangent);
      DoTexCoordVertex(0, 0, Vector3Single( 1, -1,  1), Normal, STangent, TTangent);
      DoTexCoordVertex(1, 0, Vector3Single(-1, -1,  1), Normal, STangent, TTangent);

      Normal   := Vector3Single( 1, 0, 0);
      STangent := Vector3Single( 0, 0,-1);
      TTangent := Vector3Single( 0, 1, 0);
      glNormalv(Normal);
      DoTexCoordVertex(1, 0, Vector3Single( 1, -1, -1), Normal, STangent, TTangent);
      DoTexCoordVertex(1, 1, Vector3Single( 1,  1, -1), Normal, STangent, TTangent);
      DoTexCoordVertex(0, 1, Vector3Single( 1,  1,  1), Normal, STangent, TTangent);
      DoTexCoordVertex(0, 0, Vector3Single( 1, -1,  1), Normal, STangent, TTangent);

      Normal   := Vector3Single(-1, 0, 0);
      STangent := Vector3Single( 0, 0, 1);
      TTangent := Vector3Single( 0, 1, 0);
      glNormalv(Normal);
      DoTexCoordVertex(0, 0, Vector3Single(-1, -1, -1), Normal, STangent, TTangent);
      DoTexCoordVertex(1, 0, Vector3Single(-1, -1,  1), Normal, STangent, TTangent);
      DoTexCoordVertex(1, 1, Vector3Single(-1,  1,  1), Normal, STangent, TTangent);
      DoTexCoordVertex(0, 1, Vector3Single(-1,  1, -1), Normal, STangent, TTangent);
    glEnd();
  end;

  procedure Test(TextureNum: Integer);

    procedure SetTexturesForEmboss(ShowRealBump, Modulate: boolean);
    var
      Tex: TGLuint;
    begin
      if Modulate then
        Tex := LighterAndBumpTex[TextureNum] else
        Tex := NormalAndBumpTex[TextureNum];

      glActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_2D, Tex);
      glEnable(GL_TEXTURE_2D);
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE);
      { only alpha 0.5 is important below }
      glTexEnvv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, Vector4Single(0, 0, 0, 0.5));

      { Texture unit 0 RGB calculates normal texturing (PRIMARY_COLOR
        modulate with TEXTURE in this unit). }
      glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB, GL_MODULATE);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB, GL_PRIMARY_COLOR);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB, GL_SRC_COLOR);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB, GL_TEXTURE);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_RGB, GL_SRC_COLOR);

      { Texture unit 0 alpha loads bump map (not shifted) value.
        *Loads*, i.e. it's not mixed with PRIMARY_COLOR in any way. }
      if Modulate then
        { I add 0.5 here, so that original bump is in 0.5...1.0
          range, and subtraction will give us result in 0.0..1.0,
          where 0.5 = no difference between shifted bumps. }
        glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_ALPHA, GL_ADD) else
        { SOURCE1 and OPERAND1 will be ignored in this case. }
        glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_ALPHA, GL_REPLACE);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_ALPHA, GL_TEXTURE);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_ALPHA, GL_SRC_ALPHA);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_ALPHA, GL_CONSTANT);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_ALPHA, GL_SRC_ALPHA);

      glActiveTexture(GL_TEXTURE1);
      { Load Tex again, as it has bump in the alpha
        channel. Don't care about RGB channels of texture loaded here,
        they will not be used. }
      glBindTexture(GL_TEXTURE_2D, Tex);
      glEnable(GL_TEXTURE_2D);
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE);

      { Texture unit 1 RGB simply passes the result forward, no modifications.
        This step is only to subtract bump maps. }
      glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB, GL_REPLACE);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB, GL_PREVIOUS);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB, GL_SRC_COLOR);

      { Texture unit 1 alpha does subtraction: from previous result
        (unshifted bump) subtract shifted bump. }
      glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_ALPHA, GL_SUBTRACT);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_ALPHA, GL_PREVIOUS);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_ALPHA, GL_SRC_ALPHA);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_ALPHA, GL_TEXTURE);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_ALPHA, GL_SRC_ALPHA);

      { Now we have RGB and alpha color that should be multiplied for the final
        color. We want to keep 1-pass approach, so I see two ways to solve this:

        1. Use another texture unit, with no texture bound to it,
           only to perform this operation.

           As a bonus, we can use this to grab alpha value from PRIMARY_COLOR
           and simply put it here. This way you can use your "normal" alpha
           from color/material, e.g. for doing partially transparent materials
           using blending.

        2. On older (but still reasonable ?) hardware, you may have
           only 2 texture units.

           - On kocury (newer NVidia: GeForce FX 5200): 4
           - On ii.107 (poor Radeon (GL_VERSION : 1.4.5469 WinXP Release so really old drivers)): 8
           - On chantal (Radeon X1600, MacBookPro): 8
           - On kocur.ii (older NVidia: GeForce4 MX 440): 2, indeed
           - On crypto.ii (Radeon X300): 8

           Then you can use blending to multiply incoming color by it's own
           incoming alpha with glBlendFunc(GL_SRC_ALPHA, GL_ZERO).
           This means that you lose the ability to do partial transparency,
           as you lost alpha from your PRIMARY_COLOR --- but you keep 1-pass
           method, which is crucial.

        You could also make this into 2-pass method, it's probably even possible
        to do this without EXT_texture_env_combine (only ARB_multitexture).
        But since EXT_texture_env_combine and ARB_multitexture should be
        pretty much commonly found (OpenGL >= 1.3), I don't see much point for
        more elaborate tricks. }

      if not EmbossAlphaMultiplyByBlending then
      begin
        glActiveTexture(GL_TEXTURE2);
        { Hm, funny, actually I have to bind *any* texture here
          (it's values will not be used by equation on this texture unit),
          otherwise this texture unit is always off. }
        glBindTexture(GL_TEXTURE_2D, Tex);
        glEnable(GL_TEXTURE_2D);
        glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE);

        if ShowRealBump then
        begin
          if Modulate then
            glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB, GL_MODULATE) else
            glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB, GL_ADD);

          glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB, GL_PREVIOUS);
          glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB, GL_SRC_COLOR);
          glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB, GL_PREVIOUS);
          glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_RGB, GL_SRC_ALPHA);
        end else
        begin
          { just for test: show the current alpha values
            (subtracted bumps) }
          glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB, GL_REPLACE);
          glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB, GL_PREVIOUS);
          glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB, GL_SRC_ALPHA);
        end;

        glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_ALPHA, GL_REPLACE);
        glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_ALPHA, GL_PRIMARY_COLOR);
        glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_ALPHA, GL_SRC_ALPHA);
      end;
    end;

    procedure DrawEmbossCubes;
    var
      SavedMatrix: TMatrix4Single;
    begin
      { bump_amount * normal_color, with lighter textures }

      if not EmbossAlphaMultiplyByBlending then
      begin
        SetTexturesForEmboss(false, true);

        SavedMatrix := PushMatrix;
          Translate(+0, 0, 0);
          DrawCube(@TexCoordVertex_Emboss);
        PopMatrix(SavedMatrix);
      end;

      SetTexturesForEmboss(true, true);

      if EmbossAlphaMultiplyByBlending then
      begin
        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA, GL_ZERO);
      end;

      SavedMatrix := PushMatrix;
        Translate(+4, 0, 0);
        DrawCube(@TexCoordVertex_Emboss);
      PopMatrix(SavedMatrix);

      SavedMatrix := PushMatrix;
        Translate(+8, 0, 0);
        { Rotate just to show that lightdir tangent calculation works }
        Rotate(RotatingColumn, 0, 1, 0);
        DrawCube(@TexCoordVertex_Emboss);
      PopMatrix(SavedMatrix);

      if EmbossAlphaMultiplyByBlending then
        glDisable(GL_BLEND);

      { bump_amount + normal_color hack }

      if not EmbossAlphaMultiplyByBlending then
      begin
        SetTexturesForEmboss(false, false);

        SavedMatrix := PushMatrix;
          Translate(+12, 0, 0);
          DrawCube(@TexCoordVertex_Emboss);
        PopMatrix(SavedMatrix);

        SetTexturesForEmboss(true, false);

        SavedMatrix := PushMatrix;
          Translate(+16, 0, 0);
          DrawCube(@TexCoordVertex_Emboss);
        PopMatrix(SavedMatrix);
      end;
    end;

    procedure SetTexturesForDot3NotNormalized;
    begin
      glActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_2D, NormalMap[TextureNum]);
      glEnable(GL_TEXTURE_2D);
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE);

      { texture unit 0: make dot between PRIMARY_COLOR and
        current texture (normal map). IOW, calculate diffuse factor. }
      glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB, GL_DOT3_RGB);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB, GL_PRIMARY_COLOR);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB, GL_SRC_COLOR);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB, GL_TEXTURE);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_RGB, GL_SRC_COLOR);

      glActiveTexture(GL_TEXTURE1);
      { We bind NormalAndBumpTex, although actually we don't use
        bump stored there as alpha channel (they are only for emboss
        method). }
      glBindTexture(GL_TEXTURE_2D, NormalAndBumpTex[TextureNum]);
      glEnable(GL_TEXTURE_2D);
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE);

      { texture unit 1: multiply diffuse factor by texture value. }
      glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB, GL_MODULATE);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB, GL_PREVIOUS);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB, GL_SRC_COLOR);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB, GL_TEXTURE);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_RGB, GL_SRC_COLOR);

      if GLMaxTextureUnits > 2 then
      begin
        { texture unit 2 not used. }
        glActiveTexture(GL_TEXTURE2);
        glDisable(GL_TEXTURE_2D);
      end;
    end;

    procedure SetTexturesForDot3Normalized;
    begin
      { texture unit 0: just lookup your coordinates (which are
        3D lighting dir, not normalized) in cube map.
        This calculates normalized lighting dir. }
      glActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_CUBE_MAP_ARB, NormalizationCubeTex);
      glEnable(GL_TEXTURE_CUBE_MAP_ARB);
      { make sure GL_TEXTURE_2D is disabled here (other bump mapping
        methods could enable this) }
      glDisable(GL_TEXTURE_2D);
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE);

      glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB, GL_REPLACE);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB, GL_TEXTURE);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB, GL_SRC_COLOR);

      glActiveTexture(GL_TEXTURE1);
      glBindTexture(GL_TEXTURE_2D, NormalMap[TextureNum]);
      glEnable(GL_TEXTURE_2D);
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE);

      { texture unit 1: make dot between previous (light dir normalized) and
        current texture (normal map). IOW, calculate diffuse factor. }
      glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB, GL_DOT3_RGB);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB, GL_PREVIOUS);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB, GL_SRC_COLOR);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB, GL_TEXTURE);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_RGB, GL_SRC_COLOR);

      glActiveTexture(GL_TEXTURE2);
      { We bind NormalAndBumpTex, although actually we don't use
        bump stored there as alpha channel (they are only for emboss
        method). }
      glBindTexture(GL_TEXTURE_2D, NormalAndBumpTex[TextureNum]);
      glEnable(GL_TEXTURE_2D);
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE);

      { texture unit 1: multiply diffuse factor by texture value. }
      glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB, GL_MODULATE);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB, GL_PREVIOUS);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB, GL_SRC_COLOR);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB, GL_TEXTURE);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_RGB, GL_SRC_COLOR);
    end;

    procedure DrawDot3Cubes;
    var
      Dot3Normalized, Dot3NotNormalized: TGLfloat;
      SavedMatrix: TMatrix4Single;
    begin
      if NormalizedDot3First then
      begin
        Dot3Normalized := 0;
        Dot3NotNormalized := 4;
      end else
      begin
        Dot3NotNormalized := 0;
        Dot3Normalized := 4;
      end;

      SetTexturesForDot3NotNormalized;

      glDisable(GL_LIGHTING);
      glColorv(White3Single);

      SavedMatrix := PushMatrix;
        Translate(Dot3NotNormalized, 0, 0);
        DrawCube(@TexCoordVertex_Dot3_NotNormalized);
      PopMatrix(SavedMatrix);

      if GLMaxTextureUnits > 2 then
      begin
        SetTexturesForDot3Normalized;

        SavedMatrix := PushMatrix;
          Translate(Dot3Normalized, 0, 0);
          DrawCube(@TexCoordVertex_Dot3_Normalized);
        PopMatrix(SavedMatrix);

        SavedMatrix := PushMatrix;
          Translate(8, 0, 0);
          { Rotate just to show that lightdir tangent calculation works }
          Rotate(RotatingColumn, 0, 1, 0);
          DrawCube(@TexCoordVertex_Dot3_Normalized);
        PopMatrix(SavedMatrix);

        { clean after ourselves, otherwise we'd have to call
          glDisable(GL_TEXTURE_CUBE_MAP_ARB) everywhere where we
          enable GLTEXTURE_2D.
          Doing this by push/pop tex environments could be cleaner,
          but not needed now. }
        glActiveTexture(GL_TEXTURE0);
        glDisable(GL_TEXTURE_CUBE_MAP_ARB);
      end;

      glEnable(GL_LIGHTING);
    end;

  var
    SavedMatrix: TMatrix4Single;
  begin
    { texture unit 0 contains normal texture value mixed with scene color }
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, NormalAndBumpTex[TextureNum]);
    glEnable(GL_TEXTURE_2D);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

    { texture unit 1 not used. }
    glActiveTexture(GL_TEXTURE1);
    glDisable(GL_TEXTURE_2D);

    { texture unit 2 not used. }
    if GLMaxTextureUnits > 2 then
    begin
      glActiveTexture(GL_TEXTURE2);
      glDisable(GL_TEXTURE_2D);
    end;

    SavedMatrix := PushMatrix;
      Translate(-4, 0, 0);
      DrawCube(@TexCoordVertex_NoBump);
    PopMatrix(SavedMatrix);

    case Method of
      bmEmboss: DrawEmbossCubes;
      bmMultiTexDot: DrawDot3Cubes;
      else raise EInternalError.Create('method?');
    end;
  end;

begin
  glClear(GL_DEPTH_BUFFER_BIT or GL_COLOR_BUFFER_BIT);

  DisableTextures;

  if (Method = bmVRML) and (Scene.Background <> nil) then
  begin
    glLoadMatrix(RenderingCamera.RotationMatrix);
    Scene.Background.Render;
  end;

  glLoadMatrix(RenderingCamera.Matrix);

  if (Method = bmEmboss) and (GLMaxTextureUnits < 3) then
  begin
    { You can't have emboss with not EmbossAlphaMultiplyByBlending
      on this hardware. }
    EmbossAlphaMultiplyByBlending := true;
  end;

  glPushAttrib(GL_ENABLE_BIT);
    glEnable(GL_DEPTH_TEST);
    glDisable(GL_LIGHTING);

    glColorv(Yellow3Single);
    glPointSize(10); { VRML renderer will reset it }
    glBegin(GL_POINTS);
      glVertexv(LightPosition);
    glEnd;
  glPopAttrib;

  if Method = bmVRML then
  begin
    { This is high for non-bmVRML methods, they look better then.
      But for bmVRML it's clearly visible that light is non-natural,
      so I change it back to default GL value. }
    glLightModelv(GL_LIGHT_MODEL_AMBIENT, Vector4Single(0.2, 0.2, 0.2, 1.0));

    Scene.Render(RenderingCamera.Frustum, RenderParams);
  end else
  begin
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);
    glEnable(GL_DEPTH_TEST);

    glLightv(GL_LIGHT0, GL_POSITION, Vector4Single(LightPosition, 1));

    glLightModelv(GL_LIGHT_MODEL_AMBIENT, Vector4Single(0.5, 0.5, 0.5, 1.0));

    ModelInvertedTrasformation := IdentityMatrix4Single;

    glMaterialv(GL_FRONT_AND_BACK, GL_AMBIENT, Vector4Single(1, 1, 1, 1));
    glMaterialv(GL_FRONT_AND_BACK, GL_DIFFUSE, Vector4Single(1, 1, 1, 1));

    Translate(0,-4, 0);
    Test(2);

    Translate(0, 4, 0);
    Test(0);

    Translate(0, 4, 0);
    Test(1);

    Translate(0, 4, 0);
    Test(3);

    { We should disable textures on all affected units at the end of this Draw,
      otherwise TGLModeFrozenScreen displays wrong image.
      This could be fixed in TGLModeFrozenScreen in FrozenImageDraw
      by disabling textures on all units... for now, it's easier
      to just disable textures here.

      TGLModeFrozenScreen is used when doing dialogs, e.g. dialog to save screen.
      Without this DisableTextures, save screen produces wrong results.

      Probably this makes DisableTextures call at the beginning of Draw useless.
      For safety, I'll keep it, this is only demo program anyway... VRML engine
      properly enables/disables textures itself, that's enough for real
      "production" programs. }
    DisableTextures;
  end;
end;

procedure TMySceneManager.ApplyProjection;

  procedure UpdateCameraProjectionMatrix(const M: TMatrix4Single);
  begin
    Walker.ProjectionMatrix := M;
    Examiner.ProjectionMatrix := M;
  end;

begin
  glViewport(0, 0, ContainerWidth, ContainerHeight);

  { Assign arbitrary walk projection and angle of view values.
    We do not want to let inherited ApplyProjection to use MainScene
    for this, as MainScene is not always assigned, we have our own geometry
    for Method <> bmVRML. }

  FProjectionNear := SceneBoundingBox.AverageSize * 0.05;
  FProjectionFar  := SceneBoundingBox.AverageSize * 20.0;

  FPerspectiveView := true;
  FPerspectiveViewAngles[1] := 30.0;
  FPerspectiveViewAngles[0] := AdjustViewAngleDegToAspectRatio(
    FPerspectiveViewAngles[1], ContainerWidth / ContainerHeight);

  UpdateCameraProjectionMatrix(PerspectiveProjection(FPerspectiveViewAngles[1],
    ContainerWidth / ContainerHeight, ProjectionNear, ProjectionFar));

  Scene.BackgroundSkySphereRadius :=
    TBackground.NearFarToSkySphereRadius(ProjectionNear, ProjectionFar);
end;

{ vrml scene loading --------------------------------------------------------- }

{ Some preparations for bmVRML, to make it look better.
  LightPosition more suitable.
  Walker position initialized from Scene viewport. }
procedure PrepareForSceneCore;
var
  CamPos, CamDir, CamUp, GravityUp: TVector3Single;
begin
  LightPosition := Vector3Single(3, 3, 5);

  { When bmVRML, these two should always be equal }
  SceneBrightestLight.FdLocation.Send(LightPosition);

  { reinit both cameras }

  Scene.GetPerspectiveViewpoint(CamPos, CamDir, CamUp, GravityUp);
  Walker.Init(CamPos, CamDir, CamUp, GravityUp,
    0, 0 { unused, we don't use Gravity here });
  Walker.MoveSpeed := Scene.BoundingBox.AverageSize * 0.2;

  Examiner.Init(Scene.BoundingBox, 0.1);
end;

type
  TSeekBrightestLight = class
    BrightestLight: TAbstractPositionalLightNode;
    procedure Enum(Node: TX3DNode);
  end;

procedure TSeekBrightestLight.Enum(Node: TX3DNode);
begin
  if (BrightestLight = nil) or
     (BrightestLight.FdIntensity.Value <
      TAbstractPositionalLightNode(Node).FdIntensity.Value) then
    BrightestLight := TAbstractPositionalLightNode(Node);
end;

procedure LoadSceneCore(const FileName: string);
var
  SeekBrightestLight: TSeekBrightestLight;
begin
  Scene.Load(FileName);

  { find SceneBrightestLight }
  SeekBrightestLight := TSeekBrightestLight.Create;
  try
    Scene.RootNode.EnumerateNodes(TAbstractPositionalLightNode,
      @SeekBrightestLight.Enum, true);
    SceneBrightestLight := SeekBrightestLight.BrightestLight;
  finally
    FreeAndNil(SeekBrightestLight);
  end;

  { create SceneBrightestLight, if none }
  if SceneBrightestLight = nil then
  begin
    SceneBrightestLight := TPointLightNode.Create('', '');
    Scene.RootNode.FdChildren.Add(SceneBrightestLight);
    Scene.ChangedAll;
  end;
end;

procedure LoadScene(const FileName: string);
begin
  LoadSceneCore(FileName);

  { if Method not bmVRML, then this will be done anyway when user will switch
    to bmVRML method. }
  if Method = bmVRML then
    PrepareForSceneCore;
end;

{ status text ---------------------------------------------------------------- }

type
  TStatusText = class(TUIControl)
    function DrawStyle: TUIControlDrawStyle; override;
    procedure Draw; override;
  end;

function TStatusText.DrawStyle: TUIControlDrawStyle;
begin
  Result := ds2D;
end;

procedure TStatusText.Draw;
var
  S: TStrings;
const
  MethodNames: array [TBumpMethod] of string =
  ('emboss', 'dot3 by multitexturing', 'built in VRML');
begin
  glPushAttrib(GL_ENABLE_BIT);
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_LIGHTING);
    DisableTextures;

    glColorv(Yellow3Single);

    S := TStringList.Create;

    { fps is on caption already }
    { S.Append(Format('FPS : %f (real : %f)' +nl, [Window.FpsFrameTime, Window.FpsRealTime])); }
    S.Append('Method: ' + MethodNames[Method]);
    if ShiftByHand then
      S.Append(Format('Shift: %f %f', [XShift, YShift])) else
      S.Append('Shift automatic from tangent space light position');
    S.Append(Format('Dot3 normalized first (for quick visual compare): %s',
      [BoolToStr[NormalizedDot3First]]));
    S.Append(Format('Emboss scale: %f', [EmbossScale]));
    S.Append(Format('Emboss alpha multiply by blending: %s',
      [BoolToStr[EmbossAlphaMultiplyByBlending]]));

    Font.PrintStrings(S, false, 0, 5, 5);

    FreeAndNil(S);
  glPopAttrib;
end;

{ TNextButton ---------------------------------------------------------------- }

type
  TNextButton = class(TCastleButton)
    constructor Create(AOwner: TComponent); override;
    procedure DoClick; override;
  end;

var
  NextButton: TNextButton;

constructor TNextButton.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Next bump mapping method';
end;

procedure TNextButton.DoClick;
begin
  inherited;

  if Method = bmVRML then
  begin
    SceneManager.Items.Remove(Scene);
    SceneManager.MainScene := nil;
  end;

  if Method = High(Method) then
    Method := Low(Method) else
    Method := Succ(Method);
  if Method = bmVRML then
    PrepareForSceneCore;

  { if, and only if, Method = bmVRML, then SceneManager contains our Scene. }
  if Method = bmVRML then
  begin
    SceneManager.Items.Add(Scene);
    SceneManager.MainScene := Scene;
  end;
end;

{ glw callbacks -------------------------------------------------------------- }

var
  VrmlFileName: string = 'data/levels/fountain/fountain_final.wrl';

procedure Open(glwin: TCastleWindowBase);
const
  MinFilter = GL_LINEAR_MIPMAP_LINEAR;
  MagFilter = GL_LINEAR;

  procedure LoadTextureBumpMap(const BaseFileName, BumpFileName: string;
    var Tex: TGLuint);
  var
    BaseImage: TRGBImage;
    BumpImage: TGrayscaleImage;
    Image: TRGBAlphaImage;
  begin
    BaseImage := nil;
    BumpImage := nil;
    Image := nil;
    try
      BaseImage := LoadImage('textures/' + BaseFileName,
        [TRGBImage]) as TRGBImage;

      BumpImage := LoadImage('textures/' + BumpFileName,
        [TGrayscaleImage]) as TGrayscaleImage;
      BumpImage.HalfColors;
      BumpImageWidth := BumpImage.Width;
      BumpImageHeight := BumpImage.Height;

      Image := TRGBAlphaImage.Create;
      Image.Compose(BaseImage, BumpImage);

      Tex := LoadGLTexture(Image, MinFilter, MagFilter, Texture2DClampToEdge);

      { BumpTex := LoadGLTexture(BumpImage, MinFilter, MagFilter,
        Texture2DClampToEdge, true); }
    finally
      FreeAndNil(BaseImage);
      FreeAndNil(BumpImage);
      FreeAndNil(Image);
    end;
  end;

  procedure LoadNormalMap(const FileName: string;
    var Tex: TGLuint);
  begin
    { This is quite simple... }
    Tex := LoadGLTexture('textures/' + FileName, MinFilter, MagFilter,
      Texture2DClampToEdge);
  end;

begin
  Check(GLUseMultiTexturing, 'OpenGL with multitexture support required');

  Check(GLMaxTextureUnits >= 2, 'At least 2 texture units required for dot3 (for emboss, 3 texture units)');

  glClearColor(0.3, 0.3, 0.3, 1);

  LoadTextureBumpMap('brick_1.png'      , 'brick_1_bump.png', NormalAndBumpTex[0]);
  LoadTextureBumpMap('brick_1_light.png', 'brick_1_bump.png', LighterAndBumpTex[0]);
  LoadNormalMap('brick_1_normalmap.png', NormalMap[0]);

  LoadTextureBumpMap('brick_2.png'      , 'brick_2_bump.png', NormalAndBumpTex[1]);
  LoadTextureBumpMap('brick_2_light.png', 'brick_2_bump.png', LighterAndBumpTex[1]);
  LoadNormalMap('brick_2_normalmap.png', NormalMap[1]);

  LoadTextureBumpMap('brick_1.png'      , 'brick_1_bump_better.png', NormalAndBumpTex[2]);
  LoadTextureBumpMap('brick_1_light.png', 'brick_1_bump_better.png', LighterAndBumpTex[2]);
  LoadNormalMap('brick_1_normalmap_worse.png', NormalMap[2]);

  LoadTextureBumpMap('shell.png'      , 'shell_bump.png', NormalAndBumpTex[3]);
  LoadTextureBumpMap('shell.png'      , 'shell_bump.png', LighterAndBumpTex[3]);
  LoadNormalMap('shell_normalmap.png', NormalMap[3]);

  NormalizationCubeTex := MakeNormalizationCubeMap;

  Font := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans);
end;

procedure Close(glwin: TCastleWindowBase);
begin
  FreeAndNil(Font);
  Scene.GLContextClose;
end;

procedure Resize(Window: TCastleWindowBase);
begin
  NextButton.Left := Window.Width - NextButton.Width - 10;
  NextButton.Bottom := Window.Height - NextButton.Height - 10;
end;

procedure Idle(glwin: TCastleWindowBase);
var
  LightPositionChanged: boolean;

  procedure ChangeLightPosition(Coord, Change: Integer);
  begin
    LightPosition[Coord] += Change * Glwin.Fps.IdleSpeed * 50 / 10;
    LightPositionChanged := true;
  end;

begin
  LightPositionChanged := false;

  if Glwin.Pressed[K_A] then ChangeLightPosition(0, -1);
  if Glwin.Pressed[K_D] then ChangeLightPosition(0,  1);
  if Glwin.Pressed[K_S] then ChangeLightPosition(1, -1);
  if Glwin.Pressed[K_W] then ChangeLightPosition(1,  1);

  if Glwin.Pressed[K_Q] then
  begin
    if mkShift in Glwin.Pressed.Modifiers then
      ChangeLightPosition(2,  1) else
      ChangeLightPosition(2, -1);
  end;

  if LightPositionChanged and (Method = bmVRML) then
    SceneBrightestLight.FdLocation.Send(LightPosition);

  if Glwin.Pressed[K_J] then XShift -= Glwin.Fps.IdleSpeed * 50 / 1000;
  if Glwin.Pressed[K_L] then XShift += Glwin.Fps.IdleSpeed * 50 / 1000;
  if Glwin.Pressed[K_K] then YShift -= Glwin.Fps.IdleSpeed * 50 / 1000;
  if Glwin.Pressed[K_I] then YShift += Glwin.Fps.IdleSpeed * 50 / 1000;

  if Glwin.Pressed[K_E] then
  begin
    if mkShift in Glwin.Pressed.Modifiers then
      EmbossScale -= Glwin.Fps.IdleSpeed * 50 / 10 else
      EmbossScale += Glwin.Fps.IdleSpeed * 50 / 10;
  end;

  RotatingColumn += Glwin.Fps.IdleSpeed * 50 / 5;
end;

{ menu ----------------------------------------------------------------------- }

procedure MenuCommand(glwin: TCastleWindowBase; MenuItem: TMenuItem);

  procedure NextCamera;
  begin
    if SceneManager.Camera = Examiner then
      SceneManager.Camera := Walker else
      SceneManager.Camera := Examiner;
  end;

var
  C: TVector3Single;
  S: string;
begin
  case MenuItem.IntData of
    50 : NextButton.DoClick;
    60 : NextCamera;
    100: Glwin.SaveScreenDialog(FileNameAutoInc(SUnformattable(Parameters[0]) + '_screen_%d.png'));
    200: Glwin.Close;
    300: ShiftByHand := not ShiftByHand;
    400: EmbossAlphaMultiplyByBlending := not EmbossAlphaMultiplyByBlending;
    500: NormalizedDot3First := not NormalizedDot3First;
    600: begin
           S := ExtractFilePath(VrmlFileName);
           if Glwin.FileDialog('Open VRML model', S, true, Load3D_FileFilters) then
           begin
             LoadScene(S);
             VrmlFileName := S;
           end;
         end;
    601: begin
           C := SceneBrightestLight.FdColor.Value;
           if Glwin.ColorDialog(C) then
             SceneBrightestLight.FdColor.Send(C);
         end;
    610: if (Method = bmVRML) and not Scene.BoundingBox.IsEmpty then
         begin
           LightPosition := Scene.BoundingBox.Middle;
           LightPosition[2] := Scene.BoundingBox.Data[1][2];
           SceneBrightestLight.FdLocation.Send(LightPosition);
         end;
    1100..1199: Scene.Attributes.BumpMapping :=
      TBumpMapping(MenuItem.IntData - 1100);
  end;
end;

function CreateMainMenu: TMenu;
var
  M, M2: TMenu;
begin
  Result := TMenu.Create('Main menu');
  M := TMenu.Create('_Program');
    M.Append(TMenuItem.Create('Next _Bump Mapping Method', 50, 'm'));
    M.Append(TMenuItem.Create('Next _Navigation Method', 60, K_Enter));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Save Screen ...', 100, K_F5));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Exit', 200));
    Result.Append(M);

  M := TMenu.Create('_Emboss');
    M.Append(TMenuItemChecked.Create('_Shift Emboss by Hand', 300, 'h',
      ShiftByHand, true));
    M.Append(TMenuItemChecked.Create('_Alpha Multiply by Blending (1 Less Texture Unit Needed)', 400, 'b',
      EmbossAlphaMultiplyByBlending, true));
    Result.Append(M);

  M := TMenu.Create('_Dot3');
    M.Append(TMenuItemChecked.Create('_Display Normalized Version First', 500, 'n',
      NormalizedDot3First, true));
    Result.Append(M);

  M := TMenu.Create('_VRML');
    M.Append(TMenuItem.Create('_Open VRML Model ...', 600, CtrlO));
    M.Append(TMenuSeparator.Create);
    M2 := TMenu.Create('Bump mapping');
      M2.AppendRadioGroup(BumpMappingNames, 1100, Ord(Scene.Attributes.BumpMapping), true);
      M.Append(M2);
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Change Light Color ...', 601));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Position _Light at nice position above VRML model', 610));
    Result.Append(M);
end;

const
  Options: array[0..0] of TOption =
  ( (Short:  #0; Long: 'log'; Argument: oaNone) );

  procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
    const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
  begin
    case OptionNum of
      0: InitializeLog('(no version)');
    end;
  end;

begin
  Window := TCastleWindowCustom.Create(Application);

  OnWarning := @OnWarningWrite;

  { parse params }
  Window.ParseParameters(StandardParseOptions);
  Parameters.Parse(Options, @OptionProc, nil);
  Parameters.CheckHighAtMost(1);
  if Parameters.High >= 1 then
    VrmlFileName := Parameters[1];

  { init cameras }
  Examiner := TExamineCamera.Create(Window);
  Examiner.Init(SceneBoundingBox, 0.1);

  Walker := TWalkCamera.Create(Window);
  Walker.Init(Vector3Single(0, 0, 10), Vector3Single(0, 0, -1),
    Vector3Single(0, 1, 0), Vector3Single(0, 1, 0),
    0, 0 { unused, we don't use Gravity here });

  SceneManager := TMySceneManager.Create(Window);
  Window.Controls.Add(SceneManager);
  SceneManager.Camera := Examiner;

  Window.Controls.Add(TStatusText.Create(Window));

  NextButton := TNextButton.Create(Window);
  Window.Controls.Insert(0, NextButton);

  Scene := TCastleScene.Create(nil);
  LoadSceneCore(VrmlFileName);
  { make octree for fast RenderFrustum }
  Scene.ShapeOctreeProgressTitle := 'Building Shape octree';
  Scene.Spatial := [ssRendering { add here ssDynamicCollisions
    if you want more features, like mouse picking of objects }];
  Scene.ProcessEvents := true;

  RenderParams := TBasicRenderParams.Create;

  { Initialize default Method.
    Scene is part of SceneManager only when Method = bmVRML }
  if Method = bmVRML then
  begin
    PrepareForSceneCore;
    SceneManager.Items.Add(Scene);
    SceneManager.MainScene := Scene;
  end;

  Window.MainMenu := CreateMainMenu;
  Window.OnMenuCommand := @MenuCommand;

  Window.AutoRedisplay := true; { for easy Idle code }
  Window.OnOpen := @Open;
  Window.OnResize := @Resize;
  Window.OnClose := @Close;
  Window.OnIdle := @Idle;
  Window.SetDemoOptions(K_F11, CharEscape, true);

  Window.OpenAndRun;

  FreeAndNil(Scene);
  FreeAndNil(RenderParams);
end.
