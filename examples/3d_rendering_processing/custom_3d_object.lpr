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

{ If you want to write your own rendering and collisions code,
  you can define your own T3D descendant.

  Note that most programs should be happy just using the T3D descendants
  already implemented inside our engine. First of all there's TCastleScene,
  that allows to express virtually everything through VRML/X3D nodes,
  and has an optimized renderer, collision solver etc.

  But if you really want, you can define your own T3D descendant.

  Note that the rendering code below uses immediate mode OpenGL.
  It will not work with OpenGL ES.
}

program custom_3d_object;

{$I castleconf.inc}
{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

uses SysUtils, Classes,
  {$ifdef CASTLE_OBJFPC} CastleGL, {$else} GL, GLExt, {$endif}
  CastleVectors, CastleBoxes, CastleWindow, CastleFrustum,
  CastleClassUtils, CastleUtils, CastleTransform,
  CastleGLUtils, CastleFilesUtils, CastleStringUtils, CastleKeysMouse,
  CastleGLBoxes;

type
  TCube = class(T3D)
  public
    procedure Render(const Frustum: TFrustum; const Params: TRenderParams); override;
    function BoundingBox: TBox3D; override;
  end;

procedure TCube.Render(const Frustum: TFrustum; const Params: TRenderParams);
begin
  if GetExists and (not Params.Transparent) and Params.ShadowVolumesReceivers then
  begin
    if not Params.RenderTransformIdentity then
    begin
      glPushMatrix;
      glMultMatrix(Params.RenderTransform);
    end;

    glPushAttrib(GL_ENABLE_BIT);
      glDisable(GL_LIGHTING);
      glDrawBox3DWire(Box3D(
        Vector3(-1, -1, -1),
        Vector3( 1,  1,  1)));
    glPopAttrib;

    if not Params.RenderTransformIdentity then
      glPopMatrix;
  end;
end;

function TCube.BoundingBox: TBox3D;
begin
  Result := Box3D(
    Vector3(-1, -1, -1),
    Vector3( 1,  1,  1));
end;

var
  Window: TCastleWindow;
  Cube: TCube;
begin
  Window := TCastleWindow.Create(Application);

  Cube := TCube.Create(Application);
  Window.SceneManager.Items.Add(Cube);
  Window.SetDemoOptions(K_F11, CharEscape, true);
  Window.OpenAndRun;
end.
