{
  Copyright 2009-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @abstract(Current rendering camera (RenderingCamera).)

  @bold(This is an internal and deprecated unit).

  - Normal applications should not access
    camera this way (instead use TCastleAbstractViewport.Camera).
    However, there are some valid uses for it (like querying
    RenderingCamera.Target to selectively hide some stuff).

  - Moreover, using RenderingCamera singleton is deprecated now.
    Use RenderParams.RenderingCamera if you really need it.

  @exclude }
unit CastleRenderingCamera
  deprecated 'RenderingCamera singleton should not be used; most applications should only deal with camera through SceneManager.Camera (TCastleAbstractViewport.Camera); eventually, if doing custom rendering, you can read TRenderParams.RenderingCamera';

{$I castleconf.inc}

interface

uses Generics.Collections,
  CastleUtils, CastleVectors, CastleFrustum, CastleCameras, CastleTransform;

type
  TRenderingCamera = CastleTransform.TRenderingCamera;
  TRenderTarget    = CastleTransform.TRenderTarget;

const
  rtScreen             = CastleTransform.rtScreen            ;
  {$warnings off}
  rfOffScreen          = CastleTransform.rfOffScreen         ;
  {$warnings on}
  rtCubeMapEnvironment = CastleTransform.rtCubeMapEnvironment;
  rtShadowMap          = CastleTransform.rtShadowMap         ;
  rtVarianceShadowMap  = CastleTransform.rtVarianceShadowMap ;

var
  { Current camera used for rendering. }
  RenderingCamera: TRenderingCamera;

implementation

uses SysUtils, CastleLog;

initialization
  RenderingCamera := TRenderingCamera.Create;
finalization
  FreeAndNil(RenderingCamera);
end.
