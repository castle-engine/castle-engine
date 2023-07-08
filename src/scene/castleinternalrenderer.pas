{
  Copyright 2002-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Core of managing resources for shape rendering (like textures, shaders)
  and using them to render shapes.

  This unit is used by internal TShapesRenderer,
  which is used by public TCastleScene in turn.
  Normal user code renders by placing TCastleScene instances in TCastleViewport
  hierarchy, never directly using this unit.

  @bold(Usage:)

  @orderedList(
    @item(
      Before rendering prepare textures using TTextureResources.Prepare
      for all textures. Failure to do so will result in textures not being
      used.

      TGLShape takes care of doing this.

      TTextureResource and descendants have to be freed when the context
      is active. TAbstractTextureNode uses ApplicationProperties.OnGLContextClose
      to free them automatically when last GL context is lost.
    )

    @item(
      Shaders, VBOs and other things in TShapeCache will be prepared
      automatically at first render.
      You can also render with RenderMode = rmPrepare* to prepare them
      explicitly.

      TGLShape takes care of releasing TShapeCache.

      TCastleScene takes care of rendering stuff with
      RenderMode = rmPrepare*.

      Note that TShapeCache has some links to nodes.
      These node references must stay valid (otherwise we could get weird
      bugs, not only SEGFAULTS, also cache mistakes if these nodes would
      be freed and the same pointer allocated for different instance).
      Be sure to unprepare the TShapeCache before nodes are freed.

      Both TX3DRendererShape.Cache and TX3DRendererShape.ProgramCache
      have to be freed when the context is active.
      TGLShape in GLContextClose releases TX3DRendererShape.Cache and
      TX3DRendererShape.ProgramCache.
      That's why both TCastleViewport and TCastleScreenEffects take good
      care to pass GLContextClose event to TCastleScene.GLContextClose
      which passes it to TGLShape.GLContextClose for all shapes.
    )

    @item(
      The resulting OpenGL resources should only live until
      last OpenGL context is lost.

      They can be shared across all OpenGL contexts.
      CGE TCastleWindow and TCastleControl make
      all OpenGL contexts share all resources, so RendererCache can be just
      used freely. Use ApplicationProperties.OnGLContextClose to free
      things when last context is closed.
    )

    @item(
      Notes for things that render manually inside TCastleTransform.LocalRender
      (and thus are part of TCastleViewport), but are not TCastleScene:

      ( This affects e.g. TMyMesh that does rendering using TCastleRenderUnlitMesh
      in ../../examples/research_special_rendering_methods/test_rendering_old_opengl/code/gameinitialize.pas .
      )

      Custom rendering inside TCastleTransform.LocalRender
      never happens between RenderBegin and RenderEnd calls.
      It didn't happen in the past (before TShapesRenderer) because
      each scene did it's own RenderBegin+RenderEnd.
      It doesn't happen now (after TShapesRenderer) because
      the RenderBegin+RenderEnd is done at the end of viewport rendering,
      after all TCastleTransform.LocalRender calls ended.

      ( There was a period when we had ViewportRenderBegin/End,
      but not anymore. )

      See ClassRenderEnd for the up-to-date list of state
      (some managed using RenderContext, some using direct OpenGL(ES) calls)
      that is reset at the end of viewport rendering,
      because before each scene or before each shape it may change and *not*
      be reset.

      When you render manually inside TCastleTransform.LocalRender,
      this state is undefined when you start,
      and you can change it carelessly (since next shape
      will adjust it). Though we advise you save/restore it
      (it case it will matter to other custom rendering code in
      TCastleTransform.LocalRender, that will (even if it shouldn't) assume that
      the state is reasonable).

      But note that, since your rendering happens outside of RenderBegin/End
      and RenderEnd (with ClassRenderEnd) set reasonable state,
      you will not have any "weird" state (like depth range) unless
      some other custom renderer sets it.
    )

    @item(
      Surround shapes rendering between TRenderer.RenderBegin and TRenderer.RenderEnd.

      Between these calls, you should not touch OpenGL state or RenderContext
      yourself at all.
      The renderer may depend that every state change goes through it.
    )

    @item(
      Between TRenderer.RenderBegin and TRenderer.RenderEnd
      you should render the shapes by calling RenderShape.
    )

    @item(
      All render commands that affect state (from RenderBegin to RenderEnd)
      must be done in the same OpenGL context.
    )
  )

  @bold(OpenGL state affecting X3D rendering:)

  Some OpenGL state is unconditionally set by rendering
  (TRenderer.RenderBegin, or just each shape rendering).

  But there's also some OpenGL state that we let affect our rendering.
  This allows you to customize rendering by using normal OpenGL commands
  or RenderContext settings.

  @unorderedList(
    @item(Current glPolygonMode.
      This is used by @link(TCastleScene) to optionally render wireframe.
    )

    @item(Blending settings, controlled using RenderContext.BlendingBegin,
      RenderContext.BlendingEnd.

      Also RenderContext.DepthBufferUpdate.

      These are used by @link(TShapesRenderer) to render
      scenes with a mix of tranparent and opaque objects.
      Only @link(TShapesRenderer) deals with it (not this renderer),
      as doing it correctly requires ordering the shapes.
    )
  )

  The renderer uses arrays in GPU-friendly format defined by TGeometryArrays.
}
unit CastleInternalRenderer;

{$I castleconf.inc}

interface

uses Classes, SysUtils, Generics.Collections,
  {$ifdef FPC} CastleGL, {$else} OpenGL, OpenGLext, {$endif}
  CastleUtils, CastleVectors, X3DFields, X3DNodes, CastleColors,
  CastleInternalX3DLexer, CastleImages, CastleGLUtils, CastleRendererInternalLights,
  CastleGLShaders, CastleGLImages, CastleTextureImages, CastleVideos, X3DTime,
  CastleShapes, CastleClassUtils, CastleInternalCompositeImage,
  CastleInternalGeometryArrays, CastleInternalArraysGenerator, CastleRendererInternalShader,
  CastleRendererInternalTextureEnv, CastleBoxes, CastleTransform, CastleRenderOptions;

{$define read_interface}

type
  {$I castleinternalrenderer_initial_types.inc}
  {$I castleinternalrenderer_cache_types.inc}
  {$I castleinternalrenderer_cache.inc}
  {$I castleinternalrenderer_resource.inc}
  {$I castleinternalrenderer_texture.inc}
  {$I castleinternalrenderer_glsl.inc}
  {$I castleinternalrenderer_pass.inc}
  {$I castleinternalrenderer_shape.inc}
  {$I castleinternalrenderer_custom_shaders.inc}
  {$I castleinternalrenderer_renderer.inc}
  {$I castleinternalrenderer_screen_effects.inc}

{$I castleinternalrenderer_final_globals.inc}

{$undef read_interface}

implementation

uses Math,
  CastleStringUtils, CastleGLVersion, CastleLog, CastleInternalGLCubeMaps,
  X3DCameraUtils, CastleProjection, CastleRectangles, CastleTriangles,
  CastleCameras, CastleSceneInternalShape, CastleApplicationProperties,
  CastleRenderContext, CastleInternalGLUtils;

{$define read_implementation}

{$I castleinternalrenderer_initial_types.inc}
{$I castleinternalrenderer_cache_types.inc}
{$I castleinternalrenderer_cache.inc}
{$I castleinternalrenderer_pass.inc}
{$I castleinternalrenderer_shape.inc}
{$I castleinternalrenderer_meshrenderer.inc} // must be before TRenderer
{$I castleinternalrenderer_custom_shaders.inc}
{$I castleinternalrenderer_renderer.inc}
{$I castleinternalrenderer_resource.inc}
{$I castleinternalrenderer_texture.inc}
{$I castleinternalrenderer_surfacetextures.inc}
{$I castleinternalrenderer_glsl.inc}
{$I castleinternalrenderer_screen_effects.inc}
{$I castleinternalrenderer_final_globals.inc}

initialization
  TCastleRenderOptions.DefaultMinificationFilter := minLinearMipmapLinear;
  TCastleRenderOptions.DefaultMagnificationFilter := magLinear;

  RendererCache := TRendererCache.Create;
finalization
  RendererCache.FreeWhenEmpty(@RendererCache);
end.
