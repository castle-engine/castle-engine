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
      Call @link(TRenderer.PrepareShape) for all
      the shapes (geometry and state) that you want to later render.
      The order of calling TRenderer.PrepareShape
      methods doesn't matter, also you are free to prepare states that you
      will not actually use later. Of course a state, once prepared,
      may be used in rendering as many times as you want.

      It's important that you have to prepare @italic(every state that
      you plan to later render). During rendring the state
      must have exactly the same (fields, properties) values as when
      it was prepared. In particular, it must have the same
      pointers to nodes Last*/Active* and their contents
      also must be the same. TRenderer.PrepareSha[e]
      may save some associations between objects and OpenGL resources,
      so it's important that the same pointer must always point to the
      same object (until it's unprepared).

      TRenderer.PrepareShape requires active OpenGL context. It doesn't modify
      OpenGL state (only allocates some resources like texture names).
      It cannot be called inside a display list.
    )

    @item(
      When you want to release resources, you should call
      TRenderer.UnprepareShape on
      shapes that you want to change or free. This should be used
      with nodes that were passed as Last*/Active*
      in some State for TRenderer.Prepare.

      Note that you cannot free the nodes before unpreparing them.
      The node instance must remain valid while it's prepared.
    )

    @item(
      Surround rendering of multiple scenes with TRenderer.ViewportRenderBegin,
      TRenderer.ViewportRenderEnd.

      Notes below are for things that render manually inside TCastleTransform.LocalRender
      (and thus are part of TCastleViewport), but are not TCastleScene.
      This affects e.g. TMyMesh that does rendering using TCastleRenderUnlitMesh
      in ../../examples/research_special_rendering_methods/test_rendering_old_opengl/code/gameinitialize.pas .

      1. See TRenderer.ViewportRenderEnd for the up-to-date list of state
         (some managed using RenderContext, some using direct OpenGL(ES) calls)
         that is reset at the end of viewport rendering,
         because before each scene or before each shape it may change and *not*
         be reset.

         When you render manually inside TCastleTransform.LocalRender,
         this state is undefined when you start,
         and you can change it carelessly (since next scene or shape
         will adjust it), though we advise you save/restore it (in case it will matter
         in the future, e.g. the state will move to ViewportRenderBegin and shapes
         will assume it).

      2. See TRenderer.ViewportRenderBegin for the up-to-date list of state
         (some managed using RenderContext, some using direct OpenGL(ES) calls)
         that is initialized at the beginning of viewport rendering,
         because each shape may assume it is such.

         When you render manually inside TCastleTransform.LocalRender,
         you can also assume the state is set to this value,
         and if you change it -- make sure to save/restore it.

      Note that custom rendering inside TCastleTransform.LocalRender
      never happens between TRenderer.RenderBegin and TRenderer.RenderEnd calls.
      It didn't happen in the past (before TShapesRenderer) because
      each scene did it's own RenderBegin+RenderEnd.
      It doesn't happen now (after TShapesRenderer) because
      the RenderBegin+RenderEnd is done at the end of viewport rendering,
      after all TCastleTransform.LocalRender calls ended.
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

      Remember that you can render only shapes that have Shape.State
      prepared by TRenderer.Prepare.
    )

    @item(
      All render commands that affect state (like RenderBegin, RenderEnd,
      ViewportRenderBegin, ViewportRenderEnd)
      must be done in the same OpenGL context, until you finish
      given rendering by ViewportRenderEnd.

      All the commands, including rendering and preparing (like PrepareShape),
      must be done in the OpenGL contexts that share resources.
      CGE TCastleWindow and TCastleControl guarantee this automatically,
      all OpenGL contexts used by CGE share resources.

      When the last OpenGL context is destroyed, all resources are automatically
      released.

      For some things, like textures, you don't need to worry about OpenGL
      context anymore: because outside of CastleInternalRenderer unit,
      you don't reference OpenGL texture resources directly. You only have
      TX3DNode instances, and the fact that their OpenGL counterparts have been
      released probably doesn't bother you.

      For some other things, like TX3DRendererShape.Cache and
      TX3DRendererShape.ProgramCache, you still have to make sure you free
      them before OpenGL context gets destroyed.
      That is why both TCastleViewport and TCastleScreenEffects take good
      care to pass GLContextClose event to TCastleScene.GLContextClose
      which passes it to TGLShape.GLContextClose for all shapes.
    )
  )

  @bold(OpenGL state affecting X3D rendering:)

  Some OpenGL state is unconditionally set by rendering (TRenderer.ViewportRenderBegin,
  TRenderer.RenderBegin, or just each shape rendering).

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

      These are used by @link(TCastleScene) to render
      scenes with a mix of tranparent and opaque objects.
      Only @link(TCastleScene) deals with it (not this renderer),
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
  CastleShapes, CastleInternalGLCubeMaps, CastleClassUtils, CastleInternalCompositeImage,
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
  {$I castleinternalrenderer_renderer.inc}

{$I castleinternalrenderer_final_globals.inc}

{$undef read_interface}

implementation

uses Math,
  CastleStringUtils, CastleGLVersion, CastleLog,
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
{$I castleinternalrenderer_renderer.inc}
{$I castleinternalrenderer_resource.inc}
{$I castleinternalrenderer_texture.inc}
{$I castleinternalrenderer_surfacetextures.inc}
{$I castleinternalrenderer_glsl.inc}
{$I castleinternalrenderer_final_globals.inc}

initialization
  TCastleRenderOptions.DefaultMinificationFilter := minLinearMipmapLinear;
  TCastleRenderOptions.DefaultMagnificationFilter := magLinear;

  RendererCache := TRendererCache.Create;
finalization
  RendererCache.FreeWhenEmpty(@RendererCache);
end.
