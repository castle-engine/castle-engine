{%MainUnit castleinternalrenderer.pas}
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

{ Global consts, variables at the end of unit interface. }

{$ifdef read_interface}

const
  AllVboTypes = [Low(TVboType) .. High(TVboType)];

  { This RenderContext.TDepthRange value results from
    using rlParent on Viewport.Items.

    Also this value is reset by TRenderer.RenderEnd.
    It cannot reset to drFull, we need shadow quads to check for depth versus
    scene objects in DefaultDepthRange.

    Testcases: If RenderEnd would reset to drFull:
    - demo-models/shadow_volumes/stonehenge_with_chopper.x3dv
      would have weird shadows in view3dcene on chopper
    - demo-models/shadow_volumes/shadows_worm_in_a_room.x3dv
      would have weird shadows in view3dcene on worm
    - examples/viewport_and_scenes/shadows
      would have weird shadows
    - examples/research_special_rendering_methods/test_rendering_opengl_capabilities/
      would have weird shadows
    - examples/deprecated_to_upgrade/fixed_camera_game/
      would behave weirdly, have confusing shadows,
      suggesting player isn't at the correct place (like floating above floor).

    TODO: Should we set this in TRenderContext constructor, on context,
    so that it's really default value? Right now rendering 1st frame
    with TRenderer effectively changes drFull into drFar for everything.

    TODO: Do shadow volumes work for stuff in rlFront, like weapon in FPS games?
    Likely not, since they are in disjoint range.
  }
  DefaultDepthRange = drFar;

var
  { Log renderer cache events. Allows to see how the cache performs.
    A @italic(lot) of log messages.

    Meaningful only if you initialized log (see CastleLog unit) by InitializeLog first. }
  LogRendererCache: boolean = false;

  { Log various renderer information.

    Meaningful only if you initialized log (see CastleLog unit) by InitializeLog first. }
  LogRenderer: boolean = false;

  { Global renderer cache.
    This caches common things, like textures, shapes, and much more.
    Our OpenGL resources are shared across all OpenGL contexts,
    and everything automatically uses this cache. }
  RendererCache: TRendererCache;

{$endif read_interface}

{$ifdef read_implementation}
{$endif}
