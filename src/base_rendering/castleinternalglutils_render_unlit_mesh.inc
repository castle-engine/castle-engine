{%MainUnit castleinternalglutils.pas}
{
  Copyright 2001-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Part of CastleInternalGLImages unit: drawing 2D images on screen (TDrawableImage class). }

{$ifdef read_interface}

type
  { Render a set of vertexes, with optional indexes.
    Can be used multiple times, to render the same vertexes many times,
    just make sure all usage is within the same rendering context.

    This is a deliberately simple and @italic(thin) wrapper over creating
    and drawing a bunch of vertexes on modern GPUs.
    E.g. on OpenGL(ES) it creates 1 VBO (vertex buffer object) for position data,
    and optional 1 VBO for indexes.

    This is suitable to render only trivial unlit (or invisible) meshes.
    @italic(This is not a full-featured mesh renderer.)
    It doesn't define any per-vertex attributes other than vertex positions.
    For a full-featured mesh rendering, just use TCastleScene, and construct your
    mesh as TIndexedTriangleSetNode or TIndexedFaceSetNode, see
    https://castle-engine.io/viewport_and_scenes_from_code#_building_a_mesh_using_code .

    For shading this uses a simple shader:

    @unorderedList(
      @link(If @link(UseColor) (default @true) then the shader writes @link(Color)
        to the color buffer. Effectively this makes unlit rendering.

        Note: This class does not set up blending if @link(Color) alpha is less than 1.
      )

      @link(If @link(UseColor) is @false then the shader writes undefined
        value to the color buffer.

        But it still writes proper values to depth and stencil.

        This is esp. useful when you render things with color buffer disabled
        (using @code(RenderContext.ColorChannels := [])).
        The rendering may still be useful, e.g. to fill depth buffer (for shadow maps),
        stencil buffer, or define shape for GPU occlusion query.

        Note: This class does not configure @code(RenderContext.ColorChannels).
        It is up to you to decide how to hide from user the undefined effect
        this has on the color buffer.
      )

      @link(
        TODO: Maybe in the future it will be extended to optionally use a provided Shader.
        Potential docs:

        Uses the provided @link(Shader) for shading, which must be linked when calling @link(Draw).
        If the OpenGL(ES) does not support shaders or @link(Shader) is @nil,
        this will use built-in shader based on @link(UseColor).
      )
    )
  }
  TCastleRenderUnlitMesh = class
  end;

{$endif read_interface}

{$ifdef read_implementation}

{$endif read_implementation}
