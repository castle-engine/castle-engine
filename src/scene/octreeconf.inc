{ ----------------------------------------------------------------------------

  Define the CONSERVE_TRIANGLE_MEMORY symbol to decrease memory consumed
  by the triangles (in spatial structures created when you set
  "TCastleScene.Spatial").
  Defining this symbol removes some (seldom used) per-triangle information:

  - Per-vertex texture coordinates and per-vertex normal vectors at triangles
    (see TTriangle.TexCoord and TTriangle.Normal).

    Not having this information, we cannot implement the X3D events
    ProximitySensor.hitTexCoord_changed and
    ProximitySensor.hitNormal_changed (they will never be generated).

    We also cannot generate textures or produce smooth normal vectors
    in ray-tracer.

  - Face indexes stored at triangles.

    Not having this information, we cannot implement the feature
    "Edit -> Remove face" in castle-model-viewer.

  - Defining this also disables the "mailbox" speed optimization for triangles.

    This may cause a little worse collision speed, but it should not be
    noticeable at all in interactive simulations (like FPS walking).
    The slowdown is only noticeable when you use ray-tracing,
    and even then the slowdown is small and depends on the scene.

    See the TRIANGLE_OCTREE_USE_MAILBOX symbol below,
    and see the TTriangle.Mailbox* fields.
}
{ $define CONSERVE_TRIANGLE_MEMORY}

{ ----------------------------------------------------------------------------

  Define CONSERVE_TRIANGLE_MEMORY_MORE to conserve even more triangle memory,
  but at the cost of speed. Some useful triangle values will not
  be precalculated in the memory, but will be recalculated every time they
  are needed.

  Note that defining CONSERVE_TRIANGLE_MEMORY and CONSERVE_TRIANGLE_MEMORY_MORE
  is independent, you can define any combination of them.
  For now, we advice first trying with CONSERVE_TRIANGLE_MEMORY,
  and eventually adding CONSERVE_TRIANGLE_MEMORY_MORE if you really must.

  This will affect collision detection speed. The difference in normal
  FPS games is probably not noticeable, but if you do something intensive,
  like ray-tracing, it will be noticeable. The slowdown is about 1.1-1.23.

  $ ./rayhunter classic 3 1000 1000  ../demo-models/lights_materials/raytracer/area_light_test_final.wrl /tmp/a.png
  Undefined:
  Rendering done in 3.90 seconds.
  Defined:
  Rendering done in 4.30 seconds.

  ./rayhunter path 2 2 2000 1000  ../demo-models/lights_materials/raytracer/area_light_test_final.wrl /tmp/a.png
  Undefined:
  Rendering done in 67.00 seconds.
  Defined:
  Rendering done in 82.83 seconds.
}
{ $define CONSERVE_TRIANGLE_MEMORY_MORE}

{ ----------------------------------------------------------------------------

  Define TRIANGLE_OCTREE_USE_MAILBOX to use "mailboxes" optimization
  for the triangle octree.

  This way we avoid checking multiple times collision of the same ray
  with the same triangle. Otherwise, it could be checked many times,
  because a triangle may be duplicated in many octree leaves.

  How much the triangle mailbox actually helps depends greatly on
  the kind of the scene,
  and generally in my tests is a noticeable but not terrible advantage.
  For average octree with max-depth = 10 and leaf-capacity = 20 the
  speedup is 1.09 (that is, old time / new time = 1.09).
}
{$ifndef CONSERVE_TRIANGLE_MEMORY}
  {$define TRIANGLE_OCTREE_USE_MAILBOX}
{$endif}

{ ----------------------------------------------------------------------------

  Define SHAPE_OCTREE_USE_MAILBOX to use "mailboxes" optimization
  for the shape octree.
  This helps for octrees with a lot of shapes duplicated in leaves.

  E.g. caste's gate_final.wrl speed --- more than 2 times speedup
  with rayhunter using OctreeDynamicCollisions! (from 12.87 to 5.20).
  Tests show that this helps for any model where shapes are duplicated > 1.

  For models where shapes are not duplicated (like alien_mirror.wrl),
  this makes a slight slowdown (from to 1.38 to 1.49), but I think that's
  acceptable. Trivial models are fast enough anyway. And note that ray-tracers
  are free to use OctreeVisibleTriangles, not based on shapes,
  since the scene is static there. This is really only for interactive usage
  with OctreeDynamicCollisions and such.
}
{$define SHAPE_OCTREE_USE_MAILBOX}
