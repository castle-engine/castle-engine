{ ----------------------------------------------------------------------------

  Define the CONSERVE_TRIANGLE_MEMORY symbol to conserve memory consumed
  by the triangles, by turning off some (seldom used / needed) per-triangle
  information.

  This disables:

  - Mailboxes for triangles (see TRIANGLE_OCTREE_USE_MAILBOX below,
    and see TTriangle.Mailbox* fields).

    This may cause a little worse collision speed, but it should not be
    noticeable at all in interactive simulations (like walking on the model),
    it's only noticeable when you render with ray-tracing (and even then,
    the slowdown is not much and depends on the scene,
    see TRIANGLE_OCTREE_USE_MAILBOX comments below).

  - Per-vertex 4D texture coordinates and 3D normal vectors at triangles,
    see TTriangle.TexCoord and TTriangle.Normal.

    This disables ProximitySensor.hitTexCoord_changed
    and ProximitySensor.hitNormal_changed events (they will never be generated).

  - Face indexes stored at triangles.
    This disables the "Edit -> Remove face" feature of view3dscene.
}
{ $define CONSERVE_TRIANGLE_MEMORY}

{ ----------------------------------------------------------------------------

  Define CONSERVE_TRIANGLE_MEMORY_MORE to conserve even more triangle memory,
  but at the cost of speed. Some useful triangle values will not
  be precalculated in the memory, but will be recalculated every time they
  are needed.

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
