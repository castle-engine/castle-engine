{ Define the CONSERVE_TRIANGLE_MEMORY symbol to conserve memory consumed
  by triangles, by turning off some (seldom used / needed) per-triangle
  information.

  This disables:

  - Mailboxes for triangles (see TRIANGLE_OCTREE_USE_MAILBOX below,
    and see TTriangle.Mailbox* fields).

    This may cause a little worse collision speed, but it should not be
    noticeable at all in interactive simulations (like walking on the model),
    it's only noticeable when you render with ray-tracing (and even then,
    the slowdown is not much and depends on the scene anyway).

  - Per-vertex 4D texture coordinates and 3D normal vectors at triangles,
    see TTriangle.TexCoord and TTriangle.Normal.

    This disables ProximitySensor.hitTexCoord_changed
    and ProximitySensor.hitNormal_changed events (they will never be generated).

  - Face indexes stored at triangles.
    This disables the "Edit -> Remove face" feature of view3dscene.
}
{ $define CONSERVE_TRIANGLE_MEMORY}

{ Define TRIANGLE_OCTREE_USE_MAILBOX to use mailboxes for TVRMLTriangleOctree.

  This way we avoid checking multiple times collision of the same ray
  with the same triangle. This may otherwise happen, because a triangle
  may be included in many octree leaves.

  How much the triangle mailbox actually helps depends greatly on
  the kind of the scene,
  and generally in my tests is a noticeable but not terrible advantage.
  For average octree with max-depth = 10 and leaf-capacity = 20 the
  speedup is 1.09 (that is, old time / new time = 1.09). }
{$ifndef CONSERVE_TRIANGLE_MEMORY}
  {$define TRIANGLE_OCTREE_USE_MAILBOX}
{$endif}

{ Define SHAPE_OCTREE_USE_MAILBOX to use in TVRMLShapeOctree the
  mailbox method.
  This helps for TVRMLShapeOctree with a lot of shapes duplicated in leaves.

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
