{ Define TRIANGLE_OCTREE_USE_MAILBOX to use mailboxes for TVRMLTriangleOctree.

  This way we avoid checking multiple times collision of the same ray
  with the same triangle. This may otherwise happen, because a triangle
  may be included in many octree leaves.

  How much the triangle mailbox actually helps depends greatly on
  the kind of the scene,
  and generally in my tests is a noticeable but not terrible advantage.
  For average octree with max-depth = 10 and leaf-capacity = 20 the
  speedup is 1.09 (that is, old time / new time = 1.09). }
{$define TRIANGLE_OCTREE_USE_MAILBOX}

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
