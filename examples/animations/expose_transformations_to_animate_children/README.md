Demo of TCastleSceneCore.ExposeTransforms feature.

This allows to

- Expose a "transformation" (usually a bone of an animated model)
  from a model loaded to TCastleScene.

  For example, you can load humanoid.gltf
  bone and expose a bone called 'Hand_R' (right hand).

- "Exposing" means that the bone corresponds to a child TCastleTransform
  which is synchronized with the bone (thus, it will be animated with the bone).

  In this example, it means that you get a TCastleTransform instance named 'Hand_R'
  inside the viewport.

- In effect, you can attach stuff to the bone (e.g. attaching weapons to hand)
  in a natural way, just by creating children TCastleScene instances.

  In this example, we attach an axe or a sword to the hand.
  They are both defined by separate glTF files.

The TCastleSceneCore.ExposeTransforms can be configured in the CGE editor,
the whole construction is friendly to visual editing.
