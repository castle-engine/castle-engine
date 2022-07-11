# What to do with node names when importing e.g. glTF?

## Information

Both glTF and X3D allow (optionally) to assing string names to various things in models. Like transformations, meshes, materials.

- In case of glTF, the names are within type-specific lists (because glTF has lists of materials, list of meshes etc.) but even within one type names are not guaranteed to be unique.

    And across types, names are definitely not unique. E.g. Blender exporter can easily just set equal name for glTF node, mesh, material, image... Causing name clashes.

- In case of X3D, node names are global. And they are not guranteed to be unique (even if it's advised).

- Moreover, in CGE, we merge some X3D namespaces. That is, our `TCastleScene.Node` and `TX3DNode.FindNode` methods do not take namespaces (like Inline and prototypes) into account (because in many cases it would be extra complication that is not necessary) and they just search the whole X3D node graph.

## So node name clashes can and certainly will happen. What to do?

- I considered and rejected the idea of prexifing various glTF names with types, like `Group.X3DName := 'Mesh_' + Mesh.Name`. While it helps to avoid name clashes, but

    - It does not eliminate the possibility of name clashes anyway, above information describes many ways name clases could happen anyway.

    - It makes all names look complicated, even if the initial names (in Blender, in glTF) are simple. And this makes things complicated.

      From user perspective, if user creates in Blender something called `"Foo"`, we want to make it accessible in CGE using `Scene.Node('Foo')`. Not `Scene.Node('Mesh_Foo')` or such, as that is surprising.

    - In Pascal API, we have ways to make it better. Like `Scene<TAbstractGeometryNode>.Node('Foo')` or `Scene<TAbstractMaterialNode>.Node('Foo')` that could filter by type and only look for names inside.

- Should we to try to make names unique by adding suffix like `_2`, `_3`?

    - Definitely not in glTF importer, because the problem is independent of glTF. Right now we do it in `TX3DRootNode.InternalFixNodeNames`.

    - Should we do it *at all*? Maybe we should not do unique renames in `TX3DRootNode.InternalFixNodeNames`, and just let names clash. Just like X3D and glTF in the end allow.

      But then, some things will be just not accessible from X3D. Like referencing meshes/nodes using X3D export, from imported glTF file with duplicate names. But then, using suffixes like `_2`, `_3` to access them is not reliable also (the order is implementation-dependant).

      But having unreliable suffixes like `_2`, `_3` is better than nothing. For X3D author. See `x3d-tests/gltf_inlined/avocado_and_exports/avocado_imported.x3dv` testcase.

      For Pascal author, more flexible API like `Scene<TAbstractMaterialNode>.Node('Foo')` is actually the future.

## TODO

- Pascal API with types, to allow `Scene<TAbstractMaterialNode>.Node('Foo')`, to find name `Foo` even if it is shared also by meshes, materials etc.

- Should `TX3DRootNode.InternalFixNodeNames` do unique renames at all? Not doing them is better for Pascal developer using `Scene<TAbstractMaterialNode>.Node('Foo')`.
