# What to do with node names when importing e.g. glTF?

## Information

Various model formats (this document focuses on glTF, X3D, VRML) allow (optionally) to assing string names to various things in models. Like transformations, meshes, materials, animations. The name can be set in 3D authoring tools like Blender.

These names are very useful in certain use-cases. They allow to find (from code) a particular thing present in the model, by name. E.g.

- Find a given material to modify it from code.

    ```delphi
    MyMaterial := Scene.Node(TPhysicalMaterialNode, 'MyMaterial') as TPhysicalMaterialNode;
    MyMaterial.BaseColor := Vector3(1, 1, 0);
    ```

- Run animation by name.

    ```delphi
    Scene.PlayAnimation('MyAnimation', { loop } true);
    ```

## Are the names unique?

- In case of glTF, the names are within type-specific lists (because glTF has lists of materials, list of meshes etc.).

    But even within one type (like "material names") names are not guaranteed to be unique. The index of the thing (like a material index in the materials list) is the only way to uniquely identify it.

    And across types (e.g. between materials and textures), names are definitely not unique, and it is commonly used. E.g. Blender exporter can easily just set equal name for glTF node, mesh, material, image... Causing name clashes.

    So "non-unique names" are not only valid, but even common situation in glTF. Materials, meshes easily have non-unique names.

- In case of X3D: Spec says that node names should be unique in their namespace.

- In case of VRML 97: It is explicitly allowed to have non-unique names. The last one wins.

- Moreover, in CGE, we merge some X3D namespaces. That is, our `TCastleScene.Node`, `TX3DNode.FindNode` methods do not take some X3D namespaces (like `Inline` and prototypes) into account (because in many cases it would be extra complication that is not necessary) and they just search the whole X3D node graph.

   So we can have non-unique names considered by `TCastleScene.Node`, even when reading valid X3D when all names are unique in one namespace.

## So node name clashes can and certainly will happen. What to do?

- We considered and rejected the idea of prexifing various glTF names with types, like `Group.X3DName := 'Mesh_' + Mesh.Name`. While it helps to avoid name clashes, but

    - It does not eliminate the possibility of name clashes anyway, above information describes many ways name clases could happen anyway.

    - It makes all names look complicated, even if the initial names (in Blender, in glTF) are simple. And this makes things complicated.

      From user perspective, if user creates in Blender something called `"Foo"`, we want to make it accessible in CGE using `Scene.Node('Foo')`. Not `Scene.Node('Mesh_Foo')` or such, as that is surprising.

    - In Pascal API, we already have a way to make it better:

      Search for node name, specifying also node class as a criteria. Already working: `Scene.Node(TAbstractGeometryNode, 'Foo')` or `Scene.Node(TAbstractMaterialNode, 'Foo')` that can filter by type and only look for names inside.

      And in the future (see `GENERIC_METHODS` define) maybe we can change it to type-safe `Scene.Node<TAbstractGeometryNode>('Foo')` or `Scene.Node<TAbstractMaterialNode>('Foo')`.

- Should we to try to make names unique by adding suffix like `_2`, `_3`?

    - Definitely not in glTF importer, because the problem is independent of glTF. Right now we do it in `TX3DRootNode.InternalFixNodeNames`.

    - Should we do it *at all*? Maybe we should not do unique renames in `TX3DRootNode.InternalFixNodeNames`, and just let names clash. Just like VRML and glTF (and X3D, as implemented in CGE) in the end allow.

      Moreover `TX3DRootNode.InternalFixNodeNames` is not 100% reliable. If you add nodes manually, they are not automatically renamed.

      But then, some things will be just not accessible from X3D. Like referencing meshes/nodes using X3D export, from imported glTF file with duplicate names. But then, using suffixes like `_2`, `_3` to access them is not reliable also (the order is implementation-dependant).

      From POV of X3D author: Having unreliable suffixes like `_2`, `_3` is better than nothing. For X3D author. See `x3d-tests/gltf_inlined/avocado_and_exports/avocado_imported.x3dv` testcase.

      From POV of someone doing conversion to X3D, using castle-model-converter or online version on https://castle-engine.io/convert.php : Also having unreliable suffixes like `_2`, `_3` is better than nothing. Valid input (valid glTF with non-unique names) should result in valid output (valid X3D with unique names).

      For Pascal author: More flexible API like `Scene.Node(TAbstractMaterialNode, 'Foo')` is the future. Name clashes can happen anyway, glTF and X3D and CGE allow them in various situations. Better to accept them. Accepting this allows to have name clash in glTF, and from Pascal still access both versions:

      ```delphi
      MaterialFoo := Scene.Node(TPhysicalMaterialNode, 'Foo');
      MeshFoo := Scene.Node(TGroupNode, 'Foo');
      ```

      Decision: Don't do this. Unreliable suffixes `_2`, `_3` are not that helpful.

## Decision

- Do not rename at all. Do not add prefixes like `Mesh_`, `Material_` . Do not add suffixes like `_2`, `_3`.

- `TX3DRootNode.InternalFixNodeNames` no longer renames nodes (it only fixes nodes referenced in ROUTEs now, since having non-unique names referred by ROUTEs would be just impossible to later correctly read).

**User should guarantee node name is unique (if user wants to later find it), or use node-searching criteria that make it unique, like Pascal API `Scene.Node(TAbstractMaterialNode, 'Foo')` that limit search to specific type.**

## Known issue

- This decision implies we have a known issue when converting other formats (like glTF) to X3D:

    Namely, when the input (like glTF) has non-unique names then output X3D will also have non-unique names.

    And remember that non-unique names are OK in glTF. They are also OK in Blender (e.g. you can have in Blender mesh called `Plane`, and Blender object called `Plane` etc.). glTF and Blender don't use these names as "references".

    This means that converting (using CGE) valid glTF to X3D, can result in invalid X3D. CGE accepts this invalid X3D, only makes a warning, but not all X3D browsers have to be as forgiving.

    In light of the above argumentation, for now we just "live with this problem". It's more important to have a good Pascal API to access stuff, that allows to use in CGE the same names as you set in Blender (and if you make non-unique names, you can deal with them by searching for name+type, or of course by correcting the input model to have unique names).

    Adding prefixes/suffixes wasn't a satisfactory solution, since users didn't know about these prefixes/suffixes (and for users, the order in which we added `_2`, `_3`... was also unclear). Even adding them only at saving was problematic -- because then "load glTF" resulted in different in-memory graph than "load glTF, save to X3D, load X3D".

## We still "invent" names if none provided on input

Note that above doesn't change the fact we _may invent a name, when it is necessary, and input didn't have any name_.

E.g.

- glTF meshes are sometimes unnamed,

- and glTF primitives are always unnamed (they are just part of a mesh, can be referred to by mesh + primitive index).

But we need names for them, to name relevant interpolator nodes and be able to save them. So we make sure that each mesh has a name (if not given, we use names like `Mesh0`, `Mesh1` etc.). And each primitive has a name, like `Mesh0_Primitive0`, `Mesh0_Primitive1` or `MyMeshName_Primitive0`, `MyMeshName_Primitive1`.