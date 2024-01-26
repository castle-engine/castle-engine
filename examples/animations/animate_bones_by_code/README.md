# Animate Bones by Code

Demo that you can access and animate from code bones of a 3D model.

That is, you can:

1. Design a model in Blender, with animations designed in Blender, and export it to glTF (see https://castle-engine.io/blender and https://castle-engine.io/gltf ). Then load as one `TCastleScene`.

2. From code, you can access particular transformation like `LeftArm := MyScene.Node('LeftArm') as TTransformNode` and transform it using e.g. `LeftArm.Rotation := ...`.

Note:

- Use regular Blender objects organized using Blender parent-child hierarchy (Ctrl+P in Blender).

    If you want to animate them in Blender too (to mix animations created in Blender and by code -- because you can) then set keyframes on objects' location/rotation in Blender. To have multiple Blender actions (e.g. rotations of different transformations) exported to one single glTF animation (like `walk` in this example) just push all actions to 1 _"NLA track"_ with the same name in Blender (`walk` in this case). The Blender->glTF exporter has a checkbox _"Group by NLA Track"_ (_"on"_ by default) to deal with this setup.

- For now, you cannot use Blender armature/bones to express the bones for CGE (`TTransformNode`). Because the only way to export Blender armature/bones -> glTF is to use skinned animation. While CGE will of course play such skinned animation OK, but then you cannot modify the bones from code anymore (see TODO below).

- You should set proper transformation hierarchy in Blender. For each Blender object, assign a parent to it by Ctrl+P. Advised: set also a useful name for Blender object (it will be reflected in glTF and then in `TTransformNode` name). To confirm this is set correctly, this demo application writes to log a raport of your transformations, it should look like this:

    ```
    Found node: Torso, parent: (unnamed)
    Found node: ArmHinge.L, parent: Torso
    Found node: Arm.L, parent: ArmHinge.L
    Found node: Elbow.L, parent: Arm.L
    Found node: Forearm.L, parent: Elbow.L
    Found node: ArmHinge.R, parent: Torso
    Found node: Arm.R, parent: ArmHinge.R
    Found node: Elbow.R, parent: Arm.R
    Found node: Forearm.R, parent: Elbow.R
    Found node: LegHinge.L, parent: Torso
    Found node: Thigh.L, parent: LegHinge.L
    Found node: Knee.L, parent: Thigh.L
    Found node: Calf.L, parent: Knee.L
    Found node: LegHinge.R, parent: Torso
    Found node: Thigh.R, parent: LegHinge.R
    Found node: Knee.R, parent: Thigh.R
    Found node: Calf.R, parent: Knee.R
    Found node: Neck, parent: Torso
    Found node: Head, parent: Neck
    ```

- You should not transform (from code) a bone that is also being animated (e.g. using `MyScene.PlayAnimation` or `MyScene.AutoAnimation`). Transformation of such bone will be overridden by animation at each frame.

- But you *can* transform (from code) a bone that is not being touched by the current animation.

- To make it easy to set hardcoded values, we advise to keep rotations and scale set at "identity" in Blender (you can select all objects in Blender and use Ctrl+A to "Apply" rotation and scale).

- You can test what happens in Blender when you animate given transformation with "Local" pivot. This is what your code will be doing too, so make sure in Blender that it works as you expect.

TODO:

- In the future, it should also be possible to animate bones exposed using `TCastleSceneCore.ExposeTransform`. Instead of accessing `TTransformNode` you would then access `TCastleTransform`. This will have a few advantages:

    - You can see the bones, exposed using `TCastleSceneCore.ExposeTransform`, in CGE editor. You can transform them in CGE editor too. So it will be more obvious what's going on, as it will testable using CGE editor.

    - You can attach behaviors to any `TCastleTransform` (so, also to bones exposed using `TCastleSceneCore.ExposeTransform`). E.g. you will be able to attach a physics rigid body and joint to a particular bone.

- In the future, once we'll perform glTF skinned animation on GPU, it will be possible to do the same trick on meshes animated using skinned animation. Both animating `TTransformNode` and animating exposed bone from `TCastleSceneCore.ExposeTransform` should affect the skinned mesh.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `animate_bones_by_code_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `animate_bones_by_code_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
