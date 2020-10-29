Demo of reading glTF (exported from Blender) file with multiple animations,
each animation affecting different objects in the scene.
Then we can use `TTimeSensorNode.Start` and `TTimeSensorNode.Stop` to control
these animations separately.

Note that using `TTimeSensorNode.Start` and `TTimeSensorNode.Stop`
is a bit harder than using `TCastleSceneCore.PlayAnimation`
and `TCastleSceneCore.StopAnimation` (and you will miss some `PlayAnimation`
features, like cross-fading between previous and next animation).
But it allows you to independently control multiple animations
within the same `TCastleSceneCore` instance.
