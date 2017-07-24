Examples developed to show Castle Game Engine on Android and iOS.

Note that the engine support for Android and iOS is so good now that these
examples do not contain (anymore) any Android/iOS-specific code :)
They can be compiled to any platform, not only mobile (Android, iOS),
but also standalone (exe, for Windows, Linux, Mac OS X...),
or a web browser plugin.

Compile them using the Castle Game Engine build tool
https://github.com/castle-engine/castle-engine/wiki/Build-Tool .

- simple_3d_demo/ - simple demo with a 3D scene.
  Tests various things
  - 3D and 2D formats (including png, with our compiled-in
    png support),
  - 2D controls,
  - shader effects and screen effects,
  - touch controls (nice to navigate in the 3D scene on touch devices),
  - application message loop (also for modal boxes, like MessageOK or MessageYesNo,
    and progress bars).

- drawing_toy/ - simple toy to showcase multitouch support.
  Draw with many fingers at once, each finger will have a different color:)

If you're interested in Android and iOS demos, note that
*many other Castle Game Engine examples and games can be compiled to Android or iOS*.
If they contain a CastleEngineManifest.xml file with "game_units" declaration,
they are ready-to-go for Android and iOS!
(And if they don't, it's often trivial to fix them, if only they use CastleWindow.
Just declare game_units="MyGameUnit" in CastleEngineManifest.xml ,
and make sure that game logic starts in a platform-independent unit like MyGameUnit.pas.)
