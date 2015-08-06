These examples were initially developed to show Castle Game Engine on Android.

Note that the engine support for Android is actually so good that these
examples do not contain (anymore) any Android-specific code :)
They can be compiled to any platform, not only Android,
but also standalone (exe, for Windows, Linux, Mac OS X...), or a web browser
plugin.

Compile them using the Castle Game Engine build tool
https://sourceforge.net/p/castle-engine/wiki/Build%20tool/ .

- android_demo/ - the first demo for Android with a 3D scene.
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

If you're interested in Android demos, note that *many other Castle Game Engine examples
and games can be compiled to Android too*. If they contain a CastleEngineManifest.xml
file with "android_source" declaration, they are ready-to-go for Android!
(And even if they don't, it's often trivial to port them to Android,
if only they use CastleWindow. Just add "android_source" with a trivial xxx_android.lpr
file, and make sure that game logic starts in a platform-independent unit
like XxxGame.pas.)
