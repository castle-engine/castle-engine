# Activity recognition (walking, running) on iOS

Demo of CastleActivityRecognition unit, which relies on iOS service [activity_recognition](https://github.com/castle-engine/castle-engine/blob/master/tools/build-tool/data/ios/services/activity_recognition/README.adoc). See https://castle-engine.io/ios_services for more information about iOS services.

It may be extended in the future to use an equivalent service on Android, with the same Pascal API in Castle Game Engine.

Note that, while it compiles and runs on any platform (including Android and desktops), it is rather useless (will never detect any activity) on other platforms than iOS.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `activity_recognition_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `activity_recognition_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
