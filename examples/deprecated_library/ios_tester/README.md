# iOS example using CGE as a static library with C API to load and visualize 3D models

The ios_tester project is prepared in Xcode, written in Objective-C.
Before you begin, please compile the CGE library:

1. go to `src/deprecated_library`
2. run `sh compile-iOS.sh`

This produces libcastleengine.a file. Just leave it inside its directory.

To compile ios_tester, you also need to install CocoaPods, see [the Manual](https://castle-engine.io/ios).

Then,

1. run `pod install` in Terminal inside the main ios_tester directory
2. open **ios_tester.xcworkspace** (not the xcodeproj!) in Xcode
3. select your team in the project settings for Code Signing

