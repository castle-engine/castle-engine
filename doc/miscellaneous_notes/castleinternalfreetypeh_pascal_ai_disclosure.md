## TODO: CastleInternalFreeTypeHPascal (and friends) is AI-generated

We have [guidelines about AI usage in Castle Game Engine](https://castle-engine.io/ai) that come down to _"you can use AI, but you must remain in complete understanding and make all decisions about the code"_. The simplest practical approach to this is to not generate large pieces of code by AI.

The 4 files implementing font reading using Pascal (exposing FreeType-like API in Pascal) are the **single case (and I don't plan to make more)** where we... break or bend this rule (depending on where you draw the line). This concerns these files:

- `src/fonts/castleinternalfreetypehpascal.pas`
- `src/fonts/castleinternalglyphrasterizer.pas`
- `src/fonts/castleinternalopentypefont.pas`
- `tests/data/fonts/castle_test_cff_generate.py`

These 4 files are largely AI-generated, which is unusual for _Castle Game Engine_. They were reviewed and modified by Michalis, but due to time constraints my understanding of some parts (esp. deep in implementation of `CastleInternalGlyphRasterizer` and `CastleInternalOpenTypeFont`) is admittedly shallow.

_I'm not happy about this_, and I explicitly say "this is a TODO". I want to have font reading code that I fully understand and control. And/or rely on external library that other humans fully understand and control (like existing FreeType library).

## Why this was done and on which platforms it actually matters

This was done as a practical solution for our font-reading issues on platforms where using FreeType library (static or shared) was problematic:

- [Web](https://castle-engine.io/web)

- [Delphi on mobile (Android and iOS)](https://castle-engine.io/delphi_mobile)

- [Nintendo Switch](https://castle-engine.io/nintendo_switch)

On other platforms, this is merely a fallback, in case dynamic FreeType library is not available.

- We use the standard _FreeType_ library available on your system (Linux, FreeBSD, macOS).

- For Windows, we distribute the _FreeType_ DLL with the engine and package it with your application.

- For FPC/Android and FPC/iOS we link FreeType library with your application (see [Android services](https://castle-engine.io/android_services), [iOS services](https://castle-engine.io/ios_services)).

We have a roadmap item to actually avoid this entirely: [generating font resources](https://castle-engine.io/roadmap#fonts) which will avoid the need to read TTF/OTF files at runtime in most cases.
