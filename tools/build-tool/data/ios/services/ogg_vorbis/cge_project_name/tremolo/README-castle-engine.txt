# Tremolo for iOS (iPhone / iPad etc.)

"Tremolo" is (simplifying a little) a fast native library to read OggVorbis on mobile devices (Android, iOS). See http://wss.co.uk/pinknoise/tremolo/ for the details.

This directory contains a subset of Tremolo, adjusted to compile for iOS. It can be linked with Castle Game Engine (or other) applications, to provide OggVorbis decoding.

We use "pure C" implementation (with ONLY_C define), as the assembler routines in Tremolo have known problems on iOS. See https://stackoverflow.com/questions/2904597/building-arm-assembler-vorbis-decoder-lib-tremolo-for-iphone . We could also fix the assembler routines, but in practice the "pure C" implementation is fast enough for us.

The source code here is originally taken from Tremolo 0.08 release, http://wss.co.uk/pinknoise/tremolo/Tremolo008.zip .

# License

Tremolo source code, as well as all modifications done within the Castle Game Engine project, are licensed on the terms of the BSD license. See tremolo.copying.txt for the details.

# P.S. Tremolo on Android

This directory contains the Tremolo code for iOS.

If you're instead interested in Tremolo adjusted for Android, see https://github.com/michaliskambi/tremolo-android . That one is automatically used by Castle Game Engine on Android.
