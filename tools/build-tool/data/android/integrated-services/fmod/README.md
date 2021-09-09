# fmod

This service links your project with [FMOD](https://github.com/castle-engine/castle-engine/wiki/FMOD) on Android.

## Declare in CastleEngineManifest.xml

Declare it like this in [CastleEngineManifest.xml](https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples):

~~~~xml
<?xml version="1.0" encoding="utf-8"?>
<project name="..." game_units="...">
  <android>
    <services>
      <service name="fmod">
        <parameter key="library_path" value="fmod-android-library" />
      </service>
    </services>
  </android>
</project>
~~~~

You need to download the _FMOD Engine_ for _Android_ from https://www.fmod.com/ yourself (any version 2.x is OK, so just take the latest). Make sure you understand the FMOD licensing and pricing terms. FMOD is not open-source, and it is not free (although they have some clauses friendly to indie game companies without big revenue).

The `library_path` parameter is the location of the FMOD for Android library. You should point `library_path` to the contents of `fmodstudioapi(VersionNumber)android/api/core/lib/`, it contains libraries for different CPU architectures possible on Android devices, it also contains cross-platform jar file. We expect a directory structure with at least files like this:

```
fmodstudioapi(VersionNumber)android/
  api/
    core/
      lib/ <- point the library_path to this directory
        fmod.jar
        arm64-v8a/
          libfmod.so
          libfmodL.so
        armeabi-v7a/
          libfmod.so
          libfmodL.so
```

## Use FMOD sound backend from Pascal

Use `CastleFMODSoundBackend` unit and call `UseFMODSoundBackend`.

That's it, now all our sound API (see [manual chapter about sound](https://castle-engine.io/manual_sound.php) and [API reference of CastleSoundEngine unit](https://castle-engine.io/apidoc-unstable/html/CastleSoundEngine.html)) uses FMOD under the hood.

See https://github.com/castle-engine/castle-engine/wiki/FMOD for more details.
