# fmod

This service links your project with [FMOD](https://github.com/castle-engine/castle-engine/wiki/FMOD) on iOS.

## Declare in CastleEngineManifest.xml

Declare it like this in [CastleEngineManifest.xml](https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples):

~~~~xml
<?xml version="1.0" encoding="utf-8"?>
<project name="..." game_units="...">
  <ios>
    <services>
      <service name="fmod">
        <parameter key="library_file" value="libraries/ios/libfmod_iphoneos.a" />
      </service>
    </services>
  </ios>
</project>
~~~~

The `library_file` parameter is the location of the FMOD for iOS library. You need to download it from https://www.fmod.com/ yourself.

## Use FMOD sound backend from Pascal

Use `CastleFMODSoundBackend` unit and call `UseFMODSoundBackend`.

That's it, now all our sound API (see [manual chapter about sound](https://castle-engine.io/manual_sound.php) and [API reference of CastleSoundEngine unit](https://castle-engine.io/apidoc-unstable/html/CastleSoundEngine.html)) uses FMOD under the hood.

See https://github.com/castle-engine/castle-engine/wiki/FMOD for more details.
