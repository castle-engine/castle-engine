This service is a convenient way to link with [FMOD](https://github.com/castle-engine/castle-engine/wiki/FMOD) on iOS, and point the [build tool](https://github.com/castle-engine/castle-engine/wiki/Build-Tool) to the FMOD library compiled for iOS. Use it like this in [CastleEngineManifest.xml](https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples):

~~~~xml
<ios ...>
  <services>
    <service name="fmod">
      <parameter key="library_file" value="libraries/ios/libfmod_iphoneos.a" />
    </service>
  </services>
</ios>
~~~~

The `library_file` parameter is the location of the FMOD for iOS library. You need to download it from https://www.fmod.com/ yourself.
