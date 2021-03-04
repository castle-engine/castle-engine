This service is a convenient way to link with [FMOD](https://github.com/castle-engine/castle-engine/wiki/FMOD) on Android, and point the [build tool](https://github.com/castle-engine/castle-engine/wiki/Build-Tool) to the FMOD library compiled for Android. Use it like this in [CastleEngineManifest.xml](https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples):

~~~~xml
  <android project_type="integrated">
    <services>
      <service name="fmod">
        <parameter key="library_folder" value="libraries/android" />
      </service>
    </services>
  </android>
~~~~

The `library_folder` parameter is the location of the FMOD for Android library. Note that you should point `library_folder` to the contents of `fmodstudioapi(VersionNumber)android\api\core\lib` as it contains libraries for different CPU architectures possible on Android devices. You need to download it from https://www.fmod.com/ yourself.
