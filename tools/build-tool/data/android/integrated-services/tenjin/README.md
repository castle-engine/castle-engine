# tenjin

This service integrates your iOS project with [Tenjin](https://www.tenjin.com/), an install attribution service.

See https://docs.tenjin.com/ for an overview of what Tenjin offers. It is a commercial service, although can be used for free with some limits.

You will need to:

- Create an account on https://www.tenjin.com/ , add your game (for Android), and get the API key from https://www.tenjin.io/dashboard/docs (this is similar as for iOS).

- You also need to allow Tenjin to verify purchases done on Google Play. See https://docs.tenjin.com/en/send-events/android.html , section _"Purchase Events"_.

- You also need to download latest release from https://github.com/tenjin/tenjin-android-sdk/releases, unpack it somewhere. Indicate the location of the directory that contains the `tenjin.aar` and `tenjin.jar` files using the parameter `library_path` shown below. (It can be a directory relative to your project, or an absolute path.)

## Declare in CastleEngineManifest.xml

Declare it like this in [CastleEngineManifest.xml](https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples):

~~~~xml
<?xml version="1.0" encoding="utf-8"?>
<project name="..." game_units="...">
  <android min_sdk_version="21">
    <services>
      <service name="tenjin">
        <parameter key="library_path" value="tenjin-android-sdk/" />
        <parameter key="api_key" value="xxx" />
      </service>
    </services>
  </android>
</project>
~~~~

In this example:

- we use Android `tenjin` service and set its parameters

- we bump Android `min_sdk_version` to 21, as required by Tenjin (you can bump it higher if needed, but it must be at least 21).
