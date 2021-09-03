# tenjin

This service integrates your iOS project with [Tenjin](https://www.tenjin.com/), an install attribution service.

See https://docs.tenjin.com/ for an overview of what Tenjin offers. It is a commercial service, although can be used for free with some limits.

You will need to create an account on https://www.tenjin.com/ , add your game (for iOS), and get the API key from https://www.tenjin.io/dashboard/docs .

## Declare in CastleEngineManifest.xml

Declare it like this in [CastleEngineManifest.xml](https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples):

~~~~xml
<?xml version="1.0" encoding="utf-8"?>
<project name="..." game_units="...">
  <ios>
    <services>
      <service name="tenjin">
        <parameter key="api_key" value="xxx" />
        <parameter key="user_tracking_usage_description" value="Installation statistics help us improve the game" />
      </service>
    </services>
  </ios>
</project>
~~~~

The `user_tracking_usage_description` value is specified for Apple [NSUserTrackingUsageDescription](https://developer.apple.com/documentation/bundleresources/information_property_list/nsusertrackingusagedescription), it is shown to the user by the iOS, to explain why Tenjin needs to identify user for statistics.
