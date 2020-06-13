# vibrate

This service enables vibrations on iOS.

Request this service in [CastleEngineManifest.xml](https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples) like this:

~~~~xml
<?xml version="1.0" encoding="utf-8"?>
<project name="..." game_units="...">
  <ios>
    <services>
      <service name="vibrate" />
    </services>
  </ios>
</project>
~~~~

Then use [Vibrate](https://castle-engine.io/apidoc-unstable/html/CastleOpenDocument.html#Vibrate) procedure from the [CastleOpenDocument](https://castle-engine.io/apidoc-unstable/html/CastleOpenDocument.html) unit in your application.
