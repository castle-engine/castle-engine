# activity_recognition

This service enables activity recognition (whether user is walking, running, cycling etc.) in _Castle Game Engine_ applications on iOS.

## Manifest

Request this service in [CastleEngineManifest.xml](https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples) like this:

~~~~xml
<?xml version="1.0" encoding="utf-8"?>
<project name="..." game_units="...">
  <ios>
    <services>
      <service name="activity_recognition">
        <!-- provide here a reason for user *why* do you need this permission -->
        <parameter key="description" value="This application wants to access your motion." />
      </service>
    </services>
  </ios>
</project>
~~~~
