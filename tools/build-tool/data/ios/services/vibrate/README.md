# vibrate

This service enables vibrations on iOS.

Request this service in `CastleEngineManifest.xml` like this:

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

Then use `Vibrate` procedure from `CastleOpenDocument` in your application.
