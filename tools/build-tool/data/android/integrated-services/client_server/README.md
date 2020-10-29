# client_server

This service makes it possible to initialize a TCP/IP client or server on Android.

Request this service in [CastleEngineManifest.xml](https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples) like this:

~~~~xml
<?xml version="1.0" encoding="utf-8"?>
<project name="..." game_units="...">
  <android>
    <services>
      <service name="client_server" />
    </services>
  </android>
</project>
~~~~

Then use [CastleClientServer](https://castle-engine.io/apidoc-unstable/html/CastleClientServer.html) in your application. Examples how to use it are inside [Castle Game Engine examples/tcp_connection subdirectory](https://github.com/castle-engine/castle-engine/tree/master/examples/tcp_connection).
