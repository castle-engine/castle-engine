# download_urls

This service allows to download from `http` and `https` on Android.

Add it to your [CastleEngineManifest.xml](https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples):

~~~~xml
<android>
  <services>
    <service name="download_urls" />
  </services>
</android>
~~~~

Then you can use our Pascal API ([Download and TCastleDownload](https://castle-engine.io/manual_network.php)) to download resources from `http` and `https` on Android, just like on other platforms.

Note: This service is automatically used if your project already declares that it needs to download `https` like this:

```xml
<dependencies>
  <dependency name="Https" /> <!-- read https links -->
</dependencies>
```

In you declare a dependency on `https`, there's no need to request the `download_urls` service on Android explicitly.
