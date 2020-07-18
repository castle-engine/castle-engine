# download_urls

This service allows to download from `http` and `https` on Android.

Add it to your [CastleEngineManifest.xml](https://github.com/castle-engine/castle-engine/wiki/CastleEngineManifest.xml-examples):

```
<android>
  <services>
    <service name="download_urls" />
  </services>
</android>
```

Then you can use our Pascal API ([Download and TCastleDownload](https://castle-engine.io/manual_network.php)) to download resources from `http` and `https` on Android, just like on other platforms.
