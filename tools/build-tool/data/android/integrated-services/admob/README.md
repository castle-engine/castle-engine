# admob
This service enables the AdMob banner, interstitial and rewarded advertisements. Use [TAds](http://castle-engine.sourceforge.net/apidoc/html/CastleAds.TAds.html) class from the [CastleAds](http://castle-engine.sourceforge.net/apidoc/html/CastleAds.html) unit to show and control the ads from your Pascal code, with `AdNetwork` set to `anAdMob`.

## Requires:
* Using this service requires using also <code>google_play_services</code>.

## App ID

From Ads SDK version 17 you need use App ID to properly initialize ads.
To do this in CGE you need to declare it in `CastleEngineManifest.xml` like this:

~~~~xml
<service name="admob">
    <parameter key="app_id" value="..."/>
</service>
~~~~

### Test App ID
If you just want test admob service you can use test app id: `ca-app-pub-3940256099942544~3347511713`


