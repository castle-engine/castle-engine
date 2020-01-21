# test_fairy

This service integrates _Castle Game Engine_ applications with [Test Fairy](https://www.testfairy.com/). It's a nice way to distribute mobile applications to your testers, gather logs, feedback etc.

On iOS, it's a nice alternative to TestFlight. Unlike TestFlight, you do not need Apple to accept your test applications (but you will still need to sign them with a developer key signed by Apple).

Using this service is *not* necessary to use [Test Fairy](https://www.testfairy.com/) in a basic way. That is, you can distribute your apps through TestFairy without integrating their SDK. But it's a nice bonus, see https://docs.testfairy.com/iOS_SDK/Integrating_iOS_SDK.html for information what this SDK adds.

## Parameters

You need to specify additional parameters inside `CastleEngineManifest.xml` when using this service:

~~~~xml
<service name="test_fairy">
  <parameters>
    <parameter name="domain" value="xxxxxx" />
    <parameter name="sdk_app_token" value="SDK-yyyyyy" />
  </parameters>
</service>
~~~~

The "domain" parameter above is the initial component of your TestFairy domain name. E.g. if you login to `https://catgames.testfairy.com/` , then use `<parameter name="domain" name="catgames" />`.

Get your "SDK App Token" from your dashboard settings (accessible like `https://catgames.testfairy.com/settings` , but make sure to change the domain in this URL).
