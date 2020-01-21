# test_fairy

This service integrates _Castle Game Engine_ applications with [Test Fairy](https://www.testfairy.com/). It's a nice way to distribute mobile applications to your testers, gather logs, feedback etc.

Using this service is *not* necessary to use [Test Fairy](https://www.testfairy.com/) in a basic way. That is, you can distribute your apps through TestFairy without integrating their SDK. But it's a nice bonus, see https://docs.testfairy.com/Android/Integrating_Android_SDK.html: _"It tells you when and how people are using your app, and provides you with any metrics you may need in order to optimize your user experience and code."_

Note (following TestFairy docs): to gather logs, make sure e to check the _"Log collection"_ checkbox found under the "Insights" tab in [Build Settings](https://docs.testfairy.com/Getting_Started/Version_Settings.html). This can be done after the app was uploaded or the first session performed.

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
