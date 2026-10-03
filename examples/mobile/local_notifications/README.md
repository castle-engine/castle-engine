# Local Notifications

Show a notification at a chosen time, even when the application is no longer running, using [TLocalNotifications](https://castle-engine.io/apidoc/html/CastleLocalNotifications.TLocalNotifications.html).

- The _Test_ buttons send, and cancel, simple notifications: one right away, one after 10 seconds, one after 60 seconds. Send one, then close the application (or switch to another one): the notification still arrives.

- The _Garden_ is the use-case notifications are for: something happens in your game while the player is not looking. Plant a seed and the flower blooms after a minute -- and the player is notified, even if the application was closed. Reopen the application to see the flower. _Dig it up_ cancels the notification.

Notifications are shown on Android now (using the `local_notifications` service, declared in `CastleEngineManifest.xml`). On other platforms, `TLocalNotifications` methods do nothing, so the code doesn't need any `{$ifdef}`.

The project also shows how to provide your own status bar icon: `android/notification_icon.png`, white on a transparent background, set as the `small_icon` service parameter.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `local_notifications_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `local_notifications_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.