# Examples that show Castle Game Engine features specific to mobile (Android and iOS)

Note that the examples here *do not contain any Android/iOS non-portable code*.
All these examples can be compiled on any platform: mobile (Android, iOS),
standalone (Windows, Linux, macOS...), console (Nintendo Switch).
We have a cross-platform API for all the demonstrated things.

However some calls (like `Vibrate` or `TGameService.ShowAchievements`)
are just ignored on platforms that don't support them, and in practice only
mobile platforms support them now.

Note that *almost all other Castle Game Engine examples and games
can be compiled to mobile (Android or iOS) as well*.
This directory contains some demos created specifically to show some Android/iOS
features (like vibrations or Google Play Games / Apple Game Center integration),
but most other demos work on mobile as well.
