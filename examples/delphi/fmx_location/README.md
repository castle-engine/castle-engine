# Location (GPS) on FMX (FireMonkey) form, displayed on 3D Earth in TCastleControl

Demo of using the location sensor (GPS; on Android and iOS) in an FMX application, with _Castle Game Engine_ rendering on the same form using `TCastleControl`:

- The top of the form shows (using standard FMX labels) the latitude, longitude and additional information from the location sensor (altitude, speed, heading, accuracy; note that not all of them are available on all devices).

- Below, `TCastleControl` shows the Earth (a sphere with a texture) and a red pin sticking out at your current location. You can drag to rotate the view.

- The button _"Test: Show Warsaw"_ shows a hardcoded location. It allows to test the 3D display without the location sensor, e.g. on desktops.

The location is accessed using the cross-platform Delphi component `TLocationSensor` (unit `System.Sensors.Components`). See the `Unit1.pas` for the code, in particular `TForm1.ShowLocation` shows how to convert the latitude and longitude into a 3D position.

This is only useful with Delphi.

Using [Castle Game Engine](https://castle-engine.io/).

## Permissions

The application needs a permission to access the location:

- Android: The permissions _"Access fine location"_ and _"Access coarse location"_ must be enabled in Delphi _"Project -> Options -> Application -> Uses Permissions"_ (for each Android platform you use). The project file already enables them (`AUP_ACCESS_FINE_LOCATION`, `AUP_ACCESS_COARSE_LOCATION` in the DPROJ file), but check it after adding the Android platform.

    At runtime, the code asks the user for the permission using `PermissionsService.RequestPermissions`, and activates the location sensor only when the permission is granted.

- iOS: The key `NSLocationWhenInUseUsageDescription` must be present in Delphi _"Project -> Options -> Application -> Version Info"_. Delphi adds it by default, you may want to adjust the text.

    Note that you must have also enabled location services on the device itself: chek _"Settings -> Privacy -> Location Services"_ on iOS. If it is disabled system-wide (for every app), the location sensor will not work, and you will not even see a prompt for the permission.

Remember that the device needs some time to determine the location, especially indoors.

## Building

1. Install Delphi packages following https://castle-engine.io/delphi_packages .

2. Open this project in Delphi and compile + run it from Delphi, as usual Delphi application.

    To run on Android or iOS, add the platform first: in Delphi IDE, right-click on _"Target Platforms"_ and choose _"Add Platform..."_.

## Delphi >= 13 required

This example requires Delphi 13 or newer.

Older Delphi versions miss the form event `OnSafeAreaChanged`.
