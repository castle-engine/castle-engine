# Gyroscope: tilt a dungeon with a ball, in TCastleWindow (Delphi on Android and iOS)

Demo of using the gyroscope (on Android and iOS) in an application using `TCastleWindow`, compiled using Delphi:

- You see a 3D dungeon (maze) from the top, with a ball inside.

- Rotating (tilting) the device tilts the dungeon. The ball follows the physics, so it rolls "down" inside the tilted dungeon. The dungeon uses a mesh collider (`TCastleMeshCollider`), the ball uses a sphere collider.

- The _"Reset"_ button makes the current device position the "flat" one, and moves the ball back to the initial position.

- When the gyroscope is not available (e.g. on desktops), use the arrow keys to tilt the dungeon.

The gyroscope is accessed using the cross-platform Delphi component `TMotionSensor` (unit `System.Sensors.Components`). See the `code/gameviewmain.pas` for the code.

Some notes about the implementation:

- The gyroscope measures how fast the device rotates. To know the current tilt, we sum the measurements over time. This is simple and reacts immediately, but the errors accumulate. That is one of the reasons for the _"Reset"_ button.

- The code doesn't really rotate the dungeon. A mesh collider is the best choice for a static level, but it cannot be rotated by physics. So we do something that looks exactly the same: we rotate (in the reverse direction) the camera and the gravity direction. See `TViewMain.ApplyTilt`.

- The sensors have differences between platforms: On Android the gyroscope is a separate sensor and reports radians per second. On iOS the gyroscope data is part of the "device motion" sensor and reports degrees per second. The code handles both, see `TViewMain.MotionSensorChoosing`.

The dungeon model in `data/dungeon.x3d` is a simple mesh (floor and walls) generated from a text map.

Using [Castle Game Engine](https://castle-engine.io/).

The user interface is designed for the portrait orientation, and the application locks the screen orientation to portrait by code (see `FormFactor.Orientations` in `code/gameinitialize.pas`).

Note: Do not use Delphi _"Project -> Options -> Application -> Orientation"_ for this. Delphi IDE implements that option by adding a line to the main program file (DPR), and it fails (with an error that `Application.CreateForm` was not found) because the main program file of an application using `TCastleWindow` doesn't look like a standard FMX program.

## Screenshots

![Screenshot](screenshot.png)

## Which sensor component to use to get rotation?

The current example code uses the rotation rate (angular velocity) reported by the gyroscope. To do this, we look at `TMotionSensor` and we have 2 branches in code to account for:

- `TMotionSensorType.Gyrometer3D` (Android)
- and `TMotionSensorType.MotionDetector` (iOS).

The gyroscope measures angular velocity, and we integrate it over time to get the current rotation. That's what the `TiltX := TiltX + DeviceRotationX * SecondsPassed` line in the code is doing.

An alternative way, to achieve a similar effect by querying for the _current rotation_, would be to use the `TOrientationSensor` with `TOrientationSensorType.Inclinometer3D`. The data it reports is coming from a combination of sensors (accelerometer, gyroscope, possibly magnetometer) so it would not be strictly a _"gyroscope demo"_. But it is directly providing the current tilt angles, so in practice it would probably be a better fit to get the effect of this demo (rotating phone rotates a virtual labyrinth).

Note that [Delphi sample code](https://github.com/Embarcadero/RADStudio11Demos/blob/fdbff4181fb6bf9ae0d818bd4ee4b19653bc4be4/Object%20Pascal/Mobile%20Snippets/Gyroscope/uMain.pas#L92) shows that the data obtained from `TCustomOrientationSensor` is still platform-dependent: the axes need per-platform sign flipping.

Note that errors don't accumulate in case of using the `TOrientationSensor` approach. So the need for _"Reset"_ button is less critical.

TODO: Show code, under `{$ifdef USE_ORIENTATION_SENSOR}`, using the `TOrientationSensor` approach.

## Building

This example is designed to be compiled only using [Delphi](https://www.embarcadero.com/products/Delphi), as it uses Delphi-specific units to access the device.

Open this project in Delphi (open the `window_gyroscope_standalone.dproj` file) and compile + run it from Delphi, as usual Delphi application.

To run on Android or iOS, add the platform first: in Delphi IDE, right-click on _"Target Platforms"_ and choose _"Add Platform..."_.
