# Game controllers (joysticks, gamepads)

List all avalaible game controlers (joysticks, gamepads).

Test their inputs: axes and buttons.

Using [Castle Game Engine](https://castle-engine.io/).

## Public API for game controllers

This example shows the public API to access game controllers:

- Access them all using the `Controllers` singleton. Use `Controllers.Initialize` to initialize the list of available game controllers. Use `Controllers.Count` and `Controllers[Index]` to access each game controller as a list.

- Receive game controllers button press / release using regular `TCastleUserInterface.Press` / `TCastleUserInterface.Release`. Just like when you handle key and mouse events, see https://castle-engine.io/view_events . Just check for `EventType = itGameController` to detect game controller button events, like this:

    ```delphi
    function TViewMain.Press(const Event: TInputPressRelease): Boolean;
    begin
      Result := inherited;
      if Result then Exit;

      // Handle game controller inputs, when Event.EventType = itGameController.
      // Check Event.Controller.Button and/or Event.Controller.Meaning .

      if (Event.EventType = itGameController) and
         (Event.Controller.Button = gbMenu) then
      begin
        // ... do something
        Exit(true); // handled
      end;

      if (Event.EventType = itGameController) and
         (Event.Controller.Meaning = gmConfirm) then
      begin
        // ... do something
        Exit(true); // handled
      end;
    end;
    ```

- Observe the game controller axis in your view's `Update` method. Look at specific controller properties like

    - `Controllers[0].AxisTrigger` (1D axis)
    - `Controllers[0].AxisLeft` (2D axis)
    - `Controllers[0].AxisRight` (2D axis)

## Internal API for game controllers

This example is accessing also some internal joystick API, clearly marked by `Internal*` names. In particular,

- `TGameController.InternalAxis`
- `TGameController.InternalButtonDown`

For your usage, you *should not* use this API. The "public API" we recommend above is stable, and much more comfortable. The internal API exposes axis and buttons using integers, and the mapping from these integers to something meaningful (like "which button is A" or "which axis describes left stick") depends on the joystick type.

Our implementation handles

- XBox controllers (on any system except Nintendo Switch)
- Nintendo Switch gamepads

In the future, we plan to extend our support to other popular controller types, like PS or Nintendo controllers plugged to PC (auto-detect them and report proper values). As you see, our API is ready for this, with things like potentially-controller-specific button captions.

In the meantime, if you really need to support additional controller types -> that's a valid reason to look at internal button axis and buttons indexes, I guess :) But please also improve CGE by submitting your new mapping (and how to autodetect your game controller) type, as PR or just a forum post ( https://castle-engine.io/talk.php ).

This example uses this internal API, to allow us to debug game controllers support.

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `gamepads_demo_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `gamepads_demo_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
