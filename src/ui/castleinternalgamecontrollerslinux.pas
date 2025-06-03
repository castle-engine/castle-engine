{
  Copyright 2015-2025 Tomasz Wojtyś, Michalis Kamburelis.
  Based on zgl_joystick.pas by Andrey Kemka.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.
  This file is based on ZenGL, with "zlib" license
  ( http://www.zengl.org/license.html ) which is fully compatible with
  Castle Game Engine "LGPL with static linking exception" / GPL licensing.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Linux game controllers support. }
unit CastleInternalGameControllersLinux;

{$I castleconf.inc}

interface

uses BaseUnix,
  CastleGameControllers;

type
  TLinuxControllerManagerBackend = class(TInternalControllerManagerBackend)
    procedure Initialize; override;
    procedure Poll; override;
  end;

implementation

uses SysUtils, Math,
  CastleLog, CastleKeysMouse, CastleVectors;

{ Backend using /dev/js* device.
  See docs:
  - https://www.kernel.org/doc/html/latest/input/joydev/joystick.html
  - https://www.kernel.org/doc/html/latest/input/joydev/joystick-api.html

  TODO:
  - Alternative backend (or remake this one) using /dev/event*
  - Alternative backend using a higher-level library (like SDL2) to handle joysticks?
}

{ Helpful types and constants ------------------------------------------------ }

type
  TLinuxJsEvent = record
    time   : UInt32; // event timestamp in milliseconds
    value  : SmallInt; // value
    EventType  : Byte;     // event type
    number : Byte;     // axis/button number
  end;

const
  ABS_MAX = $3F;

  JS_EVENT_BUTTON = $01; // button pressed/released
  JS_EVENT_AXIS   = $02; // joystick moved
  JS_EVENT_INIT   = $80; // initial state of device

  JSIOCGNAME    = -2142213613;
  JSIOCGAXMAP   = -2143262158;
  JSIOCGAXES    = -2147390959;
  JSIOCGBUTTONS = -2147390958;

  EAGAIN = 11; //Error: "there is no data available right now, try again later"

  AxisMap : array[ 0..17 ] of record
    Axis: TInternalGameControllerAxis;
    Handled: Boolean;
  end = (
    (Axis: jaX; Handled: True),
    (Axis: jaY; Handled: True),
    (Axis: jaZ; Handled: True),

    (Axis: jaU; Handled: True),
    (Axis: jaV; Handled: True),
    (Axis: jaR; Handled: True),

    (Axis: jaZ; Handled: True), // 2 things mapped to jaZ
    (Axis: jaR; Handled: True), // 2 things mapped to jaR

    // https://gist.github.com/emdeex/97b771b264bebbd1e18dd897404040be
    (Axis: jaX; Handled: false), // wheel
    (Axis: jaGas; Handled: true), // gas
    (Axis: jaBrake; Handled: true), // brake
    (Axis: jaX; Handled: false), // hat0x
    (Axis: jaX; Handled: false), // hat0y
    (Axis: jaX; Handled: false),
    (Axis: jaX; Handled: false),
    (Axis: jaX; Handled: false),

    (Axis: jaPovX; Handled: True),
    (Axis: jaPovY; Handled: True)
  );

{ TLinuxControllerBackend -------------------------------------------------- }

type
  TLinuxControllerBackend = class(TInternalControllerBackend)
    DeviceInitialized: Boolean;
    Device  : LongInt;
    AxesMap : array[ 0..ABS_MAX - 1 ] of Byte;
    function AxisLeft: TVector2; override;
    function AxisRight: TVector2; override;
    function AxisLeftTrigger: Single; override;
    function AxisRightTrigger: Single; override;
    function InternalButtonMap(
      const Button: TInternalGameControllerButton): TGameControllerButton;
      override;
    destructor Destroy; override;
  end;

function TLinuxControllerBackend.AxisLeft: TVector2;
begin
  Result := Vector2(
    Controller.InternalAxis[jaX],
    { Y axis should be 1 when pointing up, -1 when pointing down.
      This is consistent with CGE 2D coordinate system
      (and standard math 2D coordinate system). }
    -Controller.InternalAxis[jaY]
  );
end;

function TLinuxControllerBackend.AxisRight: TVector2;
begin
  Result := Vector2(
    Controller.InternalAxis[jaZ], // on Linux, contrary to Windows, this is from jaZ
    { Y axis should be 1 when pointing up, -1 when pointing down.
      This is consistent with CGE 2D coordinate system
      (and standard math 2D coordinate system). }
    -Controller.InternalAxis[jaR]
  );
end;

function TLinuxControllerBackend.AxisLeftTrigger: Single;
begin
  Result := Controller.InternalAxis[jaBrake];
end;

function TLinuxControllerBackend.AxisRightTrigger: Single;
begin
  Result := Controller.InternalAxis[jaGas];
end;

const
  { Map TInternalGameControllerButton to TGameControllerButton.
    Specific to:
    - the XBox Controller, more specifically
      https://en.wikipedia.org/wiki/Xbox_Wireless_Controller
    - and to this controller implementation. }
  XBoxInternalMap: array[TInternalGameControllerButton] of record
    Button: TGameControllerButton;
    Handled: Boolean;
  end = (
    {  0 } (Button: gbSouth; Handled: true),
    {  1 } (Button: gbEast; Handled: true),

    {  2 } (Button: gbNorth; Handled: false),

    {  3 } (Button: gbWest; Handled: true),
    {  4 } (Button: gbNorth; Handled: true),

    {  5 } (Button: gbNorth; Handled: false),

    {  6 } (Button: gbLeftBumper; Handled: true),
    {  7 } (Button: gbRightBumper; Handled: true),

    {  8 } (Button: gbNorth; Handled: false),
    {  9 } (Button: gbNorth; Handled: false),

    { 10 } (Button: gbView; Handled: true),
    { 11 } (Button: gbMenu; Handled: true),
    { 12 } (Button: gbGuide; Handled: true),
    { 13 } (Button: gbLeftStickClick; Handled: true),
    { 14 } (Button: gbRightStickClick; Handled: true),

    // Note: Linux backend cannot detect "share" button at all, it seems
    // (present on XBox controller 3rd revision).

    { 15 } (Button: gbNorth; Handled: false),
    { 16 } (Button: gbNorth; Handled: false),
    { 17 } (Button: gbNorth; Handled: false),
    { 18 } (Button: gbNorth; Handled: false),
    { 19 } (Button: gbNorth; Handled: false),
    { 20 } (Button: gbNorth; Handled: false),
    { 21 } (Button: gbNorth; Handled: false),
    { 22 } (Button: gbNorth; Handled: false),
    { 23 } (Button: gbNorth; Handled: false),
    { 24 } (Button: gbNorth; Handled: false),
    { 25 } (Button: gbNorth; Handled: false),
    { 26 } (Button: gbNorth; Handled: false),
    { 27 } (Button: gbNorth; Handled: false),
    { 28 } (Button: gbNorth; Handled: false),
    { 29 } (Button: gbNorth; Handled: false),
    { 30 } (Button: gbNorth; Handled: false),
    { 31 } (Button: gbNorth; Handled: false)
  );

function TLinuxControllerBackend.InternalButtonMap(
  const Button: TInternalGameControllerButton): TGameControllerButton;
begin
  if not XBoxInternalMap[Button].Handled then
  begin
    WritelnWarning('TGameController.InternalButtonMap: Button %d is not handled by the game controller "%s".', [
      Ord(Button),
      // TODO: Write here also controller index
      Controller.Name
    ]);
  end;
  Result := XBoxInternalMap[Button].Button;
end;

destructor TLinuxControllerBackend.Destroy;
begin
  { Check for DeviceInitialized, since any Device >= 0 is valid in theory. }
  if DeviceInitialized then
    FpClose(Device);
  inherited;
end;

{ TLinuxControllerManagerBackend --------------------------------------------- }

procedure TLinuxControllerManagerBackend.Initialize;
var
  ControllerIndex, j : Integer;
  NewController: TGameController;
  NewControllerBackend: TLinuxControllerBackend;
begin
  for ControllerIndex := 0 to 15 do
  begin
    NewController := TGameController.Create;
    NewControllerBackend := TLinuxControllerBackend.Create(NewController);

    NewControllerBackend.Device := FpOpen( '/dev/input/js' + IntToStr(ControllerIndex), O_RDONLY or O_NONBLOCK );
    if NewControllerBackend.Device < 0 then
      NewControllerBackend.Device := FpOpen( '/dev/js' + IntToStr(ControllerIndex), O_RDONLY or O_NONBLOCK );

    if NewControllerBackend.Device > -1 then
    begin
      NewControllerBackend.DeviceInitialized := true;
      SetLength( NewController.Name, 256 );
      { TODO: why is cast to TIOCtlRequest needed (with FPC 3.3.1-r43920),
        we should probably fix the definition of JSIOCGNAME etc. instead. }
      FpIOCtl( NewControllerBackend.Device, TIOCtlRequest(JSIOCGNAME),    @NewController.Name[ 1 ] );
      FpIOCtl( NewControllerBackend.Device, TIOCtlRequest(JSIOCGAXMAP),   @NewControllerBackend.AxesMap[ 0 ] );
      FpIOCtl( NewControllerBackend.Device, TIOCtlRequest(JSIOCGAXES),    @NewController.InternalAxesCount );
      FpIOCtl( NewControllerBackend.Device, TIOCtlRequest(JSIOCGBUTTONS), @NewController.InternalButtonsCount );

      for j := 0 to NewController.InternalAxesCount - 1 do
      begin
        // debug
        // WritelnLog('Controller %d reports axis %d mapped to %d', [
        //   ControllerIndex,
        //   j,
        //   NewControllerBackend.AxesMap[ j ]
        // ]);
        // if AxisMap[NewControllerBackend.AxesMap[ j ]].Handled then
          case AxisMap[NewControllerBackend.AxesMap[ j ]].Axis of
            jaZ: Include(NewController.InternalCapabilities, jcZ);
            jaR: Include(NewController.InternalCapabilities, jcR);
            jaU: Include(NewController.InternalCapabilities, jcU);
            jaV: Include(NewController.InternalCapabilities, jcV);
            jaPovX, jaPovY: Include(NewController.InternalCapabilities, jcPOV);
          end;
      end;

      for j := 1 to 255 do
        if NewController.Name[ j ] = #0 then
        begin
          SetLength( NewController.Name, j - 1 );
          break;
        end;

      { Checking if controller is a real one,
        because laptops with accelerometer can be detected as a joystick :) }
      if ( NewController.InternalAxesCount >= 2 ) and
         ( NewController.InternalButtonsCount > 0 ) then
      begin
        WritelnLog('CastleGameControllers', 'Detected game controller: %s (ID: %d); Axes: %d; Buttons: %d', [
          NewController.Name,
          ControllerIndex,
          NewController.InternalAxesCount,
          NewController.InternalButtonsCount
        ]);
        List.Add(NewController);
      end else
        FreeAndNil(NewController);
    end else
      FreeAndNil(NewController);
  end;
end;

procedure TLinuxControllerManagerBackend.Poll;
var
  i : Integer;
  Value: Single;
  axis: TInternalGameControllerAxis;
  event : TLinuxJsEvent;
  Controller: TGameController;
  ControllerBackend: TLinuxControllerBackend;
  BytesRead: TSsize;
  ControllerHasBeenDisconnected: Boolean;
begin
  ControllerHasBeenDisconnected := false;
  for I := 0 to List.Count - 1 do
  begin
    Controller := List[I];
    ControllerBackend := Controller.InternalBackend as TLinuxControllerBackend;
    BytesRead := FpRead( ControllerBackend.Device, event, 8 );
    if BytesRead = 8 then
      repeat
        case event.EventType of
          JS_EVENT_AXIS:
            begin
              Value := event.value / 32767;
              if AxisMap[ ControllerBackend.AxesMap[ event.number ] ].Handled then
              begin
                axis := AxisMap[ ControllerBackend.AxesMap[ event.number ] ].Axis;
                if Axis in [jaGas, jaBrake] then
                  Value := (Value + 1.0) / 2.0; // map from [-1,1] to [0,1]
                Controller.InternalAxis[ axis ] := Value;
              end else
              begin
                WritelnLog('Controller %d reports unhandled axis (%d mapped to %d) (value: %f)', [
                  I,
                  event.number,
                  ControllerBackend.AxesMap[ event.number ],
                  Value
                ]);
              end;
            end;
          JS_EVENT_BUTTON:
            case event.value of
              0: Controller.InternalButtonDown[ event.number ] := False;
              1: Controller.InternalButtonDown[ event.number ] := True;
            end;
        end;
        BytesRead := FpRead( ControllerBackend.Device, event, 8 );
      until BytesRead <> 8
    else
      if fpgeterrno <> EAGAIN then
      begin
        WritelnLog('Controller error: possibly "%s" was disconnected.', [Controller.Name]);
        ControllerHasBeenDisconnected := true;
      end;
  end;
  { Do this *after* the loop. If any controller was disconnected,
    this will reinitialize List, so we don't want to be in the middle
    of iteration. }
  if ControllerHasBeenDisconnected then
    Controllers.InternalDisconnected;
end;

end.
