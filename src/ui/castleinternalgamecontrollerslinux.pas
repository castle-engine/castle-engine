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

    (Axis: jaX; Handled: false),
    (Axis: jaX; Handled: false),
    (Axis: jaX; Handled: false),
    (Axis: jaX; Handled: false),
    (Axis: jaX; Handled: false),
    (Axis: jaX; Handled: false),
    (Axis: jaX; Handled: false),
    (Axis: jaX; Handled: false),

    (Axis: jaPovX; Handled: True),
    (Axis: jaPovY; Handled: True)
  );

type
  TLinuxControllerBackendInfo = class
    DeviceInitialized: Boolean;
    Device  : LongInt;
    AxesMap : array[ 0..ABS_MAX - 1 ] of Byte;
    destructor Destroy; override;
  end;

  TLinuxControllersBackend = class(TGameControllersBackend)
    procedure Initialize(const List: TGameControllerList); override;
    procedure Poll(const List: TGameControllerList;
      const EventContainer: TGameControllers); override;
  end;

implementation

uses
  SysUtils, CastleLog, Math;

{ Using /dev/js* device.
  See docs:
  - https://www.kernel.org/doc/html/latest/input/joydev/joystick.html
  - https://www.kernel.org/doc/html/latest/input/joydev/joystick-api.html

  TODO:
  - Alternative backend (or remake this one) using /dev/event*
  - Alternative backend using a higher-level library (like SDL2) to handle joysticks?
}

{ TLinuxControllerBackendInfo -------------------------------------------------- }

destructor TLinuxControllerBackendInfo.Destroy;
begin
  { Check for DeviceInitialized, since any Device >= 0 is valid in theory. }
  if DeviceInitialized then
    FpClose(Device);
  inherited;
end;

{ TLinuxControllersBackend ----------------------------------------------------- }

procedure TLinuxControllersBackend.Initialize(const List: TGameControllerList);
var
  ControllerIndex, j : Integer;
  NewController: TGameController;
  NewBackendInfo: TLinuxControllerBackendInfo;
begin
  for ControllerIndex := 0 to 15 do
  begin
    NewController := TGameController.Create;
    NewBackendInfo := TLinuxControllerBackendInfo.Create;
    NewController.InternalBackendInfo := NewBackendInfo;

    NewBackendInfo.Device := FpOpen( '/dev/input/js' + IntToStr(ControllerIndex), O_RDONLY or O_NONBLOCK );
    if NewBackendInfo.Device < 0 then
      NewBackendInfo.Device := FpOpen( '/dev/js' + IntToStr(ControllerIndex), O_RDONLY or O_NONBLOCK );

    if NewBackendInfo.Device > -1 then
    begin
      NewBackendInfo.DeviceInitialized := true;
      SetLength( NewController.Name, 256 );
      { TODO: why is cast to TIOCtlRequest needed (with FPC 3.3.1-r43920),
        we should probably fix the definition of JSIOCGNAME etc. instead. }
      FpIOCtl( NewBackendInfo.Device, TIOCtlRequest(JSIOCGNAME),    @NewController.Name[ 1 ] );
      FpIOCtl( NewBackendInfo.Device, TIOCtlRequest(JSIOCGAXMAP),   @NewBackendInfo.AxesMap[ 0 ] );
      FpIOCtl( NewBackendInfo.Device, TIOCtlRequest(JSIOCGAXES),    @NewController.InternalAxesCount );
      FpIOCtl( NewBackendInfo.Device, TIOCtlRequest(JSIOCGBUTTONS), @NewController.InternalButtonsCount );

      for j := 0 to NewController.InternalAxesCount - 1 do
        if AxisMap[NewBackendInfo.AxesMap[ j ]].Handled then
        case AxisMap[NewBackendInfo.AxesMap[ j ]].Axis of
          jaZ: Include(NewController.InternalCapabilities, jcZ);
          jaR: Include(NewController.InternalCapabilities, jcR);
          jaU: Include(NewController.InternalCapabilities, jcU);
          jaV: Include(NewController.InternalCapabilities, jcV);
          jaPovX, jaPovY: Include(NewController.InternalCapabilities, jcPOV);
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

procedure TLinuxControllersBackend.Poll(const List: TGameControllerList;
  const EventContainer: TGameControllers);
var
  i : Integer;
  Value: Single;
  axis: TInternalGameControllerAxis;
  event : TLinuxJsEvent;
  Controller: TGameController;
  BackendInfo: TLinuxControllerBackendInfo;
  BytesRead: TSsize;
  ControllerHasBeenDisconnected: Boolean;
begin
  ControllerHasBeenDisconnected := false;
  for I := 0 to List.Count - 1 do
  begin
    Controller := List[I];
    BackendInfo := Controller.InternalBackendInfo as TLinuxControllerBackendInfo;
    BytesRead := FpRead( BackendInfo.Device, event, 8 );
    if BytesRead = 8 then
      repeat
        case event.EventType of
          JS_EVENT_AXIS:
            begin
              Value := event.value / 32767;
              if AxisMap[ BackendInfo.AxesMap[ event.number ] ].Handled then
              begin
                axis := AxisMap[ BackendInfo.AxesMap[ event.number ] ].Axis;
                Controller.InternalAxis[ axis ] := Value;
              end else
              begin
                WritelnLog('Controller %d reports unhandled axis (%d mapped to %d) (value: %f)', [
                  I,
                  event.number,
                  BackendInfo.AxesMap[ event.number ],
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
        BytesRead := FpRead( BackendInfo.Device, event, 8 );
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
