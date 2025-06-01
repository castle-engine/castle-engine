{
  Copyright 2015-2019 Tomasz Wojtyś, Michalis Kamburelis.
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

{ Linux Joystick support. }
unit CastleInternalJoysticksLinux;

// TODO: drop legacy interface /dev/js#; enable /dev/event#

interface

uses BaseUnix,
  CastleJoysticks;

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
    Axis: TInternalGamepadAxis;
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
  TLinuxJoystickBackendInfo = class
    DeviceInitialized: Boolean;
    Device  : LongInt;
    AxesMap : array[ 0..ABS_MAX - 1 ] of Byte;
    destructor Destroy; override;
  end;

  TLinuxJoysticksBackend = class(TJoysticksBackend)
    procedure Initialize(const List: TJoystickList); override;
    procedure Poll(const List: TJoystickList;
      const EventContainer: TJoysticks); override;
  end;

implementation

uses
  SysUtils, CastleLog, Math;

{ TLinuxJoystickBackendInfo -------------------------------------------------- }

destructor TLinuxJoystickBackendInfo.Destroy;
begin
  { Check for DeviceInitialized, since any Device >= 0 is valid in theory. }
  if DeviceInitialized then
    FpClose(Device);
  inherited;
end;

{ TLinuxJoysticksBackend ----------------------------------------------------- }

procedure TLinuxJoysticksBackend.Initialize(const List: TJoystickList);
var
  i, j : Integer;
  NewJoystick: TJoystick;
  NewBackendInfo: TLinuxJoystickBackendInfo;
begin
  for i := 0 to 15 do
  begin
    NewJoystick := TJoystick.Create;
    NewBackendInfo := TLinuxJoystickBackendInfo.Create;
    NewJoystick.InternalBackendInfo := NewBackendInfo;

    NewBackendInfo.Device := FpOpen( '/dev/input/js' + IntToStr( i ), O_RDONLY or O_NONBLOCK );
    if NewBackendInfo.Device < 0 then
      NewBackendInfo.Device := FpOpen( '/dev/js' + IntToStr( i ), O_RDONLY or O_NONBLOCK );

    if NewBackendInfo.Device > -1 then
    begin
      NewBackendInfo.DeviceInitialized := true;
      SetLength( NewJoystick.Name, 256 );
      { TODO: why is cast to TIOCtlRequest needed (with FPC 3.3.1-r43920),
        we should probably fix the definition of JSIOCGNAME etc. instead. }
      FpIOCtl( NewBackendInfo.Device, TIOCtlRequest(JSIOCGNAME),    @NewJoystick.Name[ 1 ] );
      FpIOCtl( NewBackendInfo.Device, TIOCtlRequest(JSIOCGAXMAP),   @NewBackendInfo.AxesMap[ 0 ] );
      FpIOCtl( NewBackendInfo.Device, TIOCtlRequest(JSIOCGAXES),    @NewJoystick.InternalAxesCount );
      FpIOCtl( NewBackendInfo.Device, TIOCtlRequest(JSIOCGBUTTONS), @NewJoystick.InternalButtonsCount );

      for j := 0 to NewJoystick.AxesCount - 1 do
        case NewBackendInfo.AxesMap[ j ] of
          2, 6:   Include(NewJoystick.Capabilities, jcZ);
          5, 7:   Include(NewJoystick.Capabilities, jcR);
          3:      Include(NewJoystick.Capabilities, jcU);
          4:      Include(NewJoystick.Capabilities, jcV);
          16, 17: Include(NewJoystick.Capabilities, jcPOV);
        end;

      for j := 1 to 255 do
        if NewJoystick.Name[ j ] = #0 then
        begin
          SetLength( NewJoystick.Name, j - 1 );
          break;
        end;

      { Checking if joystick is a real one,
        because laptops with accelerometer can be detected as a joystick :) }
      if ( NewJoystick.Info.Count.Axes >= 2 ) and ( NewJoystick.InternalButtonsCount > 0 ) then
      begin
        WritelnLog('CastleJoysticks Init', 'Find gamepad: %s (ID: %d); Axes: %d; Buttons: %d', [
          NewJoystick.Name,
          I,
          NewJoystick.InternalAxesCount,
          NewJoystick.InternalButtonsCount
        ]);
        List.Add(NewJoystick);
      end else
        FreeAndNil(NewJoystick);
    end else
      FreeAndNil(NewJoystick);
  end;
end;

procedure TLinuxJoysticksBackend.Poll(const List: TJoystickList;
  const EventContainer: TJoysticks);
var
  i : Integer;
  Value: Single;
  axis: TInternalGamepadAxis;
  event : TLinuxJsEvent;
  Joystick: TJoystick;
  BackendInfo: TLinuxJoystickBackendInfo;
  BytesRead: TSsize;
  JoystickHasBeenDisconnected: Boolean;
begin
  JoystickHasBeenDisconnected := false;
  for I := 0 to List.Count - 1 do
  begin
    Joystick := List[I];
    BackendInfo := Joystick.InternalBackendInfo as TLinuxJoystickBackendInfo;
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
                Joystick.State.Axis[ axis ] := Value;
                if Assigned(EventContainer.OnAxisMove) then EventContainer.OnAxisMove(Joystick, axis, Value);
              end else
              begin
                WritelnLog('Joystick %d reports unhandled axis (%d mapped to %d) (value: %f)', [
                  I,
                  event.number,
                  BackendInfo.AxesMap[ event.number ],
                  Value
                ]);
              end;
            end;
          JS_EVENT_BUTTON:
            case event.value of
              0:
                begin
                  if Joystick.InternalButtonDown[ event.number ] then
                  begin
                    Joystick.InternalButtonUp[ event.number ] := True;
                    Joystick.InternalButtonPress   [ event.number ] := False;
                    if Assigned(EventContainer.OnButtonUp) then EventContainer.OnButtonUp(Joystick, event.number);
                    Joystick.InternalButtonCanPress[ event.number ] := True;
                  end;

                  Joystick.InternalButtonDown[ event.number ] := False;
                end;
              1:
                begin
                  Joystick.InternalButtonDown[ event.number ] := True;
                  if Assigned(EventContainer.OnButtonDown) then EventContainer.OnButtonDown(Joystick, event.number);
                  Joystick.InternalButtonUp  [ event.number ] := False;
                  if Joystick.InternalButtonCanPress[ event.number ] then
                    begin
                      Joystick.InternalButtonPress   [ event.number ] := True;
                      if Assigned(EventContainer.OnButtonPress) then EventContainer.OnButtonPress(Joystick, event.number);
                      Joystick.InternalButtonCanPress[ event.number ] := False;
                    end;
                end;
            end;
        end;
        BytesRead := FpRead( BackendInfo.Device, event, 8 );
      until BytesRead <> 8
    else
      if fpgeterrno <> EAGAIN then
      begin
        WritelnLog('Joystick error: possibly "%s" was disconnected.', [Joystick.Name]);
        JoystickHasBeenDisconnected := true;
      end;
  end;
  if JoystickHasBeenDisconnected then
    if Assigned(Joysticks.OnDisconnect) then
      Joysticks.OnDisconnect;
end;

end.
