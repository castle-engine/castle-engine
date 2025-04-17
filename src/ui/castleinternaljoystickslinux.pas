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

  JS_AXIS : array[ 0..17 ] of Byte = ( JOY_AXIS_X, JOY_AXIS_Y, JOY_AXIS_Z, JOY_AXIS_U, JOY_AXIS_V, JOY_AXIS_R, JOY_AXIS_Z, JOY_AXIS_R, 0, 0, 0, 0, 0, 0, 0, 0, JOY_POVX, JOY_POVY );

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
      SetLength( NewJoystick.Info.Name, 256 );
      { TODO: why is cast to TIOCtlRequest needed (with FPC 3.3.1-r43920),
        we should probably fix the definition of JSIOCGNAME etc. instead. }
      FpIOCtl( NewBackendInfo.Device, TIOCtlRequest(JSIOCGNAME),    @NewJoystick.Info.Name[ 1 ] );
      FpIOCtl( NewBackendInfo.Device, TIOCtlRequest(JSIOCGAXMAP),   @NewBackendInfo.AxesMap[ 0 ] );
      FpIOCtl( NewBackendInfo.Device, TIOCtlRequest(JSIOCGAXES),    @NewJoystick.Info.Count.Axes );
      FpIOCtl( NewBackendInfo.Device, TIOCtlRequest(JSIOCGBUTTONS), @NewJoystick.Info.Count.Buttons );

      for j := 0 to NewJoystick.Info.Count.Axes - 1 do
        with NewJoystick.Info do
          case NewBackendInfo.AxesMap[ j ] of
            2, 6:   Caps := Caps or JOY_HAS_Z;
            5, 7:   Caps := Caps or JOY_HAS_R;
            3:      Caps := Caps or JOY_HAS_U;
            4:      Caps := Caps or JOY_HAS_V;
            16, 17: Caps := Caps or JOY_HAS_POV;
          end;

      for j := 1 to 255 do
        if NewJoystick.Info.Name[ j ] = #0 then
          begin
            SetLength( NewJoystick.Info.Name, j - 1 );
            break;
          end;

      // Checking if joystick is a real one, because laptops with accelerometer can be detected as a joystick :)
      if ( NewJoystick.Info.Count.Axes >= 2 ) and ( NewJoystick.Info.Count.Buttons > 0 ) then
      begin
        WritelnLog('CastleJoysticks Init', 'Find joy: %s (ID: %d); Axes: %d; Buttons: %d', [NewJoystick.Info.Name, I, NewJoystick.Info.Count.Axes, NewJoystick.Info.Count.Buttons]);
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
  axis: Byte;
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
              axis := JS_AXIS[ BackendInfo.AxesMap[ event.number ] ];
              Value := event.value / 32767;
              { Y axis should be 1 when pointing up, -1 when pointing down.
                This is consistent with CGE 2D coordinate system
                (and standard math 2D coordinate system). }
              if Axis = JOY_AXIS_Y then
                Value := -Value;
              Joystick.State.Axis[ axis ] := Value;
              if Assigned(EventContainer.OnAxisMove) then EventContainer.OnAxisMove(Joystick, axis, Value);
            end;
          JS_EVENT_BUTTON:
            case event.value of
              0:
                begin
                  if Joystick.State.BtnDown[ event.number ] then
                  begin
                    Joystick.State.BtnUp[ event.number ] := True;
                    Joystick.State.BtnPress   [ event.number ] := False;
                    if Assigned(EventContainer.OnButtonUp) then EventContainer.OnButtonUp(Joystick, event.number);
                    Joystick.State.BtnCanPress[ event.number ] := True;
                  end;

                  Joystick.State.BtnDown[ event.number ] := False;
                end;
              1:
                begin
                  Joystick.State.BtnDown[ event.number ] := True;
                  if Assigned(EventContainer.OnButtonDown) then EventContainer.OnButtonDown(Joystick, event.number);
                  Joystick.State.BtnUp  [ event.number ] := False;
                  if Joystick.State.BtnCanPress[ event.number ] then
                    begin
                      Joystick.State.BtnPress   [ event.number ] := True;
                      if Assigned(EventContainer.OnButtonPress) then EventContainer.OnButtonPress(Joystick, event.number);
                      Joystick.State.BtnCanPress[ event.number ] := False;
                    end;
                end;
            end;
        end;
        BytesRead := FpRead( BackendInfo.Device, event, 8 );
      until BytesRead <> 8
    else
      if fpgeterrno <> EAGAIN then
      begin
        WritelnLog('Joystick error: possibly "%s" was disconnected.', [Joystick.Info.Name]);
        JoystickHasBeenDisconnected := true;
      end;
  end;
  if JoystickHasBeenDisconnected then
    if Assigned(Joysticks.OnDisconnect) then
      Joysticks.OnDisconnect;
end;

end.
