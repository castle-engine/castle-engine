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

{ Windows Joystick support. }
unit CastleInternalJoysticksWindows;

{$I castleconf.inc}

interface

{$ifndef MSWINDOWS}
{ To make things easier for packages, this unit is part of Delphi
  castle_engine.bpl package, and so it has to compile on any platform
  (including Linux), but then it just doesn't do anything.
  From code, just don't use this unit when not MSWINDOWS. }
implementation
{$else}

// TODO: Make alternative implementation based on xinput library

uses MMSystem,
  CastleJoysticks;

type
  TWindowsJoystickBackendInfo = class
    Caps    : TJOYCAPSW;
    AxesMap : array[ 0..5 ] of TInternalGamepadAxis;
  end;

  TWindowsJoysticksBackend = class(TJoysticksBackend)
    procedure Initialize(const List: TJoystickList); override;
    procedure Poll(const List: TJoystickList;
      const EventContainer: TJoysticks); override;
  end;

implementation

uses
  SysUtils, Math,
  CastleLog,
  { Needed to have PUInt32 defined for Delphi 10.2 }
  CastleUtils;

procedure TWindowsJoysticksBackend.Initialize(const List: TJoystickList);
var
  i, j : Integer;
  axis : Integer;
  NewJoystick: TJoystick;
  NewBackendInfo: TWindowsJoystickBackendInfo;

  state : TJOYINFOEX;
  JoyError: UInt32;

  JoyCapsResult: UInt32;
begin
  j := joyGetNumDevs();
  for i := 0 to j - 1 do
  begin
    NewJoystick := TJoystick.Create;
    NewBackendInfo := TWindowsJoystickBackendInfo.Create;
    NewJoystick.InternalBackendInfo := NewBackendInfo;

    JoyCapsResult := joyGetDevCapsW( i, @NewBackendInfo.Caps, SizeOf( TJOYCAPSW ) );
    { Below we try to counter the WinAPI bug, when in case the application was run
      with no joysticks connected and this is the first call to joyGetDevCapsW
      after the joystick has been connected, the joyGetDevCapsW returns JOYERR_PARAMS = 165
      which is also returned for disconnected/unavailable joysticks.
      Here we just call joyGetDevCapsW the second time to get the new value. }
    if JoyCapsResult <> 0 then
      JoyCapsResult := joyGetDevCapsW( i, @NewBackendInfo.Caps, SizeOf( TJOYCAPSW ) );

    if JoyCapsResult = 0 then
    begin
      NewJoystick.Name         := NewBackendInfo.Caps.szPname;
      NewJoystick.InternalAxesCount    := NewBackendInfo.Caps.wNumAxes;
      NewJoystick.InternalButtonsCount := NewBackendInfo.Caps.wNumButtons;

      NewBackendInfo.AxesMap[ 0 ] := jaX;
      NewBackendInfo.AxesMap[ 1 ] := jaY;
      axis := 2;
      if NewBackendInfo.Caps.wCaps and JOYCAPS_HASZ > 0 then
      begin
        Include(NewJoystick.InternalCapabilities, jcZ);
        NewBackendInfo.AxesMap[ axis ] := jaZ;
        Inc( axis );
      end;
      if NewBackendInfo.Caps.wCaps and JOYCAPS_HASR > 0 then
      begin
        Include(NewJoystick.InternalCapabilities, jcR);
        NewBackendInfo.AxesMap[ axis ] := jaR;
        Inc( axis );
      end;
      if NewBackendInfo.Caps.wCaps and JOYCAPS_HASU > 0 then
      begin
        Include(NewJoystick.InternalCapabilities, jcU);
        NewBackendInfo.AxesMap[ axis ] := jaU;
        Inc( axis );
      end;
      if NewBackendInfo.Caps.wCaps and JOYCAPS_HASV > 0 then
      begin
        Include(NewJoystick.InternalCapabilities, jcV);
        NewBackendInfo.AxesMap[ axis ] := jaV;
        Inc( axis );
      end;
      if NewBackendInfo.Caps.wCaps and JOYCAPS_HASPOV > 0 then
      begin
        Include(NewJoystick.InternalCapabilities, jcPOV);
        Inc( NewJoystick.InternalAxesCount, 2 );
      end;

      // workaround Windows reporting recently disconnected joysticks as connected
      state.dwSize := SizeOf( TJOYINFOEX );
      state.dwFlags := JOY_RETURNALL or JOY_USEDEADZONE;
      if NewBackendInfo.Caps.wCaps and JOYCAPS_POVCTS > 0 then
        state.dwFlags := state.dwFlags or JOY_RETURNPOVCTS;
      JoyError := joyGetPosEx( i, @state );
      //if no errors, then add this joystick
      if JoyError = JOYERR_NOERROR then
      begin
        WriteLnLog('CastleJoysticks Init', 'Found gamepad: %s (ID: %d); Axes: %d; Buttons: %d', [
          NewJoystick.Name,
          i,
          NewJoystick.InternalAxesCount,
          NewJoystick.InternalButtonsCount
        ]);

        List.Add(NewJoystick);
      end else
      begin
        if JoyError = JOYERR_UNPLUGGED then
          WriteLnLog('CastleJoysticks Init', 'Found gamepad: %s, but it will not be added because it seems to have been disconnected from the system recently (JOYERR_UNPLUGGED).', [
            NewJoystick.Name
          ])
        else
          WriteLnWarning('CastleJoysticks Init', 'Found gamepad: %s, but it cannot be added due to an error %d.', [
            NewJoystick.Name,
            JoyError
          ]);
        FreeAndNil(NewJoystick);
      end;
    end else
      FreeAndNil(NewJoystick);
  end;
end;

procedure TWindowsJoysticksBackend.Poll(const List: TJoystickList;
  const EventContainer: TJoysticks);
var
  i: Integer;
  AxisValueInt: UInt32;
  AxisValueFloat, PovSin, PovCos: Single;
  j: Integer;
  btn   : Integer;
  state : TJOYINFOEX;
  vMin  : UInt32;
  vMax  : UInt32;
  Joystick: TJoystick;
  BackendInfo: TWindowsJoystickBackendInfo;
  JoyError: UInt32;
  JoystickHasBeenDisconnected: Boolean;
  Axis: TInternalGamepadAxis;
begin
  JoystickHasBeenDisconnected := false;
  state.dwSize := SizeOf( TJOYINFOEX );
  for I := 0 to List.Count - 1 do
  begin
    Joystick := List[I];
    BackendInfo := Joystick.InternalBackendInfo as TWindowsJoystickBackendInfo;

    state.dwFlags := JOY_RETURNALL or JOY_USEDEADZONE;
    if BackendInfo.Caps.wCaps and JOYCAPS_POVCTS > 0 then
      state.dwFlags := state.dwFlags or JOY_RETURNPOVCTS;

    JoyError := joyGetPosEx( i, @state );
    case JoyError of
      JOYERR_NOERROR:
        begin
          for j := 0 to Joystick.InternalAxesCount - 1 do
          begin
            //stop if joystick reported more axes than the backend can handle
            if j > High(BackendInfo.AxesMap) then
              Break;

            Axis := BackendInfo.AxesMap[ j ];

            case Axis of
              jaX: begin vMin := BackendInfo.Caps.wXmin; vMax := BackendInfo.Caps.wXmax; end;
              jaY: begin vMin := BackendInfo.Caps.wYmin; vMax := BackendInfo.Caps.wYmax; end;
              jaZ: begin vMin := BackendInfo.Caps.wZmin; vMax := BackendInfo.Caps.wZmax; end;
              jaR: begin vMin := BackendInfo.Caps.wRmin; vMax := BackendInfo.Caps.wRmax; end;
              jaU: begin vMin := BackendInfo.Caps.wUmin; vMax := BackendInfo.Caps.wUmax; end;
              jaV: begin vMin := BackendInfo.Caps.wVmin; vMax := BackendInfo.Caps.wVmax; end;
              else
                begin
                  WriteLnWarning('CastleJoysticks Poll', 'Unknown axis %d for joystick to determine vMin/vMax', [Ord(Axis)]);
                  Continue;
                end;
            end;

            case Axis of
              jaX: AxisValueInt := state.wXpos;
              jaY: AxisValueInt := state.wYpos;
              jaZ: AxisValueInt := state.wZpos;
              jaR: AxisValueInt := state.dwRpos;
              jaU: AxisValueInt := state.dwUpos;
              jaV: AxisValueInt := state.dwVpos;
              else
                begin
                  WriteLnWarning('CastleJoysticks Poll', 'Unknown axis %d for joystick to determine value', [Ord(Axis)]);
                  Continue;
                end;
            end;

            AxisValueFloat := AxisValueInt / ( vMax - vMin ) * 2 - 1;
            Joystick.InternalAxis[Axis] := AxisValueFloat;
          end;

          Joystick.InternalAxis[jaPovX] := 0;
          Joystick.InternalAxis[jaPovY] := 0;
          if (jcPOV in Joystick.InternalCapabilities) and
             (state.dwPOV and $FFFF <> $FFFF) then
          begin
            SinCos( DegToRad(state.dwPOV and $FFFF / 100.0), PovSin, PovCos );
            Joystick.InternalAxis[ jaPovX ] := PovSin;
            Joystick.InternalAxis[ jaPovY ] := PovCos;
          end;

          for j := 0 to Joystick.InternalButtonsCount - 1 do
          begin
            btn := state.wButtons and ( 1 shl j );
            Joystick.InternalButtonDown[ j ] := btn <> 0;
          end;
        end;
      JOYERR_UNPLUGGED:
        begin
          WritelnLog('Joystick %s was disconnected', [Joystick.Name]);
          JoystickHasBeenDisconnected := true;
        end;
      JOYERR_PARMS:
        WriteLnWarning('Joystick %s parameters are no longer valid.', [Joystick.Name]);
      else
        WriteLnWarning('Joystick %s error %d', [Joystick.Name, JoyError]);
    end;
    if JoystickHasBeenDisconnected then
      Joysticks.InternalDisconnected;
  end;
end;

{$endif}

end.
