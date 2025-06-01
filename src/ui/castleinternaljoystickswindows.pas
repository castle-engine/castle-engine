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

{ Windows Joystick support. }
unit CastleInternalJoysticksWindows;

// TODO: only partially works on Windows (no support of new features); need to be ported to xinput library

interface

{$ifndef MSWINDOWS}
{ To make things easier for packages, this unit is part of Delphi
  castle_engine.bpl package, and so it has to compile on any platform
  (including Linux), but then it just doesn't do anything.
  From code, just don't use this unit when not MSWINDOWS. }
implementation
{$else}

uses CastleJoysticks;

type
  PJOYCAPSW = ^TJOYCAPSW;
  TJOYCAPSW = packed record
    wMid: Word;
    wPid: Word;
    szPname: array[ 0..31 ] of WideChar;
    wXmin: UInt32;
    wXmax: UInt32;
    wYmin: UInt32;
    wYmax: UInt32;
    wZmin: UInt32;
    wZmax: UInt32;
    wNumButtons: UInt32;
    wPeriodMin: UInt32;
    wPeriodMax: UInt32;
    wRmin: UInt32;
    wRmax: UInt32;
    wUmin: UInt32;
    wUmax: UInt32;
    wVmin: UInt32;
    wVmax: UInt32;
    wCaps: UInt32;
    wMaxAxes: UInt32;
    wNumAxes: UInt32;
    wMaxButtons: UInt32;
    szRegKey: array[ 0..31 ] of WideChar;
    szOEMVxD: array[ 0..259 ] of WideChar;
end;

type
  PJOYINFOEX = ^TJOYINFOEX;
  TJOYINFOEX = packed record
    dwSize: UInt32;
    dwFlags: UInt32;
    wXpos: UInt32;
    wYpos: UInt32;
    wZpos: UInt32;
    dwRpos: UInt32;
    dwUpos: UInt32;
    dwVpos: UInt32;
    wButtons: UInt32;
    dwButtonNumber: UInt32;
    dwPOV: UInt32;
    dwReserved1: UInt32;
    dwReserved2: UInt32;
  end;

const
  JOY_POVCENTERED    = -1;
  JOY_POVFORWARD     = 0;
  JOY_POVRIGHT       = 9000;
  JOY_POVBACKWARD    = 18000;
  JOY_POVLEFT        = 27000;
  JOY_RETURNX        = 1;
  JOY_RETURNY        = 2;
  JOY_RETURNZ        = 4;
  JOY_RETURNR        = 8;
  JOY_RETURNU        = 16;
  JOY_RETURNV        = 32;
  JOY_RETURNPOV      = 64;
  JOY_RETURNBUTTONS  = 128;
  JOY_RETURNRAWDATA  = 256;
  JOY_RETURNPOVCTS   = 512;
  JOY_RETURNCENTERED = $400;
  JOY_USEDEADZONE    = $800;
  JOY_RETURNALL      = ( JOY_RETURNX or JOY_RETURNY or JOY_RETURNZ or JOY_RETURNR or JOY_RETURNU or JOY_RETURNV or JOY_RETURNPOV or JOY_RETURNBUTTONS );

  JOYCAPS_HASZ    = 1;
  JOYCAPS_HASR    = 2;
  JOYCAPS_HASU    = 4;
  JOYCAPS_HASV    = 8;
  JOYCAPS_HASPOV  = 16;
  JOYCAPS_POV4DIR = 32;
  JOYCAPS_POVCTS  = 64;

  JOYERR_NOERROR = 0;
  JOYERR_BASE = 160;
  JOYERR_PARMS = JOYERR_BASE + 5; // The specified joystick identifier is invalid.
  JOYERR_UNPLUGGED = JOYERR_BASE + 7; //The specified joystick is not connected to the system.

  WINMMLIB = 'winmm.dll';

  JS_AXIS : array[jaX..jaV] of UInt32 = ( 17 {X}, 19 {Y}, 21 {Z}, 26 {R}, 28 {U}, 30 {V} );

function joyGetNumDevs : UInt32; stdcall; external WINMMLIB name 'joyGetNumDevs';
function joyGetDevCapsW( uJoyID : UInt32; lpCaps : PJOYCAPSW; uSize : UInt32 ) : UInt32; stdcall; external WINMMLIB name 'joyGetDevCapsW';
function joyGetPosEx( uJoyID : UInt32; lpInfo : PJOYINFOEX ) : UInt32; stdcall; external WINMMLIB name 'joyGetPosEx';

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
  _value: Single;
  j: Integer;
  btn   : Integer;
  state : TJOYINFOEX;
  pcaps : PUInt32;
  value : PUInt32;
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

            // Say "no" to if's, and do everything trciky :)
            // TODO: Above is a bad comment and indeed the code is rather messy. Make it more readable.
            Axis := BackendInfo.AxesMap[ j ];
            pcaps := @BackendInfo.Caps;
            Inc( pcaps, JS_AXIS[ Axis ] );
            vMin  := pcaps^;
            Inc( pcaps );
            vMax  := pcaps^;
            value := @state;
            Inc( value, 2 + Ord(Axis) );

            _value := value^ / ( vMax - vMin ) * 2 - 1;

            if Joystick.InternalAxis[Axis] <> _value then
              if Assigned(EventContainer.OnAxisMove) then
                EventContainer.OnAxisMove(Joystick, Axis, _value);
            Joystick.InternalAxis[Axis] := _value;
          end;

          Joystick.InternalAxis[jaPOVX] := 0;
          Joystick.InternalAxis[jaPOVY] := 0;
          if (jcPOV in Joystick.InternalCapabilities) and
             (state.dwPOV and $FFFF <> $FFFF) then
          begin
            _value := Sin( DegToRad(state.dwPOV and $FFFF / 100.0) );
            if Joystick.InternalAxis[ jaPovX ] <> _value then
              if Assigned(EventContainer.OnAxisMove) then
                EventContainer.OnAxisMove(Joystick, jaPovX, _value);
            Joystick.InternalAxis[ jaPovX ] := _value;

            _value := -Cos( DegToRad(state.dwPOV and $FFFF / 100.0 ) );
            if Joystick.InternalAxis[ jaPovY ] <> _value then
              if Assigned(EventContainer.OnAxisMove) then
                EventContainer.OnAxisMove(Joystick, jaPovY, _value);
            Joystick.InternalAxis[ jaPovY ] := _value;
          end;

          for j := 0 to Joystick.InternalButtonsCount - 1 do
          begin
            btn := state.wButtons and ( 1 shl j );
            if ( Joystick.InternalButtonDown[ j ] ) and ( btn = 0 ) then
            begin
              Joystick.InternalButtonPress[ j ] := False;
              if Assigned(EventContainer.OnButtonUp) then EventContainer.OnButtonUp(Joystick, j);
              Joystick.InternalButtonCanPress[ j ] := True;
            end;

            if ( Joystick.InternalButtonCanPress[ j ] ) and ( not Joystick.InternalButtonDown[ j ] ) and ( btn <> 0 ) then
            begin
              Joystick.InternalButtonPress   [ j ] := True;
              if Assigned(EventContainer.OnButtonPress) then EventContainer.OnButtonPress(Joystick, j);
              Joystick.InternalButtonCanPress[ j ] := False;
            end;
            Joystick.InternalButtonDown[ j ] := btn <> 0;
            Joystick.InternalButtonUp[ j ] := btn = 0;
            if Assigned(EventContainer.OnButtonDown) and (btn <> 0) then EventContainer.OnButtonDown(Joystick, j);
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
