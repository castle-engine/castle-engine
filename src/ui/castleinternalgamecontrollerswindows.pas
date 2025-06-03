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

{ Windows game controllers support. }
unit CastleInternalGameControllersWindows;

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
  CastleGameControllers;

type
  TWindowsControllerManagerBackend = class(TInternalControllerManagerBackend)
    procedure Initialize; override;
    procedure Poll; override;
  end;

implementation

uses
  SysUtils, Math,
  CastleLog,
  { CastleUtils needed (among other needs) to have PUInt32 defined for Delphi 10.2 }
  CastleUtils, CastleVectors, CastleKeysMouse;

{ TWindowsControllerBackend ------------------------------------------------- -}

type
  TWindowsControllerBackend = class(TInternalControllerBackend)
    WindowsId: Integer;
    Caps    : TJOYCAPSW;
    AxesMap : array[ 0..5 ] of TInternalGameControllerAxis;
    function AxisLeft: TVector2; override;
    function AxisRight: TVector2; override;
    function AxisLeftTrigger: Single; override;
    function AxisRightTrigger: Single; override;
    function InternalButtonMap(
      const Button: TInternalGameControllerButton): TGameControllerButton; override;
  end;

function TWindowsControllerBackend.AxisLeft: TVector2;
begin
  Result := Vector2(
    Controller.InternalAxis[jaX],
    { Y axis should be 1 when pointing up, -1 when pointing down.
      This is consistent with CGE 2D coordinate system
      (and standard math 2D coordinate system). }
    -Controller.InternalAxis[jaY]
  );
end;

function TWindowsControllerBackend.AxisRight: TVector2;
begin
  Result := Vector2(
    Controller.InternalAxis[jaU],
    { Y axis should be 1 when pointing up, -1 when pointing down.
      This is consistent with CGE 2D coordinate system
      (and standard math 2D coordinate system). }
    -Controller.InternalAxis[jaR]
  );
end;

function TWindowsControllerBackend.AxisLeftTrigger: Single;
begin
  if Controller.InternalAxis[jaZ] > 0 then
    Result := Controller.InternalAxis[jaZ]
  else
    Result := 0;
end;

function TWindowsControllerBackend.AxisRightTrigger: Single;
begin
  { Left and right triggers are reported as one axis.

    InternalAxis[jaZ] meaning:
    @unorderedlist(
      @item(1.0 means "only left trigger fully pressed",)
      @item(0.0 means "no trigger pressed, or both triggers pressed,
        or more generally: both triggers pressed with equal strength",)
      @item(-1.0 means "only right trigger fully pressed".)
    )
  }
  if Controller.InternalAxis[jaZ] < 0 then
    Result := Abs(Controller.InternalAxis[jaZ])
  else
    Result := 0;
end;

const
  { Map TInternalGameControllerButton to TGameControllerButton.
    Specific to:
    - the XBox Controller, more specifically
      https://en.wikipedia.org/wiki/Xbox_Wireless_Controller
    - and to this controller implementation, using WinAPI MMSystem. }
  XBoxInternalMap: array[TInternalGameControllerButton] of record
    Button: TGameControllerButton;
    Handled: Boolean;
  end = (
    {  0 } (Button: gbSouth; Handled: true),
    {  1 } (Button: gbEast; Handled: true),
    {  2 } (Button: gbWest; Handled: true),
    {  3 } (Button: gbNorth; Handled: true),
    {  4 } (Button: gbLeftBumper; Handled: true),
    {  5 } (Button: gbRightBumper; Handled: true),
    {  6 } (Button: gbView; Handled: true),
    {  7 } (Button: gbMenu; Handled: true),
    {  8 } (Button: gbLeftStickClick; Handled: true),
    {  9 } (Button: gbRightStickClick; Handled: true),
    { 10 } (Button: gbGuide; Handled: true),
    { 11 } (Button: gbShare; Handled: true),

    // all the rest, up to 31, are not handled
    { 12 } (Button: gbNorth; Handled: false),
    { 13 } (Button: gbNorth; Handled: false),
    { 14 } (Button: gbNorth; Handled: false),
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

function TWindowsControllerBackend.InternalButtonMap(
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

{ TWindowsControllerManagerBackend ------------------------------------------------- }

procedure TWindowsControllerManagerBackend.Initialize;
var
  WindowsControllerId, WindowsControllersCount: Integer;
  axis : Integer;
  NewController: TGameController;
  NewControllerBackend: TWindowsControllerBackend;

  state : TJOYINFOEX;
  JoyError: UInt32;

  JoyCapsResult: UInt32;
begin
  WindowsControllersCount := joyGetNumDevs();
  for WindowsControllerId := 0 to WindowsControllersCount - 1 do
  begin
    NewController := TGameController.Create;
    NewControllerBackend := TWindowsControllerBackend.Create(NewController);

    //NewController.Index := List.Count; // TODO

    { JOYSTICKID1 is 0, so NewControllerBackend.WindowsId is actually just
      an index of this loop, equal to WindowsControllerId.

      Note that it may be different from the index on List
      (NewController.Index),
      since we don't add all controllers to List.
      This can happen if you unplugged some controllers, they remain
      on Windows list with JOYERR_UNPLUGGED. }
    NewControllerBackend.WindowsId := JOYSTICKID1 + WindowsControllerId;

    JoyCapsResult := joyGetDevCapsW(NewControllerBackend.WindowsId, @NewControllerBackend.Caps, SizeOf( TJOYCAPSW ) );
    { Below we try to counter the WinAPI bug, when in case the application was run
      with no controllers connected and this is the first call to joyGetDevCapsW
      after the controller has been connected, the joyGetDevCapsW returns JOYERR_PARAMS = 165
      which is also returned for disconnected/unavailable controllers.
      Here we just call joyGetDevCapsW the second time to get the new value. }
    if JoyCapsResult <> 0 then
      JoyCapsResult := joyGetDevCapsW(NewControllerBackend.WindowsId, @NewControllerBackend.Caps, SizeOf( TJOYCAPSW ) );

    if JoyCapsResult = 0 then
    begin
      NewController.Name := NewControllerBackend.Caps.szPname;
      NewController.InternalAxesCount := NewControllerBackend.Caps.wNumAxes;
      NewController.InternalButtonsCount := NewControllerBackend.Caps.wNumButtons;

      NewControllerBackend.AxesMap[ 0 ] := jaX;
      NewControllerBackend.AxesMap[ 1 ] := jaY;
      axis := 2;
      if NewControllerBackend.Caps.wCaps and JOYCAPS_HASZ > 0 then
      begin
        Include(NewController.InternalCapabilities, jcZ);
        NewControllerBackend.AxesMap[ axis ] := jaZ;
        Inc( axis );
      end;
      if NewControllerBackend.Caps.wCaps and JOYCAPS_HASR > 0 then
      begin
        Include(NewController.InternalCapabilities, jcR);
        NewControllerBackend.AxesMap[ axis ] := jaR;
        Inc( axis );
      end;
      if NewControllerBackend.Caps.wCaps and JOYCAPS_HASU > 0 then
      begin
        Include(NewController.InternalCapabilities, jcU);
        NewControllerBackend.AxesMap[ axis ] := jaU;
        Inc( axis );
      end;
      if NewControllerBackend.Caps.wCaps and JOYCAPS_HASV > 0 then
      begin
        Include(NewController.InternalCapabilities, jcV);
        NewControllerBackend.AxesMap[ axis ] := jaV;
        Inc( axis );
      end;
      if NewControllerBackend.Caps.wCaps and JOYCAPS_HASPOV > 0 then
      begin
        Include(NewController.InternalCapabilities, jcPOV);
        Inc( NewController.InternalAxesCount, 2 );
      end;

      // workaround Windows reporting recently disconnected controllers as connected
      state.dwSize := SizeOf( TJOYINFOEX );
      state.dwFlags := JOY_RETURNALL or JOY_USEDEADZONE;
      if NewControllerBackend.Caps.wCaps and JOYCAPS_POVCTS > 0 then
        state.dwFlags := state.dwFlags or JOY_RETURNPOVCTS;
      JoyError := joyGetPosEx(NewControllerBackend.WindowsId, @state);
      //if no errors, then add this controller
      if JoyError = JOYERR_NOERROR then
      begin
        WriteLnLog('CastleGameControllers', 'Detected game controller: %s (Windows Id: %d); Axes: %d; Buttons: %d', [
          NewController.Name,
          NewControllerBackend.WindowsId,
          NewController.InternalAxesCount,
          NewController.InternalButtonsCount
        ]);

        List.Add(NewController);
      end else
      begin
        if JoyError = JOYERR_UNPLUGGED then
          WriteLnLog('CastleGameControllers', 'Detected game controller: %s (Windows Id: %d), but it will not be added because it seems to have been disconnected from the system recently (JOYERR_UNPLUGGED).', [
            NewController.Name,
            NewControllerBackend.WindowsId
          ])
        else
          WriteLnWarning('CastleGameControllers', 'Detected game controller: %s (Windows Id: %d), but it cannot be added due to an error %d.', [
            NewController.Name,
            NewControllerBackend.WindowsId,
            JoyError
          ]);
        FreeAndNil(NewController);
      end;
    end else
    begin
      // too spammy
      // WriteLnLog('CastleGameControllers', 'Detected game controller: %s (Windows Id: %d), but it will not be added because it seems to have been disconnected from the system recently (joyGetDevCapsW = 0).', [
      //   NewController.Name,
      //   NewControllerBackend.WindowsId
      // ]);
      FreeAndNil(NewController);
    end;
  end;
end;

procedure TWindowsControllerManagerBackend.Poll;
var
  ControllerIndex: Integer;
  AxisValueInt: UInt32;
  AxisValueFloat, PovSin, PovCos: Single;
  j: Integer;
  btn   : Integer;
  state : TJOYINFOEX;
  vMin  : UInt32;
  vMax  : UInt32;
  Controller: TGameController;
  ControllerBackend: TWindowsControllerBackend;
  JoyError: UInt32;
  ControllerHasBeenDisconnected: Boolean;
  Axis: TInternalGameControllerAxis;
begin
  ControllerHasBeenDisconnected := false;
  for ControllerIndex := 0 to List.Count - 1 do
  begin
    Controller := List[ControllerIndex];
    ControllerBackend := Controller.InternalBackend as TWindowsControllerBackend;

    state.dwSize := SizeOf( TJOYINFOEX );
    state.dwFlags := JOY_RETURNALL or JOY_USEDEADZONE;
    if ControllerBackend.Caps.wCaps and JOYCAPS_POVCTS > 0 then
      state.dwFlags := state.dwFlags or JOY_RETURNPOVCTS;

    JoyError := joyGetPosEx(ControllerBackend.WindowsId, @state );
    case JoyError of
      JOYERR_NOERROR:
        begin
          for j := 0 to Controller.InternalAxesCount - 1 do
          begin
            //stop if controller reported more axes than the backend can handle
            if j > High(ControllerBackend.AxesMap) then
              Break;

            Axis := ControllerBackend.AxesMap[ j ];

            case Axis of
              jaX: begin vMin := ControllerBackend.Caps.wXmin; vMax := ControllerBackend.Caps.wXmax; end;
              jaY: begin vMin := ControllerBackend.Caps.wYmin; vMax := ControllerBackend.Caps.wYmax; end;
              jaZ: begin vMin := ControllerBackend.Caps.wZmin; vMax := ControllerBackend.Caps.wZmax; end;
              jaR: begin vMin := ControllerBackend.Caps.wRmin; vMax := ControllerBackend.Caps.wRmax; end;
              jaU: begin vMin := ControllerBackend.Caps.wUmin; vMax := ControllerBackend.Caps.wUmax; end;
              jaV: begin vMin := ControllerBackend.Caps.wVmin; vMax := ControllerBackend.Caps.wVmax; end;
              else
                begin
                  WriteLnWarning('CastleGameControllers', 'Unknown axis %d for controller to determine vMin/vMax', [Ord(Axis)]);
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
                  WriteLnWarning('CastleGameControllers', 'Unknown axis %d for controller to determine value', [Ord(Axis)]);
                  Continue;
                end;
            end;

            AxisValueFloat := AxisValueInt / ( vMax - vMin ) * 2 - 1;
            Controller.InternalAxis[Axis] := AxisValueFloat;
          end;

          Controller.InternalAxis[jaPovX] := 0;
          Controller.InternalAxis[jaPovY] := 0;
          if (jcPOV in Controller.InternalCapabilities) and
             (state.dwPOV and $FFFF <> $FFFF) then
          begin
            SinCos( DegToRad(state.dwPOV and $FFFF / 100.0), PovSin, PovCos );
            Controller.InternalAxis[ jaPovX ] := PovSin;
            Controller.InternalAxis[ jaPovY ] := PovCos;
          end;

          for j := 0 to Controller.InternalButtonsCount - 1 do
          begin
            btn := state.wButtons and ( 1 shl j );
            Controller.InternalButtonDown[ j ] := btn <> 0;
          end;
        end;
      JOYERR_UNPLUGGED:
        begin
          WritelnLog('Controller %s (Windows Id: %d) was disconnected (JOYERR_UNPLUGGED)', [
            Controller.Name,
            ControllerBackend.WindowsId
          ]);
          ControllerHasBeenDisconnected := true;
        end;
      JOYERR_PARMS:
        begin
          WriteLnWarning('Controller %s (Windows Id: %d) parameters are no longer valid (JOYERR_PARMS), possibly disconnected?', [
            Controller.Name,
            ControllerBackend.WindowsId
          ]);
          //ControllerHasBeenDisconnected := true;
        end;
      else
        WriteLnWarning('Controller %s (Windows Id: %d) error %d', [
          Controller.Name,
          ControllerBackend.WindowsId,
          JoyError
        ]);
    end;
  end;
  { Do this *after* the loop. If any controller was disconnected,
    this will reinitialize List, so we don't want to be in the middle
    of iteration. }
  if ControllerHasBeenDisconnected then
    Controllers.InternalDisconnected;
end;

{$endif}

end.
