{
  Copyright 2015-2025 Tomasz Wojtyś, Michalis Kamburelis.

  Original based on zgl_joystick.pas by Andrey Kemka
  ( https://github.com/goldsmile/zengl/blob/master/src/zgl_joystick.pas ),
  though our arrangement at this point into CGE backends and public API
  resulted in very different code.

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

{ Backend using /dev/input/js* devices.
  See docs:
  - https://www.kernel.org/doc/html/latest/input/joydev/joystick.html
  - https://www.kernel.org/doc/html/latest/input/joydev/joystick-api.html
    Follow hint from there for low-level testing:
    """
    there is a set of utilities, such as `jstest`, `jscal`, and `evtest`,
    usually packaged as `joystick`, `input-utils`, `evtest`, and so on.
    """
}

{ Helpful types and constants ------------------------------------------------ }

type
  TLinuxJsEvent = record
    time: UInt32; //< event timestamp in milliseconds
    value: SmallInt; //< value
    EventType: Byte;  //< event type
    number: Byte; //< axis/button number
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

{ TLinuxControllerBackend -------------------------------------------------- }

type
  TLinuxControllerBackend = class(TInternalControllerBackend)
    DeviceInitialized: Boolean;
    Device: LongInt;

    { Axes mapping from JSIOCGAXMAP call. }
    AxesMap : array[0..ABS_MAX - 1] of Byte;

    { Axes count available in this controller.
      Warning: This doesn't imply the number of axes that are available in
      InternalAxis, it talks about internal axes (backend-specific) that
      are mapped to real axes in TInternalGameControllerAxis.
      So *do not* use this to iterate over InternalAxis.
      @exclude }
    AxesCount: Integer;

    { Buttons count reported to be supported. }
    ButtonsCount: Integer;

    FAxisLeft, FAxisRight: TVector2;
    FAxisLeftTrigger, FAxisRightTrigger: Single;

    function AxisLeft: TVector2; override;
    function AxisRight: TVector2; override;
    function AxisLeftTrigger: Single; override;
    function AxisRightTrigger: Single; override;
    destructor Destroy; override;

    procedure HandleEventButton(const Event: TLinuxJsEvent);
    procedure HandleEventAxis(const Event: TLinuxJsEvent);
  end;

function TLinuxControllerBackend.AxisLeft: TVector2;
begin
  Result := FAxisLeft;
end;

function TLinuxControllerBackend.AxisRight: TVector2;
begin
  Result := FAxisRight;
end;

function TLinuxControllerBackend.AxisLeftTrigger: Single;
begin
  Result := FAxisLeftTrigger;
end;

function TLinuxControllerBackend.AxisRightTrigger: Single;
begin
  Result := FAxisRightTrigger;
end;

const
  { Map TInternalGameControllerButton to TGameControllerButton.
    Specific to:
    - the XBox Controller, more specifically
      https://en.wikipedia.org/wiki/Xbox_Wireless_Controller
    - and to this controller implementation. }
  XBoxButtonsMap: array[0..14] of record
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
    { 14 } (Button: gbRightStickClick; Handled: true)

    // Note: Linux backend cannot detect "share" button at all, it seems
    // (present on XBox controller 3rd revision).
  );

destructor TLinuxControllerBackend.Destroy;
begin
  { Check for DeviceInitialized, since any Device >= 0 is valid in theory. }
  if DeviceInitialized then
    FpClose(Device);
  inherited;
end;

procedure TLinuxControllerBackend.HandleEventButton(const Event: TLinuxJsEvent);
var
  Button: TGameControllerButton;
begin
  if (Event.Number <= High(XBoxButtonsMap)) and
     XBoxButtonsMap[Event.Number].Handled then
  begin
    Button := XBoxButtonsMap[Event.Number].Button;
    case event.value of
      0: Controller.InternalPressedToReport[Button] := false;
      1: Controller.InternalPressedToReport[Button] := true;
      else
        begin
          WritelnWarning('Game Controllers', 'Controller "%s" received press/release for button index %d with unexpected value %d', [
            Controller.Name,
            Event.Number,
            Event.Value
          ]);
        end;
    end;
  end else
  begin
    WritelnWarning('Game Controllers', 'Controller "%s" received press/release for unhandled button index %d', [
      Controller.Name,
      Event.Number
    ]);
  end;
end;

procedure TLinuxControllerBackend.HandleEventAxis(const Event: TLinuxJsEvent);
var
  Axis: Byte;
  Value: Single;
begin
  // Some axis names follow
  // https://gist.github.com/emdeex/97b771b264bebbd1e18dd897404040be
  Axis := AxesMap[event.number];
  Value := event.value / 32767;

  case Axis of
    0: FAxisLeft.X := Value;
    { Y axis should be 1 when pointing up, -1 when pointing down.
      This is consistent with CGE 2D coordinate system
      (and standard math 2D coordinate system). }
    1: FAxisLeft.Y := -Value;

    2: FAxisRight.X := Value;
    { Y axis should be 1 when pointing up, -1 when pointing down.
      This is consistent with CGE 2D coordinate system
      (and standard math 2D coordinate system). }
    5: FAxisRight.Y := -Value;

    // gas
    9: FAxisRightTrigger := (Value + 1.0) / 2.0; // map from [-1,1] to [0,1]
    // brake
    10: FAxisLeftTrigger := (Value + 1.0) / 2.0; // map from [-1,1] to [0,1]

    16:
      begin
        Controller.InternalPressedToReport[gbDPadLeft] := false;
        Controller.InternalPressedToReport[gbDPadRight] := false;
        if SameValue(Value, 1) then
          Controller.InternalPressedToReport[gbDPadRight] := true
        else
        if SameValue(Value, -1) then
          Controller.InternalPressedToReport[gbDPadLeft] := true;
      end;
    17:
      begin
        Controller.InternalPressedToReport[gbDPadUp] := false;
        Controller.InternalPressedToReport[gbDPadDown] := false;
        if SameValue(Value, 1) then
          Controller.InternalPressedToReport[gbDPadDown] := true
        else
        if SameValue(Value, -1) then
          Controller.InternalPressedToReport[gbDPadUp] := true;
      end;

    else
      begin
        WritelnLog('Controller %s reports unhandled axis (%d mapped from %d) (value: %f)', [
          Controller.Name,
          Axis,
          event.number,
          Value
        ]);
      end;
  end;
end;

{ TLinuxControllerManagerBackend --------------------------------------------- }

procedure TLinuxControllerManagerBackend.Initialize;
var
  ControllerIndex, j : Integer;
  NewController: TGameController;
  NewControllerBackend: TLinuxControllerBackend;
  NewName: AnsiString;
begin
  for ControllerIndex := 0 to 15 do
  begin
    NewController := TGameController.Create;
    NewControllerBackend := TLinuxControllerBackend.Create(NewController);

    NewControllerBackend.Device := FpOpen('/dev/input/js' + IntToStr(ControllerIndex), O_RDONLY or O_NONBLOCK);
    if NewControllerBackend.Device < 0 then
      NewControllerBackend.Device := FpOpen('/dev/js' + IntToStr(ControllerIndex), O_RDONLY or O_NONBLOCK);

    if NewControllerBackend.Device > -1 then
    begin
      NewControllerBackend.DeviceInitialized := true;
      SetLength(NewName, 256);
      { TODO: why is cast to TIOCtlRequest needed (with FPC 3.3.1-r43920),
        we should probably fix the definition of JSIOCGNAME etc. instead. }
      FpIOCtl(NewControllerBackend.Device, TIOCtlRequest(JSIOCGNAME),    @NewName[1]);
      FpIOCtl(NewControllerBackend.Device, TIOCtlRequest(JSIOCGAXMAP),   @NewControllerBackend.AxesMap[0]);
      FpIOCtl(NewControllerBackend.Device, TIOCtlRequest(JSIOCGAXES),    @NewControllerBackend.AxesCount);
      FpIOCtl(NewControllerBackend.Device, TIOCtlRequest(JSIOCGBUTTONS), @NewControllerBackend.ButtonsCount);

      for j := 1 to 255 do
        if NewName[J] = #0 then
        begin
          SetLength(NewName, j - 1);
          break;
        end;
      NewControllerBackend.Name := NewName;

      { Checking if controller is a real one,
        because laptops with accelerometer can be detected as a joystick.
        TODO: Above comment should be made more precise -- on which laptops it is?
        And can we somehow differentiate better between laptop accelerometer
        and something else, that is maybe not a gamepad but still
        a useful controller?
        Actually, laptop accelerometer could also be useful for some games? }
      if (NewControllerBackend.AxesCount >= 2) and
         (NewControllerBackend.ButtonsCount > 0) then
      begin
        WritelnLog('CastleGameControllers', 'Detected game controller: %s (ID: %d); Axes: %d; Buttons: %d', [
          NewController.Name,
          ControllerIndex,
          NewControllerBackend.AxesCount,
          NewControllerBackend.ButtonsCount
        ]);
        NewControllerBackend.Index := List.Count;
        List.Add(NewController);
      end else
        FreeAndNil(NewController);
    end else
      FreeAndNil(NewController);
  end;
end;

procedure TLinuxControllerManagerBackend.Poll;
var
  i: Integer;
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
          JS_EVENT_AXIS: ControllerBackend.HandleEventAxis(Event);
          JS_EVENT_BUTTON: ControllerBackend.HandleEventButton(Event);
          else ;
        end;
        BytesRead := FpRead(ControllerBackend.Device, event, 8);
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
