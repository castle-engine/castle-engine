{
  Copyright 2015-2017 Tomasz Wojtyś.
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

//todo: only partially works on Windows (no support of new features); need to be ported to xinput library
//todo: on Linux: drop legacy interface /dev/js#; enable /dev/event#

{ Receiving input from joysticks and gamepads. }
unit CastleJoysticks;

{$I castleconf.inc}

interface

{$IFDEF LINUX}
uses
  BaseUnix;
{$ENDIF}

{$IFDEF LINUX}
type
  js_event = record
    time   : LongWord; // event timestamp in milliseconds
    value  : SmallInt; // value
    _type  : Byte;     // event type
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
{$ENDIF}
{$IFDEF WINDOWS}
  type
    PJOYCAPSW = ^TJOYCAPSW;
    TJOYCAPSW = packed record
      wMid: Word;
      wPid: Word;
      szPname: array[ 0..31 ] of WideChar;
      wXmin: LongWord;
      wXmax: LongWord;
      wYmin: LongWord;
      wYmax: LongWord;
      wZmin: LongWord;
      wZmax: LongWord;
      wNumButtons: LongWord;
      wPeriodMin: LongWord;
      wPeriodMax: LongWord;
      wRmin: LongWord;
      wRmax: LongWord;
      wUmin: LongWord;
      wUmax: LongWord;
      wVmin: LongWord;
      wVmax: LongWord;
      wCaps: LongWord;
      wMaxAxes: LongWord;
      wNumAxes: LongWord;
      wMaxButtons: LongWord;
      szRegKey: array[ 0..31 ] of WideChar;
      szOEMVxD: array[ 0..259 ] of WideChar;
  end;

  type
    PJOYINFOEX = ^TJOYINFOEX;
    TJOYINFOEX = packed record
      dwSize: LongWord;
      dwFlags: LongWord;
      wXpos: LongWord;
      wYpos: LongWord;
      wZpos: LongWord;
      dwRpos: LongWord;
      dwUpos: LongWord;
      dwVpos: LongWord;
      wButtons: LongWord;
      dwButtonNumber: LongWord;
      dwPOV: LongWord;
      dwReserved1: LongWord;
      dwReserved2: LongWord;
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

  WINMMLIB = 'winmm.dll';

  function joyGetNumDevs : LongWord; stdcall; external WINMMLIB name 'joyGetNumDevs';
  function joyGetDevCapsW( uJoyID : LongWord; lpCaps : PJOYCAPSW; uSize : LongWord ) : LongWord; stdcall; external WINMMLIB name 'joyGetDevCapsW';
  function joyGetPosEx( uJoyID : LongWord; lpInfo : PJOYINFOEX ) : LongWord; stdcall; external WINMMLIB name 'joyGetPosEx';
{$ENDIF}

type
  PJoyInfo = ^TJoyInfo;
  TJoyInfo = record
    Name   : UTF8String;
    Count  : record
      Axes    : Integer;
      Buttons : Integer;
             end;
    Caps   : LongWord;
  end;

type
  PJoyState = ^TJoyState;
  TJoyState = record
    Axis        : array[ 0..7 ] of Single;
    BtnUp       : array[ 0..31 ] of Boolean;
    BtnDown     : array[ 0..31 ] of Boolean;
    BtnPress    : array[ 0..31 ] of Boolean;
    BtnCanPress : array[ 0..31 ] of Boolean;
  end;

type
  PJoy = ^TJoy;
  TJoy = record
    {$IFDEF LINUX}
    device  : LongInt;
    axesMap : array[ 0..ABS_MAX - 1 ] of Byte;
    {$ENDIF}
    {$IFDEF WINDOWS}
    caps    : TJOYCAPSW;
    axesMap : array[ 0..5 ] of Byte;
    {$ENDIF}
    Info    : TJoyInfo;
    State   : TJoyState;
  end;

const
  JOY_HAS_Z   = $000001;
  JOY_HAS_R   = $000002;
  JOY_HAS_U   = $000004;
  JOY_HAS_V   = $000008;
  JOY_HAS_POV = $000010;

  JOY_AXIS_X = 0;
  JOY_AXIS_Y = 1;
  JOY_AXIS_Z = 2;
  JOY_AXIS_R = 3;
  JOY_AXIS_U = 4;
  JOY_AXIS_V = 5;
  JOY_POVX   = 6;
  JOY_POVY   = 7;

{$IFDEF LINUX}
  JS_AXIS : array[ 0..17 ] of Byte = ( JOY_AXIS_X, JOY_AXIS_Y, JOY_AXIS_Z, JOY_AXIS_U, JOY_AXIS_V, JOY_AXIS_R, JOY_AXIS_Z, JOY_AXIS_R, 0, 0, 0, 0, 0, 0, 0, 0, JOY_POVX, JOY_POVY );
{$ENDIF}
{$IFDEF WINDOWS}
  JS_AXIS : array[ 0..5 ] of LongWord = ( 17 {X}, 19 {Y}, 21 {Z}, 26 {R}, 28 {U}, 30 {V} );
{$ENDIF}

type

  { Joystick axis move event. }
  TOnJoyAxisMove = procedure(const Joy: PJoy; const Axis: Byte; const Value: Single) of object;
  { Joystick button action event. Used on button press/up/down. }
  TOnJoyButtonEvent = procedure(const Joy: PJoy; const Button: Byte) of object;

  { TJoysticks is a class for joysticks and gamepads management }
  TJoysticks = class
  private
    FOnAxisMove: TOnJoyAxisMove;
    FOnButtonDown: TOnJoyButtonEvent;
    FOnButtonUp: TOnJoyButtonEvent;
    FOnButtonPress: TOnJoyButtonEvent;
    FjoyArray: array[ 0..15 ] of TJoy;
    FjoyCount: Integer;
    function  Init: Byte;
  public
    { Constructor search for connected devices. If new device will be connected
      after Create it won't be automatically discovered. In such case
      TJoysticks have to be destroied and created again. }
    constructor Create;
    destructor Destroy; override;
    { Check state of every connected joystick and run event procedures.  }
    procedure Poll;
    function  GetInfo( JoyID : Byte ) : PJoyInfo;
    function  AxisPos( JoyID, Axis : Byte ): Single;
    function  Down( JoyID, Button : Byte ): Boolean;
    function  Up( JoyID, Button : Byte ): Boolean;
    function  Press( JoyID, Button : Byte ): Boolean;
    procedure ClearState;
    function GetJoy(const JoyID: Byte): PJoy;
  published
    property OnAxisMove: TOnJoyAxisMove read FOnAxisMove write FOnAxisMove;
    property OnButtonDown: TOnJoyButtonEvent read FOnButtonDown write FOnButtonDown;
    property OnButtonUp: TOnJoyButtonEvent read FOnButtonUp write FOnButtonUp;
    property OnButtonPress: TOnJoyButtonEvent read FOnButtonPress write FOnButtonPress;
    property JoyCount: Integer read FjoyCount;
  end;

{ Initialize Joysticks global variable. }
procedure EnableJoysticks;

var
  { Global joystick manager object (singelton). To initialize Joysticks instance
    run EnableJoysticks procedure }
  Joysticks: TJoysticks;

implementation

uses
  SysUtils, CastleLog, Math;

procedure EnableJoysticks;
begin
  Joysticks := TJoysticks.Create;
end;

{ TJoysticks }

function TJoysticks.Init: Byte;
{$IFDEF LINUX}
var
  i, j : Integer;
{$ENDIF}
{$IFDEF WINDOWS}
var
  i, j : Integer;
  axis : Integer;
  caps : PLongWord;
{$ENDIF}
begin
  FjoyCount := 0;

  {$IFDEF LINUX}
  for i := 0 to 15 do
  begin
    FjoyArray[ FjoyCount ].device := FpOpen( '/dev/input/js' + IntToStr( i ), O_RDONLY or O_NONBLOCK );
    if FjoyArray[ FjoyCount ].device < 0 then
      FjoyArray[ FjoyCount ].device := FpOpen( '/dev/js' + IntToStr( i ), O_RDONLY or O_NONBLOCK );

    if FjoyArray[ FjoyCount ].device > -1 then
    begin
      SetLength( FjoyArray[ FjoyCount ].Info.Name, 256 );
      FpIOCtl( FjoyArray[ FjoyCount ].device, JSIOCGNAME,    @FjoyArray[ FjoyCount ].Info.Name[ 1 ] );
      FpIOCtl( FjoyArray[ FjoyCount ].device, JSIOCGAXMAP,   @FjoyArray[ FjoyCount ].axesMap[ 0 ] );
      FpIOCtl( FjoyArray[ FjoyCount ].device, JSIOCGAXES,    @FjoyArray[ FjoyCount ].Info.Count.Axes );
      FpIOCtl( FjoyArray[ FjoyCount ].device, JSIOCGBUTTONS, @FjoyArray[ FjoyCount ].Info.Count.Buttons );

      for j := 0 to FjoyArray[ FjoyCount ].Info.Count.Axes - 1 do
        with FjoyArray[ FjoyCount ].Info do
          case FjoyArray[ FjoyCount ].axesMap[ j ] of
            2, 6:   Caps := Caps or JOY_HAS_Z;
            5, 7:   Caps := Caps or JOY_HAS_R;
            3:      Caps := Caps or JOY_HAS_U;
            4:      Caps := Caps or JOY_HAS_V;
            16, 17: Caps := Caps or JOY_HAS_POV;
          end;

      for j := 1 to 255 do
        if FjoyArray[ FjoyCount ].Info.Name[ j ] = #0 Then
          begin
            SetLength( FjoyArray[ FjoyCount ].Info.Name, j - 1 );
            break;
          end;

      // Checking if joystick is a real one, because laptops with accelerometer can be detected as a joystick :)
      if ( FjoyArray[ FjoyCount ].Info.Count.Axes >= 2 ) and ( FjoyArray[ FjoyCount ].Info.Count.Buttons > 0 ) then
      begin
        if Log then
          WritelnLog('CastleJoysticks Init', 'Find joy: %S (ID: %D); Axes: %D; Buttons: %D', [FjoyArray[ joyCount ].Info.Name, FjoyCount, FjoyArray[ FjoyCount ].Info.Count.Axes, FjoyArray[ FjoyCount ].Info.Count.Buttons]);

          Inc( FjoyCount );
        end;
      end else
        break;
  end;
  {$ENDIF}
  {$IFDEF WINDOWS}
  j := joyGetNumDevs();
  for i := 0 to j - 1 do
    if joyGetDevCapsW( i, @FjoyArray[ i ].caps, SizeOf( TJOYCAPSW ) ) = 0 then
      begin
        FjoyArray[ i ].Info.Name          := FjoyArray[ i ].caps.szPname;
        FjoyArray[ i ].Info.Count.Axes    := FjoyArray[ i ].caps.wNumAxes;
        FjoyArray[ i ].Info.Count.Buttons := FjoyArray[ i ].caps.wNumButtons;

        caps  := @FjoyArray[ i ].Info.Caps;
        FjoyArray[ i ].axesMap[ 0 ] := JOY_AXIS_X;
        FjoyArray[ i ].axesMap[ 1 ] := JOY_AXIS_Y;
        axis := 2;
        if FjoyArray[ i ].caps.wCaps and JOYCAPS_HASZ > 0 then
          begin
            caps^ := caps^ or JOY_HAS_Z;
            FjoyArray[ i ].axesMap[ axis ] := JOY_AXIS_Z;
            Inc( axis );
          end;
        if FjoyArray[ i ].caps.wCaps and JOYCAPS_HASR > 0 then
          begin
            caps^ := caps^ or JOY_HAS_R;
            FjoyArray[ i ].axesMap[ axis ] := JOY_AXIS_R;
            Inc( axis );
          end;
        if FjoyArray[ i ].caps.wCaps and JOYCAPS_HASU > 0 then
          begin
            caps^ := caps^ or JOY_HAS_U;
            FjoyArray[ i ].axesMap[ axis ] := JOY_AXIS_U;
            Inc( axis );
          end;
        if FjoyArray[ i ].caps.wCaps and JOYCAPS_HASV > 0 then
          begin
            caps^ := caps^ or JOY_HAS_V;
            FjoyArray[ i ].axesMap[ axis ] := JOY_AXIS_V;
            //Inc( axis );
          end;
        if FjoyArray[ i ].caps.wCaps and JOYCAPS_HASPOV > 0 then
          begin
            caps^ := caps^ or JOY_HAS_POV;
            Inc( FjoyArray[ i ].Info.Count.Axes, 2 );
          end;

        if Log then
          WritelnLog('CastleJoysticks Init', 'Find joy: %S (ID: %D); Axes: %D; Buttons: %D',
                     [FjoyArray[ i ].Info.Name, i, FjoyArray[ i ].Info.Count.Axes, FjoyArray[ i ].Info.Count.Buttons]);

        Inc( FjoyCount );
      end else
        break;
  {$ENDIF}

  Result := FjoyCount;
  ClearState;
  if Result = 0 then
    WritelnLog('CastleJoysticks Init', 'Couldn''t find joysticks' );
end;

constructor TJoysticks.Create;
begin
  inherited;
  Init;
end;

destructor TJoysticks.Destroy;
{$IFDEF LINUX}
var
  i : Integer;
{$ENDIF}
begin
{$IFDEF LINUX}
  for i := 0 to FjoyCount - 1 do
    FpClose( FjoyArray[ i ].device );
{$ENDIF}
  inherited;
end;

procedure TJoysticks.Poll;
{$IFDEF LINUX}
var
  i : Integer;
  _value: Single;
  axis: Byte;
  event : js_event;
{$ENDIF}
{$IFDEF WINDOWS}
var
  i : Integer;
  _value: Single;
  j, a  : Integer;
  btn   : Integer;
  state : TJOYINFOEX;
  pcaps : PLongWord;
  value : PLongWord;
  vMin  : LongWord;
  vMax  : LongWord;
{$ENDIF}
begin
if FjoyCount = 0 then Exit;

{$IFDEF LINUX}
for i := 0 to FjoyCount - 1 do
  begin
    while FpRead( FjoyArray[ i ].device, event, 8 ) = 8 do
      case event._type of
        JS_EVENT_AXIS:
          begin
            axis := JS_AXIS[ FjoyArray[ i ].axesMap[ event.number ] ];
            _value := Round( ( event.value / 32767 ) * 1000 ) / 1000;
            FjoyArray[ i ].State.Axis[ axis ] := _value;
            if Assigned(FOnAxisMove) then FOnAxisMove(@FjoyArray[ i ], axis, _value);
          end;
        JS_EVENT_BUTTON:
          case event.value of
            0:
              begin
                if FjoyArray[ i ].State.BtnDown[ event.number ] then
                begin
                  FjoyArray[ i ].State.BtnUp[ event.number ] := True;
                  FjoyArray[ i ].State.BtnPress   [ event.number ] := False;
                  if Assigned(FOnButtonUp) then FOnButtonUp(@FjoyArray[ i ], event.number);
                  FjoyArray[ i ].State.BtnCanPress[ event.number ] := True;
                end;

                FjoyArray[ i ].State.BtnDown[ event.number ] := False;
              end;
            1:
              begin
                FjoyArray[ i ].State.BtnDown[ event.number ] := True;
                if Assigned(FOnButtonDown) then FOnButtonDown(@FjoyArray[ i ], event.number);
                FjoyArray[ i ].State.BtnUp  [ event.number ] := False;
                if FjoyArray[ i ].State.BtnCanPress[ event.number ] then
                  begin
                    FjoyArray[ i ].State.BtnPress   [ event.number ] := True;
                    if Assigned(FOnButtonPress) then FOnButtonPress(@FjoyArray[ i ], event.number);
                    FjoyArray[ i ].State.BtnCanPress[ event.number ] := False;
                  end;
              end;
          end;
      end;
  end;
{$ENDIF}
{$IFDEF WINDOWS}
state.dwSize := SizeOf( TJOYINFOEX );
for i := 0 to FjoyCount - 1 do
  begin
    state.dwFlags := JOY_RETURNALL or JOY_USEDEADZONE;
    if FjoyArray[ i ].caps.wCaps and JOYCAPS_POVCTS > 0 then
      state.dwFlags := state.dwFlags or JOY_RETURNPOVCTS;

    if joyGetPosEx( i, @state ) = 0 then
      begin
        for j := 0 to FjoyArray[ i ].Info.Count.Axes - 1 do
          begin
            // Say "no" to if's, and do everything trciky :)
            a     := FjoyArray[ i ].axesMap[ j ];
            pcaps := @FjoyArray[ i ].caps;
            Inc( pcaps, JS_AXIS[ a ] );
            vMin  := pcaps^;
            Inc( pcaps );
            vMax  := pcaps^;
            value := @state;
            Inc( value, 2 + a );

            _value := Round( ( value^ / ( vMax - vMin ) * 2 - 1 ) * 1000 ) / 1000;
            if FjoyArray[ i ].State.Axis[ a ] <> _value then
              if Assigned(FOnAxisMove) then FOnAxisMove(@FjoyArray[ i ], j, _value);
            FjoyArray[ i ].State.Axis[ a ] := _value;
          end;

        FillChar( FjoyArray[ i ].State.Axis[ JOY_POVX ], 8, 0 );
        if ( FjoyArray[ i ].Info.Caps and JOY_HAS_POV > 0 ) and ( state.dwPOV and $FFFF <> $FFFF ) then
          begin
            FjoyArray[ i ].State.Axis[ JOY_POVX ] := Round( Sin( state.dwPOV and $FFFF div 100 ) );
            FjoyArray[ i ].State.Axis[ JOY_POVY ] := -Round( Cos( state.dwPOV and $FFFF div 100 ) );
          end;

        for j := 0 to FjoyArray[ i ].Info.Count.Buttons - 1 do
          begin
            btn := state.wButtons and ( 1 shl j );
            if ( FjoyArray[ i ].State.BtnDown[ j ] ) and ( btn = 0 ) then
            begin
              FjoyArray[ i ].State.BtnPress[ j ] := False;
              if Assigned(FOnButtonUp) then FOnButtonUp(@FjoyArray[ i ], j);
              FjoyArray[ i ].State.BtnCanPress[ j ] := True;
            end;

            if ( FjoyArray[ i ].State.BtnCanPress[ j ] ) and ( not FjoyArray[ i ].State.BtnDown[ j ] ) and ( btn <> 0 ) then
              begin
                FjoyArray[ i ].State.BtnPress   [ j ] := True;
                if Assigned(FOnButtonPress) then FOnButtonPress(@FjoyArray[ i ], j);
                FjoyArray[ i ].State.BtnCanPress[ j ] := False;
              end;
            FjoyArray[ i ].State.BtnDown[ j ] := btn <> 0;
            FjoyArray[ i ].State.BtnUp[ j ] := btn = 0;
            if Assigned(FOnButtonDown) and (btn <> 0) then FOnButtonDown(@FjoyArray[ i ], j);
          end;
      end;
  end;
{$ENDIF}
end;

function TJoysticks.GetInfo(JoyID: Byte): PJoyInfo;
begin
  Result := nil;
  if JoyID >= FjoyCount then Exit;

  Result := @FjoyArray[ JoyID ].Info;
end;

function TJoysticks.AxisPos(JoyID, Axis: Byte): Single;
begin
  Result := 0;
  if ( JoyID >= FjoyCount ) or ( Axis > JOY_POVY ) then Exit;

  Result := FjoyArray[ JoyID ].State.Axis[ Axis ];
end;

function TJoysticks.Down(JoyID, Button: Byte): Boolean;
begin
  Result := False;
  if ( JoyID >= FjoyCount ) or ( Button >= FjoyArray[ JoyID ].Info.Count.Buttons ) then Exit;

  Result := FjoyArray[ JoyID ].State.BtnDown[ Button ];
end;

function TJoysticks.Up(JoyID, Button: Byte): Boolean;
begin
  Result := False;
  if ( JoyID >= FjoyCount ) or ( Button >= FjoyArray[ JoyID ].Info.Count.Buttons ) then Exit;

  Result := FjoyArray[ JoyID ].State.BtnUp[ Button ];
end;

function TJoysticks.Press(JoyID, Button: Byte): Boolean;
begin
  Result := False;
  if ( JoyID >= FjoyCount ) or ( Button >= FjoyArray[ JoyID ].Info.Count.Buttons ) then Exit;

  Result := FjoyArray[ JoyID ].State.BtnPress[ Button ];
end;

procedure TJoysticks.ClearState;
var
  i, j  : Integer;
  state : PJoyState;
begin
  for i := 0 to FjoyCount - 1 do
    for j := 0 to FjoyArray[ i ].Info.Count.Buttons - 1 do
      begin
        state := @FjoyArray[ i ].State;
        state^.BtnUp[ j ]       := False;
        state^.BtnDown[ j ]     := False;
        state^.BtnPress[ j ]    := False;
        state^.BtnCanPress[ j ] := True;
      end;
end;

function TJoysticks.GetJoy(const JoyID: Byte): PJoy;
begin
  Result := nil;
  if JoyID >= FjoyCount then Exit;
  Result := @FjoyArray[JoyID];
end;

end.
