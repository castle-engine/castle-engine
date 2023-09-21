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

{ Cross-platform joystick and gamepad handling. }
unit CastleJoysticks;

{$I castleconf.inc}

interface

uses Generics.Collections, Classes,
  CastleVectors, CastleUtils;

type
  PJoyInfo = ^TJoyInfo;
  { Joystick information.
    TODO: Deprecate at some point, in favor of simpler joystick API like @link(TJoystick.Axis). }
  TJoyInfo = record
    Name   : String;
    Count  : record
      Axes    : Integer;
      Buttons : Integer;
             end;
    Caps   : UInt32;
  end;

  PJoyState = ^TJoyState;
  { Joystick state.
    TODO: Deprecate at some point, in favor of simpler joystick API like @link(TJoystick.Axis). }
  TJoyState = record
    Axis        : array[ 0..7 ] of Single;
    BtnUp       : array[ 0..31 ] of Boolean;
    BtnDown     : array[ 0..31 ] of Boolean;
    BtnPress    : array[ 0..31 ] of Boolean;
    BtnCanPress : array[ 0..31 ] of Boolean;
  end;

  { Properties of a given joystick, use by accessing @link(TJoysticks.Items Joysticks[Index]).
    Do not construct instances of this yourself, TJoysticks creates
    this automatically when necessary. }
  TJoystick = class
  public
    { Implementation-specific information. }
    InternalBackendInfo: TObject;
    { Information.
      TODO: Deprecate at some point, in favor of simpler joystick API like @link(TJoystick.Axis). }
    Info    : TJoyInfo;
    { State.
      TODO: Deprecate at some point, in favor of simpler joystick API like @link(TJoystick.Axis). }
    State   : TJoyState;

    function Axis: TVector2;
    destructor Destroy; override;
  end;

  PJoy = TJoystick deprecated 'use TJoystick';

  TJoystickList = {$ifdef FPC}specialize{$endif} TObjectList<TJoystick>;

const
  { TODO: Deprecate these constants at some point, in favor of simpler joystick API like @link(TJoystick.Axis). }

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

type
  TJoysticks = class;

  { Internal class to provide different implementations of joystick events reading.
    @exclude }
  TJoysticksBackend = class abstract
    { Detect and add joysticks to given list. }
    procedure Initialize(const List: TJoystickList); virtual; abstract;
    { Update state of joysticks on given list. }
    procedure Poll(const List: TJoystickList;
      const EventContainer: TJoysticks); virtual; abstract;
  end;

  { Joystick axis move event. }
  TOnJoyAxisMove = procedure(const Joy: TJoystick; const Axis: Byte; const Value: Single) of object;
  { Joystick button action event. Used on button press/up/down. }
  TOnJoyButtonEvent = procedure(const Joy: TJoystick; const Button: Byte) of object;

  { TJoysticks is a class for joysticks and gamepads management }
  TJoysticks = class
  private
    FOnAxisMove: TOnJoyAxisMove;
    FOnButtonDown: TOnJoyButtonEvent;
    FOnButtonUp: TOnJoyButtonEvent;
    FOnButtonPress: TOnJoyButtonEvent;
    Backend: TJoysticksBackend;
    FList: TJoystickList;
    FInitialized: Boolean;
    FOnChange: TNotifyEvent;
    FOnDisconnect: TSimpleNotifyEvent;
    FOnConnect: TSimpleNotifyEvent;
    function GetItems(const Index: Integer): TJoystick;
    { Get (creating if necessary) joystick's explicit backend.
      Always returns TExplicitJoystickBackend, but cannot be declared as such. }
    function ExplicitBackend: TJoysticksBackend;
  protected
    { See OnChange. }
    procedure DoChange;
  public
    constructor Create;
    destructor Destroy; override;
    { Check state of every connected joystick and run event procedures.
      This is internal, called automatically by CastleUIControls unit,
      user code does not need to call this.
      @exclude }
    procedure InternalPoll;
    { @exclude }
    procedure Poll; deprecated 'do not call this, it is not necessary';

    function  GetInfo( JoyID : Byte ) : PJoyInfo;
    function  AxisPos( JoyID, Axis : Byte ): Single;
    function  Down( JoyID, Button : Byte ): Boolean;
    function  Up( JoyID, Button : Byte ): Boolean;
    function  Press( JoyID, Button : Byte ): Boolean;
    procedure ClearState;
    function GetJoy(const JoyID: Integer): TJoystick; deprecated 'use Joysticks[xxx] instead of Joysticks.GetJoy(xxx)';

    property OnAxisMove: TOnJoyAxisMove read FOnAxisMove write FOnAxisMove;
    property OnButtonDown: TOnJoyButtonEvent read FOnButtonDown write FOnButtonDown;
    property OnButtonUp: TOnJoyButtonEvent read FOnButtonUp write FOnButtonUp;
    property OnButtonPress: TOnJoyButtonEvent read FOnButtonPress write FOnButtonPress;
    function JoyCount: Integer; deprecated 'use Count';
    function Count: Integer;

    property Items[const Index: Integer]: TJoystick read GetItems; default;

    { Detect connected joysticks.
      On some platforms, you need to call this to search for connected joysticks,
      otherwise you will always have zero joysticks.
      Calling this again is allowed, it searches for connected joysticks again. }
    procedure Initialize;

    property Initialized: Boolean read FInitialized;

    { Used by CASTLE_WINDOW_LIBRARY when
      an external API notifies us about the joysticks state.
      @exclude }
    procedure InternalSetJoystickCount(const JoystickCount: Integer);
    { @exclude }
    procedure InternalSetJoystickAxis(const JoystickIndex: Integer; const Axis: TVector2);

    { Used by CastleWindow when
      an external API notifies us about connecting/disconnecting devices to the system.
      @exclude }
    procedure InternalConnected;
    { @exclude }
    procedure InternalDisconnected;

    { Called after TJoystick instances on the list change (some are added, destroyed).
      In case of some backends, this is only called at the end of @link(Initialize),
      but it may be called in other cases (e.g. "explicit" joystick backend,
      used by Nintendo Switch, may call this at any moment). }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    { Called in case a previously initalized joystick has been disconnected. }
    property OnDisconnect: TSimpleNotifyEvent read FOnDisconnect write FOnDisconnect;
    { Called in case a joystick has been connected to the system. }
    property OnConnect: TSimpleNotifyEvent read FOnConnect write FOnConnect;
  end;

{ Detect connected joysticks. }
procedure EnableJoysticks; deprecated 'use Joysticks.Initialize';

{ Global joystick manager object. Singleton, automatically created when being accessed.
  Remember to call @link(TJoysticks.Initialize), this is necessary on some platforms
  to detect joysticks. }
function Joysticks: TJoysticks;

implementation

uses SysUtils, Math,
  CastleLog,
  {$ifdef FPC} {$ifdef LINUX} CastleInternalJoysticksLinux, {$endif} {$endif}
  {$ifdef MSWINDOWS} CastleInternalJoysticksWindows, {$endif}
  CastleInternalJoysticksExplicit;

{ TJoystick ------------------------------------------------------------------ }

destructor TJoystick.Destroy;
begin
  FreeAndNil(InternalBackendInfo);
  inherited;
end;

function TJoystick.Axis: TVector2;
begin
  Result := Vector2(
    State.Axis[JOY_AXIS_X],
    State.Axis[JOY_AXIS_Y]
  );
end;

{ TJoysticks ----------------------------------------------------------------- }

constructor TJoysticks.Create;
begin
  inherited;
  FList := TJoystickList.Create(true);
  {$if defined(MSWINDOWS)}
  Backend := TWindowsJoysticksBackend.Create;
  {$elseif defined(LINUX) and defined(FPC)}
  // TODO: Delphi on Linux doesn't support joysticks now
  Backend := TLinuxJoysticksBackend.Create;
  {$else}
  // This way Backend is non-nil always
  Backend := TExplicitJoystickBackend.Create;
  {$endif}
end;

destructor TJoysticks.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(Backend);
  inherited;
end;

procedure TJoysticks.Initialize;
begin
  FInitialized := true;

  { In case of TExplicitJoystickBackend,
    do not clear the list and call Backend.Initialize.
    Instead leave existing joysticks (set by TExplicitJoystickBackend.SetJoystickCount).
    That's because Backend.Initialize doesn't have a way to get new joystick information
    (in case of TExplicitJoystickBackend, it is CGE that is informed by external code
    about joystick existence). }
  if Backend is TExplicitJoystickBackend then
    Exit;

  FList.Clear;
  Backend.Initialize(FList);
  ClearState;
  DoChange;
end;

procedure TJoysticks.Poll;
begin
  InternalPoll;
end;

procedure TJoysticks.InternalPoll;
begin
  if FInitialized then
    Backend.Poll(FList, Self);
end;

function TJoysticks.GetInfo(JoyID: Byte): PJoyInfo;
begin
  Result := nil;
  if JoyID >= Count then Exit;

  Result := @(FList[JoyID].Info);
end;

function TJoysticks.AxisPos(JoyID, Axis: Byte): Single;
begin
  Result := 0;
  if ( JoyID >= Count ) or ( Axis > JOY_POVY ) then Exit;

  Result := FList[JoyID].State.Axis[ Axis ];
end;

function TJoysticks.Down(JoyID, Button: Byte): Boolean;
begin
  Result := False;
  if ( JoyID >= Count ) or ( Button >= FList[JoyID].Info.Count.Buttons ) then Exit;

  Result := FList[JoyID].State.BtnDown[ Button ];
end;

function TJoysticks.Up(JoyID, Button: Byte): Boolean;
begin
  Result := False;
  if ( JoyID >= Count ) or ( Button >= FList[JoyID].Info.Count.Buttons ) then Exit;

  Result := FList[JoyID].State.BtnUp[ Button ];
end;

function TJoysticks.Press(JoyID, Button: Byte): Boolean;
begin
  Result := False;
  if ( JoyID >= Count ) or ( Button >= FList[JoyID].Info.Count.Buttons ) then Exit;

  Result := FList[JoyID].State.BtnPress[ Button ];
end;

procedure TJoysticks.ClearState;
var
  i, j  : Integer;
  state : PJoyState;
begin
  for i := 0 to Count - 1 do
    for j := 0 to FList[I].Info.Count.Buttons - 1 do
      begin
        state := @FList[I].State;
        state^.BtnUp[ j ]       := False;
        state^.BtnDown[ j ]     := False;
        state^.BtnPress[ j ]    := False;
        state^.BtnCanPress[ j ] := True;
      end;
end;

function TJoysticks.GetJoy(const JoyID: Integer): TJoystick;
begin
  Result := nil;
  if JoyID >= Count then Exit;
  Result := FList[JoyID];
end;

function TJoysticks.ExplicitBackend: TJoysticksBackend;
begin
  Assert(Backend <> nil);
  if not (Backend is TExplicitJoystickBackend) then
  begin
    FreeAndNil(Backend);
    Backend := TExplicitJoystickBackend.Create;
    { Although TExplicitJoystickBackend.Initialize doesn't do anything for now,
      but call it, to make sure Initialized = true. }
    Initialize;
  end;
  Result := Backend;
end;

procedure TJoysticks.InternalSetJoystickCount(const JoystickCount: Integer);
begin
  TExplicitJoystickBackend(ExplicitBackend).SetJoystickCount(FList, JoystickCount);
  DoChange;
end;

procedure TJoysticks.InternalSetJoystickAxis(const JoystickIndex: Integer; const Axis: TVector2);
begin
  TExplicitJoystickBackend(ExplicitBackend).SetJoystickAxis(FList, JoystickIndex, Axis);
end;

procedure TJoysticks.InternalConnected;
begin
  if Assigned(OnConnect) then
    OnConnect;
end;

procedure TJoysticks.InternalDisconnected;
begin
  if Assigned(OnDisconnect) then
    OnDisconnect;
end;

function TJoysticks.GetItems(const Index: Integer): TJoystick;
begin
  Result := FList[Index];
end;

function TJoysticks.JoyCount: Integer;
begin
  Result := FList.Count;
end;

function TJoysticks.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TJoysticks.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

{ global --------------------------------------------------------------------- }

procedure EnableJoysticks;
begin
  Joysticks.Initialize;
end;

var
  FJoysticks: TJoysticks;

function Joysticks: TJoysticks;
begin
  if FJoysticks = nil then
    FJoysticks := TJoysticks.Create;
  Result := FJoysticks;
end;

{$ifndef FPC}initialization{$endif}

finalization
  FreeAndNil(FJoysticks);
end.
