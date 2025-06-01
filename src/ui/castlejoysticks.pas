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

{ Cross-platform joystick and gamepad handling. }
unit CastleJoysticks;

{$I castleconf.inc}

interface

uses Generics.Collections, Classes,
  CastleVectors, CastleUtils;

type
  TInternalGamepadCapability = (
    { Gamepad has a Z axis. }
    jcZ,
    { Gamepad has an R axis. }
    jcR,
    { Gamepad has a U axis. }
    jcU,
    { Gamepad has a V axis. }
    jcV,
    { Gamepad has a POV (point of view) hat. }
    jcPOV
  );
  TInternalGamepadCapabilities = set of TInternalGamepadCapability;

  { 1D gamepad axis. }
  TInternalGamepadAxis = (
    { Gamepad X axis. }
    jaX,
    { Gamepad Y axis. }
    jaY,
    { Gamepad Z axis. }
    jaZ,
    { Gamepad R axis. }
    jaR,
    { Gamepad U axis. }
    jaU,
    { Gamepad V axis. }
    jaV,
    { Gamepad POV X axis. }
    jaPOVX,
    { Gamepad POV Y axis. }
    jaPOVY
  );

  { Gamepad button as integer index. }
  TInternalGamepadButton = 0..31;

  { Properties of a given joystick,
    use by accessing @link(TJoysticks.Items Joysticks[Index]).

    Do not construct instances of this yourself, TJoysticks creates
    this automatically when necessary.

    All the contents of this class are read-only for applications.
    Only the gamepad backends (that implement TJoystick logic
    using underlying system-specific APIs) can change it. }
  TJoystick = class
  private
  public
    { Implementation-specific information. }
    InternalBackendInfo: TObject;

    { Gamepad name. Not necessarily unique, so be sure to display also
      gamepad index to the user in UI. }
    Name: String;

    { Gamepad information. }
    InternalButtonsCount: Integer;
    InternalCapabilities: TInternalGamepadCapabilities;

    { Axes count available in this gamepad.
      Warning: This doesn't imply the number of axes that are available in
      InternalAxis, it talks about internal axes (backend-specific) that
      are mapped to real axes in TInternalGamepadAxis.
      So *do not* use this to iterate over InternalAxis. }
    InternalAxesCount: Integer;

    { Current gamepad state.
      This is internal, use instead nicer
      @link(TJoystick.LeftAxis) and @link(TJoystick.RightAxis). }
    InternalAxis           : array[TInternalGamepadAxis] of Single;
    InternalButtonUp       : array[TInternalGamepadButton] of Boolean;
    InternalButtonDown     : array[TInternalGamepadButton] of Boolean;
    InternalButtonPress    : array[TInternalGamepadButton] of Boolean;
    InternalButtonCanPress : array[TInternalGamepadButton] of Boolean;

    { Left analog stick position. }
    function LeftAxis: TVector2;

    { Right analog stick position. }
    function RightAxis: TVector2;

    destructor Destroy; override;
  end;

  TJoystickList = {$ifdef FPC}specialize{$endif} TObjectList<TJoystick>;

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
  TOnJoyAxisMove = procedure(const Joy: TJoystick;
    const Axis: TInternalGamepadAxis; const Value: Single) of object;

  { Joystick button action event. Used on button press/up/down. }
  TOnJoyButtonEvent = procedure(const Joy: TJoystick;
    const Button: TInternalGamepadButton) of object;

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

    procedure ClearState;

    property OnAxisMove: TOnJoyAxisMove read FOnAxisMove write FOnAxisMove;
    property OnButtonDown: TOnJoyButtonEvent read FOnButtonDown write FOnButtonDown;
    property OnButtonUp: TOnJoyButtonEvent read FOnButtonUp write FOnButtonUp;
    property OnButtonPress: TOnJoyButtonEvent read FOnButtonPress write FOnButtonPress;
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
    procedure InternalSetJoystickAxis(const JoystickIndex: Integer; const Axis: TVector2); deprecated 'use InternalSetJoystickLeftAxis';
    { @exclude }
    procedure InternalSetJoystickLeftAxis(const JoystickIndex: Integer; const Axis: TVector2);
    { @exclude }
    procedure InternalSetJoystickRightAxis(const JoystickIndex: Integer; const Axis: TVector2);

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

    { Called when previously initalized gamepad has been disconnected. }
    property OnDisconnect: TSimpleNotifyEvent read FOnDisconnect write FOnDisconnect;

    { Called when gamepad has been connected to the system. }
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

function TJoystick.LeftAxis: TVector2;
begin
  Result := Vector2(
    InternalAxis[jaX],
    { Y axis should be 1 when pointing up, -1 when pointing down.
      This is consistent with CGE 2D coordinate system
      (and standard math 2D coordinate system). }
    -InternalAxis[jaY]
  );
end;

function TJoystick.RightAxis: TVector2;
begin
  Result := Vector2(
    InternalAxis[jaU],
    { Y axis should be 1 when pointing up, -1 when pointing down.
      This is consistent with CGE 2D coordinate system
      (and standard math 2D coordinate system). }
    -InternalAxis[jaR]
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

procedure TJoysticks.InternalPoll;
begin
  if FInitialized then
    Backend.Poll(FList, Self);
end;

procedure TJoysticks.ClearState;
var
  I: Integer;
  J: TInternalGamepadButton;
begin
  for i := 0 to Count - 1 do
    for j := 0 to FList[I].InternalButtonsCount - 1 do
    begin
      FList[I].InternalButtonUp[J] := false;
      FList[I].InternalButtonDown[J] := false;
      FList[I].InternalButtonPress[J] := false;
      FList[I].InternalButtonCanPress[J] := true;
    end;
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
  InternalSetJoystickLeftAxis(JoystickIndex, Axis);
end;

procedure TJoysticks.InternalSetJoystickLeftAxis(const JoystickIndex: Integer; const Axis: TVector2);
begin
  TExplicitJoystickBackend(ExplicitBackend).SetJoystickLeftAxis(FList, JoystickIndex, Axis);
end;

procedure TJoysticks.InternalSetJoystickRightAxis(const JoystickIndex: Integer; const Axis: TVector2);
begin
  TExplicitJoystickBackend(ExplicitBackend).SetJoystickRightAxis(FList, JoystickIndex, Axis);
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
