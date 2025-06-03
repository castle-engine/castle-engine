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

{ Game controllers (joystick, gamepad) handling. }
unit CastleGameControllers;

{$I castleconf.inc}

interface

uses Generics.Collections, Classes,
  CastleVectors, CastleUtils, CastleKeysMouse;

type
  { Internal capability of a controller. }
  TInternalGameControllerCapability = (
    jcZ,
    jcR,
    jcU,
    jcV,
    jcPOV
  );
  TInternalGameControllerCapabilities = set of TInternalGameControllerCapability;

  { Internal 1D controller axis. }
  TInternalGameControllerAxis = (
    jaX,
    jaY,
    jaZ,
    jaR,
    jaU,
    jaV,
    jaPovX,
    jaPovY
  );

  { Internal controller button as integer index. }
  TInternalGameControllerButton = 0..31;

  { Properties of a given game controller (joystick, gamepad).
    Get instance of this by @link(TGameControllers.Items Controllers[Index]).

    Do not construct instances of this yourself, TGameControllers creates
    this automatically when necessary.

    All the contents of this class are read-only for applications.
    Only the controller backends (that implement TGameController logic
    using underlying system-specific APIs) can change it. }
  TGameController = class
  public
    { Game controller name. Not necessarily unique, so be sure to display also
      game controller index to the user in UI. }
    Name: String;

    { Implementation-specific information.
      @exclude }
    InternalBackendInfo: TObject;

    { Buttons count reported to be supported.
      @exclude }
    InternalButtonsCount: Integer;

    { Capabilities reported to be supported.
      @exclude }
    InternalCapabilities: TInternalGameControllerCapabilities;

    { Axes count available in this controller.
      Warning: This doesn't imply the number of axes that are available in
      InternalAxis, it talks about internal axes (backend-specific) that
      are mapped to real axes in TInternalGameControllerAxis.
      So *do not* use this to iterate over InternalAxis.
      @exclude }
    InternalAxesCount: Integer;

    { Current state of the axis.
      This is internal, use instead nicer
      @link(TGameController.AxisLeft) and @link(TGameController.AxisLeft).
      @exclude }
    InternalAxis: array[TInternalGameControllerAxis] of Single;

    { Current state of the buttons.
      @exclude }
    InternalButtonDown: array[TInternalGameControllerButton] of Boolean;

    { Last state of the buttons reported to TCastleUserInterface.Press/Release.
      @exclude }
    InternalButtonDownReported: array[TInternalGameControllerButton] of Boolean;

    { Left analog stick position.
      Both coordinates are in range -1..1,
      where (0, 0) is the center of the stick.

      Note: It is not guaranteed that they fit within the circle of radius 1
      (they usually don't, they go a bit outside the circle).
      But it is also not guaranteed that they can reach the edge of the square,
      e.g. position (-1,-1) may not be reachable, usually it is not reachable.

      So: Your application should not assume that user can make a stick
      position beyond the circle of radius 1, but it should accept such positions. }
    function AxisLeft: TVector2;

    { Right analog stick position.
      See @link(TGameController.AxisLeft) for details of possible values. }
    function AxisRight: TVector2;

    { Left/right triggers are expressed as 1D axis (on XBox controller).

      @unorderedlist(
        @item(1.0 means "only left trigger fully pressed",)
        @item(0.0 means "no trigger pressed, or both triggers pressed,
          or more generally: both triggers pressed with equal strength",)
        @item(-1.0 means "only right trigger fully pressed".)
      )

      Values in between are determined based on the pressure
      of the left/right trigger. }
    function AxisTrigger: Single;

    { Nice caption (label) of a given button.

      For buttons that have different names depending on the game controller,
      like "face buttons" (A, B, X, Y, square triangle circle cross),
      this caption depends on the game controller type.

      TODO: The current implementation assumes
      @unorderedlist(
        @item(XBox controller on all platforms except Nintendo Switch,)
        @item(or Nintendo Switch controllers on @url(https://castle-engine.io/nintendo_switch
          Nintendo Switch).)
      ) }
    function ButtonCaption(const Button: TGameControllerButton): String;

    { Intended meaning of the given button.
      This may depend on the game controller type.

      TODO: The current implementation assumes
      @unorderedlist(
        @item(XBox controller on all platforms except Nintendo Switch,)
        @item(or Nintendo Switch controllers on @url(https://castle-engine.io/nintendo_switch
          Nintendo Switch).)
      ) }
    function ButtonMeaning(const Button: TGameControllerButton): TGameControllerButtonMeaning;

    { Map TInternalGameControllerButton (button as internal, device-specific integer)
      to nice TGameControllerButton.

      TODO: The current implementation assumes
      @unorderedlist(
        @item(XBox controller on all platforms except Nintendo Switch,)
        @item(or Nintendo Switch controllers on @url(https://castle-engine.io/nintendo_switch
          Nintendo Switch).)
      ) }
    function InternalButtonMap(const Button: TInternalGameControllerButton): TGameControllerButton;

    destructor Destroy; override;
  end;

  TGameControllerList = {$ifdef FPC}specialize{$endif} TObjectList<TGameController>;

  TGameControllers = class;

  { Internal class to provide different implementations of game controllers.
    @exclude }
  TGameControllersBackend = class abstract
    { Detect and add controllers to given list. }
    procedure Initialize(const List: TGameControllerList); virtual; abstract;

    { Update state of controllers on given list. }
    procedure Poll(const List: TGameControllerList;
      const EventContainer: TGameControllers); virtual; abstract;
  end;

  { Manage game controllers (joysticks, gamepads).
    See examples/game_controllers for usage examples. }
  TGameControllers = class
  private
    Backend: TGameControllersBackend;
    FList: TGameControllerList;
    FInitialized: Boolean;
    FOnChange: TNotifyEvent;
    function GetItems(const Index: Integer): TGameController;
    { Get (creating if necessary) explicit backend.
      Always returns TExplicitGameControllerBackend, but cannot be declared as such. }
    function ExplicitBackend: TGameControllersBackend;
  protected
    { See OnChange. }
    procedure DoChange;
  public
    constructor Create;
    destructor Destroy; override;

    { Check state of every connected controller and run event procedures.
      This is internal, called automatically by CastleUIControls unit,
      user code does not need to call this.
      @exclude }
    procedure InternalPoll;

    { Number of connected controllers. }
    function Count: Integer;

    { List of connected controllers.
      Use this to access individual controllers. }
    property Items[const Index: Integer]: TGameController read GetItems; default;

    { Detect connected controllers now and keep updating this list
      when controllers are connected / disconnected.
      On some platforms, you need to call this to search for connected controllers,
      otherwise you will always have zero controllers.
      Calling this again is allowed and harmless. }
    procedure Initialize;

    { Was @link(Initialize) called. }
    property Initialized: Boolean read FInitialized;

    { Used by CASTLE_WINDOW_LIBRARY when
      an external API notifies us about the controllers state.
      @exclude }
    procedure InternalSetCount(const ControllerCount: Integer);
    { @exclude }
    procedure InternalSetAxisLeft(const ControllerIndex: Integer; const Axis: TVector2);
    { @exclude }
    procedure InternalSetAxisRight(const ControllerIndex: Integer; const Axis: TVector2);

    { Used by CastleWindow to notify us that some devices have been
      connected / disconnected, so we should run @link(Initialize).
      @exclude }
    procedure InternalConnected;
    { @exclude }
    procedure InternalDisconnected;

    { Called after TGameController instances on the list changed
      (some have been added, some destroyed).

      This can be called at any moment, as controllers may be
      connected / disconnected at any moment. }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{ Global game controller object.
  Singleton, automatically created when being accessed. }
function Controllers: TGameControllers;

implementation

uses SysUtils, Math,
  CastleLog,
  {$ifdef FPC} {$ifdef LINUX} CastleInternalGameControllersLinux, {$endif} {$endif}
  {$ifdef MSWINDOWS} CastleInternalGameControllersWindows, {$endif}
  CastleInternalGameControllersExplicit;

{ TGameController ------------------------------------------------------------------ }

destructor TGameController.Destroy;
begin
  FreeAndNil(InternalBackendInfo);
  inherited;
end;

function TGameController.AxisLeft: TVector2;
begin
  Result := Vector2(
    InternalAxis[jaX],
    { Y axis should be 1 when pointing up, -1 when pointing down.
      This is consistent with CGE 2D coordinate system
      (and standard math 2D coordinate system). }
    -InternalAxis[jaY]
  );
end;

function TGameController.AxisRight: TVector2;
begin
  Result := Vector2(
    InternalAxis[jaU],
    { Y axis should be 1 when pointing up, -1 when pointing down.
      This is consistent with CGE 2D coordinate system
      (and standard math 2D coordinate system). }
    -InternalAxis[jaR]
  );
end;

function TGameController.AxisTrigger: Single;
begin
  Result := InternalAxis[jaZ];
end;

const
  { Map TInternalGameControllerButton to TGameControllerButton.
    Specific to the XBox Controller, more specifically
    https://en.wikipedia.org/wiki/Xbox_Wireless_Controller }
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

  { Map TGameControllerButton to additional information, like Caption.
    Specific to the XBox Controller, more specifically
    https://en.wikipedia.org/wiki/Xbox_Wireless_Controller }
  XBoxMap: array[TGameControllerButton] of record
    Caption: String;
    Meaning: TGameControllerButtonMeaning;
    { For now, always @true, because we declare all and only buttons for
      XBox Controller, and we treat all controlles as XBox Controller.
      But it will not be such always. }
    Handled: Boolean;
  end = (
    { gbNorth }           (Caption: 'Y'                ; Meaning: gmNone   ; Handled: true),
    { gbEast }            (Caption: 'B'                ; Meaning: gmCancel ; Handled: true),
    { gbSouth }           (Caption: 'A'                ; Meaning: gmConfirm; Handled: true),
    { gbWest }            (Caption: 'X'                ; Meaning: gmNone   ; Handled: true),
    { gbLeftTrigger }     (Caption: 'Left Trigger'     ; Meaning: gmNone   ; Handled: true),
    { gbRightTrigger }    (Caption: 'Right Trigger'    ; Meaning: gmNone   ; Handled: true),
    { gpLeftBumper }      (Caption: 'Left Bumper'      ; Meaning: gmNone   ; Handled: true),
    { gpRightBumper }     (Caption: 'Right Bumper'     ; Meaning: gmNone   ; Handled: true),
    { gbLeftStickClick }  (Caption: 'Left Stick Click' ; Meaning: gmNone   ; Handled: true),
    { gbRightStickClick } (Caption: 'Right Stick Click'; Meaning: gmNone   ; Handled: true),
    { gbDPadUp }          (Caption: 'D-Pad Up'         ; Meaning: gmNone   ; Handled: true),
    { gbDPadRight }       (Caption: 'D-Pad Right'      ; Meaning: gmNone   ; Handled: true),
    { gbDPadDown }        (Caption: 'D-Pad Down'       ; Meaning: gmNone   ; Handled: true),
    { gbDPadLeft }        (Caption: 'D-Pad Left'       ; Meaning: gmNone   ; Handled: true),
    { gpView }            (Caption: 'View'             ; Meaning: gmNone   ; Handled: true),
    { gpMenu }            (Caption: 'Menu'             ; Meaning: gmNone   ; Handled: true),
    { gpGuide }           (Caption: 'Guide'            ; Meaning: gmNone   ; Handled: true),
    { gpShare }           (Caption: 'Share'            ; Meaning: gmNone   ; Handled: true)
  );

function TGameController.ButtonCaption(const Button: TGameControllerButton): String;
begin
  if not XBoxMap[Button].Handled then
  begin
    WritelnWarning('TGameController.ButtonCaption: Button %d is not handled by the game controller "%s".', [
      Ord(Button),
      // TODO: Write here also controller index
      Name
    ]);
  end;
  Result := XBoxMap[Button].Caption;
end;

function TGameController.ButtonMeaning(const Button: TGameControllerButton): TGameControllerButtonMeaning;
begin
  if not XBoxMap[Button].Handled then
  begin
    WritelnWarning('TGameController.ButtonMeaning: Button %d is not handled by the game controller "%s".', [
      Ord(Button),
      // TODO: Write here also controller index
      Name
    ]);
  end;
  Result := XBoxMap[Button].Meaning;
end;

function TGameController.InternalButtonMap(const Button: TInternalGameControllerButton): TGameControllerButton;
begin
  if not XBoxInternalMap[Button].Handled then
  begin
    WritelnWarning('TGameController.InternalButtonMap: Button %d is not handled by the game controller "%s".', [
      Ord(Button),
      // TODO: Write here also controller index
      Name
    ]);
  end;
  Result := XBoxInternalMap[Button].Button;
end;

{ TGameControllers ----------------------------------------------------------------- }

constructor TGameControllers.Create;
begin
  inherited;
  FList := TGameControllerList.Create(true);
  {$if defined(MSWINDOWS)}
  Backend := TWindowsControllersBackend.Create;
  {$elseif defined(LINUX) and defined(FPC)}
  // TODO: Delphi on Linux doesn't support game controllers now
  Backend := TLinuxControllersBackend.Create;
  {$else}
  // This way Backend is non-nil always
  Backend := TExplicitGameControllerBackend.Create;
  {$endif}
end;

destructor TGameControllers.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(Backend);
  inherited;
end;

procedure TGameControllers.Initialize;
begin
  FInitialized := true;

  { In case of TExplicitGameControllerBackend,
    do not clear the list and call Backend.Initialize.
    Instead leave existing controllers (set by TExplicitGameControllerBackend.SetCount).
    That's because Backend.Initialize doesn't have a way to get new
    controllers information (in case of TExplicitGameControllerBackend,
    it is CGE that is informed by external code about controller existence). }
  if Backend is TExplicitGameControllerBackend then
    Exit;

  FList.Clear;
  Backend.Initialize(FList);
  DoChange;
end;

procedure TGameControllers.InternalPoll;
begin
  if FInitialized then
    Backend.Poll(FList, Self);
end;

function TGameControllers.ExplicitBackend: TGameControllersBackend;
begin
  Assert(Backend <> nil);
  if not (Backend is TExplicitGameControllerBackend) then
  begin
    FreeAndNil(Backend);
    Backend := TExplicitGameControllerBackend.Create;
    { Although TExplicitGameControllerBackend.Initialize doesn't do anything for now,
      but call it, to make sure Initialized = true. }
    Initialize;
  end;
  Result := Backend;
end;

procedure TGameControllers.InternalSetCount(const ControllerCount: Integer);
begin
  (ExplicitBackend as TExplicitGameControllerBackend).SetCount(FList, ControllerCount);
  DoChange;
end;

procedure TGameControllers.InternalSetAxisLeft(const ControllerIndex: Integer; const Axis: TVector2);
begin
  (ExplicitBackend as TExplicitGameControllerBackend).SetAxisLeft(FList, ControllerIndex, Axis);
end;

procedure TGameControllers.InternalSetAxisRight(const ControllerIndex: Integer; const Axis: TVector2);
begin
  (ExplicitBackend as TExplicitGameControllerBackend).SetAxisRight(FList, ControllerIndex, Axis);
end;

procedure TGameControllers.InternalConnected;
begin
  if not Initialized then
    Exit;
  Initialize; // call Initialize again, to update the list of controllers
end;

procedure TGameControllers.InternalDisconnected;
begin
  if not Initialized then
    Exit;
  Initialize; // call Initialize again, to update the list of controllers
end;

function TGameControllers.GetItems(const Index: Integer): TGameController;
begin
  Result := FList[Index];
end;

function TGameControllers.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TGameControllers.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

{ global --------------------------------------------------------------------- }

var
  FControllers: TGameControllers;

function Controllers: TGameControllers;
begin
  if FControllers = nil then
    FControllers := TGameControllers.Create;
  Result := FControllers;
end;

initialization
finalization
  FreeAndNil(FControllers);
end.
