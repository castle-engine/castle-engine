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
  { Internal 1D controller axis.
    @exclude }
  TInternalGameControllerAxis = (
    jaX,
    jaY,
    jaZ,
    jaR,
    jaU,
    jaV,
    jaPovX,
    jaPovY,
    // Only by Linux backend; Windows backend returns both triggers as one axis in jaZ.
    jaGas,
    jaBrake
  );

  { Internal controller button as integer index.
    @exclude }
  TInternalGameControllerButton = 0..31;

  TGameController = class;

  { Internal backend for a single controller.
    @exclude }
  TInternalControllerBackend = class
  strict private
    FController: TGameController;
  public
    property Controller: TGameController read FController;

    { Create instance.
      Sets @link(Controller) read-only property.
      Sets @link(TGameController.InternalBackend) to this instance. }
    constructor Create(const AController: TGameController);

    // Override these methods to provide controller-specific logic.
    function AxisLeft: TVector2; virtual; abstract;
    function AxisRight: TVector2; virtual; abstract;
    function AxisLeftTrigger: Single; virtual; abstract;
    function AxisRightTrigger: Single; virtual; abstract;
    function InternalButtonMap(
      const Button: TInternalGameControllerButton): TGameControllerButton; virtual; abstract;
  end;

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

    { Which buttons are now pressed. }
    Pressed: array [TGameControllerButton] of Boolean;

    { Implementation-specific information.
      @exclude }
    InternalBackend: TInternalControllerBackend;

    { Buttons count reported to be supported.
      @exclude }
    InternalButtonsCount: Integer;

    { Current state of the axis.
      This is internal, use instead nicer
      @link(TGameController.AxisLeft) and @link(TGameController.AxisLeft).
      @exclude }
    InternalAxis: array [TInternalGameControllerAxis] of Single;

    { Current state of the internal buttons.
      @exclude }
    InternalButtonDown: array [TInternalGameControllerButton] of Boolean;

    { Last state of the buttons reported to TCastleUserInterface.Press/Release.
      @exclude }
    InternalButtonDownReported: array [TInternalGameControllerButton] of Boolean;

    { Last state of the DPad buttons detected by backend.
      @exclude }
    InternalDPadDown: array [TGameControlledDPadButton] of Boolean;

    { Last state of the DPad buttons reported to TCastleUserInterface.Press/Release.
      This is used by UpdateGameControllers, as DPad buttons
      are internally axis, but only make sense in practice to be treated
      as buttons.
      @exclude }
    InternalDPadDownReported: array [TGameControlledDPadButton] of Boolean;

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

    { Left trigger axis, in range 0..1. }
    function AxisLeftTrigger: Single;

    { Right trigger axis, in range 0..1. }
    function AxisRightTrigger: Single;

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
  TInternalControllerManagerBackend = class abstract
  strict private
    FControllers: TGameControllers;
    FList: TGameControllerList;
  public
    property Controllers: TGameControllers read FControllers;
    property List: TGameControllerList read FList;

    { Create instance.
      Sets @link(Controllers), @link(List) read-only property.
      Sets @link(TGameControllers.Backend) to this instance.
      @exclude }
    constructor Create(const AControllers: TGameControllers);

    { Detect and add controllers to @link(List). }
    procedure Initialize; virtual; abstract;

    { Update state of controllers on @link(List). }
    procedure Poll; virtual; abstract;
  end;

  { Manage game controllers (joysticks, gamepads).
    See examples/game_controllers for usage examples. }
  TGameControllers = class
  private
    Backend: TInternalControllerManagerBackend;
    FList: TGameControllerList;
    FInitialized: Boolean;
    FOnChange: TNotifyEvent;
    function GetItems(const Index: Integer): TGameController;
  public
    constructor Create;
    destructor Destroy; override;

    { Calls OnChange. }
    procedure DoChange;

    { Check state of every connected controller.
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

    { Get (creating if necessary) explicit backend.
      Always returns TExplicitControllerManagerBackend, but cannot be
      declared as such.
      Used by CASTLE_WINDOW_LIBRARY when
      an external API notifies us about the controllers state. }
    function InternalExplicitBackend: TInternalControllerManagerBackend;

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

{ TInternalControllerBackend ------------------------------------------------- }

constructor TInternalControllerBackend.Create(const AController: TGameController);
begin
  inherited Create;
  Assert(AController <> nil);
  FController := AController;
  FController.InternalBackend := Self;
end;

{ TGameController ------------------------------------------------------------ }

destructor TGameController.Destroy;
begin
  FreeAndNil(InternalBackend);
  inherited;
end;

function TGameController.AxisLeft: TVector2;
begin
  Result := InternalBackend.AxisLeft;
end;

function TGameController.AxisRight: TVector2;
begin
  Result := InternalBackend.AxisRight;
end;

function TGameController.AxisLeftTrigger: Single;
begin
  Result := InternalBackend.AxisLeftTrigger;
end;

function TGameController.AxisRightTrigger: Single;
begin
  Result := InternalBackend.AxisRightTrigger;
end;

function TGameController.InternalButtonMap(const Button: TInternalGameControllerButton): TGameControllerButton;
begin
  Result := InternalBackend.InternalButtonMap(Button);
end;

const
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

{ TInternalControllerManagerBackend ------------------------------------------ }

constructor TInternalControllerManagerBackend.Create(
  const AControllers: TGameControllers);
begin
  inherited Create;
  Assert(AControllers <> nil);

  FControllers := AControllers;
  FControllers.Backend := Self;

  FList := FControllers.FList;
  Assert(List <> nil, 'TGameControllers.List must be initialized before backend');
end;

{ TGameControllers ----------------------------------------------------------------- }

constructor TGameControllers.Create;
begin
  inherited;
  FList := TGameControllerList.Create(true);

  { No need to assign Backend:= below, constructors will set our Backend field. }
  {$if defined(MSWINDOWS)}
  TWindowsControllerManagerBackend.Create(Self);
  {$elseif defined(LINUX) and defined(FPC)}
  // TODO: Delphi on Linux doesn't support game controllers now
  TLinuxControllerManagerBackend.Create(Self);
  {$else}
  // This way Backend is non-nil always
  TExplicitControllerManagerBackend.Create(Self);
  {$endif}
  Assert(Backend <> nil);
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

  { In case of TExplicitControllerManagerBackend,
    do not clear the list and call Backend.Initialize.
    Instead leave existing controllers (set by TExplicitControllerManagerBackend.SetCount).
    That's because Backend.Initialize doesn't have a way to get new
    controllers information (in case of TExplicitControllerManagerBackend,
    it is CGE that is informed by external code about controller existence). }
  if Backend is TExplicitControllerManagerBackend then
    Exit;

  FList.Clear;
  Backend.Initialize;
  DoChange;
end;

procedure TGameControllers.InternalPoll;
begin
  if FInitialized then
    Backend.Poll;
end;

function TGameControllers.InternalExplicitBackend: TInternalControllerManagerBackend;
begin
  Assert(Backend <> nil);
  if not (Backend is TExplicitControllerManagerBackend) then
  begin
    FreeAndNil(Backend);
    { No need to assign Backend:= below, TExplicitControllerManagerBackend
      constructor will set our Backend field. }
    TExplicitControllerManagerBackend.Create(Self);
    Assert(Backend is TExplicitControllerManagerBackend);
    { Although TExplicitControllerManagerBackend.Initialize doesn't do anything for now,
      but call it, to make sure Initialized = true. }
    Initialize;
  end;
  Result := Backend;
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
