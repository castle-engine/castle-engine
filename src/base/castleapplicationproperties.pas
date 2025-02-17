{
  Copyright 2014-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Events and properties of the Castle Game Engine application
  (TCastleApplicationProperties). }
unit CastleApplicationProperties;

{$I castleconf.inc}

interface

uses SysUtils, Generics.Collections, Contnrs, Classes,
  CastleClassUtils;

type
  TGLContextEvent = procedure;

  TGLContextEventList = class({$ifdef FPC}specialize{$endif} TList<TGLContextEvent>)
  public
    { Call all items, first to last. }
    procedure ExecuteForward;
    { Call all items, last to first. }
    procedure ExecuteBackward;
  end;

  TWarningEvent = procedure (const Category, Message: String) of object;
  TLogEvent = procedure (const Message: String) of object;

  TWarningEventList = class({$ifdef FPC}specialize{$endif} TList<TWarningEvent>)
  public
    procedure ExecuteAll(const Category, Message: String);
  end;

  TLogEventList = class({$ifdef FPC}specialize{$endif} TList<TLogEvent>)
  public
    procedure ExecuteAll(const Message: String);
  end;

  { Events and properties of each Castle Game Engine application,
    always accessed through the @link(ApplicationProperties) singleton.

    The members of this class work regardless of how is the rendering context
    initialized, in particular regardless of whether
    you use CastleWindow or CastleControl.
    For various other application properties and methods use

    @unorderedList(
      @item(TCastleApplication class, using the @link(CastleWindow.Application)
        singleton (if you use CastleWindow).)
      @item(@code(TApplication) class, using the @code(Application) singleton,
        in Lazarus LCL or Delphi VCL or FMX (in case you use CastleControl).)
    ) }
  TCastleApplicationProperties = class
  private
    FIsGLContextOpen, FFileAccessSafe: boolean;
    FOnGLContextEarlyOpen, FOnGLContextOpen, FOnGLContextClose: TGLContextEventList;
    FOnUpdate, FOnInitializeJavaActivity,
      FOnGLContextOpenObject, FOnGLContextCloseObject,
      FOnPause, FOnResume: TNotifyEventList;
    FOnWarning: TWarningEventList;
    FOnLog: TLogEventList;
    FOnInitializeDebug: TProcedureList;
    FVersion: String;
    FTouchDevice: boolean;
    FLimitFPS: Single;
    FShowUserInterfaceToQuit: Boolean;
    FCaption: String;
    // Maybe we will expose them as public read-only properties in the future.
    FInitializedDebug: Boolean;
    FInitializedRelease: Boolean;
    FPendingToFree: TComponentList;
    function GetApplicationName: String;
    procedure SetApplicationName(const Value: String);
    procedure DoPendingFree;
  public
    const
      DefaultLimitFPS = 120.0;

      { Some platforms do not support Application.ProcessMessage, which means you
        cannot just write a function like MessageYesNo that waits until user clicks
        something.
        You *have* to implement modal boxes then using views,
        e.g. using CastleDialogViews or your own TCastleView descendants. }
      PlatformAllowsModalRoutines = {$if defined(CASTLE_IOS) or defined(CASTLE_NINTENDO_SWITCH)} false {$else} true {$endif};

    constructor Create;
    destructor Destroy; override;

    { Application short name.
      Used e.g. by @link(InitializeLog) to name the log file.

      When compiled with FPC, this returns and sets the same thing
      as standard SysUtils.ApplicationName.
      When setting this, we automatically set SysUtils.OnGetApplicationName. }
    property ApplicationName: String read GetApplicationName write SetApplicationName;

    { @abstract(Pretty application name, to show to user e.g. as a window caption.) }
    property Caption: String read FCaption write FCaption;

    { Version of this application.
      It may be used e.g. by @link(InitializeLog) and
      @link(TCastleApplication.ParseStandardParameters). }
    property Version: String read FVersion write FVersion;

    { Initialized to @true on devices with a touch screen (Android, iOS, Nintendo Switch).

      On such devices:

      @unorderedList(
        @item(We cannot track @link(TCastleContainer.MousePosition)
          when nothing is pressed (@link(TCastleContainer.MousePressed) is [])
          on a touch device.)

        @item(The only "mouse button" you will ever see pressed
          on a touch device is buttonLeft.)

        @item(On the other hand, touch devices support multitouch, exposed by
          @link(TCastleContainer.Touches) and @link(TCastleContainer.TouchesCount).
          On touch devices, @link(TCastleContainer.TouchesCount) can range
          from 0 to a few (modern touch devices support up to 5 simultaneous
          touches).

          On non-touch devices, @link(TCastleContainer.TouchesCount) is always 1.)
      )

      See @url(https://castle-engine.io/touch_input documentation
      about touch input).

      As a debugging feature, you can set this to @true
      to simulate touch devices on a desktop.
      The idea is that when an application shows a different input behavior
      on touch devices, it should always condition it depending on
      this boolean property. So an application may do this:

      @longCode(#
      // show the button only on mobile
      ButtonExit.Exists := ApplicationProperties.TouchDevice;
      #)

      To test on desktop whether everything behaves OK on mobile,
      you can just earlier call this:

      @longCode(#
      if DebugTouchDeviceOnDesktop then
        ApplicationProperties.TouchDevice := true;
      #)

      If you use our standard initialization using @link(TCastleWindow.ParseParameters),
      you can also pass command-line option @code(--pretend-touch-device) to do this.
      If you execute the game from our editor, you can also use the menu item
      @italic("Run -> Run Parameters -> Pretend Touch Device") to do this.
      Underneath, all these methods do is set this property to @true. }
    property TouchDevice: boolean read FTouchDevice write FTouchDevice;

    { Is it common, on current platform, to show the "Quit" button in your application.
      E.g. it is normal to show "Quit" on PC (Windows, Linux etc.).
      But on mobile devices and consoles (like Nintendo Switch) you should not
      show "Quit", it is expected that user knows how to use OS-specific
      mechanism to just switch to a different application.

      Just like the @link(TouchDevice), you can change this at runtime
      for debug purposes (to e.g. easily test mobile UI on PC). }
    property ShowUserInterfaceToQuit: Boolean read FShowUserInterfaceToQuit write FShowUserInterfaceToQuit;

    { Limit the number of (real) frames per second, to not hog the CPU.
      Set to zero to not limit.

      The mechanism is implemented by occasionally sleeping
      (when we see that we render way faster than we need to).
      So it's a global thing, not just a property of TCastleWindow or TCastleControl.

      In some cases, this also means the "desired number of FPS".
      This happens when we may be clogged with events
      (which is especially possible in case of mouse look,
      when we use CastleControl, or when we use CastleWindow with LCL backend).
      In such cases we try hard to call "update" and (if necessary) "render"
      events at least as often as LimitFPS.
      When LimitFPS is used for this purpose ("desired number of FPS, not just a limit"),
      it is also capped (by 100.0).

      @unorderedList(
        @item(Comments specifically about TCastleWindow:

          This limits the number of TCastleApplication.ProcessMessage
          calls per second, in situations when we do not have to process any user input.
          So we limit not only rendering (@link(TCastleUserInterface.Render))
          but also @link(TCastleUserInterface.Update) processing
          (which includes physics and animation processing) calls per second.
          See TCastleApplication.ProcessMessage.

          See TCastleWindow.ProcessMessage documentation about WaitToLimitFPS
          parameter, and see TCastleApplication.LimitFPS documentation.)

        @item(Comments specifically about TCastleControl:

          This mechanism is activated only when some TCastleControl
          component is used, and only when LCL idle is fired (so we have no pending
          events, as LCL idle is "lazy" and fires only when process is really idle),
          and not at Lazarus design time.)
      )
    }
    property LimitFPS: Single read FLimitFPS write FLimitFPS {$ifdef FPC} default DefaultLimitFPS {$endif};

    { Called before OnGLContextOpen, even before Application.OnInitialize,
      but after we can read files. }
    property OnGLContextEarlyOpen: TGLContextEventList read FOnGLContextEarlyOpen;

    { Callbacks called when the OpenGL context is opened or closed.
      Use when you want to be notified about OpenGL context availability,
      but cannot refer to a particular instance of TCastleControl or TCastleWindow.

      Note that we may have many OpenGL contexts (many
      TCastleWindow or TCastleControl instances) open simultaneously.
      They all share OpenGL resources.
      OnGLContextOpen is called when first OpenGL context is open,
      that is: no previous context was open.
      OnGLContextClose is called when last OpenGL context is closed,
      that is: no more contexts remain open.
      Note that this implies that they may be called many times:
      e.g. if you open one window, then close it, then open another
      window then close it.

      Callbacks on OnGLContextOpen are called from first to last.
      Callbacks on OnGLContextClose are called in reverse order,
      so OnGLContextClose[0] is called last.

      @groupBegin }
    property OnGLContextOpen: TGLContextEventList read FOnGLContextOpen;
    property OnGLContextOpenObject: TNotifyEventList read FOnGLContextOpenObject;
    property OnGLContextClose: TGLContextEventList read FOnGLContextClose;
    property OnGLContextCloseObject: TNotifyEventList read FOnGLContextCloseObject;
    { @groupEnd }

    { Is the OpenGL context available. IOW, we are between the first OnGLContextOpen
      and last OnGLContextClose. }
    property IsGLContextOpen: boolean read FIsGLContextOpen;

    { Callbacks called continuously when (at least one) window is open. }
    property OnUpdate: TNotifyEventList read FOnUpdate;

    { Callbacks called when Android Java activity started.
      Called every time a Java activity is created.

      @unorderedList(
        @item(For the first time, it's called right before
          @link(TCastleApplication.OnInitialize).)

        @item(Later this is called when Java activity
          died (and is restarting now), but the native code thread survived.
          So all native code memory is already cool (no need to call
          @link(TCastleApplication.OnInitialize)),
          but we need to reinitialize Java part.

          Note that this is different from opening a new rendering context
          (when the @link(TCastleUserInterface.GLContextOpen) is called).
          On mobile we lose OpenGLES context often,
          actually every time user switches to another app,
          but we don't necessarily have our Java or native threads killed at this moment.
        )
      )

      For non-Android applications, this is simply always called exactly
      once, exactly before calling @link(TCastleApplication.OnInitialize). }
    property OnInitializeJavaActivity: TNotifyEventList read FOnInitializeJavaActivity;

    { Callbacks called when Android Java activity is paused or resumed.
      @italic(For now) not called on non-Android, but this may change ---
      consider these events somewhat internal for the time being.
      @groupBegin }
    property OnPause: TNotifyEventList read FOnPause;
    property OnResume: TNotifyEventList read FOnResume;
    { @groupEnd }

    { Events called upon @link(WritelnWarning). }
    property OnWarning: TWarningEventList read FOnWarning;

    { Events called upon any @link(WritelnLog), including @link(WritelnWarning). }
    property OnLog: TLogEventList read FOnLog;

    { Events called at @link(InitializeDebug).
      If @link(InitializeDebug) was already called, then we call the listener
      @italic(now), from this AddInitializeDebugListener! }
    procedure AddInitializeDebugListener(const Listener: TProcedure);

    { Add this to OnWarning to output warnings to standard output (usually, console).
      Eventually, on GUI Windows programs, it will make a dialog box.
      This is handled by @link(WarningWrite) procedure. }
    procedure WriteWarningOnConsole(const Category, Message: String);

    { Internal for Castle Game Engine.
      Called from CastleWindow or CastleControl.
      Don't call these methods yourself.
      @groupBegin
      @exclude }
    procedure _GLContextOpen;
    { @exclude }
    procedure _GLContextEarlyOpen;
    { @exclude }
    procedure _GLContextClose;
    { @exclude }
    procedure _Update;
    { @exclude }
    procedure _UpdateEnd;
    { @exclude }
    procedure _InitializeJavaActivity;
    { @exclude }
    procedure _Pause;
    { @exclude }
    procedure _Resume;
    { @exclude }
    procedure _Warning(const Category, Message: String);
    { @exclude }
    procedure _Log(const Message: String);
    { @exclude
      Indicates that operating on files (opening, saving, creating dirs) is safe.

      Always @true when not using CastleWindow (e.g. in command-line utilities,
      Lazarus CastleControl, or custom context situations like
      https://gist.github.com/michaliskambi/ca0eb18aeb7e326e5dc79c3b5002bcc5 ).

      In case of CastleWindow:
      On Android, opening files before Android application started (on the Java side)
      is not possible.
      On iOS, some things (like ApplicationConfig path) may not be initialized so early. }
    property _FileAccessSafe: boolean read FFileAccessSafe write FFileAccessSafe;
    { @groupEnd }

    { Print some common information about application,
      for example to use in --help command-line output.
      It shows application name, version, CGE version, compiler version, platform.

      Includes the output of SCompilerDescription and SPlatformDescription. }
    function Description: String;

    { Initialize debug facilities, that should not be enabled in released
      applications, but are very useful when enabled out-of-the-box
      during development.
      Must be called early (before any files are loaded).

      This includes now:

      @unorderedList(
        @item(Setting up @link(TCastleContainer.InputInspector) to enable
          invoking inspector at runtime, by pressing F8 key or
          pressing 3 fingers for 1 second on the screen.)

        @item(Setting up file monitor, so that it is possible to enable it
          at runtime, using "Monitor and Auto-Reload Data" from inspector.)
      )

      This is called automatically from auto-generated CastleAutoGenerated unit
      in each project, if the DEBUG symbol was defined during compilation.

      For backward compatibility, it is also called when the CGE units
      like CastleInternalFileMonitor and CastleUiControls
      are compiled with the DEBUG symbol, but please don't depend on this,
      it will be removed in future engine versions.
      You shall not depend on DEBUG / RELEASE symbols used when compiling the engine,
      they may be independent from the project DEBUG / RELEASE symbols. }
    procedure InitializeDebug;

    { Enable release features at run-time.
      This does *nothing* for now, but enables possible future extensions
      (e.g. special optimizations). }
    procedure InitializeRelease;

    { Free given component, at the nearest suitable moment.
      The pending free operations are done at least after processing
      all "update" events and before processing the "render" event
      (so the items pending to be freed will not be rendered). }
    procedure FreeDelayed(const Item: TComponent);
  end;

function ApplicationProperties(
  const CreateIfNotExisting: boolean = true): TCastleApplicationProperties;

implementation

uses CastleUtils;

{ TGLContextEventList -------------------------------------------------------- }

procedure TGLContextEventList.ExecuteForward;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I]();
end;

procedure TGLContextEventList.ExecuteBackward;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    Items[I]();
end;

{ TWarningEventList ---------------------------------------------------------- }

procedure TWarningEventList.ExecuteAll(const Category, Message: String);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I](Category, Message);
end;

{ TLogEventList ---------------------------------------------------------- }

procedure TLogEventList.ExecuteAll(const Message: String);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I](Message);
end;

{ TCastleApplicationProperties ----------------------------------------------- }

var
  FApplicationProperties: TCastleApplicationProperties;

function ApplicationProperties(const CreateIfNotExisting: boolean): TCastleApplicationProperties;
begin
  if (FApplicationProperties = nil) and CreateIfNotExisting then
    FApplicationProperties := TCastleApplicationProperties.Create;
  Result := FApplicationProperties;
end;

constructor TCastleApplicationProperties.Create;
begin
  inherited;
  FOnGLContextOpen := TGLContextEventList.Create;
  FOnGLContextEarlyOpen := TGLContextEventList.Create;
  FOnGLContextOpenObject := TNotifyEventList.Create;
  FOnGLContextClose := TGLContextEventList.Create;
  FOnGLContextCloseObject := TNotifyEventList.Create;
  FOnUpdate := TNotifyEventList.Create;
  FOnInitializeJavaActivity := TNotifyEventList.Create;
  FOnPause := TNotifyEventList.Create;
  FOnResume := TNotifyEventList.Create;
  FOnWarning := TWarningEventList.Create;
  FOnLog := TLogEventList.Create;
  FOnInitializeDebug := TProcedureList.Create;
  FFileAccessSafe := true;
  FTouchDevice :=
    {$if defined(ANDROID) or defined(CASTLE_IOS) or defined(CASTLE_NINTENDO_SWITCH)}
      true
    {$else}
      false
    {$endif};
  { Note: for now, FShowUserInterfaceToQuit starts just as a negation of FTouchDevice.
    But it will not always be like that.
    E.g. on other consoles, FTouchDevice may be false,
    but FShowUserInterfaceToQuit may be true. }
  FShowUserInterfaceToQuit :=
    {$if defined(ANDROID) or defined(CASTLE_IOS) or defined(CASTLE_NINTENDO_SWITCH)}
      false
    {$else}
      true
    {$endif};
  FLimitFPS := DefaultLimitFPS;
end;

destructor TCastleApplicationProperties.Destroy;
begin
  FreeAndNil(FPendingToFree);
  FreeAndNil(FOnGLContextOpen);
  FreeAndNil(FOnGLContextEarlyOpen);
  FreeAndNil(FOnGLContextOpenObject);
  FreeAndNil(FOnGLContextClose);
  FreeAndNil(FOnGLContextCloseObject);
  FreeAndNil(FOnUpdate);
  FreeAndNil(FOnInitializeJavaActivity);
  FreeAndNil(FOnPause);
  FreeAndNil(FOnResume);
  FreeAndNil(FOnWarning);
  FreeAndNil(FOnLog);
  FreeAndNil(FOnInitializeDebug);
  inherited;
end;

function TCastleApplicationProperties.GetApplicationName: String;
begin
  Result := {$ifdef FPC} SysUtils {$else} CastleUtils {$endif} .ApplicationName;
end;

var
  CastleApplicationNameValue: String;

function CastleGetApplicationName: String;
begin
  Result := CastleApplicationNameValue;
end;

procedure TCastleApplicationProperties.SetApplicationName(const Value: String);
begin
  OnGetApplicationName := @CastleGetApplicationName;
  CastleApplicationNameValue := Value;
end;

procedure TCastleApplicationProperties._GLContextEarlyOpen;
begin
  FIsGLContextOpen := true;
  FOnGLContextEarlyOpen.ExecuteForward;
end;

procedure TCastleApplicationProperties._GLContextOpen;
begin
  FIsGLContextOpen := true;
  FOnGLContextOpen.ExecuteForward;
  FOnGLContextOpenObject.ExecuteForward(Self);
end;

procedure TCastleApplicationProperties._GLContextClose;
begin
  FOnGLContextCloseObject.ExecuteBackward(Self);
  FOnGLContextClose.ExecuteBackward;
  FIsGLContextOpen := false;
end;

procedure TCastleApplicationProperties._Update;
begin
  DoPendingFree;
  FOnUpdate.ExecuteAll(Self);
end;

procedure TCastleApplicationProperties._UpdateEnd;
begin
  DoPendingFree;
end;

procedure TCastleApplicationProperties.DoPendingFree;
var
  I: Integer;
begin
  if FPendingToFree <> nil then
    for I := FPendingToFree.Count - 1 downto 0 do
      if I < FPendingToFree.Count then
        FPendingToFree[I].Free; // this will remove it from children, and from FPendingToFree
end;

procedure TCastleApplicationProperties.FreeDelayed(const Item: TComponent);
begin
  if FPendingToFree = nil then
    FPendingToFree := TComponentList.Create(false);
  { Do not allow duplicates on FPendingToFree list (which would happen
    if code calls FreeDelayed(xxx) with the same object).
    Duplicates make list not behave OK when item is freed, not all copies
    disappear from TComponentList then (looks like it removes only the 1st),
    and then FPendingToFree contains dangling pointer.
    Testcase: space_shooter, fly into rocks. }
  if FPendingToFree.IndexOf(Item) = -1 then
    FPendingToFree.Add(Item);
end;

procedure TCastleApplicationProperties._InitializeJavaActivity;
begin
  FOnInitializeJavaActivity.ExecuteAll(Self);
end;

procedure TCastleApplicationProperties._Pause;
begin
  FOnPause.ExecuteAll(Self);
end;

procedure TCastleApplicationProperties._Resume;
begin
  FOnResume.ExecuteAll(Self);
end;

procedure TCastleApplicationProperties._Warning(const Category, Message: String);
begin
  FOnWarning.ExecuteAll(Category, Message);
end;

procedure TCastleApplicationProperties._Log(const Message: String);
begin
  if FOnLog.Count <> 0 then // quickly eliminate most often case
    FOnLog.ExecuteAll(Message);
end;

procedure TCastleApplicationProperties.WriteWarningOnConsole(const Category, Message: String);
var
  WarningCategory: String;
begin
  if Category <> '' then
    WarningCategory := 'Warning: ' + Category
  else
    WarningCategory := 'Warning';
  WarningWrite(ApplicationName + ': ' + WarningCategory + ': ' + Message);
end;

function TCastleApplicationProperties.Description: String;
begin
  Result :=
    ApplicationName + ' version ' + Version + '.' + NL +
    'Using Castle Game Engine ( https://castle-engine.io/ ) version ' + CastleEngineVersion + '.' + NL +
    'Compiled with ' + SCompilerDescription + '.' + NL +
    'Platform: ' + SPlatformDescription + '.';
end;

procedure TCastleApplicationProperties.InitializeDebug;
begin
  if FInitializedDebug or FInitializedRelease then
    raise Exception.Create('Cannot call InitializeDebug after InitializeRelease or InitializeDebug was already called');
  FInitializedDebug := true;

  FOnInitializeDebug.ExecuteAll;
end;

procedure TCastleApplicationProperties.InitializeRelease;
begin
  if FInitializedDebug or FInitializedRelease then
    raise Exception.Create('Cannot call InitializeRelease after InitializeDebug or InitializeRelease was already called');
  FInitializedRelease := true;

  { Does nothing for now, but exists to provide easy extension point
    for future release-specific features.
    Called from CastleAutoGenerated unit in new projects. }
end;

procedure TCastleApplicationProperties.AddInitializeDebugListener(const Listener: TProcedure);
begin
  FOnInitializeDebug.Add(Listener);
  if FInitializedDebug then
    Listener();
end;

initialization
finalization
  FreeAndNil(FApplicationProperties);
end.
