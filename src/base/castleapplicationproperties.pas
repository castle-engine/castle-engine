{
  Copyright 2014-2018 Michalis Kamburelis.

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

uses Generics.Collections,
  CastleClassUtils;

type
  TGLContextEvent = procedure;

  TGLContextEventList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TList<TGLContextEvent>)
  public
    { Call all items, first to last. }
    procedure ExecuteForward;
    { Call all items, last to first. }
    procedure ExecuteBackward;
  end;

  TWarningEvent = procedure (const Category, Message: String) of object;
  TLogEvent = procedure (const Message: String) of object;

  TWarningEventList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TList<TWarningEvent>)
  public
    procedure ExecuteAll(const Category, Message: String);
  end;

  TLogEventList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TList<TLogEvent>)
  public
    procedure ExecuteAll(const Message: String);
  end;

  { Events and properties of the Castle Game Engine application,
    usually accessed through the @link(ApplicationProperties) singleton.

    These members work regardless if you use CastleWindow or CastleControl.
    For more fine-grained application control,
    see TCastleApplication (in case you use CastleWindow)
    or Lazarus (LCL) TApplication (in case you use CastleControl). }
  TCastleApplicationProperties = class
  private
    FIsGLContextOpen, FFileAccessSafe: boolean;
    FOnGLContextOpen, FOnGLContextClose: TGLContextEventList;
    FOnUpdate, FOnInitializeJavaActivity,
      FOnGLContextOpenObject, FOnGLContextCloseObject,
      FOnPause, FOnResume: TNotifyEventList;
    FOnWarning: TWarningEventList;
    FOnLog: TLogEventList;
    FVersion: String;
    FTouchDevice: boolean;
    FLimitFPS: Single;
    FShowUserInterfaceToQuit: Boolean;
    function GetApplicationName: String;
    procedure SetApplicationName(const Value: String);
  public
    const
      DefaultLimitFPS = 100.0;

    constructor Create;
    destructor Destroy; override;

    { Application short name.
      Used e.g. by @link(InitializeLog) to name the log file.

      When compiled with FPC, this returns and sets the same thing
      as standard SysUtils.ApplicationName.
      When setting this, we automatically set SysUtils.OnGetApplicationName. }
    property ApplicationName: String read GetApplicationName write SetApplicationName;

    { Version of this application.
      It may be used e.g. by @link(InitializeLog) and
      @link(TCastleApplication.ParseStandardParameters). }
    property Version: String read FVersion write FVersion;

    { Initialized to @true on touch devices (Android, iOS, Nintendo Switch).

      A "touch device" means that:

      @unorderedList(
        @item(We cannot track @link(TUIContainer.MousePosition)
          when nothing is pressed (@link(TUIContainer.MousePressed) is [])
          on a touch device.)

        @item(The only "mouse button" you will ever see pressed
          on a touch device is buttonLeft.)

        @item(On the other hand, touch devices support multitouch, exposed by
          @link(TUIContainer.Touches) and @link(TUIContainer.TouchesCount).
          On touch devices, @link(TUIContainer.TouchesCount) can range
          from 0 to a few (modern touch devices support up to 5 simultaneous
          touches).

          On non-touch devices, @link(TUIContainer.TouchesCount) is always 1.)
      )

      As a debugging feature, you can set this to @true
      to simulate touch devices on a desktop.
      The idea is that when an application shows a different input behaviour
      on touch devices, it should always condition it depending on
      this boolean property. So an application may do this:

      @longCode(#
      Viewport.WalkCamera.MouseLook := not ApplicationProperties.TouchDevice;
      #)

      And to test on desktop whether everything behaves OK on mobile,
      you can just earlier call this:

      @longCode(#
      if FakeTouchDeviceOnDesktop then
        ApplicationProperties.TouchDevice := true;
      #)
    }
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
      So it's a global thing, not just a property of TCastleWindowBase or TCastleControlBase.

      In some cases, this also means the "desired number of FPS".
      This happens when we may be clogged with events
      (which is especially possible in case of mouse look,
      when we use CastleControl, or when we use CastleWindow with LCL backend).
      In such cases we try hard to call "update" and (if necessary) "render"
      events at least as often as LimitFPS.
      When LimitFPS is used for this purpose ("desired number of FPS, not just a limit"),
      it is also capped (by 100.0).

      @unorderedList(
        @item(Comments specifically about TCastleWindowBase:

          This limits the number of TCastleApplication.ProcessMessage
          calls per second, in situations when we do not have to process any user input.
          So we limit not only rendering (TCastleWindowBase.OnRender)
          but also other animation processing (TCastleWindowBase.OnUpdate) calls per second.
          See TCastleApplication.ProcessMessage.

          See TCastleWindow.ProcessMessage documentation about WaitToLimitFPS
          parameter, and see TCastleApplication.LimitFPS documentation.)

        @item(Comments specifically about TCastleControlBase:

          This mechanism is activated only when some TCastleControlBase
          component is used, and only when LCL idle is fired (so we have no pending
          events, as LCL idle is "lazy" and fires only when process is really idle),
          and not at Lazarus design time.)
      )
    }
    property LimitFPS: Single read FLimitFPS write FLimitFPS {$ifdef FPC} default DefaultLimitFPS {$endif};

    { Callbacks called when the OpenGL context is opened or closed.
      Use when you want to be notified about OpenGL context availability,
      but cannot refer to a particular instance of TCastleControlBase or TCastleWindowBase.

      Note that we may have many OpenGL contexts (many
      TCastleWindowBase or TCastleControlBase instances) open simultaneously.
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

    { Callbacks called continuously when (at least one) window is open.

      You can use this just like @link(TCastleControlBase.OnUpdate)
      or @link(TCastleWindowBase.OnUpdate) or @link(TCastleApplication.OnUpdate). }
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

          Note that this is different from @link(TCastleWindowBase.OnOpen).
          We lose OpenGL context often, actually every time user switches to another
          app, without having neither Java nor native threads killed.
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
  end;

function ApplicationProperties(
  const CreateIfNotExisting: boolean = true): TCastleApplicationProperties;

implementation

uses SysUtils,
  CastleUtils;

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
  FOnGLContextOpenObject := TNotifyEventList.Create;
  FOnGLContextClose := TGLContextEventList.Create;
  FOnGLContextCloseObject := TNotifyEventList.Create;
  FOnUpdate := TNotifyEventList.Create;
  FOnInitializeJavaActivity := TNotifyEventList.Create;
  FOnPause := TNotifyEventList.Create;
  FOnResume := TNotifyEventList.Create;
  FOnWarning := TWarningEventList.Create;
  FOnLog := TLogEventList.Create;
  FFileAccessSafe := true;
  FTouchDevice :=
    {$if defined(ANDROID) or defined(IOS) or defined(CASTLE_NINTENDO_SWITCH)}
      true
    {$else}
      false
    {$endif};
  { Note: for now, FShowUserInterfaceToQuit starts just as a negation of FTouchDevice.
    But it will not always be like that.
    E.g. on other consoles, FTouchDevice may be false,
    but FShowUserInterfaceToQuit may be true. }
  FShowUserInterfaceToQuit :=
    {$if defined(ANDROID) or defined(IOS) or defined(CASTLE_NINTENDO_SWITCH)}
      false
    {$else}
      true
    {$endif};
  FLimitFPS := DefaultLimitFPS;
end;

destructor TCastleApplicationProperties.Destroy;
begin
  FreeAndNil(FOnGLContextOpen);
  FreeAndNil(FOnGLContextOpenObject);
  FreeAndNil(FOnGLContextClose);
  FreeAndNil(FOnGLContextCloseObject);
  FreeAndNil(FOnUpdate);
  FreeAndNil(FOnInitializeJavaActivity);
  FreeAndNil(FOnPause);
  FreeAndNil(FOnResume);
  FreeAndNil(FOnWarning);
  FreeAndNil(FOnLog);
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
  FOnUpdate.ExecuteAll(Self);
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

initialization
finalization
  FreeAndNil(FApplicationProperties);
end.
