{
  Copyright 2014-2017 Michalis Kamburelis.

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

  TWarningEvent = procedure (Sender: TObject; const Category, Message: string) of object;

  TWarningEventList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TList<TWarningEvent>)
  public
    procedure ExecuteAll(Sender: TObject; const Category, Message: string);
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
    FVersion: string;
    function GetApplicationName: string;
    procedure SetApplicationName(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    { Version of this application.
      It may be used e.g. by @link(InitializeLog) and @link(TCastleWindow.ParseStandardParameters). }
    property Version: string read FVersion write FVersion;

    { Application short name.
      Used e.g. by @link(InitializeLog) to name the log file.

      When compiled with FPC, this returns and sets the same thing
      as standard SysUtils.ApplicationName.
      When setting this, we automatically set SysUtils.OnGetApplicationName. }
    property ApplicationName: string read GetApplicationName write SetApplicationName;

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

    { Callbacks called continously when (at least one) window is open.

      You can use this just like @link(TCastleControlCustom.OnUpdate)
      or @link(TCastleWindowCustom.OnUpdate) or @link(TCastleApplication.OnUpdate),
      but in situations where you cannot access an instance of control/window
      and you want to work both with Lazarus @link(TCastleControl)
      and our custom @link(TCastleApplication). }
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

          Note that this is different from @link(TCastleWindowCustom.OnOpen).
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

    property OnWarning: TWarningEventList read FOnWarning;

    { Add this to OnWarning to output warnings to standard output (usually, console).
      Eventually, on GUI Windows programs, it will make a dialog box.
      This is handled by @link(WarningWrite) procedure. }
    procedure WriteWarningOnConsole(Sender: TObject; const Category, Message: string);

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
    procedure _Warning(const Category, Message: string);
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

procedure TWarningEventList.ExecuteAll(Sender: TObject; const Category, Message: string);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I](Sender, Category, Message);
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
  FFileAccessSafe := true;
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
  inherited;
end;

function TCastleApplicationProperties.GetApplicationName: string;
begin
  Result := {$ifdef FPC} SysUtils {$else} CastleUtils {$endif} .ApplicationName;
end;

var
  CastleApplicationNameValue: string;

function CastleGetApplicationName: string;
begin
  Result := CastleApplicationNameValue;
end;

procedure TCastleApplicationProperties.SetApplicationName(const Value: string);
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

procedure TCastleApplicationProperties._Warning(const Category, Message: string);
begin
  FOnWarning.ExecuteAll(Self, Category, Message);
end;

procedure TCastleApplicationProperties.WriteWarningOnConsole(
  Sender: TObject; const Category, Message: string);
begin
  WarningWrite(ApplicationName + ': ' + Category + ' warning: ' + Message);
end;

initialization
finalization
  FreeAndNil(FApplicationProperties);
end.
