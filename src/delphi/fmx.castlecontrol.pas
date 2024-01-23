{
  Copyright 2022-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Control rendering "Castle Game Engine" on Delphi FMX (FireMonkey) form. }
unit Fmx.CastleControl;

{$I castleconf.inc}

{ How to implement calling Update continuously?
  Similar choice as castlewindow_form.inc discusses.

  - USE_TIMER is preferred.
    It "just works" from user point of view.
    We use this on all platforms (Windows)...

  - ...except Linux, when with FMXLinux we failed to make it work.
    It means user code must execute loop in a special way.
    In the main program code, instead of

      Application.Run;

    do

      TCastleControl.ApplicationRun;

    And if anywhere you do

      Application.ProcessMessages;

    enhance it with

      Application.ProcessMessages;
      TCastleControl.ProcessTasks; // additional CGE processing
}
{$if not defined(LINUX)}
  {$define USE_TIMER}
{$endif}

interface

uses // standard units
  SysUtils, Classes,
  // fmx
  {$ifdef MSWINDOWS} FMX.Presentation.Win, {$endif}
  FMX.Controls, FMX.Controls.Presentation, FMX.Types, UITypes,
  // cge
  CastleGLVersion, CastleGLUtils, CastleVectors, CastleKeysMouse,
  CastleInternalContextBase, CastleInternalContainer, CastleInternalFmxUtils;

type
  { Control rendering "Castle Game Engine" on FMX form. }
  TCastleControl = class(TPresentedControl)
  strict private
    type
      { Non-abstract implementation of TCastleContainer that cooperates with
        TCastleControl. }
      TContainer = class(TCastleContainerEasy)
      private
        Parent: TCastleControl;
        {$ifdef USE_TIMER}
        class var
          UpdatingTimer: TTimer;
        {$endif}
        class procedure UpdatingTimerEvent(Sender: TObject);
      protected
        function GetMousePosition: TVector2; override;
        procedure SetMousePosition(const Value: TVector2); override;
        procedure AdjustContext(const PlatformContext: TGLContext); override;
        class procedure UpdatingEnable; override;
        class procedure UpdatingDisable; override;
      public
        constructor Create(AParent: TCastleControl); reintroduce;
        procedure Invalidate; override;
        function PixelsWidth: Integer; override;
        function PixelsHeight: Integer; override;
        procedure SetInternalCursor(const Value: TMouseCursor); override;
      end;

    var
      FContainer: TContainer;
      FMousePosition: TVector2;
      FGLUtility: TFmxOpenGLUtility;

    function GetCurrentShift: TShiftState;
    procedure SetCurrentShift(const Value: TShiftState);

    { Current knowledge about shift state, based on Container.Pressed.

      Call this whenever you have new new shift state.

      Sometimes, releasing shift / alt / ctrl keys will not be reported
      properly to KeyDown / KeyUp. Example: opening a menu
      through Alt+F for "_File" will make keydown for Alt,
      but not keyup for it, and DoExit will not be called,
      so ReleaseAllKeysAndMouse will not be called.

      To counteract this, set this whenever Shift state is known,
      to update Container.Pressed when needed. }
    property CurrentShift: TShiftState
      read GetCurrentShift write SetCurrentShift;

    function MousePosToCastle(const X, Y: Single): TVector2;
  private
    procedure CreateHandle;
    procedure DestroyHandle;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; NewX, NewY: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;
    function DefinePresentationName: String; override;
    procedure Resize; override;
    // Not needed in the end // procedure DoRootChanged; override;
    procedure DoRootChanging(const NewRoot: IRoot); override;
    procedure SetVisible(const AValue: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;

    { If Handle not allocated yet, allocate it now.
      This makes sure we have OpenGL context created.
      Our OpenBackend must guarantee it, we want to initialize GLVersion
      afterwards etc. }
    procedure InternalHandleNeeded;

    { This control must always have "native style", which means
      it has ControlType = Platform. See FMX docs about native controls:
      https://docwiki.embarcadero.com/RADStudio/Sydney/en/FireMonkey_Native_Windows_Controls
      Native controls are always on top of non-native controls. }
    property ControlType default TControlType.Platform;

    { On some platforms (Linux now) if you call Application.ProcessMessages,
      make sure to also call this method. E.g.

      @longCode(#
      while SomeCondition do
      begin
        Application.ProcessMessages;
        TCastleControl.ProcessTasks;
      end;
      #)

      This does nothing on some other platforms (Windows)
      when it is not necessary and Application.ProcessMessages does all CGE job. }
    class procedure ProcessTasks;

    { On some platforms (Linux now) you cannot just call Application.Run,
      as we need to call CGE processing regularly. So instead call
      this method.
      This ensures that engine updates / renders regularly.

      On some other platforms (Windows)
      this just calls Application.Run, which already makes
      engine processing correct. }
    class procedure ApplicationRun;
  published
    { Access Castle Game Engine container properties and events,
      not specific for FMX. }
    property Container: TContainer read FContainer;

    property Align;
    property Anchors;
    property OnClick;
    property OnDblClick;
    property Height;
    property Width;
    property Size;
    property Position;
    property Margins;
    property TabStop default true;
    property TabOrder;
    property CanFocus default True;
  end;

procedure Register;

implementation

uses FMX.Presentation.Factory, Types, FMX.Graphics, FMX.Forms,
  CastleRenderOptions, CastleApplicationProperties, CastleRenderContext,
  CastleRectangles, CastleUtils, CastleUIControls, CastleInternalDelphiUtils,
  CastleLog;

procedure Register;
begin
  RegisterComponents('Castle', [
    TCastleControl
  ]);
end;

{$ifdef MSWINDOWS}

{ TWinNativeGLControl -------------------------------------------------------- }

type
  { Presentation for TCastleControl.
    This class is necessary to manage WinAPI HWND associated with FMX control. }
  TWinNativeGLControl = class(TWinPresentation)
  protected
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
  public
    function CastleControl: TCastleControl;
  end;

function TWinNativeGLControl.CastleControl: TCastleControl;
begin
  Result := Control as TCastleControl;
end;

procedure TWinNativeGLControl.CreateHandle;
begin
  inherited;
  { Looking at TWinNativeMemo.CreateHandle confirms this can be called with Handle = null }
  if Handle <> NullHWnd then
    CastleControl.CreateHandle;
end;

procedure TWinNativeGLControl.DestroyHandle;
begin
  if Handle <> NullHWnd then
    CastleControl.DestroyHandle;
  inherited;
end;

{$endif}

{ TCastleControl.TContainer ---------------------------------------------------}

constructor TCastleControl.TContainer.Create(AParent: TCastleControl);
begin
  inherited Create(AParent); // AParent must be a component Owner to show published properties of container in LFM
  Parent := AParent;
end;

procedure TCastleControl.TContainer.AdjustContext(const PlatformContext: TGLContext);
begin
  Parent.FGLUtility.ContextAdjustEarly(PlatformContext);
end;

function TCastleControl.TContainer.GetMousePosition: TVector2;
begin
  Result := Parent.FMousePosition;
end;

procedure TCastleControl.TContainer.SetMousePosition(const Value: TVector2);
begin
  { TODO

    There's no facility to do this using FMX, it seems.
    We need platform-specific code.
    - So far in CastleFmxUtils we have FmxSetMousePos for Linux, use it.
    - And implement equivalent for Windows.
  }
end;

function TCastleControl.TContainer.PixelsWidth: Integer;
var
  Scale: Single;
begin
  if Parent.FGLUtility <> nil then
    Scale := Parent.FGLUtility.Scale
  else
    Scale := 1;

  Result := Round(Parent.Width * Scale);
end;

function TCastleControl.TContainer.PixelsHeight: Integer;
var
  Scale: Single;
begin
  if Parent.FGLUtility <> nil then
    Scale := Parent.FGLUtility.Scale
  else
    Scale := 1;

  Result := Round(Parent.Height * Scale);
end;

procedure TCastleControl.TContainer.Invalidate;
begin
  Parent.InvalidateRect(TRectF.Create(0, 0, Width, Height));
end;

procedure TCastleControl.TContainer.SetInternalCursor(const Value: TMouseCursor);
begin
  { TODO (use similar code from CASTLE_WINDOW_FORM implementation).

    Note: This is commonly used by MouseLook, but it will not work OK
    until both this and SetMousePosition are implemented. }
end;

{ TCastleControl ---------------------------------------------------- }

constructor TCastleControl.Create(AOwner: TComponent);
begin
  inherited;

  FContainer := TContainer.Create(Self);
  FContainer.SetSubComponent(true);
  FContainer.Name := 'Container';

  FGLUtility := TFmxOpenGLUtility.Create;
  FGLUtility.Control := Self;
  FGLUtility.OnHandleAfterCreateEvent := CreateHandle;
  FGLUtility.OnHandleBeforeDestroyEvent := DestroyHandle;

  { In FMX, this causes adding WS_TABSTOP to Params.Style
    in TWinPresentation.CreateParams. So it is more efficient to call
    before we actually create window by setting ControlType. }
  TabStop := true;

  CanFocus := True;

  { Makes the Presentation be TWinNativeGLControl, which has HWND.
    Do this after FContainer is initialized, as it may call CreateHandle,
    which in turn requires FContainer to be created.

    Note that we cannnot do this at design-time (in Delphi IDE):

    - Switching to TControlType.Platform at design-time
      creates additional weird (visible in task bar, detached from form designer)
      window on Windows, alongside main Delphi IDE window.

      User can even close this window, causing crashes
      (later when closing the FMX form, there will be exception,
      because the Windows handle went away and FMX is not prepared for it).

    - We *can* create OpenGL context in this weird window, and render there...
      But all my experiments to attach it to form designer in Delphi IDE failed.
      Overriding TWinNativeGLControl.CreateParams to

      1. Params.Style := Params.Style or WS_CHILD
      2. Params.WndParent := ContainerHandle;
      3. CreateSubClass(Params, 'CastleControl');

      .. yield nothing.

      1 and 2 should indeed not be necessary, this is done by default by FMX,
      we have WS_CHILD by default.

      None of the above cause our rendering to be attached to Delphi IDE
      as you would expect.

    - FMX controls cannot render in native style at design-time it seems.

      That is also the case for TMemo or TEdit,
      their rendering can be native only at runtime (if you set their ControlType
      to platform), at design-time they just display the "styled" (non-native)
      along with an icon informing they will be native at runtime.

      See
      https://docwiki.embarcadero.com/RADStudio/Alexandria/en/FireMonkey_Native_Windows_Controls#Visual_Changes_to_Native_Windows_Controls
  }
  if not (csDesigning in ComponentState) then
    ControlType := TControlType.Platform;
end;

destructor TCastleControl.Destroy;
begin
  FreeAndNil(FGLUtility);
  inherited;
end;

procedure TCastleControl.CreateHandle;
begin
  { Do not create context at design-time.
    We don't even set "ControlType := TControlType.Platform" at design-time now
    (see constructor comments),
    this line only secures in case user would set ControlType in a TCastleControl
    descendant at design-time. }
  if csDesigning in ComponentState then
    Exit;

  { Thanks to TWinNativeGLControl, we have Windows HWND for this control now in
      (Presentation as TWinNativeGLControl).Handle
    This is used in AdjustContext and
    is necessary to create OpenGL context that only renders to this control.

    Note: The only other way in FMX to get HWND seems to be to get form HWND,
      WindowHandleToPlatform(Handle).Wnd
    but this is not useful for us (we don't want to always render to full window).
  }
  FContainer.InitializeContext;
end;

procedure TCastleControl.DestroyHandle;
begin
  FContainer.FinalizeContext;
end;

procedure TCastleControl.Paint;
var
  R: TRectF;
begin
  { See our constructor comments:
    looks like native drawing at design-time in FMX is just not possible reliably. }

  if csDesigning in ComponentState then
  begin
    inherited;
    R := LocalRect;

    Canvas.Fill.Kind := TBrushKind.Solid;
    Canvas.Fill.Color := $A0909090;
    Canvas.FillRect(R, 0, 0, [], 1.0);

    Canvas.Fill.Color := TAlphaColors.Yellow;
    Canvas.FillText(R,
      'Run the project to see actual rendering of ' + Name + ' (' + ClassName + ')',
      true, 1.0, [], TTextAlign.Center);
  end else
  begin
    { We must have OpenGL context at this point,
      and on Delphi/Linux there is no way to register "on native handle creation",
      we must manually perform native handle creation (with GL context)
      using our FGLUtility.

      Maybe in the future it will make sense to do this also from some other events,
      like update or mouse/key press?
      Doesn't seem necessary in practice for now. }
    InternalHandleNeeded;

    { TODO: Not the best place to call this.
      This assumes we render often (like every frame)
      and we can update our GTK control size (thus OpenGL size)
      right before render.
      Hm, but we don't have in this class comfortable Update (for this TCastleWindow)
      now, though our Container could expose it. }
    FGLUtility.Update;

    // inherited not needed, and possibly causes something unnecessary
    FContainer.DoRender;
  end;
end;

class procedure TCastleControl.TContainer.UpdatingEnable;
begin
  inherited;

  {$ifdef USE_TIMER}
  UpdatingTimer := TTimer.Create(nil);
  UpdatingTimer.Interval := 1;
  UpdatingTimer.OnTimer := {$ifdef FPC}@{$endif} UpdatingTimerEvent;
  {$endif}
end;

class procedure TCastleControl.TContainer.UpdatingDisable;
begin
  {$ifdef USE_TIMER}
  FreeAndNil(UpdatingTimer);
  {$endif}
  inherited;
end;

class procedure TCastleControl.TContainer.UpdatingTimerEvent(Sender: TObject);
begin
  DoUpdateEverything;
end;

class procedure TCastleControl.ProcessTasks;
begin
  // Does nothing when USE_TIMER, which is OK, timer does everything
  {$ifndef USE_TIMER}
  TContainer.UpdatingTimerEvent(nil);
  {$endif}
end;

procedure TCastleControl.Resize;
begin
  inherited;
  if Container.GLInitialized then
  begin
    Container.MakeContextCurrent;
    Container.EventResize;
  end;
end;

class procedure TCastleControl.ApplicationRun;
begin
  {$ifdef USE_TIMER}
  Application.Run;
  {$else}
  Application.RealCreateForms;

  { On Linux, it's especially important to check Terminating (not just Terminated)
    below.

    That's because FMXLinux seems to rely on GTK mechanism to exit
    main loop when user calls Application.Terminate, which means it assumes
    that you have executed Application.Run. (But we cannot use Application.Run
    because timer is unreliable on FMXLinux and would sometimes hang application
    rendering, see USE_TIMER comments.) Calling "Application.Run" sets
    only Terminating:=true and makes GTK error:

       gtk_main_quit: assertion 'main_loops != NULL' failed

    We can't really prevent GTK error,
    but at least we can react to Terminating and exit. }

  while not (ApplicationState in [
    TApplicationState.Terminating,
    TApplicationState.Terminated]) do
  begin
    Application.ProcessMessages;
    ProcessTasks;
  end;

  { TODO (but it seems FMXLinux issue -- we should report):
    On FMXLinux, not using "Application.Run"
    means that FMXLinux never frees the Application singleton.

    Consequently, things owned by Application are never freed,
    TForm.OnDestroy callbacks are not run,
    and component destructors like TCastleControl.Destroy are not run.
    Things just leak at program exit.

    It seems FMXLinux "TPlatformLinux.Destroy"
    should just call "FreeAndNil(Application)". This is what Delphi built-in
    "TPlatformWin.Destroy" and "TPlatformCocoa.Destroy" are doing.
    It also makes sense since "TPlatformLinux.Create" creates the Application
    singleton.

    In fact, I haven't found *how is Application freed when
    we call Application.Run*. FMXLinux never seems to free Application.
    Unlike Windows and macOS FMX platforms.
    But logging shows that forms *are* freed at application exit
    if we run Application.Run, but not otherwise.

    This is reproducible in FMX almost-blank application, without any CGE,
    just remove Application.Run but call Application.RealCreateForms .
    You will notice that form OnCreate callback executes,
    but form OnDestroy callback never happens. }

  FreeAndNil(Application);
  {$endif}
end;

function TCastleControl.GetCurrentShift: TShiftState;
begin
  Result := [];
  if Container.Pressed.Keys[keyShift] then
    Include(Result, ssShift);
  if Container.Pressed.Keys[keyAlt] then
    Include(Result, ssAlt);
  if Container.Pressed.Keys[keyCtrl] then
    Include(Result, ssCtrl);
end;

procedure TCastleControl.SetCurrentShift(const Value: TShiftState);
begin
  Container.Pressed.Keys[keyShift] := ssShift in Value;
  Container.Pressed.Keys[keyAlt  ] := ssAlt   in Value;
  Container.Pressed.Keys[keyCtrl ] := ssCtrl  in Value;
end;

function TCastleControl.MousePosToCastle(const X, Y: Single): TVector2;
begin
  Result := Vector2(X, Height - 1 - Y) * FGLUtility.Scale;
end;

procedure TCastleControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  MyButton: TCastleMouseButton;
begin
  if not IsFocused then // TODO: doesn't seem to help with focus
    SetFocus;

  inherited; { FMX OnMouseDown before our callbacks }

  FMousePosition := MousePosToCastle(X, Y);

  if MouseButtonToCastle(Button, MyButton) then
    Container.MousePressed := Container.MousePressed + [MyButton];

  CurrentShift := Shift; { do this after Pressed update above, and before *Event }

  if MouseButtonToCastle(Button, MyButton) then
    Container.EventPress(InputMouseButton(FMousePosition, MyButton, 0,
      ModifiersDown(Container.Pressed)));
end;

procedure TCastleControl.MouseMove(Shift: TShiftState; NewX, NewY: Single);
var
  NewMousePos: TVector2;
begin
  inherited;

  NewMousePos := MousePosToCastle(NewX, NewY);

  Container.EventMotion(InputMotion(FMousePosition,
    NewMousePos, Container.MousePressed, 0));

  // change FMousePosition *after* EventMotion, callbacks may depend on it
  FMousePosition := NewMousePos;

  CurrentShift := Shift;
end;

procedure TCastleControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  MyButton: TCastleMouseButton;
begin
  inherited; { FMX OnMouseUp before our callbacks }

  FMousePosition := MousePosToCastle(X, Y);

  if MouseButtonToCastle(Button, MyButton) then
    Container.MousePressed := Container.MousePressed - [MyButton];

  CurrentShift := Shift; { do this after Pressed update above, and before *Event }

  if MouseButtonToCastle(Button, MyButton) then
    Container.EventRelease(InputMouseButton(FMousePosition, MyButton, 0));
end;

procedure TCastleControl.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
begin
  if not Handled then
    Handled := Container.EventPress(InputMouseWheel(
      FMousePosition, WheelDelta / 120, true, ModifiersDown(Container.Pressed)));

  inherited;
end;

procedure TCastleControl.KeyDown(var Key: Word; var KeyChar: WideChar;
  Shift: TShiftState);
var
  CastleKey: TKey;
  CastleKeyString: String;
  CastleEvent: TInputPressRelease;
begin
  inherited;
  CurrentShift := Shift;

  FmxKeysToCastle(Key, KeyChar, Shift, CastleKey, CastleKeyString);

  if (CastleKey <> keyNone) or (CastleKeyString <> '') then
  begin
    CastleEvent := InputKey(FMousePosition, CastleKey, CastleKeyString,
      ModifiersDown(Container.Pressed));

    // check this before updating Container.Pressed
    CastleEvent.KeyRepeated :=
      // Key already pressed
      ((CastleEvent.Key = keyNone) or Container.Pressed.Keys[CastleEvent.Key]) and
      // KeyString already pressed
      ((CastleEvent.KeyString = '') or Container.Pressed.Strings[CastleEvent.KeyString]);

    Container.Pressed.KeyDown(CastleEvent.Key, CastleEvent.KeyString);
    if Container.EventPress(CastleEvent) then
    begin
      Key := 0;
      KeyChar := #0;
    end;
  end;
end;

procedure TCastleControl.KeyUp(var Key: Word; var KeyChar: WideChar;
  Shift: TShiftState);
var
  CastleKey: TKey;
  CastleKeyString: String;
  CastleEvent: TInputPressRelease;
begin
  inherited;
  CurrentShift := Shift;

  FmxKeysToCastle(Key, KeyChar, Shift, CastleKey, CastleKeyString);

  { Note that KeyUp seems to have additional issue with FMXLinux,
    not fixed in this code:

    When the key is held (so we expect to receive multiple KeyDown
    for one KeyUp), FMXLinux sends additional KeyUp before each KeyDown.
    So for code, it seems as if user is pressing and releasing the key.
    We cannot easily workaround it on CGE side.
    To track keys being held, we advise to check Pressed[keyXxx] in Update
    methods, instead of relying on KeyDown/KeyUp. }

  if (CastleKey <> keyNone) or (CastleKeyString <> '') then
  begin
    CastleEvent := InputKey(FMousePosition, CastleKey, CastleKeyString,
      ModifiersDown(Container.Pressed));

    Container.Pressed.KeyUp(CastleEvent.Key, CastleEvent.KeyString);
    if Container.EventRelease(CastleEvent) then
    begin
      Key := 0;
      KeyChar := #0;
    end;
  end;
end;

procedure TCastleControl.InternalHandleNeeded;
begin
  FGLUtility.HandleNeeded;
end;

function TCastleControl.DefinePresentationName: String;
begin
  { This method may seem not necessary in some tests: if your application
    just instantiates exactly TCastleControl (not a descendant of it),
    then this method is not necessary.
    E.g. CastleFmx example doesn't need it to work properly.

    But this method becomes necessary if you instantiate *descendants of
    TCastleControl*. Like TGoodOpenGLControl created by CASTLE_WINDOW_FORM.
    Without this method, these descendants would not
    use TWinNativeGLControl (they would use default FMX
    TWinStyledPresentation) and in effect critical code from CGE
    TWinNativeGLControl would not run.

    See also:
    - FMX TMemo does it, likely for above reasons.
    - https://stackoverflow.com/questions/37281970/a-descendant-of-tstyledpresentationproxy-has-not-been-registered-for-class
    - See also
      http://yaroslavbrovin.ru/new-approach-of-development-of-firemonkey-control-control-model-presentation-part-1-en/
      https://github.com/tothpaul/Firemonkey/tree/master/GLPanel
      for hints how to use platform-specific controls with FMX.
  }

  Result := 'CastleControl-' + GetPresentationSuffix;
end;

procedure TCastleControl.SetVisible(const AValue: Boolean);
begin
  inherited;
  { This happens e.g. when developer does "CastleControl.Visible := false".
    We will recreate this handle later by FGLUtility.HandleNeeded,
    at first paint. }
  if not AValue then
    FGLUtility.HandleRelease;
end;

procedure TCastleControl.DoRootChanging(const NewRoot: IRoot);
begin
  inherited;
  { This happens e.g. when developer does "CastleControl.Parent := nil"
    or when TCastleControl is destroyed (e.g. because parent form is destroyed).
    We will recreate this handle later by FGLUtility.HandleNeeded,
    at first paint. }
  FGLUtility.HandleRelease;
end;

(*
procedure TCastleControl.DoRootChanged;
begin
  inherited;
  { Not really needed, we'll create context from Paint anyway.
    And this was too early in some cases. }
//  if FGLUtility.HandlePossible then
//    FGLUtility.HandleNeeded;
end;
*)

{$ifdef MSWINDOWS}
initialization
  { Make TWinNativeGLControl used
    for TCastleControl with ControlType = TControlType.Platform. }
  TPresentationProxyFactory.Current.Register(TCastleControl, TControlType.Platform, TWinPresentationProxy<TWinNativeGLControl>);
finalization
  TPresentationProxyFactory.Current.Unregister(TCastleControl, TControlType.Platform, TWinPresentationProxy<TWinNativeGLControl>);
{$endif}
end.
