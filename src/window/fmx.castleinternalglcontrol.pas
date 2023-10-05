{
  Copyright 2022-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Control with OpenGL context (and nothing else, like CGE container)
  on a Delphi FMX form.
  This is like TCastleControl, but stripped from all CGE container features.
  It is similar in concept to TOpenGLControl from LCL,
  though implementation is independent -- we use special tricks
  to get OpenGL context on FMX form.

  This is only internal for TCastleWindow in CASTLE_WINDOW_FORM case.
  So we don't register it, don't care about publishing stuff,
  don't care how it looks in Delphi IDE etc.

  TODO: For now, this doesn't share code with TCastleControl,
  but it duplicates some of it. Any way to make it share more code?

  TODO: CASTLE_WINDOW_FORM could use reliable update mechanism
  from easy container? }
unit Fmx.CastleInternalGLControl;

{$I castleconf.inc}

interface

uses // standard units
  {$ifdef MSWINDOWS} Windows, {$endif}
  SysUtils, Classes,
  // fmx
  {$ifdef MSWINDOWS} FMX.Presentation.Win, {$endif}
  {$ifdef LINUX} FMX.Platform.Linux, {$endif}
  FMX.Controls, FMX.Controls.Presentation, FMX.Types, UITypes,
  // cge
  {$ifdef MSWINDOWS} CastleInternalContextWgl, {$endif}
  {$ifdef LINUX} CastleInternalContextEgl, {$endif}
  CastleInternalContextBase;

type
  { Control with OpenGL context (and nothing else, like CGE container)
    on a Delphi FMX form.
    This is like TCastleControl, but stripped from all CGE container features. }
  TOpenGLControl = class(TPresentedControl)
  private
    FRequirements: TGLContextRequirements;
    { Internal platform-specific context data and initialization/finalization.
      This is in contrast to TCastleContainer.Context, that is public
      and manages context properties that are cross-platform and available
      for all OpenGL(ES). }
    FPlatformContext: TGLContext;
    FGLInitialized: Boolean;
    procedure CreateHandle;
    procedure DestroyHandle;
    {$if defined(LINUX)}
    { Get XWindow handle (to pass to EGL) from this control. }
    function XWindowHandle: Pointer;
    {$endif}
    procedure AdjustContext(const PlatformContext: TGLContext);
    procedure CreateContext;
    procedure DestroyContext;
  protected
    function DefinePresentationName: String; override;
  public
    OnPaint: TNotifyEvent;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;

    procedure Invalidate; virtual;
    procedure SwapBuffers;
    procedure MakeCurrent;

    { Context required parameters. }
    property Requirements: TGLContextRequirements read FRequirements;

    { If Handle not allocated yet, allocate it now.
      This makes sure we have OpenGL context created.
      Our OpenBackend must guarantee it, we want to initialize GLVersion
      afterwards etc. }
    procedure HandleNeeded;

    { This control must always have "native style", which means
      it has ControlType = Platform. See FMX docs about native controls:
      https://docwiki.embarcadero.com/RADStudio/Sydney/en/FireMonkey_Native_Windows_Controls
      Native controls are always on top of non-native controls. }
    property ControlType default TControlType.Platform;
  end;

implementation

uses FMX.Presentation.Factory, Types, FMX.Graphics,
  FMX.Forms, // TODO should not be needed
  CastleLog, CastleUtils;

{$ifdef MSWINDOWS}

{ TWinNativeOpenGLControl -------------------------------------------------------- }

type
  { Presentation for TOpenGLControl.
    This class is necessary to manage WinAPI HWND associated with FMX control. }
  TWinNativeOpenGLControl = class(TWinPresentation)
  protected
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
  public
    function OpenGLControl: TOpenGLControl;
  end;

function TWinNativeOpenGLControl.OpenGLControl: TOpenGLControl;
begin
  Result := Control as TOpenGLControl;
end;

procedure TWinNativeOpenGLControl.CreateHandle;
begin
  inherited;
  { Looking at TWinNativeMemo.CreateHandle confirms this can be called with Handle = null }
  if Handle <> NullHWnd then
    OpenGLControl.CreateHandle;
end;

procedure TWinNativeOpenGLControl.DestroyHandle;
begin
  if Handle <> NullHWnd then
    OpenGLControl.DestroyHandle;
  inherited;
end;

{$endif}

{ TOpenGLControl ------------------------------------------------------------ }

{$if defined(LINUX)}
{ Following FMXLinux sample (GtkWindow) }
function gtk_widget_get_window(widget: Pointer): Pointer; cdecl; external 'libgtk-3.so.0';
function gdk_x11_window_get_xid(widget: Pointer): Pointer; cdecl; external 'libgdk-3.so.0';

function TOpenGLControl.XWindowHandle: Pointer;
var
  GtkWnd, GdkWnd: Pointer;
  Form: TCustomForm;
  LinuxHandle: TLinuxWindowHandle;
begin
  { TODO: This is a hack to require a form as direct Parent,
    to get handle from form... }

  if Parent = nil then
    raise Exception.Create('Parent of TOpenGLControl must be set');
  // This actually also tests Parent <> nil, but previous check makes better error message
  if not (Parent is TCustomForm) then
    raise Exception.Create('Parent of TOpenGLControl must be form');
  Form := Parent as TCustomForm;

  LinuxHandle := TLinuxWindowHandle(Form.Handle);
  if LinuxHandle = nil then
    raise Exception.Create('Form of TOpenGLControl does not have TLinuxHandle initialized yet');

  GtkWnd := LinuxHandle.NativeHandle;
  if GtkWnd = nil then
    raise Exception.Create('Form of TOpenGLControl does not have GTK handle initialized yet');

  GdkWnd := gtk_widget_get_window(GtkWnd);
  if GdkWnd = nil then
    raise Exception.Create('Form of TOpenGLControl does not have GDK handle initialized yet');

  Result := gdk_x11_window_get_xid(GdkWnd);
  if Result = nil then
    raise Exception.Create('Form of TOpenGLControl does not have X11 handle initialized yet');
end;
{$endif}

procedure TOpenGLControl.AdjustContext(const PlatformContext: TGLContext);
{$if defined(MSWINDOWS)}
var
  WinContext: TGLContextWgl;
begin
  inherited;
  WinContext := PlatformContext as TGLContextWgl;
  WinContext.WndPtr :=
    (Presentation as TWinNativeOpenGLControl).Handle;
  if WinContext.WndPtr = 0 then
    raise Exception.Create('Native handle not ready when calling TOpenGLControl.AdjustContext');
  WinContext.h_Dc := GetWindowDC(WinContext.WndPtr);
{$elseif defined(LINUX)}
var
  EglContext: TGLContextEgl;
begin
  inherited;
  EglContext := PlatformContext as TGLContextEgl;
  EglContext.WndPtr := XWindowHandle;
  if EglContext.WndPtr = nil then
    raise Exception.Create('Native handle not ready when calling TOpenGLControl.AdjustContext');
{$else}
end;
{$endif}
end;

procedure TOpenGLControl.CreateContext;
begin
  if not FGLInitialized then
  begin
    FGLInitialized := true;
    // TODO: implement sharing of OpenGL contexts in this case
    // In CGE, all open contexts should share GL resources
    // FPlatformContext.SharedContext := AnyOtherOpenContext;
    AdjustContext(FPlatformContext);
    FPlatformContext.ContextCreate(FRequirements);
    // Invalidate; // would be too early, CASTLE_WINDOW_FORM will do it later
  end;
end;

procedure TOpenGLControl.DestroyContext;
begin
  if FGLInitialized then
  begin
    FGLInitialized := false;
  end;
  inherited;
end;

constructor TOpenGLControl.Create(AOwner: TComponent);
begin
  inherited;

  FRequirements := TGLContextRequirements.Create(Self);
  FRequirements.Name := 'Requirements';
  FRequirements.SetSubComponent(true);

  FPlatformContext :=
    {$if defined(MSWINDOWS)} TGLContextWgl.Create
    {$elseif defined(LINUX)} TGLContextEgl.Create
    {$else}
      {$message fatal 'Define how to create OpenGL context for this platform.'}
    {$endif}
  ;

  { In FMX, this causes adding WS_TABSTOP to Params.Style
    in TWinPresentation.CreateParams. So it is more efficient to call
    before we actually create window by setting ControlType. }
  TabStop := true;

  CanFocus := True;

  { Makes the Presentation be TWinNativeOpenGLControl, which has HWND.
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
      Overriding TWinNativeOpenGLControl.CreateParams to

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

destructor TOpenGLControl.Destroy;
begin
  inherited;
end;

procedure TOpenGLControl.CreateHandle;
begin
  { Do not create context at design-time.
    We don't even set "ControlType := TControlType.Platform" at design-time now
    (see constructor comments),
    this line only secures in case user would set ControlType in a TOpenGLControl
    descendant at design-time. }
  if csDesigning in ComponentState then
    Exit;

  { Thanks to TWinNativeOpenGLControl, we have Windows HWND for this control now in
      (Presentation as TWinNativeOpenGLControl).Handle
    This is used in AdjustContext and
    is necessary to create OpenGL context that only renders to this control.

    Note: The only other way in FMX to get HWND seems to be to get form HWND,
      WindowHandleToPlatform(Handle).Wnd
    but this is not useful for us (we don't want to always render to full window).
  }
  CreateContext;
end;

procedure TOpenGLControl.DestroyHandle;
begin
  DestroyContext;
end;

procedure TOpenGLControl.Paint;
begin
  // We must have OpenGL context at this point,
  // and on Delphi/Linux there seems no way to register "on native handle creation".
  // TODO: We should make sure we get handle before some other events,
  // like update or mouse/key press.
  HandleNeeded;

  if Assigned(OnPaint) then
    OnPaint(Self);

  // inherited not needed, and possibly causes something unnecessary
end;

procedure TOpenGLControl.HandleNeeded;
{$ifdef MSWINDOWS}
var
  H: HWND;
begin
  if Presentation = nil then
    raise EInternalError.Create('TOpenGLControl: Cannot use InternalHandleNeeded as Presentation not created yet');
  H := (Presentation as TWinPresentation).Handle;
  if H = 0 { NullHWnd } then
    raise Exception.Create('TOpenGLControl: InternalHandleNeeded failed to create a handle');
{$elseif defined(LINUX)}
begin
  { There seems no way to create a handle for something else
    than entire TCastleForm on FMXLinux.
    So here we just initialize the context immediately
    and we will work with that (TODO: well that's vague hope :) ) }
  CreateHandle;
  // TODO: Where to call DestroyHandle
{$else}
begin
  // Make sure we have a handle, and OpenGL context, on other platforms
{$endif}
end;

function TOpenGLControl.DefinePresentationName: String;
begin
  Result := 'OpenGLControl-' + GetPresentationSuffix;
end;

procedure TOpenGLControl.Invalidate;
begin
  InvalidateRect(TRectF.Create(0, 0, Width, Height));
end;

procedure TOpenGLControl.SwapBuffers;
begin
  FPlatformContext.SwapBuffers;
end;

procedure TOpenGLControl.MakeCurrent;
begin
  FPlatformContext.MakeCurrent;
end;

{$ifdef MSWINDOWS}
initialization
  { Make TWinNativeOpenGLControl used
    for TOpenGLControl with ControlType = TControlType.Platform. }
  TPresentationProxyFactory.Current.Register(TOpenGLControl, TControlType.Platform, TWinPresentationProxy<TWinNativeOpenGLControl>);
finalization
  TPresentationProxyFactory.Current.Unregister(TOpenGLControl, TControlType.Platform, TWinPresentationProxy<TWinNativeOpenGLControl>);
{$endif}
end.
