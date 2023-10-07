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

{ Utilities specific to FMX in Castle Game Engine.
  This allows sharing of solutions between FMX TOpenGLControl and FMX TCastleControl. }
unit CastleInternalFmxUtils;

interface

uses FMX.Controls, FMX.Controls.Presentation, FMX.Types, UITypes,
  {$ifdef MSWINDOWS} FMX.Presentation.Win, {$endif}
  {$ifdef LINUX} FMX.Platform.Linux, {$endif}
  CastleInternalContextBase;

type
  THandleCreatedEvent = procedure of object;

  { Utility for FMX controls to help them initialize OpenGL context.

    This tries to abstract as much as possible the platform-specific ways
    how to get OpenGL context (and native handle) on a given control,
    in a way that is useful for both FMX TOpenGLControl and FMX TCastleControl.

    The current state:

    - On Windows: make sure native Windows handle is initialized when necessary,
      and pass it to TGLContextWgl.
    - On Linux: we have to create our own Gtk widget (since FMXLinux only ever
      creates native handle for the whole form, it seems).
      And insert it into FMX form, keeping the existing FMX drawing area too.
      And then use TGLContextEgl to connect to our own Gtk widget.

    Note: We could not make TCastleControl descend from TOpenGLControl on FMX
    (like we did on LCL), since the GL work of TCastleControl is partially
    done by the container (to be shared, in turn,
    with VCL and in future LCL implementations).
    So to have enough flexibility how to organize hierarchy, this is rather
    a separate class that is just created and used by both
    FMX TOpenGLControl and FMX TCastleControl. }
  TFmxOpenGLUtility = class
  private
    GLAreaInitialized: Boolean;
    GLAreaGtk: Pointer;
  public
    { Set before calling HandleNeeded.
      Cannot change during lifetime of this instance, for now. }
    Control: TPresentedControl;

    { Called, if assigned, but only on platforms where
      FMX Presentation is not available.
      This also implies it is called only on platforms
      where our code creates the native handle we need,
      e.g. Gtk handle on Linux.

      This means this is called now only on Delphi/Linux.

      On Delphi/Windows, use FMX Presentation features
      instead of register notifications when handle is created/
      destroyed. }
    OnHandleCreatedEvent: THandleCreatedEvent;

    { Make sure that Control has initialized internal Handle.

      - On some platforms, like Windows,
        creating a handle should provoke ContextAdjustEarly
        and TGLContext.ContextCreate.
        The caller should make it happen: see
        TPresentationProxyFactory.Current.Register and our presentation classes.
        This works nicely when FMX platform defines
        Presentation and classes like TWinPresentation.

      - On other platforms, like Linux, we create handle ourselves.
        Manually make sure context is created
        after handle is obtained.
        Needed for Delphi/Linux that doesn't define any "presentation"
        stuff and only creates a handle for the entire form. }
    procedure HandleNeeded;

    { Adjust TGLContext parameters before calling TGLContext.CreateContext.

      Extracts platform-specific bits from given FMX Control,
      and puts in platform-specific bits of TGLContext descendants.
      This is used by TCastleControl and TOpenGLControl.
      It does quite low-level and platform-specific job, dictated
      by the necessity of how FMX works, to be able to get context.

      This is synchronized with what ContextCreateBestInstance does on this platform. }
    procedure ContextAdjustEarly(const PlatformContext: TGLContext);
  end;

implementation

uses
  {$ifdef MSWINDOWS} Windows, {$endif}
  SysUtils,
  { Needed for TCustomForm used by XWindowHandle }
  FMX.Forms,
  CTypes,
  {$ifdef MSWINDOWS} CastleInternalContextWgl, {$endif}
  {$ifdef LINUX} CastleInternalContextEgl, {$endif}
  CastleLog, CastleUtils;

{$if defined(LINUX)}
  {$I castleinternalfmxutils_gtk3.inc}
{$endif}

procedure TFmxOpenGLUtility.ContextAdjustEarly(const PlatformContext: TGLContext);
{$if defined(MSWINDOWS)}
var
  WinContext: TGLContextWgl;
begin
  WinContext := PlatformContext as TGLContextWgl;
  WinContext.WndPtr :=
    (Control.Presentation as TWinPresentation).Handle;
  if WinContext.WndPtr = 0 then
    raise Exception.Create('Native handle not ready when calling ContextAdjustEarly');
  WinContext.h_Dc := GetWindowDC(WinContext.WndPtr);
end;
{$elseif defined(LINUX)}

  { Get XWindow handle (to pass to EGL) from GTK widget. }
  function XHandleFromGtkWidget(const GtkWnd: Pointer): Pointer;
  var
    GdkWnd: Pointer;
  begin
    GdkWnd := gtk_widget_get_window(GtkWnd);
    if GdkWnd = nil then
      raise Exception.Create('Widget does not have GDK handle initialized yet');

    Result := gdk_x11_window_get_xid(GdkWnd);
    if Result = nil then
      raise Exception.Create('Widget does not have X11 handle initialized yet');
  end;

var
  EglContext: TGLContextEgl;
  XHandle: Pointer;
begin
  if GLAreaGtk = nil then
    raise Exception.Create('Native GTK area not ready when calling ContextAdjustEarly');
  XHandle := XHandleFromGtkWidget(GLAreaGtk);
  EglContext := PlatformContext as TGLContextEgl;
  EglContext.WndPtr := XHandle;
  Assert(EglContext.WndPtr <> nil); // XHandleFromGtkWidget already checks this and made exception if problem
end;
{$else}
begin
end;
{$endif}

{ TFmxOpenGLUtility ------------------------------------------------------------- }

procedure TFmxOpenGLUtility.HandleNeeded;
{$if defined(MSWINDOWS)}
var
  H: HWND;
begin
  if Control.Presentation = nil then
    raise EInternalError.CreateFmt('%s: Cannot use ControlHandleNeeded as Presentation not created yet', [Control.ClassName]);
  H := (Control.Presentation as TWinPresentation).Handle;
  if H = 0 { NullHWnd } then
    raise Exception.CreateFmt('%s: ControlHandleNeeded failed to create a handle', [Control.ClassName]);
end;

{$elseif defined(LINUX)}

var
  Form: TCustomForm;
  LinuxHandle: TLinuxWindowHandle;
  DrawingAreaParent, DrawingAreaParentAsFixed: Pointer;
  DrawingAreaParentClassName: AnsiString;
begin
  { Use GLAreaInitialized, instead of check GLAreaGtk <> nil,
    to avoid repeating this when some check below raises exception
    and GLAreaGtk would always remain nil. }
  if GLAreaInitialized then
    Exit;
  GLAreaInitialized := true;

  { TODO: For TCastleControl, it is bad this
    assumes TCastleControl has form as direct parent.
    We could extract form somehow better, check Parent recursively maybe?
  }
  if Control.Parent = nil then
    raise Exception.CreateFmt('Parent of %s must be set', [Control.ClassName]);
  // This actually also tests Parent <> nil, but previous check makes better error message
  if not (Control.Parent is TCustomForm) then
    raise Exception.CreateFmt('Parent of %s must be form', [Control.ClassName]);
  Form := Control.Parent as TCustomForm;

  LinuxHandle := TLinuxWindowHandle(Form.Handle);
  if LinuxHandle = nil then
    raise Exception.CreateFmt('Form of %s does not have TLinuxHandle initialized yet', [Control.ClassName]);

  if LinuxHandle.NativeHandle = nil then
    raise Exception.CreateFmt('Form of %s does not have GTK NativeHandle initialized yet', [Control.ClassName]);

  if LinuxHandle.NativeDrawingArea = nil then
    raise Exception.CreateFmt('Form of %s does not have GTK NativeDrawingArea initialized yet', [Control.ClassName]);

  { Tests show that parent of is GtkFixed,
    this makes things easy for us to insert GLAreaGtk later. }
  DrawingAreaParent := gtk_widget_get_parent(LinuxHandle.NativeDrawingArea);
  if DrawingAreaParent = nil then
    raise Exception.Create('FMX drawing area in GTK has no parent');

  DrawingAreaParentClassName := G_OBJECT_TYPE_NAME(DrawingAreaParent);
  if DrawingAreaParentClassName <> 'GtkFixed' then
    WritelnWarning('FMX drawing area has parent with unexpected class "%s". We will try to continue it and cast it to GtkFixed', [
      DrawingAreaParentClassName
    ]);

  DrawingAreaParentAsFixed := g_type_check_instance_cast(DrawingAreaParent, gtk_fixed_get_type);

  { Initialization of variables and checks are done.
    Now actually create GLAreaGtk, if things look sensible. }

  GLAreaGtk := gtk_drawing_area_new;
  // Do this using gtk_fixed_put instead:
  //gtk_container_add(DrawingAreaParentAsFixed, GLAreaGtk);
  gtk_fixed_put(DrawingAreaParentAsFixed, GLAreaGtk,
    Round(Control.Position.X), Round(Control.Position.Y));
  gtk_widget_set_size_request(GLAreaGtk,
    Round(Control.Size.Width), Round(Control.Size.Height));
  gtk_widget_show(GLAreaGtk);

  { Debugging what are some Gtk classes, to reverse-engineer what FMXLinux
    is doing inside it's closed library:
  WritelnLog('LinuxHandle.NativeHandle type ' + G_OBJECT_TYPE_NAME(LinuxHandle.NativeHandle));
  WritelnLog('LinuxHandle.NativeDrawingArea type ' + G_OBJECT_TYPE_NAME(LinuxHandle.NativeDrawingArea));
  WritelnLog('GLAreaGtk type ' + G_OBJECT_TYPE_NAME(GLAreaGtk));
  }

  if Assigned(OnHandleCreatedEvent) then
    OnHandleCreatedEvent();
end;

{$else}
begin
  // Nothing to do on other platforms by default
end;

{$endif}

end.
