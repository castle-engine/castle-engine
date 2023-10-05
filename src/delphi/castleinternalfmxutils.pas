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

{ Adjust TGLContext parameters before calling TGLContext.CreateContext.

  Extracts platform-specific bits from given FMX control,
  and puts in platform-specific bits of TGLContext descendants.
  This is used by TCastleControl and TOpenGLControl.
  It does quite low-level and platform-specific job, dictated
  by the necessity of how FMX works, to be able to get context.

  This is synchronized with what ContextCreateBestInstance does on this platform. }
procedure ContextAdjustEarly(const Control: TPresentedControl;
  const PlatformContext: TGLContext);

{ Make sure that Control has initialized internal Handle.

  On platforms where Control may have a native handle, like Windows,
  creating a handle should provoke ContextAdjustEarly and TGLContext.ContextCreate.
  On other platforms, ContextHandleNeded cannot do anything. }
procedure ControlHandleNeeded(const Control: TPresentedControl);

implementation

uses
  {$ifdef MSWINDOWS} Windows, {$endif}
  SysUtils,
  { Needed for TCustomForm used by XWindowHandle }
  FMX.Forms,
  {$ifdef MSWINDOWS} CastleInternalContextWgl, {$endif}
  {$ifdef LINUX} CastleInternalContextEgl, {$endif}
  CastleLog, CastleUtils;

{$if defined(LINUX)}
{ Following FMXLinux sample (GtkWindow) }
function gtk_widget_get_window(widget: Pointer): Pointer; cdecl; external 'libgtk-3.so.0';
function gdk_x11_window_get_xid(widget: Pointer): Pointer; cdecl; external 'libgdk-3.so.0';

{ Get XWindow handle (to pass to EGL) from this control. }
function XWindowHandle(const Control: TPresentedControl): Pointer;
var
  GtkWnd, GdkWnd: Pointer;
  Form: TCustomForm;
  LinuxHandle: TLinuxWindowHandle;
begin
  { TODO: For TCastleControl, it is bad this:
    - assumes TCastleControl has form as direct parent
    - creates context for whole form
  }

  if Parent = nil then
    raise Exception.CreateFmt('Parent of %s must be set', [Control.ClassName]);
  // This actually also tests Parent <> nil, but previous check makes better error message
  if not (Parent is TCustomForm) then
    raise Exception.CreateFmt('Parent of %s must be form', [Control.ClassName]);
  Form := Parent as TCustomForm;

  LinuxHandle := TLinuxWindowHandle(Form.Handle);
  if LinuxHandle = nil then
    raise Exception.CreateFmt('Form of %s does not have TLinuxHandle initialized yet', [Control.ClassName]);

  GtkWnd := LinuxHandle.NativeHandle;
  if GtkWnd = nil then
    raise Exception.CreateFmt('Form of %s does not have GTK handle initialized yet', [Control.ClassName]);

  GdkWnd := gtk_widget_get_window(GtkWnd);
  if GdkWnd = nil then
    raise Exception.CreateFmt('Form of %s does not have GDK handle initialized yet', [Control.ClassName]);

  Result := gdk_x11_window_get_xid(GdkWnd);
  if Result = nil then
    raise Exception.CreateFmt('Form of %s does not have X11 handle initialized yet', [Control.ClassName]);
end;
{$endif}

procedure ContextAdjustEarly(const Control: TPresentedControl;
  const PlatformContext: TGLContext);
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
var
  EglContext: TGLContextEgl;
begin
  inherited;
  EglContext := PlatformContext as TGLContextEgl;
  EglContext.WndPtr := XWindowHandle(Control);
  Assert(EglContext.WndPtr <> nil); // XWindowHandle already checks this and made exception if problem
end;
{$else}
begin
end;
{$endif}

procedure ControlHandleNeeded(const Control: TPresentedControl);
{$ifdef MSWINDOWS}
var
  H: HWND;
begin
  if Control.Presentation = nil then
    raise EInternalError.CreateFmt('%s: Cannot use ControlHandleNeeded as Presentation not created yet', [Control.ClassName]);
  H := (Control.Presentation as TWinPresentation).Handle;
  if H = 0 { NullHWnd } then
    raise Exception.CreateFmt('%s: ControlHandleNeeded failed to create a handle', [Control.ClassName]);
end;
{$else}
begin
end;
{$endif}

end.
