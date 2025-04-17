{%MainUnit castleinternalfmxutils.pas}
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

{ Implementation of CastleInternalFmxUtils (initialize OpenGL context in FMX,
  code shared between FMX TOpenGLControl and FMX TCastleControl)
  for Linux. }

uses
  SysUtils,
  { Needed for TCustomForm used by XWindowHandle }
  FMX.Forms,
  CTypes,
  CastleInternalContextEgl, CastleLog, CastleUtils;

{$I castleinternalfmxutils_gtk3.inc}

{ internal routines --------------------------------------------------------- }

procedure PassSignalToOtherWidget(const SignalName: AnsiString;
  const OtherWidget: Pointer; const Event: Pointer);
var
  SignalEmitArgs: array[0..2] of TGValue;
  ReturnValue: TGValue;
  SignalInt: CUInt;
begin
  SignalInt := g_signal_lookup(PAnsiChar(SignalName), G_TYPE_FROM_INSTANCE(OtherWidget));
  if SignalInt = 0 then
    raise Exception.CreateFmt('Signal "%s" on target not found', [SignalName]);

  { Initialize GValue list.
    - 1st item must be instance to which we send signal,
    - then params of that signal (just Event),
    - then finish (leave uinitialized GValue,
      "uinitialized" == filled with zeroes for glib).

    Note that we have to fill initially values with zeroes, otherwise
    it could be mistaken with initialized value with some garbage and cause
    valid glib warnings/errors. }
  FillChar(SignalEmitArgs, SizeOf(SignalEmitArgs), 0);
  g_value_init(@SignalEmitArgs[0], G_TYPE_OBJECT);
  g_value_set_instance(@SignalEmitArgs[0], OtherWidget);
  g_value_init(@SignalEmitArgs[1], G_TYPE_POINTER);
  g_value_set_pointer(@SignalEmitArgs[1], Event);

  { Initialie ReturnValue (GValue). Despite docs,
    g_signal_emitv cannot get nil as return value. }
  FillChar(ReturnValue, SizeOf(ReturnValue), 0);
  g_value_init(@ReturnValue, G_TYPE_BOOLEAN);
  g_value_set_boolean(@ReturnValue, false);

  g_signal_emitv(@SignalEmitArgs, SignalInt, 0, @ReturnValue);

  g_value_unset(@SignalEmitArgs[0]);
  g_value_unset(@SignalEmitArgs[1]);
  g_value_unset(@ReturnValue);
end;

function signal_button_press_event(AGLAreaGtk: Pointer; Event: PGdkEventAny;
  Data: Pointer): gboolean; cdecl;
begin
  { Pass the signal to FMX control,
    which will make FMX callbacks,
    and then TCastleWindow (in case of CASTLE_WINDOW_FORM)
    or TCastleControl will handle the event from FMX. }
  PassSignalToOtherWidget('button_press_event', Data, Event);
  Result := false;
end;

function signal_button_release_event(AGLAreaGtk: Pointer; Event: PGdkEventAny;
  Data: Pointer): gboolean; cdecl;
begin
  PassSignalToOtherWidget('button_release_event', Data, Event);
  Result := false;
end;

function signal_motion_notify_event(AGLAreaGtk: Pointer; Event: PGdkEventAny;
  Data: Pointer): gboolean; cdecl;
begin
  { We need own motion_notify_event otherwise events do not reach
    FMX after you clicked (when mouse events are grabbed),
    regardless of button_press_event return value. }
  PassSignalToOtherWidget('motion_notify_event', Data, Event);
  Result := false;
end;

function signal_scroll_event(AGLAreaGtk: Pointer; Event: PGdkEventAny;
  Data: Pointer): gboolean; cdecl;
begin
  PassSignalToOtherWidget('scroll_event', Data, Event);
  Result := false;
end;

{ TFmxOpenGLUtility ---------------------------------------------------------- }

(*
// Note: This wasn't needed in the end, so we never finished or tested it.
// Keeping it here -- as it may make sense again.

function TFmxOpenGLUtility.HandlePossible: Boolean;
var
  LinuxFormHandle: TLinuxWindowHandle;
begin
  Result := false;

  if (Control.Root <> nil) and
     (Control.Root is TCustomForm) then
  begin
    LinuxFormHandle := TLinuxWindowHandle(TCustomForm(Control.Root).Handle);
    if LinuxFormHandle <> nil then
      Exit(true);
  end;
end;
*)

procedure TFmxOpenGLUtility.ContextAdjustEarly(const PlatformContext: TGLContext);

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

{ If @true, there's a reasonable chance that everything necessary
  for GetLinuxHandle is already initialized. }
function GetLinuxHandlePossible(const Control: TPresentedControl): Boolean;
begin
  Result :=
    (Control <> nil) and
    (Control.Root is TCustomForm) and // also checks Control.Root <> nil
    (TCustomForm(Control.Root).Handle <> nil);
end;

function GetLinuxHandle(const Control: TPresentedControl): TLinuxWindowHandle;
var
  Form: TCustomForm;
begin
  if Control.Root = nil then
    raise Exception.CreateFmt('Control %s does not have root', [Control.ClassName]);
  if not (Control.Root is TCustomForm) then
    raise Exception.CreateFmt('Control %s root is not TCastleForm, but is %s', [
      Control.ClassName,
      (Control.Root as TObject).ClassName
    ]);
  Form := Control.Root as TCustomForm;

  (*
  Old version, assuming Control.Parent is form (which is only sometimes true):
  if Control.Parent = nil then
    raise Exception.CreateFmt('Parent of %s must be set', [Control.ClassName]);
  // This actually also tests Parent <> nil, but previous check makes better error message
  if not (Control.Parent is TCustomForm) then
    raise Exception.CreateFmt('Parent of %s must be form', [Control.ClassName]);
  Form := Control.Parent as TCustomForm;
  *)

  Result := TLinuxWindowHandle(Form.Handle);
  if Result = nil then
    raise Exception.CreateFmt('Form of %s does not have TLinuxHandle initialized yet', [Control.ClassName]);
end;

function GetControlRect(const Control: TPresentedControl; const Scale: Single): TVector4Integer;
begin
  Result := Vector4Integer(
    Round(Control.Position.X * Scale),
    Round(Control.Position.Y * Scale),
    Round(Control.Size.Width * Scale),
    Round(Control.Size.Height * Scale)
  );
end;

procedure TFmxOpenGLUtility.HandleNeeded;
var
  LinuxHandle: TLinuxWindowHandle;
  DrawingAreaParentAsFixed: Pointer;
  DrawingAreaParentClassName: AnsiString;
begin
  { Use GLAreaInitialized, instead of check GLAreaGtk <> nil,
    to avoid repeating this when some check below raises exception
    and GLAreaGtk would always remain nil. }
  if GLAreaInitialized then
    Exit;
  GLAreaInitialized := true;

  WritelnLog('FMX OpenGL', 'Creating context and GLAreaGtk widget');

  LinuxHandle := GetLinuxHandle(Control);

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

  { Note: We tested alternative solution
      GLAreaGtk := LinuxHandle.NativeDrawingArea;
    which seems to make sense when GLAreaGtk should just fill the whole window,
    like for TCastleWindow. But it causes blinking. }

  { Initialization of variables and checks are done.
    Now actually create GLAreaGtk, if things look sensible. }

  GLAreaGtk := gtk_drawing_area_new;

  { Add a refcount to GLAreaGtk, and later HandleRelease will do g_object_unref.

    This way HandleRelease is more robust: in case the GTK window of the form
    was already destroyed at the moment of HandleRelease call,
    the GLAreaGtk remains valid (it would be invalid
    without this g_object_ref call). So we can call gtk_widget_destroy
    safely on it.

    Otherwise, gtk_widget_destroy(GLAreaGtk) in HandleRelease could cause
    GTK errors, that argument is not a widget, sometimes: when the form
    was already freed.

    Testcase: CastleFmx with explicit "FreeAndNil(TestCgeControl)" in main program file. }
  g_object_ref(GLAreaGtk);

  { connect signal handlers to GLAreaGtk }
  { What events to catch ? It must cover all signal_yyy_event functions that we
    will connect. This must be called before X Window is created. }
  gtk_widget_set_events(GLAreaGtk,
    // GDK_EXPOSURE_MASK {for expose_event} or
    GDK_BUTTON_PRESS_MASK {for button_press_event} or
    GDK_BUTTON_RELEASE_MASK {for button_release_event} or
    GDK_POINTER_MOTION_MASK {for motion_notify_event}
  );
  g_signal_connect(GLAreaGtk, 'button_press_event', @signal_button_press_event,
    LinuxHandle.NativeDrawingArea);
  g_signal_connect(GLAreaGtk, 'button_release_event', @signal_button_release_event,
    LinuxHandle.NativeDrawingArea);
  g_signal_connect(GLAreaGtk, 'motion_notify_event', @signal_motion_notify_event,
    LinuxHandle.NativeDrawingArea);
  g_signal_connect(GLAreaGtk, 'scroll_event', @signal_scroll_event,
    LinuxHandle.NativeDrawingArea);

  // Do this using gtk_fixed_put instead:
  //gtk_container_add(DrawingAreaParentAsFixed, GLAreaGtk);
  GLAreaRect := GetControlRect(Control, LinuxHandle.Scale);
  gtk_fixed_put(DrawingAreaParentAsFixed, GLAreaGtk, GLAreaRect.X, GLAreaRect.Y);
  gtk_widget_set_size_request(GLAreaGtk, GLAreaRect.Z, GLAreaRect.W);
  gtk_widget_show(GLAreaGtk);

  { Debugging what are some Gtk classes, to reverse-engineer what FMXLinux
    is doing inside it's closed library:
  WritelnLog('LinuxHandle.NativeHandle type ' + G_OBJECT_TYPE_NAME(LinuxHandle.NativeHandle));
  WritelnLog('LinuxHandle.NativeDrawingArea type ' + G_OBJECT_TYPE_NAME(LinuxHandle.NativeDrawingArea));
  WritelnLog('GLAreaGtk type ' + G_OBJECT_TYPE_NAME(GLAreaGtk));
  }

  if Assigned(OnHandleAfterCreateEvent) then
    OnHandleAfterCreateEvent();
end;

procedure TFmxOpenGLUtility.Update;
var
  NewRect: TVector4Integer;
begin
  // abort if HandleNeeded not yet called (or not successful)
  if GLAreaGtk = nil then
    Exit;

  NewRect := GetControlRect(Control, Scale);
  if not TVector4Integer.Equals(NewRect, GLAreaRect) then
  begin
    gtk_fixed_move(DrawingAreaParent, GLAreaGtk, NewRect.X, NewRect.Y);
    gtk_widget_set_size_request(GLAreaGtk, NewRect.Z, NewRect.W);
    GLAreaRect := NewRect;
  end;
end;

procedure TFmxOpenGLUtility.HandleRelease;
begin
  if GLAreaGtk <> nil then
  begin
    WritelnLog('FMX OpenGL', 'Destroying context and GLAreaGtk widget');
    if Assigned(OnHandleBeforeDestroyEvent) then
      OnHandleBeforeDestroyEvent();
    gtk_widget_destroy(GLAreaGtk);
    g_object_unref(GLAreaGtk);

    GLAreaGtk := nil;
    GLAreaInitialized := false;
    DrawingAreaParent := nil;
  end;
end;

function TFmxOpenGLUtility.Scale: Single;
var
  LinuxHandle: TLinuxWindowHandle;
begin
  { Note that we check GetLinuxHandlePossible, not "GLAreaGtk <> nil".
    This way we can get scale before the OpenGL context was initialized.

    This way castlewindow_form.inc code doesn't need to care that
    FGLUtility.Scale may change after form creation,
    and thus PixelWidth/Height could change without causing OnResize event.

    Testcase: multi_window.
  }
  if GetLinuxHandlePossible(Control) then
  begin
    LinuxHandle := GetLinuxHandle(Control);
    Result := LinuxHandle.Scale;
  end else
    Result := 1;
end;
