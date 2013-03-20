{
  Copyright 2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_window_interface}
private
  Form: TForm;
  OpenGLControl: TOpenGLControl;
  Menu: TMainMenu;
  procedure MenuItemClick(Sender: TObject);
  procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  procedure FormDeactivate(Sender: TObject);
  procedure OpenGLControlPaint(Sender: TObject);
  procedure OpenGLControlResize(Sender: TObject);
  procedure OpenGLControlKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
  procedure OpenGLControlKeyUp(Sender: TObject; var Key: Word;
    Shift: TShiftState);
  procedure OpenGLControlMouseDown(Sender: TObject; Button: Controls.TMouseButton;
    Shift: TShiftState; X, Y: Integer);
  procedure OpenGLControlMouseUp(Sender: TObject; Button: Controls.TMouseButton;
    Shift: TShiftState; X, Y: Integer);
  procedure OpenGLControlMouseMove(Sender: TObject; Shift: TShiftState; X,
    Y: Integer);
  procedure OpenGLControlMouseWheel(Sender: TObject; Shift: TShiftState;
    WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
{$endif read_window_interface}

{$ifdef read_application_interface}
private
  LastDoTimerTime: TMilisecTime;
  procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
{$endif read_application_interface}

{$ifdef read_implementation}

{ TODO:

  Bugs on GTK2, not on Carbon:
  - All radio menu items are always on. Why?
  - Keys do not work, like arrows or Home. They do work on Carbon, why fail on GTK2?

  Bugs on Carbon and GTK2:
  - MouseLook stutters. Something similar to aggressive update needed?
  - (untested on GTK2, confirmed on Carbon)
    MainMenu.Enabled doesn't work,
    e.g. when you do command+r menu is not disabled in view3dscene.
  - (untested on GTK2, confirmed on Carbon)
    Hiding cursor doesn't work (e.g. in MouseLook)

  Bugs on Carbon, not on GTK2:
  - When TCastleWindowBase.MenuUpdateCheckedFast is false,
    changing some radio menus (like view3dscene display->4 viewports)
    make segfaults in Carbon widgetset, like our menu items were invalid.
    Now MenuUpdateCheckedFast is true, but maybe we have some related errors elsewhere
    when rebuilding menu?
  - When opening many times new files using view3dscene open, also Carbon segfaults.
    Again the above bug?
  - Using command+o to open file in view3dscene shows open dialog twice.
    Same when using command+r, which shows our own MessageXxx.

  Missing:
  - TCastleWindowBase.AccumBits: not possible to express in TOpenGLControl now
}

{ TCastleWindowBase ------------------------------------------------------------------ }

procedure TCastleWindowBase.CreateBackend;
begin
end;

procedure TCastleWindowBase.BackendMenuInitialize;

  function MenuShortcutCastleToLCL(const Key: TKey; const CharKey: char): TShortCut;
  var
    LazKey: Word;
    Shift: TShiftState;
  begin
    KeyCastleToLCL(Key, CharKey, LazKey, Shift);
    Result := KeyToShortCut(LazKey, Shift);
  end;

  procedure MenuCastleToLCL(MyMenu: TMenu; Menu: Menus.TMenuItem);
  var
    MenuItem: Menus.TMenuItem;
    MyEntry: TMenuEntry;
    I: Integer;
  begin
    for I := 0 to MyMenu.EntriesCount - 1 do
    begin
      MyEntry := MyMenu.Entries[I];
      MenuItem := Menus.TMenuItem.Create(Form);
      MenuItem.OnClick := @MenuItemClick;
      { store connection both ways between LCL menu item <-> our menu entry }
      MenuItem.Tag := PtrInt(MyEntry);
      MyEntry.Handle := MenuItem;
      if MyEntry is TMenuItem then
        MenuItem.ShortCut := MenuShortcutCastleToLCL(
          TMenuItem(MyEntry).Key, TMenuItem(MyEntry).CharKey);
      if MyEntry is TMenuEntryWithCaption then
      begin
        { set MenuItem.Caption and Enabled }
        MenuUpdateCaption(TMenuEntryWithCaption(MyEntry));
        MenuUpdateEnabled(TMenuEntryWithCaption(MyEntry));
        if MyEntry is TMenuItemChecked then
        begin
          MenuItem.ShowAlwaysCheckable := true;
          MenuItem.RadioItem := MyEntry is TMenuItemRadio;
          MenuUpdateChecked(TMenuItemChecked(MyEntry));
        end;
      end else
      if MyEntry is TMenuSeparator then
        { set MenuItem.Caption - special for separator }
        MenuItem.Caption := '-';
      { if we have submenu, do this recursively }
      if MyEntry is TMenu then
        MenuCastleToLCL(TMenu(MyEntry), MenuItem);
      Menu.Add(MenuItem);
    end;
  end;

begin
  Menu := TMainMenu.Create(Form);
  Menu.Parent := Form;
  Form.Menu := Menu;
  MenuCastleToLCL(MainMenu, Menu.Items);
end;

procedure TCastleWindowBase.BackendMenuFinalize;
begin
  FreeAndNil(Menu);
  MainMenu.ClearHandles;
end;

function CheckMenu(Entry: TMenuEntryWithCaption; out EntryLCL: Menus.TMenuItem): boolean;
begin
  EntryLCL := Menus.TMenuItem(Entry.Handle);
  Result := EntryLCL <> nil;
  if (not Result) and Log then
    WritelnLog('Menu', 'Warning: menu entry "' + Entry.Caption + '" has Handle = nil (this means that corresponding LCL menu item object was not created, but it should)');
end;

procedure TCastleWindowBase.MenuUpdateCaption(Entry: TMenuEntryWithCaption);

  function MenuCaptionCastleToLCL(const S: string): string;
  begin
    { We use _ for the same purpose as LCL uses &, to mark mnemonics.
      But we cannot just replace _ with &:
      Double underscore should be converted to single underscore.
      Only unpaired single underscore should be converted to &.
      Our SReplacePatterns guarantees correct behavior. }
    Result := SReplacePatterns(S, ['__', '_'], ['_', '&'], [soMatchCase]);
  end;

var
  EntryLCL: Menus.TMenuItem;
begin
  if not CheckMenu(Entry, EntryLCL) then Exit;
  EntryLCL.Caption := MenuCaptionCastleToLCL(Entry.Caption);
end;

procedure TCastleWindowBase.MenuUpdateEnabled(Entry: TMenuEntryWithCaption);
var
  EntryLCL: Menus.TMenuItem;
begin
  if not CheckMenu(Entry, EntryLCL) then Exit;
  EntryLCL.Enabled := Entry.Enabled;
end;

procedure TCastleWindowBase.MenuUpdateChecked(Entry: TMenuItemChecked);
var
  EntryLCL: Menus.TMenuItem;
begin
  if not CheckMenu(Entry, EntryLCL) then Exit;
  EntryLCL.Checked := Entry.Checked;
end;

function TCastleWindowBase.MenuUpdateCheckedFast: boolean;
begin
  Result := true;
end;

procedure TCastleWindowBase.SwapBuffers;
begin
  OpenGLControl.SwapBuffers;
end;

procedure TCastleWindowBase.OpenBackend;
begin
  { A hacky way to create and set our Form as Forms.Application.MainForm.

    We really want our Form to be set as Forms.Application.MainForm,
    as that's the only way to capture (in form close event)
    the Mac OS X "ProjectName -> Quit" automatic menu item (see
    http://bugs.freepascal.org/view.php?id=10983).
    But there is no way to nicely set MainForm, instead we have to use
    mechanism that "the first form created by Application.CreateForm
    is set as main", see http://www.lazarus.freepascal.org/index.php?topic=14438.0
    and LCL sources of TApplication.UpdateMainForm.
    So we have to use Application.CreateForm for this, which in turn means
    we have to use ugly "RequireDerivedFormResource := false" (because our
    form does not have a resource, but Application.CreateForm always looks
    for resource).

    What we would like to do is
      Form := TForm.CreateNew(nil);
      Forms.Application.MainForm := Form;
    but MainForm is not settable. Or have another way to capture Mac OS X
    "ProjectName -> Quit" automatic menu item. }

  RequireDerivedFormResource := false;
  Forms.Application.CreateForm(TForm, Form);

  Form.Top := Top;
  Form.Left := Left;
  { We would prefer to leave Form.AutoSize := true and allow form to adjust
    to OpenGLControl.Width/Height. But it doesn't work. }
  Form.Width := Width;
  Form.Height := Height;
  Form.Caption := Caption; // SetCaption may also change it later
  Form.Cursor := CursorCastleToLCL[Cursor]; // SetCursor may also change it later
  if FullScreen then
    Form.BorderStyle := bsNone else
  if ResizeAllowed <> raAllowed then
    Form.BorderStyle := bsSingle;

  Form.OnCloseQuery := @FormCloseQuery;
  { Note: I tried also using OpenGLControl.OnExit for this, seems to be quivalent
    in our case. And, similarly, both methods still need manual workaround
    to call ReleaseAllKeysAndMouse manually in MessageOK and similar methods
    (otherwise: check e.g. window_events with Window.MessageOk from Idle:
    entering MessageOK *must* make ReleaseAllKeysAndMouse, otherwise it hangs.) }
  Form.OnDeactivate := @FormDeactivate;

  OpenGLControl := TOpenGLControl.Create(Form);
  OpenGLControl.Parent := Form;
  OpenGLControl.Align := alClient;
  OpenGLControl.Width := Width;
  OpenGLControl.Height := Height;
  OpenGLControl.Constraints.MinWidth := MinWidth;
  OpenGLControl.Constraints.MinHeight := MinHeight;
  OpenGLControl.Constraints.MaxWidth := MaxWidth;
  OpenGLControl.Constraints.MaxHeight := MaxHeight;
  OpenGLControl.TabStop := true;

  OpenGLControl.DoubleBuffered := DoubleBuffer;
  OpenGLControl.StencilBits := StencilBits;
  OpenGLControl.DepthBits := DepthBits;
  OpenGLControl.AlphaBits := AlphaBits;
  OpenGLControl.MultiSampling := MultiSampling;

  Form.ActiveControl := OpenGLControl;

  if MainMenu <> nil then MenuInitialize;

  Form.Visible := Visible;

  { pass the actual OpenGLControl sizes (e.g. after accounting for MainMenu size
    under GTK2 widgetset, actually under all widgetsets that do not have global
    menu (like Mac OS X). DoResize is already implemented to handle such
    call (from OpenBackend, before EventOpen was called) correctly. }
  DoResize(OpenGLControl.Width, OpenGLControl.Height, false);

  { Assign OpenGLControl.OnPaint *after* making Form visible.
    If we would assign OpenGLControl.OnPaint earlier, then
    Form.Visible := true would cause OpenGLControlPaint *before*
    OpenGL context was created. }
  OpenGLControl.OnPaint := @OpenGLControlPaint;
  OpenGLControl.OnResize := @OpenGLControlResize;
  OpenGLControl.OnKeyDown := @OpenGLControlKeyDown;
  OpenGLControl.OnKeyUp := @OpenGLControlKeyUp;
  OpenGLControl.OnMouseDown := @OpenGLControlMouseDown;
  OpenGLControl.OnMouseUp := @OpenGLControlMouseUp;
  OpenGLControl.OnMouseMove := @OpenGLControlMouseMove;
  OpenGLControl.OnMouseWheel := @OpenGLControlMouseWheel;

  Application.OpenWindowsAdd(Self);
end;

procedure TCastleWindowBase.CloseBackend;
begin
  FreeAndNil(Form);
  { freeing the Form will automatically free other owned components }
  OpenGLControl := nil;
  Menu := nil;

  { although Menu was already freed, but make sure we also change state,
    like TCastleWindowBase.MenuInitialized and clear TMenuItem.Handle. }
  MenuFinalize;
end;

procedure TCastleWindowBase.PostRedisplay;
begin
  if not Closed then OpenGLControl.Invalidate;
end;

procedure TCastleWindowBase.SetCaption(const Value: string);
begin
  FCaption := Value;
  if not Closed then Form.Caption := Caption;
end;

procedure TCastleWindowBase.BackendMakeCurrent;
begin
  OpenGLControl.MakeCurrent;
end;

procedure TCastleWindowBase.FlushRedisplay;
begin
  { TODO (redraw *now*, if PostRedisplay pending;
    it's valid (although non-optimal) to just always call DoDraw here) }
end;

procedure TCastleWindowBase.SetCursor(const Value: TMouseCursor);
begin
  if FCursor <> Value then
  begin
    FCursor := Value;
    if not Closed then
      Form.Cursor := CursorCastleToLCL[Value];
  end;
end;

procedure TCastleWindowBase.SetCustomCursor(const Value: TRGBAlphaImage);
begin
  { TODO }
  FCustomCursor := Value;
end;

function TCastleWindowBase.RedirectKeyDownToMenuCommand: boolean;
begin
  Result := false;
end;

procedure TCastleWindowBase.SetMousePosition(const NewMouseX, NewMouseY: Integer);
begin
  if not Closed then
    Mouse.CursorPos := OpenGLControl.ControlToScreen(Point(NewMouseX, NewMouseY))
end;

procedure TCastleWindowBase.MenuItemClick(Sender: TObject);
var
  SenderTag: TObject;
begin
  if Sender is Menus.TMenuItem then
  begin
    SenderTag := TObject(Pointer(Menus.TMenuItem(Sender).Tag));
    if SenderTag is TMenuItem then
    begin
      { It seems that checked menu item is always automatically swapped,
        at least with LCL-GTK2. So manually revert it to original setting,
        as we don't want this (DoMenuCommand will change Checked as necessary). }
      if SenderTag is TMenuItemChecked then
        MenuUpdateChecked(TMenuItemChecked(SenderTag));
      DoMenuCommand(TMenuItem(SenderTag));
    end;
  end;
end;

procedure TCastleWindowBase.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  DoCloseQuery;
  CanClose := false; // DoCloseQuery will call Close when needed
end;

procedure TCastleWindowBase.FormDeactivate(Sender: TObject);
begin
  ReleaseAllKeysAndMouse;
end;

procedure TCastleWindowBase.OpenGLControlPaint(Sender: TObject);
begin
  DoDraw;
end;

procedure TCastleWindowBase.OpenGLControlResize(Sender: TObject);
begin
  DoResize(OpenGLControl.Width, OpenGLControl.Height, false);
end;

procedure TCastleWindowBase.OpenGLControlKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  MyKey: TKey;
  MyCharKey: char;
begin
  KeyLCLToCastle(Key, Shift, MyKey, MyCharKey);
  if (MyKey <> K_None) or (MyCharKey <> #0) then
    DoKeyDown(MyKey, MyCharKey);
end;

procedure TCastleWindowBase.OpenGLControlKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  MyKey: TKey;
  MyCharKey: char;
begin
  KeyLCLToCastle(Key, Shift, MyKey, MyCharKey);
  if MyKey <> K_None then
    DoKeyUp(MyKey);
end;

procedure TCastleWindowBase.OpenGLControlMouseDown(Sender: TObject;
  Button: Controls.TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  MyButton: CastleKeysMouse.TMouseButton;
begin
  if MouseButtonLCLToCastle(Button, MyButton) then
    DoMouseDown(X, Y, MyButton);
end;

procedure TCastleWindowBase.OpenGLControlMouseUp(Sender: TObject;
  Button: Controls.TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  MyButton: CastleKeysMouse.TMouseButton;
begin
  if MouseButtonLCLToCastle(Button, MyButton) then
    DoMouseUp(X, Y, MyButton);
end;

procedure TCastleWindowBase.OpenGLControlMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  DoMouseMove(X, Y);
end;

procedure TCastleWindowBase.OpenGLControlMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  DoMouseWheel(WheelDelta/120, true);
end;

{ TCastleWindowBase dialogs ---------------------------------------------------------- }

{ Methods below should make native-looking dialog boxes.
  Alternatively, you can include castlewindow_dialogs_by_messages.inc,
  to have simple dialog boxes drawn on OpenGL context using our GLWinMessages
  unit. }
{ $I castlewindow_dialogs_by_messages.inc}

function TCastleWindowBase.FileDialog(const Title: string; var FileName: string;
  OpenDialog: boolean; FileFilters: TFileFilterList): boolean;
var
  Dialog: TFileDialog;
  ExpandedFileName: string;
begin
  if OpenDialog then
    Dialog := TOpenDialog.Create(nil) else
    Dialog := TSaveDialog.Create(nil);
  try
    Dialog.Title := Title;
    if FileName = '' then
      Dialog.InitialDir := SysToUTF8(GetCurrentDir) else
    begin
      ExpandedFileName := ExpandFileName(FileName);
      Dialog.InitialDir := SysToUTF8(ExtractFilePath(ExpandedFileName));
      Dialog.FileName := SysToUTF8(ExtractFileName(ExpandedFileName));
    end;
    FileFiltersToDialog(FileFilters, Dialog);
    FormDeactivate(Form); // do it manually
    Result := Dialog.Execute;
    if Result then
      FileName := UTF8ToSys(Dialog.FileName);
  finally FreeAndNil(Dialog) end;
end;

function TCastleWindowBase.ColorDialog(var Color: TVector3Single): boolean;
var
  Dialog: TColorDialog;
  ColorByte: TVector3Byte;
begin
  Dialog := TColorDialog.Create(nil);
  try
    ColorByte := Vector3Byte(Color);
    Dialog.Color := RGBToColor(ColorByte[0], ColorByte[1], ColorByte[2]);
    FormDeactivate(Form); // do it manually
    Result := Dialog.Execute;
    if Result then
    begin
      RedGreenBlue(Dialog.Color, ColorByte[0], ColorByte[1], ColorByte[2]);
      Color := Vector3Single(ColorByte);
    end;
  finally FreeAndNil(Dialog) end;
end;

procedure TCastleWindowBase.MessageOK(const S: string; const MessageType: TWindowMessageType);
const
  MessageTypeCastleToLCL: array [TWindowMessageType] of TMsgDlgType =
  (Dialogs.mtInformation, Dialogs.mtWarning, Dialogs.mtConfirmation, Dialogs.mtError, Dialogs.mtCustom);
begin
  FormDeactivate(Form); // do it manually
  MessageDlg(S, MessageTypeCastleToLCL[MessageType], [mbOK], 0);
end;

function TCastleWindowBase.MessageYesNo(const S: string;
  const MessageType: TWindowMessageType): boolean;
begin
  FormDeactivate(Form); // do it manually
  Result := MessageDlg(S, mtConfirmation, mbYesNo, 0) = mrYes;
end;

{ TGLApplication ---------------------------------------------------------- }

procedure TGLApplication.CreateBackend;
begin
  Forms.Application.Initialize;
  Forms.Application.AddOnIdleHandler(@ApplicationIdle);
end;

procedure TGLApplication.DestroyBackend;
begin
  Forms.Application.RemoveOnIdleHandler(@ApplicationIdle);
end;

procedure TGLApplication.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  { Call idle and timer events of Application and all windows.
    Idle events are handled by DoSelfIdle and OpenWindows.DoIdle.
    Timer events are handled by MaybeDoTimer, that in turn will call
    (when needed) DoSelfTimer and OpenWindows.DoTimer. }
  DoSelfIdle;
  FOpenWindows.DoIdle;
  MaybeDoTimer(LastDoTimerTime);

  { We need Done := false, otherwise the widgetset will hang waiting
    for next message. See ../components/castlecontrol.pas comments.

    Done := false also prevents other application idle handlers,
    but fortunately in this case we're the only thing interfacing with LCL,
    so that's not a problem. }
  Done := false;
end;

function TGLApplication.ProcessMessage(WaitForMessage, WaitToLimitFPS: boolean): boolean;
begin
  if Forms.Application.Terminated then Exit(false);

  { TODO: take WaitForMessage, WaitToLimitFPS into account }
  Forms.Application.ProcessMessages;
  Result := not Forms.Application.Terminated;
end;

function TGLApplication.ProcessAllMessages: boolean;
begin
//  Result := not Forms.Application.Terminated;
//  while Result do
    Result := ProcessMessage(true, true);
end;

procedure TGLApplication.Run;
begin
  if OpenWindowsCount = 0 then Exit;

  { Implementing Run by calling ProcessMessage in a loop (like below)
    is a valid and good implementation. Make sure your ProcessMessage
    honours WaitForMessage and WaitToLimitFPS = true,
    to avoid wasting CPU on "busy waiting". }
  // while ProcessMessage(true, true) do ;

  { We have to call here Forms.Application.Run, not just call our
    ProcessMessage (which makes Forms.Application.ProcessMessage) in a loop.
    That is because on some LCL widgetsets, you really need to run
    Forms.Application.Run, see e.g. Carbon that does special stuff inside
    TCarbonWidgetSet.AppRun inside lcl/interfaces/carbon/carbonobject.inc.
    Without this, global menu on Carbon is always blocked.

    TODO: that's unfortunate, as our TCastleWindowBase interface
    promises that it's not necessary to call Run, it should be always
    equivalent to call just ProcessMessage in a loop.

    TODO: this makes idle/timer broken, they need our
    TGLApplication.ProcessMessage to work. }

  Forms.Application.Run;
end;

procedure TGLApplication.QuitWhenNoOpenWindows;
begin
  Forms.Application.Terminate; // break also Forms.Application.Run
end;

function TGLApplication.ScreenWidth: integer;
begin
  Result := Screen.Width;
end;

function TGLApplication.ScreenHeight: integer;
begin
  Result := Screen.Height;
end;

function TGLApplication.BackendName: string;
begin
  Result := 'LCL';
end;

{$endif read_implementation}
