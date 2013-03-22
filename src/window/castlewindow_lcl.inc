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

  Bugs on Carbon and GTK2:
  - MouseLook stutters. Something similar to aggressive update needed?
    Also, during dragging with mouse, e.g. rotating a model with Examine mode,
    the Idle isn't called.
    This is all just like with TCastleControl. Official comments:

    The behavior of Lazarus application Idle events is such that they are executed
    only when there are no events left to process. This makes sense, and actually
    follows the docs and the name "idle".

    However, our Idle events (which would be more appropriately named something
    like Update events) expect to be run continously, that is: about the same number
    of times per second as the screen Redraw (and if the screen doesn't need to be redrawn,
    our Idle should still run a sensible number of times per second ---
    around the same value as FPSLimit, or (when FPSLimit is set to 0, meaning "unused")
    as many times as possible). For our Idle, it should not matter whether your event
    loop has something left to process.

    The problem is most obvious when moving the mouse, for example when using
    the mouse look to walk and look around in Walk mode, or when dragging with mouse
    in Examine mode. The event loop is then typically busy processing mouse move
    events all the time, so it's never/seldom empty (note: it doesn't mean that
    event loop is clogged, as mouse move events can be potentially accumulated
    at various levels --- LCL, underlying widgetset like GTK, underlying system
    like XWindows etc. I think in practice XWindows does it, but I'm not sure).
    Our program should however still be responsive. Not only the screen should
    be redrawn, regardless if our event loop is empty or not, but also
    our Idle event should be continously called. But if we just use Lazarus Idle/Redraw
    behavior (that descends from other widgetsets) then you may find that:
    - during mouse look things "stutter" --- no Idle, not even Redraw, happens regularly.
    - during mouse drag Redraw may be regular, but still Idle are not called
      (so e.g. animations do not move, instead they suddenly jump a couple of seconds
      forward when you stop dragging after a couple of seconds).

    TODO:
    - cleanup implementation of "aggresive update" in CastleControl
      (it's a little hacky in CastleControl, it could be more integrated to use FPSLimit
      and just cleaner)
    - make it enabled by default
    - possibly move it to CastleTimeUtils
    - reuse it here

    TODO: also, rename our Idle to Update? This would make the distinction clear,
    our "Idle" name is really a leftover from ancient times (where it was common
    to use glut idle callback for similar purposes).

  - (untested on GTK2, confirmed on Carbon)
    MainMenu.Enabled doesn't work,
    e.g. when you do command+r menu is not disabled in view3dscene.

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

{ TGoodMenuItem ---------------------------------------------------------------- }

type
  { Descendant of LCL TMenuItem used for our menu. }
  TGoodMenuItem = class(Menus.TMenuItem)
  protected
    Window: TCastleWindowBase;
    Entry: TMenuEntry;

    { Capture IntfDoSelect, which is fired on GTK2
      event 'select' on menu item, which we have to capture to call
      ReleaseAllKeysAndMouse.

      Just like in our gtk/castlewindow_gtk_menu.inc: capturing 'select' GTK2
      event is the only way to capture when user opened menu bar.
      And we need to capture it, to work with GTK2 and menu bar,
      e.g. when you use Alt+F to reach "File" menu, do not keep "Alt" key in
      the pressed state indefinitely (e.g. it would make camera keys non-working
      after opening a file using Alt+F and recent files in view3dscene).

      This should be also harmless on other non-GTK2 widgetsets. }
    procedure IntfDoSelect; override;
  end;

procedure TGoodMenuItem.IntfDoSelect;
begin
  inherited;
  Window.ReleaseAllKeysAndMouse;
end;

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

  procedure MenuCastleToLCL(const MyMenu: TMenu; const Menu: Menus.TMenuItem);
  var
    MenuItem: TGoodMenuItem;
    MyEntry: TMenuEntry;
    I: Integer;
  begin
    for I := 0 to MyMenu.EntriesCount - 1 do
    begin
      MyEntry := MyMenu.Entries[I];
      MenuItem := TGoodMenuItem.Create(Form);
      MenuItem.Window := Self;
      MenuItem.OnClick := @MenuItemClick;
      { store connection both ways between LCL menu item <-> our menu entry }
      MenuItem.Entry := MyEntry;
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

  { connet Menu with Form }
  Menu.Parent := Form;
  Form.Menu := Menu;

  { connect our MainMenu with LCL Menu.Items.
    Note that connection the other way (from Menu.Items to MainMenu)
    is not needed, as we don't handle Menu.Items.OnClick (it would also
    be impossible now, as Menu.Items is not of TGoodMenuItem class). }
  MainMenu.Handle := Menu.Items;
  { TODO: under GTK2 Menu.Items.Enabled is ignored? }
  MenuUpdateEnabled(MainMenu); // apply initial MainMenu.Enabled value

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
  { We use Forms.Application.CreateForm, not just "Form := TForm.CreateNew(nil)",
    because we want our Form to be set as Forms.Application.MainForm.

    And there is no other way to set Forms.Application.MainForm,
    we have to use mechanism that "the first form created by Application.CreateForm
    is set as main", see http://www.lazarus.freepascal.org/index.php?topic=14438.0
    and LCL sources of TApplication.UpdateMainForm.
    The "Forms.Application.MainForm" property is not directly settable.

    And we need to have a main form, as that's the only way to capture (in form
    close event) the Mac OS X "ProjectName -> Quit" automatic menu item (see
    http://bugs.freepascal.org/view.php?id=10983).

    Note: This would be a problem if we used TForm descendant,
    as then Forms.Application.CreateForm would search for a related resource.
    Unless we set RequireDerivedFormResource to false (which actually
    is the default RequireDerivedFormResource value for now, but the adviced
    value is true).
    Fortunately, the whole resource-searching is not used when we just
    have a TForm class (see TCustomForm.Create in LCL), so that's not a problem. }

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
      { Do not use Form.Cursor for this, e.g. hiding the cursor would not work
        then on Carbon. }
      OpenGLControl.Cursor := CursorCastleToLCL[Value];
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
  Entry: TMenuEntry;
begin
  Entry := (Sender as TGoodMenuItem).Entry;
  if Entry is TMenuItem then
  begin
    { It seems that checked menu item is always automatically swapped,
      at least with LCL-GTK2. So manually revert it to original setting,
      as we don't want this (DoMenuCommand will change Checked as necessary). }
    if Entry is TMenuItemChecked then
      MenuUpdateChecked(TMenuItemChecked(Entry));
    DoMenuCommand(TMenuItem(Entry));
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
    ReleaseAllKeysAndMouse; // do it manually
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
    ReleaseAllKeysAndMouse; // do it manually
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
  ReleaseAllKeysAndMouse; // do it manually
  MessageDlg(S, MessageTypeCastleToLCL[MessageType], [mbOK], 0);
end;

function TCastleWindowBase.MessageYesNo(const S: string;
  const MessageType: TWindowMessageType): boolean;
begin
  ReleaseAllKeysAndMouse; // do it manually
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
