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
  QuitPosted: boolean;
  LastDoTimerTime: TMilisecTime;
{$endif read_application_interface}

{$ifdef read_implementation}

{ TCastleWindowBase ------------------------------------------------------------------ }

procedure TCastleWindowBase.CreateBackend;
begin
end;

procedure TCastleWindowBase.BackendMenuInitialize;

  // TODO: checked and radio menu items

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
      if MyEntry is TMenuEntryWithCaption then
      begin
        { set MenuItem.Caption and Enabled }
        MenuUpdateCaption(TMenuEntryWithCaption(MyEntry));
        MenuUpdateEnabled(TMenuEntryWithCaption(MyEntry));
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
  MenuCastleToLCL(MainMenu, Menu.Items);
end;

procedure TCastleWindowBase.BackendMenuFinalize;
begin
  FreeAndNil(Menu);
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
    // TODO: this is bad. Should be: __ goes to _, _x goes to &x
    Result := SReplaceChars(S, '_', '&');
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
begin
  // TODO: slow
  MenuFinalize;
  MenuInitialize;
end;

function TCastleWindowBase.MenuUpdateCheckedFast: boolean;
begin
  Result := false;
end;

procedure TCastleWindowBase.SwapBuffers;
begin
  OpenGLControl.SwapBuffers;
end;

procedure TCastleWindowBase.OpenBackend;
begin
  Form := TForm.CreateNew(nil);
  Form.Width := Width;
  Form.Height := Height;
  Form.Top := Top;
  Form.Left := Left;
  Form.Caption := Caption; // SetCaption may also change it later
  Form.Cursor := CursorCastleToLCL[Cursor]; // SetCursor may also change it later

  Form.OnCloseQuery := @FormCloseQuery;
  Form.OnDeactivate := @FormDeactivate;

  // TODO: FullScreen
  // TODO: ResizeAllowed

  OpenGLControl := TOpenGLControl.Create(Form);
  OpenGLControl.Parent := Form;
  OpenGLControl.Align := alClient;

  OpenGLControl.DoubleBuffered := DoubleBuffer;
  OpenGLControl.StencilBits := StencilBits;
  OpenGLControl.DepthBits := DepthBits;
  OpenGLControl.AlphaBits := AlphaBits;
  OpenGLControl.MultiSampling := MultiSampling;
  // TODO: AccumBits: not possible to express in TOpenGLControl now

  // TODO: initial resize because OpenGL control is smaller because menu takes space?
  if MainMenu <> nil then MenuInitialize;

  Form.Visible := Visible;

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
  Result := { TODO } true;
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
      DoMenuCommand(TMenuItem(SenderTag));
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
  QuitPosted := false;
end;

procedure TGLApplication.DestroyBackend;
begin
end;

function TGLApplication.ProcessMessage(WaitForMessage, WaitToLimitFPS: boolean): boolean;
begin
  if QuitPosted then Exit(false);

  { TODO: take WaitForMessage, WaitToLimitFPS into account }
  Forms.Application.ProcessMessages;
  Result := not QuitPosted;

  { Call idle and timer events of Application and all windows.

    Idle events are handled by DoSelfIdle and OpenWindows.DoIdle.

    Timer events are handled by MaybeDoTimer, that in turn will call
    (when needed) DoSelfTimer and OpenWindows.DoTimer. }
  if Result then
  begin
    DoSelfIdle;
    FOpenWindows.DoIdle;
    MaybeDoTimer(LastDoTimerTime);
  end;
end;

function TGLApplication.ProcessAllMessages: boolean;
begin
//  Result := not QuitPosted;
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
  while ProcessMessage(true, true) do ;
end;

procedure TGLApplication.QuitWhenNoOpenWindows;
begin
  QuitPosted := true;
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
