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
  procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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
begin
  { TODO (may do nothing if you do not care about displaying menu) }
end;

procedure TCastleWindowBase.BackendMenuFinalize;
begin
  { TODO (may do nothing if you do not care about displaying menu) }
end;

procedure TCastleWindowBase.MenuUpdateCaption(Entry: TMenuEntryWithCaption);
begin
  { TODO (may do nothing if you do not care about displaying menu;
    may do MenuFinalize + MenuInitialize if you display menu,
    but don't care about updating it fast) }
end;

procedure TCastleWindowBase.MenuUpdateEnabled(Entry: TMenuEntryWithCaption);
begin
  { TODO (may do nothing if you do not care about displaying menu;
    may do MenuFinalize + MenuInitialize if you display menu,
    but don't care about updating it fast) }
end;

procedure TCastleWindowBase.MenuUpdateChecked(Entry: TMenuItemChecked);
begin
  { TODO (may do nothing if you do not care about displaying menu;
    may do MenuFinalize + MenuInitialize if you display menu,
    but don't care about updating it fast) }
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
  Form.OnCloseQuery := @FormCloseQuery;
  // TODO: FullScreen property apply in some way?

  // TODO: Cursor, CustomCursor, ResizeAllowed, MainMenu

  OpenGLControl := TOpenGLControl.Create(Form);
  OpenGLControl.Parent := Form;
  OpenGLControl.Align := alClient;

  OpenGLControl.DoubleBuffered := DoubleBuffer;
  OpenGLControl.StencilBits := StencilBits;
  OpenGLControl.DepthBits := DepthBits;
  OpenGLControl.AlphaBits := AlphaBits;
  OpenGLControl.MultiSampling := MultiSampling;
  // TODO: if not ZeroVector(AccumBits) then

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
  OpenGLControl := nil; { freeing the Form will free OpenGLControl too }
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
      { TODO UpdateCursor };
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
    { TODO };
end;

procedure TCastleWindowBase.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  DoCloseQuery;
  CanClose := false; // DoCloseQuery will call Close when needed
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
  LKeyToMyKey(Key, Shift, MyKey, MyCharKey);
  if (MyKey <> K_None) or (MyCharKey <> #0) then
    DoKeyDown(MyKey, MyCharKey);
end;

procedure TCastleWindowBase.OpenGLControlKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  MyKey: TKey;
  MyCharKey: char;
begin
  LKeyToMyKey(Key, Shift, MyKey, MyCharKey);
  if MyKey <> K_None then
    DoKeyUp(MyKey);
end;

procedure TCastleWindowBase.OpenGLControlMouseDown(Sender: TObject;
  Button: Controls.TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  MyButton: CastleKeysMouse.TMouseButton;
begin
  if LMouseButtonToMyMouseButton(Button, MyButton) then
    DoMouseDown(X, Y, MyButton);
end;

procedure TCastleWindowBase.OpenGLControlMouseUp(Sender: TObject;
  Button: Controls.TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  MyButton: CastleKeysMouse.TMouseButton;
begin
  if LMouseButtonToMyMouseButton(Button, MyButton) then
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
begin
  if OpenDialog then
    Dialog := TOpenDialog.Create(nil) else
    Dialog := TSaveDialog.Create(nil);
  try
    Dialog.Title :=  Title;
    Dialog.FileName := FileName;
    FileFiltersToDialog(FileFilters, Dialog);
    Result := Dialog.Execute;
    if Result then
      FileName := Dialog.FileName;
  finally FreeAndNil(Dialog) end;
end;

function TCastleWindowBase.ColorDialog(var Color: TVector3Single): boolean;
begin
  { TODO }
end;

procedure TCastleWindowBase.MessageOK(const S: string; const MessageType: TWindowMessageType);
begin
  { TODO }
end;

function TCastleWindowBase.MessageYesNo(const S: string;
  const MessageType: TWindowMessageType): boolean;
begin
  { TODO }
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
