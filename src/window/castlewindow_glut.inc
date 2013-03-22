{
  Copyright 2001-2013 Michalis Kamburelis.

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
  keysTimeout: array [TKey] of byte;
  glutHwnd: integer;
  function GetLeft: integer;
  function GetTop: integer;
  { Use always GlutDoKeyDown instead of DoKeyDown.
    GlutDoKeyDown calls DoKeyDown and takes care of updating
    KeysTimeout. }
  procedure GlutDoKeyDown(Key: TKey; c: char);
  procedure UpdateGlutModifiers;

  procedure UpdateCursor;
{$endif read_window_interface}

{$ifdef read_application_interface}
private
  LastDoTimerTime: TMilisecTime;
  function FindGlutWindow(seekGlutHwnd: integer): TCastleWindowBase;
{$endif read_application_interface}

{$ifdef read_implementation}
{$I castlewindow_dialogs_by_messages.inc}

{ some conversions ------------------------------------------------- }

function CharToKey(c: char): TKey;
{ zamien kod ASCII na odpowiedni klawisz ktory mogl taki char wygenerowac;
  zwroc K_None jesli nie ma. }
begin
 case c of
  CharBackSpace: result := K_BackSpace;
  CharTab: result := K_Tab;
  CharEnter: result := K_Enter;
  CharEscape: result := K_Escape;
  ' ': result := K_Space;
  'a'..'z': result := TKey(Ord(K_A) + Ord(c) - Ord('a'));
  'A'..'Z': result := TKey(Ord(K_A) + Ord(c) - Ord('A'));
  '0'..'9': result := TKey(Ord(K_0) + Ord(c) - Ord('0'));
  '+': result := K_Numpad_Plus;
  '-': result := K_Numpad_Minus;
  ',': result := K_Comma;
  '.': result := K_Period;
  else result := K_None;
 end;
end;

function GlutKeyToKey(GlutKey: Integer): TKey;
{ convert GLUT_KEY_xxx constant to TKey.
  Never returns K_None because we know about all defined GLUT_KEY_xxx
  constants and they all can be expressed as TKey values. }
begin
 case GlutKey of
  GLUT_KEY_F1: Result := K_F1;
  GLUT_KEY_F2: Result := K_F2;
  GLUT_KEY_F3: Result := K_F3;
  GLUT_KEY_F4: Result := K_F4;
  GLUT_KEY_F5: Result := K_F5;
  GLUT_KEY_F6: Result := K_F6;
  GLUT_KEY_F7: Result := K_F7;
  GLUT_KEY_F8: Result := K_F8;
  GLUT_KEY_F9: Result := K_F9;
  GLUT_KEY_F10: Result := K_F10;
  GLUT_KEY_F11: Result := K_F11;
  GLUT_KEY_F12: Result := K_F12;

  GLUT_KEY_LEFT:      Result := K_Left;
  GLUT_KEY_UP:        Result := K_Up;
  GLUT_KEY_RIGHT:     Result := K_Right;
  GLUT_KEY_DOWN:      Result := K_Down;
  GLUT_KEY_PAGE_UP:   Result := K_PageUp;
  GLUT_KEY_PAGE_DOWN: Result := K_PageDown;
  GLUT_KEY_HOME:      Result := K_Home;
  GLUT_KEY_END:       Result := K_End;
  GLUT_KEY_INSERT:    Result := K_Insert;

  else raise EInternalError.CreateFmt('unknown glut key %d', [GlutKey]);
 end;
end;

{ glut callbacks ---------------------------------------------------------- }

{$define GLUTBEGIN :=
 var GLUTBEGIN_glwin: TCastleWindowBase;
     GLUTBEGIN_glutHwnd: longint;
 begin
  GLUTBEGIN_glutHwnd := glutGetWindow();
  GLUTBEGIN_glwin := Application.FindGlutWindow(GLUTBEGIN_glutHwnd);
  if GLUTBEGIN_glwin <> nil then
   with GLUTBEGIN_glwin do
   begin }

{$define GLUTEND := end; end }

procedure displayByGlut; cdecl;
GLUTBEGIN
 DoDraw;
GLUTEND;

procedure reshapeByGlut(w, h: integer); cdecl;
GLUTBEGIN
 DoResize(w, h, false);
GLUTEND;

procedure keyboardByGlut(CharKeyByte: Byte; x, y: integer); cdecl;
{ glut keyboard = robimy GlutDoKeyDown,
  przy okazji robimy tez UpdateGlutModifiers. }
var
  Key: TKey;
  CharKey: Char absolute CharKeyByte;
GLUTBEGIN
 UpdateGlutModifiers;
 Key := CharToKey(CharKey);
 { Actually glut should never give me CharKey = #0 so this "if" should
   always pass. This is just a "paranoid" check. }
 if (Key <> K_None) or (CharKey <> #0) then
  GlutDoKeyDown(Key, CharKey);
GLUTEND;

procedure specialByGlut(key, x, y: integer); cdecl;
GLUTBEGIN
 UpdateGlutModifiers;
 GlutDoKeyDown(GlutKeyToKey(key), #0);
GLUTEND;

procedure mouseByGlut(button, state, x, y: integer);cdecl;
{ gdy glut daje nam mouse callback my robimy
  DoMouseUp / DoMouseDown i uaktualniamy zmienna mousePressedBtns. }
var btn: TMouseButton;
GLUTBEGIN
 UpdateGlutModifiers;
 case button of
  GLUT_LEFT_BUTTON: btn := mbLeft;
  GLUT_MIDDLE_BUTTON: btn := mbMiddle;
  else {GLUT_RIGHT_BUTTON:} btn := mbRight;
 end;
 case state of
  GLUT_UP:   DoMouseUp(x, y, btn);
  GLUT_DOWN: DoMouseDown(x, y, btn);
 end;
GLUTEND;

procedure motion_and_passiveMotionByGlut(x, y: integer); cdecl;
{ gdy glut daje nam motion lub passiveMotion my wywolujemy
  DoMouseMove. Uzywamy przy tym zmiennej mousepressed, ktora
  pozwala aplikacjom napisanym przy uzyciu CastleWindow dowiedziec
  sie jakie przyciski sa aktualnia wcisniete (i tym sposobem
  w CastleWindow nie potrzebujemy osobnego motion i passiveMotion). }
GLUTBEGIN
 DoMouseMove(x, y);
GLUTEND;

{procedure specialUpByGlut(key, x, y: integer); cdecl;
GLUTBEGIN
 DoKeyUp(GlutKeyToKey(key));
GLUTEND;}

procedure menuByGlut(Value: Integer); cdecl;
var
  MenuItem: TMenuItem;
GLUTBEGIN
  if Value <> -1 then
  begin
    MenuItem := MenuItemFromSmallId(Value);
    { Since glut doesn't allow me to show and respect Enabled
      property in the UI, I just check it here. }
    if MenuItem.Enabled then
      DoMenuClick(MenuItem);
  end;
GLUTEND;

{ menu things ------------------------------------------------------------ }

function TCastleWindowBase.RedirectKeyDownToMenuClick: boolean;
begin
 Result := true;
end;

procedure TCastleWindowBase.BackendMenuInitialize;

  procedure AddToCurrentMenuAllEntries(Menu: TMenu); forward;

  procedure AddToCurrentMenu(Entry: TMenuEntry);
  { Note: under glut I COULD use PtrInt(Item) instead of Item.SmallId.
    But, still, using Item.SmallId is just a little safer. }
  var
    CurrentMenu: Integer;
    TrueCaption: string;
    SubMenu: TMenu;
  begin
   if Entry is TMenuItem then
   begin
    TrueCaption := SRemoveMnemonics(TMenuItem(Entry).CaptionWithKey);
    if Entry is TMenuItemRadio then
    begin
      if TMenuItemChecked(Entry).Checked then
        TrueCaption += ' (*)' else
        TrueCaption += ' ( )';
    end else
    if Entry is TMenuItemChecked then
    begin
      if TMenuItemChecked(Entry).Checked then
        TrueCaption += ' <ON>' else
        TrueCaption += ' <OFF>';
    end;
    glutAddMenuEntry(PChar(TrueCaption), TMenuItem(Entry).SmallId);
   end else
   if Entry is TMenuSeparator then
    glutAddMenuEntry('------', -1) else
   if Entry is TMenu then
   begin
    SubMenu := TMenu(Entry);
    CurrentMenu := glutGetMenu();
    try
     SubMenu.Handle := Pointer(glutCreateMenu(@menuByGlut));
     AddToCurrentMenuAllEntries(TMenu(Entry));
    finally glutSetMenu(CurrentMenu) end;
    glutAddSubMenu(PChar(SRemoveMnemonics(TMenu(Entry).Caption)), PtrInt(SubMenu.Handle));
   end else
    raise EInternalError.Create('Not implemented TMenuEntry subclass');
  end;

  procedure AddToCurrentMenuAllEntries(Menu: TMenu);
  var i: Integer;
  begin
   for i := 0 to Menu.EntriesCount-1 do AddToCurrentMenu(Menu[i]);
  end;

begin
 MainMenu.Handle := Pointer(glutCreateMenu(@menuByGlut));
 AddToCurrentMenuAllEntries(MainMenu);
 glutAttachMenu(GLUT_RIGHT_BUTTON);
end;

procedure TCastleWindowBase.BackendMenuFinalize;

  procedure DoDestroyMenu(MenuEntry: TMenuEntry);
  var
    I: Integer;
    Menu: TMenu;
  begin
    if MenuEntry is TMenu then
    begin
      Menu := TMenu(MenuEntry);
      for I := 0 to Menu.EntriesCount - 1 do
        DoDestroyMenu(Menu[I]);
      glutDestroyMenu(PtrInt(Menu.Handle));
      Menu.Handle := nil;
    end;
  end;

begin
  { Recursively call glutDestroyMenu on all menus created by BackendMenuInitialize. }
  DoDestroyMenu(MainMenu);
end;

procedure TCastleWindowBase.MenuUpdateCaption(Entry: TMenuEntryWithCaption);
begin
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindowBase.MenuUpdateEnabled(Entry: TMenuEntryWithCaption);
begin
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindowBase.MenuUpdateChecked(Entry: TMenuItemChecked);
begin
  MenuFinalize;
  MenuInitialize;
end;

function TCastleWindowBase.MenuUpdateCheckedFast: boolean;
begin
  Result := false;
end;

{ ------------------------------------------------------------------------------
  procedury obslugujace TCastleWindowBase poprzez GLUTa }

procedure TCastleWindowBase.SetCursor(const Value: TMouseCursor);
begin
  if FCursor <> Value then
  begin
    FCursor := Value;
    if not Closed then
      UpdateCursor;
  end;
end;

procedure TCastleWindowBase.SetCustomCursor(const Value: TRGBAlphaImage);
begin
  { CustomCursor cannot be handled in glut. So we save FCustomCursor property and...
    do nothing with it. }
  FCustomCursor := Value;
end;

procedure TCastleWindowBase.UpdateCursor;
begin
  case Cursor of
    { CustomCursor cannot be handled in glut, so mcCustom is treated just like mcDefault }
    mcDefault, mcCustom: glutSetCursor(GLUT_CURSOR_INHERIT);
    mcNone: glutSetCursor(GLUT_CURSOR_NONE);
    mcStandard: glutSetCursor(GLUT_CURSOR_RIGHT_ARROW);
    mcWait: glutSetCursor(GLUT_CURSOR_WAIT);
    mcText: glutSetCursor(GLUT_CURSOR_TEXT);
    else raise EInternalError.Create('castlewindow_glut.inc TCastleWindowBase.UpdateCursor: unknown Cursor');
  end;
end;

procedure TCastleWindowBase.SetMousePosition(const NewMouseX, NewMouseY: Integer);
begin
  if not Closed then
    glutWarpPointer(NewMouseX, NewMouseY);
end;

procedure TCastleWindowBase.SwapBuffers;
begin
 glutSwapBuffers;
end;

procedure TCastleWindowBase.GlutDoKeyDown(Key: TKey; c: char);
const
  { tyle razy musi "tyknac" idleByGlut zebym uznal ze klawisz
    zostal zwolniony. }
  StartKeyTimeout = 10;
begin
 DoKeyDown(Key, c);
 if Key <> K_None then KeysTimeout[Key] := StartKeyTimeout
end;

procedure TCastleWindowBase.UpdateGlutModifiers;
{ uaktualnij stan K_Shift i Ctrl na podstawie glutGetModifiers().
  current window jest juz odpowiednio ustawione. }

  procedure UpdateModif(key: TKey; NewVal: boolean);
  begin
   if NewVal then
   begin
    if not Pressed[key] then GlutDoKeyDown(key, #0);
   end else
   begin
    if Pressed[key] then DoKeyUp(key)
   end;
  end;

var m: integer;
begin
 m := glutGetModifiers();
 UpdateModif(K_Shift, (m and GLUT_ACTIVE_SHIFT) <> 0);
 UpdateModif(K_Ctrl, (m and GLUT_ACTIVE_CTRL) <> 0);
 UpdateModif(K_Alt, (m and GLUT_ACTIVE_ALT) <> 0);
end;

procedure TCastleWindowBase.OpenBackend;
var GlutDispMode: LongWord;
begin
 { calculate GlutDispMode, call glutInitDisplayMode(GlutDispMode) }
 GlutDispMode := GLUT_RGB;
 if DoubleBuffer then
  GlutDispMode := GlutDispMode or GLUT_DOUBLE else
  GlutDispMode := GlutDispMode or GLUT_SINGLE;
 if StencilBits > 0 then
  GlutDispMode := GlutDispMode or GLUT_STENCIL;
 if DepthBits > 0 then
  GlutDispMode := GlutDispMode or GLUT_DEPTH;
 if AlphaBits > 0 then
  GlutDispMode := GlutDispMode or GLUT_ALPHA;
 if not ZeroVector(AccumBits) then
  GlutDispMode := GlutDispMode or GLUT_ACCUM;
 if MultiSampling > 1 then
  GlutDispMode := GlutDispMode or GLUT_MULTISAMPLE;

 glutInitDisplayMode(GlutDispMode);

 { Visible property: no way to implement this with glutCreateWindow? }

 if fullscreen then
 begin
  glutHwnd := glutCreateWindow(PChar(FCaption));
  glutFullScreen;
 end else
 begin
  glutInitWindowSize(width, height);
  glutInitWindowPosition(left, top);
  glutHwnd := glutCreateWindow(PChar(FCaption));
 end;

 { sprawdz czy zadane *Bits sa spelnione. Sprawdzamy to dopiero po
   glutCreateWindow. }
 CheckRequestedBufferAttributes('GlutInitDisplayMode',
   glutGet(GLUT_WINDOW_STENCIL_SIZE),
   glutGet(GLUT_WINDOW_DEPTH_SIZE), glutGet(GLUT_WINDOW_ALPHA_SIZE),
   glutGet(GLUT_WINDOW_ACCUM_RED_SIZE), glutGet(GLUT_WINDOW_ACCUM_GREEN_SIZE),
   glutGet(GLUT_WINDOW_ACCUM_BLUE_SIZE), glutGet(GLUT_WINDOW_ACCUM_ALPHA_SIZE),
   glutGet(GLUT_WINDOW_NUM_SAMPLES));

 Application.OpenWindowsAdd(Self);
 glutReshapeFunc(@reshapeByGlut);
 glutDisplayFunc(@displayByGlut);
 glutKeyboardFunc(@keyboardByGlut);
 glutSpecialFunc(@specialByGlut);
 glutMouseFunc(@mouseByGlut);
 glutMotionFunc(@motion_and_passiveMotionByGlut);
 glutPassiveMotionFunc(@motion_and_passiveMotionByGlut);
 {glutSpecialUpFunc(specialUpByGlut); (glutSpecialUpFunc is not implemented yet)}

 UpdateCursor;

 { menu }
 if MainMenu <> nil then MenuInitialize;
end;

procedure TCastleWindowBase.PostRedisplay;
begin
 if glutHwnd <> 0 then
 begin
  glutSetWindow(glutHwnd);
  glutPostRedisplay;
 end;
end;

procedure TCastleWindowBase.SetCaption(const Value: string);
begin
 FCaption := Value;
 if glutHwnd <> 0 then
 begin
  glutSetWindow(glutHwnd);
  glutSetWindowTitle(PChar(FCaption));
 end;
end;

function TCastleWindowBase.GetLeft: integer;
begin
 if glutHwnd <> 0 then
  result := glutGet(GLUT_WINDOW_X) else
  result := fLeft
end;

function TCastleWindowBase.GetTop: integer;
begin
 if glutHwnd <> 0 then
  result := glutGet(GLUT_WINDOW_Y) else
  result := fTop
end;

procedure TCastleWindowBase.CloseBackend;
begin
 if MainMenu <> nil then MenuFinalize;
 glutDestroyWindow(glutHwnd);
 glutHwnd := 0;
end;

procedure TCastleWindowBase.BackendMakeCurrent;
begin
  glutSetWindow(glutHwnd);
end;

procedure TCastleWindowBase.CreateBackend;
begin
end;

procedure TCastleWindowBase.FlushRedisplay;
begin
 { pod glutem nie mamy jak sie dowiedziec "czy wyslano redisplay ?" }
 DoDraw;
end;

{ TGLApplication na GLUTcie ------------------------------------------- }

procedure idleByGlut; cdecl;
var k: TKey;
    i: integer;
begin
 with Application do
 begin
  for i := 0 to OpenWindowsCount-1 do
   with OpenWindows[i] do
   begin
    for k := Low(TKey) to High(TKey) do
     if Pressed[k] then
     begin
      Dec(keysTimeout[k]);
      if keysTimeout[k] = 0 then DoKeyUp(k);
     end;
    DoIdle;
   end;
  DoSelfIdle;

  { Below: simple OnTimer implementation.
    More sophisticated implementation that tries to use glutTimerFunc
    is in old file gluttimerfunc.inc. }
  MaybeDoTimer(LastDoTimerTime);
 end;
end;

procedure TGLApplication.Run;
begin
  if OpenWindowsCount = 0 then Exit;

  glutMainLoop;
end;

function TGLApplication.ProcessMessage(WaitForMessage, WaitToLimitFPS: boolean): boolean;
begin
  Result := true;
  if Assigned(glutMainLoopEvent) then
    glutMainLoopEvent() else
    raise Exception.Create('glutMainLoopEvent not found (probably this is not freeglut, see freeglut.sourceforge.net). This implies that ProcessMessage (and friends, like modal boxes) cannot be implemented');
end;

function TGLApplication.ProcessAllMessages: boolean;
begin
  Result := true;
  if Assigned(glutMainLoopEvent) then
    glutMainLoopEvent() else
    raise Exception.Create('glutMainLoopEvent not found (probably this is not freeglut, see freeglut.sourceforge.net). This implies that ProcessMessage (and friends, like modal boxes) cannot be implemented');
end;

procedure TGLApplication.QuitWhenNoOpenWindows;
begin
  Halt;
end;

function TGLApplication.FindGlutWindow(seekGlutHwnd: integer): TCastleWindowBase;
var
  i: integer;
begin
  for i := 0 to OpenWindowsCount-1 do
    if OpenWindows[i].glutHwnd = seekGlutHwnd then
      Exit(OpenWindows[i]);
  result := nil;
end;

procedure TGLApplication.CreateBackend;
begin
 glutInit(@Argc, Argv);
 glutIdleFunc(@idleByGlut);
end;

procedure TGLApplication.DestroyBackend;
begin
end;

function TGLApplication.ScreenWidth: integer;
begin result := glutGet(GLUT_SCREEN_WIDTH) end;

function TGLApplication.ScreenHeight: integer;
begin result := glutGet(GLUT_SCREEN_HEIGHT) end;

function TGLApplication.BackendName: string;
begin
 Result := 'GLUT';
end;

{$endif read_implementation}
