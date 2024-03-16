{%MainUnit castlewindow.pas}
{
  Copyright 2013-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

(*CastleWindow backend using Lazarus LCL or Delphi FMX underneath.

  *Should* work with all platforms,
  all LCL widgetsets,
  all Delphi FMX platforms.

  Note that there are a few TODOs in this file, for some specific Delphi (FMX)
  or FPC / Lazarus (LCL) backends. Things are not perfect on their end always,
  there are platform-specific edge cases.
  We still try to support all LCL and FMX widgetsets here, *but*
  we focus our testing on platforms where we actually use this backend by default.
  This means right now only:

  - Delphi/Linux with FMX

  Historically, it was useful also with some LCL backends (e.g. we depended on
  this for FPC/macOS for a long time, before we implemented castlewindow_cocoa.inc).
  And it may be useful with more platforms in the future, both LCL and FMX
  (e.g. Delphi/macOS will likely depend on this too).

  - LCL TODO: Mouse look still stutters awfully on both Carbon and GTK2.
    ProcessTasks helps (without it, mouse look would not work at all)
    but doesn't completely fix the problem. Other CastleWindow backends
    have smooth mouse look.

    Note that Carbon is unused now (only Cocoa matters).
    Needs retest with Cocoa.
*)

{ How to implement calling CGE Update (and if needed Render) events continuously?

  - When USE_TIMER defined, we will use TTimer with short Interval
    to execute our processing regularly.

    This seems a cleaner solution, thus preferred if possible.
    Timer explicitly expresses "we want to run Update regularly,
    no matter how busy application is" which is what we want.

    And it means that all LCL/FMX functionality,
    like Forms.Application.Run, is free to be used. Internally,
    LCL/FMX is responsible for calling our timer event regularly, instead
    of hanging indefinitely waiting for user input.

  - When not USE_TIMER, we instead "control" the event loop more
    carefully and manually insert our processing between LCL/FMX processing.

    That is, we never let application sleep indefinitely inside
    OS event processing (e.g. we'll never call LCL/FMX Forms.Application.Run).
    We will always do LCL/FMX non-blocking event processing,
    and follow it with calls to CGE Update / Render as needed.

  Note: In the past we experimented with Application.OnIdle, but that's useless
  for our purpose.
  Idle is not executed regularly, it may not be executed at all when application
  is busy doing something else.
  Moreover, with Delphi FMX, there's no "add/remove
  idle listener" API, instead we'd have to override Application.OnIdle.
  So user cannot use Application.OnIdle for anything else.

  Delphi (with FMX) situation:

  - Timer is reliable on Windows. All our Delphi FMX and VCL components
    use TTimer like this.

  - On Linux, we *cannot* use timer.

    Timer with FMXLinux fails badly when we do non-trivial work in each update,
    like fps_game animations, or Spine dragon animations in play_animation.
    Running "flying" in play_animation just makes the screen go black,
    never updated. I was not able to debug it to the end -- the DoUpdate calls
    are still executed by timer, but rendering just doesn't work,
    even if we force doing DoRender from timer. It gets executed, but displayed
    window is not refreshed. The same problem occurs for FMX-drawn controls
    on form (like memo and buttons on CastleFmx).
    The same problem occurs for GTK dialogs, like open dialog shown
    by play_animation.

  FPC (with LCL) situation:

  (Possibly this is outdated comment!
  FPC situation wasn't tested for quite some time now.
  We don't really need CASTLE_WINDOW_FORM for FPC now, so we don't test it.)

  - Carbon (macOS): Timer is the only reliable solution.

    It is translated to Carbon timer.
    It is the only way to receive continuous messages in some cases.

    Otherwise such bugs can be observed (only on LCL-Carbon, not on LCL-GTK2):

    - When trying to Walk using mouse dragging in view3dscene, it doesn't
      work smoothly: when you simply keep some mouse button pressed,
      but don't move the mouse anymore (which should result in constant
      movement forward/backward if you did it after dragging up/down),
      then we don't get any events (so no movement, no redraw...).

    - Using view3dscene Navigation->Jump to viewpoint... doesn't work
      smoothly, there is a visible delay when we don't receive messages
      right after menu click, and so transtion is visible with some delay.

  - Other platforms?
    No decision, and practically it doesn't matter:
    with FPC, at this point we have covered all platforms with native
    TCastleWindow implementation, not depending on LCL and CASTLR_WINDOW_FORM.
}
{$if defined(FPC) or (not defined(LINUX))}
  {$define USE_TIMER}
{$endif}

{$ifdef read_interface_uses}
  {$ifdef FPC}
    // LCL
    Interfaces, Forms, Dialogs, OpenGLContext, Menus, ExtCtrls,
    Controls, FileUtil, Graphics, LCLType, CastleLCLUtils, Clipbrd, CustomTimer,
    LCLVersion,
  {$else}
    UITypes, Types, Rtti,
    // FMX
    {$ifdef LINUX} FMX.Platform.Linux, {$endif}
    FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
    FMX.StdCtrls, FMX.Menus, FMX.Platform,
    // CGE
    Fmx.CastleInternalGLControl, CastleInternalDelphiUtils, CastleFmxUtils,
  {$endif}
{$endif}

{$ifdef read_window_interface}
private
  type
    TFormMenuItem = {$ifndef FPC} FMX. {$endif} Menus.TMenuItem;

    { Descendant of LCL TMenuItem used for our menu. }
    TGoodMenuItem = class(TFormMenuItem)
    {$ifndef FPC}
    strict private
      function GetCaption: String;
      procedure SetCaption(const Value: String);
      function GetChecked: Boolean;
      procedure SetChecked(const Value: Boolean);
    {$endif}
    public
      Window: TCastleWindow;
      Entry: TMenuEntry;

      {$ifdef FPC}
      // only for LCL
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
      {$else}
      { Use FMX Text under the hood. }
      property Caption: String read GetCaption write SetCaption;

      { Use FMX IsChecked under the hood. }
      property Checked: Boolean read GetChecked write SetChecked;
      {$endif}
    end;

    { Like TOpenGLControl but sets Invalidated:=true on Window,
      so that Window knows when it's invalidated by the OS. }
    TGoodOpenGLControl = class(TOpenGLControl)
    public
      Window: TCastleWindow;
      procedure Invalidate; override;
      {$ifdef FPC}
      { Size in pixels, not scaled by anything.
        This is trivial in LCL, defined just for compatibility
        with FMX TOpenGLControl where it is not so trivial. }
      function PixelsWidth: Integer;
      function PixelsHeight: Integer;
      { Scale of mouse coordinates. }
      function MousePosScale: Single;
      {$endif}
    end;

  var
    Form: TForm;
    OpenGLControl: TGoodOpenGLControl;
    Menu: TMainMenu;
    { TLCLKeyPressHandler is only necessary for LCL.
      FMX has much more comfortable KeyDown (that gets both key code and char),
      just like CGE KeyDown, so it's more straightforward to handle. }
    {$ifdef FPC}
    FKeyPressHandler: TLCLKeyPressHandler;
    {$endif}

  procedure MenuConvert(const MyMenu: TMenu; const AMenu: TFormMenuItem);
  function MenuEntryConvert(const MyEntry: TMenuEntry): TGoodMenuItem;
  procedure MainMenuConvert(const MyMenu: TMenu; const AMenu: TMainMenu);
  class function CheckMenu(const Entry: TMenuEntry; out GoodMenuItem: TGoodMenuItem): boolean;

  procedure MenuItemClick(Sender: TObject);
  procedure MenuUpdateShortcut(Entry: TMenuItem);
  procedure ProcessTasks;
  procedure UpdateCursor;
  procedure UpdateFullScreenForm;
  {$ifdef FPC}
  procedure KeyPressHandlerPress(Sender: TObject;
    const Event: TInputPressRelease);
  {$endif}

  procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  procedure FormDeactivate(Sender: TObject);
  procedure FormDropFiles(Sender: TObject; const FileNames: array of string);

  procedure OpenGLControlPaint(Sender: TObject);
  procedure OpenGLControlResize(Sender: TObject);
  procedure OpenGLControlKeyDown(Sender: TObject; var Key: Word;
    {$ifndef FPC} var KeyChar: WideChar; {$endif}
    Shift: TShiftState);
  {$ifdef FPC}
  procedure OpenGLControlUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  {$endif}
  procedure OpenGLControlKeyUp(Sender: TObject; var Key: Word;
    {$ifndef FPC} var KeyChar: WideChar; {$endif}
    Shift: TShiftState);
  procedure OpenGLControlMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState;
    X, Y: {$ifdef FPC} Integer {$else} Single {$endif});
  procedure OpenGLControlMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState;
    X, Y: {$ifdef FPC} Integer {$else} Single {$endif});
  procedure OpenGLControlMouseMove(Sender: TObject; Shift: TShiftState;
    X, Y: {$ifdef FPC} Integer {$else} Single {$endif});
  procedure OpenGLControlMouseWheel(Sender: TObject; Shift: TShiftState;
    WheelDelta: Integer; {$ifdef FPC} MousePos: TPoint; {$endif}
    var Handled: Boolean);
  function MousePosToCastle(
    const X, Y: {$ifdef FPC} Integer {$else} Single {$endif}): TVector2;
{$endif read_window_interface}

{$ifdef read_application_interface}
private
  {$ifdef USE_TIMER}
  FTimer: TTimer;
  procedure ApplicationTimer(Sender: TObject);
  {$endif}
  procedure ProcessTasks;
{$endif read_application_interface}

{$ifdef read_implementation}

{ Return LCL or FMX Application singleton.
  Just trivial "Application" in most code, but we need to avoid clashing with
  CGE "Application" and take into account that LCL / FMX
  have different units. }
function FormApplication: TApplication;
begin
  Result := {$ifndef FPC} FMX. {$endif} Forms.Application;
end;

{ TGoodMenuItem ---------------------------------------------------------------- }

{$ifdef FPC}
procedure TCastleWindow.TGoodMenuItem.IntfDoSelect;
begin
  inherited;
  Window.ReleaseAllKeysAndMouse;
end;
{$else}
function TCastleWindow.TGoodMenuItem.GetCaption: String;
begin
  Result := Text;
end;

procedure TCastleWindow.TGoodMenuItem.SetCaption(const Value: String);
begin
  Text := Value;
end;

function TCastleWindow.TGoodMenuItem.GetChecked: Boolean;
begin
  Result := IsChecked;
end;

procedure TCastleWindow.TGoodMenuItem.SetChecked(const Value: Boolean);
begin
  IsChecked := Value;
end;
{$endif}

{ TGoodOpenGLControl --------------------------------------------------------- }

procedure TCastleWindow.TGoodOpenGLControl.Invalidate;

{ TODO: In FMX, this is not called automatically when a system event
  (e.g. when window is obscured and then revealed again) occurs.

  Still, this is a useful method to react to.
  When CGE knows to invalidate (because e.g. some UI called
  VisibleChange, or we have AutoRedisplay) then TCastleWindow.Invalidate
  is called, and in turn calls this method.

  We should make sure to call DoRender soon:

  - In FMX, "inherited" will invalidate the control
    using FMX features, causing OpenGLControl.OnPaint soon.

    So we actually don't depend on "Window.Invalidated" (own tracking
    of invalidated).

  - Unsure about LCL at this point, does it cause OpenGLControl.OnPaint
    reliably on all backends.

    So with LCL (FPC) we rely on own Window.Invalidated.
    Window.Invalidated := true is set set here,
    later DoRender will clear it.
}

begin
  // track our own Invalidated state, although still cooperate with LCL invalidate
  Window.Invalidated := true;
  inherited;
end;

{$ifdef FPC}
function TCastleWindow.TGoodOpenGLControl.PixelsWidth: Integer;
begin
  Result := Width;
end;

function TCastleWindow.TGoodOpenGLControl.PixelsHeight: Integer;
begin
  Result := Height;
end;

function TCastleWindow.TGoodOpenGLControl.MousePosScale: Single;
begin
  Result := 1;
end;
{$endif}

{ TCastleWindow ------------------------------------------------------------------ }

procedure TCastleWindow.CreateBackend;
begin
end;

function TCastleWindow.MenuEntryConvert(const MyEntry: TMenuEntry): TGoodMenuItem;
begin
  Result := TGoodMenuItem.Create(Form);
  Result.Window := Self;
  Result.OnClick := {$ifdef FPC}@{$endif} MenuItemClick;
  { store connection both ways between LCL menu item <-> our menu entry }
  Result.Entry := MyEntry;
  MyEntry.Handle := Result;
  if MyEntry is TMenuItem then
    MenuUpdateShortcut(TMenuItem(MyEntry));
  if MyEntry is TMenuEntryWithCaption then
  begin
    { set MenuItem.Caption and Enabled }
    MenuUpdateCaption(TMenuEntryWithCaption(MyEntry));
    MenuUpdateEnabled(TMenuEntryWithCaption(MyEntry));
    if MyEntry is TMenuItemChecked then
    begin
      {$ifdef FPC}
      Result.ShowAlwaysCheckable := true;
      {$endif}
      Result.RadioItem := MyEntry is TMenuItemRadio;
      MenuUpdateChecked(TMenuItemChecked(MyEntry));
    end;
  end else
  if MyEntry is TMenuSeparator then
    { set Result.Caption - special for separator }
    Result.Caption := '-';
  { if we have submenu, do this recursively }
  if MyEntry is TMenu then
    MenuConvert(TMenu(MyEntry), Result);
end;

procedure TCastleWindow.MenuConvert(const MyMenu: TMenu;
  const AMenu: TFormMenuItem);
var
  I: Integer;
  NewMenuChild: TFormMenuItem;
begin
  for I := 0 to MyMenu.Count - 1 do
  begin
    NewMenuChild := MenuEntryConvert(MyMenu[I]);
    AMenu.{$ifdef FPC}Add{$else}AddObject{$endif}(NewMenuChild);
  end;
end;

procedure TCastleWindow.MainMenuConvert(
  const MyMenu: TMenu; const AMenu: TMainMenu);
{$ifdef FPC}
begin
  MenuConvert(MyMenu, AMenu.Items);
{$else}
var
  I: Integer;
  NewMenuChild: TFormMenuItem;
begin
  for I := 0 to MyMenu.Count - 1 do
  begin
    NewMenuChild := MenuEntryConvert(MyMenu[I]);
    AMenu.{$ifdef FPC}Add{$else}AddObject{$endif}(NewMenuChild);
  end;
{$endif}
end;

procedure TCastleWindow.BackendMenuInitialize;
begin
  Menu := TMainMenu.Create(Form);
  { connet Menu with Form }
  Menu.Parent := Form;
  {$ifdef FPC} // in FMX, there's no need to assign main menu to any form property
  Form.Menu := Menu;
  {$endif}
  MainMenuConvert(MainMenu, Menu);
end;

procedure TCastleWindow.BackendMenuFinalize;
begin
  FreeAndNil(Menu);
  MainMenu.ClearHandles;
end;

class function TCastleWindow.CheckMenu(const Entry: TMenuEntry; out GoodMenuItem: TGoodMenuItem): boolean;
begin
  GoodMenuItem := TObject(Entry.Handle) as TGoodMenuItem;
  Result := GoodMenuItem <> nil;

  if (not Result) and
     ((Entry.CorrectParentWindow = nil) or Entry.CorrectParentWindow.MainMenuVisible) then
    WritelnWarning('Menu', 'Menu entry has Handle = nil (this means that corresponding LCL menu item object was not created, but it should)');
end;

procedure TCastleWindow.MenuUpdateShortcut(Entry: TMenuItem);

  {$ifndef FPC}
  { Convert FMX key code (Word) and shift state to TShortCut.
    FMX doesn't have this function built-in,
    but it is straightforward following
    http://docwiki.embarcadero.com/RADStudio/Sydney/en/Representing_Keys_and_Shortcuts#Representing_Shortcuts }
  function KeyToShortCut(const Key: Word; const Shift: TShiftState): TShortCut;
  begin
    Result := Key;
    if ssShift in Shift then
      Result := Result or scShift;
    if ssCtrl in Shift then
      Result := Result or scCtrl;
    if ssAlt in Shift then
      Result := Result or scAlt;
  end;
  {$endif}

var
  KeyCode: Word;
  Shift: TShiftState;
  FormMenuItem: TGoodMenuItem;
begin
  if not CheckMenu(Entry, FormMenuItem) then Exit;

  { TODO: FMX Linux doesn't handle (pass to GTK?) "Home" shortcut it seems.
    Or Page Up / Page Down.
    It is displayed in menu OK, but FMX doesn't actually call menu item
    when pressing home.
    Testcase: castle-view-image. }

  KeyFromCastle(Entry.Key, Entry.KeyString, Entry.Modifiers, KeyCode, Shift);
  FormMenuItem.ShortCut := KeyToShortCut(KeyCode, Shift);
end;

procedure TCastleWindow.MenuUpdateCaption(Entry: TMenuEntryWithCaption);

  function MenuCaptionFromCastle(const S: string): string;
  begin
    { We use _ for the same purpose as FMX / LCL use &: to mark mnemonics.

      But we cannot just replace _ with &:

      Double underscore should be converted to single underscore.
      Only unpaired single underscore should be converted to &.

      Moreover, single & shoule be doubled: to make it visible,
      not as accelerator.

      Note: FMX Linux has a bug, and it strips last _ from
      '&foo with underscore : _' .
      Probably FMX Linux code doesn't quote (double) the underscore and it gets
      interpreted by GTK as accelerator?
      Since underscore is GTK accelerator,
      https://docs.gtk.org/gtk3/ctor.CheckMenuItem.new_with_mnemonic.html .
      We have autotest that we pass it OK (single underscore) to FMX,
      see TTestCastleStringUtils.TestSReplacePatternsMenu . }

    Result := SReplacePatterns(S, ['__', '_', '&'], ['_', '&', '&&'], false);
  end;

var
  FormMenuItem: TGoodMenuItem;
begin
  if not CheckMenu(Entry, FormMenuItem) then Exit;
  FormMenuItem.Caption := MenuCaptionFromCastle(Entry.Caption);
end;

procedure TCastleWindow.MenuUpdateEnabled(Entry: TMenuEntryWithCaption);

  procedure RefreshEnabled(MyMenu: TMenu);

    procedure RefreshEnabledEntry(const MyEntry: TMenuEntry);
    begin
      if MyEntry is TMenuEntryWithCaption then
        MenuUpdateEnabled(TMenuEntryWithCaption(MyEntry));
      if MyEntry is TMenu then
        RefreshEnabled(TMenu(MyEntry));
    end;

  var
    I: Integer;
  begin
    for I := 0 to MyMenu.Count - 1 do
      RefreshEnabledEntry(MyMenu[I]);
  end;

var
  FormMenuItem: TGoodMenuItem;
begin
  { To apply MainMenu.Enabled: we would prefer to map MainMenu.Enabled
    on LCL Menu.Items.Enabled, and just handle MainMenu.Enabled like
    every other menu item. But LCL (at least GTK2 and Carbon) seems to
    completely ignore Menu.Items.Enabled.

    So to do make it work, we
    1. Always combine MainMenu.Enabled with particular item's Enabled value
       (in particular, this way BackendMenuInitialize applies it to every item).
    2. At MenuUpdateEnabled for MainMenu (so when you change MainMenu.Enabled
       value) we do a refresh (update enabled state) of all the menu items.

    Note that we cannot just do MenuFinalize + MenuInitialize to implement
    the 2nd step, because macOS (Carbon) reacts very badly to menu rebuilds.
    There is a problem when you have a menu item with key shortcut,
    and when this menu item is executed --- we destroy and create new menu
    that also reacts to this shortcut. In this case, Carbon executes
    the menu item again! Consider e.g. view3dscene "File->Open" with
    Ctrl+O shortcut, that after opening a new file rebuilds the menu
    to update the "recent files" menu section. Consider also "Display->Raytrace"
    with Ctrl+R shortcut, that after choosing options changes
    the MainMenu.Enabled. If we would do a MenuFinalize + MenuInitialize
    to easiy recreate LCL menu, we would see open dialog twice after Ctrl+O,
    and raytrace dialog twice after Ctrl+R. So we don't do it.
    In MenuInsert and MenuDelete and in this method, MenuUpdateEnabled,
    we merely change the existing LCL menu instances.

    This all could be considered LCL bug, anyway we have to work with it.
  }

  if Entry = MainMenu then
  begin
    RefreshEnabled(MainMenu);
    Exit;
  end;

  if not CheckMenu(Entry, FormMenuItem) then Exit;
  FormMenuItem.Enabled := Entry.Enabled and MainMenu.Enabled;
end;

procedure TCastleWindow.MenuUpdateChecked(Entry: TMenuItemChecked);
var
  FormMenuItem: TGoodMenuItem;
begin
  // TODO: LCL GTK2 backend only: All radio menu items are always on, reason unknown.

  { TODO: FMX on Linux (not a problem elsewhere, in particular with FMX on Windows):
    Sometimes it seems that checked menu item, after updating to "true",
    is left with an empty checkbox box -- a state that actually should not be
    possible through FMX API (it shows a checkbox possibility but it is "off",
    and FMX doesn't have property like ShowAlwaysCheckable from LCL)
    and moreover incorrect (we set it to "true", but it shows "false").
    Testcase: window_menu, set to "Top", then set to "Top" again.
  }

  if not CheckMenu(Entry, FormMenuItem) then Exit;
  FormMenuItem.Checked := Entry.Checked;
end;

function TCastleWindow.MenuUpdateCheckedFast: boolean;
begin
  Result := true;
end;

procedure TCastleWindow.MenuInsert(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);

  procedure InsertInMainMenu;
  var
    NewFormMenu: TFormMenuItem;
  begin
    NewFormMenu := MenuEntryConvert(Entry);
    Menu.{$ifdef FPC}Items.Insert{$else}InsertObject{$endif}(ParentPosition, NewFormMenu);
  end;

var
  ParentFormMenu, NewFormMenu: TGoodMenuItem;
begin
  if not CheckMenu(Parent, ParentFormMenu) then
  begin
    { Our main menu (Menu:TMainMenu) is not set as MainMenu.Handle,
      because we use just standard FMX / LCL TMainMenu class,
      not our TGoodMenuItem.
      But we can handle it by a special case here. }
    if Parent = MainMenu then
      InsertInMainMenu;
    Exit;
  end;

  NewFormMenu := MenuEntryConvert(Entry);
  ParentFormMenu.{$ifdef FPC}Insert{$else}InsertObject{$endif}(ParentPosition, NewFormMenu);
end;

procedure TCastleWindow.MenuDelete(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
var
  FormMenuItem: TGoodMenuItem;
begin
  if not CheckMenu(Entry, FormMenuItem) then Exit;
  Entry.ClearHandles;

  {$if defined(LCLCarbon) or defined(LCLCocoa)}
    (*Workaround LCL on Carbon bug http://bugs.freepascal.org/view.php?id=20688 .
      (assuming it's the same on Cocoa)

      Symptoms:
      Sometimes after menu changes we have a segfault in Carbon widgetset,
      like our menu item reference was invalid.
      Happens after various menu changes: rebuilding the menu, also fast changes
      like MenuDelete (like when changing recent files submenu in view3dscene).
      It seems that Carbon doesn't like deleting a menu currently used.

      Happens both when debugging and not debugging.
      Using the file menu to choose a recent file to load from view3dscene
      is the most common case.

      In Lazarus debuggger:
        Project view3dscene.app raised exception class 'External: EXC_BAD_ACCESS'.
        In file './carbon/carbonlclintf.inc' at line 342:
        end; {TCarbonWidgetSet.PromptUser}

      When running from console:
        TApplication.HandleException Access violation
          Stack trace:
          $0039566B  CARBONAPP_COMMANDPROCESS,  line 2872 of /Users/michalis/sources/lazarus/trunk/lcl/interfaces/./carbon/carbonint.pas

      Most probably this is the same thing as reported on
      http://bugs.freepascal.org/view.php?id=20688  .
      Less likely, to http://bugs.freepascal.org/view.php?id=18908 ?

      Workaround for now is just to not remove LCL menu items, only hide them.
      This at least fixes the issue when we delete menu by MenuDelete
      (as opposed to menu rebuild).
    *)
    FormMenuItem.Visible := false;
  {$else}
    FreeAndNil(FormMenuItem);
  {$endif}
end;

procedure TCastleWindow.SwapBuffers;
begin
  OpenGLControl.SwapBuffers;
end;

procedure TCastleWindow.OpenBackend;

  procedure GetInitialMousePos;
  var
    {$ifdef FPC}
    InitialMousePos: TPoint;
    {$else}
    InitialMousePos: TPointF;
    MouseService: IFMXMouseService;
    {$endif}
  begin
    {$ifdef FPC}
    InitialMousePos := Mouse.CursorPos;
    InitialMousePos := OpenGLControl.ScreenToControl(InitialMousePos);
    {$else}
    if not TPlatformServices.Current.SupportsPlatformService(IFMXMouseService, MouseService) then
      Exit;
    InitialMousePos := MouseService.GetMousePos;
    //WritelnLog('Initial mouse position (screen coordinates): %f %f', [InitialMousePos.X, InitialMousePos.Y]);
    InitialMousePos := Form.ScreenToClient(InitialMousePos);
    //WritelnLog('Initial mouse position (form coordinates): %f %f', [InitialMousePos.X, InitialMousePos.Y]);
    InitialMousePos := OpenGLControl.AbsoluteToLocal(InitialMousePos);
    //WritelnLog('Initial mouse position (OpenGL control coordinates): %f %f', [InitialMousePos.X, InitialMousePos.Y]);
    {$endif}
    FMousePosition := MousePosToCastle(InitialMousePos.X, InitialMousePos.Y);
    //WritelnLog('Initial mouse position (CGE coordinates): %s', [FMousePosition.ToString]);
  end;

  { Convert argument to assign to Form.Width / Height to make the result
    be a size in pixels size (not some FMX scaled size). }
  function FormSizeRequestPrecisely(const Form: TCustomForm; const PixelSize: Integer): Integer;
  begin
    Result := PixelSize;

    // Without this, requesting window size like 200 actually gives you 200 scaled by window scale
    // Testcase: multi_window with Delphi/Linux.
    {$if defined(DELPHI) and defined(LINUX)}
    if Form.Handle <> nil then
      Result := Round(Result / TLinuxWindowHandle(Form.Handle).Scale);
    {$endif}

    { Note: We tried, but failed, to cope with FMX (Delphi) scaling also on Windows.
      See castlewindow_form_fmx_scaling.md }
  end;

begin
  {$ifdef FPC}
  FKeyPressHandler := TLCLKeyPressHandler.Create;
  FKeyPressHandler.OnPress := @KeyPressHandlerPress;
  {$endif}

  {$ifdef FPC}
  { We use FormApplication.CreateForm, not just "Form := TForm.CreateNew(nil)",
    because we want our Form to be set as FormApplication.MainForm.

    And there is no other way to set FormApplication.MainForm,
    we have to use mechanism that "the first form created by Application.CreateForm
    is set as main", see http://www.lazarus.freepascal.org/index.php?topic=14438.0
    and LCL sources of TApplication.UpdateMainForm.
    The "Forms.Application.MainForm" property is not directly settable.

    And we need to have a main form, as that's the only way to capture (in form
    close event) the macOS "ProjectName -> Quit" automatic menu item (see
    http://bugs.freepascal.org/view.php?id=10983).

    Note: This would be a problem if we used TForm descendant,
    as then Forms.Application.CreateForm would search for a related resource.
    Unless we set RequireDerivedFormResource to false (which actually
    is the default RequireDerivedFormResource value for now, but the adviced
    value is true).
    Fortunately, the whole resource-searching is not used when we just
    have a TForm class (see TCustomForm.Create in LCL), so that's not a problem. }

  FormApplication.CreateForm(TForm, Form);
  {$else}
  { In FMX, we cannot use
      FormApplication.CreateForm
    as it doesn't create form immediately.
    We'd have to follow with RealCreateForms,
    https://docwiki.embarcadero.com/Libraries/Sydney/en/FMX.Forms.TApplication.RealCreateForms

    But it is simpler to just use TForm.CreateNew.
    We can assign MainForm later. }

  Form := TForm.CreateNew(FormApplication);
  FormApplication.MainForm := Form;
  {$endif}

  {$ifdef FPC}
  // FMX: TODO
  Form.OnDropFiles := @FormDropFiles;
  {$endif}

  Form.Top := Top;
  Form.Left := Left;
  { LCL note:
    We would prefer to leave Form.AutoSize := true and allow form to adjust
    to OpenGLControl.Width/Height. But it doesn't work with LCL. }
  Form.Width := FormSizeRequestPrecisely(Form, Width);
  Form.Height := FormSizeRequestPrecisely(Form, Height);
  Form.Caption := GetWholeCaption; // SetCaption may also change it later

  UpdateFullScreenForm;

  Form.OnCloseQuery := {$ifdef FPC}@{$endif} FormCloseQuery;
  { Note: I tried also using OpenGLControl.OnExit for this, seems to be quivalent
    in our case. And, similarly, both methods still need manual workaround
    to call ReleaseAllKeysAndMouse manually in MessageOK and similar methods
    (otherwise: check e.g. window_events with Window.MessageOk from Update:
    entering MessageOK *must* make ReleaseAllKeysAndMouse, otherwise it hangs.) }
  Form.OnDeactivate := {$ifdef FPC}@{$endif} FormDeactivate;

  { Do not make it owned by Form, instead we will free it explicitly.
    This avoids problems in Invalidate, where we access OpenGLControl instance,
    but we may be already in csDestroying state of Form (so it's owned components
    are already freed).

    Affects macOS with Carbon widgetset behavior when making view3dscene
    screenshot from command-line, like
    ".../view3dscene .../dynamic_world.x3dv --screenshot 0 output_2d_screenshot.png".

    Alternatively we could check "not (csDestroying in Form.ComponentState)"
    in Invalidate, but this feels cleaner. }

  OpenGLControl := TGoodOpenGLControl.Create(nil);
  OpenGLControl.Window := Self;
  OpenGLControl.Parent := Form;
  OpenGLControl.Align := {$ifdef FPC} alClient {$else} TAlignLayout.Client {$endif};
  OpenGLControl.Width := Width;
  OpenGLControl.Height := Height;
  {$ifdef FPC} // TODO: FMX
  OpenGLControl.Constraints.MinWidth := MinWidth;
  OpenGLControl.Constraints.MinHeight := MinHeight;
  OpenGLControl.Constraints.MaxWidth := MaxWidth;
  OpenGLControl.Constraints.MaxHeight := MaxHeight;
  {$endif}
  OpenGLControl.TabStop := true;

  {$ifdef FPC}
    OpenGLControl.DoubleBuffered := DoubleBuffer;
    OpenGLControl.StencilBits := StencilBits;
    OpenGLControl.DepthBits := DepthBits;
    OpenGLControl.AlphaBits := AlphaBits;
    OpenGLControl.MultiSampling := MultiSampling;
  {$else}
    OpenGLControl.Requirements.DoubleBuffer := DoubleBuffer;
    OpenGLControl.Requirements.StencilBits := StencilBits;
    OpenGLControl.Requirements.DepthBits := DepthBits;
    OpenGLControl.Requirements.AlphaBits := AlphaBits;
    OpenGLControl.Requirements.MultiSampling := MultiSampling;
  {$endif}

  UpdateCursor; // SetCursor may call it also later

  Form.ActiveControl := OpenGLControl;

  if (MainMenu <> nil) and MainMenuVisible then
    MenuInitialize;

  Form.Visible :=
    {$if not (defined(DELPHI) and defined(LINUX))}
      Visible
    {$else}
      { For Delphi/Linux, we set Form.Visible always to true.
        Otherwise the GDK resources are not created for hidden window,
        and thus we cannot create OpenGL context there.
        In effect our testcases (that create window with Visible:=false)
        would fail on Delphi/Linux with
        "Widget does not have GDK handle initialized yet". }
      true
    {$endif};

  if FullScreen then
  begin
    FLeft := 0;
    FTop := 0;
  end;

  { pass the actual OpenGLControl sizes (e.g. after accounting for MainMenu size
    under GTK2 widgetset, actually under all widgetsets that do not have global
    menu (like macOS), and after accounting for FullScreen).
    DoResize is already implemented to handle such
    call (from OpenBackend, before EventOpen was called) correctly. }
  DoResize(
    OpenGLControl.PixelsWidth,
    OpenGLControl.PixelsHeight, false);

  { Assign OpenGLControl.OnPaint *after* making Form visible.
    If we would assign OpenGLControl.OnPaint earlier, then
    Form.Visible := true would cause OpenGLControlPaint *before*
    OpenGL context was created. }
  OpenGLControl.OnPaint := {$ifdef FPC}@{$endif} OpenGLControlPaint;
  OpenGLControl.OnResize := {$ifdef FPC}@{$endif} OpenGLControlResize;
  OpenGLControl.OnKeyDown := {$ifdef FPC}@{$endif} OpenGLControlKeyDown;
  {$ifdef FPC}
  OpenGLControl.OnUTF8KeyPress := {$ifdef FPC}@{$endif} OpenGLControlUTF8KeyPress;
  {$endif}
  OpenGLControl.OnKeyUp := {$ifdef FPC}@{$endif} OpenGLControlKeyUp;
  OpenGLControl.OnMouseDown := {$ifdef FPC}@{$endif} OpenGLControlMouseDown;
  OpenGLControl.OnMouseUp := {$ifdef FPC}@{$endif} OpenGLControlMouseUp;
  OpenGLControl.OnMouseMove := {$ifdef FPC}@{$endif} OpenGLControlMouseMove;
  OpenGLControl.OnMouseWheel := {$ifdef FPC}@{$endif} OpenGLControlMouseWheel;

  { Make sure we have OpenGL context created now, and send initial Invalidate
    (it may not happen automatically, testcase: castle-view-image on Carbon).
    TCastleWindow.OpenCore will call MakeCurrent next. }
  OpenGLControl.HandleNeeded;
  OpenGLControl.Invalidate;

  Application.OpenWindowsAdd(Self);

  GetInitialMousePos;

  Container.Dpi := {$ifdef FPC} Screen {$else} OpenGLControl {$endif}.PixelsPerInch;
end;

const
  BorderNone = {$ifdef FPC} Controls.bsNone {$else} TFmxFormBorderStyle.None {$endif};

procedure TCastleWindow.UpdateFullScreenForm;
begin
  if FullScreen then
  begin
    { For Lazarus (LCL with FPC):
      - Setting Form.WindowState to wsFullScreen is key to make form fullscreen.

      For FMX (in Delphi):
      - Setting Form.FullScreen := true (done later) it key.
        For both Linux and Windows.
      - Setting Form.BorderStyle to BorderNone actually breaks it on Linux,
        making form non-full-screen (but borderless).
        For Windows, it is not a problem.
        Seems FMXLinux bug? Or at least inconsistency with FMX on Windows.
    }
    Form.BorderStyle := {$ifdef FPC} BorderNone {$else} TFmxFormBorderStyle.Sizeable {$endif};
    Form.WindowState := {$ifdef FPC} wsFullScreen {$else} TWindowState.wsNormal {$endif};
  end else
  begin
    if ResizeAllowed <> raAllowed then
      Form.BorderStyle := {$ifdef FPC} bsSingle {$else} TFmxFormBorderStyle.Single {$endif}
    else
      Form.BorderStyle := {$ifdef FPC} bsSizeable {$else} TFmxFormBorderStyle.Sizeable {$endif};
    Form.WindowState := {$ifdef FPC} wsNormal {$else} TWindowState.wsNormal {$endif};
  end;

  {$ifdef DELPHI}
  Form.FullScreen := FullScreen;
  {$endif}
end;

{$ifdef FPC}
procedure TCastleWindow.KeyPressHandlerPress(Sender: TObject;
  const Event: TInputPressRelease);
begin
  DoKeyDown(Event.Key, Event.KeyString);
end;
{$endif}

procedure TCastleWindow.UpdateFullScreenBackend;
begin
  if FFullScreenBackend <> FFullScreenWanted then
  begin
    FFullScreenBackend := FFullScreenWanted;
    if not Closed then
      UpdateFullScreenForm;
  end;
end;

procedure TCastleWindow.CloseBackend;
begin
  { Make freeing of Form and OpenGLControl independent.
    It seems that otherwise, when OpenGLControl.Parent is Form,
    freeing Form will also free OpenGLControl - at least on FMX Linux. }
  if OpenGLControl <> nil then
    OpenGLControl.Parent := nil;

  {$if not (defined(LCLCarbon) or defined(LCLCocoa))}
  FreeAndNil(Form);
  FreeAndNil(OpenGLControl);
  {$else}
  { Workaround LCL problem with both Carbon and Cocoa
    https://github.com/castle-engine/castle-engine/issues/237
    Freeing LCL form may fail with EAccessViolation. }
  Form := nil; // causing memory leak, but at least not crash
  OpenGLControl := nil;
  {$endif}

  { freeing the Form will automatically free other owned components }
  Menu := nil;

  { although Menu was already freed, but make sure we also change state,
    like TCastleWindow.MenuInitialized and clear TMenuItem.Handle. }
  MenuFinalize;

  {$ifdef FPC}
  FreeAndNil(FKeyPressHandler);
  {$endif}
end;

procedure TCastleWindow.SetCaption(const Part: TCaptionPart; const Value: string);
begin
  FCaption[Part] := Value;
  if not Closed then Form.Caption := GetWholeCaption;
end;

procedure TCastleWindow.BackendMakeCurrent;
begin
  OpenGLControl.MakeCurrent;
end;

procedure TCastleWindow.UpdateCursor;
begin
  { FPC (LCL) notes:

    - OpenGLControl.Cursor is better than Form.Cursor for this,
      hiding the cursor would not work on Carbon with Form.Cursor.

    Delphi (FMX) notes:

    - Setting Form.Cursor := .. no effect.
      Tested on Delphi/Linux and Delphi/Win64, with Delphi 11.3.

    - Using

      var
        CursorService: IFMXCursorService;
      begin
        if not TPlatformServices.Current.SupportsPlatformService(IFMXCursorService, CursorService) then
         Exit;
       CursorService.SetCursor(CursorFromCastle(InternalCursor));
      end;

      On Delphi/Linux with Delphi 11.3:
      It changes the cursor only temporarily -- until next mouse move.
      So we're not supposed to use this, as I understand -- FMX is internally
      I guess using IFMXCursorService to set the value from
      OpenGLControl.Cursor.

      On Delphi/Win64 with Delphi 11.3:
      Seems to do nothing.

    - OpenGLControl.Cursor := .. :

      Is reliable, on Delphi/Linux, with Delphi 11.3.

      Doesn't work Delphi/Win64 with Delphi 11.3.

      So really no solution works for Delphi/Win64 with Delphi 11.3...
      We just ignore it, as Windows users shall not use this backend,
      they shall use better CASTLE_WINDOW_WINAPI.
  }

  OpenGLControl.Cursor := CursorFromCastle(InternalCursor);
end;

procedure TCastleWindow.SetCursor(const Value: TMouseCursor);
begin
  if FCursor <> Value then
  begin
    FCursor := Value;
    if not Closed then
      UpdateCursor;
  end;
end;

function TCastleWindow.RedirectKeyDownToMenuClick: boolean;
begin
  Result := false;
end;

procedure TCastleWindow.SetMousePosition(const Value: TVector2);

  { Convert window position from the CGE convention to window system
    convention where (0,0) is left-top.
    This reverses LeftTopToCastle. }
  function CastleToLeftTop(const V: TVector2): TVector2;
  begin
    Result.X := V.X;
    Result.Y := FRealHeight / OpenGLControl.MousePosScale - 1 - V.Y;
  end;

var
  { LCL takes int mouse positions, Delphi (FMX) takes float mouse positions. }
  P: {$ifdef FPC} TVector2Integer {$else} TVector2 {$endif};
  Pt: {$ifdef FPC} TPoint {$else} TPointF {$endif};
begin
  if not Closed then
  begin

    { Do not set Mouse.CursorPos to the same value, to make sure we don't cause
      unnecessary OnMotion on some systems while actual MousePosition didn't change. }
    if TVector2.PerfectlyEquals(Value, FMousePosition) then Exit;

    //WritelnLog('New mouse position (CGE coordinates): %f %f', [Value.X, Value.Y]);

    P := {$ifdef FPC} CastleToLeftTopInt {$else} CastleToLeftTop {$endif}
      (Value / OpenGLControl.MousePosScale);

    {$ifdef FPC}
    { LCL version: use Mouse.CursorPos .}
    Pt := Point(P.X, P.Y);
    Pt := OpenGLControl.ControlToScreen(Pt);
    Mouse.CursorPos := Pt;

    {$else}
    { FMX version.
      It seems there's no cross-platform solution for this in FMX.
      It has IFMXMouseService but only with GetMousePos, not setter.
      So we just do platform-specific call in FmxSetMousePos,
      for now implemented only for Linux (GTK 3). }

    Pt := PointF(P.X, P.Y);
    //WritelnLog('New mouse position (OpenGL control coordinates): %f %f', [Pt.X, Pt.Y]);
    Pt := OpenGLControl.LocalToAbsolute(Pt);
    //WritelnLog('New mouse position (form coordinates): %f %f', [Pt.X, Pt.Y]);
    Pt := Form.ClientToScreen(Pt);
    //WritelnLog('New mouse position (screen coordinates): %f %f', [Pt.X, Pt.Y]);

    {$ifdef LINUX}
    if Form.Handle = nil then
    begin
      WritelnWarning('Cannot set mouse position, form has no Handle');
      Exit;
    end;
    if TLinuxWindowHandle(Form.Handle).NativeHandle = nil then
    begin
      WritelnWarning('Cannot set mouse position, form has no NativeHandle');
      Exit;
    end;
    FmxSetMousePos(TLinuxWindowHandle(Form.Handle).NativeHandle, Pt);
    {$else}
    WritelnWarning('Setting mouse position not implemented for this platform with Delphi and CASTLE_WINDOW_FORM');
    {$endif}

    {$endif}
  end;
end;

procedure TCastleWindow.MenuItemClick(Sender: TObject);
var
  Entry: TMenuEntry;
begin
  Entry := (Sender as TGoodMenuItem).Entry;
  if Entry is TMenuItem then
  begin
    { It seems that checked menu item is always automatically swapped,
      at least with LCL-GTK2. So manually revert it to original setting,
      as we don't want this (DoMenuClick will change Checked as necessary). }
    if Entry is TMenuItemChecked then
      MenuUpdateChecked(TMenuItemChecked(Entry));
    DoMenuClick(TMenuItem(Entry));
  end;
end;

procedure TCastleWindow.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  DoCloseQuery;
  CanClose := false; // DoCloseQuery will call Close when needed
end;

procedure TCastleWindow.FormDeactivate(Sender: TObject);
begin
  ReleaseAllKeysAndMouse;
end;

procedure TCastleWindow.OpenGLControlPaint(Sender: TObject);
begin
  DoRender;
end;

procedure TCastleWindow.OpenGLControlResize(Sender: TObject);
begin
  DoResize(
    OpenGLControl.PixelsWidth,
    OpenGLControl.PixelsHeight, false);

  (*
  TODO: The issue is now solved in a different way, to effectively
  always behave as if AutoRedisplay=true.
  It seems that calling either Invalidate or even DoRender from
  TCastleWindow.OpenGLControlResize is sometimes not effective -- the context
  doesn't get redrawn, even though logs show it is executed.

  Testcase: AutoRedisplay:=false in some
  TCastleWindow application *not* doing something in each update that
  already causes redisplay each frame (like updating FPS on label).
  Then maximize / restore it (e.g. by Alt+F10 in GNOME) repeatedly.
  On Delphi/Linux.

  {$ifndef FPC}
  { When using FMX TOpenGLControl, our Invalidate is not automatically
    called by the system when we should be redrawn.
    So we have to call it manually.

    Testcase this is needed: set AutoRedisplay:=false in some
    TCastleWindow application *not* doing something in each update that
    already causes redisplay each frame (like updating FPS on label).
    Then resize this window.
    Without this code, the window would not be redrawn to show new size.
    On Delphi/Linux.
  }

  Invalidate;

  //DoRender;
  {$endif}
  *)
end;

procedure TCastleWindow.OpenGLControlKeyDown(Sender: TObject;
  var Key: Word; {$ifndef FPC} var KeyChar: WideChar; {$endif}
  Shift: TShiftState);
{$ifdef FPC}
begin
  FKeyPressHandler.KeyDown(Key, Shift);
{$else}
var
  CastleKey: TKey;
  CastleKeyString: String;
begin
  FmxKeysToCastle(Key, KeyChar, Shift, CastleKey, CastleKeyString);

  if (CastleKey <> keyNone) or (CastleKeyString <> '') then
    DoKeyDown(CastleKey, CastleKeyString);
{$endif}
end;

{$ifdef FPC}
procedure TCastleWindow.OpenGLControlUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  FKeyPressHandler.UTF8KeyPress(UTF8Key);
end;
{$endif}

procedure TCastleWindow.OpenGLControlKeyUp(Sender: TObject; var Key: Word;
  {$ifndef FPC} var KeyChar: WideChar; {$endif}
  Shift: TShiftState);
var
  CastleKey: TKey;
  {$ifndef FPC}
  CastleKeyString: String;
  {$endif}
begin
  {$ifdef FPC}
  FKeyPressHandler.BeforeKeyUp(Key, Shift);
  CastleKey := KeyToCastle(Key, Shift);
  {$else}

  { Note that KeyUp seems to have additional issue with FMXLinux,
    not fixed in this code:

    When the key is held (so we expect to receive multiple KeyDown
    for one KeyUp), FMXLinux sends additional KeyUp before each KeyDown.
    So for code, it seems as if user is pressing and releasing the key.
    We cannot easily workaround it on CGE side.
    To track keys being held, we advise to check Pressed[keyXxx] in Update
    methods, instead of relying on KeyDown/KeyUp. }

  { In principle, FmxKeysToCastle does something similar to
    "CastleKey := KeyToCastle(Key, Shift)" in FPC.
    But we have to use it with Delphi, to apply some FMXLinux-specific fixes.
    We actually ignore the resulting CastleKeyString now. }

  FmxKeysToCastle(Key, KeyChar, Shift, CastleKey, CastleKeyString);
  {$endif}

  if CastleKey <> keyNone then
    DoKeyUp(CastleKey);
end;

procedure TCastleWindow.OpenGLControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState;
  X, Y: {$ifdef FPC} Integer {$else} Single {$endif});
var
  MyButton: TCastleMouseButton;
begin
  if MouseButtonToCastle(Button, MyButton) then
    DoMouseDown(MousePosToCastle(X, Y), MyButton, 0);
end;

procedure TCastleWindow.OpenGLControlMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState;
  X, Y: {$ifdef FPC} Integer {$else} Single {$endif});
var
  MyButton: TCastleMouseButton;
begin
  if MouseButtonToCastle(Button, MyButton) then
    DoMouseUp(MousePosToCastle(X, Y), MyButton, 0);
end;

procedure TCastleWindow.ProcessTasks;
begin
  { Use ApplicationProperties.LimitFPS (if non-zero, by default 100)
    to avoid performing too many updates per second. }
  if (ApplicationProperties.LimitFPS <= 0) or
     (Fps.UpdateStartTime.ElapsedTime > 1 / ApplicationProperties.LimitFPS) then
  begin
    { Note that calling here ApplicationProperties._Update
      may be too often, in case multiple TCastleWindow are open. }
    ApplicationProperties._Update;
    DoUpdate;

    {$ifdef FPC}

    { Manually using Invalidated flag to call DoRender
      doesn't seem needed with Delphi/Linux,
      so we don't do it to conserve resources.
      On Delphi/Linux, DoRender is called from OpenGLControlPaint.

      For FPC/LCL, we rely on own Invalidated tracking,
      and apply Invalidated here.
      Note that DoRender will clear the Invalidated flag
      (unless AutoRedisplay). }

    if Invalidated then
      DoRender;

    {$else}

    { Original intention: Implement AutoRedisplay,
      for FMX this is the only place causing render each frame.

      The "if AutoRedisplay then Invalidate;" done by TCastleWindow.DoRender
      doesn't have a desired effect on FMX (at least on Linux),
      it seems TOpenGLControl.Invalidate inside OnPaint is ignored.
      And we ignore own Window.Invalidated tracking with FMX.
      So we redo it here.

      Testcase: edit_test, but with TCastleEdit deliberately
      not doing VisibleChanged each frame.
      Without the code below: Window would not be redrawn there
      with blinking cursor, so it would ignore AutoRedisplay=true. }

    { TODO: Later intention: behave as if AutoRedisplay was always true.
      Reason: Only then we get proper redisplay on maximize / restore of the window
      with Delphi/Linux. }

    //if AutoRedisplay then
      Invalidate;
    {$endif}
  end;
end;

procedure TCastleWindow.Invalidate;
begin
  if not Closed then
    OpenGLControl.Invalidate;
end;

procedure TCastleWindow.OpenGLControlMouseMove(Sender: TObject;
  Shift: TShiftState;
  X, Y: {$ifdef FPC} Integer {$else} Single {$endif});
begin
  DoMotion(InputMotion(MousePosition, MousePosToCastle(X, Y),
    MousePressed, 0));

  {$ifdef FPC}
  { On LCL, we need to call ProcessTasks often,
    as Forms.Application.ProcessMessages in LCL may process ~100 OS events in one call.
    This means that LCL Forms.Application.ProcessMessages is quite blocked
    when user moves mouse a lot (e.g. doing mouse look in games).
    We ensure processing by manually calling ProcessTasks often.

    With Delphi FMX, we don't need to do this, and actually we shouldn't
    -- it was causing perceived FPS drops when we were moving the mouse.
    Testcase:
    - moving with mouse Examine mode in play_animation
    - rotating around in fps_game }
  ProcessTasks;
  {$endif}
end;

procedure TCastleWindow.OpenGLControlMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer;
  {$ifdef FPC} MousePos: TPoint; {$endif}
  var Handled: Boolean);
begin
  DoMouseWheel(WheelDelta/120, true);
end;

{ TCastleWindow dialogs ---------------------------------------------------------- }

function TCastleWindow.BackendFileDialog(const Title: string; var FileName: string;
  OpenDialog: boolean; FileFilters: TFileFilterList): boolean;
var
  // In FMX, TSaveDialog descends from TOpenDialog
  Dialog: {$ifdef FPC} TFileDialog {$else} TOpenDialog {$endif};
  ExpandedFileName: string;
begin
  if OpenDialog then
    Dialog := TOpenDialog.Create(nil)
  else
    Dialog := TSaveDialog.Create(nil);
  try
    Dialog.Title := Title;
    if FileName = '' then
      Dialog.InitialDir := GetCurrentDir
    else
    begin
      ExpandedFileName := ExpandFileName(FileName);
      Dialog.InitialDir := ExtractFilePath(ExpandedFileName);
      Dialog.FileName := ExtractFileName(ExpandedFileName);
    end;
    FileFiltersToDialog(FileFilters, Dialog);
    ReleaseAllKeysAndMouse; // do it manually
    Result := Dialog.Execute;
    if Result then
      FileName := Dialog.FileName;
  finally FreeAndNil(Dialog) end;
end;

function TCastleWindow.ColorDialog(var Color: TCastleColor): boolean;
{$ifdef FPC}
var
  Dialog: TColorDialog;
  ColorByte: TVector3Byte;
begin
  Dialog := TColorDialog.Create(nil);
  try
    ColorByte := Vector3Byte(Color.RGB);
    Dialog.Color := RGBToColor(ColorByte.X, ColorByte.Y, ColorByte.Z);
    ReleaseAllKeysAndMouse; // do it manually
    Result := Dialog.Execute;
    if Result then
    begin
      RedGreenBlue(Dialog.Color, ColorByte.X, ColorByte.Y, ColorByte.Z);
      Color := Vector4(Vector3(ColorByte), Color.W);
    end;
  finally FreeAndNil(Dialog) end;
{$else}
begin
  WritelnWarning('TODO: ColorDialog not implemented with TCastleWindow backend on FMX');
  Result := false;
{$endif}
end;

procedure TCastleWindow.MessageOK(const S: string; const MessageType: TWindowMessageType);
const
  MessageTypeCastleToLCL: array [TWindowMessageType] of TMsgDlgType = (
    TMsgDlgType.mtInformation,
    TMsgDlgType.mtWarning,
    TMsgDlgType.mtConfirmation,
    TMsgDlgType.mtError,
    TMsgDlgType.mtCustom
  );
begin
  ReleaseAllKeysAndMouse; // do it manually
  MessageDlg(S, MessageTypeCastleToLCL[MessageType], [TMsgDlgBtn.mbOK], 0);
end;

function TCastleWindow.MessageYesNo(const S: string;
  const MessageType: TWindowMessageType): boolean;
begin
  ReleaseAllKeysAndMouse; // do it manually
  Result := MessageDlg(S, TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes;
end;

procedure TCastleWindow.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  DoDropFiles(FileNames);
end;

procedure TCastleWindow.BackendInsideUpdate;
begin
  {$if defined(FPC)}
  FKeyPressHandler.Flush; // finish any pending key presses
  {$else}
  OpenGLControl.Update; // on FMX, we need to call TOpenGLControl.Update
  {$endif}
end;

function TCastleWindow.MousePosToCastle(
  const X, Y: {$ifdef FPC} Integer {$else} Single {$endif}): TVector2;
begin
  Result := LeftTopToCastle(
    X * OpenGLControl.MousePosScale,
    Y * OpenGLControl.MousePosScale);
end;

{ TFormsClipboard ----------------------------------------------------------- }

type
  { Easy API for LCL or FMX clipboard. }
  TFormsClipboard = class(TCastleClipboard)
  protected
    function GetAsText: string; override;
    procedure SetAsText(const Value: string); override;
  end;

function TFormsClipboard.GetAsText: string;
{$ifdef FPC}
begin
  Result := Clipbrd.Clipboard.AsText;
{$else}
var
  ClipboardService: IFMXClipboardService;
  Value: TValue;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, ClipboardService) then
  begin
    Value := ClipboardService.GetClipboard;
    if not Value.TryAsType(Result) then
      Result := '';
  end;
{$endif}
end;

procedure TFormsClipboard.SetAsText(const Value: string);
{$ifdef FPC}
begin
  Clipbrd.Clipboard.AsText := Value;
{$else}
var
  ClipboardService: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, ClipboardService) then
  begin
    ClipboardService.SetClipboard(Value);
  end;
{$endif}
end;

{ TCastleApplication ---------------------------------------------------------- }

procedure TCastleApplication.CreateBackend;
begin
  RegisterClipboard(TFormsClipboard.Create);

  {$ifdef FPC}
  { Do not install LCL handler for ExceptProc, we want to use our own handler
    in CastleUtils. }
  FormApplication.CaptureExceptions := false;
  {$else}
  { We tested assigning on Delphi FMX
      FormApplication.OnException := DoNothingOnException;
    to a callback that does nothing.
    But this was bad: exceptions still didn't reach TCastleWindow
    (testcase: deliberate exception from simple_3d_demo)
    and CtrlC handling was worse (first CtrlC was ignored -- though later tests
    suggest it can happen anyway, regardless of FormApplication.OnException).
    So don't tweak this for Delphi FMX. }
  {$endif}

  FormApplication.Initialize;

  {$ifdef USE_TIMER}
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 1;
  FTimer.OnTimer := {$ifdef FPC}@{$endif} ApplicationTimer;
  FTimer.Enabled := true;
  {$endif USE_TIMER}
end;

procedure TCastleApplication.DestroyBackend;
begin
  {$ifdef USE_TIMER}
  FreeAndNil(FTimer);
  {$endif}
end;

{$ifdef USE_TIMER}
procedure TCastleApplication.ApplicationTimer(Sender: TObject);
begin
  ApplicationProperties._Update;
  { Call Update events on all windows. }
  FOpenWindows.DoUpdate;
end;
{$endif}

function TCastleApplication.ProcessMessage(WaitForMessage, WaitToLimitFPS: boolean): boolean;

  function CheckTerminated: Boolean;
  begin
    Result :=
      { Check our "Terminated" (from TCustomApplication):

        - This way we honor "Application.Terminate" calls
          from CGE applications, like fps_game main menu "Quit" button.
          Which is necessary regardless of USE_TIMER.

        - And we honor "Terminate" calls done by TCastleApplication
          itself when last open window is closed.
      }
      Terminated
      or
      { It seems we don't need to check FormApplication.Terminated at all now.
        Unless some LCL widgetset sets FormApplication.Terminated directly,
        e.g. maybe Cocoa application menu "Quit" sets FormApplication.Terminated?

        Note that such widgetset would interrupt its own FormApplication.Run
        anyway. So us checking here "FormApplication.Terminated" matters only
        if
        1. USE_TIMER is not defined in this case,
        2. or user code uses CGE "Application.ProcessMessage" directly.
      }
      FormApplication.Terminated;
  end;

var
  I: Integer;
begin
  if CheckTerminated then
    Exit(false);

  { When not USE_TIMER, we cannot wait for message, as it would mean we
    can hang indefinitely. }
  {$ifndef USE_TIMER}
  WaitForMessage := false;
  {$endif}

  { TODO: take WaitToLimitFPS into account }

  if WaitForMessage then
  begin
    { TODO: We never call Fps.InternalSleeping, so Fps.WasSleeping
      will be always false.

      This may result in confusing Fps.ToString in case
      - AutoRedisplay is false (since otherwise we render each frame)
      - and USE_TIMER is defined (since otherwise we never have WaitForMessage=true)

      This doesn't seem to matter for any practical application of this. }

    { HandleMessage will call timer events, so no need for ProcessTasks
      hack in this case. }
    FormApplication.HandleMessage;
    Result := not CheckTerminated;
  end else
  begin
    FormApplication.ProcessMessages;
    Result := not CheckTerminated;

    {$ifndef USE_TIMER}
    if Result then
      ProcessTasks;
    {$endif}
  end;
end;

procedure TCastleApplication.ProcessTasks;
var
  I: Integer;
begin
  for I := 0 to OpenWindowsCount - 1 do
    OpenWindows[I].ProcessTasks;
end;

function TCastleApplication.ProcessAllMessages: boolean;
begin
//  Result := not FormApplication.Terminated;
//  while Result do
    Result := ProcessMessage(false, false);
end;

procedure TCastleApplication.Run;
begin
  if OpenWindowsCount = 0 then Exit;

  {$ifdef USE_TIMER}
  { LCL Cocoa really must call here FormApplication.Run.

    It cannot just call our ProcessMessage
    (which makes FormApplication.ProcessMessage) in a loop.
    That is because on some LCL widgetsets, you really need to run
    FormApplication.Run, see e.g. Carbon that does special stuff inside
    TCarbonWidgetSet.AppRun inside lcl/interfaces/carbon/carbonobject.inc.
    Without this, global menu on Carbon is always blocked.

    This causes necessary Cocoa problems:

    - Our TCastleWindow interface
      promises that it's not necessary to call Run, it should be always
      equivalent to call just ProcessMessage in a loop.
      For Cocoa with CASTLE_WINDOW_FORM, this cannot be true.

    Other platforms with USE_TIMER call FormApplication.Run here
    as this is most straightforward.
    It maps to AppService.Run in FMX, so specific platforms
    may benefit from having FormApplication.Run call instead of loop
    over ProcessMessage.
  }
  FormApplication.Run;
  {$else}
  while ProcessMessage(false, false) do ;
  {$endif}
end;

procedure TCastleApplication.BackendTerminate;
begin
  {$ifdef USE_TIMER}

  { TODO: This line makes sense, for USE_TIMER,
    to break FormApplication.Run when user code runs "Application.Terminate".
    Or when last window is closed (by Alt+F4,
    or clicking on "X" button on window frame).
    Testcase: "fps_game" "Quit" menu button, or "fps_game" click on "X" button.

    But it causes crash with "fps_game" "Quit" menu button
    -- reproduced with Delphi 11.3 on Win32 and Win64,
    when compiled with CASTLE_WINDOW_FORM for Windows.
    Backtrace uncertain, stack trace seems somewhat currupted.
    Maybe this fails when called from CloseAllOpenWindows,
    maybe from TCastleApplication destructor,
    but workaround by "if not (csDestroying in ComponentState)"
    doesn't help. }
  FormApplication.Terminate; // break also FormApplication.Run

  {$endif}
end;

function TCastleApplication.ScreenWidth: integer;
begin
  { TODO: On FMX (Delphi) and Windows, this doesn't return correct size in pixels.
    We tried, but failed, to fix it:
    See castlewindow_form_fmx_scaling.md }

  Result := Round(Screen.Width);  // Round for FMX
end;

function TCastleApplication.ScreenHeight: integer;
begin
  Result := Round(Screen.Height); // Round for FMX
end;

function TCastleApplication.ScreenStatusBarScaledHeight: Cardinal;
begin
  Result := 0;
end;

function TCastleApplication.BackendName: string;
begin
  Result := 'Form';
end;

{ TWindowContainer ----------------------------------------------------------- }

function TWindowContainer.SettingMousePositionCausesMotion: Boolean;
begin
  { Delphi/Linux, using
    https://docs.gtk.org/gdk3/method.Device.warp.html for SetMousePosition,
    says

    """
    Warping the pointer creates events as if the user had moved
    the mouse instantaneously to the destination.
    """

    So it does generate Motion events.
  }
  Result := true;
end;

{$endif read_implementation}
