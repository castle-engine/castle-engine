{%MainUnit castlewindow.pas}
{
  Copyright 2013-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

(*CastleWindow backend using Lazarus LCL or Delphi FMX underneath.

  Should work with all platforms,
  all LCL widgetsets,
  all Delphi FMX platforms.

  TODO:
  - LCL: Mouse look still stutters awfully on both Carbon and GTK2.
    AggressiveUpdate helps (without it, mouse look would not work at all)
    but doesn't completely fix the problem. Other CastleWindow backends
    have smooth mouse look.

  - LCL: We never call Fps.InternalSleeping, so Fps.WasSleeping will be always false.
    This may result in confusing Fps.ToString in case AutoRedisplay was false.

  LCL: Bugs on GTK2, not on Carbon:
  - All radio menu items are always on. Why?

  LCL: Bugs on Carbon, untested elsewhere:
  - When you switch FullScreen, at application exit there is an access violation
    in LCL. Seems harmless, so we just ignore it for now.
*)

{ Define to use TTimer to call Update continuously.

  Reason for Delphi: It is reliable. All our Delphi FMX and VCL components
  use TTimer like this.

  Reason for LCL Carbon: Using LCL timer (that is translated to Carbon timer)
  is the only way to receive continuous messages in some cases.

    Otherwise such bugs can be observed (only on LCL-Carbon, not on LCL-GTK2):

    - When trying to Walk using mouse dragging in view3dscene, it doesn't
      work smoothly: when you simply keep some mouse button pressed,
      but don't move the mouse anymore (which should result in constant
      movement forward/backward if you did it after dragging up/down),
      then we don't get any events (so no movement, no redraw...).

    - Using view3dscene Navigation->Jump to viewpoint... doesn't work
      smoothly, there is a visible delay when we don't receive messages
      right after menu click, and so transtion is visible with some delay.
}
{$if defined(LCLCarbon) or not defined(FPC)}
  {$define USE_TIMER}
{$endif}

{$ifdef read_interface_uses}
  {$ifdef FPC}
    // LCL
    Interfaces, Forms, Dialogs, OpenGLContext, Menus,
    Controls, FileUtil, Graphics, LCLType, CastleLCLUtils, Clipbrd, CustomTimer,
    LCLVersion,
  {$else}
    UITypes, Types, Rtti,
    // FMX
    FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
    FMX.StdCtrls, FMX.Menus, FMX.Platform,
    // CGE
    Fmx.CastleInternalGLControl, CastleInternalDelphiUtils,
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
      // FMX calls this "Text", alias to "Caption" to be easier to handle - like in LCL
      property Caption: String read GetCaption write SetCaption;
      {$endif}
    end;

    { Like TOpenGLControl but sets Invalidated:=true on Window,
      so that Window knows when it's invalidated by the OS. }
    TGoodOpenGLControl = class(TOpenGLControl)
    public
      Window: TCastleWindow;
      procedure Invalidate; override;
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
  procedure AggressiveUpdate;
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
{$endif read_window_interface}

{$ifdef read_application_interface}
private
  {$ifdef USE_TIMER}
  FTimer: TTimer;
  procedure ApplicationTimer(Sender: TObject);
  {$else}
  procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
  {$endif}
  procedure DoNothingOnException(Sender: TObject; E: Exception);
{$endif read_application_interface}

{$ifdef read_implementation}

const
  MaxDesiredFPS = TCastleApplicationProperties.DefaultLimitFPS;

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
  Text := Caption;
end;
{$endif}

{ TGoodOpenGLControl --------------------------------------------------------- }

procedure TCastleWindow.TGoodOpenGLControl.Invalidate;

{ Note that in FMX, this is not called by the system.
  So this is worthless to catch system invalidate events
  (TODO: we should somehow catch system invalidate).
  But it is still useful as TCastleWindow.Invalidate sets
  Window.Invalidated := true indirectly, through this. }

begin
  // track our own Invalidated state, although still cooperate with LCL invalidate
  Window.Invalidated := true;
  inherited;
end;

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
    {$ifdef FPC}
    AMenu.Add(NewMenuChild);
    {$else}
    NewMenuChild.Parent := AMenu;
    {$endif}
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
    NewMenuChild.Parent := AMenu;
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
var
  LazKey: Word;
  Shift: TShiftState;
  FormMenuItem: TGoodMenuItem;
begin
  if not CheckMenu(Entry, FormMenuItem) then Exit;

  {$ifdef LCLCarbon}
  { Workaround yet another bug in LCL Carbon.
    When a menu item is disabled (because Entry.Enabled is false or
    MainMenu.Enabled is false), it turns out we have to disable it's shortcut.
    Otherwise pressing this shortcut can be captured and cause strange behavior
    later.

    view3dscene bugs (when this workaround not present) on macOS:
    - Enter "Open Link" with Command+L. Press Command+O (that leads to disabled
      menu item, and doesn't do anything!). Nothing happens, dummy letter with
      Ord(CtrlO) is inserted, that's Ok. Then press Escape. And the dialog appears
      again... Somehow, the Command+O (pressed when it was disabled!) was captured,
      with delay, and caused weird double menu item activation.
    - Similarly, after Command+R, press Command+O before escaping.
      It causes the raytracing dialog to appear again after escaping.
  }
  if not FormMenuItem.Enabled then
  begin
    FormMenuItem.ShortCut := 0;
    Exit;
  end;
  {$endif}

  // TODO: fmx
  {$ifdef FPC}
  KeyCastleToLCL(Entry.Key, Entry.KeyString, Entry.Modifiers, LazKey, Shift);
  FormMenuItem.ShortCut := KeyToShortCut(LazKey, Shift);
  {$endif}
end;

procedure TCastleWindow.MenuUpdateCaption(Entry: TMenuEntryWithCaption);

  function MenuCaptionCastleToLCL(const S: string): string;
  begin
    { We use _ for the same purpose as LCL uses &, to mark mnemonics.
      But we cannot just replace _ with &:
      Double underscore should be converted to single underscore.
      Only unpaired single underscore should be converted to &.
      Our SReplacePatterns guarantees correct behavior. }
    Result := SReplacePatterns(S, ['__', '_'], ['_', '&'], false);
  end;

var
  FormMenuItem: TGoodMenuItem;
begin
  if not CheckMenu(Entry, FormMenuItem) then Exit;
  FormMenuItem.Caption := MenuCaptionCastleToLCL(Entry.Caption);
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

    This all could be considered Carbon's bug, anyway we have to work with it.
  }

  if Entry = MainMenu then
  begin
    RefreshEnabled(MainMenu);
    Exit;
  end;

  if not CheckMenu(Entry, FormMenuItem) then Exit;
  FormMenuItem.Enabled := Entry.Enabled and MainMenu.Enabled;

  {$ifdef LCLCarbon}
  if Entry is TMenuItem then
    MenuUpdateShortcut(TMenuItem(Entry));
  {$endif}
end;

procedure TCastleWindow.MenuUpdateChecked(Entry: TMenuItemChecked);
var
  FormMenuItem: TGoodMenuItem;
begin
  if not CheckMenu(Entry, FormMenuItem) then Exit;
  {$ifdef FPC} // FMX: TODO
  FormMenuItem.Checked := Entry.Checked;
  {$endif}
end;

function TCastleWindow.MenuUpdateCheckedFast: boolean;
begin
  Result := true;
end;

procedure TCastleWindow.MenuInsert(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
var
  ParentFormMenu, NewFormMenu: TGoodMenuItem;
begin
  if not CheckMenu(Parent, ParentFormMenu) then Exit;
  NewFormMenu := MenuEntryConvert(Entry);
  {$ifdef FPC}
  ParentFormMenu.Insert(ParentPosition, NewFormMenu);
  {$else}
  NewFormMenu.Parent := ParentFormMenu;
  {$endif}
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

  procedure GetInitialCursorPos;
  var
    InitialCursorPos: TPoint;
  begin
    {$ifdef FPC}
    // FMX: TODO
    InitialCursorPos := OpenGLControl.ScreenToControl(Mouse.CursorPos);
    FMousePosition := LeftTopToCastle(InitialCursorPos.X, InitialCursorPos.Y);
    {$endif}
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
  { We would prefer to leave Form.AutoSize := true and allow form to adjust
    to OpenGLControl.Width/Height. But it doesn't work. }
  Form.Width := Width;
  Form.Height := Height;
  Form.Caption := GetWholeCaption; // SetCaption may also change it later
  Form.Cursor := CursorFromCastle(InternalCursor); // SetCursor may also change it later

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

  Form.ActiveControl := OpenGLControl;

  if (MainMenu <> nil) and MainMenuVisible then
    MenuInitialize;

  Form.Visible := Visible;

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
    Round(OpenGLControl.Width),
    Round(OpenGLControl.Height), false);  // Round for FMX

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

  GetInitialCursorPos;

  {$ifdef FPC}
  Container.Dpi := Screen.PixelsPerInch;
  {$endif}
end;

const
  BorderNone = {$ifdef FPC} Controls.bsNone {$else} TFmxFormBorderStyle.None {$endif};

procedure TCastleWindow.UpdateFullScreenForm;
begin
  if FullScreen then
  begin
    Form.BorderStyle := BorderNone;
    {$ifdef FPC} // TODO: FMX
    Form.WindowState := wsFullScreen;
    {$endif}
  end else
  begin
    if ResizeAllowed <> raAllowed then
      Form.BorderStyle := {$ifdef FPC} bsSingle {$else} TFmxFormBorderStyle.Single {$endif}
    else
      Form.BorderStyle := {$ifdef FPC} bsSizeable {$else} TFmxFormBorderStyle.Sizeable {$endif};
    Form.WindowState := {$ifdef FPC} wsNormal {$else} TWindowState.wsNormal {$endif};
  end;
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
  { Workaround LCL problem with both Carbon and Cocoa
    https://github.com/castle-engine/castle-engine/issues/237
    Freeing LCL form may fail with EAccessViolation. }
  {$if not (defined(LCLCarbon) or defined(LCLCocoa))}
  FreeAndNil(Form);
  {$else}
  Form := nil; // causing memory leak, but at least not crash
  {$endif}

  // FreeAndNil(OpenGLControl);
  // TODO: seems freeing the form has already freed OpenGLControl, but why?

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

procedure TCastleWindow.SetCursor(const Value: TMouseCursor);
begin
  if FCursor <> Value then
  begin
    FCursor := Value;
    if not Closed then
      { Do not use Form.Cursor for this, e.g. hiding the cursor would not work
        then on Carbon. }
      OpenGLControl.Cursor := CursorFromCastle(Value);
  end;
end;

function TCastleWindow.RedirectKeyDownToMenuClick: boolean;
begin
  Result := false;
end;

procedure TCastleWindow.SetMousePosition(const Value: TVector2);
var
  P: TVector2Integer;
begin
  if not Closed then
  begin

    { Do not set Mouse.CursorPos to the same value, to make sure we don't cause
      unnecessary OnMotion on some systems while actual MousePosition didn't change. }
    if TVector2.PerfectlyEquals(Value, FMousePosition) then Exit;

    P := CastleToLeftTopInt(Value);
    // TODO: FMX
    {$ifdef FPC}
    Mouse.CursorPos := OpenGLControl.ControlToScreen(Point(P.X, P.Y));
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
    Round(OpenGLControl.Width),
    Round(OpenGLControl.Height), false);  // Round for FMX
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
  CastleKey := KeyToCastle(Key, Shift);

  if KeyChar <> #0 then
    CastleKeyString := KeyChar
  else
    CastleKeyString := '';

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
begin
  {$ifdef FPC}
  FKeyPressHandler.BeforeKeyUp(Key, Shift);
  {$endif}
  CastleKey := KeyToCastle(Key, Shift);
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
    DoMouseDown(LeftTopToCastle(X, Y), MyButton, 0);
end;

procedure TCastleWindow.OpenGLControlMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState;
  X, Y: {$ifdef FPC} Integer {$else} Single {$endif});
var
  MyButton: TCastleMouseButton;
begin
  if MouseButtonToCastle(Button, MyButton) then
    DoMouseUp(LeftTopToCastle(X, Y), MyButton, 0);
end;

{ Since LCL event loop may become clogged when dragging with mouse
  (like when using mouse look), force doing update and (if needed) redraw
  from time to time.
  See TCastleControl implementation for analogous AggressiveUpdate
  procedure with detailed discussion about why this is needed. }
procedure TCastleWindow.AggressiveUpdate;
var
  DesiredFPS: Single;
begin
  if ApplicationProperties.LimitFPS <= 0 then
    DesiredFPS := MaxDesiredFPS
  else
    DesiredFPS := Min(MaxDesiredFPS, ApplicationProperties.LimitFPS);
  if TimerSeconds(Timer, Fps.UpdateStartTime) > 1 / DesiredFPS then
  begin
    DoUpdate;

    { TODO:
      - TCastleApplication.OnIdle is not taken care of here.
        This takes care of calling OnUpdate for this window.
        Other windows (in case you have multiple TCastleWindow instances)
        will take care of themselves too, they all have separate Fps counter.
        But TCastleApplication.OnIdle is left not called.
      - OnTimer (both from Application and TCastleWindow)
        is not taken care of here.
    }

    if Invalidated then DoRender;
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
  DoMotion(InputMotion(MousePosition, LeftTopToCastle(X, Y),
    MousePressed, 0));
  AggressiveUpdate;
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
    {$ifdef FPC} // TODO: FMX
    FileFiltersToDialog(FileFilters, Dialog);
    {$endif}
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

{ TFormsClipboard ----------------------------------------------------------- }

type
  { Easy API for LCL or FMX clipboard. }
  TFormsClipboard = class(TCastleClipboard)
  protected
    function GetAsText: string; override;
    procedure SetAsText(const Value: string); override;
  end;

{$ifndef FPC}
{ FMX clipboard handling thanks to
  https://blog.dummzeuch.de/2015/02/28/accessing-the-clipboard-in-a-firemonkey-application/
  https://blogs.embarcadero.com/multi-device-apps-and-clipboard-support/
}
function TryGetClipboardService(out Clp: IFMXClipboardService): Boolean;
begin
  Result := TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService);
  if Result then
    Clp := IFMXClipboardService(TPlatformServices.Current.GetPlatformService(IFMXClipboardService));
end;
{$endif}

function TFormsClipboard.GetAsText: string;
{$ifdef FPC}
begin
  Result := Clipbrd.Clipboard.AsText;
{$else}
var
  Clp: IFMXClipboardService;
  Value: TValue;
begin
  if TryGetClipboardService(Clp) then
  begin
    Value := Clp.GetClipboard;
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
  Clp: IFMXClipboardService;
begin
  if TryGetClipboardService(Clp) then
  begin
    Clp.SetClipboard(Value);
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
  { Do not capture exceptions in our ProcessMessages (catched in FMX
    TApplication.HandleException). Let exceptions happen,
    code in TCastleWindow (unrelated to backend) will catch them
    and display nice window. }
  FormApplication.OnException := DoNothingOnException;
  {$endif}

  FormApplication.Initialize;

  {$ifdef USE_TIMER}
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 1;
  FTimer.OnTimer := {$ifdef FPC}@{$endif} ApplicationTimer;
  FTimer.Enabled := true;
  {$else}
  FormApplication.AddOnIdleHandler(@ApplicationIdle);
  {$endif USE_TIMER}
end;

procedure TCastleApplication.DestroyBackend;
begin
  {$ifdef USE_TIMER}
  FreeAndNil(FTimer);
  {$else}
  FormApplication.RemoveOnIdleHandler(@ApplicationIdle);
  {$endif}
end;

procedure TCastleApplication.DoNothingOnException(Sender: TObject; E: Exception);
begin
  // deliberately do nothing
  WritelnLog('Exception in TForm event, passing up to TCastleWindow: ' + ExceptMessage(E));
end;

{$ifdef USE_TIMER}
procedure TCastleApplication.ApplicationTimer(Sender: TObject);
{$else}
procedure TCastleApplication.ApplicationIdle(Sender: TObject; var Done: Boolean);
{$endif}
begin
  { Call Update and Timer events of Application and all windows.
    Update events are handled by DoApplicationUpdate and OpenWindows.DoUpdate.
    Timer events are handled by MaybeDoTimer, that in turn will call
    (when needed) DoApplicationTimer and OpenWindows.DoTimer. }
  DoApplicationUpdate;
  FOpenWindows.DoUpdate;
  MaybeDoTimer;

  {$ifndef USE_TIMER}
  { We need Done := false, otherwise the widgetset will hang waiting
    for next message. See ../lcl/castlecontrol.pas comments.

    Done := false also prevents other application idle handlers,
    but fortunately in this case we're the only thing interfacing with LCL,
    so that's not a problem. }
  Done := false;
  {$endif}
end;

function TCastleApplication.ProcessMessage(WaitForMessage, WaitToLimitFPS: boolean): boolean;
var
  I: Integer;
begin
  if FormApplication.Terminated then
    Exit(false);

  { TODO: take WaitToLimitFPS into account }

  if WaitForMessage then
  begin
    { HandleMessage will call ApplicationIdle, so no need for AggressiveUpdate
      hack in this case. }
    FormApplication.HandleMessage;
    Result := not FormApplication.Terminated;
  end else
  begin
    FormApplication.ProcessMessages;
    Result := not FormApplication.Terminated;

    { Without this, OnUpdate will not be called when application calls
      TCastleApplication.ProcessMessage in a loop, because LCL will not make
      ApplicationIdle calls from FormApplication.ProcessMessages.

      Bugs observed when the below workaround is not used (on both LCL-Carbon and LCL-GTK2):
      - Arrow keys up/down do not work in CastleMessages window.
        E.g. view3dscene "OpenGL info".
        That's becaue UpdateMessg is never called.
        (Although this is now fixed anyway by above HandleMessage usage.)
      - Progress is not rendered smoothly.
        E.g. in fps_game, define -dTESTING_PROGRESS_DELAY, and notice that progress
        is not smoothly updated (if you don't move the mouse, since MouseMove
        already causes AggressiveUpdate).
    }
    if Result then
    begin
      for I := 0 to OpenWindowsCount - 1 do
        OpenWindows[I].AggressiveUpdate;
    end;
  end;
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

  { Implementing Run by calling ProcessMessage in a loop (like below)
    is a valid and good implementation. Make sure your ProcessMessage
    honours WaitForMessage and WaitToLimitFPS = true,
    to avoid wasting CPU on "busy waiting". }
  // while ProcessMessage(true, true) do ;

  { We have to call here FormApplication.Run, not just call our
    ProcessMessage (which makes FormApplication.ProcessMessage) in a loop.
    That is because on some LCL widgetsets, you really need to run
    FormApplication.Run, see e.g. Carbon that does special stuff inside
    TCarbonWidgetSet.AppRun inside lcl/interfaces/carbon/carbonobject.inc.
    Without this, global menu on Carbon is always blocked.

    TODO: that's unfortunate, as our TCastleWindow interface
    promises that it's not necessary to call Run, it should be always
    equivalent to call just ProcessMessage in a loop.

    TODO: this makes Application.OnUpdate/Application.OnTimer broken,
    they need our TCastleApplication.ProcessMessage to work. }

  FormApplication.Run;
end;

procedure TCastleApplication.QuitWhenNoOpenWindows;
begin
  FormApplication.Terminate; // break also FormApplication.Run
end;

function TCastleApplication.ScreenWidth: integer;
begin
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
  Result := 'LCL';
end;

{ TWindowContainer ----------------------------------------------------------- }

function TWindowContainer.SettingMousePositionCausesMotion: Boolean;
begin
  Result := true;
end;

{$endif read_implementation}
