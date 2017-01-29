{
  Copyright 2003-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Helpers for making modal boxes (TGLMode, TGLModeFrozenScreen)
  cooperating with the TCastleWindowCustom windows.
  They allow to easily save / restore TCastleWindowCustom attributes.

  This unit is a tool for creating functions like
  @link(CastleMessages.MessageOK). To make nice "modal" box,
  you want to temporarily replace TCastleWindowCustom callbacks with your own,
  call Application.ProcessMessage method in a loop until user gives an answer,
  and restore everything. This way you can implement functions that
  wait for some keypress, or wait until user inputs some
  string, or wait until user picks something with mouse,
  or wait for 10 seconds displaying some animation, etc. }
unit CastleWindowModes;

{$I castleconf.inc}

interface

uses SysUtils, Classes, CastleWindow, CastleGLUtils, CastleImages,
  CastleUIControls, CastleKeysMouse, CastleGLImages, CastleControls;

type
  { Enter / exit modal state on a TCastleWindowCustom.
    Saves/restores the state of TCastleWindowCustom. }
  TGLMode = class
  strict private
    OldWidth, OldHeight: integer;
    FFakeMouseDown: boolean;
  strict protected
    type
      TWindowState = class(TComponent)
      strict private
        Window: TCastleWindowCustom;
        OldMotion: TInputMotionEvent;
        OldPress, OldRelease: TInputPressReleaseEvent;
        OldOpenObject, OldCloseObject: TContainerObjectEvent;
        OldBeforeRender, OldRender, OldCloseQuery, OldUpdate, OldTimer: TContainerEvent;
        {$warnings off} // knowingly looking at deprecated RenderStyle, to keep it working
        OldRenderStyle: TRenderStyle;
        {$warnings on}
        OldResize: TContainerEvent;
        OldMenuClick: TMenuClickFunc;
        OldCaption: string;
        OldUserdata: Pointer;
        OldAutoRedisplay: boolean;
        OldMainMenu: TMenu;
        { This is the saved value of OldMainMenu.Enabled.
          So that you can change MainMenu.Enabled without changing MainMenu
          and SetWindowState will restore this. }
        OldMainMenuEnabled: boolean;
        OldCursor: TMouseCursor;
        OldCustomCursor: TRGBAlphaImage;
        OldSwapFullScreen_Key: TKey;
        OldClose_charkey: char;
        OldControls: TChildrenControls;
        OldAutomaticTouchControl: boolean;
        procedure WindowOpen(Container: TUIContainer);
        procedure WindowClose(Container: TUIContainer);
      public
        { When adding new attributes to TCastleWindowCustom that should be saved/restored,
          you must remember to
          1. expand this class with new fields
          2. expand constructor, destructor and SetStandardState } { }

        procedure SetStandardState(
          NewRender, NewResize, NewCloseQuery: TContainerEvent);

        { Constructor saves the TCastleWindowCustom state, destructor applies this state
          back to the window.
          Every property that can change when TCastleWindowCustom is open are saved.
          This way you can save/restore TCastleWindowCustom state, you can also copy
          a state from one window into another.

          Notes about TCastleWindowCustom.MainMenu saving: only the reference
          to MainMenu is stored. So:

          @unorderedList(
            @item(If you use TCastleWindowCustom.MainMenu,
              be careful when copying it to another window (no two windows
              may own the same MainMenu instance at the same time;
              also, you would have to make sure MainMenu instance will not be
              freed two times).)

            @item(Do not change the MainMenu contents
              during TGLMode.Create/Free. Although you can change MainMenu
              to something completely different. Just keep the assumption
              that MainMenu stays <> nil.)

            @item(As an exception to the previous point, you can freely
              change MainMenu.Enabled, that is saved specially for this.)
          )
        }
        constructor Create(AWindow: TCastleWindowCustom); reintroduce;
        destructor Destroy; override;
      end;
    var
    OldState: TWindowState;
    Window: TCastleWindowCustom;
    DisabledContextOpenClose: boolean;
  public
    { Constructor saves open TCastleWindowCustom and OpenGL state.
      Destructor will restore them.

      Some gory details (that you will usually not care about...
      the point is: everything works sensibly of the box) :

      @unorderedList(
        @item(We save/restore window state.)

        @item(OpenGL context connected to this window is also made current
          during constructor and destructor. Also, TCastleWindowCustom.Invalidate
          is called (since new callbacks, as well as original callbacks,
          probably want to redraw window contents.))

        @item(
          All pressed keys and mouse butons are saved and faked to be released,
          by calling TCastleWindowCustom.EventRelease with original
          callbacks.
          This way, if user releases some keys/mouse inside modal box,
          your original TCastleWindowCustom callbacks will not miss this fact.
          This way e.g. user scripts in VRML/X3D worlds that observe keys
          work fine.

          If FakeMouseDown then at destruction (after restoring original
          callbacks) we will also notify your original callbacks that
          user pressed these buttons (by sending TCastleWindowCustom.EventMouseDown).
          Note that FakeMouseDown feature turned out to be usually more
          troublesome than  usefull --- too often some unwanted MouseDown
          event was caused by this mechanism.
          That's because if original callbacks do something in MouseDown (like
          e.g. activate some click) then you don't want to generate
          fake MouseDown by TGLMode.Destroy.
          So the default value of FakeMouseDown is @false.
          But this means that original callbacks have to be careful
          and @italic(never assume) that when some button is pressed
          (because it's included in MousePressed, or has EventRelease generated for it)
          then for sure there occurred some MouseDown for it.
        )

        @item(At destructor, we notify original callbacks about size changes
          by sending TCastleWindowCustom.EventResize. This way your original callbacks
          know about size changes, and can set OpenGL projection etc.)

        @item(
          We call ZeroNextSecondsPassed at the end, when closing our mode,
          see TFramesPerSecond.ZeroNextSecondsPassed for comments why this is needed.)

        @item(This also performs important optimization to avoid closing /
          reinitializing window TCastleWindowCustom.Controls OpenGL resources,
          see TUIControl.DisableContextOpenClose.)
      ) }
    constructor Create(AWindow: TCastleWindowCustom);

    { Save TCastleWindowCustom state, and then change it to a standard
      state. Destructor will restore saved state.

      For most properties, we simply reset them to some sensible default
      values. For some important properties, we take their value
      explicitly by parameter.

      Window properties resetted:

      @unorderedList(
        @item(All callbacks (OnXxx) are set to @nil.

          Except the open/close callbacks
          (OnOpen and OnClose, OnOpenObject and OnCloseObject).
          Actually, OnOpenObject and OnCloseObject are changed for internal purposes,
          but, assuming you use SetStandardState, the orignal ones will still happen.
          Global CastleUIControls.OnGLContextOpen, CastleUIControls.OnGLContextClose
          are also untouched.

          @unorderedList(
            @item(On standalone, we can expect that the window
              (and OpenGL context) will stay open during the lifetime of a single
              TGLMode. So it doesn't really matter what we do with callbacks
              OnOpen / OnClose.)
            @item(On mobiles (Android) this is not necessarily true.
              Window may get closed at any time.
              So be extra careful when implementing OnOpen / OnClose callbacks,
              remember that they may happen when we're inside a mode (for example
              inside a modal message in CastleMessages or a progress bar).)
          )
        )
        @item(TCastleWindowCustom.Caption and TCastleWindowCustom.MainMenu are left as they were.)
        @item(TCastleWindowCustom.Cursor is reset to mcDefault.)
        @item(TCastleWindowCustom.UserData is reset to @nil.)
        @item(TCastleWindowCustom.AutoRedisplay is reset to @false.)
        @item(TCastleWindowCustom.RenderStyle is reset to rs2D.)
        @item(TCastleWindowCustom.MainMenu.Enabled will be reset to @false (only if MainMenu <> nil).)

        @item(TCastleWindowDemo.SwapFullScreen_Key will be reset to K_None.)
        @item(TCastleWindowDemo.Close_charkey will be reset to #0.)

        @item(All TCastleWindowCustom.Controls are temporarily removed.)
      )

      If you're looking for a suitable callback to pass as NewCloseQuery
      (new TCastleWindowCustom.OnCloseQuery), @@NoClose may be suitable:
      it's an empty callback, thus using it disables the possibility
      to close the window by window manager
      (usually using "close" button in some window corner or Alt+F4). }
    constructor CreateReset(AWindow: TCastleWindowCustom;
      NewRender, NewResize, NewCloseQuery: TContainerEvent);

    destructor Destroy; override;

    property FakeMouseDown: boolean
      read FFakeMouseDown write FFakeMouseDown default false;
  end;

  { Enter / exit modal box on a TCastleWindowCustom, additionally saving the screen
    contents before entering modal box. This is nice if you want to wait
    for some event (like pressing a key), keeping the same screen
    displayed.

    During this lifetime, we set special TCastleWindowCustom.OnRender and TCastleWindowCustom.OnResize
    to draw the saved image in a simplest 2D OpenGL projection. }
  TGLModeFrozenScreen = class(TGLMode)
  private
    SaveScreenControl: TCastleImageControl;
    ErrorBackground: TErrorBackground;
  public
    constructor Create(AWindow: TCastleWindowCustom);
    destructor Destroy; override;
  end;

{ Empty TCastleWindowCustom callback, useful as TCastleWindowCustom.OnCloseQuery
  to disallow closing the window by user. }
procedure NoClose(Container: TUIContainer);

implementation

uses CastleUtils, CastleWindowTouch;

{ TGLMode.TWindowState -------------------------------------------------------------- }

constructor TGLMode.TWindowState.Create(AWindow: TCastleWindowCustom);
begin
  inherited Create(nil);
  Window := AWindow;

  OldOpenObject := Window.OnOpenObject;
  OldCloseObject := Window.OnCloseObject;
  { Note that we do not touch OnOpen and OnClose. Let them happen.
    Our WindowOpen/Close will also call origina OnOpenObject/Close. }
  OldMotion := Window.OnMotion;
  OldPress := Window.OnPress;
  OldRelease := Window.OnRelease;
  OldBeforeRender := Window.OnBeforeRender;
  OldRender := Window.OnRender;
  {$warnings off} // knowingly looking at deprecated RenderStyle, to keep it working
  OldRenderStyle := Window.RenderStyle;
  {$warnings on}
  OldCloseQuery := Window.OnCloseQuery;
  OldResize := Window.OnResize;
  OldUpdate := Window.OnUpdate;
  OldTimer := Window.OnTimer;
  OldMenuClick := Window.OnMenuClick;
  oldCaption := Window.Caption;
  oldUserdata := Window.Userdata;
  oldAutoRedisplay := Window.AutoRedisplay;
  oldMainMenu := Window.MainMenu;
  if Window.MainMenu <> nil then
    oldMainMenuEnabled := Window.MainMenu.Enabled;
  OldCursor := Window.InternalCursor;
  OldCustomCursor := Window.CustomCursor;
  oldSwapFullScreen_Key := Window.SwapFullScreen_Key;
  oldClose_charkey := Window.Close_charkey;

  OldControls := TChildrenControls.Create(nil);
  OldControls.Assign(Window.Controls);
  OldControls.BeginDisableContextOpenClose;

  { save AutomaticTouchInterface,
    as it has to be reset in SetStandardState,
    as adding to Controls during a mode doesn't work (Controls contain
    only a temporary list of controls e.g. for progress bar UI),
    so TouchInterface (e.g. when camera changes to nil,
    then to non-nil, during SceneManager.LoadLevel) should remain unchanged. }
  if Window is TCastleWindowTouch then
    OldAutomaticTouchControl := TCastleWindowTouch(Window).AutomaticTouchInterface;
end;

destructor TGLMode.TWindowState.Destroy;
begin
  Window.OnOpenObject := OldOpenObject;
  Window.OnCloseObject := OldCloseObject;
  Window.OnMotion := OldMotion;
  Window.OnPress := OldPress;
  Window.OnRelease := OldRelease;
  Window.OnBeforeRender := OldBeforeRender;
  Window.OnRender := OldRender;
  {$warnings off} // knowingly looking at deprecated RenderStyle, to keep it working
  Window.RenderStyle := OldRenderStyle;
  {$warnings on}
  Window.OnCloseQuery := OldCloseQuery;
  Window.OnResize := OldResize;
  Window.OnUpdate := OldUpdate;
  Window.OnTimer := OldTimer;
  Window.OnMenuClick := OldMenuClick;
  Window.Caption := oldCaption;
  Window.Userdata := oldUserdata;
  Window.AutoRedisplay := oldAutoRedisplay;
  Window.MainMenu := oldMainMenu;
  if Window.MainMenu <> nil then
    Window.MainMenu.Enabled := OldMainMenuEnabled;
  Window.InternalCursor := OldCursor;
  Window.CustomCursor := OldCustomCursor;
  Window.SwapFullScreen_Key := oldSwapFullScreen_Key;
  Window.Close_charkey := oldClose_charkey;

  if OldControls <> nil then
  begin
    Window.Controls.Assign(OldControls);
    OldControls.EndDisableContextOpenClose;
    FreeAndNil(OldControls);
  end;

  { restore AutomaticTouchInterface after Controls are restored,
    as it may add/remove touch controls, if navigation type changed
    during the mode. }
  if Window is TCastleWindowTouch then
    TCastleWindowTouch(Window).AutomaticTouchInterface := OldAutomaticTouchControl;

  inherited;
end;

procedure TGLMode.TWindowState.WindowOpen(Container: TUIContainer);
var
  I: Integer;
  C: TUIControl;
begin
  if Assigned(OldOpenObject) then
    OldOpenObject(Container);
  { Make sure to call GLContextOpen on OldControls,
    otherwise they would not initialize OpenGL resources even though OpenGL
    context was open. This goes around the C.DisableContextOpenClose value,
    so BeginDisableContextOpenClose / EndDisableContextOpenClose also don't matter. }
  for I := 0 to OldControls.Count - 1 do
  begin
    C := OldControls[I];
    C.GLContextOpen;
  end;
end;

procedure TGLMode.TWindowState.WindowClose(Container: TUIContainer);
var
  I: Integer;
  C: TUIControl;
begin
  if Assigned(OldCloseObject) then
    OldCloseObject(Container);
  { Make sure to call GLContextClose on OldControls,
    otherwise they would not release OpenGL resources even though OpenGL
    context was closed. This goes around the C.DisableContextOpenClose value,
    so BeginDisableContextOpenClose / EndDisableContextOpenClose also don't matter. }
  for I := 0 to OldControls.Count - 1 do
  begin
    C := OldControls[I];
    C.GLContextClose;
  end;
end;

procedure TGLMode.TWindowState.SetStandardState(
  NewRender, NewResize, NewCloseQuery: TContainerEvent);
begin
  Window.OnOpenObject := @WindowOpen;
  Window.OnCloseObject := @WindowClose;
  Window.OnMotion := nil;
  Window.OnPress := nil;
  Window.OnRelease := nil;
  Window.OnBeforeRender := nil;
  Window.OnRender := nil;
  {$warnings off} // knowingly looking at deprecated RenderStyle, to keep it working
  Window.RenderStyle := rs2D;
  {$warnings on}
  Window.OnCloseQuery := nil;
  Window.OnUpdate := nil;
  Window.OnTimer := nil;
  Window.OnResize := nil;
  Window.OnMenuClick := nil;
  Window.OnRender := NewRender;
  Window.OnResize := NewResize;
  Window.OnCloseQuery := NewCloseQuery;
  {Window.Caption := leave current value}
  Window.Userdata := nil;
  Window.AutoRedisplay := false;
  if Window.MainMenu <> nil then
    Window.MainMenu.Enabled := false;
  {Window.MainMenu := leave current value}
  Window.InternalCursor := mcDefault;
  Window.SwapFullScreen_Key := K_None;
  Window.Close_charkey := #0;
  if Window is TCastleWindowTouch then
    TCastleWindowTouch(Window).AutomaticTouchInterface := false;
  Window.Controls.Clear;
end;

{ TGLMode -------------------------------------------------------------------- }

constructor TGLMode.Create(AWindow: TCastleWindowCustom);

  procedure SimulateReleaseAll;
  var
    Button: TMouseButton;
    Key: TKey;
    C: char;
  begin
    { Simulate (to original callbacks) that user releases
      all mouse buttons and key presses now. }
    for Button := Low(Button) to High(Button) do
      if Button in Window.MousePressed then
        Window.Container.EventRelease(InputMouseButton(Window.MousePosition, Button, 0));
    for Key := Low(Key) to High(Key) do
      if Window.Pressed[Key] then
        Window.Container.EventRelease(InputKey(Window.MousePosition, Key, #0));
    for C := Low(C) to High(C) do
      if Window.Pressed.Characters[C] then
        Window.Container.EventRelease(InputKey(Window.MousePosition, K_None, C));
  end;

begin
  inherited Create;

  Window := AWindow;

  FFakeMouseDown := false;

  Check(not Window.Closed, 'ModeGLEnter cannot be called on a closed CastleWindow.');

  OldState := TWindowState.Create(Window);
  OldWidth := Window.Width;
  OldHeight := Window.Height;

  Window.MakeCurrent;

  SimulateReleaseAll;

  Window.Invalidate;
end;

constructor TGLMode.CreateReset(AWindow: TCastleWindowCustom;
  NewRender, NewResize, NewCloseQuery: TContainerEvent);
begin
  Create(AWindow);
  OldState.SetStandardState(NewRender, NewResize, NewCloseQuery);
end;

destructor TGLMode.Destroy;
var
  btn: TMouseButton;
begin
  FreeAndNil(OldState);

  { Although it's forbidden to use TGLMode on Closed TCastleWindowCustom,
    in destructor we must take care of every possible situation
    (because this may be called in finally ... end things when
    everything should be possible). }
  if not Window.Closed then
  begin
    { fake resize event, and fake mouse presss events.
      This way original callbacks are notified about current container state. }
    Window.MakeCurrent;
    if (OldWidth <> Window.Width) or
       (OldHeight <> Window.Height) then
      Window.Container.EventResize;
    if FakeMouseDown then
      for Btn in Window.MousePressed do
        Window.Container.EventPress(InputMouseButton(Window.MousePosition, Btn, 0));

    Window.Invalidate;

    Window.Fps.ZeroNextSecondsPassed;
  end;

  inherited;
end;

{ TGLModeFrozenScreen ------------------------------------------------------ }

constructor TGLModeFrozenScreen.Create(AWindow: TCastleWindowCustom);
begin
  inherited Create(AWindow);

  if Theme.MessageErrorBackground then
  begin
    ErrorBackground := TErrorBackground.Create(nil);
  end else
  begin
    SaveScreenControl := TCastleImageControl.Create(nil);
    SaveScreenControl.Stretch := true;
    SaveScreenControl.FullSize := true;

    { save screen, before changing state. }
    SaveScreenControl.Image := Window.SaveScreen;
    SaveScreenControl.Color := Theme.BackgroundTint;
  end;

  OldState.SetStandardState(nil, nil, @NoClose);

  if Theme.MessageErrorBackground then
    AWindow.Controls.InsertFront(ErrorBackground) else
    AWindow.Controls.InsertFront(SaveScreenControl)
end;

destructor TGLModeFrozenScreen.Destroy;
begin
  inherited;
  { it's a little safer to call this after inherited }
  FreeAndNil(SaveScreenControl);
  FreeAndNil(ErrorBackground);
end;

{ routines ------------------------------------------------------------------- }

procedure NoClose(Container: TUIContainer);
begin
end;

end.
