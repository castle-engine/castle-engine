{
  Copyright 2003-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Helpers for making modal boxes (TGLMode, TGLModeFrozenScreen)
  cooperating with the TCastleWindow windows.
  They allow to easily save / restore TCastleWindow attributes.

  This unit is a tool for creating functions like
  @link(CastleMessages.MessageOK). To make nice "modal" box,
  you want to temporarily replace TCastleWindow callbacks with your own,
  call Application.ProcessMessage method in a loop until user gives an answer,
  and restore everything. This way you can implement functions that
  wait for some keypress, or wait until user inputs some
  string, or wait until user picks something with mouse,
  or wait for 10 seconds displaying some animation, etc.

  This unit is internal, and in time may be removed.
  We recommend using TCastleView(s) instances to organize your user-interface. }
unit CastleInternalWindowModes;

{$I castleconf.inc}

interface

uses SysUtils, Classes, CastleWindow, CastleGLUtils, CastleImages,
  CastleUIControls, CastleKeysMouse, CastleGLImages, CastleControls;

type
  { Enter / exit modal state on a TCastleWindow.
    Saves/restores the state of TCastleWindow. }
  TGLMode = class
  strict private
    OldWidth, OldHeight: integer;
    FFakeMouseDown: boolean;
  strict protected
    type
      TWindowState = class(TComponent)
      strict private
        Window: TCastleWindow;
        OldMotion: TInputMotionEvent;
        OldPress, OldRelease: TInputPressReleaseEvent;
        OldOpenObject, OldCloseObject: TContainerObjectEvent;
        OldBeforeRender, OldRender, OldCloseQuery, OldUpdate, OldTimer: TContainerEvent;
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
        OldSwapFullScreen_Key: TKey;
        OldClose_KeyString: String;
        OldControls: TInternalChildrenControls;
        OldAutomaticTouchControl: boolean;
        procedure WindowOpen(Container: TCastleContainer);
        procedure WindowClose(Container: TCastleContainer);
      public
        { When adding new attributes to TCastleWindow that should be saved/restored,
          you must remember to
          1. expand this class with new fields
          2. expand constructor, destructor and SetStandardState } { }

        procedure SetStandardState(
          NewRender, NewResize, NewCloseQuery: TContainerEvent);

        { Constructor saves the TCastleWindow state, destructor applies this state
          back to the window.
          Every property that can change when TCastleWindow is open are saved.
          This way you can save/restore TCastleWindow state, you can also copy
          a state from one window into another.

          Notes about TCastleWindow.MainMenu saving: only the reference
          to MainMenu is stored. So:

          @unorderedList(
            @item(If you use TCastleWindow.MainMenu,
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
        constructor Create(AWindow: TCastleWindow); reintroduce;
        destructor Destroy; override;
      end;
    var
    OldState: TWindowState;
    Window: TCastleWindow;
    DisabledContextOpenClose: boolean;
  public
    { Constructor saves open TCastleWindow and OpenGL state.
      Destructor will restore them.

      Some gory details (that you will usually not care about...
      the point is: everything works sensibly of the box) :

      @unorderedList(
        @item(We save/restore window state.)

        @item(OpenGL context connected to this window is also made current
          during constructor and destructor. Also, TCastleWindow.Invalidate
          is called (since new callbacks, as well as original callbacks,
          probably want to redraw window contents.))

        @item(
          All pressed keys and mouse butons are saved and faked to be released,
          by calling TCastleWindow.EventRelease with original
          callbacks.
          This way, if user releases some keys/mouse inside modal box,
          your original TCastleWindow callbacks will not miss this fact.
          This way e.g. user scripts in VRML/X3D worlds that observe keys
          work fine.

          If FakeMouseDown then at destruction (after restoring original
          callbacks) we will also notify your original callbacks that
          user pressed these buttons (by sending TCastleWindow.EventMouseDown).
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
          by sending TCastleWindow.EventResize. This way your original callbacks
          know about size changes, and can set OpenGL projection etc.)

        @item(
          We call ZeroNextSecondsPassed at the end, when closing our mode,
          see TFramesPerSecond.ZeroNextSecondsPassed for comments why this is needed.)

        @item(This also performs important optimization to avoid closing /
          reinitializing window TCastleWindow.Controls OpenGL resources,
          see TCastleUserInterface.DisableContextOpenClose.)
      ) }
    constructor Create(AWindow: TCastleWindow);

    { Save TCastleWindow state, and then change it to a standard
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
        @item(TCastleWindow.Caption and TCastleWindow.MainMenu are left as they were.)
        @item(TCastleWindow.Cursor is reset to mcDefault.)
        @item(TCastleWindow.UserData is reset to @nil.)
        @item(TCastleWindow.AutoRedisplay is reset to @false.)
        @item(TCastleWindow.MainMenu.Enabled will be reset to @false (only if MainMenu <> nil).)

        @item(TCastleWindowDemo.SwapFullScreen_Key will be reset to keyNone.)
        @item(TCastleWindowDemo.Close_KeyString will be reset to ''.)

        @item(All TCastleWindow.Controls are temporarily removed.)
      )

      If you're looking for a suitable callback to pass as NewCloseQuery
      (new TCastleWindow.OnCloseQuery), @@NoClose may be suitable:
      it's an empty callback, thus using it disables the possibility
      to close the window by window manager
      (usually using "close" button in some window corner or Alt+F4). }
    constructor CreateReset(AWindow: TCastleWindow;
      NewRender, NewResize, NewCloseQuery: TContainerEvent);

    destructor Destroy; override;

    property FakeMouseDown: boolean
      read FFakeMouseDown write FFakeMouseDown default false;
  end;

  { Enter / exit modal box on a TCastleWindow, additionally saving the screen
    contents before entering modal box. This is nice if you want to wait
    for some event (like pressing a key), keeping the same screen
    displayed.

    During this lifetime, we set special TCastleWindow.OnRender and TCastleWindow.OnResize
    to draw the saved image in a simplest 2D OpenGL projection. }
  TGLModeFrozenScreen = class(TGLMode)
  private
    BackgroundControls: TCastleUserInterface;
  public
    constructor Create(AWindow: TCastleWindow);
    destructor Destroy; override;
  end;

{ Empty TCastleWindow callback, useful as TCastleWindow.OnCloseQuery
  to disallow closing the window by user. }
procedure NoClose(Container: TCastleContainer);

implementation

uses CastleUtils, CastleColors, CastleVectors;

{ TGLMode.TWindowState -------------------------------------------------------------- }

constructor TGLMode.TWindowState.Create(AWindow: TCastleWindow);
begin
  inherited Create(nil);
  Window := AWindow;

  {$warnings off} // keep deprecated OnXxx working
  OldOpenObject := Window.OnOpenObject;
  OldCloseObject := Window.OnCloseObject;
  { Note that we do not touch OnOpen and OnClose. Let them happen.
    Our WindowOpen/Close will also call origina OnOpenObject/Close. }
  OldMotion := Window.OnMotion;
  OldPress := Window.OnPress;
  OldRelease := Window.OnRelease;
  OldBeforeRender := Window.OnBeforeRender;
  OldRender := Window.OnRender;
  OldCloseQuery := Window.OnCloseQuery;
  OldResize := Window.OnResize;
  OldUpdate := Window.OnUpdate;
  {$ifdef FPC}
  OldTimer := Window.OnTimer;
  {$endif}
  {$warnings on}
  OldMenuClick := Window.OnMenuClick;
  oldCaption := Window.Caption;
  oldUserdata := Window.Userdata;
  oldAutoRedisplay := Window.AutoRedisplay;
  oldMainMenu := Window.MainMenu;
  if Window.MainMenu <> nil then
    oldMainMenuEnabled := Window.MainMenu.Enabled;
  OldCursor := Window.InternalCursor;
  oldSwapFullScreen_Key := Window.SwapFullScreen_Key;
  oldClose_KeyString := Window.Close_KeyString;

  OldControls := TInternalChildrenControls.Create(nil);
  OldControls.Assign(Window.Controls);
  OldControls.BeginDisableContextOpenClose;
end;

destructor TGLMode.TWindowState.Destroy;
begin
  {$warnings off} // keep deprecated OnXxx working
  Window.OnOpenObject := OldOpenObject;
  Window.OnCloseObject := OldCloseObject;
  Window.OnMotion := OldMotion;
  Window.OnPress := OldPress;
  Window.OnRelease := OldRelease;
  Window.OnBeforeRender := OldBeforeRender;
  Window.OnRender := OldRender;
  Window.OnCloseQuery := OldCloseQuery;
  Window.OnResize := OldResize;
  Window.OnUpdate := OldUpdate;
  {$ifdef FPC}
  Window.OnTimer := OldTimer;
  {$endif}
  {$warnings on}
  Window.OnMenuClick := OldMenuClick;
  Window.Caption := oldCaption;
  Window.Userdata := oldUserdata;
  Window.AutoRedisplay := oldAutoRedisplay;
  Window.MainMenu := oldMainMenu;
  if Window.MainMenu <> nil then
    Window.MainMenu.Enabled := OldMainMenuEnabled;
  Window.InternalCursor := OldCursor;
  Window.SwapFullScreen_Key := oldSwapFullScreen_Key;
  Window.Close_KeyString := oldClose_KeyString;

  if OldControls <> nil then
  begin
    Window.Controls.Assign(OldControls);
    OldControls.EndDisableContextOpenClose;
    FreeAndNil(OldControls);
  end;

  inherited;
end;

procedure TGLMode.TWindowState.WindowOpen(Container: TCastleContainer);
var
  I: Integer;
  C: TCastleUserInterface;
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

procedure TGLMode.TWindowState.WindowClose(Container: TCastleContainer);
var
  I: Integer;
  C: TCastleUserInterface;
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
  {$warnings off} // keep deprecated OnXxx working
  Window.OnOpenObject := {$ifdef FPC}@{$endif}WindowOpen;
  Window.OnCloseObject := {$ifdef FPC}@{$endif}WindowClose;
  Window.OnMotion := nil;
  Window.OnPress := nil;
  Window.OnRelease := nil;
  Window.OnBeforeRender := nil;
  Window.OnRender := nil;
  Window.OnCloseQuery := nil;
  Window.OnUpdate := nil;
  {$ifdef FPC}
  Window.OnTimer := nil;
  {$endif}
  Window.OnResize := nil;
  Window.OnRender := NewRender;
  Window.OnResize := NewResize;
  Window.OnCloseQuery := NewCloseQuery;
  {$warnings on}
  Window.OnMenuClick := nil;
  {Window.Caption := leave current value}
  Window.Userdata := nil;
  Window.AutoRedisplay := false;
  if Window.MainMenu <> nil then
    Window.MainMenu.Enabled := false;
  {Window.MainMenu := leave current value}
  Window.InternalCursor := mcDefault;
  Window.SwapFullScreen_Key := keyNone;
  Window.Close_KeyString := '';
  Window.Controls.Clear;
end;

{ TGLMode -------------------------------------------------------------------- }

constructor TGLMode.Create(AWindow: TCastleWindow);

  procedure SimulateReleaseAll;
  var
    Button: TCastleMouseButton;
    Key: TKey;
    C: char;
    ModifiersDown: TModifierKeys;
  begin
    { Simulate (to original callbacks) that user releases
      all mouse buttons and key presses now. }
    for Button := Low(Button) to High(Button) do
      if Button in Window.MousePressed then
        Window.Container.EventRelease(InputMouseButton(Window.MousePosition, Button, 0, []));
    ModifiersDown := CastleKeysMouse.ModifiersDown(Window.Container.Pressed);
    for Key := Low(Key) to High(Key) do
      if Window.Pressed[Key] then
        Window.Container.EventRelease(InputKey(Window.MousePosition, Key, '', ModifiersDown));
    for C := Low(C) to High(C) do
      if Window.Pressed.Characters[C] then
        Window.Container.EventRelease(InputKey(Window.MousePosition, keyNone, C, ModifiersDown));
  end;

begin
  inherited Create;

  Window := AWindow;

  FFakeMouseDown := false;

  Check(not Window.Closed, 'TGLMode.Create cannot be called on a closed CastleWindow.');

  OldState := TWindowState.Create(Window);
  OldWidth := Window.Width;
  OldHeight := Window.Height;

  Window.MakeCurrent;

  SimulateReleaseAll;

  Window.Invalidate;
end;

constructor TGLMode.CreateReset(AWindow: TCastleWindow;
  NewRender, NewResize, NewCloseQuery: TContainerEvent);
begin
  Create(AWindow);
  OldState.SetStandardState(NewRender, NewResize, NewCloseQuery);
end;

destructor TGLMode.Destroy;
var
  btn: TCastleMouseButton;
begin
  FreeAndNil(OldState);

  { Although it's forbidden to use TGLMode on Closed TCastleWindow,
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
        Window.Container.EventPress(InputMouseButton(Window.MousePosition, Btn, 0, []));

    Window.Invalidate;

    Window.Fps.ZeroNextSecondsPassed;
  end;

  inherited;
end;

{ TGLModeFrozenScreen ------------------------------------------------------ }

constructor TGLModeFrozenScreen.Create(AWindow: TCastleWindow);

  { Fill BackgroundControls with UI to represent frozen screen.
    This is quite similar to what TStateDialog.Start does. }
  procedure FillBackgroundControls;
  var
    BackgroundColor: TCastleColor;
    BackgroundImage: TCastleImageControl;
    BackgroundRect: TCastleRectangleControl;
  begin
    if Theme.InternalMessageFallbackLook then
      BackgroundColor := Vector4(Theme.BackgroundOpaqueColor, 1)
    else
      BackgroundColor := Theme.BackgroundColor;

    if BackgroundColor[3] <> 1 then
    begin
      BackgroundImage := TCastleImageControl.Create(BackgroundControls);
      BackgroundImage.Stretch := true;
      BackgroundImage.FullSize := true;
      { save screen, before changing state. }
      BackgroundImage.Image := Window.SaveScreen;
      BackgroundControls.InsertFront(BackgroundImage);
    end;

    BackgroundRect := TCastleRectangleControl.Create(BackgroundControls);
    BackgroundRect.Color := BackgroundColor;
    BackgroundRect.FullSize := true;
    {$warnings off} // using deprecated, but this whole TGLMode is hacky, we should rather use TCastleView and TCastleView.InterceptInput
    BackgroundRect.InterceptInput := true;
    {$warnings on}
    BackgroundControls.InsertFront(BackgroundRect);
  end;

begin
  inherited Create(AWindow);

  BackgroundControls := TCastleUserInterface.Create(nil);
  BackgroundControls.FullSize := true;
  FillBackgroundControls;

  OldState.SetStandardState(nil, nil, @NoClose);

  AWindow.Controls.InsertFront(BackgroundControls);
end;

destructor TGLModeFrozenScreen.Destroy;
begin
  inherited;
  { it's a little safer to call this after inherited }
  FreeAndNil(BackgroundControls);
end;

{ routines ------------------------------------------------------------------- }

procedure NoClose(Container: TCastleContainer);
begin
end;

end.
