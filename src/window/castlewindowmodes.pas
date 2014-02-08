{
  Copyright 2003-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Helpers for making modal boxes (TWindowState, TGLMode, TGLModeFrozenScreen)
  cooperating with the TCastleWindowCustom windows.
  They allow to easily save/restore TCastleWindowCustom attributes.

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

uses SysUtils, CastleWindow, CastleGLUtils, CastleImages,
  CastleUIControls, CastleKeysMouse, CastleGLImages;

type
  { }
  TWindowState = class
  private
    { TCastleWindowCustom attributes }
    OldMouseMove: TMouseMoveEvent;
    OldPress, OldRelease: TInputPressReleaseEvent;
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
    OldCustomCursor: TRGBAlphaImage;
    { TCastleWindowDemo attributes } { }
    OldSwapFullScreen_Key: TKey;
    OldClose_charkey: char;
    OldFpsShowOnCaption: boolean;
    { TCastleWindowCustom attributes } { }
    OldControls: TUIControlList;
    OldRenderStyle: TRenderStyle;

    { When adding new attributes to TCastleWindowCustom that should be saved/restored,
      you must remember to
      1. expand this record with new fields
      2. expand routines Get, Set and SetStandard below. } { }
  public
    { Constructor. Gets the state of given window (like GetState). }
    constructor Create(Window: TCastleWindowCustom);
    destructor Destroy; override;

    { GetState saves the TCastleWindowCustom state, SetState applies this state
      back to the window (the same window, or other).
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

      @groupBegin }
    procedure GetState(Window: TCastleWindowCustom);
    procedure SetState(Window: TCastleWindowCustom);
    { @groupEnd }

    { Resets all window properties (that are get / set by TWindowState).
      For most properties, we simply reset them to some sensible default
      values. For some important properties, we take their value
      explicitly by parameter.

      Window properties resetted:

      @unorderedList(
        @item(Callbacks (OnXxx) are set to @nil.

          All callbacks are affected except OnOpen and OnClose callbacks,
          also global CastleUIControls.OnGLContextOpen, CastleUIControls.OnGLContextClose
          are untouched. It is expected that the window (and OpenGL context)
          will exist during the lifetime of a single TGLMode,
          so it makes no sense to deal with them.
        )
        @item(TCastleWindowCustom.Caption and TCastleWindowCustom.MainMenu are left as they were.)
        @item(TCastleWindowCustom.Cursor is reset to mcDefault.)
        @item(TCastleWindowCustom.UserData is reset to @nil.)
        @item(TCastleWindowCustom.AutoRedisplay is reset to @false.)
        @item(TCastleWindowCustom.RenderStyle is reset to rs2D.)
        @item(TCastleWindowCustom.MainMenu.Enabled will be reset to @false (only if MainMenu <> nil).)

        @item(TCastleWindowDemo.SwapFullScreen_Key will be reset to K_None.)
        @item(TCastleWindowDemo.Close_charkey will be reset to #0.)
        @item(TCastleWindowDemo.FpsShowOnCaption will be reset to false.)

        @item(TCastleWindowCustom.Controls is set to empty.)
      )

      If you're looking for a suitable callback to pass as NewCloseQuery
      (new TCastleWindowCustom.OnCloseQuery), @@NoClose may be suitable:
      it's an empty callback, thus using it disables the possibility
      to close the window by window manager
      (usually using "close" button in some window corner or Alt+F4). }
    class procedure SetStandardState(Window: TCastleWindowCustom;
      NewRender, NewResize, NewCloseQuery: TContainerEvent);
  end;

  { Enter / exit modal box on a TCastleWindowCustom. Saves/restores the state
    of TCastleWindowCustom properties (see TWindowState) and various OpenGL state. }
  TGLMode = class
  protected
    Window: TCastleWindowCustom;
  private
    OldState: TWindowState;
    OldWidth, OldHeight: integer;
    FFakeMouseDown: boolean;
    DisabledContextOpenClose: boolean;
  public
    { Constructor saves open TCastleWindowCustom and OpenGL state.
      Destructor will restore them.

      Some gory details (that you will usually not care about...
      the point is: everything works sensibly of the box) :

      @unorderedList(
        @item(We save/restore TWindowState.)

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

    { Save OpenGL and TCastleWindowCustom state, and then change this to a standard
      state. Destructor will restore saved state.

      This is a shortcut for @link(Create) followed by
      @link(TWindowState.SetStandardState), see there for explanation
      of parameters. }
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
    to draw the saved image in a simplest 2D OpenGL projection.

    Between creation/destroy, TCastleWindowCustom.UserData is used by this function
    for internal purposes. So don't use it yourself.
    We'll restore initial TCastleWindowCustom.UserData at destruction. }
  TGLModeFrozenScreen = class(TGLMode)
  private
    type
      TFrozenScreenControl = class(TUIControl)
      private
        Background: TGLImage;
      public
        procedure Render; override;
        procedure GLContextClose; override;
      end;
    var
      Control: TFrozenScreenControl;
  public
    constructor Create(AWindow: TCastleWindowCustom);
    destructor Destroy; override;
  end;

{ Empty TCastleWindowCustom callback, useful as TCastleWindowCustom.OnCloseQuery
  to disallow closing the window by user. }
procedure NoClose(Container: TUIContainer);

implementation

uses CastleGL, CastleUtils;

{ TWindowState -------------------------------------------------------------- }

constructor TWindowState.Create(Window: TCastleWindowCustom);
begin
  inherited Create;
  OldControls := TUIControlList.Create(false);
  GetState(Window);
end;

destructor TWindowState.Destroy;
begin
  FreeAndNil(OldControls);
  inherited;
end;

procedure TWindowState.GetState(Window: TCastleWindowCustom);
begin
  OldMouseMove := Window.OnMouseMove;
  OldPress := Window.OnPress;
  OldRelease := Window.OnRelease;
  OldBeforeRender := Window.OnBeforeRender;
  OldRender := Window.OnRender;
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
  OldCursor := Window.Cursor;
  OldCustomCursor := Window.CustomCursor;
  oldSwapFullScreen_Key := Window.SwapFullScreen_Key;
  oldClose_charkey := Window.Close_charkey;
  oldFpsShowOnCaption := Window.FpsShowOnCaption;
  OldControls.Assign(Window.Controls);
  OldRenderStyle := Window.RenderStyle;
end;

procedure TWindowState.SetState(Window: TCastleWindowCustom);
begin
  Window.OnMouseMove := OldMouseMove;
  Window.OnPress := OldPress;
  Window.OnRelease := OldRelease;
  Window.OnBeforeRender := OldBeforeRender;
  Window.OnRender := OldRender;
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
  Window.Cursor := OldCursor;
  Window.CustomCursor := OldCustomCursor;
  Window.SwapFullScreen_Key := oldSwapFullScreen_Key;
  Window.Close_charkey := oldClose_charkey;
  Window.FpsShowOnCaption := oldFpsShowOnCaption;
  Window.Controls.Assign(OldControls);
  Window.RenderStyle := OldRenderStyle;
end;

class procedure TWindowState.SetStandardState(Window: TCastleWindowCustom;
  NewRender, NewResize, NewCloseQuery: TContainerEvent);
begin
  Window.OnMouseMove := nil;
  Window.OnPress := nil;
  Window.OnRelease := nil;
  Window.OnBeforeRender := nil;
  Window.OnRender := nil;
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
  Window.Cursor := mcDefault;
  Window.SwapFullScreen_Key := K_None;
  Window.Close_charkey := #0;
  Window.FpsShowOnCaption := false;
  Window.Controls.Clear;
  Window.RenderStyle := rs2D;
end;

{ GL Mode ---------------------------------------------------------------- }

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
        Window.Container.EventRelease(InputMouseButton(Button));
    for Key := Low(Key) to High(Key) do
      if Window.Pressed[Key] then
        Window.Container.EventRelease(InputKey(Key, #0));
    for C := Low(C) to High(C) do
      if Window.Pressed.Characters[C] then
        Window.Container.EventRelease(InputKey(K_None, C));
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

 if AWindow is TCastleWindowCustom then
 begin
   { We know that at destruction these controls will be restored to
     the window's Controls list. So there's no point calling any
     GLContextOpen / Close on these controls (that could happen
     e.g. when doing SetStandardState / CreateReset, that clear Controls,
     and at destruction when restoring.) }

   DisabledContextOpenClose := true;
   TCastleWindowCustom(AWindow).Controls.BeginDisableContextOpenClose;
 end;
end;

constructor TGLMode.CreateReset(AWindow: TCastleWindowCustom;
  NewRender, NewResize, NewCloseQuery: TContainerEvent);
begin
  Create(AWindow);
  TWindowState.SetStandardState(AWindow, NewRender, NewResize, NewCloseQuery);
end;

destructor TGLMode.Destroy;
var
  btn: TMouseButton;
begin
  OldState.SetState(Window);
  FreeAndNil(OldState);

  if DisabledContextOpenClose then
    TCastleWindowCustom(Window).Controls.EndDisableContextOpenClose;

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
        Window.Container.EventPress(InputMouseButton(Btn));

    Window.Invalidate;

    Window.Fps.ZeroNextSecondsPassed;
  end;

  inherited;
end;

{ TGLModeFrozenScreen ------------------------------------------------------ }

procedure TGLModeFrozenScreen.TFrozenScreenControl.Render;
begin
  inherited;
  Background.Draw(ContainerRect);
end;

procedure TGLModeFrozenScreen.TFrozenScreenControl.GLContextClose;
begin
  { On Android, it is possible that context will be closed in the middle
    on TGLModeFrozenScreen. Make sure to release Background then. }
  FreeAndNil(Background);
  inherited;
end;

{ TGLModeFrozenScreen ------------------------------------------------------ }

constructor TGLModeFrozenScreen.Create(AWindow: TCastleWindowCustom);
begin
  inherited Create(AWindow);

  Control := TFrozenScreenControl.Create(nil);

  { save screen, before changing state. }
  Control.Background := Window.SaveScreenToGL(true);

  TWindowState.SetStandardState(AWindow, nil, nil, @NoClose);
  AWindow.Controls.InsertFront(Control);

  { setup our 2d projection }
  Window.Container.EventResize;
end;

destructor TGLModeFrozenScreen.Destroy;
begin
  inherited;
  { it's a little safer to call this after inherited }
  FreeAndNil(Control);
end;

{ routines ------------------------------------------------------------------- }

procedure NoClose(Container: TUIContainer);
begin
end;

end.
