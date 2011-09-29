{
  Copyright 2003-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Helpers for making modal boxes (TWindowState, TGLMode, TGLModeFrozenScreen)
  cooperating with the TCastleWindowBase windows.
  They allow to easily save/restore TCastleWindowBase attributes along
  with OpenGL state.

  This unit is a tool for creating functions like
  @link(GLWinMessages.MessageOK). To make nice "modal" box,
  you want to temporarily replace TCastleWindowBase callbacks with your own,
  call Application.ProcessMessage method in a loop until user gives an answer,
  and restore everything. This way you can implement functions that
  wait for some keypress, or wait until user inputs some
  string, or wait until user picks something with mouse,
  or wait for 10 seconds displaying some animation, etc. }
unit GLWinModes;

{$I kambiconf.inc}

interface

uses SysUtils, GL, GLWindow, KambiGLUtils, Images, GLWinMessages,
  UIControls, KeysMouse;

type
  { }
  TWindowState = class
  private
    { TCastleWindowBase attributes }
    oldCallbacks: TWindowCallbacks;
    oldCaption: string;
    oldUserdata: Pointer;
    oldAutoRedisplay: boolean;
    oldFPSActive: boolean;
    oldMainMenu: TMenu;
    { This is saved value of oldMainMenu.Enabled.
      So that you can change MainMenu.Enabled without changing MainMenu
      and SeTWindowState will restore this. }
    oldMainMenuEnabled: boolean;
    OldCursor: TMouseCursor;
    OldCustomCursor: TRGBAlphaImage;
    { TCastleWindowDemo attributes } { }
    oldSwapFullScreen_Key: TKey;
    oldClose_charkey: char;
    oldFpsShowOnCaption: boolean;
    { TCastleWindowCustom attributes } { }
    OldControls: TUIControlList;
    { protected now: OldUseControls: boolean; } { }
    OldOnDrawStyle: TUIControlDrawStyle;

    { When adding new attributes to TCastleWindowBase that should be saved/restored,
      you must remember to
      1. expand this record with new fields
      2. expand routines Get, Set and SetStandard below. } { }
  public
    { Constructor. Gets the state of given window (like GetState). }
    constructor Create(Window: TCastleWindowBase);
    destructor Destroy; override;

    { GetState saves the TCastleWindowBase state, SetState applies this state
      back to the window (the same window, or other).
      Every property that can change when TCastleWindowBase is open are saved.
      This way you can save/restore TCastleWindowBase state, you can also copy
      a state from one window into another.

      Notes about TCastleWindowBase.MainMenu saving: only the reference
      to MainMenu is stored. So:

      @unorderedList(
        @item(If you use TCastleWindowBase.MainMenu,
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
    procedure GetState(Window: TCastleWindowBase);
    procedure SetState(Window: TCastleWindowBase);
    { @groupEnd }

    { Resets all window properties (that are get / set by TWindowState).
      For most properties, we simply reset them to some sensible default
      values. For some important properties, we take their value
      explicitly by parameter.

      Window properties resetted:

      @unorderedList(
        @item(Callbacks (OnXxx) are set to @nil.)
        @item(TCastleWindowBase.Caption and TCastleWindowBase.MainMenu are left as they were.)
        @item(TCastleWindowBase.Cursor is reset to mcDefault.)
        @item(TCastleWindowBase.UserData is reset to @nil.)
        @item(TCastleWindowBase.AutoRedisplay is reset to @false.)
        @item(TCastleWindowBase.OnDrawStyle is reset to dsNone.)
        @item(TCastleWindowBase.MainMenu.Enabled will be reset to @false (only if MainMenu <> nil).)

        @item(TCastleWindowDemo.SwapFullScreen_Key will be reset to K_None.)
        @item(TCastleWindowDemo.Close_charkey will be reset to #0.)
        @item(TCastleWindowDemo.FpsShowOnCaption will be reset to false.)

        @item(TCastleWindowCustom.Controls is set to empty.)
      )

      If you're looking for a suitable callback to pass as NewCloseQuery
      (new TCastleWindowBase.OnCloseQuery), @@NoClose may be suitable:
      it's an empty callback, thus using it disables the possibility
      to close the window by window manager
      (usually using "close" button in some window corner or Alt+F4). }
    class procedure SetStandardState(Window: TCastleWindowBase;
      NewDraw, NewResize, NewCloseQuery: TWindowFunc;
      NewFPSActive: boolean);
  end;

  { Enter / exit modal box on a TCastleWindowBase. Saves/restores the state
    of TCastleWindowBase properties (see TWindowState) and various OpenGL state. }
  TGLMode = class
  protected
    Window: TCastleWindowBase;
  private
    oldWinState: TWindowState;
    oldProjectionMatrix, oldTextureMatrix, oldModelviewMatrix: TMatrix4f;
    oldPixelStoreUnpack: TPixelStoreUnpack;
    oldMatrixMode: TGLenum;
    oldWinWidth, oldWinHeight: integer;
    oldGLWinMessagesTheme: TGLWinMessagesTheme;
    FPushPopGLWinMessagesTheme: boolean;
    FFakeMouseDown: boolean;
    FRestoreProjectionMatrix: boolean;
    FRestoreModelviewMatrix: boolean;
    FRestoreTextureMatrix: boolean;
    DisabledContextOpenClose: boolean;
  public
    { Constructor saves open TCastleWindowBase and OpenGL state.
      Destructor will restore them.

      Some gory details (that you will usually not care about...
      the point is: everything works sensibly of the box) :

      @unorderedList(
        @item(We save/restore:
          @unorderedList(
            @itemSpacing Compact
            @item TWindowState
            @item OpenGL attributes specified in AttribsToPush
            @item OpenGL matrix mode
            @item OpenGL matrices (saved without using OpenGL stack)
            @item OpenGL PIXEL_STORE_* state
            @item GLWinMessagesTheme (only if APushPopGLWinMessagesTheme)
          )
        )

        @item(OpenGL context connected to this window is also made current
          during constructor and destructor. Also, TCastleWindowBase.PostRedisplay
          is called (since new callbacks, as well as original callbacks,
          probably want to redraw window contents.))

        @item(
          All pressed keys and mouse butons are saved and faked to be released,
          by calling TCastleWindowBase.EventMouseUp, Window.EventKeyUp with original
          callbacks.
          This way, if user releases some keys/mouse inside modal box,
          your original TCastleWindowBase callbacks will not miss this fact.
          This way e.g. user scripts in VRML/X3D worlds that observe keys
          work fine.

          If FakeMouseDown then at destruction (after restoring original
          callbacks) we will also notify your original callbacks that
          user pressed these buttons (by sending TCastleWindowBase.EventMouseDown).
          Note that FakeMouseDown feature turned out to be usually more
          troublesome than  usefull --- too often some unwanted MouseDown
          event was caused by this mechanism.
          That's because if original callbacks do something in MouseDown (like
          e.g. activate some click) then you don't want to generate
          fake MouseDown by TGLMode.Destroy.
          So the default value of FakeMouseDown is @false.
          But this means that original callbacks have to be careful
          and @italic(never assume) that when some button is pressed
          (because it's included in MousePressed, or has MouseUp generated for it)
          then for sure there occurred some MouseDown for it.
        )

        @item(At destructor, we notify original callbacks about size changes
          by sending TCastleWindowBase.EventResize. This way your original callbacks
          know about size changes, and can set OpenGL projection etc.)

        @item(
          We call IgnoreNextIdleSpeed at the end, when closing our mode,
          see TCastleWindowBase.IgnoreNextIdleSpeed for comments why this is needed.)

        @item(This also performs important optimization to avoid closing /
          reinitializing window TCastleWindowCustom.Controls OpenGL resources,
          see TUIControl.DisableContextOpenClose.)
      ) }
    constructor Create(AWindow: TCastleWindowBase; AttribsToPush: TGLbitfield;
      APushPopGLWinMessagesTheme: boolean);

    { Save OpenGL and TCastleWindowBase state, and then change this to a standard
      state. Destructor will restore saved state.

      This is a shortcut for @link(Create) followed by
      @link(TWindowState.SetStandardState), see there for explanation
      of parameters. }
    constructor CreateReset(AWindow: TCastleWindowBase; AttribsToPush: TGLbitfield;
      APushPopGLWinMessagesTheme: boolean;
      NewDraw, NewResize, NewCloseQuery: TWindowFunc;
      NewFPSActive: boolean);

    destructor Destroy; override;

    property FakeMouseDown: boolean
      read FFakeMouseDown write FFakeMouseDown default false;

    property RestoreProjectionMatrix: boolean
      read FRestoreProjectionMatrix write FRestoreProjectionMatrix default true;
    property RestoreModelviewMatrix: boolean
      read FRestoreModelviewMatrix write FRestoreModelviewMatrix default true;
    property RestoreTextureMatrix: boolean
      read FRestoreTextureMatrix write FRestoreTextureMatrix default true;
  end;

  { Enter / exit modal box on a TCastleWindowBase, additionally saving the screen
    contents before entering modal box. This is nice if you want to wait
    for some event (like pressing a key), keeping the same screen
    displayed.

    During this lifetime, we set special TCastleWindowBase.OnDraw and TCastleWindowBase.OnResize
    to draw the saved image in a simplest 2D OpenGL projection.

    If you pass PolygonStipple <> nil to constructor,
    window will be additionally covered by this stipple (remember we only
    copy PolygonStipple pointer, so don't free it).

    Between creation/destroy, TCastleWindowBase.UserData is used by this function
    for internal purposes. So don't use it yourself.
    We'll restore initial TCastleWindowBase.UserData at destruction.

     }
  TGLModeFrozenScreen = class(TGLMode)
  private
    dlScreenImage: TGLuint;
    SavedScreenWidth, SavedScreenHeight: Cardinal;
    FPolygonStipple: PPolygonStipple;
  public
    constructor Create(AWindow: TCastleWindowBase; AttribsToPush: TGLbitfield;
      APushPopGLWinMessagesTheme: boolean;
      APolygonStipple: PPolygonStipple);

    destructor Destroy; override;
  end;

{ Empty TCastleWindowBase callback, useful as TCastleWindowBase.OnCloseQuery
  to disallow closing the window by user. }
procedure NoClose(Window: TCastleWindowBase);

implementation

uses KambiUtils, GLImages;

{ TWindowState -------------------------------------------------------------- }

constructor TWindowState.Create(Window: TCastleWindowBase);
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

procedure TWindowState.GetState(Window: TCastleWindowBase);
begin
  oldCallbacks := Window.GetCallbacksState;
  oldCaption := Window.Caption;
  oldUserdata := Window.Userdata;
  oldAutoRedisplay := Window.AutoRedisplay;
  oldFPSActive := Window.Fps.Active;
  oldMainMenu := Window.MainMenu;
  if Window.MainMenu <> nil then
    oldMainMenuEnabled := Window.MainMenu.Enabled;
  OldCursor := Window.Cursor;
  OldCustomCursor := Window.CustomCursor;

  if Window is TCastleWindowDemo then
  begin
    oldSwapFullScreen_Key := TCastleWindowDemo(Window).SwapFullScreen_Key;
    oldClose_charkey := TCastleWindowDemo(Window).Close_charkey;
    oldFpsShowOnCaption := TCastleWindowDemo(Window).FpsShowOnCaption;
  end;

  if Window is TCastleWindowCustom then
  begin
    OldControls.Assign(TCastleWindowCustom(Window).Controls);
    { protected now OldUseControls := TCastleWindowCustom(Window).UseControls; }
    OldOnDrawStyle := TCastleWindowCustom(Window).OnDrawStyle;
  end;
end;

procedure TWindowState.SetState(Window: TCastleWindowBase);
begin
  Window.SetCallbacksState(oldCallbacks);
  Window.Caption := oldCaption;
  Window.Userdata := oldUserdata;
  Window.AutoRedisplay := oldAutoRedisplay;
  Window.Fps.Active := oldFPSActive;
  Window.MainMenu := oldMainMenu;
  if Window.MainMenu <> nil then
    Window.MainMenu.Enabled := OldMainMenuEnabled;
  Window.Cursor := OldCursor;
  Window.CustomCursor := OldCustomCursor;

  if Window is TCastleWindowDemo then
  begin
    TCastleWindowDemo(Window).SwapFullScreen_Key := oldSwapFullScreen_Key;
    TCastleWindowDemo(Window).Close_charkey := oldClose_charkey;
    TCastleWindowDemo(Window).FpsShowOnCaption := oldFpsShowOnCaption;
  end;

  if Window is TCastleWindowCustom then
  begin
    TCastleWindowCustom(Window).Controls.Assign(OldControls);
    { protected now TCastleWindowCustom(Window).UseControls := OldUseControls; }
    TCastleWindowCustom(Window).OnDrawStyle := OldOnDrawStyle;
  end;
end;

class procedure TWindowState.SetStandardState(Window: TCastleWindowBase;
  NewDraw, NewResize, NewCloseQuery: TWindowFunc;
  NewFPSActive: boolean);
begin
  Window.SetCallbacksState(DefaultCallbacksState);
  Window.OnDraw := NewDraw;
  Window.OnResize := NewResize;
  Window.OnCloseQuery := NewCloseQuery;
  {Window.Caption := leave current value}
  Window.Userdata := nil;
  Window.AutoRedisplay := false;
  Window.Fps.Active := NewFPSActive;
  if Window.MainMenu <> nil then
    Window.MainMenu.Enabled := false;
  {Window.MainMenu := leave current value}
  Window.Cursor := mcDefault;

  if Window is TCastleWindowDemo then
  begin
    TCastleWindowDemo(Window).SwapFullScreen_Key := K_None;
    TCastleWindowDemo(Window).Close_charkey := #0;
    TCastleWindowDemo(Window).FpsShowOnCaption := false;
  end;

  if Window is TCastleWindowCustom then
  begin
    TCastleWindowCustom(Window).Controls.Clear;
    { protected now TCastleWindowCustom(Window).UseControls := true; }
    TCastleWindowCustom(Window).OnDrawStyle := dsNone;
  end;
end;

{ GL Mode ---------------------------------------------------------------- }

constructor TGLMode.Create(AWindow: TCastleWindowBase; AttribsToPush: TGLbitfield;
  APushPopGLWinMessagesTheme: boolean);

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
        Window.EventMouseUp(Button);
    for Key := Low(Key) to High(Key) do
      if Window.Pressed[Key] then
        Window.EventKeyUp(Key, #0);
    for C := Low(C) to High(C) do
      if Window.Pressed.Characters[C] then
        Window.EventKeyUp(K_None, C);
  end;

begin
 inherited Create;

 Window := AWindow;

 FFakeMouseDown := false;
 FRestoreProjectionMatrix := true;
 FRestoreModelviewMatrix := true;
 FRestoreTextureMatrix := true;

 Check(not Window.Closed, 'ModeGLEnter cannot be called on a closed GLWindow.');

 oldWinState := TWindowState.Create(Window);
 oldWinWidth := Window.Width;
 oldWinHeight := Window.Height;

 FPushPopGLWinMessagesTheme := APushPopGLWinMessagesTheme;
 if FPushPopGLWinMessagesTheme then
   oldGLWinMessagesTheme := GLWinMessagesTheme;

 Window.MakeCurrent;

 SimulateReleaseAll;

 { save some OpenGL state.
   Musimy sejwowac MatrixMode specjalnie - nie mozemy polegac na tym ze
   GL_TRANSFORM_BIT jest w atrybutach AttribsToPush, a nie chcemy tez
   tego wymuszac (bo byc moze bedzie kiedys pozadane dla uzywajacego teog modulu
   zeby jakies atrybuty z maski GL_TRANSFORM_BIT "przeciekly" na zewnatrz
   ModeGLExit - my sprawiamy tylko ze nie przecieknie MatrixMode).  }
 glPushAttrib(AttribsToPush);
 glGetFloatv(GL_PROJECTION_MATRIX, @oldProjectionMatrix);
 glGetFloatv(GL_TEXTURE_MATRIX, @oldTextureMatrix);
 glGetFloatv(GL_MODELVIEW_MATRIX, @oldModelviewMatrix);
 oldMatrixMode := glGetInteger(GL_MATRIX_MODE);
 SavePixelStoreUnpack(oldPixelStoreUnpack);

 Window.PostRedisplay;

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

constructor TGLMode.CreateReset(AWindow: TCastleWindowBase; AttribsToPush: TGLbitfield;
  APushPopGLWinMessagesTheme: boolean;
  NewDraw, NewResize, NewCloseQuery: TWindowFunc;
  NewFPSActive: boolean);
begin
  Create(AWindow, AttribsToPush, APushPopGLWinMessagesTheme);
  TWindowState.SetStandardState(AWindow,
    NewDraw, NewResize, NewCloseQuery, NewFPSActive);
end;

destructor TGLMode.Destroy;
var
  btn: TMouseButton;
begin
 oldWinState.SetState(Window);
 FreeAndNil(oldWinState);

 if DisabledContextOpenClose then
   TCastleWindowCustom(Window).Controls.EndDisableContextOpenClose;

 if FPushPopGLWinMessagesTheme then
   GLWinMessagesTheme := oldGLWinMessagesTheme;

 { Although it's forbidden to use TGLMode on Closed TCastleWindowBase,
   in destructor we must take care of every possible situation
   (because this may be called in finally ... end things when
   everything should be possible). }
 if not Window.Closed then
 begin
   Window.MakeCurrent;

   { restore OpenGL state }
   LoadPixelStoreUnpack(oldPixelStoreUnpack);

   if RestoreProjectionMatrix then
   begin
     glMatrixMode(GL_PROJECTION);
     glLoadMatrix(oldProjectionMatrix);
   end;

   if RestoreTextureMatrix then
   begin
     glMatrixMode(GL_TEXTURE);
     glLoadMatrix(oldTextureMatrix);
   end;

   if RestoreModelviewMatrix then
   begin
     glMatrixMode(GL_MODELVIEW);
     glLoadMatrix(oldModelviewMatrix);
   end;

   glMatrixMode(oldMatrixMode);
   glPopAttrib;

   { (pamietajmy ze przed EventXxx musi byc MakeCurrent) - juz zrobilismy
     je powyzej }
   { Gdy byly aktywne nasze callbacki mogly zajsc zdarzenia co do ktorych
     oryginalne callbacki chcialyby byc poinformowane. Np. OnResize. }
   if (oldWinWidth <> Window.Width) or
      (oldWinHeight <> Window.Height) then
    Window.EventResize;

   { udajemy ze wszystkie przyciski myszy jakie sa wcisniete sa wciskane wlasnie
     teraz }
   if FakeMouseDown then
     for btn := Low(btn) to High(btn) do
       if btn in Window.mousePressed then
         Window.EventMouseDown(btn);

   Window.PostRedisplay;

   Window.Fps.IgnoreNextIdleSpeed;
 end;

 inherited;
end;

{ TGLModeFrozenScreen ------------------------------------------------------ }

procedure FrozenImageDraw(Window: TCastleWindowBase);
var Mode: TGLModeFrozenScreen;
    Attribs: TGLbitfield;
begin
 Mode := TGLModeFrozenScreen(Window.UserData);

 { TODO:  I should build display list with this in each FrozenImageResize
   (Window.Width, Window.Height may change with time). }

 if (Cardinal(Window.Width ) > Mode.SavedScreenWidth ) or
    (Cardinal(Window.Height) > Mode.SavedScreenHeight) then
  glClear(GL_COLOR_BUFFER_BIT);

 Attribs := GL_CURRENT_BIT or GL_ENABLE_BIT;
 if Mode.FPolygonStipple <> nil then
  Attribs := Attribs or GL_POLYGON_BIT or GL_POLYGON_STIPPLE_BIT;

 glPushAttrib(Attribs);
 try
  glPushMatrix;
  try
   glDisable(GL_DEPTH_TEST);

   glLoadIdentity;
   glRasterPos2i(0, 0);
   glCallList(Mode.dlScreenImage);

   if Mode.FPolygonStipple <> nil then
   begin
    glEnable(GL_POLYGON_STIPPLE);
    KamGLPolygonStipple(Mode.FPolygonStipple);
    glColor3ub(0, 0, 0);
    glRectf(0, 0, Window.Width, Window.Height);
   end;
  finally glPopMatrix end;
 finally glPopAttrib end;
end;

constructor TGLModeFrozenScreen.Create(AWindow: TCastleWindowBase;
  AttribsToPush: TGLbitfield; APushPopGLWinMessagesTheme: boolean;
  APolygonStipple: PPolygonStipple);
begin
 inherited Create(AWindow, AttribsToPush, APushPopGLWinMessagesTheme);

 FPolygonStipple := APolygonStipple;

 { We must do it before SaveScreen.
   Moreover, we must do it before we set our own projection below
   (calling EventResize) and before we set OnDraw to FrozenImageDraw
   (because we want that Window.FlushRedisplay calls original OnDraw). }
 Window.FlushRedisplay;

 TWindowState.SetStandardState(AWindow,
   {$ifdef FPC_OBJFPC} @ {$endif} FrozenImageDraw,
   {$ifdef FPC_OBJFPC} @ {$endif} Resize2D,
   {$ifdef FPC_OBJFPC} @ {$endif} NoClose,
   AWindow.Fps.Active);
 AWindow.UserData := Self;

 { setup our 2d projection. We must do it before SaveScreen }
 Window.EventResize;

 SavedScreenWidth  := Window.Width;
 SavedScreenHeight := Window.Height;
 dlScreenImage := SaveScreen_ToDisplayList_noflush(
   0, 0, SavedScreenWidth, SavedScreenHeight, GL_FRONT);
end;

destructor TGLModeFrozenScreen.Destroy;
begin
 inherited;
 { it's a little safer to call this after inherited }
 glFreeDisplayList(dlScreenImage);
end;

{ routines ------------------------------------------------------------------- }

procedure NoClose(Window: TCastleWindowBase);
begin
end;

end.
