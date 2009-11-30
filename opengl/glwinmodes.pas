{
  Copyright 2003-2009 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{ @abstract(Unit with utilities to easily save/restore TGLWindow
  attributes along with OpenGL state. This is useful
  to create routines that work like "modal windows" in programs
  that use GLWindow unit.)

  This unit is a tool for creating functions like
  @link(GLWinMessages.MessageOK). Such functions want to temporarily
  replace TGLWindow callbacks with their own,
  then call glwm.ProcessMessage method in a loop and then
  return, restoring GLWindow callbacks and OpenGL state back into
  the original state. This way you can implement functions that
  e.g. wait for some keypress, or wait until user inputs some
  string, or wait until user picks something with mouse,
  or wait for 10 seconds displaying some animation, etc.
  Such functions can implement things like "modal boxes" known
  from many GUI toolkits.

  In such situations it's crucial to reliably do operations
  "save the state of some things" and "restore state of some things".
  OpenGL has routines gl(Push|Pop)(Matrix|Attrib) for exactly
  such work, and this unit provides such routines for saving and restoring
  attributes of @link(TGLWindow) object. Also it provides routines to set
  in one call all attributes of @link(TGLWindow) object that can be
  saved/restored. Finally it provides @link(TGLMode) class that somehow
  merges saving/restoring of @link(TGLWindow) attributes and OpenGL state.
  Also some sample generally useful descendant of @link(TGLMode)
  is provided: @link(TGLModeFrozenScreen).

  Pl:
  Nasz pomysl ma duzo szersze zastosowanie, nie chodzi nam o okienka
  ale o cokolwiek zwiazanego z konstrukcja procedur ktore w "punkcie
  kulminacyjnym" wykonuja petle w rodzaju
    while not <warunek-konca> do glwm.ProcessMessage;
  lub
    while (not <warunek-konca>) and glwm.ProcessMessage do ;
  (druga wersja dopuszcza fakt ze user moze zamknac okienko w czasie trwania
  procedury; zazwyczaj procedury w rodzaju MessageOK beda blokowac mozliwosc
  zamkniecia programu na czas swojego dzialania
  (moga to zrobic prosto ustawiajac odpowiednie OnCloseQuery)) }

unit GLWinModes;

{$I kambiconf.inc}
{$I openglmac.inc}

interface

uses SysUtils, GL, GLU, GLExt, GLWindow, KambiGLUtils, Images, GLWinMessages;

{ GLWindowState --------------------------------------------------------- }

type
  { }
  TGLWindowState = class
  private
    { TGLWindow attributes }
    oldCallbacks: TGLWindowCallbacks;
    oldCaption: string;
    oldUserdata: Pointer;
    oldAutoRedisplay: boolean;
    oldFPSActive: boolean;
    oldMainMenu: TMenu;
    { This is saved value of oldMainMenu.Enabled.
      So that you can change MainMenu.Enabled without changing MainMenu
      and SetGLWindowState will restore this. }
    oldMainMenuEnabled: boolean;
    OldCursor: TGLWindowCursor;
    OldCustomCursor: TRGBAlphaImage;
    { TGLWindowDemo attributes } { }
    oldSwapFullScreen_Key: TKey;
    oldClose_charkey: char;
    oldFpsShowOnCaption: boolean;
    { TGLWindowNavigated attributes } { }
    OldUseControls: boolean;

    { When adding new attributes to TGLWindow that should be saved/restored,
      you must remember to
      1. expand this record with new fields
      2. expand routines Get, Set and SetStandard below. } { }
  public
    { Constructor. Gets the state of given window (like GetState). }
    constructor Create(Glwin: TGLWindow);

    { GetState saves the TGLWindow state, SetState applies this state to the window
      (the same or other).
      Sejwuja / ustawiaja wszystkie wlasciwosci okienka TGLWindow
      ktore moga sie zmieniac w czasie gdy okienko pozostaje not Closed.
      Te wlasciwosci sa zapamietywane in our fields.
      W ten sposob mozesz za pomoca Get/Set robic cos jak Push/Pop stanu
      wlasciwosci TGLWindow, mozesz tez robic w ten sposob kopiowanie
      wlasciwosci z jednego okienka do drugiego.

      Notka: pamietaj ze tylko referencja wartosci MainMenu jest kopiowana.
      Wiec 1. nie mozesz robic tak po prostu kopiowania wartosci MainMenu
      z jednego okienka do drugiego (zeby jej pozniej dwa razy nie robic
      Free), o ile jest ona <> nil.
      2. Nie mozesz zmieniac zawartosci MainMenu w czasie TGLMode.Create/Free.
      (chociaz mozesz podmieniac wartosc MainMenu na inna, pod warunkiem
      ze obie sa <> nil). Wyjatkiem jest tutaj MainMenu.Enabled, ktore
      jest tutaj zapamietywane specjalnie.

      Note that Set first sets Glwin.MainMenu, and then,
      if Glwin.MainMenu <> nil, sets Glwin.MainMenu.Enabled from saved state.

      @groupBegin }
    procedure GetState(Glwin: TGLWindow);
    procedure SetState(Glwin: TGLWindow);
    { @groupEnd }

    { Ustawia wszystkie wlasciwosci ktore sa
      zawarte w TGLWindowState. Ale pozwala w wygodniejszy sposob niz SetState
      podac te wlasciwosci :
        czesc wlasciwosci jest pobierana jako parametry,
        czesc wlasciwosci jest pominieta -
          pominiete callbacki beda ustawione na nil,
          pominiete Caption i MainMenu bedzie zostawione takie jakie jest,
          pominiete Cursor bedzie ustawione na gcDefault.
        Note that NewMainMenuEnabled will be set only if Glwin.MainMenu <> nil. }
    class procedure SetStandardState(Glwin: TGLWindow;
      NewDraw, NewCloseQuery, NewResize: TGLWindowFunc;
      NewUserData: Pointer; NewAutoRedisplay: boolean; NewFPSActive: boolean;
      NewMainMenuEnabled: boolean;
      NewSwapFullScreen_Key: TKey;
      NewClose_charkey: char; NewFpsShowOnCaption, NewUseControls: boolean);

    { Jak SetStandardState
      ale ustawia zawsze oldClose_charkey na #0 i NewCloseQuery
      na procedure bez zadnego kodu : begin end; W ten sposob uniemozliwia
      userowi zamkniecie okienka metodami WindowManagera lub dawanymi przez
      klase TGLWindowDemo. }
    class procedure SetStandardNoCloseState(Glwin: TGLWindow;
      NewDraw, NewResize: TGLWindowFunc;
      NewUserData: Pointer; NewAutoRedisplay: boolean; NewFPSActive: boolean;
      NewMainMenuEnabled: boolean;
      NewSwapFullScreen_Key: TKey;
      NewFpsShowOnCaption, NewUseControls: boolean);
  end;

{ GL Mode ---------------------------------------------------------------- }

{ }
type
  TGLMode = class
  protected
    glwin: TGLWindow;
  private
    oldWinState: TGLWindowState;
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
  public
    { Enter / Exit mode:

      Create wykonuje save stanu OpenGL'a i stanu glwin do klasy TGLMode.
      Destroy przywraca caly zesejwowany stan.
      If APushPopGLWinMessagesTheme then GLWinMessagesTheme is also
      saved and restored.

      Szczegoly ktore niestety musza sie tutaj znalezc w interfejsie mimo ze
      zazwyczaj uzywajac tych procedur bedziesz chcial o tych szczegolach
      wlasnie zapomniec :

      @unorderedList(
        @item(
          stan jaki jest sejwowany i pozniej odpowiednio przywracany :
            @unorderedList(
              @itemSpacing Compact
              @item stan TGLWindowState
              @item atrybuty AttribsToPush (przez glPush/Pop Attrib)
              @item stan MatrixMode
              @item(Macierze projection, texture i modelview (czyli wszystkie)
                (nie, nie uzywamy matrix stack).

                You can turn off restoring of them by setting corresponding
                properties to @false. See RestoreProjectionMatrix,
                RestoreModelviewMatrix, RestoreTextureMatrix.)
              @item stan PIXEL_STORE_*
            )
          )

        @item(
          zarowno wywolanie Enter jak i Exit ustawia aktywny kontekst OpenGL'a
          na kontekst okienka glwin)

        @item(
          W Enter wszystkie aktualnie wcisniete klawisze myszki sa sztucznie
          wylaczane przez wywolanie Glwin.EventMouseUp.
          Also, all currently pressed keys are released by fake
          Glwin.EventKeyUp. This is important: otherwise, if user releases
          mouse / key when inside mode, your original callbacks would not
          be informed about releasing a pressed key. And various things
          may depend on that (including user scripts in VRML worlds.)

          If FakeMouseDown then
          w Exit wszystkie aktualnie wcisniete klawisze myszki sa sztucznie
          wlaczane przez wywolanie Glwin.EventMouseDown (juz PO przywroceniu
          oryginalnych callbackow okienka).

          W ten sposob callbacki okienka (ktore zapewne bedziesz podmienial
          na czas trwania ModeGLEnter..Exit) beda myslaly ze w momencie
          wejscia do Mode user puszcza wszystkie klawisze myszy a w momencie
          wyjscia z Mode zostana powiadomione o tym jakie klawisze myszy
          sa rzeczywiscie wcisniete. To jest pozadane bo inaczej jezeli
          podmienisz callbacki dla myszki na wlasne to oryginalne callbacki
          okienka np. nigdy sie nie dowiedza ze user puscil klawisze myszki
          w czasie gdy bylismy pomiedzy ModeGLEnter..Exit.

          FakeMouseDown turned out to be usually more troublesome than
          usefull --- too often some unwanted MouseDown
          event was caused by this mechanism.
          That's because if original callbacks do something in MouseDown (like
          e.g. activate some click) then you don't want to generate
          fake MouseDown by TGLMode.Destroy.
          So the default value of FakeMouseDown is @false.
          But this means that original callbacks have to be careful
          and *never assume* that when some button is pressed
          (because it's included in MousePressed, or has MouseUp generated for it)
          then for sure there occured some MouseDown for it.
        )

        @item(
          Jezeli rozmiary okienka (Width, Height) ulegly zmianie pomiedzy Enter
          a Exit to w Exit wywolujemy sztucznie Glwin.EventResize
          (juz PO przywroceniu oryginalnych callbackow okienka).
          Powod : jak wyzej - gdyby user zmienil rozmiary okienka w czasie
          trwania ModeGLEnter...Exit z podmienionymi callbackami to oryginalne
          callbacki moglyby sie nigdy nie dowiedziec o zmianie rozmiaru okna
          i nie zareagowac na to prawidlowo.)

        @item(
          W ModeEnter i Exit sa wywolywane Glwin.PostRedisplay (bo spodziewam
          sie ze podmienisz callback OnDraw a wiec zdecydowanie bedziesz
          chcial odmalowac okienko).)

        @item(
          ModeGLEnter nie moze byc uzyte na Closed okienku,
          oczywiscie. Wiec dla bezpiecznstwa jest w nim robiony
          Check(not Glwin.Closed, ...).)

        @item(
          We call IgnoreNextIdleSpeed at the end, when closing our mode,
          see TGLWindow.IgnoreNextIdleSpeed for comments why this is needed.)
      ) }
    constructor Create(AGLWindow: TGLWindow; AttribsToPush: TGLbitfield;
      APushPopGLWinMessagesTheme: boolean);

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

type
  TGLModeFrozenScreen = class(TGLMode)
  private
    dlScreenImage: TGLuint;
    SavedScreenWidth, SavedScreenHeight: Cardinal;
    FPolygonStipple: PPolygonStipple;
  public
    { This mode on enter catches current screen (with Glwin.SaveScreen) then
      calls TGLWindowState.SetStandardNoCloseState with such OnDraw and OnResize private
      callbacks that

      @unorderedList(
        @item(
          the projection is always simple 2D projection
          (this is the simplest Resize2D function from GLWindow unit).)

        @item(
          OnDraw always simply draws catched screen image on screen.
          If window ever gets larger than initially catched image
          it draws the area not covered by image by color you set with glClearColor
          (i.e. it uses glClear(GL_COLOR_BUFFER_BIT)).

          If PolygonStipple <> nil, then it will additionally draw this stipple
          all over the window. (note : remember that we copy here only the pointer
          PolygonStipple, not pointer's contents).)
      )

      Between creation/destroy, Glwin.UserData is used by this function
      for "private" purposes so you should not use it yourself.
      Glwin.UserData will be restored after destroying this object
      to whatever value it had at creation time.

      This mode is often quite usable because it gives you some minimal
      sensible OnDraw and OnResize callbacks, you usually can use it if you
      just don't want to do anything with your own callbacks for some time,
      i.e. usually you can use it like

@longcode(#
  Mode := TGLModeFrozenScreen.Create(glwin, 0);
  try
   while <something> do glwm.ProcessMessage(...);
  finally Mode.Free end;
#)

      and you can safely assume that NO your own glwin callbacks (that were
      registered before Mode := TGLModeFrozenScreen.Create) will be called between
      try .. finally, yet everything will work correctly (window contents will
      be properly refreshed if window manager will request window redraw etc.) }
    constructor Create(AGLWindow: TGLWindow; AttribsToPush: TGLbitfield;
      APushPopGLWinMessagesTheme: boolean;
      APolygonStipple: PPolygonStipple);

    destructor Destroy; override;
  end;

implementation

uses KambiUtils, GLImages;

{ TGLWindowState -------------------------------------------------------------- }

constructor TGLWindowState.Create(Glwin: TGLWindow);
begin
  inherited Create;
  GetState(Glwin);
end;

procedure TGLWindowState.GetState(Glwin: TGLWindow);
begin
  oldCallbacks := Glwin.GetCallbacksState;
  oldCaption := Glwin.Caption;
  oldUserdata := Glwin.Userdata;
  oldAutoRedisplay := Glwin.AutoRedisplay;
  oldFPSActive := Glwin.Fps.Active;
  oldMainMenu := Glwin.MainMenu;
  if Glwin.MainMenu <> nil then
    oldMainMenuEnabled := Glwin.MainMenu.Enabled;
  OldCursor := Glwin.Cursor;
  OldCustomCursor := Glwin.CustomCursor;

  if glwin is TGLWindowDemo then
  begin
    oldSwapFullScreen_Key := TGLWindowDemo(glwin).SwapFullScreen_Key;
    oldClose_charkey := TGLWindowDemo(glwin).Close_charkey;
    oldFpsShowOnCaption := TGLWindowDemo(glwin).FpsShowOnCaption;
  end;

  if glwin is TGLWindowNavigated then
  begin
    OldUseControls := TGLWindowNavigated(Glwin).UseControls;
  end;
end;

procedure TGLWindowState.SetState(Glwin: TGLWindow);
begin
  Glwin.SetCallbacksState(oldCallbacks);
  Glwin.Caption := oldCaption;
  Glwin.Userdata := oldUserdata;
  Glwin.AutoRedisplay := oldAutoRedisplay;
  Glwin.Fps.Active := oldFPSActive;
  Glwin.MainMenu := oldMainMenu;
  if Glwin.MainMenu <> nil then
    Glwin.MainMenu.Enabled := OldMainMenuEnabled;
  Glwin.Cursor := OldCursor;
  Glwin.CustomCursor := OldCustomCursor;

  if glwin is TGLWindowDemo then
  begin
    TGLWindowDemo(glwin).SwapFullScreen_Key := oldSwapFullScreen_Key;
    TGLWindowDemo(glwin).Close_charkey := oldClose_charkey;
    TGLWindowDemo(glwin).FpsShowOnCaption := oldFpsShowOnCaption;
  end;

  if glwin is TGLWindowNavigated then
  begin
    TGLWindowNavigated(Glwin).UseControls := OldUseControls;
  end;
end;

class procedure TGLWindowState.SetStandardState(glwin: TGLWindow;
  NewDraw, NewCloseQuery, NewResize: TGLWindowFunc;
  NewUserData: Pointer; NewAutoRedisplay: boolean; NewFPSActive: boolean;
  NewMainMenuEnabled: boolean;
  NewSwapFullScreen_Key: TKey;
  NewClose_charkey: char; NewFpsShowOnCaption, NewUseControls: boolean);
begin
  Glwin.SetCallbacksState(DefaultCallbacksState);
  Glwin.OnDraw := NewDraw;
  Glwin.OnCloseQuery := NewCloseQuery;
  Glwin.OnResize := NewResize;
  {Glwin.Caption := leave current value}
  Glwin.Userdata := NewUserdata;
  Glwin.AutoRedisplay := NewAutoRedisplay;
  Glwin.Fps.Active := NewFPSActive;
  if Glwin.MainMenu <> nil then
    Glwin.MainMenu.Enabled := NewMainMenuEnabled;
  {Glwin.MainMenu := leave current value}
  Glwin.Cursor := gcDefault;

  if glwin is TGLWindowDemo then
  begin
    TGLWindowDemo(glwin).SwapFullScreen_Key := NewSwapFullScreen_Key;
    TGLWindowDemo(glwin).Close_charkey := NewClose_charkey;
    TGLWindowDemo(glwin).FpsShowOnCaption := NewFpsShowOnCaption;
  end;

  if glwin is TGLWindowNavigated then
  begin
    TGLWindowNavigated(Glwin).UseControls := NewUseControls;
  end;
end;

procedure CloseQuery_Ignore(glwin: TGLWindow);
begin
end;

class procedure TGLWindowState.SetStandardNoCloseState(glwin: TGLWindow;
  NewDraw, NewResize: TGLWindowFunc;
  NewUserData: Pointer; NewAutoRedisplay: boolean; NewFPSActive: boolean;
  NewMainMenuEnabled: boolean;
  NewSwapFullScreen_Key: TKey;
  NewFpsShowOnCaption, NewUseControls: boolean);
begin
  SetStandardState(glwin,
    NewDraw, {$ifdef FPC_OBJFPC} @ {$endif} CloseQuery_Ignore, NewResize,
    NewUserData, NewAutoRedisplay, NewFPSActive,
    NewMainMenuEnabled,
    NewSwapFullScreen_Key, #0, NewFpsShowOnCaption, NewUseControls);
end;

{ GL Mode ---------------------------------------------------------------- }

constructor TGLMode.Create(AGLWindow: TGLWindow; AttribsToPush: TGLbitfield;
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
      if Button in Glwin.MousePressed then
        Glwin.EventMouseUp(Button);
    for Key := Low(Key) to High(Key) do
      if Glwin.KeysDown[Key] then
        Glwin.EventKeyUp(Key, #0);
    for C := Low(C) to High(C) do
      if Glwin.CharactersDown[C] then
        Glwin.EventKeyUp(K_None, C);
  end;

begin
 inherited Create;

 glwin := AGLWindow;

 FFakeMouseDown := false;
 FRestoreProjectionMatrix := true;
 FRestoreModelviewMatrix := true;
 FRestoreTextureMatrix := true;

 Check(not Glwin.Closed, 'ModeGLEnter cannot be called on a closed GLWindow.');

 oldWinState := TGLWindowState.Create(glwin);
 oldWinWidth := Glwin.Width;
 oldWinHeight := Glwin.Height;

 FPushPopGLWinMessagesTheme := APushPopGLWinMessagesTheme;
 if FPushPopGLWinMessagesTheme then
   oldGLWinMessagesTheme := GLWinMessagesTheme;

 Glwin.MakeCurrent;

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

 Glwin.PostRedisplay;
end;

destructor TGLMode.Destroy;
var btn: TMouseButton;
begin
 oldWinState.SetState(glwin);
 FreeAndNil(oldWinState);

 if FPushPopGLWinMessagesTheme then
   GLWinMessagesTheme := oldGLWinMessagesTheme;

 { Although it's forbidden to use TGLMode on Closed TGLWindow,
   in destructor we must take care of every possible situation
   (because this may be called in finally ... end things when
   everything should be possible). }
 if not Glwin.Closed then
 begin
   Glwin.MakeCurrent;

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
   if (oldWinWidth <> Glwin.Width) or
      (oldWinHeight <> Glwin.Height) then
    Glwin.EventResize;

   { udajemy ze wszystkie przyciski myszy jakie sa wcisniete sa wciskane wlasnie
     teraz }
   if FakeMouseDown then
     for btn := Low(btn) to High(btn) do
       if btn in Glwin.mousePressed then
         Glwin.EventMouseDown(btn);

   Glwin.PostRedisplay;

   Glwin.Fps.IgnoreNextIdleSpeed;
 end;

 inherited;
end;

{ TGLModeFrozenScreen ------------------------------------------------------ }

procedure FrozenImageDraw(glwin: TGLWindow);
var Mode: TGLModeFrozenScreen;
    Attribs: TGLbitfield;
begin
 Mode := TGLModeFrozenScreen(Glwin.UserData);

 { TODO:  I should build display list with this in each FrozenImageResize
   (Glwin.Width, Glwin.Height may change with time). }

 if (Cardinal(Glwin.Width ) > Mode.SavedScreenWidth ) or
    (Cardinal(Glwin.Height) > Mode.SavedScreenHeight) then
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
    glRectf(0, 0, Glwin.Width, Glwin.Height);
   end;
  finally glPopMatrix end;
 finally glPopAttrib end;
end;

constructor TGLModeFrozenScreen.Create(AGLWindow: TGLWindow;
  AttribsToPush: TGLbitfield; APushPopGLWinMessagesTheme: boolean;
  APolygonStipple: PPolygonStipple);
begin
 inherited Create(AGLWindow, AttribsToPush, APushPopGLWinMessagesTheme);

 FPolygonStipple := APolygonStipple;

 { We must do it before SaveScreen.
   Moreover, we must do it before we set our own projection below
   (calling EventResize) and before we set OnDraw to FrozenImageDraw
   (because we want that Glwin.FlushRedisplay calls original OnDraw). }
 Glwin.FlushRedisplay;

 TGLWindowState.SetStandardNoCloseState(AGLWindow,
   {$ifdef FPC_OBJFPC} @ {$endif} FrozenImageDraw,
   {$ifdef FPC_OBJFPC} @ {$endif} Resize2D,
   Self, false, AGLWindow.Fps.Active, false, K_None, false, false);

 { setup our 2d projection. We must do it before SaveScreen }
 Glwin.EventResize;

 dlScreenImage := SaveScreenWhole_ToDisplayList_noflush(GL_FRONT,
   SavedScreenWidth, SavedScreenHeight);
end;

destructor TGLModeFrozenScreen.Destroy;
begin
 inherited;
 { it's a little safer to call this after inherited }
 glFreeDisplayList(dlScreenImage);
end;

end.
