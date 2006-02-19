{
  Copyright 2003-2006 Michalis Kamburelis.

  This file is part of "Kambi's OpenGL Pascal units".

  "Kambi's OpenGL Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's OpenGL Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's OpenGL Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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

uses SysUtils, OpenGLh, GLWindow, KambiGLUtils, Images;

{ GLWindowState --------------------------------------------------------- }

type
  { }
  TGLWindowState = record
    { TGLWindow attributes }
    oldCallbacks: TGLWindowCallbacks;
    oldCaption: string;
    oldUserdata: Pointer;
    oldAutoRedisplay: boolean;
    oldFPSActive: boolean;
    oldMenuActive: boolean;
    oldMainMenu: TMenu;
    { TGLWindowDemo attributes }
    oldSwapFullScreen_Key: TKey;
    oldClose_charkey: char;
    oldFpsShowOnCaption: boolean;
    { TGLWindowNavigated attributes }
    oldUseNavigator: boolean;

    { When adding new attributes to TGLWindow that should be saved/restored,
      you must remember to
      1. expand this record with new fields
      2. expand routines GetGLWindowState, SetGLWindowState and
         SetStandardGLWindowState below. }
  end;

{ Get/Set sejwuja / ustawiaja wszystkie wlasciwosci okienka TGLWindow
  ktore moga sie zmieniac w czasie gdy okienko pozostaje not Closed.
  Te wlasciwosci sa zapamietywane w strukturze TGLWindowState.
  W ten sposob mozesz za pomoca Get/Set robic cos jak Push/Pop stanu
  wlasciwosci TGLWindow, mozesz tez robic w ten sposob kopiowanie
  wlasciwosci z jednego okienka do drugiego.

  Notka: pamietaj ze tylko referencja wartosci MainMenu jest kopiowana.
  Wiec 1. nie mozesz robic tak po prostu kopiowania wartosci MainMenu
  z jednego okienka do drugiego (zeby jej pozniej dwa razy nie robic
  Free), o ile jest ona <> nil.
  2. Nie mozesz zmieniac zawartosci MainMenu w czasie TGLMode.Create/Free.
  (chociaz mozesz podmieniac wartosc MainMenu na inna, pod warunkiem
  ze obie sa <> nil).
}
function GetGLWindowState(glwin: TGLWindow): TGLWindowState;
procedure SetGLWindowState(glwin: TGLWindow; const State: TGLWindowState);

{ SetStandardGLWindowState ustawia wszystkie wlasciwosci ktore sa
  zawarte w TGLWindowState. Ale pozwala w wygodniejszy sposob niz SetGLWindowState
  podac te wlasciwosci :
    czesc wlasciwosci jest pobierana jako prametry,
    czesc wlasciwosci jest pominieta -
      pominiete callbacki beda ustawione na nil,
      pominiete Caption bedzie zostawione takie jakie jest. }
procedure SetStandardGLWindowState(glwin: TGLWindow;
  NewDraw, NewCloseQuery, NewResize: TGLWindowFunc;
  NewUserData: Pointer; NewAutoRedisplay: boolean; NewFPSActive: boolean;
  NewMenuActive: boolean;
  NewSwapFullScreen_Key: TKey;
  NewClose_charkey: char; NewFpsShowOnCaption, NewUseNavigator: boolean);

{ SetStdNoCloseGLWindowState dziala jak SetStandardGLWindowState
  ale ustawia zawsze oldClose_charkey na #0 i NewCloseQuery
  na procedure bez zadnego kodu : begin end; W ten sposob uniemozliwia
  userowi zamkniecie okienka metodami WindowManagera lub dawanymi przez
  klase TGLWindowDemo. }
procedure SetStdNoCloseGLWindowState(glwin: TGLWindow;
  NewDraw, NewResize: TGLWindowFunc;
  NewUserData: Pointer; NewAutoRedisplay: boolean; NewFPSActive: boolean;
  NewMenuActive: boolean;
  NewSwapFullScreen_Key: TKey;
  NewFpsShowOnCaption, NewUseNavigator: boolean);

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
  public
    { Enter / Exit mode:

      Create wykonuje save stanu OpenGL'a i stanu glwin do klasy TGLMode.
      Destroy przywraca caly zesejwowany stan.

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
              @item(macierze projection, texture i modelview (czyli wszystkie)
                (nie, nie uzywamy matrix stack))
              @item stan PIXEL_STORE_*
            )
          )

        @item(
          zarowno wywolanie Enter jak i Exit ustawia aktywny kontekst OpenGL'a
          na kontekst okienka glwin)

        @item(
          W Enter wszystkie aktualnie wcisniete klawisze myszki sa sztucznie
          wylaczane przez wywolanie glwin.EventMouseUp.

          W Exit wszystkie aktualnie wcisniete klawisze myszki sa sztucznie
          wlaczane przez wywolanie glwin.EventMouseDown (juz PO przywroceniu
          oryginalnych callbackow okienka).

          W ten sposob callbacki okienka (ktore zapewne bedziesz podmienial
          na czas trwania ModeGLEnter..Exit) beda myslaly ze w momencie
          wejscia do Mode user puszcza wszystkie klawisze myszy a w momencie
          wyjscia z Mode zostana powiadomione o tym jakie klawisze myszy
          sa rzeczywiscie wcisniete. To jest pozadane bo inaczej jezeli
          podmienisz callbacki dla myszki na wlasne to oryginalne callbacki
          okienka np. nigdy sie nie dowiedza ze user puscil klawisze myszki
          w czasie gdy bylismy pomiedzy ModeGLEnter..Exit.)

        @item(
          Jezeli rozmiary okienka (Width, Height) ulegly zmianie pomiedzy Enter
          a Exit to w Exit wywolujemy sztucznie glwin.EventResize
          (juz PO przywroceniu oryginalnych callbackow okienka).
          Powod : jak wyzej - gdyby user zmienil rozmiary okienka w czasie
          trwania ModeGLEnter...Exit z podmienionymi callbackami to oryginalne
          callbacki moglyby sie nigdy nie dowiedziec o zmianie rozmiaru okna
          i nie zareagowac na to prawidlowo.)

        @item(
          W ModeEnter i Exit sa wywolywane glwin.PostRedisplay (bo spodziewam
          sie ze podmienisz callback OnDraw a wiec zdecydowanie bedziesz
          chcial odmalowac okienko).)

        @item(
          One more note --- ModeGLEnter nie moze byc uzyte na Closed okienku,
          oczywiscie. Wiec dla bezpiecznstwa jest w nim robiony
          Check(not glwin.Closed, ...).)
      )

      Co do obiektu savedMode to w ModeGLEnter jest on tworzony a w ModeGLExit
      niszczony i ustawiana na nil i to jest wszystko co mozesz o nim wiedziec
      z zewntarz tego modulu.

      @noAutoLinkHere }
    constructor Create(AGLWindow: TGLWindow; AttribsToPush: TGLbitfield);

    { @noAutoLinkHere }
    destructor Destroy; override;
  end;

type
  TGLModeFrozenScreen = class(TGLMode)
  private
    dlScreenImage: TGLuint;
    ScreenImage: TImage;
    FPolygonStipple: PPolygonStipple;
  public
    { This mode on enter catches current screen (with glwin.SaveScreen) then
      calls SetStdNoCloseGLWindowState with such OnDraw and OnResize private
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

      Between creation/destroy, glwin.UserData is used by this function
      for "private" purposes so you should not use it yourself.
      glwin.UserData will be restored after destroying this object
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
      be properly refreshed if window manager will request window redraw etc.)

      @noAutoLinkHere }
    constructor Create(AGLWindow: TGLWindow; AttribsToPush: TGLbitfield;
      APolygonStipple: PPolygonStipple);

    { @noAutoLinkHere }
    destructor Destroy; override;
  end;

implementation

uses KambiUtils;

{ GLWindowState --------------------------------------------------------------- }

function GetGLWindowState(glwin: TGLWindow): TGLWindowState;
begin
 result.oldCallbacks := glwin.GetCallbacksState;
 result.oldCaption := glwin.Caption;
 result.oldUserdata := glwin.Userdata;
 result.oldAutoRedisplay := glwin.AutoRedisplay;
 result.oldFPSActive := glwin.FpsActive;
 result.oldMenuActive := glwin.MenuActive;
 result.oldMainMenu := glwin.MainMenu;

 if glwin is TGLWindowDemo then
 begin
  result.oldSwapFullScreen_Key := TGLWindowDemo(glwin).SwapFullScreen_Key;
  result.oldClose_charkey := TGLWindowDemo(glwin).Close_charkey;
  result.oldFpsShowOnCaption := TGLWindowDemo(glwin).FpsShowOnCaption;
 end;

 if glwin is TGLWindowNavigated then
  result.oldUseNavigator := TGLWindowNavigated(glwin).UseNavigator;
end;

procedure SetGLWindowState(glwin: TGLWindow; const State: TGLWindowState);
begin
 glwin.SetCallbacksState(State.oldCallbacks);
 glwin.Caption := State.oldCaption;
 glwin.Userdata := State.oldUserdata;
 glwin.AutoRedisplay := State.oldAutoRedisplay;
 glwin.FpsActive := State.oldFPSActive;
 glwin.MenuActive := State.oldMenuActive;
 glwin.MainMenu := State.oldMainMenu;

 if glwin is TGLWindowDemo then
 begin
  TGLWindowDemo(glwin).SwapFullScreen_Key := State.oldSwapFullScreen_Key;
  TGLWindowDemo(glwin).Close_charkey := State.oldClose_charkey;
  TGLWindowDemo(glwin).FpsShowOnCaption := State.oldFpsShowOnCaption;
 end;

 if glwin is TGLWindowNavigated then
  TGLWindowNavigated(glwin).UseNavigator := State.oldUseNavigator;
end;

procedure SetStandardGLWindowState(glwin: TGLWindow;
  NewDraw, NewCloseQuery, NewResize: TGLWindowFunc;
  NewUserData: Pointer; NewAutoRedisplay: boolean; NewFPSActive: boolean;
  NewMenuActive: boolean;
  NewSwapFullScreen_Key: TKey;
  NewClose_charkey: char; NewFpsShowOnCaption, NewUseNavigator: boolean);
begin
 glwin.SetCallbacksState(DefaultCallbacksState);
 glwin.OnDraw := @NewDraw;
 glwin.OnCloseQuery := @NewCloseQuery;
 glwin.OnResize := @NewResize;
 {glwin.Caption := leave current value}
 glwin.Userdata := NewUserdata;
 glwin.AutoRedisplay := NewAutoRedisplay;
 glwin.FpsActive := NewFPSActive;
 glwin.MenuActive := NewMenuActive;
 {glwin.MainMenu := leave current value}

 if glwin is TGLWindowDemo then
 begin
  TGLWindowDemo(glwin).SwapFullScreen_Key := NewSwapFullScreen_Key;
  TGLWindowDemo(glwin).Close_charkey := NewClose_charkey;
  TGLWindowDemo(glwin).FpsShowOnCaption := NewFpsShowOnCaption;
 end;

 if glwin is TGLWindowNavigated then
  TGLWindowNavigated(glwin).UseNavigator := NewUseNavigator;
end;

procedure CloseQuery_Ignore(glwin: TGLWindow); begin end;

procedure SetStdNoCloseGLWindowState(glwin: TGLWindow;
  NewDraw, NewResize: TGLWindowFunc;
  NewUserData: Pointer; NewAutoRedisplay: boolean; NewFPSActive: boolean;
  NewMenuActive: boolean;
  NewSwapFullScreen_Key: TKey;
  NewFpsShowOnCaption, NewUseNavigator: boolean);
begin
 SetStandardGLWindowState(glwin,
   NewDraw, CloseQuery_Ignore, NewResize,
   NewUserData, NewAutoRedisplay, NewFPSActive,
   NewMenuActive,
   NewSwapFullScreen_Key, #0, NewFpsShowOnCaption, NewUseNavigator);
end;

{ GL Mode ---------------------------------------------------------------- }

constructor TGLMode.Create(AGLWindow: TGLWindow; AttribsToPush: TGLbitfield);
var btn: TMouseButton;
begin
 inherited Create;

 glwin := AGLWindow;

 Check(not glwin.Closed, 'ModeGLEnter cannot be called on a closed GLWindow.');

 oldWinState := GetGLWindowState(glwin);
 oldWinWidth := glwin.Width;
 oldWinHeight := glwin.Height;

 { udajemy ze wszystkie przyciski myszy jakie byly wcisniete sa puszczane.
   (pamietajmy ze przed EventXxx musi byc MakeCurrent) }
 glwin.MakeCurrent;
 for btn := Low(btn) to High(btn) do
  if btn in glwin.mousePressed then
   glwin.EventMouseUp(btn);

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

 glwin.PostRedisplay;
end;

destructor TGLMode.Destroy;
var btn: TMouseButton;
begin
 SetGLWindowState(glwin, oldWinState);

 glwin.MakeCurrent;

 { restore OpenGL state }
 LoadPixelStoreUnpack(oldPixelStoreUnpack);
 glMatrixMode(GL_PROJECTION); glLoadMatrix(oldProjectionMatrix);
 glMatrixMode(GL_TEXTURE);    glLoadMatrix(oldTextureMatrix);
 glMatrixMode(GL_MODELVIEW);  glLoadMatrix(oldModelviewMatrix);
 glMatrixMode(oldMatrixMode);
 glPopAttrib;

 { (pamietajmy ze przed EventXxx musi byc MakeCurrent) - juz zrobilismy
   je powyzej }
 { Gdy byly aktywne nasze callbacki mogly zajsc zdarzenia co do ktorych
   oryginalne callbacki chcialyby byc poinformowane. Np. OnResize. }
 if (oldWinWidth <> glwin.Width) or
    (oldWinHeight <> glwin.Height) then
  glwin.EventResize;

 { udajemy ze wszystkie przyciski myszy jakie sa wcisniete sa wciskane wlasnie
   teraz }
 for btn := Low(btn) to High(btn) do
  if btn in glwin.mousePressed then
   glwin.EventMouseDown(btn);

 glwin.PostRedisplay;

 inherited;
end;

{ TGLModeFrozenScreen ------------------------------------------------------ }

procedure FrozenImageDraw(glwin: TGLWindow);
var Mode: TGLModeFrozenScreen;
    Attribs: TGLbitfield;
begin
 Mode := TGLModeFrozenScreen(glwin.UserData);

 { sorry - I should build display list with this in each FrozenImageResize
   (glwin.Width, glwin.Height may change with time). }

 if (glwin.Width > Mode.ScreenImage.Width) or
    (glwin.Height > Mode.ScreenImage.Height) then
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
    glRectf(0, 0, glwin.Width, glwin.Height);
   end;
  finally glPopMatrix end;
 finally glPopAttrib end;
end;

constructor TGLModeFrozenScreen.Create(AGLWindow: TGLWindow;
  AttribsToPush: TGLbitfield; APolygonStipple: PPolygonStipple);
begin
 inherited Create(AGLWindow, AttribsToPush);

 FPolygonStipple := APolygonStipple;

 { We must do it before SaveScreen.
   Moreover, we must do it before we set our own projection below
   (calling EventResize) and before we set OnDraw to FrozenImageDraw
   (because we want that glwin.FlushRedisplay calls original OnDraw). }
 glwin.FlushRedisplay;

 SetStdNoCloseGLWindowState(AGLWindow, FrozenImageDraw, Resize2D,
   Self, false, false, AGLWindow.FPSActive, K_None, false, false);

 { setup our 2d projection. We must do it before SaveScreen }
 glwin.EventResize;

 ScreenImage := SaveScreen_noflush(GL_FRONT);
 dlScreenImage := ImageDrawToDispList(ScreenImage);
end;

destructor TGLModeFrozenScreen.Destroy;
begin
 inherited;
 { it's a little safer to call this after inherited }
 glFreeDisplayList(dlScreenImage);
 FreeAndNil(ScreenImage);
end;

end.
