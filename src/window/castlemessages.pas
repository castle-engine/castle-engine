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

(*
  @abstract(Dialog boxes (asking user for confirmation, question,
  simple text input etc.) inside @link(TCastleWindowBase).)

  Features:

  @unorderedList(
    @item(MessageInputXxx ask user to enter some text.)

    @item(The dialog boxes have vertical scroll bar, displayed when needed.
      So don't worry about displaying long text. Scroll bar can be operated
      with keys (up/down, ctrl+up/down, page up/down, home/end) and mouse
      (drag the scroll bar, or click below/above it).)

    @item(Long text lines are automatically broken, so don't worry about
      displaying text with long lines.

      We will try to break text only at whitespace. Note that our "line breaking"
      works correctly for all fonts (see @link(TMessagesTheme.Font)),
      even if font is not fixed-character-width font.

      If you pass a text as a single string parameter, then our "line breaking"
      works  correctly even for text that already contains newline characters
      (they are correctly recognized as forcing line break).

      If you pass a text as an "array of string" or TStringList, it's expected
      that strings inside don't contain newline characters anymore. It's undefined
      what will happen (i.e. whether they will be correctly broken) otherwise.
      Of course, TStringList contents used to pass text to MessageXxx will never
      be modified in any way.)

    @item(User is allowed to resize the window while MessageXxx works.
      (As long as TCastleWindowBase.ResizeAllowed = raAllowed, of course.)
      Long lines are automatically broken taking into account current window
      width.)

    @item(You can configure dialog boxes look using CastleMessagesTheme variable.
      For example, you can make the dialog box background partially transparent.)
  )

  Call MessageXxx functions only when Window.Closed = false.
  Note that MessageXxx will do Window.MakeCurrent (probably more than once).
  Calling MessageXxx requires one free place on OpenGL attrib stack.

  Notes about implementation:

  @unorderedList(
    @item(
      It's implemented using CastleWindowModes approach. Which means that when you call
      some MessageXxx procedure, it temporarily switches all TCastleWindowBase callbacks
      (OnDraw, OnPress, OnRelease, OnIdle etc.) for it's own.
      So you can be sure that e.g. no TCastleWindow callbacks that you registered in your
      own programs/units will be called while MessageXxx works.
      When message box ends (e.g. for simple MessageOk, this happens when user
      will accept it by Enter key or clicking OK), original callbacks are
      restored.

      This way you can call MessageXxx procedures in virtually any place of your
      program, and things will magically work --- inside MessageXxx we wait until
      user answers the dialog box.

      This also means that this units is only able to work when CastleWindow unit
      implements Application.ProcessMessage routine. For now this means
      that CastleWindow unit must not work on top of glut (CASTLE_WINDOW_GLUT
      must not be defined while compiling CastleWindow), other CastleWindow implementations
      are OK.)

    @item(
      Be careful if you use Application callbacks, OnIdle / OnTimer.
      They are tied to GL window manager, Glwn, not to any particular window,
      and so they continue to work even while we're inside MessageXxx procedure.

      While this can be useful, you should be careful when implementing these
      Application callbacks. When they access some TCastleWindowBase instance, it may be currently
      inside MessageXxx call.)

    @item(
      The whole MessageXxx is supposed to start and end on an open TCastleWindowBase
      instance, so no Open or Close methods of TCastleWindowBase will be called during
      MessageXxx.

      OnCloseQuery during MessageXxx calls prevents user from exiting the program,
      actually OnCloseQuery callback is simple no-op. Which means, among other things,
      that you can safely use MessageXxx inside your own OnCloseQuery, for example
      you can implement you OnCloseQuery like
      @longCode(#
  if MessageYesNo('Are you sure ?') then
    Window.Close;
#))
  )
*)

unit CastleMessages;

{ TODO
  - przydalby sie jeszcze kursor dla MessageInput[Query]
  - this should be implemented on top of CastleWindowModes.TGLModeFrozenScreen,
    this would simplify implementation a bit
}

{$I castleconf.inc}

interface

uses Classes, CastleWindow, CastleGLUtils, GL, GLU, GLExt, CastleUtils, CastleGLBitmapFonts,
  CastleStringUtils, CastleVectors, CastleKeysMouse;

type
  { Specifies text alignment for MessageXxx functions in
    @link(CastleMessages) unit. }
  TTextAlign = (taLeft, taMiddle, taRight);

{ Ask user for simple confirmation. In other words, this is the standard
  and simplest "OK" dialog box.

  @groupBegin }
procedure MessageOK(Window: TCastleWindowBase; const s: string;
  textalign: TTextAlign = taMiddle); overload;
procedure MessageOK(Window: TCastleWindowBase;  const SArray: array of string;
  textalign: TTextAlign = taMiddle); overload;
procedure MessageOK(Window: TCastleWindowBase;  textlist: TStringList;
  textalign: TTextAlign = taMiddle); overload;
{ @groupEnd }

{ Ask user to input a string.
  User must give an answer (there is no "Cancel" button) --- see
  MessageInputQuery if you want "Cancel" button.
  @param AnswerMaxLen 0 (zero) means that there's no maximum answer length.

  @groupBegin }
function MessageInput(Window: TCastleWindowBase; const s: string;
  textalign: TTextAlign = taMiddle;
  const answerDefault: string = '';
  answerMinLen: integer = 0;
  answerMaxLen: integer = 0;
  const answerAllowedChars: TSetOfChars = AllChars): string; overload;
function MessageInput(Window: TCastleWindowBase; textlist: TStringList;
  textalign: TTextAlign = taMiddle;
  const answerDefault: string = '';
  answerMinLen: integer = 0;
  answerMaxLen: integer = 0;
  const answerAllowedChars: TSetOfChars = AllChars): string; overload;
{ @groupEnd }

{ Ask user to input a string, or cancel.
  Returns @true and sets Answer if user accepted some text.
  Note that initial Answer value is the answer proposed to the user.
  @param AnswerMaxLen 0 (zero) means that there's no maximum answer length.

  @groupBegin }
function MessageInputQuery(Window: TCastleWindowBase; const s: string;
  var answer: string; textalign: TTextAlign;
  answerMinLen: integer = 0;
  answerMaxLen: integer = 0;
  const answerAllowedChars: TSetOfChars = AllChars): boolean; overload;
function MessageInputQuery(Window: TCastleWindowBase; textlist: TStringList;
  var answer: string; textalign: TTextAlign;
  answerMinLen: integer = 0;
  answerMaxLen: integer = 0;
  const answerAllowedChars: TSetOfChars = AllChars): boolean; overload;
{ @groupEnd }

{ Ask user to input a single character from a given set.
  This is good when user has a small, finite number of answers for some question,
  and each answer can be assigned some key. For example, MessageYesNo is
  built on top of this procedure: keys "y" and "n" are allowed characters that
  user can input.

  @param(ClosingInfo This is a text to indicate to user what keys can be pressed
    and what they mean. ClosingInfo is displayed below basic Text of the
    message, and in a different color. If may be '' if you don't want this.)

  @param(IgnoreCase This means that case of letters in AllowedChars
    will be ignored. For example, you can include only uppercase 'A' in
    AllowedChars. Or you can include lowercase 'a'. Or you can include
    both. It doesn't matter --- we will always accept both lower and upper case.

    Also, when character is returned, it's always uppercased.
    So you can't even know if user pressed lower of upper case letter.)

  @groupBegin }
function MessageChar(Window: TCastleWindowBase; const s: string;
  const AllowedChars: TSetOfChars;
  const ClosingInfo: string;
  textalign: TTextAlign = taMiddle;
  IgnoreCase: boolean = false): char; overload;
function MessageChar(Window: TCastleWindowBase;  const SArray: array of string;
  const AllowedChars: TSetOfChars;
  const ClosingInfo: string;
  textalign: TTextAlign = taMiddle;
  IgnoreCase: boolean = false): char; overload;
function MessageChar(Window: TCastleWindowBase;  textlist: TStringList;
  const AllowedChars: TSetOfChars;
  const ClosingInfo: string;
  textalign: TTextAlign = taMiddle;
  IgnoreCase: boolean = false): char; overload;
{ @groupEnd }

{ Ask user to press any key, return this key as Keys.TKey.

  Never returns K_None (which means that keys that cannot be interpreted
  as Keys.TKey will be ignored, and will not close the dialog box).

  @groupBegin }
function MessageKey(Window: TCastleWindowBase; const S: string;
  const ClosingInfo: string; TextAlign: TTextAlign): TKey; overload;

function MessageKey(Window: TCastleWindowBase; const SArray: array of string;
  const ClosingInfo: string; TextAlign: TTextAlign): TKey; overload;

function MessageKey(Window: TCastleWindowBase; TextList: TStringList;
  const ClosingInfo: string; TextAlign: TTextAlign): TKey; overload;
{ @groupEnd }

{ Ask user to press any key or mouse button or mouse wheel, and return it.
  The natural use for this is to allow user to configure
  keybindings of your program, like for TInputShortcut.

  TODO: for historical reasons, in case of key events,
  it for now returns only events that can be represented as TKey
  (so Event.Key <> K_None, as long as Event.EventType = itKey).

  @groupBegin }
procedure MessageKeyMouse(Window: TCastleWindowBase; const S: string;
  const ClosingInfo: string; TextAlign: TTextAlign;
  out Event: TInputPressRelease); overload;

procedure MessageKeyMouse(Window: TCastleWindowBase; TextList: TStringList;
  const ClosingInfo: string; TextAlign: TTextAlign;
  out Event: TInputPressRelease); overload;
{ @groupEnd }

function MessageYesNo(Window: TCastleWindowBase; const s: string;
  textalign: TTextAlign = taMiddle): boolean; overload;
function MessageYesNo(Window: TCastleWindowBase;  const SArray: array of string;
  textalign: TTextAlign = taMiddle): boolean; overload;
function MessageYesNo(Window: TCastleWindowBase;  textlist: TStringList;
  textalign: TTextAlign = taMiddle): boolean; overload;

{ Ask user to input an unsigned integer.

  This is actually a shortcut for simple MessageInput with answerMinLength = 1
  and AllowedChars = ['0'..'9'], since this guarantees input of some unsigned
  integer number.

  Note that AnswerDefault below may be given as Cardinal or as a string.
  The latter is useful if you want the default answer to be '', i.e. empty string
  --- no default answer.

  @groupBegin }
function MessageInputCardinal(Window: TCastleWindowBase; const s: string;
  TextAlign: TTextAlign; const AnswerDefault: string): Cardinal; overload;
function MessageInputCardinal(Window: TCastleWindowBase; const s: string;
  TextAlign: TTextAlign; AnswerDefault: Cardinal): Cardinal; overload;

function MessageInputQueryCardinal(Window: TCastleWindowBase; const Title: string;
  var Value: Cardinal; TextAlign: TTextAlign): boolean;
{ @groupEnd }

{ Ask user to input a value in hexadecimal.
  Give MaxWidth = 0 to say that there is no maximum width. }
function MessageInputQueryCardinalHex(Window: TCastleWindowBase; const Title: string;
  var Value: Cardinal; TextAlign: TTextAlign; MaxWidth: Cardinal): boolean;

{ Ask user to input a floating-point number.

  If you give non-empty ValueAsString, it will be used to show
  the initial value for the user. Otherwise, we will just show
  FloatToStr(Value), which sometimes may be too ugly.
  For example Value = 0.01 cannot be precisely represented as a floating point
  number, and FloatToStr shows that this is really something like 0.0099xxxxx.

  @groupBegin }
function MessageInputQuery(Window: TCastleWindowBase; const Title: string;
  var Value: Extended; TextAlign: TTextAlign; const ValueAsString: string = ''): boolean;
function MessageInputQuery(Window: TCastleWindowBase; const Title: string;
  var Value: Single; TextAlign: TTextAlign; const ValueAsString: string = ''): boolean;
{$ifndef EXTENDED_EQUALS_DOUBLE}
function MessageInputQuery(Window: TCastleWindowBase; const Title: string;
  var Value: Double; TextAlign: TTextAlign; const ValueAsString: string = ''): boolean;
{$endif not EXTENDED_EQUALS_DOUBLE}
{ @groupEnd }

function MessageInputQueryVector3Single(
  Window: TCastleWindowBase; const Title: string;
  var Value: TVector3Single; TextAlign: TTextAlign): boolean;

function MessageInputQueryVector4Single(
  Window: TCastleWindowBase; const Title: string;
  var Value: TVector4Single; TextAlign: TTextAlign): boolean;

type
  TMessagesTheme = record
    { Color of the inside area in the message rectangle.
      If RectColor[3] <> 1.0, then it will be nicely blended on the screen. }
    RectColor: TVector4f;

    { Color of the frame of the rectangle. }
    RectBorderCol: TVector3f;
    ScrollBarCol: TVector3f;
    ClosingInfoCol: TVector3f;
    AdditionalStrCol: TVector3f;
    TextCol: TVector3f;

    { Font used by procedures in this unit.
      Nil means "use default font".
      This font doesn't have to be mono-spaced.  }
    Font: TGLBitmapFontAbstract;
  end;

const
  MessagesTheme_Default: TMessagesTheme =
  ( RectColor: (0, 0, 0, 1);           { = Black3Single }
    RectBorderCol: (1, 1, 0.33);       { = Yellow3Single }
    ScrollBarCol: (0.5, 0.5, 0.5);     { = Gray3Single }
    ClosingInfoCol: (1, 1, 0.33);      { = Yellow3Single }
    AdditionalStrCol: (0.33, 1, 1);    { = LightCyan3Single }
    TextCol: (1, 1, 1);                { = White3Single }
    Font: nil;
  );

  { }
  MessagesTheme_TypicalGUI: TMessagesTheme =
  ( RectColor: (0.75, 0.75, 0.66, 1);
    RectBorderCol: (0.87, 0.87, 0.81);
    ScrollBarCol: (0.87, 0.87, 0.81);
    ClosingInfoCol: (0.4, 0, 1);
    AdditionalStrCol: (0, 0.4, 0);
    TextCol: (0, 0, 0);
    Font: nil;
  );

var
  { The way MessageXxx procedures in this unit are displayed.
    By default it is equal to MessagesTheme_Default.

    Note that all procedures in this unit are re-entrant (safe for recursive
    calls, and in threads), unless you modify this variable. When you modify
    this from one thread, be sure that you don't currently use it in some
    MessageXxx (in other thread, or maybe you're in Application.OnIdle or such that
    is called while other window is in MessageXxx). }
  MessagesTheme: TMessagesTheme;

implementation

uses CastleBitmapFont_BVSansMono_m18, CastleImages,
  CastleClassUtils, SysUtils, CastleWindowModes, CastleLog, CastleGLImages;

{ TIntRect ------------------------------------------------------------------- }

type
  TIntRect = array[0..1]of TVector2Integer;

const
  IntRectEmpty: TIntRect = ((0, 0), (0, 0));

function IntRect(X1, Y1, X2, Y2: Integer): TIntRect;
begin
  Result[0, 0] := X1;
  Result[0, 1] := Y1;
  Result[1, 0] := X2;
  Result[1, 1] := Y2;
end;

function RectWidth(const r: TIntRect): Cardinal;
begin
  Result := r[1, 0] - r[0, 0];
end;

function RectHeight(const r: TIntRect): Cardinal;
begin
  Result := r[1, 1] - r[0, 1];
end;

{ It's grow, or shrink (when GrowValue < 0).
  Just remember to preserve basic TIntRect type assumptions
  (Rect[0, 0] <= Rect [1, 0], Rect[0, 1] <= Rect [1, 1]),
  so don't shrink it too much. }
function GrowRect(const r: TIntRect; GrowValue: Integer): TIntRect;
begin
  Result[0, 0] := R[0, 0] - GrowValue;
  Result[0, 1] := R[0, 1] - GrowValue;
  Result[1, 0] := R[1, 0] + GrowValue;
  Result[1, 1] := R[1, 1] + GrowValue;
end;

function CenteredRect(const R: TIntRect; w, h: Cardinal): TIntRect;
begin
  { We're casting W, H to integer. They are declared as Cardinal only
    to produce some compiler RunTime checks in debug mode. }
  Result[0, 0] := R[0, 0] + (R[1, 0] - R[0, 0] - Integer(W)) div 2;
  Result[0, 1] := R[0, 1] + (R[1, 1] - R[0, 1] - Integer(H)) div 2;
  Result[1, 0] := Result[0, 0] + Integer(W);
  Result[1, 1] := Result[0, 1] + Integer(H);
end;

function PointInRect(const x, y: Integer; const r: TIntRect): boolean;
begin
  Result := (r[0, 0] <= x) and (x < r[1, 0]) and
            (r[0, 1] <= y) and (y < r[1, 1]);
end;

procedure GLRectangleWithBorder(const R: TIntRect;
  const InsideCol, BorderCol: TVector4f);
begin
  CastleGLUtils.GLRectangleWithBorder(R[0, 0], R[0, 1], R[1, 0], R[1, 1],
    InsideCol, BorderCol);
end;

{ end of TIntRect utils ------------------------------------------------------ }

const
  DrawMessg_BoxMargin = 10;
  DrawMessg_WindMargin = 10;
  DrawMessg_ScrollBarWholeWidth = 20;

{ TMessageData ------------------------------------------------------------ }

type
  TMessageData = class
  private
    FFloatShiftY: Single;
    FSAdditional: string;
  public
    { zmienne ktore mowia jak i co rysowac }
    { Broken (using Font.BreakLines) MessgText (calculated in every ResizeMessg). }
    Broken_MessgText: TStringList;
    { Ignored (not visible) if ClosingInfo = ''.
      Else broken ClosingInfo (calculated in every ResizeMessg). }
    Broken_ClosingInfo: TStringList;
    { Ignored (not visible) if not DrawAdditional.
      Else broken SAdditional (calculated in every ResizeMessg). }
    Broken_SAdditional: TStringList;

    { Maximum of Font.MaxTextWidth(Broken_Xxx) for all visible Broken_Xxx.
      Calculated in every ResizeMessg. }
    MaxLineWidth: integer;
    { Sum of all Broken_MessgText.Count + Broken_SAdditional.Count.
      In other words, all lines that are scrolled by the scrollbar. }
    AllScrolledLinesCount: integer;
    { linie w calosci widoczne na ekranie (przydatne do obslugi page up / down),
      sposrod linii scrolled by the scrollbar (MessgText and SAdditional).
      Calculated in every ResizeMessg. }
    VisibleScrolledLinesCount: integer;

    { shiftY = o ile pixeli w gore przesunac caly napis. Przechowywane w
      formacie float zeby umozliwic time-based zmiane w idleMessg.
      Uwaga - SetFloatShiftY jest tak napisane ze SetFloatShitY(..,FloatShiftY)
      spowoduje ew. clamp wartosci shiftY, tzn. mozesz tego uzywac np.
      gdy zmienisz min/maxShiftY }
    function ShiftY: integer;
    property FloatShiftY: Single read FFloatshiftY;
    procedure SetFloatShiftY(Window: TCastleWindowBase; newValue: Single);

    procedure SetFloatShiftYPageDown(Window: TCastleWindowBase);
    procedure SetFloatShiftYPageUp(Window: TCastleWindowBase);
  public
    { minimalne i maksymalne sensowne wartosci dla shiftY }
    minShiftY, maxShiftY: integer;

    { normalne min/maxShiftY dba zeby na ekranie zawsze
      bylo mozliwie duzo linii, zeby nie bylo tak ze zostaje na samym dole ekranu
      tylko malo linii poczatkowych linii albo tylko malo koncowych linii
      na samej gorze ekranu a reszta recta jest pusta.
      Zakres minShiftY .. notprettyMaxShiftY zmienia ten punkt widzenia.
      W ten sposob minShiftY w tym zakresie jest proporcjonalne to
      pozycji pierwszej widocznej linii w AllScrolledLinesCount.
      (co jest nam potrzebne przy rysowaniu ScrollBara) }
    notprettyMaxShiftY: integer;

    { rzeczy zwiazane ze ScrollBarem; na poczatku ScrollBarVisible := false;
      potem jest uaktualniane w drawMessg na false lub true;
      - jesli na true to jest tez inicjowany
        przewX1, Y1, X2, Y2 (obszar w ktorym porusza sie ScrollBar)
        i przewVisY1 , przewVisY2 (ograniczenia w tym obszarze w ktorych jest ScrollBar teraz)
        (wspolrzedne sa rosnace, tzn. xxxX1 <= xxxX2, xxxY1 <= xxxY2)
      - jesli na false to przewDrag := false, gdyby jakims sposobem ScrollBar zniknal
        w czasie gdy bedziemy go przeciagac mysza (chociaz chwilowo nie wyobrazam sobie
        jak mialoby sie to stac; ale przed takim czyms lepiej sie zabezpieczyc na
        przyszlosc). }
    ScrollBarVisible: boolean; { Calculated in every ResizeMessg. }
    ScrollBarRect: TIntRect;
    przewVisY1, przewVisY2: Single;
    ScrollBarDragging: boolean;

    { set in MessageCore, readonly afterwards }
    OnUserPress: TInputPressReleaseFunc;
    { If @true then OnUserPress with itMouseButton will occur only when user clicks
      inside the rectangular area of our message window.
      If @false, it will occur on mouse down event
      anywhere on our window. }
    UserMouseDownOnlyWithinRect: boolean;

    { Wspolrzedne calego recta messaga, tzn. poza MessageRect nie rysujemy
      naszego okienka (tam powinno byc widoczne tlo, czyli list_DrawBG). }
    WholeMessageRect: TIntRect;

    { set in MessageCore, readonly afterwards for various callbacks }

    { przekazana jako parametr lista stringow; tylko do odczytu,
      nie modyfikuj tego zawartosci ! }
    MessgText: TStringList;
    Font: TGLBitmapFontAbstract;   { font ktorego uzyc }
    DrawBG: TGLImage;            { zapamietane tlo okienka }
    align: TTextAlign;
    DrawAdditional: boolean; { czy wypisywac SAdditional }
    ClosingInfo: string;     { ClosingInfo : jesli '' to nie bedzie wypisywane }

    { zmienne ktore umozliwiaja inna komunikacje miedzy MessageCore a callbackami }
    answered: boolean;              { ustaw na true aby zakonczyc }

    { bedzie wyswietlany jezeli DrawAdditional }
    property SAdditional: string read FSAdditional;
    procedure SetSAdditional(Window: TCastleWindowBase; const value: string);
  public
    UserData: Pointer;

    { Calculate height in pixels needed to draw ClosingInfo.
      Returns 0 if ClosingInfo = ''. Uses ClosingInfo and Broken_ClosingInfo
      (and of course Font), so be sure to init these things before
      calling this. }
    function ClosingInfoBoxHeight: Integer;
  end;

function TMessageData.ShiftY: integer;
begin result := Round(FloatShiftY) end;

procedure TMessageData.SetFloatShiftY(Window: TCastleWindowBase; newValue: Single);
begin
 Clamp(newValue, minShiftY, maxShiftY);
 if newValue <> FloatShiftY then
 begin
  FFloatShiftY := newValue;
  Window.PostRedisplay;
 end;
end;

procedure TMessageData.SetFloatShiftYPageDown(Window: TCastleWindowBase);
var
  PageHeight: Single;
begin
  PageHeight := VisibleScrolledLinesCount * Font.RowHeight;
  SetFloatShiftY(Window, ShiftY + PageHeight);
end;

procedure TMessageData.SetFloatShiftYPageUp(Window: TCastleWindowBase);
var
  PageHeight: Single;
begin
  PageHeight := VisibleScrolledLinesCount * Font.RowHeight;
  SetFloatShiftY(Window, ShiftY - PageHeight);
end;

procedure TMessageData.SetSAdditional(Window: TCastleWindowBase; const value: string);
begin
 FSAdditional := value;
 Window.PostRedisplay;
 {moznaby zoptymalizowac rzeczy gdyby robic tutaj tylko czesc tego co jest
  robione w resizeMessg. }
 Window.EventResize; { zeby zlamal SAdditional }
end;

function TMessageData.ClosingInfoBoxHeight: Integer;
begin
  if ClosingInfo <> '' then
  begin
    Result := 2 * DrawMessg_BoxMargin +
      Font.RowHeight * Broken_ClosingInfo.Count +
      1 { width of horizontal line };
  end else
    Result := 0;
end;

{ MessageCore callbacks -------------------------------------------------- }

procedure ResizeMessg(Window: TCastleWindowBase);
var
  MD: TMessageData;
  { width at which we should break our string lists md.Broken_Xxx }
  BreakWidth: integer;
  WindowScrolledHeight: Integer;
begin
 glViewport(0, 0, Window.Width, Window.Height);
 OrthoProjection(0, Window.Width, 0, Window.Height);

 { calculate BreakWidth. We must here always subtract
   DrawMessg_ScrollBarWholeWidth to be on the safe side, because we don't know
   yet is md.ScrollBarVisible. }
 BreakWidth := max(0, Window.Width -DrawMessg_BoxMargin*2
                                -DrawMessg_WindMargin*2
                                -DrawMessg_ScrollBarWholeWidth);

 md := TMessageData(Window.UserData);

 with md do
 begin
  { calculate MaxLineWidth and AllScrolledLinesCount }

  { calculate Broken_MessgText }
  Broken_MessgText.Clear;
  font.BreakLines(MessgText, Broken_MessgText,  BreakWidth);
  MaxLineWidth := font.MaxTextWidth(Broken_MessgText);
  AllScrolledLinesCount := Broken_MessgText.count;

  if ClosingInfo <> '' then
  begin
   { calculate Broken_ClosingInfo }
   Broken_ClosingInfo.Clear;
   Font.BreakLines(ClosingInfo, Broken_ClosingInfo, BreakWidth);
   MaxLineWidth := max(MaxLineWidth, font.MaxTextWidth(Broken_ClosingInfo));
  end;

  if DrawAdditional then
  begin
   { calculate Broken_SAdditional }
   Broken_SAdditional.Clear;
   Font.BreakLines(SAdditional, Broken_SAdditional, BreakWidth);
   { It's our intention that if DrawAdditional then *always*
     at least 1 line of additional (even if it's empty) will be shown.
     That's because SAdditional is the editable text for the user,
     so there should be indication of "empty line". }
   if Broken_SAdditional.count = 0 then Broken_SAdditional.Add('');
   MaxLineWidth := max(MaxLineWidth, font.MaxTextWidth(Broken_SAdditional));
   AllScrolledLinesCount += Broken_SAdditional.count;
  end;

  { Now we have MaxLineWidth and AllScrolledLinesCount calculated }

  { Calculate WindowScrolledHeight --- number of pixels that are controlled
    by the scrollbar. }
  WindowScrolledHeight := Window.Height
    - DrawMessg_BoxMargin * 2
    - DrawMessg_WindMargin * 2
    - ClosingInfoBoxHeight;

  { calculate VisibleScrolledLinesCount, ScrollBarVisible }

  VisibleScrolledLinesCount := Clamped(WindowScrolledHeight div Font.RowHeight,
    0, AllScrolledLinesCount);
  ScrollBarVisible := VisibleScrolledLinesCount < AllScrolledLinesCount;
  { if ScrollBarVisible changed from true to false then we must make
    sure that ScrollBarDragging is false. }
  if not ScrollBarVisible then
   ScrollBarDragging := false;

  { Note that when not ScrollBarVisible,
    then VisibleScrolledLinesCount = AllScrolledLinesCount,
    then minShiftY = 0
    so minShiftY = maxShiftY,
    so FloatShiftY will always be 0. }
  minShiftY := -Font.RowHeight *
    (AllScrolledLinesCount - VisibleScrolledLinesCount);
  { maxShiftY jest stale ale to nic; wszystko bedziemy pisac
    tak jakby maxShiftY tez moglo sie zmieniac - byc moze kiedys zrobimy
    z tej mozliwosci uzytek. }
  maxShiftY := 0;
  notprettyMaxShiftY := minShiftY + Font.RowHeight * AllScrolledLinesCount;

  { min / maxShift mogly sie zmienic wiec trzeba sie upewnic ze shiftY ciagle jest
    w odpowiednim zakresie }
  SetFloatShiftY(Window, FloatShiftY);
 end;
end;

procedure PressMessg(Window: TCastleWindowBase; const Event: TInputPressRelease);
var
  md: TMessageData;
  KeyHandled: boolean;
  mx, my: integer; { mousex, y przetlumaczone na wspolrzedne OpenGL'a tego okienka }
begin
  md := TMessageData(Window.userdata);
  case Event.EventType of
    itKey:
      begin
        KeyHandled := false;

        { if not ScrollBarVisible then there is no point in doing
          md.setFloatShiftY (because always
          minShiftY = maxShiftY = FloatShiftY = 0, see ResizeMessg comments).

          This way I allow md.OnUserKeyDown to handle K_PageDown, K_PageUp,
          K_Home and K_End keys. And this is very handy for MessageKey,
          when it's used e.g. to allow user to choose any TKey.
          Otherwise MessageKey would not be able to return
          K_PageDown, K_PageUp, etc. keys. }
        if MD.ScrollBarVisible then
        begin
          case Event.Key of
            K_PageUp:   begin md.setFloatShiftYPageUp(Window);         KeyHandled := true; end;
            K_PageDown: begin md.setFloatShiftYPageDown(Window);       KeyHandled := true; end;
            K_Home:     begin md.setFloatShiftY(Window, md.minShiftY); KeyHandled := true; end;
            K_End:      begin md.setFloatShiftY(Window, md.maxShiftY); KeyHandled := true; end;
          end;
        end;

        if not KeyHandled then
        begin
          if Assigned(md.OnUserPress) then
            md.OnUserPress(Window, Event);
        end;
      end;
    itMouseButton:
      begin
        mx := Window.mouseX;
        my := Window.height-Window.mouseY;

        if (Event.MouseButton = mbLeft) and
          md.ScrollBarVisible and PointInRect(mx, my, md.ScrollBarRect) then
        begin
          if my < md.przewVisY1 then md.SetFloatShiftYPageDown(Window) else
          if my > md.przewVisY2 then md.SetFloatShiftYPageUp(Window) else
          begin
            md.ScrollBarDragging := true;
          end;
        end else
        if (not MD.UserMouseDownOnlyWithinRect) or
           PointInRect(mx, my, md.WholeMessageRect) then
        begin
          if Assigned(MD.OnUserPress) then
            MD.OnUserPress(Window, Event);
        end;
      end;
    itMouseWheel:
      begin
        if Event.MouseWheelVertical then
        begin
          MD.SetFloatShiftY(Window,
            MD.ShiftY - Event.MouseWheelScroll * MD.Font.RowHeight);

          if Assigned(MD.OnUserPress) then
            MD.OnUserPress(Window, Event);
        end;
      end;
  end;
end;

procedure ReleaseMessg(Window: TCastleWindowBase; const Event: TInputPressRelease);
begin
  if Event.IsMouseButton(mbLeft) then
    TMessageData(Window.userdata).ScrollBarDragging := false;
end;

procedure MouseMoveMessg(Window: TCastleWindowBase; newx, newy: integer);
var md: TMessageData;
    moveY: integer;
begin
 md := TMessageData(Window.UserData);
 if not md.ScrollBarDragging then exit;

 { przesuniecie wzdluz y w mouse coords to }
 moveY:=(Window.Height-newY) - (Window.Height-Window.MouseY);

 { ten ruch w strone dodatnia oznacza chec zobaczenia wyzej a wiec zmniejszenia shiftY.
   Wiec sobie go tutaj odwracamy. }
 moveY := -moveY;

 { mamy teraz ruch, ale w skali gdzie caly tekst to RectHeight(md.ScrollBarHeight).
   A nas interesuje skala gdzie caly tekst to minShiftY .. notprettyMaxShiftY.
   A wiec skalujemy. }
 moveY := Round(moveY / RectHeight(md.ScrollBarRect)
                    * (md.notprettyMaxShiftY - md.minShiftY));

 { i gotowe, przesuwamy shiftY o moveY }
 md.setFloatShiftY(Window, md.shiftY + moveY);
end;

procedure IdleMessg(Window: TCastleWindowBase);

  function Faktor: Single;
  begin
   result := 200.0 * Window.Fps.IdleSpeed;
   if mkCtrl in Window.Pressed.Modifiers then result *= 6;
  end;

var md: TMessageData;
begin
 md := TMessageData(Window.userdata);
 with Window do begin
  if Pressed[K_up] then md.setFloatShiftY(Window, md.floatShiftY - Faktor);
  if Pressed[K_down] then md.setFloatShiftY(Window, md.floatShiftY + Faktor);
 end;
end;

procedure DrawMessg(Window: TCastleWindowBase);
var md: TMessageData;

  procedure DrawString(const text: string; textalign: TTextAlign);
  var
    X: Integer;
  begin
   case textalign of
    taLeft:   glRasterPos2i(0, 0);
    taMiddle: glRasterPos2f((md.MaxLineWidth - md.font.TextWidth(text))/2, 0);
    taRight:
      begin
        { You can't simply pass this result to glRasterPos2f, this causes
          errors on x86_64. So first calculate to variable X.
          Probably fixed in FPC by fixing
          [http://www.freepascal.org/mantis/view.php?id=9893],
          TODO: check is it fixed in trunk. }
        X := md.MaxLineWidth - md.font.TextWidth(text);
        glRasterPos2f(X, 0);
      end;
   end;
   md.font.PrintAndMove(text);
   glTranslatef(0, md.font.RowHeight, 0);
  end;

  procedure DrawStrings(const s: TStrings; textalign: TTextAlign);
  var i: integer;
  begin
   for i := s.count-1 downto 0 do drawString(s[i], textalign);
  end;

var
  MessageRect: TIntRect;
  { InnerRect to okienko w ktorym mieszcza sie napisy,
    a wiec MD.WholeMessageRect zmniejszony o BoxMargin we wszystkich kierunkach
    i z ew. obcieta prawa czescia przeznaczona na ScrollBarRect. }
  InnerRect: TIntRect;
  ScrollBarLength: integer;
  ScrollBarVisibleBegin: TGLfloat;
  { if md.ScrollBarVisible, ScrollBarWholeWidth. Else 0. }
  RealScrollBarWholeWidth: Integer;
const
  { a shorter name; box margin - margines
    pomiedzy napisami a obwodka Prostokata z Obwodka. }
  boxmargin = DrawMessg_BoxMargin;
  { a shorter name; windmargin - margines
    pomiedzy obwodka Prostokata Z Obwodka a krawedziami okienka. }
  windmargin = DrawMessg_WindMargin;
  ScrollBarWholeWidth = DrawMessg_ScrollBarWholeWidth;
  { odleglosc paska ScrollBara od krawedzi swojego waskiego recta
    (prawa krawedz jest zarazem krawedzia duzego recta !) }
  ScrollBarMargin = 2;
  { szerokosc paska ScrollBara }
  ScrollBarInternalWidth = ScrollBarWholeWidth - ScrollBarMargin*2;
begin
 md := TMessageData(Window.UserData);

 { Robimy clear bo bgimg moze nie zakryc calego tla jezeli w trakcie MessageXxx
   user resized the window. }
 glClear(GL_COLOR_BUFFER_BIT);
 glLoadIdentity;

 SetWindowPosZero;
 md.DrawBG.Draw;

 RealScrollBarWholeWidth := Iff(md.ScrollBarVisible, ScrollBarWholeWidth, 0);

 { Wspolrzedne InnerRect i MD.WholeMessageRect i MessageRect musza byc
   liczone absolutnie (tzn. nie mozna juz teraz zrobic
   glTranslate costam i wzgledem tego przesuniecia podac InnerRect[...])
   bo te wspolrzedne beda nam potem potrzebne do podania glScissor ktore pracuje
   we wspolrzednych pixeli i w zwiazku z tym byloby nieczule na
   jakies glTranslate.

   Also WholeMessageRect is used by other things not in this procedure. }
 MD.WholeMessageRect := CenteredRect(
   IntRect(0, 0, Window.Width, Window.Height),
   Min(md.MaxLineWidth + BoxMargin*2 + RealScrollBarWholeWidth,
     Window.Width - WindMargin*2),
   Min(MD.AllScrolledLinesCount * MD.Font.RowHeight +
       BoxMargin * 2 +
       MD.ClosingInfoBoxHeight,
     Window.Height - WindMargin*2));
 MessageRect := MD.WholeMessageRect;

 { draw MessageRect (using
   MessagesTheme.RectColor,
   MessagesTheme.RectBorderCol) }
 if MessagesTheme.RectColor[3] <> 1.0 then
 begin
   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
   glEnable(GL_BLEND);
 end;
 GLRectangleWithBorder(MessageRect,
   MessagesTheme.RectColor, Vector4Single(MessagesTheme.RectBorderCol));
 glDisable(GL_BLEND);

 { Now draw MD.Broken_ClosingInfo. After this, make MessageRect
   smaller as appropriate. }
 if MD.ClosingInfo <> '' then
 begin
   glLoadIdentity;
   glTranslatef(MessageRect[0, 0] + BoxMargin, MessageRect[0, 1] + BoxMargin, 0);
   glColorv(MessagesTheme.ClosingInfoCol);
   DrawStrings(MD.Broken_ClosingInfo, taRight);

   MessageRect[0, 1] += BoxMargin +
     MD.Font.RowHeight * MD.Broken_ClosingInfo.Count +
     BoxMargin;

   glLoadIdentity;
   glColorv(MessagesTheme.RectBorderCol);
   GLHorizontalLine(MessageRect[0, 0], MessageRect[1, 0], MessageRect[0, 1]);

   MessageRect[0, 1] +=  1 { width of horizontal line };
 end;

 { Calculate InnerRect now }
 InnerRect := GrowRect(MessageRect, -BoxMargin);
 InnerRect[1, 0] -= RealScrollBarWholeWidth;

 { teraz rysuj ScrollBar. Also calculate MD.ScrollBarRect here. }
 if md.ScrollBarVisible then
 begin
  MD.ScrollBarRect := MessageRect;
  MD.ScrollBarRect[0, 0] := MessageRect[1, 0] - ScrollBarWholeWidth;

  ScrollBarLength := RectHeight(MessageRect) - ScrollBarMargin*2;
  ScrollBarVisibleBegin := MapRange(md.shiftY,
    md.MinShiftY, md.notprettyMaxShiftY,
    ScrollBarLength, 0);
  md.przewVisY1 := MessageRect[0, 1] + ScrollBarMargin +
    max(0, ScrollBarVisibleBegin -
      (md.VisibleScrolledLinesCount / md.AllScrolledLinesCount)*ScrollBarLength);
  md.przewVisY2 := MessageRect[0, 1] + ScrollBarMargin + ScrollBarVisibleBegin;

  glLoadIdentity;
  glColorv(MessagesTheme.RectBorderCol);
  GLVerticalLine(md.ScrollBarRect[0, 0],
    md.ScrollBarRect[0, 1], md.ScrollBarRect[1, 1]);

  glLineWidth(ScrollBarInternalWidth);
  glColorv(MessagesTheme.ScrollBarCol);
  GLVerticalLine((md.ScrollBarRect[0, 0] + md.ScrollBarRect[1, 0]) / 2,
    md.przewVisY1, md.przewVisY2);
  glLineWidth(1);
 end else
   MD.ScrollBarRect := IntRectEmpty;

 { teraz zaaplikuj shiftY. Jednoczesnie zmusza nas to do zrobienia scissora
   aby napisy jakie wyleza za okienko byly odpowiednio sciete.
   Scissor jest obnizony na dole o font.Descend aby descend ostatniego
   wyswietlanego stringa (ktory jest przeciez na shiftY ujemnym !) mogl
   byc kiedykolwiek widoczny. }
 glScissor(InnerRect[0, 0], InnerRect[0, 1]-md.font.Descend,
           RectWidth(InnerRect),
           RectHeight(InnerRect));
 glEnable(GL_SCISSOR_TEST);
 glLoadIdentity;
 glTranslatef(0, md.shiftY, 0);

 { rysuj md.broken_SAdditional i Broken_MessgText.
   Kolejnosc ma znaczenie, kolejne linie sa rysowane od dolu w gore. }
 glTranslatef(InnerRect[0, 0], InnerRect[0, 1], 0);

 if md.drawAdditional then
 begin
  glColorv(MessagesTheme.AdditionalStrCol);
  DrawStrings(md.Broken_SAdditional, md.align);
 end;

 glColorv(MessagesTheme.TextCol);
 DrawStrings(md.Broken_MessgText, md.align);

 glDisable(GL_SCISSOR_TEST);
end;

{ main utility MessageCore ------------------------------------- }

{ Notes:
  - MessageOnUserMouseDown will be called only if the mouse position
    will be within WholeMessageRect. You should not react to mouse
    click on other places.
}
procedure MessageCore(Window: TCastleWindowBase; textlist: TStringList;
  textalign: TTextAlign; MessageOnUserPress: TInputPressReleaseFunc;
  AUserMouseDownOnlyWithinRect: boolean;
  messageUserdata: pointer;
  const AClosingInfo: string; { = '' znaczy "nie rysuj ClosingInfo" }
  AdrawAdditional: boolean; var ASAdditional: string);
{ Robi cos co wyglada jak dialog window w srodku podanego Window.
  Na pewien czas podmienia callbacki Window; mozna podac jaki
  bedzie callback na OnPress i OnDraw; W callbackach uzywamy
  ustawianej w tej procedurze strukturze TMessageData (bierzemy ja z
  PMessageData(Window.UserData) ). W ten sposob mozna wywolac na wielu
  roznch okienkach MessageCore i wszystko bedzie dzialalo ok.

  OnDraw moze zasadniczo wygladac dowolnie, ale powinno uzywac zmiennych
  ustawionych dla niego w TMessageData mowiacych mu co i jak wypisac.

  Perspektywa w ktorej dziala OnDraw jest zawsze
  Ortho2D(0, Window.width, 0, Window.height).

  Zawartosc obiektu textlist nie bedzie modyfikowana - jest to gwarantowane.

  Works until ktorys callback ustawi answered na true.
}
var messageData: TMessageData;
    SavedMode: TGLMode;
begin
  if Log then
    WritelnLogMultiline('Message', TextList.Text);

 {1 faza :
   FlushRedisplay; W ten sposob po zainstalowaniu naszych callbackow
   i ustawieniu wlasciwosci okienka robimy normalne SaveScreen_noflush
   (a nie chcielibysmy robic wtedy z flushem bo zainstalowalismy juz
   wlasne callbacki)

   If we have DoubleBuffer then we simply call Window.EventDraw,
   see comments at GLImage.SaveScreen_noflush to know why
   (in short: we DON'T want to use front buffer to save screen). }
 if Window.DoubleBuffer then
 begin
  Window.EventBeforeDraw;
  Window.EventDraw;
 end else
  Window.FlushRedisplay;

 {2 faza :
   Sejwujemy sobie wszystkie wlasciwosci okienka glwin ktore chcemy zmienic.
   Kiedy juz skonczymy bedziemy chcieli je odtworzyc. }
 SavedMode := TGLMode.CreateReset(Window,
   GL_PIXEL_MODE_BIT or GL_SCISSOR_BIT or GL_ENABLE_BIT or
   GL_LINE_BIT or GL_TRANSFORM_BIT or GL_COLOR_BUFFER_BIT, false,

 {3 faza zarazem:
   Ustawiamy wlasne wlasciwosci okienka, w szczegolnosci - wlasne callbacki. }
   @drawMessg, @resizeMessg, @NoClose);

 { FakeMouseDown must be @false.
   Otherwise closing dialog box with MouseDown will then cause MouseDown
   when SavedMode is restored. This is bad, because then the mouse click
   that closes dialog box could also do something else.
   Actually, FakeMouseDown is @false by default, so this call is not needed. }
 SavedMode.FakeMouseDown := false;

 with Window do begin
  OnMouseMove := @mouseMoveMessg;
  OnPress := @PressMessg;
  OnRelease := @ReleaseMessg;
  OnIdle := @idleMessg;
  PostRedisplay;
 end;
 glDisable(GL_FOG);
 glDisable(GL_LIGHTING);
 glDisable(GL_TEXTURE_1D);
 glDisable(GL_TEXTURE_2D);
 glDisable(GL_SCISSOR_TEST);
 glDisable(GL_DEPTH_TEST);
 glLineWidth(1);
 glPixelStorei(GL_UNPACK_SWAP_BYTES, GL_FALSE);
 glPixelStorei(GL_UNPACK_LSB_FIRST, GL_FALSE);
 glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);
 glPixelStorei(GL_UNPACK_SKIP_ROWS,  0);
 glPixelStorei(GL_UNPACK_SKIP_PIXELS, 0);
 glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
 glPixelZoom(1, 1);

 {4 faza :
   Inicjuj TMessageData, ktora moze wymagac poprawnego ustawienia
   naszych rzeczy w OpenGLu (np. utworzenie fontu wymaga poprawnego ustawienia
   GL_UNPACK_xxx bo tworzenie fontu to generowanie jego display list a te
   zostaja utworzone z zapisanym na stale odpowiednim pixel-mode). }
 messageData := TMessageData.Create;
 try
  { TODO: problem : chcielibysmy tutaj zeby SaveScreen zawsze dzialalo, bez
    wzgledu na ustawienia GL_PACK_xxx. Moglibysmy ustawiac wlasne GL_PACK
    ale lepszym wyjsciem byloby gdyby
    SaveScreen_noflush dzialalo zawsze tak samo, bez wzgledu na ustawienia
    GL_PACK_xxx. }
  with messageData do
  begin
   OnUserPress := messageOnUserPress;
   UserMouseDownOnlyWithinRect := AUserMouseDownOnlyWithinRect;
   if Window.DoubleBuffer then
    DrawBG := SaveScreenToGL_noflush(0, 0, Window.Width, Window.Height, GL_BACK) else
    DrawBG := SaveScreenToGL_noflush(0, 0, Window.Width, Window.Height, GL_FRONT);
   answered := false;
   if MessagesTheme.Font = nil then
    font := TGLBitmapFont.Create(BitmapFont_BVSansMono_m18) else
    font := MessagesTheme.Font;
   MessgText := textlist;
   { contents of Broken_xxx will be initialized in resizeMessg(),
     as well as few other MessageData fields like MaxLineWidth or
     AllScrolledLinesCount }
   Broken_MessgText := TStringList.Create;
   Broken_ClosingInfo := TStringList.Create;
   Broken_SAdditional := TStringList.Create;
   align := textalign;
   Userdata := messageUserdata;
   ClosingInfo := AClosingInfo;
   ScrollBarVisible := false;
   ScrollBarDragging := false;
   drawAdditional := AdrawAdditional;
   FSAdditional := ASAdditional;
   { These will be set in nearest DrawMessg }
   ScrollBarRect := IntRectEmpty;
   WholeMessageRect := IntRectEmpty;
  end;

  {5 faza : konczymy ustawiac wlasne wlasciwosci okienka.
    Jak widac, chodzi tu o reczne wywolanie odpowiednich callbackow aby
    zainicjowaly swoj stan. To wymaga aby TMessageData bylo juz calkowicie
    zainicjowane.
  Podobnie, zainicjowanie Userdata := messageData oczywiscie
    wymaga aby messageData bylo juz ustalone. }
  Window.Userdata := messageData;
  { ustaw nasze projection matrix }
  resizeMessg(Window);
  { ustaw nas na poczatku tekstu. }
  MessageData.SetFloatShiftY(Window, MessageData.MinShiftY);

  {6 faza :
    Robimy wlasna petle, az do messageData.answered. }
  repeat Application.ProcessMessage(true, true) until messageData.answered;

 { zwolnij zainicjowane messageData }
 finally
  FreeAndNil(messageData.DrawBG);
  messageData.Broken_MessgText.Free;
  messageData.Broken_ClosingInfo.Free;
  messageData.Broken_SAdditional.Free;
  if MessagesTheme.Font = nil then messageData.font.Free;
  ASAdditional := messageData.SAdditional;
  FreeAndNil(messageData);

  {7 faza :
    Odtwarzamy zasejwowane wlasciwosci okienka. }
  FreeAndNil(SavedMode);
 end;
end;

procedure MessageCore_NoAdditional(Window: TCastleWindowBase; textlist: TStringList;
  textalign: TTextAlign;
  MessageOnUserPress: TInputPressReleaseFunc;
  AUserMouseDownOnlyWithinRect: boolean;
  messageUserdata: pointer;
  const AClosingInfo: string);
var dummy: string;
begin
 dummy := '';
 MessageCore(Window, textlist, textalign, MessageOnUserPress,
   AUserMouseDownOnlyWithinRect,
   messageUserdata, AClosingInfo, false, dummy);
end;

{ MessageOK function with callbacks ------------------------------------------ }

procedure PressMessgOK(Window: TCastleWindowBase; const Event: TInputPressRelease);
begin
  if Event.IsMouseButton(mbLeft) or Event.IsKey(CharEnter) then
    TMessageData(Window.UserData).answered := true;
end;

procedure MessageOK(Window: TCastleWindowBase;  const SArray: array of string;
  textalign: TTextAlign = taMiddle);
var textlist: TStringList;
begin
 textlist := TStringList.Create;
 try
  AddStrArrayToStrings(SArray, textlist);
  MessageOK(Window, textlist, textalign);
 finally textlist.Free end;
end;

procedure MessageOK(Window: TCastleWindowBase; const s: string; textalign: TTextAlign);
var textlist: TStringList;
begin
 textlist := TStringList.Create;
 try
  Strings_SetText(textlist, s);
  MessageOK(Window, textlist, textalign);
 finally textlist.free end;
end;

procedure MessageOK(Window: TCastleWindowBase;  textlist: TStringList;
  textalign: TTextAlign);
begin
 MessageCore_NoAdditional(Window, textlist, textalign,
   @PressMessgOK, true, nil, '[Enter]');
end;

{ MessageInput function with callbacks --------------------------------------- }

type
  TInputData = record
    answerCancelled: boolean; { wazne tylko jesli userCanCancel }

    {pola ustawiane w wywolaniu MessageInput, read only z callbackow xxxMessgInput }
    answerMinLen, answerMaxLen: integer;
    answerAllowedChars: TSetOfChars;
    userCanCancel: boolean; { czy user moze wyjsc przez "Cancel" }
  end;
  PInputData = ^TInputData;

procedure PressMessgInput(Window: TCastleWindowBase; const Event: TInputPressRelease);
var md: TMessageData;
    id: PInputData;
begin
  md := TMessageData(Window.UserData);
  id := PInputData(md.userdata);

  { Under Windows, pressing ctrl+backspace causes key = K_BackSpace with
    character = CharDelete. That is, Windows automatically replaces ctrl+backspace
    with delete (leaving it ambigous what should I do --- look at key code or
    character?). Here, I want to detect ctrl+backspace, and not detect "real" delete
    key presses (that may be handled in the future, right now there's no cursor
    so delete doesn't work, only backspace). So I just look at both Key and C
    to detect backspace. }
  if Event.IsKey(CharBackSpace) or Event.IsKey(K_BackSpace) then
  begin
    if md.SAdditional <> '' then
      if mkCtrl in Window.Pressed.Modifiers then
        md.SetSAdditional(Window, '') else
        md.SetSAdditional(Window, Copy(md.SAdditional, 1, Length(md.SAdditional)-1));
  end else
  if Event.IsKey(CharEnter) then
  begin
    if Length(md.SAdditional) >= id^.answerMinLen then
      md.answered := true else
      MessageOk(Window, Format('You must enter at least %d characters.',
        [id^.answerMinLen]), taMiddle);
  end else
  if Event.IsKey(CharEscape) then
  begin
    if id^.userCanCancel then
    begin
      id^.answerCancelled := true;
      md.answered := true;
    end;
  end else
  if (Event.EventType = itKey) and
     (Event.KeyCharacter <> #0) and
     (Event.KeyCharacter in id^.answerAllowedChars) and
     ((id^.answerMaxLen = 0) or (length(md.SAdditional) < id^.answerMaxLen)) then
    md.SetSAdditional(Window, md.SAdditional + Event.KeyCharacter);
end;

function MessageInput(Window: TCastleWindowBase; const s: string;
  textalign: TTextAlign; const answerDefault: string;
  answerMinLen: integer; answerMaxLen: integer; const answerAllowedChars: TSetOfChars): string;
var textlist: TStringList;
begin
 textlist := TStringList.Create;
 try
  Strings_SetText(textlist, s);
  result := MessageInput(Window, textlist, textalign, answerDefault, answerMinLen,
     answerMaxLen, answerAllowedChars);
 finally textlist.free end;
end;

function MessageInput(Window: TCastleWindowBase; textlist: TStringList;
  textalign: TTextAlign; const answerDefault: string;
  answerMinLen: integer; answerMaxLen: integer; const answerAllowedChars: TSetOfChars): string;
var inputData: TInputData;
begin
 inputdata.answerMinLen := answerMinLen;
 inputdata.answerMaxLen := answerMaxLen;
 inputdata.answerAllowedChars := answerAllowedChars;
 inputdata.userCanCancel := false;
 inputdata.answerCancelled := false;
 result := answerDefault;
 MessageCore(Window, textlist, textalign,
   @PressMessgInput, false, @inputdata, '', true, result);
end;

function MessageInputQuery(Window: TCastleWindowBase; const s: string;
  var answer: string; textalign: TTextAlign;
  answerMinLen: integer; answerMaxLen: integer; const answerAllowedChars: TSetOfChars): boolean;
var textlist: TStringList;
begin
 textlist := TStringList.Create;
 try
  Strings_SetText(textlist, s);
  result := MessageInputQuery(Window, textlist, answer, textalign, answerMinLen,
     answerMaxLen, answerAllowedChars);
 finally textlist.free end;
end;

function MessageInputQuery(Window: TCastleWindowBase; textlist: TStringList;
  var answer: string; textalign: TTextAlign;
  answerMinLen: integer; answerMaxLen: integer; const answerAllowedChars: TSetOfChars): boolean;
var inputData: TInputData;
    SAdditional: string;
begin
 inputdata.answerMinLen := answerMinLen;
 inputdata.answerMaxLen := answerMaxLen;
 inputdata.answerAllowedChars := answerAllowedChars;
 inputdata.userCanCancel := true;
 inputdata.answerCancelled := false;

 {uzywamy dodatkowej zmiennej SAdditional zamiast bezposrednio przekazywac
  GLWinMessage zmienna answer bo jezeli not result to nie chcemy zmieniac
  answer. }
 SAdditional := answer;
 MessageCore(Window, textlist, textalign,
   @PressMessgInput, false, @inputdata, 'OK[Enter] / Cancel[Escape]', true, SAdditional);
 result := not inputdata.answerCancelled;
 if result then answer := SAdditional;
end;

{ MessageChar functions with callbacks --------------------------------------- }

type
  TCharData = record
    AllowedChars: TSetOfChars;
    answer: char;
    IgnoreCase: boolean;
  end;
  PCharData = ^TCharData;

procedure PressMessgChar(Window: TCastleWindowBase; const Event: TInputPressRelease);
var
  md: TMessageData;
  cd: PCharData;
  C: char;
begin
  md := TMessageData(Window.UserData);
  cd := PCharData(md.userdata);

  if Event.EventType <> itKey then Exit;
  C := Event.KeyCharacter;

  if cd^.IgnoreCase then
  begin
    if (UpCase(C) in cd^.AllowedChars) or
       (LoCase(C) in cd^.AllowedChars) then
    begin
      md.answered := true;
      cd^.answer := LoCase(c);
    end;
  end else
  begin
    if c in cd^.AllowedChars then
    begin
      md.answered := true;
      cd^.answer := c;
    end;
  end;
end;

function MessageChar(Window: TCastleWindowBase; const s: string; const AllowedChars: TSetOfChars;
  const ClosingInfo: string; textalign: TTextAlign;
  IgnoreCase: boolean): char;
var textlist: TStringList;
begin
 textlist := TStringList.Create;
 try
  Strings_SetText(textlist, s);
  result := MessageChar(Window, textlist, AllowedChars, ClosingInfo, textalign, IgnoreCase);
 finally textlist.free end;
end;

function MessageChar(Window: TCastleWindowBase;  const SArray: array of string; const AllowedChars: TSetOfChars;
  const ClosingInfo: string; textalign: TTextAlign;
  IgnoreCase: boolean): char; overload;
var textlist: TStringList;
begin
 textlist := TStringList.Create;
 try
  AddStrArrayToStrings(SArray, textlist);
  result := MessageChar(Window, textlist, AllowedChars, ClosingInfo, textalign, IgnoreCase);
 finally textlist.Free end;
end;

function MessageChar(Window: TCastleWindowBase; textlist: TStringList;
  const AllowedChars: TSetOfChars; const ClosingInfo: string;
  textalign: TTextAlign;
  IgnoreCase: boolean): char; overload;
var charData: TCharData;
begin
 chardata.allowedChars := AllowedChars;
 chardata.IgnoreCase := IgnoreCase;
 MessageCore_NoAdditional(Window, textlist, textalign,
   @PressMessgChar, false,
   @chardata, ClosingInfo);
 result := chardata.answer;
end;

{ MessageKey functions with callbacks --------------------------------------- }

type
  TMessageKeyData = record
    Answer: TKey;
  end;
  PMessageKeyData = ^TMessageKeyData;

procedure MessageKey_Press(Window: TCastleWindowBase; const Event: TInputPressRelease);
var
  MD: TMessageData;
  KD: PMessageKeyData;
begin
  MD := TMessageData(Window.UserData);
  KD := PMessageKeyData(MD.UserData);

  if (Event.EventType = itKey) and (Event.Key <> K_None) then
  begin
    MD.Answered := true;
    KD^.Answer := Event.Key;
  end;
end;

function MessageKey(Window: TCastleWindowBase; const S: string;
  const ClosingInfo: string; TextAlign: TTextAlign): TKey;
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    Strings_SetText(TextList, S);
    Result := MessageKey(Window, TextList, ClosingInfo, TextAlign);
  finally TextList.free end;
end;

function MessageKey(Window: TCastleWindowBase; const SArray: array of string;
  const ClosingInfo: string; TextAlign: TTextAlign): TKey;
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    AddStrArrayToStrings(SArray, TextList);
    Result := MessageKey(Window, TextList, ClosingInfo, TextAlign);
  finally TextList.Free end;
end;

function MessageKey(Window: TCastleWindowBase; TextList: TStringList;
  const ClosingInfo: string; TextAlign: TTextAlign): TKey;
var
  MessageKeyData: TMessageKeyData;
begin
  MessageCore_NoAdditional(Window, TextList, TextAlign,
    @MessageKey_Press, false, @MessageKeyData, ClosingInfo);
  Result := MessageKeyData.Answer;
end;

{ MessageKeyMouse functions with callbacks ----------------------------------- }

type
  TMessageKeyMouseData = record
    Answer: TInputPressRelease;
  end;
  PMessageKeyMouseData = ^TMessageKeyMouseData;

procedure MessageKeyMouse_Press(Window: TCastleWindowBase; const Event: TInputPressRelease);
var
  MD: TMessageData;
  KD: PMessageKeyMouseData;
begin
  MD := TMessageData(Window.UserData);
  KD := PMessageKeyMouseData(MD.UserData);

  if (Event.EventType <> itKey) or (Event.Key <> K_None) then
  begin
    MD.Answered := true;
    KD^.Answer := Event;
  end;
end;

procedure MessageKeyMouse(Window: TCastleWindowBase; const S: string;
  const ClosingInfo: string; TextAlign: TTextAlign;
  out Event: TInputPressRelease);
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    Strings_SetText(TextList, S);
    MessageKeyMouse(Window, TextList, ClosingInfo, TextAlign, Event);
  finally TextList.Free end;
end;

procedure MessageKeyMouse(Window: TCastleWindowBase; TextList: TStringList;
  const ClosingInfo: string; TextAlign: TTextAlign;
  out Event: TInputPressRelease);
var
  Data: TMessageKeyMouseData;
begin
  MessageCore_NoAdditional(Window, TextList, TextAlign,
    @MessageKeyMouse_Press, false, @Data, ClosingInfo);
  Event := Data.Answer;
end;

{ MessageYesNo ktore jest po prostu realizowane przez MessageChar ------------ }

const
  { TODO: to be localized ? }
  MessageYesNo_YesLetter = 'y';
  MessageYesNo_NoLetter = 'n';
  MessageYesNo_ClosingInfo = '[Y]es/[N]o';
  MessageYesNo_AllowedChars: TSetOfChars = ['n','N','y','Y'];

function MessageYesNo(Window: TCastleWindowBase; const s: string;
  textalign: TTextAlign): boolean; overload;
begin
 result := LoCase(MessageChar(Window, s, MessageYesNo_AllowedChars,
   MessageYesNo_ClosingInfo, textalign)) = MessageYesNo_YesLetter;
end;

function MessageYesNo(Window: TCastleWindowBase;  const SArray: array of string;
  textalign: TTextAlign): boolean; overload;
begin
 result := LoCase(MessageChar(Window, SArray, MessageYesNo_AllowedChars,
   MessageYesNo_ClosingInfo, textalign)) = MessageYesNo_YesLetter;
end;

function MessageYesNo(Window: TCastleWindowBase;  textlist: TStringList;
  textalign: TTextAlign): boolean; overload;
begin
 result := LoCase(MessageChar(Window, textlist, MessageYesNo_AllowedChars,
   MessageYesNo_ClosingInfo, textalign)) = MessageYesNo_YesLetter;
end;

{ MessageInputCardinal ------------------------------------------------------- }

function MessageInputCardinal(Window: TCastleWindowBase; const s: string;
  TextAlign: TTextAlign; const AnswerDefault: string): Cardinal;
begin
 result := StrToInt( MessageInput(Window, s, TextAlign, AnswerDefault,
   1, 0, ['0'..'9']) );
end;

function MessageInputCardinal(Window: TCastleWindowBase; const s: string;
  TextAlign: TTextAlign; AnswerDefault: Cardinal): Cardinal;
begin
 result := MessageInputCardinal(Window, s, TextAlign, IntToStr(AnswerDefault));
end;

function MessageInputQueryCardinal(Window: TCastleWindowBase; const Title: string;
  var Value: Cardinal; TextAlign: TTextAlign): boolean;
var ValueStr: string;
begin
 ValueStr := IntToStr(Value);
 Result := MessageInputQuery(Window, Title, ValueStr, TextAlign, 1, 0, ['0'..'9']);
 if Result then
  Value := StrToInt(ValueStr);
end;

function MessageInputQueryCardinalHex(Window: TCastleWindowBase; const Title: string;
  var Value: Cardinal; TextAlign: TTextAlign; MaxWidth: Cardinal): boolean;
var ValueStr: string;
begin
 ValueStr := IntToHex(Value, 4);
 Result := MessageInputQuery(Window, Title, ValueStr, TextAlign, 1, MaxWidth,
   ['0'..'9', 'a'..'f', 'A'..'F']);
 if Result then
  Value := StrHexToInt(ValueStr);
end;

{ MessageInputQuery on floats ------------------------------------------------ }

function MessageInputQuery(Window: TCastleWindowBase; const Title: string;
  var Value: Extended; TextAlign: TTextAlign; const ValueAsString: string): boolean;
var s: string;
begin
 Result := false;
 if ValueAsString <> '' then
   S := ValueAsString else
   S := FloatToStr(Value);
 if MessageInputQuery(Window, Title, s, TextAlign) then
 begin
  try
   Value := StrToFloat(s);
   Result := true;
  except
   on E: EConvertError do
   begin
    MessageOK(Window, 'Invalid floating point value : ' +E.Message, taLeft);
   end;
  end;
 end;
end;

function MessageInputQuery(Window: TCastleWindowBase; const Title: string;
  var Value: Single; TextAlign: TTextAlign; const ValueAsString: string): boolean;
var
  ValueExtended: Extended;
begin
  ValueExtended := Value;
  Result := MessageInputQuery(Window, Title, ValueExtended, TextAlign, ValueAsString);
  if Result then
    Value := ValueExtended;
end;

{$ifndef EXTENDED_EQUALS_DOUBLE}
function MessageInputQuery(Window: TCastleWindowBase; const Title: string;
  var Value: Double; TextAlign: TTextAlign; const ValueAsString: string): boolean;
var
  ValueExtended: Extended;
begin
  ValueExtended := Value;
  Result := MessageInputQuery(Window, Title, ValueExtended, TextAlign, ValueAsString);
  if Result then
    Value := ValueExtended;
end;
{$endif not EXTENDED_EQUALS_DOUBLE}

{ MessageInputQueryVector3Single --------------------------------------------- }

function MessageInputQueryVector3Single(
  Window: TCastleWindowBase; const Title: string;
  var Value: TVector3Single; TextAlign: TTextAlign): boolean;
var s: string;
begin
 Result := false;
 s := Format('%g %g %g', [Value[0], Value[1], Value[2]]);
 if MessageInputQuery(Window, Title, s, TextAlign) then
 begin
  try
   Value := Vector3SingleFromStr(s);
   Result := true;
  except
   on E: EConvertError do
   begin
    MessageOK(Window, 'Invalid vector 3 value : ' + E.Message, taLeft);
   end;
  end;
 end;
end;

{ MessageInputQueryVector4Single --------------------------------------------- }

function MessageInputQueryVector4Single(
  Window: TCastleWindowBase; const Title: string;
  var Value: TVector4Single; TextAlign: TTextAlign): boolean;
var s: string;
begin
 Result := false;
 s := Format('%g %g %g %g', [Value[0], Value[1], Value[2], Value[3]]);
 if MessageInputQuery(Window, Title, s, TextAlign) then
 begin
  try
   Value := Vector4SingleFromStr(s);
   Result := true;
  except
   on E: EConvertError do
   begin
    MessageOK(Window, 'Invalid vector 4 value : ' + E.Message, taLeft);
   end;
  end;
 end;
end;

{ init / fini ---------------------------------------------------------------- }

initialization
  MessagesTheme := MessagesTheme_Default;
end.
