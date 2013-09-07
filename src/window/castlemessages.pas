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

(* Dialog windows (asking user for confirmation, question,
  simple text input and such) displayed within an OpenGL context
  (TCastleWindow or TCastleControl).

  Features:

  @unorderedList(
    @item(All the MessageXxx routines display a modal dialog.
      They return only when the user accepted / answered the dialog box.
      This way they are comfortable to use anywhere in your program.)

    @item(MessageInputXxx family of functions ask user to enter some text.)

    @item(All the dialog boxes have vertical scroll bar, displayed when needed.
      So it's OK to use really long text. Scroll bar can be operated
      with keys (up/down, ctrl+up/down, page up/down, home/end) and mouse
      (drag the scroll bar, or click below/above it).)

    @item(Long text lines are automatically broken. So it's OK to use
      text with long lines.
      We will try to break text only at whitespace.

      If you pass a text as a single string parameter, then our "line breaking"
      works  correctly even for text that already contains newline characters
      (they are correctly recognized as forcing line break).

      If you pass a text as an "array of string" or TStringList, it's expected
      that strings inside don't contain newline characters anymore. It's undefined
      what will happen (i.e. whether they will be correctly broken) otherwise.
      Of course, TStringList contents used to pass text to MessageXxx will never
      be modified in any way.)

    @item(User is allowed to resize the window while MessageXxx works.
      (As long as TCastleWindowCustom.ResizeAllowed = raAllowed, of course.)
      Long lines are automatically broken taking into account current window
      width.)

    @item(You can configure dialog boxes look using TCastleTheme.
      Various parts of the dialog use scaled images.
      This way you can change the border, background of the dialog,
      you can also make the dialog box partially transparent.)
  )

  Call MessageXxx functions only when Window.Closed = false.
  Note that MessageXxx will do Window.MakeCurrent (probably more than once).
  Calling MessageXxx requires one free place on OpenGL attrib stack.

  Notes about implementation:

  @unorderedList(
    @item(We temporary replace normal window callbacks and controls
      using TGLMode. This allows you to call MessageXxx procedures
      in any place of your program, and things will just work,
      the MessageXxx will return only once user answers the dialog box.)

    @item(Be careful if you use TCastleApplication.OnUpdate or
      TCastleApplication.OnTimer. As these events are not tied
      to a particular window, they continue to work even
      while we're inside MessageXxx procedure.
      Be sure to implement them such that they make sense also when
      we're inside a dialog box.

      In particular, remember that you cannot close the Window
      when the message box in running. So do not blindly call
      TCastleWindowBase.Close from TCastleApplication callbacks. )

    @item(Since your normal callbacks
      and controls are not run when message box is running, you usually
      don't need to do anything special about it, unless you use
      TCastleApplication callbacks mentioned above.)
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

uses Classes, CastleWindow, CastleGLUtils, GL, GLU, GLExt, CastleUtils,
  CastleStringUtils, CastleVectors, CastleKeysMouse;

type
  { Text alignment for MessageXxx functions in @link(CastleMessages) unit. }
  TTextAlign = (taLeft, taMiddle, taRight);

{ Ask user for simple confirmation. This is the simplest "OK" dialog box.

  @groupBegin }
procedure MessageOK(Window: TCastleWindowCustom; const s: string;
  TextAlign: TTextAlign = taMiddle); overload;
procedure MessageOK(Window: TCastleWindowCustom;  const SArray: array of string;
  TextAlign: TTextAlign = taMiddle); overload;
procedure MessageOK(Window: TCastleWindowCustom;  TextList: TStringList;
  TextAlign: TTextAlign = taMiddle); overload;
{ @groupEnd }

{ Ask user to input a string.
  User must give an answer (there is no "Cancel" button),
  use MessageInputQuery if you want a version with "Cancel" button.
  @param AnswerMaxLen 0 (zero) means that there's no maximum answer length.

  @groupBegin }
function MessageInput(Window: TCastleWindowCustom; const s: string;
  TextAlign: TTextAlign = taMiddle;
  const answerDefault: string = '';
  answerMinLen: integer = 0;
  answerMaxLen: integer = 0;
  const answerAllowedChars: TSetOfChars = AllChars): string; overload;
function MessageInput(Window: TCastleWindowCustom; TextList: TStringList;
  TextAlign: TTextAlign = taMiddle;
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
function MessageInputQuery(Window: TCastleWindowCustom; const s: string;
  var answer: string; TextAlign: TTextAlign;
  answerMinLen: integer = 0;
  answerMaxLen: integer = 0;
  const answerAllowedChars: TSetOfChars = AllChars): boolean; overload;
function MessageInputQuery(Window: TCastleWindowCustom; TextList: TStringList;
  var answer: string; TextAlign: TTextAlign;
  answerMinLen: integer = 0;
  answerMaxLen: integer = 0;
  const answerAllowedChars: TSetOfChars = AllChars): boolean; overload;
{ @groupEnd }

{ Ask user to press a single character from a given set, or to press one
  of the buttons. This is good to ask user a question with a small number
  of answers.

  AllowedChars specifies the characters that can be used to answer.

  Buttons specify the buttons available to press,
  and ButtonsChars specify what character is returned when given button is pressed
  (always the length of Buttons and ButtonsChars must be equal).

  @param(IgnoreCase This means that case of letters in AllowedChars
    and ButtonsChars will be ignored.

    For example, you can include only uppercase 'A' in
    AllowedChars. Or you can include lowercase 'a'. Or you can include
    both. It doesn't matter --- we will always accept both lower and upper case.

    And it doesn't matter what case of letters you use for ButtonsChars.

    Also, when character is returned, it's always lowercased.
    So you donn't even know if user pressed lower of upper case letter.)

  @groupBegin }
function MessageChar(Window: TCastleWindowCustom; const s: string;
  const AllowedChars: TSetOfChars;
  const Buttons: array of string; const ButtonsChars: array of char;
  TextAlign: TTextAlign = taMiddle;
  IgnoreCase: boolean = false): char; overload;
function MessageChar(Window: TCastleWindowCustom; const SArray: array of string;
  const AllowedChars: TSetOfChars;
  const Buttons: array of string; const ButtonsChars: array of char;
  TextAlign: TTextAlign = taMiddle;
  IgnoreCase: boolean = false): char; overload;
function MessageChar(Window: TCastleWindowCustom; TextList: TStringList;
  const AllowedChars: TSetOfChars;
  const Buttons: array of string; const ButtonsChars: array of char;
  TextAlign: TTextAlign = taMiddle;
  IgnoreCase: boolean = false): char; overload;
{ @groupEnd }

{ Ask user to press any key, return this key as Keys.TKey.

  Never returns K_None (which means that keys that cannot be interpreted
  as Keys.TKey will be ignored, and will not close the dialog box).

  @groupBegin }
function MessageKey(Window: TCastleWindowCustom; const S: string;
  TextAlign: TTextAlign): TKey; overload;
function MessageKey(Window: TCastleWindowCustom; const SArray: array of string;
  TextAlign: TTextAlign): TKey; overload;
function MessageKey(Window: TCastleWindowCustom; TextList: TStringList;
  TextAlign: TTextAlign): TKey; overload;
{ @groupEnd }

{ Ask user to press any key or mouse button or mouse wheel, and return it.
  The natural use for this is to allow user to configure
  keybindings of your program, like for TInputShortcut.

  TODO: for historical reasons, in case of key events,
  it for now returns only events that can be represented as TKey
  (so Event.Key <> K_None, as long as Event.EventType = itKey).

  @groupBegin }
procedure MessageKeyMouse(Window: TCastleWindowCustom; const S: string;
  TextAlign: TTextAlign; out Event: TInputPressRelease); overload;

procedure MessageKeyMouse(Window: TCastleWindowCustom; TextList: TStringList;
  TextAlign: TTextAlign; out Event: TInputPressRelease); overload;
{ @groupEnd }

function MessageYesNo(Window: TCastleWindowCustom; const s: string;
  TextAlign: TTextAlign = taMiddle): boolean; overload;
function MessageYesNo(Window: TCastleWindowCustom;  const SArray: array of string;
  TextAlign: TTextAlign = taMiddle): boolean; overload;
function MessageYesNo(Window: TCastleWindowCustom;  TextList: TStringList;
  TextAlign: TTextAlign = taMiddle): boolean; overload;

{ Ask user to input an unsigned integer.

  Note that AnswerDefault below may be given as Cardinal or as a string.
  The latter is useful if you want the default answer to be '', i.e. empty string
  --- no default answer.

  @groupBegin }
function MessageInputCardinal(Window: TCastleWindowCustom; const s: string;
  TextAlign: TTextAlign; const AnswerDefault: string): Cardinal; overload;
function MessageInputCardinal(Window: TCastleWindowCustom; const s: string;
  TextAlign: TTextAlign; AnswerDefault: Cardinal): Cardinal; overload;
function MessageInputQueryCardinal(Window: TCastleWindowCustom; const Title: string;
  var Value: Cardinal; TextAlign: TTextAlign): boolean;
{ @groupEnd }

{ Ask user to input a value in hexadecimal.
  Give MaxWidth = 0 to say that there is no maximum width. }
function MessageInputQueryCardinalHex(Window: TCastleWindowCustom; const Title: string;
  var Value: Cardinal; TextAlign: TTextAlign; MaxWidth: Cardinal): boolean;

{ Ask user to input a floating-point number.

  If you give non-empty ValueAsString, it will be used to show
  the initial value for the user. Otherwise, we will just show
  FloatToStr(Value), which sometimes may be too ugly.
  For example Value = 0.01 cannot be precisely represented as a floating point
  number, and FloatToStr shows that this is really something like 0.0099xxxxx.

  @groupBegin }
function MessageInputQuery(Window: TCastleWindowCustom; const Title: string;
  var Value: Extended; TextAlign: TTextAlign; const ValueAsString: string = ''): boolean;
function MessageInputQuery(Window: TCastleWindowCustom; const Title: string;
  var Value: Single; TextAlign: TTextAlign; const ValueAsString: string = ''): boolean;
{$ifndef EXTENDED_EQUALS_DOUBLE}
function MessageInputQuery(Window: TCastleWindowCustom; const Title: string;
  var Value: Double; TextAlign: TTextAlign; const ValueAsString: string = ''): boolean;
{$endif not EXTENDED_EQUALS_DOUBLE}
{ @groupEnd }

function MessageInputQueryVector3Single(
  Window: TCastleWindowCustom; const Title: string;
  var Value: TVector3Single; TextAlign: TTextAlign): boolean;

function MessageInputQueryVector4Single(
  Window: TCastleWindowCustom; const Title: string;
  var Value: TVector4Single; TextAlign: TTextAlign): boolean;

implementation

uses CastleImages, CastleControls, CastleGLBitmapFonts,
  CastleClassUtils, SysUtils, CastleWindowModes, CastleLog, CastleGLImages,
  CastleUIControls, CastleRectangles;

{ TCastleDialog -------------------------------------------------------------- }

type
  TCastleDialog = class abstract(TUIControl)
  private
    const
      BoxMargin = 10;
      WindowMargin = 10;
      ScrollBarWholeWidth = 20;
      ButtonHorizontalMargin = 10;
      MinButtonWidth = 100; //< OK button looks too small without this
    var
    FScroll: Single;
    FInputText: string;

    { What to draw. }
    { Broken Text. }
    Broken_Text: TStringList;
    { Ignored (not visible) if not DrawInputText.
      Else broken InputText. }
    Broken_InputText: TStringList;
    Buttons: array of TCastleButton;

    MaxLineWidth: integer;
    { Sum of all Broken_Text.Count + Broken_InputText.Count.
      In other words, all lines that are scrolled by the scrollbar. }
    AllScrolledLinesCount: integer;
    { linie w calosci widoczne na ekranie (przydatne do obslugi page up / down),
      sposrod linii scrolled by the scrollbar (Text and InputText).
      Calculated in every ResizeMessg. }
    VisibleScrolledLinesCount: integer;

    { Min and max sensible values for @link(Scroll). }
    ScrollMin, ScrollMax: integer;

    ScrollMaxForScrollbar: integer;

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
    ScrollbarVisible: boolean; { Calculated in every ResizeMessg. }
    ScrollbarFrame: TRectangle;
    ScrollbarSlider: TRectangle;
    ScrollBarDragging: boolean;

    { set in MessageCore, readonly afterwards for various callbacks }

    { Main text to display. Read-only contents. }
    Text: TStringList;
    DrawBG: TGLImage; // window background
    Align: TTextAlign;
    { Should we display InputText }
    DrawInputText: boolean;

    { When implementing TCastleDialog descendants, set this
      to @true to signal that dialog window should be closed and
      the MessageCore should finish. }
    Answered: boolean;

    procedure SetScroll(Value: Single);
    { How many pixels up should be move the text.
      Kept as a float, to allow smooth time-based changes.
      Note that setting Scroll always clamps the value to sensible range. }
    property Scroll: Single read FScroll write SetScroll;
    procedure ScrollPageDown;
    procedure ScrollPageUp;

    procedure SetInputText(const value: string);
    { Displayed only if DrawInputText. }
    property InputText: string read FInputText write SetInputText;

    { Calculate height in pixels needed to draw Buttons.
      Returns 0 if there are no Buttons = ''. }
    function ButtonsHeight: Integer;
    procedure UpdateSizes;
    { The whole rectangle where we draw dialog box. }
    function WholeMessageRect: TRectangle;
    { If ScrollBarVisible, ScrollBarWholeWidth. Else 0. }
    function RealScrollBarWholeWidth: Integer;
    function Font: TGLBitmapFont;
  public
    procedure ContainerResize(const AContainerWidth, AContainerHeight: Cardinal); override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    function MouseMove(const OldX, OldY, NewX, NewY: Integer): boolean; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function DrawStyle: TUIControlDrawStyle; override;
    procedure Draw; override;
    function PositionInside(const X, Y: Integer): boolean; override;
  end;

procedure TCastleDialog.SetScroll(Value: Single);
begin
  Clamp(Value, ScrollMin, ScrollMax);
  if Value <> Scroll then
  begin
    FScroll := Value;
    VisibleChange;
  end;
end;

procedure TCastleDialog.ScrollPageDown;
var
  PageHeight: Single;
begin
  PageHeight := VisibleScrolledLinesCount * Font.RowHeight;
  Scroll := Scroll + PageHeight;
end;

procedure TCastleDialog.ScrollPageUp;
var
  PageHeight: Single;
begin
  PageHeight := VisibleScrolledLinesCount * Font.RowHeight;
  Scroll := Scroll - PageHeight;
end;

procedure TCastleDialog.SetInputText(const value: string);
begin
  FInputText := value;
  VisibleChange;
  UpdateSizes;
end;

function TCastleDialog.ButtonsHeight: Integer;
var
  Button: TCastleButton;
begin
  Result := 0;
  for Button in Buttons do
    MaxTo1st(Result, Button.Height + 2 * BoxMargin);
end;

procedure TCastleDialog.ContainerResize(const AContainerWidth, AContainerHeight: Cardinal);
var
  MessageRect: TRectangle;
  X, Y, I: Integer;
  Button: TCastleButton;
begin
  inherited;
  UpdateSizes;

  { Reposition Buttons. }
  if Length(Buttons) <> 0 then
  begin
    MessageRect := WholeMessageRect;
    X := MessageRect.Right  - BoxMargin;
    Y := MessageRect.Bottom + BoxMargin;
    for I := Length(Buttons) - 1 downto 0 do
    begin
      Button := Buttons[I];
      X -= Button.Width;
      Button.Left := X;
      Button.Bottom := Y;
      X -= ButtonHorizontalMargin;
    end;
  end;
end;

procedure TCastleDialog.UpdateSizes;
var
  BreakWidth, ButtonsWidth: integer;
  WindowScrolledHeight: Integer;
  Button: TCastleButton;
begin
  { calculate BreakWidth, which is the width at which we should break
    our string lists Broken_Xxx. We must here always subtract
    ScrollBarWholeWidth to be on the safe side, because we don't know
    yet is ScrollBarVisible. }
  BreakWidth := Max(0, ContainerWidth - BoxMargin * 2
    - WindowMargin * 2 - ScrollBarWholeWidth);

  { calculate MaxLineWidth and AllScrolledLinesCount }

  { calculate Broken_Text }
  Broken_Text.Clear;
  font.BreakLines(Text, Broken_Text,  BreakWidth);
  MaxLineWidth := font.MaxTextWidth(Broken_Text);
  AllScrolledLinesCount := Broken_Text.count;

  ButtonsWidth := 0;
  for Button in Buttons do
    ButtonsWidth += Button.Width + ButtonHorizontalMargin;
  if ButtonsWidth > 0 then
    ButtonsWidth -= ButtonHorizontalMargin; // extract margin from last button
  MaxTo1st(MaxLineWidth, ButtonsWidth);

  if DrawInputText then
  begin
    { calculate Broken_InputText }
    Broken_InputText.Clear;
    Font.BreakLines(InputText, Broken_InputText, BreakWidth);
    { It's our intention that if DrawInputText then *always*
      at least 1 line of InputText (even if it's empty) will be shown.
      That's because InputText is the editable text for the user,
      so there should be indication of "empty line". }
    if Broken_InputText.count = 0 then Broken_InputText.Add('');
    MaxLineWidth := max(MaxLineWidth, font.MaxTextWidth(Broken_InputText));
    AllScrolledLinesCount += Broken_InputText.count;
  end;

  { Now we have MaxLineWidth and AllScrolledLinesCount calculated }

  { Calculate WindowScrolledHeight --- number of pixels that are controlled
    by the scrollbar. }
  WindowScrolledHeight := ContainerHeight - BoxMargin * 2
    - WindowMargin * 2 - ButtonsHeight;

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
    then ScrollMin = 0
    so ScrollMin = ScrollMax,
    so Scroll will always be 0. }
  ScrollMin := -Font.RowHeight *
    (AllScrolledLinesCount - VisibleScrolledLinesCount);
  { ScrollMax jest stale ale to nic; wszystko bedziemy pisac
    tak jakby ScrollMax tez moglo sie zmieniac - byc moze kiedys zrobimy
    z tej mozliwosci uzytek. }
  ScrollMax := 0;
  ScrollMaxForScrollbar := ScrollMin + Font.RowHeight * AllScrolledLinesCount;

  { This clamps Scroll to proper range }
  Scroll := Scroll;
end;

function TCastleDialog.Press(const Event: TInputPressRelease): boolean;
var
  MY: Integer;
begin
  Result := inherited;
  if Result or (not GetExists) then Exit;

  { if not ScrollBarVisible then there is no point in changing Scroll
    (because always ScrollMin = ScrollMax = Scroll = 0).

    This way we allow descendants like TCastleKeyMouseDialog
    to handle K_PageDown, K_PageUp, K_Home and K_End keys
    and mouse wheel. And this is very good for MessageKey,
    when it's used e.g. to allow user to choose any TKey.
    Otherwise MessageKey would not be able to return
    K_PageDown, K_PageUp, etc. keys. }

  if ScrollBarVisible then
    case Event.EventType of
      itKey:
        case Event.Key of
          K_PageUp:   begin ScrollPageUp;        Result := true; end;
          K_PageDown: begin ScrollPageDown;      Result := true; end;
          K_Home:     begin Scroll := ScrollMin; Result := true; end;
          K_End:      begin Scroll := ScrollMax; Result := true; end;
        end;
      itMouseButton:
        begin
          MY := ContainerHeight - Container.MouseY;
          if (Event.MouseButton = mbLeft) and ScrollBarVisible and
            ScrollbarFrame.Contains(Container.MouseX, MY) then
          begin
            if MY < ScrollbarSlider.Bottom then
              ScrollPageDown else
            if MY >= ScrollbarSlider.Top then
              ScrollPageUp else
              ScrollBarDragging := true;
            Result := true;
          end;
        end;
      itMouseWheel:
        if Event.MouseWheelVertical then
        begin
          Scroll := Scroll - Event.MouseWheelScroll * Font.RowHeight;
          Result := true;
        end;
    end;
end;

function TCastleDialog.Release(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result or (not GetExists) then Exit;

  if Event.IsMouseButton(mbLeft) then
  begin
    ScrollBarDragging := false;
    Result := true;
  end;
end;

function TCastleDialog.MouseMove(const OldX, OldY, NewX, NewY: Integer): boolean;
begin
  Result := inherited;
  if Result or (not GetExists) then Exit;

  Result := ScrollBarDragging;
  if Result then
    Scroll := Scroll + (NewY- OldY) / ScrollbarFrame.Height *
      (ScrollMaxForScrollbar - ScrollMin);
end;

procedure TCastleDialog.Update(const SecondsPassed: Single;
  var HandleInput: boolean);

  function Factor: Single;
  begin
    result := 200.0 * SecondsPassed;
    if mkCtrl in Container.Pressed.Modifiers then result *= 6;
  end;

begin
  inherited;

  if HandleInput then
  begin
    if Container.Pressed[K_Up  ] then Scroll := Scroll - Factor;
    if Container.Pressed[K_Down] then Scroll := Scroll + Factor;
    HandleInput := not ExclusiveEvents;
  end;
end;

function TCastleDialog.DrawStyle: TUIControlDrawStyle;
begin
  if GetExists then
    Result := ds2D else
    Result := dsNone;
end;

procedure TCastleDialog.Draw;

  { Render a Text line, and move Y up to the line above. }
  procedure DrawString(const text: string; TextAlign: TTextAlign;
    X: Integer; var Y: Integer);
  begin
    { change X only locally, to take TextAlign into account }
    case TextAlign of
      taMiddle: X += (MaxLineWidth - font.TextWidth(text)) div 2;
      taRight : X +=  MaxLineWidth - font.TextWidth(text);
    end;
    font.Print(X, Y, text);
    { change Y for caller, to print next line higher }
    Y += font.RowHeight;
  end;

  { Render all lines in S, and move Y up to the line above. }
  procedure DrawStrings(const s: TStrings; TextAlign: TTextAlign;
    const X: Integer; var Y: Integer);
  var
    i: integer;
  begin
    for i := s.count-1 downto 0 do
      { each DrawString call will move Y up }
      DrawString(s[i], TextAlign, X, Y);
  end;

var
  MessageRect: TRectangle;
  { InnerRect to okienko w ktorym mieszcza sie napisy,
    a wiec WholeMessageRect zmniejszony o BoxMargin we wszystkich kierunkach
    i z ew. obcieta prawa czescia przeznaczona na ScrollbarFrame. }
  InnerRect: TRectangle;
  ScrollBarLength: integer;
  TextX, TextY: Integer;
const
  { odleglosc paska ScrollBara od krawedzi swojego waskiego recta
    (prawa krawedz jest zarazem krawedzia duzego recta !) }
  ScrollBarMargin = 2;
  { szerokosc paska ScrollBara }
  ScrollBarInternalWidth = ScrollBarWholeWidth - ScrollBarMargin * 2;
begin
  inherited;
  if not GetExists then Exit;

  { Robimy clear bo bgimg moze nie zakryc calego tla jezeli w trakcie MessageXxx
    user resized the window. }
  glClear(GL_COLOR_BUFFER_BIT);
  glLoadIdentity;

  DrawBG.Draw(0, 0);

  MessageRect := WholeMessageRect;

  Theme.Draw(MessageRect, tiWindow);

  MessageRect := MessageRect.RemoveBottom(ButtonsHeight);

  { calculate InnerRect }
  InnerRect := MessageRect.Grow(-BoxMargin);
  InnerRect.Width -= RealScrollBarWholeWidth;

  { draw scrollbar, and calculate it's rectangles }
  if ScrollBarVisible then
  begin
    ScrollbarFrame := MessageRect.RightPart(ScrollBarWholeWidth);
    Theme.Draw(ScrollbarFrame, tiScrollbarFrame);

    ScrollBarLength := MessageRect.Height - ScrollBarMargin*2;
    ScrollbarSlider := ScrollbarFrame;
    ScrollbarSlider.Height := VisibleScrolledLinesCount * ScrollBarLength
      div AllScrolledLinesCount;
    ScrollbarSlider.Bottom += Round(MapRange(Scroll,
      ScrollMin, ScrollMax, ScrollbarFrame.Height - ScrollbarSlider.Height, 0));
    Theme.Draw(ScrollbarSlider, tiScrollbarSlider);
  end else
  begin
    ScrollbarFrame := TRectangle.Empty;
    ScrollbarSlider := TRectangle.Empty;
  end;

  { Make scissor to cut off text that is too far up/down.
    We subtract Font.Descend from Y0, to see the descend of
    the bottom line (which is below InnerRect.Bottom, and would not be
    ever visible otherwise). }
  glScissor(InnerRect.Left, InnerRect.Bottom - Font.Descend,
    InnerRect.Width, InnerRect.Height + Font.Descend);
  glEnable(GL_SCISSOR_TEST);

  TextX := InnerRect.Left;
  TextY := InnerRect.Bottom + Round(Scroll);

  { rysuj Broken_InputText i Broken_Text.
    Kolejnosc ma znaczenie, kolejne linie sa rysowane od dolu w gore. }

  if DrawInputText then
  begin
    glColorv(Theme.MessageInputTextColor);
    DrawStrings(Broken_InputText, align, TextX, TextY);
  end;

  glColorv(Theme.MessageTextColor);
  DrawStrings(Broken_Text, align, TextX, TextY);

  glDisable(GL_SCISSOR_TEST);
end;

function TCastleDialog.PositionInside(const X, Y: Integer): boolean;
begin
  Result := true;
end;

function TCastleDialog.RealScrollBarWholeWidth: Integer;
begin
  Result := Iff(ScrollBarVisible, ScrollBarWholeWidth, 0);
end;

function TCastleDialog.WholeMessageRect: TRectangle;
begin
  Result := Rectangle(0, 0, ContainerWidth, ContainerHeight).Center(
    Min(MaxLineWidth + BoxMargin * 2 + RealScrollBarWholeWidth,
      ContainerWidth  - WindowMargin * 2),
    Min(AllScrolledLinesCount * Font.RowHeight + BoxMargin * 2 + ButtonsHeight,
      ContainerHeight - WindowMargin * 2));
end;

function TCastleDialog.Font: TGLBitmapFont;
begin
  Result := Theme.GLMessageFont;
end;

{ MessageCore ---------------------------------------------------------------- }

{ Show a modal dialod window. Uses TGLMode to temporarily replace
  normal event processing (on this Window) with it's own,
  to wait until the dialog is answered by the user.

  Given TextList is never modified here. }
procedure MessageCore(
  const Window: TCastleWindowCustom; const TextList: TStringList;
  const TextAlign: TTextAlign; const Dialog: TCastleDialog;
  const AButtons: array of TCastleButton;
  const ADrawInputText: boolean; var AInputText: string);
var
  SavedMode: TGLMode;
  I: Integer;
begin
  if Log then
    WritelnLogMultiline('Message', TextList.Text);

  { FlushRedisplay; W ten sposob po zainstalowaniu naszych callbackow
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

  SavedMode := TGLMode.CreateReset(Window,
    GL_PIXEL_MODE_BIT or GL_SCISSOR_BIT or GL_ENABLE_BIT or
    GL_LINE_BIT or GL_TRANSFORM_BIT or GL_COLOR_BUFFER_BIT,
    { Using @NoClose is good, it also allows users to safely use
      MessageXxx inside own OnCloseQuery, like
        if MessageYesNo('Are you sure ?') then Window.Close; }
    nil, nil, @NoClose);

  { FakeMouseDown must be @false.
    Otherwise closing dialog box with MouseDown will then cause MouseDown
    when SavedMode is restored. This is bad, because then the mouse click
    that closes dialog box could also do something else.
    Actually, FakeMouseDown is @false by default, so this call is not needed. }
  SavedMode.FakeMouseDown := false;

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

  { Inicjuj Dialog, ktora moze wymagac poprawnego ustawienia
    naszych rzeczy w OpenGLu (np. utworzenie fontu wymaga poprawnego ustawienia
    GL_UNPACK_xxx bo tworzenie fontu to generowanie jego display list a te
    zostaja utworzone z zapisanym na stale odpowiednim pixel-mode). }
  try
    { TODO: problem : chcielibysmy tutaj zeby SaveScreen zawsze dzialalo, bez
      wzgledu na ustawienia GL_PACK_xxx. Moglibysmy ustawiac wlasne GL_PACK
      ale lepszym wyjsciem byloby gdyby
      SaveScreen_noflush dzialalo zawsze tak samo, bez wzgledu na ustawienia
      GL_PACK_xxx. }
    with Dialog do
    begin
      if Window.DoubleBuffer then
        DrawBG := SaveScreenToGL_noflush(0, 0, Window.Width, Window.Height, GL_BACK) else
        DrawBG := SaveScreenToGL_noflush(0, 0, Window.Width, Window.Height, GL_FRONT);
      Answered := false;
      Text := TextList;
      { Contents of Broken_xxx will be initialized in TCastleDialog.UpdateSizes. }
      Broken_Text := TStringList.Create;
      Broken_InputText := TStringList.Create;
      Align := TextAlign;
      ScrollBarVisible := false;
      ScrollBarDragging := false;
      DrawInputText := AdrawInputText;
      FInputText := AInputText;
      SetLength(Buttons, Length(AButtons));
      for I := 0 to High(AButtons) do
      begin
        Buttons[I] := AButtons[I];
        Window.Controls.InsertFront(Buttons[I]);
      end;
    end;

    Window.Controls.InsertBack(Dialog);
    Dialog.Scroll := Dialog.ScrollMin;

    Window.PostRedisplay;
    repeat Application.ProcessMessage(true, true) until Dialog.Answered;

  finally
    FreeAndNil(Dialog.DrawBG);
    Dialog.Broken_Text.Free;
    Dialog.Broken_InputText.Free;
    AInputText := Dialog.InputText;

    { Odtwarzamy zasejwowane wlasciwosci okienka. }
    FreeAndNil(SavedMode);
  end;
end;

procedure MessageCore(
  const Window: TCastleWindowCustom; const TextList: TStringList;
  const TextAlign: TTextAlign; const Dialog: TCastleDialog;
  const AButtons: array of TCastleButton);
var
  Dummy: string;
begin
  Dummy := '';
  MessageCore(Window, TextList, TextAlign, Dialog, AButtons, false, Dummy);
end;

{ MessageOK ------------------------------------------------------------------ }

procedure MessageOK(Window: TCastleWindowCustom;  const SArray: array of string;
  TextAlign: TTextAlign = taMiddle);
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    AddStrArrayToStrings(SArray, TextList);
    MessageOK(Window, TextList, TextAlign);
  finally TextList.Free end;
end;

procedure MessageOK(Window: TCastleWindowCustom; const s: string; TextAlign: TTextAlign);
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    Strings_SetText(TextList, s);
    MessageOK(Window, TextList, TextAlign);
  finally TextList.free end;
end;

procedure MessageOK(Window: TCastleWindowCustom;  TextList: TStringList;
  TextAlign: TTextAlign);
begin
  MessageChar(Window, TextList,
    [CharEnter, CharEscape], ['OK'], [CharEnter], TextAlign, true);
end;

{ MessageInput function with callbacks --------------------------------------- }

type
  TCastleInputDialog = class(TCastleDialog)
  private
    { meaningful only if userCanCancel }
    answerCancelled: boolean;

    {pola ustawiane w wywolaniu MessageInput, read only z callbackow xxxMessgInput }
    answerMinLen, answerMaxLen: integer;
    answerAllowedChars: TSetOfChars;
    userCanCancel: boolean; { czy user moze wyjsc przez "Cancel" }
  public
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

  TInputDialogOKButton = class(TCastleButton)
  private
    Dialog: TCastleInputDialog;
  public
    procedure DoClick; override;
  end;

  TInputDialogCancelButton = class(TCastleButton)
  private
    Dialog: TCastleInputDialog;
  public
    procedure DoClick; override;
  end;

procedure TInputDialogOKButton.DoClick;
begin
  inherited;
  Dialog.Press(InputKey(K_Enter, CharEnter));
end;

procedure TInputDialogCancelButton.DoClick;
begin
  inherited;
  Dialog.Press(InputKey(K_Escape, CharEscape));
end;

function TCastleInputDialog.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result or (not GetExists) then Exit;

  { Under Windows, pressing ctrl+backspace causes key = K_BackSpace with
    character = CharDelete. That is, Windows automatically replaces ctrl+backspace
    with delete (leaving it ambigous what should I do --- look at key code or
    character?). Here, I want to detect ctrl+backspace, and not detect "real" delete
    key presses (that may be handled in the future, right now there's no cursor
    so delete doesn't work, only backspace). So I just look at both Key and C
    to detect backspace. }
  if Event.IsKey(CharBackSpace) or Event.IsKey(K_BackSpace) then
  begin
    if InputText <> '' then
      if mkCtrl in Container.Pressed.Modifiers then
        InputText := '' else
        InputText := Copy(InputText, 1, Length(InputText) - 1);
  end else
  if Event.IsKey(CharEnter) then
  begin
    if Length(InputText) >= answerMinLen then
      answered := true;
      { No nice way to show it for now?
      else
      MessageOk(Window, Format('You must enter at least %d characters.',
        [answerMinLen]), taMiddle); }
  end else
  if Event.IsKey(CharEscape) then
  begin
    if userCanCancel then
    begin
      answerCancelled := true;
      answered := true;
    end;
  end else
  if Event.IsKey(CtrlC) then
  begin
    if InputText <> '' then
      Clipboard.AsText := InputText;
  end else
  if Event.IsKey(CtrlX) then
  begin
    if InputText <> '' then
    begin
      Clipboard.AsText := InputText;
      InputText := '';
    end;
  end else
  if Event.IsKey(CtrlV) then
    InputText := Clipboard.AsText else
  if (Event.EventType = itKey) and
     (Event.KeyCharacter <> #0) and
     (Event.KeyCharacter in answerAllowedChars) and
     ((answerMaxLen = 0) or (Length(InputText) < answerMaxLen)) then
    InputText := InputText + Event.KeyCharacter;
end;

function MessageInput(Window: TCastleWindowCustom; const s: string;
  TextAlign: TTextAlign; const answerDefault: string;
  answerMinLen: integer; answerMaxLen: integer; const answerAllowedChars: TSetOfChars): string;
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    Strings_SetText(TextList, s);
    result := MessageInput(Window, TextList, TextAlign, answerDefault,
      answerMinLen, answerMaxLen, answerAllowedChars);
  finally TextList.free end;
end;

function MessageInput(Window: TCastleWindowCustom; TextList: TStringList;
  TextAlign: TTextAlign; const answerDefault: string;
  answerMinLen: integer; answerMaxLen: integer; const answerAllowedChars: TSetOfChars): string;
var
  Dialog: TCastleInputDialog;
  OKButton: TInputDialogOKButton;
  ReadyButtons: array of TCastleButton;
begin
  Dialog := TCastleInputDialog.Create(nil);
  try
    Dialog.answerMinLen := answerMinLen;
    Dialog.answerMaxLen := answerMaxLen;
    Dialog.answerAllowedChars := answerAllowedChars;
    Dialog.userCanCancel := false;
    Dialog.answerCancelled := false;
    Result := answerDefault;

    OKButton := TInputDialogOKButton.Create(Dialog);
    OKButton.Dialog := Dialog;
    OKButton.Caption := 'OK';
    OKButton.MinWidth := TCastleDialog.MinButtonWidth;
    SetLength(ReadyButtons, 1);
    ReadyButtons[0] := OKButton;

    MessageCore(Window, TextList, TextAlign, Dialog, ReadyButtons, true, result);
  finally FreeAndNil(Dialog) end;
end;

function MessageInputQuery(Window: TCastleWindowCustom; const s: string;
  var answer: string; TextAlign: TTextAlign;
  answerMinLen: integer; answerMaxLen: integer; const answerAllowedChars: TSetOfChars): boolean;
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    Strings_SetText(TextList, s);
    result := MessageInputQuery(Window, TextList, answer, TextAlign, answerMinLen,
      answerMaxLen, answerAllowedChars);
  finally TextList.free end;
end;

function MessageInputQuery(Window: TCastleWindowCustom; TextList: TStringList;
  var answer: string; TextAlign: TTextAlign;
  answerMinLen: integer; answerMaxLen: integer; const answerAllowedChars: TSetOfChars): boolean;
var
  Dialog: TCastleInputDialog;
  InputText: string;
  OKButton: TInputDialogOKButton;
  CancelButton: TInputDialogCancelButton;
  ReadyButtons: array of TCastleButton;
begin
  Dialog := TCastleInputDialog.Create(nil);
  try
    Dialog.answerMinLen := answerMinLen;
    Dialog.answerMaxLen := answerMaxLen;
    Dialog.answerAllowedChars := answerAllowedChars;
    Dialog.userCanCancel := true;
    Dialog.answerCancelled := false;

    { uzywamy dodatkowej zmiennej InputText zamiast bezposrednio przekazywac
      MessageCore zmienna answer bo jezeli not result to nie chcemy zmieniac
      answer. }
    InputText := answer;

    OKButton := TInputDialogOKButton.Create(Dialog);
    OKButton.Dialog := Dialog;
    OKButton.Caption := 'OK';
    OKButton.MinWidth := TCastleDialog.MinButtonWidth;
    CancelButton := TInputDialogCancelButton.Create(Dialog);
    CancelButton.Dialog := Dialog;
    CancelButton.Caption := 'Cancel';
    CancelButton.MinWidth := TCastleDialog.MinButtonWidth;
    SetLength(ReadyButtons, 2);
    ReadyButtons[0] := CancelButton;
    ReadyButtons[1] := OKButton;

    MessageCore(Window, TextList, TextAlign, Dialog, ReadyButtons,
      true, InputText);
    result := not Dialog.answerCancelled;
    if result then answer := InputText;
  finally FreeAndNil(Dialog) end;
end;

{ MessageChar ---------------------------------------------------------------- }

type
  TCastleCharDialog = class(TCastleDialog)
  public
    AllowedChars: TSetOfChars;
    Answer: char;
    IgnoreCase: boolean;
  public
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

function TCastleCharDialog.Press(const Event: TInputPressRelease): boolean;
var
  C: char;
begin
  Result := inherited;
  if Result or (not GetExists) then Exit;

  if Event.EventType <> itKey then Exit;
  C := Event.KeyCharacter;

  if IgnoreCase then
  begin
    if (UpCase(C) in AllowedChars) or
       (LoCase(C) in AllowedChars) then
    begin
      Answered := true;
      Answer := LoCase(c);
    end;
  end else
  begin
    if c in AllowedChars then
    begin
      Answered := true;
      Answer := c;
    end;
  end;
end;

type
  TCharDialogButton = class(TCastleButton)
  private
    Dialog: TCastleCharDialog;
    Answer: char;
  public
    procedure DoClick; override;
  end;

procedure TCharDialogButton.DoClick;
begin
  inherited;
  Dialog.Answered := true;
  if Dialog.IgnoreCase then
    Dialog.Answer := LoCase(Answer) else
    Dialog.Answer := Answer;
end;

function MessageChar(Window: TCastleWindowCustom;
  const s: string; const AllowedChars: TSetOfChars;
  const Buttons: array of string; const ButtonsChars: array of char;
  TextAlign: TTextAlign; IgnoreCase: boolean): char;
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    Strings_SetText(TextList, s);
    Result := MessageChar(Window, TextList, AllowedChars,
      Buttons, ButtonsChars, TextAlign, IgnoreCase);
  finally TextList.free end;
end;

function MessageChar(Window: TCastleWindowCustom;
  const SArray: array of string; const AllowedChars: TSetOfChars;
  const Buttons: array of string; const ButtonsChars: array of char;
  TextAlign: TTextAlign; IgnoreCase: boolean): char; overload;
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    AddStrArrayToStrings(SArray, TextList);
    Result := MessageChar(Window, TextList, AllowedChars,
      Buttons, ButtonsChars, TextAlign, IgnoreCase);
  finally TextList.Free end;
end;

function MessageChar(Window: TCastleWindowCustom; TextList: TStringList;
  const AllowedChars: TSetOfChars;
  const Buttons: array of string; const ButtonsChars: array of char;
  TextAlign: TTextAlign; IgnoreCase: boolean): char; overload;
var
  Dialog: TCastleCharDialog;
  Button: TCharDialogButton;
  ReadyButtons: array of TCastleButton;
  I: Integer;
begin
  Assert(Length(Buttons) = Length(ButtonsChars));

  Dialog := TCastleCharDialog.Create(nil);
  try
    Dialog.AllowedChars := AllowedChars;
    Dialog.IgnoreCase := IgnoreCase;
    SetLength(ReadyButtons, Length(Buttons));
    for I := 0 to Length(Buttons) - 1 do
    begin
      Button := TCharDialogButton.Create(Dialog);
      Button.Dialog := Dialog;
      Button.Caption := Buttons[I];
      Button.MinWidth := TCastleDialog.MinButtonWidth;
      Button.Answer := ButtonsChars[I];
      ReadyButtons[I] := Button;
    end;
    MessageCore(Window, TextList, TextAlign, Dialog, ReadyButtons);
    Result := Dialog.Answer;
  finally FreeAndNil(Dialog) end;
end;

{ MessageKey functions with callbacks --------------------------------------- }

type
  TCastleKeyDialog = class(TCastleDialog)
  private
    Answer: TKey;
  public
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

function TCastleKeyDialog.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result or (not GetExists) then Exit;

  if (Event.EventType = itKey) and (Event.Key <> K_None) then
  begin
    Answered := true;
    Answer := Event.Key;
  end;
end;

function MessageKey(Window: TCastleWindowCustom; const S: string;
  TextAlign: TTextAlign): TKey;
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    Strings_SetText(TextList, S);
    Result := MessageKey(Window, TextList, TextAlign);
  finally TextList.free end;
end;

function MessageKey(Window: TCastleWindowCustom; const SArray: array of string;
  TextAlign: TTextAlign): TKey;
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    AddStrArrayToStrings(SArray, TextList);
    Result := MessageKey(Window, TextList, TextAlign);
  finally TextList.Free end;
end;

function MessageKey(Window: TCastleWindowCustom; TextList: TStringList;
  TextAlign: TTextAlign): TKey;
var
  Dialog: TCastleKeyDialog;
begin
  Dialog := TCastleKeyDialog.Create(nil);
  try
    MessageCore(Window, TextList, TextAlign, Dialog, []);
    Result := Dialog.Answer;
  finally FreeAndNil(Dialog) end;
end;

{ MessageKeyMouse ------------------------------------------------------------ }

type
  TCastleKeyMouseDialog = class(TCastleDialog)
  private
    Answer: TInputPressRelease;
  public
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

function TCastleKeyMouseDialog.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result or (not GetExists) then Exit;

  if (Event.EventType <> itKey) or (Event.Key <> K_None) then
  begin
    Answered := true;
    Answer := Event;
  end;
end;

procedure MessageKeyMouse(Window: TCastleWindowCustom; const S: string;
  TextAlign: TTextAlign; out Event: TInputPressRelease);
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    Strings_SetText(TextList, S);
    MessageKeyMouse(Window, TextList, TextAlign, Event);
  finally TextList.Free end;
end;

procedure MessageKeyMouse(Window: TCastleWindowCustom; TextList: TStringList;
  TextAlign: TTextAlign; out Event: TInputPressRelease);
var
  Dialog: TCastleKeyMouseDialog;
begin
  Dialog := TCastleKeyMouseDialog.Create(nil);
  try
    MessageCore(Window, TextList, TextAlign, Dialog, []);
    Event := Dialog.Answer;
  finally FreeAndNil(Dialog) end;
end;

{ MessageYesNo --------------------------------------------------------------- }

function MessageYesNo(Window: TCastleWindowCustom; const s: string;
  TextAlign: TTextAlign): boolean; overload;
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    Strings_SetText(TextList, S);
    Result := MessageYesNo(Window, TextList, TextAlign);
  finally TextList.free end;
end;

function MessageYesNo(Window: TCastleWindowCustom; const SArray: array of string;
  TextAlign: TTextAlign): boolean; overload;
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    AddStrArrayToStrings(SArray, TextList);
    Result := MessageYesNo(Window, TextList, TextAlign);
  finally TextList.Free end;
end;

function MessageYesNo(Window: TCastleWindowCustom; TextList: TStringList;
  TextAlign: TTextAlign): boolean; overload;
begin
  Result := MessageChar(Window, TextList, ['y', 'n'], ['Yes', 'No'], ['y', 'n'],
    TextAlign, true) = 'y';
end;

{ MessageInputCardinal ------------------------------------------------------- }

function MessageInputCardinal(Window: TCastleWindowCustom; const s: string;
  TextAlign: TTextAlign; const AnswerDefault: string): Cardinal;
begin
  result := StrToInt( MessageInput(Window, s, TextAlign, AnswerDefault,
    1, 0, ['0'..'9']) );
end;

function MessageInputCardinal(Window: TCastleWindowCustom; const s: string;
  TextAlign: TTextAlign; AnswerDefault: Cardinal): Cardinal;
begin
  result := MessageInputCardinal(Window, s, TextAlign, IntToStr(AnswerDefault));
end;

function MessageInputQueryCardinal(Window: TCastleWindowCustom; const Title: string;
  var Value: Cardinal; TextAlign: TTextAlign): boolean;
var
  ValueStr: string;
begin
  ValueStr := IntToStr(Value);
  Result := MessageInputQuery(Window, Title, ValueStr, TextAlign, 1, 0, ['0'..'9']);
  if Result then
    Value := StrToInt(ValueStr);
end;

function MessageInputQueryCardinalHex(Window: TCastleWindowCustom; const Title: string;
  var Value: Cardinal; TextAlign: TTextAlign; MaxWidth: Cardinal): boolean;
var
  ValueStr: string;
begin
  ValueStr := IntToHex(Value, 4);
  Result := MessageInputQuery(Window, Title, ValueStr, TextAlign, 1, MaxWidth,
    ['0'..'9', 'a'..'f', 'A'..'F']);
  if Result then
    Value := StrHexToInt(ValueStr);
end;

{ MessageInputQuery on floats ------------------------------------------------ }

function MessageInputQuery(Window: TCastleWindowCustom; const Title: string;
  var Value: Extended; TextAlign: TTextAlign; const ValueAsString: string): boolean;
var
  s: string;
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
        MessageOK(Window, 'Invalid floating point value : ' +E.Message, taLeft);
    end;
  end;
end;

function MessageInputQuery(Window: TCastleWindowCustom; const Title: string;
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
function MessageInputQuery(Window: TCastleWindowCustom; const Title: string;
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
  Window: TCastleWindowCustom; const Title: string;
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
        MessageOK(Window, 'Invalid vector 3 value : ' + E.Message, taLeft);
    end;
  end;
end;

{ MessageInputQueryVector4Single --------------------------------------------- }

function MessageInputQueryVector4Single(
  Window: TCastleWindowCustom; const Title: string;
  var Value: TVector4Single; TextAlign: TTextAlign): boolean;
var
  s: string;
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
        MessageOK(Window, 'Invalid vector 4 value : ' + E.Message, taLeft);
    end;
  end;
end;

end.
