{
  Copyright 2001-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

(* Dialog windows (asking user for confirmation, question,
  simple text input and such) displayed within an OpenGL context (TCastleWindow).

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
      (As long as TCastleWindow.ResizeAllowed = raAllowed, of course.)
      Long lines are automatically broken taking into account current window
      width.)

    @item(You can configure dialog boxes look using TCastleTheme.
      Various parts of the dialog use scaled images.
      This way you can change the border, background of the dialog,
      you can also make the dialog box partially transparent.)
  )

  Call MessageXxx functions only when Window.Closed = false.

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
      TCastleWindow.Close from TCastleApplication callbacks. )

    @item(Since your normal callbacks
      and controls are not run when message box is running, you usually
      don't need to do anything special about it, unless you use
      TCastleApplication callbacks mentioned above.)
  )
*)

unit CastleMessages;

{$I castleconf.inc}

interface

uses Classes, CastleWindow, CastleGLUtils, CastleUtils,
  CastleStringUtils, CastleVectors, CastleKeysMouse, CastleControls,
  CastleRectangles;

type
  { Position of text in message dialogs.
    Deprecated, use THorizontalPosition instead. }
  TTextAlign = THorizontalPosition deprecated;
const
  DefaultAlign = hpLeft;
  taRight = hpRight deprecated;
  taMiddle = hpMiddle deprecated;
  taLeft = hpLeft deprecated;

{ Ask user for simple confirmation. This is the simplest "OK" dialog box.

  @groupBegin }
procedure MessageOK(Window: TCastleWindow; const s: string;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false); overload;
procedure MessageOK(Window: TCastleWindow;  const SArray: array of string;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false); overload;
procedure MessageOK(Window: TCastleWindow;  TextList: TStringList;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false); overload;
{ @groupEnd }

{ Ask user to input a string.
  User must give an answer (there is no "Cancel" button),
  use MessageInputQuery if you want a version with "Cancel" button.
  @param AnswerMaxLen 0 (zero) means that there's no maximum answer length.

  @groupBegin }
function MessageInput(Window: TCastleWindow; const s: string;
  const answerDefault: string = '';
  const MinLength: integer = 0;
  const MaxLength: integer = 0;
  const AllowedChars: TSetOfChars = AllChars;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): string; overload;
function MessageInput(Window: TCastleWindow; TextList: TStringList;
  const answerDefault: string = '';
  const MinLength: integer = 0;
  const MaxLength: integer = 0;
  const AllowedChars: TSetOfChars = AllChars;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): string; overload;
{ @groupEnd }

{ Ask user to input a string, or cancel.
  Returns @true and sets Answer if user accepted some text.
  Note that initial Answer value is the answer proposed to the user.
  @param AnswerMaxLen 0 (zero) means that there's no maximum answer length.

  @groupBegin }
function MessageInputQuery(Window: TCastleWindow; const s: string;
  var Answer: string;
  const MinLength: integer = 0;
  const MaxLength: integer = 0;
  const AllowedChars: TSetOfChars = AllChars;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): boolean; overload;
function MessageInputQuery(Window: TCastleWindow; TextList: TStringList;
  var Answer: string;
  const MinLength: integer = 0;
  const MaxLength: integer = 0;
  const AllowedChars: TSetOfChars = AllChars;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): boolean; overload;
{ @groupEnd }

{ Ask user to choose one option from many.
  ButtonCaptions contain a list of button captions.

  ButtonChars (must have always the same length as ButtonCaptions)
  contains the chars that are returned for each corresponding button press.
  The user can also directly press the given key. ButtonChars is not case
  sensitive, all letters on this list must be different (not only in case).
  We always return a lowercase letter corresponding to one of ButtonChars letters.

  Example usage:

  @longCode(#
    case MessageChoice(Window, 'Which fruit to you want to eat?',
      ['Apple', 'Banana', 'Cancel'],
      ['a', 'b', CharEscape]) of
      'a': // ... user pressed "Apple" button or "A" key -> likes apples
      'b': // ... user pressed "Banana" button or "B" key -> likes bananas
      CharEscape: // ... user pressed "Cancel" button or "Escape" key -> cancelled
    end;
  #)

  When AllowCancel, user can always press Escape
  to easily cancel the dialog. Regardless if CharEscape is among ButtonChars.
  We return CharEscape then.

  @groupBegin }
function MessageChoice(Window: TCastleWindow; const s: string;
  const ButtonCaptions: array of string; const ButtonChars: array of char;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false;
  const AllowCancel: boolean = false): char; overload;
function MessageChoice(Window: TCastleWindow; const SArray: array of string;
  const ButtonCaptions: array of string; const ButtonChars: array of char;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false;
  const AllowCancel: boolean = false): char; overload;
function MessageChoice(Window: TCastleWindow; TextList: TStringList;
  const ButtonCaptions: array of string; const ButtonChars: array of char;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false;
  const AllowCancel: boolean = false): char; overload;
{ @groupEnd }

{ Ask user to press any key, return this key as Keys.TKey.

  Never returns keyNone (which means that keys that cannot be interpreted
  as Keys.TKey will be ignored, and will not close the dialog box).

  @groupBegin }
function MessageKey(Window: TCastleWindow; const S: string;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): TKey; overload;
function MessageKey(Window: TCastleWindow; const SArray: array of string;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): TKey; overload;
function MessageKey(Window: TCastleWindow; TextList: TStringList;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): TKey; overload;
{ @groupEnd }

{ Ask user to press anything (key or mouse button or mouse wheel),
  and return it.
  The natural use for this is to allow user to configure
  keybindings of your program, like for TInputShortcut.
  @groupBegin }
procedure MessageKeyMouse(Window: TCastleWindow; const S: string;
  out Event: TInputPressRelease;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false); overload; deprecated 'use the function form of MessageKeyMouse';
procedure MessageKeyMouse(Window: TCastleWindow; TextList: TStringList;
  out Event: TInputPressRelease;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false); overload; deprecated 'use the function form of MessageKeyMouse';
function MessageKeyMouse(Window: TCastleWindow; const S: string;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): TInputPressRelease; overload;
function MessageKeyMouse(Window: TCastleWindow; TextList: TStringList;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): TInputPressRelease; overload;
{ @groupEnd }

function MessageYesNo(Window: TCastleWindow; const s: string;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): boolean; overload;
function MessageYesNo(Window: TCastleWindow;  const SArray: array of string;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): boolean; overload;
function MessageYesNo(Window: TCastleWindow;  TextList: TStringList;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): boolean; overload;

{ Ask user to input an unsigned integer.

  Note that AnswerDefault below may be given as Cardinal or as a string.
  The latter is useful if you want the default answer to be '', i.e. empty string
  --- no default answer.

  @groupBegin }
function MessageInputCardinal(Window: TCastleWindow; const s: string;
  const AnswerDefault: string;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): Cardinal; overload;
function MessageInputCardinal(Window: TCastleWindow; const s: string;
  const AnswerDefault: Cardinal;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): Cardinal; overload;
function MessageInputQueryCardinal(Window: TCastleWindow; const Title: string;
  var Value: Cardinal;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): boolean;
{ @groupEnd }

{ Ask user to input a value in hexadecimal.
  Give MaxWidth = 0 to say that there is no maximum width. }
function MessageInputQueryCardinalHex(Window: TCastleWindow; const Title: string;
  var Value: Cardinal; const MaxWidth: Cardinal;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): boolean;

{ Ask user to input a floating-point number.
  Number can use CastleScript expressions, so e.g. "1/30", "2+2" or "pi/2" are allowed too.

  If you give non-empty ValueAsString, it will be used to show
  the initial value for the user. Otherwise, we will just show
  FloatToStrDot(Value), which sometimes may be too ugly.
  For example Value = 0.01 cannot be precisely represented as a floating point
  number, and FloatToStrDot shows that this is really something like 0.0099xxxxx.

  @groupBegin }
function MessageInputQuery(Window: TCastleWindow; const Title: string;
  var Value: Extended;
  const ValueAsString: string = '';
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): boolean; overload;
function MessageInputQuery(Window: TCastleWindow; const Title: string;
  var Value: Single;
  const ValueAsString: string = '';
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): boolean; overload;
{$ifndef EXTENDED_EQUALS_DOUBLE}
function MessageInputQuery(Window: TCastleWindow; const Title: string;
  var Value: Double;
  const ValueAsString: string = '';
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): boolean; overload;
{$endif not EXTENDED_EQUALS_DOUBLE}
{ @groupEnd }

function MessageInputQueryVector3(
  Window: TCastleWindow; const Title: string;
  var Value: TVector3;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): boolean;

function MessageInputQueryVector4(
  Window: TCastleWindow; const Title: string;
  var Value: TVector4;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): boolean;

var
  { Change MessageOK behavior to create @link(TViewDialogOK)
    and push it (using @link(TCastleContainer.PushView))
    and immediately return, without waiting for user confirmation.

    Why you may want to use this (or not to use this)?

    @unorderedList(
      @item(
        This is the only way to make MessageOK working on iOS
        (where Application.ProcessMessages is not available).)

      @item(
        This looks a little better in case user may resize the window while
        the MessageOK is running, or when something animates under MessageOK.
        When this is @true, the view underneath redraws properly, so it can adjust
        to window size and animate. Otherwise, all MessageXxx methods in this unit
        only show a static screenshot underneath, that is scaled if needed.)

      @item(
        On the other hand, the notification about unhandled exceptions
        (done by CastleWindow automatically, using MessageOK) is a little safer
        when this is @false. When this is @false, there's a greater chance that
        the problematic code (e.g. the update method of some other TCastleView)
        it disabled during the display of the error messsage.)
    )

    If you turn this on, then you should organize your whole application
    into views using TCastleView. You can even pause the running game
    in overridden @link(TCastleView.Pause), to make game paused when
    the message dialog is displayed.

    Note that this feature doesn't change other routines in CastleMessages.
    For example @link(MessageYesNo) is still a modal function (it waits
    for user input), and it simply doesn't work on iOS.
    If you want to use TCastleView to manage all dialogs,
    then use explicitly views like @link(TViewDialogYesNo) from
    the @link(CastleDialogViews) unit.
  }
  MessageOKPushesView: boolean;

implementation

uses SysUtils,
  CastleImages, CastleClassUtils, CastleInternalWindowModes, CastleLog,
  CastleUIControls, CastleDialogViews, CastleScriptParser;

{ MessageCore ---------------------------------------------------------------- }

{ Show a modal dialog window. Uses TGLMode to temporarily replace
  normal event processing (on this Window),
  to wait until the dialog is answered by the user. }
procedure MessageCore(const Window: TCastleWindow; const View: TViewDialog);
var
  SavedMode: TGLMode;
begin
  // WritelnLogMultiline('Message', TextList.Text);

  { TODO:
    This way of starting the View manually is a hack.

    This hack avoids calling InternalStart / InternalStop,
    which modify container view stack (we don't want this)
    and do other useful things (we don't need it *for now*) like

    - handling WaitingForRender
    - handling DesignUrl
    - freeing FFreeAtStop
    - setting FStartContainer

    A cleaner way would be to use it like a proper view.
    - set View.PopOnAnswered := true;
    - do not call
        View.Start;
        ...
        Window.Controls.InsertFront(View);
      explicitly.
    - do call
        Window.Container.View := View;

    Problem of the cleaner solution: it means we'll change container FViewStack,
    wel'll do stop (and later start) on the user TCastleView descendants.
    We don't want this, MessageOK (esp. when it is used to display a debug message
    in case of unhandled exception on mobile) should avoid calling user code of views
    (as user views may be in bad view). }

  View.BackgroundScreenshot := true;
  View.PopOnAnswered := false;
  View.SaveScreenIfNecessary(Window.Container); // get a screenshot before TGLMode.CreateReset
  View.Start;

  { Using @NoClose below allows to safely use MessageXxx inside own OnCloseQuery,
    like "if MessageYesNo('Are you sure ?') then Window.Close;" }
  SavedMode := TGLMode.CreateReset(Window, nil, nil, @NoClose);
  try
    { use View directly as UI control, not using TCastleContainer.PushView
      nor setting TCastleContainer.View,
      because we can (TGLMode already took care to pause everything),
      and this way we don't mess TCastleView stack (in case game is using it). }
    Window.Controls.InsertFront(View);

    repeat
      { WaitForMessage = false is necessary, otherwise SecondsPassed
        for update would be large. }
      Application.ProcessMessage(false, true)
    until View.Answered;
  finally
    FreeAndNil(SavedMode);
    { Message boxes should not leave the keys in false/strange pressed view. }
    Window.Pressed.Clear;
  end;

  View.Stop;
end;

{ MessageOK ------------------------------------------------------------------ }

procedure MessageOK(Window: TCastleWindow; const SArray: array of string;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false);
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    AddStrArrayToStrings(SArray, TextList);
    MessageOK(Window, TextList, Alignment, Html);
  finally TextList.Free end;
end;

procedure MessageOK(Window: TCastleWindow; const s: string;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false);
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    Strings_SetText(TextList, s);
    MessageOK(Window, TextList, Alignment, Html);
  finally TextList.free end;
end;

procedure MessageOK(Window: TCastleWindow; TextList: TStringList;
  const Alignment: THorizontalPosition = DefaultAlign;
  const Html: boolean = false);
var
  View: TViewDialogOK;
begin
  View := TViewDialogOK.Create(Window);
  View.Text.Assign(TextList);
  View.Alignment := Alignment;
  View.Html := Html;
  if MessageOKPushesView then
  begin
    Window.Container.PushView(View);
  end else
  begin
    MessageCore(Window, View);
    FreeAndNil(View);
  end;
end;

{ MessageInput --------------------------------------------------------------- }

function MessageInput(Window: TCastleWindow; const s: string;
  const answerDefault: string;
  const MinLength: integer; const MaxLength: integer;
  const AllowedChars: TSetOfChars;
  const Alignment: THorizontalPosition;
  const Html: boolean): string;
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    Strings_SetText(TextList, s);
    result := MessageInput(Window, TextList, answerDefault,
      MinLength, MaxLength, AllowedChars, Alignment, Html);
  finally TextList.free end;
end;

function MessageInput(Window: TCastleWindow; TextList: TStringList;
  const answerDefault: string;
  const MinLength: integer; const MaxLength: integer;
  const AllowedChars: TSetOfChars;
  const Alignment: THorizontalPosition;
  const Html: boolean): string;
var
  View: TViewDialogInput;
begin
  View := TViewDialogInput.Create(Window);
  try
    View.Text.Assign(TextList);
    View.Alignment := Alignment;
    View.Html := Html;
    View.MinLength := MinLength;
    View.MaxLength := MaxLength;
    View.AllowedChars := AllowedChars;
    View.Answer := AnswerDefault;
    View.CanCancel := false;

    MessageCore(Window, View);
    Result := View.Answer;
  finally FreeAndNil(View) end;
end;

function MessageInputQuery(Window: TCastleWindow; const s: string;
  var Answer: string;
  const MinLength: integer; const MaxLength: integer;
  const AllowedChars: TSetOfChars;
  const Alignment: THorizontalPosition;
  const Html: boolean): boolean;
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    Strings_SetText(TextList, s);
    result := MessageInputQuery(Window, TextList, answer, MinLength,
      MaxLength, AllowedChars, Alignment, Html);
  finally TextList.free end;
end;

function MessageInputQuery(Window: TCastleWindow; TextList: TStringList;
  var Answer: string;
  const MinLength: integer; const MaxLength: integer;
  const AllowedChars: TSetOfChars;
  const Alignment: THorizontalPosition;
  const Html: boolean): boolean;
var
  View: TViewDialogInput;
begin
  View := TViewDialogInput.Create(Window);
  try
    View.Text.Assign(TextList);
    View.Alignment := Alignment;
    View.Html := Html;
    View.MinLength := MinLength;
    View.MaxLength := MaxLength;
    View.AllowedChars := AllowedChars;
    View.Answer := Answer;
    View.CanCancel := true;

    MessageCore(Window, View);
    Result := not View.AnswerCancelled;
    if Result then // modify Answer only if not cancelled
      Answer := View.Answer;
  finally FreeAndNil(View) end;
end;

{ MessageChoice -------------------------------------------------------------- }

function MessageChoice(Window: TCastleWindow;
  const s: string;
  const ButtonCaptions: array of string; const ButtonChars: array of char;
  const Alignment: THorizontalPosition;
  const Html, AllowCancel: boolean): char;
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    Strings_SetText(TextList, s);
    Result := MessageChoice(Window, TextList, ButtonCaptions, ButtonChars, Alignment, Html, AllowCancel);
  finally TextList.free end;
end;

function MessageChoice(Window: TCastleWindow;
  const SArray: array of string;
  const ButtonCaptions: array of string; const ButtonChars: array of char;
  const Alignment: THorizontalPosition;
  const Html, AllowCancel: boolean): char; overload;
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    AddStrArrayToStrings(SArray, TextList);
    Result := MessageChoice(Window, TextList, ButtonCaptions, ButtonChars, Alignment, Html, AllowCancel);
  finally TextList.Free end;
end;

function MessageChoice(Window: TCastleWindow; TextList: TStringList;
  const ButtonCaptions: array of string; const ButtonChars: array of char;
  const Alignment: THorizontalPosition;
  const Html, AllowCancel: boolean): char; overload;
var
  View: TViewDialogChoice;
  I: Integer;
begin
  View := TViewDialogChoice.Create(Window);
  try
    View.Text.Assign(TextList);
    View.Alignment := Alignment;
    View.Html := Html;
    //View.ButtonCaptions := ButtonCaptions;
    SetLength(View.ButtonCaptions, High(ButtonCaptions) + 1);
    for I := 0 to High(ButtonCaptions) do
      View.ButtonCaptions[I] := ButtonCaptions[I];
    //View.ButtonChars := ButtonChars;
    SetLength(View.ButtonChars, High(ButtonChars) + 1);
    for I := 0 to High(ButtonChars) do
      View.ButtonChars[I] := ButtonChars[I];
    View.AllowCancel := AllowCancel;
    MessageCore(Window, View);
    Result := View.Answer;
  finally FreeAndNil(View) end;
end;

{ MessageKey ----------------------------------------------------------------- }

function MessageKey(Window: TCastleWindow; const S: string;
  const Alignment: THorizontalPosition;
  const Html: boolean): TKey;
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    Strings_SetText(TextList, S);
    Result := MessageKey(Window, TextList, Alignment, Html);
  finally TextList.free end;
end;

function MessageKey(Window: TCastleWindow; const SArray: array of string;
  const Alignment: THorizontalPosition;
  const Html: boolean): TKey;
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    AddStrArrayToStrings(SArray, TextList);
    Result := MessageKey(Window, TextList, Alignment, Html);
  finally TextList.Free end;
end;

function MessageKey(Window: TCastleWindow; TextList: TStringList;
  const Alignment: THorizontalPosition;
  const Html: boolean): TKey;
var
  View: TViewDialogKey;
begin
  View := TViewDialogKey.Create(Window);
  try
    View.Text.Assign(TextList);
    View.Alignment := Alignment;
    View.Html := Html;
    MessageCore(Window, View);
    Result := View.Answer;
  finally FreeAndNil(View) end;
end;

{ MessageKeyMouse ------------------------------------------------------------ }

function MessageKeyMouse(Window: TCastleWindow; const S: string;
  const Alignment: THorizontalPosition; const Html: boolean): TInputPressRelease;
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    Strings_SetText(TextList, S);
    Result := MessageKeyMouse(Window, TextList, Alignment, Html);
  finally TextList.Free end;
end;

function MessageKeyMouse(Window: TCastleWindow; TextList: TStringList;
  const Alignment: THorizontalPosition; const Html: boolean): TInputPressRelease;
var
  View: TViewDialogPressEvent;
begin
  View := TViewDialogPressEvent.Create(Window);
  try
    View.Text.Assign(TextList);
    View.Alignment := Alignment;
    View.Html := Html;
    MessageCore(Window, View);
    Result := View.Answer;
  finally FreeAndNil(View) end;
end;

procedure MessageKeyMouse(Window: TCastleWindow; const S: string;
  out Event: TInputPressRelease; const Alignment: THorizontalPosition;
  const Html: boolean);
begin
  Event := MessageKeyMouse(Window, S, Alignment, Html);
end;

procedure MessageKeyMouse(Window: TCastleWindow; TextList: TStringList;
  out Event: TInputPressRelease; const Alignment: THorizontalPosition;
  const Html: boolean);
begin
  Event := MessageKeyMouse(Window, TextList, Alignment, Html);
end;

{ MessageYesNo --------------------------------------------------------------- }

function MessageYesNo(Window: TCastleWindow; const s: string;
  const Alignment: THorizontalPosition;
  const Html: boolean): boolean; overload;
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    Strings_SetText(TextList, S);
    Result := MessageYesNo(Window, TextList, Alignment, Html);
  finally TextList.free end;
end;

function MessageYesNo(Window: TCastleWindow; const SArray: array of string;
  const Alignment: THorizontalPosition;
  const Html: boolean): boolean; overload;
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    AddStrArrayToStrings(SArray, TextList);
    Result := MessageYesNo(Window, TextList, Alignment, Html);
  finally TextList.Free end;
end;

function MessageYesNo(Window: TCastleWindow; TextList: TStringList;
  const Alignment: THorizontalPosition;
  const Html: boolean): boolean; overload;
var
  View: TViewDialogYesNo;
begin
  View := TViewDialogYesNo.Create(Window);
  try
    View.Text.Assign(TextList);
    View.Alignment := Alignment;
    View.Html := Html;
    MessageCore(Window, View);
    Result := View.Answer;
  finally FreeAndNil(View) end;
end;

{ MessageInputCardinal ------------------------------------------------------- }

function MessageInputCardinal(Window: TCastleWindow; const s: string;
  const AnswerDefault: string; const Alignment: THorizontalPosition;
  const Html: boolean): Cardinal;
begin
  result := StrToInt( MessageInput(Window, s, AnswerDefault,
    1, 0, ['0'..'9'], Alignment, Html) );
end;

function MessageInputCardinal(Window: TCastleWindow; const s: string;
  const AnswerDefault: Cardinal; const Alignment: THorizontalPosition;
  const Html: boolean): Cardinal;
begin
  result := MessageInputCardinal(Window, s, IntToStr(AnswerDefault), Alignment, Html);
end;

function MessageInputQueryCardinal(Window: TCastleWindow;
  const Title: string;
  var Value: Cardinal; const Alignment: THorizontalPosition;
  const Html: boolean): boolean;
var
  ValueStr: string;
begin
  ValueStr := IntToStr(Value);
  Result := MessageInputQuery(Window, Title, ValueStr, 1, 0, ['0'..'9'], Alignment, Html);
  if Result then
    Value := StrToInt(ValueStr);
end;

function MessageInputQueryCardinalHex(Window: TCastleWindow; const Title: string;
  var Value: Cardinal; const MaxWidth: Cardinal;
  const Alignment: THorizontalPosition;
  const Html: boolean): boolean;
var
  ValueStr: string;
begin
  ValueStr := IntToHex(Value, 4);
  Result := MessageInputQuery(Window, Title, ValueStr, 1, MaxWidth,
    ['0'..'9', 'a'..'f', 'A'..'F'], Alignment, Html);
  if Result then
    Value := StrHexToInt(ValueStr);
end;

{ MessageInputQuery on floats ------------------------------------------------ }

function MessageInputQuery(Window: TCastleWindow; const Title: string;
  var Value: Extended; const ValueAsString: string;
  const Alignment: THorizontalPosition;
  const Html: boolean): boolean;
var
  s: string;
begin
  Result := false;
  if ValueAsString <> '' then
    S := ValueAsString
  else
    S := FloatToStrDot(Value);
  if MessageInputQuery(Window, Title, S, 0, 0, AllChars, Alignment, Html) then
  begin
    try
      Value := ParseConstantFloatExpression(s);
      Result := true;
    except
      on E: EConvertError do
        MessageOK(Window, 'Invalid floating point value : ' +E.Message);
    end;
  end;
end;

function MessageInputQuery(Window: TCastleWindow; const Title: string;
  var Value: Single; const ValueAsString: string;
  const Alignment: THorizontalPosition;
  const Html: boolean): boolean;
var
  ValueExtended: Extended;
begin
  ValueExtended := Value;
  Result := MessageInputQuery(Window, Title, ValueExtended, ValueAsString, Alignment, Html);
  if Result then
    Value := ValueExtended;
end;

{$ifndef EXTENDED_EQUALS_DOUBLE}
function MessageInputQuery(Window: TCastleWindow; const Title: string;
  var Value: Double; const ValueAsString: string;
  const Alignment: THorizontalPosition;
  const Html: boolean): boolean;
var
  ValueExtended: Extended;
begin
  ValueExtended := Value;
  Result := MessageInputQuery(Window, Title, ValueExtended, ValueAsString, Alignment, Html);
  if Result then
    Value := ValueExtended;
end;
{$endif not EXTENDED_EQUALS_DOUBLE}

{ MessageInputQueryVector3 --------------------------------------------- }

function MessageInputQueryVector3(
  Window: TCastleWindow; const Title: string;
  var Value: TVector3; const Alignment: THorizontalPosition;
  const Html: boolean): boolean;
var s: string;
begin
  Result := false;
  s := Format('%g %g %g', [Value[0], Value[1], Value[2]]);
  if MessageInputQuery(Window, Title, s, 0, 0, AllChars, Alignment, Html) then
  begin
    try
      Value := Vector3FromStr(s);
      Result := true;
    except
      on E: EConvertError do
        MessageOK(Window, 'Invalid vector 3 value : ' + E.Message);
    end;
  end;
end;

{ MessageInputQueryVector4 --------------------------------------------- }

function MessageInputQueryVector4(
  Window: TCastleWindow; const Title: string;
  var Value: TVector4; const Alignment: THorizontalPosition;
  const Html: boolean): boolean;
var
  s: string;
begin
  Result := false;
  s := Format('%g %g %g %g', [Value[0], Value[1], Value[2], Value[3]]);
  if MessageInputQuery(Window, Title, s, 0, 0, AllChars, Alignment, Html) then
  begin
    try
      Value := Vector4FromStr(s);
      Result := true;
    except
      on E: EConvertError do
        MessageOK(Window, 'Invalid vector 4 value : ' + E.Message);
    end;
  end;
end;

end.
