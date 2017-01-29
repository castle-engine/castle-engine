{
  Copyright 2001-2017 Michalis Kamburelis.

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
      TCastleWindowCustom.Close from TCastleApplication callbacks. )

    @item(Since your normal callbacks
      and controls are not run when message box is running, you usually
      don't need to do anything special about it, unless you use
      TCastleApplication callbacks mentioned above.)
  )
*)

unit CastleMessages;

{ TODO:
  - blinking cursor for MessageInput[Query] woul be useful
}

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
procedure MessageOK(Window: TCastleWindowCustom; const s: string;
  const TextAlign: THorizontalPosition = DefaultAlign;
  const Html: boolean = false); overload;
procedure MessageOK(Window: TCastleWindowCustom;  const SArray: array of string;
  const TextAlign: THorizontalPosition = DefaultAlign;
  const Html: boolean = false); overload;
procedure MessageOK(Window: TCastleWindowCustom;  TextList: TStringList;
  const TextAlign: THorizontalPosition = DefaultAlign;
  const Html: boolean = false); overload;
{ @groupEnd }

{ Ask user to input a string.
  User must give an answer (there is no "Cancel" button),
  use MessageInputQuery if you want a version with "Cancel" button.
  @param AnswerMaxLen 0 (zero) means that there's no maximum answer length.

  @groupBegin }
function MessageInput(Window: TCastleWindowCustom; const s: string;
  const answerDefault: string = '';
  const answerMinLen: integer = 0;
  const answerMaxLen: integer = 0;
  const answerAllowedChars: TSetOfChars = AllChars;
  const TextAlign: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): string; overload;
function MessageInput(Window: TCastleWindowCustom; TextList: TStringList;
  const answerDefault: string = '';
  const answerMinLen: integer = 0;
  const answerMaxLen: integer = 0;
  const answerAllowedChars: TSetOfChars = AllChars;
  const TextAlign: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): string; overload;
{ @groupEnd }

{ Ask user to input a string, or cancel.
  Returns @true and sets Answer if user accepted some text.
  Note that initial Answer value is the answer proposed to the user.
  @param AnswerMaxLen 0 (zero) means that there's no maximum answer length.

  @groupBegin }
function MessageInputQuery(Window: TCastleWindowCustom; const s: string;
  var answer: string;
  const answerMinLen: integer = 0;
  const answerMaxLen: integer = 0;
  const answerAllowedChars: TSetOfChars = AllChars;
  const TextAlign: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): boolean; overload;
function MessageInputQuery(Window: TCastleWindowCustom; TextList: TStringList;
  var answer: string;
  const answerMinLen: integer = 0;
  const answerMaxLen: integer = 0;
  const answerAllowedChars: TSetOfChars = AllChars;
  const TextAlign: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): boolean; overload;
{ @groupEnd }

{ Ask user to choose one option from many.
  Buttons contain a list of button captions.

  ButtonsChars (must have always the same length as Buttons)
  contains the chars that are returned for each corresponding button press.
  The user can also directly press the given key. ButtonsChars is not case
  sensitive, all letters on this list must be different (not only in case).
  We always return a lowercase letter corresponding to one of ButtonsChars letters.

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

  @groupBegin }
function MessageChoice(Window: TCastleWindowCustom; const s: string;
  const Buttons: array of string; const ButtonsChars: array of char;
  const TextAlign: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): char; overload;
function MessageChoice(Window: TCastleWindowCustom; const SArray: array of string;
  const Buttons: array of string; const ButtonsChars: array of char;
  const TextAlign: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): char; overload;
function MessageChoice(Window: TCastleWindowCustom; TextList: TStringList;
  const Buttons: array of string; const ButtonsChars: array of char;
  const TextAlign: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): char; overload;
{ @groupEnd }

{ Ask user to press any key, return this key as Keys.TKey.

  Never returns K_None (which means that keys that cannot be interpreted
  as Keys.TKey will be ignored, and will not close the dialog box).

  @groupBegin }
function MessageKey(Window: TCastleWindowCustom; const S: string;
  const TextAlign: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): TKey; overload;
function MessageKey(Window: TCastleWindowCustom; const SArray: array of string;
  const TextAlign: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): TKey; overload;
function MessageKey(Window: TCastleWindowCustom; TextList: TStringList;
  const TextAlign: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): TKey; overload;
{ @groupEnd }

{ Ask user to press any key or mouse button or mouse wheel, and return it.
  The natural use for this is to allow user to configure
  keybindings of your program, like for TInputShortcut.
  @groupBegin }
procedure MessageKeyMouse(Window: TCastleWindowCustom; const S: string;
  out Event: TInputPressRelease;
  const TextAlign: THorizontalPosition = DefaultAlign;
  const Html: boolean = false); overload;
procedure MessageKeyMouse(Window: TCastleWindowCustom; TextList: TStringList;
  out Event: TInputPressRelease;
  const TextAlign: THorizontalPosition = DefaultAlign;
  const Html: boolean = false); overload;
{ @groupEnd }

function MessageYesNo(Window: TCastleWindowCustom; const s: string;
  const TextAlign: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): boolean; overload;
function MessageYesNo(Window: TCastleWindowCustom;  const SArray: array of string;
  const TextAlign: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): boolean; overload;
function MessageYesNo(Window: TCastleWindowCustom;  TextList: TStringList;
  const TextAlign: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): boolean; overload;

{ Ask user to input an unsigned integer.

  Note that AnswerDefault below may be given as Cardinal or as a string.
  The latter is useful if you want the default answer to be '', i.e. empty string
  --- no default answer.

  @groupBegin }
function MessageInputCardinal(Window: TCastleWindowCustom; const s: string;
  const AnswerDefault: string;
  const TextAlign: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): Cardinal; overload;
function MessageInputCardinal(Window: TCastleWindowCustom; const s: string;
  const AnswerDefault: Cardinal;
  const TextAlign: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): Cardinal; overload;
function MessageInputQueryCardinal(Window: TCastleWindowCustom; const Title: string;
  var Value: Cardinal;
  const TextAlign: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): boolean;
{ @groupEnd }

{ Ask user to input a value in hexadecimal.
  Give MaxWidth = 0 to say that there is no maximum width. }
function MessageInputQueryCardinalHex(Window: TCastleWindowCustom; const Title: string;
  var Value: Cardinal; const MaxWidth: Cardinal;
  const TextAlign: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): boolean;

{ Ask user to input a floating-point number.

  If you give non-empty ValueAsString, it will be used to show
  the initial value for the user. Otherwise, we will just show
  FloatToStr(Value), which sometimes may be too ugly.
  For example Value = 0.01 cannot be precisely represented as a floating point
  number, and FloatToStr shows that this is really something like 0.0099xxxxx.

  @groupBegin }
function MessageInputQuery(Window: TCastleWindowCustom; const Title: string;
  var Value: Extended;
  const ValueAsString: string = '';
  const TextAlign: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): boolean;
function MessageInputQuery(Window: TCastleWindowCustom; const Title: string;
  var Value: Single;
  const ValueAsString: string = '';
  const TextAlign: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): boolean;
{$ifndef EXTENDED_EQUALS_DOUBLE}
function MessageInputQuery(Window: TCastleWindowCustom; const Title: string;
  var Value: Double;
  const ValueAsString: string = '';
  const TextAlign: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): boolean;
{$endif not EXTENDED_EQUALS_DOUBLE}
{ @groupEnd }

function MessageInputQueryVector3Single(
  Window: TCastleWindowCustom; const Title: string;
  var Value: TVector3Single;
  const TextAlign: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): boolean;

function MessageInputQueryVector4Single(
  Window: TCastleWindowCustom; const Title: string;
  var Value: TVector4Single;
  const TextAlign: THorizontalPosition = DefaultAlign;
  const Html: boolean = false): boolean;

implementation

uses CastleImages, CastleClassUtils, SysUtils, CastleWindowModes, CastleLog,
  CastleUIControls;

const
  MinButtonWidth = 100; //< OK button looks too small without this

{ MessageCore ---------------------------------------------------------------- }

{ Show a modal dialod window. Uses TGLMode to temporarily replace
  normal event processing (on this Window) with it's own,
  to wait until the dialog is answered by the user.

  Given TextList is never modified here. }
procedure MessageCore(
  const Window: TCastleWindowCustom; const TextList: TStringList;
  const TextAlign: THorizontalPosition; const Html: boolean;
  const Dialog: TCastleDialog;
  const AButtons: array of TCastleButton;
  const ADrawInputText: boolean; var AInputText: string);
var
  SavedMode: TGLMode;
  Background: TCastleImage;
  ErrorBackground: TErrorBackground;
begin
  // if Log then
  //   WritelnLogMultiline('Message', TextList.Text);

  if Theme.MessageErrorBackground then
  begin
    Background := nil;
    ErrorBackground := TErrorBackground.Create(nil);
  end else
  begin
    Background := Window.SaveScreen;
    ErrorBackground := nil;
  end;

  { Among other things, using @NoClose below allows users to safely use
    MessageXxx inside own OnCloseQuery, like
      if MessageYesNo('Are you sure ?') then Window.Close; }
  SavedMode := TGLMode.CreateReset(Window, nil, nil, @NoClose);
  try
    Dialog.Initialize(TextList, TextAlign, Html, AButtons, ADrawInputText,
      AInputText, Background);

    Window.Controls.InsertBack(Dialog);
    if Theme.MessageErrorBackground then
      Window.Controls.InsertBack(ErrorBackground);
    Window.Invalidate;
    { WaitForMessage = false is necessary, otherwise SecondsPassed
      for update would be large. }
    repeat Application.ProcessMessage(false, true) until Dialog.Answered;

  finally
    FreeAndNil(SavedMode);
    FreeAndNil(ErrorBackground);
    AInputText := Dialog.InputText;
  end;
end;

procedure MessageCore(
  const Window: TCastleWindowCustom; const TextList: TStringList;
  const TextAlign: THorizontalPosition; const Html: boolean;
  const Dialog: TCastleDialog;
  const AButtons: array of TCastleButton);
var
  Dummy: string;
begin
  Dummy := '';
  MessageCore(Window, TextList, TextAlign, Html, Dialog, AButtons, false, Dummy);
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

{ Ask user to press a single character from a given set, or to press one
  of the buttons. This is good to ask user a question with a small number
  of answers.
  This is like a lower-level version of MessageChoice, not in the interface now.

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
}
function MessageChar(Window: TCastleWindowCustom; TextList: TStringList;
  const AllowedChars: TSetOfChars;
  const Buttons: array of string; const ButtonsChars: array of char;
  const TextAlign: THorizontalPosition; const Html, IgnoreCase: boolean): char; overload;
var
  Dialog: TCastleCharDialog;
  Button: TCharDialogButton;
  ReadyButtons: array of TCastleButton;
  I: Integer;
begin
  Assert(Length(Buttons) = Length(ButtonsChars),
    'Buttons and ButtonsChars arrays must have equal length');

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
      Button.MinWidth := MinButtonWidth;
      Button.Answer := ButtonsChars[I];
      ReadyButtons[I] := Button;
    end;
    MessageCore(Window, TextList, TextAlign, Html, Dialog, ReadyButtons);
    Result := Dialog.Answer;
  finally FreeAndNil(Dialog) end;
end;

{ MessageOK ------------------------------------------------------------------ }

procedure MessageOK(Window: TCastleWindowCustom; const SArray: array of string;
  const TextAlign: THorizontalPosition;
  const Html: boolean = false);
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    AddStrArrayToStrings(SArray, TextList);
    MessageOK(Window, TextList, TextAlign, Html);
  finally TextList.Free end;
end;

procedure MessageOK(Window: TCastleWindowCustom; const s: string;
  const TextAlign: THorizontalPosition;
  const Html: boolean = false);
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    Strings_SetText(TextList, s);
    MessageOK(Window, TextList, TextAlign, Html);
  finally TextList.free end;
end;

procedure MessageOK(Window: TCastleWindowCustom; TextList: TStringList;
  const TextAlign: THorizontalPosition;
  const Html: boolean = false);
begin
  MessageChar(Window, TextList,
    [CharEnter, CharEscape], ['OK'], [CharEnter], TextAlign, Html, true);
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
  Dialog.Press(InputKey(Container.MousePosition, K_Enter, CharEnter));
end;

procedure TInputDialogCancelButton.DoClick;
begin
  inherited;
  Dialog.Press(InputKey(Container.MousePosition, K_Escape, CharEscape));
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
        [answerMinLen]), DefaultAlign); }
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
  const answerDefault: string;
  const answerMinLen: integer; const answerMaxLen: integer;
  const answerAllowedChars: TSetOfChars;
  const TextAlign: THorizontalPosition;
  const Html: boolean): string;
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    Strings_SetText(TextList, s);
    result := MessageInput(Window, TextList, answerDefault,
      answerMinLen, answerMaxLen, answerAllowedChars, TextAlign, Html);
  finally TextList.free end;
end;

function MessageInput(Window: TCastleWindowCustom; TextList: TStringList;
  const answerDefault: string;
  const answerMinLen: integer; const answerMaxLen: integer;
  const answerAllowedChars: TSetOfChars;
  const TextAlign: THorizontalPosition;
  const Html: boolean): string;
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
    OKButton.MinWidth := MinButtonWidth;
    SetLength(ReadyButtons, 1);
    ReadyButtons[0] := OKButton;

    MessageCore(Window, TextList, TextAlign, Html, Dialog, ReadyButtons, true, result);
  finally FreeAndNil(Dialog) end;
end;

function MessageInputQuery(Window: TCastleWindowCustom; const s: string;
  var answer: string;
  const answerMinLen: integer; const answerMaxLen: integer;
  const answerAllowedChars: TSetOfChars;
  const TextAlign: THorizontalPosition;
  const Html: boolean): boolean;
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    Strings_SetText(TextList, s);
    result := MessageInputQuery(Window, TextList, answer, answerMinLen,
      answerMaxLen, answerAllowedChars, TextAlign, Html);
  finally TextList.free end;
end;

function MessageInputQuery(Window: TCastleWindowCustom; TextList: TStringList;
  var answer: string;
  const answerMinLen: integer; const answerMaxLen: integer;
  const answerAllowedChars: TSetOfChars;
  const TextAlign: THorizontalPosition;
  const Html: boolean): boolean;
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
    OKButton.MinWidth := MinButtonWidth;
    CancelButton := TInputDialogCancelButton.Create(Dialog);
    CancelButton.Dialog := Dialog;
    CancelButton.Caption := 'Cancel';
    CancelButton.MinWidth := MinButtonWidth;
    SetLength(ReadyButtons, 2);
    ReadyButtons[0] := CancelButton;
    ReadyButtons[1] := OKButton;

    MessageCore(Window, TextList, TextAlign, Html, Dialog, ReadyButtons, true, InputText);
    result := not Dialog.answerCancelled;
    if result then answer := InputText;
  finally FreeAndNil(Dialog) end;
end;

{ MessageChoice -------------------------------------------------------------- }

function MessageChoice(Window: TCastleWindowCustom;
  const s: string;
  const Buttons: array of string; const ButtonsChars: array of char;
  const TextAlign: THorizontalPosition;
  const Html: boolean): char;
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    Strings_SetText(TextList, s);
    Result := MessageChoice(Window, TextList, Buttons, ButtonsChars, TextAlign, Html);
  finally TextList.free end;
end;

function MessageChoice(Window: TCastleWindowCustom;
  const SArray: array of string;
  const Buttons: array of string; const ButtonsChars: array of char;
  const TextAlign: THorizontalPosition;
  const Html: boolean): char; overload;
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    AddStrArrayToStrings(SArray, TextList);
    Result := MessageChoice(Window, TextList, Buttons, ButtonsChars, TextAlign, Html);
  finally TextList.Free end;
end;

function MessageChoice(Window: TCastleWindowCustom; TextList: TStringList;
  const Buttons: array of string; const ButtonsChars: array of char;
  const TextAlign: THorizontalPosition;
  const Html: boolean): char; overload;
var
  AllowedChars: TSetOfChars;
  C: char;
begin
  AllowedChars := [];
  for C in ButtonsChars do
    Include(AllowedChars, C);
  Result := MessageChar(Window, TextList, AllowedChars, Buttons, ButtonsChars,
    TextAlign, Html, true);
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
  const TextAlign: THorizontalPosition;
  const Html: boolean): TKey;
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    Strings_SetText(TextList, S);
    Result := MessageKey(Window, TextList, TextAlign, Html);
  finally TextList.free end;
end;

function MessageKey(Window: TCastleWindowCustom; const SArray: array of string;
  const TextAlign: THorizontalPosition;
  const Html: boolean): TKey;
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    AddStrArrayToStrings(SArray, TextList);
    Result := MessageKey(Window, TextList, TextAlign, Html);
  finally TextList.Free end;
end;

function MessageKey(Window: TCastleWindowCustom; TextList: TStringList;
  const TextAlign: THorizontalPosition;
  const Html: boolean): TKey;
var
  Dialog: TCastleKeyDialog;
begin
  Dialog := TCastleKeyDialog.Create(nil);
  try
    MessageCore(Window, TextList, TextAlign, Html, Dialog, []);
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
  Answered := true;
  Answer := Event;
end;

procedure MessageKeyMouse(Window: TCastleWindowCustom; const S: string;
  out Event: TInputPressRelease; const TextAlign: THorizontalPosition;
  const Html: boolean);
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    Strings_SetText(TextList, S);
    MessageKeyMouse(Window, TextList, Event, TextAlign, Html);
  finally TextList.Free end;
end;

procedure MessageKeyMouse(Window: TCastleWindowCustom; TextList: TStringList;
  out Event: TInputPressRelease; const TextAlign: THorizontalPosition;
  const Html: boolean);
var
  Dialog: TCastleKeyMouseDialog;
begin
  Dialog := TCastleKeyMouseDialog.Create(nil);
  try
    MessageCore(Window, TextList, TextAlign, Html, Dialog, []);
    Event := Dialog.Answer;
  finally FreeAndNil(Dialog) end;
end;

{ MessageYesNo --------------------------------------------------------------- }

function MessageYesNo(Window: TCastleWindowCustom; const s: string;
  const TextAlign: THorizontalPosition;
  const Html: boolean): boolean; overload;
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    Strings_SetText(TextList, S);
    Result := MessageYesNo(Window, TextList, TextAlign, Html);
  finally TextList.free end;
end;

function MessageYesNo(Window: TCastleWindowCustom; const SArray: array of string;
  const TextAlign: THorizontalPosition;
  const Html: boolean): boolean; overload;
var
  TextList: TStringList;
begin
  TextList := TStringList.Create;
  try
    AddStrArrayToStrings(SArray, TextList);
    Result := MessageYesNo(Window, TextList, TextAlign, Html);
  finally TextList.Free end;
end;

function MessageYesNo(Window: TCastleWindowCustom; TextList: TStringList;
  const TextAlign: THorizontalPosition;
  const Html: boolean): boolean; overload;
begin
  Result := MessageChar(Window, TextList, ['n', 'y', CharEnter, CharEscape],
    ['No', 'Yes'], ['n', 'y'], TextAlign, Html, true) in ['y', CharEnter];
end;

{ MessageInputCardinal ------------------------------------------------------- }

function MessageInputCardinal(Window: TCastleWindowCustom; const s: string;
  const AnswerDefault: string; const TextAlign: THorizontalPosition;
  const Html: boolean): Cardinal;
begin
  result := StrToInt( MessageInput(Window, s, AnswerDefault,
    1, 0, ['0'..'9'], TextAlign, Html) );
end;

function MessageInputCardinal(Window: TCastleWindowCustom; const s: string;
  const AnswerDefault: Cardinal; const TextAlign: THorizontalPosition;
  const Html: boolean): Cardinal;
begin
  result := MessageInputCardinal(Window, s, IntToStr(AnswerDefault), TextAlign, Html);
end;

function MessageInputQueryCardinal(Window: TCastleWindowCustom;
  const Title: string;
  var Value: Cardinal; const TextAlign: THorizontalPosition;
  const Html: boolean): boolean;
var
  ValueStr: string;
begin
  ValueStr := IntToStr(Value);
  Result := MessageInputQuery(Window, Title, ValueStr, 1, 0, ['0'..'9'], TextAlign, Html);
  if Result then
    Value := StrToInt(ValueStr);
end;

function MessageInputQueryCardinalHex(Window: TCastleWindowCustom; const Title: string;
  var Value: Cardinal; const MaxWidth: Cardinal;
  const TextAlign: THorizontalPosition;
  const Html: boolean): boolean;
var
  ValueStr: string;
begin
  ValueStr := IntToHex(Value, 4);
  Result := MessageInputQuery(Window, Title, ValueStr, 1, MaxWidth,
    ['0'..'9', 'a'..'f', 'A'..'F'], TextAlign, Html);
  if Result then
    Value := StrHexToInt(ValueStr);
end;

{ MessageInputQuery on floats ------------------------------------------------ }

function MessageInputQuery(Window: TCastleWindowCustom; const Title: string;
  var Value: Extended; const ValueAsString: string;
  const TextAlign: THorizontalPosition;
  const Html: boolean): boolean;
var
  s: string;
begin
  Result := false;
  if ValueAsString <> '' then
    S := ValueAsString else
    S := FloatToStr(Value);
  if MessageInputQuery(Window, Title, S, 0, 0, AllChars, TextAlign, Html) then
  begin
    try
      Value := StrToFloat(s);
      Result := true;
    except
      on E: EConvertError do
        MessageOK(Window, 'Invalid floating point value : ' +E.Message);
    end;
  end;
end;

function MessageInputQuery(Window: TCastleWindowCustom; const Title: string;
  var Value: Single; const ValueAsString: string;
  const TextAlign: THorizontalPosition;
  const Html: boolean): boolean;
var
  ValueExtended: Extended;
begin
  ValueExtended := Value;
  Result := MessageInputQuery(Window, Title, ValueExtended, ValueAsString, TextAlign, Html);
  if Result then
    Value := ValueExtended;
end;

{$ifndef EXTENDED_EQUALS_DOUBLE}
function MessageInputQuery(Window: TCastleWindowCustom; const Title: string;
  var Value: Double; const ValueAsString: string;
  const TextAlign: THorizontalPosition;
  const Html: boolean): boolean;
var
  ValueExtended: Extended;
begin
  ValueExtended := Value;
  Result := MessageInputQuery(Window, Title, ValueExtended, ValueAsString, TextAlign, Html);
  if Result then
    Value := ValueExtended;
end;
{$endif not EXTENDED_EQUALS_DOUBLE}

{ MessageInputQueryVector3Single --------------------------------------------- }

function MessageInputQueryVector3Single(
  Window: TCastleWindowCustom; const Title: string;
  var Value: TVector3Single; const TextAlign: THorizontalPosition;
  const Html: boolean): boolean;
var s: string;
begin
  Result := false;
  s := Format('%g %g %g', [Value[0], Value[1], Value[2]]);
  if MessageInputQuery(Window, Title, s, 0, 0, AllChars, TextAlign, Html) then
  begin
    try
      Value := Vector3SingleFromStr(s);
      Result := true;
    except
      on E: EConvertError do
        MessageOK(Window, 'Invalid vector 3 value : ' + E.Message);
    end;
  end;
end;

{ MessageInputQueryVector4Single --------------------------------------------- }

function MessageInputQueryVector4Single(
  Window: TCastleWindowCustom; const Title: string;
  var Value: TVector4Single; const TextAlign: THorizontalPosition;
  const Html: boolean): boolean;
var
  s: string;
begin
  Result := false;
  s := Format('%g %g %g %g', [Value[0], Value[1], Value[2], Value[3]]);
  if MessageInputQuery(Window, Title, s, 0, 0, AllChars, TextAlign, Html) then
  begin
    try
      Value := Vector4SingleFromStr(s);
      Result := true;
    except
      on E: EConvertError do
        MessageOK(Window, 'Invalid vector 4 value : ' + E.Message);
    end;
  end;
end;

end.
