{
  Copyright 2001-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

(*Dialog windows (to display some information, or ask user for confirmation,
  or ask user to input a simple value) as a user-interface state (@link(TUIState)).
  This unit defines the user-interface state classes
  (@link(TUIState) descendants) to display given dialog.

  If you use @link(TCastleWindow) with most platforms (but not iOS),
  then it's usually more comfortable to use the unit @link(CastleMessages)
  instead of this unit.
  Using the @link(CastleMessages), you get comfortable functions
  like @link(MessageOK) and @link(MessageYesNo) that
  wait until user presses a button (like "OK", "Yes", "No") to close the dialog.
  This is comfortable to use, as you can write code like this:

  @longCode(#
  if MessageYesNo(Window, 'Are you sure you want to delete this file?') then
    DeleteFile(...);
  #)

  Underneath, the @link(MessageYesNo) will use a @link(TStateDialogYesNo),
  making sure that your normal window callbacks are redirected as appropriate.
  But you don't need to be concerned with this.

  However, if you need to work on iOS, then you cannot use most of routines
  from the @link(CastleMessages) unit. You can use @link(MessageOK) if you turn on
  the @link(MessageOKPushesState) flag, but nothing else. E.g. @link(MessageYesNo)
  cannot work on iOS now. Making modal boxes like that is not supported on iOS.

  In this case you should use this unit and instantiate the
  user-interface classes yourself, and you need to
  @italic(organize your whole game using TUIState classes).
  See https://castle-engine.io/manual_2d_user_interface.php#section_ui_state
  about how to use @link(TUIState). Like this:

  @longCode(#
  type
    TMyGameState = class(TUIState)
    private
      DialogAskDeleteFile: TStateDialogYesNo;
    public
      function Press(const Event: TInputPressRelease): boolean; override;
      procedure Resume; override;
    end;

  function Press(const Event: TInputPressRelease): boolean; override;
  begin
    Result := inherited;
    if Result then Exit;

    if Event.IsKey(K_Enter) then
    begin
      DialogAskDeleteFile := TStateDialogYesNo.Create(Self);
      DialogAskDeleteFile.Caption := 'Are you sure you want to delete this file?';
      TUIState.Push(DialogAskDeleteFile);
    end;
  end;

  procedure TStatePlay.Resume;
  begin
    inherited;
    if DialogAskDeleteFile <> nil then // returning from DialogAskDeleteFile
    begin
      if DialogAskDeleteFile.Answer then
        DeleteFile(...);
      FreeAndNil(DialogAskDeleteFile);
    end;
  end;
  #)
*)

unit CastleDialogStates;

{$I castleconf.inc}

interface

uses Classes, Math,
  CastleGLUtils, CastleUtils,
  CastleStringUtils, CastleVectors, CastleKeysMouse, CastleControls,
  CastleRectangles, CastleUIState, CastleColors, CastleUIControls,
  CastleFonts, CastleFontFamily, CastleTimeUtils;

type
  { Abstract class for a modal dialog user-interface state.
    See unit @link(CastleDialogStates) documentation for example usage. }
  TStateDialog = class abstract(TUIState)
  strict private
    type
      {$define read_interface}
      {$I castledialogstates_dialog.inc}
      {$undef read_interface}
    var
      FText: TStrings;
      FHtml: boolean;
      FAlignment: THorizontalPosition;
      FAnswered: boolean;
      FBackground: boolean;
      FBackgroundColor: TCastleColor;
      FBackgroundScreenshot: boolean;
      FPopOnAnswered: boolean;
      FDialog: TDialog; // non-nil only between Start and Stop
      FOverrrideContainer: TUIContainer;
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    function GetInputText: string;
    procedure SetInputText(const Value: string);
  protected
    type
      TButtonArray = array of TCastleButton;
    function StateContainer: TUIContainer; override;
    procedure InitializeButtons(var Buttons: TButtonArray); virtual;
    function DrawInputText: boolean; virtual;
    procedure DoAnswered;
    property InputText: string read GetInputText write SetInputText;
  public
    const
      DefaultAlignment = hpLeft;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
    property InterceptInput default true;

    { When user answers the dialog, this is set to @true.
      The state also normally does TUIState.Pop, so there's no need to check
      this property, unless you set @link(PopOnAnswered) to @false. }
    property Answered: boolean read FAnswered;

    { Caption displayed in the dialog. }
    property Text: TStrings read FText;

    { Caption displayed in the dialog, as a simple string.
      This is just a shortcut to get/set @link(Text) as a single string.

      Use LineEnding or NL constant when setting this to indicate a newline.
      The two examples below are equivalent:

      @longCode(#
        // one way
        StateDialogOK.Text.Clear;
        StateDialogOK.Text.Add('First line');
        StateDialogOK.Text.Add('Second line');

        // alternative way to do the same
        StateDialogOK.Caption := 'First line' + LineEnding + 'Second line';
      #)
    }
    property Caption: string read GetCaption write SetCaption stored false;

    { Horizontal alignment of the text. }
    property Alignment: THorizontalPosition
      read FAlignment write FAlignment default DefaultAlignment;

    { Enable a subset of HTML to mark font changes inside the text.
      See the TCastleFont.PrintStrings for a description of supported
      HTML constructs. }
    property Html: boolean read FHtml write FHtml default false;

    { Obscure the state underneath with our own background (using a color or screenshot).

      The obscuring background is defined by @link(BackgroundColor).
      It may be just a solid opaque color.
      Or it may be a partially-transparent or even completely transparent
      color, showing the state underneath (or showing the screenshot
      of the state underneath, if @link(BackgroundScreenshot)). }
    property Background: boolean read FBackground write FBackground default false;

    { Color of the background obscuring the state underneath,
      if @link(Background) is @true.
      This color may be partially-transparent (e.g. to visually "dim" the state
      underneath) or even completely transparent (alpha 0).

      Default is Theme.BackgroundColor,
      which is dark with alpha = 0.5, so it will dim the state underneath.
      Sometimes (under exception handler) it's Theme.BackgroundOpaqueColor. }
    property BackgroundColor: TCastleColor read FBackgroundColor write FBackgroundColor;

    { Initialize the background by taking a screenshot of the current screen
      when the state was started.
      This screenshot is shown, instead of actually rendering the state underneath,
      under the @link(BackgroundColor), when @link(Background) is @true.

      This is less functional (when the user scales the window,
      the screenshot is stretched too), but is safer:
      it means that the state underneath does not need to be "renderable" anymore. }
    property BackgroundScreenshot: boolean
      read FBackgroundScreenshot write FBackgroundScreenshot default false;

    { Should the state do @link(TUIState.Pop) when answered.
      This is usually most natural. }
    property PopOnAnswered: boolean
      read FPopOnAnswered write FPopOnAnswered default true;

    { Force state to use indicated TUIContainer to insert itself and get screenshot.
      By default it uses
      @link(TCastleApplication.MainWindow Application.MainWindow)
      if you use CastleWindow or
      @link(TCastleControl.MainControl) if you use CastleControl. }
    property OverrrideContainer: TUIContainer
      read FOverrrideContainer write FOverrrideContainer;
  end;

  { Wait for simple confirmation ("OK") from user.
    See unit @link(CastleDialogStates) documentation for example usage. }
  TStateDialogOK = class(TStateDialog)
  strict private
    procedure ButtonOKClick(Sender: TObject);
  protected
    procedure InitializeButtons(var Buttons: TButtonArray); override;
  public
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

  { Ask user a simple "yes" / "no" question.
    See unit @link(CastleDialogStates) documentation for example usage. }
  TStateDialogYesNo = class(TStateDialog)
  strict private
    FAnswer: boolean;
    procedure ButtonYesClick(Sender: TObject);
    procedure ButtonNoClick(Sender: TObject);
  protected
    procedure InitializeButtons(var Buttons: TButtonArray); override;
  public
    function Press(const Event: TInputPressRelease): boolean; override;
    { User answer to the dialog question, defined when @link(Answered). }
    property Answer: boolean read FAnswer;
  end;

  { Ask user to choose from a number of options.
    Each choice is shown as a button, user can also press the appropriate character.
    ButtonChars length must be always equal to ButtonCaptions.
    See unit @link(CastleDialogStates) documentation for example usage. }
  TStateDialogChoice = class(TStateDialog)
  strict private
    FAnswer: char;
    procedure ButtonClick(Sender: TObject);
  protected
    procedure InitializeButtons(var Buttons: TButtonArray); override;
  public
    ButtonCaptions: array of string;
    ButtonChars: array of char;
    function Press(const Event: TInputPressRelease): boolean; override;
    { User answer to the dialog question, defined when @link(Answered).
      This is one of the ButtonChars. }
    property Answer: char read FAnswer;
  end;

  { Ask user to input a string, or cancel.
    See unit @link(CastleDialogStates) documentation for example usage. }
  TStateDialogInput = class(TStateDialog)
  strict private
    FAllowedChars: TSetOfChars;
    FMinLength, FMaxLength: Cardinal;
    FCanCancel: boolean;
    FAnswerCancelled: boolean;
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    function GetAnswer: string;
    procedure SetAnswer(const Value: string);
  protected
    procedure InitializeButtons(var Buttons: TButtonArray); override;
    function DrawInputText: boolean; override;
  public
    function Press(const Event: TInputPressRelease): boolean; override;

    { Allowed characters that user can input. }
    property AllowedChars: TSetOfChars read FAllowedChars write FAllowedChars;

    { Min length of user input. }
    property MinLength: Cardinal read FMinLength write FMinLength default 0;

    { Max length of user input.
      Value of 0 (default) means "no limit". }
    property MaxLength: Cardinal read FMaxLength write FMaxLength default 0;

    { User can cancel the input by pressing a button like "cancel". }
    property CanCancel: boolean read FCanCancel write FCanCancel default false;

    { User clicked "cancel" instead of accepting an answer.
      This is defined only when @link(Answered).
      This is possible only if @link(CanCancel). }
    property AnswerCancelled: boolean read FAnswerCancelled;

    { The user input. May be set before starting the state.
      After the state stopped, if @link(Answered), then this contains user answer.
      You should ignore it if @link(AnswerCancelled). }
    property Answer: string read GetAnswer write SetAnswer;
  end;

  { Ask user a press any key, and return this key.
    See unit @link(CastleDialogStates) documentation for example usage. }
  TStateDialogKey = class(TStateDialog)
  strict private
    FAnswer: TKey;
  public
    function Press(const Event: TInputPressRelease): boolean; override;
    { Key pressed by user, defined when @link(Answered). }
    property Answer: TKey read FAnswer;
  end;

  { Ask user a press anything (key, mouse button, mouse wheel),
    for example to configure a keybinding for a game.
    See unit @link(CastleDialogStates) documentation for example usage. }
  TStateDialogPressEvent = class(TStateDialog)
  strict private
    FAnswer: TInputPressRelease;
  public
    function Press(const Event: TInputPressRelease): boolean; override;
    { Key pressed by user, defined when @link(Answered). }
    property Answer: TInputPressRelease read FAnswer;
  end;

implementation

uses SysUtils,
  CastleImages;

{$define read_implementation}
{$I castledialogstates_dialog.inc}

{ TStateDialog ------------------------------------------------------------- }

constructor TStateDialog.Create(AOwner: TComponent);
begin
  inherited;
  FText := TStringList.Create;
  FAlignment := DefaultAlignment;
  FBackground := true;
  if Theme.InternalForceOpaqueBackground then
    FBackgroundColor := Vector4(Theme.BackgroundOpaqueColor, 1)
  else
    FBackgroundColor := Theme.BackgroundColor;
  FPopOnAnswered := true;
  FDialog := TDialog.Create(Self);
end;

destructor TStateDialog.Destroy;
begin
  FreeAndNil(FText);
  inherited;
end;

function TStateDialog.GetCaption: string;
begin
  Result := Text.Text;
end;

procedure TStateDialog.SetCaption(const Value: string);
begin
  Text.Text := Value;
end;

function TStateDialog.GetInputText: string;
begin
  Result := FDialog.InputText;
end;

procedure TStateDialog.SetInputText(const Value: string);
begin
  FDialog.InputText := Value;
end;

procedure TStateDialog.Start;
const
  MinButtonWidth = 100; //< OK button looks too small without this
var
  Buttons: TButtonArray;
  BackgroundImage: TCastleImageControl;
  BackgroundRect: TCastleRectangleControl;
  I: Integer;
begin
  inherited;

  if Background then
  begin
    { Avoid making screenshot when BackgroundColor[3] = 1.
      This means that Theme.InternalForceOpaqueBackground prevents from taking a screenshot. }
    if BackgroundScreenshot and (BackgroundColor[3] <> 1) then
    begin
      BackgroundImage := TCastleImageControl.Create(FreeAtStop);
      BackgroundImage.Stretch := true;
      BackgroundImage.FullSize := true;
      BackgroundImage.Image := StateContainer.SaveScreen;
      InsertFront(BackgroundImage);
    end;

    BackgroundRect := TCastleRectangleControl.Create(FreeAtStop);
    BackgroundRect.Color := BackgroundColor;
    BackgroundRect.FullSize := true;
    InsertFront(BackgroundRect);
  end;

  SetLength(Buttons, 0);
  InitializeButtons(Buttons);
  for I := Low(Buttons) to High(Buttons) do
    Buttons[I].MinWidth := MinButtonWidth;

  FDialog.Initialize(Text, Alignment, Html, Buttons, DrawInputText);
  InsertFront(FDialog);

  InterceptInput := true;
end;

procedure TStateDialog.Stop;
begin
  // remove FDialog, to clearly reinsert it at next Start call
  RemoveControl(FDialog);
  inherited;
end;

function TStateDialog.StateContainer: TUIContainer;
begin
  if OverrrideContainer <> nil then
    Result := OverrrideContainer
  else
    Result := inherited;
end;

procedure TStateDialog.InitializeButtons(var Buttons: TButtonArray);
begin
end;

function TStateDialog.DrawInputText: boolean;
begin
  Result := false;
end;

procedure TStateDialog.DoAnswered;
begin
  FAnswered := true;
  if PopOnAnswered then
    TUIState.Pop(Self);
end;

{ TStateDialogOK ------------------------------------------------------------- }

procedure TStateDialogOK.InitializeButtons(var Buttons: TButtonArray);
begin
  SetLength(Buttons, 1);
  Buttons[0] := TCastleButton.Create(Self);
  Buttons[0].OnClick := @ButtonOKClick;
  Buttons[0].Caption := 'OK';
end;

procedure TStateDialogOK.ButtonOKClick(Sender: TObject);
begin
  DoAnswered;
end;

function TStateDialogOK.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  // if Result then Exit; // ignore inherited Result, always true when InterceptInput

  if Event.IsKey(CharEnter) or Event.IsKey(CharEscape) then
  begin
    DoAnswered;
    Result := true;
  end;
end;

{ TStateDialogYesNo ---------------------------------------------------------- }

procedure TStateDialogYesNo.InitializeButtons(var Buttons: TButtonArray);
begin
  SetLength(Buttons, 2);

  Buttons[0] := TCastleButton.Create(Self);
  Buttons[0].OnClick := @ButtonNoClick;
  Buttons[0].Caption := 'No';

  Buttons[1] := TCastleButton.Create(Self);
  Buttons[1].OnClick := @ButtonYesClick;
  Buttons[1].Caption := 'Yes';
end;

procedure TStateDialogYesNo.ButtonYesClick(Sender: TObject);
begin
  FAnswer := true;
  DoAnswered;
end;

procedure TStateDialogYesNo.ButtonNoClick(Sender: TObject);
begin
  FAnswer := false;
  DoAnswered;
end;

function TStateDialogYesNo.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  // if Result then Exit; // ignore inherited Result, always true when InterceptInput

  if Event.IsKey(K_Y) or Event.IsKey(K_Enter) then
  begin
    FAnswer := true;
    DoAnswered;
    Result := true; // set Result, in case developer changed our InterceptInput to false
  end;

  if Event.IsKey(K_N) or Event.IsKey(K_Escape) then
  begin
    FAnswer := false;
    DoAnswered;
    Result := true; // set Result, in case developer changed our InterceptInput to false
  end;
end;

{ TStateDialogChoice ---------------------------------------------------------- }

procedure TStateDialogChoice.InitializeButtons(var Buttons: TButtonArray);
var
  I: Integer;
begin
  Check(Length(ButtonCaptions) = Length(ButtonChars), 'In TStateDialogChoice, ButtonCaptions must have the same length as ButtonChars');
  SetLength(Buttons, Length(ButtonCaptions));

  for I := 0 to High(ButtonCaptions) do
  begin
    Buttons[I] := TCastleButton.Create(Self);
    Buttons[I].OnClick := @ButtonClick;
    Buttons[I].Caption := ButtonCaptions[I];
    Buttons[I].Tag := Ord(ButtonChars[I]);
  end;
end;

procedure TStateDialogChoice.ButtonClick(Sender: TObject);
begin
  FAnswer := Chr((Sender as TCastleButton).Tag);
  DoAnswered;
end;

function TStateDialogChoice.Press(const Event: TInputPressRelease): boolean;
var
  C: char;
begin
  Result := inherited;
  // if Result then Exit; // ignore inherited Result, always true when InterceptInput

  for C in ButtonChars do
    if Event.IsKey(LoCase(C)) or Event.IsKey(UpCase(C)) then
    begin
      FAnswer := C;
      DoAnswered;
      Result := true;
      Break;
    end;
end;

{ TStateDialogInput ---------------------------------------------------------- }

procedure TStateDialogInput.InitializeButtons(var Buttons: TButtonArray);
begin
  if CanCancel then
  begin
    SetLength(Buttons, 2);

    Buttons[0] := TCastleButton.Create(Self);
    Buttons[0].OnClick := @ButtonCancelClick;
    Buttons[0].Caption := 'Cancel';

    Buttons[1] := TCastleButton.Create(Self);
    Buttons[1].OnClick := @ButtonOKClick;
    Buttons[1].Caption := 'OK';
  end else
  begin
    SetLength(Buttons, 1);
    Buttons[0] := TCastleButton.Create(Self);
    Buttons[0].OnClick := @ButtonOKClick;
    Buttons[0].Caption := 'OK';
  end;
end;

procedure TStateDialogInput.ButtonOKClick(Sender: TObject);
begin
  Press(InputKey(Container.MousePosition, K_Enter, CharEnter));
end;

procedure TStateDialogInput.ButtonCancelClick(Sender: TObject);
begin
  Press(InputKey(Container.MousePosition, K_Escape, CharEscape));
end;

function TStateDialogInput.Press(const Event: TInputPressRelease): boolean;
{ TODO: copy-paste of TCastleEdit.Press here.
  We should instead reuse TCastleEdit? Although TDialog provides multiline display,
  which may be cool e.g. to edit long URLs in view3dscene. }
begin
  Result := inherited;
  // if Result then Exit; // ignore inherited Result, always true when InterceptInput

  { Under Windows, pressing Ctrl + Backspace causes key = K_BackSpace with
    character = CharDelete (not CharBackSpace).
    That is, Windows automatically equates Ctrl + Backspace
    with CharDelete, assuming that we always want to do the same action for them.

    However, I want to detect Ctrl + Backspace, and not detect "real" delete key
    presses (that may be handled in the future to delete char in front of cursor).
    So I just query for both CharBackSpace and K_BackSpace to detect backspace. }
  if Event.IsKey(CharBackSpace) or Event.IsKey(K_BackSpace) then
  begin
    if InputText <> '' then
      if mkCtrl in Container.Pressed.Modifiers then
        InputText := ''
      else
        InputText := Copy(InputText, 1, Length(InputText) - 1);
    Result := true;
  end else
  if Event.IsKey(CharEnter) then
  begin
    if Length(InputText) >= MinLength then
    begin
      FAnswerCancelled := false;
      DoAnswered;
    end;

    { How to show that we cannot accept the answer yet?
      This looks bad:
      MessageOk(Window, Format('You must enter at least %d characters.',
        [answerMinLen]), DefaultAlign); }
    Result := true;
  end else
  if Event.IsKey(CharEscape) then
  begin
    if CanCancel then
    begin
      FAnswerCancelled := true;
      DoAnswered;
    end;
    Result := true;
  end else
  if Event.IsKey(CtrlC) then
  begin
    if InputText <> '' then
      Clipboard.AsText := InputText;
    Result := true;
  end else
  if Event.IsKey(CtrlX) then
  begin
    if InputText <> '' then
    begin
      Clipboard.AsText := InputText;
      InputText := '';
    end;
    Result := true;
  end else
  if Event.IsKey(CtrlV) then
  begin
    InputText := SDeleteChars(Clipboard.AsText, AllChars - AllowedChars);
    Result := true;
  end else
  if (Event.EventType = itKey) and
     (Event.KeyString <> '') and
     (Event.KeyCharacter in AllowedChars) and
     ((MaxLength = 0) or (Length(InputText) < MaxLength)) then
  begin
    InputText := InputText + Event.KeyString;
    Result := true;
  end;
end;

function TStateDialogInput.DrawInputText: boolean;
begin
  Result := true;
end;

function TStateDialogInput.GetAnswer: string;
begin
  Result := InputText;
end;

procedure TStateDialogInput.SetAnswer(const Value: string);
begin
  InputText := Value;
end;

{ TStateDialogKey ------------------------------------------------------------ }

function TStateDialogKey.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  // if Result then Exit; // ignore inherited Result, always true when InterceptInput

  if (Event.EventType = itKey) and (Event.Key <> K_None) then
  begin
    FAnswer := Event.Key;
    DoAnswered;
    Result := true;
  end;
end;

{ TStateDialogPressEvent ------------------------------------------------------------ }

function TStateDialogPressEvent.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  // if Result then Exit; // ignore inherited Result, always true when InterceptInput

  FAnswer := Event;
  DoAnswered;
  Result := true;
end;

end.
