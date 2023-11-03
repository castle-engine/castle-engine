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

(*Dialog windows (to display some information, or ask user for confirmation,
  or ask user to input a simple value) as a user-interface view (@link(TCastleView)).
  This unit defines the user-interface view classes
  (@link(TCastleView) descendants) to display given dialog.

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

  Underneath, the @link(MessageYesNo) will use a @link(TViewDialogYesNo),
  making sure that your normal window callbacks are redirected as appropriate.
  But you don't need to be concerned with this.

  However, if you need to work on iOS, then you cannot use most of routines
  from the @link(CastleMessages) unit. You can use @link(MessageOK) if you turn on
  the @link(MessageOKPushesView) flag, but nothing else. E.g. @link(MessageYesNo)
  cannot work on iOS now. Making modal boxes like that is not supported on iOS.

  In this case you should use this unit and instantiate the
  user-interface classes yourself, and you need to
  @italic(organize your whole game using TCastleView classes).
  See https://castle-engine.io/views about how to use @link(TCastleView). Like this:

  @longCode(#
  type
    TMyGameView = class(TCastleView)
    private
      DialogAskDeleteFile: TViewDialogYesNo;
    public
      function Press(const Event: TInputPressRelease): boolean; override;
      procedure Resume; override;
    end;

  function TMyGameView.Press(const Event: TInputPressRelease): boolean; override;
  begin
    Result := inherited;
    if Result then Exit;

    if Event.IsKey(keyEnter) then
    begin
      DialogAskDeleteFile := TViewDialogYesNo.Create(Self);
      DialogAskDeleteFile.Caption := 'Are you sure you want to delete this file?';
      Container.PushView(DialogAskDeleteFile);
    end;
  end;

  procedure TMyGameView.Resume;
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

unit CastleDialogViews;

{$I castleconf.inc}

interface

uses Classes, Math,
  CastleGLUtils, CastleUtils, CastleImages,
  CastleStringUtils, CastleVectors, CastleKeysMouse, CastleControls,
  CastleRectangles, CastleColors, CastleUIControls,
  CastleFonts, CastleInternalRichText, CastleTimeUtils;

type
  { Abstract class for a modal dialog user-interface view.
    See unit @link(CastleDialogViews) documentation for example usage. }
  TViewDialog = class abstract(TCastleView)
  strict private
    type
      {$define read_interface}
      {$I castledialogviews_dialog.inc}
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
      FUnusedSaveScreen: TCastleImage;
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    function GetInputText: string;
    procedure SetInputText(const Value: string);
  protected
    type
      TButtonArray = array of TCastleButton;
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

    { Save screen @italic(now) to be used by subsequent @link(Start).
      Does a screenshot only if our current properties (@link(Background),
      @link(BackgroundScreenshot), @link(BackgroundColor)) indicate screenshot is needed.
      It allows to make a screenshot earlier than at @link(Start) call. }
    procedure SaveScreenIfNecessary(const AContainer: TCastleContainer);

    { When user answers the dialog, this is set to @true.
      The view also normally does TCastleContainer.PopView, so there's no need to check
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
        ViewDialogOK.Text.Clear;
        ViewDialogOK.Text.Add('First line');
        ViewDialogOK.Text.Add('Second line');

        // alternative way to do the same
        ViewDialogOK.Caption := 'First line' + LineEnding + 'Second line';
      #)
    }
    property Caption: string read GetCaption write SetCaption stored false;

    { Horizontal alignment of the text. }
    property Alignment: THorizontalPosition
      read FAlignment write FAlignment default DefaultAlignment;

    { Enable a subset of HTML to mark font changes inside the text.
      See the TCastleAbstractFont.PrintStrings for a description of supported
      HTML constructs. }
    property Html: boolean read FHtml write FHtml default false;

    { Obscure the view underneath with our own background (using a color or screenshot).

      The obscuring background is defined by @link(BackgroundColor).
      It may be just a solid opaque color.
      Or it may be a partially-transparent or even completely transparent
      color, showing the view underneath (or showing the screenshot
      of the view underneath, if @link(BackgroundScreenshot)). }
    property Background: boolean read FBackground write FBackground default false;

    { Color of the background obscuring the view underneath,
      if @link(Background) is @true.
      This color may be partially-transparent (e.g. to visually "dim" the view
      underneath) or even completely transparent (alpha 0).

      Default is Theme.BackgroundColor,
      which is dark with alpha = 0.5, so it will dim the view underneath.
      Sometimes (under exception handler) it's Theme.BackgroundOpaqueColor. }
    property BackgroundColor: TCastleColor read FBackgroundColor write FBackgroundColor;

    { Initialize the background by taking a screenshot of the current screen
      when the view was started.
      This screenshot is shown, instead of actually rendering the view underneath,
      under the @link(BackgroundColor), when @link(Background) is @true.

      This is less functional (when the user scales the window,
      the screenshot is stretched too), but is safer:
      it means that the view underneath does not need to be "renderable" anymore. }
    property BackgroundScreenshot: boolean
      read FBackgroundScreenshot write FBackgroundScreenshot default false;

    { Should the view do @link(TCastleContainer.PopView) when answered.
      This is usually most natural. }
    property PopOnAnswered: boolean
      read FPopOnAnswered write FPopOnAnswered default true;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tviewdialog_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  { Wait for simple confirmation ("OK") from user.
    See unit @link(CastleDialogViews) documentation for example usage. }
  TViewDialogOK = class(TViewDialog)
  strict private
    procedure ButtonOKClick(Sender: TObject);
  protected
    procedure InitializeButtons(var Buttons: TViewDialog.TButtonArray); override;
  public
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

  { Ask user a simple "yes" / "no" question.
    See unit @link(CastleDialogViews) documentation for example usage. }
  TViewDialogYesNo = class(TViewDialog)
  strict private
    FAnswer: boolean;
    procedure ButtonYesClick(Sender: TObject);
    procedure ButtonNoClick(Sender: TObject);
  protected
    procedure InitializeButtons(var Buttons: TViewDialog.TButtonArray); override;
  public
    function Press(const Event: TInputPressRelease): boolean; override;
    { User answer to the dialog question, defined when @link(Answered). }
    property Answer: boolean read FAnswer;
  end;

  { Ask user to choose from a number of options.
    Each choice is shown as a button, user can also press the appropriate character.
    ButtonChars length must be always equal to ButtonCaptions.
    See unit @link(CastleDialogViews) documentation for example usage. }
  TViewDialogChoice = class(TViewDialog)
  strict private
    FAnswer: char;
    procedure ButtonClick(Sender: TObject);
  protected
    procedure InitializeButtons(var Buttons: TViewDialog.TButtonArray); override;
  public
    ButtonCaptions: array of string;
    ButtonChars: array of char;
    AllowCancel: boolean;
    function Press(const Event: TInputPressRelease): boolean; override;
    { User answer to the dialog question, defined when @link(Answered).
      This is one of the ButtonChars. }
    property Answer: char read FAnswer;
  end;

  { Ask user to input a string, or cancel.
    See unit @link(CastleDialogViews) documentation for example usage. }
  TViewDialogInput = class(TViewDialog)
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
    procedure InitializeButtons(var Buttons: TViewDialog.TButtonArray); override;
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

    { The user input. May be set before starting the view.
      After the view stopped, if @link(Answered), then this contains user answer.
      You should ignore it if @link(AnswerCancelled). }
    property Answer: string read GetAnswer write SetAnswer;
  end;

  { Ask user a press any key, and return this key.
    See unit @link(CastleDialogViews) documentation for example usage. }
  TViewDialogKey = class(TViewDialog)
  strict private
    FAnswer: TKey;
  public
    function Press(const Event: TInputPressRelease): boolean; override;
    { Key pressed by user, defined when @link(Answered). }
    property Answer: TKey read FAnswer;
  end;

  { Ask user a press anything (key, mouse button, mouse wheel),
    for example to configure a keybinding for a game.
    See unit @link(CastleDialogViews) documentation for example usage. }
  TViewDialogPressEvent = class(TViewDialog)
  strict private
    FAnswer: TInputPressRelease;
  public
    function Press(const Event: TInputPressRelease): boolean; override;
    { Key pressed by user, defined when @link(Answered). }
    property Answer: TInputPressRelease read FAnswer;
  end;

implementation

uses SysUtils;

{$define read_implementation}
{$I castledialogviews_dialog.inc}

resourcestring
  SOk = 'OK';
  SCancel = 'Cancel';
  SYes = 'Yes';
  SNo = 'No';

{ TViewDialog ------------------------------------------------------------- }

constructor TViewDialog.Create(AOwner: TComponent);
begin
  inherited;
  FText := TStringList.Create;
  FAlignment := DefaultAlignment;
  FBackground := true;
  if Theme.InternalMessageFallbackLook then
    FBackgroundColor := Vector4(Theme.BackgroundOpaqueColor, 1)
  else
    FBackgroundColor := Theme.BackgroundColor;
  FPopOnAnswered := true;
  FDialog := TDialog.Create(Self);

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tviewdialog_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TViewDialog.Destroy;
begin
  FreeAndNil(FText);
  FreeAndNil(FUnusedSaveScreen);

  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tviewdialog_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

function TViewDialog.GetCaption: string;
begin
  Result := TrimEndingNewline(Text.Text);
end;

procedure TViewDialog.SetCaption(const Value: string);
begin
  Text.Text := Value;
end;

function TViewDialog.GetInputText: string;
begin
  Result := FDialog.InputText;
end;

procedure TViewDialog.SetInputText(const Value: string);
begin
  FDialog.InputText := Value;
end;

procedure TViewDialog.SaveScreenIfNecessary(const AContainer: TCastleContainer);
begin
  if Background and BackgroundScreenshot and (BackgroundColor[3] <> 1) then
  begin
    FUnusedSaveScreen := AContainer.SaveScreen;
  end;
end;

procedure TViewDialog.Start;
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
      This means that Theme.InternalMessageFallbackLook prevents from taking a screenshot. }
    if BackgroundScreenshot and (BackgroundColor[3] <> 1) then
    begin
      BackgroundImage := TCastleImageControl.Create(FreeAtStop);
      BackgroundImage.Stretch := true;
      BackgroundImage.FullSize := true;
      if FUnusedSaveScreen <> nil then
      begin
        BackgroundImage.Image := FUnusedSaveScreen;
        FUnusedSaveScreen := nil;
      end else
        BackgroundImage.Image := Container.SaveScreen;
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

  if Theme.InternalMessageFallbackLook then
    ForceFallbackLook(FDialog);

  InterceptInput := true;
end;

procedure TViewDialog.Stop;
begin
  // remove FDialog, to clearly reinsert it at next Start call
  RemoveControl(FDialog);
  inherited;
end;

procedure TViewDialog.InitializeButtons(var Buttons: TButtonArray);
begin
end;

function TViewDialog.DrawInputText: boolean;
begin
  Result := false;
end;

procedure TViewDialog.DoAnswered;
begin
  FAnswered := true;
  if PopOnAnswered then
    Container.PopView(Self);
end;

{ TViewDialogOK ------------------------------------------------------------- }

procedure TViewDialogOK.InitializeButtons(var Buttons: TViewDialog.TButtonArray);
begin
  SetLength(Buttons, 1);
  Buttons[0] := TCastleButton.Create(Self);
  Buttons[0].OnClick := {$ifdef FPC}@{$endif}ButtonOKClick;
  Buttons[0].Caption := SOk;
end;

procedure TViewDialogOK.ButtonOKClick(Sender: TObject);
begin
  DoAnswered;
end;

function TViewDialogOK.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  // if Result then Exit; // ignore inherited Result, always true when InterceptInput

  if Event.IsKey(CharEnter) or Event.IsKey(CharEscape) then
  begin
    DoAnswered;
    Result := true;
  end;
end;

{ TViewDialogYesNo ---------------------------------------------------------- }

procedure TViewDialogYesNo.InitializeButtons(var Buttons: TViewDialog.TButtonArray);
begin
  SetLength(Buttons, 2);

  Buttons[0] := TCastleButton.Create(Self);
  Buttons[0].OnClick := {$ifdef FPC}@{$endif}ButtonNoClick;
  Buttons[0].Caption := SNo;

  Buttons[1] := TCastleButton.Create(Self);
  Buttons[1].OnClick := {$ifdef FPC}@{$endif}ButtonYesClick;
  Buttons[1].Caption := SYes;
end;

procedure TViewDialogYesNo.ButtonYesClick(Sender: TObject);
begin
  FAnswer := true;
  DoAnswered;
end;

procedure TViewDialogYesNo.ButtonNoClick(Sender: TObject);
begin
  FAnswer := false;
  DoAnswered;
end;

function TViewDialogYesNo.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  // if Result then Exit; // ignore inherited Result, always true when InterceptInput

  if Event.IsKey(keyY) or Event.IsKey(keyEnter) then
  begin
    FAnswer := true;
    DoAnswered;
    Result := true; // set Result, in case developer changed our InterceptInput to false
  end;

  if Event.IsKey(keyN) or Event.IsKey(keyEscape) then
  begin
    FAnswer := false;
    DoAnswered;
    Result := true; // set Result, in case developer changed our InterceptInput to false
  end;
end;

{ TViewDialogChoice ---------------------------------------------------------- }

procedure TViewDialogChoice.InitializeButtons(var Buttons: TViewDialog.TButtonArray);
var
  I: Integer;
begin
  Check(Length(ButtonCaptions) = Length(ButtonChars), 'In TViewDialogChoice, ButtonCaptions must have the same length as ButtonChars');
  SetLength(Buttons, Length(ButtonCaptions));

  for I := 0 to High(ButtonCaptions) do
  begin
    Buttons[I] := TCastleButton.Create(Self);
    Buttons[I].OnClick := {$ifdef FPC}@{$endif}ButtonClick;
    Buttons[I].Caption := ButtonCaptions[I];
    Buttons[I].Tag := Ord(ButtonChars[I]);
  end;
end;

procedure TViewDialogChoice.ButtonClick(Sender: TObject);
begin
  FAnswer := Chr((Sender as TCastleButton).Tag);
  DoAnswered;
end;

function TViewDialogChoice.Press(const Event: TInputPressRelease): boolean;
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
      Exit(true);
    end;

  if AllowCancel and Event.IsKey(CharEscape) then
  begin
    FAnswer := CharEscape;
    DoAnswered;
    Exit(true);
  end;
end;

{ TViewDialogInput ---------------------------------------------------------- }

procedure TViewDialogInput.InitializeButtons(var Buttons: TViewDialog.TButtonArray);
begin
  if CanCancel then
  begin
    SetLength(Buttons, 2);

    Buttons[0] := TCastleButton.Create(Self);
    Buttons[0].OnClick := {$ifdef FPC}@{$endif}ButtonCancelClick;
    Buttons[0].Caption := SCancel;

    Buttons[1] := TCastleButton.Create(Self);
    Buttons[1].OnClick := {$ifdef FPC}@{$endif}ButtonOKClick;
    Buttons[1].Caption := SOk;
  end else
  begin
    SetLength(Buttons, 1);
    Buttons[0] := TCastleButton.Create(Self);
    Buttons[0].OnClick := {$ifdef FPC}@{$endif}ButtonOKClick;
    Buttons[0].Caption := SOk;
  end;
end;

procedure TViewDialogInput.ButtonOKClick(Sender: TObject);
begin
  Press(InputKey(Container.MousePosition, keyEnter, CharEnter, []));
end;

procedure TViewDialogInput.ButtonCancelClick(Sender: TObject);
begin
  Press(InputKey(Container.MousePosition, keyEscape, CharEscape, []));
end;

function TViewDialogInput.Press(const Event: TInputPressRelease): boolean;
{ TODO: copy-paste of TCastleEdit.Press here.
  We should instead reuse TCastleEdit? Although TDialog provides multiline display,
  which may be cool e.g. to edit long URLs in view3dscene. }
begin
  Result := inherited;
  // if Result then Exit; // ignore inherited Result, always true when InterceptInput

  { Under Windows, pressing Ctrl + Backspace causes key = keyBackSpace with
    character = CharDelete (not CharBackSpace).
    That is, Windows automatically equates Ctrl + Backspace
    with CharDelete, assuming that we always want to do the same action for them.

    However, I want to detect Ctrl + Backspace, and not detect "real" delete key
    presses (that may be handled in the future to delete char in front of cursor).
    So I just query for both CharBackSpace and keyBackSpace to detect backspace. }
  if Event.IsKey(CharBackSpace) or Event.IsKey(keyBackSpace) then
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
     CharInSet(Event.KeyCharacter, AllowedChars) and
     ((MaxLength = 0) or (Length(InputText) < MaxLength)) then
  begin
    InputText := InputText + Event.KeyString;
    Result := true;
  end;
end;

function TViewDialogInput.DrawInputText: boolean;
begin
  Result := true;
end;

function TViewDialogInput.GetAnswer: string;
begin
  Result := InputText;
end;

procedure TViewDialogInput.SetAnswer(const Value: string);
begin
  InputText := Value;
end;

{ TViewDialogKey ------------------------------------------------------------ }

function TViewDialogKey.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  // if Result then Exit; // ignore inherited Result, always true when InterceptInput

  if (Event.EventType = itKey) and (Event.Key <> keyNone) then
  begin
    FAnswer := Event.Key;
    DoAnswered;
    Result := true;
  end;
end;

{ TViewDialogPressEvent ------------------------------------------------------------ }

function TViewDialogPressEvent.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  // if Result then Exit; // ignore inherited Result, always true when InterceptInput

  FAnswer := Event;
  DoAnswered;
  Result := true;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tviewdialog_persistent_vectors.inc}
{$undef read_implementation_methods}

end.
