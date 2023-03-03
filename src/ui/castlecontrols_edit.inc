{%MainUnit castlecontrols.pas}
{
  Copyright 2017-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Edit box to input a single line of text.
    The contents are in the @link(Text) property, you can get or set them
    at any time.

    Note that you should set a suitable
    @link(Width TCastleUserInterface.Width) of the edit box,
    to nicely display a typical content.
    The height is by default auto-calculated (see @link(AutoSizeHeight))
    looking at font size (and other properties, like padding and frame size),
    and usually is reasonable automatically.
  }
  TCastleEdit = class(TCastleUserInterfaceFont)
  strict private
    FText: string;
    FPaddingHorizontal, FPaddingVertical, FPadding: Single;
    FBackgroundColor, FFocusedColor, FUnfocusedColor: TCastleColor;
    FPlaceholderColor: TCastleColor;
    FFrame: boolean;
    LifeTime: TFloatTime;
    FMaxLength: Cardinal;
    FAllowedChars: TSetOfChars;
    FCaptureAllInput: boolean;
    FAutoOnScreenKeyboard: Boolean;
    Scissor: TScissor;
    FAutoSizeHeight: Boolean;
    FPlaceholder: String;
    FOnChange: TNotifyEvent;
    FPasswordChar: Char;
    FTextTranslate, FPlaceholderTranslate: Boolean;
    FAlignment: THorizontalPosition;
    FEnabled: boolean;
    procedure SetText(const Value: string);
    procedure SetFocusedColor(const Value: TCastleColor);
    procedure SetUnfocusedColor(const Value: TCastleColor);
    procedure SetPlaceholderColor(const Value: TCastleColor);
    procedure SetBackgroundColor(const Value: TCastleColor);
    procedure SetPadding(const Value: Single);
    procedure SetPaddingHorizontal(const Value: Single);
    procedure SetPaddingVertical(const Value: Single);
    procedure SetFrame(const Value: boolean);
    procedure SetAutoSizeHeight(const Value: Boolean);
    procedure SetPlaceholder(const Value: String);
    procedure SetPasswordChar(const Value: Char);
    procedure SetAlignment(const Value: THorizontalPosition);
    procedure SetEnabled(const Value: boolean);
  protected
    procedure PreferredSize(var PreferredWidth, PreferredHeight: Single); override;
    function GetInternalText: String; override;
    procedure SetInternalText(const Value: String); override;
    procedure DoChange; virtual;
    procedure TranslateProperties(const TranslatePropertyEvent: TTranslatePropertyEvent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    function CapturesEventsAtPosition(const Position: TVector2): boolean; override;
    procedure SetFocused(const Value: boolean); override;
    procedure EditorAllowResize(
      out ResizeWidth, ResizeHeight: Boolean; out Reason: String); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Focused text color. By default it's black. }
    property FocusedColor: TCastleColor read FFocusedColor write SetFocusedColor;

    { Unfocused text color. By default it's dark gray. }
    property UnfocusedColor: TCastleColor read FUnfocusedColor write SetUnfocusedColor;

    { Color for @link(Placeholder). By default it's light gray. }
    property PlaceholderColor: TCastleColor read FPlaceholderColor write SetPlaceholderColor;

    { Background color. Fills the area under @link(Text) and the padding.
      By default it's white.
      Note that it can be transparent, in which case it will show the
      tiEdit image underneath (if @link(Frame) is @true),
      or UI control underneath (if @link(Frame) is @false). }
    property BackgroundColor: TCastleColor read FBackgroundColor write SetBackgroundColor;

    { Allowed characters that user can input.
      Note that this @italic(only restricts the user input),
      it does not prevent from programmatically setting @link(Text)
      to include disallowed characters. }
    property AllowedChars: TSetOfChars read FAllowedChars write FAllowedChars;

    { Max length of user input.
      Value of 0 (default) means "no limit".
      Note that this @italic(only restricts the user input),
      it does not prevent from programmatically setting @link(Text)
      to be longer. }
    property MaxLength: Cardinal read FMaxLength write FMaxLength;
  published
    const
      DefaultPadding = 4;
      DefaultAutoOnScreenKeyboard = false;

    { Currently input text. }
    property Text: string read FText write SetText;

    { Inside the box rectangle, padding between the borders (or frame, if @link(Frame)) and text.
      Total horizontal padding is the sum @code(PaddingHorizontal + Padding),
      total vertical padding is the sum @code(PaddingVertical + Padding).
      @groupBegin }
    property PaddingHorizontal: Single
      read FPaddingHorizontal write SetPaddingHorizontal {$ifdef FPC}default 0{$endif};
    property PaddingVertical: Single
      read FPaddingVertical write SetPaddingVertical {$ifdef FPC}default 0{$endif};
    property Padding: Single
      read FPadding write SetPadding {$ifdef FPC}default DefaultPadding{$endif};
    { @groupEnd }

    { Draw frame around the box. Frame uses theme image tiEdit,
      see TCastleTheme.Images if you want to customize it. }
    property Frame: boolean read FFrame write SetFrame default true;

    {$ifdef FPC}
    { Should we capture input regardless of the currently focused control.
      This is in practice only sensible if this is the only edit box currently
      visible on the screen. }
    property CaptureAllInput: boolean read FCaptureAllInput write FCaptureAllInput;
      deprecated 'use Container.ForceCaptureInput instead of this';
    {$endif}

    { When true, it shows the software keyboard on Android. }
    property AutoOnScreenKeyboard: Boolean
      read FAutoOnScreenKeyboard write FAutoOnScreenKeyboard
      default DefaultAutoOnScreenKeyboard;

    property AutoSizeHeight: Boolean
      read FAutoSizeHeight write SetAutoSizeHeight default true;

    { Displayed when @link(Text) is empty. }
    property Placeholder: String
      read FPlaceholder write SetPlaceholder;

    { Event sent when @link(Text) value was changed by a user.
      Note that this is @italic(not) called when you change @link(Text)
      property programmatically. }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    { Show given character (usually "*") instead of actual input characters.
      Useful for password input fields.

      Setting the PasswordChar to something else than #0 also informs
      the system that the input is sensitive (e.g. it is a password)
      and should not be stored in any system-wide dictionary for easy future reuse.
      This is meaningful for on-screen keyboards on mobile systems,
      that by default (when PasswordChar is #0) may store entered text in some
      system-wide dictionary and propose user to use the same words in future inputs.
      If the user inputs something sensitive (like a password) you want to disable this
      using PasswordChar <> #0. }
    property PasswordChar: Char
      read FPasswordChar write SetPasswordChar default #0;

    { Should the @link(Text) be localized (translated into other languages).
      Determines if the property is enumerated by @link(TCastleComponent.TranslateProperties),
      which affects the rest of localization routines. }
    property TextTranslate: Boolean read FTextTranslate write FTextTranslate default true;

    { Should the @link(Placeholder) be localized (translated into other languages).
      Determines if the property is enumerated by @link(TCastleComponent.TranslateProperties),
      which affects the rest of localization routines. }
    property PlaceholderTranslate: Boolean read FPlaceholderTranslate write FPlaceholderTranslate default true;

    { Horizontal alignment of the text. }
    property Alignment: THorizontalPosition
      read FAlignment write SetAlignment default hpLeft;

    { Enabled control can be focused and edited, is not grayed-out. }
    property Enabled: boolean read FEnabled write SetEnabled default true;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastleedit_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  { Descendant of TCastleEdit specialized for editing floating-point numbers.
    Use @link(Value) instead of @link(Text). }
  TCastleFloatEdit = class(TCastleEdit)
  strict private
    FDecimalDigits: Integer;
    FMax: Single;
    FMin: Single;
    function GetValue: Single;
    procedure SetValue(const AValue: Single);
    procedure SetDecimalDigits(const AValue: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetFocused(const AValue: Boolean); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    { In case of this descendant, Text is always derived from Value. }
    property Text stored false;
    property TextTranslate default false;

    property Value: Single read GetValue write SetValue {$ifdef FPC}default 0{$endif};
    { Minimum and maximum limits.
      Applied only if Min <= Max, which is not the case by default. }
    property Min: Single read FMin write FMin {$ifdef FPC}default 1.0{$endif};
    property Max: Single read FMax write FMax {$ifdef FPC}default 0.0{$endif};
    { How many decimal digits to display (and preserve).
      Set to -1 to display (and preserve) all digits,
      but beware that many numbers will then look hard to
      read, because they cannot be represented perfectly as a float.
      E.g. inputting "0.2" will change (in SetFocused) to something like
      "0.200000003" since the number 0.2 cannot be represented precisely. }
    property DecimalDigits: Integer
      read FDecimalDigits write SetDecimalDigits default 2;
  end;

  { Descendant of TCastleEdit specialized for editing integer numbers.
    Use @link(Value) instead of @link(Text). }
  TCastleIntegerEdit = class(TCastleEdit)
  strict private
    FMax: Int64;
    FMin: Int64;
    function GetValue: Int64;
    procedure SetValue(const AValue: Int64);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetFocused(const AValue: Boolean); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    { In case of this descendant, Text is always derived from Value. }
    property Text stored false;
    property TextTranslate default false;

    property Value: Int64 read GetValue write SetValue default 0;
    { Minimum and maximum limits.
      Applied only if Min <= Max, which is not the case by default. }
    property Min: Int64 read FMin write FMin default 1;
    property Max: Int64 read FMax write FMax default 0;
  end;

{$endif read_interface}

{$ifdef read_implementation}

const
  CaretChar = '|';

{ These routines may be public at some point. }

{ Displays the keyboard on Android. It requires the current text to be edited
  and the target user interface to which the events will be send. }
procedure ShowOnScreenKeyboard(const KeyboardTarget: TCastleUserInterface;
  const CurrentText: String; PasswordMode: Boolean); forward;

{ Hides keyboard on Android but only when KeyboardTarget and ForceCaptureInput
  is the same object. }
procedure HideOnScreenKeyboard(const KeyboardTarget: TCastleUserInterface); forward;

procedure ShowOnScreenKeyboard(const KeyboardTarget: TCastleUserInterface;
  const CurrentText: String; PasswordMode: Boolean);
begin
  { Remove focus from another control }
  if (KeyboardTarget.Container.ForceCaptureInput <> nil) and
     (KeyboardTarget.Container.ForceCaptureInput <> KeyboardTarget) then
    KeyboardTarget.Container.ForceCaptureInput.Focused := false;

  { Set ForceCaptureInput to control that will show keyboard }
  KeyboardTarget.Container.ForceCaptureInput := KeyboardTarget;

  { Send message to open keyboard. }
  Messaging.Send(['castle-show-keyboard', CurrentText,
    Iff(PasswordMode, 'true', 'false')]);
end;

procedure HideOnScreenKeyboard(const KeyboardTarget: TCastleUserInterface);
begin
  { Don't hide keyboard if another TCastleEdit has displayed it }
  if KeyboardTarget.Container.ForceCaptureInput = KeyboardTarget then
  begin
    Messaging.Send(['castle-hide-keyboard']);
    KeyboardTarget.Container.ForceCaptureInput := nil;
  end;
end;


{ TCastleEdit --------------------------------------------------------------- }

constructor TCastleEdit.Create(AOwner: TComponent);
begin
  inherited;
  FFocusedColor := Black;
  FUnfocusedColor := Vector4(0.25, 0.25, 0.25, 1);
  FPlaceholderColor := Vector4(0.75, 0.75, 0.75, 1);
  FBackgroundColor := White;
  FPadding := DefaultPadding;
  FFrame := true;
  FAllowedChars := AllChars;
  FMaxLength := 0;
  FAutoOnScreenKeyboard := DefaultAutoOnScreenKeyboard;
  Scissor := TScissor.Create;
  FAutoSizeHeight := true;
  FTextTranslate := true;
  FPlaceholderTranslate := true;
  FEnabled := true;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastleedit_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleEdit.Destroy;
begin
  FreeAndNil(Scissor);

  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastleedit_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

procedure TCastleEdit.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleEdit.SetFocusedColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FFocusedColor, Value) then
  begin
    FFocusedColor := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleEdit.SetUnfocusedColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FUnfocusedColor, Value) then
  begin
    FUnfocusedColor := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleEdit.SetPlaceholderColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FPlaceholderColor, Value) then
  begin
    FPlaceholderColor := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleEdit.SetBackgroundColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FBackgroundColor, Value) then
  begin
    FBackgroundColor := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleEdit.SetPadding(const Value: Single);
begin
  if FPadding <> Value then
  begin
    FPadding := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleEdit.SetPaddingHorizontal(const Value: Single);
begin
  if FPaddingHorizontal <> Value then
  begin
    FPaddingHorizontal := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleEdit.SetPaddingVertical(const Value: Single);
begin
  if FPaddingVertical <> Value then
  begin
    FPaddingVertical := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleEdit.SetFrame(const Value: boolean);
begin
  if FFrame <> Value then
  begin
    FFrame := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleEdit.SetAutoSizeHeight(const Value: Boolean);
begin
  if FAutoSizeHeight <> Value then
  begin
    FAutoSizeHeight := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleEdit.PreferredSize(var PreferredWidth, PreferredHeight: Single);
var
  PaddingVerticalScaled, HeightScaled: Single;
  US: Single;
begin
  inherited;

  if AutoSizeHeight then
  begin
    US := UIScale;
    PaddingVerticalScaled := US * (PaddingVertical + Padding);

    HeightScaled := Font.RowHeight + 2 * PaddingVerticalScaled;
    if Frame then
    begin
      HeightScaled := HeightScaled +
        US * Theme.ImagesPersistent[tiEdit].ProtectedSides.TotalTop +
        US * Theme.ImagesPersistent[tiEdit].ProtectedSides.TotalBottom;
    end;

    PreferredHeight := HeightScaled;
  end;
end;

procedure TCastleEdit.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TCastleEdit.EditorAllowResize(
  out ResizeWidth, ResizeHeight: Boolean; out Reason: String);
begin
  inherited;
  if AutoSizeHeight then
  begin
    ResizeHeight := false;
    Reason := SAppendPart(Reason, NL, 'Turn off "TCastleEdit.AutoSizeHeight" to change height.');
  end;
end;

function TCastleEdit.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'Text') or
     (PropertyName = 'Alignment') or
     (PropertyName = 'Enabled') or
     (PropertyName = 'Placeholder') or
     (PropertyName = 'PasswordChar') or
     (PropertyName = 'MaxLength') or
     (PropertyName = 'FocusedColorPersistent') or
     (PropertyName = 'UnfocusedColorPersistent') or
     (PropertyName = 'PlaceholderColorPersistent') or
     (PropertyName = 'BackgroundColorPersistent') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleEdit.Render;
const
  CaretSpeed = 1; //< how many blinks per second
var
  SR, RectInside: TFloatRectangle;
  PaddingHorizontalScaled, PaddingVerticalScaled: Single;
  US, TextLeft, TextBottom: Single;
  FrameSides: TBorder;
  CaretVisible: boolean;
  S: string;
  Color: TCastleColor;
  CaretShift: Single;
begin
  inherited;

  SR := RenderRect;
  US := UIScale;
  RectInside := SR;

  if Frame then
  begin
    { make smaller RectInside, by frame size }
    FrameSides := Theme.ImagesPersistent[tiEdit].ProtectedSides;
    RectInside := RectInside.
      RemoveTop   (FrameSides.TotalTop    * US).
      RemoveRight (FrameSides.TotalRight  * US).
      RemoveBottom(FrameSides.TotalBottom * US).
      RemoveLeft  (FrameSides.TotalLeft   * US);

    { draw frame }
    Theme.Draw(SR, tiEdit, US);
  end;

  { fill padding and area under Text with BackgroundColor }
  DrawRectangle(RectInside, BackgroundColor);

  { make smaller RectInside, by padding }
  PaddingHorizontalScaled := US * (PaddingHorizontal + Padding);
  PaddingVerticalScaled := US * (PaddingVertical + Padding);
  RectInside := RectInside.Grow(-PaddingHorizontalScaled, -PaddingVerticalScaled);

  // calculate S, Color, CaretShift
  CaretShift := 0;
  if Text <> '' then
  begin
    S := Text;

    { in case PasswordChar is non-zero, replace S with repeated PasswordChar }
    if PasswordChar <> #0 then
      S := StringOfChar(PasswordChar, Length(S));

    if Focused and Enabled then
    begin
      { Note that S here may be already processed by PasswordChar. }
      CaretShift := Font.TextWidth(S);
      Color := FocusedColor
    end else
    begin
      Color := UnfocusedColor;
      { CaretShift doesn't matter in this case,
        so we don't have to calculate TextWidth, saving a small time. }
    end;
  end else
  begin
    S := Placeholder;
    Color := PlaceholderColor;
  end;

  Scissor.Rect := RectInside.Round;
  Scissor.Enabled := true;

  case Alignment of
    hpLeft  : TextLeft := RectInside.Left;
    hpMiddle: TextLeft := (RectInside.Left + RectInside.Right - Font.TextWidth(S)) / 2;
    hpRight :
      begin
        TextLeft := RectInside.Right - Font.TextWidth(S);
        if Focused and Enabled then
          TextLeft := TextLeft - Font.TextWidth(CaretChar);
      end;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TCastleLabel.Render: Alignment?');
    {$endif}
  end;

  // center in RectInside, important when AutoSize = false
  TextBottom := RectInside.Bottom +
    (RectInside.Height - Font.RowHeight) / 2 + Font.Descend;
  Font.Print(TextLeft, TextBottom, Color, S);

  { Note that we have to display Caret even when Text = '' and S = Placeholder. }
  if Focused and Enabled then
  begin
    CaretVisible := FloatModulo(LifeTime * CaretSpeed, 1.0) < 0.5;
    if CaretVisible then
      Font.Print(TextLeft + CaretShift, TextBottom, FocusedColor, CaretChar);
  end;

  Scissor.Enabled := false;
end;

procedure TCastleEdit.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
begin
  inherited;
  LifeTime := LifeTime + SecondsPassed;
  { do not allow other controls to handle key presses
    (and e.g. move camera when "A" is pressed down), TCastleEdit handles all keys }
  HandleInput := false;
end;

function TCastleEdit.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result or (not Exists) or (not Enabled) then Exit;

  if Event.IsKey(CharBackSpace) or Event.IsKey(keyBackSpace) then
  begin
    if Text <> '' then
    begin
      {$ifdef FPC}
      Text := UTF8Copy(Text, 1, UTF8Length(Text) - 1);
      {$else}
      Text := UTF32Copy(Text, 1, GetUTF32Length(Text) - 1);
      {$endif}
      DoChange;
    end;
    Result := true;
  end else
  if Event.IsKey(CtrlC) then
  begin
    if Text <> '' then
      Clipboard.AsText := Text;
    Result := true;
  end else
  if Event.IsKey(CtrlX) then
  begin
    if Text <> '' then
    begin
      Clipboard.AsText := Text;
      Text := '';
      DoChange;
    end;
    Result := true;
  end else
  if Event.IsKey(CtrlV) then
  begin
    Text := SDeleteChars(Clipboard.AsText, AllChars - FAllowedChars);
    DoChange;
    Result := true;
  end else
  if (Event.EventType = itKey) and
     (Event.KeyString <> '') and
     CharInSet(Event.KeyCharacter, AllowedChars) and
     ((MaxLength = 0) or (Length(Text) < MaxLength)) and
     { Do not add unprintable special characters, like Enter and Escape.
       TCastleEdit doesn't process them,
       but also users don't expect these keys are something added to the edit field.
       Note: The range CtrlA..CtrlZ includes CharEnter. }
     (not CharInSet(Event.KeyCharacter, [CtrlA..CtrlZ, CharEscape])) then
  begin
    Text := Text + Event.KeyString;
    DoChange;
    Result := true;
  end;

  { Ability to open keyboard when it is closed and edit is focused }
  if FAutoOnScreenKeyboard and ApplicationProperties.TouchDevice and
     (Event.IsMouseButton(TCastleMouseButton.buttonLeft)) then
  begin
    { If Edit is focused but ForceCaptureInput is not set to it that means
      we need set focus again to show keyboard. }
    if Container.ForceCaptureInput = nil then
    begin
      Focused := true;
    end else
    if Container.ForceCaptureInput <> Self then
    begin
      { Only when other edit shows keyboard (or none), change it focus to false
        and focus/open keyboard for current edit }
      Container.ForceCaptureInput.Focused := false;
      Focused := true;
    end;
  end;
end;

function TCastleEdit.CapturesEventsAtPosition(const Position: TVector2): boolean;
begin
  {$ifdef FPC}
  {$warnings off} // using deprecated just to keep it working
  if CaptureAllInput then
    Result := true // always capture
  else
  {$warnings on}
  {$endif}
    Result := inherited CapturesEventsAtPosition(Position);
end;

procedure TCastleEdit.SetFocused(const Value: boolean);
begin
  inherited;

  { We take care about on screen keyboard only when AutoOnScreenKeyboard = true
    and this is touch device. We don't want to change Container.ForceCaptureInput
    in other cases.}
  if FAutoOnScreenKeyboard and ApplicationProperties.TouchDevice then
  begin
    if Value then
      ShowOnScreenKeyboard(Self, Text, PasswordChar <> #0)
    else
      HideOnScreenKeyboard(Self);
  end;
end;

procedure TCastleEdit.SetPlaceholder(const Value: string);
begin
  if FPlaceholder <> Value then
  begin
    FPlaceholder := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleEdit.SetPasswordChar(const Value: Char);
begin
  if FPasswordChar <> Value then
  begin
    FPasswordChar := Value;
    VisibleChange([chRender]);
  end;
end;

function TCastleEdit.GetInternalText: String;
begin
  Result := Text;
end;

procedure TCastleEdit.SetInternalText(const Value: String);
begin
  Text := Value;
end;

procedure TCastleEdit.TranslateProperties(
  const TranslatePropertyEvent: TTranslatePropertyEvent);
var
  S: String;
begin
  if TextTranslate and (Text <> '') then
  begin
    S := Text;
    TranslatePropertyEvent(Self, 'Text', S);
    Text := S;
  end;

  if PlaceholderTranslate and (Placeholder <> '') then
  begin
    S := Placeholder;
    TranslatePropertyEvent(Self, 'Placeholder', S);
    Placeholder := S;
  end;
end;

procedure TCastleEdit.SetAlignment(const Value: THorizontalPosition);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleEdit.SetEnabled(const Value: boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    VisibleChange([chRender]);
  end;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleedit_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastleFloatEdit ---------------------------------------------------------- }

constructor TCastleFloatEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TextTranslate := false;
  FMin := 1;
  FMax := 0;
  Assert(not (Min <= Max)); // deliberately, so that limits do not work by default
  AllowedChars := ['-', '+', '0'..'9'] + [{$ifdef FPC}DefaultFormatSettings.{$else}FormatSettings.{$endif}DecimalSeparator];
  FDecimalDigits := 2;
  Value := 0.0; // will also assign Text
end;

procedure TCastleFloatEdit.SetFocused(const AValue: Boolean);
begin
  inherited SetFocused(AValue);
  { Apply to Text limits (Min, Max) and fix invalid input (like Text empty). }
  Value := Value;
end;

function TCastleFloatEdit.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'Text') then
    Result := [] // override TCastleEdit that made it [psBasic], we instead expose Value
  else if (PropertyName = 'Value') or
          (PropertyName = 'Min') or
          (PropertyName = 'Max') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

function TCastleFloatEdit.GetValue: Single;
begin
  Result := StrToFloatDefDot(Text, 0);
  if Min <= Max then
    ClampVar(Result, Min, Max);
end;

procedure TCastleFloatEdit.SetValue(const AValue: Single);
begin
  if DecimalDigits < 0 then
    Text := FloatToStrDot(AValue)
  else
    // TODO Add FloatToStrFDot, use it
    Text := FloatToStrF(AValue, ffFixed, 0 { Precision doesn't matter }, DecimalDigits);
end;

procedure TCastleFloatEdit.SetDecimalDigits(const AValue: Integer);
begin
  if FDecimalDigits <> AValue then
  begin
    FDecimalDigits := AValue;
    { Apply the DecimalDigits to current Text look }
    Value := Value;
  end;
end;

{ TCastleIntegerEdit ---------------------------------------------------------- }

constructor TCastleIntegerEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TextTranslate := false;
  FMin := 1;
  FMax := 0;
  Assert(not (Min <= Max)); // deliberately, so that limits do not work by default
  AllowedChars := ['-', '+', '0'..'9'];
  Value := 0; // will also assign Text
end;

procedure TCastleIntegerEdit.SetFocused(const AValue: Boolean);
begin
  inherited SetFocused(AValue);
  { Apply to Text limits (Min, Max) and fix invalid input (like Text empty). }
  Value := Value;
end;

function TCastleIntegerEdit.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'Text') then
    Result := [] // override TCastleEdit that made it [psBasic], we instead expose Value
  else if (PropertyName = 'Value') or
          (PropertyName = 'Min') or
          (PropertyName = 'Max') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

function TCastleIntegerEdit.GetValue: Int64;
begin
  Result := StrToInt64Def(Text, 0);
  if Min <= Max then
    ClampVar(Result, Min, Max);
end;

procedure TCastleIntegerEdit.SetValue(const AValue: Int64);
begin
  Text := IntToStr(AValue);
end;

{$endif read_implementation}
