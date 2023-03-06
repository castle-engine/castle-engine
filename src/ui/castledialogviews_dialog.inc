{
  Copyright 2010-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Dialog box that can display a long text, with automatic vertical scrollbar.
    You can also add buttons at the bottom.
    You can also have an input text area. }
  TDialog = class(TCastleUserInterfaceFont)
  strict private
    const
      BoxMargin = 10;
      WindowMargin = 10;
      ButtonHorizontalMargin = 10;
    type
      TDialogScrollArea = class(TCastleUserInterface)
      strict private
        Dialog: TDialog;
      public
        constructor Create(AOwner: TComponent); override;
        procedure Render; override;
      end;
    var
    FInputText: string;

    { Broken Text. }
    Broken_Text: TRichText;
    { Ignored (not visible) if not DrawInputText.
      Else broken InputText. }
    Broken_InputText: TStringList;

    MaxLineWidth: Single;
    { Sum of all Broken_Text.Count + Broken_InputText.Count.
      In other words, all lines that are scrolled by the scrollbar. }
    AllScrolledLinesCount: integer;

    { Things below set in MessageCore, readonly afterwards. }
    { Main text to display. Read-only contents. }
    Text: TStringList;
    TextAlign: THorizontalPosition;
    { Should we display InputText }
    DrawInputText: boolean;
    Buttons: array of TCastleButton;
    LifeTime: TFloatTime;
    FHtml: boolean;
    ScrollView: TCastleScrollView;

    function BoxMarginScaled: Single;
    function WindowMarginScaled: Single;
    function ButtonHorizontalMarginScaled: Single;

    procedure SetInputText(const Value: string);

    { Calculate height in pixels needed to draw Buttons.
      Returns 0 if there are no Buttons. }
    function ButtonsHeightScaled: Single;
    function ButtonsHeight: Single;
    procedure UpdateSizes;
  protected
    procedure PreferredSize(var PreferredWidth, PreferredHeight: Single); override;
  public
    { Set this to @true to signal that modal dialog window should be closed.
      This is not magically handled --- if you implement a modal dialog box,
      you should check in your loop whether something set Answered to @true. }
    Answered: boolean;

    { Input text. Displayed only if DrawInputText. }
    property InputText: string read FInputText write SetInputText;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Assign display stuff. Call this before adding control to Controls list. }
    procedure Initialize(
      const TextList: TStrings; const ATextAlign: THorizontalPosition;
      const AHtml: boolean;
      const AButtons: array of TCastleButton;
      const ADrawInputText: boolean);
    procedure Resize; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    procedure Render; override;
    function CapturesEventsAtPosition(const Position: TVector2): boolean; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

const
  CaretChar = '|';

{ TDialogScrollArea -------------------------------------------------------------- }

constructor TViewDialog.TDialog.TDialogScrollArea.Create(AOwner: TComponent);
begin
  inherited;
  Dialog := AOwner as TViewDialog.TDialog;
  FullSize := true; // we want our RenderRect to be equal to parent
end;

procedure TViewDialog.TDialog.TDialogScrollArea.Render;
type
  TCaretMode = (cmNone, cmVisible, cmInvisible);

  { Render a Text line, and move Y up to the line above. }
  procedure DrawString(X: Single; var Y: Single; const Color: TCastleColor;
    Text: string; const TextAlign: THorizontalPosition;
    const Caret: TCaretMode);
  var
    CaretWidth: Single;
  begin
    if Caret <> cmNone then
      CaretWidth := Dialog.Font.TextWidth(CaretChar) else
      CaretWidth := 0;
    { change X only locally, to take TextAlign into account }
    case TextAlign of
      hpMiddle: X := X + ((Dialog.MaxLineWidth - (Dialog.Font.TextWidth(Text) + CaretWidth)) / 2);
      hpRight : X := X + ( Dialog.MaxLineWidth - (Dialog.Font.TextWidth(Text) + CaretWidth));
      else ;
    end;
    if Caret = cmVisible then
      Text := Text + CaretChar;
    Dialog.Font.Print(X, Y, Color, Text);
    { change Y for caller, to print next line higher }
    Y := Y + Dialog.Font.Height;
  end;

  { Render all lines in S, and move Y up to the line above. }
  procedure DrawStrings(const X: Single; var Y: Single;
    const Color: TCastleColor; const s: TStrings; TextAlign: THorizontalPosition;
    const AddCaret: boolean);
  const
    CaretSpeed = 1; //< how many blinks per second
  var
    I: Integer;
    Caret: TCaretMode;
  begin
    for i := S.Count - 1 downto 0 do
    begin
      if AddCaret and (I = S.Count - 1) then
      begin
        if FloatModulo(Dialog.LifeTime * CaretSpeed, 1.0) < 0.5 then
          Caret := cmVisible else
          Caret := cmInvisible;
      end else
        Caret := cmNone;
      { each DrawString call will move Y up }
      DrawString(X, Y, Color, s[i], TextAlign, Caret);
    end;
  end;

var
  TextX, TextY: Single;
  SR: TFloatRectangle;
begin
  inherited;

  SR := RenderRect; // screen rectangle of ScrollView.ScrollArea
  TextX := SR.Left   + Dialog.BoxMarginScaled;
  TextY := SR.Bottom + Dialog.BoxMarginScaled;

  { draw Broken_InputText and Broken_Text.
    Order matters, as it's drawn from bottom to top. }
  if Dialog.DrawInputText then
    DrawStrings(TextX, TextY, Theme.MessageInputTextColor,
      Dialog.Broken_InputText, Dialog.TextAlign, true);

  { adjust TextX for TRichText.Print call }
  case Dialog.TextAlign of
    hpMiddle: TextX := (SR.Left + SR.Right) / 2;
    hpRight : TextX := SR.Right - Dialog.BoxMarginScaled;
    else ;
  end;
  Dialog.Broken_Text.Print(TextX, TextY, Theme.MessageTextColor, 0, Dialog.TextAlign);
end;

{ TViewDialog.TDialog -------------------------------------------------------------- }

constructor TViewDialog.TDialog.Create(AOwner: TComponent);
begin
  inherited;
  { use Theme.MessageFont this way }
  CustomFont := Theme.MessageFont;
  Text := TStringList.Create;
  { Contents of Broken_InputText will be initialized in TViewDialog.TDialog.UpdateSizes. }
  Broken_InputText := TStringList.Create;
  Anchor(hpMiddle);
  Anchor(vpMiddle);

  ScrollView := TCastleScrollView.Create(Self);
  ScrollView.ScrollArea.InsertFront(TDialogScrollArea.Create(Self));
  // we will set size explicitly in TViewDialog.TDialog.UpdateSizes
  ScrollView.ScrollArea.AutoSizeToChildren := false;
  ScrollView.EnableDragging := true;
  InsertFront(ScrollView);
end;

procedure TViewDialog.TDialog.Initialize(const TextList: TStrings;
  const ATextAlign: THorizontalPosition; const AHtml: boolean;
  const AButtons: array of TCastleButton;
  const ADrawInputText: boolean);
var
  I: Integer;
begin
  Text.Assign(TextList);
  TextAlign := ATextAlign;
  FHtml := AHtml;
  DrawInputText := ADrawInputText;
  SetLength(Buttons, Length(AButtons));
  for I := 0 to High(AButtons) do
  begin
    Buttons[I] := AButtons[I];
    InsertFront(Buttons[I]);
  end;
end;

destructor TViewDialog.TDialog.Destroy;
begin
  FreeAndNil(Broken_Text);
  FreeAndNil(Broken_InputText);
  FreeAndNil(Text);
  inherited;
end;

procedure TViewDialog.TDialog.SetInputText(const value: string);
begin
  FInputText := value;
  if Container <> nil then
  begin
    VisibleChange([chRectangle]);
    UpdateSizes;
  end;
end;

function TViewDialog.TDialog.ButtonsHeightScaled: Single;
var
  Button: TCastleButton;
begin
  Result := 0;
  for Button in Buttons do
    MaxVar(Result, Button.EffectiveHeight * UIScale + 2 * BoxMarginScaled);
end;

function TViewDialog.TDialog.ButtonsHeight: Single;
var
  Button: TCastleButton;
begin
  Result := 0;
  for Button in Buttons do
    MaxVar(Result, Button.EffectiveHeight + 2 * BoxMargin);
end;

procedure TViewDialog.TDialog.Resize;
begin
  inherited;
  UpdateSizes;
end;

procedure TViewDialog.TDialog.UpdateSizes;

  { Reposition Buttons. }
  procedure UpdateButtons;
  var
    X, Y: Single;
    I: Integer;
    Button: TCastleButton;
  begin
    if Length(Buttons) <> 0 then
    begin
      X := -BoxMargin;
      Y :=  BoxMargin;
      for I := Length(Buttons) - 1 downto 0 do
      begin
        Button := Buttons[I];
        Button.Anchor(vpBottom, Y);
        Button.Anchor(hpRight, X);
        X := X - (Button.EffectiveWidth + ButtonHorizontalMargin);
      end;
    end;
  end;

var
  BreakWidth, ButtonsWidth: Single;
  Button: TCastleButton;
  PR: TFloatRectangle;
begin
  { TODO: Using PR variable, otherwise on iOS directly writing ParentRect.Width
    causes range check error. Investigating,
    looks like FPC bug (seen on FPC 3.0.2, other versions not tested yet).

    - Writeln(ParentRect.Width) is some huge (impossible for 32-bit actually,
      and yes, it's 32-bit on iOS too) value (4879082848896).
    - But Writeln(Format('%d', [ParentRect.Width])) shows normal value (640).
    - Using ParentRect.Width in expression causes range check error.

    Using PR.Width always works OK.
  }
  PR := ParentRect;

  { calculate BreakWidth, which is the width at which we should break
    our string lists Broken_Xxx. We must here always subtract
    EffectiveScrollBarWidth to be on the safe side, because we don't know
    yet is ScrollBarVisible. }
  BreakWidth := Max(0, PR.Width - BoxMarginScaled * 2
    - WindowMarginScaled * 2 - ScrollView.EffectiveScrollBarWidth * UIScale);

  { calculate MaxLineWidth and AllScrolledLinesCount }

  { calculate Broken_Text }
  FreeAndNil(Broken_Text);
  Broken_Text := TRichText.Create(Font, Text, FHtml);
  Broken_Text.Wrap(BreakWidth);
  MaxLineWidth := Broken_Text.Width;
  AllScrolledLinesCount := Broken_Text.Count;

  ButtonsWidth := 0;
  for Button in Buttons do
    ButtonsWidth := ButtonsWidth + (Button.EffectiveWidth * UIScale + ButtonHorizontalMarginScaled);
  if ButtonsWidth > 0 then
    ButtonsWidth := ButtonsWidth - ButtonHorizontalMarginScaled; // extract margin from last button
  MaxVar(MaxLineWidth, ButtonsWidth);

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
    MaxVar(MaxLineWidth, Font.MaxTextWidth(Broken_InputText) + Font.TextWidth(CaretChar));
    AllScrolledLinesCount := AllScrolledLinesCount + Broken_InputText.count;
  end;

  { Now we have MaxLineWidth and AllScrolledLinesCount calculated,
    so our Rect and RenderRect return valid values. }

  ScrollView.Anchor(hpLeft, 0);
  ScrollView.Anchor(vpBottom, ButtonsHeight);
  ScrollView.Width := EffectiveWidth;
  ScrollView.Height := EffectiveHeight - ButtonsHeight;

  { add Font.DescenderHeight, to be able to see the descend of the bottom line when Scroll is ScrollMax. }
  ScrollView.ScrollArea.Height :=
    (Font.Height * AllScrolledLinesCount + Font.DescenderHeight) / UIScale
    + 2 * BoxMargin;
  ScrollView.ScrollArea.Width := EffectiveWidth;

  UpdateButtons;
end;

procedure TViewDialog.TDialog.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
begin
  inherited;
  LifeTime := LifeTime + SecondsPassed;
  { when we have input text, we display blinking caret, so keep redrawing }
  if DrawInputText then
    VisibleChange([chRender]);
end;

procedure TViewDialog.TDialog.Render;
begin
  inherited;

  Theme.Draw(RenderRect, tiWindow, UIScale);
end;

function TViewDialog.TDialog.CapturesEventsAtPosition(const Position: TVector2): boolean;
begin
  Result := true; // always capture
end;

procedure TViewDialog.TDialog.PreferredSize(var PreferredWidth, PreferredHeight: Single);
var
  PR: TFloatRectangle;
begin
  PR := ParentRect;
  PreferredWidth :=
    Min(MaxLineWidth + BoxMarginScaled * 2 +
      ScrollView.ScrollBarWidth * UIScale,
      PR.Width  - WindowMarginScaled * 2);
  PreferredHeight :=
    Min(Font.Height * AllScrolledLinesCount +
      Font.DescenderHeight + BoxMarginScaled * 2 + ButtonsHeightScaled
      { adding here + 2 is a hack to make sure that TCastleScrollView will
        not show scrollbars when not necessary. That's because we set
        ScrollView.ScrollArea.Height using a similar equation as above
        "Font.Height * AllScrolledLinesCount + Font.DescenderHeight...",
        but it's in unscaled size (/ UIScale), and sometimes (with wild UIScale
        values) it seems like scrollbars are needed (EffectiveHeight < ScrollArea.EffectiveHeight)
        even though they actually are not.

        Reproducible if you try to resize to small sizes the demo on
        examples/mobile/simple_3d_demo/ . }
      + 2,
      PR.Height - WindowMarginScaled * 2);
end;

function TViewDialog.TDialog.BoxMarginScaled: Single;
begin
  Result := BoxMargin * UIScale;
end;

function TViewDialog.TDialog.WindowMarginScaled: Single;
begin
  Result := WindowMargin * UIScale;
end;

function TViewDialog.TDialog.ButtonHorizontalMarginScaled: Single;
begin
  Result := ButtonHorizontalMargin * UIScale;
end;

{$endif read_implementation}
