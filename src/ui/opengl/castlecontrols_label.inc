{
  Copyright 2010-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Label with possibly multiline text, in an optional box. }
  TCastleLabel = class(TUIControlFont)
  strict private
    FText: TStrings;
    FPaddingHorizontal, FPaddingVertical, FPadding: Single;
    FLineSpacing: Single;
    FColor: TCastleColor;
    FHtml: boolean;
    FFrame: boolean;
    FFrameColor: TCastleColor;
    FMaxWidth: Single;
    FAlignment: THorizontalPosition;
    FVerticalAlignment: TVerticalPosition;
    FAutoSize: boolean;
    FFloatWidth, FFloatHeight: Single;
    FFullSize: boolean;
    FMaxDisplayChars: Integer;
    function GetTextToRender: TRichText;
    function GetWidth: Cardinal;
    function GetHeight: Cardinal;
    procedure SetWidth(const Value: Cardinal);
    procedure SetHeight(const Value: Cardinal);
    procedure SetFloatWidth(const Value: Single);
    procedure SetFloatHeight(const Value: Single);
    procedure SetFullSize(const Value: boolean);
    procedure SetAutoSize(const Value: boolean);
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    procedure SetAlignment(const Value: THorizontalPosition);
    procedure SetVerticalAlignment(const Value: TVerticalPosition);
    procedure SetMaxDisplayChars(const Value: Integer);
  private
    { For internal use by tooltip rendering. In normal circumstances,
      leave this at tiLabel. }
    ImageType: TThemeImage;
  public
    const
      DefaultLineSpacing = 2;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    function Rect: TRectangle; override;
    function FloatRect: TFloatRectangle; override;

    { Text color. By default it's white. }
    property Color: TCastleColor read FColor write FColor;

    { Color tint of the background image, see @link(Frame). By default white. }
    property FrameColor: TCastleColor read FFrameColor write FFrameColor;

    function DisplayChars: Cardinal;
  published
    property Width: Cardinal read GetWidth write SetWidth default 0;
    property Height: Cardinal read GetHeight write SetHeight default 0;
    property FloatWidth: Single read FFloatWidth write SetFloatWidth default 0;
    property FloatHeight: Single read FFloatHeight write SetFloatHeight default 0;
    property FullSize: boolean read FFullSize write SetFullSize default false;

    { Should we automatically adjust size to the text size.
      The size of the label determines where does it display the @link(Frame),
      where does it catch events, to what width is it aligned (see @link(Alignment))
      and so on.

      When this is @true (the default) then
      @link(Width), @link(Height), @link(FullSize) values are ignored. }
    property AutoSize: boolean read FAutoSize write SetAutoSize default true;

    { Caption displayed on the label, each line as a string. }
    property Text: TStrings read FText;

    { Caption displayed on the label.
      This is just a shortcut to get/set @link(Text) as a single string.

      Use LineEnding or NL constant when setting this to indicate a newline.
      The two examples below are equivalent:

      @longCode(#
        // one way
        Label1.Text.Clear;
        Label1.Text.Add('First line');
        Label1.Text.Add('Second line');

        // alternative way to do the same
        Label1.Caption := 'First line' + LineEnding + 'Second line';
      #)
    }
    property Caption: string read GetCaption write SetCaption stored false;

    { Inside the label rectangle, padding between rect borders and text.
      Total horizontal padding is the sum @code(PaddingHorizontal + Padding),
      total vertical padding is the sum @code(PaddingVertical + Padding).
      @groupBegin }
    property PaddingHorizontal: Single
      read FPaddingHorizontal write FPaddingHorizontal default 0;
    property PaddingVertical: Single
      read FPaddingVertical write FPaddingVertical default 0;
    property Padding: Single
      read FPadding write FPadding default 0;
    { @groupEnd }

    { Extra spacing between lines.
      May be negative to squeeze lines tighter together. }
    property LineSpacing: Single read FLineSpacing write FLineSpacing default DefaultLineSpacing;

    { Does the text use HTML markup.
      This allows to easily change colors or use bold, italic text.

      See the example examples/fonts/html_text.lpr and
      examples/fonts/html_text_demo.html for a demo of what HTML tags can do.
      See @link(TCastleFont.PrintStrings) documentation for a list of support HTML markup.

      Note that to see the bold/italic font variants in the HTML markup,
      you need to set the font to be TFontFamily with bold/italic variants.
      See the example mentioned above, examples/fonts/html_text.lpr,
      for a code how to do it. }
    property Html: boolean read FHtml write FHtml default false;

    property Tags: boolean read FHtml write FHtml stored false default false;
      deprecated 'use Html instead';

    { Draw frame around the text. Frame uses theme image tiLabel,
      see TCastleTheme.Images if you want to customize it. }
    property Frame: boolean read FFrame write FFrame default false;

    { If non-zero, limit the width of resulting label.
      The text will be broken in the middle of lines, to make it fit
      (together with @link(PaddingHorizontal)) inside MaxWidth. }
    property MaxWidth: Single read FMaxWidth write FMaxWidth default 0;

    { Horizontal alignment of the text. }
    property Alignment: THorizontalPosition
      read FAlignment write SetAlignment default hpLeft;

    { Vertical alignment of the text. Usually you don't want to use this,
      instead leave @link(AutoSize) = @true and align the label to the parent
      using anchors, like @code(MyLabel.Anchor(vpMiddle);)
      or @code(MyLabel.Anchor(vpTop);).

      This property is useful if you really need to manually control the size.
      It only matters when @link(AutoSize) is @false.
      Then it controls where the text is, with respect to it's rectangle defined
      by properties like @link(Height) or @link(FullSize). }
    property VerticalAlignment: TVerticalPosition
      read FVerticalAlignment write SetVerticalAlignment default vpBottom;

    { Limit the displayed label text, if not -1.
      This doesn't affect the label size, only the rendered text.
      It's nice to show the animation of text "expanding", filling some area.
      Use DisplayChars as the maximum sensible value for this. }
    property MaxDisplayChars: Integer
      read FMaxDisplayChars write SetMaxDisplayChars default -1;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleLabel --------------------------------------------------------------- }

constructor TCastleLabel.Create(AOwner: TComponent);
begin
  inherited;

  FText := TStringList.Create;
  FColor := White;
  FFrame := false;
  FFrameColor := White;
  FLineSpacing := DefaultLineSpacing;
  FAutoSize := true;
  ImageType := tiLabel;
  FMaxDisplayChars := -1;
end;

destructor TCastleLabel.Destroy;
begin
  FreeAndNil(FText);

  inherited;
end;

function TCastleLabel.GetTextToRender: TRichText;
var
  PaddingHorizontalScaled, MaxWidthScaled, WrapWidth: Single;
  US: Single;
begin
  Result := TRichText.Create(Font, Text, Html);
  if MaxWidth <> 0 then
  begin
    US := UIScale;
    PaddingHorizontalScaled := US * PaddingHorizontal;
    MaxWidthScaled := US * MaxWidth;
    WrapWidth := MaxWidthScaled - 2 * PaddingHorizontalScaled;
    if WrapWidth > 0 then
      Result.Wrap(WrapWidth);
  end;
end;

function TCastleLabel.Rect: TRectangle;
begin
  Result := FloatRect.Round;
end;

function TCastleLabel.FloatRect: TFloatRectangle;
var
  TextToRender: TRichText;
  TextToRenderWidth, TextToRenderCount: Cardinal;
  PaddingHorizontalScaled, PaddingVerticalScaled, LineSpacingScaled: Single;
  US: Single;
begin
  if AutoSize then
  begin
    if (not Html) and (MaxWidth = 0) then
    begin
      { fast case: no need to use TRichText in this case }
      TextToRenderWidth := Font.MaxTextWidth(Text);
      TextToRenderCount := Text.Count;
    end else
    begin
      TextToRender := GetTextToRender;
      try
        TextToRenderWidth := TextToRender.Width;
        TextToRenderCount := TextToRender.Count;
      finally FreeAndNil(TextToRender) end;
    end;

    US := UIScale;
    PaddingHorizontalScaled := US * (PaddingHorizontal + Padding);
    PaddingVerticalScaled := US * (PaddingVertical + Padding);
    LineSpacingScaled := US * LineSpacing;
    Result := FloatRectangle(
      FloatLeftBottomScaled,
      TextToRenderWidth + 2 * PaddingHorizontalScaled,
      Font.RowHeight * TextToRenderCount +
      LineSpacingScaled * (TextToRenderCount - 1) +
      2 * PaddingVerticalScaled +
      Font.Descend);
  end else
  if FullSize then
    Result := ParentFloatRect else
  begin
    Result := FloatRectangle(Left, Bottom, FloatWidth, FloatHeight);
    Result := Result.ScaleAround0(UIScale);
  end;
end;

procedure TCastleLabel.Render;
var
  TextToRender: TRichText;
  LineSpacingScaled: Single;

  function TextHeight: Single;
  begin
    Result := TextToRender.Count * (Font.RowHeight + LineSpacingScaled);
  end;

var
  SR: TFloatRectangle;
  TextX, TextBottom, PaddingHorizontalScaled, PaddingVerticalScaled: Single;
  US: Single;
begin
  inherited;
  if Text.Count = 0 then Exit; // early exit in case of easy, fast case

  SR := ScreenFloatRect;
  US := UIScale;
  PaddingHorizontalScaled := US * (PaddingHorizontal + Padding);
  PaddingVerticalScaled := US * (PaddingVertical + Padding);
  LineSpacingScaled := US * LineSpacing;
  if Frame then
    Theme.Draw(SR, ImageType, UIScale, FrameColor);

  { calculate TextX }
  case Alignment of
    hpLeft  : TextX := SR.Left + PaddingHorizontalScaled;
    hpMiddle: TextX := (SR.Left + SR.Right) / 2;
    hpRight : TextX := SR.Right - PaddingHorizontalScaled;
    else raise EInternalError.Create('TCastleLabel.Render: Alignment?');
  end;

  { calculate TextBottom }
  TextBottom := SR.Bottom + PaddingVerticalScaled + Font.Descend;

  if (not Html) and (MaxWidth = 0) and
     (AutoSize or (VerticalAlignment = vpBottom)) and
     (MaxDisplayChars = -1) then
  begin
    { fast case: no need to use TRichText in this case }
    Font.PrintStrings(TextX, TextBottom, Color, Text, false, LineSpacingScaled, Alignment);
  end else
  begin
    TextToRender := GetTextToRender;
    try
      { fix TextBottom, in case of non-trivial VerticalAlignment }
      if not AutoSize then
        case VerticalAlignment of
          vpMiddle: TextBottom := SR.Bottom + (SR.Height - TextHeight) / 2;
          vpTop   : TextBottom := SR.Top - PaddingVerticalScaled - Font.Descend - TextHeight;
        end;

      TextToRender.Print(TextX, TextBottom, Color, LineSpacingScaled, Alignment,
        MaxDisplayChars);
    finally FreeAndNil(TextToRender) end;
  end;
end;

function TCastleLabel.GetWidth: Cardinal;
begin
  Result := Round(FloatWidth);
end;

function TCastleLabel.GetHeight: Cardinal;
begin
  Result := Round(FloatHeight);
end;

procedure TCastleLabel.SetWidth(const Value: Cardinal);
begin
  FloatWidth := Value;
end;

procedure TCastleLabel.SetHeight(const Value: Cardinal);
begin
  FloatHeight := Value;
end;

procedure TCastleLabel.SetFloatWidth(const Value: Single);
begin
  if FFloatWidth <> Value then
  begin
    FFloatWidth := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleLabel.SetFloatHeight(const Value: Single);
begin
  if FFloatHeight <> Value then
  begin
    FFloatHeight := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleLabel.SetFullSize(const Value: boolean);
begin
  if FFullSize <> Value then
  begin
    FFullSize := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleLabel.SetAutoSize(const Value: boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    VisibleChange([chRectangle]);
  end;
end;

function TCastleLabel.GetCaption: string;
begin
  Result := Text.Text;
end;

procedure TCastleLabel.SetCaption(const Value: string);
begin
  Text.Text := Value;
end;

procedure TCastleLabel.SetAlignment(const Value: THorizontalPosition);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleLabel.SetVerticalAlignment(const Value: TVerticalPosition);
begin
  if FVerticalAlignment <> Value then
  begin
    FVerticalAlignment := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleLabel.SetMaxDisplayChars(const Value: Integer);
begin
  if FMaxDisplayChars <> Value then
  begin
    FMaxDisplayChars := Value;
    VisibleChange([chRectangle]);
  end;
end;

function TCastleLabel.DisplayChars: Cardinal;
var
  TextToRender: TRichText;
begin
  if Text.Count = 0 then Exit(0); // early exit in case of easy, fast case

  TextToRender := GetTextToRender;
  try
    Result := TextToRender.DisplayChars;
  finally FreeAndNil(TextToRender) end;
end;

{$endif read_implementation}
