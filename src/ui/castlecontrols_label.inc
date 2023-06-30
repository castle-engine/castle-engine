{%MainUnit castlecontrols.pas}
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

  { Label with possibly multiline text.
    See @url(https://castle-engine.io/manual_text.php
    manual about displaying text and fonts). }
  TCastleLabel = class(TCastleUserInterfaceFont)
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
    FMaxDisplayChars: Integer;
    CachedRichText: TRichText;
    FCaptionTranslate: Boolean;
    function GetTextToRender: TRichText;
    procedure SetAutoSize(const Value: boolean);
    function GetCaption: String;
    procedure SetCaption(const Value: String);
    procedure SetAlignment(const Value: THorizontalPosition);
    procedure SetVerticalAlignment(const Value: TVerticalPosition);
    procedure SetMaxDisplayChars(const Value: Integer);
    procedure SetMaxWidth(const Value: Single);
    procedure TextChange(Sender: TObject);
    procedure SetText(const Value: TStrings);
    procedure SetPaddingHorizontal(const Value: Single);
    procedure SetPaddingVertical(const Value: Single);
    procedure SetPadding(const Value: Single);
    procedure SetHtml(const Value: Boolean);
  private
    { For internal use by tooltip rendering. In normal circumstances,
      leave this at tiLabel. }
    ImageType: TThemeImage;
  protected
    procedure PreferredSize(var PreferredWidth, PreferredHeight: Single); override;
    function GetInternalText: String; override;
    procedure SetInternalText(const Value: String); override;
    procedure UIScaleChanged; override;
    procedure TranslateProperties(const TranslatePropertyEvent: TTranslatePropertyEvent); override;
    procedure Loaded; override;
  public
    const
      DefaultLineSpacing = 2;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    procedure EditorAllowResize(
      out ResizeWidth, ResizeHeight: Boolean; out Reason: String); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    procedure FontChanged; override;

    { Text color. By default it's opaque black. }
    property Color: TCastleColor read FColor write FColor;

    { Color tint of the background image, see @link(Frame). By default white. }
    property FrameColor: TCastleColor read FFrameColor write FFrameColor;
      {$ifdef FPC}deprecated 'display frame around label using Border or parent UI control';{$endif}

    function DisplayChars: Cardinal;
  published
    { Should we automatically adjust size to the text size.
      The size of the label determines where does it display the @link(Frame),
      where does it catch events, to what width is it aligned (see @link(Alignment))
      and so on.

      When this is @true (the default) then
      @link(Width), @link(Height), @link(FullSize) values are ignored. }
    property AutoSize: boolean read FAutoSize write SetAutoSize default true;

    { Caption displayed on the label, each line as a String.
      Setting this property merely copies the contents using TStrings.Assign. }
    property Text: TStrings read FText write SetText;

    { Caption displayed on the label.
      This is just a shortcut to get/set @link(Text) as a single String.

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
    property Caption: String read GetCaption write SetCaption stored false;

    { Inside the label rectangle, padding between rect borders and text.
      Total horizontal padding is the sum @code(PaddingHorizontal + Padding),
      total vertical padding is the sum @code(PaddingVertical + Padding).
      @groupBegin }
    property PaddingHorizontal: Single
      read FPaddingHorizontal write SetPaddingHorizontal {$ifdef FPC}default 0{$endif};
    property PaddingVertical: Single
      read FPaddingVertical write SetPaddingVertical {$ifdef FPC}default 0{$endif};
    property Padding: Single
      read FPadding write SetPadding {$ifdef FPC}default 0{$endif};
    { @groupEnd }

    { Extra spacing between lines.
      May be negative to squeeze lines tighter together. }
    property LineSpacing: Single read FLineSpacing write FLineSpacing {$ifdef FPC}default DefaultLineSpacing{$endif};

    { Does the text use HTML markup.
      This allows to easily change colors or use bold, italic text.

      See the example examples/fonts/html_text.lpr and
      examples/fonts/html_text_demo.html for a demo of what HTML tags can do.
      See @link(TCastleAbstractFont.PrintStrings) documentation for a list of support HTML markup.

      Note that to see the bold/italic font variants in the HTML markup,
      you need to set the font to be TCastleFontFamily with bold/italic variants.
      See the example mentioned above, examples/fonts/html_text.lpr,
      for a code how to do it. }
    property Html: boolean read FHtml write SetHtml default false;

    {$ifdef FPC}
    property Tags: boolean read FHtml write SetHtml stored false default false;
      deprecated 'use Html instead';
    {$endif}

    { Draw frame around the text. Frame uses theme image tiLabel,
      see TCastleTheme.Images if you want to customize it. }
    property Frame: boolean read FFrame write FFrame default false;
      {$ifdef FPC}deprecated 'display frame around label using Border or parent UI control';{$endif}

    { If non-zero, limit the width of resulting label.
      The text will be broken in the middle of lines, to make it fit
      (together with @link(PaddingHorizontal)) inside MaxWidth. }
    property MaxWidth: Single read FMaxWidth write SetMaxWidth {$ifdef FPC}default 0{$endif};

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

    { Should the @link(Caption) be localized (translated into other languages).
      Determines if the property is enumerated by @link(TCastleComponent.TranslateProperties),
      which affects the rest of localization routines. }
    property CaptionTranslate: Boolean read FCaptionTranslate write FCaptionTranslate default true;

  {$warnings off} // silence the fact that FrameColor is deprecated
  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlelabel_persistent_vectors.inc}
  {$undef read_interface_class}
  {$warnings on}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleLabel --------------------------------------------------------------- }

constructor TCastleLabel.Create(AOwner: TComponent);
begin
  inherited;

  FText := TStringList.Create;
  TStringList(FText).OnChange := {$ifdef FPC}@{$endif} TextChange;
  {$warnings off}
  if Theme.DefaultLabelWhite then
  {$warnings on}
    FColor := White
  else
    FColor := Black;
  FFrame := false;
  FFrameColor := White;
  FLineSpacing := DefaultLineSpacing;
  FAutoSize := true;
  ImageType := tiLabel;
  FMaxDisplayChars := -1;
  FCaptionTranslate := true;

  {$warnings off} // silence the fact that FrameColor is deprecated
  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlelabel_persistent_vectors.inc}
  {$undef read_implementation_constructor}
  {$warnings on}
end;

destructor TCastleLabel.Destroy;
begin
  {$warnings off} // silence the fact that FrameColor is deprecated
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastlelabel_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  {$warnings on}

  FreeAndNil(FText);
  FreeAndNil(CachedRichText);

  inherited;
end;

function TCastleLabel.GetTextToRender: TRichText;
var
  PaddingHorizontalScaled, MaxWidthScaled, WrapWidth: Single;
  US: Single;
begin
  if CachedRichText <> nil then
    { Since GetTextToRender is called very often, we want it to usually return
      instantly fast. }
    Result := CachedRichText
  else
  begin
    Result := TRichText.Create(Font, Text, Html);
    if MaxWidth <> 0 then
    begin
      US := UIScale;
      PaddingHorizontalScaled := US * (PaddingHorizontal + Padding);
      MaxWidthScaled := US * MaxWidth;
      WrapWidth := MaxWidthScaled - 2 * PaddingHorizontalScaled;
      if WrapWidth > 0 then
        Result.Wrap(WrapWidth);
    end;
    CachedRichText := Result;
  end;
end;

procedure TCastleLabel.PreferredSize(var PreferredWidth, PreferredHeight: Single);
var
  TextToRender: TRichText;
  TextToRenderWidth: Single;
  { FPC: TStrings.Count is Integer, TObjectList.Count is SizeInt
    Delphi: TStrings.Count is Integer, TObjectList.Count is Integer so we use
    TListSize in TextToRenderCount }
  TextToRenderCount: TListSize;
  PaddingHorizontalScaled, PaddingVerticalScaled, LineSpacingScaled: Single;
  US: Single;
begin
  inherited;

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
      TextToRenderWidth := TextToRender.Width;
      TextToRenderCount := TextToRender.Count;
    end;

    US := UIScale;
    PaddingHorizontalScaled := US * (PaddingHorizontal + Padding);
    PaddingVerticalScaled := US * (PaddingVertical + Padding);
    { When text is empty there is 0 lines so we need check that
      not simply LineSpacingScaled * (TextToRenderCount - 1)}
    if TextToRenderCount = 0 then
      LineSpacingScaled := 0
    else
      LineSpacingScaled := US * LineSpacing * (TextToRenderCount - 1);

    PreferredWidth := TextToRenderWidth + 2 * PaddingHorizontalScaled;
    PreferredHeight :=
      Font.Height * TextToRenderCount +
      LineSpacingScaled +
      2 * PaddingVerticalScaled +
      { This additional Font.DescenderHeight is actually always an empty space on top of
        the text. So we do not have to increase PreferredHeight by this Font.DescenderHeight,
        strictly speaking (if we treat PreferredHeight as a "minimum bounding box height").

        But adding it makes the text looks nicer centered vertically in the bbox.
        Visually you have an ~empty space at the bottom (not really empty,
        but usually empty) of height Font.DescenderHeight and empty space at top
        (really always empty) of height Font.DescenderHeight.

        So anchoring this TCastleLabel with vpMiddle to something else looks better. }
      Font.DescenderHeight;
  end;
end;

procedure TCastleLabel.EditorAllowResize(
  out ResizeWidth, ResizeHeight: Boolean; out Reason: String);
begin
  inherited;
  if AutoSize then
  begin
    ResizeWidth := false;
    ResizeHeight := false;
    Reason := SAppendPart(Reason, NL, 'Turn off "TCastleLabel.AutoSize" to change size.');
  end;
end;

procedure TCastleLabel.Render;
var
  TextToRender: TRichText;
  LineSpacingScaled: Single;

  function TextHeight: Single;
  begin
    Result := TextToRender.Count * (Font.Height + LineSpacingScaled);
  end;

var
  SR: TFloatRectangle;
  TextX, TextBottom, PaddingHorizontalScaled, PaddingVerticalScaled: Single;
  US: Single;
begin
  inherited;
  if Text.Count = 0 then Exit; // early exit in case of easy, fast case

  SR := RenderRect;
  US := UIScale;
  PaddingHorizontalScaled := US * (PaddingHorizontal + Padding);
  PaddingVerticalScaled := US * (PaddingVertical + Padding);
  LineSpacingScaled := US * LineSpacing;

  {$warnings off} // using deprecated just to keep it working
  if Frame then
    Theme.Draw(SR, ImageType, UIScale, FrameColor);
  {$warnings on}

  { calculate TextX }
  case Alignment of
    hpLeft  : TextX := SR.Left + PaddingHorizontalScaled;
    hpMiddle: TextX := (SR.Left + SR.Right) / 2;
    hpRight : TextX := SR.Right - PaddingHorizontalScaled;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TCastleLabel.Render: Alignment?');
    {$endif}
  end;

  { calculate TextBottom }
  TextBottom := SR.Bottom + PaddingVerticalScaled + Font.DescenderHeight;

  if (not Html) and (MaxWidth = 0) and
     (AutoSize or (VerticalAlignment = vpBottom)) and
     (MaxDisplayChars = -1) then
  begin
    { fast case: no need to use TRichText in this case }
    Font.PrintStrings(TextX, TextBottom, Color, Text, false, LineSpacingScaled, Alignment);
  end else
  begin
    TextToRender := GetTextToRender;

    { fix TextBottom, in case of non-trivial VerticalAlignment }
    if not AutoSize then
      case VerticalAlignment of
        vpMiddle: TextBottom := SR.Bottom + (SR.Height - TextHeight) / 2;
        vpTop   : TextBottom := SR.Top - PaddingVerticalScaled - Font.DescenderHeight - TextHeight;
        else ;
      end;

    TextToRender.Print(TextX, TextBottom, Color, LineSpacingScaled, Alignment,
      MaxDisplayChars);
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

function TCastleLabel.GetCaption: String;
begin
  Result := TrimEndingNewline(Text.Text);
end;

procedure TCastleLabel.SetCaption(const Value: String);
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

procedure TCastleLabel.SetMaxWidth(const Value: Single);
begin
  if FMaxWidth <> Value then
  begin
    FMaxWidth := Value;
    // invalidate CachedRichText, as it depends on this property
    FreeAndNil(CachedRichText);
    VisibleChange([chRectangle]);
  end;
end;

function TCastleLabel.DisplayChars: Cardinal;
var
  TextToRender: TRichText;
begin
  if Text.Count = 0 then Exit(0); // early exit in case of easy, fast case

  TextToRender := GetTextToRender;
  Result := TextToRender.DisplayChars;
end;

procedure TCastleLabel.TextChange(Sender: TObject);
begin
  VisibleChange([chRectangle]);
  // invalidate CachedRichText, as it depends on Text value
  FreeAndNil(CachedRichText);
end;

procedure TCastleLabel.SetText(const Value: TStrings);
begin
  FText.Assign(Value);
end;

function TCastleLabel.GetInternalText: String;
begin
  Result := Caption;
end;

procedure TCastleLabel.SetInternalText(const Value: String);
begin
  Caption := Value;
end;

function TCastleLabel.PropertySections(const PropertyName: String): TPropertySections;
begin
  // Note that Caption expresses the same thing as Text, but easier to access
  if (PropertyName = 'Text') or
     (PropertyName = 'Caption') or
     (PropertyName = 'MaxWidth') or
     (PropertyName = 'ColorPersistent') or
     (PropertyName = 'Html') then
    Result := [psBasic]
  else if (PropertyName = 'Alignment') or
          (PropertyName = 'VerticalAlignment') or
          (PropertyName = 'LineSpacing') or
          (PropertyName = 'AutoSize') then
    Result := [psBasic, psLayout]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleLabel.SetPaddingHorizontal(const Value: Single);
begin
  if FPaddingHorizontal <> Value then
  begin
    FPaddingHorizontal := Value;
    // invalidate CachedRichText, as it depends on this property
    FreeAndNil(CachedRichText);
    VisibleChange([chRender]);
  end;
end;

procedure TCastleLabel.SetPaddingVertical(const Value: Single);
begin
  if FPaddingVertical <> Value then
  begin
    FPaddingVertical := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleLabel.SetPadding(const Value: Single);
begin
  if FPadding <> Value then
  begin
    FPadding := Value;
    // invalidate CachedRichText, as it depends on this property
    FreeAndNil(CachedRichText);
    VisibleChange([chRender]);
  end;
end;

procedure TCastleLabel.SetHtml(const Value: Boolean);
begin
  if FHtml <> Value then
  begin
    FHtml := Value;
    // invalidate CachedRichText, as it depends on this property
    FreeAndNil(CachedRichText);
    VisibleChange([chRender]);
  end;
end;

procedure TCastleLabel.UIScaleChanged;
begin
  inherited;
  // invalidate CachedRichText, as it depends on UIScale
  FreeAndNil(CachedRichText);
end;

procedure TCastleLabel.FontChanged;
begin
  inherited;
  // invalidate CachedRichText, as it depends on Font
  FreeAndNil(CachedRichText);
end;

procedure TCastleLabel.TranslateProperties(
  const TranslatePropertyEvent: TTranslatePropertyEvent);
var
  S: String;
begin
  if CaptionTranslate and (Caption <> '') then
  begin
    S := Caption;
    TranslatePropertyEvent(Self, 'Caption', S);
    Caption := S;
  end;
end;

procedure TCastleLabel.Loaded;
begin
  inherited;
  {$warnings off} // using deprecated just to make a warning
  if Frame or (not TVector4.PerfectlyEquals(White, FrameColor)) then
    WritelnWarning('TCastleLabel.Frame and FrameColor are deprecated. Instead: Use TCastleLabel.Border, or place TCastleLabel under a component like TCastleRectangleControl or TCastleImageControl that defines a background.');
  {$warnings on}
end;

{$warnings off} // silence the fact that FrameColor is deprecated
{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlelabel_persistent_vectors.inc}
{$undef read_implementation_methods}
{$warnings on}

{$endif read_implementation}
