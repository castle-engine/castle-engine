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

  { Progress bar user interface.
    This is usually used through the CastleWindowProgress and CastleProgress
    features. There is no need to directly create and access the TCastleProgressBar
    instance in this case. }
  TCastleProgressBar = class(TCastleUserInterfaceFont)
  strict private
    { Background image. }
    FBackground: TCastleImage;
    FDrawableBackground: TDrawableImage;
    FYPosition: Single;
    FProgress: TProgress;
    procedure SetBackground(const Value: TCastleImage);
  protected
    procedure PreferredSize(var PreferredWidth, PreferredHeight: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;

    property HorizontalAnchorSelf default hpMiddle;
    property HorizontalAnchorParent default hpMiddle;
    property VerticalAnchorSelf default vpMiddle;
    property VerticalAnchorParent default vpMiddle;

    { Progress that rules the position and title displayed. }
    property Progress: TProgress read FProgress write FProgress;

    { Background drawn under the progress bar.
      When it's assigned, it's always drawn scaled to cover the whole
      screen (container). It's owner by this component (it will be automatically
      freed when necessary). }
    property Background: TCastleImage read FBackground write SetBackground;

    { Vertical positon of the progress bar.
      0 means the middle of progress bar is at the bottom of the container,
      1 means at the top. 0.5 indicates the middle, and it's the default.

      Note that extreme values (0 or 1) mean that bottom or top half
      of the progress bar doesn't fit on the screen, as this property
      positions the middle of the progress bar. }
    property YPosition: Single read FYPosition write FYPosition
      {$ifdef FPC}default TProgressUserInterface.DefaultBarYPosition{$endif};
  end deprecated 'use own UI to display progress bar (e.g. one rectangle using WidthFraction within a bigger rectangle)';

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleProgressBar --------------------------------------------------------- }

const
  Dots = '...';

{ Make Text shorter to fit the text width (as rendered using Font)
  inside MaxWidth (in pixels). }
procedure MakeTextFit(var Text: string; const Font: TCastleAbstractFont;
  const MaxWidth: Integer);
var
  DotsWidth: Single;

  { Make Text shorter by preserving first and last words, inserting
    dots inside, and preserving as much as possible text between first and last
    words. }
  function TrimIntelligent: boolean;
  begin
    Result := false;

    { Not implemented for now, not needed. The idea of algorithm below.
      Separator characters are whitespace or / or \. They include slash
      and backslash, to work nicely with URLs and filenames, to show
      the last (usually most relevant) part of URL / filename.

      Find first separator in Text
      if not found, exit false

      Find last separator in Text
      if not found (should not happen) or <= first separator, exit false

      Prefix := Text up to and including first separator
      Suffix := Text suffix, including last separator

      NewWidth := Font.TextWidth(Prefix) + Font.TextWidth(Suffix) + DotsWidth;
      if NewWidth > MaxWidth then exit false

      // We know that we're OK now, using Prefix + ... + Suffix is good.
      // See how many additional characters we can add and still fit in MaxWidth.
      Result := true;
      NextIndex := Length(Prefix) + 1;
      while NextIndex < LastSeparator then
        PotentialPrefix := Prefix + Text[NextIndex]
        PotentialNewWidth := NewWidth + Font.TextWidth(Text[NextIndex])
        if PotentialNewWidth > MaxWidth then Break;
        NewWidth := PotentialNewWidth;
        Prefix := PotentialPrefix;
      end;
      Text := Prefix + Dots + Suffix;
    }
  end;

  { Make Text shorter by taking as long prefix as possible to fit
    the prefix + Dots. }
  procedure TrimSimple;
  var
    NewTextDotsWidth, PotentialNewTextDotsWidth: Single;
    NewText, PotentialNewText: string;
    C: char;
  begin
    NewText := '';
    NewTextDotsWidth := DotsWidth;
    while Length(NewText) < Length(Text) do
    begin
      C := Text[Length(NewText) + 1];
      PotentialNewText := NewText + C;
      PotentialNewTextDotsWidth := NewTextDotsWidth + Font.TextWidth(C);
      if PotentialNewTextDotsWidth > MaxWidth then Break;
      NewText := PotentialNewText;
      NewTextDotsWidth := PotentialNewTextDotsWidth;
    end;
    Text := NewText + Dots;
  end;

var
  TextWidth: Single;
begin
  TextWidth := Font.TextWidth(Text);
  if TextWidth <= MaxWidth then
  begin
    { No trimming needs to be done. Add dots at the end, if we have space. }
    if Font.TextWidth(Text + Dots) < MaxWidth then
      Text := Text + Dots;
    Exit;
  end;

  DotsWidth := Font.TextWidth(Dots);

  if not TrimIntelligent then
    TrimSimple;
end;

procedure TCastleProgressBar.PreferredSize(var PreferredWidth, PreferredHeight: Single);
var
  XMargin, AHeight: Single;
  PR: TFloatRectangle;
begin
  inherited;

  PR := ParentRect;
  XMargin := PR.Width / 8;
  AHeight := PR.Height / 12;

  PreferredWidth := PR.Width - 2 * XMargin;
  PreferredHeight := AHeight;
end;

procedure TCastleProgressBar.Render;
const
  PaddingHorizontal = 20;
  MinPaddingVertical = 4;
var
  MaxTextWidth: Integer;
  Caption: string;
  BarRect, FillRect: TFloatRectangle;
  HeightForText: Single;
begin
  inherited;

  if Progress = nil then Exit;

  // TODO: control should not draw over parent this way
  if FDrawableBackground <> nil then
    FDrawableBackground.Draw(ParentRect);

  BarRect := RenderRect;
  Theme.Draw(BarRect, tiProgressBar, UIScale);

  FillRect := BarRect.LeftPart(BarRect.Width * Progress.Position / Progress.Max);
  Theme.Draw(FillRect, tiProgressFill, UIScale);

  MaxTextWidth := Round(BarRect.Width - PaddingHorizontal);
  Caption := Progress.Title;

  Font.PushProperties;
  HeightForText := BarRect.Height - 2 * MinPaddingVertical;
  if Font.RowHeight > HeightForText then
    Font.Size := Font.Size / (Font.RowHeight / HeightForText);
  MakeTextFit(Caption, Font, MaxTextWidth);
  Font.Print(BarRect.Left + PaddingHorizontal,
    BarRect.Bottom + Round(BarRect.Height - Font.RowHeight) div 2,
    Theme.TextColor, Caption);
  Font.PopProperties;
end;

constructor TCastleProgressBar.Create(AOwner: TComponent);
begin
  inherited;
  FYPosition := TProgressUserInterface.DefaultBarYPosition;
  Anchor(hpMiddle);
  Anchor(vpMiddle);
end;

destructor TCastleProgressBar.Destroy;
begin
  FreeAndNil(FBackground);
  FreeAndNil(FDrawableBackground);
  inherited;
end;

procedure TCastleProgressBar.SetBackground(const Value: TCastleImage);
begin
  if FBackground <> Value then
  begin
    FreeAndNil(FBackground);
    FBackground := Value;

    if FDrawableBackground = nil then
      FDrawableBackground := TDrawableImage.Create(Value, true, false)
    else
      FDrawableBackground.Load(Value);
  end;
end;

{$endif read_implementation}
