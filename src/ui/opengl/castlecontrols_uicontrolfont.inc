{
  Copyright 2010-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Base class for all user interface controls using a font.
    Allows to customize font and font size per-control, or use defaults. }
  TUIControlFont = class(TUIControl)
  strict private
    FTooltip: string;
    TooltipLabel: TCastleLabel;
    FCustomFont: TCastleFont;
    FOwnsCustomFont: boolean;
    FLastSeenUIFontXxx: TCastleFont; //< remembered only to call FontChanged
    FFontSize: Single;
    FCustomizedFont: TFontFamily; //< used only when some properties make it necessary
    FSmallFont: boolean;
    FOutline: Cardinal;
    FOutlineColor: TCastleColor;
    FOutlineHighQuality: boolean;
    procedure SetCustomFont(const Value: TCastleFont);
    procedure SetFontSize(const Value: Single);
    procedure SetSmallFont(const Value: boolean);
    procedure SetOutline(const Value: Cardinal);
    procedure SetOutlineColor(const Value: TCastleColor);
    procedure SetOutlineHighQuality(const Value: boolean);
  protected
    procedure UIScaleChanged; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
    procedure GLContextClose; override;
    function TooltipExists: boolean; override;
    procedure TooltipRender; override;
    procedure Render; override;

    { Font used for rendering and measuring this control.

      Depending on various properties
      (@(SmallFont), @link(CustomFont), @link(FontSize), @link(TUIControl.UIScale))
      it may return @link(UIFont), @link(UIFontSmall), or @link(CustomFont),
      or a TCustomizedFont instances that wraps (and scales) one of the above. }
    function Font: TCastleFont;

    { Check does currently used font (see @link(Font)) changed,
      and eventually call FontChanged method @italic(now).

      You only need to explicitly call this in very specific circumstances,
      when you just changed @link(UIFont) or @link(UIFontSmall) (as changing
      @link(CustomFont) automatically
      immediately calls this) and you want control size to be updated
      immediately (for example, you need @link(TCastleButton.Height)
      to be immediately valid). Without calling this,
      it could be updated only at next Render call. }
    procedure CheckFontChanged;

    { Force considering font changed (font instance, or glyphs, or sizes changed).

      @italic(Usually, you don't need to call this explicitly from the outside.)
      Changing font, like @link(UIFont) or @link(UIFontSmall) or @link(CustomFont)
      or any property that may affect the font is automatically applied.

      But there are exceptions: right now, if you call @link(TTextureFont.Load Load)
      on a font, it's measurements will change but some controls using it will
      not recalculate sizes automatically. In this case, call this method. }
    procedure FontChanged; virtual;

    { Outline color, used only if Outline <> 0. Default is black.
      @seealso Outline }
    property OutlineColor: TCastleColor read FOutlineColor write SetOutlineColor;
  published
    { Tooltip string, displayed when user hovers the mouse over a control.

      Note that you can override TUIControl.TooltipExists and
      TUIControl.TooltipStyle and TUIControl.TooltipRender
      to customize the tooltip drawing. }
    property Tooltip: string read FTooltip write FTooltip;

    { When non-nil, this font will be used to draw this control.
      Otherwise the global @link(UIFont) or @link(UIFontSmall)
      (depending on @link(SmallFont)) will be used. }
    property CustomFont: TCastleFont
      read FCustomFont write SetCustomFont;
    property OwnsCustomFont: boolean
      read FOwnsCustomFont write FOwnsCustomFont default false;
      deprecated 'use TCastleFont (inherited from TComponent) owner mechanism';

    { Use given font size when drawing. Leave at default zero to use
      the default size of the font (may still be scaled by @link(TUIControl.UIScale),
      but this happens only if @link(TUIContainer.UIScaling) is set to
      something else than default usNone).
      Font itself is taken from
      @link(CustomFont) or, if not set, from @link(UIFont) or @link(UIFontSmall). }
    property FontSize: Single read FFontSize write SetFontSize default 0.0;

    { Use smaller font.
      If no @link(CustomFont) is assigned, this says to use
      @link(UIFontSmall) instead of @link(UIFont).
      If @link(CustomFont) is assigned, this says to scale it's size by 2. }
    property SmallFont: boolean read FSmallFont write SetSmallFont default false;

    { Outline size around the font.
      Note that the current implementation is very simple, it will only
      look sensible for small outline values (like 1 or 2).

      See TCastleFont.Outline for more details.

      Specifying a non-zero outline for a label overrides the default outline
      settings for the current font. By default, fonts do not have an outline.

      @seealso OutlineHighQuality }
    property Outline: Cardinal read FOutline write SetOutline default 0;

    { Optionally force better outline quality. Used only if Outline <> 0.
      High quality outline looks better, but is about 2x more expensive to draw.
      @seealso Outline }
    property OutlineHighQuality: boolean
      read FOutlineHighQuality write SetOutlineHighQuality default false;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TUIControlFont ---------------------------------------------------------- }

destructor TUIControlFont.Destroy;
begin
  CustomFont := nil; // make sure to free FCustomFont, if necessary
  FreeAndNil(FCustomizedFont);
  inherited;
end;

function TUIControlFont.TooltipExists: boolean;
begin
  Result := Tooltip <> '';
end;

procedure TUIControlFont.TooltipRender;
var
  X, Y: Integer;
  TooltipRect: TRectangle;
begin
  if TooltipLabel = nil then
  begin
    TooltipLabel := TCastleLabel.Create(nil);
    TooltipLabel.ImageType := tiTooltip;
    TooltipLabel.Frame := true;
    TooltipLabel.Container := Container;
    { we know that GL context now exists, so just directly call GLContextOpen }
    TooltipLabel.GLContextOpen;
  end;

  { assign TooltipLabel.Text first, to get TooltipRect.Width/Height }
  TooltipLabel.Padding := 5;
  TooltipLabel.Color := Theme.TooltipTextColor;
  TooltipLabel.Text.Clear;
  TooltipLabel.Text.Append(Tooltip);
  TooltipRect := TooltipLabel.Rect;

  { divide by TooltipLabel.UIScale because TooltipLabel.Rect will multiply by it }
  X := Round(Container.TooltipPosition[0] / TooltipLabel.UIScale);
  Y := Round(Container.TooltipPosition[1] / TooltipLabel.UIScale);

  { now try to fix X, Y to make tooltip fit inside a window }
  MinVar(X, ContainerWidth - TooltipRect.Width);
  MinVar(Y, ContainerHeight - TooltipRect.Height);
  MaxVar(X, 0);
  MaxVar(Y, 0);

  TooltipLabel.Left := X;
  TooltipLabel.Bottom := Y;

  { just explicitly call Render method of TooltipLabel }
  TooltipLabel.Render;
end;

procedure TUIControlFont.GLContextClose;
begin
  { make sure to call GLContextClose on TooltipLabel,
    actually we can just free it now }
  FreeAndNil(TooltipLabel);
  inherited;
end;

function TUIControlFont.Font: TCastleFont;
begin
  if CustomFont <> nil then
    Result := CustomFont else
    Result := UIFont;

  { if FontSize or UIScale are non-trivial,
    wrap Result in TCustomizedFont instance }
  if (FFontSize <> 0) or (UIScale <> 1) or SmallFont or (Outline <> 0) then
  begin
    if FCustomizedFont = nil then
      FCustomizedFont := TFontFamily.Create(nil);
    if FFontSize <> 0 then
      FCustomizedFont.Size := FFontSize else
      FCustomizedFont.Size := Result.RealSize; // to have something to multiply in line below
    FCustomizedFont.Size := FCustomizedFont.Size * UIScale;
    if SmallFont then
      FCustomizedFont.Size := FCustomizedFont.Size * 0.5;
    if Result is TFontFamily then
    begin
      { copying Result to FCustomizedFont this way allows to render with HTML
        when CustomFont and FontSize are used together }
      FCustomizedFont.RegularFont := TFontFamily(Result).RegularFont;
      FCustomizedFont.BoldFont := TFontFamily(Result).BoldFont;
      FCustomizedFont.ItalicFont := TFontFamily(Result).ItalicFont;
      FCustomizedFont.BoldItalicFont := TFontFamily(Result).BoldItalicFont;
    end else
    begin
      FCustomizedFont.RegularFont := Result;
      FCustomizedFont.BoldFont := nil;
      FCustomizedFont.ItalicFont := nil;
      FCustomizedFont.BoldItalicFont := nil;
    end;
    if Outline <> 0 then
    begin
      FCustomizedFont.CustomizeOutline := true;
      FCustomizedFont.Outline := Outline;
      FCustomizedFont.OutlineHighQuality := OutlineHighQuality;
      FCustomizedFont.OutlineColor := OutlineColor;
    end;
    Result := FCustomizedFont;
  end;
end;

procedure TUIControlFont.SetCustomFont(const Value: TCastleFont);
begin
  if FCustomFont <> Value then
  begin
    if FCustomFont <> nil then
      FCustomFont.RemoveFreeNotification(Self);
    {$warnings off}
    if OwnsCustomFont then
      FreeAndNil(FCustomFont) else
      FCustomFont := nil;
    {$warnings on}
    FCustomFont := Value;
    if FCustomFont <> nil then
      FCustomFont.FreeNotification(Self);
    { don't call virtual function, that may try to measure the text by accessing Font
      (needlessly using UIFont, possibly even recreating UIFont if "finalization"
      of this unit already run, possibly even accessing invalid freed
      TextureFontData). }
    if not (csDestroying in ComponentState) then
      FontChanged;
  end;
end;

procedure TUIControlFont.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) and (AComponent = FCustomFont) then
  begin
    { set to nil by SetCustomFont to clean nicely }
    { since it's already being freed, do not apply OwnsCustomFont effect
      inside SetCustomFont. }
    {$warnings off}
    OwnsCustomFont := false;
    {$warnings on}
    CustomFont := nil;
  end;
end;

procedure TUIControlFont.SetFontSize(const Value: Single);
begin
  if FFontSize <> Value then
  begin
    FFontSize := Value;
    FontChanged;
  end;
end;

procedure TUIControlFont.SetSmallFont(const Value: boolean);
begin
  if FSmallFont <> Value then
  begin
    FSmallFont := Value;
    FontChanged;
  end;
end;

procedure TUIControlFont.SetOutline(const Value: Cardinal);
begin
  if FOutline <> Value then
  begin
    FOutline := Value;
    FontChanged;
  end;
end;

procedure TUIControlFont.SetOutlineColor(const Value: TCastleColor);
begin
  if not VectorsPerfectlyEqual(FOutlineColor, Value) then
  begin
    FOutlineColor := Value;
    FontChanged;
  end;
end;

procedure TUIControlFont.SetOutlineHighQuality(const Value: boolean);
begin
  if FOutlineHighQuality <> Value then
  begin
    FOutlineHighQuality := Value;
    FontChanged;
  end;
end;

procedure TUIControlFont.FontChanged;
begin
end;

procedure TUIControlFont.UIScaleChanged;
begin
  inherited;
  FontChanged;
end;

procedure TUIControlFont.Render;
begin
  inherited;
  CheckFontChanged;
end;

procedure TUIControlFont.CheckFontChanged;
var
  DefaultFont: TCastleFont;
begin
  { we do not use here UIFont[Small] but FUIFont[Small],
    to not create font without need. If the corresponding global font is nil now,
    it can remain nil, no need to initialize it yet. }
  if SmallFont then
    DefaultFont := FUIFontSmall else
    DefaultFont := FUIFont;

  if (CustomFont = nil) and (FLastSeenUIFontXxx <> DefaultFont) then
    FontChanged;
  FLastSeenUIFontXxx := DefaultFont;
end;

{$endif read_implementation}
