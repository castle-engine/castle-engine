{%MainUnit castlecontrols.pas}
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

  { Base class for all user interface controls using a font.
    Allows to customize font and font size per-control, or use defaults. }
  TCastleUserInterfaceFont = class(TCastleUserInterface)
  strict private
    FTooltip: string;
    TooltipLabel: TCastleLabel;
    FCustomFont: TCastleFont;
    FOwnsCustomFont: boolean;
    FLastSeenUIFont, FLastSeenDefaultFont: TCastleFont; //< remembered only to call FontChanged
    FFontSize, FFontScale: Single;
    FCustomizedFont: TFontFamily; //< used only when some properties make it necessary
    FSmallFont: boolean;
    FOutline: Cardinal;
    FOutlineColor: TCastleColor;
    FOutlineHighQuality: boolean;
    procedure SetCustomFont(const Value: TCastleFont);
    procedure SetFontSize(const Value: Single);
    procedure SetFontScale(const Value: Single);
    procedure SetSmallFont(const Value: boolean);
    procedure SetOutline(const Value: Cardinal);
    procedure SetOutlineColor(const Value: TCastleColor);
    procedure SetOutlineHighQuality(const Value: boolean);
  protected
    procedure UIScaleChanged; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GLContextClose; override;
    function TooltipExists: boolean; override;
    procedure TooltipRender(const TooltipPosition: TVector2); override;
    procedure Render; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Copy font properties from another control. }
    procedure AssignFont(const Source: TCastleUserInterfaceFont);

    { Font used for rendering and measuring this control.

      Depending on which one is set, this method returns
      @link(CustomFont) or @link(TUIContainer.DefaultFont) or global @link(UIFont).
      It may also return a TFontFamily instance that wraps (and scales)
      one of them. }
    function Font: TCastleFont;

    { Check does currently used font (see @link(Font)) changed,
      and eventually call FontChanged method @italic(now).

      You only need to explicitly call this in very specific circumstances,
      when you just changed @link(UIFont) or @link(TUIContainer.DefaultFont)
      and you want control size to be updated immediately
      (for example, you need @link(EffectiveHeight)
      to be immediately valid). Without calling this,
      it could be updated only at next Render call.

      Changing @link(CustomFont) automatically calls this. }
    procedure CheckFontChanged;

    { Force considering font changed (font instance, or glyphs, or sizes changed).

      @italic(Usually, you don't need to call this explicitly from the outside.)
      Changing font, like @link(CustomFont)
      or @link(TUIContainer.DefaultFont) or @link(UIFont),
      or any property that may affect the font is automatically applied.

      But there are exceptions: right now, if you call @link(TTextureFont.Load Load)
      on a font, it's measurements will change but some controls using it will
      not recalculate sizes automatically. In this case, call this method. }
    procedure FontChanged; virtual;

    property OwnsCustomFont: boolean
      read FOwnsCustomFont write FOwnsCustomFont default false;
      deprecated 'use TCastleFont (inherited from TComponent) owner mechanism';

    { Use smaller font.
      Right now this just says to scale font by 0.5. }
    property SmallFont: boolean read FSmallFont write SetSmallFont default false;
      {$ifdef FPC} deprecated 'use "FontScale := 0.5" or "CustomFont := MySmalFont" instead of "SmallFont := true", this is more flexible'; {$endif}

    { Outline color, used only if Outline <> 0. Default is black.
      @seealso Outline }
    property OutlineColor: TCastleColor read FOutlineColor write SetOutlineColor;
  published
    { Tooltip string, displayed when user hovers the mouse over a control.

      Note that you can override TCastleUserInterface.TooltipExists and
      TCastleUserInterface.TooltipRender to customize the tooltip drawing. }
    property Tooltip: string read FTooltip write FTooltip;

    { When non-nil, this font will be used to draw this control.
      Otherwise the @link(TUIContainer.DefaultFont) will be used,
      or the global @link(UIFont). }
    property CustomFont: TCastleFont
      read FCustomFont write SetCustomFont;

    { Use given font size when drawing. Leave at default zero to use
      the default size of the current font. }
    property FontSize: Single read FFontSize write SetFontSize default 0.0;

    { Scale the font. This additionally scales the font defined
      by all other properties of this class, thus you can use it to reliably
      scale the font (regardless if you use @link(CustomFont) or not,
      regardless if you set @link(FontSize) or not). }
    property FontScale: Single read FFontScale write SetFontScale default 1.0;

    { Outline size around the font.
      Note that the current implementation is very simple, it will only
      look sensible for small outline values (like 1 or 2).

      See TCastleFont.Outline for more details.

      Specifying a non-zero outline for a label overrides the default outline
      settings for the current font. By default, fonts do not have an outline.

      TODO: UI scaling (@link(TUIContainer.UIScaling))
      doesn't affect outline size now.

      @seealso OutlineHighQuality }
    property Outline: Cardinal read FOutline write SetOutline default 0;

    { Optionally force better outline quality. Used only if Outline <> 0.
      High quality outline looks better, but is about 2x more expensive to draw.
      @seealso Outline }
    property OutlineHighQuality: boolean
      read FOutlineHighQuality write SetOutlineHighQuality default false;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastleuserinterfacefont_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  TUIControlFont = TCastleUserInterfaceFont deprecated 'use TCastleUserInterfaceFont';

{$endif read_interface}

{$ifdef read_implementation}

type
  TCastleLabelTooltip = class(TCastleLabel)
  public
    procedure PreferredSize(var PreferredWidth, PreferredHeight: Single); override;
  end;

procedure TCastleLabelTooltip.PreferredSize(
  var PreferredWidth, PreferredHeight: Single);
begin
  inherited;
end;

{ TCastleUserInterfaceFont ---------------------------------------------------------- }

constructor TCastleUserInterfaceFont.Create(AOwner: TComponent);
begin
  inherited;
  FFontScale := 1.0;
  FOutlineColor := Black;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastleuserinterfacefont_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleUserInterfaceFont.Destroy;
begin
  CustomFont := nil; // make sure to free FCustomFont, if necessary
  FreeAndNil(FCustomizedFont);

  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastleuserinterfacefont_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

function TCastleUserInterfaceFont.TooltipExists: boolean;
begin
  Result := Tooltip <> '';
end;

procedure TCastleUserInterfaceFont.TooltipRender(const TooltipPosition: TVector2);
var
  X, Y, TooltipWidth, TooltipHeight: Single;
begin
  if TooltipLabel = nil then
  begin
    TooltipLabel := TCastleLabelTooltip.Create(nil);
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
  TooltipWidth := 0;
  TooltipHeight := 0;
  TCastleLabelTooltip(TooltipLabel).PreferredSize(TooltipWidth, TooltipHeight);

  { divide by TooltipLabel.UIScale because TooltipLabel.Rect will multiply by it }
  X := TooltipPosition[0] / TooltipLabel.UIScale;
  Y := TooltipPosition[1] / TooltipLabel.UIScale;

  { now try to fix X, Y to make tooltip fit inside a window }
  MinVar(X, ContainerWidth  - TooltipWidth );
  MinVar(Y, ContainerHeight - TooltipHeight);
  MaxVar(X, 0);
  MaxVar(Y, 0);

  TooltipLabel.Anchor(hpLeft, X);
  TooltipLabel.Anchor(vpBottom, Y);

  { just explicitly call Render method of TooltipLabel }
  TooltipLabel.Render;
end;

procedure TCastleUserInterfaceFont.GLContextClose;
begin
  { make sure to call GLContextClose on TooltipLabel,
    actually we can just free it now }
  FreeAndNil(TooltipLabel);
  inherited;
end;

function TCastleUserInterfaceFont.Font: TCastleFont;
begin
  if CustomFont <> nil then
    Result := CustomFont
  else
  if (Container <> nil) and
     (Container.DefaultFont <> nil) then
    Result := Container.DefaultFont
  else
  { If the Container is not assigned yet, then assume that UI control
    will be inserted to the GetMainContainer, and use GetMainContainer.DefaultFont
    (which is typically adjusted using CastleSettings.xml).
    This makes calling e.g. TCastleLabel.Font.Text before the TCastleLabel instance
    is inserted into the Container sensible -- it will measure using the right font
    (with right sizes, right characters).
    Testcase: Escape warnings about invalid chars in Japanese version. }
  if (Container = nil) and
     (GetMainContainer <> nil) and
     (GetMainContainer.DefaultFont <> nil) then
    Result := GetMainContainer.DefaultFont
  else
    Result := UIFont;

  { if needed, wrap Result in TFontFamily instance }
  if (FontSize <> 0) or
     (FontScale <> 1) or
     (UIScale <> 1) or
     {$warnings off} // using deprecated to keep it working
     SmallFont or
     {$warnings on}
     (Outline <> 0) then
  begin
    if FCustomizedFont = nil then
      FCustomizedFont := TFontFamily.Create(nil);
    if FontSize <> 0 then
      FCustomizedFont.Size := FontSize
    else
      FCustomizedFont.Size := Result.EffectiveSize; // to have something to multiply in line below
    FCustomizedFont.Size := FCustomizedFont.Size * UIScale * FontScale;
    {$warnings off} // using deprecated to keep it working
    if SmallFont then
      FCustomizedFont.Size := FCustomizedFont.Size * 0.5;
    {$warnings on}
    if Result is TFontFamily then
    begin
      { copying Result to FCustomizedFont this way allows to render with HTML
        when CustomFont and FontSize/FontScale are used together }
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

procedure TCastleUserInterfaceFont.SetCustomFont(const Value: TCastleFont);
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

procedure TCastleUserInterfaceFont.Notification(AComponent: TComponent; Operation: TOperation);
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

procedure TCastleUserInterfaceFont.SetFontSize(const Value: Single);
begin
  if FFontSize <> Value then
  begin
    FFontSize := Value;
    FontChanged;
  end;
end;

procedure TCastleUserInterfaceFont.SetFontScale(const Value: Single);
begin
  if FFontScale <> Value then
  begin
    FFontScale := Value;
    FontChanged;
  end;
end;

procedure TCastleUserInterfaceFont.SetSmallFont(const Value: boolean);
begin
  if FSmallFont <> Value then
  begin
    FSmallFont := Value;
    FontChanged;
  end;
end;

procedure TCastleUserInterfaceFont.SetOutline(const Value: Cardinal);
begin
  if FOutline <> Value then
  begin
    FOutline := Value;
    FontChanged;
  end;
end;

procedure TCastleUserInterfaceFont.SetOutlineColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FOutlineColor, Value) then
  begin
    FOutlineColor := Value;
    FontChanged;
  end;
end;

procedure TCastleUserInterfaceFont.SetOutlineHighQuality(const Value: boolean);
begin
  if FOutlineHighQuality <> Value then
  begin
    FOutlineHighQuality := Value;
    FontChanged;
  end;
end;

procedure TCastleUserInterfaceFont.FontChanged;
begin
end;

procedure TCastleUserInterfaceFont.UIScaleChanged;
begin
  inherited;
  FontChanged;
end;

procedure TCastleUserInterfaceFont.Render;
begin
  inherited;
  CheckFontChanged;
end;

procedure TCastleUserInterfaceFont.CheckFontChanged;
var
  NewUIFont, NewDefaultFont: TCastleFont;
begin
  { we do not use here UIFont but FUIFont, to not create font without need.
    If the corresponding global font is nil now,
    it can remain nil, no need to initialize it yet. }
  NewUIFont := FUIFont;

  if Container <> nil then
    NewDefaultFont := Container.DefaultFont
  else
    NewDefaultFont := nil;

  if (CustomFont = nil) and
     ( (FLastSeenUIFont <> NewUIFont) or
       (FLastSeenDefaultFont <> NewDefaultFont) ) then
    FontChanged;

  FLastSeenUIFont := NewUIFont;
  FLastSeenDefaultFont := NewDefaultFont;
end;

procedure TCastleUserInterfaceFont.AssignFont(const Source: TCastleUserInterfaceFont);
begin
  CustomFont := Source.CustomFont;
  FontSize := Source.FontSize;
  FontScale := Source.FontScale;
end;

function TCastleUserInterfaceFont.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'FontSize') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleuserinterfacefont_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
