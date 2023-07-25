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

  TCastleImageControl = class;

  { Base class for all user interface controls using a font.
    Allows to customize font and font size per-control, or use defaults. }
  TCastleUserInterfaceFont = class(TCastleUserInterface)
  strict private
    FTooltip: string;
    TooltipLabelContainer: TCastleImageControl;
    TooltipLabel: TCastleLabel;
    FCustomFont: TCastleAbstractFont;
    FOwnsCustomFont: boolean;
    FLastSeenUIFont, FLastSeenDefaultFont: TCastleAbstractFont; //< remembered only to call FontChanged
    FFontSize, FFontScale: Single;
    FCustomizedFont: TCastleFontFamily; //< used only when some properties make it necessary
    FSmallFont: boolean;
    FOutline: Cardinal;
    FOutlineColor: TCastleColor;
    FOutlineHighQuality: boolean;
    procedure SetCustomFont(const Value: TCastleAbstractFont);
    procedure SetFontSize(const Value: Single);
    procedure SetFontScale(const Value: Single);
    procedure SetSmallFont(const Value: boolean);
    procedure SetOutline(const Value: Cardinal);
    procedure SetOutlineColor(const Value: TCastleColor);
    procedure SetOutlineHighQuality(const Value: boolean);
    procedure CustomFontSizeChanged(Sender: TObject);
  protected
    procedure InternalSetTooltipExists(const Value: Boolean); override;
    procedure UIScaleChanged; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function TooltipExists: boolean; override;
    procedure TooltipRender(const TooltipPosition: TVector2); override;
    procedure Render; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Copy font properties from another control. }
    procedure AssignFont(const Source: TCastleUserInterfaceFont);

    { Font used for rendering and measuring this control.

      Depending on which one is set, this method returns
      @link(CustomFont) or @link(TCastleContainer.DefaultFont) or global @link(UIFont).
      It may also return a TCastleFontFamily instance that wraps (and scales)
      one of them. }
    function Font: TCastleAbstractFont;

    { Check does currently used font (see @link(Font)) changed,
      and eventually call FontChanged method @italic(now).

      You only need to explicitly call this in very specific circumstances,
      when you just changed @link(UIFont) or @link(TCastleContainer.DefaultFont)
      and you want control size to be updated immediately
      (for example, you need @link(EffectiveHeight)
      to be immediately valid). Without calling this,
      it could be updated only at next Render call.

      Changing @link(CustomFont) automatically calls this. }
    procedure CheckFontChanged;

    { Force considering font changed (font instance, or glyphs, or sizes changed).

      @italic(Usually, you don't need to call this explicitly from the outside.)
      Changing font, like @link(CustomFont)
      or @link(TCastleContainer.DefaultFont) or @link(UIFont),
      or any property that may affect the font is automatically applied.

      But there are exceptions: right now, if you call @link(TCastleFont.Load Load)
      on a font, it's measurements will change but some controls using it will
      not recalculate sizes automatically. In this case, call this method. }
    procedure FontChanged; virtual;

    {$ifdef FPC}
    property OwnsCustomFont: boolean
      read FOwnsCustomFont write FOwnsCustomFont default false;
      deprecated 'use TCastleAbstractFont (inherited from TComponent) owner mechanism';

    { Use smaller font.
      Right now this just says to scale font by 0.5. }
    property SmallFont: boolean read FSmallFont write SetSmallFont default false;
      deprecated 'use "FontScale := 0.5" or "CustomFont := MySmalFont" instead of "SmallFont := true", this is more flexible';
    {$endif}

    { Outline color, used only if Outline <> 0. Default is black.
      @seealso Outline }
    property OutlineColor: TCastleColor read FOutlineColor write SetOutlineColor;
  published
    { Tooltip string, displayed when user hovers the mouse over a control.

      Note that you can override TCastleUserInterface.TooltipExists and
      TCastleUserInterface.TooltipRender to customize the tooltip drawing. }
    property Tooltip: string read FTooltip write FTooltip;

    { Font used to render text of this control.
      When this is @nil then the @link(TCastleContainer.DefaultFont) is used,
      or (when @link(TCastleContainer.DefaultFont) is also @nil) the global @link(UIFont). }
    property CustomFont: TCastleAbstractFont
      read FCustomFont write SetCustomFont;

    { Use given font size when drawing. Leave at default zero to use
      the default size of the current font. }
    property FontSize: Single read FFontSize write SetFontSize {$ifdef FPC}default 0.0{$endif};

    { Scale the font. This additionally scales the font defined
      by all other properties of this class, thus you can use it to reliably
      scale the font (regardless if you use @link(CustomFont) or not,
      regardless if you set @link(FontSize) or not). }
    property FontScale: Single read FFontScale write SetFontScale {$ifdef FPC}default 1.0{$endif};

    { Outline size around the font.
      Note that the current implementation is very simple, it will only
      look sensible for small outline values (like 1 or 2).

      See TCastleAbstractFont.Outline for more details.

      Specifying a non-zero outline for a label overrides the default outline
      settings for the current font. By default, fonts do not have an outline.

      TODO: UI scaling (@link(TCastleContainer.UIScaling))
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
  Result := (TooltipControl <> nil) or (Tooltip <> '');
end;

procedure TCastleUserInterfaceFont.TooltipRender(const TooltipPosition: TVector2);
const
  TooltipPadding = 5;
var
  TooltipWidth, TooltipHeight: Single;
  Pos: TVector2;
begin
  // do not render tooltip here, if TooltipControl takes care of it
  if TooltipControl <> nil then
    Exit;
  if Container = nil then
    Exit; // we need Container to work

  if TooltipLabelContainer = nil then
  begin
    TooltipLabelContainer := TCastleImageControl.Create(Self);
    TooltipLabelContainer.Content.DrawableImage := Theme.ImagesPersistent[tiTooltip].DrawableImage;
    TooltipLabelContainer.Content.OwnsDrawableImage := false;
    TooltipLabelContainer.Content.ProtectedSides.Assign(Theme.ImagesPersistent[tiTooltip].ProtectedSides);
    TooltipLabelContainer.AutoSizeToChildren := true;
    TooltipLabelContainer.AutoSizeToChildrenPaddingRight := TooltipPadding;
    TooltipLabelContainer.AutoSizeToChildrenPaddingTop := TooltipPadding;
    TooltipLabelContainer.SetTransient; // in case this occurs at design-time
    Container.Controls.InsertFront(TooltipLabelContainer);
  end;

  if TooltipLabel = nil then
  begin
    TooltipLabel := TCastleLabelTooltip.Create(Self);
    TooltipLabel.Anchor(hpLeft, TooltipPadding);
    TooltipLabel.Anchor(vpBottom, TooltipPadding);
    TooltipLabelContainer.InsertFront(TooltipLabel);
  end;

  { assign TooltipLabel.Text first, to get TooltipLabelContainer.EffectiveXxx later }
  TooltipLabel.Color := Theme.TooltipTextColor;
  TooltipLabel.Text.Clear;
  TooltipLabel.Text.Append(Tooltip);

  TooltipWidth := TooltipLabelContainer.EffectiveWidth;
  TooltipHeight := TooltipLabelContainer.EffectiveHeight;

  Pos := TooltipPosition;

  { now try to fix X, Y to make tooltip fit inside a window }
  MinVar(Pos.X, ContainerWidth  / TooltipLabelContainer.UIScale - TooltipWidth );
  MinVar(Pos.Y, ContainerHeight / TooltipLabelContainer.UIScale - TooltipHeight);
  MaxVar(Pos.X, 0);
  MaxVar(Pos.Y, 0);

  TooltipLabelContainer.Translation := Pos;
end;

procedure TCastleUserInterfaceFont.InternalSetTooltipExists(const Value: Boolean);
begin
  inherited;
  if TooltipLabelContainer <> nil then
    TooltipLabelContainer.Exists := Value;
end;

function TCastleUserInterfaceFont.Font: TCastleAbstractFont;
begin
  { We check everywhere FontLoaded, to not use some font if it is not loaded
    (e.g. if freetype library not available, or font file failed to load),
    as it would not be visible.
    As a last fallback, we can always use UIFont. }
  if (CustomFont <> nil) and
     // TODO: hack for TCastleInspector, why needed?
     (CustomFont.ClassType <> TCastleAbstractFont) and
     (CustomFont.FontLoaded) then
    Result := CustomFont
  else
  if (Container <> nil) and
     (Container.DefaultFont <> nil) and
     (Container.DefaultFont.FontLoaded) then
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
     (GetMainContainer.DefaultFont <> nil) and
     (GetMainContainer.DefaultFont.FontLoaded) then
    Result := GetMainContainer.DefaultFont
  else
    Result := InternalGetUIFont;

  { if needed, wrap Result in TCastleFontFamily instance }
  if (FontSize <> 0) or
     (FontScale <> 1) or
     (UIScale <> 1) or
     {$ifdef FPC}
     {$warnings off} // using deprecated to keep it working
     SmallFont or
     {$warnings on}
     {$endif}
     (Outline <> 0) then
  begin
    if FCustomizedFont = nil then
      FCustomizedFont := TCastleFontFamily.Create(nil);
    if FontSize <> 0 then
      FCustomizedFont.Size := FontSize
    else
      FCustomizedFont.Size := Result.EffectiveSize; // to have something to multiply in line below
    FCustomizedFont.Size := FCustomizedFont.Size * UIScale * FontScale;
    {$ifdef FPC}
    {$warnings off} // using deprecated to keep it working
    if SmallFont then
      FCustomizedFont.Size := FCustomizedFont.Size * 0.5;
    {$warnings on}
    {$endif}
    if Result is TCastleFontFamily then
    begin
      { copying Result to FCustomizedFont this way allows to render with HTML
        when CustomFont and FontSize/FontScale are used together }
      FCustomizedFont.Regular := TCastleFontFamily(Result).Regular;
      FCustomizedFont.Bold := TCastleFontFamily(Result).Bold;
      FCustomizedFont.Italic := TCastleFontFamily(Result).Italic;
      FCustomizedFont.BoldItalic := TCastleFontFamily(Result).BoldItalic;
    end else
    begin
      FCustomizedFont.Regular := Result;
      FCustomizedFont.Bold := nil;
      FCustomizedFont.Italic := nil;
      FCustomizedFont.BoldItalic := nil;
    end;
    { When Outline = 0, set FCustomizedFont.CustomizeOutline = false,
      this makes switching between Outline <> 0 to Outline = 0 make correct changes
      -- outline should disappear. }
    FCustomizedFont.CustomizeOutline := Outline <> 0;
    if Outline <> 0 then
    begin
      FCustomizedFont.Outline := Outline;
      FCustomizedFont.OutlineHighQuality := OutlineHighQuality;
      FCustomizedFont.OutlineColor := OutlineColor;
    end;
    Result := FCustomizedFont;
  end;
end;

procedure TCastleUserInterfaceFont.SetCustomFont(const Value: TCastleAbstractFont);
begin
  if FCustomFont <> Value then
  begin
    if FCustomFont <> nil then
    begin
      FCustomFont.RemoveFreeNotification(Self);
      FCustomFont.RemoveFontSizeChangeNotification({$ifdef FPC}@{$endif}CustomFontSizeChanged);
    end;
    {$ifdef FPC}
    {$warnings off}
    if OwnsCustomFont then
      FreeAndNil(FCustomFont) else
      FCustomFont := nil;
    {$warnings on}
    {$endif}
    FCustomFont := Value;
    if FCustomFont <> nil then
    begin
      FCustomFont.FreeNotification(Self);
      { We need to be notified about font size changes,
        in particular when new font file is loaded in TCastleFont.
        Testcase: assing to TCastleLabel.CustomFont some TCastleFont,
        then change the TCastleFont.URL (without changing anything
        at TCastleLabel directly) -- the TCastleLabel size should change. }
      FCustomFont.AddFontSizeChangeNotification({$ifdef FPC}@{$endif}CustomFontSizeChanged);
    end;
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
    {$ifdef FPC}
    {$warnings off}
    OwnsCustomFont := false;
    {$warnings on}
    {$endif}
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
  { Changing font properties, like FontSize, may change the control size
    (which is usually adjusted to font size,
    e.g. TCastleLabel, TCastleButton with AutoSize). }
  VisibleChange([chRectangle]);
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
  NewUIFont, NewDefaultFont: TCastleAbstractFont;
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
  if (PropertyName = 'FontSize') or
     (PropertyName = 'CustomFont') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleUserInterfaceFont.CustomFontSizeChanged(Sender: TObject);
begin
  FontChanged;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleuserinterfacefont_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
