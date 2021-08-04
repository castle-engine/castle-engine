{%MainUnit castlecontrols.pas}
{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Abstract ancestor for containers that pack children,
    like @link(TCastleHorizontalGroup) and @link(TCastleVerticalGroup). }
  TCastlePackedGroup = class(TCastleUserInterface)
  strict private
    FFrame: boolean;
    FFrameColor: TCastleColor;
    FPadding: Single;
    FSpacing: Single;
    FInsideWidth, FInsideHeight: Single;
    FAutoSizeWidth, FAutoSizeHeight: Boolean;
    procedure SetFrameColor(const Value: TCastleColor);
    procedure SetFrame(const Value: Boolean);
    procedure SetPadding(const Value: Single);
    procedure SetSpacing(const Value: Single);
    procedure SetAutoSizeWidth(const Value: Boolean);
    procedure SetAutoSizeHeight(const Value: Boolean);
    procedure PackChildren;
  protected
    FPackingValid: boolean;
    procedure DoPackChildren(out InsideWidth, InsideHeight: Single); virtual; abstract;
    procedure PreferredSize(var PreferredWidth, PreferredHeight: Single); override;
  public
    const
      DefaultPadding = 0;
      DefaultSpacing = 0;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeRender; override;
    procedure Render; override;
    procedure VisibleChange(const Changes: TCastleUserInterfaceChanges;
      const ChangeInitiatedByChildren: boolean = false); override;
    procedure EditorAllowResize(out ResizeWidth, ResizeHeight: Boolean;
      out Reason: String); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { If @link(Frame), this is the frame's color. By default white. }
    property FrameColor: TCastleColor read FFrameColor write SetFrameColor;
  published
    property Frame: boolean read FFrame write SetFrame default false;
    property Padding: Single read FPadding write SetPadding default DefaultPadding;
    property Spacing: Single read FSpacing write SetSpacing default DefaultSpacing;

    { Whether the control's size is automatically adjusted to wrap children
      tightly.

      Be careful to not create a recursive loop! Group's size calculation
      always looks at children size (even when auto-size is off,
      since it needs to arrange children next to each other anyway).
      This means that children size @italic(cannot) depend on a parent.
      So a direct child of @link(TCastleVerticalGroup) or
      @link(TCastleHorizontalGroup) @italic(cannot)
      use @link(FullSize), or @link(WidthFraction) or @link(HeightFraction).
      This would create a loop in calculations, which would be forcefully
      resolved by some control having undefined size.

      So, you can only deactivate auto-size on a group if all children
      have explicitly defined size using their @link(Width) and @link(Height)
      properties.
    }
    property AutoSizeWidth: Boolean
      read FAutoSizeWidth write SetAutoSizeWidth default true;
    property AutoSizeHeight: Boolean
      read FAutoSizeHeight write SetAutoSizeHeight default true;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlepackedgroup_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  { Container that packs the children horizontally.

    The children are packed from left-to-right by default,
    in the order determined by their back-to-front relation.
    E.g. if you add all the children using @link(InsertFront),
    then they will appear in the same order, left-to-right, in the control.
    You can invert this by setting @link(LeftToRight) to @false. }
  TCastleHorizontalGroup = class(TCastlePackedGroup)
  strict private
    FAlignment: TVerticalPosition;
    FLeftToRight: boolean;
    procedure SetLeftToRight(const Value: boolean);
    procedure SetAlignment(const Value: TVerticalPosition);
  protected
    procedure DoPackChildren(out W, H: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    { The alignment of children inside this control. }
    property Alignment: TVerticalPosition
      read FAlignment write SetAlignment default vpMiddle;
    property LeftToRight: boolean read FLeftToRight write SetLeftToRight
      default true;
  end;

  { Container that packs the children vertically.

    The children are packed from top-to-bottom,
    in the order determined by their back-to-front relation.
    E.g. if you add all the children using @link(InsertFront),
    then they will appear in the same order, top-to-bottom, in the control.
    You can invert this by setting @link(TopToBottom) to @false. }
  TCastleVerticalGroup = class(TCastlePackedGroup)
  strict private
    FAlignment: THorizontalPosition;
    FTopToBottom: boolean;
    procedure SetTopToBottom(const Value: boolean);
    procedure SetAlignment(const Value: THorizontalPosition);
  protected
    procedure DoPackChildren(out W, H: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    { The alignment of children inside this control. }
    property Alignment: THorizontalPosition
      read FAlignment write SetAlignment default hpLeft;
    property TopToBottom: boolean read FTopToBottom write SetTopToBottom
      default true;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastlePackedGroup --------------------------------------------------------- }

constructor TCastlePackedGroup.Create(AOwner: TComponent);
begin
  inherited;
  FFrameColor := White;
  FPadding := DefaultPadding;
  FSpacing := DefaultSpacing;
  FAutoSizeWidth := true;
  FAutoSizeHeight := true;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlepackedgroup_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastlePackedGroup.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastlepackedgroup_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

procedure TCastlePackedGroup.BeforeRender;
begin
  inherited;
  { We prefer to pack inside PreferredSize, to set size early correctly.
    But in case group has FullSize, the PreferredSize may never be called. }
  PackChildren;
end;

procedure TCastlePackedGroup.Render;
begin
  inherited;
  if Frame then
    Theme.Draw(RenderRect, tiGroup, UIScale, FrameColor);
end;

procedure TCastlePackedGroup.PackChildren;
begin
  if FPackingValid then Exit;
  { setting this to true early also prevents recursively entering DoPackChildren. }
  FPackingValid := true;
  DoPackChildren(FInsideWidth, FInsideHeight);
end;

procedure TCastlePackedGroup.PreferredSize(var PreferredWidth, PreferredHeight: Single);
begin
  inherited;
  PackChildren;
  if FAutoSizeWidth then
    PreferredWidth  := FInsideWidth  * UIScale;
  if FAutoSizeHeight then
    PreferredHeight := FInsideHeight * UIScale;
end;

procedure TCastlePackedGroup.EditorAllowResize(
  out ResizeWidth, ResizeHeight: Boolean; out Reason: String);
begin
  inherited;
  if AutoSizeWidth then
  begin
    ResizeWidth := false;
    Reason := SAppendPart(Reason, NL, 'Turn off "TCastlePackedGroup.AutoSizeWidth" to change width.');
  end;
  if AutoSizeHeight then
  begin
    ResizeHeight := false;
    Reason := SAppendPart(Reason, NL, 'Turn off "TCastlePackedGroup.AutoSizeHeight" to change height.');
  end;
end;

procedure TCastlePackedGroup.SetFrameColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FFrameColor, Value) then
  begin
    FFrameColor := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastlePackedGroup.SetFrame(const Value: boolean);
begin
  if FFrame <> Value then
  begin
    FFrame := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastlePackedGroup.SetPadding(const Value: Single);
begin
  if FPadding <> Value then
  begin
    FPadding := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastlePackedGroup.SetSpacing(const Value: Single);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastlePackedGroup.VisibleChange(const Changes: TCastleUserInterfaceChanges;
  const ChangeInitiatedByChildren: boolean);
begin
  inherited;
  if [chRectangle, chChildren, chChildrenExists] * Changes <> [] then
    FPackingValid := false;
end;

procedure TCastlePackedGroup.SetAutoSizeWidth(const Value: Boolean);
begin
  if FAutoSizeWidth <> Value then
  begin
    FAutoSizeWidth := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastlePackedGroup.SetAutoSizeHeight(const Value: Boolean);
begin
  if FAutoSizeHeight <> Value then
  begin
    FAutoSizeHeight := Value;
    VisibleChange([chRectangle]);
  end;
end;

function TCastlePackedGroup.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'Spacing') then
    Result := [psBasic, psLayout]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlepackedgroup_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastleHorizontalGroup ----------------------------------------------------- }

constructor TCastleHorizontalGroup.Create(AOwner: TComponent);
begin
  inherited;
  FAlignment := vpMiddle;
  FLeftToRight := true;
end;

procedure TCastleHorizontalGroup.SetLeftToRight(const Value: boolean);
begin
  if FLeftToRight <> Value then
  begin
    FLeftToRight := Value;
    FPackingValid := false;
  end;
end;

procedure TCastleHorizontalGroup.SetAlignment(const Value: TVerticalPosition);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    FPackingValid := false;
  end;
end;

procedure TCastleHorizontalGroup.DoPackChildren(out W, H: Single);
var
  I: Integer;
  C: TCastleUserInterface;
  AlignmentDelta: Single;
begin
  W := Padding;
  H := 0;

  for I := 0 to ControlsCount - 1 do
  begin
    C := Controls[I];
    if not C.GetExists then Continue;

    if LeftToRight then
      C.Anchor(hpLeft, W)
    else
      C.Anchor(hpRight, -W);

    case Alignment of
      vpBottom: AlignmentDelta := Padding;
      vpTop   : AlignmentDelta := -Padding;
      vpMiddle: AlignmentDelta := 0;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('TCastleHorizontalGroup.DoPackChildren:Alignment');
      {$endif}
    end;
    C.Anchor(Alignment, AlignmentDelta);

    W := W + C.EffectiveWidth + Spacing;
    MaxVar(H, C.EffectiveHeight);
  end;

  W := W - Spacing + Padding;
  H := H + 2 * Padding;
end;

function TCastleHorizontalGroup.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'Alignment') then
    Result := [psBasic, psLayout]
  else
    Result := inherited PropertySections(PropertyName);
end;

{ TCastleVerticalGroup ----------------------------------------------------- }

constructor TCastleVerticalGroup.Create(AOwner: TComponent);
begin
  inherited;
  FAlignment := hpLeft;
  FTopToBottom := true;
end;

procedure TCastleVerticalGroup.SetTopToBottom(const Value: boolean);
begin
  if FTopToBottom <> Value then
  begin
    FTopToBottom := Value;
    FPackingValid := false;
  end;
end;

procedure TCastleVerticalGroup.SetAlignment(const Value: THorizontalPosition);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    FPackingValid := false;
  end;
end;

procedure TCastleVerticalGroup.DoPackChildren(out W, H: Single);
var
  I: Integer;
  C: TCastleUserInterface;
  AlignmentDelta: Single;
begin
  W := 0;
  H := Padding;

  for I := 0 to ControlsCount - 1 do
  begin
    C := Controls[I];
    if not C.GetExists then Continue;

    case Alignment of
      hpLeft  : AlignmentDelta := Padding;
      hpRight : AlignmentDelta := -Padding;
      hpMiddle: AlignmentDelta := 0;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('TCastleVerticalGroup.DoPackChildren:Alignment');
      {$endif}
    end;
    C.Anchor(Alignment, AlignmentDelta);

    if TopToBottom then
      C.Anchor(vpTop, -H)
    else
      C.Anchor(vpBottom, H);

    MaxVar(W, C.EffectiveWidth);
    H := H + C.EffectiveHeight + Spacing;
  end;

  W := W + 2 * Padding;
  H := H - Spacing + Padding;
end;

function TCastleVerticalGroup.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'Alignment') then
    Result := [psBasic, psLayout]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$endif read_implementation}
