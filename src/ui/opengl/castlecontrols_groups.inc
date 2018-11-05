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
    procedure SetFrameColor(const Value: TCastleColor);
    procedure SetFrame(const Value: boolean);
    procedure SetPadding(const Value: Single);
    procedure SetSpacing(const Value: Single);
  strict protected
    FPackingValid: boolean;
    procedure DoPackChildren(out InsideWidth, InsideHeight: Single); virtual; abstract;
  protected
    procedure PreferredSize(var PreferredWidth, PreferredHeight: Single); override;
  public
    const
      DefaultPadding = 10;
      DefaultSpacing = 10;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    procedure VisibleChange(const Changes: TCastleUserInterfaceChanges;
      const ChangeInitiatedByChildren: boolean = false); override;

    { If @link(Frame), this is the frame's color. By default white. }
    property FrameColor: TCastleColor read FFrameColor write SetFrameColor;
  published
    property Frame: boolean read FFrame write SetFrame default false;
    property Padding: Single read FPadding write SetPadding default DefaultPadding;
    property Spacing: Single read FSpacing write SetSpacing default DefaultSpacing;

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
  strict protected
    procedure DoPackChildren(out W, H: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    { The alignment of children inside this control. }
    property Alignment: TVerticalPosition
      read FAlignment write FAlignment default vpMiddle;
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
  strict protected
    procedure DoPackChildren(out W, H: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    { The alignment of children inside this control. }
    property Alignment: THorizontalPosition
      read FAlignment write FAlignment default hpLeft;
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

procedure TCastlePackedGroup.Render;
begin
  inherited;
  if Frame then
    Theme.Draw(RenderRect, tiGroup, UIScale, FrameColor);
end;

procedure TCastlePackedGroup.PreferredSize(var PreferredWidth, PreferredHeight: Single);

  procedure PackChildren;
  begin
    if FPackingValid then Exit;
    { setting this to true early also prevents recursively entering DoPackChildren. }
    FPackingValid := true;
    DoPackChildren(FInsideWidth, FInsideHeight);
  end;

begin
  inherited;
  PackChildren;
  PreferredWidth  := FInsideWidth  * UIScale;
  PreferredHeight := FInsideHeight * UIScale;
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
  if [chRectangle, chChildren] * Changes <> [] then
    FPackingValid := false;
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
      else raise EInternalError.Create('TCastleVerticalGroup.DoPackChildren:Alignment');
    end;
    C.Anchor(Alignment, AlignmentDelta);

    W := W + C.EffectiveWidth + Spacing;
    MaxVar(H, C.EffectiveHeight);
  end;

  W := W - Spacing + Padding;
  H := H + 2 * Padding;
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
      else raise EInternalError.Create('TCastleVerticalGroup.DoPackChildren:Alignment');
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

{$endif read_implementation}
