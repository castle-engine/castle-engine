{%MainUnit castlecontrols.pas}
{
  Copyright 2010-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Possible crosshair shape, for @link(TCastleCrosshair.Shape). }
  TCastleCrosshairShape = (csCross, csCrossRect);

  { Display a simple crosshair in the middle of the parent control. }
  TCastleCrosshair = class(TCastleUserInterface)
  strict private
    FColor: TCastleColor;
    FShape: TCastleCrosshairShape;
    procedure SetShape(const Value: TCastleCrosshairShape);
    function ImageType: TThemeImage;
    procedure SetColor(const Value: TCastleColor);
  public
    const
      DefaultSize = 16.0;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Color tint of the crosshair. This simply multiplies the crosshair RGBA image.
      By default this is opaque white. }
    property Color: TCastleColor read FColor write SetColor;
  published
    property Shape: TCastleCrosshairShape read FShape write SetShape default csCross;

    { By default, crosshair is centered. }
    property HorizontalAnchorSelf default hpMiddle;
    property HorizontalAnchorParent default hpMiddle;
    property VerticalAnchorSelf default vpMiddle;
    property VerticalAnchorParent default vpMiddle;

    {$ifdef FPC}
    property Width default DefaultSize;
    property Height default DefaultSize;
    {$endif}

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlecrosshair_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleCrosshair ---------------------------------------------------- }

constructor TCastleCrosshair.Create(AOwner: TComponent);
begin
  inherited;
  FShape := csCross;
  FColor := White;
  Anchor(hpMiddle);
  Anchor(vpMiddle);
  Width := DefaultSize;
  Height := DefaultSize;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlecrosshair_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleCrosshair.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastlecrosshair_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

procedure TCastleCrosshair.SetShape(const Value: TCastleCrosshairShape);
begin
  if FShape <> Value then
  begin
    FShape := Value;
    VisibleChange([chRectangle]);
  end;
end;

function TCastleCrosshair.ImageType: TThemeImage;
begin
  if FShape = csCrossRect then
    Result := tiCrosshair2
  else
    Result := tiCrosshair1;
end;

procedure TCastleCrosshair.Render;
begin
  inherited;
  Theme.Draw(RenderRect, ImageType, UIScale, FColor);
end;

procedure TCastleCrosshair.SetColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FColor, Value) then
  begin
    FColor := Value;
    VisibleChange([chRender]);
  end;
end;

function TCastleCrosshair.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'ColorPersistent', 'Shape'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlecrosshair_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
