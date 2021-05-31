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

  { Possible shape type, for @link(TCastleShape.ShapeType). }
  TShapeType = (stRectangle, stCircle);

  { Draw a simple shape (rectangle, circle) with given color and optional outline. }
  TCastleShape = class(TCastleUserInterface)
  strict private
    FFilled, FOutline, FOutlineThick: boolean;
    FColor, FOutlineColor: TCastleColor;
    FOutlineWidth: Single;
    FShapeType: TShapeType;
    procedure SetShapeType(const Value: TShapeType);
    procedure SetFilled(const Value: boolean);
    procedure SetColor(const Value: TCastleColor);
    procedure SetOutline(const Value: boolean);
    procedure SetOutlineColor(const Value: TCastleColor);
    procedure SetOutlineWidth(const Value: Single);
    procedure SetOutlineThick(const Value: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { The fill color, used if @link(Filled). By default, opaque white. }
    property Color: TCastleColor read FColor write SetColor;

    { The outline color, used if @link(Outline). By default, opaque black. }
    property OutlineColor: TCastleColor read FOutlineColor write SetOutlineColor;

    { Determines the drawing method of the outline, used if @link(Outline).

      @definitionList(
        @itemLabel(@false (default))
        @item(Draw the outline using lines, and apply OutlineWidth by changing
          line width.

          Disadvantage:
          @bold(outline widths thicker than 1 pixel are not guaranteed
          to be supported. In particular they will almost never work on mobile
          (OpenGLES).)
          See the LineWidth comments at DrawPrimitive2D procedure.

          Consider using other methods if you need to draw a thick shape outline
          in a reliable manner. To draw a rectangle with frame, it's usually better
          to use Draw3x3 call, with a special texture with a frame and insides.)

        @itemLabel(@true)
        @item(Draw the outline by first drawing a larger shape with OutlineColor
          underneath the smaller shape with Color.

          Disadvantages:
          @unorderedList(
            @item(Cannot work sensibly if @link(Filled) is @false,
              so it's disabled then. When @link(Filled) is @false,
              it's like OutlineThick was always also @false.)
            @item(The alpha blending may not be exactly what you want,
              since the pixels inside are overdrawn with both OutlineColor
              and then with Color.)
            @item(May look a little worse in case of small OutlineWidth
              and non-rectangular shapes.)
          )

          Advantage: thick OutlineWidth works reliably.)
      )
    }
    property OutlineThick: boolean read FOutlineThick write SetOutlineThick;

    { The outline width, used if @link(Outline).
      It is affected by UI scaling.

      If OutlineThick is @false, then
      @bold(outline widths thicker than 1 pixel are not guaranteed
      to be supported. In particular they will almost never work on mobile (OpenGLES).)
      Change OutlineThick to @true to have reliable thick outlines. }
    property OutlineWidth: Single read FOutlineWidth write SetOutlineWidth default 1.0;
  published
    property ShapeType: TShapeType read FShapeType write SetShapeType default stRectangle;

    property Filled: boolean read FFilled write SetFilled default true;

    property Outline: boolean read FOutline write SetOutline default false;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastleshape_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleShape --------------------------------------------------------------- }

constructor TCastleShape.Create(AOwner: TComponent);
begin
  inherited;
  FShapeType := stRectangle;
  FFilled := true;
  FColor := White;
  FOutline := false;
  FOutlineWidth := 1.0;
  FOutlineColor := Black;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastleshape_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleShape.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastleshape_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

procedure TCastleShape.SetShapeType(const Value: TShapeType);
begin
  if FShapeType <> Value then
  begin
    FShapeType := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleShape.SetFilled(const Value: boolean);
begin
  if FFilled <> Value then
  begin
    FFilled := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleShape.SetColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FColor, Value) then
  begin
    FColor := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleShape.SetOutline(const Value: boolean);
begin
  if FOutline <> Value then
  begin
    FOutline := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleShape.SetOutlineColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FOutlineColor, Value) then
  begin
    FOutlineColor := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleShape.SetOutlineWidth(const Value: Single);
begin
  if FOutlineWidth <> Value then
  begin
    FOutlineWidth := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleShape.SetOutlineThick(const Value: boolean);
begin
  if FOutlineThick <> Value then
  begin
    FOutlineThick := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleShape.Render;
var
  SR: TFloatRectangle;
  OutlineW, OutlineIn, OutlineOut: Integer;
begin
  inherited;
  SR := RenderRect;

  if Filled and Outline and OutlineThick then
  begin
    { special case when we use OutlineThick drawing mode }

    OutlineW := Ceil(UIScale * OutlineWidth);
    OutlineIn := - OutlineW div 2;
    OutlineOut := OutlineW + OutlineIn;

    case ShapeType of
      stRectangle:
        begin
          DrawRectangle(SR.Grow(OutlineOut), OutlineColor);
          DrawRectangle(SR.Grow(OutlineIn ), Color);
        end;
      stCircle   :
        begin
          DrawCircle(SR.Center, SR.Width / 2 + OutlineOut, SR.Height / 2 + OutlineOut, OutlineColor);
          DrawCircle(SR.Center, SR.Width / 2 + OutlineIn , SR.Height / 2 + OutlineIn , Color);
        end;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('TCastleShape.Render: ShapeType not implemented');
      {$endif}
    end;
  end else
  begin
    if Filled then
      case ShapeType of
        stRectangle: DrawRectangle(SR, Color);
        stCircle   : DrawCircle(SR.Center, SR.Width / 2, SR.Height / 2, Color);
        {$ifndef COMPILER_CASE_ANALYSIS}
        else raise EInternalError.Create('TCastleShape.Render: ShapeType not implemented');
        {$endif}
      end;

    if Outline then
      case ShapeType of
        stRectangle: DrawRectangleOutline(SR, OutlineColor, UIScale * OutlineWidth);
        stCircle   : DrawCircleOutline(SR.Center, SR.Width / 2, SR.Height / 2, OutlineColor, UIScale * OutlineWidth);
        {$ifndef COMPILER_CASE_ANALYSIS}
        else raise EInternalError.Create('TCastleShape.Render: ShapeType not implemented');
        {$endif}
      end;
  end;
end;

function TCastleShape.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'ColorPersistent') or
     (PropertyName = 'ShapeType') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleshape_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
