{%MainUnit castlecontrols.pas}
{
  Copyright 2010-2024 Michalis Kamburelis.

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
  TShapeType = (
    stRectangle,
    stCircle,
    stTriangleUp,
    stTriangleDown,
    stTriangleRight,
    stTriangleLeft
  );

  { Draw a simple shape (rectangle, circle, triangle) with given color
    and optional outline. }
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
  published
    { The shape (rectangle, circle, triangle) to be drawn. }
    property ShapeType: TShapeType read FShapeType write SetShapeType default stRectangle;

    { Determines the drawing method of the outline, used if @link(Outline).

      @definitionList(
        @itemLabel(@false (default))
        @item(Draw the outline using lines, and apply OutlineWidth by changing
          line width.

          Disadvantage:
          @bold(outline widths thicker than 1 pixel are not guaranteed
          to be supported. In particular they will almost never work on mobile
          (OpenGLES).)

          Consider using other methods if you need to draw a thick shape outline
          in a reliable manner.
          For example, set @link(OutlineThick) to @true.
          Or draw the shape as an image with a frame,
          using @link(TCastleImageControl) and @link(TCastleImageControl.ProtectedSides).)

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
    property OutlineThick: boolean read FOutlineThick write SetOutlineThick default false;

    { The outline width, used if @link(Outline).
      It is affected by UI scaling.

      If OutlineThick is @false, then
      @bold(outline widths thicker than 1 pixel are not guaranteed
      to be supported. In particular they will almost never work on mobile (OpenGLES).)
      Change OutlineThick to @true to have reliable thick outlines. }
    property OutlineWidth: Single read FOutlineWidth write SetOutlineWidth
      {$ifdef FPC}default 1.0{$endif};

    { Display outline of the shape using @link(OutlineColor). }
    property Outline: boolean read FOutline write SetOutline default false;

    { Display inside of the shape using @link(Color). }
    property Filled: boolean read FFilled write SetFilled default true;

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

type
  TShapeTypeTriangle = stTriangleUp..stTriangleLeft;

{ Draw triangle type indicated by current ShapeType.
  Use only when ShapeType is stTriangleXxx.

  @param R Rectangle to draw the triangle inside.
  @param Color Color of the triangle.
  @param Filled Whether we should fill the triangle, otherwise we draw only lines.
  @param LineWidth Line width used when Filled = @false.

  Note: This is internal, and deliberately not a method of TCastleShape,
  to not use some TCastleShape property by accident. }
procedure DrawTriangleCore(const ShapeType: TShapeTypeTriangle;
  const R: TFloatRectangle; const Color: TCastleColor; const Filled: Boolean;
  const LineWidth: Single = 1.0);
var
  Mode: TPrimitiveMode;
  Points: array [0..2] of TVector2;
begin
  if Filled then
    Mode := pmTriangles
  else
    Mode := pmLineLoop;

  case ShapeType of
    stTriangleUp:
      begin
        Points[0] := Vector2(R.Left, R.Bottom);
        Points[1] := Vector2(R.Right, R.Bottom);
        Points[2] := Vector2((R.Left + R.Right) / 2, R.Top);
      end;
    stTriangleDown:
      begin
        Points[0] := Vector2(R.Right, R.Top);
        Points[1] := Vector2(R.Left, R.Top);
        Points[2] := Vector2((R.Left + R.Right) / 2, R.Bottom);
      end;
    stTriangleRight:
      begin
        Points[0] := Vector2(R.Left, R.Bottom);
        Points[1] := Vector2(R.Right, (R.Top + R.Bottom) / 2);
        Points[2] := Vector2(R.Left, R.Top);
      end;
    stTriangleLeft:
      begin
        Points[0] := Vector2(R.Right, R.Top);
        Points[1] := Vector2(R.Left, (R.Top + R.Bottom) / 2);
        Points[2] := Vector2(R.Right, R.Bottom);
      end;
    else raise EInternalError.Create('DrawTriangleCore only useful for triangle shapes');
  end;

  DrawPrimitive2D(Mode, Points, Color,
    { We specify BlendingSourceFactor, BlendingDestinationFactor, ForceBlending
      equal to default, only to be able to provide explicit LineWidth. }
    bsSrcAlpha, bdOneMinusSrcAlpha, false, LineWidth);
end;

procedure TCastleShape.Render;

  function RectGrowForTriangleOutline(const RR: TFloatRectangle;
    const OutlineOut: Single): TFloatRectangle;
  var
    ExtraGrowTip: Single;
  begin
    Result := RR.Grow(OutlineOut);
    { Expand RR more in the direction of the triangle, makes the outline look
      even in typical cases.
      TODO: Use trigonometry to calculate this more precisely. }
    ExtraGrowTip := 2 * OutlineOut;
    case ShapeType of
      stTriangleUp: Result := Result.GrowTop(ExtraGrowTip);
      stTriangleDown: Result := Result.GrowBottom(ExtraGrowTip);
      stTriangleRight: Result := Result.GrowRight(ExtraGrowTip);
      stTriangleLeft: Result := Result.GrowLeft(ExtraGrowTip);
      else raise EInternalError.Create('Only triangle ShapeType should be here');
    end;
  end;

var
  RR: TFloatRectangle;
  OutlineWidthScaled, OutlineIn, OutlineOut: Single;
begin
  inherited;
  RR := RenderRect;
  OutlineWidthScaled := UIScale * OutlineWidth;

  if Filled and Outline and OutlineThick then
  begin
    { special case when we use OutlineThick drawing mode }

    OutlineIn := -OutlineWidthScaled / 2;
    OutlineOut := OutlineWidthScaled / 2;

    case ShapeType of
      stRectangle:
        begin
          DrawRectangle(RR.Grow(OutlineOut), OutlineColor);
          DrawRectangle(RR.Grow(OutlineIn ), Color);
        end;
      stCircle:
        begin
          DrawCircle(RR.Center, RR.Width / 2 + OutlineOut, RR.Height / 2 + OutlineOut, OutlineColor);
          DrawCircle(RR.Center, RR.Width / 2 + OutlineIn , RR.Height / 2 + OutlineIn , Color);
        end;
      stTriangleUp..stTriangleLeft:
        begin
          DrawTriangleCore(ShapeType, RectGrowForTriangleOutline(RR, OutlineOut), OutlineColor, true);
          DrawTriangleCore(ShapeType, RR.Grow(OutlineIn), Color, true);
        end;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('TCastleShape.Render: ShapeType not implemented (Outline thick)');
      {$endif}
    end;
  end else
  begin
    if Filled then
      case ShapeType of
        stRectangle:
          DrawRectangle(RR, Color);
        stCircle:
          DrawCircle(RR.Center, RR.Width / 2, RR.Height / 2, Color);
        stTriangleUp..stTriangleLeft:
          DrawTriangleCore(ShapeType, RR, Color, true);
        {$ifndef COMPILER_CASE_ANALYSIS}
        else raise EInternalError.Create('TCastleShape.Render: ShapeType not implemented');
        {$endif}
      end;

    if Outline then
      case ShapeType of
        stRectangle:
          DrawRectangleOutline(RR, OutlineColor, OutlineWidthScaled);
        stCircle:
          DrawCircleOutline(RR.Center, RR.Width / 2, RR.Height / 2, OutlineColor, OutlineWidthScaled);
        stTriangleUp..stTriangleLeft:
          DrawTriangleCore(ShapeType, RR, OutlineColor, false, OutlineWidthScaled);
        {$ifndef COMPILER_CASE_ANALYSIS}
        else raise EInternalError.Create('TCastleShape.Render: ShapeType not implemented (Outline, not thick)');
        {$endif}
      end;
  end;
end;

function TCastleShape.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
      'ShapeType',
      'ColorPersistent', 'OutlineColorPersistent',
      'Filled', 'Outline', 'OutlineWidth', 'OutlineThick'
    ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleshape_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
