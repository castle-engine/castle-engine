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
  TCastleShape = class(TUIControlSizeable)
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
    procedure Render; override;

    property ShapeType: TShapeType read FShapeType write SetShapeType default stRectangle;

    property Filled: boolean read FFilled write SetFilled default true;

    { The fill color, used if @link(Filled). By default, opaque white. }
    property Color: TCastleColor read FColor write SetColor;

    property Outline: boolean read FOutline write SetOutline default false;

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
end;

procedure TCastleShape.SetShapeType(const Value: TShapeType);
begin
  if FShapeType <> Value then
  begin
    FShapeType := Value;
    VisibleChange;
  end;
end;

procedure TCastleShape.SetFilled(const Value: boolean);
begin
  if FFilled <> Value then
  begin
    FFilled := Value;
    VisibleChange;
  end;
end;

procedure TCastleShape.SetColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FColor, Value) then
  begin
    FColor := Value;
    VisibleChange;
  end;
end;

procedure TCastleShape.SetOutline(const Value: boolean);
begin
  if FOutline <> Value then
  begin
    FOutline := Value;
    VisibleChange;
  end;
end;

procedure TCastleShape.SetOutlineColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FOutlineColor, Value) then
  begin
    FOutlineColor := Value;
    VisibleChange;
  end;
end;

procedure TCastleShape.SetOutlineWidth(const Value: Single);
begin
  if FOutlineWidth <> Value then
  begin
    FOutlineWidth := Value;
    VisibleChange;
  end;
end;

procedure TCastleShape.SetOutlineThick(const Value: boolean);
begin
  if FOutlineThick <> Value then
  begin
    FOutlineThick := Value;
    VisibleChange;
  end;
end;

procedure TCastleShape.Render;
var
  SR: TRectangle;
  OutlineW, OutlineIn, OutlineOut: Integer;
begin
  inherited;
  SR := ScreenRect;

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
          DrawCircle(SR.Center, SR.Width div 2 + OutlineOut, SR.Height div 2 + OutlineOut, OutlineColor);
          DrawCircle(SR.Center, SR.Width div 2 + OutlineIn , SR.Height div 2 + OutlineIn , Color);
        end;
      else raise EInternalError.Create('TCastleShape.Render: ShapeType not implemented');
    end;
  end else
  begin
    if Filled then
      case ShapeType of
        stRectangle: DrawRectangle(SR, Color);
        stCircle   : DrawCircle(SR.Center, SR.Width div 2, SR.Height div 2, Color);
        else raise EInternalError.Create('TCastleShape.Render: ShapeType not implemented');
      end;

    if Outline then
      case ShapeType of
        stRectangle: DrawRectangleOutline(SR, OutlineColor, UIScale * OutlineWidth);
        stCircle   : DrawCircleOutline(SR.Center, SR.Width div 2, SR.Height div 2, OutlineColor, UIScale * OutlineWidth);
        else raise EInternalError.Create('TCastleShape.Render: ShapeType not implemented');
      end;
  end;
end;

{$endif read_implementation}
