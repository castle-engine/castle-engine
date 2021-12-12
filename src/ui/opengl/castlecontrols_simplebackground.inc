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

  { Fill the whole window with a simple color.
    This is very fast, but it unconditionally clears the whole window,
    and there is no blending (if your @link(Color) has some alpha, it is
    just copied to the color buffer).

    To clear the rectangle with a color,
    with optional blending, use @link(TCastleRectangleControl) instead. }
  TCastleSimpleBackground = class(TCastleUserInterface)
  strict private
    FColor: TCastleColor;
    procedure SetColor(const Value: TCastleColor);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Render; override;
    property Width {$ifdef FPC}default 0{$endif};
    property Height {$ifdef FPC}default 0{$endif};
    { Background color. By default, this is black color with opaque alpha. }
    property Color: TCastleColor read FColor write SetColor;
  end deprecated 'use TCastleRectangleControl with FullSize=true; note that by default it has White color, while TCastleSimpleBackground has by default Black color.';

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleSimpleBackground ---------------------------------------------------- }

constructor TCastleSimpleBackground.Create(AOwner: TComponent);
begin
  inherited;
  FColor := Black;
  // This should have size = 0, to have empty rectangle, to never catch events
  Width := 0;
  Height := 0;
end;

procedure TCastleSimpleBackground.SetColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FColor, Value) then
  begin
    FColor := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleSimpleBackground.Render;
begin
  inherited;
  RenderContext.Clear([cbColor], Color);
end;

{$endif read_implementation}
