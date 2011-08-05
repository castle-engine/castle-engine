{
  Copyright 2002-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ 2D rectangles.
  Main use is to detect in which 2D rectangle on the screen the mouse cursor is.
  @noAutoLinkHere }
unit Rectangles;

{ Internal notes: the only remaining reasons against using TRect
  (instead of TRectangle): TRect methods generally suggest that
  upper-left corner is the origin (while for OpenGL, lower-left corner
  is *usually* the origin). }

interface

{$define read_interface}

uses KambiUtils, SysUtils;

type
  { 2D rectangle. }
  TRectangle = record
    X0, Y0, Width, Height: Integer;
  end;
  PRectangle = ^TRectangle;

  TDynArrayItem_1 = TRectangle;
  PDynArrayItem_1 = PRectangle;
  {$define DYNARRAY_1_IS_STRUCT}
  {$I dynarray_1.inc}

  TDynRectangleArray = class(TDynArray_1)
  public
    { FindRectangle returns index of the rectangle that contains point (X, Y).

      @italic(It returns the index of the @bold(last) rectangle, that is:
      it searches from the end of the list.) This way the rectangles added
      later by Add method are treated as being on top of previous
      rectangles, which is more intuitive.

      Returns -1 if not found. }
    function FindRectangle(const X, Y: Integer): integer;
  end;

function Rectangle(const X0, Y0, Width, Height: Integer): TRectangle;

function PointInRectangle(const X, Y: Integer; const Rectangle: TRectangle): boolean;

{$undef read_interface}

implementation

{$define read_implementation}
{$I dynarray_1.inc}

{ TDynRectangleArray -------------------------------------------------------------- }

function TDynRectangleArray.FindRectangle(const X, Y: Integer): integer;
begin
  for Result := Count - 1 downto 0 do
    if PointInRectangle(X, Y, Items[Result]) then
      Exit;
  Result := -1;
end;

{ global funcs --------------------------------------------------------------- }

function Rectangle(const X0, Y0, Width, Height: Integer): TRectangle;
begin
  Result.X0 := X0;
  Result.Y0 := Y0;
  Result.Width := Width;
  Result.Height := Height;
end;

function PointInRectangle(const X, Y: Integer; const Rectangle: TRectangle): boolean;
begin
  Result := (X >= Rectangle.X0) and (X <= Rectangle.X0 + Rectangle.Width) and
            (Y >= Rectangle.Y0) and (Y <= Rectangle.Y0 + Rectangle.Height);
end;

end.
