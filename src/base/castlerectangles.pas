{
  Copyright 2006-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Rectangle representation (TRectangle). }
unit CastleRectangles;

interface

uses CastleGenericLists, CastleVectors;

type
  { Horizontal position of one control/rectangle
    with respect to another.

    This is used by TUIRectangularControl.Align and TRectangle.Align
    to specify the alignment of one control/rectangle with respect to another.
    In case of TUIRectangularControl.Align, this specifies
    the align of control with respect to the container
    (TCastleWindow or TCastleControl).

    This is used to talk about position of the control and the container.

    @orderedList(
      @item(
        When we talk about the position of the control
        (for example ControlPosition for TUIRectangularControl.Align),
        it determines which border of the control to align.)
      @item(
        When we talk about the position of the container
        (for example ContainerPosition for TUIRectangularControl.Align),
        this specifies the container border.)
    )

    In most cases you use equal both control and container borders.
    For example, both ControlPosition and ContainerPosition are usually equal for
    TUIControlPos.Align call. This allows to align left control edge to
    left container edge, or right control edge to right container edge,
    or to center control within the container --- which is the most common usage.

    @unorderedList(
      @item(If both are prLeft, then X specifies position
        of left control border relative to left container border.
        X should be >= 0 if you want to see the control completely
        within the container.)

      @item(If both are prMiddle, then X (most often just 0)
        specifies the shift between container middle to
        control middle. If X is zero, then control is just in the
        middle of the container.)

      @item(If both are prRight, then X specifies position
        of right control border relative to right container border.
        X should be <= 0 if you want to see the control completely
        within the container.)
    )

    @seealso TVerticalPosition
  }
  THorizontalPosition = (
    hpLeft,
    hpMiddle,
    hpRight
  );

  { Vertical position of one control/rectangle with respect to another.
    @seealso THorizontalPosition }
  TVerticalPosition = (
    vpBottom,
    vpMiddle,
    vpTop
  );

  { 2D rectangle with integer coordinates.
    Useful for various 2D GUI operations.

    The area covered by the rectangle starts in (Left,Bottom)
    pixel and spans (Width,Height) pixels. This means that the right-top pixel
    covered by the rectangle is (Left + Width - 1,Bottom + Height - 1).
    The rectangle is empty (@link(Contains) will always answer @false)
    when either Width or Height are zero. }
  TRectangle = object
  private
    function GetRight: Integer;
    function GetTop: Integer;
    function GetLeftBottom: TVector2Integer;
    procedure SetLeftBottom(const Value: TVector2Integer);
  public
    Left, Bottom: Integer;
    Width, Height: Cardinal;

    const
      Empty: TRectangle = (Left: 0; Bottom: 0; Width: 0; Height: 0);

    function Contains(const X, Y: Integer): boolean;
    function Contains(const Point: TVector2Single): boolean;
    function Contains(const Point: TVector2Integer): boolean;

    { Right and Top pixels are 1 pixel *outside* of the rectangle.
      @groupBegin }
    property Right: Integer read GetRight;
    property Top: Integer read GetTop;
    { @groupEnd }

    { Return rectangle with given width and height centered
      in the middle of this rectangle. The given W, H may be smaller or larger
      than this rectangle sizes. }
    function Center(const W, H: Cardinal): TRectangle;

    { Grow (when Delta > 0) or shrink (when Delta < 0)
      the rectangle, returning new value.
      This adds a margin of Delta pixels around all sides of the rectangle,
      so in total width grows by 2 * Delta, and the same for height.
      In case of shrinking, we protect from shrinking too much:
      the resulting width or height is set to zero (which makes a valid
      and empty rectangle) if shrinking too much. }
    function Grow(const Delta: Integer): TRectangle;
    function Grow(const DeltaX, DeltaY: Integer): TRectangle;

    { Returns the rectangle with a number of pixels from given
      side removed. Returns an empty rectangle if you try to remove too much.
      @groupBegin }
    function RemoveLeft(W: Cardinal): TRectangle;
    function RemoveBottom(H: Cardinal): TRectangle;
    function RemoveRight(W: Cardinal): TRectangle;
    function RemoveTop(H: Cardinal): TRectangle;
    { @groupEnd }

    { Returns the rectangle with a number of pixels on given
      side added.
      @groupBegin }
    function GrowLeft(const W: Cardinal): TRectangle;
    function GrowBottom(const H: Cardinal): TRectangle;
    function GrowRight(const W: Cardinal): TRectangle;
    function GrowTop(const H: Cardinal): TRectangle;
    { @groupEnd }

    { Returns the given side of the rectangle, cut down to given number of pixels
      from given side. This is similar to RemoveXxx methods, but here you specify
      which side to keep, as opposed to RemoveXxx methods where you specify which
      side you remove.

      If the requested size is larger than current size (for example,
      W > Width for LeftPart) then the unmodified rectangle is returned.

      @groupBegin }
    function LeftPart(W: Cardinal): TRectangle;
    function BottomPart(H: Cardinal): TRectangle;
    function RightPart(W: Cardinal): TRectangle;
    function TopPart(H: Cardinal): TRectangle;
    { @groupEnd }

    property LeftBottom: TVector2Integer read GetLeftBottom write SetLeftBottom;
    function Middle: TVector2Integer;

    { Clamp value to be within allowed horizontal range.
      That is, clamp to @code([Left, Right - 1]). }
    function ClampX(const X: Integer): Integer;

    { Clamp value to be within allowed vertical range.
      That is, clamp to @code([Bottom, Top - 1]). }
    function ClampY(const Y: Integer): Integer;

    function ScaleToWidth(const NewWidth: Cardinal): TRectangle;
    function ScaleToHeight(const NewHeight: Cardinal): TRectangle;

    { Align this rectangle within other rectangle by calculating new value
      for @link(Left). }
    function AlignCore(
      const ThisPosition: THorizontalPosition;
      const OtherRect: TRectangle;
      const OtherPosition: THorizontalPosition;
      const X: Integer = 0): Integer;
    function Align(
      const ThisPosition: THorizontalPosition;
      const OtherRect: TRectangle;
      const OtherPosition: THorizontalPosition;
      const X: Integer = 0): TRectangle;

    { Align this rectangle within other rectangle by calculating new value
      for @link(Bottom). }
    function AlignCore(
      const ThisPosition: TVerticalPosition;
      const OtherRect: TRectangle;
      const OtherPosition: TVerticalPosition;
      const Y: Integer = 0): Integer;
    function Align(
      const ThisPosition: TVerticalPosition;
      const OtherRect: TRectangle;
      const OtherPosition: TVerticalPosition;
      const Y: Integer = 0): TRectangle;

    function ToString: string;
  end;

  TRectangleList = class(specialize TGenericStructList<TRectangle>)
  public
    { Index of the first rectangle that contains point (X, Y).
      Returns -1 if not found. }
    function FindRectangle(const X, Y: Integer): Integer;
    function FindRectangle(const Point: TVector2Single): Integer;
  end;

function Rectangle(const Left, Bottom: Integer;
  const Width, Height: Cardinal): TRectangle;
function Rectangle(const LeftBottom: TVector2Integer;
  const Width, Height: Cardinal): TRectangle;

implementation

uses SysUtils,
  CastleUtils;

{ TRectangle ----------------------------------------------------------------- }

function Rectangle(const Left, Bottom: Integer;
  const Width, Height: Cardinal): TRectangle;
begin
  Result.Left := Left;
  Result.Bottom := Bottom;
  Result.Width := Width;
  Result.Height := Height;
end;

function Rectangle(const LeftBottom: TVector2Integer;
  const Width, Height: Cardinal): TRectangle;
begin
  Result.Left := LeftBottom[0];
  Result.Bottom := LeftBottom[1];
  Result.Width := Width;
  Result.Height := Height;
end;

function TRectangle.Contains(const X, Y: Integer): boolean;
begin
  Result := (X >= Left  ) and (X < Left   + Integer(Width)) and
            (Y >= Bottom) and (Y < Bottom + Integer(Height));
end;

function TRectangle.Contains(const Point: TVector2Single): boolean;
begin
  Result := (Point[0] >= Left  ) and (Point[0] < Left   + Integer(Width)) and
            (Point[1] >= Bottom) and (Point[1] < Bottom + Integer(Height));
end;

function TRectangle.Contains(const Point: TVector2Integer): boolean;
begin
  Result := (Point[0] >= Left  ) and (Point[0] < Left   + Integer(Width)) and
            (Point[1] >= Bottom) and (Point[1] < Bottom + Integer(Height));
end;

function TRectangle.Center(const W, H: Cardinal): TRectangle;
begin
  Result.Left   := Left   + (Integer(Width ) - Integer(W)) div 2;
  Result.Bottom := Bottom + (Integer(Height) - Integer(H)) div 2;
  Result.Width  := W;
  Result.Height := H;
end;

function TRectangle.Grow(const DeltaX, DeltaY: Integer): TRectangle;
begin
  if Integer(Width) + 2 * DeltaX < 0 then
  begin
    Result.Left := Left + Width div 2;
    Result.Width := 0;
  end else
  begin
    Result.Left := Left - DeltaX;
    Result.Width := Integer(Width) + 2 * DeltaX;
  end;

  if Integer(Height) + 2 * DeltaY < 0 then
  begin
    Result.Bottom := Bottom + Height div 2;
    Result.Height := 0;
  end else
  begin
    Result.Bottom := Bottom - DeltaY;
    Result.Height := Integer(Height) + 2 * DeltaY;
  end;
end;

function TRectangle.Grow(const Delta: Integer): TRectangle;
begin
  Result := Grow(Delta, Delta);
end;

function TRectangle.GetRight: Integer;
begin
  Result := Left + Width;
end;

function TRectangle.GetTop: Integer;
begin
  Result := Bottom + Height;
end;

function TRectangle.RemoveLeft(W: Cardinal): TRectangle;
begin
  Result := Self;
  MinTo1st(W, Width);
  Result.Left += W;
  Result.Width -= W;
end;

function TRectangle.RemoveBottom(H: Cardinal): TRectangle;
begin
  Result := Self;
  MinTo1st(H, Height);
  Result.Bottom += H;
  Result.Height -= H;
end;

function TRectangle.RemoveRight(W: Cardinal): TRectangle;
begin
  Result := Self;
  MinTo1st(W, Width);
  Result.Width -= W;
end;

function TRectangle.RemoveTop(H: Cardinal): TRectangle;
begin
  Result := Self;
  MinTo1st(H, Height);
  Result.Height -= H;
end;

function TRectangle.GrowLeft(const W: Cardinal): TRectangle;
begin
  Result := Self;
  Result.Left -= Integer(W);
  Result.Width += W;
end;

function TRectangle.GrowBottom(const H: Cardinal): TRectangle;
begin
  Result := Self;
  Result.Bottom -= Integer(H);
  Result.Height += H;
end;

function TRectangle.GrowRight(const W: Cardinal): TRectangle;
begin
  Result := Self;
  Result.Width += W;
end;

function TRectangle.GrowTop(const H: Cardinal): TRectangle;
begin
  Result := Self;
  Result.Height += H;
end;

function TRectangle.LeftPart(W: Cardinal): TRectangle;
begin
  Result := Self;
  MinTo1st(W, Width);
  Result.Width := W;
end;

function TRectangle.BottomPart(H: Cardinal): TRectangle;
begin
  Result := Self;
  MinTo1st(H, Height);
  Result.Height := H;
end;

function TRectangle.RightPart(W: Cardinal): TRectangle;
begin
  Result := Self;
  MinTo1st(W, Width);
  Result.Left += Width - W;
  Result.Width := W;
end;

function TRectangle.TopPart(H: Cardinal): TRectangle;
begin
  Result := Self;
  MinTo1st(H, Height);
  Result.Bottom += Height - H;
  Result.Height := H;
end;

function TRectangle.GetLeftBottom: TVector2Integer;
begin
  Result[0] := Left;
  Result[1] := Bottom;
end;

procedure TRectangle.SetLeftBottom(const Value: TVector2Integer);
begin
  Left := Value[0];
  Bottom := Value[1];
end;

function TRectangle.ClampX(const X: Integer): Integer;
begin
  if X <  Left then
    Result := Left else
  if X >= Left + Width then
    Result := Left + Width - 1 else
    Result := X;
end;

function TRectangle.ClampY(const Y: Integer): Integer;
begin
  if Y <  Bottom then
    Result := Bottom else
  if Y >= Bottom + Height then
    Result := Bottom + Height - 1 else
    Result := Y;
end;

function TRectangle.ToString: string;
begin
  Result := Format('TRectangle: %dx%d %dx%d', [Left, Bottom, Width, Height]);
end;

function TRectangle.Middle: TVector2Integer;
begin
  Result := Vector2Integer(Left + Width div 2, Bottom + Height div 2);
end;

function TRectangle.ScaleToWidth(const NewWidth: Cardinal): TRectangle;
begin
  Result.Left := Left;
  Result.Bottom := Bottom;
  Result.Width := NewWidth;
  Result.Height := Height * NewWidth div Width;
end;

function TRectangle.ScaleToHeight(const NewHeight: Cardinal): TRectangle;
begin
  Result.Left := Left;
  Result.Bottom := Bottom;
  Result.Width := Width * NewHeight div Height;
  Result.Height := NewHeight;
end;

function TRectangle.AlignCore(
  const ThisPosition: THorizontalPosition;
  const OtherRect: TRectangle;
  const OtherPosition: THorizontalPosition;
  const X: Integer = 0): Integer;
begin
  Result := OtherRect.Left + X;
  case ThisPosition of
    hpLeft  : ;
    hpMiddle: Result -= Width div 2;
    hpRight : Result -= Width;
  end;
  case OtherPosition of
    hpLeft  : ;
    hpMiddle: Result += OtherRect.Width div 2;
    hpRight : Result += OtherRect.Width;
  end;
end;

function TRectangle.AlignCore(
  const ThisPosition: TVerticalPosition;
  const OtherRect: TRectangle;
  const OtherPosition: TVerticalPosition;
  const Y: Integer = 0): Integer;
begin
  Result := OtherRect.Bottom + Y;
  case ThisPosition of
    vpBottom: ;
    vpMiddle: Result -= Height div 2;
    vpTop   : Result -= Height;
  end;
  case OtherPosition of
    vpBottom: ;
    vpMiddle: Result += OtherRect.Height div 2;
    vpTop   : Result += OtherRect.Height;
  end;
end;

function TRectangle.Align(
  const ThisPosition: THorizontalPosition;
  const OtherRect: TRectangle;
  const OtherPosition: THorizontalPosition;
  const X: Integer = 0): TRectangle;
begin
  Result.Left := AlignCore(ThisPosition, OtherRect, OtherPosition, X);
  Result.Bottom := Bottom;
  Result.Width := Width;
  Result.Height := Height;
end;

function TRectangle.Align(
  const ThisPosition: TVerticalPosition;
  const OtherRect: TRectangle;
  const OtherPosition: TVerticalPosition;
  const Y: Integer = 0): TRectangle;
begin
  Result.Left := Left;
  Result.Bottom := AlignCore(ThisPosition, OtherRect, OtherPosition, Y);
  Result.Width := Width;
  Result.Height := Height;
end;

{ TRectangleList -------------------------------------------------------------- }

function TRectangleList.FindRectangle(const X, Y: Integer): Integer;
begin
  for Result := 0 to Count - 1 do
    if L[Result].Contains(X, Y) then
      Exit;
  Result := -1;
end;

function TRectangleList.FindRectangle(const Point: TVector2Single): Integer;
begin
  for Result := 0 to Count - 1 do
    if L[Result].Contains(Point) then
      Exit;
  Result := -1;
end;

end.
