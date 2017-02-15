{
  Copyright 2006-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Rectangle representation (TRectangle, TFloatRectangle). }
unit CastleRectangles;

{$I castleconf.inc}

interface

uses CastleGenericLists, CastleVectors;

type
  { Horizontal position of one control/rectangle
    with respect to another.

    This is used by TUIControl.Align and TRectangle.Align
    to specify the alignment of one control/rectangle with respect to another.
    In case of TUIControl.Align, this specifies
    the align of control with respect to the container
    (TCastleWindow or TCastleControl).

    This is used to talk about position of the control and the container.

    @orderedList(
      @item(
        When we talk about the position of the control
        (for example ControlPosition for TUIControl.Align),
        it determines which border of the control to align.)
      @item(
        When we talk about the position of the container
        (for example ContainerPosition for TUIControl.Align),
        this specifies the container border.)
    )

    In most cases you use equal both control and container borders.
    For example, both ControlPosition and ContainerPosition are usually equal for
    TUIControlPos.Align call. This allows to align left control edge to
    left container edge, or right control edge to right container edge,
    or to center control within the container --- which is the most common usage.

    @unorderedList(
      @item(If both are hpLeft, then X specifies position
        of left control border relative to left container border.
        X should be >= 0 if you want to see the control completely
        within the container.)

      @item(If both are hpMiddle, then X (most often just 0)
        specifies the shift between container middle to
        control middle. If X is zero, then control is just in the
        middle of the container.)

      @item(If both are hpRight, then X specifies position
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

  { 2D rectangle with @bold(integer) coordinates.
    Useful for various 2D GUI operations.

    The area covered by the rectangle starts in (Left,Bottom)
    pixel and spans (Width,Height) pixels. This means that the right-top pixel
    covered by the rectangle is (Left + Width - 1,Bottom + Height - 1).
    The rectangle is empty (@link(Contains) will always answer @false)
    when either Width or Height are zero. Neither Width nor Height can ever
    be negative. }
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

    function IsEmpty: boolean;

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
    function CenterInside(const W, H: Cardinal): TRectangle;

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
    function Middle: TVector2Integer; deprecated 'use Center';
    function Center: TVector2Integer;

    { Clamp value to be within allowed horizontal range.
      That is, clamp to @code([Left, Right - 1]). }
    function ClampX(const X: Integer): Integer;

    { Clamp value to be within allowed vertical range.
      That is, clamp to @code([Bottom, Top - 1]). }
    function ClampY(const Y: Integer): Integer;

    function ScaleToWidth(const NewWidth: Cardinal): TRectangle;
    function ScaleToHeight(const NewHeight: Cardinal): TRectangle;

    { Scale rectangle position and size around it's own @link(Center) point.

      Since the scaling is independent in each axis,
      this handles "carefully" a half-empty rectangles
      (when one size is <= 0, but other is > 0).
      It scales correctly the positive dimension
      (not just returns @link(Empty) constant),
      leaving the other dimension (it's position and size) untouched. }
    function ScaleAroundCenter(const Factor: Single): TRectangle;
    function ScaleAroundMiddle(const Factor: Single): TRectangle;
      deprecated 'use ScaleAroundCenter';

    { Scale rectangle position and size around the (0,0) point.

      Since the scaling is independent in each axis,
      this handles "carefully" a half-empty rectangles
      (when one size is <= 0, but other is > 0).
      It scales correctly the positive dimension
      (not just returns @link(Empty) constant),
      leaving the other dimension (it's position and size) untouched.

      These details matter, e.g. when you set @link(TUIControlSizeable.Width), but not
      @link(TUIControlSizeable.Height),
      and then you expect the @link(TUIControl.CalculatedWidth) to work.
    }
    function ScaleAround0(const Factor: Single): TRectangle;

    { Scale @link(Width), in the same manner as ScaleAround0 would do. }
    function ScaleWidthAround0(const Factor: Single): Cardinal;
    { Scale @link(Height), in the same manner as ScaleAround0 would do. }
    function ScaleHeightAround0(const Factor: Single): Cardinal;

    { Scale and align us to fit inside rectangle R, preserving our aspect ratio. }
    function FitInside(const R: TRectangle;
      const AlignHorizontal: THorizontalPosition = hpMiddle;
      const AlignVertical: TVerticalPosition = vpMiddle): TRectangle;

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

    { Move the rectangle. Empty rectangle after moving is still an empty rectangle. }
    function Translate(const V: TVector2Integer): TRectangle;

    { Does it have any common part with another rectangle. }
    function Collides(const R: TRectangle): boolean;
  end;

  { 2D rectangle with @bold(float) coordinates.
    Useful for various 2D GUI operations, and for bounding boxes for 2D objects.

    The area covered by the rectangle starts at (Left,Bottom) position
    and spans (Width,Height) units.
    The rectangle is empty (@link(Contains) will always answer @false)
    when either Width or Height are less than zero.
    @bold(This is consistent with it's 3D equivalent, @link(TBox3D),
    and different from it's integer counterpart @link(TRectangle).)
    In case of float bounding box (@link(TBox3D)) or float rectangle
    (@name), having a zero size makes sense, and it still is something non-empty
    (a single 2D or 3D point has zero size, but also has position). }
  TFloatRectangle = object
  private
    function GetRight: Single;
    function GetTop: Single;
  public
    Left, Bottom: Single;
    Width, Height: Single;

    const
      Empty: TFloatRectangle = (Left: 0; Bottom: 0; Width: -1; Height: -1);

    function IsEmpty: boolean;

    function Contains(const X, Y: Single): boolean;
    function Contains(const Point: TVector2Single): boolean;

    { Right and Top pixels are 1 pixel *outside* of the rectangle.
      @groupBegin }
    property Right: Single read GetRight;
    property Top: Single read GetTop;
    { @groupEnd }

    function Middle: TVector2Single; deprecated 'use Center';
    function Center: TVector2Single;

    { Grow (when Delta > 0) or shrink (when Delta < 0)
      the rectangle, returning new value.
      This adds a margin of Delta pixels around all sides of the rectangle,
      so in total width grows by 2 * Delta, and the same for height.
      In case of shrinking, we protect from shrinking too much:
      the resulting width or height is set to zero (which makes a valid
      and empty rectangle) if shrinking too much. }
    function Grow(const Delta: Single): TFloatRectangle;
    function Grow(const DeltaX, DeltaY: Single): TFloatRectangle;

    function ToString: string;

    { Move the rectangle. Empty rectangle after moving is still an empty rectangle. }
    function Translate(const V: TVector2Single): TFloatRectangle;

    { Does it have any common part with another rectangle. }
    function Collides(const R: TFloatRectangle): boolean;

    function CollidesDisc(const DiscCenter: TVector2Single; const Radius: Single): boolean;

    { Scale rectangle position and size around the (0,0) point. }
    function ScaleAround0(const Factor: Single): TFloatRectangle;

    { Return larger rectangle, so that it includes given point. }
    function Add(const P: TVector2Single): TFloatRectangle;

    { Convert to a 4D vector, like expected by X3D fields
      OrthoViewpoint.fieldOfView or DirectionalLight.projectionRectangle. }
    function ToX3DVector: TVector4Single;

    { Convert from a 4D vector, like expected by X3D fields
      OrthoViewpoint.fieldOfView or DirectionalLight.projectionRectangle. }
    class function FromX3DVector(const V: TVector4Single): TFloatRectangle; static;
  end;

  PFloatRectangle = ^TFloatRectangle;

  TRectangleList = class(specialize TGenericStructList<TRectangle>)
  public
    { Index of the first rectangle that contains point (X, Y).
      Returns -1 if not found. }
    function FindRectangle(const X, Y: Integer): Integer;
    function FindRectangle(const Point: TVector2Single): Integer;
  end;

  TFloatRectangleList = specialize TGenericStructList<TFloatRectangle>;

function Rectangle(const Left, Bottom: Integer;
  const Width, Height: Cardinal): TRectangle;
function Rectangle(const LeftBottom: TVector2Integer;
  const Width, Height: Cardinal): TRectangle;
function FloatRectangle(const Left, Bottom, Width, Height: Single): TFloatRectangle;
function FloatRectangle(const R: TRectangle): TFloatRectangle;
function FloatRectangle(const LeftBottom: TVector2Single;
  const Width, Height: Single): TFloatRectangle;

{ Sum of the two rectangles is a bounding rectangle -
  a smallest rectangle that contains them both. }
operator+ (const R1, R2: TRectangle): TRectangle;
operator+ (const R1, R2: TFloatRectangle): TFloatRectangle;

{ Common part of the two rectangles. }
operator* (const R1, R2: TRectangle): TRectangle;
operator* (const R1, R2: TFloatRectangle): TFloatRectangle;

implementation

uses SysUtils, Math,
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

function TRectangle.IsEmpty: boolean;
begin
  Result := (Width <= 0) or (Height <= 0);
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

function TRectangle.CenterInside(const W, H: Cardinal): TRectangle;
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
  MinVar(W, Width);
  Result.Left += W;
  Result.Width -= W;
end;

function TRectangle.RemoveBottom(H: Cardinal): TRectangle;
begin
  Result := Self;
  MinVar(H, Height);
  Result.Bottom += H;
  Result.Height -= H;
end;

function TRectangle.RemoveRight(W: Cardinal): TRectangle;
begin
  Result := Self;
  MinVar(W, Width);
  Result.Width -= W;
end;

function TRectangle.RemoveTop(H: Cardinal): TRectangle;
begin
  Result := Self;
  MinVar(H, Height);
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
  MinVar(W, Width);
  Result.Width := W;
end;

function TRectangle.BottomPart(H: Cardinal): TRectangle;
begin
  Result := Self;
  MinVar(H, Height);
  Result.Height := H;
end;

function TRectangle.RightPart(W: Cardinal): TRectangle;
begin
  Result := Self;
  MinVar(W, Width);
  Result.Left += Width - W;
  Result.Width := W;
end;

function TRectangle.TopPart(H: Cardinal): TRectangle;
begin
  Result := Self;
  MinVar(H, Height);
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

function TRectangle.Center: TVector2Integer;
begin
  Result := Vector2Integer(Left + Width div 2, Bottom + Height div 2);
end;

function TRectangle.Middle: TVector2Integer;
begin
  Result := Center;
end;

function TRectangle.ScaleToWidth(const NewWidth: Cardinal): TRectangle;
begin
  if IsEmpty then Exit(Empty);
  Result.Left := Left;
  Result.Bottom := Bottom;
  Result.Width := NewWidth;
  Result.Height := Height * NewWidth div Width;
end;

function TRectangle.ScaleToHeight(const NewHeight: Cardinal): TRectangle;
begin
  if IsEmpty then Exit(Empty);
  Result.Left := Left;
  Result.Bottom := Bottom;
  Result.Width := Width * NewHeight div Height;
  Result.Height := NewHeight;
end;

function TRectangle.ScaleAroundMiddle(const Factor: Single): TRectangle;
begin
  Result := ScaleAroundCenter(Factor);
end;

function TRectangle.ScaleAroundCenter(const Factor: Single): TRectangle;
begin
  if Width > 0 then
  begin
    Result.Width  := Round(Width  * Factor);
    Result.Left   := Left   + (Width  - Result.Width ) div 2;
  end else
  begin
    Result.Width  := Width;
    Result.Left   := Left;
  end;

  if Height > 0 then
  begin
    Result.Height := Round(Height * Factor);
    Result.Bottom := Bottom + (Height - Result.Height) div 2;
  end else
  begin
    Result.Height := Height;
    Result.Bottom := Bottom;
  end;
end;

function TRectangle.ScaleAround0(const Factor: Single): TRectangle;
var
  ResultRight, ResultTop: Integer;
begin
  if Width > 0 then
  begin
    Result.Left   := Floor(Left * Factor);
    ResultRight := Ceil(Right * Factor);
    Result.Width  := ResultRight - Result.Left;
  end else
  begin
    Result.Width  := Width;
    Result.Left   := Left;
  end;

  if Height > 0 then
  begin
    Result.Bottom := Floor(Bottom * Factor);
    ResultTop   := Ceil(Top * Factor);
    Result.Height := ResultTop - Result.Bottom;
  end else
  begin
    Result.Height := Height;
    Result.Bottom := Bottom;
  end;
end;

function TRectangle.ScaleWidthAround0(const Factor: Single): Cardinal;
begin
  if Width > 0 then
    Result := Ceil(Right * Factor) - Floor(Left * Factor) else
    Result := Width;
end;

function TRectangle.ScaleHeightAround0(const Factor: Single): Cardinal;
begin
  if Height > 0 then
    Result := Ceil(Top * Factor) - Floor(Bottom * Factor) else
    Result := Height;
end;

function TRectangle.FitInside(const R: TRectangle;
  const AlignHorizontal: THorizontalPosition = hpMiddle;
  const AlignVertical: TVerticalPosition = vpMiddle): TRectangle;
begin
  if R.Width / R.Height > Width / Height then
  begin
    Result.Height := R.Height;
    Result.Width := Width * Result.Height div Height;
  end else
  begin
    Result.Width := R.Width;
    Result.Height := Height * Result.Width div Width;
  end;
  Result.Left   := Result.AlignCore(AlignHorizontal, R, AlignHorizontal, 0);
  Result.Bottom := Result.AlignCore(AlignVertical, R, AlignVertical, 0);
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

function TRectangle.Translate(const V: TVector2Integer): TRectangle;
begin
  Result.Left := Left + V[0];
  Result.Bottom := Bottom + V[1];
  Result.Width := Width;
  Result.Height := Height;
end;

function TRectangle.Collides(const R: TRectangle): boolean;
begin
  Result :=
    (not IsEmpty) and
    (not R.IsEmpty) and
    (not ((  Right - 1 < R.Left) or
          (R.Right - 1 <   Left))) and
    (not ((  Top   - 1 < R.Bottom) or
          (R.Top   - 1 <   Bottom)));
end;

{ TFloatRectangle ----------------------------------------------------------------- }

function FloatRectangle(const Left, Bottom, Width, Height: Single): TFloatRectangle;
begin
  Result.Left   := Left;
  Result.Bottom := Bottom;
  Result.Width  := Width;
  Result.Height := Height;
end;

function FloatRectangle(const R: TRectangle): TFloatRectangle;
begin
  Result.Left   := R.Left;
  Result.Bottom := R.Bottom;
  Result.Width  := R.Width;
  Result.Height := R.Height;
end;

function FloatRectangle(const LeftBottom: TVector2Single;
  const Width, Height: Single): TFloatRectangle;
begin
  Result.Left   := LeftBottom[0];
  Result.Bottom := LeftBottom[1];
  Result.Width  := Width;
  Result.Height := Height;
end;

function TFloatRectangle.IsEmpty: boolean;
begin
  Result := (Width < 0) or (Height < 0);
end;

function TFloatRectangle.Contains(const X, Y: Single): boolean;
begin
  Result := (X >= Left  ) and (X <= Left   + Width) and
            (Y >= Bottom) and (Y <= Bottom + Height);
end;

function TFloatRectangle.Contains(const Point: TVector2Single): boolean;
begin
  Result := (Point[0] >= Left  ) and (Point[0] <= Left   + Width) and
            (Point[1] >= Bottom) and (Point[1] <= Bottom + Height);
end;

function TFloatRectangle.Center: TVector2Single;
begin
  Result := Vector2Single(Left + Width / 2, Bottom + Height / 2);
end;

function TFloatRectangle.Middle: TVector2Single;
begin
  Result := Center;
end;

function TFloatRectangle.Grow(const DeltaX, DeltaY: Single): TFloatRectangle;
begin
  if Width + 2 * DeltaX < 0 then
  begin
    Result.Left := Left + Width / 2;
    Result.Width := 0;
  end else
  begin
    Result.Left := Left - DeltaX;
    Result.Width := Width + 2 * DeltaX;
  end;

  if Height + 2 * DeltaY < 0 then
  begin
    Result.Bottom := Bottom + Height / 2;
    Result.Height := 0;
  end else
  begin
    Result.Bottom := Bottom - DeltaY;
    Result.Height := Height + 2 * DeltaY;
  end;
end;

function TFloatRectangle.Grow(const Delta: Single): TFloatRectangle;
begin
  Result := Grow(Delta, Delta);
end;

function TFloatRectangle.GetRight: Single;
begin
  Result := Left + Width;
end;

function TFloatRectangle.GetTop: Single;
begin
  Result := Bottom + Height;
end;

function TFloatRectangle.ToString: string;
begin
  Result := Format('TFloatRectangle: %fx%f %fx%f', [Left, Bottom, Width, Height]);
end;

function TFloatRectangle.Translate(const V: TVector2Single): TFloatRectangle;
begin
  Result.Left := Left + V[0];
  Result.Bottom := Bottom + V[1];
  Result.Width := Width;
  Result.Height := Height;
end;

function TFloatRectangle.Collides(const R: TFloatRectangle): boolean;
begin
  Result :=
    (not IsEmpty) and
    (not R.IsEmpty) and
    (not ((  Right < R.Left) or
          (R.Right <   Left))) and
    (not ((  Top   < R.Bottom) or
          (R.Top   <   Bottom)));
end;

function TFloatRectangle.CollidesDisc(const DiscCenter: TVector2Single;
  const Radius: Single): boolean;
var
  ARight, ATop, ClosestCornerX, ClosestCornerY: Single;
  InsideX, InsideY: boolean;
begin
  if IsEmpty then
    Exit(false);

  ARight := Left + Width;
  ATop   := Bottom + Height;

  if DiscCenter[0] < Left then
  begin
    InsideX := false;
    ClosestCornerX := Left;
    if Left - DiscCenter[0] > Radius then Exit(false);
  end else
  if DiscCenter[0] > ARight then
  begin
    InsideX := false;
    ClosestCornerX := ARight;
    if DiscCenter[0] - ARight > Radius then Exit(false);
  end else
  begin
    InsideX := true;
    if DiscCenter[0] < (Left + ARight) / 2 then
      ClosestCornerX := Left
    else
      ClosestCornerX := ARight;
  end;

  if DiscCenter[1] < Bottom then
  begin
    InsideY := false;
    ClosestCornerY := Bottom;
    if Bottom - DiscCenter[1] > Radius then Exit(false);
  end else
  if DiscCenter[1] > ATop then
  begin
    InsideY := false;
    ClosestCornerY := ATop;
    if DiscCenter[1] - ATop > Radius then Exit(false);
  end else
  begin
    InsideY := true;
    if DiscCenter[1] < (Bottom + ATop) / 2 then
      ClosestCornerY := Bottom
    else
      ClosestCornerY := ATop;
  end;

  { If we get here, then DiscCenter is within a Radius margin from the rectangle.
    In other words, the bounding rect of the cirle for sure collides with
    this rectangle.
    The only way for the circle not to collide, is to be at the very corner
    of such "rectangle enlarged by Radius". }

  if InsideX or InsideY then
    Exit(true);

  Result :=
    Sqr(DiscCenter[0] - ClosestCornerX) +
    Sqr(DiscCenter[1] - ClosestCornerY) <=
    Sqr(Radius);
end;

function TFloatRectangle.ScaleAround0(const Factor: Single): TFloatRectangle;
var
  ResultRight, ResultTop: Single;
begin
  if Width >= 0 then
  begin
    Result.Left  := Left * Factor;
    ResultRight  := Right * Factor;
    Result.Width := ResultRight - Result.Left;
  end else
  begin
    Result.Width  := Width;
    Result.Left   := Left;
  end;

  if Height >= 0 then
  begin
    Result.Bottom := Bottom * Factor;
    ResultTop     := Top * Factor;
    Result.Height := ResultTop - Result.Bottom;
  end else
  begin
    Result.Height := Height;
    Result.Bottom := Bottom;
  end;
end;

function TFloatRectangle.Add(const P: TVector2Single): TFloatRectangle;
begin
  if IsEmpty then
  begin
    Result.Left := P[0];
    Result.Bottom := P[1];
    Result.Width := 0;
    Result.Height := 0;
  end else
  begin
    if P[0] < Left then
    begin
      Result.Left := P[0];
      Result.Width := (Left - P[0]) + Width;
    end else
    begin
      Result.Left := Left;
      if P[0] > Right then
        Result.Width := Width + P[0] - Right
      else
        Result.Width := Width;
    end;

    if P[1] < Bottom then
    begin
      Result.Bottom := P[1];
      Result.Height := (Bottom - P[1]) + Height;
    end else
    begin
      Result.Bottom := Bottom;
      if P[1] > Top then
        Result.Height := Height + P[1] - Top
      else
        Result.Height := Height;
    end;
  end;
end;

function TFloatRectangle.ToX3DVector: TVector4Single;
begin
  Result := Vector4Single(
    Left,
    Bottom,
    Right,
    Top);
end;

class function TFloatRectangle.FromX3DVector(const V: TVector4Single): TFloatRectangle;
begin
  Result.Left   := V[0];
  Result.Bottom := V[1];
  Result.Width  := V[2] - V[0];
  Result.Height := V[3] - V[1];
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

{ operators ------------------------------------------------------------------ }

operator+ (const R1, R2: TRectangle): TRectangle;
var
  Right, Top: Integer;
begin
  if R1.IsEmpty then
    Result := R2 else
  if R2.IsEmpty then
    Result := R1 else
  begin
    Result.Left   := Min(R1.Left  , R2.Left);
    Result.Bottom := Min(R1.Bottom, R2.Bottom);
    Right := Max(R1.Right   , R2.Right);
    Top   := Max(R1.Top     , R2.Top);
    Result.Width  := Right - Result.Left;
    Result.Height := Top   - Result.Bottom;
  end;
end;

operator+ (const R1, R2: TFloatRectangle): TFloatRectangle;
var
  Right, Top: Single;
begin
  if R1.IsEmpty then
    Result := R2 else
  if R2.IsEmpty then
    Result := R1 else
  begin
    Result.Left   := Min(R1.Left  , R2.Left);
    Result.Bottom := Min(R1.Bottom, R2.Bottom);
    Right := Max(R1.Right   , R2.Right);
    Top   := Max(R1.Top     , R2.Top);
    Result.Width  := Right - Result.Left;
    Result.Height := Top   - Result.Bottom;
  end;
end;

operator* (const R1, R2: TRectangle): TRectangle;
var
  Right, Top: Integer;
begin
  if R1.IsEmpty or R2.IsEmpty then
    Result := TRectangle.Empty else
  begin
    Result.Left   := Max(R1.Left  , R2.Left);
    Result.Bottom := Max(R1.Bottom, R2.Bottom);
    Right := Min(R1.Right   , R2.Right);
    Top   := Min(R1.Top     , R2.Top);
    if (Right > Result.Left) and (Top > Result.Bottom) then
    begin
      Result.Width  := Right - Result.Left;
      Result.Height := Top   - Result.Bottom;
    end else
      Result := TRectangle.Empty;
  end;
end;

operator* (const R1, R2: TFloatRectangle): TFloatRectangle;
var
  Right, Top: Single;
begin
  if R1.IsEmpty or R2.IsEmpty then
    Result := TFloatRectangle.Empty else
  begin
    Result.Left   := Max(R1.Left  , R2.Left);
    Result.Bottom := Max(R1.Bottom, R2.Bottom);
    Right := Min(R1.Right   , R2.Right);
    Top   := Min(R1.Top     , R2.Top);
    { ">=" unline the int version that checks ">".
      For TFloatRectangle, having zero size makes sense. }
    if (Right >= Result.Left) and (Top >= Result.Bottom) then
    begin
      { use Max(0, ..) to secure from floating point errors in case equations above
        are true but subtraction yields < 0 due to floating point inaccuracy.
        Not sure is this possible (A >= B and still A - B < 0), probably not,
        but better stay safe when dealing with floating point numbers. }
      Result.Width  := Max(0, Right - Result.Left);
      Result.Height := Max(0, Top   - Result.Bottom);
    end else
      Result := TFloatRectangle.Empty;
  end;
end;

end.
