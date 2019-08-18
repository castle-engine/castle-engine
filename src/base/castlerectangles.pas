{
  Copyright 2006-2018 Michalis Kamburelis.

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

uses Generics.Collections,
  CastleVectors, CastleUtils;

type
  { Horizontal position of one control/rectangle
    with respect to another.

    This is used by @link(TCastleUserInterface.Anchor),
    @link(TRectangle.Align), @link(TFloatRectangle.Align) and other methods
    to specify the alignment of one control/rectangle with respect to another.

    Note that @link(TCastleUserInterface.Anchor) has various overloaded
    versions. E.g. you can align the left side of the control to the left side
    of the parent (most common situation), or you can align left side
    of the control to the middle of the parent...

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
  TRectangle = record
  private
    function GetRight: Integer;
    function GetTop: Integer;
    {
    // The setters for Right and Top are deliberately commented out.
    // They are a little tricky, as they would actually set Width / Height.
    // - So setting "Left := 10; Right := 20;" works as expected,
    //   but setting "Right := 20; Left := 10;"... does not (it depends
    //   on previous rectangle value).
    // - Also, they would be necessarily inconsistent between int TRectangle
    //   and float TFloatRectangle, since int TRectangle does not allow
    //   negative Width / Height.
    // So it's probably less error-prone to leave them commented out.
    procedure SetRight(const Value: Integer);
    procedure SetTop(const Value: Integer);
    }
    function GetLeftBottom: TVector2Integer;
    procedure SetLeftBottom(const Value: TVector2Integer);
  public
    Left, Bottom: Integer;
    Width, Height: Cardinal;

    {$ifdef ENABLE_SELF_RECORD_CONSTANTS}
    const
      Empty: TRectangle = (Left: 0; Bottom: 0; Width: 0; Height: 0);
    {$else}
    class function Empty: TRectangle; static; inline;
    {$endif}

    function IsEmpty: boolean;

    function Contains(const X, Y: Integer): boolean; overload;
    function Contains(const Point: TVector2): boolean; overload;
    function Contains(const Point: TVector2Integer): boolean; overload;

    { Right and top coordinates of the rectangle.
      @code(Right) is simply the @code(Left + Width),
      @code(Top) is simply the @code(Bottom + Height).

      If you use this for drawing, note that the pixel
      with coordinates @code((Right, Top)) is actually *outside*
      of the rectangle (by 1 pixel). That's because the rectangle starts at
      pixel @code((Left, Bottom)) and spans the @code((Width, Height)) pixels.
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
    function Grow(const Delta: Integer): TRectangle; overload;
    function Grow(const DeltaX, DeltaY: Integer): TRectangle; overload;

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

      These details matter, e.g. when you set @link(TCastleUserInterface.Width), but not
      @link(TCastleUserInterface.Height),
      and then you expect the @link(TCastleUserInterface.EffectiveWidth) to work.
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
      const X: Integer = 0): Integer; overload;
    function Align(
      const ThisPosition: THorizontalPosition;
      const OtherRect: TRectangle;
      const OtherPosition: THorizontalPosition;
      const X: Integer = 0): TRectangle; overload;

    { Align this rectangle within other rectangle by calculating new value
      for @link(Bottom). }
    function AlignCore(
      const ThisPosition: TVerticalPosition;
      const OtherRect: TRectangle;
      const OtherPosition: TVerticalPosition;
      const Y: Integer = 0): Integer; overload;
    function Align(
      const ThisPosition: TVerticalPosition;
      const OtherRect: TRectangle;
      const OtherPosition: TVerticalPosition;
      const Y: Integer = 0): TRectangle; overload;

    function ToString: string;

    { Move the rectangle. Empty rectangle after moving is still an empty rectangle. }
    function Translate(const V: TVector2Integer): TRectangle;

    { Does it have any common part with another rectangle. }
    function Collides(const R: TRectangle): boolean;

    { Sum of the two rectangles is a bounding rectangle -
      a smallest rectangle that contains them both. }
    class operator {$ifdef FPC}+{$else}Add{$endif} (const R1, R2: TRectangle): TRectangle;

    { Common part of the two rectangles. }
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const R1, R2: TRectangle): TRectangle;

    function Equals(const R: TRectangle): boolean;
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
    (a single 2D or 3D point has zero size, but it also still has a position). }
  TFloatRectangle = record
  private
    function GetRight: Single;
    function GetTop: Single;
    {
    // Commented out -- see TRectangle comments.
    procedure SetRight(const Value: Single);
    procedure SetTop(const Value: Single);
    }
    function GetLeftBottom: TVector2;
    procedure SetLeftBottom(const Value: TVector2);
    function GetSize: TVector2;
    procedure SetSize(const Value: TVector2);
  public
    Left, Bottom: Single;
    Width, Height: Single;

    {$ifdef ENABLE_SELF_RECORD_CONSTANTS}
    const
      Empty: TFloatRectangle = (Left: 0; Bottom: 0; Width: -1; Height: -1);
    {$else}
    class function Empty: TFloatRectangle; static; inline;
    {$endif}

    function IsEmpty: boolean;

    function Contains(const X, Y: Single): boolean; overload;
    function Contains(const Point: TVector2): boolean; overload;
    function Contains(const R: TFloatRectangle): boolean; overload;

    { Right and top coordinates of the rectangle.
      @code(Right) is simply the @code(Left + Width),
      @code(Top) is simply the @code(Bottom + Height).

      Note: If you use this for drawing,
      and the values of Left, Bottom, Width, Height
      are actually integers (or close to integers),
      then the pixel with @code((Round(Right), Round(Top)))
      coordinates is actually *outside* of the rectangle (by 1 pixel).
      That's because the rectangle starts at the pixel
      @code((Round(Left), Round(Bottom))) and
      spans the @code((Round(Width), Round(Height))) pixels.
      @groupBegin }
    property Right: Single read GetRight;
    property Top: Single read GetTop;
    { @groupEnd }

    property LeftBottom: TVector2 read GetLeftBottom write SetLeftBottom;
    function Middle: TVector2; deprecated 'use Center';
    function Center: TVector2;

    { Return rectangle with given width and height centered
      in the middle of this rectangle. The given W, H may be smaller or larger
      than this rectangle sizes. }
    function CenterInside(const W, H: Single): TFloatRectangle;

    { Grow (when Delta > 0) or shrink (when Delta < 0)
      the rectangle, returning new value.
      This adds a margin of Delta pixels around all sides of the rectangle,
      so in total width grows by 2 * Delta, and the same for height.
      In case of shrinking, we protect from shrinking too much:
      the resulting width or height is set to zero (which makes a valid
      and empty rectangle) if shrinking too much. }
    function Grow(const Delta: Single): TFloatRectangle; overload;
    function Grow(const DeltaX, DeltaY: Single): TFloatRectangle; overload;

    { Returns the rectangle with a number of pixels from given
      side removed. Returns an empty rectangle if you try to remove too much.
      @groupBegin }
    function RemoveLeft(W: Single): TFloatRectangle;
    function RemoveBottom(H: Single): TFloatRectangle;
    function RemoveRight(W: Single): TFloatRectangle;
    function RemoveTop(H: Single): TFloatRectangle;
    { @groupEnd }

    { Returns the rectangle with a number of pixels on given side added.
      @groupBegin }
    function GrowLeft(const W: Single): TFloatRectangle;
    function GrowBottom(const H: Single): TFloatRectangle;
    function GrowRight(const W: Single): TFloatRectangle;
    function GrowTop(const H: Single): TFloatRectangle;
    { @groupEnd }

    { Returns the given side of the rectangle, cut down to given number of pixels
      from given side. This is similar to RemoveXxx methods, but here you specify
      which side to keep, as opposed to RemoveXxx methods where you specify which
      side you remove.

      If the requested size is larger than current size (for example,
      W > Width for LeftPart) then the unmodified rectangle is returned.

      @groupBegin }
    function LeftPart(W: Single): TFloatRectangle;
    function BottomPart(H: Single): TFloatRectangle;
    function RightPart(W: Single): TFloatRectangle;
    function TopPart(H: Single): TFloatRectangle;
    { @groupEnd }

    { Align this rectangle within other rectangle by calculating new value
      for @link(Left). }
    function AlignCore(
      const ThisPosition: THorizontalPosition;
      const OtherRect: TFloatRectangle;
      const OtherPosition: THorizontalPosition;
      const X: Single = 0): Single; overload;
    function Align(
      const ThisPosition: THorizontalPosition;
      const OtherRect: TFloatRectangle;
      const OtherPosition: THorizontalPosition;
      const X: Single = 0): TFloatRectangle; overload;

    { Align this rectangle within other rectangle by calculating new value
      for @link(Bottom). }
    function AlignCore(
      const ThisPosition: TVerticalPosition;
      const OtherRect: TFloatRectangle;
      const OtherPosition: TVerticalPosition;
      const Y: Single = 0): Single; overload;
    function Align(
      const ThisPosition: TVerticalPosition;
      const OtherRect: TFloatRectangle;
      const OtherPosition: TVerticalPosition;
      const Y: Single = 0): TFloatRectangle; overload;

    function ToString: string;

    { Move the rectangle. Empty rectangle after moving is still an empty rectangle. }
    function Translate(const V: TVector2): TFloatRectangle;

    { Does it have any common part with another rectangle. }
    function Collides(const R: TFloatRectangle): boolean;

    function CollidesDisc(const DiscCenter: TVector2; const Radius: Single): boolean;

    function ScaleToWidth(const NewWidth: Single): TFloatRectangle;
    function ScaleToHeight(const NewHeight: Single): TFloatRectangle;

    { Scale rectangle position and size around it's own @link(Center) point.

      Since the scaling is independent in each axis,
      this handles "carefully" a half-empty rectangles
      (when one size is <= 0, but other is > 0).
      It scales correctly the positive dimension
      (not just returns @link(Empty) constant),
      leaving the other dimension (it's position and size) untouched. }
    function ScaleAroundCenter(const Factor: Single): TFloatRectangle;

    { Scale rectangle position and size around the (0,0) point. }
    function ScaleAround0(const Factor: Single): TFloatRectangle;

    { Scale and align us to fit inside rectangle R, preserving our aspect ratio. }
    function FitInside(const R: TFloatRectangle;
      const AlignHorizontal: THorizontalPosition = hpMiddle;
      const AlignVertical: TVerticalPosition = vpMiddle): TFloatRectangle;

    { Return larger rectangle, so that it includes given point. }
    function Include(const P: TVector2): TFloatRectangle;

    { Convert to a 4D vector, like expected by X3D fields
      OrthoViewpoint.fieldOfView or DirectionalLight.projectionRectangle. }
    function ToX3DVector: TVector4;

    { Convert from a 4D vector, like expected by X3D fields
      OrthoViewpoint.fieldOfView or DirectionalLight.projectionRectangle. }
    class function FromX3DVector(const V: TVector4): TFloatRectangle; static;

    { Round rectangle coordinates, converting TFloatRectangle to TRectangle. }
    function Round: TRectangle;

    { Is another rectangle equal to this one.
      Floating-point values are compared with an epsilon tolerance. }
    function Equals(const R: TFloatRectangle): Boolean; overload;

    { Is another rectangle equal to this one.
      Floating-point values are compared with an epsilon tolerance. }
    function Equals(const R: TFloatRectangle; const Epsilon: Single): Boolean; overload;

    { Sum of the two rectangles is a bounding rectangle -
      a smallest rectangle that contains them both. }
    class operator {$ifdef FPC}+{$else}Add{$endif} (const R1, R2: TFloatRectangle): TFloatRectangle;

    { Common part of the two rectangles. }
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const R1, R2: TFloatRectangle): TFloatRectangle;

    { Alternative way to access @link(Width) and @link(Height).
      Name consistent with TBoxNode.Size, TBox3D.Size. }
    property Size: TVector2 read GetSize write SetSize;
  end;

  PFloatRectangle = ^TFloatRectangle;
  TFloatRectangleArray = packed array [0..MaxInt div SizeOf(TFloatRectangle) - 1] of TFloatRectangle;
  PFloatRectangleArray = ^TFloatRectangleArray;

  TRectangleList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<TRectangle>)
  public
    { Index of the first rectangle that contains point (X, Y).
      Returns -1 if not found. }
    function FindRectangle(const X, Y: Integer): Integer; overload;
    function FindRectangle(const Point: TVector2): Integer; overload;
  end;

  TFloatRectangleList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<TFloatRectangle>;

function Rectangle(const Left, Bottom: Integer;
  const Width, Height: Cardinal): TRectangle; overload;
function Rectangle(const LeftBottom: TVector2Integer;
  const Width, Height: Cardinal): TRectangle; overload;
function FloatRectangle(const Left, Bottom, Width, Height: Single): TFloatRectangle; overload;
function FloatRectangle(const R: TRectangle): TFloatRectangle; overload;
function FloatRectangle(const LeftBottom: TVector2;
  const Width, Height: Single): TFloatRectangle; overload;

implementation

uses SysUtils, Math;

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
  Result.Left := LeftBottom.Data[0];
  Result.Bottom := LeftBottom.Data[1];
  Result.Width := Width;
  Result.Height := Height;
end;

{$ifndef ENABLE_SELF_RECORD_CONSTANTS}
class function TRectangle.Empty: TRectangle;
begin
  FillChar(Result, SizeOf(Result), 0);
end;
{$endif}

function TRectangle.IsEmpty: boolean;
begin
  Result := (Width <= 0) or (Height <= 0);
end;

function TRectangle.Contains(const X, Y: Integer): boolean;
begin
  Result := (X >= Left  ) and (X < Left   + Integer(Width)) and
            (Y >= Bottom) and (Y < Bottom + Integer(Height));
end;

function TRectangle.Contains(const Point: TVector2): boolean;
begin
  Result := (Point.Data[0] >= Left  ) and (Point.Data[0] < Left   + Integer(Width)) and
            (Point.Data[1] >= Bottom) and (Point.Data[1] < Bottom + Integer(Height));
end;

function TRectangle.Contains(const Point: TVector2Integer): boolean;
begin
  Result := (Point.Data[0] >= Left  ) and (Point.Data[0] < Left   + Integer(Width)) and
            (Point.Data[1] >= Bottom) and (Point.Data[1] < Bottom + Integer(Height));
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

{
procedure TRectangle.SetRight(const Value: Integer);
begin
  if Value <= Left then
    Width := 0
  else
    Width := Value - Left;
end;

procedure TRectangle.SetTop(const Value: Integer);
begin
  if Value <= Bottom then
    Height := 0
  else
    Height := Value - Bottom;
end;
}

function TRectangle.RemoveLeft(W: Cardinal): TRectangle;
begin
  Result := Self;
  MinVar(W, Width);
  Result.Left := Result.Left + W;
  Result.Width := Result.Width - W;
end;

function TRectangle.RemoveBottom(H: Cardinal): TRectangle;
begin
  Result := Self;
  MinVar(H, Height);
  Result.Bottom := Result.Bottom + H;
  Result.Height := Result.Height - H;
end;

function TRectangle.RemoveRight(W: Cardinal): TRectangle;
begin
  Result := Self;
  MinVar(W, Width);
  Result.Width := Result.Width - W;
end;

function TRectangle.RemoveTop(H: Cardinal): TRectangle;
begin
  Result := Self;
  MinVar(H, Height);
  Result.Height := Result.Height - H;
end;

function TRectangle.GrowLeft(const W: Cardinal): TRectangle;
begin
  Result := Self;
  Result.Left := Result.Left - Integer(W);
  Result.Width := Result.Width + W;
end;

function TRectangle.GrowBottom(const H: Cardinal): TRectangle;
begin
  Result := Self;
  Result.Bottom := Result.Bottom - Integer(H);
  Result.Height := Result.Height + H;
end;

function TRectangle.GrowRight(const W: Cardinal): TRectangle;
begin
  Result := Self;
  Result.Width := Result.Width + W;
end;

function TRectangle.GrowTop(const H: Cardinal): TRectangle;
begin
  Result := Self;
  Result.Height := Result.Height + H;
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
  Result.Left := Result.Left + Width - W;
  Result.Width := W;
end;

function TRectangle.TopPart(H: Cardinal): TRectangle;
begin
  Result := Self;
  MinVar(H, Height);
  Result.Bottom := Result.Bottom + Height - H;
  Result.Height := H;
end;

function TRectangle.GetLeftBottom: TVector2Integer;
begin
  Result.Data[0] := Left;
  Result.Data[1] := Bottom;
end;

procedure TRectangle.SetLeftBottom(const Value: TVector2Integer);
begin
  Left := Value.Data[0];
  Bottom := Value.Data[1];
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
  if IsEmpty then
    Result := 'TRectangle: Empty'
  else
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
    hpMiddle: Result := Result - Width div 2;
    hpRight : Result := Result - Width;
  end;
  case OtherPosition of
    hpLeft  : ;
    hpMiddle: Result := Result + OtherRect.Width div 2;
    hpRight : Result := Result + OtherRect.Width;
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
    vpMiddle: Result := Result - Height div 2;
    vpTop   : Result := Result - Height;
  end;
  case OtherPosition of
    vpBottom: ;
    vpMiddle: Result := Result + OtherRect.Height div 2;
    vpTop   : Result := Result + OtherRect.Height;
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
  Result.Left := Left + V.Data[0];
  Result.Bottom := Bottom + V.Data[1];
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

class operator TRectangle.{$ifdef FPC}+{$else}Add{$endif} (const R1, R2: TRectangle): TRectangle;
var
  NewRight, NewTop: Integer;
begin
  if R1.IsEmpty then
    Result := R2 else
  if R2.IsEmpty then
    Result := R1 else
  begin
    Result.Left   := Min(R1.Left  , R2.Left);
    Result.Bottom := Min(R1.Bottom, R2.Bottom);
    NewRight := Max(R1.Right   , R2.Right);
    NewTop   := Max(R1.Top     , R2.Top);
    Result.Width  := NewRight - Result.Left;
    Result.Height := NewTop   - Result.Bottom;
  end;
end;

class operator TRectangle.{$ifdef FPC}*{$else}Multiply{$endif} (const R1, R2: TRectangle): TRectangle;
var
  NewRight, NewTop: Integer;
begin
  if R1.IsEmpty or R2.IsEmpty then
    Result := TRectangle.Empty else
  begin
    Result.Left   := Max(R1.Left  , R2.Left);
    Result.Bottom := Max(R1.Bottom, R2.Bottom);
    NewRight := Min(R1.Right   , R2.Right);
    NewTop   := Min(R1.Top     , R2.Top);
    if (NewRight > Result.Left) and (NewTop > Result.Bottom) then
    begin
      Result.Width  := NewRight - Result.Left;
      Result.Height := NewTop   - Result.Bottom;
    end else
      Result := TRectangle.Empty;
  end;
end;

function TRectangle.Equals(const R: TRectangle): boolean;
begin
  if IsEmpty then
    Result := R.IsEmpty
  else
    Result :=
      (not R.IsEmpty) and
      (Left   = R.Left) and
      (Bottom = R.Bottom) and
      (Width  = R.Width) and
      (Height = R.Height);
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
  if R.IsEmpty then
    Exit(TFloatRectangle.Empty);
  Result.Left   := R.Left;
  Result.Bottom := R.Bottom;
  Result.Width  := R.Width;
  Result.Height := R.Height;
end;

function FloatRectangle(const LeftBottom: TVector2;
  const Width, Height: Single): TFloatRectangle;
begin
  Result.Left   := LeftBottom.Data[0];
  Result.Bottom := LeftBottom.Data[1];
  Result.Width  := Width;
  Result.Height := Height;
end;

{$ifndef ENABLE_SELF_RECORD_CONSTANTS}
class function TFloatRectangle.Empty: TFloatRectangle;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Width := -1;
  Result.Height := -1;
end;
{$endif}

function TFloatRectangle.IsEmpty: boolean;
begin
  Result := (Width < 0) or (Height < 0);
end;

function TFloatRectangle.Contains(const X, Y: Single): boolean;
begin
  Result := (X >= Left  ) and (X <= Left   + Width) and
            (Y >= Bottom) and (Y <= Bottom + Height);
end;

function TFloatRectangle.Contains(const Point: TVector2): boolean;
begin
  Result := (Point.Data[0] >= Left  ) and (Point.Data[0] <= Left   + Width) and
            (Point.Data[1] >= Bottom) and (Point.Data[1] <= Bottom + Height);
end;

function TFloatRectangle.Contains(const R: TFloatRectangle): boolean;
begin
  if R.IsEmpty then
    Result := true
  else
    Result :=
      (not IsEmpty) and
      (R.Left >= Left) and
      (R.Bottom >= Bottom) and
      (R.Right <= Right) and
      (R.Top <= Top);
end;

function TFloatRectangle.GetLeftBottom: TVector2;
begin
  Result.Data[0] := Left;
  Result.Data[1] := Bottom;
end;

procedure TFloatRectangle.SetLeftBottom(const Value: TVector2);
begin
  Left := Value.Data[0];
  Bottom := Value.Data[1];
end;

function TFloatRectangle.GetSize: TVector2;
begin
  Result.Data[0] := Width;
  Result.Data[1] := Height;
end;

procedure TFloatRectangle.SetSize(const Value: TVector2);
begin
  Width := Value.Data[0];
  Height := Value.Data[1];
end;

function TFloatRectangle.Center: TVector2;
begin
  Result := Vector2(Left + Width / 2, Bottom + Height / 2);
end;

function TFloatRectangle.Middle: TVector2;
begin
  Result := Center;
end;

function TFloatRectangle.CenterInside(const W, H: Single): TFloatRectangle;
begin
  Result.Left   := Left   + (Width  - W) / 2;
  Result.Bottom := Bottom + (Height - H) / 2;
  Result.Width  := W;
  Result.Height := H;
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

function TFloatRectangle.RemoveLeft(W: Single): TFloatRectangle;
begin
  Result := Self;
  MinVar(W, Width);
  Result.Left := Result.Left + W;
  Result.Width := Result.Width - W;
end;

function TFloatRectangle.RemoveBottom(H: Single): TFloatRectangle;
begin
  Result := Self;
  MinVar(H, Height);
  Result.Bottom := Result.Bottom + H;
  Result.Height := Result.Height - H;
end;

function TFloatRectangle.RemoveRight(W: Single): TFloatRectangle;
begin
  Result := Self;
  MinVar(W, Width);
  Result.Width := Result.Width - W;
end;

function TFloatRectangle.RemoveTop(H: Single): TFloatRectangle;
begin
  Result := Self;
  MinVar(H, Height);
  Result.Height := Result.Height - H;
end;

function TFloatRectangle.GrowLeft(const W: Single): TFloatRectangle;
begin
  Result := Self;
  Result.Left := Result.Left - W;
  Result.Width := Result.Width + W;
end;

function TFloatRectangle.GrowBottom(const H: Single): TFloatRectangle;
begin
  Result := Self;
  Result.Bottom := Result.Bottom - H;
  Result.Height := Result.Height + H;
end;

function TFloatRectangle.GrowRight(const W: Single): TFloatRectangle;
begin
  Result := Self;
  Result.Width := Result.Width + W;
end;

function TFloatRectangle.GrowTop(const H: Single): TFloatRectangle;
begin
  Result := Self;
  Result.Height := Result.Height + H;
end;

function TFloatRectangle.LeftPart(W: Single): TFloatRectangle;
begin
  Result := Self;
  MinVar(W, Width);
  Result.Width := W;
end;

function TFloatRectangle.BottomPart(H: Single): TFloatRectangle;
begin
  Result := Self;
  MinVar(H, Height);
  Result.Height := H;
end;

function TFloatRectangle.RightPart(W: Single): TFloatRectangle;
begin
  Result := Self;
  MinVar(W, Width);
  Result.Left := Result.Left + Width - W;
  Result.Width := W;
end;

function TFloatRectangle.TopPart(H: Single): TFloatRectangle;
begin
  Result := Self;
  MinVar(H, Height);
  Result.Bottom := Result.Bottom + Height - H;
  Result.Height := H;
end;

function TFloatRectangle.GetRight: Single;
begin
  Result := Left + Width;
end;

function TFloatRectangle.GetTop: Single;
begin
  Result := Bottom + Height;
end;

{
procedure TFloatRectangle.SetRight(const Value: Single);
begin
  Width := Value - Left;
end;

procedure TFloatRectangle.SetTop(const Value: Single);
begin
  Height := Value - Bottom;
end;
}

function TFloatRectangle.AlignCore(
  const ThisPosition: THorizontalPosition;
  const OtherRect: TFloatRectangle;
  const OtherPosition: THorizontalPosition;
  const X: Single = 0): Single;
begin
  Result := OtherRect.Left + X;
  case ThisPosition of
    hpLeft  : ;
    hpMiddle: Result := Result - Width / 2;
    hpRight : Result := Result - Width;
  end;
  case OtherPosition of
    hpLeft  : ;
    hpMiddle: Result := Result + OtherRect.Width / 2;
    hpRight : Result := Result + OtherRect.Width;
  end;
end;

function TFloatRectangle.AlignCore(
  const ThisPosition: TVerticalPosition;
  const OtherRect: TFloatRectangle;
  const OtherPosition: TVerticalPosition;
  const Y: Single = 0): Single;
begin
  Result := OtherRect.Bottom + Y;
  case ThisPosition of
    vpBottom: ;
    vpMiddle: Result := Result - Height / 2;
    vpTop   : Result := Result - Height;
  end;
  case OtherPosition of
    vpBottom: ;
    vpMiddle: Result := Result + OtherRect.Height / 2;
    vpTop   : Result := Result + OtherRect.Height;
  end;
end;

function TFloatRectangle.Align(
  const ThisPosition: THorizontalPosition;
  const OtherRect: TFloatRectangle;
  const OtherPosition: THorizontalPosition;
  const X: Single = 0): TFloatRectangle;
begin
  Result.Left := AlignCore(ThisPosition, OtherRect, OtherPosition, X);
  Result.Bottom := Bottom;
  Result.Width := Width;
  Result.Height := Height;
end;

function TFloatRectangle.Align(
  const ThisPosition: TVerticalPosition;
  const OtherRect: TFloatRectangle;
  const OtherPosition: TVerticalPosition;
  const Y: Single = 0): TFloatRectangle;
begin
  Result.Left := Left;
  Result.Bottom := AlignCore(ThisPosition, OtherRect, OtherPosition, Y);
  Result.Width := Width;
  Result.Height := Height;
end;

function TFloatRectangle.ToString: string;
begin
  if IsEmpty then
    Result := 'TFloatRectangle: Empty'
  else
    Result := Format('TFloatRectangle: %fx%f %fx%f', [Left, Bottom, Width, Height]);
end;

function TFloatRectangle.Translate(const V: TVector2): TFloatRectangle;
begin
  Result.Left := Left + V.Data[0];
  Result.Bottom := Bottom + V.Data[1];
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

function TFloatRectangle.CollidesDisc(const DiscCenter: TVector2;
  const Radius: Single): boolean;
var
  ARight, ATop, ClosestCornerX, ClosestCornerY: Single;
  InsideX, InsideY: boolean;
begin
  if IsEmpty then
    Exit(false);

  ARight := Left + Width;
  ATop   := Bottom + Height;

  if DiscCenter.Data[0] < Left then
  begin
    InsideX := false;
    ClosestCornerX := Left;
    if Left - DiscCenter.Data[0] > Radius then Exit(false);
  end else
  if DiscCenter.Data[0] > ARight then
  begin
    InsideX := false;
    ClosestCornerX := ARight;
    if DiscCenter.Data[0] - ARight > Radius then Exit(false);
  end else
  begin
    InsideX := true;
    if DiscCenter.Data[0] < (Left + ARight) / 2 then
      ClosestCornerX := Left
    else
      ClosestCornerX := ARight;
  end;

  if DiscCenter.Data[1] < Bottom then
  begin
    InsideY := false;
    ClosestCornerY := Bottom;
    if Bottom - DiscCenter.Data[1] > Radius then Exit(false);
  end else
  if DiscCenter.Data[1] > ATop then
  begin
    InsideY := false;
    ClosestCornerY := ATop;
    if DiscCenter.Data[1] - ATop > Radius then Exit(false);
  end else
  begin
    InsideY := true;
    if DiscCenter.Data[1] < (Bottom + ATop) / 2 then
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
    Sqr(DiscCenter.Data[0] - ClosestCornerX) +
    Sqr(DiscCenter.Data[1] - ClosestCornerY) <=
    Sqr(Radius);
end;

function TFloatRectangle.ScaleToWidth(const NewWidth: Single): TFloatRectangle;
begin
  if IsEmpty then Exit(Empty);
  Result.Left := Left;
  Result.Bottom := Bottom;
  Result.Width := NewWidth;
  Result.Height := Height * NewWidth / Width;
end;

function TFloatRectangle.ScaleToHeight(const NewHeight: Single): TFloatRectangle;
begin
  if IsEmpty then Exit(Empty);
  Result.Left := Left;
  Result.Bottom := Bottom;
  Result.Width := Width * NewHeight / Height;
  Result.Height := NewHeight;
end;

function TFloatRectangle.ScaleAroundCenter(const Factor: Single): TFloatRectangle;
begin
  if Width >= 0 then
  begin
    Result.Width  := Width  * Factor;
    Result.Left   := Left   + (Width  - Result.Width ) / 2;
  end else
  begin
    Result.Width  := Width;
    Result.Left   := Left;
  end;

  if Height >= 0 then
  begin
    Result.Height := Height * Factor;
    Result.Bottom := Bottom + (Height - Result.Height) / 2;
  end else
  begin
    Result.Height := Height;
    Result.Bottom := Bottom;
  end;
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

function TFloatRectangle.FitInside(const R: TFloatRectangle;
  const AlignHorizontal: THorizontalPosition = hpMiddle;
  const AlignVertical: TVerticalPosition = vpMiddle): TFloatRectangle;
begin
  if R.Width / R.Height > Width / Height then
  begin
    Result.Height := R.Height;
    Result.Width := Width * Result.Height / Height;
  end else
  begin
    Result.Width := R.Width;
    Result.Height := Height * Result.Width / Width;
  end;
  Result.Left   := Result.AlignCore(AlignHorizontal, R, AlignHorizontal, 0);
  Result.Bottom := Result.AlignCore(AlignVertical, R, AlignVertical, 0);
end;

function TFloatRectangle.Include(const P: TVector2): TFloatRectangle;
begin
  if IsEmpty then
  begin
    Result.Left := P.Data[0];
    Result.Bottom := P.Data[1];
    Result.Width := 0;
    Result.Height := 0;
  end else
  begin
    if P.Data[0] < Left then
    begin
      Result.Left := P.Data[0];
      Result.Width := (Left - P.Data[0]) + Width;
    end else
    begin
      Result.Left := Left;
      if P.Data[0] > Right then
        Result.Width := Width + P.Data[0] - Right
      else
        Result.Width := Width;
    end;

    if P.Data[1] < Bottom then
    begin
      Result.Bottom := P.Data[1];
      Result.Height := (Bottom - P.Data[1]) + Height;
    end else
    begin
      Result.Bottom := Bottom;
      if P.Data[1] > Top then
        Result.Height := Height + P.Data[1] - Top
      else
        Result.Height := Height;
    end;
  end;
end;

function TFloatRectangle.ToX3DVector: TVector4;
begin
  Result := Vector4(
    Left,
    Bottom,
    Right,
    Top);
end;

class function TFloatRectangle.FromX3DVector(const V: TVector4): TFloatRectangle;
begin
  Result.Left   := V.Data[0];
  Result.Bottom := V.Data[1];
  Result.Width  := V.Data[2] - V.Data[0];
  Result.Height := V.Data[3] - V.Data[1];
end;

function TFloatRectangle.Round: TRectangle;
begin
  if IsEmpty then
    Result := TRectangle.Empty
  else
    Result := Rectangle(
      System.Round(Left),
      System.Round(Bottom),
      System.Round(Width),
      System.Round(Height));
end;

function TFloatRectangle.Equals(const R: TFloatRectangle): Boolean;
begin
  if IsEmpty then
    Result := R.IsEmpty
  else
    Result :=
      (not R.IsEmpty) and
      (SameValue(Left  , R.Left)) and
      (SameValue(Bottom, R.Bottom)) and
      (SameValue(Width , R.Width)) and
      (SameValue(Height, R.Height));
end;

function TFloatRectangle.Equals(const R: TFloatRectangle; const Epsilon: Single): Boolean;
begin
  if IsEmpty then
    Result := R.IsEmpty
  else
    Result :=
      (not R.IsEmpty) and
      (SameValue(Left  , R.Left  , Epsilon)) and
      (SameValue(Bottom, R.Bottom, Epsilon)) and
      (SameValue(Width , R.Width , Epsilon)) and
      (SameValue(Height, R.Height, Epsilon));
end;

class operator TFloatRectangle.{$ifdef FPC}+{$else}Add{$endif} (const R1, R2: TFloatRectangle): TFloatRectangle;
var
  NewRight, NewTop: Single;
begin
  if R1.IsEmpty then
    Result := R2 else
  if R2.IsEmpty then
    Result := R1 else
  begin
    Result.Left   := Min(R1.Left  , R2.Left);
    Result.Bottom := Min(R1.Bottom, R2.Bottom);
    NewRight := Max(R1.Right   , R2.Right);
    NewTop   := Max(R1.Top     , R2.Top);
    Result.Width  := NewRight - Result.Left;
    Result.Height := NewTop   - Result.Bottom;
  end;
end;

class operator TFloatRectangle.{$ifdef FPC}*{$else}Multiply{$endif} (const R1, R2: TFloatRectangle): TFloatRectangle;
var
  NewRight, NewTop: Single;
begin
  if R1.IsEmpty or R2.IsEmpty then
    Result := TFloatRectangle.Empty else
  begin
    Result.Left   := Max(R1.Left  , R2.Left);
    Result.Bottom := Max(R1.Bottom, R2.Bottom);
    NewRight := Min(R1.Right   , R2.Right);
    NewTop   := Min(R1.Top     , R2.Top);
    { ">=" unline the int version that checks ">".
      For TFloatRectangle, having zero size makes sense. }
    if (NewRight >= Result.Left) and (NewTop >= Result.Bottom) then
    begin
      { use Max(0, ..) to secure from floating point errors in case equations above
        are true but subtraction yields < 0 due to floating point inaccuracy.
        Not sure is this possible (A >= B and still A - B < 0), probably not,
        but better stay safe when dealing with floating point numbers. }
      Result.Width  := Max(0, NewRight - Result.Left);
      Result.Height := Max(0, NewTop   - Result.Bottom);
    end else
      Result := TFloatRectangle.Empty;
  end;
end;

{ TRectangleList -------------------------------------------------------------- }

function TRectangleList.FindRectangle(const X, Y: Integer): Integer;
begin
  for Result := 0 to Count - 1 do
    if List^[Result].Contains(X, Y) then
      Exit;
  Result := -1;
end;

function TRectangleList.FindRectangle(const Point: TVector2): Integer;
begin
  for Result := 0 to Count - 1 do
    if List^[Result].Contains(Point) then
      Exit;
  Result := -1;
end;

end.
