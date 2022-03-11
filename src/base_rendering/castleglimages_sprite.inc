{
  Copyright 2015-2018 Tomasz Wojtyś, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Part of CastleGLImages unit: drawing 2D sprites on screen (TSprite class). }

{$ifdef read_interface}
{$ifdef read_interface_type}
// type - don't add type delphi can't parse it correctly when some types are forward declared
  { A frame of a custom animation. Part of the TSpriteAnimation. }
  TSpriteAnimationFrame = record
    { Frame number from the sprite. }
    Frame: Cardinal;
    { This frame individual duration time (in seconds). }
    Duration: Single;
  end;

  { Custom animation of a sprite.
    Used by TSprite.AddAnimation and TSprite.PlayAnimation.
    Animation is defined as an array of frames. }
  TSpriteAnimation = class
  strict private
    FDuration: TFloatTime;
  private
    Frames: array of TSpriteAnimationFrame;
  public
    constructor CreateConstantFps(const AFrames: array of Cardinal; const FramesPerSecond: Single);
    constructor Create(const AFrames: array of TSpriteAnimationFrame);
    { Duration, in seconds, of this animation. }
    property Duration: TFloatTime read FDuration;
  end;

  { Sprite is an animation composed from frames arranged
    in rows and columns inside a single image.
    Frames are read from left to right and from top to bottom.

    In the simple case, a sprite represents a single animation,
    and the frames are just in consecutive order.

    With the help of "custom animations" feature, you can define many
    animations in a sprite.
    Each custom animation is an array of frames with
    corresponding frames duration. Frames can be in any order. It is valid
    to use some particural frame many times with different
    duration time.

    @deprecated
    Instead of this, load sprite sheets to TCastleScene from .castle-sprite-sheet format. }
  TSprite = class
  private
    type
      TSpriteAnimations = {$ifdef FPC}specialize{$endif} TObjectList<TSpriteAnimation>;
    var
    FImage: TDrawableImage;
    FX, FY: Single;
    FDrawingWidth, FDrawingHeight: Single;
    FFrames, FFrameWidth, FFrameHeight, FColumns: Cardinal;
    FFramesPerSecond: Single;
    FTime: TFloatTime;
    FFrame, FCustomFrame: Cardinal;
    FPlaying: Boolean;
    FTimeLoop: Boolean;
    FLeftMargin, FTopMargin: Cardinal;
    FHorizontalSpacing, FVerticalSpacing: Cardinal;
    FVerticalSpacingBottom: boolean;
    FTimeBackwards: Boolean;
    FCustomAnimations: TSpriteAnimations;
    FCurrentAnimation: Integer;
    FCustomAnimTimestamp: TFloatTime;
    FHorizontalFlip, FVerticalFlip: Boolean;
    FSmoothScalingSafeBorder, FSmoothScaling: Boolean;
    FOwnsImage: Boolean;
    function GetCenterX: Single;
    function GetCenterY: Single;
    function GetFrameRect: TFloatRectangle;
    function GetDrawRect: TFloatRectangle;
    function GetRotationCenter: TVector2;
    function GetPosition: TVector2;
    function GetRotation: Single;
    function GetColor: TCastleColor;
    procedure SetTime(const AValue: TFloatTime);
    procedure SetRotationCenter(const AValue: TVector2);
    procedure SetCenterX(const AValue: Single);
    procedure SetCenterY(const AValue: Single);
    procedure SetDrawRect(const AValue: TFloatRectangle);
    procedure SetFramesPerSecond(const AValue: Single);
    procedure SetFrame(const AValue: Cardinal);
    procedure SetPosition(const APosition: TVector2);
    procedure SetRotation(const AValue: Single);
    procedure SetColor(const AValue: TCastleColor);
    procedure SetSmoothScaling(const Value: Boolean);
  public
    { Constructor.
      @param(URL URL of source image.)
      @param(AFrames Total numer of animation frames.)
      @param(AColumns Mumber of columns.)
      @param(ARows Number of rows.)
      @param(ASmoothscaling Enables interpolation, see @link(TDrawableImage.SmoothScaling) for details, default @True.)
      @param(ATimeLoop Should animation be repeated?, default @true.)
      @param(APlay Is animation playing? if not then current frame will be displayed.) }
    constructor Create(const URL: string;
      const AFrames, AColumns, ARows: Cardinal;
      const ASmoothScaling: Boolean = True;
      const ATimeLoop: Boolean = True; const APlay: Boolean = False);

    { Constructor that takes explicit frame size, and loaded TDrawableImage instance.

      @param(AImage Source image (will be owned by this sprite).)
      @param(AFrames Total numer of animation frames.)
      @param(AColumns Number of columns.)
      @param(AFrameWidth Width of each frame.)
      @param(AFrameHeight Height of each frame.)
      @param(ATimeLoop Should animation be repeated?, default @true.)
      @param(APlay Is animation playing? if not then current frame will be displayed.) }
    constructor CreateFrameSize(const AImage: TDrawableImage;
      const AFrames, AColumns, AFrameWidth, AFrameHeight: Cardinal;
      const ATimeLoop: Boolean = True; const APlay: Boolean = False); overload;

    { Constructor that takes explicit frame size.

      @param(URL URL of source image.)
      @param(AFrames Total numer of animation frames.)
      @param(AColumns Number of columns.)
      @param(AFrameWidth Width of each frame.)
      @param(AFrameHeight Height of each frame.)
      @param(ASmoothscaling Enables interpolation, see @link(TDrawableImage.SmoothScaling) for details, default @True.)
      @param(ATimeLoop Should animation be repeated?, default True.)
      @param(APlay Is animation playing? if not then current frame will be displayed.) }
    constructor CreateFrameSize(const URL: string;
      const AFrames, AColumns, AFrameWidth, AFrameHeight: Cardinal;
      const ASmoothScaling: Boolean = True;
      const ATimeLoop: Boolean = True; const APlay: Boolean = False); overload;

    { By default this class "owns" the underlying image, even when passed explicitly to
      CreateFrameSize as TDrawableImage instance.
      That is, the underlying TDrawableImage instance will be automatically freed when
      the sprite will be freed.
      You turn it off with this property. }
    property OwnsImage: Boolean read FOwnsImage write FOwnsImage default true;

    destructor Destroy; override;

    { Update current frame.
      @param(SecondsPassed Time from previous update.) }
    procedure Update(const SecondsPassed: TFloatTime);

    { Play playing animation.,
      When playing, the @link(Time) will move forward when you call @link(Update). }
    procedure Play;

    { Stop playing animation.
      When stopped, the @link(Time) will not move forward, regardless
      if you call @link(Update). }
    procedure Stop;

    procedure Pause; deprecated 'use Stop';

    { Draw the sprite.

      The overloaded version without AX, AY or ScreenRectangle parameters uses
      the last position set by @link(Move) method. This is the position
      of the bottom-left frame corner on screen.

      The overloaded version without DrawWidth, DrawHeight or ScreenRectangle
      parameters uses the last size set by @link(DrawingWidth), @link(DrawingHeight)
      properties. This is the size of the frame on screen.

      All versions use the rotation set by the last @link(Move) method.
      This is the rotation of the frame on screen.

      The overloaded versions deliberately look and work
      similar to @link(TDrawableImage.Draw) versions.
      @groupBegin }
    procedure Draw; overload;
    procedure Draw(const AX, AY: Single); overload;
    procedure Draw(const AX, AY, DrawWidth, DrawHeight: Single); overload;
    procedure Draw(const ScreenRectangle: TRectangle); overload;
    procedure Draw(const ScreenRectangle: TFloatRectangle); overload;
    { @groupEnd }

    { Draw the sprite, optionally flipped horizontally and/or vertically.
      These methods ignore the @link(HorizontalFlip), @link(VerticalFlip)
      -- which axis is flipped (if any) depends only on the parameters to these methods.
      @groupBegin }
    procedure DrawFlipped(const ScreenRect: TRectangle;
      const FlipHorizontal, FlipVertical: Boolean); overload;
    procedure DrawFlipped(const ScreenRect: TFloatRectangle;
      const FlipHorizontal, FlipVertical: Boolean); overload;
    { @groupEnd }

    { Move sprite to position and rotation. }
    procedure Move(AX, AY: Single; ARot: Single=0);
  public
    property X: Single read FX write FX;
    property Y: Single read FY write FY;
    property Position: TVector2 read GetPosition write SetPosition;

    {$ifdef FPC}
    { Center X of rotation. Values between 0 and 1. }
    property CenterX: Single read GetCenterX write SetCenterX default 0.5;
      deprecated 'use RotationCenter';

    { Center Y of rotation. Values between 0 and 1. }
    property CenterY: Single read GetCenterY write SetCenterY default 0.5;
      deprecated 'use RotationCenter';
    {$endif}

    { Destination frame width to draw. }
    property DrawingWidth: Single read FDrawingWidth write FDrawingWidth;

    { Destination frame height to draw. }
    property DrawingHeight: Single read FDrawingHeight write FDrawingHeight;

    { Drawing rectangle. This is just a shortcut to get / set
      properties @link(X), @link(Y), DrawingWidth, DrawingHeight. }
    property DrawRect: TFloatRectangle read GetDrawRect write SetDrawRect;

    { Center of rotation.
      Expressed as a fraction within the drawn ScreenRectangle,
      (0,0) means bottom-left corner, (1,1) means top-right corner.
      Default (0.5,0.5). }
    property RotationCenter: TVector2 read GetRotationCenter write SetRotationCenter;

    {$ifdef FPC}
    property Center: TVector2 read GetRotationCenter write SetRotationCenter;
      deprecated 'use RotationCenter';
    {$endif}

    { Rotation in degrees. }
    property Rotation: Single read GetRotation write SetRotation {$ifdef FPC}default 0{$endif};

    { Color multiplier of the sprite, see @link(TDrawableImage.Color) }
    property Color: TCastleColor read GetColor write SetColor;

    { Total number of frames. Readonly. Defined at creation. }
    property Frames: Cardinal read FFrames;

    { How many frames per second to play.
      This is used only for the default sprite animation (spanning all frames).
      For the custom animations (added by AddAnimation), each frame
      has an explicit (possibly different) duration. }
    property FramesPerSecond: Single
      read FFramesPerSecond write SetFramesPerSecond
      {$ifdef FPC}default DefaultFramesPerSecond{$endif};

    { Current frame. }
    property Frame: Cardinal read FFrame write SetFrame;

    { Does the animation proceeds forward when you call @link(Update) method. }
    property Playing: Boolean read FPlaying write FPlaying default False;

    { Does the animation loop, that is display the same animation over and over
      when time exceeded the animation duration. }
    property TimeLoop: Boolean read FTimeLoop write FTimeLoop;

    { Current time within the animation, it determines the current frame.
      Yoy can set this explicity. Alternatively, you can call the @link(Update)
      method continuously while the animation is @link(Playing), and then
      the time will increase automatically. }
    property Time: TFloatTime read FTime write SetTime;

    { Width of a single frame. Initial value is set by the constructor. }
    property FrameWidth: Cardinal read FFrameWidth write FFrameWidth;

    { Height of a single frame. Initial value is set by the constructor. }
    property FrameHeight: Cardinal read FFrameHeight write FFrameHeight;

    { X margin for frame position on the underlying image.
      Useful if the first frame doesn't start at X=0. }
    property LeftMargin: Cardinal read FLeftMargin write FLeftMargin default 0;
    {$ifdef FPC}property LeftMarginOffset: Cardinal read FLeftMargin write FLeftMargin default 0; deprecated 'use LeftMargin';{$endif}

    { Y margin for frame position on the underlying image.
      Useful if the first frame doesn't start at Y=0. }
    property TopMargin: Cardinal read FTopMargin write FTopMargin default 0;
    {$ifdef FPC}property TopMarginOffset: Cardinal read FTopMargin write FTopMargin default 0; deprecated 'use TopMargin';{$endif}

    { Horizontal spacing between frames.
      Use this if you have an empty space at the @bold(right) of every frame
      in your spritesheet. This is useful to avoid frames "bleeding"
      into each other (due to smooth scaling). }
    property HorizontalSpacing: Cardinal
      read FHorizontalSpacing write FHorizontalSpacing default 0;

    { Vertical spacing between frames.
      Use this if you have an empty space at the @bold(top)
      (or @bold(bottom), if @link(VerticalSpacingBottom))
      of every frame in your spritesheet.
      This is useful to avoid frames "bleeding"
      into each other (due to smooth scaling). }
    property VerticalSpacing: Cardinal
      read FVerticalSpacing write FVerticalSpacing default 0;

    { When @true, the @link(VerticalSpacing) is assumed to be at the bottom
      of every frame, not top. }
    property VerticalSpacingBottom: boolean
      read FVerticalSpacingBottom write FVerticalSpacingBottom default false;

    { Should we play the animation backwards after playing it forward.
      See TVideo.TimeBackwards. }
    property TimeBackwards: Boolean read FTimeBackwards write FTimeBackwards default False;

    { Currently used animation.
      Equal to -1 when we're using the default animation
      spanning all sprite frames.
      Otherwise, indicates a custom animation index
      (added by AddAnimation and used by SwitchToAnimation).

      Call SwitchToAnimation to change this. }
    property CurrentAnimation: Integer read FCurrentAnimation default -1;

    { Add a custom animation (using an explicit sequence of frames),
      that can be later used by SwitchToAnimation.
      @param(AAnimation The animation information. The animation instance
        (TSpriteAnimation) becomes owned by this object, so don't free
        it yourself.)
      @param(AnimationFrames The animation information can also be given
        as a simple list of frame indexes. In this case,
        all frames are assumed to take the same time: @code(1 / FramesPerSecond).)
      @returns(The animation index, that can be used with SwitchToAnimation.)
      @groupBegin }
    function AddAnimation(const AAnimation: TSpriteAnimation): Integer; overload;
    function AddAnimation(const AnimationFrames: array of Cardinal): Integer; overload;
    function AddSpriteAnimation(const AnimationFrames: array of TSpriteAnimationFrame): Integer;
      deprecated 'use AddAnimation';
    { @groupEnd }

    { Switch to the animation with index AAnimIndex.
      Use animation index obtained from AddAnimation to use a custom animation,
      or use -1 to use the default animation (spanning all sprite frames).

      @returns(@true on success, @false if AAnimIndex is out of bounds.) }
    function SwitchToAnimation(const AAnimIndex: Integer; const ACustomFrame: Cardinal = 0): Boolean;

    { Duration, in seconds, of the currently used animation.
      See @link(CurrentAnimation) to know what the current animation is. }
    function Duration: TFloatTime;

    { Is sprite horizontal flipped? }
    property HorizontalFlip: Boolean read FHorizontalFlip write FHorizontalFlip default False;

    { Is sprite vertical flipped? }
    property VerticalFlip: Boolean read FVerticalFlip write FVerticalFlip default False;

     { Use smooth "bilinear" filtering when this is scaled. }
    property SmoothScaling: boolean read FSmoothScaling write SetSmoothScaling;

    { Prevent the color outside of rendered frame from creeping in,
      by scaling image a bit to the inside.
      Because of bilinear filtering (used when @link(SmoothScaling)),
      the colors of neighboring pixels may be visible when you render
      the sprite frame, and the sprite is significantly scaled.
      The "neighboring pixels" may mean
      "pixels from the adjacent sprite frame",
      or sprite border (in case sprite frame is at the edge of the image),
      or spacing between sprite frames (if any).

      One way to workaround this is to add a 1-pixel or 2-pixel
      spacing between each sprite frames, and fill this spacing
      with the same color (including alpha) as the neighbor pixels.

      Another workaround is to use this property.
      This renders a slightly smaller sprite frame, such that bilinear
      filtering doesn't have a chance to introduce colors from outside
      of the desired sprite frame.
      This is done only when @link(SmoothScaling), otherwise this property
      is ignored.

      The downside is that everything is slightly scaled,
      and doesn't look perfect if you look closely at each pixel.
      This property is ignored  }
    property SmoothScalingSafeBorder: Boolean
      read FSmoothScalingSafeBorder write FSmoothScalingSafeBorder
      default false;

  { Unfortunately deprecating it makes unavoidable warnings from TSpriteList
    specialization, and we cannot hide them putting $warnings off/on around it. }
  end {deprecated 'load sprite sheets to TCastleScene from .castle-sprite-sheet format'};

  { List of sprites. }
  TSpriteList = {$ifdef FPC}specialize{$endif} TObjectList<TSprite>;

{$endif read_interface_type}

{$endif read_interface}

{$ifdef read_implementation}

{ TSpriteAnimation ----------------------------------------------------------- }

constructor TSpriteAnimation.CreateConstantFps(const AFrames: array of Cardinal; const FramesPerSecond: Single);
var
  I: Integer;
  FrameDuration: Single;
begin
  inherited Create;
  FrameDuration := 1 / FramesPerSecond;
  { calculate Frames }
  SetLength(Frames, High(AFrames) + 1);
  for I := 0 to High(AFrames) do
  begin
    Frames[I].Frame := AFrames[I];
    Frames[I].Duration := FrameDuration;
  end;
  { calculate FDuration }
  FDuration := FrameDuration * Length(AFrames);
end;

constructor TSpriteAnimation.Create(const AFrames: array of TSpriteAnimationFrame);
var
  I: Integer;
begin
  inherited Create;
  { calculate Frames and FDuration }
  FDuration := 0;
  SetLength(Frames, High(AFrames) + 1);
  for I := 0 to High(AFrames) do
  begin
    Frames[I] := AFrames[I];
    FDuration := FDuration + Frames[I].Duration;
  end;
end;

{ TSprite -------------------------------------------------------------------- }

function TSprite.GetPosition: TVector2;
begin
  Result.X := FX;
  Result.Y := FY;
end;

function TSprite.GetRotation: Single;
begin
  Result := FImage.Rotation;
end;

function TSprite.GetColor: TCastleColor;
begin
  Result := FImage.Color;
end;

procedure TSprite.SetTime(const AValue: TFloatTime);
var
  Anim: TSpriteAnimation;
begin
  if FTime = AValue then Exit;
  FTime := AValue;
  if FCurrentAnimation <> -1 then
  begin
    Assert(FCurrentAnimation < FCustomAnimations.Count);
    Anim := FCustomAnimations.Items[FCurrentAnimation];
    FFrame := Anim.Frames[FCustomFrame].Frame;
    while (FTime - FCustomAnimTimestamp) >= Anim.Frames[FCustomFrame].Duration do
    begin
      // todo: custom animation loops
      if FCustomFrame = High(Anim.Frames) then
      begin
        if FTimeLoop then
          FCustomFrame := 0
        else
          Stop;
      end else
        Inc(FCustomFrame);
      FCustomAnimTimestamp := FCustomAnimTimestamp + Anim.Frames[FCustomFrame].Duration;
      FFrame := Anim.Frames[FCustomFrame].Frame;
    end;
  end else
    FFrame := TVideo.FrameIndexFromTime(FTime, FFrames, FFramesPerSecond, FTimeLoop, FTimeBackwards);
end;

function TSprite.GetFrameRect: TFloatRectangle;
var
  FullFrameWidth, FullFrameHeight: Cardinal;
begin
  FullFrameWidth := FFrameWidth + FHorizontalSpacing;
  FullFrameHeight := FFrameHeight + FVerticalSpacing;
  Result.Left :=
    FFrame mod FColumns * FullFrameWidth + FLeftMargin;
  Result.Bottom := FImage.Height -
    (FFrame div FColumns + 1) * FullFrameHeight - FTopMargin;
  if VerticalSpacingBottom then
    Result.Bottom := Result.Bottom + FVerticalSpacing;
  Result.Width := FFrameWidth;
  Result.Height := FFrameHeight;
end;

function TSprite.GetDrawRect: TFloatRectangle;
begin
  Result.Left := FX;
  Result.Bottom := FY;
  Result.Width := FDrawingWidth;
  Result.Height := FDrawingHeight;
end;

procedure TSprite.SetDrawRect(const AValue: TFloatRectangle);
begin
  FX := AValue.Left;
  FY := AValue.Bottom;
  FDrawingWidth := AValue.Width;
  FDrawingHeight := AValue.Height;
end;

procedure TSprite.SetFramesPerSecond(const AValue: Single);
begin
  if FFramesPerSecond = AValue then Exit;
  FFramesPerSecond := AValue;
end;

procedure TSprite.SetFrame(const AValue: Cardinal);
begin
  if (FFrame = AValue) or (AValue >= FFrames) then Exit;
  FFrame := AValue;
end;

procedure TSprite.SetPosition(const APosition: TVector2);
begin
  FX := APosition[0];
  FY := APosition[1];
end;

procedure TSprite.SetRotation(const AValue: Single);
begin
  FImage.Rotation := AValue;
end;

procedure TSprite.SetColor(const AValue: TCastleColor);
begin
  FImage.Color := AValue;
end;

constructor TSprite.CreateFrameSize(const AImage: TDrawableImage;
  const AFrames, AColumns, AFrameWidth, AFrameHeight: Cardinal;
  const ATimeLoop: Boolean = True; const APlay: Boolean = False);
begin
  inherited Create;
  FImage := AImage;
  FFrames := AFrames;
  FColumns := AColumns;
  FFrameWidth := AFrameWidth;
  FFrameHeight := AFrameHeight;
  FDrawingWidth := FFrameWidth;
  FDrawingHeight := FFrameHeight;
  FPlaying := APlay;
  FFrame := 0;
  FTime := 0;
  SetFramesPerSecond(DefaultFramesPerSecond);
  FTimeLoop := ATimeLoop;
  FLeftMargin := 0;
  FTopMargin := 0;
  FTimeBackwards := False;
  FCurrentAnimation := -1;
  FCustomAnimations := TSpriteAnimations.Create(true);
  FCustomFrame := 0;
  FHorizontalFlip := False;
  FVerticalFlip := False;
  FOwnsImage := true;
end;

constructor TSprite.CreateFrameSize(const URL: string;
  const AFrames, AColumns, AFrameWidth, AFrameHeight: Cardinal;
  const ASmoothScaling: Boolean = True;
  const ATimeLoop: Boolean = True; const APlay: Boolean = False);
var
  Img: TDrawableImage;
begin
  FSmoothScaling := ASmoothScaling;
  Img := TDrawableImage.Create(URL, SmoothScaling);
  CreateFrameSize(Img, AFrames, AColumns, AFrameWidth, AFrameHeight,
    ATimeLoop, APlay);
end;

constructor TSprite.Create(const URL: string;
  const AFrames, AColumns, ARows: Cardinal;
  const ASmoothScaling: Boolean = True;
  const ATimeLoop: Boolean = True; const APlay: Boolean = False);
var
  Img: TDrawableImage;
begin
  FSmoothScaling := ASmoothScaling;
  Img := TDrawableImage.Create(URL, SmoothScaling);
  CreateFrameSize(Img, AFrames, AColumns,
    { Note that ARows is not remembered in this instance.
      It's only used to calculate default AFrameHeight below, which can
      be changed later. }
    { AFrameWidth } Img.Width div AColumns,
    { AFrameHeight } Img.Height div ARows,
    ATimeLoop, APlay);
end;

destructor TSprite.Destroy;
begin
  if OwnsImage then
    FreeAndNil(FImage);
  FreeAndNil(FCustomAnimations);
  inherited;
end;

procedure TSprite.Update(const SecondsPassed: TFloatTime);
begin
  if not FPlaying then Exit;
  Time := Time + SecondsPassed;
end;

procedure TSprite.Play;
begin
  FPlaying := True;
end;

procedure TSprite.Stop;
begin
  FPlaying := False;
end;

procedure TSprite.Pause;
begin
  Stop;
end;

procedure TSprite.Draw;
begin
  Draw(GetDrawRect);
end;

procedure TSprite.Draw(const AX, AY: Single);
begin
  Draw(FloatRectangle(AX, AY, DrawingWidth, DrawingHeight));
end;

procedure TSprite.Draw(const AX, AY, DrawWidth, DrawHeight: Single);
begin
  Draw(FloatRectangle(AX, AY, DrawWidth, DrawHeight));
end;

procedure TSprite.Draw(const ScreenRectangle: TRectangle);
begin
  Draw(FloatRectangle(ScreenRectangle));
end;

procedure TSprite.Draw(const ScreenRectangle: TFloatRectangle);
begin
  DrawFlipped(ScreenRectangle,
    HorizontalFlip,
    VerticalFlip);
end;

procedure TSprite.DrawFlipped(const ScreenRect: TRectangle;
  const FlipHorizontal, FlipVertical: Boolean);
begin
  DrawFlipped(FloatRectangle(ScreenRect), FlipHorizontal, FlipVertical);
end;

procedure TSprite.DrawFlipped(const ScreenRect: TFloatRectangle;
  const FlipHorizontal, FlipVertical: Boolean);
var
  F: TFloatRectangle;
begin
  F := GetFrameRect;
  if SmoothScaling and SmoothScalingSafeBorder then
    F := F.Grow(-0.51);
  FImage.DrawFlipped(ScreenRect, F, FlipHorizontal, FlipVertical);
end;

procedure TSprite.Move(AX, AY: Single; ARot: Single);
begin
  FX := AX;
  FY := AY;
  Rotation := ARot;
end;

function TSprite.AddAnimation(const AAnimation: TSpriteAnimation): Integer;
begin
  Result := FCustomAnimations.Add(AAnimation);
end;

function TSprite.AddAnimation(
  const AnimationFrames: array of Cardinal): Integer;
begin
  Result := AddAnimation(
    TSpriteAnimation.CreateConstantFps(AnimationFrames, FramesPerSecond));
end;

function TSprite.AddSpriteAnimation(
  const AnimationFrames: array of TSpriteAnimationFrame): Integer;
begin
  Result := AddAnimation(TSpriteAnimation.Create(AnimationFrames));
end;

function TSprite.SwitchToAnimation(const AAnimIndex: Integer;
  const ACustomFrame: Cardinal): Boolean;
begin
  if (AAnimIndex >= FCustomAnimations.Count) or (AAnimIndex < -1) then
  begin
    Result := False;
    Exit;
  end;
  FCurrentAnimation := AAnimIndex;
  if FCurrentAnimation <> -1 then
  begin
    FCustomFrame := Clamped(ACustomFrame, 0, High(FCustomAnimations.Items[AAnimIndex].Frames));
  end else
    FCustomFrame := 0; // ACustomFrame, FCustomFrame unused in this case
  Result := True;
end;

function TSprite.Duration: TFloatTime;
begin
  if FCurrentAnimation <> -1 then
    Result := FCustomAnimations[FCurrentAnimation].Duration
  else
    Result := Frames / FramesPerSecond;
end;

procedure TSprite.SetSmoothScaling(const Value: Boolean);
begin
  if FSmoothScaling <> Value then
  begin
    FSmoothScaling := Value;
    FImage.SmoothScaling := Value;
  end;
end;

function TSprite.GetRotationCenter: TVector2;
begin
  Result := FImage.RotationCenter;
end;

procedure TSprite.SetRotationCenter(const AValue: TVector2);
begin
  FImage.RotationCenter := AValue;
end;

function TSprite.GetCenterX: Single;
begin
  Result := RotationCenter.X;
end;

function TSprite.GetCenterY: Single;
begin
  Result := RotationCenter.Y;
end;

procedure TSprite.SetCenterX(const AValue: Single);
var
  V: TVector2;
begin
  V := RotationCenter;
  V.X := AValue;
  RotationCenter := V;
end;

procedure TSprite.SetCenterY(const AValue: Single);
var
  V: TVector2;
begin
  V := RotationCenter;
  V.Y := AValue;
  RotationCenter := V;
end;

{$endif read_implementation}
