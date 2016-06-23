{
  Copyright 2015 Tomasz Wojtyś

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

type

  { Single frame individual duration time (in seconds). }
  TSpriteAnimationFrame = record
    Frame: Cardinal;
    Duration: Single;
  end;

  { Custom animation of a sprite. Animation is defined as array of used frames. }
  TSpriteAnimation = array of TSpriteAnimationFrame;

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
    duration time. }
  TSprite = class
  private
    type
      TSpriteAnimations = specialize TFPGList<TSpriteAnimation>;
    var
    FImage: TGLImage;
    FX, FY: Integer;
    FDrawingWidth, FDrawingHeight: Cardinal;
    FFrames, FFrameWidth, FFrameHeight, FColumns: Cardinal;
    FFramesPerSecond: Single;
    FAnimTime: TFloatTime;
    FFrame, FCustomFrame: Cardinal;
    FPlaying: Boolean;
    FTimeLoop: Boolean;
    FLeftMarginOffset, FTopMarginOffset: Cardinal;
    FTimeBackwards: Boolean;
    FAnimations: TSpriteAnimations;
    FUseCustomAnimations: Boolean;
    FCurrentAnimation: Integer;
    FCustomAnimTimestamp: TFloatTime;
    function GetCenterX: Single;
    function GetCenterY: Single;
    function GetFrameRect: TRectangle;
    function GetDrawRect: TRectangle;
    function GetCenter: TVector2Single;
    function GetPosition: TVector2Integer;
    function GetRotation: Single;
    procedure SetAnimTime(AValue: TFloatTime);
    procedure SetCenter(AValue: TVector2Single);
    procedure SetCenterX(AValue: Single);
    procedure SetCenterY(AValue: Single);
    procedure SetDrawRect(AValue: TRectangle);
    procedure SetFramesPerSecond(AValue: Single);
    procedure SetFrame(AValue: Cardinal);
    procedure SetPosition(APosition: TVector2Integer);
    procedure SetRotation(AValue: Single);
  public
    { Constructor.
      @param(URL URL of source image.)
      @param(AFrames Total numer of animation frames.)
      @param(AColumns Mumber of columns.)
      @param(ARows Number of rows.)
      @param(ASmoothscaling Enables interpolation, see @link(TGLImageCore.SmoothScaling) for details, default @True.)
      @param(ATimeLoop Should animation be repeated?, default @true.)
      @param(APlay Is animation playing? if not then current frame will be displayed.) }
    constructor Create(const URL: string;
      const AFrames, AColumns, ARows: Cardinal;
      const ASmoothScaling: Boolean = True;
      const ATimeLoop: Boolean = True; const APlay: Boolean = False);

    { Constructor that takes explicit frame size, and loaded TGLImage instance.

      @param(AImage Source image (will be owned by this sprite).)
      @param(AFrames Total numer of animation frames.)
      @param(AColumns Number of columns.)
      @param(AFrameWidth Width of each frame.)
      @param(AFrameHeight Height of each frame.)
      @param(ATimeLoop Should animation be repeated?, default @true.)
      @param(APlay Is animation playing? if not then current frame will be displayed.) }
    constructor CreateFrameSize(const AImage: TGLImage;
      const AFrames, AColumns, AFrameWidth, AFrameHeight: Cardinal;
      const ATimeLoop: Boolean = True; const APlay: Boolean = False);

    { Constructor that takes explicit frame size.

      @param(URL URL of source image.)
      @param(AFrames Total numer of animation frames.)
      @param(AColumns Number of columns.)
      @param(AFrameWidth Width of each frame.)
      @param(AFrameHeight Height of each frame.)
      @param(ASmoothscaling Enables interpolation, see @link(TGLImageCore.SmoothScaling) for details, default @True.)
      @param(ATimeLoop Should animation be repeated?, default True.)
      @param(APlay Is animation playing? if not then current frame will be displayed.) }
    constructor CreateFrameSize(const URL: string;
      const AFrames, AColumns, AFrameWidth, AFrameHeight: Cardinal;
      const ASmoothScaling: Boolean = True;
      const ATimeLoop: Boolean = True; const APlay: Boolean = False);

    destructor Destroy; override;

    { Update current frame.
      @param(SecondsPassed Time from previous update.) }
    procedure Update(SecondsPassed: Single);

    { Play animation. }
    procedure Play;

    { Pause animation. }
    procedure Pause;

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
      similar to @link(TGLImageCore.Draw) versions. }
    procedure Draw;
    procedure Draw(const AX, AY: Single);
    procedure Draw(const AX, AY, DrawWidth, DrawHeight: Single);
    procedure Draw(const ScreenRectangle: TRectangle);

    { Move sprite to position and rotation. }
    procedure Move(AX, AY: Integer; ARot: Single=0);
  public
    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;
    property Position: TVector2Integer read GetPosition write SetPosition;

    { Center X of rotation. Values between 0 and 1. }
    property CenterX: Single read GetCenterX write SetCenterX default 0.5;

    { Center Y of rotation. Values between 0 and 1. }
    property CenterY: Single read GetCenterY write SetCenterY default 0.5;

    { Destination frame width to draw. }
    property DrawingWidth: Cardinal read FDrawingWidth write FDrawingWidth;

    { Destination frame height to draw. }
    property DrawingHeight: Cardinal read FDrawingHeight write FDrawingHeight;

    { Drawing rectangle. This is just a shortcut to get / set
      properties @link(X), @link(Y), DrawingWidth, DrawingHeight. }
    property DrawRect: TRectangle read GetDrawRect write SetDrawRect;

    { Center of rotation. Values between 0 and 1. }
    property Center: TVector2Single read GetCenter write SetCenter;

    { Rotation in degrees. }
    property Rotation: Single read GetRotation write SetRotation default 0;

    { Total number of frames. Readonly. Defined at creation. }
    property Frames: Cardinal read FFrames;

    { How many frames per second to play. }
    property FramesPerSecond: Single read FFramesPerSecond write SetFramesPerSecond default DefaultFramesPerSecond;

    { Current frame. }
    property Frame: Cardinal read FFrame write SetFrame;

    { Does the animation proceeds forward when you call @link(Update) method. }
    property Playing: Boolean read FPlaying write FPlaying default False;

    { Does the animation loop, that is display the same animation over and over
      when time exceeded the animation duration. }
    property TimeLoop: Boolean read FTimeLoop write FTimeLoop;

    { Current time within the animation, it determines the current frame.
      Yoy can set this explicity. Alternatively, you can call the @link(Update)
      method continously while the animation is @link(Playing), and then
      the time will increase automatically. }
    property Time: TFloatTime read FAnimTime write SetAnimTime;

    { Width of a single frame. Initial value is set by the constructor. }
    property FrameWidth: Cardinal read FFrameWidth write FFrameWidth;

    { Height of a single frame. Initial value is set by the constructor. }
    property FrameHeight: Cardinal read FFrameHeight write FFrameHeight;

    { X offset for frame position on the underlying image.
      Useful if the first frame doesn't start at X=0. }
    property LeftMarginOffset: Cardinal read FLeftMarginOffset write FLeftMarginOffset default 0;

    { Y offset for frame position on the underlying image.
      Useful if the first frame doesn't start at Y=0. }
    property TopMarginOffset: Cardinal read FTopMarginOffset write FTopMarginOffset default 0;

    { Should we play the animation backwards after playing it forward.
      See TVideo.TimeBackwards. }
    property TimeBackwards: Boolean read FTimeBackwards write FTimeBackwards default False;

    { Is custom animations feature in use? }
    property UseCustomAnimations: Boolean read FUseCustomAnimations write FUseCustomAnimations default False;

    { Adding an animation to internal list.
      @param(AAnimation The animation to add.)
      @returns(the animation index). }
    function AddSpriteAnimation(const AAnimation: TSpriteAnimation): Integer;

    { Switch to animation with index AAnimIndex.
      @returns(@true on success, @false if AAnimIndex is out of bound) }
    function SwitchToAnimation(const AAnimIndex: Integer; const ACustomFrame: Cardinal=0): Boolean;

    { Duration, in seconds. Assuming no custom animations are
      used. See @link(AddSpriteAnimation), @link(UseCustomAnimations)
      about custom animations -- in that case, each custom animation
      has it's own duration.

      This is just the number of @link(Frames) divided by @link(FramesPerSecond). }
    function Duration: Single;
  end;

{$endif read_interface}

{$ifdef read_implementation}

function TSprite.GetPosition: TVector2Integer;
begin
  Result[0] := FX;
  Result[1] := FY;
end;

function TSprite.GetRotation: Single;
begin
  Result := FImage.Rotation;
end;

procedure TSprite.SetAnimTime(AValue: TFloatTime);
var
  Anim: TSpriteAnimation;
begin
  if FAnimTime = AValue then Exit;
  FAnimTime := AValue;
  if FUseCustomAnimations then
  begin
    if FAnimations.Count = 0 then Exit;
    Anim := FAnimations.Items[FCurrentAnimation];
    while (FAnimTime - FCustomAnimTimestamp) >= Anim[FCustomFrame].Duration do
    begin
      // todo: custom animation loops
      if FCustomFrame = High(Anim) then
      begin
        if FTimeLoop then
        begin
          FCustomFrame := 0;
        end else
        begin
          Pause;
        end;
      end else
        Inc(FCustomFrame);
      FCustomAnimTimestamp += Anim[FCustomFrame].Duration;
      FFrame := Anim[FCustomFrame].Frame;
    end;
  end else
    FFrame := TVideo.FrameIndexFromTime(FAnimTime, FFrames, FFramesPerSecond, FTimeLoop, FTimeBackwards);
end;

function TSprite.GetFrameRect: TRectangle;
begin
  Result.Left := FFrame mod FColumns * FFrameWidth + FLeftMarginOffset;
  Result.Bottom := (FImage.Height - FFrameHeight) - FFrame div FColumns * FFrameHeight - FTopMarginOffset;
  Result.Width := FFrameWidth;
  Result.Height := FFrameHeight;
end;

function TSprite.GetCenterX: Single;
begin
  Result := FImage.CenterX;
end;

function TSprite.GetCenterY: Single;
begin
  Result := FImage.CenterY;
end;

function TSprite.GetDrawRect: TRectangle;
begin
  Result.Left := FX;
  Result.Bottom := FY;
  Result.Width := FDrawingWidth;
  Result.Height := FDrawingHeight;
end;

function TSprite.GetCenter: TVector2Single;
begin
  Result[0] := CenterX;
  Result[1] := CenterY;
end;

procedure TSprite.SetCenter(AValue: TVector2Single);
begin
  CenterX := AValue[0];
  CenterY := AValue[1];
end;

procedure TSprite.SetCenterX(AValue: Single);
begin
  FImage.CenterX := AValue;
end;

procedure TSprite.SetCenterY(AValue: Single);
begin
  FImage.CenterY := AValue;
end;

procedure TSprite.SetDrawRect(AValue: TRectangle);
begin
  FX := AValue.Left;
  FY := AValue.Bottom;
  FDrawingWidth := AValue.Width;
  FDrawingHeight := AValue.Height;
end;

procedure TSprite.SetFramesPerSecond(AValue: Single);
begin
  if FFramesPerSecond = AValue then Exit;
  FFramesPerSecond := AValue;
end;

procedure TSprite.SetFrame(AValue: Cardinal);
begin
  if (FFrame = AValue) or (AValue >= FFrames) then Exit;
  FFrame := AValue;
end;

procedure TSprite.SetPosition(APosition: TVector2Integer);
begin
  FX := APosition[0];
  FY := APosition[1];
end;

procedure TSprite.SetRotation(AValue: Single);
begin
  FImage.Rotation := AValue;
end;

constructor TSprite.CreateFrameSize(const AImage: TGLImage;
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
  FAnimTime := 0;
  SetFramesPerSecond(DefaultFramesPerSecond);
  FTimeLoop := ATimeLoop;
  FLeftMarginOffset := 0;
  FTopMarginOffset := 0;
  FTimeBackwards := False;
  FUseCustomAnimations := False;
  FAnimations := TSpriteAnimations.Create;
  FCustomFrame := 0;
end;

constructor TSprite.CreateFrameSize(const URL: string;
  const AFrames, AColumns, AFrameWidth, AFrameHeight: Cardinal;
  const ASmoothScaling: Boolean = True;
  const ATimeLoop: Boolean = True; const APlay: Boolean = False);
var
  Img: TGLImage;
begin
  Img := TGLImage.Create(URL, ASmoothScaling);
  CreateFrameSize(Img, AFrames, AColumns, AFrameWidth, AFrameHeight,
    ATimeLoop, APlay);
end;

constructor TSprite.Create(const URL: string;
  const AFrames, AColumns, ARows: Cardinal;
  const ASmoothScaling: Boolean = True;
  const ATimeLoop: Boolean = True; const APlay: Boolean = False);
var
  Img: TGLImage;
begin
  Img := TGLImage.Create(URL, ASmoothScaling);
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
  FreeAndNil(FImage);
  FreeAndNil(FAnimations);
  inherited;
end;

procedure TSprite.Update(SecondsPassed: Single);
begin
  if not FPlaying then Exit;

  Time := Time + SecondsPassed;

  // looping implemented in SetAnimTime
  {if (FFrame >= FFrames) and (not FTimeLoop) then
  begin
    Pause;
    Exit;
  end;}
end;

procedure TSprite.Play;
begin
  FPlaying := True;
end;

procedure TSprite.Pause;
begin
  FPlaying := False;
end;

procedure TSprite.Draw;
begin
  FImage.Draw(GetDrawRect, GetFrameRect);
end;

procedure TSprite.Draw(const AX, AY: Single);
begin
  FImage.Draw(
    Rectangle(Round(AX), Round(AY), DrawingWidth, DrawingHeight),
    GetFrameRect);
end;

procedure TSprite.Draw(const AX, AY, DrawWidth, DrawHeight: Single);
begin
  FImage.Draw(
    Rectangle(Round(AX), Round(AY), Round(DrawWidth), Round(DrawHeight)),
    GetFrameRect);
end;

procedure TSprite.Draw(const ScreenRectangle: TRectangle);
begin
  FImage.Draw(ScreenRectangle, GetFrameRect);
end;

procedure TSprite.Move(AX, AY: Integer; ARot: Single);
begin
  FX := AX;
  FY := AY;
  Rotation := ARot;
end;

function TSprite.AddSpriteAnimation(const AAnimation: TSpriteAnimation
  ): Integer;
begin
  Result := FAnimations.Add(AAnimation);
end;

function TSprite.SwitchToAnimation(const AAnimIndex: Integer;
  const ACustomFrame: Cardinal): Boolean;
begin
  if (AAnimIndex >= FAnimations.Count) or (AAnimIndex < 0) then
  begin
    Result := False;
    Exit;
  end;
  FCurrentAnimation := AAnimIndex;
  if ACustomFrame > High(FAnimations.Items[AAnimIndex]) then
    FCustomFrame := 0 else
      FCustomFrame := ACustomFrame;
  Result := True;
end;

function TSprite.Duration: Single;
begin
  Result := Frames / FramesPerSecond;
end;

{$endif read_implementation}
