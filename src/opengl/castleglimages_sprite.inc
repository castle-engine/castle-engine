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

{ Part of CastleGLImages unit: drawing 2D images on screen (TGLImage class). }

{$ifdef read_interface}

type

  //todo: individual each frame duration

  { Sprite images class. Sprite is an image with many animation frames arranged
    in rows and columns. Frames are readed from left to right and from top
    to bottom. }
  TSprite = class
  private
    FImage: TGLImage;
    FX, FY: Integer;
    FDrawingWidth, FDrawingHeight: Cardinal;
    FFrames, FFrameWidth, FFrameHeight, FCols, FRows: Cardinal;
    FFramesPerSecond, FFrameDuration: Single;
    FAnimTime: TFloatTime;
    FFrame: Cardinal;
    FPlaying: Boolean;
    FTimeLoop: Boolean;
    FLeftMarginOffset, FTopMarginOffset: Cardinal;
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
      @param(URL) - URL of source image,
      @param(AFrames) - total numer of animation frames,
      @param(ACols) - number of columns,
      @param(ARows) - number of rows,
      @param(ASmoothscaling) - enables interpolation, see TGLImage for details, default True,
      @param(ATimeLoop) - should animation be repeated?, default True,
      @param(APlay) - is animation playing? if not then current frame will be displaied. }
    constructor Create(const URL: string;
      const AFrames, ACols, ARows: Cardinal;
      const ASmoothScaling: Boolean = True;
      const ATimeLoop: Boolean = True; const APlay: Boolean=False);
    destructor Destroy; override;

    { Update current frame.
      @param(ATime) time from previous update. }
    procedure Update(ATime: Single);

    { Play animation. }
    procedure Play;

    { Pause animation. }
    procedure Pause;

    { Draw the sprite. }
    procedure Draw;

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

    { Drawing rectangle. }
    property DrawRect: TRectangle read GetDrawRect write SetDrawRect;

    { Center of rotation. Values between 0 and 1. }
    property Center: TVector2Single read GetCenter write SetCenter;

    { Rotation in degrees. }
    property Rotation: Single read GetRotation write SetRotation default 0;

    { Total number of frames. Readonly. Defined at creation. }
    property Frames: Cardinal read FFrames;

    { Frames Per Second for sprite animation. }
    property FramesPerSecond: Single read FFramesPerSecond write SetFramesPerSecond default DefaultFramesPerSecond;

    { Current frame. }
    property Frame: Cardinal read FFrame write SetFrame;

    { Is animation proceed? }
    property Playing: Boolean read FPlaying write FPlaying default False;

    { Is animation TimeLoop? }
    property TimeLoop: Boolean read FTimeLoop write FTimeLoop;

    { Animation time can be set explicity. Writing to this property also recalculate
      current frame. }
    property Time: TFloatTime read FAnimTime write SetAnimTime;

    { Width of single frame. Default value is calculated in constructor
      depend on cols count. }
    property FrameWidth: Cardinal read FFrameWidth write FFrameWidth;

    { Height of single frame. Default value is calculated in constructor
      depend on rows count. }
    property FrameHeight: Cardinal read FFrameHeight write FFrameHeight;

    { Define X offset. Useful if first frame don't start at X=0. }
    property LeftMarginOffset: Cardinal read FLeftMarginOffset write FLeftMarginOffset default 0;

    { Define Y offset. Useful if first frame don't start at Y=0. }
    property TopMarginOffset: Cardinal read FTopMarginOffset write FTopMarginOffset default 0;
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
begin
  if FAnimTime = AValue then Exit;
  FAnimTime := AValue;
  FFrame := TVideo.FrameIndexFromTime(FAnimTime, FFrames, FFramesPerSecond, FTimeLoop, False); //todo: add time backward
end;

function TSprite.GetFrameRect: TRectangle;
begin
  Result.Left := FFrame mod FCols * FFrameWidth + FLeftMarginOffset;
  Result.Bottom := (FImage.Height - FFrameHeight) - FFrame div FCols * FFrameHeight - FTopMarginOffset;
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
  FFrameDuration := 1 / FFramesPerSecond;
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

constructor TSprite.Create(const URL: string;
  const AFrames, ACols, ARows: Cardinal;
  const ASmoothScaling: Boolean = True;
  const ATimeLoop: Boolean = True; const APlay: Boolean=False);
begin
  FImage := TGLImage.Create(URL, ASmoothScaling);
  FFrames := AFrames;
  FCols := ACols;
  FRows := ARows;
  FFrameWidth := FImage.Width div FCols;
  FFrameHeight := FImage.Height div FRows;
  FDrawingWidth := FFrameWidth;
  FDrawingHeight := FFrameHeight;
  FPlaying := APlay;
  FFrame := 0;
  FAnimTime := 0;
  SetFramesPerSecond(DefaultFramesPerSecond);
  FTimeLoop := ATimeLoop;
  FLeftMarginOffset := 0;
  FTopMarginOffset := 0;
end;

destructor TSprite.Destroy;
begin
  FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TSprite.Update(ATime: Single);
begin
  if not FPlaying then Exit;

  Time := Time + ATime;

  if (FFrame >= FFrames) and (not FTimeLoop) then
  begin
    Pause;
    Exit;
  end;
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

procedure TSprite.Move(AX, AY: Integer; ARot: Single);
begin
  FX := AX;
  FY := AY;
  Rotation := ARot;
end;

{$endif read_implementation}
