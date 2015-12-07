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
  TCastleSprite2D = class
    private
      FImage: TGLImage;
      FX, FY: Integer;
      FDrawingWidth, FDrawingHeight: Cardinal;
      //FCenterX, FCenterY, FRotation: Single;
      FFrames, FFrameWidth, FFrameHeight, FCols, FRows: Cardinal;
      FFPS, FFrameDuration: Single;
      FAnimTime: Double;
      FFrame: Cardinal;
      FPlaying: Boolean;
      FLooped: Boolean;
      function GetCenterX: Single;
      function GetCenterY: Single;
      function GetFrameRect: TRectangle;
      function GetDrawRect: TRectangle;
      function GetCenter: TVector2Single;
      function GetPosition: TVector2Integer;
      function GetRotation: Single;
      procedure SetCenter(AValue: TVector2Single);
      procedure SetCenterX(AValue: Single);
      procedure SetCenterY(AValue: Single);
      procedure SetDrawRect(AValue: TRectangle);
      procedure SetFPS(AValue: Single);
      procedure SetFrame(AValue: Cardinal);
      procedure SetPosition(APosition: TVector2Integer);
      procedure SetRotation(AValue: Single);
    public
      { Constructor.
        @param(URL) - URL of source image,
        @param(AFrames) - total numer of animation frames,
        @param(ACols) - number of columns,
        @param(ARows) - number of rows,
        @param(ASmoothscaling) - enables interpolation, see TGLImage fo details,
        @param(ALooped) - should animation be repeated?, default True,
        @param(APlay) - is animation playing? if not then current frame will be displaied. }
      constructor Create(const URL: string;
        const AFrames, ACols, ARows: Cardinal;
        const ASmoothScaling: Boolean = False;
        const ALooped: Boolean = True; const APlay: Boolean=False);
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
      property FPS: Single read FFPS write SetFPS default 10;

      { Current frame. }
      property Frame: Cardinal read FFrame write SetFrame;

      { Is animation proceed? }
      property Playing: Boolean read FPlaying write FPlaying default False;

      { Is animation looped? }
      property Looped: Boolean read FLooped write FLooped;
  end;

  {$endif read_interface}

  {$ifdef read_implementation}

function TCastleSprite2D.GetPosition: TVector2Integer;
begin
  Result[0] := FX;
  Result[1] := FY;
end;

function TCastleSprite2D.GetRotation: Single;
begin
  Result := FImage.Rotation;
end;

function TCastleSprite2D.GetFrameRect: TRectangle;
begin
  Result.Left := FFrame mod FCols * FFrameWidth;
  Result.Bottom := (FImage.Height - FFrameHeight) - FFrame div FCols * FFrameHeight;
  Result.Width := FFrameWidth;
  Result.Height := FFrameHeight;
end;

function TCastleSprite2D.GetCenterX: Single;
begin
  Result := FImage.CenterX;
end;

function TCastleSprite2D.GetCenterY: Single;
begin
  Result := FImage.CenterY;
end;

function TCastleSprite2D.GetDrawRect: TRectangle;
begin
  Result.Left := FX;
  Result.Bottom := FY;
  Result.Width := FDrawingWidth;
  Result.Height := FDrawingHeight;
end;

function TCastleSprite2D.GetCenter: TVector2Single;
begin
  Result[0] := CenterX;
  Result[1] := CenterY;
end;

procedure TCastleSprite2D.SetCenter(AValue: TVector2Single);
begin
  CenterX := AValue[0];
  CenterY := AValue[1];
end;

procedure TCastleSprite2D.SetCenterX(AValue: Single);
begin
  FImage.CenterX := AValue;
end;

procedure TCastleSprite2D.SetCenterY(AValue: Single);
begin
  FImage.CenterY := AValue;
end;

procedure TCastleSprite2D.SetDrawRect(AValue: TRectangle);
begin
  FX := AValue.Left;
  FY := AValue.Bottom;
  FDrawingWidth := AValue.Width;
  FDrawingHeight := AValue.Height;
end;

procedure TCastleSprite2D.SetFPS(AValue: Single);
begin
  if FFPS = AValue then Exit;
  FFPS := AValue;
  FFrameDuration := 1 / FFPS;
end;

procedure TCastleSprite2D.SetFrame(AValue: Cardinal);
begin
  if (FFrame = AValue) or (AValue >= FFrames) then Exit;
  FFrame := AValue;
end;

procedure TCastleSprite2D.SetPosition(APosition: TVector2Integer);
begin
  FX := APosition[0];
  FY := APosition[1];
end;

procedure TCastleSprite2D.SetRotation(AValue: Single);
begin
  FImage.Rotation := AValue;
end;

constructor TCastleSprite2D.Create(const URL: string;
  const AFrames, ACols, ARows: Cardinal;
  const ASmoothScaling: Boolean;
  const ALooped: Boolean = True; const APlay: Boolean=False);
begin
  FImage := TGLImage.Create(URL, ASmoothScaling);
  if not Assigned(FImage) then Exit; //todo: error
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
  SetFPS(10); // set 10 FPS as deafault
  FLooped := ALooped;
end;

destructor TCastleSprite2D.Destroy;
begin
  FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TCastleSprite2D.Update(ATime: Single);
begin
  if not FPlaying then Exit;
  FAnimTime += ATime;
  FFrame := Round(FAnimTime / FFrameDuration);
  if FFrame >= FFrames then
  begin
    if not FLooped then
    begin
      Pause;
      Exit;
    end;
    FFrame := FFrame mod FFrames;
  end;
end;

procedure TCastleSprite2D.Play;
begin
  FPlaying := True;
end;

procedure TCastleSprite2D.Pause;
begin
  FPlaying := False;
end;

procedure TCastleSprite2D.Draw;
begin
  FImage.Draw(GetDrawRect, GetFrameRect);
end;

procedure TCastleSprite2D.Move(AX, AY: Integer; ARot: Single);
begin
  FX := AX;
  FY := AY;
  Rotation := ARot;
end;

{$endif read_implementation}
