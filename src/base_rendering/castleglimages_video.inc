{%MainUnit castleglimages.pas}
{
  Copyright 2001-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Part of CastleGLImages unit: videos rendered in OpenGL. }

{$ifdef read_interface}
{$ifdef read_interface_type}
// type (continue type section from previous include file)
  { Video as a sequence of OpenGL textures that can be easily played.
    Use TGLVideo3D to have a list of normal OpenGL textures,
    e.g. for rendering video as texture on free 3D objects.
    Use TGLVideo2D to have a list of GUI textures (TDrawableImage),
    e.g. for rendering video as simple 2D control. }
  TGLVideo = class
  private
    FCount: Integer;
    FTimeLoop: boolean;
    FTimeBackwards: boolean;
    FFramesPerSecond: Single;
    FWidth, FHeight: Cardinal;
  public
    { Constructor that initializes video from TVideo class.

      TVideo passed here must be already @link(TVideo.Loaded Loaded).

      Note that this class doesn't descend
      or keep reference to TVideo instance. The idea is that after
      creating TGLVideo instance, you can often free original TVideo
      instance (if you care only about playing the movie). This can
      conserve memory greatly, as TVideo keeps all frames in the memory,
      and so is rather memory-costly.
      (Actually, TGLVideo itself may eat a lot of texture memory,
      so be careful with large videos anyway.) }
    constructor Create(Video: TVideo);

    property Count: Integer read FCount;
    function IndexFromTime(const Time: TFloatTime): Integer;

    { See TVideo.FramesPerSecond. }
    property FramesPerSecond: Single read FFramesPerSecond write FFramesPerSecond;

    { See TVideo.TimeLoop. }
    property TimeLoop: boolean read FTimeLoop write FTimeLoop;

    { See TVideo.TimeBackwards. }
    property TimeBackwards: boolean
      read FTimeBackwards write FTimeBackwards;

    { Duration of the movie, in seconds.
      This is just the number of frames @link(Count) divided by FramesPerSecond. }
    function Duration: Single;

    property Width: Cardinal read FWidth;
    property Height: Cardinal read FHeight;
  end;

  { Video expressed as a series of textures, to play as texture on any 3D object. }
  TGLVideo3D = class(TGLVideo)
  private
    FItems: array of TGLTextureId;
  public
    constructor Create(Video: TVideo;
      const Filter: TTextureFilter;
      const Anisotropy: TGLfloat;
      const Wrap: TTextureWrap2D;
      const GUITexture:  boolean);
    destructor Destroy; override;

    function GLTextureFromTime(const Time: TFloatTime): TGLTextureId;
  end;

  { Video expressed as a series of TDrawableImage, to play as 2D GUI control. }
  TGLVideo2D = class(TGLVideo)
  private
    FItems: array of TDrawableImage;
  public
    constructor Create(Video: TVideo;
      const SmoothScaling: boolean = true); overload;
    constructor Create(const URL: string;
      const SmoothScaling: boolean = true); overload;
    constructor Create(const URL: string;
      const ResizeToX: Cardinal = 0;
      const ResizeToY: Cardinal = 0;
      const Interpolation: TResizeInterpolation = riBilinear); overload;
    destructor Destroy; override;

    function DrawableImageFromTime(const Time: TFloatTime): TDrawableImage;
    function GLImageFromTime(const Time: TFloatTime): TDrawableImage;
      deprecated 'use DrawableImageFromTime';
  end;
{$endif read_interface_type}

{$endif read_interface}

{$ifdef read_implementation}

{ TGLVideo ------------------------------------------------------------------- }

constructor TGLVideo.Create(Video: TVideo);
begin
  inherited Create;
  Check(Video.Loaded, 'Video must be loaded before using TGLVideo.Create');
  FCount := Video.Count;
  FTimeLoop := Video.TimeLoop;
  FTimeBackwards := Video.TimeBackwards;
  FFramesPerSecond := Video.FramesPerSecond;
  FWidth := Video.Width;
  FHeight := Video.Height
end;

function TGLVideo.IndexFromTime(const Time: TFloatTime): Integer;
begin
  Result := TVideo.FrameIndexFromTime(Time, Count, FramesPerSecond,
    TimeLoop, TimeBackwards);
end;

function TGLVideo.Duration: Single;
begin
  Result := Count / FramesPerSecond;
end;

{ TGLVideo3D ----------------------------------------------------------------- }

constructor TGLVideo3D.Create(Video: TVideo;
  const Filter: TTextureFilter; const Anisotropy: TGLfloat;
  const Wrap: TTextureWrap2D; const GUITexture: boolean);
var
  I: Integer;
begin
  inherited Create(Video);

  SetLength(FItems, Count);
  for I := 0 to High(FItems) do
  begin
    FItems[I] := LoadGLTexture(Video.Items[I], Filter, Wrap, nil, GUITexture);
    TexParameterMaxAnisotropy(GL_TEXTURE_2D, Anisotropy);
  end;
end;

destructor TGLVideo3D.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FItems) do
    glFreeTexture(FItems[0]);
  inherited;
end;

function TGLVideo3D.GLTextureFromTime(const Time: TFloatTime): TGLTextureId;
begin
  Result := FItems[IndexFromTime(Time)];
end;

{ TGLVideo2D ----------------------------------------------------------------- }

constructor TGLVideo2D.Create(Video: TVideo;
  const SmoothScaling: boolean = true);
var
  I: Integer;
begin
  inherited Create(Video);

  SetLength(FItems, Count);
  for I := 0 to High(FItems) do
    FItems[I] := TDrawableImage.Create(Video.Items[I], SmoothScaling, false);
end;

constructor TGLVideo2D.Create(const URL: string;
  const SmoothScaling: boolean = true);
var
  Video: TVideo;
begin
  Video := TVideo.Create;
  try
    Video.LoadFromFile(URL);
    Create(Video, SmoothScaling);
  finally FreeAndNil(Video) end;
end;

constructor TGLVideo2D.Create(const URL: string;
  const ResizeToX: Cardinal;
  const ResizeToY: Cardinal;
  const Interpolation: TResizeInterpolation);
var
  Video: TVideo;
begin
  Video := TVideo.Create;
  try
    Video.LoadFromFile(URL, ResizeToX, ResizeToY, Interpolation);
    Create(Video);
  finally FreeAndNil(Video) end;
end;

destructor TGLVideo2D.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FItems) do
    FreeAndNil(FItems[I]);
  inherited;
end;

function TGLVideo2D.DrawableImageFromTime(const Time: TFloatTime): TDrawableImage;
begin
  Result := FItems[IndexFromTime(Time)];
end;

function TGLVideo2D.GLImageFromTime(const Time: TFloatTime): TDrawableImage;
begin
  Result := DrawableImageFromTime(Time);
end;

{$endif read_implementation}
