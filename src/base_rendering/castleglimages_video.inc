{%MainUnit castleglimages.pas}
{
  Copyright 2001-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
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
    FRHITextures: array of TRHITexture;
    FRHISampler: TRHISampler;
  public
    constructor Create(Video: TVideo;
      const Filter: TTextureFilter;
      const Anisotropy: TGLfloat;
      const Wrap: TTextureWrap2D;
      const GUITexture:  boolean);
    destructor Destroy; override;

    function GLTextureFromTime(const Time: TFloatTime): TGLTextureId;
    function RHITextureFromTime(const Time: TFloatTime): TRHITexture;
    function RHITexture(const Index: Integer): TRHITexture;
    property RHISampler: TRHISampler read FRHISampler;
  end;

  { Video expressed as a series of TDrawableImage, to play as 2D GUI control. }
  TGLVideo2D = class(TGLVideo)
  private
    FItems: array of TDrawableImage;
  public
    constructor Create(Video: TVideo;
      const SmoothScaling: boolean = true); overload;
    constructor Create(const Url: String;
      const SmoothScaling: boolean = true); overload;
    constructor Create(const Url: String;
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

  function WrapGLToRHI(const W: TGLenum): TRHISamplerAddressMode;
  begin
    if W = GL_REPEAT then
      Result := amRepeat
    else
    if W = GL_MIRRORED_REPEAT then
      Result := amMirroredRepeat
    else
      Result := amClampToEdge;
  end;

var
  I: Integer;
  MipCount, FrameW, FrameH: Cardinal;
  TexDesc: TRHITextureDesc;
  SamplerDesc: TRHISamplerDesc;
  ResizedFrame: TCastleImage;
begin
  inherited Create(Video);

  SetLength(FItems, Count);
  SetLength(FRHITextures, Count);

  if RHIDevice <> nil then
  begin
    for I := 0 to High(FRHITextures) do
    begin
      if Video.Items[I] <> nil then
      begin
        FrameW := Max(1, Video.Items[I].Width);
        FrameH := Max(1, Video.Items[I].Height);
        if (not GUITexture) and (GLTextureScale > 1) then
        begin
          if FrameW > GLTextureMinSize then
            FrameW := Max(FrameW shr (GLTextureScale - 1), GLTextureMinSize);
          if FrameH > GLTextureMinSize then
            FrameH := Max(FrameH shr (GLTextureScale - 1), GLTextureMinSize);
        end;
        if (not GUITexture) and Filter.NeedsMipmaps then
          MipCount := CalculateTextureMipLevels(Max(Cardinal(1), Cardinal(Max(FrameW, FrameH))))
        else
          MipCount := 1;
        TexDesc := TRHITextureDesc.Create2D(
          FrameW,
          FrameH,
          pfRGBA8_UNorm,
          [tuSampled, tuTransferSrc, tuTransferDst],
          MipCount);
        FRHITextures[I] := RHIDevice.CreateTexture(TexDesc);
        if ((FrameW <> Video.Items[I].Width) or (FrameH <> Video.Items[I].Height)) and
           (Video.Items[I] is TCastleImage) then
        begin
          ResizedFrame := TCastleImage(Video.Items[I]).MakeResized(FrameW, FrameH, riBilinear);
          try
            FRHITextures[I].UploadImage(ResizedFrame);
          finally
            FreeAndNil(ResizedFrame);
          end;
        end else
          FRHITextures[I].UploadImage(Video.Items[I]);
        if MipCount > 1 then
          FRHITextures[I].GenerateMipmaps;
        TextureMemoryProfiler.AllocateRHI(FRHITextures[I], Video.Items[I].URL,
          Video.Items[I].ClassName,
          Int64(TexDesc.Width) * Int64(TexDesc.Height) * 4 +
          Iff(MipCount > 1, TTextureMemoryProfiler.MipmapsSize(Video.Items[I]), 0),
          MipCount > 1, TexDesc.Width, TexDesc.Height, 1);
      end;
    end;

    FillChar(SamplerDesc, SizeOf(SamplerDesc), 0);
    if Filter.Magnification = magNearest then
      SamplerDesc.MagFilter := sfNearest
    else
      SamplerDesc.MagFilter := sfLinear;
    if Filter.Minification in [minNearest, minNearestMipmapNearest, minNearestMipmapLinear] then
      SamplerDesc.MinFilter := sfNearest
    else
      SamplerDesc.MinFilter := sfLinear;
    if Filter.Minification in [minNearestMipmapNearest, minLinearMipmapNearest] then
      SamplerDesc.MipFilter := sfNearest
    else
      SamplerDesc.MipFilter := sfLinear;
    SamplerDesc.AddressModeU := WrapGLToRHI(Wrap.Data[0]);
    SamplerDesc.AddressModeV := WrapGLToRHI(Wrap.Data[1]);
    SamplerDesc.AddressModeW := amClampToEdge;
    SamplerDesc.MaxAnisotropy := Max(1.0, Anisotropy);
    if MipCount > 1 then
      SamplerDesc.MaxLod := 1000.0
    else
      SamplerDesc.MaxLod := 0.0;
    FRHISampler := RHIDevice.CreateSampler(SamplerDesc);
  end;

  if ApplicationProperties.IsGLContextOpen and (GLFeatures <> nil) then
  begin
    for I := 0 to High(FItems) do
    begin
      FItems[I] := LoadGLTexture(Video.Items[I], Filter, Wrap, nil, GUITexture);
      TexParameterMaxAnisotropy(GL_TEXTURE_2D, Anisotropy);
    end;
  end;
end;

destructor TGLVideo3D.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FRHITextures) do
    if FRHITextures[I] <> nil then
    begin
      TextureMemoryProfiler.DeallocateRHI(FRHITextures[I]);
      FreeAndNil(FRHITextures[I]);
    end;
  FreeAndNil(FRHISampler);
  for I := 0 to High(FItems) do
    if FItems[I] <> GLObjectNone then
      FreeTexture(FItems[I]);
  inherited;
end;

function TGLVideo3D.GLTextureFromTime(const Time: TFloatTime): TGLTextureId;
begin
  Result := FItems[IndexFromTime(Time)];
end;

function TGLVideo3D.RHITextureFromTime(const Time: TFloatTime): TRHITexture;
begin
  Result := FRHITextures[IndexFromTime(Time)];
end;

function TGLVideo3D.RHITexture(const Index: Integer): TRHITexture;
begin
  if (Index >= 0) and (Index <= High(FRHITextures)) then
    Result := FRHITextures[Index]
  else
    Result := nil;
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

constructor TGLVideo2D.Create(const Url: String;
  const SmoothScaling: boolean = true);
var
  Video: TVideo;
begin
  Video := TVideo.Create;
  try
    Video.LoadFromFile(Url);
    Create(Video, SmoothScaling);
  finally FreeAndNil(Video) end;
end;

constructor TGLVideo2D.Create(const Url: String;
  const ResizeToX: Cardinal;
  const ResizeToY: Cardinal;
  const Interpolation: TResizeInterpolation);
var
  Video: TVideo;
begin
  Video := TVideo.Create;
  try
    Video.LoadFromFile(Url, ResizeToX, ResizeToY, Interpolation);
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
