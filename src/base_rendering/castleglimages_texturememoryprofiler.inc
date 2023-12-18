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

{ Part of CastleGLImages unit: texture memory profiler (TextureMemoryProfiler). }

{$ifdef read_interface}

{$ifdef read_interface_type}
// type (continue type section from previous include file)
  { OpenGL texture memory profiler. See @link(TextureMemoryProfiler)
    for usage documentation. }
  TTextureMemoryProfiler = class
  private
  type
    TAllocatedTexture = class
      TextureId: TGLTextureId;
      Url: String;
      Width, Height, Depth: Integer;
      Mipmaps: boolean;
      Size: Int64;
      ImageFormat: string;
    end;
    { Using TObjectList, not map, this way we can sort the list looking
      at data sizes. }
    TAllocatedTextures = {$ifdef FPC}specialize{$endif} TObjectList<TAllocatedTexture>;
  var
    AllocatedTextures: TAllocatedTextures;
    FEnabled: boolean;
    function FindTextureId(const TextureId: Integer): Integer;
    procedure CheckLeaks;
  public
    constructor Create;
    destructor Destroy; override;
    property Enabled: boolean read FEnabled write FEnabled;

    { Notify about texture memory allocation.
      This should be used only by code doing direct OpenGL operations. }
    procedure Allocate(const TextureId: TGLTextureId;
      const Url, ImageFormat: string; const Size: Int64; const Mipmaps: boolean;
      const Width, Height, Depth: Integer);

    { Notify about texture memory deallocation.
      This should be used only by code doing direct OpenGL operations. }
    procedure Deallocate(const TextureId: TGLTextureId);

    function Summary: string;

    { Helper function to calculate memory needed by all mipmaps of given Image.
      Assumes that all mipmaps are generated, as by GenerateMipmap call. }
    class function MipmapsSize(const Image: TEncodedImage): Int64;
  end;
{$endif read_interface_type}

{$ifdef read_interface_func}
{ OpenGL texture memory profiler, to detect which textures use up
  the GPU texture memory. Especially useful on mobile devices,
  where texture memory is limited and your application must really optimize
  texture usage. Also useful to detect texture memory leaks.

  Enable it at the very beginning of the application
  (for example in the main unit initialization clause), like this:

  @longCode(#
    TextureMemoryProfiler.Enabled := true
  #)

  Then at any point during the game you can query texture usage by
  @link(TTextureMemoryProfiler.Summary) method.
  For example dump it to the log like this:

  @longCode(#
    WritelnLog('Textures', TextureMemoryProfiler.Summary);
  #)

  The resulting output lists the currently allocated textures,
  in descending order of their size.
  This tells you which textures are worth optimizing.
  Maybe scale these textures down, maybe compress them,
  maybe remove alpha channel or convert them to grayscale...

  The profiler counts the @italic(real texture size on GPU),
  knowing that some textures are resized to power of 2,
  that textures may be compressed on GPU, that some textures have mipmaps
  and so on. All kinds of textures (2D, 3D, cubemaps, float, depth etc.)
  are covered. Every type is reported --- loaded from files,
  generated (like by GeneratedCubeMapTexture), embedded in code
  (by embedding images or fonts in code)... The textures loaded from disk
  are described by URL, for other textures we invent special URLs like:

  @unorderedList(
    @itemSpacing compact
    @item(@code(embedded-font:/CastleTextureFont_CanterBold_120))
    @item(@code(embedded-image:/PanelSeparator))
    @item(@code(generated:/GeneratedCubeMapTexture(GenTex2)))
  )

  You can also use the profiler from view3dscene, run it with
  "--debug-texture-memory" command-line option, load your 3D model
  and then use menu option "Console -> Print Texture Memory Usage".
}
function TextureMemoryProfiler: TTextureMemoryProfiler;

var
  { Log (through CastleLog) all texture and image loading to GPU.
    If additionally @link(TextureMemoryProfiler) is enabled, then we will dump
    texture memory usage. }
  LogTextureLoading: boolean = false;
{$endif read_interface_func}

{$endif read_interface}

{$ifdef read_implementation}

{ TTextureMemoryProfiler ----------------------------------------------------- }

constructor TTextureMemoryProfiler.Create;
begin
  inherited;
  AllocatedTextures := TAllocatedTextures.Create(true);
end;

destructor TTextureMemoryProfiler.Destroy;
begin
  FreeAndNil(AllocatedTextures);
  inherited;
end;

procedure TTextureMemoryProfiler.Allocate(const TextureId: TGLTextureId;
  const Url, ImageFormat: string;
  const Size: Int64; const Mipmaps: boolean;
  const Width, Height, Depth: Integer);
var
  AllocatedTex: TAllocatedTexture;
  I: Integer;
begin
  if FEnabled then
  begin
    AllocatedTex := TAllocatedTexture.Create;
    AllocatedTex.Url := Url;
    AllocatedTex.Width := Width;
    AllocatedTex.Height := Height;
    AllocatedTex.Depth := Depth;
    AllocatedTex.Mipmaps := Mipmaps;
    AllocatedTex.Size := Size;
    AllocatedTex.ImageFormat := ImageFormat;
    AllocatedTex.TextureId := TextureId;
    I := FindTextureId(TextureId);
    if I <> -1 then
      AllocatedTextures[I] := AllocatedTex else
      AllocatedTextures.Add(AllocatedTex);
  end;

  if LogTextureLoading then
  begin
    WritelnLog('Texture Loading', 'Loaded to GPU texture "%s" size %d x %d',
      [UriDisplay(Url), Width, Height]);
    if FEnabled then
      WritelnLogMultiline('Texture Loading', Summary);
  end;
end;

procedure TTextureMemoryProfiler.Deallocate(const TextureId: TGLTextureId);
var
  I: Integer;
begin
  if not ApplicationProperties.IsGLContextOpen then
    WritelnWarning('Textures', Format(
      'Releasing texture id %d after the rendering context has been closed. You release some rendering resources too late. You should release (at the latest) in an event registered on "ApplicationProperties.OnGLContextClose".' + NL +
      'That said, all classes (like TDrawableImage, TCastleScene, TCastleImageControl, TCastleImageTransform) should do this management correctly automatically, so this warning should never happen.' + ' If you see this -> please submit a bug to Castle Game Engine with a testcase to reproduce this problem.',
      [TextureId]));

  if FEnabled then
  begin
    I := FindTextureId(TextureId);
    if I = -1 then
      WritelnWarning('Textures', Format('Texture id %d is released, but was not reported as allocated to TextureMemoryProfiler. Probably TextureMemoryProfiler was not enabled when the texture was created, which may mean TextureMemoryProfiler was enabled too late to accurately capture everything.',
        [TextureId])) else
      AllocatedTextures.Delete(I);
  end;
end;

procedure TTextureMemoryProfiler.CheckLeaks;
begin
  if (AllocatedTextures.Count <> 0) and
     ((AllocatedTextures.Count <> 1) or
      { texture id 0 may be allocated, but never released }
      (AllocatedTextures[0].TextureId = 0)) then
  begin
    WritelnWarning('Textures', Format('TextureMemoryProfiler contains some textures when closing the rendering context. Possibly we have texture memory leak (textures will be freed anyway when closing rendering context, but possibly we could free them earlier).'+' Or TextureMemoryProfiler was only enabled for part of program''s code. We have %d textures, 1st one is "%s"',
      [AllocatedTextures.Count,
       AllocatedTextures[0].Url]));
  end;
end;

function TTextureMemoryProfiler.FindTextureId(const TextureId: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to AllocatedTextures.Count - 1 do
    if AllocatedTextures[I].TextureId = TextureId then
      Exit(I);
  Result := -1;
end;

class function TTextureMemoryProfiler.MipmapsSize(const Image: TEncodedImage): Int64;
var
  W, H, D: Cardinal;
  Size: Int64;
begin
  W := Image.Width;
  H := Image.Height;
  D := Image.Depth;
  Size := Image.Size;

  Result := 0;

  if (W = 0) or (H = 0) or (D = 0) then Exit; // empty image data

  while (W > 1) or (H > 1) or (D > 1) do
  begin
    if W > 1 then begin W := W div 2; Size := Size div 2; end;
    if H > 1 then begin H := H div 2; Size := Size div 2; end;
    if D > 1 then begin D := D div 2; Size := Size div 2; end;
    Result := Result + Size;
  end;

{  WritelnLog('Mipmaps', Format('Mipmap sizes is %d for original image size %d (%f)',
    [Result, Image.Size, Result / Image.Size]));}
end;

function AllocatedTexturesSort({$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} Value1, Value2: TTextureMemoryProfiler.TAllocatedTexture): Integer;
begin
  Result := Value2.Size - Value1.Size;
end;

function TTextureMemoryProfiler.Summary: string;

  function FormatSize(const Size: Int64): string;
  begin
    if Size >= 1024 * 1024 * 1024 then
      Result := Format('%f GB', [Size / (1024 * 1024 * 1024)]) else
    if Size >= 1024 * 1024 then
      Result := Format('%f MB', [Size / (1024 * 1024)]) else
    if Size >= 1024 then
      Result := Format('%f KB', [Size / 1024]) else
      Result := IntToStr(Size) + ' B';
  end;

type
  TAllocatedTextureComparer = {$ifdef FPC}specialize{$endif} TComparer<TAllocatedTexture>;
var
  S: string;
  I: Integer;
  Used: Int64;
  AllocatedTex: TAllocatedTexture;
begin
  S := '';

  Used := 0;
  for I := 0 to AllocatedTextures.Count - 1 do
    Used := Used + AllocatedTextures[I].Size;
  S := Format('Texture memory used: %s (%d bytes in %d textures)',
    [FormatSize(Used), Used, AllocatedTextures.Count]) + NL;

  AllocatedTextures.Sort(TAllocatedTextureComparer.Construct({$ifdef FPC}@{$endif}AllocatedTexturesSort));
  for I := 0 to AllocatedTextures.Count - 1 do
  begin
    AllocatedTex := AllocatedTextures[I];
    S := S + Format('  %f - %s (size %d : format %s, dimensions %d x %d x %d, mipmaps: %s)',
      [AllocatedTex.Size / Used, AllocatedTex.Url,
       AllocatedTex.Size, AllocatedTex.ImageFormat,
       AllocatedTex.Width, AllocatedTex.Height, AllocatedTex.Depth,
       BoolToStr(AllocatedTex.Mipmaps, true)]) + NL;
  end;
  Result := S;
end;

var
  FTextureMemoryProfiler: TTextureMemoryProfiler;

function TextureMemoryProfiler: TTextureMemoryProfiler;
begin
  if FTextureMemoryProfiler = nil then
    FTextureMemoryProfiler := TTextureMemoryProfiler.Create;
  Result := FTextureMemoryProfiler;
end;

{$endif read_implementation}
