{%MainUnit castleglimages.pas}
{
  Copyright 2019-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Part of CastleGLImages unit: TDrawableImageCache. }

{$ifdef read_interface}

{$endif read_interface}

{$ifdef read_implementation}

type
  { Cache of TDrawableImage, one instance for one URL.
    This is only internally used by TCastleImagePersistent now,
    not visible from the outside. }
  TDrawableImageCache = class
  strict private
    type
      TCachedDrawableImage = class
        References: Cardinal;
        Url: String;
        DrawableImage: TDrawableImage;
      end;
      TCachedDrawableImageList = {$ifdef FPC}specialize{$endif} TObjectList<TCachedDrawableImage>;
    var
      Cached: TCachedDrawableImageList;
  public
    Log: Boolean;
    constructor Create;
    destructor Destroy; override;

    { Load new image, possibly only increasing reference to cache.

      TODO: ASmoothScaling is not used as a key to select different cache,
      instead it's just the initial value in case we need to create
      the image.
      In the long run, we should probably have different cache entries
      for different ASmoothScaling versions. }
    function LoadDrawableImage(const Url: String;
      const ASmoothScaling: Boolean): TDrawableImage;
    procedure FreeDrawableImage(var DrawableImage: TDrawableImage);
  end;

function DrawableImageCache: TDrawableImageCache; forward;

{ ---------------------------------------------------------------------------- }

constructor TDrawableImageCache.Create;
begin
  inherited;
  Cached := TCachedDrawableImageList.Create;
end;

destructor TDrawableImageCache.Destroy;
begin
  if Cached <> nil then
  begin
    Assert(Cached.Count = 0, 'Some references to TDrawableImage still exist when freeing TDrawableImageCache');
    FreeAndNil(Cached);
  end;
  inherited;
end;

function TDrawableImageCache.LoadDrawableImage(const Url: String;
  const ASmoothScaling: Boolean): TDrawableImage;
var
  C: TCachedDrawableImage;
begin
  for C in Cached do
  begin
    if C.Url = Url then
    begin
      Inc(C.References);
      if Log then
        WritelnLog('DrawableImageCache', '++ : TDrawableImage %s : %d', [Url, C.References]);
      Exit(C.DrawableImage);
    end;
  end;

  Result := TDrawableImage.Create(Url);

  C := TCachedDrawableImage.Create;
  C.References := 1;
  C.Url := Url;
  C.DrawableImage := Result;
  Cached.Add(C);

  if Log then
    WritelnLog('DrawableImageCache', '++ : TDrawableImage %s : %d', [Url, C.References]);
end;

procedure TDrawableImageCache.FreeDrawableImage(var DrawableImage: TDrawableImage);
var
  I: Integer;
  C: TCachedDrawableImage;
begin
  if DrawableImage = nil then Exit;

  for I := 0 to Cached.Count - 1 do
  begin
    C := Cached[I];
    if C.DrawableImage = DrawableImage then
    begin
      if Log then
        WritelnLog('DrawableImageCache', '-- : TDrawableImage %s : %d', [C.Url, C.References - 1]);
      DrawableImage := nil;
      if C.References = 1 then
      begin
        FreeAndNil(C.DrawableImage);
        Cached.Delete(I);
      end else
        Dec(C.References);
      Exit;
    end;
  end;

  raise EInternalError.CreateFmt('FreeDrawableImage: no reference found for image %s',
    [PointerToStr(DrawableImage)]);
end;

var
  FDrawableImageCache: TDrawableImageCache;

function DrawableImageCache: TDrawableImageCache;
begin
  if FDrawableImageCache = nil then
  begin
    FDrawableImageCache := TDrawableImageCache.Create;
    // FDrawableImageCache.Log := true; // for debug
  end;
  Result := FDrawableImageCache;
end;

{$endif read_implementation}
