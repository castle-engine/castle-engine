{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the files COPYING*,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Download image from Unsplash.
  You can copy and use this unit as-is in your applications.

  See Unsplash docs: https://unsplash.com/documentation#get-a-random-photo }
unit GameUnsplash;

interface

uses Classes;

{ Get stream with image data (JPG) from Unsplash.

  @param(UnsplashImageId Unique ID of the image specific to Unsplash.
    Can be used as any unique id, or to later refer to the image on Unsplash.)

  @param(SearchQuery Optional search query to filter images.
    If empty, return any random image.)

  @param(Collections Optional comma-separated list of public
    collection IDs to filter images.)

  @param(Orientation Optional orientation to filter images.
    Allowed values: landscape, portrait, squarish )

  @param(Size Optional size of the image.
    Allowed values: full, raw, regular, small, thumb )
}
function UnsplashGetRandomImage(out UnsplashImageId: String;
  const SearchQuery: String = '';
  const Collections: String = '';
  const Orientation: String = 'landscape';
  const Size: String = 'small'): TMemoryStream;

implementation

uses SysUtils, FpJson, JsonParser,
  // enable https with FPC and TCastleDownload
  {$ifdef FPC} {$ifndef VER3_0} OpenSSLSockets, {$endif} {$endif}
  CastleDownload, CastleUriUtils,
  CastleLog;

const
  {$I unsplash_secrets.inc}

function UnsplashGetRandomImage(out UnsplashImageId: String;
  const SearchQuery, Collections, Orientation, Size: String): TMemoryStream;

  { Returns JSON information about a random image from Unsplash.
    See https://unsplash.com/documentation#get-a-random-photo }
  function QueryRandomImage: TJsonData;
  var
    D: TCastleDownload;
  begin
    D := TCastleDownload.Create(nil);
    try
      D.HttpHeader('Authorization', 'Client-ID ' + UnsplashAccessKey);
      D.Url := 'https://api.unsplash.com/photos/random?orientation=' + UrlEncode(Orientation);
      if SearchQuery <> '' then
        D.Url := D.Url + '&query=' + UrlEncode(SearchQuery);
      if Collections <> '' then
        D.Url := D.Url + '&collections=' + UrlEncode(Collections);
      WritelnLog('Querying random image from Unsplash: begin');
      D.Start;
      D.WaitForFinish;
      if D.Status <> dsSuccess then
        raise Exception.Create('Failed to download random image from Unsplash: ' + D.ErrorMessage);
      WritelnLog('Querying random image from Unsplash: success, JSON size %d', [
        D.Contents.Size
      ]);
      Result := GetJson(D.Contents);
    finally FreeAndNil(D) end;
  end;

  { Stream with image data. }
  function QueryGetImage(const ImageUrl: String): TMemoryStream;
  var
    D: TCastleDownload;
  begin
    D := TCastleDownload.Create(nil);
    try
      D.HttpHeader('Authorization', 'Client-ID ' + UnsplashAccessKey);
      D.Url := ImageUrl;
      WritelnLog('Downloading image from Unsplash: begin, URL: ' + ImageUrl);
      D.OwnsContents := false;
      D.Options := [soForceMemoryStream];
      D.Start;
      D.WaitForFinish;
      if D.Status <> dsSuccess then
        raise Exception.Create('Failed to download image from Unsplash: ' + D.ErrorMessage);
      WritelnLog('Downloading image from Unsplash: success, image stream size %d', [
        D.Contents.Size
      ]);
      Result := D.Contents as TMemoryStream; // forced by soForceMemoryStream
    finally FreeAndNil(D) end;
  end;

var
  AnswerJson: TJsonObject;
  ImageUrl: String;
begin
  AnswerJson := QueryRandomImage as TJsonObject;
  try
    ImageUrl := AnswerJson.Objects['urls'].Strings[Size];
    UnsplashImageId := AnswerJson.Strings['id'];
  finally FreeAndNil(AnswerJson) end;

  WritelnLog('Image from Unsplash id: ' + UnsplashImageId);

  Result := QueryGetImage(ImageUrl);
end;

end.