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

uses Classes,
  CastleDownload;

type
  { Various statuses of TUnsplashDownload, used by @link(TUnsplashDownload.Status). }
  TUnsplashDownloadStatus = (

    { Download not started yet, so @link(TUnsplashDownload.Start) was not called yet. }
    usNotStarted,

    { @link(TUnsplashDownload.Start) was called, waiting for the answer
      from Unsplash "what is the ID and URL of a random image". }
    usWorkingRandomImageQuery,

    { @link(TUnsplashDownload.Start) was called, waiting for the answer
      from Unsplash "get contents of the image". }
    usWorkingDownload,

    { Download finished, failed. Consult log for details. }
    usError,

    { Download finished, success.
      @link(TUnsplashDownload.ImageStream) contains the image data.
      @link(TUnsplashDownload.UnsplashImageId) contains the image id. }
    usSuccess
  );

  { Get stream with image data (JPG) from Unsplash.
    Performs asynchronous image query and download from Unsplash.
    Usage:

    @orderedList(
      @item(
        Set input properties, like @link(Query),
        @link(Collections), @link(Orientation), @link(Size).
      )

      @item(
        Call @link(Start) to download the image.
      )

      @item(
        Wait for @link(Status) to no longer be @code(usWorkingXxx).
        @link(OnFinish) is also called when we finish.
      )

      @item(
        Read @link(Stream) with image data, if
        @link(Status) was @code(usSuccess).
      )
    )
  }
  TUnsplashDownload = class
  strict private
    const
      {$I unsplash_secrets.inc}
    var
      FUnsplashImageId: String;
      FSearchQuery: String;
      FCollections: String;
      FOrientation: String;
      FSize: String;
      FOnFinish: TNotifyEvent;
      FImageStream: TMemoryStream;
      FStatus: TUnsplashDownloadStatus;
      FDownload: TCastleDownload;
    procedure FinishRandomImageQuery(const Sender: TCastleDownload; var FreeSender: Boolean);
    procedure FinishDownload(const Sender: TCastleDownload; var FreeSender: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    { Set this before @link(Start)
      to specify search query to filter images.
      If empty, return any random image. }
    property SearchQuery: String read FSearchQuery write FSearchQuery;

    { Set this before @link(Start)
      to specify comma-separated list of public
      collection IDs to filter images.
      Use https://unsplash.com/collections to browse collections.
      If empty, don't filter by collections. }
    property Collections: String read FCollections write FCollections;

    { Set this before @link(Start)
      to specify orientation of the image.
      Allowed values: 'landscape', 'portrait', 'squarish'.
      Default is 'landscape'. }
    property Orientation: String read FOrientation write FOrientation;

    { Set this before @link(Start)
      to specify size of the image.
      Allowed values: 'full', 'raw', 'regular', 'small', 'thumb'.
      Default is 'small'. }
    property Size: String read FSize write FSize;

    { Determine a random image and download it.
      @link(Status) will change immediately to one of usWorkingXxx.
      Once finished, @link(Status) will change to usSuccess or usError
      and @link(OnFinish) is called.

      Note: Calling @name again, before the previous download finished,
      discards previous download (without calling @link(OnFinish)).
      Destroying an instance of this class also discards any ongoing download. }
    procedure Start;

    { Current status of the download. }
    property Status: TUnsplashDownloadStatus read FStatus;

    { Event called when we finish downloading. }
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;

    { Downloaded memory stream, if @link(Status) is usSuccess. }
    property ImageStream: TMemoryStream read FImageStream;

    { Once we successfully downloaded the image, this contains the image id.
      This is unique ID of the image specific to Unsplash.

      The image can be accessed online at URL
      @code('https://unsplash.com/photos/' + UnsplashImageId)
      (such URL will redirect to nicer-looking URL on access,
      that nicer URL will contain also image textual name,
      but the really important part is this unique id).

      This id can also be used as any unique id for your own purpses.
      It contains only safe characters,so can be used even as filename/URL part,
      e.g. you can save it as @code('castle-config:/' + UnsplashImageId + '.jpg').
      ( Though for security purposes, don't trust, and better check
      whether UnsplashImageId is really safe, before doing such thing. ) }
    property UnsplashImageId: String read FUnsplashImageId;

    { Wait for download to finish.
      Don't use this for asynchronous download, that keeps your
      application interactive while downloading.
      Use this only if you really want to just block the process,
      waiting for response. }
    procedure WaitForFinish;
  end;

implementation

uses SysUtils, FpJson, JsonParser,
  // enable https with FPC and TCastleDownload
  CastleHttps, CastleUriUtils, CastleApplicationProperties, CastleUtils,
  CastleLog;

{ TUnsplashDownload ----------------------------------------------------------- }

constructor TUnsplashDownload.Create;
begin
  inherited;
  FStatus := usNotStarted;
  FSize := 'small';
  FOrientation := 'landscape';
end;

destructor TUnsplashDownload.Destroy;
begin
  FreeAndNil(FDownload);
  FreeAndNil(FImageStream);
  inherited;
end;

procedure TUnsplashDownload.Start;
begin
  FreeAndNil(FDownload);
  FreeAndNil(FImageStream);

  FStatus := usWorkingRandomImageQuery;

  { First query for random image metadata.
    Returns JSON information about a random image from Unsplash.
    See https://unsplash.com/documentation#get-a-random-photo }

  FDownload := TCastleDownload.Create(nil);
  FDownload.HttpHeader('Authorization', 'Client-ID ' + UnsplashAccessKey);
  FDownload.Url := 'https://api.unsplash.com/photos/random?orientation=' + UrlEncode(FOrientation);
  if FSearchQuery <> '' then
    FDownload.Url := FDownload.Url + '&query=' + UrlEncode(FSearchQuery);
  if FCollections <> '' then
    FDownload.Url := FDownload.Url + '&collections=' + UrlEncode(FCollections);
  FDownload.OnFinish := {$ifdef FPC}@{$endif} FinishRandomImageQuery;
  FDownload.Start;

  WritelnLog('Querying random image from Unsplash');
end;

procedure TUnsplashDownload.FinishRandomImageQuery(const Sender: TCastleDownload; var FreeSender: Boolean);
var
  AnswerJson: TJsonObject;
  AnswerJsonData: TJsonData;
  ImageUrl: String;
begin
  case FDownload.Status of
    dsError:
      begin
        WritelnLog('Querying random image from Unsplash: error: %s', [
          FDownload.ErrorMessage
        ]);
        FStatus := usError;

        FreeAndNil(FDownload);
        if Assigned(OnFinish) then
          OnFinish(Self);
      end;
    dsSuccess:
      begin
        WritelnLog('Querying random image from Unsplash: success, JSON size %d', [
          FDownload.Contents.Size
        ]);

        AnswerJsonData := GetJson(FDownload.Contents);
        try
          AnswerJson := AnswerJsonData as TJsonObject;
          ImageUrl := AnswerJson.Objects['urls'].Strings[Size];
          FUnsplashImageId := AnswerJson.Strings['id'];
        finally FreeAndNil(AnswerJsonData) end;

        FStatus := usWorkingDownload;

        { Repurpose FDownload for next query, to get image from ImageUrl. }
        FDownload.Url := ImageUrl;
        FDownload.OnFinish := {$ifdef FPC}@{$endif} FinishDownload;
        FDownload.Start;
      end;
    else
      raise EInternalError.Create('Unexpected FDownload.Status in FinishRandomImageQuery');
  end;
end;

procedure TUnsplashDownload.FinishDownload(const Sender: TCastleDownload; var FreeSender: Boolean);
begin
  case FDownload.Status of
    dsError:
      begin
        WritelnLog('Downloading image from Unsplash: error: %s', [
          FDownload.ErrorMessage
        ]);
        FStatus := usError;

        FreeAndNil(FDownload);
        if Assigned(OnFinish) then
          OnFinish(Self);
      end;
    dsSuccess:
      begin
        WritelnLog('Downloading image from Unsplash: success, image stream size %d', [
          FDownload.Contents.Size
        ]);
        FImageStream := FDownload.Contents as TMemoryStream; // forced by soForceMemoryStream
        FDownload.OwnsContents := false;
        FStatus := usSuccess;

        FreeAndNil(FDownload);
        if Assigned(OnFinish) then
          OnFinish(Self);
      end;
    else
      raise EInternalError.Create('Unexpected FDownload.Status in FinishDownload');
  end;
end;

procedure TUnsplashDownload.WaitForFinish;
begin
  while Status in [usWorkingRandomImageQuery, usWorkingDownload] do
  begin
    ApplicationProperties._Update;
    Sleep(100);
  end;
end;

end.