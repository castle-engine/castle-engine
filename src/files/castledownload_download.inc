{
  Copyright 2013-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

type
  { See @link(TDownload.Status). }
  TDownloadStatus = (dsNotStarted, dsDownloading, dsError, dsSuccess);

  { See @link(TDownload.HttpMethod). }
  THttpMethod = (hmGet, hmPost, hmPut, hmDelete, hmOptions, hmHead);

  { Download an URL asynchronously, without blocking the application.
    You can register a callback @link(OnFinish) or watch
    when the @link(Status) property changes from dsDownloading to dsError or dsSuccess
    to detect when this finished downloading.

    The download starts when you call @link(Start).
    Be sure to configure the properties, including @link(OnFinish),
    before calling @link(Start), because in case of some protocols @link(Start) may immediately
    read everything and finish.
    When the download ends, the @link(OnFinish) is called and @link(Status) changes.

    You can always just free an instance of this class, this will
    break the download immediately, if it's still in-progress.

    The download continues while your application is running,
    because we use @link(ApplicationProperties.OnUpdate TCastleApplicationProperties.OnUpdate)
    mechanism. If your application uses TCastleWindowBase or TCastleControlBase,
    then this just works.
    Note that if you just want to wait for download to finish,
    you can use @link(WaitForFinish) method or just call simpler @link(Download)
    routine.

    Do not worry whether this uses threads (or not) internally.
    All the methods and properties of this class should be accessed
    from the main thread, the same thread you use for all Castle Game Engine
    functions. And the OnFinish is called in the main thread,
    so you can handle it without worrying about threading.
  }
  TCastleDownload = class(TComponent)
  strict private
    FUrl: String;
    FHttpMethod: THttpMethod;
    FOnFinish: TNotifyEvent;
    FStatus: TDownloadStatus;
    FErrorMessage: String;
    FContents: TStream;
    FOwnsContents: Boolean;
    FOptions: TStreamOptions;
    FTotalBytes, FDownloadedBytes: Int64;
    FMimeType: String;
    procedure SetUrl(const Value: String);
    procedure SetHttpMethod(const Value: THttpMethod);
    procedure SetOnFinish(const Value: TNotifyEvent);
    procedure SetOptions(const Value: TStreamOptions);
    procedure ReleaseContents;
  public
    destructor Destroy; override;

    { Get the data. This starts downloading.
      Be sure to set @link(Url) and other necessary properties before calling this. }
    procedure Start;

    { URL to read or write.
      Supports all protocols, like @code(file), @code(http), @code(https),
      @code(castle-data) and other documented on
      https://castle-engine.io/manual_network.php .

      Can only change when there is no download in progress
      (@link(Status) is dsNotStarted or dsSuccess). }
    property Url: String read FUrl write SetUrl;

    { Options that configure the output stream. See TStreamOption for details. }
    property Options: TStreamOptions read FOptions write SetOptions;

    { In case of HTTP and HTTPS protocols, choose the http request method (verb).
      See https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods
      and https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol#Request_methods
      for description.

      Can only change when there is no download in progress
      (@link(Status) is not dsDownloading). }
    property HttpMethod: THttpMethod read FHttpMethod write SetHttpMethod default hmGet;

    { Event called when we finish downloading.

      Can only change when there is no download in progress
      (@link(Status) is not dsDownloading). }
    property OnFinish: TNotifyEvent read FOnFinish write SetOnFinish;

    { Whether we finished the download (and if yes, was it an error or success). }
    property Status: TDownloadStatus read FStatus;

    { Wait until status is no longer dsDownloading. }
    procedure WaitForFinish;

    { If the @link(Status) is dsError, this contains a detailed error message. }
    property ErrorMessage: String read FErrorMessage;

    { If the @link(Status) is dsSuccess, this contains the downloaded contents.
      This stream is owned by default (if ContentOwned) by this TCastleDownload instance,
      so it will become invalid when the TCastleDownload instance is freed. }
    property Contents: TStream read FContents;

    { Is the @link(Contents) owned by this @link(TCastleDownload) instance.
      Set this to @false to be able to free this TCastleDownload instance
      and still keep the stream reference.
      It is your responsibility then to keep and free the @link(Contents)
      stream whenever you want. }
    property OwnsContents: boolean read FOwnsContents;

    { How many bytes were downloaded.
      Together with @link(TotalBytes), you can use it e.g. to show a progress bar
      when downloading.
      This is always >= 0. }
    property DownloadedBytes: Int64 read FDownloadedBytes;

    { How many bytes are expected to be downloaded, in total.
      -1 if unknown.
      Depending on the server answer, this may be known fairly quickly after
      starting the download, or if may not be known at all (until we finish
      the download).
      It's guaranteed that this is known (not -1) when @link(Status) = dsSuccess,
      in all other cases always be prepared that this may be equal -1. }
    property TotalBytes: Int64 read FTotalBytes;

    { As soon as the MIME type of the downloaded contents is known, this is set.
      It is guaranteed to be set when @link(Status) is dsSuccess,
      it *may* be determined earlier (when dsDownloading). }
    property MimeType: String read FMimeType;
  end;

{$endif}

{$ifdef read_implementation}

destructor TCastleDownload.Destroy;
begin
  ReleaseContents;
  inherited;
end;

procedure TCastleDownload.SetUrl(const Value: String);
begin
  if FUrl <> Value then
  begin
    if Status = dsDownloading then
      raise Exception.Create('Cannot change URL when downloading');
    FUrl := Value;
  end;
end;

procedure TCastleDownload.SetHttpMethod(const Value: THttpMethod);
begin
  if FHttpMethod <> Value then
  begin
    if Status = dsDownloading then
      raise Exception.Create('Cannot change HTTP method when downloading');
    FHttpMethod := Value;
  end;
end;

procedure TCastleDownload.SetOnFinish(const Value: TNotifyEvent);
begin
  if not SameMethods(TMethod(FOnFinish), TMethod(Value)) then
  begin
    if Status = dsDownloading then
      raise Exception.Create('Cannot change OnFinish when downloading, you have to set it before Start to be reliable');
    FOnFinish := Value;
  end;
end;

procedure TCastleDownload.SetOptions(const Value: TStreamOptions);
begin
  if FOptions <> Value then
  begin
    if Status = dsDownloading then
      raise Exception.Create('Cannot change Options when downloading');
    FOptions := Value;
  end;
end;

procedure TCastleDownload.ReleaseContents;
begin
  if OwnsContents then
    FreeAndNil(FContents)
  else
    FContents := nil;
end;

procedure TCastleDownload.Start;
begin
  ReleaseContents;
  { Reset properties that describe the downloaded result. }
  FErrorMessage := '';
  FMimeType := '';
  FTotalBytes := -1;
  FDownloadedBytes := 0;
  FStatus := dsDownloading;

  try
    { TODO of course this is just a testing implementation,
      - that just does synchronous downloading.
      - It also ignores HttpMethod
      - EnableNetwork disables http, https here too. Should it?
    }
    FContents := Download(Url, Options, FMimeType);
    FStatus := dsSuccess;
    FTotalBytes := FContents.Size;
    FDownloadedBytes := FContents.Size;
    if Assigned(OnFinish) then
      OnFinish(Self);
  except
    on E: TObject do
    begin
      FStatus := dsError;
      FErrorMessage := Format('Error when downloading "%s": ', [URIDisplay(FUrl)]) +
        ExceptMessage(E);
    end;
  end;
end;

procedure TCastleDownload.WaitForFinish;
begin
  while Status = dsDownloading do
    Sleep(10);
end;

{$endif}
