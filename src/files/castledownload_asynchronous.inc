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
  { See @link(TCastleDownload.HttpMethod). }
  THttpMethod = (hmGet, hmPost, hmPut, hmDelete, hmOptions, hmHead);

  TCastleDownload = class;

  TDownloadEvent = procedure (const Sender: TCastleDownload) of object;

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
    FOnFinish: TDownloadEvent;
    FStatus: TDownloadStatus;
    FErrorMessage: String;
    FContents: TStream;
    FOwnsContents: Boolean;
    FOptions: TStreamOptions;
    FTotalBytes, FDownloadedBytes: Int64;
    FMimeType: String;
    Reader: TUrlAsynchronousReader; //< non-nil when asynchronous reading is in-progress
    procedure SetUrl(const Value: String);
    procedure SetHttpMethod(const Value: THttpMethod);
    procedure SetOnFinish(const Value: TDownloadEvent);
    procedure SetOptions(const Value: TStreamOptions);
    procedure ReleaseContents;
    { Set Contents from given Stream.
      The passed UnderlyingStream becomes owned by something in this method
      (like TCastleDownload instance or FContents or it is freed).
      Consider the reference to UnderlyingStream invalid after this call. }
    procedure SetContentsApplyOptions(UnderlyingStream: TStream);
  protected
    procedure DoFinish; virtual;
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
    property OnFinish: TDownloadEvent read FOnFinish write SetOnFinish;

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
    property OwnsContents: boolean read FOwnsContents write FOwnsContents;

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
  if Reader <> nil then
  begin
    FreeAndNil(Reader.Stream); // Reader doesn't own Stream, so we need to take care of it
    FreeAndNil(Reader);
  end;
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

procedure TCastleDownload.SetOnFinish(const Value: TDownloadEvent);
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

procedure TCastleDownload.DoFinish;
begin
  if Assigned(OnFinish) then
    OnFinish(Self);
end;

procedure TCastleDownload.Start;

  { Reset properties that describe the downloaded result. }
  procedure ClearPreviousDownload;
  begin
    ReleaseContents;
    if Reader <> nil then
    begin
      FreeAndNil(Reader.Stream); // Reader doesn't own Stream, so we need to take care of it
      FreeAndNil(Reader);
    end;
    FErrorMessage := '';
    FMimeType := '';
    FTotalBytes := -1;
    FDownloadedBytes := 0;
    FStatus := dsDownloading;
  end;

  procedure SynchronousRead(const ReadEvent: TUrlReadEvent; const RealUrl: String);
  var
    Size: Int64;
  begin
    try
      SetContentsApplyOptions(ReadEvent(RealUrl, FMimeType));
      FStatus := dsSuccess;

      // determine FTotalBytes, FDownloadedBytes from stream Size
      try
        Size := FContents.Size;
        FTotalBytes := Size;
        FDownloadedBytes := Size;
      except
        on E: TObject do
        begin
          { Gracefully handle exceptions from Size.
            E.g. TGZFileStream raises EZlibError: Compression stream seek error,
            testcase: view3dscene escape_universe/trunk/data/entities/meteorite_4/animations.x3dv }
          FTotalBytes := -1;
          FDownloadedBytes := 0;
          // WritelnLog('Cannot determine Size of %s (the DownloadedBytes/TotalBytes will be 0/-1): %s', [
          //   FContents.ClassName,
          //   ExceptMessage(E)
          // ]);
        end;
      end;

    except
      on E: TObject do
      begin
        FStatus := dsError;
        FErrorMessage := Format('Error when downloading "%s": ', [URIDisplay(Url)]) +
          ExceptMessage(E);
      end;
    end;
    DoFinish;
  end;

var
  P, RealUrl: String;
  RegisteredProtocol: TRegisteredProtocol;
begin
  ClearPreviousDownload;

  P := URIProtocol(Url);

  { Handle early castle-data:, protocol to access application data,
    https://castle-engine.io/manual_data_directory.php . }
  if P = 'castle-data' then
  begin
    RealUrl := ResolveCastleDataURL(Url);
    P := URIProtocol(RealUrl);
  end else
    RealUrl := Url;

  if LogAllLoading then
    WritelnLog('Loading', 'Loading "%s"', [URIDisplay(RealUrl)]);

  RegisteredProtocol := RegisteredProtocols.Find(P);
  if RegisteredProtocol <> nil then
  begin
    if RegisteredProtocol.AsynchronousReader <> nil then
    begin
      Reader := RegisteredProtocol.AsynchronousReader.Create(RealUrl);
    end else
    if Assigned(RegisteredProtocol.ReadEvent) then
    begin
      SynchronousRead(RegisteredProtocol.ReadEvent, RealUrl);
    end else
      raise EDownloadError.CreateFmt('Cannot read URLs with protocol "%s"', [P]);
  end else
    raise EDownloadError.CreateFmt('Downloading from protocol "%s" is not supported', [P]);
end;

procedure TCastleDownload.WaitForFinish;
begin
  if (Status = dsDownloading) and (Reader = nil) then
    raise EInternalError.Create('Status is dsDownloading but no asynchronous Reader initialized');

  while Status = dsDownloading do
  begin
    Assert(Reader <> nil);

    Reader.Update;

    Assert(Reader.Status <> dsNotStarted);
    FStatus := Reader.Status;
    FDownloadedBytes := Reader.DownloadedBytes;
    FTotalBytes := Reader.TotalBytes;

    case FStatus of
      dsError:
        begin
          FErrorMessage := Reader.ErrorMessage;
          FreeAndNil(Reader.Stream);
          FreeAndNil(Reader);
          DoFinish;
        end;
      dsSuccess:
        begin
          SetContentsApplyOptions(Reader.Stream);
          Reader.Stream := nil; // not valid anymore, SetContentsApplyOptions took ownership
          FreeAndNil(Reader);
          DoFinish;
        end;
    end;

    // Reader continues to exist if and only if still dsDownloading
    Assert((Reader <> nil) = (FStatus = dsDownloading));

    Sleep(10);
  end;
end;

procedure TCastleDownload.SetContentsApplyOptions(UnderlyingStream: TStream);

  { Load Stream to TMemoryStream.
    Sets given Stream to @nil (it is freed by this function). }
  function CreateMemoryStream(var Stream: TStream): TMemoryStream; overload;
  begin
    Result := TMemoryStream.Create;
    try
      Result.LoadFromStream(Stream);
      FreeAndNil(Stream);
      Result.Position := 0; { rewind for easy reading }
    except
      FreeAndNil(Result); raise;
    end;
  end;

  { Decompress gzipped FileName.
    When ForceMemoryStream, always returns TMemoryStream.
    Sets given Stream to @nil (it is owned by us now, possibly it is freed by this function). }
  function ReadGzipped(var Stream: TStream; const ForceMemoryStream: boolean): TStream;
  var
    NewResult: TMemoryStream;
  begin
    Result := TGZFileStream.Create(Stream, false);
    try
      Stream := nil; // Stream is owned by Result now

      if ForceMemoryStream then
      begin
        { TODO: our engine never uses both soGzip and soForceMemoryStream
          for now, so below code path is untested. }
        NewResult := TMemoryStream.Create;
        ReadGrowingStream(Result, NewResult, true);
        FreeAndNil(Result);
        Result := NewResult;
      end;
    except
      FreeAndNil(Result); raise;
    end;
  end;

begin
  // unpack gzip if requested
  if soGzip in Options then
    FContents := ReadGzipped(UnderlyingStream, soForceMemoryStream in Options)
  else
  if soForceMemoryStream in Options then
    FContents := CreateMemoryStream(UnderlyingStream)
  else
    FContents := UnderlyingStream;
end;

{$endif}
