{%MainUnit castledownload.pas}
{
  Copyright 2013-2022 Michalis Kamburelis.

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
  TCastleDownload = class;

  TDownloadFinishedEvent = procedure (const Sender: TCastleDownload; var FreeSender: Boolean) of object;

  { Download an URL (possibly making an HTTP(S) request) asynchronously,
    without blocking the application.
    You can register a callback @link(OnFinish) or watch
    when the @link(Status) property changes from dsDownloading to dsError or dsSuccess
    to detect when this finished downloading.

    @italic(Features:)

    @unorderedList(
      @item(The download can be observed (looking at @link(Status), @link(DownloadedBytes), @link(TotalBytes)).)

      @item(When the downlad is finished, look at @link(Contents) (if success) or @link(ErrorMessage) (if error, that is @link(Status) = @link(dsError)).)

      @item(We can read MIME type from server (can be used throughout our engine to designate file type).)

      @item(For downloading (making a request) using HTTP, this is a handful of additional features, to support downloading from modern web servers and using REST APIs:

        @unorderedList(
          @item(First of all, we support both HTTP and HTTPS on all platforms and compilers, through various backend (FpHttpClient, Indy, Delphi Net Client).)

          @item(You can provide arbitrary HTTP headers using @link(HttpHeader). For example, various REST APIs require you to provide some key to authenticate through a custom HTTP header.)

          @item(You can specify any @link(HttpMethod), like GET, POST, PUT and more. Default is naturally GET which is the standard method to just download the data.)

          @item(For HTTP POST method, you can provide form data as @link(HttpPostData).)

          @item(You can provide input data for any request using @link(HttpRequestBody). The most common use case is to provide a body for PUT requests, but it can be used for any request, e.g. to provide POST body overriding the @link(HttpPostData).)

          @item(Once the download finishes, you can read @link(HttpResponseCode) and @link(HttpResponseHeaders).)

          @item(By default, we follow HTTP/HTTPS redirects.)
        )
      )
    )

    @italic(See examples:)

    @unorderedList(
      @item(@url(https://github.com/castle-engine/castle-engine/blob/master/examples/network/asynchronous_download/ asynchronous_download) to download resources asynchronously, downloading a few URLs and at the same time displaying an animation and progress bar.)

      @item(@url(https://github.com/castle-engine/castle-engine/blob/master/examples/network/remote_logging/ remote_logging) to send logs to a remote server, using HTTP POST.)

      @item(@url(https://github.com/castle-engine/castle-engine/tree/master/examples/network/put_data/ put_data) to send HTTP PUT request.)

      @item(And other examples in @url(https://github.com/castle-engine/castle-engine/tree/master/examples/network examples/network) subdirectory.)
    )

    @italic(Usage:)

    The download starts when you call @link(Start).
    Be sure to configure the properties, including @link(OnFinish),
    before calling @link(Start), because in case of some protocols @link(Start) may immediately
    read everything and finish.
    When the download ends, the @link(OnFinish) is called and @link(Status) changes.

    You can always just free an instance of this class, this will
    break the download immediately, if it's still in-progress.

    The download continues while your application is running,
    because we use @link(TCastleApplicationProperties.OnUpdate ApplicationProperties.OnUpdate)
    mechanism. If your application uses TCastleWindow or TCastleControl,
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
    FFinalUrl: String;
    FHttpMethod: THttpMethod;
    FOnFinish: TDownloadFinishedEvent;
    FStatus: TDownloadStatus;
    FErrorMessage: String;
    FContents: TStream;
    FOwnsContents: Boolean;
    FOptions: TStreamOptions;
    FTotalBytes, FDownloadedBytes: Int64;
    FMimeType: String;
    Reader: TUrlAsynchronousReader; //< non-nil when asynchronous reading is in-progress
    UpdateInitialized: Boolean;
    FHttpPostData: TStringList;
    FHttpHeadersKeys, FHttpHeadersValues: TStringList;
    FHttpRequestBody: TMemoryStream;
    FHttpResponseCode: Integer;
    FHttpResponseHeaders: TStrings;
    procedure SetUrl(const Value: String);
    procedure SetHttpMethod(const Value: THttpMethod);
    procedure SetOnFinish(const Value: TDownloadFinishedEvent);
    procedure SetOptions(const Value: TStreamOptions);
    procedure ReleaseContents;
    { Set Contents from given UnderlyingStream.
      The passed UnderlyingStream becomes owned by something in this method
      (like TCastleDownload instance or FContents or it is freed).
      Consider the reference to UnderlyingStream invalid after this call. }
    procedure SetContentsApplyOptions(UnderlyingStream: TStream);
    procedure Update(Sender: TObject);
  private
    { Set by Download(). }
    ForceSynchronous: Boolean;
  protected
    { Note that this may destroy our own instance.
      So never access our own fields / virtual methods after calling this. }
    procedure DoFinish; virtual;
  public
    constructor Create(AOwner: TComponent); override;
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

    { When the download has finished (with success or error),
      this is updated to "final" URL, after resolving HTTP/HTTPS redirects and
      castle-data:/ protocol. }
    property FinalUrl: String read FFinalUrl;

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
    property OnFinish: TDownloadFinishedEvent read FOnFinish write SetOnFinish;

    { Whether we finished the download (and if yes, was it an error or success). }
    property Status: TDownloadStatus read FStatus;

    { Wait until status is no longer dsDownloading. }
    procedure WaitForFinish;

    { If the @link(Status) is dsError, this contains a detailed error message. }
    property ErrorMessage: String read FErrorMessage;

    { The downloaded contents.

      If the @link(Status) is dsSuccess, this is always set (never @nil).
      In case of some protocols and some failures, the contents may be available
      (not @nil) even when error occurred, that is when Status is dsError.
      E.g. when HTTP server returns an error,
      like 404, it also still sends the response contents.

      This stream is owned by default (if @link(OwnsContents)) by this TCastleDownload instance,
      so it will become invalid when the TCastleDownload instance is freed. }
    property Contents: TStream read FContents;

    { Is the @link(Contents) owned by this @link(TCastleDownload) instance.
      Set this to @false to be able to free this TCastleDownload instance
      and still keep the stream reference.
      It is your responsibility then to keep and free the @link(Contents)
      stream whenever you want. }
    property OwnsContents: boolean read FOwnsContents write FOwnsContents default true;

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

    { Form data to send when HttpMethod = hmPost.
      Initially empty.
      Use the TStrings name/value mechanism to set the form, like

      @longCode(#
        // Advised:
        Download.HttpPostData.Values['my-form-key'] := 'my-form-value';
        // This also works, it is equivalent to above if key didn't already exist
        Download.HttpPostData.Append('my-form-key-2=my-form-value-2');
      #)
    }
    function HttpPostData: TStrings;
    function PostData: TStrings; deprecated 'use HttpPostData';

    { Request body to send along with the request.
      Most useful with PUT (to send data to save on the server side).
      May also be useful with POST (only when the HttpPostData key-value is not
      flexible enough) or other requests.

      You can use various @code(TStream) methods to set the stream,
      like @code(TStream.CopyFrom). The CastleClassUtils also has various
      helpers for stream, e.g. to write a String (AnsiString, UTF-8 encoding)
      to a stream use @code(WriteStr(Download.HttpRequestBody, 'My content')).

      Example usage:

      @includeCode(../../examples/network/put_data/put_data.dpr)

      Note: For some requests you may want to add HTTP headers like Content-Type etc.
      Do this using @link(HttpHeader).

      In case of POST requests, when this stream has non-empty contents
      (stream size > 0), it overrules the HttpPostData. That is,
      HttpPostData is completely ignored in this case.
      Reason: Internally, HttpPostData is just an alternative way to set
      HttpRequestBody.

      TODO: We may change this behavior in the future, to make it such that
      using HttpPostData will automatically set also HttpRequestBody contents
      to represent the given POST data, immediately.
      This way the "overriding" will be "whatever is set last, wins".
      For now, @italic(never use both HttpPostData and HttpRequestBody
      at the same time.)

      TODO: For now this is supported only when using FPC (not Delphi),
      on desktop or Android platforms (not iOS yet).
      It should be trivial to
      make it work in other cases too, just let us know
      ( https://castle-engine.io/talk.php ).
    }
    function HttpRequestBody: TMemoryStream;

    { Add additional HTTP headers, e.g. User-Agent. }
    procedure HttpHeader(const AKey, AValue: String);
    procedure AddHeader(const AKey, AValue: String); deprecated 'use HttpHeader';

    { When Status is dsSuccess or dsError, and request was using HTTP or HTTPS,
      this contains the HTTP status code.
      It is always 200 on success. }
    property HttpResponseCode: Integer read FHttpResponseCode;

    { When Status is dsSuccess or dsError, and request was using HTTP or HTTPS,
      this contains the HTTP response headers.
      The NameValueSeparator is set to ':' which means you can use TStrings
      name/value mechanism to read the data, like this:

      @longCode(#
        LastModified := Download.HttpResponseHeaders.Values['Last-Modified'];
      #)

      Note that it is @nil in case of non-HTTP/HTTPS requests.
    }
    property HttpResponseHeaders: TStrings read FHttpResponseHeaders;
  end;

{$endif}

{$ifdef read_implementation}

constructor TCastleDownload.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwnsContents := true;
end;

destructor TCastleDownload.Destroy;
begin
  ReleaseContents;
  if Reader <> nil then
  begin
    FreeAndNil(Reader.Contents); // Reader doesn't own Contents, so we need to take care of it
    FreeAndNil(Reader);
  end;
  if UpdateInitialized then
  begin
    ApplicationProperties.OnUpdate.Remove({$ifdef FPC}@{$endif}Update);
    UpdateInitialized := false;
  end;
  FreeAndNil(FHttpPostData);
  FreeAndNil(FHttpResponseHeaders);
  FreeAndNil(FHttpHeadersKeys);
  FreeAndNil(FHttpHeadersValues);
  FreeAndNil(FHttpRequestBody);
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

procedure TCastleDownload.SetOnFinish(const Value: TDownloadFinishedEvent);
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
var
  FreeSender: Boolean;
begin
  if Assigned(OnFinish) then
  begin
    FreeSender := false;
    OnFinish(Self, FreeSender);
    if FreeSender then
      Self.Destroy;
  end;
end;

procedure TCastleDownload.Start;

  { Reset properties that describe the downloaded result. }
  procedure ClearPreviousDownload;
  begin
    ReleaseContents;
    if Reader <> nil then
    begin
      FreeAndNil(Reader.Contents); // Reader doesn't own Contents, so we need to take care of it
      FreeAndNil(Reader);
    end;
    FErrorMessage := '';
    FMimeType := '';
    FTotalBytes := -1;
    FDownloadedBytes := 0;
    FStatus := dsDownloading;
  end;

  { Read synchronously.
    This calls DoFinish at end, and may free us. }
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
            testcase: castle-model-viewer escape_universe/trunk/data/entities/meteorite_4/animations.x3dv }
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
        FErrorMessage := Format('Error when downloading "%s": ', [UriDisplay(Url)]) +
          ExceptMessage(E);
        {$if defined(VER3_2) and defined(DARWIN) and not defined(CASTLE_IOS)}
        if (E is ESocketError) and (URIProtocol(RealUrl) = 'https') then
        begin
          FErrorMessage += NL +
            'Warning: Https downloading with FPC 3.2.0 on macOS is broken for many hosts.' + NL +
            'Upgrade to FPC 3.3.1.' + NL +
            'See https://bugs.freepascal.org/view.php?id=37747 , https://github.com/castle-engine/castle-engine/issues/184 .';
        end;
        {$endif}
      end;
    end;
    DoFinish;
  end;

var
  P: String;
  RegisteredProtocol: TRegisteredProtocol;
begin
  ClearPreviousDownload;

  P := URIProtocol(Url);

  if not UpdateInitialized then
  begin
    ApplicationProperties.OnUpdate.Add({$ifdef FPC}@{$endif}Update);
    UpdateInitialized := true;
  end;

  { Handle early castle-data:, protocol to access application data,
    https://castle-engine.io/data . }
  if P = 'castle-data' then
  begin
    FFinalUrl := ResolveCastleDataURL(Url);
    P := URIProtocol(FFinalUrl);
  end else
    FFinalUrl := Url;

  if LogAllLoading then
    WritelnLog('Loading', 'Loading "%s"', [UriDisplay(FFinalUrl)]);

  RegisteredProtocol := RegisteredProtocols.Find(P);
  if RegisteredProtocol <> nil then
  begin
    if RegisteredProtocol.AsynchronousReader <> nil then
    begin
      Reader := RegisteredProtocol.AsynchronousReader.Create;
      Reader.Url := FFinalUrl;
      Reader.ForceSynchronous := ForceSynchronous;
      Reader.HttpMethod := HttpMethod;
      { Assign from field FHttpPostData, not HttpPostData function,
        this way it may remain nil and not waste memory
        in case user didn't touch HttpPostData. }
      Reader.HttpPostData := FHttpPostData;
      Reader.HttpRequestBody := FHttpRequestBody;
      Reader.HttpHeadersKeys := FHttpHeadersKeys;
      Reader.HttpHeadersValues := FHttpHeadersValues;
      Reader.Start;
    end else
    if Assigned(RegisteredProtocol.ReadEvent) then
    begin
      SynchronousRead(RegisteredProtocol.ReadEvent, FFinalUrl);
    end else
    begin
      FStatus := dsError;
      raise EDownloadError.CreateFmt('Cannot read URLs with protocol "%s"', [P]);
    end;
  end else
  begin
    FStatus := dsError;
    raise EDownloadError.CreateFmt('Downloading from protocol "%s" is not supported', [P]);
  end;
end;

procedure TCastleDownload.WaitForFinish;
begin
  while Status = dsDownloading do
  begin
    Update(nil);
    Sleep(10);
  end;
end;

procedure TCastleDownload.Update(Sender: TObject);
begin
  if Status = dsDownloading then
  begin
    Assert(Reader <> nil);

    Reader.Update;

    Assert(Reader.Status <> dsNotStarted);
    FStatus := Reader.Status;
    FDownloadedBytes := Reader.DownloadedBytes;
    FTotalBytes := Reader.TotalBytes;
    FMimeType := Reader.MimeType;

    if FStatus in [dsError, dsSuccess] then
    begin
      // copy stuff from Reader
      if FStatus = dsSuccess then
        Assert(Reader.Contents <> nil); // in this case, Reader.Contents is obligatory
      if Reader.Contents <> nil then // in this case, Reader.Contents is optional
      begin
        SetContentsApplyOptions(Reader.Contents);
        Reader.Contents := nil; // not valid anymore, SetContentsApplyOptions took ownership
      end;
      FHttpResponseCode := Reader.HttpResponseCode;
      FHttpResponseHeaders := Reader.HttpResponseHeaders;
      FFinalUrl := Reader.FinalUrl;
      Reader.HttpResponseHeaders := nil; // do not free HttpResponseHeaders in Reader destructor
      if FStatus = dsError then
        FErrorMessage := Reader.ErrorMessage;

      FreeAndNil(Reader);
      DoFinish;
      Exit; // DoFinish possibly freed us, so Exit and do not access any own fields
    end;

    // Reader continues to exist if and only if still dsDownloading
    Assert((Reader <> nil) = (FStatus = dsDownloading));
  end;
end;

procedure CopyStreamWithoutRewinding(const Dest: TMemoryStream; const Source: TStream);
begin
  { Do not call Dest.LoadFromStream(Source), as it does "Source.Position := 0"
    (at least in FPC) which should not be required, and actually fails with
    some stream implementations.

    Testcase: open with castle-model-viewer demo-models/x3d/data_uri.x3dv .
    The TUrlData.Read returns TBase64DecodingStream which doesn't allow assigning Position.
  }
  if Source.Position <> 0 then
    Source.Position := 0; // set Source.Position, but only if it's not already 0
    // raise EDownloadError.CreateFmt('Underlying stream %s position should be 0, but is %d', [
    //   Source.ClassName,
    //   Source.Position
    // ]);
  Dest.SetSize(Source.Size);
  if Source.Size <> 0 then
    Source.ReadBuffer(Dest.Memory^, Source.Size);
end;

procedure TCastleDownload.SetContentsApplyOptions(UnderlyingStream: TStream);

  { Load Stream to TMemoryStream.
    Sets given Stream to @nil (it is freed by this function). }
  function CreateMemoryStream(var Stream: TStream): TMemoryStream; overload;
  begin
    Result := TMemoryStream.Create;
    try
      CopyStreamWithoutRewinding(Result, Stream);
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
        { This case happens e.g. when reading .x3d.gz, that is gzipped
          (must pass through TGZFileStream)
          but also needs OnLoadForceMemoryStream. }
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
  if (soForceMemoryStream in Options) and
     // if UnderlyingStream is already TMemoryStream, like with data URI, then we're OK
     (not (UnderlyingStream is TMemoryStream)) then
    FContents := CreateMemoryStream(UnderlyingStream)
  else
    FContents := UnderlyingStream;
end;

function TCastleDownload.PostData: TStrings;
begin
  Result := HttpPostData;
end;

procedure TCastleDownload.AddHeader(const AKey, AValue: String);
begin
  HttpHeader(AKey, AValue);
end;

function TCastleDownload.HttpPostData: TStrings;
begin
  // FHttpPostData is created on-demand, because it is often not needed
  if FHttpPostData = nil then
    FHttpPostData := TStringList.Create;
  Result := FHttpPostData;
end;

function TCastleDownload.HttpRequestBody: TMemoryStream;
begin
  // FHttpRequestBody is created on-demand, because it is often not needed
  if FHttpRequestBody = nil then
    FHttpRequestBody := TMemoryStream.Create;
  Result := FHttpRequestBody;
end;

procedure TCastleDownload.HttpHeader(const AKey, AValue: String);
begin
  // 2 string lists are created on-demand, because they are often not needed
  if FHttpHeadersKeys = nil then
    FHttpHeadersKeys := TStringList.Create;
  if FHttpHeadersValues = nil then
    FHttpHeadersValues := TStringList.Create;
  FHttpHeadersKeys.Append(AKey);
  FHttpHeadersValues.Append(AValue);
  Assert(FHttpHeadersKeys.Count = FHttpHeadersValues.Count);
end;

{$endif}
