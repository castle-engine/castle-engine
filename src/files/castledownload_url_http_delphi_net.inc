{%MainUnit castledownload.pas}
{
  Copyright 2023-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Handle http, https URLs using Delphi TNetHTTPClient.

  See https://docwiki.embarcadero.com/RADStudio/Sydney/en/Using_an_HTTP_Client .
  See https://github.com/michaliskambi/delphi-http-download for my tests.

  Compared to Indy TIdHTTP, TNetHTTPClient :

  - Seems more feature-packed:
    TNetHTTPClient can do asynchronous operation,
    doing work in thread, automatically.

  - Supports HTTPS out of the box without the need for any additional DLLs on Windows.
    On Windows, it relies on system libraries (winhttp.dll) for HTTPS (and unencrypted HTTP too).
    On Linux, it relies on libcurl (and unencrypted HTTP too).
    Overall, it works on "higher-level", whereas Indy uses sockets directly
    and has more work to implement e.g. HTTPS support.

  - See https://stackoverflow.com/questions/43153919/delphi-tidhttp-vs-tnethttpclient
    for more info about the differences between the two.

  TODO:

  - On Windows: The download using TNetHTTPClient seems to be quite slow.
    Test with large file, like https://castle-engine.io/latest.zip ,
    using e.g. asynchronous_download CGE example.

    Compare with FPC execution of asynchronous_download, using TFpHttpClient.

    The TNetHTTPClient is like 10x slower, very noticeable.

    Unknown whether this is also on Linux, due to Linux problems below.
}

{$if defined(DELPHI) and defined(CASTLE_DELPHI_NET_HTTP_CLIENT)}

{$ifdef read_implementation_uses}
System.Net.HttpClientComponent, System.Net.HttpClient,
// for sLocation used when CASTLE_HTTP_MANUAL_REDIRECTS
System.NetConsts,
{$endif}

{$ifdef read_implementation}

type
  { TUrlAsynchronousReader descendant that implements http(s) downloading. }
  TDelphiNetReader = class(TUrlAsynchronousReader)
  strict private
    C: TNetHttpClient;
    ContentsOwned: TMemoryStream;
    Response: IHTTPResponse;
    { Call C.Get or similar method of C to actually start HTTP request. }
    procedure NetClientStart(const StartUrl: String);
    procedure ReceiveData(const Sender: TObject;
      AContentLength: Int64; AReadCount: Int64; var AAbort: Boolean);
    procedure RequestCompleted(const Sender: TObject;
      const AResponse: IHTTPResponse);
    procedure RequestError(const Sender: TObject;
      const AError: string);
    procedure RequestException(const Sender: TObject;
      const AError: Exception);
    class procedure ReceiveDataAbort(const Sender: TObject;
      AContentLength: Int64; AReadCount: Int64; var AAbort: Boolean);
  public
    procedure Start; override;
    destructor Destroy; override;
    procedure Update; override;
  end;

  { Define TDelphiHttpReader used in castledownload_register.inc.
    It is an alias to a different class, depending on whether
    CASTLE_DELPHI_NET_HTTP_CLIENT was defined. }
  TDelphiHttpReader = TDelphiNetReader;

{ TNetHttpClient on Delphi/Linux fails to handle redirects in some cases,
  testcase: http://castle-engine.io/latest.zip failing with timeout.
  Handling redirects on our side fixes the issue.

  Details:

  - https://github.com/michaliskambi/delphi-http-download (no CGE code)
    allows to reproduce it too (so it's really unrelated to CGE):

    - using the command-line https://github.com/michaliskambi/delphi-http-download/blob/master/test_net_http_client.dpr

    - using the FMX (GUI) application in https://github.com/michaliskambi/delphi-http-download/tree/master/fmx

  - Tested and reproduced with both Delphi 11.3 and 12.

  - It is reproducible regardless of the `Asynchronous` property value --
    so it fails even in simpler case of synchronous operation.

  - Using `curl` from the command-line on the same system works OK,
    i.e. this works OK:

      curl -L http://castle-engine.io/latest.zip --output cge.zip

    Note that `TNetHttpClient` uses `libcurl` under the hood,
    so it should really work like `curl`.
}
{$define CASTLE_HTTP_MANUAL_REDIRECTS}

{ TDelphiNetReader --------------------------------------------------- }

procedure TDelphiNetReader.NetClientStart(const StartUrl: String);
var
  EmptyPostData: TStringList;
begin
  case HttpMethod of
    hmGet: Response := C.Get(StartUrl, ContentsOwned);
    hmPost:
      begin
        if HttpPostData = nil then
        begin
          EmptyPostData := TStringList.Create;
          try
            Response := C.Post(StartUrl, EmptyPostData, Contents)
          finally FreeAndNil(EmptyPostData) end;
        end else
          Response := C.Post(StartUrl, HttpPostData, Contents);
      end;
    hmPut:
      // TODO: We should pass Source stream to put, otherwise it is not very useful
      begin
        EmptyPostData := TStringList.Create;
        try
          Response := C.Put(StartUrl, EmptyPostData, Contents);
        finally FreeAndNil(EmptyPostData) end;
      end;
    hmDelete: Response := C.Delete(StartUrl, Contents);
    hmOptions: Response := C.Options(StartUrl, Contents);
    hmHead: Response := C.Head(StartUrl);
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('HttpMethod=?');
    {$endif}
  end;
end;

procedure TDelphiNetReader.Start;
begin
  inherited;
  if not CheckNetworkAccess then
    Exit;

  { TODO: honor
    ForceSynchronous: Boolean; // or nothing more to do?
    HttpHeadersKeys, HttpHeadersValues: TStrings;
  }

  { Using separate ContentsOwned is a hack, to let the client
    leak in some cases.
    See TDelphiNetReader.Destroy comments. }
  ContentsOwned := TMemoryStream.Create;

  Contents := TMemoryStream.Create;

  C := TNetHttpClient.Create(nil);
  C.Asynchronous := not ForceSynchronous;
  C.OnReceiveData := ReceiveData;
  C.OnRequestCompleted := RequestCompleted;
  C.OnRequestError := RequestError;
  C.OnRequestException := RequestException;
  C.SynchronizeEvents := true;

  {$ifdef CASTLE_HTTP_MANUAL_REDIRECTS}
  C.HandleRedirects := false;
  {$endif}

  {$ifdef LINUX}
  { Disable timeouts, otherwise downloading long files
    (like http://castle-engine.io/latest.zip) is always broken at timeout,
    even when things actually go OK (e.g. after downloading over 400 MB).
    This is asynchronous downloader anyway -- outside code can always
    break it, so timeouts hardcoded here should not be necessary. }
  C.ConnectionTimeout := -1;
  C.ResponseTimeout := -1;
  {$endif}

  { TODO: Update FinalUrl during HTTP redirects even when
    CASTLE_HTTP_MANUAL_REDIRECTS not defined.
    For now only CASTLE_HTTP_MANUAL_REDIRECTS will update it. }
  FinalUrl := Url;

  NetClientStart(Url);
end;

destructor TDelphiNetReader.Destroy;

  { TNetHttpClient cannot be freed before it finished the work
    or it will crash with access violation.

    We try to cancel it, and this seems to usually work,
    but in case it fails:
    we let the C and ContentsOwned to just leak,
    this still causes some crashes but at least we can ignore them
    (they seem to happen then only in some non-main thread).

    We also pass ReceiveDataAbort to at least stop actual transfer
    as soon as feasible. }
  procedure LeakNetHttpClient;
  begin
    C.OnReceiveData := TDelphiNetReader.ReceiveDataAbort;
    C.OnRequestCompleted := nil;
    C.OnRequestError := nil;
    C.OnRequestException := nil;

    WritelnWarning('Leaking TNetHttpClient reference. Freeing it before the request has completed leads to crashes');

    ContentsOwned := nil;
    C := nil;
    Response := nil;
  end;

const
  WaitForCancelStep = 10; //< in miliseconds
  WaitForCancelMax = 1000; //< in miliseconds; so we wait max 1 second
var
  Waited: Cardinal;
begin
  if (C <> nil) and
    (Status = dsDownloading) and
    (Response <> nil) and
    (not Response.AsyncResult.IsCompleted) then
  begin
    WritelnLog('TNetHttpClient', 'Aborting download in progress of "%s"', [UriDisplay(Url)]);

    { We have to use SynchronizeEvents:=false, otherwise aborting
      TNetHttpClient would cause crashes as it would put in Synchronize()
      calls to our callbacks, so they will run from next
      Application.ProcessMessages, but by then we are already freed.

      So we also change callbacks to things that are harmless to run
      from arbitrary thread.
    }
    C.OnReceiveData := TDelphiNetReader.ReceiveDataAbort;
    C.OnRequestCompleted := nil;
    C.OnRequestError := nil;
    C.OnRequestException := nil;
    C.SynchronizeEvents := false;

    { TNetHttpClient causes exceptions if freed without properly closing the connection.
      Testcase: asynchronous_download CGE example, press "Abort Downloads"
      after starting the downloads.
      Luckily, usually it can be cancelled immediately. }
    if Response.AsyncResult.Cancel then
    begin
      Assert(Response.AsyncResult.IsCancelled);
      Waited := 0;
      while
        (not Response.AsyncResult.IsCompleted) and
        (Waited < WaitForCancelMax) do
      begin
        Sleep(WaitForCancelStep);
        Waited := Waited + WaitForCancelStep;
      end;
      // warn when Waited <> 0
      if Waited <> 0 then
      begin
        if Response.AsyncResult.IsCancelled then
          WritelnWarning('Waited to cancel for %f seconds. At least we cancelled OK, so we can cleanup TNetHttpClient OK. But cancelling this way causes delays in execution of the main loop.', [Waited / 1000])
        else
        begin
          WritelnWarning('Waited to cancel for %f seconds. And we failed to cancel, to we will just leak TNetHttpClient now. Report a bug with reproduction.', [Waited / 1000]);
          LeakNetHttpClient;
        end;
      end;
    end else
    begin
      WritelnWarning('TNetHttpClient cannot be cancelled; leaking it');
      LeakNetHttpClient;

      // TODO: Calling LeakNetHttpClient, to avoid crashes.
      // Find a case when it can be tested it in practice.
      // When does Cancel return false? Does it imply we can free TNetHttpClient?
    end;
  end;

  FreeAndNil(C);
  Response := nil; // release reference count from interface
  FreeAndNil(ContentsOwned);

  inherited;
end;

procedure TDelphiNetReader.ReceiveData(const Sender: TObject;
  AContentLength, AReadCount: Int64; var AAbort: Boolean);
begin
  DownloadedBytes := AReadCount;
  TotalBytes := AContentLength;
end;

class procedure TDelphiNetReader.ReceiveDataAbort(const Sender: TObject;
  AContentLength, AReadCount: Int64; var AAbort: Boolean);
begin
  { Note: This cannot access any main thread stuff, it may be run
    in arbitrary thread by TNetHttpClient. }
  AAbort := true;
end;

procedure TDelphiNetReader.RequestCompleted(const Sender: TObject;
  const AResponse: IHTTPResponse);

  {$ifdef CASTLE_HTTP_MANUAL_REDIRECTS}
  function ArrayContainsInt(const X: Integer; const A: array of Integer): Boolean;
  var
    I: Integer;
  begin
    for I := 0 to High(A) do
      if A[I] = X then
        Exit(true);
    Result := false;
  end;

  { Detect and handle http redirect, doing new NetClientStart if needed.
    Returns @true if redirect was handled, @false if not. }
  function HandleRedirect: Boolean;
  var
    RedirectToGet: Boolean;
    RedirectUrl: String;
  begin
    Result := false;

    if ArrayContainsInt(AResponse.StatusCode, [300, 301, 302, 303, 307, 308]) then
    begin
      RedirectUrl := Trim(AResponse.GetHeaderValue(sLocation));
      if RedirectUrl <> '' then
      begin
        { TODO: Preserve fragment (#xxx, it seems https://tools.ietf.org/html/rfc7231#section-7.1.2 )
          from Url to FinalUrl. It practically doesn't matter for just downloading. }
        FinalUrl := RedirectUrl;
        WritelnLog('Redirecting to "%s"', [UriDisplay(FinalUrl)]);

        { Some redirects should change method to GET.
          Logic matches default value of CHTTPDefRedirectsWithGET in Delphi. }
        RedirectToGet := false;
        case HttpMethod of
          hmPost: RedirectToGet := ArrayContainsInt(AResponse.StatusCode, [301, 302, 303]);
          hmPut: RedirectToGet := ArrayContainsInt(AResponse.StatusCode, [303]);
          hmDelete: RedirectToGet := ArrayContainsInt(Response.StatusCode, [303]);
        end;
        if RedirectToGet and (HttpMethod <> hmGet) then
        begin
          WritelnLog('Redirect changes method to GET');
          HttpMethod := hmGet;
        end;

        // reset ContentsOwned
        ContentsOwned.Size := 0;
        ContentsOwned.Position := 0;

        NetClientStart(FinalUrl);

        Result := true;
      end;
    end;
  end;
  {$endif}

begin
  {$ifdef CASTLE_HTTP_MANUAL_REDIRECTS}
  if HandleRedirect then
    Exit;
  {$endif}

  WritelnLog('Completed request for "%s" with status %d', [
    UriDisplay(Url),
    AResponse.StatusCode
  ]);

  HttpResponseCode := AResponse.StatusCode;
  MimeType := ContentTypeToMimeType(AResponse.MimeType);

  { TODO: Set HttpResponseHeaders from AResponse.Headers
  HttpResponseHeaders := TStringList.Create;
  HttpResponseHeaders.NameValueSeparator := ':';
  // for each pair in AResponse.Headers, add it to HttpResponseHeaders
  }

  // finish the Contents stream
  Contents.CopyFrom(ContentsOwned);
  DownloadedBytes := TMemoryStream(Contents).Size;
  TotalBytes := TMemoryStream(Contents).Size;
  Contents.Position := 0; // rewind

  if HttpResponseCode <> 200 then
  begin
    Status := dsError;
    ErrorMessage := Format('HTTP request completed with status %d (not 200, so not success)', [
      HttpResponseCode
    ]);
  end else
  begin
    Status := dsSuccess;
  end;
end;

procedure TDelphiNetReader.RequestError(const Sender: TObject;
  const AError: string);
begin
  Status := dsError;
  ErrorMessage := AError;
end;

procedure TDelphiNetReader.RequestException(const Sender: TObject;
  const AError: Exception);
begin
  Status := dsError;
  ErrorMessage := ExceptMessage(AError);
end;

procedure TDelphiNetReader.Update;
begin
  inherited;

  { No need to do anything, our event callbacks are called already.

    Even in console application, that doesn't explicitly
    do Application.ProcessMessages.
    On both Windows and Linux.
    See https://github.com/michaliskambi/delphi-http-download test.

    TODO: How are the callbacks called, from where,
    since we don't necessarily execute the Delphi VCL / FMX event loop?
    E.g. with TCastleWindow (WinAPI backend) we execute own loop,
    and we don't call VCL / FMX Application.ProcessMessages.
    But the callbacks still happen, even in console application.
    It seems that at least for Windows, with TCastleWindow,
    Delphi somehow hooked to the Windows event loop to make them happen?
    I haven't found how are they done on Linux.

    Note: The work of TNetHttpClient is done in a thread, so it makes sense
    that it *does processing (downloads new bytes)* without
    Application.ProcessMessages.
    The above question is how does Delphi Synchronize work in
    Linux console application
    that doesn't call VCL / FMX Application.ProcessMessages?
  }
end;

{$endif read_implementation}

{$endif defined(DELPHI) and defined(CASTLE_DELPHI_NET_HTTP_CLIENT)}
