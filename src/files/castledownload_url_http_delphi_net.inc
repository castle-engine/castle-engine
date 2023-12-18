{%MainUnit castledownload.pas}
{
  Copyright 2023-2023 Michalis Kamburelis.

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
    It relies on Windows system libraries (winhttp.dll) for HTTPS.
  - See https://stackoverflow.com/questions/43153919/delphi-tidhttp-vs-tnethttpclient
    for more info about the differences between the two.

  TODO:

  - The download using TNetHTTPClient seems to be quite slow.
    Test with large file, like https://castle-engine.io/latest.zip ,
    using e.g. asynchronous_download CGE example.

    Compare with FPC execution of asynchronous_download, using TFpHttpClient.

    The TNetHTTPClient is like 10x slower, very noticeable.

  - Downloading on Linux works... unless you test with larger file like
    https://castle-engine.io/latest.zip . Then it just breaks with timeout.

  - There are some other TODOs in this file.

    Most importantly, the TNetHTTPClient behaves very bad when trying to free
    it before it finished the job, and the only solution seems to be to
    "let it leak".
}

{$if defined(DELPHI) and defined(CASTLE_DELPHI_NET_HTTP_CLIENT)}

{$ifdef read_implementation_uses}
System.Net.HttpClientComponent, System.Net.HttpClient,
{$endif}

{$ifdef read_implementation}

type
  { TUrlAsynchronousReader descendant that implements http(s) downloading. }
  TDelphiNetReader = class(TUrlAsynchronousReader)
  strict private
    C: TNetHttpClient;
    ContentsOwned: TMemoryStream;
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

{ TDelphiNetReader --------------------------------------------------- }

procedure TDelphiNetReader.Start;
begin
  inherited;

  { TODO: honor
    ForceSynchronous: Boolean;
    HttpMethod: THttpMethod;
    HttpPostData, HttpHeadersKeys, HttpHeadersValues: TStrings;
  }

  { Using separate ContentsOwned is a hack, to let the client
    leak in some cases.
    See TDelphiNetReader.Destroy comments. }
  ContentsOwned := TMemoryStream.Create;

  Contents := TMemoryStream.Create;

  C := TNetHttpClient.Create(nil);
  C.Asynchronous := true;
  C.OnReceiveData := ReceiveData;
  C.OnRequestCompleted := RequestCompleted;
  C.OnRequestError := RequestError;
  C.OnRequestException := RequestException;
  C.SynchronizeEvents := true;
  C.Get(Url, ContentsOwned);

  // TODO: Change this value during HTTP redirects.
  // The TNetHttpClient already supports redirects, according to docs.
  FinalUrl := Url;
end;

destructor TDelphiNetReader.Destroy;
begin
  if (C <> nil) and (Status = dsDownloading) then
  begin
    { TODO:
      Freeing TNetHttpClient before it finished work leads to crashes.
      Testcase: asynchronous_download CGE example, press "Abort Downloads"
      after starting the downloads.

      We let the C and ContentsOwned to just leak,
      this still causes some crashes but at least we can ignore them
      (they seem to happen then only in some non-main thread).

      We also pass ReceiveDataAbort to at least stop actual transfer
      as soon as feasible.

      Maybe we should instead use TNetHTTPRequest with Cancel method?
      > https://stackoverflow.com/questions/76268341/correct-way-to-terminate-a-tnethttpclient-get-request-with-a-live-stream
      > You can use TNetHTTPClient asynchronously from the main thread and just cancel the request when application is closing or on any other appropriate user action. But to do that you need to use TNetHTTPRequest as you can call Cancel on the request associated with the TNetHTTPClient and you call Get on the request and not on the client.
    }

    C.OnReceiveData := TDelphiNetReader.ReceiveDataAbort;
    C.OnRequestCompleted := nil;
    C.OnRequestError := nil;
    C.OnRequestException := nil;
    WritelnWarning('Leaking TNetHttpClient reference. Freeing it before the request has completed leads to crashes');
  end else
  begin
    FreeAndNil(ContentsOwned);
    FreeAndNil(C);
  end;

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
  AAbort := true;
end;

procedure TDelphiNetReader.RequestCompleted(const Sender: TObject;
  const AResponse: IHTTPResponse);
begin
  HttpResponseCode := AResponse.StatusCode;
  MimeType := ContentTypeToMimeType(AResponse.MimeType);

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

  // TODO: set HttpResponseHeaders from AResponse.Headers
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

    How are the callbacks called, from where,
    since we don't necessarily execute the Delphi VCL / FMX event loop?
    E.g. with TCastleWindow we execute own loop.
    But the callbacks still happen, even in console application.
    It seems that at least for Windows, with TCastleWindow,
    Delphi somehow hooked to the Windows event loop to make them happen.

    TODO:
    - Do these callbacks still happen with Delphi/Linux? How?
    - How do these callbacks happen in console app?
      Are they really in main thread then -- but when? At Sleep,
      by more Windows hooks?
  }
end;

{$endif read_implementation}

{$endif defined(DELPHI) and defined(CASTLE_DELPHI_NET_HTTP_CLIENT)}
