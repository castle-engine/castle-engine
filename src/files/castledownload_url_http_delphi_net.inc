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

{$ifdef read_interface}

{$endif}

{$ifdef read_implementation}

{ Handle http, https URLs using Delphi TNetHTTPClient.
  See https://docwiki.embarcadero.com/RADStudio/Sydney/en/Using_an_HTTP_Client ,
  https://github.com/michaliskambi/test-delphi-net-http/tree/master .

  Note: We use this instead of Indy TIdHTTP,
  https://docwiki.embarcadero.com/CodeExamples/Sydney/en/HTTP_Get_(Delphi) ,
  as TNetHTTPClient seems more feature-packed:
  TNetHTTPClient can do asynchronous operation,
  doing work in thread, automatically. }

{$ifdef DELPHI}

type
  { TUrlAsynchronousReader descendant that implements http(s) downloading. }
  TDelphiNetReader = class(TUrlAsynchronousReader)
  strict private
    C: TNetHttpClient;
    procedure ReceiveData(const Sender: TObject;
      AContentLength: Int64; AReadCount: Int64; var AAbort: Boolean);
    procedure RequestCompleted(const Sender: TObject;
      const AResponse: IHTTPResponse);
    procedure RequestError(const Sender: TObject;
      const AError: string);
  public
    procedure Start; override;
    destructor Destroy; override;
    procedure Update; override;
  end;

{ TDelphiNetReader --------------------------------------------------- }

procedure TDelphiNetReader.Start;
begin
  inherited;

  { TODO: honor
    ForceSynchronous: Boolean;
    HttpMethod: THttpMethod;
    HttpPostData, HttpHeadersKeys, HttpHeadersValues: TStrings;
  }

  Contents := TMemoryStream.Create;

  C := TNetHttpClient.Create(nil);
  C.Asynchronous := true;
  C.OnRequestCompleted := RequestCompleted;
  C.OnRequestError := RequestError;
  C.OnReceiveData := ReceiveData;
  C.SynchronizeEvents := true;
  C.Get(Url, Contents);

  // TODO: support redirects, change this value during HTTP redirects
  FinalUrl := Url;
end;

destructor TDelphiNetReader.Destroy;
begin
  FreeAndNil(C);
  inherited;
end;

procedure TDelphiNetReader.ReceiveData(const Sender: TObject;
  AContentLength, AReadCount: Int64; var AAbort: Boolean);
begin
  DownloadedBytes := AReadCount;
  TotalBytes := AContentLength;
end;

procedure TDelphiNetReader.RequestCompleted(const Sender: TObject;
  const AResponse: IHTTPResponse);
begin
  HttpResponseCode := AResponse.StatusCode;
  MimeType := AResponse.MimeType;
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

procedure TDelphiNetReader.Update;
begin
  inherited;

  // No need to do anything, our event callbacks are called already.

  // TODO: When are they called? How does Delphi do Synchronize when
  // we're not in FMX or VCL loop? It seems to work even in console application.
end;

{$endif DELPHI}

{$endif read_implementation}
