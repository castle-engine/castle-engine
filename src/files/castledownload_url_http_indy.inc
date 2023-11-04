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

{$if defined(DELPHI) and not defined(CASTLE_DELPHI_NET_HTTP_CLIENT)}

{ Handle downloading http, https URLs using TIdHttp (Indy component).

  See https://docwiki.embarcadero.com/CodeExamples/Sydney/en/HTTP_Get_(Delphi) .

  We use this only with Delphi now, though in principle we could make it work
  with FPC too, as it has Indy too. But we don't need it for FPC, where we use
  FpHttpClient.

  Since Delphi includes Indy, we don't need to distribute Indy with CGE,
  and users don't need to worry about getting Indy.
}

{$ifdef read_implementation_uses}
IdHttp, IdSSL, IdSSLOpenSSL, IdSSLOpenSSLHeaders, IdCTypes,
{$endif}

{$ifdef read_implementation}

type
  { TUrlAsynchronousReader descendant that implements http(s) downloading. }
  TIndyReader = class(TUrlAsynchronousReader)
  strict private
    IdHttp: TIdHttp;
    MyIOHandler: TIdSSLIOHandlerSocketOpenSSL;
    procedure StatusInfoEx(ASender: TObject; const AsslSocket: PSSL; const AWhere, Aret: TIdC_INT;
      const AType, AMsg: String);
  public
    procedure Start; override;
    destructor Destroy; override;
    procedure Update; override;
  end;

  { Define TDelphiHttpReader used in castledownload_register.inc.
    It is an alias to a different class, depending on whether
    CASTLE_DELPHI_NET_HTTP_CLIENT was defined. }
  TDelphiHttpReader = TIndyReader;

{ TIndyReader --------------------------------------------------- }

procedure TIndyReader.StatusInfoEx(ASender: TObject; const AsslSocket: PSSL; const AWhere, Aret: TIdC_INT;
  const AType, AMsg: String);
begin
  SSL_set_tlsext_host_name(AsslSocket, IdHttp.Request.Host);
end;

procedure TIndyReader.Start;
begin
  inherited;
  Contents := TMemoryStream.Create;

  // TODO: Put this in thread, this seems to be how to make it asynchronous
  IdHttp := TIdHTTP.Create(nil);
  try
    IdHttp.HandleRedirects := true;

    { We need to pass User-Agent to avoid Cloudflare answering with 403 Forbidden.
      See https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/User-Agent
      about User-Agent. }
    IdHttp.Request.UserAgent :=  'Mozilla/5.0 (compatible; CastleGameEngine/1.0; https://castle-engine.io/manual_network.php)';

    { Set special IO handler to do SSL_set_tlsext_host_name,
      otherwise accessing sites behind Cloudflare will fail,
      see https://stackoverflow.com/questions/29875664/eidosslunderlyingcryptoerror-exception }
    MyIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(IdHttp);
    MyIOHandler.OnStatusInfoEx := StatusInfoEx;
    MyIOHandler.SSLOptions.Method := sslvSSLv23;
    MyIOHandler.SSLOptions.SSLVersions := [sslvTLSv1_2, sslvTLSv1_1, sslvTLSv1];
    IdHttp.IOHandler := MyIOHandler;

    IdHttp.Get(Url, Contents);

    HttpResponseCode := IdHttp.ResponseCode;
    DownloadedBytes := Contents.Size;
    TotalBytes := Contents.Size;
    MimeType := URIMimeType(Url); // TODO: get from server
    Contents.Position := 0; { rewind for easy reading }
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
  except
    on E: TObject do
    begin
      Status := dsError;
      ErrorMessage := Format('Exception when downloading "%s": ', [URIDisplay(Url)]) + ExceptMessage(E);
    end;
  end;

  // TODO: Change this value during HTTP redirects.
  FinalUrl := Url;
end;

destructor TIndyReader.Destroy;
begin
  // We have to free MyIOHandler early, to avoid later exceptions
  FreeAndNil(MyIOHandler);
  FreeAndNil(IdHttp);
  inherited;
end;

procedure TIndyReader.Update;
begin
  inherited;
  // TODO: This does nothing, Start actually is synchronous
end;

{$endif read_implementation}

{$endif defined(DELPHI) and not defined(CASTLE_DELPHI_NET_HTTP_CLIENT)}
