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

{ Handle http, https URLs using Indy.
  See https://docwiki.embarcadero.com/CodeExamples/Sydney/en/HTTP_Get_(Delphi) .

  For now this is used only with Delphi, where Indy is built-in in Delphi
  (users don't need to download Indy, CGE doesn't need to distribute Indy).
  With FPC we use castledownload_url_http_fphttpclient.inc,
  with FpHttpClient that is built-in in FPC.
  Though in principle this code could be made to compile also with FPC
  using Indy one day, if necessary. }

{$ifdef DELPHI}

type
  { TUrlAsynchronousReader descendant that implements http(s) downloading. }
  TIndyHttpClientReader = class(TUrlAsynchronousReader)
  strict private
    IdHttp: TIdHTTP;
  public
    procedure Start; override;
    destructor Destroy; override;
    procedure Update; override;
  end;

{ TIndyHttpClientReader --------------------------------------------------- }

procedure TIndyHttpClientReader.Start;
begin
  inherited;

  // TODO: Put this in thread, this seems to be how to make it asynchronous
  IdHttp := TIdHTTP.Create(nil);
  try
    // TODO: handle redirects
    IdHttp.Get(Url, Contents);
    Status := dsSuccess;
    MimeType := URIMimeType(Url); // TODO: get from server
    Contents.Position := 0; { rewind for easy reading }
  except
    on E: TObject do
    begin
      Status := dsError;
      ErrorMessage := Format('Error when downloading "%s": ', [URIDisplay(Url)]) + ExceptMessage(E);
    end;
  end;
end;

destructor TIndyHttpClientReader.Destroy;
begin
  FreeAndNil(IdHttp);
  inherited;
end;

procedure TIndyHttpClientReader.Update;
begin
  inherited;
  // TODO: This does nothing, Start actually is synchronous
end;

{$endif DELPHI}

{$endif read_implementation}
