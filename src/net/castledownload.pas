{
  Copyright 2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Download URLs. }
unit CastleDownload;

{ TODO:
  Problems of FpHttpClient:
  - Get crashes on URLs with only host name without trailing slash,
    like http://google.pl . I'm not sure are these valid URLs --- anyway,
    people use them.
  - Get leaks memory on larger files? Like http://maps.com.pl/ .
}

interface

uses SysUtils, Classes, FpHttpClient;

const
  DefaultEnableNetwork = false;

var
  { Can @link(Download) actually use the network.
    As all the downloading is blocking for now, this is initially @false.
    If you want to really use the network, change it to @true. }
  EnableNetwork: boolean = DefaultEnableNetwork;

type
  EDownloadError = class(Exception);

{ Return a stream to read given URL.
  Returned stream is suitable only for reading, and the initial position
  is always at the beginning.

  All errors are reported by raising exceptions.

  A local file URL is always supported,
  without using any networking library. URL without any protocol is always
  treated like a local filename (absolute or relative to current dir),
  so this function can be a drop-in replacement for normal file reading.
  Set EnableNetwork to @true
  to have also support for network protocols (right now only http,
  handled by FpHttpClient).

  LocalFileInMemory determines whether to open local files as TFileStream
  (when it is false, default) or load them to TMemoryStream.
  Using TMemoryStream means that reading is very fast (TFileStream may have
  poor buffering, depending on OS), but eats memory. }
function Download(const URL: string;
  const LocalFileInMemory: boolean = false): TStream;

implementation

uses URIParser, CastleURLUtils, CastleUtils, CastleLog;

{ Just like Download, but
  - Assumes that the URL is from the network (this prevents network URLs
    redirecting to local URLs),
  - Limits the number of redirects to given value. }
function NetworkDownload(const URL: string; const MaxRedirects: Cardinal): TStream;
var
  Client: TFPHTTPClient;
  RedirectLocation: string;
begin
  Result := TMemoryStream.Create;
  try
    Client := TFPHTTPClient.Create(nil);
    try
      { do not simply use Client.Get(URL, Result), as it cannot handle redirects }
      Client.HTTPMethod('GET', URL, Result, [200,
        { redirect status codes, see http://en.wikipedia.org/wiki/HTTP_302 }
        301, 302, 303, 307]);
      if Client.ResponseStatusCode <> 200 then
      begin
        FreeAndNil(Result);
        // Writeln(Client.ResponseHeaders.Text);
        Client.ResponseHeaders.NameValueSeparator := ':';
        RedirectLocation := Trim(Client.ResponseHeaders.Values['Location']);
        if RedirectLocation = '' then
          raise EDownloadError.Create('HTTP redirect location is not set');
        if MaxRedirects = 0 then
          raise EDownloadError.Create('Cannot download resource, maximum number of redirects reached. Possible redirect loop');
        WritelnLog('Network', 'Following HTTP redirect (code %d) to "%s"',
          [Client.ResponseStatusCode, RedirectLocation]);
        Exit(NetworkDownload(RedirectLocation, MaxRedirects - 1));
      end;
    finally FreeAndNil(Client) end;
    Result.Position := 0; { rewind for easy reading }
  except
    FreeAndNil(Result); raise;
  end;
end;

function Download(const URL: string; const LocalFileInMemory: boolean): TStream;
var
  P, FileName: string;
const
  MaxRedirects = 32;
begin
  P := UrlProtocol(URL);

  { network protocols: get data into a new TMemoryStream using FpHttpClient }
  if EnableNetwork and (CompareText(P, 'http') = 0) then
  begin
    WritelnLog('Network', 'Downloading "%s"', [URL]);
    Result := NetworkDownload(URL, MaxRedirects);
  end else

  { local filenames are directly handled, without the need for any downloader }
  if (P = '') or (CompareText(P, 'file') = 0) then
  begin
    if not URIToFilename(URL, FileName) then
      raise EDownloadError.CreateFmt('Cannot convert URL "%s" to filename', [URL]);
    if LocalFileInMemory then
    begin
      Result := TMemoryStream.Create;
      try
        TMemoryStream(Result).LoadFromFile(FileName);
        Result.Position := 0;
      except
        FreeAndNil(Result); raise;
      end;
    end else
      Result := TFileStream.Create(FileName, fmOpenRead);
  end else

    raise EDownloadError.CreateFmt('Downloading from protocol "%s" is not supported', [P]);
end;

end.
