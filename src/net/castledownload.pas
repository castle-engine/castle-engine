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

var
  { Can @link(Download) actually use the network.
    As all the downloading is blocking for now, this is initially @false.
    If you want to really use the network, change it to @true. }
  EnableNetwork: boolean = false;

type
  EDownloadError = class(Exception);

{ Return a stream to read given URL.
  Returned stream is suitable only for reading, and the initial position
  is always at the beginning.

  All errors are reported by raising exceptions.

  A local file URL (or the URL without any protocol) is always supported,
  without using any networking library. Set EnableNetwork to @true
  to have also support for network protocols (right now only http,
  handled by FpHttpClient). }
function Download(const URL: string): TStream;

implementation

uses URIParser, CastleUtils, CastleLog;

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
end;

function Download(const URL: string): TStream;
var
  URI: TURI;
  P, FileName: string;
const
  MaxRedirects = 32;
begin
  URI := ParseURI(URL);
  P := URI.Protocol;

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

    { TODO: TFileStream is not buffered (on some platforms).
      Make an option to just load this to TMemoryStream
      (like our CreateReadFileStream).
      Make an option to wrap in a buffered stream,
      like our VRML/X3D reader does.

      And make a version that always returns buffered stream,
      to avoid wrapping buffered stream in a buffered stream for VRML/X3D
      reader.

      The point is that for local filenames, no additional overhead
      (no additional streams in-between) should be created. }

    Result := TFileStream.Create(FileName, fmOpenRead);
  end else

    raise EDownloadError.CreateFmt('Downloading from protocol "%s" is not supported', [P]);
end;

end.
