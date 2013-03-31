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

  { Options for the @link(Download) function. }
  TDownloadOption = (
    { Force result to be a TMemoryStream,
      with contents fully loaded to the memory,
      and freely seekable (you can move back and forth within).
      Without this option, @link(Download)may return other streams,
      like TFileStream
      (that may not have good buffering, depending on OS).

      Using TMemoryStream means that reading is fast and comfortable,
      but eats memory and doesn't allow to simultaneously read and process
      the contents (the file must be fully loaded, e.g. downloaded from
      the Internet, and ungzipped, before this function returns).
      So use this option only for files that aren't too big.

      For larger files, you usually don't want to use this option,
      instead wrap result in TBufferedReadStream. }
    doForceMemoryStream,

    { Filter the contents through gzip decompression. }
    doGunzip
  );
  TDownloadOptions = set of TDownloadOption;

{ Return a stream to read given URL.
  Returned stream is suitable only for reading, and the initial position
  is always at the beginning.
  Overloaded version also returns a MIME type (or '' if unknown).

  All errors are reported by raising exceptions.

  A local file URL is always supported,
  without using any networking library. URL without any protocol is always
  treated like a local filename (absolute or relative to current dir),
  so this function can be a drop-in replacement for normal file reading.
  The MIME type for local files is guessed based on their extension.

  A data URI scheme (http://en.wikipedia.org/wiki/Data_URI_scheme)
  is also always supported.
  The MIME type for such content is specified explicitly in URI.

  Set EnableNetwork to @trueto have also support for network protocols.
  Right now this means only http, handled by FpHttpClient.
  The MIME type for such content is reported by http server. }
function Download(const URL: string; const Options: TDownloadOptions = []): TStream;
function Download(const URL: string; const Options: TDownloadOptions;
  out MimeType: string): TStream;

implementation

uses URIParser, CastleURIUtils, CastleUtils, CastleLog, CastleZStream,
  CastleClassUtils, CastleFilesUtils, CastleDataURI;

{ Just like Download, but
  - Assumes that the URL is from the network (this prevents network URLs
    redirecting to local URLs),
  - Limits the number of redirects to given value.
  - Guarantees that result is TMemoryStream. Never handles gzip decompression. }
function NetworkDownload(const URL: string;
  const MaxRedirects: Cardinal; out MimeType: string): TMemoryStream;

  function ContentTypeToMimeType(const ContentType: string): string;
  var
    P: Integer;
  begin
    P := Pos(';', ContentType);
    if P <> 0 then
      Result := Trim(Copy(ContentType, 1, P - 1)) else
      Result := Trim(ContentType);
  end;

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
      // Writeln(Client.ResponseHeaders.Text);
      Client.ResponseHeaders.NameValueSeparator := ':';
      if Client.ResponseStatusCode <> 200 then
      begin
        FreeAndNil(Result);
        RedirectLocation := Trim(Client.ResponseHeaders.Values['Location']);
        if RedirectLocation = '' then
          raise EDownloadError.Create('HTTP redirect location is not set');
        if MaxRedirects = 0 then
          raise EDownloadError.Create('Cannot download resource, maximum number of redirects reached. Possible redirect loop');
        WritelnLog('Network', 'Following HTTP redirect (code %d) to "%s"',
          [Client.ResponseStatusCode, RedirectLocation]);
        Exit(NetworkDownload(RedirectLocation, MaxRedirects - 1, MimeType));
      end;
      MimeType := ContentTypeToMimeType(Client.ResponseHeaders.Values['Content-Type']);
      WritelnLog('Network', 'Successfully downloaded "%s", MIME type "%s"', [URL, MimeType]);
    finally FreeAndNil(Client) end;
    Result.Position := 0; { rewind for easy reading }
  except
    FreeAndNil(Result); raise;
  end;
end;

{ Load FileName to TMemoryStream. }
function CreateMemoryStream(const FileName: string): TMemoryStream;
begin
  Result := TMemoryStream.Create;
  try
    Result.LoadFromFile(FileName);
    Result.Position := 0; { rewind for easy reading }
  except
    FreeAndNil(Result); raise;
  end;
end;

{ Decompress gzipped FileName.
  When ForceMemoryStream, always returns TMemoryStream. }
function ReadGzipped(const FileName: string; const ForceMemoryStream: boolean): TStream;
var
  NewResult: TMemoryStream;
begin
  Result := TGZFileStream.Create(FileName, gzOpenRead);
  try
    if ForceMemoryStream then
    begin
      { TODO: our engine never uses both doGunzip and doForceMemoryStream
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

function Download(const URL: string; const Options: TDownloadOptions;
  out MimeType: string): TStream;
var
  P, FileName, TempFileName: string;
  NetworkResult: TMemoryStream;
  DataURI: TDataURI;
const
  MaxRedirects = 32;
begin
  P := LowerCase(URIProtocol(URL));

  { network protocols: get data into a new TMemoryStream using FpHttpClient }
  if EnableNetwork and (P = 'http') then
  begin
    WritelnLog('Network', 'Downloading "%s"', [URL]);
    NetworkResult := NetworkDownload(URL, MaxRedirects, MimeType);
    try
      if doGunzip in Options then
      begin
        { for now, reading gzipped file from a TMemoryStream means using
          a temporary file }
        TempFileName := GetTempFileNameCheck;
        if Log then
          WritelnLog('Network', Format('Decompressing gzip from the network by temporary file "%s"', [TempFileName]));
        NetworkResult.SaveToFile(TempFileName);
        FreeAndNil(NetworkResult);
        Result := ReadGzipped(TempFileName, doForceMemoryStream in Options);
        CheckDeleteFile(TempFileName, true);
      end else
        Result := NetworkResult;
    except
      FreeAndNil(NetworkResult); raise;
    end;
  end else

  { local filenames are directly handled, without the need for any downloader }
  if (P = '') or (P = 'file') then
  begin
    { when there is no protocol, do not call URIToFilename.
      This fixes the case when URL = absolute Windows filename.
      Our URIProtocol detects an empty protocol, but ParseURI and URIToFilename
      will detect a single-letter protocol and URIToFilename will fail.
      So it's just not possible to pass safely a Windows filename to URIToFilename
      and expect it to do nothing. }
    if P = '' then
      FileName := URL else
    if not URIToFilename(URL, FileName) then
      raise EDownloadError.CreateFmt('Cannot convert URL "%s" to filename', [URL]);

    if doGunzip in Options then
      Result := ReadGzipped(FileName, doForceMemoryStream in Options) else
    if doForceMemoryStream in Options then
      Result := CreateMemoryStream(FileName) else
      Result := TFileStream.Create(FileName, fmOpenRead);
    MimeType := URIMimeType(URL);
  end else

  { data: URI scheme }
  if P = 'data' then
  begin
    DataURI := TDataURI.Create;
    try
      DataURI.URI := URL;
      if not DataURI.Valid then
        raise EDownloadError.Create('Invalid data: URI scheme');
      Result := DataURI.ExtractStream;
      MimeType := DataURI.MimeType;
      Assert(Result <> nil, 'DataURI.ExtractStream must be non-nil when DataURI.Valid is true');
    finally FreeAndNil(DataURI) end;
  end else

    raise EDownloadError.CreateFmt('Downloading from protocol "%s" is not supported', [P]);
end;

function Download(const URL: string; const Options: TDownloadOptions): TStream;
var
  MimeType: string;
begin
  Result := Download(URL, Options, MimeType { ignored });
end;

end.
