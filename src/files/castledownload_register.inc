{%MainUnit castledownload.pas}
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
  { See @link(TCastleDownload.Status). }
  TDownloadStatus = (dsNotStarted, dsDownloading, dsError, dsSuccess);

  { See @link(TCastleDownload.HttpMethod). }
  THttpMethod = (hmGet, hmPost, hmPut, hmDelete, hmOptions, hmHead);

  { Implement this class, and pass to RegisterUrlProtocol,
    to read protocols asynchronously (such that TCastleDownload can read
    them asynchronously).

    @bold(There is no point in using this class
    unless you want to handle custom URL protocols).
    If you just want to download asynchronously from standard URLs,
    like file or http, then use TCastleDownload class. }
  TUrlAsynchronousReader = class
  public
    { Fields set before Start is called. Should not be modified by this instance after Start.
      @groupBegin }
    Url: String;
    ForceSynchronous: Boolean;
    HttpMethod: THttpMethod;
    HttpPostData, HttpHeadersKeys, HttpHeadersValues: TStrings;
    { @groupEnd }

    { Fields that should be updated during downloading.
      They will set TCastleDownload properties of the same name.
      @groupBegin }
    Status: TDownloadStatus;
    DownloadedBytes, TotalBytes: Int64;
    Contents: TStream; //< Not owned by this instance, TCastleDownload takes care of freeing this.
    ErrorMessage: String; //< When Status = dsError, download failed with this error.
    MimeType: String; //< May be updated at any point, even when dsDownloading, or only when dsSuccess.
    HttpResponseCode: Integer; // In case of HTTP(S) requests, HTTP response code.
    HttpResponseHeaders: TStrings; // In case of HTTP(S) requests, this must contain HTTP response headers.
    FinalUrl: String;
    { @groupEnd }

    { Start downloading, called when @link(TCastleDownload.Start) is called.

      @italic(Note for descendant implementors):
      Exceptions raised inside this will make an actual exception from
      @link(TCastleDownload.Start).
      In case of normal download failure, e.g. file (or HTTP resource) doesn't exist
      (so it is not some bug in code), this method should set Status to dsError
      and not raise an exception. }
    procedure Start; virtual;

    { Called continuously to push the download further.

      @italic(Note for descendant implementors):
      Exceptions raised inside this will make an actual exception (from
      undefined place, for now ApplicationProperties._Update, called by TCastleWindow and TCastleControl).
      In case of normal download failure, e.g. file (or HTTP resource) doesn't exist
      (so it is not some bug in code), this method should set Status to dsError
      and not raise an exception. }
    procedure Update; virtual;
  end;

  TUrlAsynchronousReaderClass = class of TUrlAsynchronousReader;

  { Event called when @link(Download) function wants to download URL with this protocol.
    Use with @link(RegisterUrlProtocol). }
  TUrlReadEvent = function (
    const Url: string; out MimeType: string): TStream of object;

  { Event called when @link(URLSaveStream) function wants to save URL with this protocol.
    Use with @link(RegisterUrlProtocol). }
  TUrlWriteEvent = function(const Url: string): TStream of object;

  EProtocolAlreadyRegistered = class(Exception);

{ Register how we can load and/or save the URLs with given protocol.
  One (or even both) of given events (ReadEvent, WriteEvent) can be @nil.

  @param ReadEvent Used when using synchronous reading by @link(Download).

  @param WriteEvent Used when using synchronus writing by @link(URLSaveStream).

  @param AsynchronousReader Used when using asynchronous reading by @link(TCastleDownload).

  @raises(EProtocolAlreadyRegistered If the protocol handlers are already registered.) }
procedure RegisterUrlProtocol(const Protocol: String;
  const ReadEvent: TUrlReadEvent;
  const WriteEvent: TUrlWriteEvent;
  const AsynchronousReader: TUrlAsynchronousReaderClass = nil;
  const DetectMimeTypeFromExtension: Boolean = true);

{ Is given protocol name registered with @link(RegisterUrlProtocol). }
function RegisteredUrlProtocol(const Protocol: String): Boolean;

{ Unregister protocol, reverting the @link(RegisterUrlProtocol). }
procedure UnregisterUrlProtocol(const Protocol: String);

{$endif}

{$ifdef read_implementation}

procedure TUrlAsynchronousReader.Start;
begin
  Status := dsDownloading;
  TotalBytes := -1;

  { ForceSynchronous allows to make synchronous Download work even when
    - binary has no thread support linked (e.g. no CThreads linked on Unix).
    - OS has no thread support.
    Moreover TCastleDownload can "downgrade" to working synchronously
    (instead of just failing) when OS never supports threads.

    Outside code may set ForceSynchronous, but we can override it to true. }
  {$ifndef CASTLE_SUPPORTS_THREADING}
  ForceSynchronous := true;
  {$endif}
end;

procedure TUrlAsynchronousReader.Update;
begin
end;

{ registering URL protocols -------------------------------------------------- }

type
  { Event called when @link(URIMimeType) function wants to get MIME type from given URL.
    Use with @link(RegisterUrlProtocol). }
  TUrlMimeTypeEvent = function(const Url: string; out Gzipped: Boolean): String of object;

  TRegisteredProtocol = class
    Protocol: String;
    ReadEvent: TUrlReadEvent;
    WriteEvent: TUrlWriteEvent;
    AsynchronousReader: TUrlAsynchronousReaderClass;
    MimeTypeEvent: TUrlMimeTypeEvent;
  end;

  TRegisteredProtocols = class({$ifdef FPC}specialize{$endif} TObjectList<TRegisteredProtocol>)
    { @nil if not found. }
    function Find(const Protocol: String): TRegisteredProtocol;

    procedure Add(const Protocol: String;
      const ReadEvent: TUrlReadEvent;
      const WriteEvent: TUrlWriteEvent;
      const AsynchronousReader: TUrlAsynchronousReaderClass;
      const MimeTypeEvent: TUrlMimeTypeEvent); reintroduce;
  end;

function TRegisteredProtocols.Find(const Protocol: String): TRegisteredProtocol;
begin
  for Result in Self do
    if Result.Protocol = Protocol then
      Exit;
  Result := nil;
end;

procedure TRegisteredProtocols.Add(const Protocol: String;
  const ReadEvent: TUrlReadEvent;
  const WriteEvent: TUrlWriteEvent;
  const AsynchronousReader: TUrlAsynchronousReaderClass;
  const MimeTypeEvent: TUrlMimeTypeEvent);
var
  P: TRegisteredProtocol;
begin
  if Find(Protocol) <> nil then
    raise EProtocolAlreadyRegistered.CreateFmt('URL protocol "%s" is already registered', [Protocol]);
  // We allow Protocol = '', as it is used internally
  if (Protocol <> '') and
     (not URIValidProtocol(Protocol)) then
    raise Exception.CreateFmt('URL protocol name "%s" is invalid (see URIValidProtocol for the requirements on protocol name)', [Protocol]);
  P := TRegisteredProtocol.Create;
  P.Protocol := Protocol;
  P.ReadEvent := ReadEvent;
  P.WriteEvent := WriteEvent;
  P.AsynchronousReader := AsynchronousReader;
  P.MimeTypeEvent := MimeTypeEvent;
  inherited Add(P);
end;

var
  FRegisteredProtocols: TRegisteredProtocols;

function RegisteredProtocols: TRegisteredProtocols;
begin
  { initialize FRegisteredProtocols on-demand }
  if FRegisteredProtocols = nil then
  begin
    FRegisteredProtocols := TRegisteredProtocols.Create(true);

    // register default protocols handled in this unit

    {$if defined(ANDROID)}
    // http(s) using Android service
    FRegisteredProtocols.Add('http', nil, nil, THttpAndroidReader,
      {$ifdef FPC}@{$endif} TUrlFile{$ifdef FPC}(nil){$endif}.MimeType);
    FRegisteredProtocols.Add('https', nil, nil, THttpAndroidReader,
      {$ifdef FPC}@{$endif} TUrlFile{$ifdef FPC}(nil){$endif}.MimeType);

    {$elseif defined(HAS_FP_HTTP_CLIENT)}
    // http(s) using FPC FpHttpClient
    FRegisteredProtocols.Add('http', nil, nil, TFpHttpClientReader,
      {$ifdef FPC}@{$endif} TUrlFile{$ifdef FPC}(nil){$endif}.MimeType);
    FRegisteredProtocols.Add('https', nil, nil, TFpHttpClientReader,
      {$ifdef FPC}@{$endif} TUrlFile{$ifdef FPC}(nil){$endif}.MimeType);

    {$elseif defined(DELPHI)}
    // http(s) using Delphi System.Net.HttpClient or Indy
    FRegisteredProtocols.Add('http', nil, nil, TDelphiHttpReader,
      TUrlFile.MimeType);
    FRegisteredProtocols.Add('https', nil, nil, TDelphiHttpReader,
      TUrlFile.MimeType);
    {$endif}

    FRegisteredProtocols.Add('',
      {$ifdef FPC}@{$endif} TUrlFile{$ifdef FPC}(nil){$endif}.Read,
      {$ifdef FPC}@{$endif} TUrlFile{$ifdef FPC}(nil){$endif}.Write, nil,
      {$ifdef FPC}@{$endif} TUrlFile{$ifdef FPC}(nil){$endif}.MimeType);
    FRegisteredProtocols.Add('file',
      {$ifdef FPC}@{$endif} TUrlFile{$ifdef FPC}(nil){$endif}.Read,
      {$ifdef FPC}@{$endif} TUrlFile{$ifdef FPC}(nil){$endif}.Write, nil,
      {$ifdef FPC}@{$endif} TUrlFile{$ifdef FPC}(nil){$endif}.MimeType);
    {$if defined(ANDROID)}
    FRegisteredProtocols.Add('assets',
      {$ifdef FPC}@{$endif} TUrlCastleAndroidAssets{$ifdef FPC}(nil){$endif}.Read, nil, nil,
      {$ifdef FPC}@{$endif} TUrlFile{$ifdef FPC}(nil){$endif}.MimeType);
    FRegisteredProtocols.Add('castle-android-assets',
      {$ifdef FPC}@{$endif} TUrlCastleAndroidAssets{$ifdef FPC}(nil){$endif}.Read, nil, nil,
      {$ifdef FPC}@{$endif} TUrlFile{$ifdef FPC}(nil){$endif}.MimeType);
    {$endif}
    FRegisteredProtocols.Add('castle-data', nil, nil, nil,
      {$ifdef FPC}@{$endif} TUrlFile{$ifdef FPC}(nil){$endif}.MimeType);
    FRegisteredProtocols.Add('data',
      {$ifdef FPC}@{$endif} TUrlData{$ifdef FPC}(nil){$endif}.Read, nil, nil,
      {$ifdef FPC}@{$endif} TUrlData{$ifdef FPC}(nil){$endif}.MimeType);
    FRegisteredProtocols.Add('ecmascript',
      {$ifdef FPC}@{$endif} TUrlEcmaScript{$ifdef FPC}(nil){$endif}.Read, nil, nil,
      {$ifdef FPC}@{$endif} TUrlEcmaScript{$ifdef FPC}(nil){$endif}.MimeType);
    FRegisteredProtocols.Add('javascript',
      {$ifdef FPC}@{$endif} TUrlEcmaScript{$ifdef FPC}(nil){$endif}.Read, nil, nil,
      {$ifdef FPC}@{$endif} TUrlEcmaScript{$ifdef FPC}(nil){$endif}.MimeType);
    FRegisteredProtocols.Add('castlescript',
      {$ifdef FPC}@{$endif} TUrlCastleScript{$ifdef FPC}(nil){$endif}.Read, nil, nil,
      {$ifdef FPC}@{$endif} TUrlCastleScript{$ifdef FPC}(nil){$endif}.MimeType);
    FRegisteredProtocols.Add('kambiscript',
      {$ifdef FPC}@{$endif} TUrlCastleScript{$ifdef FPC}(nil){$endif}.Read, nil, nil,
      {$ifdef FPC}@{$endif} TUrlCastleScript{$ifdef FPC}(nil){$endif}.MimeType);
    FRegisteredProtocols.Add('compiled',
      {$ifdef FPC}@{$endif} TUrlCompiled{$ifdef FPC}(nil){$endif}.Read, nil, nil,
      {$ifdef FPC}@{$endif} TUrlCompiled{$ifdef FPC}(nil){$endif}.MimeType);
  end;
  Result := FRegisteredProtocols;
end;

procedure RegisterUrlProtocol(const Protocol: String;
  const ReadEvent: TUrlReadEvent;
  const WriteEvent: TUrlWriteEvent;
  const AsynchronousReader: TUrlAsynchronousReaderClass = nil;
  const DetectMimeTypeFromExtension: Boolean = true);
var
  MimeTypeEvent: TUrlMimeTypeEvent;
begin
  if DetectMimeTypeFromExtension then
    MimeTypeEvent := {$ifdef FPC}@{$endif} TUrlFile{$ifdef FPC}(nil){$endif}.MimeType
  else
    MimeTypeEvent := nil;
  RegisteredProtocols.Add(Protocol, ReadEvent, WriteEvent, AsynchronousReader, MimeTypeEvent);
end;

function RegisteredUrlProtocol(const Protocol: String): Boolean;
begin
  Result := RegisteredProtocols.Find(Protocol) <> nil;
end;

procedure UnregisterUrlProtocol(const Protocol: String);
var
  P: TRegisteredProtocol;
begin
  P := RegisteredProtocols.Find(Protocol);
  if P <> nil then
    RegisteredProtocols.Remove(P);
end;

{$endif}
