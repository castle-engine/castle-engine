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
    { Fields set before Start is called. Should not be modified by this instance after Start. }
    Url: String;
    ForceSynchronous: Boolean;
    HttpMethod: THttpMethod;
    HttpPostData, HttpHeadersKeys, HttpHeadersValues: TStrings;

    { Fields that should be updated during downloading. }
    Status: TDownloadStatus; //< Never dsNotStarted, as we start in constructor.
    DownloadedBytes, TotalBytes: Int64; //< Will set TCastleDownlad properties of the same name.
    Contents: TStream; //< Not owned by this instance, TCastleDownlad takes care of freeing this.
    ErrorMessage: String; //< When Status = dsError, download failed with this error.
    MimeType: String; //< May be updated at any point, even when dsDownloading, or only when dsSuccess.
    HttpResponseCode: Integer; // In case of HTTP requests, HTTP response code.
    HttpResponseHeaders: TStrings; // In case of HTTP requests, may (if not @nil) contain HTTP response headers.

    { Exceptions inside this will make an exception from TCastleDownload.Start.
      They should be caught if should make Status = dsError instead. }
    procedure Start; virtual;

    { Exceptions inside Update will make an exception at undefined application point
      (for now, ApplicationProperties._Update, called by CastleWindow and CastleControl).
      They should be caught if should make Status = dsError instead. }
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
  @raises(EProtocolAlreadyRegistered If the protocol handlers are already registered.) }
procedure RegisterUrlProtocol(const Protocol: String;
  const ReadEvent: TUrlReadEvent;
  const WriteEvent: TUrlWriteEvent;
  const AsynchronousReader: TUrlAsynchronousReaderClass = nil);

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
  TRegisteredProtocol = class
    Protocol: String;
    ReadEvent: TUrlReadEvent;
    WriteEvent: TUrlWriteEvent;
    AsynchronousReader: TUrlAsynchronousReaderClass;
  end;

  TRegisteredProtocols = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TRegisteredProtocol>)
    { @nil if not found. }
    function Find(const Protocol: String): TRegisteredProtocol;

    procedure Add(const Protocol: String;
      const ReadEvent: TUrlReadEvent;
      const WriteEvent: TUrlWriteEvent;
      const AsynchronousReader: TUrlAsynchronousReaderClass); reintroduce;
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
  const AsynchronousReader: TUrlAsynchronousReaderClass);
var
  P: TRegisteredProtocol;
begin
  if Find(Protocol) <> nil then
    raise EProtocolAlreadyRegistered.CreateFmt('URL protocol "%s" is already registered', [Protocol]);
  P := TRegisteredProtocol.Create;
  P.Protocol := Protocol;
  P.ReadEvent := ReadEvent;
  P.WriteEvent := WriteEvent;
  P.AsynchronousReader := AsynchronousReader;
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
    FRegisteredProtocols.Add('http', @TUrlHttpAndroid(nil).Read, nil, nil);
    FRegisteredProtocols.Add('https', @TUrlHttpAndroid(nil).Read, nil, nil);
    {$elseif defined(HAS_FP_HTTP_CLIENT)}
    FRegisteredProtocols.Add('http', nil, nil, TFpHttpClientReader);
    FRegisteredProtocols.Add('https', nil, nil, TFpHttpClientReader);
    {$endif}
    FRegisteredProtocols.Add('', @TUrlFile(nil).Read, @TUrlFile(nil).Write, nil);
    FRegisteredProtocols.Add('file', @TUrlFile(nil).Read, @TUrlFile(nil).Write, nil);
    {$if defined(ANDROID)}
    FRegisteredProtocols.Add('assets', @TUrlCastleAndroidAssets(nil).Read, nil, nil);
    FRegisteredProtocols.Add('castle-android-assets', @TUrlCastleAndroidAssets(nil).Read, nil, nil);
    {$endif}
    FRegisteredProtocols.Add('castle-data', nil, nil, nil);
    FRegisteredProtocols.Add('data', @TUrlData(nil).Read, nil, nil);
    FRegisteredProtocols.Add('ecmascript', @TUrlEcmaScript(nil).Read, nil, nil);
    FRegisteredProtocols.Add('javascript', @TUrlEcmaScript(nil).Read, nil, nil);
    FRegisteredProtocols.Add('castlescript', @TUrlCastleScript(nil).Read, nil, nil);
    FRegisteredProtocols.Add('kambiscript', @TUrlCastleScript(nil).Read, nil, nil);
    FRegisteredProtocols.Add('compiled', @TUrlCompiled(nil).Read, nil, nil);
  end;
  Result := FRegisteredProtocols;
end;

procedure RegisterUrlProtocol(const Protocol: String;
  const ReadEvent: TUrlReadEvent;
  const WriteEvent: TUrlWriteEvent;
  const AsynchronousReader: TUrlAsynchronousReaderClass);
begin
  RegisteredProtocols.Add(Protocol, ReadEvent, WriteEvent, AsynchronousReader);
end;

{$endif}
