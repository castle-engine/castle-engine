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
  const WriteEvent: TUrlWriteEvent);

{$endif}

{$ifdef read_implementation}

{ registering URL protocols -------------------------------------------------- }

type
  TRegisteredProtocol = class
    Protocol: String;
    ReadEvent: TUrlReadEvent;
    WriteEvent: TUrlWriteEvent;
  end;

  TRegisteredProtocols = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TRegisteredProtocol>)
    { @nil if not found. }
    function Find(const Protocol: String): TRegisteredProtocol;

    procedure Add(const Protocol: String;
      const ReadEvent: TUrlReadEvent;
      const WriteEvent: TUrlWriteEvent); reintroduce;
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
  const WriteEvent: TUrlWriteEvent);
var
  P: TRegisteredProtocol;
begin
  if Find(Protocol) <> nil then
    raise EProtocolAlreadyRegistered.CreateFmt('URL protocol "%s" is already registered', [Protocol]);
  P := TRegisteredProtocol.Create;
  P.Protocol := Protocol;
  P.ReadEvent := ReadEvent;
  P.WriteEvent := WriteEvent;
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
    FRegisteredProtocols.Add('http', @TUrlHttpAndroid(nil).Read, nil);
    FRegisteredProtocols.Add('https', @TUrlHttpAndroid(nil).Read, nil);
    {$elseif defined(HAS_FP_HTTP_CLIENT)}
    FRegisteredProtocols.Add('http', @TUrlHttpFpHttpClient(nil).Read, nil);
    FRegisteredProtocols.Add('https', @TUrlHttpFpHttpClient(nil).Read, nil);
    {$endif}
    FRegisteredProtocols.Add('', @TUrlFile(nil).Read, @TUrlFile(nil).Write);
    FRegisteredProtocols.Add('file', @TUrlFile(nil).Read, @TUrlFile(nil).Write);
    {$if defined(ANDROID)}
    FRegisteredProtocols.Add('assets', @TUrlCastleAndroidAssets(nil).Read, nil);
    FRegisteredProtocols.Add('castle-android-assets', @TUrlCastleAndroidAssets(nil).Read, nil);
    {$endif}
    FRegisteredProtocols.Add('castle-data', nil, nil);
    FRegisteredProtocols.Add('data', @TUrlData(nil).Read, nil);
    FRegisteredProtocols.Add('ecmascript', @TUrlEcmaScript(nil).Read, nil);
    FRegisteredProtocols.Add('javascript', @TUrlEcmaScript(nil).Read, nil);
    FRegisteredProtocols.Add('castlescript', @TUrlCastleScript(nil).Read, nil);
    FRegisteredProtocols.Add('kambiscript', @TUrlCastleScript(nil).Read, nil);
    FRegisteredProtocols.Add('compiled', @TUrlCompiled(nil).Read, nil);
  end;
  Result := FRegisteredProtocols;
end;

procedure RegisterUrlProtocol(const Protocol: String;
  const ReadEvent: TUrlReadEvent;
  const WriteEvent: TUrlWriteEvent);
begin
  RegisteredProtocols.Add(Protocol, ReadEvent, WriteEvent);
end;

{$endif}
