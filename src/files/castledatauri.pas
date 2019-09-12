{
  Copyright 2010-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Reading data URI scheme (TDataURI). }
unit CastleDataURI;

{$I castleconf.inc}

interface

uses SysUtils, Classes;

type
  { Reading data URI scheme, see http://en.wikipedia.org/wiki/Data_URI_scheme.
    Such URI specifies the MIME type and contains the data, encoded
    in plain text or base64. That is, the data is not merely referenced / linked
    (like with a usual URL), it's simply fully encoded inside the URI.
    Since such URI can be used anywhere a normal URI is expected,
    it allows you to inline any kind of data inside any container file.
    For example, you can place images (textures) and such directly
    inside a VRML/X3D file. }
  TDataURI = class
  private
    StreamBegin: Cardinal;
    FStream: TStream;
    FURI: string;
    FMimeType: string;
    FBase64: boolean;
    FCharset: string;
    FValid: boolean;
    FURIPrefix: string;
    FForceMemoryStream: boolean;
    procedure FreeStream;
    procedure SetURI(const Value: string);
  public
    destructor Destroy; override;
    class function IsDataURI(const URI: string; out Colon: Integer): boolean; overload;
    class function IsDataURI(const URI: string): boolean; overload;

    { Force @link(Stream) and @link(ExtractStream) to return a TMemoryStream,
      that is always seekable and fully buffered in memory.
      Without this, they may return TBase64DecodingStream that may not be seekable. }
    property ForceMemoryStream: boolean
      read FForceMemoryStream write FForceMemoryStream;

    { The data URI that this class reads.

      When you set this, we read the beginning of URI.
      If this is a valid data URI, then we set @link(Valid) to @true,
      update MimeType, Base64, Charset, URIPrefix accordingly, and you can call
      @link(Stream) if you want to read actual contents.

      If this is not a valid data URI, then we set @link(Valid) to @false,
      make appropriate warning through WritelnWarning,
      and reset MimeType, Base64, Charset, URIPrefix to some default values. }
    property URI: string read FURI write SetURI;
    property Valid: boolean read FValid;
    property MimeType: string read FMimeType;
    property Base64: boolean read FBase64;
    property Charset: string read FCharset;
    { URI without the data, nice to show to user. }
    property URIPrefix: string read FURIPrefix;

    { Read the actual data contents. If the @link(URI) is not valid
      (includes the initial state when it's not set) then returns @nil.

      The important property of this reader is that no expensive
      encoding is done until you call this method. In particular,
      you can set URI, check MimeType, and if you see that MimeType
      is something not interesting for you (for example, maybe you require
      some image type) just don't call this method. Then nothing
      expensive will happen, e.g. data will not be base64-decoded without
      a need. }
    function Stream: TStream;

    { Get @link(Stream) and clear it.
      Makes the stream no longer owner by this TDataURI instance.

      Do not call @link(Stream) after calling @link(ExtractStream).
      Results are undefined.
      (Right now, another decoding stream will then be created,
      that will return the same thing as previous stream.
      But do not depend on it.) }
    function ExtractStream: TStream;
  end;

implementation

uses {$ifdef FPC} Base64,
  {$else} System.NetEncoding,
  {$endif}
  CastleURIUtils, CastleStringUtils, CastleClassUtils, CastleLog;

{ TODO: We treat non-base64 data verbatim, not interpreting %xx hex encoding
  inside. }

procedure TDataURI.FreeStream;
begin
  FreeAndNil(FStream);
end;

destructor TDataURI.Destroy;
begin
  FreeStream;
  inherited;
end;

class function TDataURI.IsDataURI(const URI: string; out Colon: Integer): boolean;
begin
  Result := URIProtocolIs(URI, 'data', Colon);
end;

class function TDataURI.IsDataURI(const URI: string): boolean;
var
  Colon: Integer; { ignored }
begin
  Result := URIProtocolIs(URI, 'data', Colon);
end;

procedure TDataURI.SetURI(const Value: string);
var
  ValidMimeType, ValidCharset: string;
  ValidBase64: boolean;
  PosBegin, PosNow, Colon: Integer;
  Part: string;
begin
  FreeStream;

  FURI := Value;

  { default values }
  FValid := false;
  FMimeType := 'text/plain'; { default mime-type when not specified }
  FBase64 := false; { default encoding is not base64 }
  FCharset := 'US-ASCII'; { default charset }
  FURIPrefix := '';

  if not IsDataURI(URI, Colon) then
  begin
    WritelnWarning('Data URI', 'Not a data URI scheme');
    Exit;
  end;

  ValidMimeType := FMimeType;
  ValidCharset := FCharset;
  ValidBase64 := FBase64;

  { First Colon characters were already parsed by URIProtocolIs as "data:".
    Read mime-type now. }

  PosBegin := Colon + 1;
  PosNow := PosBegin;
  while (PosNow <= Length(Value)) and
        (Value[PosNow] <> ';') and
        (Value[PosNow] <> ',') and
        (Value[PosNow] <> ' ') do
    Inc(PosNow);

  if PosBegin < PosNow then
    ValidMimeType := CopyPos(Value, PosBegin, PosNow - 1);

  repeat
    { Now, we either stand on ";" (then read charset or base64 tag)
      or we stand on "," or " " (then read data and exit)
      or we have end of string (then error).

      Note that terminating with space is invalid, unfortunately
      http://www.web3d.org/x3d/content/examples/Basic/Shaders/_pages/page01.html
      uses it. }

    if PosNow > Length(Value) then
    begin
      WritelnWarning('Data URI', 'Unexpected end (expected ",")');
      Exit;
    end else
    if Value[PosNow] = ';' then
    begin
      PosBegin := PosNow + 1;
      PosNow := PosBegin;
      while (PosNow <= Length(Value)) and
            (Value[PosNow] <> ';') and
            (Value[PosNow] <> ',') and
            (Value[PosNow] <> ' ') do
        Inc(PosNow);

      Part := CopyPos(Value, PosBegin, PosNow - 1);
      if Part = 'base64' then
        ValidBase64 := true else
      if IsPrefix('charset=', Part) then
        ValidCharset := SEnding(Part, Length('charset=') + 1) else
        WritelnWarning('Data URI', Format('Invalid part "%s" (expected "base64" or "charset=...")', [Part]));
    end else
    if Value[PosNow] in [',', ' '] then
    begin
      if Value[PosNow] = ' ' then
        WritelnWarning('Data URI', 'Header terminated by space, which is invalid (you should terminate with a comma)');
      Break;
    end;
  until false;

  FURIPrefix := Copy(Value, 1, PosNow - 1);

  FValid := true;
  FMimeType := ValidMimeType;
  FBase64 := ValidBase64;
  FCharset := ValidCharset;

  StreamBegin := PosNow + 1;
end;

function TDataURI.Stream: TStream;
var
  MemStream: TMemoryStream;
  Contents: string;
  {$ifdef FPC}
  DecodingStream: TBase64DecodingStream;
  {$endif}
begin
  if Valid then
  begin
    if FStream = nil then
    begin
      Contents := SEnding(URI, StreamBegin);

      MemStream := MemoryStreamLoadFromString(Contents);

      if Base64 then
      begin
        {$ifdef FPC}
        DecodingStream := TBase64DecodingStream.Create(MemStream, bdmMIME);
        DecodingStream.SourceOwner := true;
        if ForceMemoryStream then
        begin
          FStream := TMemoryStream.Create;
          ReadGrowingStream(DecodingStream, FStream, true);
          FreeAndNil(DecodingStream);
        end else
          FStream := DecodingStream;
        {$else}
        FStream := TMemoryStream.Create;
        // TODO: should we call this repeatedly until the end?
        TNetEncoding.Base64.Decode(MemStream, FStream);
        FStream.Position := 0;
        FreeAndNil(MemStream);
        {$endif}
      end else
        FStream := MemStream;
    end;
    Result := FStream;
  end else
    Result := nil;
end;

function TDataURI.ExtractStream: TStream;
begin
  Result := Stream;
  FStream := nil;
end;

end.
