{
  Copyright 2021-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Base64 decoding, with API compatible to FPC Base64 unit. }
unit Base64;

interface

uses SysUtils, Classes, System.NetEncoding;

type
  TBase64DecodingMode = (bdmStrict, bdmMIME);

  { Decode base64 stream.

    Note: Underneath, in Delphi, the whole decoding (so, reading Source)
    is done immediately at constructor.

    Note: in Delphi, AMode is ignored.

    The @bold(input (base64 string of characters) is assumed to be a String),
    whatever "String" means for this compiler.
    With Delphi, it means it is UTF-16 string, as String=UnicodeString.

    The @bold(output is just binary data). What it is (and what encoding it has)
    depends solely on what was encoded.
    So it may be UTF-8 characters or UTF-16 characters or something
    else -- this does not depend on whether this is compiled with Delphi
    (String=UnicodeString, UTF-16) or FPC (String=AnsiString, UTF-8,
    at least without DelphiUnicode mode)). }
  TBase64DecodingStream = class(TMemoryStream)
  strict private
    FSource: TStream;
    FSourceOwner: Boolean;
  public
    constructor Create(const ASource: TStream); overload;
    constructor Create(const ASource: TStream; const AMode: TBase64DecodingMode); overload;
    destructor Destroy; override;
    property SourceOwner: Boolean read FSourceOwner write FSourceOwner default false;
  end;

  { Encode base64 stream.

    Note: Underneath, in Delphi, the whole encoding (so, writing to Destination)
    is done at destructor.

    The @bold(output (base64 string of characters) is a String),
    whatever "String" means for this compiler.
    With Delphi, it means it is UTF-16 string, as String=UnicodeString.

    The @bold(input is just binary data, whatever you want to encode).
    It may be UTF-8 characters in Utf8String, e.g. you can write
    them using @link(CastleClassUtils.WriteStr). }
  TBase64EncodingStream = class(TMemoryStream)
  strict private
    FDestination: TStream;
    FDestinationOwner: Boolean;
  public
    constructor Create(const ADestination: TStream);
    destructor Destroy; override;
    property DestinationOwner: Boolean read FDestinationOwner write FDestinationOwner default false;
  end;

implementation

{ TBase64DecodingStream ------------------------------------------------------ }

constructor TBase64DecodingStream.Create(const ASource: TStream);
begin
  inherited Create;
  FSource := ASource;
  while TNetEncoding.Base64.Decode(FSource, Self) <> 0 do ;
  Position := 0;
end;

constructor TBase64DecodingStream.Create(const ASource: TStream;
  const AMode: TBase64DecodingMode);
begin
  Create(ASource);
end;

destructor TBase64DecodingStream.Destroy;
begin
  if SourceOwner then
    FreeAndNil(FSource);
  inherited;
end;

{ TBase64EncodingStream ------------------------------------------------------ }

constructor TBase64EncodingStream.Create(const ADestination: TStream);
begin
  inherited Create;
  FDestination := ADestination;
end;

destructor TBase64EncodingStream.Destroy;
begin
  if FDestination <> nil then
  begin
    Position := 0;
    while TNetEncoding.Base64.Encode(Self, FDestination) <> 0 do ;
  end;

  if DestinationOwner then
    FreeAndNil(FDestination);
  inherited;
end;

end.
