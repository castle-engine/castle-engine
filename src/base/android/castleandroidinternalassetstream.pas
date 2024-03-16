{
  Copyright 2013-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Reading Android asset files as streams. }
unit CastleAndroidInternalAssetStream;

{$I castleconf.inc}

interface

uses SysUtils, Classes, CastleAndroidInternalAssetManager;

type
  EAssetReadError = class(EReadError);
  EAssetNotFound = class(EAssetReadError);

  TReadAssetStream = class(TStream)
  private
    Asset: PAAsset;
    FPosition: Int64;
  protected
    function GetSize: Int64; override;
    function GetPosition: Int64; override;

    { This stream doesn't support setting size.
      (All other versions of SetSize also call this.)
      @raises(EStreamNotImplementedSetSize Always.) }
    procedure SetSize(NewSize: Longint); override;
  public
    { Open a stream for an asset on given path.
      The path should be a valid Android asset path,
      like @code(images/my_texture.png). }
    constructor Create(Path: string);
    destructor Destroy; override;

    { This stream doesn't support seeking.
      (SetPosition and all other versions of Seek also call this.)
      @raises(EStreamNotImplementedSeek Always.) }
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;

    { This stream doesn't support writing.
      (WriteBuffer also calls this.)
      @raises(EStreamNotImplementedWrite Always.) }
    function Write(const Buffer; Count: Longint): Longint; override;

    function Read(var Buffer; Count: Longint): Longint; override;
  end;

{ Assuming that this is @code(castle-android-assets:/xxx/yyy) URL, convert it to an asset path
  @code(xxx/yyy). Does percent-decoding along the way. }
function URIToAssetPath(const URI: string): string;

function AssetPathToURI(const AssetPath: string): string;

implementation

uses CastleAndroidInternalLog, CastleAndroidNativeAppGlue,
  CastleClassUtils, CastleLog, CastleStringUtils, URIParser;

constructor TReadAssetStream.Create(Path: string);
var
  AssetManager: PAAssetManager;
begin
  inherited Create;
  if ExtractFileExt(Path) = '.gz' then
  begin
    // WritelnLog('Assets', 'Trying to access Android asset with .gz extension, stripping the .gz (because Android tools strip them too when packing the .apk file): %s',
    //   [Path]);
    Path := ChangeFileExt(Path, '');
  end;
  AssetManager := AndroidMainApp^.Activity^.AssetManager;
  Asset := AAssetManager_open(AssetManager, PChar(Path), AASSET_MODE_STREAMING);
  if Asset = nil then
    raise EAssetNotFound.CreateFmt('Android asset "%s" not found', [Path]);
end;

destructor TReadAssetStream.Destroy;
begin
  if Asset <> nil then
    AAsset_close(Asset);
  inherited;
end;

function TReadAssetStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := AAsset_read(Asset, @Buffer, Count);
  if Result < 0 then
    raise EAssetReadError.Create('Error when reading asset data stream');
  FPosition += Result;
end;

function TReadAssetStream.GetSize: Int64;
begin
  Result := AAsset_getLength(Asset);
  { Take only the least-significant 32 bits of result, because
    on some Androids the higher 32-bits are nonsense (Sony Ericsson,
    Android 2.3.4, WT191l). }
  Result := Result and Int64(High(UInt32));
end;

function TReadAssetStream.GetPosition: Int64;
begin
  Result := FPosition;
end;

procedure TReadAssetStream.SetSize(NewSize: Longint);
begin
  raise EStreamNotImplementedSetSize.Create(
    'TReadAssetStream.SetSize not supported');
end;

function TReadAssetStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if ( (Origin = soBeginning) and (Offset = FPosition) ) or
     ( (Origin = soCurrent  ) and (Offset = 0) ) then
    { nothing needs to be done, ok }
    Exit;

  raise EStreamNotImplementedSeek.Create('TReadAssetStream.Seek not supported');
  Result := 0; // just to get rid of warning
end;

function TReadAssetStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EStreamNotImplementedWrite.Create('TReadAssetStream.Write not supported');
  Result := 0; // just to get rid of warning
end;

{ global routines ------------------------------------------------------------ }

function URIToAssetPath(const URI: string): string;
var
  U: TURI;
  FixedURI: String;
begin
  U := ParseURI(URI);
  if SameText(U.Protocol, 'assets') or
     SameText(U.Protocol, 'castle-android-assets') then
  begin
    // Try to work (but with warning) even in case of some invalid URI
    if IsPrefix('castle-android-assets://', URI, true) then
    begin
      WritelnWarning('Too many / at the beginning of URL "%s". This usually means you used ApplicationData(''/xxx'') or ''castle-data://xxx'', while you should use ApplicationData(''xxx'') or ''castle-data:/xxx''.',
        [URI]);
      FixedURI := 'castle-android-assets:/' + PrefixRemove('castle-android-assets://', URI, true);
      U := ParseURI(FixedURI);
    end;

    Result := PrefixRemove('/', U.Path + U.Document, false)
  end else
    raise Exception.CreateFmt('URI does not have protocol "castle-android-assets:" or "assets:", cannot convert to Android asset path: %s, protocol %s',
      [URI, U.Protocol]);
end;

function AssetPathToURI(const AssetPath: string): string;
var
  U: TURI;
begin
  FillByte(U, SizeOf(U), 0);
  U.Protocol := 'castle-android-assets';
  U.Path := '/' + AssetPath; // AssetPath does not start with slash
  Result := EncodeURI(U);
end;

end.
