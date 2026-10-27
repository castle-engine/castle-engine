{
  Copyright 2013-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
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

uses SysUtils, Classes,
  {$ifdef FPC} CastleAndroidInternalAssetManager
  {$else} Androidapi.AssetManager
  {$endif},
  CastleUriUtils, CastleFindFiles;

type
  EAssetReadError = class(EReadError);
  EAssetNotFound = class(EAssetReadError);

  TReadAssetStream = class(TStream)
  private
    Asset: PAAsset;
    FPosition: Int64;
  protected
    function GetSize: Int64; override;

    {$ifdef FPC}
    { Current position in Stream.
      Note: Delphi doesn't need this, to get position it always does
      "Result := Seek(0, soCurrent)" which our Seek also handles OK. }
    function GetPosition: Int64; override;
    {$endif}

    { This stream doesn't support setting size.
      (All other versions of SetSize also call this.)
      @raises(EStreamNotImplementedSetSize Always.) }
    procedure SetSize(NewSize: Longint); override;
  public
    { Open a stream for an asset on given path.
      The path should be a valid Android asset path,
      like @code(images/my_texture.png). }
    constructor Create(Path: AnsiString);
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

{ Check for existence of an Android asset at the given path. }
function AndroidAssetExists(const AssetPath: string): TUriExists;

{ List files in Android AssetPath matching the given Mask.

  Known limitation: This reports only files, not subdirectories,
  as Android asset API doesn't allow to enumerate subdirectories.
  For example, if you have in assets

    level1/level.gltf
    level1/textures/my_texture.png

  and you do FindFilesInAndroidAssets('level1', '*', ...),
  it will report 'level.gltf', but it will not report subdirectory 'textures'.
  Consequently, recursive searching by @link(CastleFindFiles.FindFiles) will
  not visit the 'textures' subdirectory and it will not list 'my_texture.png'
  inside.

  There seems no reasonable solution to this, as long as we use Android asset API.

  To enable listing subdirectories, you must generate "CastleDataInformation.xml"
  and include it in your data. Our engine will automatically use it then
  for UriExists and FindFiles, and listing subdirectories will work.

  The "CastleDataInformation.xml" is automatically generated and packed
  when using our build tool to package your application for Android with FPC.
  It is not yet generated and used for Android with Delphi -- you need to
  generate it manually, following code from
  "procedure GenerateDataInformation(const CurrentDataPath: String);"
  in tools/build-tool/code/toolpackage.pas .
  TODO: We plan to make it easier, by generating "CastleDataInformation.xml"
  automatically when building from Delphi IDE. }
procedure FindFilesInAndroidAssets(const AssetPath, Mask: String;
  const FileEvent: TFoundFileMethod; var StopSearch: Boolean);

implementation

uses CastleAndroidInternalLog,
  { Needed for IsWild with FPC } StrUtils,
  {$ifdef FPC} CastleAndroidNativeAppGlue, {$else} Androidapi.AppGlue, {$endif}
  CastleClassUtils, CastleLog, CastleStringUtils, URIParser;

function GetMainAssetManager: PAAssetManager;
begin
  Result :=
    {$ifdef FPC} AndroidMainApp^.Activity^.AssetManager
    {$else} TAndroidApplicationGlue.Current.NativeActivity^.AssetManager
    {$endif};
end;

{ TReadAssetStream ----------------------------------------------------------- }

constructor TReadAssetStream.Create(Path: AnsiString);
begin
  inherited Create;
  if ExtractFileExt(Path) = '.gz' then
  begin
    // WritelnLog('Assets', 'Trying to access Android asset with .gz extension, stripping the .gz (because Android tools strip them too when packing the .apk file): %s',
    //   [Path]);
    Path := ChangeFileExt(Path, '');
  end;
  Asset := AAssetManager_open(GetMainAssetManager, PAnsiChar(Path), AASSET_MODE_STREAMING);
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
  FPosition := FPosition + Result;
end;

function TReadAssetStream.GetSize: Int64;
begin
  Result := AAsset_getLength(Asset);
  { Take only the least-significant 32 bits of result, because
    on some Androids the higher 32-bits are nonsense (Sony Ericsson,
    Android 2.3.4, WT191l). }
  Result := Result and Int64(High(UInt32));
end;

{$ifdef FPC}
function TReadAssetStream.GetPosition: Int64;
begin
  Result := FPosition;
end;
{$endif}

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
    Exit(FPosition);

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
      WritelnWarning('Too many / at the beginning of URL "%s". This usually means you used ''castle-data://xxx'', while you should use ''castle-data:/xxx''.',
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
  FillChar(U, SizeOf(U), 0);
  U.Protocol := 'castle-android-assets';
  U.Path := '/' + AssetPath; // AssetPath does not start with slash
  Result := EncodeURI(U);
end;

function AndroidAssetExists(const AssetPath: string): TUriExists;
var
  AssetManager: PAAssetManager;
  ADir: PAAssetDir;
  FilePtr: PAnsiChar;
  PathAnsi, PathDir, PathBaseName, FoundPathBaseName: AnsiString;
begin
  PathDir := ExtractFileDir(AssetPath);
  PathBaseName := ExtractFileName(AssetPath);

  AssetManager := GetMainAssetManager;
  ADir := AAssetManager_openDir(AssetManager, PAnsiChar(PathDir));
  if ADir <> nil then
  begin
    try
      FilePtr := AAssetDir_getNextFileName(ADir);
      while FilePtr <> nil do
      begin
        FoundPathBaseName := FilePtr;
        if FoundPathBaseName = PathBaseName then
          Exit(ueFile);
        FilePtr := AAssetDir_getNextFileName(ADir);
      end;
    finally AAssetDir_close(ADir) end;
  end;

  { Check is it maybe directory }
  PathAnsi := AssetPath;
  ADir := AAssetManager_openDir(AssetManager, PAnsiChar(PathAnsi));
  if ADir <> nil then
  begin
    AAssetDir_close(ADir);
    Exit(ueDirectory);
  end;

  Result := ueNotExists;
end;

procedure FindFilesInAndroidAssets(const AssetPath, Mask: String;
  const FileEvent: TFoundFileMethod; var StopSearch: Boolean);
var
  AssetManager: PAAssetManager;
  ADir: PAAssetDir;
  Asset: PAAsset;
  FilePtr: PAnsiChar;
  PathDir, FilePath: AnsiString;
  FileInfo: TFileInfo;
begin
  { Asset paths use slash as separator, and the directory name passed
    to AAssetManager_openDir should not end with slash.
    Empty string means the top-level assets directory.

    We overuse UriExcludeSlash for this purpose, it handles /,
    and doesn't really assume it is an URL. }
  PathDir := UriExcludeSlash(AssetPath);

  AssetManager := GetMainAssetManager;
  ADir := AAssetManager_openDir(AssetManager, PAnsiChar(PathDir));
  if ADir = nil then
    Exit;
  try
    { Note: AAssetDir_getNextFileName returns only files, not subdirectories.
      This is a limitation of the Android NDK asset API,
      so we cannot report subdirectories here. }
    FilePtr := AAssetDir_getNextFileName(ADir);
    while FilePtr <> nil do
    begin
      FileInfo := Default(TFileInfo);
      FileInfo.Name := FilePtr;
      if IsWild(FileInfo.Name, Mask, false) then
      begin
        if PathDir <> '' then
          FilePath := PathDir + '/' + FileInfo.Name
        else
          FilePath := FileInfo.Name;
        FileInfo.Url := AssetPathToURI(FilePath);
        FileInfo.Directory := false;
        FileInfo.Symlink := false;

        { Opening asset in AASSET_MODE_UNKNOWN to query length is cheap,
          it doesn't read the contents. }
        Asset := AAssetManager_open(AssetManager, PAnsiChar(FilePath), AASSET_MODE_UNKNOWN);
        if Asset <> nil then
        begin
          { Take only the least-significant 32 bits, like TReadAssetStream.GetSize. }
          FileInfo.Size := AAsset_getLength(Asset) and Int64(High(UInt32));
          AAsset_close(Asset);
        end;

        FileEvent(FileInfo, StopSearch);
        if StopSearch then
          Exit;
      end;
      FilePtr := AAssetDir_getNextFileName(ADir);
    end;
  finally AAssetDir_close(ADir) end;
end;

end.
