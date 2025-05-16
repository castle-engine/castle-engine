{%MainUnit castleuriutils.pas}
{
  Copyright 2025-2025 Michalis Kamburelis.

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
  { Filesystem stored ony in memory, e.g. for temporary data that should remain
    persistent only during the program execution.
    Filesystem is case-sensitive. }
  TCastleMemoryFileSystem = class
  private
    { Not strict private, needs to be accessed by
      TMemoryStreamSaveOnDestroy implementation. }
    type
      TMemoryStreamSaveOnDestroy = class(TMemoryStream)
        PathRelative: String;
        FileSystem: TCastleMemoryFileSystem;
        procedure BeforeDestruction; override;
      end;
      TMemoryStreamSaveOnDestroyList = {$ifdef FPC}specialize{$endif}
        TObjectList<TMemoryStreamSaveOnDestroy>;
    var
      FPendingStreams: TMemoryStreamSaveOnDestroyList;
    { Make Stream contents be new contents of the file PathRelative.
      At the end, Stream.Position is undefined. }
    procedure SetContents(const PathRelative: String; const Stream: TMemoryStream);
  strict private
    { Map path (path within the filesystem, not URL-encoded, so space is space,
      not %20) to the contents of the file (expressed as TMemoryStream). }
    Files: TStringList;
    // Non-empty means that RegisterUrlProtocol was called.
    FRegisteredUrlProtocol: String;

    { Convert URL trying to access this filesystem
      (thanks to the RegisterUrlProtocol mechanism)
      into a relative path within this filesystem.
      This relative path is not URL-encoded, so space is space,
      and it doesn't start with a slash. }
    class function UrlToRelativePath(const Url: String): String; static;

    { Make sure that relative path ends with a slash.
      Doesn't change '', so it doesn't make '' -> '/'.
      Otherwise, this is the simplest string operation, it doesn't
      really care if given PathRelative is URL or filename or whatever. }
    class function IncludeSlash(const PathRelative: String): String; static;

    function ReadUrlHandler(const Url: String; out MimeType: string): TStream;
    function WriteUrlHandler(const Url: String): TStream;
    function ExistsUrlHandler(const Url: String): TUriExists;
    procedure FindFilesUrlHandler(const UrlPath, Mask: String;
      const FileEvent: TFoundFileMethod; var StopSearch: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    { Register URL protocol to refer to this filesystem. }
    procedure RegisterUrlProtocol(const Protocol: String);
    { Unregister URL protocol. Also done automatically in destructor. }
    procedure UnregisterUrlProtocol;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleMemoryFileSystem implementation.
  Parts adapted from TCastleZip implementation. }

constructor TCastleMemoryFileSystem.Create;
begin
  inherited;
  Files := TStringList.Create;
  FPendingStreams := TMemoryStreamSaveOnDestroyList.Create(false);
end;

procedure TCastleMemoryFileSystem.BeforeDestruction;
var
  I: Integer;
begin
  if FPendingStreams <> nil then
  begin
    { To avoid causing access violations in case you free TCastleMemoryFileSystem
      before freeing TMemoryStreamSaveOnDestroy instances,
      we unassign from their FileSystem field and make a graceful warning. }
    for I := 0 to FPendingStreams.Count - 1 do
    begin
      WritelnWarning('Unfinished writing to TCastleMemoryFileSystem file "%s", aborting because TCastleMemoryFileSystem is destroyed', [
        FPendingStreams[I].PathRelative
      ]);
      FPendingStreams[I].FileSystem := nil;
    end;
  end;
end;

destructor TCastleMemoryFileSystem.Destroy;
var
  I: Integer;
begin
  UnregisterUrlProtocol;
  if Files <> nil then
  begin
    // free all TMemoryStream instances representing the files
    for I := 0 to Files.Count - 1 do
      Files.Objects[I].Free;
    FreeAndNil(Files);
  end;
  FreeAndNil(FPendingStreams);
  inherited;
end;

procedure TCastleMemoryFileSystem.RegisterUrlProtocol(const Protocol: String);
var
  P: TRegisteredProtocol;
begin
  if FRegisteredUrlProtocol <> '' then
    raise Exception.CreateFmt('Cannot register TCastleMemoryFileSystem as URL protocol "%s", already registered "%s"', [
      Protocol,
      FRegisteredUrlProtocol
    ]);
  FRegisteredUrlProtocol := Protocol;
  P := CastleDownload.RegisterUrlProtocol(Protocol);
  P.ReadEvent := {$ifdef FPC}@{$endif} ReadUrlHandler;
  P.WriteEvent := {$ifdef FPC}@{$endif} WriteUrlHandler;
  P.ExistsEvent := {$ifdef FPC}@{$endif} ExistsUrlHandler;
  P.FindFilesEvent := {$ifdef FPC}@{$endif} FindFilesUrlHandler;
end;

procedure TCastleMemoryFileSystem.UnregisterUrlProtocol;
begin
  if FRegisteredUrlProtocol <> '' then
  begin
    CastleDownload.UnregisterUrlProtocol(FRegisteredUrlProtocol);
    FRegisteredUrlProtocol := '';
  end;
  Assert(FRegisteredUrlProtocol = '');
end;

class function TCastleMemoryFileSystem.UrlToRelativePath(const Url: String): String;
var
  U: TURI;
begin
  U := ParseURI(Url);
  Result := PrefixRemove('/', U.Path + U.Document, false);
end;

class function TCastleMemoryFileSystem.IncludeSlash(const PathRelative: String): String;
begin
  Result := PathRelative;
  if (Result <> '') and
     (Result[Length(Result)] <> '/') then
    Result := Result + '/';
end;

function TCastleMemoryFileSystem.ReadUrlHandler(const Url: String; out MimeType: string): TStream;
var
  PathRelative: String;
  I: Integer;
  StoredMemoryStream: TMemoryStream;
begin
  PathRelative := UrlToRelativePath(Url);

  I := Files.IndexOf(PathRelative);
  if I = -1 then
    raise EDownloadError.CreateFmt('Cannot read URL "%s", path "%s" not found in TCastleMemoryFileSystem', [
      Url,
      PathRelative
    ]);

  StoredMemoryStream := Files.Objects[I] as TMemoryStream;
  Result := TMemoryStream.Create;
  Result.CopyFrom(StoredMemoryStream, 0);
  StoredMemoryStream.Position := 0;
  Result.Position := 0;

  { Determine mime type from Url, which practically means:
    determine content type from filename extension. }
  MimeType := UriMimeType(Url);
end;

procedure TCastleMemoryFileSystem.SetContents(const PathRelative: String; const Stream: TMemoryStream);

  function TotalSize: UInt64;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to Files.Count - 1 do
      Result := Result + (Files.Objects[I] as TMemoryStream).Size;
  end;

var
  I: Integer;
  StoredMemoryStream: TMemoryStream;
begin
  I := Files.IndexOf(PathRelative);
  if I = -1 then
  begin
    StoredMemoryStream := TMemoryStream.Create;
    Files.AddObject(PathRelative, StoredMemoryStream);
  end else
  begin
    StoredMemoryStream := Files.Objects[I] as TMemoryStream;
    StoredMemoryStream.Clear;
  end;

  // copy Stream -> StoredMemoryStream
  StoredMemoryStream.CopyFrom(Stream, Stream.Size);
  StoredMemoryStream.Position := 0;

  WritelnLog('Stored "%s" (size %s) in TCastleMemoryFileSystem, total size of TCastleMemoryFileSystem now: %s', [
    PathRelative,
    SizeToStr(Stream.Size),
    SizeToStr(TotalSize)
  ]);
end;

function TCastleMemoryFileSystem.WriteUrlHandler(const Url: String): TStream;
var
  PathRelative: String;
  ResultStream: TMemoryStreamSaveOnDestroy;
begin
  PathRelative := UrlToRelativePath(Url);

  ResultStream := TMemoryStreamSaveOnDestroy.Create;
  ResultStream.FileSystem := Self;
  ResultStream.PathRelative := PathRelative;
  FPendingStreams.Add(ResultStream);
  Result := ResultStream;
end;

function TCastleMemoryFileSystem.ExistsUrlHandler(const Url: String): TUriExists;
var
  PathRelative, PathRelativeAsDirectory: String;
  I: Integer;
begin
  PathRelative := UrlToRelativePath(Url);
  if PathRelative = '' then
    Exit(ueDirectory); // top directory

  // calculate PathRelativeAsDirectory, ending with /
  PathRelativeAsDirectory := IncludeSlash(PathRelative);

  for I := 0 to Files.Count - 1 do
  begin
    if Files[I] = PathRelative then
    begin
      Result := ueFile;
      Exit;
    end else
    if IsPrefix(PathRelativeAsDirectory, Files[I]) then
    begin
      Result := ueDirectory;
      Exit;
    end;
  end;

  Result := ueNotExists;
end;

procedure TCastleMemoryFileSystem.FindFilesUrlHandler(
  const UrlPath, Mask: String;
  const FileEvent: TFoundFileMethod; var StopSearch: Boolean);
const
  IgnoreCase = false;
var
  UrlPathSlash, PathRelative: String;
  FileInfo: TFileInfo;
  AlreadyReportedSubdirectories: TStringList;
  I, SlashPos: Integer;
  NamePart: String;
begin
  UrlPathSlash := UriIncludeSlash(UrlPath);
  PathRelative := UrlToRelativePath(UrlPathSlash);
  AlreadyReportedSubdirectories := TStringList.Create;
  try
    for I := 0 to Files.Count - 1 do
    begin
      if IsPrefix(PathRelative, Files[I], IgnoreCase) then
      begin
        NamePart := SEnding(Files[I], Length(PathRelative) + 1);
        SlashPos := Pos('/', NamePart);
        if SlashPos = 0 then
        begin
          // found file inside PathRelative
          if IsWild(NamePart, Mask, IgnoreCase) then
          begin
            FileInfo := Default(TFileInfo);
            FileInfo.Name := NamePart;
            FileInfo.Url := UrlPathSlash + UrlEncode(NamePart);
            FileInfo.Directory := false;
            FileInfo.Size := (Files.Objects[I] as TMemoryStream).Size;
            FileEvent(FileInfo, StopSearch);
            if StopSearch then
              Exit;
          end;
        end else
        begin
          // found directory inside PathRelative, but it may occur multiple times
          NamePart := Copy(NamePart, 1, SlashPos - 1);
          if IsWild(NamePart, Mask, IgnoreCase) and
            (AlreadyReportedSubdirectories.IndexOf(NamePart) = -1) then
          begin
            AlreadyReportedSubdirectories.Add(NamePart);

            FileInfo := Default(TFileInfo);
            FileInfo.Name := NamePart;
            FileInfo.Url := UrlPathSlash + UrlEncode(NamePart);
            FileInfo.Directory := true;
            FileInfo.Size := 0;
            FileEvent(FileInfo, StopSearch);
            if StopSearch then
              Exit;
          end;
        end;
      end;
    end;
  finally FreeAndNil(AlreadyReportedSubdirectories); end;
end;

{ TCastleMemoryFileSystem.TMemoryStreamSaveOnDestroy -------------------------------------- }

procedure TCastleMemoryFileSystem.TMemoryStreamSaveOnDestroy.BeforeDestruction;
begin
  if FileSystem <> nil then
  begin
    FileSystem.FPendingStreams.Remove(Self);
    Position := 0;
    FileSystem.SetContents(PathRelative, Self);
    FileSystem := nil;
  end;
end;

{$endif read_implementation}
