{
  Copyright 2019-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ ZIP files handling (@link(TCastleZip)). }
unit CastleZip;

interface

uses SysUtils, Classes,
  {$ifdef FPC} Zipper {$else} System.Zip {$endif};

type
  EZipNotOpen = class(Exception);

  { A collection of files inside a ZIP archive.

    Use this to:

    @unorderedList(
      @item(Read ZIP files from any TStream or
        URL supported by Castle Game Engine.

        Example of reading:

        @longCode(
        var
          Zip: TCastleZip;
        begin
          Zip := TCastleZip.Create;
          try
            Zip.Open('castle-data:/my_textures.zip');
            MyStream := Zip.Read('level1/grass.png');
          finally FreeAndNil(Zip) end;
        end;
        )
      )

      @item(Modify and write ZIP files, consisting with above.

        TODO: For now, with both FPC and Delphi, ZIP can only be open for reading
        or writing (when OpenEmpty). So you cannot open existing ZIP
        and then modify it.

        Example of writing:

        @longCode(
        var
          Zip: TCastleZip;
        begin
          Zip := TCastleZip.Create;
          try
            Zip.OpenEmpty;
            Zip.Write('level1/grass.png', 'file:///home/myuser/my_textures/level1/grass.png');
            Zip.Save('file:///home/myuser/my_textures.zip');
          finally FreeAndNil(Zip) end;
        end;
        )
      )

      @item(Optionally register a URL handler, to read files inside a ZIP
        archive by just accesing URL with the given prefix.
        See @link(RegisterUrlProtocol) for example.

        TODO: add write support this way too. For now, it only works
        for reading, when the ZIP was opened for reading.
      )

      @item(Have uniform API for both FPC and Delphi (they have different
        support in their standard units).)
    )

    Open a ZIP archive by @link(Open). Later close it by @link(Close).
    While the ZIP file is open, read files using @link(Read),
    add files using @link(Write). }
  TCastleZip = class
  strict private
    // Non-empty means that RegisterUrlProtocol was called.
    FRegisteredUrlProtocol: String;
    FFiles: TStringList;
    {$ifdef FPC}
    { When open for reading, we always have FReadStream <> nil and UnZipper <> nil.
      We never assign UnZipper.FileName, instead relying on own URL -> TStream
      reading. }
    UnZipper: TUnZipper;
    FReadStream: TStream;
    FOwnsReadStream: Boolean;
    { When open for writing, we always have Zipper <> nil. }
    Zipper: TZipper;
    { Handler for TUnZipper.OnOpenInputStream. }
    procedure OpenInputStream(Sender: TObject; var AStream: TStream);
    { Handler for TUnZipper.OnCloseInputStream. }
    procedure CloseInputStream(Sender: TObject; var AStream: TStream);
    { Update FFiles from UnZipper.Entries or Zipper.Entries. }
    procedure UpdateFiles;
    {$else}
    { When open, we *may* have FZipStream <> nil.
      Only when FZipStream <> nil, the FOwnsZipStream may be true
      to indicate should we free FZipStream in Close. }
    FZipStream: TStream;
    FOwnsZipStream: Boolean;
    ZipFile: TZipFile;
    function PathToZip(const PathInZip: String): String;
    { Update FFiles from ZipFile.FileCount,FleName[]. }
    procedure UpdateFiles;
    {$endif}
    { Handler given to CastleDownload.RegisterUrlProtocol. }
    function ReadUrlHandler(const Url: String; out MimeType: string): TStream;
    function GetFiles: TStrings;
    function IsOpenRead: Boolean;
    function IsOpenWrite: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    { Open the ZIP archive from given URL.

      Depending on the implementation details, the opened ZIP file
      may need to exist on disk while we read from it, until we @link(Close) it. }
    procedure Open(const Url: String); overload;

    { Open the ZIP archive from given TStream instance.
      The TStream instance must exist as long as the ZIP file is open.
      We can free it automatically at close if OwnsStream is @true. }
    procedure Open(const Stream: TStream; const OwnsStream: boolean); overload;

    { Create a new, empty ZIP archive in memory.
      You can then add files to it using @link(Write) and save it using @link(Save). }
    procedure OpenEmpty;

    { Close the ZIP archive. This releases all resources
      (avoids keeping in memory the ZIP contents),
      but afterwards you cannot @link(Read) files from ZIP anymore.

      There's usually no need to call this, as the destructor or opening
      a new ZIP archive using @link(Open) will first close the existing archive.
      It is only useful if you want to release the resources related to your ZIP
      earlier, which is in turn only a concern if you really deal with huge
      (e.g. gigabytes) ZIP files. }
    procedure Close;

    { Was @link(Open) called and succeded (without any exception)
      and we didn't yet call @link(Close). }
    function IsOpen: Boolean;

    { Read a file from the ZIP archive.
      PathInZip should be a relative path within the zip archive,
      with parts separated by slashes,
      like 'images/my_image.png'.

      The caller is responsible for freeing the returned stream.

      The stream is positioned at the beginning,
      so you can read from it immediately.

      The stream contents are guaranteed to be valid independently of the
      @link(TCastleZip) instance lifetime and independently of whether you
      will close the ZIP archive. }
    function Read(const PathInZip: String): TStream;

    { Register read handler for a given URL protocol,
      to access the ZIP contents by just reading from a given URL protocol
      using @link(Download) routine (or any routine on top of it).

      For example, you can register a URL handler for 'my-textures' prefix:

      @longCode(
        var
          Zip: TCastleZip;
        begin
          Zip := TCastleZip.Create;
          try
            Zip.Open('castle-data:/my_textures.zip');
            Zip.RegisterUrlProtocol('my-textures');
            MyStream := Download('my-textures:/level1/grass.png');
          finally FreeAndNil(Zip) end;
        end;
      )

      Full example usage in the @url(https://github.com/castle-engine/castle-engine/tree/master/examples/network/custom_url_handler
      examples/network/custom_url_handler).

      ZIP doesn't have to be @link(IsOpen) when calling this.
      In fact, you can close and reopen the ZIP file while the URL handler
      continues to be registered.
      But it has to be open when you actually read using the indicated URL
      protocol. }
    procedure RegisterUrlProtocol(const Protocol: String);

    { Unregister the URL protocol handler registered by @link(RegisterUrlProtocol).
      This is automatically done when destroying
      the @link(TCastleZip) instance. }
    procedure UnregisterUrlProtocol;

    { Create a new file entry inside the ZIP.
      If the given path already existed in the ZIP, it is overwritten.

      There are 2 overloads, one taking a TStream and one taking a URL of the
      file to be added.

      @unorderedList(
        @item(For overload with URL:

          We will read the URL contents and add them to the ZIP.

          We will also use some of the "metadata" of the URL,
          which in practice now means: if the URL indicates a local file,
          we will look at the file modification time and Unix permissions
          and store them alongside the ZIP entry.)

        @item(For overload with TStream:

          We expect the provided stream to be positioned at the beginning,
          and we will read from it until the end. We assume nothing
          about the given TStream, in particular we don't assume that it is seekable,
          we don't assume it has useful TStream.Size. We will process it using
          @link(ReadGrowingStream).

          Note that there's no way to provide the "metadata" of the file
          (like file modification, Unix permissions)
          when you provide a stream. For this reason we advise to use the
          overload with URL whenever possible.

          When OwnsStream, we will take care of freeing the Stream.
          The work may be more efficient then (because when OwnsStream=false
          we may have to do a copy of stream, to have it available for later),
          so use OwnsStream = @true when possible.
        )
      )

      After this is called, the new file entry appears in the @link(Files).

      The ZIP archive is not saved to disk until you call @link(Save).
      If you call @link(Close) without calling @link(Save), the changes
      are lost. @link(Save) is never called automatically. }
    procedure Write(const PathInZip: String; const Stream: TStream; const OwnsStream: Boolean); overload;
    procedure Write(const PathInZip: String; const Url: String); overload;

    { Save the currently open ZIP archive (with all modificaiotns
      done by @link(Write)) to the given URL. }
    procedure Save(const Url: String);

    { Read-only list of all files within the ZIP archive.
      They are relative paths within the zip archive,
      with parts separated by slashes.

      The directories, by itself, are never listed here.

      Note: This is in contrast to both Delphi TZipFile.FileName and
      FPC TUnZipper.Entries, where this is undefined, because ZIP files
      @url(https://unix.stackexchange.com/questions/743511/why-are-directories-sometimes-listed-explicitly-in-zip-files
      may list directories explicitly, but don't have to).
      For TCastleZip.Files, we explicitly guarantee that directories
      are never listed here.

      All these values are valid as arguments for @link(Read).

      No order of the contents is guaranteed.

      @raises EZipNotOpen If the ZIP archive is not open. }
    property Files: TStrings read GetFiles;

    (* TODO: maybe, if needed:

    { Extract all files from the ZIP archive into the given path,
      recreating the subdirectory structure of the ZIP inside TargetPath.

      The TargetPathUrl should be an URL of an existing directory.
      It may but doesn't have to end with slash.
      It will be combined with the relative URLs of the files
      inside ZIP using @link(CombineUri).

      Any URL protocol where we can write files (using @link(UrlSaveStream))
      is allowed. }
    procedure ExtractAl(const TargetPathUrl: String);
    *)
  end;

{ Create a .zip file named ZipUrl
  containing all files (recursively) in the DirectoryUrl.

  As usual in our engine, both parameters should be URLs.
  Although we also make reasonable effort to handle filenames when given.

  Directory may but doesn't have to end with trailing /.
  The directory URL must, right now, resolve to a local directory (file URL),
  we cannot package other URL types.

  SingleTopLevelDirectory meaning:

  @unorderedList(
    @item(When SingleTopLevelDirectory = @true:

      The directory is added to the zip file
      as a top-level directory inside zip.
      E.g. if directory is 'file:///home/michalis/mydir', then the zip file will contain
      as top level 'mydir'. There shall be no trace of '/home/michalis/' in
      the resulting zp file.)

    @item(When SingleTopLevelDirectory = @false:

      Then only contents of the directory
      are inside the zip, and so it can have multiple top-level entries.)
  )

  We gracefully handle the case when ZIP file (ZipUrl) is inside the directory,
  by not packing the ZIP in this case in itself (and making a warning),
  to avoid possible reading and writing the file at the same time. }
procedure ZipDirectory(const ZipUrl: String; DirectoryUrl: String;
  const SingleTopLevelDirectory: Boolean = true);

implementation

uses URIParser,
  CastleUriUtils, CastleDownload, CastleFilesUtils, CastleStringUtils,
  CastleClassUtils, CastleFindFiles, CastleLog, CastleUtils;

{$ifdef FPC}

{ TUnzipFileHelper ----------------------------------------------------------- }

type
  { Helper for FPC that implements TUnZipper OnCreateStream event. }
  TUnzipFileHelper = class
    Stream: TStream;
    procedure UnzipCreateStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
    procedure UnzipDoneStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
  end;

procedure TUnzipFileHelper.UnzipCreateStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
begin
  AStream := Stream;
end;

procedure TUnzipFileHelper.UnzipDoneStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
begin
  { Do nothing.
    We need this event, because TUnZipper.CloseOutput by default
    (when on OnDoneStream assigned) frees the stream at the end of work. }
end;

{ TCastleZip using FPC -------------------------------------------------------- }

{ FPC version uses TUnZipper,
  https://www.freepascal.org/docs-html/fcl/zipper/tunzipper.html }

function TCastleZip.IsOpen: Boolean;
begin
  Result := (UnZipper <> nil) or (Zipper <> nil);
end;

function TCastleZip.IsOpenRead: Boolean;
begin
  Result := UnZipper <> nil;
end;

function TCastleZip.IsOpenWrite: Boolean;
begin
  Result := Zipper <> nil;
end;

procedure TCastleZip.Open(const Url: String);
begin
  Open(Download(Url), true);
end;

procedure TCastleZip.Open(const Stream: TStream; const OwnsStream: boolean);
begin
  Close;

  FReadStream := Stream;
  FOwnsReadStream := OwnsStream;

  UnZipper := TUnZipper.Create;
  UnZipper.UseUTF8 := true;
  UnZipper.OnOpenInputStream := @OpenInputStream;
  UnZipper.OnCloseInputStream := @CloseInputStream;
  UnZipper.Examine; // read UnZipper.Entries, used by UpdateFiles

  UpdateFiles;
end;

procedure TCastleZip.OpenEmpty;
begin
  Close;

  Zipper := TZipper.Create;
  { Store filenames using UTF-8 in zip,
    see https://wiki.lazarus.freepascal.org/paszlib#TZipper
    This matters only for zip writing when filename has non-ASCII characters. }
  {$ifndef VER3_0} // only for FPC >= 3.2.0
  Zipper.UseLanguageEncoding := true;
  {$endif}
end;

procedure TCastleZip.UpdateFiles;
var
  I: Integer;
begin
  FFiles.Clear;

  // zero or one of Zipper, UnZipper may be assigned
  Assert(not ((Zipper <> nil) and (UnZipper <> nil)));

  if Zipper <> nil then
  begin
    for I := 0 to Zipper.Entries.Count - 1 do
      if not IsSuffix('/', Zipper.Entries[I].UTF8ArchiveFileName) then
        FFiles.Add(Zipper.Entries[I].UTF8ArchiveFileName);
  end;
  if UnZipper <> nil then
  begin
    for I := 0 to UnZipper.Entries.Count - 1 do
      if not IsSuffix('/', UnZipper.Entries[I].UTF8ArchiveFileName) then
        FFiles.Add(UnZipper.Entries[I].UTF8ArchiveFileName);
  end;
end;

procedure TCastleZip.Close;
begin
  FreeAndNil(Zipper);
  FreeAndNil(UnZipper);

  if FOwnsReadStream then
  begin
    FreeAndNil(FReadStream);
    { FOwnsReadStream should not really matter when FReadStream = nil,
      but set it to false to have 100% reliable state when closed,
      so make logic easier. }
    FOwnsReadStream := false;
  end else
    FReadStream := nil;

  Assert(not FOwnsReadStream);
  Assert(UnZipper = nil);
  Assert(FReadStream = nil);
  Assert(Zipper = nil);

  UpdateFiles; // clears file list
end;

procedure TCastleZip.OpenInputStream(Sender: TObject; var AStream: TStream);
begin
  AStream := FReadStream;
end;

procedure TCastleZip.CloseInputStream(Sender: TObject; var AStream: TStream);
begin
  { Set AStream to nil, to not let TUnZipper.CloseInput to free the stream.
    We manage FReadStream lifetime ourselves. }
  AStream := nil;
end;

function TCastleZip.Read(const PathInZip: String): TStream;
var
  FilesInZipList: TStringlist;
  UnzipFileHelper: TUnzipFileHelper;
begin
  if not IsOpenRead then
    raise EZipNotOpen.Create('Cannot read from ZIP, it is not open for reading');

  UnzipFileHelper := TUnzipFileHelper.Create;
  try
    UnzipFileHelper.Stream := TMemoryStream.Create;
    Unzipper.OnCreateStream := @UnzipFileHelper.UnzipCreateStream;
    Unzipper.OnDoneStream := @UnzipFileHelper.UnzipDoneStream;
    FilesInZipList := TStringlist.Create;
    try
      FilesInZipList.Add(PathInZip);
      UnZipper.UnZipFiles(FilesInZipList);
    finally FreeAndNil(FilesInZipList) end;
    Result := UnzipFileHelper.Stream;
    { Rewind, as our API guarantees this and it is more natural,
      it is also required for RegisterUrlProtocol URL read handlers. }
    Result.Position := 0;
  finally FreeAndNil(UnzipFileHelper) end;
end;

procedure TCastleZip.Write(const PathInZip: String;
  const Stream: TStream; const OwnsStream: Boolean);
var
  WriteCopy: TMemoryStream;
begin
  if not IsOpenWrite then
    raise EZipNotOpen.Create('Cannot write to ZIP, it is not open for writing');

  { Right now, we always make a copy of contents.
    This is safe: FPC TZippper saves Stream instance for later use.

    TODO: Possible optimization: We could avoid making WriteCopy
    when OwnsStream = true, then just pass Stream directly to
    Zipper.Entries.AddFileEntry and free it at close.
    Need to manage a "list of streams to free at close". }

  WriteCopy := TMemoryStream.Create;
  try
    ReadGrowingStream(Stream, WriteCopy, true);
    Zipper.Entries.AddFileEntry(WriteCopy, PathInZip);
  finally FreeAndNil(WriteCopy) end;

  if OwnsStream then
    Stream.Free; // cannot FreeAndNil(Stream), as Stream is const

  UpdateFiles; // add new file to file list
end;

procedure TCastleZip.Write(const PathInZip: String; const Url: String);
var
  FileName: String;
  WriteEntryStream: TStream;
begin
  if not IsOpenWrite then
    raise EZipNotOpen.Create('Cannot write to ZIP, it is not open for writing');

  FileName := UriToFilenameSafe(Url);
  if FileName <> '' then
  begin
    Zipper.Entries.AddFileEntry(FileName, PathInZip);
    UpdateFiles; // add new file to file list
  end else
  begin
    WriteEntryStream := Download(Url);
    Write(PathInZip, WriteEntryStream, true);
  end;
end;

procedure TCastleZip.Save(const Url: String);
var
  WriteStream: TStream;
begin
  if not IsOpenWrite then
    raise EZipNotOpen.Create('Cannot save ZIP, it is not open for writing');

  WriteStream := UrlSaveStream(Url);
  try
    Zipper.SaveToStream(WriteStream);
  finally FreeAndNil(WriteStream) end;
end;

{$else}

{ TCastleZip using Delphi ---------------------------------------------------- }

{ Delphi version uses TZipFile,
  https://docwiki.embarcadero.com/Libraries/Sydney/en/System.Zip.TZipFile }

function TCastleZip.IsOpen: Boolean;
begin
  Result := ZipFile <> nil;
end;

function TCastleZip.IsOpenRead: Boolean;
begin
  Result := IsOpen and (ZipFile.Mode in [zmRead, zmReadWrite]);
end;

function TCastleZip.IsOpenWrite: Boolean;
begin
  Result := IsOpen and (ZipFile.Mode in [zmWrite, zmReadWrite]);
end;

procedure TCastleZip.Open(const Url: String);
begin
  if UriToFilenameSafe(Url) <> '' then
  begin
    Close;
    ZipFile := TZipFile.Create;
    ZipFile.Open(UriToFilenameSafe(Url), zmRead);
    UpdateFiles;
  end else
  begin
    Open(Download(Url), true);
  end;
end;

procedure TCastleZip.Open(const Stream: TStream; const OwnsStream: boolean);
begin
  Close;

  // set these after Close, to avoid freeing them by Close
  FZipStream := Stream;
  FOwnsZipStream := OwnsStream;

  // this code is similar to TCastleZip.Open(Url), but now we pass FZipStream
  ZipFile := TZipFile.Create;
  ZipFile.Open(FZipStream, zmRead);
  UpdateFiles;
end;

procedure TCastleZip.OpenEmpty;
begin
  Close;

  FZipStream := TMemoryStream.Create;
  FOwnsZipStream := true;

  ZipFile := TZipFile.Create;
  ZipFile.Open(FZipStream, zmWrite);
  UpdateFiles;
end;

procedure TCastleZip.UpdateFiles;
var
  I: Integer;
  PathInZip: String;
begin
  FFiles.Clear;
  if ZipFile.Mode <> zmClosed then
  begin
    for I := 0 to ZipFile.FileCount - 1 do
    begin
      PathInZip := ZipFile.FileName[I];
      { On Windows, Delphi TZipFile converts paths to contain \.
        We don't want this, we want always /, to be the same cross-platform
        and to be consistent with URLs.
        In our eyes, PathInZip is not a filename, and PathInZip is not platform-specific,
        so no reason why it should use platform-specific \. }
      {$ifdef MSWINDOWS}
      PathInZip := SReplaceChars(PathInZip, '\', '/');
      {$endif}
      if not IsSuffix('/', PathInZip) then
        FFiles.Add(PathInZip);
    end;
  end;
end;

procedure TCastleZip.Close;
begin
  if FOwnsZipStream then
  begin
    FreeAndNil(FZipStream);
    FOwnsZipStream := false;
  end else
    FZipStream := nil;

  FreeAndNil(ZipFile);

  Assert(not FOwnsZipStream);
  Assert(FZipStream = nil);
  Assert(ZipFile = nil);
end;

function TCastleZip.PathToZip(const PathInZip: String): String;
begin
  { TZipFile on Windows expects paths in ZIP with \, convert from /.
    We prefer /, as documented in UpdateFiles. }
  Result :=
    {$ifdef MSWINDOWS} SReplaceChars(PathInZip, '/', '\')
    {$else} PathInZip
    {$endif};
end;

function TCastleZip.Read(const PathInZip: String): TStream;
var
  LocalHeader: TZipHeader;
  TempStream: TStream;
begin
  if not IsOpenRead then
    raise EZipNotOpen.Create('Cannot read from ZIP, it is not open for reading');

  try
    { Depending on what is inside ZIP, TZipFile on Windows may expect
      argument with / or \ ... We try both, to have consistent API
      that just accepts /, on all platforms, for all ZIP files. }
    if ZipFile.IndexOf(PathToZip(PathInZip)) <> -1 then
      ZipFile.Read(PathToZip(PathInZip), TempStream, LocalHeader)
    else
      ZipFile.Read(PathInZip, TempStream, LocalHeader);
  except
    // improve EZipFileNotFoundException message, to include PathInZip
    on E: EZipFileNotFoundException do
    begin
      E.Message := E.Message + ' (path in zip: ' + PathInZip + ')';
      raise;
    end;
  end;

  { We cannot just use Result := TempStream,
    because the stream returned by ZipFile.Read will be destroyed
    when ZipFile is destroyed. }

  Result := TMemoryStream.Create;
  Result.CopyFrom(TempStream, 0);
  Result.Position := 0;
end;

procedure TCastleZip.Write(const PathInZip: String; const Stream: TStream; const OwnsStream: Boolean);
begin
  if not IsOpenWrite then
    raise EZipNotOpen.Create('Cannot write to ZIP, it is not open for writing');

  ZipFile.Add(Stream, PathToZip(PathInZip));

  // The TZipFile.Add doesn't need the Stream instance to remain valid anymore.
  // This is in contrast to FPC TZippper that saves Stream instance.
  // So we can immediately free Stream.
  if OwnsStream then
    Stream.Free; // cannot FreeAndNil(Stream), as Stream is const

  UpdateFiles; // add new file to file list
end;

procedure TCastleZip.Write(const PathInZip: String; const Url: String);
var
  FileName: String;
  WriteEntryStream: TStream;
begin
  if not IsOpenWrite then
    raise EZipNotOpen.Create('Cannot write to ZIP, it is not open for writing');

  FileName := UriToFilenameSafe(Url);
  if FileName <> '' then
  begin
    ZipFile.Add(FileName, PathToZip(PathInZip));
    UpdateFiles; // add new file to file list
  end else
  begin
    WriteEntryStream := Download(Url);
    Write(PathInZip, WriteEntryStream, true);
  end;
end;

procedure TCastleZip.Save(const Url: String);
var
  SaveStream: TStream;
begin
  if not IsOpenWrite then
    raise EZipNotOpen.Create('Cannot save ZIP, it is not open for writing');

  // Close writes to FZipStream all data
  ZipFile.Close;

  SaveStream := UrlSaveStream(Url);
  try
    FZipStream.Position := 0;
    ReadGrowingStream(FZipStream, SaveStream, true);
  finally FreeAndNil(SaveStream) end;

  // Delphi TZipFile closes the ZIP after writing to it.
  // Reopen it, in read-write mode.
  ZipFile.Open(FZipStream, zmReadWrite);
end;

{$endif}

{ TCastleZip common ---------------------------------------------------------- }

constructor TCastleZip.Create;
begin
  inherited;
  FFiles := TStringList.Create;
end;

destructor TCastleZip.Destroy;
begin
  Close;
  UnregisterUrlProtocol;
  FreeAndNil(FFiles);
  inherited;
end;

procedure TCastleZip.RegisterUrlProtocol(const Protocol: String);
begin
  FRegisteredUrlProtocol := Protocol;
  CastleDownload.RegisterUrlProtocol(Protocol,
    {$ifdef FPC}@{$endif} ReadUrlHandler, nil);
end;

procedure TCastleZip.UnregisterUrlProtocol;
begin
  if FRegisteredUrlProtocol <> '' then
  begin
    CastleDownload.UnregisterUrlProtocol(FRegisteredUrlProtocol);
    FRegisteredUrlProtocol := '';
  end;
  Assert(FRegisteredUrlProtocol = '');
end;

function TCastleZip.ReadUrlHandler(const Url: String; out MimeType: string): TStream;
var
  U: TURI;
  FileInZip: String;
begin
  U := ParseURI(Url);
  FileInZip := PrefixRemove('/', U.Path + U.Document, false);
  Result := Read(FileInZip);

  { Determine mime type from Url, which practically means:
    determine content type from filename extension. }
  MimeType := UriMimeType(Url);
end;

function TCastleZip.GetFiles: TStrings;
begin
  Result := FFiles;
end;

{ global routines ------------------------------------------------------------- }

procedure ZipDirectory(const ZipUrl: String; DirectoryUrl: String;
  const SingleTopLevelDirectory: Boolean);
var
  Zip: TCastleZip;
  FilesList: TFileInfoList;
  FileInfo: TFileInfo;
  Directory, DirectoryParentPath, ExpandedZipFileName, PathInZip: String;
begin
  // absolute filename, if ZipUrl points to a file
  ExpandedZipFileName := UriToFilenameSafe(ZipUrl);
  if ExpandedZipFileName <> '' then
    ExpandedZipFileName := ExpandFileName(ExpandedZipFileName);

  Zip := TCastleZip.Create;
  try
    Zip.OpenEmpty;

    Directory := UriToFilenameSafe(DirectoryUrl);
    if Directory = '' then
      raise Exception.CreateFmt('Cannot zip directory "%s", it is not a valid file URL', [DirectoryUrl]);
    Directory := ExclPathDelim(Directory);
    DirectoryParentPath := ExtractFilePath(Directory);

    FilesList := FindFilesList(Directory, '*', { FindDirectories } false, [ffRecursive]);
    try
      for FileInfo in FilesList do
      begin
        if (ExpandedZipFileName <> '') and
           SameFileName(ExpandedZipFileName, FileInfo.AbsoluteName) then
        begin
          WritelnWarning('Package', Format('Directory to zip contains also the target zip file "%s", not packing (to avoid possible reading and writing the file at the same time)', [
            FileInfo.AbsoluteName
          ]));
          Continue;
        end;
        if SingleTopLevelDirectory then
          PathInZip := ExtractRelativePath(DirectoryParentPath, FileInfo.AbsoluteName)
        else
          // Below we need InclPathDelim(Directory) to have proper relative results
          PathInZip := ExtractRelativePath(InclPathDelim(Directory), FileInfo.AbsoluteName);
        Zip.Write(PathInZip, FileInfo.Url);
      end;
    finally FreeAndNil(FilesList) end;

    Zip.Save(ZipUrl);
  finally FreeAndNil(Zip) end;
end;

end.
