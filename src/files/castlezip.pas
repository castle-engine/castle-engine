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
unit CastleZip experimental;

interface

uses SysUtils, Classes,
  {$ifdef FPC} Zipper {$else} System.Zip {$endif};

type
  { A collection of files inside a ZIP archive.

    This class wraps various ZIP archive handling routines,
    presenting a uniform API for ZIP handling that works with any compiler
    we support (FPC and Delphi) and allows to use URLs for everything.

    Open a ZIP archive by @link(Open). Later close it by @link(Close).

    Read a file from the ZIP archive, while it is open, using @link(Read). }
  TCastleZip = class
  strict private
    {$ifdef FPC}
    { When open, we always have FOpenStream <> nil and Unzip <> nil.
      We never assign Unzip.FileName, instead relying on own URL -> TStream
      reading. }
    Unzip: TUnZipper;
    FOpenStream: TStream;
    FOwnsOpenStream: Boolean;
    { Handler for TUnZipper.OnOpenInputStream. }
    procedure OpenInputStream(Sender: TObject; var AStream: TStream);
    { Handler for TUnZipper.OnCloseInputStream. }
    procedure CloseInputStream(Sender: TObject; var AStream: TStream);
    {$else}
    ZipFile: TZipFile;
    {$endif}
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

    { Close the ZIP archive. This releases all resources
      (avoids keeping in memory the ZIP contents),
      but afterwards you cannot @link(Read) files from ZIP anymore.

      There's usually no need to call this, as the destructor or opening
      a new ZIP archive using @link(Open) will first close the existing archive.
      It is only useful if you want to release the resources related to your ZIP
      earlier, which is in turn only a concern if you really deal with huge
      (e.g. gigabytes) ZIP files. }
    procedure Close;

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

    (* TODO:
    // We have plans for richer API for ZIPs.
    // How rich? Time will tell what is necessary (and reasonably doable
    // using FPC and Delphi APIs underneath).
    // Plans below are just a starting point.

    { Read-only list of all files within the ZIP archive.
      All entries are valid as arguments for @link(Read),
      so they are relative paths within the zip archive,
      with parts separated by slashes.

      No order of the contents is guaranteed.

      @raises EZipNotOpen If the ZIP archive is not open. }
    property FileList: TStrings read GetFileList;

    { Extract all files from the ZIP archive into the given path,
      recreating the subdirectory structure of the ZIP inside TargetPath.

      The TargetPathUrl should be an URL of an existing directory.
      It may but doesn't have to end with slash.
      It will be combined with the relative URLs of the files
      inside ZIP using @link(CombineUri).

      TODO: remember to combine reative paths -> URL.
      Do we need to percent-encode?
      How do we handle spaces in paths in ZIP?
      How do we handle %20 in paths in ZIP?
      Make it either consistent with URLs, or clearly document the differences
      at Read, FileList, and other routines that take/return PathInZip.

      Any URL protocol where we can write files (using @link(UrlSaveStream))
      is allowed. }
    procedure ExtractAl(const TargetPathUrl: String);

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

        @item(For overload with Stream:

          We expect the provided stream to be positioned at the beginning,
          and we will read from it until the end. We assume nothing
          about the given Stream, in particular we don't assume that it is seekable,
          we don't assume it has useful TStream.Size. We will process it using
          @link(ReadGrowingStream).

          Note that there's no way to provide the "metadata" of the file
          (like file modification, Unix permissions)
          when you provide a stream. For this reason we advise to use the
          overload with URL whenever possible.
        )
      )


      After this is called, the new file entry appears in the @link(FileList).

      The ZIP archive is not saved to disk until you call @link(Save).
      If you call @link(Close) without calling @link(Save), the changes
      are lost. @link(Save) is never called automatically.

      // TODO: check existing APIs, do they allow this, implying that they
      keep ZIP in memory? Or do they always save to disk immediately?
      The FPC TZipper allows this, yes, has SaveToFile and SaveToStream.
    }
    procedure Write(const PathInZip: String; const Stream: TStream);
    procedure Write(const PathInZip: String; const Url: String);

    { Save the currently open ZIP archive (with all modificaiotns
      done by @link(Write)) to the given URL. }
    procedure Save(const Url: String);
    *)
  end;

implementation

uses CastleUriUtils, CastleDownload, CastleFilesUtils;

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

procedure TCastleZip.Open(const Url: String);
begin
  Open(Download(Url), true);
end;

procedure TCastleZip.Open(const Stream: TStream; const OwnsStream: boolean);
begin
  Close;

  FOpenStream := Stream;
  FOwnsOpenStream := OwnsStream;

  Unzip := TUnZipper.Create;
  Unzip.OnOpenInputStream := @OpenInputStream;
  Unzip.OnCloseInputStream := @CloseInputStream;
end;

procedure TCastleZip.Close;
begin
  FreeAndNil(Unzip);

  if FOwnsOpenStream then
  begin
    FreeAndNil(FOpenStream);
    { FOwnsOpenStream should not really matter when FOpenStream = nil,
      but set it to false to have 100% reliable state when closed,
      so make logic easier. }
    FOwnsOpenStream := false;
  end else
    FOpenStream := nil;

  Assert(not FOwnsOpenStream);
  Assert(Unzip = nil);
  Assert(FOpenStream = nil);
end;

procedure TCastleZip.OpenInputStream(Sender: TObject; var AStream: TStream);
begin
  AStream := FOpenStream;
end;

procedure TCastleZip.CloseInputStream(Sender: TObject; var AStream: TStream);
begin
  { Set AStream to nil, to not let TUnZipper.CloseInput to free the stream.
    We manage FOpenStream lifetime ourselves. }
  AStream := nil;
end;

function TCastleZip.Read(const PathInZip: String): TStream;
var
  FilesInZipList: TStringlist;
  UnzipFileHelper: TUnzipFileHelper;
begin
  UnzipFileHelper := TUnzipFileHelper.Create;
  try
    UnzipFileHelper.Stream := TMemoryStream.Create;
    Unzip.OnCreateStream := @UnzipFileHelper.UnzipCreateStream;
    Unzip.OnDoneStream := @UnzipFileHelper.UnzipDoneStream;
    FilesInZipList := TStringlist.Create;
    try
      FilesInZipList.Add(PathInZip);
      Unzip.UnZipFiles(FilesInZipList);
    finally FreeAndNil(FilesInZipList) end;
    Result := UnzipFileHelper.Stream;
    { Rewind, as our API guarantees this and it is more natural,
      it is also required for RegisterUrlProtocol URL read handlers. }
    Result.Position := 0;
  finally FreeAndNil(UnzipFileHelper) end;
end;

{$else}

{ TCastleZip using Delphi ---------------------------------------------------- }

{ Delphi version uses TZipFile,
  https://docwiki.embarcadero.com/Libraries/Sydney/en/System.Zip.TZipFile }

procedure TCastleZip.Open(const Url: String);
begin
  ZipFile := TZipFile.Create;
  if UriToFilenameSafe(Url) <> '' then
    ZipFile.Open(UriToFilenameSafe(Url), zmRead)
  else
    raise Exception.Create('TODO: Opening non-file URLs for ZIP not implemented');
end;

procedure TCastleZip.Open(const Stream: TStream; const OwnsStream: boolean);
begin
  raise Exception.Create('TODO: Opening ZIP from stream not implemented');
end;

procedure TCastleZip.Close;
begin
  FreeAndNil(ZipFile);
end;

function TCastleZip.Read(const PathInZip: String): TStream;
var
  LocalHeader: TZipHeader;
  TempStream: TStream;
begin
  ZipFile.Read(PathInZip, TempStream, LocalHeader);

  { We cannot just use Result := TempStream,
    because the stream returned by ZipFile.Read will be destroyed
    when ZipFile is destroyed. }

  Result := TMemoryStream.Create;
  Result.CopyFrom(TempStream, 0);
  Result.Position := 0;
end;

{$endif}

{ TCastleZip common ---------------------------------------------------------- }

constructor TCastleZip.Create;
begin
  inherited;
end;

destructor TCastleZip.Destroy;
begin
  Close;
  inherited;
end;

end.
