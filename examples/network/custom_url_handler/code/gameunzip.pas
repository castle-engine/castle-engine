{
  Copyright 2019-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Unzip routines that both with both FPC and Delphi. }
unit GameUnzip;

interface

uses SysUtils, Classes,
  {$ifdef FPC} Zipper {$else} System.Zip {$endif};

{ Unpack single file from zip, to a TStream.
  FileInZip should be relative path within the zip archive. }
function UnzipFile(const ZipFileName, FileInZip: String): TStream;

implementation

uses CastleURIUtils, CastleDownload, CastleFilesUtils;

{$ifdef FPC}
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
{$endif}

function UnzipFile(const ZipFileName, FileInZip: String): TStream;
{$ifdef FPC}

{ FPC version uses TUnZipper,
  https://www.freepascal.org/docs-html/fcl/zipper/tunzipper.html }

var
  Unzip: TUnZipper;
  FilesInZipList: TStringlist;
  UnzipFileHelper: TUnzipFileHelper;
begin
  Unzip := TUnZipper.Create;
  try
    Unzip.FileName := ZipFileName;
    UnzipFileHelper := TUnzipFileHelper.Create;
    try
      UnzipFileHelper.Stream := TMemoryStream.Create;
      Unzip.OnCreateStream := @UnzipFileHelper.UnzipCreateStream;
      Unzip.OnDoneStream := @UnzipFileHelper.UnzipDoneStream;
      FilesInZipList := TStringlist.Create;
      try
        FilesInZipList.Add(FileInZip);
        Unzip.UnZipFiles(FilesInZipList);
      finally FreeAndNil(FilesInZipList) end;
      Result := UnzipFileHelper.Stream;
      Result.Position := 0; // rewind, CGE reading routines expect this
    finally FreeAndNil(UnzipFileHelper) end;
  finally FreeAndNil(Unzip) end;
{$else}

{ Delphi version uses TZipFile,
  https://docwiki.embarcadero.com/Libraries/Sydney/en/System.Zip.TZipFile }

var
  ZipFile: TZipFile;
  LocalHeader: TZipHeader;
  TempStream: TStream;
begin
  ZipFile := TZipFile.Create;
  try
    ZipFile.Open(ZipFileName, zmRead);
    ZipFile.Read(FileInZip, TempStream, LocalHeader);

    { We cannot just use Result := TempStream,
      because the stream returned by ZipFile.Read will be destroyed
      when ZipFile is destroyed. }

    Result := TMemoryStream.Create;
    Result.CopyFrom(TempStream, 0);
    Result.Position := 0; // rewind, CGE reading routines expect this
  finally FreeAndNil(ZipFile) end;
{$endif}
end;

end.