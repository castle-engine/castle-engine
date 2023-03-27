program tczipper;
{
    This file is part of the Free Pascal packages.
    Copyright (c) 2012-2014 by the Free Pascal Development Team
    Created by Reinier Olislagers

    Tests zip/unzip functionality provided by the FPC zipper.pp unit.
    If passed a zip file name as first argument, it will try and decompress
    and list the contents of the zip file.

    See the file COPYING.FPC, included in this distribution,
    for details about the license.

 **********************************************************************}
{$mode objfpc}{$h+}

//Define this if you want to inspect the generated zips etc
{$define KEEPTESTFILES}

uses
  SysUtils, classes,
  zipper, unzip, zdeflate, zinflate, zip, md5, zstream, nullstream;

type

  { TCallBackHandler }

  TCallBackHandler = class(TObject) //Callbacks used in zip/unzip processing
  private
    FPerformChecks: boolean;
    FOriginalContent: string;
    FShowContent: boolean;
    FStreamResult: boolean;
  public
    property PerformChecks: boolean read FPerformChecks write FPerformChecks; //If false, do not perform any consistency checks
    property OriginalContent: string read FOriginalContent write FOriginalContent; //Zip entry uncompressed content used in TestZipEntries
    property ShowContent: boolean read FShowContent write FShowContent; //Show contents of zip when extracting?
    property StreamResult: boolean read FStreamResult; //For handler to report success/failure
    procedure EndOfFile(Sender:TObject; const Ratio:double);
    procedure StartOfFile(Sender:TObject; const AFileName:string);
    procedure DoCreateZipOutputStream(Sender: TObject; var AStream: TStream;
      AItem: TFullZipFileEntry);
    procedure DoDoneOutZipStream(Sender: TObject; var AStream: TStream;
      AItem: TFullZipFileEntry); //Used to verify zip entry decompressed contents
    constructor Create;
  end;

procedure TCallBackHandler.EndOfFile(Sender: TObject; const Ratio: double);
begin
  writeln('End of file handler hit; compression ratio: '+floattostr(ratio));
  if (FPerformChecks) and (Ratio<0) then
  begin
    writeln('Found compression ratio '+floattostr(Ratio)+', which should never be lower than 0.');
    halt(1);
  end;
end;

procedure TCallBackHandler.StartOfFile(Sender: TObject; const AFileName: string);
begin
  writeln('Start of file handler hit; filename: '+AFileName);
  if (FPerformChecks) and (AFileName='') then
  begin
    writeln('Archive filename should not be empty.');
    halt(1);
  end;
end;

procedure TCallBackHandler.DoCreateZipOutputStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  AStream:=TMemoryStream.Create;
end;

procedure TCallBackHandler.DoDoneOutZipStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
var
  DecompressedContent: string;
begin
  //writeln('At end of '+AItem.ArchiveFileName);
  AStream.Position:=0;
  SetLength(DecompressedContent,Astream.Size);
  if AStream.Size>0 then
    (AStream as TMemoryStream).Read(DecompressedContent[1], AStream.Size);
  if (FPerformChecks) and (DecompressedContent<>OriginalContent) then
  begin
    FStreamResult:=false;
    writeln('TestZipEntries failed: found entry '+AItem.ArchiveFileName+
      ' has value ');
    writeln('*'+DecompressedContent+'*');
    writeln('expected ');
    writeln('*'+OriginalContent+'*');
  end;
  if (FPerformChecks=false) and (ShowContent=true) then
  begin
    //display only
    writeln('TestZipEntries info: found entry '+AItem.ArchiveFileName+
      ' has value ');
    writeln('*'+DecompressedContent+'*');
  end;
  Astream.Free;
end;

constructor TCallBackHandler.Create;
begin
  FOriginalContent:='A'; //nice short demo content
  FStreamResult:=true;
  FPerformChecks:=true; //perform verification by default
  FShowContent:=true;
end;


function CompareCompressDecompress: boolean;
var
  CallBackHandler: TCallBackHandler;
  CompressedFile: string;
  FileContents: TStringList;
  UncompressedFile1: string;
  UncompressedFile1Hash: string;
  UncompressedFile2: string;
  UncompressedFile2Hash: string;
  OurZipper: TZipper;
  UnZipper: TUnZipper;
begin
  result:=true;

  FileContents:=TStringList.Create;
  OurZipper:=TZipper.Create;
  UnZipper:=TUnZipper.Create;
  CallBackHandler:=TCallBackHandler.Create;
  try
    // Set up uncompressed files
    FileContents.Add('This is an uncompressed file.');
    FileContents.Add('And another line.');
    UncompressedFile1:=SysUtils.GetTempFileName('', 'UN1');
    FileContents.SaveToFile(UncompressedFile1);
    FileContents.Clear;
    FileContents.Add('Have you looked into using fpcup today?');
    FileContents.Add('It works nicely with fpc and goes well with a fruity red wine, too.');
    // Second GetTempFileName call needs to be done after saving first file because
    // GetTempFileName checks for existing file names and may give the *same* file name
    // if called before
    UncompressedFile2:=SysUtils.GetTempFileName('', 'UN2');
    FileContents.SaveToFile(UncompressedFile2);
    // Remember their content, so we can compare later.
    UncompressedFile1Hash:=MD5Print(MD5File(UncompressedFile1, MDDefBufSize));
    UncompressedFile2Hash:=MD5Print(MD5File(UncompressedFile2, MDDefBufSize));

    // Test zip functionality.
    CompressedFile:=SysUtils.GetTempFileName('', 'CC');
    OurZipper.FileName:=CompressedFile;
    // Add the files only with their filenames, we don't want to create
    // subdirectories:
    OurZipper.Entries.AddFileEntry(UncompressedFile1,ExtractFileName(UncompressedFile1));
    OurZipper.Entries.AddFileEntry(UncompressedFile2,ExtractFileName(UncompressedFile2));
    OurZipper.OnStartFile:=@CallBackHandler.StartOfFile;
    OurZipper.OnEndFile:=@CallBackHandler.EndOfFile;
    OurZipper.ZipAllFiles;
    if not FileExists(CompressedFile) then
    begin
      writeln('Zip file was not created.');
      exit(false);
    end;

    // Delete original files
    {$IFNDEF KEEPTESTFILES}
    DeleteFile(UncompressedFile1);
    DeleteFile(UncompressedFile2);
    {$ENDIF}

    // Now unzip
    Unzipper.FileName:=CompressedFile;
    Unzipper.OutputPath:=ExtractFilePath(UncompressedFile1);
    UnZipper.OnStartFile:=@CallBackHandler.StartOfFile;
    UnZipper.OnEndFile:=@CallBackHandler.EndOfFile;
    Unzipper.Examine;
    Unzipper.UnZipAllFiles;

    // Now we should have the uncompressed files again
    if (not FileExists(UncompressedFile1)) or
      (not FileExists(UncompressedFile2)) then
    begin
      writeln('Unzip failed: could not find decompressed files.');
      exit(false);
    end;

    // Compare hashes
    if
      (UncompressedFile1Hash<>MD5Print(MD5File(UncompressedFile1, MDDefBufSize)))
      or
      (UncompressedFile2Hash<>MD5Print(MD5File(UncompressedFile2, MDDefBufSize)))
    then
    begin
      writeln('Unzip failed: uncompressed files are not the same as the originals.');
      exit(false);
    end;

  finally
    FileContents.Free;
    CallBackHandler.Free;
    OurZipper.Free;
    UnZipper.Free;
    {$IFNDEF KEEPTESTFILES}
    try
      if FileExists(CompressedFile) then DeleteFile(CompressedFile);
      if FileExists(UncompressedFile1) then DeleteFile(UncompressedFile1);
      if FileExists(UncompressedFile2) then DeleteFile(UncompressedFile2);
    finally
      // Ignore errors: OS should eventually clean out temp files anyway
    end;
    {$ENDIF}
  end;
end;

function CompressSmallStreams: boolean;
// Compresses some small streams using default compression and
// no compression (storage)
// Just storing is the best option; compression will enlarge the zip.
// Test verifies that the entries in the zip are not bigger than
// the originals.
var
  DestFile: string;
  z: TZipper;
  zfe: TZipFileEntry;
  s: string = 'abcd';
  DefaultStream, StoreStream: TStringStream;
begin
  result:=true;
  DestFile:=SysUtils.GetTempFileName('', 'CS1');
  z:=TZipper.Create;
  z.FileName:=DestFile;
  try
    DefaultStream:=TStringStream.Create(s);
    StoreStream:=TStringStream.Create(s);

    //DefaultStream - compression  level = Default
    zfe:=z.Entries.AddFileEntry(DefaultStream, 'Compressed');
    z.ZipAllFiles;

    if (z.Entries[0].Size>zfe.Size) then
    begin
      result:=false;
      writeln('Small stream test default compression failed: compressed size '+
        inttostr(z.Entries[0].Size) + ' > original size '+inttostr(zfe.Size));
      exit;
    end;

  finally
    DefaultStream.Free;
    StoreStream.Free;
    z.Free;
  end;

  {$IFNDEF KEEPTESTFILES}
  try
    DeleteFile(DestFile);
  except
    // ignore mess
  end;
  {$ENDIF}

  DestFile:=SysUtils.GetTempFileName('', 'CS2');
  z:=TZipper.Create;
  z.FileName:=DestFile;
  try
    DefaultStream:=TStringStream.Create(s);
    StoreStream:=TStringStream.Create(s);

    //StoreStream - compression  level = Store
    zfe:=z.Entries.AddFileEntry(StoreStream, 'Uncompressed');
    zfe.CompressionLevel:=clnone;
    z.ZipAllFiles;

    if (z.Entries[0].Size>zfe.Size) then
    begin
      result:=false;
      writeln('Small stream test uncompressed failed: compressed size '+
        inttostr(z.Entries[0].Size) + ' > original size '+inttostr(zfe.Size));
      exit;
    end;
  finally
    DefaultStream.Free;
    StoreStream.Free;
    z.Free;
  end;

  {$IFNDEF KEEPTESTFILES}
  try
    DeleteFile(DestFile);
  except
    // ignore mess
  end;
  {$ENDIF}

  //The result can be checked with the command (on Linux):
  //unzip -v <DestFile>
  //The column Size Shows that compressed files are bigger than source files
end;

function ShowZipFile(ZipFile: string): boolean;
// Reads zip file and lists entries
var
  CallBackHandler: TCallBackHandler;
  i: integer;
  UnZipper: TUnZipper;
  UnzipArchiveFiles: TStringList;
begin
  result:=true;
  UnZipper:=TUnZipper.Create;
  CallBackHandler:=TCallBackHandler.Create;
  UnzipArchiveFiles:=TStringList.Create;
  try
    CallBackHandler.PerformChecks:=false; //only display output
    UnZipper.FileName:=ZipFile;
    Unzipper.Examine;
    writeln('ShowZipFile: zip file has '+inttostr(UnZipper.Entries.Count)+' entries');

    i:=0;
    Unzipper.OnCreateStream:=@CallBackHandler.DoCreateZipOutputStream;
    Unzipper.OnDoneStream:=@CallBackHandler.DoDoneOutZipStream;
    while i<Unzipper.Entries.Count do
    begin
      if CallBackHandler.StreamResult then
      begin
        UnzipArchiveFiles.Clear;
        UnzipArchiveFiles.Add(Unzipper.Entries[i].ArchiveFileName);
        Unzipper.UnZipFiles(UnzipArchiveFiles);
        // This will kick off the DoCreateOutZipStream/DoDoneOutZipStream handlers
        inc(i);
      end
      else
      begin
        break; // Handler has reported error; stop loop
      end;
    end;
  finally
    Unzipper.Free;
    CallBackHandler.Free;
    UnzipArchiveFiles.Free;
  end;
end;

function TestZipEntries(Entries: qword): boolean;
// Adds Entries amount of zip file entries and reads them
// Starting from 65535 entries, the zip needs to be in zip64 format
var
  CallBackHandler: TCallBackHandler;
  DestFile: string;
  i: qword;
  OriginalContent: string = 'A'; //Uncompressed content for zip file entry
  ContentStreams: TFPList;
  ContentStream: TStringStream;
  UnZipper: TUnZipper;
  UnzipArchiveFiles: TStringList;
  Zipper: TZipper;
begin
  result:=true;
  DestFile:=SysUtils.GetTempFileName('', 'E'+inttostr(Entries)+'_');
  Zipper:=TZipper.Create;
  Zipper.FileName:=DestFile;
  ContentStreams:=TFPList.Create;
  try
    i:=0;
    while i<Entries do
    begin
      ContentStream:=TStringStream.Create(OriginalContent);
      ContentStreams.Add(ContentStream);
      // Start filenames at 1
      Zipper.Entries.AddFileEntry(TStringStream(ContentStreams.Items[i]), format('%U',[i+1]));
      inc(i);
    end;
    Zipper.ZipAllFiles;
    {
    i:=0;
    while i<Entries do
    begin
      ContentStreams.Delete(i);
    end;
    }
  finally
    ContentStreams.Free;
    Zipper.Free;
  end;

  UnZipper:=TUnZipper.Create;
  CallBackHandler:=TCallBackHandler.Create;
  UnzipArchiveFiles:=TStringList.Create;
  try
    CallBackHandler.OriginalContent:=OriginalContent;
    UnZipper.FileName:=DestFile;
    Unzipper.Examine;
    if (UnZipper.Entries.Count<>Entries) then
    begin
      result:=false;
      writeln('TestZipEntries failed: found '+
        inttostr(UnZipper.Entries.Count) + ' entries; expected '+inttostr(Entries));
      exit;
    end;
    i:=0;
    Unzipper.OnCreateStream:=@CallBackHandler.DoCreateZipOutputStream;
    Unzipper.OnDoneStream:=@CallBackHandler.DoDoneOutZipStream;
    while i<Entries do
    begin
      if CallBackHandler.StreamResult then
      begin
        UnzipArchiveFiles.Clear;
        UnzipArchiveFiles.Add(Unzipper.Entries[i].ArchiveFileName);
        Unzipper.UnZipFiles(UnzipArchiveFiles);
        // This will kick off the DoCreateOutZipStream/DoDoneOutZipStream handlers
        inc(i);
      end
      else
      begin
        break; // Handler has reported error; stop loop
      end;
    end;
  finally
    Unzipper.Free;
    CallBackHandler.Free;
    UnzipArchiveFiles.Free;
  end;

  {$IFNDEF KEEPTESTFILES}
  try
    DeleteFile(DestFile);
  except
    // ignore mess
  end;
  {$ENDIF}
end;

function TestEmptyZipEntries(Entries: qword): boolean;
// Same as TestZipEntries, except uses empty data:
// useful for testing large number of files
var
  CallBackHandler: TCallBackHandler;
  DestFile: string;
  i: qword;
  ContentStreams: TFPList;
  ContentStream: TNullStream;
  UnZipper: TUnZipper;
  UnzipArchiveFiles: TStringList;
  Zipper: TZipper;
begin
  result:=true;
  DestFile:=SysUtils.GetTempFileName('', 'EZ'+inttostr(Entries)+'_');
  Zipper:=TZipper.Create;
  Zipper.FileName:=DestFile;
  ContentStreams:=TFPList.Create;
  try
    i:=0;
    while i<Entries do
    begin
      ContentStream:=TNullStream.Create;
      ContentStreams.Add(ContentStream);
      // Start filenames at 1
      Zipper.Entries.AddFileEntry(TStringStream(ContentStreams.Items[i]), format('%U',[i+1]));
      inc(i);
    end;
    Zipper.ZipAllFiles;
    {
    i:=0;
    while i<Entries do
    begin
      ContentStreams.Delete(i);
    end;
    }
  finally
    ContentStreams.Free;
    Zipper.Free;
  end;

  UnZipper:=TUnZipper.Create;
  UnzipArchiveFiles:=TStringList.Create;
  CallBackHandler:=TCallBackHandler.Create;
  try
    // Use callbacks to dump zip output into the bit bucket
    CallBackHandler.PerformChecks:=false;
    CallBackHandler.ShowContent:=false;
    Unzipper.OnCreateStream:=@CallBackHandler.DoCreateZipOutputStream;
    Unzipper.OnDoneStream:=@CallBackHandler.DoDoneOutZipStream;
    UnZipper.FileName:=DestFile;
    Unzipper.Examine;
    if (UnZipper.Entries.Count<>Entries) then
    begin
      result:=false;
      writeln('TestEmptyZipEntries failed: found '+
        inttostr(UnZipper.Entries.Count) + ' entries; expected '+inttostr(Entries));
      exit;
    end;
    i:=0;
    while i<Entries do
    begin
      UnzipArchiveFiles.Clear;
      UnzipArchiveFiles.Add(Unzipper.Entries[i].ArchiveFileName);
      Unzipper.UnZipFiles(UnzipArchiveFiles);
      inc(i);
    end;
  finally
    CallBackHandler.Free;
    Unzipper.Free;
    UnzipArchiveFiles.Free;
  end;

  {$IFNDEF KEEPTESTFILES}
  try
    DeleteFile(DestFile);
  except
    // ignore mess
  end;
  {$ENDIF}
end;

function SaveToFileTest: boolean;
var
  NewFileName: string;
  OldFileName: string;
  z: TZipper;
  zfe: TZipFileEntry;
  s: string = 'abcd';
  DefaultStream: TStringStream;
begin
  result:=true;
  OldFileName:=SysUtils.GetTempFileName('', 'OLD');
  NewFileName:=SysUtils.GetTempFileName('', 'NEW');
  z:=TZipper.Create;
  z.FileName:=OldFileName;
  try
    DefaultStream:=TStringStream.Create(s);
    zfe:=z.Entries.AddFileEntry(DefaultStream, 'Compressed');
    z.ZipAllFiles; //saves to OldFileName
    DeleteFile(NewFileName); //delete if present
    z.SaveToFile(NewFileName); //should save to newfilename
    if not(FileExists(NewFileName)) then
    begin
      writeln('Failure: file '+NewFileName+' does not exist.');
      result:=false;
    end
    else
    begin
      result:=true;
    end;
  finally
    DefaultStream.Free;
    z.Free;
  end;

  {$IFNDEF KEEPTESTFILES}
  try
    DeleteFile(DestFile);
  except
    // ignore mess
  end;
  {$ENDIF}
end;



function TestLargeFileName: boolean;
// Zips/unzips 259-character filename
var
  ArchiveFile: string;
  DestFile: string;
  s: string = 'a';
  DefaultStream: TStringStream;
  UnZipper: TUnZipper;
  Zipper: TZipper;
begin
  result:=true;
  ArchiveFile:=StringOfChar('A',259);
  DestFile:=SysUtils.GetTempFileName('', 'TL');
  Zipper:=TZipper.Create;
  Zipper.FileName:=DestFile;
  try
    DefaultStream:=TStringStream.Create(s);
    Zipper.Entries.AddFileEntry(DefaultStream, ArchiveFile);
    Zipper.ZipAllFiles;
  finally
    DefaultStream.Free;
    Zipper.Free;
  end;

  UnZipper:=TUnZipper.Create;
  try
    UnZipper.FileName:=DestFile;
    Unzipper.Examine;
    if (Unzipper.Entries[0].ArchiveFileName<>ArchiveFile) then
    begin
      result:=false;
      writeln('TestLargeFileName failed: found filename length '+
        inttostr(Length(Unzipper.Entries[0].ArchiveFileName)));
      writeln('*'+Unzipper.Entries[0].ArchiveFileName + '*');
      writeln('Expected length '+inttostr(Length(ArchiveFile)));
      writeln('*'+ArchiveFile+'*');
      exit;
    end;
  finally
    Unzipper.Free;
  end;

  {$IFNDEF KEEPTESTFILES}
  try
    DeleteFile(DestFile);
  except
    // ignore mess
  end;
  {$ENDIF}
end;

function TestWindowsPath: boolean;
// Zips filename in a subdirectory with a \ used as separator
// Zip standard requires using /
// On Linux, \ should be seen as a regular part of the filename
var
  FileWithBackslash: string;
  DestFile: string;
  s: string = 'a';
  DefaultStream: TStringStream;
  UnZipper: TUnZipper;
  Zipper: TZipper;
begin
  result:=true;
  FileWithBackslash:='test\afile.txt'; //on Windows, zip should handle this and internally replace \ with /
  // On *nix, this should just be a long file
  DestFile:=SysUtils.GetTempFileName('', 'TW');
  Zipper:=TZipper.Create;
  Zipper.FileName:=DestFile;
  try
    DefaultStream:=TStringStream.Create(s);
    Zipper.Entries.AddFileEntry(DefaultStream, FileWithBackslash);
    Zipper.ZipAllFiles;
  finally
    DefaultStream.Free;
    Zipper.Free;
  end;

  UnZipper:=TUnZipper.Create;
  try
    UnZipper.FileName:=DestFile;
    Unzipper.Examine;
    {$ifdef mswindows}
    if (pos('\',Unzipper.Entries[0].ArchiveFileName)>0) then
    begin
      result:=false;
      writeln('Failed: found \ in archive filename; expected /:');
      writeln('*'+Unzipper.Entries[0].ArchiveFileName+'*');
      exit;
    end;
    {$else}
    if (pos('\',Unzipper.Entries[0].ArchiveFileName)<=0) then
    begin
      result:=false;
      writeln('Failed: did not find / in archive filename:');
      writeln('*'+Unzipper.Entries[0].ArchiveFileName+'*');
      exit;
    end;
    {$endif}
  finally
    Unzipper.Free;
  end;

  {$IFNDEF KEEPTESTFILES}
  try
    DeleteFile(DestFile);
  except
    // ignore mess
  end;
  {$ENDIF}
end;


function TestLargeZip64: boolean;
// Tests single zip file with large uncompressed content
// which forces it to zip64 format
var
  ArchiveFile: string;
  DestFile: string;
  ContentStream: TNullStream; //empty contents
  UnZipper: TUnZipper;
  Zipper: TZipper;
begin
  result:=true;
  DestFile:=SysUtils.GetTempFileName('', 'LZ');
  Zipper:=TZipper.Create;
  Zipper.FileName:=DestFile;
  ArchiveFile:='HugeString.txt';

  ContentStream:=TNullStream.Create;
  // About 4Gb; content of 4 bytes+1 added
  ContentStream.Size:=(1+$FFFFFFFF);
  ContentStream.Position:=0;
  writeln('Buffer created');
  try
    Zipper.Entries.AddFileEntry(ContentStream, ArchiveFile);
    writeln('entry added');
    Zipper.ZipAllFiles;
  finally
    ContentStream.Free;
    Zipper.Free;
  end;

  UnZipper:=TUnZipper.Create;
  try
    UnZipper.FileName:=DestFile;
    Unzipper.Examine;
    if (UnZipper.Entries.Count<>1) then
    begin
      result:=false;
      writeln('TestLargeZip64 failed: found '+
        inttostr(UnZipper.Entries.Count) + ' entries; expected 1');
      exit;
    end;
    if (Unzipper.Entries[0].ArchiveFileName<>ArchiveFile) then
    begin
      result:=false;
      writeln('TestLargeZip64 failed: found filename length '+
        inttostr(Length(Unzipper.Entries[0].ArchiveFileName)));
      writeln('*'+Unzipper.Entries[0].ArchiveFileName + '*');
      writeln('Expected length '+inttostr(Length(ArchiveFile)));
      writeln('*'+ArchiveFile+'*');
      exit;
    end;
  finally
    Unzipper.Free;
  end;

  {$IFNDEF KEEPTESTFILES}
  try
    DeleteFile(DestFile);
  except
    // ignore mess
  end;
  {$ENDIF}
end;

var
  code: cardinal; //test result code: 0 for success
begin
  code:=0;
  try
    if FileExists(ParamStr(1)) then
    begin
      writeln('');
      writeln('Started investigating file '+ParamStr(1));
      ShowZipFile(ParamStr(1));
      writeln('Finished investigating file '+ParamStr(1));
      writeln('');
    end;

    writeln('CompareCompressDecompress started');
    try
      if not(CompareCompressDecompress) then code:=code+2; //1 already taken by callback handler
    except
      On E: Exception do
      begin
        writeln('Exception: '+E.Message);
        code:=code+2;
      end;
    end;
    writeln('CompareCompressDecompress finished');
    writeln('');

    writeln('CompressSmallStreams started');
    try
      if not(CompressSmallStreams) then code:=code+4;
    except
      On E: Exception do
      begin
        writeln('Exception: '+E.Message);
        code:=code+4;
      end;
    end;
    writeln('CompressSmallStreams finished');
    writeln('');

    writeln('TestZipEntries(2) started');
    try
      if not(TestZipEntries(2)) then code:=code+8;
    except
      On E: Exception do
      begin
        writeln('Exception: '+E.Message);
        code:=code+8;
      end;
    end;
    writeln('TestZipEntries(2) finished');
    writeln('');

    writeln('TestLargeFileName started');
    try
      if not(TestLargeFileName) then code:=code+16;
    except
      On E: Exception do
      begin
        writeln('Exception: '+E.Message);
        code:=code+16;
      end;
    end;
    writeln('TestLargeFileName finished');
    writeln('');

    writeln('TestWindowsPath started');
    try
      if not(TestWindowsPath) then code:=code+32;
    except
      On E: Exception do
      begin
        writeln('Exception: '+E.Message);
        code:=code+32;
      end;
    end;
    writeln('TestWindowsPath finished');
    writeln('');

    writeln('TestEmptyZipEntries(10) started');
    // Run testemptyzipentries with a small number to test the test itself... as
    // well as zip structure generated with empty files.
    try
      if not(TestEmptyZipEntries(10)) then code:=code+64;
    except
      On E: Exception do
      begin
        writeln('Exception: '+E.Message);
        code:=code+64;
      end;
    end;
    writeln('TestEmptyZipEntries(10) finished');
    writeln('');

    writeln('SaveToFileTest started');
    try
      if not(SaveToFileTest) then code:=code+128;
    except
      On E: Exception do
      begin
        writeln('Exception: '+E.Message);
        code:=code+128;
      end;
    end;
    writeln('SaveToFileTest finished');
    writeln('');

    writeln('TestEmptyZipEntries(65537) started');
    writeln('(note: this will take a long time)');
    {Note: tested tools with this file:
    - info-zip unzip 6.0
    - Ionic's DotNetZip library unzip.exe utility verison 1.9.1.8 works
    - 7zip's 7za 9.22 beta works.
    }
    try
      if not(TestEmptyZipEntries(65537)) then code:=code+256;
    except
      On E: Exception do
      begin
        writeln('Exception: '+E.Message);
        code:=code+256;
      end;
    end;
    writeln('TestEmptyZipEntries(65537) finished');
    writeln('');

    { This test will take a very long time as it tries to zip a 4Gb memory block.
    It is therefore commented out by default }
    {
    writeln('TestLargeZip64 - started');
    if not(TestLargeZip64) then code:=code+thefollowingstatuscode;
    writeln('TestLargeZip64 format - finished');
    writeln('');
    }
  except
    on E: Exception do
    begin
      writeln('');
      writeln('Exception: ');
      writeln(E.Message);
      writeln('');
      if code=0 then code:=maxint; //more or less random error code
    end;
  end;

  if code=0 then
    writeln('Basic zip/unzip tests passed: code '+inttostr(code))
  else
    writeln('Basic zip/unzip tests failed: code '+inttostr(code));
  Halt(code);
end.
