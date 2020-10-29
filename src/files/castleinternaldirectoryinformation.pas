{
  Copyright 2019-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Cached information about a directory (@link(TDirectoryInformation))
  and in particular about the current castle-data:/ contents (@link(DataDirectoryInformation)). }
unit CastleInternalDirectoryInformation;

{$I castleconf.inc}

interface

uses DOM, Generics.Collections,
  CastleFindFiles, CastleUtils;

type
  { Generate, load, save to file an information about files under given path.
    This is useful to store CastleDataInformation.xml file,
    that can be used to perform FindFiles on platforms/filesystems that don't support
    recursive searching for files/directories (like Android "assets"). }
  TDirectoryInformation = class
  public
    type
      TFile = class;
      TDirectory = class;
      TFiles = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TFile>;
      TDirectories = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TDirectory>;

      TEntry = class
      private
        procedure LoadFromFile(const Element: TDOMElement); virtual; abstract;
        procedure SaveToFile(const ParentElement: TDOMElement); virtual; abstract;
        procedure Sum(var DirsCount, FilesCount, FilesSize: QWord); virtual; abstract;
      public
        Name: String;
      end;

      TDirectory = class(TEntry)
      strict private
        procedure AddEntry(const FileInfo: TFileInfo; var StopSearch: boolean);
      private
        procedure Generate(const PathURL: String);
        procedure LoadFromFile(const Element: TDOMElement); override;
        procedure SaveToFile(const ParentElement: TDOMElement); override;
        procedure Sum(var DirsCount, FilesCount, FilesSize: QWord); override;
        { Find file or directory (direct children, not recursive) of given name.
          @nil if not found. }
        function FindEntry(const AName: String): TEntry;
      public
        Directories: TDirectories;
        Files: TFiles;
        constructor Create;
        destructor Destroy; override;
      end;

      TFile = class(TEntry)
      private
        procedure SaveToFile(const ParentElement: TDOMElement); override;
        procedure LoadFromFile(const Element: TDOMElement); override;
        procedure Sum(var DirsCount, FilesCount, FilesSize: QWord); override;
      public
        Size: QWord;
      end;

    var
      RootDirectory: TDirectory;

    destructor Destroy; override;
    procedure Generate(const PathURL: String);
    procedure LoadFromFile(const URL: String);
    procedure SaveToFile(const URL: String);
    procedure Sum(out DirsCount, FilesCount, FilesSize: QWord);

    { Find entry (file or directory) designated by given relative URL,
      like 'subdir1/subdir2/my_file.txt'.
      @nil if not found. }
    function FindEntry(const URL: String): TEntry;
  end;

var
  { When <> 0, FindFiles and URIFileExists and URIExists don't look at cached
    castle-data:/ contents in DataDirectoryInformation. }
  DisableDataDirectoryInformation: Integer;

function DataDirectoryInformation: TDirectoryInformation;

implementation

uses SysUtils,
  CastleXMLUtils, CastleURIUtils, CastleStringUtils, CastleLog;

{ TDirectory ----------------------------------------------------------------- }

procedure TDirectoryInformation.TDirectory.AddEntry(
  const FileInfo: TFileInfo; var StopSearch: boolean);
var
  D: TDirectory;
  F: TFile;
  E: TEntry;
begin
  if FileInfo.Directory then
  begin
    D := TDirectory.Create;
    D.Generate(FileInfo.URL);
    Directories.Add(D);
    E := D;
  end else
  begin
    F := TFile.Create;
    F.Size := FileInfo.Size;
    Files.Add(F);
    E := F;
  end;
  E.Name := FileInfo.Name;
end;

procedure TDirectoryInformation.TDirectory.Generate(const PathURL: String);
begin
  Directories.Clear;
  Files.Clear;
  FindFiles(PathURL, '*', true, {$ifdef CASTLE_OBJFPC}@{$endif} AddEntry, []);
end;

constructor TDirectoryInformation.TDirectory.Create;
begin
  inherited;
  Directories := TDirectories.Create(true);
  Files := TFiles.Create(true);
end;

destructor TDirectoryInformation.TDirectory.Destroy;
begin
  FreeAndNil(Files);
  FreeAndNil(Directories);
  inherited;
end;

procedure TDirectoryInformation.TDirectory.LoadFromFile(const Element: TDOMElement);
var
  D: TDirectory;
  F: TFile;
  I: TXMLElementIterator;
begin
  Name := Element.AttributeStringDef('name', '');

  I := Element.ChildrenIterator('directory');
  try
    while I.GetNext do
    begin
      D := TDirectory.Create;
      Directories.Add(D);
      D.LoadFromFile(I.Current);
    end;
  finally FreeAndNil(I) end;

  I := Element.ChildrenIterator('file');
  try
    while I.GetNext do
    begin
      F := TFile.Create;
      Files.Add(F);
      F.LoadFromFile(I.Current);
    end;
  finally FreeAndNil(I) end;
end;

procedure TDirectoryInformation.TDirectory.SaveToFile(const ParentElement: TDOMElement);
var
  Element: TDOMElement;
  F: TFile;
  D: TDirectory;
begin
  Element := ParentElement.CreateChild('directory');
  if Name <> '' then
    Element.AttributeSet('name', Name);
  for D in Directories do
    D.SaveToFile(Element);
  for F in Files do
    F.SaveToFile(Element);
end;

procedure TDirectoryInformation.TDirectory.Sum(var DirsCount, FilesCount, FilesSize: QWord);
var
  F: TFile;
  D: TDirectory;
begin
  Inc(DirsCount);
  for D in Directories do
    D.Sum(DirsCount, FilesCount, FilesSize);
  for F in Files do
    F.Sum(DirsCount, FilesCount, FilesSize);
end;

function TDirectoryInformation.TDirectory.FindEntry(const AName: String): TEntry;
var
  F: TFile;
  D: TDirectory;
begin
  for D in Directories do
    if SameFileName(D.Name, AName) then
      Exit(D);
  for F in Files do
    if SameFileName(F.Name, AName) then
      Exit(F);
  Result := nil;
end;

{ TFile ---------------------------------------------------------------------- }

procedure TDirectoryInformation.TFile.LoadFromFile(const Element: TDOMElement);
begin
  Name := Element.AttributeString('name');
  Size := Element.AttributeQWord('size');
end;

procedure TDirectoryInformation.TFile.SaveToFile(const ParentElement: TDOMElement);
var
  E: TDOMElement;
begin
  E := ParentElement.CreateChild('file');
  E.AttributeSet('name', Name);
  E.AttributeSet('size', Size);
end;

procedure TDirectoryInformation.TFile.Sum(var DirsCount, FilesCount, FilesSize: QWord);
begin
  Inc(FilesCount);
  FilesSize := FilesSize + Size;
end;

{ TDirectoryInformation ---------------------------------------------- }

destructor TDirectoryInformation.Destroy;
begin
  FreeAndNil(RootDirectory);
  inherited;
end;

procedure TDirectoryInformation.Generate(const PathURL: String);
begin
  FreeAndNil(RootDirectory);
  RootDirectory := TDirectory.Create;
  // RootDirectory.Name deliberately left empty, don't save

  Inc(DisableDataDirectoryInformation);
  try
    RootDirectory.Generate(PathURL);
  finally Dec(DisableDataDirectoryInformation) end;
end;

procedure TDirectoryInformation.LoadFromFile(const URL: String);
var
  Doc: TXMLDocument;
  DirectoryElement: TDOMElement;
begin
  FreeAndNil(RootDirectory);

  Doc := URLReadXML(URL);

  if Doc.DocumentElement.TagName <> 'directory_information' then
    raise Exception.Create('Cannot load TDirectoryInformation from file, XML root element must be <directory_information>');

  DirectoryElement := Doc.DocumentElement.Child('directory');
  RootDirectory := TDirectory.Create;
  RootDirectory.LoadFromFile(DirectoryElement);
end;

procedure TDirectoryInformation.SaveToFile(const URL: String);
var
  Doc: TXMLDocument;
  RootElement: TDOMElement;
  Comment: TDOMComment;
begin
  if RootDirectory = nil then
    raise Exception.Create('Cannot save TDirectoryInformation to file, no information loaded');

  Doc := TXMLDocument.Create;

  Comment := Doc.CreateComment(
    'DO NOT EDIT THIS FILE MANUALLY.' + NL +
    'DO NOT COMMIT THIS FILE TO THE VERSION CONTROL.' + NL +
    NL +
    'This file is automatically generated to describe the directory contents.' + NL +
    'This way we can access the directory information quicker,' + NL +
    'and we can search data files regardless if the OS API allows it.' + NL);
  Doc.AppendChild(Comment);

  RootElement := Doc.CreateElement('directory_information');
  Doc.AppendChild(RootElement);

  RootDirectory.SaveToFile(RootElement);

  URLWriteXML(Doc, URL);
end;

procedure TDirectoryInformation.Sum(out DirsCount, FilesCount, FilesSize: QWord);
begin
  DirsCount := 0;
  FilesCount := 0;
  FilesSize := 0;
  if RootDirectory <> nil then
    RootDirectory.Sum(DirsCount, FilesCount, FilesSize);
end;

function TDirectoryInformation.FindEntry(const URL: String): TEntry;
var
  SeekPos: Integer;
  Token: String;
begin
  if RootDirectory = nil then
    raise Exception.Create('Cannot search for entries in TDirectoryInformation, no information loaded');

  SeekPos := 1;
  Result := RootDirectory;
  repeat
    // although URL should only contain /, we tolerate here Windows \ also
    Token := NextToken(URL, SeekPos, ['/', '\']);
    if Token = '' then Break;

    if Result is TFile then
      Exit(nil); // path prefix is a name of a file, we cannot enter subdirectory of a file
    Result := TDirectory(Result).FindEntry(Token);
    if Result = nil then
      Exit; // path component doesn't exist
  until false;
end;

{ globals -------------------------------------------------------------------- }

var
  { We checked existence of CastleDataInformation.xml,
    and created and loaded FDataDirectoryInformation if it exists. }
  FDataDirectoryInformationInitialized: Boolean;
  FDataDirectoryInformation: TDirectoryInformation;

procedure InitializeDataDirectoryInformation;
var
  DataInfoURL, DataInfoProtocol: String;
  DirsCount, FilesCount, FilesSize: QWord;
begin
  DataInfoURL := ResolveCastleDataURL('castle-data:/auto_generated/CastleDataInformation.xml');
  DataInfoProtocol := URIProtocol(DataInfoURL);

  if (DataInfoProtocol = 'file') or
     (DataInfoProtocol = 'castle-nx-contents') or
     (DataInfoProtocol = '') then
  begin
    { To avoid exceptions during debugging,
      follow a different approach when castle-data:/ resolves to something that
      has reliable URIFileExists implementation: check by URIFileExists. }
    Inc(DisableDataDirectoryInformation);
    try
      if URIFileExists(DataInfoURL) then
      begin
        FDataDirectoryInformation := TDirectoryInformation.Create;
        FDataDirectoryInformation.LoadFromFile(DataInfoURL);
      end else
        FDataDirectoryInformation := nil;
    finally Dec(DisableDataDirectoryInformation) end;
  end else
  begin
    { With other protocols, the only way is to try opening
      DataInfoURL and see whether there's exception. }
    FDataDirectoryInformation := TDirectoryInformation.Create;
    try
      FDataDirectoryInformation.LoadFromFile(DataInfoURL);
    except
      on E: Exception do
      begin
        WritelnLog('Cannot read CastleDataInformation.xml: ' + ExceptMessage(E));
        FreeAndNil(FDataDirectoryInformation);
      end;
    end;
  end;

  if FDataDirectoryInformation <> nil then
  begin
    FDataDirectoryInformation.Sum(DirsCount, FilesCount, FilesSize);
    WritelnLog(Format('Loaded CastleDataInformation.xml information. Data contains %d directories, %d files, total (uncompressed) size %s.',
      [DirsCount, FilesCount, SizeToStr(FilesSize)]));
  end;
end;

function DataDirectoryInformation: TDirectoryInformation;
begin
  if not FDataDirectoryInformationInitialized then
  begin
    InitializeDataDirectoryInformation;
    FDataDirectoryInformationInitialized := true;
  end;

  Result := FDataDirectoryInformation;
end;

initialization
finalization
  FreeAndNil(FDataDirectoryInformation);
end.
