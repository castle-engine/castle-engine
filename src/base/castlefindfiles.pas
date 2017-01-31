{
  Copyright 2002-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Finding files matching some mask. }
unit CastleFindFiles;

{$I castleconf.inc}

interface

uses SysUtils, CastleUtils, Classes, CastleGenericLists;

type
  { }
  TFileInfo = record
    { Filename, without any directory path. }
    Name: string;
    { Expanded (with absolute path) file name.
      For local filesystem paths, this is an absolute filename.
      For Android asset, this is an absolute path within asset (note that
      absolute paths within Android assets do @italic(not) start with slash).
      It's adviced to use URL field instead of this. }
    AbsoluteName: string;
    { Absolute URL. }
    URL: string;
    Directory: boolean;
    Size: Int64; //< This may be 0 in case of non-local file
  end;

  TFileInfoList = specialize TGenericStructList<TFileInfo>;

  { Called for each file found.
    StopSearch is always initially @false, you can change it to @true to stop
    the enclosing FindFiles call. }
  TFoundFileProc = procedure (const FileInfo: TFileInfo; Data: Pointer; var StopSearch: boolean);
  TFoundFileMethod = procedure (const FileInfo: TFileInfo; var StopSearch: boolean) of object;

  TFindFilesOption = (
    { If ffRecursive is in Options then FindFiles (and friends) descend into
      subdirectories.

      Note that we always descend into @italic(every) subdirectory (not only
      into those matching Mask, that would be pretty useless (and is a problem
      with Unix shell expansion)).

      Note that including ffRecursive in Options
      is something completely different than the FindDirectories parameter:
      FindDirectories says whether to report directories to FileProc;
      ffRecursive says whether to descend into directories and enumerate
      their files too.

      Recursive does @italic(not) descend into symlinks to directories on Unix
      right now. The reason is that this would risk an infinite loop
      (unless some time-consuming precautions would be taken, or a level limit).
      In other words, it's just not implemented.
      But it @italic(may) be implemented someday, as this would be definitely
      something useful.

      For Android asset searching: note that recursive searching
      if unfortunately not supported. Although Android NDK contains functions
      to iterate over a files inside a directory, it stupidly omits
      the subdirectories (only returns the non-directory files). }
    ffRecursive,

    { Determines the order of reporting directory contents.
      Meaningfull only if ffRecursive is also included in Options.

      If not included, then directory contents (files and directories
      matching mask) are reported to the callback first.
      And only then we enter the subdirectories.

      If ffDirContentsLast is included, then we first enter subdirectories.
      Only then we list directory contents to the callback. }
    ffDirContentsLast,

    { If ffReadAllFirst is included in the Options then before calling FileProc,
      we will first read @italic(all) file information to an internal array.
      And only then we will call FileProc for each item of this array.

      Why this may be useful? This way changes to the directory
      (like renaming / deleting / creating files) will not have any effect
      on the list of files we will get. This is important if our FileProc
      may modify the directory contents. Without ffReadAllFirst,
      it's undefined (OS-dependent and sometimes just random)
      if the new file names will appear in the directory list. }
    ffReadAllFirst);
  TFindFilesOptions = set of TFindFilesOption;

{ Find all files matching the given mask, and call FileProc for them.

  @param(Path Path URL to search inside.
    May be absolute or relative. Like everywhere in our engine,
    it can also be a local filesystem path, although we advice using
    URLs for everything. May, but doesn't have to, end with slash or PathDelim.)

  @param(Mask Mask of the files to search, may contain wildcards * and ?.)

  @param(PathAndMask Overloaded versions without separate Path and Mask parameters
    just assume that PathAndMask contain concatenated Path + Mask,
    separated by any valid path delimiter.)

  @param(FindDirectories Should directories also be found
    (reported by FileProc). Note that this is completely independent
    from whether we work recursively (ffRecursive in Options).)

  @param(FileProc Called on each file found.
    May be @nil (useful if you are only interested in the number of files found,
    returned by this function).)

  @param(Options A set of options. See TFindFilesOption for meaning
    of each option.)

  @returns(How many times FileProc was called, that is: how many
    files/directories were matching. Useful to report to user how many
    files were processed, in particular to warn if nothing was processed.)

  @groupBegin }
function FindFiles(const Path, Mask: string; const FindDirectories: boolean;
  FileMethod: TFoundFileMethod; Options: TFindFilesOptions): Cardinal; overload;
function FindFiles(const Path, Mask: string; const FindDirectories: boolean;
  FileProc: TFoundFileProc; FileProcData: Pointer;
  Options: TFindFilesOptions): Cardinal; overload;

function FindFiles(const PathAndMask: string; const FindDirectories: boolean;
  FileMethod: TFoundFileMethod; Options: TFindFilesOptions): Cardinal; overload;
function FindFiles(const PathAndMask: string; const FindDirectories: boolean;
  FileProc: TFoundFileProc; FileProcData: Pointer;
  Options: TFindFilesOptions): Cardinal; overload;
{ @groupEnd }

{ Search for a file, ignoring the case.
  Path must be absolute URL and contain the final slash.
  Returns URL relative to Path.

  We prefer to return just Base, if it exists, or when no alternative exists.
  When Base doesn't exist but some likely alternative exists (e.g. with
  different case), we return it.

  Looks for normal files/symlinks, that can be opened as usual files.
  Not directories.

  Returns if some file was found. Note that even when we return @false,
  we still set NewBase (to original Base). }
function SearchFileHard(Path: string; const Base: string; out NewBase: string): boolean;

{ Find first file matching given Mask inside Path.
  If found, returns @true and sets FileInfo.
  Otherwise, returns @false and leaves FileInfo undefined. }
function FindFirstFile(const Path, Mask: string;
  const FindDirectories: boolean; const Options: TFindFilesOptions;
  out FileInfo: TFileInfo): boolean;

implementation

uses CastleURIUtils, CastleLog, StrUtils
  {$ifdef ANDROID}, CastleAndroidInternalAssetManager, CastleAndroidInternalAssetStream {$endif};

{ Note that some limitations of FindFirst/FindNext underneath are reflected in our
  functionality. Under Windows, mask is treated somewhat hacky:

  @orderedList(
    @item(Every filename conceptually has an extention under Windows,
      so *.* matches any file. On more sane OSes, *.* matches only
      files with a dot inside a filename.)

    @item(Extensions may be shortened to the first 3 letters.
      For example searching for @code(*.txt) actually searches
      for @code(*.txt*), that is it finds all the files with extensions
      starting from txt.)
  )
}

{ FindFiles ------------------------------------------------------------------ }

{ This is equivalent to FindFiles with Recursive = false
  and ReadAllFirst = false. }
function FindFiles_NonRecursive(const Path, Mask: string;
  const FindDirectories: boolean;
  FileProc: TFoundFileProc; FileProcData: Pointer;
  var StopSearch: boolean): Cardinal;

  procedure LocalFileSystem;
  var
    AbsoluteName, LocalPath: string;
    FileRec: TSearchRec;
    Attr, searchError: integer;
    FileInfo: TFileInfo;
  begin
    Result := 0;

    {$warnings off}
    { don't warn that faXxx are unportable }
    Attr := faReadOnly or faHidden or faArchive { for symlinks } or faSysFile;
    {$warnings on}
    if FindDirectories then
      Attr := Attr or faDirectory;

    if Path <> '' then
      LocalPath := URIToFilenameSafe(Path) else
      LocalPath := GetCurrentDir;
    LocalPath := InclPathDelim(LocalPath);
    SearchError := FindFirst(LocalPath + Mask, Attr, FileRec);
    try
      while (SearchError = 0) and (not StopSearch) do
      begin
        AbsoluteName := LocalPath + FileRec.Name;
        Inc(Result);

        FileInfo.AbsoluteName := AbsoluteName;
        FileInfo.Name := FileRec.Name;
        FileInfo.Directory := (FileRec.Attr and faDirectory) <> 0;
        FileInfo.Size := FileRec.Size;
        FileInfo.URL := FilenameToURISafe(AbsoluteName);
        if Assigned(FileProc) then
          FileProc(FileInfo, FileProcData, StopSearch);

        SearchError := FindNext(FileRec);
      end;
    finally FindClose(FileRec) end;
  end;

  {$ifdef ANDROID}
  procedure AndroidAssetFileSystem;
  var
    AssetDir, AssetName: string;
    Dir: PAAssetDir;
    FileInfo: TFileInfo;
  begin
    Result := 0;
    AssetDir := URIToAssetPath(Path);
    Dir := AAssetManager_openDir(AssetManager, PChar(ExclPathDelim(AssetDir)));
    if Dir <> nil then
    try
      repeat
        AssetName := AAssetDir_getNextFileName(Dir);
        if AssetName = '' then
          Break;

        if AssetDir <> '' then
          { Contrary to AAssetDir_getNextFileName docs, returned AssetName
            *does not* contain full path, we have to prefix it with AssetDir.
            See http://code.google.com/p/android/issues/detail?id=35079 }
          FileInfo.AbsoluteName := InclPathDelim(AssetDir) + AssetName else
          FileInfo.AbsoluteName := AssetName;
        FileInfo.Name := AssetName;
        { AAssetDir_getNextFileName never returns directories. }
        FileInfo.Directory := false;
        FileInfo.Size := 0;
        FileInfo.URL := AssetPathToURI(FileInfo.AbsoluteName);
        if IsWild(FileInfo.Name, Mask, false) then
        begin
          if Assigned(FileProc) then
            FileProc(FileInfo, FileProcData, StopSearch);
          Inc(Result);
        end;
      until StopSearch;
    finally AAssetDir_close(Dir) end;
  end;
  {$endif}

var
  P: string;
begin
  P := URIProtocol(Path);

  if (P = 'file') or (P = '') then
    LocalFileSystem else
  {$ifdef ANDROID}
  if P = 'assets' then
    AndroidAssetFileSystem else
  {$endif}
    WritelnLog('FindFiles',
      'Searching inside filesystem with protocol %s not possible', [P]);
end;

{ This is equivalent to FindFiles with Recursive = true,
  and ReadAllFirst = false. }
function FindFiles_Recursive(const Path, Mask: string; const FindDirectories: boolean;
  FileProc: TFoundFileProc; FileProcData: Pointer;
  DirContentsLast: boolean; var StopSearch: boolean): Cardinal;

  procedure WriteDirContent;
  begin
    Result := Result +
      FindFiles_NonRecursive(Path, Mask, FindDirectories, FileProc, FileProcData, StopSearch);
  end;

  { Search in subdirectories recursively. }
  procedure WriteSubdirs;
  var
    FileRec: TSearchRec;
    SearchError: integer;
    P, LocalPath: string;
  begin
    P := URIProtocol(Path);
    if P = 'file' then
      LocalPath := URIToFilenameSafe(Path) else
    if P = '' then
      LocalPath := Path else
    begin
      WritelnLog('FindFiles',
        'Searching inside subdirectories with protocol %s not possible', [P]);
      Exit;
    end;

    LocalPath := InclPathDelim(LocalPath);
    {$warnings off}
    SearchError := FindFirst(LocalPath + '*',
      faDirectory { potential flags on directory: } or faSysFile or faArchive,
      FileRec);
    {$warnings on}
    try
      while (SearchError = 0) and (not StopSearch) do
      begin
        if ((faDirectory and FileRec.Attr) <> 0) and
           (not SpecialDirName(FileRec.Name)) then
          Result := Result +
            FindFiles_Recursive(FilenameToURISafe(LocalPath + FileRec.Name), Mask,
              FindDirectories, FileProc, FileProcData, DirContentsLast, StopSearch);
        SearchError := FindNext(FileRec);
      end;
    finally FindClose(FileRec) end;
  end;

begin
  Result := 0;

  if DirContentsLast then
  begin
    WriteSubdirs;    if StopSearch then Exit;
    WriteDirContent; if StopSearch then Exit;
  end else
  begin
    WriteDirContent; if StopSearch then Exit;
    WriteSubdirs;    if StopSearch then Exit;
  end;
end;

{ This is equivalent to FindFiles with ReadAllFirst = false. }
function FindFiles_NonReadAllFirst(const Path, Mask: string; FindDirectories: boolean;
  FileProc: TFoundFileProc; FileProcData: Pointer;
  Recursive, DirContentsLast: boolean): Cardinal;
var
  StopSearch: boolean;
begin
  StopSearch := false;
  if Recursive then
    Result := FindFiles_Recursive(Path, Mask, FindDirectories, fileProc, FileProcData, DirContentsLast, StopSearch) else
    Result := FindFiles_NonRecursive(Path, Mask, FindDirectories, fileProc, FileProcData, StopSearch);
end;

procedure FileProc_AddToFileInfos(
  const FileInfo: TFileInfo; Data: Pointer; var StopSearch: boolean);
begin
  TFileInfoList(Data).Add(FileInfo);
end;

function FindFiles(const Path, Mask: string; const FindDirectories: boolean;
  FileProc: TFoundFileProc; FileProcData: Pointer;
  Options: TFindFilesOptions): Cardinal;
var
  FileInfos: TFileInfoList;
  i: Integer;
  StopSearch: boolean;
begin
  if ffReadAllFirst in Options then
  begin
    FileInfos := TFileInfoList.Create;
    try
      Result := FindFiles_NonReadAllFirst(Path, Mask, FindDirectories,
        @FileProc_AddToFileInfos, FileInfos,
        ffRecursive in Options,
        ffDirContentsLast in Options);
      if Assigned(FileProc) then
      begin
        StopSearch := false;
        for i := 0 to FileInfos.Count - 1 do
        begin
          FileProc(FileInfos.L[i], FileProcData, StopSearch);
          if StopSearch then Break;
        end;
      end;
    finally FileInfos.Free end;
  end else
  begin
    Result := FindFiles_NonReadAllFirst(Path, Mask, FindDirectories,
      FileProc, FileProcData,
      ffRecursive in Options,
      ffDirContentsLast in Options);
  end;
end;

function FindFiles(const PathAndMask: string; const FindDirectories: boolean;
  FileProc: TFoundFileProc; FileProcData: Pointer;
  Options: TFindFilesOptions): Cardinal;
begin
  Result := FindFiles(ExtractURIPath(PathAndMask), ExtractURIName(PathAndMask),
    FindDirectories, FileProc, FileProcData, Options);
end;

{ FindFiles with TFoundFileMethod -------------------------------------------- }

type
  TFoundFileMethodWrapper = record
    Contents: TFoundFileMethod;
  end;
  PFoundFileMethodWrapper = ^TFoundFileMethodWrapper;

procedure FoundFileProcToMethod(
  const FileInfo: TFileInfo; Data: Pointer; var StopSearch: boolean);
begin
  PFoundFileMethodWrapper(Data)^.Contents(FileInfo, StopSearch);
end;

function FindFiles(const Path, Mask: string; const FindDirectories: boolean;
  FileMethod: TFoundFileMethod; Options: TFindFilesOptions): Cardinal;
var
  FileMethodWrapper: TFoundFileMethodWrapper;
begin
  FileMethodWrapper.Contents := FileMethod;
  Result := FindFiles(Path, Mask, FindDirectories,
    @FoundFileProcToMethod, @FileMethodWrapper, Options);
end;

function FindFiles(const PathAndMask: string; const FindDirectories: boolean;
  FileMethod: TFoundFileMethod; Options: TFindFilesOptions): Cardinal;
begin
  Result := FindFiles(ExtractURIPath(PathAndMask), ExtractURIName(PathAndMask),
    FindDirectories, FileMethod, Options);
end;

{ related utilities ---------------------------------------------------------- }

type
  TSearchFileHardHelper = class
    Base: string;
    IsFound: boolean;
    Found: string;
    procedure Callback(const FileInfo: TFileInfo; var StopSearch: boolean);
  end;

  procedure TSearchFileHardHelper.Callback(const FileInfo: TFileInfo; var StopSearch: boolean);
  begin
    if AnsiSameText(FileInfo.Name, Base) then
    begin
      Found := FileInfo.Name;
      IsFound := true;
      StopSearch := true;
    end;
  end;

function SearchFileHard(Path: string; const Base: string; out NewBase: string): boolean;
var
  Helper: TSearchFileHardHelper;
  P: string;
begin
  NewBase := Base;
  Result := false;

  P := URIProtocol(Path);
  if P = 'file' then
    { convert Path to filename and continue }
    Path := URIToFilenameSafe(Path) else
  if P <> '' then
    { we can't do anything when protocol is not file or empty. }
    Exit(true);

  if FileExists(Path + Base) then Exit(true);

  Helper := TSearchFileHardHelper.Create;
  try
    Helper.Base := Base;
    FindFiles(Path + '*', false, @Helper.Callback, []);
    Result := Helper.IsFound;
    if Result then
      NewBase := Helper.Found;
  finally FreeAndNil(Helper) end;
end;

type
  TFindFirstFileHelper = class
    IsFound: boolean;
    FoundFile: TFileInfo;
    procedure Callback(const FileInfo: TFileInfo; var StopSearch: boolean);
  end;

  procedure TFindFirstFileHelper.Callback(const FileInfo: TFileInfo; var StopSearch: boolean);
  begin
    FoundFile := FileInfo;
    IsFound := true;
    StopSearch := true;
  end;

function FindFirstFile(const Path, Mask: string;
  const FindDirectories: boolean; const Options: TFindFilesOptions;
  out FileInfo: TFileInfo): boolean;
var
  Helper: TFindFirstFileHelper;
begin
  Helper := TFindFirstFileHelper.Create;
  try
    FindFiles(Path, Mask, FindDirectories, @Helper.Callback, Options);
    Result := Helper.IsFound;
    if Result then
      FileInfo := Helper.FoundFile;
  finally FreeAndNil(Helper) end;
end;

end.
