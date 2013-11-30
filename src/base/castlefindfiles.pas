{
  Copyright 2002-2013 Michalis Kamburelis.

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
    { Expanded (with absolute path) file name. }
    AbsoluteName: string;
    { Absolute URL. }
    URL: string;
    Directory: boolean;
    Size: Int64; //< This may be 0 in case of non-local file
  end;

  TFileInfoList = specialize TGenericStructList<TFileInfo>;

type
  TFoundFileProc = procedure (const FileInfo: TFileInfo; Data: Pointer);
  TFoundFileMethod = procedure (const FileInfo: TFileInfo) of object;

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
      something useful. }
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

  @param(FileProc Called on each file found.)

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

{ Find first file matching given Mask. The file can be anything (including
  a symlink or directory). It is not searched recursively, it is only searched
  for in the directory inside Mask.

  If found, returns @true and sets FileInfo.
  Otherwise, returns @false and leaves FileInfo undefined. }
function FindFirstFile(const Mask: string;
  out FileInfo: TFileInfo): boolean;

implementation

uses CastleFilesUtils, CastleURIUtils;

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
  FileProc: TFoundFileProc; FileProcData: Pointer): Cardinal;
var
  AbsoluteName, LocalPath: string;
  FileRec: TSearchRec;
  Attr, searchError: integer;
  FileInfo: TFileInfo;
begin
  Result := 0;

  Attr := faReadOnly or faHidden or faArchive { for symlinks } or faSysFile;
  if FindDirectories then
    Attr := Attr or faDirectory;

  LocalPath := InclPathDelim(URIToFilenameSafe(Path));
  SearchError := FindFirst(LocalPath + Mask, Attr, FileRec);
  try
    while SearchError = 0 do
    begin
      AbsoluteName := LocalPath + FileRec.Name;
      Inc(Result);

      FileInfo.AbsoluteName := AbsoluteName;
      FileInfo.Name := FileRec.Name;
      FileInfo.Directory := (FileRec.Attr and faDirectory) <> 0;
      FileInfo.Size := FileRec.Size;
      FileInfo.URL := FilenameToURISafe(AbsoluteName);
      FileProc(FileInfo, FileProcData);

      SearchError := FindNext(FileRec);
    end;
  finally FindClose(FileRec) end;
end;

{ This is equivalent to FindFiles with Recursive = true,
  and ReadAllFirst = false. }
function FindFiles_Recursive(const Path, Mask: string; const FindDirectories: boolean;
  FileProc: TFoundFileProc; FileProcData: Pointer;
  DirContentsLast: boolean): Cardinal;

  procedure WriteDirContent;
  begin
    Result := Result +
      FindFiles_NonRecursive(Path, Mask, FindDirectories, FileProc, FileProcData);
  end;

  procedure WriteSubdirs;
  var
    FileRec: TSearchRec;
    SearchError: integer;
    LocalPath: string;
  { go down recursively }
  begin
    LocalPath := InclPathDelim(URIToFilenameSafe(Path));
    SearchError := FindFirst(LocalPath + '*',
      faDirectory { potential flags on directory: } or faSysFile or faArchive,
      FileRec);
    try
      while SearchError = 0 do
      begin
        if ((faDirectory and FileRec.Attr) <> 0) and
           (not SpecialDirName(FileRec.Name)) then
          Result := Result +
            FindFiles_Recursive(FilenameToURISafe(LocalPath + FileRec.Name), Mask,
              FindDirectories, FileProc, FileProcData, DirContentsLast);
        SearchError := FindNext(FileRec);
      end;
    finally FindClose(FileRec) end;
  end;

begin
  Result := 0;

  if DirContentsLast then
  begin
    WriteSubdirs;
    WriteDirContent;
  end else
  begin
    WriteDirContent;
    WriteSubdirs;
  end;
end;

{ This is equivalent to FindFiles with ReadAllFirst = false. }
function FindFiles_NonReadAllFirst(const Path, Mask: string; FindDirectories: boolean;
  FileProc: TFoundFileProc; FileProcData: Pointer;
  Recursive, DirContentsLast: boolean): Cardinal;
begin
  if Recursive then
    Result := FindFiles_Recursive(Path, Mask, FindDirectories, fileProc, FileProcData, DirContentsLast) else
    Result := FindFiles_NonRecursive(Path, Mask, FindDirectories, fileProc, FileProcData);
end;

procedure FileProc_AddToFileInfos(
  const FileInfo: TFileInfo; Data: Pointer);
begin
  TFileInfoList(Data).Add(FileInfo);
end;

function FindFiles(const Path, Mask: string; const FindDirectories: boolean;
  FileProc: TFoundFileProc; FileProcData: Pointer;
  Options: TFindFilesOptions): Cardinal;
var
  FileInfos: TFileInfoList;
  i: Integer;
begin
  if ffReadAllFirst in Options then
  begin
    FileInfos := TFileInfoList.Create;
    try
      Result := FindFiles_NonReadAllFirst(Path, Mask, FindDirectories,
        @FileProc_AddToFileInfos, FileInfos,
        ffRecursive in Options,
        ffDirContentsLast in Options);
      for i := 0 to FileInfos.Count - 1 do
        FileProc(FileInfos.L[i], FileProcData);
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
  const FileInfo: TFileInfo; Data: Pointer);
begin
  PFoundFileMethodWrapper(Data)^.Contents(FileInfo);
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
  TSearchFileHard = class
    Base: string;
    Found: string;
    procedure Callback(const FileInfo: TFileInfo);
  end;
  BreakSearchFileHard = class(TCodeBreaker);

procedure TSearchFileHard.Callback(const FileInfo: TFileInfo);
begin
  if AnsiSameText(FileInfo.Name, Base) then
  begin
    Found := FileInfo.Name;
    raise BreakSearchFileHard.Create;
  end;
end;

function SearchFileHard(Path: string; const Base: string; out NewBase: string): boolean;
var
  S: TSearchFileHard;
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

  S := TSearchFileHard.Create;
  try
    try
      S.Base := Base;
      FindFiles(Path + '*', false, @S.Callback, []);
    except
      on BreakSearchFileHard do
      begin
        NewBase := S.Found;
        Result := true;
        Exit;
      end;
    end;
  finally FreeAndNil(S) end;
end;

type
  BreakFindFirstFile = class(TCodeBreaker)
    FoundFile: TFileInfo;
  end;

procedure FindFirstFileCallback(const FileInfo: TFileInfo; Data: Pointer);
var
  E: BreakFindFirstFile;
begin
  E := BreakFindFirstFile.Create;
  E.FoundFile := FileInfo;
  raise E;
end;

function FindFirstFile(const Mask: string; out FileInfo: TFileInfo): boolean;
begin
  try
    FindFiles(Mask, true, @FindFirstFileCallback, nil, []);
    Result := false;
  except
    on B: BreakFindFirstFile do
    begin
      Result := true;
      FileInfo := B.FoundFile;
    end;
  end;
end;

end.
