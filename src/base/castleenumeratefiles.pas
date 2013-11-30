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

{ Enumerating filenames matching some mask. }
unit CastleEnumerateFiles;

{$I castleconf.inc}

interface

uses SysUtils, CastleUtils, Classes, CastleGenericLists;

type
  { }
  TEnumeratedFileInfo = record
    { Filename, without any directory path. }
    Name: string;
    { Expanded (with absolute path) file name. }
    AbsoluteName: string;
    { Absolute URL. }
    URL: string;
    Directory: boolean;
    Size: Int64; //< This may be 0 in case of non-local file
  end;

  TEnumeratedFileInfoList = specialize TGenericStructList<TEnumeratedFileInfo>;

type
  TEnumFileProc =
    procedure (const FileInfo: TEnumeratedFileInfo; Data: Pointer);
  TEnumFileMethod =
    procedure (const FileInfo: TEnumeratedFileInfo) of object;

  TEnumFilesOption = (
    { If eoRecursive is in Options then EnumFiles (and friends) descend into
      subdirectories.

      Note that we always descend into @italic(every) subdirectory (not only
      into those matching Mask, that would be pretty useless (and is a problem
      with Unix shell expansion)).

      Note that including eoRecursive in Options
      is something completely different than the FindDirectories parameter:
      FindDirectories says whether to report directories to FileProc;
      eoRecursive says whether to descend into directories and enumerate
      their files too.

      Recursive does @italic(not) descend into symlinks to directories on Unix
      right now. The reason is that this would risk an infinite loop
      (unless some time-consuming precautions would be taken, or a level limit).
      In other words, it's just not implemented.
      But it @italic(may) be implemented someday, as this would be definitely
      something useful. }
    eoRecursive,

    { Determines the order of reporting directory contents.
      Meaningfull only if eoRecursive is also included in Options.

      If not included, then directory contents (files and directories
      matching mask) are reported to the callback first.
      And only then we enter the subdirectories.

      If eoDirContentsLast is included, then we first enter subdirectories.
      Only then we list directory contents to the callback. }
    eoDirContentsLast,

    { If eoReadAllFirst is included in the Options then before calling FileProc,
      we will first read @italic(all) file information to an internal array.
      And only then we will call FileProc for each item of this array.

      Why this may be useful? This way changes to the directory
      (like renaming / deleting / creating files) will not have any effect
      on the list of files we will get. This is important if our FileProc
      may modify the directory contents. Without eoReadAllFirst,
      it's undefined (OS-dependent and sometimes just random)
      if the new file names will appear in the directory list. }
    eoReadAllFirst);
  TEnumFilesOptions = set of TEnumFilesOption;

{ Find all files matching the given mask, and call FileProc for them.

  @param(Mask Path and filename mask. May have a path, absolute or relative.
    The final part (filename) may contain wildcards * and ?.)

  @param(FindDirectories Should directories also be found
    (reported by FileProc). Note that this is completely independent
    from whether we work recursively (eoRecursive in Options).)

  @param(FileProc Called on each file found.)

  @param(Options A set of options. See TEnumFilesOption for meaning
    of each option.)

  @returns(How many times FileProc was called, that is: how many
    files/directories were matching. Useful to report to user how many
    files were processed, in particular to warn if nothing was processed.)

  Some limitations of FindFirst/FindNext underneath are reflected in our
  functionality. @italic(Under Windows, mask is treated somewhat hacky):

  @orderedList(
    @item(Every filename conceptually has an extention under Windows,
      so *.* matches any file. On more sane OSes, *.* matches only
      files with a dot inside a filename.)

    @item(Extensions may be shortened to the first 3 letters.
      For example searching for @code(*.txt) actually searches
      for @code(*.txt*), that is it finds all the files with extensions
      starting from txt.)
  )

  @groupBegin }
function EnumFiles(const Mask: string; const FindDirectories: boolean;
  FileProc: TEnumFileProc; FileProcData: Pointer;
  Options: TEnumFilesOptions): Cardinal; overload;
function EnumFilesObj(const Mask: string; const FindDirectories: boolean;
  FileMethod: TEnumFileMethod;
  Options: TEnumFilesOptions): Cardinal; overload;
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
function EnumerateFirst(const Mask: string;
  out FileInfo: TEnumeratedFileInfo): boolean;

{ Scan recursively subdirectories of given path for files named Name.
  For each file, the HandleFile method is called.
  We always pass an absolute URL to the HandleFile method. }
procedure ScanForFiles(PathURL: string; const Name: string;
  const HandleFile: TEnumFileMethod);

implementation

uses CastleFilesUtils, CastleURIUtils;

{ EnumFiles ------------------------------------------------------------ }

{ This is equivalent to EnumFiles with Recursive = false
  and ReadAllFirst = false.

  Moreover, MaskFull must be a full, absolute filename with path
  (and disk under Windows).
  FPath must be ExtractFilePath(MaskFull). }
function EnumFiles_Core(const FPath, MaskFull: string;
  const FindDirectories: boolean;
  FileProc: TEnumFileProc; FileProcData: Pointer): Cardinal;
var
  AbsoluteName: string;
  FileRec: TSearchRec;
  Attr, searchError: integer;
  FileInfo: TEnumeratedFileInfo;
begin
  Result := 0;

  Attr := faReadOnly or faHidden or faArchive { for symlinks } or faSysFile;
  if FindDirectories then
    Attr := Attr or faDirectory;

  SearchError := FindFirst(MaskFull, Attr, FileRec);
  try
    while SearchError = 0 do
    begin
      AbsoluteName := FPath + FileRec.Name;
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

{ This is equivalent to EnumFiles with Recursive = false
  and ReadAllFirst = false. }
function EnumFiles_NonRecursive(const Mask: string; const FindDirectories: boolean;
  FileProc: TEnumFileProc; FileProcData: Pointer): Cardinal; overload;
var
  MaskFull: string;
begin
  MaskFull := ExpandFileName(Mask);
  result := EnumFiles_Core(ExtractFilePath(MaskFull), MaskFull, FindDirectories,
    FileProc, FileProcData);
end;

{ This is equivalent to EnumFiles with Recursive = true,
  and ReadAllFirst = false. }
function EnumFiles_Recursive(const Mask: string; const FindDirectories: boolean;
  FileProc: TEnumFileProc; FileProcData: Pointer;
  DirContentsLast: boolean): Cardinal;

  function EnumFilesRecursWewn(const Mask: string): Cardinal;
  { The Mask must contain an absolute path }
  var
    Directory: string;

    procedure WriteDirContent;
    begin
      result := result + EnumFiles_Core(Directory, Mask, FindDirectories,
        FileProc, FileProcData);
    end;

    procedure WriteSubdirs;
    var
      FileRec: TSearchRec;
      searchError: integer;
      FileRecName: string;
    { go down recursively }
    begin
      SearchError := FindFirst(Directory + '*',
        faDirectory { potential flags on directory: } or faSysFile or faArchive,
        FileRec);
      try
        while searchError = 0 do
        begin
          FileRecName := FileRec.Name;
          if ((faDirectory and FileRec.Attr) <> 0) and
             (not SpecialDirName(FileRecName)) then
          begin { go down recursively }
            result := result + EnumFilesRecursWewn(
              Directory +FileRecName +PathDelim +ExtractFileName(Mask));
          end;
          searchError := FindNext(FileRec);
        end;
      finally FindClose(FileRec) end;
    end;

  begin
    Directory := ExtractFilePath(Mask);
    result := 0;

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

begin
  result := EnumFilesRecursWewn(ExpandFileName(Mask));
end;

{ This is equivalent to EnumFiles with ReadAllFirst = false. }
function EnumFiles_NonReadAllFirst(const Mask: string; FindDirectories: boolean;
  FileProc: TEnumFileProc; FileProcData: Pointer;
  Recursive, DirContentsLast: boolean): Cardinal;
begin
  if Recursive then
    Result := EnumFiles_Recursive(Mask, FindDirectories, fileProc, FileProcData, DirContentsLast) else
    Result := EnumFiles_NonRecursive(Mask, FindDirectories, fileProc, FileProcData);
end;

procedure FileProc_AddToFileInfos(
  const FileInfo: TEnumeratedFileInfo; Data: Pointer);
begin
  TEnumeratedFileInfoList(Data).Add(FileInfo);
end;

function EnumFiles(const Mask: string; const FindDirectories: boolean;
  FileProc: TEnumFileProc; FileProcData: Pointer;
  Options: TEnumFilesOptions): Cardinal;
var
  FileInfos: TEnumeratedFileInfoList;
  i: Integer;
begin
  if eoReadAllFirst in Options then
  begin
    FileInfos := TEnumeratedFileInfoList.Create;
    try
      Result := EnumFiles_NonReadAllFirst(Mask, FindDirectories,
        @FileProc_AddToFileInfos, FileInfos,
        eoRecursive in Options,
        eoDirContentsLast in Options);
      for i := 0 to FileInfos.Count - 1 do
        FileProc(FileInfos.L[i], FileProcData);
    finally FileInfos.Free end;
  end else
  begin
    Result := EnumFiles_NonReadAllFirst(Mask, FindDirectories,
      FileProc, FileProcData,
      eoRecursive in Options,
      eoDirContentsLast in Options);
  end;
end;

{ EnumFilesObj ------------------------------------------------------------ }

type
  TEnumFileMethodWrapper = record
    Contents: TEnumFileMethod;
  end;
  PEnumFileMethodWrapper = ^TEnumFileMethodWrapper;

procedure EnumFileProcToMethod(
  const FileInfo: TEnumeratedFileInfo; Data: Pointer);
begin
  PEnumFileMethodWrapper(Data)^.Contents(FileInfo);
end;

function EnumFilesObj(const Mask: string; const FindDirectories: boolean;
  FileMethod: TEnumFileMethod; Options: TEnumFilesOptions): Cardinal;
var
  FileMethodWrapper: TEnumFileMethodWrapper;
begin
  FileMethodWrapper.Contents := FileMethod;
  Result := EnumFiles(Mask, FindDirectories,
    @EnumFileProcToMethod, @FileMethodWrapper, Options);
end;

type
  TSearchFileHard = class
    Base: string;
    Found: string;
    procedure Callback(const FileInfo: TEnumeratedFileInfo);
  end;
  BreakSearchFileHard = class(TCodeBreaker);

procedure TSearchFileHard.Callback(const FileInfo: TEnumeratedFileInfo);
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
      EnumFilesObj(Path + '*', false, @S.Callback, []);
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
  BreakEnumerateFirst = class(TCodeBreaker)
    FoundFile: TEnumeratedFileInfo;
  end;

procedure EnumerateFirstCallback(const FileInfo: TEnumeratedFileInfo; Data: Pointer);
var
  E: BreakEnumerateFirst;
begin
  E := BreakEnumerateFirst.Create;
  E.FoundFile := FileInfo;
  raise E;
end;

function EnumerateFirst(const Mask: string;
  out FileInfo: TEnumeratedFileInfo): boolean;
begin
  try
    EnumFiles(Mask, true, @EnumerateFirstCallback, nil, []);
    Result := false;
  except
    on B: BreakEnumerateFirst do
    begin
      Result := true;
      FileInfo := B.FoundFile;
    end;
  end;
end;

procedure ScanForFiles(PathURL: string; const Name: string;
  const HandleFile: TEnumFileMethod);
var
  Path: string;
begin
  Path := InclPathDelim(URIToFilenameSafe(PathURL));
  EnumFilesObj(Path + Name, true, HandleFile, [eoRecursive]);
end;

end.
