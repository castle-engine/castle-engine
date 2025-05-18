{
  Copyright 2002-2025 Michalis Kamburelis.

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

uses SysUtils, Classes, Generics.Collections, DOM,
  CastleUtils;

type
  { Information about a single file or directory collected by FindFiles. }
  TFileInfo = record
    { Filename, without any directory path. }
    Name: String;
    { Expanded (with absolute path) file name.
      Only when URL is using "file" protocol.
      You should prefer to use URL field instead of this,
      to work with all possible URLs. }
    AbsoluteName: String;
    { Absolute URL. }
    Url: String;
    Directory: Boolean;

    { Whether this is a symbolic link.
      Note that this is independent from Directory:
      symlinks may have Directory=false (when the symlink is to file)
      or Directory=true (when the symlink is to directory,
      and thus can be browsed like directory). }
    Symlink: Boolean;

    { File size in bytes.

      Undefined in case of @link(Directory), we do not sum the directory size
      (all files inside) here, as it is a costly operation for large directories.
      Use @link(DirectorySize) if you want to calculate directory size.

      This may be 0 in case of URL pointing to non-local file, e.g. http
      (though we cannot search in such URLs anyway). }
    Size: QWord;
  end;

  { Returned by FindFilesList. }
  TFileInfoList = class({$ifdef FPC}specialize{$endif} TStructList<TFileInfo>)
    { Sort alphabetically by @link(TFileInfo.Url).
      Useful, since the order returned by @link(FindFilesList) is undefined. }
    procedure SortUrls;
  end;

  { Called for each file found.
    StopSearch is always initially @false, you can change it to @true to stop
    the enclosing FindFiles call. }
  TFoundFileProc = procedure (const FileInfo: TFileInfo; Data: Pointer; var StopSearch: Boolean);
  TFoundFileMethod = procedure (const FileInfo: TFileInfo; var StopSearch: Boolean) of object;

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
    URLs for everything. May, but doesn't have to, end with slash or PathDelim.
    Empty Path means "the current directory".)

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

  @param(FileProcData Pointer passed to every call of FileProc.
    This routine just passes FileProcData value as "Data" parameter to each FileProc call.
    It is a pointer that may have absolutely any meaning you want,
    and may point to any data structure you want,
    it is useful to communicate information between the caller and the FileProc implementation.

    If you don't need this, then just ignore the "Data" in your FileProc implementation,
    and pass anything (like @nil) as FileProcData value.

    Note that the overloaded version with FileMethod parameter doesn't have
    any FileProcData, as in this case the instance that implements FileMethod
    may carry any additional information necessary.)

  @param(Options A set of options. See TFindFilesOption for meaning
    of each option.)

  @returns(How many times FileProc was called, that is: how many
    files/directories were matching. Useful to report to user how many
    files were processed, in particular to warn if nothing was processed.)

  @groupBegin }
function FindFiles(const Path, Mask: string; const FindDirectories: boolean;
  const FileMethod: TFoundFileMethod; const Options: TFindFilesOptions): Cardinal; overload;
function FindFiles(const Path, Mask: string; const FindDirectories: boolean;
  const FileProc: TFoundFileProc; const FileProcData: Pointer;
  const Options: TFindFilesOptions): Cardinal; overload;

function FindFiles(const PathAndMask: string; const FindDirectories: boolean;
  const FileMethod: TFoundFileMethod; const Options: TFindFilesOptions): Cardinal; overload;
function FindFiles(const PathAndMask: string; const FindDirectories: boolean;
  const FileProc: TFoundFileProc; const FileProcData: Pointer;
  const Options: TFindFilesOptions): Cardinal; overload;
{ @groupEnd }

{ Like @link(FindFiles), but return found files as a list.
  Caller is resopnsible for freeing the returned list. }
function FindFilesList(const Path, Mask: string; const FindDirectories: boolean;
  const Options: TFindFilesOptions): TFileInfoList;

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

{ Find first file matching given Mask inside Path, always ignores case
  (regardless of OS conventions).
  If found, returns @true and sets FileInfo.
  Otherwise, returns @false and leaves FileInfo undefined. }
function FindFirstFileIgnoreCase(const Path, Mask: string;
  const FindDirectories: boolean; const Options: TFindFilesOptions;
  out FileInfo: TFileInfo): boolean;

implementation

uses URIParser, StrUtils, Generics.Defaults,
  CastleUriUtils, CastleLog, CastleXmlUtils, CastleStringUtils,
  CastleInternalDirectoryInformation, CastleFilesUtils, CastleDownload;

{ TFileInfoList -------------------------------------------------------------- }

function CompareFileInfo(
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif}
  Left, Right: TFileInfo): Integer;
begin
  Result := AnsiCompareStr(Left.Url, Right.Url);
end;

procedure TFileInfoList.SortUrls;
type
  TFileInfoComparer = {$ifdef FPC}specialize{$endif} TComparer<TFileInfo>;
begin
  Sort(TFileInfoComparer.Construct({$ifdef FPC}@{$endif} CompareFileInfo));
end;

{ TFindFilesCastleConfigProxy ----------------------------------------------- }

type
  { Utility to use FindFiles_xxx without castle-config protocol,
    and for all files enumarated -- try to add there castle-config
    to the reported URL. }
  TFindFilesCastleConfigProxy = class
    OriginalFileProc: TFoundFileProc;
    OriginalFileProcData: Pointer;
    function ProxyFileProcData: Pointer;
  end;

function TFindFilesCastleConfigProxy.ProxyFileProcData: Pointer;
begin
  Result := Pointer(Self);
end;

procedure ProxyFileProc(
  const FileInfo: TFileInfo; Data: Pointer; var StopSearch: Boolean);
var
  DataUtility: TFindFilesCastleConfigProxy;
  NewFileInfo: TFileInfo;
begin
  DataUtility := TFindFilesCastleConfigProxy(Data);
  NewFileInfo := FileInfo;
  NewFileInfo.Url := MaybeUseCastleConfigProtocol(NewFileInfo.Url);
  DataUtility.OriginalFileProc(NewFileInfo, DataUtility.OriginalFileProcData, StopSearch);
end;

{ FindFiles_NonRecursive ----------------------------------------------------- }

type
  { Utility to implement FindFiles_NonRecursive for URLs with protocol
    that has TRegisteredProtocol.FindFilesEvent handler. }
  TFindFiles_NonRecursiveEventUtility = class
    FindDirectories: Boolean;
    FileProc: TFoundFileProc;
    FileProcData: Pointer;
    Count: Cardinal;
    procedure Callback(const FileInfo: TFileInfo; var StopSearch: Boolean);
  end;

procedure TFindFiles_NonRecursiveEventUtility.Callback(
  const FileInfo: TFileInfo; var StopSearch: Boolean);
begin
  { We filter out directories here, because TUrlFindFilesEvent,
    for ease of implementation, is expected to return all files and directories. }
  if (not FileInfo.Directory) or FindDirectories then
  begin
    Inc(Count);
    FileProc(FileInfo, FileProcData, StopSearch);
  end;
end;

{ Implement FindFiles in a special case:
  - Recursive = false (so just list files/directories in the given directory)
  - ReadAllFirst = false (so order doesn't matter) }
function FindFiles_NonRecursive(const Path, Mask: String;
  const FindDirectories: Boolean;
  const FileProc: TFoundFileProc; const FileProcData: Pointer;
  var StopSearch: Boolean): Cardinal;

  { Implementation when Path is '' (current directory)
    or maps to a regular filename using UriToFilenameSafe.
    Always sets Result to the number of calls of FileProc. }
  procedure UseLocalFileSystem;

  { This follows standard FindFirst/FindNext behavior,
    which on Windows match the Windows API specific behavior,
    a bit weird but expected by Windows users, it seems:

    @orderedList(
      @item(On Windows, every filename conceptually "has an extension".
        Which means that *.* matches any file.

        On more sane OSes (everything non-Windows), *.* matches only
        files with a dot inside a filename. Use "*" to match everything.)

      @item(Doesn't matter anymore:

        By default, in Windows API and FindFirst/FindNext,
        extensions may be shortened to the first 3 letters.
        For example searching for @code(*.txt) actually searches
        for @code(*.txt*), that is it finds all the files with extensions
        starting from txt.

        So e.g. searching for '*.pas' will also find 'emacs-backup-file.pas~' .

        To be sensible, and to be consistent across platforms, we fix it by
        additional IsWild check, following DOUBLE_CHECK_WILDCARD define below.
        This also matters e.g. for Pascal files list found by
        "castle-engine generate-program" -- we don't want there
        'emacs-backup-file.pas~'.
      )
    )
  }
  {$ifdef MSWINDOWS}
    {$define DOUBLE_CHECK_WILDCARD}
  {$endif}

  var
    AbsoluteName, LocalPath: string;
    FileRec: TSearchRec;
    Attr, SearchError: Integer;
    FileInfo: TFileInfo;
  begin
    Result := 0;

    {$warnings off} // don't warn that faXxx are unportable
    Attr := faReadOnly or faHidden or faArchive
      { for symlinks with old FPC versions } or faSysFile
      { for symlinks with new FPC versions, at least FPC >= 3.0.2 } or faSymLink;
    {$warnings on}
    if FindDirectories then
      Attr := Attr or faDirectory;

    if Path <> '' then
      LocalPath := UriToFilenameSafe(Path)
    else
      LocalPath := GetCurrentDir;
    LocalPath := InclPathDelim(LocalPath);
    SearchError := FindFirst(LocalPath + Mask, Attr, FileRec);
    try
      while (SearchError = 0) and (not StopSearch) do
      begin
        // do not enumerate directory names '.' and '..'
        if not (
          ((FileRec.Attr and faDirectory) <> 0) and
          SpecialDirName(FileRec.Name))
          {$ifdef DOUBLE_CHECK_WILDCARD}
          and IsWild(FileRec.Name, Mask, { IgnoreCase } not FileNameCaseSensitive)
          {$endif}
          then
        begin
          AbsoluteName := LocalPath + FileRec.Name;
          Inc(Result);

          FileInfo := Default(TFileInfo);
          FileInfo.AbsoluteName := AbsoluteName;
          FileInfo.Name := FileRec.Name;
          FileInfo.Directory := (FileRec.Attr and faDirectory) <> 0;
          FileInfo.Size := FileRec.Size;
          {$warnings off} // we know faSymLink is platform-specific, this is OK
          FileInfo.Symlink := (FileRec.Attr and faSymLink) <> 0;
          {$warnings on}
          FileInfo.URL := FilenameToUriSafe(AbsoluteName);
          if Assigned(FileProc) then
            FileProc(FileInfo, FileProcData, StopSearch);
        end;

        SearchError := FindNext(FileRec);
      end;
    finally FindClose(FileRec) end;
  end;

  { Implementation when Path has protocol 'castle-data'.
    Always sets Result to the number of calls of FileProc. }
  procedure UseDataDirectoryInformation;
  var
    U: TURI;
    PathPartsStr: String;
    PathEntry: TDirectoryInformation.TEntry;
    F: TDirectoryInformation.TFile;
    PathDir, D: TDirectoryInformation.TDirectory;
    FileInfo: TFileInfo;
  begin
    Result := 0;

    U := ParseURI(Path);
    PathPartsStr := PrefixRemove('/', U.Path + U.Document, false);
    PathEntry := DataDirectoryInformation.FindEntry(PathPartsStr);
    if PathEntry is TDirectoryInformation.TDirectory then
    begin
      PathDir := TDirectoryInformation.TDirectory(PathEntry);

      if FindDirectories then
        for D in PathDir.Directories do
          if IsWild(D.Name, Mask, false) then
          begin
            Inc(Result);
            FileInfo := Default(TFileInfo); // clear information like FileInfo.Size to zero
            FileInfo.Name := D.Name;
            FileInfo.Directory := true;
            FileInfo.URL := UriIncludeSlash(Path) + D.Name;
            if Assigned(FileProc) then
            begin
              FileProc(FileInfo, FileProcData, StopSearch);
              if StopSearch then Break;
            end;
          end;

      if not StopSearch then
        for F in PathDir.Files do
          if IsWild(F.Name, Mask, false) then
          begin
            Inc(Result);
            FileInfo := Default(TFileInfo);
            FileInfo.Name := F.Name;
            FileInfo.Directory := false;
            FileInfo.Size := F.Size;
            FileInfo.URL := UriIncludeSlash(Path) + F.Name;
            FileInfo.Symlink := false; // packaged data cannot contain symlinks, as they are not portable to all platforms
            if Assigned(FileProc) then
            begin
              FileProc(FileInfo, FileProcData, StopSearch);
              if StopSearch then Break;
            end;
          end;
    end;
  end;

  { Implementation when Path has protocol with @link(TRegisteredProtocol.FindFilesEvent)
    handler.
    Always sets Result to the number of calls of FileProc. }
  procedure UseEvent(const Event: TUrlFindFilesEvent);
  var
    Utility: TFindFiles_NonRecursiveEventUtility;
  begin
    Utility := TFindFiles_NonRecursiveEventUtility.Create;
    try
      Utility.FindDirectories := FindDirectories;
      Utility.FileProc := FileProc;
      Utility.FileProcData := FileProcData;
      Utility.Count := 0;
      Event(Path, Mask, {$ifdef FPC}@{$endif} Utility.Callback, StopSearch);
      Result := Utility.Count;
    finally FreeAndNil(Utility) end;
  end;

var
  P: String;
  R: TRegisteredProtocol;
  ConfigProxy: TFindFilesCastleConfigProxy;
begin
  P := URIProtocol(Path);

  if (P = 'file') or (P = '') then
    UseLocalFileSystem
  else
  if P = 'castle-data' then
  begin
    if (DisableDataDirectoryInformation = 0) and
       (DataDirectoryInformation <> nil) then
      UseDataDirectoryInformation
    else
      // resolve URL using ResolveCastleDataUrl, and make recursive call
      Result := FindFiles_NonRecursive(ResolveCastleDataUrl(Path), Mask,
        FindDirectories, FileProc, FileProcData, StopSearch);
  end else
  if P = 'castle-config' then
  begin
    // resolve URL using ResolveCastleConfigUrl, and make recursive call
    ConfigProxy := TFindFilesCastleConfigProxy.Create;
    try
      ConfigProxy.OriginalFileProc := FileProc;
      ConfigProxy.OriginalFileProcData := FileProcData;
      Result := FindFiles_NonRecursive(ResolveCastleConfigUrl(Path), Mask,
        FindDirectories,
        {$ifdef FPC}@{$endif} ProxyFileProc, ConfigProxy.ProxyFileProcData, StopSearch);
    finally FreeAndNil(ConfigProxy) end;
    // This would also work, but user code would see the URLs without 'castle-config':
    // Result := FindFiles_NonRecursive(ResolveCastleConfigUrl(Path), Mask,
    //   FindDirectories, FileProc, FileProcData, StopSearch);
  end else
  begin
    R := FindRegisteredUrlProtocol(P);
    if R <> nil then
    begin
      if Assigned(R.FindFilesEvent) then
        UseEvent(R.FindFilesEvent)
      else
      begin
        Result := 0;
        WritelnLog('FindFiles', 'Searching inside filesystem with protocol %s not possible, ignoring path "%s"', [
          P,
          UriCaption(Path)
        ]);
      end;
    end else
    begin
      Result := 0;
      WritelnLog('FindFiles', 'URL protocol %s not known, not searching inside "%s"', [
        P,
        UriCaption(Path)
      ]);
    end;
  end;
end;

{ FindFiles_Recursive ------------------------------------------------------- }

function FindFiles_Recursive(const Path, Mask: string; const FindDirectories: boolean;
  const FileProc: TFoundFileProc; const FileProcData: Pointer;
  const DirContentsLast: boolean; var StopSearch: Boolean): Cardinal; forward;

type
  { Utility for FindFiles_Recursive to find all subdirectories
    and call FindFiles_Recursive for each of them. }
  TFindFiles_RecursiveSearchSubdirectoriesUtility = class
    Path, Mask: String;
    FindDirectories: Boolean;
    FileProc: TFoundFileProc;
    FileProcData: Pointer;
    DirContentsLast: Boolean;
    Count: Cardinal;
    procedure Callback(const FileInfo: TFileInfo; var StopSearch: Boolean);
  end;

procedure TFindFiles_RecursiveSearchSubdirectoriesUtility.Callback(
  const FileInfo: TFileInfo; var StopSearch: Boolean);
begin
  if FileInfo.Directory then
  begin
    Count := Count +
      FindFiles_Recursive(UriIncludeSlash(FileInfo.URL), Mask, FindDirectories,
        FileProc, FileProcData, DirContentsLast, StopSearch);
  end;
end;

{ Implement FindFiles for a specific case:
  - Recursive = true (so we enter all subdirectories)
  - ReadAllFirst = false (so order doesn't matter). }
function FindFiles_Recursive(const Path, Mask: string; const FindDirectories: boolean;
  const FileProc: TFoundFileProc; const FileProcData: Pointer;
  const DirContentsLast: boolean; var StopSearch: Boolean): Cardinal;

  { Search for files/directories in the current directory matching Mask.
    This just calls FindFiles_NonRecursive and increases Result. }
  procedure SearchDirContents;
  begin
    Result := Result +
      FindFiles_NonRecursive(Path, Mask, FindDirectories, FileProc, FileProcData, StopSearch);
  end;

  { Do SearchSubdirectories when Path has 'file' protocol (or '', just filename).
    So call FindFiles_Recursive for each subdirectory.
    Increase Result by the number of calls to FileProc. }
  procedure UseLocalFileSystem;
  var
    LocalPath: string;
    FileRec: TSearchRec;
    SearchError: integer;
  begin
    if Path <> '' then
      LocalPath := UriToFilenameSafe(Path)
    else
      LocalPath := GetCurrentDir;
    LocalPath := InclPathDelim(LocalPath);

    {$warnings off}
    SearchError := FindFirst(LocalPath + '*',
      faDirectory { potential flags on directory: } or faSysFile or faArchive or faReadOnly or faHidden,
      FileRec);
    {$warnings on}
    try
      while (SearchError = 0) and (not StopSearch) do
      begin
        if ((faDirectory and FileRec.Attr) <> 0) and
           (not SpecialDirName(FileRec.Name)) then
        begin
          Result := Result +
            FindFiles_Recursive(FilenameToUriSafe(LocalPath + FileRec.Name), Mask,
              FindDirectories, FileProc, FileProcData, DirContentsLast, StopSearch);
        end;
        SearchError := FindNext(FileRec);
      end;
    finally FindClose(FileRec) end;
  end;

  { Do SearchSubdirectories when Path has 'castle-data' protocol
    and we have DataDirectoryInformation.
    So call FindFiles_Recursive for each subdirectory.
    Increase Result by the number of calls to FileProc. }
  procedure UseDataDirectoryInformation;
  var
    U: TURI;
    PathPartsStr: String;
    PathEntry: TDirectoryInformation.TEntry;
    PathDir, D: TDirectoryInformation.TDirectory;
  begin
    U := ParseURI(Path);
    PathPartsStr := PrefixRemove('/', U.Path + U.Document, false);
    PathEntry := DataDirectoryInformation.FindEntry(PathPartsStr);
    if PathEntry is TDirectoryInformation.TDirectory then
    begin
      PathDir := TDirectoryInformation.TDirectory(PathEntry);
      for D in PathDir.Directories do
      begin
        Result := Result +
          FindFiles_Recursive(UriIncludeSlash(Path) + D.Name, Mask,
            FindDirectories, FileProc, FileProcData, DirContentsLast, StopSearch);
        if StopSearch then Break;
      end;
    end;
  end;

  { Do SearchSubdirectories when Path has protocol
    with TRegisteredProtocol.FindFilesEvent handler.
    So call FindFiles_Recursive for each subdirectory.
    Increase Result by the number of calls to FileProc. }
  procedure UseEvent(const Event: TUrlFindFilesEvent);
  var
    Utility: TFindFiles_RecursiveSearchSubdirectoriesUtility;
  begin
    Utility := TFindFiles_RecursiveSearchSubdirectoriesUtility.Create;
    try
      Utility.Path := Path;
      Utility.Mask := Mask;
      Utility.FindDirectories := FindDirectories;
      Utility.FileProc := FileProc;
      Utility.FileProcData := FileProcData;
      Utility.DirContentsLast := DirContentsLast;
      // Find *, not Mask, to find all subdirectories
      Event(Path, '*', {$ifdef FPC}@{$endif} Utility.Callback, StopSearch);
      Result := Result + Utility.Count;
    finally FreeAndNil(Utility) end;
  end;

  { Search in subdirectories recursively.
    This means we find all subdirectories (ignoring Mask) and call
    FindFiles_Recursive for each of them.
    Increase Result by the number of calls to FileProc. }
  procedure SearchSubdirectories;
  var
    P: String;
    R: TRegisteredProtocol;
  begin
    P := URIProtocol(Path);

    if (P = 'file') or (P = '') then
      UseLocalFileSystem
    else
    if P = 'castle-data' then
    begin
      if (DisableDataDirectoryInformation = 0) and
         (DataDirectoryInformation <> nil) then
        UseDataDirectoryInformation
      else
        raise EInternalError.Create('This case should cause recursive exit earlier in FindFiles_Recursive');
    end else
    begin
      R := FindRegisteredUrlProtocol(P);
      if R <> nil then
      begin
        if Assigned(R.FindFilesEvent) then
          UseEvent(R.FindFilesEvent)
        else
        begin
          WritelnLog('FindFiles', 'Searching inside filesystem with protocol %s not possible, ignoring path "%s"', [
            P,
            UriCaption(Path)
          ]);
        end;
      end else
      begin
        WritelnLog('FindFiles', 'URL protocol %s not known, not searching inside "%s"', [
          P,
          UriCaption(Path)
        ]);
      end;
    end;
  end;

var
  P: String;
  ConfigProxy: TFindFilesCastleConfigProxy;
begin
  Result := 0;

  P := URIProtocol(Path);

  { early exit if we should do ResolveCastleDataUrl and make recursive call }
  if (P = 'castle-data') and
     not ( (DisableDataDirectoryInformation = 0) and
           (DataDirectoryInformation <> nil) ) then
  begin
    Exit(FindFiles_Recursive(ResolveCastleDataUrl(Path), Mask,
      FindDirectories, FileProc, FileProcData, DirContentsLast, StopSearch));
  end;
  { early exit if we should do ResolveCastleConfigUrl and make recursive call }
  if P = 'castle-config' then
  begin
    ConfigProxy := TFindFilesCastleConfigProxy.Create;
    try
      ConfigProxy.OriginalFileProc := FileProc;
      ConfigProxy.OriginalFileProcData := FileProcData;
      Exit(FindFiles_Recursive(ResolveCastleConfigUrl(Path), Mask,
        FindDirectories,
        {$ifdef FPC}@{$endif} ProxyFileProc, ConfigProxy.ProxyFileProcData, DirContentsLast, StopSearch));
    finally FreeAndNil(ConfigProxy) end;
    // This would also work, but user code would see the URLs without 'castle-config':
    // Exit(FindFiles_Recursive(ResolveCastleConfigUrl(Path), Mask,
    //   FindDirectories, FileProc, FileProcData, DirContentsLast, StopSearch));
  end;

  if DirContentsLast then
  begin
    SearchSubdirectories; if StopSearch then Exit;
    SearchDirContents;    if StopSearch then Exit;
  end else
  begin
    SearchDirContents;    if StopSearch then Exit;
    SearchSubdirectories; if StopSearch then Exit;
  end;
end;

{ Other FindFiles functions -------------------------------------------------- }

{ This is equivalent to FindFiles with ReadAllFirst = false. }
function FindFiles_NonReadAllFirst(const Path, Mask: string; const FindDirectories: boolean;
  const FileProc: TFoundFileProc; const FileProcData: Pointer;
  const Recursive, DirContentsLast: boolean): Cardinal;
var
  StopSearch: Boolean;
begin
  StopSearch := false;
  if Recursive then
    Result := FindFiles_Recursive(Path, Mask, FindDirectories, fileProc, FileProcData, DirContentsLast, StopSearch)
  else
    Result := FindFiles_NonRecursive(Path, Mask, FindDirectories, fileProc, FileProcData, StopSearch);
end;

procedure FileProc_AddToFileInfos(
  const FileInfo: TFileInfo; Data: Pointer; var StopSearch: Boolean);
begin
  TFileInfoList(Data).Add(FileInfo);
end;

function FindFiles(const Path, Mask: string; const FindDirectories: boolean;
  const FileProc: TFoundFileProc; const FileProcData: Pointer;
  const Options: TFindFilesOptions): Cardinal;
var
  FileInfos: TFileInfoList;
  i: Integer;
  StopSearch: Boolean;
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
  const FileProc: TFoundFileProc; const FileProcData: Pointer;
  const Options: TFindFilesOptions): Cardinal;
begin
  Result := FindFiles(ExtractURIPath(PathAndMask), ExtractURIName(PathAndMask),
    FindDirectories, FileProc, FileProcData, Options);
end;

{ FindFiles with TFoundFileMethod -------------------------------------------- }

type
  TFoundFileMethodWrapper = record
    FileMethod: TFoundFileMethod;
  end;
  PFoundFileMethodWrapper = ^TFoundFileMethodWrapper;

procedure FoundFileProcToMethod(
  const FileInfo: TFileInfo; Data: Pointer; var StopSearch: Boolean);
var
  FileMethod: TFoundFileMethod;
begin
  FileMethod := PFoundFileMethodWrapper(Data)^.FileMethod;
  if Assigned(FileMethod) then
    FileMethod(FileInfo, StopSearch);
end;

function FindFiles(const Path, Mask: string; const FindDirectories: boolean;
  const FileMethod: TFoundFileMethod; const Options: TFindFilesOptions): Cardinal;
var
  FileMethodWrapper: TFoundFileMethodWrapper;
begin
  FileMethodWrapper.FileMethod := FileMethod;
  Result := FindFiles(Path, Mask, FindDirectories,
    {$ifdef FPC}@{$endif} FoundFileProcToMethod,
    @FileMethodWrapper, Options);
end;

function FindFiles(const PathAndMask: string; const FindDirectories: boolean;
  const FileMethod: TFoundFileMethod; const Options: TFindFilesOptions): Cardinal;
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
    procedure Callback(const FileInfo: TFileInfo; var StopSearch: Boolean);
  end;

  procedure TSearchFileHardHelper.Callback(const FileInfo: TFileInfo; var StopSearch: Boolean);
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
    Path := UriToFilenameSafe(Path)
  else
  if (P = 'castle-nx-contents') or
     (P = 'castle-nx-save') then
    Exit(URIFileExists(Path + Base)) // URIFileExists handles castle-nx-contents and castle-nx-save
  else
  if P <> '' then
    { we can't do anything with different protocols
      (note that empty protocol means it's a filename, so this is OK). }
    Exit(true);

  if RegularFileExists(Path + Base) then Exit(true);

  Helper := TSearchFileHardHelper.Create;
  try
    Helper.Base := Base;
    FindFiles(Path + '*', false, {$ifdef FPC}@{$endif}Helper.Callback, []);
    Result := Helper.IsFound;
    if Result then
      NewBase := Helper.Found;
  finally FreeAndNil(Helper) end;
end;

type
  TFindFirstFileHelper = class
    IsFound: boolean;
    FoundFile: TFileInfo;
    procedure Callback(const FileInfo: TFileInfo; var StopSearch: Boolean);
  end;

  procedure TFindFirstFileHelper.Callback(const FileInfo: TFileInfo; var StopSearch: Boolean);
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
    FindFiles(Path, Mask, FindDirectories,
      {$ifdef FPC}@{$endif} Helper.Callback, Options);
    Result := Helper.IsFound;
    if Result then
      FileInfo := Helper.FoundFile;
  finally FreeAndNil(Helper) end;
end;

type
  TFindFirstFileIgnoreCaseHelper = class
    Mask: String;
    IsFound: boolean;
    FoundFile: TFileInfo;
    procedure Callback(const FileInfo: TFileInfo; var StopSearch: Boolean);
  end;

  procedure TFindFirstFileIgnoreCaseHelper.Callback(const FileInfo: TFileInfo; var StopSearch: Boolean);
  begin
    if IsWild(FileInfo.Name, Mask, { IgnoreCase } true) then
    begin
      FoundFile := FileInfo;
      IsFound := true;
      StopSearch := true;
    end;
  end;

function FindFirstFileIgnoreCase(const Path, Mask: string;
  const FindDirectories: boolean; const Options: TFindFilesOptions;
  out FileInfo: TFileInfo): boolean;
var
  Helper: TFindFirstFileIgnoreCaseHelper;
begin
  Helper := TFindFirstFileIgnoreCaseHelper.Create;
  try
    Helper.Mask := Mask;
    { We enumerate all files (so it doesn't matter if FindFirst/Next,
      which may be using native OS functions when searching local file system,
      are case-sensitive or not).
      Then we check Mask in TFindFirstFileIgnoreCaseHelper.Callback . }
    FindFiles(Path, '*', FindDirectories,
      {$ifdef FPC}@{$endif} Helper.Callback, Options);
    Result := Helper.IsFound;
    if Result then
      FileInfo := Helper.FoundFile;
  finally FreeAndNil(Helper) end;
end;

type
  TFindFilesListHelper = class
    List: TFileInfoList;
    procedure Callback(const FileInfo: TFileInfo; var StopSearch: Boolean);
  end;

  procedure TFindFilesListHelper.Callback(const FileInfo: TFileInfo; var StopSearch: Boolean);
  begin
    List.Add(FileInfo);
  end;

function FindFilesList(const Path, Mask: string; const FindDirectories: boolean;
  const Options: TFindFilesOptions): TFileInfoList;
var
  Helper: TFindFilesListHelper;
begin
  Result := TFileInfoList.Create;
  try
    Helper := TFindFilesListHelper.Create;
    try
      Helper.List := Result;
      FindFiles(Path, Mask, FindDirectories,
        {$ifdef FPC}@{$endif} Helper.Callback, Options);
    finally FreeAndNil(Helper) end;
  except FreeAndNil(Result); raise end;
end;

end.
