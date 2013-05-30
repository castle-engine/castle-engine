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

{ Enumerating filenames matching some mask.
  Intention is to wrap standard FindFirst/FindNext procedures
  in a much safer and cleaner interface. }
unit CastleEnumerateFiles;

{
  TODO:
  - better interface, probably TEnumerateFiles as class
  - don't select files using faXxx constants, as these do not have
    perfect definition on non-Dos/Windows systems.
    Maybe use readdir and opendir on UNIX systems
    (instead of relaying on FindFirst/Next ported to UNIX).
}

{$I castleconf.inc}

interface

uses SysUtils, CastleUtils, Classes, CastleGenericLists;

type
  { }
  TEnumeratedFileInfo = record
    { TSearchRec, as returned by FindFirst / FindNext. }
    SearchRec: TSearchRec;
    { Expanded (with absolute path) file name. }
    FullFileName: string;
  end;
  PEnumeratedFileInfo = ^TEnumeratedFileInfo;

  TEnumeratedFileInfoList = specialize TGenericStructList<TEnumeratedFileInfo>;

type
  TEnumFileProc =
    procedure (const FileInfo: TEnumeratedFileInfo; Data: Pointer);
  TEnumFileMethod =
    procedure (const FileInfo: TEnumeratedFileInfo) of object;

  TEnumFilesOption = (
    { Means that symlinks should also be enumerated. Excluding
      this from Options means that symlinks will not be reported
      to FileProc.

      Note that you do @italic(not) control whether symlinks
      are enumerated using Attr parameter,
      you control it only by including or excluding eoSymlinks
      from Options. }
    eoSymlinks,

    { If eoRecursive is in Options then EnumFiles (and friends) descend into
      subdirectories.

      Note that we always descend into @italic(every) subdirectory (not only
      into those matching Mask, that would be pretty useless (and is a problem
      with Unix shell expansion)).

      Note that including eoRecursive in Options
      is something completely different than the flag faDirectory in Attr:
      faDirectory in Attr says whether to report directories to FileProc;
      eoRecursive says whether to descend into directories and enumerate
      their files too.

      Recursive does @italic(not) descend into symlinks to directories.
      Why? Well, this would produce risk of falling into infinite loop
      (unless some time-consuming precautions would be taken).
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

    { If Mask will equal exactly '-', it will be intrepreted specially:
      we will then return exactly one file record, with SearchRec.Name
      and FullFileName equal to '-'. }
    eoAllowStdIn,

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

  @param(Attr Bit-wise or of faXxx flags specifying which files to find.
    Normal, regular files are always found.)

  @param(FileProc Called on each file found.)

  @param(Options A set of options. See TEnumFilesOption for meaning
    of each option. Often [eoSymlinks] is the right choice.)

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
function EnumFiles(const Mask: string; Attr: integer;
  FileProc: TEnumFileProc; FileProcData: Pointer;
  Options: TEnumFilesOptions): Cardinal; overload;
function EnumFilesObj(const Mask: string; Attr: integer;
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

implementation

uses CastleFilesUtils, CastleURIUtils;

{ EnumFiles ------------------------------------------------------------ }

{ Notka o Kylixie 3 :

  Kylix 3 defines faSymLink flag that we can use when we want
  to check TSearchRec.Attr field - whether the file is Sym Link ?
  We don't have to specify this attribute as an argument
  to FindFirst, (we can't DON'T look for symbolic links),
  I checked this, and it's good because my Kylix 1 code
  is still correct.

  Also, Kylix 3 sets the faDirectory flag
  if symlink points to directory. It *was* OK too because
  our EnumFiles also used faDirectory to decide whether or not
  return symlinks to directories to EnumFile.
  But it's not the case anymore (symlinks are always either
  always reported or never reported, no difference between
  symlinks to directory and symlinks to non-directory).
  So this code is incorrect under Kylix 3 in this special case !

  ----------------------------------------------------------------------------

  Note that if you request symlinks (by eoSymlinks in Options),
  you will get to FileProc records with faSysFile flag included
  (even if faSysFile wasn't in Attr).
}

{ This is equivalent to EnumFiles with Recursive = false
  and ReadAllFirst = false.

  Moreover, Mask_fname musi byc PELNA nazwa pliku,
  razem z katalogiem (i ew. dyskiem pod Windows).
  fpath MUSI byc rowne ExtractFilePath(Mask_fname).

  Polegam na FindFirst/Next ze sym-linki sa faSysFile (ale nie musisz podawac
  w Attr faSysFile zeby znalezc sym-linki - wystarczy ze Symlinks = true) }
function EnumFiles_Core(const fpath, Mask_fname: string; attr: integer;
  FileProc: TEnumFileProc; FileProcData: Pointer;
  Symlinks: boolean): Cardinal;
var Fullname: string;
    FileRec: TSearchRec;
    realattr, searchError: integer;
    FileInfo: TEnumeratedFileInfo;
begin
 Result := 0;

 if Symlinks then
  realattr := Attr or faSysFile else
  realattr := Attr;
 SearchError := FindFirst(Mask_fname, realattr, FileRec);
 try
  while SearchError = 0 do
  begin
   Fullname := fpath + FileRec.Name; { znalazles plik }

   if (faSysFile and FileRec.Attr) <> 0 then { znalazles plik z faSysFile }
   begin
    { TODO: it would be nice to be able to write here FileRec instead of
      FullName under FPC. See comments at IsSymLink in CastleUtils.
      See FPC bug (wishlist, actually) 2995. }
    if IsSymLink(FullName) then
    begin
     if not Symlinks then FullName := '';
    end else
    begin { plik faSysFile ale nie sym-link }
     if (faSysFile and Attr) = 0 then FullName := '';
    end;
   end;

   if Fullname <> '' then { przetwarzaj plik do FileProc }
   begin
    Inc(result);
    FileInfo.FullFileName := FullName;
    FileInfo.SearchRec := FileRec;
    FileProc(FileInfo, FileProcData);
   end;

   SearchError := FindNext(FileRec);
  end;
 finally FindClose(FileRec) end;
end;

{ This is equivalent to EnumFiles with Recursive = false
  and ReadAllFirst = false. }
function EnumFiles_NonRecursive(const Mask: string; attr: integer;
  FileProc: TEnumFileProc; FileProcData: Pointer;
  Symlinks: boolean): Cardinal; overload;
var Mask_full: string;
begin
 Mask_full := ExpandFileName(Mask);
 result := EnumFiles_Core(ExtractFilePath(Mask_full), Mask_full, Attr,
   FileProc, FileProcData, Symlinks);
end;

{ This is equivalent to EnumFiles with Recursive = true,
  and ReadAllFirst = false. }
function EnumFiles_Recursive(const Mask: string; attr: integer;
  FileProc: TEnumFileProc; FileProcData: Pointer;
  Symlinks: boolean; DirContentsLast: boolean): Cardinal;

  function EnumFilesRecursWewn(const Mask: string): Cardinal;
  {przekazana tu argumentem Mask musi zawierac juz pelna sciezke}
  var katalog: string;

    procedure WriteDirContent;
    {wykonujamy normalne szukanie, zupelnie jak w EnumFiles}
    begin
     result := result + EnumFiles_Core(katalog, Mask, Attr,
       FileProc, FileProcData, Symlinks);
    end;

    procedure WriteSubdirs;
    var FileRec: TSearchRec;
        searchError: integer;
        FileRecName: string;
    {schodzimy rekurencyjnie w podkatalogi}
    begin
     searchError := FindFirst(katalog+'*', faDirectory or faSysFile or faArchive,
       FileRec);
     try
      while searchError = 0 do
      begin
       FileRecName := FileRec.Name;
       if ((faDirectory and FileRec.attr) <> 0) and
          (not SpecialDirName(FileRecName))
          {$ifdef FREEBSD}
          and
            { TODO: checking for '' is just a hack to make it work on
              FreeBSD with FPC 1.9.6 }
             (FileRec.Name <> '')
          {$endif} then
       begin {schodzimy rekurencyjnie}
        result := result + EnumFilesRecursWewn(
          katalog +FileRecName +PathDelim +ExtractFileName(Mask));
       end;
       searchError := FindNext(FileRec);
      end;
     finally FindClose(FileRec) end;
    end;

  begin
   katalog := ExtractFilePath(Mask);
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
function EnumFiles_NonReadAllFirst(const Mask: string; attr: integer;
  FileProc: TEnumFileProc; FileProcData: Pointer;
  Symlinks, Recursive, DirContentsLast: boolean): Cardinal;
begin
 if Recursive then
  Result := EnumFiles_Recursive(Mask, attr, fileProc, FileProcData, Symlinks, DirContentsLast) else
  Result := EnumFiles_NonRecursive(Mask, attr, fileProc, FileProcData, Symlinks);
end;

procedure FileProc_AddToFileInfos(
  const FileInfo: TEnumeratedFileInfo; Data: Pointer);
begin
 TEnumeratedFileInfoList(Data).Add(FileInfo);
end;

function EnumFiles(const Mask: string; attr: integer;
  FileProc: TEnumFileProc; FileProcData: Pointer;
  Options: TEnumFilesOptions): Cardinal;
const
  { Ignore the fact some fields after Name are not initialized. }
  {$warnings off}
  StdInFileInfo: TEnumeratedFileInfo = (
    SearchRec: (Time: 0; Size: 0; Attr: 0; Name: '-');
    FullFileName: '-');
  {$warnings on}
var
  FileInfos: TEnumeratedFileInfoList;
  i: Integer;
begin
 if (Mask = '-') and (eoAllowStdIn in Options) then
 begin
   FileProc(StdInFileInfo, FileProcData);
   Result := 1;
 end else
 if eoReadAllFirst in Options then
 begin
  FileInfos := TEnumeratedFileInfoList.Create;
  try
   Result := EnumFiles_NonReadAllFirst(Mask, Attr,
     {$ifdef FPC_OBJFPC} @ {$endif} FileProc_AddToFileInfos, FileInfos,
     eoSymlinks in Options,
     eoRecursive in Options,
     eoDirContentsLast in Options);
    for i := 0 to FileInfos.Count - 1 do
     FileProc(FileInfos.L[i], FileProcData);
  finally FileInfos.Free end;
 end else
 begin
   Result := EnumFiles_NonReadAllFirst(Mask, Attr,
     FileProc, FileProcData,
     eoSymlinks in Options,
     eoRecursive in Options,
     eoDirContentsLast in Options);
 end;
end;

{ EnumFilesObj ------------------------------------------------------------ }

type
  { Once I used here
      PEnumFileMethod = ^TEnumFileMethod;
    but it seems that passing @FileMethod as a pointer is an error
    that produces EAccessViolation at runtime (this behaviour is
    compatible with Delphi...). I could experiment with @@FileMethod
    but I feel that it's just safer to wrap TEnumFileMethod inside a
    record and use a pointer to a record. }
  TEnumFileMethodWrapper = record
    Contents: TEnumFileMethod;
  end;
  PEnumFileMethodWrapper = ^TEnumFileMethodWrapper;

procedure EnumFileProcToMethod(
  const FileInfo: TEnumeratedFileInfo; Data: Pointer);
begin
 PEnumFileMethodWrapper(Data)^.Contents(FileInfo);
end;

function EnumFilesObj(const Mask: string; Attr: integer;
  FileMethod: TEnumFileMethod;
  Options: TEnumFilesOptions): Cardinal;
var FileMethodWrapper: TEnumFileMethodWrapper;
begin
 FileMethodWrapper.Contents := FileMethod;
 Result := EnumFiles(Mask, Attr,
   {$ifdef FPC_OBJFPC} @ {$endif} EnumFileProcToMethod,
   @FileMethodWrapper, Options);
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
  if AnsiSameText(FileInfo.SearchRec.Name, Base) then
  begin
    Found := FileInfo.SearchRec.Name;
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
      EnumFilesObj(Path + '*', faReadOnly or faHidden or faArchive or faSymLink,
        @S.Callback, [eoSymlinks]);
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

end.
