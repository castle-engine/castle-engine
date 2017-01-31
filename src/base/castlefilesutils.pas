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

{ Operations on files.

  Includes functions to help cross-platform programs to know
  where to read/write files:
  @unorderedList(
    @itemSpacing Compact
    @item(ApplicationConfig -- user config files)
    @item(ApplicationData -- installed program's data files)
  )

  List of things to do / not to do if you want to write
  truly cross-platform program (that also handles URLs everywhere):

  @unorderedList(
    @item(Never use things like ParamStr(0), or Lazarus Application.ExeName.
      If you really want the filename of your executable, we have
      CastleFilesUtils.ExeName, but this also should not be depended on
      (it may raise exception on some OSes).

      If you want a nice application name, use SysUtils.ApplicationName
      (our units use it too).

      If you want to load program data or save program config,
      it's best to use ApplicationConfig and ApplicationData for all paths.)

    @item(Do not use standard FindFirst/FindNext.

      If you need to search a directory for some files,
      use CastleFindFiles unit. But it only works for
      local filesystems (or Android assets filesystem),
      of course you cannot search for files within http URLs.
      So in general avoid such file searching.)

    @item(Read / write all data using streams (TStream) descendants.
      Open and save these streams using our CastleDownload unit.)
  )
}
unit CastleFilesUtils;

{$I castleconf.inc}

interface

uses {$ifdef MSWINDOWS} Windows, {$endif}
  {$ifdef UNIX} BaseUnix, Unix, {$endif}
  SysUtils, CastleUtils;

type
  EExeNameNotAvailable = class(Exception);
  ERemoveFailed = class(Exception);

{ Full (absolute) filename to executable file of this program.
  If it's impossible to obtain, raises exception @link(EExeNameNotAvailable).

  Under Windows this is simply ParamStr(0) (and it never raises
  exception), but under other OSes it's not so simple to obtain
  (although it's important to note that usually programs under
  UNIX should not need this, actually).

  Internal implementation notes:

  Under UNIXes other than Linux I don't know how to obtain this,
  so e.g. under FreeBSD this will always raise an exception.
  Under Linux I'm trying to read file /proc/getpid()/exe,
  this should work under most Linuxes as long as user
  compiled Linux kernel with /proc support. So under Linux
  this may work, but still you should be always prepared that it
  may raise @link(EExeNameNotAvailable). }
function ExeName: string;

{ The name of our program.

  @deprecated Deprecated, this is equivalent to ApplicationName,
  and you should just call ApplicationName directly in new code.
  ApplicationName is included in standard FPC SysUtils unit, had good default
  and is easily configurable by callback OnGetApplicationName.
  See http://www.freepascal.org/docs-html/rtl/sysutils/getappconfigdir.html .

  This is suitable to show to user. It should also indicate how to run the program,
  usually it should be the basename of the executable (although we do not depend
  on it technically). It is used to derive config and data paths for our program,
  see ApplicationConfig and ApplicationData. }
function ProgramName: string; deprecated;

{ Returns true if file exists and is a normal file.
  Detects and returns @false for special Windows files
  like 'con', 'c:\con', 'c:\somedir\con' etc.
  ('con' is a special device name).
  For all other files (and other OSes) this function returns the same
  as FileExists.

  @deprecated Deprecated, since we use URLs everywhere,
  use URIFileExists to check does file exist. }
function NormalFileExists(const fileName: string): boolean; deprecated;

{ Path to store user configuration files.
  This is some directory that should be writeable
  and that is a standard directory under this OS to put user config files.
  Always returns absolute (not relative) path. Result contains trailing
  PathDelim.

  @deprecated Deprecated, use ApplicationConfig instead. }
function UserConfigPath: string; deprecated;

{ Filename to store user configuration.
  Always returns absolute (not relative) path.

  Returns filename that:
  @unorderedList(
    @itemSpacing Compact
    @item is inside UserConfigPath
    @item depends on OnGetApplicationName
    @item(has given Extension. Extension should contain
      beginning dot. E.g. FExtension = '.ini'. This way you can pass
      FExtension = '' to have a filename without extension.)
  )

  @deprecated Deprecated,
  use ApplicationConfig(ApplicationName + Extension) instead. }
function UserConfigFile(const Extension: string): string; deprecated;

{ Path to access installed data files.
  Returns absolute path, containing trailing PathDelim.

  @deprecated Deprecated, use ApplicationData instead. }
function ProgramDataPath: string; deprecated;

var
  ApplicationConfigOverride: string;

{ URL where we should read and write configuration files.
  This always returns a @code(file://...) URL,
  which is comfortable since our engine operates on URLs most of the time.

  Given Path specifies a name of the file (with possible subdirectories)
  under the user config directory. The Path is a relative URL, so you should
  always use slashes (regardless of OS), and you can escape characters by %xx.
  We make sure that the directory (including
  the subdirectories you specify in Path) exists, creating it if necessary.
  But we do not create the file. We should have permissions
  to write inside the given directory (although, as always on multi-process OS,
  the only 100% way to know if you can write there is to actually try it).

  This uses FPC GetAppConfigDir under the hood.
  Which in turn looks at OnGetApplicationName, and may use
  OS-specific algorithm to find good config directory, see
  http://www.freepascal.org/docs-html/rtl/sysutils/ongetapplicationname.html .
  On UNIX this follows XDG Base Directory Specification,
  see http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html
  (simplifying: looks inside ~/.config/<application-name>/). }
function ApplicationConfig(const Path: string): string;

{ URL from which we should read data files.
  This returns URL, which is comfortable since our engine operates
  on URLs everywhere. On normal desktops systems this will return
  a @code(file://...) URL. On Android, it will return an URL indicating
  assets (files packages together inside Android apk) starting with
  @code(assets:/...).

  Given Path specifies a path under the data directory,
  with possible subdirectories, with possible filename at the end.
  The Path is a relative URL, so you should
  always use slashes "/" (regardless of OS), and you can escape characters by %xx.
  You can use Path = '' to get the URL to whole data directory.
  Note that files there may be read-only, do not try to write there.

  The algorithm to find base data directory (with respect to which
  Path is resolved) is OS-specific.
  It looks at ApplicationName, and searches a couple of common locations,
  using the first location that exists. We try to look first inside
  user-specific directories, then inside system-wide directories,
  and as a fallback we use current exe directory (under Windows)
  or current working directory (under other OSes).

  The exact details how we currently look for data directory
  (specified here so that you know how to install your program):

  @definitionList(
    @itemLabel(Windows)
    @item(@orderedList(
      @item(@code(data) subdirectory inside our exe directory, if exists.)
      @item(Last resort fallback: just our exe directory.)
    ))

    @itemLabel(Mac OS X)
    @item(@orderedList(
      @item(@code(Contents/Resources/data) subdirectory inside our bundle directory,
        if we are inside a bundle and such subdirectory exists.)
      @item(Otherwise, algorithm on Mac OS X follows algorithm on other Unixes,
        see below.)
    ))

    @itemLabel(Android)
    @item(@orderedList(
      @item(We always return @code(assets:/) directory, to read assets
        from the apk.)
    ))

    @itemLabel(Unix (Linux, Mac OS X, FreeBSD etc.))
    @item(@orderedList(
      @item(@code(~/.local/share/) + ApplicationName.
        This is nice user-specific data directory, following the default dictated by
        http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html .
        If such directory exists, it is returned.

        This is checked first, to allow user to always override system-wide
        installation of a program with his own installation.
        E.g. consider the situation when an old version of a program
        is installed system-wide in /usr/local/share/my_program/,
        but some user (with no access to root account) wants to
        install a newer version of it for himself. Now he can do it,
        because ~/.local/share/my_program/ is checked 1st, before system-wide paths.)

      @item(@code(HomePath +'.' +ApplicationName+'.data/').
        If such directory exists, it is returned.

        This is another location of user-specific data directory, deprecated now.
        You should instead use more standard
        @code(~/.local/share/) + ApplicationName.)

      @item(@code('/usr/local/share/' +ApplicationName+ '/').
        If such directory exists, it is returned.

        This is suitable for system-wide installations without package manager.)

      @item(@code('/usr/share/' +ApplicationName+ '/').
        If such directory exists, it is returned.

        This is suitable for system-wide installations with package manager.)

       @item(@code(data) subdirectory of current directory, if exists.
         Using @code(data) subdirectory is usually comfortable,
         it allows you to separate code from data better.)

      @item(As a last resort, we just return the current directory.
        So you can just place data files inside the current directory,
        and if user will run your game from it's own directory --- it will
        work without any fuss.)
    )
  )
) }
function ApplicationData(const Path: string): string;

var
  ApplicationDataOverride: string;

{$ifdef UNIX}
{ User's home directory, with trailing PathDelim.

  Taken from environment variable $HOME, unless it's empty or unset,
  in which case we take this from Unix user database by real uid.
  This is what bash does (more-or-less, when home directory does
  not exist strange things happen), that's what programs should
  do according to `info libc' and Kambi preferences. }
function HomePath: string;
{$endif}

{ Expand tilde (~) in path, just like shell. Expands ~ to
  ExclPathDelim(HomePath) under UNIX. Under Windows, does nothing. }
function ExpandHomePath(const FileName: string): string;

{ Call SysUtils.DeleteFile and check result.

  When Warn = @false (default) raises an exception on failure,
  otherwise (when Warn = @true) makes only WritelnWarning on failure.
  @raises ERemoveFailed If delete failed, and Warn = @false. }
procedure CheckDeleteFile(const FileName: string; const Warn: boolean = false);

{ Call RemoveDir and check result.

  When Warn = @false (default) raises an exception on failure,
  otherwise (when Warn = @true) makes only WritelnWarning on failure.
  @raises ERemoveFailed If delete failed, and Warn = @false. }
procedure CheckRemoveDir(const DirFileName: string; const Warn: boolean = false);

{ Make sure directory exists, eventually creating it, recursively, checking result. }
procedure CheckForceDirectories(const Dir: string);

procedure CheckCopyFile(const Source, Dest: string);

procedure CheckRenameFile(const Source, Dest: string);

{ Remove the directory DirName, @italic(recursively, unconditionally,
  with all the files and subdirectories inside).
  DirName may but doesn't have to end with PathDelim.

  When Warn = @false (default) raises an exception on failure,
  otherwise (when Warn = @true) makes only WritelnWarning on failure.
  @raises ERemoveFailed If delete failed, and Warn = @false. }
procedure RemoveNonEmptyDir(const DirName: string; const Warn: boolean = false);

{ Substitute %d in given filename pattern with successive numbers,
  until the filename doesn't exist.

  The idea is to start with number = 0 and do
  @code(Format(FileNamePattern, [number])), until you find non-existing
  filename. Example filename pattern is @code(screenshot_%d.png),
  by saving to this filename you're relatively sure that each save goes
  to a new file. Since we use standard @code(Format) function,
  you can use e.g. @code(screenshot_%04d.png) to have a number inside
  the filename always at least 4 digits long.

  Note that it's possible on every OS that some other program,
  or a second copy of your own program, will write to the filename
  between FileNameAutoInc determined it doesn't exist and you opened the file.
  So using this cannot guarantee that you really always write to a new file
  (use proper file open modes for this). }
function FileNameAutoInc(const FileNamePattern: string): string;

{ Deprecated name for FileNameAutoInc. @deprecated }
function FnameAutoInc(const FileNamePattern: string): string; deprecated;

{ Parent directory name.

  Given DirName may be absolute or relative.
  Given DirName may but doesn't have to include trailing PathDelim.
  Result is always absolute filename, and contains trailing PathDelim.

  Returns the same DirName if there's no parent directory.

  When DoExpandDirName = false then it is assumed that DirName already
  is absolute path. Then this function is pure string-operation
  (no actual reading of any filesystem info), so it works faster and
  DirName does not need to exist. }
function ParentPath(DirName: string;
  DoExpandDirName: boolean = true): string;

{ Combines BasePath with RelPath into complete path.
  BasePath MUST be an absolute path,
  on Windows it must contain at least drive specifier (like 'c:'),
  on Unix it must begin with "/". RelPath can be relative and can
  be absolute. If RelPath is absolute, result is RelPath.
  Else the result is an absolute path calculated by combining RelPath
  with BasePath.

  Usually you should instead operate on URLs
  and combine them using @link(CastleURIUtils.CombineURI). }
function CombinePaths(BasePath, RelPath: string): string;

{ Search a file on $PATH. Works with double quotes around components
  of path list, avoiding this bug: http://bugs.freepascal.org/view.php?id=19279.
  See http://www.freepascal.org/docs-html/rtl/sysutils/filesearch.html
  for original FileSearch docs.

  In FPC >= 2.5.1, you should instead use just ExeSearch(Name).
  It also will use $PATH and avoid double quotes problems on Windows.
  See http://bugs.freepascal.org/view.php?id=19282 and
  fix on http://svn.freepascal.org/cgi-bin/viewvc.cgi?view=rev&revision=17717 . }
Function PathFileSearch(Const Name : String; ImplicitCurrentDir : Boolean = True) : String;

{ Find program on $PATH. Automatically adds ExeExtension, so don't add it yourself.
  On Windows, may also add alternative executable extensions (.com, .bat, .cmd).
  Searches in $PATH (and, if OS does this, in current directory --- this is standard
  on Windows but not on Unix).
  Returns '' (if not found) or absolute filename. }
function FindExe(const ExeName: string): string;

{ Add an exe file extension, searching for an existing file starting with ExePath.
  On non-Windows, this is just equal to ExePath + ExeExtension,
  which in practice is just equal to ExePath (since ExeExtension is empty on Unix).
  But on Windows, this tries to append other extensions (.com, .bat, .cmd,
  just like @link(FindExe)), depending on what file exists. }
function AddExeExtension(const ExePath: string): string;

{ Get temporary filename, suitable for ApplicationName, checking that
  it doesn't exist. }
function GetTempFileNameCheck: string;

{ Return a prefix (beginning of an absolute filename)
  to save a series of temporary files. }
function GetTempFileNamePrefix: string;

{$ifdef DARWIN}
{ Main directory of the current Mac OS X bundle, including final slash.
  Empty string if we're not run from a bundle. }
function BundlePath: string;
{$endif}

implementation

uses {$ifdef DARWIN} MacOSAll, {$endif} Classes, CastleStringUtils,
  {$ifdef MSWINDOWS} CastleDynLib, {$endif} CastleLog,
  CastleURIUtils, CastleFindFiles;

var
  { inicjowane w initialization i pozniej stale.
    Nie-Windowsy nie daja zadnej gwarancji ze to sie uda zainicjowac -
    wtedy function ExeName rzuca wyjatek. ZADNA funkcja poza
    initialization i ExeName nie powinna wprost odwolywac sie do tej zmiennej ! }
  FExeName: string;

function ExeName: string;
begin
 { Under Windows ParamStr(0) is always OK, so there is no need to check
   is FExeName = ''. }
 {$ifndef MSWINDOWS}
 if FExeName = '' then
  raise EExeNameNotAvailable.Create(
    'ExeName: Cannot obtain filename of executable of this program');
 {$endif}

 Result := FExeName;
end;

function ProgramName: string;
begin
  Result := ApplicationName;
end;

function NormalFileExists(const FileName: string): boolean;
{$ifdef MSWINDOWS}
var s: string;
begin
 { Don't warn about deprecation of ExtractOnlyFileName,
   since NormalFileExists is deprecated too... }
 {$warnings off}
 s := UpperCase(ExtractOnlyFileName(fileName));
 {$warnings on}
 result :=  FileExists(fileName) and
    (not( (s='CON') or (s='PRN') or (s='NUL') or
          (s='LPT1') or (s='LPT2') or (s='LPT3') or (s='LPT4') or
          (s='COM1') or (s='COM2') or (s='COM3') or (s='COM4') ) );
{$else}
begin
 result := FileExists(filename);
{$endif}
end;

function UserConfigPath: string;
begin
  Result := ApplicationConfig('');
end;

function UserConfigFile(const Extension: string): string;
begin
  Result := ApplicationConfig(ApplicationName + Extension);
end;

function ApplicationConfig(const Path: string): string;
var
  ConfigDir, Dir: string;
begin
  if ApplicationConfigOverride <> '' then
    Exit(ApplicationConfigOverride + Path);

  ConfigDir := InclPathDelim(GetAppConfigDir(false));
  Dir := ConfigDir + ExtractFilePath(Path);
  if not ForceDirectories(Dir) then
    raise Exception.CreateFmt('Cannot create directory for config file: "%s"',
      [Dir]);

  Result := FilenameToURISafe(ConfigDir + Path);
end;

function ProgramDataPath: string;
begin
  Result := ApplicationData('');
end;

var
  ApplicationDataIsCache: boolean = false;
  ApplicationDataCache: string;

function ApplicationData(const Path: string): string;

  {$ifndef ANDROID}
  function GetApplicationDataPath: string;
  {$ifdef MSWINDOWS}
  var
    ExePath: string;
  begin
    ExePath := ExtractFilePath(ExeName);

    Result := ExePath + 'data' + PathDelim;
    if DirectoryExists(Result) then Exit;

    Result := ExePath;
  {$endif MSWINDOWS}
  {$ifdef UNIX}
  var
    CurPath: string;
  begin
    {$ifdef DARWIN}
    if BundlePath <> '' then
    begin
      {$ifdef IOS}
      Result := BundlePath + 'data/';
      {$else}
      Result := BundlePath + 'Contents/Resources/data/';
      {$endif}
      if DirectoryExists(Result) then Exit;
    end;
    {$endif DARWIN}

    Result := HomePath + '.local/share/' + ApplicationName + '/';
    if DirectoryExists(Result) then Exit;

    Result := HomePath + '.' + ApplicationName + '.data/';
    if DirectoryExists(Result) then Exit;

    Result := '/usr/local/share/' + ApplicationName + '/';
    if DirectoryExists(Result) then Exit;

    Result := '/usr/share/' + ApplicationName + '/';
    if DirectoryExists(Result) then Exit;

    CurPath := InclPathDelim(GetCurrentDir);

    Result := CurPath + 'data/';
    if DirectoryExists(Result) then Exit;

    Result := CurPath;
  {$endif UNIX}
  end;
  {$endif not ANDROID}

begin
  if ApplicationDataOverride <> '' then
    Exit(ApplicationDataOverride + Path);

  if Pos('\', Path) <> 0 then
    WritelnWarning('ApplicationData', 'Do not use backslashes (or a PathDelim constant) in the ApplicationData parameter. The ApplicationData parameter should be a relative URL, with components separated by slash ("/"), regardless of the OS. Path given was: ' + Path);

  { Cache directory of ApplicationData. This has two reasons:
    1. On Unix GetApplicationDataPath makes three DirectoryExists calls,
       so it's not too fast, avoid calling it often.
    2. It would be strange if ApplicationData results
       suddenly changed in the middle of the program (e.g. because user just
       made appropriate symlink or such).
       The only case where we allow it is by ApplicationDataOverride. }

  if not ApplicationDataIsCache then
  begin
    ApplicationDataCache :=
      {$ifdef ANDROID}
        'assets:/'
      {$else}
        FilenameToURISafe(GetApplicationDataPath)
      {$endif}
    ;
    if Log then
      WritelnLog('Path', Format('Program data path detected as "%s"', [ApplicationDataCache]));
    ApplicationDataIsCache := true;
  end;

  Result := ApplicationDataCache + Path;
end;

{ other file utilities ---------------------------------------------------- }

{$ifdef UNIX}
function HomePath:  string;
begin
 { home dir jest dla mnie zmienna $HOME a nie tym co moglbym uzyskac z libc
   pytajac o uzytkownika real uid i jego home dir zapisany w /etc/passwd.

   Jest to zgodne z tym co mi radza w info libc, ze zdrowym rozsadkiem
   (bo, jak napisali w info libc, konfigurowac $HOME jest userowi duzo
   latwiej) i zgodne z tym co robi np. bash. Co wiecej, sprawdzilem i
   bash rozwija $HOME nawet gdy jest zle (np. rowne 'gowno' lub '').

   Gdy HOME jest niezdefiniowane lub nieprawidlowe to dopiero bash zwraca
   homedir z user database, chociaz np. w prompt nie wyswietla ~. }

 result := GetEnvironmentVariable('HOME');

 { TODO: with Libc we could take home dir from user-database looking for real-uid

 if (result='') or (not DirectoryExists(result)) then
 begin
  alloc := 0;
  Buffer := nil;
  repeat
   alloc := alloc+200;
   Buffer := Libc.Realloc(Buffer,alloc);
  until getpwuid_r(getuid, ResultBuf, Buffer, alloc, dummy)=0;
  SetString(result, ResultBuf.pw_dir, strlen(ResultBuf.pw_dir));
  Libc.free(Buffer);
 end;

 }

 Result := InclPathDelim(Result);
end;
{$endif}

function ExpandHomePath(const FileName: string): string;
{$ifdef UNIX}
begin
 { Rozwin '~' w nazwe home dir. Rozwin '~/xxx' w homedir+'/xxx'. }
 if Length(FileName) = 1 then
 begin
  if FileName[1] = '~' then
   Result := ExclPathDelim(HomePath) else
   Result := FileName;
 end else
 if (Length(FileName) > 0) and (FileName[1] = '~') then
  Result := HomePath + SEnding(FileName, 3) else
  Result := FileName;
{$else}
begin
 result := FileName;
{$endif}
end;

{ file handling ---------------------------------------------------------- }

procedure CheckDeleteFile(const FileName: string; const Warn: boolean);
begin
  if not SysUtils.DeleteFile(FileName) then
  begin
    if Warn then
      WritelnWarning('File', Format('Cannot delete file "%s"', [FileName])) else
      raise ERemoveFailed.Create(Format('Cannot delete file "%s"', [FileName]));
  end;
end;

procedure CheckRemoveDir(const DirFileName:  string; const Warn: boolean = false);
begin
  if not RemoveDir(DirFileName) then
  begin
    if Warn then
      WritelnWarning('File', Format('Cannot remove directory "%s"', [DirFileName])) else
      raise ERemoveFailed.Create(Format('Cannot remove directory "%s"', [DirFileName]));
  end;
end;

procedure CheckForceDirectories(const Dir: string);
begin
  if not ForceDirectories(Dir) then
    raise Exception.CreateFmt('Cannot create directory "%s"', [Dir]);
end;

procedure CheckCopyFile(const Source, Dest: string);
var
  SourceFile, DestFile: TFileStream;
begin
  SourceFile := TFileStream.Create(Source, fmOpenRead);
  try
    DestFile := TFileStream.Create(Dest, fmCreate);
    try
      DestFile.CopyFrom(SourceFile, SourceFile.Size);
    finally FreeAndNil(DestFile) end;
  finally FreeAndNil(SourceFile) end;
end;

procedure CheckRenameFile(const Source, Dest: string);
begin
  {$ifdef MSWINDOWS}
  { On Windows, we have to remove Dest explicitly, otherwise RenameFile will fail
    when Dest exists }
  SysUtils.DeleteFile(Dest);
  {$endif}
  if not RenameFile(Source, Dest) then
  begin
    {$ifdef UNIX}
    WritelnLog('File', Format('Cannot rename/move from "%s" to "%s", assuming it''s because they are on different disks, trying slower copy+delete approach', [Source, Dest]));
    CheckCopyFile(Source, Dest);
    CheckDeleteFile(Source, false);
    {$else}
    raise Exception.CreateFmt('Cannot rename/move from "%s" to "%s"', [Source, Dest]);
    {$endif}
  end;
end;

procedure RemoveNonEmptyDir_Internal(const FileInfo: TFileInfo; Data: Pointer; var StopSearch: boolean);
var
  Warn: boolean;
begin
  if SpecialDirName(FileInfo.Name) then Exit;

  Warn := PBoolean(Data)^;

  if FileInfo.Directory then
    CheckRemoveDir(FileInfo.AbsoluteName, Warn) else
    CheckDeleteFile(FileInfo.AbsoluteName, Warn);
end;

procedure RemoveNonEmptyDir(const DirName: string; const Warn: boolean = false);
begin
  FindFiles(DirName, '*', true,
    @RemoveNonEmptyDir_Internal, @Warn, [ffRecursive, ffDirContentsLast]);
  CheckRemoveDir(Dirname, Warn);
end;

{ dir handling -------------------------------------------------------- }

function FileNameAutoInc(const FileNamePattern: string): string;
var i: integer;
begin
 i := 0;
 repeat
  result := Format(FileNamePattern,[i]);
  if not FileExists(result) then exit;
  Inc(i);
 until false;
end;

function FnameAutoInc(const FileNamePattern: string): string;
begin
  Result := FileNameAutoInc(FileNamePattern);
end;

{ Note: the only things here that makes this function belong to
  CastleFilesUtils instead of casleutils_filenames.inc is
  using ExpandFileName. }

function ParentPath(DirName: string; DoExpandDirName: boolean): string;
var p: integer;
begin
{$ifdef MSWINDOWS}
 { if it's only drive name - return(dirname) }
 if (DirName[2]=DriveDelim) and
    ( (Length(DirName)=2) or
      ((Length(DirName)=3) and (DirName[3]=PathDelim)) ) then
 begin
  Result := InclPathDelim(DirName);
  Exit;
 end;
{$endif}
 if DoExpandDirName then
  DirName := ExpandFileName(DirName);
 DirName := ExclPathDelim(DirName);
 p := LastDelimiter(PathDelim, DirName);
 if p>0 then Result := Copy(DirName,1,p) else Result := RootDir;
end;

function CombinePaths(BasePath, RelPath: string): string;
begin
  if IsPathAbsolute(RelPath) then
    result := RelPath else
  {$ifdef MSWINDOWS}
  if IsPathAbsoluteOnDrive(RelPath) then
    result := BasePath[1] +DriveDelim +RelPath else
  {$endif}
  begin
    repeat
      if (Copy(RelPath, 1, 2) = './')
        {$ifdef MSWINDOWS} or (Copy(RelPath, 1, 2) = '.\') {$endif} then
        RelPath := SEnding(RelPath, 3) else
      if (Copy(RelPath, 1, 3) = '../')
        {$ifdef MSWINDOWS} or (Copy(RelPath, 1, 3) = '..\') {$endif} then
      begin
        BasePath := ExtractFileDir(ExclPathDelim(BasePath));
        RelPath := SEnding(RelPath, 4);
      end else
        Break;
    until false;

    result := InclPathDelim(BasePath) + RelPath;
  end;
end;

Function PathFileSearch(Const Name : String; ImplicitCurrentDir : Boolean = True) : String;

{ This is identical to FileSearch, except on Windows each $PATH component
  is stripped from surrounding double quotes.
  Added also "not DirectoryExists(Result)" check, to avoid accidentaly finding
  a directory named like file (esp. easy on Unix without '.exe' extension),
  at least with FPC 2.6.4 and 2.7.1 FileExists is true for directories. }

Var
  I : longint;
  Temp : String;

begin
  Result:=Name;
  temp:=SetDirSeparators(GetEnvironmentVariable('PATH'));
  // Start with checking the file in the current directory
  If ImplicitCurrentDir and (Result <> '') and FileExists(Result) and not DirectoryExists(Result) Then
    exit;
  while True do begin
    If Temp = '' then
      Break; // No more directories to search - fail
    I:=pos(PathSeparator,Temp);
    If I<>0 then
      begin
        Result:=Copy (Temp,1,i-1);
        system.Delete(Temp,1,I);
      end
    else
      begin
        Result:=Temp;
        Temp:='';
      end;
    If Result<>'' then
    begin
      { On Windows, each path on the list may be surrounded by quotes. }
      {$ifdef MSWINDOWS}
      if (Length(Result) >= 2) and
         (Result[1] = '"') and
         (Result[Length(Result)] = '"') then
        Result := Copy(Result, 2, Length(Result) - 2);
      {$endif}
      Result:=IncludeTrailingPathDelimiter(Result)+name;
    end;
    If (Result <> '') and FileExists(Result) and not DirectoryExists(Result) Then
      exit;
  end;
  result:='';
end;

function FindExe(const ExeName: string): string;
begin
  {$ifdef MSWINDOWS}
  { The default order of extensions is .com, .exe, .bat, .cmd,
    see http://stackoverflow.com/questions/605101/order-in-which-command-prompt-executes-files-with-the-same-name-a-bat-vs-a-cmd }
  Result := PathFileSearch(ExeName + '.com', true);
  if Result = '' then
    Result := PathFileSearch(ExeName + '.exe' { ExeExtension }, true);
  if Result = '' then
    Result := PathFileSearch(ExeName + '.bat', true);
  if Result = '' then
    Result := PathFileSearch(ExeName + '.cmd', true);
  {$else}
  Result := PathFileSearch(ExeName + ExeExtension, false);
  {$endif}
end;

function AddExeExtension(const ExePath: string): string;
begin
  {$ifdef MSWINDOWS}
  { The default order of extensions is .com, .exe, .bat, .cmd,
    see http://stackoverflow.com/questions/605101/order-in-which-command-prompt-executes-files-with-the-same-name-a-bat-vs-a-cmd }
  if FileExists(ExePath + '.com') then
    Result := ExePath + '.com' else
  if FileExists(ExePath + '.exe' { ExeExtension }) then
    Result := ExePath + '.exe' { ExeExtension } else
  if FileExists(ExePath + '.bat') then
    Result := ExePath + '.bat' else
  if FileExists(ExePath + '.cmd') then
    Result := ExePath + '.cmd' else
  {$else}
    Result := ExePath + ExeExtension;
  {$endif}
end;

procedure DoInitialization;
begin
 { inicjalizacja FExeName }

 { First, assume that there is no way to obtain FExeName
   on this platform }
 FExeName := '';

 {$ifdef LINUX}

 { Pod UNIXem wlasciwie ExeName nie powinno nam byc do niczego
   potrzebne - pod Windowsem uzywam ExeName np. aby uzyskac sciezke
   do aplikacji i tam zalozyc plik ini, ale pod UNIXem
   powinienem uzywac do tego celu katalogu $HOME albo czytac ustawienia
   gdzies z /etc.

   Ale zrobilem to. Nie jest to 100% pewna metoda ale nie jest tez taka
   zupelnie nieelegancka : korzystamy z proc/getpid()/exe.

   Notka : NIE mozemy w zaden sposob uzywac ParamStr(0) do obliczania fExeName.
   Nasze ParamStr(0) jest ustalane przez proces ktory nas wywolal - moze to
   byc nazwa naszego pliku wykonywalnmego lub symboic linka do niego, ze sciezka
   wzgledna lub bezwzgledna lub bez sciezki gdy nasz executable byl wsrod $PATH
   ale to wszystko to tylko GDYBANIE - taka jest konwencja ale tak naprawde
   nasze ParamStr(0) moze byc absolutnie czymkolwiek. Nie mozemy wiec w zaden
   sposob polegac na tym ze jego wartosc okresla cokolwiek w jakikolwiek sposob. }

 try
  FExeName := CastleReadLink('/proc/' + IntToStr(FpGetpid) + '/exe')
 except
  on EOSError do FExeName := '';
 end;
 {$endif}

 {$ifdef MSWINDOWS} FExeName := ParamStr(0) {$endif};
end;

function GetTempFileNameCheck: string;
begin
  Result := GetTempFileName('', ApplicationName);
  { Be paranoid and check whether file does not exist. }
  if FileExists(Result) then
    raise Exception.CreateFmt('Temporary file "%s" already exists', [Result]);
end;

function GetTempFileNamePrefix: string;
var
  FileInfo: TFileInfo;
begin
  Result := GetTempFileName('', ApplicationName) + '_' +
    { Although GetTempFileName should add some randomization here,
      there's no guarantee. And we really need randomization ---
      we may load ffmpeg output using image %d pattern, so we don't want to
      accidentaly pick up other images in the temporary directory
      (e.g. leftovers from previous TRangeScreenShot.BeginCapture). }
    { System.Random, not just Random, to avoid using Random from MacOSAll unit. }
    IntToStr(System.Random(MaxInt)) + '_';

  { Check is it really Ok. }
  if FindFirstFile(Result, '*', true, [], FileInfo) then
    raise Exception.CreateFmt('Failed to generate unique temporary file prefix "%s": filename "%s" already exists',
      [Result, FileInfo.AbsoluteName]);
end;

{$ifdef DARWIN}
var
  BundlePathCached: boolean;
  BundlePathCache: string;

function BundlePath: string;
{ Based on
  http://wiki.freepascal.org/OS_X_Programming_Tips#How_to_obtain_the_path_to_the_Bundle }
var
  bundle: CFBundleRef;
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
begin
  if not BundlePathCached then
  begin
    bundle := CFBundleGetMainBundle();
    if bundle = nil then
      BundlePathCache := '' else
    begin
      pathRef := CFBundleCopyBundleURL(bundle);
      pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
      CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
      CFRelease(pathRef);
      CFRelease(pathCFStr);
      BundlePathCache := pathStr;
      BundlePathCache := InclPathDelim(BundlePathCache);
    end;
    BundlePathCached := true;
  end;
  Result := BundlePathCache;
end;
{$endif DARWIN}

initialization
  DoInitialization;
end.
