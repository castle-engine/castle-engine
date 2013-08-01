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

{ Operations on files.

  Includes functions to help cross-platform programs to know
  where to read/write files:
  @unorderedList(
    @itemSpacing Compact
    @item(ApplicationConfig -- user config files)
    @item(ApplicationData -- installed program's data files)
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
  use ApplicationConfig(ApplicatioName + Extension) instead. }
function UserConfigFile(const Extension: string): string; deprecated;

{ Path to access installed data files.
  Returns absolute path, containing trailing PathDelim.

  @deprecated Deprecated, use ApplicationData instead. }
function ProgramDataPath: string; deprecated;

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
  This always returns a @code(file://...) URL,
  which is comfortable since our engine operates on URLs most of the time.

  Given Path specifies a path under the data directory,
  with possible subdirectories, with possible filename at the end.
  The Path is a relative URL, so you should
  always use slashes (regardless of OS), and you can escape characters by %xx.
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

    @itemLabel(Unix (Linux, Mac OS X, FreeBSD etc.))
    @item(@orderedList(
      @item(@code(~/.local/share/) + ApplicationName.
        This is nice user-specific data directory, following the default distated by
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

{ Functions IsSymLink, CanonicalizeFileName assume Windows has no symlinks.

  Why not treat Windows shell-links as symlinks?

  - normal programs don't do this, so user would be surprised

  - making OpenFile(shell-link) doesn't automatically open target file,
    like under UNIX.

    Under UNIX you cannot normally open symlink as a file,
    you have to operate on symlink using special functions.

    Under Windows shell-link is a just a file format,
    you can open and read/write it as usual. Normal open operations
    (Reset and such) doesn't derefence the link.
    Instead, ShellExecute('open',file,...) derefences the link.

    So trying to treat shell-links like normal symlinks, I would have
    to manually derefence links in a lot of cases.

  - shell-links under Windows have more information (icon, working dir etc.).
    Using it as mere symlink (using only filename inside) would not feel OK.
}

{ Is FileName a symbolic link. }
function IsSymLink(const FileName: string): boolean; overload;

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
  otherwise (when Warn = @true) makes only OnWarning on failure.
  @raises Exception If delete failed, and Warn = @false. }
procedure CheckDeleteFile(const FileName: string; const Warn: boolean = false);

{ Call RemoveDir and check result.
  @raises Exception If delete failed. }
procedure CheckRemoveDir(const DirFileName: string);

{ Change directory, raising exception if not possible.
  NewDir may (but doesn't have to) include trailing PathDelim.

  @raises EInOutError If changing dir is not possible.

  Improvements over ChDir:
  @orderedList(
    @item(Fixes a bug in Delphi 6, in Delphi 6 ChDir when fails (and program
     is compiled in $I+) does not raise EInOutError (but it sets IOResult).)
    @item(Fixes a bug in FPC 2.4.2-2.4.4:
      http://bugs.freepascal.org/view.php?id=19977 ,
      causing EInOutError to be deferred to later I/O call.)
    @item(Better error message (that will always contain NewDir).)
  ) }
procedure ChangeDir(const NewDir: string);

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

  @deprecated This is deprecated, you should instead operate on URLs
  and combine them using CastleURIUtils.Combines. }
function CombinePaths(BasePath, RelPath: string): string; deprecated;

{ Search a file on $PATH. Works with double quotes around components
  of path list, avoiding this bug: http://bugs.freepascal.org/view.php?id=19279.
  See http://www.freepascal.org/docs-html/rtl/sysutils/filesearch.html
  for original FileSearch docs.

  In FPC >= 2.5.1, you should instead use just ExeSearch(Name).
  It also will use $PATH and avoid double quotes problems on Windows.
  See http://bugs.freepascal.org/view.php?id=19282 and
  fix on http://svn.freepascal.org/cgi-bin/viewvc.cgi?view=rev&revision=17717 . }
Function PathFileSearch(Const Name : String; ImplicitCurrentDir : Boolean = True) : String;

const
  { }
  RegularFileAttr = faReadOnly or faHidden or faArchive;

  { Regular file that is possibly writeable.
    @italic(Possibly) writeable, not @italic(writeable for sure). }
  RegularWriteableFileAttr = RegularFileAttr and (not faReadOnly);

  { Any file, including symlinks. }
  faReallyAnyFile = faAnyFile or faSymLink;

type
  THandleFileMethod = procedure (const FileName: string) of object;

{ Scan recursively subdirectories of given path for files named Name.
  For each file, the HandleFile method is called.
  If URLs is @false, we pass to HandleFile method a filename
  (relative or absolute, just like given Path parameter).
  If URLs is @true then we pass an absolute URL to HandleFile method. }
procedure ScanForFiles(PathURL: string; const Name: string;
  const HandleFile: THandleFileMethod; const URLs: boolean);

{ Get temporary filename, suitable for ApplicationName, checking that
  it doesn't exist. }
function GetTempFileNameCheck: string;

{$ifdef DARWIN}
{ Main directory of the current Mac OS X bundle, including final slash.
  Empty string if we're not run from a bundle. }
function BundlePath: string;
{$endif}

implementation

uses {$ifdef DARWIN} MacOSAll, {$endif} CastleStringUtils,
  {$ifdef MSWINDOWS} CastleDynLib, {$endif} CastleLog, CastleWarnings, CastleURIUtils;

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

  function GetApplicationDataPath: string;
  {$ifdef MSWINDOWS}
  var
    ExePath: string;
  begin
    ExePath := ExtractFilePath(ExeName);

    Result := ExePath + 'data' + PathDelim;
    if DirectoryExists(Result) then Exit;

    Result := ExePath;
  {$endif}
  {$ifdef UNIX}
  var
    CurPath: string;
  begin
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
  {$endif}
  end;

begin
  { Cache directory of ApplicationData. This has two reasons:
    1. On Unix this makes three DirectoryExists calls, so it's not too fast.
    2. It would be strange if ApplicationData results
       suddenly changed in the middle of the program (e.g. because user just
       made appropriate symlink or such). }

  if not ApplicationDataIsCache then
  begin
    ApplicationDataCache := FilenameToURISafe(GetApplicationDataPath);
    if Log then
      WritelnLog('Path', Format('Program data path detected as "%s"', [ApplicationDataCache]));
    ApplicationDataIsCache := true;
  end;

  Result := ApplicationDataCache + Path;
end;

{ other file utilities ---------------------------------------------------- }

function IsSymLink(const FileName: string): boolean;
{$ifdef UNIX}
var statbuf: TStat;
begin
 FpLstat(PChar(FileName), @statbuf);
 Result := FpS_ISLNK(statbuf.st_mode);
{$endif}
{$ifdef MSWINDOWS}
begin
 Result := false;
{$endif}
end;

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
      OnWarning(wtMinor, 'File', Format('Cannot delete file "%s"', [FileName])) else
      raise Exception.Create(Format('Cannot delete file "%s"', [FileName]));
  end;
end;

procedure CheckRemoveDir(const DirFileName:  string);
begin
  if not RemoveDir(DirFileName) then
    raise Exception.Create('Cannot remove directory "' +DirFileName+ '"');
end;

{ dir handling -------------------------------------------------------- }

procedure ChangeDir(const NewDir: string);
begin
{$ifdef MSWINDOWS}
  if not SetCurrentDirectory(PChar(NewDir)) then
{$endif}
{$ifdef UNIX}
  if FpChDir(PChar(NewDir)) < 0 Then
{$endif}
    raise EInOutError.CreateFmt('Cannot change directory to "%s"', [NewDir]);
end;

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
  is stripped from surrounding double quotes. }

Var
  I : longint;
  Temp : String;

begin
  Result:=Name;
  temp:=SetDirSeparators(GetEnvironmentVariable('PATH'));
  // Start with checking the file in the current directory
  If ImplicitCurrentDir and (Result <> '') and FileExists(Result) Then
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
    If (Result <> '') and FileExists(Result) Then
      exit;
  end;
  result:='';
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

procedure ScanForFiles(PathURL: string; const Name: string;
  const HandleFile: THandleFileMethod; const URLs: boolean);
var
  F: TSearchRec;
  FileName, Path: string;
begin
  Path := InclPathDelim(URIToFilenameSafe(PathURL));

  FileName := Path + Name;
  if FileExists(FileName) then
    if URLs then
      HandleFile(FilenameToURISafe(FileName)) else
      HandleFile(FileName);

  if FindFirst(Path + '*', faDirectory, F) = 0 then
  repeat
    if (F.Attr and faDirectory = faDirectory) and
      not SpecialDirName(F.Name) then
      ScanForFiles(Path + F.Name + PathDelim, Name, HandleFile, URLs);
  until FindNext(F) <> 0;
  FindClose(F);
end;

function GetTempFileNameCheck: string;
begin
  Result := GetTempFileName('', ApplicationName);
  { Be paranoid and check whether file does not exist. }
  if FileExists(Result) then
    raise Exception.CreateFmt('Temporary file "%s" already exists', [Result]);
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
