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
    @item(UserConfigFile and UserConfigPath -- user config files)
    @item(ProgramDataPath -- installed program's data files)
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

{ A name of our program.

  Suitable to show to user. Should also indicate how to run the program,
  that is: should be the basename of the executable (although we do depend on it
  technically, but some log messages may suggest it).
  Also suitable to derive config/data paths for this program.

  Right now this is simply equivalent to FPC's ApplicationName.
  We had a complicated mechanisms for this in earlier versions,
  but ultimately the FPC's ApplicationName approach, which is fully configurable
  by callback OnGetApplicationName and has a nice default, is good. }
function ProgramName: string;

{ Returns true if file exists and is a normal file.
  Detects and returns @false for special Windows files
  like 'con', 'c:\con', 'c:\somedir\con' etc.
  ('con' is a special device name).
  For all other files (and other OSes) this function returns the same
  as FileExists. }
function NormalFileExists(const fileName: string): boolean;

{ Path to store user configuration files.
  This is some directory that is probably writeable
  and that is a standard directory under this OS to put user config files.
  Always returns absolute (not relative) path. Result contains trailing
  PathDelim.

  Right now, this is simply a comfortable wrapper around FPC's GetAppConfigDir,
  making sure dir exists and we return it with final path delimiter.
  Which means we look at OnGetApplicationName, and we use OS-specific
  algorithm, see
  http://www.freepascal.org/docs-html/rtl/sysutils/ongetapplicationname.html .
  On UNIX this follows XDG Base Directory Specification,
  see http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html
  (in short: look inside ~/.config/<application-name>/). }
function UserConfigPath: string;

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
  ) }
function UserConfigFile(const Extension: string): string;

{ Path to access installed data files.
  Returns absolute path, containing trailing PathDelim.

  Based on ApplicationName (and possibly ExeName under Windows).
  Here are details:

  (Note that in normal circumstances such details are treated as
  internal implementation notes that shouldn't be exposed...
  But in case of this function, they must be exposed, since
  user and programmer must know how this function works
  (and usually it should be described in documentation of a program).)

  Under Windows: returns ExtractFilePath(ExeName).

  Under UNIXes: tries these three locations, in order:

  @orderedList(
    @item(@code(HomePath +'.' +ApplicationName+'.data/').
      If such directory exists, it is returned.

      This is checked first, to allow local user to always override system-wide
      installation of my program with his own installation.
      E.g. consider the situation when an old version of my program
      is installed system-wide in /usr/local/share/my_program/,
      but some user (with no access to root account) wants to
      install a newer version of it for himself. Now he can do it,
      because ~/.my_program.data/ is checked 1st, system-wide
      /usr/local/share/my_program/ is used only if ~/.my_program.data/
      does not exist.)

    @item(@code('/usr/local/share/' +ApplicationName+ '/').
      If such directory exists, it is returned.

      This is suitable for system-wide installations without package manager.)

    @item(@code('/usr/share/' +ApplicationName+ '/').
      If such directory exists, it is returned.

      This is suitable for system-wide installations with package manager.)

    @item(As a last resort, we return the current directory.
      This always exists, and is an easy way for users to run my game
      without making any symlinks.

      Although conceptually this should be checked first (even before
      @code(HomePath +'.' +ApplicationName+'.data/')), as it's
      @italic("most local"), but we can't: current directory always
      exists. To remedy this, we would need to know some filename inside
      this directory that is required to exist. Maybe for later.)
  ) }
function ProgramDataPath: string;

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

{ User's home directory, with trailing PathDelim.

  Taken from $HOME, unless $HOME = '' or is not defined,
  then I'm trying to take this from user-database by real-uid.
  This is what bash does (more-or-less, when home directory does
  not exist strange things happen), that's what programs should
  do according to `info libc' and my (Kambi's) preferences. }
{$ifdef UNIX} function HomePath: string; {$endif}

{ Expand tilde (~) in path, just like shell. Expands ~ to
  ExclPathDelim(HomePath) under UNIX. Under Windows, does nothing. }
function ExpandHomePath(const FileName: string): string;

{ Call SysUtils.DeleteFile and check result.
  @raises Exception If delete failed. }
procedure CheckDeleteFile(const FileName: string);

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

type
  { }
  EFileOpenError=class(Exception);

{ Safely open files. Does Assign and then resets/rewrites a file.

  @raises(EFileOpenError In case of error. Error message is nice
    and contains filename, contrary to standard EInOutError.)

  SafeReset sets always FileMode variable (to fmOpenRead if ReadOnly,
  or fmOpenReadWrite if not ReadOnly). This is important,
  and too easy to forget otherwise.

  For undefined files, default size is 1, not the strange default 128.
  @groupBegin }
procedure SafeReset(var f: file; const filename: string; readonly: boolean;
  opensize: word = 1); overload;
procedure SafeReset(var f: text; const filename: string; readonly: boolean); overload;

procedure SafeRewrite(var f: file; const filename: string;
  opensize: word = 1); overload;
procedure SafeRewrite(var f: text; const filename: string); overload;
{ @groupEnd }

{ Combines BasePath with RelPath into complete path.
  BasePath MUST be an absolute path,
  on Windows it must contain at least drive specifier (like 'c:'),
  on Unix it must begin with "/". RelPath can be relative and can
  be absolute. If RelPath is absolute, result is RelPath.
  Else the result is an absolute path calculated by combining RelPath
  with BasePath. }
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
  For each file, the HandleFile method is called, with filename
  (given filename is relative or absolute, just like given Path parameter). }
procedure ScanForFiles(Path: string; const Name: string;
  const HandleFile: THandleFileMethod);

implementation

uses CastleStringUtils, {$ifdef MSWINDOWS} CastleDynLib, {$endif} CastleLog;

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
 s := UpperCase(ExtractOnlyFileName(fileName));
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
  Result := GetAppConfigDir(false);
  if not ForceDirectories(Result) then
    raise Exception.CreateFmt('Cannot create directory for config file: "%s"',
      [Result]);

  Result := IncludeTrailingPathDelimiter(Result);
end;

function UserConfigFile(const Extension: string): string;
begin
  Result := UserConfigPath + ApplicationName + Extension;
end;

var
  ProgramDataPathIsCache: boolean = false;
  ProgramDataPathCache: string;

function ProgramDataPath: string;

  function GetProgramDataPath: string;
  {$ifdef MSWINDOWS}
  begin
    Result := ExtractFilePath(ExeName);
  {$endif}
  {$ifdef UNIX}
  begin
    Result := HomePath +'.' +ApplicationName +'.data/';
    if DirectoryExists(Result) then Exit;

    Result := '/usr/local/share/' +ApplicationName +'/';
    if DirectoryExists(Result) then Exit;

    Result := '/usr/share/' +ApplicationName +'/';
    if DirectoryExists(Result) then Exit;

    Result := InclPathDelim(GetCurrentDir);
  {$endif}
  end;

begin
  { Cache results of ProgramDataPath. This has two reasons:
    1. ProgramDataPath_Other on Unix makes three DirectoryExists calls,
       so it's not too fast,
    2. the main reason is that it would be strange if ProgramDataPath results
       suddenly changed in the middle of the program (e.g. because user just
       made appropriate symlink or such). }

  if not ProgramDataPathIsCache then
  begin
    ProgramDataPathCache := GetProgramDataPath;
    if Log then
      WritelnLog('Path', Format('Program data path detected as "%s"', [ProgramDataPathCache]));
    ProgramDataPathIsCache := true;
  end;

  Result := ProgramDataPathCache;
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

procedure CheckDeleteFile(const FileName: string);
begin
 if not SysUtils.DeleteFile(FileName) then
  raise Exception.Create('Cannot delete file "' +FileName+ '"');
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

{ bezpieczne otw. plikow ------------------------------------------------------ }

procedure ShowFileException(E: Exception; const FileName: string);
begin
 raise EFileOpenError.Create('Error ' +E.ClassName +' while trying to open file "'
   +FileName+'" : '+E.Message);
end;

procedure SafeReset(var f: file; const filename: string; readonly: boolean; opensize: word); overload;
begin
 try
  if readonly then FileMode := fmOpenRead else FileMode := fmOpenReadWrite;
  { FileMode dotyczy tylko Reseta i tylko not-text Files.
    Dlatego w innych funkcjach sie nim nie zajmujemy. }
  AssignFile(f,filename);
  Reset(f,opensize);
 except on e: Exception do ShowFileException(e,filename) end;
end;

procedure SafeReset(var f: text; const filename: string; readonly: boolean); overload;
begin
 {readonly not used; but let this param exist - maybe one day
  it will be usable to do something here.}
 try
  AssignFile(f,filename);
  Reset(f);
 except on e: Exception do ShowFileException(e,filename) end;
end;

procedure SafeRewrite(var f: file; const filename: string; opensize: word); overload;
begin
 try
  AssignFile(f,filename);
  Rewrite(f,opensize);
 except on e: Exception do ShowFileException(e,filename) end;
end;

procedure SafeRewrite(var f: text; const filename: string); overload;
begin
 try
  AssignFile(f,filename);
  Rewrite(f);
 except on e: Exception do ShowFileException(e,filename) end;
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

procedure ScanForFiles(Path: string; const Name: string;
  const HandleFile: THandleFileMethod);
var
  F: TSearchRec;
  FileName: string;
begin
  Path := InclPathDelim(Path);

  FileName := Path + Name;
  if FileExists(FileName) then
    HandleFile(FileName);

  if FindFirst(Path + '*', faDirectory, F) = 0 then
  repeat
    if (F.Attr and faDirectory = faDirectory) and
      not SpecialDirName(F.Name) then
      ScanForFiles(Path + F.Name + PathDelim, Name, HandleFile);
  until FindNext(F) <> 0;
  FindClose(F);
end;

initialization
  DoInitialization;
end.
