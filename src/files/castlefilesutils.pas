{
  Copyright 2002-2024 Michalis Kamburelis.

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
  )

  Hints about what to use (and what not to use) in a cross-platform applications
  using Castle Game Engine:

  @unorderedList(
    @item(
      To get a nice application name, use the @code(ApplicationName)
      function (defined in the standard SysUtils unit for FPC,
      or CastleUtils unit for Delphi).
      Every application can customize it by assigning OnGetApplicationName
      or (often more comfortable) assigning
      @link(TCastleApplicationProperties.ApplicationName ApplicationProperties.ApplicationName).
      The Castle Game Engine units use it where appropriate.
    )

    @item(
      Use URLs for everything.
      See https://castle-engine.io/manual_network.php .
    )

    @item(
      Use "castle-data:/xxx" URLs to refer to the data directory of your project.
      See https://castle-engine.io/data .
      For example:
      @longCode(#
        MyImage := LoadImage('castle-data:/gui/my_image.png');
      #)
    )

    @item(
      Use the @link(ApplicationConfig) for all URLs where you save or load
      the user configuration files, savegames etc.
    )

    @item(
      Do not try to get the application name, or executable file name,
      using ParamStr(0). Do not rely on Lazarus Application.ExeName
      or our own (deprecated) @link(ExeName).

      The "executable file name" is just not 100% reliably available on all OSes.
      And in some cases, like Android or iOS, the concept of
      @italic("executable file name") doesn't even make sense,
      as your application is a library being called by a higher-level application
      (written in Java or Objective-C), and it's really an "internal matter"
      where is the executable file that started it.
    )

    @item(
      Use CastleFindFiles unit to search for files and directories.
      It nicely works with URLs, has some support for the Android "assets"
      filesystem, and it has more comfortable API (e.g. searching recursively
      or not is just a matter of passing a particular flag).

      Do not use standard FindFirst/FindNext.
    )

    @item(
      Read and write all data using streams (TStream) descendants.
      Open and save these streams using our CastleDownload unit.
    )
  )
}
unit CastleFilesUtils;

{$I castleconf.inc}

interface

uses {$ifdef MSWINDOWS} Windows, {$endif}
  {$ifdef UNIX} {$ifdef FPC} BaseUnix, Unix, {$endif} {$endif}
  SysUtils, CastleUtils;

type
  EExeNameNotAvailable = class(Exception);
  ERemoveFailed = class(Exception);

{ Full (absolute) FileName to executable file of this program.
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
function ExeName: string; deprecated 'as this function is not portable (may raise exception on non-Windows), it should not be used in a cross-platform game code';

{ The name of our program.

  @deprecated Deprecated, this is equivalent to ApplicationName,
  and you should just call ApplicationName directly in new code.
  ApplicationName is included in standard FPC SysUtils unit for FPC,
  has good default and is easily configurable by callback OnGetApplicationName
  or our @link(TCastleApplicationProperties.ApplicationName ApplicationProperties.ApplicationName).
  See http://www.freepascal.org/docs-html/rtl/sysutils/getappconfigdir.html .

  This is suitable to show to user. It should also indicate how to run the program,
  usually it should be the basename of the executable (although we do not depend
  on it technically). It may be used to derive config file/dir for our program,
  see ApplicationConfig. }
function ProgramName: string; deprecated;

{ Returns @true if file exists and is a "regular" file.

  Detects and returns @false for special Windows files
  like 'con', 'c:\con', 'c:\somedir\con' etc.
  ('con' is a special device name).

  Returns @false for directories (on all operating systems,
  unlike FPC FileExists which is inconsistent between OSes
  -- on Unix, FPC FileExists surprisingly answers @true for a directory).

  Consider using URIExists or URIFileExists instead of this function,
  since in CGE you should use URLs for everything. }
function RegularFileExists(const FileName: String): Boolean;

function NormalFileExists(const FileName: String): Boolean; deprecated 'use RegularFileExists';

{ Path to store user configuration files.
  This is some directory that should be writeable
  and that is a standard directory under this OS to put user config files.
  Always returns absolute (not relative) path. Result contains trailing
  PathDelim.

  @deprecated Deprecated, use ApplicationConfig instead. }
function UserConfigPath: string; deprecated;

{ Filename to store user configuration.
  Always returns absolute (not relative) path.

  Returns FileName that:
  @unorderedList(
    @itemSpacing Compact
    @item is inside UserConfigPath
    @item depends on ApplicationName
    @item(has given Extension. Extension should contain
      beginning dot. E.g. FExtension = '.ini'. This way you can pass
      FExtension = '' to have a FileName without extension.)
  )

  @deprecated Deprecated,
  use ApplicationConfig(ApplicationName + Extension) instead. }
function UserConfigFile(const Extension: string): string; deprecated;

var
  { URL used as a prefix of all @link(ApplicationConfig) returned URLs.
    This overrides any autodetection of a suitable "user config" directory
    done by default by @link(ApplicationConfig).

    This must always end with a slash, if it's not empty. }
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
  Which in turn looks at ApplicationName, and may use
  OS-specific algorithm to find good config directory, see
  http://www.freepascal.org/docs-html/rtl/sysutils/ongetapplicationname.html .
  On UNIX this follows XDG Base Directory Specification,
  see http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html
  (simplifying: looks inside ~/.config/<application-name>/). }
function ApplicationConfig(const Path: string): string;

{ @deprecated }
function ApplicationData(const Path: String): String; deprecated 'use ''castle-data:/xxx'' instead of ApplicationData(''xxx'')';

var
  { URL used as the base application data directory.
    Overrides the platform-specific autodetection of this directory.
    See @url(https://castle-engine.io/data data directory
    documentation).

    If it's not empty, this must always end with a slash.

    This cannot start with 'castle-data:/...', since it would mean that
    ResolveCastleDataUrl can never finish its job. }
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

{ Remove file.

  Similar to standard DeleteFile, but this checks result and raises exception
  or makes a warning (see Warn parameter) in case of trouble

  When Warn = @false (default) raises an exception on failure,
  otherwise (when Warn = @true) makes only WritelnWarning on failure.
  @raises ERemoveFailed If delete failed, and Warn = @false. }
procedure CheckDeleteFile(const FileName: string; const Warn: Boolean = false);

{ Remove empty directory.

  Similar to standard RemoveDir, but this checks result and raises exception
  or makes a warning (see Warn parameter) in case of trouble.

  When Warn = @false (default) raises an exception on failure,
  otherwise (when Warn = @true) makes only WritelnWarning on failure.
  @raises ERemoveFailed If delete failed, and Warn = @false. }
procedure CheckRemoveDir(const DirFileName: string; const Warn: Boolean = false);

{ Make sure directory exists, eventually creating it and all directories along the way.

  Similar to standard ForceDirectories, but this checks result and raises exception
  in case of trouble. }
procedure CheckForceDirectories(const Dir: string);

{ Copy file from Source to Dest.
  The directory of Dest must already exist.
  File is overwritten unconditionally. }
procedure CheckCopyFile(const Source, Dest: string);

{ Move file from Source to Dest.
  The directory of Dest must already exist.
  File is overwritten unconditionally. }
procedure CheckRenameFile(const Source, Dest: string);

{ Remove the directory DirName, @italic(recursively, unconditionally,
  with all the files and subdirectories inside).
  DirName may but doesn't have to end with PathDelim.

  In case of symlinks, removes the symlink (but does not descend inside,
  i.e. will not remove files inside a symlink to a directory).

  When Warn = @false (default) raises an exception on failure,
  otherwise (when Warn = @true) makes only WritelnWarning on failure.
  @raises ERemoveFailed If delete failed, and Warn = @false. }
procedure RemoveNonEmptyDir(const DirName: string; const Warn: Boolean = false);

{ Copies the contents from SourceDir to DestinationDir.
  DestinationDir and necessary subdirectories are created, if needed.
  Both SourceDir and DestinationDir may, but do not have to, end with PathDelim.

  Note that DestinationDir contents are not cleared here.
  In effect, the existing files (present in DestinationDir before the copy operation,
  and not overwritten by corresponding files in SourceDir) will be left.
  If you need to clear them, consider using

  @longCode(#
    if DirectoryExists(DestinationDir) then
      RemoveNonEmptyDir(DestinationDir);
    CopyDirectory(SourceDir, DestinationDir);
  #)
}
procedure CopyDirectory(SourcePath, DestinationPath: string);

{ Substitute @code(%d) in given URL (UrlPattern) with successive numbers,
  until the resulting URL (which can be just a filename) doesn't exist.

  We start with number = 0 and do @code(Url := Format(UrlPattern, [Number])),
  until we find non-existing URL. Example UrlPattern is @code('screenshot_%d.png'),
  by saving to this UrlPattern you're relatively sure that each save goes
  to a new file.

  Since we use standard @code(Format) function,
  you can use any @url(https://www.freepascal.org/docs-html/rtl/sysutils/format.html Format placeholder syntax).
  E.g. use @code('screenshot_%.04d.png') to have a number padded with zeros,
  with results like @code('screenshot_0005.png').

  Note that it's possible on every OS that some other program,
  or a second copy of your own program, will write to the resulting URL
  between FileNameAutoInc determined it doesn't exist and you opened the file.
  So using this cannot guarantee that you really always write to a new file
  -- as always, be prepared for anything happening in parallel
  when dealing with a filesystem.

  The overloaded version with separate UrlPrefix and UrlSuffixWithPattern
  replaces @code(%d) in UrlSuffixWithPattern,
  and then glues (unmodified) UrlPrefix with (processed) UrlSuffixWithPattern.
  This is useful when you have a user-specified path,
  where you don't want to perform %d substitution.
  Alternative is to wrap the string in @link(SUnformattable).

  Example usage to save a screenshot:

  @longCode(#
    ScreenshotUrl := FileNameAutoInc(SaveScreenPath, 'screenshot_%d.png');
  #)

  Note that to save a screenshot in the most cross-platform way possible, we advise using
  @link(TCastleContainer.SaveScreenToDefaultFile Window.Container.SaveScreenToDefaultFile)
  instead, it will use @link(SaveScreenPath) or more elebarate mechanism to work on all platforms.

  Example usage to save anything else to user config:

  @longCode(#
    SaveSomethingUrl := FileNameAutoInc(ApplicationConfig('save_something_%d.png'));
  #)

  Note that it will be replaced soon by @code(SaveSomethingUrl := FileNameAutoInc('castle-config:/', 'save_something_%d.png'))
  which also deals with the (unlikely, but still) possibility that ApplicationConfig
  will contain a percent sign.
}
function FileNameAutoInc(const UrlPattern: string): string; overload;
function FileNameAutoInc(const UrlPrefix, UrlSuffixWithPattern: string): string; overload;

function FnameAutoInc(const UrlPattern: string): string;
  deprecated 'use FileNameAutoInc';

{ Parent directory name.

  Given DirName may be absolute or relative.
  Given DirName may but doesn't have to include trailing PathDelim.
  Result is always absolute FileName, and contains trailing PathDelim.

  Returns the same DirName if there's no parent directory.

  When DoExpandDirName = false then it is assumed that DirName already
  is absolute path. Then this function is pure string-operation
  (no actual reading of any filesystem info), so it works faster and
  DirName does not need to exist. }
function ParentPath(DirName: string;
  DoExpandDirName: Boolean = true): string;
  deprecated 'use URLs and operate on them using CastleUriUtils unit';

{ Combines BasePath with RelPath into complete path.
  BasePath MUST be an absolute path,
  on Windows it must contain at least drive specifier (like 'c:'),
  on Unix it must begin with "/". RelPath can be relative and can
  be absolute. If RelPath is absolute, result is RelPath.
  Else the result is an absolute path calculated by combining RelPath
  with BasePath.

  Usually you should instead operate on URLs
  and combine them using @link(CastleUriUtils.CombineURI). }
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
  Returns '' (if not found) or absolute FileName. }
function FindExe(const ExeName: string): string;

{ Add an exe file extension, searching for an existing file starting with ExePath.
  On non-Windows, this is just equal to ExePath + ExeExtension,
  which in practice is just equal to ExePath (since ExeExtension is empty on Unix).
  But on Windows, this tries to append other extensions (.com, .bat, .cmd,
  just like @link(FindExe)), depending on what file exists. }
function AddExeExtension(const ExePath: string): string;

{ Get temporary FileName, suitable for ApplicationName, checking that
  it doesn't exist. }
function GetTempFileNameCheck: string;

{ Return a prefix (beginning of an absolute FileName)
  to save a series of temporary files. }
function GetTempFileNamePrefix: string;

{$ifdef DARWIN}
{ Main directory of the current macOS bundle, including final slash.
  Empty string if we're not run from a bundle. }
function BundlePath: string;
{$endif}

{ Read file or URL contents to a string.
  MimeType is returned, calculated just like the @link(Download) function. }
function FileToString(const Url: String;
  out MimeType: string): AnsiString; overload;
function FileToString(const Url: String): AnsiString; overload;

procedure StringToFile(const Url: String; const Contents: AnsiString);

{ Recommended path where to put screenshots on the current platform.
  Always ends with PathDelim and returns a directory that exists.

  Empty string means that we cannot recommend any safe path to store screenshots
  (on some platforms, like mobile or console, it's not so easy to just store a file
  in the filesystem, without any permissions;
  and on some platforms they may have special API for this stuff). }
function SaveScreenPath: String;

{ Calculate size of all files in this dir. }
function DirectorySize(const Dir: String): QWord;

implementation

uses {$ifdef MSWINDOWS} ShlObj, {$endif}
  {$ifdef DARWIN} MacOSAll, {$endif} Classes,
  {$ifdef FPC} Process, {$endif}
  CastleStringUtils,
  {$ifdef MSWINDOWS} CastleDynLib, {$endif} CastleLog,
  CastleUriUtils, CastleFindFiles, CastleClassUtils, CastleDownload,
  CastleApplicationProperties;

var
  { Initialized once in initialization, afterwards constant. }
  FExeName: string;

function ExeName: string;
begin
  if FExeName = '' then
    raise EExeNameNotAvailable.Create(
      'ExeName: Cannot obtain FileName of executable of this program');
  Result := FExeName;
end;

function ProgramName: string;
begin
  Result := ApplicationName;
end;

function NormalFileExists(const FileName: string): Boolean;
begin
  Result := RegularFileExists(FileName);
end;

function RegularFileExists(const FileName: string): Boolean;
{$ifdef MSWINDOWS}
var
  S: String;
begin
  {$warnings off}
  S := UpperCase(ExtractOnlyFileName(FileName));
  {$warnings on}
  Result := FileExists(FileName) and
     (not( (S = 'CON') or (S = 'PRN') or (S = 'NUL') or
           (S = 'LPT1') or (S = 'LPT2') or (S = 'LPT3') or (S = 'LPT4') or
           (S = 'COM1') or (S = 'COM2') or (S = 'COM3') or (S = 'COM4') ) );
{$else}
begin
  Result := FileExists(FileName) and not DirectoryExists(FileName);
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

  { ApplicationConfig relies that ForceDirectories is reliable
    (on Android, it's not reliable before activity started)
    and ApplicationConfigOverride is set
    (on iOS, it's not set before CGEApp_Initialize called). }
  if not ApplicationProperties._FileAccessSafe then
    WritelnWarning('Using ApplicationConfig(''%s'') before the Application.OnInitialize was called. ' +
      'This is not reliable on mobile platforms (Android, iOS). ' +
      'This usually happens if you open a file from the "initialization" section of a unit. ' +
      'You should do it in Application.OnInitialize instead.',
      [Path]);

  ConfigDir := InclPathDelim(GetAppConfigDir(false));
  Dir := ConfigDir + ExtractFilePath(Path);
  if not ForceDirectories(Dir) then
    raise Exception.CreateFmt('Cannot create directory for config file: "%s"',
      [Dir]);

  Result := FilenameToUriSafe(ConfigDir + Path);
end;

function ApplicationData(const Path: String): String;
begin
  Result := 'castle-data:/' + Path;
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

procedure CheckDeleteFile(const FileName: string; const Warn: Boolean);
begin
  if not SysUtils.DeleteFile(FileName) then
  begin
    if Warn then
      WritelnWarning('File', Format('Cannot delete file "%s"', [FileName])) else
      raise ERemoveFailed.Create(Format('Cannot delete file "%s"', [FileName]));
  end;
end;

procedure CheckRemoveDir(const DirFileName:  string; const Warn: Boolean = false);
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

{ RemoveNonEmptyDir with helpers ---------------------------------------------------------- }

procedure RemoveNonEmptyDir_Internal(const FileInfo: TFileInfo; Data: Pointer; var StopSearch: Boolean);
var
  Warn: Boolean;
begin
  Warn := PBoolean(Data)^;

  if FileInfo.Directory and not FileInfo.Symlink then
    RemoveNonEmptyDir(FileInfo.AbsoluteName, Warn)
  else
    CheckDeleteFile(FileInfo.AbsoluteName, Warn);
end;

procedure RemoveNonEmptyDir(const DirName: string; const Warn: Boolean = false);
begin
  { Note that we don't use ffRecursive,
    as we don't want to descend into directory with symlink.
    We will implement recursion in RemoveNonEmptyDir_Internal manually. }
  FindFiles(DirName, '*', true, @RemoveNonEmptyDir_Internal, @Warn, []);
  CheckRemoveDir(Dirname, Warn);
end;

{ CopyDirectory with helpers ------------------------------------------------- }

type
  TCopyDirectoryHandler = class
    SourcePath, DestinationPath: String;
    procedure FoundFile(const FileInfo: TFileInfo; var StopSearch: Boolean);
  end;

procedure TCopyDirectoryHandler.FoundFile(const FileInfo: TFileInfo; var StopSearch: Boolean);
var
  NewFileName: String;
begin
  { Safer to use FileNameCaseSensitive always.
    On Windows, this should still be true, as we glue SourcePath with rest ourselves
    in CastleFindFiles. }
  if not IsPrefix(SourcePath, FileInfo.AbsoluteName, false) then
    raise Exception.CreateFmt('Cannot copy directory, filename in source directory ("%s") does not have prefix of this directory ("%s")', [
      FileInfo.AbsoluteName,
      SourcePath
    ]);
  NewFileName := DestinationPath + PrefixRemove(SourcePath, FileInfo.AbsoluteName, false);
  CheckForceDirectories(ExtractFilePath(NewFileName));
  CheckCopyFile(FileInfo.AbsoluteName, NewFileName);
end;

procedure CopyDirectory(SourcePath, DestinationPath: String);
var
  Handler: TCopyDirectoryHandler;
begin
  SourcePath := ExpandFileName(InclPathDelim(SourcePath));
  DestinationPath := ExpandFileName(InclPathDelim(DestinationPath));

  Handler := TCopyDirectoryHandler.Create;
  try
    Handler.SourcePath := SourcePath;
    Handler.DestinationPath := DestinationPath;
    FindFiles(SourcePath, '*', false, {$ifdef FPC}@{$endif} Handler.FoundFile, [ffRecursive]);
  finally FreeAndNil(Handler) end;
end;

{ various -------------------------------------------------------------------- }

function FileNameAutoInc(const UrlPattern: string): string;
begin
  Result := FileNameAutoInc('', UrlPattern);
end;

function FileNameAutoInc(const UrlPrefix, UrlSuffixWithPattern: string): string;
var
  I: Integer;
begin
  I := 0;
  repeat
    Result := UrlPrefix + Format(UrlSuffixWithPattern, [I]);
    if URIExists(Result) in [ueNotExists, ueUnknown] then
      Exit;
    Inc(I);
  until false;
end;

function FnameAutoInc(const UrlPattern: string): string;
begin
  Result := FileNameAutoInc(UrlPattern);
end;

{ Note: the only things here that makes this function belong to
  CastleFilesUtils instead of casleutils_filenames.inc is
  using ExpandFileName. }

function ParentPath(DirName: string; DoExpandDirName: Boolean): string;
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

{$ifdef FPC}

{ This is identical to FileSearch, except on Windows each $PATH component
  is stripped from surrounding double quotes.

  Also, uses RegularFileExists instead of FileExists,
  thus it avoids FPC FileExists inconsistency
  (on Unix, FPC FileExists returns true).
  It matters, otherwise e.g. searching for "fpc" on Unix
  could find directory "fpc" that is under a directory on $PATH. }

Var
  I : Integer;
  Temp : String;

begin
  Result:=Name;
  temp:=SetDirSeparators(GetEnvironmentVariable('PATH'));
  // Start with checking the file in the current directory
  If ImplicitCurrentDir and (Result <> '') and RegularFileExists(Result) Then
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
    If (Result <> '') and RegularFileExists(Result) Then
      exit;
  end;
  result:='';
end;

{$else}

begin
  Result := FileSearch(Name, GetEnvironmentVariable('PATH'));
end;

{$endif}

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

  { On Windows when PathFileSearch gets ImplicitCurrentDir = true,
    then it can return relative filename.
    And we want to use ImplicitCurrentDir to search also the current dir
    (as standard on Windows).

    On all platforms, even disregarding ImplicitCurrentDir, PathFileSearch
    doesn't exactly guarantee an absolute path. It should usually be true because $PATH
    should contain only absolute paths, but this is nowhere guaranteed,
    user can set there anything, on any OS.

    So better do ExpandFileName to make sure the result in absolute,
    as promised in FindExe API. E.g. RunCommandSimple usage in ToolCommonUtils
    assumes it, checking IsPathAbsolute to know whether FindExe should be used again. }
  if Result <> '' then
    Result := ExpandFileName(Result);
end;

function AddExeExtension(const ExePath: string): string;
begin
  {$ifdef MSWINDOWS}
  { The default order of extensions is .com, .exe, .bat, .cmd,
    see http://stackoverflow.com/questions/605101/order-in-which-command-prompt-executes-files-with-the-same-name-a-bat-vs-a-cmd }
  if RegularFileExists(ExePath + '.com') then
    Result := ExePath + '.com' else
  if RegularFileExists(ExePath + '.exe' { ExeExtension }) then
    Result := ExePath + '.exe' { ExeExtension } else
  if RegularFileExists(ExePath + '.bat') then
    Result := ExePath + '.bat' else
  if RegularFileExists(ExePath + '.cmd') then
    Result := ExePath + '.cmd' else
  {$else}
    Result := ExePath + ExeExtension;
  {$endif}
end;

{$ifdef MSWINDOWS}
{ Get preferred user directory.
  The DirectoryId is a CSIDL_xxx constant, https://docs.microsoft.com/pl-pl/windows/win32/shell/csidl .

  Returns always a directory with trailing path delimiter,
  unless it is not possible to get then returns ''.
  Checks that directory exists.
}
function GetUserPath(const DirectoryId: Integer): String;
var
  Dir: array [0 .. MAX_PATH] of AnsiChar;
begin
  if (SHGetFolderPath(0, DirectoryId, 0, 0, @Dir) = S_OK) and
     DirectoryExists(Dir) then
    Result := InclPathDelim(Dir)
  else
    Result := '';
end;
{$endif}

{$if defined(UNIX) and (not (defined(DARWIN) or defined(ANDROID) or defined(CASTLE_NINTENDO_SWITCH) or defined(CASTLE_IOS)))}
{ Get preferred user directory, by calling xdg-user-dir
  ( https://jlk.fjfi.cvut.cz/arch/manpages/man/xdg-user-dir.1.en ).

  Returns always a directory with trailing path delimiter,
  unless it is not possible to get then returns HomePath
  (a safe default on normal Unix to store stuff).
  Checks that directory exists.
}
function GetUserPath(const DirectoryId: String): String;
var
  Exe, Dir: String;
begin
  Result := HomePath;

  {$ifdef FPC}
  Exe := FindExe('xdg-user-dir');
  if Exe = '' then
    Exit;

  Dir := '';
  if RunCommand(Exe, [DirectoryId], Dir) then
  begin
    Dir := Trim(Dir);
    if (Dir <> '') and DirectoryExists(Dir) then
      Result := InclPathDelim(Dir);
  end else
  {$endif}

    WritelnWarning('xdg-user-dir call failed');
end;
{$endif}

{$ifndef FPC}

{ Get temporary FileName, also creating this file.
  There seems to be no cross-platform function for this in Delphi. }
function GetTempFileNameDelphi(const Prefix: AnsiString): AnsiString;
{$ifdef MSWINDOWS}
var
  MyPath, MyFileName: array [0..MAX_PATH] of AnsiChar;
begin
  FillChar(MyPath, MAX_PATH, 0);
  FillChar(MyFileName, MAX_PATH, 0);
  OSCheck(GetTempPathA(SizeOf(MyPath), MyPath) <> 0);
  OSCheck(GetTempFileNameA(MyPath, PAnsiChar(Prefix), 0, MyFileName) <> 0);
  Result := MyFileName;
{$endif}
{$ifdef UNIX}
begin
  // TODO: trivial impl for Delphi on Linux
  Result := '/tmp/' + Prefix;
{$endif}
end;

function GetTempFileNameCheck: string;
begin
  Result := GetTempFileNameDelphi(ApplicationName);
end;

function GetTempFileNamePrefix: string;
begin
  Result := GetTempFileNameDelphi(ApplicationName);
end;
{$else}

function GetTempFileNameCheck: string;
begin
  Result := GetTempFileName('', ApplicationName);
  { Be paranoid and check whether file does not exist. }
  if RegularFileExists(Result) or
     DirectoryExists(Result) then
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
      accidentally pick up other images in the temporary directory
      (e.g. leftovers from previous TRangeScreenShot.BeginCapture). }
    { System.Random, not just Random, to avoid using Random from MacOSAll unit. }
    IntToStr(System.Random(MaxInt)) + '_';

  { Check is it really Ok. }
  if FindFirstFile(Result, '*', true, [], FileInfo) then
    raise Exception.CreateFmt('Failed to generate unique temporary file prefix "%s": FileName "%s" already exists',
      [Result, FileInfo.AbsoluteName]);
end;

{$endif}

{$ifdef DARWIN}
var
  BundlePathCached: Boolean;
  BundlePathCache: string;

function BundlePath: string;
{ Based on
  http://wiki.freepascal.org/OS_X_Programming_Tips#How_to_obtain_the_path_to_the_Bundle }
var
  bundle: CFBundleRef;
  pathRef: CFUrlRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
begin
  if not BundlePathCached then
  begin
    bundle := CFBundleGetMainBundle();
    if bundle = nil then
    begin
      BundlePathCache := '';
      WritelnLog('We cannot detect our macOS AppBundle. Probably the application was run directly (like a Unix application, without being wrapped in a directory like "xxx.app"). Some GUI features (like application menu) will not work without running through AppBundle.');
    end else
    begin
      pathRef := CFBundleCopyBundleUrl(bundle);
      pathCFStr := CFUrlCopyFileSystemPath(pathRef, kCFUrlPOSIXPathStyle);
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

function FileToString(const Url: String;
  out MimeType: string): AnsiString;
var
  F: TStream;
begin
  F := Download(Url, [], MimeType);
  try
    { Some streams can be optimized, just load file straight to string memory }
    if (F is TFileStream) or
       (F is TMemoryStream) then
    begin
      SetLength(Result, F.Size);
      if F.Size <> 0 then
        F.ReadBuffer(Result[1], Length(Result));
    end else
      Result := ReadGrowingStreamToString(F);
  finally FreeAndNil(F) end;
end;

function FileToString(const Url: String): AnsiString;
var
  MimeType: string;
begin
  Result := FileToString(Url, MimeType { ignored });
end;

procedure StringToFile(const Url: String; const Contents: AnsiString);
var
  F: TStream;
begin
  F := UrlSaveStream(Url);
  try
    if Length(Contents) <> 0 then
      F.WriteBuffer(Contents[1], Length(Contents));
  finally FreeAndNil(F) end;
end;

var
  CachedSaveScreenPath: String;
  HasCachedSaveScreenPath: Boolean;

function SaveScreenPath: String;
begin
  if HasCachedSaveScreenPath then
    Exit(CachedSaveScreenPath);

  {$if defined(ANDROID) or defined(CASTLE_IOS) or defined(CASTLE_NINTENDO_SWITCH)}
  { These platforms require special treatment. Although we could use

      Result := UriToFilenameSafe(ApplicationConfig(''));

    but then we risk storing more data than expected (users/OS don't expect us
    to fill this space uncontrollably, and users also don't have direct
    access to the ApplicationConfig space). }
  Result := '';
  {$else}
    {$if defined(DARWIN)}
    Result := HomePath; // TODO: probably there's some macOS specific API to get this
    {$elseif defined(UNIX)}
    Result := GetUserPath('PICTURES');
    {$elseif defined(MSWINDOWS)}
    Result := GetUserPath(CSIDL_MYPICTURES);
    {$else}
    Result := '';
    {$endif}
  {$endif}

  CachedSaveScreenPath := Result;
  HasCachedSaveScreenPath := true;
end;

{ DirectorySize utility ------------------------------------------------------ }

type
  TDirectorySizeHelper = class
  public
    { Total size of all files in the directory. }
    Size: QWord;
    procedure FoundFile(const FileInfo: TFileInfo; var StopSearch: Boolean);
  end;

procedure TDirectorySizeHelper.FoundFile(const FileInfo: TFileInfo; var StopSearch: Boolean);
begin
  Size := Size + FileInfo.Size;
end;

function DirectorySize(const Dir: String): QWord;
var
  Helper: TDirectorySizeHelper;
begin
  Helper := TDirectorySizeHelper.Create;
  try
    FindFiles(Dir, '*', false, {$ifdef FPC}@{$endif}Helper.FoundFile, [ffRecursive]);
    Result := Helper.Size;
  finally FreeAndNil(Helper) end;
end;

procedure InitializeExeName;
{$ifdef LINUX}
var
  ExeLinkName: String;
{$endif}
begin
  { Initialize FExeName. }
  FExeName :=
    {$ifdef MSWINDOWS} ExeNameFromGetModule
    // On non-Windows OSes, using ParamStr(0) for this is not reliable, but at least it's some default
    {$else} ParamStr(0)
    {$endif};

  {$if defined(LINUX) and defined(FPC)}
  // TODO: we could port this to Delphi
  { Under Linux, try to use /proc/getpid()/exe.
    This is more reliable than ParamStr(0),
    as ParamStr(0) is set by the calling process,
    and it may be absolute or relative, it may be symlink,
    and in general it may contain anything. }
  ExeLinkName := '/proc/' + IntToStr(FpGetpid) + '/exe';
  try
    FExeName := CastleReadLink(ExeLinkName);
  except
    on EOSError do
      WritelnWarning('Cannot read "%s" (to get ExeName on Linux)', [ExeLinkName]);
  end;
  {$endif}
end;

initialization
  InitializeExeName;
end.
