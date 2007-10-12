{
  Copyright 2002-2006 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ Operations on files.

  Include functions to help cross-platform programs to know
  where to read/write files:
  @unorderedList(
    @item(UserConfigFile and UserConfigPath -- user config files)
    @item(GetTempFname and GetTempDir -- temporary files)
    @item(ProgramDataPath -- installed program's data files)
  )
}
unit KambiFilesUtils;

{$I kambiconf.inc}

interface

uses
  {$ifdef MSWINDOWS} Windows, {$endif}
  {$ifdef UNIX}
    {$ifdef USE_LIBC} Libc, {$else} BaseUnix, Unix, {$endif}
  {$endif}
  SysUtils, KambiUtils;

type
  EExeNameNotAvailable = class(Exception);

{ Returns full (absolute) filename to executable file of this program.
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

{$ifdef DELPHI}
{ This is copied from FPC RTL. }
{ See FPC ApplicationName documentation. }
Function ApplicationName : String;

Type
  TGetAppNameEvent  = Function : String;

Var
  OnGetApplicationName : TGetAppNameEvent;
{$endif}

{ Returns *something like* basename of our executable file.
  This does not contain path and extension of our executable.

  Under UNIXes this may be only the name of user-defined bash
  alias that was used to call our program, or user-defined symlink,
  or just pretty much anything that was passed to our program as
  argv[0].

  Anyway, this is something that can be shown to user to
  identify our program. E.g. useful when you're writing
  error message on console, when UNIX standard is to always prefix
  error message with ProgramName + ': ', like
  'cp: invalid file name'.

  Right now this is simply equivalent to FPC's ApplicationName. }
function ProgramName: string;

{ Basically, returns true if file exists. So what's the difference between
  this and FileExists ? FileExists under Windows returns true for
  such names like 'con', 'c:\con', 'c:\somedir\con' itd.
  ('con' is a special device name). And this function returns false for them.
  For all other files (and other OSes) this function returns the same
  as FileExists. }
function NormalFileExists(const fileName: string): boolean;

{ Returns directory suitable for creating temporary files/directories.
  Note that this directory is shared by other programs, so be careful
  when creating here anything -- to minimize name conflicts
  usually all filenames created here should start with ProgramName
  and then should follow things like process id (or sometimes
  user name, when you can guarantee that one user runs always only one
  instance of this program) or some random number.

  Also remember that good program should avoid creating temporary
  files -- you should keep your content in memory, and store in it
  filesystem only when user really wants to. One exception to this
  rule is when program must cooperate with other program, sometimes
  the only possible (or stable, or fast) way to do this
  is by use of temporary files.

  Always ends with trailing PathDelim. }
function GetTempPath: string;

{ ----------------------------------------------------------------------
  Filenames derived from ProgramName (and possibly ExeName under Windows).

  General comments for all functions below:

  Functions below return filenames like
  - path that program should use to store user configuration files
  - filenames             to use to store user configuration files
  - path that program should use to obtain installed data files

  Results of this functions are based on ProgramName
  (and possibly ExeName under Windows).

  They may also be based on existence on some files/directories.
  E.g. ProgramDataPath under UNIXes checks
  "HomePath + '.' + ProgramName + '.data/'" for existence,
  and if it does not exist it assumes that program was installed
  system-wide and returns "/usr/local/share/ + ProgramName".

  Because from time to time there arises a need to ask from one
  program about "what would be a result of Xxx function if it would
  be called by some other program", all functions come in 2 versions:
  Xxx and Xxx_Other. Xxx_Other does *not* use ProgramName (and ExeName for
  Windows) of current program -- instead caller must pass to Xxx_Other
  value that would be returned by ProgramName/ExeName if it would
  be called from "some other program". If it seems confusing,
  just don't look at Xxx_Other functions, you probably don't need them.
}

{ Returns path that program should use to store user configuration files.
  This is some directory that is supposed to be writeable
  and that is a standard directory under this OS to put user config files.

  E.g.

  - under Windows with PlatformId = VER_PLATFORM_WIN32_NT
    (see GetVersionEx docs, VER_PLATFORM_WIN32_NT means
    Windows Server 2003, Windows XP, Windows 2000, or Windows NT")
    it tries to use
    @code(SHGetSpecialFolderPath(0, @@Path, CSIDL_APPDATA, true))
    This should return something like
      C:\Documents and Settings\<user-name>\Application Data
    ("Application Data" is localized, e.g. it's "Dane aplikacji" on Polish
    Windowses)

    If that fails (because SHGetSpecialFolderPath is not available in
    shell32.dll (this can happen on Windows NT without Internet Explorer 4.0)
    or for some other reason) it falls back on
      ExtractFilePath(ExeName)

  - under other Windowses (this includes Windows 95, 98, Millenium)
    it returns ExtractFilePath(ExeName)

  - under UNIXes it's user's home directory

  Always returns absolute (not relative) path. Result contains trailing
  PathDelim. }
function UserConfigPath: string;

function UserConfigPath_Other(const WindowsExeNamePath: string): string;

{ Returns a filename that program should use to store it's configuration.
  This returns absolute filename that:
  - is inside UserConfigPath
  - has extention FExtension (FExtension should, as always, contain
    beginning dot. E.g. FExtension = '.ini'. This way you can pass
    FExtension = '' to have a filename without extension)
  - filename depends on ProgramName

  This is equivalent to
  UserConfigFile_FromProposed(ProgramName + FExtension) }
function UserConfigFile(const FExtension: string): string;

function UserConfigFile_Other(
  const FExtension, UnixProgramName, WindowsExeName: string): string;

{ Returns abslute file name:
  - inside UserConfigPath
  - with FileName somehow derived from ProposedFileName

  E.g.
  - under UNIXes,  this is UserConfigPath + '.' + ProposedFileName
  - under Windows, this is UserConfigPath + ProposedFileName }
function UserConfigFile_FromProposed(const ProposedFileName: string): string;

function UserConfigFile_FromProposed_Other(
  const ProposedFileName, WindowsExeNamePath: string): string;

{ Returns path that program should use to obtain installed data files.
  Returns absolute path, containing trailing PathDelim.

  Here are details:

  (Note that in normal circumstances such details are treated as
  internal implementation notes that shouldn't be exposed...
  But in case of this function, they must be exposed, since
  user and programmer must know how this function works
  (and usually it should be described in documentation of a program).

  Under Windows : returns ExtractFilePath(ExeName).

  Under UNIXes: returns HomePath +'.' +ProgramName+'.data/'
  if such directory exists, else returns
  '/usr/local/share/' +ProgramName+ '/'.

  Note that HomePath +'.' +ProgramName +'.data/'
  is checked first, this allows user to override system-wide
  installation of my program with his own installation.
  E.g. consider the situation when an old version of my program
  is installed system-wide in /usr/local/share/my_program/,
  but some user (with no access to root account) wants to
  install a newer version of it for himself. Now he can do it,
  because ~/.my_program.data/ is checked 1st, system-wide
  /usr/local/share/my_program/ is used only if ~/.my_program.data/
  does not exist. }
function ProgramDataPath: string;

{ Under Windows WindowsExeNamePath must be
  ExtractFilePath(ExeName) where ExeName is what ExeName would return
  for this "other" program.

  Under UNIX UnixProgramName must be what ProgramName would return
  for this "other" program.

  This way under UNIX parameter WindowsExeNamePath is ignored
  and under Windows parameter UnixProgramName is ignored.
  I know that this looks strange, but this is the only safe way
  to write interface of this procedure.

  Use version without "_Other" suffix to get ProgramDataPath
  of *this* program, that uses this function. }
function ProgramDataPath_Other(
  const UnixProgramName, WindowsExeNamePath: string): string;

{ other file utilities ---------------------------------------------------- }

{ funkcje IsSymLink x 2, CanonicalizeFileName uznaja ze pod Windowsem nie ma
  sym-linkow. Jesli kiedys zmienie zdanie bede musial przebudowac te funkcje.

  Dlaczego nie uznac Windowsowych Shell-linkow za sym-linki ?

  - normalne aplikacje konsolowe tak nie robia i uzytkownik bylby zaskoczony

  - robienie OpenFile(shell-link) nie otwiera automatycznie pliku docelowego
    shell-linka jak to sie dzieje pod UNIXem.

    Pod UNIXem sym-linka nie mozna otworzyc jako takiego, otwieranie go musi
    spowodowac otworzenie pliku docelowego. Operowac na samym sym-linku mozna
    tylko poprzez specjalne procedury z kernela/libc, nie mozna wprost zapisywac
    "zawartosci" samego sym-linka tak jakby byl on zwyklym plikiem.

    Pod windowsem shell-link to tylko taki format pliku i taki plik mozna otworzyc
    recznie i cos w tym formacie zepsuc; Zachowanie polegajace na automatycznym
    schodzeniu po shell-linku nie jest wykonywane przy otwieraniu (Reset/FileReset) pliku,
    natomiast zachodzi przy otwieaniu pliku w sensie ShellExecute('open',plik,...).

    Razem te roznice sprawiaja ze obslugiwanie windowsowych Shell-linkow jest
    arcy-niewygodne. Gdybym przyjal ze shell-linki chce traktowac jak sym-linki
    to chociazby zachowanie na parametr onsProcess byloby rozne pod roznymi
    sys.op'ami - pod Windowsem trzeba by zawsze robic
    Assign(CanonicalizeFileName(plik))
    zamiast Assign(plik), co jest niepotrzebne pod UNIXem. I tak dalej.

  - shell-linki pod windowsem niosa ze soba szersza informacje : ikonke skrotu,
    katalog roboczy, klawisz skrotu, ew. parametry wywolania. W rezultacie nazwa
    shell-link dobrze oddaje istote tego shell-linka : jest to dobry rodzaj skrotu
    ktory mozna uzytkownikowi pokazac, za pomoca ktorego mozna konstruowac jakies
    struktury menu startowego itd.

    Uzywanie shell-linka jako sym-linka w stylu UNIXowym (czyli zwracanie uwagi tylko
    na target-filename) byloby jakies nienaturalne.
}
{ czy fname to symbolic link ? }
function IsSymLink(const fname: string): boolean; overload;

{$ifdef DELPHI}
{ Unfortunately this is not possible to implement in FPC. Kylix has
  Linux-specific field Mode that allows it. This is one of the few points
  where Kylix has something in RTL that is better than in FPC...

  For some time I had in KambiUtils implementation of FindFirst/Next
  and TSearch that had this Mode field. But now, for ease, I'm using
  FPC's TSearchRec. So function IsSymLink(TSearchRec) is not possible.

  Since I do not care about Kylix compatibility anymore this function is
  quite useless for now. It is here because I hope that some day it will be
  possible with FPC.

  2004-12-20 update: bug 2995 was fixed, and FPC 1.9.5 since 2004-12-20
  has TSearchRec.Mode field. Unfortunately, it's implemented in such way
  that when file is symlink, Mode field describes the *target* file
  (using FpStat instead of FpLStat; and it's not a bug -- it was done
  on purpose).
  So doing IS_LNK(Mode) is pointless, as Mode never describes a symlink.
  See here
    http://www.freepascal.org/bugs/showrec.php3?ID=2995
  So this function will probably never be available with FPC.
}
function IsSymLink(const f: TSearchRec): boolean; overload;
{$endif}

{ User's home directory, with trailing PathDelim.

  Taken from $HOME, unless $HOME = '' or is not defined,
  then I'm trying to take this from user-database by real-uid.
  This is what bash does (more-or-less, when home directory does
  not exist strange things happen), that's what programs should
  do according to `info libc' and my (Kambi's) preferences. }
{$ifdef UNIX} function HomePath: string; {$endif}

{ Pod UNIXem rozwijanie ~ to rzecz powloki. Procedury z libc,
  kernela, a za nimi procedury w System czy SysUtils nie rozumieja
  sciezek z nazwa ~.

  Co gorsza ~ to pod Linuxem poprawna nazwa pliku, nawet jesli wiele
  programow wariuje widzac takie nazwy plikow.

  Ponizsza funkcja przeprowadza rozwijanie ~ na poczatku sciezki
  do ExclPathDelim(HomePath).

  Pod windowsem ta funkcja nic nie robi, zwraca filename.  }
function ExpandHomePath(const fname: string): string;

{$ifdef MSWINDOWS}
{ fname : nazwa istniejacego pliku, ze sciezka absolutna lub wzgledna.
  Zwraca nazwe tego pliku gwarantujac ze ostatni czlon (tzn. sama nazwa
  pliku beda uzywaly dlugiej (tj. nie DOSowej) nazwy. }
function GetLongFilename(const fname: string): string;

{ jak GetLongFilename, ale zwraca tylko ten ostatni czlon.
  Wiesz ze jest to nazwa pliku w katalogu ExtractFilePath(fname), i sam
  musisz podjac decyzje co z tym zrobic. }
function GetLongFileOnlyname(const fname: string): string;

{ fname to nazwa istniejacego pliku\, absolutna lub wzgledna.
  Zwraca nazwe tego samego pliku (jesli fname bylo sciezka
  wzgledna to zwrocone fname tez bedzie wzgledne) ale z kazdym czlonem
  zapisanym w wersji long-filename, a wiec zamienia DOSowe czlony
  nazw na windowsowe dlugie. }
function GetLongPathName(fname: string): string;
{$endif}

{ fname to nazwa pliku, moze byc wzgledna. Niniejsza funkcja zwraca
  to samo co ExpandFilename a wiec sciezke absolutna.
  Pod Windowsem dodatkowo gwarantowane jest ze KAZDY KOMPONENT
  tej sciezki bedzie mial nazwe dluga (nie DOSowa).
  Wiec ta funkcja nie tylko rozwija sciezke, ale sprawia ze jest
  zawsze napisana z uzyciem long-filenames. }
function ExpandLongFilename(const fname: string): string;

{ Wykonuje SysUtils.DeleteFile i sprawdza czy sie udalo, jesli nie - exception }
procedure CheckDeleteFile(const FileName: string);
{ Podobnie jak CheckDeleteFile -- wywoluje RemoveDir(DirFileName) i
  jesli sie nie udalo -- exception z sensownym komunikatem }
procedure CheckRemoveDir(const DirFileName: string);

type EFileExists=class(Exception);

{ kopiuj Source na Dest plik, w razie bledu - exception.
  Jesli not CanOverwrite to i plik istnieje wyrzuca exception EFileExists,
  jesli CanOverwrite to robi overwrite pliku dest jesli juz istnial.

  TODO: niech FileCopy kopiuje tez directory }
procedure FileCopy(const SourceFname, DestFname: string; CanOverwrite: boolean);
{ j.w. ale zwraca false zamiast rzucac exception jesli sie nie udalo }
function TryFileCopy(const SourceFname, DestFname: string; CanOverwrite: boolean): boolean;

{ move/rename file. Wersja bezpieczna - pozwala przenosic zarowno pliki jak i katalogi
  pomiedzy roznymi dyskami. W miare mozliwosci sprobuje wykonac normalne MoveFile/Rename
  ale jesli Dest jest na innym file-system niz Source (pod UNIXem
  spowoduje to blad procedury rename(), pod win spowoduje to blad w MoveFile() jezeli
  SourceFname to katalog) to wykona Copy(source,dest)+Delete(source).

  Jesli DestFname istnieje to :
    jesli CanOverwrite to nadpisuje go
    jesli not CanOverwrite to wyrzuca EFileExists.
  W razie bledu - exception.

  Uwagi : pod Linuxem z GNU libc sytuacja gdy SourceFname i DestFname to
  dwie nazwy dla tego samego pliku (do tego samego i-node) jest niezdefiniowana
  -- nie wiadomo
  co w zwiazku z tym zrobi libc.__rename, w zwiazku z czym nie wiadomo tez
  co zrobi moje FileMove. Patrz strona info libc o "Renaming Files".
  Nie wiem jak wyglada tu sytuacja gdy nie uzywam libc (USE_LIBC symbol
  not defined), but I will assume that this situation is also undefined.

  TODO: przenoszenia katalogow nie zrobilem jeszcze (no, w pewnych przypadkach
  mogloby zadzialac ale to sliskie i niedodefiniowane teraz rzeczy). Na razie
  uzywaj FileMove tylko do plikow. }
procedure FileMove(const SourceFname, DestFname: string; CanOverwrite: boolean {$ifdef DEFPARS}=False{$endif}); overload;

{ ChangeDir is something like "an improved replacement for ChDir".
  Changes current directory to NewDir, raises EInOutError if this is
  not possible. In other words, it mimics ChDir when compiled with
  $I (IO checking) on.

  NewDir may (but don't have to) triling PathDelim.

  Improvements:

  1. It fixes a bug in Delphi 6, in Delphi 6 ChDir when fails (and program
     is compiled in $I+) does not raise EInOutError (but it sets IOResult).

     Z $I+ kompilator powinien generowac kod sprawdzajacy zmienna IOResult po
     wywolaniu ChDir i wywolujacy ew. EInOutError (tak jak robi np. po
     wywolaniu Rewrite i w ogole po wywolaniu czegokolwiek o czym wie
     ze moze zwrocic InOutError).

     Ale Delphi 6 nie robi tego - zwykle przeoczenie ? Uzywaj ChangeDir.

  2. Better error message (that will always contain NewDir). }
procedure ChangeDir(const NewDir: string);

{ FnameAutoInc robi Format(fname_pattern, [i]) zwiekszajac i od 0 az
  skonstruuje nazwe pliku ktory nie istnieje. Slowem, niech fname
  bedzie czyms w rodzaju "save_screen_%d.bmp" - FnameAutoInc
  z takim parametrem podstawi za %d najmniejsza liczbe nieujemna taka
  ze nie istnieje plik o tej nazwie. }
function FnameAutoInc(const fname_pattern: string): string;

{ Zwroc katalog nadrzedny do katalogu DirName.
  DirName moze byc absolutna sciezka, ale nie musi.
  Dirname moze ale nie musi zawierac koncowego PathDelim.

  Jesli nie ma katalogu nadrzednego - zwroc DirName.
  Wynik zawsze jest expanded (absolutna sciezka) i
  zawsze zawiera koncowe PathDelim.

  When DoExpandDirName then it is assumed that DirName already IS absolute
  path. What's the advantage of using DoExpandDirName = false ?
  Then this function is pure string-operation
  (no actual reading of any filesystem info), i.e. it belongs
  to KambiUtils_filenames.inc instead of KambiUtils_files.inc.
  This means that
  1. Things work faster
  2. DirName does not need to exist
}
function ParentPath(DirName: string;
  DoExpandDirName: boolean {$ifdef DEFPARS} = true {$endif}): string;

{ operacje na plikach Pascala - file/text ------------------------------------ }

{ bezpieczne otwieranie plikow.
  Najpierw robi assign(f,filename) i potem otwiera plik.

  Jesli byl blad - wyrzuca EFileOpenError z ladnym komunikatem bledu,
  ktory (w przeciwienstwie do standardowego EInOutError rzucanego
  przez standardowe Rewrite) zawiera nazwe pliku, co jest zazwyczaj bardzo
  porêczne !.

  SafeReset controls FileMode variable:
    if ReadOnly then FileMode := fmOpenRead else
                     FileMode := fmOpenReadWrite;
  (because it's oftent too easy to forget to initialize FileMode before
  using Reset).

  Dla plikow niezdefiniowanych - domyslny opensize to 1, a nie 128,
  jak dla Reset/Rewrite (co bylo czesta przyczyna pomylek) }
type EFileOpenError=class(Exception);

procedure SafeReset(var f: file; const filename: string; readonly: boolean;
  opensize: word {$ifdef DEFPARS}=1{$endif}); overload;
procedure SafeReset(var f: text; const filename: string; readonly: boolean); overload;

procedure SafeRewrite(var f: file; const filename: string;
  opensize: word {$ifdef DEFPARS}=1{$endif}); overload;
procedure SafeRewrite(var f: text; const filename: string); overload;

{ Combines BasePath with RelPath. BasePath MUST be an absolute path,
  on Windows it must contain at least drive specifier (like 'c:'),
  on Unix it must begin with "/". RelPath can be relative and can
  be absolute. If RelPath is absolute, result is RelPath.
  Else the result is an absolute path calculated by combining RelPath
  with BasePath. }
function CombinePaths(BasePath, RelPath: string): string;

implementation

uses KambiStringUtils, KambiDynLib;

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

{$ifdef DELPHI}
{ Copied from FPC RTL }
Function ApplicationName : String;

begin
  If Assigned(OnGetApplicationName) then
    Result:=OnGetApplicationName()
  else
    Result:=ChangeFileExt(ExtractFileName(Paramstr(0)),'');
end;
{$endif}

function ProgramName_Other(const ParamStr0: string): string;
begin
 Result :=
   {$ifdef MSWINDOWS} ExtractOnlyFilename(ParamStr0) {$endif}
   {$ifdef UNIX}  ExtractFilename(ParamStr0) {$endif} ;
end;

function ProgramName: string;
begin
  Result := ApplicationName;
  { Result := ProgramName_Other(ParamStr(0)); }
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

function GetTempPath: string;
{$ifdef MSWINDOWS}
var reqlen: Dword;
begin
 reqlen := Windows.GetTempPath(0,nil);
 KambiOSCheck(reqlen>0);
 SetLength(result, reqlen-1);
 KambiOSCheck( Windows.GetTempPath(reqlen,PChar(result)) = reqlen-1 );
 Result := InclPathDelim(result);
{$endif MSWINDOWS}
{$ifdef UNIX}

  function UsableDir(var DirName: string): boolean;
  begin
   Result :=
     (dirname<>'') and
     DirectoryExists(dirname) and
     {$ifdef USE_LIBC}
       (euidaccess(PChar(dirname), W_OK or X_OK)=0)
     {$else}
       true {TODO}
     {$endif};
   if Result then
    DirName := InclPathDelim(DirName);
  end;

begin
 result := GetEnvironmentVariable('TMPDIR');
 if UsableDir(result) then Exit;

 {$ifdef USE_LIBC}
 result := P_tmpdir;
 if UsableDir(result) then Exit;
 {$endif}

 { P_tmpdir is not available when not USE_LIBC.

   But it's a constant '/tmp' (at least with FPC's Libc unit,
   maybe in C stdio.h header it sometimes gets other value
   on some UNIXes) so it can be safely ignored. }

 result := '/tmp/';
 if UsableDir(result) then Exit;

 result := HomePath;
{$endif UNIX}
end;

{ ----------------------------------------------------------------------
  filenames derived from ProgramName (and possibly ExeName under Windows) }

function UserConfigPath: string;
begin
 Result := UserConfigPath_Other(
   {$ifdef MSWINDOWS} ExtractFilePath(ExeName) {$else} '' {$endif}
   );
end;

function UserConfigPath_Other(const WindowsExeNamePath: string): string;
{$ifdef MSWINDOWS}
const
  CSIDL_APPDATA = $001a;
var
  SHGetSpecialFolderPath: function(hwndOwner: HWND; lpszPath: PChar;
    nFolder: Integer; fCreate: BOOL): BOOL; stdcall;
  ShellLib: TDynLib;
  SHPath: array[0 .. MAX_PATH]of char;
begin
 Result := '';

 { TODO: fix this for win64, what's the symbol for VER_PLATFORM_WIN32_NT }
 if Win32Platform = VER_PLATFORM_WIN32_NT then
 begin
  ShellLib := TDynLib.Load(ShellDLL);
  try
   ShellLib.SymbolErrorBehaviour := seReturnNil;
   {$ifdef FPC_OBJFPC} Pointer(SHGetSpecialFolderPath)
   {$else} @SHGetSpecialFolderPath
   {$endif} := ShellLib.Symbol('SHGetSpecialFolderPathA');
   if Assigned(SHGetSpecialFolderPath) and
      SHGetSpecialFolderPath(0, @SHPath, CSIDL_APPDATA, true) then
   begin
    Result := SHPath;
   end;
  finally ShellLib.Free end;
 end;

 if Result = '' then
  Result := WindowsExeNamePath else
  Result := InclPathDelim(Result);
{$endif}
{$ifdef UNIX}
begin
 Result := HomePath;
{$endif}
end;

function UserConfigFile(const FExtension: string): string;
begin
 Result := UserConfigFile_Other(FExtension,
   {$ifdef UNIX} ProgramName {$else} '' {$endif},
   {$ifdef MSWINDOWS} ExeName {$else} '' {$endif});
end;

function UserConfigFile_Other(
  const FExtension, UnixProgramName, WindowsExeName: string): string;
begin
 Result := UserConfigFile_FromProposed_Other(
   {$ifdef MSWINDOWS} ProgramName_Other(WindowsExeName) {$endif}
   {$ifdef UNIX}  UnixProgramName {$endif}
    + FExtension,
   {$ifdef MSWINDOWS} ExtractFilePath(WindowsExeName) {$else} '' {$endif}
   );
end;

function UserConfigFile_FromProposed(const ProposedFileName: string): string;
begin
 Result := UserConfigFile_FromProposed_Other(ProposedFileName,
   {$ifdef MSWINDOWS} ExtractFilePath(ExeName) {$else} '' {$endif});
end;

function UserConfigFile_FromProposed_Other(
  const ProposedFileName, WindowsExeNamePath: string): string;
begin
 Result :=
   {$ifdef MSWINDOWS} UserConfigPath_Other(WindowsExeNamePath) + ProposedFileName {$endif}
   {$ifdef UNIX}  UserConfigPath_Other('') + '.' + ProposedFileName {$endif};
end;

function ProgramDataPath: string;
begin
 Result := ProgramDataPath_Other(
   {$ifdef UNIX}  ProgramName {$endif}
   {$ifdef MSWINDOWS} '' {$endif},
   {$ifdef MSWINDOWS} ExtractFilePath(ExeName) {$endif}
   { Avoid using ExeName under UNIXes }
   {$ifdef UNIX} '' {$endif}
   );
end;

function ProgramDataPath_Other(
  const UnixProgramName, WindowsExeNamePath: string): string;
{$ifdef MSWINDOWS}
begin
 Result := WindowsExeNamePath;
{$endif}
{$ifdef UNIX}
begin
 Result := HomePath +'.' +UnixProgramName +'.data/';
 if not DirectoryExists(Result) then
  Result := '/usr/local/share/' +UnixProgramName +'/';
{$endif}
end;

{ other file utilities ---------------------------------------------------- }

function IsSymLink(const fname: string): boolean;
{$ifdef UNIX}
var statbuf: {$ifdef USE_LIBC} TStatBuf {$else} TStat {$endif};
begin
 {$ifdef USE_LIBC} lstat(PChar(fname), statbuf)
 {$else}           FpLstat(PChar(fname), @statbuf)
 {$endif};

 Result := {$ifdef USE_LIBC} S_ISLNK {$else} FpS_ISLNK {$endif}
   (statbuf.st_mode);
{$endif}
{$ifdef MSWINDOWS}
begin
 Result := false;
{$endif}
end;

{$ifdef DELPHI}
function IsSymLink(const f: TSearchRec): boolean;
{$ifdef UNIX}
begin
 result := S_ISLNK(f.Mode);
{$endif}
{$ifdef MSWINDOWS}
begin
 result := false;
{$endif}
end;
{$endif}

{$ifdef UNIX}
function HomePath:  string;
{$ifdef USE_LIBC}
var ResultBuf: passwd;
    dummy: PPasswordRecord;
    Buffer: PChar;
    alloc: integer;
{$endif}
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

 {$ifdef USE_LIBC}
 { take home dir from user-database looking for real-uid }
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
 {$else}
 {TODO}
 {$endif}

 Result := InclPathDelim(Result);
end;
{$endif}

function ExpandHomePath(const fname: string): string;
{$ifdef UNIX}
begin
 { Rozwin '~' w nazwe home dir. Rozwin '~/xxx' w homedir+'/xxx'. }
 if Length(FName) = 1 then
 begin
  if FName[1] = '~' then
   Result := ExclPathDelim(HomePath) else
   Result := FName;
 end else
 if (Length(fname) > 0) and (FName[1] = '~') then
  Result := HomePath + SEnding(FName, 3) else
  Result := FName;
{$else}
begin
 result := fname;
{$endif}
end;

{$ifdef MSWINDOWS}
function GetLongFilename(const fname: string): string;
begin
 result := ExtractFilePath(fname)+GetLongFileOnlyname(fname);
end;

function GetLongFileOnlyname(const fname: string): string;
var srec: TSearchRec;
begin
 if FindFirst(fname, faAnyFile, srec)<>0 then
  raise Exception.Create('File '+fname+' does not exist');
 result := srec.Name;
end;

function GetLongPathName(fname: string): string;
var p: integer;
    pathpart: string;
begin
 {idea dzialania jest taka : obcinamy z lewej strony fname i doklejamy go do
  result. Robimy to sukcesywnie, starajac sie caly czas utrzymac zaleznosc
  ze result+fname okresla nazwe tego samego pliku co fname, gdzie czesc w result
  zostala juz zamieniona na long-filenames. }
 result := '';
 repeat
  p := Pos(PathDelim, fname);
  if p = 0 then break;

  pathpart := Copy(fname, 1, p-1);
  if (pathpart = '') or SCharIs(pathpart, 2, ':') or SpecialDirName(pathpart) then
  begin
   {mamy tu poczatek sciezki relatywnej bez okreslenia dysku, a wiec zaczynajacej
    sie od "\", albo mamy tu okreslenie dysku, albo mamy okreslenie specjalnego
    katalogu w rodzaju '.' lub '..'. A wiec przepisujemy do result
    bez zmian. }
   result := result+pathpart+PathDelim;
  end else
  begin
   {maym tu nazwe katalogu w katalogu result. Wiec zamieniamy ja na nazwe long i
    doklejamy do result}
   result := result+GetLongFileOnlyName(result+pathpart)+PathDelim;
  end;
  Delete(fname, 1, p);
 until false;
 {zostala nazwa pliku :}
 result := result+GetLongFileOnlyname(result+fname);
end;
{$endif}

function ExpandLongFilename(const fname: string): string;
{$ifdef MSWINDOWS}
begin
 result := ExpandFilename(GetLongPathName(fname));
{$else}
begin
 result := ExpandFilename(fname);
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

procedure FileCopy(const SourceFname, DestFname: string; CanOverwrite: boolean);
{kopiuj plik, w razie bledu - exception}
{ pod windowsem kopiowanie moznaby tu zrobic
  KambiOSCheck( CopyFile(PChar(filename),PChar(backFileName),not CanOverwrite);
  Ale : po pierwsze, SafeReset/Rewrite zwracaja mi lepsze opisy bledow w E.Message
        po drugie, szybkosc tego kopiowania jest doskonala :
           testy - plik 370 MegaB,
               kopiowanie przez WinCommandera - 1min 35sek
               kopiowanie przez Explorera - 1min 30sek
               kopiowanie tutejsze - 1min 25sek !
               (oczywiscie moja metoda nie wymagala uczestictwa interfejsu;
               ale mimo to wniosek brzmi - moja metoda jest co najmniej dobra)
           ...wiec nie mam po co z niego rezygnowac
}
const
  BUF_SIZE = 100000;
var SourceF, DestF: file;
    ReadCount: integer;
    buf: PByte;
begin
 if (not CanOverwrite) and FileExists(DestFname) then
  raise EFileExists.Create('File '+DestFname+' already exists.');
 SafeReset(SourceF, SourceFname, true);
 try
  SafeRewrite(DestF,DestFname);
  try
   buf := GetMem(BUF_SIZE);
   try
    while not Eof(SourceF) do
    begin
     BlockRead(SourceF, buf^, BUF_SIZE, ReadCount);
     BlockWrite(DestF, buf^, ReadCount);
    end;
   finally FreeMem(buf) end;
  finally CloseFile(DestF) end;
 finally CloseFile(SourceF) end;
end;

function TryFileCopy(const SourceFname, DestFname: string; CanOverwrite: boolean): boolean;
begin
 try
  FileCopy(SourceFname, DestFname, CanOverwrite);
  result := true;
 except
  result := false
 end;
end;

procedure FileMove(const SourceFname, DestFname: string; CanOverwrite: boolean{=False});
begin
 if (not CanOverwrite) and FileExists(DestFname) then
  raise EFileExists.Create('File '+DestFname+' already exists.');
{$ifdef MSWINDOWS}
 { kiedys zapisalem to jako
   var dwFlags: Dword;
     dwFlags := MOVEFILE_COPY_ALLOWED;
     if CanOverwrite then dwFlags := dwFlags or MOVEFILE_REPLACE_EXISTING;
     KambiOSCheck( MoveFileEx(PChar(SourceFname),PChar(DestFname),dwFlags) , 'MoveFileEx');
   i dzialalo. A potem przestalo dzialac - MoveFileEx zawsze wywala blad ze jest
   zaimplementowane tylko pod WinNT. ????
   W kazdym razie ponizsza wersja z MoveFile (bez "Ex") dziala zawsze.
   MoveFile zawsze dziala jakby MoveFileEx z flaga MOVEFILE_COPY_ALLOWED,
   BEZ flagi MOVEFILE_REPLACE_EXISTING (wiec DestFname nie moze istniec).
 }

 if CanOverwrite then
 begin
  DeleteFile(DestFname); {don't check DeleteFile result, it may fail}

  { Tests:
  if not DeleteFile(DestFname) then
   RaiseLastKambiOSError;}
 end;

 KambiOSCheck( MoveFile(PChar(SourceFname), PChar(DestFname)), 'MoveFile');

 { TODO: zrob jeszcze obsluge kopiowania katalogow jesli sa na innych dyskach }
{$else}
 { TODO: zrobic zeby mozna bylo move directory z override'm }
 if {$ifdef USE_LIBC} __rename {$else} FpRename {$endif}
   (PChar(SourceFname), PChar(DestFname)) = -1 then
  if Errno = {$ifdef USE_LIBC} EXDEV {$else} ESysEXDEV {$endif} then
  begin
   { gdy sa na innym systemie plikow rob Copy + Delete }
   FileCopy(SourceFname, DestFname, CanOverwrite);
   CheckDeleteFile(SourceFname);
  end else RaiseLastKambiOSError;
{$endif}
end;

{ dir handling -------------------------------------------------------- }

procedure ChangeDir(const NewDir: string);

const
  SChangeDirError = 'Error when changing directory to "%s": ';

  function IOError(ErrorCode: integer): EInOutError;
  begin
   Result := EInOutError.Create( Format(SChangeDirError, [NewDir])
     + SysErrorMessage(ErrorCode));
   Result.ErrorCode := ErrorCode;
  end;

{$ifdef MSWINDOWS}
begin
 if not SetCurrentDirectory(PChar(NewDir)) then raise IOError(GetLastError);
{$endif}

{$ifdef UNIX}
  {$ifdef USE_LIBC}
  begin
   if Libc.__chdir(PChar(NewDir)) <> 0 then raise IOError(Libc.ErrNo);
  {$else}
  begin
   { This a generic implementation of ChangeDir: just call ChDir
     and improve eventual error message. }
   try
    ChDir(NewDir);
   except
    on E: EInOutError do
    begin
     E.Message := Format(SChangeDirError, [NewDir]) + E.Message;
     raise;
    end;
   end;
  {$endif}
{$endif}

end;

function FnameAutoInc(const fname_pattern: string): string;
var i: integer;
begin
 i := 0;
 repeat
  result := Format(fname_pattern,[i]);
  if not FileExists(result) then exit;
  Inc(i);
 until false;
end;

{ Note: the only things here that makes this function belong to
  KambiUtils_files.inc instead of KambiUtils_filenames.inc is
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

procedure ShowFileException(E: Exception; const fname: string);
begin
 raise EFileOpenError.Create('Error ' +E.ClassName +' while trying to open file "'
   +fname+'" : '+E.Message);
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
  FExeName := KamReadLink('/proc/'+IntToStr(
    {$ifdef USE_LIBC} Libc.Getpid {$else} FpGetpid {$endif}
    )+'/exe')
 except
  on EKambiOSError do FExeName := '';
 end;
 {$endif}

 {$ifdef MSWINDOWS} FExeName := ParamStr(0) {$endif};
end;

initialization
  DoInitialization;
end.
