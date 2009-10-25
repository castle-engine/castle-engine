{
  Copyright 2002-2005 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{ @abstract(Enumerating filenames matching some mask.)

  In short, intention is to wrap FindFirst/Next procedures in much
  safer and cleaner interface.
}

unit EnumerateFiles;

{
  TODO:
  - better interface, probably TEnumerateFiles as class
  - don't select files using faXxx constants, as these do not have
    perfect definition on non-Dos/Windows systems.
    Maybe use readdir and opendir on UNIX systems
    (instead of relaying on FindFirst/Next ported to UNIX).
}

{$I kambiconf.inc}

interface

uses SysUtils, KambiUtils, Classes;

{$define read_interface}

const
  { }
  RegularFileAttr = faReadOnly or faHidden or faArchive;

  { Subset of RegularFileAttr that is possibly writeable.
    @italic(Possibly) writeable, not @italic(writeable for sure). }
  RegularWriteableFileAttr = RegularFileAttr and (not faReadOnly);

  { This is really any file. Since FPC's faAnyFile doesn't include faSymLink,
    faReallyAnyFile does contain faSymLink. }
  faReallyAnyFile = faAnyFile or faSymLink;

type
  TEnumeratedFileInfo = record
    { SearchRec, as returned by FindFirst / FindNext }
    SearchRec: TSearchRec;
    { expanded file name (i.e. with absolute path) }
    FullFileName: string;
  end;
  PEnumeratedFileInfo = ^TEnumeratedFileInfo;

  TDynArrayItem_1 = TEnumeratedFileInfo;
  PDynArrayItem_1 = PEnumeratedFileInfo;
  {$define DYNARRAY_1_IS_STRUCT}
  {$define DYNARRAY_1_IS_INIT_FINI_TYPE}
  {$I dynarray_1.inc}
  TDynEnumeratedFileInfoArray = TDynArray_1;

type
  { You can raise this from FileProc
    (given as FileProc: TEnumFileProc / TEnumFileMethod
    parameter to EnumFiles) to exit from enclosing EnumFiles.
    This is useful to terminate search. }
  BreakEnumFiles = class(TCodeBreaker);

  TEnumFileProc =
    procedure (const FileInfo: TEnumeratedFileInfo; Data: Pointer);
  TEnumFileMethod =
    procedure (const FileInfo: TEnumeratedFileInfo) of object;

  TEnumFilesOption = (eoSymlinks, eoRecursive, eoDirContentsLast,
    eoAllowStdIn, eoReadAllFirst);
  TEnumFilesOptions = set of TEnumFilesOption;

{ Argumenty :
  - Mask moze byc z wieloznacznikami * i ?, moze byc ze sciezka,
    wzgledna lub bezwzgledna.
    Sama sciezka nie moze zawierac * i ? (tak dzialaja FindFirst/Next).

  - Attr : skladanka faXxx; zwykle pliki (bez zadnych atrybutow) sa
    ZAWSZE znajdowane i przekazywane do FileProc (tak dzialaja FindFirst/Next).

  - FileProc : procedura do ktorej przekaze wszystkie znalezione pliki.

  - Options (common default value is [eoSymlinks]) :

    eoSymlinks:
      Means that symlinks should also be enumerated. Excluding
      this from Options means that symlinks will not be reported
      to FileProc. Note that this means that you do *not* control
      whether symlinks are enumerated using Attr parameter,
      you control it only by including or excluding eoSymlinks
      from Options.

    eoRecursive:
      If eoRecursive in Options then EnumFiles (and friends) descend into
      subdirectories.

      Note that it always descends into *every* subdirectory (not only
      into those matching Mask, as I would consider such behaviour pretty
      useless; but maybe someday I'll implement "UNIX tools recursive
      behaviour" -- decend only into subdirs matching Mask and
      enumerate all files inside (like Mask = '*'),
      this is sensible in some situations (even if it's done only because
      it is the shell that expands wilcards),
      this will be activated by some eoTraditionalRecursion in Options).

      Also, note that including eoRecursive in Options
      is something completely different than the flag faDirectory in Attr:
      faDirectory in Attr says whether to report directories to FileProc;
      eoRecursive says whether to descend into directories and enumerate
      their files too.

      Recursive does *not* descend into symlinks to directories.
      Why ? Well, this would produce risk of falling into infinite loop
      (unless some time-consuming precautions would be taken).
      In other words, it's just not implemented.
      But it *may* be implemented someday, as this would be definitely
      something useful.

    eoDirContentsLast:
      This is meaningfull only if eoRecursive is also included in Options.

      pl: jesli not included to w kazdym katalogu najpierw wyrzuca do
      FileProc jego zawartosc a potem dopiero wchodzi
      rekurencyjnie w jego podkatalogi. A wiec kazdy podkatalog zostanie zgloszony
      do FileProc zanim zostanie zgloszona jego zawartosc (chyba ze nazwa
      podkatalogu nie pasuje do maski, wtedy on sam nigdy nie zostanie
      zgloszony do FileProc).

      Jesli included to na odwrot - najpierw wchodzi w glab, a potem dopiero
      wypisuje zawartosc. Wiec kazdy podkatalog zostanie zgloszony do FileProc
      albo wcale albo dopiero po wypisaniu calej jego zawartosci.

    eoAllowStdIn:
      Then Mask equal exactly '-' will be intrepreted specially:
      we will then return exactly one file record, with SearchRec.Name
      and FullFileName equal to '-'.

    eoReadAllFirst:
      See below.

  Wszystkie wersje EnumFiles zwracaja ile razy zostalo wywolane FileProc.
  Zazwyczaj (dopoki nie wykonujesz w FileProc jakiegos dodatkowego filtrowania
  plikow z ktorymi bedziesz cos robil - wtedy musisz to zrobic samemu)
  jest to dobra podstawa do wypisania np. podsumowania
  "xxx files processed" lub wypisania w razie wyniku 0 ze
  "No matching files found" (szczegolnie to drugie jest wazne i czesto uzyteczne -
  np. jezeli program bierze jako argument nazwe pliku i podamy zla
  nazwe pliku to user spodziewa sie ze program wypisze "wrong filename".
  Ale gdy user moze podac _maske_ nazwy pliku (co jest zazwyczaj uznawane
  za bardziej ogolne niz podawania prostej nazwy pliku) program musi
  wlasnie sprawdzic wynik EnumFiles <> 0 aby cos takiego wypisac).

  Some notes:
  - Pamietaj ze Windowsowe funkcje FindFirst/NextFile sa oczywiscie glupie i
    w zwiazku z tym zawieraja bledy (nazywane 'nietypowa funkcjonalnoscia') :
    szukanie '*.txt' pod windowsem spowoduje tak naprawde szukanie maski
    '*.txt*' tyle ze koncowy ciag * nie moze zawierac kropki (innymi slowy
    *.txt nie oznacza "pliki z ostatnim rozszerzeniem txt" ale "pliki z ostatnim
    rozszerzeniem zaczynajacym sie od txt".

    TODO: workaround this stupidity - czyli zrobic wlasne FindFirst/Next(File)
          i samemu dopasowywac maski.
    TODO: zrobic tez obsluge miltiple-masks w formacie : Mask1+PathSep+Mask2 itp..
          w rezultacie np. *.exe;*.tpu;*.~*

  - jak napisalem przy EnumFiles_Core, implementacja polega na fakcie ze
    sym-linki sa uznawane przez FindFirst/Next za pliki systemowe (faSysFile).
    Tym niemniej, wszystko jest zorganizowane tak ze nawet jesli nie podasz
    w Attr faSysFile ale podasz eoSymlinks to procedura
    FileProc bedzie dostawala takze te sym-linki (wyspecyfikowanie lub nie atrybutu
    faSysFile w Attr bedzie mialo za to znaczenie jesli plik bedzie plikiem
    systemowym, ale nie sym-linkiem (np. block/char device)).

    W kazdym razie, chcialem tu zwrocic uwage ze potencjalnie niebezpieczny
    jest fakt ze FileProc moze dostawac pliki z atrybutem faSysFile nawet
    jesli tego nie zazada.

  ------------------------------------------------------------
  eoReadAllFirst in Options:

  if eoReadAllFirst included in Options then before calling FileProc
  (or FileMethod),
  we will first read *all* file infos (TEnumeratedFileInfo)
  to some internal TEnumeratedFileInfo's array.
  And then we will call FileProc (or FileMethod) for each item
  of this array.
  What does this mean ?
  It means that when we will be calling FileProc (or FileMethod),
  changes made to some directory (that we search)
  (e.g. renaming / deleting / creating files) will not have any effect on the
  list of files we will get.
  This is important when we are changing some directory (that we search)
  inside FileProc (or FileMethod).

  E.g. assume that you write FileProc to
  "for each file, create a file named FullFileName + '.copy' with some
  contents", e.g. if you will have directory with two files
    foo  bar
  then you want to convert it to directory with files
    foo  foo.copy  bar  bar.copy
  Assume that you give ReadAllFirst = false.
  FileProc is called with file 'foo', it creates 'foo.copy'.
  Question: do you will receive information about file 'foo.copy'
  in some subsequent FileProc ? Answer: you don't know.
  It depends on implementation of SysUtils.FindFirst/Next
  and it depends on implementation of some OS-specific
  things (e.g. Windows.FindFirst/NextFile under Windows).
  In fact under Windows my experience shows that it's random:
  sometimes you will get foo.copy, sometimes you will not.

  Such uncertain behaviour is very dangerous in this situation:
  your program may loop forever (creating foo.copy, foo.copy.copy,
  foo.copy.copy.copy .... files), your program may create
  e.g. foo.copy and foo.copy.copy and then terminate in a normal way,
  etc.

  So including eoReadAllFirst is a way to handle this: we will read directory
  contents (two entries: 'foo' and 'bar') into internal array and
  then it is guaranteed that FileProc will be called with
  these two entries. It doesn't matter if you will create some
  files inside FileProc. It doesn't matter if you will delete
  or rename some files.

  Also it doesn't matter if some other program creates/renames/deletes
  some files inside directory while you will be processing FileProc.
  So it's possible that FileProc will get infos about files that
  no longer exist. But you're always running such risk in a multi-task OSes,
  in fact, this risk exists even when eoReadAllFirst is not included,
  it's even bigger (because the time when you actively read
  directory from OS (and you are more vulnerable to changes to it)
  is longer with eoReadAllFirst is not in Options).
}
function EnumFiles(const Mask: string; Attr: integer;
  FileProc: TEnumFileProc; FileProcData: Pointer;
  Options: TEnumFilesOptions): Cardinal; overload;

function EnumFilesObj(const Mask: string; Attr: integer;
  FileMethod: TEnumFileMethod;
  Options: TEnumFilesOptions): Cardinal; overload;

{ EnumFilesWritelnCount calls result := EnumFiles(with given args) and then
  calls InfoWrite with a line like
    No files matching "', Mask, '" found (if result = 0)
    1 file processed (if result = 1)
    result, ' files processed (else)
  based on result.

  Wersja EnumFilesWritelnZero wypisuje tylko
    No files matching "', Mask, '" found (if result = 0)
  ,w innej sytuacji milczy.

  It's useful for simple printing the results of EnumFiles.
}
function EnumFilesWritelnCount(const Mask: string; Attr: integer;
  FileProc: TEnumFileProc; FileProcData: Pointer;
  Options: TEnumFilesOptions): Cardinal; overload;

function EnumFilesWritelnZero(const Mask: string; Attr: integer;
  FileProc: TEnumFileProc; FileProcData: Pointer;
  Options: TEnumFilesOptions): Cardinal; overload;

{ Robi tak: najpierw wymaga zeby Parameters.High >= 1.
  (jesli nie, exception "expected
  at least one file mask").
  Potem dla kazdego Parameters[1] ... Parameters[Parameters.High]
  wykonuje EnumFiles(Parameters[I], ...) gdzie ... sa brane z podanych parametrow.
  Jezeli jakies Parameters[I] zwroci 0 to pisze ze "No files matching ...".
  Wyniki wszystkich EnumFiles sumuje i (zanim zwroci go jako swoj result)
  jesli na koncu otrzyma cos > 0 to wypisuje "xxx files processed".

  Wnioski: jezeli wiesz ze teraz w parametrach
  Parameters[1] ... Parameters[Parameters.High]
  masz maski plikow (i powinienes miec przynajmniej jeden taki parametr
  w reku) to mozesz uzyc tej funkcji aby jednoczesnie wykonac iterowanie
  po Parameters i wykonywanie odpowiednich EnumFiles na kazdym parametrze
  (i wypisywanie odpowiednich informacji o wyniku wszystkich EnumFiles).

  O ile samo w sobie to zadanie nie jest zbyt ambitne to najwazniejsza
  rzecza jaka daje ta funkcja jest ze beda wypisywane przez InfoWrite
  komunikaty dla usera ktore powiedza mu
    1) jezeli jakis Parameters[I] nie byl Mask pasujaca do czegokolwiek
       (a wiec, w szczegolnosci, sam Parameters[I] nie byl tez gotowa
       nazwa zadnego istniejacego pliku)
    2) ile w sumie plikow zostalo przetworzonych przez wszystkie wywolania
       EnumFiles.
  Zwracam uwage ze na pewno ta funkcja wypisze min 1 komunikat :
  albo ze "xxx files processed" albo (gdyby Parameters.High = 1 i Parameters[1]
  nie pasowalo) ze "No files matching". (no, o ile nie wyjdzie z wyjatkiem
  oczywiscie).
}
function EnumFilesWritelnParameters(Attr: integer;
  FileProc: TEnumFileProc; FileProcData: Pointer;
  Options: TEnumFilesOptions): Cardinal; overload;

{$undef read_interface}

implementation

uses KambiFilesUtils;

{$define read_implementation}
{$I dynarray_1.inc}

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
}

{ This is equivalent to EnumFiles with Recursive = false
  and ReadAllFirst = false,
  but it doesn't catch BreakEnumFiles.

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
      FullName under FPC. See comments at IsSymLink in KambiUtils.
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
  and ReadAllFirst = false,
  but it doesn't catch BreakEnumFiles. }
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
  and ReadAllFirst = false,
  but it doesn't catch BreakEnumFiles. }
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

{ This is equivalent to EnumFiles with ReadAllFirst = false,
  but it doesn't catch BreakEnumFiles }
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
 TDynEnumeratedFileInfoArray(Data).AppendItem(FileInfo);
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
  FileInfos: TDynEnumeratedFileInfoArray;
  i: Integer;
begin
 if (Mask = '-') and (eoAllowStdIn in Options) then
 begin
   FileProc(StdInFileInfo, FileProcData);
   Result := 1;
 end else
 if eoReadAllFirst in Options then
 begin
  FileInfos := TDynEnumeratedFileInfoArray.Create;
  try
   Result := EnumFiles_NonReadAllFirst(Mask, Attr,
     {$ifdef FPC_OBJFPC} @ {$endif} FileProc_AddToFileInfos, FileInfos,
     eoSymlinks in Options,
     eoRecursive in Options,
     eoDirContentsLast in Options);
   try
    for i := 0 to FileInfos.Count - 1 do
     FileProc(FileInfos.Items[i], FileProcData);
   except on BreakEnumFiles do ; end;
  finally FileInfos.Free end;
 end else
 begin
  try
   Result := EnumFiles_NonReadAllFirst(Mask, Attr,
     FileProc, FileProcData,
     eoSymlinks in Options,
     eoRecursive in Options,
     eoDirContentsLast in Options);
  except
   on BreakEnumFiles do
    { TODO: on BreakEnumFiles, Result should remain with valid value.
      Below (Result := 0) is just a temporary replacement: Result will
      not be correct (always 0), but at least it's not undefined. }
    Result := 0;
  end;
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

{ EnumFilesWriteln* ---------------------------------------------------------- }

const
  SEFW_NoMatching = 'No files matching "%s" found';
  SEFW_FilesDone = '%d file(s) processed';
  SEFW_FilesDone_1 = '1 file processed';
  SEFW_FilesDone_2OrMore = '%d files processed';

function EnumFilesWritelnCount(const Mask: string; Attr: integer;
  FileProc: TEnumFileProc; FileProcData: Pointer;
  Options: TEnumFilesOptions): Cardinal;
begin
 result := EnumFiles(Mask, Attr, FileProc, FileProcData, Options);
 case result of
  0: InfoWrite(Format(SEFW_NoMatching, [Mask]));
  1: InfoWrite(SEFW_FilesDone_1);
  else InfoWrite(Format(SEFW_FilesDone_2OrMore, [result]));
 end;
end;

function EnumFilesWritelnZero(const Mask: string; Attr: integer;
  FileProc: TEnumFileProc; FileProcData: Pointer;
  Options: TEnumFilesOptions): Cardinal;
begin
 result := EnumFiles(Mask, Attr, FileProc, FileProcData, Options);
 if result = 0 then InfoWrite(Format(SEFW_NoMatching, [Mask]));
end;

function EnumFilesWritelnParameters(Attr: integer;
  FileProc: TEnumFileProc; FileProcData: Pointer;
  Options: TEnumFilesOptions): Cardinal;
var i: Integer;
begin
 if Parameters.High = 0 then
  raise EInvalidParams.Create('Expected at least one filename-mask as parameter');

 result := 0;

 for i := 1 to Parameters.High do
 begin
  result := result + EnumFilesWritelnZero(Parameters[i], Attr,
    FileProc, FileProcData, Options);
 end;

 if result > 0 then InfoWrite(Format(SEFW_FilesDone, [result]));
end;

end.
