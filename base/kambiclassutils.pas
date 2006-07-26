{
  Copyright 2000-2006 Michalis Kamburelis.

  This file is part of "Kambi's base Pascal units".

  "Kambi's base Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's base Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's base Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(Kambi class utilities.)

  This unit contains stuff for dealing with non-visual classes.
  Basically, it can be considered as the extension of Classes unit from RTL.
  It contains many wrappers for classes that are defined in the Classes unit
  and also defines some basic useful classes made entirely on my own.
  It also contains many global functions that deal with some classes
  defined in Classes unit.

  Some notes about TStream descendants :
  @unorderedList(
    @item(
      I call a stream "purely sequential" (or just "sequential")
      if it allows only reading and/or writing of data
      and does not allow free "Seek" calls,
      in particular --- it does not allow Seek to move back in a stream.)

    @item(
      I call a stream "growing" if it's read-only and it's purely sequential
      and it's Size property is useless. In other words, when you read
      a "growing" stream, you don't know when it ends, until you reach the end.
      You just have to read data until Read returns 0.)

    @item(Some question related to "growing" streams:

      Reading the Borland's help it's not clear whether after
        @longCode# ReadCount := Stream.Read(Buf, Count) #
      one should test
        @longCode# ReadCount = 0 #
      or rather the test
        @longCode# ReadCount < Count #
      is sufficient.

      In other words, if Read can't read exactly Count bytes but it
      can read @italic(some) bytes (more than zero) :
      can we then be sure we're standing at the end of a stream ?
      Or maybe we should call Read once again and only if this time
      ReadCount = 0 we are sure it's the end of stream ?
      Answer : test "ReadCount = 0" is good. Test "ReadCount < Count" is not
      sufficient in many cases (e.g. with THandleStream when handle is
      stdin (so StdinStream defined in this unit is "growing")
      or with net socket streams).

      This question occurs only when you have to deal with "growing" streams
      --- with non-growing streams you can always test whether
      Stream.Position <= Stream.Size. And, thinking about
      implementation of various non-growing streams,
      one can be relatively sure that the test "ReadCount < Count"
      will be @italic(usually) sufficient.)
  )
}

unit KambiClassUtils;

{ TODO:
  TStreamReaderMediator class :
  ta klasa (podklasa TStream ? Niekoniecznie !) bedzie zapewniala metode
  do odczytywania strumienia. Bedzie ona pobierala jako arg. strumien
  TStream z ktorego odczyt ma posredniczyc i umozliwiala odczyt
  z tego strumienia dodajac wlasne funkcje :

  - przede wszystkim, nieograniczony bufor Unget. A wiec cofanie swojej
    pozycji w strumieniu o ten 1 czy ilestam bajtow bedzie zawsze mozliwe,
    co nie jest prawda dla samego TStream.

  - Ta klasa powinna miec metody ReadUpto_xxx ktorych dzialanie bedzie
    analogiczne to tych w tym module ale te metody beda robic backEndingChar
    na posredniku i beda mogly zawsze dzialac.

  - CurrentLine i CurrentColumn, jako inny sposob wyrazania Position :
    to bedzie pomocne.

  To bedzie przydatna do TPascalLexer i TVRMLLexer ktore w tym momencie cierpia :
  uzywaja bezposrednio typu TStream, robiac na Seek(-1, soFromPosition)
  i nie majac informacji o CurrentLine i CurrentColumn.

  Nalezy taka rzecz zrobic ogolnie.
}

{$I kambiconf.inc}
{$ifdef DELPHI} {$warn SYMBOL_PLATFORM OFF} {$endif}

interface

uses Classes, SysUtils, KambiUtils, IniFiles, KambiStringUtils;

{ ---------------------------------------------------------------------------- }
{ @section(Text reading) }

type
  { TTextReader reads given Stream line by line.
    Lines may be terminated in Stream with #13, #10, #13+#10 or #10+#13.
    This way I can treat any TStream quite like standard Pascal text files:
    I have simple Readln method.

    After calling Readln or Eof you should STOP directly using underlying
    Stream (but you CAN use Stream right after creating
    TTextReader.Create(Stream) and before any Readln or Eof
    operations on this TTextReader). }
  TTextReader = class
  private
    Stream: TStream;
    ReadBuf: string;
    FOwnsStream: boolean;
    { This is either #0 or #10 (tells to ignore next #13 char) or #13
      (tells to ignore next #10 char) }
    LastNewLineChar: char;
  public
    { This is a comfortable constructor, equivalent to
        TTextReader.Create(TFileStream.Create(FileName, fmOpenRead), true) }
    constructor CreateFromFileStream(const FileName: string);

    { If AOwnsStream then in Destroy we will free Stream object. }
    constructor Create(AStream: TStream; AOwnsStream: boolean);
    destructor Destroy; override;

    { Reads next line from Stream. Returned string does not contain
      any end-of-line characters. }
    function Readln: string;

    function Eof: boolean;
  end;

{ ---------------------------------------------------------------------------- }
{ @section(TIniFile related) }

type
  { TIniFile with minor enhancements. }
  TKamIniFile = class(TIniFile)
  private
    FUpdateOnDestroy: boolean;
  public
    { Clears all sections in ini file, therefore deleting all info in ini file. }
    procedure Clear;

    { If true UpdateFile will be automatically called on Destroy.
      Useless under Delphi, useful under Kylix,
      TODO: I don't know how about multiplatform FPC's TIniFile. }
    property UpdateOnDestroy: boolean
      read FUpdateOnDestroy write FUpdateOnDestroy;

    constructor Create(const AFileName: string; AUpdateOnDestroy: boolean);
    destructor Destroy; override;
  end;

{ ---------------------------------------------------------------------------- }
{ @section(TObjectsList_Abstract) }

type
  { }
  TObjectsList_Abstract = class
    procedure FreeContents; virtual; abstract;
    { If Self <> nil then call FreeContents and Destroy. }
    procedure FreeWithContents;
  end;

{ Equivalent to FreeAndNil, but calls FreeWithContents instead of Free.
  Be careful --- pass here only TObjectsList_Abstract descendants ! }
procedure FreeWithContentsAndNil(var Obj);

{ ---------------------------------------------------------------------------- }
{ @section(TStrings related) }

{ Add some strings to TStrings }
procedure StringsAdd(Strs: TStrings; Count: integer; itemVal: string='dummy'); overload;

{ Add all strings from string array to TStrings object }
procedure AddStrArrayToStrings(const StrArr: array of string; strlist: TStrings);

{ wersje z IniFile : zapisuja klucze o nazwach Count i ItemXxx gdzie Xxx to numer stringa
  w podanej sekcji. Defaultowo zostanie przyjete count = 0. }
procedure Load_Strings(IniFile: TIniFile; const section: string; Strs: TStrings); overload;
procedure Save_Strings(IniFile: TIniFile; const section: string; Strs: TStrings); overload;

{ ponizsze trzy funkcje dopisuja do listy slist katalogi znalezione w :
  pliku o podanej nazwie (jesli plik nie istnieje (lub nie mozna go odczytac),
    nie nie dopisze (ale nie wyrzuci w jakikolwiek sposob bledu),
    puste linie w pliku (zlozone z samych bialych znakow) zostana wyeliminowane),
  podanej liscie katalogow (rozdzielonej PathSeparator'em)
  podanej zmiennej srodowiska (np. PATH czy LD_LIBRARY_PATH; jej wartosc
    zostanie potraktowana jako lista katalogow rozdzielona PathSeparator'em)
  Wszystkie katalogi sa juz na pewno zakonczone PathDelim.  }
procedure AddPathsFromFileLines(slist: TStrings; const fname: string);
procedure AddPathsFromEnvironmentVar(slist: TStrings; const varname: string);
procedure AddPathsFromPathList(slist: TStrings; const pathlist: string);

{ zwraca wszystkie stringi z slist sklejone separatorem. }
function StringsSeparated(slist: TStrings; const separator: string): string;

{ This is supposed to be TStringList that is case sensitive.

  TODO: In FPC >= 2.0.0 TStringList.CaseSensitive property
  was added. However, TStringList.CaseSensitive should always be left
  false (because of a bug, see
  [http://www.freepascal.org/bugs/showrec.php3?ID=4698]).
  So with FPC 2.0.0 and 2.0.2 we can't have CaseSensitive TStringList.

  I use this type to mark the places in my code that are vulnerable
  to this FPC bug. }
type
  {$ifdef FPC}
  TStringListCaseSens = TStringList;
  {$else}
  TStringListCaseSens = class(TStringList)
    constructor Create;
  end;
  {$endif}

{ Assuming that List is sorted, searches in log time for a Value in List.
  Returns -1 if not found.
  Very useful for simple dictionaries of words, where lookup must be fast. }
function SearchSortedList(List: TStrings; const Value: string): Integer;

{ Splits S by Splitter, and adds each splitted part to Strings.
  Splitting is done by Splitter, i.e. if N is the number of occurences
  of Splitter inside S, then it always adds N + 1 strings to Strings.
  Yes, this means that if S starts with Splitter then the first
  part is equal to ''. And if S ends with Splitter then the last
  oart is equal to ''. }
procedure Strings_AddSplittedString(Strings: TStrings;
  const S, Splitter: string);

{ Something like @link(SCamelotProgramHelpSuffix), but appends
  contents as a couple of lines to Strings. }
procedure Strings_AddCamelotProgramHelpSuffix(
  Strings: TStrings; const DisplayProgramName: string;
  const Version: string; WrapLines: boolean);

{ Use this instead of @code(SList.Text := S) to workaround FPC 2.0.2 bug.
  See [http://www.freepascal.org/bugs/showrec.php3?ID=4831] }
procedure Strings_SetText(SList: TStrings; const S: string);

{ ---------------------------------------------------------------------------- }
{ @section(TStream related) }

{ AppendStream : skopiuj cala zawartosc strumienia OwnedSourceStream do
  DestStream. Potem zrob OwnedSourceStream.Free (czyli OwnedSourceStream
  najlepiej przekazac w postaci freshly created stream, like this
  @code(AppendStream(DestStream, TMyStream.Create(...)))). }
procedure AppendStream(DestStream, OwnedSourceStream: TStream);

procedure StreamWriteLongWord(Stream: TStream; const Value: LongWord);
function StreamReadLongWord(Stream: TStream): LongWord;

procedure StreamWriteByte(Stream: TStream; const Value: Byte);
function StreamReadByte(Stream: TStream): Byte;

{ This simply writes Length(s) bytes starting from s[1].
  Versions with "ln" append nl, end of the line marker, to this.
  Versions without Stream param write to StdOutStream. }
procedure WriteStr(Stream: TStream; const S: string); overload;
procedure WritelnStr(Stream: TStream; const S: string); overload;
procedure WriteStr(const S: string); overload;
procedure WritelnStr(const S: string); overload;

{reads one char from stream using ReadBuffer (so EReadError will be raised
 if end of stream)}
function StreamReadChar(Stream: TStream): char;

function StreamReadZeroEndString(Stream: TStream): string;

{ StringReadUpto_NotEOS czyta strumien az do jakiegos znaku sposrod endingChars.
  Jezeli wczesniej napotka koniec strumienia - exception Stream Read Error.
  Zwrocony result nie zawiera endingChar. Wersja 2-argumentowa po prostu
  o nim "zapomina" - jezeli jej uzyjesz i backEndingChar = false, nie dowiesz sie
  jaki znak sposrod endingChars zatrzymal czytanie.
  Jezeli nie podasz backEndingChar to bedzie znaczylo ze nie ma go zwracac
  (tak samo jakbys podal false).

  NOTE: not every stream can back characters - it is implemented as
  Seek(-1, soFromCurrent) and some streams in FCL/VCL simply raise exception
  when we're doing this ! This is a problem with TStream class, not with our
  code.

  Use @link(TPeekCharStream) if you want to avoid this uncertainty,
  i.e. if you need functionality of StreamReadUpto_Xxx without
  using tricks with changing Position of the stream. }
function StreamReadUpto_NotEOS(Stream: TStream; const endingChars: TSetOfChars;
  backEndingChar: boolean; out endingChar: char): string; overload;
function StreamReadUpto_NotEOS(Stream: TStream; const endingChars: TSetOfChars;
  backEndingChar: boolean): string; overload;
function StreamReadUpto_NotEOS(Stream: TStream; const endingChars: TSetOfChars;
  { backEndingChar: boolean = false }
  out endingChar: char): string; overload;
function StreamReadUpto_NotEOS(Stream: TStream; const endingChars: TSetOfChars
  { backEndingChar: boolean = false }): string; overload;

{ StringReadUpto_EOS czyta az napotka znak sposrod endingChars lub
  koniec strumienia. Zwraca endingChar = -1 w tym przypadku,
  jezeli endingChar <> -1 to mozesz swobodnie wziac Chr(endingChar) i masz
  znak ktory zakonczyl czytanie. Podobnie jako _NotEOS, zwrocony
  result nie zawiera endingChar. }
function StreamReadUpto_EOS(Stream: TStream; const endingChars: TSetOfChars;
  backEndingChar: boolean; out endingChar: integer): string; overload;
function StreamReadUpto_EOS(Stream: TStream; const endingChars: TSetOfChars;
  backEndingChar: boolean): string; overload;
function StreamReadUpto_EOS(Stream: TStream; const endingChars: TSetOfChars;
  { backEndingChar: boolean = false }
  out endingChar: integer): string; overload;
function StreamReadUpto_EOS(Stream: TStream; const endingChars: TSetOfChars
  { backEndingChar: boolean = false }): string; overload;

{ _NotEOS wyrzuca blad EStreamReadError jezeli bedziemy stac na koncu
  stringa. Natomiast _EOS zwroci -1. Robia Peek czytajac znak a potem
  wracajac Seek(-1, soFromCurrent) a wiec aby dzialac wymagaja strumienia
  TStream na ktorym takie Seek jest dozwolone. }
function StreamPeekChar_NotEOS(Stream: TStream): char;
function StreamPeekChar_EOS(Stream: TStream): integer;

{ odczytaj plik ze strumienia CreateReadFileStream.
  CreateReadFileStream gwarantuje ze po otrzymanyn strumieniu mozna swobodnie
  skakac Seek'iem (czy ustawiajac Position:=) i gwarantuje ze odczyt ze
  strumienia bedzie buforowany (wiec mozna bez strachu o szybkosc
  odczytywac strumien malymi porcyjkami).

  Tradycyjny strumien Delphi/FPC z modulu Classes TFileStream.Create(filename,
  fmOpenRead); NIE robi buforowania - jego operacje przenosza sie 1:1 na
  wywolania systemu operacyjnego. Pod Linuxem nawet prymitywne I/O
  (open/read/write) dziala szybciutko (wiem ze linux przechowuje ostatnio
  odczytywane dane w pamieci; przypuszcam ze plik jest po prostu zawsze
  odczytywany duzymi porcjami do pamieci i w zwiazku z tym nawet
  prymitywne IO ma jakby buforowanie), pod Windowsem proste operacje
  CreateFile/ReadFile/WriteFile (na ktore jest tlumaczony TFileStream)
  nie sa juz tak dobrze zrobione i uzywanie ponizszej proc. pod Windowsem
  z miejsca przyspiesza WIELE miejsc w programie i pozwala mi piszac
  kod czytajacy ze strumienia nie martwic sie tak bardzo o to zeby
  odczytywac ze strumienia duzymi porcjami. Ale oczywiscie najlepiej
  pod Linuxem tez uzywac ponizszej funkcji, ona bedzie zawsze "dostrojona"
  do aktualnego systemu operacyjnego i implementacji TFileStream. }
function CreateReadFileStream(const filename: string): TStream;

{ this procedure reads GrowingStream (i.e. a stream that we can only read
  sequentially, no seeks allowed, and Size of the Stream cannot be queried.)
  and writes (appends) everything to DestStream.

  It means that the only
  operation we do on GrowingStream is GrowingStream.Read and the only
  operation on DestStream is DestStream.WriteBuffer. So DestStream usually
  must be able to grow dynamically to accomodate any GrowingStream input size.

  This procedure ends when GrowingStream ends. If ResetDestStreamPosition
  then at the end we do DestStream.Position := 0 (since it is usually useful
  and it would be easy for you to forget about it). }
procedure ReadGrowingStream(GrowingStream, DestStream: TStream;
  ResetDestStreamPosition: boolean);

{ This is like ReadGrowingStream, but it returns read contents as a string. }
function ReadGrowingStreamToString(GrowingStream: TStream): string;

{ read and write string as Length(s) (4 bytes) + s contents (Length(s) bytes). }
procedure StreamWriteString(Stream: TStream; const s: string);
function StreamReadString(Stream: TStream): string;

{ Convert whole Stream to string.
  This changes Stream.Position to 0 and then reads Stream.Size bytes,
  so be sure that Stream supports these operations. }
function StreamToString(Stream: TStream): string;

type
  { This is TMemoryStream that at the end of construction
    loads it's contents from file AFileName,
    and (if not ReadOnly) at the beginning of destruction
    saves it's contents to file.

    This is useful if you want to read a file in a very random way
    and you don't want to depend on how efficient is the implementation
    of TFileStream or how efficient is the implementation of OS routines
    used by TFileStream implementation. Using this class you know that
    everything is in memory, so you can freely
    - seek to some random places in file, forward, backward etc.
    - read some place in a file many times (by reading, seeking backward,
      reading again etc.)
    ... and you don't have to worry about how efficient this will be.

    You shouldn't use LoadFromFile/SaveToFile methods (although this class
    is actually so simple that it won't break anything; but you should be
    aware of what you are doing, i.e. you can possibly break connection
    between FileName property and actual contents of stream). }
  TMemoryFileStream = class(TMemoryStream)
  private
    FFileName: string;
    FReadOnly: boolean;
    FileContentsLoaded: boolean;
  public
    constructor Create(const AFileName: string; AReadOnly: boolean);
    destructor Destroy; override;
    property FileName: string read FFileName;
  end;

  EStreamNotImplemented = class(Exception);
  EStreamNotImplementedWrite = class(EStreamNotImplemented);
  EStreamNotImplementedSeek = class(EStreamNotImplemented);
  EStreamNotImplementedSetSize = class(EStreamNotImplemented);

  { This is a purely sequential read-only stream.
    This means that calling Write, Seek (changing Position)
    or setting Size will always cause an exception with
    appropriate descendant class of @link(EStreamNotImplemented).

    Getting Size and Position is allowed. Getting Size is simply
    implemented by getting SourceStream.Size. Position works
    correctly, i.e. it always returns the number of characters
    *read* from underlying stream. This means that peeking (using PeekChar)
    never changes position, also using read buffer
    (see @link(TBufferedReadStream)) do not affect Position.
    Position returns you position in *this stream*, not your "real"
    position in underlying SourceStream.

    In exchange, you get the ability to use PeekChar routine:
    you can always peek ahead one char in the stream,
    without reading it (i.e. next time you will call Read or ReadBuffer
    you will still get that char). This way even SourceStream may
    be purely sequential read-only stream (if you're sure that
    your SourceStream allows always to set Position than you could
    simply use Position := Position - 1; instead of using this class...).

    Notes about Read method overriden by this class descendants:

      This tries to read next Count bytes from SourceStream, making sure
      that even character that you obtained by PeekChar will be returned
      here. In other words, this just implements Read method of TStream :)

      This may call SourceStream.Read, and any exceptions that may be
      raised in SourceStream.Read are propagated higher from this method.
  }
  TPeekCharStream = class(TStream)
  private
    FSourceStream: TStream;
    FOwnsSourceStream: boolean;
  protected
    { @returns(SourceStream.Size) }
    function GetSize: Int64; override;

    { All other versions of SetSize also call this.
      @raises(EStreamNotImplementedSetSize always) }
    procedure SetSize(NewSize: Longint); override;

    {$ifndef FPC}
    function GetPosition: Int64; virtual; abstract;
    {$endif}
  public
    { SetPosition and all other versions of Seek also call this.
      @raises(EStreamNotImplementedSeek always (well, actually with
         FPC 1.9.8 raises some other thing ("Seek not implemented"),
         but it's temporary)) }
    {$ifndef VER1_9_8}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    {$endif}

    { WriteBuffer also calls this.
      @raises(EStreamNotImplementedWrite always) }
    function Write(const Buffer; Count: Longint): Longint; override;

    { Underlying stream. }
    property SourceStream: TStream read FSourceStream;

    { If true then at the destruction it will free FSourceStream. }
    property OwnsSourceStream: boolean
      read FOwnsSourceStream write FOwnsSourceStream;

    { Peeks next char from the stream but doesn't treat it as "already read",
      i.e. the next call to Read or ReadBuffer will return this char.
      Subsequent calls to PeekChar (without any Read/ReadBuffer between)
      will always return the same value.

      Returns -1 if stream ended, otherwise returns Ord or given char.

      This may call SourceStream.Read, and any exceptions that may be
      raised in SourceStream.Read are propagated higher from this method. }
    function PeekChar: Integer; virtual; abstract;

    { This is somehow a shortcut for Read(c, 1), but it returns -1 if eof
      is reached. So it's consistent with PeekChar.
      Sometimes it's also more comfortable, and it's a little faster. }
    function ReadChar: Integer; virtual; abstract;

    { Whole PeekChar is not one of EndingChars (and it's not eof)
      it reads chars from stream and appends them to Result.
      This means that Result is guaranteed to not contain any char
      from EndingChars. }
    function ReadUpto(const EndingChars: TSetOfChars): string; virtual; abstract;

    {$ifndef FPC}
    property Position: Int64 read GetPosition;
    {$endif}

    constructor Create(ASourceStream: TStream; AOwnsSourceStream: boolean);
    destructor Destroy; override;
  end;

  { This is a simplest non-abstract implementation of abstract
    @link(TPeekCharStream) class. }
  TSimplePeekCharStream = class(TPeekCharStream)
  private
    PeekedChar: Integer;
    IsPeekedChar: boolean;
    FPosition: Int64;
  protected
    { See @link(TPeekCharStream) for description how this Position behaves. }
    function GetPosition: Int64; override;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function PeekChar: Integer; override;
    function ReadChar: Integer; override;
    function ReadUpto(const EndingChars: TSetOfChars): string; override;
  end;

const
  DefaultReadBufferSize = 1024 * 1024;

type
  { This implements abstract TPeekCharStream class,
    so this is purely sequential read-only stream that reads from
    underlying SourceStream and you can use PeekChar and ReadChar and
    ReadUpto routines.

    Additionally this stream promises that it will buffer incoming data
    from SourceStream. This means that reading from a SourceStream
    by a very small chunks (like e.g. byte-by-byte) does not hurt
    performance. And at the same time, reading a huge file does
    not hurt memory consumption (because buffer size may usually
    be much smaller than file size). }
  TBufferedReadStream = class(TPeekCharStream)
  private
    FPosition: Int64;

    { This is always non-nil allocated for BufferSize bytes. }
    Buffer: PByteArray;

    { This is a position of next unread char in Buffer,
      i.e. PeekChar simply returns Buffer[BufferPos]
      (unless BufferPos = BufferEnd, in which case buffer must be refilled). }
    BufferPos: LongWord;

    { Always BufferPos <= BufferEnd.
      Always Buffer[BufferPos..BufferEnd - 1] is the data that still must be
      returned by TBufferedReadStream.Read method. }
    BufferEnd: LongWord;

    FBufferSize: LongWord;

    { Sets Buffer contents, BufferEnd reading data from SourceStream.
      BufferPos is always resetted to 0 by this. }
    procedure FillBuffer;
  protected
    { See TPeekCharStream for description how this Position behaves. }
    function GetPosition: Int64; override;
  public
    { @noAutoLinkHere }
    function Read(var LocalBuffer; Count: Longint): Longint; override;
    function PeekChar: Integer; override;
    function ReadChar: Integer; override;
    function ReadUpto(const EndingChars: TSetOfChars): string; override;

    property BufferSize: LongWord read FBufferSize;

    constructor Create(ASourceStream: TStream; AOwnsSourceStream: boolean;
      ABufferSize: LongWord = DefaultReadBufferSize);
    destructor Destroy; override;
  end;

{ ---------------------------------------------------------------------------- }
{ @section(TCollection related) }

{$ifdef DELPHI}
procedure CollectionSetCount(Collection: TCollection; NewCount: integer);
{$endif}

{ ---------------------------------------------------------------------------- }
{ @section(TComponent related) }

{ If Component = nil then it will do
    Component := ComponentClass.Create(Owner); }
procedure CreateIfNeeded(var Component: TComponent;
  ComponentClass: TComponentClass; Owner: TComponent);

{ ---------------------------------------------------------------------------- }
{ @section(Variables to read/write standard input/output using TStream classes.
  Initialized and finalized in this unit.) }

var
  { Under Win32 when program is a GUI program then some
    of the variables below may be nil (although that may
    be <> nil, even for GUI program, e.g. if user has
    run our GUI program like
      @preformatted cat something | my_program
    ).

    Note that you can't simultaneously read from StdInStream
    and StdInReader (see comments at TTextReader class). }
  StdInStream, StdOutStream, StdErrStream :TStream;
  StdInReader: TTextReader;

implementation

uses
  {$ifdef UNIX}
    {$ifdef USE_LIBC} Libc {$else} BaseUnix, Unix {$endif}
  {$endif}
  {$ifdef WIN32} Windows {$endif}
  , StrUtils, KambiFilesUtils;

{ TTextReader ---------------------------------------------------------------- }

constructor TTextReader.CreateFromFileStream(const FileName: string);
begin
 Create(TFileStream.Create(FileName, fmOpenRead), true);
end;

constructor TTextReader.Create(AStream: TStream; AOwnsStream: boolean);
begin
 inherited Create;
 Stream := AStream;
 FOwnsStream := AOwnsStream;
 LastNewLineChar := #0;
end;

destructor TTextReader.Destroy;
begin
 if FOwnsStream then Stream.Free;
 inherited;
end;

function TTextReader.Readln: string;
const BUF_INC = 100;
var ReadCnt, i: integer;
begin
 i := 1;

 { Note that ReadBuf may contain data that we
   already read from stream at some time but did not returned it to
   user of this class
   (because we realized we have read too much). }

 repeat
  if i > Length(ReadBuf) then
  begin
   SetLength(ReadBuf, Length(ReadBuf) + BUF_INC);
   ReadCnt := Stream.Read(ReadBuf[Length(ReadBuf) - BUF_INC + 1], BUF_INC);
   SetLength(ReadBuf, Length(ReadBuf) - BUF_INC + ReadCnt);
   if ReadCnt = 0 then
   begin
    Result := ReadBuf;
    ReadBuf := '';
    Exit;
   end;
  end;

  if ((ReadBuf[i] = #10) and (LastNewLineChar = #13)) or
     ((ReadBuf[i] = #13) and (LastNewLineChar = #10)) then
  begin
   { We got 2nd newline character ? Ignore it. }
   Assert(i = 1);
   Delete(ReadBuf, 1, 1);
   LastNewLineChar := #0;
  end else
  if ReadBuf[i] in [#10, #13] then
  begin
   Result := Copy(ReadBuf, 1, i-1);
   LastNewLineChar := ReadBuf[i];
   Delete(ReadBuf, 1, i);
   Exit;
  end else
  begin
   LastNewLineChar := #0;
   Inc(i);
  end;
 until false;
end;

function TTextReader.Eof: boolean;
var ReadCnt: Integer;
begin
 if ReadBuf = '' then
 begin
  SetLength(ReadBuf, 1);
  ReadCnt := Stream.Read(ReadBuf[1], 1);
  SetLength(ReadBuf, ReadCnt);
 end;
 Result := ReadBuf = '';
end;

{ TIniFile replacement -------------------------------------------------------- }

procedure TKamIniFile.Clear;
var sects: TStringList;
    i: integer;
begin
 sects := TStringList.Create;
 try
  ReadSections(sects);
  for i := 0 to sects.count-1 do EraseSection(sects[i]);
 finally sects.free end;
end;

constructor TKamIniFile.Create(const AFileName: string; AUpdateOnDestroy: boolean);
begin
 inherited Create(AFileName);
 FUpdateOnDestroy := AUpdateOnDestroy;
end;

destructor TKamIniFile.Destroy;
begin
 if UpdateOnDestroy then UpdateFile;
 inherited;
end;

{ TObjectsList_Abstract ---------------------------------------------------- }

procedure TObjectsList_Abstract.FreeWithContents;
begin
 if Self <> nil then
 begin
  FreeContents;
  {pod FPC samo Destroy (nie wywolane przez Self) nie zwolni pamieci ?!?}
  Self.Destroy;
 end;
end;

procedure FreeWithContentsAndNil(var Obj);
begin
  TObjectsList_Abstract(Obj).FreeWithContents;
  TObjectsList_Abstract(Obj) := nil;
end;

{ TStrings helpers ------------------------------------------------------- }

procedure StringsAdd(Strs: TStrings; Count: integer; itemVal: string);
var i: integer;
begin
 for i := 1 to Count do Strs.Add(itemVal);
end;

procedure AddStrArrayToStrings(const StrArr: array of string; strlist: TStrings);
var i: integer;
begin
 for i := 0 to High(StrArr) do strlist.Append(StrArr[i]);
end;

procedure AddPathsFromPathList(slist: TStrings; const pathlist: string);
var spoz: integer;
    path: string;
begin
 spoz := 1;
 repeat
  path := NextToken(pathlist, spoz, [PathSep]);
  if path = '' then
   break else
   slist.Append(InclPathDelim(path));
 until false;
end;

procedure AddPathsFromEnvironmentVar(slist: TStrings; const varname: string);
begin
 AddPathsFromPathList(slist, SysUtils.GetEnvironmentVariable(varname));
end;

procedure AddPathsFromFileLines(slist: TStrings; const fname: string);
var f: TextFile;
    path: string;
begin
 try
  SafeReset(f, fname, true);
 except
  {if file can't be opened - exit, without error}
  on EFileOpenError do exit;
 end;
 while not Eof(f) do
 begin
  Readln(f, path);
  if Trim(path) <> '' then slist.Append(InclPathDelim(path));
 end;
end;

procedure Load_Strings(IniFile: TIniFile; const section: string; Strs: TStrings);
var i: integer;
begin
 for i := 0 to IniFile.ReadInteger(section, 'Count', 0)-1 do
  Strs.Append(IniFile.ReadString(section, 'Item'+IntToStr(i), ''));
end;

procedure Save_Strings(IniFile: TIniFile; const section: string; Strs: TStrings);
var i: integer;
begin
 IniFile.WriteInteger(section, 'Count', Strs.Count);
 for i := 0 to Strs.Count-1 do
  IniFile.WriteString(section, 'Item'+IntToStr(i), Strs[i]);
end;

function StringsSeparated(slist: TStrings; const separator: string): string;
var i: integer;
begin
 result := '';
 if slist.count > 0 then
 begin
  for i := 0 to slist.count-2 do
   result := result +slist[i] +separator;
  result := result +slist[slist.count-1];
 end;
end;

{$ifndef FPC}
constructor TStringListCaseSens.Create;
begin
 inherited;
 CaseSensitive := true;
end;
{$endif}

function SearchSortedList(List: TStrings; const Value: string): Integer;
var A, B, AB: Integer;
begin
 A := 0;
 B := List.Count - 1;
 while A < B do
 begin
  AB := (A + B) div 2;

  { AB may be equal to A (consider B = A + 1 then AB = (2 * A + 1) div 2 = A).
    AB must be < B (because A + B < 2 * B (because A < B) so (A + B) div 2 < B).
    So we must take care below to never do A := AB in some case,
    always A := AB + 1, otherwise we get a loop. }

  if Value <= List[AB] then
   B := AB else
   A := AB + 1;
 end;
 if (A = B) and (Value = List[A]) then
  Result := A else
  Result := -1;
end;

procedure Strings_AddSplittedString(Strings: TStrings;
  const S, Splitter: string);
var
  SplitterPos, Done: Integer;
begin
  Done := 0;
  SplitterPos := Pos(Splitter, S);
  while SplitterPos <> 0 do
  begin
    Strings.Append(CopyPos(S, Done + 1, SplitterPos - 1));
    Done := SplitterPos + Length(Splitter) - 1;
    SplitterPos := PosEx(Splitter, S, Done + 1);
  end;
  Strings.Append(SEnding(S, Done + 1));
end;

procedure Strings_AddCamelotProgramHelpSuffix(
  Strings: TStrings; const DisplayProgramName: string;
  const Version: string; WrapLines: boolean);
begin
  Strings_AddSplittedString(Strings,
    SCamelotProgramHelpSuffix(DisplayProgramName, Version, WrapLines), nl);
end;

procedure Strings_SetText(SList: TStrings; const S: string);
begin
  if Length(S) = 1 then
    SList.Text := S + LineEnding else
    SList.Text := S;
end;

{ TStream helpers -------------------------------------------------------- }

procedure AppendStream(DestStream, OwnedSourceStream: TStream);
begin
 try
  DestStream.CopyFrom(OwnedSourceStream, 0);
 finally OwnedSourceStream.Free end;
end;

procedure StreamWriteLongWord(Stream: TStream; const Value: LongWord);
begin
  Stream.WriteBuffer(Value, SizeOf(Value));
end;

function StreamReadLongWord(Stream: TStream): LongWord;
begin
  Stream.ReadBuffer(Result, SizeOf(Result));
end;

procedure StreamWriteByte(Stream: TStream; const Value: Byte);
begin
  Stream.WriteBuffer(Value, SizeOf(Value));
end;

function StreamReadByte(Stream: TStream): Byte;
begin
  Stream.ReadBuffer(Result, SizeOf(Result));
end;

procedure WriteStr(Stream: TStream; const S: string);
begin
 Stream.WriteBuffer(Pointer(S)^, Length(S));
end;

procedure WritelnStr(Stream: TStream; const S: string);
begin
 WriteStr(Stream, S);
 WriteStr(Stream, nl);
end;

procedure WriteStr(const S: string);
begin
 WriteStr(StdOutStream, S);
end;

procedure WritelnStr(const S: string);
begin
 WritelnStr(StdOutStream, S);
end;

function StreamReadChar(Stream: TStream): char;
begin
 Stream.ReadBuffer(result, SizeOf(result));
end;

function StreamReadZeroEndString(Stream: TStream): string;
begin
 result := StreamReadUpto_NotEOS(Stream, [#0], false);
end;

function StreamReadUpto_NotEOS(Stream: TStream; const endingChars: TSetOfChars;
  backEndingChar: boolean; out endingChar: char): string; overload;
var readLen: integer; { ile znakow odczytales }
    ch: char;
begin
 readLen := 0;
 result := '';
 repeat
  Stream.ReadBuffer(ch, 1);
  if ch in endingChars then
  begin
   endingChar := ch;
   break;
  end;

  {zwiekszamy Length(result) o duze bloki zeby nie marnowac czasu
   na wiele malych realokacji pamieci}
  Inc(readLen);
  if readLen > Length(result) then SetLength(result, readLen + 100);
  result[readLen] := ch;
 until false;

 if backEndingChar then Stream.Seek(-1, soFromCurrent);

 SetLength(result, readLen);
end;

function StreamReadUpto_NotEOS(Stream: TStream; const endingChars: TSetOfChars;
  backEndingChar: boolean): string; overload;
var dummy: char;
begin
 result := StreamReadUpto_NotEOS(Stream, endingChars, backEndingChar, dummy);
end;

function StreamReadUpto_NotEOS(Stream: TStream; const endingChars: TSetOfChars;
  out endingChar: char): string;
begin
 result := StreamReadUpto_NotEOS(Stream, endingChars, false, endingChar);
end;

function StreamReadUpto_NotEOS(Stream: TStream; const endingChars: TSetOfChars): string;
begin
 result := StreamReadUpto_NotEOS(Stream, endingChars, false);
end;

function StreamReadUpto_EOS(Stream: TStream; const endingChars: TSetOfChars;
  backEndingChar: boolean; out endingChar: integer): string; overload;
var readLen: integer; { ile znakow odczytales }
    ch: char;
begin
 readLen := 0;
 result := '';
 repeat
  if Stream.Read(ch, 1) = 0 then
  begin
   endingChar := -1;
   break;
  end else
  if ch in endingChars then
  begin
   endingChar := Ord(ch);
   break;
  end;

  {zwiekszamy Length(result) o duze bloki zeby nie marnowac czasu
   na wiele malych realokacji pamieci}
  Inc(readLen);
  if readLen > Length(result) then SetLength(result, readLen+100);
  result[readLen] := ch;
 until false;

 if backEndingChar then
  if endingChar <> -1 then Stream.Seek(-1, soFromCurrent);

 SetLength(result, readLen);
end;

function StreamReadUpto_EOS(Stream: TStream; const endingChars: TSetOfChars;
  backEndingChar: boolean): string; overload;
var dummy: integer;
begin
 result := StreamReadUpto_EOS(Stream, endingChars, backEndingChar, dummy);
end;

function StreamReadUpto_EOS(Stream: TStream; const endingChars: TSetOfChars;
  out endingChar: integer): string;
begin
 result := StreamReadUpto_EOS(Stream, endingChars, false, endingChar);
end;

function StreamReadUpto_EOS(Stream: TStream; const endingChars: TSetOfChars): string;
begin
 result := StreamReadUpto_EOS(Stream, endingChars, false);
end;

function StreamPeekChar_NotEOS(Stream: TStream): char;
begin
 Stream.ReadBuffer(result, 1);
 Stream.Seek(-1, soFromCurrent);
end;

function StreamPeekChar_EOS(Stream: TStream): integer;
var ch: char;
begin
 if Stream.Read(ch, 1) = 1 then
  begin Stream.Seek(-1, soFromCurrent); result := Ord(ch) end else
  result := -1;
end;

function CreateReadFileStream(const filename: string): TStream;
begin
 {NAIWNA implementacja : result := TFileStream.Create(filename, fmOpenRead) }
 result := TMemoryStream.Create;
 try
  TMemoryStream(result).LoadFromFile(filename);
  result.Position := 0;
 except FreeAndNil(result); raise end;
end;

procedure ReadGrowingStream(GrowingStream, DestStream: TStream;
  ResetDestStreamPosition: boolean);
var ReadCount: Integer;
    Buffer: array[1..10000]of Byte;
begin
 repeat
  ReadCount := GrowingStream.Read(Buffer, SizeOf(Buffer));
  if ReadCount = 0 then Break;
  DestStream.WriteBuffer(Buffer, ReadCount);
 until false;
 if ResetDestStreamPosition then DestStream.Position := 0;
end;

function ReadGrowingStreamToString(GrowingStream: TStream): string;
const
  BufferSize = 10000;
var
  ReadCount: Integer;
  Buffer: string;
begin
  SetLength(Buffer, BufferSize);
  Result := '';
  repeat
    ReadCount := GrowingStream.Read(Buffer[1], Length(Buffer));
    if ReadCount = 0 then Break;
    Result := Result + Copy(Buffer, 1, ReadCount);
  until false;
end;

procedure StreamWriteString(Stream: TStream; const s: string);
var L: Integer;
begin
 L := Length(s);
 Stream.WriteBuffer(L, SizeOf(L));
 { check L > 0 to avoid range check error on S[1] }
 if L > 0 then Stream.WriteBuffer(S[1], L);
end;

function StreamReadString(Stream: TStream): string;
var L: Integer;
begin
 Stream.ReadBuffer(L, SizeOf(L));
 SetLength(Result, L);
 { check L > 0 to avoid range check error on Result[1] }
 if L > 0 then Stream.ReadBuffer(Result[1], L);
end;

function StreamToString(Stream: TStream): string;
begin
  SetLength(Result, Stream.Size);
  Stream.Position := 0;
  Stream.ReadBuffer(Pointer(Result)^, Length(Result));
end;

{ TMemoryFileStream ------------------------------------------------------- }

constructor TMemoryFileStream.Create(const AFileName: string; AReadOnly: boolean);
begin
 inherited Create;
 FFileName := AFileName;
 FReadOnly := AReadOnly;
 LoadFromFile(AFileName);
 FileContentsLoaded := true;
end;

destructor TMemoryFileStream.Destroy;
begin
 { I'm checking FileContentsLoaded here, to prevent situation
   where constructor failed (in inherited TMemoryStream constructor)
   with exception -- this causes destructor to be called, but of couse
   I should NOT in this situation save my contents to file
   (as this would erase contents of innocent file). }
 if FileContentsLoaded and (not FReadOnly) then
  SaveToFile(FileName);
end;

{ TPeekCharStream -------------------------------------------------- }

function TPeekCharStream.GetSize: Int64;
begin
 Result := SourceStream.Size;
end;

procedure TPeekCharStream.SetSize(NewSize: Longint);
begin
 raise EStreamNotImplementedSetSize.Create(
   'TPeekCharStream.SetSize not supported');
end;

{$ifndef VER1_9_8}
function TPeekCharStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
 raise EStreamNotImplementedSeek.Create('TPeekCharStream.Seek not supported');
end;
{$endif}

function TPeekCharStream.Write(const Buffer; Count: Longint): Longint;
begin
 raise EStreamNotImplementedWrite.Create('TPeekCharStream.Write not supported');
end;

constructor TPeekCharStream.Create(ASourceStream: TStream;
  AOwnsSourceStream: boolean);
begin
 inherited Create;
 FOwnsSourceStream := AOwnsSourceStream;
 FSourceStream := ASourceStream;
end;

destructor TPeekCharStream.Destroy;
begin
 if OwnsSourceStream then FreeAndNil(FSourceStream);
 inherited;
end;

{ TSimplePeekCharStream --------------------------------------------------- }

function TSimplePeekCharStream.GetPosition: Int64;
begin
 Result := FPosition;
end;

function TSimplePeekCharStream.Read(var Buffer; Count: Longint): Longint;
begin
 if (Count <= 0) or
    (IsPeekedChar and (PeekedChar = -1)) then
  Result := 0 else
 if IsPeekedChar then
 begin
  PChar(@Buffer)[0] := Chr(PeekedChar);
  Result := 1 + SourceStream.Read(PChar(@Buffer)[1], Count - 1);
  { Note that if SourceStream.Read will raise an exception,
    we will still have IsPeekedChar = true. }
  IsPeekedChar := false;
 end else
  Result := SourceStream.Read(Buffer, Count);

 FPosition := FPosition + Result;
end;

function TSimplePeekCharStream.PeekChar: Integer;
begin
 if not IsPeekedChar then
 begin
  if SourceStream.Read(PeekedChar, 1) = 0 then
   PeekedChar := -1;
  IsPeekedChar := true;
 end;
 Result := PeekedChar;
end;

function TSimplePeekCharStream.ReadChar: Integer;
{ This is somehow optimized version of TSimplePeekCharStream.Read
  for the case when Count = 1. }
var C: char;
begin
 if IsPeekedChar then
 begin
  Result := PeekedChar;
  IsPeekedChar := false;
  Inc(FPosition);
 end else
 begin
  if SourceStream.Read(C, 1) = 0 then
   Result := -1 else
  begin
   Result := Ord(C);
   Inc(FPosition);
  end;
 end;
end;

function TSimplePeekCharStream.ReadUpto(const EndingChars: TSetOfChars): string;
var Peeked: Integer;
begin
 Result := '';
 while true do
 begin
  Peeked := PeekChar;
  if (Peeked = -1) or (Chr(Peeked) in EndingChars) then
   Exit;
  Result := Result + Chr(Peeked);
  { I could call above "Result := Result + Chr(ReadChar);"
    to make implementation of ReadUpto cleaner (not dealing
    with private fields of TStreamPeekChar).
    But doing like I'm doing now works a little faster. }
  IsPeekedChar := false;
  Inc(FPosition);
 end;
end;

{ TBufferedReadStream ----------------------------------------------------- }

function TBufferedReadStream.GetPosition: Int64;
begin
 Result := FPosition;
end;

procedure TBufferedReadStream.FillBuffer;
begin
 BufferEnd := SourceStream.Read(Buffer[0], BufferSize);
 BufferPos := 0;
end;

function TBufferedReadStream.Read(var LocalBuffer; Count: Longint): Longint;
var CopyCount: LongWord;
begin
 if Count < 0 then
  Result := 0 else
 if LongWord(Count) <= BufferEnd - BufferPos then
 begin
  { In this case we can fill LocalBuffer using only data from Buffer }
  Move(Buffer[BufferPos], LocalBuffer, Count);
  BufferPos := BufferPos + LongWord(Count);
  Result := Count;
 end else
 begin
  Move(Buffer[BufferPos], LocalBuffer, BufferEnd - BufferPos);
  Result := BufferEnd - BufferPos;
  BufferPos := BufferEnd;
  Count := Count - Result;

  { Now we must read remaining Count bytes from SourceStream.
    If Count < BufferSize I must use Buffer (after all this is the purpose
    of TBufferedReadStream, to guarantee buffered reading).
    On the other hand, if Count >= BufferSize I can read it directly
    from SourceStream, no need to use Buffer in this case. }
  if LongWord(Count) < BufferSize then
  begin
   FillBuffer;
   CopyCount := Min(Count, BufferEnd - BufferPos);
   Move(Buffer[0], PChar(@LocalBuffer)[Result], CopyCount);
   BufferPos := BufferPos + CopyCount;
   Result := Result + LongInt(CopyCount);
  end else
  begin
   Result := Result + SourceStream.Read(PChar(@LocalBuffer)[Result], Count);
  end;
 end;

 FPosition := FPosition + Result;
end;

function TBufferedReadStream.PeekChar: Integer;
begin
 if BufferPos < BufferEnd then
  Result := Buffer^[BufferPos] else
 begin
  FillBuffer;
  if BufferPos < BufferEnd then
   Result := Buffer^[BufferPos] else
   Result := -1;
 end;
end;

function TBufferedReadStream.ReadChar: Integer;
begin
 if BufferPos < BufferEnd then
 begin
  Result := Buffer^[BufferPos];
  Inc(BufferPos);
  Inc(FPosition);
 end else
 begin
  FillBuffer;
  if BufferPos < BufferEnd then
  begin
   Result := Buffer^[BufferPos];
   Inc(BufferPos);
   Inc(FPosition);
  end else
   Result := -1;
 end;
end;

function TBufferedReadStream.ReadUpto(const EndingChars: TSetOfChars): string;
var Peeked: Integer;
begin
 Result := '';
 while true do
 begin
  Peeked := PeekChar;
  if (Peeked = -1) or (Chr(Peeked) in EndingChars) then
   Exit;
  Result := Result + Chr(Peeked);
  { I could call above "Result := Result + Chr(ReadChar);"
    to make implementation of ReadUpto cleaner (not dealing
    with private fields of TStreamPeekChar).
    But doing like I'm doing now works a little faster. }
  Inc(BufferPos);
  Inc(FPosition);
 end;
end;

constructor TBufferedReadStream.Create(ASourceStream: TStream;
  AOwnsSourceStream: boolean; ABufferSize: LongWord);
begin
 inherited Create(ASourceStream, AOwnsSourceStream);

 FBufferSize := ABufferSize;
 Buffer := GetMem(BufferSize);
 BufferPos := 0;
 BufferEnd := 0;
end;

destructor TBufferedReadStream.Destroy;
begin
 FreeMemNiling(Pointer(Buffer));
 inherited;
end;

{ TCollection helpers -------------------------------------------------- }

{$ifdef DELPHI}
procedure CollectionSetCount(Collection: TCollection; NewCount: integer);
begin
 while NewCount < Collection.Count do Collection.Delete(Collection.Count-1);
 while NewCount > Collection.Count do Collection.Add;
end;
{$endif}

{ TComponent helpers --------------------------------------------------- }

procedure CreateIfNeeded(var Component: TComponent;
  ComponentClass: TComponentClass; Owner: TComponent);
begin
 if Component = nil then
  Component := ComponentClass.Create(Owner);
end;

{ init / fini --------------------------------------------------------------- }

{TODO: use StdInpu/Output/ErrortHandle also under Win32, simplify this}

procedure InitStdStreams;

  procedure InitStdStream(var Stream: TStream;
    {$ifdef WIN32}nStdHandle: DWord{$endif}
    {$ifdef UNIX}Handle: THandle{$endif});
  {$ifdef WIN32}var Handle: THandle;{$endif}
  begin
   {$ifdef UNIX}
   Stream := THandleStream.Create(Handle);
   {$else}
   Handle := GetStdHandle(nStdHandle);
   if Handle <> INVALID_HANDLE_VALUE then
    Stream := THandleStream.Create(Handle) else
    Stream := nil;
   {$endif}
  end;

begin
 InitStdStream(StdInStream,  {$ifdef WIN32} STD_INPUT_HANDLE  {$else} StdInputHandle  {$endif});
 InitStdStream(StdOutStream, {$ifdef WIN32} STD_OUTPUT_HANDLE {$else} StdOutputHandle {$endif});
 InitStdStream(StdErrStream, {$ifdef WIN32} STD_ERROR_HANDLE  {$else} StdErrorHandle  {$endif});
 if StdInStream <> nil then
  StdInReader := TTextReader.Create(StdInStream, false) else
  StdInReader := nil;
end;

procedure FiniStdStreams;
begin
 FreeAndNil(StdInStream);
 FreeAndNil(StdOutStream);
 FreeAndNil(StdErrStream);
 FreeAndNil(StdInReader);
end;

initialization
 InitStdStreams;
finalization
 FiniStdStreams;
end.
