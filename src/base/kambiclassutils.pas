{
  Copyright 2000-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Basic classes and class utilities (for streams, strings, lists and such).
  Many utilities for classes that are defined in the Classes unit,
  and some classes of our my own.

  Some notes about TStream descendants :
  @unorderedList(
    @item(
      I call a stream "purely sequential" (or just "sequential")
      if it allows only reading and/or writing of data
      and does not allow free "Seek" calls,
      in particular --- it does not allow Seek to move back in a stream.)

    @item(
      I call a stream "growing" if it's read-only and it's purely sequential
      and it's Size property may be useless. In other words, when you read
      a "growing" stream, you don't know when it ends, until you reach the end.
      You just have to read data until Read returns 0.)

    @item(Remember that to reliably detect end of the stream when you
      use TStream.Read, you have to test is ReadCount = 0.
      Merely testing that ReadCount < less than you requested is not enough,
      e.g. seen for THandleStream when handle is StdIn.)
  )
}

unit KambiClassUtils;

{$I kambiconf.inc}
{$ifdef DELPHI} {$warn SYMBOL_PLATFORM OFF} {$endif}

interface

uses Classes, SysUtils, KambiUtils, KambiStringUtils, Contnrs,
  FGL {$ifdef VER2_2}, FGLObjectList22 {$endif};

{$define read_interface}

{ ---------------------------------------------------------------------------- }
{ @section(Text reading) }

type
  { Read given Stream line by line.
    Lines may be terminated in the Stream with #13, #10, #13+#10 or #10+#13.
    This way I can treat any TStream quite like standard Pascal text files:
    I have simple Readln method.

    After calling Readln or Eof you should stop directly using underlying
    Stream (but you can use Stream right after creating
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
    { Open a file in read-only mode. }
    constructor CreateFromFileStream(const FileName: string);

    { Open a stream. If AOwnsStream then in destructor we will free
      given Stream object. }
    constructor Create(AStream: TStream; AOwnsStream: boolean);
    destructor Destroy; override;

    { Read next line from Stream. Returned string does not contain
      any end-of-line characters. }
    function Readln: string;

    function Eof: boolean;
  end;

{ ---------------------------------------------------------------------------- }
{ @section(TStrings utilities) }

{ Add some strings. }
procedure StringsAdd(Strs: TStrings; Count: integer; itemVal: string='dummy'); overload;

{ Add all strings from string array to TStrings instance. }
procedure AddStrArrayToStrings(const StrArr: array of string; strlist: TStrings);

{ Append to TStrings some directiories.

  @unorderedList(
    @item(AddPathsFromFileLines reads directiories list from given file.
      If the file doesn't exist or is not readable, will append nothing,
      without raising any error. Empty lines (only whitespace)
      in file are ignored.)

    @item(AddPathsFromPathList reads directiories from a list
      separated by PathSeparator.)

    @item(AddPathsFromEnvironmentVar reads directiories from a list
      in the environment variable, also separated by PathSeparator.
      This is suitable for parsing environment variables like
      $PATH or $LD_LIBRARY_PATH.)
  )

  All appended directories are guaranteed to end with PathDelim.

  @groupBegin }
procedure AddPathsFromFileLines(slist: TStrings; const fname: string);
procedure AddPathsFromEnvironmentVar(slist: TStrings; const varname: string);
procedure AddPathsFromPathList(slist: TStrings; const pathlist: string);
{ @groupEnd }

type
  { TStringList that is case sensitive. }
  TStringListCaseSens = class(TStringList)
    constructor Create;
    property CaseSensitive default true;
  end;

{ Splits S by Splitter, and adds each splitted part to Strings.
  Splitting is done by Splitter, i.e. if N is the number of occurrences
  of Splitter inside S, then it always adds N + 1 strings to Strings.
  Yes, this means that if S starts with Splitter then the first
  part is equal to ''. And if S ends with Splitter then the last
  oart is equal to ''. }
procedure Strings_AddSplittedString(Strings: TStrings;
  const S, Splitter: string);

{ Something like @link(SVrmlEngineProgramHelpSuffix), but appends
  contents as a couple of lines to Strings. }
procedure Strings_AddVrmlEngineProgramHelpSuffix(
  Strings: TStrings; const DisplayProgramName: string;
  const Version: string; WrapLines: boolean);

{ Use this instead of @code(SList.Text := S) to workaround FPC 2.0.2 bug.
  See [http://www.freepascal.org/mantis/view.php?id=6699] }
procedure Strings_SetText(SList: TStrings; const S: string);

{ Make sure we don't have more than MaxCount strings on a list.
  Removes the last strings if necessary. }
procedure Strings_Trim(Strings: TStrings; MaxCount: Cardinal);

{ ---------------------------------------------------------------------------- }
{ @section(TStream utilities) }

{ }
procedure StreamWriteLongWord(Stream: TStream; const Value: LongWord);
function StreamReadLongWord(Stream: TStream): LongWord;

procedure StreamWriteByte(Stream: TStream; const Value: Byte);
function StreamReadByte(Stream: TStream): Byte;

{ Write string contents to stream.
  This isn't a procedure to encode a string within a binary stream,
  this only writes string contents (Length(S) bytes) into the stream.
  Versions with "ln" append newline.
  Versions without Stream parameter write to StdOutStream.
  @groupBegin }
procedure WriteStr(Stream: TStream; const S: string); overload;
procedure WritelnStr(Stream: TStream; const S: string); overload;
procedure WriteStr(const S: string); overload;
procedure WritelnStr(const S: string); overload;
{ @groupEnd }

{ Read one character from stream.
  @raises EReadError If end of stream. }
function StreamReadChar(Stream: TStream): char;

function StreamReadZeroEndString(Stream: TStream): string;

{ Read stream, until you find some character in EndingChars.
  Returns read contents, without final character (the one in EndingChars set).

  If you use a version with BackEndingChar parameter and pass
  BackEndingChar = @true, then the ending character will be returned back to
  stream (we will start reading from it next time).
  Note that "returning the character" is done by Seek(-1, soFromCurrent),
  which may not be possible on some streams. Wrap a stream
  in TPeekCharStream instead, and use TPeekCharStream.ReadUpto,
  to be able to "return back" a character reliably.

  Independently from BackEndingChar, if you use a version with EndingChar
  parameter, it will be set to the ending character.

  @raises EReadError If the stream will end before encountering one of EndingChars.
  @groupBegin }
function StreamReadUpto_NotEOS(Stream: TStream; const endingChars: TSetOfChars;
  backEndingChar: boolean; out endingChar: char): string; overload;
function StreamReadUpto_NotEOS(Stream: TStream; const endingChars: TSetOfChars;
  backEndingChar: boolean): string; overload;
function StreamReadUpto_NotEOS(Stream: TStream; const endingChars: TSetOfChars;
  out endingChar: char): string; overload;
function StreamReadUpto_NotEOS(Stream: TStream; const endingChars: TSetOfChars): string; overload;
{ @groupEnd }

{ Read stream, until you find some character in EndingChars, or end of stream.

  Compared to StreamReadUpto_NotEOS, this treats "end of stream"
  as a normal situation, and doesn't raise any exception on it.
  It sets EndingChar to -1 on end of stream. When EndingChar is not -1,
  you know you can safely cast it to normal 8-bit character.

  Everything else works like with StreamReadUpto_NotEOS.
  @groupBegin }
function StreamReadUpto_EOS(Stream: TStream; const endingChars: TSetOfChars;
  backEndingChar: boolean; out endingChar: integer): string; overload;
function StreamReadUpto_EOS(Stream: TStream; const endingChars: TSetOfChars;
  backEndingChar: boolean): string; overload;
function StreamReadUpto_EOS(Stream: TStream; const endingChars: TSetOfChars;
  out endingChar: integer): string; overload;
function StreamReadUpto_EOS(Stream: TStream; const endingChars: TSetOfChars): string; overload;
{ @groupEnd }

{ Open a proper stream to read a file, fast (with buffering) and with seeking.
  This gives you a stream most comfortable for reading (buffering means
  that you can read small, comfortable pieces of it; seeking means
  you can jump freely to various file positions, back and forward).

  On different OSes or even compilers this may require a little different
  stream, so it's safest to just use this function. For example,
  traditional Classes.TFileStream doesn't do buffering. Although under Linux,
  the buffering of file handles is done at kernel level (so everything
  works fast), on Windows the slowdown is noticeable.
  This function will always create
  proper stream descendant, eventually wrapping some standard stream
  in a buffered stream with full seeking capability. }
function CreateReadFileStream(const filename: string): TStream;

{ Read a growing stream, and append it to another destination stream.
  A "growing stream" is a stream that we can only read
  sequentially, no seeks allowed, and size is unknown until we hit the end.

  The only operation we do on GrowingStream is GrowingStream.Read and the only
  operation on DestStream is DestStream.WriteBuffer. So DestStream usually
  must be able to grow dynamically to accomodate any GrowingStream input size.

  This procedure ends when GrowingStream ends. If ResetDestStreamPosition
  then at the end we do DestStream.Position := 0 (since it is usually useful
  and it would be easy for you to forget about it). }
procedure ReadGrowingStream(GrowingStream, DestStream: TStream;
  ResetDestStreamPosition: boolean);

{ Read a growing stream, and returns it's contents as a string.
  A "growing stream" is a stream that we can only read
  sequentially, no seeks allowed, and size is unknown until we hit the end. }
function ReadGrowingStreamToString(GrowingStream: TStream): string;

{ Encode / decode a string in a binary stream. Records string length (4 bytes),
  then the string contents (Length(S) bytes).
  @groupBegin }
procedure StreamWriteString(Stream: TStream; const s: string);
function StreamReadString(Stream: TStream): string;
{ @groupEnd }

{ Convert whole Stream to a string.
  Changes Stream.Position to 0 and then reads Stream.Size bytes,
  so be sure that Stream.Size is usable. }
function StreamToString(Stream: TStream): string;

procedure StreamSaveToFile(Stream: TStream; const FileName: string);

type
  { Simple file mapped into the memory. This is a TMemoryStream descendant
    that at construction loads it's contents from file,
    and (if not ReadOnly) at the destruction saves it's contents into
    the same file.

    This allows for full stream capabilities, very fast seeking in all
    direction, you can seek and read freely, as the whole thing is buffered
    in memory. However, it wastes a lot of memory --- don't use this for large
    files.

    You shouldn't use LoadFromFile/SaveToFile methods. Although this class
    is actually so simple that it won't break anything. But you should be
    aware of what you are doing, i.e. you can possibly break connection
    between FileName property and actual contents of the stream. }
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

  { Abstract class to read another stream, always being able to back one character.
    This is a purely sequential read-only stream.
    This means that calling @link(Write), @link(Seek) (changing Position)
    or setting @code(Size) will always cause an exception with
    appropriate descendant of @link(EStreamNotImplemented).

    Getting @code(Size) and @code(Position) is allowed.
    Getting @code(Size) is simply
    implemented by getting SourceStream.Size (so it works if the underlying
    source stream supports getting Size). Getting @code(Position) always works
    correctly, as it's just the number of characters
    @italic(read) from this stream. The fact that we may
    read ahead in the underlying source stream (for buffering, for peeking)
    is completely hidden.

    We do not assume anything about the underlying source stream, in particular
    the underlying source stream may also be purely sequential
    (so we can only read from it, and we cannot predict when it ends).

    The main advantage of using this class is that you get PeekChar routine:
    you can always peek ahead one character in the stream,
    without reading it (i.e. next time you will call Read or ReadBuffer
    you will still get that char). And it works for all source streams,
    including the ones where seeking is not allowed (so Seek(-1, soCurrent)
    would not work). }
  TPeekCharStream = class(TStream)
  private
    FSourceStream: TStream;
    FOwnsSourceStream: boolean;
  protected
    { @returns(SourceStream.Size). }
    function GetSize: Int64; override;

    { This stream doesn't support setting size.
      (All other versions of SetSize also call this.)
      @raises(EStreamNotImplementedSetSize Always.) }
    procedure SetSize(NewSize: Longint); override;

    {$ifndef FPC}
    function GetPosition: Int64; virtual; abstract;
    {$endif}
  public
    { This stream doesn't support seeking.
      (SetPosition and all other versions of Seek also call this.)
      @raises(EStreamNotImplementedSeek Always.) }
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;

    { This stream doesn't support writing.
      (WriteBuffer also calls this.)
      @raises(EStreamNotImplementedWrite Always.) }
    function Write(const Buffer; Count: Longint): Longint; override;

    { Underlying stream. }
    property SourceStream: TStream read FSourceStream;

    { Should we free underlying SourceStream at destruction. }
    property OwnsSourceStream: boolean
      read FOwnsSourceStream write FOwnsSourceStream;

    { Peek next character. Returns the next character
      without making it "already read", so the next call to
      Read or ReadBuffer will return this char.
      Subsequent calls to PeekChar (without any Read/ReadBuffer between)
      will also return the same character.

      Returns -1 if stream ended, otherwise returns Ord or given char.

      This may call SourceStream.Read, and any exceptions that may be
      raised in SourceStream.Read are propagated higher from this method. }
    function PeekChar: Integer; virtual; abstract;

    { Read next character. A shortcut for Read(c, 1), but it returns -1
      if end of stream is reached. So it's consistent with PeekChar.
      Sometimes it's also more comfortable, and it's a little faster. }
    function ReadChar: Integer; virtual; abstract;

    { Read characters, until one of EndingChars (or end of stream) is found.
      The ending character is not "consumed" from the stream.
      The Result is guaranteed to not contain any char from EndingChars. }
    function ReadUpto(const EndingChars: TSetOfChars): string; virtual; abstract;

    {$ifndef FPC}
    property Position: Int64 read GetPosition;
    {$endif}

    constructor Create(ASourceStream: TStream; AOwnsSourceStream: boolean);
    destructor Destroy; override;
  end;

  { Read another stream, sequentially, always being able to back one character.
    This is a simplest non-abstract implementation of
    the @link(TPeekCharStream) class. }
  TSimplePeekCharStream = class(TPeekCharStream)
  private
    PeekedChar: Integer;
    IsPeekedChar: boolean;
    FPosition: Int64;
  protected
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
  { Read another stream, sequentially, always being able to back one character,
    and buffering it. This implements abstract TPeekCharStream class,
    so this is a purely sequential read-only stream that reads from
    underlying SourceStream and you can use PeekChar and ReadChar and
    ReadUpto routines.

    This stream will buffer incoming data from SourceStream.
    This means that reading by a very small chunks (like e.g. byte-by-byte)
    does not hurt performance. }
  TBufferedReadStream = class(TPeekCharStream)
  private
    FPosition: Int64;

    { Always non-nil, allocated for BufferSize bytes. }
    Buffer: PByteArray;

    { A position of the next unread char in Buffer,
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
    function GetPosition: Int64; override;
  public
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
{ @section(TComponent utilities) }

{ Create Component instance, if it's @nil.
  If Component = nil then it will do
  @code(Component := ComponentClass.Create(Owner)). }
procedure CreateIfNeeded(var Component: TComponent;
  ComponentClass: TComponentClass; Owner: TComponent);

{ ---------------------------------------------------------------------------- }
{ @section(Variables to read/write standard input/output using TStream classes.
  Initialized and finalized in this unit.) }

var
  { Streams that wrap standard input/output/error of the program.

    Note that you can't simultaneously read from StdInStream
    and StdInReader (reasons: see comments at TTextReader class,
    TTextReader has to internally manage the stream underneath).

    Notes for Windows:

    @orderedList(
      @item(
        Under Windows when program is a GUI program then some (or all)
        of the variables below may be nil.)

      @item(
        But they don't @italic(have) to be nil. User can run a GUI
        program and explicitly redirect it's standard stream, e.g.
        @code(cat something | my_program) for stdin or
        @code(my_program > output.txt) for stdout. Actually
        some shells, like Cygwin's bash, always redirect some streams
        "under the mask". And then you have
        some of std* streams available.

        Actually FPC (and Delphi?)
        RTL don't provide in such cases valid Input/Output/ErrOutput
        variables (because IsConsole = false). But my streams below
        try to obtain standard stream handles under Windows
        @italic(regardless of IsConsole value). So even a GUI program
        is able to write to stdin/stdout/stderr using these streams.)

      @item(
        Unfortunately, in a GUI program under Windows you @italic(cannot)
        depend on the fact that "StdOutStream <> nil means that stdout
        is actually available (because user redirected stdout etc.)".
        Reason? Windows failure, as usual:

        This is tested on Windows 2000 Prof, with FPC 2.0.0 and 2.1.1 (revision 4317).
        When no stdout is available, StdOutStream should be nil, because
        GetStdHandle(STD_OUTPUT_HANDLE) should return 0. However,
        GetStdHandle(STD_OUTPUT_HANDLE) doesn't return 0... It returns
        some stupid handle (no, not INVALID_HANDLE_VALUE either)
        that you can't write into (trying to write returns in ERROR_INVALID_HANDLE
        WinAPI error). It seems that there is no way for me to check
        whether GetStdHandle(STD_OUTPUT_HANDLE) returned valid handle
        (e.g. because the program's stdout was redirected, so stdout is perfectly
        available) or whether it returned something unusable.

        So if you write an $apptype GUI program and you want to try to use stdout
        anyway, you can't just check for StdOutStream <> nil.
        You should also check the first write to StdOutStream for EWriteError.

        Note that GetStdHandle(STD_INPUT_HANDLE) and GetStdHandle(STD_ERROR_HANDLE)
        work correctly, so it should be OK to check StdInStream <> nil or
        StdErrStream <> nil. The only problematic one is GetStdHandle(STD_OUTPUT_HANDLE).)
    )

    @groupBegin
  }
  StdInStream, StdOutStream, StdErrStream :TStream;
  StdInReader: TTextReader;
  { @groupEnd }

{ ---------------------------------------------------------------------------- }
{ @section(Stack, Queue) }

type
  { }
  TKamObjectStack = class(TObjectStack)
  private
    function GetCapacity: Integer;
    procedure SetCapacity(const Value: Integer);
  public
    property Capacity: Integer read GetCapacity write SetCapacity;
  end;

  TKamObjectQueue = class(TObjectQueue)
  private
    function GetCapacity: Integer;
    procedure SetCapacity(const Value: Integer);
  public
    property Capacity: Integer read GetCapacity write SetCapacity;
  end;

{ ---------------------------------------------------------------------------- }

  { Extended TObjectList for Kambi engine. }
  TKamObjectList = class(TObjectList)
  public
    { Create and fill with the contents of given array.

      Since in ObjectPascal you can create open array parameter on the fly,
      this constructor is often comfortable to use, for example you can
      write @code(List := TKamObjectList.Create(..., [Item1, Item2]);). }
    constructor CreateFromArray(const FreeObjects: boolean;
      const AItems: array of TObject);

    { Add contents of given array to the list. }
    procedure AddArray(const A: array of TObject);

    { Add contents of other TObjectList instance to the list. }
    procedure AddList(AList: TObjectList);

    { Replace first found descendant of ReplaceClass with NewItem.
      In case no descendant of ReplaceClass was found,
      we'll we add NewItem to the list (depending on AddBeginning value:
      at the beginning or at the end of the list).

      If NewItem is @nil, this simply removes the first found
      descendant of ReplaceClass.

      Returns the replaced (or removed) old item. It is removed from
      the list just like the Extract method, so it's never freed.
      Or @nil, if none was found (or there was @nil inside the list).

      The typical use scenario for this method is when NewItem is also
      a descendant from ReplaceClass, and you always keep at most one
      ReplaceClass descendant on the list.
      For example, you have UI controls list (like
      TGLWindow.Controls), and you want your NewItem to be the only instance
      of TGLMenu class inside.
      Moreover, in case order on the list is important (for example on
      TGLWindow.Controls order corresponds to screen depth --- what control
      is under / above each other), you want to place NewItem at the same
      position as previous TGLMenu instance, if any. }
    function MakeSingle(ReplaceClass: TClass; NewItem: TObject;
      AddBeginning: boolean = false): TObject;

    { Extract (remove from the list, but never free) given item index.
      This is similar TObjectList.Extract, except it takes an index. }
    function Extract(Index: Integer): TObject; overload;

    { Extract (remove from the list, but never free) first found descendant of
      RemoveClass. Returns the removed item. Or @nil, if none was found
      (or there was @nil inside the list and it got removed). }
    function Extract(RemoveClass: TClass): TObject; overload;

    { Delete (do not free) all found instances of the given Item.
      Shifts all other pointers to the left.
      Returns how many instances were removed (that is, how much Count
      was decreased). }
    function DeleteAll(Item: TObject): Cardinal;

    function IsFirst(Value: TObject): boolean;
    function IsLast(Value: TObject): boolean;

    procedure InsertIfNotExists(Index: Integer; Value: TObject);
    procedure AddIfNotExists(Value: TObject);
  end;

  {$define DYNARRAY_17_IS_FUNCTION}
  {$define DYNARRAY_17_IS_FUNCTION_METHOD}
  TDynArrayItem_17 = TNotifyEvent;
  PDynArrayItem_17 = ^TDynArrayItem_17;
  {$I dynarray_17.inc}
  TDynNotifyEventArray = class(TDynArray_17)
  public
    { Call all (non-nil) Items. }
    procedure ExecuteAll(Sender: TObject);
  end;

{ Remove all nils.
  Returns how many instances were removed (how much Count was decreased).
  Do not call this with other TFPSList descendants,
  only TFPGObjectList specializations. }
function FPGObjectList_RemoveNils(List: TFPSList): Cardinal;

{ Replace all OldItem instances with NewItem.
  Do not call this with other TFPSList descendants,
  only TFPGObjectList specializations. }
procedure FPGObjectList_ReplaceAll(List: TFPSList; OldItem, NewItem: TObject);

{ Free and set to @nil given item on TFPGObjectList.

  Usually, simply assigning to it @nil value (when list has FreeObjects = @true)
  would do the trick. Unfortunately there's bug
  http://bugs.freepascal.org/view.php?id=19854 . }
procedure FPGObjectList_FreeAndNilItem(List: TFPSList; I: Integer);

{ Set to @nil (never freeing) given item on TFPGObjectList. }
procedure FPGObjectList_NilItem(List: TFPSList; I: Integer);

{$undef read_interface}

implementation

uses
  {$ifdef UNIX}
    {$ifdef USE_LIBC} Libc {$else} Unix {$endif}
  {$endif}
  {$ifdef MSWINDOWS} Windows {$endif}
  , StrUtils, KambiFilesUtils;

{$define read_implementation}
{$I dynarray_17.inc}

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
   { We got 2nd newline character? Ignore it. }
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

constructor TStringListCaseSens.Create;
begin
  inherited;
  CaseSensitive := true;
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

procedure Strings_AddVrmlEngineProgramHelpSuffix(
  Strings: TStrings; const DisplayProgramName: string;
  const Version: string; WrapLines: boolean);
begin
  Strings_AddSplittedString(Strings,
    SVrmlEngineProgramHelpSuffix(DisplayProgramName, Version, WrapLines), nl);
end;

procedure Strings_SetText(SList: TStrings; const S: string);
begin
  if Length(S) = 1 then
    SList.Text := S + LineEnding else
    SList.Text := S;
end;

procedure Strings_Trim(Strings: TStrings; MaxCount: Cardinal);
begin
  while Cardinal(Strings.Count) > MaxCount do
    Strings.Delete(Strings.Count - 1);
end;

{ TStream helpers -------------------------------------------------------- }

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

procedure StreamSaveToFile(Stream: TStream; const FileName: string);
const
  BufSize = 100000;
var
  S : TFileStream;
  Buffer: Pointer;
  ReadCount: Integer;
begin
  Buffer := GetMem(BufSize);
  try
    S := TFileStream.Create(FileName, fmCreate);
    try
      repeat
        ReadCount := Stream.Read(Buffer^, BufSize);
        if ReadCount = 0 then
          Break else
          S.WriteBuffer(Buffer^, ReadCount);
      until false;
    finally
      S.free;
    end;
  finally FreeMem(Buffer) end;
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

function TPeekCharStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
 raise EStreamNotImplementedSeek.Create('TPeekCharStream.Seek not supported');
 Result := 0; { just to get rid of dummy fpc warning }
end;

function TPeekCharStream.Write(const Buffer; Count: Longint): Longint;
begin
 raise EStreamNotImplementedWrite.Create('TPeekCharStream.Write not supported');
 Result := 0; { just to get rid of dummy fpc warning }
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

{ Notes about TPeekCharStream.Read overriding:

  This tries to read next Count bytes from SourceStream, making sure
  that even character that you obtained by PeekChar will be returned
  here. In other words, this just implements Read method of TStream :)

  This may call SourceStream.Read, and any exceptions that may be
  raised in SourceStream.Read are propagated higher from this method.
}

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
 BufferEnd := SourceStream.Read(Buffer^[0], BufferSize);
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
  Move(Buffer^[BufferPos], LocalBuffer, Count);
  BufferPos := BufferPos + LongWord(Count);
  Result := Count;
 end else
 begin
  Move(Buffer^[BufferPos], LocalBuffer, BufferEnd - BufferPos);
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
   Move(Buffer^[0], PChar(@LocalBuffer)[Result], CopyCount);
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
var
  Peeked: Integer;
  BufferBeginPos, OldResultLength, ReadCount: LongWord;
begin
  Result := '';
  while true do
  begin
    Peeked := PeekChar;
    if (Peeked = -1) or (Chr(Peeked) in EndingChars) then
      Exit;

    (*Since this is TBufferedReadStream, we often have a lot of data in our
      Buffer. It would be wasteful to just check and copy it one-by-one,
      by code like:

        Result := Result + Chr(Peeked);

        { I could call above "Result := Result + Chr(ReadChar);"
          to make implementation of ReadUpto cleaner (not dealing
          with private fields of TStreamPeekChar).

          But doing like I'm doing now works a little faster.
          After PeekChar with result <> -1 I know that I have one place in the buffer.
          So I just explicitly increment BufferPos below. }
        Inc(BufferPos);
        Inc(FPosition);

      So the optimized version tries to grab as much as large as possible data
      chunk from the buffer, and copy it to Result at once.
    *)

    { Increase BufferPos as much as you can. We know that we can increase
      at least by one, since we just called PeekChar and it returned character
      <> -1 and not in EndingChars, so we use repeat...until instead of
      while...do. }
    BufferBeginPos := BufferPos;
    repeat
      Inc(BufferPos);
    until (BufferPos >= BufferEnd) or (Chr(Buffer^[BufferPos]) in EndingChars);

    ReadCount := BufferPos - BufferBeginPos;

    { Increase FPosition by the same amount that BufferPos was incremented }
    FPosition := FPosition + ReadCount;

    { Append Buffer^[BufferBeginPos... BufferPos - 1] to Result }
    OldResultLength := Length(Result);
    SetLength(Result, OldResultLength + ReadCount);
    Move(Buffer^[BufferBeginPos], Result[OldResultLength + 1], ReadCount);
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

{ TComponent helpers --------------------------------------------------- }

procedure CreateIfNeeded(var Component: TComponent;
  ComponentClass: TComponentClass; Owner: TComponent);
begin
 if Component = nil then
  Component := ComponentClass.Create(Owner);
end;

{ init / fini --------------------------------------------------------------- }

procedure InitStdStreams;

{ Note that instead of GetStdHandle(STD_INPUT_HANDLE) I could just use
  StdInputHandle, as this is initialized by FPC RTL exactly to
  GetStdHandle(STD_INPUT_HANDLE). Same for other Std*Handle.
  However
  1. This would not allow me to write InitStdStream without any $ifdefs,
     because Windows would still require checking for 0 and INVALID_HANDLE_VALUE
  2. This is not documented, so I prefer to not depend on this.
     For example, maybe in the future StdInputHandle will be always left as 0
     when not IsConsole? I want to exactly avoid this for my Std*Stream.
}

  {$ifdef MSWINDOWS}
  procedure InitStdStream(var Stream: TStream; nStdHandle: DWord);
  var
    Handle: THandle;
  begin
    Handle := GetStdHandle(nStdHandle);
    { If the function fails, the return value is INVALID_HANDLE_VALUE.
      If an application does not have associated standard handles,
      the return value is NULL.
      See [http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dllproc/base/getstdhandle.asp] }
    if (Handle <> INVALID_HANDLE_VALUE) and
       (Handle <> 0) then
      Stream := THandleStream.Create(Handle) else
      Stream := nil;
  end;
  {$endif MSWINDOWS}

  {$ifdef UNIX}
  procedure InitStdStream(var Stream: TStream; Handle: THandle);
  begin
    Stream := THandleStream.Create(Handle);
  end;
  {$endif UNIX}

begin
  InitStdStream(StdInStream,  {$ifdef MSWINDOWS} STD_INPUT_HANDLE  {$else} StdInputHandle  {$endif});
  InitStdStream(StdOutStream, {$ifdef MSWINDOWS} STD_OUTPUT_HANDLE {$else} StdOutputHandle {$endif});
  InitStdStream(StdErrStream, {$ifdef MSWINDOWS} STD_ERROR_HANDLE  {$else} StdErrorHandle  {$endif});
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

{ TKamObjectStack ------------------------------------------------------------ }

function TKamObjectStack.GetCapacity: Integer;
begin
  Result := List.Capacity;
end;

procedure TKamObjectStack.SetCapacity(const Value: Integer);
begin
  List.Capacity := Value;
end;

{ TKamObjectQueue ------------------------------------------------------------ }

function TKamObjectQueue.GetCapacity: Integer;
begin
  Result := List.Capacity;
end;

procedure TKamObjectQueue.SetCapacity(const Value: Integer);
begin
  List.Capacity := Value;
end;

{ TKamObjectList ------------------------------------------------------------- }

constructor TKamObjectList.CreateFromArray(const FreeObjects: boolean;
  const AItems: array of TObject);
begin
  Create(FreeObjects);
  AddArray(AItems);
end;

procedure TKamObjectList.AddArray(const A: array of TObject);
var
  I: Integer;
begin
  Capacity := Capacity + High(A) + 1;
  for I := 0 to High(A) do
    Add(A[I]);
end;

procedure TKamObjectList.AddList(AList: TObjectList);
var
  I: Integer;
begin
  inherited AddList(AList);

  { workaround http://bugs.freepascal.org/view.php?id=15655 }
  { make lnAdded notifications }
  for I := 0 to AList.Count - 1 do
    if AList[I] <> nil then
      Notify(AList[I], lnAdded);
end;

function TKamObjectList.MakeSingle(ReplaceClass: TClass; NewItem: TObject;
  AddBeginning: boolean): TObject;
var
  I: Integer;
begin
  if NewItem = nil then
  begin
    Result := Extract(ReplaceClass);
    Exit;
  end;

  for I := 0 to Count - 1 do
    if (TObject(List^[I]) <> nil) and
       (TObject(List^[I]) is ReplaceClass) then
    begin
      Result := TObject(List^[I]);
      TObject(List^[I]) := NewItem;
      { We're already sure Result <> nil here, because old TObject(List^[I])
        had to be <> nil to enter this code. So no need for usual
        check <> nil before Notify call. }
      Notify(Result, lnExtracted);
      { We're similarly already sure NewItem <> nil here, because the case
        of NewItem = nil is handled specially at the beginning of this method. }
      Notify(NewItem, lnAdded);
      Exit;
    end;

  Result := nil;
  if AddBeginning then
    Insert(0, NewItem) else
    Insert(Count, NewItem);
end;

function TKamObjectList.Extract(Index: Integer): TObject;
begin
  Result := TObject(List^[Index]);

  { Set to nil and then delete by index. This is a hack to prevent
    TList implementation from making any notification about Result
    delete/extraction. }
  TObject(List^[Index]) := nil;
  Delete(Index);

  if Assigned(Result) then Notify(Result, lnExtracted);
end;

function TKamObjectList.Extract(RemoveClass: TClass): TObject;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if TObject(List^[I]) is RemoveClass then
    begin
      Result := Extract(I);
      Exit;
    end;

  Result := nil;
end;

function TKamObjectList.DeleteAll(Item: TObject): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  I := 0;
  while I < Count do
  begin
    if Items[I] = Item then
      begin Delete(I); Inc(Result) end else
      Inc(I);
  end;
end;

function TKamObjectList.IsFirst(Value: TObject): boolean;
begin
  Result := (Count > 0) and (Items[0] = Value);
end;

function TKamObjectList.IsLast(Value: TObject): boolean;
begin
  Result := (Count > 0) and (Items[Count - 1] = Value);
end;

procedure TKamObjectList.InsertIfNotExists(Index: Integer; Value: TObject);
begin
  if IndexOf(Value) = -1 then
    Insert(Index, Value);
end;

procedure TKamObjectList.AddIfNotExists(Value: TObject);
begin
  if IndexOf(Value) = -1 then
    Add(Value);
end;

{ TDynNotifyEventArray  ------------------------------------------------------ }

procedure TDynNotifyEventArray.ExecuteAll(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Assigned(Items[I]) then
      Items[I](Sender);
end;

{ FGL helpers ---------------------------------------------------------------- }

function FPGObjectList_RemoveNils(List: TFPSList): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  I := 0;
  while I < List.Count do
  begin
    if PPointer(List.Items[I])^ = nil then
      begin List.Delete(I); Inc(Result) end else
      Inc(I);
  end;
end;

procedure FPGObjectList_ReplaceAll(List: TFPSList; OldItem, NewItem: TObject);
var
  I: Integer;
begin
  { do not assign to List.Items[I], to never free,
    regardless of http://bugs.freepascal.org/view.php?id=19854 fixed or not. }
  for I := 0 to List.Count - 1 do
    if TObject(PPointer(List.List)[I]) = OldItem then
      TObject(PPointer(List.List)[I]) := NewItem;
end;

procedure FPGObjectList_FreeAndNilItem(List: TFPSList; I: Integer);
begin
  { do not set the list item by normal List.Items, as it will cause
    problems once http://bugs.freepascal.org/view.php?id=19854
    will be fixed (if FreeObjects = true, then when assigning "Items[I] := nil",
    previous Items[I] must contain a valid reference or nil, not something freed.
    And we cannot temporarily change FreeObjects, as it's not in TFPSList). }
  FreeAndNil(PPointer(List.List)[I]);
end;

procedure FPGObjectList_NilItem(List: TFPSList; I: Integer);
begin
  { do not set the list item by normal List.Items, as it will cause
    problems once http://bugs.freepascal.org/view.php?id=19854
    will be fixed (if FreeObjects = true, then when assigning "Items[I] := nil",
    previous Items[I] must contain a valid reference or nil, not something freed.
    And we cannot temporarily change FreeObjects, as it's not in TFPSList). }
  PPointer(List.List)[I] := nil;
end;

initialization
  InitStdStreams;
finalization
  FiniStdStreams;
end.
