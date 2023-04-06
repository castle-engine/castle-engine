{
  Copyright 2000-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
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

unit CastleClassUtils;

{$I castleconf.inc}

interface

uses Classes, SysUtils, Contnrs, Generics.Collections,
  CastleUtils, CastleStringUtils;

{ ---------------------------------------------------------------------------- }
{ @section(TStrings utilities) }

{ Add some strings. }
procedure StringsAdd(Strs: TStrings; Count: integer; itemVal: string='dummy'); overload;

{ Add all strings from string array to TStrings instance. }
procedure AddStrArrayToStrings(const StrArr: array of string; strlist: TStrings);

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

{ Use this instead of @code(SList.Text := S) to workaround FPC 2.0.2 bug.
  See [http://www.freepascal.org/mantis/view.php?id=6699] }
procedure Strings_SetText(SList: TStrings; const S: String);

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

{ Write string contents, encoded as 8-bit (UTF-8), to stream.
  Versions with "Ln" suffix append a newline.
  Versions without Stream parameter write to StdOutStream.

  Note: When compiled with Delphi, overloaded versions that take
  Delphi 16-bit String convert it to 8-bit AnsiString,
  and still write 8-bit. So the output stream contents are the same,
  in both FPC and Delphi.

  @groupBegin }
procedure WriteStr(const Stream: TStream; const S: AnsiString); overload;
procedure WritelnStr(const Stream: TStream; const S: AnsiString); overload;
procedure WriteStr(const S: AnsiString); overload;
procedure WritelnStr(const S: AnsiString); overload;

{$ifndef FPC}
procedure WriteStr(const Stream: TStream; const S: String); overload;
procedure WritelnStr(const Stream: TStream; const S: String); overload;
procedure WriteStr(const S: String); overload;
procedure WritelnStr(const S: String); overload;
{$endif}
{ @groupEnd }

{ Read one character from stream.
  @raises EReadError If end of stream. }
function StreamReadChar(Stream: TStream): AnsiChar;

function StreamReadZeroEndString(Stream: TStream): AnsiString;

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
  backEndingChar: boolean; out endingChar: AnsiChar): AnsiString; overload;
function StreamReadUpto_NotEOS(Stream: TStream; const endingChars: TSetOfChars;
  backEndingChar: boolean): AnsiString; overload;
function StreamReadUpto_NotEOS(Stream: TStream; const endingChars: TSetOfChars;
  out endingChar: AnsiChar): AnsiString; overload;
function StreamReadUpto_NotEOS(Stream: TStream; const endingChars: TSetOfChars): AnsiString; overload;
{ @groupEnd }

{ Read stream, until you find some character in EndingChars, or end of stream.

  Compared to StreamReadUpto_NotEOS, this treats "end of stream"
  as a normal situation, and doesn't raise any exception on it.
  It sets EndingChar to -1 on end of stream. When EndingChar is not -1,
  you know you can safely cast it to normal 8-bit character.

  Everything else works like with StreamReadUpto_NotEOS.
  @groupBegin }
function StreamReadUpto_EOS(Stream: TStream; const endingChars: TSetOfChars;
  backEndingChar: boolean; out endingChar: integer): AnsiString; overload;
function StreamReadUpto_EOS(Stream: TStream; const endingChars: TSetOfChars;
  backEndingChar: boolean): AnsiString; overload;
function StreamReadUpto_EOS(Stream: TStream; const endingChars: TSetOfChars;
  out endingChar: integer): AnsiString; overload;
function StreamReadUpto_EOS(Stream: TStream; const endingChars: TSetOfChars): AnsiString; overload;
{ @groupEnd }

{ Read a growing stream, and append it to another destination stream.
  A "growing stream" is a stream that we can only read
  sequentially, no seeks allowed, and size is unknown until we hit the end.

  The only operation we do on GrowingStream is GrowingStream.Read and the only
  operation on DestStream is DestStream.WriteBuffer. So DestStream usually
  must be able to grow dynamically to accomodate any GrowingStream input size.

  This procedure ends when GrowingStream ends. If ResetDestStreamPosition
  then at the end we do DestStream.Position := 0 (since it is usually useful
  and it would be easy for you to forget about it). }
procedure ReadGrowingStream(const GrowingStream, DestStream: TStream;
  const ResetDestStreamPosition: Boolean;
  const BufferSize: Cardinal = 10 * 1000);

{ Read a growing stream, and returns it's contents as a string.
  A "growing stream" is a stream that we can only read
  sequentially, no seeks allowed, and size is unknown until we hit the end.
  Works on 8-bit strings, i.e. AnsiStrings. }
function ReadGrowingStreamToString(const GrowingStream: TStream): AnsiString;

{ Encode / decode a string in a binary stream. Records string length (4 bytes),
  then the string contents (Length(S) bytes).
  Works on 8-bit strings, i.e. AnsiStrings.
  @groupBegin }
procedure StreamWriteString(const Stream: TStream; const S: AnsiString);
function StreamReadString(const Stream: TStream): AnsiString;
{ @groupEnd }

{ Convert whole Stream to a string.
  Changes Stream.Position to 0 and then reads Stream.Size bytes,
  so be sure that Stream.Size is usable.
  Works on 8-bit strings, i.e. AnsiStrings. }
function StreamToString(const Stream: TStream): AnsiString;

{ Set contents of TMemoryStream to given string, in UTF-8 encoding.
  If Rewind then the position is reset to the beginning,
  otherwise it stays at the end.

  Works on 8-bit strings, i.e. AnsiStrings. }
procedure MemoryStreamLoadFromString(const Stream: TMemoryStream;
  const S: AnsiString; const Rewind: boolean = true); overload;
function MemoryStreamLoadFromString(
  const S: AnsiString; const Rewind: boolean = true): TMemoryStream; overload;

{ Set contents of TMemoryStream to given string,
  in UTF-8 or UTF-16 (matching default String) encoding.
  If Rewind then the position is reset to the beginning,
  otherwise it stays at the end.

  On FPC, works with 8-bit strings (AnsiStrings) and is equivalent to MemoryStreamLoadFromString8.
  On Delphi, works with 16-bit strings (UnicodeString), so the resulting stream size is 2x larger. }
procedure MemoryStreamLoadFromDefaultString(const Stream: TMemoryStream;
  const S: String; const Rewind: boolean = true); overload;
function MemoryStreamLoadFromDefaultString(
  const S: String; const Rewind: boolean = true): TMemoryStream; overload;

type
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
    would not work).

    Another advantage is the @link(Line) and @link(Column) information. }
  TPeekCharStream = class(TStream)
  private
    FSourceStream: TStream;
    FOwnsSourceStream: boolean;
    FLine, FColumn: Int64;
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
    procedure UpdateLineColumn(const C: AnsiChar); overload;
    procedure UpdateLineColumn(const Buffer; const BufferCount: Integer); overload;
  public
    constructor Create(ASourceStream: TStream; AOwnsSourceStream: boolean);
    destructor Destroy; override;

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
    function ReadUpto(const EndingChars: TSetOfChars): AnsiString; virtual;

    {$ifndef FPC}
    property Position: Int64 read GetPosition;
    {$endif}

    { Line number in the file (counting from 1). }
    property Line: Int64 read FLine;

    { Column number in the file (counting from 1). }
    property Column: Int64 read FColumn;
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
    constructor Create(ASourceStream: TStream; AOwnsSourceStream: boolean;
      ABufferSize: LongWord = DefaultReadBufferSize);
    destructor Destroy; override;

    function Read(var LocalBuffer; Count: Longint): Longint; override;
    function PeekChar: Integer; override;
    function ReadChar: Integer; override;
    function ReadUpto(const EndingChars: TSetOfChars): AnsiString; override;

    property BufferSize: LongWord read FBufferSize;
  end;

{ ---------------------------------------------------------------------------- }
{ @section(TComponent utilities) }

{ Create Component instance, if it's @nil.
  If Component = nil then it will do
  @code(Component := ComponentClass.Create(Owner)). }
procedure CreateIfNeeded(var Component: TComponent;
  ComponentClass: TComponentClass; Owner: TComponent);

type
  { Used by @link(TCastleComponent.PropertySections). }
  TPropertySection = (psBasic, psLayout);
  TPropertySections = set of TPropertySection;

  TCastleComponent = class;

  { Use by @link(TCastleComponent.TranslateProperties). }
  TTranslatePropertyEvent = procedure (const Sender: TCastleComponent;
    const PropertyName: String; var PropertyValue: String) of object;

  { Call methods of this class within @link(TCastleComponent.CustomSerialization) override.
    Do not create instances of this class yourself. }
  TSerializationProcess = class abstract
  public
    type
      TListEnumerateEvent = procedure (const Proc: TGetChildProc) of object;
      TListAddEvent = procedure (const NewComponent: TComponent) of object;
      TListClearEvent = procedure of object;

    var
      { @exclude
        Used by TCastleViewport to keep navigation/camera across undo. }
      InternalPreserveDataAcrossUndo: TComponent;

    { Serialize and deserialize given simple Value.
      This mechanism allows to explicitly serialize/deserialize any internal value,
      without the need to make it a published property.

      When deserializing, we always try to read it from file.
      If it is not present, the Value is not modified.

      When serializing, we write it to file only if IsStored.
      Generally IsStored=false should indicate "the Value is the same as when the object
      is created, thus there's no point in serializing it".

      The values are guaranteed to be read/written in the same way as if a published
      property with the same name (Key) and same type would be read/written.
      This allows to utilize this mechanism to read, to a local/private variable,
      a value that was previously a published property value.
      This is a way to provide backward-compatibility for old designs: this way class
      can interpret old values in design files, even though it no longer publishes
      given property.

      @seealso ReadWriteSubComponent

      @groupBegin }
    procedure ReadWriteInteger(const Key: String; var Value: Integer; const IsStored: Boolean);
      overload; virtual; abstract;
    procedure ReadWriteBoolean(const Key: String; var Value: Boolean; const IsStored: Boolean);
      overload; virtual; abstract;
    procedure ReadWriteString(const Key: String; var Value: String; const IsStored: Boolean);
      overload; virtual; abstract;
    procedure ReadWriteSingle(const Key: String; var Value: Single; const IsStored: Boolean);
      overload; virtual; abstract;
    { @groupEnd }

    { Serialize and deserialize a subcomponent.
      Being a subcomponent, we know that Value is not nil,
      and it is not referenced anywhere else in the design
      (so serialization/deserialization can expect class contents, not just name),
      so we just serialize and deserialize the contents.

      When deserializing, we always try to read it from file.
      If it is not present, nothing is modified.

      When serializing, we write it to file only if IsStored.
      Generally IsStored=false should indicate "the Value is the same as when the object
      is created, thus there's no point in serializing it".

      The values are guaranteed to be read/written in the same way as if a published
      property with the same name (Key) and same type would be read/written.
      This allows to utilize this mechanism to read, to a local/private variable,
      a value that was previously a published property value.
      This is a way to provide backward-compatibility for old designs: this way class
      can interpret old values in design files, even though it no longer publishes
      given property.

      @seealso ReadWriteInteger
      @seealso ReadWriteBoolean
      @seealso ReadWriteSingle
      @seealso ReadWriteString }
    procedure ReadWriteSubComponent(const Key: String; const Value: TComponent;
      const IsStored: Boolean); virtual; abstract;

    { Make a list serialized and deserialized.
      The definition of list is very flexible here, you provide callbacks
      that should, when called,

      @unorderedList(
        @itemSpacing compact
        @item enumerate (call other callback for each item),
        @item clear all items (that were possibly added by previous deserialization),
        @item add new item.
      )

      Do not worry about conflict between Key and some published property.
      We internally "mangle" keys to avoid it. }
    procedure ReadWriteList(const Key: String;
      const ListEnumerate: TListEnumerateEvent; const ListAdd: TListAddEvent;
      const ListClear: TListClearEvent); virtual; abstract;
  end;

  { Component with various CGE extensions: can be a parent of other non-visual components
    (to display them in CGE editor and serialize them to files), can be translated,
    can have custom logic when serializing/deserializing (CustomSerialization).

    Note that everywhere in CGE (in particular in editor and when serializing) we handle
    a standard Pascal TComponent as well. So there's no need to derive all your components from
    TCastleComponent, so you derive from standard TComponent too.
    You can use TCastleComponent only if necessary, i.e. only if you need one of
    the extra features in this class. }
  TCastleComponent = class(TComponent)
  strict private
    type
      { Used by @link(TCastleComponent.NonVisualComponentsEnumerate).
        Do not use this type explicitly, it should only be used by for..in
        construction like "for C in MyComponent.NonVisualComponentsEnumerate do ...".
        @exclude }
      TNonVisualComponentsEnumerator = record
      strict private
        FParent: TCastleComponent;
        FPosition: Integer;
        function GetCurrent: TComponent; inline;
      public
        constructor Create(const AParent: TCastleComponent);
        function MoveNext: Boolean; inline;
        property Current: TComponent read GetCurrent;
        function GetEnumerator: TNonVisualComponentsEnumerator;
      end;
    var
      FNonVisualComponents: TComponentList;
      FIsLoading: Boolean;
    function GetNonVisualComponents(const Index: Integer): TComponent;
    procedure SerializeNonVisualComponentsEnumerate(const Proc: TGetChildProc);
    procedure SerializeNonVisualComponentsAdd(const C: TComponent);
    procedure SerializeNonVisualComponentsClear;
  protected
    function GetInternalText: String; virtual;
    procedure SetInternalText(const Value: String); virtual;
    procedure SetName(const Value: TComponentName); override;

    { Enumerate all properties that are possible to translate in this component.
      E.g. in @link(TCastleLabel) it will return @link(TCastleLabel.Caption),
      in @link(TCastleEdit) it will return @link(TCastleEdit.Text) and
      @link(TCastleEdit.Placeholder).

      Returns only non-empty properties, thus assuming that if current
      (by convention, English) text is empty, then there is no point
      in translating it. Moreover descendants may define boolean properties
      to exclude particular text from translating, e.g.
      @link(TCastleLabel.CaptionTranslate),
      @link(TCastleEdit.TextTranslate),
      @link(TCastleEdit.PlaceholderTranslate).

      It is not recursive (it doesn't enumerate children properties).
      Use global @link(TranslateProperties) procedure to call this
      on a hierarchy of TComponent.

      You usually don't want to call this method (it is called by other engine routines).
      But you may find it useful to override this, if you define new component. }
    procedure TranslateProperties(const TranslatePropertyEvent: TTranslatePropertyEvent); virtual;
  public
    { Internal field used by CastleComponentSerialize.
      @exclude }
    InternalOriginalName: String;

    destructor Destroy; override;

    { Override this method to call various methods of SerializationProcess,
      which in turn allows to serialize/deserialize things that are not published.
      This allows to serialize/deserialize with more freedom, e.g. to serialize/deserialize
      some private field. }
    procedure CustomSerialization(const SerializationProcess: TSerializationProcess); virtual;

    { Main text property, that is synchronized with Name initially.
      @exclude }
    property InternalText: String read GetInternalText write SetInternalText;

    { Set IsLoading to @true and (only on FPC) add csLoading flag to ComponentState.
      Used when deserializing.
      Do not call this yourself, CastleComponentSerialize automatically calls this.
      @exclude }
    procedure InternalLoading;

    { Set IsLoading to @false and (only on FPC) remove csLoading flag from ComponentState
      and call virtual TComponent.Loaded.
      Used when deserializing.
      Do not call this yourself, CastleComponentSerialize automatically calls this.
      Descendants can override TComponent.Loaded to react to being loaded.
      @exclude }
    procedure InternalLoaded;

    { Section where to show property in the editor. }
    function PropertySections(const PropertyName: String): TPropertySections; virtual;

    { Ignore this component when serializing parent's
      @link(TCastleUserInterface.Controls) list or @link(TCastleTransform.List),
      and do not show this component in CGE editor.
      This simply sets csTransient flag in ComponentStyle.

      This is useful for children that are automatically managed by the parent,
      and should not be modified by user code. For example,
      TCastleCheckbox is internally composed from TCastleImageControl
      and TCastleLabel children, but we don't want to serialize or even
      show these children to user.

      Note that if you want to prevent this component from serializing as part of
      @link(TCastleUserInterface.Controls) list or @link(TCastleTransform.List),
      but you still want it to be visible in CGE editor,
      then make it a "subcomponent" instead, by @code(SetSubComponent(true)).

      Note that both csSubComponent and csTransient only disable the component
      serialization as part of parent's lists enumerated by CustomSerialization
      (see internal TCastleUserInterface.SerializeChildrenEnumerate ,
      TCastleTransform.SerializeChildrenEnumerate,
      TCastleTransform.SerializeBehaviorsEnumerate).

      If you will make the component published in its own property
      (which is normal for "subcomponents")
      then it will be serialized anyway, just as part of it's own property
      (like TCastleScrollView.ScrollArea).
      So to @italic(really) avoid serializing a children component
      make it csSubComponent and/or csTransient,
      and do not publish it.
    }
    procedure SetTransient;

    { Add non-visual component to this component.
      This is used to organize non-visual components in a tree hierarchy,
      in CGE designs and editor.

      @seealso NonVisualComponentsCount
      @seealso NonVisualComponents
      @seealso NonVisualComponentsEnumerate }
    procedure AddNonVisualComponent(const NonVisualComponent: TComponent);

    { Insert non-visual component to this component.
      This is used to organize non-visual components in a tree hierarchy,
      in CGE designs and editor.

      @seealso NonVisualComponentsCount
      @seealso NonVisualComponents
      @seealso NonVisualComponentsEnumerate }
    procedure InsertNonVisualComponent(const Index: Integer; const NonVisualComponent: TComponent);

    { Remove component previously added by AddNonVisualComponent. }
    procedure RemoveNonVisualComponent(const NonVisualComponent: TComponent);

    { Index of component previously added non-visual component.
      Returns -1 if component was not found. }
    function NonVisualComponentsIndexOf(const NonVisualComponent: TComponent): Integer;

    { Count of components added by AddNonVisualComponent.

      @seealso AddNonVisualComponent
      @seealso NonVisualComponents
      @seealso NonVisualComponentsEnumerate }
    function NonVisualComponentsCount: Integer;

    { Components added by AddNonVisualComponent. }
    property NonVisualComponents [const Index: Integer]: TComponent read GetNonVisualComponents;

    { You can enumerate current non-visual components using loop like
      @code(for C in MyComponent.NonVisualComponentsEnumerate do ...).
      Do not call this method in other contexts, it is only useful for "for..in" construction.

      @seealso AddNonVisualComponent }
    function NonVisualComponentsEnumerate: TNonVisualComponentsEnumerator;

    { Is the component during deserialization now.

      Note: We can't use @code(csLoading in ComponentState) because in Delphi
      it is not possible to control it from CastleComponentSerialize. }
    property IsLoading: Boolean read FIsLoading;
  end;

{ Enumerate all properties that are possible to translate in this component
  and its children. E.g. in @link(TCastleLabel) it will return @link(TCastleLabel.Caption),
  in @link(TCastleEdit) it will return @link(TCastleEdit.Text)
  and @link(TCastleEdit.Placeholder).

  Returns only non-empty properties, thus assuming that if current
  (by convention, English) text is empty, then there is no point
  in translating it. Moreover descendants may define boolean properties
  to exclude particular text from translating, e.g.
  @link(TCastleLabel.CaptionTranslate),
  @link(TCastleEdit.TextTranslate),
  @link(TCastleEdit.PlaceholderTranslate).

  For every TComponent it also recursively enumerates properties
  to translate in children, i.e. in all published subcomponents and children
  (returned by TCastleComponent.CustomSerialization). The goal is to be 100% consistent with
  CastleComponentSerialize, which is used to (de)serialize hierarchy of
  components (like TCastleUserInterface or TCastleTransform).

  You usually don't want to call this method (it is called by other engine routines).
  Use higher-level routines in @link(CastleLocalizationGetText).

  @seealso TCastleComponent.TranslateProperties }
procedure TranslateProperties(const C: TComponent;
  const TranslatePropertyEvent: TTranslatePropertyEvent);

{ ---------------------------------------------------------------------------- }
{ @section(Variables to read/write standard input/output using TStream classes.
  Initialized and finalized in this unit.) }

var
  { Streams to read/write a standard input/output/error of the program.

    Tip: to read the standard input as a text file,
    you can use @link(TTextReader) and @link(StdInStream):

    @longCode(#
    var
      StdInReader: TTextReader;
    begin
      StdInReader := TTextReader.Create(StdInStream, false);
      try
        while not StdInReader.Eof do
          DoSomethingWithInputLine(StdInReader.Readln);
      finally FreeAndNil(StdinReader) end;
    end;
    #)

    The advantage of using @link(TTextReader) above,
    compared to using the standard Pascal @code(Readln) procedure
    to read from the standard Pascal @code(Input) text file,
    is that you can easily modify the above code to read from @italic(any)
    stream. So, instead of the standard input, you can easily read
    some stream that decompresses gzip data, or downloads data from
    the Internet... Actually, the overloaded constructor
    @link(TTextReader.Create) can accept an URL, like a @code(file://...)
    or @code(http://...), and will internally use the stream returned
    by @link(Download) function for this URL.

    Notes @bold(only for Windows GUI applications):

    @orderedList(
      @item(
        Some (or all) of the variables below may be @nil in a Windows GUI
        application.

        If you want to be on the safe side, then don't use these streams
        in the GUI application (when IsConsole = false on Windows).
        But (with some caveats, see below) you can sometimes use them,
        if they are not-nil.
      )

      @item(
        You @italic(can) have some of the standard streams available,
        even in a GUI application.

        User can run a GUI program and explicitly redirect it's standard stream,
        e.g.  @code(cat something | my_program.exe) for stdin or
        @code(my_program.exe > output.txt) for stdout. Actually
        some shells, like Cygwin's bash, always redirect some streams
        under the hood.

        Note that the FPC (and Delphi?) RTL may not provide the standard
        Input/Output/ErrOutput in this case (because IsConsole = false),
        but we try a little harder, regardless of the IsConsole value.
      )

      @item(
        Caveat: in a GUI program under Windows you @italic(cannot)
        depend on the fact that "StdOutStream <> nil means that stdout
        is actually available (because user redirected stdout etc.)".
        A first write to the StdOutStream may cause EWriteError,
        so watch out for this.

        This is tested on Windows 2000 Prof, with FPC 2.0.0 and 2.1.1 (revision 4317).
        When no stdout is available, StdOutStream should be nil, because
        GetStdHandle(STD_OUTPUT_HANDLE) should return 0. However,
        GetStdHandle(STD_OUTPUT_HANDLE) doesn't return 0... It returns
        some invalid handle (no, not INVALID_HANDLE_VALUE either)
        that you can't write into (trying to write returns in ERROR_INVALID_HANDLE
        WinAPI error). It seems that there is no way for me to check
        whether GetStdHandle(STD_OUTPUT_HANDLE) returned valid handle
        (e.g. because the program's stdout was redirected, so stdout is perfectly
        available) or whether it returned something unusable.

        Note that GetStdHandle(STD_INPUT_HANDLE) and GetStdHandle(STD_ERROR_HANDLE)
        work correctly, so it should be OK to check StdInStream <> nil or
        StdErrStream <> nil. The only problematic one is GetStdHandle(STD_OUTPUT_HANDLE).)
      )
    )
  }
  StdInStream, StdOutStream, StdErrStream: TStream;

{ ---------------------------------------------------------------------------- }
{ @section(Containers) }

type
  { Extended TObjectStack for Castle Game Engine. }
  TCastleObjectStack = class({$ifndef PASDOC}Contnrs.{$endif}TObjectStack)
  private
    function GetCapacity: TListSize;
    procedure SetCapacity(const Value: TListSize);
  public
    property Capacity: TListSize read GetCapacity write SetCapacity;
  end;

  { Extended TObjectQueue for Castle Game Engine. }
  TCastleObjectQueue = class({$ifndef PASDOC}Contnrs.{$endif}TObjectQueue)
  private
    function GetCapacity: TListSize;
    procedure SetCapacity(const Value: TListSize);
  public
    property Capacity: TListSize read GetCapacity write SetCapacity;
  end;

  { Extended TObjectList for Castle Game Engine. }
  TCastleObjectList = class({$ifndef PASDOC}Contnrs.{$endif}TObjectList)
  public
    { Create and fill with the contents of given array.

      Since in ObjectPascal you can create open array parameter on the fly,
      this constructor is often comfortable to use, for example you can
      write @code(List := TCastleObjectList.Create(..., [Item1, Item2]);). }
    constructor CreateFromArray(const FreeObjects: boolean;
      const AItems: array of TObject);

    { Add contents of given array to the list. }
    procedure AddRange(const A: array of TObject); overload;
    procedure AddArray(const A: array of TObject); deprecated 'use AddRange, consistent with other lists';

    { Add contents of other TObjectList instance to the list. }
    procedure AddRange(AList: Contnrs.TObjectList); overload;
    procedure AddList(AList: Contnrs.TObjectList); deprecated 'use AddRange, consistent with other lists';

    { Replace first found descendant of ReplaceClass with NewItem.
      In case no descendant of ReplaceClass was found,
      we'll we add NewItem to the list (depending on AddEnd value:
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
      TCastleWindow.Controls), and you want your NewItem to be the only instance
      of TCastleOnScreenMenu class inside.
      Moreover, in case order on the list is important (for example on
      TCastleWindow.Controls order corresponds to screen depth --- what control
      is under / above each other), you want to place NewItem at the same
      position as previous TCastleOnScreenMenu instance, if any. }
    function MakeSingle(ReplaceClass: TClass; NewItem: TObject;
      AddEnd: boolean): TObject;

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
    function RemoveAll(Item: TObject): Cardinal;

    function IsFirst(Value: TObject): boolean;
    function IsLast(Value: TObject): boolean;

    procedure InsertIfNotExists(Index: Integer; Value: TObject);
    procedure AddIfNotExists(Value: TObject);
  end;

  TNotifyEventList = class({$ifdef FPC}specialize{$endif} TList<TNotifyEvent>)
  public
    { Call all (non-nil) Items, from first to last. }
    procedure ExecuteAll(Sender: TObject);
    { Call all (non-nil) Items, from first to last. }
    procedure ExecuteForward(Sender: TObject);
    { Call all (non-nil) Items, from last to first. }
    procedure ExecuteBackward(Sender: TObject);
  end;

{$ifdef FPC}
function DumpStackToString(const BaseFramePointer: Pointer): string;
function DumpExceptionBackTraceToString: string;
{$endif}

{ Propose a name for given component class, making it unique in given ComponentsOwner. }
function InternalProposeName(const ComponentClass: TComponentClass;
  const ComponentsOwner: TComponent): String;

type
  TFreeNotificationObserver = class;

  { Notification from TFreeNotificationObserver.
    Note that it doesn't specify the freed component,
    because you can get it from @code(Sender.Observed) if needed. }
  TFreeNotificationEvent = procedure (const Sender: TFreeNotificationObserver) of object;

  (*Observe when something is freed, and call an event then.
    You need to set @link(Observed) of this component
    to make it notified about freeing of something.
    When the @link(Observed) is freed, this component will make OnFreeNotification event.

    An example code using it:

    @longCode(#
    type
      TChild = class(TComponent)
      end;

      TContainer = class(TComponent)
      private
        FChild: TChild;
        FChildObserver: TFreeNotificationObserver;
        procedure SetChild(const Value: TChild);
        procedure ChildFreeNotification(const Sender: TFreeNotificationObserver);
      public
        constructor Create(AOwner: TComponent);
        property Child: TChild read FChild write SetChild;
      end;

    implementation

    constructor TContainer.Create(AOwner: TComponent);
    begin
      inherited;
      FChildObserver := TFreeNotificationObserver.Create(Self);
      FChildObserver.OnFreeNotification := @ChildFreeNotification;
    end;

    procedure TContainer.SetChild(const Value: TChild);
    begin
      if FChild <> Value then
      begin
        FChild := Value;
        FChildObserver.Observed := Value;
      end;
    end;

    procedure TContainer.ChildFreeNotification(const Sender: TFreeNotificationObserver);
    begin
      FChild := nil;
    end;
    #)

    Using this is an alternative to observing freeing using
    standard TComponent.FreeNotification / TComponent.RemoveFreeNotification mechanism
    https://castle-engine.io/modern_pascal_introduction.html#_free_notification .
    Using TFreeNotificationObserver is:

    @unorderedList(
      @item(A bit simpler and it's harder to make a mistake.

        E.g. the line @code(FChildObserver.Observed := Value) is simpler
        than the typical equivalent lines required to unregister notification
        of the old value and register notification of the new value.
        (See https://castle-engine.io/modern_pascal_introduction.html#_free_notification
        example of FreeNotification usage.)
      )

      @item(The TContainer doesn't need to descend from TComponent.

        You can manually free FChildObserver in the TContainer.Destroy.)

      @item(Reliable if one class wants to observe freeing of multiple properties,
        and some of those properties may be sometimes equal.
        In this case using FreeNotification / RemoveFreeNotification with the main class
        would be unreliable (as RemoveFreeNotification removes the notification,
        even if you registered to it twice by FreeNotification), and requires complicated
        code to handles these special cases.

        Using this component as a "proxy" to track each property is simpler,
        there's no additional code to handle this case. You just need different
        observer for each property to be observed.)
    )
  *)
  TFreeNotificationObserver = class(TComponent)
  strict private
    FObserved: TComponent;
    FOnFreeNotification: TFreeNotificationEvent;
    procedure SetObserved(const Value: TComponent);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;

    { Called when we receive notification that something was freed. }
    property OnFreeNotification: TFreeNotificationEvent read FOnFreeNotification write FOnFreeNotification;

    { Setting this property makes the given component observed, freeing it will make
      OnFreeNotification.
      When setting, the previous value of this property stops being observed.

      Note that this property will be automatically changed to @nil after OnFreeNotification,
      if the OnFreeNotification will not change it. This is necessary (we need to detach
      our "free notification" from a component that will be freed) and makes it safer
      (we will not expose dangling pointer). }
    property Observed: TComponent read FObserved write SetObserved;
  end;

implementation

uses
  {$ifdef UNIX} {$ifdef FPC} Unix, {$endif} {$endif}
  {$ifdef MSWINDOWS} Windows, {$endif}
  StrUtils, Math {$ifdef FPC}, StreamIO, RTTIUtils {$endif}, TypInfo,
  CastleLog;

{ TStrings helpers ------------------------------------------------------- }

procedure StringsAdd(Strs: TStrings; Count: integer; itemVal: string);
var
  i: integer;
begin
  for i := 1 to Count do Strs.Add(itemVal);
end;

procedure AddStrArrayToStrings(const StrArr: array of string; strlist: TStrings);
var
  i: integer;
begin
  for i := 0 to High(StrArr) do strlist.Append(StrArr[i]);
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

procedure Strings_SetText(SList: TStrings; const S: String);
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

procedure WriteStr(const Stream: TStream; const S: AnsiString);
begin
  Stream.WriteBuffer(Pointer(S)^, Length(S));
end;

procedure WritelnStr(const Stream: TStream; const S: AnsiString);
begin
  WriteStr(Stream, S);
  WriteStr(Stream, nl);
end;

procedure WriteStr(const S: AnsiString);
begin
  WriteStr(StdOutStream, S);
end;

procedure WritelnStr(const S: AnsiString);
begin
  WritelnStr(StdOutStream, S);
end;

{$ifndef FPC}
{$warnings off}

procedure WriteStr(const Stream: TStream; const S: String);
begin
  WriteStr(Stream, AnsiString(S));
end;

procedure WritelnStr(const Stream: TStream; const S: String);
begin
  WritelnStr(Stream, AnsiString(S));
end;

procedure WriteStr(const S: String);
begin
  WriteStr(AnsiString(S));
end;

procedure WritelnStr(const S: String);
begin
  WritelnStr(AnsiString(S));
end;

{$warnings on}
{$endif}

function StreamReadChar(Stream: TStream): AnsiChar;
begin
  Stream.ReadBuffer(result, SizeOf(result));
end;

function StreamReadZeroEndString(Stream: TStream): AnsiString;
begin
  result := StreamReadUpto_NotEOS(Stream, [#0], false);
end;

function StreamReadUpto_NotEOS(Stream: TStream; const endingChars: TSetOfChars;
  backEndingChar: boolean; out endingChar: AnsiChar): AnsiString; overload;
var
  readLen: integer; { ile znakow odczytales }
  ch: AnsiChar;
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
  backEndingChar: boolean):AnsiString; overload;
var
  dummy: AnsiChar;
begin
  result := StreamReadUpto_NotEOS(Stream, endingChars, backEndingChar, dummy);
end;

function StreamReadUpto_NotEOS(Stream: TStream; const endingChars: TSetOfChars;
  out endingChar: AnsiChar): AnsiString;
begin
  result := StreamReadUpto_NotEOS(Stream, endingChars, false, endingChar);
end;

function StreamReadUpto_NotEOS(Stream: TStream; const endingChars: TSetOfChars): AnsiString;
begin
  result := StreamReadUpto_NotEOS(Stream, endingChars, false);
end;

function StreamReadUpto_EOS(Stream: TStream; const endingChars: TSetOfChars;
  backEndingChar: boolean; out endingChar: integer): AnsiString; overload;
var readLen: integer; { ile znakow odczytales }
    ch: AnsiChar;
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
  backEndingChar: boolean): AnsiString; overload;
var
  dummy: integer;
begin
  result := StreamReadUpto_EOS(Stream, endingChars, backEndingChar, dummy);
end;

function StreamReadUpto_EOS(Stream: TStream; const endingChars: TSetOfChars;
  out endingChar: integer): AnsiString;
begin
  result := StreamReadUpto_EOS(Stream, endingChars, false, endingChar);
end;

function StreamReadUpto_EOS(Stream: TStream; const endingChars: TSetOfChars): AnsiString;
begin
  result := StreamReadUpto_EOS(Stream, endingChars, false);
end;

procedure ReadGrowingStream(const GrowingStream, DestStream: TStream;
  const ResetDestStreamPosition: Boolean;
  const BufferSize: Cardinal);
var
  ReadCount: Integer;
  Buffer: Pointer;
begin
  Buffer := GetMem(BufferSize);
  try
    repeat
      ReadCount := GrowingStream.Read(Buffer^, BufferSize);
      if ReadCount = 0 then Break;
      DestStream.WriteBuffer(Buffer^, ReadCount);
    until false;
  finally FreeMemNiling(Buffer) end;
  if ResetDestStreamPosition then DestStream.Position := 0;
end;

function ReadGrowingStreamToString(const GrowingStream: TStream): AnsiString;
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

procedure StreamWriteString(const Stream: TStream; const S: AnsiString);
var
  L: Integer;
begin
  L := Length(s);
  Stream.WriteBuffer(L, SizeOf(L));
  { check L > 0 to avoid range check error on S[1] }
  if L > 0 then Stream.WriteBuffer(S[1], L);
end;

function StreamReadString(const Stream: TStream): AnsiString;
var
  L: Integer;
begin
  Stream.ReadBuffer(L, SizeOf(L));
  SetLength(Result, L);
  { check L > 0 to avoid range check error on Result[1] }
  if L > 0 then Stream.ReadBuffer(Result[1], L);
end;

function StreamToString(const Stream: TStream): AnsiString;
begin
  SetLength(Result, Stream.Size);
  Stream.Position := 0;
  Stream.ReadBuffer(Pointer(Result)^, Length(Result));
end;

procedure MemoryStreamLoadFromString(const Stream: TMemoryStream;
  const S: AnsiString; const Rewind: boolean);
begin
  Stream.Size := Length(S);
  if S <> '' then
  begin
    Stream.WriteBuffer(S[1], Length(S));
    if Rewind then Stream.Position := 0;
  end;
end;

function MemoryStreamLoadFromString(const S: AnsiString; const Rewind: boolean): TMemoryStream;
begin
  Result := TMemoryStream.Create;
  try
    MemoryStreamLoadFromString(Result, S, Rewind);
  except FreeAndNil(Result); raise end;
end;

procedure MemoryStreamLoadFromDefaultString(const Stream: TMemoryStream;
  const S: String; const Rewind: boolean);
begin
  Stream.Size := Length(S) * SizeOf(Char);
  if S <> '' then
  begin
    Stream.WriteBuffer(S[1], Length(S) * SizeOf(Char));
    if Rewind then Stream.Position := 0;
  end;
end;

function MemoryStreamLoadFromDefaultString(const S: String; const Rewind: boolean): TMemoryStream;
begin
  Result := TMemoryStream.Create;
  try
    MemoryStreamLoadFromString(Result, S, Rewind);
  except FreeAndNil(Result); raise end;
end;

{ TPeekCharStream -------------------------------------------------- }

constructor TPeekCharStream.Create(ASourceStream: TStream;
  AOwnsSourceStream: boolean);
begin
  inherited Create;
  FOwnsSourceStream := AOwnsSourceStream;
  FSourceStream := ASourceStream;
  FLine := 1;
  FColumn := 1;
end;

destructor TPeekCharStream.Destroy;
begin
  if OwnsSourceStream then FreeAndNil(FSourceStream);
  inherited;
end;

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
  {$ifdef FPC}
  Result := 0; { just to get rid of dummy fpc warning }
  {$endif}
end;

function TPeekCharStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EStreamNotImplementedWrite.Create('TPeekCharStream.Write not supported');
  {$ifdef FPC}
  Result := 0; { just to get rid of dummy fpc warning }
  {$endif}
end;

procedure TPeekCharStream.UpdateLineColumn(const C: AnsiChar);
begin
  if (C = #13) or
     (C = #10) then
  begin
    // This way we increase FLine correctly for both Unix and Windows line ending
    if C = #10 then
      Inc(FLine);
    FColumn  := 1;
  end else
    Inc(FColumn);
end;

procedure TPeekCharStream.UpdateLineColumn(const Buffer; const BufferCount: Integer);
var
  I: Integer;
begin
  for I := 0 to BufferCount - 1 do
    UpdateLineColumn(PAnsiChar(@Buffer)[I]);
end;

function TPeekCharStream.ReadUpto(const EndingChars: TSetOfChars): AnsiString;
var
  Peeked: Integer;
begin
  Result := '';
  while true do
  begin
    Peeked := PeekChar;
    if (Peeked = -1) or (AnsiChar(Peeked) in EndingChars) then
      Exit;
    { ReadChar will return same thing as Peeked now }
    Result := Result + AnsiChar(ReadChar);
  end;
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
    { Note: It would be more natural to access
        PAnsiChar(@Buffer)[...],
      but on Delphi the PAnsiChar cannot be indexed like an array. }
    PByteArray(@Buffer)^[0] := PeekedChar;
    Result := 1 + SourceStream.Read(PByteArray(@Buffer)^[1], Count - 1);
    { Note that if SourceStream.Read will raise an exception,
      we will still have IsPeekedChar = true. }
    IsPeekedChar := false;
  end else
    Result := SourceStream.Read(Buffer, Count);
  FPosition := FPosition + Result;

  UpdateLineColumn(Buffer, Result);
end;

function TSimplePeekCharStream.PeekChar: Integer;
var
  C: AnsiChar;
begin
  if not IsPeekedChar then
  begin
    if SourceStream.Read(C, 1) = 0 then
      PeekedChar := -1
    else
      PeekedChar := Ord(C);
    IsPeekedChar := true;
  end;
  Result := PeekedChar;
end;

function TSimplePeekCharStream.ReadChar: Integer;
{ This is somehow optimized version of TSimplePeekCharStream.Read
  for the case when Count = 1. }
var
  C: AnsiChar;
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

  if Result <> -1 then
    UpdateLineColumn(AnsiChar(Result));
end;

{ TBufferedReadStream ----------------------------------------------------- }

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
var
  CopyCount: LongWord;
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
      CopyCount := Min(LongWord(Count), BufferEnd - BufferPos);
      Move(Buffer^[0], PChar(@LocalBuffer)[Result], CopyCount);
      BufferPos := BufferPos + CopyCount;
      Result := Result + LongInt(CopyCount);
    end else
    begin
      Result := Result + SourceStream.Read(PChar(@LocalBuffer)[Result], Count);
    end;
  end;
  FPosition := FPosition + Result;

  UpdateLineColumn(LocalBuffer, Result);
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

  if Result <> -1 then
    UpdateLineColumn(AnsiChar(Result));
end;

function TBufferedReadStream.ReadUpto(const EndingChars: TSetOfChars): AnsiString;
var
  Peeked: Integer;
  BufferBeginPos, OldResultLength, ReadCount: LongWord;
  ConsumingChar: AnsiChar;
begin
  Result := '';
  while true do
  begin
    Peeked := PeekChar;
    if (Peeked = -1) or (AnsiChar(Peeked) in EndingChars) then
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

    UpdateLineColumn(AnsiChar(Peeked));

    { Increase BufferPos as much as you can. We know that we can increase
      at least by one, since we just called PeekChar and it returned character
      <> -1 and not in EndingChars, so we use repeat...until instead of
      while...do. }
    BufferBeginPos := BufferPos;
    repeat
      Inc(BufferPos);
      if BufferPos >= BufferEnd then
        Break
      else
      begin
        ConsumingChar := AnsiChar(Buffer^[BufferPos]);
        if ConsumingChar in EndingChars then
          Break
        else
          UpdateLineColumn(ConsumingChar);
      end;
    until false;

    ReadCount := BufferPos - BufferBeginPos;

    { Increase FPosition by the same amount that BufferPos was incremented }
    FPosition := FPosition + ReadCount;

    { Append Buffer^[BufferBeginPos... BufferPos - 1] to Result }
    OldResultLength := Length(Result);
    SetLength(Result, OldResultLength + ReadCount);
    Move(Buffer^[BufferBeginPos], Result[OldResultLength + 1], ReadCount);
  end;
end;

{ TComponent helpers --------------------------------------------------- }

procedure CreateIfNeeded(var Component: TComponent;
  ComponentClass: TComponentClass; Owner: TComponent);
begin
  if Component = nil then
    Component := ComponentClass.Create(Owner);
end;

{ TCastleComponent.TNonVisualComponentsEnumerator ------------------------------------------------- }

{ TNonVisualComponentsEnumerator is optimized to be a record, following
  https://hallvards.blogspot.com/2007/10/more-fun-with-enumerators.html }

constructor TCastleComponent.TNonVisualComponentsEnumerator.Create(const AParent: TCastleComponent);
begin
//  inherited Create;
  FParent := AParent;
  FPosition := -1;
end;

function TCastleComponent.TNonVisualComponentsEnumerator.GetCurrent: TComponent;
begin
  Result := FParent.NonVisualComponents[FPosition];
end;

function TCastleComponent.TNonVisualComponentsEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FParent.NonVisualComponentsCount;
end;

function TCastleComponent.TNonVisualComponentsEnumerator.GetEnumerator: TNonVisualComponentsEnumerator;
begin
  // Returns itself. See https://wiki.freepascal.org/for-in_loop
  Result := Self;
end;

{ TCastleComponent ----------------------------------------------------------- }

destructor TCastleComponent.Destroy;
begin
  FreeAndNil(FNonVisualComponents);
  inherited;
end;

procedure TCastleComponent.SetTransient;
begin
  Include(FComponentStyle, csTransient);
end;

function TCastleComponent.GetInternalText: String;
begin
  Result := '';
end;

procedure TCastleComponent.SetInternalText(const Value: String);
begin
end;

procedure TCastleComponent.InternalLoading;
begin
  {$ifdef FPC}
  Loading;
  {$endif}
  FIsLoading := true;
end;

procedure TCastleComponent.InternalLoaded;
begin
  FIsLoading := false;

  { We need to call Loaded because some things can be delayed to run after
    loading for example TCastleSceneCore.UpdateAutoAnimation() }
  Loaded;
end;

procedure TCastleComponent.SetName(const Value: TComponentName);
var
  ChangeInternalText: Boolean;
begin
  { Implementation similar to procedure TControl.SetName in Lazarus. }
  if Name = Value then exit;
  ChangeInternalText :=
    // TControl.SetName does this even in non-design mode.
    // ((csDesigning in ComponentState) or CastleDesignMode) and
    //
    // Note that we don't do it during loading, otherwise having empty
    // Caption e.g. on TCastleButton would be impossible:
    // reading Name would set Caption.
    // During loading, we assume that all component properties are to be deserialized from file,
    // and InternalText should not be automatically modified.
    (not IsLoading) and
    (Name = InternalText) and
    // Do not update InternalText when Owner is during deserialization.
    ( (Owner = nil) or
      (not (Owner is TCastleComponent)) or
      (not TCastleComponent(Owner).IsLoading));
  // Note that this can raise exception is Value is not a valid name.
  inherited SetName(Value);
  if ChangeInternalText then
    InternalText := Value;
end;

function TCastleComponent.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'Name') then
    Result := [psBasic]
  else
    Result := [];
end;

procedure TCastleComponent.TranslateProperties(
  const TranslatePropertyEvent: TTranslatePropertyEvent);
begin
  // nothing to do in this class
end;

function TCastleComponent.NonVisualComponentsCount: Integer;
begin
  if FNonVisualComponents = nil then
    Result := 0
  else
    Result := FNonVisualComponents.Count;
end;

function TCastleComponent.GetNonVisualComponents(const Index: Integer): TComponent;
begin
  { showing ERangeError will be nicer
    than showing EAccessViolation when accessing FNonVisualComponents below }
  if FNonVisualComponents = nil then
    System.Error(reRangeError);
  Result := FNonVisualComponents[Index];
end;

procedure TCastleComponent.AddNonVisualComponent(const NonVisualComponent: TComponent);
begin
  // create FNonVisualComponents on-demand, to not burden typical TCastleComponent that doesn't need this
  if FNonVisualComponents = nil then
    FNonVisualComponents := TComponentList.Create(false);
  FNonVisualComponents.Add(NonVisualComponent);
end;

procedure TCastleComponent.InsertNonVisualComponent(const Index: Integer; const NonVisualComponent: TComponent);
begin
  // create FNonVisualComponents on-demand, to not burden typical TCastleComponent that doesn't need this
  if FNonVisualComponents = nil then
    FNonVisualComponents := TComponentList.Create(false);
  FNonVisualComponents.Insert(Index, NonVisualComponent);
end;

function TCastleComponent.NonVisualComponentsIndexOf(const NonVisualComponent: TComponent): Integer;
begin
  if FNonVisualComponents <> nil then
    Result := FNonVisualComponents.IndexOf(NonVisualComponent)
  else
    Result := -1;
end;

procedure TCastleComponent.RemoveNonVisualComponent(const NonVisualComponent: TComponent);
begin
  if FNonVisualComponents <> nil then
    FNonVisualComponents.Remove(NonVisualComponent);
end;

function TCastleComponent.NonVisualComponentsEnumerate: TNonVisualComponentsEnumerator;
begin
  Result := TNonVisualComponentsEnumerator.Create(Self);
end;

procedure TCastleComponent.CustomSerialization(const SerializationProcess: TSerializationProcess);
begin
  inherited;
  SerializationProcess.ReadWriteList('NonVisualComponents',
    {$ifdef FPC}@{$endif} SerializeNonVisualComponentsEnumerate,
    {$ifdef FPC}@{$endif} SerializeNonVisualComponentsAdd,
    {$ifdef FPC}@{$endif} SerializeNonVisualComponentsClear);
end;

procedure TCastleComponent.SerializeNonVisualComponentsEnumerate(const Proc: TGetChildProc);
var
  I: Integer;
begin
  if FNonVisualComponents <> nil then
    for I := 0 to FNonVisualComponents.Count - 1 do
      if FNonVisualComponents[I].ComponentStyle * [csSubComponent, csTransient] = [] then
        Proc(FNonVisualComponents[I]);
end;

procedure TCastleComponent.SerializeNonVisualComponentsAdd(const C: TComponent);
begin
  AddNonVisualComponent(C);
end;

procedure TCastleComponent.SerializeNonVisualComponentsClear;
var
  I: Integer;
begin
  if FNonVisualComponents <> nil then
    for I := FNonVisualComponents.Count - 1 downto 0 do // downto, as list may shrink during loop
      if FNonVisualComponents[I].ComponentStyle * [csSubComponent, csTransient] = [] then
        FNonVisualComponents[I].Free; // will remove itself from Behaviors list
end;

{ TComponent routines -------------------------------------------------------- }

type
  { Helper class to implement TranslateProperties.

    Calls TranslatePropertyEvent on all "children", where "children" are all
    components that this class enumerated in CustomSerialization:

    - all items on lists reported by ReadWriteList

    - all subcomponents reported by ReadWriteSubComponent
  }
  TTranslatePropertiesOnChildren = class(TSerializationProcess)
  strict private
    procedure TranslatePropertiesOnChild(Child: TComponent);
  public
    TranslatePropertyEvent: TTranslatePropertyEvent;
    procedure ReadWriteInteger(const Key: String; var Value: Integer; const IsStored: Boolean);
      overload; override;
    procedure ReadWriteBoolean(const Key: String; var Value: Boolean; const IsStored: Boolean);
      overload; override;
    procedure ReadWriteString(const Key: String; var Value: String; const IsStored: Boolean);
      overload; override;
    procedure ReadWriteSingle(const Key: String; var Value: Single; const IsStored: Boolean);
      overload; override;
    procedure ReadWriteSubComponent(const Key: String; const Value: TComponent;
      const IsStored: Boolean); override;
    procedure ReadWriteList(const Key: String;
      const ListEnumerate: TSerializationProcess.TListEnumerateEvent;
      const ListAdd: TSerializationProcess.TListAddEvent;
      const ListClear: TSerializationProcess.TListClearEvent); override;
  end;

procedure TTranslatePropertiesOnChildren.TranslatePropertiesOnChild(Child: TComponent);
begin
  TranslateProperties(Child, TranslatePropertyEvent);
end;

procedure TTranslatePropertiesOnChildren.ReadWriteList(const Key: String;
  const ListEnumerate: TSerializationProcess.TListEnumerateEvent;
  const ListAdd: TSerializationProcess.TListAddEvent;
  const ListClear: TSerializationProcess.TListClearEvent);
begin
  ListEnumerate({$ifdef FPC}@{$endif} TranslatePropertiesOnChild);
end;

procedure TTranslatePropertiesOnChildren.ReadWriteSubComponent(
  const Key: String; const Value: TComponent; const IsStored: Boolean);
begin
  TranslateProperties(Value, TranslatePropertyEvent);
end;

procedure TTranslatePropertiesOnChildren.ReadWriteInteger(const Key: String; var Value: Integer; const IsStored: Boolean);
begin
  // just override abstract method to do nothing
end;

procedure TTranslatePropertiesOnChildren.ReadWriteBoolean(const Key: String; var Value: Boolean; const IsStored: Boolean);
begin
  // just override abstract method to do nothing
end;

procedure TTranslatePropertiesOnChildren.ReadWriteString(const Key: String; var Value: String; const IsStored: Boolean);
begin
  // just override abstract method to do nothing
end;

procedure TTranslatePropertiesOnChildren.ReadWriteSingle(const Key: String; var Value: Single; const IsStored: Boolean);
begin
  // just override abstract method to do nothing
end;

procedure TranslateProperties(const C: TComponent;
  const TranslatePropertyEvent: TTranslatePropertyEvent);

  { Call TranslateProperties on C and its children. }
  procedure TranslateChildClass(const C: TObject);
  begin
    if (C is TComponent) and // also checks is C <> nil
       (csSubComponent in TComponent(C).ComponentStyle) then
      TranslateProperties(TComponent(C), TranslatePropertyEvent);

    { One day we may add here special handling
      of TCollection and TObjectList, like TJSONStreamer.StreamClassProperty. }
  end;

{$ifdef FPC}
var
  PropInfos: TPropInfoList;
  PropInfo: PPropInfo;
  TypeInfo: PTypeInfo;
  I: Integer;
  ChildrenEnumerator: TTranslatePropertiesOnChildren;
begin
  if C is TCastleComponent then
  begin
    // translate properties of C
    TCastleComponent(C).TranslateProperties(TranslatePropertyEvent);

    // translate properties of C children
    ChildrenEnumerator := TTranslatePropertiesOnChildren.Create;
    try
      ChildrenEnumerator.TranslatePropertyEvent := TranslatePropertyEvent;
      TCastleComponent(C).CustomSerialization(ChildrenEnumerator);
    finally FreeAndNil(ChildrenEnumerator) end;
  end;

  // translate properties of other C serialized children
  PropInfos := TPropInfoList.Create(C, tkProperties);
  try
    for I := 0 to PropInfos.Count - 1 do
    begin
      PropInfo := PropInfos[I];
      TypeInfo := PropInfo^.PropType {$ifndef FPC}^{$endif};
      if TypeInfo^.Kind = tkClass then
        TranslateChildClass(GetObjectProp(C, PropInfo));
    end;
  finally FreeAndNil(PropInfos) end;
{$else}
begin
  WritelnWarning('Localization (TranslateProperties) not implemented for Delphi yet.');
{$endif}
end;

{ initialization / finalization ---------------------------------------------- }

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
end;

procedure FiniStdStreams;
begin
  FreeAndNil(StdInStream);
  FreeAndNil(StdOutStream);
  FreeAndNil(StdErrStream);
end;

{ TCastleObjectStack ------------------------------------------------------------ }

function TCastleObjectStack.GetCapacity: TListSize;
begin
  Result := List.Capacity;
end;

procedure TCastleObjectStack.SetCapacity(const Value: TListSize);
begin
  List.Capacity := Value;
end;

{ TCastleObjectQueue ------------------------------------------------------------ }

function TCastleObjectQueue.GetCapacity: TListSize;
begin
  Result := List.Capacity;
end;

procedure TCastleObjectQueue.SetCapacity(const Value: TListSize);
begin
  List.Capacity := Value;
end;

{ TCastleObjectList ------------------------------------------------------------- }

constructor TCastleObjectList.CreateFromArray(const FreeObjects: boolean;
  const AItems: array of TObject);
begin
  Create(FreeObjects);
  AddRange(AItems);
end;

procedure TCastleObjectList.AddRange(const A: array of TObject);
var
  I: Integer;
begin
  Capacity := Capacity + High(A) + 1;
  for I := 0 to High(A) do
    Add(A[I]);
end;

procedure TCastleObjectList.AddArray(const A: array of TObject);
begin
  AddRange(A);
end;

procedure TCastleObjectList.AddRange(AList: Contnrs.TObjectList);
{$ifdef FPC}
begin
  inherited AddList(AList);
{$else}
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
    Add(AList[I]);
{$endif}
end;

procedure TCastleObjectList.AddList(AList: Contnrs.TObjectList);
begin
  AddRange(AList);
end;

function TCastleObjectList.MakeSingle(ReplaceClass: TClass; NewItem: TObject;
  AddEnd: boolean): TObject;
var
  I: Integer;
begin
  if NewItem = nil then
  begin
    Result := Extract(ReplaceClass);
    Exit;
  end;

  for I := 0 to Count - 1 do
    if Items[I] is ReplaceClass then
    begin
      Result := Extract(I);
      Insert(I, NewItem);
      Exit;
    end;

  Result := nil;
  if AddEnd then
    Insert(Count, NewItem)
  else
    Insert(0, NewItem);
end;

function TCastleObjectList.Extract(Index: Integer): TObject;
{$ifdef FPC}
begin
  Result := TObject(List^[Index]);

  { Set to nil and then delete by index. This is a hack to prevent
    TList implementation from making any notification about Result
    delete/extraction. }
  TObject(List^[Index]) := nil;
  Delete(Index);

  if Assigned(Result) then
    Notify(Result, lnExtracted);
{$else}
var
  OldOwnsObjects: boolean;
begin
  OldOwnsObjects := OwnsObjects;
  OwnsObjects := false;
  Result := Items[Index];
  Delete(Index);
  OwnsObjects := OldOwnsObjects;
{$endif}
end;

function TCastleObjectList.Extract(RemoveClass: TClass): TObject;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I] is RemoveClass then
    begin
      Result := Extract(I);
      Exit;
    end;

  Result := nil;
end;

function TCastleObjectList.RemoveAll(Item: TObject): Cardinal;
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

function TCastleObjectList.IsFirst(Value: TObject): boolean;
begin
  Result := (Count > 0) and (Items[0] = Value);
end;

function TCastleObjectList.IsLast(Value: TObject): boolean;
begin
  Result := (Count > 0) and (Items[Count - 1] = Value);
end;

procedure TCastleObjectList.InsertIfNotExists(Index: Integer; Value: TObject);
begin
  if IndexOf(Value) = -1 then
    Insert(Index, Value);
end;

procedure TCastleObjectList.AddIfNotExists(Value: TObject);
begin
  if IndexOf(Value) = -1 then
    Add(Value);
end;

{ TNotifyEventList  ------------------------------------------------------ }

procedure TNotifyEventList.ExecuteAll(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do

    { TODO: The test "I < Count" is a quick fix for the problem that list
      may be modified during iteration.
      E.g. network/remote_logging/gameloghandler.pas in HttpPostFinish
      sets FreeSender, which means that Application.OnUpdate list
      is modified while we iterate over it.

      We should introduce a reliable way to handle this, but for now the test
      at least prevents a crash in this case. }

    if (I < Count) and Assigned(Items[I]) then
      Items[I](Sender);
end;

procedure TNotifyEventList.ExecuteForward(Sender: TObject);
begin
  ExecuteAll(Sender);
end;

procedure TNotifyEventList.ExecuteBackward(Sender: TObject);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do

    { TODO: The test "I < Count" is a quick fix for the problem that when
      TCastleApplicationProperties._GLContextClose calls
      FOnGLContextCloseObject.ExecuteBackward(Self),
      some "on close" callbacks modify the FOnGLContextCloseObject list.

      We should introduce a reliable way to handle this, but for now the test
      at least prevents a crash in this case. }

    if (I < Count) and Assigned(Items[I]) then
      Items[I](Sender);
end;

{ DumpStack ------------------------------------------------------------------ }

{$ifdef FPC}
function DumpStackToString(const BaseFramePointer: Pointer): string;
var
  TextFile: Text;
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create('');
  try
    AssignStream(TextFile, StringStream);
    Rewrite(TextFile);
    try
      Dump_Stack(TextFile, BaseFramePointer);
    finally CloseFile(TextFile) end;
    Result := StringStream.DataString;
  finally FreeAndNil(StringStream) end;
end;

function DumpExceptionBackTraceToString: string;
{$ifdef CASTLE_NINTENDO_SWITCH}
begin
  Result := ''; // DumpExceptionBackTrace fails with Access Violation
{$else}
var
  TextFile: Text;
  StringStream: TStringStream;
begin
  {$ifdef VER3_3} {$define CASTLE_SECURE_BACKTRACE} {$endif}
  {$ifdef CASTLE_SECURE_BACKTRACE}
  try
  {$endif}

  StringStream := TStringStream.Create('');
  try
    AssignStream(TextFile, StringStream);
    Rewrite(TextFile);
    try
      DumpExceptionBackTrace(TextFile);
    finally CloseFile(TextFile) end;
    Result := StringStream.DataString;
  finally FreeAndNil(StringStream) end;

  {$ifdef CASTLE_SECURE_BACKTRACE}
  except
    // TODO: investigate and report, reproducible by running play_animation with non-existent data.
    // WritelnWarning('Capturing backtrace failed, this is known to happen with some FPC 3.3.1 versions.');
    // Cannot log this problem -- as logging itself could use backtrace, causing infinite loop...
    Result := '';
  end;
  {$endif}
{$endif}
end;
{$endif}

function InternalProposeName(const ComponentClass: TComponentClass;
  const ComponentsOwner: TComponent): String;

  { Cleanup S (right now, always taken from some ClassName)
    to be a nice component name, which also must make it a valid Pascal identifier. }
  function CleanComponentName(const S: String): String;
  begin
    Result := S;

    // remove common prefixes
    if IsPrefix('TCastleUserInterface', Result, true) then
      Result := PrefixRemove('TCastleUserInterface', Result, true)
    else
    if IsPrefix('TCastle', Result, true) then
      Result := PrefixRemove('TCastle', Result, true)
    else
    if IsPrefix('T', Result, true) then
      Result := PrefixRemove('T', Result, true);

    // move 2D and 3D to the back, as component name cannot start with a number
    if IsPrefix('2D', Result, true) then
      Result := PrefixRemove('2D', Result, true) + '2D';
    if IsPrefix('3D', Result, true) then
      Result := PrefixRemove('3D', Result, true) + '3D';

    // in case the replacements above made '', fix it (can happen in case of TCastleUserInterface)
    if Result = '' then
      Result := 'Group';

    if SCharIs(Result, 1, ['0'..'9']) then
      Result := 'Component' + Result;
  end;

var
  ResultBase: String;
  I: Integer;
begin
  ResultBase := CleanComponentName(ComponentClass.ClassName);

  { A simple test of the CleanComponentName routine.
    This is *not* a good place for such automated test, but for now it was simplest to put it here. }
  {
  Assert(CleanComponentName('TSomething') = 'Something');
  Assert(CleanComponentName('TCastleUserInterface') = 'Group');
  Assert(CleanComponentName('TCastleUserInterfaceButton') = 'Button');
  Assert(CleanComponentName('TCastleSomething') = 'Something');
  Assert(CleanComponentName('TCastle2DStuff') = 'Stuff2D');
  Assert(CleanComponentName('TCastle3DStuff') = 'Stuff3D');
  Assert(CleanComponentName('TCastle4DProcessing') = 'Component4DProcessing');
  }

  // make unique
  I := 1;
  Result := ResultBase + IntToStr(I);
  while ComponentsOwner.FindComponent(Result) <> nil do
  begin
    Inc(I);
    Result := ResultBase + IntToStr(I);
  end;
end;

{ TFreeNotificationObserver -------------------------------------------------- }

destructor TFreeNotificationObserver.Destroy;
begin
  { detach free notification, if any remains }
  Observed := nil;
  inherited;
end;

procedure TFreeNotificationObserver.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  { Note: initially I tried implementing this class without tracking FObserved,
    instead user needed to call
      Xxx.FreeNotification(MyFreeNotificationObserver),
      Xxx.RemoveFreeNotification(MyFreeNotificationObserver)
    explicitly. But this was bad: even on TFreeNotificationObserver,
    we also get notification about freeing of some unrelated components
    (TCustomHintAction, THintWindow, TCustomTimer...) when using LCL.

    Using Observed property also makes usage simpler:
    - No need to use RemoveFreeNotification and FreeNotification.
      Instead just set Observed.
    - It is obvious that this component observes only *one* component at a time.
      (Since this is its purpose). }

  if (Operation = opRemove) and
     (AComponent <> nil) and // should not happen, but better be secure
     (AComponent = FObserved) and
     Assigned(OnFreeNotification) then
  begin
    OnFreeNotification(Self);

    { Possibly OnFreeNotification already changed Observed, and we no longer observe it.
      But if it didn't... then let's fix it.
      We know FObserved will be freed, so we can stop watching. }
    if AComponent = FObserved then
      Observed := nil;
  end;
end;

procedure TFreeNotificationObserver.SetObserved(const Value: TComponent);
begin
  if FObserved <> Value then
  begin
    if FObserved <> nil then
      FObserved.RemoveFreeNotification(Self);
    FObserved := Value;
    if FObserved <> nil then
      FObserved.FreeNotification(Self);
  end;
end;

initialization
  InitStdStreams;
finalization
  FiniStdStreams;
end.
