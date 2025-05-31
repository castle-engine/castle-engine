{%MainUnit castledownload.pas}
{
  Copyright 2013-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

type
  { Common class for reading or writing a stream like a text file. }
  TCastleTextReaderWriter = class
  private
    FOwnsStream: boolean;
    FStream: TStream;
  public
    { Open a stream. If AOwnsStream then in destructor we will free
      given AStream object. }
    constructor Create(AStream: TStream; AOwnsStream: boolean); overload;
    destructor Destroy; override;
  end;

  { Read any stream like a text file. You can read an arbitrary TStream
    instance, or you can read an URL. Reading from URL supports
    all URL protocols supportted by the Castle Game Engine engine
    (see https://castle-engine.io/url ).

    Includes comfortable @link(Readln) routine to read line by line
    (lines may be terminated in any OS convention).
    Includes comfortable @link(Read) to read next non-whitespace
    characters, @link(ReadInteger) to read next integer and such.

    Do not use the underlying stream once you started reading it with
    this class. We will move the position within this stream ourselves. }
  TCastleTextReader = class(TCastleTextReaderWriter)
  private
    { For now, Stream.Read call assumes that ReadBuf is 8-bit.
      Changing ReadBuf to String on Delphi would break e.g. Spine atlas reading,
      TCastleTextReader.Readln would be garbage. }
    ReadBuf: AnsiString;
    { Try to read more data from underlying stream and add it to ReadBuf.
      Returns if we succeded, @false means that the stream ends.
      When it returns @true, you can be sure that Length(ReadBuf) increased. }
    function IncreaseReadBuf: boolean;
  public
    { Download and open a file. }
    constructor Create(const Url: String); overload;

    { Read next line from Stream. Returned string does not contain
      any end-of-line characters. }
    function Readln: string;

    { Read the next string of non-whitespace characters.
      This skips any whitespace (including newlines) we currently see,
      then reads all non-whitespace characters as far as it can.
      It does not consume any whitespace characters after the string.

      Returns empty string if and only if the stream ended.
      Otherwise, returns the read non-whitespace characters. }
    function Read: string;

    { Read the next Integer from stream. Reads next string of non-whitespace
      characters, like @link(Read), and then converts it to Integer.

       @raises(EConvertError If the next non-whitespace string
         cannot be converted to Integer. This includes situations
         when stream ended (@link(Read) would return empty string in this
         case).)  }
    function ReadInteger: Integer;

    { Read the next Single value from stream.
      Reads next string of non-whitespace
      characters, like @link(Read), and then converts it to Single.

       @raises(EConvertError If the next non-whitespace string
         cannot be converted to Single. This includes situations
         when stream ended (@link(Read) would return empty string in this
         case).)  }
    function ReadSingle: Single;

    { Read the next vector from a stream, simply reading 3 Single values
      in sequence.

       @raises(EConvertError If one of the components cannot be converted
         to Single, or when stream ended prematurely.) }
    function ReadVector3: TVector3;
    function ReadVector3Single: TVector3; deprecated 'use ReadVector3';

    function Eof: boolean;
  end;

  { Write to a stream like to a text file.

    You can write to an arbitrary TStream instance,
    or you can write to an URL. Writing to an URL supports all
    URL protocols supportted by @link(UrlSaveStream), which for now
    doesn't include much: only @code(file) protocol. But it will produce
    a nice exception message in case of unsupprted URL protocol. }
  TCastleTextWriter = class(TCastleTextReaderWriter)
  public
    constructor Create(const Url: String); overload;
    procedure Write(const S: string); overload;
    procedure Write(const S: string; const Args: array of const); overload;
    procedure Writeln(const S: string = ''); overload;
    procedure Writeln(const S: string; const Args: array of const); overload;
  end;

  TTextReaderWriter = TCastleTextReaderWriter deprecated 'use TCastleTextReaderWriter instead';
  TTextReader = TCastleTextReader deprecated 'use TCastleTextReader instead';
  TTextWriter = TCastleTextWriter deprecated 'use TCastleTextWriter instead';

{$endif}

{$ifdef read_implementation}

{ TCastleTextReaderWriter ---------------------------------------------------------- }

constructor TCastleTextReaderWriter.Create(AStream: TStream; AOwnsStream: boolean);
begin
  inherited Create;
  FStream := Astream;
  FOwnsStream := AOwnsStream;
end;

destructor TCastleTextReaderWriter.Destroy;
begin
  if FOwnsStream then FStream.Free;
  inherited;
end;

{ TCastleTextReader ---------------------------------------------------------------- }

constructor TCastleTextReader.Create(const Url: String);
begin
  inherited Create(Download(Url), true);
end;

function TCastleTextReader.IncreaseReadBuf: boolean;
const
  BufferIncrease = 100;
var
  ReadCnt: Integer;
begin
  SetLength(ReadBuf, Length(ReadBuf) + BufferIncrease);
  ReadCnt := FStream.Read(ReadBuf[Length(ReadBuf) - BufferIncrease + 1], BufferIncrease);
  SetLength(ReadBuf, Length(ReadBuf) - BufferIncrease + ReadCnt);
  Result := ReadCnt <> 0;
end;

function TCastleTextReader.Readln: string;
var
  I, DeleteCount: integer;
begin
  I := 1;

  { Note that ReadBuf may contain data that we
    already read from stream at some time but did not returned it to
    user of this class
    (because we realized we have read too much). }

  repeat
    if (I > Length(ReadBuf)) and not IncreaseReadBuf then
    begin
      Result := ReadBuf;
      ReadBuf := '';
      Exit;
    end;

    if CharInSet(ReadBuf[I], [#10, #13]) then
    begin
      Result := Copy(ReadBuf, 1, I - 1);
      DeleteCount := I;

      { If this is followed by 2nd newline character, we want to consume it.
        To do this, we may have to increase ReadBuf. }
      if ( (I < Length(ReadBuf)) or IncreaseReadBuf ) and
         { check we have #13 followed by #10 or #10 followed by #13.
           Be careful to *not* eat #10 followed by #10, as that would
           make us silently consume empty lines in files with Unix line ending. }
         ( ( (ReadBuf[I] = #10) and (ReadBuf[I + 1] = #13) ) or
           ( (ReadBuf[I] = #13) and (ReadBuf[I + 1] = #10) ) ) then
        Inc(DeleteCount);

      Delete(ReadBuf, 1, DeleteCount);
      Exit;
    end;

    Inc(I);
  until false;
end;

function TCastleTextReader.Read: string;
var
  Start, I: integer;
begin
  I := 1;

  repeat
    if (I > Length(ReadBuf)) and not IncreaseReadBuf then
      Exit('');
    if not CharInSet(ReadBuf[I], WhiteSpaces) then
      Break;
    Inc(I);
  until false;

  Start := I;

  repeat
    if (I > Length(ReadBuf)) and not IncreaseReadBuf then
      Break;
    if CharInSet(ReadBuf[I], WhiteSpaces) then
      Break;
    Inc(I);
  until false;

  Dec(I); { we know we're 1 position too far now }
  Assert(I > 0);
  Result := CopyPos(ReadBuf, Start, I);
  Delete(ReadBuf, 1, I);
end;

function TCastleTextReader.ReadInteger: Integer;
begin
  Result := StrToInt(Read);
end;

function TCastleTextReader.ReadSingle: Single;
begin
  Result := StrToFloatDot(Read);
end;

function TCastleTextReader.ReadVector3: TVector3;
begin
  Result.X := ReadSingle;
  Result.Y := ReadSingle;
  Result.Z := ReadSingle;
end;

function TCastleTextReader.ReadVector3Single: TVector3;
begin
  Result := ReadVector3;
end;

function TCastleTextReader.Eof: boolean;
begin
  Result := (ReadBuf = '') and not IncreaseReadBuf;
end;

{ TCastleTextWriter ---------------------------------------------------------------- }

constructor TCastleTextWriter.Create(const Url: String);
begin
  inherited Create(UrlSaveStream(Url), true);
end;

procedure TCastleTextWriter.Write(const S: string);
begin
  WriteStr(FStream, S);
end;

procedure TCastleTextWriter.Writeln(const S: string);
begin
  WritelnStr(FStream, S);
end;

procedure TCastleTextWriter.Write(const S: string; const Args: array of const);
begin
  WriteStr(FStream, Format(S, Args));
end;

procedure TCastleTextWriter.Writeln(const S: string; const Args: array of const);
begin
  WritelnStr(FStream, Format(S, Args));
end;

{$endif}
