{
  CSV Parser, Builder classes.
  Version 0.5 2014-10-25

  Copyright (C) 2010-2014 Vladimir Zhirov <vvzh.home@gmail.com>

  Contributors:
    Luiz Americo Pereira Camara
    Mattias Gaertner
    Reinier Olislagers

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit csvreadwrite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils;

Type
  TCSVChar = Char;

  { TCSVHandler }

  TCSVHandler = class(TPersistent)
  private
    procedure SetDelimiter(const AValue: TCSVChar);
    procedure SetQuoteChar(const AValue: TCSVChar);
    procedure UpdateCachedChars;
  protected
    // special chars
    FDelimiter: TCSVChar;
    FQuoteChar: TCSVChar;
    FLineEnding: String;
    // cached values to speed up special chars operations
    FSpecialChars: TSysCharSet;
    FDoubleQuote: String;
    // parser settings
    FIgnoreOuterWhitespace: Boolean;
    // builder settings
    FQuoteOuterWhitespace: Boolean;
    // document settings
    FEqualColCountPerRow: Boolean;
  public
    constructor Create; virtual;
    procedure Assign(ASource: TPersistent); override;
    procedure AssignCSVProperties(ASource: TCSVHandler);
    // Delimiter that separates the field, e.g. comma, semicolon, tab
    property Delimiter: TCSVChar read FDelimiter write SetDelimiter;
    // Character used to quote "problematic" data
    // (e.g. with delimiters or spaces in them)
    // A common quotechar is "
    property QuoteChar: TCSVChar read FQuoteChar write SetQuoteChar;
    // String at the end of the line of data (e.g. CRLF)
    property LineEnding: String read FLineEnding write FLineEnding;
    // Ignore whitespace between delimiters and field data
    property IgnoreOuterWhitespace: Boolean read FIgnoreOuterWhitespace write FIgnoreOuterWhitespace;
    // Use quotes when outer whitespace is found
    property QuoteOuterWhitespace: Boolean read FQuoteOuterWhitespace write FQuoteOuterWhitespace;
    // When reading and writing: make sure every line has the same column count, create empty cells in the end of row if required
    property EqualColCountPerRow: Boolean read FEqualColCountPerRow write FEqualColCountPerRow;
  end;

  // Sequential input from CSV stream

  { TCSVParser }

  TCSVParser = class(TCSVHandler)
  private
    FFreeStream: Boolean;
    // fields
    FSourceStream: TStream;
    FStrStreamWrapper: TStringStream;
    // parser state
    EndOfFile: Boolean;
    EndOfLine: Boolean;
    FCurrentChar: TCSVChar;
    FCurrentRow: Integer;
    FCurrentCol: Integer;
    FMaxColCount: Integer;
    // output buffers
    FCellBuffer: String;
    FWhitespaceBuffer: String;
    procedure ClearOutput;
    // basic parsing
    procedure SkipEndOfLine;
    procedure SkipDelimiter;
    procedure SkipWhitespace;
    procedure NextChar;
    // complex parsing
    procedure ParseCell;
    procedure ParseQuotedValue;
    // simple parsing
    procedure ParseValue;
  public
    constructor Create;
    destructor Destroy; override;
    // Source data stream
    procedure SetSource(AStream: TStream); overload;
    // Source data string.
    procedure SetSource(const AString: String); overload;
    // Rewind to beginning of data
    procedure ResetParser;
    // Read next cell data; return false if end of file reached
    function  ParseNextCell: Boolean;
    // Current row (0 based)
    property CurrentRow: Integer read FCurrentRow;
    // Current column (0 based); -1 if invalid/before beginning of file
    property CurrentCol: Integer read FCurrentCol;
    // Data in current cell
    property CurrentCellText: String read FCellBuffer;
    // The maximum number of columns found in the stream:
    property MaxColCount: Integer read FMaxColCount;
    // Does the parser own the stream ? If true, a previous stream is freed when set or when parser is destroyed.
    Property FreeStream : Boolean Read FFreeStream Write FFreeStream;
  end;

  // Sequential output to CSV stream
  TCSVBuilder = class(TCSVHandler)
  private
    FOutputStream: TStream;
    FDefaultOutput: TMemoryStream;
    FNeedLeadingDelimiter: Boolean;
    function GetDefaultOutputAsString: String;
  protected
    procedure AppendStringToStream(const AString: String; AStream: TStream);
    function  QuoteCSVString(const AValue: String): String;
  public
    constructor Create;
    destructor Destroy; override;
    // Set output/destination stream.
    // If not called, output is sent to DefaultOutput
    procedure SetOutput(AStream: TStream);
    // If using default stream, reset output to beginning.
    // If using user-defined stream, user should reposition stream himself
    procedure ResetBuilder;
    // Add a cell to the output with data AValue
    procedure AppendCell(const AValue: String);
    // Write end of row to the output, starting a new row
    procedure AppendRow;
    // Default output as memorystream (if output not set using SetOutput)
    property DefaultOutput: TMemoryStream read FDefaultOutput;
    // Default output in string format (if output not set using SetOutput)
    property DefaultOutputAsString: String read GetDefaultOutputAsString;
  end;

function ChangeLineEndings(const AString, ALineEnding: String): String;

implementation

const
  CsvCharSize = SizeOf(TCSVChar);
  CR    = #13;
  LF    = #10;
  HTAB  = #9;
  SPACE = #32;
  WhitespaceChars = [HTAB, SPACE];
  LineEndingChars = [CR, LF];

// The following implementation of ChangeLineEndings function originates from
// Lazarus CodeTools library by Mattias Gaertner. It was explicitly allowed
// by Mattias to relicense it under modified LGPL and include into CsvDocument.

function ChangeLineEndings(const AString, ALineEnding: String): String;
var
  I: Integer;
  Src: PChar;
  Dest: PChar;
  DestLength: Integer;
  EndingLength: Integer;
  EndPos: PChar;
begin
  if AString = '' then
    Exit(AString);
  EndingLength := Length(ALineEnding);
  DestLength := Length(AString);

  Src := PChar(AString);
  EndPos := Src + DestLength;
  while Src < EndPos do
  begin
    if (Src^ = CR) then
    begin
      Inc(Src);
      if (Src^ = LF) then
      begin
        Inc(Src);
        Inc(DestLength, EndingLength - 2);
      end else
        Inc(DestLength, EndingLength - 1);
    end else
    begin
      if (Src^ = LF) then
        Inc(DestLength, EndingLength - 1);
      Inc(Src);
    end;
  end;

  SetLength(Result, DestLength);
  Src := PChar(AString);
  Dest := PChar(Result);
  EndPos := Dest + DestLength;
  while (Dest < EndPos) do
  begin
    if Src^ in LineEndingChars then
    begin
      for I := 1 to EndingLength do
      begin
        Dest^ := ALineEnding[I];
        Inc(Dest);
      end;
      if (Src^ = CR) and (Src[1] = LF) then
        Inc(Src, 2)
      else
        Inc(Src);
    end else
    begin
      Dest^ := Src^;
      Inc(Src);
      Inc(Dest);
    end;
  end;
end;

{ TCSVHandler }

procedure TCSVHandler.SetDelimiter(const AValue: TCSVChar);
begin
  if FDelimiter <> AValue then
  begin
    FDelimiter := AValue;
    UpdateCachedChars;
  end;
end;

procedure TCSVHandler.SetQuoteChar(const AValue: TCSVChar);
begin
  if FQuoteChar <> AValue then
  begin
    FQuoteChar := AValue;
    UpdateCachedChars;
  end;
end;

procedure TCSVHandler.UpdateCachedChars;
begin
  FDoubleQuote := FQuoteChar + FQuoteChar;
  FSpecialChars := [CR, LF, FDelimiter, FQuoteChar];
end;

constructor TCSVHandler.Create;
begin
  inherited Create;
  FDelimiter := ',';
  FQuoteChar := '"';
  FLineEnding := sLineBreak;
  FIgnoreOuterWhitespace := False;
  FQuoteOuterWhitespace := True;
  FEqualColCountPerRow := True;
  UpdateCachedChars;
end;

procedure TCSVHandler.Assign(ASource: TPersistent);
begin
  if (ASource is TCSVHandler) then
    AssignCSVProperties(ASource as TCSVHandler)
  else
    inherited Assign(ASource);
end;

procedure TCSVHandler.AssignCSVProperties(ASource: TCSVHandler);
begin
  FDelimiter := ASource.FDelimiter;
  FQuoteChar := ASource.FQuoteChar;
  FLineEnding := ASource.FLineEnding;
  FIgnoreOuterWhitespace := ASource.FIgnoreOuterWhitespace;
  FQuoteOuterWhitespace := ASource.FQuoteOuterWhitespace;
  FEqualColCountPerRow := ASource.FEqualColCountPerRow;
  UpdateCachedChars;
end;

{ TCSVParser }

procedure TCSVParser.ClearOutput;
begin
  FCellBuffer := '';
  FWhitespaceBuffer := '';
  FCurrentRow := 0;
  FCurrentCol := -1;
  FMaxColCount := 0;
end;

procedure TCSVParser.SkipEndOfLine;
begin
  // treat LF+CR as two linebreaks, not one
  if (FCurrentChar = CR) then
    NextChar;
  if (FCurrentChar = LF) then
    NextChar;
end;

procedure TCSVParser.SkipDelimiter;
begin
  if FCurrentChar = FDelimiter then
    NextChar;
end;

procedure TCSVParser.SkipWhitespace;
begin
  while FCurrentChar = SPACE do
    NextChar;
end;

procedure TCSVParser.NextChar;
begin
  if FSourceStream.Read(FCurrentChar, CsvCharSize) < CsvCharSize then
  begin
    FCurrentChar := #0;
    EndOfFile := True;
  end;
  EndOfLine := FCurrentChar in LineEndingChars;
end;

procedure TCSVParser.ParseCell;
begin
  FCellBuffer := '';
  if FIgnoreOuterWhitespace then
    SkipWhitespace;
  if FCurrentChar = FQuoteChar then
    ParseQuotedValue
  else
    ParseValue;
end;

procedure TCSVParser.ParseQuotedValue;
var
  QuotationEnd: Boolean;
begin
  NextChar; // skip opening quotation char
  repeat
    // read value up to next quotation char
    while not ((FCurrentChar = FQuoteChar) or EndOfFile) do
    begin
      if EndOfLine then
      begin
        AppendStr(FCellBuffer, FLineEnding);
        SkipEndOfLine;
      end else
      begin
        AppendStr(FCellBuffer, FCurrentChar);
        NextChar;
      end;
    end;
    // skip quotation char (closing or escaping)
    if not EndOfFile then
      NextChar;
    // check if it was escaping
    if FCurrentChar = FQuoteChar then
    begin
      AppendStr(FCellBuffer, FCurrentChar);
      QuotationEnd := False;
      NextChar;
    end else
      QuotationEnd := True;
  until QuotationEnd;
  // read the rest of the value until separator or new line
  ParseValue;
end;

procedure TCSVParser.ParseValue;
begin
  while not ((FCurrentChar = FDelimiter) or EndOfLine or EndOfFile) do
  begin
    AppendStr(FWhitespaceBuffer, FCurrentChar);
    NextChar;
  end;
  // merge whitespace buffer
  if FIgnoreOuterWhitespace then
    RemoveTrailingChars(FWhitespaceBuffer, WhitespaceChars);
  AppendStr(FCellBuffer, FWhitespaceBuffer);
  FWhitespaceBuffer := '';
end;

constructor TCSVParser.Create;
begin
  inherited Create;
  ClearOutput;
  FStrStreamWrapper := nil;
  EndOfFile := True;
end;

destructor TCSVParser.Destroy;
begin
  if FFreeStream and (FSourceStream<>FStrStreamWrapper) then
     FreeAndNil(FSourceStream);
  FreeAndNil(FStrStreamWrapper);
  inherited Destroy;
end;

procedure TCSVParser.SetSource(AStream: TStream);
begin
  If FSourceStream=AStream then exit;
  if FFreeStream and (FSourceStream<>FStrStreamWrapper) then
     FreeAndNil(FSourceStream);
  FSourceStream := AStream;
  ResetParser;
end;

procedure TCSVParser.SetSource(const AString: String); overload;
begin
  FreeAndNil(FStrStreamWrapper);
  FStrStreamWrapper := TStringStream.Create(AString);
  SetSource(FStrStreamWrapper);
end;

procedure TCSVParser.ResetParser;
begin
  ClearOutput;
  FSourceStream.Seek(0, soFromBeginning);
  EndOfFile := False;
  NextChar;
end;

// Parses next cell; returns True if there are more cells in the input stream.
function TCSVParser.ParseNextCell: Boolean;
var
  LineColCount: Integer;
begin
  if EndOfLine or EndOfFile then
  begin
    // Having read the previous line, adjust column count if necessary:
    LineColCount := FCurrentCol + 1;
    if LineColCount > FMaxColCount then
      FMaxColCount := LineColCount;
  end;

  if EndOfFile then
    Exit(False);

  // Handle line ending
  if EndOfLine then
  begin
    SkipEndOfLine;
    if EndOfFile then
      Exit(False);
    FCurrentCol := 0;
    Inc(FCurrentRow);
  end else
    Inc(FCurrentCol);

  // Skipping a delimiter should be immediately followed by parsing a cell
  // without checking for line break first, otherwise we miss last empty cell.
  // But 0th cell does not start with delimiter unlike other cells, so
  // the following check is required not to miss the first empty cell:
  if FCurrentCol > 0 then
    SkipDelimiter;
  ParseCell;
  Result := True;
end;

{ TCSVBuilder }

function TCSVBuilder.GetDefaultOutputAsString: String;
var
  StreamSize: Integer;
begin
  Result := '';
  StreamSize := FDefaultOutput.Size;
  if StreamSize > 0 then
  begin
    SetLength(Result, StreamSize);
    FDefaultOutput.ReadBuffer(Result[1], StreamSize);
  end;
end;

procedure TCSVBuilder.AppendStringToStream(const AString: String; AStream: TStream);
var
  StrLen: Integer;
begin
  StrLen := Length(AString);
  if StrLen > 0 then
    AStream.WriteBuffer(AString[1], StrLen);
end;

function TCSVBuilder.QuoteCSVString(const AValue: String): String;
var
  I: Integer;
  ValueLen: Integer;
  NeedQuotation: Boolean;
begin
  ValueLen := Length(AValue);

  NeedQuotation := (AValue <> '') and FQuoteOuterWhitespace
    and ((AValue[1] in WhitespaceChars) or (AValue[ValueLen] in WhitespaceChars));

  if not NeedQuotation then
    for I := 1 to ValueLen do
    begin
      if AValue[I] in FSpecialChars then
      begin
        NeedQuotation := True;
        Break;
      end;
    end;

  if NeedQuotation then
  begin
    // double existing quotes
    Result := FDoubleQuote;
    Insert(StringReplace(AValue, FQuoteChar, FDoubleQuote, [rfReplaceAll]),
      Result, 2);
  end else
    Result := AValue;
end;

constructor TCSVBuilder.Create;
begin
  inherited Create;
  FDefaultOutput := TMemoryStream.Create;
  FOutputStream := FDefaultOutput;
end;

destructor TCSVBuilder.Destroy;
begin
  FreeAndNil(FDefaultOutput);
  inherited Destroy;
end;

procedure TCSVBuilder.SetOutput(AStream: TStream);
begin
  if Assigned(AStream) then
    FOutputStream := AStream
  else
    FOutputStream := FDefaultOutput;

  ResetBuilder;
end;

procedure TCSVBuilder.ResetBuilder;
begin
  if FOutputStream = FDefaultOutput then
    FDefaultOutput.Clear;

  // Do not clear external FOutputStream because it may be pipe stream
  // or something else that does not support size and position.
  // To clear external output is up to the user of TCSVBuilder.

  FNeedLeadingDelimiter := False;
end;

procedure TCSVBuilder.AppendCell(const AValue: String);
var
  CellValue: String;
begin
  if FNeedLeadingDelimiter then
    FOutputStream.WriteBuffer(FDelimiter, CsvCharSize);

  CellValue := ChangeLineEndings(AValue, FLineEnding);
  CellValue := QuoteCSVString(CellValue);
  AppendStringToStream(CellValue, FOutputStream);

  FNeedLeadingDelimiter := True;
end;

procedure TCSVBuilder.AppendRow;
begin
  AppendStringToStream(FLineEnding, FOutputStream);
  FNeedLeadingDelimiter := False;
end;

end.

