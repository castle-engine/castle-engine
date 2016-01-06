{
  CSV  Document classes.
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

unit csvdocument;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, Contnrs, csvreadwrite;

type
  TCSVChar = csvreadwrite.TCSVChar;
  TCSVParser = csvreadwrite.TCSVParser;
  TCSVBuilder = csvreadwrite.TCSVBuilder;

  {$IFNDEF FPC}
  TFPObjectList = TObjectList;
  {$ENDIF}

  // Random access to CSV document. Reads entire document into memory.
  TCSVDocument = class(TCSVHandler)
  private
    FRows: TFPObjectList;
    FParser: TCSVParser;
    FBuilder: TCSVBuilder;
    // helpers
    procedure ForceRowIndex(ARowIndex: Integer);
    function  CreateNewRow(const AFirstCell: String = ''): TObject;
    // property getters/setters
    function  GetCell(ACol, ARow: Integer): String;
    procedure SetCell(ACol, ARow: Integer; const AValue: String);
    function  GetCSVText: String;
    procedure SetCSVText(const AValue: String);
    function  GetRowCount: Integer;
    function  GetColCount(ARow: Integer): Integer;
    function  GetMaxColCount: Integer;
  public
    constructor Create;
    destructor  Destroy; override;

    // Input/output

    // Load document from file AFileName
    procedure LoadFromFile(const AFilename: String);
    // Load document from stream AStream
    procedure LoadFromStream(AStream: TStream);
    // Save document to file AFilename
    procedure SaveToFile(const AFilename: String);
    // Save document to stream AStream
    procedure SaveToStream(AStream: TStream);

    // Row and cell operations

    // Add a new row and a cell with content AFirstCell
    procedure AddRow(const AFirstCell: String = '');
    // Add a cell at row ARow with data AValue
    procedure AddCell(ARow: Integer; const AValue: String = '');
    // Insert a row at row ARow with first cell data AFirstCell
    // If there is no row ARow, insert row at end
    procedure InsertRow(ARow: Integer; const AFirstCell: String = '');
    // Insert a cell at specified position with data AValue
    procedure InsertCell(ACol, ARow: Integer; const AValue: String = '');
    // Remove specified row
    procedure RemoveRow(ARow: Integer);
    // Remove specified cell
    procedure RemoveCell(ACol, ARow: Integer);
    // Indicates if there is a row at specified position
    function  HasRow(ARow: Integer): Boolean;
    // Indicates if there is a cell at specified position
    function  HasCell(ACol, ARow: Integer): Boolean;
    
    // Search
    
    // Return column for cell data AString at row ARow
    function  IndexOfCol(const AString: String; ARow: Integer): Integer;
    // Return row for cell data AString at coloumn ACol
    function  IndexOfRow(const AString: String; ACol: Integer): Integer;

    // Utils

    // Remove all data
    procedure Clear;
    // Copy entire row ARow to row position AInsertPos.
    // Adds empty rows if necessary
    procedure CloneRow(ARow, AInsertPos: Integer);
    // Exchange contents of the two specified rows
    procedure ExchangeRows(ARow1, ARow2: Integer);
    // Rewrite all line endings within cell data to LineEnding
    procedure UnifyEmbeddedLineEndings;
    // Remove empty cells at end of rows from entire document
    procedure RemoveTrailingEmptyCells;

    // Properties

    // Cell data at column ACol, row ARow.
    property Cells[ACol, ARow: Integer]: String read GetCell write SetCell; default;
    // Number of rows
    property RowCount: Integer read GetRowCount;
    // Number of columns for row ARow
    property ColCount[ARow: Integer]: Integer read GetColCount;
    // Maximum number of columns found in all rows in document
    property MaxColCount: Integer read GetMaxColCount;
    // Document formatted as CSV text
    property CSVText: String read GetCSVText write SetCSVText;
  end;

implementation


//------------------------------------------------------------------------------

type
  TCSVCell = class
  public
    // Value (contents) of cell in string form
    Value: String;
  end;

  TCSVRow = class
  private
    FCells: TFPObjectList;
    procedure ForceCellIndex(ACellIndex: Integer);
    function  CreateNewCell(const AValue: String): TCSVCell;
    function  GetCellValue(ACol: Integer): String;
    procedure SetCellValue(ACol: Integer; const AValue: String);
    function  GetColCount: Integer;
  public
    constructor Create;
    destructor  Destroy; override;
    // cell operations
    // Add cell with value AValue to row
    procedure AddCell(const AValue: String = '');
    // Insert cell with value AValue at specified column
    procedure InsertCell(ACol: Integer; const AValue: String);
    // Remove cell from specified column
    procedure RemoveCell(ACol: Integer);
    // Indicates if specified column contains a cell/data
    function  HasCell(ACol: Integer): Boolean;
    // utilities
    // Copy entire row
    function  Clone: TCSVRow;
    // Remove all empty cells at the end of the row
    procedure TrimEmptyCells;
    // Replace various line endings in data with ALineEnding
    procedure SetValuesLineEnding(const ALineEnding: String);
    // properties
    // Value/data of cell at column ACol
    property CellValue[ACol: Integer]: String read GetCellValue write SetCellValue;
    // Number of columns in row
    property ColCount: Integer read GetColCount;
  end;

{ TCSVRow }

procedure TCSVRow.ForceCellIndex(ACellIndex: Integer);
begin
  while FCells.Count <= ACellIndex do
    AddCell();
end;

function TCSVRow.CreateNewCell(const AValue: String): TCSVCell;
begin
  Result := TCSVCell.Create;
  Result.Value := AValue;
end;

function TCSVRow.GetCellValue(ACol: Integer): String;
begin
  if HasCell(ACol) then
    Result := TCSVCell(FCells[ACol]).Value
  else
    Result := '';
end;

procedure TCSVRow.SetCellValue(ACol: Integer; const AValue: String);
begin
  ForceCellIndex(ACol);
  TCSVCell(FCells[ACol]).Value := AValue;
end;

function TCSVRow.GetColCount: Integer;
begin
  Result := FCells.Count;
end;

constructor TCSVRow.Create;
begin
  inherited Create;
  FCells := TFPObjectList.Create;
end;

destructor TCSVRow.Destroy;
begin
  FreeAndNil(FCells);
  inherited Destroy;
end;

procedure TCSVRow.AddCell(const AValue: String = '');
begin
  FCells.Add(CreateNewCell(AValue));
end;

procedure TCSVRow.InsertCell(ACol: Integer; const AValue: String);
begin
  FCells.Insert(ACol, CreateNewCell(AValue));
end;

procedure TCSVRow.RemoveCell(ACol: Integer);
begin
  if HasCell(ACol) then
    FCells.Delete(ACol);
end;

function TCSVRow.HasCell(ACol: Integer): Boolean;
begin
  Result := (ACol >= 0) and (ACol < FCells.Count);
end;

function TCSVRow.Clone: TCSVRow;
var
  I: Integer;
begin
  Result := TCSVRow.Create;
  for I := 0 to ColCount - 1 do
    Result.AddCell(CellValue[I]);
end;

procedure TCSVRow.TrimEmptyCells;
var
  I: Integer;
  MaxCol: Integer;
begin
  MaxCol := FCells.Count - 1;
  for I := MaxCol downto 0 do
  begin
    if (TCSVCell(FCells[I]).Value = '') then
    begin
      if (FCells.Count > 1) then
        FCells.Delete(I);
    end else
      break; // We hit the first non-empty cell so stop
  end;
end;

procedure TCSVRow.SetValuesLineEnding(const ALineEnding: String);
var
  I: Integer;
begin
  for I := 0 to FCells.Count - 1 do
    CellValue[I] := ChangeLineEndings(CellValue[I], ALineEnding);
end;

{ TCSVDocument }

procedure TCSVDocument.ForceRowIndex(ARowIndex: Integer);
begin
  while FRows.Count <= ARowIndex do
    AddRow();
end;

function TCSVDocument.CreateNewRow(const AFirstCell: String): TObject;
var
  NewRow: TCSVRow;
begin
  NewRow := TCSVRow.Create;
  if AFirstCell <> '' then
    NewRow.AddCell(AFirstCell);
  Result := NewRow;
end;

function TCSVDocument.GetCell(ACol, ARow: Integer): String;
begin
  if HasRow(ARow) then
    Result := TCSVRow(FRows[ARow]).CellValue[ACol]
  else
    Result := '';
end;

procedure TCSVDocument.SetCell(ACol, ARow: Integer; const AValue: String);
begin
  ForceRowIndex(ARow);
  TCSVRow(FRows[ARow]).CellValue[ACol] := AValue;
end;

function TCSVDocument.GetCSVText: String;
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create('');
  try
    SaveToStream(StringStream);
    Result := StringStream.DataString;
  finally
    FreeAndNil(StringStream);
  end;
end;

procedure TCSVDocument.SetCSVText(const AValue: String);
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create(AValue);
  try
    LoadFromStream(StringStream);
  finally
    FreeAndNil(StringStream);
  end;
end;

function TCSVDocument.GetRowCount: Integer;
begin
  Result := FRows.Count;
end;

function TCSVDocument.GetColCount(ARow: Integer): Integer;
begin
  if HasRow(ARow) then
    Result := TCSVRow(FRows[ARow]).ColCount
  else
    Result := 0;
end;

// Returns maximum number of columns in the document
function TCSVDocument.GetMaxColCount: Integer;
var
  I, CC: Integer;
begin
  // While calling MaxColCount in TCSVParser could work,
  // we'd need to adjust for any subsequent changes in
  // TCSVDocument
  Result := 0;
  for I := 0 to RowCount - 1 do
  begin
    CC := ColCount[I];
    if CC > Result then
      Result := CC;
  end;
end;

constructor TCSVDocument.Create;
begin
  inherited Create;
  FRows := TFPObjectList.Create;
  FParser := nil;
  FBuilder := nil;
end;

destructor TCSVDocument.Destroy;
begin
  FreeAndNil(FBuilder);
  FreeAndNil(FParser);
  FreeAndNil(FRows);
  inherited Destroy;
end;

procedure TCSVDocument.LoadFromFile(const AFilename: String);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TCSVDocument.LoadFromStream(AStream: TStream);
var
  I, J, MaxCol: Integer;
begin
  Clear;

  if not Assigned(FParser) then
    FParser := TCSVParser.Create;

  FParser.AssignCSVProperties(Self);
  with FParser do
  begin
    SetSource(AStream);
    while ParseNextCell do
      Cells[CurrentCol, CurrentRow] := CurrentCellText;
  end;

  if FEqualColCountPerRow then
  begin
    MaxCol := MaxColCount - 1;
    for I := 0 to RowCount - 1 do
      for J := ColCount[I] to MaxCol do
        Cells[J, I] := '';
  end;
end;

procedure TCSVDocument.SaveToFile(const AFilename: String);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(AFilename, fmCreate);
  try
    SaveToStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TCSVDocument.SaveToStream(AStream: TStream);
var
  I, J, MaxCol: Integer;
begin
  if not Assigned(FBuilder) then
    FBuilder := TCSVBuilder.Create;

  FBuilder.AssignCSVProperties(Self);
  with FBuilder do
  begin
    if FEqualColCountPerRow then
      MaxCol := MaxColCount - 1;

    SetOutput(AStream);
    for I := 0 to RowCount - 1 do
    begin
      if not FEqualColCountPerRow then
        MaxCol := ColCount[I] - 1;
      for J := 0 to MaxCol do
        AppendCell(Cells[J, I]);
      AppendRow;
    end;
  end;
end;

procedure TCSVDocument.AddRow(const AFirstCell: String = '');
begin
  FRows.Add(CreateNewRow(AFirstCell));
end;

procedure TCSVDocument.AddCell(ARow: Integer; const AValue: String = '');
begin
  ForceRowIndex(ARow);
  TCSVRow(FRows[ARow]).AddCell(AValue);
end;

procedure TCSVDocument.InsertRow(ARow: Integer; const AFirstCell: String = '');
begin
  if HasRow(ARow) then
    FRows.Insert(ARow, CreateNewRow(AFirstCell))
  else
    AddRow(AFirstCell);
end;

procedure TCSVDocument.InsertCell(ACol, ARow: Integer; const AValue: String);
begin
  ForceRowIndex(ARow);
  TCSVRow(FRows[ARow]).InsertCell(ACol, AValue);
end;

procedure TCSVDocument.RemoveRow(ARow: Integer);
begin
  if HasRow(ARow) then
    FRows.Delete(ARow);
end;

procedure TCSVDocument.RemoveCell(ACol, ARow: Integer);
begin
  if HasRow(ARow) then
    TCSVRow(FRows[ARow]).RemoveCell(ACol);
end;

function TCSVDocument.HasRow(ARow: Integer): Boolean;
begin
  Result := (ARow >= 0) and (ARow < FRows.Count);
end;

function TCSVDocument.HasCell(ACol, ARow: Integer): Boolean;
begin
  if HasRow(ARow) then
    Result := TCSVRow(FRows[ARow]).HasCell(ACol)
  else
    Result := False;
end;

function TCSVDocument.IndexOfCol(const AString: String; ARow: Integer): Integer;
var
  CC: Integer;
begin
  CC := ColCount[ARow];
  Result := 0;
  while (Result < CC) and (Cells[Result, ARow] <> AString) do
    Inc(Result);
  if Result = CC then
    Result := -1;
end;

function TCSVDocument.IndexOfRow(const AString: String; ACol: Integer): Integer;
var
  RC: Integer;
begin
  RC := RowCount;
  Result := 0;
  while (Result < RC) and (Cells[ACol, Result] <> AString) do
    Inc(Result);
  if Result = RC then
    Result := -1;
end;

procedure TCSVDocument.Clear;
begin
  FRows.Clear;
end;

procedure TCSVDocument.CloneRow(ARow, AInsertPos: Integer);
var
  NewRow: TObject;
begin
  if not HasRow(ARow) then
    Exit;
  NewRow := TCSVRow(FRows[ARow]).Clone;
  if not HasRow(AInsertPos) then
  begin
    ForceRowIndex(AInsertPos - 1);
    FRows.Add(NewRow);
  end else
    FRows.Insert(AInsertPos, NewRow);
end;

procedure TCSVDocument.ExchangeRows(ARow1, ARow2: Integer);
begin
  if not (HasRow(ARow1) and HasRow(ARow2)) then
    Exit;
  FRows.Exchange(ARow1, ARow2);
end;

procedure TCSVDocument.UnifyEmbeddedLineEndings;
var
  I: Integer;
begin
  for I := 0 to FRows.Count - 1 do
    TCSVRow(FRows[I]).SetValuesLineEnding(FLineEnding);
end;

procedure TCSVDocument.RemoveTrailingEmptyCells;
var
  I: Integer;
begin
  for I := 0 to FRows.Count - 1 do
    TCSVRow(FRows[I]).TrimEmptyCells;
end;

end.
