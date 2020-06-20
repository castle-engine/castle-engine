{ An undo system that handles
  Recording the changes made to the designed Scene
  Performing Undo/Redo actions based on current undo history }
unit CastleUndoSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, Generics.Collections;

type
  TUndoData = string;

type
  TUndoHistoryElement = class(TObject)
    Data: TUndoData;
    function Size: Integer;
  end;

type
  TUndoHistory = specialize TObjectList<TUndoHistoryElement>;

type
  TUndoSystem = class(TComponent)
  //private const MaxUndo = 50;
  private const MaxUndoHistorySize = 128 * 1024 * 1024; //128 Mb
  private
    CurrentUndo: Integer;
    UndoHistory: TUndoHistory;
    function UndoHistorySize: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Should be called after every significant change, including initial loading of the design }
    procedure RecordUndo(UndoData: TUndoData);
    function Undo: TUndoData;
    function Redo: TUndoData;
    function IsUndoPossible: Boolean;
    function IsRedoPossible: Boolean;
    procedure ClearUndoHistory;
  end;

function UndoSystem: TUndoSystem;

implementation
uses
  SysUtils,
  CastleComponentSerialize, CastleLog, CastleUtils;

function TUndoHistoryElement.Size: Integer;
begin
  Result := Data.Length;
end;

constructor TUndoSystem.Create(AOwner: TComponent);
begin
  inherited;
  UndoHistory := TUndoHistory.Create(true);
  ClearUndoHistory;
end;

destructor TUndoSystem.Destroy;
begin
  FreeAndNil(UndoHistory);
  inherited;
end;

function TUndoSystem.UndoHistorySize: Integer;
var
  U: TUndoHistoryElement;
begin
  Result := 0;
  for U in UndoHistory do
    Result += U.Size;
end;

procedure TUndoSystem.RecordUndo(UndoData: String);
var
  NewUndoElement: TUndoHistoryElement;
  I: Integer;
  NewUndoHistorySize: Integer;
begin
  if (UndoHistory.Count > 0) and (UndoData = UndoHistory[CurrentUndo].Data) then
  begin
    WriteLnLog('New Undo is identical to previous Undo record. Not saving.');
    Exit;
  end;
  WriteLnLog('Saving Undo record. CurrentUndo = ' + IntToStr(CurrentUndo));
  //Clean all next undo recoreds if available;
  for I := UndoHistory.Count - 1 downto CurrentUndo + 1 do
    UndoHistory.Delete(I);
  //add new UndoElement
  NewUndoElement := TUndoHistoryElement.Create;
  NewUndoElement.Data := UndoData;
  UndoHistory.Add(NewUndoElement);
  Inc(CurrentUndo);
  Assert(UndoHistory.Count - 1 = CurrentUndo);
  //make sure Undo doesn't consume too much RAM
  {if CurrentUndo > MaxUndo then
  begin
    UndoHistory.Delete(0);
    Dec(CurrentUndo);
  end;}
  NewUndoHistorySize := UndoHistorySize;
  while (NewUndoHistorySize > MaxUndoHistorySize) and (UndoHistory.Count > 1) do
  begin
    NewUndoHistorySize -= UndoHistory[0].Size;
    UndoHistory.Delete(0);
    Dec(CurrentUndo);
  end;
  Assert(UndoHistorySize = NewUndoHistorySize);
  WriteLnLog('Undo record saved. CurrentUndo = ' + IntToStr(CurrentUndo) + '; Undo History Size = ' + IntToStr(NewUndoHistorySize div 1024) + 'kb.');
end;

function TUndoSystem.Undo: TUndoData;
begin
  if IsUndoPossible then
  begin
    WriteLnLog('Performing Undo from ' + IntToStr(CurrentUndo) + ' to ' + IntToStr(CurrentUndo - 1));
    Dec(CurrentUndo);
    Result := UndoHistory[CurrentUndo].Data;
  end else
    raise EInternalError.Create('Undo was requested but undo is not possible');
end;

function TUndoSystem.Redo: TUndoData;
begin
  if IsRedoPossible then
  begin
    WriteLnLog('Performing Redo from ' + IntToStr(CurrentUndo) + ' to ' + IntToStr(CurrentUndo + 1));
    Inc(CurrentUndo);
    Result := UndoHistory[CurrentUndo].Data;
  end else
    raise EInternalError.Create('Redo was requested but redo is not possible');
end;

function TUndoSystem.IsUndoPossible: Boolean;
begin
  Result := CurrentUndo > 0;
end;

function TUndoSystem.IsRedoPossible: Boolean;
begin
  Result := CurrentUndo < UndoHistory.Count - 1;
end;

procedure TUndoSystem.ClearUndoHistory;
begin
  UndoHistory.Clear;
  CurrentUndo := -1;
  WriteLnLog('Clearing Undo hisotry.');
end;

var
  FUndoSystem: TUndoSystem;

function UndoSystem: TUndoSystem;
begin
  if FUndoSystem = nil then
    FUndoSystem := TUndoSystem.Create(nil); //should be owned by castle-editor, I guess
  Result := FUndoSystem;
end;

finalization
  FreeAndNil(FUndoSystem); //temporary
end.

