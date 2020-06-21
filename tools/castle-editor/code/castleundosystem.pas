{ An undo system that handles
  Recording the changes made to the designed Scene
  Performing Undo/Redo actions based on current undo history }
unit CastleUndoSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, Generics.Collections,
  CastleUtils;

type
  TUndoData = string;
  TSelectedComponent = string;

type
  TUndoHistoryElement = object
    Data: TUndoData;
    Selected: TSelectedComponent;
    function Size: SizeInt;
  end;

type
  TUndoHistory = specialize TStructList<TUndoHistoryElement>;

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
    procedure RecordUndo(UndoData: TUndoData; SelectedComponent: TSelectedComponent);
    function Undo: TUndoHistoryElement;
    function Redo: TUndoHistoryElement;
    function IsUndoPossible: Boolean;
    function IsRedoPossible: Boolean;
    procedure ClearUndoHistory;
  end;

function UndoSystem: TUndoSystem;

implementation
uses
  SysUtils,
  CastleComponentSerialize, CastleLog;

function TUndoHistoryElement.Size: SizeInt;

  //see https://www.freepascal.org/docs-html/ref/refsu13.html
  function SizeOfAnsiString(const S: String): SizeInt;
  const
    HS = 16;
  begin
    Result := Length(S) + 1 + HS;
  end;

begin
  Result := SizeOf(Self) + SizeOfAnsiString(Data) + SizeOfAnsiString(Selected);
end;

constructor TUndoSystem.Create(AOwner: TComponent);
begin
  inherited;
  UndoHistory := TUndoHistory.Create;
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

procedure TUndoSystem.RecordUndo(UndoData: TUndoData; SelectedComponent: TSelectedComponent);
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
  //Clean all next undo records if available;
  for I := UndoHistory.Count - 1 downto CurrentUndo + 1 do
    UndoHistory.Delete(I);
  //add new UndoElement
  //NewUndoElement := TUndoHistoryElement.Create;
  NewUndoElement.Data := UndoData;
  NewUndoElement.Selected := SelectedComponent;
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

function TUndoSystem.Undo: TUndoHistoryElement;
begin
  if IsUndoPossible then
  begin
    WriteLnLog('Performing Undo from ' + IntToStr(CurrentUndo) + ' to ' + IntToStr(CurrentUndo - 1));
    Result.Data := UndoHistory[CurrentUndo - 1].Data;
    Result.Selected := UndoHistory[CurrentUndo].Selected;
    Dec(CurrentUndo);
  end else
    raise EInternalError.Create('Undo was requested but undo is not possible');
end;

function TUndoSystem.Redo: TUndoHistoryElement;
begin
  if IsRedoPossible then
  begin
    WriteLnLog('Performing Redo from ' + IntToStr(CurrentUndo) + ' to ' + IntToStr(CurrentUndo + 1));
    Result.Data := UndoHistory[CurrentUndo + 1].Data;
    if CurrentUndo < UndoHistory.Count - 2 then
      Result.Selected := UndoHistory[CurrentUndo + 2].Selected
    else
      Result.Selected := UndoHistory[CurrentUndo + 1].Selected;
    Inc(CurrentUndo);
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

