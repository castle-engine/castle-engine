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
  end;

type
  TUndoHistory = specialize TObjectList<TUndoHistoryElement>;

type
  TUndoSystem = class(TComponent)
  private
    CurrentUndo: Integer;
    UndoHistory: TUndoHistory;
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

procedure TUndoSystem.RecordUndo(UndoData: String);
var
  NewUndoElement: TUndoHistoryElement;
  I: Integer;
begin
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
  WriteLnLog('Undo record saved. CurrentUndo = ' + IntToStr(CurrentUndo));
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
  Result := CurrentUndo >= 0;
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

