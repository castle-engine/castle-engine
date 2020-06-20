{ An undo system that handles
  Recording the changes made to the designed Scene
  Performing Undo/Redo actions based on current undo history }
unit CastleUndoSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, Generics.Collections;

type
  TUndoHistoryElement = class(TObject)
    Data: String;
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
    procedure RecordUndo(UndoData: String);
    procedure Undo;
    procedure Redo;
    function IsUndoPossible: Boolean;
    function IsRedoPossible: Boolean;
    procedure ClearUndoHistory;
  end;

function UndoSystem: TUndoSystem;

implementation
uses
  SysUtils,
  CastleComponentSerialize, CastleLog;

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
  WriteLnLog('Record Undo data: ' + UndoData);
  //Clean all next undo recoreds if available;
  for I := UndoHistory.Count - 1 downto CurrentUndo + 1 do
    UndoHistory.Delete(I);
  //add new UndoElement
  NewUndoElement := TUndoHistoryElement.Create;
  NewUndoElement.Data := UndoData;
  UndoHistory.Add(NewUndoElement);
  Inc(CurrentUndo);
  Assert(UndoHistory.Count - 1 = CurrentUndo);
end;

procedure TUndoSystem.Undo;
begin
  if IsUndoPossible then
  begin
    WriteLnLog('Performing Undo from ' + IntToStr(CurrentUndo) + ' to ' + IntToStr(CurrentUndo - 1));
    //TODO and perform the Undo actually
    Dec(CurrentUndo);
  end;
end;

procedure TUndoSystem.Redo;
begin
  if IsRedoPossible then
  begin
    WriteLnLog('Performing Redo from ' + IntToStr(CurrentUndo) + ' to ' + IntToStr(CurrentUndo + 1));
    //TODO and perform the Redo actually
    Inc(CurrentUndo);
  end;
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

