{ An undo system that handles:
  Recording the changes made to the designed Scene
  Performing Undo/Redo actions based on current undo history }
unit CastleUndoSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, Generics.Collections,
  CastleUtils;

type
  { Content of the Undo record.
    We might want to change it in future, most likely make several types of data
    that can be recorded as undo events }
  TUndoData = String;
  { Reference to the component selected at the moment of recording Undo data.
    Currently we are "finding" the component by name to select it. }
  TSelectedComponent = String;

type
  { A single change in scene content. }
  TUndoHistoryElement = class(TObject)
    { Content of this undo. }
    Data: TUndoData;
    { Compopent, selected at the time of undo recording. }
    Selected: TSelectedComponent;
    { Index of the value selected/edited in the ObjectInspector. }
    ItemIndex: Integer;
    { Index of the tab, opened at the moment of the Undo recording. }
    TabIndex: Integer;
    { Human-readable explanation of what action is recorded in this undo record. }
    Comment: String;
    { Estimate of this undo record size;
      It may be several bytes inaccurate,
      but it's used only to avoid exceeding some preset RAM limit. }
    function Size: SizeInt;
  end;

type
  { All undo records currently availalbe. }
  TUndoHistory = specialize TObjectList<TUndoHistoryElement>;

type
  { Manages undo records and performs undo/redo. }
  TUndoSystem = class(TComponent)
  //private const MaxUndo = 50;
  { Max amount of memory that can be reserved by Undo System.
    If the current records total size overruns this limit,
    the fist undo records will be purged.
    Maybe we'll want to have it as user-definable variable in Castle-Editor settings. }
  private const MaxUndoHistorySize = 128 * 1024 * 1024; //128 Mb
  private
    { Current undo step,
      equals to the last element of the Undo History, unless an undo has been performed. }
    CurrentUndo: Integer;
    { All undo records in this session. }
    UndoHistory: TUndoHistory;
    { Calculates total size of RAM used by the Undo History. }
    function UndoHistorySize: Integer;
    { Construct human-readable comment for this Undo record. }
    function GetUndoComment(const UndoI: Integer): String;
  public
    { Called when Undo information displayed to the User should be changed. }
    OnUpdateUndo: TNotifyEvent;
    { Should the Undo be recorded on Release event?
      Note, that we rely here on Release event, which may never come in case
      user has switched a window (e.g. with Alt-Tab) while dragging.
      This value will be purged when recording a new Undo record. }
    ScheduleRecordUndoOnRelease: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Should be called after every significant change, including initial loading of the design. }
    { Try to record a new Undo record.
      If several actions have been undone before, all the redo history will be cleared at this moment.
      If the new Undo record is equal to the recorded one then nothing will be recorded. }
    procedure RecordUndo(const UndoData: TUndoData; const SelectedComponent: TSelectedComponent; const ItemIndex: Integer; const TabIndex: Integer; const UndoComment: String);
    { Get a recent state change and move one step backwards in Undo History. }
    function Undo: TUndoHistoryElement;
    { Get a state change following current state and move one step fowrard in Undo History. }
    function Redo: TUndoHistoryElement;
    { Is it possible to perform Undo right now? }
    function IsUndoPossible: Boolean;
    { Is it possible to perform Redo right now? }
    function IsRedoPossible: Boolean;
    { Get comment for Undo action if it's possible, or return ''. }
    function UndoComment: String;
    { Get comment for Redo action if it's possible, or return ''. }
    function RedoComment: String;
    { Clear all Undo History. }
    procedure ClearUndoHistory;
  end;

implementation
uses
  SysUtils,
  CastleComponentSerialize, CastleLog, CastleStringUtils;

function TUndoHistoryElement.Size: SizeInt;

  //see https://www.freepascal.org/docs-html/ref/refsu13.html
  //I'm not sure if this includes size of string pointer or not.
  //Note that Length(AnsiString) returns length of the string in bytes, not in UTF8 characters.
  function SizeOfAnsiString(const S: String): SizeInt;
  const
    HS = 16;
  begin
    Result := Length(S) + 1 + HS;
  end;

begin
  Result := Self.InstanceSize + SizeOfAnsiString(Data) + SizeOfAnsiString(Selected) + SizeOfAnsiString(Comment);
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

procedure TUndoSystem.RecordUndo(const UndoData: TUndoData; const SelectedComponent: TSelectedComponent; const ItemIndex: Integer; const TabIndex: Integer; const UndoComment: String);
var
  NewUndoElement: TUndoHistoryElement;
  NewUndoHistorySize: Integer;
begin
  ScheduleRecordUndoOnRelease := false;
  if (UndoHistory.Count > 0) and (UndoData = UndoHistory[CurrentUndo].Data) then
  begin
    if (SelectedComponent = UndoHistory[CurrentUndo].Selected) or (UndoHistory[CurrentUndo].Selected = '') then
      WriteLnLog('New Undo is identical to previous Undo record. Not saving.')
    else
    begin
      WriteLnLog('New Undo is identical to previous Undo record. Only selection has changed from ' + UndoHistory[CurrentUndo].Selected + ' to ' + SelectedComponent + '. This change has been saved.');
      UndoHistory[CurrentUndo].Selected := SelectedComponent;
      OnUpdateUndo(Self);
    end;
    if (UndoComment <> '') and (UndoHistory[CurrentUndo].Comment = '') then
    begin
      WriteLnLog('The last undo comment was set to generic "Edit value", overwriting with better reason: ' + UndoComment);
      UndoHistory[CurrentUndo].Comment := UndoComment;
      OnUpdateUndo(Self);
    end;
    Exit;
  end;
  WriteLnLog('Saving Undo record. CurrentUndo = ' + IntToStr(CurrentUndo));
  //Clean all next undo records if available;
  UndoHistory.Count := CurrentUndo + 1;
  //add new UndoElement
  NewUndoElement := TUndoHistoryElement.Create;
  NewUndoElement.Data := UndoData;
  NewUndoElement.Selected := SelectedComponent;
  NewUndoElement.ItemIndex := ItemIndex;
  NewUndoElement.TabIndex := TabIndex;
  NewUndoElement.Comment := UndoComment;
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
  OnUpdateUndo(Self);
  WriteLnLog('Undo record saved. CurrentUndo = ' + IntToStr(CurrentUndo) + '; Undo History Size = ' + SizeToStr(NewUndoHistorySize));
end;

function TUndoSystem.Undo: TUndoHistoryElement;
begin
  ScheduleRecordUndoOnRelease := false;
  if IsUndoPossible then
  begin
    WriteLnLog('Performing Undo from ' + IntToStr(CurrentUndo) + ' to ' + IntToStr(CurrentUndo - 1));
    Dec(CurrentUndo);
    Result := UndoHistory[CurrentUndo];
    //OnUpdateUndo; The caller itself should better take care of that
  end else
    raise EInternalError.Create('Undo was requested but undo is not possible');
end;

function TUndoSystem.Redo: TUndoHistoryElement;
begin
  ScheduleRecordUndoOnRelease := false;
  if IsRedoPossible then
  begin
    WriteLnLog('Performing Redo from ' + IntToStr(CurrentUndo) + ' to ' + IntToStr(CurrentUndo + 1));
    Inc(CurrentUndo);
    Result := UndoHistory[CurrentUndo];
    //OnUpdateUndo; The caller itself should better take care of that
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

function TUndoSystem.GetUndoComment(const UndoI: Integer): String;
begin
  if UndoHistory[UndoI].Comment = '' then
  begin
    if UndoHistory[UndoI].Selected = '' then
      Result := 'Modify global variable'
    else
      Result := 'Modify ' + UndoHistory[UndoI].Selected;
  end else
  begin
    if UndoHistory[UndoI].Selected = '' then
      Result := UndoHistory[UndoI].Comment
    else
      Result := UndoHistory[UndoI].Comment + ' (' + UndoHistory[UndoI].Selected + ')';
  end;
end;

function TUndoSystem.RedoComment: String;
begin
  if IsRedoPossible then
    Result := 'Redo: ' + GetUndoComment(CurrentUndo + 1)
  else
    Result := 'Redo';
end;

function TUndoSystem.UndoComment: String;
begin
  if IsUndoPossible then
    Result := 'Undo: ' + GetUndoComment(CurrentUndo)
  else
    Result := 'Undo';
end;

procedure TUndoSystem.ClearUndoHistory;
begin
  ScheduleRecordUndoOnRelease := false;
  UndoHistory.Clear;
  CurrentUndo := -1;
  if Assigned(OnUpdateUndo) then
    OnUpdateUndo(Self);
  WriteLnLog('Clearing Undo hisotry.');
end;

end.

