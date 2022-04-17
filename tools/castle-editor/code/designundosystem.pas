{
  Copyright 2020-2022 Yevhen Loza, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ An undo system that handles:
  Recording the changes made to the designed Scene
  Performing Undo/Redo actions based on current undo history }
unit DesignUndoSystem;

interface

uses
  Classes, Generics.Collections,
  CastleUtils;

type
  TUndoCommentPriority = (
    { Low priority comments will get overwritten by a higher priority comments
      which correspond to a more specific description of what has happened,
      but are not always available. }
    ucLow,
    { High priority comments usually describe what exactly happened
      and should overwrite less specific comments. }
    ucHigh
    );


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
    { Current undo element priority,
      new undo record with identical data but higher priority
      can overwrite undo record with lower priority.
      This is required because often we receive several identical undo records
      for the same user action, some of them may have a "better" (i.e. more specific) comment
      which should overwrite the previous (or following) less specific comments. }
    CurrentUndoCommentPriority: TUndoCommentPriority;
    { All undo records in this session. }
    UndoHistory: TUndoHistory;
    { Calculates total size of RAM used by the Undo History. }
    function UndoHistorySize: Integer;
  public
    { Called when Undo information displayed to the User should be changed. }
    OnUpdateUndo: TNotifyEvent;
    { Should the Undo be recorded on Release event?
      Note, that we rely here on Release event, which may never come in case
      user has switched a window (e.g. with Alt-Tab) while dragging.
      This value will be purged when recording a new Undo record. }
    ScheduleRecordUndoOnRelease: Boolean;
    { Verbose log to WritelnLog. }
    VerboseLog: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Should be called after every significant change, including initial loading of the design. }
    { Try to record a new Undo record.
      If several actions have been undone before, all the redo history will be cleared at this moment.
      If the new Undo record is equal to the recorded one then nothing will be recorded. }
    procedure RecordUndo(const UndoData: TUndoData; const SelectedComponent: TSelectedComponent;
      const ItemIndex: Integer; const TabIndex: Integer; const UndoComment: String;
      const UndoCommentPriority: TUndoCommentPriority);
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
    { Log, following VerboseLog flag. }
    procedure DoLog(const S: String);
    procedure DoLog(const SPattern: String; const Args: array of const);
  end;

implementation
uses
  SysUtils,
  CastleComponentSerialize, CastleLog, CastleStringUtils;

function TUndoHistoryElement.Size: SizeInt;

  { See https://www.freepascal.org/docs-html/ref/refsu13.html
    I'm not sure if this includes size of string pointer or not.
    Note that Length(AnsiString) returns length of the string in bytes, not in UTF8 characters. }
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

procedure TUndoSystem.RecordUndo(const UndoData: TUndoData; const SelectedComponent: TSelectedComponent;
  const ItemIndex: Integer; const TabIndex: Integer; const UndoComment: String;
  const UndoCommentPriority: TUndoCommentPriority);
var
  NewUndoElement: TUndoHistoryElement;
  NewUndoHistorySize: Integer;
begin
  ScheduleRecordUndoOnRelease := false;
  if (UndoHistory.Count > 0) and (UndoData = UndoHistory[CurrentUndo].Data) then
  begin
    if (SelectedComponent <> UndoHistory[CurrentUndo].Selected) then
    begin
      DoLog('New Undo is identical to previous Undo record. Only selection has changed from ' + UndoHistory[CurrentUndo].Selected + ' to ' + SelectedComponent + '. This change has been saved.');
      UndoHistory[CurrentUndo].Selected := SelectedComponent;
      OnUpdateUndo(Self);
    end;
    if (UndoCommentPriority > CurrentUndoCommentPriority) then
    begin
      DoLog('Overwriting previous Undo record "' + UndoHistory[CurrentUndo].Comment +
        '" with a higher priority comment: "' + UndoComment + '".');
      UndoHistory[CurrentUndo].Comment := UndoComment;
      OnUpdateUndo(Self); // It is safe to call OnUpdateUndo multiple times - it only updates menu captions
    end;
    Exit;
  end;

  DoLog('Saving Undo record. CurrentUndo = ' + IntToStr(CurrentUndo));
  // Clean all next undo records if available;
  UndoHistory.Count := CurrentUndo + 1;
  // Add new UndoElement
  NewUndoElement := TUndoHistoryElement.Create;
  NewUndoElement.Data := UndoData;
  NewUndoElement.Selected := SelectedComponent;
  NewUndoElement.ItemIndex := ItemIndex;
  NewUndoElement.TabIndex := TabIndex;
  NewUndoElement.Comment := UndoComment;
  UndoHistory.Add(NewUndoElement);
  Inc(CurrentUndo);
  CurrentUndoCommentPriority := UndoCommentPriority;

  Assert(UndoHistory.Count - 1 = CurrentUndo);
  // Make sure Undo doesn't consume too much RAM
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
  DoLog('Undo record saved. CurrentUndo = ' + IntToStr(CurrentUndo) + '; Undo History Size = ' + SizeToStr(NewUndoHistorySize));
end;

function TUndoSystem.Undo: TUndoHistoryElement;
begin
  ScheduleRecordUndoOnRelease := false;
  if IsUndoPossible then
  begin
    DoLog('Performing Undo from ' + IntToStr(CurrentUndo) + ' to ' + IntToStr(CurrentUndo - 1));
    Dec(CurrentUndo);
    Result := UndoHistory[CurrentUndo];
    CurrentUndoCommentPriority := High(TUndoCommentPriority); // Whatever happens next this Undo record cannot be overwritten
    OnUpdateUndo(Self);
  end else
    raise EInternalError.Create('Undo was requested but undo is not possible');
end;

function TUndoSystem.Redo: TUndoHistoryElement;
begin
  ScheduleRecordUndoOnRelease := false;
  if IsRedoPossible then
  begin
    DoLog('Performing Redo from ' + IntToStr(CurrentUndo) + ' to ' + IntToStr(CurrentUndo + 1));
    Inc(CurrentUndo);
    Result := UndoHistory[CurrentUndo];
    CurrentUndoCommentPriority := High(TUndoCommentPriority); // Whatever happens next this Undo record cannot be overwritten
    OnUpdateUndo(Self);
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

function TUndoSystem.RedoComment: String;
begin
  if IsRedoPossible then
  begin
    if UndoHistory[CurrentUndo + 1].Comment = '' then
      Result := 'Redo'
    else
      Result := 'Redo: ' + UndoHistory[CurrentUndo + 1].Comment;
  end else
    Result := 'Redo';
end;

function TUndoSystem.UndoComment: String;
begin
  if IsUndoPossible then
  begin
    if UndoHistory[CurrentUndo].Comment = '' then
      Result := 'Undo'
    else
      Result := 'Undo: ' + UndoHistory[CurrentUndo].Comment;
  end else
    Result := 'Undo';
end;

procedure TUndoSystem.ClearUndoHistory;
begin
  ScheduleRecordUndoOnRelease := false;
  UndoHistory.Clear;
  CurrentUndo := -1;
  CurrentUndoCommentPriority := High(TUndoCommentPriority); // Just for consistency
  if Assigned(OnUpdateUndo) then
    OnUpdateUndo(Self);
  DoLog('Clearing Undo hisotry.');
end;

procedure TUndoSystem.DoLog(const S: String);
begin
  if VerboseLog then
    WriteLnLog(S);
end;

procedure TUndoSystem.DoLog(const SPattern: String; const Args: array of const);
begin
  DoLog(Format(SPattern, Args));
end;

end.
