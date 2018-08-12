{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple castle-editor utilities. }
unit EditorUtils;

{$mode objfpc}{$H+}

interface

uses Classes, Types, Controls, StdCtrls, Process,
  CastleStringUtils;

procedure ErrorBox(const Message: String);
procedure WarningBox(const Message: String);
function YesNoBox(const Message: String): Boolean;

type
  TOutputKind = (
    okInfo,
    okImportantInfo,
    okWarning,
    okError
  );

  { Instance of this takes control of your TListBox to display messages. }
  TOutputList = class
  strict private
    type
      TOutputInfo = class
        Kind: TOutputKind;
      end;
    var
      OutputInfoPerKind: array [TOutputKind] of TOutputInfo;
      FList: TListBox;
    procedure DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  public
    constructor Create(const AList: TListBox);
    property List: TListBox read FList;

    procedure AddLine(const S: String; const Kind: TOutputKind);
    { Split S into multiple lines.
      Add each of them as separate line.
      Avoids adding empty lines at the very end (since they would
      make the look uglier; instead,
      we make nice separators with AddSeparator). }
    procedure SplitAddLines(const S: String; const Kind: TOutputKind);
    procedure AddSeparator;
    procedure Clear;
    procedure ScrollBottom;
  end;

  { Call external process, sending output to TOutputList. }
  TAsynchronousProcess = class
  strict private
    Process: TProcess;
    { Text read from Process output, but not passed to OutputList yet. }
    PendingLines: String;
    FRunning: Boolean;
    FExitStatus: Integer;
  public
    { Set before @link(Start), do not change later.
      @groupBegin }
    ExeName, CurrentDirectory: String;
    Parameters: TCastleStringList;
    OutputList: TOutputList;
    { @groupEnd }

    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Update;
    property Running: Boolean read FRunning;

    { Defined only after callling Start, once Running turned false. }
    property ExitStatus: Integer read FExitStatus;
  end;

implementation

uses SysUtils, Dialogs, Graphics,
  CastleUtils;

procedure ErrorBox(const Message: String);
begin
  MessageDlg('Error', Message, mtError, [mbOK], 0);
end;

procedure WarningBox(const Message: String);
begin
  MessageDlg('Warning', Message, mtWarning, [mbOK], 0);
end;

function YesNoBox(const Message: String): Boolean;
begin
  Result := MessageDlg('Question', Message, mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

{ TAsynchronousProcess ------------------------------------------------------- }

constructor TAsynchronousProcess.Create;
begin
  inherited;
  Parameters := TCastleStringList.Create;
end;

destructor TAsynchronousProcess.Destroy;
begin
  FreeAndNil(Parameters);
  if (Process <> nil) and Process.Running then
    { stop the process, TProcess destructor doesn't do it
      (https://www.freepascal.org/docs-html/fcl/process/tprocess.destroy.html) }
    Process.Terminate(0);
  FreeAndNil(Process);
  inherited;
end;

procedure TAsynchronousProcess.Start;
var
  S, LogLine: String;
begin
  { create Process and call Process.Execute }
  Process := TProcess.Create(nil);
  Process.Executable := ExeName;
  if CurrentDirectory <> '' then
    Process.CurrentDirectory := CurrentDirectory;
  for S in Parameters do
    Process.Parameters.Add(S);
  Process.Options := [poUsePipes, poStderrToOutput];
  Process.Execute;

  { since the process is executed independently, in theory it *could*
    finish before we get to next line, possibly? }
  //if not Process.Running then
  //  raise EInternalError.Create('Process not running right after Execute?');

  { log in OutputList }
  LogLine := 'Running "' + ExeName;
  for S in Parameters do
    LogLine += ' ' + S;
  LogLine += '"';
  OutputList.AddLine(LogLine, okImportantInfo);
  OutputList.AddSeparator;

  FRunning := true;
end;

procedure TAsynchronousProcess.Update;
const
  ReadMaxSize = 65536;
var
  Buffer: array [0..ReadMaxSize - 1] of Byte;
  ReadBytes: LongInt;
  NewLinePos, OldPendingLinesLen, ProcessedLength: Integer;
  Line: String;
begin
  { In case Process.Running=false, set our own FRunning and FExitStatus.
    Note that we don't make a method like TAsynchronousProcess.Running that
    simply returnsProcess.Running, as then we could report as Running=false
    a processthat didn't yet dump all it's output to OutputList. }
  if not Process.Running then
  begin
    FRunning := false;
    FExitStatus := Process.ExitStatus;
  end;

  { Read Process.Output.
    This must never hang waiting for process, so we check NumBytesAvailable. }
  if Process.Output.NumBytesAvailable <> 0 then
    ReadBytes := Process.Output.Read(Buffer[0], ReadMaxSize)
  else
    ReadBytes := 0;

  // early exit for most usual case when ReadBytes = 0
  if ReadBytes = 0 then
  begin
    { Once the process is not running, and we read everything (ReadBytes = 0),
      dump the remaining PendingLines as last line. }
    if (not Process.Running) and (Length(PendingLines) <> 0) then
    begin
      OutputList.AddLine(PendingLines, okInfo);
      PendingLines := '';
    end;
    Exit;
  end;

  // add Buffer to PendingLines
  OldPendingLinesLen := Length(PendingLines);
  SetLength(PendingLines, OldPendingLinesLen + ReadBytes);
  Move(Buffer[0], PendingLines[OldPendingLinesLen + 1], ReadBytes);

  // extract from PendingLines complete lines (as many as possible)
  repeat
    { Note: it's tempting to try to detect #13#10 (Windows),
      or #10 (Unix),
      or #13 (nothing supported),
      or #10#13 (nothing supported)
      sequence,
      by detecting first char (#13 or #10) and then remove the following
      (if #10 is followed or #13, or #13 is followed by #10).
      But that's risky: if the read chunks will be split right in the middle
      of #13#10, we would introduce an extra newline in the intepreted output.

      It's easier *and* more reliable to just detect newlines as #10,
      and simply ignore any #13 character if it is followed by #10.
      This works for Windows and Unix, which is OK for us. }

    NewLinePos := Pos(#10, PendingLines);
    if NewLinePos <> 0 then
    begin
      ProcessedLength := NewLinePos;

      if SCharIs(PendingLines, NewLinePos - 1, #13) then
        Dec(NewLinePos);
      Line := Copy(PendingLines, 1, NewLinePos - 1);
      OutputList.AddLine(Line, okInfo);

      PendingLines := SEnding(PendingLines, ProcessedLength + 1);
    end else
      Break;
  until false;

  // if we get a lot of output, maybe more than ReadMaxSize, then keep reading!
  if ReadMaxSize = ReadBytes then
    Update;
end;

{ TOutputList --------------------------------------------------------------- }

procedure TOutputList.DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);

  { Copied from LCL stdctrls.pp }
  procedure InternalDrawItem(Control: TControl;
    Canvas: TCanvas; ARect: TRect; const Text: string);
  var
    OldBrushStyle: TBrushStyle;
    OldTextStyle: TTextStyle;
    NewTextStyle: TTextStyle;
  begin
    OldBrushStyle := Canvas.Brush.Style;
    Canvas.Brush.Style := bsClear;

    OldTextStyle := Canvas.TextStyle;
    NewTextStyle := OldTextStyle;
    NewTextStyle.Layout := tlCenter;
    NewTextStyle.RightToLeft := Control.UseRightToLeftReading;
    if Control.UseRightToLeftAlignment then
    begin
      NewTextStyle.Alignment := taRightJustify;
      ARect.Right := ARect.Right - 2;
    end
    else
    begin
      NewTextStyle.Alignment := taLeftJustify;
      ARect.Left := ARect.Left + 2;
    end;

    Canvas.TextStyle := NewTextStyle;

    Canvas.TextRect(ARect, ARect.Left, ARect.Top, Text);
    Canvas.Brush.Style := OldBrushStyle;
    Canvas.TextStyle := OldTextStyle;
  end;

var
  C: TCanvas;
  OutputInfo: TOutputInfo;
begin
  C := List.Canvas;
  OutputInfo := List.Items.Objects[Index] as TOutputInfo;

  case OutputInfo.Kind of
    okImportantInfo: C.Font.Bold := true;
    okWarning      : C.Brush.Color := clYellow;
    okError        : C.Brush.Color := clRed;
  end;

  C.FillRect(ARect);
  InternalDrawItem(List, C, ARect, List.Items[Index]);
end;

constructor TOutputList.Create(const AList: TListBox);
var
  Kind: TOutputKind;
begin
  inherited Create;

  FList := AList;
  FList.Style := lbOwnerDrawFixed;
  FList.OnDrawItem := @DrawItem;

  { Create OutputInfoPerKind instances.
    The idea is that every AddLine call should not construct new
    TOutputInfo, this could eat quite some memory for a lot of lines.
    Instead most AddLine calls can just take OutputInfoPerKind[Kind]. }
  for Kind in TOutputKind do
  begin
    OutputInfoPerKind[Kind] := TOutputInfo.Create;
    OutputInfoPerKind[Kind].Kind := Kind;
  end;
end;

procedure TOutputList.AddLine(const S: String; const Kind: TOutputKind);
begin
  List.Items.AddObject(S, OutputInfoPerKind[Kind]);
  // TODO: scroll to bottom once new message received only if already looking
  // at bottom
  ScrollBottom;
end;

procedure TOutputList.SplitAddLines(const S: String; const Kind: TOutputKind);
var
  SL: TStringList;
  Line: String;
begin
  SL := TStringList.Create;
  try
    SL.Text := S;
    { remove empty lines at end }
    while (SL.Count <> 0) and (SL[SL.Count - 1] = '') do
      SL.Delete(SL.Count - 1);
    for Line in SL do
      AddLine(Line, Kind);
  finally FreeAndNil(SL) end;
end;

procedure TOutputList.AddSeparator;
begin
  AddLine('', okInfo);
end;

procedure TOutputList.Clear;
begin
  List.Items.Clear;
end;

procedure TOutputList.ScrollBottom;
begin
  List.TopIndex := List.Items.Count - 1;
end;

end.
