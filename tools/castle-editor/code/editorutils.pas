{
  Copyright 2018-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Various castle-editor utilities. }
unit EditorUtils;

{$I editorconf.inc}

interface

uses Classes, Types, Controls, StdCtrls, Process, Menus, Generics.Collections,
  Dialogs,
  CastleStringUtils,
  ToolArchitectures, ToolManifest, ToolProcess;

type
  TMenuItemHelper = class helper for TMenuItem
  public
    procedure SetEnabledVisible(const Value: Boolean);
  end;

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
    destructor Destroy; override;
    property List: TListBox read FList;

    procedure AddLine(const S: String; const Kind: TOutputKind);
    { Split S into multiple lines.
      Add each of them as separate line.
      Avoids adding empty lines at the very end (since they would
      make the look uglier; instead,
      we make nice separators with AddSeparator). }
    //procedure SplitAddLines(const S: String; const Kind: TOutputKind);
    procedure AddSeparator;
    procedure Clear;
  end;

  { Call external process asynchronously (doesn't hang this process at any point),
    sending output to TOutputList. }
  TAsynchronousProcess = class
  strict private
    Process: TProcess;
    { Text read from Process output, but not passed to OutputList yet. }
    PendingLines: String;
    FRunning: Boolean;
    FExitStatus: Integer;
    Environment: TStringList;
    { Additional process id to kill, when killing Process. }
    ChildProcessId: TProcessId;
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

    { On some platforms, if you want to make sure to kill children
      (like child of "castle-engine run") you should run this before
      freeing the process. }
    procedure TerminateChildrenHarder;
  end;

  { Call asynchronous processes, one after another, until the end
    or until one of them exits with non-zero status.

    This has deliberately similar API to TAsynchronousProcess,
    since (from the outside) calling a sequence of processes
    should have the same features as calling a single process. }
  TAsynchronousProcessQueue = class
  strict private
    FQueuePosition: Integer;
    AsyncProcess: TAsynchronousProcess;
    procedure CreateAsyncProcess;
  public
    type
      TQueueItem = class
        ExeName, CurrentDirectory: String;
        Parameters: TCastleStringList;
        constructor Create;
        destructor Destroy; override;
      end;

      TQueueItemList = specialize TObjectList<TQueueItem>;

    var
      Queue: TQueueItemList;
      OutputList: TOutputList;
      OnFinished, OnSuccessfullyFinishedAll: TNotifyEvent;

    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Update;
    function Running: Boolean;

    { On some platforms, if you want to make sure to kill children
      (like child of "castle-engine run") you should run this before
      freeing the process. }
    procedure TerminateChildrenHarder;
  end;

  TPlatformInfo = class
    Target: TTarget;
    OS: TOS;
    CPU: TCPU;
  end;
  TPlatformInfoList = specialize TObjectList<TPlatformInfo>;

procedure ErrorBox(const Message: String);
procedure InfoBox(const Message: String);
procedure WarningBox(const Message: String);
function YesNoBox(const Message: String): Boolean;
function YesNoBox(const Caption, Message: String): Boolean;

{ Set both C.Enabled and C.Exists. }
procedure SetEnabledVisible(const C: TControl; const Value: Boolean);

const
  FpcRtlApiReferenceUrl = 'https://www.freepascal.org/docs-html/rtl/';
  LclApiReferenceUrl = 'https://lazarus-ccr.sourceforge.io/docs/lcl/';

function ApiReferenceUrl: String;

{ Get full URL to display API reference of a given property in the given
  PropertyObject.

  PropertyName may be '', in which case the link leads to the whole class reference.
  In this case PropertyNameForLink must also be ''.
  Both PropertyName and PropertyNameForLink should be '',
  or both should be non-empty.
}
function ApiReference(const PropertyObject: TObject;
  const PropertyName, PropertyNameForLink: String): String;

{ Add to given submenus (TMenuItem) items for each registered serializable
  component. Thus it allows to construct menu to add all possible
  TCastleTransform instances, all possible TCastleUserInterface instances etc.
  Any ParentXxx may be nil, then relevant components are not added.

  All created menu items have OnClick set to OnClickEvent. }
procedure BuildComponentsMenu(
  const ParentUserInterface, ParentTransform, ParentBehavior, ParentNonVisual: TMenuItem;
  const OnClickEvent: TNotifyEvent);

type
  TCodeEditor = (
    { Autodetect one of the below (Lazarus, Delphi, VS Code) and use hardcoded logic suitable for it. }
    ceAutodetect,
    { Use hardcoded logic suitable for Lazarus. }
    ceLazarus,
    { Use hardcoded logic suitable for Delphi. }
    ceDelphi,
    { Use hardcoded logic suitable for Visual Studio Code. }
    ceVSCode,
    { Use custom commands from CodeEditor, CodeEditorProject. }
    ceCustom
  );

const
  DefaultCodeEditor = ceAutodetect;

var
  { Which code editor to use. Current user preference. }
  CodeEditor: TCodeEditor;
  { Code editor used to open Pascal files, when CodeEditor = ceCustom. }
  CodeEditorCommand: String;
  { Code editor used to open Pascal files with line and number, when CodeEditor = ceCustom. }
  CodeEditorCommandLineColumn: String;
  { Code editor used to open project, when CodeEditor = ceCustom. }
  CodeEditorCommandProject: String;

var
  { Which compiler to use (passed to build tool). Current user preference. }
  Compiler: TCompiler;

const
  DefaultMuteOnRun = true;
  DefaultEditorVolume = 1.0;

var
  { Mute/restore when you run the application. }
  MuteOnRun: Boolean;
  EditorVolume: Single;

  { Current state on "running application" for the purpose of implementing
    MuteOnRun. }
  RunningApplication: Boolean;

{ Update SoundEngine.Volume based on
  global MuteOnRun, EditorVolume, RunningApplication. }
procedure SoundEngineSetVolume;

{ Update SoundEngine.Volume based on
  global MuteOnRun, RunningApplication and parameter FakeVolume. }
procedure SoundEngineSetVolume(const FakeVolume: Single);

{ Find Visual Studio Code, or '' if cannot find. }
function FindExeVSCode(const ExceptionWhenMissing: Boolean): String;

{ Return auto-detected code editor, never ceAutodetect.
  @eaises Exception if cannot autodetect, no IDE available. }
function AutodetectCodeEditor: TCodeEditor;

{ Consider this path as candidate for CastleEngineOverridePath,
  check (without setting anything) whether it would make sense. }
function CgePathStatus(const CgePath: String; out StatusText: String): Boolean;
function CgePathStatus(const CgePath: String): Boolean;

{ Set SaveDialog.DefaultExt / Filter, based on ComponentToSave class. }
procedure PrepareSaveDesignDialog(const SaveDialog: TSaveDialog;
  const ComponentToSave: TComponent);

{ Should we use colors and icons for dark theme. Based on luminance. }
function UseIconsAndColorsForDarkTheme: Boolean;

type
  { Saved selection state,
    that can be restored even after loading a new version of design from file.
    We save component by a Name (String) to avoid worrying about references
    to freed component, and to be able to restore it even on new design file. }
  TSavedSelection = record
    SelectedComponent: String;
    CurrentViewport: String;
    { Index of the value selected/edited in the Object Inspector. }
    ItemIndex: Integer;
    { Index of the tab of Object Inspector. }
    TabIndex: Integer;
    class function Equals(const A, B: TSavedSelection): Boolean; static;
  end;

{ Show last file modification time as nice string. }
function FileDateTimeStr(const FileName: String): String;

implementation

uses
  SysUtils, Graphics, TypInfo, Generics.Defaults, Math, DateUtils,
  CastleUtils, CastleLog, CastleSoundEngine, CastleFilesUtils, CastleLclUtils,
  CastleComponentSerialize, CastleUiControls, CastleCameras, CastleTransform,
  CastleColors, CastleTimeUtils, CastleUriUtils,
  ToolCompilerInfo, ToolCommonUtils;

procedure TMenuItemHelper.SetEnabledVisible(const Value: Boolean);
begin
  Visible := Value;
  Enabled := Value;
end;

{ TAsynchronousProcessQueue.TQueueItem --------------------------------------- }

constructor TAsynchronousProcessQueue.TQueueItem.Create;
begin
  inherited;
  Parameters := TCastleStringList.Create;
end;

destructor TAsynchronousProcessQueue.TQueueItem.Destroy;
begin
  FreeAndNil(Parameters);
  inherited;
end;

{ TAsynchronousProcessQueue -------------------------------------------------- }

constructor TAsynchronousProcessQueue.Create;
begin
  inherited;
  Queue := TQueueItemList.Create(true);
  FQueuePosition := -1;
end;

destructor TAsynchronousProcessQueue.Destroy;
begin
  FreeAndNil(AsyncProcess);
  FreeAndNil(Queue);
  inherited;
end;

procedure TAsynchronousProcessQueue.Start;
begin
  if Queue.Count = 0 then
    Exit; // nothing to do

  FQueuePosition := 0;
  CreateAsyncProcess;
end;

procedure TAsynchronousProcessQueue.CreateAsyncProcess;
begin
  AsyncProcess := TAsynchronousProcess.Create;
  AsyncProcess.ExeName := Queue[FQueuePosition].ExeName;
  AsyncProcess.CurrentDirectory := Queue[FQueuePosition].CurrentDirectory;
  AsyncProcess.Parameters.Assign(Queue[FQueuePosition].Parameters);
  AsyncProcess.OutputList := OutputList;
  AsyncProcess.Start;
end;

procedure TAsynchronousProcessQueue.Update;
var
  LastLineKind: TOutputKind;
  SuccessfullyFinishedAll: Boolean;
begin
  if AsyncProcess <> nil then
  begin
    AsyncProcess.Update;
    if not AsyncProcess.Running then
    begin
      SuccessfullyFinishedAll := false;

      if AsyncProcess.ExitStatus <> 0 then
      begin
        LastLineKind := okError;
        FQueuePosition := Queue.Count; // abort executing rest
      end else
      begin
        Inc(FQueuePosition);
        LastLineKind := okImportantInfo;
        // if we just finished the last queue item, then success
        SuccessfullyFinishedAll := FQueuePosition = Queue.Count;
      end;
      OutputList.AddSeparator;
      OutputList.AddLine('Command finished with status ' + IntToStr(AsyncProcess.ExitStatus) + '.',
        LastLineKind);
      FreeAndNil(AsyncProcess);

      // create next process in queue
      if FQueuePosition < Queue.Count then
        CreateAsyncProcess
      else
      begin
        // no more processes (regardless if we done all, or interrupted because of some error)
        if Assigned(OnFinished) then
          OnFinished(Self);
      end;

      if SuccessfullyFinishedAll and Assigned(OnSuccessfullyFinishedAll) then
        OnSuccessfullyFinishedAll(Self);
    end;
  end;
end;

function TAsynchronousProcessQueue.Running: Boolean;
begin
  Result := (FQueuePosition >= 0) and (FQueuePosition < Queue.Count);
end;

procedure TAsynchronousProcessQueue.TerminateChildrenHarder;
begin
  if (AsyncProcess <> nil) and AsyncProcess.Running then
    AsyncProcess.TerminateChildrenHarder;
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
  FreeAndNil(Environment);
  inherited;
end;

procedure TAsynchronousProcess.Start;
var
  S, LogLine: String;
begin
  { copy environment }
  Environment := EnvironmentStrings;

  { Extend PATH, to effectively use FpcCustomPath and LazarusCustomPath
    in the build tool.
    Initially we used to just pass them in special environment variables,
    and restore in ToolCompilerInfo initialization, but this is not enough:
    on Windows, calling "windres" requires that "cpp" is also on PATH.
    It seems more reliable to just add them to PATH. }
  Environment.Values['PATH'] := PathExtendForFpcLazarus(Environment.Values['PATH']);
  WritelnLog('Calling process with extended PATH: ' + Environment.Values['PATH']);

  { Pass CASTLE_ENGINE_PATH to build tool, to use the same CGE as detected by editor.
    This means that e.g. editor that autodetects CGE (based on GetCastleEnginePathFromExeName,
    because editor exe is in <cge>/tools/castle-editor/castle-editor)
    invokes build tool in local bin (like ~/bin)
    and the build tool uses the same <cge> as detected by editor. }
  if CastleEnginePath <> '' then
  begin
    Environment.Values['CASTLE_ENGINE_PATH'] := CastleEnginePath;
    WritelnLog('Calling process with extended CASTLE_ENGINE_PATH: ' + Environment.Values['CASTLE_ENGINE_PATH']);
  end;

  { create Process and call Process.Execute }
  Process := TProcess.Create(nil);
  Process.Executable := ExeName;
  if CurrentDirectory <> '' then
    Process.CurrentDirectory := CurrentDirectory;
  for S in Parameters do
    Process.Parameters.Add(S);
  { on Windows, we need swoHide, otherwise console appears for build tool.
    Note that poNoConsole is not a solution, as it prevents castle-engine
    console, but FPC (called by castle-engine) console is still visible. }
  Process.ShowWindow := swoHide;
  Process.Options := [poUsePipes, poStderrToOutput];
  Process.Environment := Environment;
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

  function LineProcessInternalInfo(const Line: String): Boolean;
  const
    Prefix = 'Castle Game Engine Internal: ProcessID: ';
  var
    ParsedProcessId: TProcessId;
  begin
    Result := false;
    if IsPrefix(Prefix, Line, false) and
       TryStrToInt64(PrefixRemove(Prefix, Line, false), ParsedProcessId) then
    begin
      ChildProcessId := ParsedProcessId;
      Result := true;
    end;
  end;

  function LineOutputKind(const Line: String): TOutputKind;
  begin
    if (Pos(') Error', Line) <> 0) or
       (Pos(') Fatal', Line) <> 0) then
      Result := okError
    else
    if (Pos(') Warning', Line) <> 0) then
      Result := okWarning
    else
      Result := okInfo;
  end;

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
    a process that didn't yet dump all it's output to OutputList. }
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
      if not LineProcessInternalInfo(Line) then
        OutputList.AddLine(PendingLines, LineOutputKind(PendingLines));
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
      if not LineProcessInternalInfo(Line) then
        OutputList.AddLine(Line, LineOutputKind(Line));

      PendingLines := SEnding(PendingLines, ProcessedLength + 1);
    end else
      Break;
  until false;

  // if we get a lot of output, maybe more than ReadMaxSize, then keep reading!
  if ReadMaxSize = ReadBytes then
    Update;
end;

procedure TAsynchronousProcess.TerminateChildrenHarder;
begin
  if ChildProcessId <> 0 then
  begin
    OutputList.AddLine(Format('Stopping child process (id: %d)', [ChildProcessId]), okInfo);
    StopProcess(ChildProcessId);
  end;
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

  { (On Windows only)
    For unknown reason (LCL bug? our bug?), sometimes OutputInfo is nil,
    even though we clear the List.Items and we always add to it by
    AddLine (List.Items.AddObject(S, OutputInfoPerKind[Kind])).

    This can be reproduced in various situations if you hit F9:
    1. after switching from Lazarus -> CGE editor
    2. open CGE editor, some project, and use "Restart Editor (with custom components)"
    3. open CGE editor, some project, open some design, hit F9

    It is *not* 100% reproducible always. Case 1. and 2. was definitely
    observed (by Michalis and others) but the crash can just disappear by itself
    after you reproduce it a few times.
    Case 3. proved to be most reliably reproducible.

    Seen with
    - Lazarus 2.0.10 + FPC 3.2.0
    - Lazarus 2.0.12 + FPC 3.2.2
    Only seen on Windows/x86_64 (though we make a workaround general).

    The log confirms the problem:
    - AddLine added
        'Running "...\castle-engine.exe --mode=debug compile"'
      with OutputInfo <> nil
    - But then TOutputList.DrawItem is executed with
        Index = 0
        List.Items[Index] = 'Running "...\castle-engine.exe --mode=debug compile"'
        OutputInfo = nil
  }

  if OutputInfo <> nil then
  begin
    case OutputInfo.Kind of
      okImportantInfo: C.Font.Bold := true;
      okWarning:
        begin
          C.Brush.Color := clYellow;
          { If the font color is too light on yellow, change it }
          if GrayscaleValue(ColorToVector3(C.Font.Color)) > 0.75 then
          begin
            if List.ItemIndex = Index then
              C.Font.Color := clBlue
            else
              C.Font.Color := clBlack;
          end;
        end;
      okError: C.Brush.Color := clRed;
    end;
  end else
  begin
    WritelnLog('Missing OutputInfo for the line "%s", known bug (probably in LCL) on Windows', [
      List.Items[Index]
    ]);
    // assuming okImportantInfo, as the problem occurs with 1st line in list
    C.Font.Bold := true;
  end;

  C.FillRect(ARect);
  InternalDrawItem(List, C, ARect, List.Items[Index]);
end;

constructor TOutputList.Create(const AList: TListBox);

  { Measure font height in pixels. }
  function GetRowHeight: Integer;
  begin
    FList.Canvas.Font := FList.Font;
    Result := FList.Canvas.TextHeight('Wg');
  end;

var
  Kind: TOutputKind;
begin
  inherited Create;

  FList := AList;
  FList.Style := lbOwnerDrawFixed;
  FList.OnDrawItem := @DrawItem;
  FList.ItemHeight := GetRowHeight + 4;

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

destructor TOutputList.Destroy;
var
  Kind: TOutputKind;
begin
  for Kind in TOutputKind do
    FreeAndNil(OutputInfoPerKind[Kind]);
  inherited;
end;

procedure TOutputList.AddLine(const S: String; const Kind: TOutputKind);
var
  IsBottom: Boolean;
begin
  {
  IsBottom :=
    (List.Items.Count = 0) or

    // TODO: doesn't work reliably
    // List.ItemFullyVisible(List.Items.Count - 1)

    // TODO: doesn't work reliably
    // checking is visible "List.Items.Count - 1" is not good enough
    (List.Items.Count = 1) or
    List.ItemVisible(List.Items.Count - 2);
  }
  IsBottom := true;

  List.Items.AddObject(S, OutputInfoPerKind[Kind]);

  { scroll to bottom once new message received,
    but only if already looking at bottom (otherwise we would prevent
    user from easily browsing past messages). }
  if IsBottom then
    List.TopIndex := List.Items.Count - 1;
end;

(* Unused:

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
*)

procedure TOutputList.AddSeparator;
begin
  AddLine('', okInfo);
end;

procedure TOutputList.Clear;
begin
  List.Items.Clear;
end;

{ global routines ------------------------------------------------------------ }

procedure ErrorBox(const Message: String);
begin
  MessageDlg('Error', Message, mtError, [mbOK], 0);
end;

procedure InfoBox(const Message: String);
begin
  MessageDlg('Information', Message, mtInformation, [mbOK], 0);
end;

procedure WarningBox(const Message: String);
begin
  MessageDlg('Warning', Message, mtWarning, [mbOK], 0);
end;

function YesNoBox(const Message: String): Boolean;
begin
  Result := MessageDlg('Question', Message, mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

function YesNoBox(const Caption, Message: String): Boolean;
begin
  Result := MessageDlg(Caption, Message, mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

procedure SetEnabledVisible(const C: TControl; const Value: Boolean);
begin
  C.Enabled := Value;
  C.Visible := Value;
end;

function ApiReferenceUrl: String;
// TODO: Make it possible to set from preferences, or make it just the default behavior?
{.$define CASTLE_PREFER_OFFLINE_API_DOCS}

{$ifdef CASTLE_PREFER_OFFLINE_API_DOCS}
var
  LocalDocsPath: String;
{$endif}
begin
  {$ifdef CASTLE_PREFER_OFFLINE_API_DOCS}
  if CastleEnginePath <> '' then
  begin
    LocalDocsPath := CastleEnginePath + 'doc' + PathDelim + 'reference' + PathDelim;
    if DirectoryExists(LocalDocsPath) then
      Exit(FilenameToURISafe(LocalDocsPath));
  end;
  {$endif}

  Result := 'https://castle-engine.io/apidoc/html/';
end;

function ApiReference(const PropertyObject: TObject;
  const PropertyName, PropertyNameForLink: String): String;

  { Knowing that property PropInfo is part of class C,
    determine the class where it's actually declared (C or ancestor of C).

    Note that we search for property using PropInfo, not a string PropertyName,
    this means that we don't mix a property with the same name that
    obscures ancestor property (same name, but actually different property). }
  function ClassOfPropertyDeclaration(const C: TClass; const PropInfo: PPropInfo): TClass;
  var
    ParentC: TClass;
    PropList: PPropList;
    PropCount, I: Integer;
  begin
    ParentC := C.ClassParent;
    if ParentC = nil then
      Exit(C); // no ancestor

    PropCount := GetPropList(ParentC, PropList);
    for I := 0 to PropCount - 1 do
      if PropList^[I] = PropInfo then
        // property found in ancestor
        Exit(ClassOfPropertyDeclaration(ParentC, PropInfo));

    // property not found in ancestor
    Exit(C);
  end;

  function FpDocSuffix(const LinkUnitName, LinkClassName, LinkPropertyName: String): String;
  begin
    Result := LowerCase(LinkUnitName) + '/' + LowerCase(LinkClassName) + '.';
    if LinkPropertyName <> '' then
      Result := Result + LowerCase(LinkPropertyName) + '.';
    Result := Result + 'html';
  end;

  function PasDocSuffix(const LinkUnitName, LinkClassName, LinkPropertyName: String): String;
  begin
    Result := LinkUnitName + '.' + LinkClassName + '.html';
    if LinkPropertyName <> '' then
      Result := Result + '#' + LinkPropertyName;
  end;

var
  LinkUnitName, LinkClassName, LowerLinkUnitName, LinkPropertyName: String;
  PropInfo: PPropInfo;
  ClassOfProperty: TClass;
begin
  LinkUnitName := PropertyObject.UnitName;
  LinkClassName := PropertyObject.ClassName;
  LinkPropertyName := '';

  if PropertyName <> '' then
  begin
    { PropertyName doesn't necessarily belong to the exact PropertyObject class,
      it may belong to ancestor. E.g. TCastleScene.Url is actually from
      TCastleSceneCore.
      This is important to construct API links.
      Unfortunately GetPropInfo doesn't have this info directly. }

     PropInfo := GetPropInfo(PropertyObject, PropertyName);
     if PropInfo <> nil then
     begin
       ClassOfProperty := ClassOfPropertyDeclaration(PropertyObject.ClassType, PropInfo);
       LinkClassName := ClassOfProperty.ClassName;
       LinkUnitName := ClassOfProperty.UnitName;
       LinkPropertyName := PropertyNameForLink;
     end else
       WritelnWarning('Cannot get property info "%s"', [PropertyName]);
  end;

  { construct UrlSuffix, knowing LinkUnit/Class/MemberName }
  LowerLinkUnitName := LowerCase(LinkUnitName);
  if (LowerLinkUnitName = 'sysutils') or
     (LowerLinkUnitName = 'math') or
     (LowerLinkUnitName = 'system') or
     (LowerLinkUnitName = 'classes') then
  begin
    // adjust for fpdoc links in FPC
    Result := FpcRtlApiReferenceUrl + FpDocSuffix(LinkUnitName, LinkClassName, LinkPropertyName);
  end else
  if (LowerLinkUnitName = 'controls') then
  begin
    // adjust for fpdoc links in LCL
    Result := LclApiReferenceUrl + FpDocSuffix(LinkUnitName, LinkClassName, LinkPropertyName);
  end else
  begin
    // adjust for PasDoc links in CGE
    Result := ApiReferenceUrl + PasDocSuffix(LinkUnitName, LinkClassName, LinkPropertyName);
  end;
end;

function CompareRegisteredComponent({$ifdef GENERICS_CONSTREF}constref{$else}const{$endif}
  Left, Right: TRegisteredComponent): Integer;
var
  I: Integer;
begin
  for I := 0 to Min(Length(Left.Caption), Length(Right.Caption)) - 1 do
  begin
    Result := AnsiCompareStr(Left.Caption[I], Right.Caption[I]);
    if Result <> 0 then Exit;
  end;

  { When all common parts are the same, let the shorter one be considered smaller.
    So < 0 when Length(Left.Caption) < Length(Right.Caption). }
  Result := Length(Left.Caption) - Length(Right.Caption);
end;

procedure BuildComponentsMenu(
  const ParentUserInterface, ParentTransform, ParentBehavior, ParentNonVisual: TMenuItem;
  const OnClickEvent: TNotifyEvent);

  function CreateMenuItemForComponent(const OwnerAndParent: TMenuItem;
    const R: TRegisteredComponent; const CaptionPart: Integer = 0): TMenuItem;
  var
    S: String;
  begin
    if OwnerAndParent = nil then
      Exit; // exit if relevant ParentXxx is nil

    if CaptionPart = Length(R.Caption) - 1 then
    begin
      { create last part, to actually invoke OnClickEvent }
      Result := TMenuItem.Create(OwnerAndParent);
      S := R.Caption[CaptionPart] + ' (' + R.ComponentClass.ClassName + ')';
      if R.IsDeprecated then
        S := '(Deprecated) ' + S;
      Result.Caption := S;
      Result.Tag := PtrInt(Pointer(R));
      Result.OnClick := OnClickEvent;
      OwnerAndParent.Add(Result);
    end else
    begin
      { create intermediate submenu part }
      Result := OwnerAndParent.Find(R.Caption[CaptionPart]);
      if Result = nil then
      begin
        Result := TMenuItem.Create(OwnerAndParent);
        Result.Caption := R.Caption[CaptionPart];
        OwnerAndParent.Add(Result);
      end;
      { recursive call to create deeper menu level }
      CreateMenuItemForComponent(Result, R, CaptionPart + 1);
    end;
  end;

type
  TRegisteredComponentComparer = specialize TComparer<TRegisteredComponent>;
var
  R: TRegisteredComponent;
begin
  { While RegisteredComponents is documented as "read-only",
    we knowingly break it here for internal CGE purposes.
    We need some reliable order of this list (as "RegisterSerializableComponent" may be called in any order),
    for now alphabetic order seems good enough. }
  RegisteredComponents.Sort(TRegisteredComponentComparer.Construct(@CompareRegisteredComponent));

  { add non-deprecated components }
  for R in RegisteredComponents do
    if not R.IsDeprecated then
    begin
      if R.ComponentClass.InheritsFrom(TCastleUserInterface) then
        CreateMenuItemForComponent(ParentUserInterface, R)
      else
      if R.ComponentClass.InheritsFrom(TCastleTransform) then
        CreateMenuItemForComponent(ParentTransform, R)
      else
      if R.ComponentClass.InheritsFrom(TCastleBehavior) then
        CreateMenuItemForComponent(ParentBehavior, R)
      else
        CreateMenuItemForComponent(ParentNonVisual, R);
    end;

  (*
  Don't show deprecated for now, keep the menu clean.

  { add separators from deprecated }
  MenuItem := TMenuItem.Create(ParentUserInterface);
  MenuItem.Caption := '-';
  ParentUserInterface.Add(MenuItem);

  MenuItem := TMenuItem.Create(ParentTransform);
  MenuItem.Caption := '-';
  ParentTransform.Add(MenuItem);

  { add deprecated components }
  for R in RegisteredComponents do
    if R.IsDeprecated then
    begin
      ... same code as above
    end;
  *)
end;

procedure SoundEngineSetVolume;
begin
  SoundEngineSetVolume(EditorVolume);
end;

procedure SoundEngineSetVolume(const FakeVolume: Single);
begin
  if MuteOnRun and RunningApplication then
    SoundEngine.Volume := 0
  else
    SoundEngine.Volume := FakeVolume;
end;

function FindExeVSCode(const ExceptionWhenMissing: Boolean): String;
begin
  Result := FindExe('code');

  (*Failed workaround to fix handling filenames/dirnames with spaces inside
    as "code" arguments.
    I hoped that calling code.exe directly, instead of code.cmd, will help
    -- it doesn't.

  {$ifdef MSWINDOWS}
  if (Result <> '') and (SameText(ExtractFileName(Result), 'code.cmd')) then
    Result := ParentPath(ExtractFileDir(Result), false) + 'code.exe';
  {$endif}
  *)

  if (Result = '') and ExceptionWhenMissing then
    raise EExecutableNotFound.Create('Cannot find Visual Studio Code. Make sure it is installed, and available on environment variable $PATH (there should be an option to set this up during VS Code installlation).');
end;

(*Another failed workaround to fix handling filenames/dirnames with spaces inside
  as "code" arguments.
  I hoped that using URL, with spaces encoded as %20, will solve the problem.
  Unfortunately VS code doesn't seem to understand %20 in URLs at all.

function PathToVSCode(const Path: String): String;
var
  URI: TURI;
begin
  Result := Path;
  Assert(IsPathAbsolute(Path));

  FillByte(URI, SizeOf(URI), 0);
  URI.Protocol := 'vscode';
  { We want // between vscode and "file", see
    https://code.visualstudio.com/docs/editor/command-line }
  URI.HasAuthority := true;
  URI.Document := 'file' +
    {$ifdef MSWINDOWS} '/' + SReplaceChars(Path, '\', '/')
    {$else} Path
    {$endif};
  Result := EncodeURI(URI);
end;
*)

function AutodetectCodeEditor: TCodeEditor;
begin
  if FindExeLazarusIDE(false) <> '' then
    Result := ceLazarus
  else
  if FindDelphiPath(false) <> '' then
    Result := ceDelphi
  else
  if FindExeVSCode(false) <> '' then
    Result := ceVSCode
  else
    raise Exception.Create('Cannot auto-detect IDE. Install one of the supported IDEs: Lazarus, Delphi or Visual Studio Code.');
end;

function CgePathStatus(const CgePath: String; out StatusText: String): Boolean;

  function RemoveCommitHash(const Ver: String): String;
  var
    I: Integer;
  begin
    I := Pos(' (commit ', Ver);
    if I <> 0 then
      Result := Copy(Ver, 1, I - 1)
    else
      Result := Ver;
  end;

var
  VersionFile, VersionLine, Version, EditorVersion: String;
  VersionContentsList: TStringList;
begin
  if CgePath = '' then
  begin
    StatusText := 'Status: Cannot auto-detect engine location, set it manually above';
    Exit(false);
  end;

  VersionFile := InclPathDelim(CgePath) + 'src' + PathDelim +
    'base' + PathDelim + 'castleversion.inc';
  if not FileExists(VersionFile) then
  begin
    StatusText := Format('Status: Invalid, cannot find file "%s"', [VersionFile]);
    Exit(false);
  end;

  VersionContentsList := TStringList.Create;
  try
    try
      VersionContentsList.LoadFromFile(VersionFile);
    except
      StatusText := Format('Status: Invalid, cannot read file "%s"', [VersionFile]);
      Result := true;
    end;

    if VersionContentsList.Count = 0 then
    begin
      StatusText := Format('Status: Invalid, empty file "%s"', [VersionFile]);
      Exit(false);
    end;

    VersionLine := VersionContentsList[0];
    if (not SCharIs(VersionLine, 1, '''')) or
       (not SCharIs(VersionLine, Length(VersionLine), '''')) then
    begin
      StatusText := Format('Status: Invalid, first line is not a String: "%s"', [VersionFile]);
      Exit(false);
    end;
  finally FreeAndNil(VersionContentsList) end;

  Version := Copy(VersionLine, 2, Length(VersionLine) - 2);

  { Due to the way pack_release.sh works, actually Version will never have a hash.
    Only CastleEngineVersion can have it,
    and it would cause mismatches, to remove it. }
  Version := RemoveCommitHash(Version);
  EditorVersion := RemoveCommitHash(CastleEngineVersion);

  if Version <> EditorVersion then
  begin
    StatusText := Format('Status: Valid engine, but version mismatch with editor: "%s" vs editor "%s"', [
      Version,
      EditorVersion
    ]);
    Exit(False);
  end;

  StatusText := 'Status: OK (engine found, version matches editor)';
  Result := true;
end;

function CgePathStatus(const CgePath: String): Boolean;
var
  IgnoreStatusText: String;
begin
  Result := CgePathStatus(CgePath, IgnoreStatusText);
end;

procedure PrepareSaveDesignDialog(const SaveDialog: TSaveDialog; const ComponentToSave: TComponent);
begin
  if ComponentToSave is TCastleUserInterface then
  begin
    SaveDialog.DefaultExt := 'castle-user-interface';
    SaveDialog.Filter := 'CGE User Interface Design (*.castle-user-interface)|*.castle-user-interface|All Files|*';
  end else
  if ComponentToSave is TCastleTransform then
  begin
    { We modify both Filter and DefaultExt, otherwise (at least on GTK2)
      the default extension (for filter like '*.castle-user-interface;*.castle-transform')
      would still be castle-user-interface. I.e. DefaultExt seems to be ignored,
      and instead GTK applies first filter. }
    SaveDialog.DefaultExt := 'castle-transform';
    SaveDialog.Filter := 'CGE Transform Design (*.castle-transform)|*.castle-transform|All Files|*';
  end else
  begin
    SaveDialog.DefaultExt := 'castle-component';
    SaveDialog.Filter := 'CGE Component Design (*.castle-component)|*.castle-component|All Files|*';
  end;
end;

function UseIconsAndColorsForDarkTheme: Boolean;
var
  Luminance: Single;
begin
  Luminance := GrayscaleValue(ColorToVector3(clForm));
  Result := Luminance < 180 / 255;
end;

class function TSavedSelection.Equals(const A, B: TSavedSelection): Boolean; static;
begin
  Result :=
    (A.SelectedComponent = B.SelectedComponent) and
    (A.CurrentViewport = B.CurrentViewport) and
    (A.ItemIndex = B.ItemIndex) and
    (A.TabIndex = B.TabIndex);
end;

function FileDateTimeStr(const FileName: String): String;

  function RoundUp(const Val: Double): Int64;
  begin
    Result := Trunc(Val);
    if Frac(Val) > 0 then
      Inc(Result);
  end;

var
  FileDateTime: TDateTime;
  Secs, Mins: Int64;
begin
  if FileAge(FileName, FileDateTime) then
  begin
    Secs := RoundUp(SecondSpan(Now, FileDateTime));
    if Secs < 60 then
      FileDateTimeStr := Format('%d second%s ago', [
        Secs,
        Iff(Secs > 0, 's', '')
      ])
    else
    begin
      Mins := RoundUp(MinuteSpan(Now, FileDateTime));
      if Mins < 60 then
        FileDateTimeStr := Format('%d minute%s ago', [
          Mins,
          Iff(Mins > 0, 's', '')
        ])
      else
        FileDateTimeStr := DateTimeToAtStr(FileDateTime);
    end;
  end else
    Result := 'Unknown';
end;

end.
