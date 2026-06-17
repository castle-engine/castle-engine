{%MainUnit castleinternalprocess.pas}

{$ifdef read_interface}

{ Copy current environemt variables.
  Useful as a starting point for the OverrideEnvironment parameter of
  @link(ExecuteCommand). }
function EnvironmentStrings: TStringList;

{ Find absolute filename of an executable, given its name.

  ExeName can be either an absolute filename (with extension like @code(.exe)
  on Windows) or just the name of executable to find on environment variable
  $PATH (without extension, even on Windows).

  If ExeName is not found, raises an exception.

  There's no need to process your ExeName with this function before passing it
  to @link(ExecuteCommand). But you should process it with this function
  if you use directly TCastleProcess and want to set
  @code(TCastleProcess.Executable). }
function AbsoluteExeName(const ExeName: String): String;

type
  { Line filtering used by @link(ExecuteCommand) and friends.
    You can process the variable Line in any way.
    Return whether to pass it (return @false to completely discard it,
    any edits to Line don't matter then). }
  TLineFiltering = function (var Line: String; const Data: Pointer): Boolean;

  { Possible flags for @link(ExecuteCommand) and friends. }
  TExecuteCommandFlag = (ecNoConsole);

  { Set of flags for @link(ExecuteCommand) and friends. }
  TExecuteCommandFlags = set of TExecuteCommandFlag;

{ Run command in the given directory with the given arguments,
  wait for it to finish.

  The stdout/stderr of the command are not captured anywhere,
  they are passed-through as our stdout/stderr.
  This is in contrast to ExecuteCommandCapture that captures output to String.

  Optionally can override environment variables.
  If OverrideEnvironment is <> nil, then we override environment variables for
  the executed process. Example usage, to merely override a few select
  variables, looks like this:

  @longCode(#
  var
    OverrideEnvironment: TStringList;
  begin
    OverrideEnvironment := EnvironmentStrings; // start from current environment
    try
      OverrideEnvironment.Values['MY_VARIABLE_1_NAME'] := 'my_variable_1_value';
      OverrideEnvironment.Values['MY_VARIABLE_2_NAME'] := 'my_variable_2_value';
      ExecuteCommandWait(CurrentDirectory, ExeName, Options, OverrideEnvironment);
    finally
      FreeAndNil(OverrideEnvironment);
    end;
  end;
  #)

  This routine is similar to the standard ExecuteProcess in FPC.

  @param(CurrentDirectory Directory in which to run the command.
    The child process will see this as its current directory (GetCurrentDir()
    in Pascal).
    If empty (''), uses current directory of our process.)

  @param(ExeName Executable name, either an absolute filename (with extension
    like @code(.exe) on Windows) or just the name of executable to find on
    environment variable $PATH (without extension, even on Windows).)

  @param(OverrideEnvironment
    When not nil, overrides environment variables for the process.
    You can start from EnvironmentStrings.
    Remember to free the passed TStringList after calling this routine.) }
procedure ExecuteCommandWait(const CurrentDirectory: String;
  ExeName: String;
  const Options: array of string;
  out ExitStatus: Integer;
  const Flags: TExecuteCommandFlags = [];
  const OverrideEnvironment: TStringList = nil);

{ Run command in the given directory with the given arguments,
  wait for it to finish.

  Checks the exit status, and raises an exception if it is not 0.
  This is in contrast to ExecuteCommandWait that doesn't check the exit status,
  and only returns it. }
procedure ExecuteCommandCheckStatus(const CurrentDirectory: String;
  const ExeName: String;
  const Options: array of string;
  const Flags: TExecuteCommandFlags = [];
  const OverrideEnvironment: TStringList = nil);

{$ifdef FPC} // needs full TProcess

{ Run command in the given directory with the given arguments,
  gathering the output (including error output, so we capture stdout and stderr)
  to a string.

  Optionally can filter the gathered output (if LineFiltering is not nil).

  Optionally can override environment variables.
  See @link(ExecuteCommandWait) for details and example.

  This routine is similar to the standard Process.RunCommandIndir in FPC >= 2.6.4,
  but also captures error output, and enables filtering, and enables
  environment variable overrides.

  @param(CurrentDirectory Directory in which to run the command.
    The child process will see this as its current directory (GetCurrentDir()
    in Pascal).
    If empty (''), uses current directory of our process.)

  @param(ExeName Executable name, either an absolute filename (with extension
    like @code(.exe) on Windows) or just the name of executable to find on
    environment variable $PATH (without extension, even on Windows).)

  @param(OutputString Standard output (stdout) and standard error output
    (stderr) of the command.)

  @param(LineFiltering Allows to filter the gathered lines, see TLineFiltering docs.)

  @param(LineFilteringData Passed to the LineFiltering callback.
    Ignored if LineFiltering is nil.)

  @param(OverrideEnvironment
    When not nil, overrides environment variables for the process.
    You can start from EnvironmentStrings.
    Remember to free the passed TStringList after calling this routine.)

  @param(PassThrough
    If @true, we let the collected output (stdout and stderr combined)
    to go to our output.) }
procedure ExecuteCommand(
  const CurrentDirectory: String;
  ExeName: String; const Options: array of string;
  out OutputString: String; out ExitStatus: integer;
  const LineFiltering: TLineFiltering = nil;
  const LineFilteringData: Pointer = nil;
  const Flags: TExecuteCommandFlags = [];
  const OverrideEnvironment: TStringList = nil;
  const PassThrough: Boolean = false); overload;

{$endif FPC}

{$endif read_interface}

{$ifdef read_implementation}

{ Output capturing API ------------------------------------------------------- }

type
  { Read from Source stream (with the option to read without blocking) all contents.

    Collected contents can be obtained by calling @link(Collected)
    (it should be used only once all ReadAvailable and ReadEverything calls are done).

    The TCaptureOutput class doesn't process the contents in any way.
    When it reads newline characters (#10, #13) they are treated
    just like any other character.
    Descendants like TCaptureOutputFilter may have different behavior. }
  TCaptureOutput = class
  strict private
    const
     { Original code for this adjusted from fpc/trunk/packages/fcl-process/src/process.pp ,
       although reworked many times.
       MaxReadBytes is not too small to avoid fragmentation when reading large files. }
      MaxReadBytes = 65536;
    var
      OutputUsefulCount: Integer;
      OutputString: String;
  protected
    function ReadCore: Integer; virtual;
  public
    Source: TStream;

    { Also write everything you collect to out output (stdout).
      Note that it's not a perfect "passthrough": we collect both stdout and stderr
      from the child process, and output it always on our stdout.
      So we redirect child's stdout to our stderr. }
    PassThrough: Boolean;

    { Read from Source stream as much bytes as possible (without blocking waiting for more). }
    procedure ReadAvailable;

    { Read from Source stream all remaining bytes. }
    procedure ReadEverything;

    { Return read contents so far. Call this only after all the reading is done. }
    function Collected: String; virtual;

    { Create new instance of TCaptureOutput or a descendant,
      like TCaptureOutputFilter, suitable for given parameters. }
    class function Construct(const ASource: TStream;
      const ALineFiltering: TLineFiltering;
      const ALineFilteringData: Pointer): TCaptureOutput; static;
  end;

  { API just like ancestor, but filters the read lines through LineFiltering callback.
    Output line endings are also converted to native (LineEnding) along the way. }
  TCaptureOutputFilter = class(TCaptureOutput)
  strict private
    { Note that TCaptureOutputFilter has entirely different implementation than
      ancestor. While it also defines private MaxReadBytes and OutputString,
      they are used in very different way. }
    const
      MaxReadBytes = 65536;
    var
      OutputString: String;
      Buffer: String;
      PendingLine: String;
  protected
    function ReadCore: Integer; override;
  public
    LineFiltering: TLineFiltering; //< Never @nil
    LineFilteringData: Pointer;
    constructor Create;
    function Collected: String; override;
  end;

{ Output capturing: TCaptureOutput -------------------------------- }

function TCaptureOutput.ReadCore: Integer;
begin
  SetLength(OutputString, OutputUsefulCount + MaxReadBytes);
  Result := Source.Read(OutputString[1 + OutputUsefulCount], MaxReadBytes);
  if PassThrough then
    Write(Copy(OutputString, 1 + OutputUsefulCount, Result));
  if Result > 0 then
    Inc(OutputUsefulCount, Result);
end;

procedure TCaptureOutput.ReadAvailable;
var
  NumBytes: Integer;
begin
  NumBytes := ReadCore;
  if NumBytes = 0 then
  begin
    Sleep(100); // when no data, wait a bit instead of wasting CPU trying to query stream
    Exit;
  end;
end;

procedure TCaptureOutput.ReadEverything;
var
  NumBytes: Integer;
begin
  repeat
    NumBytes := ReadCore;
  until NumBytes <= 0;
end;

function TCaptureOutput.Collected: String;
begin
  SetLength(OutputString, OutputUsefulCount);
  Result := OutputString;
end;

class function TCaptureOutput.Construct(const ASource: TStream;
  const ALineFiltering: TLineFiltering;
  const ALineFilteringData: Pointer): TCaptureOutput;
begin
  if Assigned(ALineFiltering) then
  begin
    Result := TCaptureOutputFilter.Create;
    TCaptureOutputFilter(Result).LineFiltering := ALineFiltering;
    TCaptureOutputFilter(Result).LineFilteringData := ALineFilteringData;
  end else
  begin
    Result := TCaptureOutput.Create;
  end;
  Result.Source := ASource;
end;

{ Output capturing: TCaptureOutputFilter ------------------------------------- }

constructor TCaptureOutputFilter.Create;
begin
  inherited;
  SetLength(Buffer, MaxReadBytes);
end;

function TCaptureOutputFilter.ReadCore: Integer;
var
  NewlinePos, PreviousPendingLineLength, NextLineStart: Integer;
  Line: String;
  LineAllow: Boolean;
begin
  Result := Source.Read(Buffer[1], MaxReadBytes);

  if Result = 0 then
    Exit;

  PreviousPendingLineLength := Length(PendingLine);
  PendingLine := PendingLine + Copy(Buffer, 1, Result);

  // in a loop, remove as much lines from PendingLine as possible
  repeat
    // Using here CharsPosEx instead of CharsPos
    // is just optimization to not search entire PendingLine every time.
    NewlinePos := CharsPosEx([#10, #13], PendingLine, PreviousPendingLineLength + 1);

    if NewlinePos = 0 then Break;

    // we have a line -- filter it and possibly add to OutputString
    Line := Copy(PendingLine, 1, NewlinePos - 1);
    LineAllow := LineFiltering(Line, LineFilteringData);
    if LineAllow then
    begin
      OutputString := OutputString + Line + LineEnding;
      if PassThrough then
        Writeln(Line);
    end;

    { If this is followed by 2nd newline character, we want to consume it.

      TODO: We here assume that both newline characters will be returned by Source.Read,
      which may not be true (when newline is 2-character, like on #13#10,
      it is possible that line break will be split across MaxReadBytes borders).
      When this assumption will break, we'll just cause an additional newline,
      which is acceptable for now. }

    if (NewlinePos + 1 <= Length(PendingLine)) and
       { check we have #13 followed by #10 or #10 followed by #13.
         Be careful to *not* eat #10 followed by #10, as that would
         make us silently consume empty lines in files with Unix line ending. }
       ( ( (PendingLine[NewlinePos] = #10) and (PendingLine[NewlinePos + 1] = #13) ) or
         ( (PendingLine[NewlinePos] = #13) and (PendingLine[NewlinePos + 1] = #10) ) ) then
      NextLineStart := NewlinePos + 2
    else
      NextLineStart := NewlinePos + 1;

    PendingLine := SEnding(PendingLine, NextLineStart);
    PreviousPendingLineLength := 0; // reset for next CharsPosEx search
  until false;
end;

function TCaptureOutputFilter.Collected: String;
begin
  Result := OutputString + PendingLine;
  PendingLine := '';
end;

{ Environment strings -------------------------------------------------------- }

function EnvironmentStrings: TStringList;

{$ifdef FPC}
// FPC cross-platform implementation using SysUtils.GetEnvironmentXxx.
var
  I: Integer;
  S: String;
begin
  Result := TStringList.Create;
  for I := 1 to GetEnvironmentVariableCount do
  begin
    S := GetEnvironmentString(I);
    { Avoid weird lines "=::=::\" on Windows 11.
      See comments below for Delphi. }
    if (S <> '') and (S[1] <> '=') then
      Result.Add(S);
  end;
end;

{$elseif defined(DELPHI) and defined(MSWINDOWS)}
// For Delphi/Windows, use WinApi GetEnvironmentStrings.
var
  EnvBlock, P: PChar;
  S: String;
begin
  Result := TStringList.Create;
  { GetEnvironmentStrings returns a block of consecutive #0-terminated
    "name=value" strings, the whole block terminated by an additional #0. }
  EnvBlock := GetEnvironmentStrings;
  try
    P := EnvBlock;
    while P^ <> #0 do
    begin
      S := P; // convert the #0-terminated PChar to String
      { Skip the special entries that begin with "=", which would then
        imply env variable with name = '', so unexpected by various APIs,
        TStringList.Values[] is not ready for this.

        On Windows:
        - this claims you may see environment like "=X:",
          https://devblogs.microsoft.com/oldnewthing/20100506-00/?p=14133 ,
          although this is not confirmed on Windows 11.

        - but we see environment "=::=::\" on Windows 11, so avoid it.
          Saw also on https://stackoverflow.com/questions/10431689/what-are-these-strange-environment-variables ,
          without a real explanation (all answers refer to above article
          about "=X:").
      }
      if (S <> '') and (S[1] <> '=') then
         Result.Add(S);
      Inc(P, Length(S) + 1); // advance past this string and its #0
    end;
  finally
    FreeEnvironmentStrings(EnvBlock);
  end;
end;

{$else}
begin
  Result := TStringList.Create;
  WritelnWarning('EnvironmentStrings not implemented for this compiler and platform');
end;

{$endif}

{ AbsoluteExeName ------------------------------------------------------------ }

function AbsoluteExeName(const ExeName: String): String;
begin
  if IsPathAbsolute(ExeName) then
    Result := ExeName
  else
  begin
    { Use FindExe, to use our fixed PathFileSearch.
      It fixes old FPC bug, FPC could accidentally find "ant" directory as "ant"
      executable (likely caused by old FPC FileExists implementation on Unix
      that answered true for directories, fixed since then). }
    Result := FindExe(ExeName);
    if Result = '' then
      raise Exception.CreateFmt('Cannot find "%s" on environment variable $PATH. Make sure "%s" is installed and $PATH is configured correctly',
        [ExeName, ExeName]);
  end;
end;

{ ExecuteCommand and friends ------------------------------------------------- }

procedure ExecuteCommandWait(const CurrentDirectory: String;
  ExeName: String;
  const Options: array of string;
  out ExitStatus: Integer;
  const Flags: TExecuteCommandFlags = [];
  const OverrideEnvironment: TStringList = nil);
var
  P: TCastleProcess;
  I: Integer;
begin
  // default out values
  ExitStatus := 0;

  ExeName := AbsoluteExeName(ExeName);

  P := TCastleProcess.Create(nil);
  try
    P.Executable := ExeName;
    if CurrentDirectory <> '' then
      P.CurrentDirectory := CurrentDirectory;
    if High(Options) >= 0 then
      for I := Low(Options) to High(Options) do
        P.Parameters.Add(Options[I]);
    WritelnLog('Calling ' + ExeName);
    WritelnLog(P.Parameters.Text);

    if ecNoConsole in Flags then
      P.Options := P.Options + [poNoConsole];
    if OverrideEnvironment <> nil then
      P.Environment := OverrideEnvironment;

    P.Execute;
    P.WaitOnExit;

    ExitStatus := P.ExitStatus;
  finally FreeAndNil(P) end;
end;

procedure ExecuteCommandCheckStatus(const CurrentDirectory: String;
  const ExeName: String;
  const Options: array of string;
  const Flags: TExecuteCommandFlags = [];
  const OverrideEnvironment: TStringList = nil);
var
  ExitStatus: Integer;
begin
  ExecuteCommandWait(CurrentDirectory, ExeName, Options, ExitStatus, Flags,
    OverrideEnvironment);
  if ExitStatus <> 0 then
    raise Exception.CreateFmt('Command "%s" exited with non-zero exit status %d', [
      ExeName,
      ExitStatus
    ]);
end;

{$ifdef FPC} // needs full TProcess

procedure ExecuteCommand(const CurrentDirectory: String;
  ExeName: String;const Options: array of string;
  out OutputString: String; out ExitStatus: integer;
  const LineFiltering: TLineFiltering = nil;
  const LineFilteringData: Pointer = nil;
  const Flags: TExecuteCommandFlags = [];
  const OverrideEnvironment: TStringList = nil;
  const PassThrough: Boolean = false);
var
  P: TCastleProcess;
  I: Integer;
  Capture: TCaptureOutput;
begin
  // default out values
  OutputString := '';
  ExitStatus := 0;

  ExeName := AbsoluteExeName(ExeName);

  P := TCastleProcess.Create(nil);
  try
    P.Executable := ExeName;
    if CurrentDirectory <> '' then
      P.CurrentDirectory := CurrentDirectory;
    if High(Options) >= 0 then
      for I := Low(Options) to High(Options) do
        P.Parameters.Add(Options[I]);
    WritelnLog('Calling ' + ExeName);
    WritelnLog(P.Parameters.Text);

    P.Options := [poUsePipes, poStderrToOutPut];
    if ecNoConsole in Flags then
      P.Options := P.Options + [poNoConsole];
    if OverrideEnvironment <> nil then
      P.Environment := OverrideEnvironment;
    P.Execute;

    Capture := TCaptureOutput.Construct(P.Output, LineFiltering, LineFilteringData);
    try
      Capture.PassThrough := PassThrough;

      // wait until P finishes, read OutputString
      while P.Running do
        Capture.ReadAvailable;
      Capture.ReadEverything;
      OutputString := Capture.Collected;
    finally FreeAndNil(Capture) end;

    ExitStatus := P.ExitStatus;
  finally FreeAndNil(P) end;
end;

{$endif FPC}

{$endif read_implementation}
