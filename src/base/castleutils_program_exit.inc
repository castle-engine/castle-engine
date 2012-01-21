{
  Copyright 2002-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Things related to exiting the program.
  Some important things are set up here,

  If your program ends with an exception (that is, the main @code(begin..end.)
  of program code exits because exception) we want to output some nice
  error message for user. This is done here by registering our own
  exception handler for SysUtils.ExceptProc.

  @unorderedList(
    @item(In non-debug mode, in case of exception,
      we will output nice message by OutputException and will Halt with
      HaltCodeOnException code.)

    @item(Unless the DEBUG symbold is defined, then we (after showing the nice
      error by OutputException) pass the control to default SysUtils.ExceptProc.
      This way we allow FPC to print nice backtrace of the exception,
      which is useful.)

    @item(When you cause program exit by special BreakProgram exception
      class, then have special behavior. In this case we don't output
      any exception description (like OutputException),
      and we will exit with code you gave to BreakProgram.HaltCode
      (ignoring HaltCodeOnException).

      This BreakProgram behavior is done regardless of DEBUG or not.

      This way you can use @code(raise BreakProgram.Create(123))
      instead or @code(Halt(123)) to exit the program with given status.
      BreakProgram will exit more gracefully than Halt, as all
      the "finally" clauses will execute.)
  ) }

{$ifdef read_interface}

var
  { Additional message output when you end program with an exception. }
  BonusErrorMessg: string ='';

type
  { Class of exceptions that will not have ClassName displayed by
    various routines. ExceptMessage, OutputException will not show exception
    class. Sometimes useful, when you know that exception message
    is good enough, and showing exception class is not needed. }
  EWithHiddenClassName = class(Exception);

{ Nice exception description. Contains ProgramName, exception ClassName
  (if not descends from EWithHiddenClassName), exception Message (if
  descends from Exception), and ExceptAddr (and not-nil, and this is compiled
  with -dDEBUG), and BonusErrorMesssg. }
function ExceptMessage(E: TObject; ExceptAddr: Pointer = nil): string; overload;

{ Show nice exception description on console or (for GUI Windows programs)
  by a message box. Equivalent to ErrorWrite(ExceptMessage(E)). }
procedure OutputException(E: TObject; ExceptAddr: Pointer = nil); overload;

{ If Value then Halt(0), else Halt(1).

  It is the standard convention of command-line programs to
  exit with code 0 on success and <> 0 on failure.
  Or (for some programs like `test') exit with code 0 to indicate true result
  and <> 0 to indicate false result.
  So you will probably want to pass here some boolean variable
  indicating "Success" or "TestPassed". }
procedure HaltBool(Value: boolean);

var
  HaltCodeOnException: Integer = 1;

{ Call Proc, catch all exceptions inside the Proc,
  and in case of exception make OutputException and Halt(HaltCode).
  The result is that HaltOnException doesn't
  raise any exception, never. It always deals with exceptions inside
  Proc itself.

  Version without HaltCode parameter uses global HaltCodeOnException value.

  For the special exception class @link(BreakProgram), it does simply
  Halt(BreakProgram(E).ExitCode)) (no OutputException in this case).

  When symbol DEBUG is defined, then HaltOnException works differently
  --- it just calls Proc (and doesn't catch any exceptions).

  This is particularly useful under Delphi/Win32. There main program
  should @italic(never) exit with exception. Because such exception
  (because of Delphi stupidity ?) shows ugly Windows
  dialog box saying something like "Program exited unexpectedly,
  contact with author etc. bullshit". There is no way for me to avoid
  this dialog box, even by my own ExceptProc. }
procedure HaltOnException(proc: TProcedure; HaltCode: integer); overload;
procedure HaltOnException(proc: TProcedure); overload;

type
  { Exception specially handled by my exception handler.
    It will not cause any message to be output, and will cause Halt
    with given code. }
  BreakProgram = class(TCodeBreaker)
  private
    FHaltCode: Integer;
  public
    property HaltCode: Integer read FHaltCode;
    constructor Create(AHaltCode: Integer = 0);
  end;

{ Raise BreakProgram with AHaltCode, causing the program to stop with
  given exit code nicely (finalizes all exceptions try..finally and such). }
procedure ProgramBreak(AHaltCode: Integer = 0); overload;

{$endif read_interface}

{$ifdef read_implementation}

function ExceptMessage(E: TObject; ExceptAddr: Pointer): string;
var Message, NiceMessage: string;
begin
 if E is Exception then
 begin
  Message := Exception(E).Message;

  if E is EInOutError then
  begin
   { jezeli wyjatek to EInOutError to zamien [...] nazwy bledow
     "IO Error xxx" na troche lepsze. }
   case EInOutError(E).ErrorCode of
    1..99: NiceMessage := SysErrorMessage(EInOutError(E).ErrorCode);
    100: NiceMessage := 'End of File';
    101: NiceMessage := 'Disk Full';
    102: NiceMessage := 'File variable not assigned';
    103: NiceMessage := 'File not open';
    104: NiceMessage := 'File not open for input';
    105: NiceMessage := 'File not open for output';
    106: NiceMessage := 'Error in formatted input';
    107: NiceMessage := 'File already open';
    else NiceMessage := '';
   end;
   if NiceMessage<>'' then Message := Message +' (' +NiceMessage +')';
  end;

 end else
  Message := '';

 if (E is EWithHiddenClassName) and (Message<>'') then
  result := ProgramName +': ' +Message else
 begin
  result := ProgramName +': Exception ' +E.ClassName;
  {$ifdef DEBUG}
  if ExceptAddr<>nil then
   result := result +' (at address ' + PointerToStr(ExceptAddr) + ')';
  {$endif}
  if Message<>'' then result := result +' : ' +nl +Message;
 end;

 if BonusErrorMessg<>'' then result := result +nl +BonusErrorMessg;
end;

procedure OutputException(E: TObject; ExceptAddr: Pointer);
begin
 ErrorWrite(ExceptMessage(E, ExceptAddr));
 { zeby wyswietlic tu message tak jak Delphi/Kylix : ShowException(E, nil) }
end;

procedure HaltBool(Value: boolean);
begin
 if Value then Halt(0) else Halt(1);
end;

procedure HaltOnException(proc: TProcedure; HaltCode: integer);
begin
 {$ifdef DEBUG} proc;
 {$else}
  try Proc;
  except
   on E: TObject do
   begin
    if E is BreakProgram then Halt(BreakProgram(E).HaltCode);
    OutputException(E, nil);
    Halt(HaltCode);
   end;
  end;
 {$endif}
end;

procedure HaltOnException(proc: TProcedure);
begin HaltOnException(proc, HaltCodeOnException); end;

var OldExceptProc: TExceptProc;

procedure CastleUtils_ExceptProc(ExceptObject: TObject;
  ExceptAddr: Pointer; FrameCount: Longint; Frame: PPointer);
begin
 if ExceptObject is BreakProgram then
  Halt(BreakProgram(ExceptObject).HaltCode);

 OutputException(ExceptObject, ExceptAddr);
 {$ifdef DEBUG}
 OldExceptProc(ExceptObject, ExceptAddr, FrameCount, Frame);
 {$else}
 Halt(HaltCodeOnException);
 {$endif}
end;

constructor BreakProgram.Create(AHaltCode: Integer);
begin
 inherited Create;
 FHaltCode := AHaltCode;
end;

procedure ProgramBreak(AHaltCode: Integer);
begin
 raise BreakProgram.Create(AHaltCode);
end;

procedure InitializationProgramExit;
begin
 OldExceptProc := ExceptProc;
 ExceptProc := {$ifdef FPC_OBJFPC} @ {$endif} CastleUtils_ExceptProc;
end;

procedure FinalizationProgramExit;
begin
 ExceptProc := OldExceptProc;
end;

{$endif read_implementation}
