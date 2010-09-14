{
  Copyright 2002-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Some miscellaneous things, too small to be included in separate files. }

{$ifdef read_interface}

{ OS constants ---------------------------------------- }

const
  {$ifdef DELPHI}
    LineEnding = {$ifdef UNIX}  #10 {$endif}
                 {$ifdef MSWINDOWS} #13#10 {$endif} ;
  {$endif}

  { Kambi short name for new line }
  nl = LineEnding;
  { root dir name ('' if not applicable to this OS) }
  RootDir = {$ifdef UNIX} '/' {$endif}
            {$ifdef MSWINDOWS} '' {$endif} ;

{ basic types ---------------------------------------- }

type
  {$ifdef FPC}
  PByteArray = ^TByteArray;
  TByteArray = array[0..MaxInt div SizeOf(Byte)-1] of Byte;
  {$endif}

  TArray_PChar = array[0..MaxInt div SizeOf(PChar)-1]of PChar;
  PArray_PChar=^TArray_PChar;

  TArray_TObject = array[0..MaxInt div SizeOf(Pointer)-1]of TObject;
  PArray_TObject=^TArray_TObject;

  PString = ^AnsiString;

  { Don't call this PObject or PTObject to avoid possible name clashes
    with other not-mine units
    (pointers are often used in situations that prevent
    good type-checking, so better to avoid name clashes to avoid some
    nasty errors). }
  PtrObject = ^TObject;

  (*
    TCodeBreaker is a special class intended to be
    1. raised and catched (like a normal Exception descendant)
    2. but, unlike normal Exception descendant,
       it's not to be raised to signal some error.
    Instead it should be used to exit from some code blocks.

    When you define some descendant of TCodeBreaker class you must
    always say when it's catched, and catching it should always look like
    @longcode(#
      try
       { ... some code that can raise BreakXxx ... }
      except on BreakXxx do ; end;
    #)
    This way doing @code(BreakXxx.Create;) inside code
    @code({ ... some code ... }) will work like doing @code(Exit)
    but with obvious differences -- e.g. procedures called
    from @code({ ... some code ... }) can also raise @code(BreakXxx)
    to exit from the code.

    Example -- see @link(BreakGLWinEvent) in @link(GLWindow) unit.

    Naming conventions: @code(Break<CodeName>)
    (not @code(BreakFrom<CodeName>) or @code(EBreak<CodeName>) etc.)

    This class does not inherit from @code(Exception) because
    1. it does not signal some "exceptional" situation
    2. it must be always catched and "silenced" in some
       try ... except block, so having here property
       like @code(Message: string) would be a waste of time and memory. *)
  TCodeBreaker = class(TObject);

  { Internal error in the program. Something that definitely should not
    happen and indicates a bug that should be reported.

    This exception must be shown to user (never silently ignored),
    and usually the safest thing to do is to close the program at this point.
    (As the state is undefined now.)

    We will add to your error message (passed to the constructor) some
    instructive message, like @code('Internal error occured, please
    submit this to the author : '). }
  EInternalError = class(Exception)
    constructor Create(const s: string);
    constructor CreateFmt(const s: string; const Args: array of const);
  end;

  { See @link(Check) }
  ECheckFailed = class(Exception);

{ if not TrueValue then raise ECheckFailed.Create(ErrMessage). }
procedure Check(TrueValue: boolean; const ErrMessage: string);

{$ifdef DELPHI}
{$ifdef LINUX}
{ Fix needed for Kylix 1:
  when x < 0 this will raise EInvalidOp instead of exiting with SIGFPE.
  To be checked is it still needed with Kylix 3. }
function Sqrt(const x: Extended): Extended;
{$endif}
{$endif}

{ arrays searching ---------------------------------------- }

{podstawowe zastosowanie ArrayPos jest do zapisywania instrukcji
 case variable of
  val1 : inst1;
  val2 : inst2;
  else instElse;
 end;
  (gdzie variable: Typ) w taki sposob aby Typ nie musial byc typem ordered
  (np. moze byc stringiem) a val1, val2 nie musialby byc stalymi.
  Zapisz case jako
 case ArrayPos(variable,[val1, val2]) of
  0 : inst1;
  1 : inst2;
  else instElse;
 end;
 ArrayPos(A: T; Arr: Array of T) zwraca pozycje A w Arr (liczac od zera),
  zwraca -1 jesli A nie wystepuje w Arr. }

function ArrayPosPointer(A: pointer; const Arr: array of pointer): Integer; overload;
function ArrayPosStr(const A: string; const Arr: array of string): Integer; overload;
function ArrayPosInt(A: Integer; const Arr: array of Integer): Integer; overload;
function ArrayPosCard(A: Cardinal; const Arr: array of Cardinal): Integer; overload;
function ArrayPosExt(A: Extended; const Arr: array of Extended): Integer; overload;

function ArrayPosText(const A: string; const Arr: array of string;
  IgnoreCase: boolean {$ifdef DEFPARS}=true{$endif}): Integer; overload;

function PArrayPosStr(const A: string; Arr: PString; ArrCount: Integer): Integer; overload;
function PArrayPosText(const A: string; Arr: PString; ArrCount: Integer;
  IgnoreCase: boolean {$ifdef DEFPARS}=true{$endif}): Integer; overload;

{ Iff functions ------------------------------------------------------------- }

function Iff(boolval: boolean; trueval, falseval: string)  : string;   overload;
function Iff(boolval: boolean; trueval, falseval: Integer) : Integer;  overload;
function Iff(boolval: boolean; trueval, falseval: Float)   : Float;    overload;
function Iff(boolval: boolean; trueval, falseval: Cardinal): Cardinal; overload;
function Iff(boolval: boolean; trueval, falseval: char)    : char;     overload;

{ Some helpful consts ---------------------------------------- }

{$ifdef FPC}
{ This is a string describing FPC version, in the form
  version.release.patch .

  This is actually a constant (for every run of a program it has
  always the same value) but I can't declare it as a Pascal constant
  because it must use "Format" function that is not allowed in constant
  expressions. }
function SFPCVersion: string;
{$endif}

{ SCompilerDescription is a short name and version of Pascal compiler used
  to compile this unit. It is a constant, actually, but I cannot
  declare it as a constant because it must call SFPCVersion that
  is not declared as a constant.

  Calculating nice SCompilerDescription uses compiler directives
  defined at time of compilation, and not all compilers allow to define
  such things in a uniform manner (i.e. in a way that is guaranteed to
  work on all, present and future, compiler versions).
  So I can't promise here that this function will always return
  something precise, I do not even promise that it will compile
  with all FPC, GPC, Delphi versions. Only compiler versions
  that I, Kambi, use, will be properly defined here.

  That said, all FPC versions supporting macros FPC_VERSION/RELEASE/PATCH
  are guaranteed to be nicely reported by this function. }
function SCompilerDescription: string;

{ Print some common info for programs released on
  [http://vrmlengine.sourceforge.net/].
  This is useful only for programs released on this WWW page by Michalis.
  Resulting string is multiline, delimited by @link(nl).

  @param(DisplayProgramName Usually ProgramName, but you can give
    here something else if you want.)

  @param(Version For my programs this usually looks like '%d.%d.%d'
    and conforms to [http://vrmlengine.sourceforge.net/versioning.php])

  @param(WrapLines If true then resulting string will not have lines
    longer than 80 characters. Suitable for printing program
    help message on stdout, e.g. in response to @--help option.) }
function SVrmlEngineProgramHelpSuffix(const DisplayProgramName: string;
  const Version: string; WrapLines: boolean): string;

{$endif read_interface}

{$ifdef read_implementation}

constructor EInternalError.Create(const s: string);
begin
  inherited Create('Internal error: something that should not ever happen... happened. ' +
    'This indicates a bug in the program''s code (or something related, ' +
    'like a shared library, that should be workarounded from the program then). ' +
{   'Don''t panic - if you see this note then at least my code noticed the '+
    'bug in some way, so it may be something simple and I can probably fix it '+
    '(or it''s external bug and I am not responsible for this...). '+}
    'We definitely want you to report this as a bug! Please send this error-specific info: ' + S);
end;

constructor EInternalError.CreateFmt(const s: string; const Args: array of const);
begin
  Create(Format(S, Args));
end;

procedure Check(TrueValue: boolean; const errMessage: string);
begin
 if not TrueValue then raise ECheckFailed.Create(errMessage);
end;

{$ifdef DELPHI}
{$ifdef LINUX}
function Sqrt(const x: Extended): Extended;
begin
 if x >= 0 then
  Result := System.Sqrt(x) else
  raise EInvalidOp.Create('Sqrt argument <0');
end;
{$endif}
{$endif}

{ arrays searching ---------------------------------------- }

(* PIERWSZE WERSJE - nie mozna tego bylo tak zrobic,
   bo kompilator delphi/kylixa glupial gdy dostawal ta procedure
   wywolana z drugim parametrem skonstruowanym w locie (blad "ambiguous call").
   Przypuszczam ze kompilator probowal odgadnac typ elementow tablicy na
   podstawie deklaracji procedury, ale w deklaracji procedury to bylo
   nieustalone na skutek overloadu. W sumie jest to dosc logiczne, ale
   moznaby to zrobic tak zeby dzialalo - odgadywac najpierw po pierwszym
   parametrze ktora deklaracja procedury jest dobra a potem sprawdzac
   typ elementow tablicy.

deklaracje :
function ArrayPos(A: pointer; const Arr: array of pointer): Integer; overload;
function ArrayPos(const A: string; const Arr: array of string): Integer; overload;
function ArrayPos(A: Integer; const Arr: array of Integer): Integer; overload;
function ArrayPos(A: Cardinal; const Arr: array of Cardinal): Integer; overload;
function ArrayPos(A: Extended; const Arr: array of Extended): Integer; overload;}

implementacje :
function ArrayPos(A: pointer; const Arr: array of pointer): Integer; {$Include KambiUtils_implement_ArrayPos.inc}
function ArrayPos(const A: string; const Arr: array of string): Integer; {$Include KambiUtils_implement_ArrayPos.inc}
function ArrayPos(A: Integer; const Arr: array of Integer): Integer; {$Include KambiUtils_implement_ArrayPos.inc}
function ArrayPos(A: Cardinal; const Arr: array of Cardinal): Integer; {$Include KambiUtils_implement_ArrayPos.inc}
function ArrayPos(A: Extended; const Arr: array of Extended): Integer; {$Include KambiUtils_implement_ArrayPos.inc}
*)

function ArrayPosPointer(A: Pointer; const Arr: array of pointer): Integer; {$Include KambiUtils_implement_ArrayPos.inc}
function ArrayPosStr(const A: string; const Arr: array of string): Integer; {$Include KambiUtils_implement_ArrayPos.inc}
function ArrayPosInt(A: Integer; const Arr: array of Integer): Integer; {$Include KambiUtils_implement_ArrayPos.inc}
function ArrayPosCard(A: Cardinal; const Arr: array of Cardinal): Integer; {$Include KambiUtils_implement_ArrayPos.inc}
function ArrayPosExt(A: Extended; const Arr: array of Extended): Integer; {$Include KambiUtils_implement_ArrayPos.inc}

function ArrayPosText(const A: string; const Arr: array of string;
  IgnoreCase: boolean): Integer;
begin
 result := PArrayPosText(A, @Arr, High(Arr)+1, IgnoreCase);
end;

function PArrayPosStr(const A: string; Arr: PString; ArrCount: Integer): Integer;
begin
 for result := 0 to ArrCount-1 do
 begin
  if Arr^=A then exit;
  Inc(Arr);
 end;
 result := -1;
end;

function PArrayPosText(const A: string; Arr: PString; ArrCount: Integer;
  IgnoreCase: boolean): Integer;
var AA: string;
begin
 if IgnoreCase then
 begin
  AA := AnsiUpperCase(A);
  for result := 0 to ArrCount-1 do
   begin if AnsiUpperCase(Arr^) = AA then exit; Inc(Arr) end;
  result := -1;
 end else
 begin
  for result := 0 to ArrCount-1 do
   begin if Arr^=A then exit; Inc(Arr) end;
  result := -1;
 end;
end;

{ Iff functions ------------------------------------------------------------- }

function Iff(boolval: boolean; trueval, falseval: string)  : string;   begin if boolval then result := trueval else result := falseval end;
function Iff(boolval: boolean; trueval, falseval: Integer) : Integer;  begin if boolval then result := trueval else result := falseval end;
function Iff(boolval: boolean; trueval, falseval: Float)   : Float;    begin if boolval then result := trueval else result := falseval end;
function Iff(boolval: boolean; trueval, falseval: Cardinal): Cardinal; begin if boolval then result := trueval else result := falseval end;
function Iff(boolval: boolean; trueval, falseval: char)    : char;     begin if boolval then result := trueval else result := falseval end;

{ Helpful consts ------------------------------------------------------------ }

{$ifdef FPC}
function SFPCVersion: string;
begin
 Result := Format('%d.%d.%d', [FPC_VERSION, FPC_RELEASE, FPC_PATCH]);

(*
  Notes about old ideas how to implement it to be able to declare it
  as a constant:

    As some time, I used this code:

      {$ifdef VER1_0_6} '1.0.6' {$endif}
      {$ifdef VER1_0_10} '1.0.10' {$endif}
      {$ifdef VER1_9_4} '1.9.4' {$endif}
      {$ifdef VER1_9_5} '1.9.5' {$endif}
      {$ifdef VER1_9_6} '1.9.6' {$endif}
      {$ifdef VER1_9_7} '1.9.7' {$endif};

    but this obviously had to be fixed for each new FPC version.

    There's also this trick to define it as a constant,
    but it works only when there are only single digits in version
    (e.g. it wouldn't work for FPC 1.0.10):
      Chr(Ord(FPC_VERSION)+Ord('0')) +'.' +
      Chr(Ord(FPC_RELEASE)+Ord('0')) +'.' +
      Chr(Ord(FPC_PATCH)+Ord('0'));

  After some time, I decided to implement SFPCVersion cleanly
  as a function.  *)
end;
{$endif FPC}

function SCompilerDescription: string;
begin
 Result :=
   {$ifdef FPC}
     'Free Pascal Compiler ' + SFPCVersion
   {$else} {$ifdef DELPHI}
     {$ifdef WIN32} 'Delphi'
       {$ifdef VER140} + ' 6' {$endif}
       {$ifdef VER150} + ' 7' {$endif}
     {$else} 'Kylix'
     {$endif}
   {$else} {$ifdef __GPC__}
     'GNU Pascal'
   {$else}
     Undefined compiler.
   {$endif} {$endif} {$endif} ;
end;

function SVrmlEngineProgramHelpSuffix(const DisplayProgramName: string;
  const Version: string; WrapLines: boolean): string;
begin
 Result :=
   DisplayProgramName + ' version ' + Version + '.' +nl+
   'Author: Michalis Kamburelis, aka Kambi <michalis.kambi@gmail.com>' +nl+
   'See http://vrmlengine.sourceforge.net/ for latest versions' +
   Iff(WrapLines, nl + ' ', '') +
   ' of this program, sources, documentation etc.' +nl+
   'Compiled with ' + SCompilerDescription +'.';
end;

{$endif read_implementation}
