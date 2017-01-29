{
  Copyright 2002-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Some miscellaneous things, too small to be included in separate files. }

{$ifdef read_interface}

{ OS constants ---------------------------------------- }

const
  { New line. Short name for LineEnding. }
  NL = LineEnding;

{ basic types ---------------------------------------- }

type
  {$ifdef FPC}
  { }
  PByteArray = ^TByteArray;
  TByteArray = array[0..MaxInt div SizeOf(Byte)-1] of Byte;
  {$endif}

  TArray_PChar = array[0..MaxInt div SizeOf(PChar)-1]of PChar;
  PArray_PChar=^TArray_PChar;

  TArray_TObject = array[0..MaxInt div SizeOf(Pointer)-1]of TObject;
  PArray_TObject=^TArray_TObject;

  PString = ^AnsiString;

  { Pointer to TObject.
    Don't call this PObject or PTObject to avoid possible name clashes
    with other units (pointers are often used in situations that prevent
    good type-checking, so better to avoid name clashes to avoid some
    nasty errors). }
  PtrObject = ^TObject;

  { Internal error in the program. Something that definitely should not
    happen and indicates a bug that should be reported.

    This exception must be shown to user (never silently ignored),
    and usually the safest thing to do is to close the program at this point.

    We will add to your error message (passed to the constructor) some
    instructive message, like @code('Internal error occurred, please
    submit this to the author : '). }
  EInternalError = class(Exception)
    constructor Create(const s: string);
    constructor CreateFmt(const s: string; const Args: array of const);
  end;

  ECheckFailed = class(Exception);

{ Check condition.
  @raises ECheckFailed Raised with ErrMessage if condition TrueValue if false. }
procedure Check(TrueValue: boolean; const ErrMessage: string = 'Check failed');

{ Arrays searching ----------------------------------------------------------- }

{ Search the array for a given value.
  Returns index (zero-based) or -1 if not found.

  Useful for writing case as:

  @longCode(#
    case ArrayPosStr(variable, [val1, val2]) of
      0 : Something1;
      1 : Something2;
      else SomethingElse;
    end;
  #) }
function ArrayPosStr(const A: string; const Arr: array of string): Integer; overload;

function ArrayPosText(const A: string; const Arr: array of string;
  IgnoreCase: boolean = true): Integer; overload;

function PArrayPosStr(const A: string; Arr: PString; ArrCount: Integer): Integer; overload;
function PArrayPosText(const A: string; Arr: PString; ArrCount: Integer;
  IgnoreCase: boolean = true): Integer; overload;

{ Iff functions ------------------------------------------------------------- }

{ }
function Iff(boolval: boolean; trueval, falseval: string)  : string;   overload;
function Iff(boolval: boolean; trueval, falseval: Integer) : Integer;  overload;
function Iff(boolval: boolean; trueval, falseval: Float)   : Float;    overload;
function Iff(boolval: boolean; trueval, falseval: Cardinal): Cardinal; overload;
function Iff(boolval: boolean; trueval, falseval: char)    : char;     overload;

{ Some helpful version stuff ------------------------------------------------ }

const
  CastleEngineVersion = {$I castleversion.inc};

{$ifdef FPC}
{ Describe FPC version. In the form 'version.release.patch'.

  This is actually a constant (for every run of a program it has
  always the same value) but I can't declare it as a Pascal constant
  because it must use "Format" function that is not allowed in constant
  expressions. }
function SFPCVersion: string;
{$endif}

{ Short name and version of Pascal compiler used
  to compile this unit. It is a constant, actually, but I cannot
  declare it as a constant because it must call SFPCVersion that
  is not declared as a constant. }
function SCompilerDescription: string;

{ Print some common info for programs released on
  [http://castle-engine.sourceforge.net/].
  This is useful only for programs released on this WWW page by Michalis.
  Resulting string is multiline.

  @param(DisplayApplicationName Usually ApplicationName, but you can give
    here something else if you want.)

  @param(Version For my programs this usually looks like '%d.%d.%d'
    and conforms to [http://castle-engine.sourceforge.net/versioning.php])

  @param(WrapLines If true then resulting string will not have lines
    longer than 80 characters. Suitable for printing program
    help message on stdout, e.g. in response to @--help option.) }
function SCastleEngineProgramHelpSuffix(const DisplayApplicationName: string;
  const Version: string; WrapLines: boolean): string;

{ If not TrueValue then RaiseLastOSError. }
procedure OSCheck(TrueValue: boolean); overload;
procedure OSCheck(TrueValue: boolean; const Place: string); overload;
{$endif read_interface}

{$ifdef read_implementation}

constructor EInternalError.Create(const s: string);
begin
  inherited Create('Internal error (submit a bugreport!): ' + S);
//   inherited Create('Internal error: something that should not ever happen... happened. ' +
//     'This indicates a bug in the program''s code (or something related, ' +
//     'like a shared library, that should be workarounded from the program then). ' +
// {   'Don''t panic - if you see this note then at least my code noticed the '+
//     'bug in some way, so it may be something simple and I can probably fix it '+
//     '(or it''s external bug and I am not responsible for this...). '+}
//     'We definitely want you to report this as a bug! Please send this error-specific info: ' + S);
end;

constructor EInternalError.CreateFmt(const s: string; const Args: array of const);
begin
  Create(Format(S, Args));
end;

procedure Check(TrueValue: boolean; const errMessage: string);
begin
 if not TrueValue then raise ECheckFailed.Create(errMessage);
end;

{ arrays searching ---------------------------------------- }

function ArrayPosStr(const A: string; const Arr: array of string): Integer;
begin
  for Result := 0 to High(Arr) do
    if Arr[result] = A then Exit;
  Result := -1;
end;

function ArrayPosText(const A: string; const Arr: array of string;
  IgnoreCase: boolean): Integer;
begin
  Result := PArrayPosText(A, @Arr, High(Arr)+1, IgnoreCase);
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
  Result := {$I %FPCVERSION%} +
    ' (' + {$I %FPCTARGETOS%} + ' / ' + {$I %FPCTARGETCPU%} + ')';
end;
{$endif FPC}

function SCompilerDescription: string;
begin
  Result :=
    {$ifdef FPC}
      'FPC ' + SFPCVersion
    {$else} {$ifdef DELPHI}
      {$ifdef WIN32} 'Delphi'
        {$ifdef VER140} + ' 6' {$endif}
        {$ifdef VER150} + ' 7' {$endif}
      {$else} 'Kylix'
      {$endif}
    {$else} {$ifdef __GPC__}
      'GNU Pascal'
    {$else}
      'Unrecognized compiler'
    {$endif} {$endif} {$endif} ;
end;

function SCastleEngineProgramHelpSuffix(const DisplayApplicationName: string;
  const Version: string; WrapLines: boolean): string;
begin
  Result :=
    DisplayApplicationName + ' version ' + Version + '.' +nl+
    'Author: Michalis Kamburelis, aka Kambi <michalis.kambi@gmail.com>' +nl+
    'See http://castle-engine.sourceforge.net/ for latest versions' +
    Iff(WrapLines, nl + ' ', '') +
    ' of this program, sources, documentation etc.' +nl+
    'Compiled with ' + SCompilerDescription +'.';
end;

procedure OSCheck(TrueValue: boolean);
begin
  if not TrueValue then RaiseLastOSError;
end;

procedure OSCheck(TrueValue: boolean; const Place: string);
begin
  if not TrueValue then
  try
    RaiseLastOSError;
  except on E: EOSError do
    begin
      E.Message := Place + ': ' + E.Message;
      raise;
    end;
  end;
end;
{$endif read_implementation}
