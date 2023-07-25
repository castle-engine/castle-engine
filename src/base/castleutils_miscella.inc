{%MainUnit castleutils.pas}
{
  Copyright 2002-2022 Michalis Kamburelis.

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
  { New line sequence, suitable for current OS. Short name for LineEnding. }
  NL = LineEnding;

{ basic exceptions ---------------------------------------- }

type
  { Internal error (bug) in the program. Something that definitely should not
    happen and indicates a bug that should be reported.

    This exception must be shown to user (never silently ignored),
    and usually the safest thing to do is to close the program at this point.

    We will add to your error message (passed to the constructor) some
    instructive message, like @code('Please submit this to the developers: '). }
  EInternalError = class(Exception)
    constructor Create(const S: string); overload;
    constructor Create(const ErrorNumber: Int64); overload;
    constructor CreateFmt(const s: string; const Args: array of const);
  end;

  ECheckFailed = class(Exception);

  { Simple notify event with no Sender }
  TSimpleNotifyEvent = procedure of object;

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
function ArrayPosStr(const A: String; const Arr: array of String): Integer; overload;
function ArrayPosText(const A: String; const Arr: array of String;
  IgnoreCase: boolean = true): Integer; overload;

function ArrayContainsString(const A: String; const Arr: array of String): Boolean;

{ Iff functions ------------------------------------------------------------- }

{ }
function Iff(boolval: boolean; trueval, falseval: string)  : string;   overload;
function Iff(boolval: boolean; trueval, falseval: Integer) : Integer;  overload;
function Iff(boolval: boolean; trueval, falseval: Float)   : Float;    overload;
function Iff(boolval: boolean; trueval, falseval: Cardinal): Cardinal; overload;
function Iff(boolval: boolean; trueval, falseval: char)    : char;     overload;

{ Some helpful version stuff ------------------------------------------------ }

const
  { Castle Game Engine version. }
  CastleEngineVersion = {$I castleversion.inc};

{ Name and version of the Pascal compiler used to compile this unit.

  @seealso TCastleApplicationProperties.Description
  @seealso SPlatformDescription }
function SCompilerDescription: string;

{ Describe current platform, OS, CPU.
  This is all detected looking at compilation symbols defined when compiling this unit.

  @seealso TCastleApplicationProperties.Description
  @seealso SCompilerDescription }
function SPlatformDescription: String;

function SCastleEngineProgramHelpSuffix(const AppName, AppVersion: String; const Ignored: Boolean): String;
  deprecated 'use ApplicationProperties.Description';

{ If not TrueValue then RaiseLastOSError. }
procedure OSCheck(TrueValue: boolean); overload;
procedure OSCheck(TrueValue: boolean; const Place: string); overload;

type
  { Descend from this to hide a parameterless constructor.

    In Delphi, if you have multiple overloaded constructors (marked
    with "overload"), the user can call any constructor of the ancestor
    class that you have not "obscured" by a new definition.
    In particular, if all your constructors have some parameters,
    then user can "bypass" them by calling a parameterless constructor
    defined by TObject.
    Descend from this class to disallow this.
    See
    https://stackoverflow.com/questions/14003153/how-to-hide-the-inherited-tobject-constructor-while-the-class-has-overloaded-one
    http://andy.jgknet.de/blog/2011/07/hiding-the-tobject-create-constructor/

    Note that in FPC (in ObjFpc mode) this is not a problem,
    as the standard overloading (without the "overload" keyword) is more sane:
    the overloaded constructors obscure the ancestor constructors. }
  {$warnings off}
  TNoParameterlessContructor = class
  strict private
    constructor Create;
  end;
  {$warnings on}

{ Like standard Format, but always uses dot (.) as a decimal separator
  for the floating point numbers, regardless of the user's locale settings.

  In general this ignores locale, so also ThousandSeparator doesn't matter
  (conversion is done like it is #0). }
function FormatDot(const Fmt: String; const Args: array of const): String;

{ Like standard FloatToStr, but always uses dot (.) as a decimal separator
  for the floating point numbers, regardless of the user's locale settings.

  In general this ignores locale, so also ThousandSeparator doesn't matter
  (conversion is done like it is #0). }
function FloatToStrDot(const Value: Extended): String;

{ Like standard FloatToStr, but always uses dot (.) as a decimal separator
  for the floating point numbers, regardless of the user's locale settings.

  In general this ignores locale, so also ThousandSeparator doesn't matter
  (conversion is done like it is #0). }
function FloatToStrFDot(const Value: Extended;
  const FloatFormat: TFloatFormat;
  const Precision: Integer;
  const Digits: Integer): String;

{ Display float as nice concise number to read (but not necessarily with
  absolute precision):
  always decimal,
  no digits after dot if not necessary,
  show some digits (but not too many - right now 2) after dot if they are meaningful.

  Like other ...Dot routines, always uses dot (.) as a decimal separator,
  regardless of the user's locale settings. }
function FloatToStrDisplay(const AValue: Extended): String;

{ Like standard TryStrToFloat, but always uses dot (.) as a decimal separator
  for the floating point numbers, regardless of the user's locale settings.

  Also it doesn't Trim the argument (so whitespace around number is @italic(not) tolerated),
  and in general ignores locale (standard StrToFloat looks at ThousandSeparator). }
function TryStrToFloatDot(const S: String; out Value: Single): Boolean; overload;
function TryStrToFloatDot(const S: String; out Value: Double): Boolean; overload;
{$ifdef EXTENDED_OVERLOADS}
function TryStrToFloatDot(const S: String; out Value: Extended): Boolean; overload;
{$endif}

{ Like standard StrToFloat, but always uses dot (.) as a decimal separator
  for the floating point numbers, regardless of the user's locale settings.

  Also it doesn't Trim the argument (so whitespace around number is @italic(not) tolerated),
  and in general ignores locale (standard StrToFloat looks at ThousandSeparator). }
function StrToFloatDot(const S: String): Extended;

{ Like standard StrToFloatDef, but always uses dot (.) as a decimal separator
  for the floating point numbers, regardless of the user's locale settings.

  Also it doesn't Trim the argument (so whitespace around number is @italic(not) tolerated),
  and in general ignores locale (standard StrToFloat looks at ThousandSeparator). }
function StrToFloatDefDot(Const S: String; const Default: Extended): Extended;

type
  { Application mode. }
  TCastleApplicationMode = (
    { Application is running.
      We are not inside any editor (CGE editor, LCL/Delphi designer). }
    appRunning,

    { We are designing the application in CGE editor (or LCL/Delphi designer).
      Simulation is not started. }
    appDesign,

    { We are designing the application and simulation has been started by user.
      The main feature of simulation right now is to test the physics at design-time.
      More features may be unlocked in particular components. }
    appSimulation,

    { We are designing the application and simulation has been started and paused by the user. }
    appSimulationPaused
  );

{ Current application mode, see TCastleApplicationMode for various possible values. }
function CastleApplicationMode: TCastleApplicationMode;

{ Are we in design-time now. Shortcut for CastleApplicationMode <> appRunning. }
function CastleDesignMode: Boolean;

var
  { @exclude Write-able version of CastleApplicationMode. }
  InternalCastleApplicationMode: TCastleApplicationMode;

  { @exclude Path to editor data, in design mode.
    Useful e.g. to initialize gizmo textures.

    Note: always be prepared it may not be initialized (empty),
    e.g. it is not initialized when using TCastleControl in Delphi IDE. }
  InternalCastleDesignData: String;

  { @exclude Refresh UI of CGE editor, because hierarchy of UI/transforms changed. }
  InternalCastleDesignInvalidate: Boolean;

{$endif read_interface}

{$ifdef read_implementation}

constructor EInternalError.Create(const S: string);
begin
  inherited Create('Internal error occured. Please submit a bugreport providing this information to the developers: ' + S);
//   inherited Create('Internal error: something that should not ever happen... happened. ' +
//     'This indicates a bug in the program''s code (or something related, ' +
//     'like a shared library, that should be workarounded from the program then). ' +
// {   'Don''t panic - if you see this note then at least my code noticed the '+
//     'bug in some way, so it may be something simple and I can probably fix it '+
//     '(or it''s external bug and I am not responsible for this...). '+}
//     'We definitely want you to report this as a bug! Please send this error-specific info: ' + S);
end;

constructor EInternalError.Create(const ErrorNumber: Int64);
begin
  Create(IntToStr(ErrorNumber));
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

function ArrayPosStr(const A: String; const Arr: array of String): Integer;
begin
  for Result := 0 to High(Arr) do
    if Arr[Result] = A then
      Exit;
  Result := -1;
end;

function ArrayContainsString(const A: String; const Arr: array of String): Boolean;
begin
  Result := ArrayPosStr(A, Arr) <> -1;
end;

function ArrayPosText(const A: String; const Arr: array of String;
  IgnoreCase: boolean): Integer;
var
  AA: string;
begin
  if IgnoreCase then
  begin
    AA := AnsiUpperCase(A);
    for Result := 0 to High(Arr) do
      if AnsiUpperCase(Arr[Result]) = AA then
        Exit;
    Result := -1;
  end else
    Result := ArrayPosStr(A, Arr);
end;

{ Iff functions ------------------------------------------------------------- }

function Iff(boolval: boolean; trueval, falseval: string)  : string;   begin if boolval then result := trueval else result := falseval end;
function Iff(boolval: boolean; trueval, falseval: Integer) : Integer;  begin if boolval then result := trueval else result := falseval end;
function Iff(boolval: boolean; trueval, falseval: Float)   : Float;    begin if boolval then result := trueval else result := falseval end;
function Iff(boolval: boolean; trueval, falseval: Cardinal): Cardinal; begin if boolval then result := trueval else result := falseval end;
function Iff(boolval: boolean; trueval, falseval: char)    : char;     begin if boolval then result := trueval else result := falseval end;

{ Various routines ------------------------------------------------------------ }

function SCompilerDescription: string;
begin
  Result :=
    {$ifdef FPC}
      'FPC ' + {$I %FPCVERSION%}
    {$else}

    {$ifdef DCC}
    { Make it clear we show compiler version, not Delphi version, as they are different things:
      https://docwiki.embarcadero.com/RADStudio/Sydney/en/Compiler_Versions .
      E.g. Delphi 11 = product version 28 = compiler version 35.
    }
    'Delphi (Compiler Version ' + FloatToStr(CompilerVersion) + ')'
    {$else}

    {$ifdef __GPC__}
    'GNU Pascal'
    {$else}

    'Unrecognized compiler'

    {$endif} {$endif} {$endif};
end;

function SPlatformDescription: String;
begin
  Result := PlatformToStr(Platform) +
    {$ifdef FPC}
    ', OS: ' + {$I %FPCTARGETOS%} +
    ', CPU: ' + {$I %FPCTARGETCPU%} +
    {$else}
    ', OS Version: ' + TOSVersion.ToString +
    {$endif}
    { Clearly stating CPU bits is more obvious in some cases.
      E.g. this avoids the question "what does Arm mean, is it 32-bit or 64-bit Arm?". }
    ' (this exe is using ' + IntToStr(SizeOf(Pointer) * 8) + '-bit architecture)';
end;

function SCastleEngineProgramHelpSuffix(const AppName, AppVersion: String; const Ignored: Boolean): String;
begin
  Result :=
    AppName + ' version ' + AppVersion + '.' + NL +
    'Using Castle Game Engine ( https://castle-engine.io/ ) version ' + CastleEngineVersion + '.' + NL +
    'Compiled with ' + SCompilerDescription + '.' + NL +
    'Platform: ' + SPlatformDescription + '.';
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

constructor TNoParameterlessContructor.Create;
begin
  raise Exception.Create('Parameterless constructor of ' + ClassName + ' should never be called');
end;

{$ifndef FPC}
function DefaultFormatSettings: TFormatSettings;
begin
  Result := TFormatSettings.Create;
end;
{$endif}

function FormatDot(const Fmt: String; const Args: array of const): String;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings := DefaultFormatSettings;
  FormatSettings.DecimalSeparator := '.';
  FormatSettings.ThousandSeparator := #0;
  Result := Format(Fmt, Args, FormatSettings);
end;

function FloatToStrDot(const Value: Extended): String;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings := DefaultFormatSettings;
  FormatSettings.DecimalSeparator := '.';
  FormatSettings.ThousandSeparator := #0;
  Result := FloatToStr(Value, FormatSettings);
end;

function FloatToStrFDot(const Value: Extended;
  const FloatFormat: TFloatFormat;
  const Precision: Integer;
  const Digits: Integer): String;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings := DefaultFormatSettings;
  FormatSettings.DecimalSeparator := '.';
  FormatSettings.ThousandSeparator := #0;
  Result := FloatToStrF(Value, FloatFormat, Precision, Digits, FormatSettings);
end;

function FloatToStrDisplay(const AValue: Extended): String;
begin
  Result := FloatToStrFDot(AValue, ffFixed,
    { With FPC, Precision = 0 is also OK, but with Delphi it's not,
      it will use scientific notation then early, e.g. for 123.0.
      See TTestCastleUtils.TestFloatToStrDisplay testcase. }
    20,
    2);

  { We just remove trailing digits after decimal separator, we don't need them.
    A less hacky way (that doens't involve string fixing) would be nicer,
    but it doesn't seem possible based on FloatToStrF possibilities,
    at least with FPC. }
  if Pos('.', Result) <> 0 then
  begin
    while Result[Length(Result)] = '0' do
      SetLength(Result, Length(Result) - 1);
    // previous removals of zero possibly removed all zeroes after ., so remove .
    if Result[Length(Result)] = '.' then
      SetLength(Result, Length(Result) - 1);
  end;
end;

(*
function TryStrToFloatDot(const S: String; out Value: Single): Boolean;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings := DefaultFormatSettings;
  FormatSettings.DecimalSeparator := '.';
  Result := TryStrToFloat(S, Value, FormatSettings);
end;

function TryStrToFloatDot(const S: String; out Value: Double): Boolean;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings := DefaultFormatSettings;
  FormatSettings.DecimalSeparator := '.';
  Result := TryStrToFloat(S, Value, FormatSettings);
end;

{$ifdef EXTENDED_OVERLOADS}
function TryStrToFloatDot(const S: String; out Value: Extended): Boolean;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings := DefaultFormatSettings;
  FormatSettings.DecimalSeparator := '.';
  Result := TryStrToFloat(S, Value, FormatSettings);
end;
{$endif}

function StrToFloatDot(const S: String): Extended;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings := DefaultFormatSettings;
  FormatSettings.DecimalSeparator := '.';
  Result := StrToFloat(S, FormatSettings);
end;

function StrToFloatDefDot(const S: String; const Default: Extended): Extended;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings := DefaultFormatSettings;
  FormatSettings.DecimalSeparator := '.';
  Result := StrToFloatDef(S, Default, FormatSettings);
end;
*)

{ We use Val, instead of calling StrToFloat with modified TFormatSettings.
  This makes our "Dot" routines slightly faster.
  This matters e.g. when parsing 3D model formats that encode floats in text. }

function TryStrToFloatDot(const S: String; out Value: Single): Boolean;
var
  Err: Integer;
begin
  Val(S, Value, Err);
  Result := Err = 0;
end;

function TryStrToFloatDot(const S: String; out Value: Double): Boolean;
var
  Err: Integer;
begin
  Val(S, Value, Err);
  Result := Err = 0;
end;

{$ifdef EXTENDED_OVERLOADS}
function TryStrToFloatDot(const S: String; out Value: Extended): Boolean;
var
  Err: Integer;
begin
  Val(S, Value, Err);
  Result := Err = 0;
end;
{$endif}

function StrToFloatDot(const S: String): Extended;
var
  Err: Integer;
begin
  Val(S, Result, Err);
  if Err <> 0 then
    raise EConvertError.CreateFmt('"%s" is an invalid float', [S]);
end;

function StrToFloatDefDot(const S: String; const Default: Extended): Extended;
var
  Err: Integer;
begin
  Val(S, Result, Err);
  if Err <> 0 then
    Result := Default;
end;

function CastleApplicationMode: TCastleApplicationMode;
begin
  Result := InternalCastleApplicationMode;
end;

function CastleDesignMode: Boolean;
begin
  Result := InternalCastleApplicationMode <> appRunning;
end;

{$endif read_implementation}
