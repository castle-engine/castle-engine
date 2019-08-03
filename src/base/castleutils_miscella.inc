{
  Copyright 2002-2018 Michalis Kamburelis.

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

{$ifdef FPC}
{ FPC version. In the form 'version.release.patch'.

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
  [https://castle-engine.io/].
  This is useful only for programs released on this WWW page by Michalis.
  Resulting string is multiline.

  @param(DisplayApplicationName Short application name.
    Usually you should take it from ApplicationName.)

  @param(Version Version to display.
    Usually you should take it from
    @link(TCastleApplicationProperties.Version ApplicationProperties.Version).)

  @param(WrapLines If true then resulting string will not have lines
    longer than 80 characters. Suitable for printing program
    help message on stdout, e.g. in response to @--help option.) }
function SCastleEngineProgramHelpSuffix(const DisplayApplicationName: string;
  const Version: string; WrapLines: boolean): string;

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
  for the floating point numbers, regardless of the user's locale settings. }
function FormatDot(const Fmt: String; const Args: array of const): String;

{ Like standard TryStrToFloat, but always uses dot (.) as a decimal separator
  for the floating point numbers, regardless of the user's locale settings. }
function TryStrToFloatDot(const S: String; out Value: Single): Boolean; overload;
function TryStrToFloatDot(const S: String; out Value: Double): Boolean; overload;
{$ifndef EXTENDED_EQUALS_DOUBLE}
function TryStrToFloatDot(const S: String; out Value: Extended): Boolean; overload;
{$endif}

{ Like standard StrToFloat, but always uses dot (.) as a decimal separator
  for the floating point numbers, regardless of the user's locale settings. }
function StrToFloatDot(const S: String): Extended;

{ Like standard StrToFloatDef, but always uses dot (.) as a decimal separator
  for the floating point numbers, regardless of the user's locale settings. }
function StrToFloatDefDot(Const S: String; const Default: Extended): Extended;

var
  { Are we inside Castle Game Engine designer mode. }
  CastleDesignMode: Boolean;

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

function ArrayPosStr(const A: string; const Arr: array of string): Integer;
begin
  for Result := 0 to High(Arr) do
    if Arr[Result] = A then
      Exit;
  Result := -1;
end;

function ArrayPosText(const A: string; const Arr: array of string;
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

{ Helpful consts ------------------------------------------------------------ }

{$ifdef FPC}
function SFPCVersion: string;
begin
  Result := {$I %FPCVERSION%} +
    ' (' + {$I %FPCTARGETOS%} + ' / ' + {$I %FPCTARGETCPU%} +
      {$ifdef CASTLE_NINTENDO_SWITCH} ' for Nintendo Switch' + {$endif}
    ')';
end;
{$endif FPC}

function SCompilerDescription: string;
begin
  Result :=
    {$ifdef FPC}
      'FPC ' + SFPCVersion
    {$else}

    {$ifdef DCC}
    'Delphi'
    {$else}

    {$ifdef __GPC__}
    'GNU Pascal'
    {$else}

    'Unrecognized compiler'

    {$endif} {$endif} {$endif};
end;

function SCastleEngineProgramHelpSuffix(const DisplayApplicationName: string;
  const Version: string; WrapLines: boolean): string;
begin
  if Version <> '' then
    Result := DisplayApplicationName + ' version ' + Version + '.' + NL
  else
    Result := '';
  Result := Result +
    'Created using Castle Game Engine ( https://castle-engine.io/ ) version ' + CastleEngineVersion + '.' + NL +
    'Compiled with ' + SCompilerDescription + '.';
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
  Result := Format(Fmt, Args, FormatSettings);
end;

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

{$ifndef EXTENDED_EQUALS_DOUBLE}
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

{$endif read_implementation}
