{
  Copyright 2002-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Processing command-line parameters and options (TParameters,
  single instance Parameters). }
unit CastleParameters;

{$I castleconf.inc}

interface

uses SysUtils, CastleVectors, CastleUtils, CastleStringUtils;

type
  EInvalidParams = class(EShortErrorMessage);
  EInvalidShortOption = class(EInvalidParams);
  EInvalidLongOption = class(EInvalidParams);
  EWrongOptionArgument = class(EInvalidParams);
  EExcessiveOptionArgument = class(EWrongOptionArgument);
  EMissingOptionArgument = class(EWrongOptionArgument);

  TOptionArgument = (
    { No arguments allowed. }
    oaNone,

    { An optional argument. It must be given as @--option=argument
      or (short form) -o=argument.

      If you use a short form and you combine many short option names
      into one parameter, then only the last option may have an argument.
      For example @code(-abc=blah) is equivalent to @code(-a -b -c=blah). }
    oaOptional,

    { A required argument. It must be given as for oaOptional,
      but this time the equal sign is not needed (we know anyway that
      following parameter must be an argument). So the following versions
      are possible:

      @preformatted(
        --option=argument # long form, as one parameter
        --option argument # long form, as two parameters
        -o=argument # short form, as one parameter
        -o argument # short form, as two parameters
      ) }
    oaRequired,

    { We require a specified (more than one) argument.
      All of the arguments must be specified as separate parameters, like

      @preformatted(
        --option Argument1 Argument2
        -o Argument1 Argument2
      ) }
    oaRequired2Separate,
    oaRequired3Separate,
    oaRequired4Separate,
    oaRequired5Separate,
    oaRequired6Separate,
    oaRequired7Separate,
    oaRequired8Separate,
    oaRequired9Separate
  );

  TOptionArguments = set of TOptionArgument;

const
  oaRequiredSeparateFirst = oaRequired2Separate;
  oaRequiredSeparateLast = oaRequired9Separate;

  RequiredSeparateFirstCount = 2;
  RequiredSeparateLastCount = RequiredSeparateFirstCount
    + Ord(oaRequiredSeparateLast) - Ord(oaRequiredSeparateFirst);

  OptionArgumentsRequiredSeparate: TOptionArguments =
    [oaRequiredSeparateFirst .. oaRequiredSeparateLast];

type
  TOptionSeparateArgument = oaRequiredSeparateFirst .. oaRequiredSeparateLast;
  TSeparateArgs = array[1..RequiredSeparateLastCount] of string;

const
  EmptySeparateArgs: TSeparateArgs = ('','','', '','','', '','','');

type
  { Callback used by TParameters.Parse to notify about new option.

    @param(OptionNum The option number in the Options table (zero-based).)

    @param(HasArgument Says if you have a single argument in the Argument
      parameter. Always @false when your option has oaNone or
      oaRequiredXSeparate. Always @true when your option has oaRequired.
      For oaOptional, this is how you know if the optional argument was used.)

    @param(Argument A single argument for oaRequired or oaOptional,
      only if HasArgument. Otherwise empty string.)

    @param(SeparateArgs For options using oaRequiredXSeparate,
      your arguments are here. You get exactly as many argument
      as your oaRequiredXSeparate requested, the rest of SeparateArgs
      is empty strings.)
  }
  TOptionMethod = procedure (
    const OptionNum: Integer; const HasArgument: Boolean;
    const Argument: String; const SeparateArgs: TSeparateArgs) of object;

  { Non-method version of the @link(TOptionMethod).
    Some parameters are not "const" just to not break backward compatibility.

    Most parameters are the same, with the same meaning, as @link(TOptionMethod).

    @param(Data This is the OptionProcData value you passed to TParameters.Parse,
      use this to pass some pointer to your callback.) }
  TOptionProc = procedure (OptionNum: Integer; HasArgument: Boolean;
    const Argument: String; const SeparateArgs: TSeparateArgs; Data: Pointer);

  { Command-line option specification, for TParameters.Parse.

    Both Short and Long option names are case-sensitive.
    The convention is to make Long option names using only lower-case letters,
    separates by dashes, like @code(my-option-name).

    Note that spaces are allowed (as Short option name, or within Long
    option nam), but in practice should not be used as they are a pain
    to pass for the users (you'd have to quote option names under most shells). }
  TOption = record
    { Short option name. Use #0 if none. Cannot be '-' or '=' (these would
      cause ambiguity when parsing options). }
    Short: Char;
    { Long option name. Use '' if none. Cannot contain '=' (this would
      cause ambiguity when parsing options). }
    Long: string;
    Argument: TOptionArgument;
  end;
  POption = ^TOption;

  TOption_Array = array [0..MaxInt div SizeOf(TOption) - 1] of TOption;
  POption_Array = ^TOption_Array;

  { Storing and processing command-line parameters and options.
    For simple processing, you can just read values of this list,
    checking count with comfortable methods like CheckHigh.
    For more involved processing, use the Parse function,
    that does a whole job for you based on a simple specification
    of allowed options.

    Some terminology:

    @definitionList(
      @itemLabel @italic(Parameter)
      @item(Command-line parameters list is given directly by the OS to our
        program. These are the contents of this list,
        initialized from the standard Pascal ParamStr/ParamCount.
        They can be modified to remove the already-handled parameters.)

      @itemLabel @italic(Option)
      @item(Options are things encoded by the user in the parameters.
        Examples:

        @unorderedList(
          @item(Command-line

            @preformatted(  castle-model-viewer --navigation Walk)

            passes two parameters (@code(@--navigation) and @code(Walk))
            for castle-model-viewer, and these two parameters form one option:
            @code(@--navigation=Walk).)

         @item(Command-line

            @preformatted(  castle-model-viewer -hv)

            passes one parameter (@code(-hv)) for castle-model-viewer,
            and inside this parameter two options are encoded:
            @code(-h) and @code(-v).)
        )

        The very idea of this unit is to decode "options" from the "parameters".
      )

      @itemLabel @italic(Argument)
      @item(Argument is a part of the option, that clarifies what this option does.
        For example in @code(@--navigation=Walk), "@code(Walk)" is the argument
        and "@code(@--navigation)" is the option long name.

        Some options don't take any arguments, some take optional argument,
        some take required argument, some have a couple of arguments.
        TOptionArgument type allows you to specify all this.)
    )

    For simple programs, you can directly parse command-line by looking at
    our parameters strings. For more involved cases,
    using @link(Parse) method has a lot of advantages:

    @unorderedList(
      @item(Less error-prone, and your program's code stays simple.)

      @item(We automatically handle special parameter @-- that is a standard
        way to mark the end of the options. (Useful for users that have filenames
        that start with "-" character.))

      @item(We automatically detect and make exceptions with nice messages
        on various errors. For example unrecognized options are clearly
        reported (so they will not mistaken for e.g. missing filenames
        by your program).)

      @item(We automatically allow combining of short options,
        so user can use @code(-abc) instead of @code(-a -b -c).)

      @item(We have a simple interface, where you simply specify what
        options you want, long and short option names, option arguments
        and such.)
    )

    See [https://castle-engine.io/common_options.php]
    for a user description how short and long options are expected to be given
    on the command-line. }
  TParameters = class(TCastleStringList)
  public
    function High: Integer;

    { Does the number of parameters (High) satisfy given condition.
      @raises EInvalidParams When High is wrong.
      @groupBegin }
    procedure CheckHigh(ParamValue: integer);
    procedure CheckHighAtLeast(ParamValue: integer);
    procedure CheckHighAtMost(ParamValue: integer);
    { @groupEnd }

    { Is one of given strings present on the parameters list.
      Looks inside Strings[1..High], case sensitive. }
    function IsPresent(const A: array of string): boolean;

    { Parse command-line parameters. Given a specification of your command-line
      options (in AOptions), we will find and pass these options to your
      OptionMethod callback. The handled options will be removed from
      the @link(Parameters) list.

      After running this, you should treat the remaining @link(Parameters)
      as "normal" parameters, usually a filenames to open by your program or such.

      See also TOption for a specification of an option,
      and see TOptionArgument for a specification of an option argument,
      and see TOptionMethod for a specification what your OptionMethod callback gets.

      @raises EInvalidShortOption On invalid (unknown) short option name.
      @raises EInvalidLongOption On invalid long option name.
      @raises(EExcessiveOptionArgument When an option gets too many arguments,
        this may happen for options with oaNone or oaRequiredXSeparate
        that are specified with @code(--option=argument) form.)
      @raises(EMissingOptionArgument When an option gets too few arguments,
        this may happen when argument for oaRequired option is missing,
        or when too few arguments are given for oaRequiredXSeparate option.)
      @raises(EInvalidParams On invalid parameter without an option,
        like @code(-=argument) or @code(--=argument).)

      Note that a single dash parameter is left alone, without making any
      exceptions, as this is a standard way of telling "standard input"
      or "standard output" for some programs.

      Note that a double dash parameter @-- is handled and removed from
      the @link(Parameters) list, and signals an end of options.

      You should not modify @link(Parameters) list when this function
      is running, in particular do not modify it from your OptionMethod callback.
      Also, do not depend on when the handled options are exactly removed
      from the @link(Parameters) list (before or after OptionMethod callback).

      We never touch here the Strings[0] value, we look
      only at the Strings[1] to Strings[High].

      ParseOnlyKnownOptions = @true makes this procedure work a little
      differently, it's designed to allow you to process @italic(some) options
      and leave the rest options not handled (without making any error):

      @orderedList(
        @item(The "known" short options (in Options param) are recognized
          only if they are not combined with other options.
          E.g. if you allow '-c' in Options, we will handle '-c' parameter,
          but 'c' will be ignored in combined '-abc' parameter.)
        @item(All unknown short and long options are ignored, without making any error.)
        @item(The special @-- is handled (signals the end of options),
          but it's not removed from the @link(Parameters).)
      )

      The ParseOnlyKnownOptions = @true is useful if you want to handle
      some command-line options, but you still want to leave final options
      parsing to a later code. For example TCastleWindow.ParseParameters parses
      some window parameters (like --geometry), leaving your program-specific
      stuff in peace.

      Note that ParseOnlyKnownOptions = @true isn't an absolutely
      fool-proof solution, for example the command-line
      @code(castle-model-viewer --navigation --geometry 800x600 Walk) is actually invalid.
      But we will handle it, by first detecting and removing @code(--geometry 800x600)
      from TCastleWindow.ParseParameters, and then detecting and removing
      @code(--navigation Walk) from castle-model-viewer code.
      Basically, processing by Parse many times is not fool-proof
      in some weird situations.

      It has a few overloads, we advise to use the 1st overload (with
      "const AOptions: array of TOption;" and "const OptionMethod: TOptionMethod")
      as the most convenient one.

      See the simplest demo application here:
      https://gist.github.com/michaliskambi/d0a9d307f6684c45fe55962305b2a66f

      @groupBegin }
    procedure Parse(const AOptions: array of TOption;
      const OptionMethod: TOptionMethod;
      const ParseOnlyKnownOptions: boolean = false); overload;
    procedure Parse(const AOptions: array of TOption;
      const OptionProc: TOptionProc; const OptionProcData: Pointer;
      const ParseOnlyKnownOptions: boolean = false); overload;
    procedure Parse(const AOptions: POption_Array; const OptionsCount: Integer;
      const OptionProc: TOptionProc; const OptionProcData: Pointer;
      const ParseOnlyKnownOptions: boolean = false); overload;
    { @groupEnd }
  end;

function SeparateArgsToVector3(const v: TSeparateArgs): TVector3;
function SeparateArgsToVector3Single(const v: TSeparateArgs): TVector3;
  deprecated 'use SeparateArgsToVector3';

const
  OnlyHelpOptions: array [0..0] of TOption = (
    (Short: 'h'; Long: 'help'; Argument: oaNone)
  ) deprecated 'define this in your application';

  HelpOptionHelp =
    '  -h / --help           Print this help message and exit.'
    deprecated 'use OptionDescription';
  VersionOptionHelp =
    '  -v / --version        Print the version number and exit.'
    deprecated 'use OptionDescription';

var
  { Command-line parameters. Initialized from standard
    ParamStr(0) ... ParamStr(ParamCount). Can be later modified,
    which is good --- you can remove handled parameters.
    You also have all the methods of TCastleStringList class
    (e.g. you can assign to another TCastleStringList instance). }
  Parameters: TParameters;

{ Return a multiline command-line option help,
  containing the option name and description,
  nicely indented and broken into multiple lines.
  Existing newlines in Description are correctly accounted for. }
function OptionDescription(const Name, Description: string): string;

implementation

uses CastleLog
  {$ifdef CASTLE_ANDROID_ARGV_LOGGING} , CastleAndroidInternalLog {$endif};

function OptionSeparateArgumentToCount(const v: TOptionSeparateArgument): Integer; forward;

{ TParameters ---------------------------------------------------------------- }

function ParametersCountString(Count: Integer; const MiddleStr: string): string; overload;
begin
  result := IntToStr(Count);
  if Count = 1 then
    result := result +MiddleStr +' parameter'
  else
    result := result +MiddleStr +' parameters';
end;

function ParametersCountString(Count: Integer): string; overload;
begin
  result := ParametersCountString(Count, '');
end;

{ Since we can modify Parameters, we can't really output
  in CheckHigh* for user how many parameters were excepted (because you maybe
  ate some). Output only how many params are missing/too much. }

function TParameters.High: Integer;
begin
  Result := Count - 1;
end;

procedure TParameters.CheckHigh(ParamValue: integer);
begin
  if ParamValue <> High then
  begin
    if ParamValue < High then
      raise EInvalidParams.Create('Expected ' +
        ParametersCountString(High-ParamValue, ' less')) else
      raise EInvalidParams.Create('Expected ' +
        ParametersCountString(ParamValue-High, ' more'));
  end;
end;

procedure TParameters.CheckHighAtLeast(ParamValue: integer);
begin
  if ParamValue > High then
    raise EInvalidParams.Create('Expected ' +
      ParametersCountString(ParamValue-High, ' more'));
end;

procedure TParameters.CheckHighAtMost(ParamValue: integer);
begin
  if ParamValue < High then
    raise EInvalidParams.Create('Expected ' +
      ParametersCountString(High-ParamValue, ' less'));
end;

function TParameters.IsPresent(const A: array of string): boolean;
var
  I, J: Integer;
begin
  for I := 1 to High do
    for J := 0 to System.High(A) do
      if Strings[I] = A[J] then
        Exit(true);
  Result := false;
end;

type
  TMethodWrapper = record
    MethodToCall: TOptionMethod;
  end;
  PMethodWrapper = ^TMethodWrapper;

{ Call TOptionMethod within PMethodWrapper(Data)^. }
procedure OptionProcToCallMethod(OptionNum: Integer; HasArgument: Boolean;
  const Argument: String; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
  PMethodWrapper(Data)^.MethodToCall(OptionNum, HasArgument, Argument, SeparateArgs);
end;

procedure TParameters.Parse(const AOptions: array of TOption;
  const OptionMethod: TOptionMethod; const ParseOnlyKnownOptions: boolean);
var
  MethodWrapper: TMethodWrapper;
begin
  MethodWrapper.MethodToCall := OptionMethod;
  Parse(AOptions, {$ifdef FPC}@{$endif} OptionProcToCallMethod, @MethodWrapper,
    ParseOnlyKnownOptions);
end;

procedure TParameters.Parse(const AOptions: array of TOption; const OptionProc: TOptionProc;
  const OptionProcData: Pointer; const ParseOnlyKnownOptions: boolean);
begin
  Parse(@AOptions, System.High(AOptions) + 1, OptionProc, OptionProcData,
    ParseOnlyKnownOptions);
end;

procedure TParameters.Parse(
  const AOptions: POption_Array; const OptionsCount: Integer; const OptionProc: TOptionProc;
  const OptionProcData: Pointer; const ParseOnlyKnownOptions: boolean);

  { Splits S into

    - rejected prefix, which is either "-" or "--" depending on PrefixLength = 1 or 2
    - ParamName, which is the part before "=" (or everything, if parameter has no "="),
      and it cannot be empty otherwise we raise exception
    - Argument, which is the part after "=" (or nothing, if parameter has no "=")

    Examples:

    SplitParameter('-', ..., 1) -> exception (empty ParamName)
    SplitParameter('--', ..., 2) -> exception (empty ParamName)
    SplitParameter('--=file.txt', ..., 2) -> exception (empty ParamName)
    SplitParameter('-=file.txt', ..., 1) -> exception (empty ParamName)
    SplitParameter('-f=file.txt', ..., 1) ->
      ParamName = 'f'
      HasArgument = true
      Argument = 'file.txt'
    SplitParameter('--read-file=file.txt', ..., 2) ->
      ParamName = 'read-file'
      HasArgument = true
      Argument = 'file.txt'
    SplitParameter('-xzvf=file.txt', ..., 2) ->
      ParamName = 'xzvf' // called must interpret it as a series of short options
      HasArgument = true
      Argument = 'file.txt'
  }
  procedure SplitParameter(const S: string; out ParamName: string;
    out HasArgument: boolean; out Argument: string; const PrefixLength: Integer);
  var
    p: Integer;
  begin
    p := Pos('=', s);
    HasArgument := p <> 0;
    if HasArgument then
    begin
      ParamName := CopyPos(s, PrefixLength + 1, p - 1);
      Argument := SEnding(s, p+1);
    end else
    begin
      ParamName := SEnding(s, PrefixLength + 1);
      Argument := '';
    end;

    if ParamName = '' then
      raise EInvalidParams.Create('Invalid empty parameter "' + s + '"');
  end;

  { Assumption: s is some parameter that starts with '--' and is not equal exactly '--'.

    This extracts from S the option it represents (and returns its number in Params,
    zero-based), also extracts parameter written together with it and returns
    HasArgument and Argument (remember that it extracts only arguments attached
    to option using "=" sign; it also doesn't check at all whether HasArgument
    in some way matches Options[result].Argument).

    If ParseOnlyKnownOptions then it may return -1 to indicate that
    this parameter doesn't represent any known option (although still
    invalid forms like --=argument or --non-arg-option=argument
    will obviously cause exception.) }
  function ParseLongParameter(const S: string; out HasArgument: boolean;
    out Argument: string): Integer;
  var
    ParamLong: string;
    i: Integer;
  begin
    SplitParameter(s, ParamLong, HasArgument, Argument, 2);
    for i := 0 to OptionsCount-1 do
      if AOptions^[i].Long = ParamLong then
        begin result := i; Exit; end;

    if ParseOnlyKnownOptions then
      result := -1
    else
      raise EInvalidLongOption.Create('Invalid long option "'+s+'"');
  end;

  { Find index of option with AOptions[I].Short = C (also check that C <> #0).

    If fails -- EInvalidshortOption (if ParseOnlyKnownOptions = false)
    or exits with -1 (if ParseOnlyKnownOptions).

    Note that "Parameter" is useful only to construct nice error message,
    it should be parameter where we found C. }
  function FindShortOption(const c: char; const Parameter: string): Integer;
  const
    SInvalidShortOpt = 'Invalid short option character "%s" in parameter "%s"';
  begin
    if c = #0 then
    begin
      if ParseOnlyKnownOptions then
        Exit(-1)
      else
        raise EInvalidShortOption.CreateFmt(SInvalidShortOpt, ['#0 (null char)', Parameter]);
    end;

    for result := 0 to OptionsCount-1 do
      if AOptions^[result].Short = c then
        Exit;

    if ParseOnlyKnownOptions then
      Exit(-1)
    else
      raise EInvalidShortOption.CreateFmt(SInvalidShortOpt, [c, Parameter]);
  end;

  { Assumption: S is some parameter starting with '-' and not being exactly '-'.

    This Works like ParseLongParameter...

    - additionally to SimpleShortOptions it will append sequence of simple options that were
      given together with last option (i.e. with option returned under name).
      These simple options were "combined" together with last option in
      one parameter. As a result I call them "simple" because they cannot
      have argument - AOptions^[].Argument of these options can be only oaNone
      or oaOptional. This procedure does NOT check:

      - That AOptions^[].Argument is in [oaNone, oaOptional], as assumed above.

      - This also doesn't check any AOptions^[].Argument at all,

      - also for last (returned under name) option it doesn't check anything.
        So it can return oaNone option with HasArgument or oaRequired[*Separate]
        with not HasArgument.

    Caller must check whether HasArgument makes sense with
    returned option. In case of oaRequired[*Separate] it may/must read
    further parameters to form argument/arguments of option.
    The rule is that this procedure deals ONLY with parameter S.
    It doesn't go into other Strings[], moreover it doesn't know at all for which
    I the Strings[I] = S.
  }
  function ParseShortParameter(const S: string; out HasArgument: boolean;
    out Argument: string; const SimpleShortOptions: TIntegerList): Integer;
  var
    ParamAllShortOptions: string;
    OptionIndex: Integer;
    i: Integer;
  begin
    { calculate ParamAllShortOptions, HasArgument, Argument }
    SplitParameter(s, ParamAllShortOptions, HasArgument, Argument, 1);

    { When ParseOnlyKnownOptions we ignore combined short options, like '-abc',
      even if one of them (like 'c') is known.
      It would be complicated to remove only known options. }
    if ParseOnlyKnownOptions and (Length(ParamAllShortOptions) > 1) then
      Exit(-1);

    { add to SimpleShortOptions }
    for i := 1 to Length(ParamAllShortOptions) - 1 do
    begin
      OptionIndex := FindShortOption(ParamAllShortOptions[i], s);
      { we are here only if Length(ParamAllShortOptions) > 1,
        so ParseOnlyKnownOptions is false,
        so FindShortOption always answers with something <> -1. }
      Assert(OptionIndex <> -1);
      SimpleShortOptions.Add(OptionIndex);
    end;

    { handle last short option in ParamAllShortOptions }
    OptionIndex := FindShortOption(ParamAllShortOptions[Length(ParamAllShortOptions)], s);
    { OptionIndex may be -1 here,
      if ParseOnlyKnownOptions = true and FindShortOption returned -1,
      and then we just want to return -1. }
    Result := OptionIndex;
  end;

var
  i, j, k, OptionNum: Integer;
  HasArgument: boolean;
  Argument, OptionName: string;
  SeparateArgs: TSeparateArgs;
  SimpleShortOptions: TIntegerList;
begin
  i := 1;
  SimpleShortOptions := TIntegerList.Create;
  try

    while i <= High do
    begin
      if Strings[i] = '--' then
      begin
        if not ParseOnlyKnownOptions then Delete(I);
        Break;
      end;

      Assert(SimpleShortOptions.Count = 0);

      { calculate OptionNum:

        - -1 if Strings[I] is not recognized as a valid option.

          This can happen only if ParseOnlyKnownOptions (otherwise invalid
          option will cause exception from ParseShortParameter, ParseLongParameter)

        - or an index in Params, if Strings[I] is a valid option.
          In this case also OptionName is set.
          In this case also SimpleShortOptions is relevant,
          which are combined short options present before the option indicated by OptionNum.

        E.g. "-abc=file.txt", assuming that all "a" "b" "c" are valid short options,
        means that "a", "b" are in SimpleShortOptions,
        and "c" is indicated by OptionNum and OptionName.

        Note about "Length(Strings[i]) > 1" condition:
        it means that "-" is treated as non-special parameter (it is commonly used
        as stdin / stdout name), instead of making an exception about invalid short option. }

      OptionNum := -1;
      if SCharIs(Strings[i], 1, '-') and (Length(Strings[i]) > 1) then
      begin
        if SCharIs(Strings[i], 2, '-') then
        begin
          OptionNum := ParseLongParameter(Strings[i], HasArgument, Argument);
          if OptionNum <> -1 then OptionName := '--'+AOptions^[OptionNum].Long;
        end else
        begin
          OptionNum := ParseShortParameter(Strings[i], HasArgument, Argument, SimpleShortOptions);
          if OptionNum <> -1 then OptionName := '-'+AOptions^[OptionNum].Short;
        end;
      end;

      if OptionNum <> -1 then
      begin
        { first handle SimpleShortOptions }
        for k := 0 to SimpleShortOptions.Count-1 do
        begin
          if not (AOptions^[SimpleShortOptions[k]].Argument in [oaNone, oaOptional]) then
            raise EMissingOptionArgument.Create('Missing argument for short option -'+
              AOptions^[SimpleShortOptions[k]].Short +'; when combining short options only the last '+
              'option can have an argument');
          OptionProc(SimpleShortOptions[k], false, '', EmptySeparateArgs, OptionProcData);
        end;
        SimpleShortOptions.Count := 0;

        { now handle OptionNum and OptionName }

        Delete(I);
        SeparateArgs := EmptySeparateArgs;

        { upewnij sie ze HasArgument ma dopuszczalna wartosc. Odczytaj argumenty
          podane jako osobne paranetry dla oaRequired i oaRequired?Separate. }

        if (AOptions^[OptionNum].Argument = oaRequired) and (not HasArgument) then
        begin
          if i > High then
            raise EMissingOptionArgument.Create('Missing argument for option '+OptionName);
          HasArgument := true;
          Argument := Strings[i];
          Delete(i);
        end else
        if (AOptions^[OptionNum].Argument = oaNone) and HasArgument then
        begin
          raise EExcessiveOptionArgument.Create('Excessive argument for option '+OptionName);
        end else
        if AOptions^[OptionNum].Argument in OptionArgumentsRequiredSeparate then
        begin
          if HasArgument then
            raise EExcessiveOptionArgument.CreateFmt('Option %s requires %d arguments, '+
              'you cannot give them using the form --option=argument, you must give '+
              'all the arguments as separate parameters', [OptionName,
              OptionSeparateArgumentToCount(AOptions^[OptionNum].Argument) ]);

          for j := 1 to OptionSeparateArgumentToCount(AOptions^[OptionNum].Argument) do
          begin
            if i > High then
              raise EMissingOptionArgument.CreateFmt('Not enough arguments for option %s, '+
                'this option needs %d arguments but we have only %d', [OptionName,
                OptionSeparateArgumentToCount(AOptions^[OptionNum].Argument), j-1]);
            SeparateArgs[j] := Strings[i];
            Delete(i);
          end;
        end;

        OptionProc(OptionNum, HasArgument, Argument, SeparateArgs, OptionProcData);
      end else
        Inc(i);
    end;

  finally FreeAndNil(SimpleShortOptions) end;
end;

{ some simple helper utilities ---------------------------------------------- }

function OptionSeparateArgumentToCount(const v: TOptionSeparateArgument): Integer;
begin
  Result := RequiredSeparateFirstCount + Ord(v) - Ord(oaRequiredSeparateFirst)
end;

function SeparateArgsToVector3(const v: TSeparateArgs): TVector3;
begin
  Result.X := StrToFloatDot(v[1]);
  Result.Y := StrToFloatDot(v[2]);
  Result.Z := StrToFloatDot(v[3]);
end;

function SeparateArgsToVector3Single(const v: TSeparateArgs): TVector3;
begin
  Result := SeparateArgsToVector3(V);
end;

procedure InitializationParams;
{$ifndef CASTLE_PARAMSTR_BUGGY}
var
  I: Integer;
{$endif}
begin
  Parameters := TParameters.Create;

  { Android 15, Google Pixel 6a, Aarch64 (aka "64-bit Arm") exhibits a weird bug:
    Argv[...] values, like Argv[0] (but also following ones), are invalid pointers.
    See below for details: some of them are nil, some of them are non-nil
    but a weirdly small value as a number (like just number 6, equals to Argc actually),
    accessing any of them crashes with SIGSEGV.

    The same device worked OK before upgrading from Android 14.

    The problem is *not* reproduced on other test devices:
    - Android 11 (Samsung Galaxy tablet),
    - Android 13 (Fairphone 4),
    - emulated Android 15 x86_64 with Google APIs.

    Note: We also tried to run on this, but AVD crashes when run:
    - emulated Android 15 arm64 with Google Play (so, as close as possible
      to Google Pixel 6a as we could).

    Define CASTLE_ANDROID_ARGV_LOGGING to observe
    (testing with CGE examples/eye_of_beholder below, but it affects any
    CGE application):

      ... I eye_of_beholder: Argc: 6
      ... I eye_of_beholder: ParamCount (should be Argc-1): 5
      ... I eye_of_beholder: Argv <> nil: True
      ... I eye_of_beholder: Will access Argv[0]
      ... I eye_of_beholder: Argv[0] <> nil: True
      ... I eye_of_beholder: Argv[0] as a pointer: 0x0000000000000006

    and then reading Argv[0] crashes with:

      ... F libc    : Fatal signal 11 (SIGSEGV), code 1 (SEGV_MAPERR), fault addr 0x6 in tid 7531 (eye.of.beholder), pid 7531 (eye.of.beholder)
      ...
      ... I crash_dump64: performing dump of process 7531 (target tid = 7531)
      ... F DEBUG   : *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** ***
      ... F DEBUG   : Build fingerprint: 'google/bluejay/bluejay:15/AP4A.250105.002.A1/12703047:user/release-keys'
      ... F DEBUG   : Revision: 'MP1.0'
      ... F DEBUG   : ABI: 'arm64'
      ... F DEBUG   : Timestamp: 2025-01-29 13:58:16.466745658+0100
      ... F DEBUG   : Process uptime: 2s
      ... F DEBUG   : Cmdline: io.castleengine.eye.of.beholder
      ... F DEBUG   : pid: 7531, tid: 7531, name: eye.of.beholder  >>> io.castleengine.eye.of.beholder <<<
      ... F DEBUG   : uid: 10368
      ... F DEBUG   : tagged_addr_ctrl: 0000000000000001 (PR_TAGGED_ADDR_ENABLE)
      ... F DEBUG   : signal 11 (SIGSEGV), code 1 (SEGV_MAPERR), fault addr 0x0000000000000006
      ... F DEBUG   : Cause: null pointer dereference
      ... F DEBUG   :     x0  0000007fdfae2c48  x1  0000000000000006  x2  0000000000000000  x3  0000000000000000
      ... F DEBUG   :     x4  00000077908fb698  x5  0000007ae8ffd2f1  x6  0000000000000000  x7  0000000000000034
      ... F DEBUG   :     x8  0000007af0965b70  x9  0000000000000001  x10 0000000000000001  x11 0000000000001d6b
      ... F DEBUG   :     x12 0000000000000000  x13 0000000000000001  x14 0000000000000000  x15 0000000000000010
      ... F DEBUG   :     x16 0000007790ff80e8  x17 0000007aeb4e8b80  x18 0000007af18b8000  x19 0000007fdfae2c48
      ... F DEBUG   :     x20 0000007fdfae2c48  x21 0000000000000006  x22 0000000000000000  x23 00000077908a46c0
      ... F DEBUG   :     x24 0000007af1c55000  x25 0000000000000002  x26 0000007af1c55000  x27 0000007af1c55000
      ... F DEBUG   :     x28 0000007af1c55000  x29 0000007fdfae2a50
      ... F DEBUG   :     lr  00000077908a8514  sp  0000007fdfae2a30  pc  0000007790894a2c  pst 0000000020001000
      ... F DEBUG   : 1 total frames
      ... F DEBUG   : backtrace:
      ... F DEBUG   :       #00 pc 000000000047fa2c  /data/app/~~43_eojuIg4r_xSRJeVmOMQ==/io.castleengine.eye.of.beholder-Zn9hz7QAqZKCRKUukh0jcw==/lib/arm64/libeye_of_beholder_android.so (BuildId: 1ae92e229a184f079ea978419d0b01cacdd90a45)

    Investigating further Argv (to see whether Argv is maybe "shifted",
    and Argv[0] is pointing to Argc (as they are both 6),
    and Argv[1] is actually Argv[0])... no, further Argv[...] values are also
    weird and incorrect. Below you can see that Argv[0] = Argv[1] = Argv[2] = 6,
    then Argv[3] = Argv[4] = 0, and only Argv[5] *looks* like a valid pointer
    (but it's not, accessing it crashes!).
    So, all Argv values are just nonsense.

      ... I eye_of_beholder: Will access Argv[0]
      ... I eye_of_beholder: Argv[0] <> nil: True
      ... I eye_of_beholder: Argv[0] as a pointer: 0x0000000000000006
      ... I eye_of_beholder: Will access Argv[1]
      ... I eye_of_beholder: Argv[1] <> nil: True
      ... I eye_of_beholder: Argv[1] as a pointer: 0x0000000000000006
      ... I eye_of_beholder: Will access Argv[2]
      ... I eye_of_beholder: Argv[2] <> nil: True
      ... I eye_of_beholder: Argv[2] as a pointer: 0x0000000000000006
      ... I eye_of_beholder: Will access Argv[3]
      ... I eye_of_beholder: Argv[3] <> nil: False
      ... I eye_of_beholder: Argv[3] as a pointer: 0x0000000000000000
      ... I eye_of_beholder: Will access Argv[4]
      ... I eye_of_beholder: Argv[4] <> nil: False
      ... I eye_of_beholder: Argv[4] as a pointer: 0x0000000000000000
      ... I eye_of_beholder: Will access Argv[5]
      ... I eye_of_beholder: Argv[5] <> nil: True
      ... I eye_of_beholder: Argv[5] as a pointer: 0x481F0000000B0109
  }
  {$ifdef CASTLE_ANDROID_ARGV_LOGGING}
  AndroidLog(alInfo, 'Argc: %d', [Argc]);
  AndroidLog(alInfo, 'ParamCount (should be Argc-1): %d', [ParamCount]);
  AndroidLog(alInfo, 'Argv <> nil: %s', [
    BoolToStr(Argv <> nil, true)
  ]);
  {$endif}

  {$ifdef CASTLE_PARAMSTR_BUGGY}
  { Add non-empty name, just in case something relies on Parameters[0].
    Regular CGE code should always access "ApplicationProperties.ApplicationName"
    or global "ApplicationName", which are set (by "CastleAutoGenerated"
    unit in a project) to reflect application name without relying on
    ParamStr(0) / Parameters[0]. }
  Parameters.Add('application-name-unknown');
  WritelnWarning('ParamStr is buggy on this platform, workarounding: Parameters[0] will be set to "application-name-unknown". Recommendation: Do not rely on Parameters / ParamStr on this platform and use ApplicationName instead of Parameters[0] / ParamStr(0).');

  {$else}
  for I := 0 to ParamCount do
  begin
    {$ifdef CASTLE_ANDROID_ARGV_LOGGING}
    AndroidLog(alInfo, 'Will access Argv[%d]', [I]);
    AndroidLog(alInfo, 'Argv[%d] <> nil: %s', [
      I,
      BoolToStr(Argv[I] <> nil, true)
    ]);
    AndroidLog(alInfo, 'Argv[%d] as a pointer: %s', [
      I,
      PointerToStr(Argv[I])
    ]);
    AndroidLog(alInfo, 'Argv[%d] value: %s', [
      I,
      Argv[I]
    ]);
    AndroidLog(alInfo, 'ParamStr(%d) value: %s', [
      I,
      ParamStr(I)
    ]);
    {$endif}
    Parameters.Add(ParamStr(I));
  end;
  {$endif}
end;

procedure FinalizationParams;
begin
  FreeAndNil(Parameters);
end;

function OptionDescription(const Name, Description: string): string;
const
  MaxLineWidth = 75;
  Indent = 24;
  NameIndent = 2;
var
  PadLength: Integer;
begin
  Result := StringOfChar(' ', NameIndent) + Name;
  PadLength := Indent - Length(Result);
  if PadLength > 0 then
    { option name and first line of description can fit on a single line }
    Result := Result + StringOfChar(' ', PadLength)
  else
    Result := Result + NL + StringOfChar(' ', Indent);

  Result := Result + BreakLine(Description, MaxLineWidth - Indent, WhiteSpaces,
    NL, StringOfChar(' ', Indent));
end;

initialization
  InitializationParams;
finalization
  FinalizationParams;
end.
