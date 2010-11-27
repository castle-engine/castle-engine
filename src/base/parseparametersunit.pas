{
  Copyright 2003-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Parsing command-line arguments.

  Some terminology:

  @definitionList(
    @itemLabel Parameter
    @item(Command-line parameters list is given directly by the OS to our
      program.

      This unit obtains them from the @link(Parameters) list.
      This is initialized from standard Pascal ParamStr/ParamCount.
      It can be modified to remove the already-handled parameters.)

    @itemLabel Option
    @item(Options are things encoded by the user in the parameters.
      Examples:

      @unorderedList(
        @item(Command-line

          @preformatted(  view3dscene --navigation Walk)

          passes two parameters (@code(@--navigation) and @code(Walk))
          for view3dscene, and these two parameters form one option:
          @code(@--navigation=Walk).)

       @item(Command-line

          @preformatted(  view3dscene -hv)

          passes one parameter (@code(-hv)) for view3dscene,
          and inside this parameter two options are encoded:
          @code(-h) and @code(-v).)
      )

      The very idea of this unit is to decode "options" from the "parameters".
    )

    @itemLabel Argument
    @item(Is a part of the option, that clarifies what this option does.
      For example in @code(@--navigation=Walk), @code(Walk) is the argument
      and @--navigation is the option long name.

      Some options don't take any arguments, some take optional argument,
      some take required argument, some have a couple of arguments.
      TOptionArgument type allows you to specify all this.)
  )

  For simple programs, you can directly parse command-line by looking at
  ParamStr/ParamCount or the @link(Parameters) list. For more involved cases,
  using ParseParameters has a lot of advantages:

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

  See [http://vrmlengine.sourceforge.net/common_options.php]
  for a user description how short and long options are expected to be given
  on the command-line.
}
unit ParseParametersUnit;

{$I kambiconf.inc}

interface

uses SysUtils, VectorMath, KambiUtils;

{$define read_interface}

type
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
  TSeparateArgs = array[1..RequiredSeparateLastCount]of string;

const
  EmptySeparateArgs: TSeparateArgs = ('','','', '','','', '','','');

type
  { Gdy ta funkcja bedzie wywolywana z funkcji ParseParameters to znaczenie parametrow bedzie
    nastepujace :

      OptionNum = bedzie numer opcji z tablicy Options (zero-based).

      HasArgument = false, jesli Options[ParamNum].Argument
      in [oaNone, oaRequired?Separate] lub (oaOptional i nie podano argumentu).
      true wpp. (a wiec gdy Options[ParamNum].Argument = oaRequired lub
      (oaOptional i podano argument).

      Argument = jesli HasArgument to jest to argument podany przy opcji,
      wpp. '' (notka: zwroc uwage ze nic nie przeszkadza userowi podac argument = ''.
      Dlatego wlasnie potrzebny jest parametr HasArgument zeby opcje
      oaOptional mogly rozpoznac czy user nie podal argumentu (HasArgument=false)
      czy tez podal argument rowny '' (HasArgument = true, Argument = '').

      SeparateArgs = jezeli Options[ParamNum].Argument in oaRequired?Separate to
      odpowiednia ilosc poczatkowych SeparateArgs bedzie usatwiona na poczatkowe
      argumenty, np. dla oaRequired2Separate powinienes odczytac argumenty
      z SeparateArgs[1] i SeparateArgs[2]. Wszystkie pozostale SeparateArgs
      (w powyzszym przykladzie, SeparateArgs[3] .. SeparateArgs[High(SeparateArgs)])
      beda rowne ''. Jesli Options[ParamNum].Argument nie byl rowny oaRequired?Separate
      to wszystkie SeparateArgs[] beda rowne '' (czyli SeparateArgs = EmptySeparateArgs).

      Data = OptionProcData podane do ParseParameters
  }
  TOptionProc = procedure (OptionNum: Integer; HasArgument: boolean;
    const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);

  TOption = record
    { Short moze byc rowne ' ' (spacji), Long moze zawierac w sobie spacje
      na dowolnych pozycjach, to nie powoduje zadnych problemow,
      ale nie jest zalecane (raczej nie ulatwialoby to zycia userowi ktory
      musialby wprowadzic opcje ze spacjami w srodku).
      Zarowno Short jak i Long beda dopasowywane case-sensitive, '-a' i '-A' to
      rozne opcje. }

    { jezeli Short = #0 to nie istnieje krotka postac opcji.
      Short nie moze byc takze '-' lub '=' (to powodowaloby wiele potencjalnych
      niejednoznacznosci) }
    Short: Char;
    { jezeli Long = '' to nie istnieje dluga postac opcji.
      Long nie moze zawierac znaku '=' (to powodowaloby pewne niejednoznacznosci) }
    Long: string;
    Argument: TOptionArgument;
  end;
  POption = ^TOption;

  TDynArrayItem_2 = TOption;
  PDynArrayItem_2 = POption;
  {$define DYNARRAY_2_IS_STRUCT}
  {$define DYNARRAY_2_IS_INIT_FINI_TYPE}
  {$I DynArray_2.inc}
  TDynOptionArray = TDynArray_2;
  TOption_Array = TInfiniteArray_2;
  POption_Array = PInfiniteArray_2;

{ Parse command-line parameters. Given a specification of your command-line
  options (in Options), we will find and pass these options to your
  OptionProc callback. The handled options will be removed from
  the @link(Parameters) list.

  After running this, you should treat the remaining @link(Parameters)
  as "normal" parameters, usually a filenames to open by your program or such.

  Most of the documentation is given at this unit's docs, see ParseParametersUnit.
  See also TOption for a specification of an option,
  and see TOptionArgument for a specification of an option argument,
  and see TOptionProc for a specification what your OptionProc callback gets.

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
  is running, in particular do not modify it from your OptionProc callback.
  Also, do not depend on when the handled options are exactly removed
  from the @link(Parameters) list (before or after OptionProc callback).

  We never touch here the Parameters[0] value, we look
  only at the Parameters[1] to Parameters[Parameters.High].

  ParseOnlyKnownLongOptions = @true makes this procedure work a little
  differently, it's designed to allow you to process @italic(some) long options
  and leave the rest options not handled (without making any error):

  @orderedList(
    @item(All short options are ignored then.)
    @item(All unknown long options are also ignored, without making any error.)
    @item(The special @-- is handled (signals the end of options),
      but it's not removed from the @link(Parameters).)
  )

  The ParseOnlyKnownLongOptions = @true is useful if you want to handle
  some command-line options, but you still want to leave final options
  parsing to a later code. For example TGLWindow.ParseParameters parses
  some window parameters (like --geometry), leaving your program-specific
  stuff in peace.

  Note that ParseOnlyKnownLongOptions = @true isn't an absolutely
  fool-proof solution, for example the command-line
  @code(view3dscene --navigation --geometry 800x600 Walk) is actually invalid.
  But we will handle it, by first detecting and removing @code(--geometry 800x600)
  from TGLWindow.ParseParameters, and then detecting and removing
  @code(--navigation Walk) from view3dscene code.
  Basically, processing by ParseParameters many times is not fool-proof
  in some weird situations.

  @groupBegin }
procedure ParseParameters(
  Options: POption_Array; OptionsCount: Integer;
  OptionProc: TOptionProc; OptionProcData: Pointer;
  ParseOnlyKnownLongOptions: boolean {$ifdef DEFPARS} =false {$endif}); overload;
procedure ParseParameters(
  const Options: array of TOption;
  OptionProc: TOptionProc; OptionProcData: Pointer;
  ParseOnlyKnownLongOptions: boolean {$ifdef DEFPARS} =false {$endif}); overload;
procedure ParseParameters(
  Options: TDynOptionArray;
  OptionProc: TOptionProc; OptionProcData: Pointer;
  ParseOnlyKnownLongOptions: boolean {$ifdef DEFPARS} =false {$endif}); overload;
{ @groupEnd }

type
  TParsedOption = record
    OptionNum: Integer;
    HasArgument: boolean;
    Argument: string;
    SeparateArgs: TSeparateArgs;
  end;
  PParsedOption = ^TParsedOption;

  TDynArrayItem_1 = TParsedOption;
  PDynArrayItem_1 = PParsedOption;
  {$define DYNARRAY_1_IS_STRUCT}
  {$define DYNARRAY_1_IS_INIT_FINI_TYPE}
  {$I DynArray_1.inc}
  TDynParsedOptionArray = TDynArray_1;

{ Parse command-line parameters returning a list of parsed options.
  Works exactly like previous ParseParameters procedure,
  but instead of using a callback like OptionProc, this time
  it returns a list.
  @groupBegin }
function ParseParameters(
  const Options: array of TOption;
  ParseOnlyKnownLongOptions: boolean {$ifdef DEFPARS} =false {$endif})
  : TDynParsedOptionArray; overload;
function ParseParameters(
  Options: POption_Array; OptionsCount: Integer;
  ParseOnlyKnownLongOptions: boolean {$ifdef DEFPARS} =false {$endif})
  : TDynParsedOptionArray; overload;
{ @groupEnd }

function OptionSeparateArgumentToCount(const v: TOptionSeparateArgument): Integer;
function SeparateArgsToVector3Single(const v: TSeparateArgs): TVector3Single;

const
  OnlyHelpOptions: array[0..1]of TOption = (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    { dodajemy tutaj dodatkowo pusta opcje tylko po to zeby obejsc bug FPC 1.0.10
      pod Windowsem, patrz InfoWriteBug (w fpc.testy/ lub
      kambi_fpc_bugs/already_fixed/). To bedzie zdjete gdy przejde na FPC 2.0. }
    (Short:  #0; Long: ''; Argument: oaNone)
  );

  HelpOptionHelp =
    '  -h / --help           Print this help message and exit';
  VersionOptionHelp =
    '  -v / --version        Print the version number and exit';

{$undef read_interface}

implementation

{$define read_implementation}

uses KambiStringUtils;

{$ifdef FPC}
procedure Initialize(var v: TOption); overload;
begin
 FillChar(v, SizeOf(v), 0);
end;

procedure Finalize(var v: TOption); overload;
begin
 v.Long := '';
end;

procedure Initialize(var v: TParsedOption); overload;
begin
 FillChar(v, SizeOf(v), 0);
end;

procedure Finalize(var v: TParsedOption); overload;
var i: Integer;
begin
 v.Argument := '';
 for i := Low(TSeparateArgs) to High(TSeparateArgs) do v.SeparateArgs[i] := '';
end;
{$endif}

{$I dynarray_1.inc}
{$I dynarray_2.inc}

procedure ParseParameters(const Options: array of TOption; OptionProc: TOptionProc;
  OptionProcData: Pointer; ParseOnlyKnownLongOptions: boolean);
begin
 ParseParameters(@Options, High(Options)+1, OptionProc, OptionProcData,
   ParseOnlyKnownLongOptions);
end;

procedure ParseParameters(Options: TDynOptionArray; OptionProc: TOptionProc;
  OptionProcData: Pointer; ParseOnlyKnownLongOptions: boolean);
begin
 ParseParameters(Options.ItemsArray, Options.Count,
   OptionProc, OptionProcData,
   ParseOnlyKnownLongOptions);
end;

procedure SplitLongParameter(const s: string; out ParamLong: string;
  out HasArgument: boolean; out Argument: string; PrefixLength: Integer);
{ zadany s musi sie zaczynac od PrefixLength znakow ktore sa ignorowane
  (dla "prawdziwej" long option z definicji ParseParameters PrefixLength musi byc
  2 i musza one byc rowne '--').
  Rozbija parametr na nazwe parametru (nie zawierajaca znaku '=', rozna od '',
    bedzie wyjatek EInvalidParams w tym rzadkim przypadku gdy
    s[PrefixLength+1] = '=' lub gdy string sie konczy po PrefixLength znakach)
  i Argument, tzn. jezeli s nie zawieral znaku '=' zwraca HasArgument
  =false i Argument = '', wpp. ParamLong to czesc zawarta pomiedzy '--' a '=',
  HasArgument = true, Argument to czesc za pierwszyn znakiem '='
  (w ten sposob sam Argument moze bez problemu zawierac znak '=').
  Przyklady:
    s = '--long-option' ->
      ParamLong = 'long-option', HasArgument = false, Argument = ''
    s = '--long-option=arg' ->
      ParamLong = 'long-option', HasArgument = true, Argument = 'arg'
    s = '--' ->
      EInvalidParams
    s = '--=arg' ->
      EInvalidParams
}
var p: Integer;
begin
 p := Pos('=', s);
 HasArgument := p <> 0;
 if HasArgument then
 begin
  ParamLong := CopyPos(s, PrefixLength+1, p-1);
  Argument := SEnding(s, p+1);
 end else
 begin
  ParamLong := SEnding(s, PrefixLength+1);
  Argument := '';
 end;

 if ParamLong = '' then
  raise EInvalidParams.Create('Invalid empty parameter "'+s+'"');
end;

procedure ParseParameters(Options: POption_Array; OptionsCount: Integer; OptionProc: TOptionProc;
  OptionProcData: Pointer; ParseOnlyKnownLongOptions: boolean);

  function ParseLongParameter(const s: string; out HasArgument: boolean;
    out Argument: string): Integer;
  { s jest jakims parametrem ktory zaczyna sie od '--' i nie jest rowny '--'.
    Wyciaga z s-a opcje jaka reprezentuje (i zwraca jej numer w Params,
    zero-based), wyciaga tez zapisany razem z nia parametr i zwraca
    HasArgument i Argument (pamietaj ze wyciaga tylko argumenty dolaczone
    do opcji przy pomocy znaku "="; nie sprawdza tez w ogole czy HasArgument
    w jakis sposob zgadza sie z Options[result].Argument.).

    Jezeli ParseOnlyKnownLongOptions to moze zwrocic -1 aby zaznaczyc ze
    ten parametr nie reprezentuje zadnej znanej opcji (chociaz ciagle
    nieprawidlowe postacie w rodzaju --=argument czy --non-arg-option=argument
    beda oczywiscie powodowaly wyjatek.) }
  var ParamLong: string;
      i: Integer;
  begin
   SplitLongParameter(s, ParamLong, HasArgument, Argument, 2);
   for i := 0 to OptionsCount-1 do
    if Options^[i].Long = ParamLong then
     begin result := i; Exit; end;

   if ParseOnlyKnownLongOptions then
    result := -1 else
    raise EInvalidLongOption.Create('Invalid long option "'+s+'"');
  end;

  function FindShortOption(c: char; const Parameter: string): Integer;
  { znajdz takie i ze Options[i].Short = c (i c <> #0).
    Jesli sie nie uda - wyjatek EInvalidshortOption.
    Parametr "Parameter" jest nam potrzebny
    _tylko_ zeby skomponowac ladniejszy (wiecej mowiacy) Message wyjatku,
    podany Parameter powinien byc parametrem w ktorym znalezlismy literke c. }
  const
    SInvalidShortOpt = 'Invalid short option character "%s" in parameter "%s"';
  begin
   if c = #0 then
    raise EInvalidShortOption.CreateFmt(SInvalidShortOpt, ['#0 (null char)', Parameter]);

   for result := 0 to OptionsCount-1 do
    if Options^[result].Short = c then Exit;

   raise EInvalidShortOption.CreateFmt(SInvalidShortOpt, [c, Parameter]);
  end;

  function ParseShortParameter(const s: string; var HasArgument: boolean;
    var Argument: string; SimpleShortOptions: TDynIntegerArray): Integer;
  { s jest jakims parametrem zaczynajacym sie od '-' i nie bedacym '-'.
    Dziala tak jak ParseLongParameter tyle ze nigdy nie zwraca -1
    (podany s MUSI zawierac znany parametr).

    Ponadto do SimpleShortOptions dopisze ciag prostych opcji ktore zostaly
    podane razem z ostatnia opcja (czyli z opcja zwracana pod nazwa).
    Te proste opcje zostaly "skombinowane" razem z ostatnia opcja w
    jednym parametrze. W rezultacie nazywam je "prostymi" bo one nie moga
    miec argumentu - Options^[].Argument tych opcji moze byc tylko oaNone
    lub oaOptional. Ta procedura NIE sprawdza ze to sie zgadza
    tak jak w ogole nie sprawdza zadnego Options^[].Argument, takze
    dla ostatniej (zwracanej pod nazwa) opcji nie sprawdza - moze wiec
    zwrocic opcje oaNone z HasArgument albo oaRequired[*Separate] z
    not HasArgument.

    Ten kto uzywa tej opcji musi sprawdzic czy HasArgument ma sens ze
    zwrocona opcja. W przypadku oaRequired[*Separate] moze/musi odczytac
    dalsze parametry zeby postac argument/argumenty opcji.
    Zasada jest taka ze ta procedura zajmuje sie TYLKO parametrem s.
    Ona nie wchodzi na inne Parameters[], zreszta w ogole nie wie dla jakiego
    I zachodzi Parameters[I] = s.
  }
  var ParamShortStr: string;
      i: Integer;
  begin
   { calculate ParamShortStr, HasArgument, Argument }
   SplitLongParameter(s, ParamShortStr, HasArgument, Argument, 1);

   { add to SimpleShortOptions }
   for i := 1 to Length(ParamShortStr)-1 do
    SimpleShortOptions.Add( FindShortOption(ParamShortStr[i], s) );

   { calculate result }
   result := FindShortOption(ParamShortStr[Length(ParamShortStr)], s);
  end;

var i, j, k, OptionNum: Integer;
    HasArgument: boolean;
    Argument, OptionName: string;
    SeparateArgs: TSeparateArgs;
    SimpleShortOptions: TDynIntegerArray;
begin
 i := 1;
 SimpleShortOptions := TDynIntegerArray.Create;
 try

  while i <= Parameters.High do
  begin
   if Parameters[i] = '--' then
   begin
    if not ParseOnlyKnownLongOptions then Parameters.Delete(i, 1);
    Break
   end;

   Assert(SimpleShortOptions.Length = 0);

   { calculate OptionNum; Ustaw je na numer w Params jezeli Parameters[i] to opcja
     (w tym przypadku musisz tez ustalic OptionName), wpp. (jesli to nie opcja
     i mozemy ja pominac) ustal OptionNum na -1.

     Warunek Length(Parameters[i]) > 1 w linijce ponizej gwarantuje nam
     ze parametr '-' uznamy za nie-opcje (zamiast np. powodowac wyjatek
     "empty option") }
   OptionNum := -1;
   if SCharIs(Parameters[i], 1, '-') and (Length(Parameters[i]) > 1) then
   begin
    if SCharIs(Parameters[i], 2, '-') then
    begin
     OptionNum := ParseLongParameter(Parameters[i], HasArgument, Argument);
     if OptionNum <> -1 then OptionName := '--'+Options^[OptionNum].Long;
    end else
    if not ParseOnlyKnownLongOptions then
    begin
     OptionNum := ParseShortParameter(Parameters[i], HasArgument, Argument, SimpleShortOptions);
     OptionName := '-'+Options^[OptionNum].Short;
    end;
   end;

   { OptionNum = -1 oznacza ze z jakiegos powodu Parameters[i] jednak NIE przedstawia
     soba zadnej opcji i powinnismy postepowac dalej jakby Parameters[i] byl
     normalnym parametrem, nie-opcja. W praktyce bylo nam to potrzebne
     bo gdy ParseOnlyKnownLongOptions = true to fakt ze chcemy dany Parameters[i]
     mozemy czasem odkryc dosc pozno, np. bedac w wywolaniu ParseLongParameter. }

   if OptionNum <> -1 then
   begin
    { najpierw zajmij sie SimpleShortOptions }
    for k := 0 to SimpleShortOptions.Length-1 do
    begin
     if not (Options^[SimpleShortOptions[k]].Argument in [oaNone, oaOptional]) then
      raise EMissingOptionArgument.Create('Missing argument for short option -'+
        Options^[SimpleShortOptions[k]].Short +'; when combining short options only the last '+
        'option can have an argument');
     OptionProc(SimpleShortOptions[k], false, '', EmptySeparateArgs, OptionProcData);
    end;
    SimpleShortOptions.Length := 0;

    { teraz zajmij sie opcja OptionNum o nazwie OptionName }

    Parameters.Delete(i, 1);
    SeparateArgs := EmptySeparateArgs;

    { upewnij sie ze HasArgument ma dopuszczalna wartosc. Odczytaj argumenty
      podane jako osobne paranetry dla oaRequired i oaRequired?Separate. }

    if (Options^[OptionNum].Argument = oaRequired) and (not HasArgument) then
    begin
     if i > Parameters.High then
      raise EMissingOptionArgument.Create('Missing argument for option '+OptionName);
     HasArgument := true;
     Argument := Parameters[i];
     Parameters.Delete(i, 1);
    end else
    if (Options^[OptionNum].Argument = oaNone) and HasArgument then
     raise EExcessiveOptionArgument.Create('Excessive argument for option '+OptionName) else
    if Options^[OptionNum].Argument in OptionArgumentsRequiredSeparate then
    begin
     if HasArgument then
      raise EExcessiveOptionArgument.CreateFmt('Option %s requires %d arguments, '+
        'you cannot give them using the form --option=argument, you must give '+
        'all the arguments as separate parameters', [OptionName,
        OptionSeparateArgumentToCount(Options^[OptionNum].Argument) ]);

     for j := 1 to OptionSeparateArgumentToCount(Options^[OptionNum].Argument) do
     begin
      if i > Parameters.High then
       raise EMissingOptionArgument.CreateFmt('Not enough arguments for option %s, '+
         'this option needs %d arguments but we have only %d', [OptionName,
         OptionSeparateArgumentToCount(Options^[OptionNum].Argument), j-1]);
      SeparateArgs[j] := Parameters[i];
      Parameters.Delete(i, 1);
     end;
    end;

    OptionProc(OptionNum, HasArgument, Argument, SeparateArgs, OptionProcData);
   end else
    Inc(i);
  end;

 finally SimpleShortOptions.Free end;
end;

procedure ParseNextParam(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
var ParsedArray: TDynParsedOptionArray absolute Data;
    LastItem: PParsedOption;
begin
 LastItem := ParsedArray.Add;
 LastItem^.OptionNum := OptionNum;
 LastItem^.HasArgument := HasArgument;
 LastItem^.Argument := Argument;
 LastItem^.SeparateArgs := SeparateArgs;
end;

function ParseParameters(
  Options: POption_Array; OptionsCount: Integer;
  ParseOnlyKnownLongOptions: boolean)
  : TDynParsedOptionArray;
begin
 result := TDynParsedOptionArray.Create;
 try
  ParseParameters(Options, OptionsCount,
    {$ifdef FPC_OBJFPC} @ {$endif} ParseNextParam, result,
    ParseOnlyKnownLongOptions);
 except result.Free; raise end;
end;

function ParseParameters(
  const Options: array of TOption; ParseOnlyKnownLongOptions: boolean)
  : TDynParsedOptionArray;
begin
 result := ParseParameters(@Options, High(Options)+1, ParseOnlyKnownLongOptions);
end;

{ some simple helper utilities ---------------------------------------------- }

function OptionSeparateArgumentToCount(const v: TOptionSeparateArgument): Integer;
begin
 result := RequiredSeparateFirstCount + Ord(v) - Ord(oaRequiredSeparateFirst)
end;

function SeparateArgsToVector3Single(const v: TSeparateArgs): TVector3Single;
begin
 result[0] := StrToFloat(v[1]);
 result[1] := StrToFloat(v[2]);
 result[2] := StrToFloat(v[3]);
end;

end.
