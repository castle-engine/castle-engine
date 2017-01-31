{
  Copyright 2004-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestCastleParameters;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, CastleParameters,
  CastleGenericLists;

type
  TParsedOption = record
    OptionNum: Integer;
    HasArgument: boolean;
    Argument: string;
    SeparateArgs: TSeparateArgs;
  end;
  PParsedOption = ^TParsedOption;
  TParsedOptionList = specialize TGenericStructList<TParsedOption>;

  TTestParsingParameters = class(TTestCase)
  private
    procedure AssertParsEqual(const ParsValues: array of string);
    procedure AssertParsedParsEqual(const ParsedPars1: TParsedOptionList;
      const ParsedPars2: array of TParsedOption);
  published
    procedure TestParsingParameters;
  end;

implementation

uses CastleUtils, CastleStringUtils;

procedure ParseNextParam(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
var
  ParsedArray: TParsedOptionList absolute Data;
  LastItem: PParsedOption;
begin
 LastItem := ParsedArray.Add;
 LastItem^.OptionNum := OptionNum;
 LastItem^.HasArgument := HasArgument;
 LastItem^.Argument := Argument;
 LastItem^.SeparateArgs := SeparateArgs;
end;

{ Parse command-line parameters returning a list of parsed options.
  Works exactly like previous TParameters.Parse procedure,
  but instead of using a callback like OptionProc, this time
  it returns a list.
  @groupBegin }
function ParseParameters(
  Options: POption_Array; OptionsCount: Integer;
  ParseOnlyKnownLongOptions: boolean = false)
  : TParsedOptionList;
begin
 result := TParsedOptionList.Create;
 try
  Parameters.Parse(Options, OptionsCount,
    {$ifdef FPC_OBJFPC} @ {$endif} ParseNextParam, result,
    ParseOnlyKnownLongOptions);
 except result.Free; raise end;
end;

function ParseParameters(
  const Options: array of TOption; ParseOnlyKnownLongOptions: boolean = false)
  : TParsedOptionList;
begin
 result := ParseParameters(@Options, High(Options)+1, ParseOnlyKnownLongOptions);
end;

procedure TTestParsingParameters.AssertParsEqual(const ParsValues: array of string);
var i: Integer;
begin
 AssertTrue(Parameters.High = High(ParsValues));
 for i := 0 to Parameters.High do AssertTrue(Parameters[i] = ParsValues[i]);
end;

procedure TTestParsingParameters.AssertParsedParsEqual(const ParsedPars1: TParsedOptionList;
  const ParsedPars2: array of TParsedOption);
var i, j: Integer;
begin
 AssertTrue(ParsedPars1.Count - 1 = High(ParsedPars2));
 for i := 0 to ParsedPars1.Count - 1 do
 begin
  AssertTrue(ParsedPars1.L[i].OptionNum   = ParsedPars2[i].OptionNum);
  AssertTrue(ParsedPars1.L[i].HasArgument = ParsedPars2[i].HasArgument);
  AssertTrue(ParsedPars1.L[i].Argument    = ParsedPars2[i].Argument);
  for j := Low(TSeparateArgs) to High(TSeparateArgs) do
   AssertTrue(ParsedPars1.L[i].SeparateArgs[j] = ParsedPars2[i].SeparateArgs[j]);
 end;
end;

procedure TTestParsingParameters.TestParsingParameters;

  function DynParsedOptionArrayToStr(const name: string;
    v: TParsedOptionList): string;
  var i: Integer;
  begin
   result := name + nl;
   for i := 0 to v.Count - 1 do
    result += Format('  [%d] OptionNum %d, HasArg %s, Argument "%s"',
      [ i,
        v.L[i].OptionNum,
        BoolToStr(v.L[i].HasArgument, true),
        v.L[i].Argument]) + nl;
  end;

  function ParsToStr: string;
  var i: Integer;
  begin
   result := 'Params now = ' + nl;
   for i := 0 to Parameters.High do
    result += Format('  ParStr(%d) = "%s"', [i, Parameters[i]]) + nl;
  end;

  procedure CheckPars(TestName: string; const StartPars: array of string;
    const Options: array of TOption;
    const GoodAnswer: array of TParsedOption; const GoodRest: array of string;
    ParseOnlyKnownLongOptions: boolean);
  var Answer: TParsedOptionList;
  begin
   Parameters.AssignArray(StartPars);
   AssertParsEqual(StartPars);

   try
    Answer := ParseParameters(Options, ParseOnlyKnownLongOptions);
   except
    Writeln('failed na tescie ',TestName);
    raise;
   end;

   try
    try
     AssertParsedParsEqual(Answer, GoodAnswer);
     AssertParsEqual(GoodRest);
    except
     Writeln('failed na Assertach w tescie ',TestName);
     Write(DynParsedOptionArrayToStr('Answer', Answer));
     Write(ParsToStr);
     raise;
    end;
   finally Answer.Free end;
  end;

  procedure CheckParsFail(TestName: string; const StartPars: array of string;
    const Options: array of TOption; EClass: ExceptClass; const EMessage: string);
  var Answer: TParsedOptionList;
  begin
   Parameters.AssignArray(StartPars);
   AssertParsEqual(StartPars);

   try
    Answer := ParseParameters(Options);
    try
     Writeln('CheckParsFail '+ TestName + ' przeszedl mimo ze nie powinien');
     Write(DynParsedOptionArrayToStr('Answer', Answer));
     Write(ParsToStr);
     raise Exception.Create('Test fail succeded... er, I mean, failed');
    finally Answer.Free end;
   except
    on E: Exception do
    begin
     { kazde inne niz EClass exception przepuszczamy, niech wywali test }
     if not (E is EClass) then
      raise else
     begin
      if EMessage = '' then
       Write('failtest ok:',nl, E.Message) else
      begin
       if EMessage <> E.Message then
	raise Exception.Create('no i failed : powinno byc "'+EMessage+
	  '", jest "'+E.Message+'"');
      end;
     end;
    end;
   end;

  end;

const

  {$define EmptySeparateArgs:=('','','', '','','', '','','')}

  { Pars1_2 ------------------------------------------------------------ }

  Pars1_2Question: array[0..4]of string =
  ('--zero-niewazne', 'ala', '--mama', 'teresa', '--kot-kocur=cygan');

  Pars1: array[0..3]of TOption = (
    (Short:'m'; Long:'mama'; Argument: oaOptional),
    (Short:#0; Long:'kot'; Argument: oaOptional),
    (Short:#0; Long:'kot-kocur'; Argument: oaOptional),
    (Short:'a'; Long:'ala'; Argument: oaNone)
  );
  Pars1Answer: array[0..1]of TParsedOption = (
    (OptionNum:0; HasArgument: false; Argument:''; SeparateArgs: EmptySeparateArgs),
    (OptionNum:2; HasArgument: true; Argument:'cygan'; SeparateArgs: EmptySeparateArgs)
  );
  Pars1Rest: array[0..2]of string = ('--zero-niewazne', 'ala', 'teresa');

  Pars2: array[0..2]of TOption = (
    (Short:'m'; Long:'mama'; Argument: oaRequired),
    (Short:#0; Long:'kot-kocur'; Argument: oaOptional),
    (Short:'a'; Long:'ala'; Argument: oaNone)
  );
  Pars2Answer: array[0..1]of TParsedOption = (
    (OptionNum:0; HasArgument: true; Argument:'teresa'; SeparateArgs: EmptySeparateArgs),
    (OptionNum:1; HasArgument: true; Argument:'cygan'; SeparateArgs: EmptySeparateArgs)
  );
  Pars2Rest: array[0..1]of string = ('--zero-niewazne', 'ala');

  { Pars3 ------------------------------------------------------------ }

  Pars3Question: array[0..8]of string =
  ('-l=no', 'ala', '-l', '-l=foo nie=l', '--mama', '--', 'teresa', '--kot=cygan', '--');

  Pars3: array[0..3]of TOption = (
    (Short:'m'; Long:'mama'; Argument: oaOptional),
    (Short:#0; Long:'kot'; Argument: oaOptional),
    (Short:'a'; Long:'ala'; Argument: oaNone),
    (Short:'l'; Long:'lollobrygida'; Argument: oaOptional)
  );
  Pars3Answer: array[0..2]of TParsedOption = (
    (OptionNum:3; HasArgument: false; Argument:''; SeparateArgs: EmptySeparateArgs),
    (OptionNum:3; HasArgument: true; Argument:'foo nie=l'; SeparateArgs: EmptySeparateArgs),
    (OptionNum:0; HasArgument: false; Argument:''; SeparateArgs: EmptySeparateArgs)
  );
  Pars3Rest: array[0..4]of string = ('-l=no', 'ala', 'teresa', '--kot=cygan', '--');

  { Pars4_5 ------------------------------------------------------------ }

  Pars4_5Question: array[0..2]of string = ('-nic', '--ala spacja', '--kot');
  Pars45Rest: array[0..0]of string = ('-nic');

  Pars4: array[0..2]of TOption = (
    (Short:'m'; Long:'mama'; Argument: oaOptional),
    (Short:#0; Long:'kot'; Argument: oaOptional),
    (Short:'a'; Long:'ala spacja'; Argument: oaNone)
  );
  Pars4Answer: array[0..1]of TParsedOption = (
    (OptionNum:2; HasArgument: false; Argument:''; SeparateArgs: EmptySeparateArgs),
    (OptionNum:1; HasArgument: false; Argument:''; SeparateArgs: EmptySeparateArgs)
  );

  Pars5: array[0..2]of TOption = (
    (Short:'m'; Long:'mama'; Argument: oaOptional),
    (Short:#0; Long:'kot'; Argument: oaOptional),
    (Short:'a'; Long:'ala spacja'; Argument: oaRequired)
  );
  Pars5Answer: array[0..0]of TParsedOption = (
    (OptionNum:2; HasArgument: true; Argument:'--kot'; SeparateArgs: EmptySeparateArgs)
  );

  { Pars6 ------------------------------------------------------------ }

  Pars6Question: array[0..3]of string = ('--zero-niewazne', '--ala spacja', '-m', '--kot');
  Pars6: array[0..1]of TOption = (
    (Short:'m'; Long:'mama'; Argument: oaOptional),
    (Short:'a'; Long:'ala spacja'; Argument: oaNone)
  );

  { '--kot' powinien zostac zignorowany jesli uruchomimy z ParseOnlyLongKnownOptions.
    Podobnie '-m' (bo mimo ze -m jest znane to jest short option).
    Jednak '--ala spacja' powinno zostac sparsowane i usuniete. }
  Pars6_OnlyKnown_Answer: array[0..0]of TParsedOption = (
    (OptionNum:1; HasArgument: false; Argument:''; SeparateArgs: EmptySeparateArgs)
  );
  Pars6_OnlyKnown_Rest: array[0..2]of string =
  ('--zero-niewazne', '-m', '--kot');

  { Pars7 ------------------------------------------------------------ }

  Pars7Question: array[0..2]of string = ('zero', '-a', '-k');
  Pars7: array[0..1]of TOption = (
    (Short:'m'; Long:'mama'; Argument: oaOptional),
    (Short:'a'; Long:'ala spacja'; Argument: oaNone)
  );

  { Pars8 ------------------------------------------------------------ }

  Pars8Question: array[0..2]of string = ('hehe', '--=ala spacja', '--kot');
  Pars8: array[0..1]of TOption = (
    (Short:'m'; Long:'mama'; Argument: oaOptional),
    (Short:'a'; Long:'ala spacja'; Argument: oaRequired)
  );

  { Pars9 ------------------------------------------------------------ }

  Pars9Question: array[0..2]of string = ('foo', 'blah', '--kot');
  Pars9: array[0..1]of TOption = (
    (Short:'m'; Long:'mama'; Argument: oaOptional),
    (Short:'k'; Long:'kot'; Argument: oaRequired)
  );

  { Pars10 ------------------------------------------------------------ }

  Pars10Question: array[0..0]of string = ('--kot=blah');
  Pars10: array[0..1]of TOption = (
    (Short:'m'; Long:'mama'; Argument: oaOptional),
    (Short:'k'; Long:'kot'; Argument: oaNone)
  );
  {$define Pars10Answer:=[]}
  Pars10Rest: array[0..0]of string = ('--kot=blah');

  { Pars11 ------------------------------------------------------------ }

  Pars11Question: array[0..1]of string = ('ble', '--kot=blah');
  Pars11: array[0..1]of TOption = (
    (Short:'m'; Long:'mama'; Argument: oaOptional),
    (Short:'k'; Long:'kot'; Argument: oaNone)
  );

  { Pars12 ------------------------------------------------------------ }

  Pars12Question: array[0..1]of string = ('ble', '-=blah');
  Pars12: array[0..1]of TOption = (
    (Short:'m'; Long:'mama'; Argument: oaOptional),
    (Short:'k'; Long:'kot'; Argument: oaNone)
  );

  { Pars13 ------------------------------------------------------------ }

  Pars13Question: array[0..4]of string = ('--ble', '--ble', '1 ', '2', '3');
  Pars13: array[0..2]of TOption = (
    (Short:'m'; Long:'mama'; Argument: oaOptional),
    (Short:'k'; Long:'kot'; Argument: oaNone),
    (Short:'b'; Long:'ble'; Argument: oaRequired2Separate)
  );
  Pars13Answer: array[0..0]of TParsedOption = (
    (OptionNum:2; HasArgument: false; Argument:''; SeparateArgs:('1 ','2','', '','','', '','',''))
  );
  Pars13Rest: array[0..1]of string = ('--ble', '3');

  { Pars14 ------------------------------------------------------------ }

  Pars14Question: array[0..4]of string = ('--ble', '--ble', '1 ', '2', '3');
  Pars14: array[0..2]of TOption = (
    (Short:'m'; Long:'mama'; Argument: oaOptional),
    (Short:'k'; Long:'kot'; Argument: oaNone),
    (Short:'b'; Long:'ble'; Argument: oaRequired3Separate)
  );
  Pars14Answer: array[0..0]of TParsedOption = (
    (OptionNum:2; HasArgument: false; Argument:''; SeparateArgs:('1 ', '2', '3', '','','', '','',''))
  );
  Pars14Rest: array[0..0]of string = ('--ble');

  { Pars15 ------------------------------------------------------------ }

  Pars15Question: array[0..4]of string = ('--ble', '--ble', '1 ', '2', '3');
  Pars15: array[0..2]of TOption = (
    (Short:'m'; Long:'mama'; Argument: oaOptional),
    (Short:'k'; Long:'kot'; Argument: oaNone),
    (Short:'b'; Long:'ble'; Argument: oaRequired4Separate)
  );

  { Pars16 ------------------------------------------------------------ }

  { '-' is not errorneous "empty short option", it is treated as usual
    non-option parameter. '' is harmless too. }
  Pars16Question: array[0..2]of string = ('ble', '-', '');
  Pars16: array[0..0]of TOption = (
    (Short:'b'; Long:'ble'; Argument: oaRequired3Separate)
  );
  {$define Pars16Answer:=[]}
  Pars16Rest: array[0..2]of string = ('ble', '-', '');

  { Pars17 ------------------------------------------------------------ }

  { #0 is not allowed as short option (Short = #0 means "no short form for this
    option exists") }
  Pars17Question: array[0..1]of string = ('--ble', '-'#0);
  Pars17: array[0..0]of TOption = (
    (Short:#0; Long:'mama'; Argument: oaOptional)
  );

  { Pars18 ------------------------------------------------------------- }

  { combining short options works. Arguments for last option are allowed. }
  Pars18Question: array[0..4]of string = ('-zero', '-abc', '--rere', 'blabla', '-abc=foo');
  Pars18: array[0..2]of TOption = (
    (Short:'a'; Long:'ania'; Argument: oaOptional),
    (Short:'b'; Long:'basia'; Argument: oaOptional),
    (Short:'c'; Long:'cycek'; Argument: oaRequired)
  );
  Pars18Answer: array[0..5]of TParsedOption = (
    (OptionNum:0; HasArgument: false; Argument:''; SeparateArgs: EmptySeparateArgs),
    (OptionNum:1; HasArgument: false; Argument:''; SeparateArgs: EmptySeparateArgs),
    (OptionNum:2; HasArgument: true; Argument:'--rere'; SeparateArgs: EmptySeparateArgs),
    (OptionNum:0; HasArgument: false; Argument:''; SeparateArgs: EmptySeparateArgs),
    (OptionNum:1; HasArgument: false; Argument:''; SeparateArgs: EmptySeparateArgs),
    (OptionNum:2; HasArgument: true; Argument:'foo'; SeparateArgs: EmptySeparateArgs)
  );
  Pars18Rest: array[0..1]of string = ('-zero', 'blabla');

  { Pars19 -------------------------------------------------------------- }

  { specyfying argument for paReuired?Separate is not allowed }
  Pars19Question: array[0..3]of string = ('', '--baba=kobita', '1', '2');
  Pars19: array[0..0]of TOption = (
    (Short:'a'; Long:'baba'; Argument: oaRequired2Separate)
  );

  { Pars20 ----------------------------------------------------------------- }

  { OnlyKnown nie rusza krotkich opcji, rusza tylko znane dlugie,
    nieznane dlugie zostawia, honoruje tez -- i za nim nie rusza
    nawet znanych dlugich, ale samego -- nie usuwa. }
  Pars20Question: array[0..7]of string = ('--bar', '-k', '--ble=xyz', '--1', '2', '--foo', '--', '--1');
  Pars20: array[0..2]of TOption = (
    (Short:'m'; Long:'mama'; Argument: oaOptional),
    (Short:'k'; Long:'1'; Argument: oaRequired),
    (Short:'b'; Long:'ble'; Argument: oaRequired)
  );
  Pars20_OnlyKnown_Answer: array[0..1]of TParsedOption = (
    (OptionNum:2; HasArgument: true; Argument:'xyz'; SeparateArgs: EmptySeparateArgs),
    (OptionNum:1; HasArgument: true; Argument:'2'; SeparateArgs: EmptySeparateArgs)
  );
  Pars20_OnlyKnown_Rest: array[0..4]of string = ('--bar', '-k', '--foo', '--', '--1');

  { Pars21 ----------------------------------------------------------------- }

  Pars21Question: array[0..2]of string =
  ('glplotter.exe', '--grid-custom', '12');
  Pars21: array[0..3] of TOption = (
    (Short:'h'; Long:'help'; Argument: oaNone),
    (Short:#0; Long:'grid-custom'; Argument: oaRequired),
    (Short:#0; Long:'light'; Argument: oaNone),
    (Short:#0; Long:'dark'; Argument: oaNone)
  );
  Pars21Answer: array[0..0]of TParsedOption = (
    (OptionNum:1; HasArgument: true; Argument:'12'; SeparateArgs: EmptySeparateArgs)
  );
  Pars21Rest: array[0..0]of string = ('glplotter.exe');

begin
 CheckPars('1', Pars1_2Question, Pars1, Pars1Answer, Pars1Rest, false);
 CheckPars('2', Pars1_2Question, Pars2, Pars2Answer, Pars2Rest, false);
 CheckPars('3', Pars3Question, Pars3, Pars3Answer, Pars3Rest, false);
 CheckPars('4', Pars4_5Question, Pars4, Pars4Answer, Pars45Rest, false);
 CheckPars('5', Pars4_5Question, Pars5, Pars5Answer, Pars45Rest, false);

 CheckParsFail('6', Pars6Question, Pars6, EInvalidLongOption, 'Invalid long option "--kot"');
 CheckPars('6_OnlyKnown', Pars6Question, Pars6, Pars6_OnlyKnown_Answer, Pars6_OnlyKnown_Rest, true);
 CheckParsFail('7', Pars7Question, Pars7, EInvalidShortOption, 'Invalid short option character "k" in parameter "-k"');
 CheckParsFail('8', Pars8Question, Pars8, EInvalidParams, 'Invalid empty parameter "--=ala spacja"');
 CheckParsFail('9', Pars9Question, Pars9, EMissingOptionArgument, 'Missing argument for option --kot');
 CheckPars('10', Pars10Question, Pars10, Pars10Answer, Pars10Rest, false);
 CheckParsFail('11', Pars11Question, Pars11, EExcessiveOptionArgument, 'Excessive argument for option --kot');
 CheckParsFail('12', Pars12Question, Pars12, EInvalidParams, 'Invalid empty parameter "-=blah"');

 CheckPars('13', Pars13Question, Pars13, Pars13Answer, Pars13Rest, false);
 CheckPars('14', Pars14Question, Pars14, Pars14Answer, Pars14Rest, false);
 CheckParsFail('15', Pars15Question, Pars15, EMissingOptionArgument, 'Not enough arguments for option --ble, this option needs 4 arguments but we have only 3');
 CheckPars('16', Pars16Question, Pars16, Pars16Answer, Pars16Rest, false);
 CheckParsFail('17', Pars17Question, Pars17, EInvalidShortOption, 'Invalid short option character "#0 (null char)" in parameter "-'#0'"');
 CheckPars('18', Pars18Question, Pars18, Pars18Answer, Pars18Rest, false);
 CheckParsFail('19', Pars19Question, Pars19, EExcessiveOptionArgument, 'Option --baba requires 2 arguments, you cannot give them using the form --option=argument, you must give all the arguments as separate parameters');
 CheckPars('20_OnlyKnown', Pars20Question, Pars20, Pars20_OnlyKnown_Answer, Pars20_OnlyKnown_Rest, true);
 CheckPars('21', Pars21Question, Pars21, Pars21Answer, Pars21Rest, false);
end;

initialization
 RegisterTest(TTestParsingParameters);
end.
