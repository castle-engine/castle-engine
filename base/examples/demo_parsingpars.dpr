{ Simple example how to use ParsingPars unit.
  Call with --help to get help. }

uses SysUtils, KambiUtils, ParsingPars;

var Number: Integer = 0;

const
  Options: array[0..1]of TOption = (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'n'; Long: 'number'; Argument: oaRequired)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
 case OptionNum of
  0: begin
      Writeln(
        'demo_parsingpars: Demo how to use ParsingPars unit.' +nl+
        nl+
        'Available options are:' +nl+
        HelpOptionHelp +nl+
        '  -n / --number NUMBER' +nl+
        '                        Set''s some Number to given value.' +nl+
        nl+
        'Example calls : ' +nl+
        nl+
        'Following 4 calls are equivalent:' +nl+
        '  demo_parsingpars -n 12' +nl+
        '  demo_parsingpars -n=12' +nl+
        '  demo_parsingpars --number=12' +nl+
        '  demo_parsingpars --number 12' +nl+
        nl+
        'Following 2 calls are equivalent:' +nl+
        '  demo_parsingpars -n 12 foo' +nl+
        '  demo_parsingpars foo --number=12 ' +nl+
        nl+
        'Example incorrect calls that will result in exceptions' +nl+
        'with proper error messages:' +nl+
        '  demo_parsingpars --something' +nl+
        '  demo_parsingpars -s' +nl+
        nl+
        'Following call demonstrates how to pass something starting with "-"' +nl+
        'as parameter (usefull to e.g. pass filenames starting with "-"))' +nl+
        '  demo_parsingpars -- -some-file-name'
       );
      ProgramBreak;
     end;
  1: Number := StrToInt(Argument);
  else raise EInternalError.Create('OptionProc -- unknown arg');
 end;
end;

var i: Integer;
begin
 { Do parsing parameters }

 ParsePars(Options, OptionProc, nil);

 { Report state of ParStr(1) .. ParStr(ParCount) and Number after
   parsing parameters. }

 Writeln(Format('After ParsePars, %d non-options are left.', [ParCount]));
 for i := 1 to ParCount do
  Writeln('ParStr(', i, ') = ', ParStr(i));

 Writeln('Number is now ', Number);
end.