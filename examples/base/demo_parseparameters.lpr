{ Simple example how to parse command-line parameters using KambiParameters.
  Call with --help to get help. }
program demo_parseparameters;

uses SysUtils, KambiUtils, KambiParameters;

var
  Number: Integer = 0;

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
          'demo_parsingparameters: Demo how to use the KambiParameters.' +nl+
          nl+
          'Available options are:' +nl+
          HelpOptionHelp +nl+
          '  -n / --number NUMBER' +nl+
          '                        Set''s some Number to given value.' +nl+
          nl+
          'Example calls : ' +nl+
          nl+
          'Following 4 calls are equivalent:' +nl+
          '  demo_parseparameters -n 12' +nl+
          '  demo_parseparameters -n=12' +nl+
          '  demo_parseparameters --number=12' +nl+
          '  demo_parseparameters --number 12' +nl+
          nl+
          'Following 2 calls are equivalent:' +nl+
          '  demo_parseparameters -n 12 foo' +nl+
          '  demo_parseparameters foo --number=12 ' +nl+
          nl+
          'Example incorrect calls that will result in exceptions' +nl+
          'with proper error messages:' +nl+
          '  demo_parseparameters --something' +nl+
          '  demo_parseparameters -s' +nl+
          nl+
          'Following call demonstrates how to pass something starting with "-"' +nl+
          'as parameter (usefull to e.g. pass filenames starting with "-"))' +nl+
          '  demo_parseparameters -- -some-file-name'
         );
        ProgramBreak;
       end;
    1: Number := StrToInt(Argument);
    else raise EInternalError.Create('OptionProc -- unknown arg');
  end;
end;

var
  i: Integer;
begin
  { Do parsing parameters }

  Parameters.Parse(Options, @OptionProc, nil);

  { Report state of Parameters[1] .. Parameters[Parameters.High] and Number after
    parsing parameters. }

  Writeln(Format('After Parameters.Parse, %d non-options are left.',
    [Parameters.High]));
  for i := 1 to Parameters.High do
    Writeln('Parameters[', i, '] = ', Parameters[i]);

  Writeln('Number is now ', Number);
end.