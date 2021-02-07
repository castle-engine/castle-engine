{ -*- compile-command: "./x3d-nodes-to-pascal_compile.sh && ./x3d-nodes-to-pascal_run.sh" -*- }
{
  Copyright 2015-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Generate Pascal code from X3D nodes specification. }

uses SysUtils, CastleParameters, CastleClassUtils, CastleStringUtils,
  CastleTimeUtils, CastleLog, CastleColors, CastleUtils,
  CastleApplicationProperties,
  GenerateProcessors;

type
  TOutputMode = (omHelper, omTemplate);

var
  OutputMode: TOutputMode;

const
  Options: array [0..4] of TOption =
  (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'v'; Long: 'version'; Argument: oaNone),
    (Short: #0 ; Long: 'output'; Argument: oaRequired),
    (Short: #0 ; Long: 'verbose'; Argument: oaNone),
    (Short: #0 ; Long: 'output-path'; Argument: oaRequired)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: Boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
  case OptionNum of
    0: begin
         Writeln(
           'x3d-nodes-to-pascal: Generate Pascal code' +NL+
           'from X3D nodes specification, for Castle Game Engine.' +NL+
           NL+
           'Usage:' +NL+
           '  x3d-nodes-to-pascal [options...] [nodes_description.txt...]' +NL+
           NL+
           'Available options:' +NL+
           '  -h / --help           Print this help message and exit' +NL+
           '  -v / --version        Print the version and exit' +NL+
           '  --verbose             Be verbose' +NL+
           '  --output=helper|template  Choose the output type' +NL+
           NL+
           SCastleEngineProgramHelpSuffix('x3d-nodes-to-pascal', CastleEngineVersion, true));
         Halt;
       end;
    1: begin
         // include ApplicationName in version, good for help2man
         Writeln(ApplicationName + ' ' + CastleEngineVersion);
         Halt;
       end;
    2: begin
         if Argument = 'helper' then
           OutputMode := omHelper
         else
         if Argument = 'template' then
           OutputMode := omTemplate
         else
           raise EInvalidParams.Create('Invalid argument for --output');
       end;
    3: Verbose := true;
    4: OutputPath := InclPathDelim(Argument);
    else raise EInternalError.Create('OptionProc');
  end;
end;

var
  I: Integer;
  Processor: TProcessor;
begin
  ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);

  Parameters.Parse(Options, @OptionProc, nil);
  Parameters.CheckHighAtLeast(1);

  case OutputMode of
    omHelper  : Processor := THelperProcessor.Create;
    // no other possibility right now
    else raise EInternalError.Create('OutputMode?');
  end;

  for I := 1 to Parameters.High do
    Processor.ProcessFile(Parameters[I]);
end.
