// -*- compile-command: "castle-engine compile && castle-engine run" -*-
{
  Copyright 2015-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Generate Pascal code from X3D nodes specification.
  Writes Pascal include files with auto-generated pieces of Pascal code
  that define X3D nodes.
  This allows to define all X3D fiels in a consistent way,
  and easier change it in the future.
}

uses SysUtils, CastleParameters, CastleClassUtils, CastleStringUtils,
  CastleTimeUtils, CastleLog, CastleColors, CastleUtils,
  CastleApplicationProperties, CastleFindFiles, CastleFilesUtils,
  GenerateProcessors;

var
  { When are input files (*.txt) located.
    May but doesn't have to end with PathDelim. }
  InputPath: String = './nodes-specification/';

const
  Options: array [0..4] of TOption =
  (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'v'; Long: 'version'; Argument: oaNone),
    (Short: #0 ; Long: 'verbose'; Argument: oaNone),
    (Short: #0 ; Long: 'input-path'; Argument: oaRequired),
    (Short: #0 ; Long: 'output-path'; Argument: oaRequired)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
  case OptionNum of
    0: begin
         Writeln(
           'x3d-nodes-to-pascal: Generate Pascal code' +NL+
           'from X3D nodes specification, for Castle Game Engine.' +NL+
           NL+
           'Usage:' +NL+
           '  x3d-nodes-to-pascal [options...]' +NL+
           NL+
           'Available options:' +NL+
           '  -h / --help           Print this help message and exit' +NL+
           '  -v / --version        Print the version and exit' +NL+
           '  --verbose             Be verbose' +NL+
           '  --input-path PATH    Directory with txt files that describe nodes.' +NL+
           '  --output-path PATH    Directory with generated (Pascal include) files.' +NL+
           NL+
           ApplicationProperties.Description);
         Halt;
       end;
    1: begin
         // include ApplicationName in version, good for help2man
         Writeln(ApplicationName + ' ' + CastleEngineVersion);
         Halt;
       end;
    2: Verbose := true;
    3: InputPath := Argument;
    4: OutputPath := Argument;
    else raise EInternalError.Create('OptionProc');
  end;
end;

procedure RemoveFilesMatching(const Path, Mask: String);
var
  FilesToRemove: TFileInfoList;
  FileToRemove: TFileInfo;
begin
  Writeln('Removing files matching "' + Mask + '" in "' + Path + '"');
  FilesToRemove := FindFilesList(Path, Mask, false, []);
  try
    for FileToRemove in FilesToRemove do
      CheckDeleteFile(FileToRemove.AbsoluteName, true);
  finally FreeAndNil(FilesToRemove) end;
end;

var
  Processor: TProcessor;
  InputFiles: TFileInfoList;
  InputFile: TFileInfo;
begin
  ApplicationProperties.ApplicationName := 'x3d-nodes-to-pascal';
  ApplicationProperties.Version := '1.0';
  ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);

  if not DirectoryExists(OutputPath) then
    raise Exception.CreateFmt('Output path "%s" does not exist', [OutputPath]);
  if not DirectoryExists(InputPath) then
    raise Exception.CreateFmt('Output path "%s" does not exist', [InputPath]);

  RemoveFilesMatching(OutputPath, 'x3dnodes_*.inc');

  Parameters.Parse(Options, @OptionProc, nil);
  Parameters.CheckHigh(0);

  try
    Processor := THelperProcessor.Create;

    InputFiles := FindFilesList(InputPath, '*.txt', false, []);

    if InputFiles.Count = 0 then
      raise Exception.CreateFmt('No input files found in "%s"', [InputPath]);

    for InputFile in InputFiles do
    begin
      Writeln('Processing "' + InputFile.Name + '"');
      Processor.ProcessFile(InputFile.AbsoluteName);
    end;
  finally
    { Knowing that these instances are initially nil, we can write simple
      finally clause that deals with all of them,
      instead of nesting multiple try..finally clauses. }
    FreeAndNil(InputFiles);
    FreeAndNil(Processor);
  end;
end.
