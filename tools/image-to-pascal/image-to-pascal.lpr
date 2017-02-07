{
  Copyright 2001-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Convert image files to a Pascal source code, thus allowing
  you to easily embed image files in program executables.

  1st command-line option specifies the unit name, usually in CamelCase.
  Rest of the command-line options specify image URLs (usually filenames).

  The output unit filename will be lowercase(unit name) + '.pas'.
  (We make the name lowercase, as this is nice under Unix.)
  We also output an include file, with .images_data extension, containing
  actual image data (the extension is deliberately weird:
  the contents may be large, and would confuse http://www.ohloh.net/
  into thinking you have a lot of (uncommented) Pascal source code).
  We place it all inside directory given by --output option, by default
  just in the current directory.

  The generated Pascal unit defines TCastleImage instances
  (created / freed in unit's initialization / finalization)
  that contain the size and contents of your images.
  Remember that you can provide many image URLs on the command-line,
  then all of them will be included in the unit.

  For an example output of this program see e.g. view3dscene sources,
  unit v3dsceneimages, generated from images inside view3dscene/images.

  Run with --help for a description of other command-line parameters.
}

{$apptype CONSOLE}

uses SysUtils, CastleImages, CastleUtils, CastleFilesUtils, CastleProgress,
  CastleProgressConsole, CastleParameters, CastleURIUtils,
  CastleClassUtils;

var
  ShowProgress: boolean = true;
  OutputDirectory: string = '';

const
  Options: array[0..2]of TOption = (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: #0 ; Long: 'no-show-progress'; Argument: oaNone),
    (Short: 'o'; Long: 'output'; Argument: oaRequired)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
  case OptionNum of
    0: begin
        Writeln(
          'image-to-pascal: Convert image files into Pascal source code,' +NL+
          'useful together with Castle Game Engine image units.' +NL+
          nl+
          'Call like' +NL+
          '  image-to-pascal [OPTIONS...] UnitName image_name1.png...' +NL+
          nl+
          'Available options are:' +NL+
          HelpOptionHelp +NL+
          '  --no-show-progress    Do not show progress on stderr.' +NL+
          '  -o / --output DIRECTORY' +NL+
          '                        Place output unit files inside this dir.' +NL+
          '  @alpha=strip          Strip the alpha channel from the following' +NL+
          '                        images.' +NL+
          '  @alpha=keep           Keep the alpha channel on the following' +NL+
          '                        images (the default). As a result,' +NL+
          '                        alpha channel will be stored in source files.'
         );
        Halt;
       end;
    1: ShowProgress := false;
    2: OutputDirectory := Argument + '/';
    else raise EInternalError.Create('OptionProc -- unknown arg');
  end;
end;

var
  Image, TempImage: TCastleImage;
  ImageURL: string;
  UnitName, ImageName: string;
  CodeInterface, CodeImplementation, CodeInitialization, CodeFinalization: string;
  ImageIndex: Integer;
  OutputUnit: TTextWriter;
  AlphaStrip: boolean = false;
begin
  { parse params }
  Parameters.Parse(Options, @OptionProc, nil);
  Parameters.CheckHighAtLeast(2);
  UnitName := Parameters[1];
  Parameters.Delete(1);

  { init progres }
  Progress.UserInterface := ProgressConsoleInterface;

  { calculate unit's content from images into Images* strings }
  CodeInterface := '';
  CodeImplementation := '';
  CodeInitialization := '';
  CodeFinalization := '';
  for ImageIndex := 1 to Parameters.High do
  begin
    ImageURL := Parameters[ImageIndex];

    if ImageURL = '@alpha=strip' then
    begin
      AlphaStrip := true;
      Continue;
    end else
    if ImageURL = '@alpha=keep' then
    begin
      AlphaStrip := false;
      Continue;
    end;

    { init other Image* variables }
    ImageName := DeleteURIExt(ExtractURIName(ImageURL));
    ImageName[1] := UpCase(ImageName[1]);
    Image := LoadImage(ImageURL);
    try
      if AlphaStrip and Image.HasAlpha then
      begin
        if Image is TRGBAlphaImage then
        begin
          TempImage := TRGBAlphaImage(Image).ToRGBImage;
          FreeAndNil(Image);
          Image := TempImage;
          TempImage := nil; {< for safety }
        end else
          raise Exception.CreateFmt('Cannot strip alpha channel information from image %s (class %s)',
            [ImageURL, Image.ClassName]);
      end;
      Image.SaveToPascalCode(ImageName, ShowProgress,
        CodeInterface, CodeImplementation, CodeInitialization, CodeFinalization);
    finally FreeAndNil(Image) end;
  end;

  { output full unit contents.
    Beware to not concatenate huge Images* strings in the memory,
    could be a performance / memory problem? Although code above does it anyway? }
  OutputUnit := TTextWriter.Create(OutputDirectory + LowerCase(UnitName) + '.pas');
  OutputUnit.Write(
    '{ -*- buffer-read-only: t -*- }' +NL+
    nl+
    '{ Unit automatically generated by image-to-pascal tool,' +NL+
    '  to embed images in Pascal source code.' +NL+
    '  @exclude (Exclude this unit from PasDoc documentation.) }' +NL+
    'unit '+UnitName+';' +NL+
    nl+
    'interface' +NL+
    nl+
    'uses CastleImages;' +NL+
    nl);
  OutputUnit.Write(CodeInterface);
  OutputUnit.Write(
    'implementation' + nl +
    nl+
    'uses SysUtils;' + nl +
    nl +
    '{ Actual image data is included from another file, with a deliberately' +NL+
    '  non-Pascal file extension ".image_data". This way online code analysis' +NL+
    '  tools will NOT consider this source code as an uncommented Pascal code' +NL+
    '  (which would be unfair --- the image data file is autogenerated' +NL+
    '  and never supposed to be processed by a human). }' +NL+
    '{$I ' + LowerCase(UnitName) + '.image_data}' +NL+
    nl +
    'initialization' +NL+
    CodeInitialization +
    'finalization' +NL+
    CodeFinalization +
    'end.');
  FreeAndNil(OutputUnit);

  OutputUnit := TTextWriter.Create(OutputDirectory + LowerCase(UnitName) + '.image_data');
  OutputUnit.Write(CodeImplementation);
  FreeAndNil(OutputUnit);
end.
