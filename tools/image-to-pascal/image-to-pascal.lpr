{
  Copyright 2001-2023 Michalis Kamburelis.

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
  The images are compressed with simple RLE compression per-channel,
  to not make the EXE size (and compilation speed) too large.

  For an example output of this program see e.g. castle-model-viewer sources,
  unit v3dsceneimages, generated from images inside castle-model-viewer/images.

  Run with --help for a description of other command-line parameters.
}

{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses SysUtils, CastleImages, CastleUtils, CastleFilesUtils,
  CastleParameters, CastleUriUtils, CastleStringUtils,
  CastleClassUtils, CastleDownload, CastleLog;

var
  OutputDirectory: string = '';

const
  Options: array [0..3] of TOption = (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'v'; Long: 'version'; Argument: oaNone),
    (Short: 'o'; Long: 'output'; Argument: oaRequired),
    (Short: 'V'; Long: 'verbose'; Argument: oaNone)
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
           'Usage:' +NL+
           '  image-to-pascal [OPTIONS...] UnitName image_name1.png...' +NL+
           nl+
           'Available options are:' +NL+
           OptionDescription('-h / --help', 'Print this help message and exit.') + NL +
           OptionDescription('-v / --version', 'Print the version number and exit.') + NL +
           OptionDescription('--no-show-progress', 'Do not show progress on stderr.') + NL +
           OptionDescription('-o / --output DIRECTORY', 'Place output unit files inside this dir.') + NL +
           OptionDescription('-V / --verbose', 'Print full log.') + NL +
           OptionDescription('@alpha=keep', 'Keep the alpha channel on the following images (the default). As a result, alpha channel will be stored in source files.') + NL +
           OptionDescription('@alpha=strip', 'Strip the alpha channel from the following images.') + NL +
           OptionDescription('@alpha=keep-and-bleed', 'Like "keep", moreover perform "alpha bleeding" (see https://castle-engine.io/manual_alpha_bleeding.php ) to fix RGB values under transparent pixels.') + NL
         );
         Halt;
       end;
    1: begin
         // include ApplicationName in version, good for help2man
         Writeln(ApplicationName + ' ' + CastleEngineVersion);
         Halt;
       end;
    2: OutputDirectory := Argument + '/';
    3: InitializeLog;
    else raise EInternalError.Create('OptionProc -- unknown arg');
  end;
end;

function PascalNameFromUrl(const Url: String): String;
const
  ValidChars = ['0'..'9', 'a'..'z', 'A'..'Z', '_'];
  ValidFirstChars = ['a'..'z', 'A'..'Z', '_'];
begin
  Result := DeleteURIExt(ExtractURIName(Url));
  // replace chars not valid in the middle of Pascal identifier
  Result := SReplaceChars(Result, AllChars - ValidChars, '_');
  // replace chars not valid as 1st char of Pascal identifier
  if not (Result[1] in ValidFirstChars) then
    Result[1] := '_';
  Result[1] := UpCase(Result[1]);
end;

type
  TAlpha = (alphaKeep, alphaStrip, alphaKeepAndBleed);

var
  Image, TempImage: TCastleImage;
  ImageUrl: String;
  UnitName, ImageName: string;
  CodeInterface, CodeImplementation, CodeInitialization, CodeFinalization: string;
  ImageIndex: Integer;
  OutputUnit: TTextWriter;
  Alpha: TAlpha = alphaKeep;
begin
  { parse params }
  Parameters.Parse(Options, @OptionProc, nil);
  Parameters.CheckHighAtLeast(2);
  UnitName := Parameters[1];
  Parameters.Delete(1);

  { calculate unit's content from images into Images* strings }
  CodeInterface := '';
  CodeImplementation := '';
  CodeInitialization := '';
  CodeFinalization := '';
  for ImageIndex := 1 to Parameters.High do
  begin
    ImageUrl := Parameters[ImageIndex];

    if ImageUrl = '@alpha=strip' then
    begin
      Alpha := alphaStrip;
      Continue;
    end else
    if ImageUrl = '@alpha=keep' then
    begin
      Alpha := alphaKeep;
      Continue;
    end else
    if ImageUrl = '@alpha=keep-and-bleed' then
    begin
      Alpha := alphaKeepAndBleed;
      Continue;
    end;

    { init other Image* variables }
    ImageName := PascalNameFromUrl(ImageUrl);
    Image := LoadImage(ImageUrl);
    try
      if Image.HasAlpha then
      begin
        case Alpha of
          alphaStrip:
            begin
              Writeln(ErrOutput, 'Stripping alpha from ', ImageUrl);
              TempImage := TRGBImage.Create;
              TempImage.Assign(Image);
              FreeAndNil(Image);
              Image := TempImage;
              TempImage := nil; {< for safety }
            end;
          alphaKeepAndBleed:
            begin
              Writeln(ErrOutput, 'Making alpha bleeding on ', ImageUrl);
              // convert to TRGBAlphaImage as alpha bleeding is implemented only there
              TempImage := TRGBAlphaImage.Create;
              TempImage.Assign(Image);
              TempImage.AlphaBleed;
              FreeAndNil(Image);
              Image := TempImage;
              TempImage := nil; {< for safety }
            end;
          else { do nothing for alphaKeep };
        end;
      end;

      Writeln(ErrOutput, Format('Generating %s (%s, alpha: %s)', [
        ImageName,
        Image.ClassName,
        AlphaToString[Image.AlphaChannel]
      ]));
      Image.SaveToPascalCode(ImageName,
        CodeInterface, CodeImplementation, CodeInitialization, CodeFinalization);
    finally FreeAndNil(Image) end;
  end;

  { output full unit contents }
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
    'uses SysUtils, CastleInternalDataCompression;' + nl +
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
