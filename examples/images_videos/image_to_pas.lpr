{
  Copyright 2001-2012 Michalis Kamburelis.

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
  Rest of the command-line options specify image filenames.

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
  Remember that you can provide many image filenames on the command-line,
  then all of them will be included in the unit.

  For an example output of this program see e.g. view3dscene sources,
  unit v3dsceneimages, generated from images inside view3dscene/images.

  Run with --help for a description of other command-line parameters.
}

{$apptype CONSOLE}

program image_to_pas;

uses SysUtils, Images, CastleUtils, CastleFilesUtils, ProgressUnit,
  ProgressConsole, CastleParameters;

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
          'image_to_pas: Convert image files into Pascal source code.' +nl+
          nl+
          'Call like' +nl+
          '  image_to_pas [OPTIONS...] UnitName image_name1.png...' +nl+
          nl+
          'Available options are:' +nl+
          HelpOptionHelp +nl+
          '  --no-show-progress    Do not show progress on stderr.' +nl+
          '  -o / --output DIRECTORY' +nl+
          '                        Place output unit files inside this dir.' +nl+
          '  @alpha=strip          Strip the alpha channel from the following' +nl+
          '                        images.' +nl+
          '  @alpha=keep           Keep the alpha channel on the following' +nl+
          '                        images (the default). As a result,' +nl+
          '                        alpha channel will be stored in source files.'
         );
        ProgramBreak;
       end;
    1: ShowProgress := false;
    2: OutputDirectory := Argument;
    else raise EInternalError.Create('OptionProc -- unknown arg');
  end;
end;

var
  Image, TempImage: TCastleImage;
  ImageFileName: string;
  UnitName, NameWidth, NameHeight, NamePixels, NameImage: string;
  ImagesInterface, ImagesImplementation, ImagesInitialization,
    ImagesFinalization: string;
  ImageIndex, I: Integer;
  pb: PByte;
  OutputUnit: Text;
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
  ImagesInterface := '';
  ImagesImplementation := '';
  ImagesInitialization := '';
  ImagesFinalization := '';
  for ImageIndex := 1 to Parameters.High do
  begin
    ImageFilename := Parameters[ImageIndex];

    if ImageFilename = '@alpha=strip' then
    begin
      AlphaStrip := true;
      Continue;
    end else
    if ImageFilename = '@alpha=keep' then
    begin
      AlphaStrip := false;
      Continue;
    end;

    { init other Image* variables }
    NameImage := ExtractOnlyFileName(ImageFileName);
    NameImage[1] := UpCase(NameImage[1]);
    Image := LoadImage(ImageFileName, [], []);
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
            [ImageFileName, Image.ClassName]);
      end;

      { calculate Name* variables }
      NameWidth := NameImage +'Width';
      NameHeight := NameImage +'Height';
      NamePixels := NameImage +'Pixels';

      ImagesInterface +=
        'var' +nl+
        '  ' +NameImage+ ': ' +Image.ClassName+ ';' +nl + nl;

      ImagesImplementation +=
        'const' +nl+
        '  ' +NameWidth+ ' = ' +IntToStr(Image.Width)+ ';' +nl+
        '  ' +NameHeight+ ' = ' +IntToStr(Image.Height)+ ';' +nl+
        '  ' +NamePixels+ ': array[0 .. ' +NameWidth+ ' * '
          +NameHeight+ ' * ' +IntToStr(Image.PixelSize) + ' - 1] of Byte = (' +nl+
        '    ';

      if ShowProgress then
        Progress.Init((Image.Width * Image.Height * Image.PixelSize - 1) div 12,
          'Generating image ' +NameImage);

      pb := PByte(Image.RawPixels);
      for I := 1 to Image.Width * Image.Height * Image.PixelSize - 1 do
      begin
        ImagesImplementation += Format('%4d,', [pb^]);
        if (i mod 12) = 0 then
        begin
          ImagesImplementation += nl + '    ';
          if ShowProgress then Progress.Step;
        end else
          ImagesImplementation += ' ';
        Inc(pb);
      end;
      ImagesImplementation += Format('%4d);', [pb^]) + nl + nl;

      if ShowProgress then Progress.Fini;

      ImagesInitialization +=
        '  ' +NameImage+ ' := ' +Image.ClassName+ '.Create(' +NameWidth+', ' +NameHeight+ ');' +nl+
        '  Move(' +NamePixels+ ', ' +NameImage+ '.RawPixels^, SizeOf(' +NamePixels+ '));' +nl;

      ImagesFinalization +=
        '  FreeAndNil(' +NameImage+ ');' +nl;
    finally FreeAndNil(Image) end;
  end;

  if OutputDirectory <> '' then
    OutputDirectory := InclPathDelim(OutputDirectory);

  { output full unit contents.
    Beware to not concatenate huge Images* strings in the memory,
    could be a performance / memory problem? Although code above does it anyway? }
  SafeRewrite(OutputUnit, OutputDirectory + LowerCase(UnitName) + '.pas');
  Write(OutputUnit,
    '{ -*- buffer-read-only: t -*- }' +nl+
    nl+
    '{ Unit automatically generated by image_to_pas tool,' +nl+
    '  to embed images in Pascal source code.' +nl+
    '  @exclude (Exclude this unit from PasDoc documentation.) }' +nl+
    'unit '+UnitName+';' +nl+
    nl+
    'interface' +nl+
    nl+
    'uses Images;' +nl+
    nl);
  Write(OutputUnit, ImagesInterface);
  Write(OutputUnit,
    'implementation' + nl +
    nl+
    'uses SysUtils;' + nl +
    nl +
    '{ Actual image data is included from another file, with a deliberately' +nl+
    '  non-Pascal file extension ".image_data". This way ohloh.net will' +nl+
    '  not recognize this source code as (uncommented) Pascal source code. }' +nl+
    '{$I ' + LowerCase(UnitName) + '.image_data}' +nl+
    nl +
    'initialization' +nl+
    ImagesInitialization +
    'finalization' +nl+
    ImagesFinalization +
    'end.');
  CloseFile(OutputUnit);

  SafeRewrite(OutputUnit, OutputDirectory + LowerCase(UnitName) + '.image_data');
  Write(OutputUnit, ImagesImplementation);
  CloseFile(OutputUnit);
end.
