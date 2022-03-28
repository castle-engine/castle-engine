{
  Vampyre Imaging Library Demo
  Vampyre Image Converter (core low level API)

  Image Converter is command line tool for converting images between
  file and data formats. It also provides some basic manipulation functions
  like resizing, rotating, or color reduction.
  See PrintUsage procedure for usage details (or just run binary without parameters).
  Note: Operations (change format, resize, rotate) are processed in the same order
  as they appear on the command line.
}
unit DemoUnit;

{$I ImagingOptions.inc}

interface

procedure RunDemo;

implementation

uses
  SysUtils,
  Classes,
  ImagingTypes,
  Imaging,
  ImagingUtility;

const
  DefaultOutputFile = 'output.png';
  DefaultFileFormat = 'png';

var
  InFile, OutFile: string;
  Operations: TStringList;

procedure PrintHeader;
begin
  WriteLn('Vampyre Image Converter (library version ', Imaging.GetVersionStr, ')');
  WriteLn('by Marek Mauder');
  WriteLn;
end;

procedure PrintUsage;
type
  TFormatInfo = record
    Ext: string;
    CanSave: Boolean;
  end;
var
  I: LongInt;
  FmtIter: TImageFormat;
  Info: TImageFormatInfo;
  Name, Ext, Masks: string;
  CanSave, IsMulti: Boolean;
  FileFormats: array of TFormatInfo;
begin
  WriteLn('Usage:');
  WriteLn('VampConvert [-op=arg] [..] -infile=file.ext [..] [-outfile=file.ext] [-op=arg]');
  WriteLn('  Options:');
  WriteLn('    -infile  | -i: specify input image file path');
  WriteLn('    -outfile | -o: specify output image file path');
  WriteLn('       argument: file path or "*.ext" where input file name will be used ');
  WriteLn('                 but with "ext" extension');
  WriteLn('  Operations:');
  WriteLn('    Note: they are processed in the same order as they appear on command line');
  WriteLn('    -format:  changes data format of input images');
  WriteLn('       argument: name of data format supported by Imaging like A8R8G8B8');
  WriteLn('    -resize:  changes size of input images');
  WriteLn('       argument: string in format AxBxC where A is desired width,');
  WriteLn('                 B is desired height, and C is resampling filter used.');
  WriteLn('                 If A or B is 0 then original dimension will be preserved.');
  WriteLn('                 C is optional and can have one of following values: ');
  WriteLn('                 nearest(default), bilinear, bicubic, lanczos.');
  WriteLn('    -flip:    flips input images upside down');
  WriteLn('    -mirror:  mirrors input images left to right');
  WriteLn('    -colorcount: reduces number of colors in image');
  WriteLn('       argument: number of desired colors (2-4096)');
  WriteLn('    -genmipmaps: generates mipmaps for main image');
  WriteLn('       argument: number of desired mip levels. 0 or no arg means');
  WriteLn('                 create all possible levels');
  WriteLn('    -rotate: rotates input images counterclockwise');
  WriteLn('       argument: angle in degrees (integer)');

  // Enumerate all supported file formats and store default ext and
  // their capability to save files to string list.
  I := 0;
  while EnumFileFormats(I, Name, Ext, Masks, CanSave, IsMulti) do
  begin
    SetLength(FileFormats, I);
    FileFormats[I - 1].Ext := Ext;
    FileFormats[I - 1].CanSave := CanSave;
  end;
  // Print all file formats that support loading files (just write all)
  WriteLn;
  WriteLn(' Supported file formats (INPUT):');
  for I := 0 to High(FileFormats) do
    Write(FileFormats[I].Ext, ' ');

  WriteLn;
  // Print all file formats that support saving files
  WriteLn('  Supported file formats (OUTPUT):');
  for I := 0 to High(FileFormats) do
  begin
    if FileFormats[I].CanSave then
      Write(FileFormats[I].Ext, ' ');
  end;

  WriteLn;
  // Iterate over all image data formats and write their names
  Write('  Supported data formats: ');
  for FmtIter := ifIndex8 to High(TImageFormat) do
  begin
    if Imaging.GetImageFormatInfo(FmtIter, Info) then
      Write(Info.Name, ' ');
  end;
end;

procedure PrintErrorAndExit(const Msg: string; const Args: array of const);
begin
  WriteLn(Format('Error: ' + Msg, Args));
  WriteLn;
  PrintUsage;
  Operations.Free;
  Halt(1);
end;

procedure PrintHelpAndExit;
begin
  WriteLn;
  PrintUsage;
  Operations.Free;
  Halt(0);
end;

procedure PrintWarning(const Msg: string; const Args: array of const);
begin
  WriteLn(Format('Warning: ' + Msg, Args));
end;

procedure PrintInfo(const Msg: string; const Args: array of const);
begin
  WriteLn(Format('Info: ' + Msg, Args));
end;

procedure ParseCommandLine;
var
  I: LongInt;

  procedure ParseOption(const Opt: string);
  var
    I: LongInt;
    S, Arg: string;
  begin
    S := Opt;
    I := Pos('=', S);
    if I > 0 then
      Arg := Copy(S, I + 1, MaxInt)
    else
      Arg := 'none';

    Delete(S, I, MaxInt);
    Delete(S, 1, 1);
    S := LowerCase(S);

    if (S = 'infile') or (S = 'i') then
      InFile := Arg
    else if (S = 'outfile') or (S = 'o') then
      OutFile := Arg
    else if S = 'h' then
      PrintHelpAndExit
    else
      Operations.Add(Format('%s=%s', [S, LowerCase(Arg)]));
  end;

begin
  for I := 1 to ParamCount do
    ParseOption(ParamStr(I));
end;

procedure CheckOptions;
var
  InFileName, InFileDir: string;
begin
  // Check if input and output filenames are valid
  if InFile = '' then
    PrintErrorAndExit('Input file not specified', []);

  if not FileExists(InFile) then
    PrintErrorAndExit('Input file not found: "%s"', [InFile]);

  if not Imaging.IsFileFormatSupported(InFile) then
    PrintErrorAndExit('Input file format not supported: %s', [ImagingUtility.GetFileExt(InFile)]);

  if OutFile = '' then
  begin
    PrintWarning('Output file not specified, using default: %s (in current directory)',
      [DefaultOutputFile]);
    OutFile := DefaultOutputFile;
  end;

  InFileName := ExtractFileName(InFile);
  InFileDir := ExtractFileDir(InFile);
  InFileDir := Iff(InFileDir <> '', PathDelim, InFileDir);

  // If outpout filename is in format "*.ext" then input filename is used
  // but with "ext" extension
  if ChangeFileExt(ExtractFileName(OutFile), '') = '*' then
    OutFile := InFileDir + ChangeFileExt(InFileName, ExtractFileExt(OutFile));

  if not Imaging.IsFileFormatSupported(OutFile) then
  begin
    PrintWarning('Output file format not supported, using default: %s',
      [DefaultFileFormat]);
    OutFile := InFileDir + ChangeFileExt(InFileName, '.' + DefaultFileFormat);
  end;
end;

procedure ProcessOperations;
var
  I, J, X, Y, NewWidth, NewHeight: Integer;
  OpName, Arg, S: string;
  Images: TDynImageDataArray;
  Format: TImageFormat;
  ResFilter: TResizeFilter;
  MainImage: TImageData;

  procedure PrintInvalidArg(const OpName, Arg: string);
  begin
    PrintErrorAndExit('Invalid argument (%s) for operation: %s', [Arg, OpName]);
  end;

  function FindFormat(const FmtString: string): TImageFormat;
  var
    I: TImageFormat;
    Name: string;
  begin
    Result := ifUnknown;
    for I := ifIndex8 to High(TImageFormat) do
    begin
      Name := Imaging.GetFormatName(I);
      if SameText(FmtString, Name) or SameText(FmtString, 'if' + Name) then
      begin
        Result := I;
        Exit;
      end;
    end;
  end;

begin
  Operations.NameValueSeparator := '=';
  InitImage(MainImage);

  try
    // Load input image
    if not Imaging.LoadMultiImageFromFile(InFile, Images) then
      PrintErrorAndExit('Input file loading failed: %s', [ImagingUtility.GetExceptObject.Message]);
    // Check if all loaded images are OK or if they are any at all
    if (Length(Images) = 0) or not Imaging.TestImagesInArray(Images) then
      PrintErrorAndExit('Input file loaded but it does not contain any images or some of them are invalid', []);

    PrintInfo('Input images (count: %d) loaded succesfully from: %s', [Length(Images), InFile]);

    // Now process operations one by one
    for I := 0 to Operations.Count - 1 do
    begin
      // Get operation name and argument
      OpName := Operations.Names[I];
      Arg := Operations.ValueFromIndex[I];

      if OpName = 'format' then
      begin
        // Check if argument is name of some data format
        Format := FindFormat(Arg);
        if Format = ifUnknown then
          PrintInvalidArg(OpName, Arg);
        // If some format was found then all images are converted to it
        PrintInfo('Converting images to data format: %s', [Imaging.GetFormatName(Format)]);
        for J := 0 to High(Images) do
          Imaging.ConvertImage(Images[J], Format);
      end
      else if OpName = 'resize' then
      begin
        // Parse argument in format %dx%d[x%s]
        J := Pos('x', Arg);
        if J = 0 then
          PrintInvalidArg(OpName, Arg);
        X := StrToIntDef(Copy(Arg, 1, J - 1), Images[0].Width);
        Delete(Arg, 1, J);
        J := Pos('x', Arg);
        S := 'nearest';
        if J <> 0 then
        begin
          S := Copy(Arg, J + 1, MaxInt);
          Delete(Arg, J, MaxInt);
        end;
        Y := StrToIntDef(Arg, 0);
        // Limit new dimensions and convert
        // invalid dimensions are set to 0 which is special value (later)
        X := ClampInt(X, 0, 32768);
        Y := ClampInt(Y, 0, 32768);
        // Select filtering method used for resizing according to argument
        ResFilter := rfNearest;
        if Pos('bil', S) = 1 then
          ResFilter := rfBilinear
        else if Pos('bic', S) = 1 then
          ResFilter := rfBicubic
        else if Pos('lan', S) = 1 then
          ResFilter := rfLanczos;

        PrintInfo('Resizing images to %dx%d using [%s] filter: ', [X, Y, S]);

        for J := 0 to High(Images) do
        begin
          // If any of new dimensions is 0 we use the original dimension
          // of image
          NewWidth := Iff(X = 0, Images[J].Width, X);
          NewHeight := Iff(Y = 0, Images[J].Height, Y);
          Imaging.ResizeImage(Images[J], NewWidth, NewHeight, ResFilter);
        end;
      end
      else if OpName = 'flip' then
      begin
        // Simply flip all images
        PrintInfo('Flipping images upside down', []);
        for J := 0 to High(Images) do
          Imaging.FlipImage(Images[J]);
      end
      else if OpName = 'mirror' then
      begin
        // Simply mirror all images
        PrintInfo('Mirroring images left to right', []);
        for J := 0 to High(Images) do
          Imaging.MirrorImage(Images[J]);
      end
      else if OpName = 'colorcount' then
      begin
        // Get value of the argument ...
        if not TryStrToInt(Arg, X) then
          PrintInvalidArg(OpName, Arg);
        X := ClampInt(X, 2, 4096);
        PrintInfo('Reducing color count of images to: %d', [X]);
        // ... and reduce number of colors of all images
        for J := 0 to High(Images) do
          Imaging.ReduceColors(Images[J], X);
      end
      else if OpName = 'genmipmaps' then
      begin
        // Get number of mipmaps from argument or use
        // default 0 which means "create all mip levels you can"
        X := StrToIntDef(Arg, 0);
        PrintInfo('Generating mipmaps for main image', []);
        // Clone main image and use input array as the output of
        // mipmap generation function
        Imaging.CloneImage(Images[0], MainImage);
        Imaging.GenerateMipMaps(MainImage, X, Images);
      end
      else if OpName = 'rotate' then
      begin
        // Parse argument, only integer degrees are allowed
        if not TryStrToInt(Arg, X) then
          PrintInvalidArg(OpName, Arg);
        PrintInfo('Rotating images: %d degrees CCW', [X]);
        // Rotate all
        for J := 0 to High(Images) do
          Imaging.RotateImage(Images[J], X);
      end
      else
      begin
        // Warn about unknown operations passed to program
        PrintWarning('Unrecognized operation: ' + OpName, []);
      end;
    end;

    // Copy metadata if present
    GlobalMetadata.CopyLoadedMetaItemsForSaving;

    // Finally save the result
    if not Imaging.SaveMultiImageToFile(OutFile, Images) then
      PrintErrorAndExit('Output file saving failed: %s', [ImagingUtility.GetExceptObject.Message])
    else
      PrintInfo('Output images saved succesfully to: %s', [OutFile])
  finally
    // Free images in array as well as temp image
    Imaging.FreeImagesInArray(Images);
    Imaging.FreeImage(MainImage);
  end;
end;

procedure RunDemo;
begin
  PrintHeader;
  Operations := TStringList.Create;
  ParseCommandLine;
  CheckOptions;
  try
    ProcessOperations;
  except
    PrintErrorAndExit('Exception raised during processing oprations: %s',
      [ImagingUtility.GetExceptObject.Message]);
  end;
  Operations.Free;
end;

{
  File Notes:

  -- 0.80 -----------------------------------------------------
    - Added Lanczos as a resampling option
    - Removed no longer required limit on rotation angles to be multimples of 90.

  -- 0.77.1 ---------------------------------------------------
    - Refactored the demo (moved stuff to unit from dpr) and
      added Lazarus project files.

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - added -i and -o shortcut cmd line parameters and fixed
      FPC 32/64 bit compatibility issue
    - List of supported file formats printed by PrintUsage is now
      dynamic and shows input and output formats separately

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - demo created
}

end.
