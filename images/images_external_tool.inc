function LoadAnyImageExternalTool(
  Stream: TStream; FormatRequired: TImageFormatRequirements;
  ConvertToRequired: boolean;
  const ImageMagickName: string): TImage;
{$ifdef HAS_PROCESS}

  function GetTempFileNameCheck: string;
  begin
    Result := GetTempFileName('', ProgramName);
    { Be paranoid and check whether file does not exist. }
    if FileExists(Result) then
      raise Exception.CreateFmt('Temporary file "%s" already exists', [Result]);
  end;

var
  Process: TProcess;
  ImageFileName: string;
  PngFileName: string;
  PngStream: TStream;
begin
  ImageFileName := GetTempFileNameCheck;
  StreamSaveToFile(Stream, ImageFileName);
  PngFileName := GetTempFileNameCheck;

  { We use temporary files instead of pipes
    (we could do "convert gif:- png:-" and pass our streams to
    stdin and stdout using TProcess) to avoid the case with deadlock
    when you try to insert data to stdin of one process
    (but input pipe buffer is full to you have to wait),
    but the process is hanging trying to write it's data on stdout
    (but the output pipe buffer is full, so it has to wait). }

  Process := TProcess.Create(nil);
  try
    { TODO: passing params in separate array would be better. }
    Process.CommandLine := Format(
      'convert "%s:%s" "png:%s', [ImageMagickName, ImageFileName, PngFileName]);
    { Writeln('Converting by: ', Process.CommandLine); }
    Process.Options := Process.Options + [poWaitOnExit];
    Process.Execute;
  finally FreeAndNil(Process) end;

  PngStream := CreateReadFileStream(PngFileName);
  try
    Result := LoadAnyPng(PngStream, FormatRequired, ConvertToRequired);
  finally FreeAndNil(PngStream) end;

  CheckDeleteFile(ImageFileName);
  CheckDeleteFile(PngFileName);
end;

{$else}

begin
  raise EImageFormatNotSupported.Create('Running external processes ' +
    'is not implemented for this OS and compiler');
end;
{$endif}

function LoadGIF(Stream: TStream): TRGBImage;
begin
  Result := LoadAnyGIF(Stream, frWithoutAlpha, true) as TRGBImage;
end;

function LoadAnyGIF(Stream: TStream; FormatRequired: TImageFormatRequirements;
  ConvertToRequired: boolean): TImage;
begin
  Result := LoadAnyImageExternalTool(
    Stream, FormatRequired, ConvertToRequired, 'gif');
end;