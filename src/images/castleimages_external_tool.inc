{$ifdef FPC}
  {$define HAS_EXECUTE_PROCESS}
{$endif}

function LoadImageExternalTool(
  Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass;
  const ImageMagickName: string): TEncodedImage;
{$ifdef HAS_EXECUTE_PROCESS}
var
  ImageFileName: string;
  PngFileName: string;
  PngStream: TMemoryStream;
  ConvertExecutable: string;
begin
  ImageFileName := GetTempFileNameCheck;
  StreamSaveToFile(Stream, ImageFileName);
  PngFileName := GetTempFileNameCheck;

  try
    { We use temporary files instead of pipes
      (we could do "convert gif:- png:-" and pass our streams to
      stdin and stdout using TProcess) to avoid the case with deadlock
      when you try to insert data to stdin of one process
      (but input pipe buffer is full to you have to wait),
      but the process is hanging trying to write it's data on stdout
      (but the output pipe buffer is full, so it has to wait).

      We use ExecuteProcess, not TProcess, because

      1) It allows to pass separate arguments as separate array items,
         which is the only solution to avoid any problems with filenames
         with strange chars (like spaces). TProcess doesn't allow it,
         at least as of FPC 2.0.2.

      2) It workarounds a horrible and mysterious bug on Michalis
         Linux installation on kangury.
         It happened when we used TProcess, and we let it search for
         `convert' utility on it's own (instead of searching
         $PATH by FindExe on our own). When no convert was
         available then

         - It seems that TProcess.Execute wasn't exiting with
           exception, it just continued like nothing happened.
           So we later exited with exception that "PngFileName
           doesn''t exist", which is quite confusing for user.

         - After writing above warning on console, view3dscene
           is trying for a couple of seconds to open OpenGL window...
           and then SYSTEM HANGS. It's not a joke (I wish it was).
           System really hangs, you cannot reboot with Ctrl+Alt+Del,
           you cannot switch to text console, nothing. And it doesn't
           look like the resource drain, because it happens suddenly
           and waiting a couple of minutes doesn't help.
           Looks like somehow we cannot initialize OpenGL context,
           and then kernel just hangs. Possibly it's a bug of
           NVidia drivers (my bet), possibly it's in Linux kernel
           (ouhhh...) ?

           Looking at TProcess for Unix implementation,
           it's clear that there are some problems...
           TProcess does PathFileSearch('convert') on it's own,
           and instead of failing when result is '', it forks
           and then calls fpexecv with first argument = ''.
           What happens then ? Remains to be tested.
           It looks like this can indeed cause problems:
           we fork, and try to do something... err... strange ?
           Possibly trying to work within the same OpenGL context
           from 2 different versions of forked process ?
           So it seems like it's TProcess Unix implementation bug,
           that accidentaly hits also NVidia Linux driver bug.
           TODO: investigate more, submit to FPC bug/patch.

      First implementation using TProcess:

        Process := TProcess.Create(nil);
        try
          Process.CommandLine := Format(
            'convert "%s:%s" "png:%s"',
            [ImageMagickName, ImageFileName, PngFileName]);
          Process.Options := Process.Options + [poWaitOnExit];
          Process.Execute;
        finally FreeAndNil(Process) end;

      We use FindExe because:
      Under Windows it seems that we *have* to manually search $PATH
      variable for convert.exe executable. Otherwise when trying to run
      just 'convert' (either by TProcess or ExecuteProcess) we always
      end up trying to run convert.exe from c:/windows/system32/
      (utlity on Windows XP to convert filesystems from FAT to NTFS),
      even when the path to ImageMagick is first on $PATH (before
      the path to windows/system32/). }

    ConvertExecutable := FindExe('convert');

    if ConvertExecutable = '' then
      raise EImageLoadError.CreateFmt('You must have "convert" program from ' +
        'ImageMagick package (available on $PATH) to be able to load image ' +
        'format "%s"', [ImageMagickName]);

    ExecuteProcess(ConvertExecutable,
      [ ImageMagickName + ':' + ImageFileName,
        'png:' + PngFileName ]);

    PngStream := TMemoryStream.Create;
    try
      PngStream.LoadFromFile(PngFileName);
      PngStream.Position := 0;
      Result := LoadPNG(PngStream, AllowedImageClasses);
    finally FreeAndNil(PngStream) end;

  finally
    CheckDeleteFile(ImageFileName, true);
    CheckDeleteFile(PngFileName, true);
  end;
end;

{$else}

begin
  raise EImageFormatNotSupported.Create('Running external processes ' +
    'is not implemented for this OS and compiler');
end;
{$endif}

function LoadSGI(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  Result := LoadImageExternalTool(Stream, AllowedImageClasses, 'sgi');
end;

function LoadTIFF(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  Result := LoadImageExternalTool(Stream, AllowedImageClasses, 'tiff');
end;

function LoadJP2(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  Result := LoadImageExternalTool(Stream, AllowedImageClasses, 'jp2');
end;

function LoadEXR(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  Result := LoadImageExternalTool(Stream, AllowedImageClasses, 'exr');
end;
