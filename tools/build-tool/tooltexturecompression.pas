{
  Copyright 2016-2016 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Compressing textures. }
unit ToolTextureCompression;

interface

uses CastleUtils, CastleStringUtils,
  ToolProject;

procedure AutoCompressTextures(const Project: TCastleProject);
procedure AutoCompressClean(const Project: TCastleProject);

implementation

uses SysUtils,
  CastleURIUtils, CastleMaterialProperties, CastleImages, CastleFilesUtils,
  CastleWarnings, CastleFindFiles,
  ToolUtils;

procedure AutoCompressTextures(const Project: TCastleProject);

  procedure TryToolExe(var ToolExe: string; const ToolExeAbsolutePath: string);
  begin
    if (ToolExe = '') and FileExists(ToolExeAbsolutePath) then
      ToolExe := ToolExeAbsolutePath;
  end;

  procedure TryToolExePath(var ToolExe: string; const ToolExeName: string;
    const C: TTextureCompression);
  begin
    if ToolExe = '' then
    begin
      ToolExe := FindExe(ToolExeName);
      if ToolExe = '' then
        raise Exception.CreateFmt('Cannot find tool "%s" necessary to make compressed texture format %s',
          [ToolExeName, TextureCompressionToString(C)]);
    end;
  end;

  procedure AMDCompress(const InputFile, OutputFile: string;
    const C: TTextureCompression; const CompressionNameForTool: string);
  var
    {$ifndef MSWINDOWS}
    WineExe: string;
    {$endif}
    ToolExe, InputFlippedFile, OutputTempFile, TempPrefix: string;
    Image: TCastleImage;
  begin
    { On non-Windows, we need wine for this }
    {$ifndef MSWINDOWS}
    WineExe := '';
    TryToolExePath(WineExe, 'wine', C);
    {$endif}

    ToolExe := '';
    {$ifdef MSWINDOWS}
    TryToolExe(ToolExe, 'c:/Program Files/AMD/AMDCompress/AMDCompressCLI.exe');
    TryToolExe(ToolExe, 'c:/Program Files (x86)/AMD/AMDCompress/AMDCompressCLI.exe');
    {$endif}
    {$ifdef UNIX}
    TryToolExe(ToolExe, HomePath + '.wine/drive_c/Program Files/AMD/AMDCompress/AMDCompressCLI.exe');
    TryToolExe(ToolExe, HomePath + '.wine/drive_c/Program Files (x86)/AMD/AMDCompress/AMDCompressCLI.exe');
    {$endif}
    TryToolExePath(ToolExe, 'AMDCompressCLI', C);

    TempPrefix := GetTempFileNamePrefix;

    InputFlippedFile := TempPrefix + '.png';

    { in theory, when DDSFlipped = false, we could just do
      CheckCopyFile(InputFile, InputFlippedFile).
      But then AMDCompressCLI fails to read some png files
      (like flying in dark_dragon). }
    Image := LoadImage(FilenameToURISafe(InputFile));
    try
      if TextureCompressionInfo[C].DDSFlipped then
        Image.FlipVertical;
      SaveImage(Image, FilenameToURISafe(InputFlippedFile));
    finally FreeAndNil(Image) end;

    { this is worse, as it requires ImageMagick }
    // RunCommandSimple(FindExe('convert'), [InputFile, '-flip', InputFlippedFile]);

    OutputTempFile := TempPrefix + 'output' + ExtractFileExt(OutputFile);

    RunCommandSimple(ExtractFilePath(TempPrefix),
      {$ifdef MSWINDOWS} ToolExe, [
      {$else}            WineExe, [ToolExe,
      {$endif}
      '-fd', CompressionNameForTool,
      { we cannot just pass InputFlippedFile, OutputFile to compressonator,
        because it may be running in wine and not understanding Unix absolute paths. }
      ExtractFileName(InputFlippedFile),
      ExtractFileName(OutputTempFile)]);

    CheckRenameFile(OutputTempFile, OutputFile);
    CheckDeleteFile(InputFlippedFile, true);
  end;

  procedure PVRTexTool(const InputFile, OutputFile: string;
    const C: TTextureCompression; const CompressionNameForTool: string);
  var
    ToolExe: string;
  begin
    ToolExe := '';
    {$ifdef UNIX}
    { Try the standard installation path on Linux.
      On x86_64, try the 64-bit version first, otherwise fallback on 32-bit. }
    {$ifdef CPU64}
    TryToolExe(ToolExe, '/opt/Imagination/PowerVR_Graphics/PowerVR_Tools/PVRTexTool/CLI/Linux_x86_64/PVRTexToolCLI');
    {$endif}
    TryToolExe(ToolExe, '/opt/Imagination/PowerVR_Graphics/PowerVR_Tools/PVRTexTool/CLI/Linux_x86_32/PVRTexToolCLI');
    {$endif}
    { otherwise, assume it's on $PATH }
    TryToolExePath(ToolExe, 'PVRTexToolCLI', C);

    RunCommandSimple(ToolExe,
      ['-f', CompressionNameForTool,
       '-q', 'pvrtcbest',
       '-m', '1',
       '-squarecanvas', '+' ,
       '-flip', 'y', // TODO: use this only when TextureCompressionInfo[C].DDSFlipped
       '-i', InputFile,
       '-o', OutputFile]);
  end;

var
  Textures: TCastleStringList;
  I: Integer;
  C: TTextureCompression;
  TextureFile, TextureURL, OutputFile, OutputURL, OutputPath: string;
  MatProps: TMaterialProperties;
begin
  MatProps := TMaterialProperties.Create(false);
  try
    MatProps.URL := FilenameToURISafe(Project.DataPath + 'material_properties.xml');
    Textures := MatProps.AutoCompressedTextures;
    try
      for I := 0 to Textures.Count - 1 do
      begin
        TextureURL := Textures[I];
        TextureFile := URIToFilenameSafe(TextureURL);
        for C in MatProps.AutoCompressedTextureFormats do
        begin
          OutputURL := MatProps.AutoCompressedTextureURL(TextureURL, C);
          OutputFile := URIToFilenameSafe(OutputURL);
          if (not FileExists(OutputFile)) or
             (FileAge(OutputFile) < FileAge(TextureFile)) then
          begin
            Writeln(Format('Updating "%s" from input "%s"', [OutputFile, TextureFile]));
            OutputPath := ExtractFilePath(OutputFile);
            CheckForceDirectories(OutputPath);
            case C of
              { tcDxt1_RGB and tcDxt1_RGBA result in the same output file,
                DXT1 is the same compression in both cases, and there's no option
                how to differentiate between this in DDS file.

                TODO: hm, with new AMDCompress, maybe we can/should use
                -DXT1UseAlpha and -AlphaThreshold for tcDxt1_RGBA? }
              tcDxt1_RGB : AMDCompress(TextureFile, OutputFile, C, 'DXT1');
              tcDxt1_RGBA: AMDCompress(TextureFile, OutputFile, C, 'DXT1');
              tcDxt3     : AMDCompress(TextureFile, OutputFile, C, 'DXT3');
              tcDxt5     : AMDCompress(TextureFile, OutputFile, C, 'DXT5');

              tcATITC_RGB                   : AMDCompress(TextureFile, OutputFile, C, 'ATC_RGB');
              tcATITC_RGBA_InterpolatedAlpha: AMDCompress(TextureFile, OutputFile, C, 'ATC_RGBA_Interpolated');
              tcATITC_RGBA_ExplicitAlpha    : AMDCompress(TextureFile, OutputFile, C, 'ATC_RGBA_Explicit');

              tcPvrtc1_4bpp_RGB:  PVRTexTool(TextureFile, OutputFile, C, 'PVRTC1_4_RGB');
              tcPvrtc1_2bpp_RGB:  PVRTexTool(TextureFile, OutputFile, C, 'PVRTC1_2_RGB');
              tcPvrtc1_4bpp_RGBA: PVRTexTool(TextureFile, OutputFile, C, 'PVRTC1_4');
              tcPvrtc1_2bpp_RGBA: PVRTexTool(TextureFile, OutputFile, C, 'PVRTC1_2');
              tcPvrtc2_4bpp:      PVRTexTool(TextureFile, OutputFile, C, 'PVRTC2_4');
              tcPvrtc2_2bpp:      PVRTexTool(TextureFile, OutputFile, C, 'PVRTC2_2');

              tcETC1:             PVRTexTool(TextureFile, OutputFile, C, 'ETC1');
                            // or AMDCompress(TextureFile, OutputFile, C, 'ETC_RGB');

              else OnWarning(wtMajor, 'GPUCompression', Format('Compressing to GPU format %s not implemented (to update "%s")',
                [TextureCompressionToString(C), OutputFile]));
            end;
          end else
          begin
            if Verbose then
              Writeln(Format('Not need to update "%s", it is already newer than input', [OutputFile]));
          end;
        end;
      end;
    finally FreeAndNil(Textures) end;
  finally FreeAndNil(MatProps) end;
end;

procedure CleanDir(const FileInfo: TFileInfo; Data: Pointer;
  var StopSearch: boolean);
begin
  Writeln('Removing ', FileInfo.AbsoluteName);
  RemoveNonEmptyDir(FileInfo.AbsoluteName);
end;

procedure AutoCompressClean(const Project: TCastleProject);
begin
  FindFiles(Project.DataPath, TMaterialProperties.AutoCompressedDirName, true,
    @CleanDir, nil, [ffRecursive]);
end;

end.
