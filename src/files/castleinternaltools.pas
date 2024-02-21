{
  Copyright 2022-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Functionality useful for CGE tools and design-time operations.

  Practically, this is a shared code between

  @unorderedList(
    @item(CGE build tool, in tools/build-tool/)

    @item(Delphi design-time functionality, in castle_engine_design.dpk,
      in unit CastleInternalDelphiDesign.)
  )
}
unit CastleInternalTools;

interface

uses Classes;

const
  { Paths with units and include files that are for all OSes and all compilers.

    Note:

    - We don't bother trying to have separate include dirs (.inc) and units (.pas).
      We just pass the same paths for both includes and units, this is simpler.

    - We pass all paths, even system-specific, regardless of the target
      OS/architecture.

      We tried smarter approach in the past (such that you could have e.g.
      "windows/castle_system_specific.inc" and "unix/castle_system_specific.inc",
      and compiler recognized what to do on [$I castle_system_specific.inc]
      based on include paths)...
      but it was not friendly to Lazarus or Delphi packages,
      where it's easier to define the same include path for all platforms.

      So it is simpler to just name all includes and units differently,
      even across system-specific dirs. }

  EnginePaths: array [0..43] of String = (
    'base',
    'common_includes',
    'base/android',
    'base/windows',
    'base/unix',
    'base_rendering',
    'base_rendering/dglopengl',
    'base_rendering/glsl/generated-pascal',
    'fonts',
    'window',
    'window/gtk',
    'window/windows',
    'window/unix',
    'window/deprecated_units',
    'images',
    'transform',
    'scene',
    'scene/glsl/generated-pascal',
    'scene/x3d',
    'scene/load',
    'scene/load/spine',
    'scene/load/md3',
    'scene/load/collada',
    'scene/load/pasgltf',
    'audio',
    'audio/fmod',
    'audio/openal',
    'audio/ogg_vorbis',
    'files',
    'files/indy',
    'castlescript',
    'ui',
    'ui/windows',
    'services',
    'physics',
    'physics/kraft',
    'deprecated_units',
    { Vampyre Imaging Library }
    'vampyre_imaginglib/src/Source',
    'vampyre_imaginglib/src/Source/JpegLib',
    'vampyre_imaginglib/src/Source/ZLib',
    'vampyre_imaginglib/src/Extras/Extensions',
    'vampyre_imaginglib/src/Extensions/J2KObjects',
    'vampyre_imaginglib/src/Extensions/LibTiff',
    'vampyre_imaginglib/src/Extensions'
  );

  { Additional include/units paths, only for Delphi. }
  EnginePathsDelphi: array [0..2] of String = (
    'delphi',
    'compatibility/delphi-only',
    'compatibility/delphi-only/fcl-json'
  );

  { Paths for library (object) files.
    For FPC these are passed using -Fl. }
  EngineLibraryPaths: array [0..1] of String = (
    'vampyre_imaginglib/src/Extensions/J2KObjects',
    'vampyre_imaginglib/src/Extensions/LibTiff/Compiled'
  );

type
  { CGE project dependencies. }
  TDependency = (depFreetype, depZlib, depPng, depSound, depOggVorbis, depHttps);
  TDependencies = set of TDependency;

  { Platforms where we make effort to copy extra files alongside the EXE
    ("deploy").
    This is done when building a CGE project
    using build tool ("castle-engine compile")
    or in Delphi IDE (with our CastleInternalDelphiDesign feature).

    TODO: In the future, we could use TOS and TCPU and make it general.
    For now, this is practically only used by Windows platforms that need
    some DLLs, on other platforms we're fine with just using system libraries. }
  TDeployFilesPlatform = (dpWin32, dpWin64);

{ Add deploy files (.dll on Windows) based on given platform and project
  dependencies. The resulting filenames are relative to build tool's data. }
procedure DeployFiles(const DeployPlatform: TDeployFilesPlatform;
  const Dependencies: TDependencies; const Files: TStrings);

{ API reference (online or offline).
  EnginePath is the path to the engine root directory,
  or '' if not known. }
function ApiReferenceUrlCore(const EnginePath: String): String;

implementation

uses SysUtils,
  CastleUriUtils;

function ApiReferenceUrlCore(const EnginePath: String): String;
var
  LocalDocsPath: String;
begin
  if EnginePath <> '' then
  begin
    LocalDocsPath := EnginePath + 'doc' + PathDelim + 'reference' + PathDelim;
    if DirectoryExists(LocalDocsPath) then
      Exit(FilenameToUriSafe(LocalDocsPath));
  end;

  Result := 'https://castle-engine.io/apidoc/html/';
end;

procedure DeployFiles(const DeployPlatform: TDeployFilesPlatform;
  const Dependencies: TDependencies; const Files: TStrings);
var
  Prefix: String;
begin
  case DeployPlatform of
    dpWin32:
      begin
        { Matches CPUToString(CPU) + '-' + OSToString(OS) for win32. }
        Prefix := 'external_libraries/i386-win32/';

        if depFreetype in Dependencies then
        begin
          Files.Add(Prefix + 'freetype.dll');
          Files.Add(Prefix + 'vcruntime140.dll');
        end;
        if depZlib in Dependencies then
          Files.Add(Prefix + 'zlib1.dll');
        if depPng in Dependencies then
          Files.Add(Prefix + 'libpng12.dll');
        if depSound in Dependencies then
        begin
          Files.Add(Prefix + 'OpenAL32.dll');
          Files.Add(Prefix + 'wrap_oal.dll');
        end;
        if depOggVorbis in Dependencies then
        begin
          Files.Add(Prefix + 'ogg.dll');
          Files.Add(Prefix + 'vorbis.dll');
          Files.Add(Prefix + 'vorbisenc.dll');
          Files.Add(Prefix + 'vorbisfile.dll');
          Files.Add(Prefix + 'msvcr120.dll');
        end;
        if depHttps in Dependencies then
        begin
          Files.Add(Prefix + 'openssl/libeay32.dll');
          Files.Add(Prefix + 'openssl/ssleay32.dll');
        end;
      end;

    dpWin64:
      begin
        { Matches CPUToString(CPU) + '-' + OSToString(OS) for win32. }
        Prefix := 'external_libraries/x86_64-win64/';

        if depFreetype in Dependencies then
        begin
          Files.Add(Prefix + 'freetype.dll');
          Files.Add(Prefix + 'vcruntime140.dll');
        end;
        if depZlib in Dependencies then
          Files.Add(Prefix + 'zlib1.dll');
        if depPng in Dependencies then
          Files.Add(Prefix + 'libpng14-14.dll');
        if depSound in Dependencies then
        begin
          Files.Add(Prefix + 'OpenAL32.dll');
          Files.Add(Prefix + 'wrap_oal.dll');
        end;
        if depOggVorbis in Dependencies then
        begin
          Files.Add(Prefix + 'libogg.dll');
          Files.Add(Prefix + 'libvorbis.dll');
          { Files.Add(Prefix + 'vorbisenc.dll'); not present? }
          Files.Add(Prefix + 'vorbisfile.dll');
          Files.Add(Prefix + 'msvcr120.dll');
        end;
        if depHttps in Dependencies then
        begin
          Files.Add(Prefix + 'openssl/libeay32.dll');
          Files.Add(Prefix + 'openssl/ssleay32.dll');
        end;
      end;
  end;
end;

end.