{
  Copyright 2022-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Proxy program to call the real FPC with proper command-line options.
  Passes own command-line options to FPC,
  adds options to set standard units path.
  In particular, it adds -Fu so that FPC can find standard units.
  This is critical, as FPC bundled with Castle Game Engine doesn't have fpc.cfg
  (because then we'd have to maintain/update this fpc.cfg when engine
  is moved, leading to other issues).

  This program assumes the simple layout of FPC bundled with Castle Game Engine.
  It assumes that fpc-cge executable is exactly alongside the fpc executable,
  in bin/ subdurectory, and ../units/$fpctarget/* are the standard RTL units.

  In general, we have various tools in CGE that need to know / call FPC:
  like Lazarus CodeTools (used by our pasls, LSP server, which is in turn
  used by VS Code extension https://castle-engine.io/vscode )
  or our build-tool (castle-engine executable, https://castle-engine.io/build_tool ,
  called in turn by our CGE editor).

  There are 2 ways to call "bundled FPC" from various tools:

  1. Calling using this executable, `fpc-cge`.

      Note that this application relies on ExeName working.
      Which is reliable on Windows and Linux (using /proc/xxx/exe),
      it is only guessed (usually using ParamStr(0), which can in theory
      contain anything, though our CGE tools always try to pass full
      exe filename) on other platforms.

  2. Calling the regular 'fpc' (or 'fpc.exe' on Windows).

      In this case you must also pass proper -Fu to let this FPC find
      the standard units. If using the FindExeFpcCompiler function in CGE,
      it returns additional "out FpcStandardUnitsPath: String" that you
      should use for this purpose.

  Failure to do this will result in our bundled FPC not working correctly,
  it will fail to compile any program because it will not be able to find
  standard FPC units.
}

uses SysUtils, CastleUtils, CastleFilesUtils;

var
  FpcExe, UnitsPath: String;
  I: Integer;
  Parameters: array of String;
begin
  FpcExe := ExtractFilePath(ExeName) + 'fpc' + ExeExtension;
  UnitsPath := ExtractFileDir(ExtractFileDir(ExeName)) + PathDelim +
    'units' + PathDelim + '$fpctarget' + PathDelim + '*';

  SetLength(Parameters, ParamCount + 3);

  { Add parameters to bundled FPC just like tools/build-tool/code/toolcompile.pas
    does, passing standard units on the command-line. }
  Parameters[0] := '-Fu' + UnitsPath;
  Parameters[1] := '-n';
  Parameters[2] := '-viwn';

  { Copy all parameters from our command-line. }
  for I := 1 to ParamCount do
    Parameters[I + 2] := ParamStr(I);

  { debug }
  // for I := 0 to Length(Parameters) - 1 do
  //   Writeln('Parameters[', I, '] = ', Parameters[I]);

  { Execute FpcExe, passing stdin/out/err as our.
    Exit with the same exit code. }
  ExitCode := ExecuteProcess(FpcExe, Parameters);
end.
