{
  Copyright 2022-2022 Michalis Kamburelis.

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

  This assumes the simple layout of FPC bundled with Castle Game Engine.
  It assumes that fpc-cge executable is exactly alongside the fpc executable,
  in bin/ subdurectory, and ../units/$fpctarget/* are the standard RTL units.

  It is used for now only when this FPC needs to be executed by CodeTools
  (used in turn by the LSP server, see https://castle-engine.io/vscode ).
  In other cases, CGE editor executes real FPC and calculates and passes
  the appropriate parameters from the outside -- which is a bit more reliable,
  as it doesn't rely on ExeName (and ExeName is only guaranteed on Windows
  and Linux).
  In case of FPC for LSP it may acceptable, we always pass full FPC path,
  so even fallback ExeName implementation ("ParamStr(0)") should work OK
  in this case.
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
