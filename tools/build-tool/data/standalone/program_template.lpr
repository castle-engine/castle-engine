{
  Copyright 2017-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in the "Castle Game Engine" distribution,
  for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Program to run the game on desktop (standalone) platforms. }
program ${NAME_PASCAL};

{$apptype GUI}

{ This adds icons and version info for Windows,
  automatically created by "castle-engine compile". }
{$ifdef MSWINDOWS} {$R automatic-windows-resources.res} {$endif MSWINDOWS}

uses CastleUtils, CastleParameters, CastleSoundEngine, CastleLog,
  CastleWindow, ${GAME_UNITS};

const
  Version = '${VERSION}';
  Options: array [0..1] of TOption =
  (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'v'; Long: 'version'; Argument: oaNone)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
  case OptionNum of
    0:begin
        InfoWrite(
          ApplicationName +
          NL+
          'Available options are:' + NL +
          HelpOptionHelp + NL +
          VersionOptionHelp + NL +
          SoundEngine.ParseParametersHelp + NL+
          NL +
          TCastleWindowCustom.ParseParametersHelp(StandardParseOptions, true) + NL +
          NL+
          SCastleEngineProgramHelpSuffix(ApplicationName, Version, true));
        Halt;
      end;
    1:begin
        // include ApplicationName in version, good for help2man
        WritelnStr(ApplicationName + ' ' + Version);
        Halt;
      end;
    else raise EInternalError.Create('OptionProc');
  end;
end;

begin
  InitializeLog(Version);
  SoundEngine.ParseParameters;
  Window.ParseParameters;
  Window.OpenAndRun;
end.
